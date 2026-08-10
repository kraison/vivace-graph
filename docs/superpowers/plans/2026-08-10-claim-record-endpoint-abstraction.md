# The claim record and the endpoint abstraction — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `graph-db/spacetime`'s claim record — a reified relation stored as a node, with arity as a type, identity enforced by `def-unique`, and an inverse query answerable from the claim graph's own indexes.

**Architecture:** A macro expands into a tenant's own `claim` / `unary-claim` / `binary-claim` hierarchy plus its constraints and indexes. Endpoints are stored values, never graph references, so nothing crosses a graph boundary inside a write transaction. Both uniqueness constraints are complete — no nullable component — so #107's null-exemption never fires for claims.

**Tech Stack:** Common Lisp (SBCL), `graph-db/core`, `graph-db/spacetime` (S1a), FiveAM.

**Spec:** `docs/superpowers/specs/2026-08-10-claim-record-endpoint-abstraction-design.md`
**Issue:** #131. **Parent:** #108. **Depends on:** #130 (merged).

## Global Constraints

- **80-column hard limit** in all Lisp source: code, comments, docstrings and strings alike. Spaces only, never tabs.
- **Comments terse, pointing elsewhere** — state the non-obvious fact and cite `(design §N)` or `(GH #131)`.
- **No change to any file belonging to the `graph-db/core` ASDF system** — that is every `.lisp` file at the repository *root*. `graph-db.asd` is the sole exception.
- **Nothing may name a concept from any tenant application.**
- **`producer` never includes the rule version**; `rule-version` is provenance only.
- **The unary constraint is declared on `unary-claim`, never on `claim`** — `subtypep` would bind `binary-claim` too and forbid one producer relating a subject to several objects.
- **SBCL only.** ECL is demoted to periodic; say explicitly when skipped.
- Work on a branch off `experiment`. Show the full diff under `## 📋 DIFF FOR REVIEW` before each commit. Do not push.
- After any deliberate break-and-restore, delete `~/.cache/common-lisp/sbcl-*/Users/kraison/work/vivace-graph-v3/spacetime` and `.../tests/spacetime` before trusting a run — a stale FASL makes a restored tree look broken.

### Three plan-level refinements the spec does not carry

1. **A family registry.** `claims-touching` and `delete-claims-by-producer` take the *parent* class name, but need the arity subclasses. The macro registers `parent → (unary . binary)` in `*claim-families*` rather than deriving names by string munging.
2. **A third index, `(producer)` on the parent.** The spec names two indexes, both for the inverse query. `delete-claims-by-producer` needs to *find* by producer, and the uniqueness constraints are not queryable — `index-lookup` reads `def-index` indexes only. Without this the sweep is a full scan, and regeneration is a hot path.
3. **Slot and accessor symbols live in `graph-db.spacetime`**, not the tenant's package, so two claim families share one set of accessor generic functions. `def-node-type` interns `MAKE-<NAME>` in the *calling* package, which is correct and unchanged.

---

## File Structure

| File | Responsibility |
|---|---|
| `spacetime/claim.lisp` | shared slot list, the family registry, `def-claim-classes` |
| `spacetime/claim-query.lisp` | `claims-touching`, `delete-claims-by-producer`, `claim-extent` |
| `tests/spacetime/claim-tests.lisp` | classes, constructors, extra-slots |
| `tests/spacetime/claim-identity-tests.lisp` | the permit/forbid table, unary dedup |
| `tests/spacetime/claim-query-tests.lisp` | inverse query, extent codec, regeneration |
| `tests/spacetime/claim-concurrency-tests.lisp` | the eight-thread race |
| `graph-db.asd` | two components per system |

---

### Task 1: The macro, the registry, and the classes

**Files:**
- Create: `spacetime/claim.lisp`, `tests/spacetime/claim-tests.lisp`
- Modify: `spacetime/package.lisp`, `graph-db.asd`

**Scope:** classes and constructors only. **No `def-unique` and no `def-index` yet** — Task 2 and Task 3 add them, and their tests must fail first.

**Interfaces:**
- Produces: `def-claim-classes (parent graph-name &key extra-slots)`; `*claim-families*`; `claim-family-parent` / `-unary` / `-binary`; `claim-family (parent)`; accessors `claim-subject-namespace`, `claim-subject-key`, `claim-object-namespace`, `claim-object-key`, `claim-relation`, `claim-producer`, `claim-rule-version`, `claim-method`, `claim-standing`, `claim-confidence`, `claim-extent-sexp`, `claim-geometry`.

- [ ] **Step 1: Add the exports**

In `spacetime/package.lisp`, add to the existing `(:export ...)` form:

```lisp
   ;; claim (GH #131)
   #:def-claim-classes #:claim-family #:claim-family-parent
   #:claim-family-unary #:claim-family-binary
   #:claim-subject-namespace #:claim-subject-key
   #:claim-object-namespace #:claim-object-key
   #:claim-relation #:claim-producer #:claim-rule-version
   #:claim-method #:claim-standing #:claim-confidence
   #:claim-extent-sexp #:claim-geometry
   #:claim-extent #:claims-touching #:delete-claims-by-producer
   #:unknown-claim-family
```

- [ ] **Step 2: Write the failing tests**

`tests/spacetime/claim-tests.lisp`:

```lisp
;;;; The claim class hierarchy and its macro (GH #131, design §3-§5).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *claim-graph-name* :graph-db-claim-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *claim-graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes ct-claim :graph-db-claim-test
  :extra-slots ((weight :initarg :weight :accessor ct-weight
                        :initform nil)))

(defmacro with-claim-graph ((g) &body body)
  "A fresh on-disk graph named *CLAIM-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *claim-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(test the-macro-defines-a-three-class-hierarchy
  (is-true (find-class 'ct-claim nil))
  (is-true (find-class 'ct-claim-unary nil))
  (is-true (find-class 'ct-claim-binary nil))
  (is-true (subtypep 'ct-claim-unary 'ct-claim))
  (is-true (subtypep 'ct-claim-binary 'ct-claim))
  (is-false (subtypep 'ct-claim-binary 'ct-claim-unary)))

(test the-parent-gets-no-constructor
  "Design §3.3: non-instantiability is signalled by not generating a
constructor.  MAKE-INSTANCE still works; nothing invites it."
  (is-true (fboundp 'make-ct-claim-unary))
  (is-true (fboundp 'make-ct-claim-binary))
  (is-false (fboundp 'make-ct-claim)))

(test object-slots-exist-only-on-the-binary-class
  "This IS the arity-as-a-type property (design §3.1): a unary claim cannot
carry an object because the slot does not exist.

Uses CL's SLOT-EXISTS-P on instances rather than a MOP call on classes:
graph-db :USEs SB-MOP on SBCL and CLOSER-MOP only on CCL/LispWorks, so
CLOSER-MOP is not loaded here and this test package sees neither."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((u (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                                    :relation :r :producer :p
                                    :standing :inferred))
            (b (make-ct-claim-binary :subject-namespace :ns :subject-key "s"
                                     :relation :r :object-namespace :ns
                                     :object-key "o" :producer :p
                                     :standing :inferred)))
        (is-true (slot-exists-p b 'graph-db.spacetime::object-key))
        (is-false (slot-exists-p u 'graph-db.spacetime::object-key))))))

(test extra-slots-land-on-the-parent-so-both-arities-inherit-them
  (with-claim-graph (g)
    (declare (ignorable g))
    (let (u b)
      (with-transaction ()
        (setq u (make-ct-claim-unary :subject-namespace :ns
                                     :subject-key "s1" :relation :r
                                     :producer :rule-a :standing :inferred
                                     :weight 1.5d0))
        (setq b (make-ct-claim-binary :subject-namespace :ns
                                      :subject-key "s1" :relation :r
                                      :object-namespace :ns :object-key "o1"
                                      :producer :rule-a :standing :inferred
                                      :weight 2.5d0)))
      (is (= 1.5d0 (ct-weight u)))
      (is (= 2.5d0 (ct-weight b))))))

(test the-registry-maps-a-parent-to-its-arity-subclasses
  (let ((f (claim-family 'ct-claim)))
    (is (eq 'ct-claim (claim-family-parent f)))
    (is (eq 'ct-claim-unary (claim-family-unary f)))
    (is (eq 'ct-claim-binary (claim-family-binary f)))
    (signals unknown-claim-family (claim-family 'no-such-claim))))

(test standing-is-validated-at-construction
  "A claim cannot be built with a standing outside the vocabulary."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals invalid-standing
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                             :relation :r :producer :p :standing :probably)))))
```

- [ ] **Step 3: Run the tests to verify they fail**

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(graph-db/spacetime-test:run-spacetime-tests)'
```

Expected: load failure — `claim.lisp` does not exist and `def-claim-classes` is undefined.

- [ ] **Step 4: Implement `claim.lisp`**

```lisp
;;;; The reified claim: a relation stored as a NODE, carrying the provenance
;;;; an edge cannot.  Endpoints are stored VALUES, never graph references --
;;;; a read-write transaction is single-graph, so an edge to a foreign node
;;;; could not be created at all (GH #131, design §2).

(in-package #:graph-db.spacetime)

(define-condition unknown-claim-family (spacetime-error)
  ((parent :initarg :parent :reader unknown-claim-family-parent))
  (:report (lambda (c s)
             (format s "~S names no claim family; DEF-CLAIM-CLASSES first."
                     (unknown-claim-family-parent c)))))

(defstruct (claim-family (:constructor %make-claim-family
                             (parent unary binary))
                         (:copier nil))
  "The three class names DEF-CLAIM-CLASSES generated together.  Registered so
CLAIMS-TOUCHING and DELETE-CLAIMS-BY-PRODUCER can reach the arity subclasses
from the parent name alone."
  (parent nil :read-only t)
  (unary nil :read-only t)
  (binary nil :read-only t))

(defvar *claim-families* (make-hash-table :test 'eq)
  "Parent class name -> CLAIM-FAMILY.")

(defun claim-family (parent)
  "The CLAIM-FAMILY registered for PARENT, or signal UNKNOWN-CLAIM-FAMILY."
  (or (gethash parent *claim-families*)
      (error 'unknown-claim-family :parent parent)))

(defparameter +claim-shared-slots+
  '((subject-namespace :initarg :subject-namespace
                       :accessor claim-subject-namespace)
    (subject-key :initarg :subject-key :accessor claim-subject-key)
    (relation :initarg :relation :accessor claim-relation)
    (producer :initarg :producer :accessor claim-producer)
    (rule-version :initarg :rule-version :accessor claim-rule-version
                  :initform nil)
    (method :initarg :method :accessor claim-method :initform nil)
    (standing :initarg :standing :accessor claim-standing)
    (confidence :initarg :confidence :accessor claim-confidence
                :initform nil)
    (extent-sexp :initarg :extent-sexp :accessor claim-extent-sexp
                 :initform nil)
    (geometry :initarg :geometry :accessor claim-geometry :initform nil))
  "Slots every claim carries, on the PARENT class.  Symbols live in this
package so two claim families share one set of accessors (design §5).")

(defparameter +claim-object-slots+
  '((object-namespace :initarg :object-namespace
                      :accessor claim-object-namespace)
    (object-key :initarg :object-key :accessor claim-object-key))
  "Slots only BINARY-CLAIM carries.  Their absence from UNARY-CLAIM is what
makes a unary claim unable to carry an object (design §3.1).")

(defmethod initialize-instance :after ((c t) &key)
  ;; Placeholder specialised in Step 5 below; see DEF-CLAIM-CLASSES.
  nil)

(defmacro def-claim-classes (parent graph-name &key extra-slots)
  "Define PARENT and its UNARY/BINARY subclasses in GRAPH-NAME, and register
the family.  The subsystem cannot ship these classes: DEF-VERTEX binds a node
type to a graph name and class names are globally unique, so a shipped class
would collide between tenants (design §4).

PARENT is deliberately given no constructor -- it exists to hold the shared
slots and the shared indexes, and carries no uniqueness constraint of its own
(design §3.3).  :EXTRA-SLOTS go on PARENT, so both arities inherit them."
  (let ((unary (intern (format nil "~A-UNARY" parent)))
        (binary (intern (format nil "~A-BINARY" parent))))
    `(progn
       (def-vertex ,parent () (,@+claim-shared-slots+ ,@extra-slots)
         ,graph-name)
       (def-vertex ,unary (,parent) () ,graph-name)
       (def-vertex ,binary (,parent) (,@+claim-object-slots+) ,graph-name)
       (fmakunbound ',(intern (format nil "MAKE-~A" parent)))
       (setf (gethash ',parent *claim-families*)
             (%make-claim-family ',parent ',unary ',binary))
       ',parent)))
```

Note the `fmakunbound`: `def-vertex` always generates `MAKE-<NAME>`, so the
parent's constructor is removed after the fact rather than never created. That
is the only lever the engine offers (design §3.3).

- [ ] **Step 5: Validate standing at construction**

Append to `claim.lisp`, replacing the placeholder method from Step 4:

```lisp
(defmethod initialize-instance :after ((c claim-standing-mixin) &key)
  (check-standing (claim-standing c)))
```

This needs a class to specialise on that exists before any tenant calls the
macro. Add it above `def-claim-classes` and make the generated parent inherit
it:

```lisp
(defclass claim-standing-mixin () ()
  (:documentation "Specialisation point for the STANDING check.  Not a node
class -- it holds no slots and is never persisted."))
```

and change the parent's `def-vertex` line to
`(def-vertex ,parent (claim-standing-mixin) ...)`.

- [ ] **Step 6: Add the ASDF components**

`(:file "claim")` after `(:file "allen")` in `graph-db/spacetime`, and
`(:file "claim-tests")` after `(:file "conformance-tests")` in
`graph-db/spacetime-test`. Both systems are `:serial t`.

- [ ] **Step 7: Run the tests to verify they pass**

Same command as Step 3. Expected: the six new tests pass, and the existing 47
spacetime tests still pass.

- [ ] **Step 8: Commit**

```bash
git add spacetime/claim.lisp spacetime/package.lisp \
        tests/spacetime/claim-tests.lisp graph-db.asd
git commit -m "feat(spacetime): the claim class hierarchy and its macro (#131)"
```

---

### Task 2: Identity — the two uniqueness constraints

**Files:**
- Modify: `spacetime/claim.lisp` (emit two `def-unique` forms)
- Create: `tests/spacetime/claim-identity-tests.lisp`
- Modify: `graph-db.asd`

**Interfaces:**
- Consumes: `def-claim-classes` (Task 1).
- Produces: no new functions — the macro's expansion gains two declarations.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/claim-identity-tests.lisp`:

```lisp
;;;; Claim identity: what the constraint permits and forbids (design §6).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun make-b (&key (producer :rule-a) (subject "s1") (object "o1")
                    (relation :r) (standing :inferred))
  (make-ct-claim-binary :subject-namespace :ns :subject-key subject
                        :relation relation
                        :object-namespace :ns :object-key object
                        :producer producer :standing standing))

(defun make-u (&key (producer :rule-a) (subject "s1") (relation :r))
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred))

(test two-producers-may-disagree
  "Design §6.2.  This is the entire reason for reifying: an edge model would
have to resolve this at write time."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-b :producer :rule-a :object "yes")
        (make-b :producer :operator-o :object "no")))))

(test one-producer-may-relate-a-subject-to-many-objects
  "Design §6.2 -- an ordinary one-to-many.  This is what breaks if the unary
constraint is wrongly declared on the PARENT class."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-b :object "o1")
        (make-b :object "o2")
        (make-b :object "o3")))))

(test the-same-producer-may-not-assert-the-identical-claim-twice
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-b)))))

(test a-unary-claim-deduplicates
  "THE test for the whole structural decision (design §3.1, §10).  With a
single class and a nullable object slot this FAILS: #107 exempts any tuple
containing a null, so the duplicate commits silently."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-u)))))

(test unary-and-binary-claims-do-not-collide
  "They are constrained separately, so a unary claim and a binary claim with
the same producer, subject and relation coexist."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-u :producer :rule-a :subject "s1" :relation :r)
        (make-b :producer :rule-a :subject "s1" :relation :r)))))

(test differing-in-any-identity-component-makes-a-distinct-claim
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (finishes
      (with-transaction ()
        (make-b :producer :rule-b)
        (make-b :subject "s2")
        (make-b :object "o2")
        (make-b :relation :other)))))

(test rule-version-is-not-part-of-identity
  "Design §6.1: PRODUCER excludes the version, so re-running a rule at a new
version collides with its own prior claim rather than adding a second one."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((c (make-b))) (setf (claim-rule-version c) "v1")))
    (signals graph-db:unique-constraint-violation
      (with-transaction ()
        (let ((c (make-b))) (setf (claim-rule-version c) "v2"))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Same command as Task 1 Step 3. Expected: the four `signals` tests FAIL — no
constraint exists yet, so every duplicate commits. The `finishes` tests pass
already, which is correct: they assert what must remain legal.

- [ ] **Step 3: Emit the constraints**

In `def-claim-classes`, after the three `def-vertex` forms:

```lisp
       ;; The unary constraint goes on UNARY, never on PARENT: PARENT has
       ;; exactly the unary slot set, so declaring it there would bind
       ;; BINARY too (CLASS-UNIQUE-TUPLE-SPECS matches on SUBTYPEP) and
       ;; forbid one producer relating a subject to several objects
       ;; (design §3.2).
       (def-unique ,unary (producer subject-namespace subject-key relation)
         ,graph-name)
       (def-unique ,binary (producer subject-namespace subject-key
                            object-namespace object-key relation)
         ,graph-name)
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: all seven identity tests pass; nothing earlier
regressed.

- [ ] **Step 5: Prove the unary test is non-vacuous**

Temporarily change the unary constraint to include the object slots — i.e.
make it identical to the binary one — which is what a single-class design
would produce. `class-unique-tuple-specs` requires every named slot to exist
in the class, so it will no longer apply to `unary-claim` at all.

Re-run. Expected: `a-unary-claim-deduplicates` FAILS. **Restore the constraint
exactly** and re-run to confirm green. Record what the broken run showed.

- [ ] **Step 6: Commit**

```bash
git add spacetime/claim.lisp tests/spacetime/claim-identity-tests.lisp \
        graph-db.asd
git commit -m "feat(spacetime): claim identity -- two complete constraints (#131)"
```

---

### Task 3: The indexes and the inverse query

**Files:**
- Modify: `spacetime/claim.lisp` (emit three `def-index` forms)
- Create: `spacetime/claim-query.lisp`, `tests/spacetime/claim-query-tests.lisp`
- Modify: `graph-db.asd`

**Interfaces:**
- Consumes: `claim-family`, the class hierarchy.
- Produces: `(claims-touching graph claim-class namespace key &key role)` → list of claim nodes; `role` is `:subject`, `:object` or `:either` (default).

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/claim-query-tests.lisp`:

```lisp
;;;; The inverse query (design §8).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test claims-touching-finds-a-subject-across-both-arities
  (with-claim-graph (g)
    (with-transaction ()
      (make-u :subject "alpha")
      (make-b :subject "alpha" :object "beta"))
    (is (= 2 (length (claims-touching g 'ct-claim :ns "alpha"))))))

(test claims-touching-finds-an-object
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha" :object "beta"))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"))))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :object))))
    (is (= 0 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :subject))))))

(test claims-touching-does-not-cross-namespaces
  "The namespace is part of the key, so the same string in another namespace
is a different endpoint."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha"))
    (is (= 0 (length (claims-touching g 'ct-claim :other "alpha"))))))

(test claims-touching-returns-each-claim-once
  "A claim naming the same endpoint as BOTH subject and object must not be
returned twice by the :EITHER union."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "self" :object "self"))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "self"))))))

(test claims-touching-signals-on-an-unregistered-parent
  (with-claim-graph (g)
    (signals unknown-claim-family
      (claims-touching g 'no-such-claim :ns "x"))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: load failure — `claim-query.lisp` does not exist.

- [ ] **Step 3: Emit the indexes**

In `def-claim-classes`, after the two `def-unique` forms:

```lisp
       ;; Subject index on PARENT reaches both arities via SUBTYPEP.  Object
       ;; index on BINARY, where those slots live -- declaring it on PARENT
       ;; also works (%APPLICABLE-INDEX-DESCRIPTORS requires every named slot
       ;; to exist) but reads as a mistake.  PRODUCER index exists so the
       ;; regeneration sweep is not a full scan (design §4, plan note 2).
       (def-index ,parent (subject-namespace subject-key) ,graph-name)
       (def-index ,binary (object-namespace object-key) ,graph-name)
       (def-index ,parent (producer) ,graph-name)
```

- [ ] **Step 4: Implement the query**

`spacetime/claim-query.lisp`:

```lisp
;;;; Reading claims back: the inverse query, the extent codec, and the
;;;; regeneration sweep (GH #131, design §7-§8).

(in-package #:graph-db.spacetime)

(defun claims-touching (graph claim-class namespace key
                        &key (role :either))
  "Claims in GRAPH naming (NAMESPACE, KEY) as subject, object, or either.
CLAIM-CLASS is the PARENT class name; one call covers both arities.  Answers
from the claim graph's own indexes -- no cross-graph read, no snapshot, which
is what makes it implementable in this unit (design §8)."
  (let* ((family (claim-family claim-class))
         (want (list namespace key))
         (subjects (when (member role '(:subject :either))
                     (index-lookup graph (claim-family-parent family)
                                   '(subject-namespace subject-key) want)))
         (objects (when (member role '(:object :either))
                    (index-lookup graph (claim-family-binary family)
                                  '(object-namespace object-key) want))))
    ;; A claim naming one endpoint as BOTH subject and object appears in both
    ;; lookups; the union must still return it once.
    (if (and subjects objects)
        (remove-duplicates (append subjects objects) :key #'id :test #'equalp)
        (or subjects objects))))
```

- [ ] **Step 5: Add the ASDF components**

`(:file "claim-query")` after `(:file "claim")`, and `(:file "claim-query-tests")` after `(:file "claim-identity-tests")`.

- [ ] **Step 6: Run the tests to verify they pass**

Expected: all five query tests pass.

- [ ] **Step 7: Commit**

```bash
git add spacetime/claim.lisp spacetime/claim-query.lisp \
        tests/spacetime/claim-query-tests.lisp graph-db.asd
git commit -m "feat(spacetime): claim indexes and the inverse query (#131)"
```

---

### Task 4: The extent codec on a claim

**Files:**
- Modify: `spacetime/claim-query.lisp`
- Modify: `tests/spacetime/claim-query-tests.lisp`

**Interfaces:**
- Consumes: S1a's `extent->sexp` / `sexp->extent`, `make-granule-instant`.
- Produces: `(claim-extent claim)` → `temporal-extent` or `nil`; `(setf claim-extent)`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/claim-query-tests.lisp`:

```lisp
(test a-claim-carries-a-temporal-extent-across-a-reopen
  "Design §7: the slot holds the sexp, the accessor decodes.  The reopen is
the point -- an in-memory round trip would not exercise serialization."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (let ((c (make-u)))
                   (setf (claim-extent c)
                         (make-granule-instant (ts 2026 3 15) :month
                                               :standing :observed))
                   (setq id (id c)))))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let* ((graph-db:*graph* g2)
                    (e (claim-extent (lookup-vertex id))))
               (is (eq :instant (extent-kind e)))
               (is (eq :month (extent-precision e)))
               (is (eq :observed (extent-standing e)))
               (is (eq (extent-start e) (extent-end e))
                   "the instant coupling survives storage"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test a-claim-without-an-extent-reads-as-nil-not-as-an-error
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (is (null (claim-extent (make-u)))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: `claim-extent` is undefined.

- [ ] **Step 3: Implement the accessor**

Append to `spacetime/claim-query.lisp`:

```lisp
(defun claim-extent (claim)
  "CLAIM's TEMPORAL-EXTENT, decoded from the stored sexp, or NIL.  The stored
form is EXTENT-SEXP; the two never share a name so neither is mistaken for the
other (design §7)."
  (let ((s (claim-extent-sexp claim)))
    (when s (sexp->extent s))))

(defun (setf claim-extent) (extent claim)
  "Store EXTENT on CLAIM as its sexp.  Only values GRAPH-DB:SERIALIZE already
handles reach the heap, so no core type byte is reserved."
  (setf (claim-extent-sexp claim) (and extent (extent->sexp extent)))
  extent)
```

- [ ] **Step 4: Run the tests to verify they pass**

Expected: both new tests pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/claim-query.lisp tests/spacetime/claim-query-tests.lisp
git commit -m "feat(spacetime): decode a claim's temporal extent on read (#131)"
```

---

### Task 5: Regeneration

**Files:**
- Modify: `spacetime/claim-query.lisp`
- Modify: `tests/spacetime/claim-query-tests.lisp`

**Interfaces:**
- Produces: `(delete-claims-by-producer graph claim-class producer)` → count of claims marked deleted.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/claim-query-tests.lisp`:

```lisp
(test the-sweep-removes-only-the-named-producers-claims
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "o1")
      (make-b :producer :rule-a :object "o2")
      (make-u :producer :rule-a)
      (make-b :producer :rule-b :object "o1"))
    (is (= 3 (with-transaction ()
               (delete-claims-by-producer g 'ct-claim :rule-a))))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "s1"))))))

(test regeneration-leaves-no-orphan-when-a-rule-stops-producing-a-claim
  "Design §6.4 -- the case the constraint alone cannot fix.  v1 produces two
claims, v2 produces one; without the sweep the dropped claim would survive
forever, because no upsert ever touches it."
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "kept")
      (make-b :producer :rule-a :object "dropped"))
    (with-transaction ()
      (delete-claims-by-producer g 'ct-claim :rule-a)
      (make-b :producer :rule-a :object "kept"))
    (let ((live (claims-touching g 'ct-claim :ns "s1")))
      (is (= 1 (length live)))
      (is (string= "kept" (claim-object-key (first live)))))))

(test the-sweep-makes-a-claim-re-insertable
  "After a sweep the constraint must not still be holding the old key."
  (with-claim-graph (g)
    (with-transaction () (make-b))
    (with-transaction () (delete-claims-by-producer g 'ct-claim :rule-a))
    (finishes (with-transaction () (make-b)))))

(test the-sweep-signals-on-an-unregistered-parent
  (with-claim-graph (g)
    (signals unknown-claim-family
      (delete-claims-by-producer g 'no-such-claim :rule-a))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: `delete-claims-by-producer` is undefined.

- [ ] **Step 3: Implement the sweep**

Append to `spacetime/claim-query.lisp`:

```lisp
(defun delete-claims-by-producer (graph claim-class producer)
  "Mark every claim PRODUCER wrote as deleted; return how many.  CLAIM-CLASS
is the PARENT, so one call sweeps both arities.

Regeneration is sweep-then-insert, and the uniqueness constraint is NOT what
makes it work: a rule that stops producing a claim leaves an orphan no upsert
can remove (design §6.4).  Uses the PRODUCER index, so this is O(matching)
rather than a scan of every claim."
  (let ((family (claim-family claim-class))
        (n 0))
    (dolist (c (index-lookup graph (claim-family-parent family)
                             '(producer) (list producer))
             n)
      (mark-deleted c)
      (incf n))))
```

- [ ] **Step 4: Run the tests to verify they pass**

Expected: all four sweep tests pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/claim-query.lisp tests/spacetime/claim-query-tests.lisp
git commit -m "feat(spacetime): delete-claims-by-producer, the regeneration sweep (#131)"
```

---

### Task 6: The concurrency gate

**Files:**
- Create: `tests/spacetime/claim-concurrency-tests.lisp`
- Modify: `graph-db.asd`

**Non-negotiable**, as in #107 — this unit's constraints run on the commit path, and uniqueness that holds single-threaded but fails under contention is not uniqueness.

- [ ] **Step 1: Write the test**

`tests/spacetime/claim-concurrency-tests.lisp`:

```lisp
;;;; The phantom the commit lock defeats, on a claim (design §6.3, §10).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test concurrent-identical-claims-exactly-one-wins
  "Eight threads racing the same claim: one commits, seven are rejected."
  (with-claim-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((graph-db:*graph* g))
                   (handler-case
                       (progn (with-transaction ()
                                (make-b :producer :race :object "one"))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects)
      (is (= 1 (length (claims-touching g 'ct-claim :ns "s1")))
          "one claim exists"))))

(test concurrent-unary-claims-exactly-one-wins
  "The same gate on the unary arity -- its constraint is a different index,
so passing on binary claims proves nothing about it."
  (with-claim-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((graph-db:*graph* g))
                   (handler-case
                       (progn (with-transaction ()
                                (make-u :producer :race-u))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects))))
```

- [ ] **Step 2: Add the ASDF component and run**

`(:file "claim-concurrency-tests")` after `(:file "claim-query-tests")`.

Expected: both pass. If either reports 8 commits, the constraint is not
reaching the commit path — stop and report rather than adjusting the test.

- [ ] **Step 3: Commit**

```bash
git add tests/spacetime/claim-concurrency-tests.lisp graph-db.asd
git commit -m "test(spacetime): the eight-thread claim race, both arities (#131)"
```

---

### Task 7: Conformance and documentation

**Files:**
- Modify: `tests/spacetime/conformance-tests.lisp`
- Modify: `docs/vivace-graph-v3-doc.org`

- [ ] **Step 1: Write the conformance tests**

Append to `tests/spacetime/conformance-tests.lisp`:

```lisp
(test a-unary-claim-is-distinguishable-from-an-unknown-object
  "Design §3.1 and §10.  Structural absence and epistemic absence must not
share a spelling -- the defect class this whole subsystem exists to prevent,
arriving in the first record built on top of it."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((unary (make-u))
            (unknown (make-ct-claim-binary
                      :subject-namespace :ns :subject-key "s9"
                      :relation :r :object-namespace :ns :object-key "?"
                      :producer :p :standing :indeterminate)))
        ;; Structural absence: the slot does not exist at all.
        (is-false (slot-exists-p unary 'graph-db.spacetime::object-key))
        ;; Epistemic absence: the slot exists, and STANDING says why.
        (is-true (slot-exists-p unknown 'graph-db.spacetime::object-key))
        (is-true (standing-absence-p (claim-standing unknown)))
        (is-false (standing-absence-p (claim-standing unary)))))))

(test a-claims-standing-and-its-extents-standing-are-independent
  "Design §5: one records how the claim came to be known, the other how the
TIME was known."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((c (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                                    :relation :r :producer :p
                                    :standing :asserted)))
        (setf (claim-extent c)
              (make-granule-instant (ts 2026 1 15) :day :standing :observed))
        (is (eq :asserted (claim-standing c)))
        (is (eq :observed (extent-standing (claim-extent c))))))))
```

- [ ] **Step 2: Run the spacetime suite**

Expected: green.

- [ ] **Step 3: Document the claim in the org manual**

Extend the `graph-db/spacetime` chapter added by #130 with a claim section
covering: that a claim is a node and endpoints are stored values, with the
transaction reason; the three-class hierarchy and why arity is a type; what
the constraint permits and forbids, with the two-producers and one-to-many
cases shown; that regeneration is sweep-then-insert; `claims-touching` and
`claim-extent` with a worked example; and that resolution, `precision` and
`fraction` belong to a later unit.

- [ ] **Step 4: Run the full graph-db suite**

```bash
sbcl --dynamic-space-size 12288 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))' \
  > /tmp/full-131.log 2>&1
```

Expected: **3526 checks, 3516 pass, 10 skip, 0 fail** — identical to the
pre-branch baseline, because nothing here touches the engine core. A different
number means this plan changed core behaviour; report it rather than
explaining it away.

- [ ] **Step 5: Commit**

```bash
git add tests/spacetime/conformance-tests.lisp docs/vivace-graph-v3-doc.org
git commit -m "test(spacetime): claim conformance, and document the record (#131)"
```

---

## Self-Review

**Spec coverage.** §2 no edges → Task 1's stored-value slots. §3.1 arity as a type → Task 1 Step 2's slot-existence test, Task 2's unary dedup. §3.2 shape and the parent-constraint trap → Task 2 Step 3's comment and the one-to-many test. §3.3 parent not instantiable → Task 1's `fmakunbound` and test. §4 macro and indexes → Tasks 1 and 3. §5 record → Task 1. §6.1 producer excludes version → Task 2's `rule-version-is-not-part-of-identity`. §6.2/§6.3 permit/forbid → Task 2. §6.4 regeneration → Task 5. §7 storage → Task 4. §8 inverse query → Task 3. §9 deferrals → nothing to implement; the plan adds no `precision`, `fraction`, or resolver. §10 testing → Tasks 2, 5, 6, 7. §11 acceptance → out of scope by definition. §12 version floor → Task 7 Step 4.

**Type consistency.** `claim-family` returns a `claim-family` struct in Tasks 1, 3 and 5. `index-lookup`'s multi-slot form takes the slot list and a list of values, used identically in Tasks 3 and 5. `claim-extent-sexp` is the slot accessor; `claim-extent` is the decoding function; they never swap.

**Two risks checked against the engine before this plan shipped, rather than left for the implementer to discover.**

*The standing mixin works.* Task 1 Step 5 makes `claim-standing-mixin`, a plain `standard-class`, a superclass of a `def-vertex` class whose metaclass is `node-class`. I verified SBCL accepts this: the class defines, finalizes, and reports its 19 inherited slots. No fallback needed.

*`closer-mop` is not available here.* `graph-db`'s package `:use`s `sb-mop` on SBCL and `closer-mop` only on CCL and LispWorks (`package.lisp:7-10`), and this test package uses neither. Task 1's slot-existence test therefore uses CL's `slot-exists-p` on instances. Any later task reaching for a MOP call must go through `graph-db::class-slots`, not `closer-mop`.
