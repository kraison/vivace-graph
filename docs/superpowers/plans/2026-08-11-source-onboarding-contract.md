# The source onboarding contract — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `graph-db/spacetime`'s source onboarding contract — seven declarable facets enforced at declaration time — and the endpoint resolution that the identity facet makes possible.

**Architecture:** `def-source` defines a tenant's node class and will not expand without all seven facets, so a non-conforming source cannot exist. Identity is the only facet with behaviour: it emits an index and registers the class under its namespace, which is what `resolve-endpoint` reads. The other six are validated, stored and read back through `source-contract`.

**Tech Stack:** Common Lisp (SBCL), `graph-db/core`, `graph-db/spacetime` (S1a + S1b), FiveAM.

**Spec:** `docs/superpowers/specs/2026-08-11-source-onboarding-contract-design.md`
**Issue:** #132. **Parent:** #108. **Split out:** #138 (registration, deferred).

## Global Constraints

- **80-column hard limit** in all Lisp source: code, comments, docstrings and strings alike. Spaces only, never tabs.
- **Comments terse, pointing elsewhere** — state the non-obvious fact and cite `(design §N)` or `(GH #132)`.
- **No change to any file belonging to the `graph-db/core` ASDF system** — that is every `.lisp` file at the repository *root*. `graph-db.asd` is the sole exception.
- **Nothing may name a concept from any tenant application.** This unit is where that rule is most easily broken; the spec already had to be corrected once for it.
- **Every facet accepts `:none`**, uniformly, with no exceptions.
- **Fail-closed**: an unrecognised or `:none` disclosure class is treated as *more* restricted than every known one, never less.
- **SBCL only.** ECL is demoted to periodic; say explicitly when skipped.
- Work on a branch off `experiment`. Show the full diff under `## 📋 DIFF FOR REVIEW` before each commit. Do not push.
- If a run disagrees with the source, clear the FASL cache. Both paths are under `~/.cache`, neither is in the repository — copy them exactly:

```bash
rm -rf ~/.cache/common-lisp/sbcl-*/Users/kraison/work/vivace-graph-v3/spacetime
rm -rf ~/.cache/common-lisp/sbcl-*/Users/kraison/work/vivace-graph-v3/tests/spacetime
```

### One spec clarification this plan makes explicit

The spec says every facet accepts `:none` but does not say what `:identity :none` *means*. It means **records of this class are never endpoint targets**: no index is emitted, no namespace registration happens, and `resolve-endpoint` will never return one. That is coherent — such a class can still declare time, attribution, sensitivity and indexed text — and it keeps the uniform rule intact. Task 2 tests it.

---

## File Structure

| File | Responsibility |
|---|---|
| `spacetime/source.lisp` | facet vocabulary, shape validation, `def-source`, the two registries, `source-contract` |
| `spacetime/resolve.lisp` | `resolve-endpoint`, `disclosable-p` |
| `tests/spacetime/source-tests.lisp` | conformance by construction, facet shapes, `:none` |
| `tests/spacetime/resolve-tests.lisp` | resolution, ambiguity, the transaction guard, fail-closed |
| `graph-db.asd` | two components per system |

---

### Task 1: The facet vocabulary, `def-source`, and `source-contract`

**Files:**
- Create: `spacetime/source.lisp`, `tests/spacetime/source-tests.lisp`
- Modify: `spacetime/package.lisp`, `spacetime/conditions.lisp`, `graph-db.asd`

**Scope:** declaration and validation only. **No `def-index`, no namespace registry, no resolution** — Task 2 and Task 3 add those.

**Interfaces:**
- Produces: `def-source (name graph-name slots &key identity space time attribution sensitivity registration indexed-text)`; `source-contract (class)` → `source-facets` struct; readers `source-facets-identity` / `-space` / `-time` / `-attribution` / `-sensitivity` / `-registration` / `-indexed-text` / `-class` / `-graph`; `+source-facets+`; conditions `missing-source-facet`, `invalid-source-facet`, `not-a-source`.

- [ ] **Step 1: Add the conditions**

Append to `spacetime/conditions.lisp`:

```lisp
(define-condition missing-source-facet (spacetime-error)
  ((name :initarg :name :reader missing-source-facet-name)
   (facets :initarg :facets :reader missing-source-facet-facets))
  (:report (lambda (c s)
             (format s "DEF-SOURCE ~S is missing required facets: ~{~S~^, ~}.~
  Every facet must be given; use :NONE to say one does not apply."
                     (missing-source-facet-name c)
                     (missing-source-facet-facets c)))))

(define-condition invalid-source-facet (spacetime-error)
  ((facet :initarg :facet :reader invalid-source-facet-facet)
   (value :initarg :value :reader invalid-source-facet-value)
   (reason :initarg :reason :reader invalid-source-facet-reason))
  (:report (lambda (c s)
             (format s "Bad ~S facet ~S: ~A."
                     (invalid-source-facet-facet c)
                     (invalid-source-facet-value c)
                     (invalid-source-facet-reason c)))))

(define-condition not-a-source (spacetime-error)
  ((class :initarg :class :reader not-a-source-class))
  (:report (lambda (c s)
             (format s "~S was not defined with DEF-SOURCE."
                     (not-a-source-class c)))))
```

- [ ] **Step 2: Add the exports**

In `spacetime/package.lisp`, add to the existing `(:export ...)` form:

```lisp
   ;; source onboarding contract (GH #132)
   #:def-source #:source-contract #:+source-facets+
   #:source-facets #:source-facets-p #:source-facets-class
   #:source-facets-graph #:source-facets-identity #:source-facets-space
   #:source-facets-time #:source-facets-attribution
   #:source-facets-sensitivity #:source-facets-registration
   #:source-facets-indexed-text
   #:missing-source-facet #:invalid-source-facet #:not-a-source
   #:resolve-endpoint #:disclosable-p #:+disclosure-classes+
   #:unknown-namespace #:ambiguous-endpoint
   #:resolution-in-transaction
```

- [ ] **Step 3: Write the failing tests**

`tests/spacetime/source-tests.lisp`:

```lisp
;;;; The source onboarding contract: declaration and validation (GH #132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *source-graph-name* :graph-db-source-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *source-graph-name* graph-db::*schema-node-metadata*) nil))

(def-source st-report :graph-db-source-test
    ((headline :initarg :headline :accessor st-headline)
     (report-id :initarg :report-id :accessor st-report-id))
  :identity     (:namespace :st-reports :key-slot report-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC-BY-4.0" :citation "Example Reports")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(test a-source-records-all-seven-facets
  (let ((c (source-contract 'st-report)))
    (is (eq 'st-report (source-facets-class c)))
    (is (equal '(:namespace :st-reports :key-slot report-id)
               (source-facets-identity c)))
    (is (eq :none (source-facets-space c)))
    (is (eq :none (source-facets-time c)))
    (is (equal '(:class :public) (source-facets-sensitivity c)))
    (is (eq :none (source-facets-registration c)))
    (is (eq :none (source-facets-indexed-text c)))))

(test the-vocabulary-is-seven-facets
  (is (= 7 (length +source-facets+)))
  (is (= 7 (length (remove-duplicates +source-facets+)))))

(test omitting-any-facet-fails-to-expand
  "Design §2: enforcement is structural.  A non-conforming source class
cannot be defined at all, so the violation surfaces at macroexpansion."
  (dolist (omit +source-facets+)
    (let ((form `(def-source st-bad :graph-db-source-test ((a :initarg :a))
                   ,@(loop for f in +source-facets+
                           unless (eq f omit)
                             append (list f :none)))))
      (signals missing-source-facet (macroexpand-1 form)))))

(test none-is-accepted-for-every-facet
  "Design §1: the rule is uniform, with no exceptions.  One expansion with
every facet :NONE proves it for all seven at once -- looping over the facets
here would expand an identical form seven times and assert nothing extra."
  (finishes
    (macroexpand-1
     `(def-source st-allnone :graph-db-source-test ((a :initarg :a))
        ,@(loop for g in +source-facets+ append (list g :none))))))

(test a-malformed-facet-signals-and-names-the-facet
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad2 :graph-db-source-test ((a :initarg :a))
        :identity (:namespace :x)          ; missing :KEY-SLOT
        :space :none :time :none :attribution :none
        :sensitivity :none :registration :none :indexed-text :none)))
  (signals invalid-source-facet
    (macroexpand-1
     `(def-source st-bad3 :graph-db-source-test ((a :initarg :a))
        :identity :none :space :none :time :none
        :attribution (:licence "x")        ; missing :CITATION
        :sensitivity :none :registration :none :indexed-text :none))))

(test source-contract-signals-for-a-non-source
  "Design §5: \"declared nothing\" and \"is not a source\" are different
facts; NIL for both would let a consumer treat an unconverted class as a
conforming one with empty facets."
  (signals not-a-source (source-contract 'uq-claim)))
```

- [ ] **Step 4: Run the tests to verify they fail**

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(graph-db/spacetime-test:run-spacetime-tests)'
```

Expected: load failure — `source.lisp` does not exist and `def-source` is undefined.

- [ ] **Step 5: Implement `source.lisp`**

```lisp
;;;; The source onboarding contract: what a source declares about itself.
;;;;
;;;; Enforcement is STRUCTURAL -- DEF-SOURCE defines the class and will not
;;;; expand without all seven facets, so a non-conforming source cannot be
;;;; defined at all (GH #132, design §2).  A class defined with plain
;;;; DEF-VERTEX is simply not a source, which is correct rather than a gap.

(in-package #:graph-db.spacetime)

(defparameter +source-facets+
  '(:identity :space :time :attribution :sensitivity :registration
    :indexed-text)
  "The seven facets.  Every one is required and every one accepts :NONE --
the uniform rule is what makes the contract learnable (design §1).")

(defstruct (source-facets (:conc-name source-facets-) (:copier nil))
  "A class's declared facets, read back through SOURCE-CONTRACT."
  (class nil :read-only t)
  (graph nil :read-only t)
  (identity nil :read-only t)
  (space nil :read-only t)
  (time nil :read-only t)
  (attribution nil :read-only t)
  (sensitivity nil :read-only t)
  (registration nil :read-only t)
  (indexed-text nil :read-only t))

(defvar *source-contracts* (make-hash-table :test 'eq)
  "Class name -> SOURCE-FACETS.")

(defun source-contract (class)
  "CLASS's declared facets.  Signals NOT-A-SOURCE when CLASS was not defined
with DEF-SOURCE -- \"declared nothing\" and \"is not a source\" are different
facts (design §5)."
  (or (gethash class *source-contracts*)
      (error 'not-a-source :class class)))

(defun %plist-has-p (plist &rest keys)
  (every (lambda (k) (member k plist)) keys))

(defun %check-facet (facet value)
  "Return VALUE if it is a well-formed FACET, else signal.  :NONE is always
well-formed (design §1)."
  (flet ((bad (reason)
           (error 'invalid-source-facet :facet facet :value value
                                        :reason reason)))
    (unless (eq value :none)
      (unless (listp value) (bad "expected a plist or :NONE"))
      (ecase facet
        (:identity
         (unless (%plist-has-p value :namespace :key-slot)
           (bad "expected (:NAMESPACE <keyword> :KEY-SLOT <slot>)")))
        (:space
         (unless (%plist-has-p value :geometry-slot :kind :precision)
           (bad "expected (:GEOMETRY-SLOT <slot> :KIND k :PRECISION p)")))
        (:time
         (unless (%plist-has-p value :extent-fn)
           (bad "expected (:EXTENT-FN <function-name>)")))
        (:attribution
         (unless (%plist-has-p value :licence :citation)
           (bad "expected (:LICENCE <string> :CITATION <string>)")))
        (:sensitivity
         (unless (%plist-has-p value :class)
           (bad "expected (:CLASS <keyword>)")))
        ;; Uninterpreted here; #138 defines its shape (design §3.3).
        (:registration value)
        (:indexed-text
         (unless (%plist-has-p value :text-fn)
           (bad "expected (:TEXT-FN <function-name>)")))))
    value))

(defmacro def-source (name graph-name slots
                      &key (identity nil identity-p)
                           (space nil space-p)
                           (time nil time-p)
                           (attribution nil attribution-p)
                           (sensitivity nil sensitivity-p)
                           (registration nil registration-p)
                           (indexed-text nil indexed-text-p))
  "Define NAME as a source vertex in GRAPH-NAME with SLOTS, declaring all
seven facets.  Omitting any signals MISSING-SOURCE-FACET at macroexpansion;
use :NONE to say a facet does not apply (design §2)."
  (let ((missing (append (unless identity-p '(:identity))
                         (unless space-p '(:space))
                         (unless time-p '(:time))
                         (unless attribution-p '(:attribution))
                         (unless sensitivity-p '(:sensitivity))
                         (unless registration-p '(:registration))
                         (unless indexed-text-p '(:indexed-text)))))
    (when missing
      (error 'missing-source-facet :name name :facets missing)))
  (%check-facet :identity identity)
  (%check-facet :space space)
  (%check-facet :time time)
  (%check-facet :attribution attribution)
  (%check-facet :sensitivity sensitivity)
  (%check-facet :registration registration)
  (%check-facet :indexed-text indexed-text)
  `(progn
     (graph-db:def-vertex ,name () ,slots ,graph-name)
     (setf (gethash ',name *source-contracts*)
           (make-source-facets :class ',name :graph ',graph-name
                               :identity ',identity :space ',space
                               :time ',time :attribution ',attribution
                               :sensitivity ',sensitivity
                               :registration ',registration
                               :indexed-text ',indexed-text))
     ',name))
```

- [ ] **Step 6: Add the ASDF components**

`(:file "source")` after `(:file "claim-query")` in `graph-db/spacetime`, and `(:file "source-tests")` after `(:file "claim-concurrency-tests")` — but **before** `(:file "conformance-tests")`, which must stay last. Both systems are `:serial t`.

- [ ] **Step 7: Run the tests to verify they pass**

Same command as Step 4. Expected: the six new tests pass and nothing earlier regressed.

- [ ] **Step 8: Commit**

```bash
git add spacetime/source.lisp spacetime/conditions.lisp \
        spacetime/package.lisp tests/spacetime/source-tests.lisp graph-db.asd
git commit -m "feat(spacetime): the source onboarding contract (#132)"
```

---

### Task 2: Identity's structural consequences

**Files:**
- Modify: `spacetime/source.lisp`
- Modify: `tests/spacetime/source-tests.lisp`

**Interfaces:**
- Consumes: `def-source`, `source-contract`.
- Produces: `*namespace-sources*` (namespace → list of class names); `namespace-sources (namespace)` → list, signalling `unknown-namespace` when the namespace has no registered classes.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/source-tests.lisp`:

```lisp
(def-source st-photo :graph-db-source-test
    ((sha :initarg :sha :accessor st-sha))
  :identity     (:namespace :st-media :key-slot sha)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  (:class :restricted)
  :registration :none
  :indexed-text :none)

(def-source st-note :graph-db-source-test
    ((body :initarg :body :accessor st-body))
  :identity     :none
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)

(test identity-registers-the-class-under-its-namespace
  (is (member 'st-report (namespace-sources :st-reports)))
  (is (member 'st-photo (namespace-sources :st-media)))
  (is-false (member 'st-photo (namespace-sources :st-reports))))

(test identity-none-registers-nothing
  "Plan clarification: :IDENTITY :NONE means records of this class are never
endpoint targets.  It is legal, and it registers no namespace."
  (dolist (ns '(:st-reports :st-media))
    (is-false (member 'st-note (namespace-sources ns)))))

(test an-unregistered-namespace-signals
  "Design §4: an unknown namespace is a programming error, distinct from a
key that simply matches nothing."
  (signals unknown-namespace (namespace-sources :st-no-such)))

(test identity-emits-an-index-on-the-key-slot
  "Without this index RESOLVE-ENDPOINT would be a full scan.  Proved by
using the index rather than by inspecting the registry: INDEX-LOOKUP signals
if no index covers the class and slot, so a successful call IS the evidence."
  (with-source-graph (g)
    (with-transaction () (make-st-report :headline "x" :report-id "idx-1"))
    (is (= 1 (length (graph-db:index-lookup g 'st-report '(report-id)
                                            "idx-1"))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: `namespace-sources` is undefined.

- [ ] **Step 3: Implement**

Add the condition to `spacetime/conditions.lisp`:

```lisp
(define-condition unknown-namespace (spacetime-error)
  ((namespace :initarg :namespace :reader unknown-namespace-namespace))
  (:report (lambda (c s)
             (format s "No source class is registered under namespace ~S."
                     (unknown-namespace-namespace c)))))
```

In `spacetime/source.lisp`, add the registry and accessor:

```lisp
(defvar *namespace-sources* (make-hash-table :test 'eq)
  "Namespace keyword -> list of class names declaring it.  Populated by the
:IDENTITY facet, and read by RESOLVE-ENDPOINT (design §4).")

(defun namespace-sources (namespace)
  "Class names registered under NAMESPACE.  Signals UNKNOWN-NAMESPACE when
none are -- a typo or an unloaded system, distinct from a key that matches
nothing (design §4)."
  (or (gethash namespace *namespace-sources*)
      (error 'unknown-namespace :namespace namespace)))

(defun %register-identity (class identity)
  "Register CLASS under its namespace.  :NONE registers nothing: such a
class is never an endpoint target (plan clarification)."
  (unless (eq identity :none)
    (let ((ns (getf identity :namespace)))
      (pushnew class (gethash ns *namespace-sources*)))))
```

and extend `def-source`'s expansion, after the `def-vertex` form:

```lisp
     ,@(unless (eq identity :none)
         `((graph-db:def-index ,name (,(getf identity :key-slot))
             ,graph-name)))
     (%register-identity ',name ',identity)
```

- [ ] **Step 4: Run the tests to verify they pass**

Expected: the four new tests pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/source.lisp spacetime/conditions.lisp \
        tests/spacetime/source-tests.lisp
git commit -m "feat(spacetime): identity emits an index and a namespace registration (#132)"
```

---

### Task 3: `resolve-endpoint`

**Files:**
- Create: `spacetime/resolve.lisp`, `tests/spacetime/resolve-tests.lisp`
- Modify: `spacetime/conditions.lisp`, `graph-db.asd`

**Interfaces:**
- Consumes: `namespace-sources`, `source-contract`, `source-facets-identity`.
- Produces: `resolve-endpoint (namespace key)` → node or `nil`; condition `ambiguous-endpoint`.

- [ ] **Step 1: Write the failing tests**

`tests/spacetime/resolve-tests.lisp`:

```lisp
;;;; Endpoint resolution, and the fail-closed sensitivity predicate (#132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defmacro with-source-graph ((g) &body body)
  "A fresh on-disk graph named *SOURCE-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *source-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(test resolve-endpoint-finds-a-record-by-external-key
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "one" :report-id "r-1")
      (make-st-report :headline "two" :report-id "r-2"))
    (let ((n (resolve-endpoint :st-reports "r-2")))
      (is-true n)
      (is (string= "two" (st-headline n))))))

(test a-key-that-matches-nothing-returns-nil
  "Distinct from an unknown namespace, which signals (design §4)."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (is (null (resolve-endpoint :st-reports "r-99")))))

(test an-unknown-namespace-signals-rather-than-returning-nil
  "Collapsing both to NIL would make a misspelled namespace
indistinguishable from an absent record."
  (with-source-graph (g)
    (declare (ignorable g))
    (signals unknown-namespace (resolve-endpoint :st-nope "r-1"))))

(test resolve-endpoint-refuses-to-run-in-a-read-write-transaction
  "Design §4.1: resolution can cross graphs, and the 3.0 contract permits
cross-graph reads only from a read-only snapshot or outside a transaction.
The caller's mistake is the call site, not the lookup."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (signals resolution-in-transaction
      (with-transaction () (resolve-endpoint :st-reports "r-1")))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: load failure — `resolve.lisp` does not exist.

- [ ] **Step 3: Implement**

Add to `spacetime/conditions.lisp`:

```lisp
(define-condition resolution-in-transaction (spacetime-error)
  ((namespace :initarg :namespace :reader resolution-in-transaction-namespace)
   (key :initarg :key :reader resolution-in-transaction-key))
  (:report (lambda (c s)
             (format s "RESOLVE-ENDPOINT ~S/~S was called inside a ~
read-write transaction.  Resolution can cross graphs, and a read-write ~
transaction is single-graph; resolve before opening it."
                     (resolution-in-transaction-namespace c)
                     (resolution-in-transaction-key c)))))

(define-condition ambiguous-endpoint (spacetime-error)
  ((namespace :initarg :namespace :reader ambiguous-endpoint-namespace)
   (key :initarg :key :reader ambiguous-endpoint-key)
   (classes :initarg :classes :reader ambiguous-endpoint-classes))
  (:report (lambda (c s)
             (format s "~S/~S resolves in more than one class: ~{~S~^, ~}.~
  An external key must be unique within its namespace."
                     (ambiguous-endpoint-namespace c)
                     (ambiguous-endpoint-key c)
                     (ambiguous-endpoint-classes c)))))
```

`spacetime/resolve.lisp`:

```lisp
;;;; Endpoint resolution: (namespace, external key) -> node.
;;;;
;;;; The forward direction of the pair #131 started -- CLAIMS-TOUCHING is the
;;;; inverse and needs no cross-graph read.  This one does, which is why it
;;;; refuses to run inside a read-write transaction (GH #132, design §4).

(in-package #:graph-db.spacetime)

(defun resolve-endpoint (namespace key)
  "The node in NAMESPACE whose external key is KEY, or NIL.

Signals UNKNOWN-NAMESPACE when nothing is registered under NAMESPACE, and
AMBIGUOUS-ENDPOINT when two classes both answer -- an external key must be
unique within its namespace, and returning the first would make the answer
depend on class-definition order (design §4.2).

Must NOT be called inside a read-write transaction: resolution can cross
graphs, and cross-graph reads are legal only from a read-only snapshot or
outside a transaction (design §4.1)."
  ;; *TRANSACTION* IS the read-write transaction; read-only snapshots live
  ;; in *READ-SNAPSHOTS*, keyed by graph, and never bind this.  So a bound
  ;; *TRANSACTION* is exactly the illegal case (design §4.1).
  (when graph-db:*transaction*
    (error 'resolution-in-transaction :namespace namespace :key key))
  (let ((graph (graph-db:lookup-graph namespace))
        (hits '())
        (classes '()))
    (dolist (class (namespace-sources namespace))
      (let* ((facets (source-contract class))
             (slot (getf (source-facets-identity facets) :key-slot))
             (found (graph-db:index-lookup graph class (list slot) key)))
        (when found
          (push class classes)
          (setf hits (append hits found)))))
    (when (cdr classes)
      (error 'ambiguous-endpoint :namespace namespace :key key
                                 :classes classes))
    (first hits)))
```

- [ ] **Step 4: Add the ASDF components and run**

`(:file "resolve")` after `(:file "source")`, and `(:file "resolve-tests")` after `(:file "source-tests")` — still before `conformance-tests`.

Expected: the four new tests pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/resolve.lisp spacetime/conditions.lisp \
        tests/spacetime/resolve-tests.lisp graph-db.asd
git commit -m "feat(spacetime): resolve-endpoint, the forward direction (#132)"
```

---

### Task 4: Ambiguity, and the fail-closed sensitivity predicate

**Files:**
- Modify: `spacetime/resolve.lisp`, `tests/spacetime/resolve-tests.lisp`, `tests/spacetime/source-tests.lisp`

**Interfaces:**
- Produces: `+disclosure-classes+`; `disclosable-p (class clearance)` → boolean.

- [ ] **Step 1: Write the failing tests**

Append a second class sharing a namespace, to `tests/spacetime/source-tests.lisp`:

```lisp
;; Deliberately shares :ST-REPORTS with ST-REPORT, to exercise §4.2.
(def-source st-summary :graph-db-source-test
    ((topic :initarg :topic :accessor st-topic)
     (summary-id :initarg :summary-id :accessor st-summary-id))
  :identity     (:namespace :st-reports :key-slot summary-id)
  :space        :none
  :time         :none
  :attribution  :none
  :sensitivity  :none
  :registration :none
  :indexed-text :none)
```

and append to `tests/spacetime/resolve-tests.lisp`:

```lisp
(test two-classes-answering-one-key-signals-and-names-both
  "Design §4.2: DEF-UNIQUE cannot catch this -- the classes have different
owners and the constraint registry keys on owner."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "shared")
      (make-st-summary :topic "s" :summary-id "shared"))
    (handler-case (progn (resolve-endpoint :st-reports "shared")
                         (is-true nil "expected AMBIGUOUS-ENDPOINT"))
      (ambiguous-endpoint (c)
        (is (= 2 (length (ambiguous-endpoint-classes c))))))))

(test distinct-keys-in-a-shared-namespace-still-resolve
  "Sharing a namespace is legal; only a shared KEY is a violation."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "k-1")
      (make-st-summary :topic "s" :summary-id "k-2"))
    (is (string= "r" (st-headline (resolve-endpoint :st-reports "k-1"))))
    (is (string= "s" (st-topic (resolve-endpoint :st-reports "k-2"))))))

(test disclosable-p-is-fail-closed
  "Design §3.2.  An unrecognised class, and :NONE, are treated as MORE
restricted than every known one -- never less.  If this test is ever
inverted the facet becomes worse than nothing, because a caller trusts it."
  (is-true (disclosable-p :public :public))
  (is-true (disclosable-p :public :restricted))
  (is-false (disclosable-p :restricted :public))
  (is-true (disclosable-p :restricted :restricted))
  (is-false (disclosable-p :no-such-class :restricted))
  (is-false (disclosable-p :none :restricted))
  (is-false (disclosable-p :public :no-such-clearance)))
```

- [ ] **Step 2: Run the tests to verify they fail**

Expected: `disclosable-p` undefined; the ambiguity test fails because `resolve-endpoint` currently returns the first hit.

- [ ] **Step 3: Implement `disclosable-p`**

Append to `spacetime/resolve.lisp`:

```lisp
(defparameter +disclosure-classes+
  '(:public :internal :restricted :secret)
  "Least to most restricted.  A class outside this list -- including :NONE --
is treated as more restricted than every member (design §3.2).")

(defun %disclosure-rank (class)
  "CLASS's position, or NIL when unrecognised."
  (position class +disclosure-classes+))

(defun disclosable-p (class clearance)
  "True when CLASS may be disclosed at CLEARANCE.  FAIL-CLOSED: an
unrecognised CLASS or CLEARANCE yields NIL, so the unknown case withholds
rather than releases.  The substrate never calls this itself -- enforcement
belongs to whoever reads or exports (design §3.2)."
  (let ((c (%disclosure-rank class))
        (k (%disclosure-rank clearance)))
    (and c k (<= c k))))
```

The ambiguity check is already in `resolve-endpoint` from Task 3; Step 2's
failure is expected to come only from `disclosable-p`. If the ambiguity test
also fails, the Task 3 implementation is wrong — report it rather than
patching the test.

- [ ] **Step 4: Run the tests to verify they pass**

Expected: all new tests pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/resolve.lisp tests/spacetime/resolve-tests.lisp \
        tests/spacetime/source-tests.lisp
git commit -m "feat(spacetime): fail-closed disclosure predicate; pin endpoint ambiguity (#132)"
```

---

### Task 5: Conformance, documentation, and the regression gate

**Files:**
- Modify: `tests/spacetime/conformance-tests.lisp`, `docs/vivace-graph-v3-doc.org`

- [ ] **Step 1: Write the conformance tests**

Append to `tests/spacetime/conformance-tests.lisp`:

```lisp
(test a-declared-none-facet-is-distinguishable-from-a-value
  "The absence-vs-value category, third unit running.  :NONE is a
declaration; it must never read as a configured value."
  (let ((c (source-contract 'st-report)))
    (is (eq :none (source-facets-space c)))
    (is-false (getf (source-facets-space c) :geometry-slot)
              ":NONE must not answer a facet's sub-keys")
    (is (listp (source-facets-attribution c))
        "a declared facet is a plist, not :NONE")))

(test an-undeclared-facet-cannot-exist-to-be-confused-with-none
  "Enforcement is structural, so there is no third state to test for at
runtime -- the class does not compile.  This records that reasoning."
  (signals missing-source-facet
    (macroexpand-1 '(def-source st-nope :graph-db-source-test ((a :initarg :a))
                     :identity :none))))
```

- [ ] **Step 2: Run the spacetime suite**

Expected: green.

- [ ] **Step 3: Document the contract in the org manual**

Extend the `graph-db/spacetime` chapter with a source-contract section covering: the seven facets and that every one accepts `:none`; that `def-source` defines the class and will not expand without all seven, so a non-conforming source cannot exist; that identity is the **only facet with behaviour** here, emitting the index and the namespace registration, while the other six are declared and consulted by nothing inside `graph-db`; that sensitivity is fail-closed and **the substrate never enforces it — a tenant that never calls `disclosable-p` gets no protection**; `resolve-endpoint` with its two distinct failure modes and its transaction restriction; and that registration is stored uninterpreted until #138.

Check every code sample against the shipped API rather than writing from memory.

- [ ] **Step 4: Run the full graph-db suite**

```bash
sbcl --dynamic-space-size 12288 --non-interactive \
  --eval '(progn (require :asdf) (load "~/quicklisp/setup.lisp"))' \
  --eval '(ql:quickload :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))' \
  > /tmp/full-132.log 2>&1
```

Expected: **3577 checks, 3567 pass, 10 skip, 0 fail** — the post-#135 baseline. Do **not** compare against the older 3526 figure, which predates the slot-mutation fix. A different number means this branch changed core behaviour; report it rather than explaining it away. Do not background this run.

- [ ] **Step 5: Commit**

```bash
git add tests/spacetime/conformance-tests.lisp docs/vivace-graph-v3-doc.org
git commit -m "test(spacetime): source-contract conformance, and document the contract (#132)"
```

---

## Self-Review

**Spec coverage.** §2 structural enforcement → Task 1's `omitting-any-facet-fails-to-expand`. §3.1 identity → Task 2. §3.2 sensitivity fail-closed → Task 4. §3.3 registration uninterpreted → Task 1's `%check-facet` passes it through untouched. §3.4 remaining four → Task 1 shape validation. §4 resolution → Task 3. §4.1 transaction guard → Task 3. §4.2 ambiguity → Task 4. §5 `source-contract` and declaration-only → Task 1 and Task 5's documentation step. §6 testing → Tasks 1-5. §7 acceptance → out of scope by definition. §8 version floor → Task 5 Step 4.

**Type consistency.** `source-facets` readers are used identically in Tasks 1-4. `namespace-sources` returns a list of class names in Tasks 2 and 3. `resolve-endpoint` takes `(namespace key)` throughout. `%check-facet` is called only from the macro, at expansion time.

**Three engine facts checked before this plan shipped, rather than guessed.**

*The read-write test is just `*transaction*`.* An earlier draft used a `read-only-p` predicate, which does not exist. The engine's model is simpler: `*transaction*` **is** the read-write transaction, while read-only snapshots live in `*read-snapshots*` keyed by graph and never bind it. So a bound `*transaction*` is exactly the illegal case. `*transaction*` is exported.

*`cross-graph-transaction-error` is the wrong condition to reuse.* It carries `:node`, `:transaction-graph` and `:node-graph`, and its report calls `string-id` on the node — reusing it for "you called this in a transaction" would print nonsense. Task 3 defines `resolution-in-transaction` instead.

*Index existence is proved by use, not inspection.* `%registered-index-specs` takes a real graph object, so a fabricated one would not work. `index-lookup` signals when no index covers the class and slot, so a successful lookup is the evidence.
