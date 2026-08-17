# Declarative Value Constraints Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `def-value-constraint`, a declarative per-`(class, slot)` constraint that a slot's value is drawn from a closed enumeration and/or is present, enforced at commit — and use it to close a live hole where an invalid `standing` can be written to disk.

**Architecture:** A third schema registry beside `*schema-index-metadata*` and `*schema-unique-metadata*`, reusing the shared `%spec-identity` machinery from GH #139/#140. One evaluator over a node produces violation records; the write path signals on the first, the audit path collects them all. Enforcement is one call in `%COMMIT`'s manager-locked region, beside `validate-unique-constraints`. Unlike `def-unique` there is **no index**: no cross-node lookup, no rebuild-on-open, no sidecar, no reconciliation.

**Tech Stack:** Common Lisp (SBCL), ASDF, FiveAM, closer-mop (via graph-db's `node-class` helpers).

**Spec:** `docs/superpowers/specs/2026-08-17-value-constraints-design.md`

## Global Constraints

- **Branch:** `experiment`. **Do not release, do not bump the version floor, do not merge to master.** Version stays `3.0.0` in `graph-db.asd` (three systems declare it; leave all three).
- **Lisp style: spaces only, never tabs. Hard limit of 80 columns** — code, comments, docstrings and strings alike. A 96-column line is a defect.
- **Comments are terse and point elsewhere.** State the non-obvious fact in a line or two and reference `docs/` or the GH issue. Do not narrate reasoning in source.
- **Docs travel with the code.** A `PreToolUse` hook blocks any push whose commit changes source without changing documentation. Task 6 carries the reader-facing doc; earlier commits are not pushed individually.
- **TDD, and RED must be OBSERVED.** Run the test and see it fail, for the expected reason, before writing implementation. GH #102 in this repo was implemented test-and-code-together and had to establish non-vacuity by ablation afterwards. Do not repeat that.
- **The test package does not `:use` graph-db.** `tests/package.lisp` uses explicit `:import-from`. Any new symbol used unqualified in a test must be added there or it reads as unbound — this has already cost one misdiagnosis in this programme.
- **`eql` is not enough for membership.** Use `equal` throughout so a non-keyword enumeration works.
- Issue to reference in commits: **#149** (unit 1 of ontology epic #109).

---

## File Structure

| File | Responsibility |
|---|---|
| `value-constraint.lisp` (new) | Everything: condition, registry, spec struct, macro, evaluator, commit validator, audit pass. One file because these are ~200 lines that change together; splitting would separate the macro from the evaluator it feeds. |
| `package.lisp` | Exports. |
| `transactions.lisp` | Two edits: forward `declaim`, and one call in `%COMMIT`. |
| `graph-db.asd` | Two component entries (main + test system, one file). |
| `spacetime/claim.lisp` | `def-claim-classes` emits the standing constraint. |
| `tests/value-constraint-tests.lisp` (new) | Mechanism tests (generic graph-db). |
| `tests/spacetime/claim-standing-guard-tests.lisp` (new) | The probed hole, as a regression test. |
| `tests/package.lisp` | `:import-from` for new symbols. |
| `docs/value-constraint-design.md` (new) | Reader-facing doc. |

**Why the tests are split across two systems:** the mechanism is generic graph-db and belongs in `graph-db/test`. The claim classes and `+standings+` live in `graph-db/spacetime`, whose test system is `graph-db/spacetime-test`. The regression test for the actual probed hole must live there.

---

### Task 1: Registry, macro, and retraction

Declaration only — nothing is enforced yet. Deliverable: you can declare, re-declare idempotently, and withdraw a constraint, and ask which apply to a class.

**Files:**
- Create: `value-constraint.lisp`
- Modify: `package.lisp` (export block near line 176-185)
- Modify: `graph-db.asd` (main system components, after the `index` entry at line 104)
- Modify: `graph-db.asd` (test system components, after `index-prolog-tests` at line 507)
- Modify: `tests/package.lisp` (`:import-from #:graph-db` block, near line 217-224)
- Test: `tests/value-constraint-tests.lisp`

**Interfaces:**
- Consumes: `%spec-identity (owner-name slot-names name)` and `%normalize-slots` from `index.lisp:99` / `index.lisp:91`; `class-finalized-p`, `class-slots`, `slot-definition-name` (MOP, already used by `class-unique-tuple-specs`); `graph-name` accessor on a graph.
- Produces:
  - `(defstruct value-constraint-spec owner-name slot-name graph-name one-of required name)`
  - `value-constraint-spec-identity (spec) => cons`
  - `register-value-constraint-spec (spec) => spec`
  - `unregister-value-constraint-spec (owner-name graph-name &key slot name) => boolean`
  - `%registered-value-constraint-specs (graph) => list`
  - `class-value-constraint-specs (class graph) => list of spec`
  - macro `def-value-constraint (owner-class slot graph-name &key one-of required name)`
  - macro `undef-value-constraint (owner-class graph-name &key slot name)`

- [ ] **Step 1: Write the failing tests**

Create `tests/value-constraint-tests.lisp`:

```lisp
;;;; Declarative value constraints: declaration, retraction, applicability.
;;;; Design: docs/superpowers/specs/2026-08-17-value-constraints-design.md
;;;; (GH #149).

(in-package #:graph-db/test)

(in-suite graph-db-suite)

(defparameter *vc-graph-name* :graph-db-vc-test)

;; Idempotent-reload guard, as graph-tests.lisp does: re-loading this file
;; must not stack duplicate node metadata.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (gethash *vc-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex vc-doc ()
  ((status :initarg :status :accessor vc-doc-status)
   (note :initarg :note :accessor vc-doc-note))
  :graph-db-vc-test)

(def-vertex vc-report (vc-doc) () :graph-db-vc-test)

(defparameter +vc-statuses+ '(:draft :final :withdrawn))

;; A fresh on-disk graph per test, verbatim after WITH-UQ-GRAPH
;; (tests/unique-constraint-tests.lisp:54).
(defmacro with-vc-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *vc-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %vc-specs ()
  (gethash *vc-graph-name*
           graph-db::*schema-value-constraint-metadata*))

;; The registry is keyed by graph NAME, so it outlives the fixture's
;; teardown and must be cleared per test.
(defun %vc-clear ()
  (setf (gethash *vc-graph-name*
                 graph-db::*schema-value-constraint-metadata*)
        nil))

(test value-constraint-declaration-registers-one-spec
  "The declarative registry, mirroring *SCHEMA-UNIQUE-METADATA*."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is (= 1 (length (%vc-specs))))
  (is (equal +vc-statuses+
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))))

(test redeclaring-a-named-constraint-replaces-rather-than-stacks
  "GH #139: replacing in place is what stops the table growing one entry
per evaluation.  A file is loaded more than once in a session."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of '(:draft :final) :name vc-status)
  (is (= 1 (length (%vc-specs))))
  (is (equal '(:draft :final)
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))))

(test one-of-is-evaluated-not-quoted
  "⚠ Deliberately unlike :SLOTS and :NAME.  This is what lets the standing
constraint name +STANDINGS+ instead of duplicating the vocabulary, so the
constraint and STANDINGP cannot drift apart (design, \"The macro\")."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is (equal '(:draft :final :withdrawn)
             (graph-db::value-constraint-spec-one-of
              (first (%vc-specs))))
      "a quoted :ONE-OF would have stored the SYMBOL +VC-STATUSES+"))

(test undef-value-constraint-withdraws-by-name
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (is-true (undef-value-constraint vc-doc :graph-db-vc-test
                                   :name vc-status))
  (is (= 0 (length (%vc-specs)))))

(test undef-value-constraint-withdraws-by-slot
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+)
  (is-true (undef-value-constraint vc-doc :graph-db-vc-test
                                   :slot status))
  (is (= 0 (length (%vc-specs)))))

(test undef-value-constraint-is-a-no-op-when-nothing-matches
  (%vc-clear)
  (is-false (undef-value-constraint vc-doc :graph-db-vc-test
                                    :name no-such-constraint)))

(test a-constraint-on-a-parent-applies-to-its-subclass
  "⚠ Load-bearing, not incidental: STANDING lives on the PARENT claim class
and DEF-CLAIM-CLASSES generates two arities, so one declaration must cover
both (design, \"Registry\")."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (with-vc-graph (g)
    (is (= 1 (length (graph-db::class-value-constraint-specs
                      (find-class 'vc-report) g))))))

(test a-constraint-naming-an-absent-slot-does-not-apply
  (%vc-clear)
  (def-value-constraint vc-doc no-such-slot :graph-db-vc-test
    :one-of '(:a :b) :name vc-absent)
  (with-vc-graph (g)
    (is (= 0 (length (graph-db::class-value-constraint-specs
                      (find-class 'vc-doc) g))))))

(test a-constraint-that-declares-nothing-is-refused
  "⚠ :ONE-OF NIL with :REQUIRED NIL constrains nothing while reading as a
guard -- the counter-that-cannot-fail shape.  Refused at declaration."
  (%vc-clear)
  (signals error
    (eval '(graph-db:def-value-constraint
            vc-doc status :graph-db-vc-test))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run:

```bash
cd /home/raison/work/vivace-graph-v3
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/test)' 2>&1 | tail -20
```

Expected: compile FAILS — `DEF-VALUE-CONSTRAINT` is an undefined macro and `*SCHEMA-VALUE-CONSTRAINT-METADATA*` an unbound symbol. That is the correct RED for this task; the file cannot load yet.

- [ ] **Step 3: Create `value-constraint.lisp`**

```lisp
;;;; Declarative value constraints: a slot's value drawn from a closed
;;;; enumeration and/or required to be present, enforced at COMMIT.
;;;;
;;;; The third schema registry, beside *SCHEMA-INDEX-METADATA* (index.lisp)
;;;; and *SCHEMA-UNIQUE-METADATA* (unique-constraint.lisp), sharing their
;;;; identity rule (%SPEC-IDENTITY, GH #139/#140).
;;;;
;;;; Unlike DEF-UNIQUE there is NO INDEX: a value constraint is a predicate
;;;; over one node's own slot.  No cross-node lookup, no rebuild-on-open, no
;;;; sidecar.  Design:
;;;; docs/superpowers/specs/2026-08-17-value-constraints-design.md (GH #149).

(in-package :graph-db)

(defvar *schema-value-constraint-metadata* (make-hash-table)
  "graph-name (symbol) -> list of VALUE-CONSTRAINT-SPECs (newest first).")

(defstruct (value-constraint-spec
            (:constructor make-value-constraint-spec))
  owner-name slot-name graph-name one-of required name)

(defun value-constraint-spec-identity (spec)
  "See %SPEC-IDENTITY (index.lisp).  All three registries share ONE identity
rule on purpose (GH #140)."
  (%spec-identity (value-constraint-spec-owner-name spec)
                  (list (value-constraint-spec-slot-name spec))
                  (value-constraint-spec-name spec)))

(defun register-value-constraint-spec (spec)
  "Record SPEC, REPLACING any spec of the same identity in place.  Replacing
rather than pushing is what stops the table growing one entry per evaluation
(GH #139).  Signals when SPEC constrains nothing."
  (let ((one-of (value-constraint-spec-one-of spec)))
    (when (and (null one-of) (not (value-constraint-spec-required spec)))
      (error "Value constraint on ~S.~S declares neither :ONE-OF nor ~
              :REQUIRED, so it constrains nothing."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec)))
    (when (and one-of (not (and (listp one-of) (null (cdr (last one-of))))))
      (error "Value constraint on ~S.~S has a :ONE-OF that is not a proper ~
              list: ~S."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec) one-of)))
  (let* ((g (value-constraint-spec-graph-name spec))
         (id (value-constraint-spec-identity spec))
         (existing (gethash g *schema-value-constraint-metadata*))
         (hit (find id existing :key #'value-constraint-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-value-constraint-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-value-constraint-spec (owner-name graph-name
                                         &key slot name)
  "Withdraw the declaration identified by (OWNER . NAME) or (OWNER . (SLOT)).
Returns T if one was withdrawn; a no-op otherwise.  Nothing is rebuilt or
released -- there is no index behind a value constraint."
  (let* ((id (%spec-identity owner-name (when slot (list slot)) name))
         (existing (gethash graph-name *schema-value-constraint-metadata*))
         (hit (find id existing :key #'value-constraint-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-value-constraint-metadata*)
            (remove hit existing))
      t)))

(defun %registered-value-constraint-specs (graph)
  (gethash (graph-name graph) *schema-value-constraint-metadata*))

(defun class-value-constraint-specs (class graph)
  "Specs from the registry applying to CLASS: owner is CLASS or an ancestor
(subtype IS-A) and the named slot exists on CLASS.  Mirrors
CLASS-UNIQUE-TUPLE-SPECS (unique-constraint.lisp)."
  (when (class-finalized-p class)
    (loop for spec in (%registered-value-constraint-specs graph)
          for owner = (value-constraint-spec-owner-name spec)
          for slot = (value-constraint-spec-slot-name spec)
          when (and (subtypep (class-name class) owner)
                    (find slot (class-slots class)
                          :key #'slot-definition-name))
          collect spec)))

(defmacro def-value-constraint (owner-class slot graph-name
                                &key one-of required name)
  "Declare that OWNER-CLASS's SLOT in GRAPH-NAME draws its value from the
closed enumeration :ONE-OF, and with :REQUIRED is never NIL.  Enforced at
commit (VALIDATE-VALUE-CONSTRAINTS), never merely indexed.

Without :REQUIRED, NIL is EXEMPT -- \"if present, it must be one of these\" --
matching DEF-UNIQUE's null rule.  Diverging from that would be the trap
GH #107 named: two neighbouring macros disagreeing about nulls.

⚠ :ONE-OF is EVALUATED, unlike SLOT and :NAME.  That is what lets a caller
name an existing vocabulary constant rather than duplicate it.  The value is
captured at registration, so editing the constant does not retroactively
change a registered constraint -- re-evaluate this form (idempotent, GH #139)."
  `(register-value-constraint-spec
    (make-value-constraint-spec
     :owner-name ',owner-class
     :slot-name ',slot
     :graph-name ',graph-name
     :one-of ,one-of
     :required ,required
     :name ',name)))

(defmacro undef-value-constraint (owner-class graph-name &key slot name)
  "Withdraw a DEF-VALUE-CONSTRAINT declaration, by :NAME or by :SLOT.
Keyword rather than positional for the same reason as UNDEF-INDEX: a graph
name is itself a keyword."
  `(unregister-value-constraint-spec ',owner-class ',graph-name
                                     :slot ',slot :name ',name))
```

- [ ] **Step 4: Wire it into `graph-db.asd` and `package.lisp`**

In `graph-db.asd`, after the `index` component (line 104), add:

```lisp
               ;; Declarative value constraints (GH #149).  After INDEX for
               ;; %SPEC-IDENTITY; no index of its own to build.
               (:file "value-constraint" :depends-on ("index"))
```

In the `graph-db/test` system components, **append** after `(:file "index-prolog-tests")` — the "append, don't insert" discipline the `segment-integration-tests` comment at line 455 exists to protect:

```lisp
               (:file "value-constraint-tests")     ; GH #149
```

In `package.lisp`, beside the existing exports at lines 176-185:

```lisp
           #:def-value-constraint #:undef-value-constraint
```

In `tests/package.lisp`, in the `:import-from #:graph-db` block near line 224:

```lisp
                #:def-value-constraint
                #:undef-value-constraint
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))' 2>&1 | tail -30
```

Expected: all pass; **no new failures elsewhere**. Record the total check count — it must be higher than before this task, and you will compare against it in every later task.

- [ ] **Step 6: Commit**

```bash
git add value-constraint.lisp package.lisp graph-db.asd \
        tests/package.lisp tests/value-constraint-tests.lisp
git commit -m "feat(schema): the value-constraint registry and its macros (#149)

Declaration and retraction only; nothing is enforced yet.  Third registry
beside the index and unique ones, sharing %SPEC-IDENTITY (#139/#140).

:ONE-OF is evaluated where :SLOT and :NAME are quoted, so a caller names an
existing vocabulary constant instead of duplicating it.

[skip-docs] -- reader-facing doc lands with enforcement in a later commit."
```

---

### Task 2: The evaluator and its condition

Deliverable: a pure function from a node to violation records, and the condition that reports one. Still not wired to commit.

**Files:**
- Modify: `value-constraint.lisp`
- Test: `tests/value-constraint-tests.lisp`

**Interfaces:**
- Consumes: `class-value-constraint-specs` (Task 1); `id` and `string-id` on a node.
- Produces:
  - `(defstruct vc-violation spec node-id class-name slot actual expected reason)` — `reason` is `:not-in-vocabulary` or `:missing`
  - `%value-constraint-violations (node graph) => list of vc-violation`
  - condition `value-constraint-violation` with readers `vcv-class-name`, `vcv-slot-name`, `vcv-value`, `vcv-expected`, `vcv-reason`, `vcv-node-id`

- [ ] **Step 1: Write the failing tests**

Append to `tests/value-constraint-tests.lisp`:

```lisp
;;; --- the evaluator -------------------------------------------------------

;; ⚠ ORDERING, in every test below: CREATE THE NODE FIRST, DECLARE THE
;; CONSTRAINT SECOND.  Task 3 makes a violating commit signal, so a node
;; created under a live constraint could not be committed at all -- and
;; creating it first is also the only way to obtain the pre-constraint damage
;; the audit pass (Task 4) exists to find.
(defun %vc-make (g &rest initargs)
  "Commit a VC-DOC and return it, read back from G."
  (let ((id (with-transaction () (id (apply #'make-vc-doc initargs)))))
    (lookup-vertex id)))

(defun %vc-violations-for (node g)
  (graph-db::%value-constraint-violations node g))

(test a-value-outside-the-enumeration-is-a-violation
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status :nonsense)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (let ((vs (%vc-violations-for v g)))
        (is (= 1 (length vs)))
        (is (eq :not-in-vocabulary
                (graph-db::vc-violation-reason (first vs))))
        (is (eq :nonsense (graph-db::vc-violation-actual (first vs))))
        (is (equal +vc-statuses+
                   (graph-db::vc-violation-expected (first vs))))))))

(test every-member-of-the-enumeration-is-accepted
  "⚠ A guard bought by refusing everything is not a guard."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((nodes (mapcar (lambda (s) (%vc-make g :status s))
                         +vc-statuses+)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (dolist (v nodes)
        (is (null (%vc-violations-for v g))
            "~S is in the vocabulary and must not be a violation"
            (vc-doc-status v))))))

(test nil-is-exempt-without-required
  "Matches DEF-UNIQUE's null rule: \"if present, it must be one of these\".
Diverging would be the trap GH #107 named."
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (is (null (%vc-violations-for v g))))))

(test nil-is-a-violation-under-required
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :required t :name vc-status)
      (let ((vs (%vc-violations-for v g)))
        (is (= 1 (length vs)))
        (is (eq :missing (graph-db::vc-violation-reason (first vs))))))))

(test required-alone-checks-presence-only
  (%vc-clear)
  (with-vc-graph (g)
    (let ((present (%vc-make g :status :anything))
          (absent (%vc-make g :status nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :required t :name vc-status)
      (is (null (%vc-violations-for present g)))
      (is (= 1 (length (%vc-violations-for absent g)))))))

(test two-constraints-on-one-node-both-report
  (%vc-clear)
  (with-vc-graph (g)
    (let ((v (%vc-make g :status :nonsense :note nil)))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of +vc-statuses+ :name vc-status)
      (def-value-constraint vc-doc note :graph-db-vc-test
        :required t :name vc-note)
      (is (= 2 (length (%vc-violations-for v g)))))))

(test the-report-names-the-vocabulary-it-expected
  "⚠ This is why :ONE-OF is an enumeration rather than :SATISFIES a
predicate -- a predicate could only say that it returned NIL."
  (%vc-clear)
  (def-value-constraint vc-doc status :graph-db-vc-test
    :one-of +vc-statuses+ :name vc-status)
  (let ((text (princ-to-string
               (make-condition 'value-constraint-violation
                               :class-name 'vc-doc :slot-name 'status
                               :value :nonsense :expected +vc-statuses+
                               :reason :not-in-vocabulary
                               :node-id (graph-db::gen-vertex-id)))))
    (is (search "NONSENSE" text))
    (is (search "WITHDRAWN" text)
        "the report must name the vocabulary, not merely the bad value")))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))' 2>&1 | tail -30
```

Expected: FAIL — `%VALUE-CONSTRAINT-VIOLATIONS` and `VALUE-CONSTRAINT-VIOLATION` are undefined. Confirm the failure names those symbols; a failure for any other reason means the test is wrong.

If `gen-vertex-id` is not the right constructor for a node id in this codebase, find the one `unique-constraint-tests.lisp` uses for the same purpose and use that instead.

- [ ] **Step 3: Add the condition and evaluator to `value-constraint.lisp`**

Insert the condition immediately after the `(in-package :graph-db)` form, and the evaluator after `class-value-constraint-specs`:

```lisp
(define-condition value-constraint-violation (error)
  ((class-name :initarg :class-name :reader vcv-class-name)
   (slot-name  :initarg :slot-name  :reader vcv-slot-name)
   (value      :initarg :value      :reader vcv-value)
   (expected   :initarg :expected   :reader vcv-expected)
   (reason     :initarg :reason     :reader vcv-reason)
   (node-id    :initarg :node-id    :reader vcv-node-id))
  (:report
   (lambda (c s)
     (if (eq (vcv-reason c) :missing)
         (format s "Value constraint on ~S.~S violated by node ~A: the ~
                    slot is required but holds NIL."
                 (vcv-class-name c) (vcv-slot-name c)
                 (string-id (vcv-node-id c)))
         (format s "Value constraint on ~S.~S violated by node ~A: ~
                    expected one of~{ ~S~}; got ~S."
                 (vcv-class-name c) (vcv-slot-name c)
                 (string-id (vcv-node-id c))
                 (vcv-expected c) (vcv-value c))))))
```

```lisp
(defstruct (vc-violation (:constructor %make-vc-violation))
  spec node-id class-name slot actual expected reason)

(defun %value-constraint-violations (node graph)
  "Every value constraint NODE violates, as VC-VIOLATION records.  The one
evaluator behind both consumers: the write path signals on the first, the
audit pass collects them all (design, \"Violation shape\").

EQUAL, not EQL, so a non-keyword enumeration works."
  (let ((class (class-of node)))
    (loop for spec in (class-value-constraint-specs class graph)
          for slot = (value-constraint-spec-slot-name spec)
          for one-of = (value-constraint-spec-one-of spec)
          for val = (slot-value node slot)
          append
          (cond
            ((null val)
             (when (value-constraint-spec-required spec)
               (list (%make-vc-violation
                      :spec spec :node-id (id node)
                      :class-name (class-name class) :slot slot
                      :actual nil :expected one-of :reason :missing))))
            ((and one-of (not (member val one-of :test #'equal)))
             (list (%make-vc-violation
                    :spec spec :node-id (id node)
                    :class-name (class-name class) :slot slot
                    :actual val :expected one-of
                    :reason :not-in-vocabulary)))
            (t nil)))))
```

Add to `package.lisp` exports:

```lisp
           #:value-constraint-violation
           #:vcv-class-name #:vcv-slot-name #:vcv-value
           #:vcv-expected #:vcv-reason #:vcv-node-id
```

Add to `tests/package.lisp` `:import-from`:

```lisp
                #:value-constraint-violation
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command as Step 2. Expected: all pass, check count up, no new failures.

- [ ] **Step 5: Commit**

```bash
git add value-constraint.lisp package.lisp tests/package.lisp \
        tests/value-constraint-tests.lisp
git commit -m "feat(schema): the value-constraint evaluator and its report (#149)

One evaluator over a node, feeding both consumers.  The report names the
vocabulary it expected, which is the half of the acceptance bar a
:SATISFIES predicate could not pay -- it could only say the predicate
returned NIL.

[skip-docs] -- reader-facing doc lands with enforcement."
```

---

### Task 3: Commit-time enforcement

Deliverable: an invalid write is refused before anything is journaled. This is the task that closes the hole.

**Files:**
- Modify: `value-constraint.lisp`
- Modify: `transactions.lisp` (declaim at line 5-7; call site at line 3053)
- Test: `tests/value-constraint-tests.lisp`

**Interfaces:**
- Consumes: `%value-constraint-violations` (Task 2); `writes`, `node`, `deleted-p` (as used by `validate-unique-constraints`, `unique-constraint.lisp:680`).
- Produces: `validate-value-constraints (tx graph) => nil, or signals value-constraint-violation`

- [ ] **Step 1: Write the failing tests**

Append to `tests/value-constraint-tests.lisp`:

```lisp
;;; --- commit-time enforcement --------------------------------------------

(test an-invalid-value-is-refused-at-commit
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (signals value-constraint-violation
      (with-transaction ()
        (make-vc-doc :status :nonsense)))))

(test a-valid-value-commits
  "The guard must not have been bought by refusing everything."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (finishes
      (with-transaction ()
        (make-vc-doc :status :final)))))

(test an-invalid-value-is-refused-on-the-UPDATE-path
  "⚠ THE REASON THIS UNIT EXISTS.  A construction-time check cannot see
this: COPY + SETF + SAVE never goes through the constructor (#149)."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :final)))))
      (signals value-constraint-violation
        (with-transaction ()
          (let ((v (copy (lookup-vertex id))))
            (setf (vc-doc-status v) :nonsense)
            (save v)))))))

(test a-withdrawn-constraint-stops-being-enforced
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (undef-value-constraint vc-doc :graph-db-vc-test :name vc-status)
    (finishes
      (with-transaction ()
        (make-vc-doc :status :nonsense)))))

(test deleting-a-node-is-not-blocked-by-its-own-violation
  "A delete claims nothing, exactly as in VALIDATE-UNIQUE-CONSTRAINTS --
otherwise a store holding pre-constraint damage could not be repaired."
  (%vc-clear)
  (with-vc-graph (g)
    (declare (ignorable g))
    (let ((id (with-transaction ()
                (id (make-vc-doc :status :final)))))
      (def-value-constraint vc-doc status :graph-db-vc-test
        :one-of '(:nothing-matches) :name vc-status)
      (finishes
        (with-transaction ()
          (mark-deleted (lookup-vertex id)))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Same command as Task 2 Step 2.

Expected: `an-invalid-value-is-refused-at-commit`, `...-on-the-UPDATE-path` FAIL because nothing signals. `a-valid-value-commits` and `a-withdrawn-constraint...` will pass already — that is fine and expected; they are the non-vacuity half and prove the guard is not bought by refusing everything.

**Confirm the UPDATE-path test fails.** If it does not, the whole premise of this unit is wrong and you must stop and report that rather than continue.

- [ ] **Step 3: Add the validator**

Append to `value-constraint.lisp`:

```lisp
(defun validate-value-constraints (tx graph)
  "Signal VALUE-CONSTRAINT-VIOLATION if any write in TX violates a declared
value constraint.  Called in %COMMIT's manager-locked region, after VALIDATE
and before durability, so a violation aborts before anything is journaled --
the same placement, and the same reason, as VALIDATE-UNIQUE-CONSTRAINTS.

That placement is the point: it sees every write whatever accessor produced
it, which a construction-time check cannot (GH #149)."
  (dolist (write (writes tx))
    (let ((node (node write)))
      (unless (deleted-p node)      ; a delete claims nothing
        (let ((v (first (%value-constraint-violations node graph))))
          (when v
            (error 'value-constraint-violation
                   :class-name (vc-violation-class-name v)
                   :slot-name (vc-violation-slot v)
                   :value (vc-violation-actual v)
                   :expected (vc-violation-expected v)
                   :reason (vc-violation-reason v)
                   :node-id (vc-violation-node-id v))))))))
```

In `transactions.lisp`, extend the forward declaim at lines 5-7 — it already carries the same comment explaining why:

```lisp
(declaim (ftype (function (t t) t)
                validate-unique-constraints apply-tx-writes-to-unique-indexes
                apply-tx-writes-to-secondary-indexes
                validate-value-constraints))
```

And at line 3053, immediately after the `validate-unique-constraints` call:

```lisp
               ;; Declarative value constraints (GH #149): same region, same
               ;; reason -- a violation aborts before anything is journaled.
               (validate-value-constraints tx (graph tx))
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: all pass; **no new failures anywhere in the suite.** This task touches `%COMMIT`, so a regression here would show up far from these tests — check the whole run, not just the new names.

- [ ] **Step 5: Commit**

```bash
git add value-constraint.lisp transactions.lisp \
        tests/value-constraint-tests.lisp
git commit -m "feat(schema): enforce value constraints at commit (#149)

One call in %COMMIT's manager-locked region beside the unique check, after
VALIDATE and before durability, so a violation aborts before anything is
journaled.  That placement is the point -- it sees every write whatever
accessor produced it, which a construction-time check cannot.

[skip-docs] -- reader-facing doc lands with the audit pass."
```

---

### Task 4: The audit pass

Deliverable: an existing store can be surveyed for damage written before the constraint existed.

**Files:**
- Modify: `value-constraint.lisp`
- Modify: `package.lisp`, `tests/package.lisp`
- Test: `tests/value-constraint-tests.lisp`

**Interfaces:**
- Consumes: `map-vertices (fn graph &key vertex-type ...)` (`vertex.lisp:173`).
- Produces: `check-value-constraints (graph &key vertex-type) => (values violations checked-count spec-count)`

- [ ] **Step 1: Write the failing tests**

Append to `tests/value-constraint-tests.lisp`:

```lisp
;;; --- the audit pass ------------------------------------------------------

(test the-audit-pass-finds-damage-written-before-the-constraint
  "⚠ Not speculative tooling.  The probe on #149 proves invalid values are
writable today, so an existing store may already hold them -- a guard that
only protects future writes would leave that undetectable."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (multiple-value-bind (violations checked specs)
        (check-value-constraints g :vertex-type 'vc-doc)
      (is (= 1 (length violations)))
      (is (eq :not-in-vocabulary
              (graph-db::vc-violation-reason (first violations))))
      (is (= 1 checked)
          "a violation count with no population is not a result")
      (is (= 1 specs)))))

(test the-audit-pass-reports-the-population-it-checked
  "⚠ This programme's most repeated error is a count with no population.
Zero violations over zero specs is an unchecked graph, not a clean one, and
the caller must be able to tell them apart."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (multiple-value-bind (violations checked specs)
        (check-value-constraints g :vertex-type 'vc-doc)
      (is (null violations))
      (is (= 0 specs) "no constraints are declared, so nothing was checked")
      (is (= 1 checked)
          "⚠ the graph is NOT empty -- zero violations here means unchecked,
which is exactly what SPECS lets the caller tell apart"))))

(test the-audit-pass-does-not-signal
  "It collects.  Signalling would stop at the first find, which is the
opposite of what a survey is for."
  (%vc-clear)
  (with-vc-graph (g)
    (with-transaction () (make-vc-doc :status :nonsense))
    (def-value-constraint vc-doc status :graph-db-vc-test
      :one-of +vc-statuses+ :name vc-status)
    (finishes (check-value-constraints g :vertex-type 'vc-doc))))
```

Note the first test writes `:nonsense` **before** declaring the constraint — that is deliberate and is the only way to create pre-constraint damage now that Task 3 refuses it at commit.

- [ ] **Step 2: Run the tests to verify they fail**

Same command. Expected: FAIL — `CHECK-VALUE-CONSTRAINTS` is undefined.

- [ ] **Step 3: Add the audit pass**

Append to `value-constraint.lisp`:

```lisp
(defun check-value-constraints (graph &key vertex-type)
  "Survey live vertices of GRAPH and COLLECT violations without signalling.
Returns (values VIOLATIONS CHECKED-COUNT SPEC-COUNT).

⚠ The two counts are part of the answer, not diagnostics.  Zero violations
over zero specs is an UNCHECKED graph, not a clean one; a caller that prints
\"OK\" without reading them is reporting a count with no population.

:VERTEX-TYPE narrows the scan and keeps it snapshot-consistent; the untyped
scan reads live node versions and bypasses MVCC (see MAP-VERTICES), so it is
for admin passes over a quiescent graph."
  (let ((violations '())
        (checked 0))
    (map-vertices (lambda (v)
                    (incf checked)
                    (let ((vs (%value-constraint-violations v graph)))
                      (when vs
                        (setf violations (nconc violations vs)))))
                  graph :vertex-type vertex-type)
    (values violations checked
            (length (%registered-value-constraint-specs graph)))))
```

Add to `package.lisp` exports and `tests/package.lisp` `:import-from`:

```lisp
           #:check-value-constraints
```

- [ ] **Step 4: Run the tests to verify they pass**

Same command. Expected: all pass, no new failures.

- [ ] **Step 5: Commit**

```bash
git add value-constraint.lisp package.lisp tests/package.lisp \
        tests/value-constraint-tests.lisp
git commit -m "feat(schema): survey a store for value-constraint damage (#149)

Collects rather than signals, and returns the population it checked
alongside the violations -- zero violations over zero specs is an unchecked
graph, not a clean one.

[skip-docs] -- reader-facing doc lands next."
```

---

### Task 5: Close the standing hole

Deliverable: the probe from #149 no longer reproduces. This is the task the unit was justified by.

**Files:**
- Modify: `spacetime/claim.lisp` (`def-claim-classes`, line 109)
- Modify: `graph-db.asd` (spacetime-test components, after line 423)
- Create: `tests/spacetime/claim-standing-guard-tests.lisp`
- Modify: `tests/spacetime/package.lisp` if the new symbols are used unqualified

**Interfaces:**
- Consumes: `def-value-constraint` (Task 1), enforcement (Task 3); `+standings+` (`spacetime/standing.lisp:6`).
- Produces: every family defined by `def-claim-classes` carries a `standing-vocabulary` constraint on its parent class.

**Symbol access in `graph-db/spacetime-test`** — unlike `graph-db/test`, this package **does** `:use #:graph-db.spacetime`, so `standingp`, `+standings+`, `invalid-standing`, `claim-standing` and `claims-touching` are all available unqualified. But `graph-db` itself is *not* used: `make-graph`, `close-graph`, `open-graph`, `with-transaction`, `id` and `lookup-vertex` are explicitly imported (`tests/spacetime/package.lisp:24`), and anything else from `graph-db` must be written qualified — hence `graph-db::copy` and `graph-db::save` in the tests below. `collect-garbage` and `with-temp-directory` are **test-local helpers** defined in `tests/spacetime/suite.lisp:56` and `:49`, not graph-db exports; use them unqualified. `graph-db:value-constraint-violation` is qualified because it is new in Task 2 and this package does not import it.

- [ ] **Step 1: Write the failing tests**

Create `tests/spacetime/claim-standing-guard-tests.lisp`:

```lisp
;;;; The standing vocabulary is enforced on the UPDATE path, not only at
;;;; construction.  Probe and rationale: GH #149.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test an-invalid-standing-is-refused-on-the-update-path
  "⚠ THE REGRESSION TEST FOR #149.  CHECK-STANDING fires inside the
generated MAKE-<NAME> wrapper only (claim.lisp:177), so COPY + SETF + SAVE
committed an invalid standing and it survived a reopen."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) :nonsense)
            (graph-db::save copy)))))))

(test a-nil-standing-is-refused-on-the-update-path
  "The :REQUIRED half.  :ONE-OF alone exempts NIL, so without :REQUIRED this
write would still commit and the claim would carry no standing at all."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) nil)
            (graph-db::save copy)))))))

(test the-refused-standing-is-not-durable
  "⚠ The in-session read is not the test.  The node cache has made two
earlier tests in this programme vacuous by serving the right answer from
memory; the probe on #149 only became decisive at line E, after a reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u :subject "s1"))
               (ignore-errors
                (with-transaction ()
                  (let ((c (graph-db::copy
                            (first (claims-touching g 'ct-claim
                                                    :ns "s1")))))
                    (setf (claim-standing c) :nonsense)
                    (graph-db::save c)))))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is-true
                (standingp
                 (claim-standing
                  (first (claims-touching g2 'ct-claim :ns "s1"))))
                "an invalid standing reached disk and survived a reopen"))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

(test every-standing-in-the-vocabulary-still-commits
  "⚠ A guard bought by refusing everything is not a guard.  If this fails,
the constraint and +STANDINGS+ have drifted apart -- which is the drift
:ONE-OF being evaluated exists to prevent."
  (with-claim-graph (g)
    (declare (ignorable g))
    (dolist (s +standings+)
      (finishes
        (with-transaction ()
          (make-ct-claim-unary :subject-namespace :ns
                               :subject-key (string s)
                               :relation :r :producer :p
                               :standing s))))))

(test construction-still-refuses-an-invalid-standing
  "CHECK-STANDING stays as a fast-fail with a better error site; it just
stops being the only thing there."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals invalid-standing
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s9"
                             :relation :r :producer :p9
                             :standing :nonsense)))))
```

- [ ] **Step 2: Run the spacetime suite to verify they fail**

```bash
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(fiveam:run! (quote graph-db/spacetime-test::spacetime-suite))' \
  2>&1 | tail -30
```

Expected: the first three FAIL — no constraint is declared on `ct-claim` yet, so nothing signals and the invalid standing reaches disk. The last two PASS already (construction is guarded, and valid standings commit).

**`the-refused-standing-is-not-durable` failing is the reproduction of the original probe.** Confirm you see it fail before continuing.

- [ ] **Step 3: Emit the constraint from `def-claim-classes`**

In `spacetime/claim.lisp`, inside the `` `(progn `` of `def-claim-classes`, after the three `def-vertex` forms and before the family registration, add:

```lisp
       ;; The closed vocabulary, enforced on every write path -- not only at
       ;; construction, where CHECK-STANDING alone left it (GH #149).
       ;; :ONE-OF is evaluated, so this names +STANDINGS+ rather than
       ;; duplicating it and cannot drift from STANDINGP.
       (graph-db:def-value-constraint ,parent standing ,graph-name
         :one-of +standings+
         :required t
         :name standing-vocabulary)
```

Extend the macro's docstring with one line:

```lisp
STANDING is ALSO declared as a value constraint on PARENT, so the closed
vocabulary is enforced at commit on every write path, not only through the
MAKE-<NAME> wrapper (GH #149).
```

`:name standing-vocabulary` matters and is exactly the case `%spec-identity`'s docstring was written for: a macro emitting specs on a caller's behalf cannot name what a previous version of itself emitted, so a stable name is what makes re-declaration replace rather than stack.

In `graph-db.asd`, **append** to the `graph-db/spacetime-test` components after the last entry:

```lisp
               (:file "claim-standing-guard-tests")   ; GH #149
```

- [ ] **Step 4: Run both suites to verify they pass**

```bash
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/spacetime-test)' \
  --eval '(fiveam:run! (quote graph-db/spacetime-test::spacetime-suite))' \
  2>&1 | tail -30
```

Then the main suite as in Task 3. Expected: both green, no new failures.

- [ ] **Step 5: Re-run the original probe**

The probe that justified the unit must no longer reproduce. Write it to the scratchpad and run it:

```bash
cat > /tmp/probe-standing-after.lisp <<'LISP'
(require :asdf)
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(ql:register-local-projects)
(ql:quickload :graph-db/spacetime-test :silent t)
(in-package :graph-db/spacetime-test)
(format t "~&=== RESULT ===~%")
(with-claim-graph (g)
  (declare (ignorable g))
  (with-transaction () (make-u :subject "s1"))
  (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
    (format t "C. copy+setf+save :nonsense -> ~A~%"
            (handler-case
                (progn (with-transaction ()
                         (let ((copy (graph-db::copy c)))
                           (setf (claim-standing copy) :nonsense)
                           (graph-db::save copy)))
                       "ACCEPTED")
              (error (e) (format nil "REJECTED (~A)" (type-of e)))))))
LISP
sbcl --dynamic-space-size 16384 --non-interactive \
  --load /tmp/probe-standing-after.lisp 2>&1 | grep -aE '^C\. '
```

Expected: `C. copy+setf+save :nonsense -> REJECTED (VALUE-CONSTRAINT-VIOLATION)`

(Before this unit it read `ACCEPTED`, and the value survived a reopen.)

- [ ] **Step 6: Commit**

```bash
git add spacetime/claim.lisp graph-db.asd \
        tests/spacetime/claim-standing-guard-tests.lisp
git commit -m "fix(spacetime): enforce the standing vocabulary on every write path (#149)

DEF-CLAIM-CLASSES now emits a value constraint on the parent's STANDING, so
the closed vocabulary is checked at commit rather than only inside the
generated MAKE-<NAME> wrapper.

Before this, COPY + SETF + SAVE committed an invalid standing and it
survived a close and reopen -- the substrate's central invariant was
enforced at construction and nowhere else.  The probe on #149 is now a
regression test, including its reopen step: the in-session read was never
the test, since the node cache has made two earlier tests here vacuous.

:ONE-OF names +STANDINGS+ rather than copying it, so the constraint and
STANDINGP cannot drift.

[skip-docs] -- reader-facing doc lands next."
```

---

### Task 6: Documentation and issue closure

Deliverable: a reader-facing doc, and the issues left true. **This is the commit that unblocks pushing** — the docs hook rejects a push whose source changed without docs.

**Files:**
- Create: `docs/value-constraint-design.md`
- Modify: `docs/spatiotemporal-substrate-programme.md` (note unit 1 landed)

- [ ] **Step 1: Write `docs/value-constraint-design.md`**

Model it on `docs/unique-constraint-design.md` — read that file first and match its structure and register. It must cover, with no placeholders:

- What `def-value-constraint` declares, with a worked example using `+standings+`.
- The null rule, and that it matches `def-unique`'s on purpose (GH #107).
- That `:one-of` is evaluated while the slot and `:name` are quoted, why, and the staleness consequence with its fix (re-evaluate; idempotent).
- Where enforcement happens (`%COMMIT`, manager-locked, pre-durability) and why that placement is what makes it stronger than a construction-time check.
- Inheritance: declaring on a parent covers subclasses.
- `check-value-constraints`, its three return values, and the warning that zero violations over zero specs is an unchecked graph.
- What this does **not** do — the list from the spec's "What unit 1 does NOT do", especially that `claim-extent` and the other claim accessors have the same unguarded update path and are a later unit's problem.

- [ ] **Step 2: Run the full suite one last time**

Both suites, as in Task 5 Step 4, plus confirm the total check count is higher than the baseline you recorded in Task 1.

**If the count did not move, a file's tests are not in a suite.** A missing `(in-suite ...)` made `schema-retraction-tests.lisp` pass by name while never running, and it was caught only because the count stayed at 3577. Check both new test files carry `(in-suite ...)`.

- [ ] **Step 3: Commit**

```bash
git add docs/value-constraint-design.md \
        docs/spatiotemporal-substrate-programme.md
git commit -m "docs: value constraints, and what unit 1 deliberately leaves (#149)"
```

- [ ] **Step 4: Ask before pushing**

**Do not push.** Pushing is outward-facing and needs Kevin's approval each time. Report the suite counts and the probe result, and ask.

- [ ] **Step 5: Update the issues**

After approval to push, close #149 with what actually shipped, and comment on #109 recording what unit 1 taught about the shape of units 2-4 — that is the stated reason they were left unfiled.

---

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| The macro, `:one-of` evaluated | 1 |
| Registry, identity, retraction | 1 |
| Inheritance via `subtypep` | 1 |
| Null semantics + `:required` (option C) | 2 |
| Violation shape, one evaluator | 2 |
| Reporting names the vocabulary | 2 |
| Enforced at `transactions.lisp:3053` | 3 |
| Audit path collects, does not signal | 4 |
| "consult-only, never conjure, and loud" | 4 (returns spec-count so a caller cannot report OK over zero specs) |
| Closing the probed standing hole | 5 |
| Test list items 1-9 | 1-5; item 2 (durability) is Task 5 Step 1 test 3; item 9 (`in-suite`) is Task 6 Step 2 |
| "What unit 1 does NOT do" | 6 |

**Note on one spec item:** the spec's "Undeclared / unbuilt" section describes the loud-not-silent rule. There is nothing to build, so this is discharged by `check-value-constraints` returning `spec-count` rather than by a `log:warn` — the caller cannot mistake an unchecked graph for a clean one. That is a deliberate reading of the spec, recorded here so a reviewer can reject it if they disagree.

**Type consistency:** `vc-violation` accessors are `vc-violation-{spec,node-id,class-name,slot,actual,expected,reason}` throughout Tasks 2-4. Condition readers are `vcv-*` throughout Tasks 2-3. `check-value-constraints` returns three values in Task 4 and is consumed as three in Task 4's tests only.

**Placeholder scan:** none — every code step carries the actual code, and Task 6's doc step enumerates required content rather than saying "write docs".
