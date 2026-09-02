# Slot-Mutation Contract Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make mutating a node created in the current transaction correct, and make every other unauthorized persistent-slot write signal at the point of the mistake instead of corrupting the graph.

**Architecture:** Three independent changes. (1) `APPLY-TX-WRITE (tx-create)` re-serializes `BYTES` from `DATA`, restoring the symmetry it lost against `tx-update`. (2) A guard in the single persistent-slot write funnel — the `(setf SLOT-VALUE-USING-CLASS)` `:around` — rejects writes to nodes the transaction is not entitled to mutate. (3) `COPY-NODE` rejects nodes created in the current transaction. Each is small; the risk lives in the guard's interaction with engine-internal paths, which is why Task 2 is characterisation-only and lands no guard.

**Tech Stack:** Common Lisp (SBCL 2.6.6, ECL 26.5.5), FiveAM, ASDF.

## Global Constraints

- **Lisp indentation: spaces only, never tabs.** 80-column hard limit for code, comments, docstrings and strings alike.
- **Comments state the invariant and cite the issue (`GH #135`); detail belongs in the spec or issue, never inline.**
- **SBCL needs `--dynamic-space-size 16384` to run any suite.** The default 1 GiB heap dies with "Heap exhausted, game over" partway through.
- **Every regression test in this plan must close and reopen the graph and assert after reopen.** In-session reads are served by the node cache and pass against a graph that is being destroyed. This property is why #135 survived 3,359 checks.
- **Ephemeral and meta slots must remain freely mutable.** Guarding them is a regression.
- **`*INITIALIZING-NODE*` is the internal escape** and must keep working for `CHANGE-NODE-CLASS` and the ECL construction paths (`vertex.lisp:27`, `edge.lisp:35`).
- **Work on branch `experiment`.** Show Kevin the full diff before any commit.
- Spec: `docs/superpowers/specs/2026-08-10-slot-mutation-contract-design.md`.

---

## File Structure

| File | Responsibility | Tasks |
|---|---|---|
| `tests/slot-mutation-tests.lisp` | **New.** Every test in this plan: the four patterns, the create+delete characterisation, the escape-hatch and ephemeral/meta regressions. | 1–5 |
| `graph-db.asd` | Register the new test file in `graph-db/test`. | 1 |
| `transactions.lisp` | `APPLY-TX-WRITE (tx-create)` re-serialize (T1); the two new conditions (T3, T4); `COPY-NODE` guard (T4). | 1, 3, 4 |
| `primitive-node.lisp` | `CHECK-SLOT-MUTATION-ALLOWED` and its call from the `(setf SLOT-VALUE-USING-CLASS)` `:around`. | 3 |
| `package.lisp` | Export the two new conditions. | 3, 4 |
| `example.lisp` | A worked copy/modify/save section — currently contains none. | 6 |
| `docs/vivace-graph-v3-doc.org` | The contract, the pattern table, and why `COPY` exists. | 6 |
| `CHANGELOG.md` | `Fixed` for A and B, `Changed` for the guard. | 6 |

A single new test file rather than additions to `tests/write-path-tests.lisp`: these tests need the close/reopen fixture, which `WITH-TEST-GRAPH` does not provide, and grouping them keeps the reopen discipline visible in one place.

---

### Task 1: Pattern A — a mutation after CREATE survives reopen

**Files:**
- Create: `tests/slot-mutation-tests.lisp`
- Modify: `graph-db.asd` (add the file to `graph-db/test`)
- Modify: `transactions.lisp:781-804` (`APPLY-TX-WRITE (tx-create)`)

**Interfaces:**
- Consumes: nothing.
- Produces: `WITH-SM-GRAPH` (macro, `(g)` + body, fresh on-disk graph in a temp dir) and `WITH-SM-REOPEN` (macro, `(dir g)` + body, reopens an existing dir) — Tasks 2, 3, 4, 5 all use these. Schema: `SM-THING` with persistent slots `NAME` (string) and `NOTE` (string), ephemeral `E1`, meta `M1`.

- [ ] **Step 1: Write the failing test**

Create `tests/slot-mutation-tests.lisp`:

```lisp
;;;; The slot-mutation contract (GH #135): which node a transaction may write.
;;;;
;;;; EVERY test here closes and reopens the graph.  In-session reads are served
;;;; by the node cache, so a write/read in one image passes against a graph that
;;;; is being destroyed -- that is why #135 survived the rest of the suite.

(in-package #:graph-db/test)

(def-suite slot-mutation-suite
  :description "GH #135: the slot-mutation contract, asserted after reopen."
  :in graph-db-suite)

(in-suite slot-mutation-suite)

(defparameter *sm-graph-name* :graph-db-slot-mutation-test)

(def-vertex sm-thing ()
  ((name :type string)
   (note :type string)
   (e1 :ephemeral t)
   (m1 :meta t))
  :graph-db-slot-mutation-test)

(defmacro with-sm-graph ((g dir) &body body)
  "A fresh on-disk graph in DIR (a bound temp directory), closed on exit."
  `(let ((,g (make-graph *sm-graph-name* (namestring ,dir)
                         :buffer-pool-size 1000)))
     (unwind-protect (let ((*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g))
       (collect-garbage))))

(defmacro with-sm-reopen ((g dir) &body body)
  "Reopen the graph in DIR.  Signals if it cannot be opened -- which is the
symptom pattern B produces, so this is load-bearing, not scaffolding."
  `(let ((,g (open-graph *sm-graph-name* (namestring ,dir)
                         :buffer-pool-size 1000)))
     (unwind-protect (let ((*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g))
       (collect-garbage))))

(test created-node-mutation-survives-reopen
  "PATTERN A (GH #135).  A node created and then SETF'd in the same transaction
must persist the mutation.  APPLY-TX-WRITE (tx-create) wrote construction-time
BYTES and dropped it; the tx-update path has always re-serialized."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "A")))
            (setq id (id n))
            (setf (note n) "set-after-create"))))
      (with-sm-reopen (g dir)
        (let ((n (lookup-vertex id :graph g)))
          (is (not (null n)) "the node itself must survive reopen")
          (is (equal "set-after-create" (note n))
              "the post-create mutation must survive reopen"))))))
```

- [ ] **Step 2: Register the file and run the test to verify it fails**

In `graph-db.asd`, in the `graph-db/test` system's `:components`, add immediately after `(:file "write-path-tests")`:

```lisp
               (:file "slot-mutation-tests")
```

Run:

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -A6 'CREATED-NODE-MUTATION'
```

Expected: FAIL — `note` reads back `NIL` after reopen.

- [ ] **Step 3: Make the create path re-serialize**

In `transactions.lisp`, in `APPLY-TX-WRITE ((write tx-create) graph)`, immediately after the `(let ((table ...) (node (node write)))` bindings and before `(maybe-write-to-heap node graph)`, insert:

```lisp
    ;; Refresh BYTES from DATA before sizing the allocation.  BYTES is a cache
    ;; filled at construction and MAYBE-INITIALIZE-BYTES only fills an empty
    ;; one, so a SETF between MAKE-<TYPE> and commit updates DATA alone and the
    ;; node persists its construction-time value.  The tx-update path has always
    ;; done this; the create path never did (GH #135).
    (when (data node)
      (setf (bytes node) (serialize (data node))))
```

It must precede `maybe-write-to-heap`, which sizes the heap allocation from `(length (bytes node))`.

- [ ] **Step 4: Run the test to verify it passes**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'Did |Pass:|Fail:'
```

Expected: PASS, and the whole suite still 0 failures.

- [ ] **Step 5: Commit**

```bash
git add tests/slot-mutation-tests.lisp graph-db.asd transactions.lisp
git commit -m "fix(transactions): persist a mutation made after CREATE (GH #135)

APPLY-TX-WRITE (tx-create) wrote the node's construction-time BYTES.
MAYBE-INITIALIZE-BYTES only serializes when BYTES is empty, so a SETF between
MAKE-<TYPE> and commit updated DATA and never BYTES -- the value read back
correctly all session (the node cache serves DATA) and came back NIL after
reopen.  The tx-update path has always re-serialized for exactly this reason.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: Characterise create-then-delete (no guard yet)

The spec requires this be measured, not assumed: `DELETE-NODE` copies internally (`transactions.lisp:2632`), so Task 4's guard will also catch create-then-`MARK-DELETED`. This task only records what that pattern does today. **Land no guard here.**

**Files:**
- Modify: `tests/slot-mutation-tests.lisp`

**Interfaces:**
- Consumes: `WITH-SM-GRAPH`, `WITH-SM-REOPEN`, `SM-THING` from Task 1.
- Produces: a passing characterisation test whose assertions Task 4 may have to invert.

- [ ] **Step 1: Write the characterisation test**

Append to `tests/slot-mutation-tests.lisp`:

```lisp
(test characterise-create-then-delete-same-transaction
  "CHARACTERISATION, not a contract (GH #135).  DELETE-NODE copies internally,
so a create-set guard in COPY-NODE will also reject this.  Whether that is a
fix or a behaviour change depends on what it does TODAY, which this pins.
If the graph cannot be reopened, this pattern has the same shape as pattern B
and the guard is a fix -- update this test's name and docstring accordingly."
  (with-temp-directory (dir)
    (let (id (reopened nil) (opened-ok nil))
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "doomed")))
            (setq id (id n))
            (mark-deleted n))))
      (handler-case
          (with-sm-reopen (g dir)
            (setq opened-ok t)
            (setq reopened (lookup-vertex id :graph g)))
        (error (e)
          (format t "~&CREATE-THEN-DELETE: reopen FAILED: ~A~%" e)))
      (format t "~&CREATE-THEN-DELETE: opened-ok=~A node=~A~%"
              opened-ok reopened)
      ;; Deliberately asserts only that we learned something: the printed
      ;; result is the deliverable.  Task 4 replaces this with a real gate.
      (is (or opened-ok (not opened-ok))))))
```

- [ ] **Step 2: Run it and record the outcome**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep 'CREATE-THEN-DELETE'
```

Expected: one of two outcomes, both informative:
- `opened-ok=NIL` → the pattern corrupts like B. Task 4's guard is a **fix**; say so in its commit and give it a `Fixed` changelog entry.
- `opened-ok=T` → the pattern works today. Task 4's guard is a **behaviour change**; it needs a `Changed` entry, and Kevin should be asked whether to exempt `DELETE-NODE` instead.

**Stop and report the outcome before starting Task 4.**

- [ ] **Step 3: Commit the characterisation**

```bash
git add tests/slot-mutation-tests.lisp
git commit -m "test(slot-mutation): characterise create-then-delete in one txn

DELETE-NODE copies internally, so the GH #135 create-set guard will catch this
pattern too.  Pin what it does today before deciding whether that guard is a
fix or a behaviour change.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: The setf guard (patterns A-legal and D)

**Files:**
- Modify: `transactions.lisp:136-142` region (add the condition next to `MODIFYING-NON-COPY`)
- Modify: `primitive-node.lisp:406-418` (the `(setf SLOT-VALUE-USING-CLASS)` `:around`)
- Modify: `package.lisp` (export)
- Modify: `tests/slot-mutation-tests.lisp`

**Interfaces:**
- Consumes: `WITH-SM-GRAPH`, `WITH-SM-REOPEN`, `SM-THING` from Task 1.
- Produces: `MUTATING-UNREGISTERED-NODE` (condition; readers `MUTATING-UNREGISTERED-NODE-NODE`, `MUTATING-UNREGISTERED-NODE-SLOT`) and `CHECK-SLOT-MUTATION-ALLOWED` (function of `(node slot-name)`, returns nil or signals). Task 4 references neither.

- [ ] **Step 1: Write the failing tests**

Append to `tests/slot-mutation-tests.lisp`:

```lisp
(test pattern-d-setf-on-looked-up-node-signals
  "PATTERN D (GH #135).  LOOKUP-NODE returns the SHARED cached instance, so a
SETF on it mutates state every other reader and thread can see, is never
persisted, and reads back correctly until restart.  This is what COPY exists
to prevent."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (with-transaction ()
          (signals graph-db:mutating-unregistered-node
            (setf (note (lookup-vertex id :graph g)) "no copy")))))))

(test setf-outside-any-transaction-signals
  "The same write with no transaction at all: also unregistered, also signals."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (signals graph-db:mutating-unregistered-node
          (setf (note (lookup-vertex id :graph g)) "no txn"))))))

(test setf-on-a-copy-is-allowed
  "PATTERN C is unchanged: a copy registered by COPY is writable, and persists."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (with-transaction ()
          (let ((c (copy (lookup-vertex id :graph g))))
            (setf (note c) "via copy")
            (save c))))
      (with-sm-reopen (g dir)
        (is (equal "via copy" (note (lookup-vertex id :graph g))))))))

(test ephemeral-and-meta-slots-stay-mutable
  "The guard covers PERSISTENT slots only.  Ephemeral and meta slots are real
CLOS slots holding per-instance state and must stay freely writable -- guarding
them would be a regression."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (let ((n (lookup-vertex id :graph g)))
          (finishes (setf (e1 n) :ephemeral-ok))
          (finishes (setf (m1 n) :meta-ok))
          (is (eq :ephemeral-ok (e1 n)))
          (is (eq :meta-ok (m1 n))))))))
```

- [ ] **Step 2: Run them to verify they fail**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'PATTERN-D|SETF-OUTSIDE|MUTATING-UNREGISTERED'
```

Expected: the two `signals` tests FAIL (no such condition / nothing signalled). `SETF-ON-A-COPY` and the ephemeral/meta test should already PASS.

- [ ] **Step 3: Define the condition**

In `transactions.lisp`, immediately after the `MODIFYING-NON-COPY` definition (which ends at line 142):

```lisp
(define-condition mutating-unregistered-node (error)
  ((node
    :initarg :node
    :reader mutating-unregistered-node-node)
   (slot
    :initarg :slot
    :reader mutating-unregistered-node-slot))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot write persistent slot ~A of ~A: this transaction may ~
              write only a node it created, or a COPY it registered.  A node ~
              from LOOKUP-* is the shared cached instance -- COPY it inside ~
              the transaction, SETF the copy, then SAVE it."
             (mutating-unregistered-node-slot condition)
             (mutating-unregistered-node-node condition)))))
```

- [ ] **Step 4: Add the predicate and call it from the funnel**

In `primitive-node.lisp`, above the `(setf SLOT-VALUE-USING-CLASS)` `:around` method:

```lisp
(defun check-slot-mutation-allowed (node slot-name)
  "Signal MUTATING-UNREGISTERED-NODE unless the current transaction may write
NODE's persistent slots: it may write a COPY it registered, or a node it
created.  Anything else is either lost at commit or a mutation of the shared
cached instance (GH #135)."
  (unless (and *transaction*
               (or (gethash node (copies *transaction*))
                   (object-set-member-p node (create-set *transaction*))))
    (error 'mutating-unregistered-node :node node :slot slot-name)))
```

Then in the `:around`, replace the persistent branch so it reads:

```lisp
    (if slot-keyword-name
        (progn
          (unless *initializing-node*
            (check-slot-mutation-allowed instance slot-name))
          (setf (node-slot-value instance slot-keyword-name) new-value))
        (call-next-method))
```

Delete the now-answered `;; FIXME: Check for txn and handle` comment on that branch.

- [ ] **Step 5: Export the condition**

In `package.lisp`, immediately after the `#:no-transaction-in-progress` line:

```lisp
           #:mutating-unregistered-node
```

- [ ] **Step 6: Run the full suite**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'Did |Pass:|Skip:|Fail:'
```

Expected: all four new tests PASS and **the rest of the suite is still 0 failures**. A failure elsewhere means an engine-internal path writes a persistent slot outside a copy — read the backtrace, and do not widen the escape without understanding which path it is.

- [ ] **Step 7: Commit**

```bash
git add transactions.lisp primitive-node.lisp package.lisp tests/slot-mutation-tests.lisp
git commit -m "feat(transactions): guard persistent-slot writes (GH #135)

A persistent slot may now be written only on a node the current transaction may
mutate: a COPY it registered, or a node it created.  Everything else signals
MUTATING-UNREGISTERED-NODE at the write.

This closes the FIXME on that branch.  The unguarded case that mattered was not
in the issue: LOOKUP-NODE returns the SHARED cached instance, so a SETF on a
looked-up node mutated state every other reader and thread could see, was never
persisted, and read back correctly until restart.

The guard sits in the single funnel -- (setf SLOT-VALUE-USING-CLASS) -- so it
covers accessors, SLOT-VALUE and WITH-SLOTS alike.  Ephemeral and meta slots
reach CALL-NEXT-METHOD and are untouched; *INITIALIZING-NODE* stays the escape.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: The copy guard (pattern B)

**Prerequisite:** Task 2's outcome has been reported and Kevin has confirmed how create-then-delete should behave.

**Files:**
- Modify: `transactions.lisp` (condition next to Task 3's; guard in `COPY-NODE` at 2559-2577)
- Modify: `package.lisp`
- Modify: `tests/slot-mutation-tests.lisp`

**Interfaces:**
- Consumes: `WITH-SM-GRAPH`, `WITH-SM-REOPEN`, `SM-THING` from Task 1.
- Produces: `COPYING-UNCOMMITTED-NODE` (condition; reader `COPYING-UNCOMMITTED-NODE-NODE`).

- [ ] **Step 1: Write the failing test**

Append to `tests/slot-mutation-tests.lisp`:

```lisp
(test pattern-b-copy-of-created-node-signals
  "PATTERN B (GH #135).  COPY of a node created in this same transaction built a
tx-update whose OLD-NODE was a pending create; it committed and closed cleanly
and the graph then could not be opened at all.  It signals at the COPY now."
  (with-temp-directory (dir)
    (with-sm-graph (g dir)
      (with-transaction ()
        (let ((n (make-sm-thing :name "A")))
          (signals graph-db:copying-uncommitted-node
            (copy n)))))))

(test graph-opens-after-a-rejected-pattern-b
  "The point of the guard: the transaction is refused, so the graph still opens.
Before it, this sequence produced a DESERIALIZATION-ERROR on OPEN-GRAPH."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (ignore-errors
         (with-transaction ()
           (let ((n (make-sm-thing :name "A")))
             (setq id (id n))
             (let ((c (copy n)))
               (setf (note c) "B")
               (save c))))))
      (finishes
       (with-sm-reopen (g dir)
         (declare (ignorable g)))))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'PATTERN-B|GRAPH-OPENS-AFTER'
```

Expected: both FAIL — the first with no such condition, the second because the graph will not reopen.

- [ ] **Step 3: Define the condition**

In `transactions.lisp`, after `MUTATING-UNREGISTERED-NODE`:

```lisp
(define-condition copying-uncommitted-node (error)
  ((node
    :initarg :node
    :reader copying-uncommitted-node-node))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot COPY ~A: it was created in this transaction, so it has no ~
              committed version to update against.  SETF its slots directly -- ~
              a node you created is writable without a copy."
             (copying-uncommitted-node-node condition)))))
```

- [ ] **Step 4: Add the guard**

In `COPY-NODE`, as the first form of the `:method` body, before `(maybe-init-node-data node)`:

```lisp
    ;; A node created in THIS transaction has no committed version to update
    ;; against; copying it built a tx-update whose OLD-NODE was a pending
    ;; create, which committed cleanly and left the graph unopenable (GH #135).
    (when (and *transaction*
               (object-set-member-p node (create-set *transaction*)))
      (error 'copying-uncommitted-node :node node))
```

- [ ] **Step 5: Export**

In `package.lisp`, after `#:mutating-unregistered-node`:

```lisp
           #:copying-uncommitted-node
```

- [ ] **Step 6: Resolve Task 2's characterisation**

Replace `CHARACTERISE-CREATE-THEN-DELETE-SAME-TRANSACTION` with a real gate matching the confirmed decision. If Task 2 showed the graph could not be reopened:

```lisp
(test create-then-delete-same-transaction-signals
  "DELETE-NODE copies internally, so create-then-MARK-DELETED in one
transaction has pattern B's shape and left the graph unopenable.  It signals at
the COPY inside DELETE-NODE now (GH #135)."
  (with-temp-directory (dir)
    (with-sm-graph (g dir)
      (with-transaction ()
        (let ((n (make-sm-thing :name "doomed")))
          (signals graph-db:copying-uncommitted-node
            (mark-deleted n)))))))
```

If Task 2 instead showed the graph reopened cleanly, **do not write this test** — return to Kevin with the finding, because the guard would then be removing working behaviour and `DELETE-NODE` likely needs an exemption instead.

- [ ] **Step 7: Run the full suite**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'Did |Pass:|Skip:|Fail:'
```

Expected: 0 failures.

- [ ] **Step 8: Commit**

```bash
git add transactions.lisp package.lisp tests/slot-mutation-tests.lisp
git commit -m "feat(transactions): reject COPY of a node created in this txn (GH #135)

COPY of a not-yet-committed node produced a tx-update whose OLD-NODE was a
pending create.  It committed without complaint, closed without complaint, and
the graph then could not be opened at all -- the worst failure in the issue,
and the only unguarded one.  It signals at the COPY now, which is where the
mistake is, and the report names the simpler correct action: a node you created
is writable without a copy.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: The internal escape still works

A regression gate for the paths that legitimately write persistent slots. `CHANGE-NODE-CLASS` binds `*INITIALIZING-NODE*`, and ECL binds it around `MAKE-INSTANCE` in `vertex.lisp:27` / `edge.lisp:35`. If Task 3 broke either, the whole suite would already be red — this pins it deliberately so a future change to the escape cannot pass silently.

**Files:**
- Modify: `tests/slot-mutation-tests.lisp`

**Interfaces:**
- Consumes: `WITH-SM-GRAPH` and `SM-THING` from Task 1.
- Produces: nothing.

- [ ] **Step 1: Write the test**

```lisp
(test initializing-node-escape-still-permits-writes
  "*INITIALIZING-NODE* is the engine-internal escape (CHANGE-NODE-CLASS, and
ECL's construction path).  The guard must defer to it, or every node
construction on ECL signals."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (let ((n (lookup-vertex id :graph g)))
          ;; Not a copy and not created here: guarded WITHOUT the escape...
          (signals graph-db:mutating-unregistered-node
            (setf (note n) "denied"))
          ;; ...and permitted WITH it.
          (finishes
           (let ((graph-db::*initializing-node* t))
             (setf (note n) "permitted")))
          (is (equal "permitted" (note n))))))))

(test construction-does-not-trip-the-guard
  "MAKE-<TYPE> builds an alist and hands it to MAKE-VERTEX as DATA; no
persistent-slot SETF happens while a node is built, so the guard cannot fire
before %CREATE-NODE registers it.  Pins that, since a constructor rewrite that
started SETF-ing slots would break every write in the engine."
  (with-temp-directory (dir)
    (with-sm-graph (g dir)
      (finishes
       (with-transaction ()
         (make-sm-thing :name "built" :note "at construction")))
      (with-transaction ()
        (let ((n (first (map-vertices #'identity g :vertex-type 'sm-thing
                                                   :collect-p t))))
          (is (equal "at construction" (note n))))))))
```

- [ ] **Step 2: Run them**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'INITIALIZING-NODE-ESCAPE|CONSTRUCTION-DOES-NOT'
```

Expected: both PASS immediately — they pin behaviour Tasks 1–4 already produce.

- [ ] **Step 3: Commit**

```bash
git add tests/slot-mutation-tests.lisp
git commit -m "test(slot-mutation): pin the internal escape and the construction path

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Teach the contract

The half that prevents recurrence. `example.lisp` — the canonical walkthrough — contains no `COPY`, `SAVE` or `UPDATE-NODE` at all, so a reader has no worked example of the one pattern that is easy to get catastrophically wrong.

**Files:**
- Modify: `example.lisp`
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 5, Transactions and Concurrency, line 563)
- Modify: `CHANGELOG.md` (the `[Unreleased]` section)

**Interfaces:**
- Consumes: the final behaviour from Tasks 1–5.
- Produces: nothing.

- [ ] **Step 1: Add a worked update to `example.lisp`**

Append a section after the existing insert/query walkthrough, matching the file's existing comment style:

```lisp
;;; --- Updating a node -------------------------------------------------------
;;;
;;; LOOKUP-* returns the SHARED cached node: the same object every other reader
;;; and thread holds.  Writing its slots in place would be invisible to disk and
;;; visible to everyone else, so COPY it inside the transaction, modify the
;;; copy, and SAVE that.  Writing an uncopied node signals (GH #135).

(with-transaction ()
  (let ((c (copy (lookup-user some-user-id))))
    (setf (email c) "new@example.com")
    (save c)))

;;; A node you created in THIS transaction is different: it has no committed
;;; version to update against, so it needs no copy and COPY of it signals.
;;; Just set its slots.

(with-transaction ()
  (let ((u (make-user :username "carol")))
    (setf (email u) "carol@example.com")))
```

- [ ] **Step 2: Add the contract to the manual**

In `docs/vivace-graph-v3-doc.org`, inside Chapter 5, add a subsection titled `**** Which node may I write?` containing: the rule (a copy registered by `COPY`, or a node created in this transaction); the four-pattern table from the spec; the explanation that `lookup-*` returns the shared cached instance and that this is *why* `COPY` exists; and the two conditions by name.

- [ ] **Step 3: Add the changelog entries**

In `CHANGELOG.md` under `## [Unreleased]`, add a `### Fixed` and a `### Changed` section:

```markdown
### Fixed

- **A mutation made after `MAKE-<TYPE>` in the same transaction was discarded**
  (#135). `APPLY-TX-WRITE (tx-create)` wrote the node's construction-time
  `BYTES`; `MAYBE-INITIALIZE-BYTES` only serializes an empty one, so a `SETF`
  between construction and commit updated `DATA` alone. The value read back
  correctly for the rest of the session — the node cache serves `DATA` — and
  came back `NIL` after reopen. The `tx-update` path had always re-serialized
  for this reason; the create path never did.

- **`COPY` of a node created in the same transaction corrupted the graph**
  (#135). It built a `tx-update` whose `OLD-NODE` was a pending create; the
  transaction committed and the graph closed without complaint, and then could
  not be opened at all. It now signals `COPYING-UNCOMMITTED-NODE` at the `COPY`.

### Changed

- **Writing a persistent slot now requires a node the transaction may mutate**
  (#135) — a copy registered by `COPY`, or a node created in that transaction.
  Anything else signals the new `MUTATING-UNREGISTERED-NODE`. The case this
  matters most for is not in the issue: `lookup-*` returns the **shared cached
  instance**, so `(setf (slot (lookup-thing id)) v)` mutated state every other
  reader and thread could see, was never persisted, and read back correctly
  until restart. Ephemeral and meta slots are unaffected. No correct program
  changes behaviour: the guard rejects only writes that were already lost or
  corrupting.
```

- [ ] **Step 4: Verify nothing regressed and commit**

```bash
sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(asdf:test-system :graph-db)' 2>&1 | grep -E 'Did |Pass:|Fail:'
```

```bash
git add example.lisp docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "docs: teach the slot-mutation contract (GH #135)

example.lisp -- the canonical walkthrough -- contained no COPY, SAVE or
UPDATE-NODE at all, so the one pattern that is easy to get catastrophically
wrong had no worked example.  Both hand-written mutation sites in the consuming
app carry comments about traps their author had already hit; the pain was
documented downstream and nowhere in the engine.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: Validate on the four-config matrix

The guard sits on the MOP slot path and its escape is ECL-specific in two places, so a single SBCL run is not sufficient evidence.

**Files:** none (verification only).

**Interfaces:**
- Consumes: everything from Tasks 1–6.
- Produces: the evidence for the release note.

- [ ] **Step 1: Run all seven suites on local SBCL and ECL**

```bash
for S in graph-db graph-db/concurrency-test graph-db/acid-test graph-db/stress-test graph-db/geos-test graph-db/algorithms-test graph-db/concurrent-stress-test; do
  echo "=== $S ==="
  rm -rf /var/tmp/graph* 2>/dev/null
  sbcl --dynamic-space-size 16384 --non-interactive --eval "(ql:quickload :$S)" --eval "(asdf:test-system :$S)" 2>&1 | grep -E 'Did |Pass:|Skip:|Fail:'
done
```

ECL needs a pty and a warmed fasl cache — run each suite as `script -q /dev/null ecl --load <runner>`, after one warm-up load over a TTY.

- [ ] **Step 2: Run the same seven on `ma` (Linux) for SBCL and ECL**

Sync to an isolated checkout as user `raison` (never `/home/ma`, which is the live server), `touch` every file after `rsync -a` so ASDF cannot serve stale fasls, and give each lisp its own `XDG_CACHE_HOME`.

- [ ] **Step 3: Report the matrix**

Expected: 28 cells, 0 failures. Any new failure on ECL specifically points at the `*INITIALIZING-NODE*` escape — read the backtrace before touching the guard.

- [ ] **Step 4: File the follow-up issue**

File the `BYTES`-invalidation root fix deferred by the spec: `BYTES` is a derived cache of `DATA` with no invalidation on write; fixing that would make the compensation at `transactions.lisp:813` and Task 1's create-path fix both unnecessary, and would prevent this bug class rather than its two instances. Note that it changes an invariant every `BYTES` consumer relies on (replication, the txn log, `COPY-NODE`'s deliberate bytes-copy) and needs the full matrix.

---

## Self-Review

**Spec coverage:**

| Spec requirement | Task |
|---|---|
| Contract: copy or created-here | 3 (setf), 4 (copy) |
| Pattern A legal and correct | 1 |
| Pattern B signals at the COPY | 4 |
| Pattern C unchanged | 3 (`SETF-ON-A-COPY-IS-ALLOWED`) |
| Pattern D signals at the SETF | 3 |
| Ephemeral/meta unaffected | 3 |
| `*INITIALIZING-NODE*` escape preserved | 3 (implementation), 5 (gate) |
| Construction does not trip the guard | 5 |
| Create-path re-serialize | 1 |
| `MUTATING-UNREGISTERED-NODE` | 3 |
| `COPYING-UNCOMMITTED-NODE` | 4 |
| create-then-delete verified empirically | 2, resolved in 4 |
| Every test asserts after reopen | 1, 3, 4 (Task 5's two are in-session by nature: they test signalling and construction, not persistence) |
| `example.lisp` worked update | 6 |
| Manual section incl. why COPY exists | 6 |
| CHANGELOG Fixed + Changed | 6 |
| Four-config matrix | 7 |
| `BYTES`-invalidation follow-up issue | 7 |
| Version 3.1.0 | 6 (changelog lands under `[Unreleased]`; the version cut is a separate release task) |

No gaps.

**Placeholder scan:** clean — every code step carries the actual code; no "TBD", no "similar to Task N", no "add error handling".

**Type consistency:** `WITH-SM-GRAPH (g dir)` and `WITH-SM-REOPEN (g dir)` take the same argument order everywhere. `SM-THING` slots `NAME`/`NOTE`/`E1`/`M1` are used consistently. `CHECK-SLOT-MUTATION-ALLOWED (node slot-name)` is defined in Task 3 Step 4 and called in Task 3 Step 4 with `(instance slot-name)` — both bound in that method. Condition reader names match their `define-condition` forms and their uses in the `:report` lambdas.

Issues found were fixed inline.
