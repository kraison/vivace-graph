# Vector Segment Transaction Integration (Phase 2, Step 3) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Keep a vector segment in sync with node creates/updates/deletes on the transaction apply path, declared by a `:vector-index` slot option, with lazy creation and crash recovery by rebuild-from-nodes.

**Architecture:** Mirror the spatial index end to end. A new `:vector-index` slot option (plumbed exactly like `:unique`) marks a slot; the graph owns a per-`(class,slot)` `vector-segments` hash (mirroring `unique-indexes`); `apply-tx-write-to-vector-segments` methods (mirroring `apply-tx-write-to-spatial-index`) maintain segments inside the transaction write lock; a clean-shutdown header flag drives rebuild-from-nodes (mirroring `rebuild-spatial-index`) on an unclean open.

**Tech Stack:** Common Lisp (SBCL), ASDF, closer-mop, FiveAM, the Step-2 `segment.lisp`.

## Global Constraints

- **SBCL only. ECL is out of scope** — do not run ECL, do not add ECL reader conditionals.
- **Lisp indentation is spaces only, never tabs.**
- **This is Step 3 of 5. Build ONLY transaction integration + declaration + recovery.** NO `segment-scan`/`segment-score-subset` (Step 4). NO cl-llm integration (Step 5). NO `def-vector-index` macro (deferred). If you touch a query/scan path or `cl-llm`, stop.
- **Mirror existing precedents; do not invent parallel machinery.** `:unique` (`node-class.lisp`) is the slot-option template; `unique-indexes` (`graph-class.lisp`) is the graph-ownership template; `apply-tx-write-to-spatial-index` (`transactions.lisp:852-873`) is the apply-hook template; `node-geometry-index-slots` (`transactions.lisp:813`) is the slot-lookup template; `rebuild-spatial-index` (`spatial-query.lisp:174`) is the rebuild template.
- **Maintenance runs inside the transaction write lock** (`apply-transaction`, `transactions.lisp:966`) — so it needs no new lock, and a `segment-put` that grows is exclusive for free. **Do not add locking.**
- **`live-count` is occupancy, not an iterator.** Any node sweep walks the graph via `map-vertices`/`map-edges`; any segment slot sweep walks `[0, capacity)` skipping free cells. Never iterate `[0, live-count)`.
- **The standing test discipline:** every consistency assertion must be able to fail. Guard NIL/empty before any `every`/`some`/`loop` that could pass vacuously — `(every #'= expected nil) => T` has recurred seven times in this project. For the load-bearing gates (the invariant test, the rebuild test, the delete-removes test), include a **sabotage proof**: break the maintenance, confirm the test fails, restore.

## What already exists (Step 2, `segment.lisp`)

`create-vector-segment (path dimension &key initial-capacity)`, `open-vector-segment (path)`, `close-vector-segment (segment)`, `segment-put (segment id vector)` → slot (signals on dimension mismatch and on an all-ones id), `segment-get (segment id)` → `(simple-array single-float (*))` or nil, `segment-remove (segment id)` → t/nil, `segment-capacity`, `segment-live-count`, `segment-dimension`, in-place growth. Header is 64 bytes: magic(0) format(8) dimension(16) element-type(24) capacity(32) live-count(40) free-head(48) **reserved(56, written as literal 0)**.

## File Structure

| file | responsibility | change |
|---|---|---|
| `node-class.lisp` | `:vector-index` slot option + inheritance | modify |
| `graph-class.lisp` | `vector-segments` graph slot | modify |
| `transactions.lisp` | `node-vector-index-slots`; apply hooks; `%ensure-segment` | modify |
| `segment.lisp` | clean-shutdown flag; `rebuild-vector-segment` | modify |
| `graph.lisp` | open-path open-or-rebuild; `close-graph` teardown | modify |
| `tests/segment-integration-tests.lisp` | all Step-3 tests | create |

`node-vector-index-slots`, the apply hooks, and `%ensure-segment` go in `transactions.lisp` because that is where `node-geometry-index-slots` and `apply-tx-write-to-spatial-index` already live. `rebuild-vector-segment` goes in `segment.lisp` (it is segment machinery); the open/close wiring goes in `graph.lisp` alongside the spatial-index wiring.

---

### Task 1: The `:vector-index` slot option

**Files:**
- Modify: `node-class.lisp:10-22` (the `node-slot-definition` class), the default-method block near `node-class.lisp:24-40`, and the inheritance block in `compute-effective-slot-definition :around` near `node-class.lisp:118-127`
- Modify: `graph-db.asd` (register the new test file — do this here so Task 1's test runs)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes: nothing from earlier tasks
- Produces: slot option `:vector-index` on `def-vertex`/`def-edge` slots; reader `vector-index-p (slot)` → boolean; inheritance so a `:vector-index` slot on a parent applies to subclasses

- [ ] **Step 1: Write the failing test**

Create `tests/segment-integration-tests.lisp`:

```lisp
;;;; Tests for vector-segment transaction integration (Phase 2 step 3).

(in-package #:graph-db/test)

(def-suite segment-integration-suite
  :description "vector-index declaration, apply-path maintenance, recovery."
  :in graph-db-suite)

(in-suite segment-integration-suite)

;; Declared once at load time, like the other integration schema in this file.
(def-vertex si-doc ()
  ((title :type string)
   (embedding :vector-index t))
  :segment-integration-test)

(def-vertex si-sub (si-doc)
  ((extra))
  :segment-integration-test)

(test vector-index-slot-is-recognised
  "A :vector-index slot reports vector-index-p on the effective slot, and the
option is inherited by a subclass."
  (let ((doc-slot (find 'embedding (graph-db::class-slots (find-class 'si-doc))
                        :key #'graph-db::slot-definition-name))
        (sub-slot (find 'embedding (graph-db::class-slots (find-class 'si-sub))
                        :key #'graph-db::slot-definition-name)))
    (is (graph-db::vector-index-p doc-slot))
    (is (graph-db::vector-index-p sub-slot)
        "a :vector-index slot on the parent must apply to the subclass")))
```

Register the file in `graph-db.asd`'s `graph-db/test` system after `segment-tests`:

```lisp
               (:file "segment-integration-tests")
```

- [ ] **Step 2: Run it to verify it fails**

```
cd /Users/kraison/work/vivace-graph-v3
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: FAIL — `:vector-index` is not a recognised slot initarg / `vector-index-p` undefined. (An unknown slot option may error at class-definition load; that still counts as red for this step.)

- [ ] **Step 3: Add the slot and reader**

In `node-class.lisp`, add a slot to the `node-slot-definition` class (`node-class.lisp:10`), after the `unique-scope` slot:

```lisp
   (vector-index :accessor vector-index-p :initarg :vector-index :initform nil
                 :allocation :instance)
```

Add a default method alongside the other slot-def defaults (near `node-class.lisp:24-40`, next to `(defmethod unique-spec (slot-def) nil)`):

```lisp
(defmethod vector-index-p (slot-def)
  nil)
```

- [ ] **Step 4: Add inheritance**

In `compute-effective-slot-definition :around` (`node-class.lisp:101`), after the `:UNIQUE` inheritance block (near line 127, just before the closing `slot`), add — mirroring it exactly:

```lisp
    ;; Inherit the :VECTOR-INDEX flag from the declaring direct slot, so a
    ;; :VECTOR-INDEX slot on a parent is indexed across its subclasses (like
    ;; :INDEX / :UNIQUE above).
    (let ((vi (find-if #'vector-index-p direct-slots)))
      (when (or (vector-index-p slot) vi)
        (setf (slot-value slot 'vector-index) (or (vector-index-p slot)
                                                  (and vi (vector-index-p vi))))))
```

- [ ] **Step 5: Run it to verify it passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: PASS. If `class-slots` / `slot-definition-name` are not visible in `graph-db/test`, import them into `tests/package.lisp` (the package does not `:use :graph-db`); note which you added.

- [ ] **Step 6: Commit**

```bash
git add node-class.lisp graph-db.asd tests/segment-integration-tests.lisp tests/package.lisp
git commit -m "feat(segment): :vector-index slot option, plumbed like :unique"
```

---

### Task 2: `node-vector-index-slots` and the `vector-segments` graph slot

**Files:**
- Modify: `transactions.lisp` (near `node-geometry-index-slots`, `transactions.lisp:813`)
- Modify: `graph-class.lisp` (near the `unique-indexes` slot)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes: `vector-index-p` (Task 1)
- Produces: `node-vector-index-slots (class)` → list of slot-name symbols, cached per class; `vector-segments (graph)` → an `equal` hash keyed by `(class-name . slot-name)`

- [ ] **Step 1: Write the failing test**

Append to `tests/segment-integration-tests.lisp`:

```lisp
(test node-vector-index-slots-lists-declared-slots
  "node-vector-index-slots returns the :vector-index slot names of a class."
  (is (member 'embedding
              (graph-db::node-vector-index-slots (find-class 'si-doc))))
  (is (null (graph-db::node-vector-index-slots
             (find-class 'graph-db::vertex)))
      "a class with no :vector-index slot has none"))

(test graph-has-empty-vector-segments-table
  "A fresh graph exposes an empty vector-segments hash."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (is (hash-table-p (graph-db::vector-segments g)))
             (is (= 0 (hash-table-count (graph-db::vector-segments g)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
```

- [ ] **Step 2: Run to verify it fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: FAIL — `node-vector-index-slots` and `vector-segments` undefined.

- [ ] **Step 3: Add the graph slot**

In `graph-class.lisp`, after the `unique-indexes` slot, add:

```lisp
   ;; Vector segments (Phase 2): (class-name . slot-name) -> VECTOR-SEGMENT.
   ;; A derived index maintained on the apply path; created lazily on first
   ;; conforming insert; recovered by rebuild-from-nodes on an unclean open.
   ;; See docs/superpowers/specs/2026-07-21-vector-segment-transaction-integration-design.md
   (vector-segments :accessor vector-segments :initarg :vector-segments
                    :initform (make-hash-table :test 'equal))
```

- [ ] **Step 4: Add `node-vector-index-slots`**

In `transactions.lisp`, immediately after `node-geometry-index-slots` (ends near `transactions.lisp:835`), mirroring it — add the cache var next to `*node-geometry-slot-cache*` and the function:

```lisp
(defvar *node-vector-index-slot-cache* (make-hash-table :test 'eq))

(defun node-vector-index-slots (class)
  "Names of CLASS's :VECTOR-INDEX slots -- the slots that get a vector segment.
Cached per class (runtime schema redefinition is not expected).  Value gating is
done at maintenance time, not here: only a conforming (simple-array single-float
(*)) value is actually indexed."
  (multiple-value-bind (val present) (gethash class *node-vector-index-slot-cache*)
    (if present
        val
        (setf (gethash class *node-vector-index-slot-cache*)
              (when (class-finalized-p class)
                (loop for slot in (class-slots class)
                      when (vector-index-p slot)
                        collect (slot-definition-name slot)))))))
```

- [ ] **Step 5: Run to verify it passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: PASS. Import `vector-segments` into `tests/package.lisp` if referenced unqualified.

- [ ] **Step 6: Commit**

```bash
git add transactions.lisp graph-class.lisp tests/segment-integration-tests.lisp tests/package.lisp
git commit -m "feat(segment): node-vector-index-slots + per-(class,slot) vector-segments on the graph"
```

---

### Task 3: Lazy creation and create-maintenance on the apply path

**Files:**
- Modify: `transactions.lisp` (a helper block near the spatial hooks; the `apply-transaction` body at `transactions.lisp:983-991`)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes: `node-vector-index-slots`, `vector-segments` (Task 2); `create-vector-segment`, `segment-put`, `segment-get` (Step 2); the `tx-create`/`tx-update`/`tx-delete` classes and `node`/`old-node`/`id` accessors used by `apply-tx-write-to-spatial-index`
- Produces: `%conforming-vector-p (v)` → boolean; `%node-segment-value (node slot-name)` → the vector or nil; `%ensure-segment (graph class-name slot-name dimension)` → `vector-segment`; `apply-tx-write-to-vector-segments (write graph)` generic + a `tx-create` method; `apply-tx-writes-to-vector-segments (writes graph)`; a call to it in `apply-transaction`

- [ ] **Step 1: Write the failing test**

Append to `tests/segment-integration-tests.lisp`:

```lisp
(defun %si-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(defun %si-segment (graph slot)
  (gethash (cons 'si-doc slot) (graph-db::vector-segments graph)))

(test create-populates-the-segment
  "Creating a node with a conforming :vector-index value, through a transaction,
lazily creates the segment and stores the vector under the node id."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0))))))
             (let ((seg (%si-segment g 'embedding)))
               (is (not (null seg)) "segment was not created on insert")
               (let ((back (graph-db::segment-get seg id)))
                 (is (typep back '(simple-array single-float (*)))
                     "vector not stored (got ~S)" back)
                 (is (= 8 (length back)))
                 (is (every #'= (%si-embedding 8 1.0) back)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test create-without-conforming-value-makes-no-segment
  "A node whose :vector-index slot is nil creates no segment."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction () (make-si-doc :title "no-vec")))
             (is (null (%si-segment g 'embedding))
                 "a nil embedding must not create a segment"))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
```

The schema uses `si-doc` under `:segment-integration-test`, but `make-graph` uses `*integration-graph-name*`. Confirm the file loads its schema under the graph name the tests open, matching the existing pattern in `graph-tests.lisp`; if `si-doc` must be declared under `*integration-graph-name*`, adjust the `def-vertex` graph-name argument in Task 1 accordingly and note it.

- [ ] **Step 2: Run to verify it fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: FAIL — no maintenance runs, so no segment is created.

- [ ] **Step 3: Implement the helpers and the create hook**

In `transactions.lisp`, after the spatial-index hooks (near `transactions.lisp:875`), add:

```lisp
(defun %conforming-vector-p (v)
  "True when V is a value a vector segment can store."
  (typep v '(simple-array single-float (*))))

(defun %node-segment-value (node slot-name)
  "The conforming vector in NODE's SLOT-NAME, or NIL.  Reads via SLOT-VALUE
directly (NOT slot-boundp) -- persistent slots read as unbound on the backing
CLOS slot, exactly as node-geometry does."
  (let ((v (ignore-errors (slot-value node slot-name))))
    (when (%conforming-vector-p v) v)))

(defun %segment-file (graph class-name slot-name)
  (format nil "~A/vseg-~A-~A.dat"
          (location graph) (string-downcase class-name) (string-downcase slot-name)))

(defun %ensure-segment (graph class-name slot-name dimension)
  "The segment for (CLASS-NAME, SLOT-NAME), created lazily if absent with
DIMENSION (the length of the first conforming vector).  Registered in the graph's
VECTOR-SEGMENTS table."
  (let* ((key (cons class-name slot-name))
         (table (vector-segments graph)))
    (or (gethash key table)
        (setf (gethash key table)
              (create-vector-segment (%segment-file graph class-name slot-name)
                                     dimension)))))

(defgeneric apply-tx-write-to-vector-segments (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-vector-segments ((write tx-create) graph)
  (let ((node (node write)))
    (when (not (deleted-p node))
      (let ((class-name (class-name (class-of node))))
        (dolist (slot (node-vector-index-slots (class-of node)))
          (let ((v (%node-segment-value node slot)))
            (when v
              (let ((seg (%ensure-segment graph class-name slot (length v))))
                (segment-put seg (id node) v)))))))))

(defgeneric apply-tx-writes-to-vector-segments (writes graph)
  (:method (writes graph)
    (dolist (write writes) (apply-tx-write-to-vector-segments write graph))))
```

- [ ] **Step 4: Wire it into `apply-transaction`**

In `apply-transaction` (`transactions.lisp:988`), immediately after the `apply-tx-writes-to-spatial-index` line, add:

```lisp
        (apply-tx-writes-to-vector-segments writes graph)
```

- [ ] **Step 5: Run to verify it passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: `create-populates-the-segment` and `create-without-conforming-value-makes-no-segment` PASS.

- [ ] **Step 6: Commit**

```bash
git add transactions.lisp tests/segment-integration-tests.lisp
git commit -m "feat(segment): lazy creation + create-maintenance on the apply path"
```

---

### Task 4: update and delete maintenance, and dimension-mismatch rollback

**Files:**
- Modify: `transactions.lisp` (add `tx-update` and `tx-delete` methods)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes: everything from Task 3; `segment-remove` (Step 2); `old-node` on `tx-update`/`tx-delete` (as used by `apply-tx-write-to-spatial-index`)
- Produces: `tx-update` and `tx-delete` methods on `apply-tx-write-to-vector-segments`

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-integration-tests.lisp`:

```lisp
(test update-overwrites-the-vector (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) (%si-embedding 8 5.0))
                 (save v))))
           (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
             (is (typep back '(simple-array single-float (*))))
             (is (every #'= (%si-embedding 8 5.0) back))))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test clearing-the-value-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) nil)
                 (save v))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "an update to nil must remove the segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test delete-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction () (mark-deleted (lookup-vertex id :graph g))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "deleting a node must remove its segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test wrong-dimension-signals-and-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             ;; a 9-dim vector into an established 8-dim segment must signal
             (signals error
               (let ((*graph* g))
                 (with-transaction ()
                   (make-si-doc :title "bad" :embedding (%si-embedding 9 2.0))))))
           ;; the good node is still there; the bad transaction rolled back
           (is (every #'= (%si-embedding 8 1.0)
                      (graph-db::segment-get (%si-segment g 'embedding) id)))
           (is (= 1 (graph-db::segment-live-count (%si-segment g 'embedding)))
               "the rolled-back insert must not have landed in the segment"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))
```

The mutation idiom is **copy-modify-save**, verbatim from `graph-tests.lisp:126-138`: `(let ((v (copy (lookup-vertex id)))) (setf (slot-value v 'embedding) …) (save v))` inside a `with-transaction` with `*graph*` bound. There is no `update-node`. `mark-deleted` and `lookup-vertex` are real (`graph-tests.lisp:109`, `vertex.lisp:97`). `save` inside a transaction produces a `tx-update` write, so the update hook fires.

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: `update-overwrites`, `clearing-removes`, `delete-removes` FAIL (no update/delete method). `wrong-dimension` may already pass on the signal but the live-count assertion needs the update path — verify it fails on at least one assertion.

- [ ] **Step 3: Implement the update and delete hooks**

In `transactions.lisp`, add after the `tx-create` method:

```lisp
(defmethod apply-tx-write-to-vector-segments ((write tx-update) graph)
  (let* ((new-node (node write))
         (class-name (class-name (class-of new-node))))
    (dolist (slot (node-vector-index-slots (class-of new-node)))
      (let ((key (cons class-name slot))
            (v (and (not (deleted-p new-node)) (%node-segment-value new-node slot))))
        (if v
            (let ((seg (%ensure-segment graph class-name slot (length v))))
              (segment-put seg (id new-node) v))
            ;; value cleared/invalidated or node now deleted -> drop any entry
            (let ((seg (gethash key (vector-segments graph))))
              (when seg (segment-remove seg (id new-node)))))))))

(defmethod apply-tx-write-to-vector-segments ((write tx-delete) graph)
  (let* ((node (node write))
         (class-name (class-name (class-of node))))
    (dolist (slot (node-vector-index-slots (class-of node)))
      (let ((seg (gethash (cons class-name slot) (vector-segments graph))))
        (when seg (segment-remove seg (id node)))))))
```

- [ ] **Step 4: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: all four PASS.

- [ ] **Step 5: Sabotage-prove `delete-removes-the-entry`**

Temporarily make the `tx-delete` method a no-op (comment out its `segment-remove`), run the suite, and confirm `delete-removes-the-entry` now FAILS (the entry survives, `segment-get` returns the vector, the `(null ...)` assertion trips). Restore. Report both outcomes — this proves the delete gate is real, not vacuous.

- [ ] **Step 6: Commit**

```bash
git add transactions.lisp tests/segment-integration-tests.lisp
git commit -m "feat(segment): update/delete maintenance + dimension-mismatch rollback"
```

---

### Task 5: The clean-shutdown header flag

**Files:**
- Modify: `segment.lisp` (offset-56 flag; `open-vector-segment` exposes the pre-open value; `close-vector-segment` marks clean)
- Test: `tests/segment-tests.lisp` (this is segment-local; keep it with the Step-2 segment tests)

**Interfaces:**
- Consumes: `serialize-uint64`/`deserialize-uint64`, the header offsets (Step 2)
- Produces: constant `+segment-clean-offset+` (56); `segment-clean-shutdown-p (segment)` → boolean reflecting the **pre-open** on-disk value; `open-vector-segment` marks the file dirty (0) after capturing that value; `close-vector-segment` marks it clean (1)

- [ ] **Step 1: Write the failing test**

Append to `tests/segment-tests.lisp`:

```lisp
(test segment-clean-shutdown-flag
  "A cleanly closed segment reopens reporting clean-shutdown; a segment left open
(simulated crash) reopens reporting NOT clean."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           ;; clean lifecycle
           (let ((s (create-vector-segment path 8 :initial-capacity 4)))
             (segment-put s (%id 1) (%vec 8 1.0))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (is (segment-clean-shutdown-p s)
                 "a cleanly closed segment must reopen clean")
             ;; do NOT close -> simulate a crash: the flag was marked dirty on open
             (declare (ignore s)))
           ;; reopen after the un-closed session
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (is (not (segment-clean-shutdown-p s))
                      "a segment left open (crash) must reopen NOT clean")
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))
```

- [ ] **Step 2: Run to verify it fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: FAIL — `segment-clean-shutdown-p` undefined.

- [ ] **Step 3: Implement the flag**

In `globals.lisp`, near the other segment constants:

```lisp
(alexandria:define-constant +segment-clean-offset+ 56)  ; header's reserved uint64
(alexandria:define-constant +segment-clean+ 1)
(alexandria:define-constant +segment-dirty+ 0)
```

In `segment.lisp`, add to the `vector-segment` struct a slot capturing the pre-open value:

```lisp
  (clean-at-open nil)   ; the on-disk clean flag as it was when this segment opened
```

In `open-vector-segment`, after reading the header and BEFORE returning, capture then flip:

```lisp
    ;; Capture the persisted clean flag (the recovery decision reads THIS), then
    ;; mark the file dirty for the new session.
    (let ((clean (= (deserialize-uint64 mmap +segment-clean-offset+) +segment-clean+)))
      (setf (segment-clean-at-open segment) clean)
      (serialize-uint64 mmap +segment-dirty+ +segment-clean-offset+))
```

Add the reader and mark-clean:

```lisp
(defun segment-clean-shutdown-p (segment)
  "True if the segment's on-disk state at open time was cleanly closed."
  (segment-clean-at-open segment))
```

In `close-vector-segment`, before `munmap-file`:

```lisp
    (serialize-uint64 (segment-mmap segment) +segment-clean+ +segment-clean-offset+)
```

- [ ] **Step 4: Run to verify it passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: PASS, and the rest of `segment-suite` still green (the flag lives in previously-unread reserved space).

- [ ] **Step 5: Commit**

```bash
git add globals.lisp segment.lisp tests/segment-tests.lisp
git commit -m "feat(segment): clean-shutdown header flag; dirty on open, clean on close"
```

---

### Task 6: Rebuild, open-path wiring, close-graph teardown, and the invariant

**Files:**
- Modify: `segment.lisp` (`rebuild-vector-segment`)
- Modify: `graph.lisp` (`restore-vector-segments` in the open path; close segments in `close-graph`)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes: `node-vector-index-slots`, `vector-segments`, `%segment-file`, `%conforming-vector-p`, `%node-segment-value` (Tasks 2–3); `segment-clean-shutdown-p` (Task 5); `map-vertices`, `all-vertex-classes-with-vector-index-slots`
- Produces: `rebuild-vector-segment (graph class-name slot-name)`; `restore-vector-segments (graph)`; segment teardown in `close-graph`

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-integration-tests.lisp`:

```lisp
(test segment-survives-clean-reopen
  "After a clean close, reopening the graph opens the segment as-is and its
vectors are intact."
  (with-temp-directory (dir)
    (let ((id nil))
      (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 3.0))))))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
               (is (typep back '(simple-array single-float (*))))
               (is (every #'= (%si-embedding 8 3.0) back)))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))

(test invariant-segment-matches-rebuild
  "After an arbitrary create/update/delete sequence, the live segment equals a
fresh rebuild-from-nodes: same id set, same vectors."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
          (kept '()))
      (unwind-protect
           (progn
             (let ((*graph* g))
               ;; create 12
               (dotimes (i 12)
                 (with-transaction ()
                   (let ((n (make-si-doc :title (format nil "n~d" i)
                                         :embedding (%si-embedding 8 (coerce i 'single-float)))))
                     (push (cons i (id n)) kept))))
               ;; delete 3
               (dolist (i '(2 5 9))
                 (with-transaction () (mark-deleted (lookup-vertex (cdr (assoc i kept)) :graph g)))
                 (setf kept (remove i kept :key #'car)))
               ;; update 2 (copy-modify-save)
               (dolist (i '(0 7))
                 (with-transaction ()
                   (let ((v (copy (lookup-vertex (cdr (assoc i kept)) :graph g))))
                     (setf (slot-value v 'embedding)
                           (%si-embedding 8 (coerce (+ 100 i) 'single-float)))
                     (save v)))))
             ;; snapshot the live segment's (id -> vector) map
             (let* ((live (%si-segment g 'embedding))
                    (live-map (make-hash-table :test 'equalp)))
               (dolist (cell kept)
                 (setf (gethash (cdr cell) live-map)
                       (graph-db::segment-get live (cdr cell))))
               ;; rebuild from nodes into a fresh segment and compare
               (let ((rebuilt (graph-db::rebuild-vector-segment g 'si-doc 'embedding)))
                 (is (= (hash-table-count live-map)
                        (graph-db::segment-live-count rebuilt))
                     "rebuild has a different id count than the live segment")
                 (loop for id being the hash-keys of live-map using (hash-value v)
                       for r = (graph-db::segment-get rebuilt id)
                       do (is (typep r '(simple-array single-float (*)))
                              "id missing from rebuild")
                          (is (and v (every #'= v r))
                              "vector differs between live and rebuilt")))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
```

`rebuild-vector-segment` here returns a fresh segment for comparison; in production it also replaces the registered one (Step 3 of this task). If returning-and-registering complicates the comparison, have it return the rebuilt segment and register it — the test reads the return value.

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: `segment-survives-clean-reopen` FAILS (open path does not wire segments yet — `%si-segment` is nil after reopen); `invariant-segment-matches-rebuild` FAILS (`rebuild-vector-segment` undefined).

- [ ] **Step 3: Implement `rebuild-vector-segment`**

In `segment.lisp`, mirroring `rebuild-spatial-index` (`spatial-query.lisp:174`):

```lisp
(defun rebuild-vector-segment (graph class-name slot-name)
  "Rebuild the (CLASS-NAME, SLOT-NAME) segment from live nodes: drop any current
segment/file, create a fresh one sized to the first conforming vector, and
segment-put every live node's conforming value.  Registers and returns the fresh
segment.  Run when quiescent (at open, before writes) -- it mutates outside the
transaction path, like rebuild-spatial-index."
  (let* ((key (cons class-name slot-name))
         (table (vector-segments graph))
         (path (%segment-file graph class-name slot-name)))
    (let ((old (gethash key table)))
      (when old (close-vector-segment old)))
    (remhash key table)
    (ignore-errors (delete-file path))
    (let ((seg nil))
      (map-vertices
       (lambda (node)
         (unless (deleted-p node)
           (let ((v (%node-segment-value node slot-name)))
             (when v
               (unless seg
                 (setf seg (create-vector-segment path (length v)))
                 (setf (gethash key table) seg))
               (segment-put seg (id node) v)))))
       graph :vertex-type class-name)
      seg)))
```

- [ ] **Step 4: Wire the open path and close-graph**

In `graph.lisp`, add near `restore-spatial-index`:

```lisp
(defun restore-vector-segments (graph)
  "For every class with :VECTOR-INDEX slots, open each existing segment as-is if
it was cleanly closed, else rebuild it from nodes.  Runs at open, before the graph
accepts writes (quiescent)."
  (dolist (class (all-node-classes-with-vector-index-slots graph))
    (let ((class-name (class-name class)))
      (dolist (slot (node-vector-index-slots class))
        (let ((path (%segment-file graph class-name slot)))
          (when (probe-file path)
            (let ((seg (open-vector-segment path)))
              (if (segment-clean-shutdown-p seg)
                  (setf (gethash (cons class-name slot) (vector-segments graph)) seg)
                  (progn
                    (close-vector-segment seg)
                    (rebuild-vector-segment graph class-name slot))))))))))
```

Implement `all-node-classes-with-vector-index-slots` using the engine's existing node-type enumeration — `all-node-types (graph)` (`schema.lisp:185`), the same one `unique-constraint.lisp:301` and `index.lisp:275` iterate. A node-type's name *is* its class name (`schema.lisp:221` does `(find-class (node-type-name meta))`):

```lisp
(defun all-node-classes-with-vector-index-slots (graph)
  "The node classes of GRAPH that declare at least one :VECTOR-INDEX slot."
  (loop for nt in (all-node-types graph)
        for class = (find-class (node-type-name nt) nil)
        when (and class (class-finalized-p class)
                  (node-vector-index-slots class))
          collect class))
```

Call `(restore-vector-segments graph)` in `open-graph` right after `(restore-spatial-index graph)` (`graph.lisp:317`).

In `close-graph` (`graph.lisp:371`), before the heap/indexes close, close every registered segment (each marks itself clean):

```lisp
    (maphash (lambda (k seg) (declare (ignore k)) (close-vector-segment seg))
             (vector-segments graph))
    (clrhash (vector-segments graph))
```

- [ ] **Step 5: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: both PASS.

- [ ] **Step 6: Sabotage-prove the invariant test**

Temporarily break maintenance (e.g. make the `tx-delete` hook a no-op), run `invariant-segment-matches-rebuild`, and confirm it FAILS (the live segment retains a deleted id that the rebuild does not → id-count mismatch). Restore. Report both outcomes — the invariant test is the load-bearing gate; it must be shown to fail on a real drift.

- [ ] **Step 7: Full-suite regression**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))'
```

Expected: green, with the new checks, zero failures. Report the count.

- [ ] **Step 8: Commit**

```bash
git add segment.lisp graph.lisp tests/segment-integration-tests.lisp
git commit -m "feat(segment): rebuild-from-nodes, open-path open-or-rebuild, close-graph teardown"
```

---

## Done Criteria

- [ ] `segment-integration-suite` green; `segment-suite` green; full `graph-db-suite` green with the new checks (report counts)
- [ ] A `:vector-index` slot is recognised and inherited by subclasses
- [ ] Transactional create/update/delete keep the segment in sync; clearing the value removes the entry; delete removes the entry
- [ ] A wrong-dimension embedding signals and rolls back — neither node nor segment lands it
- [ ] The segment is created lazily on first conforming insert, sized to that vector
- [ ] A clean close reopens the segment as-is; an unclean close reopens by rebuild-from-nodes
- [ ] The invariant test (segment == rebuild after an arbitrary sequence) passes AND was shown to fail under sabotaged maintenance
- [ ] No `segment-scan`/`segment-score-subset`, no cl-llm change, no `def-vector-index` macro, no new locking
