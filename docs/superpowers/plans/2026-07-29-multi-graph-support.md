# Multi-Graph Support Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make running several graphs in one Lisp image a supported, enforced configuration rather than one that happens to mostly work.

**Architecture:** Nodes carry their home graph in a never-serialized `:META` slot. Read-write transactions stay strictly single-graph and signal on cross-graph access; read-only snapshots become per-graph and may compose. Class names become globally unique across schemas, and the schema registry replaces rather than accumulates.

**Tech Stack:** Common Lisp (SBCL + ECL both required green), CLOS MOP, FiveAM.

**Spec:** `docs/superpowers/specs/2026-07-29-multi-graph-support-design.md`

## Global Constraints

- **Code comments are terse.** State the invariant and cite `(GH #NN)`. Detail goes in the issue or commit message, never inline.
- **Both implementations must pass.** SBCL *and* ECL. ECL matters specifically because it does **not** pool node instances, so it exercises the opposite branch of the ownership work.
- **SBCL needs `--dynamic-space-size 8192`** (16384 for the full suite). The default heap is not enough.
- **Every test must be shown to fail without its change** — and fail on the *assertion*, not on an undefined function. Revert only the engine hunk, never the whole file.
- **`rsync -a` preserves mtimes**; `touch` changed files afterward or ASDF serves a stale fasl.
- **No on-disk format change.** Nothing in this plan alters serialized bytes.
- Existing WIP is stashed as `WIP node-graph pre-design; reconcile against spec`. It predates the design — treat it as a reference, not as correct. Do not `stash pop` blindly.

---

### Task 1: `NODE-GRAPH` ownership slot

**Files:**
- Modify: `node-class.lisp` (the `node` defclass, ~line 358)
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Produces: slot `graph` with `:accessor node-graph`; `(node-home-graph node &optional default)` returning the node's graph or `default` (default `*graph*`).

- [ ] **Step 1: Write the failing test**

In `tests/multi-graph-tests.lisp`:

```lisp
(test nodes-record-their-home-graph
  "Node buffers are pooled and reused on SBCL/CCL/LispWorks, so an unstamped
node inherits the previous occupant's graph (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "in-a")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "in-b")))))
      (let ((*graph* gc))
        (is (eq ga (graph-db::node-graph (lookup-vertex a-id :graph ga)))
            "home graph must come from the lookup, not *GRAPH*")
        (is (eq gb (graph-db::node-graph (lookup-vertex b-id :graph gb)))
            "home graph must come from the lookup, not *GRAPH*")))))
```

- [ ] **Step 2: Run it and confirm it fails**

```bash
ssh ma 'cd ~/vg-repo && VG_ROOT=$HOME/vg-repo SUITE=multi-graph-suite sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load ~/run1.lisp 2>&1 | grep -E "NODES-RECORD|Fail:"'
```
Expected: FAIL — `NODE-GRAPH` undefined.

- [ ] **Step 3: Add the slot and accessor**

In `node-class.lisp`, append to the `node` defclass slot list (after `bytes`):

```lisp
   ;; Home graph. :META so it is a real CLOS slot, :PERSISTENT NIL so it is never
   ;; serialized. NIL = unknown -> callers fall back to *GRAPH* (GH #53).
   (graph :accessor node-graph :initform nil :initarg :graph
          :meta t :persistent nil))
```

Immediately after the defclass:

```lisp
(defun node-home-graph (node &optional (default *graph*))
  "NODE's graph, or DEFAULT when unknown. Use instead of a bare *GRAPH* when
resolving a node's heap, tables or schema (GH #53)."
  (or (node-graph node) default))
```

- [ ] **Step 4: Stamp every materialization path**

`primitive-node.lisp`, in `finalize-node` after `(setf (written-p node) t)`:
```lisp
  (setf (node-graph node) graph)
```

`primitive-node.lisp`, in `ensure-node-bytes` immediately inside `(when (node-p node)`:
```lisp
    (setf (node-graph node) graph)
```

`primitive-node.lisp`, in `lookup-node` after `(setf (id node) key)`:
```lisp
          (setf (node-graph node) graph)
```

`transactions.lisp`, in `copy-node` after the `make-instance` form binds `new-node`, before `(setf (data new-node) ...)`:
```lisp
      (setf (node-graph new-node) (node-graph node))
```

- [ ] **Step 5: Run the test and the full suite**

```bash
ssh ma 'cd ~/vg-repo && VG_ROOT=$HOME/vg-repo SUITE=multi-graph-suite sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load ~/run1.lisp 2>&1 | grep -E "Did |Pass:|Fail:"'
```
Expected: PASS, 0 failures.

- [ ] **Step 6: Commit**

```bash
git add node-class.lisp primitive-node.lisp transactions.lisp tests/multi-graph-tests.lisp
git commit -m "feat(node): carry the home graph on every node

Nodes record their graph in a :META, never-serialized slot so a node's heap
is never resolved through a foreign *GRAPH*. Stamped on every materialization
path because node buffers are pooled and reused.

Refs #53"
```

---

### Task 2: Cross-graph read tests (the behaviour Task 1 enables)

**Files:**
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Consumes: `node-graph`, `node-home-graph` from Task 1.

- [ ] **Step 1: Write the tests**

```lisp
(test slot-reads-resolve-through-the-nodes-own-graph
  "Read a node's slots with *GRAPH* bound to a different graph (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "alpha-value")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "beta-value")))))
      (let ((*graph* gb))
        (is (string= "alpha-value"
                     (slot-value (lookup-vertex a-id :graph ga) 'label))))
      (let ((*graph* ga))
        (is (string= "beta-value"
                     (slot-value (lookup-vertex b-id :graph gb) 'label))))
      (let ((*graph* gc))
        (is (string= "alpha-value"
                     (slot-value (lookup-vertex a-id :graph ga) 'label)))
        (is (string= "beta-value"
                     (slot-value (lookup-vertex b-id :graph gb) 'label)))))))

(test node-to-alist-resolves-through-the-nodes-own-graph
  "NODE-TO-ALIST omits :GRAPH and fell back to *GRAPH* (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "alist-alpha")))))
      (let ((*graph* gb))
        (is (string= "alist-alpha"
                     (cdr (assoc :label
                                 (graph-db::node-to-alist
                                  (lookup-vertex a-id :graph ga)))))))
      gc)))

(test copies-carry-the-home-graph
  "COPY-NODE enumerates the slots it copies (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "orig")))))
      (let ((node (let ((*graph* gb)) (lookup-vertex a-id :graph ga))))
        (let ((*graph* ga))
          (with-transaction ()
            (let ((copy (copy node)))
              (is (eq ga (graph-db::node-graph copy)))
              (is (string= "orig" (slot-value copy 'label)))))))
      gc)))
```

Note the lookups sit **outside** the transaction: reading another graph from inside a read-write transaction becomes an error in Task 4.

- [ ] **Step 2: Run them**

```bash
ssh ma 'cd ~/vg-repo && VG_ROOT=$HOME/vg-repo SUITE=multi-graph-suite sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load ~/run1.lisp 2>&1 | grep -E "Did |Pass:|Fail:"'
```
Expected: PASS.

- [ ] **Step 3: Confirm they fail without Task 1**

Do **not** use `git stash` — a pre-design stash already exists and popping the wrong
entry would be destructive. Back the files up, remove only the stamping lines, retest,
restore:

```bash
cd ~/vg-repo
cp primitive-node.lisp /tmp/pn.bak && cp transactions.lisp /tmp/tx.bak
# delete the four `(setf (node-graph ...))` stamping lines added in Task 1 step 4,
# leaving the slot and NODE-HOME-GRAPH in place so the failure is the ASSERTION and
# not an undefined function
python3 - <<'EOF'
for f in ("primitive-node.lisp", "transactions.lisp"):
    s = open(f).read()
    s = "\n".join(l for l in s.split("\n") if "(setf (node-graph" not in l)
    open(f, "w").write(s)
EOF
touch primitive-node.lisp transactions.lisp
# rerun the suite; expect the assertions to fail, not an error
cp /tmp/pn.bak primitive-node.lisp && cp /tmp/tx.bak transactions.lisp
touch primitive-node.lisp transactions.lisp
```

- [ ] **Step 4: Commit**

```bash
git add tests/multi-graph-tests.lisp
git commit -m "test(multi-graph): cross-graph reads under a foreign *GRAPH*

Refs #53"
```

---

### Task 3: `CROSS-GRAPH-TRANSACTION-ERROR` condition

**Files:**
- Modify: `conditions.lisp`
- Modify: `package.lisp` (export)

**Interfaces:**
- Produces: condition `cross-graph-transaction-error` with initargs `:node`, `:transaction-graph`, `:node-graph`.

- [ ] **Step 1: Define the condition**

In `conditions.lisp`, following the existing `transaction-error` shape:

```lisp
(define-condition cross-graph-transaction-error (error)
  ((node :initarg :node)
   (transaction-graph :initarg :transaction-graph)
   (node-graph :initarg :node-graph))
  (:report (lambda (error stream)
             (with-slots (node transaction-graph node-graph) error
               (format stream "Cross-graph access in a read-write transaction: ~
node ~A belongs to ~A but the transaction is on ~A. A read-write transaction ~
is single-graph; use one transaction per graph."
                       node (and node-graph (graph-name node-graph))
                       (and transaction-graph (graph-name transaction-graph)))))))
```

- [ ] **Step 2: Export it**

In `package.lisp`, beside the other condition exports:
```lisp
           #:cross-graph-transaction-error
```

- [ ] **Step 3: Verify it loads and reports**

Write `/tmp/cond-check.lisp`:
```lisp
(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (pathname (concatenate 'string (uiop:getenv "VG_ROOT") "/")) asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (ql:quickload :graph-db :silent t))
(format t "~&REPORT: ~a~%"
        (princ-to-string (make-condition 'graph-db:cross-graph-transaction-error
                                         :node :n :transaction-graph nil :node-graph nil)))
(uiop:quit 0)
```
Run:
```bash
scp /tmp/cond-check.lisp ma:~/ && ssh ma 'VG_ROOT=$HOME/vg-repo sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load ~/cond-check.lisp 2>&1 | grep REPORT'
```
Expected: one `REPORT:` line containing a readable sentence, no error.

- [ ] **Step 4: Commit**

```bash
git add conditions.lisp package.lisp
git commit -m "feat(conditions): add cross-graph-transaction-error

Refs #53"
```

---

### Task 4: Enforce single-graph read-write transactions

**Files:**
- Modify: `transactions.lisp` — `lookup-object` `(transaction t)` method (~line 215)
- Modify: `transactions.lisp` — `update-node` (~line 2519)
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Consumes: `node-home-graph` (Task 1), `cross-graph-transaction-error` (Task 3).

- [ ] **Step 1: Write the failing tests**

```lisp
(test read-write-transaction-rejects-a-foreign-read
  "A read-write transaction is single-graph; reading another graph signals
rather than silently returning NIL (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "in-a")))))
      (signals graph-db:cross-graph-transaction-error
        (let ((*graph* gb))
          (with-transaction () (lookup-vertex a-id :graph ga))))
      (is (not (null (let ((*graph* ga))
                       (with-transaction () (lookup-vertex a-id :graph ga)))))
          "a same-graph transactional read is unaffected")
      gc)))

(test read-write-transaction-rejects-a-foreign-write
  "Saving a node whose home is another graph signals (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "orig")))))
      (let ((node (let ((*graph* gb)) (lookup-vertex a-id :graph ga))))
        (signals graph-db:cross-graph-transaction-error
          (let ((*graph* gb))
            (with-transaction ()
              (let ((copy (copy node)))
                (setf (slot-value copy 'label) "nope")
                (save copy :graph gb))))))
      gc)))
```

- [ ] **Step 2: Run and confirm they fail**

Expected: the read test fails because the read returns `NIL` instead of signalling.

- [ ] **Step 3: Enforce on the read path**

In `transactions.lisp`, at the top of the `(:method (id table transaction (graph t))` body of `lookup-object`, before touching the caches:

```lisp
    (let ((txn-graph (graph transaction)))
      (unless (eq graph txn-graph)
        (error 'cross-graph-transaction-error
               :node id :transaction-graph txn-graph :node-graph graph)))
```

- [ ] **Step 4: Enforce on the write path**

In `update-node`'s default method, after the `modifying-non-copy` check:

```lisp
    (let ((home (node-graph new-node))
          (txn-graph (graph *transaction*)))
      (when (and home (not (eq home txn-graph)))
        (error 'cross-graph-transaction-error
               :node new-node :transaction-graph txn-graph :node-graph home)))
```

- [ ] **Step 5: Run the tests, then acid + concurrency**

```bash
ssh ma 'cd ~/vg-repo && for S in "graph-db/acid-test run-acid-tests" "graph-db/concurrency-test run-concurrency-tests"; do set -- $S; VG_ROOT=$HOME/vg-repo SYS=$1 FN=$2 sbcl --dynamic-space-size 16384 --noinform --disable-debugger --load ~/run-suite-system.lisp 2>&1 | grep -E "Did |Fail:|=== "; done'
```
Expected: acid 31/31, concurrency 1668/1668.

- [ ] **Step 6: Commit**

```bash
git add transactions.lisp tests/multi-graph-tests.lisp
git commit -m "feat(transactions): read-write transactions are single-graph

Touching a node whose home is another graph signals instead of silently
returning NIL. Atomicity and durability are defined per graph.

Refs #53"
```

---

### Task 5: Per-graph read snapshots

**Files:**
- Modify: `transactions.lisp` — `call-with-read-snapshot` (~line 2795)
- Modify: `transactions.lisp` — `lookup-object` `(transaction null)` method
- Modify: `package.lisp` (export `*read-snapshots*`)
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Produces: `*read-snapshots*`, an `eq` hash graph → snapshot transaction, empty by default.
- Consumes: `cross-graph-transaction-error` (Task 3).

- [ ] **Step 1: Write the failing test**

```lisp
(test read-only-snapshots-compose-across-graphs
  "A read-only snapshot may span graphs; each graph is internally consistent,
with no single instant across graphs (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id b-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "a1")))))
      (let ((*graph* gb))
        (with-transaction () (setq b-id (id (make-mg-text :label "b1")))))
      (graph-db:with-read-snapshot (ga)
        (graph-db:with-read-snapshot (gb)
          (is (string= "a1" (slot-value (lookup-vertex a-id :graph ga) 'label)))
          (is (string= "b1" (slot-value (lookup-vertex b-id :graph gb) 'label)))))
      gc)))
```

- [ ] **Step 2: Run and confirm it fails**

Expected: FAIL — the inner snapshot inherits `*transaction*` on `ga`, so the `gb` read hits Task 4's error.

- [ ] **Step 3: Add the registry**

In `transactions.lisp`, near `*transaction*`:

```lisp
(defvar *read-snapshots* nil
  "Graph -> read-only snapshot transaction, or NIL. Read-only snapshots are
per graph and may compose; read-write transactions are not (GH #53).")
```

- [ ] **Step 4: Rewrite `call-with-read-snapshot`**

Replace its body with:

```lisp
  (let ((tm (and graph
                 (slot-boundp graph 'transaction-manager)
                 (transaction-manager graph))))
    (cond
      ((null tm) (funcall thunk))
      ;; a read-write transaction on this graph already provides a snapshot
      ((and *transaction* (eq (graph *transaction*) graph)) (funcall thunk))
      ;; already snapshotted this graph -> inherit
      ((and *read-snapshots* (gethash graph *read-snapshots*)) (funcall thunk))
      (t
       (let ((txn (create-transaction tm))
             (table (or *read-snapshots* (make-hash-table :test 'eq))))
         (unwind-protect
              (let ((*read-snapshots* table))
                (setf (gethash graph table) txn)
                (funcall thunk))
           (remhash graph table)
           (remove-transaction txn tm)))))))
```

- [ ] **Step 5: Consult the registry on the non-transactional read path**

In `lookup-object`'s `(transaction null)` method, before the existing body:

```lisp
    (let ((snap (and *read-snapshots* (gethash graph *read-snapshots*))))
      (when snap
        (return-from lookup-object (lookup-object id table snap graph))))
```

`DEFMETHOD` bodies are already implicit blocks named for the generic function, so
`(return-from lookup-object ...)` works as written — no restructuring needed. Verify by
compiling; if the compiler objects, wrap the body in an explicit
`(block lookup-object ...)` instead.

- [ ] **Step 6: Run the test, then the full suite on both lisps**

Expected: PASS; SBCL and ECL full suites green.

- [ ] **Step 7: Commit**

```bash
git add transactions.lisp package.lisp tests/multi-graph-tests.lisp
git commit -m "feat(transactions): per-graph read snapshots that compose

Read-only snapshots move from the single *TRANSACTION* binding to a per-graph
registry, so a cross-graph query snapshots each participating graph. No single
instant across graphs, by design.

Refs #53"
```

---

### Task 6: Globally unique class names

**Files:**
- Modify: `schema.lisp` — the shared `def-vertex`/`def-edge` expansion (~line 255, before the emitted `defclass`)
- Modify: `conditions.lisp`, `package.lisp`
- Test: `tests/node-class-tests.lisp`

**Interfaces:**
- Produces: condition `duplicate-node-class-error`; `(%check-node-class-graph-unique name graph-name)`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test duplicate-class-name-across-graphs-errors
  "One CL class namespace, per-graph schemas: a second graph reusing a name
silently clobbered the first class's slots (GH #53)."
  (eval '(def-vertex dupchk-thing () ((alpha :type string)) :dupchk-one))
  (signals graph-db:duplicate-node-class-error
    (eval '(def-vertex dupchk-thing () ((beta)) :dupchk-two)))
  (is (member 'alpha (mapcar #'graph-db::slot-definition-name
                             (graph-db::class-slots (find-class 'dupchk-thing))))
      "the original class must be untouched — the guard runs before DEFCLASS"))

(test same-graph-redefinition-still-allowed
  "Runtime schema evolution must keep working; the check is on graph-name
identity, not on presence (GH #53)."
  (eval '(def-vertex samechk-thing () ((alpha :type string)) :samechk-one))
  (finishes (eval '(def-vertex samechk-thing () ((alpha :type string) (beta))
                    :samechk-one)))
  (is (member 'beta (mapcar #'graph-db::slot-definition-name
                            (graph-db::class-slots (find-class 'samechk-thing))))))
```

- [ ] **Step 2: Run and confirm the first fails**

Expected: no error signalled; `alpha` gone.

- [ ] **Step 3: Add the condition**

`conditions.lisp`:
```lisp
(define-condition duplicate-node-class-error (error)
  ((name :initarg :name) (existing-graph :initarg :existing-graph)
   (new-graph :initarg :new-graph))
  (:report (lambda (error stream)
             (with-slots (name existing-graph new-graph) error
               (format stream "Node class ~A is already defined for graph ~A; ~
cannot redefine it for ~A. Class names are global; remove the old definition ~
to re-home it." name existing-graph new-graph)))))
```
Export it in `package.lisp`.

- [ ] **Step 4: Add the guard**

`schema.lisp`, above the macro expansion:

```lisp
(defun %check-node-class-graph-unique (name graph-name)
  "Signal if NAME is registered under a graph other than GRAPH-NAME. Keys on
graph-name identity, not presence: a same-graph redefinition legitimately adds
a second entry under the same key (GH #53)."
  (maphash (lambda (gname metas)
             (unless (eq gname graph-name)
               (when (find name metas :key #'node-type-name)
                 (error 'duplicate-node-class-error
                        :name name :existing-graph gname :new-graph graph-name))))
           *schema-node-metadata*))
```

Emit the call as the **first** form of the expansion, before `(defclass ,name ...)`:
```lisp
         (%check-node-class-graph-unique ',name ',graph-name)
```

- [ ] **Step 5: Run the tests and the full suite (both lisps)**

Expected: both new tests pass; no existing test regresses. The scan found no real
cross-graph name reuse in the repo — only a docstring example and `xach-test.lisp`,
which is not in the ASD.

- [ ] **Step 6: Commit**

```bash
git add schema.lisp conditions.lisp package.lisp tests/node-class-tests.lisp
git commit -m "feat(schema): class names are globally unique across graphs

A second graph reusing a class name silently replaced the first class's slots,
leaving that graph's stored data unreachable. Now an error, raised before the
DEFCLASS that would clobber it.

Refs #53"
```

---

### Task 7: Schema registry replaces instead of accumulating

**Files:**
- Modify: `schema.lisp` (~line 423, the `push` into `*schema-node-metadata*`)
- Test: `tests/node-class-tests.lisp`

**Interfaces:**
- Consumes: nothing new.

- [ ] **Step 1: Write the failing test**

```lisp
(test redefinition-replaces-its-registry-entry
  "UPDATE-SCHEMA replays every meta in the list on graph open, so accumulating
duplicates costs an instantiation per historical version, forever (GH #53)."
  (eval '(def-vertex regchk-thing () ((alpha :type string)) :regchk-one))
  (eval '(def-vertex regchk-thing () ((alpha :type string) (beta)) :regchk-one))
  (eval '(def-vertex regchk-thing () ((alpha :type string) (beta) (gamma))
          :regchk-one))
  (is (= 1 (count 'regchk-thing
                  (gethash :regchk-one graph-db::*schema-node-metadata*)
                  :key #'graph-db::node-type-name))
      "three definitions must leave exactly one registry entry"))
```

- [ ] **Step 2: Run and confirm it fails**

Expected: FAIL, count is 3.

- [ ] **Step 3: Replace in place**

In `schema.lisp`, swap the `push` for:

```lisp
           ;; Replace in place, preserving position: UPDATE-SCHEMA applies the
           ;; list oldest-to-newest and INSTANTIATE-NODE-TYPE assigns type-ids in
           ;; that order, so moving a redefined type would change its type-id on
           ;; a fresh graph (GH #53).
           (let* ((metas (gethash ',graph-name *schema-node-metadata*))
                  (pos (position ',name metas :key #'node-type-name)))
             (if pos
                 (setf (nth pos metas) ,meta)
                 (setf (gethash ',graph-name *schema-node-metadata*)
                       (append metas (list ,meta)))))
```

This makes the list **oldest-first**, so `UPDATE-SCHEMA`'s `reverse` must go in the same
commit or the replay order silently inverts. In `schema.lisp`, `update-schema`:

```lisp
(defmethod update-schema ((graph graph))
  (with-recursive-lock-held ((schema-lock (schema graph)))
    (let ((node-metadata (gethash (graph-name graph) *schema-node-metadata*)))
      ;; The list is maintained oldest-first (GH #53); apply in order.
      (dolist (meta node-metadata)
        (instantiate-node-type meta graph)))
    (save-schema (schema graph) graph)))
```

Every test helper that resets the registry with
`(setf (gethash <name> *schema-node-metadata*) nil)` stays valid — NIL is still the empty
list.

- [ ] **Step 4: Verify type-id stability**

Add:
```lisp
(test redefinition-keeps-type-id-stable
  "Position in the registry determines type-id assignment order (GH #53)."
  (eval '(def-vertex tidchk-a () ((x)) :tidchk-graph))
  (eval '(def-vertex tidchk-b () ((y)) :tidchk-graph))
  (let ((before (position 'tidchk-a
                          (gethash :tidchk-graph graph-db::*schema-node-metadata*)
                          :key #'graph-db::node-type-name)))
    (eval '(def-vertex tidchk-a () ((x) (z)) :tidchk-graph))
    (is (= before (position 'tidchk-a
                            (gethash :tidchk-graph graph-db::*schema-node-metadata*)
                            :key #'graph-db::node-type-name))
        "a redefined type must keep its position")))
```

- [ ] **Step 5: Run the full suite on both lisps**

This task touches graph open. Run the **full** suite plus a reopen check, both lisps.

- [ ] **Step 6: Commit**

```bash
git add schema.lisp tests/node-class-tests.lisp
git commit -m "fix(schema): replace a redefined type's registry entry in place

DEF-VERTEX pushed a new meta per evaluation, so UPDATE-SCHEMA replayed every
historical version of every type on graph open. Replacement preserves position,
keeping type-id assignment order stable.

Refs #53"
```

---

### Task 7a: Slot access resolves through the node's own graph

**Added after Task 4 review.** Tasks 1-2 stamp `NODE-GRAPH` at 16 sites but nothing
READS it on the slot-access path, so the stamps are currently decoration. This is the
core of #53.

**Files:**
- Modify: `primitive-node.lisp` — the four `SLOT-*-USING-CLASS :AROUND` methods (~371-450)
  and `MAYBE-INIT-NODE-DATA` (~207)
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Consumes: `NODE-HOME-GRAPH` (Task 1).

- [ ] **Step 1: Write the failing test**

A node whose lazy data has not yet been materialized, read under a foreign `*GRAPH*`,
must return its own data. This is the case that currently dies on ECL inside `COPY` with
a `DESERIALIZATION-ERROR`, because `MAYBE-INIT-NODE-DATA` dereferences the foreign
`DATA-POINTER` into `*GRAPH*`'s heap.

```lisp
(test lazy-slot-reads-resolve-through-the-nodes-own-graph
  "MAYBE-INIT-NODE-DATA resolved the heap via *GRAPH*, so a node whose data was
still lazy deserialized from the wrong file (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id loc-a)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "lazy-alpha")))))
      (setq loc-a (graph-db:location ga))
      (close-graph ga :snapshot-p t)
      (setq ga (open-graph :mg-alpha loc-a))
      ;; reopened: the node's data is lazy, not yet materialized
      (let ((*graph* gb))
        (let ((node (lookup-vertex a-id :graph ga)))
          (is (string= "lazy-alpha" (slot-value node 'label))
              "a lazy slot must materialize from GA's heap, not *GRAPH*'s")))
      gc)))
```

- [ ] **Step 2: Run it and confirm it fails**

Run `multi-graph-suite` on **both** SBCL and ECL. ECL is expected to fail with a
`DESERIALIZATION-ERROR`; SBCL may pass by luck of heap layout, which is itself the
argument for the fix — a wrong-heap read that happens to decode is worse than one that
errors.

- [ ] **Step 3: Resolve through the node's own graph**

`MAYBE-INIT-NODE-DATA` already takes `:GRAPH` defaulting to `*GRAPH*`. Prefer the node's
own:

```lisp
(defun maybe-init-node-data (node &key (graph *graph*))
  ;; Resolve the heap through the NODE's own graph (GH #53).
  (let ((graph (node-home-graph node graph)))
    ...existing body unchanged...))
```

Note the existing body must be wrapped in the new `LET`; keep the trailing `node` return
value outside it exactly as now.

- [ ] **Step 4: Thread the graph through the slot-access methods**

The four `SLOT-*-USING-CLASS :AROUND` methods in `primitive-node.lisp` call
`NODE-SLOT-VALUE` / `NODE-SLOT-BOUNDP` with no `:GRAPH`. Pass the node's own:

```lisp
(node-slot-value instance slot-keyword-name :graph (node-home-graph instance))
```

and the same for the `setf` and `slot-boundp` paths. `NODE-SLOT-VALUE` and
`NODE-SLOT-BOUNDP` already accept `:GRAPH`.

- [ ] **Step 5: Run the test, then the full suite on both lisps**

This touches the hottest read path in the engine. Run the full suite plus
`graph-db/acid-test` and `graph-db/concurrency-test` on both SBCL and ECL, redirecting
each to a log file.

- [ ] **Step 6: Commit**

```bash
git add primitive-node.lisp tests/multi-graph-tests.lisp
git commit -m "fix(node): resolve a node's heap through its own graph

Refs #53"
```

---

### Task 7b: Enforce single-graph on the delete path

**Added after Task 4 review.** `DELETE-NODE` / `MARK-DELETED` bypass `UPDATE-NODE`, so
Task 4's write enforcement does not cover them and a cross-graph delete is still silent.
Spec §6 names them.

**Files:**
- Modify: `transactions.lisp` — `DELETE-NODE` (~2549)
- Test: `tests/multi-graph-tests.lisp`

**Interfaces:**
- Consumes: `NODE-HOME-GRAPH` (Task 1), `CROSS-GRAPH-TRANSACTION-ERROR` (Task 3).

- [ ] **Step 1: Write the failing test**

```lisp
(test read-write-transaction-rejects-a-foreign-delete
  "DELETE-NODE bypasses UPDATE-NODE, so it needs its own check (GH #53)."
  (with-three-graphs (ga gb gc)
    (let (a-id)
      (let ((*graph* ga))
        (with-transaction () (setq a-id (id (make-mg-plain :label "doomed")))))
      (let ((node (let ((*graph* gb)) (lookup-vertex a-id :graph ga))))
        (signals graph-db:cross-graph-transaction-error
          (let ((*graph* gb))
            (with-transaction () (mark-deleted node)))))
      ;; and it must still be there
      (is (not (null (lookup-vertex a-id :graph ga)))
          "the foreign delete must not have landed")
      gc)))
```

- [ ] **Step 2: Run it and confirm it fails**

Expected: no error signalled, and the node is deleted from GA.

- [ ] **Step 3: Add the check**

In `DELETE-NODE`'s default method, mirroring `UPDATE-NODE`:

```lisp
    ;; A read-write transaction is single-graph (GH #53).  A NIL home is
    ;; unknown, not foreign.
    (let ((home (node-home-graph node nil))
          (txn-graph (and *transaction* (graph *transaction*))))
      (when (and home txn-graph (not (eq home txn-graph)))
        (error 'cross-graph-transaction-error
               :node node :transaction-graph txn-graph :node-graph home)))
```

Guard on `*transaction*` being non-NIL: `DELETE-NODE` auto-wraps a transaction in some
paths, so it can be reached outside one.

- [ ] **Step 4: Run the test, then the full suite on both lisps**

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/multi-graph-tests.lisp
git commit -m "feat(transactions): enforce single-graph on the delete path

Refs #53"
```

---

### Task 8: Documentation and issues

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (multi-graph section)
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Document the contract**

Add a "Multiple graphs in one image" section stating: a read-write transaction is
single-graph and signals on cross-graph access; read-only snapshots are per graph
and compose, with **no single instant across graphs**; class names are global;
writing to several graphs means one transaction per graph, sequenced by the
application and **not atomic**.

- [ ] **Step 2: CHANGELOG entry under 3.0.0**

- [ ] **Step 3: File the deferred issues**

```bash
gh issue create --repo kraison/vivace-graph \
  --title "Multi-graph transactions: two-phase commit across per-graph WALs" --body ...
gh issue create --repo kraison/vivace-graph \
  --title "Global cross-graph epoch for a single-instant cross-graph query" --body ...
```

- [ ] **Step 4: Confirm whether any consumer needs atomic cross-graph writes**

```bash
grep -rn "with-transaction" /Users/kraison/work/mine-action --include=*.lisp | head -40
```
Record the answer in the spec; if a real cross-graph invariant exists, the
single-graph contract needs an explicit compensating pattern documented.

- [ ] **Step 5: Commit**

```bash
git add docs/ CHANGELOG.md
git commit -m "docs: multi-graph contract for 3.0

Refs #53"
```

---

## Self-Review

**Spec coverage:** §3 → Tasks 3–4. §4 → Task 5. §5 → Task 1. §6 → Tasks 4–5. §7 → Task 6. §8 → Task 7. §9 → verified by full suites throughout. §10 → tests in every task. §11 → Task 8 step 3. §12 risks → Task 1 step 4 (stamping points enumerated), Task 5 step 4 (`remhash` on unwind), Task 4 step 4 (`home` guarded non-NIL).

**Placeholders:** none — every code step carries its code. Task 8's issue bodies are the one deliberate exception (prose, written at the time).

**Type consistency:** `node-graph` / `node-home-graph` (Task 1) used unchanged in 2, 4, 5. `cross-graph-transaction-error` initargs `:node`/`:transaction-graph`/`:node-graph` consistent in 3 and 4. `duplicate-node-class-error` initargs consistent in 6. `*read-snapshots*` is an `eq` hash in 5 only.

**Known risk carried forward:** Task 7 step 3 changes list order (oldest-first) and requires dropping `UPDATE-SCHEMA`'s `reverse` in the same commit. That coupling is the most likely place for this plan to go wrong; verify ordering with a reopen test, not just the unit test.
