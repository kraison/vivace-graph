# Read-your-own-writes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A read inside `with-transaction` answers as the transaction will commit: its own creates, updates and deletes on top of the snapshot it already reads.

**Architecture:** Layer 1 stores a transaction's own writes in its private `local-cache`, which the transactional `lookup-object` already consults first. Layer 2 is an overlay over the write set, consulted after the committed index walk at six enumeration points under one rule: the walk skips any id the transaction wrote, the overlay supplies every own live node at its current type, endpoints or key. A `*read-own-writes*` switch turns layer 2 off underneath the commit view's evaluators so the commit path reads what it reads today.

**Tech Stack:** Common Lisp (SBCL gating; CCL/ECL/LispWorks arms compile), FiveAM, ASDF.

**Spec:** `docs/superpowers/specs/2026-10-29-read-your-own-writes-design.md` (issue kraison/vivace-graph#408). The plan argues from it; read both.

## Global Constraints

- Lisp: spaces only, 80 columns, terse comments that point at the spec (user CLAUDE.md).
- Run one suite with `tests/run-suite.sh SUITE`; never `fiveam:run!` bare. The main suite before each engine commit: `tests/run-suite.sh graph-db-suite` (SBCL needs `--dynamic-space-size 16384`; the script sets it).
- Every test below is ablated once: remove the one call it protects, run, watch it fail, restore (memory: mutation testing is the gate). Each test step names its ablation.
- Tests that compare two lookups run with `*cache-enabled*` both t and nil (`ryow-both-caches` below).
- A new special that an earlier file binds is `defvar`ed in `globals.lisp` (CLAUDE.md, engine traps).
- Branch `feat/read-your-own-writes-408` off `experiment`, one commit per task, attribution lines from the session; push only on Kevin's word.
- The peer wire, the write-set, `.txn` serialisation and replay do not change (spec §2).

## Review Focus

Inputs the spec implies that a person will hit; each has its test pinned in the owning task.

1. A node created and then `mark-deleted` in one transaction must be invisible to every entry point and leave nothing at commit (Task 4, `ryow-create-then-delete-invisible`).
2. A `save` of a copy then `mark-deleted` of the looked-up copy must archive the committed pre-image and release the committed index entry, not the copy's (Task 3, `ryow-save-then-delete-pre-image`).
3. A scan inside a transaction with a large write set must stay O(writes) per scan, not per visited node (Task 7, perf case `scan-in-tx`).
4. A commit-time validator must not count an own edge twice once scans see it (Task 5, `ryow-cardinality-counts-once`).
5. A read of an own write must add nothing to the read-set, so an unrelated concurrent commit does not force a retry (Task 7, `ryow-own-read-not-in-read-set`).

---

### Task 1: The switch, the overlay core and the commit-view bindings

**Files:**
- Modify: `globals.lisp` (after `*record-reads*`, ~line 463)
- Create: `tx-overlay.lisp`
- Modify: `graph-db.asd:118` (add `(:file "tx-overlay" :depends-on ("transactions"))`; make `"interface"` at :134 depend on it)
- Modify: `transactions.lisp:485-500` (`tx` class), `:3593-3612` (`%commit` validation region)
- Modify: `evaluator.lisp:68` (`validate-writes`), `cardinality.lisp:173` (`view-edge-count`), `spacetime/claim-query.lisp:738,752` (`%name-admitted-p`, `%name-count`)
- Modify: `package.lisp` (export `*read-own-writes*` beside `with-read-snapshot`, :282)
- Create: `tests/read-your-own-writes-tests.lisp`; register in `graph-db.asd` after `"count-index-tests"` (:741)

**Interfaces:**
- Produces, in `tx-overlay.lisp`:
  - `*read-own-writes*` (globals.lisp): special, default `t`.
  - `(defstruct (tx-overlay ...))` with slot `writes` (an id-table, id → effective tx-write).
  - `(overlay-of tx) → tx-overlay`: built lazily from `(writes tx)` (creates then write-set, later wins, the `make-commit-view` rule) and cached in a new `tx` slot `overlay` (initform nil).
  - `(%invalidate-overlay tx)`: sets the slot to nil. Called by Task 2's write entry points.
  - `(overlay-effective-write ov id) → tx-write | nil`.
  - `(overlay-live-nodes ov) → list`: the `node` of every effective write whose node is not `deleted-p`.
  - `(overlay-nodes ov kind type-ids &key include-deleted-p) → list`: effective-write nodes of KIND (`:vertex`/`:edge`, by `typep`) whose `type-id` is in TYPE-IDS; deleted ones only with INCLUDE-DELETED-P.
  - `(overlay-edges-at ov vertex-id direction type-ids &key include-deleted-p)`: edges whose `from` (`:out`) or `to` (`:in`) is `equalp` VERTEX-ID.
  - `(overlay-edges-between ov from-id to-id type-ids &key include-deleted-p)`.
  - `(current-overlay graph) → tx-overlay | nil`: `(overlay-of *transaction*)` when `*read-own-writes*`, `*transaction*` is a `tx` and `(eq (graph *transaction*) graph)`; else nil. Every enumeration point calls this once.
  - `(own-write-p tx id) → boolean`: `(overlay-effective-write (overlay-of tx) id)`.
- Test scaffold in `tests/read-your-own-writes-tests.lisp`: suite `read-your-own-writes-suite` in `graph-db-suite`; graph name `:graph-db-ryow-test`; `(def-vertex ryow-node () ((k :type string) (tag :type string :index t)) :graph-db-ryow-test)`; `(def-edge ryow-link () () :graph-db-ryow-test)`; `(def-index ryow-node (k tag) :graph-db-ryow-test)`; macro `(with-ryow-graph (g) ...)` on the `with-test-graph` shape (`tests/suite.lisp:172`) with graph name `:graph-db-ryow-test`; macro `(ryow-both-caches () ...)` that runs its body once per `*cache-enabled*` value.

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-overlay-effective-write-last-wins
  "Create then delete of one id leaves a TX-DELETE; update then update
leaves the later copy (spec §4.3)."
  (with-ryow-graph (g)
    (with-transaction ()
      (let ((v (make-ryow-node :k "a" :tag "t")))
        (mark-deleted v)
        (let ((ov (graph-db::overlay-of graph-db::*transaction*)))
          (is (typep (graph-db::overlay-effective-write ov (id v))
                     'graph-db::tx-delete))
          (is (null (graph-db::overlay-live-nodes ov))))))))

(test ryow-switch-off-under-commit-validators
  "*READ-OWN-WRITES* is NIL inside %COMMIT's validators and inside
VALIDATE-WRITES, T in the transaction body (spec §4.6)."
  (with-ryow-graph (g)
    (let ((seen :unset)
          (graph-db::*commit-validators*
            (list (lambda (tx graph) (declare (ignore tx graph))
                    (setf seen graph-db:*read-own-writes*)))))
      (with-transaction ()
        (is (eq t graph-db:*read-own-writes*))
        (make-ryow-node :k "a" :tag "t"))
      (is (null seen))
      (with-transaction ()
        (let ((graph-db::*commit-validators*
                (list (lambda (tx graph) (declare (ignore tx graph))
                        (setf seen graph-db:*read-own-writes*)))))
          (setf seen :unset)
          (make-ryow-node :k "b" :tag "t")
          (graph-db:validate-transaction g)
          (is (null seen)))))))
```

Ablations: remove the `let` binding in `%commit` (second `is` fails); remove the binding in `validate-writes` (last `is` fails).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL, `overlay-of` undefined.

- [ ] **Step 3: Implement**

`globals.lisp`: `(defvar *read-own-writes* t "...")` pointing at spec §4.6. `tx-overlay.lisp`: the struct and functions above; `tx` gains slot `overlay`. Bind `(*read-own-writes* nil)` around: the `with-transaction-manager-lock` body in `%commit` from `validate-unique-constraints` through the `*commit-validators*` loop (transactions.lisp:3593-3612); the whole body of `validate-writes`; the `map-edges` call in `view-edge-count`; the `map-index` calls in `%name-admitted-p` and `%name-count`. Add the asd entries and the export.

- [ ] **Step 4: Run to verify they pass, then the main suite**

Run: `tests/run-suite.sh read-your-own-writes-suite` then `tests/run-suite.sh graph-db-suite`
Expected: both exit 0 (nothing reads the overlay yet, so the main suite is a no-change check).

- [ ] **Step 5: Commit**

```bash
git add globals.lisp tx-overlay.lisp graph-db.asd transactions.lisp evaluator.lisp cardinality.lisp spacetime/claim-query.lisp package.lisp tests/read-your-own-writes-tests.lisp
git commit -m "tx-overlay: the switch, the overlay core and the commit-view bindings (#408)"
```

---

### Task 2: Layer 1, own writes in the local cache

**Files:**
- Modify: `transactions.lisp:2895-2900` (`%create-node` default method), `:2972-3002` (`update-node`), `:3004-3030` (`delete-node`), `:343-384` (`lookup-object` transactional method)
- Test: `tests/read-your-own-writes-tests.lisp`

**Interfaces:**
- Consumes: `%invalidate-overlay`, `own-write-p` (Task 1).
- Produces: `(%note-own-write node tx)` in `transactions.lisp`: `(setf (gethash (id node) (local-cache tx)) node)` then `(%invalidate-overlay tx)`. Called from the three write entry points, `tx` instances only (`%create-node`'s `recovery-transaction` method is untouched; guard the default method with `(typep transaction 'tx)`).

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-lookup-sees-own-create
  (with-ryow-graph (g)
    (ryow-both-caches ()
      (with-transaction ()
        (let ((v (make-ryow-node :k "a" :tag "t")))
          (is (eq v (lookup-vertex (id v))))
          (is (eq v (lookup-ryow-node (id v)))))))))

(test ryow-lookup-sees-own-save-and-delete
  (with-ryow-graph (g)
    (ryow-both-caches ()
      (let (a e)
        (with-transaction ()
          (setf a (make-ryow-node :k "a" :tag "t")
                e (make-ryow-link :from a :to a)))
        (with-transaction ()
          (let ((c (copy (lookup-vertex (id a)))))
            (setf (ryow-node-k c) "a2")
            (save c)
            (is (equal "a2" (ryow-node-k (lookup-vertex (id a))))))
          (mark-deleted (lookup-edge (id e)))
          (is (deleted-p (lookup-edge (id e))))
          (is (null (lookup-ryow-link (id e)))))))))

(test ryow-rollback-and-nesting
  "A non-local exit leaves nothing; an inner WITH-TRANSACTION (an
independent commit, #401) does not see the outer's writes."
  (with-ryow-graph (g)
    (let (id)
      (ignore-errors
       (with-transaction ()
         (setf id (id (make-ryow-node :k "x" :tag "t")))
         (error "abort")))
      (with-transaction () (is (null (lookup-vertex id))))
      (with-transaction ()
        (let ((v (make-ryow-node :k "o" :tag "t")))
          (with-transaction ()
            (is (null (lookup-vertex (id v))))))))))

(test ryow-switch-off-reads-committed
  "With *READ-OWN-WRITES* NIL a lookup passes over an own write
(spec §4.6)."
  (with-ryow-graph (g)
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (let ((c (copy (lookup-vertex (id a)))))
          (setf (ryow-node-k c) "a2") (save c)
          (let ((graph-db:*read-own-writes* nil))
            (is (equal "a" (ryow-node-k (lookup-vertex (id a)))))))))))
```

Ablations: remove the `%note-own-write` call in `%create-node` (first test), in `update-node` and `delete-node` (second); remove the switch check in `lookup-object` (fourth).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL on the four new tests, `lookup-vertex` returning NIL or the committed value.

- [ ] **Step 3: Implement**

`%note-own-write` as above, called at the end of `%create-node`'s default method, `update-node` and `delete-node`. In `lookup-object`'s transactional method, the `local` hit stands unless `(and (null *read-own-writes*) (own-write-p transaction id))`, in which case fall through to the store path **without** overwriting the cache entry.

- [ ] **Step 4: Run to verify they pass, then the main suite**

Run: `tests/run-suite.sh read-your-own-writes-suite`; `tests/run-suite.sh graph-db-suite`
Expected: exit 0 both.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/read-your-own-writes-tests.lisp
git commit -m "transactions: own writes in the local cache (#408 layer 1)"
```

---

### Task 3: The pre-image rule for copy and delete of an own write

**Files:**
- Modify: `transactions.lisp:2928-2960` (`copy-node`), `:3004-3030` (`delete-node`)
- Test: `tests/read-your-own-writes-tests.lisp`

**Interfaces:**
- Produces: `(%committed-pre-image node tx) → node`: `(old-node w)` when `w`, the write-set entry for `(id node)`, is a `tx-update` whose `node` is `eq` NODE; else NODE.

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-copy-of-own-update-chains-to-committed
  "The archived pre-image of a save-of-a-copy-of-an-own-update is the
committed version (spec §4.4)."
  (with-ryow-graph (g)
    (let (a e0)
      (with-transaction () (setf a (make-ryow-node :k "v1" :tag "t")))
      (setf e0 (graph-db::tm-current-epoch (graph-db::transaction-manager g)))
      (with-transaction ()
        (let ((c1 (copy a))) (setf (ryow-node-k c1) "v2") (save c1))
        (let ((c2 (copy (lookup-vertex (id a)))))
          (setf (ryow-node-k c2) "v3") (save c2)))
      (is (equal "v3" (ryow-node-k (lookup-vertex (id a)))))
      (is (equal "v1" (ryow-node-k (lookup-vertex (id a) :as-of e0)))))))

(test ryow-copy-of-own-create-still-refused
  (with-ryow-graph (g)
    (with-transaction ()
      (let ((v (make-ryow-node :k "a" :tag "t")))
        (signals graph-db::copying-uncommitted-node
          (copy (lookup-vertex (id v))))))))

(test ryow-save-then-delete-pre-image
  "Save then MARK-DELETED of the looked-up copy: the committed index
entry under the old key is released and the chain is intact (review
focus 2)."
  (with-ryow-graph (g)
    (let (a e0)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "old")))
      (setf e0 (graph-db::tm-current-epoch (graph-db::transaction-manager g)))
      (with-transaction ()
        (let ((c (copy a))) (setf (ryow-node-tag c) "new") (save c))
        (mark-deleted (lookup-vertex (id a))))
      (is (null (index-lookup g 'ryow-node 'tag "old")))
      (is (null (index-lookup g 'ryow-node 'tag "new")))
      (is (equal "a" (ryow-node-k (lookup-vertex (id a) :as-of e0)))))))

(test ryow-copy-of-own-delete-undeletes
  (with-ryow-graph (g)
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (mark-deleted a)
        (let ((c (copy (lookup-vertex (id a)))))
          (setf (deleted-p c) nil) (save c)))
      (is (lookup-ryow-node (id a))))))
```

Ablations: remove `%committed-pre-image` from `copy-node` (first test's `:as-of` check) and from `delete-node` (third test's `"old"` check).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL on the first and third (the second and fourth may already pass; keep them as pins).

- [ ] **Step 3: Implement**

`copy-node` registers `(setf (gethash new-node (copies *transaction*)) (%committed-pre-image node *transaction*))`; `delete-node` builds its `tx-delete` with `:old-node (%committed-pre-image node *transaction*)`. The `copying-uncommitted-node` guard in `interface.lisp` is unchanged.

- [ ] **Step 4: Run to verify they pass, then `slot-mutation-suite` and the main suite**

Run: `tests/run-suite.sh read-your-own-writes-suite`; `tests/run-suite.sh slot-mutation-suite`; `tests/run-suite.sh graph-db-suite`
Expected: exit 0 each.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/read-your-own-writes-tests.lisp
git commit -m "transactions: copy and delete of an own write chain to the committed pre-image (#408)"
```

---

### Task 4: Typed scans

**Files:**
- Modify: `vertex.lisp:275-300` (`scan-type-id` in `map-vertices`), `edge.lisp:492-560` (`emit` and the typed branch of `map-edges`)
- Test: `tests/read-your-own-writes-tests.lisp`

**Interfaces:**
- Consumes: `current-overlay`, `overlay-effective-write`, `overlay-nodes` (Task 1).
- Produces: in each scan, a local `ov` from `(current-overlay graph)`; the index walk skips an id when `(and ov (overlay-effective-write ov id))`; after the walk, `(overlay-nodes ov kind type-ids :include-deleted-p include-deleted-p)` goes through the deleted filter (`active-edge-p` for edges) and `fn`, never the `written-p` filter. Under `as-of` (`%as-of-snapshot` non-nil) `ov` is nil by construction (refused inside a read-write transaction), no extra guard.

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-typed-scan-sees-create-once-and-update-once
  (with-ryow-graph (g)
    (ryow-both-caches ()
      (let (a)
        (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
        (with-transaction ()
          (let ((c (copy a))) (setf (ryow-node-k c) "a2") (save c))
          (make-ryow-node :k "b" :tag "t")
          (let ((ks (map-vertices #'ryow-node-k g :vertex-type 'ryow-node
                                  :collect-p t)))
            (is (equal '("a2" "b") (sort ks #'string<)))))))))

(test ryow-typed-edge-scan-and-create-then-delete-invisible
  "Review focus 1: created-then-deleted appears nowhere and commits
nothing."
  (with-ryow-graph (g)
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (let ((e (make-ryow-link :from a :to a)))
          (is (= 1 (length (map-edges #'identity g :edge-type 'ryow-link
                                      :collect-p t))))
          (mark-deleted e)
          (is (= 0 (length (map-edges #'identity g :edge-type 'ryow-link
                                      :collect-p t))))
          (is (= 1 (length (map-vertices #'identity g :vertex-type 'ryow-node
                                         :collect-p t))))))
      (is (= 0 (length (map-edges #'identity g :edge-type 'ryow-link
                                  :collect-p t)))))))
```

(After commit the edge exists as a tombstone, as it does today; "leaves nothing" means nothing live.) Ablations: remove the skip in `scan-type-id` (first test sees `"a2"` twice); remove the `overlay-nodes` append (first test misses `"b"`; second misses the edge).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL, the new vertex and edge missing.

- [ ] **Step 3: Implement** per the Interfaces block, in both scans.

- [ ] **Step 4: Run to verify they pass, then `map-vertices-untyped-suite` and the main suite**

Expected: exit 0 each; the untyped scan is unchanged.

- [ ] **Step 5: Commit**

```bash
git add vertex.lisp edge.lisp tests/read-your-own-writes-tests.lisp
git commit -m "map-vertices, map-edges: typed scans see the transaction's own writes (#408)"
```

---

### Task 5: Adjacency, `edge-exists-p`, and the cardinality proof

**Files:**
- Modify: `edge.lisp:389-410` (`edge-exists-p`), `:520-545` (the `:from-vertex`/`:to-vertex` and `:vertex` branches of `map-edges`)
- Test: `tests/read-your-own-writes-tests.lisp`

**Interfaces:**
- Consumes: `overlay-edges-at`, `overlay-edges-between` (Task 1), the skip from Task 4.

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-issue-408-drop-then-make-twice
  "The issue's repro: replace-the-edge twice in one transaction leaves
one edge."
  (with-ryow-graph (g)
    (ryow-both-caches ()
      (let (a b)
        (with-transaction ()
          (setf a (make-ryow-node :k "a" :tag "t")
                b (make-ryow-node :k "b" :tag "t")))
        (with-transaction ()
          (dotimes (i 2)
            (dolist (e (map-edges #'identity g :vertex a :direction :out
                                  :edge-type 'ryow-link :collect-p t))
              (mark-deleted e))
            (make-ryow-link :from a :to b)))
        (is (= 1 (length (outgoing-edges a :edge-type 'ryow-link))))
        (is (= 1 (length (incoming-edges b :edge-type 'ryow-link))))))))

(test ryow-adjacency-in-out-between-and-exists
  (with-ryow-graph (g)
    (let (a b)
      (with-transaction ()
        (setf a (make-ryow-node :k "a" :tag "t")
              b (make-ryow-node :k "b" :tag "t")))
      (with-transaction ()
        (is (null (edge-exists-p 'ryow-link a b)))
        (let ((e (make-ryow-link :from a :to b)))
          (is (eq e (edge-exists-p 'ryow-link a b)))
          (is (= 1 (length (incoming-edges b :edge-type 'ryow-link))))
          (is (= 1 (length (map-edges #'identity g :from-vertex a
                                      :to-vertex b :edge-type 'ryow-link
                                      :collect-p t))))
          (mark-deleted e)
          (is (null (edge-exists-p 'ryow-link a b))))))))

(test ryow-cardinality-counts-once
  "Review focus 4: a :MAX 1 vertex gains one edge and commits; the
validator does not see it twice (spec §4.6)."
  (with-ryow-graph (g)
    (def-cardinality ryow-node ryow-link :graph-db-ryow-test :max 1)
    (let (a b)
      (with-transaction ()
        (setf a (make-ryow-node :k "a" :tag "t")
              b (make-ryow-node :k "b" :tag "t")))
      (finishes (with-transaction ()
                  (make-ryow-link :from a :to b)
                  (is (= 1 (length (outgoing-edges a :edge-type 'ryow-link))))
                  (is (null (graph-db::validation-report-violations
                             (graph-db:validate-transaction g))))))
      (signals graph-db::cardinality-violation
        (with-transaction () (make-ryow-link :from a :to b))))))
```

Ablations: remove `overlay-edges-at` (first two tests); remove `overlay-edges-between` in `edge-exists-p` (second); remove the `view-edge-count` binding from Task 1 (third test's `finishes` fails with a violation).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL (two edges after the repro; `edge-exists-p` NIL).

- [ ] **Step 3: Implement** per Interfaces; `edge-exists-p` returns the first of `overlay-edges-between` for its one type-id when the vev walk finds nothing.

- [ ] **Step 4: Run to verify they pass, then the suite `tests/cardinality-tests.lisp` defines, `evaluator-suite`, `traverse-suite`, and the main suite**

Expected: exit 0 each.

- [ ] **Step 5: Commit**

```bash
git add edge.lisp tests/read-your-own-writes-tests.lisp
git commit -m "map-edges, edge-exists-p: adjacency sees the transaction's own writes (#408)"
```

---

### Task 6: Secondary-index reads

**Files:**
- Modify: `index.lisp:1003-1055` (`index-lookup`, `map-index`, `index-range`); add `overlay-index-members` in `index.lisp` after `ix-map` (:511)
- Test: `tests/read-your-own-writes-tests.lisp`

**Interfaces:**
- Produces: `(overlay-index-members ov six &key key prefix start end) → list of (key . node)`: for each `overlay-live-nodes` node whose class is a subtype of the index's owner and `%tuple-indexable-p`, `k = (%index-tuple-key six node)`; keep when `key` matches exactly (or as a prefix with PREFIX), or when within `[start,end]` by `%index-value-lessp` (both open-ended when nil); sorted by `%index-comp-lessp` on `(append k (list (id node)))`.
- `index-lookup`: skip an id with an effective write; append the overlay members' nodes. `map-index`: skip likewise and **merge** overlay members into the ascending walk by `%index-comp-lessp` on the composite key; `index-range` follows through `map-index`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test ryow-index-lookup-sees-create-and-moved-key
  (with-ryow-graph (g)
    (ryow-both-caches ()
      (let (a)
        (with-transaction () (setf a (make-ryow-node :k "a" :tag "old")))
        (with-transaction ()
          (make-ryow-node :k "b" :tag "new")
          (let ((c (copy a))) (setf (ryow-node-tag c) "new") (save c))
          (is (null (index-lookup g 'ryow-node 'tag "old")))
          (is (equal '("a" "b")
                     (sort (mapcar #'ryow-node-k
                                   (index-lookup g 'ryow-node 'tag "new"))
                           #'string<)))
          (is (eq t (index-lookup g 'ryow-node 'tag "new" :collect-p nil)))
          (is (= 1 (length (index-lookup g 'ryow-node '(k tag) '("b")
                                         :prefix t)))))))))

(test ryow-map-index-order-and-range-with-own-writes
  (with-ryow-graph (g)
    (with-transaction ()
      (make-ryow-node :k "1" :tag "b") (make-ryow-node :k "2" :tag "d"))
    (with-transaction ()
      (make-ryow-node :k "3" :tag "a") (make-ryow-node :k "4" :tag "c")
      (make-ryow-node :k "5" :tag "e")
      (is (equal '("a" "b" "c" "d" "e")
                 (mapcar #'ryow-node-tag
                         (index-range g 'ryow-node 'tag :start "a" :end "e"))))
      (is (equal '("b" "c" "d")
                 (mapcar #'ryow-node-tag
                         (index-range g 'ryow-node 'tag :start "b" :end "d")))))))
```

Ablations: remove the skip in `index-lookup` (`"old"` still found); remove the overlay append (`"b"` missing); replace the merge with a plain append in `map-index` (order test fails).

- [ ] **Step 2: Run to verify they fail**

Run: `tests/run-suite.sh read-your-own-writes-suite`
Expected: FAIL.

- [ ] **Step 3: Implement** per Interfaces.

- [ ] **Step 4: Run to verify they pass, then `index-suite`, `count-index-suite`, the spacetime suite (`asdf:test-system :graph-db/spacetime-test` from an SBCL with 16 GiB) and the main suite**

Expected: exit 0 each; spacetime green proves the `%name-count` binding.

- [ ] **Step 5: Commit**

```bash
git add index.lisp tests/read-your-own-writes-tests.lisp
git commit -m "index-lookup, map-index: secondary-index reads see the transaction's own writes (#408)"
```

---

### Task 7: Cross-subsystem proof, OCC pin, perf baseline

**Files:**
- Test: `tests/read-your-own-writes-tests.lisp`; `tests/perf/benchmarks.lisp` (new `bench-tx-scan` beside `bench-edges`, :64, registered where `bench-edges` is, :982)
- Modify if the rules reference goes red: `rules/facts.lisp:315-342` (dedup `*claim-derived-this-run*` by id)
- Modify: `docs/perf-baselines.md` (one line naming the new case)

- [ ] **Step 1: Write the OCC pin**

```lisp
(test ryow-own-read-not-in-read-set
  "Review focus 5: reading an own write adds nothing to the read-set;
an unrelated concurrent commit does not force a retry."
  (with-ryow-graph (g)
    (let (a attempts)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (setf attempts 0)
      (with-transaction ()
        (incf attempts)
        (let ((c (copy a))) (setf (ryow-node-k c) "a2") (save c))
        (lookup-vertex (id a))
        (is (zerop (graph-db::object-set-count
                    (graph-db::read-set graph-db::*transaction*))))
        (when (= attempts 1)
          (bt:join-thread
           (bt:make-thread
            (lambda () (let ((*graph* g))
                         (with-transaction ()
                           (make-ryow-node :k "other" :tag "t"))))))))
      (is (= 1 attempts)))))
```

Ablation: make `%note-own-write` also `add-to-object-set` the node into the read-set; the `zerop` check fails.

- [ ] **Step 2: Run it, then the rules suite twice**

Run: `tests/run-suite.sh read-your-own-writes-suite`; then from SBCL `(asdf:test-system :graph-db/rules-test)`; `tests/rules/fixpoint-tests.lisp:140` already runs the naive-rounds reference.
Expected: green. If a naive-rounds test fails with a duplicated derivation, dedup the push site in `rules/facts.lisp` by `(id claim)` and rerun; do not bind the switch there (spec §4.6).

- [ ] **Step 3: Add the perf case**

`bench-tx-scan`: 1,000 vertices committed; one transaction that creates 1,000 edges then runs `outgoing-edges` on each of 200 vertices, timed as `"scan-in-tx"`; same shape as `bench-edges` (`with-perf-graph`, `timed-ops`). Record one run's ops/s in `docs/perf-baselines.md` under the current generation. If the scan is slower than the committed-only equivalent by more than a constant factor that grows with the write set, bucket the overlay lazily by type and endpoint inside `tx-overlay` (spec §6); the interface does not change.

- [ ] **Step 4: Run the full main suite and the acid suite**

Run: `tests/run-suite.sh graph-db-suite`; from SBCL `(asdf:test-system :graph-db/acid-test)`
Expected: exit 0 / no error.

- [ ] **Step 5: Commit**

```bash
git add tests/read-your-own-writes-tests.lisp tests/perf/benchmarks.lisp docs/perf-baselines.md rules/facts.lisp
git commit -m "tests: OCC pin, rules reference and perf case for read-your-own-writes (#408)"
```

---

### Task 8: Documentation and issues

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` Chapter 5 (:632) new section "What a transaction sees"; `docs/codebase-notes.md:18-27`; `docs/rules.md:323-331,368-373`; `CHANGELOG.md` under `## [Unreleased]` / `### Fixed`
- GitHub: comment on #401 with the new contract bullet; file three follow-up issues (search first): entry-level index reads under the overlay (`index-count`, `map-index-prefixes`), spatial and vector membership under the overlay, delete the hand-rolled overlays (`view-edge-count`'s delta, `%created-under`/`%name-count`) in favour of the engine's; add them and #408 to a board if Kevin wants one (three items, one session so far: an issue list is enough unless he says otherwise).

- [ ] **Step 1: Write the manual section**: the layer table from spec §4.1, the non-goals list from §2 with issue numbers, the copy rule from §4.4, the `*read-own-writes*` switch from §4.6, and that a nested `with-transaction` still sees nothing of the outer. Qualify "indexes update at commit" wherever the chapter says it.

- [ ] **Step 2: Update `codebase-notes.md`** (one line after the transaction idiom: a read inside the transaction sees its writes; the untyped scans and entry-level counts do not) and **`docs/rules.md`** (the two passages now state the new rule and point at the spec). Run `tests/run-suite.sh codebase-notes-suite`.

- [ ] **Step 3: CHANGELOG entry** under Fixed, one paragraph, `(#408)`, naming the switch and the non-goals.

- [ ] **Step 4: Issues**: the #401 comment; the three follow-ups with `gh issue create` after `gh issue list --search`; a `Ready for review:` comment on #408 naming the branch and `git diff experiment...feat/read-your-own-writes-408`.

- [ ] **Step 5: Commit**

```bash
git add docs/vivace-graph-v3-doc.org docs/codebase-notes.md docs/rules.md CHANGELOG.md
git commit -m "docs: what a transaction sees (#408)"
```

Then stop: Kevin reviews the branch; push and the #408 close with the landing SHA follow his word.
