# Segment Reservation — Wave 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make vector-segment capacity exhaustion a clean, pre-durability abort instead of silent vector loss, and make rebuild/crash-recovery possible above 131,072 entries.

**Architecture:** Two independent changes. Task 1 adds a capacity check to the manager-locked, pre-`finalize-tx-persistence` validation region, mirroring the existing dimension check. Task 2 sizes a rebuilt segment to its corpus and stops a missing segment file from being a silent no-op.

**Tech Stack:** SBCL, ASDF/Quicklisp, FiveAM.

**Spec:** [`docs/superpowers/specs/2026-07-22-segment-reservation-exhaustion-design.md`](../specs/2026-07-22-segment-reservation-exhaustion-design.md) — Parts 1, 5 and 6. Parts 2, 3 and 4 are **wave 2 and out of scope here.**

## Global Constraints

- **This is the engine.** mine-action (a macOS dev hub and odm/Linux) depends on it. Do not change public behaviour beyond what the spec describes.
- **Branch is `experiment`.** Do not create branches, do not push, do not merge.
- **Baseline: 2544 checks, 100% pass, 0 fail.** Any drop is a regression, not noise.
- Run the suite with:
  ```
  cd /Users/kraison/work/vivace-graph-v3
  sbcl --dynamic-space-size 8192 --non-interactive \
    --eval '(ql:register-local-projects)' \
    --eval '(ql:quickload :graph-db/test :silent t)' \
    --eval '(funcall (read-from-string "graph-db/test::run-tests"))' 2>&1 | tail -8
  ```
  `--dynamic-space-size 8192` is required; the default heap is not enough.
- **Do not touch ECL-specific code paths or worry about ECL.** The Android field app no longer embeds vivace-graph (it uses a SQLite replication peer), so ECL has no live consumer. Do not spend effort there.
- **Spaces, never tabs.**
- `git add` only the files each task names. The tree has unrelated untracked paths (`.local/`, `tools/`, several `docs/android-*.md`, `docs/ecl-change-class-leak-report.md`) that are not yours. Never `git add -A` or `git add .`.
- **Search boundary:** work only within `/Users/kraison/work/vivace-graph-v3`. Do not run `find`/`grep` from `/` or `~`.

## Reference — verified symbols you will need

| Symbol | Where | What |
| --- | --- | --- |
| `%segment-key` | transactions.lisp | `(owner-name . slot-name)` cons for a node+slot |
| `%node-segment-value` | transactions.lisp | the conforming vector, or NIL |
| `node-vector-index-slots` | transactions.lisp | the `:vector-index` slots of a class |
| `vector-segments` | graph.lisp / graph-class | hash on the graph, keyed by `%segment-key` |
| `%seg-slot-of` | segment.lisp | slot index for an id, or NIL — use to tell new from existing |
| `segment-live-count` / `segment-capacity` | segment.lisp:88,91 | header reads |
| `segment-dimension` | segment.lisp | struct slot |
| `%seg-file-bytes` | segment.lisp | `64 + capacity × (16 + dimension×4)` |
| `segment-mmap` / `m-reserved-size` | segment.lisp / mmap.lisp | the reservation |
| `create-vector-segment` | segment.lisp:61 | `&key (initial-capacity 1024)` |

---

### Task 1: Pre-durability capacity validation (spec Parts 1 and 5)

**Files:**
- Modify: `conditions.lisp` (new condition), `transactions.lisp` (the check + its call site), `mmap.lisp` (Part 5 message fix)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Produces: `vector-segment-capacity-exhausted` (a subtype of `error`) and `validate-vector-segment-capacity (tx graph)`. Task 2 does not consume either.

- [ ] **Step 1: Write the failing test**

Append to `tests/segment-integration-tests.lisp`. Model it on `wrong-dimension-signals-and-rolls-back` (line 141) — that test already asserts the exact negative this one needs.

The test must force exhaustion deliberately: bind `graph-db::*mmap-min-reservation*` and `graph-db::*mmap-reservation-multiplier*` low **before** creating the graph, so the segment's reservation is small enough to exhaust with a handful of vectors.

```lisp
(test capacity-exhaustion-signals-and-rolls-back
  (with-temp-directory (dir)
    ;; A deliberately tiny reservation, so a few vectors exhaust it.  Bound
    ;; BEFORE make-graph: the reservation is fixed when the mapping is created.
    (let* ((graph-db::*mmap-min-reservation* (* 64 1024))
           (graph-db::*mmap-reservation-multiplier* 1)
           (g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
           (count-before nil)
           (live-before nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-doc :title "seed" :embedding (%si-embedding 8 1.0))))
             (setf count-before (length (map-vertices #'identity g
                                                      :collect-p t :vertex-type 'si-doc))
                   live-before (graph-db::segment-live-count (%si-segment g 'embedding)))
             ;; Keep inserting until the segment must grow past its reservation.
             ;; The FIRST transaction that would exceed it must signal, and must
             ;; signal BEFORE anything is journaled.
             (signals graph-db::vector-segment-capacity-exhausted
               (let ((*graph* g))
                 (dotimes (i 100000)
                   (with-transaction ()
                     (make-si-doc :title "fill" :embedding (%si-embedding 8 (float i 1.0))))))))
        (close-graph g :snapshot-p nil))
      ;; THE POINT OF THIS TEST: the aborted transaction must not have persisted
      ;; its node.  A test that only asserts "an error was signalled" passes
      ;; against the broken behaviour this change exists to fix.
      (let ((g2 (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((nodes (length (map-vertices #'identity g2 :collect-p t :vertex-type 'si-doc)))
                   (live (graph-db::segment-live-count (%si-segment g2 'embedding))))
               (is (= nodes live)
                   "every persisted si-doc must have a segment entry: ~D nodes vs ~D live"
                   nodes live)
               (is (> nodes count-before) "the fill loop should have committed something")
               (is (> live live-before)))
          (close-graph g2 :snapshot-p nil)))
      (collect-garbage))))
```

The `nodes = live` assertion is the whole invariant. Against current code it fails, because the node is journaled before the segment write errors.

- [ ] **Step 2: Run it and confirm it fails for the right reason**

```bash
cd /Users/kraison/work/vivace-graph-v3
sbcl --dynamic-space-size 8192 --non-interactive \
  --eval '(ql:register-local-projects)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(funcall (read-from-string "graph-db/test::run-tests"))' 2>&1 | tail -12
```

Expected: this test fails. It will first fail because `vector-segment-capacity-exhausted` is undefined — that is fine and expected; after Step 3 defines it, re-run and confirm it then fails on the `nodes = live` assertion (or on the wrong condition type), which is the real defect. **Record both failure modes in your report** — proving the test discriminates matters more than making it green.

- [ ] **Step 3: Define the condition**

In `conditions.lisp`, following the existing `define-condition` idiom in that file:

```lisp
(define-condition vector-segment-capacity-exhausted (error)
  ((owner :initarg :owner :reader vsce-owner)
   (slot :initarg :slot :reader vsce-slot)
   (required :initarg :required :reader vsce-required)
   (reserved :initarg :reserved :reader vsce-reserved)
   (needed-bytes :initarg :needed-bytes :reader vsce-needed-bytes))
  (:report (lambda (c s)
             (format s "vector segment ~A/~A: growing to hold ~D entries needs ~D bytes, ~
but its mmap reservation is ~D. Reopen the graph (the reservation is recomputed ~
from the file's current size, which grants roughly 8x more headroom), or raise ~
GRAPH-DB::*MMAP-RESERVATION-MULTIPLIER* / GRAPH-DB::*MMAP-MIN-RESERVATION* before opening."
                     (vsce-owner c) (vsce-slot c) (vsce-required c)
                     (vsce-needed-bytes c) (vsce-reserved c)))))
```

The report text is deliberately actionable — it names the real remedy (restart) and the real variables, which is what spec Part 5 is about.

- [ ] **Step 4: Implement the check**

In `transactions.lisp`, directly after `validate-vector-segment-dimensions` (which ends around line 1010), add:

```lisp
(defun validate-vector-segment-capacity (tx graph)
  "Signal VECTOR-SEGMENT-CAPACITY-EXHAUSTED if applying TX would require growing
a vector segment past its mmap reservation.  Runs in the same manager-locked,
pre-FINALIZE-TX-PERSISTENCE region as VALIDATE-VECTOR-SEGMENT-DIMENSIONS and for
the same reason: %SEG-GROW signals from inside APPLY-TRANSACTION, by which point
the node write is already journaled, leaving a persisted node with no segment
entry -- invisible to VECTOR-SEARCH, with no error and no self-correction.

Conservative by design: it ignores the free list, so a transaction that would in
fact reuse freed slots may abort slightly early.  Aborting early is recoverable;
aborting after durability is not.

A (owner, slot) with no committed segment cannot exhaust -- creation sizes the
file -- so it is skipped."
  (let ((new-ids (make-hash-table :test 'equal)))
    ;; Count DISTINCT new ids per segment key: an id already in the segment
    ;; reuses its slot, and the same id written twice in one transaction still
    ;; claims only one.
    (dolist (write (writes tx))
      (let ((node (node write)))
        (unless (deleted-p node)
          (dolist (slot (node-vector-index-slots (class-of node)))
            (let ((v (%node-segment-value node slot)))
              (when v
                (let* ((key (%segment-key node slot))
                       (seg (gethash key (vector-segments graph))))
                  (when (and seg (null (%seg-slot-of seg (id node))))
                    (pushnew (id node) (gethash key new-ids) :test #'equalp)))))))))
    (maphash
     (lambda (key ids)
       (let* ((seg (gethash key (vector-segments graph)))
              (required (+ (segment-live-count seg) (length ids)))
              (cap (segment-capacity seg)))
         (loop while (< cap required) do (setf cap (* 2 cap)))
         (let ((needed (%seg-file-bytes cap (segment-dimension seg)))
               (reserved (m-reserved-size (segment-mmap seg))))
           (when (> needed reserved)
             (error 'vector-segment-capacity-exhausted
                    :owner (car key) :slot (cdr key)
                    :required required :needed-bytes needed :reserved reserved)))))
     new-ids)))
```

Then call it at the existing validation site (`transactions.lisp:2352`), immediately after the dimension check:

```lisp
               (validate-vector-segment-dimensions tx (graph tx))
               (validate-vector-segment-capacity tx (graph tx))
```

Note on locking: this reads `segment-live-count` / `segment-capacity` without the segment's rw-lock, matching the "lock at public boundaries only" rule — it runs under the manager lock, and `apply-transaction` (the only mutator) runs under it too, so no concurrent mutation is possible.

- [ ] **Step 5: Fix the misleading error text (spec Part 5)**

`mmap.lisp:256` tells the operator to raise `*mmap-reservation-size*`, which **does not exist**. The stale name also appears in the `mapped-file` docstring at `mmap.lisp:15`. Correct both to name the real variables (`*mmap-reservation-multiplier*`, `*mmap-min-reservation*`) and to mention that reopening recomputes the reservation from the file's current size.

- [ ] **Step 6: Run the full suite**

Same command as Step 2. Expected: **2545+ checks, 0 failures** (baseline 2544 plus your new assertions). A drop below 2544 passing is a regression — investigate, do not proceed.

- [ ] **Step 7: Commit**

Print the full diff in a fenced `## 📋 DIFF FOR REVIEW` block first, then:

```bash
cd /Users/kraison/work/vivace-graph-v3
git add conditions.lisp transactions.lisp mmap.lisp tests/segment-integration-tests.lisp
git commit -m "fix(segment): validate capacity before durability, not during apply

%seg-grow signalled from inside apply-transaction, after
finalize-tx-persistence, so the node write was already durable when the
segment write failed -- leaving a persisted node with no segment entry,
invisible to vector-search forever, with no error and store-count still
counting it.

Checked now in the same manager-locked pre-durability region as the unique
and dimension checks, so the whole transaction rolls back cleanly instead.
Conservative: ignores the free list, so it may abort slightly early.

Also fixes the exhaustion message, which told the operator to raise
*mmap-reservation-size* -- a variable that does not exist."
```

---

### Task 2: Survivable rebuild and recovery (spec Part 6)

**Files:**
- Modify: `segment.lisp` (`rebuild-vector-segment`), `graph.lisp` (`restore-vector-segments`)
- Test: `tests/segment-integration-tests.lisp`

**Interfaces:**
- Consumes nothing from Task 1. `rebuild-vector-segment`'s signature is unchanged; only its internal sizing changes.

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-integration-tests.lisp`:

```lisp
(test rebuild-sizes-the-segment-to-the-corpus
  ;; rebuild-vector-segment created at the 1024 default, so a fresh file was
  ;; ~4 MB, its reservation fell to the 1 GiB floor, and doubling stalled at
  ;; 131,072 entries -- meaning automatic crash recovery (restore-vector-segments
  ;; rebuilds whenever the clean flag is unset) could not complete above that.
  ;; Testing the real 131k threshold would be far too slow, so this asserts the
  ;; mechanism instead: a rebuild must not start at the 1024 default when the
  ;; corpus is larger than that.
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (dotimes (i 2000)
                 (with-transaction ()
                   (make-si-doc :title "n" :embedding (%si-embedding 8 (float i 1.0))))))
             (let ((seg (graph-db::rebuild-vector-segment g 'si-doc 'embedding)))
               (is (= 2000 (graph-db::segment-live-count seg)))
               (is (>= (graph-db::segment-capacity seg) 2000)
                   "a rebuild must size capacity to the corpus, not the 1024 default")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test missing-segment-file-is-rebuilt-not-ignored
  ;; restore-vector-segments guarded everything with (when (probe-file path) ...),
  ;; so a missing segment file meant the graph opened clean with a permanently
  ;; empty vector index and no diagnostic at all.
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
          (path nil))
      (let ((*graph* g))
        (dotimes (i 25)
          (with-transaction ()
            (make-si-doc :title "n" :embedding (%si-embedding 8 (float i 1.0))))))
      (setf path (graph-db::%segment-file g 'si-doc 'embedding))
      (close-graph g :snapshot-p t)
      (delete-file path)
      (let ((g2 (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((seg (%si-segment g2 'embedding)))
               (is (not (null seg)) "a missing segment file must not leave the index absent")
               (is (= 25 (graph-db::segment-live-count seg))
                   "the segment must be rebuilt from the vertices, which are authoritative"))
          (close-graph g2 :snapshot-p nil)))
      (collect-garbage))))
```

- [ ] **Step 2: Run and confirm both fail**

Same command as Task 1 Step 2. Expected: both new tests fail — the first on capacity being 1024-derived, the second on a NIL segment. Record the exact failure text.

- [ ] **Step 3: Size the rebuild to its corpus**

In `rebuild-vector-segment` (`segment.lisp:423`), the current call is
`(setf seg (create-vector-segment path (length v)))` — dimension only, so
`initial-capacity` takes its 1024 default.

Add a counting pre-pass over the same nodes the rebuild already sweeps, capturing both the dimension (from the first conforming vector) and the count of conforming nodes, then create with `:initial-capacity` at least that count. A pre-pass doubles a sweep that is already O(corpus) and quiescent, and it removes roughly eight doubling-and-relocate passes from every rebuild, so it is a net win as well as a correctness fix.

Keep the existing lazy-create behaviour when there are no conforming vectors (no segment should be created at all in that case). Do not change the function's signature or its documented contract about owners and subclasses.

- [ ] **Step 4: Make a missing segment file rebuild**

In `restore-vector-segments` (`graph.lisp:78-93`), the `(when (probe-file path) …)` guard means a missing file is a silent no-op. Change it so an absent file triggers `rebuild-vector-segment` — the vertices are present and authoritative, which is exactly the case rebuild exists for. Update the docstring to say so.

- [ ] **Step 5: Run the full suite**

Expected: **2547+ checks, 0 failures.** Confirm the two new tests now pass and nothing regressed.

- [ ] **Step 6: Commit**

Print the full diff in a fenced `## 📋 DIFF FOR REVIEW` block first, then:

```bash
cd /Users/kraison/work/vivace-graph-v3
git add segment.lisp graph.lisp tests/segment-integration-tests.lisp
git commit -m "fix(segment): rebuild sized to the corpus; a missing file is rebuilt

rebuild-vector-segment created at the 1024 default, so a fresh file was ~4 MB,
its reservation fell to the 1 GiB floor, and doubling stalled at 131,072
entries. restore-vector-segments calls it automatically whenever the clean
flag is unset, so automatic crash recovery could not complete above 131k --
half the incremental ceiling, with preconditions already observed in the field
(an OOM during a shutdown snapshot).

Sizing capacity to the live node count fixes that and removes ~8
doubling-and-relocate passes from every rebuild.

Separately: restore-vector-segments skipped entirely when the segment file was
absent, so the graph opened clean with a permanently empty vector index and no
diagnostic. It now rebuilds from the vertices."
```

---

## Out of scope (wave 2)

- **Part 2** — explicit `*segment-min-reservation*` on the open *and* create paths.
- **Part 3** — adjacent re-reservation (`MAP_FIXED_NOREPLACE`, Linux first). The `MAP_FIXED` clobber hazard is the whole difficulty; do not attempt it here.
- **Part 4** — re-reserve and relocate under the segment write lock.
