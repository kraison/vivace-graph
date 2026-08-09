# Vector Segment Query Layer (Phase 2, Step 4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Query a vector segment — bounded top-k cosine over the whole segment, or over a caller-supplied candidate set — safely against concurrent growing commits.

**Architecture:** A per-segment reader-writer lock makes mutations write-exclusive and scans shared-concurrent, removing the memory-ordering race that `%seg-grow` would otherwise expose to the first concurrent reader. `segment-scan` sweeps the contiguous vector block computing full cosine, feeding a bounded top-k collector whose total order (score desc, node-id asc) is carried through eviction so ranking is deterministic. `vector-search` resolves the owner segment and scans it.

**Tech Stack:** Common Lisp (SBCL), `rw-lock.lisp`, `bordeaux-threads` (concurrency test), FiveAM.

## Global Constraints

- **SBCL only. ECL is out of scope** — do not run ECL, do not add ECL reader conditionals.
- **Lisp indentation is spaces only, never tabs.**
- **This is Step 4 of 5. Build ONLY the query layer.** NO cl-llm changes (Step 5). NO ANN/HNSW. NO int8 quantization. NO Prolog predicate or REST endpoint. If you are editing anything under `/Users/kraison/work/cl-llm`, stop.
- **The rw-lock is NON-RECURSIVE. Lock at PUBLIC boundaries only; `%seg-*` internals stay lock-free.** `segment-put` → `%seg-claim-slot` → `%seg-grow` nests, so if `%seg-grow` also took the write lock it would self-deadlock. This is the codebase's established idiom (CLAUDE.md records it for the skip-list: "lock once at public boundaries, lock-free `%`-cores internally").
- **Lock ordering invariant (deadlock-freedom):** the segment write lock is only ever taken *inside* the transaction manager lock (mutations run on the apply path); the read lock is taken alone. Never take the manager lock while holding a segment lock.
- **Scoring is full cosine** `dot/(|q|·|v|)`, not a bare dot. Zero-norm → score `0.0`, never a divide error.
- **Ranking: score descending, node-id ascending, tiebreak carried through EVICTION** (not just the final sort). This is what makes ranking deterministic across rebuilds.
- **`live-count` is occupancy, not an iterator.** Sweeps walk `[0, capacity)` and skip free cells.

## What already exists (Steps 1–3)

`segment.lisp`: `create-vector-segment`, `open-vector-segment`, `close-vector-segment`, `segment-put`, `segment-get`, `segment-remove`, `segment-capacity`, `segment-live-count`, `segment-dimension`, `rebuild-vector-segment`, and internals `%seg-id-offset (slot)`, `%seg-vec-offset (segment slot)`, `%seg-read-vector (segment slot)`, `%seg-slot-of (segment id)`, `%seg-claim-slot`, `%seg-grow`, `%seg-rebuild-id->slot`. The `vector-segment` struct has `(:conc-name segment-)` with slots `mmap`, `dimension`, `id->slot`, `clean-at-open`.

`transactions.lisp`: `%segment-key (node slot)` → `(owner-name . slot)`, `%vector-index-slot-owner-name (class slot-name)`, `node-vector-index-slots (class)`. `graph-class.lisp`: `vector-segments (graph)` → `equal` hash keyed `(owner-name . slot)`.

`rw-lock.lisp`: `(make-rw-lock)`, `(with-read-lock (lock) &body)`, `(with-write-lock (lock &key reading-p) &body)`.

The free-cell test, used by every sweep: a slot is free iff `(= (deserialize-uint64 mmap (%seg-id-offset slot)) +free-slot-marker+)`. See `%seg-rebuild-id->slot` (`segment.lisp:133`).

## File Structure

| file | responsibility | change |
|---|---|---|
| `segment.lisp` | the `lock` slot; lock the public mutation/read entry points; `%id-less-p`; the top-k collector; `segment-scan`; `segment-score-subset` | modify |
| `graph.lisp` | `vector-search` graph-level entry point | modify |
| `tests/segment-tests.lisp` | collector + `%id-less-p` unit tests | modify |
| `tests/segment-query-tests.lisp` | scan / score-subset / vector-search / concurrency / determinism | create |
| `graph-db.asd` | register the new test file | modify |

The collector and `%id-less-p` live in `segment.lisp` beside their only consumer rather than in a new file — they are ~40 lines total and meaningless outside segment scanning.

---

### Task 1: The per-segment reader-writer lock

**Files:**
- Modify: `segment.lisp` (struct; `create-vector-segment`; `open-vector-segment`; `segment-put`; `segment-remove`; `segment-get`)
- Test: existing `tests/segment-tests.lisp` (no new test — the gate is that the existing suite, which exercises put-that-grows, stays green)

**Interfaces:**
- Consumes: `make-rw-lock`, `with-read-lock`, `with-write-lock` (`rw-lock.lisp`)
- Produces: `segment-lock (segment)` → an rw-lock; the invariant that public mutations hold the write lock and public reads hold the read lock

- [ ] **Step 1: Add the lock slot**

In `segment.lisp`, add to the `vector-segment` defstruct after `clean-at-open`:

```lisp
  ;; Per-segment reader/writer lock.  All PUBLIC mutations (segment-put,
  ;; segment-remove) take the write side; public reads (segment-get,
  ;; segment-scan, segment-score-subset) take the read side.  Never persisted --
  ;; created fresh by create/open.
  ;;
  ;; NON-RECURSIVE: lock at PUBLIC boundaries only.  The %SEG-* internals are
  ;; lock-free and assume the caller holds the lock -- segment-put ->
  ;; %seg-claim-slot -> %seg-grow nests, so locking inside %seg-grow would
  ;; self-deadlock.  Same idiom as the skip list.
  ;;
  ;; LOCK ORDER: the write side is only ever taken INSIDE the transaction
  ;; manager lock (mutations run on the apply path); the read side is taken
  ;; alone.  Never take the manager lock while holding a segment lock.
  (lock (make-rw-lock)))
```

- [ ] **Step 2: Confirm create/open get a lock**

The defstruct initform gives every `%make-vector-segment` call a fresh lock, so `create-vector-segment` and `open-vector-segment` need no change. Verify by reading both — if either passes `:lock` explicitly or bypasses the constructor, fix it so both get a fresh lock. Note what you found in your report.

- [ ] **Step 3: Run the suite to confirm it is still green BEFORE locking anything**

```
cd /Users/kraison/work/vivace-graph-v3
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: PASS (117 checks). Adding an unused slot changes nothing. This is the baseline for Step 5.

- [ ] **Step 4: Take the locks at the public boundaries**

Wrap the *bodies* of the three public entry points. `segment-put` (`segment.lisp:271`) and `segment-remove` (`:334`) take the write lock; `segment-get` (`:294`) takes the read lock. Example shape for `segment-put` — apply the same wrapping to the other two, keeping each function's existing body verbatim inside:

```lisp
(defun segment-put (segment id vector)
  "<keep the existing docstring, and add:>
Takes the segment's WRITE lock: mutations are exclusive against concurrent
scans.  The %SEG-* internals it calls (including %seg-grow) are lock-free and
run under this lock."
  (with-write-lock ((segment-lock segment))
    ;; ... existing body verbatim ...
    ))
```

Do **not** add a lock inside `%seg-grow`, `%seg-claim-slot`, `%seg-write-vector`, `%seg-write-id`, `%seg-read-vector`, `%seg-slot-of`, or `%seg-rebuild-id->slot` — they run under a caller's lock.

`open-vector-segment` calls `%seg-rebuild-id->slot` before the segment is reachable by anyone else, so it needs no lock.

- [ ] **Step 5: Run the suite — this is the real gate**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-integration-suite))'
```

Expected: both PASS, unchanged counts. **This specifically proves no self-deadlock:** `segment-grows-past-initial-capacity` drives `segment-put` → `%seg-claim-slot` → `%seg-grow` while holding the write lock. If it hangs instead of failing, you have added a lock inside an internal — remove it.

Also run the full `graph-db-suite` and report the count (the apply path calls `segment-put` under the manager lock; this confirms the nested locking is sound end to end).

- [ ] **Step 6: Commit**

```bash
git add segment.lisp
git commit -m "feat(segment): per-segment rw-lock; mutations write-exclusive, reads shared"
```

---

### Task 2: `%id-less-p` and the bounded top-k collector

**Files:**
- Modify: `segment.lisp` (add both, near the scan code that will use them)
- Test: `tests/segment-tests.lisp`

**Interfaces:**
- Consumes: nothing from Task 1
- Produces:
  - `%id-less-p (a b)` → boolean — lexicographic over two 16-byte `(unsigned-byte 8)` id arrays
  - `%score-before-p (s1 id1 s2 id2)` → boolean — the total order: score desc, id asc
  - `%make-topk (k)` → collector; `%topk-offer (collector score id)`; `%topk-results (collector)` → `((score . id) ...)` best first

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-tests.lisp`:

```lisp
(defun %tid (n)
  "A 16-byte id whose first byte is N (so ids order by N)."
  (let ((v (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref v 0) n)
    v))

(test id-less-p-is-lexicographic
  "%id-less-p orders ids by the first differing byte; equal ids are not less."
  (is (%id-less-p (%tid 1) (%tid 2)))
  (is (not (%id-less-p (%tid 2) (%tid 1))))
  (is (not (%id-less-p (%tid 1) (%tid 1))))
  ;; a later differing byte decides when earlier bytes tie
  (let ((a (%tid 5)) (b (%tid 5)))
    (setf (aref a 3) 1 (aref b 3) 2)
    (is (%id-less-p a b))
    (is (not (%id-less-p b a)))))

(test topk-keeps-the-best-k
  "The collector keeps exactly the k highest scores, best first."
  (let ((c (%make-topk 3)))
    (dolist (row (list (list 0.1 (%tid 1)) (list 0.9 (%tid 2)) (list 0.5 (%tid 3))
                       (list 0.7 (%tid 4)) (list 0.2 (%tid 5))))
      (%topk-offer c (coerce (first row) 'single-float) (second row)))
    (let ((ids (mapcar (lambda (pair) (aref (cdr pair) 0)) (%topk-results c))))
      (is (equal '(2 4 3) ids)))))

(test topk-handles-fewer-than-k
  "Fewer offers than k returns all of them, ordered."
  (let ((c (%make-topk 5)))
    (%topk-offer c 0.2f0 (%tid 1))
    (%topk-offer c 0.8f0 (%tid 2))
    (is (equal '(2 1) (mapcar (lambda (p) (aref (cdr p) 0)) (%topk-results c))))))

(test topk-tiebreak-is-order-independent
  "A tie at the k-th boundary resolves by id ascending, NOT by arrival order.
This is the property that makes ranking deterministic across rebuilds: slot
iteration order is meaningless under free-list reuse, so eviction must consult
the tiebreak, not just the final sort."
  (flet ((collect-in (rows)
           (let ((c (%make-topk 2)))
             (dolist (row rows)
               (%topk-offer c (coerce (first row) 'single-float) (second row)))
             (mapcar (lambda (p) (aref (cdr p) 0)) (%topk-results c)))))
    ;; id 3 clearly best; ids 1 and 2 tie at 0.5 -- the lower id must win the
    ;; last slot regardless of which arrives first.
    (let ((forward  (collect-in (list (list 0.9 (%tid 3)) (list 0.5 (%tid 1)) (list 0.5 (%tid 2)))))
          (backward (collect-in (list (list 0.9 (%tid 3)) (list 0.5 (%tid 2)) (list 0.5 (%tid 1))))))
      (is (equal '(3 1) forward))
      (is (equal forward backward)
          "eviction depends on arrival order: ~S vs ~S" forward backward))))

(test topk-k-zero-returns-empty
  (let ((c (%make-topk 0)))
    (%topk-offer c 0.9f0 (%tid 1))
    (is (null (%topk-results c)))))
```

Add `%id-less-p`, `%make-topk`, `%topk-offer`, `%topk-results` to the `:import-from #:graph-db` list in `tests/package.lisp` (the test package does not `:use :graph-db`).

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: FAIL — the four new symbols are undefined.

- [ ] **Step 3: Implement**

Add to `segment.lisp`:

```lisp
(defun %id-less-p (a b)
  "Lexicographic order over two 16-byte node ids.  The engine has
UUID-ARRAY-EQUAL but no less-than, so this is it: first differing byte wins."
  (declare (type (array (unsigned-byte 8) (*)) a b))
  (dotimes (i +key-bytes+ nil)
    (let ((x (aref a i)) (y (aref b i)))
      (cond ((< x y) (return t))
            ((> x y) (return nil))))))

(defun %score-before-p (s1 id1 s2 id2)
  "The segment ranking order: score DESCENDING, node-id ASCENDING on a tie."
  (declare (type single-float s1 s2))
  (cond ((> s1 s2) t)
        ((< s1 s2) nil)
        (t (%id-less-p id1 id2))))

;;; Bounded top-k collector.  Never materialises one result per candidate: a
;;; scan offers every occupied slot and only k are retained.  k is small, so a
;;; linear scan of the k-element buffer beats a heap's bookkeeping.
;;;
;;; The tiebreak is carried through EVICTION, not applied only at the end.
;;; Eviction happens during iteration, so a score-only comparison at the k-th
;;; boundary would make the result depend on slot order -- which is meaningless
;;; under free-list reuse, and would make ranking differ between an incrementally
;;; built segment and a rebuilt one.
(defstruct (topk (:constructor %make-topk-raw))
  (k 0 :type fixnum)
  (count 0 :type fixnum)
  (scores nil :type (or null (simple-array single-float (*))))
  (ids nil :type (or null simple-vector)))

(defun %make-topk (k)
  (%make-topk-raw :k k
                  :scores (make-array (max k 1) :element-type 'single-float)
                  :ids (make-array (max k 1) :initial-element nil)))

(defun %topk-worst-index (c)
  "Index of the entry that ranks LAST under %SCORE-BEFORE-P."
  (let ((scores (topk-scores c)) (ids (topk-ids c)) (worst 0))
    (declare (type (simple-array single-float (*)) scores))
    (dotimes (i (topk-count c) worst)
      (when (%score-before-p (aref scores worst) (aref ids worst)
                             (aref scores i) (aref ids i))
        (setf worst i)))))

(defun %topk-offer (c score id)
  "Offer SCORE/ID; keep it only if it outranks the current worst."
  (declare (type single-float score))
  (when (plusp (topk-k c))
    (let ((scores (topk-scores c)) (ids (topk-ids c)))
      (cond ((< (topk-count c) (topk-k c))
             (setf (aref scores (topk-count c)) score
                   (aref ids (topk-count c)) id)
             (incf (topk-count c)))
            (t
             (let ((worst (%topk-worst-index c)))
               (when (%score-before-p score id (aref scores worst) (aref ids worst))
                 (setf (aref scores worst) score
                       (aref ids worst) id)))))))
  c)

(defun %topk-results (c)
  "Retained entries as (score . id) conses, best first."
  (let ((out '()))
    (dotimes (i (topk-count c))
      (push (list (aref (topk-scores c) i) (aref (topk-ids c) i)) out))
    (mapcar (lambda (row) (cons (first row) (second row)))
            (sort out (lambda (a b)
                        (%score-before-p (first a) (second a)
                                         (first b) (second b)))))))
```

- [ ] **Step 4: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: PASS, all five new tests.

- [ ] **Step 5: Sabotage-prove the tiebreak**

Temporarily change `%score-before-p`'s tie branch from `(%id-less-p id1 id2)` to `nil` (score-only ordering), re-run, and confirm `topk-tiebreak-is-order-independent` FAILS. Restore and confirm green. Report both outcomes. A tiebreak test that passes under a score-only comparator is decoration — this project has shipped nine such assertions.

- [ ] **Step 6: Commit**

```bash
git add segment.lisp tests/segment-tests.lisp tests/package.lisp
git commit -m "feat(segment): %id-less-p and the bounded top-k collector"
```

---

### Task 3: `segment-scan`

**Files:**
- Modify: `segment.lisp`
- Modify: `graph-db.asd` (register `segment-query-tests` — do it here so this task's tests run)
- Test: `tests/segment-query-tests.lisp` (create)

**Interfaces:**
- Consumes: `segment-lock` (Task 1); `%make-topk` / `%topk-offer` / `%topk-results` (Task 2); `%seg-id-offset`, `%seg-read-vector`, `segment-capacity`, `+free-slot-marker+`, `+key-bytes+`
- Produces: `segment-scan (segment query-vector k)` → `((score . node-id) ...)` best first

- [ ] **Step 1: Write the failing tests**

Create `tests/segment-query-tests.lisp`:

```lisp
;;;; Tests for the vector-segment query layer (segment-scan, score-subset,
;;;; vector-search, concurrency).

(in-package #:graph-db/test)

(def-suite segment-query-suite
  :description "segment-scan / segment-score-subset / vector-search."
  :in graph-db-suite)

(in-suite segment-query-suite)

(defun %qpath ()
  (format nil "/var/tmp/vgquery-~a.dat" (get-internal-real-time)))

(defun %qvec (dim &rest floats)
  "A DIM-long single-float vector from FLOATS, zero-padded."
  (let ((v (make-array dim :element-type 'single-float :initial-element 0.0)))
    (loop for f in floats for i from 0 do (setf (aref v i) (coerce f 'single-float)))
    v))

(defun %qid (n)
  (let ((v (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref v 0) n)
    v))

(defun %brute-force (pairs query)
  "Reference ranking: (score . id) best first, full cosine, id-asc tiebreak."
  (let ((scored (mapcar (lambda (p)
                          (cons (graph-db::%cosine query (car p)) (cdr p)))
                        pairs)))
    (sort scored (lambda (a b) (graph-db::%score-before-p (car a) (cdr a)
                                                          (car b) (cdr b))))))

(test scan-matches-brute-force
  "segment-scan's top-k equals a brute-force full-cosine ranking."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8))
               (pairs '()))
           (unwind-protect
                (progn
                  (loop for n from 1 to 6
                        for v = (%qvec 4 (coerce n 'single-float) 1.0 0.0 0.0)
                        do (segment-put s (%qid n) v)
                           (push (cons v (%qid n)) pairs))
                  (let* ((q (%qvec 4 1.0 1.0 0.0 0.0))
                         (got (segment-scan s q 3))
                         (want (subseq (%brute-force (nreverse pairs) q) 0 3)))
                    (is (= 3 (length got)) "expected 3 hits, got ~S" got)
                    (loop for g in got for w in want
                          do (is (equalp (cdr g) (cdr w)) "id order differs")
                             (is (< (abs (- (car g) (car w))) 1e-5)
                                 "score differs: ~A vs ~A" (car g) (car w)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-k-larger-than-occupancy
  "k greater than the number of stored vectors returns them all, no padding."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 0.0 1.0 0.0 0.0))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 2 (length got)))
                    (is (every (lambda (p) (and (numberp (car p)) (cdr p))) got))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-skips-removed-slots
  "A removed id does not appear in scan results."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-remove s (%qid 1))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 1 (length got)))
                    (is (equalp (%qid 2) (cdr (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-empty-and-zero-k
  "An empty segment and k=0 both return NIL, not an error."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (is (null (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 5)))
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (is (null (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 0))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-zero-norm-scores-zero
  "A zero-norm query returns nothing; a zero-norm stored vector scores 0.0
rather than signalling a divide error."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 0.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  ;; zero query -> empty
                  (is (null (segment-scan s (%qvec 4 0.0 0.0 0.0 0.0) 5)))
                  ;; zero stored vector scores 0.0 and ranks last
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 5)))
                    (is (= 2 (length got)))
                    (is (equalp (%qid 2) (cdr (first got))))
                    (is (< (abs (car (second got))) 1e-6)
                        "zero-norm vector should score 0.0, got ~A" (car (second got)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))
```

Register the file in `graph-db.asd`'s `graph-db/test` system, **after `segment-integration-tests`**:

```lisp
               (:file "segment-query-tests")
```

Add `segment-scan`, `%cosine`, and `%score-before-p` to `tests/package.lisp`'s `:import-from #:graph-db` list as needed (`%score-before-p` may already be there from Task 2).

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: FAIL — `segment-scan` and `%cosine` undefined.

- [ ] **Step 3: Implement `%cosine` and `segment-scan`**

Add to `segment.lisp`:

```lisp
(defun %vector-norm (v)
  "Euclidean norm of V."
  (declare (type (simple-array single-float (*)) v)
           (optimize (speed 3) (safety 1)))
  (let ((sum 0f0))
    (declare (type single-float sum))
    (dotimes (i (length v) (sqrt sum))
      (incf sum (* (aref v i) (aref v i))))))

(defun %cosine (a b)
  "Full cosine similarity of two equal-length single-float vectors.  Returns
0.0 when either has zero norm (no divide error).  This does NOT assume unit
vectors -- the segment stores whatever the caller put in it."
  (declare (type (simple-array single-float (*)) a b)
           (optimize (speed 3) (safety 1)))
  (let ((dot 0f0) (na 0f0) (nb 0f0))
    (declare (type single-float dot na nb))
    (dotimes (i (min (length a) (length b)))
      (let ((x (aref a i)) (y (aref b i)))
        (incf dot (* x y))
        (incf na (* x x))
        (incf nb (* y y))))
    (if (or (zerop na) (zerop nb))
        0f0
        (/ dot (* (sqrt na) (sqrt nb))))))

(defun segment-scan (segment query-vector k)
  "Top-K by full cosine over every occupied slot, best first, as (score . id)
conses.  Takes the segment's READ lock, so it is safe against a concurrent
growing commit (which holds the write lock).

Touches ONLY the id array and the contiguous vector block -- it never
materialises a node, which is the entire point of the segment.

Sweeps [0, capacity) skipping free cells: occupied slots are NOT dense
[0, live-count) once the free list has been used."
  (declare (type (simple-array single-float (*)) query-vector))
  (when (or (zerop k) (zerop (%vector-norm query-vector)))
    (return-from segment-scan nil))
  (with-read-lock ((segment-lock segment))
    (let ((mmap (segment-mmap segment))
          (cap (segment-capacity segment))
          (collector (%make-topk k)))
      (dotimes (slot cap)
        (unless (= (deserialize-uint64 mmap (%seg-id-offset slot)) +free-slot-marker+)
          (let ((id (get-bytes mmap (%seg-id-offset slot) +key-bytes+))
                (v (%seg-read-vector segment slot)))
            (%topk-offer collector (%cosine query-vector v) id))))
      (%topk-results collector))))
```

- [ ] **Step 4: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: PASS, all five.

- [ ] **Step 5: Commit**

```bash
git add segment.lisp graph-db.asd tests/segment-query-tests.lisp tests/package.lisp
git commit -m "feat(segment): segment-scan -- bounded top-k full cosine, read-locked"
```

---

### Task 4: `segment-score-subset`

**Files:**
- Modify: `segment.lisp`
- Test: `tests/segment-query-tests.lisp`

**Interfaces:**
- Consumes: `segment-lock`, `%cosine`, `%score-before-p`, `%seg-slot-of`, `%seg-read-vector`
- Produces: `segment-score-subset (segment query-vector node-ids)` → `((score . node-id) ...)` best first

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-query-tests.lisp`:

```lisp
(test score-subset-agrees-with-scan
  "Scoring a candidate set gives the same scores and order as a full scan
restricted to those ids.  This is the ANN seam: a future index proposes
candidates and this scores them exactly."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (loop for n from 1 to 5
                        do (segment-put s (%qid n)
                                        (%qvec 4 (coerce n 'single-float) 1.0 0.0 0.0)))
                  (let* ((q (%qvec 4 1.0 1.0 0.0 0.0))
                         (subset (list (%qid 2) (%qid 4)))
                         (got (segment-score-subset s q subset))
                         (full (segment-scan s q 10)))
                    (is (= 2 (length got)) "expected 2 scored, got ~S" got)
                    ;; every subset result must match that id's score in the full scan
                    (dolist (pair got)
                      (let ((from-scan (find (cdr pair) full :key #'cdr :test #'equalp)))
                        (is (not (null from-scan)) "id missing from full scan")
                        (is (< (abs (- (car pair) (car from-scan))) 1e-6)
                            "score differs from scan: ~A vs ~A"
                            (car pair) (car from-scan))))
                    ;; and they must be in best-first order
                    (is (>= (car (first got)) (car (second got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-skips-unknown-ids
  "Ids absent from the segment are skipped, not errors."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (let ((got (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0)
                                                   (list (%qid 1) (%qid 99)))))
                    (is (= 1 (length got)))
                    (is (equalp (%qid 1) (cdr (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test score-subset-empty-inputs
  "An empty id list, and a zero-norm query, both return NIL."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (is (null (segment-score-subset s (%qvec 4 1.0 0.0 0.0 0.0) '())))
                  (is (null (segment-score-subset s (%qvec 4 0.0 0.0 0.0 0.0)
                                                  (list (%qid 1))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))
```

Add `segment-score-subset` to `tests/package.lisp`'s import list.

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: FAIL — `segment-score-subset` undefined.

- [ ] **Step 3: Implement**

Add to `segment.lisp`:

```lisp
(defun segment-score-subset (segment query-vector node-ids)
  "Score only NODE-IDS against QUERY-VECTOR by full cosine, best first, as
 (score . id) conses.  Ids absent from the segment are silently skipped.

This is the extension seam that keeps ANN addable: a future index (or an int8
pre-rank) proposes a candidate set and this scores it exactly.  Nothing here
assumes it has seen every vector in the segment.

Takes the READ lock, like SEGMENT-SCAN."
  (declare (type (simple-array single-float (*)) query-vector))
  (when (or (null node-ids) (zerop (%vector-norm query-vector)))
    (return-from segment-score-subset nil))
  (with-read-lock ((segment-lock segment))
    (let ((out '()))
      (dolist (id node-ids)
        (let ((slot (%seg-slot-of segment id)))
          (when slot
            (push (cons (%cosine query-vector (%seg-read-vector segment slot)) id)
                  out))))
      (sort out (lambda (a b) (%score-before-p (car a) (cdr a) (car b) (cdr b)))))))
```

- [ ] **Step 4: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add segment.lisp tests/segment-query-tests.lisp tests/package.lisp
git commit -m "feat(segment): segment-score-subset -- the ANN candidate-scoring seam"
```

---

### Task 5: `vector-search` — the graph-level entry point

**Files:**
- Modify: `graph.lisp`
- Test: `tests/segment-query-tests.lisp`

**Interfaces:**
- Consumes: `segment-scan` (Task 3); `%vector-index-slot-owner-name` (`transactions.lisp`); `vector-segments` (`graph-class.lisp`)
- Produces: `vector-search (graph class-name slot-name query-vector k)` → `((score . node-id) ...)`

- [ ] **Step 1: Write the failing tests**

Append to `tests/segment-query-tests.lisp`:

```lisp
(test vector-search-finds-the-nearest-node
  "vector-search resolves the owner segment and returns nearest node ids."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (near nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf near (id (make-si-doc :title "near"
                                             :embedding (%qvec 8 1.0 0.0)))))
               (with-transaction ()
                 (make-si-doc :title "far" :embedding (%qvec 8 0.0 1.0))))
             (let ((got (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 2)))
               (is (= 2 (length got)) "expected 2 hits, got ~S" got)
               (is (equalp near (cdr (first got)))
                   "nearest node should rank first")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test vector-search-empty-when-nothing-indexed
  "A declared slot with no segment yet returns NIL, not an error (segments are
created lazily on first conforming write)."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (is (null (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 5)))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test vector-search-spans-subclasses
  "Model B: a subclass instance is in the owner's segment, so searching the
owner class finds it."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (sub-id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-doc :title "parent" :embedding (%qvec 8 0.0 1.0)))
               (with-transaction ()
                 (setf sub-id (id (make-si-sub :title "child" :extra "x"
                                               :embedding (%qvec 8 1.0 0.0))))))
             (let ((got (vector-search g 'si-doc 'embedding (%qvec 8 1.0 0.0) 5)))
               (is (= 2 (length got)) "owner segment should hold both, got ~S" got)
               (is (equalp sub-id (cdr (first got)))
                   "the subclass instance should be found and rank first")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
```

`si-doc` / `si-sub` are declared in `tests/segment-integration-tests.lisp` under `:graph-db-integration-test`; this file loads after it, so the schema is available. Add `vector-search` to `tests/package.lisp`'s import list.

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: FAIL — `vector-search` undefined.

- [ ] **Step 3: Implement**

Add to `graph.lisp`, near `restore-vector-segments`:

```lisp
(defun vector-search (graph class-name slot-name query-vector k)
  "Top-K nodes of CLASS-NAME (and its subclasses) whose SLOT-NAME vector is
nearest QUERY-VECTOR by cosine, as (score . node-id) conses, best first.

Resolves the OWNER segment for (CLASS-NAME, SLOT-NAME) -- under Model B one
segment per declaring class holds the whole hierarchy -- and scans it.  Returns
NIL when no segment exists yet: segments are created lazily on the first
conforming write, so a declared-but-never-written slot simply has nothing to
search."
  (let* ((class (find-class class-name nil))
         (owner (and class (%vector-index-slot-owner-name class slot-name)))
         (segment (and owner
                       (gethash (cons owner slot-name) (vector-segments graph)))))
    (when segment
      (segment-scan segment query-vector k))))
```

`graph.lisp` loads *before* `transactions.lisp` in `graph-db.asd`, but this is fine and already established: `all-vector-segment-owner-keys` (`graph.lisp:72`, from Step 3) calls `%vector-index-slot-owner-name` the same way. It is a runtime call, so the definition is present by the time it runs. Put `vector-search` in `graph.lisp` beside `restore-vector-segments` and do NOT duplicate the owner-resolution logic.

- [ ] **Step 4: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: PASS, all three.

- [ ] **Step 5: Commit**

```bash
git add graph.lisp tests/segment-query-tests.lisp tests/package.lisp
git commit -m "feat(segment): vector-search graph-level entry point"
```

---

### Task 6: The concurrency test and ranking determinism

**Files:**
- Test: `tests/segment-query-tests.lisp`

**Interfaces:**
- Consumes: everything from Tasks 1–5; `bordeaux-threads` (already a graph-db dependency); `rebuild-vector-segment` (`segment.lisp`)
- Produces: no new API — this task is the proof that Task 1's lock does its job

**This is the load-bearing task of the step.** A memory-ordering race passes a single-threaded suite green and fails in production. A badly-built concurrency test also passes green without ever exercising the race — so the sabotage step is what makes this real, and it is not optional.

- [ ] **Step 1: Write the concurrency test**

Append to `tests/segment-query-tests.lisp`:

```lisp
(test scan-is-safe-against-growing-writes
  "A scanner running continuously against a writer doing GROWING commits never
observes a torn read.  Every score must be a real number in [-1, 1]: a torn
vector (half-old, half-new bytes) or a half-relocated block produces NaN or an
out-of-range score.  This is the race the per-segment rw-lock exists to prevent.

Constructed to actually exercise it: a small initial capacity so growth (which
relocates the WHOLE vector block and flips capacity last) happens many times,
a wide dimension so each relocation takes real time, and a scanner that runs
throughout."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 256 :initial-capacity 2))
               (stop nil)
               (bad '())
               (scans 0))
           (unwind-protect
                (let* ((q (let ((v (make-array 256 :element-type 'single-float
                                                   :initial-element 0.0)))
                            (setf (aref v 0) 1.0)
                            v))
                       (scanner
                         (bordeaux-threads:make-thread
                          (lambda ()
                            (loop until stop
                                  do (let ((hits (segment-scan s q 5)))
                                       (incf scans)
                                       (dolist (hit hits)
                                         (let ((score (car hit)))
                                           ;; NaN fails both comparisons
                                           (unless (and (<= score 1.0001)
                                                        (>= score -1.0001))
                                             (push score bad)))))))
                          :name "segment-scanner")))
                  ;; 400 inserts into a capacity-2 segment forces ~8 growths
                  (dotimes (i 400)
                    (let ((v (make-array 256 :element-type 'single-float
                                             :initial-element 0.0))
                          (id (make-array 16 :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
                      (setf (aref v (mod i 256)) 1.0
                            (aref id 0) (mod i 256)
                            (aref id 1) (floor i 256))
                      (segment-put s id v)))
                  (setf stop t)
                  (bordeaux-threads:join-thread scanner)
                  (is (plusp scans) "the scanner never ran -- test proves nothing")
                  (is (null bad)
                      "~D torn reads: out-of-range scores ~S"
                      (length bad) (subseq bad 0 (min 5 (length bad)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test ranking-is-deterministic-across-rebuild
  "Scanning a segment, rebuilding it from nodes, and scanning again gives an
identical ranking -- including ties.  Slot order is meaningless under free-list
reuse, so this only holds because the tiebreak is carried through eviction."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (let ((*graph* g))
               ;; several nodes sharing a score with the query, to force ties
               (dotimes (i 6)
                 (with-transaction ()
                   (make-si-doc :title (format nil "n~d" i)
                                :embedding (%qvec 8 1.0 1.0)))))
             (let* ((q (%qvec 8 1.0 1.0))
                    (before (vector-search g 'si-doc 'embedding q 6)))
               (is (= 6 (length before)) "expected 6 hits, got ~S" before)
               (rebuild-vector-segment g 'si-doc 'embedding)
               (let ((after (vector-search g 'si-doc 'embedding q 6)))
                 (is (= (length before) (length after)))
                 (loop for b in before for a in after
                       do (is (equalp (cdr b) (cdr a))
                              "ranking changed across rebuild: ~S vs ~S"
                              (cdr b) (cdr a))
                          (is (< (abs (- (car b) (car a))) 1e-6))))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
```

- [ ] **Step 2: Run to verify they pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-query-suite))'
```

Expected: PASS. If `scan-is-safe-against-growing-writes` hangs, you have a lock-ordering bug (a scan waiting on a writer that never releases) — investigate before proceeding.

- [ ] **Step 3: SABOTAGE — prove the lock is load-bearing**

The race is timing-dependent, so removing the lock alone may not fail reliably. Widen the window deterministically, exactly as a prior race investigation in this repo did:

1. Temporarily remove `with-read-lock` from `segment-scan` and `with-write-lock` from `segment-put`.
2. Temporarily insert `(sleep 0.001)` inside `%seg-grow` between the relocation loop and the `(serialize-uint64 mmap new-cap 32)` capacity flip — this is precisely the window where a reader can see the new capacity against a not-yet-relocated block.
3. Run `segment-query-suite` and confirm `scan-is-safe-against-growing-writes` **FAILS** with torn reads (out-of-range or NaN scores).
4. Restore all three edits and confirm the suite is green again, and that `git diff` on `segment.lisp` is empty relative to the pre-sabotage commit.

Report all of it: the failing output with the bad scores, and the restored-green run. **If the sabotaged run does NOT fail, say so plainly rather than claiming the proof** — that would mean the test is not exercising the race and needs reworking (try a larger dimension, a longer sleep, or more scanner iterations).

- [ ] **Step 4: Run the full suite**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))'
```

Expected: green, with the new checks, zero failures. Report the count. Note: a `write-path-suite` temp-dir flake has been seen once in this repo — if you hit it, re-run once and say so.

- [ ] **Step 5: Commit**

```bash
git add tests/segment-query-tests.lisp
git commit -m "test(segment): concurrency safety against growing writes; rebuild determinism"
```

---

## Done Criteria

- [ ] `segment-query-suite` green; `segment-suite` and `segment-integration-suite` unchanged and green; full `graph-db-suite` green (report counts)
- [ ] `segment-scan` matches a brute-force full-cosine ranking; handles k>occupancy, k=0, empty segment, zero-norm query, zero-norm stored vector, and removed slots
- [ ] `segment-score-subset` agrees with `segment-scan` on the same ids and skips unknown ids
- [ ] `vector-search` finds nearest nodes through a graph, spans subclasses (Model B), and returns NIL for a declared-but-unwritten slot
- [ ] The tiebreak was **shown to fail** under a score-only comparator (Task 2 sabotage)
- [ ] The concurrency test was **shown to fail** with the locks removed and the grow window widened (Task 6 sabotage) — or the failure to reproduce was reported honestly
- [ ] No cl-llm change, no ANN, no int8, no Prolog/REST surface
