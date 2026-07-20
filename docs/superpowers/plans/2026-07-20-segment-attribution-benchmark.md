# Segment Attribution Benchmark (Phase 2, Step 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Decompose `scan-graph-store`'s `store-search` cost into node loading versus float scoring, and estimate what a vector segment would cost, so Phase 2 proceeds on a measurement rather than an inference.

**Architecture:** A new `cl-llm/bench` ASDF system builds a synthetic chunk corpus in a temporary graph, then times four operations over it. Three isolate layers of the current search; the fourth scores the same vectors from one contiguous array, which is what a segment would do. The numbers go back into the Phase 2 spec.

**Tech Stack:** Common Lisp (SBCL), ASDF, `cl-llm/rag/vivace`, `graph-db`.

## Global Constraints

- **SBCL only. ECL is out of scope for this project** — do not run it, do not add ECL reader conditionals.
- **Lisp indentation is spaces only, never tabs.**
- **This benchmark gates Phase 2 and may invalidate it.** Steps 2–5 of the spec (the segment itself) are deliberately not in this plan. Do not build any part of a segment here.
- **Never report a single sample.** Every timing is the median of at least 5 runs after a discarded warm-up. Phase 1 lost two rounds to two people drawing different single samples of a flaky measurement and reaching opposite confident conclusions.
- Existing suites must stay green: `cl-llm-rag-suite` 209/209, `cl-llm-rag-vivace` 94/94.
- Repos: benchmark code in `/Users/kraison/work/cl-llm`; the spec to update lives at `/Users/kraison/work/vivace-graph-v3/docs/superpowers/specs/2026-07-20-vector-segments-design.md`.

## What is being measured, and what each answer means

Four timings over the same corpus and the same query vector:

| id | operation | isolates |
|---|---|---|
| **A** | `rag:store-search` on a `scan-graph-store` | the number we want to improve |
| **B** | `map-chunk-vertices`, touching `%slot "EMBEDDING"`, no scoring | node loading + slot access |
| **C** | scoring vectors already collected into a Lisp vector-of-vectors | float work, scattered allocations |
| **D** | scoring the same values from ONE contiguous `(simple-array single-float (*))` | **what a segment would cost** |

Interpretations, decided in advance so the result cannot be rationalised after the fact:

- **B ≈ A** → node loading dominates. The attribution in spec §2.2 holds and segments are the right fix.
- **C ≈ A** (and B small) → scoring dominates. **The attribution is wrong; spec §10 says revisit the design rather than build anyway.**
- **D ≪ C** → contiguity itself is a large part of the win.
- **D ≈ C** → contiguity buys little. A segment's advantage over the existing `:cache` strategy would then be about *resident memory* (mmap paging vs 4.3 GB of live Lisp objects), not scan speed — a materially different justification than the spec currently gives, and one that must be written down rather than glossed.

Predicted segment latency ≈ **D**. Predicted win ≈ **A − D**.

## File Structure

| file | responsibility | change |
|---|---|---|
| `cl-llm.asd` | system definitions | add a `cl-llm/bench` system |
| `bench/packages.lisp` | package for the harness | create |
| `bench/corpus.lisp` | build a synthetic chunk corpus in a temp graph | create |
| `bench/attribution.lisp` | the four timings and the report | create |
| `docs/superpowers/specs/2026-07-20-vector-segments-design.md` (engine repo) | record results | modify §10 |

The harness uses internal symbols (`cl-llm.rag.vivace::map-chunk-vertices`, `::%slot`, `cl-llm.rag::dot`). That is acceptable in a benchmark and is preferable to widening the public API for measurement. Do not add exports for this.

---

### Task 1: Bench system and corpus generator

**Files:**
- Modify: `/Users/kraison/work/cl-llm/cl-llm.asd`
- Create: `/Users/kraison/work/cl-llm/bench/packages.lisp`
- Create: `/Users/kraison/work/cl-llm/bench/corpus.lisp`

**Interfaces:**
- Consumes: `cl-llm.rag:make-chunk`, `cl-llm.rag:as-embedding`, `cl-llm.rag:store-add`, `cl-llm.rag.vivace:make-graph-store`, `graph-db:make-graph`, `graph-db:close-graph`
- Produces:
  - `build-corpus (n dim &key batch)` → `(values store graph dir)` — an open `scan-graph-store` over a temp graph holding N chunks
  - `teardown-corpus (graph dir)` → `nil`
  - `random-unit-vector (dim)` → `(simple-array single-float (*))`

- [ ] **Step 1: Add the bench system**

Append to `/Users/kraison/work/cl-llm/cl-llm.asd`:

```lisp
(defsystem "cl-llm/bench"
  :description "Benchmarks for cl-llm/rag. Not loaded by the test suites."
  :license "MIT"
  :depends-on ("cl-llm/rag/vivace")
  :serial t
  :components ((:module "bench"
                :serial t
                :components ((:file "packages")
                             (:file "corpus")
                             (:file "attribution")))))
```

- [ ] **Step 2: Create the package**

`/Users/kraison/work/cl-llm/bench/packages.lisp`:

```lisp
;;;; bench/packages.lisp -- benchmark harness for cl-llm/rag.

(defpackage #:cl-llm.bench
  (:use #:cl)
  (:local-nicknames (#:rag #:cl-llm.rag)
                    (#:v #:cl-llm.rag.vivace)
                    (#:gdb #:graph-db))
  (:export #:build-corpus #:teardown-corpus #:random-unit-vector
           #:run-attribution #:report-attribution))
```

- [ ] **Step 3: Write the corpus generator**

`/Users/kraison/work/cl-llm/bench/corpus.lisp`:

```lisp
;;;; bench/corpus.lisp -- synthetic chunk corpora for benchmarking.

(in-package #:cl-llm.bench)

(defparameter *chunk-text-length* 800
  "Characters of body text per synthetic chunk.  Real mine-action chunks run
several hundred to a couple of thousand characters; the point is that loading a
vertex must cost something realistic, because that cost is what measurement B
exists to expose.  A corpus of empty-text chunks would understate it.")

(defun random-unit-vector (dim)
  "A random L2-normalised (simple-array single-float (DIM))."
  (let ((raw (make-array dim :element-type 'single-float)))
    (dotimes (i dim)
      (setf (aref raw i) (- (random 2.0) 1.0)))
    (rag:as-embedding raw)))

(defun %filler-text (i)
  (let ((s (make-string *chunk-text-length*)))
    (dotimes (j *chunk-text-length* s)
      (setf (char s j) (code-char (+ 97 (mod (+ i j) 26)))))))

(defun build-corpus (n dim &key (batch 1000))
  "Create a temp graph holding N chunks of DIM dimensions and return
 (values STORE GRAPH DIR).  Chunks are added in batches of BATCH -- one
transaction per batch, not per chunk (too slow) and not one for all N (peak
memory).  The store uses :SCAN, because :CACHE would mirror everything into RAM
and measurement A is meant to exercise the graph path."
  (let* ((dir (format nil "/var/tmp/cl-llm-bench-~a/" (get-universal-time)))
         (name (intern (format nil "BENCH-~a" (get-universal-time)) :keyword)))
    (ensure-directories-exist dir)
    (v:ensure-chunk-class 'rag-chunk name)
    (let* ((graph (gdb:make-graph name (pathname dir) :buffer-pool-size 1000))
           (store (v:make-graph-store graph :strategy :scan)))
      (let ((pending '()) (added 0))
        (dotimes (i n)
          (push (rag:make-chunk (%filler-text i)
                                :document-id (format nil "doc-~7,'0d" i)
                                :embedding (random-unit-vector dim))
                pending)
          (when (= (length pending) batch)
            (rag:store-add store (nreverse pending))
            (incf added batch)
            (setf pending '())
            (format t "~&  built ~a/~a~%" added n)
            (finish-output)))
        (when pending
          (rag:store-add store (nreverse pending))))
      (values store graph dir))))

(defun teardown-corpus (graph dir)
  "Close GRAPH and delete its directory."
  (ignore-errors (gdb:close-graph graph :snapshot-p nil))
  (ignore-errors (uiop:delete-directory-tree (pathname dir) :validate t))
  nil)
```

- [ ] **Step 4: Verify it builds a small corpus**

```
cd /Users/kraison/work/cl-llm
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/bench)' \
     --eval '(in-package :cl-llm.bench)' \
     --eval '(multiple-value-bind (store graph dir) (build-corpus 50 32)
                (format t "~&count=~a~%" (rag:store-count store))
                (teardown-corpus graph dir))'
```

Expected: `count=50`, no errors, and the temp directory is gone afterwards.

If `store-count` is not 50, stop and report — the corpus generator is the foundation for every number this plan produces, and a wrong corpus silently invalidates all of them.

- [ ] **Step 5: Commit**

```bash
git add cl-llm.asd bench/packages.lisp bench/corpus.lisp
git commit -m "bench: synthetic chunk corpus generator for the segment attribution experiment"
```

---

### Task 2: The four measurements

**Files:**
- Create: `/Users/kraison/work/cl-llm/bench/attribution.lisp`

**Interfaces:**
- Consumes: `build-corpus`, `teardown-corpus`, `random-unit-vector` from Task 1
- Produces:
  - `run-attribution (n dim &key runs)` → a plist `(:n :dim :a :b :c :d)` with times in milliseconds
  - `report-attribution (results)` → prints a table and the interpretation

- [ ] **Step 1: Write the measurement harness**

`/Users/kraison/work/cl-llm/bench/attribution.lisp`:

```lisp
;;;; bench/attribution.lisp -- decompose store-search cost into loading vs scoring.
;;;;
;;;; See docs/superpowers/specs/2026-07-20-vector-segments-design.md sec 10.
;;;; A: full store-search                      -- the number to improve
;;;; B: load vertices + touch the slot          -- node loading
;;;; C: score a vector-of-vectors already in RAM -- float work, scattered
;;;; D: score ONE contiguous simple-array        -- what a segment would cost

(in-package #:cl-llm.bench)

(defun %ms (fn)
  "Milliseconds FN takes to run once."
  (let ((start (get-internal-real-time)))
    (funcall fn)
    (/ (* 1000.0 (- (get-internal-real-time) start))
       internal-time-units-per-second)))

(defun %median-ms (fn runs)
  "Median milliseconds over RUNS calls, after one discarded warm-up.
Median, not mean, and never a single sample: a lone measurement of a noisy
operation is how this project previously reached two opposite confident
conclusions about the same code."
  (funcall fn)                          ; warm-up, discarded
  (let ((times (sort (loop repeat runs collect (%ms fn)) #'<)))
    (nth (floor runs 2) times)))

(defun %collect-embeddings (store)
  "Every embedding in STORE as a simple-vector of (simple-array single-float (*))."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (v::map-chunk-vertices
     store (lambda (vx) (vector-push-extend (v::%slot vx "EMBEDDING") out)))
    (coerce out 'simple-vector)))

(defun %flatten-embeddings (vectors dim)
  "Pack VECTORS into ONE contiguous (simple-array single-float (* )) of
 (length VECTORS) * DIM -- the layout a segment would use."
  (let* ((n (length vectors))
         (flat (make-array (* n dim) :element-type 'single-float)))
    (dotimes (i n flat)
      (let ((v (aref vectors i)))
        (declare (type (simple-array single-float (*)) v))
        (dotimes (j dim)
          (setf (aref flat (+ (* i dim) j)) (aref v j)))))))

(defun %score-flat (flat query n dim)
  "Score N vectors packed contiguously in FLAT against QUERY, returning the best
score.  Strides the block; no per-candidate indirection."
  (declare (type (simple-array single-float (*)) flat query)
           (type fixnum n dim)
           (optimize (speed 3) (safety 1)))
  (let ((best -2.0))
    (declare (type single-float best))
    (dotimes (i n best)
      (let ((sum 0.0) (base (* i dim)))
        (declare (type single-float sum) (type fixnum base))
        (dotimes (j dim)
          (incf sum (* (aref flat (+ base j)) (aref query j))))
        (when (> sum best) (setf best sum))))))

(defun run-attribution (n dim &key (runs 5))
  "Build an N x DIM corpus, take the four timings, tear down, return a plist."
  (format t "~&=== building corpus: n=~a dim=~a ===~%" n dim)
  (multiple-value-bind (store graph dir) (build-corpus n dim)
    (unwind-protect
         (let* ((query (random-unit-vector dim))
                (vectors (%collect-embeddings store))
                (flat (%flatten-embeddings vectors dim))
                (a (%median-ms (lambda () (rag:store-search store query 10)) runs))
                (b (%median-ms
                    (lambda ()
                      ;; Accumulate something so the slot read cannot be elided.
                      (let ((acc 0))
                        (declare (type fixnum acc))
                        (v::map-chunk-vertices
                         store (lambda (vx)
                                 (incf acc (length (v::%slot vx "EMBEDDING")))))
                        acc))
                    runs))
                (c (%median-ms
                    (lambda ()
                      (let ((best -2.0))
                        (declare (type single-float best))
                        (loop for v across vectors
                              for s = (rag:cosine query v)
                              do (when (> s best) (setf best s)))
                        best))
                    runs))
                (d (%median-ms (lambda () (%score-flat flat query n dim)) runs)))
           (list :n n :dim dim :a a :b b :c c :d d))
      (teardown-corpus graph dir))))

(defun report-attribution (results)
  "Print RESULTS and the interpretation the spec commits to in advance."
  (destructuring-bind (&key n dim a b c d) results
    (format t "~&~%=== attribution: n=~a dim=~a ===~%" n dim)
    (format t "A  full store-search        ~,1f ms~%" a)
    (format t "B  load + slot, no scoring  ~,1f ms  (~,0f%% of A)~%" b (* 100 (/ b a)))
    (format t "C  score, scattered vectors ~,1f ms  (~,0f%% of A)~%" c (* 100 (/ c a)))
    (format t "D  score, contiguous block  ~,1f ms  (~,0f%% of A)~%" d (* 100 (/ d a)))
    (format t "~%predicted segment latency  ~,1f ms~%" d)
    (format t "predicted win (A - D)      ~,1f ms~%" (- a d))
    (format t "~%interpretation:~%")
    (format t "  loading dominates?   ~a  (B >= 60%% of A)~%"
            (if (>= b (* 0.6 a)) "YES -- attribution holds" "NO"))
    (format t "  scoring dominates?   ~a  (C >= 60%% of A)~%"
            (if (>= c (* 0.6 a)) "YES -- REVISIT THE DESIGN (spec sec 10)" "NO"))
    (format t "  contiguity matters?  ~a  (D <= 70%% of C)~%"
            (if (<= d (* 0.7 c)) "YES" "NO -- segment's win is resident memory, not scan speed"))
    results))
```

- [ ] **Step 2: Run it small to verify the harness works**

```
cd /Users/kraison/work/cl-llm
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/bench)' \
     --eval '(cl-llm.bench:report-attribution (cl-llm.bench:run-attribution 200 128 :runs 5))'
```

Expected: four timings printed, all non-zero, and the interpretation lines rendered. At n=200 the absolute numbers are meaningless — this step only proves the harness runs and reports.

If any timing is exactly `0.0`, the operation is being optimised away or the clock resolution is too coarse; increase n until all four are measurable, and say so in your report.

- [ ] **Step 3: Sanity-check that B actually loads**

Confirm B is not accidentally cheap because slot access is lazy. Run the same command with `n=2000 dim=128` and check that B grows roughly linearly with n from the n=200 run (within a factor of ~2 of 10×). If B is flat as n grows, the slot read is not forcing materialisation and measurement B is invalid — stop and report.

- [ ] **Step 4: Verify the suites still pass**

The bench system is not loaded by the test suites, but `cl-llm.asd` changed:

```
sbcl --non-interactive --eval '(ql:quickload :cl-llm/rag/tests)' --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
sbcl --non-interactive --eval '(ql:quickload :cl-llm/rag/vivace/tests)' --eval '(fiveam:run! :cl-llm-rag-vivace)'
```

Expected: 209/209 and 94/94, zero failures.

- [ ] **Step 5: Commit**

```bash
git add bench/attribution.lisp
git commit -m "bench: four-way attribution of store-search cost (load vs score vs contiguous)"
```

---

### Task 3: Run at scale and record the result

**Files:**
- Modify: `/Users/kraison/work/vivace-graph-v3/docs/superpowers/specs/2026-07-20-vector-segments-design.md` (§10)

**Interfaces:**
- Consumes: `run-attribution`, `report-attribution` from Task 2
- Produces: measured numbers recorded in the spec; a go/revisit determination for Phase 2

- [ ] **Step 1: Run at the reference corpus shape**

```
cd /Users/kraison/work/cl-llm
sbcl --dynamic-space-size 8192 --non-interactive \
     --eval '(ql:quickload :cl-llm/bench)' \
     --eval '(cl-llm.bench:report-attribution (cl-llm.bench:run-attribution 19973 1024 :runs 5))'
```

This is the mine-action reference shape. Corpus build will take several minutes and print progress every 1000 chunks. Capture the full output verbatim.

- [ ] **Step 2: Run at a larger synthetic**

```
cd /Users/kraison/work/cl-llm
sbcl --dynamic-space-size 8192 --non-interactive \
     --eval '(ql:quickload :cl-llm/bench)' \
     --eval '(cl-llm.bench:report-attribution (cl-llm.bench:run-attribution 250000 1024 :runs 5))'
```

250k × 1024 is ~1 GB of embeddings plus text, so this is slow and may exhaust memory or disk.

**If it cannot complete, do not silently drop it.** Halve n and retry (125k, then 60k) until one completes, and report the largest size that ran and exactly how the larger one failed. A curve from two points is worth much more than one point, because the whole question is how cost scales.

- [ ] **Step 3: Record the numbers in the spec**

Add a `### 10.1 Results (measured)` subsection to spec §10, immediately after the existing paragraph beginning "**If loading ≈ full search**". Keep that paragraph — it states the commitment made in advance, and the value of recording results next to it is that anyone can check the outcome was not reinterpreted afterwards.

The subsection must contain:
- a table of A/B/C/D in milliseconds at each corpus size that completed
- predicted segment latency (D) and predicted win (A − D) at each size
- the machine and SBCL version the numbers came from
- if a larger size failed, the size and how it failed
- the determination, in one sentence: **attribution holds** or **design must be revisited**

Write what the numbers say, not what the design hoped. If C dominates, spec §10 commits to revisiting the design — record that plainly and stop. Do not proceed to spec steps 2–5.

- [ ] **Step 4: Commit the spec update**

```bash
cd /Users/kraison/work/vivace-graph-v3
git add docs/superpowers/specs/2026-07-20-vector-segments-design.md
git commit -m "docs(spec): record Phase 2 attribution measurements"
```

- [ ] **Step 5: Confirm both repos are clean**

```bash
cd /Users/kraison/work/cl-llm && git status --porcelain | grep -v '^??' || echo "cl-llm clean"
cd /Users/kraison/work/vivace-graph-v3 && git status --porcelain | grep -v '^??' || echo "engine clean"
```

Expected: both clean. Bench code was committed in Tasks 1–2 and the spec update in Step 4; anything else outstanding means work was done that this plan did not account for — report it rather than committing it.

---

## Done Criteria

- [ ] `cl-llm/bench` loads cleanly and both existing suites remain green (209/209, 94/94)
- [ ] All four timings are non-zero and B demonstrably scales with corpus size
- [ ] Results recorded at the 19,973 × 1024 reference shape
- [ ] Results recorded at the largest larger-synthetic size that completed, with any failure at a bigger size described rather than omitted
- [ ] Spec §10 states the determination — attribution holds, or the design must be revisited
- [ ] No part of a vector segment has been built
