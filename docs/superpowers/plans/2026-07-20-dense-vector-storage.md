# Dense Vector Storage (Phase 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Store embedding vectors in VivaceGraph as a contiguous block of IEEE-754 float32s instead of one boxed, separately-allocated float per element, and make the cl-llm scoring path typed and bounded.

**Architecture:** A new serialization type tag `+float-vector+` handles `(simple-array single-float (*))` in one allocation on encode and one on decode, following the existing `(unsigned-byte 8)` blob special-case already inside `serialize ((v vector))`. On the cl-llm side, embeddings become single-float and L2-normalised at ingest, so cosine reduces to a typed dot product, and `store-search` uses a bounded top-k heap instead of building and sorting a full-corpus hit list.

**Tech Stack:** Common Lisp (SBCL + ECL), ASDF, FiveAM, `ieee-floats` (already a transitive dependency, used by the existing single/double float codecs).

## Global Constraints

- **Lisp indentation is spaces only, never tabs.** Applies to every `.lisp` and `.asd` edit.
- **Must work on SBCL and ECL.** No implementation-specific code without a reader conditional covering both. ECL has no custom hash-table tests and needs an `#+ecl` branch in any per-implementation conditional that wraps a value or a macro body.
- **Must work across both index backends** (`:skip-list` and `:bplus-tree`). Phase 1 touches serialization only, which is backend-independent, but the round-trip-through-a-graph test must run under both.
- **Existing serialized data must still read.** `+float-vector+` is a NEW tag (31). No existing tag's encoding changes. Vectors that are not `single-float` continue through the existing generic path unmodified.
- **Reading correctly is not the same as meaning the same thing.** Phase 1 changes both the element type and the normalisation contract of stored embeddings. Old rows decode perfectly and *rank wrong*. Task 6 exists solely to close that gap, and Task 7's direct-slot scoring is unsafe without it. Do not reorder them.
- **Two repos:** `/Users/kraison/work/vivace-graph-v3` (engine, Tasks 1–3) and `/Users/kraison/work/cl-llm` (Tasks 4–6). Commit in each repo separately. Engine work is on the `experiment` branch.
- **Show the full diff for review before every `git commit`.** Do not commit unreviewed.

## Scope Notes (read before starting)

Two things the spec mentions that are **deliberately NOT in this plan**:

1. **True zero-copy typed views onto the mmap.** This plan delivers *one allocation* per encode and per decode, down from ~1024. A genuine no-copy view requires the vector to live outside the node's serialized data alist, which is the L1 segments work in Phase 2. Do not attempt it here.
2. **Not deserialising `TEXT`/`METADATA` for scored candidates.** A node's slots are materialised together when the node loads, so skipping `TEXT` requires vectors to live outside node payload — again Phase 2. What Task 6 *does* achieve is not **constructing** `rag:chunk` objects for candidates that lose, which is a real and separate win.
3. **The int8-quantised companion representation** (spec §7.1). The wire format reserves an element-type byte for it, but nothing in Phase 1 writes or reads one. It buys memory-bandwidth wins that only matter once segments exist to stream.

## File Structure

| file | responsibility | change |
|---|---|---|
| `globals.lisp` | type tag constants | add `+float-vector+` (31), `+fv-single-float+` (1) |
| `serialize.lisp` | binary codec | add float-vector branch to `serialize ((v vector))`; add `deserialize-help` method |
| `tests/serialize-tests.lisp` | codec round-trip tests | add float-vector tests |
| `tests/graph-tests.lisp` | graph-level round trip | add a vector-slot-through-a-graph test |
| `cl-llm/vivace/schema.lisp` | chunk vertex type | `double-float` → `single-float` |
| `cl-llm/rag/embed.lisp` | embedding ingest | `as-embedding` produces normalised single-float |
| `cl-llm/rag/store.lisp` | scoring + memory store | typed `dot`; bounded top-k heap |
| `cl-llm/vivace/store.lisp` | graph-backed store | use the heap; build chunks only for survivors |

`extract-length` and `extract-length-mmap` need **no change** — tag 31 falls through to their default variable-length branch, which reads `[tag][n-bytes][length-bytes...][payload]`, exactly the layout used here. Verify this in Task 1 Step 2 rather than assuming it.

## Wire Format

```
byte 0        : +float-vector+ (31)
byte 1        : n = number of length bytes
bytes 2..1+n  : payload length, little-endian
byte 2+n      : element type code (+fv-single-float+ = 1)
rest          : dimension * 4 bytes, little-endian IEEE-754 float32
```

Dimension is derived: `(floor (1- payload-length) 4)`. The element type byte exists so `double-float` and `int8` can be added later without a new tag.

---

### Task 1: The `+float-vector+` tag and encoder

**Files:**
- Modify: `globals.lisp:162` (after `+bignum+`)
- Modify: `serialize.lisp:221-238` (the `serialize ((v vector))` method)
- Test: `tests/serialize-tests.lisp`

**Interfaces:**
- Consumes: `encode-length` (`serialize.lisp:19`), `ieee-floats:encode-float32`
- Produces: constants `+float-vector+`, `+fv-single-float+`; function `%serialize-float-vector (v)` → `(simple-array (unsigned-byte 8) (*))`

- [ ] **Step 1: Write the failing test**

Append to `tests/serialize-tests.lisp`:

```lisp
(defun fv (&rest floats)
  "A (simple-array single-float (*)) built from FLOATS."
  (make-array (length floats) :element-type 'single-float
                              :initial-contents (mapcar (lambda (x) (coerce x 'single-float))
                                                        floats)))

(test float-vector-header
  "A serialized float vector carries the float-vector tag and a correct payload length."
  (let ((bytes (serialize (fv 1.0 2.0 3.0))))
    (is (= +float-vector+ (aref bytes 0)))
    ;; header is [tag][n][length bytes...]; payload is 1 type byte + 3*4 float bytes
    (let* ((n (aref bytes 1))
           (header-length (+ 2 n))
           (payload-length (decode-length (subseq bytes 2 header-length))))
      (is (= 13 payload-length))
      (is (= (+ header-length 13) (length bytes)))
      (is (= +fv-single-float+ (aref bytes header-length))))))
```

- [ ] **Step 2: Run the test to verify it fails**

```
cd /Users/kraison/work/vivace-graph-v3
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::serialize-suite))'
```

Expected: FAIL — `+FLOAT-VECTOR+` is an undefined variable.

This step also confirms the assumption that `extract-length` needs no change: if `payload-length` reads back as 13, the default branch parsed the header correctly.

- [ ] **Step 3: Add the constants**

In `globals.lisp`, immediately after the `+bignum+` line (`globals.lisp:162`):

```lisp
(alexandria:define-constant +float-vector+ 31)

;; Element type codes for a +float-vector+ payload's first byte.  The byte exists
;; so double-float and int8-quantised vectors can be added later without burning
;; another type tag.
(alexandria:define-constant +fv-single-float+ 1)
```

- [ ] **Step 4: Add the encoder**

In `serialize.lisp`, immediately BEFORE the existing `(defmethod serialize ((v vector)) ...)` at line 221:

```lisp
(defun %serialize-float-vector (v)
  "Encode V, a (simple-array single-float (*)), as one contiguous block: a type
byte followed by DIM little-endian IEEE-754 float32s.  One allocation, versus the
generic vector path's one allocation per element."
  (declare (type (simple-array single-float (*)) v))
  (let* ((dim (length v))
         (payload-length (+ 1 (* 4 dim)))
         (encoded-length (encode-length payload-length))
         (l-of-l (length encoded-length))
         (vec (make-array (+ 1 l-of-l payload-length)
                          :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) +float-vector+)
    (dotimes (i l-of-l)
      (setf (aref vec (+ 1 i)) (aref encoded-length i)))
    (let ((base (+ 1 l-of-l)))
      (setf (aref vec base) +fv-single-float+)
      (dotimes (i dim)
        (let ((bits (ieee-floats:encode-float32 (aref v i)))
              (off (+ base 1 (* 4 i))))
          (dotimes (b 4)
            (setf (aref vec (+ off b)) (ldb (byte 8 (* b 8)) bits))))))
    vec))
```

- [ ] **Step 5: Dispatch to it**

Replace the `if` at the head of `serialize ((v vector))` (`serialize.lisp:222`) with a `cond`. The existing two branches keep their current bodies verbatim; only the dispatch changes:

```lisp
(defmethod serialize ((v vector))
  (cond
    ((equal (array-element-type v) '(unsigned-byte 8))
     ;; ... existing blob branch, unchanged ...
     )
    ;; SUBTYPEP rather than EQUAL: the upgraded element type of a single-float
    ;; array is spelled differently across implementations, and a T-vector
    ;; correctly fails this test.
    ((subtypep (array-element-type v) 'single-float)
     (%serialize-float-vector v))
    (t
     ;; ... existing generic branch, unchanged ...
     )))
```

- [ ] **Step 6: Run the test to verify it passes**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::serialize-suite))'
```

Expected: PASS, no other test regressed.

- [ ] **Step 7: Commit**

```bash
git add globals.lisp serialize.lisp tests/serialize-tests.lisp
git commit -m "feat(serialize): +float-vector+ tag and contiguous float32 encoder"
```

---

### Task 2: The decoder and round trip

**Files:**
- Modify: `serialize.lisp` (add `deserialize-help` method next to `%serialize-float-vector`)
- Test: `tests/serialize-tests.lisp`

**Interfaces:**
- Consumes: `+float-vector+`, `+fv-single-float+`, `ieee-floats:decode-float32`
- Produces: `(deserialize-help (eql +float-vector+) array)` → `(simple-array single-float (*))`

- [ ] **Step 1: Write the failing test**

Append to `tests/serialize-tests.lisp`:

```lisp
(test float-vector-round-trip
  "Float vectors round-trip exactly, preserving element type and dimension."
  (dolist (v (list (fv)
                   (fv 0.0)
                   (fv 1.0 -1.0 0.5 -0.5)
                   (fv 3.14159 -2.71828 1.0e10 -1.0e-10)))
    (let ((back (deserialized v)))
      (is (typep back '(simple-array single-float (*)))
          "round-tripped value has the wrong type: ~S" (type-of back))
      (is (= (length v) (length back)))
      (dotimes (i (length v))
        (is (= (aref v i) (aref back i))
            "element ~A differs: ~A vs ~A" i (aref v i) (aref back i))))))

(test float-vector-extremes
  "Boundary float32 values survive the round trip bit-exactly."
  (let ((v (fv most-positive-single-float most-negative-single-float
               least-positive-single-float least-negative-single-float)))
    (let ((back (deserialized v)))
      (dotimes (i (length v))
        (is (= (aref v i) (aref back i)))))))

(test float-vector-large-dimension
  "A realistic embedding dimension round-trips (exercises multi-byte lengths)."
  (let ((v (make-array 1536 :element-type 'single-float)))
    (dotimes (i 1536)
      (setf (aref v i) (coerce (/ (- i 768) 768.0) 'single-float)))
    (let ((back (deserialized v)))
      (is (= 1536 (length back)))
      (is (every #'= v back)))))

(test float-vector-rejects-misaligned-payload
  "A corrupt payload errors rather than silently decoding to a short vector."
  (let ((bytes (serialize (fv 1.0 2.0))))
    ;; drop one trailing byte: payload is now 8 bytes after the type byte, not 9
    (let ((truncated (subseq bytes 0 (1- (length bytes)))))
      (signals error (deserialize truncated)))))

(test float-vector-nan-and-infinity-behaviour
  "Pin whatever ieee-floats does with non-finite values, so a later change to
that library cannot alter stored data silently. Embeddings must never contain
these -- AS-EMBEDDING rejects them at ingest (Task 4) -- but the codec's
behaviour should still be known rather than assumed."
  (let ((inf #+sbcl sb-ext:single-float-positive-infinity
             #+ecl si:single-float-positive-infinity))
    ;; Record the actual behaviour: either it round-trips or it signals.
    ;; Whichever it is, assert it explicitly here rather than leaving it open.
    (let ((v (make-array 1 :element-type 'single-float :initial-element inf)))
      (handler-case
          (let ((back (deserialized v)))
            (is (= inf (aref back 0))
                "infinity round-tripped to ~A" (aref back 0)))
        (error (e)
          (is t "codec signals on infinity: ~A" (type-of e)))))))

(test generic-vectors-unaffected
  "Non-single-float vectors still take their existing paths."
  (let ((tv (vector 1 "two" :three)))
    (is (equalp tv (deserialized tv))))
  (let ((bv (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3))))
    (is (equalp bv (deserialized bv)))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::serialize-suite))'
```

Expected: `float-vector-round-trip`, `float-vector-extremes`, `float-vector-large-dimension` FAIL with no applicable method for `deserialize-help` on `31`. `generic-vectors-unaffected` should already PASS — if it does not, Task 1 Step 5 broke an existing branch.

- [ ] **Step 3: Write the decoder**

In `serialize.lisp`, immediately after `%serialize-float-vector`:

```lisp
(defmethod deserialize-help ((become (eql +float-vector+)) (bytes array))
  "Decode a contiguous float32 block into a fresh (simple-array single-float (*)).
BYTES is the payload only: a type byte followed by DIM*4 little-endian float32s."
  (declare (type (array (unsigned-byte 8)) bytes))
  (let ((etype (aref bytes 0)))
    (unless (= etype +fv-single-float+)
      (error "unknown float-vector element type ~A" etype))
    ;; Guard the alignment rather than truncating: FLOOR alone would silently
    ;; yield a short vector on a corrupt or misaligned payload, on a codec that
    ;; will outlive everyone's memory of this plan.
    (unless (zerop (mod (- (length bytes) 1) 4))
      (error "float-vector payload is not 4-byte aligned: ~A bytes after the type byte"
             (- (length bytes) 1)))
    (let* ((dim (floor (- (length bytes) 1) 4))
           (v (make-array dim :element-type 'single-float)))
      (dotimes (i dim v)
        (let ((bits 0)
              (off (+ 1 (* 4 i))))
          (dotimes (b 4)
            (setf bits (dpb (aref bytes (+ off b)) (byte 8 (* b 8)) bits)))
          (setf (aref v i) (ieee-floats:decode-float32 bits)))))))
```

- [ ] **Step 4: Run the tests to verify they pass**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::serialize-suite))'
```

Expected: PASS, all four tests.

- [ ] **Step 5: Commit**

```bash
git add serialize.lisp tests/serialize-tests.lisp
git commit -m "feat(serialize): decode +float-vector+ back to a typed single-float array"
```

---

### Task 3: Round trip through a real graph, on both implementations and both backends

**Files:**
- Test: `tests/graph-tests.lisp` (append)

**Interfaces:**
- Consumes: `+float-vector+` encoding from Tasks 1–2; `gdb:def-vertex`, `gdb:with-transaction`, `gdb:make-graph`, `gdb:close-graph`
- Produces: nothing consumed by later tasks — this is the durability gate

- [ ] **Step 1: Write the failing test**

This file declares its schema **once at load time** against `*integration-graph-name*` (see the header comment at `tests/graph-tests.lisp:5`), so the vertex type goes at top level, not inside the test.

First add the type alongside the existing ones, after the `g-employee` form at `tests/graph-tests.lisp:23`:

```lisp
;; For the dense float-vector serialization round trip.
(def-vertex g-embedded ()
  ((payload))
  :graph-db-integration-test)
```

Then append the test, following the `with-temp-directory` idiom used at `tests/graph-tests.lisp:235`:

```lisp
(test float-vector-slot-survives-close-and-reopen
  "A single-float vector stored in a vertex slot reads back bit-exactly after a
close/reopen cycle."
  (with-temp-directory (dir)
    (let ((v (make-array 512 :element-type 'single-float))
          (id nil))
      (dotimes (i 512)
        (setf (aref v i) (coerce (/ i 512.0) 'single-float)))
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setf id (id (make-g-embedded :payload v)))))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((back (slot-value (lookup-vertex id :graph g) 'payload)))
               (is (typep back '(simple-array single-float (*)))
                   "reopened slot has type ~S" (type-of back))
               (is (= 512 (length back)))
               (is (every #'= v back)))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))
```

If `id` or `lookup-vertex` is spelled differently in this file, match the file — do not change the engine to fit the test.

- [ ] **Step 2: Run it to verify it fails**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::graph-suite))'
```

Expected: FAIL. If it fails on fixture/macro names rather than on the assertions, fix the test to match the file's actual idiom before proceeding — do not change the engine.

- [ ] **Step 3: Make it pass**

If Tasks 1–2 are correct this test should pass with **no engine change**. If it does not, the fault is in the codec, not the graph layer — fix `serialize.lisp` rather than special-casing anything in the node path.

- [ ] **Step 4: Verify on ECL**

```
ecl --eval '(ql:quickload :graph-db/test)' \
    --eval '(fiveam:run! (quote graph-db/test::serialize-suite))' \
    --eval '(fiveam:run! (quote graph-db/test::graph-suite))' \
    --eval '(quit)'
```

Expected: PASS. The likely ECL-specific failure is the `subtypep` dispatch in Task 1 Step 5 — if a single-float array falls into the generic branch on ECL, print `(array-element-type v)` there and widen the test to cover what ECL actually reports.

- [ ] **Step 5: Verify under the B+ tree backend**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(setf graph-db:*index-backend* :bplus-tree)' \
     --eval '(fiveam:run! (quote graph-db/test::graph-suite))'
```

Expected: PASS. Serialization is backend-independent, so this is a guard against an unexpected coupling, not an expected failure point.

- [ ] **Step 6: Run the full engine suite for regressions**

```
sbcl --non-interactive \
     --eval '(ql:quickload :graph-db/test)' \
     --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))'
```

Expected: zero failures. Report the actual pass/fail counts — do not claim green without the output.

- [ ] **Step 7: Commit**

```bash
git add tests/graph-tests.lisp
git commit -m "test(serialize): float-vector slot survives close/reopen on both backends"
```

---

### Task 4: cl-llm embeddings become normalised single-float

**Files:**
- Modify: `/Users/kraison/work/cl-llm/rag/embed.lisp:7` (`as-embedding`)
- Modify: `/Users/kraison/work/cl-llm/vivace/schema.lisp` (`ensure-chunk-class`)
- Test: `/Users/kraison/work/cl-llm/tests-rag/embed.lisp`

**Interfaces:**
- Consumes: nothing from earlier tasks (the engine change is what makes this *fast*, not what makes it *work*)
- Produces: `as-embedding (sequence)` → `(simple-array single-float (*))`, L2-normalised; `embedding-norm (v)` → `single-float`

- [ ] **Step 1: Write the failing test**

Append to `/Users/kraison/work/cl-llm/tests-rag/embed.lisp`:

```lisp
(test as-embedding-is-normalised-single-float
  "as-embedding returns a single-float array of unit length."
  (let ((v (rag:as-embedding '(3.0d0 4.0d0))))
    (is (typep v '(simple-array single-float (*))))
    (is (< (abs (- 1.0 (rag:embedding-norm v))) 1e-5))
    ;; 3-4-5 triangle: normalised components are 0.6 and 0.8
    (is (< (abs (- 0.6 (aref v 0))) 1e-5))
    (is (< (abs (- 0.8 (aref v 1))) 1e-5))))

(test as-embedding-zero-vector-is-left-alone
  "A zero vector has no direction; normalising must not divide by zero."
  (let ((v (rag:as-embedding '(0.0 0.0 0.0))))
    (is (typep v '(simple-array single-float (*))))
    (is (every #'zerop v))))
```

- [ ] **Step 2: Run it to verify it fails**

```
cd /Users/kraison/work/cl-llm
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
```

Expected: FAIL — `embedding-norm` undefined, and `as-embedding` returns the wrong element type.

- [ ] **Step 3: Implement**

Replace `as-embedding` in `rag/embed.lisp` and add `embedding-norm`:

```lisp
(defun embedding-norm (v)
  "L2 norm of V."
  (declare (type (simple-array single-float (*)) v))
  (let ((sum 0f0))
    (declare (type single-float sum))
    (dotimes (i (length v) (sqrt sum))
      (incf sum (* (aref v i) (aref v i))))))

(defun as-embedding (sequence)
  "Coerce SEQUENCE to a (simple-array single-float (*)) and L2-normalise it.
Normalising at ingest is what lets cosine similarity reduce to a plain dot
product at query time.  A zero vector has no direction and is returned as-is."
  (let* ((n (length sequence))
         (v (make-array n :element-type 'single-float)))
    (let ((i 0))
      (map nil (lambda (x)
                 (setf (aref v i) (coerce x 'single-float))
                 (incf i))
           sequence))
    (let ((norm (embedding-norm v)))
      ;; A malformed provider response is the realistic source of NaN/Inf.  Reject
      ;; at ingest: a non-finite component poisons the norm, and every downstream
      ;; comparison against it silently returns false, so the vector would rank
      ;; last forever instead of failing.  (NaN /= NaN is the tell.)
      (unless (= norm norm)
        (error 'llm-rag-error
               :message "embedding contains NaN or infinity; refusing to index it"))
      (unless (zerop norm)
        (dotimes (i n)
          (setf (aref v i) (/ (aref v i) norm))))
      v)))
```

Export `embedding-norm` from `rag/packages.lisp` alongside the existing `#:as-embedding`.

- [ ] **Step 4: Change the schema's declared slot type**

In `/Users/kraison/work/cl-llm/vivace/schema.lisp`, inside `ensure-chunk-class`:

```lisp
                  (,(intern "EMBEDDING" :graph-db)
                   :type (simple-array single-float (*)))
```

Update `vertex->chunk`'s docstring in the same file — it currently claims "VG deserialises it as a T-vector", which stops being true once the engine change lands.

- [ ] **Step 5: Run the tests to verify they pass**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cd /Users/kraison/work/cl-llm
git add rag/embed.lisp rag/packages.lisp vivace/schema.lisp tests-rag/embed.lisp
git commit -m "feat(rag): embeddings are normalised single-float"
```

---

### Task 5: Typed dot-product cosine

**Files:**
- Modify: `/Users/kraison/work/cl-llm/rag/store.lisp:18` (`cosine`)
- Test: `/Users/kraison/work/cl-llm/tests-rag/store.lisp`

**Interfaces:**
- Consumes: `as-embedding` from Task 4 (normalised vectors are the precondition)
- Produces: `cosine (a b)` → `single-float`, unchanged signature and semantics

- [ ] **Step 1: Write the failing test**

Append to `/Users/kraison/work/cl-llm/tests-rag/store.lisp`:

```lisp
(test cosine-of-normalised-vectors
  "Cosine of unit vectors: identical = 1, orthogonal = 0, opposed = -1."
  (let ((a (rag:as-embedding '(1.0 0.0)))
        (b (rag:as-embedding '(0.0 1.0)))
        (c (rag:as-embedding '(-1.0 0.0))))
    (is (< (abs (- 1.0 (rag:cosine a a))) 1e-5))
    (is (< (abs (rag:cosine a b)) 1e-5))
    (is (< (abs (- -1.0 (rag:cosine a c))) 1e-5))))

(test cosine-returns-single-float
  "Scoring stays in single-float; no boxing to double."
  (let ((a (rag:as-embedding '(1.0 2.0 3.0))))
    (is (typep (rag:cosine a a) 'single-float))))
```

- [ ] **Step 2: Run it to verify it fails**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
```

Expected: `cosine-returns-single-float` FAILS — the current implementation accumulates in `0d0` and returns a double.

- [ ] **Step 3: Implement**

Replace `cosine` in `rag/store.lisp`:

```lisp
(declaim (inline dot))
(defun dot (a b)
  "Dot product of two equal-length single-float vectors."
  (declare (type (simple-array single-float (*)) a b)
           (optimize (speed 3) (safety 1)))
  (let ((sum 0f0))
    (declare (type single-float sum))
    (dotimes (i (length a) sum)
      (incf sum (* (aref a i) (aref b i))))))

(defun cosine (a b)
  "Cosine similarity of two embedding vectors, 0 on a zero-norm vector.
Embeddings are L2-normalised at ingest (AS-EMBEDDING), so this is a plain dot
product -- no per-candidate norm recomputation, no sqrt."
  (declare (type (simple-array single-float (*)) a b))
  (if (or (zerop (length a)) (zerop (length b)))
      0f0
      (dot a b)))
```

Export `dot` from `rag/packages.lisp`; Task 6 uses it.

- [ ] **Step 4: Run the tests to verify they pass**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
```

Expected: PASS, including the pre-existing retrieval-ranking tests. Those tests are the real check that dropping norm recomputation did not change rankings.

- [ ] **Step 5: Commit**

```bash
git add rag/store.lisp rag/packages.lisp tests-rag/store.lisp
git commit -m "perf(rag): cosine is a typed dot product over normalised vectors"
```

---

### Task 6: Migrate stored embeddings to normalised single-float

**Files:**
- Modify: `/Users/kraison/work/cl-llm/vivace/store.lisp` (`hydrate` on both store classes)
- Test: `/Users/kraison/work/cl-llm/tests-vivace/store-scan.lisp`

**Interfaces:**
- Consumes: `as-embedding`, `embedding-norm` from Task 4; the `+float-vector+` codec from Tasks 1–3
- Produces: `migrate-embeddings (store)` → integer count of rewritten chunks; `*embedding-migration-policy*` → one of `:migrate`, `:error`

**Why this task exists.** Two independent things change under Phase 1, and *both* leave already-stored data behind:

1. **Element type.** Existing chunks were serialised through the generic vector path as a T-vector of boxed `double-float`s. The new `+float-vector+` tag only affects *new* writes — old rows still decode as T-vectors. Task 7 scores `%slot` directly with a `single-float` declaration, which is wrong for those rows.
2. **Normalisation semantics.** Today `cosine` self-normalises, so a non-unit stored vector ranks correctly. After Task 5 it is a bare dot product, correct only if every stored vector is unit-norm. Old data reads back perfectly and **ranks wrong**, with nothing logged.

The engine's "existing serialized data must still read" constraint covers the codec and misses both of these. Reading correctly is not the same as meaning the same thing.

Empirically the live corpus is *probably* fine on point 2 — bge-m3 returns unit vectors (sampled norms 0.999999912, 0.999999939, 1.000000261) — but that is a property of one embedder, not of the design. Point 1 affects it regardless.

- [ ] **Step 1: Write the failing test**

Append to `/Users/kraison/work/cl-llm/tests-vivace/store-scan.lisp`:

```lisp
(test migrates-legacy-double-t-vector-embeddings
  "A chunk stored as a T-vector of doubles is rewritten to a normalised
single-float array on hydrate."
  (with-test-graph (g)                     ; match this file's existing fixture
    (let ((legacy (vector 3.0d0 4.0d0)))   ; unnormalised, boxed, T-vector
      (let ((gdb:*graph* g))
        (gdb:with-transaction ()
          (funcall (chunk-constructor 'rag-chunk)
                   :text "legacy" :document-id "doc-legacy"
                   :metadata nil :embedding legacy :graph g)))
      (let ((store (make-graph-store g :strategy :scan)))
        (declare (ignore store))
        (let ((v (%slot (first (graph-store-vertices-for-test g)) "EMBEDDING")))
          (is (typep v '(simple-array single-float (*)))
              "embedding was not migrated: ~S" (type-of v))
          (is (< (abs (- 1.0 (rag:embedding-norm v))) 1e-5)
              "embedding was not normalised"))))))

(test migration-policy-error-refuses-instead-of-migrating
  "With :error policy, a legacy store signals rather than silently rewriting."
  (with-test-graph (g)
    (let ((cl-llm.rag.vivace::*embedding-migration-policy* :error))
      (let ((gdb:*graph* g))
        (gdb:with-transaction ()
          (funcall (chunk-constructor 'rag-chunk)
                   :text "legacy" :document-id "doc-legacy"
                   :metadata nil :embedding (vector 3.0d0 4.0d0) :graph g)))
      (signals rag:llm-rag-error (make-graph-store g :strategy :scan)))))
```

If `graph-store-vertices-for-test` does not exist, read the chunk back through `map-chunk-vertices` instead — do not add a test-only accessor to production code.

- [ ] **Step 2: Run it to verify it fails**

```
cd /Users/kraison/work/cl-llm
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/vivace/tests)' \
     --eval '(fiveam:run! :cl-llm-rag-vivace)'
```

Expected: FAIL — `*embedding-migration-policy*` undefined; embeddings come back as T-vectors.

- [ ] **Step 3: Implement**

Add to `/Users/kraison/work/cl-llm/vivace/store.lisp`:

```lisp
(defparameter *embedding-migration-policy* :migrate
  "What HYDRATE does with stored embeddings that are not already normalised
single-float arrays.  :MIGRATE rewrites them in place (default).  :ERROR refuses
to open the store.  There is deliberately no :IGNORE -- scoring after Phase 1 is
a bare dot product, so an unnormalised stored vector ranks WRONG rather than
merely slow, and a silent wrong answer is the failure mode this guards.")

(defun %needs-migration-p (e)
  (or (not (typep e '(simple-array single-float (*))))
      (> (abs (- 1.0 (rag:embedding-norm (rag:as-embedding e)))) 1e-4)))

(defun migrate-embeddings (store)
  "Rewrite any stored embedding that is not already a normalised single-float
array.  Returns the number of chunks rewritten.  Collect victims first, then
write in one transaction -- do NOT mutate while map-vertices iterates."
  (let ((victims '()))
    (map-chunk-vertices
     store
     (lambda (vertex)
       (when (%needs-migration-p (%slot vertex "EMBEDDING"))
         (push vertex victims))))
    (when victims
      (ecase *embedding-migration-policy*
        (:error
         (error 'rag:llm-rag-error
                :message (format nil "~a stored embeddings are not normalised ~
                                      single-float vectors; scoring would rank ~
                                      them incorrectly. Re-embed, or set ~
                                      *embedding-migration-policy* to :migrate."
                                 (length victims))))
        (:migrate
         (let ((gdb:*graph* (graph-store-graph store)))
           (gdb:with-transaction ()
             (dolist (v victims)
               (setf (slot-value v (intern "EMBEDDING" :graph-db))
                     (rag:as-embedding (%slot v "EMBEDDING")))))))))
    (length victims)))
```

Call `migrate-embeddings` at the top of both `hydrate` methods, before either reads a dimension.

- [ ] **Step 4: Run the tests to verify they pass**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/vivace/tests)' \
     --eval '(fiveam:run! :cl-llm-rag-vivace)'
```

Expected: PASS, and the pre-existing `store-cache` tests still green.

- [ ] **Step 5: Commit**

```bash
git add vivace/store.lisp tests-vivace/store-scan.lisp
git commit -m "feat(rag): migrate stored embeddings to normalised single-float on open"
```

---

### Task 7: Bounded top-k heap

**Files:**
- Modify: `/Users/kraison/work/cl-llm/rag/store.lisp` (`store-search` on `memory-store`)
- Modify: `/Users/kraison/work/cl-llm/vivace/store.lisp:89-102` (`store-search` on `scan-graph-store`)
- Test: `/Users/kraison/work/cl-llm/tests-rag/store.lisp`, `/Users/kraison/work/cl-llm/tests-vivace/store-scan.lisp`

**Interfaces:**
- Consumes: `cosine` from Task 5; the total order defined by `hit<` (`rag/store.lisp:76`)
- Produces: `top-k-collector (k)` → collector struct; `rank-before-p (s1 t1 s2 t2)` → boolean; `collect-candidate (collector score tiebreak payload)`; `collector-results (collector)` → list of `(score . payload)` conses, best-ranked first

- [ ] **Step 1: Write the failing test**

Append to `/Users/kraison/work/cl-llm/tests-rag/store.lisp`:

```lisp
(test top-k-collector-keeps-the-best-k
  "A bounded collector returns exactly the k highest scores, best first."
  (let ((c (rag::top-k-collector 3)))
    (dolist (row '((0.1 "a" :a) (0.9 "b" :b) (0.5 "c" :c) (0.7 "d" :d) (0.2 "e" :e)))
      (rag::collect-candidate c (coerce (first row) 'single-float)
                              (second row) (third row)))
    (is (equal '(:b :d :c) (mapcar #'cdr (rag::collector-results c))))))

(test top-k-collector-handles-fewer-than-k
  "Fewer candidates than k returns all of them, still ordered."
  (let ((c (rag::top-k-collector 5)))
    (rag::collect-candidate c 0.2f0 "x" :x)
    (rag::collect-candidate c 0.8f0 "y" :y)
    (is (equal '(:y :x) (mapcar #'cdr (rag::collector-results c))))))

(test top-k-collector-tie-break-is-order-independent
  "A tie at the k-th boundary resolves by document-id, not by insertion order.
This is the regression that keeps scan and cache stores agreeing: they iterate
in different orders, so an order-dependent eviction would make them differ."
  (flet ((collect-in (rows)
           (let ((c (rag::top-k-collector 2)))
             (dolist (row rows)
               (rag::collect-candidate c (coerce (first row) 'single-float)
                                       (second row) (third row)))
             (mapcar #'cdr (rag::collector-results c)))))
    ;; three candidates, two tied at 0.5; k=2 must keep 0.9 and the tied
    ;; candidate with the smaller document-id, whichever order they arrive in.
    (let ((forward  (collect-in '((0.9 "a" :top) (0.5 "b" :b) (0.5 "c" :c))))
          (backward (collect-in '((0.5 "c" :c) (0.5 "b" :b) (0.9 "a" :top)))))
      (is (equal '(:top :b) forward))
      (is (equal forward backward)
          "eviction depends on insertion order: ~S vs ~S" forward backward))))

(test search-matches-brute-force
  "Heap-based search agrees with a full sort on the same corpus."
  (let ((store (rag:make-memory-store))
        (chunks '()))
    (dotimes (i 50)
      (push (rag:make-chunk (format nil "chunk ~A" i)
                            :document-id (format nil "doc-~A" i)
                            :embedding (rag:as-embedding
                                        (list (coerce (sin i) 'single-float)
                                              (coerce (cos i) 'single-float))))
            chunks))
    (rag:store-add store chunks)
    (let* ((q (rag:as-embedding '(1.0 0.0)))
           (heap-hits (rag:store-search store q 5))
           ;; Reference must use the SAME total order as the collector
           ;; (score DESC, document-id ASC).  Sorting by score alone would make
           ;; this test blind to exactly the tie-break regression Task 7 guards.
           (brute (subseq (sort (mapcar (lambda (c)
                                          (cons (rag:cosine q (rag:chunk-embedding c)) c))
                                        chunks)
                                (lambda (a b)
                                  (rag::rank-before-p
                                   (car a) (or (rag:chunk-document-id (cdr a)) "")
                                   (car b) (or (rag:chunk-document-id (cdr b)) ""))))
                          0 5)))
      (is (= 5 (length heap-hits)))
      (loop for hit in heap-hits
            for ref in brute
            do (is (< (abs (- (rag:hit-score hit) (car ref))) 1e-5))))))
```

- [ ] **Step 2: Run it to verify it fails**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'
```

Expected: FAIL — `top-k-collector` undefined.

- [ ] **Step 3: Implement the collector**

Add to `rag/store.lisp`, above `memory-store`:

**Critical:** eviction must use the *same* total order as `hit<` (`rag/store.lisp:76`), score **and** document-id. Ranking by score alone would make the retained set depend on iteration order, and scan and cache stores iterate differently — which is precisely the disagreement `hit<` exists to prevent. A tie at the k-th boundary would then produce different results from the two stores for the same corpus and query.

```lisp
;;; A bounded top-k collector.  The point is to never materialise a hit per
;;; corpus entry: at 1M chunks the old build-everything-then-stable-sort path
;;; conses 1M objects and sorts them to keep 5.  K is small, so a linear scan of
;;; the k-element buffer beats the bookkeeping of a real heap.
;;;
;;; Cost is O(k) per candidate once full (%TOP-K-WORST-INDEX rescans the buffer).
;;; That is the right trade at the k values we actually use -- production k is 8
;;; -- but K IS CALLER-SUPPLIED.  A real binary heap becomes worth its bookkeeping
;;; somewhere around k >= 64; past that, most candidates fail a single compare
;;; against the root instead of scanning k slots.  Revisit if a caller passes a
;;; large k, which today none does.
;;;
;;; Ordering is (score DESC, tiebreak ASC) -- the same total order as HIT<.  The
;;; tiebreak MUST be carried through eviction, not applied only at the end:
;;; scan and cache stores iterate in different orders, so an order-dependent
;;; eviction at the k-th boundary would make them disagree on a tie.

(defstruct (top-k (:constructor %make-top-k))
  (k 0 :type fixnum)
  (count 0 :type fixnum)
  (scores nil :type (or null (simple-array single-float (*))))
  (tiebreaks nil :type (or null simple-vector))
  (payloads nil :type (or null simple-vector)))

(defun top-k-collector (k)
  (%make-top-k :k k
               :scores (make-array (max k 1) :element-type 'single-float)
               :tiebreaks (make-array (max k 1) :initial-element "")
               :payloads (make-array (max k 1) :initial-element nil)))

(defun rank-before-p (s1 t1 s2 t2)
  "True when (S1,T1) ranks ahead of (S2,T2): higher score first, ties by
tiebreak ascending.  The same total order as HIT<."
  (declare (type single-float s1 s2))
  (cond ((> s1 s2) t)
        ((< s1 s2) nil)
        (t (string< t1 t2))))

(defun %top-k-worst-index (c)
  "Index of the slot that ranks LAST under RANK-BEFORE-P."
  (let ((scores (top-k-scores c))
        (tiebreaks (top-k-tiebreaks c))
        (worst 0))
    (declare (type (simple-array single-float (*)) scores))
    (dotimes (i (top-k-count c) worst)
      (when (rank-before-p (aref scores worst) (aref tiebreaks worst)
                           (aref scores i) (aref tiebreaks i))
        (setf worst i)))))

(defun collect-candidate (c score tiebreak payload)
  "Offer a candidate to the collector; keep it only if it outranks the current worst.
TIEBREAK is the candidate's document-id (or \"\" when it has none)."
  (declare (type single-float score))
  (let ((scores (top-k-scores c))
        (tiebreaks (top-k-tiebreaks c))
        (payloads (top-k-payloads c))
        (tb (or tiebreak "")))
    (cond ((< (top-k-count c) (top-k-k c))
           (setf (aref scores (top-k-count c)) score
                 (aref tiebreaks (top-k-count c)) tb
                 (aref payloads (top-k-count c)) payload)
           (incf (top-k-count c)))
          (t
           (let ((worst (%top-k-worst-index c)))
             (when (rank-before-p score tb
                                  (aref scores worst) (aref tiebreaks worst))
               (setf (aref scores worst) score
                     (aref tiebreaks worst) tb
                     (aref payloads worst) payload))))))
  c)

(defun collector-results (c)
  "The retained candidates as (score . payload) conses, best-ranked first."
  (let ((out '()))
    (dotimes (i (top-k-count c))
      (push (list (aref (top-k-scores c) i)
                  (aref (top-k-tiebreaks c) i)
                  (aref (top-k-payloads c) i))
            out))
    (mapcar (lambda (row) (cons (first row) (third row)))
            (sort out (lambda (a b)
                        (rank-before-p (first a) (second a)
                                       (first b) (second b)))))))
```

- [ ] **Step 4: Use it in the memory store**

Replace `store-search` on `memory-store` (`rag/store.lisp:86`) in full:

```lisp
(defmethod store-search ((store memory-store) query-vector k)
  (when (plusp (store-count store))
    (check-dimension store query-vector))
  ;; Bounded collection: one hit per SURVIVOR, not one per corpus entry.
  (let ((c (top-k-collector k)))
    (loop for chunk across (store-chunks store)
          do (collect-candidate c
                                (cosine query-vector (chunk-embedding chunk))
                                (or (chunk-document-id chunk) "")
                                chunk))
    (mapcar (lambda (pair) (make-hit (cdr pair) (car pair)))
            (collector-results c))))
```

`hit<` stays in the file — `dense-preserving-fusion` and the vivace store still reference it.

- [ ] **Step 5: Use it in the graph store**

Replace `store-search` on `scan-graph-store` (`vivace/store.lisp:89`):

```lisp
(defmethod rag:store-search ((store scan-graph-store) query-vector k)
  (when (and (graph-store-dimension store)
             (/= (length query-vector) (graph-store-dimension store)))
    (error 'rag:llm-rag-error
           :message (format nil "query dimension ~a does not match store dimension ~a"
                            (length query-vector) (graph-store-dimension store))))
  ;; Score against the embedding slot only, and build a rag:chunk ONLY for the
  ;; survivors.  vertex->chunk per candidate was the dominant cost: it rebuilt
  ;; text and metadata for every chunk in the corpus to rank five of them.
  (let ((collector (rag::top-k-collector k)))
    (map-chunk-vertices
     store
     (lambda (vertex)
       ;; Score the slot value DIRECTLY.  Do NOT call as-embedding here: after
       ;; Task 4 it allocates and L2-normalises, which would put one allocation,
       ;; one sqrt and DIM divisions in the per-candidate inner loop -- exactly
       ;; the cost Tasks 1-3 exist to remove.  Task 6's migration guarantees the
       ;; slot is already a normalised (simple-array single-float (*)).
       (let ((e (%slot vertex "EMBEDDING")))
         (declare (type (simple-array single-float (*)) e))
         (rag::collect-candidate collector
                                 (rag:cosine query-vector e)
                                 (or (%slot vertex "DOCUMENT-ID") "")
                                 vertex))))
    (mapcar (lambda (pair)
              (rag:make-hit (vertex->chunk (cdr pair)) (car pair)))
            (rag::collector-results collector))))
```

- [ ] **Step 6: Run both suites to verify they pass**

```
sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/tests)' \
     --eval '(fiveam:run! (find-symbol "CL-LLM-RAG-SUITE" :cl-llm.rag.test))'

sbcl --non-interactive \
     --eval '(ql:quickload :cl-llm/rag/vivace/tests)' \
     --eval '(fiveam:run! :cl-llm-rag-vivace)'
```

Expected: PASS. The pre-existing `store-scan` and `store-cache` tests are the real gate — scan and cache stores iterate in different orders and must still agree on ties (`hit<`, `vivace/store.lisp:79`).

- [ ] **Step 7: Commit**

```bash
git add rag/store.lisp rag/packages.lisp vivace/store.lisp \
        tests-rag/store.lisp tests-vivace/store-scan.lisp
git commit -m "perf(rag): bounded top-k search; build chunks only for survivors"
```

---

## Done Criteria

- [ ] `graph-db-suite` green on SBCL, with actual counts reported
- [ ] `serialize-suite` and `graph-suite` green on ECL, with actual counts reported
- [ ] `graph-suite` green under `:bplus-tree`
- [ ] `cl-llm-rag-suite` and `cl-llm-rag-vivace` green
- [ ] A 1536-dim embedding round-trips through a closed and reopened graph bit-exactly
- [ ] No existing serialization tag's encoding changed
- [ ] A store written **before** this change opens, migrates, and returns the same ranking it did before — verified against a corpus written by the pre-change code, not a fixture authored after it
- [ ] `store-search` scores the slot value directly: no `as-embedding`, no allocation, no `sqrt` in the per-candidate loop
- [ ] `:error` migration policy refuses a non-conforming store instead of mis-ranking it
