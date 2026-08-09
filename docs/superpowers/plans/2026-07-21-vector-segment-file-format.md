# Vector Segment File Format (Phase 2, Step 2) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A standalone, mmap-backed on-disk vector segment in the graph-db engine — header, id array with a threaded free list, and a contiguous single-float vector block — with create/open/put/get/remove and an in-RAM id→slot map rebuilt at open.

**Architecture:** A new `segment.lisp` builds directly on the engine's existing stable-address `mapped-file` (`mmap.lisp`): `mmap-file` reserves a virtual-address window and maps the file into its head with `MAP_FIXED`; `extend-mapped-file` grows in place without moving the base pointer. The segment is a fixed-layout region over one `mapped-file`, storing vectors by slot index, with an id→slot hash held only in RAM and reconstructed by sweeping the on-disk id array — which is authoritative.

**Tech Stack:** Common Lisp (SBCL), ASDF, CFFI/mmap (`mmap.lisp`), FiveAM.

## Global Constraints

- **SBCL only. ECL is out of scope for this project** — do not run it, do not add ECL reader conditionals.
- **Lisp indentation is spaces only, never tabs.**
- **This is Step 2 of 5. Build ONLY the file format and its unit operations.** No transaction hooks, no rebuild-from-nodes, no `map-vertices` — those are Step 3. No `segment-scan` / `segment-score-subset` — those are Step 4. If you find yourself editing `transactions.lisp`, `vertex.lisp`, or writing a scan/score function, STOP: it is out of scope.
- **Reuse the engine's mmap primitives; do not invent a new mmap layer.** `mmap-file`, `extend-mapped-file`, `get-byte`/`set-byte`, `get-bytes`/`set-bytes`, `serialize-uint64`/`deserialize-uint64`, `mapped-file-length` all already exist in `mmap.lisp` and operate on a `mapped-file`. Use them.
- **The on-disk id array is authoritative; the id→slot map is derived** (spec §5.1). Persist the id array and header; never persist the hash.
- **Dimension is fixed per segment** (spec §5), set at create time, validated on every write; a mismatch **signals**.
- **Growth uses the stable-address mmap** (spec §5): a growing segment must never move the base pointer or invalidate an in-flight read. `extend-mapped-file` provides this; a test must exercise growth across the initial mapping boundary.

## Spec references

- §5 on-disk layout (header / id array / vector block; free list; no compaction; slot order meaningless)
- §5.1 id→slot map is derived, rebuilt at open by sweeping the id array
- §6 dimension mismatch signals (the write-path rule; the transaction wiring is Step 3, not here)

## On-disk layout (fixed constants)

```
HEADER (fixed, at offset 0), each field a uint64 (8 bytes) unless noted:
  0   magic            +segment-magic+  (a recognisable constant)
  8   format-version   +segment-format+ (1)
  16  dimension        D
  24  element-type     +fv-single-float+ (1)   ; reserved for int8 later
  32  capacity         C  (number of slots allocated)
  40  live-count       number of occupied slots
  48  free-head        slot index of the free-list head, or +no-slot+
  56  (reserved, zero)
  64  END OF HEADER

ID ARRAY (at +segment-id-array-offset+ = 64):
  slot i occupies [64 + i*16, 64 + (i+1)*16)   ; 16 bytes = +key-bytes+
  an OCCUPIED slot holds the node's 16-byte id
  a FREE slot holds +free-slot-marker+ in its first 8 bytes and the next
    free slot index in its second 8 bytes (the threaded free list)

VECTOR BLOCK (at +segment-id-array-offset+ + capacity*16):
  slot i occupies [vblock + i*D*4, vblock + (i+1)*D*4)   ; D single-floats
```

The vector block's start depends on `capacity`, so **growing capacity moves the vector block** — see Task 5. This is the one real subtlety in the format; the plan handles it by rewriting rather than pretending the block is at a fixed offset.

## File Structure

| file | responsibility | change |
|---|---|---|
| `segment.lisp` | the segment struct, layout constants, create/open/put/get/remove, id→slot rebuild | create |
| `globals.lisp` | segment layout constants | add a block |
| `graph-db.asd` | load order + test registration | add `segment` after `allocator`; add `segment-tests` |
| `tests/segment-tests.lisp` | unit tests | create |

`segment.lisp` depends only on `mmap` and `globals` (via `serialize`/`allocator` already in the chain). Place it in the `.asd` after `allocator` (it uses `serialize-uint64`, defined there) and before `spatial-index`.

---

### Task 1: Layout constants and the segment struct

**Files:**
- Modify: `globals.lisp` (after the `+float-vector+` block near line 168)
- Create: `segment.lisp`
- Modify: `graph-db.asd` (register `segment` after `allocator`)

**Interfaces:**
- Consumes: `+key-bytes+` (16), `+fv-single-float+` (1) from `globals.lisp`
- Produces: constants `+segment-magic+`, `+segment-format+`, `+segment-header-bytes+`, `+segment-id-array-offset+`, `+free-slot-marker+`, `+no-slot+`; struct `vector-segment` with accessors `segment-mmap`, `segment-dimension`, `segment-id->slot`

- [ ] **Step 1: Add the constants**

In `globals.lisp`, immediately after the `+fv-single-float+` line:

```lisp
;; --- Vector segment (Phase 2) on-disk layout ---------------------------------
;; A segment is a derived, mmap-backed index: one fixed-width single-float vector
;; per node, addressable by node id.  See docs/superpowers/specs/
;; 2026-07-20-vector-segments-design.md sec 5.
(alexandria:define-constant +segment-magic+ #x5647534547 ) ; "VGSEG" as bytes
(alexandria:define-constant +segment-format+ 1)
(alexandria:define-constant +segment-header-bytes+ 64)
(alexandria:define-constant +segment-id-array-offset+ 64)
;; A free slot's id-array cell holds this marker in its first 8 bytes; its second
;; 8 bytes hold the next free slot index.  Occupied cells hold a real 16-byte id,
;; whose first 8 bytes are never this value (ids are uuids; see note in
;; segment.lisp on why the marker is safe).
(alexandria:define-constant +free-slot-marker+ #xFFFFFFFFFFFFFFFF)
;; Sentinel "no slot" index -- terminates the free list and marks "id not found".
(alexandria:define-constant +no-slot+ #xFFFFFFFFFFFFFFFF)
```

- [ ] **Step 2: Create the struct and package the file**

`segment.lisp`:

```lisp
(in-package :graph-db)

;;; Vector segment: a derived, mmap-backed index holding one fixed-width
;;; single-float vector per node, addressable by node id.  See
;;; docs/superpowers/specs/2026-07-20-vector-segments-design.md sec 5.
;;;
;;; This file is the FILE FORMAT and its unit operations ONLY.  Transaction
;;; hooks, rebuild-from-nodes, and scan/score are later steps.
;;;
;;; The on-disk id array is authoritative.  ID->SLOT is a RAM-only hash rebuilt
;;; at open by sweeping it (sec 5.1); it is never persisted.

(defstruct (vector-segment (:constructor %make-vector-segment)
                           (:predicate vector-segment-p))
  (mmap nil)                 ; a mapped-file (mmap.lisp)
  (dimension 0 :type fixnum) ; fixed at create time
  (id->slot nil))            ; equalp hash: 16-byte id vector -> slot index
```

- [ ] **Step 3: Register in the .asd**

In `graph-db.asd`, add after the `allocator` component (line 59):

```lisp
               (:file "segment" :depends-on ("allocator" "mmap"))
```

- [ ] **Step 4: Verify it loads**

```
cd /Users/kraison/work/vivace-graph-v3
sbcl --non-interactive --eval '(ql:quickload :graph-db)' --eval '(format t "~&LOADS: ~a~%" (fboundp (quote graph-db::%make-vector-segment)))'
```

Expected: `LOADS: T`, no compile errors.

- [ ] **Step 5: Commit**

```bash
git add globals.lisp segment.lisp graph-db.asd
git commit -m "feat(segment): layout constants and the vector-segment struct"
```

---

### Task 2: Create and open — header round-trip

**Files:**
- Modify: `segment.lisp`
- Modify: `graph-db.asd` (register `segment-tests`)
- Create: `tests/segment-tests.lisp`

**Interfaces:**
- Consumes: `mmap-file`, `serialize-uint64`, `deserialize-uint64`, `mapped-file-length` (`mmap.lisp`); the constants from Task 1
- Produces:
  - `create-vector-segment (path dimension &key initial-capacity)` → `vector-segment`
  - `open-vector-segment (path)` → `vector-segment`
  - `close-vector-segment (segment)` → `nil`
  - `segment-capacity (segment)` → fixnum; `segment-live-count (segment)` → fixnum

- [ ] **Step 1: Write the failing test**

Create `tests/segment-tests.lisp`:

```lisp
;;;; Tests for the mmap-backed vector segment file format (segment.lisp).

(in-package #:graph-db/test)

(def-suite segment-suite
  :description "vector segment: create/open/put/get/remove, header, free list, growth."
  :in graph-db-suite)

(in-suite segment-suite)

(defun %seg-path ()
  (format nil "/var/tmp/vgseg-~a.dat" (get-internal-real-time)))

(test segment-create-and-reopen-header
  "A created segment's header (dimension, capacity, live-count) survives close and reopen."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 128 :initial-capacity 10)))
             (is (= 128 (segment-dimension s)))
             (is (= 10 (segment-capacity s)))
             (is (= 0 (segment-live-count s)))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (progn
                    (is (= 128 (segment-dimension s)))
                    (is (= 10 (segment-capacity s)))
                    (is (= 0 (segment-live-count s))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))
```

Register the test file in `graph-db.asd` after the `spatial-index-tests` line in the `graph-db/test` system:

```lisp
               (:file "segment-tests")
```

- [ ] **Step 2: Run it to verify it fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: FAIL — `create-vector-segment` undefined.

- [ ] **Step 3: Implement create/open/close**

Append to `segment.lisp`:

```lisp
(defun %seg-write-header (mmap &key magic format dimension element-type
                                    capacity live-count free-head)
  (serialize-uint64 mmap magic 0)
  (serialize-uint64 mmap format 8)
  (serialize-uint64 mmap dimension 16)
  (serialize-uint64 mmap element-type 24)
  (serialize-uint64 mmap capacity 32)
  (serialize-uint64 mmap live-count 40)
  (serialize-uint64 mmap free-head 48)
  (serialize-uint64 mmap 0 56))

(defun %seg-vblock-offset (capacity)
  "Byte offset of the vector block for CAPACITY slots."
  (+ +segment-id-array-offset+ (* capacity +key-bytes+)))

(defun %seg-file-bytes (capacity dimension)
  "Total bytes a segment file needs for CAPACITY slots of DIMENSION."
  (+ (%seg-vblock-offset capacity) (* capacity dimension 4)))

(defun create-vector-segment (path dimension &key (initial-capacity 1024))
  "Create a new vector segment at PATH holding DIMENSION-wide single-float
vectors, with room for INITIAL-CAPACITY slots.  DIMENSION is fixed for the life
of the segment.  Returns an open VECTOR-SEGMENT."
  (check-type dimension (integer 1))
  (check-type initial-capacity (integer 1))
  (let* ((bytes (%seg-file-bytes initial-capacity dimension))
         (mmap (mmap-file path :create-p t :size bytes)))
    (%seg-write-header mmap
                       :magic +segment-magic+
                       :format +segment-format+
                       :dimension dimension
                       :element-type +fv-single-float+
                       :capacity initial-capacity
                       :live-count 0
                       :free-head +no-slot+)
    (%make-vector-segment :mmap mmap
                          :dimension dimension
                          :id->slot (make-hash-table :test 'equalp))))

(defun segment-capacity (segment)
  (deserialize-uint64 (segment-mmap segment) 32))

(defun segment-live-count (segment)
  (deserialize-uint64 (segment-mmap segment) 40))

(defun %seg-free-head (segment)
  (deserialize-uint64 (segment-mmap segment) 48))

(defun open-vector-segment (path)
  "Open an existing vector segment at PATH.  Validates magic and format, reads
the header, and rebuilds the RAM id->slot map by sweeping the id array (the
on-disk id array is authoritative; the map is never persisted)."
  (let ((mmap (mmap-file path :create-p nil)))
    (let ((magic (deserialize-uint64 mmap 0))
          (format (deserialize-uint64 mmap 8)))
      (unless (= magic +segment-magic+)
        (error "~A is not a vector segment (magic ~X)" path magic))
      (unless (= format +segment-format+)
        (error "vector segment ~A is format ~D, expected ~D"
               path format +segment-format+)))
    (let ((segment (%make-vector-segment
                    :mmap mmap
                    :dimension (deserialize-uint64 mmap 16)
                    :id->slot (make-hash-table :test 'equalp))))
      (%seg-rebuild-id->slot segment)
      segment)))

(defun close-vector-segment (segment)
  "Release the segment's mmap."
  (when (segment-mmap segment)
    (munmap-file (segment-mmap segment))
    (setf (segment-mmap segment) nil))
  nil)
```

Note: `%seg-rebuild-id->slot` is defined in Task 4. For Task 2 it must at least exist so `open` compiles — add a stub that does nothing yet, and replace it in Task 4:

```lisp
(defun %seg-rebuild-id->slot (segment)
  ;; Filled in in a later task; the header round-trip test does not exercise it.
  (declare (ignore segment))
  nil)
```

Confirm `munmap-file` is the correct engine name for releasing a `mapped-file`; if the engine spells it differently (e.g. `munmap` or a method on `mapped-file`), use that spelling and note it in your report.

- [ ] **Step 4: Run it to verify it passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add segment.lisp graph-db.asd tests/segment-tests.lisp
git commit -m "feat(segment): create/open/close with a persisted header"
```

---

### Task 3: put and get — vectors by id, dimension validation

**Files:**
- Modify: `segment.lisp`
- Modify: `tests/segment-tests.lisp`

**Interfaces:**
- Consumes: `create-vector-segment`, `segment-capacity`, `segment-live-count` (Task 2); `set-bytes`, `get-bytes`, `serialize-uint64`, `deserialize-uint64` (`mmap.lisp`); the `+float-vector+` codec's single-float byte packing (see `%serialize-float-vector` in `serialize.lisp` for the exact little-endian layout to match)
- Produces:
  - `segment-put (segment id vector)` → slot index — store VECTOR under the 16-byte ID; overwrite if ID already present, else take a free slot or extend
  - `segment-get (segment id)` → `(simple-array single-float (*))` or `nil`
  - `%seg-slot-of (segment id)` → slot index or `nil`

- [ ] **Step 1: Write the failing test**

Append to `tests/segment-tests.lisp`:

```lisp
(defun %id (n)
  "A 16-byte id whose bytes encode N (distinct ids for distinct N)."
  (let ((v (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 8 v)
      (setf (aref v i) (ldb (byte 8 (* i 8)) n)))))

(defun %vec (dim &optional (base 0.0))
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v)
      (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(test segment-put-get-roundtrip
  "A stored vector reads back bit-exactly by id; a missing id returns nil."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 64 :initial-capacity 8)))
           (unwind-protect
                (let ((v (%vec 64 1.0)))
                  (segment-put s (%id 1) v)
                  (is (= 1 (segment-live-count s)))
                  (let ((back (segment-get s (%id 1))))
                    (is (typep back '(simple-array single-float (*))))
                    (is (= 64 (length back)))
                    (is (every #'= v back)))
                  (is (null (segment-get s (%id 999)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-put-overwrites-in-place
  "Putting the same id twice overwrites and does not grow live-count."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 32 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%id 7) (%vec 32 1.0))
                  (segment-put s (%id 7) (%vec 32 5.0))
                  (is (= 1 (segment-live-count s)))
                  (is (every #'= (%vec 32 5.0) (segment-get s (%id 7)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-put-rejects-wrong-dimension
  "A vector whose length is not the segment's dimension signals."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 16 :initial-capacity 4)))
           (unwind-protect
                (signals error (segment-put s (%id 1) (%vec 17)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-put-rejects-all-ones-id
  "An id whose first 8 bytes are all-ones collides with the free-slot marker
and must be rejected, not stored (else a reopen would misread it as free)."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 16 :initial-capacity 4))
               (bad (make-array 16 :element-type '(unsigned-byte 8)
                                   :initial-element #xFF)))
           (unwind-protect
                (signals error (segment-put s bad (%vec 16)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-values-survive-reopen
  "Stored vectors read back after close and reopen (persistence + id->slot rebuild)."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 48 :initial-capacity 8)))
             (segment-put s (%id 1) (%vec 48 1.0))
             (segment-put s (%id 2) (%vec 48 2.0))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (progn
                    (is (every #'= (%vec 48 1.0) (segment-get s (%id 1))))
                    (is (every #'= (%vec 48 2.0) (segment-get s (%id 2)))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))
```

`segment-values-survive-reopen` depends on Task 4's id→slot rebuild; it will fail until Task 4. That is expected and called out in Task 4.

- [ ] **Step 2: Run to verify it fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: the put/get tests FAIL (undefined `segment-put`); `segment-values-survive-reopen` also fails.

- [ ] **Step 3: Implement put and get (no growth yet)**

Append to `segment.lisp`. Growth is Task 5 — here, if there is no free slot and capacity is full, signal; Task 5 replaces that signal with an extend.

```lisp
(defun %seg-id-offset (slot)
  (+ +segment-id-array-offset+ (* slot +key-bytes+)))

(defun %seg-vec-offset (segment slot)
  (+ (%seg-vblock-offset (segment-capacity segment))
     (* slot (segment-dimension segment) 4)))

(defun %seg-read-vector (segment slot)
  "Read slot SLOT's vector as a fresh (simple-array single-float (*))."
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (get-bytes (segment-mmap segment) off (* dim 4)))
         (v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v)
      (let ((bits 0) (b (* i 4)))
        (dotimes (k 4)
          (setf bits (dpb (aref bytes (+ b k)) (byte 8 (* k 8)) bits)))
        (setf (aref v i) (ieee-floats:decode-float32 bits))))))

(defun %seg-write-vector (segment slot vector)
  "Write VECTOR into slot SLOT's vector-block region."
  (declare (type (simple-array single-float (*)) vector))
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (make-array (* dim 4) :element-type '(unsigned-byte 8))))
    (dotimes (i dim)
      (let ((bits (ieee-floats:encode-float32 (aref vector i)))
            (b (* i 4)))
        (dotimes (k 4)
          (setf (aref bytes (+ b k)) (ldb (byte 8 (* k 8)) bits)))))
    (set-bytes (segment-mmap segment) bytes off (* dim 4))))

(defun %seg-write-id (segment slot id)
  ;; The free-list scheme marks a free cell by all-ones in its first 8 bytes, so
  ;; a real id whose first 8 bytes are all-ones would be misread as free after a
  ;; reopen (sec 5.1 rebuild).  Engine ids are uuids and never all-ones, but an
  ;; arbitrary caller-supplied id could be; reject it loudly rather than corrupt
  ;; silently.
  (let ((first8 0))
    (dotimes (k 8) (setf first8 (dpb (aref id k) (byte 8 (* k 8)) first8)))
    (when (= first8 +free-slot-marker+)
      (error "node id's first 8 bytes are all-ones, colliding with the segment ~
              free-slot marker")))
  (set-bytes (segment-mmap segment) id (%seg-id-offset slot) +key-bytes+))

(defun %seg-slot-of (segment id)
  "Slot index storing ID, or NIL."
  (gethash id (segment-id->slot segment)))

(defun %seg-claim-slot (segment)
  "Return a slot index to write a NEW id into: the free-list head if any, else
the next slot past live-count when capacity allows.  Signals when full -- Task 5
replaces this with growth."
  (let* ((mmap (segment-mmap segment))
         (free-head (%seg-free-head segment)))
    (if (/= free-head +no-slot+)
        ;; Pop the free list: its cell's second 8 bytes hold the next free slot.
        (let ((next (deserialize-uint64 mmap (+ (%seg-id-offset free-head) 8))))
          (serialize-uint64 mmap next 48)   ; free-head := next
          free-head)
        (let ((cap (segment-capacity segment))
              (live (segment-live-count segment)))
          (when (>= live cap)
            (error "segment full: capacity ~D (growth is Task 5)" cap))
          live))))

(defun segment-put (segment id vector)
  "Store VECTOR under the 16-byte ID.  Overwrites if ID is present; else takes a
free slot (or the next free index).  Returns the slot index.  VECTOR's length
must equal the segment's dimension, or this signals."
  (check-type vector (simple-array single-float (*)))
  (unless (= (length vector) (segment-dimension segment))
    (error "vector length ~D does not match segment dimension ~D"
           (length vector) (segment-dimension segment)))
  (let ((existing (%seg-slot-of segment id)))
    (if existing
        (progn (%seg-write-vector segment existing vector) existing)
        (let ((slot (%seg-claim-slot segment)))
          (%seg-write-id segment slot id)
          (%seg-write-vector segment slot vector)
          (setf (gethash id (segment-id->slot segment)) slot)
          (serialize-uint64 (segment-mmap segment)
                            (1+ (segment-live-count segment)) 40)
          slot))))

(defun segment-get (segment id)
  "The vector stored under ID as a fresh (simple-array single-float (*)), or NIL."
  (let ((slot (%seg-slot-of segment id)))
    (when slot (%seg-read-vector segment slot))))
```

- [ ] **Step 4: Run to verify the put/get tests pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: `segment-put-get-roundtrip`, `segment-put-overwrites-in-place`, `segment-put-rejects-wrong-dimension` PASS. `segment-values-survive-reopen` still FAILS (needs Task 4). `segment-create-and-reopen-header` still passes.

- [ ] **Step 5: Commit**

```bash
git add segment.lisp tests/segment-tests.lisp
git commit -m "feat(segment): segment-put / segment-get with dimension validation"
```

---

### Task 4: id→slot rebuild at open

**Files:**
- Modify: `segment.lisp` (replace the `%seg-rebuild-id->slot` stub)
- Modify: `tests/segment-tests.lisp`

**Interfaces:**
- Consumes: `segment-capacity`, `%seg-id-offset`, `get-bytes`, `deserialize-uint64`, `+free-slot-marker+`
- Produces: a working `%seg-rebuild-id->slot (segment)` that repopulates `segment-id->slot` from the on-disk id array

- [ ] **Step 1: Add the free-slot-recognition test**

Append to `tests/segment-tests.lisp`:

```lisp
(test segment-rebuild-skips-free-slots
  "After a remove, reopening rebuilds id->slot from the id array and does NOT
resurrect the removed id (the free slot is recognised, not read as an id)."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 16 :initial-capacity 8)))
             (segment-put s (%id 1) (%vec 16 1.0))
             (segment-put s (%id 2) (%vec 16 2.0))
             (segment-remove s (%id 1))          ; Task 5 defines remove
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (progn
                    (is (null (segment-get s (%id 1))) "removed id must not resurrect")
                    (is (every #'= (%vec 16 2.0) (segment-get s (%id 2)))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))
```

This test uses `segment-remove` (Task 5), so it will error until Task 5. The reopen/rebuild behaviour it checks is Task 4's; the ordering is deliberate — implement the rebuild now, and Task 5's remove makes this test pass. Run `segment-values-survive-reopen` (Task 3) as the gate for THIS task.

- [ ] **Step 2: Run `segment-values-survive-reopen` to confirm it still fails**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: `segment-values-survive-reopen` FAILS (the stub rebuild leaves id→slot empty, so `segment-get` returns nil after reopen).

- [ ] **Step 3: Implement the rebuild**

Replace the `%seg-rebuild-id->slot` stub in `segment.lisp` with:

```lisp
(defun %seg-rebuild-id->slot (segment)
  "Repopulate SEGMENT's RAM id->slot hash by sweeping the on-disk id array.
The id array is authoritative (sec 5.1).  A slot whose first 8 bytes are
+FREE-SLOT-MARKER+ is free and skipped; every other slot holds a real 16-byte id."
  (let ((mmap (segment-mmap segment))
        (cap (segment-capacity segment))
        (table (segment-id->slot segment)))
    (clrhash table)
    (dotimes (slot cap)
      (let ((first8 (deserialize-uint64 mmap (%seg-id-offset slot))))
        (unless (= first8 +free-slot-marker+)
          (let ((id (get-bytes mmap (%seg-id-offset slot) +key-bytes+)))
            (setf (gethash id table) slot)))))))
```

**The invariant the free-slot marker depends on** — a real node id's first 8 bytes are never all-ones — is guarded at the write site by `%seg-write-id` (Task 3, Step 3), which signals rather than store such an id. That guard is what makes this sweep's "first 8 bytes = `+free-slot-marker+` ⇒ free" test sound. No further action here; just be aware the rebuild's correctness is anchored by that Task 3 guard, not by luck.

- [ ] **Step 4: Run to verify `segment-values-survive-reopen` passes**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: `segment-values-survive-reopen` PASS. `segment-rebuild-skips-free-slots` still errors (needs Task 5's `segment-remove`).

- [ ] **Step 5: Commit**

```bash
git add segment.lisp tests/segment-tests.lisp
git commit -m "feat(segment): rebuild id->slot from the authoritative id array at open"
```

---

### Task 5: remove (free list) and growth across the mmap boundary

**Files:**
- Modify: `segment.lisp` (add `segment-remove`; replace the "segment full" signal in `%seg-claim-slot` with growth)
- Modify: `tests/segment-tests.lisp`

**Interfaces:**
- Consumes: `extend-mapped-file`, `mapped-file-length` (`mmap.lisp`); `%seg-file-bytes`, `%seg-vblock-offset`, `%seg-claim-slot` (earlier tasks)
- Produces:
  - `segment-remove (segment id)` → `t` if removed, `nil` if absent
  - growth inside `%seg-claim-slot` — capacity doubles via `extend-mapped-file`, the vector block is relocated, the base pointer never moves

- [ ] **Step 1: Write the free-list and growth tests**

Append to `tests/segment-tests.lisp`:

```lisp
(test segment-remove-frees-and-reuses-slot
  "Remove drops the id and frees its slot; the next new put reuses that slot."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 16 :initial-capacity 8)))
           (unwind-protect
                (let ((slot1 (segment-put s (%id 1) (%vec 16 1.0))))
                  (is (eq t (segment-remove s (%id 1))))
                  (is (null (segment-get s (%id 1))))
                  (is (= 0 (segment-live-count s)))
                  ;; the freed slot is reused by the next NEW id
                  (let ((slot2 (segment-put s (%id 2) (%vec 16 2.0))))
                    (is (= slot1 slot2)))
                  (is (null (segment-remove s (%id 999)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-grows-past-initial-capacity
  "Putting more ids than the initial capacity grows the segment; all vectors
survive the growth bit-exactly, including ones written before it."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 32 :initial-capacity 4)))
           (unwind-protect
                (progn
                  ;; write 4 to fill, then 12 more to force >= 2 growths
                  (dotimes (i 16)
                    (segment-put s (%id i) (%vec 32 (coerce i 'single-float))))
                  (is (>= (segment-capacity s) 16))
                  (is (= 16 (segment-live-count s)))
                  ;; every vector, including the earliest, still reads correctly
                  (dotimes (i 16)
                    (is (every #'= (%vec 32 (coerce i 'single-float))
                               (segment-get s (%id i)))
                        "vector ~D corrupted by growth" i)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-growth-survives-reopen
  "A grown segment reopens with the grown capacity and all vectors intact."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 24 :initial-capacity 2)))
             (dotimes (i 10)
               (segment-put s (%id i) (%vec 24 (coerce i 'single-float))))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (progn
                    (is (>= (segment-capacity s) 10))
                    (dotimes (i 10)
                      (is (every #'= (%vec 24 (coerce i 'single-float))
                                 (segment-get s (%id i))))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))
```

- [ ] **Step 2: Run to verify they fail**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: the three new tests plus `segment-rebuild-skips-free-slots` FAIL — `segment-remove` undefined, and growth signals "segment full".

- [ ] **Step 3: Implement remove**

Append to `segment.lisp`:

```lisp
(defun segment-remove (segment id)
  "Remove ID from the segment, pushing its slot onto the free list.  Returns T
if ID was present, NIL otherwise.  A freed slot's id-array cell is marked with
+FREE-SLOT-MARKER+ (first 8 bytes) and the previous free-head (second 8 bytes),
threading the free list; its vector-block bytes are left as-is (unreachable)."
  (let ((slot (%seg-slot-of segment id)))
    (if (null slot)
        nil
        (let ((mmap (segment-mmap segment))
              (old-head (%seg-free-head segment)))
          (serialize-uint64 mmap +free-slot-marker+ (%seg-id-offset slot))
          (serialize-uint64 mmap old-head (+ (%seg-id-offset slot) 8))
          (serialize-uint64 mmap slot 48)      ; free-head := slot
          (remhash id (segment-id->slot segment))
          (serialize-uint64 mmap (1- (segment-live-count segment)) 40)
          t))))
```

- [ ] **Step 4: Implement growth**

Replace the `error "segment full ..."` branch in `%seg-claim-slot` (Task 3) with a call to `%seg-grow`, and add `%seg-grow`:

```lisp
(defun %seg-grow (segment)
  "Double the segment's capacity in place.  Because the vector block starts
after the id array and the id array's size is capacity*16, growing capacity
moves the vector block: extend the file, then relocate the existing vectors
from the OLD block offset to the NEW one, high slot first so the copy never
overwrites unread source bytes.  The base pointer never moves (extend-mapped-
file remaps into the reserved window), so a concurrent read never faults."
  (let* ((mmap (segment-mmap segment))
         (dim (segment-dimension segment))
         (old-cap (segment-capacity segment))
         (new-cap (* 2 old-cap))
         (old-vblock (%seg-vblock-offset old-cap))
         (new-vblock (%seg-vblock-offset new-cap))
         (needed (%seg-file-bytes new-cap dim))
         (have (mapped-file-length mmap)))
    (when (> needed have)
      (extend-mapped-file mmap (- needed have)))
    ;; Relocate vectors, HIGH slot first: new-vblock > old-vblock, so copying
    ;; slot i from old+i*w to new+i*w with i descending never overwrites a
    ;; not-yet-copied source region.
    (let ((w (* dim 4)))
      (loop for i from (1- old-cap) downto 0
            for src = (+ old-vblock (* i w))
            for dst = (+ new-vblock (* i w))
            do (set-bytes mmap (get-bytes mmap src w) dst w)))
    (serialize-uint64 mmap new-cap 32)         ; capacity := new-cap
    old-cap))                                  ; first fresh slot index
```

Then in `%seg-claim-slot`, replace:

```lisp
          (when (>= live cap)
            (error "segment full: capacity ~D (growth is Task 5)" cap))
          live))))
```

with:

```lisp
          (if (>= live cap)
              (%seg-grow segment)              ; returns old-cap = first fresh slot
              live)))))
```

- [ ] **Step 5: Run to verify all segment tests pass**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::segment-suite))'
```

Expected: every test in `segment-suite` PASSES, including `segment-rebuild-skips-free-slots`, the three growth tests, and everything from Tasks 2–4.

- [ ] **Step 6: Run the full engine suite for regressions**

```
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::graph-db-suite))'
```

Expected: 2116 (or more, with the new segment checks) and zero failures. Report the actual counts.

- [ ] **Step 7: Commit**

```bash
git add segment.lisp tests/segment-tests.lisp
git commit -m "feat(segment): remove (free list) and in-place growth across the mmap boundary"
```

---

## Done Criteria

- [ ] `segment-suite` fully green; full `graph-db-suite` green with the new checks and zero failures (report counts)
- [ ] A vector stored, removed, and re-put reuses the freed slot; live-count tracks occupancy
- [ ] A segment grown past its initial capacity preserves every earlier vector bit-exactly, and reopens with the grown capacity
- [ ] Reopen rebuilds id→slot from the id array alone; a removed id does not resurrect
- [ ] A dimension mismatch signals
- [ ] No transaction hook, no rebuild-from-nodes, no scan/score, no `map-vertices` — those are Steps 3 and 4
