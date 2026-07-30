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
                           (:conc-name segment-)
                           (:predicate vector-segment-p))
  (mmap nil)                 ; a mapped-file (mmap.lisp)
  (dimension 0 :type fixnum) ; fixed at create time
  (id->slot nil)             ; equalp hash: 16-byte id vector -> slot index
  (clean-at-open nil)        ; the on-disk clean flag as it was when this segment opened
  ;; Per-segment reader/writer lock.  All PUBLIC mutations (segment-put,
  ;; segment-remove) take the write side; public reads (segment-get,
  ;; segment-scan, segment-score-subset) take the read side.  Never persisted --
  ;; created fresh by create/open.
  ;;
  ;; LOCK AT PUBLIC BOUNDARIES ONLY.  The %SEG-* internals are lock-free and
  ;; assume the caller already holds the lock -- segment-put -> %seg-claim-slot
  ;; -> %seg-grow nests, so a lock inside %seg-grow would be a second acquire
  ;; under the first.  Same idiom as the skip list.
  ;;
  ;; Precisely: it is the READ side that is non-recursive.  ACQUIRE-WRITE-LOCK
  ;; (rw-lock.lisp) has an explicit same-thread re-acquire branch, so nesting
  ;; two write acquires on one thread would in fact survive; nesting two READ
  ;; acquires deadlocks if a writer arrives between them.  Do not rely on
  ;; either -- the rule above is what keeps this correct, not the recursion
  ;; behavior of any one side.
  ;;
  ;; LOCK ORDER: the write side is only ever taken INSIDE the transaction
  ;; manager lock (mutations run on the apply path); the read side is taken
  ;; alone.  Never take the manager lock while holding a segment lock.
  (lock (make-rw-lock)))

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

(defun %seg-reservation-for (size)
  "The virtual-address reservation, in bytes, a segment file of SIZE should get.

MMAP-FILE's general default -- *MMAP-RESERVATION-MULTIPLIER* x size, floored at
*MMAP-MIN-RESERVATION* -- was aimed at schema-sized heap and index files.  A
segment's size tracks the corpus, so it exhausts that headroom far sooner, and
the failure lands inside APPLY-TRANSACTION.  Segments therefore get their own,
much larger floor (*SEGMENT-MIN-RESERVATION*).

MAX, not the bare floor: a segment that is ALREADY larger than
floor / multiplier must still get proportional headroom rather than being capped
at the floor -- passing the floor alone would make the reservation SHRINK
relative to today's behaviour for exactly the large segments that need it most.
Both call sites (create and open) must use this.  Before this floor existed,
NEITHER passed MMAP-FILE an explicit :RESERVATION at all -- both simply took
its general default (*MMAP-RESERVATION-MULTIPLIER* x size, floored at
*MMAP-MIN-RESERVATION*), which is exactly the floor that was too small for a
segment.  The open path is the one that matters most in practice, since a
long-lived graph runs on reopened segments, not freshly created ones."
  (max *segment-min-reservation* (* *mmap-reservation-multiplier* size)))

(defun create-vector-segment (path dimension &key (initial-capacity 1024))
  "Create a new vector segment at PATH holding DIMENSION-wide single-float
vectors, with room for INITIAL-CAPACITY slots.  DIMENSION is fixed for the life
of the segment.  Returns an open VECTOR-SEGMENT."
  (check-type dimension (integer 1))
  (check-type initial-capacity (integer 1))
  (let* ((bytes (%seg-file-bytes initial-capacity dimension))
         (mmap (mmap-file path :create-p t :size bytes
                               :reservation (%seg-reservation-for bytes))))
    (%seg-write-header mmap
                       :magic +segment-magic+
                       :format +segment-format+
                       :dimension dimension
                       :element-type +fv-single-float+
                       :capacity initial-capacity
                       :live-count 0
                       :free-head +no-slot+)
    ;; A freshly created (or freshly extended-by-mmap-file) region is zero-
    ;; filled, not free-marked: without this, the never-written tail
    ;; [0, initial-capacity) reads as an all-zero id at reopen-time sweep
    ;; (a real bug caught before this shipped -- see %seg-mark-free-range).
    ;; Marking it here makes "never written" and "removed" the same on-disk
    ;; state, which the sweep already knows how to skip.
    (%seg-mark-free-range mmap 0 initial-capacity)
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
  "Open an existing vector segment at PATH.  Validates magic, format, and
element-type, reads the header, and rebuilds the RAM id->slot map by sweeping
the id array (the on-disk id array is authoritative; the map is never
persisted)."
  ;; Size the reservation from the file as it is NOW.  PROBE-FILE first so a
  ;; missing file still produces MMAP-FILE's own diagnostic rather than a raw
  ;; CL file-error out of %FILE-SIZE; the 0 is never used in that case.
  (let ((mmap (mmap-file path :create-p nil
                              :reservation (%seg-reservation-for
                                            (if (probe-file path)
                                                (%file-size path)
                                                0)))))
    (let ((magic (deserialize-uint64 mmap 0))
          (format (deserialize-uint64 mmap 8)))
      (unless (= magic +segment-magic+)
        (error "~A is not a vector segment (magic ~X)" path magic))
      (unless (= format +segment-format+)
        (error "vector segment ~A is format ~D, expected ~D"
               path format +segment-format+)))
    ;; Only single-float is ever written today, and every read path
    ;; (%seg-read-vector / %seg-write-vector) hard-assumes it; validate the
    ;; on-disk element-type so a future double-float/int8 segment (the whole
    ;; reason this header field exists) can't be silently misread as
    ;; single-float instead of signaling.
    (let ((etype (deserialize-uint64 mmap 24)))
      (unless (= etype +fv-single-float+)
        (error "vector segment ~A has element-type ~D, expected ~D (only ~
                single-float is supported)"
               path etype +fv-single-float+)))
    (let ((segment (%make-vector-segment
                    :mmap mmap
                    :dimension (deserialize-uint64 mmap 16)
                    :id->slot (make-hash-table :test 'equalp))))
      (%seg-rebuild-id->slot segment)
      ;; Capture the persisted clean flag (the recovery decision reads THIS), then
      ;; mark the file dirty for the new session.
      (let ((clean (= (deserialize-uint64 mmap +segment-clean-offset+) +segment-clean+)))
        (setf (segment-clean-at-open segment) clean)
        (serialize-uint64 mmap +segment-dirty+ +segment-clean-offset+)
        ;; Force the dirty flag to disk NOW, so a crash after this open reliably
        ;; leaves the segment marked dirty -> rebuild on next open.  Without this
        ;; the flip-to-dirty is an unsynced mmap store whose writeback timing is
        ;; the kernel's choice, so a hard crash could leave the on-disk flag still
        ;; reading clean, and an unsanctioned recovery (one that deletes .dirty and
        ;; reopens rather than snapshot/replay) would then TRUST a stale segment.
        ;; Defense-in-depth: the graph's .dirty marker is the primary crash guard
        ;; (open-graph refuses a crashed graph); this closes the residual window.
        ;; One msync per segment at open only -- open is not a hot path.
        (sync-region mmap :length +segment-header-bytes+))
      segment)))

(defun segment-clean-shutdown-p (segment)
  "True if the segment's on-disk state at open time was cleanly closed."
  (segment-clean-at-open segment))

(defun close-vector-segment (segment)
  "Release the segment's mmap."
  (when (segment-mmap segment)
    (serialize-uint64 (segment-mmap segment) +segment-clean+ +segment-clean-offset+)
    (munmap-file (segment-mmap segment))
    (setf (segment-mmap segment) nil))
  nil)

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

(defun %seg-id-offset (slot)
  (+ +segment-id-array-offset+ (* slot +key-bytes+)))

(defun %seg-mark-free-range (mmap start-slot end-slot)
  "Mark id-array cells [START-SLOT, END-SLOT) free: +FREE-SLOT-MARKER+ in both
8-byte halves.  The first half is what the rebuild sweep checks; the second
half doubles as a free-list \"next\" pointer of +NO-SLOT+ (same bit pattern),
so these cells are indistinguishable from a properly terminated free chain
even though they are not yet threaded onto one.  Used by CREATE-VECTOR-SEGMENT
for the initial capacity and by %SEG-GROW for newly added capacity, so a
never-written cell and a removed cell are the same on-disk state."
  (loop for slot from start-slot below end-slot
        for off = (%seg-id-offset slot)
        do (serialize-uint64 mmap +free-slot-marker+ off)
           (serialize-uint64 mmap +free-slot-marker+ (+ off 8))))

(defun %seg-vec-offset (segment slot)
  (+ (%seg-vblock-offset (segment-capacity segment))
     (* slot (segment-dimension segment) 4)))

;;; Decoding the vector block is THE hot path.  segment-scan sweeps every
;;; occupied slot and decodes its whole vector, so at 20k x 1024 the decode ran
;;; ~21M times per scan and measured 1613 ms of a 1633 ms scan -- 98.8% of the
;;; time, against a 17 ms pure-scoring floor.  The scan's entire justification is
;;; that scoring contiguous float32s beats materialising nodes; a byte-at-a-time
;;; decoder threw that away.  Hence the SBCL fast path below.
;;;
;;; ON-DISK FORMAT (unchanged): each element is 4 bytes, LITTLE-ENDIAN, holding
;;; the IEEE-754 binary32 bit pattern that IEEE-FLOATS:ENCODE-FLOAT32 produces
;;; (%SEG-WRITE-VECTOR assembles it with LDB byte 0 first).  Reading it with
;;; SB-SYS:SIGNED-SAP-REF-32 + SB-KERNEL:MAKE-SINGLE-FLOAT is therefore
;;; equivalent ONLY on a little-endian host with IEEE binary32 single-floats.
;;; That is stated as an explicit assumption rather than left implicit: SBCL
;;; targets here are arm64 and x86_64, both little-endian, and the guard below
;;; makes the assumption fail loudly rather than silently on anything else.

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (= (sb-kernel:single-float-bits 1.0f0) #x3F800000)
    (error "segment.lisp's SBCL fast decode assumes IEEE binary32 single-floats"))
  #+big-endian
  (error "segment.lisp's SBCL fast vector decode assumes a little-endian host; ~
          this build is big-endian.  Remove the #+sbcl fast path (the portable ~
          #-sbcl decoder below is byte-order explicit) before running here."))

#+sbcl
(defun %seg-decode-into (mmap off dim buffer)
  "Decode DIM little-endian float32s starting at byte offset OFF of MMAP into
BUFFER.  Bit-identical to the portable DPB + IEEE-FLOATS:DECODE-FLOAT32 loop:
for every finite pattern MAKE-SINGLE-FLOAT reproduces DECODE-FLOAT32 exactly
 (same sign, exponent and significand bits), and for the non-finite patterns
 (exponent = 255, i.e. Inf/NaN) -- which SEGMENT-PUT can never write, since
IEEE-FLOATS:ENCODE-FLOAT32 refuses them, and which therefore only ever appear
as CORRUPT or TORN bytes -- it defers to DECODE-FLOAT32 itself, so those still
signal the very same FLOATING-POINT-OVERFLOW they always did.  That matters:
the concurrency regression test relies on a torn read blowing up rather than
quietly yielding a NaN, and a bare SAP-REF-SINGLE would have silently
downgraded that detector.

⚠ THIS CACHES (M-POINTER MMAP) ONCE AND DEREFERENCES IT FOR THE WHOLE VECTOR, AT
 (SAFETY 0).  THE ONLY THING THAT MAKES THAT SAFE IS THE SEGMENT'S READ LOCK.
Say it plainly, because the reason USED to be a property of the mapping itself
and no longer is: it was true that \"the base pointer never moves\" -- MMAP-FILE
reserves the window and EXTEND-MAPPED-FILE re-maps into it with MAP_FIXED, so
growth kept the address.  RELOCATE-VECTOR-SEGMENT-MAPPING (added when the
reservation stopped being a growth ceiling) MOVES IT: %SEG-GROW re-reserves a
larger window, republishes M-POINTER, and MUNMAPS THE OLD ONE.  A SAP captured
before that call points into freed address space afterwards -- a SIGSEGV if we
are lucky, a read of some unrelated later mapping if we are not.

What holds the line is that every caller of this function reaches it through a
public segment entry point holding the segment's READ lock (SEGMENT-GET,
SEGMENT-SCAN, SEGMENT-SCORE-SUBSET), and relocation only ever happens under the
same segment's WRITE lock.  So no relocation can begin while this SAP is live.
DO NOT introduce a caller that reads a segment without that lock, and do not
\"optimise away\" the read lock on a scan: it is not there for the id table, it
is there for this pointer.

SEGV GUARD.  MMAP.LISP wraps GET-BYTE/GET-BYTES in :AROUND handlers that catch
SB-KERNEL::MEMORY-FAULT-ERROR and retry; reading through the SAP bypasses them,
so the guard is re-established HERE at whole-vector granularity (one handler
frame per vector instead of one per byte -- strictly cheaper, and the retry unit
is the same idempotent read).  It is a backstop, not the correctness argument --
the read lock above is.  *MMAP-SEGV-RETRIES* is asserted to stay 0 under
concurrency, including across relocations, and if it ever moves off 0 the
conclusion is that some reader reached a mapping without the lock, NOT that the
retry saved us.  Keeping the handler costs nothing measurable and preserves the
existing contract rather than silently dropping it."
  (declare (type fixnum off dim)
           (type (simple-array single-float (*)) buffer)
           (optimize (speed 3) (safety 0)))
  (handler-case
      (let ((sap (m-pointer mmap)))
        (declare (type sb-sys:system-area-pointer sap))
        (dotimes (i dim buffer)
          (declare (type fixnum i))
          (let ((word (sb-sys:signed-sap-ref-32 sap (the fixnum (+ off (* i 4))))))
            (declare (type (signed-byte 32) word))
            (setf (aref buffer i)
                  (if (= #xff (logand (ash word -23) #xff))
                      ;; Inf/NaN pattern: never legitimately stored.  Route it
                      ;; through the original decoder so the error is identical.
                      (ieee-floats:decode-float32 (ldb (byte 32 0) word))
                      (sb-kernel:make-single-float word))))))
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mmap)
      (%seg-decode-into mmap off dim buffer))))

#-sbcl
(defun %seg-decode-into (mmap off dim buffer)
  "Portable decoder: byte-order-explicit little-endian assembly through the
mmap accessors (which carry their own SEGV-retry :AROUND methods).  This is the
original implementation, retained verbatim as the fallback for CCL, ECL and
LispWorks so every implementation produces identical results."
  (let ((bytes (get-bytes mmap off (* dim 4))))
    (dotimes (i dim buffer)
      (let ((bits 0) (b (* i 4)))
        (dotimes (k 4)
          (setf bits (dpb (aref bytes (+ b k)) (byte 8 (* k 8)) bits)))
        (setf (aref buffer i) (ieee-floats:decode-float32 bits))))))

(defun %seg-read-vector-into (segment slot buffer)
  "Decode slot SLOT's vector into BUFFER, a (simple-array single-float (*)) of
the segment's dimension.  Returns BUFFER.  Lets a scan reuse one buffer for
every candidate instead of consing a fresh vector per slot.

The length check is NOT redundant: the SBCL decoder runs at (safety 0), so a
short buffer would scribble past the array rather than signal.  One check per
vector (not per element) is unmeasurable next to the decode it guards."
  (declare (type (simple-array single-float (*)) buffer))
  (unless (>= (length buffer) (segment-dimension segment))
    (error "decode buffer of length ~D is shorter than segment dimension ~D"
           (length buffer) (segment-dimension segment)))
  (%seg-decode-into (segment-mmap segment)
                    (%seg-vec-offset segment slot)
                    (segment-dimension segment)
                    buffer))

(defun %seg-read-vector (segment slot)
  "Read slot SLOT's vector as a fresh (simple-array single-float (*))."
  (%seg-read-vector-into
   segment slot
   (make-array (segment-dimension segment) :element-type 'single-float)))

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

(defun %seg-check-id (id)
  ;; The free-list scheme marks a free cell by all-ones in its first 8 bytes, so
  ;; a real id whose first 8 bytes are all-ones would be misread as free after a
  ;; reopen (sec 5.1 rebuild).  Engine ids are uuids and never all-ones, but an
  ;; arbitrary caller-supplied id could be; reject it loudly rather than corrupt
  ;; silently.
  ;;
  ;; This MUST run before any slot is claimed (i.e. before %SEG-CLAIM-SLOT),
  ;; not inside %SEG-WRITE-ID after the fact: %seg-claim-slot can pop a slot
  ;; off the free list as a side effect, and if the id were only validated
  ;; afterward, a rejected put would still have popped and orphaned that slot.
  ;; SEGMENT-PUT calls this first, before claiming anything.
  (let ((first8 0))
    (dotimes (k 8) (setf first8 (dpb (aref id k) (byte 8 (* k 8)) first8)))
    (when (= first8 +free-slot-marker+)
      (error "node id's first 8 bytes are all-ones, colliding with the segment ~
              free-slot marker"))))

(defun %seg-write-id (segment slot id)
  "Write ID into slot SLOT's id-array cell.  Caller must have already
validated ID via %SEG-CHECK-ID."
  (set-bytes (segment-mmap segment) id (%seg-id-offset slot) +key-bytes+))

(defun %seg-slot-of (segment id)
  "Slot index storing ID, or NIL."
  (gethash id (segment-id->slot segment)))

(defun %seg-ensure-reservation (mmap needed capacity)
  "Make MMAP's virtual-address reservation cover NEEDED bytes.  A no-op in the
overwhelmingly common case where the reservation already covers NEEDED.
Otherwise, two mechanisms in order:

  1. EXTEND IT IN PLACE (Part 3) -- claim the address range immediately after
     the current window (EXTEND-RESERVATION-IN-PLACE).  One mmap; M-POINTER
     does not move; nothing is copied or remapped; no reader can observe it at
     all.  Measured, this fires LESS often than the design assumed -- Linux
     packs a new mmap(NULL) window flush under the existing mappings, so the
     range above it is usually taken; see that function's docstring.
  2. RELOCATE (Part 4) -- move the mapping into a fresh larger window
     (RELOCATE-VECTOR-SEGMENT-MAPPING).  Nominally the fallback, but in practice
     still the path production takes most of the time.  This one MOVES
     M-POINTER.

The two ask for different amounts, deliberately.  The adjacent claim asks for
exactly NEEDED, because it costs one syscall and nothing else -- paying it once
per doubling is cheaper than reserving speculative address space that
RLIMIT_AS still charges for.  Relocation asks %SEG-RESERVATION-FOR for the full
policy size (max(floor, multiplier x size)), because it costs two syscalls and a
TLB-visible remap, so it should buy the NEXT several doublings as well.  A
relocated segment therefore lands on exactly the policy a freshly opened one
would.

CALLER MUST HOLD THE SEGMENT'S WRITE LOCK.  Relocation moves M-POINTER, and the
segment's rw-lock is the ONLY thing excluding readers from the old address --
see RELOCATE-VECTOR-SEGMENT-MAPPING's docstring, which spells out why the heap
and the linear hash can never do this.  %SEG-GROW, the sole caller, runs under
that lock (SEGMENT-PUT / SEGMENT-REMOVE take it; the %SEG-* internals are
lock-free by convention).

Signals VECTOR-SEGMENT-CAPACITY-EXHAUSTED only when BOTH mechanisms are
unavailable: the adjacent claim failed or is switched off
 (*SEGMENT-EXTEND-ADJACENT-ON-EXHAUSTION*), AND relocation is switched off
 (*SEGMENT-RELOCATE-ON-EXHAUSTION*) or failed outright (address space exhausted
/ RLIMIT_AS).  Either way NOTHING HAS BEEN MUTATED when it signals: a failed
adjacent claim unmaps whatever it got, the disabled branch runs before any
syscall, and RELOCATE-VECTOR-SEGMENT-MAPPING rolls its own failure back, so the
segment is left at its current reservation, fully consistent and usable.  Note
the branches are NOT the same code path -- they share only what is downstream of
the signal -- so a test that exercises the kill-switch has NOT thereby exercised
a failing relocation, and a test that exercises the adjacent claim has NOT
thereby exercised relocation AT ALL.  That last one is the whole reason
*SEGMENT-EXTEND-ADJACENT-ON-EXHAUSTION* exists.

ENSURE-VECTOR-SEGMENT-CAPACITY calls this DIRECTLY, once, for the FULL capacity
a transaction will need, before it grows anything; that is what makes a
transaction-path abort atomic (no doubling has run yet) and lets it re-signal
with the owner and slot, which this function does not know.

CAPACITY is the slot count NEEDED bytes corresponds to -- carried purely so the
signalled condition reports entries and bytes, rather than reporting the byte
count in the entry field (which read as a nonsense \"growing to hold 98,368
entries needs 98,368 bytes\")."
  (let ((reserved (m-reserved-size mmap)))
    (when (> needed reserved)
      ;; Cheap path first: grow the window rather than move it.  Returns NIL
      ;; without mutating anything if the adjacent range is not free.
      (let (extended adjacent-reason)
        (when *segment-extend-adjacent-on-exhaustion*
          (multiple-value-setq (extended adjacent-reason)
            (extend-reservation-in-place mmap needed)))
        (when extended
          (return-from %seg-ensure-reservation mmap))
        (unless *segment-relocate-on-exhaustion*
          (error 'vector-segment-capacity-exhausted
                 :path (m-path mmap)
                 :required capacity :needed-bytes needed :reserved reserved
                 :reason (format nil "the adjacent address range could not be claimed ~
(~A) and relocation is disabled (GRAPH-DB:*SEGMENT-RELOCATE-ON-EXHAUSTION* is NIL)"
                                 (cond
                                   ((not *segment-extend-adjacent-on-exhaustion*)
                                    "GRAPH-DB:*SEGMENT-EXTEND-ADJACENT-ON-EXHAUSTION* is NIL")
                                   ((eq adjacent-reason :occupied) "it is occupied")
                                   ((eq adjacent-reason :no-base)
                                    "the segment has no established mapping yet")
                                   (t (format nil "the mapping attempt failed: ~A"
                                              adjacent-reason)))))))
      (handler-case
          (relocate-vector-segment-mapping mmap (%seg-reservation-for needed))
        (error (e)
          ;; Nothing was mutated (the primitive rolls back), so the segment is
          ;; still usable at its current reservation -- report rather than
          ;; resignal the raw mmap error, so callers can still discriminate
          ;; this from a data error by condition type.
          (error 'vector-segment-capacity-exhausted
                 :path (m-path mmap)
                 :required capacity :needed-bytes needed :reserved reserved
                 :reason (format nil "re-reserving ~D bytes failed: ~A"
                                 (%seg-reservation-for needed) e))))))
  mmap)

(defun %seg-grow (segment)
  "Double the segment's capacity in place.  Because the vector block starts
after the id array and the id array's size is capacity*16, growing capacity
moves the vector block: extend the file, then relocate the existing vectors
from the OLD block offset to the NEW one, high slot first so the copy never
overwrites unread source bytes.  The base pointer normally never moves
 (extend-mapped-file remaps into the reserved window), so a concurrent read
never faults.  Returns OLD-CAP, the first fresh (unclaimed) slot index.

If the doubling would pass the mmap reservation, %SEG-ENSURE-RESERVATION first
tries to EXTEND the reservation in place -- claiming the address range
immediately after the current window -- in which case the base pointer does
NOT move, nothing is copied, and no reader can observe it.  Only when that
adjacent range is unavailable does it fall back to RE-RESERVING a larger window
and RELOCATING the mapping into it -- the one case where the base pointer does
move.  That relocation is safe here and nowhere else: this runs under the
segment's write lock (SEGMENT-PUT / SEGMENT-REMOVE), which excludes every
reader of this mapping.  The reservation is therefore no longer a ceiling
for segments."
  (let* ((mmap (segment-mmap segment))
         (dim (segment-dimension segment))
         (old-cap (segment-capacity segment))
         (new-cap (* 2 old-cap))
         (old-vblock (%seg-vblock-offset old-cap))
         (new-vblock (%seg-vblock-offset new-cap))
         (needed (%seg-file-bytes new-cap dim))
         (have (mapped-file-length mmap)))
    (when (> needed have)
      ;; Relocate to a larger reservation if this doubling would not fit; a
      ;; no-op otherwise.  Must precede the extend, which treats the
      ;; reservation as hard (it has no read lock to relocate under).
      (%seg-ensure-reservation mmap needed new-cap)
      (extend-mapped-file mmap (- needed have)))
    ;; Relocate vectors, HIGH slot first: new-vblock > old-vblock, so copying
    ;; slot i from old+i*w to new+i*w with i descending never overwrites a
    ;; not-yet-copied source region.
    (let ((w (* dim 4)))
      (loop for i from (1- old-cap) downto 0
            for src = (+ old-vblock (* i w))
            for dst = (+ new-vblock (* i w))
            do (set-bytes mmap (get-bytes mmap src w) dst w)))
    ;; The newly added id-array cells [old-cap, new-cap) currently sit where
    ;; stale vector bytes (already relocated above) or freshly extended file
    ;; bytes live -- neither is free-marked.  Mark them, same as create does
    ;; for the initial capacity, so an untouched cell never sweeps as a
    ;; phantom id.
    (%seg-mark-free-range mmap old-cap new-cap)
    (serialize-uint64 mmap new-cap 32)         ; capacity := new-cap
    old-cap))                                  ; first fresh slot index

(defun %seg-claim-slot (segment)
  "Return a slot index to write a NEW id into: the free-list head if any, else
the next slot past live-count, growing the segment first if capacity is
exhausted."
  (let* ((mmap (segment-mmap segment))
         (free-head (%seg-free-head segment)))
    (if (/= free-head +no-slot+)
        ;; Pop the free list: its cell's second 8 bytes hold the next free slot.
        (let ((next (deserialize-uint64 mmap (+ (%seg-id-offset free-head) 8))))
          (serialize-uint64 mmap next 48)   ; free-head := next
          free-head)
        (let ((cap (segment-capacity segment))
              (live (segment-live-count segment)))
          (if (>= live cap)
              (%seg-grow segment)              ; returns old-cap = first fresh slot
              live)))))

(defun segment-put (segment id vector)
  "Store VECTOR under the 16-byte ID.  Overwrites if ID is present; else takes a
free slot (or the next free index, growing the segment if necessary).  Returns
the slot index.  VECTOR's length must equal the segment's dimension, and ID's
first 8 bytes must not collide with the free-slot marker -- both are validated
up front, before any slot is claimed, so a rejected put never disturbs the
free list.

Takes the segment's WRITE lock: mutations are exclusive against concurrent
scans.  The %SEG-* internals it calls (including %seg-grow) are lock-free and
run under this lock."
  (with-write-lock ((segment-lock segment))
    (check-type vector (simple-array single-float (*)))
    (unless (= (length vector) (segment-dimension segment))
      (error "vector length ~D does not match segment dimension ~D"
             (length vector) (segment-dimension segment)))
    (%seg-check-id id)
    (let ((existing (%seg-slot-of segment id)))
      (if existing
          (progn (%seg-write-vector segment existing vector) existing)
          (let ((slot (%seg-claim-slot segment)))
            (%seg-write-id segment slot id)
            (%seg-write-vector segment slot vector)
            (setf (gethash id (segment-id->slot segment)) slot)
            (serialize-uint64 (segment-mmap segment)
                              (1+ (segment-live-count segment)) 40)
            slot)))))

(defun segment-get (segment id)
  "The vector stored under ID as a fresh (simple-array single-float (*)), or NIL.

Takes the segment's READ lock: shared against concurrent reads, exclusive
against a concurrent segment-put/segment-remove."
  (with-read-lock ((segment-lock segment))
    (let ((slot (%seg-slot-of segment id)))
      (when slot (%seg-read-vector segment slot)))))

(defun %sweep-vector-index-owner (fn graph owner-name)
  "Call FN on every live node of OWNER-NAME (and its subclasses), whether OWNER-
NAME is a vertex or an edge class.  Live maintenance (APPLY-TX-WRITE-TO-VECTOR-
SEGMENTS) is node-generic over CLASS-OF, so a :VECTOR-INDEX slot on an edge is
filled exactly like one on a vertex; rebuild must sweep the matching kind or
an edge-owned segment always rebuilds empty (GH #57)."
  (if (subtypep owner-name 'edge)
      (map-edges fn graph :edge-type owner-name)
      (map-vertices fn graph :vertex-type owner-name)))

(defun rebuild-vector-segment (graph owner-name slot-name)
  "Rebuild the (OWNER-NAME, SLOT-NAME) segment from live nodes: drop any current
segment/file, create a fresh one whose dimension comes from the first conforming
vector and whose capacity is (MAX 1024 LIVE-COUNT) -- the corpus size, but never
below CREATE-VECTOR-SEGMENT's own default -- and segment-put every live node's
conforming value.  Registers and returns the fresh segment, or NIL if no
live node has a conforming vector (in which case no segment is created at all).
Run when quiescent (at open, before writes) -- it mutates outside the
transaction path, like rebuild-spatial-indexes.

OWNER-NAME must be the segment's OWNER -- the declaring class returned by
%VECTOR-INDEX-SLOT-OWNER-NAME / %SEGMENT-KEY (transactions.lisp), not
necessarily a node's exact runtime class.  One segment per owner spans its
subclasses (the engine's :UNIQUE / :INDEX convention), so this sweeps via
%SWEEP-VECTOR-INDEX-OWNER (MAP-VERTICES or MAP-EDGES, whichever OWNER-NAME is;
GH #57) with its default :INCLUDE-SUBCLASSES-P T: every subclass instance's
vector is swept into the OWNER's segment, matching exactly what the
live apply path (APPLY-TX-WRITE-TO-VECTOR-SEGMENTS, via %SEGMENT-KEY) does on
create/update/delete.

NOT SAFE against a concurrent reader of the OLD segment -- latent, not live.
Making VECTOR-SEGMENTS a :SYNCHRONIZED hash table only guarantees that every
individual GETHASH/(SETF GETHASH) on the table is atomic; it says nothing
about a query thread that already holds the OLD segment object (fetched from
the table before this function ran) and is scanning it via SEGMENT-SCAN when
CLOSE-VECTOR-SEGMENT below unmaps it underneath that scan, nor about the NIL
window between REMHASH and the final (SETF GETHASH) -- during which a
concurrent VECTOR-SEARCH legitimately (if misleadingly, mid-rebuild) reports
\"nothing indexed\" rather than either the old or the new results. Both are
real gaps in a table-synchronization-only story.

This is safe TODAY only because of the caller discipline documented above:
REBUILD-VECTOR-SEGMENT runs from OPEN-GRAPH (before any query traffic exists)
or from tests, and never while VECTOR-SEARCH could be running concurrently.
If a future caller ever invokes this against a live graph, that invariant
must be re-examined -- e.g. by having VECTOR-SEARCH hold a strong reference to
the segment it looked up for the duration of its own scan (so a rebuild's
CLOSE-VECTOR-SEGMENT can't unmap out from under it) plus a defined answer for
the NIL window, rather than assuming quiescence.  Deliberately NOT fixed here:
out of scope for this task, and the right design (reference-counted segment
handles? RCU-style swap? a rebuild that queues behind in-flight scans?) is a
later design question, not a one-line patch."
  (let* ((key (cons owner-name slot-name))
         (table (vector-segments graph))
         (path (%segment-file graph owner-name slot-name)))
    (let ((old (gethash key table)))
      (when old (close-vector-segment old)))
    (remhash key table)
    (ignore-errors (delete-file path))
    ;; Counting pre-pass, so the fresh file is CREATED at its corpus size.
    ;;
    ;; WHY: a segment's mmap reservation is computed once, from the file's size
    ;; AT CREATE TIME -- today, %SEG-RESERVATION-FOR: max(*segment-min-
    ;; reservation* [16 GiB], *mmap-reservation-multiplier* x size) -- and the
    ;; segment can never grow past it in place.  Before *segment-min-reservation*
    ;; existed, creating at CREATE-VECTOR-SEGMENT's 1024 default made a fresh
    ;; file ~4 MB, 8x of which was far under the then-general *mmap-min-
    ;; reservation* (1 GiB) floor, so the reservation landed on that floor and
    ;; in-place doubling stalled at 131,072 entries.  RESTORE-VECTOR-SEGMENTS
    ;; calls this rebuild automatically whenever the clean-shutdown flag is
    ;; unset, i.e. after a hard crash -- so above 131,072 entries automatic
    ;; crash recovery could not complete at all.  Creating at the corpus size
    ;; derives the reservation from a realistic file instead, and removes ~8
    ;; doubling-and-relocate passes from the rebuild; the later 16 GiB segment
    ;; floor pushes the stall point far out regardless, but corpus-sized
    ;; creation is still what removes the relocate passes.
    ;;
    ;; SIZED EXACTLY, no growth headroom.  Whether that costs anything depends
    ;; on which side of the segment floor (currently 16 GiB; see %SEG-
    ;; RESERVATION-FOR) the rebuilt file lands on:
    ;;   - Below floor / multiplier (currently ~2 GiB of rebuilt file, i.e. an
    ;;     already-large corpus) the floor alone dominates the reservation
    ;;     regardless of any headroom multiple, so sizing exactly costs
    ;;     nothing there -- 2x headroom would NOT move the post-rebuild
    ;;     ceiling at all.
    ;;   - Above that threshold the reservation is proportional to the created
    ;;     size, so headroom is not merely a deferred first grow: 2x headroom
    ;;     would double the post-rebuild ceiling too, ~8x corpus -> ~16x.
    ;; Exact sizing is chosen either way: ~8x corpus is ample for recovery, and
    ;; the capacity a rebuild leaves behind should state what the corpus IS
    ;; rather than guess at future growth.  This is the one place that reasons
    ;; about rebuild headroom against the reservation formula -- keep it in
    ;; sync with %SEG-RESERVATION-FOR if the floor or multiplier ever change.
    ;;
    ;; CONSEQUENCE, accepted: a rebuild leaves live == capacity with an empty
    ;; free list, so the very next vector write does grow and relocate at once
    ;; -- one O(corpus) memcpy under the manager lock, per rebuild.  Safe (the
    ;; reservation is ample), but real; it is not deferred to some later
    ;; threshold.
    ;;
    ;; COST of the pre-pass itself: a second full O(corpus) MAP-VERTICES sweep,
    ;; which deserializes every node again via LOOKUP-VERTEX -- plausibly dearer
    ;; per node than the intra-mmap memcpys it removes, and unmeasured either
    ;; way.  It is bought for the RESERVATION, which is what made recovery
    ;; impossible above 131,072 entries; the relocation saving is a side effect,
    ;; not the justification.  Both passes use the identical predicate, so the
    ;; count cannot undershoot the puts.
    (let ((dimension nil)
          (live-count 0))
      (%sweep-vector-index-owner
       (lambda (node)
         (unless (deleted-p node)
           (let ((v (%node-segment-value node slot-name)))
             (when v
               (unless dimension (setf dimension (length v)))
               (incf live-count)))))
       graph owner-name)
      ;; No conforming vector anywhere: create no segment at all, as before.
      (when dimension
        (let ((seg (create-vector-segment
                    path dimension
                    ;; Never below CREATE-VECTOR-SEGMENT's own default: a small
                    ;; corpus gains nothing from a hair-tight file, and this
                    ;; keeps the small-graph shape exactly as it was.
                    :initial-capacity (max 1024 live-count))))
          (setf (gethash key table) seg)
          (%sweep-vector-index-owner
           (lambda (node)
             (unless (deleted-p node)
               (let ((v (%node-segment-value node slot-name)))
                 (when v
                   (segment-put seg (id node) v)))))
           graph owner-name)
          seg)))))

(defun rebuild-vector-segment-batched (graph owner-name slot-name
                                       &key (batch-size 5000) progress-fn)
  "ADDITIVELY fill the (OWNER-NAME, SLOT-NAME) segment from live nodes,
skipping ids the segment already holds, and calling PROGRESS-FN (if given)
roughly every BATCH-SIZE insertions.  Returns (values INSERTED SKIPPED).

Distinct from REBUILD-VECTOR-SEGMENT, which DROPS the existing segment and
rebuilds from scratch -- that is the right shape for recovery (e.g. an unclean
shutdown, see RESTORE-VECTOR-SEGMENTS), this is the right shape for migration:
an existing deployment upgrading into a newly-declared :VECTOR-INDEX slot has
chunks the live apply path never touched, so the segment must be filled in
without disturbing what a later feature (over-fetch/re-rank) may already be
reading from it.  Both are legitimate; do not merge them.

OWNER-NAME's contract is NOT the same as REBUILD-VECTOR-SEGMENT's.
REBUILD-VECTOR-SEGMENT does no resolution at all -- it uses its argument raw
for the segment key, the file path, and the %SWEEP-VECTOR-INDEX-OWNER sweep,
and its docstring requires the caller to already have resolved it to the true
owner. This function DOES resolve OWNER-NAME itself, through
%VECTOR-INDEX-SLOT-OWNER-NAME, so passing any class in the hierarchy -- the
true declaring owner OR a subclass -- reaches the same one owner segment.
Do not assume the two functions can be called the same way.

%SWEEP-VECTOR-INDEX-OWNER (MAP-VERTICES or MAP-EDGES, whichever OWNER is; GH
#57) is swept from the RESOLVED owner with its default :INCLUDE-SUBCLASSES-P
T, so every subclass instance is visited into the ONE shared owner segment.
This is Model B: one segment per declaring class spans its subclasses,
exactly matching what APPLY-TX-WRITE-TO-VECTOR-SEGMENTS
maintains on the live create/update/delete path.  Getting this wrong -- e.g.
sweeping on the raw, unresolved OWNER-NAME instead of the resolved owner, or
keying on a node's exact runtime class instead of the resolved owner --
previously produced a real LIVE-vs-REBUILT segment divergence.

RESUMABLE BY CONSTRUCTION -- OF THIS FUNCTION SPECIFICALLY, not of the
segment's crash recovery as a whole.  The segment itself records which ids it
holds (SEGMENT-GET), so an id already present is skipped and an interrupted
run (an in-process abort of this call, e.g. a condition or a killed thread,
followed by a CLEAN CLOSE-GRAPH) simply leaves a partial segment for the next
call to finish additively -- the cheap path.  This says nothing about
surviving a HARD crash (process killed, power loss): that is a different
mechanism entirely, described under RESTORE-VECTOR-SEGMENTS above -- the
segment's own dirty flag (see the file header) forces a FULL
REBUILD-VECTOR-SEGMENT (drop and rebuild from scratch) on the next open, not
a resume of this function.  An operator who reads \"resumable\" here and
expects a cheap skip-scan after a hard crash will get a full re-index
instead; both are correct, they just apply to different failure modes.
There is deliberately NO progress file or checkpoint record kept alongside
this function's own resumability: a marker that can disagree with the
segment (e.g. because the write it described was rolled back, or the process
died between the marker write and the segment write) is strictly worse than
no marker, because it can claim work is done when the segment says
otherwise. The segment is the only source of truth.

CONCURRENCY.  The caller must not invoke this concurrently with another
migration of the SAME segment (whether via this function or via
REBUILD-VECTOR-SEGMENT) -- %ENSURE-SEGMENT's get-or-create is non-atomic, and
two concurrent creators of the same (OWNER . SLOT-NAME) segment could each
CREATE-VECTOR-SEGMENT the same path, with one losing the table and its
writes. This function calls %ENSURE-SEGMENT at most ONCE per invocation
(lazily, on the first conforming vector seen, then reused for the rest of the
sweep), which narrows -- but does not close -- that window; serialize
migrations of a given segment at the call site. Credit where due: the READER
side is already safe against a concurrent migration, because SEGMENT-PUT,
SEGMENT-GET, SEGMENT-SCAN and SEGMENT-SCORE-SUBSET all take the segment's own
per-segment rw-lock, so a concurrent VECTOR-SEARCH sees a consistent (if
incomplete) snapshot of the segment while this runs, never a torn read.
Deliberately does NOT take the transaction manager lock around the whole
migration -- a long hold would stall every commit in the database for the
migration's entire duration (a scan holding its lock is already a noted
commit-latency bound elsewhere in this codebase).

DESTRUCTIVE CORNER, inherited from %ENSURE-SEGMENT: if a segment FILE already
exists on disk for (OWNER . SLOT-NAME) but is not registered in
VECTOR-SEGMENTS, %ENSURE-SEGMENT treats it as absent and calls
CREATE-VECTOR-SEGMENT, which rewrites the header and free-marks its capacity
-- destroying whatever was already on disk. Unreachable via the normal open
path (RESTORE-VECTOR-SEGMENTS registers any segment file whose owner class is
still in the schema), but reachable if a :VECTOR-INDEX declaration is removed
and later re-added with a stale file left behind from the earlier
declaration.

BATCH-SIZE is a progress-reporting CADENCE, not a memory-bounding buffer:
SEGMENT-PUT writes straight to the mmap outside any transaction, so nothing
about deferring writes bounds a transaction -- each conforming vector is put
as soon as it is encountered, with nothing queued in memory.  PROGRESS-FN, if
given, is called as (funcall progress-fn done seen) after every BATCH-SIZE-th
insertion (and once more at the end for any partial remainder) -- DONE is
inserted+skipped so far, SEEN is the number of conforming nodes encountered
so far IN THIS RUN, not the corpus's grand total (which isn't known ahead of
time without a separate pass, and this function does not compute one).  A
migration over a real corpus takes minutes, not seconds, and a silent one
looks hung."
  (let* ((owner (%vector-index-slot-owner-name (find-class owner-name) slot-name))
         (key (cons owner slot-name))
         (seg (gethash key (vector-segments graph)))
         (inserted 0)
         (skipped 0)
         (seen 0)
         (since-progress 0))
    (%sweep-vector-index-owner
     (lambda (node)
       (unless (deleted-p node)
         (let ((v (%node-segment-value node slot-name)))
           (when v
             (incf seen)
             (unless seg
               (setf seg (%ensure-segment graph owner slot-name (length v))))
             (if (segment-get seg (id node))
                 (incf skipped)
                 (progn
                   (segment-put seg (id node) v)
                   (incf inserted)
                   (incf since-progress)
                   (when (and progress-fn (>= since-progress batch-size))
                     (setf since-progress 0)
                     (funcall progress-fn (+ inserted skipped) seen))))))))
     graph owner)
    (when (and progress-fn (plusp since-progress))
      (funcall progress-fn (+ inserted skipped) seen))
    (values inserted skipped)))

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

(defun segment-remove (segment id)
  "Remove ID from the segment, pushing its slot onto the free list.  Returns T
if ID was present, NIL otherwise.  A freed slot's id-array cell is marked with
+FREE-SLOT-MARKER+ (first 8 bytes) and the previous free-head (second 8 bytes),
threading the free list; its vector-block bytes are left as-is (unreachable).

Takes the segment's WRITE lock: mutations are exclusive against concurrent
scans."
  (with-write-lock ((segment-lock segment))
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
            t)))))

;;; Query layer: segment-scan is a bounded top-k full-cosine sweep.  It takes
;;; the segment's READ lock (mutations take the write side, so a scan is safe
;;; against a concurrent growing commit) and touches ONLY the id array and the
;;; contiguous vector block -- it never materialises a node, which is the
;;; entire performance premise of the segment.

(defun %vector-norm (v)
  "Euclidean norm of V."
  (declare (type (simple-array single-float (*)) v)
           (optimize (speed 3) (safety 1)))
  (let ((sum 0f0))
    (declare (type single-float sum))
    (dotimes (i (length v) (sqrt sum))
      (incf sum (* (aref v i) (aref v i))))))

(defun %cosine-with-norm (a a-norm b)
  "Cosine similarity of A against B, where A's Euclidean norm is already known
as A-NORM.  Lets a caller that scores many B's against the same A (e.g. a scan
over a whole segment) hoist A's norm out of the per-candidate loop instead of
recomputing it once per candidate.  Returns 0.0 when either norm is zero (no
divide error).  Does NOT assume unit vectors."
  (declare (type (simple-array single-float (*)) a b)
           (type single-float a-norm)
           (optimize (speed 3) (safety 1)))
  (let ((dot 0f0) (nb 0f0))
    (declare (type single-float dot nb))
    (dotimes (i (min (length a) (length b)))
      (let ((x (aref a i)) (y (aref b i)))
        (incf dot (* x y))
        (incf nb (* y y))))
    (if (or (zerop a-norm) (zerop nb))
        0f0
        (/ dot (* a-norm (sqrt nb))))))

(defun %cosine (a b)
  "Full cosine similarity of two equal-length single-float vectors.  Returns
0.0 when either has zero norm (no divide error).  This does NOT assume unit
vectors -- the segment stores whatever the caller put in it.

General two-vector entry point; implemented via %COSINE-WITH-NORM.  A caller
scoring many B's against the same A (segment-scan) should call
%COSINE-WITH-NORM directly with a hoisted A-norm instead of using this."
  (declare (type (simple-array single-float (*)) a b))
  (%cosine-with-norm a (%vector-norm a) b))

(defun segment-scan (segment query-vector k)
  "Top-K by full cosine over every occupied slot, best first, as (score . id)
conses.  Takes the segment's READ lock, so it is safe against a concurrent
growing commit (which holds the write lock).

Touches ONLY the id array and the contiguous vector block -- it never
materialises a node, which is the entire point of the segment.

Sweeps [0, capacity) skipping free cells -- occupied slots are NOT dense
[0, live-count) once the free list has been used.

QUERY-VECTOR's length must equal the segment's dimension -- validated up
front, mirroring SEGMENT-PUT's write-side check, so a wrong-length query is
rejected loudly instead of silently scored against a prefix.

The query's own norm is computed ONCE here and threaded through
%COSINE-WITH-NORM for every candidate, rather than recomputed per occupied
slot."
  (declare (type (simple-array single-float (*)) query-vector))
  (unless (= (length query-vector) (segment-dimension segment))
    (error "query vector length ~D does not match segment dimension ~D"
           (length query-vector) (segment-dimension segment)))
  (let ((qnorm (%vector-norm query-vector)))
    (when (or (zerop k) (zerop qnorm))
      (return-from segment-scan nil))
    (with-read-lock ((segment-lock segment))
      (let* ((mmap (segment-mmap segment))
             (cap (segment-capacity segment))
             (collector (%make-topk k))
             ;; ONE scratch vector for the whole sweep instead of a fresh
             ;; dim-wide array per candidate: %cosine-with-norm consumes it
             ;; immediately and never retains it, and the buffer is local to
             ;; this call, so no other thread can see it.
             (v (make-array (segment-dimension segment)
                            :element-type 'single-float)))
        (dotimes (slot cap)
          (unless (= (deserialize-uint64 mmap (%seg-id-offset slot)) +free-slot-marker+)
            (let ((id (get-bytes mmap (%seg-id-offset slot) +key-bytes+)))
              (%seg-read-vector-into segment slot v)
              (%topk-offer collector (%cosine-with-norm query-vector qnorm v) id))))
        (%topk-results collector)))))

(defun segment-score-subset (segment query-vector node-ids)
  "Score only NODE-IDS against QUERY-VECTOR by full cosine, best first, as
 (score . id) conses.  Ids absent from the segment are silently skipped --
this is required behavior, not an error: the caller (a future ANN index or
an int8 pre-rank) proposes a candidate set that this function is not
expected to fully own.

This is the extension seam that keeps ANN addable: nothing here assumes it
has seen every vector in the segment, unlike SEGMENT-SCAN which sweeps
[0, capacity).

QUERY-VECTOR's length must equal the segment's dimension -- validated up
front, mirroring SEGMENT-SCAN's check, so a wrong-length query is rejected
loudly instead of silently scored against a prefix.

The query's own norm is computed ONCE here and threaded through
%COSINE-WITH-NORM for every candidate, rather than recomputed per id.

There is no k here -- the caller already supplied the candidate set -- so
results are sorted into the same best-first total order SEGMENT-SCAN uses
(score DESCENDING, node-id ASCENDING) via %SCORE-BEFORE-P, without being
bounded to any k.

NODE-IDS is NOT de-duplicated: an id appearing twice is scored twice and
appears twice in the result, adjacently (equal score and equal id tie under
%SCORE-BEFORE-P).  This matters for the ANN seam -- a candidate generator that
unions several probe lists can easily emit duplicates -- so de-duplicate on
the caller's side if that is not what you want.

Takes the segment's READ lock, like SEGMENT-SCAN: mutations take the write
side, so this is safe against a concurrent growing commit."
  (declare (type (simple-array single-float (*)) query-vector))
  (unless (= (length query-vector) (segment-dimension segment))
    (error "query vector length ~D does not match segment dimension ~D"
           (length query-vector) (segment-dimension segment)))
  (let ((qnorm (%vector-norm query-vector)))
    (when (or (null node-ids) (zerop qnorm))
      (return-from segment-score-subset nil))
    (with-read-lock ((segment-lock segment))
      (let ((out '())
            ;; One reusable scratch vector, as in SEGMENT-SCAN.
            (v (make-array (segment-dimension segment)
                           :element-type 'single-float)))
        (dolist (id node-ids)
          (let ((slot (%seg-slot-of segment id)))
            (when slot
              (%seg-read-vector-into segment slot v)
              (push (cons (%cosine-with-norm query-vector qnorm v) id) out))))
        (sort out (lambda (a b)
                    (%score-before-p (car a) (cdr a) (car b) (cdr b))))))))
