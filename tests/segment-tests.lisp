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
                  (let ((back (segment-get s (%id 7))))
                    (is (typep back '(simple-array single-float (*)))
                        "overwritten id must still read back a vector")
                    (is (= 32 (length back)))
                    (is (every #'= (%vec 32 5.0) back))))
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
                    (let ((back (segment-get s (%id 1))))
                      (is (typep back '(simple-array single-float (*)))
                          "id 1 did not survive reopen (segment-get returned ~S)" back)
                      (is (= 48 (length back)))
                      (is (every #'= (%vec 48 1.0) back)))
                    (let ((back (segment-get s (%id 2))))
                      (is (typep back '(simple-array single-float (*)))
                          "id 2 did not survive reopen (segment-get returned ~S)" back)
                      (is (= 48 (length back)))
                      (is (every #'= (%vec 48 2.0) back))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

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
                    (let ((back (segment-get s (%id 2))))
                      (is (typep back '(simple-array single-float (*)))
                          "id 2 lost when the sweep skipped the freed hole (got ~S)" back)
                      (is (= 16 (length back)))
                      (is (every #'= (%vec 16 2.0) back))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

(test segment-fresh-capacity-has-no-phantom-id
  "A segment created with spare capacity (more slots than ids put) must not
resurrect an all-zero phantom id from the never-written tail of the id array
after close/reopen -- create-vector-segment must pre-mark unused slots free,
just like a real remove does, so the sweep skips them instead of reading raw
zero bytes as a valid 16-byte id."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 8 :initial-capacity 8)))
             (segment-put s (%id 1) (%vec 8 1.0))
             (segment-put s (%id 2) (%vec 8 2.0))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (let ((table (segment-id->slot s))
                        (zero-id (make-array 16 :element-type '(unsigned-byte 8)
                                                 :initial-element 0)))
                    (is (= 2 (hash-table-count table))
                        "id->slot must have exactly 2 entries, not one per phantom")
                    (is (null (gethash zero-id table))
                        "an all-zero phantom id must not be present after reopen"))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

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
                    (let ((back (segment-get s (%id i))))
                      (is (typep back '(simple-array single-float (*)))
                          "vector ~D lost during growth (segment-get returned ~S)" i back)
                      (is (= 32 (length back)))
                      (is (every #'= (%vec 32 (coerce i 'single-float)) back)
                          "vector ~D corrupted by growth" i))))
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
                      (let ((back (segment-get s (%id i))))
                        (is (typep back '(simple-array single-float (*)))
                            "vector ~D lost across reopen (segment-get returned ~S)" i back)
                        (is (= 24 (length back)))
                        (is (every #'= (%vec 24 (coerce i 'single-float)) back)
                            "vector ~D corrupted across reopen" i))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

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
             )
           ;; reopen after the un-closed session
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (is (not (segment-clean-shutdown-p s))
                      "a segment left open (crash) must reopen NOT clean")
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; The segment reservation floor (%SEG-RESERVATION-FOR).
;;; ---------------------------------------------------------------------------

(test segment-reservation-floor-on-create
  "A freshly created segment reserves at least *SEGMENT-MIN-RESERVATION*.
Without its own floor a default 1024-slot file is a few MB, 8x of which is far
under *MMAP-MIN-RESERVATION*, so the reservation landed on the general 1 GiB
floor and in-place growth stalled a few doublings later."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (create-vector-segment path 64 :initial-capacity 16)))
           (unwind-protect
                (is (>= (graph-db::m-reserved-size (graph-db::segment-mmap s))
                        graph-db::*segment-min-reservation*)
                    "a created segment reserved ~D bytes; the floor is ~D"
                    (graph-db::m-reserved-size (graph-db::segment-mmap s))
                    graph-db::*segment-min-reservation*)
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-reservation-floor-on-reopen
  "A REOPENED segment gets the floor too.  OPEN-VECTOR-SEGMENT is a separate
MMAP-FILE call site from CREATE-VECTOR-SEGMENT; before *SEGMENT-MIN-RESERVATION*
existed neither call site passed an explicit :RESERVATION at all (both just
took MMAP-FILE's general default).  OPEN-VECTOR-SEGMENT is the call site that
matters most in practice, since a long-lived graph runs on reopened segments,
not freshly created ones."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (create-vector-segment path 64 :initial-capacity 16)))
             (segment-put s (%id 1) (%vec 64 1.0))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (is (>= (graph-db::m-reserved-size (graph-db::segment-mmap s))
                          graph-db::*segment-min-reservation*)
                      "a reopened segment reserved ~D bytes; the floor is ~D"
                      (graph-db::m-reserved-size (graph-db::segment-mmap s))
                      graph-db::*segment-min-reservation*)
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

(test segment-reservation-takes-the-larger-of-floor-and-multiple
  "When multiplier x size exceeds the floor, the MULTIPLE wins -- the floor is a
floor, not a cap.  A segment already larger than floor/multiplier must keep
proportional headroom; passing the bare floor would SHRINK the reservation for
exactly the largest segments.  Constructed by binding the floor small and the
multiplier large, so the MAX in %SEG-RESERVATION-FOR is genuinely exercised
rather than assumed.  *MMAP-MIN-RESERVATION* is bound down as well: otherwise
MMAP-FILE's own 1 GiB floor would dominate and mask the result."
  (let ((path (%seg-path)))
    (unwind-protect
         (let* ((graph-db::*mmap-min-reservation* 4096)
                (graph-db::*mmap-reservation-multiplier* 64)
                (graph-db::*segment-min-reservation* (* 64 1024))
                (bytes (graph-db::%seg-file-bytes 1024 64))
                (s (create-vector-segment path 64 :initial-capacity 1024)))
           (unwind-protect
                (progn
                  (is (> (* 64 bytes) graph-db::*segment-min-reservation*)
                      "test setup is broken: multiplier x size (~D) must exceed ~
                       the floor (~D) for this to test anything"
                      (* 64 bytes) graph-db::*segment-min-reservation*)
                  (is (= (* 64 bytes)
                         (graph-db::m-reserved-size (graph-db::segment-mmap s)))
                      "expected the multiple ~D to win over the floor ~D; ~
                       reserved ~D"
                      (* 64 bytes) graph-db::*segment-min-reservation*
                      (graph-db::m-reserved-size (graph-db::segment-mmap s))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; Growth PAST the reservation: re-reserve and relocate (%SEG-ENSURE-RESERVATION
;;; / RELOCATE-VECTOR-SEGMENT-MAPPING).  The direct inverse of wave 1's
;;; exhaustion test: what used to be a hard ceiling is now a relocation.
;;; ---------------------------------------------------------------------------

(defun %seg-base-address (s)
  (cffi:pointer-address (graph-db::m-pointer (graph-db::segment-mmap s))))

(defmacro with-tiny-segment-reservation (&body body)
  "Run BODY with a segment's reservation policy set to \"exactly the file size\":
floors below any real file, multiplier 1.  The FIRST %SEG-GROW then has nowhere
to grow in place, so the relocation path is entered deterministically instead of
after however many doublings the real 16 GiB floor affords."
  `(let ((graph-db::*mmap-min-reservation* 4096)
         (graph-db::*mmap-reservation-multiplier* 1)
         (graph-db::*segment-min-reservation* 4096))
     ,@body))

(test segment-growth-past-the-reservation-relocates
  "Growing past the mmap reservation now MOVES the mapping instead of signalling.
The base pointer must change (that is the whole point -- and what makes this
unsafe for the heap, which has no read lock), the reservation must be larger
afterwards, and every vector written before the move must read back bit-exactly
from the new address.  Also asserts no SEGV-retry fired: relocation happens
under the write lock, so no access should ever touch the old window."
  (let ((path (%seg-path)))
    (unwind-protect
         (let* ((n 200)
                (retries-before graph-db::*mmap-segv-retries*)
                (s (with-tiny-segment-reservation
                     (create-vector-segment path 64 :initial-capacity 32))))
           (unwind-protect
                (let ((base-before (%seg-base-address s))
                      (reserved-before
                        (graph-db::m-reserved-size (graph-db::segment-mmap s))))
                  (is (= reserved-before
                         (graph-db::mapped-file-length (graph-db::segment-mmap s)))
                      "test setup is broken: the segment must start with ZERO ~
                       growth headroom (reserved ~D vs file ~D), or the first ~
                       grow would not relocate"
                      reserved-before
                      (graph-db::mapped-file-length (graph-db::segment-mmap s)))
                  (dotimes (i n)
                    (segment-put s (%id i) (%vec 64 (float i 1.0))))
                  (is (= n (segment-live-count s)))
                  (is (/= base-before (%seg-base-address s))
                      "the mapping should have relocated; base address is still ~D"
                      base-before)
                  (is (> (graph-db::m-reserved-size (graph-db::segment-mmap s))
                         reserved-before)
                      "the reservation should have grown past ~D; it is ~D"
                      reserved-before
                      (graph-db::m-reserved-size (graph-db::segment-mmap s)))
                  ;; Every vector, including the ones written before the move.
                  (let ((bad 0))
                    (dotimes (i n)
                      (let ((back (segment-get s (%id i)))
                            (want (%vec 64 (float i 1.0))))
                        (unless (and back (every #'= want back)) (incf bad))))
                    (is (zerop bad)
                        "~D of ~D vectors did not survive the relocation intact"
                        bad n))
                  (is (= retries-before graph-db::*mmap-segv-retries*)
                      "relocation must not produce SEGV-retries (~D new)"
                      (- graph-db::*mmap-segv-retries* retries-before)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-relocation-survives-a-reopen
  "A relocated segment is still a valid file: the move is purely a
virtual-address change (same fd, same bytes), so closing and reopening must find
the grown capacity and every vector.  Guards against the relocation quietly
mapping the wrong length or the wrong offset, which an in-process read could
mask."
  (let ((path (%seg-path)))
    (unwind-protect
         (progn
           (let ((s (with-tiny-segment-reservation
                      (create-vector-segment path 64 :initial-capacity 32))))
             (dotimes (i 100)
               (segment-put s (%id i) (%vec 64 (float i 1.0))))
             (close-vector-segment s))
           (let ((s (open-vector-segment path)))
             (unwind-protect
                  (progn
                    (is (= 100 (segment-live-count s)))
                    (is (>= (segment-capacity s) 100))
                    (let ((bad 0))
                      (dotimes (i 100)
                        (let ((back (segment-get s (%id i))))
                          (unless (and back (every #'= (%vec 64 (float i 1.0)) back))
                            (incf bad))))
                      (is (zerop bad)
                          "~D vectors were wrong after reopening a relocated segment"
                          bad)))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

(test segment-relocation-can-be-switched-off
  "With *SEGMENT-RELOCATE-ON-EXHAUSTION* NIL, growth past the reservation
signals VECTOR-SEGMENT-CAPACITY-EXHAUSTED again -- the pre-wave-2 behaviour,
kept as an operator kill-switch.  The failed grow must leave the segment intact
and usable: nothing claimed, LIVE-COUNT unchanged, earlier vectors still
readable.  This is also the mechanism the transaction-level rollback test uses
to reach the abort path at all."
  (let ((path (%seg-path)))
    (unwind-protect
         (let ((s (with-tiny-segment-reservation
                    (create-vector-segment path 64 :initial-capacity 32))))
           (unwind-protect
                (let ((graph-db::*segment-relocate-on-exhaustion* nil))
                  (dotimes (i 32)
                    (segment-put s (%id i) (%vec 64 (float i 1.0))))
                  (is (= 32 (segment-live-count s)))
                  (signals graph-db::vector-segment-capacity-exhausted
                    (segment-put s (%id 32) (%vec 64 32.0)))
                  (is (= 32 (segment-live-count s))
                      "a refused grow must not have claimed a slot")
                  (is (= 32 (segment-capacity s))
                      "a refused grow must not have changed capacity")
                  (is (null (segment-get s (%id 32))))
                  (let ((bad 0))
                    (dotimes (i 32)
                      (let ((back (segment-get s (%id i))))
                        (unless (and back (every #'= (%vec 64 (float i 1.0)) back))
                          (incf bad))))
                    (is (zerop bad) "~D vectors damaged by the refused grow" bad)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test segment-reader-blocked-during-relocation-sees-consistent-data
  "A concurrent reader must never observe the move.  SEGMENT-GET takes the read
side of the segment's rw-lock and %SEG-GROW the write side, which is the ENTIRE
safety argument for relocating a mapping at all (the heap and linear hash have
no such lock -- see RELOCATE-VECTOR-SEGMENT-MAPPING).  A reader thread hammers a
slot written before the first relocation while the main thread grows the segment
through several of them; every read must return the same bytes, and no read may
fault."
  (let ((path (%seg-path)))
    (unwind-protect
         (let* ((retries-before graph-db::*mmap-segv-retries*)
                (s (with-tiny-segment-reservation
                     (create-vector-segment path 64 :initial-capacity 32)))
                (want (%vec 64 7.0))
                (done nil)
                (reads 0)
                (bad 0)
                (err nil))
           (unwind-protect
                (let ((base-before nil) (reader nil))
                  (segment-put s (%id 0) want)
                  (setf base-before (%seg-base-address s))
                  (setf reader
                        (bordeaux-threads:make-thread
                         (lambda ()
                           (handler-case
                               (loop until done
                                     do (let ((back (segment-get s (%id 0))))
                                          (incf reads)
                                          (unless (and back (every #'= want back))
                                            (incf bad))))
                             (error (e) (setf err e))))
                         :name "segment-relocation-reader"))
                  (dotimes (i 400)
                    (segment-put s (%id (1+ i)) (%vec 64 (float i 1.0))))
                  (setf done t)
                  (bordeaux-threads:join-thread reader)
                  (is (null err) "the reader thread failed: ~A" err)
                  (is (plusp reads) "the reader never ran; the test proved nothing")
                  (is (zerop bad) "~D of ~D concurrent reads were torn or wrong"
                      bad reads)
                  (is (/= base-before (%seg-base-address s))
                      "no relocation happened during the run; the test proved nothing")
                  (is (= retries-before graph-db::*mmap-segv-retries*)
                      "~D SEGV-retries fired during relocation; a reader touched ~
                       the old window"
                      (- graph-db::*mmap-segv-retries* retries-before)))
             (progn (setf done t) (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; Relocation that FAILS.  This is the path that actually fires in production:
;;; RLIMIT_AS or virtual-address exhaustion refusing the new reservation, with
;;; relocation switched ON.  It is NOT the same path as
;;; *SEGMENT-RELOCATE-ON-EXHAUSTION* = NIL, which signals before issuing any
;;; syscall; the two share only what is downstream of the signal, so the
;;; kill-switch test does not cover this and never did.
;;;
;;; Made deterministic by fault-injecting the anonymous PROT_NONE reservation
;;; mmap -- the same FDEFINITION-swap technique as
;;; MMAP-FILE-CLOSES-FD-ON-FAILED-FILE-MAP below -- rather than by trying to
;;; genuinely exhaust the address space of the test process.
;;; ---------------------------------------------------------------------------

(defmacro with-failing-segment-reservation ((counter-place &key (fail-on 1)
                                                             (stage :reservation))
                                            &body body)
  "Run BODY with RELOCATE-VECTOR-SEGMENT-MAPPING's FAIL-ON'th relocation made to
fail deterministically, at STAGE.  Restores the real %POSIX-MMAP on the way out,
so a test can go on to prove the segment is still usable once the injected
pressure is gone.

STAGE :RESERVATION (the default) fails the anonymous PROT_NONE window — the
first of the two mmaps, and the one address-space exhaustion really refuses.
STAGE :FILE-MAP lets that window be reserved and fails the MAP_FIXED file map
that follows, which is the ONLY way to reach the primitive's rollback arm (the
one that has a window to release).

COUNTER-PLACE is INCF'd on every relocation attempt (failed or not), so a test
can assert the injection actually fired instead of silently proving nothing.
The caller establishes it; it survives the macro.

HOW THE TWO MMAPS ARE TOLD APART: the reservation has a NULL address and fd -1;
the file map is whatever call comes immediately after it.  EXTEND-MAPPED-FILE's
re-map passes a non-null address and is not preceded by a reservation, so it is
never touched.  MMAP-FILE's own reservation has the same shape as a relocation's,
so create/open the segment OUTSIDE this macro."
  (let ((orig (gensym "ORIG"))
        (pending (gensym "PENDING")))
    `(let ((,orig (fdefinition 'graph-db::%posix-mmap))
           (,pending nil))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db::%posix-mmap)
                    (lambda (addr length prot flags fd offset)
                      (cond
                        ((and (cffi:null-pointer-p addr) (= fd -1))
                         (if (= (incf ,counter-place) ,fail-on)
                             (ecase ,stage
                               (:reservation
                                (error "injected reservation failure for the test"))
                               (:file-map
                                (setf ,pending t)
                                (funcall ,orig addr length prot flags fd offset)))
                             (funcall ,orig addr length prot flags fd offset)))
                        (,pending
                         (setf ,pending nil)
                         (error "injected file-map failure for the test"))
                        (t
                         (funcall ,orig addr length prot flags fd offset)))))
              ,@body)
         (setf (fdefinition 'graph-db::%posix-mmap) ,orig)))))

(test segment-relocation-failure-leaves-the-segment-consistent
  "A relocation that GENUINELY FAILS must signal
VECTOR-SEGMENT-CAPACITY-EXHAUSTED and leave the segment byte-for-byte as it
was: OLD MAPPING STILL LIVE (M-POINTER unchanged -- the primitive publishes the
new base only after both mmaps succeed, and rolls the reservation back
otherwise), reservation unchanged, FILE LENGTH unchanged (%SEG-GROW re-reserves
BEFORE it extends), capacity and live-count unchanged, no slot claimed, and
every stored vector still readable through that still-live mapping.

Then, with the injected pressure removed, the same put must SUCCEED -- a
segment that survives the failure but is wedged afterwards would satisfy every
assertion above and still be broken."
  (let ((path (%seg-path)))
    (unwind-protect
         (with-tiny-segment-reservation
           (let ((s (create-vector-segment path 64 :initial-capacity 32))
                 (reservations 0))
             (unwind-protect
                  (progn
                    ;; Exactly fills the initial capacity; the NEXT put grows.
                    (dotimes (i 32)
                      (segment-put s (%id i) (%vec 64 (float i 1.0))))
                    (let* ((mmap (graph-db::segment-mmap s))
                           (base-before (%seg-base-address s))
                           (reserved-before (graph-db::m-reserved-size mmap))
                           (file-before (graph-db::mapped-file-length mmap)))
                      (is (= reserved-before file-before)
                          "test setup is broken: the segment must start with ZERO ~
                           growth headroom, or the failing grow would not relocate")
                      (with-failing-segment-reservation (reservations :fail-on 1)
                        (signals graph-db::vector-segment-capacity-exhausted
                          (segment-put s (%id 32) (%vec 64 32.0))))
                      (is (= 1 reservations)
                          "fault injection never fired -- the test setup is ~
                           broken, not the code under test")
                      (is (= base-before (%seg-base-address s))
                          "a FAILED relocation must not move M-POINTER; the old ~
                           window is what readers are still using")
                      (is (= reserved-before (graph-db::m-reserved-size mmap))
                          "a failed relocation must not change the reservation")
                      (is (= file-before (graph-db::mapped-file-length mmap))
                          "the reservation is checked BEFORE the file is ~
                           extended, so a failed relocation must not have grown ~
                           the file")
                      (is (= 32 (segment-live-count s))
                          "a failed grow must not have claimed a slot")
                      (is (= 32 (segment-capacity s))
                          "a failed grow must not have changed capacity")
                      (is (null (segment-get s (%id 32))))
                      (let ((bad 0))
                        (dotimes (i 32)
                          (let ((back (segment-get s (%id i))))
                            (unless (and back (every #'= (%vec 64 (float i 1.0)) back))
                              (incf bad))))
                        (is (zerop bad)
                            "~D vectors unreadable after a failed relocation -- ~
                             the old mapping should still be live" bad))
                      ;; Usable, not merely undamaged.
                      (segment-put s (%id 32) (%vec 64 32.0))
                      (is (= 33 (segment-live-count s))
                          "the segment must still be usable once the address ~
                           space pressure is gone")
                      (is (every #'= (%vec 64 32.0) (segment-get s (%id 32))))
                      (is (/= base-before (%seg-base-address s))
                          "the retried grow should have relocated for real")))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

(test segment-relocation-failure-on-a-later-grow-leaves-the-segment-consistent
  "The same, but failing on the SECOND relocation rather than the first, i.e.
after an earlier doubling has already relocated the mapping and extended the
file.  That is the shape the transaction path's multi-doubling grow loop takes,
and the state a failure has to be consistent against is the state the FIRST
relocation left behind -- not the pristine one the previous test starts from.

Deliberately runs the whole body inside WITH-TINY-SEGMENT-RESERVATION so the
reservation policy stays \"exactly the file size\" at GROW time too: every
doubling then relocates, which is what makes a second relocation reachable at
all."
  (let ((path (%seg-path)))
    (unwind-protect
         (with-tiny-segment-reservation
           (let ((s (create-vector-segment path 64 :initial-capacity 32))
                 (reservations 0))
             (unwind-protect
                  (let ((base-at-create (%seg-base-address s))
                        (mmap (graph-db::segment-mmap s))
                        (base-after-first nil)
                        (reserved-after-first nil)
                        (file-after-first nil))
                    (dotimes (i 32)
                      (segment-put s (%id i) (%vec 64 (float i 1.0))))
                    (with-failing-segment-reservation (reservations :fail-on 2)
                      ;; Grow #1 (32 -> 64): relocation SUCCEEDS.
                      (dotimes (i 32)
                        (segment-put s (%id (+ 32 i)) (%vec 64 (float (+ 32 i) 1.0))))
                      (is (= 1 reservations)
                          "the first grow should have relocated exactly once (~D)"
                          reservations)
                      (is (= 64 (segment-capacity s)))
                      (setf base-after-first (%seg-base-address s)
                            reserved-after-first (graph-db::m-reserved-size mmap)
                            file-after-first (graph-db::mapped-file-length mmap))
                      (is (/= base-at-create base-after-first)
                          "the first relocation did not happen; the test proves ~
                           nothing about a SECOND one")
                      ;; Grow #2 (64 -> 128): relocation FAILS.
                      (signals graph-db::vector-segment-capacity-exhausted
                        (segment-put s (%id 64) (%vec 64 64.0))))
                    (is (= 2 reservations)
                        "the second relocation was never attempted -- test setup ~
                         is broken, not the code under test")
                    (is (= base-after-first (%seg-base-address s))
                        "the failed second relocation must leave the mapping the ~
                         FIRST one published")
                    (is (= reserved-after-first (graph-db::m-reserved-size mmap)))
                    (is (= file-after-first (graph-db::mapped-file-length mmap)))
                    (is (= 64 (segment-live-count s)))
                    (is (= 64 (segment-capacity s)))
                    (is (null (segment-get s (%id 64))))
                    (let ((bad 0))
                      (dotimes (i 64)
                        (let ((back (segment-get s (%id i))))
                          (unless (and back (every #'= (%vec 64 (float i 1.0)) back))
                            (incf bad))))
                      (is (zerop bad)
                          "~D of 64 vectors -- including the 32 that were copied ~
                           by the FIRST relocation -- did not survive the failed ~
                           second one" bad))
                    ;; Usable again.
                    (segment-put s (%id 64) (%vec 64 64.0))
                    (is (= 65 (segment-live-count s)))
                    (is (= 128 (segment-capacity s)))
                    (is (every #'= (%vec 64 64.0) (segment-get s (%id 64)))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

(test segment-relocation-failure-at-the-file-map-rolls-the-reservation-back
  "The other half of the relocation primitive's failure surface: the anonymous
window IS reserved and the MAP_FIXED file map into it fails.  This is the only
case that reaches the rollback arm, which must release the window it just took
-- on the one path whose failure mode is address-space pressure, a rollback that
silently leaked its own reservation would make things worse, so the munmap's
return code is checked and a failure WARNs.  The test asserts no such warning
was signalled, alongside the same leave-it-consistent-and-usable checks."
  (let ((path (%seg-path)))
    (unwind-protect
         (with-tiny-segment-reservation
           (let ((s (create-vector-segment path 64 :initial-capacity 32))
                 (reservations 0)
                 (warnings '()))
             (unwind-protect
                  (progn
                    (dotimes (i 32)
                      (segment-put s (%id i) (%vec 64 (float i 1.0))))
                    (let* ((mmap (graph-db::segment-mmap s))
                           (base-before (%seg-base-address s))
                           (reserved-before (graph-db::m-reserved-size mmap))
                           (file-before (graph-db::mapped-file-length mmap)))
                      (with-failing-segment-reservation (reservations :fail-on 1
                                                                      :stage :file-map)
                        (handler-bind ((warning (lambda (w)
                                                  (push w warnings)
                                                  (muffle-warning w))))
                          (signals graph-db::vector-segment-capacity-exhausted
                            (segment-put s (%id 32) (%vec 64 32.0)))))
                      (is (= 1 reservations)
                          "fault injection never fired -- test setup is broken")
                      (is (null warnings)
                          "the rollback munmap failed (~{~A~^; ~}) -- the ~
                           reservation it had just taken is leaked" warnings)
                      (is (= base-before (%seg-base-address s))
                          "M-POINTER is published only after BOTH mmaps succeed")
                      (is (= reserved-before (graph-db::m-reserved-size mmap))
                          "the reservation size is published only after both ~
                           mmaps succeed")
                      (is (= file-before (graph-db::mapped-file-length mmap)))
                      (is (= 32 (segment-live-count s)))
                      (is (= 32 (segment-capacity s)))
                      (let ((bad 0))
                        (dotimes (i 32)
                          (let ((back (segment-get s (%id i))))
                            (unless (and back (every #'= (%vec 64 (float i 1.0)) back))
                              (incf bad))))
                        (is (zerop bad) "~D vectors damaged by the failed map" bad))
                      (segment-put s (%id 32) (%vec 64 32.0))
                      (is (= 33 (segment-live-count s)))
                      (is (every #'= (%vec 64 32.0) (segment-get s (%id 32))))))
               (close-vector-segment s))))
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; MMAP-FILE cleanup on a failed file-map (mmap.lisp).  Not segment-specific,
;;; but it lives here because this change is what amplified the leak: a failed
;;; open used to leak the whole reservation window -- *MMAP-MIN-RESERVATION*
;;; (1 GiB) before this feature, now *SEGMENT-MIN-RESERVATION* (16 GiB, 16x
;;; more) for a vector segment -- plus its fd, on every retry.
;;; ---------------------------------------------------------------------------

(test mmap-file-closes-fd-on-failed-file-map
  "MMAP-FILE reserves the anonymous VA window, then maps the file over its
head.  If that second mmap fails, the fd opened for the file must not be left
open -- previously it was, on top of the anonymous window itself never being
released.  Fault-injects the SECOND %POSIX-MMAP call (the file map; the first
is the anonymous reservation) via an FDEFINITION swap -- same technique as
tests/unique-constraint-tests.lisp -- so the failure is deterministic instead
of relying on actually exhausting address space.  Verified directly: capture
the exact fd MMAP-FILE was using when the injected failure hit, then try to
close it again ourselves -- a second close on an fd MMAP-FILE already closed
must fail (EBADF); succeeding would mean MMAP-FILE leaked it."
  (let* ((path (%seg-path))
         (call-count 0)
         (captured-fd nil)
         (orig (fdefinition 'graph-db::%posix-mmap)))
    (unwind-protect
         (progn
           (setf (fdefinition 'graph-db::%posix-mmap)
                 (lambda (addr length prot flags fd offset)
                   (incf call-count)
                   (if (= call-count 2)
                       (progn (setf captured-fd fd)
                              (error "injected file-map failure for the test"))
                       (funcall orig addr length prot flags fd offset))))
           (signals error (graph-db::mmap-file path :create-p t :size 4096))
           (is (integerp captured-fd)
               "fault injection never reached the file-map call -- test setup ~
                is broken, not the code under test")
           (is (minusp (graph-db::%posix-close captured-fd))
               "MMAP-FILE leaked fd ~D on a failed file-map: closing it again ~
                afterward succeeded, meaning MMAP-FILE never closed it"
               captured-fd))
      (setf (fdefinition 'graph-db::%posix-mmap) orig)
      (ignore-errors (delete-file path)))))
