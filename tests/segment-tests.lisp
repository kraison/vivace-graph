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
