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
