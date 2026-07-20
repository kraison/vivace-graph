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
