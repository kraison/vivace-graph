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
  "k greater than the number of stored vectors returns them all, no padding.
Asserts hand-computed cosine values (not the %cosine-under-test oracle): with
query (1,0,0,0), the id-1 vector (1,0,0,0) is an exact direction match
(score 1.0) and the id-2 vector (0,1,0,0) is orthogonal (score 0.0) -- values
a bare dot product would ALSO get right here, but the explicit id order and
tight tolerance still pin the ranking and the (score . id) shape."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 0.0 1.0 0.0 0.0))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 2 (length got)) "expected 2 hits, got ~S" got)
                    (when (= 2 (length got))
                      (destructuring-bind ((s1 . i1) (s2 . i2)) got
                        (is (equalp (%qid 1) i1) "expected id 1 ranked first")
                        (is (< (abs (- s1 1.0)) 1e-6)
                            "expected top score 1.0, got ~A" s1)
                        (is (equalp (%qid 2) i2) "expected id 2 ranked second")
                        (is (< (abs s2) 1e-6)
                            "expected second score ~~0.0, got ~A" s2)))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-cosine-not-dot-product
  "Pins full cosine, not a bare dot product: a NON-unit stored vector (2,0,0,0)
against query (1,0,0,0) must score 1.0 (cosine of parallel vectors) -- a bare
dot product would score 2.0.  This is the assertion a bare-dot mutation of
%cosine cannot pass."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 2.0 0.0 0.0 0.0))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 1)))
                    (is (= 1 (length got)) "expected 1 hit, got ~S" got)
                    (when (= 1 (length got))
                      (is (< (abs (- (car (first got)) 1.0)) 1e-6)
                          "expected cosine 1.0 for a non-unit vector, got ~A -- looks like a bare dot product"
                          (car (first got))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-skips-removed-slots
  "A removed id does not appear in scan results.  Before removal, id 1 and
id 2 store IDENTICAL vectors, so their scores tie exactly -- scanning with
k=1 first pins the score-DESC/id-ASC tiebreak (id 1 must win), then removal
is exercised and only id 2 remains."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 8)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (segment-put s (%qid 2) (%qvec 4 1.0 0.0 0.0 0.0))
                  (let ((tie (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 1)))
                    (is (= 1 (length tie)) "expected 1 hit, got ~S" tie)
                    (when (= 1 (length tie))
                      (is (equalp (%qid 1) (cdr (first tie)))
                          "expected id 1 to win the score tie (id-ascending tiebreak), got ~A"
                          (cdr (first tie)))))
                  (segment-remove s (%qid 1))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 10)))
                    (is (= 1 (length got)) "expected 1 hit, got ~S" got)
                    (when (= 1 (length got))
                      (is (equalp (%qid 2) (cdr (first got)))))))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-rejects-wrong-length-query
  "A query vector whose length doesn't match the segment's dimension is
rejected loudly, mirroring segment-put's write-side check -- not silently
scored against a prefix (or, worse, silently scored as all-zero when the
query happens to be zero only within the segment's dimension and nonzero
beyond it, which used to slip past the zero-norm check entirely)."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (segment-put s (%qid 1) (%qvec 4 1.0 0.0 0.0 0.0))
                  (signals error (segment-scan s (%qvec 3 1.0 0.0 0.0) 1))
                  (signals error
                      (segment-scan s (%qvec 8 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0) 1)))
             (close-vector-segment s)))
      (ignore-errors (delete-file path)))))

(test scan-across-growth
  "segment-scan sweeps the CURRENT capacity after multiple growths and returns
the correct full ranking.  Growth is the riskiest interaction for the scan: it
relocates the whole vector block (the sweep must read the NEW capacity) and
free-marks [old-cap, new-cap) (the sweep must skip a large sparse upper
range).  Vector n (1-indexed) is (1, n-1, 0, 0) against query (1,0,0,0): its
cosine is 1/sqrt(1+(n-1)^2), which is STRICTLY DECREASING as n increases (the
denominator strictly grows) -- so there are no ties, and the expected
best-first order is simply ids 1..12, verifiable by inspection without calling
%cosine.  :initial-capacity 4 with 12 puts forces %seg-grow twice (4 -> 8 ->
16)."
  (let ((path (%qpath)))
    (unwind-protect
         (let ((s (create-vector-segment path 4 :initial-capacity 4)))
           (unwind-protect
                (progn
                  (loop for n from 1 to 12
                        do (segment-put s (%qid n)
                                        (%qvec 4 1.0 (coerce (1- n) 'single-float) 0.0 0.0)))
                  (is (= 16 (segment-capacity s))
                      "expected capacity to have grown to 16 after 12 puts from an initial 4, got ~D"
                      (segment-capacity s))
                  (let ((got (segment-scan s (%qvec 4 1.0 0.0 0.0 0.0) 12)))
                    (is (= 12 (length got)) "expected all 12 hits, got ~D" (length got))
                    (when (= 12 (length got))
                      (loop for n from 1 to 12
                            for pair in got
                            do (is (equalp (%qid n) (cdr pair))
                                   "expected id ~D at rank ~D, got a different id" n n)))))
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
