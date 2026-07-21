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
