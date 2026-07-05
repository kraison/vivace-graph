;;;; Fibonacci-heap tests -- focused on the two VivaceGraph adaptations: EQUALP
;;;; node-table (vector/UUID keys) and REAL keys (float distances).

(in-package #:graph-db/algorithms-test)

(def-suite fib-heap-suite
  :description "Fibonacci heap: ordering, decrease-key, equalp keys, real keys."
  :in graph-db-algorithms-suite)

(in-suite fib-heap-suite)

(defun drain (heap)
  "Extract every element, returning the list of data in ascending-key order."
  (loop until (fib-heap:empty-p heap)
        collect (fib-heap:extract-min heap)))

(test extract-min-ascending
  "extract-min yields data in nondecreasing key order regardless of insert order."
  (let ((h (make-instance 'fib-heap:fib-heap)))
    (dolist (pair '((5 . :e) (1 . :a) (3 . :c) (2 . :b) (4 . :d)))
      (fib-heap:insert h (car pair) (cdr pair)))
    (is (equal '(:a :b :c :d :e) (drain h)))))

(test decrease-key-reorders
  "decrease-key moves an element ahead of others."
  (let ((h (make-instance 'fib-heap:fib-heap)))
    (fib-heap:insert h 10 :x)
    (fib-heap:insert h 20 :y)
    (fib-heap:insert h 30 :z)
    (fib-heap:decrease-key h :z 5)        ; z now the smallest
    (is (eq :z (fib-heap:extract-min h)))
    (is (eq :x (fib-heap:extract-min h)))
    (is (eq :y (fib-heap:extract-min h)))))

(test equalp-vector-keys
  "DATA may be a byte vector (as VivaceGraph node ids are): node-table lookup and
decrease-key work by EQUALP, not EQL."
  (let ((h (make-instance 'fib-heap:fib-heap))
        (k1 (make-array 2 :initial-contents '(1 2)))
        (k2 (make-array 2 :initial-contents '(3 4))))
    (fib-heap:insert h 8 k1)
    (fib-heap:insert h 4 k2)
    ;; lookup-node finds an entry via an EQUALP-equal but non-EQL vector
    (is (eql 8 (fib-heap:lookup-node h (make-array 2 :initial-contents '(1 2)))))
    (fib-heap:decrease-key h (make-array 2 :initial-contents '(1 2)) 1)
    (is (equalp k1 (fib-heap:extract-min h)))
    (is (equalp k2 (fib-heap:extract-min h)))))

(test real-float-keys
  "Keys may be floats (weighted distances), not just fixnums."
  (let ((h (make-instance 'fib-heap:fib-heap)))
    (fib-heap:insert h 2.5 :b)
    (fib-heap:insert h 1.25 :a)
    (fib-heap:insert h 3.0 :c)
    (is (eq :a (fib-heap:extract-min h)))
    (multiple-value-bind (data key) (fib-heap:peek-min h)
      (is (eq :b data))
      (is (= 2.5 key)))))

(test lookup-node-absent
  "lookup-node returns NIL for data never inserted (or already extracted)."
  (let ((h (make-instance 'fib-heap:fib-heap)))
    (fib-heap:insert h 1 :a)
    (is (null (fib-heap:lookup-node h :missing)))
    (fib-heap:extract-min h)
    (is (null (fib-heap:lookup-node h :a)))))
