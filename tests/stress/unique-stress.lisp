;;;; UNIQUE-STRESS-SUITE
;;;;
;;;; Scale tests for :UNIQUE constraints: a large distinct-value set, duplicate
;;;; rejection at scale, and value reuse after deletion.

(in-package #:graph-db/stress-test)

(def-suite unique-stress-suite
  :description "Scale tests for :unique constraints."
  :in stress-suite)

(in-suite unique-stress-suite)

(test unique-large-dataset-integrity
  "Insert N distinct :unique values; a duplicate of any is rejected; deleting a
holder frees its value for reuse."
  (let ((n (scale 5000 500)))
    (with-stress-graph (g)
      (let ((ids (make-array n)))
        (let ((start (get-internal-real-time)))
          (with-transaction ()
            (dotimes (i n)
              (setf (aref ids i) (id (make-su-item :ukey i :label "u")))))
          (record-throughput "unique-insert" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        (is (= n (length (map-vertices #'identity g :collect-p t :vertex-type 'su-item)))
            "expected ~D live su-items after insert" n)
        ;; a duplicate of an existing value is rejected
        (signals unique-constraint-violation
          (with-transaction () (make-su-item :ukey 0 :label "dup")))
        ;; delete the first half, then their values must be reusable
        (with-transaction ()
          (dotimes (i (floor n 2))
            (mark-deleted (lookup-vertex (aref ids i)))))
        (finishes
          (with-transaction ()
            (dotimes (i (floor n 2))
              (make-su-item :ukey i :label "reuse"))))
        ;; and a value that is now live again is once more rejected as a duplicate
        (signals unique-constraint-violation
          (with-transaction () (make-su-item :ukey 0 :label "dup2")))))))
