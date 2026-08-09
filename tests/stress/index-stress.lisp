;;;; INDEX-STRESS-SUITE
;;;;
;;;; Scale tests for the general ordered index (:INDEX): a large keyed set,
;;;; equality + range queries, and index consistency after mass deletion.

(in-package #:graph-db/stress-test)

(def-suite index-stress-suite
  :description "Scale tests for the general ordered index (:index / def-index)."
  :in stress-suite)

(in-suite index-stress-suite)

(test index-large-dataset-consistency
  "N :index-ed items: a full range scan returns all N in key order; equality resolves
each; after deleting half, the index reflects only the live nodes."
  (let ((n (scale 5000 500)))
    (with-stress-graph (g)
      (let ((ids (make-array n)))
        (let ((start (get-internal-real-time)))
          (with-transaction ()
            (dotimes (i n)
              (setf (aref ids i) (id (make-si-item :ikey i :label "x")))))
          (record-throughput "indexed-insert" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        ;; full range scan returns all N
        (is (= n (length (index-range g 'si-item 'ikey)))
            "range scan should return all ~D items" n)
        ;; a bounded range [0,99] returns the first 100 keys in ascending order
        (is (equal (loop for i below 100 collect i)
                   (mapcar (lambda (nd) (slot-value nd 'ikey))
                           (index-range g 'si-item 'ikey :start 0 :end 99)))
            "range [0,99] should be keys 0..99 in order")
        ;; equality lookup: every key resolves to exactly one node (throughput too)
        (let ((start (get-internal-real-time)) (misses 0))
          (dotimes (i n)
            (unless (= 1 (length (index-lookup g 'si-item 'ikey i))) (incf misses)))
          (record-throughput "index-lookup-eq" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second)))
          (is (zerop misses) "~D of ~D keys did not resolve to exactly one node" misses n))
        ;; delete every other node (even keys); index must drop them
        (with-transaction ()
          (dotimes (i (floor n 2))
            (mark-deleted (lookup-vertex (aref ids (* 2 i))))))
        (is (= (- n (floor n 2)) (length (index-range g 'si-item 'ikey)))
            "range count after deleting half should be ~D" (- n (floor n 2)))
        (is (null (index-lookup g 'si-item 'ikey 0)) "a deleted key must not resolve")
        (is (= 1 (length (index-lookup g 'si-item 'ikey 1))) "a surviving key must resolve")))))
