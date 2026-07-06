;;;; CONCURRENT-INDEX-STORM-SUITE
;;;;
;;;; Concurrent general-ordered-index stress: writers churn indexed nodes while
;;;; readers scan the index concurrently (must not error); the index is fully
;;;; consistent after all threads join.

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-index-storm-suite
  :description "Concurrent general ordered index build/query storm."
  :in concurrent-stress-suite)

(in-suite concurrent-index-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Half the threads insert indexed items (distinct keys); the other half run
;;; range scans concurrently.  Readers must not error (they may see partial
;;; results).  After join: every inserted key resolves to exactly one node, and a
;;; full range scan returns exactly the inserted count.
;;; ---------------------------------------------------------------------------

(test index-storm-build-and-query
  "Writers insert indexed items while readers scan the index; final index consistent."
  (let* ((t-count (min *stress-thread-count* 8))
         (writers (max 1 (floor t-count 2)))
         (m       100))
    (with-cstress-graph (g)
      (let ((start (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (if (< i writers)
                           (dotimes (j m)
                             (with-transaction ()
                               (make-ci-item :ikey (+ (* i m) j) :label "is")))
                           ;; readers: concurrent bounded range scans over the whole
                           ;; key space -- correctness is "does not error".
                           (dotimes (_ (* m 2))
                             (index-range g 'ci-item 'ikey :start 0 :end 100000000)))))
        (record-throughput "index-storm-inserts" (* writers m)
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (let* ((total (* writers m)) (found 0))
        (dotimes (key total)
          (when (= 1 (length (index-lookup g 'ci-item 'ikey key))) (incf found)))
        (is (= total found) "index-lookup resolved ~D of ~D keys" found total))
      (is (= (* writers m) (length (index-range g 'ci-item 'ikey)))
          "final range-scan count mismatch: expected ~D got ~D"
          (* writers m) (length (index-range g 'ci-item 'ikey))))))

;;; ---------------------------------------------------------------------------
;;; Concurrent update churn: T threads repeatedly re-key the SAME nodes; every
;;; update must release the old value and claim the new one, so the index never
;;; accumulates stale entries -- afterwards the index size equals the node count.
;;; ---------------------------------------------------------------------------

(test index-storm-update-churn
  "T threads churn the indexed slot of a shared node set; index stays 1:1 with nodes."
  (let* ((t-count (min *stress-thread-count* 8))
         (nodes   200)
         (rounds  20))
    (with-cstress-graph (g)
      (let ((ids (make-array nodes)))
        (with-transaction ()
          (dotimes (i nodes)
            (setf (aref ids i) (id (make-ci-item :ikey i :label "churn")))))
        (run-threads t-count
                     (lambda (tid)
                       (dotimes (r rounds)
                         (let* ((idx (mod (+ tid r) nodes))
                                (v (ignore-errors (copy (lookup-vertex (aref ids idx))))))
                           (when v
                             (ignore-errors
                               (with-transaction ()
                                 (setf (slot-value v 'ikey) (+ 1000 (random 100000)))
                                 (save v))))))))
        ;; Every live node appears exactly once in the index (no stale duplicates).
        (let ((live (length (map-vertices #'identity g :collect-p t :vertex-type 'ci-item)))
              (indexed (length (index-range g 'ci-item 'ikey))))
          (is (= live indexed)
              "index size ~D should equal live node count ~D (no stale entries)"
              indexed live))))))
