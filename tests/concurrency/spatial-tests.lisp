;;;; CONCURRENT-SPATIAL-SUITE
;;;;
;;;; Thread-safety of the geohash spatial index under the write-path hook:
;;;; concurrent inserts, interleaved inserts + queries, concurrent updates that
;;;; move nodes between cells, and concurrent deletes.  Each geometry node is
;;;; (re)indexed by the transaction apply path (transactions.lisp
;;;; apply-tx-writes-to-spatial-index), so these exercise the index's skip-list
;;;; under the same contention the graph storage layer sees.
;;;;
;;;; The index keys on node id; point geometry yields exactly one cell per node,
;;;; so a bbox query returns one entry per in-window node with no duplicates.

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-spatial-suite
  :description "Spatial index correctness under concurrent load."
  :in concurrency-suite)

(in-suite concurrent-spatial-suite)

;;; ---------------------------------------------------------------------------
;;; Geometry layout
;;;
;;; All test points live over Kharkiv Oblast, in two TIGHT clusters A and B.
;;; Clusters are kept small on purpose: spatial-index-query-bbox enumerates
;;; every geohash cell in the query window, so a wide box is pathologically
;;; expensive -- the reader loop below would never finish.  A ~0.01 deg cluster
;;; (~1 km) bounded by a ~0.02 deg box keeps each query to a few hundred cells.
;;; A and B are ~0.2 deg apart so no cell straddles both and each box sees only
;;; its own cluster.  Per-node steps of 0.00005 deg (~5 m) keep points distinct
;;; while staying inside the box (many share a 150 m cell -- the index allows
;;; duplicate cell keys, and a bbox query dedups by node id).
;;; ---------------------------------------------------------------------------

(defparameter *step* 0.00005d0)
(defparameter *a-lon* 37.100d0)
(defparameter *a-lat* 49.100d0)
(defparameter *b-lon* 37.300d0)
(defparameter *b-lat* 49.300d0)

;; (min-lon min-lat max-lon max-lat) -- small windows bounding each cluster
(defparameter *a-box* '(37.095d0 49.095d0 37.115d0 49.115d0))
(defparameter *b-box* '(37.295d0 49.295d0 37.315d0 49.315d0))

(defun a-point (k) (make-point (+ *a-lon* (* k *step*)) (+ *a-lat* (* k *step*))))
(defun b-point (k) (make-point (+ *b-lon* (* k *step*)) (+ *b-lat* (* k *step*))))

(defun idx-count (g box)
  "Number of DISTINCT node ids the spatial index returns for BOX."
  (length (remove-duplicates
           (loop for idx in (all-spatial-indexes g)
                 append (apply #'spatial-index-query-bbox idx box))
           :test 'equalp)))

(defun live-place-count (g)
  (length (map-vertices #'identity g :collect-p t :vertex-type 'c-place)))

;;; ---------------------------------------------------------------------------
;;; Test 1: concurrent inserts — every committed geometry node is indexed
;;; ---------------------------------------------------------------------------

(test concurrent-spatial-inserts
  "N threads each insert M geometry nodes; all N×M must be in the spatial index."
  (let* ((n *thread-count*) (m 20))
    (with-conc-graph (g)
      (run-threads n
                   (lambda (i)
                     (dotimes (k m)
                       (with-transaction ()
                         (make-c-place :loc (a-point (+ (* i m) k 1)))))))
      (is (= (* n m) (live-place-count g))
          "Expected ~D live places; got ~D" (* n m) (live-place-count g))
      (is (= (* n m) (idx-count g *a-box*))
          "Expected ~D indexed places; got ~D" (* n m) (idx-count g *a-box*)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: interleaved inserts + queries — readers never error, never over-count
;;; ---------------------------------------------------------------------------

(test concurrent-spatial-insert-and-query
  "N writers insert while N readers query the index; readers must not error and
must never see more than the total ever inserted; final count is exact."
  (let* ((n *thread-count*) (m 25) (r 50) (total (* n m)))
    (with-conc-graph (g)
      (run-threads (* 2 n)
                   (lambda (i)
                     (if (< i n)
                         (dotimes (k m)
                           (with-transaction ()
                             (make-c-place :loc (a-point (+ (* i m) k 1)))))
                         (dotimes (_ r)
                           ;; A concurrent query must always return a sane
                           ;; subset -- between 0 and the grand total, never garbage.
                           (let ((c (idx-count g *a-box*)))
                             (unless (<= 0 c total)
                               (error "spatial query returned out-of-range count ~D" c)))))))
      (is (= total (idx-count g *a-box*))
          "Expected ~D indexed after concurrent insert/query; got ~D"
          total (idx-count g *a-box*)))))

;;; ---------------------------------------------------------------------------
;;; Test 3: concurrent updates that move nodes between cells
;;; ---------------------------------------------------------------------------

(test concurrent-spatial-updates-move-cells
  "Pre-seed N nodes in cluster A; N threads each move their own node to cluster
B.  Afterwards A is empty, B holds exactly N, and the total is preserved (old
cell de-indexed, new cell indexed, no node lost or duplicated)."
  (let* ((n *thread-count*)
         (ids (make-array n)))
    (with-conc-graph (g)
      (with-transaction ()
        (dotimes (i n)
          (setf (aref ids i) (id (make-c-place :loc (a-point (1+ i)))))))
      (is (= n (idx-count g *a-box*)) "seed: ~D in A; got ~D" n (idx-count g *a-box*))
      (run-threads n
                   (lambda (i)
                     ;; each thread owns a distinct node -> no write/write conflict
                     (with-transaction ()
                       (let ((v (copy (lookup-vertex (aref ids i)))))
                         (setf (slot-value v 'loc) (b-point (1+ i)))
                         (save v)))))
      (is (= 0 (idx-count g *a-box*)) "after move: A empty; got ~D" (idx-count g *a-box*))
      (is (= n (idx-count g *b-box*)) "after move: ~D in B; got ~D" n (idx-count g *b-box*))
      (is (= n (live-place-count g))  "after move: ~D live; got ~D" n (live-place-count g)))))

;;; ---------------------------------------------------------------------------
;;; Test 4: concurrent deletes — index drains to empty
;;; ---------------------------------------------------------------------------

(test concurrent-spatial-deletes
  "Pre-seed N×M nodes; N threads each delete their own M.  The index ends empty."
  (let* ((n *thread-count*) (m 15)
         (ids (make-array (* n m))))
    (with-conc-graph (g)
      (with-transaction ()
        (dotimes (j (* n m))
          (setf (aref ids j) (id (make-c-place :loc (a-point (1+ j)))))))
      (is (= (* n m) (idx-count g *a-box*))
          "seed: ~D indexed; got ~D" (* n m) (idx-count g *a-box*))
      (run-threads n
                   (lambda (i)
                     (dotimes (k m)
                       (with-transaction ()
                         (mark-deleted (lookup-vertex (aref ids (+ (* i m) k))))))))
      (is (= 0 (idx-count g *a-box*))
          "after deletes: index empty; got ~D" (idx-count g *a-box*))
      (is (= 0 (live-place-count g))
          "after deletes: 0 live; got ~D" (live-place-count g)))))
