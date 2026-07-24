;;;; End-to-end test of the write-path hook: transactions auto-maintain the
;;;; spatial index for geometry-bearing nodes (transactions.lisp
;;;; apply-tx-writes-to-spatial-index), via the NODE-GEOMETRY protocol.

(in-package #:graph-db/test)

;; A geometry-bearing vertex type in the shared integration schema, plus the
;; NODE-GEOMETRY method that opts it into spatial indexing.  (LOC holds a
;; geometry value, which serializes via the +geometry+ codec.)
(def-vertex geo-place ()
  ((loc))
  :graph-db-integration-test)

(defmethod node-geometry ((p geo-place))
  (slot-value p 'loc))

;; Declarative variant: a geometry slot marked :index t makes the type spatial
;; with NO hand-written node-geometry method (the default finds the slot).
(def-vertex geo-auto ()
  ((loc :type geometry :index t))
  :graph-db-integration-test)

(def-suite spatial-hook-suite
  :description "Transactions auto-maintain the spatial index (create/update/delete)."
  :in graph-db-suite)

(in-suite spatial-hook-suite)

(defparameter *kharkiv-box* '(37.16d0 49.19d0 37.19d0 49.21d0))
(defparameter *lviv-box*    '(23.70d0 50.00d0 23.75d0 50.05d0))

(defun in-box-p (g id box)
  (member id (loop for idx in (all-spatial-indexes g)
                   append (apply #'spatial-index-query-bbox idx box))
          :test 'equalp))

(test create-indexes-node
  "Committing a geometry-bearing node indexes it automatically."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (is (not (in-box-p g id *lviv-box*))))))

(test update-reindexes-node
  "Updating the geometry moves the node in the index (old cell out, new cell in)."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'loc) (make-point 23.7183d0 50.0263d0))
          (save v)))
      (is (not (in-box-p g id *kharkiv-box*)) "old location de-indexed")
      (is (in-box-p g id *lviv-box*) "new location indexed"))))

(test delete-removes-from-index
  "Deleting a node removes it from the spatial index."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (with-transaction ()
        (mark-deleted (lookup-vertex id)))
      (is (not (in-box-p g id *kharkiv-box*))))))

(test nodes-without-geometry-are-ignored
  "A node whose NODE-GEOMETRY is NIL (the default) is not indexed, and commits
fine."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-g-person :name "No geometry" :age 1))))
      (is (lookup-vertex id))
      ;; querying a wide box returns no g-person (only geo-place nodes index)
      (is (not (in-box-p g id *kharkiv-box*))))))

(test index-survives-reopen-with-real-nodes
  "Nodes indexed via the write-path hook are still queryable after reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (in-box-p g2 id *kharkiv-box*)))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))

(test index-slot-flag-auto-wires-node-geometry
  "A geometry slot declared :index t makes the type spatially indexed with no
hand-written node-geometry method -- node-geometry's default finds the slot."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-auto :loc (make-point 37.1724d0 49.2020d0)))))
      ;; auto-indexed on create
      (is (in-box-p g id *kharkiv-box*))
      (is (not (in-box-p g id *lviv-box*)))
      ;; and reindexed on update through the same default
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'loc) (make-point 23.7183d0 50.0263d0))
          (save v)))
      (is (not (in-box-p g id *kharkiv-box*)))
      (is (in-box-p g id *lviv-box*)))))
