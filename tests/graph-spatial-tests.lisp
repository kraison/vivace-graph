;;;; Tests for the graph-lifecycle integration of the spatial index registry
;;;; (make-graph / open-graph / close-graph wiring in graph.lisp).

(in-package #:graph-db/test)

;; A declaratively spatial vertex type in the shared integration schema.
(def-vertex gs-place ()
  ((geom :type geometry :index t))
  :graph-db-integration-test)

(def-suite graph-spatial-suite
  :description "A graph owns a registry of spatial indexes, repopulated on reopen."
  :in graph-db-suite)

(in-suite graph-spatial-suite)

(test new-graph-has-an-empty-registry
  "MAKE-GRAPH attaches the registry but creates NO index: an index appears only
when a geometry value is actually written (lazy creation, §4.1)."
  (with-test-graph (g)
    (is (hash-table-p (spatial-indexes g)))
    (is (null (all-spatial-indexes g)))
    (is (null (spatial-index-for g 'gs-place 'geom)))
    (with-transaction ()
      (make-gs-place :geom (make-point 37.1724d0 49.2020d0)))
    (let ((idx (spatial-index-for g 'gs-place 'geom)))
      (is (spatial-index-p idx))
      (is (= 1 (length (all-spatial-indexes g))))
      (is (= 1 (length (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))))))

(test spatial-index-survives-reopen
  "Geometry-bearing nodes committed before CLOSE-GRAPH are spatially queryable
after OPEN-GRAPH: the registry is repopulated by REBUILD-SPATIAL-INDEXES."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) kh-id lv-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq kh-id (id (make-gs-place :geom (make-point 37.1724d0 49.2020d0)))
                  lv-id (id (make-gs-place :geom (make-point 23.7183d0 50.0263d0)))))
          (close-graph g :snapshot-p nil)))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((idx (spatial-index-for g2 'gs-place 'geom)))
                 (is (spatial-index-p idx))
                 (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                        37.19d0 49.21d0)))
                   (is (member kh-id cands :test 'equalp) "Kharkiv point restored")
                   (is (not (member lv-id cands :test 'equalp))
                       "Lviv point is outside the query window"))))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))
