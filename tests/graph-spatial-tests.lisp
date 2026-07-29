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

;;; ---- per-index-type backend selection (GH #91) -------------------------
;;;
;;; These use their OWN graph name and types rather than the shared integration
;;; schema: DEF-VERTEX registration is per graph name and assigns type-ids in
;;; evaluation order, and *INTEGRATION-GRAPH-NAME* is shared with the
;;; thread-heavy segment-query tests.  Adding a type there to serve these tests
;;; would perturb an unrelated suite for no reason.

(defparameter *spatial-backend-graph-name* :graph-db-spatial-backend-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *spatial-backend-graph-name* *schema-node-metadata*) nil))

(def-vertex sb-first  () ((geom :type geometry :index t))
  :graph-db-spatial-backend-test)
;; A SECOND spatial owner: its index is created lazily at a LATER moment, which
;; is precisely where the naive workaround (flipping GRAPH-INDEX-BACKEND around
;; the first geometry write) silently produced one backend of each.
(def-vertex sb-second () ((geom :type geometry :index t))
  :graph-db-spatial-backend-test)

(test spatial-index-backend-overrides-the-graph-default
  "A graph can keep B+ trees for views and :UNIQUE while its SPATIAL indexes use
the skip list.  Spatial queries are a handful of SHORT prefix range scans (one
per covering geohash cell, most returning nothing), and the B+ tree's
range-scan advantage is per ENTRY, so it loses badly on that shape -- hence
wanting to differ from the graph default at all.

Also pins the property the naive workaround FAILS: spatial indexes are created
lazily per (OWNER . SLOT), so a SECOND geometry-bearing type written later must
get the same backend as the first.  Flipping GRAPH-INDEX-BACKEND around the
first write silently produced one of each."
  (with-temp-directory (dir)
    (let ((g (make-graph *spatial-backend-graph-name* (namestring dir)
                         :index-backend :bplus-tree
                         :spatial-index-backend :skip-list)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-sb-first :geom (make-point 37.1724d0 49.2020d0)))
             (with-transaction ()
               (make-sb-second :geom (make-point 37.1800d0 49.2100d0)))
             (let ((backends (mapcar #'graph-db::spatial-index-backend
                                     (mapcar (lambda (x) (if (consp x) (cdr x) x))
                                             (all-spatial-indexes g)))))
               (is (= 2 (length backends))
                   "expected two spatial indexes (two owner.slot pairs), got ~a"
                   (length backends))
               (is (every (lambda (b) (eq :skip-list b)) backends)
                   "every spatial index must follow :SPATIAL-INDEX-BACKEND, got ~a"
                   backends))
             (is (eq :bplus-tree (graph-db::graph-index-backend g))
                 "the graph's general backend must be untouched"))
        (ignore-errors (close-graph g :snapshot-p nil))))))

(test spatial-index-backend-nil-follows-the-graph-default
  "NIL (the default) means 'follow :INDEX-BACKEND' -- so existing callers, which
pass only :INDEX-BACKEND, are completely unaffected."
  (with-temp-directory (dir)
    (let ((g (make-graph *spatial-backend-graph-name* (namestring dir)
                         :index-backend :bplus-tree)))
      (unwind-protect
           (let ((*graph* g))
             (is (null (graph-db::graph-spatial-index-backend g))
                 "the new slot must default to NIL")
             (with-transaction ()
               (make-sb-first :geom (make-point 37.1724d0 49.2020d0)))
             (let ((idx (first (all-spatial-indexes g))))
               (is (eq :bplus-tree
                       (graph-db::spatial-index-backend
                        (if (consp idx) (cdr idx) idx)))
                   "with no override the spatial index must follow :INDEX-BACKEND")))
        (ignore-errors (close-graph g :snapshot-p nil))))))
