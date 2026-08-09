;;;; find-nodes-intersecting + exact-capable find-within, exercised in CORE
;;;; (no graph-db/geos loaded).  These assertions hold in the dependency-free
;;;; fallback mode: point candidates are always exact; extended-geometry
;;;; candidates use the coarse bbox path (which we assert only when GEOS is
;;;; absent, so the file is correct in any image).  Reuses GEO-PLACE +
;;;; node-geometry from spatial-hook-tests.lisp.

(in-package #:graph-db/test)

(def-suite spatial-intersect-suite
  :description "find-nodes-intersecting + find-within refine (fallback mode)."
  :in graph-db-suite)

(in-suite spatial-intersect-suite)

;; Query area over Kharkiv Oblast.
(defparameter *aoi*
  (make-polygon '(((37.170d0 49.200d0) (37.180d0 49.200d0)
                   (37.180d0 49.206d0) (37.170d0 49.206d0)
                   (37.170d0 49.200d0)))))

(test intersecting-and-within-exact-for-points
  "A point inside the AOI is returned by both find-nodes-intersecting and
find-nodes-within; a far point (Lviv) by neither.  Exact regardless of GEOS."
  (with-test-graph (g)
    (let (inside outside)
      (with-transaction ()
        (setq inside  (id (make-geo-place :loc (make-point 37.1724d0 49.2021d0)))
              outside (id (make-geo-place :loc (make-point 23.7183d0 50.0263d0)))))
      (let ((hit-i (mapcar #'id (find-nodes-intersecting 'geo-place *aoi* :graph g)))
            (hit-w (mapcar #'id (find-nodes-within 'geo-place *aoi* :graph g))))
        (is (member inside hit-i :test 'equalp))
        (is (not (member outside hit-i :test 'equalp)))
        (is (member inside hit-w :test 'equalp))
        (is (not (member outside hit-w :test 'equalp)))))))

(test find-intersects-functor-yields-nodes
  "The Prolog find-intersects/3 functor yields nodes intersecting a bound area,
composing with is-a."
  (with-test-graph (g)
    (declare (ignore g))
    (let (inside)
      (with-transaction ()
        (setq inside (id (make-geo-place :loc (make-point 37.1724d0 49.2021d0))))
        (make-geo-place :loc (make-point 23.7183d0 50.0263d0)))   ; far, excluded
      (let ((ids (mapcar #'id
                         (select-flat (?n)
                           (is-a ?n geo-place)
                           (is ?area (make-polygon '(((37.170d0 49.200d0) (37.180d0 49.200d0)
                                                      (37.180d0 49.206d0) (37.170d0 49.206d0)
                                                      (37.170d0 49.200d0)))))
                           (find-intersects ?n geo-place ?area)))))
        (is (member inside ids :test 'equalp))
        (is (= 1 (length ids)))))))

(test intersecting-polygon-node-coarse-without-geos
  "Without GEOS, a polygon node whose bbox overlaps the AOI is returned by
find-nodes-intersecting (the documented coarse fallback).  Skipped if GEOS is
loaded in this image (then the test would assert exactness instead)."
  (if (geos-available-p)
      (skip "GEOS loaded; coarse-fallback behaviour does not apply")
      (with-test-graph (g)
        (let (poly)
          (with-transaction ()
            ;; a polygon overlapping the AOI's bounding box
            (setq poly (id (make-geo-place
                            :loc (make-polygon
                                  '(((37.178d0 49.204d0) (37.200d0 49.204d0)
                                     (37.200d0 49.212d0) (37.178d0 49.212d0)
                                     (37.178d0 49.204d0))))))))
          (is (member poly (mapcar #'id (find-nodes-intersecting 'geo-place *aoi* :graph g))
                      :test 'equalp)
              "coarse bbox overlap should include the polygon node")))))
