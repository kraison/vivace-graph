;;;; S3 (GEOS-exact): find-nodes-intersecting and exact find-nodes-within on a
;;;; real indexed graph.  The discriminating fixture is POLY-POKEOUT -- a polygon
;;;; whose bbox CENTRE lies inside the AOI but which is NOT contained by it.  GEOS
;;;; exact containment excludes it from find-within; the centroid fallback wrongly
;;;; includes it.  This is exactly the approximation the add-on removes.

(in-package #:graph-db/geos-test)

(def-suite geos-query-suite
  :description "Index-backed find-nodes-intersecting / exact find-nodes-within."
  :in geos-suite)

(in-suite geos-query-suite)

;; AOI square -- kept SMALL (~0.4 km) so the geohash index's bbox query
;; enumerates only a few cells.  The containment/intersection relationships are
;; scale-invariant; what matters is the shapes' relative geometry.
(defparameter *q-aoi*
  (make-polygon '(((0d0 0d0) (0.004d0 0d0) (0.004d0 0.004d0) (0d0 0.004d0) (0d0 0d0)))))

(defun poly (x0 y0 x1 y1)
  (make-polygon (list (list (list x0 y0) (list x1 y0)
                            (list x1 y1) (list x0 y1) (list x0 y0)))))

(defmacro with-query-fixture ((g ids) &body body)
  "Build a graph with four geometry nodes; IDS is bound to a plist
(:point :inside :pokeout :disjoint) of their ids."
  `(with-geos-graph (,g)
     (let ((,ids '()))
       (with-transaction ()
         (setf (getf ,ids :point)
               (id (make-geos-place :geom (make-point 0.002d0 0.002d0)))        ; in AOI
               (getf ,ids :inside)
               (id (make-geos-place :geom (poly 0.001d0 0.001d0 0.003d0 0.003d0))) ; fully inside
               (getf ,ids :pokeout)
               (id (make-geos-place :geom (poly 0.001d0 0.001d0 0.005d0 0.003d0))) ; centre in, not contained
               (getf ,ids :disjoint)
               (id (make-geos-place :geom (poly 0.010d0 0.010d0 0.012d0 0.012d0))))) ; far
       ,@body)))

(defun within-ids (g) (mapcar #'id (find-nodes-within 'geos-place *q-aoi* :graph g)))
(defun intersect-ids (g) (mapcar #'id (find-nodes-intersecting 'geos-place *q-aoi* :graph g)))
(defun has (id ids) (member id ids :test 'equalp))

(test exact-within-excludes-uncontained-polygon
  "With GEOS, find-nodes-within returns only the point and the fully-contained
polygon -- NOT the poke-out polygon (whose centre is inside but body is not)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-query-fixture (g ids)
        (let ((w (within-ids g)))
          (is (has (getf ids :point) w))
          (is (has (getf ids :inside) w))
          (is (not (has (getf ids :pokeout) w)) "poke-out excluded (exact)")
          (is (not (has (getf ids :disjoint) w)))
          (is (= 2 (length w)))))))

(test fallback-within-includes-pokeout-centroid
  "Forcing GEOS off, find-nodes-within judges the poke-out polygon by its centre,
which IS inside the AOI -- so it is (wrongly) included.  This is the exact
approximation the GEOS path removes; the two results must differ."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-query-fixture (g ids)
        (let ((exact (within-ids g))
              (coarse (without-geos (within-ids g))))
          (is (not (has (getf ids :pokeout) exact)))
          (is (has (getf ids :pokeout) coarse) "centroid fallback includes poke-out")
          (is (> (length coarse) (length exact))
              "fallback over-includes relative to exact GEOS containment")))))

(test intersecting-includes-all-overlappers
  "find-nodes-intersecting returns every node that touches the AOI (point, inside,
poke-out) and excludes the disjoint one."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-query-fixture (g ids)
        (let ((i (intersect-ids g)))
          (is (has (getf ids :point) i))
          (is (has (getf ids :inside) i))
          (is (has (getf ids :pokeout) i))
          (is (not (has (getf ids :disjoint) i)))
          (is (= 3 (length i)))))))
