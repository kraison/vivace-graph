;;;; Tests for the spatial Prolog predicates (prolog-functors.lisp):
;;;; geo-distance/5, geo-near/5, geo-within/3.  A query needs a live *graph*,
;;;; so each is wrapped in with-test-graph.

(in-package #:graph-db/test)

(def-suite spatial-prolog-suite
  :description "Spatial Prolog predicates: geo-distance, geo-near, geo-within."
  :in graph-db-suite)

(in-suite spatial-prolog-suite)

(test geo-distance-binds-metres
  "geo-distance/5 binds its last arg to the geodesic distance."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((d (first (select-flat (?d) (geo-distance 0d0 0d0 1d0 0d0 ?d)))))
      (is (numberp d))
      (is (< (abs (- d 111194.927d0)) 1d0)))))

(test geo-near-gates-on-radius
  "geo-near/5 succeeds within the radius and fails outside it (~400 m apart)."
  (with-test-graph (g)
    (declare (ignore g))
    (is (select-flat (?x)
          (geo-near 45.6720584d0 12.3424312d0 45.6736314d0 12.3473283d0 600d0)
          (= ?x t)))
    (is (null (select-flat (?x)
                (geo-near 45.6720584d0 12.3424312d0 45.6736314d0 12.3473283d0
                  100d0)
                (= ?x t))))))

(test geo-within-point-in-polygon
  "geo-within/3 succeeds for a point inside the region polygon, fails outside."
  (with-test-graph (g)
    (declare (ignore g))
    (is (select-flat (?in)
          (is ?area (make-polygon '(((12.340d0 45.670d0) (12.350d0 45.670d0)
                                     (12.350d0 45.676d0) (12.340d0 45.676d0)
                                     (12.340d0 45.670d0)))))
          (geo-within 12.3424312d0 45.6720584d0 ?area)
          (= ?in t)))
    (is (null (select-flat (?in)
                (is ?area (make-polygon '(((12.340d0 45.670d0) (12.350d0
                                                                 45.670d0)
                                           (12.350d0 45.676d0) (12.340d0
                                                                 45.676d0)
                                           (12.340d0 45.670d0)))))
                (geo-within 12.3600d0 45.6800d0 ?area)
                (= ?in t))))))
