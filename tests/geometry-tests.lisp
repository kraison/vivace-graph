;;;; Tests for the geometry value type (geometry.lisp).

(in-package #:graph-db/test)

(def-suite geometry-suite
  :description "Geometry construction, bbox, and serialization round trips."
  :in graph-db-suite)

(in-suite geometry-suite)

(defun geo-roundtrip (g)
  "Serialize G and deserialize the result (primary value only)."
  (values (deserialize (serialize g))))

(test point
  "A point round-trips with double-float coordinates."
  (let ((r (geo-roundtrip (make-point 23.71d0 50.026d0))))
    (is (eq :point (geometry-kind r)))
    (is (= 23.71d0 (geometry-lon r)))
    (is (= 50.026d0 (geometry-lat r)))
    (is (typep (geometry-lon r) 'double-float))))

(test integer-coordinates-coerced
  "Integer inputs are coerced to double-floats."
  (let ((r (geo-roundtrip (make-point 1 2))))
    (is (typep (geometry-lon r) 'double-float))
    (is (= 1d0 (geometry-lon r)))
    (is (= 2d0 (geometry-lat r)))))

(test linestring
  (let* ((l (make-linestring '((1 2) (3 4) (5 6))))
         (r (geo-roundtrip l)))
    (is (eq :linestring (geometry-kind r)))
    (is (equalp (geometry-coordinates l) (geometry-coordinates r)))))

(test polygon-with-hole
  (let* ((p (make-polygon '(((0 0) (4 0) (4 4) (0 4) (0 0))
                            ((1 1) (2 1) (2 2) (1 2) (1 1)))))
         (r (geo-roundtrip p)))
    (is (eq :polygon (geometry-kind r)))
    (is (= 2 (length (geometry-coordinates r))) "exterior ring + one hole")
    (is (equalp (geometry-coordinates p) (geometry-coordinates r)))))

(test multipolygon
  (let* ((mp (make-multipolygon '((((0 0) (1 0) (1 1) (0 0)))
                                  (((5 5) (6 5) (6 6) (5 5))))))
         (r (geo-roundtrip mp)))
    (is (eq :multipolygon (geometry-kind r)))
    (is (equalp (geometry-coordinates mp) (geometry-coordinates r)))))

(test bbox-polygon
  (multiple-value-bind (mnx mny mxx mxy)
      (geometry-bbox (make-polygon '(((0 0) (4 0) (4 3) (0 3) (0 0)))))
    (is (= 0d0 mnx)) (is (= 0d0 mny)) (is (= 4d0 mxx)) (is (= 3d0 mxy))))

(test bbox-point
  (multiple-value-bind (mnx mny mxx mxy)
      (geometry-bbox (make-point 23.71d0 50.026d0))
    (is (= 23.71d0 mnx)) (is (= 23.71d0 mxx))
    (is (= 50.026d0 mny)) (is (= 50.026d0 mxy))))

(test real-world-find-points
  "Coordinates taken from the demining EO dataset round-trip exactly."
  (dolist (pt '((37.1724312d0 49.2020584d0)
                (23.7182919d0 50.0263233d0)
                (33.1385833d0 47.2014944d0)))
    (let ((r (geo-roundtrip (make-point (first pt) (second pt)))))
      (is (= (first pt) (geometry-lon r)))
      (is (= (second pt) (geometry-lat r))))))

(test geometry-coordinate-pairs-all-kinds
  "geometry-coordinate-pairs returns pre-6e5e368 (lon lat) double-float nested lists for all four kinds (Issue #84)."
  (let ((pt (make-point 37.1d0 49.2d0))
        (ls (make-linestring '((10 20) (30 40))))
        (poly (make-polygon '(((0 0) (4 0) (4 4) (0 4) (0 0))
                              ((1 1) (2 1) (2 2) (1 2) (1 1)))))
        (mp (make-multipolygon '((((0 0) (1 0) (1 1) (0 0)))
                                 (((5 5) (6 5) (6 6) (5 5)))))))
    (is (equalp '(37.1d0 49.2d0) (geometry-coordinate-pairs pt)))
    (is (equalp '((10.0d0 20.0d0) (30.0d0 40.0d0)) (geometry-coordinate-pairs ls)))
    (is (equalp '(((0.0d0 0.0d0) (4.0d0 0.0d0) (4.0d0 4.0d0) (0.0d0 4.0d0) (0.0d0 0.0d0))
                  ((1.0d0 1.0d0) (2.0d0 1.0d0) (2.0d0 2.0d0) (1.0d0 2.0d0) (1.0d0 1.0d0)))
                (geometry-coordinate-pairs poly)))
    (is (equalp '((((0.0d0 0.0d0) (1.0d0 0.0d0) (1.0d0 1.0d0) (0.0d0 0.0d0)))
                  (((5.0d0 5.0d0) (6.0d0 5.0d0) (6.0d0 6.0d0) (5.0d0 5.0d0))))
                (geometry-coordinate-pairs mp)))))


(test map-geometry-coordinates-traversal
  "map-geometry-coordinates visits all (lon lat) vertices."
  (let ((poly (make-polygon '(((0 0) (4 0) (4 3) (0 3) (0 0)))))
        (collected '()))
    (map-geometry-coordinates (lambda (lon lat) (push (list lon lat) collected)) poly)
    (is (equalp '((0.0d0 0.0d0) (4.0d0 0.0d0) (4.0d0 3.0d0) (0.0d0 3.0d0) (0.0d0 0.0d0))
                (nreverse collected)))))

(test do-geometry-coordinates-macro
  "do-geometry-coordinates inlines traversal with zero memory allocations (Issue #85)."
  (let ((poly (make-polygon '(((0 0) (4 0) (4 3) (0 3) (0 0)))))
        (collected '()))
    (do-geometry-coordinates (lon lat) poly
      (push (list lon lat) collected))
    (is (equalp '((0.0d0 0.0d0) (4.0d0 0.0d0) (4.0d0 3.0d0) (0.0d0 3.0d0) (0.0d0 0.0d0))
                (nreverse collected)))))

(test do-geometry-coordinates-zero-allocation
  "do-geometry-coordinates performs zero memory allocations over 2,261 vertices (Issue #85)."
  (let* ((n 2261)
         (ring (loop for i from 0 below n
                     collect (list (+ 37d0 (* 0.5d0 (cos (* 2 pi (/ i n)))))
                                   (+ 48d0 (* 0.5d0 (sin (* 2 pi (/ i n))))))))
         (poly (make-polygon (list (append ring (list (first ring)))))))
    (let ((c 0))
      (declare (fixnum c))
      (let ((before #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))
        (dotimes (rep 10)
          (do-geometry-coordinates (lon lat) poly
            (declare (ignore lon lat))
            (incf c)))
        (let ((after #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))
          (is (= (* 10 (1+ n)) c))
          (is (= 0 (- after before)) "Expected 0 bytes consed, but consed ~:D bytes" (- after before)))))))







;;; ---- GEOMETRY-EMPTY-P (GH #105) ----------------------------------------

(test empty-p-across-every-kind
  "An empty geometry of each kind reports empty and keeps its KIND.  The four
kinds do not represent emptiness the same way -- a linestring's coordinates
come back as a zero-length vector, the others as NIL -- which is exactly why
callers need a predicate instead of testing GEOMETRY-COORDINATES."
  (dolist (spec (list (cons (graph-db::%make-geometry :kind :point
                                                      :coordinates nil)
                            :point)
                      (cons (make-linestring '()) :linestring)
                      (cons (make-polygon '()) :polygon)
                      (cons (make-multipolygon '()) :multipolygon)))
    (destructuring-bind (g . kind) spec
      (is (geometry-empty-p g) "~A was not reported empty (coords ~S)"
          kind (geometry-coordinates g))
      (is (eq kind (geometry-kind g)) "empty geometry lost its kind"))))

(test empty-p-is-false-for-populated-geometries
  "A geometry with coordinates is never empty -- including a point at the
origin, which is a real location and not an empty point."
  (dolist (g (list (make-point 0d0 0d0)
                   (make-point 37.17d0 49.20d0)
                   (make-linestring '((0d0 0d0) (1d0 1d0)))
                   (make-polygon '(((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 0d0))))
                   (make-multipolygon
                    '((((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 0d0)))))))
    (is (not (geometry-empty-p g))
        "~A reported empty" (geometry-kind g))))

(test empty-p-sees-through-empty-nesting
  "A polygon whose every ring is empty, and a multipolygon whose every polygon
is, hold no coordinate positions and so are empty -- the emptiness can sit one
or two levels down."
  (is (geometry-empty-p (make-polygon (list '()))))
  (is (geometry-empty-p (make-multipolygon (list (list '())))))
  (is (not (geometry-empty-p
            (make-multipolygon
             '((((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 0d0)))))))))

(test empty-p-survives-a-serialization-round-trip
  "An empty geometry stays empty, and stays its own kind, through the on-disk
codec -- so an empty result can be stored in a slot like any other geometry."
  (dolist (g (list (make-linestring '()) (make-polygon '())
                   (make-multipolygon '())))
    (let ((r (geo-roundtrip g)))
      (is (eq (geometry-kind g) (geometry-kind r)))
      (is (geometry-empty-p r) "~A lost its emptiness in the codec: ~S"
          (geometry-kind g) (geometry-coordinates r)))))
