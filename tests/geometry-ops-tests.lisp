;;;; Tests for geometry refine operations (geometry-ops.lisp).

(in-package #:graph-db/test)

(def-suite geometry-ops-suite
  :description "Geodesic distance, point-in-polygon, and bbox overlap."
  :in graph-db-suite)

(in-suite geometry-ops-suite)

;;; ---- distance ----------------------------------------------------------

(test distance-one-degree-latitude
  "One degree of latitude is ~111.195 km under the spherical model."
  (is (< (abs (- (geodesic-distance 0d0 0d0 1d0 0d0) 111194.927d0)) 1d0)))

(test distance-symmetric-and-zero
  (is (= 0d0 (geodesic-distance 49.2d0 37.1d0 49.2d0 37.1d0)))
  (is (= (geodesic-distance 49.2d0 37.1d0 50.0d0 23.7d0)
         (geodesic-distance 50.0d0 23.7d0 49.2d0 37.1d0))))

(test distance-vs-pyproj-oracle
  "Haversine agrees with the pyproj WGS84 geodesic (397.444 m) to within 0.5%."
  (let ((d (geodesic-distance 49.2020584d0 37.1724312d0 49.2036314d0 37.1773283d0)))
    (is (< (abs (- d 397.444d0)) (* 0.005d0 397.444d0))
        "haversine ~A m vs oracle 397.444 m" d)))

;;; ---- point in polygon --------------------------------------------------

(defparameter *unit-square* '((0 0) (4 0) (4 4) (0 4) (0 0)))

(test point-in-ring-inside-outside
  (is (point-in-ring-p 2d0 2d0 *unit-square*))
  (is (not (point-in-ring-p 5d0 2d0 *unit-square*)))
  (is (not (point-in-ring-p -1d0 2d0 *unit-square*)))
  (is (not (point-in-ring-p 2d0 9d0 *unit-square*))))

(test point-in-ring-degenerate
  (is (not (point-in-ring-p 0d0 0d0 '((0 0) (1 1))))))

;;; ---- packed-coordinate fast path (GH #86) ------------------------------
;;;
;;; POINT-IN-RING-P has two branches: a packed (simple-array double-float (*))
;;; fast path and a list path.  Every test above exercises the list path
;;; directly; MAKE-POLYGON packs, so the geometry-level tests below reach the
;;; fast path only indirectly.  These pin the fast path itself.

(defun %packed-ring (&rest lon-lat)
  "Pack a flat (lon lat lon lat ...) list into the array representation."
  (make-array (length lon-lat) :element-type 'double-float
                               :initial-contents
                               (mapcar (lambda (x) (coerce x 'double-float)) lon-lat)))

(test point-in-ring-packed-matches-list
  "The packed fast path and the list path agree on the same ring."
  (let ((packed (%packed-ring 0 0  4 0  4 4  0 4  0 0))
        (listed '((0 0) (4 0) (4 4) (0 4) (0 0))))
    (dolist (p '((2d0 2d0) (5d0 2d0) (-1d0 2d0) (2d0 9d0) (3.9999d0 2d0)))
      (destructuring-bind (lon lat) p
        (is (eq (and (point-in-ring-p lon lat packed) t)
                (and (point-in-ring-p lon lat listed) t))
            "packed/list disagree at (~A,~A)" lon lat)))))

(test point-in-ring-packed-accepts-any-real-coordinate
  "LON/LAT are contract-level REALs, not necessarily DOUBLE-FLOATs: GEO-WITHIN/3
admits any NUMBERP, and POINT-IN-RING-P is exported.  The packed path must not
demand double-floats -- declaring the parameters DOUBLE-FLOAT outright (rather
than coercing at the boundary) would signal a TYPE-ERROR here."
  (let ((packed (%packed-ring 0 0  4 0  4 4  0 4  0 0)))
    (is (point-in-ring-p 2 2 packed))           ; integers
    (is (point-in-ring-p 2.0 2.0 packed))       ; single-floats
    (is (point-in-ring-p 5/2 5/2 packed))       ; rationals
    (is (not (point-in-ring-p 9 2 packed)))))

(test point-in-ring-degenerate-packed
  (is (not (point-in-ring-p 0d0 0d0 (%packed-ring 0 0  1 1)))))

#+sbcl
(test point-in-ring-packed-conses-nothing
  "The packed representation exists so coordinates stay unboxed; the fast path
must not re-box them one at a time on every read (GH #86: 47 KB/call on a
740-vertex ring before the type declarations were added)."
  (let ((ring (make-array 1480 :element-type 'double-float)))
    (dotimes (i 740)
      (let ((th (* 2d0 pi (/ (float i 1d0) 740d0))))
        (setf (aref ring (* 2 i))      (+ 30d0 (* 5d0 (cos th)))
              (aref ring (1+ (* 2 i))) (+ 50d0 (* 5d0 (sin th))))))
    (funcall (compile nil '(lambda (r) (point-in-ring-p 30d0 50d0 r))) ring) ; warm
    (sb-ext:gc :full t)
    (let* ((call (compile nil '(lambda (r) (point-in-ring-p 30d0 50d0 r))))
           (before (sb-ext:get-bytes-consed)))
      (dotimes (i 200) (funcall call ring))
      (let ((per-call (/ (- (sb-ext:get-bytes-consed) before) 200)))
        (is (< per-call 64)
            "packed point-in-ring-p consed ~A bytes/call on a 740-vertex ring"
            per-call)))))

;;; ---- boundary semantics ------------------------------------------------
;;;
;;; point-in-ring-p uses the PNPOLY even-odd rule with strict `>` vertex
;;; comparisons.  This gives a "half-open" boundary: a point lying exactly on a
;;; shared edge is classified into EXACTLY ONE of two polygons that share that
;;; edge -- never both, never neither.  That tiling property (no double-count,
;;; no gap) is the guarantee callers can rely on; which specific side "wins" is
;;; an implementation detail and is NOT part of the contract.

(test boundary-edge-tiles-without-double-count
  "A point on the edge shared by two adjacent squares belongs to exactly one of
them (XOR) -- so a partition of space neither double-counts nor drops boundary
points."
  (let ((left  '((0 0) (4 0) (4 4) (0 4) (0 0)))     ; x in [0,4]
        (right '((4 0) (8 0) (8 4) (4 4) (4 0))))    ; x in [4,8], shares x=4
    ;; midpoints of the shared vertical edge x=4 (avoid the corner vertices)
    (dolist (lat '(1d0 2d0 3d0))
      (let ((in-left  (point-in-ring-p 4d0 lat left))
            (in-right (point-in-ring-p 4d0 lat right)))
        (is (or (and in-left (not in-right))
                (and in-right (not in-left)))
            "edge point (4,~A) must be in exactly one square (left=~A right=~A)"
            lat in-left in-right)))))

(test boundary-is-deterministic
  "Boundary classification is stable: the same edge point yields the same answer
on repeated calls (no randomness / order dependence)."
  (let ((sq '((0 0) (4 0) (4 4) (0 4) (0 0))))
    (is (eq (point-in-ring-p 4d0 2d0 sq) (point-in-ring-p 4d0 2d0 sq)))
    (is (eq (point-in-ring-p 2d0 0d0 sq) (point-in-ring-p 2d0 0d0 sq)))))

(test interior-and-exterior-unambiguous
  "Points clearly inside/outside are never affected by the boundary rule."
  (let ((sq '((0 0) (4 0) (4 4) (0 4) (0 0))))
    (is (point-in-ring-p 2d0 2d0 sq))              ; centre: in
    (is (not (point-in-ring-p 4.0001d0 2d0 sq)))   ; just outside the right edge
    (is (point-in-ring-p 3.9999d0 2d0 sq))))       ; just inside the right edge

(test polygon-with-hole
  "A point inside the hole is not contained; one in the solid annulus is."
  (let ((rings '(((0 0) (10 0) (10 10) (0 10) (0 0))      ; exterior
                 ((3 3) (7 3) (7 7) (3 7) (3 3)))))        ; hole
    (is (point-in-polygon-rings-p 1d0 1d0 rings))   ; in body, outside hole
    (is (not (point-in-polygon-rings-p 5d0 5d0 rings)))   ; inside the hole
    (is (not (point-in-polygon-rings-p 11d0 5d0 rings))))) ; outside entirely

(test geometry-contains-point-polygon
  (let ((g (make-polygon '(((0 0) (4 0) (4 4) (0 4) (0 0))))))
    (is (geometry-contains-point-p g 2d0 2d0))
    (is (not (geometry-contains-point-p g 5d0 5d0)))))

(test geometry-contains-point-multipolygon
  (let ((g (make-multipolygon '((((0 0) (2 0) (2 2) (0 2) (0 0)))
                                (((10 10) (12 10) (12 12) (10 12) (10 10)))))))
    (is (geometry-contains-point-p g 1d0 1d0))     ; in first polygon
    (is (geometry-contains-point-p g 11d0 11d0))   ; in second polygon
    (is (not (geometry-contains-point-p g 5d0 5d0)))))

(test geometry-contains-realish-aoi
  "A find inside a small task-area square is contained; a nearby one is not."
  (let ((aoi (make-polygon '(((37.170 49.200) (37.180 49.200)
                              (37.180 49.206) (37.170 49.206)
                              (37.170 49.200))))))
    (is (geometry-contains-point-p aoi 37.1724312d0 49.2020584d0))
    (is (not (geometry-contains-point-p aoi 37.1900d0 49.2100d0)))))

;;; ---- bbox overlap ------------------------------------------------------

(test bbox-overlap
  (is (bbox-overlap-p 0 0 4 4   2 2 6 6))    ; overlapping
  (is (bbox-overlap-p 0 0 4 4   4 4 8 8))    ; touching at a corner
  (is (not (bbox-overlap-p 0 0 4 4   5 5 8 8)))   ; disjoint
  (is (not (bbox-overlap-p 0 0 1 1   2 0 3 1)))) ; disjoint in lon only

(test geometry-distance-points
  (is (< (abs (- (geometry-distance (make-point 0d0 0d0) (make-point 0d0 1d0))
                 111194.927d0))
         1d0)))
