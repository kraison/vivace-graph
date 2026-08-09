(in-package :graph-db)

;;; Geometry refine operations for the spatial index.
;;;
;;; Public, general-purpose, dependency-free predicates used to refine the
;;; geohash index's candidate set into exact answers: great-circle distance
;;; (haversine), point-in-polygon (ray casting, holes honoured), and
;;; bounding-box overlap.  Heavier topology (intersection, union, validity
;;; repair) is delegated to GEOS via cl-geos in a later, optional module.

(alexandria:define-constant +earth-radius-m+ 6371000d0 :test '=)
(alexandria:define-constant +radians-per-degree+
    #.(coerce (/ pi 180) 'double-float) :test '=)

(declaim (inline deg->rad))
(defun deg->rad (d)
  (* (coerce d 'double-float) +radians-per-degree+))

(defun geodesic-distance (lat1 lon1 lat2 lon2)
  "Great-circle distance in metres between two WGS84 points (haversine).
Accurate to ~0.5% vs the ellipsoid -- ample at minefield scale; a Vincenty or
Karney method can replace this later if sub-metre accuracy over long lines is
needed."
  (let* ((phi1 (deg->rad lat1))
         (phi2 (deg->rad lat2))
         (dphi (deg->rad (- lat2 lat1)))
         (dlam (deg->rad (- lon2 lon1)))
         (a (+ (expt (sin (/ dphi 2)) 2)
               (* (cos phi1) (cos phi2) (expt (sin (/ dlam 2)) 2)))))
    (* 2d0 +earth-radius-m+ (atan (sqrt a) (sqrt (- 1d0 a))))))

(defun %point-in-packed-ring-p (lon lat ring)
  "PNPOLY over a packed (lon lat lon lat ...) double-float ring.
The declarations are load-bearing, not decoration: the packed representation
exists so coordinates stay unboxed, and without them SBCL re-boxes every AREF
and every arithmetic intermediate -- ~24 KB of garbage per call on a
country-scale 740-vertex ring, and ~20x slower (GH #86).  With them it conses
nothing.  Callers coerce LON/LAT; see POINT-IN-RING-P."
  (declare (type double-float lon lat)
           (type (simple-array double-float (*)) ring))
  (let* ((len2 (length ring))
         (n (ash len2 -1))
         (inside nil))
    (declare (type fixnum len2 n))
    (when (< n 3) (return-from %point-in-packed-ring-p nil))
    (loop for i of-type fixnum from 0 below n
          ;; the previous vertex, wrapping: (mod (+ i n -1) n) without the divide
          for j of-type fixnum = (if (= i 0) (1- n) (1- i))
          do (let* ((idx-i (* 2 i))
                    (idx-j (* 2 j))
                    (xi (aref ring idx-i))
                    (yi (aref ring (1+ idx-i)))
                    (xj (aref ring idx-j))
                    (yj (aref ring (1+ idx-j))))
               (declare (type fixnum idx-i idx-j)
                        (type double-float xi yi xj yj))
               (when (and (not (eq (> yi lat) (> yj lat)))
                          (< lon (+ xi (/ (* (- xj xi) (- lat yi)) (- yj yi)))))
                 (setf inside (not inside)))))
    inside))

(defun point-in-ring-p (lon lat ring)
  "True if (LON, LAT) lies inside RING -- a list of (lon lat) or a packed double-float array -- by the
even-odd ray-casting rule (Franklin's PNPOLY).
LON and LAT are any REALs; GEO-WITHIN/3 admits any NUMBERP, so the packed
kernel's DOUBLE-FLOAT contract is met by coercing here rather than by declaring
the parameters."
  (if (typep ring '(simple-array double-float (*)))
      (%point-in-packed-ring-p (float lon 1d0) (float lat 1d0) ring)
      (let ((n (length ring)))
        (when (< n 3) (return-from point-in-ring-p nil))
        (let ((v (coerce ring 'vector)) (inside nil))
          (loop for i from 0 below n
                for j = (mod (+ i n -1) n)
                do (let* ((vi (aref v i)) (vj (aref v j))
                          (xi (coerce (first vi) 'double-float))
                          (yi (coerce (second vi) 'double-float))
                          (xj (coerce (first vj) 'double-float))
                          (yj (coerce (second vj) 'double-float)))
                     (when (and (not (eq (> yi lat) (> yj lat)))
                                (< lon (+ xi (/ (* (- xj xi) (- lat yi)) (- yj yi)))))
                       (setf inside (not inside)))))
          inside))))


(defun point-in-polygon-rings-p (lon lat rings)
  "RINGS is an exterior ring followed by zero or more hole rings.  True when the
point is inside the exterior ring and outside every hole."
  (and rings
       (point-in-ring-p lon lat (first rings))
       (notany (lambda (hole) (point-in-ring-p lon lat hole)) (rest rings))))

(defun geometry-contains-point-p (g lon lat)
  "True if the :POLYGON or :MULTIPOLYGON geometry G contains (LON, LAT)."
  (ecase (geometry-kind g)
    (:polygon (point-in-polygon-rings-p lon lat (geometry-coordinates g)))
    (:multipolygon (some (lambda (poly) (point-in-polygon-rings-p lon lat poly))
                         (geometry-coordinates g)))))

(defun bbox-overlap-p (a-min-lon a-min-lat a-max-lon a-max-lat
                       b-min-lon b-min-lat b-max-lon b-max-lat)
  "True if the two axis-aligned bounding boxes overlap (touching counts)."
  (and (<= a-min-lon b-max-lon) (>= a-max-lon b-min-lon)
       (<= a-min-lat b-max-lat) (>= a-max-lat b-min-lat)))

(defun geometry-distance (g1 g2)
  "Great-circle distance in metres between two :POINT geometries."
  (geodesic-distance (geometry-lat g1) (geometry-lon g1)
                     (geometry-lat g2) (geometry-lon g2)))

;;; -------------------------------------------------------------------------
;;; Topology refine SEAM
;;;
;;; These generic functions are the indirection through which the spatial query
;;; layer asks for exact topology.  Their DEFAULT methods here are
;;; dependency-free fallbacks (exact where a hand-rolled algorithm exists, coarse
;;; or signalling otherwise).  The OPTIONAL graph-db/geos add-on defines :AROUND
;;; methods that route through GEOS when *geos-available-p* is true and fall back
;;; via CALL-NEXT-METHOD otherwise.  Core graph-db carries no GEOS dependency.
;;; -------------------------------------------------------------------------

(defun geos-available-p ()
  "True when the graph-db/geos add-on has loaded libgeos_c, so the seam below
uses exact GEOS topology rather than the dependency-free fallbacks."
  *geos-available-p*)

(defgeneric geometry-intersects-p (a b)
  (:documentation
   "True if geometries A and B intersect.  Default (no GEOS): exact for the
point/area and point/point cases, otherwise a COARSE bounding-box overlap test
(an over-approximation).  With graph-db/geos it is exact for all kinds.")
  (:method ((a geometry) (b geometry))
    (let ((ka (geometry-kind a)) (kb (geometry-kind b)))
      (cond
        ((and (eq ka :point) (member kb '(:polygon :multipolygon)))
         (geometry-contains-point-p b (geometry-lon a) (geometry-lat a)))
        ((and (eq kb :point) (member ka '(:polygon :multipolygon)))
         (geometry-contains-point-p a (geometry-lon b) (geometry-lat b)))
        ((and (eq ka :point) (eq kb :point))
         (and (= (geometry-lon a) (geometry-lon b))
              (= (geometry-lat a) (geometry-lat b))))
        (t
         (multiple-value-bind (anx any axx axy) (geometry-bbox a)
           (multiple-value-bind (bnx bny bxx bxy) (geometry-bbox b)
             (bbox-overlap-p anx any axx axy bnx bny bxx bxy))))))))

(defgeneric geometry-contains-geometry-p (a b)
  (:documentation
   "True if geometry A contains geometry B.  Default (no GEOS): exact when B is a
:POINT (and A is a polygon/multipolygon, or A=B as points); otherwise signals
GEOS-REQUIRED-FOR-OPERATION (no dependency-free algorithm for extended-in-extended
containment).  With graph-db/geos it is exact for all kinds.")
  (:method ((a geometry) (b geometry))
    (if (eq (geometry-kind b) :point)
        (case (geometry-kind a)
          ((:polygon :multipolygon)
           (geometry-contains-point-p a (geometry-lon b) (geometry-lat b)))
          (:point (and (= (geometry-lon a) (geometry-lon b))
                       (= (geometry-lat a) (geometry-lat b))))
          (t (error 'geos-required-for-operation
                    :operation 'geometry-contains-geometry-p)))
        (error 'geos-required-for-operation
               :operation 'geometry-contains-geometry-p))))

(defgeneric geometry-make-valid (g)
  (:documentation
   "Return a valid geometry equivalent to G (repairing self-intersections etc.).
Requires graph-db/geos; the default signals GEOS-REQUIRED-FOR-OPERATION.")
  (:method ((g geometry))
    (error 'geos-required-for-operation :operation 'geometry-make-valid)))

(defgeneric geometry-distance-exact (a b)
  (:documentation
   "Exact PLANAR distance between geometries A and B in coordinate units (for
lon/lat, degrees -- NOT metres; use GEODESIC-DISTANCE/GEOMETRY-DISTANCE for
metric distance between points).  Requires graph-db/geos; the default signals
GEOS-REQUIRED-FOR-OPERATION.")
  (:method ((a geometry) (b geometry))
    (error 'geos-required-for-operation :operation 'geometry-distance-exact)))

(defgeneric geometry-geodesic-distance (a b)
  (:documentation
   "Minimum GEODESIC distance in METRES between geometries A and B.  For two
:POINT geometries this is the haversine distance (no GEOS needed).  For extended
geometries it is the haversine distance between their closest pair of points,
which requires graph-db/geos (GEOSNearestPoints); the default signals
GEOS-REQUIRED-FOR-OPERATION for that case.")
  (:method ((a geometry) (b geometry))
    (if (and (eq (geometry-kind a) :point) (eq (geometry-kind b) :point))
        (geometry-distance a b)
        (error 'geos-required-for-operation :operation 'geometry-geodesic-distance))))

;;; Constructive (overlay) operations -- all require graph-db/geos; defaults
;;; signal.  Each returns a VG geometry (the GEOS result converted back).

(defgeneric geometry-union (a b)
  (:documentation "Union of geometries A and B.  Requires graph-db/geos.")
  (:method ((a geometry) (b geometry))
    (error 'geos-required-for-operation :operation 'geometry-union)))

(defgeneric geometry-intersection (a b)
  (:documentation "Intersection of geometries A and B.  Requires graph-db/geos.")
  (:method ((a geometry) (b geometry))
    (error 'geos-required-for-operation :operation 'geometry-intersection)))

(defgeneric geometry-difference (a b)
  (:documentation "A minus B (the part of A not in B).  Requires graph-db/geos.")
  (:method ((a geometry) (b geometry))
    (error 'geos-required-for-operation :operation 'geometry-difference)))

(defgeneric geometry-buffer (g width &optional quadrant-segments)
  (:documentation
   "Buffer geometry G by WIDTH (in COORDINATE UNITS -- degrees for lon/lat, NOT
metres), approximating round corners with QUADRANT-SEGMENTS segments per quarter
circle (default 8).  Requires graph-db/geos.")
  (:method ((g geometry) width &optional (quadrant-segments 8))
    (declare (ignore width quadrant-segments))
    (error 'geos-required-for-operation :operation 'geometry-buffer)))

(defgeneric geometry-area (g)
  (:documentation
   "Area of geometry G in SQUARED COORDINATE UNITS (squared degrees for lon/lat,
NOT m^2).  Requires graph-db/geos.")
  (:method ((g geometry))
    (error 'geos-required-for-operation :operation 'geometry-area)))
