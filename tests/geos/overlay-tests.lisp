;;;; N1 + N2: constructive overlay ops (union/intersection/difference/buffer),
;;;; area, and geodesic distance between geometries.  Areas are checked against
;;;; exact computed ground truth (independent of any oracle); the WKT bridge is
;;;; already cross-checked against shapely in oracle-tests.

(in-package #:graph-db/geos-test)

(def-suite geos-overlay-suite
  :description "union / intersection / difference / buffer / area / geodesic distance."
  :in geos-suite)

(in-suite geos-overlay-suite)

(defun osq (x0 y0 x1 y1)
  (make-polygon (list (list (list x0 y0) (list x1 y0)
                            (list x1 y1) (list x0 y1) (list x0 y0)))))

(defun approx2 (a b &optional (eps 1d-6)) (<= (abs (- a b)) eps))

;; A = [0,4]x[0,4] (area 16); B = [2,6]x[2,6] (area 16); overlap [2,4]x[2,4] (area 4).
(defparameter *oa* (osq 0d0 0d0 4d0 4d0))
(defparameter *ob* (osq 2d0 2d0 6d0 6d0))

(test area-of-square
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx2 16d0 (geometry-area *oa*)))))

(test union-area
  "Union area = areaA + areaB - overlap = 16 + 16 - 4 = 28."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((u (geometry-union *oa* *ob*)))
        (is (geometryp u))
        (is (approx2 28d0 (geometry-area u))))))

(test intersection-area
  "Intersection is the [2,4]x[2,4] overlap, area 4."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx2 4d0 (geometry-area (geometry-intersection *oa* *ob*))))))

(test difference-area
  "A minus B removes the overlap: area 16 - 4 = 12."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx2 12d0 (geometry-area (geometry-difference *oa* *ob*))))))

(test buffer-of-point-approximates-disc
  "Buffering a point by radius 1 gives a ~unit disc (area ~= pi) and contains
the point."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((b (geometry-buffer (make-point 0d0 0d0) 1d0 64)))
        (is (member (geometry-kind b) '(:polygon :multipolygon)))
        (is (< (abs (- pi (geometry-area b))) 0.01d0)
            "buffer area ~A vs pi ~A" (geometry-area b) pi)
        (is (geometry-contains-point-p b 0d0 0d0)))))

(test buffer-grows-a-polygon
  "Buffering a square outward increases its area."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((s (osq 0d0 0d0 2d0 2d0)))
        (is (> (geometry-area (geometry-buffer s 0.5d0 16))
               (geometry-area s))))))

;;; ---- geodesic distance --------------------------------------------------

(test geodesic-distance-points-equals-haversine
  "For two points, geodesic distance equals the haversine (geometry-distance)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((a (make-point 37.10d0 49.10d0))
            (b (make-point 37.20d0 49.15d0)))
        (is (approx2 (geometry-distance a b)
                     (geometry-geodesic-distance a b)
                     0.5d0)))))                ; within half a metre

(test geodesic-distance-polygons-real-metres
  "Two unit squares 2 degrees of longitude apart near the equator are ~222 km
apart by nearest-points geodesic distance (NOT planar degrees)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let* ((a (osq 0d0 0d0 1d0 1d0))
             (b (osq 3d0 0d0 4d0 1d0))
             (d (geometry-geodesic-distance a b)))
        ;; 2 deg of longitude near the equator ~ 222 km; allow a wide band so the
        ;; result is robust to which closest-point latitude GEOS picks.
        (is (< 220000d0 d 224000d0) "geodesic gap ~A m (expected ~222 km)" d))))

(test geodesic-distance-overlapping-is-zero
  "Overlapping geometries have zero distance."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx2 0d0 (geometry-geodesic-distance *oa* *ob*)))))

;;; ---- fallback (GEOS off) ------------------------------------------------

(test overlay-ops-signal-without-geos
  "Overlay ops + area have no dependency-free fallback and signal when GEOS off."
  (without-geos
    (signals geos-required-for-operation (geometry-union *oa* *ob*))
    (signals geos-required-for-operation (geometry-intersection *oa* *ob*))
    (signals geos-required-for-operation (geometry-difference *oa* *ob*))
    (signals geos-required-for-operation (geometry-buffer *oa* 1d0))
    (signals geos-required-for-operation (geometry-area *oa*))
    ;; geodesic distance: point-point still works (haversine), extended signals
    (is (numberp (geometry-geodesic-distance (make-point 0d0 0d0) (make-point 1d0 1d0))))
    (signals geos-required-for-operation (geometry-geodesic-distance *oa* *ob*))))

(test disjoint-intersection-is-empty-not-an-error
  "GH #105: two disjoint polygons intersect in nothing, and GEOS reports that as
\"POLYGON EMPTY\".  That is the NORMAL result, not a failure -- it must come
back as an empty geometry of area 0, not signal.  A caller that cannot tell
\"no overlap\" from \"could not compute\" cannot answer the question it asked."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let* ((far (osq 100d0 100d0 104d0 104d0))
             (i (handler-case (geometry-intersection *oa* far)
                  (error (e) e))))
        (is (geometryp i) "disjoint intersection signalled: ~A" i)
        (when (geometryp i)
          (is (approx2 0d0 (geometry-area i)))))))

(test difference-that-erodes-everything-is-empty
  "The same path through any overlay op: A minus a B that covers it is empty."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let* ((cover (osq -1d0 -1d0 9d0 9d0))
             (d (handler-case (geometry-difference *oa* cover)
                  (error (e) e))))
        (is (geometryp d) "fully-covered difference signalled: ~A" d)
        (when (geometryp d)
          (is (approx2 0d0 (geometry-area d)))))))
