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
      (let ((a (make-point 12.10d0 45.10d0))
            (b (make-point 12.20d0 45.15d0)))
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

;;; ---- an OVERLAY answering with a GEOMETRYCOLLECTION (GH #164) ----------
;;;
;;; Distinct from #163, which was GEOSMakeValid answering that way for an
;;; INVALID ring.  Here both inputs are valid, nothing is repaired, and the
;;; collection comes out of the intersection itself.

(defun notched-square ()
  "A valid 10x10 square with a notch cut from its right side between y=4
and y=6 -- a C opening rightward."
  (make-polygon '(((0d0 0d0) (10d0 0d0) (10d0 4d0) (4d0 4d0)
                   (4d0 6d0) (10d0 6d0) (10d0 10d0) (0d0 10d0)
                   (0d0 0d0)))))

(defun notch-filling-block ()
  "A valid rectangle sharing an AREA with NOTCHED-SQUARE's lower arm and,
separately, meeting its notch edge along a LINE.  GEOS answers the
intersection with
GEOMETRYCOLLECTION (POLYGON ((8 0, 8 4, 10 4, 10 0, 8 0)),
                   LINESTRING (8 6, 10 6))."
  (osq 8d0 0d0 12d0 6d0))

(test collection-intersection-inputs-are-both-valid
  "Sanity, and what separates this from #163: neither input is invalid, so
no repair runs and %REPAIRED is not on the path at all."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (is-true (geometry-valid-p (notched-square)))
        (is-true (geometry-valid-p (notch-filling-block))))))

(test intersection-keeps-the-polygons-of-a-collection-result
  "⚠ AN OVERLAP-PLUS-TOUCH MEASURES ITS OVERLAP.  The polygonal part is
the [8,10]x[0,4] area the two genuinely share; the linear part is the
notch edge they merely meet along, and it carries no area.  Signalling
here refused the whole subject and cost 1,560 claims over ten consecutive
days of a deployed series (GH #164)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((i (handler-case (geometry-intersection (notched-square)
                                                    (notch-filling-block))
                 (error (e) e))))
        (is (geometryp i) "collection intersection signalled: ~A" i)
        (when (geometryp i)
          (is (member (geometry-kind i) '(:polygon :multipolygon)))
          (is (approx2 8d0 (geometry-area i)))))))

(test collection-intersection-with-no-area-is-empty-not-an-error
  "The same reduction with nothing polygonal in it.  A block abutting the
notched square's right side meets its lower arm along an EDGE and its
upper arm at the single VERTEX (10,6), so GEOS answers
GEOMETRYCOLLECTION (LINESTRING (10 0, 10 4), POINT (10 6)).  That is the
measurement \"they share no area\", which must read as area 0 -- the #105
contract -- not as a refusal."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let* ((l (notched-square))
             (r (osq 10d0 0d0 14d0 6d0))
             (i (handler-case (geometry-intersection l r)
                  (error (e) e))))
        (is (geometryp i) "no-area collection intersection signalled: ~A" i)
        (when (geometryp i)
          (is (approx2 0d0 (geometry-area i)))))))
