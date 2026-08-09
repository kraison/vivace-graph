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

;;; ---- GH #99: point-in-polygon vs GEOS characterization -----------------
;;;
;;; GEOMETRY-CONTAINS-POINT-P is a DEFUN, not a DEFGENERIC, so unlike every
;;; other spatial predicate it cannot dispatch to GEOS -- it always resolves
;;; to the ray-cast above.  These tests characterize whether that matters.
;;; GEOMETRY-CONTAINS-GEOMETRY-P / GEOMETRY-INTERSECTS-P serve as the GEOS
;;; oracle: both are DEFGENERIC with a GEOS :AROUND (geos/geos-ops.lisp), so
;;; calling them with a :POINT argument routes through real
;;; GEOSContains_r / GEOSIntersects_r whenever GEOS is loaded and available,
;;; without this file depending on graph-db/geos directly.
;;;
;;; Conclusion: away from a boundary the two always agree (checked directly
;;; here and by a 8000-point randomized fuzz during development, 0
;;; disagreements).  ON a boundary they use different, each internally
;;; consistent conventions: GEOS excludes it unconditionally (DE-9IM
;;; "contains"); the ray-cast's half-open rule includes about half of it
;;; (already pinned above by BOUNDARY-EDGE-TILES-WITHOUT-DOUBLE-COUNT).  The
;;; ray-cast never claims containment where GEOS says the point doesn't even
;;; touch the polygon, and GEOS-contains never claims strict containment
;;; where the ray-cast disagrees -- i.e. the divergence is a documented
;;; boundary convention, not a correctness defect.  Decision: keep the
;;; native path (77x faster, no optional dependency, on the query hot path);
;;; document + pin rather than converge on GEOS.

(defun %geos-contains-point (g lon lat)
  "GEOS contains-oracle for a point, via the existing GEOMETRY-CONTAINS-GEOMETRY-P
:AROUND seam. Only meaningful when (GEOS-AVAILABLE-P); callers must gate."
  (and (geometry-contains-geometry-p g (make-point lon lat)) t))

(defun %geos-touches-point (g lon lat)
  "GEOS intersects-oracle for a point, via the existing GEOMETRY-INTERSECTS-P
:AROUND seam. Only meaningful when (GEOS-AVAILABLE-P); callers must gate."
  (and (geometry-intersects-p g (make-point lon lat)) t))

(test gh-99-edge-axis-aligned-vs-geos
  "A point on an axis-aligned edge: the ray-cast's half-open rule includes
the bottom edge and excludes the right edge (pinned above by
BOUNDARY-EDGE-TILES-WITHOUT-DOUBLE-COUNT); GEOS-contains excludes both
(DE-9IM boundary exclusion) while GEOS-intersects includes both (the point
genuinely touches). Refs GH #99."
  (let ((sq (make-polygon '(((0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0) (0d0 0d0))))))
    (is (geometry-contains-point-p sq 2d0 0d0))        ; bottom edge: native includes
    (is (not (geometry-contains-point-p sq 4d0 2d0)))  ; right edge: native excludes
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point sq 2d0 0d0))
          (is (not (%geos-contains-point sq 2d0 0d0)))
          (is (%geos-touches-point sq 4d0 2d0))
          (is (not (%geos-contains-point sq 4d0 2d0)))))))

(test gh-99-edge-diagonal-vs-geos
  "Same convention gap holds on a non-axis-aligned edge, where float rounding
differs from the axis-aligned case. Refs GH #99."
  (let ((tri (make-polygon '(((0d0 0d0) (4d0 0d0) (2d0 4d0) (0d0 0d0))))))
    (is (geometry-contains-point-p tri 1d0 2d0))       ; on (2,4)-(0,0): native includes
    (is (not (geometry-contains-point-p tri 3d0 2d0))) ; on (4,0)-(2,4): native excludes
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point tri 1d0 2d0))
          (is (not (%geos-contains-point tri 1d0 2d0)))
          (is (%geos-touches-point tri 3d0 2d0))
          (is (not (%geos-contains-point tri 3d0 2d0)))))))

(test gh-99-vertex-vs-geos
  "A point exactly on a vertex: same convention gap as an edge. Refs GH #99."
  (let ((sq (make-polygon '(((0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0) (0d0 0d0))))))
    (is (geometry-contains-point-p sq 0d0 0d0))
    (is (not (geometry-contains-point-p sq 4d0 4d0)))
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point sq 0d0 0d0))
          (is (not (%geos-contains-point sq 0d0 0d0)))
          (is (%geos-touches-point sq 4d0 4d0))
          (is (not (%geos-contains-point sq 4d0 4d0)))))))

(test gh-99-hole-boundary-vs-geos
  "A point on a hole's boundary, or on the exterior ring's boundary: the
convention gap applies to hole rings too -- this is the case that matters
most for hazard-area containment (a find sitting on a cleared/uncleared
boundary). Strictly inside the hole, both implementations agree (no
boundary involved). Refs GH #99."
  (let ((g (make-polygon '(((0d0 0d0) (10d0 0d0) (10d0 10d0) (0d0 10d0) (0d0 0d0))
                          ((3d0 3d0) (7d0 3d0) (7d0 7d0) (3d0 7d0) (3d0 3d0))))))
    (is (not (geometry-contains-point-p g 5d0 3d0)))  ; hole's bottom edge: native excludes (in-hole)
    (is (geometry-contains-point-p g 0d0 5d0))         ; exterior ring's left edge: native includes
    (is (not (geometry-contains-point-p g 5d0 5d0)))   ; strictly inside the hole
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point g 5d0 3d0))
          (is (not (%geos-contains-point g 5d0 3d0)))
          (is (%geos-touches-point g 0d0 5d0))
          (is (not (%geos-contains-point g 0d0 5d0)))
          (is (not (%geos-touches-point g 5d0 5d0)))
          (is (not (%geos-contains-point g 5d0 5d0)))))))

(test gh-99-self-touching-ring-vs-geos
  "A bowtie ring pinched at a single shared vertex (invalid per strict OGC
simple-polygon rules, but real data can produce it): the ray-cast returns a
deterministic answer at and around the pinch point rather than erroring,
and GEOS agrees on which lobe is interior. Refs GH #99."
  (let ((bowtie (make-polygon
                 '(((0d0 0d0) (4d0 0d0) (2d0 2d0) (4d0 4d0) (0d0 4d0) (2d0 2d0) (0d0 0d0))))))
    (is (geometry-contains-point-p bowtie 2d0 0.5d0))       ; lower lobe interior
    (is (geometry-contains-point-p bowtie 2d0 3.5d0))       ; upper lobe interior
    (is (not (geometry-contains-point-p bowtie 3.5d0 2d0))) ; between the lobes: outside
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point bowtie 2d0 0.5d0))
          (is (%geos-touches-point bowtie 2d0 3.5d0))
          (is (not (%geos-touches-point bowtie 3.5d0 2d0)))))))

(test gh-99-degenerate-rings-vs-geos
  "Zero-area rings (collapsed to a point, or collinear) never contain
anything under the ray-cast -- no positive area means PNPOLY's even-odd
count can never flip -- and GEOS agrees. A repeated-vertex ring behaves
exactly like the same ring without the duplicate. Refs GH #99."
  (let ((point-ring (make-polygon '(((5d0 5d0) (5d0 5d0) (5d0 5d0) (5d0 5d0)))))
        (collinear-ring (make-polygon '(((0d0 0d0) (2d0 0d0) (4d0 0d0) (0d0 0d0)))))
        (repeated-vertex-sq (make-polygon '(((0d0 0d0) (0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0) (0d0 0d0))))))
    (is (not (geometry-contains-point-p point-ring 5d0 5d0)))
    (is (not (geometry-contains-point-p collinear-ring 2d0 0d0)))
    (is (geometry-contains-point-p repeated-vertex-sq 2d0 2d0))  ; interior unaffected by the dup
    (is (geometry-contains-point-p repeated-vertex-sq 0d0 0d0))  ; native includes this vertex
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (not (%geos-contains-point point-ring 5d0 5d0)))
          (is (not (%geos-contains-point collinear-ring 2d0 0d0)))
          (is (%geos-contains-point repeated-vertex-sq 2d0 2d0))
          (is (not (%geos-contains-point repeated-vertex-sq 0d0 0d0)))
          (is (%geos-touches-point repeated-vertex-sq 0d0 0d0))))))

(test gh-99-winding-order-vs-geos
  "The ray-cast is winding-invariant (PNPOLY does not care about orientation);
GEOS normalizes orientation internally. Both agree regardless of winding.
Refs GH #99."
  (let ((ccw (make-polygon '(((0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0) (0d0 0d0)))))
        (cw  (make-polygon '(((0d0 0d0) (0d0 4d0) (4d0 4d0) (4d0 0d0) (0d0 0d0))))))
    (is (eq (geometry-contains-point-p ccw 2d0 2d0) (geometry-contains-point-p cw 2d0 2d0)))
    (is (eq (geometry-contains-point-p ccw 4d0 2d0) (geometry-contains-point-p cw 4d0 2d0)))
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (eq (%geos-contains-point ccw 2d0 2d0) (%geos-contains-point cw 2d0 2d0)))
          (is (eq (%geos-touches-point ccw 4d0 2d0) (%geos-touches-point cw 4d0 2d0)))))))

(test gh-99-multipolygon-shared-edge-vs-geos
  "A point on the edge shared by two MULTIPOLYGON members: GEOMETRY-CONTAINS-POINT-P's
:MULTIPOLYGON case is (SOME ...) over members, so the point lands in
whichever member's own half-open rule includes it -- native is deterministic
here regardless. Two members sharing a FULL edge is itself invalid per OGC
(MultiPolygon parts may only touch at isolated points, confirmed via
GEOMETRY-VALID-P below) -- and empirically, GEOS's own GEOSContains_r
verdict on this invalid input is GEOS-version-dependent (observed NIL on
3.14.1, T on 3.12.1), so only GEOS-intersects-p (stable: the point
unambiguously touches the geometry) is pinned here, not GEOS-contains.
Real adjacent hazard-area parcels should be modeled as separate objects (or
unioned into one valid polygon), not as touching MultiPolygon members --
this case is GH #99's own flagged scenario, and the invalidity is itself
part of the characterization."
  (let ((mp (make-multipolygon
             (list (list '((0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0) (0d0 0d0)))
                   (list '((4d0 0d0) (8d0 0d0) (8d0 4d0) (4d0 4d0) (4d0 0d0)))))))
    (is (geometry-contains-point-p mp 4d0 2d0))   ; shared edge, midpoint
    (is (geometry-contains-point-p mp 4d0 0d0))   ; shared edge, shared vertex
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (not (geometry-valid-p mp))
              "two MultiPolygon members sharing a full edge should be OGC-invalid")
          (is (%geos-touches-point mp 4d0 2d0))
          (is (%geos-touches-point mp 4d0 0d0))))))

(test gh-99-precision-limits-vs-geos
  "Very small (~1e-9 degree) and very large-magnitude (1e8 offset) polygons:
the ray-cast and GEOS agree on interior/exterior classification at both
scales, and boundary points show the same convention gap as at ordinary
scale -- it is not a magnitude-dependent effect. Refs GH #99."
  (let* ((tiny (make-polygon
                (list (list (list 37.123456d0 49.654321d0)
                            (list (+ 37.123456d0 1d-9) 49.654321d0)
                            (list (+ 37.123456d0 1d-9) (+ 49.654321d0 1d-9))
                            (list 37.123456d0 (+ 49.654321d0 1d-9))
                            (list 37.123456d0 49.654321d0)))))
         (huge (make-polygon
                (list (list (list 1d8 1d8) (list (+ 1d8 4d0) 1d8)
                            (list (+ 1d8 4d0) (+ 1d8 4d0)) (list 1d8 (+ 1d8 4d0))
                            (list 1d8 1d8))))))
    (is (geometry-contains-point-p tiny (+ 37.123456d0 0.5d-9) (+ 49.654321d0 0.5d-9)))
    (is (not (geometry-contains-point-p tiny (- 37.123456d0 1d-9) 49.654321d0)))
    (is (geometry-contains-point-p huge (+ 1d8 2d0) (+ 1d8 2d0)))
    (is (geometry-contains-point-p huge (+ 1d8 2d0) 1d8))  ; on bottom edge: native includes
    (is (geometry-contains-point-p huge 1d8 1d8))          ; on bottom-left vertex: native includes
    (if (not (geos-available-p))
        (skip "GEOS not available")
        (progn
          (is (%geos-touches-point tiny (+ 37.123456d0 0.5d-9) (+ 49.654321d0 0.5d-9)))
          (is (%geos-contains-point tiny (+ 37.123456d0 0.5d-9) (+ 49.654321d0 0.5d-9)))
          (is (not (%geos-touches-point tiny (- 37.123456d0 1d-9) 49.654321d0)))
          (is (%geos-contains-point huge (+ 1d8 2d0) (+ 1d8 2d0)))
          (is (%geos-touches-point huge (+ 1d8 2d0) 1d8))
          (is (not (%geos-contains-point huge (+ 1d8 2d0) 1d8)))
          (is (%geos-touches-point huge 1d8 1d8))
          (is (not (%geos-contains-point huge 1d8 1d8)))))))

(defun %det-star-ring (n cx cy rbase ramp phase)
  "A deterministic star-shaped ring around (CX,CY): N vertices at evenly
spaced angles (max gap 2*pi/n, always < pi for N>=3, so the ring is
guaranteed simple), radius RBASE + RAMP*sin(3*angle+PHASE) with RAMP < RBASE
so the boundary is non-convex and non-axis-aligned everywhere without ever
crossing the center. No RNG -- same rings on every run."
  (let ((verts (loop for k from 0 below n
                     for a = (+ phase (/ (* 2 pi k) n))
                     for r = (+ rbase (* ramp (sin (* 3 a))))
                     collect (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))))))
    (append verts (list (first verts)))))

(test gh-99-generative-boundary-invariant-vs-geos
  "Across a family of deterministic, non-convex, non-axis-aligned star
polygons, sampled at every vertex: the ray-cast never claims containment
where GEOS says the point does not even touch the polygon (no false
containment), and GEOS-contains never claims strict containment where the
ray-cast disagrees (no missed strict interior). The only disagreement
anywhere is the documented boundary convention -- never a clearly-interior
point reported as exterior, or vice versa. Refs GH #99."
  (if (not (geos-available-p))
      (skip "GEOS not available")
      (dolist (params '((7 0d0 0d0 5d0 2d0 0d0)
                        (11 50d0 -30d0 8d0 3d0 0.7d0)
                        (5 -20d0 20d0 3d0 1.2d0 1.9d0)
                        (16 100d0 100d0 12d0 4d0 0.3d0)))
        (destructuring-bind (n cx cy rbase ramp phase) params
          (let* ((ring (%det-star-ring n cx cy rbase ramp phase))
                 (geom (make-polygon (list ring))))
            (dolist (v (butlast ring))
              (destructuring-bind (lon lat) v
                (let ((native (and (geometry-contains-point-p geom lon lat) t))
                      (gc (%geos-contains-point geom lon lat))
                      (gi (%geos-touches-point geom lon lat)))
                  (is (not (and native (not gi)))
                      "native claims containment at vertex (~A,~A) but GEOS says it does not even touch"
                      lon lat)
                  (is (not (and gc (not native)))
                      "GEOS claims strict containment at vertex (~A,~A) but native disagrees"
                      lon lat))))
            ;; the ring's own center is guaranteed interior (star-shaped kernel point)
            (is (geometry-contains-point-p geom cx cy))
            (is (%geos-contains-point geom cx cy))
            ;; well outside every ring's extent
            (let ((far-lon (+ cx rbase ramp 1000d0)) (far-lat cy))
              (is (not (geometry-contains-point-p geom far-lon far-lat)))
              (is (not (%geos-touches-point geom far-lon far-lat)))))))))

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
