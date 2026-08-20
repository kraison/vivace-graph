;;;; S4: validity repair (GEOSMakeValid) and exact PLANAR distance.

(in-package #:graph-db/geos-test)

(def-suite geos-makevalid-suite
  :description "geometry-make-valid + geometry-distance-exact."
  :in geos-suite)

(in-suite geos-makevalid-suite)

;; A self-intersecting "bowtie" (figure-8): edges (0,0)-(4,4) and (4,0)-(0,4)
;; cross at (2,2), so the ring is invalid.
(defun bowtie ()
  (make-polygon '(((0d0 0d0) (4d0 4d0) (4d0 0d0) (0d0 4d0) (0d0 0d0)))))

(defun valid-square ()
  (make-polygon '(((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0) (0d0 0d0)))))

(test bowtie-is-invalid-square-is-valid
  "Sanity: the bowtie is invalid, the plain square is valid."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (is-false (geometry-valid-p (bowtie)))
        (is-true  (geometry-valid-p (valid-square))))))

(test make-valid-repairs-bowtie
  "make-valid turns the invalid bowtie into a valid geometry."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (let ((fixed (geometry-make-valid (bowtie))))
             (is (geometryp fixed))
             (is-true (geometry-valid-p fixed) "repaired geometry is valid")
             ;; the bowtie repairs to a polygonal result (poly or multipolygon)
             (is (member (geometry-kind fixed) '(:polygon :multipolygon)))))))

(test make-valid-keeps-valid-valid
  "make-valid on an already-valid polygon yields a still-valid geometry."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (is-true (geometry-valid-p (geometry-make-valid (valid-square)))))))

;;; ---- GEOSMakeValid answering with a GEOMETRYCOLLECTION (GH #163) -------

(defun tailed-square ()
  "A 10x10 square whose ring carries on past its start out to (20,0) and
back -- a zero-width tail.  The repair is MIXED-DIMENSION (an area plus
a line), which GEOSMakeValid answers with a GEOMETRYCOLLECTION.  That is
the shape that refused seven deployed subjects outright (GH #163)."
  (make-polygon '(((0d0 0d0) (10d0 0d0) (10d0 10d0) (0d0 10d0) (0d0 0d0)
                   (20d0 0d0) (0d0 0d0)))))

(defun collapsed-ring ()
  "A ring with NO area at all: out to (10,0) and straight back.  Its
repair is purely linear, so there is nothing polygonal to keep."
  (make-polygon '(((0d0 0d0) (5d0 0d0) (10d0 0d0) (5d0 0d0) (0d0 0d0)))))

(test make-valid-keeps-the-polygons-of-a-collection-repair
  "⚠ THE REPAIR OF A MIXED-DIMENSION RING IS ITS POLYGONS' UNION, not an
error.  GEOSMakeValid returns POLYGON + LINESTRING inside a
GEOMETRYCOLLECTION; the polygons are the repaired area and the lines are
the slivers it shed.  Signalling here instead sent SPACETIME::%REPAIRED
back to the UNREPAIRED ring, whose intersection throws, which
REGISTER-GEOMETRY turns into a refusal of the WHOLE subject (GH #163)."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t
         (is-false (geometry-valid-p (tailed-square))
                   "fixture sanity: the tailed square really is invalid")
         (let ((fixed (geometry-make-valid (tailed-square))))
           (is (member (geometry-kind fixed) '(:polygon :multipolygon))
               "a polygonal repair, not ~S" (geometry-kind fixed))
           (is-true (geometry-valid-p fixed))
           ;; Squared DEGREES (geometry-area, not the geodesic one): the
           ;; square is 100 of them and the tail contributes none.
           (is (< (abs (- 100d0 (geometry-area fixed))) 1d-9)
               "the square's area survives whole, got ~S"
               (geometry-area fixed))
           (is-true (geometry-intersects-p fixed (valid-square))
                    "and the repair is usable by the overlay ops")))))

(test make-valid-refuses-a-repair-with-no-area-left
  "⚠ A RING THAT REPAIRS TO NOTHING BUT LINES MUST NOT BECOME AN EMPTY
POLYGON.  'Covers nothing' is a measurement, and fabricating it is the
fault #163 is about, inverted; signalling leaves %REPAIRED's IGNORE-
ERRORS to hand back the original, and the scan to report itself
unevaluated."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (is-false (geometry-valid-p (collapsed-ring))
                     "fixture sanity: the collapsed ring is invalid")
           (signals geos-error (geometry-make-valid (collapsed-ring))))))

(test a-polygon-free-collection-is-refused-not-emptied
  "The same rule one level down, on a GEOMETRYCOLLECTION built by hand:
%GEOS-REPAIRED->GEOMETRY has no polygonal part to keep, so it signals.
Built here because no ring in reach makes GEOSMakeValid emit one -- a
wholly degenerate ring comes back as a MULTILINESTRING instead."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-geos-context (ctx)
        (let ((handle (graph-db::geos-ctx-handle ctx)))
          (cffi:with-foreign-object (arr :pointer 2)
            ;; GEOSGeom_createCollection_r TAKES OWNERSHIP of the members,
            ;; so destroying the collection frees all three.
            (setf (cffi:mem-aref arr :pointer 0)
                  (geometry->geos ctx (make-point 0d0 0d0))
                  (cffi:mem-aref arr :pointer 1)
                  (geometry->geos ctx (make-linestring
                                       '((0d0 0d0) (1d0 1d0)))))
            (let ((coll (graph-db::%geos-create-collection
                         handle graph-db::+geos-geometrycollection+ arr 2)))
              (unwind-protect
                   (signals geos-error
                     (graph-db::%geos-repaired->geometry ctx coll))
                (graph-db::%geos-geom-destroy handle coll))))))))

;;; ---- exact planar distance ---------------------------------------------

(defun approx= (a b &optional (eps 1d-6)) (<= (abs (- a b)) eps))

(test distance-exact-points-is-planar
  "geometry-distance-exact between two points is the PLANAR (Euclidean) distance
in coordinate units: (0,0)-(3,4) = 5.0 (degrees, NOT metres)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx= 5d0 (geometry-distance-exact (make-point 0d0 0d0)
                                                (make-point 3d0 4d0))))))

(test distance-exact-polygons
  "Distance between disjoint polygons is the gap; overlapping polygons are 0."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((a (make-polygon '(((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0) (0d0 0d0)))))
            (b (make-polygon '(((3d0 0d0) (4d0 0d0) (4d0 1d0) (3d0 1d0) (3d0 0d0)))))
            (c (make-polygon '(((0.5d0 0d0) (2d0 0d0) (2d0 1d0) (0.5d0 1d0) (0.5d0 0d0))))))
        (is (approx= 2d0 (geometry-distance-exact a b)) "gap from x=1 to x=3")
        (is (approx= 0d0 (geometry-distance-exact a c)) "overlapping -> 0"))))
