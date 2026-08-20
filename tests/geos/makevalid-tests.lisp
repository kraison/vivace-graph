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
unevaluated.

⚠ WHICH PATH IT EXERCISES: the NON-collection one.  A wholly
degenerate ring repairs to a MULTILINESTRING, so the GEOS-ERROR comes
from WKT->GEOMETRY having no such kind -- the pre-#163 refusal,
unchanged.  It passes with the polygonal-parts branch present or
absent; the %GEOS-REPAIRED->GEOMETRY tests below guard that branch."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (is-false (geometry-valid-p (collapsed-ring))
                     "fixture sanity: the collapsed ring is invalid")
           (signals geos-error (geometry-make-valid (collapsed-ring))))))

(defun call-with-geos-collection (ctx geometries fn)
  "Call FN on a GEOS GEOMETRYCOLLECTION pointer built from GEOMETRIES,
then destroy it.  Collections like these are built by hand because no
ring in reach makes GEOSMakeValid emit one -- a wholly degenerate ring
comes back as a MULTILINESTRING instead.

⚠ GEOSGeom_createCollection_r TAKES OWNERSHIP of its members, so
destroying the collection frees them too; FN must not free anything."
  (let ((handle (graph-db::geos-ctx-handle ctx))
        (n (length geometries)))
    (cffi:with-foreign-object (arr :pointer n)
      (loop for g in geometries
            for i from 0
            do (setf (cffi:mem-aref arr :pointer i) (geometry->geos ctx g)))
      (let ((coll (graph-db::%geos-create-collection
                   handle graph-db::+geos-geometrycollection+ arr n)))
        (unwind-protect (funcall fn coll)
          (graph-db::%geos-geom-destroy handle coll))))))

(test a-polygon-free-collection-is-refused-not-emptied
  "The same rule one level down, on a GEOMETRYCOLLECTION built by hand:
%GEOS-REPAIRED->GEOMETRY has no polygonal part to keep, so it signals."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-geos-context (ctx)
        (call-with-geos-collection
         ctx (list (make-point 0d0 0d0)
                   (make-linestring '((0d0 0d0) (1d0 1d0))))
         (lambda (coll)
           (signals geos-error
             (graph-db::%geos-repaired->geometry ctx coll)))))))

(test an-empty-polygonal-part-is-refused-too
  "⚠ THE SIBLING HOLE, ONE TYPE ID TO THE LEFT.  POLYGON EMPTY's type id
is +GEOS-POLYGON+, so it IS collected as a polygonal part: the no-part
guard never fires, WKT->GEOMETRY parses \"POLYGON EMPTY\" into a real
empty geometry, and %OVERLAP-FRACTION reads its zero measure as fraction
1.0 in every candidate region the ORIGINAL ring turned up.  That is the
fabrication the polygon-free branch exists to stop, arriving one step
over (GH #163)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-geos-context (ctx)
        (call-with-geos-collection
         ctx (list (make-polygon '())
                   (make-linestring '((0d0 0d0) (1d0 1d0))))
         (lambda (coll)
           ;; Fixture sanity, and it is the whole point of this test: the
           ;; empty member must really reach the walker AS A POLYGON, or
           ;; this passes for the polygon-free reason instead.
           (let ((part (graph-db::%geos-get-geometry-n
                        (graph-db::geos-ctx-handle ctx) coll 0)))
             (is (= graph-db::+geos-polygon+
                    (graph-db::%geos-geom-type-id
                     (graph-db::geos-ctx-handle ctx) part))
                 "fixture sanity: member 0 is a POLYGON, not something ~
the no-part guard would catch")
             (is-true (geometry-empty-p (geos->geometry ctx part))
                      "fixture sanity: and it really is empty"))
           (signals geos-error
             (graph-db::%geos-repaired->geometry ctx coll)))))))

(test a-top-level-empty-repair-is-refused-too
  "⚠ THE SAME HOLE ONE BRANCH TO THE LEFT.  A top-level POLYGON EMPTY is
not a GEOMETRYCOLLECTION, so it took the pass-through branch and slipped
the guard entirely -- %REPAIRED would hand REGISTER-GEOMETRY a
zero-measure subject and %OVERLAP-FRACTION answers 1.0 for every
candidate region the ORIGINAL ring turned up.  The guard is hoisted
above the branch so both paths meet it.

⚠ DEFENSIVE CONSISTENCY, NOT A LIVE BUG: no reachable input is known.
GEOSMakeValid_r defaults to linework mode, which preserves linework as
LINES rather than collapsing it to an empty polygon, and the structure
mode that yields empties is not bound here.  Fixed anyway because the
manual and the CHANGELOG state the guard unconditionally, and because
the sibling hole above was ruled in on the same grounds (GH #163)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (with-geos-context (ctx)
        (let* ((handle (graph-db::geos-ctx-handle ctx))
               (empty (geometry->geos ctx (make-polygon '()))))
          (unwind-protect
               (progn
                 ;; Fixture sanity: it must reach the function as a
                 ;; POLYGON, or this passes on the collection branch.
                 (is (= graph-db::+geos-polygon+
                        (graph-db::%geos-geom-type-id handle empty))
                     "fixture sanity: a POLYGON, not a collection")
                 (is-true (geometry-empty-p (geos->geometry ctx empty))
                          "fixture sanity: and it really is empty")
                 (signals geos-error
                   (graph-db::%geos-repaired->geometry ctx empty)))
            (graph-db::%geos-geom-destroy handle empty))))))

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
