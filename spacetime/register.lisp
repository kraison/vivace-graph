;;;; spacetime/register.lisp -- binding geometry to a registry.
;;;; Design: docs/superpowers/specs/2026-08-19-registration-design.md (#138).

(in-package #:graph-db.spacetime)

(defun %extended-geometry-p (g)
  "True when G's overlap needs GEOS.  A :POINT's does not (design §6)."
  (member (graph-db:geometry-kind g)
          '(:polygon :multipolygon :linestring)))

(defun %measure-fn (subject)
  "The measure a fraction of SUBJECT is taken against: LENGTH for a line
-- whose AREA is zero, so an area ratio would give it 1.0 in every
region it crosses -- and AREA otherwise (design §13)."
  (if (eq (graph-db:geometry-kind subject) :linestring)
      #'graph-db:geometry-geodesic-length
      #'graph-db:geometry-geodesic-area))

(defun %overlap-fraction (subject region-geometry measure subject-measure)
  "How much of SUBJECT falls within REGION-GEOMETRY, in [0,1], under
MEASURE (%MEASURE-FN) with SUBJECT-MEASURE its value for SUBJECT.
A zero-measure subject -- a point, or a degenerate line -- is wholly
wherever it is found, so it takes 1.0 rather than dividing by zero."
  (if (zerop subject-measure)
      1.0d0
      (/ (funcall measure
                  (graph-db:geometry-intersection subject region-geometry))
         subject-measure)))

(defun register-geometry (geometry registry &key (graph graph-db:*graph*))
  "Registrations of GEOMETRY against REGISTRY's regions in GRAPH.

REGISTRY is a spatial SCOPE -- a node-class name, a list of them, or
:ALL (spatial-query.lisp).

Two values: a list of (:REGION node :FRACTION double), and whether the
scan was EVALUATED at all.  A registration is PARTIAL AND FRACTIONAL: a
point takes fraction 1.0, a polygon its share of each region's AREA, a
line its share by LENGTH -- a line's area is zero, so an area ratio
would give it 1.0 everywhere it went (design §13).  The list is
UNORDERED -- 'most specific' is a tenant's notion, so a tenant sorts.

A region the subject merely TOUCHES is NOT registered: GEOS `intersects'
is true for boundary contact, so an abutting region is a candidate whose
fraction is 0, and writing it would bind a record to a region it does
not overlap.

⚠ Read (VALUES NIL NIL) as 'not answered', never as 'no region here'.
The scan is unevaluated when GEOS is absent for an extended geometry --
the index falls back to a COARSE bounding box, which is over-inclusive,
and a fraction cannot be computed at all -- or when GEOS rejects the
geometry as invalid, which is host-dependent (design §6)."
  (if (and (%extended-geometry-p geometry)
           (not graph-db::*geos-available-p*))
      (values nil nil)
      (handler-case
          ;; Region slots are read under the registry graph's own binding:
          ;; NODE-SLOT-VALUE defaults to *GRAPH*, and reading a node under
          ;; the wrong one is the node-escape class (design §7, GH #53).
          (let* ((graph-db:*graph* graph)
                 (measure (%measure-fn geometry))
                 (subject-measure (funcall measure geometry)))
            (values
             (loop for region in (graph-db:find-nodes-intersecting
                                  registry geometry :graph graph)
                   for g = (graph-db:node-geometry region)
                   for f = (and g (%overlap-fraction geometry g measure
                                                     subject-measure))
                   ;; A zero fraction is a TOUCH, not an overlap: dropped
                   ;; rather than written as a claim (design §13).
                   when (and f (plusp f))
                     collect (list :region region :fraction f))
             t))
        ;; ONLY geos-error: broader would swallow the node-escape class
        ;; (GH #53).
        (graph-db:geos-error () (values nil nil)))))
