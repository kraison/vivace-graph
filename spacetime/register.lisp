;;;; spacetime/register.lisp -- binding geometry to a registry.
;;;; Design: docs/superpowers/specs/2026-08-19-registration-design.md (#138).

(in-package #:graph-db.spacetime)

(defun %extended-geometry-p (g)
  "True when G's overlap needs GEOS.  A :POINT's does not (design §6)."
  (member (graph-db:geometry-kind g)
          '(:polygon :multipolygon :linestring)))

(defun %overlap-fraction (subject region-geometry subject-area)
  "How much of SUBJECT falls within REGION-GEOMETRY, in [0,1].
A zero-area subject -- a point or a line -- is wholly wherever it is
found, so it takes 1.0 rather than dividing by zero."
  (if (zerop subject-area)
      1.0d0
      (/ (graph-db:geometry-geodesic-area
          (graph-db:geometry-intersection subject region-geometry))
         subject-area)))

(defun register-geometry (geometry registry &key (graph graph-db:*graph*))
  "Registrations of GEOMETRY against REGISTRY's regions in GRAPH.

REGISTRY is a spatial SCOPE -- a node-class name, a list of them, or
:ALL (spatial-query.lisp).

Two values: a list of (:REGION node :FRACTION double), and whether the
scan was EVALUATED at all.  A registration is PARTIAL AND FRACTIONAL: a
point takes fraction 1.0, a polygon or line takes its share of each
region it meets.  The list is UNORDERED -- 'most specific' is a tenant's
notion, so a tenant sorts.

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
          (let ((graph-db:*graph* graph)
                (subject-area (graph-db:geometry-geodesic-area geometry)))
            (values
             (loop for region in (graph-db:find-nodes-intersecting
                                  registry geometry :graph graph)
                   for g = (graph-db:node-geometry region)
                   when g
                     collect (list :region region
                                   :fraction
                                   (%overlap-fraction geometry g
                                                      subject-area)))
             t))
        ;; ONLY geos-error: broader would swallow the node-escape class
        ;; (GH #53).
        (graph-db:geos-error () (values nil nil)))))
