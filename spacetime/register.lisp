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

(defun register-geometry (geometry registry
                          &key (registry-graph graph-db:*graph*))
  "Registrations of GEOMETRY against REGISTRY's regions in REGISTRY-GRAPH.

REGISTRY is a spatial SCOPE -- a node-class name, a list of them, or
:ALL (spatial-query.lisp).  ⚠ REGISTRY-GRAPH is the REGISTRY'S graph, not
the subject's; REGISTER-NODE's :GRAPH is the subject's, and passing that
here reads every region under the wrong binding, which NODE-GEOMETRY's
IGNORE-ERRORS turns into an empty list with EVALUATED-P true (GH #53).

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
The scan is unevaluated for any of three reasons: GEOS is absent and the
geometry is extended -- the index falls back to a COARSE bounding box,
which is over-inclusive, and a fraction cannot be computed at all; GEOS
rejects the geometry as invalid, which is host-dependent; or the
intersection GEOS returns is a kind this engine's GEOMETRY type cannot
represent -- a MULTILINESTRING, from two polygons sharing only an edge
(design §6)."
  (if (and (%extended-geometry-p geometry)
           (not graph-db::*geos-available-p*))
      (values nil nil)
      (handler-case
          ;; Region slots are read under the registry graph's own binding:
          ;; NODE-SLOT-VALUE defaults to *GRAPH*, and reading a node under
          ;; the wrong one is the node-escape class (design §7, GH #53).
          (let* ((graph-db:*graph* registry-graph)
                 (measure (%measure-fn geometry))
                 (subject-measure (funcall measure geometry)))
            (values
             (loop for region in (graph-db:find-nodes-intersecting
                                  registry geometry :graph registry-graph)
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

;;; --- REGISTER-NODE: the registration, written as claims -----------------

(defun %call-or-nil (fname node)
  "(FUNCALL FNAME NODE) when FNAME is non-NIL, else NIL.  The facet's
:PRECISION-FN, :CONFIDENCE-FN and :METHOD-FN are required KEYS whose
value may be NIL -- a source with no per-record measure of any of the
three says so explicitly (design §3)."
  (when fname (funcall fname node)))

(defun %source-endpoint (node graph)
  "NODE's claim endpoint as PLAIN VALUES: (VALUES namespace key), read
under GRAPH from its class's :IDENTITY facet.  Signals NOT-A-SOURCE when
that class was not defined with DEF-SOURCE -- an OBJECT-KEY is part of a
claim's identity, so a registry with no external key cannot be registered
against at all.  Both values are NIL for :IDENTITY :NONE, and the claim
constructor then refuses the write naming the missing component."
  (let ((identity (source-facets-identity
                   (source-contract (type-of node)))))
    (if (eq identity :none)
        (values nil nil)
        (values (getf identity :namespace)
                (graph-db:node-slot-value node (getf identity :key-slot)
                                          :graph graph)))))

(defun %claim-constructor (class)
  "The MAKE-<CLASS> function DEF-VERTEX generated.  Interned in CLASS's own
package: a tenant's claim classes are named where DEF-CLAIM-CLASSES was
read, not here (claim.lisp)."
  (fdefinition (intern (concatenate 'string "MAKE-" (symbol-name class))
                       (symbol-package class))))

(defun %registration-claim-p (c binary relation producer object-ns
                              object-key)
  "True when claim C carries this registration's identity beyond the
subject pair the index lookup already matched.  PRODUCER is checked
because it is part of DEF-UNIQUE's binary tuple -- matching on relation
and object alone would update another producer's claim.  The TYPEP guard
keeps the parent-class lookup's UNARY hits away from the object
accessors, which only BINARY has."
  (and (typep c binary)
       (equal relation (claim-relation c))
       (equal producer (claim-producer c))
       (equal object-ns (claim-object-namespace c))
       (equal object-key (claim-object-key c))))

(defun %upsert-registration-claim (registration facet subject-ns
                                   subject-key precision confidence method
                                   registry-graph)
  "Create or update the one claim binding (SUBJECT-NS . SUBJECT-KEY) to
REGISTRATION's region under FACET's contract.  Returns T; REGISTER-NODE
counts created and updated claims alike.

METHOD is already resolved by the caller -- FACET's :METHOD-FN result
when non-NIL, else its static :METHOD string (design §3) -- so both the
insert and update branches write the same value a re-registration would
also produce, which is what makes the UPDATE branch overwrite a stale
method rather than leaving it (plan Task 2).

Idempotent on DEF-UNIQUE's binary tuple -- PRODUCER, the subject pair, the
object pair and RELATION.  Looked up through the declared subject index
and filtered in Lisp, since claims per subject are few (design §4).

⚠ Runs under an ambient GRAPH-DB:*GRAPH* of REGISTRY-GRAPH: that is what
WITH-TRANSACTION's default transaction manager is taken from.  A mutation
needs (COPY c) then (SAVE c), and the COPY must happen INSIDE the
transaction or SAVE signals MODIFYING-NON-COPY."
  (let* ((family (claim-family (getf facet :claim-class)))
         (binary (claim-family-binary family))
         (relation (getf facet :relation))
         (producer (getf facet :producer))
         ;; OBJECT-NS is the FACET's; OBJECT-KEY is the region's own
         ;; :IDENTITY.  Nothing checks the two agree, deliberately: the
         ;; facet must be able to name the namespace deployed claims
         ;; already carry, which need not be the registry's own (design §3).
         (object-ns (getf facet :registry-namespace))
         (object-key (nth-value 1 (%source-endpoint
                                   (getf registration :region)
                                   registry-graph)))
         (existing (find-if (lambda (c)
                              (%registration-claim-p c binary relation
                                                     producer object-ns
                                                     object-key))
                            (graph-db:index-lookup
                             registry-graph (claim-family-parent family)
                             '(subject-namespace subject-key)
                             (list subject-ns subject-key)))))
    (graph-db:with-transaction ()
      (if existing
          (let ((c (graph-db:copy existing)))
            (setf (claim-method c) method
                  (claim-rule-version c) (getf facet :rule-version)
                  (claim-confidence c) confidence
                  (claim-precision-m c) precision
                  (claim-fraction c) (getf registration :fraction)
                  (claim-standing c) :inferred)
            (graph-db:save c))
          (funcall (%claim-constructor binary)
                   :subject-namespace subject-ns :subject-key subject-key
                   :object-namespace object-ns :object-key object-key
                   :relation relation :producer producer
                   :method method
                   :rule-version (getf facet :rule-version)
                   :confidence confidence :precision-m precision
                   :fraction (getf registration :fraction)
                   ;; A registration is DERIVED by computation, which is
                   ;; what :INFERRED means.  Not configurable (design §3).
                   :standing :inferred)))
    t))

(defun register-node (node &key (graph graph-db:*graph*)
                                (registry-graph graph))
  "Register NODE against its source contract's registry, writing one claim
per region.  Two values: how many claims were written, and whether the
scan was EVALUATED at all (see REGISTER-GEOMETRY).

A source declaring :REGISTRATION :NONE writes nothing and reports an
EVALUATED scan -- structural absence, not an unanswered question.  Every
claim is written with STANDING :INFERRED (design §3).

⚠ Read (VALUES 0 NIL) as 'not answered', never as 'no region here'.  It is
what a subject with no geometry gets too: where the record is, is unknown,
which is not the same as its being in no region.

⚠ NODE's slots are read under GRAPH and only PLAIN VALUES cross out; a
node object never leaves its graph's binding (GH #53).  The claim write is
REGISTRY-GRAPH-local, which 3.0's single-graph write transaction requires
anyway."
  (let ((facet (source-facets-registration
                (source-contract (type-of node)))))
    (if (eq facet :none)
        (values 0 t)
        (let (geometry subject-ns subject-key precision confidence method)
          (let ((graph-db:*graph* graph))
            (setf geometry (graph-db:node-geometry node)
                  precision (%call-or-nil (getf facet :precision-fn) node)
                  confidence (%call-or-nil (getf facet :confidence-fn)
                                           node)
                  ;; Non-NIL :METHOD-FN wins; NIL means "no per-record
                  ;; measure", so the facet's own static :METHOD string is
                  ;; written, exactly as before :METHOD-FN existed
                  ;; (design §3, plan Task 2).
                  method (or (%call-or-nil (getf facet :method-fn) node)
                             (getf facet :method)))
            (multiple-value-setq (subject-ns subject-key)
              (%source-endpoint node graph)))
          (if (null geometry)
              (values 0 nil)
              (multiple-value-bind (regs evaluated)
                  (register-geometry geometry (getf facet :registry)
                                     :registry-graph registry-graph)
                (if (not evaluated)
                    (values 0 nil)
                    (let ((graph-db:*graph* registry-graph))
                      (values (loop for r in regs
                                    count (%upsert-registration-claim
                                           r facet subject-ns subject-key
                                           precision confidence method
                                           registry-graph))
                              t)))))))))
