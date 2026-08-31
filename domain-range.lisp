;;;; Domain and range: the classes an edge type's endpoints may have,
;;;; enforced at COMMIT and surveyable by an audit pass (GH #156, ontology
;;;; unit 3 of #109).
;;;;
;;;; The fifth schema registry, sharing %SPEC-IDENTITY with the others
;;;; (GH #139/#140).  The first family that needs the OTHER END of a
;;;; relation: it is read through the COMMIT-VIEW's VIEW-NODE, so an
;;;; endpoint created in the same commit is found in the writes, and one
;;;; already in the store is read under the manager lock -- the same class
;;;; of read the unique validator performs.  A missing endpoint and a
;;;; wrong-typed one are different failures and stay so.  Design:
;;;; docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md §3.

(in-package :graph-db)

(define-condition domain-range-violation (error)
  ((edge-type   :initarg :edge-type   :reader drv-edge-type)
   (end         :initarg :end         :reader drv-end)         ; :from / :to
   (reason      :initarg :reason      :reader drv-reason)      ; see below
   (actual      :initarg :actual      :reader drv-actual)      ; class or NIL
   (expected    :initarg :expected    :reader drv-expected)    ; class list
   (node-id     :initarg :node-id     :reader drv-node-id)     ; the edge
   (endpoint-id :initarg :endpoint-id :reader drv-endpoint-id))
  (:report
   (lambda (c s)
     (ecase (drv-reason c)
       (:dangling
        (format s "Domain/range constraint on ~S violated by edge ~A: its ~
                   ~(~A~) endpoint ~A does not exist."
                (drv-edge-type c) (string-id (drv-node-id c))
                (drv-end c) (string-id (drv-endpoint-id c))))
       (:wrong-type
        (format s "Domain/range constraint on ~S violated by edge ~A: its ~
                   ~(~A~) endpoint is a ~S; expected one of~{ ~S~}."
                (drv-edge-type c) (string-id (drv-node-id c))
                (drv-end c) (drv-actual c) (drv-expected c)))))))

(defvar *schema-domain-range-metadata* (make-hash-table)
  "graph-name (symbol) -> list of DOMAIN-RANGE-SPECs (newest first).")

(defstruct (domain-range-spec (:constructor make-domain-range-spec))
  edge-type graph-name domain range name)

(defun domain-range-spec-identity (spec)
  "See %SPEC-IDENTITY (index.lisp).  Unnamed identity is the edge type
alone: an edge type has ONE domain/range declaration."
  (%spec-identity (domain-range-spec-edge-type spec) '(:endpoints)
                  (domain-range-spec-name spec)))

(defun %class-list (x)
  (cond ((null x) nil) ((listp x) x) (t (list x))))

(defun register-domain-range-spec (spec)
  "Record SPEC, REPLACING any spec of the same identity (GH #139).  Signals
when SPEC constrains neither end."
  (when (and (null (domain-range-spec-domain spec))
             (null (domain-range-spec-range spec)))
    (error "Domain/range constraint on ~S declares neither :DOMAIN nor ~
            :RANGE, so it constrains nothing."
           (domain-range-spec-edge-type spec)))
  (let* ((g (domain-range-spec-graph-name spec))
         (id (domain-range-spec-identity spec))
         (existing (gethash g *schema-domain-range-metadata*))
         (hit (find id existing :key #'domain-range-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-domain-range-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-domain-range-spec (edge-type graph-name &key name)
  "Withdraw the declaration for EDGE-TYPE, by :NAME or by the type alone.
T if one was withdrawn.  ⚠ A NAME interned in another package withdraws
nothing (GH #152)."
  (let* ((id (%spec-identity edge-type (unless name '(:endpoints)) name))
         (existing (gethash graph-name *schema-domain-range-metadata*))
         (hit (find id existing :key #'domain-range-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-domain-range-metadata*)
            (remove hit existing))
      t)))

(defun %registered-domain-range-specs (graph)
  (gethash (graph-name graph) *schema-domain-range-metadata*))

(defun class-domain-range-specs (class graph)
  "Specs applying to edge CLASS: the declared type is CLASS or an ancestor,
so a subtype inherits its parent's endpoint rule."
  (when (class-finalized-p class)
    (loop for spec in (%registered-domain-range-specs graph)
          when (subtypep (class-name class)
                         (domain-range-spec-edge-type spec))
          collect spec)))

(defmacro def-domain-range (edge-type graph-name &key domain range name)
  "Declare the classes an EDGE-TYPE's endpoints may have in GRAPH-NAME:
:DOMAIN for FROM, :RANGE for TO, each a class name or a list of them
(subtypes admitted); an end left NIL is unconstrained.  Enforced at
commit on every create of such an edge, subtypes included: the endpoint
is read through the commit view, so one created in the same commit is
found, and a MISSING endpoint (:DANGLING -- a raw id nothing answers to)
is reported apart from a WRONG-TYPED one (:WRONG-TYPE).  Together with
DEF-CARDINALITY this is what DEF-EDGE's endpoint comments meant (GH #156).
:NAME the declaration whenever a macro emits it (GH #139)."
  `(register-domain-range-spec
    (make-domain-range-spec
     :edge-type ',edge-type
     :graph-name ',graph-name
     :domain ',(%class-list domain)
     :range ',(%class-list range)
     :name ',name)))

(defmacro undef-domain-range (edge-type graph-name &key name)
  "Withdraw a DEF-DOMAIN-RANGE declaration, by :NAME or by the edge type."
  `(unregister-domain-range-spec ',edge-type ',graph-name :name ',name))

;;; --- Evaluation ----------------------------------------------------------

(defstruct (dr-violation (:constructor %make-dr-violation))
  spec node-id end reason actual endpoint-id)

(defun %endpoint-violation (spec edge end view)
  "The violation, if any, for EDGE's END (:FROM / :TO) under SPEC."
  (let ((allowed (ecase end
                   (:from (domain-range-spec-domain spec))
                   (:to (domain-range-spec-range spec)))))
    (when allowed
      (let* ((endpoint-id (ecase end (:from (from edge)) (:to (to edge))))
             (v (view-node view endpoint-id)))
        (cond ((null v)
               (%make-dr-violation :spec spec :node-id (id edge) :end end
                                   :reason :dangling :actual nil
                                   :endpoint-id endpoint-id))
              ((notany (lambda (c) (typep v c)) allowed)
               (%make-dr-violation :spec spec :node-id (id edge) :end end
                                   :reason :wrong-type
                                   :actual (class-name (class-of v))
                                   :endpoint-id endpoint-id)))))))

(defun %domain-range-violations (edge graph view)
  "Every domain/range constraint EDGE violates under VIEW, as DR-VIOLATION
records.  The one evaluator behind the write path and the audit pass."
  (loop for spec in (class-domain-range-specs (class-of edge) graph)
        for from-v = (%endpoint-violation spec edge :from view)
        for to-v = (%endpoint-violation spec edge :to view)
        when from-v collect from-v
        when to-v collect to-v))

(defun %signal-dr-violation (v)
  (let ((spec (dr-violation-spec v)))
    (error 'domain-range-violation
           :edge-type (domain-range-spec-edge-type spec)
           :end (dr-violation-end v)
           :reason (dr-violation-reason v)
           :actual (dr-violation-actual v)
           :expected (ecase (dr-violation-end v)
                       (:from (domain-range-spec-domain spec))
                       (:to (domain-range-spec-range spec)))
           :node-id (dr-violation-node-id v)
           :endpoint-id (dr-violation-endpoint-id v))))

(defun validate-domain-range-constraints (tx graph)
  "Signal DOMAIN-RANGE-VIOLATION if TX writes a live edge whose endpoint
is missing or of a class its type does not admit.  Called in %COMMIT's
manager-locked region beside the other validators, before durability
(GH #156)."
  (when (%registered-domain-range-specs graph)
    (let ((view (make-commit-view graph tx)))
      (dolist (w (writes tx))
        (let ((node (node w)))
          (when (and (typep node 'edge) (not (deleted-p node)))
            (let ((v (first (%domain-range-violations node graph view))))
              (when v (%signal-dr-violation v)))))))))

(defun check-domain-range-constraints (graph &key edge-type)
  "Survey live edges of GRAPH and COLLECT domain/range violations without
signalling.  Returns (values VIOLATIONS CHECKED-COUNT SPEC-COUNT) -- read
the counts: zero violations over zero specs is an unchecked graph."
  (let ((violations '())
        (checked 0)
        (view (make-commit-view graph)))
    (map-edges (lambda (e)
                 (incf checked)
                 (let ((vs (%domain-range-violations e graph view)))
                   (when vs (setf violations (nconc violations vs)))))
               graph :edge-type edge-type)
    (values violations checked
            (length (%registered-domain-range-specs graph)))))
