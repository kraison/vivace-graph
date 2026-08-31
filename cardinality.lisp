;;;; Cardinality constraints: how many edges of a type a vertex may have,
;;;; enforced at COMMIT and surveyable by an audit pass (GH #155, ontology
;;;; unit 2 of #109).
;;;;
;;;; The fourth schema registry, sharing %SPEC-IDENTITY with the index,
;;;; unique and value-constraint registries (GH #139/#140).  The count is
;;;; the first thing a constraint family needs that lives OUTSIDE the node:
;;;; it is read through the COMMIT-VIEW (value-constraint.lisp) -- the
;;;; store's adjacency index overlaid with this transaction's edge writes
;;;; -- so a commit that adds and removes edges is counted against its
;;;; POST-commit state.  Design:
;;;; docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md §3.

(in-package :graph-db)

(define-condition cardinality-violation (error)
  ((class-name :initarg :class-name :reader cdv-class-name)
   (edge-type  :initarg :edge-type  :reader cdv-edge-type)
   (direction  :initarg :direction  :reader cdv-direction)
   (actual     :initarg :actual     :reader cdv-actual)
   (min        :initarg :min        :reader cdv-min)
   (max        :initarg :max        :reader cdv-max)
   (node-id    :initarg :node-id    :reader cdv-node-id))
  (:report
   (lambda (c s)
     (format s "Cardinality constraint on ~S violated by node ~A: ~
                ~D ~(~A~) edge~:P of type ~S, ~@[at least ~D~]~@[~* and ~]~
                ~@[at most ~D~] allowed."
             (cdv-class-name c) (string-id (cdv-node-id c))
             (cdv-actual c) (cdv-direction c) (cdv-edge-type c)
             (cdv-min c) (and (cdv-min c) (cdv-max c)) (cdv-max c)))))

(defvar *schema-cardinality-metadata* (make-hash-table)
  "graph-name (symbol) -> list of CARDINALITY-SPECs (newest first).")

(defstruct (cardinality-spec (:constructor make-cardinality-spec))
  owner-name edge-type graph-name direction min max name)

(defun cardinality-spec-identity (spec)
  "See %SPEC-IDENTITY (index.lisp): (OWNER . NAME) when named, else
(OWNER . (EDGE-TYPE DIRECTION)).  One identity rule across all four
registries (GH #140)."
  (%spec-identity (cardinality-spec-owner-name spec)
                  (list (cardinality-spec-edge-type spec)
                        (cardinality-spec-direction spec))
                  (cardinality-spec-name spec)))

(defun register-cardinality-spec (spec)
  "Record SPEC, REPLACING any spec of the same identity (GH #139).  Signals
when SPEC bounds nothing, or bounds it impossibly."
  (let ((min (cardinality-spec-min spec))
        (max (cardinality-spec-max spec))
        (dir (cardinality-spec-direction spec)))
    (unless (member dir '(:in :out))
      (error "Cardinality constraint on ~S/~S: :DIRECTION must be :IN or ~
              :OUT, not ~S."
             (cardinality-spec-owner-name spec)
             (cardinality-spec-edge-type spec) dir))
    (when (and (null min) (null max))
      (error "Cardinality constraint on ~S/~S declares neither :MIN nor ~
              :MAX, so it constrains nothing."
             (cardinality-spec-owner-name spec)
             (cardinality-spec-edge-type spec)))
    (unless (and (or (null min) (typep min '(integer 0)))
                 (or (null max) (typep max '(integer 0)))
                 (or (null min) (null max) (<= min max)))
      (error "Cardinality constraint on ~S/~S: :MIN ~S and :MAX ~S must be ~
              non-negative integers with MIN <= MAX."
             (cardinality-spec-owner-name spec)
             (cardinality-spec-edge-type spec) min max)))
  (let* ((g (cardinality-spec-graph-name spec))
         (id (cardinality-spec-identity spec))
         (existing (gethash g *schema-cardinality-metadata*))
         (hit (find id existing :key #'cardinality-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-cardinality-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-cardinality-spec (owner-name graph-name
                                    &key edge-type (direction :out) name)
  "Withdraw the declaration identified by (OWNER . NAME), or by
(OWNER . (EDGE-TYPE DIRECTION)) for an unnamed one.  T if one was
withdrawn.  ⚠ A NAME interned in another package withdraws nothing
(GH #152) -- name the package when the spec was macro-emitted."
  (let* ((id (%spec-identity owner-name
                             (when edge-type (list edge-type direction))
                             name))
         (existing (gethash graph-name *schema-cardinality-metadata*))
         (hit (find id existing :key #'cardinality-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-cardinality-metadata*)
            (remove hit existing))
      t)))

(defun %registered-cardinality-specs (graph)
  (gethash (graph-name graph) *schema-cardinality-metadata*))

(defun class-cardinality-specs (class graph)
  "Specs from the registry applying to CLASS: owner is CLASS or an
ancestor.  Mirrors CLASS-VALUE-CONSTRAINT-SPECS."
  (when (class-finalized-p class)
    (loop for spec in (%registered-cardinality-specs graph)
          when (subtypep (class-name class)
                         (cardinality-spec-owner-name spec))
          collect spec)))

(defmacro def-cardinality (owner-class edge-type graph-name
                           &key (direction :out) min max name)
  "Declare that a vertex of OWNER-CLASS in GRAPH-NAME has between :MIN and
:MAX edges of EDGE-TYPE (subtypes included, as OUTGOING-EDGES counts
them), :DIRECTION :OUT (edges FROM it, the default) or :IN (edges TO it).
Enforced at commit on every write path: on the vertex's own writes and on
every create or delete of such an edge, counted against the transaction's
POST-commit state -- a vertex and its edges created in one commit are one
answer, not a min violation followed by a fix (GH #155).

⚠ A pre-existing violation blocks the vertex's later unrelated updates,
exactly as unit 1's :REQUIRED does: the check reads whole post-commit
state, not the delta.  The audit pass is how such nodes are found first.
The count is O(degree) per constrained vertex per commit.  :NAME the
declaration whenever a macro emits it (GH #139)."
  `(register-cardinality-spec
    (make-cardinality-spec
     :owner-name ',owner-class
     :edge-type ',edge-type
     :graph-name ',graph-name
     :direction ,direction
     :min ,min
     :max ,max
     :name ',name)))

(defmacro undef-cardinality (owner-class graph-name
                             &key edge-type (direction :out) name)
  "Withdraw a DEF-CARDINALITY declaration, by :NAME or by :EDGE-TYPE (and
:DIRECTION)."
  `(unregister-cardinality-spec ',owner-class ',graph-name
                                :edge-type ',edge-type
                                :direction ,direction :name ',name))

;;; --- Counting through the view -------------------------------------------

(defun %edge-endpoint-id (edge direction)
  "The id of the vertex an edge of DIRECTION hangs off: FROM for :OUT
(edges out of the vertex), TO for :IN."
  (ecase direction
    (:out (from edge))
    (:in (to edge))))

(defun %overlay-edge-delta (view vertex-id edge-type direction)
  "How this transaction's writes change VERTEX-ID's count of DIRECTION
edges of EDGE-TYPE: +1 per live create, -1 per delete of an edge that was
live before, 0 for anything else (an update that moves nothing; an edge
created and deleted in the same transaction)."
  (let ((delta 0))
    (dolist (w (view-writes view) delta)
      (let ((e (node w)))
        (when (and (typep e 'edge)
                   (typep e edge-type)
                   (equalp (%edge-endpoint-id e direction) vertex-id))
          (cond ((deleted-p e)
                 (when (and (typep w 'tx-update)
                            (let ((old (old-node w)))
                              (and old (not (deleted-p old)))))
                   (decf delta)))
                ((typep w 'tx-create) (incf delta))))))))

(defun view-edge-count (view vertex edge-type direction)
  "VERTEX's post-commit count of DIRECTION edges of EDGE-TYPE under VIEW:
the store's adjacency index -- what OUTGOING-EDGES / INCOMING-EDGES
answer, soft-deleted excluded, subtypes included -- plus the
transaction's overlay.  In a store-only view the overlay is empty."
  (+ (length (map-edges 'identity (commit-view-graph view)
                        :vertex vertex :edge-type edge-type
                        :direction direction :collect-p t))
     (%overlay-edge-delta view (id vertex) edge-type direction)))

;;; --- Evaluation ----------------------------------------------------------

(defstruct (cd-violation (:constructor %make-cd-violation))
  spec node-id class-name actual)

(defun %cardinality-violations (vertex graph view)
  "Every cardinality constraint VERTEX violates under VIEW, as
CD-VIOLATION records.  The one evaluator behind the write path and the
audit pass."
  (let ((class (class-of vertex)))
    (loop for spec in (class-cardinality-specs class graph)
          for n = (view-edge-count view vertex
                                   (cardinality-spec-edge-type spec)
                                   (cardinality-spec-direction spec))
          when (or (and (cardinality-spec-min spec)
                        (< n (cardinality-spec-min spec)))
                   (and (cardinality-spec-max spec)
                        (> n (cardinality-spec-max spec))))
            collect (%make-cd-violation :spec spec :node-id (id vertex)
                                        :class-name (class-name class)
                                        :actual n))))

(defun %signal-cd-violation (v)
  (let ((spec (cd-violation-spec v)))
    (error 'cardinality-violation
           :class-name (cd-violation-class-name v)
           :edge-type (cardinality-spec-edge-type spec)
           :direction (cardinality-spec-direction spec)
           :actual (cd-violation-actual v)
           :min (cardinality-spec-min spec)
           :max (cardinality-spec-max spec)
           :node-id (cd-violation-node-id v))))

(defun %vertices-to-check (tx graph view)
  "The vertices whose counts this transaction can have changed, each once:
every vertex it writes, and each live endpoint of every edge it creates
or deletes -- resolved through the view, so an endpoint created in the
same commit is found in the writes, not missed in the store."
  (let ((seen (make-hash-table :test 'equalp))
        (out '()))
    (flet ((consider (v)
             (when (and v (not (deleted-p v))
                        (not (gethash (id v) seen))
                        (class-cardinality-specs (class-of v) graph))
               (setf (gethash (id v) seen) t)
               (push v out))))
      (dolist (w (writes tx) out)
        (let ((node (node w)))
          (cond ((typep node 'vertex) (consider node))
                ((and (typep node 'edge)
                      (or (typep w 'tx-create) (deleted-p node)))
                 (consider (view-node view (from node)))
                 (consider (view-node view (to node))))))))))

(defun validate-cardinality-constraints (tx graph)
  "Signal CARDINALITY-VIOLATION if TX leaves any constrained vertex with
too few or too many edges.  Called in %COMMIT's manager-locked region
beside VALIDATE-VALUE-CONSTRAINTS, before durability, so a violation
aborts before anything is journaled (GH #155)."
  (when (%registered-cardinality-specs graph)
    (let ((view (make-commit-view graph tx)))
      (dolist (v (%vertices-to-check tx graph view))
        (let ((violation (first (%cardinality-violations v graph view))))
          (when violation
            (%signal-cd-violation violation)))))))

(defun check-cardinality-constraints (graph &key vertex-type)
  "Survey live vertices of GRAPH and COLLECT cardinality violations
without signalling.  Returns (values VIOLATIONS CHECKED-COUNT SPEC-COUNT)
-- read the counts: zero violations over zero specs is an unchecked
graph, not a clean one (as CHECK-VALUE-CONSTRAINTS).  A store-only view:
every count is the adjacency index as it stands."
  (let ((violations '())
        (checked 0)
        (view (make-commit-view graph)))
    (map-vertices (lambda (v)
                    (incf checked)
                    (let ((vs (%cardinality-violations v graph view)))
                      (when vs
                        (setf violations (nconc violations vs)))))
                  graph :vertex-type vertex-type)
    (values violations checked
            (length (%registered-cardinality-specs graph)))))
