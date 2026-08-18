;;;; Declarative value constraints: a slot's value drawn from a closed
;;;; enumeration and/or required to be present, enforced at COMMIT.
;;;;
;;;; The third schema registry, beside *SCHEMA-INDEX-METADATA* (index.lisp)
;;;; and *SCHEMA-UNIQUE-METADATA* (unique-constraint.lisp), sharing their
;;;; identity rule (%SPEC-IDENTITY, GH #139/#140).
;;;;
;;;; Unlike DEF-UNIQUE there is NO INDEX: a value constraint is a predicate
;;;; over one node's own slot.  No cross-node lookup, no rebuild-on-open, no
;;;; sidecar.  Design:
;;;; docs/superpowers/specs/2026-08-17-value-constraints-design.md (GH #149).

(in-package :graph-db)

(define-condition value-constraint-violation (error)
  ((class-name :initarg :class-name :reader vcv-class-name)
   (slot-name  :initarg :slot-name  :reader vcv-slot-name)
   (value      :initarg :value      :reader vcv-value)
   (expected   :initarg :expected   :reader vcv-expected)
   (reason     :initarg :reason     :reader vcv-reason)
   (node-id    :initarg :node-id    :reader vcv-node-id))
  (:report
   (lambda (c s)
     (if (eq (vcv-reason c) :missing)
         (format s "Value constraint on ~S.~S violated by node ~A: the ~
                    slot is required but holds NIL."
                 (vcv-class-name c) (vcv-slot-name c)
                 (string-id (vcv-node-id c)))
         (format s "Value constraint on ~S.~S violated by node ~A: ~
                    expected one of~{ ~S~}; got ~S."
                 (vcv-class-name c) (vcv-slot-name c)
                 (string-id (vcv-node-id c))
                 (vcv-expected c) (vcv-value c))))))

(defvar *schema-value-constraint-metadata* (make-hash-table)
  "graph-name (symbol) -> list of VALUE-CONSTRAINT-SPECs (newest first).")

(defstruct (value-constraint-spec
            (:constructor make-value-constraint-spec))
  owner-name slot-name graph-name one-of required name)

(defun value-constraint-spec-identity (spec)
  "See %SPEC-IDENTITY (index.lisp).  All three registries share ONE identity
rule on purpose (GH #140)."
  (%spec-identity (value-constraint-spec-owner-name spec)
                  (list (value-constraint-spec-slot-name spec))
                  (value-constraint-spec-name spec)))

(defun register-value-constraint-spec (spec)
  "Record SPEC, REPLACING any spec of the same identity in place.  Replacing
rather than pushing is what stops the table growing one entry per evaluation
(GH #139).  Signals when SPEC constrains nothing."
  (let ((one-of (value-constraint-spec-one-of spec)))
    (when (and (null one-of) (not (value-constraint-spec-required spec)))
      (error "Value constraint on ~S.~S declares neither :ONE-OF nor ~
              :REQUIRED, so it constrains nothing."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec)))
    (when (and one-of (not (and (listp one-of) (null (cdr (last one-of))))))
      (error "Value constraint on ~S.~S has a :ONE-OF that is not a proper ~
              list: ~S."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec) one-of)))
  (let* ((g (value-constraint-spec-graph-name spec))
         (id (value-constraint-spec-identity spec))
         (existing (gethash g *schema-value-constraint-metadata*))
         (hit (find id existing :key #'value-constraint-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-value-constraint-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-value-constraint-spec (owner-name graph-name
                                         &key slot name)
  "Withdraw the declaration identified by (OWNER . NAME) or (OWNER . (SLOT)).
Returns T if one was withdrawn; a no-op otherwise.  Nothing is rebuilt or
released -- there is no index behind a value constraint."
  (let* ((id (%spec-identity owner-name (when slot (list slot)) name))
         (existing (gethash graph-name *schema-value-constraint-metadata*))
         (hit (find id existing :key #'value-constraint-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-value-constraint-metadata*)
            (remove hit existing))
      t)))

(defun %registered-value-constraint-specs (graph)
  (gethash (graph-name graph) *schema-value-constraint-metadata*))

(defun class-value-constraint-specs (class graph)
  "Specs from the registry applying to CLASS: owner is CLASS or an ancestor
(subtype IS-A) and the named slot exists on CLASS.  Mirrors
CLASS-UNIQUE-TUPLE-SPECS (unique-constraint.lisp)."
  (when (class-finalized-p class)
    (loop for spec in (%registered-value-constraint-specs graph)
          for owner = (value-constraint-spec-owner-name spec)
          for slot = (value-constraint-spec-slot-name spec)
          when (and (subtypep (class-name class) owner)
                    (find slot (class-slots class)
                          :key #'slot-definition-name))
          collect spec)))

(defstruct (vc-violation (:constructor %make-vc-violation))
  spec node-id class-name slot actual expected reason)

(defun %value-constraint-violations (node graph)
  "Every value constraint NODE violates, as VC-VIOLATION records.  The one
evaluator behind both consumers: the write path signals on the first, the
audit pass collects them all (design, \"Violation shape\").

EQUAL, not EQL, so a non-keyword enumeration works."
  (let ((class (class-of node)))
    (loop for spec in (class-value-constraint-specs class graph)
          for slot = (value-constraint-spec-slot-name spec)
          for one-of = (value-constraint-spec-one-of spec)
          for val = (slot-value node slot)
          append
          (cond
            ((null val)
             (when (value-constraint-spec-required spec)
               (list (%make-vc-violation
                      :spec spec :node-id (id node)
                      :class-name (class-name class) :slot slot
                      :actual nil :expected one-of :reason :missing))))
            ((and one-of (not (member val one-of :test #'equal)))
             (list (%make-vc-violation
                    :spec spec :node-id (id node)
                    :class-name (class-name class) :slot slot
                    :actual val :expected one-of
                    :reason :not-in-vocabulary)))
            (t nil)))))

(defmacro def-value-constraint (owner-class slot graph-name
                                &key one-of required name)
  "Declare that OWNER-CLASS's SLOT in GRAPH-NAME draws its value from the
closed enumeration :ONE-OF, and with :REQUIRED is never NIL.  Enforced at
commit (VALIDATE-VALUE-CONSTRAINTS), never merely indexed.

Without :REQUIRED, NIL is EXEMPT -- \"if present, it must be one of these\" --
matching DEF-UNIQUE's null rule.  Diverging from that would be the trap
GH #107 named: two neighbouring macros disagreeing about nulls.

⚠ :ONE-OF is EVALUATED, unlike SLOT and :NAME.  That is what lets a caller
name an existing vocabulary constant rather than duplicate it.  The value is
captured at registration, so editing the constant does not retroactively
change a registered constraint -- re-evaluate this form (idempotent, GH #139)."
  `(register-value-constraint-spec
    (make-value-constraint-spec
     :owner-name ',owner-class
     :slot-name ',slot
     :graph-name ',graph-name
     :one-of ,one-of
     :required ,required
     :name ',name)))

(defmacro undef-value-constraint (owner-class graph-name &key slot name)
  "Withdraw a DEF-VALUE-CONSTRAINT declaration, by :NAME or by :SLOT.
Keyword rather than positional for the same reason as UNDEF-INDEX: a graph
name is itself a keyword."
  `(unregister-value-constraint-spec ',owner-class ',graph-name
                                     :slot ',slot :name ',name))
