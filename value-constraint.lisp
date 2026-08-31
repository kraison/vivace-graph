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
;;;;
;;;; Families that need more than the node's own state -- the pre-image
;;;; (:TRANSITION, GH #158), the delta, the other endpoint -- read it
;;;; through one COMMIT-VIEW, built once per commit and store-only for the
;;;; audit pass.  Design:
;;;; docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md (#109).

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
     (case (vcv-reason c)
       (:missing
        (format s "Value constraint on ~S.~S violated by node ~A: the ~
                   slot is required but holds NIL."
                (vcv-class-name c) (vcv-slot-name c)
                (string-id (vcv-node-id c))))
       ;; The :CHECK slot option (GH #172, R5): EXPECTED is the name of
       ;; the schema function that rejected the value.
       (:check-failed
        (format s "Value constraint on ~S.~S violated by node ~A: ~S ~
                   rejected ~S."
                (vcv-class-name c) (vcv-slot-name c)
                (string-id (vcv-node-id c))
                (vcv-expected c) (vcv-value c)))
       ;; :TRANSITION / :WRITE-ONCE (GH #158): EXPECTED is the transition
       ;; that refused the change to VALUE.
       (:transition-refused
        (format s "Value constraint on ~S.~S violated by node ~A: ~
                   transition ~S refused the change to ~S."
                (vcv-class-name c) (vcv-slot-name c)
                (string-id (vcv-node-id c))
                (vcv-expected c) (vcv-value c)))
       (t
        (format s "Value constraint on ~S.~S violated by node ~A: ~
                   expected one of~{ ~S~}; got ~S."
                (vcv-class-name c) (vcv-slot-name c)
                (string-id (vcv-node-id c))
                (vcv-expected c) (vcv-value c)))))))

(defvar *schema-value-constraint-metadata* (make-hash-table)
  "graph-name (symbol) -> list of VALUE-CONSTRAINT-SPECs (newest first).")

(defstruct (value-constraint-spec
            (:constructor make-value-constraint-spec))
  owner-name slot-name graph-name one-of required name
  ;; :WRITE-ONCE for the built-in transition, else the NAME of a schema
  ;; function of (OLD NEW), or NIL (GH #158).
  transition)

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
  (let ((one-of (value-constraint-spec-one-of spec))
        (transition (value-constraint-spec-transition spec)))
    (when (and (null one-of) (not (value-constraint-spec-required spec))
               (null transition))
      (error "Value constraint on ~S.~S declares none of :ONE-OF, ~
              :REQUIRED, :WRITE-ONCE or :TRANSITION, so it constrains ~
              nothing."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec)))
    (unless (symbolp transition)
      (error "Value constraint on ~S.~S has a :TRANSITION that is not the ~
              NAME of a schema function: ~S."
             (value-constraint-spec-owner-name spec)
             (value-constraint-spec-slot-name spec) transition))
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

;;; --- The commit view (evaluator design note, #109; GH #158) --------------

(defstruct (commit-view (:constructor %make-commit-view (graph writes)))
  "Post-commit state as a value: GRAPH overlaid with a transaction's
WRITES -- writes win, deleted is absent.  WRITES is id -> TX-WRITE, or
NIL for the STORE-ONLY view the audit pass uses, in which nothing is
changing.  One view serves every constraint family, so the write path and
the batch pass run the same evaluator (#109 evaluator note §2)."
  graph writes)

(defun make-commit-view (graph &optional tx)
  "The view of GRAPH as TX will leave it; store-only when TX is NIL."
  (%make-commit-view
   graph
   (when tx
     (let ((h (make-hash-table :test 'equalp)))
       (dolist (w (writes tx) h)
         (setf (gethash (id w) h) w))))))

(defun view-writes (view)
  "The delta: this transaction's TX-WRITEs, or NIL in a store-only view."
  (let ((h (commit-view-writes view)))
    (when h (alexandria:hash-table-values h))))

(defun view-old-node (view node)
  "NODE's committed pre-image under VIEW: the OLD-NODE of its update or
delete write, NIL for a create, and NODE itself when nothing is changing
it -- a store-only view, or a node this transaction does not write.  The
pre-image is already in the write record (TX-UPDATE, transactions.lisp);
this is where the evaluator is finally handed it (GH #158)."
  (let ((h (commit-view-writes view)))
    (if (null h)
        node
        (let ((w (gethash (id node) h)))
          (cond ((null w) node)
                ((typep w 'tx-update) (old-node w))
                (t nil))))))

(defun %same-stored-value-p (old new)
  "True when OLD and NEW are the same STORED value: EQUAL, or serialising
to the same octets.  EQUAL alone is not enough -- a slot holding objects
(the timestamps inside a temporal-extent sexp, say) deserialises to fresh
instances on every read, so a pre-image and a copy are never EQUAL even
when nothing changed, and an unrelated update would read as a revision
(GH #158).  SERIALIZE is the engine's own definition of \"the same value\"."
  (or (equal old new)
      (ignore-errors (equalp (serialize old) (serialize new)))))

(defun %transition-allowed-p (transition old new)
  "True when TRANSITION admits the change OLD -> NEW.  No change is always
allowed; :WRITE-ONCE admits only a first write; a named function decides
the rest.  Resolved at check time, as :CHECK is (GH #172)."
  (cond ((%same-stored-value-p old new) t)
        ((eq transition :write-once) (null old))
        (t (funcall (%resolve-schema-function transition) old new))))

(defun %check-slot-violations (node class)
  "Every :CHECK slot of CLASS whose registered function rejects NODE's
value, as VC-VIOLATION records.  NULL is exempt, exactly as it is for
:ONE-OF -- \"if present, it must satisfy this\".  The name is resolved
here, at check time, so a re-registration takes effect immediately;
presence was verified at definition and at materialize time
(GH #172, R5)."
  (loop for (slot . fn-name) in (node-check-slots class)
        for val = (slot-value node slot)
        unless (or (null val)
                   (funcall (%resolve-schema-function fn-name) val))
        collect (%make-vc-violation
                 :spec nil :node-id (id node)
                 :class-name (class-name class) :slot slot
                 :actual val :expected fn-name
                 :reason :check-failed)))

(defun %value-constraint-violations (node graph
                                     &optional (view (make-commit-view graph)))
  "Every value constraint NODE violates, as VC-VIOLATION records.  The one
evaluator behind both consumers: the write path signals on the first, the
audit pass collects them all (design, \"Violation shape\").

EQUAL, not EQL, so a non-keyword enumeration works."
  (let ((class (class-of node)))
    (loop for spec in (class-value-constraint-specs class graph)
          for slot = (value-constraint-spec-slot-name spec)
          for one-of = (value-constraint-spec-one-of spec)
          for transition = (value-constraint-spec-transition spec)
          for val = (slot-value node slot)
          ;; The pre-image, only when a spec asks: a create has none and
          ;; is never a transition violation (GH #158).
          for old-node = (and transition (view-old-node view node))
          append
          (nconc
           (when (and old-node
                      (not (%transition-allowed-p
                            transition (slot-value old-node slot) val)))
             (list (%make-vc-violation
                    :spec spec :node-id (id node)
                    :class-name (class-name class) :slot slot
                    :actual val :expected transition
                    :reason :transition-refused)))
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
             (t nil)))
          into declared
          finally
             (return (nconc declared
                            (when *schema-check-slots-present-p*
                              (%check-slot-violations node class)))))))

(defmacro def-value-constraint (owner-class slot graph-name
                                &key one-of required name
                                     write-once transition)
  "Declare that OWNER-CLASS's SLOT in GRAPH-NAME draws its value from the
closed enumeration :ONE-OF, and with :REQUIRED is never NIL.  Enforced at
commit (VALIDATE-VALUE-CONSTRAINTS), never merely indexed.

:WRITE-ONCE: the slot is settable once -- at creation, or later while it
is still NIL -- and never revised after; clearing it is a revision.
:TRANSITION names a schema function (REGISTER-SCHEMA-FUNCTION) of
(OLD NEW) that decides whether a change is legal; :WRITE-ONCE is its
degenerate case, (NULL OLD).  A create is never a transition violation,
and an unchanged value is always allowed.  ⚠ Transitions cannot be
audited after the fact -- CHECK-VALUE-CONSTRAINTS counts them and says so
(GH #158; evaluator design note §3).

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
     :name ',name
     :transition ,(cond ((and write-once transition)
                         (error "DEF-VALUE-CONSTRAINT ~S.~S: :WRITE-ONCE ~
                                 and :TRANSITION are exclusive."
                                owner-class slot))
                        (write-once :write-once)
                        (t `',transition)))))

(defmacro undef-value-constraint (owner-class graph-name &key slot name)
  "Withdraw a DEF-VALUE-CONSTRAINT declaration, by :NAME or by :SLOT.
Keyword rather than positional for the same reason as UNDEF-INDEX: a graph
name is itself a keyword."
  `(unregister-value-constraint-spec ',owner-class ',graph-name
                                     :slot ',slot :name ',name))

(defun validate-value-constraints (tx graph)
  "Signal VALUE-CONSTRAINT-VIOLATION if any write in TX violates a declared
value constraint.  Called in %COMMIT's manager-locked region, after VALIDATE
and before durability, so a violation aborts before anything is journaled
(GH #149)."
  (when (or (%registered-value-constraint-specs graph)
            ;; The :CHECK slot option needs no registry entry -- it
            ;; lives on the class (GH #172, R5).
            *schema-check-slots-present-p*)
    (let ((view (make-commit-view graph tx)))
      (dolist (write (writes tx))
        (let ((node (node write)))
          (unless (deleted-p node)      ; a delete claims nothing
            (let ((v (first (%value-constraint-violations node graph
                                                          view))))
            (when v
              (error 'value-constraint-violation
                     :class-name (vc-violation-class-name v)
                     :slot-name (vc-violation-slot v)
                     :value (vc-violation-actual v)
                     :expected (vc-violation-expected v)
                     :reason (vc-violation-reason v)
                     :node-id (vc-violation-node-id v))))))))))

(defun check-value-constraints (graph &key vertex-type)
  "Survey live vertices of GRAPH and COLLECT violations without signalling.
Returns (values VIOLATIONS CHECKED-COUNT SPEC-COUNT UNAUDITED-COUNT).

⚠ UNAUDITED-COUNT is how many of SPEC-COUNT are :TRANSITION / :WRITE-ONCE
specs (GH #158).  A transition is a fact about a CHANGE, and there is no
pre-image after the fact, so this pass cannot check one: a store-only
view sees every node as unchanged.  Those specs are counted here and
never in VIOLATIONS -- zero violations over a schema that is all
transitions is not a clean graph, and a caller must read this value
before saying so.

⚠ The two counts are part of the answer, not diagnostics.  Zero violations
over zero specs is an UNCHECKED graph, not a clean one; a caller that prints
\"OK\" without reading them is reporting a count with no population.

:VERTEX-TYPE narrows the scan and keeps it snapshot-consistent; the untyped
scan reads live node versions and bypasses MVCC (see MAP-VERTICES), so it is
for admin passes over a quiescent graph.

⚠ SPEC-COUNT counts DEF-VALUE-CONSTRAINT declarations only.  A :CHECK
slot option (GH #172) lives on the class, not in this registry, so a
schema constrained entirely by :CHECK is audited normally and still
reports SPECS 0 -- do not read that as "unchecked" here.

⚠ SPEC-COUNT counts declarations registered on GRAPH, not declarations
that apply to what was scanned -- with a spec on class B only, scanning
:VERTEX-TYPE 'A returns SPECS > 0 while none of them touched a scanned
node.  The scan is also vertices-only, so a spec on an edge class inflates
SPECS without ever being audited."
  (let ((violations '())
        (checked 0))
    (map-vertices (lambda (v)
                    (incf checked)
                    (let ((vs (%value-constraint-violations v graph)))
                      (when vs
                        (setf violations (nconc violations vs)))))
                  graph :vertex-type vertex-type)
    (let ((specs (%registered-value-constraint-specs graph)))
      (values violations checked
              (length specs)
              (count-if #'value-constraint-spec-transition specs)))))
