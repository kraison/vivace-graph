;;;; The ontology evaluator entry point (GH #301): validate proposed
;;;; writes WITHOUT committing, one report.  A second entry to the same
;;;; collectors %COMMIT runs, so the two cannot disagree; the spacetime
;;;; families ride *COMMIT-VALIDATORS*, the same seam %COMMIT uses.
;;;; Design: docs/superpowers/specs/2026-08-31-ontology-evaluator-design.md.

(in-package :graph-db)

(defclass %proposed-tx ()
  ((writes :initarg :writes :reader writes))
  (:documentation "A write set with no transaction around it: just enough
tx for MAKE-COMMIT-VIEW and the validators (GH #301)."))

(defstruct (validation-report (:constructor %make-validation-report))
  "What VALIDATE-WRITES found.  VIOLATIONS is a list of (FAMILY WRITE
DETAIL): FAMILY a keyword, WRITE the TX-WRITE refused, DETAIL the
family's own violation struct or condition (vc-violation, cd-violation,
dr-violation, unique-constraint-violation, or a subsystem's
constraint-violation), carried, not replaced.  FAMILY-COUNTS counts
violations per family.  CHECKED-COUNTS counts writes each family
examined.  SPEC-COUNTS counts the specs registered per family --
distinguishing \"no violations, N specs checked\" from \"nothing to
check\".  The :unique, :membership-disjointness and :extent-disjointness
families report at most ONE violation each (their validators signal the
first), which still covers everything %COMMIT itself would have said:
%COMMIT stops at its first refusal."
  violations family-counts checked-counts spec-counts)

(defun %normalize-proposed-write (spec)
  "SPEC as a TX-WRITE: a TX-WRITE passes through; (:create NODE),
(:update NODE OLD-NODE) and (:delete NODE OLD-NODE) build one.  A delete
marks its node deleted the way MARK-DELETED does, on a copy-free
already-proposed node."
  (etypecase spec
    (tx-write spec)
    (cons
     (destructuring-bind (kind node &optional old-node) spec
       (ecase kind
         (:create (make-instance 'tx-create :node node))
         (:update (make-instance 'tx-update :node node
                                            :old-node old-node))
         (:delete (make-instance 'tx-delete :node node
                                            :old-node old-node)))))))

(defun %evaluator-spec-counts (graph)
  (let ((gname (graph-name graph)))
    (list (cons :value
                (length (gethash gname *schema-value-constraint-metadata*)))
          ;; :UNIQUE has two declaration forms: DEF-UNIQUE tuples in the
          ;; registry, and the :unique slot option living on the class.
          (cons :unique
                (+ (length (gethash gname *schema-unique-metadata*))
                   (loop for nt in (all-node-types graph)
                         for name = (if (node-type-p nt)
                                        (node-type-name nt)
                                        nt)
                         for c = (and name
                                      (ignore-errors
                                       (find-class name nil)))
                         when (and c (class-finalized-p c))
                           sum (length (class-unique-slots c)))))
          (cons :cardinality
                (length (gethash gname *schema-cardinality-metadata*)))
          (cons :domain-range
                (length (gethash gname *schema-domain-range-metadata*)))
          (cons :subsystem (length *commit-validators*)))))

(defun validate-writes (graph proposed-writes)
  "Validate PROPOSED-WRITES against GRAPH as %COMMIT would, WITHOUT
committing: pure reads over the commit view, no journaling, no side
effect.  Returns a VALIDATION-REPORT.

PROPOSED-WRITES is a list of TX-WRITE objects, or of specs -- (:create
NODE), (:update NODE OLD-NODE), (:delete NODE OLD-NODE) -- in the same
form a transaction's write set holds (new/updated/deleted nodes).

Runs the six constraint families of the #109 evaluator note: value,
unique, cardinality, domain/range, plus every *COMMIT-VALIDATORS* entry
(membership and extent disjointness when graph-db/spacetime is loaded).
The vector-segment dimension check stays commit-only: it grows segments,
which is a side effect this function promises not to have (GH #301).

NOT run under the transaction-manager lock: the answer is advisory and
can be stale by the time a real commit runs -- the commit path remains
the enforcement (GH #301)."
  (let* ((writes (mapcar #'%normalize-proposed-write proposed-writes))
         (ptx (make-instance '%proposed-tx :writes writes))
         (view (make-commit-view graph ptx))
         (violations '())
         (checked '()))
    (flet ((note (family write detail)
             (push (list family write detail) violations))
           (checked+ (family n)
             (incf (cdr (or (assoc family checked)
                            (first (push (cons family 0) checked))))
                   n)))
      ;; Collector families: every violation, per write.
      (dolist (w writes)
        (let ((node (node w)))
          (unless (deleted-p node)
            (checked+ :value 1)
            (dolist (v (%value-constraint-violations node graph view))
              (note :value w v))
            (when (typep node 'vertex)
              (checked+ :cardinality 1)
              (dolist (v (%cardinality-violations node graph view))
                (note :cardinality w v)))
            (when (typep node 'edge)
              (checked+ :domain-range 1)
              (dolist (v (%domain-range-violations node graph view))
                (note :domain-range w v))))))
      ;; Signalling families: first violation each, as %COMMIT reports.
      (handler-case (progn (validate-unique-constraints ptx graph)
                           (checked+ :unique (length writes)))
        (constraint-violation (c)
          (checked+ :unique (length writes))
          (note :unique nil c)))
      (dolist (fn *commit-validators*)
        (handler-case (progn (funcall fn ptx graph)
                             (checked+ :subsystem (length writes)))
          (constraint-violation (c)
            (checked+ :subsystem (length writes))
            (note :subsystem nil c))))
      (let ((counts '()))
        (dolist (v violations)
          (incf (cdr (or (assoc (first v) counts)
                         (first (push (cons (first v) 0) counts))))))
        (%make-validation-report
         :violations (nreverse violations)
         :family-counts counts
         :checked-counts checked
         :spec-counts (%evaluator-spec-counts graph))))))

(defun validate-transaction (graph &optional (tx *transaction*))
  "VALIDATE-WRITES over TX's write set -- the open transaction by
default -- so a consumer that stages writes and asks before committing
need not reach for the write set itself (GH #320).  Signals when no
transaction is open: an empty set would validate clean and mislead."
  (unless tx
    (error "VALIDATE-TRANSACTION: no transaction is open."))
  (validate-writes graph (writes tx)))
