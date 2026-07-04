(in-package :graph-db)

;;;; Unique constraints (:UNIQUE slot option) -- issue #6.
;;;; See docs/unique-constraint-design.md for the full design.
;;;;
;;;; A :UNIQUE slot is backed by a per-(owner-class, slot) index mapping a canonical
;;;; key -> node-id.  Enforcement is a COMMIT-BOUNDARY constraint, split across the
;;;; durability line inside %COMMIT's single transaction-manager lock:
;;;;
;;;;   * CHECK  -- VALIDATE-UNIQUE-CONSTRAINTS, a lookup run right after VALIDATE and
;;;;               before FINALIZE-TX-PERSISTENCE.  Pre-durability, so a hit aborts
;;;;               cleanly (nothing is journaled).  This is the enforcement.
;;;;   * MAINTAIN -- APPLY-TX-WRITES-TO-UNIQUE-INDEXES, run in APPLY-TRANSACTION
;;;;               alongside the view/spatial passes.  Post-durability, so it is
;;;;               derived from the journal and crash-consistent, like the other
;;;;               indexes.
;;;;
;;;; Both run under the same manager lock, so check+insert is atomic across commits
;;;; -- no phantom, and OCC's id-based read/write sets (which cannot see two
;;;; different-id nodes claiming the same value) are not relied on.  This is why a
;;;; *view* cannot enforce uniqueness (view maintenance is post-durability); #6
;;;; sidesteps #7 by checking earlier in the pipeline.
;;;;
;;;; v1: the index is in-RAM and rebuilt on open (REBUILD-UNIQUE-INDEXES).
;;;; Persistence is the immediate follow-up -- it removes the open-time scan and,
;;;; on a lazy memory-graph, the materialization that scan forces.

(define-condition unique-constraint-violation (error)
  ((class-name :initarg :class-name :reader ucv-class-name)
   (slot-name  :initarg :slot-name  :reader ucv-slot-name)
   (value      :initarg :value      :reader ucv-value)
   (existing-id :initarg :existing-id :reader ucv-existing-id))
  (:report (lambda (c s)
             (format s "Unique constraint on ~S.~S violated: value ~S is already held by node ~A."
                     (ucv-class-name c) (ucv-slot-name c) (ucv-value c)
                     (string-id (ucv-existing-id c))))))

;;; ---------------------------------------------------------------------------
;;; Spec resolution: :UNIQUE <spec> -> (values TEST CANONICALIZER)
;;; ---------------------------------------------------------------------------

(defun %resolve-unique-canonicalizer (spec)
  "Resolve a :UNIQUE SPEC to (values TEST CANONICALIZER).  TEST is the hash-table
test for the index (EQUAL or EQUALP -- the only tests portable across SBCL/CCL/ECL).
CANONICALIZER is a 1-arg function applied to the slot value before keying, or NIL
for identity.  T/EQUAL -> (EQUAL nil); EQUALP -> (EQUALP nil); a symbol / #'fn /
lambda form -> (EQUAL that-function)."
  (cond
    ((or (eq spec t) (eq spec 'equal))  (values 'equal nil))
    ((eq spec 'equalp)                  (values 'equalp nil))
    ((functionp spec)                   (values 'equal spec))
    ((and (consp spec) (eq (car spec) 'function)) (values 'equal (fdefinition (cadr spec))))
    ((and (consp spec) (eq (car spec) 'lambda))   (values 'equal (coerce spec 'function)))
    ((symbolp spec)                     (values 'equal (fdefinition spec)))
    (t (error "Invalid :UNIQUE spec ~S" spec))))

;;; ---------------------------------------------------------------------------
;;; Per-class descriptors (MOP introspection)
;;; ---------------------------------------------------------------------------

(defun %unique-slot-owner-name (class slot-name)
  "The most-general node-class in CLASS's precedence list that declares SLOT-NAME as
a :UNIQUE direct slot -- the cross-subtype index owner (so a :UNIQUE slot on a parent
enforces across its subclasses through one shared index)."
  (let ((owner (loop for c in (reverse (class-precedence-list class))
                     when (and (typep c 'node-class)
                               (find-if (lambda (ds)
                                          (and (eq (slot-definition-name ds) slot-name)
                                               (unique-spec ds)))
                                        (class-direct-slots c)))
                     return c)))
    (class-name (or owner class))))

(defun class-unique-slots (class)
  "List of (SLOT-NAME OWNER-NAME SPEC SCOPE) for CLASS's :UNIQUE effective slots.
NIL for a class with none (the common case).  SPEC is the raw :UNIQUE value; the
test + canonicalizer are resolved lazily when the index is created."
  (when (class-finalized-p class)
    (loop for s in (class-slots class)
          for spec = (unique-spec s)
          when spec
          collect (list (slot-definition-name s)
                        (%unique-slot-owner-name class (slot-definition-name s))
                        spec (unique-scope s)))))

;;; ---------------------------------------------------------------------------
;;; The index + the graph's registry
;;; ---------------------------------------------------------------------------

(defstruct (unique-index (:constructor %make-unique-index))
  owner-name slot-name spec test canonicalizer scope table)

(defun %node-origin (node graph)
  "The origin an :ORIGIN-scoped key is partitioned by.  v1: the graph's own
origin-id (a constant within one graph, so :ORIGIN degenerates to :LOCAL for a
single graph; full per-node origin is the peer-model refinement)."
  (declare (ignore node))
  (or (ignore-errors (origin-id graph)) :graph))

(defun %unique-key (uix value node graph)
  "The canonical key VALUE maps to in UIX (canonicalizer + scope applied), or NIL for
a NULL/unbound value (which is exempt from the constraint, SQL-style)."
  (when value
    (let ((k (let ((c (unique-index-canonicalizer uix)))
               (if c (funcall c value) value))))
      (if (eq (unique-index-scope uix) :origin)
          (list (%node-origin node graph) k)
          k))))

(defun %unique-index-for (graph descriptor)
  "Get-or-create the (empty) UNIQUE-INDEX for DESCRIPTOR = (slot owner spec scope),
keyed by (owner . slot) in GRAPH's registry.  Resolves the test + canonicalizer
from SPEC on creation."
  (destructuring-bind (slot-name owner-name spec scope) descriptor
    (let* ((reg (or (unique-indexes graph)
                    (setf (unique-indexes graph)
                          (make-hash-table :test 'equal
                                           #+sbcl :synchronized #+sbcl t
                                           #+ccl :shared #+ccl t))))
           (key (cons owner-name slot-name)))
      (or (gethash key reg)
          (multiple-value-bind (test canon) (%resolve-unique-canonicalizer spec)
            (setf (gethash key reg)
                  (%make-unique-index
                   :owner-name owner-name :slot-name slot-name :spec spec :test test
                   :canonicalizer canon :scope scope
                   :table (make-hash-table :test test
                                           #+sbcl :synchronized #+sbcl t
                                           #+ccl :shared #+ccl t))))))))

;;; ---------------------------------------------------------------------------
;;; Maintenance (APPLY, post-durability, journal-replayable)
;;; ---------------------------------------------------------------------------

(defun %uix-claim (node graph)
  "Claim NODE's unique keys (create / new value of an update)."
  (dolist (d (class-unique-slots (class-of node)))
    (let* ((uix (%unique-index-for graph d))
           (key (%unique-key uix (slot-value node (first d)) node graph)))
      (when key (setf (gethash key (unique-index-table uix)) (id node))))))

(defun %uix-release (node graph)
  "Release NODE's unique keys (delete / old value of an update).  Guarded so it
never removes another node's claim."
  (dolist (d (class-unique-slots (class-of node)))
    (let* ((uix (%unique-index-for graph d))
           (key (%unique-key uix (slot-value node (first d)) node graph)))
      (when (and key (equalp (gethash key (unique-index-table uix)) (id node)))
        (remhash key (unique-index-table uix))))))

(defgeneric apply-tx-write-to-unique-indexes (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-unique-indexes ((write tx-create) graph)
  (%uix-claim (node write) graph))

(defmethod apply-tx-write-to-unique-indexes ((write tx-update) graph)
  (%uix-release (old-node write) graph)
  (unless (deleted-p (node write))
    (%uix-claim (node write) graph)))

;; tx-delete is a tx-update subclass; the node is marked deleted -> release only.
(defmethod apply-tx-write-to-unique-indexes ((write tx-delete) graph)
  (%uix-release (old-node write) graph)
  (%uix-release (node write) graph))

(defun apply-tx-writes-to-unique-indexes (writes graph)
  (dolist (write writes) (apply-tx-write-to-unique-indexes write graph)))

;;; ---------------------------------------------------------------------------
;;; Enforcement (VALIDATE, pre-durability)
;;; ---------------------------------------------------------------------------

(defun validate-unique-constraints (tx graph)
  "Signal UNIQUE-CONSTRAINT-VIOLATION if any write in TX would duplicate another live
node's unique value, or duplicate another write in the same transaction.  Called in
%COMMIT's manager-locked region, after VALIDATE and before durability, so a violation
aborts before anything is journaled."
  (let ((intra (make-hash-table :test 'equal)))
    (dolist (write (writes tx))
      (let ((node (node write)))
        (unless (deleted-p node)              ; a delete/mark-deleted claims nothing
          (dolist (d (class-unique-slots (class-of node)))
            (let* ((uix (%unique-index-for graph d))
                   (val (slot-value node (first d)))
                   (key (%unique-key uix val node graph)))
              (when key
                ;; (a) against already-committed nodes (the index reflects them, since
                ;; prior commits' APPLY ran under this same lock)
                (let ((holder (gethash key (unique-index-table uix))))
                  (when (and holder (not (equalp holder (id node))))
                    (error 'unique-constraint-violation
                           :class-name (unique-index-owner-name uix)
                           :slot-name (unique-index-slot-name uix)
                           :value val :existing-id holder)))
                ;; (b) against this transaction's own other writes
                (let* ((ik (list (unique-index-owner-name uix)
                                 (unique-index-slot-name uix) key))
                       (claimant (gethash ik intra)))
                  (when (and claimant (not (equalp claimant (id node))))
                    (error 'unique-constraint-violation
                           :class-name (unique-index-owner-name uix)
                           :slot-name (unique-index-slot-name uix)
                           :value val :existing-id claimant))
                  (setf (gethash ik intra) (id node)))))))))))

;;; ---------------------------------------------------------------------------
;;; Rebuild on open (v1)
;;; ---------------------------------------------------------------------------

(defun %graph-has-unique-slots-p (graph)
  "Cheap guard so a graph with no :UNIQUE slots pays nothing at open."
  (dolist (nt (all-node-types graph) nil)
    (let* ((name (if (node-type-p nt) (node-type-name nt) nt))
           (c (and name (ignore-errors (find-class name nil)))))
      (when (and c (class-finalized-p c) (class-unique-slots c))
        (return t)))))

(defun rebuild-unique-indexes (graph)
  "v1 rebuild-on-open: (re)populate the unique indexes by scanning live nodes once.
Runs off the commit path (at open), so no lock contention.  NOTE: on a lazy
memory-graph this materializes the scanned nodes; persistence (v1.1) removes it."
  (when (%graph-has-unique-slots-p graph)
    (let ((*graph* graph))
      (flet ((index-node (node)
               (unless (deleted-p node)
                 (dolist (d (class-unique-slots (class-of node)))
                   (let* ((uix (%unique-index-for graph d))
                          (key (%unique-key uix (slot-value node (first d)) node graph)))
                     (when key
                       (let ((existing (gethash key (unique-index-table uix))))
                         (if (and existing (not (equalp existing (id node))))
                             (log:warn "unique-index ~S.~S: pre-existing duplicate key ~S (~A / ~A); keeping first"
                                       (unique-index-owner-name uix) (unique-index-slot-name uix)
                                       key (string-id existing) (string-id (id node)))
                             (setf (gethash key (unique-index-table uix)) (id node))))))))))
        (map-vertices #'index-node graph)
        (map-edges #'index-node graph)))))

;;; ---------------------------------------------------------------------------
;;; Durable persistence -- MEMORY backend (rides the #50 checkpoint image, so no
;;; open-time scan and no lazy-materialization).  On-disk persistence (an
;;; incremental mmap skip-list) is the follow-up; on-disk still rebuilds on open.
;;; ---------------------------------------------------------------------------

;; *MEMORY-IMAGE-UNIQUE-LOADED* is defined in memory-graph.lisp (bound by
;; OPEN-MEMORY-GRAPH, which loads before this file).

(defun %dump-unique-indexes (graph)
  "Self-contained snapshot of GRAPH's unique indexes for the checkpoint image:
a list of (owner slot spec scope ((canonical-key id) ...)).  Proper-list pairs so
the image codec's tagged value writer handles the byte-array ids."
  (let ((acc '()))
    (when (unique-indexes graph)
      (maphash (lambda (k uix)
                 (declare (ignore k))
                 (let ((pairs '()))
                   (maphash (lambda (key id) (push (list key id) pairs))
                            (unique-index-table uix))
                   (push (list (unique-index-owner-name uix) (unique-index-slot-name uix)
                               (unique-index-spec uix) (unique-index-scope uix) pairs)
                         acc)))
               (unique-indexes graph)))
    acc))

(defun %load-unique-indexes (graph dump)
  "Restore GRAPH's unique indexes from a %DUMP-UNIQUE-INDEXES snapshot -- no node
scan.  Sets *MEMORY-IMAGE-UNIQUE-LOADED* so OPEN skips the rebuild."
  (dolist (u dump)
    (destructuring-bind (owner slot spec scope pairs) u
      (let ((uix (%unique-index-for graph (list slot owner spec scope))))
        (dolist (p pairs)
          (destructuring-bind (key id) p
            (setf (gethash key (unique-index-table uix)) id))))))
  (setf *memory-image-unique-loaded* t))
