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
  owner-name slot-name spec test canonicalizer scope
  ;; Backing store -- exactly one is set.  TABLE: an in-RAM hash (memory backend;
  ;; persisted via the #50 checkpoint image).  SKIP-LIST: a persistent heap skip-list
  ;; keyed by the composite (canonical-key id) via REDUCE-COMP-LESSP, like a view (on-
  ;; disk backend; durable + incremental via mmap, address saved in a sidecar).
  table skip-list)

;;; Backend-agnostic operations over the backing store (K = canonical key).
(defun make-unique-skip-list (graph)
  "The persistent ordered-map backing an on-disk unique index -- a view-style
composite (canonical-key id) map under REDUCE-COMP-LESSP.  Follows *INDEX-BACKEND*
(skip list or B+ tree); the chosen backend is persisted per index in the sidecar so
REOPEN uses the right opener (see SAVE/RESTORE-UNIQUE-INDEX-ROOTS)."
  (make-view-skip-list graph (make-view :sort-order :lessp)))

(defun %open-unique-skip-list (graph address &optional (backend :skip-list))
  "Reopen the unique index at ADDRESS with BACKEND's opener (same composite-key
codec as a view; REDUCE-COMP-LESSP order)."
  (open-heap-index backend :address address :heap (indexes graph)
                   :comparison 'reduce-comp-lessp))

(defun uix-lookup (uix key)
  "The id currently holding canonical KEY in UIX, or NIL."
  (let ((tbl (unique-index-table uix)))
    (if tbl
        (gethash key tbl)
        (let* ((cur (make-range-cursor (unique-index-skip-list uix)
                                       (list key +null-key+) (list key +max-key+)))
               (node (cursor-next cur)))
          (when node (second (%sn-key node)))))))

(defun uix-put (uix key id)
  "Claim canonical KEY for node ID."
  (let ((tbl (unique-index-table uix)))
    (if tbl
        (setf (gethash key tbl) id)
        ;; The id lives in the composite key's second slot (read back by UIX-LOOKUP);
        ;; the skip-list VALUE is unused -- store NIL, not the raw id byte array (which
        ;; SERIALIZE cannot round-trip).
        (add-to-skip-list (unique-index-skip-list uix) (list key id) nil))))

(defun uix-remove (uix key id)
  "Release node ID's claim on canonical KEY (only)."
  (let ((tbl (unique-index-table uix)))
    (if tbl
        (when (equalp (gethash key tbl) id) (remhash key tbl))
        ;; the composite (key id) is specific to this node -- removes only its entry
        (remove-from-skip-list (unique-index-skip-list uix) (list key id)))))

(defun uix-count (uix)
  "Number of live entries in UIX (backend-agnostic)."
  (let ((tbl (unique-index-table uix)))
    (if tbl
        (hash-table-count tbl)
        (let ((cur (make-cursor (unique-index-skip-list uix))) (n 0))
          (loop for node = (cursor-next cur) while node do (incf n))
          n))))

(defvar *peer-apply-origin* nil
  "Bound during device pull-apply (APPLY-PEER-CREATE-WRITES / APPLY-PEER-AUTHORED-OP)
to the incoming op's origin, so a node created off the %COMMIT path still records the
authoring origin for its :ORIGIN partition (#6).")

(defun %current-authoring-origin (graph)
  "The origin to attribute a node CREATED in the current apply context to: the
re-homed op's original author (hub), the pulled op's origin (device), else this
graph's own origin (a local authored commit)."
  (or (and (boundp '*peer-rehome-op*) *peer-rehome-op* (second *peer-rehome-op*))
      *peer-apply-origin*
      (ignore-errors (origin-id graph))
      :graph))

(defun %node-origin (node graph)
  "The origin an :ORIGIN-scoped unique value partitions by: the node's FIXED creation
origin if recorded (a peer graph's NODE-ORIGINS store), else the current authoring
context (a node being created now).  A non-peer graph has no store and a single
origin, so :ORIGIN collapses to :LOCAL there."
  (or (and (node-origins graph) (get-node-origin graph (id node)))
      (%current-authoring-origin graph)))

(defun %origin-token (origin)
  "A comparable, content-equal token for ORIGIN inside a composite unique key.  A
16-byte id array has no LESS-THAN/EQUAL content semantics (both are identity-ish on
raw ub8 vectors), so render it as its hex string; a keyword/NIL passes through."
  (cond ((typep origin '(array (unsigned-byte 8) (*))) (peer-id->hex origin))
        ((null origin) :graph)
        (t origin)))

(defun %unique-key (uix value node graph)
  "The canonical key VALUE maps to in UIX (canonicalizer + scope applied), or NIL for
a NULL/unbound value (which is exempt from the constraint, SQL-style)."
  (when value
    (let ((k (let ((c (unique-index-canonicalizer uix)))
               (if c (funcall c value) value))))
      (if (eq (unique-index-scope uix) :origin)
          (list (%origin-token (%node-origin node graph)) k)
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
            (let ((uix (%make-unique-index
                        :owner-name owner-name :slot-name slot-name :spec spec
                        :test test :canonicalizer canon :scope scope)))
              ;; On-disk (a heap is present) -> a persistent skip-list; memory -> a hash.
              (if (indexes graph)
                  (setf (unique-index-skip-list uix) (make-unique-skip-list graph))
                  (setf (unique-index-table uix)
                        (make-hash-table :test test
                                         #+sbcl :synchronized #+sbcl t
                                         #+ccl :shared #+ccl t)))
              (setf (gethash key reg) uix)))))))

;;; ---------------------------------------------------------------------------
;;; Maintenance (APPLY, post-durability, journal-replayable)
;;; ---------------------------------------------------------------------------

(defun %uix-claim (node graph)
  "Claim NODE's unique keys (create / new value of an update)."
  (let ((slots (class-unique-slots (class-of node))))
    ;; Fix this node's :ORIGIN partition at create (set-once): record the authoring
    ;; origin now, while the apply context still names it, so a later RELEASE (on
    ;; update/delete) recomputes the SAME composite key even though the context is
    ;; gone.  Only on a peer graph with an :ORIGIN-scoped slot; a no-op otherwise.
    (when (and (node-origins graph) (find :origin slots :key #'fourth))
      (set-node-origin graph (id node) (%current-authoring-origin graph)))
    (dolist (d slots)
      (let* ((uix (%unique-index-for graph d))
             (key (%unique-key uix (slot-value node (first d)) node graph)))
        (when key (uix-put uix key (id node)))))))

(defun %uix-release (node graph)
  "Release NODE's unique keys (delete / old value of an update)."
  (dolist (d (class-unique-slots (class-of node)))
    (let* ((uix (%unique-index-for graph d))
           (key (%unique-key uix (slot-value node (first d)) node graph)))
      (when key (uix-remove uix key (id node))))))

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
                (let ((holder (uix-lookup uix key)))
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
                       (let ((existing (uix-lookup uix key)))
                         (if (and existing (not (equalp existing (id node))))
                             (log:warn "unique-index ~S.~S: pre-existing duplicate key ~S (~A / ~A); keeping first"
                                       (unique-index-owner-name uix) (unique-index-slot-name uix)
                                       key (string-id existing) (string-id (id node)))
                             (uix-put uix key (id node))))))))))
        (map-vertices #'index-node graph)
        (map-edges #'index-node graph)))))

(defun regenerate-unique-indexes (graph)
  "Drop every on-disk unique index and rebuild it using GRAPH's CURRENT index
backend (its INDEX-BACKEND slot -- see MAKE-GRAPH / OPEN-GRAPH :INDEX-BACKEND).
Use this to switch a graph's unique indexes to a different backend in place (e.g.
skip list -> B+ tree): reopen with :INDEX-BACKEND :BPLUS-TREE, then call this (the
parallel of REGENERATE-ALL-VIEWS / REBUILD-SPATIAL-INDEX).  Unlike REBUILD-UNIQUE-
INDEXES, which reuses whatever index is already present, this frees the old backing
stores first so the rebuild creates fresh ones on the new backend, then persists
the new backend tags to the sidecar."
  (when (unique-indexes graph)
    (maphash (lambda (k uix)
               (declare (ignore k))
               (let ((sl (unique-index-skip-list uix)))
                 (when (and sl (view-index-p sl))
                   (delete-view-index sl))))     ; free the old heap pages
             (unique-indexes graph))
    (clrhash (unique-indexes graph)))            ; %UNIQUE-INDEX-FOR now recreates fresh
  (rebuild-unique-indexes graph)                 ; repopulate on the current backend
  (save-unique-index-roots graph)                ; persist new backend tags
  graph)

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
                 (when (unique-index-table uix)   ; hash-backed (memory) indexes only
                   (let ((pairs '()))
                     (maphash (lambda (key id) (push (list key id) pairs))
                              (unique-index-table uix))
                     (push (list (unique-index-owner-name uix) (unique-index-slot-name uix)
                                 (unique-index-spec uix) (unique-index-scope uix) pairs)
                           acc))))
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

;;; ---------------------------------------------------------------------------
;;; Durable persistence -- ON-DISK backend.  Each unique index is a persistent heap
;;; skip-list, maintained incrementally in APPLY (mmap-durable, journal-replayable).
;;; Its heap address is saved in a sidecar at CLOSE-GRAPH and reopened at OPEN, like
;;; the spatial index -- so no open-time scan.  (A cl-store of the index *contents*
;;; would be wrong: stale after a crash, since nodes committed since the last close
;;; are in the heap but not the sidecar.  Only the address is persisted here; the
;;; contents live in the mmap skip-list.)
;;; ---------------------------------------------------------------------------

(defun unique-index-root-file (location)
  (format nil "~A/unique-indexes.dat" location))

(defun save-unique-index-roots (graph)
  "Persist the on-disk unique indexes' roots (owner slot spec scope address).  No-op
with no heap (memory) or no unique indexes.  Called at CLOSE-GRAPH."
  (when (and (indexes graph) (unique-indexes graph))
    (let ((roots '()))
      (maphash (lambda (k uix)
                 (declare (ignore k))
                 (when (unique-index-skip-list uix)
                   (push (list (unique-index-owner-name uix) (unique-index-slot-name uix)
                               (unique-index-spec uix) (unique-index-scope uix)
                               (view-index-address (unique-index-skip-list uix))
                               ;; backend tag -> reopen with the right opener
                               (view-index-backend-tag (unique-index-skip-list uix)))
                         roots)))
               (unique-indexes graph))
      (cl-store:store roots (unique-index-root-file (location graph))))))

(defun restore-unique-index-roots (graph)
  "Reopen the on-disk unique indexes from the sidecar -- no node scan.  Returns T if a
sidecar was present (caller skips REBUILD-UNIQUE-INDEXES); NIL to fall back to rebuild
(a fresh graph, or a crash before the roots were saved)."
  (let ((file (unique-index-root-file (location graph))))
    (when (probe-file file)
      (let ((reg (or (unique-indexes graph)
                     (setf (unique-indexes graph)
                           (make-hash-table :test 'equal
                                            #+sbcl :synchronized #+sbcl t
                                            #+ccl :shared #+ccl t)))))
        (dolist (r (cl-store:restore file))
          ;; BACKEND is absent in pre-B+-tree sidecars (5-tuples) -> defaults to
          ;; :skip-list, so an existing graph reopens exactly as before.
          (destructuring-bind (owner slot spec scope address &optional (backend :skip-list)) r
            (multiple-value-bind (test canon) (%resolve-unique-canonicalizer spec)
              (setf (gethash (cons owner slot) reg)
                    (%make-unique-index :owner-name owner :slot-name slot :spec spec
                                        :test test :canonicalizer canon :scope scope
                                        :skip-list (%open-unique-skip-list graph address backend)))))))
      t)))
