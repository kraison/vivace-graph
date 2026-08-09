(in-package :graph-db)

(define-condition slave-auth-error (error)
  ((reason :initarg :reason)
   (host :initarg :host))
  (:report (lambda (error stream)
             (with-slots (reason host) error
               (format stream "Slave auth error ~A: ~A." host reason)))))

(define-condition transaction-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason) error
               (format stream "Transaction error: ~A." reason)))))

(define-condition cross-graph-transaction-error (error)
  ((node :initarg :node)
   (transaction-graph :initarg :transaction-graph)
   (node-graph :initarg :node-graph))
  (:report (lambda (error stream)
             (with-slots (node transaction-graph node-graph) error
               ;; NODE is a full node object on the write path but a raw id
               ;; byte vector on the read path (GH #53); STRING-ID has a
               ;; method for both, so use it rather than printing the array.
               (format stream "Cross-graph access in a read-write transaction: ~
node ~A belongs to ~A but the transaction is on ~A. A read-write transaction ~
is single-graph; use one transaction per graph."
                       (string-id node) (and node-graph (graph-name node-graph))
                       (and transaction-graph (graph-name transaction-graph)))))))

(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition deserialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Deserialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition duplicate-node-class-error (error)
  ;; One CL class namespace, per-graph schemas: a second graph reusing a name
  ;; would silently clobber the first class's slots (GH #53).
  ((name :initarg :name) (existing-graph :initarg :existing-graph)
   (new-graph :initarg :new-graph))
  (:report (lambda (error stream)
             (with-slots (name existing-graph new-graph) error
               (format stream "Node class ~A is already defined for graph ~A; ~
cannot redefine it for ~A. Class names are global; remove the old definition ~
to re-home it." name existing-graph new-graph)))))

(define-condition stale-revision-error (error)
  ((instance :initarg :instance)
   (current-revision :initarg :current-revision))
  (:report (lambda (error stream)
             (with-slots (instance current-revision) error
               (format stream "Attempt to update stale revision ~S of ~S."
                       instance current-revision)))))

(define-condition duplicate-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Duplicate key ~S in ~S."
                       key instance)))))

(define-condition nonexistent-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Nonexistent key ~S in ~S."
                       key instance)))))

(define-condition node-already-deleted-error (error)
  ((node :initarg :node))
  (:report (lambda (error stream)
             (with-slots (node) error
               (format stream "Node ~A already deleted" node)))))

(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())

(define-condition edge-already-deleted-error (node-already-deleted-error)
  ())

(define-condition invalid-view-error (error)
  ((class-name :initarg :class-name)
   (view-name :initarg :view-name))
  (:report (lambda (error stream)
             (with-slots (class-name view-name) error
               (format stream
                       "No such graph view: ~A/~A"
                       class-name view-name)))))

(define-condition view-lock-error (error)
  ((message :initarg :message))
  (:report (lambda (error stream)
             (with-slots (message) error
               (format stream
                       "View locking error: '~A'"
                       message)))))

;;; Spatial / GEOS conditions.  Defined in core (GEOS-free) so the refine seam
;;; and its callers can reference them whether or not the graph-db/geos add-on
;;; is loaded.

(define-condition geos-error (error)
  ((message :initarg :message :initform nil :reader geos-error-message))
  (:report (lambda (error stream)
             (format stream "GEOS error: ~A"
                     (or (geos-error-message error) "(no message)"))))
  (:documentation "Signalled when a GEOS operation fails or reports an error."))

(define-condition geos-required-for-operation (error)
  ((operation :initarg :operation :initform nil :reader geos-required-operation))
  (:report (lambda (error stream)
             (format stream
                     "Operation ~A requires the graph-db/geos add-on (libgeos_c), ~
which is not loaded/available."
                     (or (geos-required-operation error) "(unknown)"))))
  (:documentation "Signalled when an exact-topology operation has no
dependency-free fallback and GEOS is unavailable."))

(define-condition vector-segment-capacity-exhausted (error)
  ;; OWNER/SLOT identify the segment on the transaction path, which knows them;
  ;; PATH identifies it on the direct SEGMENT-PUT path (%SEG-GROW), which does
  ;; not.  Exactly one pair is populated, hence the initforms -- an unbound slot
  ;; here would make the report itself error, in the middle of an incident.
  ((owner :initarg :owner :initform nil :reader vsce-owner)
   (slot :initarg :slot :initform nil :reader vsce-slot)
   (path :initarg :path :initform nil :reader vsce-path)
   (required :initarg :required :reader vsce-required)
   (reserved :initarg :reserved :reader vsce-reserved)
   (needed-bytes :initarg :needed-bytes :reader vsce-needed-bytes)
   (reason :initarg :reason :initform nil :reader vsce-reason))
  (:report (lambda (c s)
             (format s "vector segment ~A: growing to hold ~D entries needs ~D bytes, ~
but its mmap reservation is ~D, and the mapping could not be re-reserved and ~
relocated into a larger window (~A). Normally a segment grows past its ~
reservation by relocating -- under its own write lock -- so this is not the ~
ordinary ceiling it used to be. If relocation is switched off, re-enable ~
GRAPH-DB:*SEGMENT-RELOCATE-ON-EXHAUSTION*; if it FAILED, the process is out of ~
address space (check RLIMIT_AS / `ulimit -v`, which counts reserved address ~
space even though it is PROT_NONE and MAP_NORESERVE). Reopening the graph also ~
recomputes the reservation from the file's current size, giving it the larger ~
of GRAPH-DB:*SEGMENT-MIN-RESERVATION* (16 GiB by default, and the floor that ~
actually applies to vector segment files) and ~
GRAPH-DB::*MMAP-RESERVATION-MULTIPLIER* times that size (8x by default). ~
GRAPH-DB::*MMAP-MIN-RESERVATION* is NOT consulted for segment files, so raising ~
it alone does nothing here."
                     (if (vsce-owner c)
                         (format nil "~A/~A" (vsce-owner c) (vsce-slot c))
                         (or (vsce-path c) "(unknown)"))
                     (vsce-required c)
                     (vsce-needed-bytes c) (vsce-reserved c)
                     (or (vsce-reason c) "no reason recorded"))))
  (:documentation "Signalled when a vector segment must grow past its mmap
reservation and cannot relocate to a larger one -- either because relocation is
switched off (*SEGMENT-RELOCATE-ON-EXHAUSTION*) or because it failed outright
 (address space exhausted / RLIMIT_AS).

WHERE IT IS SIGNALLED FROM, precisely -- the blanket \"pre-durability on the
transaction path\" this used to claim is not true of every case:

  * ENSURE-VECTOR-SEGMENT-CAPACITY (the normal transaction path) signals it
    PRE-DURABILITY, in the manager-locked region before
    FINALIZE-TX-PERSISTENCE, having changed nothing -- so the whole transaction,
    node write included, rolls back cleanly and no node is left without a
    segment entry.  That is the case this condition exists to make safe.
  * %SEG-GROW signals it directly for a NON-transactional writer -- a bare
    SEGMENT-PUT, REBUILD-VECTOR-SEGMENT-BATCHED -- where there is no transaction
    and \"pre-durability\" means nothing.  Such a signal carries PATH, not
    OWNER/SLOT.
  * It can also escape from INSIDE APPLY-TRANSACTION, i.e. POST-DURABILITY, in
    two residual cases the pre-flight cannot cover: a segment that does not
    exist yet (it is created inside apply, and one transaction inserting more
    vectors than the fresh reservation covers can exhaust it), and a segment
    whose capacity was consumed after the pre-flight by a concurrent lock-free
    mutator -- REBUILD-VECTOR-SEGMENT-BATCHED, which deliberately runs without
    the manager lock.  Both are narrow, both require relocation to be off or to
    fail, and both are documented at ENSURE-VECTOR-SEGMENT-CAPACITY.  Seeing
    this condition escape a commit rather than abort one means you are in one of
    them."))
