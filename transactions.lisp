(in-package :graph-db)

;; Defined in unique-constraint.lisp (loaded after this file); declared here so the
;; %COMMIT / APPLY-TRANSACTION hooks below compile without a forward-reference warning.
(declaim (ftype (function (t t) t)
                validate-unique-constraints apply-tx-writes-to-unique-indexes))

(defvar *transaction* nil)
(defvar *end-of-transaction-action* '%commit)
(defparameter *maximum-transaction-attempts* 8
  "The number of times a transaction is retried after failing
  validation before it is forced to run within an exclusive lock.")
(defparameter *add-to-indexes-unless-present-p* nil
  "When true, add nodes to the type indexes with a check for
  unqiueness in the index. Needed when potentially recovering from a
  transaction multiple times, e.g. if the recovery crashes and has to
  be restarted.")

;;; Psyching-out set-byte

(defmethod set-byte ((array array) offset byte)
  (setf (aref array offset) byte))

(defmethod get-byte ((array array) offset)
  (aref array offset))

;; Batched counterparts so the head codecs' single SET-BYTES/GET-BYTES also work
;; when the "file" is a plain byte array (the .txn transaction buffer and the
;; codec round-trip tests), not just an mmap'd file.
(defmethod set-bytes ((array array) vec offset length)
  (replace array vec :start1 offset :end1 (+ offset length) :start2 0 :end2 length)
  vec)

(defmethod get-bytes ((array array) offset length)
  (let ((vec (make-byte-vector length)))
    (replace vec array :start2 offset :end2 (+ offset length))
    vec))

(defmethod serialize-uint64 ((array array) int offset)
  (setf (aref array (+ offset 0)) (ldb (byte 8  0) int))
  (setf (aref array (+ offset 1)) (ldb (byte 8  8) int))
  (setf (aref array (+ offset 2)) (ldb (byte 8 16) int))
  (setf (aref array (+ offset 3)) (ldb (byte 8 24) int))
  (setf (aref array (+ offset 4)) (ldb (byte 8 32) int))
  (setf (aref array (+ offset 5)) (ldb (byte 8 40) int))
  (setf (aref array (+ offset 6)) (ldb (byte 8 48) int))
  (setf (aref array (+ offset 7)) (ldb (byte 8 56) int)))

;;; Object sets keep track of transaction read sets and write sets to
;;; aid in validating transactions.
;;;
;;; Initial implementation as a hash table is for simplicity. Many
;;; other data structures can be used for performance if needed.
(defgeneric make-object-set (initial-contents))
(defgeneric object-set-count (set))
(defgeneric object-set-list (set))
(defgeneric object-set-empty-p (set)
  (:method (set)
    (zerop (object-set-count set))))

(defgeneric add-to-object-set (object set))
(defgeneric object-set-member-p (object set))
(defgeneric call-for-object-set-objects (fun set))

(defmacro do-object-set ((object set) &body body)
  `(block nil
     (call-for-object-set-objects (lambda (,object) ,@body)
                                  ,set)))

(defgeneric object-sets-intersect-p (set1 set2)
  (:method (set1 set2)
    (do-object-set (object set1)
      (when (object-set-member-p object set2)
        (return t)))))

(defclass object-set ()
  ((table
    ;; Validation reads object-sets from other threads; CCL requires :shared t.
    :initform (make-id-table #+ccl :synchronized #+ccl t)
    :reader table)))

(defmethod object-set-list ((set object-set))
  (alexandria:hash-table-values (table set)))

(defmethod object-set-count ((set object-set))
  (hash-table-count (table set)))

(defmethod print-object ((set object-set) stream)
  (print-unreadable-object (set stream :type t)
    (cond ((plusp (object-set-count set))
           (format stream "[~{~A~^ ~}]" (object-set-list set)))
          (t
           (format stream "empty")))))

(defmethod make-object-set (initial-contents)
  (let ((set (make-instance 'object-set)))
    (dolist (object initial-contents set)
      (add-to-object-set object set))))

(defmethod add-to-object-set (object (set object-set))
  (setf (gethash (id object) (table set)) object))

(defmethod object-set-member-p (object (set object-set))
  (nth-value 1 (gethash (id object) (table set))))

(defmethod call-for-object-set-objects (fun (set object-set))
  (maphash (lambda (key object)
             (declare (ignore key))
             (funcall fun object))
           (table set)))

;;; Replication - the bulk is in transaction-streaming.lisp

(defgeneric replicate-transaction (transaction graph))

;;; Transaction conditions
(define-condition validation-conflict (error)
  ((transaction
    :initarg :transaction
    :reader validation-conflict-transaction)))

(define-condition no-transaction-in-progress (error) ())

(define-condition no-transaction-in-progress-warning (warning) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No transaction in progress; copy cannot be saved"))))

(define-condition modifying-non-copy (error)
  ((node
    :initarg :node
    :reader modifying-non-copy-node))
  (:report (lambda (condition stream)
             (format stream "Modifying ~A without copying first"
                     (modifying-non-copy-node condition)))))

;;; Transaction manager
(defgeneric create-transaction (transaction-manager))
(defgeneric cleanup-transaction (transaction))

(defgeneric graph (object)
  (:documentation
   "Return the associated graph of OBJECT."))

(defgeneric overlapping-transactions (transaction transaction-manager)
  (:documentation
   "Return a list of committed transactions that may affect
   TRANSACTION."))

(defgeneric transaction-lock (transaction))

(defgeneric call-with-transaction-lock (transaction fun)
  (:method (transaction fun)
    ;; TODO: This can have a coarse lock during recovery and fine
    ;; locks during normal use
    (with-write-lock ((transaction-lock transaction))
      (funcall fun))))

(defmacro with-transaction-lock ((transaction) &body body)
  `(call-with-transaction-lock ,transaction
                               (lambda ()
                                 ,@body)))

(defgeneric assign-transaction-id (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (let ((new-id (tx-id-counter transaction-manager)))
      (setf (transaction-id transaction) new-id)
      (incf (tx-id-counter transaction-manager))
      new-id)))

;;; Transactions
(defgeneric transaction-manager (object)
  (:documentation
   "Return the transaction manager of OBJECT."))

(defgeneric state (transaction))
(defgeneric (setf state) (transaction new-value))

(defgeneric sequence-number (transaction))
(defgeneric (setf sequence-number) (transaction new-value))

(defgeneric read-set (transaction))
(defgeneric create-set (transaction))
(defgeneric write-set (transaction))

(defgeneric local-cache (transaction))
(defgeneric graph-cache (transaction))

(defgeneric lookup-object (id table transaction graph)
  (:method (id table (transaction null) graph)
    ;; Non-transactional read: pin the read epoch so the reaper retains whatever
    ;; version we observe.  If this is a STANDALONE lookup (no enclosing pin), the
    ;; node escapes our pin, so materialize its bytes now (the lhash value-
    ;; finalizer used to do this under the bucket lock).  If we are nested in a
    ;; pinned scan, that scan already protects the node and handles escape, so we
    ;; leave the data lazy.  Transactional reads -- the other method -- are
    ;; covered by the transaction's own start-tx-id.
    (let ((standalone (not *read-pinned-p*)))
      (with-read-pin (graph)
        (let ((node (lookup-node table id graph)))
          (when (and standalone (node-p node))
            (ensure-node-bytes node graph))
          node))))
  (:method (id table transaction (graph t))
    (let ((local-cache (local-cache transaction))
          (graph-cache (graph-cache transaction)))
      (let ((local (gethash id local-cache)))
        (if local
            local
            (let ((value (or (gethash id graph-cache)
                             (lookup-node table id (graph transaction)))))
              (when value
                ;; P4: resolve the version visible at this transaction's snapshot
                ;; (commit-epoch < start-tx-id).  The resolved (possibly archived)
                ;; version is cached only in the txn-private local-cache, so reads
                ;; are repeatable within the transaction.  Validation keys the
                ;; read-set by id, so OCC is unaffected.
                (when *snapshot-reads-p*
                  (setq value (resolve-version-at-epoch
                               value (graph transaction)
                               (start-tx-id transaction))))
                (when value
                  (add-to-object-set value (read-set transaction))
                  (setf (gethash id local-cache) value)))))))))

(defgeneric write-object (object transaction))

(defgeneric writes (transaction)
  (:method (transaction)
    (append (object-set-list (create-set transaction))
            (object-set-list (write-set transaction)))))

(defgeneric write-count (transaction)
  (:method (transaction)
    (+ (object-set-count (create-set transaction))
       (object-set-count (write-set transaction)))))

(defgeneric (setf writes) (new-value transaction))

(defgeneric validate (transaction)
  (:method (transaction)
    (let ((write-set (write-set transaction)))
      (or (zerop (object-set-count write-set))
          (loop for other-transaction in (overlapping-transactions
                                          transaction
                                          (transaction-manager transaction))
             never (object-sets-intersect-p write-set
                                            (read-set other-transaction))
             never (object-sets-intersect-p write-set
                                            (write-set other-transaction)))))))

(defgeneric %commit (transaction))
(defgeneric %rollback (transaction))

(defgeneric call-with-transaction (fun transaction-manager)
  (:documentation "Call FUN with *TRANSACTION* bound to a new
  transaction created from TRANSACTION-MANAGER."))

(defmacro with-transaction ((&optional (transaction-manager '(transaction-manager *graph*)))
                            &body body)
  "Run BODY as a single ACID transaction against TRANSACTION-MANAGER (by
default that of the current *GRAPH*) and return BODY's value.

All mutations -- MAKE-<type> constructors, SAVE, DELETE-NODE/MARK-DELETED --
must run inside a transaction.  On normal exit the transaction is validated
against its read/write sets and committed; if validation finds a conflict it
is retried (up to *MAXIMUM-TRANSACTION-ATTEMPTS*, then under an exclusive
lock).  A non-local exit rolls it back.  To modify an existing node, COPY it
inside the transaction, mutate the copy, then SAVE it."
  `(call-with-transaction (lambda () ,@body) ,transaction-manager))

(defclass tx ()
  ((read-set
    :initarg :read-set
    :reader read-set)
   (create-set
    :initarg :create-set
    :reader create-set)
   (write-set
    :initarg :write-set
    :reader write-set)
   (transaction-lock
    :initarg :transaction-lock
    :reader transaction-lock)
   (local-cache
    :initarg :local-cache
    :reader local-cache)
   (copies
    :initarg :copies
    :reader copies
    :documentation "A node to be modified must first be copied via
    COPY, which places it in this EQ hash table. UPDATE-NODE will
    refer to this copy when persisting the transaction.")
   (graph-cache
    :initarg :graph-cache
    :reader graph-cache)
   (graph
    :initarg :graph
    :reader graph)
   (transaction-manager
    :initarg :transaction-manager
    :reader transaction-manager)
   (state
    :initarg :state
    :accessor state)
   (sequence-number
    :initarg :sequence-number
    :accessor sequence-number)
   (start-tx-id
    :initarg :start-tx-id
    :reader start-tx-id
    :documentation "The value of the tx-id-counter when this
    transaction was created.")
   (finish-tx-id
    :initarg :finish-tx-id
    :accessor finish-tx-id
    :documentation "The value of the tx-id-counter when this
    transaction is ended.")
   (transaction-id
    :initarg :tx-id
    :accessor transaction-id
    :documentation "A transaction-id is assigned from the transaction
    manager tx-id-counter only after a transaction has been
    validated.")
   (bytes-components
    :initarg :bytes-components
    :initform '()
    :accessor bytes-components
    :documentation "A list of vectors that will be concatenated to
    form BYTES during persisting.")
   (bytes
    :initarg :bytes
    :accessor bytes
    :documentation "BYTES has a serialization of the transaction after
    it has been committed."))
  (:default-initargs
   :read-set (make-object-set nil)
    :create-set (make-object-set nil)
    :write-set (make-object-set nil)
    :transaction-lock (make-rw-lock)
    :local-cache (make-id-table)
    :copies (make-hash-table)
    :state :init))

(defmethod print-object ((transaction tx) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~D: ~D read~:P, ~D create~:P, ~D write~:P, ~S"
            (sequence-number transaction)
            (object-set-count (read-set transaction))
            (object-set-count (create-set transaction))
            (object-set-count (write-set transaction))
            (state transaction))))


;;; Applying transaction writes to the graph

(defun maybe-initialize-bytes (node)
  "Initialize the BYTES slot of NODE, if necessary."
  (let ((data (data node))
        (bytes (bytes node)))
    (when (and data
               (or (eql bytes :init)
                   (null bytes)))
      (setf (bytes node) (serialize data)))))

(defun maybe-allocate-for-node (node graph)
  "Allocate heap storage and initialize the data pointer for NODE, if
needed."
  (maybe-initialize-bytes node)
  (setf (data-pointer node)
        (if (data node)
            (allocate (heap graph) (length (bytes node)))
            0)))

;;; FIXME: Find a better home for this method
(defmethod set-bytes ((memory memory) vec offset length)
  (declare (type word offset length))
  (dotimes (i length)
    (set-byte memory (+ i offset) (aref vec i)))
  vec)

(defun maybe-write-to-heap (node graph)
  "Write the heap data for NODE to the heap, if necesssary. Nodes with
no data are not written."
  (let ((data-pointer (maybe-allocate-for-node node graph)))
    (unless (zerop data-pointer)
      (let ((bytes (bytes node)))
        (set-bytes (heap graph) bytes data-pointer (length bytes))))))

(defun maybe-free-from-heap (node graph)
  "Free the heap space used by NODE, if necessary."
  (let ((data-pointer (data-pointer node)))
    (unless (zerop data-pointer)
      (handler-case
          (free (heap graph) data-pointer)
        (error (c)
          (log:error "Unable to free ~A (~A): ~A" (string-id node) data-pointer c))))))

;;; ---------------------------------------------------------------------------
;;; MVCC versioned write path + lazy, epoch-gated reaper
;;;
;;; On update/delete the prior version's head is archived into its own heap block
;;; (its data-pointer still references the -- now retained -- old data block) and
;;; the new live head's PREV-POINTER chains to it.  COMMIT-EPOCH stamps every
;;; write with the committing transaction-id.  Old versions are reclaimed lazily
;;; by REAP-OLD-VERSIONS once no active reader/transaction can still observe them.
;;; ---------------------------------------------------------------------------

;; The committing transaction-id, bound by APPLY-TRANSACTION (the shared apply
;; path, so masters, slaves, restore and recovery all stamp consistently).  Never
;; travels on the replication wire -- the slave re-derives it from the tx header.
(defvar *commit-epoch* 0)

(defun archive-node-version (old-node graph)
  "Copy OLD-NODE's current head into a freshly allocated heap block and return
its address (for the new live head's PREV-POINTER).  The archived head keeps
OLD-NODE's data-pointer/commit-epoch/prev-pointer, so the retained old data block
and the rest of the version chain remain reachable."
  (let* ((size (etypecase old-node
                 (edge   +edge-header-size+)
                 (vertex +node-header-size+)))
         (addr (allocate (heap graph) size)))
    (etypecase old-node
      (edge   (serialize-edge-head   (heap graph) old-node addr))
      (vertex (serialize-vertex-head (heap graph) old-node addr)))
    addr))

(defun read-archived-head (graph addr)
  "Return (values data-pointer commit-epoch prev-pointer) of the archived head
at heap ADDR.  Only the node-head prefix is read (an edge's from/to/weight are
irrelevant to reaping)."
  (multiple-value-bind (d w hw tiw vw vew vvw type-id rev data-ptr epoch prev off)
      (deserialize-node-head (heap graph) addr)
    (declare (ignore d w hw tiw vw vew vvw type-id rev off))
    (values data-ptr epoch prev)))

;;; --- P4 (PROTOTYPE): snapshot-isolation reads -------------------------------
;;; A transaction observes the newest version with commit-epoch < its
;;; start-tx-id.  When the live head is too new (committed after the reader
;;; started), walk the prev-pointer chain and materialize the archived version
;;; that was live as of the reader's snapshot.  The reaper's floor retains every
;;; version an active transaction could need, so the chain is guaranteed present.

(defvar *snapshot-reads-p* t
  "When true, transactional LOOKUP-OBJECT resolves the version visible at the
transaction's start epoch (snapshot isolation).  Prototype toggle.")

(defun resolve-version-at-epoch (live-node graph epoch)
  "Return the version of LIVE-NODE visible to a reader whose snapshot is EPOCH
(the newest version with commit-epoch < EPOCH), or NIL if the node did not exist
before EPOCH.  Materializes an archived version (full head + data bytes) when the
live head is newer than EPOCH."
  (if (< (commit-epoch live-node) epoch)
      live-node
      (let ((id (id live-node))
            (edge-p (typep live-node 'edge))
            (p (prev-pointer live-node)))
        (loop
          (when (zerop p) (return nil))   ; nothing old enough -> invisible
          (let ((ver (if edge-p
                         (deserialize-edge-head (heap graph) p)
                         (deserialize-vertex-head (heap graph) p))))
            (setf (id ver) id)
            (if (< (commit-epoch ver) epoch)
                (progn (ensure-node-bytes ver graph) (return ver))
                (setf p (prev-pointer ver))))))))

;;; Read-epoch pins (non-transactional reads).  A reader records the current
;;; epoch BEFORE it reads a node head and holds the pin until it has finished
;;; dereferencing that node's data.  While pinned, the reaper's floor is bounded
;;; by the pin, so any version that was live at pin time (stop-epoch >= pin)
;;; cannot be reclaimed out from under the reader.

(defun pin-read-epoch (transaction-manager)
  "Register a read pin at the current epoch; return its token (for UNPIN)."
  ;; Racy read of tx-id-counter is fine: it is monotonic, and a slightly stale
  ;; (smaller) value only makes the reaper MORE conservative, never less.
  (let ((epoch (tx-id-counter transaction-manager)))
    (with-recursive-lock-held ((read-pins-lock transaction-manager))
      (let ((token (incf (read-pin-counter transaction-manager))))
        (setf (gethash token (read-pins transaction-manager)) epoch)
        token))))

(defun unpin-read-epoch (transaction-manager token)
  (with-recursive-lock-held ((read-pins-lock transaction-manager))
    (remhash token (read-pins transaction-manager))))

(defun minimum-read-pin (transaction-manager)
  (with-recursive-lock-held ((read-pins-lock transaction-manager))
    (let (min)
      (maphash (lambda (token epoch)
                 (declare (ignore token))
                 (setf min (if min (min min epoch) epoch)))
               (read-pins transaction-manager))
      min)))

;; WITH-READ-PIN (the macro) lives in graph-class.lisp -- it is used by
;; MAP-VERTICES / MAP-EDGES, which are compiled before this file.

(defun reap-safe-floor (transaction-manager)
  "The oldest epoch any still-active transaction OR pinned non-transactional
reader could observe.  NIL => nothing active => every archived version is
reclaimable."
  (let ((s (minimum-start-transaction-id transaction-manager))
        (p (minimum-read-pin transaction-manager)))
    (cond ((and s p) (min s p))
          (s s)
          (p p)
          (t nil))))

(defun %sever-prev-pointer (owner table live-node graph)
  "Set OWNER's prev-pointer field to 0.  OWNER is :LIVE (LIVE-NODE's lhash head)
or the heap address of an archived head."
  (if (eq owner :live)
      (progn
        (lhash-custom-update
         table
         (lambda (mf off)
           (dotimes (i 8) (set-byte mf (+ off +node-prev-pointer-offset+ i) 0)))
         (id live-node))
        (setf (prev-pointer live-node) 0))
      (dotimes (i 8)
        (set-byte (heap graph) (+ owner +node-prev-pointer-offset+ i) 0))))

(defun %free-version-chain (addr graph)
  "Free the archived-version head blocks from ADDR down its prev-pointer chain,
plus each version's retained data block."
  (let ((heap (heap graph))
        (p addr))
    (loop
      (when (zerop p) (return))
      (multiple-value-bind (data-ptr epoch prev) (read-archived-head graph p)
        (declare (ignore epoch))
        (let ((next prev))
          (when (> data-ptr 0)
            (handler-case (free heap data-ptr)
              (error (c) (log:error "reaper: free data ~A failed: ~A" data-ptr c))))
          (handler-case (free heap p)
            (error (c) (log:error "reaper: free head ~A failed: ~A" p c)))
          (setq p next))))))

(defun reap-node-chain (live-node graph floor keep)
  "Walk LIVE-NODE's prev-pointer chain and free the contiguous oldest suffix
whose stop-epoch (the commit-epoch of the next-newer version) is < FLOOR,
retaining at least KEEP archived versions.  Repairs exactly one prev-pointer."
  (let ((table (tx-write-table live-node graph))
        (above-epoch (commit-epoch live-node))  ; stop-epoch of the first archived ver
        (owner :live)                           ; :live or a heap address
        (p (prev-pointer live-node))
        (kept 0))
    (loop
      (when (zerop p) (return))
      (multiple-value-bind (data-ptr epoch prev) (read-archived-head graph p)
        (declare (ignore data-ptr))
        (cond
          ;; A reader with start-tx-id S observes versions with commit-epoch < S.
          ;; The version at P stopped being live at ABOVE-EPOCH (its successor's
          ;; commit-epoch), so a reader needs it iff S <= ABOVE-EPOCH.  Hence it
          ;; is reclaimable iff every active start is strictly past ABOVE-EPOCH,
          ;; i.e. ABOVE-EPOCH < FLOOR (strict).  NIL floor => nothing active.
          ((and (>= kept keep)
                (or (null floor) (< above-epoch floor)))
           ;; This version (and all older) can no longer be observed: cut here.
           (%sever-prev-pointer owner table live-node graph)
           (%free-version-chain p graph)
           (return))
          (t
           ;; Retain this version; descend one link.
           (incf kept)
           (setf above-epoch epoch
                 owner p
                 p prev)))))))

(defun node-keep-revisions (node graph default)
  "How many archived versions of NODE the reaper retains regardless of epoch
safety: NODE's node-type :keep-revisions if set, else the graph DEFAULT."
  (let ((type-id (type-id node)))
    (or (and (integerp type-id) (> type-id 0)
             (let ((meta (lookup-node-type-by-id
                          type-id (if (typep node 'edge) :edge :vertex)
                          :graph graph)))
               (and meta (node-type-keep-revisions meta))))
        default)))

(defun reap-old-versions (writes graph)
  "Post-commit: reclaim archived versions of every updated/deleted node that no
active reader/transaction can still observe, keeping each node's configured
:KEEP-REVISIONS window.  Runs inside the transaction-manager lock (via
APPLY-TRANSACTION)."
  ;; During crash recovery OPEN-GRAPH replays transactions BEFORE it installs the
  ;; transaction-manager, so the slot may be unbound here.  No readers can be
  ;; active during recovery, so a NIL floor (reap-everything-safe) is correct.
  (let ((floor (let ((tm (and (slot-boundp graph 'transaction-manager)
                              (transaction-manager graph))))
                 (and tm (reap-safe-floor tm))))
        (default-keep (if (slot-boundp graph 'schema)
                          (schema-keep-revisions (schema graph))
                          0)))
    (dolist (write writes)
      (when (typep write 'tx-update)        ; tx-update and its subclass tx-delete
        (let ((node (node write)))
          (reap-node-chain node graph floor
                           (node-keep-revisions node graph default-keep)))))))

(defgeneric add-node-to-indexes (node graph &key unless-present)
  (:method ((node node) graph &key unless-present)
    (add-to-type-index node graph :unless-present unless-present)
    (setf (type-idx-written-p node) t))
  (:method ((node edge) graph &key unless-present)
    (call-next-method)
    (add-to-vev-index node graph :unless-present unless-present)
    (setf (vev-written-p node) t)
    (add-to-ve-index node graph :unless-present unless-present)
    (setf (ve-written-p node) t)))

;;; tx-writes have enough information to update the graph database and
;;; its views.

(defclass tx-write ()
  ((node
    :initarg :node
    :reader node)))

(defmethod id ((write tx-write))
  (id (node write)))

(defmethod print-object ((write tx-write) stream)
  (print-unreadable-object (write stream :type t)
    (format stream "for ~A ~A"
            (class-name (class-of (node write)))
            (string-id (id write)))))

(defclass tx-create (tx-write) ())

(defclass tx-update (tx-write)
  ((old-node
    :initarg :old-node
    :reader old-node)))

(defclass tx-delete (tx-update) ())

(defgeneric tx-write-table (object graph)
  (:method ((edge edge) graph)
    (edge-table graph))
  (:method ((vertex vertex) graph)
    (vertex-table graph))
  (:method ((write tx-write) graph)
    (tx-write-table (node write) graph)))

(defgeneric apply-tx-write (tx-write graph))

(defmethod apply-tx-write :after ((write tx-write) graph)
  (let ((node (node write)))
    (setf (gethash (id node) (cache graph)) node)))

(defmethod apply-tx-write ((write tx-create) graph)
  (let ((table (tx-write-table write graph))
        (node (node write)))
    (setf (revision node) 0)
    (maybe-write-to-heap node graph)
    (add-node-to-indexes node graph
                         :unless-present *add-to-indexes-unless-present-p*)
    ;; Mark written-p BEFORE the node becomes visible in the lhash.  written-p is
    ;; a persisted node-head flag; if it is set only afterward (in finalize-node)
    ;; the node is briefly in the table with written-p=0 on disk, and a
    ;; concurrent reader can deserialize and cache that stale copy, overwriting
    ;; the committed one — so map-vertices later skips it (its written-p guard
    ;; fails) and the insert is lost from type-indexed scans.  Setting it here
    ;; means the lhash entry carries written-p=1 from its first appearance.
    (setf (written-p node) t)
    ;; MVCC: stamp the committing epoch; a fresh node has no prior version.
    (setf (commit-epoch node) *commit-epoch*
          (prev-pointer node) 0)
    (handler-case
        (lhash-insert table (id node) node)
      (duplicate-key-error (condition)
        (declare (ignore condition))
        (lhash-update table (id node) node)))
    (finalize-node node table graph))
  write)

(defmethod apply-tx-write ((write tx-update) graph)
  (let ((new-node (node write))
        (old-node (old-node write))
        (table (tx-write-table write graph)))
    (setf (revision new-node)
          (ldb (byte 32 0) (1+ (revision old-node))))
    (setf (bytes new-node)
          (serialize (data new-node)))
    (maybe-write-to-heap new-node graph)
    ;; MVCC: archive the prior version's head (it still points at the retained
    ;; old data block) and chain the new live head to it.  The old data block is
    ;; NO LONGER freed here -- REAP-OLD-VERSIONS reclaims it when epoch-safe.
    (setf (prev-pointer new-node) (archive-node-version old-node graph)
          (commit-epoch new-node) *commit-epoch*)
    (lhash-update table (id new-node) new-node))
  write)


;;; Applying transaction view updates

(defgeneric call-for-applicable-views (fun graph node)
  (:method (fun graph (node node))
    (loop
       for view in (lookup-views graph node)
       when view do (funcall fun view))))

(defmacro do-applicable-views ((view graph node) &body body)
  `(call-for-applicable-views (lambda (,view) ,@body)
                              ,graph ,node))

(defgeneric applicable-views (node graph)
  (:method (node graph)
    (let ((result '()))
      (do-applicable-views (view graph node)
        (push view result))
      (nreverse result))))


(defgeneric apply-tx-write-to-views (write graph))

(defmethod apply-tx-write-to-views ((write tx-create) graph)
  (let ((node (node write)))
;;    (do-applicable-views (view graph node)
;;      (add-to-view graph view node))))
    (log:debug "Apply ~A to views for ~A" write (type-of node))
    (add-to-views graph node)))

(defmethod apply-tx-write-to-views ((write tx-update) graph)
  (let ((new-node (node write))
        (old-node (old-node write)))
;;    (do-applicable-views (view graph new-node)
;;      (remove-from-view graph view old-node)
;;      (add-to-view graph view new-node))))
    (log:debug "Apply ~A to views for ~A" write (type-of old-node))
    (update-in-views graph new-node old-node)))

(defmethod apply-tx-write-to-views ((write tx-delete) graph)
  (let ((node (node write)))
;;    (do-applicable-views (view graph node)
;;      (remove-from-view graph view node))))
    (remove-from-views graph node)))


;;; Applying the transaction

(defvar *highest-transaction-id-lock*
  (make-recursive-lock "transaction id file"))

(defgeneric highest-transaction-id-file (graph)
  (:method (graph)
    (make-pathname :name "transaction-id"
                   :type "dat"
                   :defaults (location graph))))

(defgeneric persist-highest-transaction-id (transaction-id graph)
  (:method (transaction-id graph)
    (let ((persist-file (highest-transaction-id-file graph))
          (serialized (make-byte-vector 8)))
      (serialize-uint64 serialized transaction-id 0)
      (with-open-file (stream persist-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create
                              :if-exists :overwrite)
        (with-recursive-lock-held (*highest-transaction-id-lock*)
          (write-sequence serialized stream)))
      transaction-id)))

(defgeneric load-highest-transaction-id (graph)
  (:method (graph)
    (let ((persist-file (highest-transaction-id-file graph))
          (serialized (make-byte-vector 8)))
      (with-recursive-lock-held (*highest-transaction-id-lock*)
        (if (probe-file persist-file)
            (with-open-file (stream persist-file
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
              (let ((offset (read-sequence serialized stream)))
                (unless (= offset (length serialized))
                  (error "Bad read-sequence from transaction id file"))
                (deserialize-uint64 serialized 0)))
            0)))))

(defgeneric apply-tx-writes (writes graph)
  (:method (writes graph)
    (dolist (write writes)
      (apply-tx-write write graph))))

(defgeneric apply-tx-writes-to-views (writes graph)
  (:method (writes graph)
    (dolist (write writes)
      (apply-tx-write-to-views write graph))))

;;; Applying transaction spatial-index updates (public spatial extension).
;;;
;;; Mirrors the view-maintenance pass above: a node's geometry -- obtained via
;;; the NODE-GEOMETRY protocol, which an application specializes for its
;;; geometry-bearing node types -- is (re)indexed on create/update and removed
;;; on delete.  The spatial index keys on node id, so it is independent of the
;;; MVCC version chains and the reaper (reaping an old version of a node never
;;; changes its index entry).

(defvar *node-geometry-slot-cache*
  #+sbcl (make-hash-table :test 'eq :synchronized t)
  #+ccl (make-hash-table :test 'eq :shared t)
  #+lispworks (make-hash-table :test 'eq :single-thread nil)
  #+ecl (make-hash-table :test 'eq)
  "Cache CLASS -> list of its :INDEX-marked slot names (candidate geometry slots).")

(defun node-geometry-index-slots (class)
  "Names of CLASS's :INDEX-marked slots -- the candidate geometry slots.  A
geometry slot opts into spatial indexing declaratively, e.g.
  (def-vertex observation () ((location :type geometry :index t)) :app)
We deliberately do NOT match on the declared slot type here: the `:type geometry'
symbol is read in the *application's* package, which is not necessarily EQ to
GRAPH-DB:GEOMETRY (and a user need not even declare a type).  Instead we return
every indexed slot, and NODE-GEOMETRY picks the one whose runtime value is an
actual GEOMETRY (via GEOMETRYP) -- robust across packages and to mixed
geometry/non-geometry indexed slots on the same type.  Cached per class (runtime
schema redefinition is not expected)."
  (multiple-value-bind (val present) (gethash class *node-geometry-slot-cache*)
    (if present
        val
        (setf (gethash class *node-geometry-slot-cache*)
              (when (class-finalized-p class)
                (loop for slot in (class-slots class)
                      when (indexed-p slot)
                        collect (slot-definition-name slot)))))))

(defgeneric node-geometry (node)
  (:documentation
   "The GEOMETRY a node occupies, or NIL if it has none.  By default this is the
value of the node's :INDEX-marked geometry slot (see NODE-GEOMETRY-INDEX-SLOTS);
declaring (slot :type geometry :index t) is enough to make a node type spatially
indexed.  Applications may instead specialize this method (e.g. for a computed
geometry); an explicit method takes precedence over the default.")
  (:method (node) (declare (ignore node)) nil)
  (:method ((node node))
    ;; NB: do NOT gate on SLOT-BOUNDP -- node-class persistent slots are read
    ;; through SLOT-VALUE-USING-CLASS from the serialized buffer, and
    ;; SLOT-BOUNDP reports the (always-unbound) backing CLOS slot, so it would
    ;; skip every persistent slot.  Read the value and test it directly.
    (loop for slot in (node-geometry-index-slots (class-of node))
          for v = (ignore-errors (slot-value node slot))
          when (geometryp v) return v)))

(defgeneric apply-tx-write-to-spatial-index (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-spatial-index ((write tx-create) graph)
  (let ((node (node write)) (idx (spatial-index graph)))
    (when idx
      (let ((geom (node-geometry node)))
        (when (and geom (not (deleted-p node)))
          (spatial-index-insert idx (id node) geom))))))

(defmethod apply-tx-write-to-spatial-index ((write tx-update) graph)
  (let ((new-node (node write)) (old-node (old-node write)) (idx (spatial-index graph)))
    (when idx
      (let ((old-geom (node-geometry old-node))
            (new-geom (node-geometry new-node)))
        (when old-geom (spatial-index-remove idx (id old-node) old-geom))
        (when (and new-geom (not (deleted-p new-node)))
          (spatial-index-insert idx (id new-node) new-geom))))))

(defmethod apply-tx-write-to-spatial-index ((write tx-delete) graph)
  (let ((node (node write)) (idx (spatial-index graph)))
    (when idx
      (let ((geom (node-geometry node)))
        (when geom (spatial-index-remove idx (id node) geom))))))

(defgeneric apply-tx-writes-to-spatial-index (writes graph)
  (:method (writes graph)
    (dolist (write writes)
      (apply-tx-write-to-spatial-index write graph))))

;; NB: the old free-the-prior-version GARBAGE-COLLECT-HEAP is gone.  Under MVCC
;; the prior version is retained (archived + chained) and reclaimed later by
;; REAP-OLD-VERSIONS; freeing it at commit would orphan the version chain.

(defvar *after-apply-tx-writes-hook* nil
  "When non-nil, a zero-argument function called after apply-tx-writes but
before apply-tx-writes-to-views.  The hook fires once and self-clears.
Intended for durability tests that need to simulate a crash between the
lhash write and the view update, leaving a pending .txn file for recovery.")

(defun filter-writes (writes filter)
  "Keep only the WRITES whose node FILTER accepts; with no FILTER, keep all.
Used for subset replication: a slave applies just the writes its filter accepts."
  (if filter
      (remove-if-not (lambda (w) (funcall filter (node w))) writes)
      writes))

;;; --- Subset-replication reconciliation -------------------------------------
;;;
;;; Plain FILTER-WRITES keeps a write iff its (new) node passes the filter.  That
;;; is wrong for a node that CROSSES the subset boundary on an update: a node that
;;; leaves the subset would be dropped, leaving the slave holding a stale in-subset
;;; copy; a node that enters the subset would be applied as an update the slave
;;; cannot satisfy (it never had the node).  RECONCILE-SLAVE-WRITES fixes both by
;;; considering the filter AND whether the slave currently holds the node:
;;;
;;;   update, passes, slave has it      -> apply update
;;;   update, passes, slave lacks it    -> apply as CREATE (node entered subset)
;;;   update, fails,  slave has it      -> apply as DELETE (node left subset)
;;;   update, fails,  slave lacks it    -> drop
;;;   create, passes / delete, present  -> keep; otherwise drop
;;;
;;; This is the generic predicate-filtered-replication MECHANISM (it works for any
;;; REPLICATION-FILTER, spatial or not); the area predicate itself is supplied by
;;; the application (e.g. MAKE-SPATIAL-REPLICATION-FILTER).

(defun %slave-current-node (node graph)
  "The slave's live local node with NODE's id (a vertex or edge), or NIL."
  (etypecase node
    (vertex (lookup-vertex (id node) :graph graph))
    (edge   (lookup-edge (id node) :graph graph))))

(defun %slave-has-node-p (node graph)
  (let ((n (%slave-current-node node graph)))
    (and n (not (deleted-p n)))))

(defun %make-slave-delete-write (write graph)
  "A tx-delete that removes the node from the slave because it left the subset.
Built from the slave's CURRENT local node (so the MVCC archive/chain uses local
heap addresses), mirroring DELETE-NODE."
  (let ((current (%slave-current-node (node write) graph)))
    (when current
      (let ((deleted (copy current)))
        (setf (bytes deleted) (bytes current)
              (deleted-p deleted) t)
        (make-instance 'tx-delete :node deleted :old-node current)))))

(defun reconcile-slave-write (write new-passes present graph)
  "Map one incoming WRITE to the write the slave should apply (or NIL to drop),
given whether its new node passes the filter and whether the slave holds it.
NB: tx-delete is a subclass of tx-update, so it must be matched first."
  (typecase write
    (tx-delete (and present write))
    (tx-create (and new-passes write))
    (tx-update
     (cond ((and new-passes present) write)
           (new-passes (make-instance 'tx-create :node (node write)))
           (present (%make-slave-delete-write write graph))
           (t nil)))
    (t (and new-passes write))))

(defun reconcile-slave-writes (writes filter graph)
  "FILTER-WRITES with boundary-crossing reconciliation (see commentary above).
With no FILTER, returns WRITES unchanged."
  (if (null filter)
      writes
      (loop for w in writes
            for reconciled = (reconcile-slave-write
                              w
                              (funcall filter (node w))
                              (%slave-has-node-p (node w) graph)
                              graph)
            when reconciled collect it)))

(defgeneric apply-transaction (transaction graph)
  (:method (transaction graph)
    (with-transaction-lock (transaction)
      (let ((writes (writes transaction))
            ;; Bind *GRAPH* to the target so every index-maintenance sink that
            ;; still defaults its graph/heap to *GRAPH* (e.g. index-list
            ;; deserialization) targets THIS graph, not whatever is ambient on a
            ;; slave/replay/multi-graph apply thread.  Defense-in-depth: the
            ;; helpers below already thread GRAPH explicitly.
            (*graph* graph)
            ;; MVCC: every write in this transaction is stamped with this id.
            (*commit-epoch* (transaction-id transaction)))
        ;; Subset replication: on a slave with a replication-filter, drop the
        ;; writes outside its subset BEFORE applying, so the lhash, views and
        ;; spatial index all stay consistent.  The highest-transaction-id is
        ;; still advanced below, so filtered transactions are not re-requested.
        (when (slave-graph-p graph)
          (setq writes (reconcile-slave-writes
                        writes (replication-filter graph) graph)))
        (apply-tx-writes writes graph)
        (when *after-apply-tx-writes-hook*
          (let ((hook *after-apply-tx-writes-hook*))
            (setf *after-apply-tx-writes-hook* nil)
            (funcall hook)))
        (apply-tx-writes-to-views writes graph)
        (apply-tx-writes-to-spatial-index writes graph)
        (apply-tx-writes-to-unique-indexes writes graph)   ; issue #6
        (reap-old-versions writes graph)
        (persist-highest-transaction-id (transaction-id transaction) graph)))))

(defmethod apply-transaction :after (transaction (graph master-graph))
  (replicate-transaction transaction graph))

;;;
;;; Serializing a transaction
;;;
;;; A transaction is serialized as a header chunk followed by a number
;;; of tx-write chunks.
;;;
;;; The transaction header is as follows:
;;;
;;;   - 8 bytes for the header size (fixed)
;;;
;;;   - 1 byte for flags; currently unused
;;;
;;;   - 1 byte for type (#\x72)
;;;
;;;   - 8 bytes for the transaction id
;;;
;;;   - 8 bytes for the tx-write count
;;;
;;;   - 8 bytes for the total size of following tx-writes
;;;

(defclass tx-header ()
  ((transaction-id
    :initarg :transaction-id
    :reader transaction-id)
   (write-count
    :initarg :write-count
    :accessor write-count)
   (write-size
    :initarg :write-size
    :accessor write-size
    :documentation "The number of bytes of serialized tx-write data
    following the tx-header.")
   (writes
    :initarg :writes
    :accessor writes)
   (graph
    :initarg :graph
    :accessor graph)))

(alexandria:define-constant +tx-header-size+ (+ 8 1 1 8 8 8))
(alexandria:define-constant +tx-header-type-code+ (char-code #\t))

(defun make-tx-header-vector ()
  (make-byte-vector +tx-header-size+))

;; The transaction-id lives at this byte offset within the serialized header
;; (and thus at the start of the tx file): 8-byte size + 1 flag + 1 type code.
;; In a .txn file this is left at the placeholder 0 (the authoritative id is the
;; filename; see the TODO above FINALIZE-TX-PERSISTENCE).  Master graphs patch
;; the real id into the in-memory bytes at this offset for the replication log.
(defconstant +tx-header-id-offset+ 10)

(defun serialize-tx-header (tx vector offset &optional (id (transaction-id tx)))
  (serialize-uint64 vector +tx-header-size+ offset)
  (incf offset 8)
  ;; Skip flags
  (incf offset)
  (setf (aref vector offset) +tx-header-type-code+)
  (incf offset)
  (serialize-uint64 vector id offset)
  (incf offset 8)
  (let ((writes (writes tx)))
    (serialize-uint64 vector (length writes) offset)
    (incf offset 8)
    (let ((total-size (reduce #'+ writes :key 'tx-write-vector-size)))
      (serialize-uint64 vector total-size offset)
      (incf offset 8)
      (values vector offset))))

(defun deserialize-tx-header-vector (vector)
  ;; Skip the size and flags and type
  (let ((offset 10))
    (let ((transaction-id (deserialize-uint64 vector offset))
          (write-count (deserialize-uint64 vector (+ offset 8)))
          (write-size (deserialize-uint64 vector (+ offset 16))))
      (make-instance 'tx-header
                     :transaction-id transaction-id
                     :write-count write-count
                     :write-size write-size))))

(defun tx-header-vector (tx &optional (id (transaction-id tx)))
  (let ((vector (make-tx-header-vector)))
    (values (serialize-tx-header tx vector 0 id))))

(defun read-uint64-sized-vector (stream)
  (let ((size-vector (make-byte-vector 8)))
    (let ((last-position (read-sequence size-vector stream)))
      (when (= 0 last-position)
        (return-from read-uint64-sized-vector nil))
      (unless (= 8 last-position)
        (error "Could not read size information from ~A" stream))
      (let* ((size (deserialize-uint64 size-vector 0))
             (vector (make-byte-vector size)))
        (setf last-position (read-sequence vector stream :start 8))
        (unless (= last-position size)
          (error "Could not read to ~D  (got ~D) bytes from ~A"
                 size last-position stream))
        (replace vector size-vector)
        vector))))


;;; Serializing tx-writes
;;;
;;; A tx-write consists of a header followed by one or two serialized
;;; nodes.
;;;
;;; tx-write header:
;;;
;;;   - 8 bytes for total tx-write size
;;;
;;;   - 1 byte for flags; currently unused
;;;
;;;   - 1 byte for type (#x63 for create, #x75 for update, #x64 for delete)
;;;
;;;   - 1 byte for node count
;;;
;;; A node:
;;;
;;;   - 8 bytes for size
;;;
;;;   - 1 byte flags; currently unused
;;;
;;;   - 1 byte for type (#x65 for edge, #x76 for vertex)
;;;
;;;   - 16 bytes for uuid
;;;
;;;   - 1 bytes for node header size
;;;
;;;   - N bytes for node header
;;;
;;;   - M bytes for node heap value
;;;



;;; Serialize a single node

(alexandria:define-constant +transaction-node-base-header-size+
    (+ 8 1 16 1 1))
(alexandria:define-constant +transaction-node-edge-code+ (char-code #\e))
(alexandria:define-constant +transaction-node-vertex-code+ (char-code #\v))

(defun transaction-node-header-size (node)
  (etypecase node
    (edge +edge-header-size+)
    (vertex +node-header-size+)))

(defun transaction-node-vector-size (node)
  (+ +transaction-node-base-header-size+
     (transaction-node-header-size node)
     (if (typep (bytes node) 'sequence)
         (length (bytes node))
         0)))

(defun transaction-node-type-code (node)
  (etypecase node
    (edge +transaction-node-edge-code+)
    (vertex +transaction-node-vertex-code+)))

(defun serialize-transaction-node-header (node vector offset)
  (etypecase node
    (edge
     (serialize-edge-head vector node offset))
    (vertex
     (serialize-node-head vector node offset))))

(defun serialize-transaction-uuid (uuid vector offset)
  (replace vector uuid :start1 offset))

(defun deserialize-transaction-uuid (vector offset)
  (subseq vector offset (+ offset 16)))

(defun serialize-transaction-node (node vector offset)
  (let* ((size (transaction-node-vector-size node))
         (type-code (transaction-node-type-code node))
         (header-size (transaction-node-header-size node))
         (bytes (bytes node))
         (flags 0))
    ;; 8 byte size
    (serialize-uint64 vector size offset)
    (incf offset 8)
    ;; 1 byte (unused) flags
    (set-byte vector offset flags)
    (incf offset)
    ;; 1 byte type
    (set-byte vector offset type-code)
    (incf offset)
    ;; 16 byte uuid
    (serialize-transaction-uuid (id node) vector offset )
    (incf offset 16)
    ;; 1 byte node header size
    (set-byte vector offset header-size)
    (incf offset)
    ;; header-size bytes of node header
    (serialize-transaction-node-header node vector offset)
    (incf offset header-size)
    ;; (length bytes) of node bytes
    (when (typep bytes 'sequence)
      (replace vector bytes :start1 offset)
      (incf offset (length bytes)))
    offset))

(defun transaction-node-vector (node)
  (let* ((size (transaction-node-vector-size node))
         (vector (make-byte-vector size))
         (offset 0))
    (serialize-transaction-node node vector offset)
    vector))

(defun deserialize-edge-transaction-node-vector (vector id
                                                 header-offset
                                                 data-offset
                                                 end)
  (let* ((edge (deserialize-edge-head vector header-offset))
         (bytes (subseq vector data-offset end)))
    (setf (id edge) id)
    (if (> (length bytes) 0)
        (progn
          (setf (data edge) (deserialize bytes))
          (setf (bytes edge) bytes))
        (setf (data edge) nil))
    edge))

(defun deserialize-vertex-transaction-node-vector (vector id
                                                   header-offset
                                                   data-offset
                                                   end)
  (let ((vertex (deserialize-vertex-head vector header-offset))
        (bytes (subseq vector data-offset end)))
    (setf (id vertex) id)
    (if (> (length bytes) 0)
        (progn
          (setf (data vertex) (deserialize bytes))
          (setf (bytes vertex) bytes))
        (setf (data vertex) nil))
    vertex))

(defun deserialize-transaction-node-vector (vector &optional (offset 0))
  "Return the edge or vertex represented by VECTOR."
  (let (size uuid type header-size end)
    (setf size (deserialize-uint64 vector offset))
    (setf end (+ offset size))
    (incf offset 8)
    ;; Skip flags
    (incf offset)
    (setf type (get-byte vector offset))
    (incf offset)
    (setf uuid (deserialize-transaction-uuid vector offset))
    (incf offset 16)
    (setf header-size (get-byte vector offset))
    (incf offset)
    (let* ((header-offset offset)
           (data-offset (+ offset header-size))
           (node
            (cond ((eql type +transaction-node-edge-code+)
                   (deserialize-edge-transaction-node-vector vector
                                                             uuid
                                                             header-offset
                                                             data-offset
                                                             end))
                  ((eql type +transaction-node-vertex-code+)
                   (deserialize-vertex-transaction-node-vector vector
                                                               uuid
                                                               header-offset
                                                               data-offset
                                                               end))
                  (t
                   (error "Unknown transaction node type ~S" type)))))
      (values node end))))

;;; Serialize a tx-write

(alexandria:define-constant +tx-write-header-size+ (+ 8 1 1 1))
(alexandria:define-constant +tx-write-create-code+ (char-code #\c))
(alexandria:define-constant +tx-write-update-code+ (char-code #\u))
(alexandria:define-constant +tx-write-delete-code+ (char-code #\d))

(defun tx-write-vector-size (tx-write)
  (+ +tx-write-header-size+
     (transaction-node-vector-size (node tx-write))
     (if (typep tx-write 'tx-update)
         (transaction-node-vector-size (old-node tx-write))
         0)))

(defgeneric tx-write-vector-code (tx-write)
  (:method ((write tx-create))
    +tx-write-create-code+)
  (:method ((write tx-update))
    +tx-write-update-code+)
  (:method ((write tx-delete))
    +tx-write-delete-code+))

(defgeneric tx-write-node-count (tx-write)
  (:method ((write tx-write))
    1)
  (:method ((write tx-update))
    2))

(defun tx-write-vector (tx-write)
  "Serialize TX-WRITE to a byte vector and return the vector."
  (let* ((size (tx-write-vector-size tx-write))
         (vector (make-byte-vector size))
         (node-count (tx-write-node-count tx-write))
         (offset 0))
    ;; 8 byte size
    (serialize-uint64 vector size offset)
    (incf offset 8)
    ;; 1 byte flag (unused)
    (incf offset 1)
    ;; 1 byte type code
    (set-byte vector offset (tx-write-vector-code tx-write))
    (incf offset 1)
    ;; 1 byte count
    (set-byte vector offset node-count)
    (incf offset 1)
    ;; Nodes
    (setf offset (serialize-transaction-node (node tx-write)
                                             vector
                                             offset))
    (when (= 2 node-count)
      (setf offset (serialize-transaction-node (old-node tx-write)
                                               vector
                                               offset)))
    (values vector offset)))

(defun tx-write-class (type-code)
  "Return the class name designated by TYPE-CODE."
  (cond ((eql type-code +tx-write-create-code+)
         'tx-create)
        ((eql type-code +tx-write-update-code+)
         'tx-update)
        ((eql type-code +tx-write-delete-code+)
         'tx-delete)
        (t
         (error "Unknown type code ~S" type-code))))

(defun deserialize-tx-write-vector (vector)
  ;; Skip the size and flags
  (let* ((offset 9)
         type-code count node class)
    (setf type-code (get-byte vector offset))
    (setf class (tx-write-class type-code))
    (incf offset)
    (setf count (get-byte vector offset))
    (incf offset)
    (setf (values node offset)
          (deserialize-transaction-node-vector vector offset))
    (if (= count 2)
        (let ((old-node (deserialize-transaction-node-vector vector offset)))
          ;; The wire/txn copy of OLD-NODE carries the writer's LOCAL heap
          ;; addresses + epoch (the master's, for a replicated tx), which are
          ;; meaningless here.  Re-derive them from THIS graph's current node so
          ;; ARCHIVE-NODE-VERSION builds a correct local version chain (the MVCC
          ;; prev-pointer is a local heap address and must never be copied across
          ;; the wire).  COMMIT-EPOCH of the new live head is re-stamped from the
          ;; transaction-id in APPLY-TRANSACTION, so it stays consistent.
          (log:debug "FINDING PROPER DATA-POINTER FOR ~A (~A)"
                     (id old-node) (data-pointer old-node))
          (let ((local-old-node (if (vertex-p old-node)
                                    (lookup-vertex (id old-node))
                                    (lookup-edge (id old-node)))))
            (when local-old-node
              (setf (data-pointer old-node)  (data-pointer local-old-node)
                    (prev-pointer old-node)  (prev-pointer local-old-node)
                    (commit-epoch old-node)  (commit-epoch local-old-node))))
          (make-instance class
                         :node node
                         :old-node old-node))
        (make-instance class :node node))))

;;; Saving tx-writes to a file

(defun save-bytes-component-vector (vector tx)
  (push vector (bytes-components tx)))

(defun write-tx-writes-to-stream (tx stream)
  (dolist (write (writes tx))
    (let ((v (tx-write-vector write)))
      (save-bytes-component-vector v tx)
      (write-sequence (tx-write-vector write) stream))))

(defun write-tx-header-to-stream (tx stream &optional (id (transaction-id tx)))
  (let ((v (tx-header-vector tx id)))
    (save-bytes-component-vector v tx)
    (write-sequence v stream)))

(defun initialize-bytes-from-components (tx)
  (let* ((components (bytes-components tx))
         (length (reduce #'+ components :key 'length))
         (bytes (make-byte-vector length))
         (offset 0))
    (dolist (component (reverse components))
      (replace bytes component :start1 offset)
      (incf offset (length component)))
    (setf (bytes-components tx) nil)
    (setf (bytes tx) bytes)))

(defgeneric persist-tx (transaction initial-file final-file)
  (:documentation
   "Persist TRANSACTION to INITIAL-FILE. After the transaction has
   been completely written and INITIAL-FILE is closed, INITIAL-FILE is
   reanamed to FINAL-FILE. The intent is to make it atomically clear
   that the transaction file has a complete complete set of related
   changes. After transaction has persisted, its BYTES slot contains a
   serialization of the transaction.")
  (:method (transaction initial-file final-file)
    (with-open-file (stream initial-file :direction :output
                            :if-exists :error
                            :element-type '(unsigned-byte 8))
      (write-tx-header-to-stream transaction stream)
      (write-tx-writes-to-stream transaction stream))
    (initialize-bytes-from-components transaction)
    (rename-file initial-file final-file)
    final-file))

(defun load-tx-header (stream)
  (let ((vector (read-uint64-sized-vector stream)))
    (when vector
      (deserialize-tx-header-vector vector))))

(defun load-one-tx-write (stream)
  "Load a single write node from STREAM. Returns NIL if no writes are
left in the stream."
  (let ((vector (read-uint64-sized-vector stream)))
    (when vector
      (deserialize-tx-write-vector vector))))

(defun load-tx-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((tx-header (load-tx-header stream)))
      (setf (writes tx-header)
            (loop for i from 0
               repeat (write-count tx-header)
               for node = (load-one-tx-write stream)
               unless node do (error "Too few writes in ~A: expected ~A, ended at ~A"
                                     file
                                     (write-count tx-header)
                                     i)
               collect node))
      tx-header)))

(defgeneric persistent-transaction-directory (graph)
  (:method (graph)
    (merge-pathnames "tx/" (location graph))))

(defgeneric transaction-pathname (transaction)
  (:method (transaction)
    (make-pathname :name (format nil "~16,'0X" (transaction-id transaction))
                   :type "txn"
                   :defaults (persistent-transaction-directory
                              (graph transaction)))))

(defgeneric transaction-temporary-pathname (transaction)
  (:method (transaction)
    (make-pathname :type "txn-tmp"
                   :defaults (transaction-pathname transaction))))

(defgeneric persist-transaction (transaction)
  (:method (transaction)
    (persist-tx transaction
                (transaction-temporary-pathname transaction)
                (transaction-pathname transaction))
    (let* ((transaction-manager (transaction-manager transaction))
           (stream (replication-log transaction-manager))
           (lock (replication-log-lock transaction-manager)))
      (with-recursive-lock-held (lock)
        (write-sequence (bytes transaction)
                        (replication-log (transaction-manager transaction)))
        (finish-output stream)))))

(defun transaction-prepare-pathname (transaction)
  "A unique per-attempt temp pathname, keyed by SEQUENCE-NUMBER (the
transaction-id is not yet assigned at prepare time).  Type \"txn-tmp\" so
RECOVERY-TRANSACTION-FILES (which scans \"txn\" files) ignores it even if a
crash orphans one."
  (make-pathname :name (format nil "prep-~16,'0X" (sequence-number transaction))
                 :type "txn-tmp"
                 :defaults (persistent-transaction-directory
                            (graph transaction))))

;;; -------------------------------------------------------------------------
;;; TODO (revisit — correctness/maintainability): the transaction file's HEADER
;;; transaction-id is intentionally left at a placeholder 0; the AUTHORITATIVE id
;;; for a .txn file is its FILENAME (~16,'0X hex), and recovery reads it from
;;; there (LOAD-RECOVERY-TRANSACTION).  This is what lets FINALIZE do a single
;;; rename under the manager lock instead of an in-lock header write — the only
;;; form measured to actually relieve the CCL/ECL commit-lock convoy at high
;;; thread counts (any in-lock file write reconvoys; see
;;; docs/concurrency-scaling-investigation.md).  The smell: the same header
;;; layout means two different things — placeholder 0 in a .txn file vs. the real
;;; id in a replication-log entry (masters patch it into the in-memory bytes
;;; below).  Replication is unaffected (it reads the replication-log, never the
;;; .txn files).  Revisit for a cleaner design (e.g. drop the id from the .txn
;;; header format entirely, or a stable-id scheme) once the rw-lock work lands.
;;; -------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Peer replication (WP-0): the peer-meta packet.
;;;
;;; A peer-graph prefixes each transaction it journals to its feed with a
;;; self-size-framed peer-meta packet carrying the authored op's identity and
;;; logical clock, so the op can be deduped across re-homing (design §3) and
;;; ordered for conflict resolution (Branch B).  The inner transaction (tx-header
;;; + tx-writes) follows in the standard format, so the peer transport reads the
;;; peer-meta packet first, then the ordinary tx packets.  Layout:
;;;   size(8) flags(1) type(1=#\M) origin-id(16) op-id(16) lamport(8) op-class(1)
;;; ---------------------------------------------------------------------------

(alexandria:define-constant +peer-meta-type-code+ (char-code #\M))
(alexandria:define-constant +peer-meta-packet-size+ (+ 8 1 1 16 16 8 1)) ; 51
(alexandria:define-constant +peer-op-authored+ 0)      ; an authored user/automation op
(alexandria:define-constant +peer-op-state-create+ 1)  ; hub membership-create (WP-5)
(alexandria:define-constant +peer-op-purge+ 2)         ; hub scope-exit purge (WP-5)
(alexandria:define-constant +peer-null-origin+
    (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
  :test 'equalp)

(defun gen-op-id ()
  "A fresh 16-byte op-id (random v4 uuid) identifying an authored change event.
Stable across re-homing -- the hub never re-mints it (design §3 #2)."
  (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))

(defvar *peer-rehome-op* nil
  "Bound to (OP-ID ORIGIN LAMPORT) while a hub re-homes a device-pushed authored op
through a journaling transaction (peer-streaming REHOME-AUTHORED-OP, Branch B).  It
tells FINALIZE-TX-PERSISTENCE to preserve the ORIGINAL op identity on the
re-journaled feed entry (design §5) instead of minting a fresh one, and to skip the
whole-diff field stamping (the re-home caller applies the merge's per-field stamps,
which the diff would stamp with the wrong lamport for a field the local copy won).")

;;; Durable Lamport clock (PT-8).  The counter must be monotonic ACROSS restarts:
;;; if it reset to 0 after a crash, a replica's post-restart authored ops would get
;;; tiny stamps and silently lose every LWW race (safety data dropped).  So we
;;; persist it on every advance and reload it on open.  Wall-clock skew is
;;; irrelevant -- Lamport is logical -- a real win for GPS/EW-denied field devices.

(defgeneric lamport-counter-file (graph)
  (:method (graph)
    (make-pathname :name "lamport" :type "dat" :defaults (location graph))))

(defgeneric persist-lamport-counter (graph)
  (:method ((graph peer-graph))
    (let ((serialized (make-byte-vector 8)))
      (serialize-uint64 serialized (lamport-counter graph) 0)
      (with-open-file (stream (lamport-counter-file graph)
                              :direction :output :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create :if-exists :overwrite)
        (write-sequence serialized stream))
      (lamport-counter graph)))
  (:method ((graph graph)) nil))

(defgeneric load-lamport-counter (graph)
  (:method (graph)
    (let ((file (lamport-counter-file graph))
          (serialized (make-byte-vector 8)))
      (if (probe-file file)
          (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
            (if (= 8 (read-sequence serialized stream))
                (deserialize-uint64 serialized 0)
                0))
          0))))

;;; Device pull-cursor (Branch B).  Kept SEPARATE from HIGHEST-TRANSACTION-ID: the
;;; latter is the graph's OWN feed-seq (advanced + persisted on every local commit,
;;; APPLY-TRANSACTION), and a Branch B device authors locally, so overloading it as
;;; the pull-cursor would conflate the device's own feed-seq (device tx-id space)
;;; with the highest HUB feed-seq it has applied (hub tx-id space) -- corrupting both
;;; the pull position and the device's tx-id-counter restore on open.  A read-only
;;; Branch A device never writes, so the two coincided; once a device writes they do
;;; not.  So the pull-cursor gets its own durable scalar.
(defgeneric peer-pull-cursor-file (graph)
  (:method (graph)
    (make-pathname :name "pull-cursor" :type "dat" :defaults (location graph))))

(defgeneric persist-peer-pull-cursor (cursor graph)
  (:method (cursor (graph peer-graph))
    (let ((serialized (make-byte-vector 8)))
      (serialize-uint64 serialized cursor 0)
      (with-open-file (stream (peer-pull-cursor-file graph)
                              :direction :output :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create :if-exists :overwrite)
        (write-sequence serialized stream))
      cursor))
  (:method (cursor (graph graph)) (declare (ignore cursor)) nil))

(defgeneric load-peer-pull-cursor (graph)
  (:method (graph)
    (let ((file (peer-pull-cursor-file graph))
          (serialized (make-byte-vector 8)))
      (if (probe-file file)
          (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
            (if (= 8 (read-sequence serialized stream)) (deserialize-uint64 serialized 0) 0))
          0))))

;;; Device push-ack (Branch B): the highest of the device's OWN feed-seqs that the
;;; hub has confirmed it re-homed.  The push feed's lower bound -- the device streams
;;; its own authored ops with feed-seq > push-ack.  Its own durable scalar (device
;;; tx-id space), distinct from the pull-cursor (hub tx-id space) and the own feed-seq.
;;; Only an optimization: the hub dedups every op by op-id, so a device that loses its
;;; push-ack merely re-streams already-applied ops (re-deduped), never double-applies.
(defgeneric peer-push-ack-file (graph)
  (:method (graph)
    (make-pathname :name "push-ack" :type "dat" :defaults (location graph))))

(defgeneric persist-peer-push-ack (ack graph)
  (:method (ack (graph peer-graph))
    (let ((serialized (make-byte-vector 8)))
      (serialize-uint64 serialized ack 0)
      (with-open-file (stream (peer-push-ack-file graph)
                              :direction :output :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create :if-exists :overwrite)
        (write-sequence serialized stream))
      ack))
  (:method (ack (graph graph)) (declare (ignore ack)) nil))

(defgeneric load-peer-push-ack (graph)
  (:method (graph)
    (let ((file (peer-push-ack-file graph))
          (serialized (make-byte-vector 8)))
      (if (probe-file file)
          (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
            (if (= 8 (read-sequence serialized stream)) (deserialize-uint64 serialized 0) 0))
          0))))

(defun peer-next-lamport (graph)
  "Advance and return GRAPH's Lamport clock, persisting the new value (PT-8).
Called under the replication-log lock while journaling an authored op; the
lamport-lock (innermost) makes the read-modify-write atomic against a concurrent
PEER-OBSERVE-LAMPORT on another thread."
  (with-recursive-lock-held ((lamport-lock graph))
    (prog1 (incf (lamport-counter graph))
      (persist-lamport-counter graph))))

(defun peer-observe-lamport (graph received)
  "Advance GRAPH's Lamport clock to at least RECEIVED (a stamp carried by an
applied op), so the replica's next mint is causally after everything it has seen
(PT-8: on receipt, advance to MAX(local,received); the +1 happens at the next
mint).  Persists if it moved.  A NIL/zero RECEIVED is a no-op."
  (when (and received (> received 0) (typep graph 'peer-graph))
    (with-recursive-lock-held ((lamport-lock graph))
      (when (> received (lamport-counter graph))
        (setf (lamport-counter graph) received)
        (persist-lamport-counter graph))))
  graph)

(defun peer-observe-epoch (graph epoch)
  "Advance GRAPH's TX-ID-COUNTER so it STRICTLY EXCEEDS EPOCH -- the MVCC commit
epoch of an op this peer just APPLIED from a remote origin (a pulled node carries the
HUB's epoch).  A device seeds its counter from its OWN feed-seq; without this the
counter can sit at or below the epochs it applied, so a subsequent LOCAL edit
transaction starts at a START-TX-ID that MVCC-hides the very node it means to edit
(the node is invisible until the counter passes its epoch).  Under the tm lock, so it
composes with CREATE-TRANSACTION; idempotent + monotonic; a NIL/zero EPOCH is a no-op.
The durable side is the pull-cursor, which the barrier advances to the pull frontier T
(>= every applied epoch) and which TX-ID-COUNTER is re-seeded from on open."
  (when (and epoch (> epoch 0) (typep graph 'peer-graph))
    (let ((tm (transaction-manager graph)))
      (with-recursive-lock-held ((lock tm))
        (when (>= epoch (tx-id-counter tm))
          (setf (tx-id-counter tm) (1+ epoch))))))
  graph)

(defun serialize-peer-meta (origin-id op-id lamport op-class)
  "Build the 51-byte peer-meta packet (see layout above)."
  (let ((v (make-byte-vector +peer-meta-packet-size+)) (i 0))
    (serialize-uint64 v +peer-meta-packet-size+ i) (incf i 8)
    (setf (aref v i) 0) (incf i)                       ; flags (unused)
    (setf (aref v i) +peer-meta-type-code+) (incf i)   ; type
    (replace v origin-id :start1 i :end2 16) (incf i 16)
    (replace v op-id :start1 i :end2 16) (incf i 16)
    (serialize-uint64 v (or lamport 0) i) (incf i 8)
    (setf (aref v i) op-class)
    v))

(defun deserialize-peer-meta (vector &optional (offset 0))
  "Parse a peer-meta packet at OFFSET.  Returns (values origin-id op-id lamport
op-class).  Asserts the packet's type byte."
  (assert (= (aref vector (+ offset 9)) +peer-meta-type-code+))
  (let ((i (+ offset 10)))
    (values (subseq vector i (+ i 16))
            (subseq vector (+ i 16) (+ i 32))
            (deserialize-uint64 vector (+ i 32))
            (aref vector (+ i 40)))))

(defun prepare-tx-persistence (transaction)
  "Serialize TRANSACTION to a temp file and populate its BYTES slot.  Runs BEFORE
the transaction-manager lock, so the bulk serialization + disk write (and the
flush at close) are off the serialized commit path.  The header id is written as
the placeholder 0 — the real id is encoded in the final FILENAME by the rename in
FINALIZE-TX-PERSISTENCE, and recovery reads it from there.  Returns the temp
pathname."
  (let ((tmp (transaction-prepare-pathname transaction)))
    (with-open-file (stream tmp :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-tx-header-to-stream transaction stream 0)
      (write-tx-writes-to-stream transaction stream))
    (initialize-bytes-from-components transaction)
    tmp))

(defun finalize-tx-persistence (transaction tmp)
  "Under the transaction-manager lock, once TRANSACTION-ID is assigned: make the
durable, recovery-visible record by renaming the prepared temp file to its final
id-keyed name.  For a non-replicated graph this is a SINGLE rename — the only
file I/O left in the commit critical section, which is what relieves the
commit-lock convoy (the bulk write happened in PREPARE-TX-PERSISTENCE before the
lock).  Recovery reads the id from this filename, so the file's header id stays
at its placeholder 0.  Master graphs additionally patch the real id into the
in-memory bytes and append them to the replication log in commit order for
downstream slaves (the replication path is the only consumer of the header id,
and it reads these patched bytes, never the .txn files)."
  ;; Use POSIX rename(2) (atomic; replaces an existing target) rather than
  ;; cl:rename-file.  SBCL/ECL's rename-file already overwrites per POSIX, but
  ;; CCL's signals "File exists" when the target exists — which intermittently
  ;; crashed concurrent-stress on CCL.  %posix-rename gives portable, atomic,
  ;; overwrite-on-rename behavior across all implementations.
  (%posix-rename (namestring tmp)
                 (namestring (transaction-pathname transaction)))
  (let ((tm (transaction-manager transaction)))
    ;; peer-replication WP-2: generalized from MASTER-GRAPH-P so a peer-graph also
    ;; journals its own committed writes (a device's push feed / a hub's pull feed).
    ;; The patched-in transaction-id is the per-origin feed sequence (design §3 #3).
    (when (journals-own-feed-p (graph tm))
      (serialize-uint64 (bytes transaction)
                        (transaction-id transaction)
                        +tx-header-id-offset+)
      (let ((repl-stream (replication-log tm))
            (lock (replication-log-lock tm))
            (gr (graph tm)))
        (with-recursive-lock-held (lock)
          ;; peer-replication WP-0: a peer-graph prefixes the feed entry with a
          ;; peer-meta packet (op identity + lamport), then records that it has
          ;; applied its own authored op so a re-homed bounce-back is deduped
          ;; (design §6).  A master journals plain tx bytes, unchanged.
          (when (peer-graph-p gr)
            (if *peer-rehome-op*
                ;; B2d-2: a hub re-home preserves the ORIGINAL op's identity on the
                ;; feed entry (design §5) so device E pulls it as the author's op and
                ;; the author dedups its own bounce-back; the re-home caller applies
                ;; the merge's per-field stamps, so finalize does not stamp here.
                (destructuring-bind (rop-id rorigin rlamport) *peer-rehome-op*
                  (write-sequence
                   (serialize-peer-meta rorigin rop-id rlamport +peer-op-authored+)
                   repl-stream)
                  (record-applied-op gr rop-id rlamport))
                (let ((op-id (gen-op-id))
                      (lamport (peer-next-lamport gr))
                      (origin (or (origin-id gr) +peer-null-origin+)))
                  (write-sequence
                   (serialize-peer-meta origin op-id lamport +peer-op-authored+)
                   repl-stream)
                  (record-applied-op gr op-id lamport)
                  ;; B2b: stamp every field this locally-authored op changed with
                  ;; (lamport . origin) -- the LWW basis a later concurrent edit
                  ;; from another replica compares against.
                  (dolist (w (writes transaction))
                    (let ((nid (id (node w))))
                      (dolist (slot (authored-changed-slots w))
                        (set-node-field-stamp gr nid slot lamport origin)))))))
          (write-sequence (bytes transaction) repl-stream)
          (finish-output repl-stream))))))

;;; Locking for object sets

(defun ordered-bucket-locks (lhash keys)
  "Return a fresh list of locks for LHASH in ascending bucket order."
  (let* ((level (read-lhash-level lhash))
         (buckets (mapcar (lambda (key)
                            (hash lhash level key))
                          keys)))
    (remove-duplicates
     (mapcar (lambda (bucket)
               (lookup-lhash-lock lhash bucket))
             (sort buckets #'<)))))


(defun tx-writes-locks (writes graph)
  (let ((vertexes '())
        (edges '()))
    (loop for write in writes
       for node = (node write)
       do
         (if (vertex-p node)
             (push (id node) vertexes)
             (push (id node) edges)))
    (let ((vertex-locks (ordered-bucket-locks (vertex-table graph) vertexes))
          (edge-locks (ordered-bucket-locks (edge-table graph) edges)))
      (nconc vertex-locks edge-locks))))

(defun call-with-locks (locks fun)
  (if (endp locks)
      (funcall fun)
      (with-lock ((first locks))
        (call-with-locks (cdr locks) fun))))




(defmacro ensure-transaction ((transaction-manager) &body body)
  `(if *transaction*
       (call-next-method)
       (with-transaction (,transaction-manager) ,@body )))

(defgeneric %create-node (node graph transaction)
  (:method (node graph transaction)
    (add-to-object-set (make-instance 'tx-create :node node)
                       (create-set *transaction*))
    node))

(defgeneric create-node (node graph)
  (:method (node graph)
    (%create-node node graph *transaction*)))

(defmethod create-node :around (node graph)
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))

(defgeneric copy-node (node)
  (:method ((node node))
    (maybe-init-node-data node)
    (let ((new-node (make-instance (type-of node)
                                   :id (slot-value node 'id)
                                   :type-id (slot-value node 'type-id)
                                   :revision (slot-value node 'revision)
                                   :deleted-p (slot-value node 'deleted-p)
                                   :written-p (slot-value node 'written-p)
                                   :data-pointer (slot-value node 'data-pointer))))
      (setf (data new-node) (copy-tree (slot-value node 'data)))
      ;; Copy bytes so maybe-init-node-data on the copy does not try to
      ;; re-read from data-pointer (which may be freed by a concurrent commit).
      (setf (bytes new-node) (bytes node))
      (if *transaction*
          (setf (gethash new-node (copies *transaction*)) node)
          (warn 'no-transaction-in-progress-warning))
      new-node)))

(defgeneric update-node (new-node graph)
  (:documentation
   "Persist NEW-NODE -- which must be a COPY (made with COPY inside the current
transaction) of an existing node -- recording the change in the transaction's
write set.  Prefer the SAVE method, which calls this.  Signals
NO-TRANSACTION-IN-PROGRESS outside a transaction, or MODIFYING-NON-COPY if
NEW-NODE was not produced by COPY.")
  (:method (new-node graph)
    ;; This does not automatically ensure a transaction, because you
    ;; have to COPY any node you want to modify within a transaction
    ;; anyway. That compound action inhibits auto-wrapping.
    (unless *transaction*
      (error 'no-transaction-in-progress))
    (let ((old-node (gethash new-node (copies *transaction*))))
      (unless old-node
        (error 'modifying-non-copy
               :node new-node))
      ;; Refresh the serialized bytes from the (modified) data: NEW-NODE is a
      ;; COPY that still carries the ORIGINAL node's bytes, and mutating a slot
      ;; updates DATA but not BYTES.  The write is serialized from BYTES into
      ;; both the .txn log and the replication stream, so without this the
      ;; logged/replicated update carries the OLD data (the master only looks
      ;; correct because apply-tx-write re-serializes its own copy locally).
      (setf (bytes new-node) (serialize (data new-node)))
      (add-to-object-set (make-instance 'tx-update
                                        :node new-node
                                        :old-node old-node)
                         (write-set *transaction*))
      new-node)))

(defgeneric delete-node (node graph)
  (:documentation
   "Soft-delete NODE from GRAPH within a transaction (auto-wrapping one if
needed): records a deletion of a copy with its deleted flag set, so the node
stops appearing in queries.  MARK-DELETED is the usual entry point.")
  (:method (node graph)
    (let ((old-node node)
          (new-node (copy node)))
      (setf (bytes new-node) (bytes old-node))
      (setf (deleted-p new-node) t)
      (add-to-object-set (make-instance 'tx-delete
                                        :old-node old-node
                                        :node new-node)
                         (write-set *transaction*)))))

(defmethod delete-node :around (node graph)
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))


(defclass transaction-manager ()
  ((sequence-number
    :initarg :sequence-number
    :accessor sequence-number
    :initform 0)
   (tx-id-counter
    :initarg :tx-id-counter
    :accessor tx-id-counter
    :initform 0)
   (transactions
    :initarg :transactions
    :reader transactions
    :initform (make-hash-table))
   (lock
    :initarg :lock
    :reader lock
    :initform (make-recursive-lock "transaction manager lock"))
   (replication-log-file
    :initarg :replication-log-file
    :accessor replication-log-file
    :initform nil)
   (replication-log
    :initarg :replication-log
    :accessor replication-log
    :documentation "An open stream appending to the replication log
    file."
    :initform nil)
   (replication-log-lock
    :initarg :replication-log-lock
    :reader replication-log-lock
    :initform (make-recursive-lock "replication log lock"))
   (graph
    :initarg :graph
    :reader graph
    :initform *graph*)
   ;; MVCC read-epoch pins: TOKEN -> epoch for each in-flight non-transactional
   ;; read.  REAP-SAFE-FLOOR folds in the minimum so the reaper never frees a
   ;; version a pinned reader could still dereference (the basis for dropping the
   ;; read-after-free finalizer).  Transactional reads need no pin -- their
   ;; start-tx-id already lower-bounds the floor.
   (read-pins
    :reader read-pins
    :initform (make-hash-table :test 'eql))
   (read-pins-lock
    :reader read-pins-lock
    :initform (make-recursive-lock "read-pins lock"))
   (read-pin-counter
    :accessor read-pin-counter
    :initform 0)))

(defmethod print-object ((transaction-manager transaction-manager) stream)
  (print-unreadable-object (transaction-manager stream :type t)
    (format stream "~D transaction~:P, sequence number ~D"
            (hash-table-count (transactions transaction-manager))
            (sequence-number transaction-manager))))

(defmethod initialize-instance :after ((instance transaction-manager)
                                       &key &allow-other-keys)
  (let* ((graph (graph instance))
         ;; A peer device applies pulled nodes at the HUB's epoch, so its counter
         ;; must dominate the pull-cursor (the pull frontier T >= every applied
         ;; epoch) as well as its own feed-seq -- else a local edit transaction
         ;; opens below a pulled node's epoch and MVCC-hides it (peer-observe-epoch).
         (tx-id-counter (1+ (max (load-highest-transaction-id graph)
                                 (if (peer-graph-p graph)
                                     (load-peer-pull-cursor graph)
                                     0)))))
    (setf (tx-id-counter instance) tx-id-counter)
    (setf (replication-log-file instance)
          (make-pathname :name (format nil "replication-~16,'0X"
                                       tx-id-counter)
                         :type "log"
                         :defaults (persistent-transaction-directory graph)))))

(defmethod replication-log-file ((graph graph))
  (replication-log-file (transaction-manager graph)))

(defgeneric init-replication-log (graph)
  (:method (graph)
    (let* ((transaction-manager (transaction-manager graph))
           (file (replication-log-file transaction-manager))
           (stream (open file
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :append
                         :if-does-not-exist :create
                         #+ccl :sharing #+ccl :lock)))
      (setf (replication-log (transaction-manager graph)) stream))))

(defgeneric close-replication-log (graph)
  (:method (graph)
    (let* ((transaction-manager (transaction-manager graph))
           (stream (replication-log transaction-manager)))
      (when (and (streamp stream)
                 (open-stream-p stream))
        (close stream)
        (setf (replication-log transaction-manager) nil)))))

(defgeneric call-with-transaction-manager-lock (fun transaction-manager)
  (:method (fun transaction-manager)
    (with-recursive-lock-held ((lock transaction-manager))
      (funcall fun))))

(defmacro with-transaction-manager-lock ((transaction-manager) &body body)
  `(call-with-transaction-manager-lock (lambda () ,@body)
                                       ,transaction-manager))

(defmethod call-with-transaction (fun transaction-manager)
  (let ((completed nil)
        (attempt-count 0))
    (flet ((call-transaction-fun ()
             (let ((*transaction* (create-transaction transaction-manager)))
               (unwind-protect
                    (prog1
                        (funcall fun)
                      (setf completed t))
                 (when completed
                   (funcall *end-of-transaction-action* *transaction*))
                 (cleanup-transaction *transaction*)))))
      (loop
         (when (<= *maximum-transaction-attempts* attempt-count)
           (with-transaction-manager-lock (transaction-manager)
             (return (call-transaction-fun))))
         (handler-case
             (return (call-transaction-fun))
           (validation-conflict ()
             (incf attempt-count)))))))

(defgeneric add-transaction (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (setf (gethash (sequence-number transaction)
                   (transactions transaction-manager))
          transaction)))

(defgeneric call-for-transactions (fun transaction-manager)
  (:method (fun (transaction-manager transaction-manager))
    (maphash (lambda (sequence-number tx)
               (declare (ignore sequence-number))
               (funcall fun tx))
             (transactions transaction-manager))))

(defmacro do-transactions ((transaction transaction-manager) &body body)
  `(call-for-transactions (lambda (,transaction) ,@body)
                          ,transaction-manager))

(defmacro do-committed-transactions ((transaction transaction-manager)
                                     &body body)
  `(call-for-transactions (lambda (,transaction)
                            (when (transaction-id ,transaction)
                              ,@body))
                          ,transaction-manager))

(defmacro do-active-transactions ((transaction transaction-manager)
                                  &body body)
  `(call-for-transactions (lambda (,transaction)
                            (when (eql (state ,transaction) :active)
                              ,@body))
                          ,transaction-manager))

(defmethod overlapping-transactions (transaction transaction-manager)
  (let ((start (start-tx-id transaction))
        (finish (finish-tx-id transaction))
        (result '()))
    (do-committed-transactions (tx transaction-manager)
      (when (<= start (transaction-id tx) finish)
        (push tx result)))
    result))

(defun minimum-start-transaction-id (transaction-manager)
  ;; Include :committing transactions, not just :active ones.  A :committing
  ;; transaction is blocked waiting for the TM lock but will validate against
  ;; committed transactions with tx-id >= its start-tx-id once it acquires the
  ;; lock.  If we used only :active starts, a newer :active transaction could
  ;; push the minimum high enough that prune-committed-transactions removes a
  ;; record the :committing thread needs, causing a silent lost update.
  (let (min)
    (do-transactions (tx transaction-manager)
      (when (or (eql (state tx) :active)
                (eql (state tx) :committing))
        (let ((start (start-tx-id tx)))
          (if min
              (setf min (min start min))
              (setf min start)))))
    min))

(defun prune-committed-transactions (transaction-manager)
  ;; Only prune when active transactions exist.  When every in-flight thread is
  ;; in :committing state, min-id is nil here; pruning everything then would
  ;; delete committed entries that the waiting committers still need for
  ;; validation (causing lost updates).
  (let ((min-id (minimum-start-transaction-id transaction-manager)))
    (when min-id
      (do-committed-transactions (tx transaction-manager)
        (when (< (transaction-id tx) min-id)
          (remove-transaction tx transaction-manager))))))

(defgeneric remove-transaction (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (remhash (sequence-number transaction)
             (transactions transaction-manager))))

(defgeneric next-sequence-number (transaction-manager)
  (:method (transaction-manager)
    (incf (sequence-number transaction-manager))))

(defmethod create-transaction (transaction-manager)
  (with-recursive-lock-held ((lock transaction-manager))
    (let* ((sequence-number (next-sequence-number transaction-manager))
           (graph (graph transaction-manager))
           (cache (cache graph))
           (tx (make-instance 'tx
                              :sequence-number sequence-number
                              :start-tx-id (tx-id-counter transaction-manager)
                              :finish-tx-id nil
                              :tx-id nil
                              :transaction-manager transaction-manager
                              :graph graph
                              :graph-cache cache)))
      (add-transaction tx transaction-manager)
      (setf (state tx) :active)
      tx)))

(defun call-with-read-snapshot (thunk &optional (graph *graph*))
  "Run THUNK with *TRANSACTION* bound to a fresh, read-only MVCC snapshot of
GRAPH, so every read THUNK performs resolves at one consistent epoch (a node
committed after the snapshot started is invisible).  The snapshot transaction is
registered active for THUNK's dynamic extent -- which holds the reaper's floor
so the observed versions are retained -- and is simply discarded on exit, never
validated or committed (a query writes nothing).  If a transaction is already
active, THUNK runs under it unchanged so an enclosing snapshot is inherited.  A
no-op (THUNK runs directly) when GRAPH has no transaction manager yet."
  (let ((tm (and graph
                 (slot-boundp graph 'transaction-manager)
                 (transaction-manager graph))))
    (if (or *transaction* (null tm))
        (funcall thunk)
        (let ((txn (create-transaction tm)))
          (unwind-protect
               (let ((*transaction* txn))
                 (funcall thunk))
            (remove-transaction txn tm))))))

(defmacro with-read-snapshot ((&optional (graph '*graph*)) &body body)
  "Evaluate BODY with reads pinned to a single consistent MVCC snapshot of GRAPH.
See CALL-WITH-READ-SNAPSHOT."
  `(call-with-read-snapshot (lambda () ,@body) ,graph))

;;; Commit sequence

(defvar *delete-committed-transaction-files* t)

(defun mark-as-committed (file)
  (if *delete-committed-transaction-files*
      (delete-file file)
      (let ((committed (make-pathname :type "committed" :defaults file)))
        (rename-file file committed))))

(defmethod %rollback ((tx tx))
  (unless (eql (state tx) :active)
    (error "Transaction ~A is not active" tx))
  (setf (state tx) :aborted))

(defmethod %commit ((tx tx))
  (when (eql (state tx) :active)
    (let ((tm (transaction-manager tx))
          (tmp nil)
          (renamed nil))
      (setf (state tx) :committing)
      (unwind-protect
           (progn
             ;; Serialize the transaction and write its log file BEFORE taking
             ;; the manager lock, so the bulk serialization + disk write are off
             ;; the serialized commit path (this is the main relief for the
             ;; commit-lock convoy on CCL/ECL).  Under the lock,
             ;; FINALIZE-TX-PERSISTENCE only renames it to its durable,
             ;; recovery-visible, id-keyed name (a single rename for a
             ;; non-replicated graph).
             (setf tmp (prepare-tx-persistence tx))
             (with-transaction-manager-lock (tm)
               ;; finish-tx-id must be set inside the manager lock so the overlap
               ;; window computed by validate is consistent with tx-id-counter.
               ;; Setting it outside would let concurrent commits advance the
               ;; counter between the read and the lock acquisition, making lost
               ;; updates invisible.
               (setf (finish-tx-id tx) (tx-id-counter tm))
               (unless (validate tx)
                 (error 'validation-conflict :transaction tx))
               ;; Unique constraints (issue #6): a pre-durability check under the same
               ;; manager lock -- a violation aborts before FINALIZE-TX-PERSISTENCE, so
               ;; nothing is journaled (the UNWIND-PROTECT below drops the temp file).
               (validate-unique-constraints tx (graph tx))
               (setf (transaction-id tx) (tx-id-counter tm))
               (incf (tx-id-counter tm))
               (prune-committed-transactions tm)
               ;; Cheap under the lock: rename temp to its final id-keyed name
               ;; (+ append replication log in commit order for masters).  Must
               ;; precede apply-transaction so the durable record exists before
               ;; the change is visible in memory.
               (finalize-tx-persistence tx tmp)
               (setf renamed t)
               ;; apply-transaction must run inside the manager lock so that:
               ;; (a) applies happen in tx-id order (no concurrent-apply race
               ;;     where a lower-tx-id apply overwrites a higher-tx-id apply);
               ;; (b) the graph cache is updated before the lock is released, so
               ;;     any transaction created after this commit (start-tx-id >
               ;;     this tx-id) reads the committed value rather than a stale
               ;;     pre-apply snapshot.
               (apply-transaction tx (graph tx)))
             (setf (state tx) :committed))
        ;; On validation conflict (retry) or any error before the rename, drop
        ;; the orphan temp file so prepared-but-not-committed attempts don't leak.
        (when (and tmp (not renamed))
          (ignore-errors (delete-file tmp)))))))

(defgeneric retain-committed-transaction-p (graph)
  (:documentation "When true, GRAPH keeps committed .txn files as its durable
journal instead of discarding them after apply.  On-disk graphs return NIL -- the
mmap heap is the durable copy, so the write-ahead entry is dropped once applied.
A memory-graph returns T: it has no heap, so the journal (compacted by a cl-store
image / snapshot at each clean close) is its only durable record.")
  (:method (graph) (declare (ignore graph)) nil))

(defmethod cleanup-transaction ((tx tx))
  (let ((transaction-manager (transaction-manager tx)))
    (if (eql (state tx) :committed)
        (unless (retain-committed-transaction-p (graph tx))
          (mark-as-committed (transaction-pathname tx)))
        (remove-transaction tx transaction-manager))))


(defun commit (&optional (transaction *transaction*))
  "Commit TRANSACTION (the current one by default), making its changes durable.
WITH-TRANSACTION commits automatically on normal exit, so this is rarely
called directly.  Signals NO-TRANSACTION-IN-PROGRESS if none is active."
  (unless *transaction*
    (error 'no-transaction-in-progress))
  (%commit transaction))

(defun rollback (&optional (transaction *transaction*))
  "Abort TRANSACTION (the current one by default), discarding its changes.
Signals NO-TRANSACTION-IN-PROGRESS if none is active."
  (unless *transaction*
    (error 'no-transaction-in-progress))
  (%rollback transaction))


;;; Recovering/restoring transactions

(defclass recovery-transaction ()
  ((transaction-id
    :initarg :transaction-id
    :accessor transaction-id)
   (writes
    :initarg :writes
    :accessor writes)))

(defun load-recovery-transaction (file)
  ;; The transaction-id is authoritative in the FILENAME (~16,'0X hex), set by the
  ;; rename in FINALIZE-TX-PERSISTENCE; the .txn header id is a deliberate
  ;; placeholder (0).  recovery-transaction-files already replays in filename
  ;; (= id) order.  See the TODO above FINALIZE-TX-PERSISTENCE.
  (let ((tx-header (load-tx-file file)))
    (make-instance 'recovery-transaction
                   :transaction-id (parse-integer (pathname-name file) :radix 16)
                   :writes (writes tx-header))))

(defmethod call-with-transaction-lock ((transaction recovery-transaction) fun)
  ;; No locking during recovery
  (funcall fun))

(defgeneric recovery-transaction-files (graph)
  (:method (graph)
    (let* ((directory (persistent-transaction-directory graph))
           (files (directory
                   (make-pathname :name :wild
                                  :type "txn"
                                  :defaults directory))))
      (sort files #'string< :key 'pathname-name))))

(defgeneric recovery-transactions (graph)
  (:method (graph)
    (mapcar 'load-recovery-transaction
            (recovery-transaction-files graph))))

(defgeneric recover-transactions (graph)
  (:method (graph)
    (dolist (transaction-file (recovery-transaction-files graph))
      (let ((transaction (load-recovery-transaction transaction-file))
            (*add-to-indexes-unless-present-p* t))
        (apply-transaction transaction graph)
        ;; A memory-graph keeps its journal until a clean-close checkpoint clears
        ;; it, so replay must not consume the tail (a crash between open and the
        ;; next checkpoint would otherwise lose it).
        (unless (retain-committed-transaction-p graph)
          (mark-as-committed transaction-file))))))

(defclass restore-transaction (recovery-transaction) ()
  (:default-initargs
   :writes nil))

(defmethod %create-node (node graph (transaction recovery-transaction))
  (push (make-instance 'tx-create :node node) (writes transaction)))

(defclass replicated-transaction (recovery-transaction)
  ((graph
    :initarg :graph
    :reader graph))
  (:default-initargs
   :writes nil))
