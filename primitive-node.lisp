(in-package :graph-db)

;; v2 head (MVCC): flags(1) type-id(2) revision(4) data-pointer(8)
;;                 commit-epoch(8) prev-pointer(8) = 31.
;; The first 15 bytes are byte-identical to the v1 head (append-only), so a v1
;; reader is just "stop after data-pointer".  +edge-header-size+ derives from this.
(alexandria:define-constant +node-header-size+ 31)

;; Byte offset of the prev-pointer field WITHIN a head (after flags(1) type-id(2)
;; revision(4) data-pointer(8) commit-epoch(8) = 23).  The reaper patches this
;; field in place (in the lhash live head or an archived heap head) to sever a
;; reclaimed version chain.
(alexandria:define-constant +node-prev-pointer-offset+ 23)

(defgeneric node-p (thing)
  (:method ((thing node)) t)
  (:method (thing) nil))

(defmethod print-object ((node node) stream)
  (format stream "#<~A ~S REV ~S>"
          (type-of node) (string-id (id node))
          (revision node)))

(defmethod flags-as-int ((n node))
  (let ((flags 0))
    (when (slot-value n 'deleted-p)
      (setq flags (dpb 1 (byte 1 0) flags)))
    (when (slot-value n 'written-p)
      (setq flags (dpb 1 (byte 1 1) flags)))
    (when (slot-value n 'heap-written-p)
      (setq flags (dpb 1 (byte 1 2) flags)))
    (when (slot-value n 'type-idx-written-p)
      (setq flags (dpb 1 (byte 1 3) flags)))
    (when (slot-value n 'views-written-p)
      (setq flags (dpb 1 (byte 1 4) flags)))
    (when (slot-value n 've-written-p)
      (setq flags (dpb 1 (byte 1 5) flags)))
    (when (slot-value n 'vev-written-p)
      (setq flags (dpb 1 (byte 1 6) flags)))
    flags))

(defmethod save-node-flags ((table lhash) (node node))
  (lhash-custom-update table
                       (lambda (mf offset)
                         (let ((flags (flags-as-int node)))
                           (set-byte mf offset flags)))
                       (id node)))

;; Little-endian (un)pack into a PLAIN byte vector.  The head codecs build the
;; whole head in such a vector and move it to/from the mmap with ONE
;; SET-BYTES/GET-BYTES, instead of per-byte SET-BYTE/GET-BYTE calls -- each of
;; which establishes the mmap SEGV-retry handler and dispatches a generic
;; function.  Heads are (de)serialized on every node write, so collapsing 31/71
;; per-byte calls into one batched transfer is the dominant write-path win.
(declaim (inline pack-uint))
(defun pack-uint (vec offset value nbytes)
  "Write VALUE little-endian into VEC[OFFSET..]; return the next free index."
  (dotimes (i nbytes)
    (setf (aref vec (+ offset i)) (ldb (byte 8 (* i 8)) value)))
  (+ offset nbytes))

(defun pack-node-head (vec i n)
  "Fill the node head of N into VEC starting at index I; return the next index."
  (setf (aref vec i) (flags-as-int n))
  (setq i (pack-uint vec (1+ i) (type-id n)      2))
  (setq i (pack-uint vec i       (revision n)     4))
  (setq i (pack-uint vec i       (data-pointer n) 8))
  (setq i (pack-uint vec i       (commit-epoch n) 8))   ;; MVCC v2
  (setq i (pack-uint vec i       (prev-pointer n) 8))   ;; MVCC v2
  i)

(defun serialize-node-head (mf n offset)
  (let ((vec (make-byte-vector +node-header-size+)))
    (pack-node-head vec 0 n)
    (set-bytes mf vec offset +node-header-size+)
    ;; Return the index of the LAST byte written (start + size - 1); the edge
    ;; codec resumes from (1+ this) to position from/to/weight.
    (+ offset (1- +node-header-size+))))

(defun deserialize-node-head (mf offset)
  (let ((flags (get-byte mf offset)))
    (values
     (ldb-test (byte 1 0) flags) ;; deleted-p
     (ldb-test (byte 1 1) flags) ;; written-p
     (ldb-test (byte 1 2) flags) ;; heap-written-p
     (ldb-test (byte 1 3) flags) ;; type-idx-written-p
     (ldb-test (byte 1 4) flags) ;; views-written-p
     (ldb-test (byte 1 5) flags) ;; ve-written-p
     (ldb-test (byte 1 6) flags) ;; vev-written-p
     (let ((int 0)) ;; type-id
       (declare (type (integer 0 65535) int))
       (dotimes (i 2)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; revision
       (declare (type (integer 0 4294967295) int))
       (dotimes (i 4)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; data-pointer
       #+sbcl (declare (type sb-ext:word int))
       (dotimes (i 8)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; commit-epoch (MVCC v2)
       #+sbcl (declare (type sb-ext:word int))
       (dotimes (i 8)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; prev-pointer (MVCC v2)
       #+sbcl (declare (type sb-ext:word int))
       (dotimes (i 8)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     offset)))

;; The head reader the vertex/edge codecs dispatch through.  Normally the v2
;; (31-byte) reader; MIGRATE-GRAPH rebinds it to DESERIALIZE-NODE-HEAD-V1 to read
;; a pre-MVCC (v1, 15-byte) graph so its data can be logically backed up + replayed.
(defvar *node-head-reader* 'deserialize-node-head)

(defun deserialize-node-head-v1 (mf offset)
  "Read a pre-MVCC (v1) 15-byte node head: flags(1) type-id(2) revision(4)
data-pointer(8).  Returns the same value shape as DESERIALIZE-NODE-HEAD with
commit-epoch and prev-pointer forced to 0 and OFFSET stopped after data-pointer,
so the edge codec positions from/to/weight at their v1 offsets."
  (let ((flags (get-byte mf offset)))
    (values
     (ldb-test (byte 1 0) flags) ;; deleted-p
     (ldb-test (byte 1 1) flags) ;; written-p
     (ldb-test (byte 1 2) flags) ;; heap-written-p
     (ldb-test (byte 1 3) flags) ;; type-idx-written-p
     (ldb-test (byte 1 4) flags) ;; views-written-p
     (ldb-test (byte 1 5) flags) ;; ve-written-p
     (ldb-test (byte 1 6) flags) ;; vev-written-p
     (let ((int 0)) ;; type-id
       (declare (type (integer 0 65535) int))
       (dotimes (i 2)
         (setq int (dpb (get-byte mf (incf offset)) (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; revision
       (declare (type (integer 0 4294967295) int))
       (dotimes (i 4)
         (setq int (dpb (get-byte mf (incf offset)) (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; data-pointer
       #+sbcl (declare (type sb-ext:word int))
       (dotimes (i 8)
         (setq int (dpb (get-byte mf (incf offset)) (byte 8 (* i 8)) int)))
       int)
     0      ;; commit-epoch (v1 has none)
     0      ;; prev-pointer (v1 has none)
     offset)))

(defun finalize-node (node table graph)
  (setf (written-p node) t)
  (save-node-flags table node)
  (setf (gethash (id node) (cache graph)) node))

;;; FIXME: reenable when deallocation bug is fixed
;;; (declaim (inline maybe-init-node-data))
(defun ensure-node-bytes (node graph)
  "Copy NODE's heap data bytes into its BYTES slot if not already present, and
return NODE.  Under MVCC this is the materialization the read paths perform
WHILE A READ PIN (or the reading transaction's start-tx-id) protects NODE's data
block, so the bytes are captured before the pin is released and the node can
escape self-contained.  Deserialize stays lazy (MAYBE-INIT-NODE-DATA).  This
replaced the lhash value-finalizer (which copied under the bucket lock)."
  (when (node-p node)
    (let ((dp (data-pointer node)))
      (when (and (> dp 0)
                 (or (eq (bytes node) :init) (null (bytes node))))
        (setf (bytes node)
              (read-bytes (make-mpointer :mmap (memory-mmap (heap graph))
                                         :loc dp))))))
  node)

(defvar *initializing-node* nil
  "Bound true around CHANGE-CLASS of a node under construction (see
CHANGE-NODE-CLASS).  A node's user-defined slot VALUES live in the DATA alist,
not in real CLOS slots; CHANGE-CLASS re-runs slot initialization and, on some
implementations (notably ECL), invokes SLOT-MAKUNBOUND-USING-CLASS on those
persistent slots.  That must NOT destroy the DATA we just populated, so while
this is bound the persistent branch of SLOT-MAKUNBOUND-USING-CLASS defers to the
standard (no-op-on-the-alist) method.  Explicit user SLOT-MAKUNBOUND -- this
unbound -- still drops the alist entry.

Why it matters under concurrency: clearing a freshly-created node's DATA leaves
the cached node with DATA = NIL but BYTES intact, so the first concurrent readers
all race to lazily re-materialize DATA from BYTES (MAYBE-INIT-NODE-DATA mutating
the shared cached node) -- on ECL that surfaced as transient NIL slot reads.

It ALSO guards MAYBE-INIT-NODE-DATA: a node being re-typed under construction
(e.g. DESERIALIZE-VERTEX-HEAD on a wire/disk record) still carries the AUTHOR's
DATA-POINTER, a heap address meaningless in THIS image.  On ECL, CHANGE-CLASS
reads a persistent slot during re-init -> NODE-SLOT-VALUE -> MAYBE-INIT-NODE-DATA,
which would dereference that foreign pointer into the local heap and deserialize
garbage.  So MAYBE-INIT-NODE-DATA no-ops while this is bound; the data is set
explicitly afterward (the transaction-node path) or materialized lazily later
(the lhash path, when this is NIL and the pointer is local).")

(defun maybe-init-node-data (node &key (graph *graph*))
  (when (and (not *initializing-node*)
             (> (data-pointer node) 0))
    ;; Ensure the raw bytes are present.  The read paths (lookup-object's bare
    ;; branch and the map-vertices/map-edges scans) already materialized them via
    ;; ENSURE-NODE-BYTES under a read pin, and transactional reads are covered by
    ;; the transaction's start-tx-id, so for nodes obtained through those paths
    ;; this bare read is a no-op (bytes already present).  It remains only as a
    ;; fallback for nodes materialized off those paths.
    (when (or (eq (bytes node) :init) (null (bytes node)))
      (setf (bytes node)
            (read-bytes (make-mpointer
                         :mmap (memory-mmap (heap graph))
                         :loc (data-pointer node)))))
    ;; Deserialize lazily from the in-memory bytes (safe; *graph* is bound here).
    (when (and (null (data node))
               (bytes node)
               (not (eq (bytes node) :init)))
      (setf (data node) (deserialize (bytes node)))))
  node)

(defmethod lookup-node ((table lhash) (key array) (graph graph))
  (or (and *cache-enabled*
           (let ((node (gethash key (cache graph))))
             (when node
               (record-graph-read graph)
               node)))
      (let ((node (lhash-get table key)))
        (when (node-p node)
          (setf (id node) key)
          ;; we should wait to do this until we need the data.
          ;;(maybe-init-node-data node :graph graph)
          (when *cache-enabled*
            (setf (gethash key (cache graph)) node))
          (record-graph-read graph)
          node))))

(defun save-node (node table &key (graph *graph*))
  ;; you must copy the node before writing to its slots,
  ;; in case others are reading it!
  ;;(log:info "SAVING ~A" (string-id node))
  (let ((old-node nil))
    (when (plusp (data-pointer node))
      (if (data node)
          (setf (bytes node) (serialize (data node)))
          (maybe-init-node-data node :graph graph))
      (let ((addr (allocate (heap graph) (length (bytes node)))))
        (dotimes (i (length (bytes node)))
          (set-byte (heap graph)
                    (+ i addr) (aref (bytes node) i)))
        (setf (data-pointer node) addr)))
    (with-locked-hash-key (table (id node))
      ;; Assure that the old node is available in cache for readers
      (setq old-node (%lhash-get table (id node)))
      (setf (id old-node) (id node))
      ;;(log:info "~A OLD ADDRESS IS ~A" (string-id node) (data-pointer old-node))
      ;;(log:info "~A NEW ADDRESS IS ~A" (string-id node) (data-pointer node))
      (maybe-init-node-data old-node :graph graph)
      (setf (gethash (id old-node) (cache graph)) old-node)
      ;;(log:info "OLD REV OF ~A CACHED" (string-id old-node))
      ;; Make sure we are modifying the latest revision or bail
      (unless (= (revision node) (revision old-node))
        ;; Undo data slot saving
        (log:error "REVISION ERROR ~A: ~A /= ~A"
                   (string-id node) (revision node) (revision old-node))
        (when (/= 0 (data-pointer node))
          (free (heap graph) (data-pointer node)))
        (error 'stale-revision-error
               :current-revision (revision old-node)
               :instance (string-id node)))
      ;; Save our changes
      ;; Don't overflow the revision (32 bit int)
      (if (>= (revision node) 4294967295)
          (setf (revision node) 0)
          (incf (revision node)))
      ;;(log:info "UPDATING LHASH FOR ~A" (string-id node))
      (%lhash-update table (id node) node)
      ;;(log:info "RECORDING WRITE FOR ~A" (string-id node))
      (record-graph-write graph)
      ;;(log:info "CACHING NEW REV OF ~A" (string-id node))
      (setf (gethash (id node) (cache graph)) node))
    (when (/= 0 (data-pointer old-node))
      (let ((pointer (data-pointer old-node))
            ;;(old-id (string-id old-node))
            (heap (heap graph)))
        ;;(log:info "~A FREEING ~A IN ~A" old-id pointer heap)
        (free heap pointer)
        ;; Remove from heap upon gc
        ;;(sb-ext:finalize old-node
        ;;                 (lambda ()
        ;;                   (let ((*package* (find-package :graph-db)))
        ;;                     (log:debug "~A FREEING ~A IN ~A" old-id pointer heap)
        ;;                     (free heap pointer))))
        ))
    ;;(log:info "SAVED ~A" (string-id node))
    (values node old-node)))

(defmethod string-id ((id array))
  (with-output-to-string (out)
    (map nil
         (lambda (byte)
           (format out "~(~2,'0X~)" byte))
         id)))

(defmethod string-id ((node node))
  (with-output-to-string (out)
    (map nil
         (lambda (byte)
           (format out "~(~2,'0X~)" byte))
         (id node))))

(defun node-to-alist (node &key slots)
  (maybe-init-node-data node)
  (remove-if (lambda (pair)
               (member (car pair) slots))
             (append (list (cons :id (string-id node)))
                     (copy-alist (data node)))))

(defun node-slot-value (node key &key (graph *graph*))
  ;;(log:info "GETTING ~A FOR ~A" key (string-id node))
  (maybe-init-node-data node :graph graph)
  (when (consp (data node))
    (cdr (assoc (if (keywordp key)
                    key
                    (intern (symbol-name key) :keyword))
                (data node)))))

(defsetf node-slot-value (node key) (value)
  (with-gensyms (pair keyword)
    `(if (typep ,value 'graph-db::node)
         (error "Cannot set ~A slots to objects with parent type NODE" (type-of ,node))
         (let ((,keyword (if (keywordp ,key)
                             ,key
                             (intern (symbol-name ,key) :keyword))))
           (maybe-init-node-data ,node)
           (cond ((null (data ,node))
                  (push (cons ,keyword ,value) (data ,node)))
                 ((consp (data ,node))
                  (let ((,pair (assoc ,keyword (data ,node))))
                    (if ,pair
                        (setf (cdr ,pair) ,value)
                        (push (cons ,keyword ,value) (data ,node)))))
                 (t
                  (error "Cannot set slot value when data slot is of type ~A"
                         (type-of (data ,node)))))))))

(defun node-slot-boundp (node key &key (graph *graph*))
  "True if NODE's persistent slot KEY has a stored value -- i.e. an entry is
present in the data alist.  This is presence, NOT non-NIL: a slot explicitly set
to NIL is bound.  Materializes the data first (mirrors NODE-SLOT-VALUE)."
  (maybe-init-node-data node :graph graph)
  (let ((keyword (if (keywordp key) key (intern (symbol-name key) :keyword))))
    (and (consp (data node))
         (assoc keyword (data node))
         t)))

(defmethod slot-value-using-class :around ((class node-class) instance slot)
  "Around method that is alternate-version aware and will show values for the current,
   working private version of instance."
  (log:trace "slot-value-using-class~%  '~A'~%  '~A'" class (slot-definition-name slot))
  (let* (#+lispworks(slot (closer-mop::find-slot slot class))
         (slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((member slot-name (persistent-slot-names class))
           ;; FIXME: Check for txn and give current revision's value
           (node-slot-value instance slot-keyword-name))
          ((member slot-name (ephemeral-slot-names class))
           ;; FIXME: Check for txn and give current revision's value
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

(defmethod (setf slot-value-using-class) :around
    (new-value (class node-class) instance slot)
  "Is alternate-version aware and will update values for the current, working private
   version of instance."
  ;;(log:debug "setf slot-value-using-class~%  '~A'~%  '~A'~%  '~A'" new-value class
  ;;(sb-pcl:slot-definition-name slot))
  (let* (#+lispworks(slot (closer-mop::find-slot slot class))
         (slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((member slot-name (persistent-slot-names class))
           ;; FIXME: Check for txn and handle
           (setf (node-slot-value instance slot-keyword-name) new-value))
          ((member slot-name (ephemeral-slot-names class))
           ;; FIXME: Check for txn and handle
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

;; *INITIALIZING-NODE* is defvar'd above MAYBE-INIT-NODE-DATA (it guards that
;; function too); see there for the full rationale.

(defmacro change-node-class (node subclass)
  "CHANGE-CLASS NODE to SUBCLASS with *INITIALIZING-NODE* bound, so the
re-initialization CLOS performs does not destroy NODE's alist-backed persistent
slot values (see *INITIALIZING-NODE*)."
  `(let ((*initializing-node* t))
     (change-class ,node ,subclass)))

(defmethod slot-boundp-using-class :around ((class node-class) instance slot)
  "Persistent slot VALUES live in the node's DATA alist, not in the real CLOS
slot (which is always unbound), so SLOT-BOUNDP must consult the alist for them.
Ephemeral and meta slots ARE real CLOS slots -- defer to the standard method."
  (let* (#+lispworks(slot (closer-mop::find-slot slot class))
         (slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((member slot-name (persistent-slot-names class))
           (node-slot-boundp instance slot-keyword-name))
          ((member slot-name (ephemeral-slot-names class))
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

(defmethod slot-makunbound-using-class :around ((class node-class) instance slot)
  "Unbind a persistent slot by dropping its DATA-alist entry (the real CLOS slot
is already unbound, so the standard method would be a no-op on the value).
Ephemeral and meta slots -> standard method.  While *INITIALIZING-NODE* is bound
(CHANGE-NODE-CLASS), the persistent branch defers to the standard method so the
re-initialization CHANGE-CLASS performs does not clear the alist-backed slot
values we just set."
  (let* (#+lispworks(slot (closer-mop::find-slot slot class))
         (slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((and (member slot-name (persistent-slot-names class))
                (not *initializing-node*))
           (maybe-init-node-data instance)
           (setf (data instance)
                 (delete slot-keyword-name (data instance) :key #'car))
           instance)
          ((member slot-name (ephemeral-slot-names class))
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

(defgeneric node-equal (x y)
  (:method ((x node) (y node))
    (equalp (id x) (id y)))
  (:method (x y)
    nil))

(defmethod %hash ((key #.(class-of (make-array 16 :element-type '(unsigned-byte 8)))))
  (declare (optimize (speed 3) (safety 0)))
  (let ((int (aref key 0)))
    (loop
       for subscript from 1 to 15
       for shift from 8 by 8
       do
         (incf int (ash (logand (aref key subscript) #xFF) shift)))
    int))

(defun sxhash-node (node) (sxhash (%hash (id node))))
#+sbcl (sb-ext:define-hash-table-test node-equal sxhash-node)
(defun make-node-table (&key weakness synchronized)
  #+ccl
  (make-hash-table :test 'node-equal
                   :hash-function 'sxhash-node
                   :weak weakness          ; CCL spells it :weak (cf. make-id-table)
                   :shared synchronized)
  #+lispworks
  (make-hash-table :test 'node-equal
                   :hash-function 'sxhash-node
                   :weak-kind weakness
                   :single-thread (not synchronized))
  #+sbcl
  (make-hash-table :test 'node-equal
                   :weakness weakness
                   :synchronized synchronized)
  ;; ECL has no custom hash-table tests; NODE-EQUAL is (equalp (id x) (id y)),
  ;; so this table is keyed by node-id (a byte vector) under EQUALP instead of
  ;; by the node object.  Callers must key by (id node) on ECL (see UNIQUE/1).
  #+ecl
  (make-hash-table :test 'equalp :weakness weakness))

(defun id-equal (x y) (equalp x y))
(defun sxhash-id-array (id) (sxhash (%hash id)))
#+sbcl (sb-ext:define-hash-table-test id-equal sxhash-id-array)
(defun make-id-table (&key weakness synchronized)
  #+lispworks
  (make-hash-table :test 'id-equal
                   :hash-function 'sxhash-id-array
                   :weak-kind weakness
                   :single-thread (not synchronized))

  #+ccl
  (make-hash-table :test 'id-equal
                   :hash-function 'sxhash-id-array
                   :weak weakness
                   :shared synchronized)
  #+sbcl
  (make-hash-table :test 'id-equal
                   :weakness weakness
                   :synchronized synchronized)
  ;; ID-EQUAL is just EQUALP, and ECL's native EQUALP tables compare byte
  ;; vectors by content, so EQUALP is an exact substitute here.
  #+ecl
  (make-hash-table :test 'equalp :weakness weakness))
