(in-package :graph-db)

;;; The skip list's node-cache is read/written lock-free by every concurrent
;;; reader and writer (READ-SKIP-NODE etc.).  On SBCL/CCL/LispWorks the cache is
;;; a per-op thread-safe hash table (:synchronized / :shared / :single-thread
;;; nil).  ECL hash tables have no such option, so concurrent access corrupts the
;;; table internally (rehash races) and crashes (SIGSEGV) under real concurrency.
;;; Guard the ECL node-cache with its own lock; a no-op everywhere else.
(defmacro with-sl-cache-lock ((skip-list) &body body)
  #+ecl `(mp:with-lock ((%sl-cache-lock ,skip-list)) ,@body)
  #-ecl `(progn ,@body))

;;; ECL cannot run the skip list's lock-free reads safely: it has no CAS for
;;; atomic pointer/value publication, and it cannot catch the SIGSEGV a torn read
;;; of a node's multi-byte size/pointers causes (SBCL/CCL/LispWorks recover via
;;; the GET-BYTES SEGV-retry :around and publish with real CAS).  So on ECL only,
;;; guard every PUBLIC skip-list operation with a per-skip-list READER/WRITER
;;; lock (a no-op everywhere else -- those keep the lock-free design): readers
;;; (FIND, cursor scans, MAP-*, COUNT) take a SHARED read lock and run
;;; concurrently; mutators (ADD/REMOVE/UPDATE/DELETE) take the EXCLUSIVE write
;;; lock.  A writer never runs concurrently with any reader, so no reader observes
;;; a half-written node -> torn-read safety holds; concurrent readers never mutate
;;; -> also safe.  This lets concurrent spatial-index queries run in parallel on
;;; ECL (the previous single recursive lock serialized readers too).
;;;
;;; The lock is taken ONCE at each public boundary and is NEVER nested -- nesting
;;; would deadlock this rw-lock: a write holder taking a read lock blocks on its
;;; own active writer, and a reader taking a second read lock can block behind a
;;; waiting writer while still holding the first.  So the internal navigation is
;;; lock-free: write ops call the %FIND-* cores, and the read-lock-held MAP-*
;;; scans step the lock-free %CURSOR-NEXT.  Views are unaffected: they reach skip
;;; lists under the view-group lock, so these locks are uncontended for them.
(defmacro with-sl-read-lock ((skip-list) &body body)
  #+ecl `(with-read-lock ((%sl-lock ,skip-list)) ,@body)
  #-ecl `(progn ,@body))

(defmacro with-sl-write-lock ((skip-list) &body body)
  #+ecl `(with-write-lock ((%sl-lock ,skip-list)) ,@body)
  #-ecl `(progn ,@body))

(define-condition skip-list-torn-read-error (error)
  ((addr :initarg :addr)
   (size :initarg :size)
   (max-len :initarg :max-len))
  (:report (lambda (error stream)
             (with-slots (addr size max-len) error
               (format stream
                       "Skip-list torn read at ~A: decoded node size ~A exceeds heap ~
extent ~A (a concurrent torn read of the node's size header)."
                       addr size max-len)))))

(define-condition skip-list-duplicate-error (error)
  ((key :initarg :key)
   (value :initarg :value)
   (skip-list :initarg :skip-list))
  (:report (lambda (error stream)
             (with-slots (key value skip-list) error
               (format stream
                       "~A already has node with key ~A and value ~A."
                       skip-list key value)))))

(define-condition skip-list-kv-not-found-error (error)
  ((key :initarg :key)
   (value :initarg :value))
  (:report (lambda (error stream)
             (with-slots (key value) error
               (format stream
                       "Could not find node with key ~A and value ~A in skip-list."
                       key value)))))

(alexandria:define-constant +max-level+ (the fixnum 64)
  :documentation
  "Maximum level of skip-list, should be enough for 2^64 elements.")
(alexandria:define-constant +skip-list-header-size+ 25
  :documentation
  "space for type byte, count, head and tail pointers")


(defmethod serialize-uuid ((id array))
  (subseq id 0 16))

(defmethod deserialize-uuid ((id array))
  (subseq id 0 16))

(defun random-level (&optional (max-level +max-level+))
  "Returns a random level for a new skip-list node, following Pugh's pattern of
L1: 50%, L2: 25%, L3: 12.5%, ..."
  (declare (optimize speed))
  (do ((level 1 (1+ level)))
      ((or (= level max-level)
           (= (random 4) 3))
       level)
    (declare (type fixnum level))))

(defun random-level-between (start &optional (max-level +max-level+))
  (declare (optimize speed))
  (do ((level start (1+ level)))
      ((or (= level max-level)
           (= (random 4) 3))
       level)
    (declare (type fixnum level))))

(defun hash-skip-key (key-bytes)
  (declare (type (array (unsigned-byte 8)) key-bytes))
  (let ((hash 5381))
    (dotimes (i (length key-bytes))
      (let ((item (elt key-bytes i)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    hash))

(defstruct (skip-node
             (:predicate skip-node-p)
             (:conc-name %sn-)
             (:constructor %make-skip-node))
  (addr 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64))
  (level 0 :type (unsigned-byte 8))
  key
  skey
  value
  svalue
  pointers
  flags
  (head-p nil)
  (tail-p nil))

(defun make-skip-node (skip-list key value level &optional pointers)
  (assert (and (>= level 0) (<= level 255)))
  (let* ((node (get-skip-node-buffer))
         (skey (funcall (%sl-key-serializer skip-list) key))
         (sval (funcall (%sl-value-serializer skip-list) value))
         (total-size (+ 8 ;; size (word)
                        1 ;; level
                        1 ;; flags: locked-p removed-p, fully-linked-p
                        (length skey)
                        (length sval)
                        (* level 8)))) ;; forward pointers (level * word size)
    (let* ((addr (allocate (%sl-heap skip-list) total-size))
           (offset addr))
      (dotimes (i 8)
        (set-byte (%sl-heap skip-list) offset (ldb (byte 8 (* i 8)) total-size))
        (incf offset))
      (set-byte (%sl-heap skip-list) offset level)  ;; level
      (set-byte (%sl-heap skip-list) (incf offset) 0) ;; flags
      (incf offset)
      (if pointers
          (dolist (p pointers)
            (dotimes (i 8)
              (set-byte (%sl-heap skip-list) offset (ldb (byte 8 (* i 8)) p))
              (incf offset)))
          (incf offset (* level 8))) ;; pointers
      (dotimes (i (length skey))
        (set-byte (%sl-heap skip-list) offset (aref skey i))
        (incf offset))
      (dotimes (i (length sval))
        (set-byte (%sl-heap skip-list) offset (aref sval i))
        (incf offset))
      (setf (%sn-addr node) addr
            (%sn-size node) total-size
            (%sn-key node) key
            (%sn-skey node) skey
            (%sn-value node) value
            (%sn-svalue node) sval
            (%sn-level node) level
            (%sn-flags node) 0
            (%sn-pointers node) (if pointers
                                    (make-array level :element-type 'word
                                                :initial-contents pointers)
                                    (make-array level :element-type  'word)))
      (with-sl-cache-lock (skip-list)
        (setf (gethash addr (%sl-node-cache skip-list)) node)))))

(defun set-node-pointer (skip-list node level addr)
  (let ((offset (+ (%sn-addr node) 10 (* level 8))))
    (dotimes (i 8)
      (set-byte (%sl-heap skip-list) offset (ldb (byte 8 (* i 8)) addr))
      (incf offset))
    (setf (aref (%sn-pointers node) level) addr)))

(defun read-skip-flags (skip-list node)
  (let ((flags (get-byte (%sl-heap skip-list) (+ (%sn-addr node) 9))))
    (setf (%sn-flags node) flags)
    flags))

(defun mark-node (skip-list node)
  (let ((flags (read-skip-flags skip-list node)))
    (setf flags (dpb 1 (byte 1 0) flags))
    (set-byte (%sl-heap skip-list) (+ (%sn-addr node) 9) flags)
    (setf (%sn-flags node) flags)))

(defun %sn-marked-p (skip-list node)
  (let ((flags (read-skip-flags skip-list node)))
    (ldb-test (byte 1 0) flags)))

(defun set-node-fully-linked (skip-list node)
  (let ((flags (read-skip-flags skip-list node)))
    (setf flags (dpb 1 (byte 1 1) flags))
    (set-byte (%sl-heap skip-list) (+ (%sn-addr node) 9) flags)
    (setf (%sn-flags node) flags)))

(defun unset-node-fully-linked (skip-list node)
  (let ((flags (read-skip-flags skip-list node)))
    (setf flags (dpb 0 (byte 1 1) flags))
    (set-byte (%sl-heap skip-list) (+ (%sn-addr node) 9) flags)
    (setf (%sn-flags node) flags)))

(defun %sn-fully-linked-p (skip-list node)
  (let ((flags (read-skip-flags skip-list node)))
    (ldb-test (byte 1 1) flags)))

(defun read-uint64-from-seq (seq)
  (let ((int 0))
    (dotimes (i 8)
      (setq int (dpb (aref seq i)
                     (byte 8 (* i 8)) int)))
    int))

(defun read-skip-node-bytes (skip-list addr)
  (let ((size 0))
    (dotimes (i 8)
      (setq size (dpb (get-byte (%sl-heap skip-list) (+ i addr))
                      (byte 8 (* i 8)) size)))
    ;; Defense-in-depth on ECL: a torn read of the 8-byte size header (during
    ;; concurrent writes) yields a garbage size; GET-BYTES would then read
    ;; out-of-bounds and SIGSEGV in ECL's raw c-inline deref (which ECL cannot
    ;; catch/retry, unlike SBCL/CCL's GET-BYTES :around).  WITH-SL-LOCK should
    ;; prevent torn reads, but validate the size against the heap extent and
    ;; signal a recoverable error rather than crash if one slips through.  NOT on
    ;; SBCL/CCL: there torn reads are transient and recovered by the SEGV-retry
    ;; :around (re-read once the writer finishes); erroring here would break that.
    #+ecl
    (let ((max-len (memory-size (%sl-heap skip-list))))
      (when (or (not (plusp size)) (> (+ addr size) max-len))
        (error 'skip-list-torn-read-error :addr addr :size size :max-len max-len)))
    (let ((bytes (get-bytes (%sl-heap skip-list) addr size)))
      (values bytes size))))

(defun read-skip-node (skip-list addr)
  (if (= addr 0)
      nil
      (or (and *cache-enabled*
               (with-sl-cache-lock (skip-list)
                 (gethash addr (%sl-node-cache skip-list))))
          (let ((node (get-skip-node-buffer)) (pointer 8))
            (setf (%sn-addr node) addr)
            ;;(log:debug "READING SKIP NODE BYTES AT ~S" addr)
            (multiple-value-bind (bytes size)
                (read-skip-node-bytes skip-list addr)
              ;;(log:debug "READ ~S: ~S" size bytes)
              (setf (%sn-size node) size
                    (%sn-level node) (aref bytes pointer)
                    (%sn-flags node) (aref bytes (incf pointer))
                    (%sn-pointers node) (make-array (%sn-level node)
                                                    :element-type 'word))
              (incf pointer)
              (dotimes (i (%sn-level node))
                (let ((p (read-uint64-from-seq (subseq bytes pointer))))
                  ;;(log:debug "READING POINTER (~S OF ~S) => ~S"
                  ;;i (1- (%sn-level node)) p)
                  (setf (aref (%sn-pointers node) i) p)
                  (incf pointer 8)))
              ;;(log:debug "READ POINTERS, OFFSET IS ~S" pointer)
              (multiple-value-bind (key length)
                  (funcall (%sl-key-deserializer skip-list) (subseq bytes pointer))
                ;;(log:debug "GOT KEY ~S OF LEN ~S" key length)
                (let ((value (funcall (%sl-value-deserializer skip-list)
                                      (subseq bytes (+ pointer length)))))
                  (setf (%sn-key node) key
                        (%sn-value node) value)
                  (with-sl-cache-lock (skip-list)
                    (setf (gethash addr (%sl-node-cache skip-list))
                          node)))))))))

(defstruct
    (skip-list
      (:predicate skip-list-p)
      (:conc-name %sl-)
      (:constructor %make-skip-list)
      (:print-function
       (lambda (sl stream depth)
         (declare (ignore depth))
         (format stream "#<SKIP-LIST OF LENGTH ~A, EQUAL-FUNC: ~A, DUPLICATES ~A>"
                 (%sl-length sl) (%sl-key-equal sl)
                 (if (%sl-duplicates-allowed-p sl) "ALLOWED" "NOT ALLOWED")))))
  heap
  mmap
  address
  head
  head-pointer
  tail
  tail-pointer
  (max-level +max-level+)
  (key-equal '=)
  (value-equal 'uuid-array-equal)
  (comparison '>)
  (duplicates-allowed-p nil)
  (length 0 :type (unsigned-byte 64))
  (key-serializer 'serialize)
  (key-deserializer 'deserialize)
  (value-serializer 'identity)
  (value-deserializer 'identity)
  (node-cache
   #+sbcl (make-hash-table :test 'eq :weakness :value :synchronized t)
   #+lispworks (make-hash-table :test 'eq :weak-kind :value :single-thread nil)
   #+ccl (make-hash-table :test 'eq :weak :value :shared t)
   #+ecl (make-hash-table :test 'eq :weakness :value))
  ;; ECL hash tables aren't thread-safe and have no :synchronized option, so the
  ;; node-cache above needs an explicit lock (see WITH-SL-CACHE-LOCK).
  #+ecl (cache-lock (mp:make-lock))
  ;; ECL-only: a per-skip-list reader/writer lock (see WITH-SL-READ-LOCK /
  ;; WITH-SL-WRITE-LOCK).  Shared among concurrent readers, exclusive for writers.
  #+ecl (lock (make-rw-lock))
  (length-lock #+sbcl (sb-thread:make-mutex)
               #+lispworks (mp:make-lock)
               #+ccl (ccl:make-lock)
               #+ecl (mp:make-lock :recursive t))
  (locks (map-into (make-array 1000)
                   #+ccl 'ccl:make-lock
                   #+lispworks 'mp:make-lock
                   ;; Recursive: LOCK-SKIP-NODE on ECL always grabs (no
                   ;; inner-lock-p owner check like the #+sbcl branch), so a
                   ;; node lock may be re-entered by the same thread.
                   #+ecl (lambda () (mp:make-lock :recursive t))
                   #+sbcl 'sb-thread:make-mutex)))

(defun make-head (skip-list &key key value)
  (let ((node (make-skip-node skip-list key value (%sl-max-level skip-list))))
    (setf (%sn-head-p node) t
          (%sl-head-pointer skip-list) (%sn-addr node)
          (%sl-head skip-list) node)
    node))

(defun make-tail (skip-list head &key key value)
  (let ((node (make-skip-node skip-list key value (%sl-max-level skip-list))))
    (setf (%sn-tail-p node) t
          (%sl-tail-pointer skip-list) (%sn-addr node)
          (%sl-tail skip-list) node)
    (dotimes (i (%sl-max-level skip-list))
      (set-node-pointer skip-list head i (%sn-addr node)))
    node))

(defmethod serialize-skip-list-header ((heap memory) (address integer) (skip-list skip-list))
  #+sbcl (declare (type sb-ext:word address))
  (set-byte heap address +skip-list+)
  (serialize-uint64 heap (%sl-length skip-list) (1+ address))
  (serialize-uint64 heap (%sl-head-pointer skip-list) (+ 1 8 address))
  (serialize-uint64 heap (%sl-tail-pointer skip-list) (+ 1 16 address))
  (+ +skip-list-header-size+ address))

(defmethod deserialize-skip-list-header ((heap memory) (address integer))
  #+sbcl (declare (type sb-ext:word address))
  (let ((type-byte (get-byte heap address)))
    (declare (type (integer 0 255) type-byte))
    (unless (= type-byte +skip-list+)
      (error "Not a skip list at address ~S" address))
    (values
     ;; length
     (deserialize-uint64 heap (1+ address))
     ;; head-pointer
     (deserialize-uint64 heap (+ 1 8 address))
     ;; tail-pointer
     (deserialize-uint64 heap (+ 1 16 address)))))

(defun make-skip-list (&key heap key-equal key-comparison head-key head-value
                       tail-key tail-value key-serializer key-deserializer
                       value-serializer value-deserializer duplicates-allowed-p
                       value-equal)
  (let* ((address (allocate heap +skip-list-header-size+))
         (sl (%make-skip-list :heap heap
                              :address address
                              :key-equal key-equal
                              :comparison key-comparison
                              :mmap (memory-mmap heap)
                              :key-serializer key-serializer
                              :key-deserializer key-deserializer
                              :value-serializer value-serializer
                              :value-deserializer value-deserializer
                              :duplicates-allowed-p duplicates-allowed-p
                              :value-equal value-equal)))
    (make-head sl :key head-key :value head-value)
    (make-tail sl (%sl-head sl) :key tail-key :value tail-value)
    (serialize-skip-list-header heap address sl)
    sl))

(defun open-skip-list (&key address heap key-equal key-comparison key-serializer
                       key-deserializer value-serializer value-deserializer
                       duplicates-allowed-p value-equal)
  (multiple-value-bind (length head-pointer tail-pointer)
      (deserialize-skip-list-header heap address)
    (let ((sl (%make-skip-list :heap heap
                               :address address
                               :key-equal key-equal
                               :comparison key-comparison
                               :mmap (memory-mmap heap)
                               :key-serializer key-serializer
                               :key-deserializer key-deserializer
                               :value-serializer value-serializer
                               :value-deserializer value-deserializer
                               :duplicates-allowed-p duplicates-allowed-p
                               :value-equal value-equal)))
      (let ((node (read-skip-node sl head-pointer)))
        (setf (%sn-head-p node) t
              (%sl-head-pointer sl) (%sn-addr node)
              (%sl-head sl) node))
      (let ((node (read-skip-node sl tail-pointer)))
        (setf (%sn-tail-p node) t
              (%sl-tail-pointer sl) (%sn-addr node)
              (%sl-tail sl) node))
      (setf (%sl-length sl) length)
      sl)))

(defun close-skip-list (skip-list)
  (declare (ignore skip-list))
  nil)

(defun delete-skip-list (skip-list)
  (with-sl-write-lock (skip-list)
    (let ((pred (%sl-head skip-list))
          (address-list (list (%sn-addr (%sl-head skip-list)))))
      (loop
         (let ((node (read-skip-node skip-list (aref (%sn-pointers pred) 0))))
           (push (%sn-addr node) address-list)
           (if (= (%sn-addr node) (%sn-addr (%sl-tail skip-list)))
               (return)
               (setq pred node))))
      (dolist (addr address-list)
        (free (%sl-heap skip-list) addr))
      (free (%sl-heap skip-list) (%sl-address skip-list))
      (setf (%sl-address skip-list) nil
            (%sl-heap skip-list) nil
            (%sl-mmap skip-list) nil
            (%sl-locks skip-list) #())
      (with-sl-cache-lock (skip-list)
        (clrhash (%sl-node-cache skip-list)))
      nil)))

(defun incf-skip-list-count (skip-list)
  (with-recursive-lock-held ((%sl-length-lock skip-list))
    (setf (%sl-length skip-list)
          (incf-uint64 (%sl-heap skip-list)
                       (1+ (%sl-address skip-list))))))

(defun decf-skip-list-count (skip-list)
  (with-recursive-lock-held ((%sl-length-lock skip-list))
    (setf (%sl-length skip-list)
          (decf-uint64 (%sl-heap skip-list)
                       (1+ (%sl-address skip-list))))))

(defun lock-skip-node (skip-list node &key (waitp t) timeout)
  (let ((mutex (aref (%sl-locks skip-list)
                     (mod (%sn-addr node) (length (%sl-locks skip-list))))))
    #+sbcl
    (let ((inner-lock-p (eq (sb-thread::mutex-owner mutex)
                            sb-thread:*current-thread*))
          (got-it nil))
      (sb-sys:without-interrupts
        (if (or inner-lock-p
                (setf got-it (sb-sys:allow-with-interrupts
                               (sb-thread:grab-mutex mutex
                                                     :waitp waitp
                                                     :timeout timeout))))
            mutex)))
    #+lispworks (progn (mp:process-lock mutex nil timeout)
                       mutex)
    #+ccl
    (and (ccl:grab-lock mutex) mutex)
    #+ecl
    (and (mp:get-lock mutex waitp) mutex)))

(defun unlock-skip-node (skip-list lock)
  (declare (ignore skip-list))
  #+ccl
  (ccl:release-lock lock)
  #+lispworks
  (mp:process-unlock lock)
  #+sbcl
  (sb-thread:release-mutex lock)
  #+ecl
  (mp:giveup-lock lock))

(defun %find-in-skip-list (skip-list key &optional preds succs)
  ;; Lock-free core; caller holds the sl read or write lock (WITH-SL-READ-LOCK).
  (let ((the-node nil) (pred (%sl-head skip-list)) (level-found -1))
    (loop for level from (1- (%sl-max-level skip-list)) downto 0 do
         (let ((curr (read-skip-node skip-list (aref (%sn-pointers pred) level))))
           (loop while (and
                        (not (= (%sn-addr curr)
                                (%sn-addr (%sl-tail skip-list))))
                        (funcall (%sl-comparison skip-list)
                                 (%sn-key curr) key))
              do
              ;;(log:debug "~S < ~S" (%sn-key curr) key)
              (setq pred
                    curr
                    curr
                    (read-skip-node skip-list (aref (%sn-pointers pred) level))))
           (when (and (not the-node)
                      (funcall (%sl-key-equal skip-list) key (%sn-key curr)))
             (setq the-node curr
                   level-found level))
           (if (and the-node (not preds) (not succs))
               (return)
               (when (and preds succs)
                 (setf (aref preds level) pred
                       (aref succs level) curr)))))
    (values the-node level-found preds succs)))

;; Generic so a memory-graph's MEM-SKIP-LIST can supply its own (used by
;; map-reduce views); the on-disk (heap) list is the method below.
(defgeneric find-in-skip-list (skip-list key &optional preds succs))

(defmethod find-in-skip-list ((skip-list skip-list) key &optional preds succs)
  (with-sl-read-lock (skip-list)
    (%find-in-skip-list skip-list key preds succs)))

(defun %find-kv-in-skip-list (skip-list key value &optional preds succs)
  ;; Lock-free core; caller holds the sl read or write lock (WITH-SL-READ-LOCK).
  ;; NOTE: PRED is re-initialized to the head at EACH level (rather than carried
  ;; down as in %FIND-IN-SKIP-LIST).  That is deliberate and required for the
  ;; duplicate-key case: a taller tower node that shares KEY but has a different
  ;; value would make a carried PRED overshoot the specific (key,value) target at
  ;; the base level.  Duplicate-free lists take the O(log n) fast path in
  ;; REMOVE-FROM-SKIP-LIST instead of this function.
  (let ((the-node nil) (level-found -1))
    (loop for level from (1- (%sl-max-level skip-list)) downto 0 do
         (let ((pred (%sl-head skip-list)))
           (let ((curr (read-skip-node skip-list (aref (%sn-pointers pred) level))))
             (loop while (and
                          (not (= (%sn-addr curr)
                                  (%sn-addr (%sl-tail skip-list))))
                          (or
                           (funcall (%sl-comparison skip-list)
                                    (%sn-key curr) key)
                           (and (funcall (%sl-key-equal skip-list) key (%sn-key curr))
                                (not (funcall (%sl-value-equal skip-list) value (%sn-value curr))))))
                do
                (setq pred
                      curr
                      curr
                      (read-skip-node skip-list (aref (%sn-pointers pred) level))))
             (when (and (not the-node)
                        (funcall (%sl-key-equal skip-list) key (%sn-key curr))
                        (funcall (%sl-value-equal skip-list) value (%sn-value curr)))
               (setq the-node curr
                     level-found level)
               (log:debug "FOUND ~A/~A AT LEVEL ~A" key value level-found))
             (if (and the-node (not preds) (not succs))
                 (return)
                 (when (and preds succs)
                   (setf (aref preds level) pred
                         (aref succs level) curr))))))
    (values the-node level-found preds succs)))

(defun find-kv-in-skip-list (skip-list key value &optional preds succs)
  (with-sl-read-lock (skip-list)
    (%find-kv-in-skip-list skip-list key value preds succs)))

;; Generic so an alternative ordered-map backend (the in-RAM MEM-SKIP-LIST used
;; by a memory-graph) can supply its own insert; the on-disk (heap) list is the
;; method below.  Consumers -- views, spatial index -- call this unchanged.
(defgeneric add-to-skip-list (skip-list key value))

(defmethod add-to-skip-list ((skip-list skip-list) key value)
  (with-sl-write-lock (skip-list)
    (log:debug "ADDING ~A/~A TO ~A" key value skip-list)
    (let ((top-level (random-level (%sl-max-level skip-list)))
          (preds (make-array (%sl-max-level skip-list)))
          (succs (make-array (%sl-max-level skip-list))))
      (loop
         (let ((node (%find-in-skip-list skip-list key preds succs)))
           (when node
             (log:debug "WORKING ON ~A" node)
             (when (not (%sn-marked-p skip-list node))
               (loop until (%sn-fully-linked-p skip-list node) do
                    #+ccl (ccl:process-allow-schedule)
                    #+lispworks (mp:yield)
                    #+sbcl (sb-thread:thread-yield)
                    #+ecl (thread-yield))
               (unless (%sl-duplicates-allowed-p skip-list)
                 ;;(error 'skip-list-duplicate-error
                 ;;:skip-list skip-list :key key :value value)
                 (let ((*print-pretty* nil))
                   (log:error "ATTEMPT TO INSERT DUP KV '~A/~A' IN ~A" key value skip-list))
                 (return-from add-to-skip-list nil)))))
         (log:debug "~S / ~S:~%  ~S~%  ~S~%" key value (elt preds 0) (elt succs 0))
         (let ((locks nil) pred succ prev-pred (valid-p t))
           (unwind-protect
                (progn
                  (loop for level from 0 to (1- top-level) while valid-p do
                       (setq pred (aref preds level)
                             succ (aref succs level))
                       (when (or (null prev-pred)
                                 (and prev-pred
                                      (/= (%sn-addr pred) (%sn-addr prev-pred))))
                         (let ((lock (lock-skip-node skip-list pred :waitp t)))
                           (if lock
                               (push lock locks)
                               (error "Unable to acquire skip-node lock for ~A" pred)))
                         (setq prev-pred pred))
                       (setq valid-p (and (not (%sn-marked-p skip-list pred))
                                          (not (%sn-marked-p skip-list succ))
                                          (= (aref (%sn-pointers pred) level)
                                             (%sn-addr succ)))))
                  (when valid-p
                    (let ((node (make-skip-node skip-list key value top-level)))
                      (log:debug "Adding ~A" node)
                      (loop for level from 0 to (1- top-level) do
                           (log:debug "Setting pointer at level ~A" level)
                           (set-node-pointer skip-list node level
                                             (%sn-addr (aref succs level)))
                           (set-node-pointer skip-list (aref preds level) level
                                             (%sn-addr node)))
                      (log:debug "Setting ~A to fully linked" node)
                      (set-node-fully-linked skip-list node)
                      (log:debug "Updating list count for ~A" skip-list)
                      (incf-skip-list-count skip-list)
                      (return-from add-to-skip-list node))))
             (dolist (lock (nreverse locks))
               (if lock
                   (unlock-skip-node skip-list lock)
                   (log:info "SKIP-LIST: Got null lock in ~A / ~A" key value)))))))))

(defgeneric update-in-skip-list (skip-list key value &optional old-value))

(defmethod update-in-skip-list ((skip-list skip-list) key value &optional old-value)
  (with-sl-write-lock (skip-list)
    (let ((lock nil))
      (let ((node (%find-in-skip-list skip-list key)))
        (if node
            (unwind-protect
                 (progn
                   (setq lock (lock-skip-node skip-list node :waitp t))
                   ;;(update-skip-node-value skip-list node value)
                   (let* ((skey (if (%sn-skey node)
                                    (%sn-skey node)
                                    (setf (%sn-skey node)
                                          (funcall (%sl-key-serializer skip-list)
                                                   (%sn-key node)))))
                          (old-sval (funcall (%sl-value-serializer skip-list) old-value))
                          (sval (funcall (%sl-value-serializer skip-list) value)))
                     (if (<= (length sval) (length old-sval))
                         (progn
                           #+sbcl (sb-ext:cas (%sn-value node) (%sn-value node) value)
                           #+sbcl (sb-ext:cas (%sn-svalue node) (%sn-svalue node) sval)
                           #+lispworks (sys:compare-and-swap (%sn-value node) (%sn-value node) value)
                           #+lispworks (sys:compare-and-swap (%sn-svalue node) (%sn-svalue node) sval)
                           #+ccl (ccl::conditional-store (%sn-value node) (%sn-value node) value)
                           #+ccl (ccl::conditional-store (%sn-svalue node) (%sn-svalue node) sval)
                           #+ecl (setf (%sn-value node) value)
                           #+ecl (setf (%sn-svalue node) sval)
                           (let* ((offset (+ (%sn-addr node)
                                             8 1 1
                                             (length skey)
                                             (* (%sn-level node) 8))))
                             (dotimes (i (length sval))
                               (set-byte (%sl-heap skip-list) offset (aref sval i))
                               (incf offset))
                             (with-sl-cache-lock (skip-list)
                               (setf (gethash (%sn-addr node) (%sl-node-cache skip-list)) node))))
                         (progn
                           ;;(unlock-skip-node skip-list lock)
                           ;;(setq lock nil)
                           (remove-from-skip-list skip-list key)
                           (let ((new-node (add-to-skip-list skip-list key value)))
                             new-node)))))
              (when lock
                (unlock-skip-node skip-list lock)))
            (return-from update-in-skip-list
              (add-to-skip-list skip-list key value)))))))

(defun ok-to-delete-p (skip-list node level)
  (and (%sn-fully-linked-p skip-list node)
       (= (%sn-level node) (1+ level))
       (not (%sn-marked-p skip-list node))))

(defgeneric remove-from-skip-list (skip-list key &optional value))

(defmethod remove-from-skip-list ((skip-list skip-list) key
                                  &optional (value nil value-supplied-p))
  "Remove a node from SKIP-LIST.  With VALUE, removes the specific key/value
pair (needed for duplicate-key lists); without it, removes one occurrence of
KEY (arbitrary when duplicates exist).  Returns T if a node was removed, NIL if
none matched."
  (with-sl-write-lock (skip-list)
    (let ((node-to-delete nil) (marked-p nil) (top-level -1)
          (preds (make-array (%sl-max-level skip-list)))
          (succs (make-array (%sl-max-level skip-list)))
          (lock nil)
          ;; Target a SPECIFIC node and use find-kv-in-skip-list's preds/succs,
          ;; which are positioned for that exact (key,value) node.
          ;; find-in-skip-list returns the tallest-tower duplicate but positions
          ;; preds/succs at the LEFTMOST key match, so its level-0 predecessor
          ;; points before a DIFFERENT node than the one being deleted -- the
          ;; resulting splice skips (orphans) the intervening duplicates.  For a
          ;; key-only removal we first read any matching node's value so we can
          ;; then target it precisely with find-kv.
          (target-value
           (if value-supplied-p
               value
               (let ((n (%find-in-skip-list skip-list key)))
                 (if n (%sn-value n) (return-from remove-from-skip-list nil))))))
      (unwind-protect
           (loop
              (multiple-value-bind (node level-found)
                  ;; Duplicate-free lists (views, unique indexes): KEY alone
                  ;; identifies the node, so %FIND-IN-SKIP-LIST descends in
                  ;; O(log n) and returns correct preds/succs (leftmost match ==
                  ;; the only match).  %FIND-KV re-walks the base list from the
                  ;; head at every level -- O(n) -- and is only needed to pick a
                  ;; specific value out of DUPLICATE keys.  Confirm the located
                  ;; node's value still matches the target before deleting.
                  (if (%sl-duplicates-allowed-p skip-list)
                      (%find-kv-in-skip-list skip-list key target-value preds succs)
                      (multiple-value-bind (n lvl)
                          (%find-in-skip-list skip-list key preds succs)
                        (if (and n (funcall (%sl-value-equal skip-list)
                                            target-value (%sn-value n)))
                            (values n lvl)
                            (values nil -1))))
                (unless node (return-from remove-from-skip-list nil))
                (when (or marked-p
                          (and node (ok-to-delete-p skip-list node level-found)))
                  (when (not marked-p)
                    (setq node-to-delete node
                          top-level (%sn-level node)
                          lock (lock-skip-node skip-list node :waitp t))
                    (when (%sn-marked-p skip-list node-to-delete)
                      (return-from remove-from-skip-list nil))
                    (mark-node skip-list node-to-delete)
                    (setq marked-p t))
                  (let ((locks nil) pred succ prev-pred (valid-p t))
                    (unwind-protect
                         (progn
                           (loop for level from 0 to (1- top-level) while valid-p do
                                (setq pred (aref preds level)
                                      succ (aref succs level))
                                (when (or (null prev-pred)
                                          (and prev-pred (/= (%sn-addr pred)
                                                             (%sn-addr prev-pred))))
                                  (push (lock-skip-node skip-list pred :waitp t) locks)
                                  (setq prev-pred pred))
                                (setq valid-p (and (not (%sn-marked-p skip-list pred))
                                                   (= (aref (%sn-pointers pred) level)
                                                      (%sn-addr succ)))))
                           (when valid-p
                             (loop for level from (1- top-level) downto 0 do
                                  (set-node-pointer
                                   skip-list
                                   (aref preds level)
                                   level
                                   (aref (%sn-pointers node-to-delete) level)))
                             (decf-skip-list-count skip-list)
                             (with-sl-cache-lock (skip-list)
                               (remhash (%sn-addr node-to-delete)
                                        (%sl-node-cache skip-list)))
                             (free (%sl-heap skip-list) (%sn-addr node-to-delete))
                             (unlock-skip-node skip-list lock)
                             (setq lock nil)
                             (return-from remove-from-skip-list t)))
                      (dolist (lock (nreverse locks))
                        (unlock-skip-node skip-list lock)))))))
        (when lock
          (unlock-skip-node skip-list lock))))))

(defun node-forward (skip-list node)
  (unless (= 0 (aref (%sn-pointers node) 0))
    (read-skip-node skip-list (aref (%sn-pointers node) 0))))

(defun skip-list-count (skip-list)
  (with-sl-read-lock (skip-list)
    (let ((pred (%sl-head skip-list)) (count 0))
      (loop
         (let ((node (read-skip-node skip-list (aref (%sn-pointers pred) 0))))
           (if (= (%sn-addr node) (%sn-addr (%sl-tail skip-list)))
               (return)
               (progn
                 (incf count)
                 ;; Advance the cursor; without this the loop re-reads the
                 ;; first node forever (infinite loop).
                 (setq pred node)))))
      count)))

(defun analyze-sl-heights (skip-list)
  (with-sl-read-lock (skip-list)
    (let ((pred (%sl-head skip-list))
          (heights (make-array (list +max-level+) :initial-element 0)))
      (loop
         for node = (read-skip-node skip-list (aref (%sn-pointers pred) 0))
         until (= (%sn-addr node) (%sn-addr (%sl-tail skip-list)))
           do
           (incf (aref heights (1- (%sn-level node))))
           (setq pred node))
      heights)))

(defun skip-list-to-list (skip-list)
  (with-sl-read-lock (skip-list)
    (let ((pred (%sl-head skip-list)))
      (loop
         for node = (read-skip-node skip-list (aref (%sn-pointers pred) 0))
         until (= (%sn-addr node) (%sn-addr (%sl-tail skip-list)))
         collecting
           (prog1
               (cons (%sn-key node) (%sn-value node))
             (setq pred node))))))

(defun skip-list-to-node-list (skip-list)
  (with-sl-read-lock (skip-list)
    (let ((pred (%sl-head skip-list)))
      (loop
         for node = (read-skip-node skip-list (aref (%sn-pointers pred) 0))
         until (= (%sn-addr node) (%sn-addr (%sl-tail skip-list)))
         collecting
           (prog1
               node
             (setq pred node))))))

(defun sl-test ()
  (let* ((heap (create-memory "/var/tmp/sl.dat" (* 1024 1024)))
         (sl (%make-skip-list :heap heap
                              :mmap (memory-mmap heap))))
    (unwind-protect
         (let* ((head (make-head sl
                                 :key most-negative-fixnum :value +null-key+))
                (tail (make-tail sl head
                                 :key most-positive-fixnum :value +null-key+))
                (ids (loop for i from 1 to 1000000 collecting (gen-id)))
                (i 0))
           (declare (ignore tail))
           (time
            (dolist (id ids)
              (add-to-skip-list sl i id)
              (incf i)))
           (log:debug "~A" (time (find-in-skip-list sl 500)))
           (let ((list (time (skip-list-to-list sl))))
             (log:debug "SL SIZE: ~A (should be ~A)" (length list) (%sl-length sl))
             (log:debug "~A" (find-in-skip-list sl 0)))
           (log:debug "DELETING")
           (time (remove-from-skip-list sl 500))
           (log:debug "FOUND: ~A" (find-in-skip-list sl 500))
           (length (skip-list-to-list sl))
           (room)
           )
      (progn
        (close-memory (%sl-heap sl))
        (delete-file "/var/tmp/sl.dat")
        ))))

(defun string-uuid (uuid)
  (with-output-to-string (out)
    (map nil
         (lambda (byte)
           (format out "~(~2,'0X~)" byte))
         uuid)))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
     do (rotatef (elt sequence (random i))
                 (elt sequence (1- i))))
  sequence)

(defun sl-perf-test (&optional (data-set-size 10000))
  (let* ((heap (create-memory "/var/tmp/sl.dat" (* 1024 1024 100))))
    (unwind-protect
         (let ((sl (make-skip-list :heap heap
                                   :head-key (format nil "~A" (code-char 0))
                                   :head-value +null-key+
                                   :tail-key (format nil "~A" (code-char 65535))
                                   :tail-value +max-key+
                                   :key-equal 'string=
                                   :key-comparison 'string<
                                   :key-serializer 'serialize
                                   :key-deserializer 'deserialize
                                   :value-serializer 'serialize
                                   :value-deserializer 'deserialize)))
           (let* ((ids (make-array (list data-set-size)
                                   :initial-contents
                                   (loop for i from 1 to data-set-size collecting (string-uuid (gen-id)))))
                  (keys (make-array (list data-set-size)
                                    :initial-contents
                                    (nshuffle (loop for x from 0 below data-set-size
                                                 collecting (format nil "~12,'0d" x))))))
             (log:debug "ABOUT TO START ADDING ITEMS")
             (let ((start (get-internal-real-time)))
               (dotimes (i data-set-size)
                 (add-to-skip-list sl
                                   (svref keys i)
                                   (svref ids i)))
               (log:debug "ADDED ~A nodes in ~F"
                          data-set-size
                          (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND)))
             (let ((start (get-internal-real-time)))
               (let ((list (skip-list-to-list sl)))
                 (log:debug "DUMPED LIST IN ~F"
                            (/ (- (get-internal-real-time) start) INTERNAL-TIME-UNITS-PER-SECOND))
                 (log:debug "SL SIZE: ~A (should be ~A)" (length list) (%sl-length sl))
                 (log:debug "HEIGHTS: ~A" (analyze-sl-heights sl))
                 list))))
      (progn
        (if (memory-magic-byte-corrupt-p heap)
            (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap)))
        (close-memory heap)
        (delete-file "/var/tmp/sl.dat")))))

(defun sl-string-test ()
  (let* ((heap (create-memory "/var/tmp/sl.dat" (* 1024 1024 100))))
    (unwind-protect
         (let ((sl (make-skip-list :heap heap
                                   :head-key (format nil "~A" #\Null)
                                   :head-value +null-key+
                                   :tail-key (format nil "~A" (code-char 65535))
                                   :tail-value +max-key+
                                   :key-equal 'string=
                                   :key-comparison 'string<
                                   :key-serializer 'serialize
                                   :key-deserializer 'deserialize
                                   :value-serializer 'serialize
                                   :value-deserializer 'deserialize)))
           (format t "SKIP-LIST: ~S~%" sl)
           (let* ((ids (loop for i from 1 to 1000 collecting (string-uuid (gen-id))))
                  (i 0))
             (log:debug "ABOUT TO START ADDING ITEMS")
             (let ((start (get-internal-real-time)))
              (dolist (id ids)
                (log:debug "ADDING ~S:~S" id i)
                (add-to-skip-list sl (format nil "~12,'0d" i) id)
                (when (memory-magic-byte-corrupt-p heap)
                  (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap))
                  (error "Corrupt heap!"))
                (incf i))
              (log:debug "ADDED 1000 nodes in ~A" (- (get-internal-real-time) start)))
             (log:debug "~A" (find-in-skip-list sl (format nil "~12,'0d" 500)))
             (let ((start (get-internal-real-time)))
               (let ((list (skip-list-to-list sl)))
                 (log:debug "SL SIZE: ~A (should be ~A)" (length list) (%sl-length sl))
                 ;;(log:debug "~A" (find-in-skip-list sl (format nil "~12,'0d" 0)))
                 (log:debug "DUMPED LIST IN ~A" (- (get-internal-real-time) start))))
             (dolist (i '(1 500 100 999))
               (let ((node (find-in-skip-list sl (format nil "~12,'0d" i)))
                     (new-id (format nil "NEW~A" (string-uuid (gen-id)))))
                 (log:debug "~S~%" node)
                 (log:debug "REPLACING ~S~%   WITH ~S" (%sn-value node) new-id)
                 ;;(log:debug "~S" (update-in-skip-list sl (%sn-key node) new-id))
                 (log:debug "REMOVE: ~S" (remove-from-skip-list sl (%sn-key node)))
                 ;;(log:debug "~{~A~^~%~}~%" (skip-list-to-list sl))
                 (sleep 1)
                 (when (memory-magic-byte-corrupt-p heap)
                   (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap))
                   (error "Corrupt heap!"))
                 (log:debug "FIND: ~S" (find-in-skip-list sl (format nil "~12,'0d" i)))
                 (log:debug "~S" sl)
                 (sleep 1)))))
      (progn
        (if (memory-magic-byte-corrupt-p heap)
            (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap)))
        (close-memory heap)
        (delete-file "/var/tmp/sl.dat")))))

(defun sl-dup-test ()
  (let* ((heap (create-memory "/var/tmp/sl.dat" (* 1024 1024 100))))
    (unwind-protect
         (let ((sl (make-skip-list :heap heap
                                   :duplicates-allowed-p t
                                   :head-key (format nil "~A" #\Null)
                                   :head-value most-negative-fixnum
                                   :tail-key (format nil "~A" (code-char 65535))
                                   :tail-value most-positive-fixnum
                                   :key-equal 'string=
                                   :key-comparison 'string<
                                   :key-serializer 'serialize
                                   :key-deserializer 'deserialize
                                   :value-equal 'equal
                                   :value-serializer 'serialize
                                   :value-deserializer 'deserialize)))
           (log:debug "ADDING ITEMS")
           (time
            (dotimes (i 5)
              (format t ".")
              (dotimes (j 20)
                (add-to-skip-list sl (format nil "~12,'0d" i) j))))
           (terpri)
           (values
            (mapcar (lambda (node)
                      (list (%sn-key node)
                            (%sn-value node)
                            :level (%sn-level node)))
                    (skip-list-to-node-list sl))
            (format nil "000000000003 -> ~A" (find-in-skip-list sl "000000000003"))
            (format nil "000000000003:0 -> ~A" (find-kv-in-skip-list sl "000000000003" 0))
            (format nil "000000000003:10 -> ~A" (find-kv-in-skip-list sl "000000000003" 10))
            (format nil "000000000003:19 -> ~A" (find-kv-in-skip-list sl "000000000003" 19))
            (analyze-sl-heights sl)
            ))
      (progn
        (close-memory heap)
        (delete-file "/var/tmp/sl.dat")))))

(defun sl-corruption-test ()
  (let* ((heap (create-memory "/var/tmp/sl.dat" (* 1024 1024 100))))
    (unwind-protect
         (let ((sl (make-skip-list :heap heap
                                   :head-key (format nil "~A" #\Null)
                                   :head-value +null-key+
                                   :tail-key (format nil "~A" (code-char 65535))
                                   :tail-value +max-key+
                                   :key-equal 'string=
                                   :key-comparison 'string<
                                   :key-serializer 'serialize
                                   :key-deserializer 'deserialize
                                   :value-serializer 'serialize
                                   :value-deserializer 'deserialize)))
           (let* ((ids (loop for i from 1 to 10 collecting (string-uuid (gen-id))))
                  (i 0))
             (log:debug "ABOUT TO START ADDING ITEMS")
             (time
              (dolist (id ids)
                (log:debug "ADDING ~S:~S" id i)
                (add-to-skip-list sl (format nil "~12,'0d" i) id)
                (when (memory-magic-byte-corrupt-p heap)
                  (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap))
                  (error "Corrupt heap!"))
                (incf i)))
             (log:debug "~A" (time (find-in-skip-list sl (format nil "~12,'0d" 5))))
             (let ((list (time (skip-list-to-list sl))))
               (log:debug "SL SIZE: ~A (should be ~A)" (length list) (%sl-length sl))
               ;;(log:debug "~A" (find-in-skip-list sl (format nil "~12,'0d" 0)))
               )
             (terpri)
             (dolist (i '(1 5 9))
               (let ((node (find-in-skip-list sl (format nil "~12,'0d" i)))
                     (new-id (format nil "NEW~A" (string-uuid (gen-id)))))
                 (log:debug "~S~%" node)
                 (log:debug "REPLACING ~S~%   WITH ~S" (%sn-value node) new-id)
                 ;;(log:debug "~S" (update-in-skip-list sl (%sn-key node) new-id))
                 (log:debug "REMOVE: ~S" (remove-from-skip-list sl (%sn-key node)))
                 ;;(log:debug "~{~A~^~%~}~%" (skip-list-to-list sl))
                 (thread-yield)
                 (sleep 5)
                 (when (memory-magic-byte-corrupt-p heap)
                   (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap))
                   (error "Corrupt heap!"))
                 (log:debug "FIND: ~S" (find-in-skip-list sl (format nil "~12,'0d" i)))
                 (log:debug "~S" sl)
                 (sleep 1)))))
      (progn
        (if (memory-magic-byte-corrupt-p heap)
            (log:debug "Heap corrupt: 0x~X" (memory-magic-byte-corrupt-p heap)))
        (close-memory heap)
        (delete-file "/var/tmp/sl.dat")))))
