(in-package :graph-db)

(defstruct (type-index
             (:constructor %make-type-index)
             (:print-function
              (lambda (i s d)
                (declare (ignore d))
                (format s "#<TYPE-INDEX ~A" (type-index-table i)))))
  table
  ;; The allocator heap these index-lists live in.  Held so the lazy %TI-LIST
  ;; (#46) can deserialize without needing *GRAPH* bound (e.g. in unit tests).
  heap
  (locks (map-into (make-array +max-node-types+)
                   #+ccl 'make-lock
                   #+lispworks 'mp:make-lock
                   #+ecl 'mp:make-lock
                   #+sbcl 'sb-thread:make-mutex))
  (cache
   #+sbcl (make-hash-table :test 'eq :synchronized t)
   #+lispworks (make-hash-table :test 'eq :single-thread nil)
   #+ccl (make-hash-table :test 'eq :shared t)
   #+ecl (make-hash-table :test 'eq))
  ;; ECL only: the cache is populated LAZILY (see %TI-LIST / #46) rather than
  ;; eagerly for all +MAX-NODE-TYPES+ types at open, so it is written at runtime
  ;; on first touch.  ECL hash tables aren't synchronized, so guard it.
  #+ecl (cache-lock (mp:make-lock)))

(defun make-type-index (location heap)
  (let* ((table (mmap-file location
                           :size (* +max-node-types+ +index-list-bytes+)))
         (idx (%make-type-index :table table :heap heap)))
    #+ecl
    ;; Lazy (#46): initialize the on-disk slots to empty index-lists but do NOT
    ;; populate the cache -- %TI-LIST materializes each type on first touch.  One
    ;; reusable empty list serialized to every slot (head=0), so no 65536 live
    ;; index-lists here.
    (let ((empty (make-index-list heap)))
      (dotimes (i +max-node-types+)
        (serialize-index-list table empty (* i +index-list-bytes+))))
    #-ecl
    (dotimes (i +max-node-types+)
      (let ((offset (* i +index-list-bytes+)))
        (let ((index-list (make-index-list heap)))
          (serialize-index-list table index-list offset)
          (setf (gethash i (type-index-cache idx)) index-list))))
    idx))

(defun open-type-index (location heap)
  (let* ((table (mmap-file location :create-p nil))
         (idx (%make-type-index :table table :heap heap)))
    #+ecl
    ;; Lazy (#46): do NOT deserialize an index-list for every one of the 65536
    ;; possible types at open (that was ~100 MB of live structs on ECL for a
    ;; schema using a few dozen types).  %TI-LIST deserializes on first touch.
    (progn)
    #-ecl
    (dotimes (i +max-node-types+)
      (let ((offset (* i +index-list-bytes+)))
        (let ((index-list (deserialize-index-list table offset)))
          (setf (gethash i (type-index-cache idx)) index-list))))
    idx))

(defun %ti-list (idx type-id)
  "The index-list for TYPE-ID in IDX.  On ECL the cache is lazy (#46): return the
cached list or deserialize-and-cache it on first touch, guarding the
unsynchronized ECL cache table.  On other impls the cache is fully populated at
make/open, so this is a plain lookup."
  #+ecl
  (with-lock ((type-index-cache-lock idx))
    (or (gethash type-id (type-index-cache idx))
        (setf (gethash type-id (type-index-cache idx))
              (deserialize-index-list (type-index-table idx)
                                      (* type-id +index-list-bytes+)
                                      (type-index-heap idx)))))
  #-ecl
  (gethash type-id (type-index-cache idx)))

(defmethod close-type-index ((index type-index))
  (munmap-file (type-index-table index) :save-p t))

(defgeneric add-to-type-index (node graph &key unless-present))
(defgeneric remove-from-type-index (node graph))

(defmethod type-index-push ((uuid array) (type-id integer) (idx type-index)
                            &key unless-present)
  (let ((lock (aref (type-index-locks idx) type-id)))
    (with-lock (lock)
      (let ((il (%ti-list idx type-id)))
        (if unless-present
            (index-list-pushnew uuid il)
            (index-list-push uuid il))
        ;; FIXME: could be optimized to only write the new head
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))
        il))))

(defmethod type-index-remove ((uuid array) (type-id integer) (idx type-index))
  (let ((lock (aref (type-index-locks idx) type-id)))
    (with-lock (lock)
      (let ((il (%ti-list idx type-id)))
        (remove-from-index-list uuid il)
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))
        il))))

(defmethod get-type-index-list ((idx type-index) (type-id integer))
  (%ti-list idx type-id))
