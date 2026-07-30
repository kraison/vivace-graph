(in-package :graph-db)

(alexandria:define-constant +memory-magic-byte-offset+ 0)
(alexandria:define-constant +memory-storage-version-offset+ 1)
(alexandria:define-constant +memory-memory-pointer-offset+ 2)
(alexandria:define-constant +memory-bins-offset+ 10)
(alexandria:define-constant +memory-bin-count+ 128)
(alexandria:define-constant +memory-usable-offset+ 1034) ;; (+ 2 8 (* 8 128))
(alexandria:define-constant +allocation-header-size+ 8
  :documentation "The size, in bytes, of the metadata associated with
  an allocation.")

(alexandria:define-constant +allocation-pointer-size+ 8
  :documentation "The size, in bytes, of a pointer to an allocation.")

(defstruct (memory
             (:print-function
              (lambda (m s d)
                (declare (ignore d))
                (format s "#<MEMORY :LOCATION ~S :SIZE ~S :DATA-OFFSET ~S>"
                        (memory-location m) (memory-size m) (memory-data-offset m)))))
  location size mmap
  (free-list
   #+sbcl (make-hash-table :synchronized t)
   #+lispworks (make-hash-table :single-thread nil)
   #+ccl (make-hash-table :shared t)
   #+ecl (make-hash-table #+graph-db-ecl-sync-hash :synchronized
                           #+graph-db-ecl-sync-hash t))
  free-list-thread
  (pointer 0 :type (UNSIGNED-BYTE 64))
  (lock (make-rw-lock))
  extent-size data-offset
  (bin-locks (map-into (make-array +memory-bin-count+) 'make-recursive-lock))
  (cache-lock (make-rw-lock))
  (cache
   #+sbcl (make-hash-table :synchronized t :weakness :value)
   #+lispworks (make-hash-table :single-thread nil :weak-kind :value)
   #+ccl (make-hash-table :shared t :weak :value)
   #+ecl (make-hash-table :weakness :value
                           #+graph-db-ecl-sync-hash :synchronized
                           #+graph-db-ecl-sync-hash t)))

(defmethod set-byte ((memory memory) offset byte)
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  (unless (>= offset (memory-data-offset memory))
    (log:error "SEGV: Tried to write byte ~S at unsafe position ~S in ~S" byte offset memory)
    (error "SEGV: Tried to write byte ~S at unsafe position ~S in ~S" byte offset memory))
  ;;(log:debug "SET-BYTE: ~A ADDR ~A TO ~A" (memory-location memory) offset byte)
  ;;(setf (cffi:mem-aref (m-pointer (memory-mmap memory)) :unsigned-char offset) byte))
  (set-byte (memory-mmap memory) offset byte))

(defmethod get-byte ((memory memory) offset)
  (declare (type word offset))
  ;;(cffi:mem-aref (m-pointer (memory-mmap memory)) :unsigned-char offset))
  (get-byte (memory-mmap memory) offset))

(defmethod get-bytes ((memory memory) offset length)
  (declare (type word offset length))
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (get-byte memory (+ i offset))))
    vec))

(defmethod serialize-uint64 ((memory memory) int offset)
  (declare (type word int offset))
  (unless (>= offset (memory-data-offset memory))
    (log:error "SEGV: Tried to serialize-uint64 ~S at unsafe position ~S in ~S" int offset memory)
    (error "SEGV: Tried to serialize-uint64 ~S at unsafe position ~S in ~S" int offset memory))
  ;;(log:debug "MEMORY: SERIALIZING UINT64 ~A TO ADDR ~A" int offset)
  (serialize-uint64 (memory-mmap memory) int offset))

(defmethod deserialize-uint64 ((memory memory) offset)
  (declare (type word offset))
  (deserialize-uint64 (memory-mmap memory) offset))

(defmethod serialize-pointer ((memory memory) int offset)
  (serialize-uint64 memory int offset))

(defmethod deserialize-pointer ((memory memory) offset)
  (deserialize-uint64 memory offset))

(defmethod incf-uint64 ((memory memory) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 memory offset)))
    (incf int)
    (serialize-uint64 memory int offset)
    int))

(defmethod decf-uint64 ((memory memory) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 memory offset)))
    (serialize-uint64 memory (decf int) offset)
    int))

(defun close-memory (memory)
  (with-write-lock ((memory-lock memory))
    (munmap-file (memory-mmap memory) :save-p t)
    (setf (memory-mmap memory) nil)))

(defun grow-memory (memory length)
  (let ((num-extents (ceiling (/ length (memory-extent-size memory)))))
    (log:info "GROWING MEMORY BY ~A EXTENTS" num-extents)
    (extend-mapped-file (memory-mmap memory)
                        (* num-extents (memory-extent-size memory)))
    (setf (memory-size memory)
          (mapped-file-length (memory-mmap memory)))))

;;; Allocations

(defmacro with-locked-bin ((memory size &key wait-p) &body body)
  (let ((lock (gensym)))
    `(let ((bin (mod (sxhash ,size) +memory-bin-count+)))
       (let ((,lock (aref (memory-bin-locks ,memory) bin)))
         #+ccl
         ;; FIXME: honor wait-p for CCL
         (ccl:with-lock-grabbed (,lock)
           ,@body)
         #+lispworks
         (with-recursive-lock-held (,lock)
           ,@body)
         #+ecl
         (with-recursive-lock-held (,lock)
           ,@body)
         #+sbcl
         (sb-thread:with-recursive-lock (,lock :wait-p ,wait-p)
           ,@body)))))

(defun deserialize-header (mmap allocation-offset)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((int (deserialize-uint64 mmap allocation-offset))
         (free-p (not (logtest 1 int)))
         (data-size (ash int -1)))
    (if free-p
        (let ((next (deserialize-pointer mmap (+ +allocation-header-size+
                                                 allocation-offset)))
              (prev (deserialize-pointer mmap (+ +allocation-header-size+
                                                 +allocation-pointer-size+
                                                 allocation-offset))))
          (values data-size free-p next prev))
        (values data-size free-p 0 0))))

(deftype allocation-data-size ()
  `(integer 1 ,(expt 2 63)))

(defun serialize-header (mmap allocation-offset data-size &key (active-p t))
  ;; Max bytes we can allocate is 9223372036854775808 (ash (expt 2 64) -1)
  ;; since we use the first bit as the active / inactive tag
  ;;(declare (optimize (speed 3) (safety 0)))
  (assert (typep data-size 'allocation-data-size))
  (let ((int (logior (ash data-size 1) (if active-p 1 0))))
    (serialize-uint64 mmap int allocation-offset)
    (unless active-p
      ;; Zero out the next and previous offsets for free blocks
      (serialize-pointer mmap 0 (+ +allocation-header-size+
                                   allocation-offset))
      (serialize-pointer mmap 0 (+ +allocation-header-size+
                                   +allocation-pointer-size+
                                   allocation-offset))))
  (+ +allocation-header-size+ allocation-offset))

(defun allocation-free-p (memory allocation-offset)
  (nth-value 1 (deserialize-header (memory-mmap memory) allocation-offset)))

(defun map-memory (fn memory &key collect-p include-free-p)
  (let ((allocation-offset (memory-data-offset memory)) (done-p nil) (r nil))
    (loop until done-p
       do
         (multiple-value-bind (size free-p)
             (deserialize-header (memory-mmap memory) allocation-offset)
           ;;(log:debug "GOT BLOCK OF ~A BYTES AT OFFSET ~A" block-len allocation-offset)
           (if (= 0 size)
               (setq done-p t)
               (let ((allocation-data-offset (+ +allocation-header-size+
                                                allocation-offset)))
                 (when (or include-free-p (not free-p))
                   (if collect-p
                       (push (funcall fn allocation-data-offset size free-p) r)
                       (funcall fn allocation-data-offset size free-p)))
                 (incf allocation-offset (+ +allocation-header-size+ size))))))
    (when collect-p
      (nreverse r))))

(defun free-list (memory)
  (remove-if
   'null
   (map-memory
    (lambda (allocation-data-offset size free?)
      (when free?
        (cons allocation-data-offset size)))
    memory
    :collect-p t
    :include-free-p t)))

(defun set-memory-pointer (memory pointer)
  (setf (memory-pointer memory) pointer)
  (serialize-uint64 (memory-mmap memory) pointer +memory-memory-pointer-offset+))

(defun read-memory-pointer (memory)
  (deserialize-uint64 memory +memory-memory-pointer-offset+))

(defmethod maybe-allocate-from-free-list ((memory memory) data-size)
  (with-locked-bin (memory data-size)
    (let ((allocation-offset (pop (gethash data-size (memory-free-list memory)))))
      (when allocation-offset
        ;; FIXME: zero the bytes?
        (serialize-header (memory-mmap memory) allocation-offset data-size
                          :active-p t)))))

(defun add-to-free-list (memory allocation-offset data-size)
  ;; Use header offset, not user offset!
  (with-locked-bin (memory data-size)
    (push allocation-offset (gethash data-size (memory-free-list memory)))))

(defmethod init-free-list ((memory memory))
  (dolist (pair (free-list memory))
    (destructuring-bind (allocation-data-offset . data-size)
        pair
      (push (- allocation-data-offset +allocation-header-size+)
            (gethash data-size (memory-free-list memory))))))

(defun create-memory (location size &key (extent-size (* 1024 1024 100)))
  (let ((memory
         (make-memory :location location
                      :pointer +memory-usable-offset+
                      :data-offset +memory-usable-offset+
                      :extent-size extent-size
                      :size size
                      :lock (make-rw-lock)
                      :mmap (mmap-file location
                                       :size (+ 1 +memory-usable-offset+ size)))))
    (set-byte (memory-mmap memory) +memory-magic-byte-offset+ +data-magic-byte+)
    (set-byte (memory-mmap memory) +memory-storage-version-offset+ +storage-version+)
    (init-free-list memory)
    (set-memory-pointer memory (memory-pointer memory))
    memory))

(defun memory-magic-byte-corrupt-p (memory)
  (let ((byte (get-byte (memory-mmap memory) +memory-magic-byte-offset+)))
    (unless (= +data-magic-byte+ byte)
      (log:error "SEGV: ~A magic-byte corrupted: 0x~X" (memory-location memory) byte)
      byte)))

(defun open-memory (location &key (extent-size (* 1024 1024 100))
                               (accept-versions (list +storage-version+)))
  "Open the memory-mapped region at LOCATION.  ACCEPT-VERSIONS is the list of
on-disk storage-format versions this open will tolerate (default: only the
current build's version).  MIGRATE-GRAPH passes the older version(s) so it can
read a pre-MVCC graph for logical backup."
  (let ((memory
         (make-memory :location location
                      :data-offset +memory-usable-offset+
                      :extent-size extent-size
                      :lock (make-rw-lock)
                      :mmap (mmap-file location :create-p nil))))
    (when (memory-magic-byte-corrupt-p memory)
      (munmap-file (memory-mmap memory))
      (error "~A is not a memory file!" location))
    (let ((found (get-byte (memory-mmap memory) +memory-storage-version-offset+)))
      (unless (member found accept-versions)
        (munmap-file (memory-mmap memory))
        (error "~A is storage format v~D but this build expects v~D. ~
Pre-MVCC (v1) graphs must be migrated with MIGRATE-GRAPH (snapshot + replay)."
               location found +storage-version+)))
    (setf (memory-pointer memory) (read-memory-pointer memory)
          (memory-size memory) (size-of (memory-mmap memory)))
    (init-free-list memory)
    memory))


(defun free (memory allocation-data-offset)
  ;; allocation-data-offset is the beginning of the user-accessible
  ;; portion of the data block
  (unless (>= allocation-data-offset (memory-data-offset memory))
    (log:error "SEGV: Unsafe call to free: pointer ~S ~S"
               allocation-data-offset memory)
    (error "SEGV: Cannot free pointer ~S" allocation-data-offset))
  (let ((allocation-offset (- allocation-data-offset +allocation-header-size+)))
    (multiple-value-bind (size free-p)
        (deserialize-header (memory-mmap memory) allocation-offset)
      (when free-p
        (log:error "FREE on already-freed allocation at ~S"
                   allocation-offset)
        (error "FREE on an already-freed allocation at ~S"
               allocation-offset))
      ;;(log:debug "ADDING ~S TO ~A FREE LIST, SIZE ~S"
      ;;           allocation-offset (m-path (memory-mmap memory)) size)
      (serialize-header (memory-mmap memory) allocation-offset size :active-p nil)
      ;; FIXME: zero the bytes?
      (add-to-free-list memory allocation-offset size)
      allocation-offset)))

(defun normalize-allocation-data-size (size)
  (cond ((<= size 16)
         16)
        ((<= size 512)
         (* 8 (ceiling size 8)))
        (t
         size)))

(defun unallocated-memory-available (memory)
  (- (memory-size memory) (memory-pointer memory)))

(defun allocate-from-memory (memory data-size)
  (let ((current-pointer nil)
        (allocation-size (+ +allocation-header-size+ data-size))
        (available-size (unallocated-memory-available memory)))
    (when (< available-size allocation-size)
      (grow-memory memory (- allocation-size available-size)))
    (setq current-pointer (memory-pointer memory))
    (set-memory-pointer memory
                        (+ (memory-pointer memory) allocation-size))
    (serialize-header (memory-mmap memory) current-pointer data-size
                      :active-p t)))

(defun allocate (memory data-size)
  (setq data-size (normalize-allocation-data-size data-size))
  (with-write-lock ((memory-lock memory))
    (let ((allocation-offset
           (or (maybe-allocate-from-free-list memory data-size)
               (allocate-from-memory memory data-size))))
      (unless (>= allocation-offset (memory-data-offset memory))
        (log:error "SEGV: Tried to allocate at unsafe position ~S in ~S"
                   allocation-offset memory)
        (error "Tried to allocate at unsafe position ~S in ~S"
               allocation-offset memory))
      (values allocation-offset data-size))))

#+sbcl
(defun test-allocator ()
  (let ((memory (create-memory "/var/tmp/testmem.dat" 10240))
        (pointers nil) (threads nil)
        (gate (sb-concurrency:make-gate :open nil)))
    (unwind-protect
         (progn
           (dotimes (i 100)
             (push
              (make-thread
               (lambda ()
                 (sb-concurrency:wait-on-gate gate)
                 (let ((pointer (allocate memory 100)))
                   (push pointer pointers))))
              threads))
           (sb-concurrency:open-gate gate)
           (log:debug "THREAD: ~A" (length threads))
           (loop until (notany 'thread-alive-p threads) do (sleep 1))
           (dolist (p pointers)
             (free memory p)))
      (progn
        (close-memory memory)
        (delete-file "/var/tmp/testmem.dat")))))
