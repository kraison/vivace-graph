(in-package :graph-db)

;;(declaim (optimize (speed 3) (space 0) (debug 0)))
(defvar *rehashing-bucket* nil)
(alexandria:define-constant +lhash-count-offset+ 1)
(alexandria:define-constant +lhash-next-overflow-pointer-offset+ 9)
(alexandria:define-constant +lhash-next-split-offset+ 17)
(alexandria:define-constant +lhash-level-offset+ 25)
(alexandria:define-constant +lhash-key-bytes-offset+ 33)
(alexandria:define-constant +lhash-value-bytes-offset+ 41)
(alexandria:define-constant +lhash-bucket-size-offset+ 49)
(alexandria:define-constant +lhash-bucket-bytes-offset+ 57)
(alexandria:define-constant +lhash-null-key-offset+ 65)

(defgeneric uuid-array-equal (x y &optional offset1 offset2))

(defstruct (lhash
             (:conc-name %lhash-)
             (:constructor %make-lhash)
             (:print-function
              (lambda (lhash stream depth)
                (declare (ignore depth))
                (format stream
                        "#<LHASH :TEST ~A :LOCATION ~A>"
                        (%lhash-test lhash)
                        (%lhash-location lhash))))
             (:predicate lhash-p))
  (test 'uuid-array-equal)
  (base-buckets 4)
  (level 0 :type (UNSIGNED-BYTE 64))
  (key-bytes +key-bytes+)
  (null-key +null-key+)
  (value-bytes +value-bytes+)
  (bucket-size +bucket-size+)
  bucket-bytes
  (next-split 0 :type (UNSIGNED-BYTE 64))
  (next-overflow-pointer 1 :type (UNSIGNED-BYTE 64))
  (count 0 :type (UNSIGNED-BYTE 64))
  count-lock
  split-lock
  overflow-lock
  config
  table
  overflow
  location
  (max-locks *max-locks*)
  lock-vector
  (key-serializer 'serialize-key)
  (key-deserializer 'deserialize-key)
  (value-serializer 'serialize-uint64)
  (value-deserializer 'deserialize-uint64)
  ;; OBSOLETE (MVCC): the node-data read-after-free finalizer is gone -- read
  ;; paths now materialize bytes under a read pin (see ENSURE-NODE-BYTES).  The
  ;; slot is retained (unused, always NIL) so cl-store can still restore lhash
  ;; struct.dat files written before the finalizer was removed.
  (value-finalizer nil))

;; ECL does not expose SIMPLE-VECTOR as a class usable as a method
;; specializer; the ((x array) (y array)) method below (using AREF) handles
;; simple vectors there too.
#-ecl
(defmethod uuid-array-equal ((x simple-vector) (y simple-vector) &optional _a _b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _a _b))
  (dotimes (i 16)
    (unless (= (svref x i) (svref y i))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x array) (y array) &optional _a _b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _a _b))
  (dotimes (i 16)
    (unless (= (aref x i) (aref y i))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x array) (y mpointer) &optional _a _b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _a _b))
  (dotimes (i 16)
    (unless (= (aref x i) (get-byte (mpointer-mmap y) (+ i (mpointer-loc y))))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x mpointer) (y array) &optional _a _b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _a _b))
  (dotimes (i 16)
    (unless (= (aref y i) (get-byte (mpointer-mmap x) (+ i (mpointer-loc x))))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x mpointer) (y mpointer) &optional _a _b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _a _b))
  (dotimes (i 16)
    (unless (= (get-byte (mpointer-mmap x) (+ i (mpointer-loc x)))
               (get-byte (mpointer-mmap y) (+ i (mpointer-loc y))))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x array) (y mapped-file) &optional offset1 _)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _))
  (dotimes (i 16)
    (unless (= (aref x i) (get-byte y (+ i offset1)))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x mapped-file) (y array) &optional offset1 _)
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore _))
  (dotimes (i 16)
    (unless (= (aref y i) (get-byte x (+ i offset1)))
      (return-from uuid-array-equal nil)))
  t)

(defmethod uuid-array-equal ((x mapped-file) (y mapped-file) &optional offset-x offset-y)
  (declare (optimize (speed 3) (safety 0)))
  (dotimes (i 16)
    (unless (= (get-byte x (+ i offset-x))
               (get-byte y (+ i offset-y)))
      (return-from uuid-array-equal nil)))
  t)

(defmethod serialize-key ((mf mapped-file) uuid offset)
  "Encode a UUID.  We keep UUIDs in byte-array form for efficiency's sake."
  (declare (type word offset))
  (declare (type (array (unsigned-byte 8) (16)) uuid))
  ;;(log:debug "SERIALIZING KEY ~A TO OFFSET ~X" uuid offset)
  (dotimes (i 16)
    ;;(log:debug "WRITING BYTE ~X" (aref uuid i))
    (set-byte mf (+ i offset) (aref uuid i)))
  (+ offset 16))

(defmethod deserialize-key ((mf mapped-file) (offset integer))
  "Decode a UUID.  We keep UUIDs in byte-array form for efficiency's sake."
  (declare (type word offset))
  (let ((vec (get-buffer 16)))
    (declare (type (array (unsigned-byte 8) (16)) vec))
    (dotimes (i 16)
      (setf (aref vec i) (get-byte mf (+ i offset))))
    vec))

(defun set-lhash-next-split (lhash value)
  (serialize-uint64 (%lhash-config lhash) value +lhash-next-split-offset+)
  (setf (%lhash-next-split lhash) value))

(defun read-lhash-next-split (lhash)
  (with-read-lock ((%lhash-split-lock lhash))
    (%lhash-next-split lhash)))

(defun incf-lhash-next-split (lhash)
  (incf (%lhash-next-split lhash))
  (set-lhash-next-split lhash (%lhash-next-split lhash))
  (%lhash-next-split lhash))

(defun set-lhash-next-overflow-pointer (lhash value)
  (serialize-uint64 (%lhash-config lhash) value +lhash-next-overflow-pointer-offset+)
  (setf (%lhash-next-overflow-pointer lhash) value))

(defun read-lhash-next-overflow-pointer (lhash)
  (%lhash-next-overflow-pointer lhash))

(defun incf-lhash-next-overflow-pointer (lhash &optional (delta 1))
  (incf (%lhash-next-overflow-pointer lhash) delta)
  (set-lhash-next-overflow-pointer lhash (%lhash-next-overflow-pointer lhash))
  (%lhash-next-overflow-pointer lhash))

(defun set-lhash-count (lhash value)
  (let ((lock (%lhash-count-lock lhash)))
    (with-write-lock (lock)
      (serialize-uint64 (%lhash-config lhash) value +lhash-count-offset+)
      (setf (%lhash-count lhash) value))))

(defun read-lhash-count (lhash)
  (let ((lock (%lhash-count-lock lhash)))
    (with-read-lock (lock)
      (%lhash-count lhash))))

(defun incf-lhash-count (lhash)
  (let ((lock (%lhash-count-lock lhash)))
    (with-write-lock (lock)
      (incf (%lhash-count lhash))
      (serialize-uint64 (%lhash-config lhash)
                        (%lhash-count lhash)
                        +lhash-count-offset+)
      (%lhash-count lhash))))

(defun decf-lhash-count (lhash)
  (let ((lock (%lhash-count-lock lhash)))
    (with-write-lock (lock)
      (decf (%lhash-count lhash))
      (serialize-uint64 (%lhash-config lhash)
                        (%lhash-count lhash)
                        +lhash-count-offset+)
      (%lhash-count lhash))))

(defun set-lhash-level (lhash value)
  (serialize-uint64 (%lhash-config lhash) value +lhash-level-offset+)
  (setf (%lhash-level lhash) value))

(defun read-lhash-level (lhash)
  (with-read-lock ((%lhash-split-lock lhash))
    (%lhash-level lhash)))

(defun incf-lhash-level (lhash)
  (incf (%lhash-level lhash))
  (set-lhash-level lhash (%lhash-level lhash))
  (%lhash-level lhash))

(defun load-factor (lhash)
  (/ (read-lhash-count lhash)
     (* (%lhash-bucket-size lhash)
        (%lhash-base-buckets lhash)
        (expt 2 (1+ (read-lhash-level lhash))))))

(defun bucket-count (lhash)
  (+ (read-lhash-next-split lhash)
     (* (%lhash-base-buckets lhash)
        (expt 2 (read-lhash-level lhash)))))

(defun %bucket-count (lhash)
  (+ (%lhash-next-split lhash)
     (* (%lhash-base-buckets lhash)
        (expt 2 (%lhash-level lhash)))))

(defun hash (lhash level key)
  ;; Uses %lhash-next-split (raw) — all callers already hold split-lock READ or
  ;; WRITE; re-acquiring via read-lhash-next-split would deadlock if a writer
  ;; is queued (nested read blocks on condition-wait while outer read holds R=1
  ;; that the writer is waiting on).
  (let* ((hkey (%hash key))
         (mod-n (* (%lhash-base-buckets lhash) (expt 2 level)))
         (h0 (mod hkey mod-n)))
    (if (and (>= h0 (%lhash-next-split lhash))
             (< h0 mod-n))
        h0
        (mod hkey (* (%lhash-base-buckets lhash) (expt 2 (1+ level)))))))

(defun hash0 (lhash level key)
  (mod (%hash key) (* (%lhash-base-buckets lhash) (expt 2 level))))

(defun make-lhash (&key (buckets 4) location (test 'uuid-array-equal)
                   (overflow-slots 100) (key-bytes +key-bytes+)
                   (value-bytes +value-bytes+) (bucket-size +bucket-size+)
                   (max-locks *max-locks*) (null-key +null-key+)
                   (key-serializer 'serialize-key)
                   (key-deserializer 'deserialize-key)
                   (value-serializer 'serialize-uint64)
                   (value-deserializer 'deserialize-uint64))
  (when (or (probe-file (format nil "~A/config.dat" location))
            (probe-file (format nil "~A/table.dat" location))
            (probe-file (format nil "~A/overflow.dat" location)))
    (error "Linear hash already exists at ~A" location))
  (ensure-directories-exist location)
  (let* ((bucket-bytes (+ 8              ;; overflow address
                          (* bucket-size ;; items in bucket
                             (+ key-bytes value-bytes)))) ;; item
         (lhash (%make-lhash :table nil
                             :overflow nil
                             :config nil
                             :base-buckets buckets
                             :null-key null-key
                             :key-bytes key-bytes
                             :value-bytes value-bytes
                             :bucket-size bucket-size
                             :bucket-bytes bucket-bytes
                             :count 0
                             :next-overflow-pointer 1
                             :location location
                             :test test
                             :max-locks max-locks
                             :lock-vector nil
                             :key-serializer key-serializer
                             :key-deserializer key-deserializer
                             :value-serializer value-serializer
                             :value-deserializer value-deserializer)))
    (cl-store:store lhash (merge-pathnames "struct.dat" location))
    (setf (%lhash-count-lock lhash) (make-rw-lock)
          (%lhash-split-lock lhash) (make-rw-lock)
          (%lhash-overflow-lock lhash) (make-rw-lock)
          (%lhash-config lhash)
          (mmap-file (merge-pathnames "config.dat" location)
                     :size (+ (* 8 8) key-bytes))
          (%lhash-table lhash)
          (mmap-file (merge-pathnames "table.dat" location)
                     :size (if (> (1+ (* bucket-bytes buckets))
                                  (* 1024 1024 1000))
                               (+ 1 +data-extent-size+ (* bucket-bytes buckets))
                               (* 1024 1024 1000)))
          (%lhash-overflow lhash)
          (mmap-file (merge-pathnames "overflow.dat" location)
                     :size (* 1024 1024 1000)) ;; (1+ (* bucket-bytes overflow-slots)))
          (%lhash-lock-vector lhash)
          (make-array (list max-locks)))
    (set-byte (%lhash-table lhash) 0 +lhash-magic-byte+)
    (set-byte (%lhash-config lhash) 0 +config-magic-byte+)
    (set-byte (%lhash-overflow lhash) 0 +overflow-magic-byte+)
    ;; Serialize our config
    (set-lhash-count lhash 0)
    (set-lhash-next-overflow-pointer lhash 1)
    (set-lhash-next-split lhash 0)
    (set-lhash-level lhash 0)
    (serialize-uint64 (%lhash-config lhash) key-bytes +lhash-key-bytes-offset+)
    (serialize-uint64 (%lhash-config lhash) value-bytes +lhash-value-bytes-offset+)
    (serialize-uint64 (%lhash-config lhash) bucket-size +lhash-bucket-size-offset+)
    (serialize-uint64 (%lhash-config lhash) bucket-bytes +lhash-bucket-bytes-offset+)
    (funcall (%lhash-key-serializer lhash) (%lhash-config lhash) null-key +lhash-null-key-offset+)
    (dotimes (i max-locks)
      (setf (svref (%lhash-lock-vector lhash) i)
            #+sbcl (sb-thread:make-mutex)
            #+lispworks (mp:make-lock)
            #+ccl (make-lock)
            #+ecl (mp:make-lock :recursive t)))
    lhash))

(defun open-lhash (location)
  (let ((lhash (cl-store:restore (merge-pathnames "struct.dat" location))))
    (handler-case
        (progn
          (setf (%lhash-config lhash)
                (mmap-file (merge-pathnames "config.dat" location) :create-p nil)
                (%lhash-table lhash)
                (mmap-file (merge-pathnames "table.dat" location) :create-p nil)
                (%lhash-overflow lhash)
                (mmap-file (merge-pathnames "overflow.dat" location) :create-p nil)
                (%lhash-count-lock lhash) (make-rw-lock)
                (%lhash-split-lock lhash) (make-rw-lock)
                (%lhash-overflow-lock lhash) (make-rw-lock)
                (%lhash-lock-vector lhash)
                (make-array (list (%lhash-max-locks lhash))))
          (unless (= +lhash-magic-byte+ (get-byte (%lhash-table lhash) 0))
            (error "Cannot open lhash with wrong magic byte in table.dat"))
          (unless (= +config-magic-byte+ (get-byte (%lhash-config lhash) 0))
            (error "Cannot open lhash with wrong magic byte in config.dat"))
          (unless (= +overflow-magic-byte+ (get-byte (%lhash-overflow lhash) 0))
            (error "Cannot open lhash with wrong magic byte in overflow.dat"))
          ;; Deserialize our config
          (setf (%lhash-count lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-count-offset+)
                (%lhash-next-overflow-pointer lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-next-overflow-pointer-offset+)
                (%lhash-next-split lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-next-split-offset+)
                (%lhash-level lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-level-offset+)
                (%lhash-key-bytes lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-key-bytes-offset+)
                (%lhash-value-bytes lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-value-bytes-offset+)
                (%lhash-bucket-size lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-bucket-size-offset+)
                (%lhash-bucket-bytes lhash)
                (deserialize-uint64 (%lhash-config lhash) +lhash-bucket-bytes-offset+)
                (%lhash-null-key lhash)
                (funcall (%lhash-key-deserializer lhash) (%lhash-config lhash) +lhash-null-key-offset+))
          (dotimes (i (%lhash-max-locks lhash))
            (setf (svref (%lhash-lock-vector lhash) i)
                  #+sbcl (sb-thread:make-mutex)
                  #+lispworks (mp:make-lock)
                  #+ccl (make-lock)
                  #+ecl (mp:make-lock :recursive t))))
      (error (c)
        (log:debug "Cannot open lhash: ~S" c)
        (munmap-file (%lhash-config lhash))
        (munmap-file (%lhash-table lhash))
        (munmap-file (%lhash-overflow lhash))))
    lhash))

(defun close-lhash (lhash)
  (munmap-file (%lhash-config lhash) :save-p t)
  (munmap-file (%lhash-table lhash) :save-p t)
  (munmap-file (%lhash-overflow lhash) :save-p t)
  (setf lhash nil))

(defun delete-lhash (lhash)
  (close-lhash lhash)
  (delete-file (merge-pathnames "table.dat" (%lhash-location lhash)))
  (delete-file (merge-pathnames "overflow.dat" (%lhash-location lhash)))
  (delete-file (merge-pathnames "config.dat" (%lhash-location lhash)))
  (delete-file (merge-pathnames "struct.dat" (%lhash-location lhash)))
  nil)

(defun lookup-lhash-lock (lhash index)
  (let ((lock (svref (%lhash-lock-vector lhash)
                     (mod index (%lhash-max-locks lhash)))))
    lock))

(defun release-lhash-lock (lhash index)
  (let ((lock (svref (%lhash-lock-vector lhash)
                     (mod index (%lhash-max-locks lhash)))))
    (prog1
        #+sbcl (sb-thread:release-mutex lock)
        #+lispworks (mp:process-unlock lock)
        #+ccl (ccl:release-lock lock)
        #+ecl (mp:giveup-lock lock)
      (log:debug "RELEASED LOCK: ~A" lock))))

(defun grab-lhash-lock (lhash index &key (wait-p t) timeout)
  (let ((lock (svref (%lhash-lock-vector lhash)
                     (mod index (%lhash-max-locks lhash)))))
    (prog1
        #+sbcl (sb-thread:grab-mutex lock :waitp wait-p :timeout timeout)
        #+lispworks (mp:process-lock lock nil timeout)
        ;; FIXME honor wait-p and timeout for CCL
        #+ccl (ccl:grab-lock lock)
        ;; FIXME honor timeout for ECL
        #+ecl (mp:get-lock lock wait-p)
      (log:debug "GOT LOCK: ~A" lock))))

(defmacro with-locked-hash-key ((lhash key) &body body)
  (with-gensyms (lock bucket lh k)
    `(let ((,lh ,lhash))
       (with-read-lock ((%lhash-split-lock ,lh))
         (let* ((,k ,key)
                ;; Use %lhash-level (raw) — split-lock is already held as read;
                ;; calling read-lhash-level here would re-acquire it and deadlock
                ;; if a writer is queued.
                (,bucket (hash ,lh (%lhash-level ,lh) ,k))
                (,lock (lookup-lhash-lock ,lhash ,bucket)))
           #+ccl
           (ccl:with-lock-grabbed (,lock)
             ,@body)
           #+lispworks
           (mp:with-lock (,lock)
             ,@body)
           #+ecl
           (mp:with-lock (,lock)
             ,@body)
           #+sbcl
           (sb-thread:with-recursive-lock (,lock)
             ,@body))))))

(defmacro with-locked-hash-bucket ((lhash bucket) &body body)
  (with-gensyms (lock)
    `(let ((,lock (lookup-lhash-lock ,lhash ,bucket)))
       #+ccl
       (ccl:with-lock-grabbed (,lock)
         ,@body)
       #+lispworks
       (mp:with-lock (,lock)
         ,@body)
       #+ecl
       (mp:with-lock (,lock)
         ,@body)
       #+sbcl
       (sb-thread:with-recursive-lock (,lock)
         ,@body))))

(defun acquire-overflow-bucket (lhash)
  (with-write-lock ((%lhash-overflow-lock lhash))
    (let ((address (incf-lhash-next-overflow-pointer
                    lhash (%lhash-bucket-bytes lhash))))
      (unless (>= (mapped-file-length (%lhash-overflow lhash))
                  (+ (read-lhash-next-overflow-pointer lhash)
                     (%lhash-bucket-bytes lhash)))
        ;;(log:info "SPLIT: extending overflow ~A" (%lhash-overflow lhash))
        (extend-mapped-file (%lhash-overflow lhash)
                            ;;(%lhash-bucket-bytes lhash)
                            +data-extent-size+))
      address)))

(defun bucket-offset (lhash bucket)
  (let ((offset (1+ (* bucket (%lhash-bucket-bytes lhash)))))
    ;;(LOG:DEBUG "OFFSET FOR BUCKET ~A IS ~X" bucket offset)
    offset))

(defun read-overflow-offset (lhash file bucket-offset)
  (let* ((pointer-offset (+ bucket-offset
                            (* (%lhash-bucket-size lhash) ;; items in bucket
                               (+ (%lhash-key-bytes lhash)
                                  (%lhash-value-bytes lhash)))))
         (pointer (deserialize-pointer file pointer-offset)))
    (values pointer pointer-offset)))

(defun get-overflow-offset (lhash file bucket-offset)
  (multiple-value-bind (pointer pointer-offset)
      (read-overflow-offset lhash file bucket-offset)
    (if (= 0 pointer)
        (let ((pointer (acquire-overflow-bucket lhash)))
          (serialize-uint64 file pointer pointer-offset)
          pointer)
        pointer)))

(defun read-bucket-as-bytes (lhash file offset)
  (let ((bytes-list nil) (begin-offset offset)
        (record-len (+ (%lhash-key-bytes lhash) (%lhash-value-bytes lhash))))
    (dotimes (i (%lhash-bucket-size lhash))
      (let ((bytes (get-bytes file offset record-len)))
        (if (every #'zerop bytes)
            (return-from read-bucket-as-bytes bytes-list)
            (push bytes bytes-list))
        (incf offset record-len)))
    (let ((overflow-offset (read-overflow-offset lhash file begin-offset)))
      (if (= 0 overflow-offset)
          bytes-list
          (nconc bytes-list
                 (read-bucket-as-bytes
                  lhash (%lhash-overflow lhash) overflow-offset))))))

(defun read-bucket (lhash file offset)
  (let ((pairs nil) (begin-offset offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (let ((key (funcall (%lhash-key-deserializer lhash) file offset)))
        (when (funcall (%lhash-test lhash) (%lhash-null-key lhash) key)
          ;;(log:debug "GOT NULL KEY ~A; RETURNING ~A" key pairs)
          (return-from read-bucket pairs))
        (let ((value (funcall (%lhash-value-deserializer lhash) file
                              (+ offset (%lhash-key-bytes lhash)))))
          (push (cons key value) pairs)
          (incf offset (+ (%lhash-key-bytes lhash) (%lhash-value-bytes lhash))))))
    (let ((overflow-offset (read-overflow-offset lhash file begin-offset)))
      (if (= 0 overflow-offset)
          pairs
          (nconc pairs
                 (read-bucket lhash (%lhash-overflow lhash) overflow-offset))))))

(defun add-to-bucket (lhash file offset key value &optional split-p)
  (declare (type word offset))
  ;;(declare (type (array (unsigned-byte 8) (16)) key))
  (let ((begin-offset offset))
    (declare (type word begin-offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (cond
        ((funcall (%lhash-test lhash) (%lhash-null-key lhash) file offset)
         (let ((new-offset
                (funcall (%lhash-key-serializer lhash) file key offset)))
           ;;(log:debug "ADDING ~A / ~A AT OFFSET ~X" key value new-offset)
           (funcall (%lhash-value-serializer lhash) file value new-offset)
           (unless *rehashing-bucket*
             (incf-lhash-count lhash)))
         (return-from add-to-bucket split-p))
        ((funcall (%lhash-test lhash) key file offset)
         (error 'duplicate-key-error :key key :instance lhash))
        (t (incf offset (+ (%lhash-key-bytes lhash)
                           (%lhash-value-bytes lhash))))))
    (let ((overflow-pointer (get-overflow-offset lhash file begin-offset)))
      ;;(log:debug "ADDING TO OVERFLOW AT ~X" overflow-pointer)
      (add-to-bucket lhash (%lhash-overflow lhash)
                     overflow-pointer key value t))))

(defun update-in-bucket (lhash file offset key value)
  (declare (type word offset))
  ;;(declare (type (array (unsigned-byte 8) (16)) key))
  (let ((begin-offset offset))
    (declare (type word begin-offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (cond
        ((funcall (%lhash-test lhash) (%lhash-null-key lhash) file offset)
         (error 'nonexistent-key-error :key key :instance lhash))
        ((funcall (%lhash-test lhash) key file offset)
         ;;(log:debug "REPLACING ~A / ~A AT OFFSET ~X" key value offset)
         (funcall (%lhash-value-serializer lhash) file value
                  (+ (%lhash-key-bytes lhash) offset))
;;         (sync-region file
;;                      :addr (+ (%lhash-key-bytes lhash) offset)
;;                      :length (%lhash-value-bytes lhash))
         (return-from update-in-bucket t))
        (t (incf offset (+ (%lhash-key-bytes lhash)
                           (%lhash-value-bytes lhash))))))
    (let ((overflow-pointer (get-overflow-offset lhash file begin-offset)))
      ;;(log:debug "ADDING TO OVERFLOW AT ~X" overflow-pointer)
      (update-in-bucket lhash (%lhash-overflow lhash) overflow-pointer key value))))

(defun custom-update-in-bucket (lhash file offset update-fn key)
  (declare (type word offset))
  ;;(declare (type (array (unsigned-byte 8) (16)) key))
  (let ((begin-offset offset))
    (declare (type word begin-offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (cond
        ((funcall (%lhash-test lhash) (%lhash-null-key lhash) file offset)
         (error 'nonexistent-key-error :key key :instance lhash))
        ((funcall (%lhash-test lhash) key file offset)
         ;;(log:debug "CUSTOM UPDATING ~A WITH FN IN ~A ~A AT OFFSET ~X"
         ;;key update-fn file offset)
         (funcall update-fn file (+ (%lhash-key-bytes lhash) offset))
;;         (sync-region file
;;                      :addr (+ (%lhash-key-bytes lhash) offset)
;;                      :length (%lhash-value-bytes lhash))
         (return-from custom-update-in-bucket t))
        (t (incf offset (+ (%lhash-key-bytes lhash)
                           (%lhash-value-bytes lhash))))))
    (let ((overflow-pointer (get-overflow-offset lhash file begin-offset)))
      ;;(log:debug "ADDING TO OVERFLOW AT ~X" overflow-pointer)
      (custom-update-in-bucket lhash (%lhash-overflow lhash)
                               overflow-pointer update-fn key))))

(defun read-from-bucket (lhash file offset key)
  (let ((begin-offset offset))
    (declare (type word begin-offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (when (funcall (%lhash-test lhash) (%lhash-null-key lhash) file offset)
        (return-from read-from-bucket nil))
      (if (funcall (%lhash-test lhash) key file offset)
          (return-from read-from-bucket
            (funcall (%lhash-value-deserializer lhash) file
                     (+ offset (%lhash-key-bytes lhash))))
          (incf offset (+ (%lhash-key-bytes lhash)
                          (%lhash-value-bytes lhash)))))
    (let ((overflow-pointer (read-overflow-offset lhash file begin-offset)))
      (when (/= 0 overflow-pointer)
        (read-from-bucket lhash (%lhash-overflow lhash) overflow-pointer key)))))

(defun clear-bucket (lhash file offset)
  ;;(log:info "CLEARING BUCKET STARTING AT ~A" offset)
  (let ((begin-offset offset))
    (dotimes (i (%lhash-bucket-size lhash))
      (if (funcall (%lhash-test lhash) (%lhash-null-key lhash) file offset)
          (return-from clear-bucket)
          (progn
            (dotimes (i (+ (%lhash-key-bytes lhash) (%lhash-value-bytes lhash)))
              (set-byte file (+ i offset) 0))
            (incf offset (+ (%lhash-key-bytes lhash)
                            (%lhash-value-bytes lhash))))))
    (let ((overflow-pointer (read-overflow-offset lhash file begin-offset)))
      (when (/= overflow-pointer 0)
        (clear-bucket lhash (%lhash-overflow lhash) overflow-pointer)))))

(defun remove-from-bucket (lhash file offset key)
  ;; FIXME: this is really expensive, but it at least makes inserts fast and
  ;; requires no bookkeeping bytes.  Use read-bucket-as-bytes for reorg?
  (let ((pairs (read-bucket lhash file offset)))
    (clear-bucket lhash file offset)
    ;; The survivors are re-added, not newly inserted, so bind
    ;; *rehashing-bucket* to keep add-to-bucket from inflating the count.
    ;; Only the matched key's decf should affect it.
    (let ((*rehashing-bucket* t))
      (dolist (pair pairs)
        (if (funcall (%lhash-test lhash) (car pair) key)
            (decf-lhash-count lhash)
            (add-to-bucket lhash
                           (%lhash-table lhash)
                           offset
                           (car pair)
                           (cdr pair)))))))

(defun rehash-bucket (lhash bucket)
  ;;(log:info "REHASHING BUCKET ~A" bucket)
  (with-locked-hash-bucket (lhash bucket)
    (let ((pairs (read-bucket lhash
                              (%lhash-table lhash)
                              (bucket-offset lhash bucket))))
      ;;(log:debug "PAIRS:~%~{~A~^~%~}" pairs)
      (clear-bucket lhash (%lhash-table lhash) (bucket-offset lhash bucket))
      (dolist (pair pairs)
        ;;(log:debug "REHASHING ~A" pair)
        ;;(let* ((level (1+ (read-lhash-level lhash)))
        (let* ((level (1+ (%lhash-level lhash)))
               (new-bucket (hash0 lhash level (car pair))))
          ;;(log:debug "NEW BUCKET IS ~A, LOCKING" new-bucket)
          (with-locked-hash-bucket (lhash new-bucket)
            (add-to-bucket lhash
                           (%lhash-table lhash)
                           (bucket-offset lhash new-bucket)
                           (car pair)
                           (cdr pair))))))))

(defun split-lhash (lhash)
  ;;(log:info "SPLIT ~A TRING TO GET LOCK ~A!" lhash (%lhash-split-lock lhash))
  (with-write-lock ((%lhash-split-lock lhash))
    ;;(log:info "SPLIT ~A GOT LOCK!" lhash)
    (let ((*rehashing-bucket* t))
      (let ((bucket (%lhash-next-split lhash))) ;; (read-lhash-next-split lhash)))
        ;;(log:info "SPLITTING ~A BUCKET ~A" lhash bucket)
        (unless (>= (mapped-file-length (%lhash-table lhash))
                    (1+ (* (%lhash-bucket-bytes lhash)
                           ;;(1+ (bucket-count lhash)))))
                           (1+ (%bucket-count lhash)))))
          ;;(log:info "SPLIT: extending mmap ~A" (%lhash-table lhash))
          (setf (%lhash-table lhash)
                (extend-mapped-file (%lhash-table lhash)
                                    ;;(%lhash-bucket-bytes lhash))))
                                    +data-extent-size+)))
        (rehash-bucket lhash bucket)
        ;;(log:debug "DONE REHASHING ~A" bucket)
        (incf-lhash-next-split lhash)
        ;;(log:debug "DONE with incf-lhash-next-split ~A" lhash)
        (when (= (%lhash-next-split lhash) ;; (read-lhash-next-split lhash)
                 (* (%lhash-base-buckets lhash)
                    ;;(expt 2 (read-lhash-level lhash))))
                    (expt 2 (%lhash-level lhash))))
          (set-lhash-next-split lhash 0)
          (incf-lhash-level lhash)))))
  ;;(log:info "DONE SPLITTING ~A!" lhash)
  lhash)

(defun %lhash-insert (lhash key val)
  ;;(log:info "ADDING ~A/~A TO ~A" key val lhash)
  ;; Use %lhash-level (raw) — callers hold split-lock READ; re-acquiring via
  ;; read-lhash-level would deadlock if a writer is queued.
  (let ((bucket (hash lhash (%lhash-level lhash) key)) (split-p nil))
    (with-locked-hash-bucket (lhash bucket)
      (setq split-p
            (add-to-bucket lhash (%lhash-table lhash)
                           (bucket-offset lhash bucket) key val)))
    split-p))

(defun lhash-insert (lhash key val)
  (let ((split-p nil))
    (with-read-lock ((%lhash-split-lock lhash))
      (setq split-p (%lhash-insert lhash key val)))
    ;; split-p is set by %lhash-insert when a bucket overflows; that is the
    ;; only split trigger.  A secondary load-factor check here would call
    ;; read-lhash-level, re-acquiring the split-lock while a writer may
    ;; already be queued — new reads block, the queued writer never drains
    ;; R=1 → deadlock.
    (handler-case
        (when split-p
          (split-lhash lhash))
      (error (c)
        (log:error "LHASH ERROR IN ~A SPLIT(): ~A" lhash c)
        (error c)))
    (read-lhash-count lhash)))

(defun %lhash-update (lhash key val)
  (let ((bucket (hash lhash (%lhash-level lhash) key)))
    (with-locked-hash-bucket (lhash bucket)
      (update-in-bucket lhash (%lhash-table lhash)
                        (bucket-offset lhash bucket) key val))))

(defun lhash-update (lhash key val)
  (handler-case
      (with-read-lock ((%lhash-split-lock lhash))
        (%lhash-update lhash key val))
    (error (c)
      (log:error "LHASH ERROR IN ~A UPDATE-IN-BUCKET(~A,~A): ~A" lhash key val c)
      (error c))))

(defun %lhash-custom-update (lhash fn key)
  (let ((bucket (hash lhash (%lhash-level lhash) key)))
    (with-locked-hash-bucket (lhash bucket)
      (custom-update-in-bucket lhash (%lhash-table lhash)
                               (bucket-offset lhash bucket)
                               fn key))))

(defun lhash-custom-update (lhash fn key)
  (handler-case
      (with-read-lock ((%lhash-split-lock lhash))
        (%lhash-custom-update lhash fn key))
    (error (c)
      (log:error "LHASH ERROR IN ~A CUSTOM-UPDATE-IN-BUCKET(~A,~A): ~A"
                 lhash fn key c)
      (error c))))

(defun %lhash-get (lhash key)
  (let ((bucket (hash lhash (%lhash-level lhash) key)))
    (with-locked-hash-bucket (lhash bucket)
      (read-from-bucket lhash (%lhash-table lhash)
                        (bucket-offset lhash bucket) key))))

(defun lhash-get (lhash key)
  (handler-case
      (with-read-lock ((%lhash-split-lock lhash))
        (%lhash-get lhash key))
    (error (c)
      (log:error "ERROR IN ~A LHASH-GET(~A): ~A" lhash key c)
      (error c))))

;; Generic so a memory-graph's MEM-TABLE (used as the vertex/edge table) can
;; supply its own removal -- e.g. the peer scope-exit purge (PEER-PURGE-NODE)
;; hard-deletes a node straight from the table.  The on-disk lhash is the method.
(defgeneric lhash-remove (table key))

(defmethod lhash-remove ((lhash lhash) key)
  (handler-case
      (with-read-lock ((%lhash-split-lock lhash))
        (let* ((bucket (hash lhash (%lhash-level lhash) key)))
          (with-locked-hash-bucket (lhash bucket)
            (remove-from-bucket lhash (%lhash-table lhash)
                                (bucket-offset lhash bucket) key))))
    (error (c)
      (log:error "ERROR IN ~A LHASH-REMOVE(~A): ~A" lhash key c)
      (error c)))
  (read-lhash-count lhash))

;; Generic so an alternative table backend (the in-RAM MEM-TABLE of a
;; memory-graph) can supply its own scan; FN is called with a (KEY . VALUE) cons,
;; the same shape the lhash method yields.
(defgeneric map-lhash (fn table &key collect-p)
  (:method (fn (lhash lhash) &key collect-p)
    (with-read-lock ((%lhash-split-lock lhash))
      (let ((result nil) (bucket-count (%bucket-count lhash)))
        (dotimes (bucket bucket-count)
          (let* ((offset (bucket-offset lhash bucket))
                 (items (read-bucket lhash (%lhash-table lhash) offset)))
            (dolist (item items)
              (if collect-p
                  (push (funcall fn item) result)
                  (funcall fn item)))))
        (when collect-p
          (nreverse result))))))

(defun analyze-lhash (lhash)
  (with-read-lock ((%lhash-split-lock lhash))
    (let ((result nil) (bucket-count (%bucket-count lhash)))
      (dotimes (bucket bucket-count)
        (let* ((offset (bucket-offset lhash bucket))
               (items (read-bucket lhash (%lhash-table lhash) offset)))
          (when (> (length items) 0)
            (push (list (cons :bucket bucket)
                        (cons :length (length items)))
                  result))))
      (sort result '> :key 'cdadr))))
