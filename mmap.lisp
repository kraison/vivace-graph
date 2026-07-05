(in-package :graph-db)

#+lispworks(deftype word () '(unsigned-byte 64))
#+ecl(deftype word () '(unsigned-byte 64))
(cffi:defctype size :unsigned-int)
(deftype uint32 () '(integer 0 4294967295))
(deftype uint40 () '(integer 0 1099511627775))
(deftype uint64 () '(integer 0 18446744073709551615))

(defstruct (mapped-file
             (:conc-name m-)
             (:predicate mapped-file-p))
  path pointer fd
  ;; Length, in bytes, of the virtual-address window reserved for this mapping
  ;; (see *mmap-reservation-size*).  POINTER is fixed at the base of that window
  ;; for the life of the mapping: the file is mapped into the head, and
  ;; extend-mapped-file maps more of it into the reserved tail with MAP_FIXED.
  ;; Because POINTER never moves and the reservation is never unmapped until
  ;; close, concurrent readers never fault and need no lock.  See
  ;; docs/mmap-remap-race-plan.md.
  (reserved-size 0 :type integer))

;;; Diagnostic: count SEGV-retries in the accessor :around methods.  With the
;;; stable-address mapping no remap moves the pointer, so this must stay 0 under
;;; concurrency; the regression test asserts it.  The :around handlers remain a
;;; cheap backstop.  Plain incf (a racy count is fine for a diagnostic).
(defparameter *mmap-segv-retries* 0)

(defstruct mpointer mmap loc)

(defmethod mapped-file-length ((mapped-file mapped-file))
  (%posix-file-size-fd (m-fd mapped-file)))

(defmethod set-byte :around (mf offset byte)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))))

;; Raw write.  Safe lock-free because the mapping's base pointer is stable for
;; its lifetime (see mmap-file / extend-mapped-file).
(declaim (inline %set-byte))
(defun %set-byte (mapped-file offset byte)
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  #+ecl
  (ffi:c-inline ((m-pointer mapped-file) offset byte)
                (:pointer-void :cl-index :unsigned-byte) :unsigned-byte
                "*((unsigned char *)(((char*)#0)+#1))=#2"
                :one-liner t)
  #-ecl
  (setf (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset) byte))

(defmethod set-byte ((mapped-file mapped-file) offset byte)
  ;;(log:debug "SET-BYTE: ~A ADDR ~A TO ~A" (m-path mapped-file) offset byte)
  ;; Lock-free: the pointer is stable (see mmap-file / extend-mapped-file).
  (%set-byte mapped-file offset byte))

(defmethod get-byte :around (mf offset)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-byte mf offset))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-byte mf offset))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-byte mf offset))))

;; Raw read.  Safe lock-free because the mapping's base pointer is stable.
(declaim (inline %get-byte))
(defun %get-byte (mapped-file offset)
  (declare (type word offset))
  #+ecl
  (ffi:c-inline ((m-pointer mapped-file) offset)
                (:pointer-void :cl-index) :unsigned-byte
                "*((unsigned char *)(((char*)#0)+#1))"
                :one-liner t)
  #-ecl
  (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset))

(defmethod get-byte ((mapped-file mapped-file) offset)
  ;; Lock-free: the pointer is stable (see mmap-file / extend-mapped-file).
  (%get-byte mapped-file offset))

(defmethod get-bytes :around (mf offset length)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-bytes mf offset length))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-bytes mf offset length))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-bytes mf offset length))))

(defmethod get-bytes ((mapped-file mapped-file) offset length)
  (declare (type word offset length))
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (%get-byte mapped-file (+ i offset))))
    vec))

(defmethod set-bytes :around (mf vec offset length)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))))

(defmethod set-bytes ((mapped-file mapped-file) vec offset length)
  (declare (type word offset length))
  (dotimes (i length)
    (%set-byte mapped-file (+ i offset) (aref vec i)))
  vec)

(defmethod size-of ((mmap mapped-file))
  (%file-size (m-path mmap)))

(defun mmap-file (file &key (create-p t) (size (* 4096 25600)) reservation)
  "Use mmap() to map FILE into memory.

Reserves a virtual-address window (PROT_NONE, anonymous, MAP_NORESERVE — address
space only) and maps the file into the head of it with MAP_FIXED.  The returned
mapped-file's POINTER is the base of that window and is stable for the life of
the mapping; EXTEND-MAPPED-FILE grows by re-mapping the file into the reserved
window, so the pointer never moves and concurrent readers never fault.  The file
may grow up to the reservation, which defaults to *MMAP-RESERVATION-MULTIPLIER*
times SIZE (floored at *MMAP-MIN-RESERVATION*); pass RESERVATION to override."
  (log:debug "Opening mmap ~A" file)
  (when (and (not create-p) (not (probe-file file)))
    (error "mmap-file: ~A does not exist and create-p is not true." file))
  (let* ((fd (%posix-open
              file
              (if create-p
                  (logior +o-creat+ +o-rdwr+)
                  +o-rdwr+))))
    (when create-p
      (%posix-lseek fd (1- size) +seek-set+)
      (cffi:with-foreign-string (null (format nil "~A" (code-char 0)))
        (cffi:foreign-funcall "write"
                              :int fd
                              :pointer null
                              size 1))
      ;; Belt-and-suspenders: set the mode explicitly to #o640 (owner rw, group
      ;; r) so database files are reopenable without being world-accessible,
      ;; even if the open() mode argument is not honored on some platform.
      (%posix-fchmod fd #o640))
    ;; Make sure the file size is set right!
    (setq size (%posix-file-size-fd fd))
    (let* ((reserved (max (or reservation
                              (* *mmap-reservation-multiplier* size))
                          *mmap-min-reservation*
                          size))
           ;; Reserve the address window with no access and no backing.
           (base (%posix-mmap
                  (cffi:null-pointer)
                  reserved
                  +prot-none+
                  (logior +map-private+
                          +map-anonymous+
                          +map-noreserve+)
                  -1
                  0))
           ;; Map the file over the head of the reservation (replaces the
           ;; PROT_NONE pages for [base, base+size); MAP_FIXED keeps the addr).
           (pointer (%posix-mmap
                     base
                     size
                     (logior +prot-read+ +prot-write+)
                     (logior +map-shared+ +map-fixed+)
                     fd
                     0)))
      (make-mapped-file :path (truename file)
                        :fd fd
                        :pointer pointer
                        :reserved-size reserved))))

(defmethod sync-region ((mapped-file mapped-file) &key addr length
                        (sync +ms-sync+))
  (%posix-msync (or addr (m-pointer mapped-file))
                (or length (mapped-file-length mapped-file))
                sync))

(defmethod munmap-file ((mapped-file mapped-file) &key (save-p nil)
                        (sync +ms-sync+))
  (when save-p
    ;;(log:debug "Calling msync on ~S" mapped-file)
    ;; Only the file-backed head is dirty/syncable, not the reserved tail.
    (%posix-msync (m-pointer mapped-file)
                  (mapped-file-length mapped-file)
                  sync))
  ;;(log:debug "Calling munmap on ~S" mapped-file)
  ;; Release the whole reserved window (file mapping + PROT_NONE tail).
  (%posix-munmap (m-pointer mapped-file)
                 (if (plusp (m-reserved-size mapped-file))
                     (m-reserved-size mapped-file)
                     (mapped-file-length mapped-file)))
  (%posix-close (m-fd mapped-file))
  (setf (m-pointer mapped-file) nil)
  nil)

;; One platform-independent implementation: there is no mremap and no munmap of
;; the live region.  We grow the backing file, then re-map the whole file at the
;; SAME base address (MAP_FIXED, offset 0).  MAP_FIXED replacement is atomic, so
;; a concurrent reader of an existing offset never observes an unmapped address,
;; and the base pointer is unchanged — no lock and no SEGV.  See
;; docs/mmap-remap-race-plan.md.
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  (log:debug "EXTENDING MMAP ~A" mapped-file)
  (let* ((old-len (mapped-file-length mapped-file))
         (new-len (+ old-len length)))
    (when (> new-len (m-reserved-size mapped-file))
      (error "mmap reservation exhausted for ~A: need ~D bytes, reserved ~D.~%~
Raise *mmap-reservation-size* (or MAKE-GRAPH's heap/index size) before creating ~
the graph."
             (m-path mapped-file) new-len (m-reserved-size mapped-file)))
    ;; Extend the backing file first so the newly mapped pages have storage.
    (%posix-lseek (m-fd mapped-file) (1- new-len) +seek-set+)
    (cffi:with-foreign-string (null (format nil "~A" (code-char 0)))
      (cffi:foreign-funcall "write"
                            :int (m-fd mapped-file)
                            :pointer null
                            size 1))
    ;; Re-map [0, new-len) over the reserved window at the same base.
    (%posix-mmap (m-pointer mapped-file)
                 new-len
                 (logior +prot-read+ +prot-write+)
                 (logior +map-shared+ +map-fixed+)
                 (m-fd mapped-file)
                 0)
    mapped-file))

(defmethod serialize-uint64 ((mf mapped-file) int offset)
  (declare (type word int offset))
  ;;(log:debug "MMAP: SERIALIZING UINT64 ~A TO ADDR ~A" int offset)
  (dotimes (i 8)
    (%set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  offset)

(defmethod deserialize-uint64 ((mf mapped-file) offset)
  "Decode a UINT64."
  (declare (type word offset))
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (%get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod deserialize-uint64 ((array array) offset)
  "Decode a UINT64."
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (aref array (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod incf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (incf int)
    (serialize-uint64 mf int offset)
    int))

(defmethod decf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (serialize-uint64 mf (decf int) offset)
    int))

(defmethod serialize-pointer ((mf mapped-file) pointer offset)
  ;;(log:debug "SERIALIZING POINTER ~A TO ADDR ~A" pointer offset)
  (serialize-uint64 mf pointer offset))

(defmethod deserialize-pointer ((mf mapped-file) offset)
  ;;(log:debug "DESERIALIZING POINTER AT ADDR ~A" offset)
  (deserialize-uint64 mf offset))

(defmethod serialize-uint32 ((mf mapped-file) int offset)
  (declare (type uint32 int))
  (declare (type word offset))
  (dotimes (i 4)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  (incf offset))

(defmethod deserialize-uint32 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint32 int))
    (dotimes (i 4)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod serialize-uint40 ((mf mapped-file) int offset)
  (declare (type uint40 int))
  (declare (type word offset))
  (dotimes (i 5)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  (incf offset))

(defmethod deserialize-uint40 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint40 int))
    (dotimes (i 5)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))
