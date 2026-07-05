(in-package #:uuid)

(require :cffi)

(export 'time-low)
(export 'time-mid)
(export 'time-high)
(export 'clock-seq-var)
(export 'clock-seq-low)
(export 'node)
(export 'time-high-and-version)
(export 'clock-seq-and-reserved)
(export 'uuid-eql)
(export 'uuid?)
(export 'mmap-array-to-uuid)
(export 'uuid-to-mfp)

(defgeneric uuid? (thing)
  (:method ((thing uuid)) t)
  (:method (thing) nil)
  (:documentation "UUID type predicate."))

(defgeneric uuid-eql (uuid1 uuid2)
  (:method ((uuid1 uuid) (uuid2 uuid))
    (equalp (uuid-to-byte-array uuid1) (uuid-to-byte-array uuid2)))
  (:method ((uuid1 uuid) uuid2)
    nil)
  (:method (uuid1 (uuid2 uuid))
    nil)
  (:documentation "Equality check for UUIDs."))

(defmethod print-object ((id uuid:uuid) stream)
  "Prints an uuid in the string represenation of an uuid. (example string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
  (format stream "~(~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X~)"
          (time-low id)
          (time-mid id)
          (time-high id)
          (clock-seq-var id)
          (clock-seq-low id)
          (node id)))

(defun set-byte (mfp offset byte)
  (setf (cffi:mem-aref mfp :unsigned-char offset) byte))
  ;;(setf (sb-alien:deref mfp offset) byte))

(defun get-byte (mfp offset)
  (cffi:mem-aref mfp :unsigned-char offset))
  ;;(sb-alien:deref mfp offset))

(defmacro mmap-array-to-bytes (from to mfp)
  "Helper macro used in byte-array-to-uuid."
  `(loop for i from ,from to ,to
         with res = 0
         do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (get-byte ,mfp i))
         finally (return res)))

(defun mmap-array-to-uuid (mfp offset)
  "Converts a byte-array generated with uuid-to-byte-array to an uuid."
  (make-instance 'uuid
                 :time-low (mmap-array-to-bytes offset (+ 3 offset) mfp)
                 :time-mid (mmap-array-to-bytes (+ 4 offset) (+ 5 offset) mfp)
                 :time-high (mmap-array-to-bytes (+ 6 offset) (+ 7 offset) mfp)
                 :clock-seq-var (get-byte mfp (+ 8 offset))
                 :clock-seq-low (get-byte mfp (+ 9 offset))
                 :node (mmap-array-to-bytes (+ 10 offset) (+ 15 offset) mfp)))

(defun uuid-to-mfp (uuid mfp offset &optional type-specifier)
  "Converts an uuid to mmap'ed file chunk."
  (if type-specifier
      (set-byte mfp offset type-specifier)
      (decf offset))
  (with-slots
        (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
      uuid
    (loop for i from 3 downto 0
       do (set-byte mfp (incf offset) (ldb (byte 8 (* 8 i)) time-low)))
    (loop for i from 1 downto 0
       do (set-byte mfp (incf offset) (ldb (byte 8 (* 8 i)) time-mid)))
    (loop for i from 1 downto 0
       do (set-byte mfp (incf offset) (ldb (byte 8 (* 8 i)) time-high-and-version)))
    (set-byte mfp (incf offset) (ldb (byte 8 0) clock-seq-and-reserved))
    (set-byte mfp (incf offset) (ldb (byte 8 0) clock-seq-low))
    (loop for i from 5 downto 0
       do (set-byte mfp (incf offset) (ldb (byte 8 (* 8 i)) node)))
    (incf offset)))

(defun uuid-to-byte-array (uuid &optional (type-specifier nil))
  "Converts an uuid to byte-array"
  (if type-specifier
      (let ((array (make-array 18 :element-type '(unsigned-byte 8))))
        (setf (aref array 0) type-specifier)
        (setf (aref array 1) 16)
        (with-slots
              (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
            uuid
          (loop for i from 3 downto 0
             do (setf (aref array (+ 2 (- 3 i))) (ldb (byte 8 (* 8 i)) time-low)))
          (loop for i from 5 downto 4
             do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
          (loop for i from 7 downto 6
             do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
          (setf (aref array (+ 2 8)) (ldb (byte 8 0) clock-seq-and-reserved))
          (setf (aref array (+ 2 9)) (ldb (byte 8 0) clock-seq-low))
          (loop for i from 15 downto 10
             do (setf (aref array (+ 2 i)) (ldb (byte 8 (* 8 (- 15 i))) node)))
          array))
      (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
        (with-slots
              (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
            uuid
          (loop for i from 3 downto 0
             do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) time-low)))
          (loop for i from 5 downto 4
             do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
          (loop for i from 7 downto 6
             do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
          (setf (aref array 8) (ldb (byte 8 0) clock-seq-and-reserved))
          (setf (aref array 9) (ldb (byte 8 0) clock-seq-low))
          (loop for i from 15 downto 10
             do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) node)))
          array))))
