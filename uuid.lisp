(in-package #:uuid)

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
