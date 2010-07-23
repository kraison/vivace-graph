(in-package #:vivace-graph)

(defgeneric serialize (object))
(defgeneric deserialize (become object))
(defgeneric deserialize-raw (object))
(defgeneric serialized-eq (x y)) 
(defgeneric serialized-lt (x y))
(defgeneric serialized-gt (x y))

(declaim (inline deserialize-raw))
(defmethod deserialize-raw ((a array))
  (declare (optimize speed))
  (deserialize (aref a 0) (subseq a 2)))

(defmethod serialized-eq ((x array) (y array))
  (equalp x y))

(defmethod serialized-lt ((x array) (y array))
  "Compare two serialized items.
FIXME: there is a way to do this without deserializing, it will just take some time to get right."
  (declare (optimize speed))
  (less-than (deserialize-raw x) (deserialize-raw y)))

(defmethod serialized-gt ((x array) (y array))
  "Compare two serialized items.
FIXME: there is a way to do this without deserializing, it will just take some time to get right."
  (declare (optimize speed))
  (greater-than (deserialize-raw x) (deserialize-raw y)))

(defmethod deserialize :around (become object)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'deserialization-error :instance object :reason condition))))

(defmethod serialize :around (object)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'serialization-error :instance object :reason condition))))

(defmethod deserialize ((become (eql +uuid+)) bytes)
  "Decode a UUID."
  (uuid:byte-array-to-uuid bytes))

(defmethod serialize ((uuid uuid:uuid))
  "Encode a UUID."
  (uuid:uuid-to-byte-array uuid +uuid+))

(defmethod deserialize ((become (eql +positive-integer+)) bytes)
  "Decode a positive integer."
  (let ((int 0) (n-bytes (length bytes)))
    (dotimes (i n-bytes)
      (setq int (dpb (elt bytes i) (byte 8 (* i 8)) int)))
    int))
 
(defmethod deserialize ((become (eql +negative-integer+)) bytes)
  "Decode a negative integer."
  (- (deserialize +positive-integer+ bytes)))

(defmethod serialize ((int integer))
  "Encodes integers between (- (1- (expt 2 (* 8 255)))) and (1- (expt 2 (* 8 255)))"
  (let* ((n-bytes (ceiling (integer-length int) 8))
	 (vec (make-array (+ 2 n-bytes) :element-type '(unsigned-byte 8))))
    (if (minusp int)
	(progn
	  (setf (aref vec 0) +negative-integer+)
	  (setq int (abs int)))
	(setf (aref vec 0) +positive-integer+))
    (setf (aref vec 1) n-bytes)
    (dotimes (i n-bytes)
      (setf (aref vec (+ 2 i)) (ldb (byte 8 0) int))
      (setq int (ash int -8)))
    vec))

(defmethod deserialize ((become (eql +single-float+)) bytes)
  (ieee-floats:decode-float32 (deserialize +positive-integer+ bytes)))

(defmethod serialize ((float single-float))
  (let ((vec (serialize (ieee-floats:encode-float32 float))))
    (setf (aref vec 0) +single-float+)
    vec))

(defmethod deserialize ((become (eql +double-float+)) bytes)
  (ieee-floats:decode-float64 (deserialize +positive-integer+ bytes)))

(defmethod serialize ((float double-float))
  (let ((vec (serialize (ieee-floats:encode-float64 float))))
    (setf (aref vec 0) +double-float+)
    vec))

(defmethod deserialize ((become (eql +ratio+)) bytes)
  (let ((numerator (deserialize +positive-integer+ (subseq bytes 1 (+ 1 (elt bytes 0)))))
	(denominator (deserialize +positive-integer+ (subseq bytes (+ 2 (elt bytes 0))))))
    (/ numerator denominator)))

(defmethod serialize ((ratio ratio))
  (let ((vec (make-array 2 :fill-pointer t :adjustable t :element-type '(unsigned-byte 8)))
	(numerator (numerator ratio))
	(denominator (denominator ratio)))
    (flet ((serialize-int (int)
	     (let ((n-bytes (ceiling (integer-length int) 8)))
	       (vector-push-extend n-bytes vec)
	       (dotimes (i n-bytes)
		 (vector-push-extend (ldb (byte 8 0) int) vec)
		 (setq int (ash int -8))))))
      (setf (aref vec 0) +ratio+)
      (serialize-int numerator)
      (serialize-int denominator)
      (setf (aref vec 1) (- (length vec) 2))
      vec)))
    
(defmethod deserialize ((become (eql +character+)) bytes)
  "Decode a Unicode-encoded byte sequence."
  (let ((int 0) (n-bytes (length bytes)))
    (dotimes (i n-bytes)
      (setq int (dpb (elt bytes i) (byte 8 (* i 8)) int)))
    (code-char int)))

(defmethod serialize ((char character))
  "Encode a Unicode character."
  (let* ((code (char-code char))
	 (total-bytes (ceiling (integer-length code) 8))
	 (vec (make-array (+ 2 total-bytes) :element-type '(unsigned-byte 8))))
    ;;(format t "~A (~A): BYTES: ~A~%" code char total-bytes)
    (setf (aref vec 0) +string+)
    (setf (aref vec 1) total-bytes)
    (dotimes (i total-bytes)
      (setf (aref vec (+ 2 i)) (ldb (byte 8 0) code))
      (setq code (ash code -8)))
    vec))

(defmethod deserialize ((become (eql +string+)) bytes)
  (sb-ext:octets-to-string bytes))

(defmethod serialize ((string string))
  "Unicode aware string encoding. Not as efficient as it could be: creates 2 arrays: one to get 
sbcl's internal byte representation of the string, and then another for prepending our code and
the length of the object."
  (let* ((unicode (sb-ext:string-to-octets string))
	 (vector-length (length unicode))
	 (vec (make-array (+ 2 vector-length) 
			  :fill-pointer t :adjustable t :element-type '(unsigned-byte 8))))
    (if (< (length string) 256)
	(setf (aref vec 0) +string+)
	(setf (aref vec 0) +clob+))
    (setf (aref vec 1) vector-length)
    (dotimes (i vector-length)
      (setf (aref vec (+ 2 i)) (aref unicode i)))
    vec))

(defmethod deserialize ((become (eql +symbol+)) bytes)
  (let* ((symbol-size (elt bytes 0))
	 (package-size (elt bytes (1+ symbol-size))))
    (intern (sb-ext:octets-to-string (subseq bytes 1 (1+ symbol-size)))
	    (find-package (sb-ext:octets-to-string 
			   (subseq bytes (+ 2 symbol-size) (+ 2 symbol-size package-size)))))))

(defmethod serialize ((symbol symbol))
  (let* ((symbol-name (sb-ext:string-to-octets (symbol-name symbol)))
	 (package-name (sb-ext:string-to-octets (package-name (symbol-package symbol))))
	 (total-size (+ 4 (length symbol-name) (length package-name)))
	 (vec (make-array total-size :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) +symbol+)
    (setf (aref vec 1) (- total-size 2))
    (setf (aref vec 2) (length symbol-name))
    (dotimes (i (aref vec 2))
      (setf (aref vec (+ 3 i)) (aref symbol-name i)))    
    (setf (aref vec (+ 3 (aref vec 2))) (length package-name))
    (dotimes (i (aref vec (+ 3 (aref vec 2))))
      (setf (aref vec (+ 3 1 (aref vec 2) i)) (aref package-name i)))
    vec))
