(in-package #:vivace-graph)

(defgeneric serialize (object))
(defgeneric deserialize-help (become object))
(defgeneric make-serialized-key (object))
(defgeneric deserialize (object))
(defgeneric serialized-eq (x y)) 
(defgeneric serialized-lt (x y))
(defgeneric serialized-gt (x y))

(defun encode-length (int)
  (let* ((n-bytes (ceiling (integer-length int) 8))
	 (vec (make-array (+ 1 n-bytes) :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) n-bytes)
    (dotimes (i n-bytes)
      (setf (aref vec (+ 1 i)) (ldb (byte 8 0) int))
      (setq int (ash int -8)))
    vec))

(defun decode-length (bytes)
  (let ((int 0) (n-bytes (length bytes)))
    (dotimes (i n-bytes)
      (setq int (dpb (elt bytes i) (byte 8 (* i 8)) int)))
    int))

(defun extract-length (a)
  (let ((id-byte (aref a 0)))
    (cond ((or (= id-byte +uuid+) ;; These are all fixed length
	       (= id-byte +positive-integer+)
	       (= id-byte +negative-integer+)
	       (= id-byte +character+)
	       (= id-byte +single-float+)
	       (= id-byte +double-float+)
	       (= id-byte +timestamp+)
	       ;; FIXME: ratios should be variable length, given that they encode 2 integers
	       (= id-byte +ratio+))
	   (values (aref a 1) 2))
	  ((or (= id-byte +t+) (= id-byte +null+))
	   (values 1 0))
	  (t ;; strings, lists, vectors, blobs, nodes, triples have variable bytes
	   (let ((header-length (+ 2 (aref a 1))))
	     (values (decode-length (subseq a 2 header-length)) header-length))))))

(defun extract-all-subseqs (a)
  (declare (optimize (speed 3)))
  (cond ((= 0 (length a))
	 nil)
	(t 
	 (multiple-value-bind (data-length header-length) (extract-length a)
	   (cons (subseq a 0 (+ header-length data-length))
		 (extract-all-subseqs (subseq a (+ header-length data-length))))))))

(declaim (inline deserialize))
(defmethod deserialize ((a array))
  (declare (optimize speed))
  (multiple-value-bind (data-length header-length) (extract-length a)
    (declare (ignore data-length))
    (deserialize-help (aref a 0) (subseq a header-length))))

(defmethod serialized-eq ((x array) (y array))
  (equalp x y))

(defmethod serialized-lt ((x array) (y array))
  "Compare two serialized items."
  (declare (optimize speed))
  (less-than (deserialize x) (deserialize y)))

(defmethod serialized-gt ((x array) (y array))
  "Compare two serialized items."
  (declare (optimize speed))
  (greater-than (deserialize x) (deserialize y)))

(defmethod deserialize :around (object)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'deserialization-error :instance object :reason condition))))

(defmethod serialize :around (object)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'serialization-error :instance object :reason condition))))

(defun serialize-multiple (type-specifier &rest slots)
  (declare (optimize (speed 3)))
  (let* ((serialized-slots (mapcar #'serialize slots))
	 (serialized-slot-lengths (mapcar #'length serialized-slots))
	 (total-length (apply #'+ serialized-slot-lengths))
	 (encoded-length (encode-length total-length))
	 (length-of-encoded-length (length encoded-length)))
    (let ((a (make-array (+ 1 length-of-encoded-length total-length) 
			 :element-type '(unsigned-byte 8))))
      (setf (aref a 0) type-specifier)
      (dotimes (i length-of-encoded-length)
	(setf (aref a (1+ i)) (aref encoded-length i)))
      (dotimes (i (length serialized-slots))
	(dotimes (j (nth i serialized-slot-lengths))
	  (setf (aref a (+ 1 
			   length-of-encoded-length 
			   j 
			   (apply #'+ (subseq serialized-slot-lengths 0 i))))
		(aref (nth i serialized-slots) j))))
      a)))

(defmethod make-slot-key (id slot-name)
  (declare (optimize (speed 3)))
  (if (symbolp slot-name) (setq slot-name (symbol-name slot-name)))
  (let* ((serialized-id (serialize id))
	 (serialized-slot-name (serialize slot-name))
	 (total-length (+ (length serialized-id) (length serialized-slot-name))))
    (let ((a (make-array (+ 1 total-length) :element-type '(unsigned-byte 8))))
      (setf (aref a 0) +slot-key+)
      (dotimes (i (length serialized-id))
	(setf (aref a (1+ i)) (aref serialized-id i)))
      (dotimes (i (length serialized-slot-name))
	(setf (aref a (+ 1 (length serialized-id) i)) (aref serialized-slot-name i)))
      a)))
  
(defmethod deserialize-help ((become (eql +vector+)) bytes)
  (let ((items (extract-all-subseqs bytes)))
    (map 'vector #'deserialize items)))

(defmethod serialize ((v vector))
  (declare (optimize (speed 3)))
  (if (equal (array-element-type v) '(unsigned-byte 8))
      v
      (let* ((serialized-items (map 'list #'serialize v))
	     (total-length (reduce #'+ (mapcar #'length serialized-items)))
	     (encoded-length (encode-length total-length))
	     (length-of-encoded-length (length encoded-length))
	     (vec (make-array 1 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))
	(vector-push-extend +vector+ vec)
	(dotimes (i length-of-encoded-length)
	  (vector-push-extend (aref encoded-length i) vec))
	(dolist (item serialized-items)
	  (dotimes (i (length item))
	    (vector-push-extend (aref item i) vec)))
	vec)))

(defmethod deserialize-help ((become (eql +uuid+)) bytes)
  "Decode a UUID."
  (uuid:byte-array-to-uuid bytes))

(defmethod serialize ((uuid uuid:uuid))
  "Encode a UUID."
  (uuid:uuid-to-byte-array uuid +uuid+))

(defmethod deserialize-help ((become (eql +positive-integer+)) bytes)
  "Decode a positive integer."
  (let ((int 0) (n-bytes (length bytes)))
    (dotimes (i n-bytes)
      (setq int (dpb (elt bytes i) (byte 8 (* i 8)) int)))
    int))
 
(defmethod deserialize-help ((become (eql +negative-integer+)) bytes)
  "Decode a negative integer."
  (- (deserialize-help +positive-integer+ bytes)))

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

(defmethod deserialize-help ((become (eql +single-float+)) bytes)
  (ieee-floats:decode-float32 (deserialize-help +positive-integer+ bytes)))

(defmethod serialize ((float single-float))
  (let ((vec (serialize (ieee-floats:encode-float32 float))))
    (setf (aref vec 0) +single-float+)
    vec))

(defmethod deserialize-help ((become (eql +double-float+)) bytes)
  (ieee-floats:decode-float64 (deserialize-help +positive-integer+ bytes)))

(defmethod serialize ((float double-float))
  (let ((vec (serialize (ieee-floats:encode-float64 float))))
    (setf (aref vec 0) +double-float+)
    vec))

(defmethod deserialize-help ((become (eql +ratio+)) bytes)
  (let ((numerator (deserialize-help +positive-integer+ (subseq bytes 1 (+ 1 (elt bytes 0)))))
	(denominator (deserialize-help +positive-integer+ (subseq bytes (+ 2 (elt bytes 0))))))
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
    
(defmethod deserialize-help ((become (eql +character+)) bytes)
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
    (setf (aref vec 0) +character+)
    (setf (aref vec 1) total-bytes)
    (dotimes (i total-bytes)
      (setf (aref vec (+ 2 i)) (ldb (byte 8 0) code))
      (setq code (ash code -8)))
    vec))

(defmethod deserialize-help ((become (eql +string+)) bytes)
  (sb-ext:octets-to-string bytes))

(defmethod serialize ((string string))
  "Unicode aware string encoding. Not as efficient as it could be: creates 2 arrays: one to get 
sbcl's internal byte representation of the string, and then another for prepending our code and
the length of the object."
  (let* ((unicode (sb-ext:string-to-octets string))
	 (vector-length (length unicode))
	 (encoded-length (encode-length vector-length))
	 (length-of-encoded-length (length encoded-length))
	 (vec (make-array (+ 1 length-of-encoded-length vector-length) 
			  :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) +string+)
    (dotimes (i length-of-encoded-length)
      (setf (aref vec (1+ i)) (aref encoded-length i)))
    (dotimes (i vector-length)
      (setf (aref vec (+ 1 length-of-encoded-length i)) (aref unicode i)))
    vec))

(defmethod deserialize-help ((become (eql +t+)) bytes)
  t)

(defmethod deserialize-help ((become (eql +null+)) bytes)
  nil)

(defmethod deserialize-help ((become (eql +symbol+)) bytes)
  (destructuring-bind (symbol package) (extract-all-subseqs bytes)
    (intern (deserialize symbol) (find-package (deserialize package)))))

(defmethod serialize ((symbol symbol))
  (declare (optimize (speed 3)))
  (or (and (null symbol) #(#.+null+))
      (and (eq symbol t) #(#.+t+))
      (let* ((symbol-name (serialize (symbol-name symbol)))
	     (package-name (serialize (package-name (symbol-package symbol))))
	     (size (+ (length symbol-name) (length package-name)))
	     (encoded-length (encode-length size))
	     (vec (make-array (+ 1 (length encoded-length) size) :element-type '(unsigned-byte 8))))
	;(format t "serializing symbol ~A in ~A~%" symbol-name package-name)
	(setf (aref vec 0) +symbol+)
	(dotimes (i (length encoded-length))
	  (setf (aref vec (1+ i)) (aref encoded-length i)))
	(dotimes (i (length symbol-name))
	  (setf (aref vec (+ 1 (length encoded-length) i)) 
		(aref symbol-name i)))
	(dotimes (i (length package-name))
	  (setf (aref vec (+ 1 (length encoded-length) (length symbol-name) i))
		(aref package-name i)))
	vec)))

(defmethod deserialize-help ((become (eql +list+)) bytes)
  (let ((items (extract-all-subseqs bytes)))
    (mapcar #'deserialize items)))

(defmethod deserialize-help ((become (eql +dotted-list+)) bytes)
  (let* ((items (extract-all-subseqs bytes))
	 (result nil))
    (loop for i downfrom (- (length items) 2) to 0 do
      (push (deserialize (nth i items)) result))
    (nconc result (deserialize (car (last items))))))

(defmethod serialize ((list list))
  (declare (optimize (speed 3)))
  (if (proper-listp list)
      (let* ((serialized-items (mapcar #'serialize list))
	     (total-length (reduce #'+ (mapcar #'length serialized-items)))
	     (encoded-length (encode-length total-length))
	     (length-of-encoded-length (length encoded-length))
	     (vec (make-array 1 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))
	(vector-push-extend +list+ vec)
	(dotimes (i length-of-encoded-length)
	  (vector-push-extend (aref encoded-length i) vec))
	(dolist (item serialized-items)
	  (dotimes (i (length item))
	    (vector-push-extend (aref item i) vec)))
	vec)
      (let ((vec (make-array 1 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8)))
	    (serialized-items nil))
	(loop for elt on list do
	     ;;(format t "Serializing ~A~%" (car elt))
	     (push (serialize (car elt)) serialized-items)
	     (when (atom (cdr elt))
	       ;; The last element
	       (push (serialize (cdr elt)) serialized-items)))
	(let* ((total-length (reduce #'+ (mapcar #'length serialized-items)))
	       (encoded-length (encode-length total-length))
	       (length-of-encoded-length (length encoded-length)))
	  (vector-push-extend +dotted-list+ vec)
	  (dotimes (i length-of-encoded-length)
	    (vector-push-extend (aref encoded-length i) vec))
	  (dolist (item (reverse serialized-items))
	    (dotimes (i (length item))
	      (vector-push-extend (aref item i) vec)))
	  vec))))




