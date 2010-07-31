(in-package #:vivace-graph)

(defstruct (predicate
	     (:conc-name pred-)
	     (:predicate predicate?))
  (uuid (make-uuid))
  (key nil)
  (name nil)
  (clauses nil)
  (func nil)
  (lock (make-recursive-lock))
  (graph *graph*))

(defgeneric save-predicate (predicate))

(defgeneric predicate-eql (p1 p2)
  (:method ((p1 predicate) (p2 predicate)) (eql (pred-name p1) (pred-name p2)))
  (:method (p1 p2) (error "Both arguments to predicate-eql must be predicates.")))

(defmethod serialize ((predicate predicate))
  (let* ((serialized-id (serialize (pred-uuid predicate)))
	 (serialized-name (serialize (pred-name predicate)))
	 (serialized-clauses (serialize (pred-clauses predicate)))
	 (total-length (+ (length serialized-id) (length serialized-name) 
			  (length serialized-clauses)))
	 (encoded-length (encode-length total-length))
	 (length-of-encoded-length (length encoded-length))
	 (vec (make-array (+ 1 length-of-encoded-length total-length) 
			  :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) +predicate+)
    (dotimes (i length-of-encoded-length)
      (setf (aref vec (1+ i)) (aref encoded-length i)))
    (dotimes (i (length serialized-id))
      (setf (aref vec (+ 1 length-of-encoded-length i)) (aref serialized-id i)))
    (dotimes (i (length serialized-name))
      (setf (aref vec (+ 1 length-of-encoded-length (length serialized-id) i)) 
	    (aref serialized-name i)))
    (dotimes (i (length serialized-clauses))
      (setf (aref vec (+ 1 length-of-encoded-length (length serialized-id) 
			 (length serialized-name) i))
	    (aref serialized-clauses i)))
    vec))

(defmethod deserialize-help ((become (eql +predicate+)) bytes)
  (destructuring-bind (id name clauses) (extract-all-subseqs bytes)
    (let ((predicate (make-predicate :uuid (deserialize id) 
				     :name (deserialize name) 
				     :clauses (deserialize clauses))))
      predicate)))

(defmethod deserialize-help ((become (eql +predicate-key+)) bytes)
  (deserialize bytes))

(defun make-predicate-key-from-name (name)
  (declare (optimize (speed 3)))
  (let* ((serialized-name (serialize name))
	 (encoded-length (encode-length (length serialized-name)))
	 (length-of-encoded-length (length encoded-length)))
    (let ((key (make-array (+ 1 length-of-encoded-length (length serialized-name)) 
			   :element-type '(unsigned-byte 8))))
      (setf (aref key 0) +predicate-key+)
      (dotimes (i length-of-encoded-length)
	(setf (aref key (1+ i)) (aref encoded-length i)))
      (dotimes (i (length serialized-name))
	(setf (aref key (+ 1 i length-of-encoded-length)) (aref serialized-name i)))
      key)))

(defmethod make-serialized-key ((predicate predicate))
  (or (pred-key predicate)
      (let ((key (make-predicate-key-from-name (pred-name predicate))))
	(cas (pred-key predicate) nil key)
	key)))

(defmethod cache-predicate ((predicate predicate))
  (setf (gethash (pred-name predicate) (predicate-cache (pred-graph predicate))) predicate))

(defmethod save-predicate ((predicate predicate))
  (let ((key (make-serialized-key predicate))
	(value (serialize predicate)))
    (store-object (rules-db (pred-graph predicate)) key value :mode :keep)))

(defmethod update-predicate ((predicate predicate))
  (let ((key (make-serialized-key predicate))
	(value (serialize predicate)))
    (store-object (rules-db (pred-graph predicate)) key value :mode :replace)))

(defgeneric lookup-predicate (name graph)
  (:method ((predicate predicate) (graph graph))
    (declare (ignore graph))
    predicate)
  (:method ((octets vector) (graph graph))
    (lookup-predicate (deserialize octets) graph))
  (:method ((name string) (graph graph))
    (lookup-predicate (intern name) graph))
  (:method ((name symbol) (graph graph))
    (or (gethash name (predicate-cache graph))
	(let ((raw (lookup-object (rules-db graph) (make-predicate-key-from-name name))))
	  (when (vectorp raw)
	    (let ((predicate (deserialize raw)))
	      (when (predicate? predicate)
		(setf (pred-graph predicate) graph
		      (pred-key predicate) (make-predicate-key-from-name name))
		(when (pred-clauses predicate)
		  (setf (pred-func predicate) (prolog-compile predicate)))
		(cache-predicate predicate))))))))

(defun make-new-predicate (&key name graph)
  (let ((name (or (and (symbolp name) name)
		  (and (stringp name) (intern name)))))
    (when (null name) (error "predicate name cannot be nil!"))
    (let* ((*graph* (or graph *graph*))
	   (predicate (make-predicate :name name :graph *graph*)))
      (handler-case
	  (save-predicate predicate)
	(persistence-error (condition)
	  (declare (ignore condition))
	  (lookup-predicate name *graph*))
	(:no-error (status)
	  (declare (ignore status))
	  (cache-predicate predicate)
	  predicate)))))

(defmethod add-default-rule ((predicate predicate))
  "Lock the predicate for compilation, add the clause, persist it and recompile."
  (format t "2. Adding default functor for ~A~%" predicate)
  (with-recursive-lock-held ((pred-lock predicate))
    (prolog-compile predicate))
  predicate)

(defmethod add-rule ((predicate predicate) clause)
  "Lock the predicate for compilation, add the clause, persist it and recompile."
  (setq clause (mapcar #'(lambda (g)
			   (when (stringp (first g)) 
			     (setf (nth 0 g) (intern (nth 0 g))))
			   g)
		       clause))
  (format t "2. Adding clause ~A / ~A~%" (pred-name predicate) clause)
  (with-recursive-lock-held ((pred-lock predicate))
    (let ((old-clauses (pred-clauses predicate)))
      (setf (pred-clauses predicate) (append old-clauses (list clause))))
    (update-predicate predicate)
    (prolog-compile predicate))
  (pred-clauses predicate))

(defmethod delete-rule (name)
  (let ((name (or (and (symbolp name) name)
		  (and (stringp name) (intern name)))))
    (remhash name (predicate-cache *graph*))
    (delete-object (rules-db *graph*) (make-predicate-key-from-name name))))


