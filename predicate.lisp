(in-package #:vivace-graph)

(defstruct (predicate
	     (:conc-name pred-)
	     (:predicate predicate?))
  (uuid (make-uuid))
  (key nil)
  (name nil)
  (clauses nil)
  (lock (make-recursive-lock))
  (graph *graph*))

(defgeneric save-predicate (predicate))

(defgeneric predicate-eql (p1 p2)
  (:method ((p1 predicate) (p2 predicate)) (eql (pred-name p1) (pred-name p2)))
  (:method (p1 p2) (error "Both arguments to predicate-eql must be predicates.")))

(defmethod serialize ((predicate predicate))
  (serialize-multiple +predicate+ 
		      (pred-uuid predicate)
		      (pred-name predicate)
		      (pred-clauses predicate)))

(defmethod deserialize-help ((become (eql +predicate+)) bytes)
  (destructuring-bind (id name clauses) (extract-all-subseqs bytes)
    (let ((predicate (make-predicate :uuid (deserialize id) 
				     :name (deserialize name) 
				     :clauses (deserialize clauses))))
      predicate)))

(defmethod deserialize-help ((become (eql +predicate-key+)) bytes)
  (deserialize bytes))

(defun make-predicate-key-from-name (name)
  (serialize-multiple +predicate-key+ (if (symbolp name) (symbol-name name) name)))

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
    (store-object (functor-db (pred-graph predicate)) key value :mode :keep)))

(defmethod update-predicate ((predicate predicate))
  (let ((key (make-serialized-key predicate))
	(value (serialize predicate)))
    (store-object (functor-db (pred-graph predicate)) key value :mode :replace)))

(defgeneric lookup-predicate (name graph)
  (:method ((predicate predicate) (graph graph))
    (declare (ignore graph))
    predicate)
  (:method ((octets vector) (graph graph))
    (lookup-predicate (deserialize octets) graph))
  (:method ((name string) (graph graph))
    (lookup-predicate (intern (string-upcase name)) graph))
  (:method ((name symbol) (graph graph))
    (or (gethash name (predicate-cache graph))
	(let ((raw (lookup-object (functor-db graph) (make-predicate-key-from-name name))))
	  (when (vectorp raw)
	    (let ((predicate (deserialize raw)))
	      (when (predicate? predicate)
		(setf (pred-graph predicate) graph
		      (pred-key predicate) (make-predicate-key-from-name name))
		(cache-predicate predicate))))))))

(defun lookup-predicate-functor (name)
  (or (gethash name (functors *graph*))
      (progn
	(lookup-predicate name *graph*)
	(gethash name (functors *graph*)))))

(defun make-new-predicate (&key name graph)
  (let ((name (or (and (symbolp name) (intern (string-upcase (symbol-name name))))
		  (and (stringp name) (intern (string-upcase name)))
		  (error "predicate name cannot be nil!"))))
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

(defmethod add-default-functor ((predicate predicate))
  "Lock the predicate for compilation, add the clause, persist it and recompile."
  ;(format t "2. Adding default functor for ~A~%" predicate)
  (with-recursive-lock-held ((pred-lock predicate))
    (prolog-compile predicate))
  predicate)

(defmethod add-functor ((predicate predicate) clause)
  "Lock the predicate for compilation, add the clause, persist it and recompile."
  ;(format t "2. Adding clause ~A / ~A~%" (pred-name predicate) clause)
  (with-recursive-lock-held ((pred-lock predicate))
    (let ((old-clauses (pred-clauses predicate)))
      (setf (pred-clauses predicate) (append old-clauses (list clause))))
    (update-predicate predicate)
    (prolog-compile predicate))
  (pred-clauses predicate))

(defmethod delete-functor (name)
  (let* ((name (or (and (symbolp name) name)
		   (and (stringp name) (intern (string-upcase name)))))
	 (predicate (lookup-predicate name *graph*)))
    (with-recursive-lock-held ((pred-lock predicate))
      (remhash name (predicate-cache *graph*))
      (delete-object (functor-db *graph*) (make-predicate-key-from-name name))
      (add-default-functor predicate))))
