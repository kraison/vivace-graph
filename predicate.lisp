(in-package #:vivace-graph)

(defstruct (predicate
	     (:conc-name pred-)
	     (:predicate predicate?))
  (uuid (make-uuid) :type uuid:uuid)
  (uri nil)
  (short-name "" :type string)
  (name "" :type string)
  (clauses nil :type list)
  (lock (make-recursive-lock) :type sb-thread:mutex)
  (graph *graph* :type graph))

(defgeneric save-predicate (predicate))
(defgeneric cache-predicate (predicate))
(defgeneric update-predicate (predicate))
(defgeneric add-functor (predicate &optional clause))
(defgeneric delete-functor (name))
(defgeneric reset-functor (name))

(defgeneric predicate-eql (p1 p2)
  (:method ((p1 predicate) (p2 predicate)) (equal (pred-name p1) (pred-name p2)))
  (:method (p1 p2) nil))

(defmethod save-predicate ((predicate predicate))
  (with-transaction ((predicate-db *graph*))
    (let ((key (pred-name predicate)))
      (set-phash (predicate-db *graph*) (make-slot-key key +uuid-slot+) (pred-uuid predicate))
      (set-phash (predicate-db *graph*) (make-slot-key key +name-slot+) (pred-name predicate))
      (set-phash (predicate-db *graph*) (make-slot-key key +clauses-slot+)
		 (pred-clauses predicate)))))

(defmethod delete-predicate ((predicate predicate))
  (with-transaction ((predicate-db *graph*))
    (let ((key (pred-name predicate)))
      (rem-phash (predicate-db *graph*) (make-slot-key key +uuid-slot+))
      (rem-phash (predicate-db *graph*) (make-slot-key key +name-slot+))
      (rem-phash (predicate-db *graph*) (make-slot-key key +clauses-slot+)))))

(defmethod update-predicate ((predicate predicate))
  (with-transaction ((predicate-db *graph*))
    (let ((key (pred-name predicate)))
      (set-phash (predicate-db *graph*) 
		 (make-slot-key key +clauses-slot+) (pred-clauses predicate) :mode :replace))))

(defmethod cache-predicate ((predicate predicate))
  (setf (gethash (pred-name predicate) (predicate-cache (pred-graph predicate))) predicate))

(defgeneric lookup-predicate (name)
  (:method ((predicate predicate))
    predicate)
  (:method ((name symbol))
    (or (lookup-predicate (symbol-name name))
	(lookup-predicate (string-downcase (symbol-name name)))))
  (:method ((name string))
    (or (gethash name (predicate-cache *graph*))
	(let ((uuid (get-phash (predicate-db *graph*) (make-slot-key name +uuid-slot+))))
	  (when (uuid:uuid? uuid)
	    (let ((p (make-predicate 
		      :uuid uuid
		      :graph *graph*
		      :name (get-phash (predicate-db *graph*) (make-slot-key name +name-slot+))
		      :clauses (get-phash (predicate-db *graph*)
					  (make-slot-key name +clauses-slot+)))))
	      (prolog-compile p)
	      (cache-predicate p)))))))

(defun make-new-predicate (&key name graph clauses)
  "Create a new predicate with name."
  (let ((*graph* (or graph *graph*))
	(name (typecase name
		(string name)
		(symbol (symbol-name name))
		(otherwise (error "predicate name must be either a string or a symbol!")))))
    (or (lookup-predicate name)
	(let ((predicate (make-predicate :name name :graph *graph* :clauses clauses)))
	  (handler-case
	      (save-predicate predicate)
	    (:no-error (status)
	      (declare (ignore status))
	      (add-functor predicate)
	      (cache-predicate predicate)
	      predicate))))))

(defmethod add-functor ((predicate predicate) &optional clause)
  "Lock the predicate for compilation, add the clause, persist it and recompile."
  (with-recursive-lock-held ((pred-lock predicate))
    (when clause
      (let ((old-clauses (pred-clauses predicate)))
	(setf (pred-clauses predicate) (append old-clauses (list clause))))
      (update-predicate predicate))
    (prolog-compile predicate)
    (or (pred-clauses predicate) predicate)))

(defmethod delete-functor (name)
  (let* ((name (typecase name
		 (string name)
		 (symbol (symbol-name name))
		 (otherwise (error "delete-functor: ~A is a ~A, should be string or symbol" 
				   name (type-of name)))))
	 (predicate (lookup-predicate name)))
    (with-recursive-lock-held ((pred-lock predicate))
      (remhash name (predicate-cache *graph*))
      (delete-predicate (lookup-predicate name)))))

(defmethod reset-functor (name)
  "Remove all clauses from a functor, making is a search-only predicate."
  (let* ((name (typecase name
		 (string name)
		 (symbol (symbol-name name))
		 (otherwise (error "delete-functor: ~A is a ~A, should be string or symbol" 
				   name (type-of name)))))
	 (predicate (lookup-predicate name)))
    (with-recursive-lock-held ((pred-lock predicate))  
      (setf (pred-clauses predicate) nil)
      (add-functor predicate))))
