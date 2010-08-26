(in-package #:vivace-graph)

(defstruct (predicate
	     (:conc-name pred-)
	     (:predicate predicate?))
  (uuid (make-uuid) :type uuid:uuid)
  (name nil :type symbol)
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
  (:method ((p1 predicate) (p2 predicate)) (eql (pred-name p1) (pred-name p2)))
  (:method (p1 p2) nil))

(defmethod save-predicate ((predicate predicate))
  (with-transaction ((functor-db *graph*))
    (let ((key (string-downcase (symbol-name (pred-name predicate)))))
      (set-phash (functor-db *graph*) (make-slot-key key "uuid") (pred-uuid predicate))
      (set-phash (functor-db *graph*) (make-slot-key key "name") (pred-name predicate))
      (set-phash (functor-db *graph*) (make-slot-key key "clauses") (pred-clauses predicate)))))

(defmethod delete-predicate ((predicate predicate))
  (with-transaction ((functor-db *graph*))
    (let ((key (string-downcase (symbol-name (pred-name predicate)))))
      (rem-phash (functor-db *graph*) (make-slot-key key "uuid"))
      (rem-phash (functor-db *graph*) (make-slot-key key "name"))
      (rem-phash (functor-db *graph*) (make-slot-key key "clauses")))))

(defmethod update-predicate ((predicate predicate))
  (with-transaction ((functor-db *graph*))
    (let ((key (string-downcase (symbol-name (pred-name predicate)))))
      (set-phash (functor-db *graph*) 
		 (make-slot-key key "clauses") (pred-clauses predicate) :mode :replace))))

(defmethod cache-predicate ((predicate predicate))
  (setf (gethash (pred-name predicate) (predicate-cache (pred-graph predicate))) predicate))

(defgeneric lookup-predicate (name)
  (:method ((predicate predicate))
    predicate)
  (:method ((name string))
    (lookup-predicate (intern (string-upcase name))))
  (:method ((name symbol))
    (or (gethash name (predicate-cache *graph*))
	(let ((key (string-downcase (symbol-name name))))
	  (let ((uuid (get-phash (functor-db *graph*) (make-slot-key key "uuid"))))
	    (when (uuid:uuid? uuid)
	      (let ((p (make-predicate 
			:uuid uuid
			:graph *graph*
			:name (get-phash (functor-db *graph*) (make-slot-key key "name"))
			:clauses (get-phash (functor-db *graph*)
					    (make-slot-key key "clauses")))))
		(prolog-compile p)
		(cache-predicate p))))))))

(defun lookup-predicate-functor (name)
  (gethash name (functors *graph*)))

(defun make-new-predicate (&key name graph clauses)
  "Create a new predicate with name."
  (let ((*graph* (or graph *graph*))
	(name (or (and (symbolp name) (intern (string-upcase (symbol-name name))))
		  (and (stringp name) (intern (string-upcase name)))
		  (error "predicate name must be either a string or a symbol!"))))
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
  (let* ((name (or (and (symbolp name) name)
		   (and (stringp name) (intern (string-upcase name)))))
	 (predicate (lookup-predicate name)))
    (with-recursive-lock-held ((pred-lock predicate))
      (remhash name (predicate-cache *graph*))
      (delete-predicate (lookup-predicate name)))))

(defmethod reset-functor (name)
  "Remove all clauses from a functor, making is a search-only predicate."
  (let* ((name (or (and (symbolp name) name)
		   (and (stringp name) (intern (string-upcase name)))))
	 (predicate (lookup-predicate name)))
    (with-recursive-lock-held ((pred-lock predicate))  
      (setf (pred-clauses predicate) nil)
      (add-functor predicate))))
