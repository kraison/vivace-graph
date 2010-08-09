(in-package #:vivace-graph)

(defvar *prolog-global-functors* (make-hash-table :synchronized t))

(defmacro def-global-prolog-functor (name lambda-list &body body)
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (setf (gethash ',name *prolog-global-functors*) #',name)))

(def-global-prolog-functor read/1 (exp cont)
  (if (unify! exp (read)) (funcall cont)))

(def-global-prolog-functor write/1 (exp cont)
  (format t "~A" (deref-exp exp)) (funcall cont))

(def-global-prolog-functor nl/0 (cont) 
  (terpri) (funcall cont))

(def-global-prolog-functor repeat/0 (cont)
  (loop (funcall cont)))

(def-global-prolog-functor fail/0 (cont)
  (declare (ignore cont))
  nil)

(def-global-prolog-functor =/2 (?arg1 ?arg2 cont)
  (if (unify! ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)	       
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor /=/2 (?arg1 ?arg2 cont)	       
  (if (not (deref-equal ?arg1 ?arg2)) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)	      
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (> ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp> ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (< ?arg1 ?arg2))
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp< ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor >=/2 (?arg1 ?arg2 cont)	      
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (>= ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp>= ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor <=/2 (?arg1 ?arg2 cont)
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (<= ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp<= ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  (if (unify! ?result (eval (deref-exp exp)))
      (funcall cont)))

(def-global-prolog-functor regex-match/2 (?arg1 ?arg2 cont)
  (if (and (stringp (var-deref ?arg1)) (stringp (var-deref ?arg2)) (cl-ppcre:scan ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor var/1 (?arg1 cont)
  (if (unbound-var-p ?arg1) (funcall cont)))

(def-global-prolog-functor is/2 (var exp cont)
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
	   (unify! var (eval (deref-exp exp))))
      (funcall cont)))

(def-global-prolog-functor call/1 (goal cont)
  (var-deref goal)
  (let ((functor (make-functor (first goal) (length (args goal)))))
    (apply (or (gethash functor (functors *graph*)) 
	       (gethash functor *prolog-global-functors*)
	       (error 'prolog-error :reason (format nil "Unknown Prolog functor ~A" functor)))
	   (append (args goal) (list cont)))))

(def-global-prolog-functor not/1 (relation cont)	      
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(def-global-prolog-functor bagof/3 (exp goal result cont)
  (let ((answers nil))
    (call/1 goal #'(lambda () (push (deref-copy exp) answers)))
    (if (and (not (null answers))
	     (unify! result (nreverse answers)))
	(funcall cont))))

(def-global-prolog-functor setof/3 (exp goal result cont)
  (let ((answers nil))
    (call/1 goal #'(lambda () (push (deref-copy exp) answers)))
    (if (and (not (null answers))
	     (unify! result (delete-duplicates answers :test #'deref-equal)))
	(funcall cont))))

(def-global-prolog-functor show-prolog-vars/2 (var-names vars cont)
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
	 for var in vars do
	   (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(def-global-prolog-functor select/2 (var-names vars cont)
  (if (null vars)
      nil
      (push (loop for name in var-names
	       for var in vars
	       collect (deref-exp var))
	    *select-list*))
  (funcall cont))

(def-global-prolog-functor select-as-bind-alist/2 (var-names vars cont)
  (if (null vars)
      nil
      (push (loop for name in var-names
	       for var in vars
	       collect (cons (or (and (stringp name) (intern name)) name) (deref-exp var)))
	    *select-list*))
  (funcall cont))

(def-global-prolog-functor triple-search/3 (p s o cont)
  (cond ((and (not (has-variable-p (var-deref s))) (not (has-variable-p (var-deref o)))
	      (not (consp s)) (not (consp o))
	      (or (not (var-p s)) (and (var-p s) (bound-p s)))
	      (or (not (var-p o)) (and (var-p o) (bound-p o))))
	 (let ((triple (lookup-triple (var-deref s) p (var-deref o))))
	   (if (triple? triple) 
	       (let ((old-trail (fill-pointer *trail*)))
		 (if (unify! s (node-value (triple-subject triple)))
		     (if (unify! o (node-value (triple-object triple)))
			 (funcall cont)))
		 (undo-bindings! old-trail)))))
	(t
	 (let ((triples 
		(cond ((and (not (has-variable-p s)) (not (consp s))
			    (or (not (var-p s)) (and (var-p s) (bound-p s))))
		       (get-triples :s (var-deref s) :p p))
		      ((and (not (has-variable-p o)) (not (consp o))
			    (or (not (var-p o)) (and (var-p o) (bound-p o))))
		       (get-triples :o (var-deref o) :p p))
		      (t
		       (get-triples :p p)))))
	   (if triples
	       (let ((old-trail (fill-pointer *trail*)))
		 (dolist (triple triples)
		   (if (unify! s (node-value (triple-subject triple)))
		       (if (unify! o (node-value (triple-object triple)))
			   (funcall cont)))
		   (undo-bindings! old-trail))))))))

#|
(defmethod load-all-functors ((graph graph))
  (let ((iter (iter-open (functor-db graph))))
    (iter-first iter)
    (loop
       (multiple-value-bind (key val) (iter-item iter :key-type :octets) 
	 (format t "~A: ~A~%" key val)
	 (if (null key) (return)))
       (iter-next iter))))
|#
