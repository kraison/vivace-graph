(in-package #:vivace-graph)

(defvar *prolog-global-functors* (make-hash-table :synchronized t))

(defmacro def-global-prolog-functor (name lambda-list &body body)
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (setf (gethash ',name *prolog-global-functors*) #',name)))

(defun default-functor? (symbol)
  (gethash symbol *prolog-global-functors*))

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
  "Unifies two prolog variables."
  (if (unify! ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)	       
  "Checks equality of the values of two prolog variables."
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor /=/2 (?arg1 ?arg2 cont)
  "Checks inequality of the values of two prolog variables."
  (if (not (deref-equal ?arg1 ?arg2)) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)
  "Prolog greater than functor."
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (> ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp> ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  "Prolog less than functor."
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (< ?arg1 ?arg2))
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp< ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor >=/2 (?arg1 ?arg2 cont)	      
  "Prolog greater than or equal to functor."
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (>= ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp>= ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor <=/2 (?arg1 ?arg2 cont)
  "Prolog less than or equal to functor."
  (if (or (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (<= ?arg1 ?arg2)) 
	  (and (timestamp? (var-deref ?arg1)) (timestamp? (var-deref ?arg2))
	       (timestamp<= ?arg1 ?arg2)))
      (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  "Call out to lisp from with a Prolog query.  Assigns result to the supplied Prolog var.
 (lisp ?result (+ 1 2)).  Any lisp variables that you wish to access within a prolog query using
the lisp functor should be declared special."
  (when *prolog-trace* (format t "TRACE: lisp/2 ?result <- ~A~%" exp))
  (let ((exp (var-deref exp)))
    (cond ((consp exp)
	   (if (unify! ?result (eval exp))
	       (funcall cont)))
	  ((and (symbolp exp) (boundp exp))
	   (if (unify! ?result (eval exp))
	       (funcall cont)))
	  (t
	   (if (unify! ?result exp)
	       (funcall cont))))))

(def-global-prolog-functor regex-match/2 (?arg1 ?arg2 cont)
  "Functor that treats first arg as a regex and uses cl-ppcre:scan to check for the pattern in the
second arg."
  (if (and (stringp (var-deref ?arg1)) (stringp (var-deref ?arg2))
	   (cl-ppcre:scan ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor var/1 (?arg1 cont)
  (if (unbound-var-p ?arg1) (funcall cont)))

(def-global-prolog-functor is/2 (var exp cont)
  "Similar to lisp/2, but unifies instead of assigns the lisp return value."
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
	   (unify! var (eval (deref-exp exp))))
      (funcall cont)))

(def-global-prolog-functor call/1 (goal cont)
  "Call a prolog form."
  (var-deref goal)
  (let* ((functor (make-functor (first goal) (length (args goal)))))
    (let ((fn (or (gethash functor (functors *graph*)) 
		  (gethash functor *prolog-global-functors*))))
      (if (functionp fn)
	  (apply fn (append (args goal) (list cont)))
	  (if (= 2 (relation-arity goal))
	      (triple-search/3 (first goal) (second goal) (third goal) cont)
	      (error 'prolog-error 
		     :reason 
		     (format nil "Unknown Prolog functor in call/1 ~A" functor)))))))

(def-global-prolog-functor if/2 (?test ?then cont)
  (when *prolog-trace* (format t "TRACE: IF/2(~A ~A)~%" ?test ?then))
  (call/1 ?test #'(lambda () (call/1 ?then cont))))

(def-global-prolog-functor if/3 (?test ?then ?else cont)
  (when *prolog-trace* (format t "TRACE: IF/3(~A ~A ~A)~%" ?test ?then ?else))
  (call/1 ?test #'(lambda () (call/1 ?then #'(lambda () (funcall cont) (return-from if/3)))))
  (call/1 ?else cont))

(def-global-prolog-functor is-valid/1 (item cont)
  "Mark a triple as VALID and remove and INVALID marker."
  (var-deref item)
  (with-transaction ((triple-db *graph*))
    (let ((triple (lookup-triple item 'has-property "invalid")))
      (when (triple? triple)
	(delete-triple triple)))
    (and (add-triple item 'has-property "valid")
	 (funcall cont))))

(def-global-prolog-functor is-valid?/1 (item cont)
  "Ask if a triple is valid."
  (var-deref item)
  (let ((triple (lookup-triple item 'has-property "valid")))
    (when (triple? triple)
      (funcall cont))))

(def-global-prolog-functor is-invalid/1 (item cont)
  "Mark a triple as INVALID and remove and VALID marker."
  (var-deref item)
  (with-transaction ((triple-db *graph*))
    (let ((triple (lookup-triple item 'has-property "valid")))
      (when (triple? triple)
	(delete-triple triple)))
    (and (add-triple item 'has-property "invalid")
	 (funcall cont))))

(def-global-prolog-functor is-invalid?/1 (item cont)
  "Ask if a triple is invalid."
  (var-deref item)
  (let ((triple (lookup-triple item 'has-property "invalid")))
    (when (triple? triple)
      (funcall cont))))

(def-global-prolog-functor valid-date?/1 (date cont)
  "Date validation functor. FIXME: This needs to be fleshed out with a more comprehensive regex."
  (var-deref date)
  (if (timestamp? date) 
      (funcall cont)
      (if (and (stringp date)
	       (cl-ppcre:scan 
		"^(19|20)\\d\\d[\-\ \/\.](0[1-9]|1[012])[\-\ \/\.](0[1-9]|[12][0-9]|3[01])$" date))
	  (funcall cont))))

(def-global-prolog-functor trigger/1 (exp cont)
  "Call out to lisp ignoring the return value."
  (eval (deref-exp exp))
  (funcall cont))

(def-global-prolog-functor assert/1 (clause cont)
  "Add a clause to the datastore."
  (when (consp clause)
    (setq clause (mapcar #'(lambda (c) (var-deref c)) clause))
    (when *prolog-trace* (format t "TRACE: Asserting ~A~%" clause))
    (if (and (= 3 (length clause)) (not (some #'var-p clause)))
	(let ((triple (add-triple (second clause) (first clause) (third clause))))
	  (when *prolog-trace* (format t "TRACE: Asserted new triple ~A~%" triple))
	  (when (triple? triple)
	    (funcall cont)))
	(call/1 clause cont))))

(def-global-prolog-functor retract/1 (clause cont)
  "Retract a fact from the datastore."
  (when (consp clause)
    (setq clause (mapcar #'(lambda (c) (var-deref c)) clause))
    (when (= (length clause) 3)
      (handler-case
	  (with-transaction ((triple-db *graph*))
	    (when *prolog-trace* (format t "TRACE: Retracting fact ~A~%" clause))
	    (let ((triple (lookup-triple (second clause) (first clause) (third clause))))
	      (if (triple? triple)
		  (delete-triple triple)
		  (error 'prolog-error 
			 :reason (format nil "clause ~A does not represent a fact" clause)))))
	(prolog-error (condition)
	  (format t "Cannot retract ~A: ~A~%" clause condition))
	(:no-error (result)
	  (declare (ignore result))
	  (funcall cont))))))

(def-global-prolog-functor not/1 (relation cont)
  "Prolog negation.  Does not retract, simply negates in the context of the query."
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
  (when *prolog-trace* (format t "TRACE: triple-search/3(~A ~A ~A)~%" p s o))
  (when (and (not (consp p)) (not (consp s)) (not (consp o)))
    (let ((triples (get-triples 
		    :p (and (or (not (var-p p)) (and (var-p p) (bound-p p))) (var-deref p))
		    :s (and (or (not (var-p s)) (and (var-p s) (bound-p s))) (var-deref s))
		    :o (and (or (not (var-p o)) (and (var-p o) (bound-p o))) (var-deref o))
		    :decode? nil)))
      (cond ((klist? triples)
	     (unwind-protect
		  (map-klist #'(lambda (id)
				 (let ((triple (lookup-triple-by-id id)))
				   (let ((old-trail (fill-pointer *trail*)))
				     (when (and (triple? triple) (not (triple-deleted? triple)))
				       (when (unify! p (pred-name (triple-predicate triple)))
;					 (when (unify! s (node-value (triple-subject triple)))
;					   (when (unify! o (node-value (triple-object triple)))
					 (when (unify! s (triple-subject triple))
					   (when (unify! o (triple-object triple))
					     (funcall cont))))
				       (undo-bindings! old-trail)))))
			     triples)
	       (klist-free triples)))
	    ((triple? triples)
	     (let ((old-trail (fill-pointer *trail*)))
	       (when (not (triple-deleted? triples))
		 (when (unify! p (pred-name (triple-predicate triples)))
;		   (when (unify! s (node-value (triple-subject triples)))
;		     (when (unify! o (node-value (triple-object triples)))
		   (when (unify! s (triple-subject triples))
		     (when (unify! o (triple-object triples))
		       (funcall cont))))
		 (undo-bindings! old-trail))))))))


(defmethod load-all-functors ((graph graph))
  (map-phash #'(lambda (key val)
		 (let ((pieces (split key '(#\Nul))))
		   (when (equal (second pieces) "name")
		     (lookup-predicate val))))
	     (functor-db graph)))
