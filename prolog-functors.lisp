(in-package #:vivace-graph)

(defvar *prolog-global-functors* nil)

(defmacro def-global-prolog-functor (name lambda-list &body body)
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (pushnew ',name *prolog-global-functors*)))

(defun load-prolog-default-functors (graph)
  (dolist (functor *prolog-global-functors*)
    (setf (gethash functor (functors graph)) functor)))

(def-global-prolog-functor read/1 (exp cont)
  (if (unify! exp (read)) (funcall cont)))

(def-global-prolog-functor write/1 (exp cont)
  (write (deref-exp exp) :pretty t) (funcall cont))

(def-global-prolog-functor nl/0 (cont) 
  (terpri) (funcall cont))

(def-global-prolog-functor =/2 (?arg1 ?arg2 cont)
  (if (unify! ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)	       
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)	      
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (> ?arg1 ?arg2)) 
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2)) (< ?arg1 ?arg2)) 
      (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  (if (and (consp (var-deref exp))
	   (unify! ?result (apply (first exp) (rest exp))))
      (funcall cont)))

(def-global-prolog-functor not/1 (relation cont)	      
  (with-undo-bindings
    (funcall (gethash 'call/1 (functors *graph*)) 
	     relation 
	     #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(def-global-prolog-functor call/1 (goal cont)
  (var-deref goal)
  (apply (gethash (make-functor (first goal) (length (args goal))) (functors *graph*))
	 (append (args goal) (list cont))))

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
  (funcall cont))))
