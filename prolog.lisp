(in-package #:vivace-graph)

;;; This is a first pass at a direct adaptation of Peter Norvig's Prolog interpreter, from
;;; Paradigms of AI Programming, to my data structures.  Thanks, Mr. Norvig!

(defconstant +fail+ nil)
(defconstant +no-bindings+ '((t . t)))
(defconstant +unbound+ :unbound)
(defparameter *occurs-check* t)
(defvar *trail*)
(defvar *var-counter* 0)

(defmacro deref (exp)
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
	     do (setf ,exp (var-binding ,exp)))
	  ,exp))

(defstruct (var
	     (:constructor ? ())
	     (:print-function (lambda (var stream depth)
				(if (or (and (numberp *print-level*)
					     (>= depth *print-level*))
					(var-p (deref var)))
				    (format stream "?~A" (var-name var))
				    (write var :stream stream)))))
  (name (incf *var-counter*)) 
  (binding +unbound+))

(defun bound-p (var)
  (not (eq (var-binding var) +unbound+)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings +no-bindings+)
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t +fail+))))

(defun set-binding! (var value)
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings! (old-trail)
  (loop until (= (fill-pointer *trail*) old-trail)
       do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defun unify! (x y)
  (cond ((equal (deref x) (deref y)) t)
	((var-p x) (set-binding! x y))
	((var-p y) (set-binding! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defun unify (x y &optional (bindings +no-bindings+))
  (cond ((eq bindings +fail+) +fail+)
	((equal x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t +fail+)))

(defun unify-variable (var x bindings)
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings))
	 +fail+)
	(t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun subst-bindings (bindings x)
  (cond ((eq bindings +fail+) +fail+)
	((eq bindings +no-bindings+) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))

(defun unifier (x y)
  (subst-bindings (unify x y) x))

(defmethod clause-head ((triple triple))
  (list (pred-name (triple-predicate triple))
	(node-value (triple-subject triple)) 
	(node-value (triple-object triple))))

(defmethod clause-head ((tuple list))
  tuple)

(defmethod clause-body ((triple triple))
  nil)

(defmethod clause-body ((tuple list))
  tuple)

(defun get-clauses (predicate)
  (get-predicates predicate))

(defmethod predicate ((triple triple))
  (pred-name (triple-predicate triple)))

(defmethod predicate ((tuple list))
  (first tuple))

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defun add-clause (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  (let ((predicate-name (first clause)))
    (assert (and (atom predicate-name) (not (variable-p predicate-name))))
    (when (stringp predicate-name) (setq predicate-name (intern predicate-name)))
    (if (and (= 3 (length clause))
	     (atom (second clause))
	     (atom (third clause))
	     (not (variable-p (second clause)))
	     (not (variable-p (third clause))))
	(add-triple (second clause) predicate-name (third clause) *graph*)
	(let ((predicate (make-new-predicate :name predicate-name)))
	  (add-rule predicate clause)
	  predicate-name))))

(defmacro fact (&rest clause)
  `(add-triple ,(second clause) ,(first clause) ,(third clause)))

(defmacro rule (&rest clauses)
  `(add-rule ',(replace-?-vars clauses) ,*graph*))

(defmacro <- (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  `(add-clause ',(replace-?-vars clause)))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun rename-variables (x)
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))

(defun prove (goal bindings)
  (when (stringp (first goal)) (setf (nth 0 goal) (intern (nth 0 goal))))
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause) bindings))))
	  (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  (cond ((eq bindings +fail+) +fail+)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal1-solution)
		       (prove-all (rest goals) goal1-solution))
		   (prove (first goals) bindings)))))

(defun show-prolog-vars (vars bindings)
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~A = ~A" var (subst-bindings bindings var))))
  (princ ";"))

(defun show-prolog-solutions (vars solutions)
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution)) solutions))
  (values))

(defun top-level-prove (goals)
  (show-prolog-solutions (variables-in goals) (prove-all goals +no-bindings+)))

(defmacro ?- (&rest goals)
  `(let ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	 (*var-counter* 0))
     (top-level-prove ',(replace-?-vars goals))))
