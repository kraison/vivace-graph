;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologc.lisp: Final version of the compiler,
;;;; including all improvements from the chapter.
(in-package #:vivace-graph)

(defconstant +unbound+ :unbound)
(defconstant +no-bindings+ '((t . t)))

(defparameter *occurs-check* t)

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0)
(defvar *uncompiled* nil 
  "Prolog symbols that have not been compiled.")
(defvar *predicate* nil
  "The Prolog predicate currently being compiled")
(defvar *select-list*)

(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding +unbound+))

(defmacro var-deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
             do (setf ,exp (var-binding ,exp)))
          ,exp))

(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (var-deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defmethod predicate ((triple triple))
  (pred-name (triple-predicate triple)))

(defmethod predicate ((tuple list))
  (first tuple))

(defun bound-p (var) (not (eq (var-binding var) +unbound+)))

(defun unify! (x y)
  "Destructively unify two expressions"
  (cond ((equal (var-deref x) (var-deref y)) t)
        ((var-p x) (set-binding! x y))
        ((var-p y) (set-binding! y x))
        ((and (consp x) (consp y))
         (and (unify! (first x) (first y))
              (unify! (rest x) (rest y))))
        (t nil)))

(defun set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defun get-clauses (predicate)
  (let ((p (lookup-predicate predicate *graph*)))
    (when (predicate? p)
      (nconc (get-predicates predicate) (pred-clauses p)))))

(defmethod clause-head ((triple triple))
  (list (pred-name (triple-predicate triple))
	(node-value (triple-subject triple)) 
	(node-value (triple-object triple))))

(defmethod clause-head ((list list))
  (first list))

(defgeneric prolog-compile-help (predicate clauses))
(defmethod prolog-compile-help ((predicate predicate) clauses)
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;(format t "arity of ~A is ~A~%" (clause-head (first clauses)) arity)
      (compile-predicate predicate arity (clauses-with-arity clauses #'= arity))
      (prolog-compile-help predicate (clauses-with-arity clauses #'/= arity)))))

(defmethod prolog-compile ((predicate predicate))
  (if (null (pred-clauses predicate))
      (progn
	(format t "Compiling search-only functor for ~A~%" (pred-name predicate))
	(prolog-compile-search predicate))
      (progn
	(format t "Compiling normal functor for ~A~%" (pred-name predicate))
	(prolog-compile-help predicate (pred-clauses predicate)))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause)
                     (relation-arity (clause-head clause)))
            :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (new-interned-symbol '?arg i)))

(defun make-functor (symbol arity)
  (new-interned-symbol symbol '/ arity))

(defun make-= (x y) `(= ,x ,y))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  ;;`(,predicate ,@args ,cont))
  `(funcall (gethash ',predicate (functors *graph*)) ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist .,body)))

(defun binding-val (binding)
  (cdr binding))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
             (compile-arg (binding-val binding) bindings)
             arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings! old-trail)
                  collect exp)))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (remove '? (set-difference (variables-in exp)
					     parameters))))
    ;(format t "bind-unbound-vars parameters: ~A, exp: ~A~%" parameters exp)
    ;(format t "variables-in ~A: ~A~%" exp (variables-in exp))
    ;(format t "bind-unbound-vars exp-vars: ~A~%" exp-vars)
    (if exp-vars
        `(let ,(mapcar #'(lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
        exp)))

(defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))
 
(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
     (multiple-value-bind (new-seen-once new-seen-more)
         (anon-vars-in (first tree) seen-once seen-more)
       (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
     (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
     (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more))))

(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
     (values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((variable-p x) (compile-unify-variable x y bindings))
    (t              (compile-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings +no-bindings+)
	    nil
	    bindings)))

(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9

(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))

(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x) (cons x x))

(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-unify (first args) (second args) bindings)
          (compile-if
            code1
            (compile-body body cont bindings1))))))

(defmethod clause-body ((triple triple))
  nil)

(defmethod clause-body ((list list))
  (rest list))

(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (let ((body
	 (bind-unbound-vars       
	  parms                  
	  (compile-body
	   (nconc
	    (mapcar #'make-= parms (args (clause-head clause)))
	    (clause-body clause))
	   cont
	   (mapcar #'self-cons parms)))))
    ;(format t "BODY: ~A~%" body)
    body))

(defun add-clause (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  (let ((predicate-name (predicate (clause-head clause))))
    (format t "1. Adding clause ~A: ~A~%" predicate-name clause)
    (assert (and (atom predicate-name) (not (variable-p predicate-name))))
    (when (stringp predicate-name) (setq predicate-name (intern predicate-name)))
    (if (and (= 1 (length clause))
	     (= 3 (length (first clause)))
	     (not (variable-p (second (first clause))))
	     (not (variable-p (third (first clause)))))
	(prog1
	    (add-triple (second (first clause)) predicate-name (third (first clause)) *graph*)
	  (unless (gethash (make-functor predicate-name 2) (functors *graph*))
	    (add-default-rule (make-new-predicate :name predicate-name))))
	(let ((predicate (make-new-predicate :name predicate-name)))
	  (add-rule predicate clause)
	  predicate))))

(defun prolog-ignore (&rest args)
  (declare (ignore args))
  nil)

(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	 (*var-counter* 0)
	 (top-level-query (gensym "PROVE"))
	 (*predicate* (make-functor top-level-query 0)))
    (unwind-protect
	 (let* ((vars (delete '? (variables-in goals)))
		(*predicate* (make-functor top-level-query 0))
		(parameters (make-parameters 0))
		(goals (mapcar #'(lambda (g)
				   (when (stringp (first g)) 
				     ;(format t "Making ~A a symbol~%" (first g))
				     (setf (nth 0 g) (intern (nth 0 g))))
				   g)
			       goals)))
	   (catch 'top-level-prove
	     (let ((func
		    `#'(lambda (,@parameters cont)
			 (block ,*predicate*
			   .,(maybe-add-undo-bindings
			      (mapcar #'(lambda (clause)
					  (compile-clause parameters clause 'cont))
				      `(((,top-level-query)
					 ,@goals
					 (show-prolog-vars ,(mapcar #'symbol-name vars)
							   ,vars)))))))))
	       ;;(format t "----TOP LEVEL----~%~A~%----END TOP LEVEL----~%" func)
	       (setf (gethash *predicate* (functors *graph*)) (eval func)))
	     (funcall (gethash *predicate* (functors *graph*)) #'prolog-ignore))
	   (format t "~&No.~%"))
      (remhash *predicate* (functors *graph*)))
    (values)))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (var-deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

(defun compile-triple-search (s p o)
  `(cond ((and (not (has-variable-p (var-deref ,s))) (not (has-variable-p (var-deref ,o)))
	       (or (not (var-p ,s)) (and (var-p ,s) (bound-p ,s)))
	       (or (not (var-p ,o)) (and (var-p ,o) (bound-p ,o))))
	  (let ((triple (lookup-triple (var-deref ,s) ,p (var-deref ,o))))
	    (if (triple? triple) 
		(let ((old-trail (fill-pointer *trail*)))
		  (if (unify! ,s (node-value (triple-subject triple)))
		      (if (unify! ,o (node-value (triple-object triple)))
			  (funcall cont)))
		  (undo-bindings! old-trail)))))
	 (t
	  (let ((triples 
		 (cond ((and (not (has-variable-p ,s)) 
			     (or (not (var-p ,s)) (and (var-p ,s) (bound-p ,s))))
			(get-triples :s (var-deref ,s) :p ,p))
		       ((and (not (has-variable-p ,o))
			     (or (not (var-p ,o)) (and (var-p ,o) (bound-p ,o))))
			(get-triples :o (var-deref ,o) :p ,p))
		       (t
			(get-triples :p ,p)))))
	    (if triples
		(let ((old-trail (fill-pointer *trail*)))
		  (dolist (triple triples)
		    (if (unify! ,s (node-value (triple-subject triple)))
			(if (unify! ,o (node-value (triple-object triple)))
			    (funcall cont)))
		    (undo-bindings! old-trail))))))))

(defmethod prolog-compile-search ((predicate predicate))
  (let ((*predicate* (make-functor (pred-name predicate) 2))
	(parameters (make-parameters 2)))
    (let ((func
	   `#'(lambda (,@parameters cont)
		(block ,*predicate*
		  ,(compile-triple-search (first parameters) 
					  `',(pred-name predicate)
					  (second parameters))))))
      (format t "prolog-compile-search:~%~A~%~%" func)
      (setf (gethash *predicate* (functors (pred-graph predicate)))
	    (eval func)))))
  
(defun compile-predicate (predicate arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-functor (pred-name predicate) arity))
	(parameters (make-parameters arity)))
    (let ((func
	   `#'(lambda (,@parameters cont)
		(block ,*predicate*
		  ,(if (= arity 2)
		       (compile-triple-search (first parameters) 
					      `',(pred-name predicate)
					      (second parameters)))
		  .,(maybe-add-undo-bindings
		     (mapcar #'(lambda (clause)
				 (compile-clause parameters clause 'cont))
			     clauses))))))
      (format t "compile-predicate: ~%~A~%~%" func)
      (setf (gethash *predicate* (functors (pred-graph predicate)))
	    (eval func)))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;*** 
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro 
                             (funcall macro goal (rest body) 
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            (compile-call (make-functor (predicate goal) (relation-arity goal))
			  (mapcar #'(lambda (arg)
				      (compile-arg arg bindings))
				  (args goal))
			   (if (null (rest body))
			       cont
			       `#'(lambda ()
				    ,(compile-body 
				      (rest body) cont
				      (bind-new-variables bindings goal))))))))))

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (intern (symbol-name (gensym "?"))))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defmacro ?- (&rest goals)
  `(top-level-prove ',(replace-?-vars goals)))

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(let ((*predicate* nil)) (add-clause ',(make-anonymous clause))))

(defun top-level-select (vars goals)
  "Prove the list of goals by compiling and calling it."
  (let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	 (*var-counter* 0)
	 (*select-list* nil)
	 (top-level-query (gensym "PROVE"))
	 (*predicate* (make-functor top-level-query 0)))
    (unwind-protect
	 (let* (;(vars (delete '? (variables-in goals)))
		(*predicate* (make-functor top-level-query 0))
		(parameters (make-parameters 0))
		(goals (mapcar #'(lambda (g)
				   (when (stringp (first g)) 
				     (setf (nth 0 g) (intern (nth 0 g))))
				   g)
			       goals)))
	   (catch 'top-level-prove
	     (let ((func
		    `#'(lambda (,@parameters cont)
			 (block ,*predicate*
			   .,(maybe-add-undo-bindings
			      (mapcar #'(lambda (clause)
					  (compile-clause parameters clause 'cont))
				      `(((,top-level-query)
					 ,@goals
					 (select ,(mapcar #'symbol-name vars) ,vars)))))))))
	       ;;(format t "----TOP LEVEL----~%~A~%----END TOP LEVEL----~%" func)
	       (setf (gethash *predicate* (functors *graph*)) (eval func)))
	     (funcall (gethash *predicate* (functors *graph*)) #'prolog-ignore)))
      (remhash *predicate* (functors *graph*)))
    (reverse *select-list*)))

(defmacro select (vars &rest goals)
  `(top-level-select ',vars ',(replace-?-vars goals)))

(defun load-prolog-default-functors (graph)
  (setf (gethash 'show-prolog-vars/2 (functors graph))
	#'(lambda (var-names vars cont)
	    (block show-prolog-vars/2
	      (format t "show-prolog-vars/2: ~A ; ~A ; ~A~%" var-names vars cont)
	      (if (null vars)
		  (format t "~&Yes")
		  (loop for name in var-names
		     for var in vars do
		       (format t "~&~a = ~a" name (deref-exp var))))
	      (if (continue-p)
		  (funcall cont)
		  (throw 'top-level-prove nil)))))
  (setf (gethash 'select/2 (functors graph))
	#'(lambda (var-names vars cont)
	    (block select/2
	      (format t "select/2: ~A ; ~A ; ~A~%" var-names vars cont)
	      (if (null vars)
		  nil
		  (push 
		   (loop for name in var-names
		      for var in vars
		      collect (deref-exp var))
		   *select-list*))
	      (funcall cont)))))


(defun prolog-test ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (progn
	   (<- (member ?item (?item . ?rest)))
	   (<- (member ?item (?x . ?rest)) (member ?item ?rest))
	   (format t "~%(?- (member 1 (4 3 2 1))) ->~%")
	   (?- (member 1 (4 3 2 1)))
	   (format t "~%(member ?x (1 2 3 4)) ->~%")
	   (?- (member ?x (1 2 3 4)))
	   (format t "~%Adding triples:~%")
	   (<- ("loves" "Kevin" "Dustie"))
	   (<- ("loves" "Dustie" "Kevin"))
	   (<- ("loves" "Kevin" "Echo"))
	   (<- ("loves" "Echo" "cat nip"))
	   (format t "(\"loves\" ?x ?y)) -> ~%")
	   (?- ("loves" ?x ?y))
	   (format t "(\"loves\" \"Kevin\" \"Dustie\")) -> ~%")
	   (?- ("loves" "Kevin" "Dustie"))
	   (format t "(\"loves\" \"Kevin\" ?y)) -> ~%")
	   (?- ("loves" "Kevin" ?y))
	   (<- ("likes" "Robin" "cats"))
	   (<- ("likes" "Sandy" ?x) ("likes" ?x "cats"))
	   (format t "(?- (\"likes\" \"Sandy\" ?who?)) ->~%")
	   (?- ("likes" "Sandy" ?who)))
      (progn
	(shutdown-graph *graph*)
	(delete-file "/var/tmp/triples")
	(delete-file "/var/tmp/rules")
	(delete-file "/var/tmp/config.ini")))))
