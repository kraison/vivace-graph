;;;; This is Kevin Raison's customization of Mr. Norvig's PAIP Prolog.  Thanks Mr. Norvig!
;;;; Copyright (c) 1991 Peter Norvig
(in-package #:vivace-graph)

(defparameter *prolog-trace* nil)

(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding +unbound+))

(defmacro var-deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
             do (setf ,exp (var-binding ,exp)))
	  ,exp))
          ;;(if (node? ,exp) (setf ,exp (node-value ,exp)) ,exp)))

(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (var-p (var-deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defun bound-p (var) (not (eq (var-binding var) +unbound+)))

(defgeneric prolog-equal (x y)
  (:documentation "Generic equality operator for prolog unification. Specialize this for new 
types that will be stored in the db.")
  (:method ((x number) (y number)) (= x y))
  (:method ((x string) (y string)) (string= x y))
  (:method ((x character) (y character)) (char= x y))
  (:method ((x timestamp) (y timestamp)) (timestamp= x y))
  (:method ((x timestamp) (y integer)) (= (timestamp-to-universal x) y))
  (:method ((x integer) (y timestamp)) (= (timestamp-to-universal y) x))
  ;;(:method ((x node) (y node)) (node-equal x y))
  ;;(:method ((x node) y) (prolog-equal (node-value x) y))
  ;;(:method (x (y node)) (prolog-equal x (node-value y)))
  (:method ((x triple) (y triple)) (triple-equal x y))
  (:method ((x uuid:uuid) (y uuid:uuid)) (uuid:uuid-eql x y))
  (:method (x y) (equal x y)))

(defun eval-?? (exp)
  (if (and (consp exp) (eq '?? (first exp)))
      (eval (deref-exp (second exp)))
      exp))

(defun unify! (x y)
  "Destructively unify two expressions."
;  (if (and (consp x) (eq '?? (first x)))
;      (setq x (eval (deref-exp (second x)))))
;  (if (and (consp y) (eq '?? (first y)))
;      (setq y (eval (deref-exp (second y)))))
  (cond ((prolog-equal (var-deref x) (var-deref y)) t)
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
  (let ((p (lookup-predicate predicate)))
    (when (predicate? p)
      (nconc (get-predicates predicate) (pred-clauses p)))))

(defmethod clause-head ((triple triple))
  (list (pred-name (triple-predicate triple))
	(triple-subject triple) (triple-object triple)))
	;;(node-value (triple-subject triple)) 
	;;(node-value (triple-object triple))))

(defmethod clause-head ((list list))
  (first list))

(defgeneric prolog-compile-help (predicate clauses))
(defmethod prolog-compile-help ((predicate predicate) clauses)
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      (compile-predicate predicate arity (clauses-with-arity clauses #'= arity))
      (prolog-compile-help predicate (clauses-with-arity clauses #'/= arity)))))

(defmethod prolog-compile ((predicate predicate))
  (if (null (pred-clauses predicate))
      (prolog-compile-search predicate)
      (prolog-compile-help predicate (pred-clauses predicate))))

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

(defun compile-call (predicate arity args cont)
  "Compile a call to a prolog predicate."
  (if (and (variable-p predicate) (= 2 arity))
      `(progn
	 (when *prolog-trace*
	   (format t "TRACE: (SEARCH) ~A/~A~A~%" ',predicate ',arity ',args))
	 (triple-search/3 ,predicate ,@args ,cont))
      (let ((functor (make-functor predicate arity)))
	`(let ((func (or (gethash ',functor (functors *graph*)) 
			 (gethash ',functor *prolog-global-functors*))))
	   (when *prolog-trace*
	     (format t "TRACE: ~A/~A~A~%" ',predicate ',arity ',args))
	   (if (functionp func)
	       (funcall func ,@args ,cont))))))

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
  ;;(and (symbolp x) (not (eq x '??)) (equal (char (symbol-name x) 0) #\?)))
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

(defmacro with-undo-bindings (&body body)
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
	 ,(first body)
	 ,@(loop for exp in (rest body)
	      collect '(undo-bindings! old-trail)
	      collect exp))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun unbound-var-p (exp)
  (and (var-p exp) (not (bound-p exp))))

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

(defun make-anonymous (exp &optional (anon-vars (anonymous-variables-in exp)))
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
     (values (prolog-equal x y) bindings))
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
      ((not (and (prolog-equal x x1) (prolog-equal y y1)))              ; deref
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

(def-prolog-compiler-macro true (goal body cont bindings)
  (declare (ignore goal))
  (compile-body body cont bindings))

(def-prolog-compiler-macro fail (goal body cont bindings)
  (declare (ignore goal body cont bindings))
  nil)

(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))

(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      (0 +fail+)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (gensym "F")))
	   `(flet ((,fn () ,(compile-body body cont bindings)))
	      .,(maybe-add-undo-bindings
		 (loop for g in disjuncts collect
		      (compile-body (list g) `#',fn bindings)))))))))

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
    ;;(format t "~A BODY:~% ~A~%" (clause-head clause) body)
    body))

(defun add-clause (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  (let* ((predicate-name (predicate (clause-head clause))))
    ;;(format t "1. Adding clause ~A: ~A~%" predicate-name clause)
    (assert (and (atom predicate-name) (not (variable-p predicate-name))))
    (when (stringp predicate-name) 
      (setq predicate-name (intern (string-upcase predicate-name))))
    (let* ((arity (relation-arity (clause-head clause)))
	   (functor (make-functor predicate-name arity)))
      (if (gethash functor *prolog-global-functors*)
	  (error 'prolog-error :reason (format nil "Cannot override default functor ~A." functor))
	  (if (and (= 1 (length clause))
		   (= 3 (length (first clause)))
		   (not (variable-p (second (first clause))))
		   (not (variable-p (third (first clause)))))
	      (add-triple (second (first clause)) predicate-name (third (first clause)) *graph*)
	      (let ((p (lookup-predicate predicate-name)))
		(if (predicate? p)
		    (add-functor p clause)
		    (make-new-predicate :name predicate-name :clauses (list clause)))))))))

(defun deref-copy (exp)
  (sublis (mapcar #'(lambda (var) (cons (var-deref var) (?)))
		  (unique-find-anywhere-if #'var-p exp))
	  exp))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (var-deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

(defun deref-equal (x y)
  (or (prolog-equal (var-deref x) (var-deref y))
      (and (consp x)
	   (consp y)
	   (deref-equal (first x) (first y))
	   (deref-equal (rest x) (rest y)))))

(defmethod prolog-compile-search ((predicate predicate))
  (let ((*predicate* (make-functor (pred-name predicate) 2)))
    (setf (gethash *predicate* (functors (pred-graph predicate))) 
	  (eval `#'(lambda (?arg1 ?arg2 cont)
		     (block ,*predicate*
		       (triple-search/3 ',(pred-name predicate) ?arg1 ?arg2 cont)))))))

(defun compile-predicate (predicate arity clauses)
  "Compile all the clauses for a given symbol/arity into a single LISP function."
  (let ((*predicate* (make-functor (pred-name predicate) arity))
	(parameters (make-parameters arity)))
    (let ((func
	   `#'(lambda (,@parameters cont)
		(block ,*predicate*
		  ,(if (and (= arity 2) (not (default-functor? *predicate*)))
		       `(triple-search/3 
			 ',(pred-name predicate) ,(first parameters) ,(second parameters) cont))
		  .,(maybe-add-undo-bindings
		     (mapcar #'(lambda (clause)
				 (compile-clause parameters clause 'cont))
			     clauses))))))
      (setf (gethash *predicate* (functors (pred-graph predicate))) (eval func)))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)
     `(progn ,(compile-body (rest body) cont bindings)
             (return-from ,*predicate* nil)))
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro (funcall macro goal (rest body) cont bindings))))
	 (if (and macro (not (eq macro-val :pass)))
	     macro-val
	     (compile-call (predicate goal) (relation-arity goal)
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

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(let ((*predicate* nil)) (add-clause ',(make-anonymous clause))))

(defmacro insert (&rest clauses)
  "Add clauses to the data base. Wraps all additions in a single transaction."
  `(let ((count 0))
     (with-transaction ((triple-db *graph*))
       (dolist (clause ',(make-anonymous clauses))
	 (let ((*predicate* nil)) (add-clause (list clause)))
	 (incf count)))
     count))

(defun prolog-ignore (&rest args)
  (declare (ignore args))
  nil)

(defmacro ?- (&rest goals)
  "Execute an interactive prolog query."
  (let* ((goals (replace-?-vars goals))
	 (vars (delete '? (variables-in goals)))
	 (top-level-query (gensym "PROVE"))
	 (*predicate* (make-functor top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*predicate* ',*predicate*)
	    (*select-list* nil))
       (unwind-protect
	    (catch 'top-level-prove
	      (let ((func #'(lambda (cont) 
			      (handler-case
				  (block ,*predicate*
				    .,(maybe-add-undo-bindings
				       (mapcar #'(lambda (clause)
						   (compile-clause nil clause 'cont))
					       `(((,top-level-query)
						  ,@goals
						  (show-prolog-vars ,(mapcar #'symbol-name vars)
								    ,vars))))))
				(undefined-function (condition)
				  (error 'prolog-error :reason condition))))))
		(setf (gethash ',*predicate* (functors *graph*)) func)
		(funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore)
		(format t "~&No.~%")))
	 (remhash ',*predicate* (functors *graph*)))
       (values))))

(defmacro select (vars &rest goals)
  "Select specific variables as a list of lists using the following form:
 (select (?x ?y) (is-a ?x ?y)) could return ((Joe Human) (Spot Dog))"
  (let* ((top-level-query (gensym "PROVE"))
	 (goals (replace-?-vars goals))
	 (*predicate* (make-functor top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*predicate* ',*predicate*)
	    (*select-list* nil))
       (unwind-protect
	    (let ((func #'(lambda (cont) 
			    (handler-case
				(block ,*predicate*
				  .,(maybe-add-undo-bindings
				     (mapcar #'(lambda (clause)
						 (compile-clause nil clause 'cont))
					     `(((,top-level-query)
						,@goals
						(select ,(mapcar #'symbol-name vars) ,vars))))))
			      (undefined-function (condition)
				(error 'prolog-error :reason condition))))))
	      (setf (gethash ',*predicate* (functors *graph*)) func)
	      (funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore))
	 (remhash ',*predicate* (functors *graph*)))
       (nreverse *select-list*))))

(defmacro select-bind-list (vars &rest goals)
  "Select specific variables as a list of assoc lists using the following form:
 (select-bind-list (?x ?y) (is-a ?x ?y)) could return ((?x . Joe) (?y . Human))"
  (let* ((top-level-query (gensym "PROVE"))
	 (goals (replace-?-vars goals))
	 (*predicate* (make-functor top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*predicate* ',*predicate*)
	    (*select-list* nil))
       (unwind-protect
	    (let ((func #'(lambda (cont) 
			    (handler-case
				(block ,*predicate*
				  .,(maybe-add-undo-bindings
				     (mapcar #'(lambda (clause)
						 (compile-clause nil clause 'cont))
					     `(((,top-level-query)
						,@goals
						(select-as-bind-alist
						 ,(mapcar #'symbol-name vars) ,vars))))))
			      (undefined-function (condition)
				(error 'prolog-error :reason condition))))))
	      (setf (gethash ',*predicate* (functors *graph*)) func)
	      (funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore))
	 (remhash ',*predicate* (functors *graph*)))
       (nreverse *select-list*))))

(defmacro select-flat (vars &rest goals)
  "Select specific variables as a flattened list of values using the following form:
 (select-flat (?x) (is-a ?x Human)) could return (Joe Henry Jill)"
  (let* ((goals (replace-?-vars goals))
	 (top-level-query (gensym "PROVE"))
	 (*predicate* (make-functor top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*predicate* ',*predicate*)
	    (*select-list* nil))
       (unwind-protect
	    (let ((func #'(lambda (cont) 
			    (handler-case
				(block ,*predicate*
				  .,(maybe-add-undo-bindings
				     (mapcar #'(lambda (clause)
						 (compile-clause nil clause 'cont))
					     `(((,top-level-query)
						,@goals
						(select ,(mapcar #'symbol-name vars) ,vars))))))
			      (undefined-function (condition)
				(error 'prolog-error :reason condition))))))
	      (setf (gethash ',*predicate* (functors *graph*)) func)
	      (funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore))
	 (remhash ',*predicate* (functors *graph*)))
       (flatten (nreverse *select-list*)))))

(defmacro do-query (&rest goals)
  "Execute a prolog query, ignoring the results."
  (let* ((top-level-query (gensym "PROVE"))
	 (goals (replace-?-vars goals))
	 (*predicate* (make-functor top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	    (*var-counter* 0)
	    (*predicate* ',*predicate*))
       (unwind-protect
	    (let ((func #'(lambda (cont) 
			    (handler-case
				(block ,*predicate*
				  (when *prolog-trace*
				    (format t "TRACE: do-query for ~A~%" ',goals))
				  .,(maybe-add-undo-bindings
				     (mapcar #'(lambda (clause)
						 (compile-clause nil clause 'cont))
					     `(((,top-level-query)
						,@goals)))))
			      (undefined-function (condition)
				(error 'prolog-error :reason condition))))))
	      (setf (gethash ',*predicate* (functors *graph*)) func)
	      (funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore))
	 (remhash ',*predicate* (functors *graph*)))
       t)))

(defun exec-rule (goals action)
  (let* ((rule (append (replace-?-vars goals) (list '! action)))
	 (top-level-query (gensym "PROVE"))
	 (*predicate* (make-functor top-level-query 0)))
    (eval
     `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
	     (*var-counter* 0)
	     (*predicate* ',*predicate*))
	(unwind-protect
	     (let ((func #'(lambda (cont) 
			     (block ,*predicate*
			       .,(maybe-add-undo-bindings
				  (mapcar #'(lambda (clause)
					      (compile-clause nil clause 'cont))
					  `(((,top-level-query)
					     ,@rule))))))))
	       (setf (gethash ',*predicate* (functors *graph*)) func)
	       (funcall (gethash ',*predicate* (functors *graph*)) #'prolog-ignore))
	  (remhash ',*predicate* (functors *graph*)))
	t))))

(defun valid-prolog-query? (form)
  (case (first form)
    (select t)
    (select-flat t)
    (select-bind-list t)
    (<- t)
    (insert t)
    (do-query t)
    (otherwise nil)))


