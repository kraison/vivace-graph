;;;; This is Kevin Raison's customization of Mr. Norvig's PAIP Prolog.
;;;; Thanks Mr. Norvig!
;;;; Copyright (c) 1991 Peter Norvig, (c) 2010 Kevin Raison
(in-package #:graph-db)

(defstruct (prolog-gensym
             (:conc-name pg-))
  (counter 0 :type (unsigned-byte 64))
  #+ccl (lock (ccl:make-lock))
  #+ecl (lock (mp:make-lock))
  (symbols nil :type list))

(defvar *prolog-gensym* (make-prolog-gensym))

(defun prolog-gensym (&optional (thing "PROVE"))
  #+sbcl
  (or (sb-ext:atomic-pop (pg-symbols *prolog-gensym*))
      (let ((num (sb-ext:atomic-incf (pg-counter *prolog-gensym*))))
        (make-symbol (format nil "~A~D" thing num))))
  #+lispworks
  (or (sys:atomic-pop (pg-symbols *prolog-gensym*))
      (let ((num (sys:atomic-incf (pg-counter *prolog-gensym*))))
        (make-symbol (format nil "~A~D" thing num))))
  #+ccl
  (with-lock ((pg-lock *prolog-gensym*))
    (or (pop (pg-symbols *prolog-gensym*))
        (let ((num (incf (pg-counter *prolog-gensym*))))
          (make-symbol (format nil "~A~D" thing num)))))
  #+ecl
  (with-lock ((pg-lock *prolog-gensym*))
    (or (pop (pg-symbols *prolog-gensym*))
        (let ((num (incf (pg-counter *prolog-gensym*))))
          (make-symbol (format nil "~A~D" thing num))))))

(defun release-prolog-symbol (symbol)
  #+sbcl
  (sb-ext:atomic-push symbol (pg-symbols *prolog-gensym*))
  #+lispworks
  (sys:atomic-push symbol (pg-symbols *prolog-gensym*))
  #+ccl
  (with-lock ((pg-lock *prolog-gensym*))
    (push symbol (pg-symbols *prolog-gensym*)))
  #+ecl
  (with-lock ((pg-lock *prolog-gensym*))
    (push symbol (pg-symbols *prolog-gensym*))))

(defun trace-prolog () (setq *prolog-trace* t))
(defun untrace-prolog () (setq *prolog-trace* nil))

(define-condition prolog-error (error)
  ((reason :initarg :reason :initform nil)
   ;; The ISO error term (the "ball") this condition carries, for catch/3 to
   ;; unify against.  NIL for a bare error (catch/3 then synthesizes one).
   (ball :initarg :ball :initform nil :reader prolog-error-ball))
  (:report (lambda (error stream)
             (with-slots (reason) error
               (format stream "Prolog error: ~A." reason)))))

;;; ---------------------------------------------------------------------------
;;; ISO error balls (#45).  Built-in errors carry an ISO-style term so a query
;;; can catch them with catch/3.  The error vocabulary is KEYWORDS so a ball
;;; unifies regardless of the query's package: (:error FORMAL CONTEXT), with
;;; FORMAL e.g. (:existence-error :procedure foo/2) or :instantiation-error.
;;; ---------------------------------------------------------------------------

(defun iso-ball (formal &optional context)
  "An ISO-style error ball (:error FORMAL CONTEXT)."
  (list :error formal context))

(defun existence-error-ball (kind what) (iso-ball (list :existence-error kind what)))
(defun instantiation-error-ball () (iso-ball :instantiation-error))
(defun resource-error-ball (resource) (iso-ball (list :resource-error resource)))
(defun permission-error-ball (operation type culprit)
  (iso-ball (list :permission-error operation type culprit)))
(defun type-error-ball (type culprit) (iso-ball (list :type-error type culprit)))

(define-condition prolog-throw (prolog-error)
  ()
  (:documentation "A ball thrown by throw/1, to be matched by an enclosing
catch/3.  A subclass of PROLOG-ERROR, so an uncaught throw aborts the query.")
  (:report (lambda (c stream)
             (format stream "Prolog ball thrown: ~S" (prolog-error-ball c)))))

(define-condition prolog-resource-error (prolog-error)
  ()
  (:documentation "Signaled when a query exceeds a resource bound -- its
inference budget (*INFERENCE-BUDGET* / the :MAX-INFERENCES select option) or its
wall-clock deadline (*QUERY-DEADLINE* / the :TIMEOUT option).  A subclass of
PROLOG-ERROR, so it aborts the query but is catchable; it lets a runaway or
cyclic recursion fail cleanly instead of crashing the Lisp control stack."))

(defvar *active-catches* nil
  "Markers of the catch/3 frames currently protecting their Goal (not their
continuation).  catch/3's handler only fires for a throw while its own marker is
present, so an error from the continuation after Goal succeeds is not caught.")

(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding +unbound+))

(defun bound-p (var) (not (eq (var-binding var) +unbound+)))

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

(defgeneric prolog-equal (x y)
  (:documentation "Generic equality operator for prolog unification.
 Specialize this for new types that will be stored in the db.")
  (:method ((x number) (y number)) (= x y))
  (:method ((x string) (y string)) (string= x y))
  (:method ((x character) (y character)) (char= x y))
  (:method ((x timestamp) (y timestamp)) (timestamp= x y))
  ;;(:method ((x timestamp) (y integer)) (= (timestamp-to-universal x) y))
  ;;(:method ((x integer) (y timestamp)) (= (timestamp-to-universal y) x))
  ;;(:method ((x uuid:uuid) (y uuid:uuid)) (uuid:uuid-eql x y))
  (:method ((x array) (y array)) (equalp x y))
  (:method ((x node) (y node)) (equalp (id x) (id y)))
  (:method (x y) (equal x y)))

(defun unify (x y)
  "Destructively unify two expressions."
  (cond ((prolog-equal (var-deref x) (var-deref y)) t)
        ((var-p x) (set-binding x y))
        ((var-p y) (set-binding y x))
        ((and (consp x) (consp y))
         (and (unify (first x) (first y))
              (unify (rest x) (rest y))))
        (t nil)))

(defun set-binding (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defun prolog-predicate (lst)
  (first lst))

(defmethod clause-head ((list list))
  (first list))

(defun prolog-compile-help (functor clauses)
  ;; Base case: stop once every clause has been compiled.  Without this guard,
  ;; (first nil) -> nil, relation-arity -> 0, and the function recurses forever
  ;; on the empty clause set, repeatedly compiling a degenerate arity-0 functor
  ;; -- which hangs the Lisp compiler.  This made every <- rule definition hang.
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      (compile-functor functor arity (clauses-with-arity clauses #'= arity))
      (prolog-compile-help functor (clauses-with-arity clauses #'/= arity)))))

(defmethod prolog-compile ((functor functor))
  (if (null (functor-clauses functor))
      (prolog-compile-null functor)
      (prolog-compile-help functor (functor-clauses functor))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause) (relation-arity (clause-head clause)))
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

(defun make-functor-symbol (symbol arity)
  (new-interned-symbol symbol '/ arity))

(defun make-= (x y) `(= ,x ,y))

(defun compile-call (predicate arity args cont)
  "Compile a call to a prolog predicate."
  (let ((functor (make-functor-symbol predicate arity)))
    `(let ((func (or (get-functor-fn ',functor)
                     (gethash ',functor *prolog-global-functors*))))
       (%tick)                          ; account one inference / enforce bounds
       (when *prolog-trace*
         (format t "TRACE: ~A/~A~A~%" ',predicate ',arity ',args))
       (if (functionp func)
           (funcall func ,@args ,cont)
           ;; Unknown predicate: stay noisy (a mistyped goal surfaces) but carry
           ;; an existence_error ball so catch/3 can recover from it.
           (error 'prolog-error
                  :reason (format nil "unknown Prolog functor ~A" ',functor)
                  :ball (existence-error-ball :procedure ',functor))))))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity.
  ;;
  ;; Goal heads are read in the caller's package, so a control word written in a
  ;; user package (e.g. GRAPH-DB/TEST::ONCE) is a *different* symbol from the
  ;; GRAPH-DB symbol the macro is defined on.  CL-inherited heads (=, and, or,
  ;; not, if) are the same symbol everywhere and hit directly; for the rest we
  ;; fall back to the same-named symbol interned in GRAPH-DB -- mirroring how
  ;; MAKE-FUNCTOR-SYMBOL canonicalizes runtime predicate names by string.
  (flet ((canonical (sym)
           (let ((c (find-symbol (symbol-name sym) :graph-db)))
             (and c (not (eq c sym)) (get c 'prolog-compiler-macro)))))
    (typecase name
      (string (get (intern (string-upcase name) :graph-db) 'prolog-compiler-macro))
      (symbol (or (get name 'prolog-compiler-macro) (canonical name)))
      (otherwise nil))))

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

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings old-trail)
                  collect exp)))))

(defmacro with-undo-bindings (&body body)
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
         ,(first body)
         ,@(loop for exp in (rest body)
              collect '(undo-bindings old-trail)
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
    (cond                                                  ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))       ; 12
      ((not (and (prolog-equal x x1) (prolog-equal y y1))) ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))        ; 11
      ((consp y1)                                          ; 7,10
       (values `(unify ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))    ; 4
           (values `(unify ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))     ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings))))))  ; 8,9

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
  ;; The disjuncts share one continuation FN, so a binding made in a disjunct
  ;; must be visible to FN at RUNTIME -- compile-time aliasing (= optimizing a
  ;; fresh var to a no-op) would be lost across the shared continuation.  Seed
  ;; the or's fresh variables as self-bound so = compiles to a runtime UNIFY
  ;; (trail-backed) rather than a compile-time alias.
  (let* ((bindings (bind-new-variables bindings goal))
         (disjuncts (args goal)))
    (case (length disjuncts)
      (0 +fail+)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (prolog-gensym "F")))
           `(flet ((,fn () ,(compile-body body cont bindings)))
              .,(maybe-add-undo-bindings
                 (loop for g in disjuncts collect
                      (compile-body (list g) `#',fn bindings)))))))))

;;; ---------------------------------------------------------------------------
;;; Control-flow core (#45 Phase 0).
;;;
;;; not/\+, if (->), once and forall are compiled inline through COMPILE-BODY so
;;; they thread the continuation and compose with conjunction and cut, instead
;;; of routing through the runtime call/1 functors (which cannot see the
;;; compiler's continuations or cut barriers).  A sub-goal that is not statically
;;; compilable (a meta-call variable, e.g. (not ?G)) is declined with :PASS, so
;;; COMPILE-BODY falls back to the runtime functor and today's behavior is
;;; preserved for that case.
;;;
;;; Binding threading: the controlled goal and everything that depends on its
;;; bindings (Then, the tail of the body) are compiled in a SINGLE COMPILE-BODY
;;; walk, so = and other binders propagate to the continuation exactly as they
;;; do in an ordinary conjunction.  Pre-compiling the continuation separately
;;; would drop compile-time variable aliases (e.g. (if (= ?x 5) (foo ?x) ...)).
;;;
;;; Cut scoping: a fresh barrier BLOCK wraps the controlled goal so the commit
;;; (return-from BARRIER) discards its remaining choice points.  The controlled
;;; goal is compiled with *FUNCTOR* bound to the barrier, so a cut inside it is
;;; opaque (local to the construct).  The %COMMIT helper restores *FUNCTOR* to
;;; the enclosing clause while compiling Then/Else and the body tail, so a cut
;;; there still cuts the clause (returns from the real functor block, pruning
;;; sibling clauses) -- not merely the barrier.
;;; ---------------------------------------------------------------------------

(defun static-goal-p (goal)
  "True when GOAL is a goal COMPILE-BODY can expand inline: a compound whose
head is a non-variable symbol.  A bare variable (meta-call) is not static."
  (and (consp goal)
       (symbolp (first goal))
       (not (variable-p (first goal)))))

(def-prolog-compiler-macro %commit (goal body cont bindings)
  "Internal control-macro helper -- not for use in user queries.
(%commit BARRIER OUTER) compiles the remaining body under OUTER's cut scope (so
a cut in a Then/Else or tail goal cuts the enclosing clause), then returns from
BARRIER, committing the preceding goal to its first solution."
  (destructuring-bind (barrier outer) (args goal)
    `(progn
       ,(let ((*functor* outer)) (compile-body body cont bindings))
       (return-from ,barrier nil))))

(def-prolog-compiler-macro not (goal body cont bindings)
  "Negation as failure.  (not G) continues with the rest of the body exactly
when G has no solution; G is an opaque cut barrier and leaves no bindings (the
rest of the body does not depend on G's bindings)."
  (let ((args (args goal)))
    (if (or (/= 1 (length args)) (not (static-goal-p (first args))))
        :pass
        ;; FOUND and TRAIL are gensyms: nested nots (e.g. forall's expansion)
        ;; would otherwise capture a shared FOUND, and an inner continuation's
        ;; (setf found t) would hit the wrong binding.
        (let* ((barrier (prolog-gensym "NOT"))
               (found (prolog-gensym "FOUND"))
               (trail (prolog-gensym "TRAIL"))
               ;; rest compiled under the clause's *FUNCTOR* (cut after a not
               ;; cuts the clause); G compiled under the barrier (opaque).
               (rest-code (compile-body body cont bindings))
               (g-code (let ((*functor* barrier))
                         (compile-body (list (first args))
                                       `(lambda ()
                                          (setf ,found t)
                                          (return-from ,barrier))
                                       bindings))))
          `(let ((,trail (fill-pointer *trail*))
                 (,found nil))
             (block ,barrier ,g-code)
             (undo-bindings ,trail)
             (unless ,found ,rest-code))))))

(def-prolog-compiler-macro once (goal body cont bindings)
  "(once G): commit to G's first solution, then continue with the rest of the
body (which sees G's bindings).  G is an opaque cut barrier."
  (let ((args (args goal)))
    (if (or (/= 1 (length args)) (not (static-goal-p (first args))))
        :pass
        (let ((barrier (prolog-gensym "ONCE"))
              (outer *functor*))
          `(block ,barrier
             ,(let ((*functor* barrier))
                (compile-body (list* (first args)
                                     (list '%commit barrier outer)
                                     body)
                              cont bindings)))))))

(def-prolog-compiler-macro if (goal body cont bindings)
  "(if Test Then) and (if Test Then Else): ISO soft cut.  Commit to the first
solution of Test; if Test has a solution run Then (seeing Test's bindings),
otherwise run Else (or fail, for the two-argument form).  Test is an opaque cut
barrier; a cut in Then or Else cuts the enclosing clause."
  (let* ((args (args goal))
         (n (length args)))
    (if (or (not (<= 2 n 3)) (notevery #'static-goal-p args))
        :pass
        (destructuring-bind (test then &optional else) args
          (let ((barrier (prolog-gensym "IF"))
                (trail (prolog-gensym "TRAIL"))
                (outer *functor*))
            ;; The Test->commit->Then->tail path is one walk so Test's bindings
            ;; thread into Then.  %commit returns from BARRIER after the first
            ;; Test solution; if Test never succeeds the block falls through.
            (if (= n 2)
                `(block ,barrier
                   ,(let ((*functor* barrier))
                      (compile-body
                       (list* test (list '%commit barrier outer) then body)
                       cont bindings)))
                `(let ((,trail (fill-pointer *trail*)))
                   ;; BLOCK yields NIL when Test succeeded (return-from in
                   ;; %commit), or :TEST-FAILED when it fell through -- the
                   ;; latter is the only case in which Else runs.
                   (if (block ,barrier
                         ,(let ((*functor* barrier))
                            (compile-body
                             (list* test (list '%commit barrier outer) then body)
                             cont bindings))
                         :test-failed)
                       (progn
                         (undo-bindings ,trail)
                         ,(compile-body (cons else body) cont bindings))))))))))

(def-prolog-compiler-macro forall (goal body cont bindings)
  "(forall Cond Action): succeed (continuing the body) iff Action succeeds for
every solution of Cond.  Compiled as (not (and Cond (not Action))), which reuses
the not/and macros' binding threading and cut scoping; forall leaves no bindings
and Cond/Action form an opaque barrier."
  (let ((args (args goal)))
    (if (or (/= 2 (length args)) (notevery #'static-goal-p args))
        :pass
        (destructuring-bind (cond action) args
          (compile-body
           (cons (list 'not (list 'and cond (list 'not action))) body)
           cont bindings)))))

(def-prolog-compiler-macro call (goal body cont bindings)
  "(call Goal Extra...): meta-call.  When Goal is a static goal template the
extra arguments are appended to it (compiled call/N) and the result is compiled
inline, composing with cut and the control constructs.  When Goal is a variable
(dynamic meta-call) it is solved at run time by %SOLVE-CALL."
  (let* ((cargs (args goal))
         (g (first cargs))
         (extra (rest cargs)))
    (cond
      ((null cargs) :pass)
      ;; (call (pred a b) x y) => compile (pred a b x y) directly.
      ((static-goal-p g)
       (compile-body (cons (append g extra) body) cont bindings))
      ;; Dynamic: resolve Goal at run time, appending the (compiled) extra args.
      (t
       (let ((k (if (null body)
                    cont
                    `(lambda ()
                       ,(compile-body body cont
                                      (bind-new-variables bindings goal))))))
         `(%solve-call ,(compile-arg g bindings)
                       (list ,@(mapcar (lambda (e) (compile-arg e bindings)) extra))
                       ,k))))))

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
    (when *prolog-trace*
      (format t "TRACE: ~A BODY:~% ~A~%" (clause-head clause) body))
    body))

(defun add-clause (clause)
  "add a user-defined functor"
  (let* ((functor-name (first (clause-head clause))))
    (when *prolog-trace* (format t "TRACE:  Adding clause ~A~%" clause))
    (assert (and (atom functor-name) (not (variable-p functor-name))))
    (let* ((arity (relation-arity (clause-head clause)))
           (functor (make-functor-symbol functor-name arity)))
      (if (gethash functor *prolog-global-functors*)
          (error 'prolog-error
                 :reason
                 (format nil "Cannot override default functor ~A." functor))
          (let ((f (lookup-functor functor)))
            (if (functor-p f)
                (add-functor-clause f clause)
                (make-functor :name functor :clauses (list clause))))))))

;(defun deref-copy (exp)
;  (sublis (mapcar #'(lambda (var) (cons (var-deref var) (?)))
;                 (unique-find-anywhere-if #'var-p exp))
;         exp))

(defun deref-copy (exp)
  (let ((var-alist nil))
    (labels ((walk (exp)
               (deref-exp exp)
               (cond ((consp exp)
                      (reuse-cons (walk (first exp))
                                  (walk (rest exp))
                                  exp))
                     ((var-p exp)
                      (let ((entry (assoc exp var-alist)))
                        (if (not (null entry))
                            (cdr entry)
                            (let ((var-copy (?)))
                              (push (cons exp var-copy) var-alist)
                              var-copy))))
                     (t exp))))
      (walk exp))))

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

(defmethod prolog-compile-null ((functor functor))
  (let ((*functor* (functor-name functor)))
    (set-functor-fn *functor*
                    #'(lambda (&rest args) (declare (ignore args)) nil))))

(defun compile-functor (functor arity clauses)
  "Compile all the clauses for a given symbol/arity into a single LISP
 function."
  (let ((*functor* (functor-name functor))
        (parameters (make-parameters arity)))
    (let ((func `#'(lambda (,@parameters cont)
                     (block ,*functor*
                       .,(maybe-add-undo-bindings
                          (mapcar
                           #'(lambda (clause)
                               (compile-clause parameters clause 'cont))
                           clauses))))))
      (when *prolog-trace*
        (format t "TRACE: Adding ~A to ~A~%" func *functor*))
      (set-functor-fn *functor* (eval func)))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (when *prolog-trace*
    (format t "TRACE: compile-body (~A ~A ~A)~%" body cont bindings))
  (cond
    ((null body)
     `(funcall ,cont))
    ((or (eq (first body) '!) (eq (first body) 'cut)
         (equalp (first body) "cut"))
     `(progn ,(compile-body (rest body) cont bindings)
             ,(if *functor*
                  `(return-from ,*functor* nil)
                  `(return-from :prolog-select nil))))
    (t
     (when *prolog-trace*
       (format t "TRACE: GOAL: ~A~%" (first body)))
     (let* ((goal (first body))
            (macro (prolog-compiler-macro (prolog-predicate goal)))
            (macro-val (if macro
                           (funcall macro goal (rest body) cont bindings))))
       (if (and macro (not (eq macro-val :pass)))
           macro-val
           (compile-call
            (prolog-predicate goal) (relation-arity goal)
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
  "Add a user-defined functor, or add clauses to an existing functor."
  `(let ((*functor* nil)) (add-clause ',(make-anonymous clause))))

(defun prolog-ignore (&rest args)
  (declare (ignore args))
  nil)

(defmacro ?- (&rest goals)
  "Execute an interactive prolog query."
  (let* ((goals (replace-?-vars goals))
         (vars (delete '? (variables-in goals)))
         (top-level-query (prolog-gensym "PROVE"))
         (*functor* (make-functor-symbol top-level-query 0)))
    `(let* ((*trail* (make-array 200 :fill-pointer 0 :adjustable t))
            (*var-counter* 0)
            (*functor* ',*functor*)
            (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
            (catch 'top-level-prove
              (let ((func #'(lambda (cont)
                              (handler-case
                                  (block ,*functor*
                                    .,(maybe-add-undo-bindings
                                       (mapcar
                                        #'(lambda (clause)
                                            (compile-clause nil clause 'cont))
                                        `(((,top-level-query)
                                           ,@goals
                                           (show-prolog-vars
                                            ,(mapcar #'symbol-name vars)
                                            ,vars))))))
                                (undefined-function (condition)
                                  (error 'prolog-error :reason condition))))))
                (set-functor-fn *functor* func)
                (funcall func #'prolog-ignore)
                (format t "~&No.~%")))
         (progn
           (delete-functor functor)
           (release-prolog-symbol ,top-level-query)))
       (values))))

#|
(defmacro select (vars &rest goals)
  "Select specific variables as a list of lists using the following form:
 (select (?x ?y) (is-a ?x ?y)) could return ((Joe Human) (Spot Dog)) and
 (select ((:entity ?x) (:species ?y)) could return
 (((:entity Joe) (:species Human))
  ((:entity Spot) (:species Dog)))"
  (let* ((goals (replace-?-vars goals)))
    `(let* ((top-level-query (prolog-gensym "PROVE"))
            (*functor* (make-functor-symbol top-level-query 0))
            (*trail* (make-array 200 :fill-pointer 0 :adjustable t))
            (*var-counter* 0)
            (*select-list* nil)
            (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
            (let ((func
                   #'(lambda (cont)
                       (handler-case
                           (block ,*functor*
                             .,(maybe-add-undo-bindings
                                (mapcar #'(lambda (clause)
                                            (compile-clause nil clause 'cont))
                                        `(((top-level-query)
                                           ,@goals
                                           (select
                                            ,(mapcar
                                              #'(lambda (var)
                                                  (typecase var
                                                    (symbol (symbol-name var))
                                                    (list (first var))))
                                              vars) ,vars))))))
                         (undefined-function (condition)
                           (error 'prolog-error :reason condition))))))
              (set-functor-fn *functor* func)
              (funcall func #'prolog-ignore))
         (delete-functor functor))
       (nreverse *select-list*))))
|#

(defvar *select-limit* nil)
(defvar *select-skip* nil)
(defvar *select-current-count* 0)
(defvar *select-current-skip* 0)
(defvar *prolog-block-name* nil)
(defvar *select-flat* nil)
(defvar *seen-table* nil)
(defvar *select-count-only* nil
  "When true (SELECT :COUNT t / SELECT-COUNT), the query counts solutions
without projecting or consing their bindings.")

(defvar *query-params* nil
  "Alist of (KEY . VALUE) parameter bindings for the current query, read by the
PARAM/2 functor.  Bound by DEF-QUERY (and bindable around any SELECT) to inject
runtime values into a query without an arbitrary-eval functor.")

(defvar *select-callback* nil
  "When set (SELECT :CALLBACK), each result row is passed to this function as it
is produced -- streaming -- instead of being consed into *SELECT-LIST*.  SELECT
then returns the number of rows streamed.  Honors :LIMIT and :SKIP.")

(defun %count-tick (cont)
  "Account one solution under the active :SKIP/:LIMIT window without consing any
bindings, then continue (or stop once :LIMIT is reached).  The :COUNT-mode
counterpart of the SELECT/2 collector; the running total is *SELECT-CURRENT-COUNT*."
  (when (or (null *select-limit*) (< *select-current-count* *select-limit*))
    (when (and *select-skip* (<= *select-current-skip* *select-skip*))
      (incf *select-current-skip*))
    (when (or (null *select-skip*) (> *select-current-skip* *select-skip*))
      (incf *select-current-count*)))
  (if (or (null *select-limit*) (< *select-current-count* *select-limit*))
      (funcall cont)
      (throw :prolog-limit-reached nil)))

;;; ---------------------------------------------------------------------------
;;; Query resource bounds (#45 Phase 0.4 / Phase 1).
;;;
;;; Opt-in safety for untrusted or possibly-runaway queries (the #44 web surface
;;; is the motivating case): bound a query by a maximum number of inferences and
;;; a wall-clock deadline, so a non-terminating or cyclic-recursive query fails
;;; with a catchable PROLOG-RESOURCE-ERROR instead of looping forever or crashing
;;; the Lisp control stack.  Both default to nil (unlimited), so trusted queries
;;; keep their current behavior; per-query :MAX-INFERENCES / :TIMEOUT options (or
;;; the *DEFAULT-* globals) turn them on.
;;; ---------------------------------------------------------------------------

(defvar *inference-budget* nil
  "Maximum number of inferences (compiled goal calls / meta-call steps) allowed
for the current query, or nil for unlimited.")
(defvar *inference-count* 0
  "Inferences accounted so far in the current query.")
(defvar *query-deadline* nil
  "INTERNAL-REAL-TIME after which the current query aborts, or nil for none.")
(defvar *default-inference-budget* nil
  "Inference budget applied to queries that don't specify :MAX-INFERENCES.")
(defvar *default-query-timeout* nil
  "Default query timeout in seconds for queries that don't specify :TIMEOUT.")

(defun %deadline (seconds)
  "Translate a timeout in SECONDS into an INTERNAL-REAL-TIME deadline (or nil)."
  (when seconds
    (+ (get-internal-real-time)
       (round (* seconds internal-time-units-per-second)))))

(declaim (inline %tick))
(defun %tick ()
  "Account one inference and abort the query (PROLOG-RESOURCE-ERROR) if a
resource bound is exceeded.  A no-op when no bound is in effect."
  (when *inference-budget*
    (when (> (incf *inference-count*) *inference-budget*)
      (error 'prolog-resource-error
             :reason (format nil "inference budget exceeded (~D)" *inference-budget*)
             :ball (resource-error-ball :inferences))))
  (when (and *query-deadline* (>= (get-internal-real-time) *query-deadline*))
    (error 'prolog-resource-error :reason "query timeout exceeded"
           :ball (resource-error-ball :time))))

;;; ---------------------------------------------------------------------------
;;; Effect partitioning (#45 Phase 1).
;;;
;;; Goals are partitioned into pure logic + graph READS (always permitted -- a
;;; query is for reading) and a few SIDE-EFFECTING functors, each tagged with an
;;; effect: :write (mutates the graph -- retract), :eval (evaluates arbitrary
;;; Lisp -- lisp/lispp/is/trigger), :io (stream input/output -- read/write/nl).
;;; A side-effecting functor calls REQUIRE-EFFECT at entry; if its effect is not
;;; permitted by the current policy it signals a catchable
;;; PROLOG-PERMISSION-ERROR before performing the effect.
;;;
;;; *ALLOWED-EFFECTS* is T (everything -- the default, so trusted queries are
;;; unchanged) or a list of permitted effect tags; (:effects ...) on SELECT sets
;;; it per query.  An untrusted surface (e.g. the #44 web layer) runs read-only
;;; queries with :effects nil, which still allows all reads and pure logic but
;;; forbids mutation, arbitrary eval, and io.  The check is transitive: a
;;; side-effecting functor reached through a user rule or a meta-call is caught
;;; the same way, since the tag lives on the functor itself.
;;; ---------------------------------------------------------------------------

(define-condition prolog-permission-error (prolog-error)
  ()
  (:documentation "Signaled when a goal attempts a side effect (:write, :eval or
:io) that the current query's effect policy (*ALLOWED-EFFECTS* / the :EFFECTS
select option) does not permit.  A subclass of PROLOG-ERROR, so it aborts the
query but is catchable."))

(defvar *allowed-effects* t
  "Effects permitted for the current query: T = all (the default), or a list of
permitted tags drawn from (:write :eval :io).  Graph reads and pure logic are
always allowed and are not tagged.")
(defvar *default-allowed-effects* t
  "Effect policy applied to queries that don't specify :EFFECTS.")

(defun require-effect (effect)
  "Signal PROLOG-PERMISSION-ERROR unless EFFECT is permitted by the current
*ALLOWED-EFFECTS* policy.  Called at entry by every side-effecting functor."
  (unless (or (eq *allowed-effects* t)
              (member effect *allowed-effects*))
    (error 'prolog-permission-error
           :reason (format nil "~A is not permitted in this query" effect)
           :ball (permission-error-ball effect :query nil))))

(defun plist-alist (plist)
  "Transform a plist into an alist."
  (iterate:iter (iterate:for k iterate:in plist iterate::by 'cddr)
                (iterate:for v iterate:in (cdr plist) iterate::by 'cddr)
                (iterate:collect (cons k v))))

(defmacro select (options vars &rest goals)
  "Run the Prolog query GOALS and collect the bindings of the result VARS.

VARS is a list of ?-variables (e.g. (?a ?b)).  GOALS are query goals such as
(is-a ?u user), an edge functor (follows ?a ?b), (node-slot-value ?n slot ?v),
or (lisp ?x form).  OPTIONS is a plist; the most useful are :FLAT (return a
flat list of the single var's values rather than a list of tuples), :LIMIT,
and :SKIP.  :MAX-INFERENCES bounds the number of inference steps and :TIMEOUT
bounds the wall-clock seconds; exceeding either aborts the query with a
PROLOG-RESOURCE-ERROR (both default to the *DEFAULT-INFERENCE-BUDGET* /
*DEFAULT-QUERY-TIMEOUT* globals, which are nil = unlimited).  :EFFECTS sets the
side-effect policy -- T (all, the default) or a list of permitted tags drawn
from (:write :eval :io); a disallowed effect aborts with a
PROLOG-PERMISSION-ERROR.  Reads and pure logic are always allowed, so
:EFFECTS nil yields a safe read-only query.  :COUNT t returns the integer number
of solutions instead of the list, consing nothing per solution (see
SELECT-COUNT).  :SNAPSHOT t runs the query under one consistent MVCC read
snapshot, so all of its reads resolve at a single epoch -- stable against
concurrent writers (it inherits an enclosing transaction if one is active).
:CALLBACK FN streams each result row to FN as it is produced (consing nothing
onto a result list) and returns the number of rows -- for unbounded or very
large result sets.  Returns a list of solutions (or, with :COUNT or :CALLBACK, a
count).

A query runs against the current *GRAPH*.  See SELECT-FLAT, SELECT-ONE and
SELECT-FIRST for common shorthands."
  (let* ((goals (replace-?-vars goals))
         (options (plist-alist options)))
    `(let* ((top-level-query (prolog-gensym "PROVE"))
            (*functor* (make-functor-symbol top-level-query 0))
            (*trail* (make-array 200 :fill-pointer 0 :adjustable t))
            (*var-counter* 0)
            (*select-list* nil)
            (*select-flat* ,(cdr (assoc :flat options)))
            (*select-limit* ,(cdr (assoc :limit options)))
            (*select-skip* ,(cdr (assoc :skip options)))
            (*select-current-count* 0)
            (*select-current-skip* 0)
            (*inference-budget* ,(if (assoc :max-inferences options)
                                     (cdr (assoc :max-inferences options))
                                     '*default-inference-budget*))
            (*inference-count* 0)
            (*query-deadline* (%deadline ,(if (assoc :timeout options)
                                              (cdr (assoc :timeout options))
                                              '*default-query-timeout*)))
            (*allowed-effects* ,(if (assoc :effects options)
                                    `',(cdr (assoc :effects options))
                                    '*default-allowed-effects*))
            (*select-count-only* ,(cdr (assoc :count options)))
            (*select-callback* ,(cdr (assoc :callback options)))
            (*seen-table* (make-hash-table)) ;; For unique values
            (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
            (let ((func
                   (lambda (cont)
                     (handler-case
                         (block :prolog-select
                           (catch :prolog-limit-reached
                             .,(maybe-add-undo-bindings
                                (mapcar #'(lambda (clause)
                                            (compile-clause nil clause 'cont))
                                        `(((top-level-query)
                                           ,@goals
                                           (select
                                            ,(mapcar
                                              (lambda (var)
                                                (typecase var
                                                  (symbol (symbol-name var))
                                                  (list (first var))))
                                              vars) ,vars)))))))
                       (undefined-function (condition)
                         (error 'prolog-error :reason condition))))))
              (set-functor-fn *functor* func)
              ,(if (cdr (assoc :snapshot options))
                   ;; Run the query under one consistent MVCC read snapshot:
                   ;; all reads resolve at a single epoch (lock-free, stable
                   ;; against concurrent writers).  Inherits an enclosing
                   ;; transaction if one is already active.
                   `(call-with-read-snapshot
                     (lambda () (funcall func #'prolog-ignore)) *graph*)
                   `(funcall func #'prolog-ignore)))
         (progn
           (delete-functor functor)
           (release-prolog-symbol top-level-query)))
       (if (or *select-count-only* *select-callback*)
           *select-current-count*
           (nreverse *select-list*)))))

(defmacro select-flat (vars &rest goals)
  "Like SELECT with :FLAT t: return a flat list of values rather than a list of
tuples.  Most convenient with a single result variable."
  `(select (:flat t) ,vars ,@goals))

(defmacro select-count (vars &rest goals)
  "Run the query GOALS and return the NUMBER of solutions as an integer, without
projecting or consing any bindings.  VARS may be () when only the count matters.
For a capped or offset count use the SELECT (:count t :limit N :skip M ...) form;
SELECT-COUNT itself counts every solution."
  `(select (:count t) ,vars ,@goals))

(defmacro select-first (vars &rest goals)
  "Return only the first solution's tuple for VARS (cuts after the first
match)."
  `(first (select () ,vars ,@goals !)))

(defmacro select-one (vars &rest goals)
  "Return the first value of the first result variable from the first solution
-- ideal for single-result lookups (e.g. find one node by a property)."
  `(first (select (:flat t :limit 1) ,vars ,@goals !)))

(defmacro do-query (&rest goals)
  "Run GOALS purely for their side effects, collecting nothing.  Useful with
goals like (trigger ...) or (retract ...)."
  `(select () () ,@goals))

#|
(defmacro map-query (fn vars goals &key collect-p remove-nulls-p)
  "Select specific variables as a list of lists using the following form:
 (select (?x ?y) (is-a ?x ?y)) could return ((Joe Human) (Spot Dog)) and
 (select ((:entity ?x) (:species ?y)) could return
 (((:entity Joe) (:species Human))
  ((:entity Spot)(:species Dog)))"
  (let* ((goals (replace-?-vars goals)))
    `(let* ((top-level-query (prolog-gensym "PROVE"))
            (*functor* (make-functor-symbol top-level-query 0))
            (*trail* (make-array 200 :fill-pointer 0 :adjustable t))
            (*var-counter* 0)
            (*select-list* nil)
            (functor (make-functor :name *functor* :clauses nil)))
       (unwind-protect
            (let ((func
                   #'(lambda (cont)
                       (handler-case
                           (block ,*functor*
                             .,(maybe-add-undo-bindings
                                (mapcar #'(lambda (clause)
                                            (compile-clause nil clause 'cont))
                                        `(((top-level-query)
                                           ,@goals
                                           (map-query
                                            ,fn
                                            ,(mapcar
                                              #'(lambda (var)
                                                  (typecase var
                                                    (symbol (symbol-name var))
                                                    (list (first var))))
                                              vars) ,vars
                                              ,collect-p ,remove-nulls-p))))))
                         (undefined-function (condition)
                           (error 'prolog-error :reason condition))))))
              (set-functor-fn *functor* func)
              (funcall func #'prolog-ignore))
         (delete-functor functor))
       (nreverse *select-list*))))
|#

(defmacro map-query (fn query &key collect-p)
  "Maps fn over the results of query. collect-p will return a list of the
 results of each application of fn."
  (with-gensyms (result)
    (if collect-p
        `(mapcar (lambda (,result)
                   (apply ,fn ,result))
                 ,query)
        `(dolist (,result ,query)
           (apply ,fn ,result)))))

(defun valid-prolog-query-p (form)
  (case (first form)
    (select t)
    (select-one t)
    (select-flat t)
    (select-first t)
    (<- t)
    (insertt)
    (otherwise nil)))
