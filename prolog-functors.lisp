(in-package #:graph-db)

(defvar *prolog-global-functors*
  #+sbcl (make-hash-table :synchronized t :test 'equalp)
  #+ccl (make-hash-table :shared t :test 'equalp)
  #+lispworks (make-hash-table :single-thread nil :test 'equalp)
  #+ecl (make-hash-table :test 'equalp))

(defmacro def-global-prolog-functor (name lambda-list &body body)
  "Define a global Prolog functor (query predicate) NAME, which must be of the
form PREDICATE/ARITY (e.g. divisible-by/2).  LAMBDA-LIST is the predicate's
arguments followed by a final CONT continuation argument.  In BODY, VAR-DEREF
each argument to get its value, and FUNCALL CONT once for each solution to
signal success (not calling CONT means the goal fails).  To bind an unbound
argument, UNIFY it and undo with UNDO-BINDINGS on backtracking.  In a query you
write the predicate WITHOUT the /arity suffix; the compiler appends it from the
goal's argument count."
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (export ',name)
     (setf (gethash ',name *prolog-global-functors*) #',name)))

(defun default-functor-p (symbol)
  (gethash symbol *prolog-global-functors*))

(def-global-prolog-functor read/1 (exp cont)
  (require-effect :io)
  (if (unify exp (read)) (funcall cont)))

(def-global-prolog-functor write/1 (exp cont)
  (require-effect :io)
  (format t "~A" (deref-exp exp)) (funcall cont))

(def-global-prolog-functor nl/0 (cont)
  (require-effect :io)
  (terpri) (funcall cont))

(def-global-prolog-functor repeat/0 (cont)
  ;; %tick each iteration so an enclosing :timeout / :max-inferences can break
  ;; an otherwise-unbounded (repeat ... fail) loop, which never re-enters
  ;; compile-call and so would not otherwise be accounted.
  (loop (%tick) (funcall cont)))

(def-global-prolog-functor fail/0 (cont)
  (declare (ignore cont))
  nil)

(def-global-prolog-functor =/2 (?arg1 ?arg2 cont)
  "Unifies two prolog variables."
  (if (unify ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)
  "Checks equality of the values of two prolog variables."
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor /=/2 (?arg1 ?arg2 cont)
  "Checks inequality of the values of two prolog variables."
  (if (not (deref-equal ?arg1 ?arg2)) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)
  "Prolog greater than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (> ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  "Prolog less than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (< ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor >=/2 (?arg1 ?arg2 cont)
  "Prolog greater than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (>= ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor <=/2 (?arg1 ?arg2 cont)
  "Prolog less than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (<= ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor numberp/1 (x cont)
  (when (numberp (var-deref x))
    (funcall cont)))

(def-global-prolog-functor atom/1 (x cont)
  (when (atom (var-deref x))
    (funcall cont)))

(def-global-prolog-functor not-in-list/2 (item list cont)
  (when (null (find (var-deref item) (var-deref list) :test 'node-equal))
    (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  "Call out to lisp from within a Prolog query.  Assigns result to the
 supplied Prolog var.  (lisp ?result (+ 1 2)).  Any lisp variables that you
 wish to access within a prolog query using the lisp functor should be
 declared special."
  (require-effect :eval)
  (let ((exp (deref-exp exp)))
    (when *prolog-trace* (format t "TRACE: LISP/2 ?result <- ~A~%" exp))
    (cond ((consp exp)
           (if (unify ?result (eval exp))
           ;;(if (unify ?result (apply (first exp) (rest exp)))
               (funcall cont)))
          ((symbolp exp)
           ;;(if (unify ?result (eval exp))
           (if (unify ?result (funcall #'symbol-value exp))
               (funcall cont)))
          (t
           (if (unify ?result exp)
               (funcall cont))))))

(def-global-prolog-functor lispp/1 (exp cont)
  "Call out to lisp from within a Prolog query and throws away the result.
 Any lisp variables that you wish to access within a prolog query using the
 lisp functor should be declared special."
  (require-effect :eval)
  (let ((exp (deref-exp exp)))
    (when *prolog-trace* (format t "TRACE: LISPP/1 ~A~%" exp))
    (cond ((consp exp)
           ;;(format t "applying ~A to ~A~%" (first exp) (rest exp))
           (eval exp))
           ;;(apply (first exp) (rest exp)))
          ((and (symbolp exp) (boundp exp)) (funcall #'identity exp))
          (t exp))
    (funcall cont)))

(def-global-prolog-functor regex-match/2 (?arg1 ?arg2 cont)
  "Functor that treats first arg as a regex and uses cl-ppcre:scan to check
 for the pattern in the second arg."
  (if (and (stringp (var-deref ?arg1))
           (stringp (var-deref ?arg2))
           (cl-ppcre:scan ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor var/1 (?arg1 cont)
  (if (unbound-var-p ?arg1) (funcall cont)))

(def-global-prolog-functor param/2 (?var key cont)
  "Unify ?VAR with the value bound to KEY in *QUERY-PARAMS* (the parameters of a
DEF-QUERY, or any SELECT run with *QUERY-PARAMS* bound).  A pure read -- it
injects a runtime value into a query without arbitrary evaluation, so it is
permitted even under the strictest effect policy (:effects nil)."
  (setq key (var-deref key))
  (let ((cell (assoc key *query-params* :test #'equal)))
    (when (and cell (unify ?var (cdr cell)))
      (funcall cont))))

(def-global-prolog-functor is/2 (var exp cont)
  "Similar to lisp/2, but unifies instead of assigns the lisp return value."
  (require-effect :eval)
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
           (unify var (eval (deref-exp exp))))
      (funcall cont)))

;;; ---------------------------------------------------------------------------
;;; Runtime meta-call solver (#45 Phase 0.2).
;;;
;;; %SOLVE is the runtime counterpart of COMPILE-BODY: it proves a goal whose
;;; structure (and possibly whose functor) is only known at run time -- the
;;; dynamic meta-call case, (call ?G).  Statically-known meta-calls compile
;;; inline via the CALL compiler macro and never reach here.
;;;
;;; It handles conjunction, disjunction, call/N (argument appending) and the
;;; true/fail atoms directly; not/if/once/forall and ordinary predicates resolve
;;; through the functor tables (their runtime functors route their sub-goals back
;;; through here via call/1).  An UNKNOWN predicate (or an uninstantiated goal)
;;; signals a PROLOG-ERROR -- deliberately noisy, so a mistyped predicate surfaces
;;; rather than silently yielding no answers.  (A future catch/3 + ISO
;;; existence_error/instantiation_error will make this recoverable; see #45.)
;;; ---------------------------------------------------------------------------

(defun control-head-p (head name)
  "True when symbol HEAD names the control construct NAME, regardless of package
(goal heads are read in the caller's package)."
  (and (symbolp head)
       (string-equal (symbol-name head) (symbol-name name))))

(defun %solve-conj (goals cont)
  "Prove a conjunction of GOALS, threading the continuation."
  (if (null goals)
      (funcall cont)
      (%solve (first goals)
              (lambda () (%solve-conj (rest goals) cont)))))

(defun %solve-disj (goals cont)
  "Prove a disjunction of GOALS, enumerating every branch (undoing bindings
between branches).  Unlike the legacy or/2 functor this does not truncate."
  (dolist (g goals)
    (let ((old-trail (fill-pointer *trail*)))
      (%solve g cont)
      (undo-bindings old-trail))))

(defun %solve (goal cont)
  "Prove GOAL at run time, invoking CONT once per solution.  GOAL may be atomic,
compound, or a control construct; its variables are runtime VAR structs."
  (%tick)                               ; account one inference / enforce bounds
  (setf goal (var-deref goal))
  (cond
    ((var-p goal)
     (error 'prolog-error :reason "meta-call of an uninstantiated goal"
            :ball (instantiation-error-ball)))
    ((null goal) nil)
    ((symbolp goal) (%solve (list goal) cont)) ; bare atom goal -> 0-arity form
    ((consp goal)
     (let ((head (var-deref (first goal)))
           (gargs (rest goal)))
       (cond
         ((not (symbolp head))
          (error 'prolog-error
                 :reason (format nil "meta-call goal with non-symbol head ~S" head)
                 :ball (type-error-ball :callable head)))
         ((control-head-p head 'true) (funcall cont))
         ((control-head-p head 'fail) nil)
         ;; cut inside a *dynamic* meta-call is opaque and not honored (call is
         ;; a cut barrier); it simply succeeds.  Static call honors cut.
         ((or (control-head-p head '!) (control-head-p head 'cut)) (funcall cont))
         ((control-head-p head 'and) (%solve-conj gargs cont))
         ((control-head-p head 'or) (%solve-disj gargs cont))
         ((control-head-p head 'call) (%solve-call (first gargs) (rest gargs) cont))
         (t
          (let* ((functor (make-functor-symbol head (length gargs)))
                 (fn (or (get-functor-fn functor)
                         (gethash functor *prolog-global-functors*))))
            (if (functionp fn)
                (apply fn (append gargs (list cont)))
                ;; Unknown predicate: noisy, but with an existence_error ball so
                ;; catch/3 can recover from it.
                (error 'prolog-error
                       :reason (format nil "unknown Prolog functor ~A" functor)
                       :ball (existence-error-ball :procedure functor))))))))
    (t nil)))

(defun %solve-call (goal extra cont)
  "Prove (call GOAL . EXTRA): append the EXTRA arguments to GOAL's argument list
(call/N), then solve.  GOAL is dereferenced first."
  (setf goal (var-deref goal))
  (%solve (cond ((null extra) goal)
                ((consp goal) (append goal extra))
                (t (cons goal extra)))
          cont))

(def-global-prolog-functor call/1 (goal cont)
  "Meta-call GOAL -- atomic, compound, or a control construct.  An unknown
predicate signals a PROLOG-ERROR (deliberately noisy).  Delegates to %SOLVE, the
shared runtime solver."
  (%solve goal cont))

;;; ---------------------------------------------------------------------------
;;; ISO exceptions: throw/1 and catch/3 (#45).
;;; ---------------------------------------------------------------------------

(def-global-prolog-functor throw/1 (ball cont)
  "throw(Ball): raise BALL as a Prolog exception for an enclosing catch/3 whose
Catcher unifies with it.  An uninstantiated BALL is an instantiation_error."
  (declare (ignore cont))               ; throw never returns to its continuation
  (let ((b (var-deref ball)))
    (if (var-p b)
        (error 'prolog-error :reason "throw of an uninstantiated ball"
               :ball (instantiation-error-ball))
        ;; copy the ball (resolve bindings, rename remaining vars apart) so it
        ;; survives the trail unwind back to the catching frame
        (error 'prolog-throw :ball (deref-copy (deref-exp ball))))))

(def-global-prolog-functor catch/3 (goal catcher recovery cont)
  "catch(Goal, Catcher, Recovery): prove GOAL; if it throws a ball that unifies
with CATCHER, undo GOAL's bindings and prove RECOVERY instead.  A ball that does
not unify propagates to an outer catch.  Errors raised by the CONTINUATION after
GOAL succeeds are not caught here (only GOAL is protected).  Resource and
permission errors are NOT catchable -- they must abort the query so a bounded,
untrusted query cannot swallow its own enforcement."
  (let ((marker (list :catch))
        (entry (fill-pointer *trail*))
        (caught nil))
    (block solve
      (handler-bind
          ((prolog-error
             (lambda (c)
               (when (and (member marker *active-catches* :test #'eq)
                          (not (typep c 'prolog-resource-error))
                          (not (typep c 'prolog-permission-error)))
                 (let ((ball (or (prolog-error-ball c)
                                 (iso-ball (list :system-error (princ-to-string c))))))
                   (undo-bindings entry)
                   (cond ((unify catcher ball)
                          (setf caught t)
                          (return-from solve)) ; unwind, then run recovery below
                         (t (undo-bindings entry))))))))  ; decline -> propagate
        (let ((*active-catches* (cons marker *active-catches*)))
          (%solve goal
                  (lambda ()
                    ;; the continuation runs outside this catch's protection
                    (let ((*active-catches*
                            (remove marker *active-catches* :test #'eq)))
                      (funcall cont)))))))
    ;; reached when GOAL exhausted with no caught throw, or after one unwinds
    (when caught
      (%solve recovery cont))))

(def-global-prolog-functor if/2 (?test ?then cont)
  (when *prolog-trace* (format t "TRACE: IF/2(~A ~A)~%" ?test ?then))
  (call/1 ?test #'(lambda () (call/1 ?then cont))))

(def-global-prolog-functor if/3 (?test ?then ?else cont)
  "(if Test Then Else): ISO soft cut.  Commit to the first solution of Test; if
Test has a solution run Then, otherwise run Else.  Else runs ONLY when Test has
no solution -- not when Test succeeds but Then fails.  This is the runtime
(meta-call) counterpart of the IF compiler macro; the two must agree."
  (when *prolog-trace* (format t "TRACE: IF/3(~A ~A ~A)~%" ?test ?then ?else))
  (let ((old-trail (fill-pointer *trail*))
        (cond-met nil))
    (block done
      (call/1 ?test #'(lambda ()
                        (setf cond-met t)
                        (call/1 ?then cont)
                        (return-from done))))
    (unless cond-met
      (undo-bindings old-trail)
      (call/1 ?else cont))))

(def-global-prolog-functor once/1 (goal cont)
  "(once Goal): succeed at most once -- commit to Goal's first solution.  Runtime
(meta-call) counterpart of the ONCE compiler macro."
  (block done
    (call/1 goal #'(lambda () (funcall cont) (return-from done)))))

(def-global-prolog-functor forall/2 (cond action cont)
  "(forall Cond Action): succeed iff Action succeeds for every solution of Cond.
Runtime (meta-call) counterpart of the FORALL compiler macro."
  (let ((old-trail (fill-pointer *trail*))
        (ok t))
    (block done
      (call/1 cond
              #'(lambda ()
                  (let ((inner-trail (fill-pointer *trail*))
                        (act nil))
                    (block inner
                      (call/1 action #'(lambda () (setf act t) (return-from inner))))
                    (undo-bindings inner-trail)
                    (unless act (setf ok nil) (return-from done))))))
    (undo-bindings old-trail)
    (when ok (funcall cont))))

(let ((date-regex
       "^(19|20)\\d\\d[\-\ \/\.](0[1-9]|1[012])[\-\ \/\.](0[1-9]|[12][0-9]|3[01])$"))
  (def-global-prolog-functor valid-date-p/1 (date cont)
    "Date validation functor. FIXME: This needs to be fleshed out with a
 more comprehensive regex."
    (var-deref date)
    (if (and (stringp date)
             (cl-ppcre:scan date-regex date))
        (funcall cont))))

(def-global-prolog-functor trigger/1 (exp cont)
  "Call out to lisp ignoring the return value."
  (require-effect :eval)
  (eval (deref-exp exp))
  ;;(let ((exp (deref-exp exp)))
    ;;(typecase exp
      ;;(cons   (apply (first exp) (rest exp)))
      ;;(symbol (symbol-value exp))))
  (funcall cont))

(def-global-prolog-functor not/1 (relation cont)
  "Prolog negation.  Does not retract, simply negates in the context of the
query."
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(def-global-prolog-functor or/2 (goal1 goal2 cont)
  (call/1 goal1 #'(lambda () (funcall cont) (return-from or/2 t)))
  (call/1 goal2 #'(lambda () (funcall cont) (return-from or/2 t))))

;;; ---------------------------------------------------------------------------
;;; All-solutions aggregation (#45 Phase 0.3): findall/3, bagof/3, setof/3.
;;;
;;; findall collects every TEMPLATE instance (always succeeds; [] on none, no
;;; grouping).  bagof/setof additionally fail on no solutions and GROUP by the
;;; goal's free (witness) variables -- those in Goal but not in Template and not
;;; existentially quantified by ^ -- producing one solution per witness binding.
;;; setof sorts each group by the standard order of terms and removes duplicates.
;;; ---------------------------------------------------------------------------

(defun %term-vars (term &optional acc)
  "Distinct unbound VAR structs occurring in TERM (post-deref), prepended to ACC."
  (setf term (var-deref term))
  (cond ((var-p term) (if (member term acc) acc (cons term acc)))
        ((consp term) (%term-vars (rest term) (%term-vars (first term) acc)))
        (t acc)))

(defun %strip-existential (goal)
  "Strip leading ^ quantifiers.  Returns (values inner-goal existential-vars).
Supports (^ Var Goal) and (^ (Var...) Goal), nestable."
  (let ((evars nil))
    (loop
      (setf goal (var-deref goal))
      (if (and (consp goal)
               (control-head-p (var-deref (first goal)) '^)
               (= 2 (length (rest goal))))
          (let ((v (var-deref (second goal))))
            (dolist (x (if (consp v) v (list v)))
              (setf evars (%term-vars x evars)))
            (setf goal (var-deref (third goal))))
          (return)))
    (values goal evars)))

(defun %findall-list (template goal)
  "Run GOAL, snapshotting TEMPLATE per solution; return the snapshots in solution
order.  Bindings are undone before returning.  DEREF-EXP (not DEREF-COPY)
resolves bound variables to their values so the snapshot survives the undo --
DEREF-COPY renames variables apart and would drop the bindings."
  (let ((acc nil)
        (old-trail (fill-pointer *trail*)))
    (%solve goal (lambda () (push (deref-exp template) acc)))
    (undo-bindings old-trail)
    (nreverse acc)))

(defun %group-by-witness (pairs)
  "PAIRS is a list of (Witness . Template) snapshots.  Group by Witness
(structural equality), preserving first-appearance order; return a list of
(Witness . list-of-Template)."
  (let ((groups nil))
    (dolist (p pairs)
      (let ((entry (assoc (car p) groups :test #'deref-equal)))
        (if entry
            (push (cdr p) (cdr entry))
            (setf groups (nconc groups (list (list (car p) (cdr p))))))))
    (mapcar (lambda (g) (cons (car g) (nreverse (cdr g)))) groups)))

(defun %term-rank (x)
  (cond ((var-p x) 0) ((numberp x) 1) ((characterp x) 2)
        ((symbolp x) 3) ((stringp x) 4) ((node-p x) 5) ((consp x) 6) (t 7)))

(defun %bytes< (a b)
  (let ((la (length a)) (lb (length b)))
    (dotimes (i (min la lb) (< la lb))
      (let ((x (aref a i)) (y (aref b i)))
        (when (/= x y) (return (< x y)))))))

(defun %term-< (a b)
  "A partial standard order of terms: Var < Number < Char < Symbol < String <
Node < Cons.  Used to sort setof results."
  (setf a (var-deref a) b (var-deref b))
  (let ((ra (%term-rank a)) (rb (%term-rank b)))
    (if (/= ra rb)
        (< ra rb)
        (case ra
          (0 (< (var-name a) (var-name b)))
          (1 (< a b))
          (2 (char< a b))
          (3 (string< (symbol-name a) (symbol-name b)))
          (4 (string< a b))
          (5 (%bytes< (id a) (id b)))
          (6 (cond ((%term-< (car a) (car b)) t)
                   ((%term-< (car b) (car a)) nil)
                   (t (%term-< (cdr a) (cdr b)))))
          (t nil)))))

(defun %sort-unique (items)
  "Sort ITEMS by the standard order of terms and drop duplicates (deref-equal)."
  (let ((result nil))
    (dolist (x (stable-sort (copy-list items) #'%term-<) (nreverse result))
      (unless (and result (deref-equal (first result) x))
        (push x result)))))

(def-global-prolog-functor findall/3 (template goal result cont)
  "findall(Template, Goal, List): List is the list of Template instances, one per
solution of Goal, in order ([] if none).  Always succeeds and does not bind
Goal's free variables."
  (if (unify result (%findall-list template (var-deref goal)))
      (funcall cont)))

(defun %solve-aggregate (template goal result cont sort-p)
  "Shared core of bagof/setof: collect Template per solution of GOAL, grouped by
GOAL's witness variables.  Fails when GOAL has no solutions.  When SORT-P, each
group's list is sorted and de-duplicated (setof); otherwise order/dups are kept
(bagof)."
  (multiple-value-bind (inner evars) (%strip-existential goal)
    (let* ((tmpl-vars (%term-vars template))
           (witnesses (remove-if (lambda (v) (or (member v tmpl-vars)
                                                 (member v evars)))
                                 (%term-vars inner)))
           (pairs (%findall-list (cons witnesses template) inner)))
      (dolist (group (%group-by-witness pairs))
        (let ((old-trail (fill-pointer *trail*))
              (items (if sort-p (%sort-unique (cdr group)) (cdr group))))
          (when (and (unify witnesses (car group))
                     (unify result items))
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor bagof/3 (template goal result cont)
  "bagof(Template, Goal, Bag): Bag is the list of Template instances for one
binding of Goal's witness variables (free vars not in Template, not bound by ^),
backtracking over witness bindings.  Fails when Goal has no solutions; keeps
order and duplicates."
  (%solve-aggregate template goal result cont nil))

(def-global-prolog-functor setof/3 (template goal result cont)
  "setof(Template, Goal, Set): like bagof, but each Set is sorted by the standard
order of terms with duplicates removed."
  (%solve-aggregate template goal result cont t))

(def-global-prolog-functor show-prolog-vars/2 (var-names vars cont)
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
         for var in vars do
           (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(let ((graph-pkg (find-package :graph-db)))
#|
  (def-global-prolog-functor select/2 (var-names vars cont)
    (if (null vars)
        nil
        (push (loop for name in var-names
                 for var in vars
                 collect (let ((var (deref-exp var)))
                           (cond ((and (symbolp var)
                                       (eq graph-pkg (symbol-package var)))
                                  (symbol-name var))
                                 ((and (consp var)
                                       (eq (first var) name)
                                       (symbolp (second var))
                                       (eq graph-pkg
                                           (symbol-package (second var))))
                                  (list name (symbol-name (second var))))
                                 (t var))))
              *select-list*))
    (funcall cont))
|#

  (def-global-prolog-functor select/2 (var-names vars cont)
    (when *prolog-trace*
      (dbg "TRACE: SELECT/2(~A ~A)~%" var-names vars)
      (dbg "TRACE: SELECT/2 COUNT: ~A, SKIP: ~A" *select-current-count* *select-current-skip*)
      (dbg "TRACE: SELECT/2 SELECT-LIST: ~{~A~^ ~}" *select-list*))
    ;; :COUNT mode -- tally solutions (honoring :skip/:limit) without projecting
    ;; or consing any bindings.
    (when *select-count-only*
      (return-from select/2 (%count-tick cont)))
    (when (or (null *select-limit*)
              (< *select-current-count* *select-limit*))
      (when (not (null vars))
        (let ((r (loop for name in var-names
                    for var in vars
                    collect (let ((var (deref-exp var)))
                              (cond ((and (symbolp var)
                                          (eq graph-pkg (symbol-package var)))
                                     (symbol-name var))
                                    ((and (consp var)
                                          (eq (first var) name)
                                          (symbolp (second var))
                                          (eq graph-pkg
                                              (symbol-package (second var))))
                                     (list name (symbol-name (second var))))
                                    (t var))))))
          (when (and *select-skip* (<= *select-current-skip* *select-skip*))
            (incf *select-current-skip*))
          (when (or (null *select-skip*)
                    (> *select-current-skip* *select-skip*))
            (incf *select-current-count*)
            (cond (*select-callback*
                   ;; streaming: hand the row to the callback as it is produced,
                   ;; consing nothing onto *select-list*
                   (funcall *select-callback* (if *select-flat* (first r) r)))
                  (*select-flat*
                   (dolist (i r) (push i *select-list*)))
                  (t (push r *select-list*)))))))
    (if (or (null *select-limit*) (< *select-current-count* *select-limit*))
        (funcall cont)
        (throw :prolog-limit-reached nil)))

  (def-global-prolog-functor map-query/5
      (fn var-names vars collect-p remove-nulls-p cont)
    (when *prolog-trace*
      (format t "TRACE: MAP-QUERY/5 FN (~A) IS ~A~%COLLECT-P is ~A~%"
              (type-of fn) fn collect-p))
    (if (null vars)
        nil
        (let ((new-vars
               (loop for name in var-names
                  for var in vars
                  collect (let ((var (deref-exp var)))
                            (cond ((and (symbolp var)
                                        (eq graph-pkg (symbol-package var)))
                                   (symbol-name var))
                                  ((and (consp var)
                                        (eq (first var) name)
                                        (symbolp (second var))
                                        (eq graph-pkg
                                            (symbol-package (second var))))
                                   (list name (symbol-name (second var))))
                                  (t var))))))
          (let ((result (eval `(apply ,fn ',new-vars))))
            (when collect-p
              (unless (and remove-nulls-p (null result))
                (push result *select-list*))))))
    (funcall cont)))

(def-global-prolog-functor outgoing-edges/2 (vertex var cont)
  (setq vertex (var-deref vertex)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/2(~S ~S)~%" vertex var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :out))

(def-global-prolog-functor outgoing-edges/3 (vertex edge-type var cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/3(~S ~S ~S)~%" vertex edge-type var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :out :edge-type edge-type))

(def-global-prolog-functor incoming-edges/2 (vertex var cont)
  (setq vertex (var-deref vertex)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/2(~S ~S)~%" vertex var))
  (map-edges (lambda (edge)
              (let ((old-trail (fill-pointer *trail*)))
                (when (unify var edge)
                  (funcall cont))
                (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :in))

(def-global-prolog-functor incoming-edges/3 (vertex edge-type var cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        var (var-deref var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/3(~S ~S ~S)~%" vertex edge-type var))
  (map-edges (lambda (edge)
               (let ((old-trail (fill-pointer *trail*)))
                 (when (unify var edge)
                   (funcall cont))
                 (undo-bindings old-trail)))
             *graph* :vertex vertex :direction :in :edge-type edge-type))

(def-global-prolog-functor incoming-edges/4 (vertex edge-type edge-var vertex-var
                                                    cont)
  (setq vertex (var-deref vertex)
        edge-type (var-deref edge-type)
        edge-var (var-deref edge-var)
        vertex-var (var-deref vertex-var))
  (when *prolog-trace*
    (format t "TRACE: incoming-edges/4(~S ~S ~S ~S)~%"
            vertex edge-type edge-var vertex-var))
    (map-edges (lambda (edge)
                 (let ((old-trail (fill-pointer *trail*)))
                   (when (unify edge-var edge)
                     (let ((v2 (lookup-vertex (from edge))))
                       (when (unify vertex-var v2)
                         (funcall cont))))
                   (undo-bindings old-trail)))
               *graph* :vertex vertex :direction :in :edge-type edge-type))

(def-global-prolog-functor outgoing-edges/4 (from edge-type edge-var to cont)
  (setq from (var-deref from)
        edge-type (var-deref edge-type)
        edge-var (var-deref edge-var)
        to (var-deref to))
  (when *prolog-trace*
    (format t "TRACE: outgoing-edges/4(~S ~S ~S ~S)~%"
            from edge-type edge-var to))
  (cond ((and (vertex-p from) (vertex-p to) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :from-vertex from
                    :to-vertex to
                    :edge-type edge-type))
        ((and (vertex-p from) (vertex-p to))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :from-vertex from
                    :to-vertex to))
        ((and (vertex-p from) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v2 (lookup-vertex (to edge))))
                            (when (unify to v2)
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex from :direction :out :edge-type edge-type))
        ((and (vertex-p to) (symbolp edge-type))
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v2 (lookup-vertex (from edge))))
                            (when (unify from v2)
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex to :direction :in :edge-type edge-type))
        ((symbolp edge-type)
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify edge-var edge)
                          (let ((v1 (lookup-vertex (from edge)))
                                (v2 (lookup-vertex (to edge))))
                            (when (and (unify from v1) (unify to v2))
                              (funcall cont))))
                        (undo-bindings old-trail)))
                    *graph* :vertex from :direction :out :edge-type edge-type))))

(def-global-prolog-functor invoke-view/4 (class-name view-name key node cont)
  ;; FIXME: does it make more sense to return the key/value or the key/node?
  (when *prolog-trace*
    (format t "TRACE: INVOKE-VIEW/4(~A ~A ~A ~A)~%" class-name view-name key node))
  (setq class-name (var-deref class-name)
        view-name (var-deref view-name)
        key (var-deref key)
        node (var-deref node))
  (dolist (pair (if (and (var-p key) (bound-p key))
                    (invoke-graph-view class-name view-name :key (var-deref key))
                    (invoke-graph-view class-name view-name)))
    (let ((old-trail (fill-pointer *trail*)))
      (when (and (unify key (cdr (assoc :key pair)))
                 (unify node (lookup-vertex (cdr (assoc :id pair)))))
        (funcall cont))
      (undo-bindings old-trail))))

(def-global-prolog-functor invoke-view/5 (class-name view-name key node value cont)
  (when *prolog-trace*
    (format t "TRACE: INVOKE-VIEW/5(~A ~A ~A ~A ~A)~%" class-name view-name key node value))
  (setq class-name (var-deref class-name)
        view-name (var-deref view-name)
        key (var-deref key)
        value (var-deref value)
        node (var-deref node))
  (dolist (pair (if (and (var-p key) (bound-p key))
                    (invoke-graph-view class-name view-name :key (var-deref key))
                    (invoke-graph-view class-name view-name)))
    (let ((old-trail (fill-pointer *trail*)))
      (when (and (unify key (cdr (assoc :key pair)))
                 (unify value (cdr (assoc :value pair)))
                 (unify node (lookup-vertex (cdr (assoc :id pair)))))
        (funcall cont))
      (undo-bindings old-trail))))

(def-global-prolog-functor invoke-reduced-view/4 (class-name view-name key value cont)
  (when *prolog-trace*
    (format t "TRACE: INVOKE-REDUCED-VIEW/4(~A ~A ~A ~A)~%" class-name view-name key value))
  (setq value (var-deref value)
        class-name (var-deref class-name)
        view-name (var-deref view-name)
        key (var-deref key))
  (if (not (var-p key))
      (let ((alist (invoke-graph-view class-name view-name :key key :group-p t)))
        (when (and alist
                   (unify key (cdr (assoc :key alist)))
                   (unify value (cdr (assoc :value alist))))
          (funcall cont)))
      (map-reduced-view (lambda (k i v)
                          (declare (ignore i))
                          (let ((old-trail (fill-pointer *trail*)))
                            (when (and (unify key k)
                                       (unify value v))
                              (funcall cont))
                            (undo-bindings old-trail)))
                        class-name view-name)))

(def-global-prolog-functor node-slot-value/3 (node slot var cont)
  (setq node (var-deref node)
        slot (var-deref slot)
        var (var-deref var))
  ;; Guard ONLY the slot read -- not (funcall cont).  The continuation is the
  ;; rest of the query; wrapping it here would swallow any error a downstream
  ;; goal signals (e.g. a prolog-permission-error from a denied write, or a
  ;; prolog-resource-error), silently turning it into a non-match.
  (let ((value (handler-case (node-slot-value node slot)
                 (error (c)
                   (log:error "Problem reading (node-slot-value ~A ~A): ~A"
                              node slot c)
                   (return-from node-slot-value/3 nil)))))
    (when (unify var value)
      (funcall cont))))

(def-global-prolog-functor weight/2 (edge var cont)
  (setq edge (var-deref edge)
        var (var-deref var))
  (when (and (typep edge 'edge)
             (unify var (weight edge)))
    (funcall cont)))

(def-global-prolog-functor unique/1 (node cont)
  (let ((var-name (var-name node)))
    (let ((table (or (gethash var-name *seen-table*)
                     (setf (gethash var-name *seen-table*)
                           (make-node-table)))))
      (setq node (var-deref node))
      ;; On ECL MAKE-NODE-TABLE is an EQUALP table keyed by node-id (ECL has no
      ;; custom hash-table tests); elsewhere it is a NODE-EQUAL table keyed by
      ;; the node object.  NODE-EQUAL is (equalp (id x) (id y)), so keying by
      ;; (id node) under EQUALP is equivalent.
      (let ((key #+ecl (and (node-p node) (id node))
                 #-ecl node))
        (when (or (not (node-p node))
                  (null (gethash key table)))
          (when (node-p node)
            (setf (gethash key table) node))
          (funcall cont))))))

(def-global-prolog-functor is-a/2 (node type cont)
  (setq node (var-deref node)
        type (var-deref type))
  (cond ((and (vertex-p node) (symbolp type))
         (when (typep node type)
           (funcall cont)))
        ((vertex-p node)
         (when (unify type (type-of node))
           (funcall cont)))
        ((and (symbolp type) (lookup-node-type-by-name type :vertex))
         (map-vertices
          (lambda (vertex)
            (let ((old-trail (fill-pointer *trail*)))
              (when (unify node vertex)
                (funcall cont))
              (undo-bindings old-trail)))
          *graph* :vertex-type type))
        (t
         ;; Both NODE and TYPE unbound: enumerate every vertex.  Rather than a
         ;; single untyped lhash scan (which reads LIVE versions, bypassing
         ;; snapshot isolation), delegate to per-type scans -- each goes through
         ;; LOOKUP-VERTEX, so under a snapshot transaction every vertex resolves
         ;; at the reader's epoch.  :INCLUDE-SUBCLASSES-P NIL visits each vertex
         ;; exactly once, under its own type-id (vertex type-ids start at 1, so
         ;; the 0 sentinel from LIST-VERTEX-TYPES never resolves and is skipped).
         (dolist (type-id (list-vertex-types *graph*))
           (when (lookup-node-type-by-id type-id :vertex)
             (map-vertices
              (lambda (vertex)
                (let ((old-trail (fill-pointer *trail*)))
                  (when (unify node vertex)
                    (when (unify type (type-of vertex))
                      (funcall cont)))
                  (undo-bindings old-trail)))
              *graph* :vertex-type type-id :include-subclasses-p nil))))))

(def-global-prolog-functor retract/1 (node cont)
  (require-effect :write)
  (setq node (var-deref node))
  (when (node-p node)
    (mark-deleted node)
    (funcall cont)))

(def-global-prolog-functor retract/3 (edge-type from to cont)
  (require-effect :write)
  (setq edge-type (var-deref edge-type)
        from (var-deref from)
        to (var-deref to))
  (cond ((and (symbolp edge-type) (vertex-p from) (vertex-p to))
         ;; use vev-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (dbg "1: retracting ~A" edge)
                        (mark-deleted edge)
                        (funcall cont)
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :from-vertex from
                    :to-vertex to))
        ((and (symbolp edge-type) (vertex-p from))
         ;; use ve-out-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify to (lookup-vertex (to edge)))
                          (dbg "2: retracting ~A" edge)
                          (mark-deleted edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :vertex from
                    :direction :out))
        ((and (symbolp edge-type) (vertex-p to))
         ;; use ve-in-index and delete
         (map-edges (lambda (edge)
                      (let ((old-trail (fill-pointer *trail*)))
                        (when (unify from (lookup-vertex (from edge)))
                          (dbg "3: retracting ~A" edge)
                          (mark-deleted edge)
                          (funcall cont))
                        (undo-bindings old-trail)))
                    *graph*
                    :edge-type edge-type
                    :vertex to
                    :direction :in))
        (t
         (error 'prolog-error
                :reason
                (format nil "retract/3: cowardly refusing to retract(~A ~A ~A)"
                        edge-type from to)))))

;;; ---------------------------------------------------------------------------
;;; Spatial predicates (part of the public spatial extension).
;;;
;;; These are pure geometric building blocks over bound coordinate/geometry
;;; values -- they carry no domain knowledge and need no spatial index.  Compose
;;; them with slot accessors to filter graph nodes by location, e.g.:
;;;   (select-flat (?n)
;;;     (is-a ?n eo-find)
;;;     (node-slot-value ?n lon ?lon) (node-slot-value ?n lat ?lat)
;;;     (geo-near ?lat ?lon 49.2 37.17 500.0))
;;; Index-backed, node-yielding spatial functors come once the write-path hook
;;; populates the spatial index (deferred with the MVCC apply-path work).
;;; ---------------------------------------------------------------------------

(def-global-prolog-functor geo-distance/5 (?lat1 ?lon1 ?lat2 ?lon2 ?dist cont)
  "Unify ?DIST with the geodesic distance (metres) between (?LAT1,?LON1) and
(?LAT2,?LON2).  The four coordinates must be bound numbers."
  (let ((lat1 (var-deref ?lat1)) (lon1 (var-deref ?lon1))
        (lat2 (var-deref ?lat2)) (lon2 (var-deref ?lon2)))
    (when (and (numberp lat1) (numberp lon1) (numberp lat2) (numberp lon2))
      (when (unify ?dist (geodesic-distance lat1 lon1 lat2 lon2))
        (funcall cont)))))

(def-global-prolog-functor geo-near/5 (?lat1 ?lon1 ?lat2 ?lon2 ?radius cont)
  "Succeed when (?LAT1,?LON1) and (?LAT2,?LON2) are within ?RADIUS metres of one
another.  All five arguments must be bound numbers."
  (let ((lat1 (var-deref ?lat1)) (lon1 (var-deref ?lon1))
        (lat2 (var-deref ?lat2)) (lon2 (var-deref ?lon2)) (radius (var-deref ?radius)))
    (when (and (numberp lat1) (numberp lon1) (numberp lat2) (numberp lon2)
               (numberp radius))
      (when (<= (geodesic-distance lat1 lon1 lat2 lon2) radius)
        (funcall cont)))))

(def-global-prolog-functor geo-within/3 (?lon ?lat ?area cont)
  "Succeed when point (?LON, ?LAT) lies within the :POLYGON or :MULTIPOLYGON
geometry ?AREA.  ?LON/?LAT must be bound numbers and ?AREA a bound geometry."
  (let ((lon (var-deref ?lon)) (lat (var-deref ?lat)) (area (var-deref ?area)))
    (when (and (numberp lon) (numberp lat) (geometryp area))
      (when (geometry-contains-point-p area lon lat)
        (funcall cont)))))
