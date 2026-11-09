;;;; explain-goals: a static account of a goal list -- the functor each
;;;; goal resolves to, which arguments are bound there, and the access
;;;; path it will take (tracer/explain design D3, GH #124).  Nothing
;;;; runs and nothing is interned.

(in-package #:graph-db)

(defun %explain-functor (head arity)
  "The registered NAME/ARITY functor HEAD resolves to, by
MAKE-FUNCTOR-SYMBOL's lookup order and without its interning; NIL when
none is registered."
  (let ((name (%functor-name-string head arity)))
    (flet ((hit (pkg)
             (and pkg
                  (let ((s (find-symbol name pkg)))
                    (and s (%registered-functor-p s) s)))))
      (or (hit (symbol-package head))
          (hit (find-package :graph-db))))))

(defun %explain-kind (functor)
  (let ((f (lookup-functor functor)))
    (if (and f (functor-clauses f)) :user :global)))

(defun %explain-bound (args bound)
  "One boolean per argument: a literal, or a variable BOUND names."
  (mapcar (lambda (a)
            (cond ((eq a '?) nil)
                  ((variable-p a) (and (member a bound) t))
                  (t t)))
          args))

(defun %explain-bind (args bound)
  "BOUND with every variable in ARGS added."
  (union (remove '? (variables-in args)) bound))

(defun %explain-row (goal functor kind bound path &optional candidates)
  (let ((row (list :goal goal :functor functor :kind kind :bound bound
                   :path path)))
    (if candidates (append row (list :candidates candidates)) row)))

(defun %explain-user-path (functor args bound)
  "An asserted functor's path and (K . N): the clauses its first-argument
index leaves for this call (design D3)."
  (let* ((f (lookup-functor functor))
         (set (functor-clause-set f))
         (n (length (clause-set-fns set)))
         (every (format nil "every clause: ~D" n)))
    (cond ((or (null args) (not *clause-indexing*) (not (first bound)))
           (values every (cons n n)))
          ((variable-p (first args))
           (values (format nil "first-argument index: by value at run ~
time, ~D clauses" n)
                   (cons nil n)))
          (t (multiple-value-bind (bucket all-p)
                 (%candidates set (first args) t)
               (if all-p
                   (values every (cons n n))
                   (let ((k (+ (length bucket)
                               (length (clause-set-vars set)))))
                     (values (format nil "first-argument index: ~D of ~D ~
clauses" k n)
                             (cons k n)))))))))

(defun %explain-predicate (goal bound rows graph)
  (let* ((head (first goal))
         (args (rest goal))
         (bl (%explain-bound args bound))
         (functor (and (symbolp head) (%explain-functor head (length args)))))
    (if (null functor)
        (values (cons (%explain-row goal nil :unknown bl
                                    "no such functor: the call signals")
                      rows)
                (%explain-bind args bound))
        (let ((kind (%explain-kind functor)))
          (multiple-value-bind (path candidates)
              (if (eq kind :user)
                  (%explain-user-path functor args bl)
                  (let ((fn (gethash functor *access-paths*)))
                    (values (if fn
                                (funcall fn bl args graph)
                                "no path declared")
                            nil)))
            (when (functor-cost-unbounded-p functor)
              (setf path (concatenate 'string path ", cost-unbounded")))
            (values (cons (%explain-row goal functor kind bl path candidates)
                          rows)
                    (%explain-bind args bound)))))))

(defun %explain-goal (goal bound rows graph)
  "(VALUES ROWS BOUND) after GOAL: its row(s) consed onto ROWS, the
variables bound after it.  The control constructs are entered as the
compiler macros enter them; NOT and FORALL bind nothing for the goals
after them; OR and a two-branch IF bind what every branch binds."
  (flet ((meta (args)
           (values (cons (%explain-row goal nil :control
                                       (%explain-bound args bound)
                                       "meta-call, decided at run time")
                         rows)
                   bound)))
    (cond
      ((variable-p goal) (meta '()))
      ((atom goal)
       (values (cons (%explain-row goal nil :control '()
                                   (format nil "inline ~(~A~)" goal))
                     rows)
               bound))
      ((variable-p (first goal)) (meta (rest goal)))
      ((and (symbolp (first goal)) (prolog-compiler-macro (first goal)))
       (let ((name (symbol-name (first goal))) (args (rest goal)))
         (cond
           ((or (string= name "AND") (string= name "ONCE"))
            (%explain-walk args bound rows graph))
           ((string= name "OR")
            (let ((after nil) (first-branch t))
              (dolist (branch args)
                (multiple-value-bind (r b)
                    (%explain-goal branch bound rows graph)
                  (setf rows r
                        after (if first-branch b (intersection after b))
                        first-branch nil)))
              (values rows (or after bound))))
           ((or (string= name "NOT") (string= name "FORALL"))
            (values (%explain-walk args bound rows graph) bound))
           ((string= name "IF")
            (destructuring-bind (test then &optional (else nil else-p)) args
              (multiple-value-bind (r1 b1) (%explain-goal test bound rows graph)
                (multiple-value-bind (r2 b2) (%explain-goal then b1 r1 graph)
                  (if else-p
                      (multiple-value-bind (r3 b3)
                          (%explain-goal else bound r2 graph)
                        (values r3 (intersection b2 b3)))
                      (values r2 b2))))))
           ((string= name "CALL")
            (let ((g (first args)))
              (if (and (consp g) (symbolp (first g))
                       (not (variable-p (first g))))
                  (%explain-goal (append g (rest args)) bound rows graph)
                  (meta args))))
           (t
            (values (cons (%explain-row goal nil :control
                                        (%explain-bound args bound)
                                        (format nil "inline ~(~A~)" name))
                          rows)
                    (%explain-bind args bound))))))
      (t (%explain-predicate goal bound rows graph)))))

(defun %explain-walk (goals bound rows graph)
  (dolist (goal goals (values rows bound))
    (multiple-value-setq (rows bound) (%explain-goal goal bound rows graph))))

(defun explain-goals (vars goals &key (graph *graph*) stream)
  "One row per goal of GOALS, in evaluation order, without running
anything (design D3, GH #124): (:GOAL form :FUNCTOR name/arity-or-nil
:KIND :user|:global|:control|:unknown :BOUND booleans :PATH string
[:CANDIDATES (k . n)]).  VARS start unbound; a variable is bound after
the first goal that mentions it, except inside NOT and FORALL.  With
STREAM, the rows are also printed, one line each."
  (declare (ignore vars))
  (let ((rows (nreverse (%explain-walk goals '() '() graph))))
    (when stream
      (dolist (row rows)
        (format stream "~&~S~%    ~A~@[ (~A)~]: ~A~%"
                (getf row :goal)
                (if (getf row :functor)
                    (symbol-name (getf row :functor))
                    (getf row :kind))
                (and (getf row :functor) (string-downcase (getf row :kind)))
                (getf row :path))))
    rows))

;;; The type and slot predicates (design D3).

(register-access-path
 'is-a/2
 (lambda (bound args graph)
   (destructuring-bind (node type) args
     (declare (ignore node))
     (cond ((first bound) "type check")
           ((and (second bound) (variable-p type))
            "type index, by value at run time")
           ((second bound)
            (if (and (symbolp type)
                     (lookup-node-type-by-name type :vertex :graph graph))
                (format nil "type index: ~A" type)
                (format nil "every vertex, type by type (~A is not a type ~
here)" type)))
           (t "every vertex, type by type")))))

(register-access-path
 'node-slot-value/3
 (lambda (bound args graph)
   (declare (ignore args graph))
   (if (first bound) "slot read" "every vertex of every type")))

;;; profile-goals (design D4): the goals run under a counting hook.

(defun %profile-row (table functor)
  (or (gethash functor table)
      (setf (gethash functor table)
            (list :functor functor :calls 0 :exits 0 :redos 0 :fails 0
                  :clauses 0 :seconds 0.0 :open '()))))

(defun profile-goals (vars goals &key (graph *graph*) stream limit
                                      max-inferences timeout)
  "Run GOALS collecting VARS as SELECT would, read-only and under one
snapshot, with a hook that counts each functor's ports and sums the
wall time from each of its calls to the matching fail (callees
included); (VALUES SOLUTIONS ROWS ERROR), ROWS one plist per functor
sorted by :SECONDS, ERROR the PROLOG-RESOURCE-ERROR a tripped budget
signalled, else NIL -- the rows of a run cut short are what ran, and
balanced, since the fails fire on the way out (design D1, D4, GH
#124).  LIMIT, MAX-INFERENCES and TIMEOUT as SELECT's; with STREAM the
rows are also printed."
  (let ((table (make-hash-table :test 'eq))
        (solutions 0)
        (err nil))
    (flet ((hook (port functor args depth)
             (declare (ignore args depth))
             (let ((r (%profile-row table functor)))
               (ecase port
                 (:call (incf (getf r :calls))
                        (push (get-internal-real-time) (getf r :open)))
                 (:exit (incf (getf r :exits)))
                 (:redo (incf (getf r :redos)))
                 (:fail (incf (getf r :fails))
                        (let ((start (pop (getf r :open))))
                          (when start
                            (incf (getf r :seconds)
                                  (/ (- (get-internal-real-time) start)
                                     (float internal-time-units-per-second))))))
                 (:clause (incf (getf r :clauses)))))))
      (handler-case
          (let ((*graph* graph)
                (form `(select (:effects nil :snapshot t :limit ,limit
                                ,@(when max-inferences
                                    `(:max-inferences ,max-inferences))
                                ,@(when timeout `(:timeout ,timeout))
                                :callback ,(lambda (row)
                                             (declare (ignore row))
                                             (incf solutions)))
                               ,vars ,@goals)))
            (call-with-prolog-ports
             #'hook
             (lambda ()
               (let (#+sbcl (sb-ext:*evaluator-mode* :interpret))
                 (eval form)))))
        (prolog-resource-error (c) (setf err c))))
    (let ((rows (sort (loop for r being the hash-values of table
                            collect (let ((plist (copy-list r)))
                                      (remf plist :open)
                                      plist))
                      #'> :key (lambda (r) (getf r :seconds)))))
      (when stream
        (dolist (r rows)
          (format stream "~&~A: ~D calls, ~D exits, ~D redos, ~D fails, ~
~D clauses, ~,3F s~%"
                  (symbol-name (getf r :functor)) (getf r :calls)
                  (getf r :exits) (getf r :redos) (getf r :fails)
                  (getf r :clauses) (getf r :seconds))))
      (values solutions rows err))))
