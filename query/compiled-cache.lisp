;;;; The compiled query cache (GH #376): a client query's shape,
;;;; canonicalised with its literals lifted out, and the one SELECT form
;;;; every call of that shape evaluates.  Design:
;;;; docs/superpowers/specs/2026-11-10-compiled-query-cache-design.md.

(in-package :graph-db)

(defun %query-variable-p (x)
  "A Prolog variable as written: a symbol whose name starts with ?,
other than ? itself, from any package (the guard's scratch symbols and
the rules' uninterned ones included)."
  (and (symbolp x) x
       (let ((name (symbol-name x)))
         (and (> (length name) 1) (char= (char name 0) #\?)))))

(defun %canonical-variable (i)
  "The i-th canonical variable, ?Vi in GRAPH-DB."
  (intern (format nil "?V~D" i) :graph-db))

(defun %literal-variable (i)
  "The variable the i-th lifted literal is bound to, ?Li in GRAPH-DB."
  (intern (format nil "?L~D" i) :graph-db))

(defun %canonical-query (vars goals)
  "VARS and GOALS as one shape for every call that shares them:
(VALUES SHAPE LITERALS).  SHAPE is (canonical-vars . canonical-goals)
with every variable renamed ?Vn by first appearance and every string,
number or character in a term position replaced by (%lit . i);
LITERALS is the vector of those values (design D1).  Not a term
position: an aggregate-all's spec (its variables are still renamed) and
the arguments of lisp, lispp and is (an expression)."
  (let ((names '()) (next 0) (literals '()) (count 0))
    (labels ((var (x)
               (let ((hit (assoc x names :test #'eq)))
                 (if hit
                     (cdr hit)
                     (let ((v (%canonical-variable next)))
                       (incf next)
                       (push (cons x v) names)
                       v))))
             (lift (x)
               (push x literals)
               (prog1 (cons '%lit count) (incf count)))
             (term (x)
               (cond ((eq x '?) x)
                     ((%query-variable-p x) (var x))
                     ((or (stringp x) (numberp x) (characterp x)) (lift x))
                     ((consp x) (cons (term (car x)) (term (cdr x))))
                     (t x)))
             (no-lift (x)
               ;; variables renamed, nothing lifted
               (cond ((%query-variable-p x) (var x))
                     ((consp x) (cons (no-lift (car x)) (no-lift (cdr x))))
                     (t x)))
             (goal (g)
               (cond ((not (consp g)) (term g))
                     ((not (symbolp (first g))) (term g))
                     (t (let ((name (symbol-name (first g))))
                          (cond ((member name '("LISP" "LISPP" "IS")
                                         :test #'string=)
                                 (cons (first g) (no-lift (rest g))))
                                ((and (string= name "AGGREGATE-ALL")
                                      (= 3 (length (rest g))))
                                 (list (first g)
                                       (no-lift (second g))
                                       (goal (third g))
                                       (term (fourth g))))
                                ((member name '("AND" "OR" "NOT" "ONCE"
                                                "IF" "FORALL")
                                         :test #'string=)
                                 (cons (first g) (mapcar #'goal (rest g))))
                                (t (cons (first g)
                                         (mapcar #'term (rest g))))))))))
      (let* ((cvars (mapcar #'term vars))
             (cgoals (mapcar #'goal goals)))
        (values (cons cvars cgoals)
                (coerce (nreverse literals) 'simple-vector))))))

(defun %literal-markers-to-variables (tree)
  "TREE with each (%lit . i) replaced by its variable ?Li."
  (cond ((and (consp tree) (eq (car tree) '%lit) (integerp (cdr tree)))
         (%literal-variable (cdr tree)))
        ((consp tree)
         (cons (%literal-markers-to-variables (car tree))
               (%literal-markers-to-variables (cdr tree))))
        (t tree)))

(defun %cached-query-form (shape literal-count)
  "The SELECT form every call of SHAPE evaluates (design D1, D2): the
options read from specials, the literals bound first by %BIND-LITERALS
when there are any, then the canonical goals."
  (let ((vars (car shape))
        (goals (%literal-markers-to-variables (cdr shape))))
    `(select (:effects nil :snapshot t
              :limit *query-run-limit*
              :skip *query-run-skip*
              :max-inferences *query-default-max-inferences*
              :timeout *query-default-timeout*
              :callback *pattern-query-callback*)
             ,vars
             ,@(when (plusp literal-count)
                 `((%bind-literals
                    ,@(loop for i below literal-count
                            collect (%literal-variable i)))))
             ,@goals)))
