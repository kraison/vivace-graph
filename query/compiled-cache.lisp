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

;;; The cache (design D3): a bounded, locked table from
;;; (generation . shape) to an entry; a shape compiles on its
;;; *COMPILE-QUERY-AFTER*th hit, outside the lock.

(defvar *compiled-query-cache-size* 256
  "Entries the compiled query cache keeps; the least recently used
goes first (design D3).")

(defvar *compile-query-after* 2
  "The hit on which a shape is compiled (design D3).")

(defstruct (cached-query (:constructor %make-cached-query (shape)))
  "One shape's entry: FN the compiled function, NIL until compiled or
:FAILED when the compiler refused the form; HITS the lookups; LAST-USED
the cache clock at the last lookup; COMPILING while one thread is
compiling it."
  shape (fn nil) (hits 0) (last-used 0) (compiling nil))

(defvar *compiled-query-cache* (make-hash-table :test 'equal))
(defvar *compiled-query-cache-lock* (bt:make-lock "compiled-query-cache"))
(defvar *cache-clock* 0)
(defvar *query-cache-counters* (list :hits 0 :misses 0 :compiles 0
                                     :evictions 0))

(defun clear-query-cache ()
  "Empty the compiled query cache and its counters."
  (bt:with-lock-held (*compiled-query-cache-lock*)
    (clrhash *compiled-query-cache*)
    (setf *query-cache-counters*
          (list :hits 0 :misses 0 :compiles 0 :evictions 0)))
  nil)

(defun query-cache-stats ()
  "(:entries n :compiled n :hits n :misses n :compiles n :evictions n)."
  (bt:with-lock-held (*compiled-query-cache-lock*)
    (let ((compiled 0))
      (maphash (lambda (key entry)
                 (declare (ignore key))
                 (when (functionp (cached-query-fn entry)) (incf compiled)))
               *compiled-query-cache*)
      (list* :entries (hash-table-count *compiled-query-cache*)
             :compiled compiled
             (copy-list *query-cache-counters*)))))

(defun %evict-least-recent ()
  "Under the lock: drop the entry used longest ago."
  (let ((oldest nil) (oldest-key nil))
    (maphash (lambda (key entry)
               (when (or (null oldest)
                         (< (cached-query-last-used entry)
                            (cached-query-last-used oldest)))
                 (setf oldest entry oldest-key key)))
             *compiled-query-cache*)
    (when oldest-key
      (remhash oldest-key *compiled-query-cache*)
      (incf (getf *query-cache-counters* :evictions)))))

(defun %compile-query-form (form)
  "The function FORM compiles to: SBCL's native compiler; elsewhere the
function EVAL makes (ECL's bytecodes compiler; CCL compiles natively),
never ECL's C compiler (design D6)."
  (let ((*package* (find-package :graph-db)))
    #+sbcl (compile nil `(lambda () ,form))
    #-sbcl (eval `(lambda () ,form))))

(defun %cached-query-function (shape literal-count)
  "The compiled function for SHAPE, or NIL when this call runs
interpreted: a new or young entry, one another thread is compiling, or
one the compiler refused (design D3)."
  (let ((key (cons *functor-generation* shape))
        (entry nil)
        (compile-p nil))
    (bt:with-lock-held (*compiled-query-cache-lock*)
      (setf entry (gethash key *compiled-query-cache*))
      (cond (entry (incf (getf *query-cache-counters* :hits)))
            (t (incf (getf *query-cache-counters* :misses))
               (loop while (>= (hash-table-count *compiled-query-cache*)
                               *compiled-query-cache-size*)
                     do (%evict-least-recent))
               (setf entry (%make-cached-query shape)
                     (gethash key *compiled-query-cache*) entry)))
      (incf (cached-query-hits entry))
      (setf (cached-query-last-used entry) (incf *cache-clock*))
      (when (and (null (cached-query-fn entry))
                 (not (cached-query-compiling entry))
                 (>= (cached-query-hits entry) *compile-query-after*))
        (setf (cached-query-compiling entry) t
              compile-p t)))
    (when compile-p
      (let ((fn (handler-case
                    (%compile-query-form (%cached-query-form shape
                                                             literal-count))
                  (error (e)
                    (log:error "compiled query cache: ~A" e)
                    :failed))))
        (bt:with-lock-held (*compiled-query-cache-lock*)
          (setf (cached-query-fn entry) fn
                (cached-query-compiling entry) nil)
          (when (functionp fn)
            (incf (getf *query-cache-counters* :compiles))))))
    (let ((fn (cached-query-fn entry)))
      (and (functionp fn) fn))))
