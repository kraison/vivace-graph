;;;; tests/query/compiled-cache-tests.lisp -- the compiled query cache
;;;; (GH #376, design
;;;; docs/superpowers/specs/2026-11-10-compiled-query-cache-design.md).
;;;; Every name here is prefixed CQ-.

(in-package #:graph-db/query-test)

(in-suite query-suite)

(defun cq-canon (vars goals)
  "(VALUES SHAPE LITERALS) of VARS and GOALS."
  (graph-db::%canonical-query vars goals))

(defun cq-shape (vars goals)
  (nth-value 0 (cq-canon vars goals)))

(test cq-canonical-shape
  "Variables renamed by first appearance everywhere, literals lifted in
term positions only, two equal literals two entries, ? kept, nested
lists walked; a different projection, goal order or symbol is another
shape; a shape with no literal has none (design D1, Review Focus 5)."
  (multiple-value-bind (s1 l1)
      (cq-canon '(?s) '((find-by-slot ?s qt-item label "a")))
    (multiple-value-bind (s2 l2)
        (cq-canon '(?s) '((find-by-slot ?s qt-item label "b")))
      (is (equal s1 s2))
      (is (equalp #("a") l1))
      (is (equalp #("b") l2))
      (is (equal '((graph-db::?v0)
                   (find-by-slot graph-db::?v0 qt-item label
                                 (graph-db::%lit . 0)))
                 s1))))
  (is (not (equal (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")))
                  (cq-shape '(?s ?l) '((find-by-slot ?s qt-item label "a"))))))
  (is (not (equal (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")))
                  (cq-shape '(?s) '((find-by-slot ?s qt-item label a))))))
  (is (not (equal (cq-shape '(?x ?y) '((qt-links ?x ?y)))
                  (cq-shape '(?y ?x) '((qt-links ?x ?y))))))
  (is (equal (cq-shape '(?x) '((qt-links ?x ?y) (is-a ?y qt-item)))
             (cq-shape '(?a) '((qt-links ?a ?b) (is-a ?b qt-item)))))
  (is (not (equal (cq-shape '(?x) '((qt-links ?x ?y) (is-a ?y qt-item)))
                  (cq-shape '(?x) '((is-a ?y qt-item) (qt-links ?x ?y))))))
  ;; graph-db's ? kept (any other ?-named symbol is a variable);
  ;; nested lists walked
  (is (equal '((graph-db::?v0) (qt-links graph-db::?v0 graph-db::?))
             (cq-shape '(?x) '((qt-links ?x graph-db::?)))))
  (multiple-value-bind (s l) (cq-canon '(?x) '((= ?x (a 1 "b"))))
    (is (equal '((graph-db::?v0)
                 (= graph-db::?v0 (a (graph-db::%lit . 0)
                                     (graph-db::%lit . 1))))
               s))
    (is (equalp #(1 "b") l)))
  ;; two equal literals, two entries
  (is (equalp #("a" "a")
              (nth-value 1 (cq-canon '(?x ?y) '((= ?x "a") (= ?y "a"))))))
  ;; the aggregate spec: variables renamed, nothing lifted; its goal walked
  (multiple-value-bind (s l)
      (cq-canon '(?t) '((aggregate-all (sum ?r)
                                       (node-slot-value ?i rank ?r) ?t)
                        (aggregate-all count
                                       (node-slot-value ?j rank 2) ?n)))
    ;; written order: the spec's ?r is seen before the goal's ?i
    (is (equal '(aggregate-all (sum graph-db::?v1)
                               (node-slot-value graph-db::?v2 rank
                                                graph-db::?v1)
                               graph-db::?v0)
               (second s)))
    (is (equalp #(2) l)))
  ;; a lisp expression: not a term position
  (is (equalp #() (nth-value 1 (cq-canon '(?x) '((lisp ?x (+ 1 2)))))))
  ;; no literal, no variable beyond the projection
  (is (equalp #() (nth-value 1 (cq-canon '(?x) '((is-a ?x qt-item)))))))

(test cq-form-binds-literals
  "The cached form binds the literals first, through %bind-literals,
and reads its options from specials; a literal-free shape has no
binding goal (design D1, D2)."
  (let ((form (graph-db::%cached-query-form
               (cq-shape '(?s) '((find-by-slot ?s qt-item label "a")
                                 (node-slot-value ?s rank 2)))
               2)))
    (is (eq 'graph-db:select (first form)))
    (is (equal '(graph-db::%bind-literals graph-db::?l0 graph-db::?l1)
               (fourth form)))
    (is (equal '(find-by-slot graph-db::?v0 qt-item label graph-db::?l0)
               (fifth form)))
    (is (eq 'graph-db::*query-run-limit* (getf (second form) :limit)))
    (is (eq 'graph-db::*query-default-max-inferences*
            (getf (second form) :max-inferences))))
  (let ((form (graph-db::%cached-query-form
               (cq-shape '(?x) '((is-a ?x qt-item))) 0)))
    (is (equal '(is-a graph-db::?v0 qt-item) (fourth form)))))

(test cq-interpreted-canonical-rows
  "The runner's rows are unchanged by canonicalisation: a battery over
the fixture through RUN-GUARDED-PROLOG."
  (with-query-graph (g)
    (seed g)
    (flet ((rows (text) (nth-value 1 (q g text :format :raw))))
      (is (equal '((2))
                 (mapcar #'last (rows "(is-a ?i qt-item)
                                       (node-slot-value ?i label \"b\")
                                       (node-slot-value ?i rank ?r)"))))
      (is (= 2 (length (rows "(is-a ?i qt-item) (node-slot-value ?i rank ?r)
                               (> ?r 1)"))))
      (is (equal '(6)
                 (mapcar (lambda (r) (first (last r)))
                         (rows "(aggregate-all (sum ?r)
                                  (node-slot-value ?i rank ?r) ?t)"))))
      (is (= 1 (length (rows "(qt-links ?a ?b) (qt-links ?b ?c)"))))
      (is (= 1 (length (rows "(is-a ?i qt-item) (node-slot-value ?i label ?l)
                               (= ?l \"c\")"))))
      (is (= 2 (length (rows "(is-a ?i qt-item) (node-slot-value ?i label ?l)
                               (not (= ?l \"c\"))")))))))

;;; The cache (design D3 to D5).

(defun cq-stat (key) (getf (graph-db:query-cache-stats) key))

(defun cq-data (g text &rest keys)
  "The :DATA rows of TEXT (nodes as id strings), so rows compare EQUAL
across calls."
  (nth-value 1 (apply #'q g text keys)))

(defparameter *cq-rank-text*
  "(is-a ?i qt-item) (node-slot-value ?i label \"~A\")
   (node-slot-value ?i rank ?r)")

(test cq-second-hit-compiles
  "One shape, three keys: one entry, compiled on the second hit, the
right rows every time (design D3)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((rows (loop for key in '("a" "b" "c")
                      collect (cq-data g (format nil *cq-rank-text* key)))))
      (is (equal '(1 2 3)
                 (mapcar (lambda (r) (first (last (first r)))) rows)))
      (is (= 1 (cq-stat :entries)))
      (is (= 1 (cq-stat :compiled)))
      (is (= 1 (cq-stat :compiles)))
      (is (= 1 (cq-stat :misses)))
      (is (= 2 (cq-stat :hits))))))

(defparameter *cq-battery*
  '("(is-a ?i qt-item) (node-slot-value ?i label ?l) (= ?l \"b\")"
    "(is-a ?i qt-item) (node-slot-value ?i rank ?r) (> ?r 1)"
    "(qt-links ?a ?b) (qt-links ?b ?c)"
    "(aggregate-all count (node-slot-value ?i rank 2) ?n)"
    "(aggregate-all (sum ?r) (node-slot-value ?i rank ?r) ?t)"
    "(is-a ?i qt-item) (not (node-slot-value ?i label \"a\"))"
    "(is-a ?a qt-item) (k-hop ?a 1 ?x qt-links)"
    "(once (is-a ?i qt-item))"
    "(is-a ?i qt-item) (node-slot-value ?i rank ?r) (= ?r 3)"))

(test cq-same-rows-either-way
  "A battery of shapes: the first (interpreted) and third (compiled)
calls return EQUAL rows; a literal of another type in the same position
is the same shape and its own answer (design D4, Review Focus 1)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (dolist (text *cq-battery*)
      (let ((first (cq-data g text)))
        (cq-data g text)
        (is (equal first (cq-data g text)) "~A" text)))
    (is (= (length *cq-battery*) (cq-stat :compiled)))
    ;; Review Focus 1: 3 then "3" then 3 in one shape
    (graph-db:clear-query-cache)
    (is (= 1 (length (cq-data g "(is-a ?i qt-item)
                                  (node-slot-value ?i rank 3)"))))
    (is (= 0 (length (cq-data g "(is-a ?i qt-item)
                                  (node-slot-value ?i rank \"3\")"))))
    (is (= 1 (length (cq-data g "(is-a ?i qt-item)
                                  (node-slot-value ?i rank 3)"))))
    (is (= 1 (cq-stat :entries)))))

(test cq-options-are-per-call
  "Limit and offset are read at run time: one entry serves every page
(design D2)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((text "(is-a ?i qt-item) (node-slot-value ?i rank ?r)"))
      (is (= 1 (length (cq-data g text :limit 1))))
      (is (= 3 (length (cq-data g text :limit 3))))
      (is (= 2 (length (cq-data g text :offset 1))))
      (is (= 1 (cq-stat :entries))))))

(test cq-budget-trips-compiled
  "The budget trips inside a compiled run; the entry survives and the
next run under the normal budget answers (design D4, Review Focus 3)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((text "(is-a ?i qt-item) (node-slot-value ?i rank ?r) (> ?r 0)"))
      (cq-data g text) (cq-data g text)
      (is (= 1 (cq-stat :compiled)))
      (signals graph-db:prolog-resource-error
        (cq-data g text :max-inferences 2))
      (is (= 3 (length (cq-data g text)))))))

(test cq-effects-nil-holds-compiled
  "An effect is refused on the compiled path as on the interpreted one."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (flet ((cq-run-effect () (graph-db::run-query-goals
                    '(?x) '((lisp ?x 1)) g :format :raw
                    :callback (lambda (row) (declare (ignore row)) nil))))
      (dotimes (i 3)
        (signals graph-db:prolog-permission-error (cq-run-effect))))))

(test cq-generation-moves
  "A user functor's content moving leaves the old entry behind; a
query root, an empty functor's delete and set-functor-fn do not move
it (design D3, Review Focus 2)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((*package* (find-package '#:graph-db/query-test)))
      (unwind-protect
           (progn
             (graph-db::<- (cq-f 1))
             (is (equal '((1)) (cq-data g "(cq-f ?x)")))
             (is (equal '((1)) (cq-data g "(cq-f ?x)")))
             (is (= 1 (cq-stat :compiled)))
             (graph-db::<- (cq-f 2))
             (is (equal '((1) (2)) (cq-data g "(cq-f ?x)")))
             (is (= 2 (cq-stat :entries)))
             (let ((f (graph-db::lookup-functor
                       (graph-db:make-functor-symbol 'cq-f 1))))
               (flet ((moves (thunk)
                        (let ((before graph-db::*functor-generation*))
                          (funcall thunk)
                          (/= before graph-db::*functor-generation*))))
                 (is-false (moves (lambda () (cq-data g "(cq-f ?x)"))))
                 (is-false (moves (lambda ()
                                    (graph-db:select () (?x) (cq-f ?x)))))
                 (is-false (moves (lambda ()
                                    (graph-db:set-functor-fn
                                     (graph-db::functor-name f)
                                     (graph-db::functor-fn f)))))
                 (is-true (moves (lambda ()
                                   (graph-db:table-functor 'cq-f 1))))
                 (is-true (moves (lambda ()
                                   (graph-db:untable-functor 'cq-f 1))))
                 (is-true (moves (lambda () (graph-db::reset-functor f))))
                 (is-true (moves (lambda ()
                                   (graph-db::%install-edge-functors
                                    'graph-db/query-test.schema::qt-links))))
                 (is-false (moves (lambda () (graph-db::delete-functor f))))
                 (graph-db::<- (cq-f 3))
                 (is-true (moves (lambda ()
                                   (graph-db::delete-functor
                                    (graph-db::lookup-functor
                                     (graph-db:make-functor-symbol
                                      'cq-f 1)))))))))
        (let ((f (graph-db::lookup-functor
                  (graph-db:make-functor-symbol 'cq-f 1))))
          (when f (graph-db::delete-functor f)))))))

(test cq-bounded-and-lru
  "The table is bounded; the least recently used entry goes first."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((graph-db:*compiled-query-cache-size* 3)
          (a "(is-a ?i qt-item)")
          (b "(is-a ?i qt-item) (node-slot-value ?i rank ?r)")
          (c "(qt-links ?x ?y)")
          (d "(is-a ?i qt-item) (node-slot-value ?i label ?l)"))
      (cq-data g a) (cq-data g b) (cq-data g c)
      (cq-data g b)                     ; b is now the most recent
      (cq-data g d)                     ; a goes
      (is (= 3 (cq-stat :entries)))
      (is (= 1 (cq-stat :evictions)))
      (let ((misses (cq-stat :misses)))
        (cq-data g b)
        (is (= misses (cq-stat :misses)))
        (cq-data g a)
        (is (= (1+ misses) (cq-stat :misses)))))))

(test cq-two-threads-one-shape
  "Two threads, one shape, ten calls each: right rows, one compile."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (flet ((worker ()
             (let ((graph-db:*graph* g))
               (handler-case
                   (loop repeat 10
                         collect (length (cq-data g "(is-a ?i qt-item)
                                       (node-slot-value ?i rank ?r)")))
                 (error (e) e)))))
      (let* ((t1 (bt:make-thread #'worker :name "cq-1"))
             (t2 (bt:make-thread #'worker :name "cq-2"))
             (r1 (bt:join-thread t1))
             (r2 (bt:join-thread t2)))
        (is (equal (make-list 10 :initial-element 3) r1))
        (is (equal (make-list 10 :initial-element 3) r2))
        (is (= 1 (cq-stat :compiles)))))))

(test cq-clear
  (with-query-graph (g)
    (seed g)
    (cq-data g "(is-a ?i qt-item)")
    (graph-db:clear-query-cache)
    (is (= 0 (cq-stat :entries)))
    (is (= 0 (cq-stat :hits)))))

(test cq-param-still-works
  "*query-params* and param/2 are untouched (builder check 2)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((graph-db:*query-params* '(("k" . 1))))
      (is (equal '((1)) (cq-data g "(param ?x \"k\")")))
      (cq-data g "(param ?x \"k\")")
      (is (equal '((1)) (cq-data g "(param ?x \"k\")"))))))

(test cq-no-extra-tick
  "The literal binding costs no inference: the smallest budget the
pre-unit form fits in (a select from Lisp with the literals in place),
the compiled run fits in too (design D1; review finding 6)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let* ((text "(is-a ?i qt-item) (node-slot-value ?i label \"b\")
                  (node-slot-value ?i rank 2)")
           (rows 0)
           (item 'graph-db/query-test.schema::qt-item)
           (least
             (loop for n from 1 to 200
                   when (handler-case
                            (progn
                              ;; with :callback, select returns the row count
                              (setf rows
                                    (let ((*package*
                                            (find-package
                                             '#:graph-db/query-test)))
                                      (eval `(graph-db:select
                                                 (:effects nil :snapshot t
                                                  :max-inferences ,n :limit 10
                                                  :callback
                                                  (lambda (row)
                                                    (declare (ignore row))))
                                                 (?i)
                                               (is-a ?i ,item)
                                               (node-slot-value ?i label "b")
                                               (node-slot-value ?i rank 2)))))
                              t)
                          (graph-db:prolog-resource-error () nil))
                     return n)))
      (is (integerp least))
      (is (= 1 rows))
      (cq-data g text) (cq-data g text)
      (is (= 1 (cq-stat :compiled)))
      (finishes (cq-data g text :max-inferences least)))))

;;; The whole-branch review's findings (2026-11-10).

(test cq-node-literals-are-lifted
  "Any non-symbol atom is a literal: a node in a = binder (the
productions path) keys one shape, not one per node (review finding 2)."
  (with-query-graph (g)
    (let ((items (seed g)))
      (graph-db:clear-query-cache)
      (flet ((cq-run-node (node)
               (let ((rows '()))
                 (graph-db::run-query-goals
                  '(?l) `((= ?i ,node) (node-slot-value ?i label ?l)) g
                  :format :raw :callback (lambda (row) (push row rows)))
                 rows)))
        (is (equal '(("a")) (cq-run-node (first items))))
        (is (equal '(("b")) (cq-run-node (second items))))
        (is (equal '(("c")) (cq-run-node (third items))))
        (is (= 1 (cq-stat :entries)))
        (is (= 1 (cq-stat :compiled)))))))

(test cq-anonymous-variable-caches
  "A free-text ? is renamed like any variable, so the shape repeats
across requests; graph-db's own ? stays ? (review finding 3)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    ;; a free-text ? is a named variable shared across its occurrences
    ;; (the guard's rule), so both links answer
    (is (= 2 (length (cq-data g "(qt-links ?a ?)"))))
    (is (= 2 (length (cq-data g "(qt-links ?a ?)"))))
    (is (= 1 (cq-stat :entries)))
    (is (= 1 (cq-stat :compiled)))
    (is (equal '((graph-db::?v0) (qt-links graph-db::?v0 graph-db::?))
               (cq-shape '(?x) '((qt-links ?x graph-db::?)))))))

(test cq-projection-literal-comes-back
  "A literal in the projection is returned as itself (review finding
4)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((rows '()))
      ;; from Lisp the type is the schema package's symbol (the guard
      ;; canonicalises text to it)
      (graph-db::run-query-goals
       '(?i "tag") '((is-a ?i graph-db/query-test.schema::qt-item)) g
       :format :raw :callback (lambda (row) (push row rows)))
      (is (= 3 (length rows)))
      (is (every (lambda (row) (equal "tag" (second row))) rows)))))

(test cq-failed-compile-keeps-the-error
  "A form the compiler refuses stays interpreted: the same PROLOG-ERROR
on every call, never a program error (review finding 5)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (flet ((formal ()
             (handler-case
                 (progn (graph-db::run-query-goals
                         '(?n) '((aggregate-all (foo ?x)
                                  (is-a ?x graph-db/query-test.schema::qt-item)
                                  ?n))
                         g :format :raw
                         :callback (lambda (row) (declare (ignore row))))
                        :no-error)
               (graph-db:prolog-error (e)
                 (first (second (graph-db:prolog-error-ball e))))
               (error (e) (type-of e)))))
      (is (eq :domain-error (formal)))
      (is (eq :domain-error (formal)))
      (is (eq :domain-error (formal)))
      (is (= 0 (cq-stat :compiled))))))

(test cq-size-zero-disables
  "A cache size of zero turns the cache off: every call interpreted,
nothing stored, no hang (review finding 1)."
  (with-query-graph (g)
    (seed g)
    (graph-db:clear-query-cache)
    (let ((graph-db:*compiled-query-cache-size* 0))
      (is (= 3 (length (cq-data g "(is-a ?i qt-item)"))))
      (is (= 3 (length (cq-data g "(is-a ?i qt-item)"))))
      (is (= 0 (cq-stat :entries))))))
