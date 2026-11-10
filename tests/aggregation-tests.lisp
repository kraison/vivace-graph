;;;; Tests for aggregate-all/3 (GH #123, design
;;;; docs/superpowers/specs/2026-11-10-aggregation-graph-predicates-design.md
;;;; D1, D5).  Every name here is prefixed AG-.

(in-package #:graph-db/test)

(def-suite aggregation-suite
  :description "aggregate-all/3: static and dynamic forms."
  :in graph-db-suite)

(in-suite aggregation-suite)

(defun ag-facts ()
  "ag-v 3 1 2; ag-w (a 3) (b 1) (c 3)."
  (<- (ag-v 3)) (<- (ag-v 1)) (<- (ag-v 2))
  (<- (ag-w a 3)) (<- (ag-w b 1)) (<- (ag-w c 3)))

(test ag-count-sum-min-max
  "count, sum, min, max over three facts, and over no solution: count
and sum 0, bag and set nil, min fails (design D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((3)) (select () (?n) (aggregate-all count (ag-v ?x) ?n))))
      (is (equal '((6)) (select () (?n) (aggregate-all (sum ?x) (ag-v ?x) ?n))))
      (is (equal '((1)) (select () (?n) (aggregate-all (min ?x) (ag-v ?x) ?n))))
      (is (equal '((3)) (select () (?n) (aggregate-all (max ?x) (ag-v ?x) ?n))))
      (is (equal '((0)) (select () (?n) (aggregate-all count (ag-v 9) ?n))))
      (is (equal '((0)) (select () (?n) (aggregate-all (sum ?x) (ag-v 9) ?n))))
      (is (equal '((nil))
                 (select () (?n) (aggregate-all (bag ?x) (ag-v 9) ?n))))
      (is (equal '((nil))
                 (select () (?n) (aggregate-all (set ?x) (ag-v 9) ?n))))
      (is (null (select () (?n) (aggregate-all (min ?x) (ag-v 9) ?n)))))))

(test ag-bag-and-set
  "bag keeps solution order and duplicates; set sorts and drops them."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v2 1))
      (<- (ag-v2 3)) (<- (ag-v2 1)) (<- (ag-v2 2)) (<- (ag-v2 1))
      (is (equal '(((3 1 2 1)))
                 (select () (?l) (aggregate-all (bag ?x) (ag-v2 ?x) ?l))))
      (is (equal '(((1 2 3)))
                 (select () (?l) (aggregate-all (set ?x) (ag-v2 ?x) ?l)))))))

(test ag-witness-forms
  "(max ?e ?w) and (min ?e ?w) bind (E W), the first extreme on ties."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '(((3 a)))
                 (select () (?r) (aggregate-all (max ?s ?n) (ag-w ?n ?s) ?r))))
      (is (equal '(((1 b)))
                 (select () (?r)
                   (aggregate-all (min ?s ?n) (ag-w ?n ?s) ?r)))))))

(test ag-leaves-no-bindings
  "The goal's variables are unbound after the aggregate; the body sees
the result."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (let ((rows (select () (?x ?n) (aggregate-all count (ag-v ?x) ?n))))
        (is (= 1 (length rows)))
        (is (graph-db::var-p (graph-db::var-deref (first (first rows)))))
        (is (= 3 (second (first rows)))))
      (is (equal '((yes))
                 (select () (?y)
                   (aggregate-all count (ag-v ?x) ?n) (> ?n 2) (= ?y yes)))))))

(test ag-non-number-signals
  "(sum ?e) over a non-number signals PROLOG-ERROR with a type-error
ball, which catch/3 can take."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-s 1))
      (<- (ag-s foo))
      (is (eq :type-error
              (handler-case
                  (progn (select () (?n) (aggregate-all (sum ?x) (ag-s ?x) ?n))
                         :no-error)
                (graph-db:prolog-error (e)
                  (first (second (graph-db:prolog-error-ball e)))))))
      (is (equal '((caught))
                 (select () (?r)
                   (catch (aggregate-all (sum ?x) (ag-s ?x) ?n)
                          ?ball
                          (= ?r caught))))))))

(test ag-template-bound-inside-the-goal
  "A compile-time alias made by = inside the goal reaches the template
(the builder check's program; design D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((((3 1) (1 1) (2 1))))
                 (select () (?l)
                   (aggregate-all (bag (?x ?y)) (and (ag-v ?x) (= ?y 1)) ?l))))
      (is (equal '((6))
                 (select () (?t)
                   (aggregate-all (sum ?s) (and (ag-v ?x) (= ?s ?x)) ?t)))))))

(test ag-spec-words-match-by-name
  "Spec words are matched by name from any package; an unknown spec
signals PROLOG-ERROR when the query compiles."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((3))
                 (select () (?n) (aggregate-all cl:count (ag-v ?x) ?n))))
      (is (equal '((1))
                 (select () (?n) (aggregate-all (cl:min ?x) (ag-v ?x) ?n))))
      (is (equal '(((1 2 3)))
                 (select () (?n) (aggregate-all (cl:set ?x) (ag-v ?x) ?n))))
      (is (equal '(((3 1 2)))
                 (select () (?n)
                   (aggregate-all (graph-db::bag ?x) (ag-v ?x) ?n))))
      (is (eq :domain-error
              (handler-case
                  (let ((*package* (find-package '#:graph-db/test)))
                    (eval '(select () (?n)
                             (aggregate-all (nonsense ?x) (ag-v ?x) ?n)))
                    :no-error)
                (graph-db:prolog-error (e)
                  (first (second (graph-db:prolog-error-ball e))))))))))

(test ag-dynamic-goal
  "A goal in a variable reaches the runtime functor; an unknown spec
there signals at run time."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((3))
                 (select () (?n) (= ?g (ag-v ?x)) (aggregate-all count ?g ?n))))
      (is (equal '((6))
                 (select () (?n)
                   (= ?g (ag-v ?x)) (aggregate-all (sum ?x) ?g ?n))))
      (is (eq :domain-error
              (handler-case
                  (progn (select () (?n)
                           (= ?s (nonsense ?x)) (aggregate-all ?s (ag-v ?x) ?n))
                         :no-error)
                (graph-db:prolog-error (e)
                  (first (second (graph-db:prolog-error-ball e))))))))))

(test ag-static-form-compiles-inline
  "The static form fires no :call for AGGREGATE-ALL/3; the dynamic form
fires one (design D1, D7)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (flet ((calls (thunk)
               (let ((n 0))
                 (graph-db:with-prolog-ports ((port f args depth)
                               (declare (ignore args depth))
                               (when (and (eq port :call)
                                          (string= "AGGREGATE-ALL/3"
                                                   (symbol-name f)))
                                 (incf n)))
                   (funcall thunk))
                 n)))
        (is (= 0 (calls (lambda ()
                          (select () (?n)
                            (aggregate-all count (ag-v ?x) ?n))))))
        (is (= 1 (calls (lambda ()
                          (select () (?n)
                            (= ?g (ag-v ?x))
                            (aggregate-all count ?g ?n))))))))))

(test ag-under-effects-nil
  "A static aggregate is a read: it runs under :effects nil."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((3))
                 (select (:effects nil) (?n)
                   (aggregate-all count (ag-v ?x) ?n)))))))

(test ag-reads-tables-as-closed
  "An aggregate over the predicate's own recursive component is
refused; over an independent tabled predicate it works (design D5)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-q 1) (ag-p 1) (ag-ind 1) (ag-r 1))
      (<- (ag-q a)) (<- (ag-q b))
      (<- (ag-p ?x) (ag-q ?x) (aggregate-all count (ag-p ?y) ?n) (> ?n 0))
      (graph-db:table-functor 'ag-p 1)
      (signals graph-db:prolog-unstratified-error
        (select (:max-inferences 5000) (?x) (ag-p ?x)))
      (<- (ag-ind 1)) (<- (ag-ind 2))
      (<- (ag-r ?x) (ag-q ?x) (aggregate-all count (ag-ind ?y) ?n) (> ?n 1))
      (<- (ag-r ?x) (ag-r ?x))
      (graph-db:table-functor 'ag-ind 1)
      (graph-db:table-functor 'ag-r 1)
      (is (equal '((a) (b)) (select (:max-inferences 5000) (?x) (ag-r ?x)))))))

(test ag-cut-inside-the-goal
  "A cut inside the goal is local to it: count 1, and the clause's
other clauses are still tried (Review Focus 1)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2) (ag-c 1))
      (ag-facts)
      (<- (ag-c ?n) (aggregate-all count (and (ag-v ?x) !) ?n))
      (<- (ag-c 99))
      (is (equal '((1) (99)) (select () (?n) (ag-c ?n)))))))

(test ag-nests
  "An aggregate inside not inside an aggregate: accumulators and floors
are per expansion (Review Focus 2)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-dropped-functors ((ag-v 1) (ag-w 2))
      (ag-facts)
      (is (equal '((3))
                 (select () (?n)
                   (aggregate-all count
                                  (and (ag-v ?x)
                                       (not (aggregate-all count (ag-v ?y) 0)))
                                  ?n)))))))
