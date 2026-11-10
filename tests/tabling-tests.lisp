;;;; Tests for tabled predicates (GH #122, design
;;;; docs/superpowers/specs/2026-11-10-tabling-design.md).
;;;;
;;;; Asserted functors are image-global, so every test drops what it
;;;; declares or asserts before and after (TB-WITH-FUNCTORS), as the
;;;; clause-indexing tests do.  Every name here is prefixed TB-.

(in-package #:graph-db/test)

(def-suite tabling-suite
  :description "Tabled predicates: declaration, variant tables, rounds."
  :in graph-db-suite)

(in-suite tabling-suite)

(defun tb-drop (name arity)
  "Remove the user functor NAME/ARITY if it is registered."
  (let ((f (lookup-functor (make-functor-symbol name arity))))
    (when f (delete-functor f))))

(defmacro tb-with-functors ((&rest name-arities) &body body)
  "BODY with each (NAME ARITY) dropped before and after."
  `(progn
     ,@(loop for (n a) in name-arities collect `(tb-drop ',n ,a))
     (unwind-protect (progn ,@body)
       ,@(loop for (n a) in name-arities collect `(tb-drop ',n ,a)))))

(test tb-declaration-round-trips
  "TABLE-FUNCTOR before any clause, across asserts, across a reset,
and back through UNTABLE-FUNCTOR; a global functor is refused (design
D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-fact 2) (tb-fresh 1) (tb-fresh2 1))
      (graph-db:table-functor 'tb-fact 2)
      (is-true (graph-db:functor-tabled-p 'tb-fact 2))
      (is (null (select () (?x) (tb-fact a ?x))))
      (<- (tb-fact a 1))
      (<- (tb-fact a 2))
      (is (equal '((1) (2)) (select () (?x) (tb-fact a ?x))))
      (is-true (graph-db:functor-tabled-p 'tb-fact 2))
      (graph-db::reset-functor
       (lookup-functor (make-functor-symbol 'tb-fact 2)))
      (<- (tb-fact a 3))
      (is-true (graph-db:functor-tabled-p 'tb-fact 2))
      (is (equal '((3)) (select () (?x) (tb-fact a ?x))))
      (graph-db:untable-functor 'tb-fact 2)
      (is-false (graph-db:functor-tabled-p 'tb-fact 2))
      (is (equal '((3)) (select () (?x) (tb-fact a ?x))))
      (signals graph-db:prolog-error (graph-db:table-functor 'is-a 2))
      (is-false (graph-db:functor-tabled-p 'tb-never 1))
      ;; MAKE-FUNCTOR :CLAUSES after the declaration keeps it tabled;
      ;; the declaration after MAKE-FUNCTOR :CLAUSES tables the clauses.
      (graph-db:table-functor 'tb-fresh 1)
      (graph-db::make-functor :name (make-functor-symbol 'tb-fresh 1 :define t)
                    :clauses '(((tb-fresh 7))))
      (is-true (graph-db:functor-tabled-p 'tb-fresh 1))
      (graph-db::make-functor :name (make-functor-symbol 'tb-fresh2 1 :define t)
                    :clauses '(((tb-fresh2 8))))
      (graph-db:table-functor 'tb-fresh2 1)
      (is-true (graph-db:functor-tabled-p 'tb-fresh2 1))
      (is (equal '((8)) (select () (?x) (tb-fresh2 ?x)))))))

;;; Variant tables, components and rounds (design D2, D3).

(defun tb-ports (functor thunk)
  "The port events FUNCTOR (a NAME/ARITY string) fires while THUNK
runs, in order: a list of (PORT ARGS)."
  (let ((log '()))
    (graph-db:with-prolog-ports ((port f args depth)
                                 (declare (ignore depth))
                                 (when (string= functor (symbol-name f))
                                   (push (list port args) log)))
      (funcall thunk))
    (nreverse log)))

(defun tb-table-events (functor thunk &optional kind)
  "The :TABLE events for FUNCTOR while THUNK runs, as their argument
lists; with KIND, only those whose first element is KIND."
  (loop for (port args) in (tb-ports functor thunk)
        when (and (eq port :table)
                  (or (null kind) (eq (first args) kind)))
          collect args))

(defun tb-set-equal (a b)
  "True when the lists A and B hold the same rows (EQUAL), any order."
  (and (= (length a) (length b))
       (null (set-difference a b :test #'equal))
       (null (set-difference b a :test #'equal))))

(defun tb-ring-facts ()
  "tb-edge: the ring a b c d e f a."
  (<- (tb-edge a b)) (<- (tb-edge b c)) (<- (tb-edge c d))
  (<- (tb-edge d e)) (<- (tb-edge e f)) (<- (tb-edge f a)))

(defun tb-right-path ()
  "tb-path: right-recursive closure over tb-edge."
  (<- (tb-path ?x ?y) (tb-edge ?x ?y))
  (<- (tb-path ?x ?z) (tb-edge ?x ?y) (tb-path ?y ?z)))

(defparameter *tb-ring* '((a) (b) (c) (d) (e) (f)))

(test tb-ring-terminates
  "Untabled, the right-recursive ring runs until the budget; tabled,
the same select returns each node once (design D3)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-edge 2) (tb-path 2))
      (tb-ring-facts)
      (tb-right-path)
      (signals graph-db:prolog-resource-error
        (select (:max-inferences 5000) (?x) (tb-path a ?x)))
      (graph-db:table-functor 'tb-path 2)
      (let ((rows (select (:max-inferences 5000) (?x) (tb-path a ?x))))
        (is (= 6 (length rows)))
        (is (tb-set-equal *tb-ring* rows))))))

(test tb-left-and-right-recursion-agree
  "A left-recursive rule is a looping consumer of its own table and
gives the same closure; the open call has every pair (design D3, D8)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-edge 2) (tb-path 2) (tb-lpath 2))
      (tb-ring-facts)
      (tb-right-path)
      (<- (tb-lpath ?x ?y) (tb-edge ?x ?y))
      (<- (tb-lpath ?x ?z) (tb-lpath ?x ?y) (tb-edge ?y ?z))
      (graph-db:table-functor 'tb-path 2)
      (graph-db:table-functor 'tb-lpath 2)
      (let ((right (select (:max-inferences 5000) (?x) (tb-path a ?x)))
            (left (select (:max-inferences 5000) (?x) (tb-lpath a ?x))))
        (is (= 6 (length left)))
        (is (tb-set-equal right left)))
      (is (= 36 (length (select (:max-inferences 20000) (?x ?y)
                          (tb-path ?x ?y)))))
      (is (= 36 (length (select (:max-inferences 20000) (?x ?y)
                          (tb-lpath ?x ?y))))))))

(defun tb-succ-facts (n)
  "tb-succ: i -> i+1 for i below N."
  (dotimes (i n)
    (let ((graph-db::*functor* nil))
      (graph-db::add-clause (list (list 'tb-succ i (1+ i)))))))

(test tb-mutual-recursion
  "tb-even and tb-odd call each other through open calls, so the two
tables form one component with one leader (design D3)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-succ 2) (tb-even 1) (tb-odd 1))
      (tb-succ-facts 10)
      (<- (tb-even 0))
      (<- (tb-even ?n) (tb-odd ?m) (tb-succ ?m ?n))
      (<- (tb-odd ?n) (tb-even ?m) (tb-succ ?m ?n))
      (graph-db:table-functor 'tb-even 1)
      (graph-db:table-functor 'tb-odd 1)
      (is (tb-set-equal '((0) (2) (4) (6) (8) (10))
                        (select (:max-inferences 20000) (?n)
                          (tb-even ?n))))
      (is (tb-set-equal '((1) (3) (5) (7) (9))
                        (select (:max-inferences 20000) (?n)
                          (tb-odd ?n))))
      ;; task 6: the :table port
      (skip "task 6 fires the :table port"))))

(test tb-independent-subtable-completes-early
  "A tabled fact predicate called inside the ring's evaluation completes
on its first evaluation and is consumed afterwards (design D3)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-edge 2) (tb-path 2) (tb-q 1))
      (tb-ring-facts)
      (<- (tb-q a))
      (<- (tb-path ?x ?y) (tb-edge ?x ?y) (tb-q a))
      (<- (tb-path ?x ?z) (tb-edge ?x ?y) (tb-q a) (tb-path ?y ?z))
      (graph-db:table-functor 'tb-path 2)
      (graph-db:table-functor 'tb-q 1)
      (is (tb-set-equal *tb-ring*
                        (select (:max-inferences 5000) (?x)
                          (tb-path a ?x))))
      ;; task 6: the :table port
      (skip "task 6 fires the :table port"))))

(defun tb-same-var-p (x y)
  "True when X and Y dereference to one unbound variable."
  (let ((x (graph-db::var-deref x)) (y (graph-db::var-deref y)))
    (and (graph-db::var-p x) (eq x y))))

(test tb-nonground-answers
  "A non-ground answer is stored once and copied per consumer; a
compound argument with a variable keys by structure (design D2)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-same 2))
      (<- (tb-same ?x ?x))
      (graph-db:table-functor 'tb-same 2)
      (let ((rows (select () (?a ?b) (tb-same ?a ?b))))
        (is (= 1 (length rows)))
        (is-true (tb-same-var-p (first (first rows))
                                (second (first rows)))))
      (is (equal '((1)) (select () (?a) (tb-same ?a 1))))
      (is (equal '((2)) (select () (?a) (tb-same (f ?a) (f 2)))))
      (let ((rows (select () (?p ?q ?r ?s)
                    (tb-same ?p ?q) (tb-same ?r ?s))))
        (is (= 1 (length rows)))
        (destructuring-bind (p q r s) (first rows)
          (is-true (tb-same-var-p p q))
          (is-true (tb-same-var-p r s))
          (is-false (tb-same-var-p p r)))))))

(test tb-answers-dedupe-by-variant
  "1 and 1.0 are one answer; a user cons shaped like a key marker is
not one; two lookups of one vertex are one answer (design D2)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-d 1) (tb-d-src 1) (tb-dn 1))
      (<- (tb-d 1))
      (<- (tb-d 1.0))
      (<- (tb-d ?x) (tb-d-src ?x))
      (<- (tb-d-src 1))
      (<- (tb-d-src (:var . 0)))
      (graph-db:table-functor 'tb-d 1)
      (is (equal '((1) ((:var . 0))) (select () (?x) (tb-d ?x))))
      (with-transaction ()
        (make-g-person :name "tb-one")
        (make-g-person :name "tb-two"))
      (<- (tb-dn ?n) (is-a ?n g-person))
      (<- (tb-dn ?n) (is-a ?n g-person))
      (let ((graph-db::*cache-enabled* nil))
        (is (= 4 (length (select () (?n) (tb-dn ?n)))))
        (graph-db:table-functor 'tb-dn 1)
        (is (= 2 (length (select () (?n) (tb-dn ?n)))))))))

(test tb-tables-scope-to-the-query
  "Tables live for one query: an assert between two selects is seen
by the second; inside one query a second call consumes (design D6)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-fact2 1))
      (<- (tb-fact2 1))
      (graph-db:table-functor 'tb-fact2 1)
      (is (equal '((1)) (select () (?x) (tb-fact2 ?x))))
      (<- (tb-fact2 2))
      (is (equal '((1) (2)) (select () (?x) (tb-fact2 ?x))))
      (is (= 4 (length (select () (?x ?y) (tb-fact2 ?x) (tb-fact2 ?y)))))
      ;; task 6: the :table port
      (skip "task 6 fires the :table port"))))

(test tb-tables-are-per-thread
  "Two threads run the ring select at once; each gets the closure."
  (with-test-graph (g)
    (tb-with-functors ((tb-edge 2) (tb-path 2))
      (tb-ring-facts)
      (tb-right-path)
      (graph-db:table-functor 'tb-path 2)
      (flet ((tb-run-ring ()
               (let ((*graph* g))
                 (handler-case
                     (select (:max-inferences 5000) (?x) (tb-path a ?x))
                   (error (e) e)))))
        (let* ((t1 (bt:make-thread #'tb-run-ring :name "tb-1"))
               (t2 (bt:make-thread #'tb-run-ring :name "tb-2"))
               (r1 (bt:join-thread t1))
               (r2 (bt:join-thread t2)))
          (is (tb-set-equal *tb-ring* r1))
          (is (tb-set-equal *tb-ring* r2)))))))

;;; Negation and stratification (design D4).

(test tb-negation-is-stratified-or-refused
  "not over a tabled predicate outside the component works; over the
predicate's own component it is refused, and catch/3 cannot take the
refusal; forall over an independent table works; a nested component
under a floor completes there (design D4)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-s 1) (tb-r 1) (tb-p 1) (tb-np 1) (tb-r2 1)
                       (tb-q3 1) (tb-s3 1) (tb-r3 1) (tb-p3 1)
                       (tb-cedge 2) (tb-walk 2) (tb-far 2))
      (<- (tb-s a)) (<- (tb-s b)) (<- (tb-s c))
      (<- (tb-r b))
      (<- (tb-p ?x) (tb-s ?x) (not (tb-r ?x)))
      (graph-db:table-functor 'tb-r 1)
      (is (tb-set-equal '((a) (c)) (select () (?x) (tb-p ?x))))
      (<- (tb-np ?x) (tb-s ?x) (not (tb-np ?x)))
      (graph-db:table-functor 'tb-np 1)
      (signals graph-db:prolog-unstratified-error
        (select () (?x) (tb-np ?x)))
      (signals graph-db:prolog-unstratified-error
        (select () (?x) (catch (tb-np ?x) ?b (= ?x caught))))
      (<- (tb-r2 ?x) (tb-s ?x))
      (graph-db:table-functor 'tb-r2 1)
      (is (equal '((yes))
                 (select () (?y) (forall (tb-s ?x) (tb-r2 ?x)) (= ?y yes))))
      ;; The coarse design refused this stratified program.
      (<- (tb-q3 a)) (<- (tb-q3 b))
      (<- (tb-s3 ?x) (tb-q3 ?x))
      (<- (tb-r3 ?x) (tb-s3 ?y) (= ?y ?x) (= ?x a))
      (<- (tb-p3 ?x) (tb-s3 ?x) (not (tb-r3 ?x)))
      (graph-db:table-functor 'tb-s3 1)
      (graph-db:table-functor 'tb-r3 1)
      (graph-db:table-functor 'tb-p3 1)
      (is (equal '((b)) (select () (?x) (tb-p3 ?x))))
      ;; A recursive table under not, inside another recursive one.
      (<- (tb-cedge a b)) (<- (tb-cedge b c))
      (<- (tb-walk ?x ?y) (tb-cedge ?x ?y))
      (<- (tb-walk ?x ?z) (tb-cedge ?x ?y) (tb-walk ?y ?z))
      (<- (tb-far ?x ?y) (tb-cedge ?x ?y) (not (tb-walk ?y ?x)))
      (<- (tb-far ?x ?z) (tb-far ?x ?y) (tb-far ?y ?z))
      (graph-db:table-functor 'tb-walk 2)
      (graph-db:table-functor 'tb-far 2)
      (is (tb-set-equal '((a b) (b c) (a c))
                        (select (:max-inferences 5000) (?x ?y)
                          (tb-far ?x ?y)))))))

(test tb-if-else-and-findall-read-as-closed
  "if with an else arm and findall read their goal's tables as closed:
refused inside the predicate's own component, fine over an independent
table; once and two-argument if only prune (design D4)."
  (with-test-graph (g)
    (declare (ignore g))
    (tb-with-functors ((tb-q 1) (tb-ip 1) (tb-fp 1) (tb-ind 1) (tb-ok 1)
                       (tb-ok2 1) (tb-ok3 1))
      (<- (tb-q a)) (<- (tb-q b))
      (<- (tb-ip ?x) (tb-q ?x) (if (tb-ip ?x) (fail) (true)))
      (graph-db:table-functor 'tb-ip 1)
      (signals graph-db:prolog-unstratified-error
        (select () (?x) (tb-ip ?x)))
      (<- (tb-fp a) (findall ?y (tb-fp ?y) nil))
      (graph-db:table-functor 'tb-fp 1)
      (signals graph-db:prolog-unstratified-error
        (select () (?x) (tb-fp ?x)))
      (<- (tb-ind a)) (<- (tb-ind b))
      (<- (tb-ok ?x) (tb-q ?x) (findall ?y (tb-ind ?y) ?l))
      (graph-db:table-functor 'tb-ind 1)
      (graph-db:table-functor 'tb-ok 1)
      (is (tb-set-equal '((a) (b)) (select () (?x) (tb-ok ?x))))
      (<- (tb-ok2 ?x) (tb-q ?x) (once (tb-ok2 ?x)))
      (graph-db:table-functor 'tb-ok2 1)
      (finishes (select () (?x) (tb-ok2 ?x)))
      (<- (tb-ok3 ?x) (tb-q ?x) (if (tb-ok3 ?x) (true)))
      (graph-db:table-functor 'tb-ok3 1)
      (finishes (select () (?x) (tb-ok3 ?x))))))
