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
