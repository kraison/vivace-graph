;;;; Tests for asserted clauses: one closure per clause, the cut inside
;;;; a clause, and first-argument indexing (GH #121, design
;;;; docs/superpowers/specs/2026-11-09-clause-indexing-design.md).
;;;;
;;;; Asserted functors are image-global, so every test drops what it
;;;; asserts before and after (CI-WITH-FUNCTORS), as DEFINING-REACH does
;;;; in prolog-stress-tests.lisp.  Every name here is prefixed CI-.

(in-package #:graph-db/test)

(def-suite clause-indexing-suite
  :description "Asserted clauses: per-clause compile, cut, first-arg index."
  :in graph-db-suite)

(in-suite clause-indexing-suite)

(defun ci-drop (name arity)
  "Remove the user functor NAME/ARITY if it is registered."
  (let ((f (lookup-functor (make-functor-symbol name arity))))
    (when f (delete-functor f))))

(defmacro ci-with-functors ((&rest name-arities) &body body)
  "BODY with each (NAME ARITY) dropped before and after."
  `(progn
     ,@(loop for (n a) in name-arities collect `(ci-drop ',n ,a))
     (unwind-protect (progn ,@body)
       ,@(loop for (n a) in name-arities collect `(ci-drop ',n ,a)))))

(test ci-an-assert-compiles-one-clause
  "Each <- compiles its own clause and nothing else (design D1)."
  (ci-with-functors ((ci-fact 2))
    (let ((before graph-db::*clause-compile-count*))
      (<- (ci-fact a 1))
      (<- (ci-fact b 2))
      (<- (ci-fact c 3))
      (is (= 3 (- graph-db::*clause-compile-count* before))))
    (is (equal '(((ci-fact a 1)) ((ci-fact b 2)) ((ci-fact c 3)))
               (graph-db::functor-clauses
                (lookup-functor (make-functor-symbol 'ci-fact 2)))))))

(test ci-a-cut-in-an-asserted-clause-commits-the-clause
  "ci-h: no cut, four solutions.  ci-f: a cut, one.  ci-o: a cut under
once is opaque, so clause two is reached.  ci-i: a cut in an if's then
branch cuts the clause.  ci-i2: a cut inside the if's test is under the
barrier, so clause two is reached.  Through a dynamic meta-call the
clause's own cut still commits the clause (design D3)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-g 1) (ci-h 1) (ci-f 1) (ci-o 1) (ci-i 1) (ci-i2 1))
      (<- (ci-g 1)) (<- (ci-g 2)) (<- (ci-g 3))
      (<- (ci-h ?x) (ci-g ?x))
      (<- (ci-h z))
      (<- (ci-f ?x) (ci-g ?x) !)
      (<- (ci-f z))
      (<- (ci-o ?x) (once (and (ci-g ?x) !)))
      (<- (ci-o z))
      (<- (ci-i ?x) (if (ci-g ?x) (and (> ?x 0) !) (= ?x none)))
      (<- (ci-i z))
      (<- (ci-i2 ?x) (if (and (ci-g ?x) !) (> ?x 0)))
      (<- (ci-i2 z))
      (is (equal '(1 2 3 z) (select-flat (?x) (ci-h ?x))))
      (is (equal '(1)       (select-flat (?x) (ci-f ?x))))
      (is (equal '(1 z)     (select-flat (?x) (ci-o ?x))))
      (is (equal '(1)       (select-flat (?x) (ci-i ?x))))
      (is (equal '(1 z)     (select-flat (?x) (ci-i2 ?x))))
      (is (equal '(1) (select-flat (?x) (= ?g (ci-f ?x)) (call ?g)))))))

(test ci-an-assert-after-set-functor-fn-wins
  "A function set by SET-FUNCTOR-FN answers until the next assert, which
reinstalls the dispatcher (design D2)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-s 1))
      (<- (ci-s a))
      (graph-db:set-functor-fn (make-functor-symbol 'ci-s 1)
                      (lambda (x cont) (declare (ignore x cont)) nil))
      (is (null (select-flat (?x) (ci-s ?x))))
      (<- (ci-s b))
      (is (equal '(a b) (select-flat (?x) (ci-s ?x)))))))

(test ci-reset-functor-empties-and-an-arity-mismatch-signals
  "RESET-FUNCTOR leaves no clause and no candidate; a clause of another
arity is a PROLOG-ERROR, through ADD-FUNCTOR-CLAUSE and through
MAKE-FUNCTOR :CLAUSES, which leaves no half-built functor (design D6)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-r 1) (ci-m 1))
      (<- (ci-r a)) (<- (ci-r b))
      (let ((f (lookup-functor (make-functor-symbol 'ci-r 1))))
        (signals graph-db:prolog-error
          (graph-db::add-functor-clause f '((ci-r a b))))
        (is (= 2 (length (graph-db::functor-clauses f))))
        (graph-db::reset-functor f)
        (is (null (graph-db::functor-clauses f)))
        (is (null (select-flat (?x) (ci-r ?x))))
        (<- (ci-r c))
        (is (equal '(c) (select-flat (?x) (ci-r ?x)))))
      (signals graph-db:prolog-error
        (graph-db:make-functor :name (make-functor-symbol 'ci-m 1 :define t)
                      :clauses '(((ci-m a)) ((ci-m a b)))))
      (is (null (graph-db::functor-clauses
                 (lookup-functor (make-functor-symbol 'ci-m 1))))))))

;;; First-argument indexing (design D4).

(defparameter *ci-key* nil
  "The first argument CI-WITH-KEY passes through a (lisp ...) goal.")

(defun ci-with-key (key)
  "Solutions of ci-k for first argument KEY, through a (lisp ...) goal
so one compiled query serves every key."
  (let ((*ci-key* key))
    (select-flat (?r) (lisp ?k *ci-key*) (ci-k ?k ?r))))

(test ci-the-index-is-a-superset-of-prolog-equal
  "For every kind of first argument the indexed answer equals the
unindexed one, in order: a bucket may hold a clause that fails, never
miss one that matches.  1 finds the 1.0 clause, \"S\" the :s clause, a
non-string array every clause, a node only the variable-headed ones."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-k 2))
      (<- (ci-k a 1)) (<- (ci-k b 2)) (<- (ci-k "s" 3)) (<- (ci-k :s 4))
      (<- (ci-k 1 5)) (<- (ci-k 1.0 6)) (<- (ci-k #\c 7))
      (<- (ci-k (x . y) 8)) (<- (ci-k nil 9)) (<- (ci-k ?v 10))
      (<- (ci-k a 11)) (<- (ci-k ? 12))
      (let ((node (with-transaction () (make-g-person :name "ci-n"))))
        (loop for (key expected)
                in `((a (1 10 11 12)) (b (2 10 12)) ("s" (3 4 10 12))
                     ("S" (4 10 12)) (:s (3 4 10 12)) (1 (5 6 10 12))
                     (1.0 (5 6 10 12)) (#\c (7 10 12)) ((x . y) (8 10 12))
                     ((p q) (10 12)) (nil (9 10 12)) (,node (10 12))
                     (#(#\s) (3 10 12))
                     #+sbcl (,sb-ext:double-float-positive-infinity
                             (10 12)))
              do (is (equal expected (ci-with-key key)) "key ~S" key)
                 (is (equal expected
                            (let ((graph-db:*clause-indexing* nil))
                              (ci-with-key key)))
                     "key ~S, index off" key))))))

(test ci-a-bound-chain-keys-by-its-value
  "The key comes from the dereferenced value, through a chain of
bound variables."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-k2 2))
      (<- (ci-k2 a 1)) (<- (ci-k2 b 2))
      (is (equal '(1) (select-flat (?r) (= ?k ?j) (= ?j a) (ci-k2 ?k ?r)))))))

(test ci-solutions-keep-source-order-under-a-key
  "Keyed and variable-headed clauses interleave in source order."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((ci-k3 2))
      (<- (ci-k3 a 1)) (<- (ci-k3 ?v 2)) (<- (ci-k3 a 3)) (<- (ci-k3 ?w 4))
      (is (equal '(1 2 3 4) (select-flat (?r) (ci-k3 a ?r))))
      (is (equal '(2 4) (select-flat (?r) (ci-k3 zz ?r)))))))
