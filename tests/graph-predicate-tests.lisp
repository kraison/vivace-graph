;;;; Tests for reachable/3 and k-hop/4 (GH #123, design
;;;; docs/superpowers/specs/2026-11-10-aggregation-graph-predicates-design.md
;;;; D2, D3).  Every name here is prefixed GP-.

(in-package #:graph-db/test)

(def-suite graph-predicate-suite
  :description "reachable/3 (tabled) and k-hop/4 over one edge type."
  :in graph-db-suite)

(in-suite graph-predicate-suite)

(defvar *gp-a* nil "A vertex a query reads through (lisp ...).")
(defvar *gp-b* nil "A second vertex a query reads through (lisp ...).")

(defun gp-persons (names)
  "One g-person per name, in one transaction; the list in order."
  (with-transaction ()
    (mapcar (lambda (n) (make-g-person :name n)) names)))

(defun gp-link (pairs)
  "A g-knows edge per (FROM . TO) pair."
  (with-transaction ()
    (dolist (p pairs) (make-g-knows :from (car p) :to (cdr p)))))

(defun gp-ring (persons)
  "g-knows edges around PERSONS in order, closing on the first."
  (gp-link (append (loop for (a b) on persons while b collect (cons a b))
                   (list (cons (car (last persons)) (first persons))))))

(defun gp-names (rows)
  "The sorted names of the persons in single-column ROWS."
  (sort (mapcar (lambda (r) (slot-value (first r) 'name)) rows) #'string<))

(defun gp-ball-formal (thunk)
  "The formal of the ball THUNK's PROLOG-ERROR carries, or :no-error."
  (handler-case (progn (funcall thunk) :no-error)
    (graph-db:prolog-error (e)
      (let ((ball (graph-db:prolog-error-ball e)))
        (if (consp (second ball)) (first (second ball)) (second ball))))))

(test gp-reachable-closure-on-a-cycle
  "From one node of a six-ring every node once; from every node 36
pairs; a bound target reachable once, an unreachable one never; no
edge, no row; a self-loop, itself (design D2, Review Focus 3)."
  (with-test-graph (g)
    (declare (ignore g))
    (let* ((ring (gp-persons '("a" "b" "c" "d" "e" "f")))
           (lone (first (gp-persons '("lone"))))
           (loop-p (first (gp-persons '("loop")))))
      (gp-ring ring)
      (gp-link (list (cons loop-p loop-p)))
      (let ((*gp-a* (first ring)))
        (let ((rows (select (:max-inferences 20000) (?x)
                      (lisp ?a *gp-a*) (reachable ?a ?x g-knows))))
          (is (= 6 (length rows)))
          (is (equal '("a" "b" "c" "d" "e" "f") (gp-names rows)))))
      ;; 36 ring pairs, plus the self-loop reaching itself.
      (is (= 37 (length (select (:max-inferences 200000) (?a ?b)
                          (is-a ?a g-person) (reachable ?a ?b g-knows)))))
      (let ((*gp-a* (first ring)) (*gp-b* (third ring)))
        (is (= 1 (length (select (:max-inferences 20000) (?r)
                           (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                           (reachable ?a ?b g-knows) (= ?r yes))))))
      (let ((*gp-a* (first ring)) (*gp-b* lone))
        (is (null (select (:max-inferences 20000) (?r)
                    (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                    (reachable ?a ?b g-knows) (= ?r yes)))))
      (let ((*gp-a* lone))
        (is (null (select (:max-inferences 20000) (?x)
                    (lisp ?a *gp-a*) (reachable ?a ?x g-knows)))))
      (let ((*gp-a* loop-p))
        (is (equal '("loop")
                   (gp-names (select (:max-inferences 20000) (?x)
                               (lisp ?a *gp-a*)
                               (reachable ?a ?x g-knows)))))))))

(test gp-reachable-needs-a-bound-start-and-type
  "An unbound start or edge type signals PROLOG-ERROR with an
instantiation ball, which catch/3 can take (design D2)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((ps (gp-persons '("p" "q"))))
      (gp-link (list (cons (first ps) (second ps))))
      (is (eq :instantiation-error
              (gp-ball-formal
               (lambda ()
                 (select (:max-inferences 5000) (?x)
                   (reachable ?a ?x g-knows))))))
      (let ((*gp-a* (first ps)) (*gp-b* (second ps)))
        (is (eq :instantiation-error
                (gp-ball-formal
                 (lambda ()
                   (select (:max-inferences 5000) (?t)
                     (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                     (reachable ?a ?b ?t))))))
        (is (equal '((caught))
                   (select (:max-inferences 5000) (?r)
                     (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                     (catch (reachable ?a ?b ?t) ?ball (= ?r caught)))))))))

(test gp-reachable-composes
  "reachable under not, under aggregate-all, and in explain-goals
(design D5, D7)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((ring (gp-persons '("a" "b" "c" "d" "e" "f")))
          (pq (gp-persons '("p" "q"))))
      (gp-ring ring)
      (gp-link (list (cons (first pq) (second pq))))
      (let ((*gp-a* (second pq)) (*gp-b* (first pq)))
        (is (equal '((yes))
                   (select (:max-inferences 5000) (?r)
                     (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                     (not (reachable ?a ?b g-knows)) (= ?r yes)))))
      (let ((*gp-a* (first ring)))
        (is (equal '((6))
                   (select (:max-inferences 20000) (?n)
                     (lisp ?a *gp-a*)
                     (aggregate-all count (reachable ?a ?x g-knows) ?n)))))
      (is (search "tabled"
                  (first (te-paths '(?x) '((reachable ?a ?x g-knows)))))))))

(defun gp-chorded-ring ()
  "a..f in a ring, chords a->c and b->d: three 3-edge walks from a, but
by distance level 2 is {d} and level 3 is {e}; returns the persons."
  (let ((ring (gp-persons '("a" "b" "c" "d" "e" "f"))))
    (gp-ring ring)
    (gp-link (list (cons (first ring) (third ring))
                   (cons (second ring) (fourth ring))))
    ring))

(test gp-k-hop-exact-and-distinct
  "Level 0 is the start; level 1 the successors; level 3 the nodes at
distance 3, once, where the three-edge select has one row per walk; a
bound target at its level once, elsewhere never; past a chain's end,
no row (design D3, Review Focus 4)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((ring (gp-chorded-ring))
          (chain (gp-persons '("x" "y" "z"))))
      (gp-link (list (cons (first chain) (second chain))
                     (cons (second chain) (third chain))))
      (let ((*gp-a* (first ring)))
        (is (equal '("a") (gp-names (select () (?x)
                                      (lisp ?a *gp-a*)
                                      (k-hop ?a 0 ?x g-knows)))))
        (is (equal '("b" "c") (gp-names (select () (?x)
                                          (lisp ?a *gp-a*)
                                          (k-hop ?a 1 ?x g-knows)))))
        (is (= 3 (length (select () (?d) (lisp ?a *gp-a*)
                           (g-knows ?a ?b) (g-knows ?b ?c) (g-knows ?c ?d)))))
        (is (equal '("e") (gp-names (select () (?x)
                                      (lisp ?a *gp-a*)
                                      (k-hop ?a 3 ?x g-knows))))))
      (let ((*gp-a* (first ring)) (*gp-b* (fifth ring)))
        (is (= 1 (length (select () (?r) (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                           (k-hop ?a 3 ?b g-knows) (= ?r yes)))))
        (is (null (select () (?r) (lisp ?a *gp-a*) (lisp ?b *gp-b*)
                    (k-hop ?a 1 ?b g-knows) (= ?r yes)))))
      (let ((*gp-a* (first chain)))
        (is (equal '("z") (gp-names (select () (?x)
                                      (lisp ?a *gp-a*)
                                      (k-hop ?a 2 ?x g-knows)))))
        (is (null (select () (?x) (lisp ?a *gp-a*)
                    (k-hop ?a 5 ?x g-knows))))))))

(test gp-k-hop-arguments
  "K unbound or negative, or an unbound start, signals PROLOG-ERROR;
an unbound type fails quietly (design D3)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((ps (gp-persons '("p" "q"))))
      (gp-link (list (cons (first ps) (second ps))))
      (let ((*gp-a* (first ps)))
        (is (eq :instantiation-error
                (gp-ball-formal
                 (lambda () (select () (?x) (lisp ?a *gp-a*)
                              (k-hop ?a ?k ?x g-knows))))))
        (is (eq :type-error
                (gp-ball-formal
                 (lambda () (select () (?x) (lisp ?a *gp-a*)
                              (k-hop ?a -1 ?x g-knows))))))
        (is (eq :instantiation-error
                (gp-ball-formal
                 (lambda () (select () (?x) (k-hop ?a 1 ?x g-knows))))))
        (is (null (select () (?x) (lisp ?a *gp-a*) (k-hop ?a 1 ?x ?t))))))))

;;; The whole-branch review's findings (2026-11-10).

(test gp-k-hop-stops-at-an-empty-frontier
  "K far past the graph's reach answers at once: the expansion stops
when a level is empty (review finding 2)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((chain (gp-persons '("x" "y" "z"))))
      (gp-link (list (cons (first chain) (second chain))
                     (cons (second chain) (third chain))))
      (let ((*gp-a* (first chain)))
        (is (null (select () (?x) (lisp ?a *gp-a*)
                    (k-hop ?a 1000000000000 ?x g-knows))))))))

(test gp-reachable-clauses-reload-once
  "Loading prolog-graph-predicates.lisp again leaves reachable/3 with
its two clauses, tabled (review finding 4)."
  (load (asdf:system-relative-pathname
         :graph-db "prolog-graph-predicates.lisp"))
  (let ((f (lookup-functor (make-functor-symbol 'graph-db::reachable 3))))
    (is (= 2 (length (graph-db::functor-clauses f))))
    (is-true (graph-db:functor-tabled-p 'graph-db::reachable 3))))
