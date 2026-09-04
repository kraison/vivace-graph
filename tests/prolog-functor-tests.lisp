;;;; Tests for the built-in Prolog functors (prolog-functors.lisp): unification
;;;; and (in)equality (=, ==, /=), numeric comparison (<, >, <=, >=), arithmetic
;;;; (is), type checks (numberp, atom, var), regex-match, the lisp escape, and
;;;; the control functors call/1, if/2, if/3, plus unique/1 deduplication.
;;;;
;;;; These predicates are mostly pure logic, but a query still needs a live
;;;; *graph*, so we wrap each in with-test-graph.

(in-package #:graph-db/test)

(def-suite prolog-functor-suite
  :description "Built-in Prolog functors: =, comparison, is, type checks, control."
  :in graph-db-suite)

(in-suite prolog-functor-suite)

(test eq-unifies-and-can-fail
  "=/2 unifies a var with a value; unifying two incompatible bound values fails."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(42) (select-flat (?x) (= ?x 42))))
    (is (null (select-flat (?x) (= ?x 1) (= ?x 2))))))

(test equality-and-inequality-functors
  "==/2 succeeds on equal derefs, /=/2 on unequal."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (== ?x 5))))
    (is (null         (select-flat (?x) (= ?x 5) (== ?x 6))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (/= ?x 6))))
    (is (null         (select-flat (?x) (= ?x 5) (/= ?x 5))))))

(test numeric-comparison-functors
  "</2 >/2 <=/2 >=/2 compare bound numbers."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (< ?x 10))))
    (is (null         (select-flat (?x) (= ?x 5) (< ?x 3))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (> ?x 4))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (>= ?x 5) (<= ?x 5))))
    (is (null         (select-flat (?x) (= ?x 5) (> ?x 5))))))

(test is-evaluates-arithmetic
  "is/2 unifies a var with the value of a Lisp arithmetic expression."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(42) (select-flat (?x) (is ?x (+ 40 2)))))
    (is (equal '(6)  (select-flat (?x) (is ?x (* 3 (+ 1 1))))))))

(test type-check-functors
  "numberp/1, atom/1 and var/1 gate on the term's type/binding."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (numberp ?x))))
    (is (null         (select-flat (?x) (= ?x foo) (numberp ?x))))
    (is (equal '(foo) (select-flat (?x) (= ?x foo) (atom ?x))))
    ;; var/1: succeeds while unbound, fails once bound
    (is (equal '(7) (select-flat (?x) (var ?x) (= ?x 7))))
    (is (null         (select-flat (?x) (= ?x 7) (var ?x))))))

(test regex-match-functor
  "regex-match/2 scans the second string for the first (a regex)."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '("hello world")
               (select-flat (?x) (= ?x "hello world") (regex-match "wor" ?x))))
    (is (null (select-flat (?x) (= ?x "hello world") (regex-match "^xyz" ?x))))))

(test lisp-escape-functor
  "lisp/2 unifies a var with the result of evaluating a Lisp form."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (lisp ?x (+ 2 3)))))
    (is (equal '((1 2 3)) (select-flat (?x) (lisp ?x (list 1 2 3)))))))

(test call-functor-invokes-goal
  "call/1 invokes a (possibly computed) goal."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(7) (select-flat (?x) (= ?x 7) (call (< ?x 10)))))
    (is (null         (select-flat (?x) (= ?x 7) (call (> ?x 10)))))))

(test if-3-chooses-branch
  "if/3 runs the THEN branch when the test succeeds, else the ELSE branch."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:yes) (select-flat (?r) (if (< 1 2) (= ?r :yes) (= ?r :no)))))
    (is (equal '(:no)  (select-flat (?r) (if (< 2 1) (= ?r :yes) (= ?r :no)))))))

(test if-2-runs-then-on-success-else-fails
  "if/2 runs THEN when the test succeeds; when the test fails the goal fails
(standard Prolog Cond -> Then with no else), so the whole branch yields nothing."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(99) (select-flat (?x) (if (< 1 2) (= ?x 99)))))
    ;; test false -> continuation never invoked -> branch fails -> no solutions
    (is (null (select-flat (?x) (= ?x 5) (if (< 2 1) (= ?x 99)))))))

(test unique-deduplicates-nodes
  "unique/1 collapses repeated bindings of the same node across solutions."
  (with-test-graph (g)
    (let (a)
      (with-transaction ()
        (setq a (make-g-person :name "A"))
        (make-g-knows :from a :to (make-g-person :name "B"))
        (make-g-knows :from a :to (make-g-person :name "C")))
      ;; A is the source of two edges -> two solutions for ?a without unique
      (is (= 2 (length (select-flat (?a) (g-knows ?a ?b)))))
      ;; unique/1 collapses them to one
      (is (= 1 (length (select-flat (?a) (g-knows ?a ?b) (unique ?a))))))))

;;; ---------------------------------------------------------------------------
;;; Control-flow core (#45 Phase 0).  These exercise the compiler-level control
;;; constructs -- not, if (->), once, forall -- which expand through
;;; COMPILE-BODY so they compose with conjunction and cut, rather than routing
;;; through the runtime call/1 functors.
;;; ---------------------------------------------------------------------------

(test if-3-else-only-when-test-has-no-solution
  "if/3 (Test -> Then ; Else) runs ELSE only when Test has ZERO solutions --
NOT when Test succeeds but Then fails (the pre-fix bug ran Else in that case)."
  (with-test-graph (g)
    (declare (ignore g))
    ;; Test succeeds, Then fails -> whole if fails; Else must NOT run.
    (is (null (select-flat (?r) (if (< 1 2) (> 1 2) (= ?r :else)))))
    ;; Test fails -> Else runs.
    (is (equal '(:else) (select-flat (?r) (if (< 2 1) (= ?r :then) (= ?r :else)))))
    ;; Test succeeds, Then succeeds -> Then runs, Else does not.
    (is (equal '(:then) (select-flat (?r) (if (< 1 2) (= ?r :then) (= ?r :else)))))))

(test if-3-commits-to-first-test-solution
  "if/3 commits to the first solution of a multi-solution Test (soft cut on the
condition): the disjunctive test offers ?r=1 then ?r=2, but only the first is
taken."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(1)
               (select-flat (?r)
                            (if (or (= ?r 1) (= ?r 2)) (numberp ?r) (= ?r :none)))))))

(test not-negates-and-composes
  "not/1 succeeds (continuing the conjunction) exactly when its goal fails, and
leaves no bindings behind."
  (with-test-graph (g)
    (declare (ignore g))
    ;; goal fails -> not succeeds, conjunction continues
    (is (equal '(5) (select-flat (?x) (= ?x 5) (not (> ?x 10)))))
    ;; goal succeeds -> not fails -> no solutions
    (is (null (select-flat (?x) (= ?x 5) (not (< ?x 10)))))
    ;; not over a negative test still binds the rest normally
    (is (equal '(5) (select-flat (?x) (= ?x 5) (not (= ?x 6)) (numberp ?x))))))

(test once-commits-to-first-solution
  "once/1 commits to the first solution of a multi-solution goal."
  (with-test-graph (g)
    (with-transaction ()
      (let ((a (make-g-person :name "A")))
        (make-g-knows :from a :to (make-g-person :name "B"))
        (make-g-knows :from a :to (make-g-person :name "C"))))
    ;; without once: two solutions for the source vertex
    (is (= 2 (length (select-flat (?a) (g-knows ?a ?b)))))
    ;; with once: committed to the first
    (is (= 1 (length (select-flat (?a) (once (g-knows ?a ?b))))))))

(test forall-universal-check
  "forall/2 succeeds iff Action holds for every solution of Cond."
  (with-test-graph (g)
    (declare (ignore g))
    ;; every disjunctive solution is a number -> forall succeeds
    (is (equal '(:ok)
               (select-flat (?r)
                            (forall (or (= ?x 2) (= ?x 4)) (numberp ?x))
                            (= ?r :ok))))
    ;; 2 is not > 3 -> forall fails -> no solution
    (is (null
         (select-flat (?r)
                      (forall (or (= ?x 2) (= ?x 4)) (> ?x 3))
                      (= ?r :ok))))))

;;; ---------------------------------------------------------------------------
;;; Meta-call: compiled call/N (#45 Phase 0.2).
;;; ---------------------------------------------------------------------------

(test call-n-appends-extra-arguments
  "Compiled call/N appends the extra arguments to a static goal template:
(call (g-knows ?a) ?b) == (g-knows ?a ?b)."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (make-g-knows :from a :to b)))
      (let ((pairs (select (:flat nil) (?a ?b) (call (g-knows ?a) ?b))))
        (is (= 1 (length pairs)))
        (destructuring-bind (a b) (first pairs)
          (is (equalp aid (id a)))
          (is (equalp bid (id b))))))))

(test call-of-compound-and-control-goals
  "A static call of a compound/control goal compiles inline and composes:
call of a disjunction enumerates both branches; call of a conjunction joins."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(1 2)
               (sort (select-flat (?x) (call (or (= ?x 1) (= ?x 2)))) #'<)))
    (is (equal '(7) (select-flat (?x) (call (and (= ?x 7) (numberp ?x))))))))

(test call-dynamic-meta-call
  "A call whose goal is a variable is solved at run time, including call/N
argument appending."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (make-g-knows :from a :to b)))
      ;; ?g bound to a goal term, then meta-called
      (is (equal '(:ok)
                 (select-flat (?r) (= ?g (< 1 2)) (call ?g) (= ?r :ok))))
      (is (null (select-flat (?r) (= ?g (< 2 1)) (call ?g) (= ?r :ok))))
      ;; dynamic call/N: extra argument appended at run time
      (let ((pairs (select (:flat nil) (?a ?b)
                           (= ?g (g-knows ?a)) (call ?g ?b))))
        (is (= 1 (length pairs)))
        (is (equalp aid (id (first (first pairs)))))
        (is (equalp bid (id (second (first pairs)))))))))

(test unknown-predicate-signals-error
  "An unknown predicate is deliberately noisy: it signals a prolog-error rather
than silently yielding no answers -- both on the compiled path and through a
dynamic meta-call."
  (with-test-graph (g)
    (declare (ignore g))
    ;; compiled path: a mistyped goal in a query body
    (signals graph-db:prolog-error
      (select-flat (?x) (no-such-predicate-xyz ?x)))
    ;; dynamic meta-call path (via %solve)
    (signals graph-db:prolog-error
      (select-flat (?r) (= ?g (no-such-predicate-xyz ?r)) (call ?g)))))

;;; ---------------------------------------------------------------------------
;;; ISO exceptions: throw/1 + catch/3 (#45).
;;; ---------------------------------------------------------------------------

(test catch-recovers-from-a-throw
  "catch/3 runs the recovery when Goal throws a ball that unifies with Catcher."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:caught)
               (select-flat (?r) (catch (throw oops) ?ball (= ?r :caught)))))))

(test catch-passes-through-when-goal-succeeds
  "When Goal succeeds without throwing, its solutions pass through and the
recovery does not run."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (catch (= ?x 5) ?b (= ?x 99)))))))

(test catch-binds-the-ball
  "The catcher unifies with the thrown ball, so the recovery can use it."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(my-ball)
               (select-flat (?b) (catch (throw my-ball) ?b (= 1 1)))))))

(test catch-non-matching-ball-propagates-to-outer
  "A ball that does not unify with the inner Catcher propagates to an outer
catch."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:outer)
               (select-flat (?r)
                            (catch (catch (throw the-ball) other-ball (= ?r :inner))
                                   ?any (= ?r :outer)))))))

(test catch-recovers-from-an-existence-error
  "A built-in error carries an ISO ball: an unknown predicate is an
existence_error catch/3 can match."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:missing)
               (select-flat (?r)
                            (catch (no-such-predicate 1)
                                   (:error (:existence-error :procedure ?proc) ?ctx)
                                   (= ?r :missing)))))))

(test catch-does-not-catch-continuation-errors
  "Only Goal is protected: a throw in the continuation after catch/3 succeeds is
not caught."
  (with-test-graph (g)
    (declare (ignore g))
    (signals graph-db:prolog-error
      (select-flat (?r) (catch (= ?r 1) ?b (= ?r 2)) (throw escaped)))))

(test catch-does-not-catch-resource-errors
  "Resource errors are not catchable -- a bounded query cannot swallow its own
budget enforcement."
  (with-test-graph (g)
    (declare (ignore g))
    (signals graph-db:prolog-resource-error
      (select (:max-inferences 40) (?r) (catch (repeat) ?b (= ?r :swallowed))))))

(test catch-does-not-catch-permission-errors
  "Permission errors are not catchable -- a read-only query cannot swallow the
effect policy."
  (with-test-graph (g)
    (declare (ignore g))
    (signals graph-db:prolog-permission-error
      (select (:effects nil) (?r) (catch (retract ?x) ?b (= ?r :swallowed))))))

(test cost-unbounded-goals-are-refused-under-resource-bounds
  "GH #285: a resource-bounded SELECT refuses a goal %TICK cannot bound
-- instantly, before the goal runs -- at any nesting; without bounds, or
with :allow-cost-unbounded t, the goal runs as before."
  (with-test-graph (g)
    (declare (ignorable g))
    ;; Refused with a bound, even nested under NOT, and instantly.
    (let ((start (get-internal-real-time)))
      (signals graph-db:prolog-cost-unbounded-error
        (select (:timeout 30) (?x)
                (lisp ?x "abc")
                (regex-match "b" ?x)))
      (signals graph-db:prolog-cost-unbounded-error
        (select (:max-inferences 1000) (?x)
                (lisp ?x "abc")
                (not (regex-match "z" ?x))))
      (is (< (/ (- (get-internal-real-time) start)
                internal-time-units-per-second)
             1)
          "refusal must not cost what the goal would have"))
    ;; Unbounded query: unchanged behaviour.
    (is (= 1 (length (select (:flat t) (?x)
                             (lisp ?x "abc")
                             (regex-match "b" ?x)))))
    ;; The explicit override accepts the uncovered bound.
    (is (= 1 (length (select (:flat t :timeout 30
                              :allow-cost-unbounded t)
                             (?x)
                             (lisp ?x "abc")
                             (regex-match "b" ?x)))))
    ;; Classification is engine data, and the name surface derives.
    (is-true (graph-db:functor-cost-unbounded-p
              (graph-db::make-functor-symbol
               (quote graph-db::regex-match) 2)))
    (is (member "REGEX-MATCH" (graph-db:cost-unbounded-predicate-names)
                :test #'string=))))

(test allow-cost-unbounded-reaches-a-running-goal
  "GH #334: DECLARE-FUNCTOR-COST-UNBOUNDED classifies a whole functor, but
a functor can be cheap or unbounded per GOAL -- cheap when an argument is
bound to an indexed value, unbounded when nothing is.  Such a functor must
refuse from its own body, and can only honour the caller's escape hatch if
the option reaches it at run time; :ALLOW-COST-UNBOUNDED was read as a
literal at macroexpansion and reached nothing."
  (with-test-graph (g)
    (declare (ignorable g))
    (is (equal '(:allowed)
               (select (:flat t :max-inferences 1000
                        :allow-cost-unbounded t)
                       (?a)
                       (lisp ?a (if graph-db:*allow-cost-unbounded*
                                    :allowed :refused)))))
    (is (equal '(:refused)
               (select (:flat t :max-inferences 1000)
                       (?a)
                       (lisp ?a (if graph-db:*allow-cost-unbounded*
                                    :allowed :refused)))))
    ;; NIL outside a query, so a functor body cannot read a stale value
    ;; left by the last one.
    (is (null graph-db:*allow-cost-unbounded*))))

;;; ---------------------------------------------------------------------------
;;; Head resolution (#322, second finding): a functor DEFINED with <- in
;;; a package that does not use GRAPH-DB still lands on that package's
;;; own NAME/ARITY symbol, via the :DEFINE T argument ADD-CLAUSE passes
;;; MAKE-FUNCTOR-SYMBOL -- not on an existing GRAPH-DB functor.  The head
;;; is =/2, a name GRAPH-DB already registers globally (see
;;; PROLOG-FUNCTORS.LISP's DEF-GLOBAL-PROLOG-FUNCTOR =/2), so the
;;; collision the fix must avoid is actually exercised.
;;; ---------------------------------------------------------------------------

(defpackage #:graph-db/test.define-probe (:use #:cl))

(test define-in-a-package-that-does-not-use-graph-db
  "<- in a package that does not use GRAPH-DB defines its own NAME/ARITY
symbol there, even though =/2 already names a GRAPH-DB global functor:
the clause lands on the probe package's own =/2, and GRAPH-DB::=/2
gains no user clause and stays the registered global (#322)."
  (let ((pkg (find-package "GRAPH-DB/TEST.DEFINE-PROBE"))
        (own nil))
    (unwind-protect
        (progn
          ;; GRAPH-DB::<- itself must be qualified to be called at all
          ;; from a package that uses nothing; only the clause body
          ;; (head, ?x) reads into PKG, which is the case this test
          ;; exists for.
          (let ((*package* pkg))
            (eval (read-from-string "(graph-db::<- (= ?x ?x))")))
          (setq own (find-symbol "=/2" pkg))
          (is-true own "the clause's functor symbol is not in its own package")
          (is-true (not (eq own 'graph-db::=/2))
                   "the probe's =/2 must not be GRAPH-DB's own symbol")
          (is-true (graph-db::lookup-functor own)
                   "the clause did not register under its own package's symbol")
          (is (null (graph-db::lookup-functor 'graph-db::=/2))
              "GRAPH-DB::=/2 must not have picked up a user clause")
          (is-true (nth-value 1 (gethash 'graph-db::=/2
                                          graph-db::*prolog-global-functors*))
                   "GRAPH-DB::=/2 must remain the registered global functor"))
      (when (and own (graph-db::lookup-functor own))
        (graph-db::delete-functor (graph-db::lookup-functor own)))
      (when pkg (delete-package pkg)))))
