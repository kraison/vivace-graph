;;;; Tests for the 4-port trace hook, trace-prolog, explain-goals and
;;;; profile-goals (GH #124, design
;;;; docs/superpowers/specs/2026-11-09-tracer-explain-design.md).
;;;;
;;;; Asserted functors are dropped before and after with
;;;; CI-WITH-FUNCTORS (tests/clause-indexing-tests.lisp, same package).
;;;; Every name here is prefixed TE-.

(in-package #:graph-db/test)

(def-suite tracer-explain-suite
  :description "The port hook, trace-prolog, explain-goals, profile-goals."
  :in graph-db-suite)

(in-suite tracer-explain-suite)

(defun te-ports (thunk &key (functor nil))
  "(PORT FUNCTOR-NAME DEPTH) for every event THUNK's run fires on a
TE- functor, in order (the engine's own goals, SELECT/2 among them,
are left out); FUNCTOR, a string, keeps only that functor's."
  (let ((log '()))
    (graph-db:with-prolog-ports ((port f args depth)
                        (declare (ignore args))
                        (let ((name (symbol-name f)))
                          (when (if functor
                                    (string= functor name)
                                    (and (>= (length name) 3)
                                         (string= "TE-" name :end2 3)))
                            (push (list port name depth) log))))
      (funcall thunk))
    (nreverse log)))

(test te-four-ports-of-a-fact-table
  "CALL, then per clause CLAUSE, EXIT, REDO, then FAIL (design D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2))
      (is (equal '((:call "TE-G/1" 0) (:clause "TE-G/1" 1)
                   (:exit "TE-G/1" 0) (:redo "TE-G/1" 0)
                   (:clause "TE-G/1" 1)
                   (:exit "TE-G/1" 0) (:redo "TE-G/1" 0) (:fail "TE-G/1" 0))
                 (te-ports (lambda () (select-flat (?x) (te-g ?x)))))))))

(test te-depth-nests-callees-not-siblings
  "A callee's goals are one level in; the goal after a solved goal is
its sibling, at the same depth (design D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-h 1))
      (<- (te-g 1))
      (<- (te-h ?x) (te-g ?x))
      (is (equal '((:call "TE-H/1" 0) (:clause "TE-H/1" 1)
                   (:call "TE-G/1" 1) (:clause "TE-G/1" 2) (:exit "TE-G/1" 1)
                   (:exit "TE-H/1" 0)
                   (:call "TE-G/1" 0) (:clause "TE-G/1" 1) (:exit "TE-G/1" 0)
                   (:redo "TE-G/1" 0) (:fail "TE-G/1" 0)
                   (:redo "TE-H/1" 0)
                   (:redo "TE-G/1" 1) (:fail "TE-G/1" 1)
                   (:fail "TE-H/1" 0))
                 (te-ports (lambda ()
                             (select-flat (?x) (te-h ?x) (te-g ?x)))))))))

(test te-dynamic-meta-call-has-ports
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (is (equal '(:call :clause :exit :redo :fail)
                 (mapcar #'first
                         (te-ports (lambda ()
                                     (select-flat (?x) (= ?goal (te-g ?x))
                                                  (call ?goal)))
                                   :functor "TE-G/1")))))))

(test te-a-cut-fires-the-fails-it-abandons
  "A cut's return-from unwinds through open ported calls; each fires
its FAIL, so CALLs and FAILs balance (design D1)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-c 1))
      (<- (te-g 1)) (<- (te-g 2))
      (<- (te-c ?x) (te-g ?x) !)
      (let ((ports (te-ports (lambda () (select-flat (?x) (te-c ?x))))))
        (is (= (count :call ports :key #'first)
               (count :fail ports :key #'first)))
        (is (equal '(1) (select-flat (?x) (te-c ?x))))))))

(test te-a-budget-error-fires-the-fails-and-resignals
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2)) (<- (te-g 3))
      (let ((log '()))
        (signals graph-db:prolog-resource-error
          (graph-db:with-prolog-ports ((port f args depth)
                              (declare (ignore args depth))
                              (push (list port f) log))
            (select (:max-inferences 2) (?x ?y ?z)
              (te-g ?x) (te-g ?y) (te-g ?z))))
        (is (plusp (count :call log :key #'first)))
        (is (= (count :call log :key #'first)
               (count :fail log :key #'first)))))))

(test te-a-hook-that-throws-leaves-the-depth-at-zero
  "The depth is bound, not set, so a hook that unwinds leaves nothing
behind (review focus 2)."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (ignore-errors
       (graph-db:with-prolog-ports ((port f args depth)
                           (declare (ignore f args depth))
                           (when (eq port :exit) (error "hook")))
         (select-flat (?x) (te-g ?x))))
      (is (= 0 graph-db:*prolog-port-depth*))
      (is (null graph-db:*prolog-port-hook*)))))

(test te-a-lisp-error-in-a-callee-resignals-after-the-fail
  "An error from inside a callee fires the FAIL and is not swallowed
(review focus 1)."
  (with-test-graph (g)
    (declare (ignore g))
    (let ((log '()))
      (signals error
        (graph-db:with-prolog-ports ((port f args depth)
                            (declare (ignore args depth))
                            (push (list port f) log))
          (select-flat (?x) (lisp ?x (error "callee")))))
      (is (member :fail log :key #'first)))))
