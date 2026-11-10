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

;;; trace-prolog (design D2).

(defun te-trace-text (thunk &rest names)
  "THUNK's trace through TRACE-PROLOG on NAMES, as a string."
  (let ((*trace-output* (make-string-output-stream)))
    (unwind-protect
         (progn (apply #'graph-db:trace-prolog names)
                (funcall thunk)
                (get-output-stream-string *trace-output*))
      (graph-db:untrace-prolog))))

(test te-trace-prolog-prints-ports-indented
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1) (te-h 1))
      (<- (te-g 1)) (<- (te-h ?x) (te-g ?x))
      (let ((text (te-trace-text
                   (lambda () (select-flat (?x) (te-h ?x))))))
        (is (search "[0] CALL TE-H/1 (" text))
        (is (search "[1]   CALL TE-G/1 (" text))
        (is (search "[1]   EXIT TE-G/1 (1)" text))
        (is (search "[0] EXIT TE-H/1 (1)" text))
        (is (search "CLAUSE TE-G/1 1/1" text))
        (is (search "[0] FAIL TE-H/1" text))))))

(test te-trace-prolog-filters-by-name-across-packages
  "A bare name traces every arity of that functor and nothing else,
whatever package the functor symbol lives in (review focus 4)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction () (make-g-person :name "te-a"))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (let ((text (te-trace-text
                   (lambda ()
                     (select-flat (?x) (is-a ?p g-person) (te-g ?x)))
                   'te-g)))
        (is (search "TE-G/1" text))
        (is (null (search "IS-A/2" text))))
      (let ((text (te-trace-text
                   (lambda () (select-flat (?a) (g-knows ?a ?b)))
                   'g-knows)))
        (is (search "G-KNOWS/2" text))))))

(test te-untrace-prolog-clears-the-hook
  (graph-db:trace-prolog)
  (graph-db:untrace-prolog)
  (is (null graph-db:*prolog-port-hook*)))

;;; explain-goals (design D3).

(def-vertex te-item ()
  ((code :type string :index t)
   (label))
  :graph-db-integration-test)

(defun te-paths (vars goals)
  (mapcar (lambda (row) (getf row :path))
          (graph-db:explain-goals vars goals)))

(test te-explain-type-slot-and-edge-paths
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '("type index: G-PERSON" "slot read" "ve-out index of ?P")
               (te-paths '(?n) '((is-a ?p g-person)
                                 (node-slot-value ?p name ?n)
                                 (g-knows ?p ?q)))))
    (is (equal "type check"
               (second (te-paths '(?t) '((is-a ?p g-person) (is-a ?p ?t))))))
    (is (search "every vertex" (first (te-paths '(?p ?t) '((is-a ?p ?t))))))
    (is (search "not a type"
                (first (te-paths '(?p) '((is-a ?p te-no-such-type))))))
    (is (equal "vev index: one lookup"
               (third (te-paths '(?a) '((is-a ?a g-person) (is-a ?b g-person)
                                        (g-knows ?a ?b))))))
    (is (equal "ve-in index of ?B"
               (second (te-paths '(?b) '((is-a ?b g-person) (g-knows ?a ?b))))))
    (is (equal "every G-KNOWS edge"
               (first (te-paths '(?a) '((g-knows ?a ?b))))))
    (is (equal "every vertex of every type"
               (first (te-paths '(?n) '((node-slot-value ?p name ?n))))))))

(test te-explain-asserted-functor-candidates
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-k 2))
      (<- (te-k a 1)) (<- (te-k b 2)) (<- (te-k ?v 3))
      (let ((rows (graph-db:explain-goals '(?r) '((te-k a ?r)))))
        (is (eq :user (getf (first rows) :kind)))
        (is (equal '(2 . 3) (getf (first rows) :candidates)))
        (is (equal "first-argument index: 2 of 3 clauses"
                   (getf (first rows) :path))))
      (is (equal "every clause: 3"
                 (first (te-paths '(?k ?r) '((te-k ?k ?r))))))
      (is (search "by value at run time, 3 clauses"
                  (second (te-paths '(?r) '((= ?k a) (te-k ?k ?r)))))))))

(test te-explain-unknown-functor-and-binding-through-not
  (with-test-graph (g)
    (declare (ignore g))
    (let ((rows (graph-db:explain-goals '(?x) '((te-no-such-pred ?x)
                                                 (is-a ?x g-person)))))
      (is (eq :unknown (getf (first rows) :kind)))
      (is (= 2 (length rows))))
    ;; ?Q is bound inside the not and unbound after it.
    (is (equal "ve-out index of ?P"
               (third (te-paths '(?p) '((is-a ?p g-person)
                                        (not (g-knows ?p ?q))
                                        (g-knows ?p ?q))))))
    (is (search "meta-call" (first (te-paths '(?g) '((call ?g))))))))

(test te-explain-index-paths
  (with-test-graph (g)
    (declare (ignore g))
    (is (search "ordered index on TE-ITEM.CODE"
                (first (te-paths '(?n)
                                 '((find-by-slot ?n te-item code "x"))))))
    (is (search "the call signals"
                (first (te-paths '(?n)
                                 '((find-by-slot ?n te-item label "x"))))))
    (is (search "the call signals"
                (first (te-paths '(?n)
                                 '((find-by-unique ?n te-item code "x"))))))
    (is (search "by value at run time"
                (second (te-paths '(?n)
                                  '((= ?s code)
                                    (find-by-slot ?n te-item ?s "x"))))))))

(test te-explain-answers-a-row-with-no-graph-to-ask
  "explain-goals promises a row, not an error, even when the type
lookup cannot be made: no graph bound."
  (let ((rows (let ((*graph* nil))
                (graph-db:explain-goals '(?p) '((is-a ?p g-person))
                                        :graph nil))))
    (is (= 1 (length rows)))
    (is (search "no graph" (getf (first rows) :path)))))

(test te-explain-prints-a-table-and-flags-cost-unbounded
  (with-test-graph (g)
    (declare (ignore g))
    (let* ((rows nil)
           (text (with-output-to-string (s)
                   (setf rows (graph-db:explain-goals
                               '(?p) '((is-a ?p g-person)
                                       (regex-match "a+" "aaa"))
                               :stream s)))))
      (is (= 2 (length rows)))
      (is (search "IS-A/2" text))
      (is (search "cost-unbounded" (getf (second rows) :path))))))

;;; profile-goals (design D4).

(test te-profile-counts-match-the-ports
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2))
      (multiple-value-bind (solutions rows err)
          (graph-db:profile-goals '(?x) '((te-g ?x)))
        (is (= 2 solutions))
        (is (null err))
        (let ((row (find "TE-G/1" rows
                         :key (lambda (r) (symbol-name (getf r :functor)))
                         :test #'string=)))
          (is (= 1 (getf row :calls)))
          (is (= 2 (getf row :exits)))
          (is (= 2 (getf row :redos)))
          (is (= 1 (getf row :fails)))
          (is (= 2 (getf row :clauses)))
          (is (and (realp (getf row :seconds)) (>= (getf row :seconds) 0))))))))

(test te-profile-under-a-budget-returns-rows-and-the-error
  "Review focus 5: a tripped budget still yields balanced rows, and the
error comes back as the third value."
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1)) (<- (te-g 2)) (<- (te-g 3))
      (multiple-value-bind (solutions rows err)
          (graph-db:profile-goals '(?x ?y ?z)
                                  '((te-g ?x) (te-g ?y) (te-g ?z))
                                  :max-inferences 2)
        (declare (ignore solutions))
        (is (typep err 'graph-db:prolog-resource-error))
        (let ((row (find "TE-G/1" rows
                         :key (lambda (r) (symbol-name (getf r :functor)))
                         :test #'string=)))
          (is (plusp (getf row :calls)))
          (is (= (getf row :calls) (getf row :fails))))))))

(test te-profile-prints-a-table
  (with-test-graph (g)
    (declare (ignore g))
    (ci-with-functors ((te-g 1))
      (<- (te-g 1))
      (let ((text (with-output-to-string (s)
                    (graph-db:profile-goals '(?x) '((te-g ?x)) :stream s))))
        (is (search "TE-G/1" text))))))
