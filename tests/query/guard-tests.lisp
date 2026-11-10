;;;; tests/query/guard-tests.lisp -- RUN-GUARDED-PROLOG (GH #322).
;;;;
;;;; Type/slot names are bare, never keyword-spelled ("qt-item", not
;;;; ":qt-item"): SCAN-QUERY-TEXT refuses any ':' outright (query/
;;;; guard.lisp), the same rule tests/gui/gui-tests.lisp's Prolog
;;;; tests already rely on.

(in-package #:graph-db/query-test)
(in-suite query-suite)

(defun q (g text &rest keys)
  (apply #'graph-db.query:run-guarded-prolog text g keys))

(test data-rows-are-json-shaped
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i qt-item) (node-slot-value ?i label ?l)
              (node-slot-value ?i rank ?r)")
      (is (equal '("i" "l" "r") columns))
      (is (= 3 (length rows)))
      (is (every (lambda (row) (and (stringp (first row))
                                    (stringp (second row))
                                    (integerp (third row))))
                 rows))
      (is (equal '("a" "b" "c") (sort (mapcar #'second rows) #'string<)))
      (is (equal '(1 2 3) (sort (mapcar #'third rows) #'<)))
      (is (null truncated)))))

(test raw-rows-carry-nodes
  (with-query-graph (g)
    (seed g)
    (let ((rows (nth-value 1 (q g "(is-a ?i qt-item)" :format :raw))))
      (is (every (lambda (row) (graph-db::node-p (first row))) rows)))))

(test limit-clamps-and-flags-truncation
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i qt-item)" :limit 2)
      (declare (ignore columns))
      (is (= 2 (length rows)))
      (is (eq t truncated)))
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i qt-item)" :limit 3)
      (declare (ignore columns))
      (is (= 3 (length rows)))
      (is (null truncated)))
    ;; :LIMIT 5000 clamps to the 1000 ceiling (CLAMP-ROW-CAP), where
    ;; PROBE-LIMIT leaves no room for an extra row: TRUNCATED-P is NIL
    ;; here because 3 rows is short of the 1000 cap -- (>= 3 1000) is
    ;; NIL, not because being at the ceiling makes it NIL.  The other
    ;; ceiling case, an EXACTLY-full 1000-row page reading TRUNCATED-P
    ;; T, needs 1000 rows to seed and is documented (docs/guarded-
    ;; query.md), not exercised here.
    (multiple-value-bind (columns rows truncated)
        (q g "(is-a ?i qt-item)" :limit 5000)
      (declare (ignore columns))
      (is (= 3 (length rows)))
      (is (null truncated)))))

(test offset-pages-one-solution-order
  "GH #387: :OFFSET skips solutions, so LIMIT-sized windows at
successive offsets concatenate to the unpaged answer in its order;
TRUNCATED-P is per page.  A bad offset is no offset."
  (with-query-graph (g)
    (seed g)
    (let ((all (nth-value 1 (q g "(is-a ?i qt-item)"))))
      (is (= 3 (length all)))
      (multiple-value-bind (columns rows truncated)
          (q g "(is-a ?i qt-item)" :limit 2)
        (declare (ignore columns))
        (is (equal (subseq all 0 2) rows))
        (is (eq t truncated)))
      (multiple-value-bind (columns rows truncated)
          (q g "(is-a ?i qt-item)" :limit 2 :offset 2)
        (declare (ignore columns))
        (is (equal (subseq all 2) rows))
        (is (null truncated)))
      (multiple-value-bind (columns rows truncated)
          (q g "(is-a ?i qt-item)" :offset 3)
        (declare (ignore columns))
        (is (null rows))
        (is (null truncated)))
      (is (equal all (nth-value 1 (q g "(is-a ?i qt-item)" :offset 0))))
      (is (equal all (nth-value 1 (q g "(is-a ?i qt-item)" :offset -4))))
      (is (equal all (nth-value 1 (q g "(is-a ?i qt-item)"
                                     :offset "two")))))))

(test a-string-bound-pages-by-key
  "GH #387's third option: </2 and >/2 order two strings lexically, so a
caller pages an indexed route by bounding the last key it saw.  The
solutions arrive in type-index order, which is hash order (CCL's changed
with #424's fold), so they are compared sorted."
  (with-query-graph (g)
    (seed g)
    (is (equal '("b" "c")
               (sort (mapcar #'second
                             (nth-value 1 (q g "(is-a ?i qt-item)
                                                (node-slot-value ?i label ?l)
                                                (> ?l \"a\")")))
                     #'string<)))
    (is (equal '("a")
               (mapcar #'second
                       (nth-value 1 (q g "(is-a ?i qt-item)
                                          (node-slot-value ?i label ?l)
                                          (< ?l \"b\")")))))
    ;; A string against a number is neither order: the goal fails.
    (is (null (nth-value 1 (q g "(is-a ?i qt-item)
                                 (node-slot-value ?i label ?l)
                                 (> ?l 0)"))))))

(test each-screened-token-is-refused-and-its-absence-accepted
  (with-query-graph (g)
    (seed g)
    (dolist (pair '(("(is-a ?i graph-db::qt-item)" . "(is-a ?i qt-item)")
                    ("(is-a ?i #.(quit))" . "(is-a ?i qt-item)")
                    ("(is-a ?i `x)" . "(is-a ?i qt-item)")
                    ("(is-a ?i ,x)" . "(is-a ?i qt-item)")
                    ("(is-a ?i qt-item" . "(is-a ?i qt-item)")))
      (signals graph-db.query:prolog-guard-error (q g (car pair)))
      (finishes (q g (cdr pair))))))

(test unregistered-functor-and-string-head-refused
  (with-query-graph (g)
    (signals graph-db.query:prolog-guard-error (q g "(no-such-thing ?x)"))
    (signals graph-db.query:prolog-guard-error
      (q g "(\"is-a\" ?x qt-item)"))
    (signals graph-db.query:prolog-guard-error (q g "(lisp ?x (quit))"))))

(test an-inference-budget-breach-is-a-resource-error
  (with-query-graph (g)
    (seed g)
    (signals graph-db:prolog-resource-error
      (q g "(is-a ?i qt-item) (is-a ?j qt-item) (is-a ?k qt-item)"
         :max-inferences 2))))

(defun %scratch-package-names ()
  "Names of every live scratch package RUN-GUARDED-PROLOG could have
left behind, so a count check alone can't miss a leak masked by some
unrelated package disappearing in the same window."
  (remove-if-not (lambda (name)
                    (and (>= (length name) 21)
                         (string= name "GRAPH-DB.GUI.SCRATCH-"
                                  :end1 21)))
                  (mapcar #'package-name (list-all-packages))))

(test the-scratch-package-is-gone-afterwards
  (with-query-graph (g)
    (seed g)
    (let ((before (length (list-all-packages))))
      (q g "(is-a ?i qt-item)")
      (ignore-errors (q g "(is-a ?i #.(quit))"))
      (is (= before (length (list-all-packages))))
      (is (null (%scratch-package-names))
          "a scratch package survived: ~S" (%scratch-package-names)))))

(test hyphenated-variable-becomes-a-camelcase-column
  "A ?hyphenated-name variable's column is its camelCase wire spelling
(%QUERY-VAR-FIELD), same rule the DSL uses (GH #322)."
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (q g "(is-a ?i qt-item) (node-slot-value ?i label ?item-label)")
      (declare (ignore rows))
      (is (equal '("i" "itemLabel") columns)))))

(test edge-and-global-functors-resolve-in-one-goal-list
  "Spec SS6: the schema package uses nothing, so before the
head-resolution change QT-LINKS/2 could not be found from GRAPH-DB's
package nor IS-A/2 from the schema's."
  (with-query-graph (g)
    (destructuring-bind (a b c) (seed g)
      (declare (ignore c))
      (multiple-value-bind (columns rows)
          (q g "(is-a ?x qt-item) (qt-links ?x ?y)")
        (is (equal '("x" "y") columns))
        (is (= 2 (length rows)))
        (is (member (list (string-id a) (string-id b)) rows
                    :test #'equal))))))

;; GUARD-QUERY-TEXT (GH #331): the compile half of RUN-GUARDED-PROLOG
;; for a caller that keeps the goals.
(test guard-query-text-returns-guarded-goals-with-uninterned-vars
  (with-query-graph (g)
    (multiple-value-bind (vars goals)
        (graph-db.query:guard-query-text
         "(is-a ?p qt-item) (qt-links ?p ?f)" g)
      (is (= 2 (length vars)))
      (is (every (lambda (v) (null (symbol-package v))) vars))
      ;; The same ?P in both goals is one symbol.
      (is (eq (second (first goals)) (second (second goals))))
      ;; Heads are the engine's own symbols, not the scratch package's.
      (is (eq (find-package :graph-db) (symbol-package (first (first goals)))))
      (is (eq 'graph-db/query-test.schema::qt-item
              (third (first goals)))))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:guard-query-text "(no-such-functor ?x)" g))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:guard-query-text "(is-a ?p graph-db::vertex)" g))))

(test a-keyword-slot-is-filtered-by-a-string
  "GH #351: RANK is untyped, so an item can hold a keyword there; the
guard refuses every keyword spelling, and a string now unifies."
  (with-query-graph (g)
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db/query-test.schema::make-qt-item :graph g :label "k"
                                                 :rank :high)
      (graph-db/query-test.schema::make-qt-item :graph g :label "n"
                                                 :rank 1))
    (let ((rows (nth-value 1 (q g "(is-a ?i qt-item)
                                   (node-slot-value ?i rank \"high\")
                                   (node-slot-value ?i label ?l)"))))
      (is (= 1 (length rows)))
      (is (string= "k" (second (first rows)))))
    (let ((rows (nth-value 1 (q g "(is-a ?i qt-item)
                                   (node-slot-value ?i rank ?r)
                                   (= ?r \"HIGH\")"))))
      (is (= 1 (length rows)) "case-insensitive through =/2 too"))))

(test node-slot-value-needs-no-is-a-through-the-guard
  "GH #351: the guarded runner reaches the enumeration; the ?i column
holds the node id as a string, as any node cell does."
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (q g "(node-slot-value ?i label \"b\") (node-slot-value ?i rank ?r)")
      (is (equal '("i" "r") columns))
      (is (= 1 (length rows)))
      (is (stringp (first (first rows))))
      (is (= 2 (second (first rows)))))))

(test node-slot-value-with-a-non-symbol-slot-is-zero-rows-not-an-error
  "GH #351: a string SLOT through the enumeration arm is not a slot
name; the guard sees zero rows, not a TYPE-ERROR server fault."
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (q g "(node-slot-value ?i \"label\" ?v)")
      (is (equal '("i" "v") columns))
      (is (null rows)))))

(test node-slot-value-unbound-slot-enumeration-through-the-guard
  "GH #351: the guarded runner reaches the unbound-slot arm too --
3 QT-ITEMs x 2 data slots (LABEL, RANK) is 6 rows, each ?s a keyword."
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (q g "(is-a ?i qt-item) (node-slot-value ?i ?s ?v)")
      (is (equal '("i" "s" "v") columns))
      (is (= 6 (length rows)))
      (is (every (lambda (row) (keywordp (second row))) rows)))))

(test an-unbound-node-enumeration-ticks-the-budget
  "GH #351: the enumeration arm visits every vertex through UNIFY,
which fails for each non-matching one without calling CONT -- so it
must %TICK per vertex itself, or :MAX-INFERENCES can never interrupt
a scan that never reaches a goal boundary."
  (with-query-graph (g)
    (seed g)
    (signals graph-db:prolog-resource-error
      (q g "(node-slot-value ?i label \"nowhere\")" :max-inferences 1))))

(test rule-delta-is-withheld-from-free-text
  "GH #333: the fixpoint's generator is not a query surface."
  (with-query-graph (g)
    (signals graph-db.query:prolog-guard-error
      (q g "(rule-delta ?c \"x\")"))))

;;; GH #373: the runner interprets the generated SELECT lambda instead of
;;; compiling it natively -- a full SBCL compile per request was ~94% of
;;; a small query's time.  PROBE-EVAL-MODE/1 reports the mode the query
;;; body actually runs under.
#+sbcl
(graph-db:def-global-prolog-functor probe-eval-mode/1 (?mode cont)
  "Test-only (GH #373): unify ?MODE with SB-EXT:*EVALUATOR-MODE* as seen
from inside a running query body."
  (let ((old (fill-pointer graph-db:*trail*)))
    (when (graph-db:unify ?mode sb-ext:*evaluator-mode*)
      (funcall cont))
    (graph-db:undo-bindings old)))

#+sbcl
(test the-runner-interprets-the-query-instead-of-compiling-it
  "GH #373: RUN-QUERY-GOALS evaluates the SELECT form under the
interpreter, so no request pays a native compile; the query body sees
:INTERPRET."
  (with-query-graph (g)
    (let ((rows '()))
      (graph-db::run-query-goals '(?m) '((probe-eval-mode ?m)) g
                                 :format :raw
                                 :callback (lambda (row) (push row rows)))
      (is (equal '((:interpret)) rows)))))

(test an-absurd-exponent-is-refused-before-the-reader-sees-it
  "GH #315: not every reader rejects 1e999999.  SBCL signals, ECL reads
infinity (caught by the non-finite screen), CCL clamps it to the largest
float and passes it on as a finite number.  The raw text is screened
first, so the answer is the same refusal everywhere; a literal inside a
string is data and passes."
  (signals graph-db.query:prolog-guard-error
    (graph-db.query::%screen-exponent-text "(= ?x 1e999999)"))
  (signals graph-db.query:prolog-guard-error
    (graph-db.query::%screen-exponent-text "(= ?x 2.5d+99999)"))
  (finishes (graph-db.query::%screen-exponent-text
             "(= ?x 1.5e3) (= ?y 1e308) (= ?z \"1e999999\")")))

(test the-cap-and-probe-rule-has-one-home
  "CLAMP-ROW-CAP and PROBE-LIMIT are the DSL's rule about
*QUERY-DEFAULT-LIMIT*, used by the runner, the guard and the GUI (#328)."
  (let ((ceiling graph-db:*query-default-limit*))
    (is (= ceiling (graph-db:clamp-row-cap nil)))
    (is (= ceiling (graph-db:clamp-row-cap 0)))
    (is (= ceiling (graph-db:clamp-row-cap -3)))
    (is (= ceiling (graph-db:clamp-row-cap "7")))
    (is (= 7 (graph-db:clamp-row-cap 7)))
    (is (= ceiling (graph-db:clamp-row-cap (1+ ceiling))))
    (is (= 8 (graph-db:probe-limit 7)))
    (is (= ceiling (graph-db:probe-limit ceiling))))
  (let ((graph-db:*query-default-limit* 5))
    (is (= 5 (graph-db:clamp-row-cap 9)))
    (is (= 4 (graph-db:clamp-row-cap 4)))
    (is (= 5 (graph-db:probe-limit 4)))
    (is (= 5 (graph-db:probe-limit 5)))))

(test the-length-cap-is-enforced-by-the-screen
  "*prolog-max-query-length* is the screen's own limit, so every entry
refuses an overlong text, not only the GUI endpoint (#366)."
  (with-query-graph (g)
    (let ((long (make-string (1+ graph-db.query:*prolog-max-query-length*)
                             :initial-element #\Space))
          (full (make-string graph-db.query:*prolog-max-query-length*
                             :initial-element #\Space)))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:scan-query-text long))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:run-guarded-prolog long g))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:guard-query-text long g))
      (is (eq t (graph-db.query:scan-query-text full))))))

(test with-screened-forms-reads-into-a-scratch-package-that-leaves-with-it
  (let ((packages (length (list-all-packages)))
        (home nil))
    (graph-db.query:with-screened-forms (forms "(sr-probe-head ?x \"s\" 3)")
      (is (= 1 (length forms)))
      (destructuring-bind (head var str num) (first forms)
        (setf home (package-name (symbol-package head)))
        (is (string= "SR-PROBE-HEAD" (symbol-name head)))
        (is (string= "?X" (symbol-name var)))
        (is (string= "s" str))
        (is (= 3 num))
        (is (eq (symbol-package head) (symbol-package var)))
        (is (not (member (symbol-package head)
                         (list (find-package :graph-db)
                               (find-package :graph-db.query)
                               (find-package :keyword)
                               *package*))))))
    (is (null (find-package home)))
    (is (= packages (length (list-all-packages))))
    (is (null (find-symbol "SR-PROBE-HEAD" :graph-db)))
    (is (null (find-symbol "SR-PROBE-HEAD" :graph-db.query)))))

(test the-screened-reader-refuses-what-the-guard-refuses
  "Each refusal is the guard's own condition, and a refusal mid-read
still deletes the scratch package."
  (let ((packages (length (list-all-packages))))
    (dolist (text '("graph-db::vertex" "(a #.(+ 1 2))" "(a `b)" "(a"
                    "1e999999"))
      (signals graph-db.query:prolog-guard-error
        (graph-db.query:call-with-screened-forms text #'identity)))
    (is (= packages (length (list-all-packages))))))

(test run-guarded-goals-runs-kept-goals-under-the-same-rails
  "A caller that kept guard-query-text's goals runs them with
run-guarded-prolog's clamp, probe, paging, budgets and shaping (#383)."
  (with-query-graph (g)
    (seed g)
    (let ((text "(is-a ?i qt-item) (node-slot-value ?i label ?l)"))
      (multiple-value-bind (vars goals)
          (graph-db.query:guard-query-text text g)
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g)
          (multiple-value-bind (columns* rows* truncated*)
              (graph-db.query:run-guarded-prolog text g)
            (is (equal columns columns*))
            (is (equal (sort (copy-list rows) #'string< :key #'second)
                       (sort (copy-list rows*) #'string< :key #'second)))
            (is (eq truncated truncated*))))
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g :limit 2)
          (declare (ignore columns))
          (is (= 2 (length rows)))
          (is (eq t truncated)))
        (multiple-value-bind (columns rows truncated)
            (graph-db.query:run-guarded-goals vars goals g
                                              :limit 2 :offset 2)
          (declare (ignore columns))
          (is (= 1 (length rows)))
          (is (null truncated)))
        (is (graph-db::node-p
             (first (first (nth-value 1 (graph-db.query:run-guarded-goals
                                         vars goals g :format :raw))))))
        (signals type-error
          (graph-db.query:run-guarded-goals vars goals g :format :json))
        (multiple-value-bind (vars2 goals2)
            (graph-db.query:guard-query-text
             "(is-a ?i qt-item) (is-a ?j qt-item) (is-a ?k qt-item)" g)
          (signals graph-db:prolog-resource-error
            (graph-db.query:run-guarded-goals vars2 goals2 g
                                              :max-inferences 2)))
        ;; The kept goals are re-runnable: a later write is seen.
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db/query-test.schema::make-qt-item
           :graph g :label "d" :rank 4))
        (is (= 4 (length (nth-value 1 (graph-db.query:run-guarded-goals
                                       vars goals g)))))))))

(test teq-explain-text-matches-explain-goals
  "explain-text screens, reads and guards the text as guard-query-text
does, then explains the same goals (design D5, GH #124)."
  (with-query-graph (g)
    (let* ((text "(is-a ?i qt-item) (qt-links ?i ?j)")
           (from-text (graph-db.query:explain-text text g))
           (from-goals (multiple-value-bind (vars goals)
                           (graph-db.query:guard-query-text text g)
                         (graph-db:explain-goals vars goals :graph g))))
      (is (equal (mapcar (lambda (r) (getf r :path)) from-text)
                 (mapcar (lambda (r) (getf r :path)) from-goals)))
      (is (equal '("type index: QT-ITEM" "ve-out index of ?I")
                 (mapcar (lambda (r) (getf r :path)) from-text))))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:explain-text "(is-a ?p graph-db::vertex)" g))))

(test teq-profile-text-runs-under-the-rails-and-refuses-like-the-guard
  (with-query-graph (g)
    (seed g)
    (multiple-value-bind (solutions rows err)
        (graph-db.query:profile-text "(is-a ?i qt-item)" g)
      (is (= 3 solutions))
      (is (null err))
      (is (find "IS-A/2" rows
                :key (lambda (r) (symbol-name (getf r :functor)))
                :test #'string=)))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:profile-text "(no-such-functor ?x)" g))))
