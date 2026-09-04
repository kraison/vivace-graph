;;;; tests/query/guard-tests.lisp -- RUN-GUARDED-PROLOG (GH #322).
;;;;
;;;; Type/slot names are bare, never keyword-spelled ("qt-item", not
;;;; ":qt-item"): %SCAN-QUERY-TEXT refuses any ':' outright (query/
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
    ;; :LIMIT 5000 clamps to the 1000 ceiling (%CLAMP-CAP), where
    ;; %PROBE leaves no room for an extra row: TRUNCATED-P is NIL
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
