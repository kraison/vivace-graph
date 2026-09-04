;;;; tests/rules/facts-tests.lisp -- claims as Prolog facts (spec §4,
;;;; GH #330).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(test claim-generates-from-the-subject-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select () (?o)
                  (claim ?c rt-claim "host" "h1" "runs" "app" ?o))))
      (is (equal '("db" "web")
                 (sort (mapcar #'first rows) #'string<))))))
