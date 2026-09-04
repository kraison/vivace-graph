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

(test claim-generates-from-the-object-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select () (?s)
                  (claim ?c rt-claim "host" ?s "runs" "app" "web"))))
      (is (equal '("h1" "h2")
                 (sort (mapcar #'first rows) #'string<))))))

(test a-unary-claim-binds-a-nil-object-pair
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select () (?r ?ons ?okey)
                  (claim ?c rt-claim "host" "h2" ?r ?ons ?okey))))
      (is (member '("reachable" nil nil) rows :test #'equal))
      (is (member '("runs" "app" "web") rows :test #'equal)))))

(test an-unknown-namespace-yields-nothing-and-interns-nothing
  (with-rules-graph (g)
    (seed g)
    (is (null (select () (?o)
                (claim ?c rt-claim "never-recorded" "x" ?r ?ons ?o))))
    (is (null (find-symbol "NEVER-RECORDED" :keyword)))))

(test an-unregistered-family-is-ill-typed
  (with-rules-graph (g)
    (signals unknown-claim-family
      (select () (?o)
        (claim ?c no-such-family "host" "h1" ?r ?ons ?o)))))

(test nothing-bound-is-refused-under-a-bound-and-walks-without-one
  (with-rules-graph (g)
    (seed g)
    (signals graph-db::prolog-cost-unbounded-error
      (select (:max-inferences 1000) (?c)
        (claim ?c rt-claim ?a ?b ?r ?d ?e)))
    (is (= 4 (select-count (?c) (claim ?c rt-claim ?a ?b ?r ?d ?e))))))

;; R17: a keyword namespace argument selects the right candidates through
;; the index, so a string-only unification would answer nothing at all.
(test a-keyword-namespace-answers-like-its-string
  (with-rules-graph (g)
    (seed g)
    (let ((strings (select () (?o)
                     (claim ?c rt-claim "host" "h1" "runs" "app" ?o)))
          (keywords (select () (?o)
                      (claim ?c rt-claim :host "h1" "runs" :app ?o))))
      (is (equal '("db" "web")
                 (sort (mapcar #'first strings) #'string<)))
      (is (equal strings keywords)))))
