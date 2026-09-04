;;;; tests/rules/facts-tests.lisp -- claims as Prolog facts (spec §4,
;;;; GH #330).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

;; Every goal that must take an index route runs under a budget.  Without
;; one the family walk is legal (R7) and answers the same rows, so a route
;; that silently degraded to it would pass; with one the walk refuses, so
;; only the route can pass.  Under the guard a budget is always in effect,
;; which is what makes that degradation a user-visible refusal.

(test claim-generates-from-the-subject-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (:max-inferences 1000) (?o)
                  (claim ?c rt-claim "host" "h1" "runs" "app" ?o))))
      (is (equal '("db" "web")
                 (sort (mapcar #'first rows) #'string<))))))

(test claim-generates-from-the-object-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (:max-inferences 1000) (?s)
                  (claim ?c rt-claim "host" ?s "runs" "app" "web"))))
      (is (equal '("h1" "h2")
                 (sort (mapcar #'first rows) #'string<))))))

(test a-unary-claim-binds-a-nil-object-pair
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (:max-inferences 1000) (?r ?ons ?okey)
                  (claim ?c rt-claim "host" "h2" ?r ?ons ?okey))))
      (is (= 2 (length rows)))
      (is (member '("reachable" nil nil) rows :test #'equal))
      (is (member '("runs" "app" "web") rows :test #'equal)))))

(test an-unknown-namespace-yields-nothing-and-interns-nothing
  (with-rules-graph (g)
    (seed g)
    (is (null (select (:max-inferences 1000) (?o)
                (claim ?c rt-claim "never-recorded" "x" ?r ?ons ?o))))
    (is (null (find-symbol "NEVER-RECORDED" :keyword)))))

;; A bound namespace argument that names no keyword answers empty whatever
;; its type: "HOST" is not :HOST's wire form, "APP" not :APP's, and 5 is
;; no namespace at all.  Each reaches CLAIM/7 through the guard, where
;; falling through to the walk would refuse rather than answer.
(test an-unresolvable-namespace-answers-empty-rather-than-refusing
  (with-rules-graph (g)
    (seed g)
    (is (null (select (:max-inferences 1000) (?o)
                (claim ?c rt-claim "HOST" ?skey ?r ?ons ?o))))
    (is (null (select (:max-inferences 1000) (?o)
                (claim ?c rt-claim 5 ?skey ?r ?ons ?o))))
    (is (null (select (:max-inferences 1000) (?s)
                (claim ?c rt-claim ?sns ?s ?r "APP" ?o))))))

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
    (let ((strings (select (:max-inferences 1000) (?o)
                     (claim ?c rt-claim "host" "h1" "runs" "app" ?o)))
          (keywords (select (:max-inferences 1000) (?o)
                      (claim ?c rt-claim :host "h1" "runs" :app ?o))))
      (is (equal '("db" "web")
                 (sort (mapcar #'first strings) #'string<)))
      (is (equal strings keywords)))))
