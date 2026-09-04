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

;; The filters below ride routes CLAIM/7's own tests already prove, so
;; they run unbudgeted (R21).  CLAIM-PRODUCER/2's generator IS a route,
;; so its two generating goals carry a budget: without one an unbound ?C
;; would leave the family walk legal for the goal that follows.

(test claim-current-filters-a-retracted-claim
  (with-rules-graph (g)
    (seed g)
    (let ((c (first (claims-touching g 'rt-claim :host "h2" :role :subject
                                     :relation "reachable"))))
      (retract-claim c))
    (is (= 2 (select-count (?c) (claim ?c rt-claim "host" "h2" ?r ?a ?b))))
    (is (= 1 (select-count (?c) (claim ?c rt-claim "host" "h2" ?r ?a ?b)
                                (claim-current ?c))))
    ;; Which one survives, not just how many: a negated test keeps one too.
    (is (equal '("runs")
               (select-flat (?r) (claim ?c rt-claim "host" "h2" ?r ?a ?b)
                                 (claim-current ?c))))))

(test claim-valid-at-uses-the-validity-extent
  (with-rules-graph (g)
    (seed g)
    (is (equal '("1")
               (select-flat (?v) (claim ?c rtt-claim "app" "web"
                                        "version" "ver" ?v)
                                 (claim-valid-at ?c "2026-02-15T00:00:00Z"))))
    (is (equal '("2")
               (select-flat (?v) (claim ?c rtt-claim "app" "web"
                                        "version" "ver" ?v)
                                 (claim-valid-at ?c "2026-06-15T00:00:00Z"))))
    ;; A claim with no extent never matches.
    (is (null (select-flat (?c) (claim ?c rt-claim "host" "h1" ?r ?a ?b)
                                (claim-valid-at ?c "2026-02-15T00:00:00Z"))))))

(test claim-producer-generates-from-the-producer-index
  (with-rules-graph (g)
    (seed g)
    (is (= 2 (select (:count t :max-inferences 1000) (?c)
               (claim-producer ?c "scan-b"))))
    ;; R6: scan-a wrote 2 rt-claims and 2 rtt-claims; the family goal
    ;; keeps the 2 rt-claims, which is what proves %UNIFY-CLAIM's
    ;; parent-class gate.
    (is (= 2 (select (:count t :max-inferences 1000) (?c)
               (claim-producer ?c "scan-a")
               (claim ?c rt-claim ?s ?k ?r ?a ?b))))
    (is (equal '("scan-a")
               (select-flat (?p) (claim ?c rt-claim "host" "h1" "runs"
                                        "app" "db")
                                 (claim-producer ?c ?p))))))

(test the-slot-filters
  (with-rules-graph (g)
    (seed g)
    (is (equal '("inferred")
               (select-flat (?s) (claim ?c rt-claim "host" "h2"
                                        "reachable" ?a ?b)
                                 (claim-standing ?c ?s))))
    (is (equal '("runs")
               (select-flat (?r) (claim ?c rt-claim "host" "h1" "runs"
                                        "app" "db")
                                 (claim-relation ?c ?r))))
    (is (equal '(nil)
               (select-flat (?v) (claim ?c rt-claim "host" "h1" "runs"
                                        "app" "db")
                                 (claim-rule-version ?c ?v))))))

;; The same functors from free text.  The guard needs no edit to admit
;; them -- it enumerates the live registry per call -- so what this pins
;; is that the names resolve there, that a keyword namespace is a
;; refusal rather than a match, and that an unregistered family is
;; client input rather than an engine fault (spec §4).
(test the-guard-admits-the-claim-functors
  (with-rules-graph (g)
    (seed g)
    (multiple-value-bind (columns rows)
        (graph-db.query:run-guarded-prolog
         "(claim ?c rt-claim \"host\" \"h1\" \"runs\" \"app\" ?o)
          (claim-producer ?c ?p)"
         g)
      (is (equal '("c" "o" "p") columns))
      (is (= 2 (length rows)))
      (is (every (lambda (row) (string= "scan-a" (third row))) rows)))
    ;; A keyword namespace is refused before READ runs, so it is never a
    ;; match -- which is why namespaces cross as strings (spec §4).
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:run-guarded-prolog
       "(claim ?c rt-claim :host \"h1\" ?r ?a ?b)" g))
    ;; RT-CLAIM-UNARY is a vertex type of this graph, so the guard admits
    ;; it; only the parent is a claim family, so CLAIM-FAMILY signals at
    ;; run time and %ILL-TYPED-CONDITION-P must classify it (GH #330).
    (signals graph-db.query:prolog-ill-typed-error
      (graph-db.query:run-guarded-prolog
       "(claim ?c rt-claim-unary \"host\" \"h1\" ?r ?a ?b)" g))))
