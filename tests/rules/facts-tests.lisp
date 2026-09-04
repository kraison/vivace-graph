;;;; tests/rules/facts-tests.lisp -- claims as Prolog facts (spec §4,
;;;; GH #330).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

;; Every goal that must take an index route runs under a budget.  Without
;; one the family walk is legal (R7) and answers the same rows, so a route
;; that silently degraded to it would pass; with one the walk refuses, so
;; only the route can pass.  Under the guard a budget is always in effect,
;; which is what makes that degradation a user-visible refusal.

(test claim-generates-from-the-subject-relation-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (:max-inferences 1000) (?o)
                  (claim ?c rt-claim "host" "h1" "runs" "app" ?o))))
      (is (equal '("db" "web")
                 (sort (mapcar #'first rows) #'string<))))))

;; The subject index is the route only while the relation is unbound; a
;; bound one takes the row above.  Both answer the same rows -- the
;; subject index is a prefix of the subject-relation one and
;; %UNIFY-CLAIM filters the relation either way -- so this guards route
;; SELECTION, not correctness: it is red only if the subject-index
;; clause itself goes, when the goal falls to the walk and the budget
;; refuses it (docs/rules.md).
(test claim-generates-from-the-subject-index
  (with-rules-graph (g)
    (seed g)
    (let ((rows (select (:max-inferences 1000) (?r ?o)
                  (claim ?c rt-claim "host" "h1" ?r ?ons ?o))))
      (is (equal '(("runs" "db") ("runs" "web"))
                 (sort (copy-list rows) #'string< :key #'second))))))

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

;; The refusal is a property of the goal's shape, not of the
;; nothing-bound shape alone.  The namespace is the leading slot of both
;; endpoint indexes and an index is usable only from a prefix, so
;; neither half of a subject pair routes on its own (docs/rules.md).
(test an-unrouted-goal-is-refused-under-a-bound
  (with-rules-graph (g)
    (seed g)
    (signals graph-db::prolog-cost-unbounded-error
      (select (:max-inferences 1000) (?c)
        (claim ?c rt-claim "host" ?k ?r ?a ?b)))
    (signals graph-db::prolog-cost-unbounded-error
      (select (:max-inferences 1000) (?c)
        (claim ?c rt-claim ?ns "h1" ?r ?a ?b)))
    ;; Control: the same pair bound together routes and answers under
    ;; the very same budget.
    (is (= 2 (select (:count t :max-inferences 1000) (?c)
               (claim ?c rt-claim "host" "h1" ?r ?a ?b))))))

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
                                (claim-valid-at ?c "2026-02-15T00:00:00Z"))))
    ;; A malformed instant fails the goal; reaching here at all is the
    ;; assertion that it did not signal.
    (is (null (select-flat (?v) (claim ?c rtt-claim "app" "web"
                                       "version" "ver" ?v)
                                (claim-valid-at ?c "not-a-timestamp"))))
    ;; %INSTANT-ARG's timestamp branch: a Lisp caller passes a
    ;; LOCAL-TIME timestamp, not the wire string, and gets the same row.
    (is (equal '("1")
               (select-flat (?v) (lisp ?at (ts 2026 2 15))
                                 (claim ?c rtt-claim "app" "web"
                                        "version" "ver" ?v)
                                 (claim-valid-at ?c ?at))))))

;; Spec §11: the functor and CLAIMS-TOUCHING :AT must answer the same
;; claims for the same instant.  They share %CLAIM-VALIDITY-TOUCHES-P,
;; and this is the assertion that goes red if someone inlines it back.
;; The pinned ("1") is what keeps the agreement from holding vacuously
;; on two empty results.  Sorted, because the two sides build their
;; lists in different orders and only membership was ever the
;; requirement.
(test claim-valid-at-agrees-with-claims-touching
  (with-rules-graph (g)
    (seed g)
    (let ((via-goal (select-flat (?k) (claim ?c rtt-claim "app" "web"
                                             "version" "ver" ?k)
                                      (claim-valid-at
                                       ?c "2026-02-15T00:00:00Z")))
          (via-query (mapcar #'claim-object-key
                             (claims-touching g 'rtt-claim :app "web"
                                              :role :subject
                                              :at (ts 2026 2 15)))))
      (is (equal '("1") via-goal))
      (is (equal (sort (copy-list via-goal) #'string<)
                 (sort (copy-list via-query) #'string<))))))

;; Spec §11's other half, which had no differential test: CLAIM-CURRENT/1
;; and CLAIMS-TOUCHING :CURRENT must keep the same claims.  Both call
;; CLAIM-CURRENT-P today; this goes red if one of them stops.  ("runs")
;; is pinned first so the agreement cannot hold on two empty lists.
(test claim-current-agrees-with-claims-touching
  (with-rules-graph (g)
    (seed g)
    (let ((c (first (claims-touching g 'rt-claim :host "h2" :role :subject
                                     :relation "reachable"))))
      (retract-claim c))
    (let ((via-goal (select-flat (?r) (claim ?c rt-claim "host" "h2"
                                             ?r ?a ?b)
                                      (claim-current ?c)))
          (via-query (mapcar #'claim-relation
                             (claims-touching g 'rt-claim :host "h2"
                                              :role :subject :current t))))
      (is (equal '("runs") via-goal))
      (is (equal (sort (copy-list via-goal) #'string<)
                 (sort (copy-list via-query) #'string<))))))

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

;; R22: %PRODUCER-CANDIDATES walks the image-wide family registry, so it
;; meets families this graph's schema does not carry.  RTF-CLAIM is one
;; (suite.lisp); the SIGNALS is the probe that the lookup really does
;; raise here, so the count below runs through the HANDLER-CASE rather
;; than past it.
(test the-producer-generator-skips-a-family-this-graph-lacks
  (with-rules-graph (g)
    (seed g)
    (is (claim-family 'rtf-claim))
    (signals graph-db:query-precondition-error
      (graph-db:index-lookup g 'rtf-claim
                             graph-db::+claim-producer-index-slots+
                             "scan-a"))
    (is (= 2 (select (:count t :max-inferences 1000) (?c)
               (claim-producer ?c "scan-b"))))))

;; Neither argument bound is unroutable, and an unroutable generator
;; that answers zero rows in silence is the one thing CLAIM/7 already
;; refuses to be.  Under a bound it refuses the same way; with no bound
;; there is no producer walk to offer, so it still just fails.
(test claim-producer-with-neither-argument-bound
  (with-rules-graph (g)
    (seed g)
    (signals graph-db::prolog-cost-unbounded-error
      (select (:max-inferences 1000) (?c) (claim-producer ?c ?p)))
    (is (zerop (select-count (?c) (claim-producer ?c ?p))))))

;; The interface contract for all six filters: a bound non-node ?C fails
;; the goal, and none of them signals -- reaching the end of the test is
;; the "never signals" half.  The last line is A1's shape: %CLAIM-ARG is
;; NIL for a bound non-node, so without the unbound-?C gate the
;; generator ran a whole cross-family lookup here and then unified
;; nothing.  That gate changes cost, not answers, so this pins the
;; contract; it does not prove the gate.
(test a-non-node-c-fails-every-filter
  (with-rules-graph (g)
    (seed g)
    (is (zerop (select-count () (claim-current "x"))))
    (is (zerop (select-count ()
                             (claim-valid-at "x" "2026-02-15T00:00:00Z"))))
    (is (zerop (select-count () (claim-standing "x" ?s))))
    (is (zerop (select-count () (claim-relation "x" ?r))))
    (is (zerop (select-count () (claim-rule-version "x" ?v))))
    (is (zerop (select-count () (claim-producer "x" ?p))))
    (is (zerop (select-count () (claim-producer "x" "scan-a"))))))

;; The bound-second-argument half of each filter's docstring -- the mode
;; an S2 rule body writes.  Every pair names both the row the filter
;; keeps and the row it must drop, so a filter that stopped filtering
;; fails the second assertion of its pair rather than passing both.
(test the-filters-filter-on-a-bound-second-argument
  (with-rules-graph (g)
    (seed g)
    ;; h2 carries two claims: "reachable" is :inferred, "runs" :observed.
    (is (equal '("reachable")
               (select-flat (?r) (claim ?c rt-claim "host" "h2" ?r ?a ?b)
                                 (claim-standing ?c "inferred"))))
    (is (equal '("runs")
               (select-flat (?r) (claim ?c rt-claim "host" "h2" ?r ?a ?b)
                                 (claim-standing ?c "observed"))))
    ;; h1 carries two "runs" claims and nothing else.
    (is (= 2 (select-count (?c) (claim ?c rt-claim "host" "h1" ?s ?a ?b)
                                (claim-relation ?c "runs"))))
    (is (zerop (select-count (?c) (claim ?c rt-claim "host" "h1" ?s ?a ?b)
                                  (claim-relation ?c "reachable"))))
    ;; No rule wrote these, so NIL is the value that filters them IN.
    (is (= 2 (select-count (?c) (claim ?c rt-claim "host" "h1" ?s ?a ?b)
                                (claim-rule-version ?c nil))))
    (is (zerop (select-count (?c) (claim ?c rt-claim "host" "h1" ?s ?a ?b)
                                  (claim-rule-version ?c "v1"))))
    ;; Both arguments bound is CLAIM-PRODUCER/2's filter half; the two
    ;; claims on the object route split between the producers.
    (is (equal '("h1")
               (select-flat (?k) (claim ?c rt-claim "host" ?k "runs"
                                        "app" "web")
                                 (claim-producer ?c "scan-a"))))
    (is (equal '("h2")
               (select-flat (?k) (claim ?c rt-claim "host" ?k "runs"
                                        "app" "web")
                                 (claim-producer ?c "scan-b"))))))

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
