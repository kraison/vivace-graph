;;;; tests/rules/fixpoint-tests.lisp -- recursive rules under fixpoint
;;;; iteration (GH #333, spec §3).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defun link (g from to)
  "A \"next\" rt-claim from node FROM to node TO, producer \"seed\"."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :node
                          :subject-key from :relation "next"
                          :object-namespace :node :object-key to
                          :producer "seed" :standing :observed)))

(defparameter *tc-base-head*
  "(claim ?c rt-claim \"node\" ?a \"reaches\" \"node\" ?b)")
;; Both endpoints of the "next" goal are unbound (?a and ?b/?m), which
;; is cost-unbounded under a resource bound (GH #285) unless something
;; routes it; CLAIM-PRODUCER "seed" does, since LINK writes every
;; "next" claim with that producer -- the same fix
;; AN-UNROUTED-BODY-GOAL-IS-REFUSED-NOT-WALKED (run-tests.lisp) names.
;; CLAIM-CURRENT on ?p: CLAIM/7 answers retracted-and-all (spec §4;
;; facts-tests.lisp CLAIM-CURRENT-FILTERS-A-RETRACTED-CLAIM), so a rule
;; that never checks it still "sees" a retracted premise.
(defparameter *tc-base-body*
  "(claim ?p rt-claim \"node\" ?a \"next\" \"node\" ?b)
   (claim-producer ?p \"seed\")
   (claim-current ?p)")
(defparameter *tc-step-body*
  "(claim ?p rt-claim \"node\" ?a \"next\" \"node\" ?m)
   (claim-producer ?p \"seed\")
   (claim-current ?p)
   (claim ?q rt-claim \"node\" ?m \"reaches\" \"node\" ?b)")
;; Doubly recursive: composes "reaches" with "reaches" instead of
;; "next" with "reaches".  Anchored -- ?a comes from a "next" edge --
;; so both recursive goals are index-routed (subject+relation bound):
;; neither hits %UNBOUND-CLAIM-SCAN, isolating C1 (two-or-more
;; recursive goals derive nothing) from I1 (an unrouted goal refuses).
(defparameter *tc-step2-body*
  "(claim ?p rt-claim \"node\" ?a \"next\" \"node\" ?z)
   (claim-producer ?p \"seed\")
   (claim-current ?p)
   (claim ?q rt-claim \"node\" ?a \"reaches\" \"node\" ?m)
   (claim ?r rt-claim \"node\" ?m \"reaches\" \"node\" ?b)")
;; Unanchored: neither "reaches" goal's subject is bound by anything
;; but the other, so a plain (round 0, pre-I1) evaluation would hit
;; %UNBOUND-CLAIM-SCAN and refuse as cost-unbounded (GH #285) -- I1
;; makes round 0 use the delta variant too, whose RULE-DELTA/2 goal
;; fails first (an empty delta) and never reaches the scan.
(defparameter *tc-step-unanchored-body*
  "(claim ?q rt-claim \"node\" ?a \"reaches\" \"node\" ?m)
   (claim ?r rt-claim \"node\" ?m \"reaches\" \"node\" ?b)")

(defun write-closure (g)
  "The two-rule transitive closure of \"next\" into \"reaches\"."
  (write-rule g :name "tc-base" :version "1" :family "rt-claim"
              :head *tc-base-head* :body *tc-base-body*)
  (write-rule g :name "tc-step" :version "1" :family "rt-claim"
              :head *tc-base-head* :body *tc-step-body*))

(defun reaches (g)
  "The distinct current \"reaches\" pairs, sorted.  TC-BASE and TC-STEP
can each independently derive the same (subject . object) tuple -- a
cyclic \"next\" graph makes a direct edge also reachable through the
cycle -- as two claims under two producers, so this dedupes by tuple,
the question a caller means by \"what does the graph reach\"."
  (remove-duplicates
   (sort (mapcar (lambda (c) (cons (claim-subject-key c)
                                   (claim-object-key c)))
                 (remove-if-not
                  #'claim-current-p
                  (claims-touching g 'rt-claim :node "a" :role :subject
                                   :relation "reaches")))
         #'string< :key #'cdr)
   :test #'equal))

(test a-transitive-closure-over-a-cycle-terminates-with-the-fixpoint
  "a -> b -> c -> a, and c -> d: 12 reaches pairs.  A top-down rule
would loop; the fixpoint terminates."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "a") (link g "c" "d")
    (write-closure g)
    (let* ((reports (graph-db.rules:run-rules g))
           (base (report-named "tc-base" reports))
           (step (report-named "tc-step" reports)))
      (is (every (lambda (r)
                   (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (equal '("tc-base" "tc-step")
                 (graph-db.rules:rule-report-stratum base)))
      (is (= (graph-db.rules:rule-report-rounds base)
             (graph-db.rules:rule-report-rounds step)))
      (is (<= 3 (graph-db.rules:rule-report-rounds step) 5))
      ;; From each of a b c: a b c d (the cycle plus d); d reaches
      ;; nothing: 12 DISTINCT pairs.  But TC-BASE's 4 direct edges are
      ;; each ALSO reachable the long way around the cycle, so TC-STEP
      ;; derives all 12 pairs on its own (a separate producer, a
      ;; separate claim) and TC-BASE derives its 4 again: 16 total
      ;; constructed, 12 distinct once REACHES dedupes by tuple.
      (is (= 16 (+ (graph-db.rules:rule-report-derived base)
                   (graph-db.rules:rule-report-derived step))))
      (is (equal '(("a" . "a") ("a" . "b") ("a" . "c") ("a" . "d"))
                 (reaches g))))))

(test the-semi-naive-delta-derives-what-naive-re-evaluation-derives
  "The delta variants are an optimisation, never a different answer:
the identity set equals what a full re-evaluation each round gives.
Naive mode is only a valid reference here because it runs SECOND, over
a closure the delta-based run already committed: it reads the
relation directly rather than through RULE-DELTA/2, so on a from-
scratch run it would only ever see round 0's own output (GH #333)."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d") (link g "d" "b")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (let ((semi (reaches g)))
      (let ((graph-db.rules::*rules-naive-rounds* t))
        (graph-db.rules:run-rules g))
      (is (equal semi (reaches g)))
      (is (= 3 (length semi))))))

(test rounds-count-the-chain-and-a-round-0-claim-survives
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (let* ((reports (graph-db.rules:run-rules g))
           (base (report-named "tc-base" reports))
           (step (report-named "tc-step" reports)))
      ;; A plain CLAIM/7 read resolves through an index that only sees
      ;; this transaction's state as of its start, so TC-STEP's round
      ;; 0 (a plain read of "reaches") cannot see TC-BASE's round 0
      ;; writes, however the two are ordered: round 0 is TC-BASE's a-b
      ;; b-c c-d alone; round 1 uses that delta (RULE-DELTA/2, an
      ;; in-memory reference, not an index read) for a-c b-d; round 2
      ;; uses THAT delta for a-d; round 3 finds nothing.  4 rounds, 3 +
      ;; 3 = 6 derived (recon, GH #333).
      (is (= 4 (graph-db.rules:rule-report-rounds step)))
      (is (equal '(("a" . "b") ("a" . "c") ("a" . "d")) (reaches g)))
      (is (= 6 (+ (graph-db.rules:rule-report-derived base)
                  (graph-db.rules:rule-report-derived step))))
      (is (= 0 (+ (graph-db.rules:rule-report-kept base)
                  (graph-db.rules:rule-report-kept step))))
      (is (= 0 (+ (graph-db.rules:rule-report-swept base)
                  (graph-db.rules:rule-report-swept step)))))))

(test a-premise-retracted-between-runs-sweeps-exactly-the-stale-closure
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (is (= 3 (length (reaches g))))
    (with-transaction ((graph-db::transaction-manager g))
      (retract-claim (first (claims-touching g 'rt-claim :node "c"
                                             :role :subject
                                             :relation "next"))))
    (let* ((reports (graph-db.rules:run-rules g))
           (base (report-named "tc-base" reports))
           (step (report-named "tc-step" reports)))
      (is (equal '(("a" . "b") ("a" . "c")) (reaches g)))
      ;; b can no longer reach d either (its only route was b-c-d), so
      ;; TC-STEP sweeps BOTH a-d and b-d, not only the pair REACHES
      ;; shows (REACHES only ever asks about node "a"): TC-BASE sweeps
      ;; its own c-d (1), TC-STEP sweeps a-d and b-d (2).  3 total; a-c
      ;; and a-b/b-c are each kept, not re-derived (0 derived).
      (is (= 0 (+ (graph-db.rules:rule-report-derived base)
                  (graph-db.rules:rule-report-derived step))))
      (is (= 3 (+ (graph-db.rules:rule-report-kept base)
                  (graph-db.rules:rule-report-kept step))))
      (is (= 3 (+ (graph-db.rules:rule-report-swept base)
                  (graph-db.rules:rule-report-swept step)))))))

(test run-rule-on-one-rule-of-a-stratum-runs-the-stratum
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (write-closure g)
    (let ((report (graph-db.rules:run-rule g "tc-step")))
      (is (string= "tc-step" (graph-db.rules:rule-report-rule-name report)))
      (is (equal '("tc-base" "tc-step")
                 (graph-db.rules:rule-report-stratum report)))
      (is (equal '(("a" . "b") ("a" . "c")) (reaches g))))))

(test the-rounds-cap-refuses-naming-the-count
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-closure g)
    (let* ((graph-db.rules:*rules-max-rounds* 1)
           (reports (graph-db.rules:run-rules g)))
      (is (every (lambda (r)
                   (eq :refused (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (search "1" (refusal-text (report-named "tc-step" reports))))
      ;; Single-store: one transaction, so nothing landed.
      (is (null (reaches g))))))

(test a-budget-refusal-in-a-later-round-leaves-the-previous-derivation
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (write-closure g)
    (graph-db.rules:run-rules g)
    (is (= 2 (length (reaches g))))
    (link g "c" "d")
    (let* ((graph-db.rules:*rules-max-inferences* 1)
           (reports (graph-db.rules:run-rules g)))
      (is (eq :refused (graph-db.rules:rule-report-outcome
                        (report-named "tc-step" reports))))
      (is (eq :budget (refusal-tag (report-named "tc-step" reports))))
      (is (= 2 (length (reaches g)))))))

(test two-recursive-goals-still-reach-the-whole-closure
  "C1: a rule with more than one recursive CLAIM/7 goal must still
reach the whole closure -- %VARIANTS substitutes one recursive goal at
a time, so the OTHER stays a plain read; without
*CLAIM-DERIVED-THIS-RUN* that plain read is excluded and finds
nothing, every round, forever."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-rule g :name "tc-base" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-base-body*)
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-step2-body*)
    (let* ((reports (graph-db.rules:run-rules g))
           (step (report-named "tc-step" reports)))
      (is (eq :derived (graph-db.rules:rule-report-outcome step)))
      (is (= 3 (graph-db.rules:rule-report-derived step)))
      (is (= 4 (graph-db.rules:rule-report-rounds step)))
      (is (equal '(("a" . "b") ("a" . "c") ("a" . "d")) (reaches g))))))

(test the-unanchored-closure-step-is-not-refused
  "I1: round 0 must run a recursive rule's %VARIANTS too, not its
literal body -- an empty delta fails the RULE-DELTA/2 goal before
CLAIM/7 ever reaches the unbound scan, so an unanchored T(x,z) :-
T(x,y), T(y,z) style step compiles and runs instead of refusing as
cost-unbounded (GH #285)."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-rule g :name "tc-base" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-base-body*)
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-step-unanchored-body*)
    (let ((reports (graph-db.rules:run-rules g)))
      (is (every (lambda (r)
                   (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (equal '(("a" . "b") ("a" . "c") ("a" . "d")) (reaches g))))))

(test run-rule-on-a-disabled-rule-is-refused-and-writes-nothing
  "I2: a disabled rule must never derive via RUN-RULE, checked before
any compile, whether or not it turns out to read its own stratum."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-rule g :name "tc-base" :version "1" :family "rt-claim"
                :enabled nil :head *tc-base-head* :body *tc-base-body*)
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-step-body*)
    (let ((report (graph-db.rules:run-rule g "tc-base")))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq :rule (refusal-tag report)))
      (is (null (reaches g))))))

(test run-rule-on-a-disabled-recursive-rule-is-refused
  "The deeper case I2 names in its prose: TC-STEP disabled but still
structurally part of TC-BASE's stratum, so run-rule's recursive path
must refuse it rather than fall back to a stale, all-zero :DERIVED
report -- TC-STEP is absent from RULES-IN-SCOPE (disabled), so the
member search that used to feed %RUN-STRATUM silently excluded the
very rule being asked for."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c") (link g "c" "d")
    (write-rule g :name "tc-base" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-base-body*)
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :enabled nil :head *tc-base-head* :body *tc-step-body*)
    (let ((report (graph-db.rules:run-rule g "tc-step")))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq :rule (refusal-tag report)))
      (is (null (reaches g))))))
