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

(defun observe-reaches (g from to)
  "An OBSERVED \"reaches\" claim under producer \"curator\": a base
fact of a stratum relation that no rule of the stratum wrote."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :node
                          :subject-key from :relation "reaches"
                          :object-namespace :node :object-key to
                          :producer "curator" :standing :observed)))

(defun reaches-from (g key)
  "The distinct current \"reaches\" object keys of subject KEY,
sorted; every producer's, the base facts included."
  (sort (remove-duplicates
         (mapcar #'claim-object-key
                 (remove-if-not
                  #'claim-current-p
                  (claims-touching g 'rt-claim :node key :role :subject
                                   :relation "reaches")))
         :test #'string=)
        #'string<))

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
relation directly rather than through RULE-DELTA/2, and an index read
does not see the open transaction's own writes, so on a from-scratch
single-store run it would see NONE of its own output (GH #333)."
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

(test a-recursive-goal-sees-a-base-fact-of-its-own-relation
  "Ruling R10 (C1): a recursive rule runs its variants only, so its
body reads its own relation through the delta -- and an OBSERVED
\"reaches\" c-d that no rule wrote is a premise for nothing unless
round 0's delta is seeded with the stratum's base facts.  Singly and
doubly recursive alike: \"next\" a-b and b-c seeded, \"reaches\" c-d
observed, so a reaches b, c and d, and b reaches c and d."
  (dolist (step (list *tc-step-body* *tc-step2-body*))
    (with-rules-graph (g)
      (link g "a" "b") (link g "b" "c")
      (observe-reaches g "c" "d")
      (write-rule g :name "tc-base" :version "1" :family "rt-claim"
                  :head *tc-base-head* :body *tc-base-body*)
      (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                  :head *tc-base-head* :body step)
      (let ((reports (graph-db.rules:run-rules g)))
        (is (every (lambda (r)
                     (eq :derived
                         (graph-db.rules:rule-report-outcome r)))
                   reports)))
      (is (equal '("b" "c" "d") (reaches-from g "a")))
      (is (equal '("c" "d") (reaches-from g "b")))
      ;; A seeded claim is a premise, never the producer's: no
      ;; reconcile keeps or sweeps it, and c derives nothing itself.
      (let ((base (claims-touching g 'rt-claim :node "c" :role :subject
                                   :relation "reaches")))
        (is (= 1 (length base)))
        (is (claim-current-p (first base)))
        (is (string= "curator" (claim-producer (first base))))))))

(test run-rule-on-the-non-recursive-member-runs-the-stratum
  "I2: TC-BASE reads only \"next\", so it is not itself recursive --
but its stratum is, and RUN-RULES runs the whole fixpoint for it.
RUN-RULE must agree, or the base case of a closure run alone derives
a partial answer and reports no stratum."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (write-closure g)
    (let ((report (graph-db.rules:run-rule g "tc-base")))
      (is (string= "tc-base"
                   (graph-db.rules:rule-report-rule-name report)))
      (is (equal '("tc-base" "tc-step")
                 (graph-db.rules:rule-report-stratum report)))
      (is (= 3 (graph-db.rules:rule-report-rounds report)))
      (is (equal '(("a" . "b") ("a" . "c")) (reaches g))))))

;;; The temporal closure: a kept claim's extent in the delta (I1).

(defparameter *tt-head*
  "(claim ?c rtt-claim \"node\" ?a \"reaches\" \"node\" ?b)")
(defparameter *tt-base-body*
  "(claim ?p rtt-claim \"node\" ?a \"next\" \"node\" ?b)
   (claim-producer ?p \"seed\")
   (claim-current ?p)")
(defparameter *tt-step-body*
  "(claim ?p rtt-claim \"node\" ?a \"next\" \"node\" ?m)
   (claim-producer ?p \"seed\")
   (claim-current ?p)
   (claim ?q rtt-claim \"node\" ?m \"reaches\" \"node\" ?b)")

(defun tlink (g from to extent)
  "A temporal \"next\" rtt-claim FROM -> TO over EXTENT, producer
\"seed\"."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rtt-claim-binary :graph g :subject-namespace :node
                           :subject-key from :relation "next"
                           :object-namespace :node :object-key to
                           :producer "seed" :standing :observed
                           :extent extent)))

(defun write-temporal-closure (g)
  "The two-rule transitive closure over the temporal family."
  (write-rule g :name "tt-base" :version "1" :family "rtt-claim"
              :head *tt-head* :body *tt-base-body*)
  (write-rule g :name "tt-step" :version "1" :family "rtt-claim"
              :head *tt-head* :body *tt-step-body*))

(defun reaches-end (g from to)
  "The end timestamp of the derived \"reaches\" FROM -> TO, or NIL."
  (let ((c (find-if (lambda (c)
                      (and (string= from (claim-subject-key c))
                           (string= to (claim-object-key c))))
                    (append (derived g 'rtt-claim "tt-base")
                            (derived g 'rtt-claim "tt-step")))))
    (and c (nth-value 1 (claim-bounds c)))))

(test a-kept-temporal-claims-extent-follows-its-premises-in-the-delta
  "I1: a kept identity enters the delta as the premise of everything a
later round derives from it, so its extent must be refreshed BEFORE it
does.  a-b, b-c, c-d all [Jan 1, Dec 31]; run; narrow b-c to
[Jan 1, Mar 31]; run again.  a-c and a-d are derived through the kept
b-c, so they end Mar 31 too -- with the stale extent in the delta they
keep the Dec 31 they were first derived with."
  (let ((wide (interval (ts 2026 1 1) (ts 2026 12 31)))
        (narrow (interval (ts 2026 1 1) (ts 2026 3 31))))
    (with-rules-graph (g)
      (tlink g "a" "b" wide)
      (tlink g "b" "c" wide)
      (tlink g "c" "d" wide)
      (write-temporal-closure g)
      (graph-db.rules:run-rules g)
      (is (local-time:timestamp= (ts 2026 12 31)
                                 (reaches-end g "a" "c")))
      (let ((bc (find "c" (claims-touching g 'rtt-claim :node "b"
                                           :role :subject
                                           :relation "next")
                      :key #'claim-object-key :test #'string=)))
        (with-transaction ((graph-db::transaction-manager g))
          (let ((c (copy bc)))
            (setf (claim-extent c) narrow)
            (save c))))
      (let ((reports (graph-db.rules:run-rules g)))
        (is (every (lambda (r)
                     (eq :derived
                         (graph-db.rules:rule-report-outcome r)))
                   reports)))
      ;; Everything reached THROUGH b-c ends where b-c now ends.
      (dolist (pair '(("a" "c") ("a" "d") ("b" "c") ("b" "d")))
        (is (local-time:timestamp=
             (ts 2026 3 31)
             (reaches-end g (first pair) (second pair)))))
      ;; a-b and c-d have no narrowed premise and stay wide.
      (is (local-time:timestamp= (ts 2026 12 31)
                                 (reaches-end g "a" "b")))
      (is (local-time:timestamp= (ts 2026 12 31)
                                 (reaches-end g "c" "d"))))))

(test disjoint-premises-accumulate-over-a-stratums-rounds
  "M2: %DESIRED owns REPORT's disjoint count and resets it per
evaluation, so a stratum accumulates it round by round.  a-b runs
[Jan 1, Mar 31] and b-c, c-d [Jul 1, Sep 30]: round 1 drops a-c and
round 2 drops a-d, both premises never holding at once, and the last
round alone would report one."
  (let ((winter (interval (ts 2026 1 1) (ts 2026 3 31)))
        (summer (interval (ts 2026 7 1) (ts 2026 9 30))))
    (with-rules-graph (g)
      (tlink g "a" "b" winter)
      (tlink g "b" "c" summer)
      (tlink g "c" "d" summer)
      (write-temporal-closure g)
      (let* ((reports (graph-db.rules:run-rules g))
             (base (report-named "tt-base" reports))
             (step (report-named "tt-step" reports)))
        (is (= 3 (graph-db.rules:rule-report-rounds step)))
        (is (= 3 (graph-db.rules:rule-report-derived base)))
        (is (= 1 (graph-db.rules:rule-report-derived step)))
        (is (= 2 (graph-db.rules:rule-report-disjoint-premises step)))
        (is (= 0 (graph-db.rules:rule-report-disjoint-premises
                  base)))))))

(test run-rule-on-a-lone-self-recursive-rule-runs-its-fixpoint
  "A self-recursive rule's stratum can be the rule alone, and RUN-RULE
must resolve its members anyway -- a stratum of one is a stratum, and
%RUN-STRATUM given none dereferences NIL.  With no base rule TC-STEP's
closure starts from the curator's observed \"reaches\" c-d: round 0
reaches b-d over next b-c, round 1 a-d over next a-b."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (observe-reaches g "c" "d")
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-step-body*)
    (let ((report (graph-db.rules:run-rule g "tc-step")))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (equal '("tc-step")
                 (graph-db.rules:rule-report-stratum report)))
      (is (= 3 (graph-db.rules:rule-report-rounds report)))
      (is (= 2 (graph-db.rules:rule-report-derived report)))
      (is (equal '("d") (reaches-from g "a")))
      (is (equal '("d") (reaches-from g "b"))))))

(test the-round-0-seed-answers-a-retracted-base-fact
  "The seed matches what CLAIM/7 answers, retracted claims included:
the goal a variant substitutes has to answer what the goal it replaces
would, or which of two recursive goals the fixpoint feeds would change
what the rule means.  A body that wants currency says CLAIM-CURRENT --
TC-STEP's does, of its \"next\" premise and not of its \"reaches\"
one, so the retracted c-d still carries the closure."
  (with-rules-graph (g)
    (link g "a" "b") (link g "b" "c")
    (observe-reaches g "c" "d")
    (with-transaction ((graph-db::transaction-manager g))
      (retract-claim (first (claims-touching g 'rt-claim :node "c"
                                             :role :subject
                                             :relation "reaches"))))
    (write-rule g :name "tc-step" :version "1" :family "rt-claim"
                :head *tc-base-head* :body *tc-step-body*)
    (let ((report (graph-db.rules:run-rule g "tc-step")))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 2 (graph-db.rules:rule-report-derived report)))
      (is (equal '("d") (reaches-from g "a")))
      (is (equal '("d") (reaches-from g "b"))))))
