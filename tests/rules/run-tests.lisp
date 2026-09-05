;;;; tests/rules/run-tests.lisp -- run-rule, provenance, validity
;;;; (spec §7-§9, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defun derived (g family name)
  "The live claims rule NAME derived into FAMILY, current only."
  (remove-if-not #'claim-current-p
                 (claims-by-producer g family
                                     (graph-db.rules::rule-producer name))))

(defun refusal-tag (report)
  "The TAG of REPORT's first refusal."
  (car (first (graph-db.rules:rule-report-refusals report))))

(defun refusal-text (report)
  "The TEXT of REPORT's first refusal."
  (cdr (first (graph-db.rules:rule-report-refusals report))))

(defun report-named (name reports)
  "The RULE-REPORT for rule NAME among REPORTS, or NIL."
  (find name reports :key #'graph-db.rules:rule-report-rule-name
                     :test #'string=))

(defun write-web-hosts (g &rest args)
  (apply #'write-rule g :name "web-hosts" :version "1" :family "rt-claim"
         :head *web-hosts-head* :body *web-hosts-body* args))

(test run-rule-derives-claims-that-name-the-rule
  (with-rules-graph (g)
    (seed g)
    (let* ((r (write-web-hosts g))
           (report (graph-db.rules:run-rule g r)))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 2 (graph-db.rules:rule-report-derived report)))
      (is (= 0 (graph-db.rules:rule-report-swept report)))
      (is (string= "web-hosts"
                   (graph-db.rules:rule-report-rule-name report)))
      (let ((claims (derived g 'rt-claim "web-hosts")))
        (is (= 2 (length claims)))
        (is (equal '("h1" "h2")
                   (sort (mapcar #'claim-object-key claims) #'string<)))
        (dolist (c claims)
          (is (string= "rule/web-hosts" (claim-producer c)))
          (is (string= "1" (claim-rule-version c)))
          (is (eq :inferred (claim-standing c)))
          (is (string= "hosted-on" (claim-relation c)))
          (is (eq :app (claim-subject-namespace c)))
          (is (eq :host (claim-object-namespace c)))
          ;; rt-claim premises carry no extent, so neither does this.
          (is (null (claim-extent c)))))
      ;; The derived claims are Prolog facts like any other.
      (is (= 2 (select (:count t :max-inferences 1000) (?h)
                 (claim ?c rt-claim "app" "web" "hosted-on" "host" ?h)))))))

(test rerunning-sweeps-the-previous-derivation
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      ;; A premise goes away; the rerun must not keep h2.
      (with-transaction ((graph-db::transaction-manager g))
        (mark-deleted (first (claims-touching g 'rt-claim :host "h2"
                                              :role :subject
                                              :relation "runs"))))
      (let ((before (graph-db:id
                     (find "h1" (derived g 'rt-claim "web-hosts")
                           :key #'claim-object-key :test #'string=)))
            (report (graph-db.rules:run-rule g r)))
        ;; Ruling P10: h1 is re-derived and kept -- same node -- h2 is
        ;; not and is swept; nothing is constructed.
        (is (= 1 (graph-db.rules:rule-report-kept report)))
        (is (= 1 (graph-db.rules:rule-report-swept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report)))
        (is (equal '("h1") (mapcar #'claim-object-key
                                   (derived g 'rt-claim "web-hosts"))))
        (is (equalp before
                    (graph-db:id
                     (first (derived g 'rt-claim "web-hosts")))))
        ;; The old derivation records went with the old claims.
        (is (= 1 (length (claims-by-producer
                          g 'graph-db.rules:derivation
                          "rule/web-hosts"))))))))

(test a-new-version-leaves-no-old-version-claim
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      (with-transaction ((graph-db::transaction-manager g))
        (let ((c (copy r)))
          (setf (graph-db.rules:rule-version c) "2")
          (save c)))
      (let ((report (graph-db.rules:run-rule g "web-hosts")))
        (is (string= "2" (graph-db.rules:rule-report-version report)))
        ;; Ruling P10: the identities are unchanged, so the claims are
        ;; kept and their version refreshed, not rebuilt.
        (is (= 2 (graph-db.rules:rule-report-kept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report))))
      (let ((claims (derived g 'rt-claim "web-hosts")))
        (is (= 2 (length claims)))
        (is (every (lambda (c) (string= "2" (claim-rule-version c)))
                   claims))
        (is (zerop (select-count (?c)
                     (claim-producer ?c "rule/web-hosts")
                     (claim-rule-version ?c "1")
                     (claim-current ?c))))))))

(test provenance-names-the-premises-and-dependents-are-findable
  (with-rules-graph (g)
    (seed g)
    (graph-db.rules:run-rule g (write-web-hosts g))
    (let* ((d (find "h1" (derived g 'rt-claim "web-hosts")
                    :key #'claim-object-key :test #'string=))
           ;; h1 runs web AND db, so the premise is named by its
           ;; object; (first ...) would be either one.
           (premise (find "web"
                          (claims-touching g 'rt-claim :host "h1"
                                           :role :subject
                                           :relation "runs")
                          :key #'claim-object-key :test #'string=))
           (premises (graph-db.rules:premises-of g d)))
      ;; h1 runs web is the one premise of "web hosted-on h1".
      (is (= 1 (length premises)))
      (is (string= (claim-identity-key premise)
                   (claim-identity-key (first premises))))
      (is (string= "web" (claim-object-key (first premises))))
      ;; The derivation record itself: (:claim derived) derived-from
      ;; (:claim premise), by the rule.
      (let ((recs (claims-touching g 'graph-db.rules:derivation
                                   :claim (claim-identity-key d)
                                   :role :subject)))
        (is (= 1 (length recs)))
        (is (string= "derived-from" (claim-relation (first recs))))
        (is (string= "rule/web-hosts" (claim-producer (first recs))))
        (is (string= "1" (claim-rule-version (first recs)))))
      ;; Retract the premise: its dependents are still findable, and
      ;; nothing was re-derived.
      (retract-claim premise)
      (let ((deps (graph-db.rules:dependents-of g premise)))
        (is (= 1 (length deps)))
        (is (string= (claim-identity-key d)
                     (claim-identity-key (first deps))))
        (is-true (claim-current-p (first deps))))
      ;; Control: a claim nothing derived from has no dependents.
      (is (null (graph-db.rules:dependents-of g d))))))

(defparameter *host-version-head*
  "(claim ?c rtt-claim \"host\" ?h \"runs-version\" \"ver\" ?v)")
(defparameter *host-version-body*
  "(claim ?d rtt-claim \"app\" \"web\" \"deployed-on\" \"host\" ?h)
   (claim ?r rtt-claim \"app\" \"web\" \"version\" \"ver\" ?v)")

(defun write-host-version (g)
  (write-rule g :name "host-version" :version "1" :family "rtt-claim"
              :head *host-version-head* :body *host-version-body*))

(defun claim-bounds (c)
  "The exact start and end timestamps of C's extent."
  (let ((e (claim-extent c)))
    (values (bound-earliest (extent-start e))
            (bound-latest (extent-end e)))))

(test validity-is-the-intersection-of-the-premises
  "Spec §8.  SEED-TEMPORAL deploys web on h1 over [Feb 1, Jun 30] and
[Aug 1, Sep 30], on h2 over [May 1, May 31]; SEED runs version 1 over
[Jan 1, Mar 31] and version 2 over [Apr 1, Dec 31].  Six solutions,
four of which intersect -- (h1,1) [Feb 1, Mar 31], (h1,2)
[Apr 1, Jun 30], (h1,2) [Aug 1, Sep 30], (h2,2) [May 1, May 31] -- and
two of which are disjoint: h1's August run against version 1, and h2's
May run against version 1."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((report (graph-db.rules:run-rule g (write-host-version g))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 4 (graph-db.rules:rule-report-derived report)))
      (is (= 2 (graph-db.rules:rule-report-disjoint-premises report)))
      (let ((claims (derived g 'rtt-claim "host-version")))
        (is (= 4 (length claims)))
        ;; Not named RUN: FIVEAM exports one and the package is locked.
        (flet ((at (host ver start)
                 (find-if (lambda (c)
                            (and (string= host (claim-subject-key c))
                                 (string= ver (claim-object-key c))
                                 (local-time:timestamp=
                                  start (claim-bounds c))))
                          claims)))
          (let ((h1v1 (at "h1" "1" (ts 2026 2 1))))
            (is-true h1v1)
            (when h1v1
              (multiple-value-bind (s e) (claim-bounds h1v1)
                (is (local-time:timestamp= (ts 2026 2 1) s))
                (is (local-time:timestamp= (ts 2026 3 31) e)))
              (is (eq :validity (extent-semantics (claim-extent h1v1))))
              (is (eq :inferred (extent-standing (claim-extent h1v1))))
              ;; Two premises: the deployment and the version run.
              (is (= 2 (length (graph-db.rules:premises-of g h1v1))))))
          (is-true (at "h1" "2" (ts 2026 4 1)))
          (is-true (at "h1" "2" (ts 2026 8 1)))
          (let ((h2v2 (at "h2" "2" (ts 2026 5 1))))
            (is-true h2v2)
            (when h2v2
              (is (local-time:timestamp=
                   (ts 2026 5 31)
                   (nth-value 1 (claim-bounds h2v2))))))
          (is (null (find "1" (remove "h1" claims
                                      :key #'claim-subject-key
                                      :test #'string=)
                          :key #'claim-object-key :test #'string=))))
        ;; The derived claims answer CLAIM-VALID-AT like any temporal
        ;; claim.
        (is (equal '("1")
                   (select-flat (?v)
                     (claim ?c rtt-claim "host" "h1" "runs-version"
                            "ver" ?v)
                     (claim-valid-at ?c "2026-03-01T00:00:00Z"))))))))

(defparameter *hv-flat-head*
  "(claim ?c rt-claim \"host\" ?h \"ran-version\" \"ver\" ?v)")
(defparameter *hv-when-head*
  "(claim ?c rt-claim \"host\" ?h \"ran-version-when\" \"ver\" ?v)")

(test extent-policy-none-derives-without-an-extent
  "Ruling P7, on the six solutions the test above works out.  :NONE
never looks at an extent, so all six stand and collapse to the four
distinct (host, version) pairs.  :PREMISES on the same non-temporal
family drops the same two disjoint solutions and collapses the
remaining four to three, since a non-temporal identity ignores the
extent."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    ;; rt-claim is not temporal, so a claim without an extent is legal
    ;; there; the same premises, :none, no extent.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "hv-flat" :version "1"
                                 :family "rt-claim"
                                 :extent-policy :none
                                 :head *hv-flat-head*
                                 :body *host-version-body*))))
      (is (= 4 (graph-db.rules:rule-report-derived report)))
      (is (= 0 (graph-db.rules:rule-report-disjoint-premises report)))
      (is (every (lambda (c) (null (claim-extent c)))
                 (derived g 'rt-claim "hv-flat"))))
    ;; Control: :premises on the same non-temporal family attaches the
    ;; intersection and drops the disjoint pairs (ruling P7).
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "hv-when" :version "1"
                                 :family "rt-claim"
                                 :head *hv-when-head*
                                 :body *host-version-body*))))
      ;; Identity ignores the extent here, so (h1,2) collapses to one.
      (is (= 3 (graph-db.rules:rule-report-derived report)))
      (is (= 2 (graph-db.rules:rule-report-disjoint-premises report)))
      (is (every #'claim-extent (derived g 'rt-claim "hv-when"))))))

(defparameter *no-extent-head*
  "(claim ?c rtt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")

(test a-temporal-family-refuses-a-derivation-with-no-extent
  (with-rules-graph (g)
    (seed g)
    ;; Premises without extents into a temporal family: nothing to
    ;; intersect, so the constructor refuses, and the report names it.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "no-extent" :version "1"
                                 :family "rtt-claim"
                                 :head *no-extent-head*
                                 :body *web-hosts-body*))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (= 0 (graph-db.rules:rule-report-derived report)))
      (is (eq 'rtt-claim (refusal-tag report)))
      (is (search "extent" (refusal-text report) :test #'char-equal))
      (is (null (derived g 'rtt-claim "no-extent"))))))

(test a-refused-derivation-leaves-the-previous-one-intact
  "Spec §7.5: the sweep unwinds with the refusal."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((r (write-host-version g)))
      (graph-db.rules:run-rule g r)
      (let ((before (mapcar #'claim-identity-key
                            (derived g 'rtt-claim "host-version"))))
        (is (= 4 (length before)))
        ;; A second producer's version 2 run overlapping scan-a's makes
        ;; (h1, 2) derivable twice with overlapping validity: one base
        ;; tuple, two live runs, EXTENT-DISJOINTNESS-VIOLATION at
        ;; commit.
        (with-transaction ((graph-db::transaction-manager g))
          (make-rtt-claim-binary :graph g :subject-namespace :app
                                 :subject-key "web" :relation "version"
                                 :object-namespace :ver :object-key "2"
                                 :producer "scan-c" :standing :observed
                                 :extent (interval (ts 2026 3 1)
                                                   (ts 2026 4 30))))
        (let ((report (graph-db.rules:run-rule g r)))
          (is (eq :refused (graph-db.rules:rule-report-outcome report)))
          (is (= 0 (graph-db.rules:rule-report-swept report)))
          (is (eq 'rtt-claim (refusal-tag report)))
          (is (search "overlapping" (refusal-text report)
                      :test #'char-equal)))
        (is (equal (sort (copy-list before) #'string<)
                   (sort (mapcar #'claim-identity-key
                                 (derived g 'rtt-claim "host-version"))
                         #'string<)))
        (is (= 8 (length (claims-by-producer
                          g 'graph-db.rules:derivation
                          "rule/host-version"))))))))

(defparameter *walker-head*
  "(claim ?c rt-claim \"host\" ?h \"has-app\" \"app\" ?a)")
(defparameter *walker-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" ?ons ?a)")
(defparameter *routed-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" ?ons ?a)
   (claim-producer ?p \"scan-a\")")

(test an-unrouted-body-goal-is-refused-not-walked
  "Ruling P4: RUN-RULE always binds a budget, so the family walk is
refused inside the transaction rather than joining every claim to the
read set.  The rule budgets fall back to the DSL's, so the last clause
nulls both pairs to reach the unbounded case at all."
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "walker" :version "1"
                                 :family "rt-claim"
                                 :head *walker-head*
                                 :body *walker-body*))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq :budget (refusal-tag report)))
      (is (search "cost-unbounded" (refusal-text report)
                  :test #'char-equal)))
    ;; Control: the same rule with the producer generator in front
    ;; routes.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "routed" :version "1"
                                 :family "rt-claim"
                                 :head *walker-head*
                                 :body *routed-body*))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 2 (graph-db.rules:rule-report-derived report))))
    ;; An exhausted budget is the same refusal class.
    (let ((graph-db.rules:*rules-max-inferences* 1))
      (is (eq :budget
              (refusal-tag (graph-db.rules:run-rule g "routed")))))
    ;; No bound at all is an operator error, not a report.
    (let ((graph-db.rules:*rules-max-inferences* nil)
          (graph-db.rules:*rules-timeout* nil)
          (graph-db::*query-default-max-inferences* nil)
          (graph-db::*query-default-timeout* nil))
      (signals error (graph-db.rules:run-rule g "routed")))))

(defparameter *effecting-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (retract ?p)")

(test an-effecting-body-goal-is-refused-at-run
  "Recon A16 / ruling PF2: the guard has no static effect registry, so
(retract ?p) compiles and is refused when the goal runs with effects
off; the report names the rule, not the budget."
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "effecting" :version "1"
                                 :family "rt-claim"
                                 :head *web-hosts-head*
                                 :body *effecting-body*))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq :rule (refusal-tag report)))
      (is (search "permitted" (refusal-text report) :test #'char-equal))
      ;; And nothing was retracted: the premise is still current.
      (is (every #'claim-current-p
                 (claims-touching g 'rt-claim :host "h1" :role :subject
                                  :relation "runs"))))))

(test a-stored-rule-and-a-def-rule-with-one-text-derive-one-set
  (with-rules-graph (g)
    (seed g)
    (let* ((r (write-web-hosts g))
           (stored (progn (graph-db.rules:run-rule g r)
                          (sort (mapcar #'claim-identity-key
                                        (derived g 'rt-claim "web-hosts"))
                                #'string<))))
      (is (= 2 (length stored)))
      ;; Same name is a collision while the record exists; delete it.
      (with-transaction ((graph-db::transaction-manager g))
        (mark-deleted r))
      (graph-db.rules:def-rule "web-hosts" :version "1" :family rt-claim
        :head *web-hosts-head* :body *web-hosts-body*)
      (unwind-protect
           (let ((report (graph-db.rules:run-rule g "web-hosts")))
             (is (= 2 (graph-db.rules:rule-report-kept report)))
             (is (= 0 (graph-db.rules:rule-report-swept report)))
             (is (equal stored
                        (sort (mapcar #'claim-identity-key
                                      (derived g 'rt-claim "web-hosts"))
                              #'string<))))
        (graph-db.rules:undef-rule "web-hosts")))))

(defparameter *hosts-web-head*
  "(claim ?c rt-claim \"host\" ?h \"hosts-web\" nil nil)")
(defparameter *hosts-web-body*
  "(claim ?x rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")
(defparameter *parked-head*
  "(claim ?c rt-claim \"app\" \"web\" \"parked\" \"host\" ?h)")

(test run-rules-runs-in-dependency-order-and-skips-the-disabled
  (with-rules-graph (g)
    (seed g)
    ;; Written in the wrong order: "hosts-web" reads what "web-hosts"
    ;; derives, and a disabled third rule is compiled but not run.
    (write-rule g :name "hosts-web" :version "1" :family "rt-claim"
                :head *hosts-web-head* :body *hosts-web-body*)
    (write-web-hosts g)
    (write-rule g :name "parked" :version "1" :family "rt-claim"
                :enabled nil
                :head *parked-head* :body *web-hosts-body*)
    (let ((reports (graph-db.rules:run-rules g)))
      (is (equal '("web-hosts" "hosts-web")
                 (mapcar #'graph-db.rules:rule-report-rule-name reports)))
      (is (every (lambda (r)
                   (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (= 2 (graph-db.rules:rule-report-derived (second reports))))
      (is (null (derived g 'rt-claim "parked")))
      (is (= 2 (select-count (?h)
                 (claim ?c rt-claim "host" ?h "hosts-web" ?a ?b)
                 (claim-producer ?c "rule/hosts-web")))))))

(defparameter *runs-db-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"db\")")

(test run-rules-runs-a-reader-after-every-producer-of-its-relation
  "Two rules derive \"hosted-on\" -- \"db-hosts\" h1 alone, the
def-rule \"web-hosts\" h1 and h2 -- and \"hosts-web\" reads it.
Scheduling the reader once ONE producer has run would hand it h1 only;
it must wait for both.  The second producer is a DEF-RULE because
RULES-IN-SCOPE appends the image's rules after the store's, which puts
the reader ahead of it in the pending set whatever order MAP-VERTICES
gives the stored two -- so the schedule under test is deterministic."
  (with-rules-graph (g)
    (seed g)
    (graph-db.rules:def-rule "web-hosts" :version "1" :family rt-claim
      :head *web-hosts-head* :body *web-hosts-body*)
    (unwind-protect
         (progn
           (write-rule g :name "db-hosts" :version "1"
                       :family "rt-claim"
                       :head *web-hosts-head* :body *runs-db-body*)
           (write-rule g :name "hosts-web" :version "1"
                       :family "rt-claim"
                       :head *hosts-web-head* :body *hosts-web-body*)
           (let ((reports (graph-db.rules:run-rules g)))
             (is (equal '("db-hosts" "web-hosts" "hosts-web")
                        (mapcar #'graph-db.rules:rule-report-rule-name
                                reports)))
             (is (= 1 (graph-db.rules:rule-report-derived
                       (report-named "db-hosts" reports))))
             (is (= 2 (graph-db.rules:rule-report-derived
                       (report-named "web-hosts" reports))))
             ;; h2 reaches the reader only through the second producer.
             (is (= 2 (graph-db.rules:rule-report-derived
                       (report-named "hosts-web" reports))))
             (is (equal '("h1" "h2")
                        (sort (mapcar #'claim-subject-key
                                      (derived g 'rt-claim "hosts-web"))
                              #'string<)))))
      (graph-db.rules:undef-rule "web-hosts"))))

(defparameter *derives-x*
  "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *reads-x*
  "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *derives-y*
  "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")
(defparameter *reads-y*
  "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")

(test run-rules-reports-a-rule-that-no-longer-compiles-and-the-store-opens
  "Spec §6: a rule that fails to compile is reported and skipped, never
refused at open.  Here a DEF-RULE added after the write closes a cycle
with a stored rule."
  (with-rules-graph-dir (g dir)
    (seed g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head *derives-x* :body *reads-y*)
    (write-web-hosts g)
    (graph-db.rules:def-rule "b" :version "1" :family rt-claim
      :head *derives-y* :body *reads-x*)
    (unwind-protect
         (progn
           (close-graph g)
           (let ((g2 (open-graph *graph-name* (namestring dir))))
             (unwind-protect
                  (let* ((graph-db:*graph* g2)
                         (reports (graph-db.rules:run-rules g2)))
                    (is (= 3 (length reports)))
                    (is (eq :refused
                            (graph-db.rules:rule-report-outcome
                             (report-named "a" reports))))
                    (is (eq :rule
                            (refusal-tag (report-named "a" reports))))
                    (is (eq :refused
                            (graph-db.rules:rule-report-outcome
                             (report-named "b" reports))))
                    (is (eq :derived
                            (graph-db.rules:rule-report-outcome
                             (report-named "web-hosts" reports))))
                    (is (= 2 (graph-db.rules:rule-report-derived
                              (report-named "web-hosts" reports)))))
               (ignore-errors (close-graph g2)))))
      (graph-db.rules:undef-rule "b"))))

(defparameter *foreign-head*
  "(claim ?c rtf-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *foreign-body*
  "(claim ?p rtf-claim \"host\" ?h \"runs\" \"app\" \"web\")")

(test run-rules-skips-a-def-rule-whose-family-the-store-lacks
  "Ruling P8: RTF-CLAIM is registered in the image and indexed in no
graph here."
  (with-rules-graph (g)
    (seed g)
    (graph-db.rules:def-rule "foreign" :version "1" :family rtf-claim
      :head *foreign-head* :body *foreign-body*)
    (unwind-protect
         (progn
           (is (null (graph-db.rules:run-rules g)))
           (let ((report (graph-db.rules:run-rule g "foreign")))
             (is (eq :refused
                     (graph-db.rules:rule-report-outcome report)))
             (is (eq :rule (refusal-tag report)))))
      (graph-db.rules:undef-rule "foreign"))))

(test the-report-carries-cost
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule g (write-web-hosts g))))
      (is (plusp (graph-db.rules:rule-report-inferences report)))
      (is (typep (graph-db.rules:rule-report-elapsed report) 'real))
      (is (>= (graph-db.rules:rule-report-elapsed report) 0)))))

(test a-retracted-derived-claim-stays-retracted-on-rerun
  "Ruling P10's stated cost: keeping is not re-assertion.  The claim is
kept -- same node -- and its closed transaction period stands."
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      (let* ((h1 (find "h1" (derived g 'rt-claim "web-hosts")
                       :key #'claim-object-key :test #'string=))
             (before (graph-db:id h1)))
        (retract-claim h1)
        (let ((report (graph-db.rules:run-rule g r)))
          (is (= 2 (graph-db.rules:rule-report-kept report)))
          (is (= 0 (graph-db.rules:rule-report-derived report)))
          (is (= 0 (graph-db.rules:rule-report-swept report))))
        (let ((again (find "h1" (claims-by-producer g 'rt-claim
                                                    "rule/web-hosts")
                           :key #'claim-object-key :test #'string=)))
          (is-true again)
          (when again
            (is (equalp before (graph-db:id again)))
            (is-false (claim-current-p again))))
        ;; Control: h2 was never retracted and is still believed.
        (is (equal '("h2")
                   (mapcar #'claim-object-key
                           (derived g 'rt-claim "web-hosts"))))
        ;; And through the COPY/SAVE %REFRESH-VERSION does when the
        ;; rule's version moves -- the other path that touches a kept
        ;; claim -- the closed period still stands.
        (with-transaction ((graph-db::transaction-manager g))
          (let ((c (copy r)))
            (setf (graph-db.rules:rule-version c) "2")
            (save c)))
        (is (= 2 (graph-db.rules:rule-report-kept
                  (graph-db.rules:run-rule g "web-hosts"))))
        (let ((again (find "h1" (claims-by-producer g 'rt-claim
                                                    "rule/web-hosts")
                           :key #'claim-object-key :test #'string=)))
          (is-true again)
          (when again
            (is (string= "2" (claim-rule-version again)))
            (is-false (claim-current-p again))))))))

(test a-temporal-rerun-keeps-its-claims
  "Ruling P10 on a temporal family, where the extent start joins the
identity: the same premises re-derive the same four identities, so a
rerun keeps all four and constructs nothing."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((r (write-host-version g)))
      (graph-db.rules:run-rule g r)
      (let ((before (sort (mapcar #'claim-identity-key
                                  (derived g 'rtt-claim "host-version"))
                          #'string<))
            (report (graph-db.rules:run-rule g r)))
        (is (= 4 (length before)))
        (is (= 4 (graph-db.rules:rule-report-kept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report)))
        (is (= 0 (graph-db.rules:rule-report-swept report)))
        (is (equal before
                   (sort (mapcar #'claim-identity-key
                                 (derived g 'rtt-claim "host-version"))
                         #'string<)))
        ;; The provenance reconciles the same way: two premises each.
        (is (= 8 (length (claims-by-producer
                          g 'graph-db.rules:derivation
                          "rule/host-version"))))))))

(test a-store-without-the-rule-schema-runs-no-rules
  "Ruling T4-R2: :GRAPH-DB-RULES-NORULE never ran DEF-RULES-SCHEMA, so
its schema carries no RULE vertex type.  RUN-RULES reports nothing, and
a rule asked for by name is \"no such rule\" -- which is the guarded
half: MAP-VERTICES skips a type the schema lacks on its own, but the
INDEX-LOOKUP behind %RESOLVE-RULE signals QUERY-PRECONDITION-ERROR
unless %GRAPH-DECLARES-P holds it back."
  (with-norule-graph (g)
    (is-false (member "RULE" (graph-db.query:schema-type-names g :vertex)
                      :test #'string-equal))
    (is (null (graph-db.rules::rules-in-scope g)))
    (is (null (graph-db.rules:run-rules g)))
    (is (search "no rule named"
                (handler-case (progn (graph-db.rules:run-rule g "nope")
                                     "")
                  (error (c) (princ-to-string c)))
                :test #'char-equal))))

(test the-constructor-name-is-not-print-case-sensitive
  "%CONSTRUCTOR interns MAKE-<CLASS> from the class's own SYMBOL-NAME:
a *PRINT-CASE* the operator set must not change which function the
derivation calls.  A unit test rather than a RUN-RULE one on purpose --
the engine's MAKE-FUNCTOR-SYMBOL builds NAME/ARITY with the same FORMAT
(prologc.lisp), so under :DOWNCASE no Prolog goal resolves at all; that
is core, pre-existing and outside this slice (GH #342)."
  (let ((*print-case* :downcase))
    (is (eq #'make-rt-claim-binary
            (graph-db.rules::%constructor (claim-family 'rt-claim) nil)))
    (is (eq #'make-rt-claim-unary
            (graph-db.rules::%constructor (claim-family 'rt-claim) t)))))

(test an-unclassified-commit-refusal-is-tagged-rule
  "The report's tag vocabulary is closed (docs/rules.md): a
CONSTRAINT-VIOLATION that is none of the three the report names is
tagged :RULE, not with its own class name.  The classified paths have
their own tests above, both tagging 'RTT-CLAIM."
  (is (eq :rule (graph-db.rules::%violation-family
                 (make-condition 'graph-db:constraint-violation)))))

(defparameter *twin-relation* "explains")

(test a-second-derivation-record-for-one-pair-is-swept
  "One derivation record per (derived claim, premise) pair: a second
record naming a pair already kept goes with the records the derivation
no longer asks for.  DEF-CLAIM-CLASSES' identity constraint makes an
exact twin impossible, so the intruder here differs in its relation --
which is also why the keep is restricted to \"derived-from\"."
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      (let ((recs (claims-by-producer g 'graph-db.rules:derivation
                                      "rule/web-hosts")))
        (is (= 2 (length recs)))
        (let ((c (first recs)))
          (with-transaction ((graph-db::transaction-manager g))
            (graph-db.rules::make-derivation-binary
             :graph g :subject-namespace :claim
             :subject-key (claim-subject-key c)
             :relation *twin-relation* :object-namespace :claim
             :object-key (claim-object-key c)
             :producer "rule/web-hosts" :rule-version "1"
             :standing :inferred))))
      (is (= 3 (length (claims-by-producer g 'graph-db.rules:derivation
                                           "rule/web-hosts"))))
      (graph-db.rules:run-rule g r)
      (let ((after (claims-by-producer g 'graph-db.rules:derivation
                                       "rule/web-hosts")))
        (is (= 2 (length after)))
        (is (every (lambda (rec)
                     (string= "derived-from" (claim-relation rec)))
                   after))))))

;;; The whole-branch review's findings (#331)

(defun host-version-of (g host ver)
  "The claim \"host-version\" derived for HOST and version VER."
  (find-if (lambda (c) (and (string= host (claim-subject-key c))
                            (string= ver (claim-object-key c))))
           (derived g 'rtt-claim "host-version")))

(test a-kept-claims-extent-follows-its-premises
  "A kept claim's validity is re-read from the current derivation, not
left at the value it was first derived with.  The dedupe key carries
the extent START, so h2's deployment ending May 15 rather than May 31
is the SAME identity -- kept, same node -- and the derived claim's end
must move with it."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((r (write-host-version g)))
      (graph-db.rules:run-rule g r)
      (let ((before (graph-db:id (host-version-of g "h2" "2")))
            (premise (find "h2"
                           (claims-touching g 'rtt-claim :app "web"
                                            :role :subject
                                            :relation "deployed-on")
                           :key #'claim-object-key :test #'string=)))
        (with-transaction ((graph-db::transaction-manager g))
          (let ((c (copy premise)))
            (setf (claim-extent c)
                  (interval (ts 2026 5 1) (ts 2026 5 15)))
            (save c)))
        (let ((report (graph-db.rules:run-rule g r)))
          (is (= 4 (graph-db.rules:rule-report-kept report)))
          (is (= 0 (graph-db.rules:rule-report-derived report)))
          (is (= 0 (graph-db.rules:rule-report-swept report))))
        (let ((after (host-version-of g "h2" "2")))
          (is-true after)
          (when after
            (is (equalp before (graph-db:id after)))
            (is (local-time:timestamp=
                 (ts 2026 5 15)
                 (nth-value 1 (claim-bounds after))))))))))

(test a-policy-change-refreshes-kept-claims
  "The extent policy is part of the derivation, so a rule moved from
:NONE to :PREMISES must give its KEPT claims the extent they would now
be derived with.  On SEED and SEED-TEMPORAL's six solutions :NONE
derives the four distinct (host, version) pairs with no extent;
:PREMISES drops the two disjoint solutions -- h1's August run against
version 1, h2's May run against version 1 -- and the remaining four
collapse to three pairs, rt-claim's identity ignoring the extent.  So
(h2, 1) is swept and the other three are kept, each with an extent and
the new version."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((r (write-rule g :name "hv-flat" :version "1"
                         :family "rt-claim" :extent-policy :none
                         :head *hv-flat-head*
                         :body *host-version-body*)))
      (is (= 4 (graph-db.rules:rule-report-derived
                (graph-db.rules:run-rule g r))))
      (is (every (lambda (c) (null (claim-extent c)))
                 (derived g 'rt-claim "hv-flat")))
      (with-transaction ((graph-db::transaction-manager g))
        (let ((c (copy r)))
          (setf (graph-db.rules:rule-extent-policy c) :premises
                (graph-db.rules:rule-version c) "2")
          (save c)))
      (let ((report (graph-db.rules:run-rule g "hv-flat")))
        (is (= 3 (graph-db.rules:rule-report-kept report)))
        (is (= 1 (graph-db.rules:rule-report-swept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report)))
        (is (= 2 (graph-db.rules:rule-report-disjoint-premises report))))
      (let ((claims (derived g 'rt-claim "hv-flat")))
        (is (= 3 (length claims)))
        (is (every #'claim-extent claims))
        (is (every (lambda (c) (string= "2" (claim-rule-version c)))
                   claims))))))

(test the-reports-counts-are-per-attempt-not-cumulative
  "CALL-WITH-TRANSACTION re-invokes its thunk on VALIDATION-CONFLICT
(transactions.lisp) and %DERIVE INCFs the report the caller holds, so
an attempt must start from zero.  Staging a real conflict is not cheap;
two %DERIVE calls in one transaction exercise the same accumulation.
They are not a literal retry -- CLAIMS-BY-PRODUCER overlays the open
transaction's writes (GH #324), so the second call KEEPS what the first
derived -- but four counts over two claims is exactly the bug.
Evaluation is the caller's since S3-P2, so one %DESIRED and two
%DERIVEs is now literally the retry shape a cross-store run takes."
  (with-rules-graph (g)
    (seed g)
    (let* ((r (write-web-hosts g))
           (compiled (graph-db.rules:compile-rule g r))
           (report (graph-db.rules::%make-rule-report
                    :rule-name "web-hosts" :version "1")))
      (with-transaction ((graph-db::transaction-manager g))
        (multiple-value-bind (desired order)
            (graph-db.rules::%desired compiled g report)
          (graph-db.rules::%derive compiled g report desired order)
          (is (= 2 (graph-db.rules:rule-report-derived report)))
          (graph-db.rules::%derive compiled g report desired order)))
      (is (= 0 (graph-db.rules:rule-report-derived report)))
      (is (= 2 (graph-db.rules:rule-report-kept report)))
      (is (= 2 (length (derived g 'rt-claim "web-hosts")))))))

(test the-provenance-reads-see-derived-from-records-only
  "PREMISES-OF and DEPENDENTS-OF filter the producer's records on
\"derived-from\": a record of another relation under the same producer
is not provenance.  The next run sweeps it (the test above), so this
asks before rerunning."
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      (let* ((claims (derived g 'rt-claim "web-hosts"))
             (h1 (find "h1" claims :key #'claim-object-key
                                   :test #'string=))
             (h2 (find "h2" claims :key #'claim-object-key
                                   :test #'string=)))
        ;; One intruder, h2's key as subject and h1's as object, so it
        ;; lies on the path of both reads at once.
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db.rules::make-derivation-binary
           :graph g :subject-namespace :claim
           :subject-key (claim-identity-key h2)
           :relation "annotates" :object-namespace :claim
           :object-key (claim-identity-key h1)
           :producer "rule/web-hosts" :rule-version "1"
           :standing :inferred))
        (is (= 3 (length (claims-by-producer
                          g 'graph-db.rules:derivation
                          "rule/web-hosts"))))
        (let ((premises (graph-db.rules:premises-of g h2)))
          (is (= 1 (length premises)))
          (is (string= "runs" (claim-relation (first premises)))))
        (is (null (graph-db.rules:dependents-of g h1)))))))

(defparameter *rtu-hosts-head*
  "(claim ?c rtu-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")

(test a-stores-own-def-unique-refuses-the-derivation
  "Ruling F-R1, spec §11's refusal half: under P10 a run cannot collide
with its own tuple, so a UNIQUE-CONSTRAINT-VIOLATION reaches RUN-RULE
only through a constraint the store declared itself.  RTU-CLAIM carries
one object per (subject, relation) (tests/rules/suite.lisp); this rule
derives two, h1 and h2."
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "rtu-hosts" :version "1"
                                 :family "rtu-claim"
                                 :head *rtu-hosts-head*
                                 :body *web-hosts-body*))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq 'rtu-claim (refusal-tag report)))
      (is (search "unique" (refusal-text report) :test #'char-equal))
      (is (null (derived g 'rtu-claim "rtu-hosts"))))))
