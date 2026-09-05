;;;; tests/rules/scope-tests.lisp -- cross-store scope (spec §10, GH
;;;; #332).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(test claim-reads-every-store-in-scope
  "With *CLAIM-SCOPE* bound, CLAIM/7 generates from each store's index in
scope order; with it NIL, from *GRAPH* alone (S3-P1)."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    ;; Control: NIL scope is S1 exactly -- A's two hosts of web.
    (is (equal '("h1" "h2")
               (sort (select-flat (?h)
                       (claim ?c rt-claim "host" ?h "runs" "app" "web"))
                     #'string<)))
    (let ((graph-db::*claim-scope* (list a b)))
      (is (equal '("h1" "h2" "h3")
                 (sort (select-flat (?h)
                         (claim ?c rt-claim "host" ?h "runs" "app" "web"))
                       #'string<)))
      ;; The subject route across stores: h1 runs web and db in A, cache
      ;; in B.
      (is (equal '("cache" "db" "web")
                 (sort (select-flat (?a)
                         (claim ?c rt-claim "host" "h1" "runs" "app" ?a))
                       #'string<)))
      ;; A node from B is a node: the filters work on it.
      (is (equal '("scan-c")
                 (select-flat (?p)
                   (claim ?c rt-claim "host" "h3" "runs" "app" "web")
                   (claim-producer ?c ?p)))))))

(test the-subject-only-route-reads-every-store-in-scope
  "The subject index route -- namespace and key bound, relation not --
is the indexed route the test above leaves out: h1 runs web and db in A,
cache in B."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    ;; Control: NIL scope answers A alone.
    (is (equal '("db" "web")
               (sort (select-flat (?o)
                       (claim ?c rt-claim "host" "h1" ?r ?ons ?o))
                     #'string<)))
    (let ((graph-db::*claim-scope* (list a b)))
      (is (equal '("cache" "db" "web")
                 (sort (select-flat (?o)
                         (claim ?c rt-claim "host" "h1" ?r ?ons ?o))
                       #'string<))))))

(test claim-producer-generates-across-scope
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      ;; scan-c wrote 2 rt-claims and 1 rtt-claim, all in B.
      (is (= 2 (select (:count t :max-inferences 1000) (?c)
                 (claim-producer ?c "scan-c")
                 (claim ?c rt-claim ?s ?k ?r ?o ?ok))))
      ;; scan-a is in A only; the scope adds nothing to it.
      (is (= 2 (select (:count t :max-inferences 1000) (?c)
                 (claim-producer ?c "scan-a")
                 (claim ?c rt-claim ?s ?k ?r ?o ?ok)))))))

(test a-store-lacking-the-family-contributes-nothing
  "rtu-claim is declared for A only; B in scope adds nothing and refuses
nothing (S3-P5)."
  (with-two-stores (a b)
    (seed a)
    (with-transaction ((graph-db::transaction-manager a))
      (make-rtu-claim-binary :graph a :subject-namespace :app
                             :subject-key "web" :relation "owned-by"
                             :object-namespace :team :object-key "t1"
                             :producer "scan-a" :standing :observed))
    (let ((graph-db::*claim-scope* (list a b)))
      (is (equal '("t1")
                 (select-flat (?t)
                   (claim ?c rtu-claim "app" "web" "owned-by"
                          "team" ?t)))))))

(test the-walk-covers-every-store-without-a-bound-and-refuses-under-one
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      ;; 4 rt-claims in A + 2 in B.
      (is (= 6 (select-count (?c) (claim ?c rt-claim ?a ?b ?r ?d ?e))))
      (signals graph-db::prolog-cost-unbounded-error
        (select (:max-inferences 1000) (?c)
          (claim ?c rt-claim ?a ?b ?r ?d ?e))))))

(test the-walk-skips-a-store-that-lacks-the-family
  "The unrouted walk visits every store in scope, and one whose schema
never declared the family visits nothing rather than signalling -- what
%UNBOUND-CLAIM-SCAN's :VERTEX-TYPE buys (recon B8).  The rt-claim count
is the control: the same scope, the same walk, a family both stores
carry."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (with-transaction ((graph-db::transaction-manager a))
      (make-rtu-claim-binary :graph a :subject-namespace :app
                             :subject-key "web" :relation "owned-by"
                             :object-namespace :team :object-key "t1"
                             :producer "scan-a" :standing :observed)
      (make-rtu-claim-binary :graph a :subject-namespace :app
                             :subject-key "db" :relation "owned-by"
                             :object-namespace :team :object-key "t2"
                             :producer "scan-a" :standing :observed))
    (let ((graph-db::*claim-scope* (list a b)))
      ;; rtu-claim is declared for A alone: two, and B signals nothing.
      (is (= 2 (select-count (?c) (claim ?c rtu-claim ?s ?k ?r ?o ?ok))))
      (is (= 6 (select-count (?c) (claim ?c rt-claim ?s ?k ?r ?o ?ok)))))))

(test a-foreign-read-inside-a-transaction-is-the-engines-refusal
  "The GH #53 contract, not ours: a read-write transaction on A refuses
every read of B.  RUN-RULE evaluates before its transaction for that
reason (S3-P2); a Lisp caller binding the scope inside one gets the
engine's error."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((graph-db::*claim-scope* (list a b)))
      (signals graph-db:cross-graph-transaction-error
        (with-transaction ((graph-db::transaction-manager a))
          (select-flat (?h)
            (claim ?c rt-claim "host" ?h "runs" "app" "web")))))))

;;; RUN-RULE with a scope (S3-P2, S3-P3).

(defun derivation-records (g claim)
  "The DERIVED-FROM records G's rule wrote for CLAIM, as subject."
  (claims-touching g 'graph-db.rules:derivation :claim
                   (claim-identity-key claim)
                   :role :subject :relation "derived-from"))

(test a-rule-in-a-derives-from-premises-in-a-and-b-and-writes-only-a
  "Spec §11's S3 bullet."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let* ((r (write-rule a :name "web-hosts" :version "1"
                          :family "rt-claim"
                          :head *web-hosts-head* :body *web-hosts-body*))
           (report (graph-db.rules:run-rule a r :scope (list a b))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 3 (graph-db.rules:rule-report-derived report)))
      (let ((claims (derived a 'rt-claim "web-hosts")))
        (is (equal '("h1" "h2" "h3")
                   (sort (mapcar #'claim-object-key claims) #'string<))))
      ;; Nothing was written to B: no derived claim under either family
      ;; it carries, its own claims untouched, and no DERIVATION family
      ;; on it at all, so no record could live there either.
      (is (null (claims-by-producer b 'rt-claim "rule/web-hosts")))
      (is (null (claims-by-producer b 'rtt-claim "rule/web-hosts")))
      (is (= 2 (length (claims-by-producer b 'rt-claim "scan-c"))))
      (is (not (graph-db.rules::%graph-declares-p
                b 'graph-db.rules:derivation)))
      ;; Provenance names B for h3's premise and nothing for h1's.
      (let* ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                       :key #'claim-object-key :test #'string=))
             (h1 (find "h1" (derived a 'rt-claim "web-hosts")
                       :key #'claim-object-key :test #'string=))
             (rec-h3 (first (derivation-records a h3)))
             (rec-h1 (first (derivation-records a h1))))
        (is-true rec-h3)
        (is-true rec-h1)
        (is (string= "graph-db-rules-b" (claim-method rec-h3)))
        (is (null (claim-method rec-h1)))))))

(defmacro with-transaction-probe ((place) &body body)
  "BODY with %DESIRED wrapped so PLACE records whether a transaction was
open when the body's evaluation ran: T on S2's path, NIL on the
cross-store one, where the evaluation precedes the write transaction
(S3-P2).  The redefine/restore shape is tests/open-hygiene-tests.lisp's
%OH-WITH-INJECTED-FAILURE."
  (let ((orig (gensym "ORIG")))
    `(let ((,orig (fdefinition 'graph-db.rules::%desired)))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db.rules::%desired)
                    (lambda (&rest args)
                      (setf ,place (and graph-db::*transaction* t))
                      (apply ,orig args)))
              ,@body)
         (setf (fdefinition 'graph-db.rules::%desired) ,orig)))))

(test the-scope-decides-where-the-body-is-evaluated
  "S3-P2's discriminator, not only its counts: every count below is the
same whichever path a run takes, so the test observes the path itself --
%DESIRED runs with a transaction open on the single-store path and with
none on the cross-store one.  That NIL is also recon B9's corollary:
during a cross-store evaluation there is no transaction to overlay."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*))
          (seen :unset))
      (with-transaction-probe (seen)
        ;; No scope: S2 exactly, evaluated inside the transaction.
        (is (= 2 (graph-db.rules:rule-report-derived
                  (graph-db.rules:run-rule a r))))
        (is (eq t seen))
        (setf seen :unset)
        ;; A scope naming the own store alone takes the same path.
        (is (= 2 (graph-db.rules:rule-report-kept
                  (graph-db.rules:run-rule a r :scope (list a)))))
        (is (eq t seen))
        (setf seen :unset)
        ;; The own store is put first whatever the caller wrote, and
        ;; named twice it is still read once -- and this one evaluates
        ;; before its transaction.
        (let ((report (graph-db.rules:run-rule a r :scope (list b a b))))
          (is (= 1 (graph-db.rules:rule-report-derived report)))
          (is (= 2 (graph-db.rules:rule-report-kept report))))
        ;; :UNSET, not NIL, when the probe never fired.
        (is (null seen))))))

(test the-store-name-merge-prefers-the-own-store-then-the-first
  "S3-P3's rule, one helper for both merge sites (%MERGE-PREMISE-REFS
within a solution set, %RECONCILE-PROVENANCE across derived claims)."
  (is (null (graph-db.rules::%merge-store-name "b" nil)))
  (is (null (graph-db.rules::%merge-store-name nil "b")))
  (is (null (graph-db.rules::%merge-store-name nil nil)))
  (is (string= "b" (graph-db.rules::%merge-store-name "b" "c")))
  (is (string= "b" (graph-db.rules::%merge-store-name "b" "b"))))

(test cross-store-validity-intersects-across-stores
  "rtt premises: web version 3 (B, Jul 1 - Sep 30) against h1's
deployments (A, Feb 1 - Jun 30 and Aug 1 - Sep 30) and h2's (A, May 1 -
May 31), plus SEED's versions 1 (Jan 1 - Mar 31) and 2 (Apr 1 - Dec 31).
Nine solutions.  Five intersect -- (h1 Feb-Jun, v1), (h1 Feb-Jun, v2),
(h1 Aug-Sep, v2), (h1 Aug-Sep, v3), (h2 May, v2) -- and four are
disjoint: (h1 Feb-Jun, v3), (h1 Aug-Sep, v1), (h2 May, v1),
(h2 May, v3).  A alone gave 4 and 2 (S2)."
  (with-two-stores (a b)
    (seed a)
    (seed-temporal a)
    (seed-b b)
    (let ((report (graph-db.rules:run-rule
                   a (write-rule a :name "host-version" :version "1"
                                 :family "rtt-claim"
                                 :head *host-version-head*
                                 :body *host-version-body*)
                   :scope (list a b))))
      (is (= 5 (graph-db.rules:rule-report-derived report)))
      (is (= 4 (graph-db.rules:rule-report-disjoint-premises report)))
      (let ((v3 (find "3" (derived a 'rtt-claim "host-version")
                      :key #'claim-object-key :test #'string=)))
        (is-true v3)
        (when v3
          (multiple-value-bind (s e) (claim-bounds v3)
            (is (local-time:timestamp= (ts 2026 8 1) s))
            (is (local-time:timestamp= (ts 2026 9 30) e)))
          ;; Two premises, one per store: the deployment from A with no
          ;; method, the version run from B named (S3-P3).
          (let ((recs (derivation-records a v3)))
            (is (= 2 (length recs)))
            (is (= 1 (count "graph-db-rules-b" recs
                            :key #'claim-method :test #'equal)))
            (is (= 1 (count nil recs :key #'claim-method)))))))))

(test a-cross-store-rerun-reconciles-and-a-b-change-is-seen-next-run
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (graph-db.rules:run-rule a r :scope (list a b))
      (with-transaction ((graph-db::transaction-manager b))
        (mark-deleted (first (claims-touching b 'rt-claim :host "h3"
                                              :role :subject))))
      (let ((report (graph-db.rules:run-rule a r :scope (list a b))))
        (is (= 2 (graph-db.rules:rule-report-kept report)))
        (is (= 1 (graph-db.rules:rule-report-swept report)))
        (is (= 0 (graph-db.rules:rule-report-derived report))))
      ;; h3's provenance record went with it.
      (is (= 2 (length (claims-by-producer a 'graph-db.rules:derivation
                                           "rule/web-hosts")))))))

(test run-rules-takes-the-scope
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                :head *web-hosts-head* :body *web-hosts-body*)
    (let ((reports (graph-db.rules:run-rules a :scope (list a b))))
      (is (= 1 (length reports)))
      (is (= 3 (graph-db.rules:rule-report-derived (first reports)))))))

(test a-cross-store-run-under-one-clock-derives
  "Under a shared clock the composed snapshots take epochs from one
counter (#168).  Observable here: the run works and derives from both
stores.  Nothing asserts one instant -- the engine provides none across
stores (recon C2), and a test of it would pass vacuously in a quiescent
suite."
  (with-clocked-stores (a b)
    (seed a)
    (seed-b b)
    (let ((report (graph-db.rules:run-rule
                   a (write-rule a :name "web-hosts" :version "1"
                                 :family "rt-claim"
                                 :head *web-hosts-head*
                                 :body *web-hosts-body*)
                   :scope (list a b))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 3 (graph-db.rules:rule-report-derived report))))))

(test a-scope-must-be-open-graphs
  "The scope is normalised before anything else runs, so a non-store in
it signals rather than being read as an empty store."
  (with-two-stores (a b)
    (seed a)
    (let ((r (write-rule a :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      ;; Control: the same call with real stores derives.
      (is (eq :derived (graph-db.rules:rule-report-outcome
                        (graph-db.rules:run-rule a r :scope (list a b)))))
      (signals error
        (graph-db.rules:run-rule a r :scope (list b :not-a-graph))))))

(test a-premise-that-moves-store-renames-its-method
  "S3-P3's collision corner and the METHOD refresh in one: h3's premise
is B's alone on the first run, so its record names B; written into A
under the same producer it has one identity key in two stores, the own
store wins, and the record must end at NIL without the run refusing."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (graph-db.rules:run-rule a r :scope (list a b))
      (let ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                      :key #'claim-object-key :test #'string=)))
        (is (string= "graph-db-rules-b"
                     (claim-method (first (derivation-records a h3))))))
      ;; The same premise identity -- same producer -- now in A too.
      (with-transaction ((graph-db::transaction-manager a))
        (make-rt-claim-binary :graph a :subject-namespace :host
                              :subject-key "h3" :relation "runs"
                              :object-namespace :app :object-key "web"
                              :producer "scan-c" :standing :observed))
      (let ((report (graph-db.rules:run-rule a r :scope (list a b))))
        (is (eq :derived (graph-db.rules:rule-report-outcome report)))
        (is (null (graph-db.rules:rule-report-refusals report)))
        (is (= 3 (graph-db.rules:rule-report-kept report)))
        (let* ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                         :key #'claim-object-key :test #'string=))
               (recs (derivation-records a h3)))
          (is (= 1 (length recs)))
          (is (null (claim-method (first recs)))))))))

(test a-cross-store-refusal-still-reports-and-writes-nothing
  "The evaluation sits OUTSIDE the write transaction on the cross-store
path (S3-P2), so RUN-RULE's handlers have to wrap that branch too.  Two
refusals raised there, and the control that the same run derives."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (let* ((graph-db.rules:*rules-max-solutions* 1)
             (report (graph-db.rules:run-rule a r :scope (list a b))))
        (is (eq :refused (graph-db.rules:rule-report-outcome report)))
        (is (eq :solutions (refusal-tag report)))
        (is (= 0 (graph-db.rules:rule-report-derived report))))
      (let* ((graph-db.rules:*rules-max-inferences* 1)
             (report (graph-db.rules:run-rule a r :scope (list a b))))
        (is (eq :refused (graph-db.rules:rule-report-outcome report)))
        (is (eq :budget (refusal-tag report))))
      ;; Neither refusal wrote anything; the control then derives.
      (is (null (derived a 'rt-claim "web-hosts")))
      (is (= 3 (graph-db.rules:rule-report-derived
                (graph-db.rules:run-rule a r :scope (list a b))))))))

;;; Reading provenance under a scope (S3-P4), and the scope's own
;;; refusals.

(defmacro signalled-text (&body body)
  "The text of the error BODY signals, or \"\" when it signals none --
so an assertion can name which of two refusals answered."
  `(handler-case (progn ,@body "")
     (error (c) (princ-to-string c))))

(test run-rules-normalises-the-scope-before-any-rule-runs
  "RUN-RULES normalises :SCOPE once at entry, so a bad scope signals
even on a store with no runnable rule -- where no RUN-RULE call would
ever look at it."
  (with-two-stores (a b)
    (seed a)
    ;; Control: nothing to run, a good scope, no error and no reports.
    (is (null (graph-db.rules:run-rules a :scope (list a b))))
    (signals error
      (graph-db.rules:run-rules a :scope (list b :not-a-graph)))))

(test a-scope-store-must-be-keyword-named
  "Recon B5: %STORE-NAME downcases the store's SYMBOL-NAME, so a store
in a scope must be keyword-named -- and %NORMALIZE-SCOPE is where that
is refused, before the rule is resolved.  Both calls name a rule no
store holds: with the odd store the scope answers, with B it is
%RESOLVE-RULE."
  (with-two-stores (a b)
    (seed a)
    (let ((odd (make-instance 'graph-db::graph :graph-name "b")))
      (is (search "keyword"
                  (signalled-text
                    (graph-db.rules:run-rule a "no-such-rule"
                                             :scope (list odd)))
                  :test #'char-equal))
      (is (search "no rule named"
                  (signalled-text
                    (graph-db.rules:run-rule a "no-such-rule"
                                             :scope (list b)))
                  :test #'char-equal)))))

(test premises-of-resolves-in-the-store-the-record-names
  "S3-P4: a record's METHOD says which store its premise is in, and a
premise whose store is not in the caller's scope is dropped rather than
looked for in the rule's own store.  DEPENDENTS-OF needs no scope: the
records are the rule's store's whatever store the premise lives in.

The decoy is what makes the drop assertion discriminate: A holds a
claim with the B premise's exact identity, written after the run, so a
resolve-in-GRAPH fallback answers it instead of NIL."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (graph-db.rules:run-rule
     a (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                   :head *web-hosts-head* :body *web-hosts-body*)
     :scope (list a b))
    ;; Written AFTER the run, so no record names A for this premise.
    (with-transaction ((graph-db::transaction-manager a))
      (make-rt-claim-binary :graph a :subject-namespace :host
                            :subject-key "h3" :relation "runs"
                            :object-namespace :app :object-key "web"
                            :producer "scan-c" :standing :observed))
    (let ((h3 (find "h3" (derived a 'rt-claim "web-hosts")
                    :key #'claim-object-key :test #'string=))
          (h1 (find "h1" (derived a 'rt-claim "web-hosts")
                    :key #'claim-object-key :test #'string=)))
      ;; In scope: the premise, from B.
      (let ((ps (graph-db.rules:premises-of a h3 :scope (list a b))))
        (is (= 1 (length ps)))
        (is (string= "h3" (claim-subject-key (first ps))))
        (is (eq b (graph-db::node-graph (first ps)))))
      ;; Out of scope: dropped, not resolved in A by mistake (S3-P4).
      (is (null (graph-db.rules:premises-of a h3)))
      ;; Control: an own-store premise answers under the default scope.
      (let ((ps (graph-db.rules:premises-of a h1)))
        (is (= 1 (length ps)))
        (is (eq a (graph-db::node-graph (first ps)))))
      ;; And a B premise's dependents are findable from A.
      (let ((premise (first (claims-touching b 'rt-claim :host "h3"
                                             :role :subject))))
        (is (= 1 (length (graph-db.rules:dependents-of a premise))))))))
