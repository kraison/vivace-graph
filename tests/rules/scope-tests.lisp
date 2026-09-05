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

(test a-single-store-scope-is-s2-unchanged
  "Control for S3-P2: RUN-RULE with no scope, or a scope of the store
alone, derives inside its transaction exactly as before."
  (with-two-stores (a b)
    (seed a)
    (seed-b b)
    (let ((r (write-rule a :name "web-hosts" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (is (= 2 (graph-db.rules:rule-report-derived
                (graph-db.rules:run-rule a r))))
      (is (= 2 (graph-db.rules:rule-report-kept
                (graph-db.rules:run-rule a r :scope (list a)))))
      ;; The scope's own store is put first whatever the caller wrote,
      ;; and named twice it is still read once.
      (let ((report (graph-db.rules:run-rule a r :scope (list b a b))))
        (is (= 1 (graph-db.rules:rule-report-derived report)))
        (is (= 2 (graph-db.rules:rule-report-kept report)))))))

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
