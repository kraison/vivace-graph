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
