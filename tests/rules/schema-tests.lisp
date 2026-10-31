;;;; tests/rules/schema-tests.lisp -- the rule record (spec §5, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defparameter *web-hosts-head*
  "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")

(defparameter *web-hosts-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")

(test a-rule-record-writes-and-reads-back
  (with-rules-graph (g)
    (let ((r (write-rule g :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (is (graph-db.rules:rule-p r))
      (is (string= "web-hosts" (graph-db.rules:rule-name r)))
      (is (eq :premises (graph-db.rules:rule-extent-policy r)))
      (is (eq t (graph-db.rules:rule-enabled r)))
      ;; The identity facet: found by name through its own index.
      (is (eq (graph-db:id r)
              (graph-db:id (first (graph-db:index-lookup
                                   g 'graph-db.rules:rule
                                   '(graph-db.rules::name)
                                   "web-hosts"))))))))

(test one-rule-per-name-is-the-identity-facets-uniqueness
  "Ruling P1: a second record with the same name is a uniqueness
violation; a new version is COPY, SETF, SAVE."
  (with-rules-graph (g)
    (let ((r (write-rule g :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (signals graph-db:unique-constraint-violation
        (write-rule g :name "web-hosts" :version "2"
                    :family "rt-claim"
                    :head *web-hosts-head* :body *web-hosts-body*))
      (with-transaction ((graph-db::transaction-manager g))
        (let ((c (copy r)))
          (setf (graph-db.rules:rule-version c) "2")
          (save c)))
      (is (string= "2" (graph-db.rules:rule-version
                        (first (graph-db:index-lookup
                                g 'graph-db.rules:rule
                                '(graph-db.rules::name) "web-hosts"))))))))

(test a-rule-name-and-extent-policy-are-validated-at-commit
  (with-rules-graph (g)
    (signals graph-db:value-constraint-violation
      (write-rule g :name "Not Canonical" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body *web-hosts-body*))
    (signals graph-db:value-constraint-violation
      (write-rule g :name "ok" :version "1" :family "rt-claim"
                  :extent-policy :sometimes
                  :head *web-hosts-head* :body *web-hosts-body*))
    ;; Control: the canonical shape commits.
    (finishes
      (write-rule g :name "ok" :version "1" :family "rt-claim"
                  :extent-policy :none
                  :head *web-hosts-head* :body *web-hosts-body*))))

(test the-derivation-family-is-declared-on-the-store
  (with-rules-graph (g)
    (is (claim-family 'graph-db.rules:derivation))
    (is-false (claim-family-temporal-p
               (claim-family 'graph-db.rules:derivation)))
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db.rules::make-derivation-binary
       :graph g :subject-namespace :claim :subject-key "a|b|c"
       :relation "derived-from" :object-namespace :claim
       :object-key "d|e|f" :producer "rule/x" :standing :inferred))
    (is (= 1 (length (claims-touching g 'graph-db.rules:derivation
                                      :claim "d|e|f" :role :object))))))
