;;;; Endpoint resolution, and the fail-closed sensitivity predicate (#132).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

;; WITH-SOURCE-GRAPH, *SOURCE-GRAPH-NAME*, and the ST-* source classes come
;; from source-tests.lisp, which loads before this file (graph-db.asd).

(test resolve-endpoint-finds-a-record-by-external-key
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "one" :report-id "r-1")
      (make-st-report :headline "two" :report-id "r-2"))
    (let ((n (resolve-endpoint :st-reports "r-2")))
      (is-true n)
      (is (string= "two" (st-headline n))))))

(test a-key-that-matches-nothing-returns-nil
  "Distinct from an unknown namespace, which signals (design §4)."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (is (null (resolve-endpoint :st-reports "r-99")))))

(test an-unknown-namespace-signals-rather-than-returning-nil
  "Collapsing both to NIL would make a misspelled namespace
indistinguishable from an absent record."
  (with-source-graph (g)
    (declare (ignorable g))
    (signals unknown-namespace (resolve-endpoint :st-nope "r-1"))))

(test resolve-endpoint-refuses-to-run-in-a-read-write-transaction
  "Design §4.1: resolution can cross graphs, and the 3.0 contract permits
cross-graph reads only from a read-only snapshot or outside a transaction.
The caller's mistake is the call site, not the lookup."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-st-report :headline "one" :report-id "r-1"))
    (signals resolution-in-transaction
      (with-transaction () (resolve-endpoint :st-reports "r-1")))))

(test two-classes-answering-one-key-signals-and-names-both
  "Design §4.2: DEF-UNIQUE cannot catch this -- the classes have different
owners and the constraint registry keys on owner."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "shared")
      (make-st-summary :topic "s" :summary-id "shared"))
    (handler-case (progn (resolve-endpoint :st-reports "shared")
                         (is-true nil "expected AMBIGUOUS-ENDPOINT"))
      (ambiguous-endpoint (c)
        (is (= 2 (length (ambiguous-endpoint-classes c))))))))

(test distinct-keys-in-a-shared-namespace-still-resolve
  "Sharing a namespace is legal; only a shared KEY is a violation."
  (with-source-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-st-report :headline "r" :report-id "k-1")
      (make-st-summary :topic "s" :summary-id "k-2"))
    (is (string= "r" (st-headline (resolve-endpoint :st-reports "k-1"))))
    (is (string= "s" (st-topic (resolve-endpoint :st-reports "k-2"))))))

(test disclosable-p-is-fail-closed
  "Design §3.2.  An unrecognised class, and :NONE, are treated as MORE
restricted than every known one -- never less.  If this test is ever
inverted the facet becomes worse than nothing, because a caller trusts it."
  (is-true (disclosable-p :public :public))
  (is-true (disclosable-p :public :restricted))
  (is-false (disclosable-p :restricted :public))
  (is-true (disclosable-p :restricted :restricted))
  (is-false (disclosable-p :no-such-class :restricted))
  (is-false (disclosable-p :none :restricted))
  (is-false (disclosable-p :public :no-such-clearance)))
