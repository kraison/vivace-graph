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
