;;;; tests/query/package.lisp -- graph-db/query-test (GH #322).

(defpackage #:graph-db/query-test
  (:use #:cl #:fiveam)
  (:import-from #:graph-db #:def-vertex #:def-edge #:make-graph
                #:close-graph #:with-transaction #:string-id)
  (:export #:run-query-tests #:query-suite))
