(defpackage #:graph-db/concurrent-stress-test
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:make-graph
                #:close-graph
                #:*graph*
                ;; schema
                #:def-vertex
                #:def-edge
                #:def-view
                #:*schema-node-metadata*
                ;; node operations
                #:lookup-vertex
                #:map-vertices
                #:map-edges
                #:outgoing-edges
                #:id
                #:deleted-p
                #:copy
                #:save
                #:mark-deleted
                ;; transactions
                #:with-transaction
                #:*maximum-transaction-attempts*
                ;; views
                #:invoke-graph-view
                #:map-view
                #:map-reduced-view
                #:index-lookup
                #:index-range
                #:map-index
                #:unique-constraint-violation
                #:yield
                #:def-view
                ;; prolog
                #:select-flat
                #:is-a/2
                #:select/2
                ;; rw-locks (for run-threads helper)
                #:make-rw-lock
                #:with-read-lock
                #:with-write-lock
                ;; misc
                #:gen-id)
  (:export #:run-concurrent-stress-tests
           #:concurrent-stress-suite
           #:*stress-thread-count*
           #:*collect-timings*))
