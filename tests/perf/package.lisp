(defpackage #:graph-db/perf-test
  (:use #:cl)
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:make-graph
                #:open-graph
                #:close-graph
                #:*graph*
                #:snapshot
                #:replay
                ;; schema
                #:def-vertex
                #:def-edge
                #:def-view
                #:*schema-node-metadata*
                ;; node operations
                #:lookup-vertex
                #:lookup-edge
                #:map-vertices
                #:map-edges
                #:outgoing-edges
                #:incoming-edges
                #:id
                #:string-id
                #:deleted-p
                #:copy
                #:save
                #:mark-deleted
                ;; transactions
                #:with-transaction
                ;; views
                #:invoke-graph-view
                #:map-view
                #:index-lookup
                #:index-range
                #:map-index
                #:yield
                ;; prolog
                #:select-flat
                #:is-a/2
                #:select/2
                #:node-slot-value/3
                ;; uuid
                #:gen-id)
  (:export #:run-perf
           #:compare-perf
           #:perf-suite              ; alias entry point used by the headless driver
           #:*perf-scale*))
