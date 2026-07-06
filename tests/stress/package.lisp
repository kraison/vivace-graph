(defpackage #:graph-db/stress-test
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:make-graph
                #:open-graph
                #:close-graph
                #:*graph*
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
                ;; prolog
                #:select-flat
                #:is-a/2
                #:select/2
                #:node-slot-value/3
                ;; storage primitives
                #:make-lhash
                #:close-lhash
                #:lhash-insert
                #:lhash-get
                #:read-lhash-count
                #:create-memory
                #:close-memory
                #:make-skip-list
                #:add-to-skip-list
                #:skip-list-to-list
                #:skip-list-count
                #:serialize
                #:deserialize
                ;; allocator
                #:allocate
                #:free
                ;; uuid
                #:gen-id)
  (:export #:run-stress-tests
           #:stress-suite
           #:*stress-scale*
           #:*collect-timings*))
