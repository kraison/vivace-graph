;;;; Package definition for the graph-db concurrency / thread-safety test suite.
;;;;
;;;; Kept separate from graph-db/test so the concurrency suite can be loaded
;;;; independently without pulling in the sequential unit suite.

(in-package #:cl-user)

(defpackage #:graph-db/concurrency-test
  (:use #:cl #:fiveam)
  ;; graph-db's CUT symbol must win over fiveam's rerun-tests helper.
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:*graph*
                #:make-graph
                #:close-graph
                #:with-transaction
                ;; node operations
                #:lookup-vertex
                #:map-vertices
                #:map-edges
                #:mark-deleted
                #:copy
                #:save
                #:id
                #:to
                #:from
                #:weight
                #:def-vertex
                #:def-edge
                #:*schema-node-metadata*
                ;; views
                #:def-view
                #:invoke-graph-view
                #:map-view
                #:yield
                ;; prolog
                #:select-flat
                #:<-
                #:lookup-functor
                #:delete-functor
                #:make-functor-symbol
                #:make-functor
                #:is-a/2
                #:select/2
                #:node-slot-value/3
                ;; rw-lock -- exported for sbcl/ecl, internal for ccl but
                ;; (:import-from) doesn't require export status
                #:make-rw-lock
                #:with-read-lock
                #:with-write-lock
                #:acquire-write-lock
                #:release-write-lock
                #:rw-lock-p
                ;; acquire/release-read-lock and the lock-* accessors exist only
                ;; on the custom-lock impls; CCL uses a native lock (no read
                ;; acquire/release fns, no such slots) via utilities.lisp.
                #+(or sbcl lispworks ecl) #:acquire-read-lock
                #+(or sbcl lispworks ecl) #:release-read-lock
                #+(or sbcl lispworks ecl) #:lock-readers
                #+(or sbcl lispworks ecl) #:lock-writer
                ;; misc
                #:gen-id)
  ;; Internal graph-db symbols the tests need but that are not exported.
  ;; Using (:import-from) works on unexported symbols too.
  (:import-from #:graph-db
                #:*maximum-transaction-attempts*
                #:functor-clauses
                #:add-functor-clause
                #:reset-functor)
  ;; Storage-layer internals for data-structure concurrency tests.
  (:import-from #:graph-db
                ;; lhash direct access
                #:make-lhash
                #:close-lhash
                #:lhash-insert
                #:lhash-get
                #:read-lhash-count
                ;; skip-list direct access
                #:create-memory
                #:close-memory
                #:make-skip-list
                #:add-to-skip-list
                #:skip-list-to-list
                #:serialize
                #:deserialize
                ;; allocator
                #:allocate
                #:heap
                ;; node flags
                #:written-p
                ;; edge traversal
                #:outgoing-edges)
  ;; Spatial extension: geometry constructors, the index, and the index-backed
  ;; queries exercised by the spatial concurrency tests.
  (:import-from #:graph-db
                #:geometry
                #:make-point
                #:make-polygon
                #:geometryp
                #:all-spatial-indexes
                #:spatial-index-query-bbox
                #:spatial-index-query-radius
                #:find-nodes-near
                #:find-nodes-within
                #:node-geometry)
  (:export #:run-concurrency-tests
           #:concurrency-suite))
