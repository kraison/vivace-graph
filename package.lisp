(in-package #:cl-user)

(defpackage #:graph-db
  (:use #:cl
        #:bordeaux-threads
        #:local-time
        #+ccl #:closer-mop
        #+lispworks #:clos
        #+ecl #:clos
        #+sbcl #:sb-mop
        #+sbcl #:sb-pcl)
  #+sbcl (:shadowing-import-from "SB-EXT" "WORD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "FINALIZE-INHERITANCE")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-GENERIC-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFMETHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFGENERIC")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-CLASS")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-DISCRIMINATING-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-APPLICABLE-METHODS-USING-CLASSES")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-EFFECTIVE-METHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "METHOD-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "MAKE-METHOD-LAMBDA")
  (:export #:make-graph
           #:*default-heap-size*
           #:*default-index-size*
           #:*index-backend*
           #:graph-index-backend
           #:open-graph
           #:close-graph
           #:lookup-graph
           #:graph-stats
           #:check-data-integrity
           #:snapshot
           #:replay
           #:restore
           #:location
           #:schema
           #:indexes
           #:*graph*
           #:execute-tx
           #:transaction-p
           #:graph-name
           #:transaction-error
           #:master-host
           #:replication-port
           #:slave-socket
           #:replication-key
           #:master-txn-id
           #:stop-replication-p
           #:execute-tx-action
           #:write-last-txn-id
           #:read-last-txn-id
           #:start-replication
           #:stop-replication
           #:stop-buffer-pool
           #:set-buffer-pool-size
           #:*buffer-pool-size*

           #:start-rest
           #:stop-rest
           #:def-rest-procedure
           #:*rest-procedures*
           #:def-query
           #:*rest-queries*
           #:*query-params*
           #:*query-default-limit*
           #:*query-default-max-inferences*
           #:*query-default-timeout*
           #:query-param-error

           #:with-transaction
           #:with-read-snapshot
           #:call-with-read-snapshot
           #:lookup-object
           #:update-node
           #:delete-node
           #:commit
           #:rollback
           #:*transaction*
           #:no-transaction-in-progress

           #:def-node-type
           #:def-vertex
           #:def-edge
           #:edge-exists-p
           #:lookup-node-type-by-name
           #:instantiate-node-type
           #:*schema-node-metadata*
           #:with-write-locked-class
           #:with-read-locked-class
           #:schema-class-locks
           #+(or sbcl ecl) #:make-rw-lock
           #+(or sbcl ecl) #:with-read-lock
           #+(or sbcl ecl) #:with-write-lock
           #+(or sbcl ecl) #:acquire-read-lock
           #+(or sbcl ecl) #:release-read-lock
           #+(or sbcl ecl) #:acquire-write-lock
           #+(or sbcl ecl) #:release-write-lock
           #+(or sbcl ecl) #:rw-lock-p

           #:vertex
           #:edge
           #:generic-edge
           #:generic-vertex
           #:make-vertex
           #:make-edge
           #:lookup-vertex
           #:lookup-edge
           #:to
           #:from
           #:weight
           #:id
           #:string-id
           #:node-to-alist
           #:type-id
           #:revision
           #:deleted-p
           #:active-edge-p
           #:data
           #:traverse
           #:traversal-path
           #:end-vertex
           #:map-vertices
           #:map-edges
           #:outgoing-edges
           #:incoming-edges
           #:node-slot-value
           #:copy
           #:save
           #:mark-deleted
           #:stale-revision-error
           ;; unique constraints (issue #6)
           #:unique-constraint-violation
           #:ucv-class-name #:ucv-slot-name #:ucv-value #:ucv-existing-id
           #:rebuild-unique-indexes
           #:regenerate-unique-indexes

           #:def-view
           #:*view-rv*
           #:yield
           #:map-view
           #:map-reduced-view
           #:invoke-graph-view
           #:make-view
           #:delete-view
           #:save-views
           #:restore-views
           #:install-views
           #:get-view-table-for-class
           #:regenerate-view
           #:regenerate-all-views
           #:lookup-view-group
           #:lookup-view
           #:with-write-locked-view-group
           #:with-read-locked-view-group
           #:view-group-lock

           ;; Prolog
           #:def-global-prolog-functor
           #:def-prolog-compiler-macro
           #:compile-body
           #:args
           #:*prolog-global-functors*
           #:deref-exp
           #:unify
           #:select
           #:?
           #:?-
           #:q-
           #:!
           #:cut
           #:once
           #:forall
           #:call
           #:var-deref
           #:undo-bindings
           #:replace-?-vars
           #:variables-in
           #:make-functor-symbol
           #:*trail*
           #:*var-counter*
           #:*functor*
           #:make-functor
           #:maybe-add-undo-bindings
           #:compile-clause
           #:show-prolog-vars
           #:prolog-error
           #:prolog-error-ball
           #:prolog-throw
           #:prolog-resource-error
           #:prolog-permission-error
           #:*inference-budget*
           #:*default-inference-budget*
           #:*default-query-timeout*
           #:*allowed-effects*
           #:*default-allowed-effects*
           #:require-effect
           #:prolog-ignore
           #:delete-functor
           #:set-functor-fn
           #:*seen-table*
           #:*select-flat*
           #:*select-list*
           #:select-count
           #:*select-skip*
           #:*select-current-count*
           #:*select-current-skip*
           #:select-one
           #:select-flat
           #:select-first
           #:do-query
           #:map-query
           #:valid-prolog-query-p
           #:init-prolog
           #:*prolog-graph*
           #:*prolog-trace*
           #:trace-prolog
           #:untrace-prolog
           #:make-node-table
           #:node-equal

           ;; --- spatial extension (public API) ---
           ;; geometry values
           #:geometry
           #:geometryp
           #:make-point
           #:make-linestring
           #:make-polygon
           #:make-multipolygon
           #:geometry-kind
           #:geometry-coordinates
           #:geometry-lon
           #:geometry-lat
           #:geometry-bbox
           ;; geometry operations
           #:geodesic-distance
           #:point-in-ring-p
           #:point-in-polygon-rings-p
           #:geometry-contains-point-p
           #:bbox-overlap-p
           #:geometry-distance
           ;; topology refine seam (exact with the optional graph-db/geos add-on,
           ;; dependency-free fallbacks otherwise)
           #:geometry-intersects-p
           #:geometry-contains-geometry-p
           #:geometry-make-valid
           #:geometry-valid-p
           #:geometry-distance-exact
           #:geometry-geodesic-distance
           #:geometry-union
           #:geometry-intersection
           #:geometry-difference
           #:geometry-buffer
           #:geometry-area
           #:geos-available-p
           #:geos-shutdown
           #:*geos-available-p*
           #:*geos-version*
           #:*geos-makevalid-available-p*
           #:geos-error
           #:geos-required-for-operation
           ;; geohash
           #:geohash-encode
           #:geohash-decode
           #:geohash-bbox
           #:geohash-cell-size
           #:geohash-covering
           #:geohash-neighbor
           #:geohash-neighbors
           ;; spatial index
           #:spatial-index
           #:spatial-index-p
           #:make-spatial-index
           #:open-spatial-index
           #:spatial-index-precision
           #:spatial-index-address
           #:spatial-index-insert
           #:spatial-index-remove
           #:spatial-index-query-bbox
           #:spatial-index-query-radius
           #:delete-spatial-index
           #:rebuild-spatial-index
           ;; write-path protocol (applications specialize this)
           #:node-geometry
           ;; subset replication (field devices)
           #:replication-filter
           #:make-spatial-replication-filter
           ;; index-backed queries + Prolog functors
           #:find-nodes-within
           #:find-nodes-intersecting
           #:find-nodes-near
           #:find-nearest-k
           #:find-within/2
           #:find-intersects/2
           #:find-near/4
           #:find-nearest/4
           #:geo-distance/5
           #:geo-near/5
           #:geo-within/3
           ;; graph algorithms (optional graph-db/algorithms add-on)
           ;; -- shared
           #:with-algorithm-snapshot
           #:algorithm-vertex
           #:adjacent-vertices
           #:all-vertices
           ;; -- shortest paths (Mode B native)
           #:shortest-path
           #:a-star
           #:single-source-shortest-paths
           ;; -- structure (Mode B native)
           #:out-degree
           #:in-degree
           #:degree
           #:degree-distribution
           #:distance-map
           #:connected-components
           #:spanning-tree
           #:eccentricity
           #:graph-center
           ;; -- ranking (Mode B native)
           #:page-rank
           #:page-rank-distribution
           #:hub-authority-values
           #:sim-rank
           ;; -- in-memory projection (Mode A)
           #:with-graph-projection
           #:build-projection
           #:projection
           #:projection-index
           #:projection-vertex
           #:projection-shortest-path
           ;; -- dense / matrix family (Mode A projection)
           #:all-pairs-shortest-paths
           #:all-pairs-result
           #:apsp-distance
           #:apsp-path
           #:graph-clustering
           #:minimum-cut
           ;; -- flow family (Mode A projection)
           #:maximum-flow
           #:bipartite-p
           #:maximum-matching
           ;; -- random graph generation (transactional builders)
           #:generate-graph
           ;; -- io: import + Graphviz export (optional graph-db/algorithms-io)
           #:import-gml
           #:import-pajek
           #:graph->dot
           #:visualize
           ))
