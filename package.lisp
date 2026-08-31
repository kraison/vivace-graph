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
  (:export
           ;; image-level epoch clock (GH #168)
           #:*system-clock*
           #:system-clock
           #:open-system-clock
           #:close-system-clock
           #:system-clock-in-use
           #:system-clock-in-use-location
           #:clock-next-epoch
           #:clock-current-epoch
           #:clock-peek-epoch
           #:clock-observe-epoch
           #:clock-lease-epochs
           #:journal-append
           #:journal-records
           #:system-journal-corrupt
           #:journal-corrupt-file
           #:journal-corrupt-position
           #:journal-corrupt-cause
           #:system-journal-torn-tail
           #:journal-torn-file
           #:journal-torn-position
           #:graph-system-clock
           #:attach-to-system-clock
           ;; Detach quiescence protocol (GH #170).
           #:accepting-p
           #:store-not-accepting-error
           #:store-not-accepting-name
           #:store-not-accepting-reason
           #:detach-drain-timeout
           #:detach-timeout-name
           #:detach-timeout-seconds
           #:store-detachment
           #:store-detachment-graph-name
           #:store-detachment-location
           #:store-detachment-store-id
           #:store-detachment-lease-start
           #:store-detachment-lease-end
           #:detach-store
           #:reattach-store
           #:pin-read-epoch
           #:unpin-read-epoch
           #:detach-unsupported-graph-error
           #:detach-unsupported-graph-error-graph
           #:detach-unsupported-graph-error-operation
           ;; Shadow generations (GH #170).
           #:shadow-store
           #:abandon-shadow
           #:open-shadow-graph
           #:discard-shadow
           #:swap-in-shadow
           #:swap-recovered-warning
           #:swap-recovered-warning-original
           #:shadow-recovery-failed
           #:shadow-recovery-failed-original
           #:shadow-recovery-failed-recovery
           #:graph-shadow-p
           #:graph-epoch-lease
           #:epoch-lease
           #:epoch-lease-start
           #:epoch-lease-next
           #:epoch-lease-end
           #:epoch-lease-exhausted
           #:epoch-lease-exhausted-name
           #:epoch-lease-exhausted-end
           ;; Recovery policy + WAL-suppressed fast path (GH #170 Task 4).
           #:store-recovery-policy
           #:set-store-recovery-policy
           #:fast-load-requires-derivable
           #:fast-load-requires-derivable-location
           #:fast-load-requires-derivable-policy
           #:wal-suppressed-p
           #:recovery-policy-mismatch-warning
           #:recovery-policy-mismatch-warning-location
           #:recovery-policy-mismatch-warning-requested
           #:recovery-policy-mismatch-warning-on-disk
           ;; whole-system restore (GH #171)
           #:retired-generations #:swap-record-missing-warning
           #:swap-record-missing-path
           #:generation-store #:generation-location #:generation-retired
           #:generation-swap-epoch #:generation-live-from #:generation-eras
           #:generation-journaled-p
           #:generation-present-p #:generation-policy
           #:prune-retired-generations #:retention-required-error
           #:retention-required-generations
           #:plan-system-restore #:restore-refused-error
           #:restore-refused-reasons #:restore-refused-epoch
           #:restore-system #:restore-inexact-warning
           #:restore-inexact-manifest #:read-restore-manifest
           #:repair-interrupted-swap
           ;; image-level type-id registry (GH #186)
           #:type-registry
           #:open-type-registry
           #:close-type-registry
           #:type-registry-busy
           #:type-registry-busy-location
           #:type-registry-package-missing-error
           #:type-registry-package-missing-name
           #:type-registry-package-missing-file
           #:registry-id-for
           #:registry-intern
           #:registry-entries
           #:*system-directory*
           #:*type-registry*
           #:system-directory-required
           #:system-directory-required-operation
           ;; .dirty refusal (GH #246)
           #:store-not-closed-cleanly-error
           #:store-not-closed-location
           #:dirty-marker-already-gone-warning
           #:dirty-marker-already-gone-location
           ;; edge store-occupancy sidecar (GH #167)
           #:edge-type-stores
           ;; image-level store-id registry (GH #169)
           #:ensure-store-registry
           #:store-registry-intern
           #:store-registry-id-for
           #:store-registry-name-for
           #:store-registry-full
           #:store-id
           #:+store-tag-bits+
           #:+max-store-tag+
           #:uuid-v8-p
           #:id-store-tag
           ;; GH #169: the tagged-id resolver and detached-read markers.
           #:resolve-node-graph
           #:lookup-vertex-anywhere
           #:unresolved-node
           #:unresolved-node-p
           #:unresolved-node-id
           #:unresolved-node-store-id
           #:unresolved-node-store-name
           #:store-detached-error
           #:store-detached-name
           #:store-detached-id
           #:store-id-collision-error
           #:store-id-collision-id
           #:store-id-collision-existing-name
           #:store-id-collision-existing-location
           #:store-id-collision-new-name
           #:store-id-collision-new-location
           ;; GH #169: BACKUP warns on a dangling cross-store edge.
           #:dangling-edge-warning
           #:dangling-edge-id
           #:dangling-edge-endpoint-id
           #:dangling-edge-store-id
           ;; GH #186: a store's persisted type-ids disagree with the
           ;; image registry.  Operator-facing -- the remedy is a seeding
           ;; run plus a renumbering migration, not a retry.
           #:store-registry-conflict
           #:store-registry-conflict-location
           #:store-registry-conflict-type-name
           #:store-registry-conflict-parent
           #:store-registry-conflict-store-id
           #:store-registry-conflict-registry-id
           #:store-registry-conflict-reason
           #:store-registry-conflict-holder
           ;; GH #186: replication refuses a graph opened frozen.
           #:frozen-graph-cannot-replicate
           #:frozen-graph-cannot-replicate-graph-name
           #:frozen-graph-cannot-replicate-location
           ;; Open a store the registry disagrees with, to read it.
           #:with-schema-frozen
           ;; tm-next-epoch/tm-current-epoch/tm-peek-epoch are internal:
           ;; TRANSACTION-MANAGER itself is unexported, and TM-NEXT-EPOCH
           ;; burns epochs -- not consumer API (GH #179).
           #:make-graph
           #:*default-heap-size*
           #:*default-index-size*
           #:*index-backend*
           #:graph-index-backend
           #:graph-spatial-index-backend
           #:open-graph
           #:close-graph
           #:lookup-graph
           #:graph-stats
           #:check-data-integrity
           #:snapshot
           #:replay
           #:restore
           ;; Vector segments (dense-vector index + cosine kNN query layer).
           #:vector-search
           #:rebuild-vector-segment-batched
           #:segment-scan
           #:segment-score-subset
           ;; Presize a segment's capacity up front (GH #170 Task 5) -- turns
           ;; a mid-apply VECTOR-SEGMENT-CAPACITY-EXHAUSTED into an upfront one.
           #:presize-vector-segment
           ;; Signalled pre-durability when a commit would grow a segment past
           ;; its mmap reservation.  Exported so a caller can tell "reopen the
           ;; graph / raise the reservation and retry" apart from a genuine data
           ;; error; the accessors go with it (as UCV-* do) so that decision can
           ;; be made from the numbers rather than by parsing the report text.
           #:vector-segment-capacity-exhausted
           #:vsce-owner #:vsce-slot #:vsce-required
           #:vsce-reserved #:vsce-needed-bytes
           #:vsce-path #:vsce-reason
           ;; The one knob that actually raises a segment's mmap reservation
           ;; ceiling (see VECTOR-SEGMENT-CAPACITY-EXHAUSTED's report and
           ;; %SEG-RESERVATION-FOR).  Exported so raising it before OPEN-GRAPH
           ;; is a supported call, not the internal-symbol surgery
           ;; (GRAPH-DB::*SEGMENT-MIN-RESERVATION*) it used to require;
           ;; *MMAP-RESERVATION-MULTIPLIER* and *MMAP-MIN-RESERVATION*
           ;; deliberately stay internal here -- they reach every mapped file
           ;; in the graph (heap, indexes, linear hashes), not just segments,
           ;; so exporting them is a broader API decision than this fix.
           #:*segment-min-reservation*
           ;; The kill-switch for growth-by-relocation.  Exported for the same
           ;; reason as the floor above: turning relocation off is a supported
           ;; operational decision (it restores the old, strictly-safe
           ;; pre-durability abort), not internal-symbol surgery.
           #:*segment-relocate-on-exhaustion*
           ;; The kill-switch for the CHEAP half of the same mechanism: growth
           ;; by claiming the adjacent address range, which relocation is only
           ;; the fallback for.  Exported alongside it for the same reason.
           #:*segment-extend-adjacent-on-exhaustion*
           #:location
           #:schema
           #:indexes
           #:*graph*
           #:execute-tx
           #:transaction-p
           #:graph-name
           #:transaction-error
           #:cross-graph-transaction-error
           #:duplicate-node-class-error
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
           #:*read-snapshots*
           #:read-transaction
           #:no-transaction-in-progress
           #:attach-with-active-transactions
           #:mutating-unregistered-node
           #:copying-uncommitted-node

           #:def-node-type
           #:def-vertex
           #:def-edge
           #:edge-exists-p
           #:lookup-node-type-by-name
           #:ambiguous-node-type-name
           #:ambiguous-type-name
           #:ambiguous-type-parent
           #:ambiguous-type-candidates
           #:divergent-node-type-redefinition
           #:divergent-type-name
           #:divergent-type-graph-name
           #:divergent-type-other-graphs
           #:schema-graph-name-cross-file-style-warning
           #:cross-file-graph-name
           #:cross-file-registering-file
           #:cross-file-previous-file

           ;; packages as namespaces (GH #167)
           #:default-store-not-open-error
           #:default-store-not-open-class
           #:default-store-not-open-store

           ;; runtime schema definition API (GH #172)
           #:ensure-namespace
           #:create-vertex-type
           #:create-edge-type
           #:materialize-schema
           #:register-schema-function
           #:find-schema-function
           #:schema-function-unresolved
           #:materialize-unresolved-functions
           #:unresolved-function-names
           #:materialize-unresolved-parents
           #:unresolved-parent-names
           #:instantiate-node-type
           #:*schema-node-metadata*
           #:read-schema-manifest

           ;; shared sidecar print/read discipline (GH #226, #227)
           #:sidecar-records-skipped
           #:sidecar-skipped-file
           #:sidecar-skipped-count
           #:sidecar-skipped-first-position

           ;; visibility tooling (GH #172, R6)
           #:describe-schema
           #:export-schema-source
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
           ;; MVCC: public read path over the versions KEEP-REVISIONS retains
           #:vertex-history
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
           ;; multi-slot uniqueness constraint (issue #107)
           #:def-unique
           ;; general ordered index (:index slot option / def-index)
           #:def-index
           ;; schema retraction: withdraw a declaration (GH #139, #140)
           #:undef-index #:undef-unique
           ;; declarative value constraints (GH #149)
           #:def-value-constraint #:undef-value-constraint
           #:value-constraint-violation
           #:vcv-class-name #:vcv-slot-name #:vcv-value
           #:vcv-expected #:vcv-reason #:vcv-node-id
           #:check-value-constraints
           ;; Cardinality constraints (GH #155)
           #:def-cardinality #:undef-cardinality
           #:cardinality-violation
           #:cdv-class-name #:cdv-edge-type #:cdv-direction
           #:cdv-actual #:cdv-min #:cdv-max #:cdv-node-id
           #:check-cardinality-constraints
           #:vc-violation-spec #:vc-violation-node-id
           #:vc-violation-class-name #:vc-violation-slot
           #:vc-violation-actual #:vc-violation-expected
           #:vc-violation-reason
           #:unregister-index-spec #:unregister-unique-tuple-spec
           #:index-lookup #:index-range #:map-index
           ;; index-backed generator predicates for Prolog (GH #102)
           #:find-by-slot/4 #:find-slot-range/5
           #:rebuild-secondary-indexes #:regenerate-secondary-indexes

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
           #:geometry-coordinate-pairs
           #:do-geometry-coordinates
           #:map-geometry-coordinates
           #:geometry-lon
           #:geometry-lat
           #:geometry-bbox
           #:geometry-empty-p
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
           #:geometry-geodesic-area
           #:geometry-geodesic-length
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
           #:spatial-indexes
           #:spatial-index-for
           #:all-spatial-indexes
           #:class-spatial-index-keys
           #:graph-default-spatial-precision
           #:graph-default-spatial-max-cells
           #:spatial-precision-spec
           #:spatial-max-cells-spec
           #:spatial-index-p
           #:make-spatial-index
           #:open-spatial-index
           #:spatial-index-precision
           #:spatial-index-max-cells
           #:spatial-index-precision-counts
           #:spatial-index-coarsest-precision
           #:spatial-index-address
           #:spatial-index-insert
           #:spatial-index-remove
           #:spatial-index-query-bbox
           #:spatial-index-query-radius
           #:map-spatial-index-bbox
           #:map-spatial-index-radius
           #:delete-spatial-index
           #:rebuild-spatial-indexes
           #:regenerate-spatial-index
           #:regenerate-spatial-indexes
           ;; §8: the inert second geometry slot.  Only the FIRST geometry-valued
           ;; :INDEX slot of a class is ever indexed; AUDIT-SPATIAL-SLOTS is the
           ;; exhaustive read-only sweep for the rest, and
           ;; NODE-GEOMETRY-SLOTS-WITH-VALUES is the per-node predicate behind it.
           #:audit-spatial-slots
           #:node-geometry-slots-with-values
           ;; declaring a spatial index's grid precision: the reader for the
           ;; (slot :spatial-precision N) slot option, its one declaration surface
           #:spatial-precision-spec
           #:install-spatial-indexes
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
           #:find-within/3
           #:find-intersects/3
           #:find-near/5
           #:find-nearest/5
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
