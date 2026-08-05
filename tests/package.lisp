;;;; Test package for graph-db.
;;;;
;;;; The storage layers we exercise here (serialization, the allocator,
;;;; the linear hash, the skip list) are internal to GRAPH-DB and are not
;;;; part of the public export list, so we import the specific internal
;;;; symbols under test.  Keeping this list explicit doubles as an
;;;; inventory of the low-level surface the suite covers.

(in-package #:cl-user)

(defpackage #:graph-db/test
  (:use #:cl #:fiveam)
  ;; graph-db's cut symbol must win over fiveam's ! (its rerun-tests helper),
  ;; so the Prolog compiler recognizes ! in query goals as cut.
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; serialization
                #:serialize
                #:deserialize
                #:serialized-equal
                #:decode-length
                #:+vector+
                #:+float-vector+
                #:+fv-single-float+
                #:+fv-double-float+

                #:serialization-error
                ;; geometry (spatial extension)
                #:make-point
                #:make-linestring
                #:make-polygon
                #:make-multipolygon
                #:geometryp
                #:geometry-kind
                #:geometry-coordinates
                #:geometry-coordinate-pairs
                #:do-geometry-coordinates
                #:map-geometry-coordinates

                #:geometry-lon

                #:geometry-lat
                #:geometry-bbox
                #:geometry-empty-p
                ;; geometry refine ops (spatial extension)
                #:geodesic-distance
                #:point-in-ring-p
                #:point-in-polygon-rings-p
                #:geometry-contains-point-p
                #:bbox-overlap-p
                #:geometry-distance
                ;; geohash (spatial extension)
                #:geohash-encode
                #:geohash-decode
                #:geohash-bbox
                #:geohash-cell-size
                #:geohash-prefix-range
                #:geohash-covering
                #:geohash-neighbor
                #:geohash-neighbors
                ;; spatial index (spatial extension)
                #:spatial-indexes          ; graph registry: (owner . slot) -> index
                #:spatial-index-for        ; one index, or NIL if not created yet
                #:all-spatial-indexes
                #:class-spatial-index-keys
                #:graph-default-spatial-precision
                #:graph-default-spatial-max-cells
                #:node-geometry            ; write-path indexing protocol
                #:make-spatial-index
                #:open-spatial-index
                #:spatial-index-p
                #:spatial-index-address
                #:spatial-index-precision
                #:spatial-index-max-cells
                #:spatial-index-precision-counts
                #:spatial-index-coarsest-precision
                #:spatial-index-insert
                #:spatial-index-remove
                #:spatial-index-query-bbox
                #:spatial-index-query-radius
                #:delete-spatial-index
                #:rebuild-spatial-indexes
                #:regenerate-spatial-index
                #:regenerate-spatial-indexes
                ;; §8: the inert second geometry slot (exhaustive read-only sweep)
                #:audit-spatial-slots
                #:node-geometry-slots-with-values
                ;; the declaration surface for a spatial index's grid precision and max-cells
                #:spatial-precision-spec
                #:spatial-max-cells-spec

                ;; index-backed spatial queries (spatial extension)
                #:find-nodes-within
                #:find-nodes-intersecting
                #:find-nodes-near
                #:find-nearest-k
                #:find-within/3
                #:find-intersects/3
                #:find-near/5
                #:find-nearest/5
                ;; topology seam (exact only with graph-db/geos; fallback here)
                #:geometry-intersects-p
                #:geometry-contains-geometry-p
                #:geos-available-p
                #:geometry-valid-p
                ;; subset replication
                #:replication-filter
                #:make-spatial-replication-filter
                #:filter-writes
                ;; allocator / memory
                #:create-memory
                #:open-memory
                #:close-memory
                #:allocate
                #:free
                #:normalize-allocation-data-size
                #:map-memory
                #:free-list
                #:unallocated-memory-available
                #:set-byte
                #:get-byte
                #:get-bytes
                #:memory-data-offset
                #:memory-mmap
                #:memory-pointer
                ;; linear hash
                #:make-lhash
                #:open-lhash
                #:close-lhash
                #:lhash-insert
                #:lhash-get
                #:lhash-remove
                #:lhash-update
                #:map-lhash
                #:read-lhash-count
                #:duplicate-key-error
                #:nonexistent-key-error
                ;; skip list
                #:make-skip-list
                #:add-to-skip-list
                #:find-in-skip-list
                #:find-kv-in-skip-list
                #:update-in-skip-list
                #:remove-from-skip-list
                #:skip-list-count
                #:skip-list-to-list
                #:skip-list-to-node-list
                #:delete-skip-list
                #:analyze-sl-heights
                #:%sn-value
                #:%sn-key
                #:%sl-length
                ;; skip-list cursors
                #:make-cursor
                #:make-keys-cursor
                #:make-values-cursor
                #:make-range-cursor
                #:cursor-next
                #:map-skip-list
                #:map-skip-list-keys
                #:map-skip-list-values
                #:skip-list-fetch-all
                ;; index list
                #:make-index-list
                #:index-list-push
                #:index-list-pushnew
                #:index-list-member-p
                #:map-index-list
                #:remove-from-index-list
                #:index-list-head
                #:delete-index-list
                ;; vector segment (segment.lisp)
                #:create-vector-segment
                #:open-vector-segment
                #:close-vector-segment
                #:segment-capacity
                #:segment-live-count
                #:segment-dimension
                #:segment-id->slot
                #:segment-put
                #:segment-get
                #:segment-remove
                #:segment-clean-shutdown-p
                #:rebuild-vector-segment
                #:rebuild-vector-segment-batched
                #:%id-less-p
                #:%score-before-p
                #:%make-topk
                #:%topk-offer
                #:%topk-results
                #:%cosine
                #:segment-scan
                #:segment-score-subset
                #:vector-search
                ;; type index
                #:make-type-index
                #:close-type-index
                #:type-index-push
                #:type-index-remove
                #:get-type-index-list
                ;; graph model / public API
                #:*graph*
                #:make-graph
                #:open-graph
                #:close-graph
                #:with-transaction
                #:lookup-vertex
                #:lookup-edge
                #:vertex-history
                #:outgoing-edges
                #:incoming-edges
                #:traverse
                #:traversal-path
                #:end-vertex
                #:map-vertices
                #:map-edges
                #:def-index
                #:index-lookup
                #:index-range
                #:map-index
                #:weight
                #:to
                #:from
                #:id
                #:deleted-p
                #:mark-deleted
                #:copy
                #:save
                #:def-vertex
                #:def-edge
                #:node-slot-value
                #:*schema-node-metadata*
                #:duplicate-node-class-error
                ;; views
                #:def-view
                #:invoke-graph-view
                #:map-view
                #:map-reduced-view
                #:delete-view
                #:regenerate-view
                #:yield
                #:string-id
                ;; prolog queries.  The query compiler interns functor symbols
                ;; in the current package, so the built-in functors a query
                ;; references must be accessible here by name.  Edge functors
                ;; (e.g. g-knows/2) are generated in this package already.
                #:select
                #:select-flat
                #:select-one
                #:select-count
                #:do-query
                #:def-query
                #:def-rest-procedure
                #:is-a/2
                #:select/2
                #:param/2
                #:throw/1
                #:catch/3
                #:node-slot-value/3
                #:retract/1
                #:retract/3
                #:trigger/1
                #:not/1
                #:bagof/3
                #:setof/3
                #:findall/3
                #:repeat/0
                ;; comparison / arithmetic / type / control built-ins
                #:=/2
                #:==/2
                #:/=/2
                #:>/2
                #:</2
                #:>=/2
                #:<=/2
                #:is/2
                #:numberp/1
                #:atom/1
                #:var/1
                #:lisp/2
                #:regex-match/2
                #:call/1
                #:if/2
                #:if/3
                #:unique/1
                ;; spatial predicates (spatial extension)
                #:geo-distance/5
                #:geo-near/5
                #:geo-within/3
                ;; rule definition + functor-table access (for stress tests).
                ;; (! is shadowing-imported above.)
                #:<-
                #:lookup-functor
                #:delete-functor
                #:make-functor-symbol
                ;; misc helpers
                #:gen-id)
  (:export #:run-tests
           #:graph-db-suite))
