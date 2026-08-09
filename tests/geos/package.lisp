;;;; Test package for the optional GEOS integration (graph-db/geos).
;;;;
;;;; Imports the GEOS add-on's symbols (most are internal to GRAPH-DB) plus the
;;;; geometry constructors and spatial queries the tests exercise.

(in-package #:cl-user)

(defpackage #:graph-db/geos-test
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; availability flags + loader
                #:*geos-available-p*
                #:*geos-version*
                #:*geos-makevalid-available-p*
                #:load-geos
                ;; geometry values
                #:geometry
                #:geometryp
                #:geometry-kind
                #:geometry-coordinates
                #:make-point
                #:make-linestring
                #:make-polygon
                #:make-multipolygon
                #:geometry-bbox
                #:geometry-empty-p
                ;; bridge + context (S1)
                #:geometry->wkt
                #:wkt->geometry
                #:geometry->geos
                #:geos->geometry
                #:with-geos-context
                #:geos-shutdown
                #:*geos-pool*
                #:*geos-pool-created*
                #:*geos-pool-in-use*
                #:*geos-pool-peak*
                #:*geos-pool-debug*
                ;; topology seam (S2)
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
                #:geometry-distance
                #:geometry-contains-point-p
                #:geos-available-p
                #:geos-error
                #:geos-required-for-operation
                ;; graph + queries (S3 query tests)
                #:*graph*
                #:make-graph
                #:close-graph
                #:with-transaction
                #:def-vertex
                #:id
                #:lookup-vertex
                #:mark-deleted
                #:map-vertices
                #:node-geometry
                #:all-spatial-indexes
                #:find-nodes-intersecting
                #:find-nodes-within
                #:find-nodes-near
                #:*schema-node-metadata*)
  (:export #:run-geos-tests
           #:geos-suite
           #:run-geos-perf))
