;;;; Test package for graph-db/algorithms.
;;;;
;;;; A standalone FiveAM suite for the optional graph-algorithms add-on.  Like
;;;; the main graph-db test package, we :use only CL + FiveAM and import the
;;;; specific graph-db symbols under test, rather than :use-ing graph-db (which
;;;; would clash with CL/FiveAM on a few names).

(in-package #:cl-user)

(defpackage #:graph-db/algorithms-test
  (:use #:cl #:fiveam)
  (:import-from #:graph-db
                ;; schema + graph lifecycle
                #:def-vertex #:def-edge #:make-graph #:close-graph
                #:with-transaction #:*graph* #:lookup-vertex #:id
                #:weight #:from #:to
                ;; algorithm API under test
                #:shortest-path #:a-star #:single-source-shortest-paths
                #:out-degree #:in-degree #:degree #:degree-distribution
                #:distance-map #:connected-components #:spanning-tree
                #:eccentricity #:graph-center
                #:page-rank #:page-rank-distribution #:hub-authority-values
                #:sim-rank
                #:adjacent-vertices #:all-vertices
                #:with-algorithm-snapshot #:algorithm-vertex
                #:with-graph-projection #:build-projection
                #:projection-index #:projection-vertex
                #:projection-shortest-path
                #:all-pairs-shortest-paths #:apsp-distance #:apsp-path
                #:graph-clustering #:minimum-cut
                #:maximum-flow #:bipartite-p #:maximum-matching
                #:generate-graph
                #:import-gml #:import-pajek #:graph->dot #:visualize
                ;; Prolog query API + functor name/arity symbols (so queries
                ;; written in this package resolve to the graph-db functors)
                #:select #:select-flat #:select-one #:do-query
                #:select/2 #:is-a/2 #:node-slot-value/3
                #:shortest-path/4 #:distance/3 #:reachable/2
                #:connected-component/2 #:degree/2 #:page-rank/2
                #:authority-value/2 #:hub-value/2)
  (:export #:run-algorithm-tests))
