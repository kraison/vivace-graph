;;;; Mode-A projection tests: builder correctness, index round-trip, and
;;;; cross-validation of the vendored graph-utils hop-count Dijkstra against the
;;;; native unweighted shortest-path.

(in-package #:graph-db/algorithms-test)

(def-suite projection-suite
  :description "In-memory projection: build, round-trip, native cross-check."
  :in graph-db-algorithms-suite)

(in-suite projection-suite)

(defparameter +proj-nodes+ '("A" "B" "C" "D" "E"))
;; A->B->C->D, plus a shortcut A->C and a spur C->E
(defparameter +proj-edges+
  '(("A" "B") ("B" "C") ("C" "D") ("A" "C") ("C" "E")))

(test build-projection-counts
  "A directed projection has one index per vertex and one edge per VG edge."
  (with-populated-graph (g h +proj-nodes+ +proj-edges+)
    (let ((proj (build-projection :graph g :directed t)))
      h                                 ; (bound by the fixture; unused here)
      (is (= 5 (graph-db.projection:node-count (graph-db::projection-pgraph proj))))
      (is (= 5 (graph-db.projection:edge-count (graph-db::projection-pgraph proj)))))))

(test projection-index-round-trip
  "projection-index and projection-vertex invert one another."
  (with-populated-graph (g h +proj-nodes+ +proj-edges+)
    (let* ((va (gethash "A" h))
           (idx (projection-index (build-projection :graph g :directed t) va g)))
      (is (integerp idx))
      ;; rebuild (projections are transient) and check vertex identity by id
      (let* ((proj (build-projection :graph g :directed t))
             (i2 (projection-index proj va g)))
        (is (equalp (id va) (id (projection-vertex proj i2))))))))

(test projection-matches-native-hops
  "Mode-A hop count == Mode-B unweighted cost for every reachable pair."
  (with-populated-graph (g h +proj-nodes+ +proj-edges+)
    (dolist (pair '(("A" . "D") ("A" . "E") ("A" . "C") ("B" . "E") ("A" . "B")))
      (let ((from (gethash (car pair) h))
            (to (gethash (cdr pair) h)))
        (multiple-value-bind (npath ncost)
            (shortest-path from to :graph g :unweighted t)
          (declare (ignore npath))
          (multiple-value-bind (ppath pcost)
              (projection-shortest-path from to :graph g :directed t)
            (declare (ignore ppath))
            (is (eql ncost pcost)
                "~A->~A: native ~A vs projection ~A"
                (car pair) (cdr pair) ncost pcost)))))))

(test projection-unreachable-agreement
  "Both modes agree a directed-unreachable pair has no path."
  (with-populated-graph (g h +proj-nodes+ +proj-edges+)
    (let ((from (gethash "D" h)) (to (gethash "A" h)))
      (multiple-value-bind (np nc) (shortest-path from to :graph g :unweighted t)
        (declare (ignore np))
        (multiple-value-bind (pp pc)
            (projection-shortest-path from to :graph g :directed t)
          (declare (ignore pp))
          (is (null nc))
          (is (null pc)))))))
