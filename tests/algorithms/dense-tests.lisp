;;;; Mode-A dense-family tests: Floyd-Warshall all-pairs (cross-validated against
;;;; native shortest-path), edge-betweenness/edge-span clustering, minimal cut.

(in-package #:graph-db/algorithms-test)

(def-suite dense-suite
  :description "All-pairs shortest paths, clustering, minimal cut (Mode A)."
  :in graph-db-algorithms-suite)

(in-suite dense-suite)

;;; ---- All-pairs (reuses the weighted diamond from shortest-path-tests) ----

(test all-pairs-distance-and-path
  "Weighted all-pairs agrees with the known diamond distances/path."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (let ((apsp (all-pairs-shortest-paths :graph g :directed t)))
      (is (approx= 3.0 (apsp-distance apsp (gethash "A" h) (gethash "D" h))))
      (is (approx= 2.0 (apsp-distance apsp (gethash "A" h) (gethash "C" h))))
      (is (equal '("A" "B" "C" "D")
                 (path-names (apsp-path apsp (gethash "A" h) (gethash "D" h))))))))

(test all-pairs-matches-native
  "All-pairs weighted distance == native shortest-path cost for every reachable
pair."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (let ((apsp (all-pairs-shortest-paths :graph g :directed t)))
      (dolist (pair '(("A" . "B") ("A" . "C") ("A" . "D") ("B" . "D") ("C" . "D")))
        (let ((from (gethash (car pair) h)) (to (gethash (cdr pair) h)))
          (multiple-value-bind (np nc) (shortest-path from to :graph g)
            (declare (ignore np))
            (is (approx= nc (apsp-distance apsp from to))
                "~A->~A" (car pair) (cdr pair))))))))

(test all-pairs-unreachable
  "No directed path D->A: all-pairs distance is NIL."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (let ((apsp (all-pairs-shortest-paths :graph g :directed t)))
      (is (null (apsp-distance apsp (gethash "D" h) (gethash "A" h)))))))

;;; ---- Clustering ----

;; Two triangles joined by a single bridge edge C-D.
(defparameter +bridge-nodes+ '("A" "B" "C" "D" "E" "F"))
(defparameter +bridge-edges+
  '(("A" "B") ("B" "C") ("C" "A")        ; triangle 1
    ("D" "E") ("E" "F") ("F" "D")        ; triangle 2
    ("C" "D")))                          ; bridge

(defun unordered-names (edge)
  "Sorted (name1 name2) of a clustering edge triple or cut pair."
  (sort (list (slot-value (first edge) 'name) (slot-value (second edge) 'name))
        #'string<))

(test clustering-edge-betweenness-finds-bridge
  "Edge-betweenness clustering removes the inter-cluster bridge C-D first."
  (with-populated-graph (g h +bridge-nodes+ +bridge-edges+)
    (let ((removed (graph-clustering :graph g :directed nil
                                     :method :edge-betweenness
                                     :edge-removal-count 1)))
      (is (= 1 (length removed)))
      (is (equal '("C" "D") (unordered-names (first removed)))))))

(test clustering-edge-span-removal-count
  "Edge-span clustering removes exactly the requested number of edges."
  (with-populated-graph (g h +bridge-nodes+ +bridge-edges+)
    (let ((removed (graph-clustering :graph g :directed nil :method :edge-span
                                     :edge-removal-count 2)))
      (is (= 2 (length removed))))))

;;; ---- Minimal cut ----

(test minimum-cut-single-edge
  "On a 2-node single-edge graph, the minimal cut is that one edge."
  (with-populated-graph (g h '("A" "B") '(("A" "B")))
    (let ((cut (minimum-cut :graph g :directed nil)))
      (is (= 1 (length cut)))
      (is (equal '("A" "B") (unordered-names (first cut)))))))
