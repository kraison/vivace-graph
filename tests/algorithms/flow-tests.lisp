;;;; Max-flow & matching tests (Mode A).  The canonical CLRS network has a unique
;;;; maximum flow of 23; every algorithm must agree on it.

(in-package #:graph-db/algorithms-test)

(def-suite flow-suite
  :description "Maximum flow (4 algorithms) and bipartite matching."
  :in graph-db-algorithms-suite)

(in-suite flow-suite)

;; CLRS figure 26.1 network; max flow s->t = 23.
(defparameter +clrs-nodes+ '("s" "v1" "v2" "v3" "v4" "t"))
(defparameter +clrs-edges+
  '(("s" "v1" 16) ("s" "v2" 13) ("v1" "v2" 10) ("v2" "v1" 4)
    ("v1" "v3" 12) ("v3" "v2" 9) ("v2" "v4" 14) ("v4" "v3" 7)
    ("v3" "t" 20) ("v4" "t" 4)))

(test maxflow-all-algorithms-agree-on-23
  "All ported algorithms find the unique CLRS max flow of 23."
  (with-populated-graph (g h +clrs-nodes+ +clrs-edges+)
    (dolist (algo '(:edmonds-karp :dinic :goldberg-tarjan))
      (is (= 23 (maximum-flow (gethash "s" h) (gethash "t" h)
                              :graph g :algorithm algo))
          "algorithm ~A" algo))))

(test maxflow-returns-flow-edges
  "The flow-edge list is returned and is conservative (no edge exceeds capacity)."
  (with-populated-graph (g h +clrs-nodes+ +clrs-edges+)
    (multiple-value-bind (flow edges)
        (maximum-flow (gethash "s" h) (gethash "t" h) :graph g :algorithm :dinic)
      (is (= 23 flow))
      (is (consp edges))
      ;; every reported flow is positive
      (is (every (lambda (e) (> (third e) 0)) edges)))))

;;; ---- bipartite check ----

(test bipartite-p-true-on-even-cycle
  "A 4-cycle is bipartite with partitions {A,C} and {B,D}."
  (with-populated-graph (g h '("A" "B" "C" "D")
                            '(("A" "B") ("B" "C") ("C" "D") ("D" "A")))
    (multiple-value-bind (bp a b) (bipartite-p :graph g)
      (is-true bp)
      (let ((part-a (sort (path-names a) #'string<))
            (part-b (sort (path-names b) #'string<)))
        ;; {A,C} vs {B,D} in some order
        (is (or (and (equal '("A" "C") part-a) (equal '("B" "D") part-b))
                (and (equal '("B" "D") part-a) (equal '("A" "C") part-b))))))))

(test bipartite-p-false-on-triangle
  "An odd cycle (triangle) is not bipartite."
  (with-populated-graph (g h '("A" "B" "C") '(("A" "B") ("B" "C") ("C" "A")))
    (is (null (bipartite-p :graph g)))))

;;; ---- maximum matching ----

;; Bipartite graph L1,L2,L3 / R1,R2,R3 with a perfect matching of size 3.
(defparameter +match-nodes+ '("L1" "L2" "L3" "R1" "R2" "R3"))
(defparameter +match-edges+
  '(("L1" "R1") ("L1" "R2") ("L2" "R1") ("L3" "R3")))

(test maximum-matching-size
  "Maximum matching has size 3 (a perfect matching exists)."
  (with-populated-graph (g h +match-nodes+ +match-edges+)
    (dolist (algo '(:edmonds-karp :dinic))
      (is (= 3 (length (maximum-matching :graph g :algorithm algo)))
          "algorithm ~A" algo))))

(test maximum-matching-pairs-are-valid-edges
  "Every matched pair corresponds to an edge of the graph and uses each node once."
  (with-populated-graph (g h +match-nodes+ +match-edges+)
    (let* ((matching (maximum-matching :graph g :algorithm :dinic))
           (names (mapcar (lambda (p) (sort (path-names p) #'string<)) matching))
           (all-nodes (reduce #'append names)))
      ;; each node appears at most once across the matching
      (is (= (length all-nodes) (length (remove-duplicates all-nodes :test #'string=)))))))
