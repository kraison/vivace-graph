;;;; Random graph generation tests (transactional VG builders).  Output is
;;;; random, so we assert deterministic structural properties.

(in-package #:graph-db/algorithms-test)

(def-suite generation-suite
  :description "Erdos-Renyi / Barabasi-Albert / Viger-Latapy transactional builders."
  :in graph-db-algorithms-suite)

(in-suite generation-suite)

(defun gen-node-count (g) (length (all-vertices g)))

(defun gen-edge-count (g)
  "Number of directed edges created (sum of out-degrees)."
  (reduce #'+ (mapcar (lambda (v) (out-degree v :graph g)) (all-vertices g))
          :initial-value 0))

(defun gen-vc () (lambda (i) (make-an :name (princ-to-string i))))
(defun gen-ec () (lambda (a b) (make-ae :from a :to b)))

(test erdos-renyi-complete
  "G(n,1.0) is the complete graph: n vertices, n(n-1)/2 edges, every vertex
adjacent to all others."
  (with-algo-graph (g)
    (generate-graph :erdos-renyi 5 :graph g :p 1.0
                    :vertex-constructor (gen-vc) :edge-constructor (gen-ec))
    (is (= 5 (gen-node-count g)))
    (is (= 10 (gen-edge-count g)))             ; 5*4/2
    (is (every (lambda (v) (= 4 (degree v :graph g))) (all-vertices g)))))

(test erdos-renyi-empty
  "G(n,0.0) has the n vertices but no edges."
  (with-algo-graph (g)
    (generate-graph :erdos-renyi 4 :graph g :p 0.0
                    :vertex-constructor (gen-vc) :edge-constructor (gen-ec))
    (is (= 4 (gen-node-count g)))
    (is (= 0 (gen-edge-count g)))))

(test barabasi-albert-structure
  "A Barabasi-Albert graph has the n vertices and at least the seed triangle's 3
edges.  (graph-utils' BA attaches probabilistically, so it can leave an isolated
node -- connectivity is not asserted.)"
  (with-algo-graph (g)
    (generate-graph :barabasi-albert 10 :graph g
                    :vertex-constructor (gen-vc) :edge-constructor (gen-ec))
    (is (= 10 (gen-node-count g)))
    (is (>= (gen-edge-count g) 3))
    (is (<= (gen-edge-count g) 45))))           ; <= n(n-1)/2
