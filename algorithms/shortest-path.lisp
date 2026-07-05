;;;; graph-db/algorithms -- shortest paths.
;;;;
;;;; Mode B (native): lazy Dijkstra and A* run directly against the live ve/vev
;;;; adjacency indexes under a read snapshot, keyed by node UUID, using the
;;;; Fibonacci-heap priority queue (algorithms/fib-heap.lisp).  "Lazy" means we
;;;; only push a node onto the frontier when it is first discovered, rather than
;;;; seeding the heap with every vertex up front (as graph-utils did) -- so a
;;;; single-pair query that terminates early never materializes the whole node
;;;; set.  This is the scalable, persistent-native path that will later back the
;;;; Prolog `shortest_path/4' predicate.
;;;;
;;;; Floyd-Warshall all-pairs (inherently O(V^2) memory) lives in the Mode-A
;;;; projection family (Phase 4), not here.
;;;;
;;;; Weights: taken from each edge's WEIGHT slot by default (WEIGHT-FN), or 1 per
;;;; edge with :UNWEIGHTED.  Dijkstra/A* require non-negative edge weights.

(in-package :graph-db)

(defun %dijkstra (source-v &key (graph *graph*) target-key target-v
                             (direction :out) edge-type
                             (weight-fn #'weight) unweighted heuristic)
  "Core single-source shortest-path search from SOURCE-V.

Settles nodes in nondecreasing distance order.  If TARGET-KEY is supplied the
search stops as soon as that node is settled; otherwise it settles every node
reachable from SOURCE-V.  With HEURISTIC (a function of (vertex target-vertex)
returning an admissible cost estimate) the frontier is ordered by f = g + h
\(A*); TARGET-V must then be supplied.  HEURISTIC requires a target.

Returns (values DIST PREV VTABLE REACHED-TARGET-P), all keyed by node-key:
DIST maps node-key -> shortest distance, PREV maps node-key -> predecessor
vertex, VTABLE maps node-key -> vertex object."
  (when (and heuristic (null target-v))
    (error "A* (a heuristic) requires a target node."))
  (let ((dist (make-hash-table :test 'equalp))
        (prev (make-hash-table :test 'equalp))
        (vtable (make-hash-table :test 'equalp))
        (settled (make-hash-table :test 'equalp))
        (heap (make-instance 'fib-heap:fib-heap))
        (source-key (id source-v)))
    (setf (gethash source-key dist) 0
          (gethash source-key vtable) source-v)
    (fib-heap:insert heap
                     (if heuristic (funcall heuristic source-v target-v) 0)
                     source-key)
    (loop until (fib-heap:empty-p heap) do
      (let* ((u-key (fib-heap:extract-min heap))
             (u (gethash u-key vtable))
             (g (gethash u-key dist)))
        (setf (gethash u-key settled) t)
        (when (and target-key (equalp u-key target-key))
          (return-from %dijkstra (values dist prev vtable t)))
        (dolist (cell (adjacent-vertices u :graph graph :direction direction
                                           :edge-type edge-type
                                           :weight-fn weight-fn
                                           :unweighted unweighted))
          (let* ((v (car cell))
                 (w (cdr cell))
                 (v-key (id v)))
            (unless (gethash v-key settled)
              (let ((nd (+ g w))
                    (old (gethash v-key dist)))
                (when (or (null old) (< nd old))
                  (setf (gethash v-key dist) nd
                        (gethash v-key prev) u
                        (gethash v-key vtable) v)
                  (let ((priority (if heuristic
                                      (+ nd (funcall heuristic v target-v))
                                      nd)))
                    (if (fib-heap:lookup-node heap v-key)
                        (fib-heap:decrease-key heap v-key priority)
                        (fib-heap:insert heap priority v-key))))))))))
    (values dist prev vtable (and target-key
                                  (gethash target-key settled)
                                  t))))

(defun %reconstruct-path (prev target-v)
  "Walk PREV (node-key -> predecessor vertex) backward from TARGET-V to the
source, returning the ordered vertex list source..target."
  (let ((path nil)
        (cur target-v))
    (loop
      (push cur path)
      (let ((p (gethash (id cur) prev)))
        (if p (setf cur p) (return))))
    path))

(defun shortest-path (from to &key (graph *graph*) (direction :out) edge-type
                                (weight-fn #'weight) unweighted heuristic)
  "Shortest path FROM -> TO in GRAPH using Dijkstra's algorithm, or A* when
HEURISTIC is supplied.

FROM and TO are node designators (vertex, id vector, or id string).  DIRECTION is
:OUT (default), :IN, or :BOTH (undirected).  EDGE-TYPE restricts the search to a
single edge type OR a list of edge types (their union); NIL follows all types.
Distances come from each edge's WEIGHT (WEIGHT-FN), or 1 per edge with
:UNWEIGHTED.  HEURISTIC, if given, is a function of (vertex target-vertex)
returning an admissible (never-overestimating) cost estimate.

Returns (values PATH COST) where PATH is the ordered vertex list from FROM to TO
inclusive and COST its total weight; or (values NIL NIL) when TO is unreachable.
Requires non-negative edge weights."
  (with-algorithm-snapshot (graph)
    (let ((from-v (algorithm-vertex from graph))
          (to-v (algorithm-vertex to graph)))
      (unless from-v (error "shortest-path: unknown source node ~S" from))
      (unless to-v (error "shortest-path: unknown target node ~S" to))
      (if (equalp (id from-v) (id to-v))
          (values (list from-v) 0)
          (multiple-value-bind (dist prev vtable reached)
              (%dijkstra from-v :graph graph
                                :target-key (id to-v) :target-v to-v
                                :direction direction :edge-type edge-type
                                :weight-fn weight-fn :unweighted unweighted
                                :heuristic heuristic)
            (declare (ignore vtable))
            (if reached
                (values (%reconstruct-path prev to-v)
                        (gethash (id to-v) dist))
                (values nil nil)))))))

(defun a-star (from to heuristic &key (graph *graph*) (direction :out) edge-type
                                   (weight-fn #'weight) unweighted)
  "A* shortest path FROM -> TO guided by HEURISTIC, a function of
\(vertex target-vertex) returning an admissible cost estimate.  A thin wrapper
over SHORTEST-PATH; see it for arguments and return values."
  (shortest-path from to :graph graph :direction direction :edge-type edge-type
                         :weight-fn weight-fn :unweighted unweighted
                         :heuristic heuristic))

(defun single-source-shortest-paths (from &key (graph *graph*) (direction :out)
                                            edge-type (weight-fn #'weight)
                                            unweighted)
  "Shortest distances from FROM to every reachable node in GRAPH (Dijkstra, no
early termination).  DIRECTION is :OUT/:IN/:BOTH and EDGE-TYPE may be a single
edge type or a list of edge types.  Returns a list of (VERTEX . DISTANCE) cells
sorted by ascending distance (FROM itself first, at distance 0)."
  (with-algorithm-snapshot (graph)
    (let ((from-v (algorithm-vertex from graph)))
      (unless from-v (error "single-source-shortest-paths: unknown node ~S" from))
      (multiple-value-bind (dist prev vtable)
          (%dijkstra from-v :graph graph :direction direction
                            :edge-type edge-type :weight-fn weight-fn
                            :unweighted unweighted)
        (declare (ignore prev))
        (let ((cells nil))
          (maphash (lambda (k d)
                     (push (cons (gethash k vtable) d) cells))
                   dist)
          (sort cells #'< :key #'cdr))))))
