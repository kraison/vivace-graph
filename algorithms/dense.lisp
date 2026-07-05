;;;; graph-db/algorithms -- dense / matrix algorithms (Mode A, projection).
;;;;
;;;; The inherently in-memory, shortest-path-derived family: Floyd-Warshall all-
;;;; pairs shortest paths, edge-betweenness / edge-span clustering, and minimal
;;;; cut.  These materialize an in-memory PROJECTION (see projection.lisp) under a
;;;; read snapshot and run vendored graph-utils code on it; the persistent graph
;;;; is never touched (clustering/min-cut mutate only the transient projection).
;;;;
;;;; This file (a) extends the vendored :GRAPH-DB.PROJECTION graph with the few
;;;; more methods these algorithms need (copy-graph, map-edges, delete-edge,
;;;; distance-map, find-components, all-pairs, clustering, cut), and (b) adds the
;;;; VivaceGraph-side entry points that build a projection and map results back
;;;; to VG vertices.  The flow family (max-flow, bipartite matching) is Phase 4b.

;;; ==================================================================
;;; More vendored graph-utils substrate
;;; ==================================================================

(in-package :graph-db.projection)

;; (These names are exported from the GRAPH-DB.PROJECTION defpackage in
;; projection.lisp so the single-colon references below read cleanly.)

(defmethod map-edges ((fn function) (graph graph) &key collect?)
  "Call FN on each matrix cell (N1 N2 WEIGHT).  For an undirected graph the
matrix is symmetric, so each edge is visited in both orientations (faithful to
graph-utils)."
  (let ((r nil))
    (fast-map-sarray (lambda (n1 n2 w)
                       (let ((v (funcall fn n1 n2 w)))
                         (when collect? (push v r))))
                     (matrix graph))
    (nreverse r)))

(defgeneric delete-edge (graph n1 n2))
(defmethod delete-edge ((graph graph) (n1 integer) (n2 integer))
  (unless (= n1 n2)
    (when (> (saref (matrix graph) n1 n2) 0)
      (decf (gethash n1 (degree-table graph)))
      (decf (gethash n2 (degree-table graph)))
      (decf (edges graph))
      (setf (saref (matrix graph) n1 n2) 0))
    (setf (saref (matrix graph) n2 n1) 0)))

(defmethod delete-edge ((graph directed-graph) (n1 integer) (n2 integer))
  (unless (= n1 n2)
    (when (> (saref (matrix graph) n1 n2) 0)
      (decf (gethash n1 (out-degree-table graph)))
      (decf (gethash n2 (in-degree-table graph)))
      (decf (edges graph))
      (setf (saref (matrix graph) n1 n2) 0))))

(defmethod copy-graph ((graph graph))
  "Deep copy of a projection graph (used by algorithms that mutate a working
copy, e.g. min-cut)."
  (let ((new (make-instance (if (directed? graph) 'directed-graph 'graph)
                            :nodes (make-hash-table
                                    :test (hash-table-test (nodes graph)))
                            :id (last-id graph)
                            :edges (edges graph)
                            :matrix (make-sparse-array
                                     (list (row-count (matrix graph))
                                           (col-count (matrix graph)))
                                     :adjustable t :initial-element 0))))
    (maphash (lambda (k v) (setf (gethash k (nodes new)) v)) (nodes graph))
    (maphash (lambda (k v) (setf (gethash k (ids new)) v)) (ids graph))
    (maphash (lambda (k v) (setf (gethash k (degree-table new)) v))
             (degree-table graph))
    (when (directed? graph)
      (maphash (lambda (k v) (setf (gethash k (in-degree-table new)) v))
               (in-degree-table graph))
      (maphash (lambda (k v) (setf (gethash k (out-degree-table new)) v))
               (out-degree-table graph)))
    (fast-map-sarray (lambda (i j w) (setf (saref (matrix new) i j) w))
                     (matrix graph))
    new))

;;; ---- BFS distance map + components (hop counts) ------------------

(defmethod distance-map ((graph graph) (id integer))
  "Sorted (node . hop-distance) reachable from ID."
  (let ((map (list (cons id 0))) (queue nil))
    (dolist (neighbor (if (directed? graph)
                          (outbound-neighbors graph id)
                          (neighbors graph id)))
      (let ((pair (cons neighbor 1)))
        (push pair map)
        (push pair queue)))
    (loop until (null queue) do
      (let ((pair (pop queue)))
        (dolist (neighbor (neighbors graph (car pair)))
          (unless (or (= neighbor id) (member neighbor map :key 'car))
            (let ((pair (cons neighbor (1+ (cdr pair)))))
              (push pair map)
              (setq queue (nconc queue (list pair))))))))
    (sort map #'< :key 'cdr)))

(defmethod find-components ((graph graph))
  "All components as a list of integer-id lists, largest first."
  (let ((nodes (node-ids graph)) (components nil))
    (loop until (null nodes) do
      (let ((dmap (distance-map graph (pop nodes))) (component nil))
        (dolist (pair dmap)
          (setq nodes (remove (car pair) nodes))
          (push (car pair) component))
        (when component (push component components))))
    (sort components #'> :key #'length)))

;;; ---- Floyd-Warshall all-pairs ------------------------------------

(defmethod all-pairs-shortest-paths ((graph graph) &key (use-weights-p t)
                                                      reconstruct-paths-p)
  "Floyd-Warshall.  Returns (values DISTANCES PATHS): DISTANCES maps (cons i j)
-> shortest distance; PATHS (when RECONSTRUCT-PATHS-P) maps (cons i j) -> next
hop for path reconstruction."
  (let ((distances (make-hash-table :test 'equalp))
        (paths nil) (infinity most-positive-fixnum))
    (when reconstruct-paths-p
      (setq paths (make-hash-table :test 'equalp)))
    (map-nodes (lambda (name i) (declare (ignore name))
                 (setf (gethash (cons i i) distances) 0))
               graph)
    (map-edges (lambda (n1 n2 w)
                 (let ((key (cons n1 n2)))
                   (when reconstruct-paths-p (setf (gethash key paths) n2))
                   (setf (gethash key distances) (if use-weights-p w 1))))
               graph)
    (map-nodes
     (lambda (name-k k) (declare (ignore name-k))
       (map-nodes
        (lambda (name-i i) (declare (ignore name-i))
          (map-nodes
           (lambda (name-j j) (declare (ignore name-j))
             (let ((new-distance (+ (gethash (cons i k) distances infinity)
                                    (gethash (cons k j) distances infinity))))
               (when (> (gethash (cons i j) distances infinity) new-distance)
                 (when reconstruct-paths-p
                   (setf (gethash (cons i j) paths) (gethash (cons i k) paths)))
                 (setf (gethash (cons i j) distances) new-distance))))
           graph))
        graph))
     graph)
    (values distances paths)))

(defmethod reconstruct-path-all-pairs ((graph graph) paths (n1 integer)
                                       (n2 integer))
  "Rebuild the path N1->N2 from the PATHS next-hop table as a list of (from to)
edges, or NIL if unreachable."
  (let ((key (cons n1 n2)))
    (if (null (gethash key paths))
        nil
        (let ((path nil) (prev n1) (next (gethash key paths)))
          (loop until (eql n2 next) do
            (push (list prev next) path)
            (setq prev next)
            (setq next (gethash (cons next n2) paths)))
          (push (list prev n2) path)
          (nreverse path)))))

(defmethod calculate-shortest-paths ((graph graph))
  "All shortest paths as a list of (i j PATH).  (Fixes the graph-utils original,
which referenced undefined PATH-MATRIX/NODE-IDX and the wrong reconstruct arity.)"
  (multiple-value-bind (distances path-table)
      (all-pairs-shortest-paths graph :reconstruct-paths-p t)
    (declare (ignore distances))
    (let ((paths nil))
      (dotimes (i (row-count (matrix graph)))
        (loop for j from (if (directed? graph) 0 i)
                to (1- (col-count (matrix graph)))
              do (unless (= i j)
                   (push (list i j (reconstruct-path-all-pairs graph path-table i j))
                         paths))))
      (nreverse paths))))

;;; ---- Clustering (edge betweenness / edge span) -------------------

(defmethod cluster ((graph graph) (method (eql :edge-betweenness))
                    &key (edge-removal-count 0))
  "Edge-betweenness clustering: count how many shortest paths cross each edge;
remove the EDGE-REMOVAL-COUNT highest.  Returns the removed edges as (i j score)."
  (let* ((shortest-paths (calculate-shortest-paths graph))
         (between-table
           (sort (map-edges
                  (lambda (i j w) (declare (ignore w))
                    (let ((coord (list i j)))
                      (list coord
                            (reduce '+ (mapcar (lambda (p)
                                                 (count coord (third p) :test 'equalp))
                                               shortest-paths)))))
                  graph :collect? t)
                 #'> :key 'second))
         (removed-edges nil))
    (dotimes (i edge-removal-count)
      (let ((edge (pop between-table)))
        (push edge removed-edges)
        (delete-edge graph (first (first edge)) (second (first edge)))))
    (nreverse (mapcar (lambda (edge)
                        (list (first (first edge)) (second (first edge)) (second edge)))
                      removed-edges))))

(defmethod score-edges ((graph graph) &key sort?)
  "Score each edge by its span (endpoint distance after the edge is removed)."
  (let ((span-map nil))
    (map-edges (lambda (n1 n2 w)
                 (delete-edge graph n1 n2)
                 (push (list (list n1 n2) (length (find-shortest-path graph n1 n2)))
                       span-map)
                 (add-edge graph n1 n2 :weight w))
               graph)
    (if sort? (sort span-map #'> :key 'second) span-map)))

(defmethod cluster ((graph graph) (method (eql :edge-span))
                    &key (edge-removal-count 0))
  "Edge-span clustering: remove the EDGE-REMOVAL-COUNT highest-span edges.
Returns the removed edges as (i j score)."
  (let ((span-map (score-edges graph :sort? t)) (removed-edges nil))
    (dotimes (i edge-removal-count)
      (let ((edge (pop span-map)))
        (push edge removed-edges)
        (delete-edge graph (first (first edge)) (second (first edge)))))
    (nreverse (mapcar (lambda (edge)
                        (list (first (first edge)) (second (first edge)) (second edge)))
                      removed-edges))))

;;; ---- Minimal cut -------------------------------------------------

(defmethod minimal-cut! ((graph graph))
  "Destructively remove high-span edges until GRAPH splits; return the removed
edges as (from to) pairs."
  (let ((removed-edges nil))
    (labels ((cut (g)
               (cond ((or (< (node-count g) 2)
                          (= 0 (edge-count g))
                          (>= (length (find-components g)) 2))
                      g)
                     (t (push (first (cluster g :edge-span :edge-removal-count 1))
                              removed-edges)
                        (cut g)))))
      (cut graph))
    (mapcar (lambda (edge) (subseq edge 0 2)) removed-edges)))

(defmethod minimal-cut ((graph graph) &key (method :cluster))
  "Non-destructive minimal cut on a copy of GRAPH.  Returns (values CUT-EDGES
COPY)."
  (let ((g (copy-graph graph)))
    (when (eq method :cluster)
      (values (minimal-cut! g) g))))

;;; ==================================================================
;;; VivaceGraph-side entry points
;;; ==================================================================

(in-package :graph-db)

(defstruct (all-pairs-result (:constructor %make-apsp))
  "Result of ALL-PAIRS-SHORTEST-PATHS: holds the projection and the Floyd-Warshall
distance/next-hop tables.  Query it with APSP-DISTANCE / APSP-PATH."
  projection distances paths)

(defun all-pairs-shortest-paths (&key (graph *graph*) directed (use-weights-p t)
                                      edge-type vertex-type)
  "Floyd-Warshall all-pairs shortest paths over an in-memory projection of GRAPH
\(Mode A).  Returns an ALL-PAIRS-RESULT; query distances/paths with APSP-DISTANCE
and APSP-PATH.  WARNING: O(V^2) memory and O(V^3) time -- scope with VERTEX-TYPE
on large graphs.  EDGE-TYPE / VERTEX-TYPE may each be a single type or a list of
types.  With USE-WEIGHTS-P (default) distances are edge-weight sums, else hop
counts."
  (let ((proj (build-projection :graph graph :directed directed
                                :edge-type edge-type :vertex-type vertex-type
                                :unweighted (not use-weights-p))))
    (multiple-value-bind (distances paths)
        (graph-db.projection:all-pairs-shortest-paths
         (projection-pgraph proj)
         :use-weights-p use-weights-p :reconstruct-paths-p t)
      (%make-apsp :projection proj :distances distances :paths paths))))

(defun apsp-distance (apsp from to)
  "Shortest distance FROM -> TO in an ALL-PAIRS-RESULT, or NIL if unreachable."
  (let* ((proj (all-pairs-result-projection apsp))
         (i (projection-index proj from))
         (j (projection-index proj to)))
    (when (and i j)
      (let ((d (gethash (cons i j) (all-pairs-result-distances apsp)
                        most-positive-fixnum)))
        (if (= d most-positive-fixnum) nil d)))))

(defun apsp-path (apsp from to)
  "Shortest path FROM -> TO in an ALL-PAIRS-RESULT as an ordered vertex list, or
NIL if unreachable."
  (let* ((proj (all-pairs-result-projection apsp))
         (i (projection-index proj from))
         (j (projection-index proj to)))
    (when (and i j)
      (if (= i j)
          (list (projection-vertex proj i))
          (let ((edges (graph-db.projection:reconstruct-path-all-pairs
                        (projection-pgraph proj)
                        (all-pairs-result-paths apsp) i j)))
            (when edges
              (cons (projection-vertex proj (first (first edges)))
                    (mapcar (lambda (e) (projection-vertex proj (second e))) edges))))))))

(defun graph-clustering (&key (graph *graph*) directed (method :edge-betweenness)
                              (edge-removal-count 1) edge-type vertex-type)
  "Cluster GRAPH by removing high-scoring edges from an in-memory projection
\(Mode A).  METHOD is :EDGE-BETWEENNESS (default) or :EDGE-SPAN.  EDGE-TYPE /
VERTEX-TYPE may each be a single type or a list of types.  Returns the removed
edges as (FROM-VERTEX TO-VERTEX SCORE) triples (highest first)."
  (let* ((proj (build-projection :graph graph :directed directed
                                 :edge-type edge-type :vertex-type vertex-type
                                 :unweighted t))
         (removed (graph-db.projection:cluster (projection-pgraph proj) method
                                               :edge-removal-count edge-removal-count)))
    (mapcar (lambda (e)
              (list (projection-vertex proj (first e))
                    (projection-vertex proj (second e))
                    (third e)))
            removed)))

(defun minimum-cut (&key (graph *graph*) directed edge-type vertex-type)
  "A minimal edge cut of GRAPH (via edge-span clustering on an in-memory
projection, Mode A).  EDGE-TYPE / VERTEX-TYPE may each be a single type or a list
of types.  Returns the cut edges as (FROM-VERTEX TO-VERTEX) pairs."
  (let* ((proj (build-projection :graph graph :directed directed
                                 :edge-type edge-type :vertex-type vertex-type
                                 :unweighted t))
         (cut (graph-db.projection:minimal-cut (projection-pgraph proj))))
    (mapcar (lambda (e)
              (list (projection-vertex proj (first e))
                    (projection-vertex proj (second e))))
            cut)))
