;;;; graph-db/algorithms -- structural algorithms (Mode B, native).
;;;;
;;;; Degree and degree distributions, BFS distance maps, weakly-connected
;;;; components, a DFS spanning tree, and the (BFS-eccentricity) graph center.
;;;; All run directly against the live ve/vev indexes under a read snapshot,
;;;; keyed by node UUID, and never mutate the graph.
;;;;
;;;; These are the matrix-free structural primitives: each is built on BFS/DFS
;;;; over ADJACENT-VERTICES, so none materializes an O(V^2) structure (unlike the
;;;; Floyd-Warshall-based center in graph-utils, which is a Mode-A concern).

(in-package :graph-db)

;;; ------------------------------------------------------------------
;;; Degree
;;; ------------------------------------------------------------------

(defun %out-degree (vertex graph edge-type)
  (length (apply #'outgoing-edges vertex :graph graph
                 (%type-args edge-type :edge-type :include-edge-types))))

(defun %in-degree (vertex graph edge-type)
  (length (apply #'incoming-edges vertex :graph graph
                 (%type-args edge-type :edge-type :include-edge-types))))

(defun out-degree (vertex &key (graph *graph*) edge-type)
  "Number of edges directed out of VERTEX (optionally restricted to EDGE-TYPE, a
single edge type or a list of edge types)."
  (with-algorithm-snapshot (graph)
    (%out-degree (algorithm-vertex vertex graph) graph edge-type)))

(defun in-degree (vertex &key (graph *graph*) edge-type)
  "Number of edges directed into VERTEX (optionally restricted to EDGE-TYPE, a
single edge type or a list of edge types)."
  (with-algorithm-snapshot (graph)
    (%in-degree (algorithm-vertex vertex graph) graph edge-type)))

(defun degree (vertex &key (graph *graph*) edge-type)
  "Total number of edges incident to VERTEX (in-degree + out-degree).  Edges are
directed in VivaceGraph, so this is the natural undirected-degree analog.
EDGE-TYPE may be a single edge type or a list of edge types."
  (with-algorithm-snapshot (graph)
    (let ((v (algorithm-vertex vertex graph)))
      (+ (%out-degree v graph edge-type) (%in-degree v graph edge-type)))))

(defun degree-distribution (&key (graph *graph*) (which :total) edge-type
                                 vertex-type)
  "Degree distribution of GRAPH as a sorted alist of (DEGREE . COUNT).  WHICH
selects :TOTAL (default; in+out), :OUT, or :IN.  VERTEX-TYPE / EDGE-TYPE narrow
the population / edges considered; EDGE-TYPE may be a single type or a list."
  (with-algorithm-snapshot (graph)
    (let ((dist (make-hash-table)))
      (apply #'map-vertices
       (lambda (v)
         (let ((d (ecase which
                    (:total (+ (%out-degree v graph edge-type)
                               (%in-degree v graph edge-type)))
                    (:out (%out-degree v graph edge-type))
                    (:in (%in-degree v graph edge-type)))))
           (incf (gethash d dist 0))))
       graph (%type-args vertex-type :vertex-type :include-vertex-types))
      (sort (loop for d being the hash-keys in dist using (hash-value c)
                  collect (cons d c))
            #'< :key #'car))))

;;; ------------------------------------------------------------------
;;; BFS distances
;;; ------------------------------------------------------------------

(defun %bfs-distances (source-v &key (graph *graph*) (direction :out) edge-type)
  "Breadth-first hop distances from SOURCE-V over GRAPH.  Returns (values DIST
VTABLE), both keyed by node-key: DIST maps node-key -> hop count, VTABLE maps
node-key -> vertex.  Assumes an active snapshot."
  (let ((dist (make-hash-table :test 'equalp))
        (vtable (make-hash-table :test 'equalp))
        (q (make-empty-queue))
        (sk (id source-v)))
    (setf (gethash sk dist) 0
          (gethash sk vtable) source-v)
    (enqueue q source-v)
    (loop for u = (dequeue q) while u do
      (let ((du (gethash (id u) dist)))
        (dolist (cell (adjacent-vertices u :graph graph :direction direction
                                           :edge-type edge-type :unweighted t))
          (let* ((v (car cell)) (vk (id v)))
            (unless (gethash vk dist)
              (setf (gethash vk dist) (1+ du)
                    (gethash vk vtable) v)
              (enqueue q v))))))
    (values dist vtable)))

(defun distance-map (source &key (graph *graph*) (direction :out) edge-type)
  "Breadth-first hop distances from SOURCE to every reachable node in GRAPH, as a
list of (VERTEX . DISTANCE) sorted by ascending distance (SOURCE first, at 0).
DIRECTION is :OUT (default), :IN, or :BOTH (undirected).  EDGE-TYPE may be a
single type or a list of types."
  (with-algorithm-snapshot (graph)
    (let ((sv (algorithm-vertex source graph)))
      (unless sv (error "distance-map: unknown node ~S" source))
      (multiple-value-bind (dist vtable)
          (%bfs-distances sv :graph graph :direction direction :edge-type edge-type)
        (let ((cells nil))
          (maphash (lambda (k d) (push (cons (gethash k vtable) d) cells)) dist)
          (sort cells #'< :key #'cdr))))))

;;; ------------------------------------------------------------------
;;; Connected components (weakly connected by default)
;;; ------------------------------------------------------------------

(defun connected-components (&key (graph *graph*) (direction :both) edge-type
                                  vertex-type)
  "All connected components of GRAPH as a list of vertex lists, largest first.
With the default DIRECTION :BOTH these are weakly-connected components (edge
direction ignored); :OUT/:IN reach only along that direction.  EDGE-TYPE /
VERTEX-TYPE may each be a single type or a list of types.  (Strongly-connected
components -- Tarjan -- are a future addition.)"
  (with-algorithm-snapshot (graph)
    (let ((seen (make-hash-table :test 'equalp))
          (components nil))
      (dolist (v (all-vertices graph vertex-type))
        (unless (gethash (id v) seen)
          (multiple-value-bind (dist vtable)
              (%bfs-distances v :graph graph :direction direction
                                :edge-type edge-type)
            (declare (ignore dist))
            (let ((comp nil))
              (maphash (lambda (k vert)
                         (setf (gethash k seen) t)
                         (push vert comp))
                       vtable)
              (push comp components)))))
      (sort components #'> :key #'length))))

;;; ------------------------------------------------------------------
;;; Spanning tree (DFS)
;;; ------------------------------------------------------------------

(defun spanning-tree (&key (graph *graph*) root (direction :both) edge-type)
  "A DFS spanning tree of the component containing ROOT (a random vertex if ROOT
is omitted).  Returns (values TREE-EDGES ROOT-VERTEX) where TREE-EDGES is a list
of (PARENT-VERTEX . CHILD-VERTEX) cons cells.  Read-only: the tree is returned as
data, not built as a new graph.  DIRECTION defaults to :BOTH (undirected).
EDGE-TYPE may be a single type or a list of types."
  (with-algorithm-snapshot (graph)
    (let ((root-v (if root
                      (algorithm-vertex root graph)
                      (first (all-vertices graph)))))
      (if (null root-v)
          (values nil nil)
          (let ((seen (make-hash-table :test 'equalp))
                (edges nil)
                (stack (list root-v)))
            (setf (gethash (id root-v) seen) t)
            (loop for u = (pop stack) while u do
              (dolist (cell (adjacent-vertices u :graph graph :direction direction
                                                 :edge-type edge-type :unweighted t))
                (let* ((v (car cell)) (vk (id v)))
                  (unless (gethash vk seen)
                    (setf (gethash vk seen) t)
                    (push (cons u v) edges)
                    (push v stack)))))
            (values (nreverse edges) root-v))))))

;;; ------------------------------------------------------------------
;;; Graph center (BFS eccentricity)
;;; ------------------------------------------------------------------

(defun %eccentricity (v graph direction edge-type)
  "Greatest hop distance from V to any node reachable from it (within the
reachable set).  Assumes an active snapshot."
  (multiple-value-bind (dist vtable)
      (%bfs-distances v :graph graph :direction direction :edge-type edge-type)
    (declare (ignore vtable))
    (let ((mx 0))
      (maphash (lambda (k d) (declare (ignore k)) (when (> d mx) (setf mx d))) dist)
      mx)))

(defun eccentricity (vertex &key (graph *graph*) (direction :both) edge-type)
  "The eccentricity of VERTEX: the greatest hop distance to any node reachable
from it.  DIRECTION defaults to :BOTH (undirected).  EDGE-TYPE may be a single
type or a list of types."
  (with-algorithm-snapshot (graph)
    (%eccentricity (algorithm-vertex vertex graph) graph direction edge-type)))

(defun graph-center (&key (graph *graph*) (direction :both) edge-type vertex-type)
  "The center of GRAPH: the vertices of minimum eccentricity.  Returns (values
CENTER-VERTICES MIN-ECCENTRICITY).  Cost is O(V*(V+E)) -- a BFS per vertex -- the
honest matrix-free form; for large graphs prefer a scoped VERTEX-TYPE.  In a
disconnected graph, eccentricity is measured within each vertex's reachable set
\(so an isolated vertex has eccentricity 0); compute on a connected graph for the
classical center.  DIRECTION defaults to :BOTH (undirected).  EDGE-TYPE /
VERTEX-TYPE may each be a single type or a list of types."
  (with-algorithm-snapshot (graph)
    (let ((centers nil)
          (best most-positive-fixnum))
      (dolist (v (all-vertices graph vertex-type))
        (let ((e (%eccentricity v graph direction edge-type)))
          (cond ((< e best) (setf best e centers (list v)))
                ((= e best) (push v centers)))))
      (values (nreverse centers) (if centers best 0)))))
