;;;; graph-db/algorithms -- maximum flow & bipartite matching (Mode A, projection).
;;;;
;;;; The flow family, vendored from graph-utils onto the in-memory projection.
;;;; Edge weights are treated as capacities.  Algorithms run on a copy of the
;;;; projection (the residual graph), so the projection -- and the persistent VG
;;;; graph it came from -- are never disturbed.
;;;;
;;;; graph-utils calls this code "beta"; the cross-validation test (every
;;;; algorithm must agree on the unique max-flow value) is the guard.
;;;;
;;;; This file extends the :GRAPH-DB.PROJECTION substrate with capacities, a FIFO
;;;; queue, three max-flow algorithms (Edmonds-Karp, Dinic, Goldberg-Tarjan
;;;; push-relabel), and bipartite matching, then adds the VG-side entry points.
;;;;
;;;; OMITTED: graph-utils' Karzanov implementation -- it does not satisfy the
;;;; max-flow invariant (disagrees with the other three on the CLRS network), so
;;;; it is not ported rather than ship a knowingly-wrong result.  Node-capacity
;;;; expansion (graph-utils' :node-capacities? option) is also omitted; only edge
;;;; capacities are supported.

;;; ==================================================================
;;; Vendored graph-utils flow substrate
;;; ==================================================================

(in-package :graph-db.projection)

;;; ---- FIFO queue (graph-utils uses EMPTY-QUEUE?) ------------------

(defstruct (mf-queue (:constructor make-empty-queue))
  (elements nil) (tail nil))

(defun empty-queue? (q) (null (mf-queue-elements q)))

(defun enqueue (q item)
  (let ((cell (cons item nil)))
    (if (mf-queue-tail q)
        (setf (cdr (mf-queue-tail q)) cell (mf-queue-tail q) cell)
        (setf (mf-queue-elements q) cell (mf-queue-tail q) cell))
    q))

(defun dequeue (q)
  (prog1 (pop (mf-queue-elements q))
    (when (null (mf-queue-elements q)) (setf (mf-queue-tail q) nil))))

;;; ---- capacities / weighted-edge mutation -------------------------

(defmethod capacity ((graph graph) n1 n2)
  (or (edge-weight graph n1 n2) 0))

(defmethod incf-edge-weight ((graph graph) (n1 integer) (n2 integer) &key (delta 1))
  (setf (saref (matrix graph) n1 n2) (+ (saref (matrix graph) n1 n2) delta)))

(defmethod decf-edge-weight ((graph graph) (n1 integer) (n2 integer) &key (delta 1))
  (setf (saref (matrix graph) n1 n2) (- (saref (matrix graph) n1 n2) delta)))

(defmethod minimum-capacity ((graph graph) edges)
  (let ((min most-positive-fixnum) (min-list nil))
    (dolist (edge edges)
      (when (< (apply #'edge-weight graph edge) min)
        (setq min (apply #'edge-weight graph edge))
        (push edge min-list)))
    (values min min-list)))

(defun position-of-edge (n1 n2 edge-list)
  "Find an edge in a list, always sorting vertices from low to high."
  (let ((p-edge (if (> n2 n1) (list n1 n2) (list n2 n1))))
    (values (position p-edge edge-list
                      :test (lambda (e1 e2)
                              (and (eql (first e1) (first e2))
                                   (eql (second e1) (second e2)))))
            p-edge)))

(defmethod push-flow-via-edges ((graph graph) path edges-in-flow)
  "Push the maximum amount of flow through the edges in PATH."
  (let ((min-cap (apply 'min (mapcar (lambda (e) (capacity graph (nth 0 e) (nth 1 e)))
                                     path))))
    (dolist (edge path)
      (destructuring-bind (n1 n2) edge
        (multiple-value-bind (p p-edge) (position-of-edge n1 n2 edges-in-flow)
          (if p
              (incf (nth 2 (nth p edges-in-flow)) min-cap)
              (push (append p-edge (list min-cap)) edges-in-flow))
          (decf-edge-weight graph n1 n2 :delta min-cap)
          (incf-edge-weight graph n2 n1 :delta min-cap))))
    (values min-cap edges-in-flow)))

;;; ---- Edmonds-Karp ------------------------------------------------

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer) (algorithm (eql :edmonds-karp)))
  "Ford-Fulkerson with the Edmonds-Karp (BFS shortest augmenting path) rule."
  (let ((flow 0) (gf (copy-graph graph)) (edges-in-flow nil))
    (loop
      (let ((path (find-shortest-path gf source sink)))
        (if path
            (multiple-value-bind (pushed-flow eif)
                (push-flow-via-edges gf path edges-in-flow)
              (incf flow pushed-flow)
              (setq edges-in-flow eif))
            (return-from find-maximum-flow
              (values flow (sort edges-in-flow #'> :key 'third) gf)))))))

;;; ---- Dinic -------------------------------------------------------

(defmethod compute-layered-network ((graph graph) source sink)
  "Build the layered (level) network of GRAPH via tweaked BFS."
  (let* ((nodes nil) (edges nil)
         (distances (make-hash-table))
         (queue (make-empty-queue)))
    (dolist (node (node-ids graph))
      (setf (gethash node distances) most-positive-fixnum))
    (setf (gethash source distances) 0)
    (enqueue queue source)
    (loop until (empty-queue? queue) do
      (let ((v (dequeue queue)))
        (dolist (w (outbound-neighbors graph v))
          (cond ((= most-positive-fixnum (gethash w distances))
                 (enqueue queue w)
                 (setf (gethash w distances) (1+ (gethash v distances)))
                 (pushnew w nodes)
                 (pushnew (list v w) edges :test 'equalp))
                ((= (gethash w distances) (1+ (gethash v distances)))
                 (pushnew (list v w) edges :test 'equalp))))))
    (let ((reversed-net (mapcar 'reverse edges)))
      (let ((touched-nodes nil) (queue (make-empty-queue)))
        (enqueue queue sink)
        (loop until (empty-queue? queue) do
          (let* ((node (dequeue queue))
                 (children (mapcar #'second
                                   (remove-if-not (lambda (edge) (eq node (first edge)))
                                                  reversed-net))))
            (pushnew node touched-nodes)
            (dolist (child children)
              (unless (member child touched-nodes) (enqueue queue child)))))
        (dolist (node (node-ids graph))
          (unless (member node touched-nodes)
            (setq nodes (remove node nodes))
            (setq edges (remove-if (lambda (edge) (member node edge)) edges))))))
    (values nodes edges)))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer) (algorithm (eql :dinic)))
  "Dinic's algorithm (blocking flow on the layered network)."
  (let ((flow 0) (gf (copy-graph graph)) (edges-in-flow nil))
    (loop until (null (find-shortest-path gf source sink)) do
      (multiple-value-bind (l0-nodes l0-edges)
          (compute-layered-network gf source sink)
        (declare (ignore l0-nodes))
        (labels ((find-path (node)
                   (let ((edge (find node l0-edges :key 'first)))
                     (cond ((null edge) nil)
                           ((eql (second edge) sink) (list edge))
                           (t (let ((path (find-path (second edge))))
                                (if path
                                    (append (list edge) path)
                                    (progn (setq l0-edges (remove edge l0-edges :test 'equalp))
                                           nil))))))))
          (loop until (null (member source l0-edges :key 'first)) do
            (let ((path (find-path source)))
              (multiple-value-bind (min-cap min-edges) (minimum-capacity gf path)
                (declare (ignore min-cap))
                (multiple-value-bind (pushed-flow eif)
                    (push-flow-via-edges gf path edges-in-flow)
                  (incf flow pushed-flow)
                  (setq edges-in-flow eif))
                (setq l0-edges (set-difference l0-edges min-edges :test 'equalp))))))))
    (values flow
            (remove-if (lambda (e) (= (third e) 0)) (sort edges-in-flow #'> :key 'third))
            gf)))

;;; ---- Goldberg-Tarjan (push-relabel) ------------------------------

(defmethod gt-push ((graph graph) f h e v w)
  (let ((f0 (min (aref e v) (- (capacity graph v w) (aref f v w)))))
    (incf (aref f v w) f0)
    (decf (aref f w v) f0)
    (incf (aref e w) f0)
    (decf (aref e v) f0)
    f0))

(defmethod gt-lift ((graph graph) f h e v)
  (let ((min most-positive-fixnum))
    (dolist (w (node-ids graph))
      (when (> (- (capacity graph v w) (aref f v w)) 0)
        (setq min (min min (aref h w)))
        (setf (aref h v) (1+ min))))))

(defmethod gt-discharge ((graph graph) f h e v)
  (loop while (> (aref e v) 0) do
    (let ((neighbors (node-ids graph)))
      (loop while neighbors do
        (let ((w (pop neighbors)))
          (when (and w (> (- (capacity graph v w) (aref f v w)) 0)
                     (> (aref h v) (aref h w)))
            (gt-push graph f h e v w)))))
    (when (> (aref e v) 0) (gt-lift graph f h e v))))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer) (algorithm (eql :goldberg-tarjan)))
  "Push-relabel (Goldberg-Tarjan).  Non-destructive: flow lives in separate
arrays, the graph matrix is only read."
  (let* ((gf graph) (edges-in-flow nil)
         (h (make-array (list (node-count gf)) :initial-element 0))
         (e (make-array (list (node-count gf)) :initial-element 0))
         (f (make-array (list (node-count gf) (node-count gf)) :initial-element 0))
         (q (remove-if (lambda (n) (or (eq n source) (eq n sink))) (node-ids gf))))
    (setf (aref h source) (node-count gf))
    (setf (aref e source) most-positive-fixnum)
    (dolist (n (outbound-neighbors gf source)) (gt-push gf f h e source n))
    (let ((p 0))
      (loop while (< p (length q)) do
        (let* ((v (nth p q)) (old-height (aref h v)))
          (gt-discharge gf f h e v)
          (if (> (aref h v) old-height)
              (progn (setq q (nconc (list v) (remove v q))) (setq p 0))
              (incf p)))))
    (let ((flow (loop for i from 0 to (1- (array-dimension f 1))
                      summing (aref f source i) into total
                      finally (return total))))
      (loop for i from 0 to (1- (array-dimension f 0)) do
        (loop for j from 0 to (1- (array-dimension f 1)) do
          (when (> (aref f i j) 0) (push (list i j (aref f i j)) edges-in-flow))))
      (values flow
              (mapcar (lambda (e) (if (> (first e) (second e))
                                      (list (second e) (first e) (third e)) e))
                      (sort edges-in-flow #'> :key 'third))
              gf))))

;;; ---- bipartite check & matching ----------------------------------

(defmethod bipartite? ((graph graph) &key show-partitions?)
  "Is GRAPH bipartite?  With SHOW-PARTITIONS?, return (values A B) on success."
  (let ((color-table (make-hash-table))
        (partition-table (make-hash-table))
        (queue (make-empty-queue))
        (components (find-components graph)))
    (flet ((color-of (node) (gethash node color-table))
           (partition-of (node) (gethash node partition-table)))
      (map-nodes (lambda (name n) (declare (ignore name))
                   (setf (gethash n partition-table) 0
                         (gethash n color-table) :white))
                 graph)
      (dolist (component components)
        (let ((start (nth (random (length component)) component)))
          (setf (gethash start partition-table) 1
                (gethash start color-table) :grey)
          (enqueue queue start)
          (loop until (empty-queue? queue) do
            (let ((node (dequeue queue)))
              (dolist (neighbor (neighbors graph node))
                (when (= (partition-of neighbor) (partition-of node))
                  (return-from bipartite? nil))
                (when (eql :white (color-of neighbor))
                  (setf (gethash neighbor color-table) :grey
                        (gethash neighbor partition-table)
                        (- 3 (partition-of node)))
                  (enqueue queue neighbor)))
              (setf (gethash node color-table) :black)))))
      (if show-partitions?
          (let ((a nil) (b nil))
            (maphash (lambda (node p) (if (eql p 1) (push node a) (push node b)))
                     partition-table)
            (values a b))
          t))))

(defmethod compute-maximum-matching ((graph graph) v1 v2 &key (algorithm :dinic))
  "Maximum matching of the bipartite GRAPH with partitions V1 and V2, via max-flow
on a unit-capacity flow network.  Returns matched (n1 n2) pairs in GRAPH's node
ids.  (Fixes graph-utils' version, which fed original ids to the flow net as if
they were the flow net's own ids.)"
  (let ((flow-net (make-graph :directed? t)))
    (map-nodes (lambda (name id) (declare (ignore name)) (add-node flow-net id)) graph)
    (flet ((fn-id (orig) (lookup-node flow-net orig)))   ; original id -> flow-net id
      (map-edges (lambda (n1 n2 w) (declare (ignore w))
                   (if (member n1 v2)
                       (add-edge flow-net (fn-id n2) (fn-id n1) :weight 1)
                       (add-edge flow-net (fn-id n1) (fn-id n2) :weight 1)))
                 graph)
      (let ((source (add-node flow-net :source))
            (sink (add-node flow-net :sink)))
        (dolist (node v1) (add-edge flow-net source (fn-id node) :weight 1))
        (dolist (node v2) (add-edge flow-net (fn-id node) sink :weight 1))
        (multiple-value-bind (flow edges)
            (compute-maximum-flow flow-net source sink :algorithm algorithm)
          (declare (ignore flow))
          (mapcar (lambda (edge)
                    (list (lookup-node flow-net (first edge))
                          (lookup-node flow-net (second edge))))
                  (remove-if (lambda (edge)
                               (or (/= 1 (nth 2 edge))
                                   (eql (first edge) source) (eql (first edge) sink)
                                   (eql (second edge) source) (eql (second edge) sink)))
                             edges)))))))

;;; ---- dispatch ----------------------------------------------------

(defmethod compute-maximum-flow ((graph directed-graph) (source integer)
                                 (sink integer) &key (algorithm :edmonds-karp))
  (find-maximum-flow graph source sink algorithm))

(defmethod compute-maximum-flow ((graph directed-graph) source sink
                                 &key (algorithm :edmonds-karp))
  "Maximum flow from SOURCE to SINK (node values) in a directed projection.
ALGORITHM is one of :edmonds-karp, :dinic, :goldberg-tarjan."
  (compute-maximum-flow graph (lookup-node graph source) (lookup-node graph sink)
                        :algorithm algorithm))

;;; ==================================================================
;;; VivaceGraph-side entry points
;;; ==================================================================

(in-package :graph-db)

(defun maximum-flow (source sink &key (graph *graph*) (algorithm :edmonds-karp)
                                   edge-type vertex-type (weight-fn #'weight))
  "Maximum flow from SOURCE to SINK in GRAPH, treating edge WEIGHT (via WEIGHT-FN)
as capacity.  Runs on an in-memory directed projection (Mode A); the persistent
graph is untouched.  ALGORITHM is :edmonds-karp (default), :dinic, or
:goldberg-tarjan.  EDGE-TYPE / VERTEX-TYPE may each be a single type or a list of
types.

Returns (values FLOW-VALUE FLOW-EDGES) where FLOW-EDGES is a list of
\(FROM-VERTEX TO-VERTEX FLOW)."
  (let* ((proj (build-projection :graph graph :directed t :edge-type edge-type
                                 :vertex-type vertex-type :weight-fn weight-fn))
         (si (projection-index proj source graph))
         (ti (projection-index proj sink graph)))
    (unless (and si ti) (error "maximum-flow: unknown source/sink node"))
    (multiple-value-bind (flow edges)
        (graph-db.projection:compute-maximum-flow (projection-pgraph proj) si ti
                                                  :algorithm algorithm)
      (values flow
              (mapcar (lambda (e)
                        (list (projection-vertex proj (first e))
                              (projection-vertex proj (second e))
                              (third e)))
                      edges)))))

(defun bipartite-p (&key (graph *graph*) edge-type vertex-type)
  "Is GRAPH bipartite (treating edges as undirected)?  Returns (values BIPARTITE-P
PARTITION-A PARTITION-B); the partitions are vertex lists, NIL when not bipartite.
EDGE-TYPE / VERTEX-TYPE may each be a single type or a list of types."
  (let ((proj (build-projection :graph graph :directed nil :edge-type edge-type
                                :vertex-type vertex-type :unweighted t)))
    (if (graph-db.projection:bipartite? (projection-pgraph proj))
        (multiple-value-bind (a b)
            (graph-db.projection:bipartite? (projection-pgraph proj)
                                            :show-partitions? t)
          (values t
                  (mapcar (lambda (id) (projection-vertex proj id)) a)
                  (mapcar (lambda (id) (projection-vertex proj id)) b)))
        (values nil nil nil))))

(defun maximum-matching (&key (graph *graph*) edge-type vertex-type
                              (algorithm :dinic))
  "Maximum matching of a bipartite GRAPH (edges treated as undirected), via
max-flow on an in-memory projection (Mode A).  EDGE-TYPE / VERTEX-TYPE may each
be a single type or a list of types.  Returns a list of (VERTEX1 VERTEX2) matched
pairs.  Signals an error if GRAPH is not bipartite."
  (let ((proj (build-projection :graph graph :directed nil :edge-type edge-type
                                :vertex-type vertex-type :unweighted t)))
    (multiple-value-bind (a b)
        (graph-db.projection:bipartite? (projection-pgraph proj) :show-partitions? t)
      (unless (graph-db.projection:bipartite? (projection-pgraph proj))
        (error "maximum-matching: graph is not bipartite"))
      (mapcar (lambda (pair)
                (list (projection-vertex proj (first pair))
                      (projection-vertex proj (second pair))))
              (graph-db.projection:compute-maximum-matching
               (projection-pgraph proj) a b :algorithm algorithm)))))
