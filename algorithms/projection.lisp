;;;; graph-db/algorithms -- in-memory projection (Mode A substrate).
;;;;
;;;; Inherently in-memory algorithms (all-pairs shortest paths, max-flow,
;;;; clustering, min-cut) want a compact, integer-indexed, freely-mutable graph.
;;;; Rather than reinvent that, we VENDOR the graph-utils representation -- its
;;;; sparse adjacency matrix and matrix-backed graph class -- into a private
;;;; :GRAPH-DB.PROJECTION package, so those algorithms port nearly verbatim in
;;;; later phases.  The vendored code is the standalone graph-utils library by
;;;; Kevin Raison, trimmed to the projection's needs and stripped of its
;;;; multithreading locks (a projection is single-threaded transient scratch
;;;; built fresh under a read snapshot and discarded).
;;;;
;;;; WITH-GRAPH-PROJECTION (in :GRAPH-DB, at the bottom) materializes such a
;;;; projection from a VivaceGraph under a snapshot, carrying a bidirectional map
;;;; between dense integer indices and VG vertices so results map back to UUIDs.
;;;;
;;;; This phase vendors only what cross-validates native Dijkstra (sparse array +
;;;; graph class + graph-utils' hop-count find-shortest-path).  Phase 4 extends
;;;; this package with the flow/all-pairs/clustering bodies.

;;; ==================================================================
;;; Vendored graph-utils substrate
;;; ==================================================================

(defpackage :graph-db.projection
  (:use :cl)
  (:nicknames :gproj)
  (:export #:graph #:directed-graph #:make-graph #:graph? #:directed? #:undirected?
           #:add-node #:lookup-node #:node-ids #:node-count #:map-nodes
           #:add-edge #:edge-weight #:edge-count #:map-edges #:list-edges
           #:neighbors #:outbound-neighbors #:inbound-neighbors
           #:find-shortest-path
           ;; dense / matrix family (methods defined in dense.lisp)
           #:all-pairs-shortest-paths #:reconstruct-path-all-pairs
           #:calculate-shortest-paths #:distance-map #:find-components
           #:score-edges #:cluster #:minimal-cut #:minimal-cut!
           #:copy-graph #:delete-edge
           ;; max flow / matching (methods defined in flow.lisp)
           #:capacity #:find-maximum-flow #:compute-maximum-flow
           #:bipartite? #:compute-maximum-matching
           ;; random graph generation (methods defined in generation.lisp)
           #:generate-random-graph #:s-point
           ;; sparse array
           #:make-sparse-array #:saref #:row-count #:col-count))

(in-package :graph-db.projection)

;;; ---- sparse 2D array (lock-free) ---------------------------------

(defclass sparse-array ()
  ((dimensions :accessor dimensions :initarg :dimensions :initform 1)
   (row-count :accessor row-count :initarg :row-count :initform 0)
   (col-count :accessor col-count :initarg :col-count :initform 0)
   (matrix :accessor matrix :initarg :matrix :initform (make-hash-table))
   (adjustable? :accessor adjustable? :initarg :adjustable? :initform nil)
   (initial-element :accessor initial-element :initarg :initial-element
                    :initform nil)))

(defun make-sparse-array (dimensions &key initial-element adjustable)
  (when (or (> (length dimensions) 2) (= 0 (length dimensions)))
    (error "We only support 1D and 2D arrays."))
  (make-instance 'sparse-array
                 :dimensions (length dimensions)
                 :row-count (first dimensions)
                 :col-count (or (second dimensions) 0)
                 :initial-element initial-element
                 :adjustable? adjustable))

(defmethod incf-sarray-dimensions ((array sparse-array) &optional (delta 1))
  (if (= 1 (dimensions array))
      (incf (row-count array) delta)
      (values (incf (row-count array) delta)
              (incf (col-count array) delta))))

(defmethod saref ((array sparse-array) &rest indices)
  (cond ((or (null indices) (/= (length indices) (dimensions array)))
         (error "You must supply proper indices for ~A" array))
        ((and (= 1 (length indices)) (= 1 (dimensions array)))
         (gethash (nth 0 indices) (matrix array) (initial-element array)))
        ((and (= 2 (length indices)) (= 2 (dimensions array)))
         (let ((table (gethash (nth 0 indices) (matrix array))))
           (if (hash-table-p table)
               (gethash (nth 1 indices) table (initial-element array))
               (initial-element array))))))

(defun set-sparse-array (array indices value)
  (cond ((or (null indices) (/= (length indices) (dimensions array)))
         (error "You must supply proper indices for ~A" array))
        ((and (= 1 (length indices)) (= 1 (dimensions array)))
         (when (>= (nth 0 indices) (row-count array))
           (if (adjustable? array)
               (setf (row-count array) (1+ (nth 0 indices)))
               (error "~A is not adjustable" array)))
         (if (eql value (initial-element array))
             (remhash (nth 0 indices) (matrix array))
             (setf (gethash (nth 0 indices) (matrix array)) value)))
        ((and (= 2 (length indices)) (= 2 (dimensions array)))
         (when (>= (nth 0 indices) (row-count array))
           (if (adjustable? array)
               (setf (row-count array) (1+ (nth 0 indices)))
               (error "~A is not adjustable" array)))
         (when (>= (nth 1 indices) (col-count array))
           (if (adjustable? array)
               (setf (col-count array) (1+ (nth 1 indices)))
               (error "~A is not adjustable" array)))
         (let ((table (gethash (nth 0 indices) (matrix array))))
           (unless (hash-table-p table)
             (setq table (setf (gethash (nth 0 indices) (matrix array))
                               (make-hash-table))))
           (if (eql value (initial-element array))
               (remhash (nth 1 indices) table)
               (setf (gethash (nth 1 indices) table) value))))))

(defun (setf saref) (value array &rest indices)
  (set-sparse-array array indices value))

(defun hash-keys (ht &optional (sort-fn #'<))
  (sort (loop for k being the hash-keys in ht collecting k) sort-fn))

(defmethod fast-map-sarray ((fn function) (array sparse-array))
  (if (= 1 (dimensions array))
      (map nil (lambda (key) (funcall fn key (gethash key (matrix array))))
           (hash-keys (matrix array)))
      (dolist (row (hash-keys (matrix array)))
        (let ((table (gethash row (matrix array))))
          (dolist (col (hash-keys table))
            (funcall fn row col (gethash col table)))))))

(defmethod map-sarray-row ((fn function) (array sparse-array) row)
  (let ((table (gethash row (matrix array))))
    (when (hash-table-p table)
      (loop for v being the hash-values in table using (hash-key k)
            collecting (funcall fn k v)))))

(defmethod map-sarray-col ((fn function) (array sparse-array) col)
  (let ((result nil))
    (maphash (lambda (row table)
               (multiple-value-bind (v p?)
                   (gethash col table (initial-element array))
                 (when p?
                   (push (funcall fn row v) result))))
             (matrix array))
    (nreverse result)))

;;; ---- matrix-backed graph -----------------------------------------

(defclass graph ()
  ((nodes :accessor nodes :initarg :nodes :initform (make-hash-table :test 'equal))
   (ids :accessor ids :initarg :ids :initform (make-hash-table))
   (last-id :accessor last-id :initarg :id :initform -1)
   (edges :accessor edges :initarg :edges :initform 0)
   (s-point :accessor s-point :initarg :s-point :initform 0)  ; barabasi saturation
   (degree-table :accessor degree-table :initform (make-hash-table))
   (matrix :accessor matrix :initarg :matrix
           :initform (make-sparse-array '(0 0) :adjustable t :initial-element 0))))

(defclass directed-graph (graph)
  ((in-degree-table :accessor in-degree-table :initform (make-hash-table))
   (out-degree-table :accessor out-degree-table :initform (make-hash-table))))

(defgeneric graph? (thing)
  (:method ((thing graph)) t)
  (:method (thing) nil))

(defgeneric directed? (thing)
  (:method ((thing directed-graph)) t)
  (:method (thing) nil))

(defmethod undirected? ((graph graph)) (null (directed? graph)))

(defun make-graph (&key directed? (node-comparator 'equal) (saturation-point 0))
  (make-instance (if directed? 'directed-graph 'graph)
                 :s-point saturation-point
                 :nodes (make-hash-table :test node-comparator)))

(defmethod node-count ((graph graph)) (hash-table-count (nodes graph)))
(defmethod edge-count ((graph graph)) (edges graph))

(defmethod lookup-node ((graph graph) value) (gethash value (nodes graph)))
(defmethod lookup-node ((graph graph) (id integer)) (gethash id (ids graph)))

(defmethod add-node ((graph graph) value &key &allow-other-keys)
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
        (incf-sarray-dimensions (matrix graph))
        (when (directed? graph)
          (setf (gethash id (in-degree-table graph)) 0
                (gethash id (out-degree-table graph)) 0))
        (setf (gethash id (degree-table graph)) 0
              (gethash value (nodes graph)) id
              (gethash id (ids graph)) value)
        id)))

(defmethod map-nodes ((fn function) (graph graph) &key collect? remove-nulls?)
  (let ((r nil))
    (maphash (lambda (node-name node-id)
               (if collect?
                   (push (funcall fn node-name node-id) r)
                   (funcall fn node-name node-id)))
             (nodes graph))
    (when collect?
      (nreverse (if remove-nulls? (remove-if #'null r) r)))))

(defmethod node-ids ((graph graph))
  (map-nodes (lambda (name id) (declare (ignore name)) id) graph :collect? t))

(defmethod edge-weight ((graph graph) (n1 integer) (n2 integer)) (saref (matrix graph) n1 n2))

(defgeneric add-edge (graph n1 n2 &key weight))
(defmethod add-edge ((graph graph) (n1 integer) (n2 integer) &key (weight 1))
  "Undirected edge: symmetric in the matrix."
  (unless (= n1 n2)
    (unless (> (saref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (degree-table graph)))
      (incf (gethash n2 (degree-table graph)))
      (incf (edges graph)))
    (setf (saref (matrix graph) n1 n2) weight)
    (setf (saref (matrix graph) n2 n1) weight))
  (list n1 n2))

(defmethod add-edge ((graph directed-graph) (n1 integer) (n2 integer) &key (weight 1))
  (unless (= n1 n2)
    (unless (> (saref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (out-degree-table graph)))
      (incf (gethash n2 (in-degree-table graph)))
      (incf (edges graph)))
    (setf (saref (matrix graph) n1 n2) weight))
  (list n1 n2))

(defgeneric neighbors (graph node))
(defmethod neighbors ((graph graph) (node integer))
  "Neighbors of NODE (for an undirected graph the matrix is symmetric)."
  (let ((neighbors nil))
    (map-sarray-col (lambda (row-id value)
                      (when (> value 0) (push row-id neighbors)))
                    (matrix graph) node)
    (when (directed? graph)
      (map-sarray-row (lambda (col-id value)
                        (when (> value 0) (push col-id neighbors)))
                      (matrix graph) node))
    (nreverse neighbors)))

(defgeneric outbound-neighbors (graph node))
(defmethod outbound-neighbors ((graph directed-graph) (node integer))
  (let ((neighbors nil))
    (map-sarray-row (lambda (col-id value)
                      (when (> value 0) (push col-id neighbors)))
                    (matrix graph) node)
    (nreverse neighbors)))

(defgeneric inbound-neighbors (graph node))
(defmethod inbound-neighbors ((graph directed-graph) (node integer))
  (let ((neighbors nil))
    (map-sarray-col (lambda (row-id value)
                      (when (> value 0) (push row-id neighbors)))
                    (matrix graph) node)
    (nreverse neighbors)))

(defmethod list-edges ((graph graph))
  (let ((r nil))
    (fast-map-sarray (lambda (n1 n2 w) (declare (ignore w)) (push (list n1 n2) r))
                     (matrix graph))
    (nreverse r)))

;;; ---- graph-utils' hop-count Dijkstra (for cross-validation) ------
;;; NOTE: like the original, this counts hops (1+ per edge), ignoring edge
;;; weights -- it computes a minimum-EDGE-COUNT path.  Native unweighted
;;; shortest-path must agree with its cost (number of edges).

(defun reconstruct-path (prev end)
  (when (cdr (assoc end prev))
    (cons (list (cdr (assoc end prev)) end)
          (reconstruct-path prev (cdr (assoc end prev))))))

(defmethod find-shortest-path ((graph graph) (n1 integer) (n2 integer))
  "Minimum-hop path between N1 and N2 as a list of (from to) integer edges, or
NIL if unreachable."
  (let ((nodes (node-ids graph)))
    (let ((distances (make-instance 'fib-heap:fib-heap))
          (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (map nil (lambda (node) (fib-heap:insert distances most-positive-fixnum node))
           nodes)
      (fib-heap:decrease-key distances n1 0)
      (loop until (null nodes) do
        (multiple-value-bind (next d) (fib-heap:extract-min distances)
          (when (= d most-positive-fixnum) (return nil))
          (when (= next n2)
            (return-from find-shortest-path
              (nreverse (reconstruct-path previous n2))))
          (setq nodes (delete next nodes))
          (dolist (neighbor (if (directed? graph)
                                (outbound-neighbors graph next)
                                (neighbors graph next)))
            (when (fib-heap:lookup-node distances neighbor)
              (let ((distance (1+ d)))
                (when (< distance (fib-heap:lookup-node distances neighbor))
                  (fib-heap:decrease-key distances neighbor distance)
                  (setf (cdr (assoc neighbor previous)) next))))))))))

;;; ==================================================================
;;; VivaceGraph-side projection builder
;;; ==================================================================

(in-package :graph-db)

(defstruct (projection (:constructor %make-projection))
  "A materialized in-memory copy of (a slice of) a VivaceGraph for Mode-A
algorithms.  PGRAPH is the vendored projection graph; KEY->IDX maps a VG node
key (UUID vector, EQUALP) to its dense integer index; IDX->VERTEX maps that index
back to the VG vertex."
  pgraph key->idx idx->vertex)

(defun projection-index (projection node-designator &optional (graph *graph*))
  "The dense integer index of NODE-DESIGNATOR within PROJECTION, or NIL."
  (let ((v (algorithm-vertex node-designator graph)))
    (when v (gethash (id v) (projection-key->idx projection)))))

(defun projection-vertex (projection index)
  "The VG vertex at dense integer INDEX within PROJECTION."
  (gethash index (projection-idx->vertex projection)))

(defun build-projection (&key (graph *graph*) directed vertex-type edge-type
                              (weight-fn #'weight) unweighted)
  "Materialize GRAPH (under a read snapshot) into an in-memory PROJECTION.  With
DIRECTED, build a directed projection (otherwise undirected).  VERTEX-TYPE /
EDGE-TYPE restrict the slice; each may be a single type or a list of types.  Edge
weights come from WEIGHT-FN, or 1 each with UNWEIGHTED."
  (with-algorithm-snapshot (graph)
    (let ((pg (gproj:make-graph :directed? directed))
          (key->idx (make-hash-table :test 'equalp))
          (idx->vertex (make-hash-table)))
      (apply #'map-vertices
             (lambda (v)
               (let ((idx (gproj:add-node pg (string-id (id v)))))
                 (setf (gethash (id v) key->idx) idx
                       (gethash idx idx->vertex) v)))
             graph :collect-p nil
             (%type-args vertex-type :vertex-type :include-vertex-types))
      (apply #'map-edges
             (lambda (e)
               (let ((fi (gethash (from e) key->idx))
                     (ti (gethash (to e) key->idx)))
                 (when (and fi ti)
                   (gproj:add-edge pg fi ti
                                   :weight (if unweighted 1 (funcall weight-fn e))))))
             graph :collect-p nil
             (%type-args edge-type :edge-type :include-edge-types))
      (%make-projection :pgraph pg :key->idx key->idx :idx->vertex idx->vertex))))

(defmacro with-graph-projection ((proj-var &key (graph '*graph*) directed
                                            vertex-type edge-type weight-fn
                                            unweighted)
                                 &body body)
  "Bind PROJ-VAR to a freshly materialized PROJECTION of GRAPH for the dynamic
extent of BODY.  See BUILD-PROJECTION."
  `(let ((,proj-var (build-projection :graph ,graph :directed ,directed
                                      :vertex-type ,vertex-type
                                      :edge-type ,edge-type
                                      ,@(when weight-fn `(:weight-fn ,weight-fn))
                                      :unweighted ,unweighted)))
     ,@body))

(defun projection-shortest-path (from to &key (graph *graph*) directed
                                          vertex-type edge-type)
  "Mode-A minimum-hop path FROM -> TO via the vendored graph-utils Dijkstra on an
in-memory projection.  Returns (values VERTEX-LIST HOP-COUNT), or (values NIL
NIL) if unreachable.  Used to cross-validate the native (Mode B) shortest-path."
  (with-graph-projection (proj :graph graph :directed directed
                               :vertex-type vertex-type :edge-type edge-type
                               :unweighted t)
    (let ((fi (projection-index proj from graph))
          (ti (projection-index proj to graph)))
      (unless (and fi ti) (error "projection-shortest-path: unknown node"))
      (if (= fi ti)
          (values (list (projection-vertex proj fi)) 0)
          (let ((edges (gproj:find-shortest-path (projection-pgraph proj) fi ti)))
            (if edges
                ;; edges = ((i j)(j k)...); rebuild the ordered vertex list
                (values (cons (projection-vertex proj (first (first edges)))
                              (mapcar (lambda (e)
                                        (projection-vertex proj (second e)))
                                      edges))
                        (length edges))
                (values nil nil)))))))
