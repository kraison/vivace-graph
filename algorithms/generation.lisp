;;;; graph-db/algorithms -- random graph generation (transactional VG builders).
;;;;
;;;; The classic random-graph models (Erdos-Renyi, Barabasi-Albert preferential
;;;; attachment), vendored from graph-utils.  The model logic builds an in-memory
;;;; topology on a projection graph; GENERATE-GRAPH then materializes it into a
;;;; real VivaceGraph in a single transaction via caller-supplied vertex/edge
;;;; constructors -- so the generators are schema-agnostic and the result is a
;;;; persisted graph.
;;;;
;;;; OMITTED: graph-utils' Viger-Latapy generator.  Its edge-swap phase does not
;;;; preserve connectivity (it raises "Edge swaps disconnected the graph!" for
;;;; most parameters) and it can spin at low target degree, so it is not ported
;;;; rather than ship a generator that hangs or usually fails.

;;; ==================================================================
;;; Vendored graph-utils generators (on the projection graph)
;;; ==================================================================

(in-package :graph-db.projection)

(defgeneric generate-random-graph (model size &key &allow-other-keys))

(defmethod generate-random-graph ((model (eql :erdos-renyi)) (size integer)
                                  &key p (name-fn #'princ-to-string)
                                  &allow-other-keys)
  "G(n,p): each undirected pair is present independently with probability P."
  (let ((graph (make-graph)))
    (dotimes (i size) (add-node graph (funcall name-fn i)))
    (dotimes (i size)
      (loop for j from (1+ i) to (1- size) do
        (when (<= (random 1.0) p) (add-edge graph i j))))
    graph))

(defmethod generate-random-graph ((model (eql :barabasi-albert)) (size integer)
                                  &key (saturation-point 0)
                                  (name-fn #'princ-to-string) &allow-other-keys)
  "Preferential attachment: new nodes attach to existing nodes with probability
proportional to their degree."
  (when (< size 4)
    (error "Cannot generate a barabasi-albert graph of size less than 4"))
  (let ((graph (make-graph :saturation-point saturation-point))
        (degree-table (make-array size :element-type 'integer :initial-element 0)))
    (dotimes (i 3) (add-node graph (funcall name-fn i)))
    (dotimes (i 3)
      (loop for j from (1+ i) to 2 do
        (incf (aref degree-table i)) (incf (aref degree-table j))
        (add-edge graph i j)))
    (loop for i from 3 to (1- size) do (add-node graph (funcall name-fn i)))
    (loop for i from 3 to (1- size) do
      (loop for j from 0 to (1- i) do
        (when (/= i j)
          (when (not (and (> (s-point graph) 0)
                          (>= (aref degree-table j) (s-point graph))))
            (when (<= (random 1.0)
                      (/ (1+ (aref degree-table j))
                         (+ (edge-count graph) (node-count graph))))
              (incf (aref degree-table i)) (incf (aref degree-table j))
              (add-edge graph i j))))))
    graph))

;;; ==================================================================
;;; VivaceGraph-side transactional builder
;;; ==================================================================

(in-package :graph-db)

(defun generate-graph (model size &key (graph *graph*) vertex-constructor
                                    edge-constructor (p 0.5) (saturation-point 0))
  "Generate a random graph of MODEL with SIZE nodes and persist it into GRAPH in
one transaction.  MODEL is :erdos-renyi or :barabasi-albert.

The model decides the topology; the caller controls the schema:
  VERTEX-CONSTRUCTOR -- a function of the integer node index (0..SIZE-1) that
    creates and returns a VG vertex (e.g. (lambda (i) (make-person :name ...))).
  EDGE-CONSTRUCTOR   -- a function of (FROM-VERTEX TO-VERTEX) that creates an
    edge between two generated vertices.
Both are called inside the builder's WITH-TRANSACTION.

Model parameters: :P (Erdos-Renyi edge probability), :SATURATION-POINT
\(Barabasi-Albert degree cap; 0 = none).

Returns the list of created vertices in index order."
  (assert vertex-constructor () "generate-graph requires :vertex-constructor")
  (assert edge-constructor () "generate-graph requires :edge-constructor")
  (let* ((pg (graph-db.projection:generate-random-graph
              model size :p p :saturation-point saturation-point))
         (n (graph-db.projection:node-count pg))
         (idx->vertex (make-array n :initial-element nil))
         (*graph* graph))
    (with-transaction ()
      (dotimes (i n)
        (setf (aref idx->vertex i) (funcall vertex-constructor i)))
      (let ((seen (make-hash-table :test 'equal)))
        (dolist (edge (graph-db.projection:list-edges pg))
          (let* ((i (first edge)) (j (second edge))
                 (key (cons (min i j) (max i j))))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (funcall edge-constructor (aref idx->vertex i)
                       (aref idx->vertex j)))))))
    (coerce idx->vertex 'list)))
