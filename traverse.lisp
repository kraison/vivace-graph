(in-package :graph-db)

(defclass traversal ()
  ((end-vertex :accessor end-vertex :initarg :end-vertex :initform nil)
   (reverse-path :accessor reverse-path :initarg :path :initform nil)))

(defmethod traversal-path ((traversal traversal))
  (reverse (reverse-path traversal)))

(defmethod depth ((traversal traversal))
  (length (reverse-path traversal)))

(defun make-traversal (vertex path)
  (make-instance 'traversal
                 :end-vertex vertex
                 :path path))

(defmethod copy-traversal ((traversal traversal))
  (make-traversal (end-vertex traversal)
                  (copy-list (reverse-path traversal))))

;; END is normally a VERTEX, but LOOKUP-VERTEX-ANYWHERE can also hand back
;; an UNRESOLVED-NODE marker (detached store) or NIL (unknown store); the
;; specializer is widened to T so those still land in a traversal object
;; for the result table, rather than hitting NO-APPLICABLE-METHOD
;; (GH #169).
(defmethod update-traversal ((traversal traversal) end (edge edge))
  (let ((new-traversal
         (make-instance 'traversal
                        :end-vertex end
                        :path (copy-list (reverse-path traversal)))))
    (push edge (reverse-path new-traversal))
    new-traversal))

(defmethod traverse ((vertex vertex) &key (graph *graph*) (order :bfs)
                                       (direction :both) (uniqueness :global)
                                       edge-type max-depth return-paths)
  ;; FIXME: respect order and uniqueness
  ;;        currently bfs, global uniqueness.
  (declare (ignore order uniqueness))
  ;; Resolve edge endpoints (LOOKUP-VERTEX below) in the graph being traversed,
  ;; not the ambient *GRAPH*: (traverse v :graph G) scans G's adjacency but the
  ;; endpoint ids are G's, so they must be looked up in G.
  (let ((*graph* graph)
        (queue (make-queue :elements
                           (list
                            (make-instance 'traversal
                                           :end-vertex vertex))))
        (result-table (make-hash-table :test 'equalp))
        (memory (make-hash-table :test 'equalp)))
    (loop until (empty-queue-p queue) do
         (let* ((traversal (dequeue queue))
                (vertex (end-vertex traversal)))
           (unless (and max-depth
                        (> (depth traversal) max-depth))
             (when (or (eql direction :out) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((to-vertex
                                    (or (lookup-vertex (to edge))
                                        (lookup-vertex-anywhere (to edge))))
                                   (new-traversal
                                    (update-traversal traversal
                                                      to-vertex
                                                      edge)))
                              ;; Only a same-store vertex is walked further; a
                              ;; detached-store marker or a resolved
                              ;; cross-store vertex still lands in RESULTS
                              ;; below, but its adjacency lives in another
                              ;; store's MAP-EDGES -- cross-store
                              ;; continuation is future work (GH #169 ->
                              ;; #170+). Gate on NODE-GRAPH (stamped by every
                              ;; read, LOOKUP-VERTEX or LOOKUP-VERTEX-ANYWHERE
                              ;; alike), not the id's tag: an untagged-reopen
                              ;; graph's own vertices and a peer's tagged ids
                              ;; landing in OUR table both have tags that
                              ;; disagree with STORE-ID, and both must still
                              ;; be walked (GH #169, #209).
                              (when (and to-vertex (vertex-p to-vertex)
                                         (eq (node-graph to-vertex) graph))
                                (unless (gethash to-vertex memory)
                                  (setf (gethash to-vertex memory) t)
                                  (enqueue queue new-traversal)))
                              (when (typep edge edge-type)
                                (setf (gethash to-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex vertex
                          :direction :out))
             (when (or (eql direction :in) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((from-vertex
                                    (or (lookup-vertex (from edge))
                                        (lookup-vertex-anywhere (from edge))))
                                   (new-traversal
                                    (update-traversal traversal
                                                      from-vertex
                                                      edge)))
                              ;; See the :OUT branch above (GH #169, #209 ->
                              ;; #170+).
                              (when (and from-vertex (vertex-p from-vertex)
                                         (eq (node-graph from-vertex) graph))
                                (unless (gethash from-vertex memory)
                                  (setf (gethash from-vertex memory) t)
                                  (enqueue queue new-traversal)))
                              (when (typep edge edge-type)
                                (setf (gethash from-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex vertex
                          :direction :in)))))
    (if return-paths
        (loop for p being the hash-values in result-table collecting p)
        (loop for v being the hash-keys in result-table collecting v))))
