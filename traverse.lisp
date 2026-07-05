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

(defmethod update-traversal ((traversal traversal) (vertex vertex) (edge edge))
  (let ((new-traversal
         (make-instance 'traversal
                        :end-vertex vertex
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
                            (let* ((to-vertex (lookup-vertex (to edge)))
                                   (new-traversal
                                    (update-traversal traversal
                                                      to-vertex
                                                      edge)))
                              (unless (gethash to-vertex memory)
                                (setf (gethash to-vertex memory) t)
                                (enqueue queue new-traversal))
                              (when (typep edge edge-type)
                                (setf (gethash to-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex vertex
                          :direction :out))
             (when (or (eql direction :in) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((from-vertex (lookup-vertex (from edge)))
                                   (new-traversal
                                    (update-traversal traversal
                                                      from-vertex
                                                      edge)))
                              (unless (gethash from-vertex memory)
                                (setf (gethash from-vertex memory) t)
                                (enqueue queue new-traversal))
                              (when (typep edge edge-type)
                                (setf (gethash from-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex vertex
                          :direction :in)))))
    (if return-paths
        (loop for p being the hash-values in result-table collecting p)
        (loop for v being the hash-keys in result-table collecting v))))
