(in-package #:vivace-graph)

(setq *graph-table* (make-uuid-table :synchronized t))

(defstruct (graph
	     (:predicate graph?)
	     (:conc-name nil)
	     (:print-function print-graph))
  (graph-uuid (make-uuid))
  (graph-name nil)
  (shutdown? nil)
  (graph-location #P".")
  (nodes (make-instance 'skip-list := 'uuid:uuid-eql))
  (triples (make-instance 'skip-list := 'uuid:uuid-eql))
  (deleted-triples (make-instance 'skip-list := 'uuid:uuid-eql))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue))
  (node-idx (make-instance 'skip-list))
  (subject-idx (make-instance 'skip-list))
  (predicate-idx (make-instance 'skip-list))
  (object-idx (make-instance 'skip-list)))

(defmethod print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric startup-graph (graph))
(defgeneric shutdown-graph (graph &key waitp))

(defmethod needs-indexing? ((graph graph))
  (null (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defun load-graph! (location)
  )

(defun make-new-graph (&key name location)
  (let ((graph (make-graph)))
    (setf (graph-name graph) (or name (graph-uuid graph))
	  (graph-location graph) (or (and location (probe-file location)) (graph-location graph))
	  (gethash (graph-uuid graph) *graph-table*) graph)
    graph))
