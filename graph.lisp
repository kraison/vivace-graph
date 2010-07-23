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
  (tx-log-mailbox (sb-concurrency:make-mailbox))
  (nodes (make-skip-list))
  (triples (make-hash-table :test 'equal :synchronized t))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue))
  (subject-idx (make-skip-list :duplicates-allowed? t))
  (predicate-idx (make-skip-list :duplicates-allowed? t))
  (object-idx (make-skip-list :duplicates-allowed? t)))

(defmethod print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric startup-graph (graph))
(defgeneric shutdown-graph (graph &key waitp))

(defmethod needs-indexing? ((graph graph))
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defun load-graph! (location)
  )

(defun make-new-graph (&key name location)
  (let ((graph (make-graph)))
    (setf (graph-name graph) (or name (graph-uuid graph))
	  (graph-location graph) (or (and location (probe-file location)) (graph-location graph))
	  (gethash (graph-uuid graph) *graph-table*) graph)
    graph))
