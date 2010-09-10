(in-package #:vivace-graph)

(setq *graph-table* (make-hash-table :synchronized t :test 'equalp))

(defstruct (graph
	     (:predicate graph?)
	     (:conc-name nil)
	     (:print-function print-graph))
  (graph-uuid (make-uuid))
  (graph-name nil)
  (shutdown? nil)
  (graph-location #P".")
  (triple-db nil)
  (deleted-triple-db nil)
  (functor-db nil)
  (rule-db nil)
  (full-text-idx nil)
  (rule-idx (make-hash-table :synchronized t :test 'equal))
  (templates (make-hash-table :synchronized t))
  (functors (make-hash-table :synchronized t))
  (rule-cache (make-hash-table :synchronized t))
  (predicate-cache (make-hash-table :synchronized t))
  ;;(node-cache (make-hash-table :test 'equal :synchronized t))
  (triple-cache (make-hash-table :test 'equal :synchronized t))
  (deleted-triple-cache (make-hash-table :test 'equal :synchronized t))
  (production-pq (make-skip-pq :key-equal #'timestamp= :comparison #'timestamp>
			       :head-value (make-timestamp :day -1000000 :sec 0 :nsec 0)))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue)))

(defun print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric shutdown-graph (graph))

(defmethod needs-indexing? ((graph graph))
  "Are there pending indexing jobs to be done for GRAPH?"
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defun lookup-graph (name)
  (gethash name *graph-table*))