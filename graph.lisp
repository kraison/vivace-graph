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
  (triple-db nil)
  (functor-db nil)
  (rule-db nil)
  (rete-net nil)
  (full-text-idx nil)
  (functors (make-hash-table :synchronized t))
  (prolog-compiler-macros (make-hash-table :synchronized t))
  (rule-cache (make-hash-table :synchronized t))
  (predicate-cache (make-hash-table :synchronized t))
  (node-cache (make-hash-table :test 'equal :synchronized t :weakness :key-or-value))
  (triple-cache (make-hash-table :test 'equal :synchronized t :weakness :key-or-value))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue)))

(defun print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric shutdown-graph (graph &key waitp))

(defmethod needs-indexing? ((graph graph))
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

