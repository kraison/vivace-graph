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
  (graph-db nil)
  (rules-db nil)
  (rete-net nil)
  (full-text-idx nil)
  (node-cache (make-hash-table :test 'equal :synchronized t :weakness :key-or-value))
  (triple-cache (make-hash-table :test 'equal :synchronized t :weakness :key-or-value))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue)))

(defmethod print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric shutdown-graph (graph &key waitp))

(defmethod needs-indexing? ((graph graph))
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defmethod do-indexing ((graph graph))
  graph)

(defun load-graph! (file)
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list file))
    (let ((uuid (uuid:make-uuid-from-string (py-configparser:get-option config "default" "uuid"))))
      (sb-ext:with-locked-hash-table (*graph-table*)
	(let ((graph (gethash uuid *graph-table*)))
	  (if (and (graph? graph) (not (shutdown? graph)))
	      (setq *graph* graph)
	      (let ((graph (make-graph :graph-uuid uuid
				       :graph-name (py-configparser:get-option 
						    config "default" "name")
				       :graph-location (py-configparser:get-option 
							config "default" "location"))))
		(setf (graph-db graph) (open-store (format nil "~A/triples" (graph-location graph)))
		      (rules-db graph) (open-hash (format nil "~A/rules" (graph-location graph)))
		      (gethash (graph-uuid graph) *graph-table*) graph
		      *graph* graph))))))))

(defun make-new-graph (&key name location)
  (let ((graph (make-graph))
	(config-file (format nil "~A/config.ini" location)))
    (with-open-file (stream config-file
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :error)
      (format stream "[default]~%")
      (format stream "uuid=~A~%" (print-object (graph-uuid graph) nil))
      (format stream "name=~A~%" name)
      (format stream "location=~A~%" location))
    (load-graph! config-file)))

(defmethod shutdown-graph ((graph graph) &key waitp)
  (declare (ignore waitp))
  (sb-ext:with-locked-hash-table (*graph-table*)
    (setf (shutdown? graph) t)
    (close-hash (rules-db graph))
    (close-store (graph-db graph))
    (remhash (graph-uuid graph) *graph-table*)))

