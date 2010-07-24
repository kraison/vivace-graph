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
  (node-cache (make-hash-table :test 'equal :synchronized t))
  (triple-cache (make-hash-table :test 'equal :synchronized t))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue)))

(defmethod print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric shutdown-graph (graph &key waitp))

(defmethod needs-indexing? ((graph graph))
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defun load-graph! (file)
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list file))
    (let ((graph (make-graph :graph-uuid (uuid:make-uuid-from-string 
					  (py-configparser:get-option config "default" "uuid"))
			     :graph-name (py-configparser:get-option config "default" "name")
			     :graph-location (py-configparser:get-option 
					      config "default" "location"))))
      (setf (graph-db graph) (open-store (format nil "~A/db" (graph-location graph))))
      (setq *graph* graph))))

(defun make-new-graph (&key name location)
  (let ((graph (make-graph)))
    (setf (graph-name graph) (or name (graph-uuid graph))
	  (graph-location graph) (or (and location (probe-file location)) (graph-location graph))
	  (graph-db graph) (open-store (format nil "~A/db" (graph-location graph)))
	  (gethash (graph-uuid graph) *graph-table*) graph)
    (with-open-file (stream (format nil "~A/config.ini" (graph-location graph)) 
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :error)
      (format stream "[default]~%")
      (format stream "uuid=~A~%" (print-object (graph-uuid graph) nil))
      (format stream "name=~A~%" (graph-name graph))
      (format stream "location=~A~%" (pathname-name (graph-location graph))))
    graph))

(defmethod shutdown-graph ((graph graph) &key waitp)
  (declare (ignore waitp))
  (setf (shutdown? graph) t)
  (close-store (graph-db graph)))
