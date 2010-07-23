(in-package #:vivace-graph)

(defstruct (node
	     (:conc-name node-)
	     (:predicate node?))
  (uuid (make-uuid))
  (value nil)
  (ref-count 0 :type (UNSIGNED-BYTE 64))
  (graph *graph*))

(defgeneric lookup-node (value graph &optional serialized?))
(defgeneric make-anonymous-node-name (uuid))

(defmethod lookup-node ((node node) (graph graph) &optional serialized?)
  (declare (ignore serialized?))
  node)

(defmethod lookup-node (value (graph graph) &optional serialized?)
  (skip-list-lookup (nodes graph) (if serialized? (deserialize-raw value) value)))

(defun list-nodes (&optional graph)
  (let ((*graph* (or graph *graph*)) (result nil))
    (map-skip-list-values #'(lambda (node) (push node result)) (nodes *graph*))
    (reverse result)))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node (&key graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) 
	   (value (make-anonymous-node-name uuid)))
      (let ((node (make-node :uuid uuid 
			     :value value
			     :graph *graph*)))
	(skip-list-add (nodes *graph*) value node)
	(save-node node)
	node))))

(defun make-new-node (&key value graph)
  (let ((*graph* (or graph *graph*)))
    (let ((node (make-node :value value :graph *graph*)))
      (handler-case
	  (skip-list-add (nodes *graph*) value node)
	(skip-list-duplicate-error (condition)
	  (declare (ignore condition))
	  (lookup-node value *graph*))
	(:no-error (status)
	  (declare (ignore status))
	  (save-node node)
	  node)))))

