(in-package #:vivace-graph)

(defstruct (node
	     (:conc-name node-)
	     (:predicate node?))
  (uuid (make-uuid))
  (type +unknown+)
  (value nil)
  (ref-count 0 :type (UNSIGNED-BYTE 64))
  (graph *graph*))

(defgeneric lookup-node (value graph &optional serialized?))
(defgeneric make-anonymous-node-name (uuid))

(defun lookup-node-by-id (uuid)
  (skip-list-lookup (nodes *graph*) uuid))

(defmethod lookup-node (value (graph graph) &optional serialized?)
  ;;(format t "lookup-node: *graph* is ~A for ~A~%" graph value)
  (lookup-node-by-id
   (skip-list-lookup (node-idx graph) (if serialized? (deserialize-raw value) value))))

(defun list-nodes (&optional graph)
  (let ((*graph* (or graph *graph*)) (result nil))
    (let ((result nil))
      (map-skip-list-values #'(lambda (node) (push node result)) (nodes *graph*)))
    (nreverse result)))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node (&key graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) 
	   (value (make-anonymous-node-name uuid))
	   (serialized-value (serialize value)))
      (let ((node (make-node 
		   :uuid uuid 
		   :value value
		   :graph *graph*)))
	(with-mcas (:equality 'equal)
	  (skip-list-add (nodes *graph*) uuid node)
	  (skip-list-add (node-idx *graph*) value uuid))
	node))))

(defun make-new-node (&key value graph)
  (let ((*graph* (or graph *graph*)))
    (or (lookup-node value *graph*)
	(let* ((serialized-value (serialize value))
	       (node (make-node 
		      :value value
		      :graph *graph*)))
	  (with-mcas (:equality 'equal)
	    (skip-list-add (nodes *graph*) (node-uuid node) node)
	    (skip-list-add (node-idx *graph*) value (node-uuid node)))
	  node))))

