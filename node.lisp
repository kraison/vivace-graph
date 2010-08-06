(in-package #:vivace-graph)

(defstruct (node
	     (:conc-name node-)
	     (:predicate node?))
  (uuid (make-uuid))
  (value nil)
  (key nil)
  (ref-count 0 :type (UNSIGNED-BYTE 64))
  (graph *graph*))

(defgeneric lookup-node (value graph &optional serialized?))
(defgeneric make-anonymous-node-name (uuid))
(defgeneric incf-ref-count (node))
(defgeneric decf-ref-count (node))
(defgeneric save-node (node))
(defgeneric make-anonymous-node-name (uuid))

(defgeneric node-eql (n1 n2)
  (:method ((n1 node) (n2 node)) (uuid:uuid-eql (node-uuid n1) (node-uuid n2)))
  (:method (n1 n2) nil))

(defgeneric node-equal (n1 n2)
  (:method ((n1 node) (n2 node)) 
    (and (uuid:uuid-eql (node-uuid n1) (node-uuid n2))
	 (equal (node-value n1) (node-value n2))))
  (:method (n1 n2) nil))

(defmethod serialize ((node node))
  "FIXME: add ref count to serialized value?"
  (serialize-multiple +node+ (node-uuid node) (node-value node) (node-ref-count node)))

(defmethod deserialize-help ((become (eql +node+)) bytes)
  "FIXME: add ref count when serialize adds it"
  (destructuring-bind (id value ref-count) (extract-all-subseqs bytes)
    (make-node :value (deserialize value) 
	       :uuid (deserialize id) 
	       :ref-count (deserialize ref-count))))

(defun make-node-key-from-value (value)
  (serialize-multiple +node-key+ value))

(defmethod make-serialized-key ((node node))
  (declare (optimize (speed 3)))
  (or (node-key node)
      (let ((key (make-node-key-from-value (node-value node))))
	(cas (node-key node) nil key)
	key)))

(defmethod save-node ((node node))
  (let ((key (make-serialized-key node))
	(value (serialize node)))
    (store-object (triple-db (node-graph node)) key value :mode :keep)))

(defmethod lookup-node ((node node) (graph graph) &optional serialized?)
  (declare (ignore serialized?))
  node)

(defmethod lookup-node (value (graph graph) &optional serialized?)
  (or (and (not serialized?) (gethash value (node-cache graph)))
      (let ((serialized-value (or (and serialized? value) (make-node-key-from-value value))))
	(let ((raw (lookup-object (triple-db graph) serialized-value)))
	  (when (vectorp raw)
	    (let ((node (deserialize raw)))
	      (setf (node-graph node) graph
		    (node-key node) serialized-value)
	      node))))))

(defmethod incf-ref-count ((node node))
  node)

(defmethod decf-ref-count ((node node))
  node)

(defun list-nodes (&optional graph)
  "FIXME: update for tokyo cabinet-based store."
  (declare (ignore graph))
;  (let ((*graph* (or graph *graph*)) (result nil))
;    (map-skip-list-values #'(lambda (node) (push node result)) (nodes *graph*))
;    (reverse result)))
  )

(defmethod cache-node ((node node))
  (setf (gethash (node-value node) (node-cache (node-graph node))) node))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node (&key graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) 
	   (value (make-anonymous-node-name uuid)))
      (let ((node (make-node :uuid uuid 
			     :value value
			     :graph *graph*)))
	(save-node node)
	node))))

(defun make-new-node (&key value graph cache?)
  (if (node? value)
      value
      (let ((*graph* (or graph *graph*)))
	(let ((node (make-node :value value :graph *graph*)))
	  (handler-case
	      (save-node node)
	    (persistence-error (condition)
	      (declare (ignore condition))
	      (lookup-node value *graph*))
	    (:no-error (status)
	      (declare (ignore status))
	      (when cache? (cache-node node))
	      node))))))
