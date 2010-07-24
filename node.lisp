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
(defgeneric incf-ref-count (node))
(defgeneric decf-ref-count (node))
(defgeneric save-node (node))
(defgeneric make-anonymous-node-name (uuid))

(defmethod serialize ((node node))
  "FIXME: add ref count to serialized value"
  (let* ((serialized-id (serialize (node-uuid node)))
	 (serialized-value (serialize (node-value node)))
	 (total-length (+ (length serialized-id) (length serialized-value)))
	 (vec (make-array (+ 2 total-length) :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) +node+)
    (setf (aref vec 1) total-length)
    (dotimes (i (length serialized-id))
      (setf (aref vec (+ 2 i)) (aref serialized-id i)))
    (dotimes (i (length serialized-value))
      (setf (aref vec (+ 2 (length serialized-id) i)) (aref serialized-value i)))
    vec))

(defmethod deserialize ((become (eql +node+)) bytes)
  "FIXME: add ref count when serialize adds it"
  (let* ((length-id (aref bytes 1))
	 (offset-val (+ length-id 2)))
    (make-node :value (deserialize-raw (subseq bytes offset-val))
	       :uuid (deserialize-raw (subseq bytes 0 offset-val)))))

(defun make-node-key-from-value (value)
  (declare (optimize (speed 3)))
  (let ((serialized-value (serialize value)))
    (let ((key (make-array (+ 2 (length serialized-value)) :element-type '(unsigned-byte 8))))
      (setf (aref key 0) +node-key+)
      (setf (aref key 1) (length serialized-value))
      (dotimes (i (length serialized-value))
	(setf (aref key (1+ i)) (aref serialized-value i)))
      key)))

(defmethod make-serialized-key ((node node))
  (declare (optimize (speed 3)))
  (make-node-key-from-value (node-value node)))

(defmethod save-node ((node node))
  (let ((key (make-serialized-key node))
	(value (serialize node)))
    (store-object (graph-db (node-graph node)) key value :mode :keep)
    (setf (gethash (node-value node) (node-cache (node-graph node))) node)))

(defmethod lookup-node ((node node) (graph graph) &optional serialized?)
  (declare (ignore serialized?))
  node)

(defmethod lookup-node (value (graph graph) &optional serialized?)
  (or (and (not serialized?) (gethash value (node-cache graph)))
      (let ((raw (lookup-object (graph-db graph) (if serialized?
						     value
						     (make-node-key-from-value value)))))
	(when (vectorp raw)
	  (let ((node (deserialize-raw raw)))
	    (setf (node-graph node) graph)
	    node)))))
;;  (skip-list-lookup (nodes graph) (if serialized? (deserialize-raw value) value)))

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

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node (&key graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) 
	   (value (make-anonymous-node-name uuid)))
      (let ((node (make-node :uuid uuid 
			     :value value
			     :graph *graph*)))
	;;(skip-list-add (nodes *graph*) value node)
	(save-node node)
	node))))

(defun make-new-node (&key value graph)
  (let ((*graph* (or graph *graph*)))
    (let ((node (make-node :value value :graph *graph*)))
      (handler-case
	  ;;(skip-list-add (nodes *graph*) value node)
	  (save-node node)
	(dbm-error (condition)
	  (declare (ignore condition))
	  (lookup-node value *graph*))
	(skip-list-duplicate-error (condition)
	  (declare (ignore condition))
	  (lookup-node value *graph*))
	(:no-error (status)
	  (declare (ignore status))
	  node)))))

