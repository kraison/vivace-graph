(in-package #:vivace-graph)

#|
(defclass node ()
  ((uuid :accessor uuid :initarg :uuid :initform (make-uuid))
   (node-type :accessor node-type :initarg :type :initform +string+)
   (value :accessor value :initarg :value :initform "")
   (graph :accessor graph :initarg :graph :initform :default)))

(defmethod print-object ((node node) stream)
  (format stream "#<~A>" (value node)))

(defgeneric node? (thing)
  (:method ((thing node)) t)
  (:method (thing) nil))
|#

(defstruct (node
	     (:conc-name node-)
	     (:print-function print-node)
	     (:predicate node?))
  (uuid (make-uuid) :type uuid:uuid)
  (type +unknown+ :type integer)
  (value "")
  (ref-count 0 :type integer)
  (graph *graph*))

(defmethod print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<~A>" (deserialize (aref (node-value node) 0) 
				      (subseq (node-value node) 2))))

(defgeneric lookup-node (value graph &optional serialized?))
(defgeneric make-anonymous-node-name (uuid))

(defmethod lookup-node (value (graph graph) &optional serialized?)
  ;;(format t "lookup-node: *graph* is ~A for ~A~%" graph value)
  (gethash (if serialized? value (serialize value)) (node-idx graph)))

(defun list-nodes (&optional graph)
  (let ((*graph* (or graph *graph*)) (result nil))
    (sb-ext:with-locked-hash-table ((node-idx *graph*))
      (maphash #'(lambda (k v)
		   (declare (ignore k))
		   (push v result))
	       (node-idx *graph*)))
    result))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (serialize (format nil "_anon:~A" uuid)))

(defun make-anonymous-node (&key graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) (serialized-value (make-anonymous-node-name uuid)))
      (let ((node (make-node 
		   :uuid uuid 
		   :value serialized-value
		   :type (aref serialized-value 1)
		   :graph *graph*)))
	(setf (gethash serialized-value (node-idx *graph*)) node)))))

(defun make-new-node (&key value graph )
  )
;(defun make-new-node (&key value graph (mailbox (sb-concurrency:make-mailbox :name val;ue)))
;  (let ((*graph* (or graph *graph*)))
;    (let ((serialized-value (serialize value)))
;      (sb-concurrency:send-message 
;       (graph-mailbox *graph*)
;       (make-message;
;	:content serialized-value
;	:sender mailbox
;	:task nil))
;      (msg-content (sb-concurrency:receive-message mailbox)))))

(defun make-new-node-ht (&key value graph)
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) (serialized-value (serialize value)))
      (let ((node (make-node 
		   :uuid uuid 
		   :value serialized-value
		   :type (aref serialized-value 1)
		   :graph *graph*)))
	;;(sb-ext:with-locked-hash-table ((node-idx *graph*))
	(or (lookup-node serialized-value *graph* t)
	    (setf (gethash serialized-value (node-idx *graph*)) node))))))



