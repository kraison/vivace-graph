(in-package #:vivace-graph)

(defstruct (node
	     (:conc-name %node-)
	     (:predicate node?))
  (uuid (make-uuid))
  (value nil))

(defgeneric lookup-node (value &optional serialized?))
(defgeneric make-anonymous-node-name (uuid))
(defgeneric incf-ref-count (node))
(defgeneric decf-ref-count (node))
(defgeneric save-node (node))
(defgeneric make-anonymous-node-name (uuid))
(defgeneric node-uuid (node))
(defgeneric node-value (node))
(defgeneric node-ref-count (node))

(defgeneric node-eql (n1 n2)
  (:method ((n1 node) (n2 node)) (uuid:uuid-eql (node-uuid n1) (node-uuid n2)))
  (:method (n1 n2) nil))

(defgeneric node-equal (n1 n2)
  (:method ((n1 node) (n2 node)) 
    (and (uuid:uuid-eql (node-uuid n1) (node-uuid n2))
	 (equal (node-value n1) (node-value n2))))
  (:method (n1 n2) nil))

(defmethod node-uuid ((node node))
  (if (eq (%node-uuid node) +needs-lookup+)
      (setf (%node-uuid node) 
	    (get-btree (triple-db *graph*) (make-slot-key (node-value node) "uuid")))
      (%node-uuid node)))

(defmethod node-value ((node node))
  (%node-value node))

(defmethod node-ref-count ((node node))
  (let ((count (get-btree (triple-db *graph*) (make-slot-key (node-value node) "ref-count"))))
    (if (numberp count)
	count
	0)))

(defmethod save-node ((node node))
  (with-transaction ((triple-db *graph*))
    (set-btree (triple-db *graph*) (make-slot-key (node-value node) "value") (node-value node))
    (set-btree (triple-db *graph*) (make-slot-key (node-value node) "uuid") (node-uuid node))
    (set-btree (triple-db *graph*) 
	       (make-slot-key (node-value node) "ref-count") (node-ref-count node))))

(defmethod cache-node ((node node))
  (setf (gethash (node-value node) (node-cache *graph*)) node))

(defmethod lookup-node ((node node) &optional serialized?)
  (declare (ignore serialized?))
  node)

(defmethod lookup-node (value &optional serialized?)
  (declare (ignore serialized?))
  ;;(if serialized? (setq value (deserialize value)))
  (or (gethash value (node-cache *graph*))
      (let ((value (get-btree (triple-db *graph*) (make-slot-key value "value"))))
	(if value
	    (let ((node (make-node :uuid +needs-lookup+ :value value)))
	      (cache-node node))
	    nil))))

(defmethod incf-ref-count ((node node))  
  (set-btree (triple-db *graph*) 
	     (make-slot-key (node-value node) "ref-count") 
	     (1+ (node-ref-count node))
	     :mode :replace)
  (node-ref-count node))

(defmethod decf-ref-count ((node node))
  (set-btree (triple-db *graph*)
	     (make-slot-key (node-value node) "ref-count") 
	     (1- (node-ref-count node))
	     :mode :replace)
  (let ((count (node-ref-count node)))
    (if (= 0 count) (sb-concurrency:enqueue node (delete-queue *graph*)))
    count))

(defmethod make-anonymous-node-name ((uuid uuid:uuid))
  (format nil "_anon:~A" uuid))

(defun make-anonymous-node (&key graph (cache? t))
  (let ((*graph* (or graph *graph*)))
    (let* ((uuid (make-uuid)) 
	   (value (make-anonymous-node-name uuid)))
      (let ((node (make-node :uuid uuid 
			     :value value)))
	(save-node node)
	(when cache? (cache-node node))
	node))))

(defun make-new-node (&key value graph (cache? t))
  (if (node? value)
      value
      (let ((*graph* (or graph *graph*)))
	(or (lookup-node value)
	    (let ((node (make-node :value value)))
	      (handler-case
		  (save-node node)
		(persistence-error (condition)
		  (or (lookup-node value)
		      (error "Problem making node ~A: ~A" value condition)))
		(:no-error (status)
		  (declare (ignore status))
		  (when cache? (cache-node node))
		  node)))))))

(defmethod delete-node ((node node))
  (with-transaction ((triple-db *graph*))
    node))

(defun make-new-node-unsafe (&key value)
  (let ((node (make-node :value value)))
    (save-node node)
    (cache-node node)))

