(in-package #:vivace-graph)

(defgeneric add-triple (subject predicate object &optional graph))
(defgeneric get-triple-by-id (uuid &optional graph))

(defmethod add-triple ((subject node) (predicate node) (object node) &optional graph)
  (let ((*graph* (or graph *graph*)))
    (make-new-triple *graph* subject predicate object)))

(defmethod add-triple ((subject triple) (predicate node) (object node) &optional graph)
  (let ((*graph* (or graph *graph*)))
    (make-new-triple *graph* subject predicate object)))

(defmethod add-triple ((subject uuid:uuid) (predicate uuid:uuid) (object uuid:uuid) &optional graph)
  (let ((*graph* (or graph *graph*)))
    (make-new-triple *graph* subject predicate object)))

(defmethod add-triple (subject predicate object &optional graph)
  (let ((*graph* (or graph *graph*)))
    (add-triple (make-new-node :value subject)
		(make-new-node :value predicate)
		(make-new-node :value object)
		*graph*)))

(defmethod get-triple-by-id ((uuid uuid:uuid) &optional graph)
  (gethash uuid (triples (or graph *graph*))))

(defmethod get-triple-by-id ((uuid string) &optional graph)
  (get-triple-by-id (uuid:make-uuid-from-string uuid) graph))

(defmethod get-triple-by-id ((uuid vector) &optional graph)
  (get-triple-by-id (uuid:byte-array-to-uuid uuid) graph))

(defun get-triples-list (&key limit graph)
  (let ((*graph* (or graph *graph*)))
    (let ((result nil) (count 0))
      (sb-ext:with-locked-hash-table ((triples *graph*))
	(block hash
	  (maphash #'(lambda (k v) 
		       (declare (ignore k))
		       (if limit 
			   (if (> count limit) 
			       (return-from hash)
			       (incf count)))
		       (push v result))
		   (triples *graph*))))
      result)))

(defun get-triples (&key s p o g)
  (let ((*graph* (or g *graph*)) (nodes nil))
    (if s (setq nodes (get-subjects s)))
    (if p 
	(if s
	    (setq nodes (intersection nodes (get-predicates p)))
	    (setq nodes (get-predicates p))))
    (if o 
	(if (or p s)
	    (setq nodes (intersection nodes (get-objects o)))
	    (setq nodes (get-objects o))))
    (mapcar #'get-triple-by-id nodes)))

(defun triple-count (&optional graph)
  (hash-table-count (node-idx (or graph *graph*))))
