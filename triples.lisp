(in-package #:vivace-graph)

(defstruct (triple
	     (:predicate triple?)
	     (:print-function print-triple)
	     (:conc-name triple-))
  (uuid (make-uuid) :type uuid:uuid)
  (subject (make-anonymous-node) :type node)
  (predicate (make-anonymous-node) :type node)
  (object (make-anonymous-node) :type node)
  (deleted? nil)
  (version 0 :type integer)
  (graph *graph* :type graph))

(defun print-triple (triple stream depth)
  (declare (ignore depth))
  (format stream "#<TRIPLE: ~A ~A ~A>" 
	  (triple-subject triple) (triple-predicate triple) (triple-object triple)))

(defgeneric make-new-triple (graph subject predicate object))
(defgeneric insert-triple (triple))
(defgeneric index-triple (triple graph))
(defgeneric index-subject (subject graph))
(defgeneric index-predicate (predicate graph))
(defgeneric index-object (object graph))
(defgeneric do-indexing (graph))
(defgeneric lookup-triple (s p o &optional g))
(defgeneric lookup-triple-by-id (uuid &optional graph))

(defun get-subjects (value &optional graph)
  (gethash value (subject-idx (or graph *graph*))))

(defun get-predicates (value &optional graph)
  (gethash value (predicate-idx (or graph *graph*))))

(defun get-objects (value &optional graph)
  (gethash value (object-idx (or graph *graph*))))

(defmethod lookup-triple-by-id ((uuid uuid:uuid) &optional graph)
  (gethash uuid (triples (or graph *graph*))))

(defmethod lookup-triple-by-id ((uuid string) &optional graph)
  (lookup-triple-by-id (uuid:make-uuid-from-string uuid) graph))

(defmethod lookup-triple-by-id ((uuid vector) &optional graph)
  (lookup-triple-by-id (uuid:byte-array-to-uuid uuid) graph))

(defmethod lookup-triple (s p o &optional g)
  (let* ((*graph* (or g *graph*))
	 (triple (intersection (intersection (get-subjects s) (get-predicates p)) (get-objects o))))
    (if triple
	(get-triple-by-id (first triple))
	nil)))

(defmethod lookup-triple ((subject node) (predicate node) (object node) &optional g)
  (let* ((*graph* (or g *graph*))
	 (triple (intersection 
		  (intersection 
		   (get-subjects (node-value subject))
		   (get-predicates (node-value predicate)))
		  (get-objects (node-value object)))))
    (if triple
	(get-triple-by-id (first triple))
	nil)))

(defmethod make-new-triple ((graph graph) (subject node) (predicate node) (object node))
  (or (lookup-triple subject predicate object graph)
      (let ((triple (make-instance 'triple 
				   :graph graph :subject subject 
				   :predicate predicate :object object)))
	(prog1
	    (insert-triple triple)
	  (sb-concurrency:enqueue triple (needs-indexing-q (triple-graph triple)))))))

(defmethod delete-triple ((triple triple))
  (prog1
      (setf (triple-deleted? triple) t)
    (setf (gethash (triple-uuid triple) (deleted-triples (triple-graph triple))) triple)
    (remhash (triple-uuid triple) (triples (triple-graph triple)))
    (sb-concurrency:enqueue triple (delete-queue (triple-graph triple)))))
  
(defmethod insert-triple ((triple triple))
  (setf (gethash (triple-uuid triple) (triples (triple-graph triple))) triple))

(defmethod index-subject ((triple triple) (graph graph))
  (pushnew (triple-uuid triple) (gethash (node-value (triple-subject triple)) (subject-idx graph))))

(defmethod index-predicate ((triple triple) (graph graph))
  (pushnew (triple-uuid triple)
	   (gethash (node-value (triple-predicate triple)) (predicate-idx graph))))

(defmethod index-object ((triple triple) (graph graph))
  (pushnew (triple-uuid triple) (gethash (node-value (triple-object triple)) (object-idx graph))))

(defmethod index-triple ((triple triple) (graph graph))
  (index-subject triple graph)
  (index-predicate triple graph)
  (index-object triple graph))

(defmethod do-indexing ((graph graph))
  (loop until (sb-concurrency:queue-empty-p (needs-indexing-q graph)) do
       (index-triple (sb-concurrency:dequeue (needs-indexing-q graph)) graph)))
