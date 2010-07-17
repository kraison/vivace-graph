(in-package #:vivace-graph)

(defstruct (triple
	     (:predicate triple?)
	     (:print-function print-triple)
	     (:conc-name triple-))
  (uuid (make-uuid))
  (subject (make-anonymous-node))
  (predicate (make-anonymous-node))
  (object (make-anonymous-node))
  (deleted? nil)
  (version 0 :type (UNSIGNED-BYTE 64))
  (graph *graph*))

(defun print-triple (triple stream depth)
  (declare (ignore depth))
  (format stream "#<TRIPLE: '~A' '~A' '~A'>" 
	  (node-value (triple-subject triple))
	  (node-value (triple-predicate triple))
	  (node-value (triple-object triple))))

(defgeneric make-new-triple (graph subject predicate object))
(defgeneric insert-triple (triple))
(defgeneric index-triple (triple))
(defgeneric index-subject (triple))
(defgeneric index-predicate (triple))
(defgeneric index-object (triple))
(defgeneric do-indexing (graph))
(defgeneric lookup-triple (s p o &optional g))
(defgeneric lookup-triple-by-id (uuid &optional graph))

(defun get-subjects (value &optional graph)
  (skip-list-fetch-all (subject-idx (or graph *graph*)) value))

(defun get-predicates (value &optional graph)
  (skip-list-fetch-all (predicate-idx (or graph *graph*)) value))

(defun get-objects (value &optional graph)
  (skip-list-fetch-all (object-idx (or graph *graph*)) value))

(defmethod lookup-triple-by-id ((uuid uuid:uuid) &optional graph)
  (skip-list-lookup (triples (or graph *graph*)) uuid))

(defmethod lookup-triple-by-id ((uuid string) &optional graph)
  (lookup-triple-by-id (uuid:make-uuid-from-string uuid) graph))

(defmethod lookup-triple-by-id ((uuid vector) &optional graph)
  (lookup-triple-by-id (uuid:byte-array-to-uuid uuid) graph))

(defmethod lookup-triple (s p o &optional g)
  (let* ((*graph* (or g *graph*))
	 (triple (intersection (intersection (get-subjects s) (get-predicates p)) (get-objects o))))
    (if triple
	(lookup-triple-by-id (first triple))
	nil)))

(defmethod lookup-triple ((subject node) (predicate node) (object node) &optional g)
  (let* ((*graph* (or g *graph*))
	 (triple (intersection 
		  (intersection 
		   (get-subjects (node-value subject))
		   (get-predicates (node-value predicate)))
		  (get-objects (node-value object)))))
    (if triple
	(lookup-triple-by-id (first triple))
	nil)))

(defmethod make-new-triple ((graph graph) (subject node) (predicate node) (object node))
  (or (lookup-triple subject predicate object graph)
      (let ((triple (make-triple 
		     :graph graph :subject subject :predicate predicate :object object)))
	(with-mcas (:success-action #'(lambda () 
					(sb-ext:atomic-incf (node-ref-count subject))
					(sb-ext:atomic-incf (node-ref-count predicate))
					(sb-ext:atomic-incf (node-ref-count object))))
	  (insert-triple triple)
	  (index-triple triple)))))
;;	  (sb-concurrency:enqueue triple (needs-indexing-q (triple-graph triple)))))))

(defmethod delete-triple ((triple triple))
  (with-mcas ()
    (when (null (cas (triple-deleted? triple) nil t))
      (skip-list-add (deleted-triples (triple-graph triple)) (triple-uuid triple) triple)
      (skip-list-delete (triples (triple-graph triple)) (triple-uuid triple))
      (sb-concurrency:enqueue triple (delete-queue (triple-graph triple))))))
  
(defmethod do-deletions ((graph graph))
  (with-mcas ()
    (loop until (sb-concurrency:queue-empty-p (delete-queue graph)) do
	 (let ((triple (sb-concurrency:dequeue (delete-queue graph))))
	   ;; FIXME:  decrement ref-count for s, p and o
	   ;; FIXME:  de-index the triple
	   ))))

(defmethod insert-triple ((triple triple))
  (skip-list-add (triples (triple-graph triple)) (triple-uuid triple) triple))

(defmethod index-subject ((triple triple))
  (skip-list-add (subject-idx (triple-graph triple))
		 (node-value (triple-subject triple)) (triple-uuid triple)))

(defmethod index-predicate ((triple triple))
  (skip-list-add (predicate-idx (triple-graph triple))
		 (node-value (triple-predicate triple)) (triple-uuid triple)))

(defmethod index-object ((triple triple))
  (skip-list-add (object-idx (triple-graph triple))
		 (node-value (triple-object triple)) (triple-uuid triple)))

(defmethod index-triple ((triple triple))
  (with-mcas ()
    (index-subject triple)
    (index-predicate triple)
    (index-object triple)))

(defmethod do-indexing ((graph graph))
  (with-mcas ()
    (loop until (sb-concurrency:queue-empty-p (needs-indexing-q graph)) do
	 (index-triple (sb-concurrency:dequeue (needs-indexing-q graph))))))

(defun triple-test ()
  (let ((*graph* (make-new-graph :name "test graph")))
    (add-triple "Kevin" "loves" "Dustie")
    (add-triple "Kevin" "loves" "Echo")
    (add-triple "Dustie" "loves" "Kevin")
    (add-triple "Echo" "loves" "cat nip")
    (add-triple "Echo" "is-a" "cat")
    (add-triple "Kevin" "is-a" "Homo Sapien")
    (format t "Who loves whom? -> ~A~%" (get-triples :p "loves"))
    (skip-list-to-list (nodes *graph*))))
