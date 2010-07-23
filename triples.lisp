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

(defmethod deserialize ((become (eql +triple+)) bytes)
  "Decode a triple."
  (make-instance 'triple))

(defmethod serialize ((triple triple))
  "Encode a triple."
  (let ((vec (make-array 16 :fill-pointer t :adjustable t :element-type '(unsigned-byte 8))))
    (serialize (triple-uuid triple))
    ()))

(defgeneric make-new-triple (graph subject predicate object &key index-immediate?))
(defgeneric insert-triple (triple))
(defgeneric index-triple (triple))
(defgeneric index-subject (triple))
(defgeneric index-predicate (triple))
(defgeneric index-object (triple))
(defgeneric do-indexing (graph))
(defgeneric delete-triple (triple))
(defgeneric lookup-triple (s p o &key g))

(defun get-subjects (value &optional graph)
  (remove-if #'triple-deleted? (skip-list-fetch-all (subject-idx (or graph *graph*)) value)))

(defun get-predicates (value &optional graph)
  (remove-if #'triple-deleted? (skip-list-fetch-all (predicate-idx (or graph *graph*)) value)))

(defun get-objects (value &optional graph)
  (remove-if #'triple-deleted? (skip-list-fetch-all (object-idx (or graph *graph*)) value)))

(defmethod lookup-triple ((subject node) (predicate node) (object node) &key g)
  (gethash (list subject predicate object) (triples (or g *graph*))))

(defmethod lookup-triple (s p o &key g)
  (lookup-triple (lookup-node s) (lookup-node p) (lookup-node o) :g (or g *graph*)))

(defmethod make-new-triple ((graph graph) (subject node) (predicate node) (object node) 
			    &key (index-immediate? t))
  (let ((new-triple 
	 (make-triple :graph graph :subject subject :predicate predicate :object object)))
    (sb-ext:with-locked-hash-table ((triples graph))
      (let ((triple (lookup-triple subject predicate object)))
	(if (triple? triple)
	    (return-from make-new-triple triple)
	    (insert-triple new-triple))))
    (sb-ext:atomic-incf (node-ref-count subject))
    (sb-ext:atomic-incf (node-ref-count predicate))
    (sb-ext:atomic-incf (node-ref-count object))
    (if index-immediate? 
	(index-triple new-triple)
	(sb-concurrency:enqueue new-triple (needs-indexing-q (triple-graph new-triple))))
    new-triple))

(defmethod delete-triple ((triple triple))
  (when (null (cas (triple-deleted? triple) nil t))
    (deindex-triple triple)
    (remhash (list (triple-subject triple) (triple-predicate triple) (triple-object triple))
	     (triples (triple-graph triple)))
    (sb-ext:atomic-decf (node-ref-count (triple-subject triple)))
    (sb-ext:atomic-decf (node-ref-count (triple-predicate triple)))
    (sb-ext:atomic-decf (node-ref-count (triple-object triple)))
    nil))

(defmethod insert-triple ((triple triple))
  (setf (gethash 
	 (list (triple-subject triple) (triple-predicate triple) (triple-object triple))
	 (triples (triple-graph triple)))
	triple))

(defmethod index-subject ((triple triple))
  (skip-list-add (subject-idx (triple-graph triple)) (node-value (triple-subject triple)) triple))

(defmethod index-predicate ((triple triple))
  (skip-list-add (predicate-idx (triple-graph triple)) 
		 (node-value (triple-predicate triple)) triple))

(defmethod index-object ((triple triple))
  (skip-list-add (object-idx (triple-graph triple)) (node-value (triple-object triple)) triple))

(defmethod index-triple ((triple triple))
  (with-mcas ()
    (index-subject triple)
    (index-predicate triple)
    (index-object triple)))

(defmethod do-indexing ((graph graph))
  (loop until (sb-concurrency:queue-empty-p (needs-indexing-q graph)) do
       (index-triple (sb-concurrency:dequeue (needs-indexing-q graph)))))

(defmethod deindex-triple ((triple triple))
  (with-mcas ()
    (deindex-subject triple)
    (deindex-predicate triple)
    (deindex-object triple)))

(defmethod deindex-subject ((triple triple))
  (skip-list-delete (subject-idx (triple-graph triple)) 
		    (node-value (triple-subject triple)) triple))

(defmethod deindex-predicate ((triple triple))
  (skip-list-delete (predicate-idx (triple-graph triple))
		    (node-value (triple-predicate triple)) triple))

(defmethod deindex-object ((triple triple))
  (skip-list-delete (object-idx (triple-graph triple))
		    (node-value (triple-object triple)) triple))


(defun triple-test-1 ()
  (let ((*graph* (make-new-graph :name "test graph")))
    (dotimes (i 10000)
      (add-triple (format nil "S~A" i) (format nil "P~A" i) (format nil "O~A" i)))))

(defun triple-test-2 ()
  (let ((*graph* (make-new-graph :name "test graph")))
    (add-triple "Kevin" "loves" "Dustie")
    (add-triple "Kevin" "loves" "Echo")
    (add-triple "Dustie" "loves" "Kevin")
    (add-triple "Echo" "loves" "cat nip")
    (add-triple "Echo" "is-a" "cat")
    (add-triple "Kevin" "is-a" "Homo Sapien")
    (format t "NODES ~A:~%~A~%" (nodes *graph*) (skip-list-to-list (nodes *graph*)))
    (format t "Who loves whom? -> ~A~%" (get-triples :p "loves"))
    (format t "What species? -> ~A~%" (get-triples :p "is-a"))
    (get-subjects "Kevin")))
