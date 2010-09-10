(in-package #:vivace-graph)

(defmethod make-new-triple ((graph graph) subject (predicate predicate) object
			    &key (index-immediate? t) (certainty-factor +cf-true+) derived?
			    (enqueue-for-rules-check? nil))
  "Add a new triple to the datastore."
  (or (lookup-triple subject predicate object)
      (let ((triple (make-triple :subject subject :predicate predicate :object object 
				 :belief-factor certainty-factor :derived? derived?)))
	(handler-case
	    (with-transaction ((triple-db graph))
	      (save-triple triple)
;	      (incf-ref-count subject)
;	      (incf-ref-count object)
	      (if index-immediate? 
		  (index-triple triple)
		  (sb-concurrency:enqueue triple (needs-indexing-q graph))))
	  (persistence-error (condition)
	    (or (lookup-triple subject predicate object)
		(error condition)))
	  (:no-error (status)
	    (declare (ignore status))
	    (prog1
		(cache-triple triple)
	      (if enqueue-for-rules-check?
		  (skip-list-add (production-pq *graph*) (triple-timestamp triple) triple))))))))

(defmethod bulk-add-triples ((graph graph) tuple-list &key cache? trigger?)
  "Add a list of (p s o) tuples to the database in a single transaction."
  (handler-case
      (with-transaction ((triple-db graph))
	(let ((new-triples nil))
	  (dolist (tuple tuple-list)
	    (or (triple? (lookup-triple-in-db (elt tuple 0) (elt tuple 1) (elt tuple 2) graph))
		(let ((s (elt tuple 0)) ;;(make-new-node :value (elt tuple 0)))
		      (p (make-new-predicate :name (elt tuple 1)))
		      (o (elt tuple 2)) ;;(make-new-node :value (elt tuple 2)))
		      (b (or (belief-factor tuple) +cf-true+)))
		  (let ((triple (make-triple :subject s :predicate p :object o :belief-factor b)))
		    (save-triple triple)
;		    (incf-ref-count s)
;		    (incf-ref-count o)
		    (sb-concurrency:enqueue triple (needs-indexing-q graph))
		    (push triple new-triples)))))
	  new-triples))
      (:no-error (new-triples)
	(when new-triples
	  (dolist (triple new-triples)
	    (when cache?
	      (cache-triple triple))
	    (when trigger?
	      (skip-list-add (production-pq *graph*) (triple-timestamp triple) triple))))
	t)))
  
(defmethod index-triple ((triple triple) &optional db)
  "Index a triple."
  (with-transaction (db)
    (index-triple-unsafe triple db)))

(defmethod deindex-triple ((triple triple) &optional db)
  "Remove a triple from the indices."
  (let ((db (or db (triple-db *graph*))))
    (with-transaction (db)
      (rem-btree db (triple-subject triple) :value (triple-uuid triple)
		 :key-serializer #'make-subject-key)
      (rem-btree db (triple-object triple) :value (triple-uuid triple)
		 :key-serializer #'make-object-key)
      (rem-btree db (triple-predicate triple) :value (triple-uuid triple)
		 :key-serializer #'make-predicate-key)
      (rem-btree db triple :value (triple-uuid triple)
		 :key-serializer #'make-triple-sp-key)
      (rem-btree db triple :value (triple-uuid triple)
		 :key-serializer #'make-triple-so-key)
      (rem-btree db triple :value (triple-uuid triple)
		 :key-serializer #'make-triple-po-key))))

(defmethod delete-triple ((triple triple))
  "Delete a triple;  this actually marks the triple as deleted and adds it to the delete-queue. The
triple is thus scheduled to be shadowed."
  (when (not (triple-deleted? triple))
    (when (null (cas (%triple-deleted? triple) nil t))
      (cas (%triple-timestamp triple) (%triple-timestamp triple) (now))
      (sb-concurrency:enqueue triple (delete-queue *graph*)))))

(defmethod shadow-triple ((triple triple))
  "Move a deleted triple from the active btree to the shadow btree."
  (handler-case
      (progn
	(with-transaction ((deleted-triple-db *graph*))
	  (save-triple triple (deleted-triple-db *graph*))
	  (index-triple triple (deleted-triple-db *graph*)))
	(with-transaction ((triple-db *graph*))
	  (remove-triple triple)
	  (deindex-triple triple)))
    (persistence-error (condition)
      (format t "Cannot shadow deleted triple ~A: ~A~%" triple condition))
    (:no-error (status)
      (declare (ignore status))
      (uncache-triple triple)
      (cache-triple (deleted-triple-cache *graph*)))))

