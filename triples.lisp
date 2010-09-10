(in-package #:vivace-graph)

(defstruct (triple
	     (:predicate triple?)
	     (:print-function print-triple)
	     (:conc-name %triple-))
  (uuid (make-uuid) :type uuid:uuid)
  (subject nil)
  (predicate nil :type predicate)
  (object nil)
  (timestamp (now))
  (belief-factor +cf-true+)
  (derived? nil)
  (deleted? nil))

(defgeneric make-new-triple (graph subject predicate object 
				   &key index-immediate? enqueue-for-rules-check?))
(defgeneric insert-triple (triple))
(defgeneric index-triple (triple &optional db))
(defgeneric deindex-triple (triple &optional db))
(defgeneric index-subject (triple))
(defgeneric index-predicate (triple))
(defgeneric index-object (triple))
(defgeneric index-text (triple))
(defgeneric deindex-text (triple))
(defgeneric do-indexing (graph))
(defgeneric delete-triple (triple))
(defgeneric lookup-triple (s p o &key g))
(defgeneric save-triple (triple &optional db))
(defgeneric predicate (triple))
(defgeneric subject (triple))
(defgeneric object (triple))
(defgeneric triple-uuid (triple))
(defgeneric triple-predicate (triple))
(defgeneric triple-subject (triple))
(defgeneric triple-object (triple))
(defgeneric triple-timestamp (triple))
(defgeneric triple-belief-factor (triple))
(defgeneric triple-deleted? (triple))
(defgeneric triple-derived? (triple))

;; FIXME: this whole mess should be implemented via the MOP.  So messy...
(defmethod triple-uuid ((triple triple))
  "Return the decoded UUID of the triple."
  (%triple-uuid triple))

(defmethod triple-predicate ((triple triple))
  "Return the decoded predicate of the triple."
  (if (eql +needs-lookup+ (%triple-predicate triple))
      (setf (%triple-predicate triple) 
	    (lookup-predicate
	     (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "predicate"))))
      (%triple-predicate triple)))

(defmethod triple-subject ((triple triple))
  "Return the decoded subject of the triple."
  (if (eql +needs-lookup+ (%triple-subject triple))
      (setf (%triple-subject triple) 
	    ;;(lookup-node
	     (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "subject"))) ;;)
      (%triple-subject triple)))

(defmethod triple-object ((triple triple))
  "Return the decoded object of the triple."
  (if (eql +needs-lookup+ (%triple-object triple))
      (setf (%triple-object triple) 
	    ;;(lookup-node
	     (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "object"))) ;;)
      (%triple-object triple)))

(defmethod triple-timestamp ((triple triple))
  "Return the decoded timestamp of the triple."
  (if (eql +needs-lookup+ (%triple-timestamp triple))
      (setf (%triple-timestamp triple) 
	    (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "timestamp")))
      (%triple-timestamp triple)))

(defmethod triple-belief-factor ((triple triple))
  "Return the decoded belief factor of the triple."
  (if (eql +needs-lookup+ (%triple-belief-factor triple))
      (setf (%triple-belief-factor triple) 
	    (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "belief-factor")))
      (%triple-belief-factor triple)))
  
(defmethod triple-deleted? ((triple triple))
  "Is the triple marked as deleted?"
  (if (eql +needs-lookup+ (%triple-deleted? triple))
      (setf (%triple-deleted? triple) 
	    (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "deleted?")))
      (%triple-deleted? triple)))

(defmethod triple-derived? ((triple triple))
  "Is this triple derived from a rule?"
  (if (eql +needs-lookup+ (%triple-derived? triple))
      (setf (%triple-derived? triple) 
	    (get-btree (triple-db *graph*) (make-slot-key (triple-uuid triple) "derived?")))
      (%triple-derived? triple)))

(defgeneric triple-eql (t1 t2)
  (:documentation "Compare the UUIDs of two triples.")
  (:method ((t1 triple) (t2 triple)) (uuid:uuid-eql (triple-uuid t1) (triple-uuid t2)))
  (:method (t1 t2) nil))

(defgeneric triple-equal (t1 t2)
  (:documentation "Compare the subject, predicate and object of two triples using node-equal and predicate-eql.")
  (:method ((t1 triple) (t2 triple)) 
    (and (triple-eql t1 t2)
	 ;;(node-equal (triple-subject t1) (triple-subject t2))
	 (equal (triple-subject t1) (triple-subject t2))
	 (predicate-eql (triple-predicate t1) (triple-predicate t2))
	 ;;(node-equal (triple-object t1) (triple-object t2))))
	 (equal (triple-object t1) (triple-object t2))))
  (:method (t1 t2) nil))

(defun print-triple (triple stream depth)
  (declare (ignore depth))
  (format stream "#<TRIPLE: '~A' '~A' '~A'>" 
	  ;;(node-value (triple-subject triple))
	  (triple-subject triple)
	  (pred-name (triple-predicate triple))
	  ;;(node-value (triple-object triple))))
	  (triple-object triple)))

(defmethod predicate ((triple triple))
  "Return the predicate name of the triple's predicate."
  (pred-name (triple-predicate triple)))

(defmethod predicate ((tuple list))
  "Return the predicate name of the triple's predicate."
  (if (predicate? (first tuple))
      (pred-name (first tuple))
      (first tuple)))

(defmethod subject ((triple triple))
  "Return the value of the triple's subject."
  ;;(node-value (triple-subject triple)))
  (triple-subject triple))

(defmethod subject ((tuple list))
  "Return the node value of the triple's subject."
;  (if (node? (second tuple))
;      (node-value (second tuple))
      (second tuple))

(defmethod object ((triple triple))
  "Return the node value of the triple's object."
  ;;(node-value (triple-object triple)))
  (triple-object triple))

(defmethod object ((tuple list))
  "Return the node value of the triple's object."
;  (if (node? (third tuple))
;      (node-value (third tuple))
      (third tuple))

(defmethod belief-factor ((triple triple))
  "Return the belief factor of a triple."
  (triple-belief-factor triple))

(defmethod as-list ((triple triple))
  "Translate the triple into a list of (predicate subject object)."
  (list (pred-name (triple-predicate triple)) 
	;;(node-value (triple-subject triple)) 
	;;(node-value (triple-object triple))))
	(triple-subject triple)
	(triple-object triple)))

(defmethod make-spo-key (value)
  (make-serialized-key +triple-key+ value))

(defun make-triple-key-from-values (s p o)
  (format nil "~A~A~A~A~A" s #\Nul p #\Nul o))

(defmethod save-triple ((triple triple) &optional db)
  "Persist triple."
  (let ((db (or db (triple-db *graph*))))
    (let ((key (uuid:print-bytes nil (triple-uuid triple)))
	  (spo-key (make-triple-key-from-values 
		    ;;(node-value (triple-subject triple))
		    (triple-subject triple)
		    (pred-name (triple-predicate triple))
		    ;;(node-value (triple-object triple)))))
		    (triple-object triple))))
      (with-transaction (db)
	(set-btree (triple-db *graph*) spo-key (triple-uuid triple) :key-serializer #'make-spo-key)
	(set-btree (triple-db *graph*) (make-slot-key key "uuid") (triple-uuid triple))
	(set-btree (triple-db *graph*) (make-slot-key key "subject") 
		   ;;(node-value (triple-subject triple)))
		   (triple-subject triple))
	(set-btree (triple-db *graph*) (make-slot-key key "predicate")
		   (string-downcase (symbol-name (pred-name (triple-predicate triple)))))
	(set-btree (triple-db *graph*) (make-slot-key key "object") 
		   ;;(node-value (triple-object triple)))
		   (triple-object triple))
	(set-btree (triple-db *graph*) (make-slot-key key "timestamp") (triple-timestamp triple))
	(set-btree (triple-db *graph*) (make-slot-key key "belief-factor") 
		   (triple-belief-factor triple))
	(when (%triple-derived? triple)
	  (set-btree (triple-db *graph*) (make-slot-key key "derived?") (triple-derived? triple)))
	(when (%triple-deleted? triple)
	  (set-btree (triple-db *graph*) (make-slot-key key "deleted?") 
		     (triple-deleted? triple)))))))

(defmethod remove-triple ((triple triple) &optional db)
  "Remove a triple."
  (let ((db (or db (triple-db *graph*))))
    (let ((key (uuid:print-bytes nil (triple-uuid triple)))
	  (spo-key (make-triple-key-from-values 
		    ;;(node-value (triple-subject triple))
		    (triple-subject triple)
		    (pred-name (triple-predicate triple))
		    ;;(node-value (triple-object triple)))))
		    (triple-object triple))))
      (with-transaction (db)
	(rem-btree (triple-db *graph*) spo-key (triple-uuid triple) :key-serializer #'make-spo-key)
	(rem-btree (triple-db *graph*) (make-slot-key key "uuid") (triple-uuid triple))
	(rem-btree (triple-db *graph*) (make-slot-key key "subject") 
		   ;;(node-value (triple-subject triple)))
		   (triple-subject triple))
	(rem-btree (triple-db *graph*) (make-slot-key key "predicate")
		   (string-downcase (symbol-name (pred-name (triple-predicate triple)))))
	(rem-btree (triple-db *graph*) (make-slot-key key "object") 
		   ;;(node-value (triple-object triple)))
		   (triple-object triple))
	(rem-btree (triple-db *graph*) (make-slot-key key "timestamp") (triple-timestamp triple))
	(rem-btree (triple-db *graph*) (make-slot-key key "belief-factor") 
		   (triple-belief-factor triple))
	(ignore-errors
	  (rem-btree (triple-db *graph*) (make-slot-key key "derived?") (triple-derived? triple))
	  (rem-btree (triple-db *graph*) (make-slot-key key "deleted?") 
		     (triple-deleted? triple)))))))

;(defmethod make-subject-key ((node node))
;  (make-serialized-key +triple-subject+ (node-value node)))

(defun make-subject-key (node)
  (make-serialized-key +triple-subject+ node))

;(defmethod make-object-key ((node node))
;  (make-serialized-key +triple-object+ (node-value node)))

(defun make-object-key (node)
  (make-serialized-key +triple-object+ node))

(defmethod make-predicate-key ((predicate predicate))
  (make-serialized-key +triple-predicate+ (string-downcase (symbol-name (pred-name predicate)))))

(defmethod make-predicate-key ((symbol symbol))
  (make-serialized-key +triple-predicate+ (string-downcase (symbol-name symbol))))

(defmethod make-predicate-key ((string string))
  (make-serialized-key +triple-predicate+ (string-downcase string)))

(defun concat-keys (k1 k2)
  (format nil "~A~A~A" k1 #\Nul k2))

(defun make-combined-triple-key (v1 v2 index-type)
  (make-serialized-key index-type (concat-keys v1 v2)))

(defmethod make-triple-sp-key ((triple triple))
  (make-combined-triple-key 
   ;;(node-value (triple-subject triple)) 
   (triple-subject triple)
   (pred-name (triple-predicate triple)) 
   +triple-subject-predicate+))

(defmethod make-triple-sp-key-from-string (string)
  (make-serialized-key +triple-subject-predicate+ string))

(defmethod make-triple-so-key ((triple triple))
  (make-combined-triple-key 
   ;;(node-value (triple-subject triple)) 
   ;;(node-value (triple-object triple))
   (triple-subject triple)
   (triple-object triple)
   +triple-subject-object+))

(defmethod make-triple-so-key-from-string (string)
  (make-serialized-key +triple-subject-object+ string))

(defmethod make-triple-po-key ((triple triple))
  (make-combined-triple-key 
   (pred-name (triple-predicate triple)) 
   ;;(node-value (triple-object triple))
   (triple-object triple)
   +triple-predicate-object+))

(defmethod make-triple-po-key-from-string (string)
  (make-serialized-key +triple-predicate-object+ string))

(defun get-indexed-triples (value key-serializer)
  (get-btree (triple-db *graph*)
	     value 
	     :mode :klist
	     :serializer key-serializer))

(defun get-subjects (value)
  (get-indexed-triples value #'make-subject-key))

(defun get-predicates (name)
  (get-indexed-triples name #'make-predicate-key))

(defun get-objects (value)
  (get-indexed-triples value #'make-object-key))

(defun get-subjects-predicates (subject predicate)
  (get-indexed-triples (concat-keys 
			;;(if (node? subject) (node-value subject) subject)
			subject
			(if (predicate? predicate) (pred-name predicate) predicate))
		       #'make-triple-sp-key-from-string))

(defun get-subjects-objects (subject object)
  (get-indexed-triples (concat-keys subject object)
			;;(if (node? subject) (node-value subject) subject)
			;;(if (node? object) (node-value object) object))
		       #'make-triple-so-key-from-string))

(defun get-predicates-objects (predicate object)
  (get-indexed-triples (concat-keys 
			(if (predicate? predicate) (pred-name predicate) predicate)
			;;(if (node? object) (node-value object) object))
			object)
		       #'make-triple-po-key-from-string))
  
(defun lookup-triple-in-db (s p o g)
  (handler-case
      (let* ((key (make-triple-key-from-values s p o))
	     (*graph* (or g *graph*))
	     (db (triple-db *graph*)))
	(let ((uuid (get-btree db key :serializer #'make-spo-key)))
	  (when (uuid:uuid? uuid)
	    (let* ((ukey (uuid:print-bytes nil uuid))
		   (triple 
		    (make-triple 
		     :uuid uuid
		     ;;:subject (lookup-node (get-btree db (make-slot-key ukey "subject")))
		     :subject (get-btree db (make-slot-key ukey "subject"))
		     :predicate (lookup-predicate (get-btree db (make-slot-key ukey "predicate")))
		     ;;:object (lookup-node (get-btree db (make-slot-key ukey "object")))
		     :object (get-btree db (make-slot-key ukey "object"))
		     :timestamp +needs-lookup+
		     :belief-factor +needs-lookup+
		     :derived? +needs-lookup+
		     :deleted? +needs-lookup+)))
	      (cache-triple triple)))))
    (serialization-error (condition)
      (declare (ignore condition))
      (format t "Cannot lookup ~A/~A/~A~%" s p o)
      nil)))

(defmethod lookup-triple (s p o &key g)
  (or (gethash (list s p o) (triple-cache (or g *graph*)))
      (lookup-triple-in-db s p o (or g *graph*))))

;(defmethod lookup-triple ((subject node) (predicate predicate) (object node) &key g)
;  (or (gethash (list subject predicate object) (triple-cache (or g *graph*)))
;      (lookup-triple (node-value subject) (pred-name predicate) (node-value object) 
;		     :g (or g *graph*))))

(defmethod lookup-triple-by-id (uuid &key g)
  (or (gethash (if (uuid:uuid? uuid) (uuid:print-bytes nil uuid) uuid)
	       (triple-cache (or g *graph*)))
      (handler-case
	  (let* ((ukey (uuid:print-bytes nil uuid)) (db (triple-db *graph*)))
	    (let ((uuid (get-btree db (make-slot-key ukey "uuid"))))
	      (when (uuid:uuid? uuid)
		(let ((p (lookup-predicate (get-btree db (make-slot-key ukey "predicate"))))
		      ;;(s (lookup-node (get-btree db (make-slot-key ukey "subject"))))
		      ;;(o (lookup-node (get-btree db (make-slot-key ukey "object")))))
		      (s (get-btree db (make-slot-key ukey "subject")))
		      (o (get-btree db (make-slot-key ukey "object"))))
		  (let ((triple (make-triple :uuid uuid
					     :subject s
					     :predicate p
					     :object o
					     :timestamp +needs-lookup+
					     :belief-factor +needs-lookup+
					     :derived? +needs-lookup+
					     :deleted? +needs-lookup+)))
		    (cache-triple triple))))))
	(serialization-error (condition)
	  (format t "Cannot lookup ~A: ~A~%" uuid condition)
	  nil))))

(defmethod uncache-triple ((triple triple) &optional ht)
  (let ((ht (or ht (triple-cache *graph*))))
    (remhash (uuid:print-bytes nil (triple-uuid triple)) ht)
    (remhash (list (triple-subject triple) (triple-predicate triple) (triple-object triple))
	     ht)
    (remhash (list 
	      ;;(node-value (triple-subject triple)) 
	      (triple-subject triple)
	      (pred-name (triple-predicate triple)) 
	      ;;(node-value (triple-object triple)))
	      (triple-object triple))
	     ht)))

(defmethod cache-triple ((triple triple) &optional ht)
  (let ((ht (or ht (triple-cache *graph*))))
    (setf (gethash (uuid:print-bytes nil (triple-uuid triple)) ht) triple)
    (setf (gethash (list 
		    ;;(node-value (triple-subject triple))
		    (triple-subject triple)
		    (pred-name (triple-predicate triple))
		    ;;(node-value (triple-object triple)))
		    (triple-object triple))
		   ht)
	  triple)
    (setf (gethash (list (triple-subject triple) 
			 (triple-predicate triple) 
			 (triple-object triple))
		   ht)
	  triple)))

(defmethod index-text ((triple triple))
  (when (or (stringp 
	     ;;(node-value (triple-subject triple)))
	     (triple-subject triple))
	    (stringp 
	     ;;(node-value (triple-object triple))))
	     (triple-object triple)))
    (let ((doc (make-instance 'montezuma:document)))
      (montezuma:add-field 
       doc (montezuma:make-field "uuid" 
				 (format nil "~A" (triple-uuid triple))
				 :stored t :index :untokenized))
      (when (stringp 
	     ;;(node-value (triple-subject triple)))
	     (triple-subject triple))
	(montezuma:add-field 
	 doc (montezuma:make-field "subject" 
				   ;;(node-value (triple-subject triple)) 
				   (triple-subject triple)
				   :stored t :index :tokenized)))
      (when (stringp 
	     ;;(node-value (triple-object triple)))
	     (triple-object triple))
	(montezuma:add-field 
	 doc (montezuma:make-field "object" 
				   ;;(node-value (triple-object triple))
				   (triple-object triple)
				   :stored t :index :tokenized)))
      (montezuma:add-document-to-index (full-text-idx *graph*) doc))))

(defmethod deindex-text ((triple triple)) 
  (let ((docs nil))
    (montezuma:search-each 
     (full-text-idx *graph*)
     (uuid:print-bytes nil (triple-uuid triple))
     #'(lambda (doc score) (declare (ignore score)) (push doc docs)))
    (dolist (doc docs)
      (montezuma:delete-document (full-text-idx *graph*) doc))))
 
(defun map-text-search (string fn &key collect)
  (let ((result nil))
    (montezuma:search-each 
     (full-text-idx *graph*)
     string
     #'(lambda (doc score)
	 (declare (ignore score))
	 (let ((r
		(funcall fn
			 (lookup-triple-by-id
			  (montezuma:document-value 
			   (montezuma:get-document (full-text-idx *graph*) doc) "uuid")))))
	   (if collect (push r result)))))
    (nreverse result)))

(defmethod do-indexing ((graph graph))
  "Index all pending triples for GRAPH."
  (loop until (sb-concurrency:queue-empty-p (needs-indexing-q graph)) do
       (index-triple (sb-concurrency:dequeue (needs-indexing-q graph)))))

(defmethod add-triple-unsafe ((graph graph) subject (predicate predicate) object
			      &key (certainty-factor +cf-true+))
  (let ((triple (make-triple :subject subject :predicate predicate :object object 
			     :belief-factor certainty-factor)))
    (save-triple triple)
;    (incf-ref-count subject)
;    (incf-ref-count object)
    (sb-concurrency:enqueue triple (needs-indexing-q graph))
    (cache-triple triple)))

(defmethod index-triple-unsafe ((triple triple) &optional db)
  (let ((db (or db (triple-db *graph*))))
    (set-btree db (triple-subject triple) (triple-uuid triple)
	       :mode :concat :key-serializer #'make-subject-key)
    (set-btree db (triple-object triple) (triple-uuid triple)
	       :mode :concat :key-serializer #'make-object-key)
    (set-btree db (triple-predicate triple) (triple-uuid triple)
	       :mode :concat :key-serializer #'make-predicate-key)
    (set-btree db triple (triple-uuid triple)
	       :mode :concat :key-serializer #'make-triple-sp-key)
    (set-btree db triple (triple-uuid triple)
	       :mode :concat :key-serializer #'make-triple-so-key)
    (set-btree db triple (triple-uuid triple)
	       :mode :concat :key-serializer #'make-triple-po-key)))

