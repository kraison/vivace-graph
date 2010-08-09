(in-package #:vivace-graph)

(defstruct (triple
	     (:predicate triple?)
	     (:print-function print-triple)
	     (:conc-name triple-))
  (uuid (make-uuid))
  (subject nil)
  (predicate nil)
  (object nil)
  (timestamp (now))
  (belief-factor +cf-true+)
  (derived? nil)
  (deleted? nil))

(defgeneric make-new-triple (graph subject predicate object &key index-immediate?))
(defgeneric insert-triple (triple))
(defgeneric index-triple (triple))
(defgeneric index-subject (triple))
(defgeneric index-predicate (triple))
(defgeneric index-object (triple))
(defgeneric index-text (triple))
(defgeneric deindex-text (triple))
(defgeneric do-indexing (graph))
(defgeneric delete-triple (triple))
(defgeneric lookup-triple (s p o &key g))
(defgeneric save-triple (triple))
(defgeneric predicate (triple))
(defgeneric subject (triple))
(defgeneric object (triple))

(defun print-triple (triple stream depth)
  (declare (ignore depth))
  (format stream "#<TRIPLE: '~A' '~A' '~A'>" 
	  (node-value (triple-subject triple))
	  (pred-name (triple-predicate triple))
	  (node-value (triple-object triple))))

(defmethod predicate ((triple triple))
  (pred-name (triple-predicate triple)))

(defmethod predicate ((tuple list))
  (first tuple))

(defmethod subject ((triple triple))
  (node-value (triple-subject triple)))

(defmethod subject ((tuple list))
  (second tuple))

(defmethod object ((triple triple))
  (node-value (triple-object triple)))

(defmethod object ((tuple list))
  (third tuple))

(defgeneric triple-eql (t1 t2)
  (:method ((t1 triple) (t2 triple)) (uuid:uuid-eql (triple-uuid t1) (triple-uuid t2)))
  (:method (t1 t2) nil))

(defgeneric triple-equal (t1 t2)
  (:method ((t1 triple) (t2 triple)) 
    (and (triple-eql t1 t2)
	 (node-equal (triple-subject t1) (triple-subject t2))
	 (predicate-eql (triple-predicate t1) (triple-predicate t2))
	 (node-equal (triple-object t1) (triple-object t2))))
  (:method (t1 t2) nil))

(defmethod as-list ((triple triple))
  (list (triple-predicate triple) (triple-subject triple) (triple-object triple)))

(defmethod belief-factor ((triple triple))
  (triple-belief-factor triple))

(defmethod deserialize-help ((become (eql +triple+)) bytes)
  "Decode a triple. FIXME: add support for derived? field."
  (declare (optimize (speed 3)))
  (destructuring-bind (subject predicate object belief id derived?) (extract-all-subseqs bytes)
    (make-triple
     :uuid (deserialize id)
     :belief-factor (deserialize belief)
     :derived? (deserialize derived?)
     :subject (lookup-node subject *graph* t)
     :predicate (lookup-predicate predicate *graph*)
     :object (lookup-node object *graph* t))))

(defmethod serialize ((triple triple))
  "Encode a triple for storage."
  (serialize-multiple +triple+
		      (node-value (triple-subject triple))
		      (make-serialized-key (triple-predicate triple))
		      (node-value (triple-object triple))
		      (triple-belief-factor triple) 
		      (triple-uuid triple)
		      (triple-derived? triple)))

(defun make-triple-key-from-values (s p o)
  (serialize-multiple +triple-key+ s (or (and (symbolp p) p) (intern p)) o))

(defmethod make-serialized-key ((triple triple))
  (declare (optimize (speed 3)))
  (make-triple-key-from-values (node-value (triple-subject triple))
			       (pred-name (triple-predicate triple))
			       (node-value (triple-object triple))))

(defmethod save-triple ((triple triple))
  (let ((key (make-serialized-key triple))
	(value (serialize triple)))
    (store-object (triple-db *graph*) key value :mode :keep)))

(defun make-triple-index-key (item key-type)
  (serialize-multiple key-type item))
    
(defmethod make-triple-uuid-key ((uuid uuid:uuid))
  (serialize uuid))

(defun make-combined-triple-key (v1 v2 index-type)
  (make-triple-index-key (format nil "~A~A~A" v1 #\Nul v2) index-type))

(defmethod make-triple-sp-key ((triple triple))
  (make-combined-triple-key (node-value (triple-subject triple)) 
			    (pred-name (triple-predicate triple)) 
			    +triple-subject-predicate+))

(defmethod make-triple-so-key ((triple triple))
  (make-combined-triple-key (node-value (triple-subject triple)) 
			    (node-value (triple-object triple))
			    +triple-subject-object+))

(defmethod make-triple-po-key ((triple triple))
  (make-combined-triple-key (pred-name (triple-predicate triple)) 
			    (node-value (triple-object triple))
			    +triple-predicate-object+))

(defun get-indexed-triples (value index-type &optional graph)
  (mapcar #'(lambda (vec) (deserialize (lookup-object (triple-db (or graph *graph*)) vec)))
	  (lookup-objects (triple-db (or graph *graph*)) 
			  (make-triple-index-key value index-type))))

(defun get-subjects (value &optional graph)
  (get-indexed-triples value +triple-subject+ graph))

(defun get-predicates (name &optional graph)
  (get-indexed-triples (or (and (symbolp name) name) (intern name)) +triple-predicate+ graph))

(defun get-objects (value &optional graph)
  (get-indexed-triples value +triple-object+ graph))

(defun get-subjects-predicates (subject predicate &optional graph)
  (get-indexed-triples 
   (format nil "~A~A~A" 
	   (if (node? subject) (node-value subject) subject) #\Nul
	   (if (predicate? predicate) (pred-name predicate) predicate))
   +triple-subject-predicate+ graph))

(defun get-subjects-objects (subject object &optional graph)
  (get-indexed-triples 
   (format nil "~A~A~A"
	   (if (node? subject) (node-value subject) subject) #\Nul
	   (if (node? object) (node-value object) object))
   +triple-subject-object+ graph))

(defun get-predicates-objects (predicate object &optional graph)
  (get-indexed-triples 
   (format nil "~A~A~A"
	   (if (predicate? predicate) (pred-name predicate) predicate) #\Nul
	   (if (node? object) (node-value object) object))
   +triple-predicate-object+ graph))
  
(defmethod lookup-triple ((subject node) (predicate predicate) (object node) &key g)
  (or (gethash (list subject predicate object) (triple-cache (or g *graph*)))
      (lookup-triple (node-value subject) (pred-name predicate) (node-value object) 
		     :g (or g *graph*))))

(declaim (inline lookup-triple-in-db))
(defun lookup-triple-in-db (s p o g)
  (handler-case
      (let ((key (make-triple-key-from-values s p o)))
	(let ((raw (lookup-object (triple-db (or g *graph*)) key)))
	  (when (vectorp raw)
	    (deserialize raw))))
    (serialization-error (condition)
      (declare (ignore condition))
      (format t "Cannot lookup ~A/~A/~A~%" s p o)
      nil)))

(defmethod lookup-triple (s p o &key g)
  (or (gethash (list s p o) (triple-cache (or g *graph*)))
      (lookup-triple-in-db s p o (or g *graph*))))

(defmethod lookup-triple-by-id (uuid &key g)
  (or (gethash (if (uuid:uuid? uuid) (format nil "~A" uuid) uuid)
	       (triple-cache (or g *graph*)))
      (handler-case
	  (let ((raw-key (lookup-object (triple-db (or g *graph*)) 
					(make-triple-uuid-key 
					 (if (uuid:uuid? uuid) 
					     uuid 
					     (uuid:make-uuid-from-string uuid))))))
	    (when (vectorp raw-key)
	      (let ((raw (lookup-object (triple-db (or g *graph*)) raw-key)))
		(deserialize raw))))
	(serialization-error (condition)
	  (format t "Cannot lookup ~A: ~A~%" uuid condition)
	  nil))))

(defmethod save-triple ((triple triple))
  (let ((key (make-serialized-key triple))
	(value (serialize triple)))
    (store-object (triple-db *graph*) key value :mode :keep)))

(defmethod cache-triple ((triple triple))
  (setf (gethash (format nil "~A" (triple-uuid triple)) (triple-cache *graph*)) triple)
  (setf (gethash (list (node-value (triple-subject triple))
		       (pred-name (triple-predicate triple))
		       (node-value (triple-object triple)))
		 (triple-cache *graph*))
	triple)
  (setf (gethash (list (triple-subject triple) 
		       (triple-predicate triple) 
		       (triple-object triple))
		 (triple-cache *graph*))
	triple))

(defmethod make-new-triple ((graph graph) (subject node) (predicate predicate) (object node) 
			    &key (index-immediate? t) (certainty-factor +cf-true+) derived?)
  (or (lookup-triple subject predicate object)
      (let ((triple (make-triple :subject subject :predicate predicate :object object 
				 :belief-factor certainty-factor :derived? derived?)))
	(handler-case
	    (with-transaction ((triple-db graph))
	      (save-triple triple)
	      (unless (gethash (make-functor (pred-name predicate) 2) (functors graph))
		(add-default-functor (make-new-predicate :name (pred-name predicate))))
	      (incf-ref-count subject)
	      (incf-ref-count object)
	      (if index-immediate? 
		  (index-triple triple)
		  (sb-concurrency:enqueue triple (needs-indexing-q graph))))
	  (persistence-error (condition)
	    (or (lookup-triple subject predicate object)
		(error condition)))
	  (:no-error (status)
	    (declare (ignore status))
	    (cache-triple triple))))))

(defmethod bulk-add-triples ((graph graph) tuple-list &key cache?)
  (let ((new-triples nil))
    (handler-case
	(with-transaction ((triple-db graph))
	  (dolist (tuple tuple-list)
	    (let ((s (make-new-node :value (elt tuple 0)))
		  (p (make-new-predicate :name (elt tuple 1)))
		  (o (make-new-node :value (elt tuple 2)))
		  (b (or (belief-factor tuple) +cf-true+)))
	      (or (lookup-triple-in-db (node-value s) (pred-name p) (node-value o) graph)
		  (let ((triple (make-triple :subject s :predicate p :object o :belief-factor b)))
		    (save-triple triple)
		    (incf-ref-count s)
		    (incf-ref-count o)
		    (index-triple triple)
		    (when cache? (push triple new-triples)))))))
      (:no-error (success?)
	(when (and success? cache?)
	  (dolist (triple new-triples)
	    (cache-triple triple))
	  t)))))
  
(defmethod delete-triple ((triple triple))
  (cas (triple-deleted? triple) nil t))

(defmethod erase-triple ((triple triple))
  (handler-case
      (with-transaction ((triple-db *graph*))
	(delete-object (triple-db *graph*) (make-serialized-key triple))
	(deindex-triple triple)
	(decf-ref-count (triple-subject triple))
	(decf-ref-count (triple-object triple)))
    (persistence-error (condition)
      (format t "Cannot erase triple ~A: ~A~%" triple condition))
    (:no-error (status)
      (declare (ignore status))
      (setf (gethash (format nil "~A" (triple-uuid triple)) (triple-cache *graph*)) triple)
      (remhash (list (triple-subject triple) (triple-predicate triple) (triple-object triple))
	       (triple-cache *graph*))
      (remhash (list (node-value (triple-subject triple)) 
		     (pred-name (triple-predicate triple)) 
		     (node-value (triple-object triple)))
	       (triple-cache *graph*))
      t)))

(defmethod index-triple ((triple triple))
  (let ((subject-key (make-triple-index-key (node-value (triple-subject triple)) 
					    +triple-subject+))
	(predicate-key (make-triple-index-key (pred-name (triple-predicate triple)) 
					      +triple-predicate+))
	(object-key (make-triple-index-key (node-value (triple-object triple)) 
					   +triple-object+))
	(sp-key (make-triple-sp-key triple))
	(so-key (make-triple-so-key triple))
	(po-key (make-triple-po-key triple))
	(uuid-key (make-triple-uuid-key (triple-uuid triple)))
	(triple-key (make-serialized-key triple)))
    (with-transaction ((triple-db *graph*))
      (store-object (triple-db *graph*) uuid-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) sp-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) so-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) po-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) subject-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) predicate-key triple-key :mode :duplicate)
      (store-object (triple-db *graph*) object-key triple-key :mode :duplicate))
    (index-text triple)))

(defmethod do-indexing ((graph graph))
  (loop until (sb-concurrency:queue-empty-p (needs-indexing-q graph)) do
       (index-triple (sb-concurrency:dequeue (needs-indexing-q graph)))))

(defmethod deindex-triple ((triple triple))
  (let ((subject-key (make-triple-index-key (node-value (triple-subject triple)) 
					    +triple-subject+))
	(predicate-key (make-triple-index-key (pred-name (triple-predicate triple)) 
					      +triple-predicate+))
	(object-key (make-triple-index-key (node-value (triple-object triple)) 
					   +triple-object+))
	(sp-key (make-triple-sp-key triple))
	(so-key (make-triple-so-key triple))
	(po-key (make-triple-po-key triple))
	(uuid-key (make-triple-uuid-key (triple-uuid triple)))
	(triple-key (make-serialized-key triple)))
    (deindex-text triple)
    (with-transaction ((triple-db *graph*))
      (delete-object (triple-db *graph*) uuid-key triple-key)
      (delete-object (triple-db *graph*) sp-key triple-key)
      (delete-object (triple-db *graph*) so-key triple-key)
      (delete-object (triple-db *graph*) po-key triple-key)
      (delete-object (triple-db *graph*) subject-key triple-key)
      (delete-object (triple-db *graph*) predicate-key triple-key)
      (delete-object (triple-db *graph*) object-key triple-key))))

(defmethod index-text ((triple triple))
  (when (or (stringp (node-value (triple-subject triple)))
	    (stringp (node-value (triple-object triple))))
    (let ((doc (make-instance 'montezuma:document)))
      (montezuma:add-field 
       doc (montezuma:make-field "uuid" 
				 (format nil "~A" (triple-uuid triple))
				 :stored t :index :untokenized))
      (when (stringp (node-value (triple-subject triple)))
	(montezuma:add-field 
	 doc (montezuma:make-field "subject" (node-value (triple-subject triple)) 
				   :stored t :index :tokenized)))
      (when (stringp (node-value (triple-object triple)))
	(montezuma:add-field 
	 doc (montezuma:make-field "object" (node-value (triple-object triple))
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




(defun triple-test-1 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (time
	  (with-transaction ((triple-db *graph*))
	    (dotimes (i 10000)
	      (add-triple (format nil "S~A" i) (format nil "P~A" i) (format nil "O~A" i)))))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(delete-file "/var/tmp/triples")
	(delete-file "/var/tmp/rules")
	(delete-file "/var/tmp/functors")
	(delete-file "/var/tmp/config.ini")))))

(defun triple-test-2 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (let ((tuples nil))
	   (dotimes (i 10000)
	     (push (list (format nil "S~A" i) (format nil "P~A" i) (format nil "O~A" i)) tuples))
	   (time (bulk-add-triples *graph* tuples)))
      (progn
	(if (graph? *graph*) (shutdown-graph *graph*))
	(delete-file "/var/tmp/triples")
	(delete-file "/var/tmp/rules")
	(delete-file "/var/tmp/functors")
	(delete-file "/var/tmp/config.ini")))))

(defun triple-test-3 ()
  (let ((*graph* (make-new-graph :name "test graph" :location "/var/tmp")))
    (unwind-protect
	 (progn
	   (add-triple "Kevin" "loves" "Dustie")
	   (add-triple "Kevin" "loves" "Echo")
	   (add-triple "Dustie" "loves" "Kevin")
	   (add-triple "Echo" "loves" "cat nip")
	   (add-triple "Echo" "is-a" "cat")
	   (add-triple "Kevin" "is-a" "Homo Sapien")
	   (add-triple "Dustie" "is-a" "Homo Sapien")
	   ;;(format t "NODES ~A:~%~A~%" (nodes *graph*) (skip-list-to-list (nodes *graph*)))
	   (format t "Who loves whom? -> ~A~%" (get-triples :p "loves"))
	   (format t "What species? -> ~A~%" (get-triples :p "is-a"))
	   (format t "~A~%" (get-subjects "Kevin")))
      (progn
	(shutdown-graph *graph*)
	(delete-file "/var/tmp/triples")
	(delete-file "/var/tmp/rules")
	(delete-file "/var/tmp/functors")
	(delete-file "/var/tmp/config.ini")))))

