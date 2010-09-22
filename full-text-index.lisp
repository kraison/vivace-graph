(in-package #:vivace-graph)

(defun tokenize-string (seq)
  (remove-if #'(lambda (token)
		 (< (length token) 3))
	     (tokenize seq
		       :escapes #\\
		       :multi-escapes "\"|"
		       :delimiters (format nil "_,:~A" #\Tab)
		       :terminators ""
		       :punctuation "[]()!.?~`;'<>/+=-*&^%$#@"
		       :whitespace :whitespace
		       :defaults (let ((i 0))
				   (lambda () (incf i))))))

(defmethod index-text ((triple triple))
  (with-transaction ((full-text-idx *graph*))
    (when (stringp (triple-subject triple)) 
      (dolist (token (remove-duplicates (tokenize-string (triple-subject triple)) :test 'equalp))
	(set-btree (full-text-idx *graph*) 
		   (make-slot-key "s" (string-downcase token)) 
		   (triple-uuid triple) :mode :concat)))
    (when (stringp (triple-object triple))
      (dolist (token (remove-duplicates (tokenize-string (triple-object triple)) :test 'equalp))
	(set-btree (full-text-idx *graph*) 
		   (make-slot-key "o" (string-downcase token)) 
		   (triple-uuid triple) :mode :concat)))))

(defmethod deindex-text ((triple triple))
  (with-transaction ((full-text-idx *graph*))
    (when (stringp (triple-subject triple)) 
      (dolist (token (remove-duplicates (tokenize-string (triple-subject triple)) :test 'equalp))
	(rem-btree (full-text-idx *graph*) 
		   (make-slot-key "s" (string-downcase token)) 
		   (triple-uuid triple))))
    (when (stringp (triple-object triple))
      (dolist (token (remove-duplicates (tokenize-string (triple-object triple)) :test 'equalp))
	(rem-btree (full-text-idx *graph*) 
		   (make-slot-key "o" (string-downcase token)) 
		   (triple-uuid triple))))))

(defun full-text-search (string &key subject? object?)
  (let ((result nil))
    (dolist (token (remove-duplicates (tokenize-string string) :test 'equalp))
      (when subject?
	(let ((klist (get-btree (full-text-idx *graph*) 
				(make-slot-key "s" (string-downcase token)) :mode :klist)))
	  (when (klist? klist)
	    (unwind-protect
		 (map-klist #'(lambda (id) 
				(format t "GOT ~A~%" (lookup-triple-by-id id))
				(pushnew (lookup-triple-by-id id) result)) klist)
	      (klist-free klist)))))
      (when object?
	(let ((klist (get-btree (full-text-idx *graph*) 
				(make-slot-key "o" (string-downcase token)) :mode :klist)))
	  (when (klist? klist)
	    (unwind-protect
		 (map-klist #'(lambda (id) 
				(format t "GOT ~A~%" (lookup-triple-by-id id))
				(pushnew (lookup-triple-by-id id) result)) klist)
	      (klist-free klist))))))
    result))





#|
(defmethod index-text ((triple triple))
  (when (or (stringp (triple-subject triple))
	    (stringp (triple-object triple)))
    (let ((doc (make-instance 'montezuma:document)))
      (montezuma:add-field 
       doc (montezuma:make-field "uuid" 
				 (format nil "~A" (triple-uuid triple))
				 :stored t :index :untokenized))
      (when (stringp (triple-subject triple))
	(montezuma:add-field 
	 doc (montezuma:make-field "subject" 
				   (triple-subject triple)
				   :stored t :index :tokenized)))
      (when (stringp (triple-object triple))
	(montezuma:add-field 
	 doc (montezuma:make-field "object" 
				   (triple-object triple)
				   :stored t :index :tokenized)))
      (with-recursive-lock-held ((full-text-lock *graph*))
	(montezuma:add-document-to-index (full-text-idx *graph*) doc)))))

(defmethod deindex-text ((triple triple)) 
  (let ((docs nil))
    (with-recursive-lock-held ((full-text-lock *graph*))
      (montezuma:search-each 
       (full-text-idx *graph*)
       (uuid:print-bytes nil (triple-uuid triple))
       #'(lambda (doc score) (declare (ignore score)) (push doc docs)))
      (dolist (doc docs)
	(montezuma:delete-document (full-text-idx *graph*) doc)))))

(defun map-text-search (string fn &key collect)
  (let ((result nil))
    (with-recursive-lock-held ((full-text-lock *graph*))
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
      (nreverse result))))
|#
