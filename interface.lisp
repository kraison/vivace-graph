(in-package #:vivace-graph)

(defun add-triple (subject predicate object &optional graph)
  (let ((*graph* (or graph *graph*)))
    (let ((subject (or (lookup-node subject) 
		       (make-new-node :value subject)))
	  (predicate (make-new-predicate :name predicate))
	  (object (or (lookup-node object) 
		      (make-new-node :value object))))
      (make-new-triple *graph* subject predicate object))))

(defun get-triples (&key s p o g (decode? t))
  (let ((*graph* (or g *graph*)) (klist nil))
    (cond ((and s p o)
	   (let ((triple (lookup-triple s p o :g *graph*)))
	     (when triple (return-from get-triples triple))))
	  ((and s p)
	   (setq klist (get-subjects-predicates s p)))
	  ((and s o)
	   (setq klist (get-subjects-objects s o)))
	  ((and p o)
	   (setq klist (get-predicates-objects p o)))
	  (s
	   (setq klist (get-subjects s)))
	  (p
	   (setq klist (get-predicates p)))
	  (o
	   (setq klist (get-objects o))))
    (if (and decode? (klist? klist))
	(unwind-protect
	     (map-klist #'(lambda (i) (lookup-triple-by-id i)) klist :collect? t)
	  (klist-free klist))
	klist)))

(defun load-graph! (file)
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list file))
    (let ((uuid (uuid:make-uuid-from-string (py-configparser:get-option config "default" "uuid"))))
      (sb-ext:with-locked-hash-table (*graph-table*)
	(let ((graph (gethash uuid *graph-table*)))
	  (if (and (graph? graph) (not (shutdown? graph)))
	      (setq *graph* graph)
	      (let ((graph (make-graph :graph-uuid uuid
				       :graph-name (py-configparser:get-option 
						    config "default" "name")
				       :graph-location (py-configparser:get-option 
							config "default" "location"))))
		(setf (triple-db graph) 
		      (open-btree (format nil "~A/triples.kct" (graph-location graph))
				  :duplicates-allowed? t)
		      (deleted-triple-db graph) 
		      (open-btree (format nil "~A/deleted-triples.kct" (graph-location graph))
				  :duplicates-allowed? t)
		      (rule-db graph) 
		      (open-phash (format nil "~A/rules.kch" (graph-location graph)))
		      (functor-db graph) 
		      (open-phash (format nil "~A/functors.kch" (graph-location graph)))
		      (full-text-idx graph)
		      (make-instance 'montezuma:index 
				     :default-field "*"
				     :fields '("subject" "object")
				     :min-merge-docs 5000 
				     :path (format nil "~A/full-text-idx" (graph-location graph)))
		      (gethash (graph-uuid graph) *graph-table*) graph
		      *graph* graph))))))
    (load-all-functors *graph*)
    (load-all-rules *graph*)
    *graph*))

(defun make-new-graph (&key name location)
  (let ((graph (make-graph))
	(config-file (format nil "~A/config.ini" location)))
    (with-open-file (stream config-file
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :error)
      (format stream "[default]~%")
      (format stream "uuid=~A~%" (print-object (graph-uuid graph) nil))
      (format stream "name=~A~%" name)
      (format stream "location=~A~%" location))
    (load-graph! config-file)))

(defmethod shutdown-graph ((graph graph))
  (sb-ext:with-locked-hash-table (*graph-table*)
    (setf (shutdown? graph) t)
    (when (eql *graph* graph) (setq *graph* nil))
    (close-phash (functor-db graph))
    (close-phash (rule-db graph))
    (close-btree (triple-db graph))
    (close-btree (deleted-triple-db graph))
    (remhash (graph-uuid graph) *graph-table*)))
