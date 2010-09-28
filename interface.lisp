(in-package #:vivace-graph)

(defun add-triple (subject predicate object &optional graph)
  "Add a triple to the datastore.  Subject, predicate and object can be nodes or atomic data types."
  (let ((*graph* (or graph *graph*)))
    (let ((predicate (make-new-predicate :name predicate)))
;           (subject (or (lookup-node subject) 
;		       (make-new-node :value subject)))   
;	  (object (or (lookup-node object) 
;		      (make-new-node :value object))))
      (make-new-triple *graph* subject predicate object))))

(defun get-triples (&key s p o g (decode? t))
  "Lookup triples by subject, predicate, and / or object. Currently returns one of two types of 
results: either a klist of undecoded triples or a list of triples. If a single triple is found,
a klist cannot be returned for reasons too complex to discuss here.  In this scenario, a list of
one triple is returned no matter whether or not decode? was specified.  This inconsistency should 
be eliminated at some point."
  (let ((*graph* (or g *graph*)) (klist nil))
    (cond ((and s p o)
	   (let ((triple (lookup-triple s p o :g *graph*)))
	     (when triple (return-from get-triples (list triple)))))
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

(defun make-graph-worker-thread (graph)
  (make-thread #'(lambda ()
		   (let ((*graph* graph))
		     (loop until (shutdown? *graph*) do
			  (vivace-gc *graph*)
			  (when (needs-indexing? *graph*) (do-indexing *graph*))
			  (sleep 0.1))))
	       :name (format nil "~A worker" graph)))

(defun load-graph! (file)
  "Load a graph from configuration file (file).  Sets *graph* to the newly opened graph."
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list file))
    (let ((name (py-configparser:get-option config "default" "name")))
      (sb-ext:with-locked-hash-table (*graph-table*)
	(let ((graph (gethash name *graph-table*)))
	  (if (and (graph? graph) (not (shutdown? graph)))
	      (setq *graph* graph)
	      (let ((graph (make-graph :graph-uuid (uuid:make-uuid-from-string
						    (py-configparser:get-option
						     config "default" "uuid"))
				       :graph-name name
				       :graph-location (py-configparser:get-option 
							config "default" "location"))))
		(format t "Created ~A~%" graph)
		(setf (triple-db graph) 
		      (open-btree (format nil "~A/triples.kct" (graph-location graph))
				  :duplicates-allowed? t)

		      (deleted-triple-db graph) 
		      (open-btree (format nil "~A/deleted-triples.kct" (graph-location graph))
				  :duplicates-allowed? t)

		      (full-text-idx graph) 
		      (open-btree (format nil "~A/full-text-idx.kct" (graph-location graph))
				  :duplicates-allowed? t)

		      (rule-db graph) 
		      (open-phash (format nil "~A/rules.kch" (graph-location graph)))

		      (functor-db graph) 
		      (open-phash (format nil "~A/functors.kch" (graph-location graph)))

		      (gethash (graph-name graph) *graph-table*) graph

		      (worker-thread graph) (make-graph-worker-thread graph)

		      *graph* graph))))))
    (load-all-functors *graph*)
    (load-all-rules *graph*)
    *graph*))

(defun make-new-graph (&key name location)
  "Create a new graph in the directory LOCATION with name NAME.  Sets *graph* to the newly created
graph."
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
  "Close the given GRAPH."
  (sb-ext:with-locked-hash-table (*graph-table*)
    (setf (shutdown? graph) t)
    (when (threadp (worker-thread *graph*)) 
      (format t "Waiting on worker thread ~A~%" (worker-thread *graph*))
      (join-thread (worker-thread *graph*))
      (setf (worker-thread *graph*) nil)
      (format t "worker thread ~A finished.~%" (worker-thread *graph*)))
    (when (eql *graph* graph) (setq *graph* nil))
    (close-phash (functor-db graph))
    (close-phash (rule-db graph))
    (close-btree (full-text-idx graph))
    (close-btree (triple-db graph))
    (close-btree (deleted-triple-db graph))
    (setf (triple-db graph) nil
	  (deleted-triple-db graph) nil
	  (full-text-idx graph) nil
	  (rule-db graph) nil
	  (functor-db graph) nil
	  (rule-idx graph) nil
	  (templates graph) nil
	  (functors graph) nil
	  (rule-cache graph) nil
	  (predicate-cache graph) nil
	  (triple-cache graph) nil
	  (deleted-triple-cache graph) nil
	  (production-pq graph) nil
	  (delete-queue graph) nil
	  (needs-indexing-q graph) nil)
    (sb-ext:gc :full t)
    (remhash (graph-name graph) *graph-table*)))
