(in-package #:vivace-graph)

(defun add-triple (subject predicate object &optional graph)
  (let ((*graph* (or graph *graph*)))
    (let ((subject (or (lookup-node subject *graph*) (make-new-node :value subject)))
	  (predicate (or (lookup-node predicate *graph*) (make-new-node :value predicate)))
	  (object (or (lookup-node object *graph*) (make-new-node :value object))))
      (make-new-triple *graph* subject predicate object))))

(defun get-triples-list (&key limit graph)
  (let ((*graph* (or graph *graph*)))
    (let ((result nil) (count 0))
      (map-skip-list-values #'(lambda (triple) (push triple result)) (triples *graph*))
      (nreverse result))))

(defun get-triples (&key s p o g)
  (let ((*graph* (or g *graph*)) (nodes nil))
    (if (and s p o)
	(lookup-triple s p o :g *graph*)
	(progn
	  (if s (setq nodes (get-subjects s)))
	  (if p 
	      (if s
		  (setq nodes (intersection nodes (get-predicates p)))
		  (setq nodes (get-predicates p))))
	  (if o 
	      (if (or p s)
		  (setq nodes (intersection nodes (get-objects o)))
		  (setq nodes (get-objects o))))
	  nodes))))

(defun triple-count (&optional graph)
  (skip-list-length (triples (or graph *graph*))))
