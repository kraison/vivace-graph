(in-package #:vivace-graph)

(defun add-triple (subject predicate object &optional graph)
  (let ((*graph* (or graph *graph*)))
    (let ((subject (or (lookup-node subject *graph*) (make-new-node :value subject)))
	  (predicate (or (lookup-predicate predicate *graph*) (make-new-predicate :name predicate)))
	  (object (or (lookup-node object *graph*) (make-new-node :value object))))
      (make-new-triple *graph* subject predicate object))))

;(defun get-triples-list (&key limit graph)
;  (let ((*graph* (or graph *graph*)))
;    (let ((result nil) (count 0))
;      (map-skip-list-values #'(lambda (triple) (push triple result)) (triples *graph*))
;      (nreverse result))))

(defun get-triples (&key s p o g)
  (let ((*graph* (or g *graph*)) (triples nil))
    (if (and s p o)
	(lookup-triple s p o :g *graph*)
	(progn
	  (if s (setq triples (get-subjects s)))
	  (if p 
	      (if s
		  (setq triples (intersection triples (get-predicates p)))
		  (setq triples (get-predicates p))))
	  (if o 
	      (if (or p s)
		  (setq triples (intersection triples (get-objects o)))
		  (setq triples (get-objects o))))
	  triples))))

;(defun triple-count (&optional graph)
;  (skip-list-length (triples (or graph *graph*))))
