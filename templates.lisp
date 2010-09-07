(in-package #:vivace-graph)

(defmacro deftemplate (name &rest slots)
  "Define a template:
 (deftemplate person
   (slot has-name)
   (slot has-age)
   (slot has-eye-color)
   (slot has-hair-color)) 
A function is added to the template table of *graph* with name NAME.  This function will be used to
create groups of triples conforming to this template. See FACT and DEFFACTS."
  (unless (graph? *graph*)
    (error "deftemplate ~A: *graph* is not bound to a proper graph!" name))
  (let ((node (gensym)))
    (setf (gethash name (templates *graph*))
	  (eval
	   `#'(lambda (&key ,@(mapcar #'second slots))
		(with-transaction ((triple-db *graph*))
		  (let ((,node (make-anonymous-node)))
		    (add-triple ,node 'is-a (string-downcase (symbol-name ',name)))
		    ,@(mapcar #'(lambda (slot)
				  `(add-triple ,node ',(second slot) ,(second slot)))
			      slots)
		    ,node)))))))

(defmacro fact (template)
  "Create a group of triples using the named template as defined in DEFTEMPLATE:
 (fact (person (has-name “John Q. Public”)
 	      (has-age 23)
 	      (has-eye-color blue)
 	      (has-hair-color black)))" 
  (let ((tmpl-name (first template)))
    `(funcall (gethash ',tmpl-name (templates *graph*))
	      ,@(flatten (mapcar #'(lambda (slot)
				     `(,(intern (symbol-name (first slot)) 'keyword)
					,(second slot)))
				 (rest template))))))
    
(defmacro deffacts (&rest templates)
  "Create a set of triple groups conforming to the named template as defined by DEFTEMPLATE:
 (deffacts
     (person (has-name “John Q. Public”) (has-age 23) 
 	    (has-eye-color blue) (has-hair-color black))
     (person (has-name “Jane S. Public”) (has-age 24)
  	    (has-eye-color blue) (has-hair-color blond)))"
  (let ((template (gensym)))
    `(mapcar #'(lambda (,template)
		 (let ((tmpl-name (first ,template)))
		   (format t "tmpl-name is ~A~%" tmpl-name)
		   (apply (gethash tmpl-name (templates *graph*))
			  (flatten
			   (mapcar #'(lambda (slot)
				       (list (intern (symbol-name (first slot)) 'keyword)
					     (second slot)))
				   (rest ,template))))))
	     ',templates)))
