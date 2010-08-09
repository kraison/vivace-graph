(in-package #:vivace-graph)

#|
(deftemplate person
  (slot name)
  (slot age)
  (slot eye-color)
  (slot hair-color)) 
|#
(defmacro deftemplate (name &rest slots)
  )

#|
(fact (person (name “John Q. Public”)
	      (age 23)
	      (eye-color blue)
	      (hair-color black)))
|#
(defmacro fact (template)
  )

#|
(deffacts
    (person (name “John Q. Public”) (age 23)
	    (eye-color blue) (hair-color black))
    (person (name “Jane S. Public”) (age 24)
	    (eye-color blue) (hair-color blond)))
|#
(defmacro deffacts (&rest templates)
  )

