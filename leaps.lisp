(in-package #:vivace-graph)

#|
(defmethod compile-rule ((rule rule))
  rule)

(defmethod apply-rule ((rule rule) (triple triple))
  (dolist (premise (rule-premises rule))
    (if (prolog-equal (predicate triple) (first premise))
	(format t "matched predicates of ~A and ~A~%" triple premise))
    (if (prolog-equal (subject triple) (second premise))
	(format t "matched subjects of ~A and ~A~%" triple premise))
    (if (prolog-equal (object triple) (third premise))
	(format t "matched objects of ~A and ~A~%" triple premise))))
|#
#|
(defrule t1
  if 
  (or (is-a ?x "dog") (is-a ?x "human"))
  (or (likes ?x "cats") (likes ?x "lizards"))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t2
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats"))
   (and (is-a ?x "dog") (likes ?x "lizards"))
   (and (is-a ?x "human") (likes ?x "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))

(defrule t3
  if 
  (or
   (and (is-a ?x "dog") (likes ?x "cats")
	(is-a ?y "dog") (likes ?y "cats"))
   (and (is-a ?x "human") (likes ?x "lizards")
	(is-a ?y "human") (likes ?y "lizards")))
  then
  (trigger (format t "~A is a strange beast!~%" ?x)))
|#