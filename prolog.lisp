(in-package #:vivace-graph)

(defparameter fail nil)

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defun add-clause (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  (add-triple (second clause) (first clause) (third clause) *graph*))

(defmacro <- (clause)
  "add a clause to the triple store. Order of args: predicate, subject, object."
  `(add-clause ',(replace-?-vars clause)))

(defun top-level-prove (goals)
  ;; Implement!
  nil)

(defmacro ?- (goals)
  `(top-level-prove ',(replace-?-vars goals)))