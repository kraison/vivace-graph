;;;; Tabled predicates (GH #122): the declaration, and the per-query
;;;; variant tables a tabled call is answered from.  Design:
;;;; docs/superpowers/specs/2026-11-10-tabling-design.md (D1 to D8).

(in-package #:graph-db)

(defun %make-tabled-entry (functor)
  "The FN a tabled FUNCTOR holds: a function of the call's arguments
and the continuation.  Reads the dispatcher at call time, so clauses
asserted after the declaration are seen (design D1)."
  (lambda (&rest args)
    (let ((dispatcher (functor-dispatcher functor)))
      (when dispatcher
        (apply dispatcher args)))))

(defun table-functor (name arity)
  "Declare the user functor NAME/ARITY tabled, registering it first if
no clause has; returns the functor.  NAME is interned in the run-time
*PACKAGE*, as <- interns a clause head.  A built-in functor is refused
with PROLOG-ERROR (design D1)."
  (let ((global (make-functor-symbol name arity)))
    (when (nth-value 1 (gethash global *prolog-global-functors*))
      (error 'prolog-error
             :reason (format nil "~A is a built-in functor; only a user ~
functor can be tabled" global))))
  (let ((functor (make-functor :name (make-functor-symbol name arity
                                                          :define t)
                               :clauses nil)))
    (with-recursive-lock-held ((functor-lock functor))
      (setf (functor-tabled functor) t)
      (%install-functor-fn functor))
    functor))

(defun untable-functor (name arity)
  "Undo TABLE-FUNCTOR: NAME/ARITY answers from its clauses again.
Returns the functor, or NIL when none is registered."
  (let ((functor (lookup-functor (make-functor-symbol name arity))))
    (when functor
      (with-recursive-lock-held ((functor-lock functor))
        (setf (functor-tabled functor) nil
              (functor-entry functor) nil)
        (%install-functor-fn functor))
      functor)))

(defun functor-tabled-p (name arity)
  "True when the registered user functor NAME/ARITY is tabled."
  (let ((functor (lookup-functor (make-functor-symbol name arity))))
    (and functor (functor-tabled functor) t)))
