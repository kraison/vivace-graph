(in-package #:graph-db)

(defstruct (clause-set (:constructor %make-clause-set))
  "A functor's compiled clauses, published whole and never mutated
(clause indexing design D5, GH #121): FNS the clause closures in source
order; INDEX first-argument key -> ascending positions; VARS the
ascending positions the index does not cover."
  (fns #() :type simple-vector)
  (index (make-hash-table :test 'equal) :type hash-table)
  (vars #() :type simple-vector))

(defstruct (functor
             (:constructor %make-functor)
             (:predicate functor-p))
  name fn clauses (lock (make-recursive-lock))
  ;; GH #121: ARITY from the first clause; DISPATCHER the function
  ;; built once over the arity; CLAUSE-SET the current published set.
  arity dispatcher (clause-set (%make-clause-set))
  ;; GH #122: TABLED set by TABLE-FUNCTOR; ENTRY the tabled entry
  ;; closure FN holds while TABLED (prolog-tabling.lisp).
  tabled entry)

(defgeneric prolog-compile (functor))

;;; On ECL 21.2.1, hash tables have no built-in thread safety.  Every
;;; access to *user-functors* is serialised through this lock.
#+ecl (defvar *user-functors-lock* (make-lock "user-functors"))

(defmacro with-user-functors-lock (&body body)
  #+ecl `(with-lock-held (*user-functors-lock*) ,@body)
  #-ecl `(progn ,@body))

(defun lookup-functor (name)
  (with-user-functors-lock (gethash name *user-functors*)))

(defun make-functor (&key name clauses)
  (or (lookup-functor name)
      (let ((functor (%make-functor :name name :clauses clauses)))
        (with-recursive-lock-held ((functor-lock functor))
          (prog1
              (with-user-functors-lock
                (setf (gethash name *user-functors*) functor))
            (prolog-compile functor))))))

(defun add-functor-clause (functor clause)
  "CLAUSE compiled on its own and appended (GH #121); the clause list."
  (with-recursive-lock-held ((functor-lock functor))
    ;; The lock serializes all writers; no CAS needed.
    (%add-compiled-clause functor clause))
  (functor-clauses functor))

(defun delete-functor (functor)
  (with-user-functors-lock (remhash (functor-name functor) *user-functors*)))

(defun reset-functor (functor)
  "No clauses, no candidates, the null function (GH #121)."
  (with-recursive-lock-held ((functor-lock functor))
    (setf (functor-clauses functor) nil)
    (prolog-compile functor))
  nil)

(defun get-functor-fn (functor-symbol)
  (let ((f (lookup-functor functor-symbol)))
    (when (functor-p f)
      (functor-fn f))))

(defun set-functor-fn (functor-symbol fn)
  (let ((f (lookup-functor functor-symbol)))
    (when *prolog-trace*
      (format t "TRACE: set-functor-fn for ~A got ~A~%" functor-symbol f))
    (if (functor-p f)
        (setf (functor-fn f) fn)
        (error 'prolog-error
               :reason (format nil "unknown functor ~A" functor-symbol)))))
