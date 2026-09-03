;;;; query/package.lisp -- the web-free guarded query subsystem (GH #322).

(defpackage #:graph-db.query
  (:use #:cl)
  (:export
   ;; conditions (spec SS3)
   #:prolog-guard-error #:prolog-guard-error-reason
   #:prolog-ill-typed-error #:prolog-server-fault
   ;; the screen's limits
   #:*prolog-max-query-length* #:*prolog-max-depth*
   ;; schema names, shared with the GUI
   #:schema-type-names))
