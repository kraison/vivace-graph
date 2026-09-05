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
   #:schema-type-names
   ;; the runner (spec SS4, GH #322)
   #:run-guarded-prolog
   ;; the compile half, for a caller that keeps the goals (GH #331)
   #:guard-query-text))
