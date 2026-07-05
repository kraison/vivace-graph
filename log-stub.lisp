;;;; No-op replacement for log4cl's `log:' macros, used ONLY when the feature
;;;; :GRAPH-DB-STUB-LOG is present (the embeddable Android core build).  log4cl
;;;; is a heavy dependency whose compile-time machinery (make-logger /
;;;; naming-option) corrupts the live image during ECL cross-compilation, and a
;;;; field device has no use for its file/console logging.  Desktop/SBCL builds
;;;; do NOT set the feature and keep the real log4cl.
;;;;
;;;; graph-db's sources reference log:debug/info/warn/error/trace and log:config.
;;;; We provide a `LOG' package (no CL use, so DEBUG/ERROR/WARN don't collide
;;;; with the CL symbols) whose names are macros expanding to NIL.  The macro
;;;; bodies are written here in CL-USER with fully-qualified LOG: names, because
;;;; the LOG package itself can't see CL:DEFMACRO.

(in-package :cl-user)

(defpackage :log
  (:use)
  (:export #:debug #:info #:warn #:error #:trace #:fatal #:config #:expr #:sexp))

(defmacro log:debug  (&rest args) (declare (ignore args)) nil)
(defmacro log:info   (&rest args) (declare (ignore args)) nil)
(defmacro log:warn   (&rest args) (declare (ignore args)) nil)
(defmacro log:error  (&rest args) (declare (ignore args)) nil)
(defmacro log:trace  (&rest args) (declare (ignore args)) nil)
(defmacro log:fatal  (&rest args) (declare (ignore args)) nil)
(defmacro log:expr   (&rest args) (declare (ignore args)) nil)
(defmacro log:sexp   (&rest args) (declare (ignore args)) nil)
;; log:config is called for side effects (e.g. (log:config :error)); no-op it.
(defmacro log:config (&rest args) (declare (ignore args)) nil)
