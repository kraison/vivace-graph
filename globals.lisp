(in-package #:vivace-graph)

;; Graphs
(defvar *graph* nil)
(defvar *graph-table* nil)

;; Logging
(defvar *syslog-program* "vivace-graph")
(defvar *syslog-facility* sb-posix:log-local7)
(progn
  (defparameter *syslog-priorities* (make-hash-table))
  (setf (gethash :emerg *syslog-priorities*) sb-posix:log-emerg)
  (setf (gethash :alert *syslog-priorities*) sb-posix:log-alert)
  (setf (gethash :crit *syslog-priorities*) sb-posix:log-crit)
  (setf (gethash :err *syslog-priorities*) sb-posix:log-err)
  (setf (gethash :warning *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :warn *syslog-priorities*) sb-posix:log-warning)
  (setf (gethash :notice *syslog-priorities*) sb-posix:log-notice)
  (setf (gethash :info *syslog-priorities*) sb-posix:log-info)
  (setf (gethash :debug *syslog-priorities*) sb-posix:log-debug))

;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *predicate* nil "The Prolog predicate currently being compiled.")
(defvar *select-list* "Accumulator for prolog selects.")
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
