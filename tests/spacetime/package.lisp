;;;; Test package for graph-db/spacetime.
;;;;
;;;; Safe to :USE GRAPH-DB.SPACETIME -- unlike GRAPH-DB, whose test package
;;;; curates an explicit import list, this package is small and ours.

(in-package #:cl-user)

(defpackage #:graph-db/spacetime-test
  (:use #:cl #:fiveam #:graph-db.spacetime)
  (:import-from #:local-time
                #:encode-timestamp #:timestamp< #:timestamp= #:timestamp+
                #:timestamp- #:+utc-zone+ #:*default-timezone*)
  (:export #:run-spacetime-tests #:spacetime-suite))
