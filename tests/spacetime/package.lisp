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
  ;; SERIALIZE/DESERIALIZE are not part of GRAPH-DB's export list (tests/
  ;; package.lisp imports them the same way) -- needed to round-trip
  ;; EXTENT->SEXP through the real engine function (#130).
  (:import-from #:graph-db #:serialize #:deserialize)
  ;; MAKE-GRAPH/CLOSE-GRAPH/WITH-TRANSACTION for claim-tests.lisp's on-disk
  ;; graph fixture (#131); OPEN-GRAPH/ID for its close-then-reopen regression
  ;; test.  *GRAPH* and *SCHEMA-NODE-METADATA* stay package-qualified at the
  ;; call sites instead, matching GRAPH-DB/GEOS-TEST and GRAPH-DB/ALGORITHMS-
  ;; TEST's import lists (tests/geos/package.lisp, tests/algorithms/package.
  ;; lisp).
  (:import-from #:graph-db #:make-graph #:close-graph #:with-transaction
                #:open-graph #:id #:lookup-vertex)
  (:export #:run-spacetime-tests #:spacetime-suite))
