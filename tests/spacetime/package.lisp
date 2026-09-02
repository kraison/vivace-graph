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
  ;; DEF-VERTEX and the geometry constructors for register-tests.lisp's
  ;; region fixture (#138); GEOMETRY is the slot type symbol, which must be
  ;; GRAPH-DB's rather than a same-named symbol of this package.
  (:import-from #:graph-db #:def-vertex #:geometry #:make-point
                #:make-polygon #:make-linestring)
  (:export #:run-spacetime-tests #:spacetime-suite))
