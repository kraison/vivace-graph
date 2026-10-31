;;;; tests/rules/package.lisp -- graph-db/rules-test (GH #330).

(defpackage #:graph-db/rules-test
  (:use #:cl #:fiveam #:graph-db.spacetime)
  (:import-from #:graph-db #:make-graph #:close-graph #:with-transaction
                #:select #:select-flat #:select-count
                #:copy #:save #:open-graph #:mark-deleted)
  (:export #:run-rules-tests #:rules-suite))
