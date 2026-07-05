;;;; Master suite + runner for graph-db/algorithms tests.

(in-package #:graph-db/algorithms-test)

(def-suite graph-db-algorithms-suite
  :description "All graph-db/algorithms tests (fib-heap, shortest paths, projection).")

(defun run-algorithm-tests ()
  "Run the graph-db/algorithms test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/algorithms-test)."
  (log:config :error)
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  (let ((results (run 'graph-db-algorithms-suite)))
    (explain! results)
    (results-status results)))
