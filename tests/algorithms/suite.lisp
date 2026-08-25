;;;; Master suite + runner for graph-db/algorithms tests.

(in-package #:graph-db/algorithms-test)

(def-suite graph-db-algorithms-suite
  :description "All graph-db/algorithms tests (fib-heap, shortest paths, projection).")

(defun run-algorithm-tests ()
  "Run the graph-db/algorithms test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/algorithms-test)."
  (log:config :error)
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  ;; Type-ids come from the system-wide registry, so every store this suite
  ;; opens needs a system directory (GH #186).  One for the whole run, which
  ;; is the shape a real system has: many stores, one registry.
  (let* ((system-dir (graph-db-test-scratch:make-scratch-directory
                      "graph-db-algo-sysdir"))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'graph-db-algorithms-suite)))
           (explain! results)
           (results-status results))
      ;; system-dir and all test scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))
