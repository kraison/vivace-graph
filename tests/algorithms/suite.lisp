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
  ;; Built inline rather than via FIXTURES.LISP's MAKE-TEMP-DIRECTORY: that
  ;; file loads after this one, and a forward reference would compile with a
  ;; style warning.
  (let* ((system-dir (ensure-directories-exist
                      (merge-pathnames
                       (format nil "graph-db-algo-sysdir-~36R/"
                               (random (expt 36 12) (make-random-state t)))
                       (uiop:temporary-directory))))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'graph-db-algorithms-suite)))
           (explain! results)
           (results-status results))
      (uiop:delete-directory-tree system-dir :validate t
                                             :if-does-not-exist :ignore))))
