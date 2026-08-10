;;;; Master suite + runner for the spacetime tests.

(in-package #:graph-db/spacetime-test)

(def-suite spacetime-suite
  :description "Temporal extents, the Allen algebra, and standing (#130).")

(defun run-spacetime-tests ()
  "Run the spacetime suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/spacetime)."
  (log:config :error)
  (let ((results (run 'spacetime-suite)))
    (explain! results)
    (results-status results)))

(defun ts (year month day &optional (hour 0) (minute 0) (sec 0) (nsec 0))
  "A UTC timestamp.  Every test builds times through this, so none of them
can accidentally depend on the host timezone (design §3.5)."
  (encode-timestamp nsec sec minute hour day month year
                    :timezone +utc-zone+))
