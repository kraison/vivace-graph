#!/usr/bin/env bash
# One FiveAM test of graph-db/rules-test in a fresh image, under the
# fixture RUN-RULES-TESTS binds (tests/rules/suite.lisp).  NAME is the
# test's symbol; RULES-SUITE runs the whole suite.
set -u
[ $# -ge 1 ] || { echo "usage: $0 TEST-NAME" >&2; exit 2; }
exec sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:quickload :graph-db/rules-test :silent t)' \
  --eval "(let* ((d (graph-db-test-scratch:make-scratch-directory
                      \"graph-db-rules-sys\"))
                 (graph-db::*system-directory* (namestring d))
                 (graph-db::*type-registry* nil))
            (log:config :error)
            (unwind-protect
                 (let ((r (fiveam:run 'graph-db/rules-test::$1)))
                   (fiveam:explain! r)
                   (uiop:quit (if (fiveam:results-status r) 0 1)))
              (graph-db-test-scratch:cleanup-scratch-run)))"
