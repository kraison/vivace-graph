#!/usr/bin/env bash
#
# Run ONE FiveAM suite in a fresh SBCL image, with the fixtures RUN-TESTS
# binds (tests/suite.lisp).
#
# Usage:
#   tests/run-suite.sh SUITE
#   tests/run-suite.sh geohash-suite
#
# A bare (fiveam:run! 'some-suite) fails at MAKE-GRAPH with
# SYSTEM-DIRECTORY-REQUIRED: the system directory and type registry are
# bound by RUN-TESTS, not by loading the test system.  This script loads
# GRAPH-DB/TEST, binds them as RUN-TESTS does, runs just SUITE, and tears
# the run's scratch parent down afterwards.
#
# It exits 0 when every check passed, 1 when one failed, and 2 when SUITE
# names no FiveAM suite (or no argument was given).

set -u

if [ "$#" -lt 1 ] || [ -z "$1" ]; then
  echo "usage: $0 SUITE" >&2
  echo "  e.g. $0 geohash-suite" >&2
  exit 2
fi

SUITE="$1"
HERE="$(cd "$(dirname "$0")/.." && pwd -P)"

# Make ASDF find THIS checkout's graph-db.asd.  Without it Quicklisp's
# local-projects resolves :graph-db to whichever checkout it links, so a
# worktree would silently test another tree (CLAUDE.md, "Working in this
# repo").
export CL_SOURCE_REGISTRY="$HERE//:"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/graph-db-run-suite-XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

export RUN_SUITE_NAME="$SUITE"

cat > "$WORK/run-suite.lisp" <<'LISP'
;;; Run one named FiveAM suite from the GRAPH-DB/TEST system, with the
;;; fixture bindings RUN-TESTS (tests/suite.lisp) makes.

(require :asdf)

;; Dependencies (fiveam, bordeaux-threads, ...) resolve through Quicklisp;
;; load its setup unless the init file already did.
(unless (find-package :ql)
  (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                (user-homedir-pathname))))
    (when (probe-file setup)
      (load setup))))

(asdf:load-system :graph-db/test)

;; Say which checkout got loaded: in a worktree the wrong graph-db is a
;; silent failure mode, so make it visible and checkable.
(format t "graph-db from ~A~%"
        (asdf:system-source-directory :graph-db))

(let* ((name (uiop:getenv "RUN_SUITE_NAME"))
       (symbol (find-symbol (string-upcase name) "GRAPH-DB/TEST"))
       (suite (and symbol (fiveam:get-test symbol))))
  (unless suite
    (format *error-output* "run-suite: no suite ~A~%" name)
    (uiop:quit 2))
  ;; Keep test output to genuine problems, as RUN-TESTS does.
  (log:config :error)
  ;; Type-ids come from the system-wide registry, so every store the suite
  ;; opens needs a system directory (GH #186).  One directory for the run.
  (let ((code 1))
    (let* ((system-dir (graph-db/test::make-temp-directory))
           (graph-db::*system-directory* (namestring system-dir))
           (graph-db::*type-registry* nil))
      (unwind-protect
           (let ((results (fiveam:run suite)))
             (fiveam:explain! results)
             (setf code (if (fiveam:results-status results) 0 1)))
        ;; Everything this run scratched lives under the shared per-run
        ;; parent; drop it whole (GH #214).
        (graph-db-test-scratch:cleanup-scratch-run)))
    ;; Quit only after the cleanup, which exiting would skip.
    (uiop:quit code)))
LISP

sbcl --dynamic-space-size 16384 --non-interactive --load "$WORK/run-suite.lisp"
