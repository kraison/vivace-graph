#!/usr/bin/env bash
#
# Run the test-op of one or more ASDF systems of THIS checkout, each in
# its own fresh SBCL with the heap the suites need (CLAUDE.md, "Tests").
# One image per system: a suite's schema registers functors and types
# that another suite's tripwires count (the GUI's pinned functor
# inventory), so two suites in one image can fail where each alone is
# green, as CI runs them.
#
# Usage:
#   tests/run-system-tests.sh graph-db/query-test graph-db/gui-test
#
# Exits 0 when every system's test-op passed, 1 when one failed or did
# not load, 2 without an argument.  Each test-op errors on failure, so
# an error anywhere is the failing status.

set -u

if [ "$#" -lt 1 ]; then
  echo "usage: $0 SYSTEM [SYSTEM...]" >&2
  exit 2
fi

HERE="$(cd "$(dirname "$0")/.." && pwd -P)"
export CL_SOURCE_REGISTRY="$HERE//:"

status=0
for system in "$@"; do
  echo "=== $system"
  sbcl --dynamic-space-size 16384 --non-interactive \
    --eval '(require :asdf)' \
    --eval '(unless (find-package :ql)
              (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                            (user-homedir-pathname))))
                (when (probe-file setup) (load setup))))' \
    --eval "(ql:quickload \"$system\" :silent t)" \
    --eval "(asdf:test-system \"$system\")" || status=1
done
exit $status
