#!/usr/bin/env bash
#
# Run the test-op of one or more ASDF systems of THIS checkout in one
# fresh SBCL, with the heap the suites need (CLAUDE.md, "Tests").
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

FORMS=()
for system in "$@"; do
  FORMS+=(--eval "(ql:quickload \"$system\" :silent t)")
  FORMS+=(--eval "(asdf:test-system \"$system\")")
done

exec sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(require :asdf)' \
  --eval '(unless (find-package :ql)
            (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
              (when (probe-file setup) (load setup))))' \
  "${FORMS[@]}"
