#!/usr/bin/env bash
#
# Run one Prolog bench function of THIS checkout alone, at small scale, in
# one fresh SBCL (GH #45).  The bench runs under the same bindings
# run-prolog-bench makes (system directory, null system clock).
#
# Usage:
#   tests/perf/run-bench.sh run-prolog-bench          # all five cases
#   tests/perf/run-bench.sh bench-clause-assert       # one case
#
# Exits 0 when the call returns, 1 on an unhandled error or a failed load,
# 2 without an argument or with a FUNCTION this script does not know.

set -u

if [ "$#" -lt 1 ]; then
  echo "usage: $0 FUNCTION" >&2
  exit 2
fi

case "$1" in
  run-prolog-bench)
    CALL="(graph-db/perf-test::run-prolog-bench :scale :small)" ;;
  bench-clause-assert | bench-clause-assert-10k \
  | bench-guarded-shape-repeat | bench-recursive-ring \
  | bench-change-feed-cursor | bench-graph-predicates \
  | bench-traced-k-hop | bench-tabled-chain)
    CALL="(graph-db/perf-test::run-prolog-bench :scale :small :only 'graph-db/perf-test::$1)" ;;
  *)
    echo "unknown FUNCTION: $1" >&2
    exit 2 ;;
esac

HERE="$(cd "$(dirname "$0")/../.." && pwd -P)"
export CL_SOURCE_REGISTRY="$HERE//:"

exec sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(require :asdf)' \
  --eval '(unless (find-package :ql)
            (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
              (when (probe-file setup) (load setup))))' \
  --eval '(ql:quickload "graph-db/perf-test" :silent t)' \
  --eval "$CALL"
