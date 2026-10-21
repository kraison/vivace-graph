#!/usr/bin/env bash
#
# Tests for tests/run-suite.sh, the one-suite-per-fresh-image runner.
#
# Usage:
#   tests/run-suite-test.sh
#
# Prints one "ok"/"FAIL" line per check and exits non-zero if any failed.
# Each check that runs a suite compiles/loads graph-db/test in a fresh SBCL
# image, so the first run of a checkout is slow; later runs reuse the FASL
# cache (see "One FASL cache per checkout" in CLAUDE.md).

set -u
HERE="$(cd "$(dirname "$0")/.." && pwd -P)"
RUN="$HERE/tests/run-suite.sh"
fails=0

ok() { echo "ok   $*"; }
fail() { echo "FAIL $*"; fails=$((fails + 1)); }

# 1. A real suite runs green and reports a pass count.
out="$(bash "$RUN" geohash-suite 2>&1)"
code=$?
if [ "$code" -eq 0 ] && printf '%s\n' "$out" | grep -q 'Pass:'; then
  ok "geohash-suite exits 0 and output contains 'Pass:'"
else
  fail "geohash-suite exits 0 and output contains 'Pass:' (exit $code)"
fi

# 2. That run loaded this checkout, not another one found through Quicklisp.
line="$(printf '%s\n' "$out" | grep -m1 '^graph-db from ')"
if [ "$line" = "graph-db from $HERE/" ]; then
  ok "graph-db from names this checkout ($HERE/)"
else
  fail "graph-db from names this checkout"
  echo "     want: graph-db from $HERE/"
  echo "     got:  ${line:-no such line}"
fi

# 3. An unknown suite is refused, not silently reported green.
bash "$RUN" no-such-suite >/dev/null 2>&1
code=$?
if [ "$code" -eq 2 ]; then
  ok "no-such-suite exits 2"
else
  fail "no-such-suite exits 2 (got $code)"
fi

# 4. No argument prints usage to stderr and exits 2.
err="$(bash "$RUN" 2>&1 >/dev/null)"
code=$?
if [ "$code" -eq 2 ] && printf '%s\n' "$err" | grep -qi 'usage'; then
  ok "no argument prints usage to stderr and exits 2"
else
  fail "no argument prints usage to stderr and exits 2 (exit $code)"
fi

if [ "$fails" -ne 0 ]; then
  echo "$fails check(s) failed"
  exit 1
fi
echo "all checks passed"
exit 0
