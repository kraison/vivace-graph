#!/usr/bin/env bash
#
# Two-process peer-replication PUSH conflict test for VivaceGraph (Branch B, B2d-2b).
#
# The device pulls a find, edits it locally (releases the hazard DANGER->SAFE +
# rewrites a note), and PUSHES the two authored ops.  The hub RE-HOMES them through
# the merge resolver: the release is rejected (hazard stays DANGER + a conflict is
# surfaced), the :lww note takes the device's value.  BOTH processes must pass -- the
# hub verifies its merged state (it is the one that re-homes), the device its own.
#
# Peer replication CANNOT be tested in one image (process-global *graphs*, schema
# registry, *graph*), so hub and device run as TWO OS processes over a loopback
# socket -- the only faithful setup (same rationale as tests/replication/).
#
# Usage:   tests/peer-replication-push/run-push-test.sh
# Overrides: REPL_LISP_CMD / REPL_HUB_LISP_CMD / REPL_DEVICE_LISP_CMD / REPL_PORT
#   (the ship config is an SBCL hub + ECL device:
#     REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
#     REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication-push/run-push-test.sh)

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
LISP_CMD="${REPL_LISP_CMD:-sbcl --non-interactive --load}"
HUB_LISP_CMD="${REPL_HUB_LISP_CMD:-$LISP_CMD}"
DEVICE_LISP_CMD="${REPL_DEVICE_LISP_CMD:-$LISP_CMD}"
PORT="${REPL_PORT:-$(( 21000 + RANDOM % 2000 ))}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/vg-push-XXXXXX")"

export REPL_WORK="$WORK"
export REPL_HUB_DIR="$WORK/hub/"
export REPL_DEVICE_DIR="$WORK/device/"
export REPL_PORT="$PORT"
mkdir -p "$REPL_HUB_DIR" "$REPL_DEVICE_DIR"

echo "Peer PUSH test: hub='$HUB_LISP_CMD' device='$DEVICE_LISP_CMD' port=$PORT work=$WORK"

$HUB_LISP_CMD "$HERE/hub.lisp" > "$WORK/hub.out" 2>&1 &
HPID=$!

$DEVICE_LISP_CMD "$HERE/device.lisp" > "$WORK/device.out" 2>&1
DEVICE_CODE=$?

wait "$HPID" 2>/dev/null
HUB_CODE=$?
kill "$HPID" 2>/dev/null

echo "--- hub output ---"
grep -E 'HUB|  ok|  FAIL' "$WORK/hub.out" 2>/dev/null || cat "$WORK/hub.out"
echo "--- device output ---"
grep -E '  ok|  FAIL|DEVICE|ERROR' "$WORK/device.out" 2>/dev/null || cat "$WORK/device.out"

if [ "$DEVICE_CODE" -eq 0 ] && [ "$HUB_CODE" -eq 0 ]; then
  echo "RESULT: PASS"
  rm -rf "$WORK"
  exit 0
else
  echo "RESULT: FAIL (device exit $DEVICE_CODE, hub exit $HUB_CODE)"
  echo "logs kept in: $WORK"
  exit 1
fi
