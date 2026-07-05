#!/usr/bin/env bash
#
# Two-process peer-replication test for VivaceGraph (Branch A pull MVP).
#
# Peer replication CANNOT be tested with hub and device in one Lisp image: they
# would share the process-global *graphs* registry, the schema registry, *graph*
# and other global state, producing false results.  This harness runs the hub
# and the device as TWO SEPARATE OS processes talking over a loopback socket --
# the only faithful setup (same rationale as tests/replication/).
#
# Usage:
#   tests/peer-replication/run-peer-test.sh
#
# Environment overrides:
#   REPL_LISP_CMD       how to load+run a Lisp file for BOTH ends
#                       (default: "sbcl --non-interactive --load"; e.g. "ecl --load")
#   REPL_HUB_LISP_CMD   override just the hub  (default: REPL_LISP_CMD)
#   REPL_DEVICE_LISP_CMD override just the device (default: REPL_LISP_CMD)
#   REPL_PORT           TCP port for the hub listener (default: random high port)
#
# The ship-to-Android configuration is an SBCL hub and an ECL device:
#   REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
#   REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication/run-peer-test.sh
#
# Exits 0 if the device converges to its authority-scoped subgraph on every
# check (seed + purge + schema-compat), else 1.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
LISP_CMD="${REPL_LISP_CMD:-sbcl --non-interactive --load}"
HUB_LISP_CMD="${REPL_HUB_LISP_CMD:-$LISP_CMD}"
DEVICE_LISP_CMD="${REPL_DEVICE_LISP_CMD:-$LISP_CMD}"
PORT="${REPL_PORT:-$(( 19000 + RANDOM % 2000 ))}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/vg-peer-XXXXXX")"

export REPL_WORK="$WORK"
export REPL_HUB_DIR="$WORK/hub/"
export REPL_DEVICE_DIR="$WORK/device/"
export REPL_PORT="$PORT"
mkdir -p "$REPL_HUB_DIR" "$REPL_DEVICE_DIR"

echo "Peer-replication test: hub='$HUB_LISP_CMD' device='$DEVICE_LISP_CMD' port=$PORT work=$WORK"

# Hub runs in the background: commits phase 1, waits for the device to verify,
# flips disclosure for phase 2, then exits when the device signals done.
$HUB_LISP_CMD "$HERE/hub.lisp" > "$WORK/hub.out" 2>&1 &
HPID=$!

# Device runs in the foreground: waits for the hub's "ready" flag, pulls, and
# verifies every phase.
$DEVICE_LISP_CMD "$HERE/device.lisp" > "$WORK/device.out" 2>&1
DEVICE_CODE=$?

wait "$HPID" 2>/dev/null
HUB_CODE=$?
kill "$HPID" 2>/dev/null   # belt and suspenders

echo "--- hub output ---"
grep -E 'HUB' "$WORK/hub.out" 2>/dev/null || cat "$WORK/hub.out"
echo "--- device output ---"
grep -E '  ok|  FAIL|DEVICE|ERROR' "$WORK/device.out" 2>/dev/null || cat "$WORK/device.out"

if [ "$DEVICE_CODE" -eq 0 ]; then
  echo "RESULT: PASS"
  rm -rf "$WORK"
  exit 0
else
  echo "RESULT: FAIL (device exit $DEVICE_CODE, hub exit $HUB_CODE)"
  echo "logs kept in: $WORK"
  exit 1
fi
