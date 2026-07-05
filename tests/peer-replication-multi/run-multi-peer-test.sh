#!/usr/bin/env bash
#
# Three-process multi-device peer-replication test: ONE hub + TWO devices
# (scopes "alpha" and "bravo") over loopback.  Exercises what the 2-process
# tests/peer-replication/ cannot: scope isolation across devices, an overlap node
# both hold, per-device re-task purge, and purge -> re-entry (PT-2).
#
# Peer replication CANNOT be tested in one Lisp image (process-global *graphs*,
# schema registry, *graph*), so each replica is its own OS process.
#
# Usage:
#   tests/peer-replication-multi/run-multi-peer-test.sh
#
# Environment overrides:
#   REPL_LISP_CMD        load+run a Lisp file for all three (default sbcl)
#   REPL_HUB_LISP_CMD    override just the hub    (default REPL_LISP_CMD)
#   REPL_DEVICE_LISP_CMD override both devices    (default REPL_LISP_CMD)
#   REPL_PORT            hub listener port        (default random high port)
#
# Ship config (SBCL hub, ECL devices):
#   REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
#   REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication-multi/run-multi-peer-test.sh

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
LISP_CMD="${REPL_LISP_CMD:-sbcl --non-interactive --load}"
HUB_LISP_CMD="${REPL_HUB_LISP_CMD:-$LISP_CMD}"
DEVICE_LISP_CMD="${REPL_DEVICE_LISP_CMD:-$LISP_CMD}"
PORT="${REPL_PORT:-$(( 21000 + RANDOM % 2000 ))}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/vg-multi-XXXXXX")"

export REPL_WORK="$WORK"
export REPL_HUB_DIR="$WORK/hub/"
export REPL_PORT="$PORT"
mkdir -p "$REPL_HUB_DIR" "$WORK/dev-a/" "$WORK/dev-b/"

# Warm each implementation's fasl cache BEFORE launching: three processes
# compiling graph-db simultaneously from cold makes the hub miss the devices'
# 60s wait-for-ready.  (Idempotent + fast once warm.)
warm() {  # $1 = a lisp command string; warm the impl it names
  case "$1" in
    *sbcl*) sbcl --non-interactive --eval '(ql:quickload :graph-db :silent t)' >/dev/null 2>&1 ;;
    *ecl*)  ecl --eval '(require :asdf)' \
                --eval '(unless (find-package :ql) (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))' \
                --eval '(ql:quickload :graph-db :silent t)' --eval '(ext:quit 0)' >/dev/null 2>&1 ;;
    *ccl*)  ccl --eval '(ql:quickload :graph-db :silent t)' --eval '(ccl:quit)' >/dev/null 2>&1 ;;
  esac
}
echo "Multi-device peer test: hub='$HUB_LISP_CMD' device='$DEVICE_LISP_CMD' port=$PORT work=$WORK"
echo "Warming fasl caches..."
warm "$HUB_LISP_CMD"
[ "$DEVICE_LISP_CMD" != "$HUB_LISP_CMD" ] && warm "$DEVICE_LISP_CMD"

# Hub in the background; both devices in the background; wait on all three.
$HUB_LISP_CMD "$HERE/hub.lisp" > "$WORK/hub.out" 2>&1 &
HPID=$!
REPL_DEVICE_ID=a REPL_DEVICE_DIR="$WORK/dev-a/" $DEVICE_LISP_CMD "$HERE/device.lisp" > "$WORK/dev-a.out" 2>&1 &
APID=$!
REPL_DEVICE_ID=b REPL_DEVICE_DIR="$WORK/dev-b/" $DEVICE_LISP_CMD "$HERE/device.lisp" > "$WORK/dev-b.out" 2>&1 &
BPID=$!

wait "$APID"; A_CODE=$?
wait "$BPID"; B_CODE=$?
wait "$HPID"; H_CODE=$?

echo "--- hub output ---";      grep -E 'HUB' "$WORK/hub.out" 2>/dev/null || cat "$WORK/hub.out"
echo "--- device A output ---"; grep -E '  ok|  FAIL|DEVICE|ERROR' "$WORK/dev-a.out" 2>/dev/null || cat "$WORK/dev-a.out"
echo "--- device B output ---"; grep -E '  ok|  FAIL|DEVICE|ERROR' "$WORK/dev-b.out" 2>/dev/null || cat "$WORK/dev-b.out"

if [ "$A_CODE" -eq 0 ] && [ "$B_CODE" -eq 0 ]; then
  echo "RESULT: PASS"
  rm -rf "$WORK"
  exit 0
else
  echo "RESULT: FAIL (device-a $A_CODE, device-b $B_CODE, hub $H_CODE)"
  echo "logs kept in: $WORK"
  exit 1
fi
