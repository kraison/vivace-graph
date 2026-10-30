#!/usr/bin/env bash
#
# Load each loadable example in a fresh SBCL image, with the example's
# own scratch directories under /var/tmp cleared first.  Any unhandled
# condition exits non-zero (--non-interactive disables the debugger).
# CI runs this on every push (GH #417); CLAUDE.md's "smoke-tested by
# loading them in a fresh image" is this script.
#
# Usage: tests/load-examples.sh            # both examples
#        tests/load-examples.sh example    # one, by basename
set -u

HERE="$(cd "$(dirname "$0")/.." && pwd -P)"
# This checkout's graph-db.asd, not whichever Quicklisp's local-projects
# links (CLAUDE.md, "Working in this repo").
export CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY:-$HERE//:}"

# example -> the scratch it writes (examples/example*.lisp).
scratch_of() {
  case "$1" in
    example)
      echo /var/tmp/test-graph /var/tmp/test-graph-system /var/tmp/graph.log ;;
    example-spacetime)
      echo /var/tmp/spacetime-example /var/tmp/spacetime-example-system ;;
    *) echo "load-examples: unknown example $1" >&2; exit 2 ;;
  esac
}

if [ "$#" -ge 1 ]; then
  examples="$*"
else
  examples="example example-spacetime"
fi

status=0
for ex in $examples; do
  file="$HERE/examples/$ex.lisp"
  [ -f "$file" ] || { echo "load-examples: no $file" >&2; exit 2; }
  # shellcheck disable=SC2046
  rm -rf $(scratch_of "$ex")
  echo "=== loading examples/$ex.lisp"
  if sbcl --dynamic-space-size 4096 --non-interactive \
          --load "$HOME/quicklisp/setup.lisp" \
          --load "$file" > "/tmp/load-$ex.$$.log" 2>&1; then
    echo "=== examples/$ex.lisp loaded"
  else
    echo "=== examples/$ex.lisp FAILED; last lines:"
    tail -40 "/tmp/load-$ex.$$.log"
    status=1
  fi
  rm -f "/tmp/load-$ex.$$.log"
done
exit $status
