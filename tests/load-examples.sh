#!/usr/bin/env bash
#
# Load each loadable example in a fresh SBCL image, with its scratch in a
# private temporary directory (GRAPH_DB_EXAMPLE_ROOT, which both examples
# honour), so runs by different users on one host never collide and
# nothing outside that directory is ever removed.  Any unhandled condition
# exits non-zero (--non-interactive disables the debugger).  CI runs this
# on every push (GH #417); CLAUDE.md's "smoke-tested by loading them in a
# fresh image" is this script.
#
# Usage: tests/load-examples.sh            # both examples
#        tests/load-examples.sh example    # one, by basename
set -u

HERE="$(cd "$(dirname "$0")/.." && pwd -P)"
# This checkout's graph-db.asd, not whichever Quicklisp's local-projects
# links (CLAUDE.md, "Working in this repo").
export CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY:-$HERE//:}"

if [ "$#" -ge 1 ]; then
  examples="$*"
else
  examples="example example-spacetime"
fi

status=0
for ex in $examples; do
  file="$HERE/examples/$ex.lisp"
  [ -f "$file" ] || { echo "load-examples: no $file" >&2; exit 2; }
  root="$(mktemp -d "${TMPDIR:-/tmp}/graph-db-example-XXXXXX")/"
  log="$root/load.log"
  echo "=== loading examples/$ex.lisp (scratch $root)"
  if GRAPH_DB_EXAMPLE_ROOT="$root" \
     sbcl --dynamic-space-size 4096 --non-interactive \
          --load "$HOME/quicklisp/setup.lisp" \
          --load "$file" > "$log" 2>&1; then
    echo "=== examples/$ex.lisp loaded"
  else
    echo "=== examples/$ex.lisp FAILED; last lines:"
    tail -40 "$log"
    status=1
  fi
  rm -rf "$root"
done
exit $status
