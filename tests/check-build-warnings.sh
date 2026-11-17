#!/usr/bin/env bash
#
# Compile this checkout's systems from a clean FASL cache in a fresh
# SBCL and fail on any full WARNING they raise: an undefined variable
# or function, a type conflict, a redefinition -- not a STYLE-WARNING.
# The suites load with warnings muted (ql:quickload :silent t), which is
# how an undefined-variable warning lived unseen from a333102 until a
# CCL lane found it (GH #435, #439).  The variables it catches are the
# ones CLAUDE.md's #92 trap is about: a special read in a file compiled
# before its defvar.
#
# Usage:
#   tests/check-build-warnings.sh                 # the default systems
#   tests/check-build-warnings.sh graph-db/gui    # named systems
#
# Exits 0 when every system compiled without a full warning, 1 when one
# warned or failed to load.  CI runs it on every push (docs/ci.md).
set -u

HERE="$(cd "$(dirname "$0")/.." && pwd -P)"
export CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY:-$HERE//:}"
if [ "$#" -ge 1 ]; then
  systems="$*"
else
  systems="graph-db graph-db/spacetime graph-db/rules graph-db/gui graph-db/algorithms-io"
fi
cache="$(mktemp -d "${TMPDIR:-/tmp}/graph-db-build-warnings-XXXXXX")/"
trap 'rm -rf "$cache"' EXIT

sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(require :asdf)' \
  --eval '(unless (find-package :ql)
            (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
              (when (probe-file setup) (load setup))))' \
  --eval "(asdf:initialize-output-translations
            '(:output-translations (\"$HERE/\" \"$cache\")
              :inherit-configuration))" \
  --eval "(defvar cl-user::*systems* '($systems))" \
  --eval '(defvar cl-user::*warnings* (list))' \
  --eval "(defvar cl-user::*tree* \"$HERE/\")" \
  --eval '(handler-bind
              ((warning
                 (lambda (w)
                   (let ((file (or *compile-file-truename* *load-truename*)))
                     ;; Counted: a full warning from a file of this tree,
                     ;; or one SBCL defers to the end of the compilation
                     ;; unit (an undefined variable or function), which
                     ;; arrives with no file bound; its text names the
                     ;; symbol and the compiler log above names the file.
                     ;; Not counted: style warnings, ASDF/UIOP wrappers
                     ;; around them, and a dependency compiled elsewhere.
                     (unless (or (typep w (quote style-warning))
                                 (typep w (quote uiop:compile-warned-warning))
                                 (typep w (quote uiop:compile-failed-warning))
                                 (and file
                                      (not (uiop:subpathp file
                                                          cl-user::*tree*))))
                       (push (list file (princ-to-string w))
                             cl-user::*warnings*))))))
            ;; ASDF:LOAD-SYSTEM, not QL:QUICKLOAD: the latter muffles
            ;; every warning under :SILENT, and prints them all otherwise.
            (dolist (s cl-user::*systems*)
              (format t "~&=== ~A~%" s)
              (asdf:load-system s)))' \
  --eval '(progn
            (cond ((null cl-user::*warnings*)
                   (format t "~&build-warnings: none~%")
                   (uiop:quit 0))
                  (t
                   (format t "~&build-warnings: ~D full warning~:P~%"
                           (length cl-user::*warnings*))
                   (dolist (w (reverse cl-user::*warnings*))
                     (format t "  ~A~%    ~A~%"
                             (or (first w) "(no file)") (second w)))
                   (uiop:quit 1))))'
