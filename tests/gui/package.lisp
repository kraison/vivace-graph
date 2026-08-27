;;;; Test package for the graph-db/gui backend (GH #269).

(in-package #:cl-user)

;; The symlink-escape regression test plants a real symlink (GH #269).
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defpackage #:graph-db/gui-test
  (:use #:cl #:fiveam #:graph-db)
  ;; graph-db's Prolog cut must win over fiveam's ! helper, exactly as
  ;; the main suite's package does.
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db.gui
                #:start-gui
                #:stop-gui)
  (:export #:run-gui-tests))
