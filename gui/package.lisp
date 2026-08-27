;;;; Package for the graph-db/gui subsystem (GH #269).
;;;;
;;;; The GUI is an operational cockpit over engine internals, so it
;;;; reads GRAPH-DB's unexported machinery directly (always via explicit
;;;; GRAPH-DB:: qualification -- greppable, and no import list to rot).

(in-package #:cl-user)

(defpackage #:graph-db.gui
  (:use #:cl)
  (:export #:start-gui
           #:stop-gui
           #:*gui-port*))
