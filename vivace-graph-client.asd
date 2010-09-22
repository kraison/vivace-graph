;; ASDF package description for vivace-graph-client              -*- Lisp -*-

(defpackage :vivace-graph-client-system (:use :cl :asdf))
(in-package :vivace-graph-client-system)

(defsystem vivace-graph-client
  :name "Vivace Graph Binary Client"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Vivace Graph Binary Protocol Client"
  :long-description "Vivace Graph Binary Protocol Client."
  :depends-on (:bordeaux-threads
	       :usocket
	       :uuid
	       :cffi
	       :kyoto-persistence
	       :local-time
	       :date-calc
	       :py-configparser
	       :split-sequence)
  :components ((:file "vivace-graph-client-package")
	       (:file "client-globals" :depends-on ("vivace-graph-client-package"))
	       (:file "client" :depends-on ("client-globals"))))

