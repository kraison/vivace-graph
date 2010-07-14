;; ASDF package description for vivace-graph              -*- Lisp -*-

(defpackage :vivace-graph-system (:use :cl :asdf))
(in-package :vivace-graph-system)

(defsystem vivace-graph
  :name "Vivace Graph Store Library"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Vivace Graph Store"
  :long-description "Vivace Graph Store."
  :depends-on (:bordeaux-threads
	       :uuid
	       :ieee-floats
	       :cffi
	       :sb-concurrency
	       :sb-posix
	       ;:cl-store
	       ;:montezuma
	       :cl-tokyo-cabinet
	       :cl-containers
	       :py-configparser
	       :usocket
	       :clpython
	       :js
	       :protobuf
	       :hunchentoot)
  :components ((:file "uuid")
	       (:file "vivace-graph-package" :depends-on ("uuid"))
	       (:file "conditions" :depends-on ("vivace-graph-package"))
	       (:file "constants" :depends-on ("conditions"))
	       (:file "globals" :depends-on ("constants"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "mcas" :depends-on ("utilities"))
	       (:file "skip-list" :depends-on ("mcas"))
	       (:file "data-structures" :depends-on ("skip-list"))
	       (:file "graph" :depends-on ("data-structures"))
	       (:file "node" :depends-on ("graph"))
	       (:file "triples" :depends-on ("node"))
	       (:file "disk-storage" :depends-on ("graph" "triples"))
	       (:file "serialize" :depends-on ("disk-storage"))
	       (:file "tx-log" :depends-on ("serialize"))
	       (:file "transactions" :depends-on ("tx-log"))
	       (:file "prolog" :depends-on ("transactions"))
	       (:file "interface" :depends-on ("prolog"))))
