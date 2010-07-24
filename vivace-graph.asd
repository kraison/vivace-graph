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
	       :cl-tokyo-cabinet
	       :montezuma
	       :py-configparser
	       :usocket
	       :js
	       :protobuf
	       :hunchentoot)
  :components ((:file "uuid")
	       (:file "vivace-graph-package" :depends-on ("uuid"))
	       (:file "gettimeofday" :depends-on ("vivace-graph-package"))
	       (:file "random" :depends-on ("vivace-graph-package"))
	       (:file "conditions" :depends-on ("vivace-graph-package"))
	       (:file "constants" :depends-on ("conditions"))
	       (:file "globals" :depends-on ("constants"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "disk-storage" :depends-on ("utilities"))
	       (:file "serialize" :depends-on ("disk-storage"))
	       (:file "tx-log" :depends-on ("serialize" "gettimeofday"))
	       (:file "mcas" :depends-on ("tx-log"))
	       (:file "skip-list" :depends-on ("mcas" "random"))
	       (:file "rete" :depends-on ("skip-list"))
	       (:file "graph" :depends-on ("rete"))
	       (:file "node" :depends-on ("graph"))
	       (:file "triples" :depends-on ("node"))
	       (:file "prolog" :depends-on ("triples"))
	       (:file "interface" :depends-on ("prolog"))))
