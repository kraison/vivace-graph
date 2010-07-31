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
	       :cl-skip-list
	       :local-time
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
	       (:file "data-types" :depends-on ("serialize"))
	       (:file "graph" :depends-on ("serialize"))
	       (:file "predicate" :depends-on ("graph"))
	       (:file "node" :depends-on ("graph" "data-types"))
	       (:file "triples" :depends-on ("predicate" "node"))
	       (:file "rules" :depends-on ("triples"))
	       (:file "prologc" :depends-on ("rules"))
	       (:file "interface" :depends-on ("prologc"))))
