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
	       :cl-fad
	       :ieee-floats
	       :parse-number
	       :cffi
	       :sb-concurrency
	       :sb-posix
	       :cl-tokyo-cabinet
	       ;;:cl-kyoto-cabinet
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
	       (:file "certainty-factors" :depends-on ("constants"))
	       (:file "graph" :depends-on ("serialize" "certainty-factors"))
	       (:file "predicate" :depends-on ("graph"))
	       (:file "node" :depends-on ("graph" "data-types"))
	       (:file "triples" :depends-on ("predicate" "node"))
	       (:file "templates" :depends-on ("triples"))
	       (:file "prologc" :depends-on ("templates"))
	       (:file "prolog-functors" :depends-on ("prologc"))
	       ;;(:file "rete" :depends-on ("prolog-functors"))
	       (:file "leaps" :depends-on ("prolog-functors"))
	       (:file "rules" :depends-on ("leaps"))
	       (:file "gc" :depends-on ("rules" "triples" "node"))
	       (:file "interface" :depends-on ("prolog-functors"))))
