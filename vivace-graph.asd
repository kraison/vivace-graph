;; ASDF package description for vivace-graph              -*- Lisp -*-

(defpackage :vivace-graph-system (:use :cl :asdf))
(in-package :vivace-graph-system)

(defsystem vivace-graph
  :name "Vivace Graph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Vivace Graph"
  :long-description "Vivace Graph."
  :depends-on (:bordeaux-threads
	       :usocket
	       :protobuf
	       :hunchentoot
	       :cl-json
	       :uuid
	       :cl-fad
	       :ieee-floats
	       :parse-number
	       :cffi
	       :sb-concurrency
	       :sb-posix
	       :kyoto-persistence
	       :cl-skip-list
	       :local-time
	       :date-calc
	       :montezuma
	       :py-configparser
	       :js
	       :split-sequence)
  :components ((:file "uuid")
	       (:file "vivace-graph-package" :depends-on ("uuid"))
	       (:file "gettimeofday" :depends-on ("vivace-graph-package"))
	       (:file "conditions" :depends-on ("vivace-graph-package"))
	       (:file "constants" :depends-on ("conditions"))
	       (:file "globals" :depends-on ("constants"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "data-types" :depends-on ("utilities"))
	       (:file "certainty-factors" :depends-on ("constants"))
	       (:file "graph" :depends-on ("data-types" "certainty-factors"))
	       (:file "predicate" :depends-on ("graph"))
	       (:file "node" :depends-on ("graph" "data-types"))
	       (:file "triples" :depends-on ("predicate" "node"))
	       (:file "triples-interface" :depends-on ("triples"))
	       (:file "templates" :depends-on ("triples-interface"))
	       (:file "prologc" :depends-on ("templates"))
	       (:file "prolog-functors" :depends-on ("prologc"))
	       (:file "rules" :depends-on ("prolog-functors"))
	       (:file "gc" :depends-on ("rules" "triples" "node"))
	       (:file "interface" :depends-on ("rules"))
	       (:file "session" :depends-on ("interface"))
	       (:file "binary-protocol" :depends-on ("session"))
	       (:file "listener" :depends-on ("binary-protocol"))
	       (:file "json-rpc" :depends-on ("interface"))
	       (:file "json-functions" :depends-on ("json-rpc"))
	       (:file "server" :depends-on ("json-functions" "listener"))))
