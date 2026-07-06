;; ASDF package description for graph-db              -*- Lisp -*-

(defpackage :graph-db-system (:use :cl :asdf))
(in-package :graph-db-system)

;; CORE: the embeddable engine -- storage (mmap), graph, spatial index, prolog
;; query, transactions, on-disk WAL/backup.  NO HTTP server and NO network
;; replication transport, so it drops :hunchentoot :ningle :clack :usocket.
;; This is the target for the offline Android field app (cross-compiled under
;; ECL); the app calls in-process, not over HTTP.  The full :graph-db system
;; below is core + the two network leaves and stays behaviour-identical for
;; existing consumers (mine-action, odm).
(defsystem graph-db/core
  :name "VivaceGraph (embeddable core)"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "2.1.1"
  :depends-on (:bordeaux-threads
               :alexandria
               :iterate
               :cffi
               :cl-ppcre
               :uuid
               :split-sequence
               #+sbcl :sb-concurrency
               #+(or ccl lispworks) :closer-mop
               #+(or ccl lispworks) :trivial-timeout
               :cl-store
               :cl-fad
               :local-time
               :ieee-floats
               :cl-json
               ;; log4cl's compile-time machinery breaks ECL cross-compilation;
               ;; the Android core build sets :graph-db-stub-log and uses the
               ;; no-op log-stub instead.  Desktop/SBCL keeps real log4cl.
               #-graph-db-stub-log :log4cl
               :md5)
  :components (;;(:file "uuid")
               #+graph-db-stub-log (:file "log-stub")
               (:file "package" #+graph-db-stub-log :depends-on #+graph-db-stub-log ("log-stub"))
               (:file "cl-store-ecl" :depends-on ("package"))
               (:file "globals" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "posix" :depends-on ("package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "queue" :depends-on ("utilities"))
               (:file "mailbox" :depends-on ("queue"))
               #+(or sbcl lispworks ecl) (:file "rw-lock" :depends-on ("queue"))
               #+(or sbcl lispworks ecl) (:file "mmap" :depends-on ("rw-lock" "posix"))
               #-(or sbcl lispworks ecl) (:file "mmap" :depends-on ("queue" "posix"))
               (:file "pcons" :depends-on ("mmap"))
               (:file "node-id" :depends-on ("package" "posix"))
               (:file "buffer-pool" :depends-on ("pcons" "node-id"))
               (:file "serialize" :depends-on ("conditions" "buffer-pool" "cl-store-ecl"))
               (:file "geometry" :depends-on ("serialize"))
               (:file "geometry-ops" :depends-on ("geometry"))
               (:file "geohash" :depends-on ("package"))
               (:file "linear-hash" :depends-on ("serialize"))
               (:file "allocator" :depends-on ("serialize"))
               (:file "graph-class" :depends-on ("globals"))
               (:file "cursors" :depends-on ("package"))
               (:file "skip-list" :depends-on ("allocator" "linear-hash"))
               (:file "skip-list-cursors" :depends-on ("skip-list" "cursors"))
               (:file "mem-skip-list" :depends-on ("skip-list-cursors"))
               ;; EXPERIMENTAL third ordered-map backend: an mmap-backed B+ tree
               ;; (locality-oriented alternative to the skip list; see
               ;; docs/next-work-handoff.md).  Reuses skip-list-cursors' SKIP-NODE
               ;; struct + cursor protocol; lives in the same heap as the skip list.
               (:file "bplus-tree" :depends-on ("skip-list-cursors" "allocator"))
               (:file "spatial-index" :depends-on ("skip-list-cursors" "geometry" "geohash" "geometry-ops"))
               (:file "index-list" :depends-on ("linear-hash" "allocator"))
               (:file "ve-index" :depends-on ("skip-list-cursors" "index-list" "graph-class"))
               (:file "vev-index" :depends-on ("index-list" "graph-class"))
               (:file "type-index" :depends-on ("vev-index"))
               (:file "graph" :depends-on
                      ("ve-index" "vev-index" "type-index" "linear-hash" "allocator"
                       "spatial-index"))
               (:file "stats" :depends-on ("graph"))
               (:file "schema" :depends-on ("stats"))
               (:file "node-class" :depends-on ("schema"))
               (:file "views" :depends-on ("node-class"))
               (:file "primitive-node" :depends-on ("views"))
               (:file "vertex" :depends-on ("primitive-node"))
               (:file "edge" :depends-on ("vertex"))
               (:file "gc" :depends-on ("edge" "vertex" "views"))
               (:file "transactions" :depends-on ("graph-class" "type-index" "vev-index" "ve-index" "edge" "vertex" "gc" "spatial-index" "posix"))
               (:file "transaction-restore" :depends-on ("transactions"))
               (:file "transaction-log-streaming" :depends-on ("transactions"))
               (:file "backup" :depends-on ("edge"))
               (:file "replication" :depends-on ("backup"))
               (:file "txn-log" :depends-on ("replication"))
               (:file "functor" :depends-on ("vertex" "edge" "views" "schema"))
               (:file "prologc" :depends-on ("functor"))
               (:file "prolog-functors" :depends-on ("prologc" "geometry" "geometry-ops"))
               (:file "spatial-query" :depends-on ("prolog-functors" "transactions" "spatial-index" "geometry-ops"))
               (:file "interface" :depends-on ("schema" "edge" "vertex" "views"))
               (:file "traverse" :depends-on ("interface"))
               (:file "memory-graph" :depends-on ("traverse" "transactions" "graph" "mem-skip-list"))
               (:file "unique-constraint" :depends-on ("traverse" "transactions" "graph" "memory-graph" "node-class" "schema"))))

;; REPLICATION: core + the usocket network transport, but NO HTTP server.  This is
;; the master/slave + hub/peer replication layer -- transaction-streaming (usocket
;; framing + master/slave packet primitives), peer-merge (the pure per-field Branch B
;; conflict resolver), and peer-streaming (hub/device pull+push transport).  It drops
;; :hunchentoot :ningle :clack, so it is loadable under ECL and is the target for the
;; offline Android field device (which must replicate but cannot run the HTTP stack).
;; graph-db/core has already compiled+loaded all engine files, so these need no
;; intra-file :depends-on -- the system-level dependency guarantees order.
(defsystem graph-db/replication
  :name "VivaceGraph (replication transport)"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "2.1.1"
  :depends-on (:graph-db/core
               :usocket)
  :serial t
  :components ((:file "transaction-streaming")
               ;; peer replication Branch B: the pure per-field conflict resolver
               ;; (loaded before the transport that will call it).
               (:file "peer-merge")
               ;; peer replication transport (hub/device pull); needs usocket and
               ;; the master/slave packet primitives in transaction-streaming.
               (:file "peer-streaming")))

;; FULL: replication + the HTTP API leaf (rest, clack/ningle).  graph-db/replication
;; (and transitively graph-db/core) has already compiled+loaded the engine + transport,
;; so rest needs no intra-file :depends-on -- the system-level dependency guarantees
;; order.  Stays behaviour-identical for existing consumers (mine-action, odm), which
;; keep depending on :graph-db.
(defsystem graph-db
  :name "VivaceGraph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "2.1.1"
  :depends-on (:graph-db/replication
               :hunchentoot
               :ningle
               :clack
               ;; Load Clack's Hunchentoot backend up front instead of relying on
               ;; clack:clackup's lazy find-package-or-load at runtime: that lazy
               ;; path re-plans :clack, which can signal ASDF SYSTEM-OUT-OF-DATE on
               ;; some Quicklisp dists (e.g. clack + a newer lack) and silently
               ;; leave the handler unregistered -> ":HUNCHENTOOT is unknown handler."
               :clack-handler-hunchentoot
               :usocket
               :trivial-shell)
  :serial t
  :components ((:file "rest"))
  :in-order-to ((test-op (test-op :graph-db/test))))

(defsystem graph-db/concurrency-test
  :name "VivaceGraph concurrency test suite"
  :description "FiveAM thread-safety and concurrency tests for graph-db."
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/concurrency/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "helpers")
               (:file "rw-lock-tests")
               (:file "transaction-tests")
               (:file "data-structure-tests")
               (:file "graph-ops-tests")
               (:file "spatial-tests")
               (:file "view-tests")
               (:file "prolog-tests")
               (:file "acid-regression-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call
                             :graph-db/concurrency-test :run-concurrency-tests)
                      (error "graph-db concurrency tests failed."))))

(defsystem graph-db/acid-test
  :name "VivaceGraph ACID compliance tests"
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/acid/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "atomicity-tests")
               (:file "isolation-tests")
               (:file "durability-tests"))
  :perform (test-op (op c)
              (unless (uiop:symbol-call :graph-db/acid-test :run-acid-tests)
                (error "graph-db ACID tests failed."))))

(defsystem graph-db/stress-test
  :name "VivaceGraph stress test suite"
  :description "Single-threaded scale and correctness stress tests for graph-db."
  :depends-on (:graph-db :fiveam)
  :pathname "tests/stress/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "storage-stress")
               (:file "graph-stress")
               (:file "transaction-stress")
               (:file "view-stress"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/stress-test :run-stress-tests)
                      (error "graph-db stress tests failed."))))

(defsystem graph-db/concurrent-stress-test
  :name "VivaceGraph concurrent stress test suite"
  :description "Multi-threaded scale and stability tests for graph-db."
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/concurrent-stress/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "graph-storm")
               (:file "transaction-storm")
               (:file "view-storm")
               (:file "mixed-storm")
               (:file "mmap-remap-stress"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call
                             :graph-db/concurrent-stress-test
                             :run-concurrent-stress-tests)
                      (error "graph-db concurrent stress tests failed."))))

(defsystem graph-db/perf-test
  :name "VivaceGraph performance benchmark suite"
  :description "SBCL-focused performance benchmarks for graph-db (measurement, not pass/fail)."
  :depends-on (:graph-db :bordeaux-threads)
  :pathname "tests/perf/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "benchmarks")
               ;; B+ tree vs skip-list side-by-side (in-package :graph-db so it can
               ;; trace both read paths); entry point (graph-db::bplus-bench).
               (:file "bplus-bench"))
  :perform (test-op (op c)
                    (uiop:symbol-call :graph-db/perf-test :run-perf)))

;; OPTIONAL graph-algorithms add-on: analysis algorithms (shortest path,
;; ranking, components, flow, ...) ported from the standalone graph-utils
;; library onto VivaceGraph's persistent MVCC model.  Depends only on the
;; embeddable core (no HTTP), so it is usable from graph-db/core deployments.
(defsystem graph-db/algorithms
  :name "VivaceGraph graph algorithms"
  :description "Graph analysis algorithms (Mode B native + Mode A projection)."
  :depends-on (:graph-db/core)
  :pathname "algorithms/"
  :serial t
  :components ((:file "fib-heap")
               (:file "common")
               (:file "shortest-path")
               (:file "structure")
               (:file "ranking")
               (:file "projection")
               (:file "dense")
               (:file "flow")
               (:file "generation")
               (:file "prolog")))

;; OPTIONAL io add-on: GML/Pajek import + Graphviz export.  Kept separate so the
;; parsing deps (yacc, dso-lex, parse-number) stay out of the core algorithm
;; add-on and the embeddable core.
(defsystem graph-db/algorithms-io
  :name "VivaceGraph graph-algorithms IO"
  :description "Optional GML/Pajek import + Graphviz export for graph-db/algorithms."
  :depends-on (:graph-db/algorithms :cl-ppcre :yacc :dso-lex :parse-number
               :trivial-shell)
  :pathname "algorithms/"
  :serial t
  :components ((:file "io")))

(defsystem graph-db/algorithms-test
  :name "VivaceGraph graph-algorithms test suite"
  :description "FiveAM tests for graph-db/algorithms."
  :depends-on (:graph-db/algorithms :graph-db/algorithms-io :fiveam)
  :pathname "tests/algorithms/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "fixtures")
               (:file "fib-heap-tests")
               (:file "shortest-path-tests")
               (:file "structure-tests")
               (:file "ranking-tests")
               (:file "projection-tests")
               (:file "dense-tests")
               (:file "flow-tests")
               (:file "generation-tests")
               (:file "io-tests")
               (:file "prolog-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/algorithms-test
                                              :run-algorithm-tests)
                      (error "graph-db algorithm tests failed."))))

;; OPTIONAL GEOS add-on: a thin in-house CFFI binding to libgeos_c giving the
;; spatial layer exact polygon topology, validity repair, and distance.  Core
;; graph-db does NOT depend on this; loading it is what flips *geos-available-p*.
;; Loads gracefully (no crash) when libgeos_c is absent.
(defsystem graph-db/geos
  :name "VivaceGraph GEOS integration"
  :description "Optional libgeos_c binding: exact spatial topology + validity repair."
  :depends-on (:graph-db :cffi :bordeaux-threads)
  :pathname "geos/"
  :serial t
  :components ((:file "geos-ffi")
               (:file "geos-context")
               (:file "geos-bridge")
               (:file "geos-ops")))

(defsystem graph-db/geos-test
  :name "VivaceGraph GEOS test suite"
  :description "FiveAM tests for the optional GEOS integration."
  :depends-on (:graph-db/geos :fiveam :bordeaux-threads)
  :pathname "tests/geos/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "load-tests")
               (:file "bridge-tests")
               (:file "ops-tests")
               (:file "query-tests")
               (:file "makevalid-tests")
               (:file "overlay-tests")
               (:file "storm-tests")
               (:file "oracle-tests")
               (:file "perf-bench"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/geos-test :run-geos-tests)
                      (error "graph-db GEOS tests failed."))))

(defsystem graph-db/test
  :name "VivaceGraph test suite"
  :description "FiveAM unit tests for graph-db."
  :depends-on (:graph-db :fiveam :drakma)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "serialize-tests")
               (:file "geometry-tests")
               (:file "geometry-ops-tests")
               (:file "geohash-tests")
               (:file "allocator-tests")
               (:file "spatial-index-tests")
               (:file "linear-hash-tests")
               (:file "skip-list-tests")
               (:file "index-list-tests")
               (:file "type-index-tests")
               (:file "graph-tests")
               (:file "type-mapping-tests")
               (:file "graph-spatial-tests")
               (:file "spatial-hook-tests")
               (:file "spatial-query-tests")
               (:file "spatial-intersect-tests")
               (:file "subset-replication-tests")
               (:file "peer-lamport-tests")
               (:file "peer-merge-tests")
               (:file "peer-merge-apply-tests")
               (:file "peer-rehome-tests")
               (:file "peer-conflict-tests")
               (:file "view-tests")
               (:file "query-tests")
               (:file "prolog-mutation-tests")
               (:file "prolog-functor-tests")
               (:file "spatial-prolog-tests")
               (:file "traverse-tests")
               (:file "write-path-tests")
               (:file "reopen-tests")
               (:file "backup-tests")
               (:file "mvcc-tests")
               (:file "rest-tests")
               (:file "rest-http-tests")
               (:file "prolog-stress-tests")
               (:file "memory-graph-tests")
               (:file "unique-constraint-tests")
               (:file "peer-unique-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/test :run-tests)
                      (error "graph-db test suite failed."))))
