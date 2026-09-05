;;;; tests/rules/suite.lisp -- runner + fixture for graph-db/rules
;;;; (GH #330).

(in-package #:graph-db/rules-test)

(def-suite rules-suite :description "graph-db/rules (GH #304).")

(defun run-rules-tests ()
  "Run the suite; T when every check passed.  Invoked by
(asdf:test-system :graph-db/rules-test)."
  (log:config :error)
  ;; Type-ids come from the image-wide registry, so every store this run
  ;; opens needs one system directory (GH #186).
  (let* ((system-dir (graph-db-test-scratch:make-scratch-directory
                      "graph-db-rules-sys"))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'rules-suite)))
           (explain! results)
           (results-status results))
      (graph-db-test-scratch:cleanup-scratch-run))))

(defparameter *graph-name* :graph-db-rules-test)

;; This file owns the graph name; the clear is what lets a second file
;; claiming it be seen (GH #198).
(eval-when (:load-toplevel :execute)
  (setf (gethash *graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes rt-claim :graph-db-rules-test)
(def-claim-classes rtt-claim :graph-db-rules-test :temporal t)

;; Ruling F-R1: a run cannot collide with its own dedupe key, so spec
;; §11's unique refusal needs a constraint the STORE declared on top of
;; the family's own -- here one object per (subject, relation), which a
;; rule deriving two objects for one subject breaks.
(def-claim-classes rtu-claim :graph-db-rules-test)

(graph-db:def-unique rtu-claim-binary
    (graph-db.spacetime::subject-namespace
     graph-db.spacetime::subject-key
     graph-db.spacetime::relation)
  :graph-db-rules-test :name rtu-one-object-per-subject)

;; R22's skip path, committed: *CLAIM-FAMILIES* is image-wide, so a
;; family declared on a graph name this suite never opens is registered
;; in it and indexed in no graph here -- which is the case
;; %PRODUCER-CANDIDATES has to skip.  Nothing writes claims of it.
(eval-when (:load-toplevel :execute)
  (setf (gethash :graph-db-rules-test-foreign
                 graph-db::*schema-node-metadata*)
        nil))

(def-claim-classes rtf-claim :graph-db-rules-test-foreign)

;; T4-R2's store: claim classes but no DEF-RULES-SCHEMA, so the schema
;; carries no RULE vertex type at all.
(eval-when (:load-toplevel :execute)
  (setf (gethash :graph-db-rules-norule
                 graph-db::*schema-node-metadata*)
        nil))

(def-claim-classes rtn-claim :graph-db-rules-norule)

;; S2: the rule record and the derivation family on the test store
;; (spec §5, §9).
(graph-db.rules:def-rules-schema :graph-db-rules-test)

(defmacro with-rules-graph ((g) &body body)
  "A fresh on-disk graph named *GRAPH-NAME*, in a scratch directory."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (graph-db-test-scratch:make-scratch-directory
                   "graph-db-rules"))
            (,g (make-graph *graph-name* (namestring ,dir)
                            :buffer-pool-size 1000)))
       (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
         (ignore-errors (close-graph ,g))))))

(defmacro with-rules-graph-dir ((g dir) &body body)
  "WITH-RULES-GRAPH with the directory bound to DIR, for a test that
closes and reopens the store."
  `(let* ((,dir (graph-db-test-scratch:make-scratch-directory
                 "graph-db-rules"))
          (,g (make-graph *graph-name* (namestring ,dir)
                          :buffer-pool-size 1000)))
     (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g)))))

(defmacro with-norule-graph ((g) &body body)
  "A fresh store under the graph name DEF-RULES-SCHEMA never touched."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (graph-db-test-scratch:make-scratch-directory
                   "graph-db-rules-norule"))
            (,g (make-graph :graph-db-rules-norule (namestring ,dir)
                            :buffer-pool-size 1000)))
       (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
         (ignore-errors (close-graph ,g))))))

(defun seed-temporal (g)
  "Three deployments of web, by producer \"deploy\": h1 twice with a gap,
h2 once.  With SEED's two version runs these are the premises the S2
temporal rules intersect.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h1"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 2 1) (ts 2026 6 30)))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h1"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 8 1) (ts 2026 9 30)))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h2"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 5 1)
                                             (ts 2026 5 31)))))

(defun write-rule (g &rest args)
  "A RULE record written in its own transaction; ARGS are MAKE-RULE's
keywords.  Returns the record."
  (with-transaction ((graph-db::transaction-manager g))
    (apply #'graph-db.rules:make-rule :graph g args)))

(defun ts (y m d)
  "A UTC timestamp, so no test depends on the host timezone."
  (local-time:encode-timestamp 0 0 0 0 d m y
                               :timezone local-time:+utc-zone+))

(defun interval (from to)
  "The validity extent [FROM, TO], both endpoints exact."
  (make-interval (exact-bound from) (exact-bound to)
                 :semantics :validity :standing :asserted))

(defun seed (g)
  "Four rt-claims and two rtt-claims.  Returns nothing; tests query."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h1"
                          :relation "runs" :object-namespace :app
                          :object-key "web" :producer "scan-a"
                          :standing :observed)
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h1"
                          :relation "runs" :object-namespace :app
                          :object-key "db" :producer "scan-a"
                          :standing :observed)
    (make-rt-claim-binary :graph g :subject-namespace :host :subject-key "h2"
                          :relation "runs" :object-namespace :app
                          :object-key "web" :producer "scan-b"
                          :standing :observed)
    (make-rt-claim-unary :graph g :subject-namespace :host :subject-key "h2"
                         :relation "reachable" :producer "scan-b"
                         :standing :inferred)
    (make-rtt-claim-binary :graph g :subject-namespace :app :subject-key "web"
                           :relation "version" :object-namespace :ver
                           :object-key "1" :producer "scan-a"
                           :standing :observed
                           :extent (interval (ts 2026 1 1) (ts 2026 3 31)))
    (make-rtt-claim-binary :graph g :subject-namespace :app :subject-key "web"
                           :relation "version" :object-namespace :ver
                           :object-key "2" :producer "scan-a"
                           :standing :observed
                           :extent (interval (ts 2026 4 1) (ts 2026 12 31)))))
