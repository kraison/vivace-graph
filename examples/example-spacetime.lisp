;;;; A runnable walkthrough of graph-db/spacetime: claims with temporal
;;;; extents (manual Chapter 18).  Load top to bottom in a fresh image.
;;;;
;;;; The subsystem models STATE SERIES: "region r1 was in state A during
;;;; [Jan..Mar], then B, then A again" -- several live claims sharing one
;;;; (producer, subject, relation, object) tuple, whose validity extents
;;;; must be pairwise disjoint (GH #296).  Extents come from
;;;; cl-temporal-extent: endpoints are BOUNDS ([earliest, latest]
;;;; ranges), so imprecision and open-endedness are first-class.

(ql:quickload :graph-db)
(ql:quickload :graph-db/spacetime)
(ql:quickload :graph-db/rules)

(defpackage #:spacetime-example
  (:use #:cl #:graph-db #:graph-db.spacetime)
  (:import-from #:local-time #:encode-timestamp #:+utc-zone+))
(in-package #:spacetime-example)

;; Type-ids come from the registry in *SYSTEM-DIRECTORY*; a store cannot
;; be opened without one (GH #186).
;; Scratch goes under GRAPH_DB_EXAMPLE_ROOT when set (tests/load-examples.sh
;; gives each run a private directory), else /var/tmp/.
(defvar *example-root*
  (or (uiop:getenv "GRAPH_DB_EXAMPLE_ROOT") "/var/tmp/"))
(setf *system-directory*
      (namestring
       (ensure-directories-exist
        (concatenate 'string *example-root* "spacetime-example-system/"))))

;;; Schema.  DEF-CLAIM-CLASSES defines a claim family: a parent class
;;; plus UNARY and BINARY subclasses, with identity, standing and
;;; canonical-name constraints installed.  :TEMPORAL T adds the validity
;;; extent's START to the identity tuple and enforces pairwise-disjoint
;;; extents per base tuple at commit.
(def-claim-classes region-state :spacetime-example :temporal t)
;; The store's RULE record and DERIVATION family, for the rules section
;; at the end; a schema form, so it precedes MAKE-GRAPH too.
(graph-db.rules:def-rules-schema :spacetime-example)

;; SETQ, not DEFVAR: GRAPH-DB:*GRAPH* is already bound (to NIL), and
;; DEFVAR would skip the init form entirely.
(setq *graph*
      (make-graph :spacetime-example
                  (concatenate 'string *example-root* "spacetime-example/")
                  :buffer-pool-size 1000))

;;; Two small helpers (the same shapes the test suite uses).
(defun ts (year month day)
  "A UTC timestamp, so nothing here depends on the host timezone."
  (encode-timestamp 0 0 0 0 day month year :timezone +utc-zone+))

(defun run-extent (from to)
  "An interval extent with exact endpoints."
  (make-interval (exact-bound from) (exact-bound to)))

;;; A state series: r1 was in state "a", then "b", then "a" again.
;;; Each run is one binary claim; the extents are disjoint.  Constructors
;;; run inside WITH-TRANSACTION like every other mutation.
(defun assert-run (state from to)
  (make-region-state-binary
   :subject-namespace :region :subject-key "r1"
   :relation "in-state"
   :object-namespace :state :object-key state
   :producer "ingest" :standing :observed
   :extent (run-extent from to)))

(with-transaction ()
  (assert-run "a" (ts 2022 1 1) (ts 2022 3 31))
  (assert-run "b" (ts 2022 4 1) (ts 2022 6 30))
  (assert-run "a" (ts 2022 7 1) (ts 2022 9 30)))

;;; Reads.  CLAIMS-TOUCHING answers from the claim graph's own indexes.
;;; :AT keeps claims whose extent possibly contains an instant; :DURING
;;; keeps claims intersecting a window; :CURRENT drops retracted ones.

;; Which state was r1 in on 2022-02-15?  => the first "a" run.
(claims-touching *graph* 'region-state :region "r1"
                 :role :subject :at (ts 2022 2 15))

;; Every run intersecting Q2 2022.  => "b", plus nothing else.
(claims-touching *graph* 'region-state :region "r1"
                 :role :subject
                 :during (run-extent (ts 2022 4 1) (ts 2022 6 30)))

;;; Overlap is refused at commit -- but only within one BASE TUPLE
;;; (producer, subject, relation, object).  A "b" run overlapping an "a"
;;; run is two different tuples and is fine; a second "a" run overlapping
;;; the existing "a" run signals EXTENT-DISJOINTNESS-VIOLATION, and
;;; nothing of the refused transaction survives.
(handler-case
    (with-transaction ()
      (assert-run "a" (ts 2022 8 1) (ts 2022 12 31)))
  (extent-disjointness-violation (c)
    (format t "~&Refused, as it should be:~%~a~%" c)))

;;; Extending an ONGOING run is an update, not a new claim: the identity
;;; is the base tuple plus the extent START, so rewriting the same run
;;; with a later end is COPY -> SETF -> SAVE of one claim.
(let ((run (first (claims-touching *graph* 'region-state :region "r1"
                                   :role :subject
                                   :at (ts 2022 8 1)))))
  (with-transaction ()
    (let ((k (copy run)))
      (setf (claim-extent k) (run-extent (ts 2022 7 1) (ts 2022 10 31)))
      (save k))))

;;; Closing a run for good: RETRACT-CLAIM stamps the claim's transaction
;;; axis; a retracted run stops counting for :CURRENT reads and no longer
;;; blocks an overlapping successor.
(let ((run (first (claims-touching *graph* 'region-state :region "r1"
                                   :role :subject
                                   :at (ts 2022 5 1)))))
  (with-transaction ()
    (retract-claim run)))

;; :CURRENT now sees two live runs; the plain read still returns all
;; three -- retracted claims are the record of what was believed.
(length (claims-touching *graph* 'region-state :region "r1"
                         :role :subject :current t))
(length (claims-touching *graph* 'region-state :region "r1"
                         :role :subject))

;;; Bounds are ranges, so "sometime in March" needs no special case:
;;; an extent may start at an uncertain bound and end open.
(with-transaction ()
  (make-region-state-binary
   :subject-namespace :region :subject-key "r2"
   :relation "in-state"
   :object-namespace :state :object-key "a"
   :producer "ingest" :standing :observed
   :extent (make-interval
            (make-bound (ts 2023 3 1) (ts 2023 3 31)) ; started in March
            (unknown-bound))))                        ; not known to end

;;; Rules and forward evaluation (graph-db/rules; docs/rules.md) -------
;;;
;;; GRAPH-DB/RULES reads claims as Prolog facts.  A RULE is a record in
;;; the store: a name, a version, the family it derives into, one head
;;; claim pattern and a guarded Prolog body -- the text RUN-GUARDED-PROLOG
;;; accepts, so no colon and no package anywhere in it, namespaces as the
;;; lowercase wire strings.  DEF-RULES-SCHEMA (with the schema, above)
;;; declared the RULE record and the DERIVATION family on this store.
;;;
;;; This rule flags a region while it is in state "b".  CLAIM-CURRENT on
;;; the premise is what makes a retraction lose the flag later: currency
;;; is the body's business (docs/rules.md, "Forward evaluation").
(with-transaction ()
  (graph-db.rules:make-rule
   :graph *graph* :name "flag-b" :version "1" :family "region-state"
   :head "(claim ?d region-state \"region\" ?r \"flagged\"
                 \"flag\" \"state-b\")"
   :body "(claim ?c region-state \"region\" ?r \"in-state\" \"state\" \"b\")
          (claim-current ?c)"))

;;; Pull: RUN-RULES derives every rule to its fixpoint and reconciles
;;; the result with the rule's previous derivation.  r1's "b" run was
;;; retracted above, so nothing is flagged yet.
(graph-db.rules:run-rules *graph*)
(claims-by-producer *graph* 'region-state "rule/flag-b")   ; => NIL

;;; Push: once a store opts in, every commit's claim writes become a
;;; delta and the rules that read what changed run from it, after the
;;; commit returns, on the committing thread; nothing calls RUN-RULES.
;;; The opt-in lives in the image: reopen the store, opt in again.
(graph-db.rules:enable-forward-rules *graph*)

(with-transaction ()
  (make-region-state-binary
   :subject-namespace :region :subject-key "r2"
   :relation "in-state"
   :object-namespace :state :object-key "b"
   :producer "ingest" :standing :observed
   :extent (run-extent (ts 2023 4 1) (ts 2023 6 30))))

;; The derivation is a claim like any other, by producer "rule/flag-b",
;; with a DERIVED-FROM record naming its premise.
(claims-by-producer *graph* 'region-state "rule/flag-b")
;; => one claim: (:region "r2") flagged (:flag "state-b")

;;; Retracting the premise: the commit's delta carries a -1 for it, the
;;; flag is re-derived with its head bound and, its premise no longer
;;; current, RETRACTED -- never deleted.  The record of what was
;;; believed stays readable without :CURRENT.
(let ((b-run (find "b" (claims-touching *graph* 'region-state
                                        :region "r2" :role :subject
                                        :relation "in-state")
                   :key #'claim-object-key :test #'string=)))
  (with-transaction ()
    (retract-claim b-run)))

(remove-if-not #'claim-current-p
               (claims-by-producer *graph* 'region-state "rule/flag-b"))
;; => NIL: the flag went with its premise

;; What forward evaluation is doing: the queue, a running step, the
;; failures a step reported (a refused rule never stops the others).
(graph-db.rules:forward-rules-status *graph*)
;; => (:enabled t :drain :commit :queue-depth 0 :running-p nil
;;     :failures nil ...)

;;; Productions -- a rule whose head is a Lisp action instead of a claim,
;;; with salience -- and :DRAIN :CALLER, which keeps the committing
;;; thread out of the step, are docs/rules.md "Forward evaluation".

;;; The change feed.  CLAIMS-TOUCHED-AFTER reads the family's touched-at
;;; index: every claim whose transaction record moved after a cursor
;;; (created, retracted or re-opened), ascending by that instant, so a
;;; consumer that keeps the last instant it saw reads what changed, not
;;; the namespace's history (GH #392).  Everything this walkthrough
;;; wrote is after 2020, the derived flag included.
(claims-touched-after *graph* 'region-state (ts 2020 1 1))

;; The same feed from Prolog.  Written as the three goals below, the
;; compiler fuses the pair (claim-touched-at ?c ?t) (instant> ?t cursor)
;; onto the index and the claim goal filters each row, so this costs the
;; claims touched after the cursor, in touched order.
(select (:max-inferences 10000) (?c ?t)
  (claim ?c region-state "region" ?r "in-state" ?ons ?ok)
  (claim-touched-at ?c ?t)
  (instant> ?t "2020-01-01T00:00:00Z"))

(graph-db.rules:disable-forward-rules *graph*)

(close-graph *graph*)
