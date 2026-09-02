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

(defpackage #:spacetime-example
  (:use #:cl #:graph-db #:graph-db.spacetime)
  (:import-from #:local-time #:encode-timestamp #:+utc-zone+))
(in-package #:spacetime-example)

;; Type-ids come from the registry in *SYSTEM-DIRECTORY*; a store cannot
;; be opened without one (GH #186).
(setf *system-directory*
      (namestring
       (ensure-directories-exist "/var/tmp/spacetime-example-system/")))

;;; Schema.  DEF-CLAIM-CLASSES defines a claim family: a parent class
;;; plus UNARY and BINARY subclasses, with identity, standing and
;;; canonical-name constraints installed.  :TEMPORAL T adds the validity
;;; extent's START to the identity tuple and enforces pairwise-disjoint
;;; extents per base tuple at commit.
(def-claim-classes region-state :spacetime-example :temporal t)

;; SETQ, not DEFVAR: GRAPH-DB:*GRAPH* is already bound (to NIL), and
;; DEFVAR would skip the init form entirely.
(setq *graph*
      (make-graph :spacetime-example "/var/tmp/spacetime-example/"
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

(close-graph *graph*)
