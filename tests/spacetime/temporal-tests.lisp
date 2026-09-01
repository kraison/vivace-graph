;;;; Temporal claim families (GH #296): the extent start in the identity
;;;; tuple, pairwise-disjoint validity per base tuple, membership per
;;;; instant, and the :AT / :DURING reads.  Design:
;;;; docs/superpowers/specs/2026-09-01-temporal-claim-families-design.md.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

;; A second family on the claim test graph, this one temporal.
(def-claim-classes tt-claim :graph-db-claim-test :temporal t)

(defun %tt-run (subject state from to &key (producer "series")
                                          (relation "in-state"))
  "SUBJECT was in STATE from FROM to TO (exact bounds), as a binary claim."
  (make-tt-claim-binary :subject-namespace :region :subject-key subject
                        :relation relation
                        :object-namespace :state :object-key state
                        :producer producer :standing :observed
                        :extent (exact-interval from to)))

(defun %tt-series (subject)
  "The acceptance series A -> B -> A, three runs, one relation."
  (%tt-run subject "a" (ts 2022 1 1) (ts 2022 3 31))
  (%tt-run subject "b" (ts 2022 4 1) (ts 2022 6 30))
  (%tt-run subject "a" (ts 2022 7 1) (ts 2022 9 30)))

(defun %tt-runs (g subject &rest keys)
  (apply #'claims-touching g 'tt-claim :region subject :role :subject keys))

(defun %tt-day-end (year month day)
  "The last nanosecond of the UTC day -- how a day-granular run ends."
  (nth-value 1 (granule-bounds (ts year month day) :day)))

;;; --- the declaration -----------------------------------------------------

(test a-temporal-family-widens-its-identity-tuple
  "GH #296 §2.1-2.2: the flag is on the registry, and both arities' named
identity constraints now end in EXTENT-SEXP.  The non-temporal family on
the same graph is untouched."
  (is-true (claim-family-temporal-p (claim-family 'tt-claim)))
  (is-false (claim-family-temporal-p (claim-family 'ct-claim)))
  (flet ((tuple (owner)
           (let ((spec (find-if
                        (lambda (s)
                          (eq owner (graph-db::unique-tuple-spec-owner-name
                                     s)))
                        (gethash :graph-db-claim-test
                                 graph-db::*schema-unique-metadata*))))
             (and spec (graph-db::unique-tuple-spec-slot-names spec)))))
    (is (equal '(graph-db.spacetime::producer
                 graph-db.spacetime::subject-namespace
                 graph-db.spacetime::subject-key
                 graph-db.spacetime::object-namespace
                 graph-db.spacetime::object-key
                 graph-db.spacetime::relation
                 graph-db.spacetime::extent-sexp)
               (tuple 'tt-claim-binary)))
    (is (equal '(graph-db.spacetime::producer
                 graph-db.spacetime::subject-namespace
                 graph-db.spacetime::subject-key
                 graph-db.spacetime::relation
                 graph-db.spacetime::extent-sexp)
               (tuple 'tt-claim-unary)))
    (is (= 6 (length (tuple 'ct-claim-binary))))))

(test the-start-key-is-fixnums-never-timestamp-objects
  "The memory backend's unique index is an EQUAL hash table, and two
TIMESTAMP structs are never EQUAL; the key must be built from fixnums."
  (let* ((e (exact-interval (ts 2022 1 1) (ts 2022 3 31)))
         (k (extent-sexp-start-key (extent->sexp e))))
    (is (equal k (extent-sexp-start-key
                  (extent->sexp (exact-interval (ts 2022 1 1)
                                                (ts 2022 12 31)))))
        "same start, different end: same key")
    (is (not (equal k (extent-sexp-start-key
                       (extent->sexp (exact-interval (ts 2022 1 2)
                                                     (ts 2022 3 31)))))))
    (labels ((fixnums-or-unbounded-p (x)
               (or (eq x :unbounded)
                   (and (consp x) (every (lambda (i) (typep i 'fixnum)) x))
                   (and (consp x) (every #'fixnums-or-unbounded-p x)))))
      (is-true (fixnums-or-unbounded-p k)))
    (is (equal '(:unbounded :unbounded)
               (extent-sexp-start-key
                (extent->sexp (make-interval (unknown-bound)
                                             (exact-bound (ts 2022 1 1)))))))
    (is (null (extent-sexp-start-key nil)))))

(test a-temporal-claim-without-an-extent-is-refused-twice
  "At construction (the identity check) and at commit (the named
:REQUIRED constraint), so the raw-slot write paths cannot leave a
temporal claim under no identity at all."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals missing-claim-identity-component
      (with-transaction ()
        (make-tt-claim-binary :subject-namespace :region :subject-key "r0"
                              :relation "in-state"
                              :object-namespace :state :object-key "a"
                              :producer "series" :standing :observed)))
    (let (c)
      (with-transaction ()
        (setq c (%tt-run "r0" "a" (ts 2022 1 1) (ts 2022 3 31))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((k (graph-db:copy c)))
            (setf (claim-extent-sexp k) nil)
            (graph-db:save k)))))))

;;; --- the series ----------------------------------------------------------

(test a-recurring-series-is-three-claims-with-one-relation
  "The acceptance case: A -> B -> A, one relation, one object key per
state, disjoint extents.  Three live claims, two of them sharing the
whole base tuple."
  (with-claim-graph (g)
    (finishes (with-transaction () (%tt-series "r1")))
    (let ((runs (%tt-runs g "r1")))
      (is (= 3 (length runs)))
      (is (every #'claim-current-p runs))
      (is (= 2 (count "a" runs :key #'claim-object-key :test #'equal))))))

(test an-overlapping-run-is-refused-with-its-own-condition
  "A fourth run overlapping the third is refused at commit with
EXTENT-DISJOINTNESS-VIOLATION -- distinct and catchable -- naming the
base tuple and the claims that would overlap.  Nothing survives."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r2"))
    (let ((e (handler-case
                 (progn
                   (with-transaction ()
                     (%tt-run "r2" "a" (ts 2022 9 15) (ts 2022 12 31)))
                   nil)
               (extent-disjointness-violation (c) c))))
      (is-true e "the overlapping run is refused")
      (when e
        (is (eq 'tt-claim (edv-claim-class e)))
        (is (equal "r2" (edv-subject-key e)))
        (is (equal "a" (edv-object-key e)))
        (is (equal "in-state" (edv-relation e)))
        (is (= 2 (length (edv-conflicting-ids e))))))
    (is (= 3 (length (%tt-runs g "r2"))) "nothing of the refusal survived")))

(test two-overlapping-runs-in-one-transaction-are-refused
  "Neither is in the store yet; only the transaction's own creates can
show the overlap (the view's other edge, as membership)."
  (with-claim-graph (g)
    (signals extent-disjointness-violation
      (with-transaction ()
        (%tt-run "r3" "a" (ts 2022 1 1) (ts 2022 3 31))
        (%tt-run "r3" "a" (ts 2022 3 1) (ts 2022 5 31))))
    (is (null (%tt-runs g "r3")))))

(test a-same-start-rewrite-updates-the-ongoing-run
  "Why the START and not the whole extent is the identity: a run the
subject is still in is rewritten by each ingest with a later end.  That
is an UPDATE of one claim (COPY, SETF CLAIM-EXTENT, SAVE), never a new
identity -- and a NEW claim with the same start collides on identity
(UNIQUE-CONSTRAINT-VIOLATION), the ordinary 'same claim twice' refusal."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r4"))
    (let ((third (find-if (lambda (c)
                            (equal (extent-sexp-start-key
                                    (claim-extent-sexp c))
                                   (extent-sexp-start-key
                                    (extent->sexp
                                     (exact-interval (ts 2022 7 1)
                                                     (ts 2022 9 30))))))
                          (%tt-runs g "r4"))))
      (is-true third)
      (finishes
        (with-transaction ()
          (let ((k (graph-db:copy third)))
            (setf (claim-extent k)
                  (exact-interval (ts 2022 7 1) (ts 2022 10 31)))
            (graph-db:save k))))
      (let ((runs (%tt-runs g "r4")))
        (is (= 3 (length runs)) "an update, not a fourth claim")
        (is (some (lambda (c)
                    (timestamp= (ts 2022 10 31)
                                (bound-latest
                                 (extent-end (claim-extent c)))))
                  runs)
            "the end moved"))
      (signals graph-db:unique-constraint-violation
        (with-transaction ()
          (%tt-run "r4" "a" (ts 2022 7 1) (ts 2022 11 30)))))))

(test a-retracted-run-does-not-block-its-successor
  "The transaction axis is untouched (GH #148/#162): a retracted run is
not live, so an overlapping successor is admitted -- and the retracted
run's identity is still occupied, exactly as any retracted claim's is."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r5"))
    (let ((b (find "b" (%tt-runs g "r5") :key #'claim-object-key
                                         :test #'equal)))
      (retract-claim b)
      (finishes
        (with-transaction ()
          (%tt-run "r5" "b" (ts 2022 4 15) (ts 2022 5 15))))
      (is (= 4 (length (%tt-runs g "r5"))))
      (is (= 3 (length (%tt-runs g "r5" :current t))))
      (signals graph-db:unique-constraint-violation
        (with-transaction ()
          (%tt-run "r5" "b" (ts 2022 4 1) (ts 2022 6 30)))
        "the retracted run still holds its (base tuple, start)"))))

(test retract-then-assert-in-one-transaction-is-atomic-for-a-run
  "The membership idiom applies to a run: closing a run and asserting
its overlapping replacement in ONE transaction commits as a unit,
because the check reads post-commit state through the view."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r6"))
    (let ((b (find "b" (%tt-runs g "r6") :key #'claim-object-key
                                         :test #'equal)))
      (finishes
        (with-transaction ()
          (retract-claim b)
          (%tt-run "r6" "b" (ts 2022 4 10) (ts 2022 6 20))))
      (is (= 3 (length (%tt-runs g "r6" :current t)))))))

(test disjointness-is-per-base-tuple
  "Another object key, another producer, another relation: a different
base tuple, so the extents may overlap freely.  (Across object keys,
DEF-DISJOINT-MEMBERSHIP is the constraint -- see below.)"
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (%tt-run "r7" "a" (ts 2022 1 1) (ts 2022 3 31))
        (%tt-run "r7" "b" (ts 2022 2 1) (ts 2022 4 30))
        (%tt-run "r7" "a" (ts 2022 2 1) (ts 2022 4 30)
                 :producer "other")
        (%tt-run "r7" "a" (ts 2022 2 1) (ts 2022 4 30)
                 :relation "was-in-state")))))

;;; --- what disjoint means -------------------------------------------------

(test meeting-runs-share-an-instant-and-are-not-disjoint
  "Intervals are closed, so two exact runs that MEET hold at the same
instant -- refused.  Day-granular runs that end at the last nanosecond of
the day never meet the next day's start."
  (is-false (extents-disjoint-p (exact-interval (ts 2022 1 1) (ts 2022 4 1))
                                (exact-interval (ts 2022 4 1) (ts 2022 7 1))))
  (is-true (extents-disjoint-p
            (make-interval (exact-bound (ts 2022 1 1))
                           (exact-bound (%tt-day-end 2022 3 31)))
            (exact-interval (ts 2022 4 1) (ts 2022 7 1))))
  (is-true (extents-disjoint-p (exact-interval (ts 2022 4 1) (ts 2022 7 1))
                               (exact-interval (ts 2022 1 1) (ts 2022 3 1)))
           "order does not matter")
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals extent-disjointness-violation
      (with-transaction ()
        (%tt-run "r8" "a" (ts 2022 1 1) (ts 2022 4 1))
        (%tt-run "r8" "a" (ts 2022 4 1) (ts 2022 7 1))))))

(test an-ambiguous-pair-is-refused
  "A fuzzy bound that MIGHT overlap its predecessor is not disjoint: the
algebra's rule -- definite only when no choice within either range gives
another answer -- applied as written.  An instant is refused inside a run
and admitted outside it."
  (is-false (extents-disjoint-p
             (exact-interval (ts 2022 1 1) (ts 2022 3 31))
             (make-interval (make-bound (ts 2022 3 1) (ts 2022 4 30))
                            (exact-bound (ts 2022 6 30)))))
  (is-true (extents-disjoint-p
            (exact-interval (ts 2022 1 1) (ts 2022 3 31))
            (make-interval (make-bound (ts 2022 4 1) (ts 2022 4 30))
                           (exact-bound (ts 2022 6 30)))))
  (is-false (extents-disjoint-p
             (exact-interval (ts 2022 1 1) (ts 2022 3 31))
             (make-instant (exact-bound (ts 2022 2 1)))))
  (is-true (extents-disjoint-p
            (exact-interval (ts 2022 1 1) (ts 2022 3 31))
            (make-instant (exact-bound (ts 2022 4 1)))))
  (is-false (extents-disjoint-p (exact-interval (ts 2022 1 1) (ts 2022 3 31))
                                nil)
            "no extent overlaps everything -- the predicate is total"))

;;; --- membership per instant ----------------------------------------------

(defmacro %tt-with-membership (&body body)
  `(unwind-protect
        (progn
          (%mb-clear)
          (def-disjoint-membership tt-claim :graph-db-claim-test
            :relation "in-state"
            :object-namespace :state
            :object-keys ("a" "b" "c")
            :name tt-states)
          ,@body)
     (%mb-clear)))

(test membership-disjointness-holds-per-instant-over-a-series
  "GH #296 §2.4: a region may be A during [a,b] and B during [c,d] with
both claims live -- the transaction-time-only reading would have refused
the second run.  A B that overlaps an A is still refused, with the
membership condition, and the audit finds a pre-existing overlap."
  (%tt-with-membership
    (with-claim-graph (g)
      (finishes (with-transaction () (%tt-series "r9")))
      (is (= 3 (length (%tt-runs g "r9" :current t))))
      (let ((e (handler-case
                   (progn
                     (with-transaction ()
                       (%tt-run "r9" "c" (ts 2022 3 15) (ts 2022 4 15)))
                     nil)
                 (membership-disjointness-violation (c) c))))
        (is-true e "an overlapping other-state run is refused")
        (when e
          (is (eq 'tt-states (mdv-name e)))
          (is (member "c" (mdv-members e) :test #'equal))
          (is (= 3 (length (mdv-members e)))
              "the members reported are the overlapping ones")))
      (finishes
        (with-transaction ()
          (%tt-run "r9" "c" (ts 2022 10 1) (ts 2022 12 31)))))))

(test the-membership-audit-is-per-instant-for-a-temporal-family
  (with-claim-graph (g)
    (%mb-clear)
    (with-transaction ()
      (%tt-series "r10")
      (%tt-series "r11")
      (%tt-run "r11" "c" (ts 2022 5 1) (ts 2022 5 31)))
    (unwind-protect
         (progn
           (def-disjoint-membership tt-claim :graph-db-claim-test
             :relation "in-state" :object-namespace :state
             :object-keys ("a" "b" "c") :name tt-states)
           (multiple-value-bind (violations checked specs)
               (check-disjoint-memberships g)
             (is (= 1 (length violations)))
             (is (equal "r11" (third (first violations))))
             (is (= 2 (length (fourth (first violations))))
                 "only the overlapping pair, not every run")
             (is (plusp checked))
             (is (= 1 specs))))
      (%mb-clear))))

;;; --- the reads -----------------------------------------------------------

(test claims-touching-at-returns-exactly-the-covering-run
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r12"))
    (let ((may (%tt-runs g "r12" :at (ts 2022 5 10))))
      (is (= 1 (length may)))
      (is (equal "b" (claim-object-key (first may)))))
    (is (equal "a" (claim-object-key
                    (first (%tt-runs g "r12" :at (ts 2022 1 1)))))
        "a closed interval contains its start")
    (is (equal "a" (claim-object-key
                    (first (%tt-runs g "r12" :at (ts 2022 9 30)))))
        "and its end")
    (is (null (%tt-runs g "r12" :at (ts 2021 12 31))))
    (is (null (%tt-runs g "r12" :at (ts 2022 10 1))))
    (is (= 1 (length (claims-touching g 'tt-claim :state "a"
                                      :role :object :at (ts 2022 8 1))))
        "the filter composes with :ROLE")))

(test claims-touching-during-returns-the-intersecting-runs
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r13"))
    (is (= 3 (length (%tt-runs g "r13"
                               :during (exact-interval (ts 2022 3 1)
                                                       (ts 2022 8 1))))))
    (is (= 1 (length (%tt-runs g "r13"
                               :during (exact-interval (ts 2022 4 1)
                                                       (ts 2022 4 2))))))
    (is (= 2 (length (%tt-runs g "r13"
                               :during (exact-interval (ts 2021 1 1)
                                                       (ts 2022 5 1)))))
        "a window wider than the series is fine")
    (is (null (%tt-runs g "r13"
                        :during (exact-interval (ts 2023 1 1)
                                                (ts 2023 12 31)))))))

(test the-validity-filters-compose-with-current
  "\"What was believed about day D, as of now\" is :AT plus :CURRENT --
the two axes stay orthogonal (GH #148)."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r14"))
    (let ((b (find "b" (%tt-runs g "r14") :key #'claim-object-key
                                          :test #'equal)))
      (retract-claim b)
      (is (= 1 (length (%tt-runs g "r14" :at (ts 2022 5 10))))
          "without :CURRENT the retracted run is still the record")
      (is (null (%tt-runs g "r14" :at (ts 2022 5 10) :current t))))))

(test the-validity-filters-signal-on-a-wrong-type
  (with-claim-graph (g)
    (signals error (%tt-runs g "r15" :at "2022-05-10"))
    (signals error (%tt-runs g "r15" :during (ts 2022 5 10)))))

(test a-non-temporal-family-reads-the-same-filters
  "On a family without the flag the filters still apply -- and a claim
with no extent makes no validity statement, so it is excluded."
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :subject "plain" :object "o1")
      (make-b :subject "plain" :object "o2"
              :extent (exact-interval (ts 2022 1 1) (ts 2022 12 31))))
    (is (= 2 (length (claims-touching g 'ct-claim :ns "plain"))))
    (let ((at (claims-touching g 'ct-claim :ns "plain" :at (ts 2022 6 1))))
      (is (= 1 (length at)))
      (is (equal "o2" (claim-object-key (first at)))))
    (is (null (claims-touching g 'ct-claim :ns "plain"
                               :at (ts 2023 6 1))))))

;;; --- the audit, and persistence -----------------------------------------

(test the-extent-audit-finds-a-pre-existing-overlap
  "Runs written before the family was temporal, or through a path that
skipped the check: the audit is how they are found."
  (with-claim-graph (g)
    (with-transaction () (%tt-series "r16"))
    ;; Write an overlap past the validator, the way legacy data would
    ;; arrive: with the check disabled.
    (let ((graph-db:*commit-validators*
            (remove 'graph-db.spacetime::%validate-extent-disjointness
                    graph-db:*commit-validators*)))
      (with-transaction ()
        (%tt-run "r16" "a" (ts 2022 9 1) (ts 2022 12 31))))
    (multiple-value-bind (violations checked)
        (check-extent-disjointness g 'tt-claim)
      (is (= 1 (length violations)))
      (is (equal "r16" (getf (first violations) :subject-key)))
      (is (= 2 (length (getf (first violations) :ids))))
      (is (= 4 checked)))
    (is (null (check-extent-disjointness g 'ct-claim))
        "a non-temporal family has nothing to audit")))

(test the-temporal-identity-survives-a-reopen
  "The unique index over the start key is persisted and rebuilt like any
DEF-UNIQUE: after close and open, the same start still collides and a
disjoint later run is still admitted."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (%tt-run "r17" "a" (ts 2022 1 1) (ts 2022 3 31))))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (signals graph-db:unique-constraint-violation
                 (with-transaction ()
                   (%tt-run "r17" "a" (ts 2022 1 1) (ts 2022 6 30))))
               (signals extent-disjointness-violation
                 (with-transaction ()
                   (%tt-run "r17" "a" (ts 2022 2 1) (ts 2022 6 30))))
               (finishes
                 (with-transaction ()
                   (%tt-run "r17" "a" (ts 2022 4 1) (ts 2022 6 30))))
               (is (= 2 (length (%tt-runs g2 "r17")))))
          (close-graph g2)
          (collect-garbage))))))
