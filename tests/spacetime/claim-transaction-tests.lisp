;;;; The transaction-time axis on the claim record (GH #148).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test a-transaction-extent-round-trips-through-the-slot
  "The second axis reuses the validity axis's codec, so a bug fixed in one
is fixed in both (design, The record).  Given via the initarg, not SETF
afterwards -- Task 4 makes the slot write-once (GH #148); the overwrite
case is OVERWRITING-A-STAMP-IS-REFUSED, below."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((e (make-interval (exact-bound (local-time:now))
                            (unknown-bound)
                            :semantics :transaction
                            :standing :asserted)))
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation "r" :producer "rule-a"
                             :standing :inferred
                             :transaction-extent e))
      (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (eq :interval (extent-kind (claim-transaction-extent c2))))
        (is (eq :transaction
                (extent-semantics (claim-transaction-extent c2))))
        (is (eq :asserted
                (extent-standing (claim-transaction-extent c2))))))))

(test the-two-axes-are-independent
  "⚠ Both slots hold the same sexp shape, so an accessor that read the
wrong one would decode perfectly and be invisible.  This pins which slot
each accessor touches.  TXN arrives via the initarg (write-once, Task 4);
VALIDITY still goes through SETF afterwards because CLAIM-EXTENT carries
no such guard."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((validity (make-interval (exact-bound (local-time:now))
                                   (unknown-bound)
                                   :semantics :validity))
          (txn (make-interval (exact-bound (local-time:now))
                              (unknown-bound)
                              :semantics :transaction
                              :standing :asserted)))
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation "r" :producer "rule-a"
                             :standing :inferred
                             :transaction-extent txn))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-extent copy) validity)
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (is (eq :validity (extent-semantics (claim-extent c2))))
          (is (eq :transaction
                  (extent-semantics (claim-transaction-extent c2)))))))))

(test claim-recorded-at-reports-the-timestamp-and-the-standing
  "The common case is a point; without this every consumer reaches through
EXTENT-START into BOUND-EARLIEST.  Stamped via :RECORDED-AT at
construction -- Task 4 makes the slot write-once (GH #148)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((then (local-time:now)))
      (with-transaction () (make-u-at :subject "s1" :recorded-at then))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (multiple-value-bind (ts standing) (claim-recorded-at c)
          (is (local-time:timestamp= then ts))
          (is (eq :asserted standing)))))))

(test a-transaction-extent-survives-a-close-and-reopen
  "⚠ The in-session read is not the test.  The node cache has made two
tests in this programme vacuous by serving the right answer from memory.
Stamped via :RECORDED-AT at construction -- Task 4 makes the slot
write-once (GH #148)."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (then (local-time:now)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (make-u-at :subject "s1" :recorded-at then)))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((c (first (claims-touching g2 'ct-claim :ns "s1"))))
                 (is (local-time:timestamp= then (claim-recorded-at c)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

(test every-new-claim-is-stamped-without-the-tenant-asking
  "Nothing a tenant does leaves a new claim built via MAKE-<class> unstamped
(design, Stamping)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing)))
      (let ((e (claim-transaction-extent c)))
        (is (eq :interval (extent-kind e)))
        (is (eq :transaction (extent-semantics e)))
        (is (eq :unbounded (bound-latest (extent-end e))))))))

(test recorded-at-overrides-the-default-stamp
  "⚠ THEN is deliberately ~25 years from now: a stamp that ignored the
argument and used the clock would still produce a valid timestamp and
would pass a weaker assertion -- only a THEN this far off forces the
comparison to actually discriminate.  The date was originally chosen
post-2000 to dodge the codec defect this unit found; #153 has since
fixed that, so the constraint is now only distance from now."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((then (local-time:parse-timestring "2001-06-15T12:00:00Z")))
      (with-transaction () (make-u-at :subject "s1" :recorded-at then))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (local-time:timestamp= then (claim-recorded-at c)))))))

(test an-explicit-transaction-extent-is-stored-as-given
  "An ingest path may know a CLOSED period, or a standing other than
:ASSERTED."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((e (make-interval
              (exact-bound (local-time:parse-timestring "2001-01-01T00:00:00Z"))
              (exact-bound (local-time:parse-timestring "2002-01-01T00:00:00Z"))
              :semantics :transaction :standing :indeterminate)))
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation "r" :producer "p"
                             :standing :inferred
                             :transaction-extent e))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (eq :indeterminate
                (extent-standing (claim-transaction-extent c))))
        (is (not (eq :unbounded
                     (bound-latest
                      (extent-end (claim-transaction-extent c))))))))))

(test conflicting-transaction-initargs-signal
  "Picking one silently is how a caller ends up with a stamp they did not
ask for; parity with :EXTENT versus :EXTENT-SEXP (claim.lisp)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals error
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation "r" :producer "p"
                             :standing :inferred
                             :recorded-at (local-time:now)
                             :transaction-extent
                             (make-interval (exact-bound (local-time:now))
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted))))))

;;; --- The stamp is write-once (GH #148) ---

(test overwriting-a-stamp-is-refused
  "Transaction time is an audit field.  ⚠ Accessor-level only -- writing
CLAIM-TRANSACTION-EXTENT-SEXP directly still bypasses this, and that limit
is recorded in the design and in #148.  Via the sanctioned write path
(COPY inside WITH-TRANSACTION, then SETF) so an ablated guard leaves
nothing to signal, rather than tripping the engine's own copy-before-
write check first (review finding, GH #148)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals transaction-extent-immutable
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent copy)
                  (make-interval (exact-bound (local-time:now))
                                 (unknown-bound)
                                 :semantics :transaction
                                 :standing :asserted))
            (graph-db::save copy)))))))

(test a-refused-overwrite-leaves-the-original-stamp
  "⚠ The refusal is only half of it; the store must still hold the
original.  Reopened, not read from the node cache.  THEN was moved
post-2000 to dodge the codec defect this unit found, which surfaced here
on the snapshot/close path rather than at DESERIALIZE; #153 has since
fixed it, so any date works."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (then (local-time:parse-timestring "2003-07-04T16:20:11Z")))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u-at :subject "s1"
                                               :recorded-at then))
               (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
                 (handler-case
                     (with-transaction ()
                       (let ((copy (graph-db::copy c)))
                         (setf (claim-transaction-extent copy)
                               (graph-db.spacetime::%open-transaction-extent
                                (local-time:now)))
                         (graph-db::save copy)))
                   (transaction-extent-immutable () nil))))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((c (first (claims-touching g2 'ct-claim :ns "s1"))))
                 (is (local-time:timestamp= then (claim-recorded-at c)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

;;; --- A NIL value on a transaction key means "nothing to say", not
;;; "leave this unstamped" (GH #148 review) ---

(test a-nil-transaction-extent-still-stamps
  "Passing :TRANSACTION-EXTENT NIL is not the same as omitting it: the
tenant is saying nothing about transaction time, and the substrate's
answer is still the default stamp."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                           :relation "r" :producer "p"
                           :standing :inferred
                           :transaction-extent nil))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing))))))

(test a-nil-recorded-at-still-stamps
  "Same guarantee via :RECORDED-AT NIL."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                           :relation "r" :producer "p"
                           :standing :inferred
                           :recorded-at nil))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing))))))

(test a-nil-transaction-extent-sexp-still-stamps
  "Same guarantee via :TRANSACTION-EXTENT-SEXP NIL -- the raw slot
initarg, closest to how a legacy pre-#148 claim would arrive."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                           :relation "r" :producer "p"
                           :standing :inferred
                           :transaction-extent-sexp nil))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing))))))

(test nil-value-on-one-key-still-conflicts-with-another-key-present
  "Conflict detection counts KEY PRESENCE, not VALUE -- so this must keep
signalling even though :RECORDED-AT's value is NIL.  Pins the fix from
weakening into counting values instead of keys."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals error
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                             :relation "r" :producer "p"
                             :standing :inferred
                             :recorded-at nil
                             :transaction-extent
                             (make-interval (exact-bound (local-time:now))
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted))))))

;;; --- A claim predating the axis is indeterminate, not the epoch
;;; (GH #148) ---

(defmacro with-legacy-stamps (&body body)
  "Run BODY with CT-CLAIM's transaction-stamp transition withdrawn, so a
test can fabricate a claim written before the axis -- raw slot NIL --
which is exactly the write #158 now refuses on every live path.  The
declaration is re-emitted after, by its NAME, in DEF-CLAIM-CLASSES's
package (GH #152: a same-named symbol elsewhere withdraws nothing)."
  `(unwind-protect
        (progn
          (graph-db:undef-value-constraint ct-claim :graph-db-claim-test
            :name graph-db.spacetime::transaction-extent-transition)
          ,@body)
     (graph-db:def-value-constraint ct-claim
         graph-db.spacetime::transaction-extent-sexp :graph-db-claim-test
       :transition graph-db.spacetime::transaction-extent-step
       :name graph-db.spacetime::transaction-extent-transition)))

(test a-claim-predating-the-axis-reports-indeterminate-not-the-epoch
  "⚠ The whole migration story rests on this.  NIL must never read as the
epoch: a fabricated audit time is worse than an admitted unknown (#148)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (with-legacy-stamps
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            ;; The raw slot, which is exactly what an old on-disk node has.
            (setf (claim-transaction-extent-sexp copy) nil)
            (graph-db::save copy))))
      (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (null (claim-transaction-extent c2)))
        (multiple-value-bind (ts standing) (claim-recorded-at c2)
          (is (null ts))
          (is (eq :indeterminate standing))
          (is (not (eq :observed standing))))))))

;;; --- Both arities are stamped (GH #148) ---

(test a-binary-claim-is-stamped-too
  "Every test above is unary.  Task 4's write-once guard was declared
once on the parent and reached BINARY only through SUBTYPEP (design,
Testing #9) -- a suite exercising only MAKE-U looked complete and was
not."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (multiple-value-bind (ts standing) (claim-recorded-at c)
        (is (typep ts 'local-time:timestamp))
        (is (eq :asserted standing))))))

;;; --- CLAIM-RECORDED-AT on a non-exact start bound (GH #148) ---

(test claim-recorded-at-on-a-non-exact-start-is-the-earliest-edge
  "⚠ CLAIM-RECORDED-AT returns (BOUND-EARLIEST (EXTENT-START E)) raw.
Pinning, not fixing -- altering the return shape is work for a
later unit (#148).  An UNBOUNDED start reports the keyword
:UNBOUNDED; a fuzzy (non-exact) start silently reports its earliest
edge, discarding the latest.  Both are legal :TRANSACTION-EXTENT
values -- exactly what an imprecise ingest source time produces."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (make-ct-claim-unary :subject-namespace :ns :subject-key "s1"
                           :relation "r" :producer "p"
                           :standing :inferred
                           :transaction-extent
                           (make-interval (unknown-bound) (unknown-bound)
                                          :semantics :transaction
                                          :standing :asserted)))
    (with-transaction ()
      (make-ct-claim-unary :subject-namespace :ns :subject-key "s2"
                           :relation "r" :producer "p"
                           :standing :inferred
                           :transaction-extent
                           (make-interval
                             (make-bound (ts 2020 1 1) (ts 2020 6 1))
                             (unknown-bound)
                             :semantics :transaction :standing :asserted)))
    (let ((c1 (first (claims-touching g 'ct-claim :ns "s1")))
          (c2 (first (claims-touching g 'ct-claim :ns "s2"))))
      (multiple-value-bind (recorded standing) (claim-recorded-at c1)
        (is (eq :unbounded recorded))
        (is (eq :asserted standing)))
      (multiple-value-bind (recorded standing) (claim-recorded-at c2)
        (is (local-time:timestamp= (ts 2020 1 1) recorded))
        (is (eq :asserted standing))))))

;;; --- Retraction: closing the transaction period (GH #162) ---------------

(test retracting-a-claim-closes-its-period-and-keeps-it
  "GH #162.  A retracted claim is the record of what was believed until
when: its transaction period closes at AT, the recorded-at start survives,
and the claim is still there -- CLAIMS-TOUCHING returns it unless :CURRENT
filters it.  Not a deletion."
  (with-claim-graph (g)
    ;; RECORDED-AT pinned in the past: with a wall-clock stamp this test
    ;; is a time bomb -- a fixed :AT of noon fails every afternoon with
    ;; "END precedes START".
    (with-transaction () (make-u-at :subject "s1"
                                    :recorded-at (ts 2026 1 1)))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1")))
          (at (ts 2026 8 31 12)))
      (is-true (claim-current-p c) "a fresh claim is current")
      (retract-claim c :at at)
      (let* ((r (first (claims-touching g 'ct-claim :ns "s1")))
             (e (claim-transaction-extent r)))
        (is-false (claim-current-p r))
        (is (eq :transaction (extent-semantics e)))
        (is-true (bound-exact-p (extent-start e))
                 "the recorded-at start survives the close")
        (is-true (bound-exact-p (extent-end e)))
        (is-true (local-time:timestamp= at (bound-earliest (extent-end e)))
                 "closed exactly at AT")
        (is (= 1 (length (claims-touching g 'ct-claim :ns "s1")))
            "retracted, not deleted")
        (is (null (claims-touching g 'ct-claim :ns "s1" :current t))
            ":CURRENT filters it out")))))

(test retracting-a-claim-that-predates-the-axis-closes-from-unknown
  "GH #162 meets #148's absence rule: a claim with no stamp is current
(never retracted) and closes as [unknown, AT) -- the start is not
fabricated."
  (with-claim-graph (g)
    (with-transaction () (make-u :subject "s2"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s2"))))
      (with-legacy-stamps
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent-sexp copy) nil)
            (graph-db::save copy))))
      (let ((legacy (first (claims-touching g 'ct-claim :ns "s2"))))
        (is (null (claim-transaction-extent legacy)) "fixture: unstamped")
        (is-true (claim-current-p legacy) "absence is not retraction")
        (retract-claim legacy :at (ts 2026 8 31 12))
        (let ((e (claim-transaction-extent
                  (first (claims-touching g 'ct-claim :ns "s2")))))
          (is-true (bound-unknown-p (extent-start e)))
          (is-true (bound-exact-p (extent-end e))))))))

(test retracting-twice-leaves-the-first-close-standing
  "GH #162.  A second retraction is a no-op, not a later close: the
period ended when belief ended, and a sweep that runs daily must not
walk the end forward."
  (with-claim-graph (g)
    (with-transaction () (make-u-at :subject "s3"
                                    :recorded-at (ts 2026 1 1)))
    (let ((c (first (claims-touching g 'ct-claim :ns "s3")))
          (first-at (ts 2026 8 31 12))
          (later (ts 2026 9 1 12)))
      (retract-claim c :at first-at)
      (retract-claim (first (claims-touching g 'ct-claim :ns "s3"))
                     :at later)
      (let ((e (claim-transaction-extent
                (first (claims-touching g 'ct-claim :ns "s3")))))
        (is-true (local-time:timestamp= first-at
                                        (bound-earliest (extent-end e))))))))

;;; --- The stamp is enforced at commit, not only at the accessor (GH #158) --

(test the-stamp-cannot-be-cleared-or-moved-at-commit
  "GH #158.  Before this the accessor refused and the raw slot did not; a
COPY/SETF/SAVE -- or a REST put -- could clear or rewrite any claim's
audit field.  Now TRANSACTION-EXTENT-STEP refuses both at commit."
  (with-claim-graph (g)
    (with-transaction () (make-u :subject "s4"))
    (let* ((c (first (claims-touching g 'ct-claim :ns "s4")))
           (stamp (claim-transaction-extent-sexp c)))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent-sexp copy) nil)
            (graph-db::save copy))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent-sexp copy)
                  (extent->sexp (graph-db.spacetime::%open-transaction-extent
                                 (ts 2020 1 1))))
            (graph-db::save copy))))
      (is (equal stamp (claim-transaction-extent-sexp
                        (first (claims-touching g 'ct-claim :ns "s4"))))
          "the stamp is exactly what it was"))))

(test transaction-extent-step-admits-exactly-the-substrate-s-own-moves
  "The rule, as a table.  (NIL x) is a stamp or a legacy claim being
stamped; open -> closed same start is RETRACT-CLAIM; closed -> open no
earlier than the close is re-assertion.  Everything else is refused."
  (let* ((t1 (ts 2026 1 1)) (t2 (ts 2026 6 1)) (t3 (ts 2026 9 1))
         (open-t1 (extent->sexp
                   (graph-db.spacetime::%open-transaction-extent t1)))
         (open-t2 (extent->sexp
                   (graph-db.spacetime::%open-transaction-extent t2)))
         (closed-t1-t2 (extent->sexp
                        (make-interval (exact-bound t1) (exact-bound t2)
                                       :semantics :transaction
                                       :standing :asserted)))
         (closed-t2-t3 (extent->sexp
                        (make-interval (exact-bound t2) (exact-bound t3)
                                       :semantics :transaction
                                       :standing :asserted)))
         (open-t3 (extent->sexp
                   (graph-db.spacetime::%open-transaction-extent t3))))
    (is-true (transaction-extent-step nil open-t1) "a stamp")
    (is-true (transaction-extent-step open-t1 closed-t1-t2) "a retraction")
    (is-false (transaction-extent-step open-t1 closed-t2-t3)
              "a close that moves the start")
    (is-true (transaction-extent-step closed-t1-t2 open-t2)
             "re-asserted at the close")
    (is-true (transaction-extent-step closed-t1-t2 open-t3)
             "re-asserted later")
    (is-false (transaction-extent-step closed-t1-t2 open-t1)
              "re-asserted BEFORE the close: a rewrite of history")
    (is-false (transaction-extent-step open-t1 nil) "clearing")
    (is-false (transaction-extent-step open-t1 open-t2) "moving the start")
    (is-false (transaction-extent-step closed-t1-t2 closed-t2-t3)
              "re-closing")
    (is-false (transaction-extent-step open-t1 '(:junk)) "junk is refused")))

