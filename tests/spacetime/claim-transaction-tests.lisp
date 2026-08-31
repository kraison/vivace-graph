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

(test a-claim-predating-the-axis-reports-indeterminate-not-the-epoch
  "⚠ The whole migration story rests on this.  NIL must never read as the
epoch: a fabricated audit time is worse than an admitted unknown (#148)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (with-transaction ()
        (let ((copy (graph-db::copy c)))
          ;; The raw slot, which is exactly what an old on-disk node has.
          (setf (claim-transaction-extent-sexp copy) nil)
          (graph-db::save copy)))
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
