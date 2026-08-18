;;;; The transaction-time axis on the claim record (GH #148).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test a-transaction-extent-round-trips-through-the-slot
  "The second axis reuses the validity axis's codec, so a bug fixed in one
is fixed in both (design, The record)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((e (make-interval (exact-bound (local-time:now))
                            (unknown-bound)
                            :semantics :transaction
                            :standing :asserted)))
      (with-transaction ()
        (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent copy) e)
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (is (eq :interval (extent-kind (claim-transaction-extent c2))))
          (is (eq :transaction
                  (extent-semantics (claim-transaction-extent c2))))
          (is (eq :asserted
                  (extent-standing (claim-transaction-extent c2)))))))))

(test the-two-axes-are-independent
  "⚠ Both slots hold the same sexp shape, so an accessor that read the
wrong one would decode perfectly and be invisible.  This pins which slot
each accessor touches."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((validity (make-interval (exact-bound (local-time:now))
                                   (unknown-bound)
                                   :semantics :validity))
          (txn (make-interval (exact-bound (local-time:now))
                              (unknown-bound)
                              :semantics :transaction
                              :standing :asserted)))
      (with-transaction () (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-extent copy) validity)
            (setf (claim-transaction-extent copy) txn)
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (is (eq :validity (extent-semantics (claim-extent c2))))
          (is (eq :transaction
                  (extent-semantics (claim-transaction-extent c2)))))))))

(test claim-recorded-at-reports-the-timestamp-and-the-standing
  "The common case is a point; without this every consumer reaches through
EXTENT-START into BOUND-EARLIEST."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((then (local-time:now)))
      (with-transaction () (make-u :subject "s1"))
      (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-transaction-extent copy)
                  (make-interval (exact-bound then) (unknown-bound)
                                 :semantics :transaction
                                 :standing :asserted))
            (graph-db::save copy)))
        (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
          (multiple-value-bind (ts standing) (claim-recorded-at c2)
            (is (local-time:timestamp= then ts))
            (is (eq :asserted standing))))))))

(test a-transaction-extent-survives-a-close-and-reopen
  "⚠ The in-session read is not the test.  The node cache has made two
tests in this programme vacuous by serving the right answer from memory."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (then (local-time:now)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u :subject "s1"))
               (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
                 (with-transaction ()
                   (let ((copy (graph-db::copy c)))
                     (setf (claim-transaction-extent copy)
                           (make-interval (exact-bound then) (unknown-bound)
                                          :semantics :transaction
                                          :standing :asserted))
                     (graph-db::save copy)))))
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
  "Nothing a tenant does leaves a new claim unstamped (design, Stamping)."
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
comparison to actually discriminate.  Must stay post-2000: GRAPH-DB's
TIMESTAMP codec corrupts any pre-epoch (pre-2000-03-01) date on the
read side (kraison/vivace-graph#153).  Do not 'tidy' this back to a
more dramatic historical date before #153 is fixed -- it would
reintroduce a silent failure, not a more interesting test."
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
                             :relation :r :producer :p
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
                             :relation :r :producer :p
                             :standing :inferred
                             :recorded-at (local-time:now)
                             :transaction-extent
                             (make-interval (exact-bound (local-time:now))
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted))))))

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
                           :relation :r :producer :p
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
                           :relation :r :producer :p
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
                           :relation :r :producer :p
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
                             :relation :r :producer :p
                             :standing :inferred
                             :recorded-at nil
                             :transaction-extent
                             (make-interval (exact-bound (local-time:now))
                                            (unknown-bound)
                                            :semantics :transaction
                                            :standing :asserted))))))
