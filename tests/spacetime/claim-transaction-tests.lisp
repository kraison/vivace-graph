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
