;;;; Absence-vs-value conformance (design §7.4).  A never-measured state must
;;;; never be confusable with a measured one -- the defect class with seven
;;;; confirmed instances that this subsystem exists to make unrepresentable.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test no-interval-is-not-a-zero-length-interval-at-the-epoch
  (let ((unknown (make-instant (unknown-bound) :standing :indeterminate))
        (epoch (make-instant (exact-bound (ts 1970 1 1))
                             :standing :observed)))
    (is-false (bound-exact-p (extent-start unknown)))
    (is-true (bound-exact-p (extent-start epoch)))
    (is-true (standing-absence-p (extent-standing unknown)))
    (is-false (standing-absence-p (extent-standing epoch)))
    (is-false (allen-definite-p unknown epoch))))

(test the-three-absence-reasons-survive-into-a-relation
  "A relation resting on SEARCHED-EMPTY and one resting on UNCOVERED must
not read the same downstream."
  (let ((known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (dolist (s '(:searched-empty :uncovered :indeterminate))
      (let ((r (allen-relations
                (make-interval (unknown-bound) (unknown-bound) :standing s)
                known)))
        (is-true (member s (temporal-relation-standings r))
                 "~S was lost from the relation" s)))))

(test an-unknown-extent-yields-relations-not-an-error-and-not-nil
  (let ((unknown (make-interval (unknown-bound) (unknown-bound)
                                :standing :uncovered))
        (known (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (finishes (allen-relations unknown known))
    (is-true (temporal-relation-relations (allen-relations unknown known)))))

(test no-accessor-returns-a-value-mistakable-for-a-measurement
  "PRECISION and SEMANTICS are metadata; STANDING says whether there is a
measurement at all.  None of them may default to something that reads as a
real observation."
  (let ((e (make-interval (unknown-bound) (unknown-bound)
                          :standing :uncovered)))
    (is (eq :uncovered (extent-standing e)))
    (is-false (standing-present-p (extent-standing e)))
    (is (eq :unbounded (bound-earliest (extent-start e))))
    (is (eq :unbounded (bound-latest (extent-end e))))))

(test standing-is-required-and-validated-at-construction
  "There is no way to build an extent with a standing that is not one of the
six -- the collapse is unrepresentable, not merely discouraged."
  (signals invalid-standing
    (make-interval (unknown-bound) (unknown-bound) :standing nil))
  (signals invalid-standing
    (make-interval (unknown-bound) (unknown-bound) :standing 0))
  (signals invalid-standing
    (make-instant (unknown-bound) :standing "observed")))

;;; The codec claims no core change is needed because EXTENT->SEXP emits
;;; only values GRAPH-DB:SERIALIZE already handles (design §6).  That is a
;;; claim about the real function, not about a structural predicate over
;;; the sexp shape -- so drive it through GRAPH-DB:SERIALIZE and
;;; GRAPH-DB:DESERIALIZE themselves and check the extent comes back whole.

(test an-interval-survives-graph-db-serialize-deserialize
  (let* ((e (make-interval (exact-bound (ts 2026 1 1))
                           (exact-bound (ts 2026 1 2))
                           :precision :day :semantics :validity
                           :standing :inferred))
         (wire (serialize (extent->sexp e)))
         (back (sexp->extent (deserialize wire))))
    (is (eq :interval (extent-kind back)))
    (is (eq :day (extent-precision back)))
    (is (eq :validity (extent-semantics back)))
    (is (eq :inferred (extent-standing back)))
    (is-true (timestamp= (ts 2026 1 1) (bound-earliest (extent-start back))))
    (is-true (timestamp= (ts 2026 1 1) (bound-latest (extent-start back))))
    (is-true (timestamp= (ts 2026 1 2) (bound-earliest (extent-end back))))
    (is-true (timestamp= (ts 2026 1 2) (bound-latest (extent-end back))))))

(test an-instant-survives-graph-db-serialize-deserialize-coupled
  "The round trip must not just preserve the timestamp -- it must preserve
the START/END identity that makes an instant an instant (design §3.3).
March, a DST-crossing month on a EU/EET host, is what this test caught the
GH #134 granule-end bug against originally -- keep it here rather than
retreat to a DST-safe month."
  (let* ((e (make-granule-instant (ts 2026 3 15) :month
                                  :semantics :event :standing :observed))
         (wire (serialize (extent->sexp e)))
         (back (sexp->extent (deserialize wire))))
    (is (eq :instant (extent-kind back)))
    (is (eq :month (extent-precision back)))
    (is (eq :event (extent-semantics back)))
    (is (eq :observed (extent-standing back)))
    (is (eq (extent-start back) (extent-end back)))
    (is-true (timestamp= (ts 2026 3 1) (bound-earliest (extent-start back))))
    (is (= 999999999 (local-time:nsec-of (bound-latest (extent-start back)))))
    (is (= 1775001599
           (local-time:timestamp-to-unix (bound-latest (extent-start back)))))))

(test a-unary-claim-is-distinguishable-from-an-unknown-object
  "Design §3.1 and §10.  Structural absence and epistemic absence must not
share a spelling -- the defect class this whole subsystem exists to prevent,
arriving in the first record built on top of it."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((unary (make-u))
            (unknown (make-ct-claim-binary
                      :subject-namespace :ns :subject-key "s9"
                      :relation :r :object-namespace :ns :object-key "?"
                      :producer :p :standing :indeterminate)))
        ;; Structural absence: the slot does not exist at all.
        (is-false (slot-exists-p unary 'graph-db.spacetime::object-key))
        ;; Epistemic absence: the slot exists, and STANDING says why.
        (is-true (slot-exists-p unknown 'graph-db.spacetime::object-key))
        (is-true (standing-absence-p (claim-standing unknown)))
        (is-false (standing-absence-p (claim-standing unary)))))))

(test a-claims-standing-and-its-extents-standing-are-independent
  "Design §5: one records how the claim came to be known, the other how the
TIME was known."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((c (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                                    :relation :r :producer :p
                                    :standing :asserted)))
        (setf (claim-extent c)
              (make-granule-instant (ts 2026 1 15) :day :standing :observed))
        (is (eq :asserted (claim-standing c)))
        (is (eq :observed (extent-standing (claim-extent c))))))))
