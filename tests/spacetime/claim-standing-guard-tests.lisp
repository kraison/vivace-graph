;;;; The standing vocabulary is enforced on the UPDATE path, not only at
;;;; construction.  Probe and rationale: GH #149.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test an-invalid-standing-is-refused-on-the-update-path
  "⚠ THE REGRESSION TEST FOR #149.  CHECK-STANDING fires inside the
generated MAKE-<NAME> wrapper only (claim.lisp:177), so COPY + SETF + SAVE
committed an invalid standing and it survived a reopen."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) :nonsense)
            (graph-db::save copy)))))))

(test a-binary-claim-is-also-refused-on-the-update-path
  "The constraint is declared once, on PARENT (claim.lisp:135-138), and
reaches BINARY only via SUBTYPEP -- unlike the unique constraints just
below it, which are declared per-arity on purpose (claim.lisp:141-146).
A maintainer 'fixing' DEF-VALUE-CONSTRAINT to name UNARY by analogy with
those would pass every other test in this file while silently dropping
the guard on binary claims; this is what pins it."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b :subject "s1" :object "o1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) :nonsense)
            (graph-db::save copy)))))))

(test a-nil-standing-is-refused-on-the-update-path
  "The :REQUIRED half.  :ONE-OF alone exempts NIL, so without :REQUIRED this
write would still commit and the claim would carry no standing at all."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) nil)
            (graph-db::save copy)))))))

(test a-valid-standing-still-commits-on-the-update-path
  "Under-refusal (above) is only half the guard.  If COPY ever dropped a
slot from the data alist, STANDING would read NIL and :REQUIRED would
reject a perfectly legitimate update -- every consumer's normal update
idiom would break.  Nothing else in this file or in
value-constraint-tests.lisp exercises a VALID value through COPY + SETF +
SAVE under a live constraint; this is that test."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (finishes
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-standing copy) :asserted)
            (graph-db::save copy))))
      (is (eq :asserted
              (claim-standing
               (first (claims-touching g 'ct-claim :ns "s1"))))))))

(test the-refused-standing-is-not-durable
  "⚠ The in-session read is not the test.  The node cache has made two
earlier tests in this programme vacuous by serving the right answer from
memory; the probe on #149 only became decisive at line E, after a reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction () (make-u :subject "s1"))
               ;; Narrowed to the one condition tests 1-2 pin: a typo or a
               ;; different failure here would otherwise still read green.
               (handler-case
                   (with-transaction ()
                     (let ((c (graph-db::copy
                               (first (claims-touching g 'ct-claim
                                                       :ns "s1")))))
                       (setf (claim-standing c) :nonsense)
                       (graph-db::save c)))
                 (graph-db:value-constraint-violation () nil)))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               ;; EQ :INFERRED, not just STANDINGP -- the probe's line E
               ;; pinned the surviving value, not merely its validity.
               (is (eq :inferred
                       (claim-standing
                        (first (claims-touching g2 'ct-claim :ns "s1"))))
                   "the original standing did not survive a reopen"))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

(test every-standing-in-the-vocabulary-still-commits
  "⚠ A guard bought by refusing everything is not a guard.  If this fails,
the constraint and +STANDINGS+ have drifted apart -- which is the drift
:ONE-OF being evaluated exists to prevent."
  (with-claim-graph (g)
    (declare (ignorable g))
    (dolist (s +standings+)
      (finishes
        (with-transaction ()
          (make-ct-claim-unary :subject-namespace :ns
                               :subject-key (string s)
                               :relation :r :producer :p
                               :standing s))))))

(test construction-still-refuses-an-invalid-standing
  "CHECK-STANDING stays as a fast-fail with a better error site; it just
stops being the only thing there."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals invalid-standing
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s9"
                             :relation :r :producer :p9
                             :standing :nonsense)))))
