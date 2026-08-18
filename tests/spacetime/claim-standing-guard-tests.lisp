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
               (ignore-errors
                (with-transaction ()
                  (let ((c (graph-db::copy
                            (first (claims-touching g 'ct-claim
                                                    :ns "s1")))))
                    (setf (claim-standing c) :nonsense)
                    (graph-db::save c)))))
          (ignore-errors (close-graph g))
          (collect-garbage)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is-true
                (standingp
                 (claim-standing
                  (first (claims-touching g2 'ct-claim :ns "s1"))))
                "an invalid standing reached disk and survived a reopen"))
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
