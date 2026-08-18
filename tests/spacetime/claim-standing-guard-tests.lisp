;;;; The standing vocabulary is enforced on the UPDATE path, not only at
;;;; construction.  Probe and rationale: GH #149.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test an-invalid-standing-is-refused-on-the-update-path
  "⚠ THE REGRESSION TEST FOR #149.  CHECK-STANDING fires inside the
generated MAKE-<NAME> wrapper only (claim.lisp:195), so COPY + SETF + SAVE
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
  "The constraint is declared once, on PARENT (claim.lisp:146-149), and
reaches BINARY only via SUBTYPEP -- unlike the unique constraints just
below it, which are declared per-arity on purpose (claim.lisp:165-171).
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
  "The narrow half of over-refusal: an explicitly re-set, currently-valid
standing is not rejected.  It does NOT pin the COPY-drop case -- this test
writes STANDING itself, so a COPY that lost the slot would be repaired by
the test's own SETF before SAVE ever saw it.  That case is the test below."
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

(test an-unrelated-update-leaves-the-standing-intact
  "⚠ The COPY-drop case, which the test above cannot reach.  The update
idiom every consumer actually uses mutates one slot and never mentions
STANDING, so the existing value has to survive COPY on its own -- if it
did not, :REQUIRED would refuse a legitimate write.  CONFIDENCE is chosen
because it is unrelated to the constraint (claim.lisp:42).  Proven RED by
stripping :STANDING in a COPY-NODE :AROUND *and* pinning HEAP-MERGED-P on
the copy -- stripping alone is not enough, MAYBE-INIT-NODE-DATA re-derives
DATA from the intact bytes.  See task-5-report.md."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (finishes
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-confidence copy) 0.5)
            (graph-db::save copy))))
      (let ((c2 (first (claims-touching g 'ct-claim :ns "s1"))))
        (is (eq :inferred (claim-standing c2))
            "an update that never mentioned STANDING lost it")
        ;; Without this the test would still pass if SAVE were a no-op.
        (is (eql 0.5 (claim-confidence c2))
            "the unrelated update did not actually commit")))))

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
