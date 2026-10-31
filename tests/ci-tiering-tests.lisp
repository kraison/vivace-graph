;;;; The fast CI tier's suite exclusion (GH #340, docs/ci.md).
;;;;
;;;; These guard the mechanism, not the tiering policy.  They must hold
;;;; in BOTH tiers, so nothing here may assume a particular suite is
;;;; currently attached: in a fast-tier run the slow ones are detached
;;;; while these very tests execute.

(in-package #:graph-db/test)

(def-suite ci-tiering-suite :in graph-db-suite
  :description "Excluding slow suites from a run, exactly and reversibly.")
(in-suite ci-tiering-suite)

(defun %excludable-victim ()
  "Some child suite of GRAPH-DB-SUITE that is safe to detach and
reattach here: not one the current run may already have excluded, and
not this suite.  Chosen from the live list so the test works in either
tier."
  (find-if (lambda (n)
             (and (not (member n *slow-suites*))
                  (not (eq n 'ci-tiering-suite))
                  (typep (get-test n) 'fiveam::test-suite)))
           (suite-children 'graph-db-suite)))

(test excluding-a-suite-hides-it-then-puts-it-back
  "The restore half is the point: FIVEAM runs a suite's children from
one mutable list, so a failed restore is invisible until a later run is
quietly missing tests."
  (let ((victim (%excludable-victim)))
    (is-true victim "no child suite available to exercise exclusion")
    (when victim
      (let ((before (suite-children 'graph-db-suite)))
        (with-suites-excluded ((list victim))
          (let ((during (suite-children 'graph-db-suite)))
            (is (zerop (count victim during)))
            ;; Counted, not decremented by one: FIVEAM appends a name
            ;; per registration, so a name can appear more than once
            ;; (lispci/fiveam#94) and every occurrence must go.
            (is (= (- (length before) (count victim before))
                   (length during)))))
        (is (equal before (suite-children 'graph-db-suite)))))))

(test an-exclusion-is-restored-after-a-non-local-exit
  "A body that signals must not leave the child list short."
  (let ((victim (%excludable-victim)))
    (when victim
      (let ((before (suite-children 'graph-db-suite)))
        (ignore-errors
         (with-suites-excluded ((list victim))
           (error "deliberate")))
        (is (equal before (suite-children 'graph-db-suite)))))))

(test excluding-a-name-that-is-not-a-child-is-refused
  "A typo, or a suite renamed out from under *SLOW-SUITES*, must fail
loudly rather than exclude nothing and merely look fast."
  (signals error
    (with-suites-excluded ('(no-such-suite-in-this-image)) nil)))

(test every-slow-suite-names-a-real-suite
  "*SLOW-SUITES* drifts as suites are renamed.  Asks whether each name
still resolves to a suite, NOT whether it is currently attached -- in a
fast-tier run these are detached while this test runs."
  (dolist (s *slow-suites*)
    (is (typep (get-test s) 'fiveam::test-suite)
        "~A is in *SLOW-SUITES* but names no test suite" s)))
