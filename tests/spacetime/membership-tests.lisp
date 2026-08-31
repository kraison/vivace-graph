;;;; Membership disjointness (GH #157 4b): declaration, the commit check
;;;; through the view, atomic retract-then-assert, the audit.
;;;; Design: the 4b addendum in
;;;; docs/superpowers/specs/2026-08-30-disjointness-design.md.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun %mb-specs ()
  (gethash :graph-db-claim-test
           graph-db.spacetime::*membership-disjointness-metadata*))

(defun %mb-clear ()
  (setf (gethash :graph-db-claim-test
                 graph-db.spacetime::*membership-disjointness-metadata*)
        nil))

(defmacro %mb-with-decl (&body body)
  `(unwind-protect
        (progn
          (%mb-clear)
          (def-disjoint-membership ct-claim :graph-db-claim-test
            :relation "instance-of"
            :object-namespace :classes
            :object-keys ("observation" "fortification" "unclassified")
            :name mb-classes)
          ,@body)
     (%mb-clear)))

(defun %mb-member (subject class-key)
  "A live membership claim: SUBJECT is-a CLASS-KEY."
  (make-ct-claim-binary :subject-namespace :ns :subject-key subject
                        :relation "instance-of"
                        :object-namespace :classes :object-key class-key
                        :producer "mb-test" :standing :observed))

;;; --- declaration ---------------------------------------------------------

(test a-membership-declaration-canonicalises-and-requires-a-name
  (%mb-clear)
  (def-disjoint-membership ct-claim :graph-db-claim-test
    :relation "instance-of" :object-namespace :classes
    :object-keys ("fortification" "observation")
    :name mb-classes)
  (def-disjoint-membership ct-claim :graph-db-claim-test
    :relation "instance-of" :object-namespace :classes
    :object-keys ("observation" "fortification")
    :name mb-classes)
  (is (= 1 (length (%mb-specs))) "either order is one declaration")
  (is (equal '("fortification" "observation")
             (graph-db.spacetime::mds-object-keys (first (%mb-specs)))))
  (signals error
    (eval '(graph-db.spacetime:def-disjoint-membership ct-claim
            :graph-db-claim-test
            :relation "instance-of" :object-namespace :classes
            :object-keys ("a" "b"))))
  (signals error
    (eval '(graph-db.spacetime:def-disjoint-membership ct-claim
            :graph-db-claim-test
            :relation :instance-of :object-namespace :classes
            :object-keys ("a" "b") :name mb-kw)))
  (is (= 1 (length (%mb-specs))))
  (is-true (undef-disjoint-membership ct-claim :graph-db-claim-test
                                      :name mb-classes))
  (is (null (%mb-specs))))

;;; --- the commit check ----------------------------------------------------

(test a-second-live-membership-is-refused-with-its-own-condition
  "GH #157 4b.  The refusal is MEMBERSHIP-DISJOINTNESS-VIOLATION -- the
tenant's condition 3: distinct and catchable, never folded into
VALUE-CONSTRAINT-VIOLATION."
  (%mb-with-decl
    (with-claim-graph (g)
      (declare (ignorable g))
      (finishes (with-transaction () (%mb-member "s1" "observation")))
      (let ((e (handler-case
                   (progn (with-transaction ()
                            (%mb-member "s1" "fortification"))
                          nil)
                 (membership-disjointness-violation (c) c))))
        (is-true e "the second live membership is refused")
        (when e
          (is (eq 'mb-classes (mdv-name e)))
          (is (equal "s1" (mdv-subject-key e)))
          (is (= 2 (length (mdv-members e))))))
      (is (= 1 (length (remove-if-not
                        (lambda (c) (equal "instance-of"
                                           (claim-relation c)))
                        (claims-touching g 'ct-claim :ns "s1"
                                         :current t))))
          "the refused write left one live membership"))))

(test reclassification-is-one-atomic-retract-then-assert
  "GH #157 4b, the tenant's condition 2.  RETRACT-CLAIM joins the
transaction, and the commit check counts POST-commit state through the
view, so the retracted sibling does not block the new assertion."
  (%mb-with-decl
    (with-claim-graph (g)
      (declare (ignorable g))
      (with-transaction () (%mb-member "s2" "observation"))
      (let ((old (first (remove-if-not
                         (lambda (c) (equal "instance-of"
                                            (claim-relation c)))
                         (claims-touching g 'ct-claim :ns "s2")))))
        (finishes
          (with-transaction ()
            (retract-claim old)
            (%mb-member "s2" "fortification")))
        (let ((live (remove-if-not
                     (lambda (c) (equal "instance-of"
                                        (claim-relation c)))
                     (claims-touching g 'ct-claim :ns "s2"
                                      :current t))))
          (is (= 1 (length live)))
          (is (equal "fortification"
                     (claim-object-key (first live)))))))))

(test a-refused-reclassification-rolls-the-retraction-back-too
  "The atomicity payoff: when the ASSERT half of a reclassification is
refused, the RETRACT half must not have committed alone -- the subject
keeps its old class rather than ending up classless."
  (%mb-with-decl
    (with-claim-graph (g)
      (declare (ignorable g))
      (with-transaction ()
        (%mb-member "s3" "observation")
        (%mb-member "s3b" "fortification"))
      (let ((old (first (remove-if-not
                         (lambda (c) (and (equal "instance-of"
                                                 (claim-relation c))
                                          (equal "s3"
                                                 (claim-subject-key c))))
                         (claims-touching g 'ct-claim :ns "s3")))))
        (signals graph-db:value-constraint-violation
          (with-transaction ()
            (retract-claim old)
            ;; the new claim is INVALID (non-canonical relation), so the
            ;; whole transaction -- retraction included -- must abort
            (make-ct-claim-binary :subject-namespace :ns
                                  :subject-key "s3"
                                  :relation :Not-Canonical
                                  :object-namespace :classes
                                  :object-key "fortification"
                                  :producer "mb-test"
                                  :standing :observed)))
        (is-true (claim-current-p
                  (first (remove-if-not
                          (lambda (c) (equal "s3" (claim-subject-key c)))
                          (claims-touching g 'ct-claim :ns "s3"
                                           :current t))))
                 "the retraction rolled back with the refused assert")))))

(test two-memberships-created-in-one-transaction-are-refused
  "The overlay's other edge: neither claim is in the store yet, so only
the transaction's own creates can show the conflict."
  (%mb-with-decl
    (with-claim-graph (g)
      (declare (ignorable g))
      (signals membership-disjointness-violation
        (with-transaction ()
          (%mb-member "s4" "observation")
          (%mb-member "s4" "unclassified")))
      (is (null (claims-touching g 'ct-claim :ns "s4"))
          "nothing survived the refused commit"))))

(test claims-outside-the-declared-set-are-untouched
  "A different relation, a different object namespace, or an object key
outside the set: none participates, and none is refused."
  (%mb-with-decl
    (with-claim-graph (g)
      (declare (ignorable g))
      (finishes
        (with-transaction ()
          (%mb-member "s5" "observation")
          ;; same subject, other relation
          (make-ct-claim-binary :subject-namespace :ns :subject-key "s5"
                                :relation "registered-at"
                                :object-namespace :classes
                                :object-key "fortification"
                                :producer "mb-test" :standing :observed)
          ;; same relation, key outside the set
          (make-ct-claim-binary :subject-namespace :ns :subject-key "s5"
                                :relation "instance-of"
                                :object-namespace :classes
                                :object-key "vehicle"
                                :producer "mb-test" :standing :observed)
          ;; same relation+key, other object namespace
          (make-ct-claim-binary :subject-namespace :ns :subject-key "s5"
                                :relation "instance-of"
                                :object-namespace :other
                                :object-key "fortification"
                                :producer "mb-test" :standing :observed))))))

;;; --- the audit -----------------------------------------------------------

(test the-membership-audit-finds-a-pre-existing-double
  "Claims written before the declaration existed: the commit never saw
them together, so only the audit can."
  (with-claim-graph (g)
    (declare (ignorable g))
    (%mb-clear)
    (with-transaction ()
      (%mb-member "s6" "observation")
      (%mb-member "s7" "fortification"))
    (with-transaction () (%mb-member "s6" "unclassified"))
    (def-disjoint-membership ct-claim :graph-db-claim-test
      :relation "instance-of" :object-namespace :classes
      :object-keys ("observation" "fortification" "unclassified")
      :name mb-classes)
    (multiple-value-bind (violations checked specs)
        (check-disjoint-memberships g)
      (is (= 1 (length violations)))
      (is (equal "s6" (third (first violations))))
      (is (= 2 (length (fourth (first violations)))))
      (is (plusp checked))
      (is (= 1 specs)))
    (%mb-clear)))
