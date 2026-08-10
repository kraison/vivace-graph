;;;; Claim identity: what the constraint permits and forbids (design §6).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun make-b (&key (producer :rule-a) (subject "s1") (object "o1")
                    (relation :r) (standing :inferred) extent)
  (make-ct-claim-binary :subject-namespace :ns :subject-key subject
                        :relation relation
                        :object-namespace :ns :object-key object
                        :producer producer :standing standing
                        :extent extent))

(defun make-u (&key (producer :rule-a) (subject "s1") (relation :r) extent)
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred :extent extent))

(test two-producers-may-disagree
  "Design §6.2.  This is the entire reason for reifying: an edge model would
have to resolve this at write time."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-b :producer :rule-a :object "yes")
        (make-b :producer :operator-o :object "no")))))

(test one-producer-may-relate-a-subject-to-many-objects
  "Design §6.2 -- an ordinary one-to-many.  This is what breaks if the unary
constraint is wrongly declared on the PARENT class."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-b :object "o1")
        (make-b :object "o2")
        (make-b :object "o3")))))

(test the-same-producer-may-not-assert-the-identical-claim-twice
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-b)))))

(test a-unary-claim-deduplicates
  "THE test for the whole structural decision (design §3.1, §10).  With a
single class and a nullable object slot this FAILS: #107 exempts any tuple
containing a null, so the duplicate commits silently."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-u))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-u)))))

(test unary-and-binary-claims-do-not-collide
  "They are constrained separately, so a unary claim and a binary claim with
the same producer, subject and relation coexist."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-u :producer :rule-a :subject "s1" :relation :r)
        (make-b :producer :rule-a :subject "s1" :relation :r)))))

(test differing-in-any-identity-component-makes-a-distinct-claim
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (finishes
      (with-transaction ()
        (make-b :producer :rule-b)
        (make-b :subject "s2")
        (make-b :object "o2")
        (make-b :relation :other)))))

(test rule-version-is-not-part-of-identity
  "Design §6.1: PRODUCER excludes the version, so re-running a rule at a new
version collides with its own prior claim rather than adding a second one."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((c (make-b))) (setf (claim-rule-version c) "v1")))
    (signals graph-db:unique-constraint-violation
      (with-transaction ()
        (let ((c (make-b))) (setf (claim-rule-version c) "v2"))))))
