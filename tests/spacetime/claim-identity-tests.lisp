;;;; Claim identity: what the constraint permits and forbids (design §6).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun make-b (&key (producer "rule-a") (subject "s1") (object "o1")
                    (relation "r") (standing :inferred) extent rule-version)
  (make-ct-claim-binary :subject-namespace :ns :subject-key subject
                        :relation relation
                        :object-namespace :ns :object-key object
                        :producer producer :standing standing
                        :extent extent :rule-version rule-version))

(defun make-u (&key (producer "rule-a") (subject "s1") (relation "r") extent)
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred :extent extent))

(defun make-u-at (&key (producer "rule-a") (subject "s1") (relation "r")
                       recorded-at)
  "MAKE-U with an explicit transaction stamp (GH #148)."
  (make-ct-claim-unary :subject-namespace :ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred :recorded-at recorded-at))

(test two-producers-may-disagree
  "Design §6.2.  This is the entire reason for reifying: an edge model would
have to resolve this at write time."
  (with-claim-graph (g)
    (declare (ignorable g))
    (finishes
      (with-transaction ()
        (make-b :producer "rule-a" :object "yes")
        (make-b :producer "operator-o" :object "no")))))

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
        (make-u :producer "rule-a" :subject "s1" :relation "r")
        (make-b :producer "rule-a" :subject "s1" :relation "r")))))

(test differing-in-any-identity-component-makes-a-distinct-claim
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b))
    (finishes
      (with-transaction ()
        (make-b :producer "rule-b")
        (make-b :subject "s2")
        (make-b :object "o2")
        (make-b :relation "other")))))

(test rule-version-is-not-part-of-identity
  "Design §6.1: PRODUCER excludes the version, so re-running a rule at a new
version collides with its own prior claim rather than adding a second one.

RULE-VERSION arrives via the constructor here, for convenience -- a
post-construction SETF would persist too (GH #135 is fixed), but
identity depends only on PRODUCER, never on RULE-VERSION, so this is not
under test either way."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction () (make-b :rule-version "v1"))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-b :rule-version "v2")))))

;;; --- Finding 1: every identity component must be non-nil (design §3.1) ---

(test omitting-a-unary-identity-component-signals
  "PRODUCER, SUBJECT-NAMESPACE, SUBJECT-KEY, RELATION are the UNARY
constraint tuple; DEF-UNIQUE exempts any tuple containing a null, so each
must be checked non-nil before the node is built (GH #131 finding 1)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (flet ((try (&rest args)
             (with-transaction ()
               (signals missing-claim-identity-component
                 (apply #'make-ct-claim-unary args)))))
      (try :subject-key "s" :relation "r" :producer "p" :standing :inferred)
      (try :subject-namespace :ns :relation "r" :producer "p"
           :standing :inferred)
      (try :subject-namespace :ns :subject-key "s" :producer "p"
           :standing :inferred)
      (try :subject-namespace :ns :subject-key "s" :relation "r"
           :standing :inferred))))

(test omitting-a-binary-identity-component-signals
  "The six components of the BINARY constraint tuple; each must be non-nil
before the node is built (GH #131 finding 1)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (flet ((try (&rest args)
             (with-transaction ()
               (signals missing-claim-identity-component
                 (apply #'make-ct-claim-binary args)))))
      ;; omit :producer
      (try :subject-namespace :ns :subject-key "s" :relation "r"
           :object-namespace :ns :object-key "o" :standing :inferred)
      ;; omit :subject-namespace
      (try :subject-key "s" :relation "r" :object-namespace :ns
           :object-key "o" :producer "p" :standing :inferred)
      ;; omit :subject-key
      (try :subject-namespace :ns :relation "r" :object-namespace :ns
           :object-key "o" :producer "p" :standing :inferred)
      ;; omit :relation
      (try :subject-namespace :ns :subject-key "s" :object-namespace :ns
           :object-key "o" :producer "p" :standing :inferred)
      ;; omit :object-namespace
      (try :subject-namespace :ns :subject-key "s" :relation "r"
           :object-key "o" :producer "p" :standing :inferred)
      ;; omit :object-key
      (try :subject-namespace :ns :subject-key "s" :relation "r"
           :object-namespace :ns :producer "p" :standing :inferred))))

(test omitting-object-key-closes-the-exemption
  "Before this fix, two binary claims identical except for an omitted
OBJECT-KEY both committed silently -- NIL is exempt from DEF-UNIQUE's
uniqueness check, so the duplicate was invisible to it (Finding 1). Now
the first omission cannot even construct a claim, so there is nothing
left to duplicate."
  (with-claim-graph (g)
    (declare (ignorable g))
    (flet ((try-without-object-key ()
             (with-transaction ()
               (signals missing-claim-identity-component
                 (make-ct-claim-binary
                  :subject-namespace :ns :subject-key "s1" :relation "r"
                  :object-namespace :ns :producer "rule-a"
                  :standing :inferred)))))
      (try-without-object-key)
      (try-without-object-key)
      (is (null (claims-touching g 'ct-claim :ns "s1"))))))

;;; RELATION and PRODUCER are canonical strings (GH #160).

(test relation-and-producer-predicates-admit-only-canonical-strings
  "GH #160.  Both slots are identity components compared with EQUAL, so
every spelling the predicates refuse would otherwise be a second claim."
  (is-true (canonical-relation-p "registered-at"))
  (is-true (canonical-relation-p "coded-in2"))
  (is-false (canonical-relation-p :registered-at))
  (is-false (canonical-relation-p 'registered-at))
  (is-false (canonical-relation-p "Registered-At"))
  (is-false (canonical-relation-p " registered-at"))
  (is-false (canonical-relation-p "registered at"))
  (is-false (canonical-relation-p ""))
  (is-false (canonical-relation-p nil))
  ;; / is provenance's privilege, not vocabulary's.
  (is-false (canonical-relation-p "tenant/registered-at"))
  (is-true (canonical-producer-p "mine-action/spine-register"))
  (is-true (canonical-producer-p "p"))
  (is-false (canonical-producer-p :rule-a))
  (is-false (canonical-producer-p "Rule/A"))
  (is-false (canonical-producer-p "")))

(test a-non-canonical-relation-or-producer-is-refused-at-commit
  "The :CHECK declarations DEF-CLAIM-CLASSES emits, exercised through the
constructor: a keyword and a case variant each abort the transaction, so
neither can fork the identity space (GH #160)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals graph-db:value-constraint-violation
      (with-transaction () (make-u :relation :r)))
    (signals graph-db:value-constraint-violation
      (with-transaction () (make-u :relation "Registered-At")))
    (signals graph-db:value-constraint-violation
      (with-transaction () (make-b :producer :rule-a)))
    (signals graph-db:value-constraint-violation
      (with-transaction () (make-b :producer "rule/A")))
    ;; A refused write left nothing behind.
    (is (null (claims-by-producer g 'ct-claim "rule-a")))
    ;; A path-like producer is the deployed convention and commits.
    (finishes
      (with-transaction () (make-b :producer "mine-action/rule-a")))
    (is (= 1 (length (claims-by-producer g 'ct-claim "mine-action/rule-a"))))))

(test the-canonical-check-covers-an-update-not-only-construction
  "A :CHECK lives on the class, so it also guards the write paths that
never go through MAKE-<NAME> -- here, SETF on an existing claim (GH #160)."
  (with-claim-graph (g)
    (with-transaction () (make-u :subject "s1"))
    (let ((c (first (claims-touching g 'ct-claim :ns "s1"))))
      (signals graph-db:value-constraint-violation
        (with-transaction ()
          (let ((copy (graph-db::copy c)))
            (setf (claim-relation copy) :r)
            (graph-db::save copy))))
      (is (equal "r" (claim-relation
                      (first (claims-touching g 'ct-claim :ns "s1"))))))))
