;;;; The standing vocabulary (GH #130, design §3.4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test standing-vocabulary-is-closed
  "Six standings, and nothing else is one."
  (is (= 6 (length +standings+)))
  (is-true (every #'standingp +standings+))
  (is-false (standingp :observedd))
  (is-false (standingp nil))
  (is-false (standingp 0)))

(test absence-is-distinguishable-from-a-value
  "The defect class this type exists to prevent: a never-measured state must
never be confusable with a measured one (design §3.4)."
  (dolist (s '(:searched-empty :uncovered :indeterminate))
    (is-true (standing-absence-p s))
    (is-false (standing-present-p s)))
  (dolist (s '(:observed :inferred :asserted))
    (is-false (standing-absence-p s))
    (is-true (standing-present-p s))))

(test the-three-absence-cases-stay-distinct
  "A source looked and found nothing, no source covers this, and we could not
find out are three different facts.  Collapsing them is the bug."
  (is (= 3 (length (remove-duplicates +absence-standings+))))
  (is (null (set-difference +absence-standings+ +standings+))))

(test standing-has-no-ordering-operator
  "Design §4.4: no defensible total order exists over standings, so this
subsystem must not define one.  An edit that adds one fails here."
  (is-false (find-symbol "STANDING<" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKEST" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKER" :graph-db.spacetime)))

(test check-standing-signals-on-a-non-standing
  (signals invalid-standing (check-standing :nope))
  (is (eq :observed (check-standing :observed))))
