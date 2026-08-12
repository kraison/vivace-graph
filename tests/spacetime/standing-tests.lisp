;;;; The standing vocabulary (GH #130, design §3.4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test standing-vocabulary-is-closed
  "Seven standings, and nothing else is one.  The count is deliberate: the
vocabulary is closed, so growing it must be a visible edit here (GH #142)."
  (is (= 7 (length +standings+)))
  (is-true (every #'standingp +standings+))
  (is-false (standingp :observedd))
  (is-false (standingp nil))
  (is-false (standingp 0)))

(test absence-is-distinguishable-from-a-value
  "The defect class this type exists to prevent: a never-measured state must
never be confusable with a measured one (design §3.4)."
  (dolist (s '(:searched-empty :determined-empty :uncovered :indeterminate))
    (is-true (standing-absence-p s))
    (is-false (standing-present-p s)))
  (dolist (s '(:observed :inferred :asserted))
    (is-false (standing-absence-p s))
    (is-true (standing-present-p s))))

(test the-four-absence-cases-stay-distinct
  "A source looked and found nothing, the subject itself has no value, no
source covers this, and we could not find out are four different facts.
Collapsing them is the bug."
  (is (= 4 (length (remove-duplicates +absence-standings+))))
  (is (null (set-difference +absence-standings+ +standings+))))

(test determined-empty-is-not-searched-empty
  "Both are DETERMINED absences, so neither may collapse into the other: the
population case can name what it covered, the subject case cannot and does
not need to (GH #142).  An edit that drops one fails here."
  ;; Both must be absences in their own right -- a merge of either into the
  ;; other drops one of these memberships and fails here.
  (is-true (standing-absence-p :determined-empty))
  (is-true (standing-absence-p :searched-empty))
  (is (= 2 (length (intersection '(:determined-empty :searched-empty)
                                 +absence-standings+))))
  (is-true (standingp :determined-empty))
  ;; It is an absence, NOT a value: a draft's validity is empty, and an
  ;; empty interval is unrepresentable -- MAKE-INTERVAL signals on
  ;; START = END, and TEMPORAL-RELATION's RELATIONS may never be empty.
  (is-false (standing-present-p :determined-empty)))

(test standing-has-no-ordering-operator
  "Design §4.4: no defensible total order exists over standings, so this
subsystem must not define one.  An edit that adds one fails here."
  (is-false (find-symbol "STANDING<" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKEST" :graph-db.spacetime))
  (is-false (find-symbol "STANDING-WEAKER" :graph-db.spacetime)))

(test check-standing-signals-on-a-non-standing
  (signals invalid-standing (check-standing :nope))
  (is (eq :observed (check-standing :observed))))
