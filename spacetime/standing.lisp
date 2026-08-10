;;;; The standing vocabulary: how we came to know a thing, including the
;;;; three distinct ways of not knowing it (GH #130, design §3.4).

(in-package #:graph-db.spacetime)

(defparameter +standings+
  '(:observed :inferred :asserted :searched-empty :uncovered :indeterminate)
  "The closed standing vocabulary.  Deliberately UNORDERED: ASSERTED and
INFERRED cannot be ranked, so no comparison operator over standings exists
in this subsystem (design §4.4).")

(defparameter +absence-standings+
  '(:searched-empty :uncovered :indeterminate)
  "The three standings meaning THERE IS NO VALUE, each for a different
reason.  Keeping them apart is the whole point of the type.")

(deftype standing ()
  '(member :observed :inferred :asserted
    :searched-empty :uncovered :indeterminate))

(defun standingp (x)
  "True when X belongs to the standing vocabulary."
  (and (member x +standings+) t))

(defun standing-absence-p (s)
  "True when S records an absence.  An absence is not a weaker value; it is
the state in which there is no interval at all."
  (and (member s +absence-standings+) t))

(defun standing-present-p (s)
  "True when S records a value we hold: OBSERVED, INFERRED or ASSERTED."
  (and (standingp s) (not (standing-absence-p s))))

(defun check-standing (x)
  "Return X when it is a standing; signal INVALID-STANDING otherwise."
  (unless (standingp x)
    (error 'invalid-standing :value x))
  x)
