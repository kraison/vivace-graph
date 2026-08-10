;;;; A BOUND is the range within which one timestamp lies.  Making the
;;;; endpoint a range rather than a value is what lets imprecision, open-
;;;; endedness and total ignorance share one mechanism (design §3.1).

(in-package #:graph-db.spacetime)

(defstruct (bound (:constructor %make-bound (earliest latest))
                  (:copier nil))
  "EARLIEST and LATEST are each a LOCAL-TIME:TIMESTAMP or :UNBOUNDED, which
denotes negative infinity in EARLIEST and positive infinity in LATEST."
  (earliest nil :read-only t)
  (latest nil :read-only t))

(defun %endpoint-ok-p (x)
  (or (eq x :unbounded) (typep x 'local-time:timestamp)))

(defun make-bound (earliest latest)
  "The range [EARLIEST, LATEST], each a TIMESTAMP or :UNBOUNDED.  Signals
INVALID-BOUND on a non-endpoint or a reversed range."
  (unless (and (%endpoint-ok-p earliest) (%endpoint-ok-p latest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "endpoints must be a TIMESTAMP or :UNBOUNDED"))
  (when (and (not (eq earliest :unbounded))
             (not (eq latest :unbounded))
             (local-time:timestamp< latest earliest))
    (error 'invalid-bound :earliest earliest :latest latest
           :reason "EARLIEST is after LATEST"))
  (%make-bound earliest latest))

(defun exact-bound (timestamp)
  "A bound pinning exactly one timestamp."
  (make-bound timestamp timestamp))

(defun unknown-bound ()
  "A bound spanning all of time -- \"we have no idea when\"."
  (%make-bound :unbounded :unbounded))

(defun bound-exact-p (b)
  "True when B pins a single timestamp."
  (and (not (eq (bound-earliest b) :unbounded))
       (not (eq (bound-latest b) :unbounded))
       (local-time:timestamp= (bound-earliest b) (bound-latest b))))

(defun bound-unknown-p (b)
  "True when B constrains nothing."
  (and (eq (bound-earliest b) :unbounded)
       (eq (bound-latest b) :unbounded)))

(defun %strictly-before (latest earliest)
  "LATEST < EARLIEST.  :UNBOUNDED is +inf in a LATEST and -inf in an
EARLIEST, so either one makes this false -- it can never PRODUCE a
verdict."
  (and (not (eq latest :unbounded))
       (not (eq earliest :unbounded))
       (local-time:timestamp< latest earliest)))

(defun bound-compare (a b)
  "Compare the timestamps A and B stand for: :< :> := or :AMBIGUOUS.
Definite only when no choice within either range could give another
answer, so two overlapping ranges are :AMBIGUOUS even if they coincide
exactly."
  (cond ((%strictly-before (bound-latest a) (bound-earliest b)) :<)
        ((%strictly-before (bound-latest b) (bound-earliest a)) :>)
        ((and (bound-exact-p a) (bound-exact-p b)
              (local-time:timestamp= (bound-earliest a)
                                     (bound-earliest b)))
         :=)
        (t :ambiguous)))
