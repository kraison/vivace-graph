;;;; Soundness: the relation set may never omit the truth (design §7.1).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *property-trials* 400)

(defparameter *property-seed* 20260810
  "Fixed so a failure is reproducible.  A flaky property test nobody can
re-run is worse than no property test.")

(defun random-day (state) (1+ (random 28 state)))

(defun random-bound (state)
  "A bound over January 2026, sometimes exact, sometimes open-ended."
  (case (random 6 state)
    (0 (unknown-bound))
    (1 (make-bound (ts 2026 1 (random-day state)) :unbounded))
    (2 (make-bound :unbounded (ts 2026 1 (random-day state))))
    (t (let* ((d1 (random-day state))
              (d2 (max d1 (random-day state))))
         (if (= d1 d2)
             (exact-bound (ts 2026 1 d1))
             (make-bound (ts 2026 1 d1) (ts 2026 1 d2)))))))

(defun random-extent (state)
  "S is capped at 27 (not RANDOM-DAY's 28) so S+1 always leaves a January
day for E -- MAKE-INTERVAL now rejects a value-degenerate S = E pair
(GH #130, design §3.2)."
  (if (zerop (random 2 state))
      (make-instant (random-bound state))
      (let* ((s (1+ (random 27 state)))
             (e (min 28 (+ s 1 (random 10 state)))))
        (make-interval (exact-bound (ts 2026 1 s))
                       (exact-bound (ts 2026 1 e))))))

(defun concretise (e state)
  "Pick one admissible timestamp inside each of E's bounds and return an
extent with EXACT bounds.  :UNBOUNDED is drawn from a window well outside
the January range the generators use, so it stays outside every interval.
For an interval, END is picked no earlier than the chosen START -- picking
both endpoints independently could otherwise invert an interval that no
caller could ever have constructed (GH #130)."
  (labels ((pick (b)
             (let ((lo (bound-earliest b))
                   (hi (bound-latest b)))
               (cond ((and (eq lo :unbounded) (eq hi :unbounded))
                      (ts 2026 1 (random-day state)))
                     ((eq lo :unbounded) hi)
                     ((eq hi :unbounded) lo)
                     ((timestamp= lo hi) lo)
                     (t (if (zerop (random 2 state)) lo hi)))))
           (pick-from (floor-ts b)
             "Like PICK, but constrained to timestamps >= FLOOR-TS.  Falls
back to FLOOR-TS itself when nothing admissible in B clears it."
             (let* ((hi (bound-latest b))
                    (lo0 (bound-earliest b))
                    (lo (if (or (eq lo0 :unbounded)
                                (timestamp< lo0 floor-ts))
                            floor-ts lo0)))
               (cond ((eq hi :unbounded) lo)
                     ((timestamp< hi lo) floor-ts)
                     ((timestamp= lo hi) lo)
                     (t (if (zerop (random 2 state)) lo hi))))))
    (if (extent-instant-p e)
        (make-instant (exact-bound (pick (extent-start e)))
                      :standing (extent-standing e))
        (let ((start (pick (extent-start e))))
          (make-interval (exact-bound start)
                         (exact-bound (pick-from start (extent-end e)))
                         :standing (extent-standing e))))))

(test the-relation-set-never-omits-the-truth
  "Design §7.1.  For any two extents, every concrete instantiation of their
endpoints must produce a relation the uncertain answer already contains.  If
this can fail, the algebra emits confidently-wrong answers."
  (let ((state (sb-ext:seed-random-state *property-seed*))
        (checked 0))
    (dotimes (i *property-trials*)
      (let* ((a (random-extent state))
             (b (random-extent state))
             (set (temporal-relation-relations (allen-relations a b)))
             (ca (concretise a state))
             (cb (concretise b state))
             (truth (allen-relation ca cb)))
        (when truth
          (incf checked)
          (is-true (member truth set)
                   "trial ~D: concrete truth ~S missing from ~S" i truth
                   set))))
    (is (> checked (floor *property-trials* 2))
        "only ~D of ~D trials concretised to a definite relation -- the ~
         generators are not exercising the exact path"
        checked *property-trials*)))

(test concretising-an-extent-always-gives-a-definite-answer
  "Guards the oracle itself: if an exact pair ever went indefinite, the
soundness test above would silently stop checking anything."
  (let ((state (sb-ext:seed-random-state *property-seed*)))
    (dotimes (i 200)
      (let ((ca (concretise (random-extent state) state))
            (cb (concretise (random-extent state) state)))
        (is-true (allen-definite-p ca cb)
                 "trial ~D: exact endpoints gave ~S" i
                 (temporal-relation-relations (allen-relations ca cb)))))))
