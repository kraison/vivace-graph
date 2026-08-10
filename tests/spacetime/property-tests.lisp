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

(defun %exact-interval (state)
  "S is capped at 27 (not RANDOM-DAY's 28) so S+1 always leaves a January
day for E -- an exact S = E pair is what MAKE-INTERVAL now rejects
(GH #130, design §3.2)."
  (let* ((s (1+ (random 27 state)))
         (e (min 28 (+ s 1 (random 10 state)))))
    (make-interval (exact-bound (ts 2026 1 s))
                   (exact-bound (ts 2026 1 e)))))

(defun %ranged-interval (state)
  "Each endpoint a several-day window, not a pinned instant.  GAP keeps
the start window's LATEST strictly before the end window's EARLIEST, so
the interval stays well-formed and never trips MAKE-INTERVAL's :=
guard.  The windows are wide enough (up to 3 days) that two
independently drawn intervals can plausibly overlap and drive
BOUND-COMPARE to :AMBIGUOUS -- the branch %COMPATIBLE-P exists for
(GH #130)."
  (let* ((s-lo (1+ (random 18 state)))
         (s-hi (+ s-lo (random 4 state)))
         (gap (1+ (random 3 state)))
         (e-lo (+ s-hi gap))
         (e-hi (min 28 (+ e-lo (random 4 state)))))
    (make-interval (make-bound (ts 2026 1 s-lo) (ts 2026 1 s-hi))
                   (make-bound (ts 2026 1 e-lo) (ts 2026 1 e-hi)))))

(defun random-extent (state)
  "Interval endpoints are exact about a third of the time and genuinely
ranged the rest, so both the oracle (exact vs exact) and the uncertain
path (ranged vs ranged) get exercised."
  (if (zerop (random 2 state))
      (make-instant (random-bound state))
      (if (zerop (random 3 state))
          (%exact-interval state)
          (%ranged-interval state))))

(defun concretise (e state)
  "Pick one admissible timestamp inside each of E's bounds and return an
extent with EXACT bounds.  :UNBOUNDED is drawn from a window well outside
the January range the generators use, so it stays outside every interval.
For an interval, END is picked STRICTLY after the chosen START -- never
equal and never earlier, since MAKE-INTERVAL now rejects both := and :>
(GH #130)."
  (labels ((pick (b)
             (let ((lo (bound-earliest b))
                   (hi (bound-latest b)))
               (cond ((and (eq lo :unbounded) (eq hi :unbounded))
                      (ts 2026 1 (random-day state)))
                     ((eq lo :unbounded) hi)
                     ((eq hi :unbounded) lo)
                     ((timestamp= lo hi) lo)
                     (t (if (zerop (random 2 state)) lo hi)))))
           (pick-after (floor-ts b)
             "Like PICK, but the result must be STRICTLY after FLOOR-TS.
RANDOM-EXTENT keeps every interval well-formed -- the start bound's
LATEST strictly precedes the end bound's EARLIEST -- so B's own PICK
already clears FLOOR-TS; BOUND-EARLIEST is the fallback for a B that
somehow does not."
             (let ((picked (pick b)))
               (if (timestamp< floor-ts picked)
                   picked
                   (bound-earliest b)))))
    (if (extent-instant-p e)
        (make-instant (exact-bound (pick (extent-start e)))
                      :standing (extent-standing e))
        (let ((start (pick (extent-start e))))
          (make-interval (exact-bound start)
                         (exact-bound (pick-after start (extent-end e)))
                         :standing (extent-standing e))))))

(test the-relation-set-never-omits-the-truth
  "Design §7.1.  For any two extents, every concrete instantiation of their
endpoints must produce a relation the uncertain answer already contains.  If
this can fail, the algebra emits confidently-wrong answers."
  (let ((state (sb-ext:seed-random-state *property-seed*))
        (checked 0)
        (indefinite 0)
        (ii-pairs 0)
        (ii-ambiguous 0))
    (dotimes (i *property-trials*)
      (let* ((a (random-extent state))
             (b (random-extent state))
             (set (temporal-relation-relations (allen-relations a b)))
             (ca (concretise a state))
             (cb (concretise b state))
             (truth (allen-relation ca cb)))
        (when (> (length set) 1)
          (incf indefinite)
          (when (and (not (extent-instant-p a)) (not (extent-instant-p b)))
            (incf ii-ambiguous)))
        (unless (or (extent-instant-p a) (extent-instant-p b))
          (incf ii-pairs))
        (when truth
          (incf checked)
          (is-true (member truth set)
                   "trial ~D: concrete truth ~S missing from ~S" i truth
                   set))))
    (is (> checked (floor *property-trials* 2))
        "only ~D of ~D trials concretised to a definite relation -- the ~
         generators are not exercising the exact path (the oracle side ~
         of the property is vacuous)"
        checked *property-trials*)
    (is (> indefinite (floor *property-trials* 10))
        "only ~D of ~D trials produced ANY indefinite relation set -- ~
         overall ambiguity (instant- or interval-based) is too rare for ~
         this property to mean anything"
        indefinite *property-trials*)
    ;; Floor sits well below this seed's observed ~26% ambiguous rate, so
    ;; a regression to always-exact intervals fails loudly instead of
    ;; passing silently (GH #133).
    (is (> ii-ambiguous 6)
        "only ~D of ~D interval-vs-interval trials produced an ~
         indefinite relation set -- %RANGED-INTERVAL is not generating ~
         overlapping bounds, and a regression to always-exact intervals ~
         (GH #133) would pass here silently"
        ii-ambiguous ii-pairs)))

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
