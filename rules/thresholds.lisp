;;;; rules/thresholds.lisp -- flip instants and the timers that
;;;; re-evaluate a threshold rule when one passes (forward-rules design
;;;; §5.2, D3; #456).

(in-package #:graph-db.rules)

(defstruct (threshold-timer (:constructor %make-threshold-timer))
  "One scheduled re-evaluation: at INSTANT, the match's premise CLAIMS
re-enter the step as a delta -- +1 for a :GAIN, +1 and -1 for a :LOSS
so the targeted path retracts what no longer holds."
  instant claims direction)

(defun %claim-instant (claim source)
  "CLAIM's instant under SOURCE, a name in *INSTANT-SOURCE-FUNCTORS*,
as a timestamp or NIL -- what the functor of that name answers."
  (flet ((edge (extent which)
           (and extent
                (let ((ts (ecase which
                            (:start (temporal-extent:bound-earliest
                                     (temporal-extent:extent-start extent)))
                            (:end (temporal-extent:bound-latest
                                   (temporal-extent:extent-end extent))))))
                  (and (typep ts 'local-time:timestamp) ts)))))
    (let ((recorded (let ((at (graph-db.spacetime:claim-recorded-at claim)))
                      (and (typep at 'local-time:timestamp) at)))
          (retracted (edge (graph-db.spacetime:claim-transaction-extent
                            claim)
                           :end)))
      (cond ((string= source "CLAIM-RECORDED-AT") recorded)
            ((string= source "CLAIM-RETRACTED-AT") retracted)
            ((string= source "CLAIM-TOUCHED-AT") (or retracted recorded))
            ((string= source "CLAIM-VALID-FROM")
             (edge (graph-db.spacetime:claim-extent claim) :start))
            ((string= source "CLAIM-VALID-TO")
             (edge (graph-db.spacetime:claim-extent claim) :end))
            (t nil)))))

(defun flip-instant (threshold claim)
  "When THRESHOLD (a COMPILED-RULE-THRESHOLDS entry) flips for the
premise CLAIM its source reads: the claim's instant plus the offset,
or NIL when the claim has no such instant."
  (destructuring-bind (goal direction claim-var source offset var)
      threshold
    (declare (ignore goal direction claim-var var))
    (let ((at (%claim-instant claim source)))
      (and at
           (if offset
               (local-time:timestamp+ at
                                      (graph-db::%parse-iso-duration offset)
                                      :nsec)
               at)))))

(defun %goals-without-thresholds (compiled)
  "COMPILED's body minus NOW/1 and the threshold comparisons: what a
match looks like before and after its flip instants."
  (let ((threshold-goals (mapcar #'first
                                 (compiled-rule-thresholds compiled))))
    (remove-if (lambda (g)
                 (or (%engine-goal-p g "NOW" 1)
                     (member g threshold-goals :test #'eq)))
               (compiled-rule-goals compiled))))

(defun %schedule-rule (graph state compiled delta)
  "Schedule the timers COMPILED's matches ask for: the threshold-less
body's solutions -- those DELTA reaches when it is given, all of them
otherwise -- each with a flip instant still ahead."
  (let* ((goals (%goals-without-thresholds compiled))
         (thresholds (compiled-rule-thresholds compiled))
         (premise-vars (compiled-rule-premise-vars compiled))
         (vars (remove-duplicates
                (append premise-vars (mapcar #'sixth thresholds))))
         (report (%make-rule-report
                  :rule-name (rule-spec-name (compiled-rule-spec compiled))))
         (now (or graph-db::*evaluation-instant*
                  (graph-db.spacetime::%st-now)))
         (goal-lists (if delta
                         (%variants compiled :all-goals t :goals goals)
                         (list goals))))
    (dolist (gl goal-lists)
      (let ((rows (let ((graph-db::*claim-scope* (list graph))
                        (graph-db:*rule-delta* delta))
                    (%solutions compiled graph report gl vars))))
        (dolist (row rows)
          (let ((claims (remove-if-not
                         #'graph-db::node-p
                         (mapcar (lambda (v) (%term-value v row vars))
                                 premise-vars))))
            (dolist (th thresholds)
              (let ((at (graph-db::%instant-arg
                         (%term-value (sixth th) row vars))))
                (when (and at (local-time:timestamp> at now))
                  (schedule-threshold state at claims
                                      (second th)))))))))))

(defun %schedule-thresholds (graph compiled-rules delta)
  "*THRESHOLD-SCHEDULER*: every threshold rule among COMPILED-RULES
scheduled on GRAPH's forward state, when it has one.  A failure is
recorded on the status, never signalled into the step."
  (let ((state (%forward-state graph)))
    (when state
      (dolist (c compiled-rules)
        (when (compiled-rule-thresholds c)
          (handler-case (%schedule-rule graph state c delta)
            (error (e)
              (%note-failure state
                             (rule-spec-name (compiled-rule-spec c))
                             (format nil "scheduling: ~A" e)))))))))

(setf *threshold-scheduler* '%schedule-thresholds)

(defun %same-timer-p (a instant claims direction)
  (and (eq (threshold-timer-direction a) direction)
       (local-time:timestamp= (threshold-timer-instant a) instant)
       (= (length (threshold-timer-claims a)) (length claims))
       (every (lambda (c)
                (find (graph-db:id c) (threshold-timer-claims a)
                      :key #'graph-db:id :test #'equalp))
              claims)))

(defun schedule-threshold (state instant claims direction)
  "A timer on STATE at INSTANT for the match CLAIMS, unless one like it
is already pending; starts the state's timer thread if need be.
Returns nothing."
  (bordeaux-threads:with-lock-held ((forward-state-lock state))
    (unless (find-if (lambda (tm) (%same-timer-p tm instant claims direction))
                     (forward-state-timers state))
      (setf (forward-state-timers state)
            (merge 'list
                   (list (%make-threshold-timer :instant instant
                                                :claims claims
                                                :direction direction))
                   (forward-state-timers state)
                   #'local-time:timestamp< :key #'threshold-timer-instant))
      (%ensure-timer-thread state)
      (bordeaux-threads:condition-notify (forward-state-timer-cv state))))
  (values))

(defun %fire-threshold (state timer)
  "TIMER's match re-enters the step as a delta (spec §5.2)."
  (let ((delta (make-hash-table :test 'equal))
        (graph (forward-state-graph state)))
    (dolist (c (threshold-timer-claims timer))
      (let ((rel (graph-db.spacetime:claim-relation c)))
        (push (cons c 1) (gethash rel delta))
        (when (eq (threshold-timer-direction timer) :loss)
          (push (cons c -1) (gethash rel delta)))))
    (%enqueue-delta state delta)
    (forward-step graph)))

(defun %timer-loop (state)
  "The timer thread: sleep until the earliest timer, fire it, repeat;
leaves when TIMER-STOP is set (DISABLE-FORWARD-RULES)."
  (let ((lock (forward-state-lock state))
        (cv (forward-state-timer-cv state)))
    (loop
      (let ((due nil))
        (bordeaux-threads:with-lock-held (lock)
          (loop
            (when (forward-state-timer-stop state)
              (return-from %timer-loop nil))
            (let ((next (first (forward-state-timers state))))
              (cond ((null next)
                     (bordeaux-threads:condition-wait cv lock))
                    (t
                     (let ((wait (local-time:timestamp-difference
                                  (threshold-timer-instant next)
                                  (local-time:now))))
                       (cond ((<= wait 0)
                              (pop (forward-state-timers state))
                              (setf due next)
                              (return))
                             (t (bordeaux-threads:condition-wait
                                 cv lock :timeout wait)))))))))
        (when due
          (handler-case (%fire-threshold state due)
            (error (e)
              (%note-failure state :timer (princ-to-string e)))))))))

(defun %ensure-timer-thread (state)
  "Under STATE's lock: the timer thread, started if none runs."
  (let ((th (forward-state-timer-thread state)))
    (unless (and th (bordeaux-threads:thread-alive-p th))
      (setf (forward-state-timer-stop state) nil
            (forward-state-timer-thread state)
            (bordeaux-threads:make-thread
             (lambda () (%timer-loop state))
             :name "forward timer")))))

(defun %stop-timers (state)
  "Stop STATE's timer thread and drop its timers; waits for the thread
unless called from it.  Returns nothing."
  (let ((th (bordeaux-threads:with-lock-held ((forward-state-lock state))
              (setf (forward-state-timer-stop state) t
                    (forward-state-timers state) '())
              (bordeaux-threads:condition-notify
               (forward-state-timer-cv state))
              (forward-state-timer-thread state))))
    (when (and th
               (bordeaux-threads:thread-alive-p th)
               (not (eq th (bordeaux-threads:current-thread))))
      (bordeaux-threads:join-thread th))
    (setf (forward-state-timer-thread state) nil))
  (values))
