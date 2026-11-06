;;;; rules/forward.lisp -- the opt-in, the per-graph state, the queue
;;;; and the step (forward-rules design §4.3, §5.3, §6; #456).

(in-package #:graph-db.rules)

(defstruct (timer-heap (:constructor %make-timer-heap))
  "Pending THRESHOLD-TIMERs: ITEMS a binary min-heap by instant, KEYS
the dedupe set (thresholds.lisp)."
  (items (make-array 16 :adjustable t :fill-pointer 0))
  (keys (make-hash-table :test 'equal)))

(defun %timer-count (heap)
  (if heap (fill-pointer (timer-heap-items heap)) 0))

(defstruct (forward-state (:constructor %make-forward-state))
  "One enabled graph's forward evaluation: QUEUE of pending deltas under
LOCK, RUNNING-P while a step drains it (D5), FAILURES as (name .
text) newest first, COMPILED the rule set cached while CACHE-STAMP
matches *DEF-RULES-STAMP* (NIL invalidates), TIMERS a TIMER-HEAP of
pending THRESHOLD-TIMERs or NIL, with TIMER-THREAD, TIMER-CV and
TIMER-STOP driving them (thresholds.lisp), LAST-REPORTS the last
delta's RULE-REPORTs, LAST-STEP-ELAPSED the wall seconds of the last
FORWARD-STEP drain, NIL before one (#457)."
  graph
  (queue '())
  (last-reports '())
  (last-step-elapsed nil)
  (lock (bordeaux-threads:make-lock "forward rules"))
  (running-p nil)
  (failures '())
  (compiled '())
  (cache-stamp nil)
  (timers nil)
  (timer-thread nil)
  (timer-cv (bordeaux-threads:make-condition-variable))
  (timer-stop nil))

(defvar *forward-states* (make-hash-table :test 'eq)
  "Graph -> FORWARD-STATE for every graph forward evaluation is enabled
on.")

(defvar *forward-states-lock*
  (bordeaux-threads:make-lock "forward states"))

(defun %forward-state (graph)
  (bordeaux-threads:with-lock-held (*forward-states-lock*)
    (values (gethash graph *forward-states*))))

(defun forward-rules-enabled-p (graph)
  "True while GRAPH derives at commit."
  (and (%forward-state graph) t))

(defun %forward-compiled (graph state)
  "STATE's compiled rules, recompiled when the stamp is stale: every
rule RUN-RULES would run, a rule that does not compile recorded on
FAILURES and skipped."
  (unless (eql (forward-state-cache-stamp state) *def-rules-stamp*)
    (let ((compiled '()))
      (dolist (spec (rules-in-scope graph))
        (when (%runnable-spec-p graph spec)
          (handler-case (push (compile-rule graph spec) compiled)
            (rule-compile-error (c)
              (%note-failure state (rule-spec-name spec)
                             (rule-compile-error-reason c))))))
      (setf (forward-state-compiled state) (nreverse compiled)
            (forward-state-cache-stamp state) *def-rules-stamp*)))
  (forward-state-compiled state))

(defun %enqueue-delta (state delta)
  (bordeaux-threads:with-lock-held ((forward-state-lock state))
    (setf (forward-state-queue state)
          (append (forward-state-queue state) (list delta)))))

(defparameter *forward-failures-kept* 100
  "How many failures FORWARD-RULES-STATUS keeps, newest first.")

(defun %note-failure (state name text)
  (bordeaux-threads:with-lock-held ((forward-state-lock state))
    (setf (forward-state-failures state)
          (subseq (cons (cons name text) (forward-state-failures state))
                  0 (min (1+ (length (forward-state-failures state)))
                         *forward-failures-kept*)))))

(defun %run-delta (graph state delta)
  "One delta through RUN-FORWARD-STRATA: a refused report and a signal
out of the step both land on FAILURES (spec §4.4) and the step goes
on; the commit that queued the delta is durable either way."
  (handler-case
      (let* ((graph-db::*evaluation-instant* (graph-db.spacetime::%st-now))
             (reports (run-forward-strata
                       graph (%forward-compiled graph state) delta)))
        (setf (forward-state-last-reports state) reports)
        (dolist (r reports)
          (when (eq (rule-report-outcome r) :refused)
            (%note-failure state (rule-report-rule-name r)
                           (format nil "~{~(~A~): ~A~^; ~}"
                                   (loop for (tag . text)
                                           in (rule-report-refusals r)
                                         append (list tag text)))))))
    (error (c)
      (log:error "forward step on ~S: ~A" (graph-db:graph-name graph) c)
      (%note-failure state :step (princ-to-string c)))))

(defun forward-step (graph)
  "Drain GRAPH's queue under D5's contract: the first entrant sets
RUNNING-P and loops -- take every queued delta, release the lock, run
them, retake it -- until the queue is empty; a later entrant finds
RUNNING-P set and returns at once, its delta left for the running
step.  So a commit never waits on another commit's derivation, and a
reader must not expect a derivation the moment WITH-TRANSACTION
returns (docs/rules.md).  Returns nothing."
  (let ((state (%forward-state graph))
        (start (get-internal-real-time)))
    (when state
      (bordeaux-threads:with-lock-held ((forward-state-lock state))
        (when (forward-state-running-p state)
          (return-from forward-step nil))
        (setf (forward-state-running-p state) t))
      (unwind-protect
           (loop for drains from 0
                 do (let ((deltas
                            (bordeaux-threads:with-lock-held
                                ((forward-state-lock state))
                              (cond ((null (forward-state-queue state))
                                     (return))
                                    ;; Spec §4.3: a step that never
                                    ;; quiesces -- an action feeding its
                                    ;; own read -- stops here.
                                    ((>= drains *rules-max-rounds*)
                                     (setf (forward-state-queue state) '())
                                     :rounds)
                                    (t (prog1 (forward-state-queue state)
                                         (setf (forward-state-queue state)
                                               '())))))))
                      (when (eq deltas :rounds)
                        (%note-failure state :rounds
                                       (format nil "no quiescence after ~
~D drains; the queue was dropped" *rules-max-rounds*))
                        (return))
                      (dolist (delta deltas)
                        (%run-delta graph state delta))))
        ;; Every exit, normal or not: the queue is empty or dropped.
        (bordeaux-threads:with-lock-held ((forward-state-lock state))
          (setf (forward-state-last-step-elapsed state)
                (/ (- (get-internal-real-time) start)
                   (float internal-time-units-per-second 1.0d0))
                (forward-state-running-p state) nil))))))

(defun %drain-queues ()
  "Every enabled graph with a queued delta gets its step: what the
outermost commit on any graph does, so a nested commit's delta is not
stranded behind a claim-free or foreign outer transaction (final
review, finding 1)."
  (dolist (g (bordeaux-threads:with-lock-held (*forward-states-lock*)
               (alexandria:hash-table-keys *forward-states*)))
    (let ((s (%forward-state g)))
      (when (and s (bordeaux-threads:with-lock-held ((forward-state-lock s))
                     (forward-state-queue s)))
        (forward-step g)))))

(defun forward-rules-status (graph)
  "What forward evaluation on GRAPH is doing: a plist (:ENABLED T
:QUEUE-DEPTH n :RUNNING-P bool :FAILURES ((name . text) ...) :TIMERS
n :LAST-STEP-ELAPSED seconds-or-NIL), newest failure first, the
elapsed the last FORWARD-STEP drain's (#457); (:ENABLED NIL) for a
graph not opted in."
  (let ((state (%forward-state graph)))
    (if (null state)
        (list :enabled nil)
        (bordeaux-threads:with-lock-held ((forward-state-lock state))
          (list :enabled t
                :queue-depth (length (forward-state-queue state))
                :running-p (forward-state-running-p state)
                :failures (copy-list (forward-state-failures state))
                :timers (%timer-count (forward-state-timers state))
                :last-step-elapsed
                (forward-state-last-step-elapsed state))))))

(defun %forward-hook (tx)
  "The *AFTER-COMMIT-HOOKS* entry: a committed TX on an enabled graph
queues its claim delta; an outermost commit -- no transaction still
open around it -- then drains every enabled graph's queue, a nested
one leaves that to the outermost; a committed RULE or PRODUCTION
record invalidates the compiled cache.  A commit forward evaluation
made itself (*FORWARD-HOOK-SUPPRESSED*) is ignored."
  (unless *forward-hook-suppressed*
    (%queue-commit tx)
    (unless graph-db::*transaction*
      (%drain-queues))))

(defun %queue-commit (tx)
  (let ((state (%forward-state (graph-db::graph tx))))
    (when state
      (let ((writes (graph-db::writes tx))
            (rule-class (find-class 'rule nil))
            (production-class (find-class 'production nil)))
        (when (some (lambda (w)
                      (let ((node (graph-db::node w)))
                        (or (and rule-class (typep node rule-class))
                            (and production-class
                                 (typep node production-class)))))
                    writes)
          (setf (forward-state-cache-stamp state) nil))
        (let ((delta (claim-delta writes (forward-state-graph state))))
          (when *agenda-production*
            (%check-production-writes state *agenda-production* delta))
          (when (plusp (hash-table-count delta))
            (%enqueue-delta state delta)))))))

(defun %check-production-writes (state compiled delta)
  "An action's commit wrote DELTA: a relation outside the production's
declared writes is recorded on FAILURES (spec §7); the write stands."
  (let* ((spec (compiled-rule-spec compiled))
         (writes (production-spec-writes spec)))
    (loop for rel being the hash-keys of delta
          unless (member rel writes :test #'string=)
            do (%note-failure state (rule-spec-name spec)
                              (format nil "wrote ~A outside its writes ~
~S" rel writes)))))

(defun enable-forward-rules (graph)
  "Derive at commit on GRAPH from now on (spec §5.3): installs the
post-commit hook once per image, registers GRAPH, and runs RUN-RULES
once so the store starts at its fixpoint -- a premise committed
before the opt-in is not a delta.  Returns GRAPH's FORWARD-STATE."
  (let* ((first-p nil)
         (state (bordeaux-threads:with-lock-held (*forward-states-lock*)
                  (pushnew '%forward-hook graph-db:*after-commit-hooks*)
                  (or (gethash graph *forward-states*)
                      (let ((new (%make-forward-state :graph graph)))
                        (bordeaux-threads:with-lock-held
                            ((forward-state-lock new))
                          (setf (forward-state-running-p new) t))
                        (setf first-p t
                              (gethash graph *forward-states*) new))))))
    (unwind-protect
         (let ((*forward-hook-suppressed* t))
           (run-rules graph))
      (when first-p
        (let ((queued (bordeaux-threads:with-lock-held
                          ((forward-state-lock state))
                        (setf (forward-state-running-p state) nil)
                        (forward-state-queue state))))
          (when queued (forward-step graph)))))
    state))

(defun disable-forward-rules (graph)
  "Stop deriving at commit on GRAPH; pending deltas and timers are
dropped, the timer thread stopped.  A step on another user thread is not
waited for.  T when it was enabled."
  (let ((state (bordeaux-threads:with-lock-held (*forward-states-lock*)
                 (prog1 (gethash graph *forward-states*)
                   (remhash graph *forward-states*)))))
    (when state
      (%stop-timers state)
      t)))

;; CLOSE-GRAPH implies the opt-out: nothing may touch a closed store
;; (spec §5.3; Review Focus 3).  A method, not a core hook.
(defmethod graph-db:close-graph :before ((graph graph-db::graph)
                                         &key snapshot-p)
  (declare (ignore snapshot-p))
  (when (forward-rules-enabled-p graph)
    (disable-forward-rules graph)))
