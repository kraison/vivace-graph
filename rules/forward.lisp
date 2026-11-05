;;;; rules/forward.lisp -- the opt-in, the per-graph state, the queue
;;;; and the step (forward-rules design §4.3, §5.3, §6; #456).

(in-package #:graph-db.rules)

(defstruct (forward-state (:constructor %make-forward-state))
  "One enabled graph's forward evaluation: QUEUE of pending deltas under
LOCK, RUNNING-P while a step drains it (D5), FAILURES as (name .
text) newest first, COMPILED the rule set cached while CACHE-STAMP
matches *DEF-RULES-STAMP* (NIL invalidates), TIMERS the pending
THRESHOLD-TIMERs by instant with TIMER-THREAD, TIMER-CV and TIMER-STOP
driving them (thresholds.lisp), LAST-REPORTS the last delta's
RULE-REPORTs."
  graph
  (queue '())
  (last-reports '())
  (lock (bordeaux-threads:make-lock "forward rules"))
  (running-p nil)
  (failures '())
  (compiled '())
  (cache-stamp nil)
  (timers '())
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
              (push (cons (rule-spec-name spec)
                          (rule-compile-error-reason c))
                    (forward-state-failures state))))))
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
  (let ((state (%forward-state graph)))
    (when state
      (bordeaux-threads:with-lock-held ((forward-state-lock state))
        (when (forward-state-running-p state)
          (return-from forward-step nil))
        (setf (forward-state-running-p state) t))
      (unwind-protect
           (loop
             (let ((deltas
                     (bordeaux-threads:with-lock-held
                         ((forward-state-lock state))
                       (if (forward-state-queue state)
                           (prog1 (forward-state-queue state)
                             (setf (forward-state-queue state) '()))
                           (progn
                             (setf (forward-state-running-p state) nil)
                             (return-from forward-step nil))))))
               (dolist (delta deltas)
                 (%run-delta graph state delta))))
        ;; Only an abnormal exit reaches this with RUNNING-P still set.
        (bordeaux-threads:with-lock-held ((forward-state-lock state))
          (setf (forward-state-running-p state) nil))))))

(defun forward-rules-status (graph)
  "What forward evaluation on GRAPH is doing: a plist (:ENABLED T
:QUEUE-DEPTH n :RUNNING-P bool :FAILURES ((name . text) ...) :TIMERS
n), newest failure first; (:ENABLED NIL) for a graph not opted in."
  (let ((state (%forward-state graph)))
    (if (null state)
        (list :enabled nil)
        (bordeaux-threads:with-lock-held ((forward-state-lock state))
          (list :enabled t
                :queue-depth (length (forward-state-queue state))
                :running-p (forward-state-running-p state)
                :failures (copy-list (forward-state-failures state))
                :timers (length (forward-state-timers state)))))))

(defun %forward-hook (tx)
  "The *AFTER-COMMIT-HOOKS* entry: a committed TX on an enabled graph
queues its claim delta and runs the step -- unless a transaction is
still open around it, when the outermost commit's hook will; a
committed RULE record invalidates the compiled cache.  A commit
forward evaluation made itself (*FORWARD-HOOK-SUPPRESSED*) is
ignored."
  (let ((state (%forward-state (graph-db::graph tx))))
    (when (and state (not *forward-hook-suppressed*))
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
            (%enqueue-delta state delta)
            ;; A nested commit (spec §6): queue only; the outermost
            ;; commit's hook, outside every *TRANSACTION*, drains.
            (unless graph-db::*transaction*
              (forward-step (forward-state-graph state)))))))))

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
  (let ((state (bordeaux-threads:with-lock-held (*forward-states-lock*)
                 (pushnew '%forward-hook graph-db:*after-commit-hooks*)
                 (or (gethash graph *forward-states*)
                     (setf (gethash graph *forward-states*)
                           (%make-forward-state :graph graph))))))
    (let ((*forward-hook-suppressed* t))
      (run-rules graph))
    state))

(defun disable-forward-rules (graph)
  "Stop deriving at commit on GRAPH; pending deltas and timers are
dropped, the timer thread stopped.  T when it was enabled."
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
