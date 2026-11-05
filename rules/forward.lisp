;;;; rules/forward.lisp -- the opt-in, the per-graph state, the queue
;;;; and the step (forward-rules design §4.3, §5.3, §6; #456).

(in-package #:graph-db.rules)

(defstruct (forward-state (:constructor %make-forward-state))
  "One enabled graph's forward evaluation: QUEUE of pending deltas under
LOCK, RUNNING-P while a step drains it (D5), FAILURES as (name .
text) newest first, COMPILED the rule set cached while CACHE-STAMP
matches *DEF-RULES-STAMP* (NIL invalidates), TIMERS the threshold
timers (thresholds.lisp), LAST-REPORTS the last delta's RULE-REPORTs."
  graph
  (queue '())
  (last-reports '())
  (lock (bordeaux-threads:make-lock "forward rules"))
  (running-p nil)
  (failures '())
  (compiled '())
  (cache-stamp nil)
  (timers '()))

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

(defun forward-step (graph)
  "Drain GRAPH's queue: each delta through RUN-FORWARD-STRATA with the
cached rule set.  Returns nothing."
  (let ((state (%forward-state graph)))
    (when state
      (loop
        (let ((delta (bordeaux-threads:with-lock-held
                         ((forward-state-lock state))
                       (pop (forward-state-queue state)))))
          (unless delta (return))
          (setf (forward-state-last-reports state)
                (run-forward-strata graph (%forward-compiled graph state)
                                    delta)))))))

(defun %forward-hook (tx)
  "The *AFTER-COMMIT-HOOKS* entry: a committed TX on an enabled graph
queues its claim delta and runs the step; a committed RULE record
invalidates the compiled cache.  A commit forward evaluation made
itself (*FORWARD-HOOK-SUPPRESSED*) is ignored."
  (let ((state (%forward-state (graph-db::graph tx))))
    (when (and state (not *forward-hook-suppressed*))
      (let ((writes (graph-db::writes tx)))
        (when (some (lambda (w) (typep (graph-db::node w) 'rule)) writes)
          (setf (forward-state-cache-stamp state) nil))
        (let ((delta (claim-delta writes (forward-state-graph state))))
          (when (plusp (hash-table-count delta))
            (%enqueue-delta state delta)
            (forward-step (forward-state-graph state))))))))

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
  "Stop deriving at commit on GRAPH; pending deltas are dropped.  T when
it was enabled."
  (bordeaux-threads:with-lock-held (*forward-states-lock*)
    (and (remhash graph *forward-states*) t)))
