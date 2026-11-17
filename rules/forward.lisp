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
  "One enabled graph's forward evaluation: QUEUE of pending (delta .
generation) under LOCK, GENERATION 0 for a commit from outside the
step and one more than the running delta's for an action's commit
(#472, #475), DRAIN :COMMIT when the committing thread drains it or :CALLER
when only FORWARD-STEP does, ON-QUEUE a function of the graph called
outside every lock whenever a delta is queued, or NIL (#467),
ON-FAILURE a function of (name tag text condition) called for every
failure recorded, or NIL (#468), RUNNING-P while a step drains it
(D5), FAILURES as (name . text) newest first, COMPILED the rule set
cached while CACHE-STAMP matches *DEF-RULES-STAMP* (NIL invalidates),
TIMERS a TIMER-HEAP of
pending THRESHOLD-TIMERs or NIL, with TIMER-THREAD, TIMER-CV and
TIMER-STOP driving them (thresholds.lisp), LAST-REPORTS the last
delta's RULE-REPORTs, LAST-STEP-ELAPSED the wall seconds of the last
FORWARD-STEP drain, NIL before one (#457)."
  graph
  (queue '())
  (drain :commit)
  (on-queue nil)
  (on-failure nil)
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
    ;; One scope and one edges cache for the whole recompile, so each
    ;; text is guarded once for its edges (#474).
    (let ((compiled '())
          (scope (rules-in-scope graph)))
      (with-edges-cache ()
        (dolist (spec scope)
          (when (%runnable-spec-p graph spec)
            (handler-case (push (compile-rule graph spec :others scope)
                                compiled)
              (rule-compile-error (c)
                (%note-failure state (rule-spec-name spec)
                               (rule-compile-error-reason c)
                               :tag :rule :condition c))))))
      (setf (forward-state-compiled state) (nreverse compiled)
            (forward-state-cache-stamp state) *def-rules-stamp*)))
  (forward-state-compiled state))

(defvar *delta-generation* 0
  "The generation of the delta the running step is on: 0 for a commit
from outside the step, one more for each delta an action's commit fed
from the last (#475).  Bound by %RUN-DELTA; read by the hook.")

(defun %enqueue-delta (state delta &optional (generation 0))
  "Queue DELTA on STATE at GENERATION, what the rounds guard measures
(#475)."
  (bordeaux-threads:with-lock-held ((forward-state-lock state))
    (setf (forward-state-queue state)
          (append (forward-state-queue state)
                  (list (cons delta generation))))))

(defun %queue-delta (state delta &optional (generation 0))
  "Queue DELTA at GENERATION and tell ON-QUEUE, outside every lock; a
signal from it is logged and recorded under :ON-QUEUE (#467)."
  (%enqueue-delta state delta generation)
  (let ((fn (forward-state-on-queue state)))
    (when fn
      (handler-case (funcall fn (forward-state-graph state))
        (error (c)
          (log:error "on-queue on ~S: ~A"
                     (graph-db:graph-name (forward-state-graph state)) c)
          (%note-failure state :on-queue (princ-to-string c)
                         :tag :on-queue :condition c))))))

(defun %caller-drains-p (state)
  (eq (forward-state-drain state) :caller))

(defparameter *forward-failures-kept* 100
  "How many failures FORWARD-RULES-STATUS keeps, newest first.")

(defun %note-failure (state name text &key tag condition)
  "Record (NAME . TEXT) on STATE's FAILURES, newest first under the
cap, then tell ON-FAILURE -- (NAME TAG TEXT CONDITION), outside the
lock -- when one is set; a signal from it is logged and the step goes
on (#468)."
  (bordeaux-threads:with-lock-held ((forward-state-lock state))
    (setf (forward-state-failures state)
          (subseq (cons (cons name text) (forward-state-failures state))
                  0 (min (1+ (length (forward-state-failures state)))
                         *forward-failures-kept*))))
  (let ((fn (forward-state-on-failure state)))
    (when fn
      (handler-case (funcall fn name tag text condition)
        (error (c)
          (log:error "on-failure on ~S for ~A: ~A"
                     (graph-db:graph-name (forward-state-graph state))
                     name c))))))

(defun %note-refused-reports (state reports)
  "Every :REFUSED report among REPORTS on FAILURES: its refusals
joined as the text, the first's tag and the report's condition for
ON-FAILURE."
  (dolist (r reports)
    (when (eq (rule-report-outcome r) :refused)
      (%note-failure state (rule-report-rule-name r)
                     (format nil "~{~(~A~): ~A~^; ~}"
                             (loop for (tag . text)
                                     in (rule-report-refusals r)
                                   append (list tag text)))
                     :tag (car (first (rule-report-refusals r)))
                     :condition (rule-report-condition r)))))

(defun %run-delta (graph state entry)
  "One queue ENTRY, (delta . generation), through RUN-FORWARD-STRATA
with *DELTA-GENERATION* bound for the actions' commits: a refused
report and a signal out of the step both land on FAILURES (spec §4.4)
and the step goes on; the commit that queued the delta is durable
either way."
  (handler-case
      (let* ((graph-db::*evaluation-instant* (graph-db.spacetime::%st-now))
             (*delta-generation* (cdr entry))
             (reports (run-forward-strata
                       graph (%forward-compiled graph state) (car entry))))
        (setf (forward-state-last-reports state) reports)
        (%note-refused-reports state reports))
    (error (c)
      (log:error "forward step on ~S: ~A" (graph-db:graph-name graph) c)
      (%note-failure state :step (princ-to-string c)
                     :tag :step :condition c))))

(defun forward-step (graph)
  "Drain GRAPH's queue under D5's contract: the first entrant sets
RUNNING-P and loops -- take every queued delta, release the lock, run
them, retake it -- until the queue is empty; a later entrant finds
RUNNING-P set and returns at once, its delta left for the running
step.  So a commit never waits on another commit's derivation, and a
reader must not expect a derivation the moment WITH-TRANSACTION
returns (docs/rules.md).  The rounds guard (spec §4.3) measures a
chain of deltas each fed by an action from the last: a queue entry's
generation is 0 for a commit from outside the step and one more than
the running delta's for an action's commit, and an entry at
*RULES-MAX-ROUNDS* is dropped with a :ROUNDS failure while everything
else, foreign commits and shorter chains alike, is run (#472, #475).
Returns nothing."
  (let ((state (%forward-state graph))
        (start (get-internal-real-time)))
    (when state
      (bordeaux-threads:with-lock-held ((forward-state-lock state))
        (when (forward-state-running-p state)
          (return-from forward-step nil))
        (setf (forward-state-running-p state) t))
      (unwind-protect
           (loop
             (let ((taken (bordeaux-threads:with-lock-held
                              ((forward-state-lock state))
                            (prog1 (forward-state-queue state)
                              (setf (forward-state-queue state) '())))))
               (when (null taken) (return))
               (let ((dropped (count-if (lambda (e)
                                          (>= (cdr e) *rules-max-rounds*))
                                        taken)))
                 (when (plusp dropped)
                   (%note-failure state :rounds
                                  (format nil "no quiescence: an action ~
fed a chain of ~D deltas each from the last; ~D at its end dropped"
                                          *rules-max-rounds* dropped)
                                  :tag :rounds))
                 (dolist (entry taken)
                   (when (< (cdr entry) *rules-max-rounds*)
                     (%run-delta graph state entry))))))
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
review, finding 1).  A graph drained by its caller (#467) is skipped."
  (dolist (g (bordeaux-threads:with-lock-held (*forward-states-lock*)
               (alexandria:hash-table-keys *forward-states*)))
    (let ((s (%forward-state g)))
      (when (and s
                 (not (%caller-drains-p s))
                 (bordeaux-threads:with-lock-held ((forward-state-lock s))
                   (forward-state-queue s)))
        (forward-step g)))))

(defun forward-rules-status (graph)
  "What forward evaluation on GRAPH is doing: a plist (:ENABLED T
:DRAIN :COMMIT-or-:CALLER :QUEUE-DEPTH n :RUNNING-P bool :FAILURES
((name . text) ...) :TIMERS n :LAST-STEP-ELAPSED seconds-or-NIL),
newest failure first, the elapsed the last FORWARD-STEP drain's
(#457); (:ENABLED NIL) for a graph not opted in."
  (let ((state (%forward-state graph)))
    (if (null state)
        (list :enabled nil)
        (bordeaux-threads:with-lock-held ((forward-state-lock state))
          (list :enabled t
                :drain (forward-state-drain state)
                :queue-depth (length (forward-state-queue state))
                :running-p (forward-state-running-p state)
                :failures (copy-list (forward-state-failures state))
                :timers (%timer-count (forward-state-timers state))
                :last-step-elapsed
                (forward-state-last-step-elapsed state))))))

(defun %forward-hook (tx)
  "The *AFTER-COMMIT-HOOKS* entry: a committed TX on an enabled graph
queues its claim delta; an outermost commit -- no transaction still
open around it -- then drains every enabled graph's queue but a
caller-drained one (#467), a nested one leaves that to the outermost;
a committed RULE or PRODUCTION record invalidates the compiled cache.
A commit forward evaluation made itself (*FORWARD-HOOK-SUPPRESSED*) is
ignored."
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
            ;; An action's commit continues the running delta's
            ;; chain (#472, #475).
            (%queue-delta state delta (if *agenda-production*
                                          (1+ *delta-generation*)
                                          0))))))))

(defun %check-production-writes (state compiled delta)
  "An action's commit wrote DELTA: a relation outside the production's
declared writes is recorded on FAILURES (spec §7); the write stands.  A
write declared as (\"family\" \"relation\") covers that relation in
that family only (#469)."
  (let* ((spec (compiled-rule-spec compiled))
         (writes (production-spec-writes spec)))
    (flet ((covered-p (claim rel)
             (some (lambda (w)
                     (and (string= (%write-relation w) rel)
                          (or (null (%write-family w))
                              (string-equal (%write-family w)
                                            (%claim-family-name claim)))))
                   writes)))
      (loop for rel being the hash-keys of delta using (hash-value entries)
            unless (every (lambda (e) (covered-p (car e) rel)) entries)
              do (%note-failure state (rule-spec-name spec)
                                (format nil "wrote ~A outside its writes ~
~S" rel writes)
                                :tag :writes)))))

(defun enable-forward-rules (graph &key (drain :commit) on-queue
                                        on-failure)
  "Derive at commit on GRAPH from now on (spec §5.3): installs the
post-commit hook once per image, registers GRAPH, and runs RUN-RULES
once so the store starts at its fixpoint -- a premise committed
before the opt-in is not a delta; its refusals land on the status.
DRAIN :COMMIT runs the step on the committing thread; :CALLER only
queues, and the application calls FORWARD-STEP on a thread of its own,
told by ON-QUEUE, a function of the graph called outside every lock
whenever a delta is queued (#467; docs/rules.md, \"The queue\").
ON-FAILURE, a function of (NAME TAG TEXT CONDITION), is called for
everything that lands on the status's failures, outside the step's
transactions (#468; docs/rules.md, \"Failure\").  Returns GRAPH's
FORWARD-STATE."
  (check-type drain (member :commit :caller))
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
    (bordeaux-threads:with-lock-held ((forward-state-lock state))
      (setf (forward-state-drain state) drain
            (forward-state-on-queue state) on-queue
            (forward-state-on-failure state) on-failure))
    (unwind-protect
         (let ((*forward-hook-suppressed* t))
           (%note-refused-reports state (run-rules graph)))
      (when first-p
        (let ((queued (bordeaux-threads:with-lock-held
                          ((forward-state-lock state))
                        (setf (forward-state-running-p state) nil)
                        (forward-state-queue state))))
          ;; Under :CALLER the hook already told ON-QUEUE.
          (when (and queued (not (%caller-drains-p state)))
            (forward-step graph)))))
    state))

(defun disable-forward-rules (graph)
  "Stop deriving at commit on GRAPH; pending deltas and timers are
dropped, the timer thread stopped.  A step on another user thread is not
waited for: stop the committers, and under :DRAIN :CALLER the drainer,
first.  T when it was enabled."
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
