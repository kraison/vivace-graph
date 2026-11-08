;;;; tests/rules/forward-tests.lisp -- forward evaluation of rules:
;;;; the post-commit seam, deltas, the step, thresholds, productions
;;;; (docs/superpowers/specs/2026-11-04-forward-rules-design.md, #456).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

;;; The core seam: *after-commit-hooks* (spec §6, plan Task 3)

(defvar *seen* '()
  "What a test hook recorded, newest first.")

(defun seed-one (g)
  "One rt-claim in its own transaction.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-unary :graph g :subject-namespace :host
                         :subject-key "h9" :relation "reachable"
                         :producer "scan-z" :standing :observed)))

(test after-commit-hooks-run-once-outside-the-transaction
  "A hook sees the committed transaction, after the commit, with
*TRANSACTION* no longer bound: it may open a transaction of its own."
  (with-rules-graph (g)
    (let* ((*seen* '())
           (graph-db::*after-commit-hooks*
             (list (lambda (tx)
                     (push (list graph-db::*transaction*
                                 (graph-db::state tx)
                                 (length (graph-db::writes tx)))
                           *seen*)))))
      (seed-one g)
      (is (= 1 (length *seen*)))
      (when *seen*
        (is (null (first (first *seen*))))
        (is (eq :committed (second (first *seen*))))
        (is (= 1 (third (first *seen*))))))))

(test after-commit-hooks-do-not-run-on-abort
  (with-rules-graph (g)
    (let* ((*seen* '())
           (graph-db::*after-commit-hooks*
             (list (lambda (tx) (push tx *seen*)))))
      (signals simple-error
        (with-transaction ((graph-db::transaction-manager g))
          (make-rt-claim-unary :graph g :subject-namespace :host
                               :subject-key "h9" :relation "reachable"
                               :producer "scan-z" :standing :observed)
          (error "abandon the body")))
      (is (null *seen*)))))

(defun lock-free-within-p (lock seconds)
  "True when a helper thread can take LOCK within SECONDS: the caller
does not hold it (nor does anyone else)."
  (let ((got nil))
    (bt:make-thread (lambda ()
                      (bt:with-lock-held (lock) (setf got t)))
                    :name "lock probe")
    (loop repeat (ceiling (* seconds 100))
          until got
          do (sleep 0.01))
    got))

(test after-commit-hooks-run-after-the-retry-exhausted-path
  "With no retries left the whole call runs under the manager lock; the
hook still runs after that lock is released (spec §6)."
  (with-rules-graph (g)
    (let* ((*seen* '())
           (tm (graph-db::transaction-manager g))
           (graph-db::*maximum-transaction-attempts* 0)
           (graph-db::*after-commit-hooks*
             (list (lambda (tx)
                     (declare (ignore tx))
                     (push (lock-free-within-p (graph-db::lock tm) 2)
                           *seen*)))))
      (seed-one g)
      (is (equal '(t) *seen*)))))

;;; Deltas and the positive path (spec §3, §4.1, plan Task 4)

(defun committed-tx (thunk)
  "The TX a commit THUNK makes, captured through *AFTER-COMMIT-HOOKS*."
  (let* ((seen nil)
         (graph-db::*after-commit-hooks*
           (cons (lambda (tx) (setf seen tx))
                 graph-db::*after-commit-hooks*)))
    (funcall thunk)
    seen))

(test a-delta-nets-a-claim-created-and-deleted-in-one-transaction
  (with-rules-graph (g)
    (let ((tx (committed-tx
               (lambda ()
                 (with-transaction ((graph-db::transaction-manager g))
                   (mark-deleted
                    (make-rt-claim-unary :graph g :subject-namespace :host
                                         :subject-key "h9"
                                         :relation "reachable"
                                         :producer "scan-z"
                                         :standing :observed)))))))
      (is (zerop (hash-table-count
                  (graph-db.rules:claim-delta (graph-db::writes tx) g)))))
    ;; Control: a plain create is one +1 under its relation.
    (let* ((tx (committed-tx (lambda () (seed-one g))))
           (delta (graph-db.rules:claim-delta (graph-db::writes tx) g)))
      (is (= 1 (hash-table-count delta)))
      (is (equal '(1) (mapcar #'cdr (gethash "reachable" delta)))))))

(test a-forward-rule-derives-after-the-commit-of-its-last-premise
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (seed g)
    (is (= 2 (length (derived g 'rt-claim "web-hosts"))))
    ;; Nothing left for run-rules to do.
    (is (every (lambda (r) (zerop (graph-db.rules:rule-report-derived r)))
               (graph-db.rules:run-rules g)))))

(defparameter *hosts-web-fwd-head*
  "(claim ?c rt-claim \"host\" ?h \"hosts-web\" nil nil)")
(defparameter *hosts-web-fwd-body*
  "(claim ?a rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")

(test a-forward-rule-derives-through-two-strata-in-one-step
  "HOSTS-WEB reads WEB-HOSTS' head; one commit of the base premises
derives both, the second rule written after the opt-in so the compiled
cache is refreshed by its commit."
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (write-rule g :name "hosts-web" :version "1" :family "rt-claim"
                  :head *hosts-web-fwd-head* :body *hosts-web-fwd-body*)
    (seed g)
    (is (= 2 (length (derived g 'rt-claim "web-hosts"))))
    (is (equal '("h1" "h2")
               (sort (mapcar #'claim-subject-key
                             (derived g 'rt-claim "hosts-web"))
                     #'string<)))))

(test an-opted-out-graph-derives-nothing-at-commit
  (with-rules-graph (g)
    (write-web-hosts g)
    (is-false (graph-db.rules:forward-rules-enabled-p g))
    (seed g)
    (is (null (derived g 'rt-claim "web-hosts")))
    (is (= 2 (graph-db.rules:rule-report-derived
              (graph-db.rules:run-rule g "web-hosts"))))))

;;; Lost derivations (spec §4.2, plan Task 5)

(defun runs-web-premise (g host)
  "HOST's \"runs web\" rt-claim."
  (find "web" (claims-touching g 'rt-claim :host host :role :subject
                               :relation "runs")
        :key #'claim-object-key :test #'string=))

(defun reopen-claim (g claim)
  "CLAIM asserted again: its transaction period re-opened as of now, in
its own transaction.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (let ((c (copy (graph-db:lookup-vertex (graph-db:id claim)))))
      (setf (claim-transaction-extent-sexp c)
            (extent->sexp
             (graph-db.spacetime::%open-transaction-extent
              (graph-db.spacetime::%st-now))))
      (save c))))

(defun derived-object (g name key &key (current t))
  "Rule NAME's derived rt-claim whose object key is KEY."
  (find key (derived g 'rt-claim name :current current)
        :key #'claim-object-key :test #'string=))

(defun derived-subject (g name key &key (current t))
  "Rule NAME's derived rt-claim whose subject key is KEY."
  (find key (derived g 'rt-claim name :current current)
        :key #'claim-subject-key :test #'string=))

(defun run-rules-is-quiet (g)
  "RUN-RULES derives and sweeps nothing: the forward step left the
store at the fixpoint."
  (every (lambda (r) (and (zerop (graph-db.rules:rule-report-derived r))
                          (zerop (graph-db.rules:rule-report-swept r))))
         (graph-db.rules:run-rules g)))

(test a-forward-rule-loses-its-derivation-when-a-current-premise-is-retracted
  (with-rules-graph (g)
    (write-web-hosts-current g)
    (graph-db.rules:enable-forward-rules g)
    (seed g)
    (retract-claim (runs-web-premise g "h2"))
    (is (equal '("h1") (mapcar #'claim-object-key
                               (derived g 'rt-claim "web-hosts"))))
    (let ((h2 (derived-object g "web-hosts" "h2" :current nil)))
      (is (not (null h2)))
      (when h2
        (is-false (claim-current-p h2))
        (is (= 1 (length (graph-db.rules:premises-of g h2))))))
    (is-true (run-rules-is-quiet g))))

(test a-restored-premise-re-opens-the-lost-derivation
  (with-rules-graph (g)
    (write-web-hosts-current g)
    (graph-db.rules:enable-forward-rules g)
    (seed g)
    (let ((premise (runs-web-premise g "h2"))
          (before (graph-db:id (derived-object g "web-hosts" "h2"))))
      (retract-claim premise)
      (is (null (derived-object g "web-hosts" "h2")))
      (reopen-claim g premise)
      (let ((after (derived-object g "web-hosts" "h2")))
        (is (not (null after)))
        (when after
          (is (equalp before (graph-db:id after)))))
      (is (= 2 (length (derived g 'rt-claim "web-hosts" :current nil))))
      (is-true (run-rules-is-quiet g)))))

(defparameter *hosts-web-current-body*
  "(claim ?a rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)
   (claim-current ?a)")

(test a-derivation-lost-in-one-stratum-is-lost-in-the-next
  (with-rules-graph (g)
    (write-web-hosts-current g)
    (write-rule g :name "hosts-web" :version "1" :family "rt-claim"
                  :head *hosts-web-fwd-head* :body *hosts-web-current-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed g)
    (is (= 2 (length (derived g 'rt-claim "hosts-web"))))
    (retract-claim (runs-web-premise g "h2"))
    (is (equal '("h1") (mapcar #'claim-subject-key
                               (derived g 'rt-claim "hosts-web"))))
    (is-false (claim-current-p
               (derived-subject g "hosts-web" "h2" :current nil)))
    (is-true (run-rules-is-quiet g))))

(defparameter *unreached-head*
  "(claim ?c rt-claim \"host\" ?h \"unreached\" nil nil)")
(defparameter *unreached-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (not (claim ?r rt-claim \"host\" ?h \"reachable\" nil nil))")

(defun unreached-hosts (g)
  (sort (mapcar #'claim-subject-key (derived g 'rt-claim "unreached"))
        #'string<))

(test a-not-rule-follows-its-blocker-retracted-then-restored
  (with-rules-graph (g)
    (write-rule g :name "unreached" :version "1" :family "rt-claim"
                  :head *unreached-head* :body *unreached-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed g)                             ; h2 is reachable, h1 is not
    (is (equal '("h1") (unreached-hosts g)))
    (with-transaction ((graph-db::transaction-manager g))
      (mark-deleted (first (claims-touching g 'rt-claim :host "h2"
                                            :role :subject
                                            :relation "reachable"))))
    (is (equal '("h1" "h2") (unreached-hosts g)))
    (with-transaction ((graph-db::transaction-manager g))
      (make-rt-claim-unary :graph g :subject-namespace :host
                           :subject-key "h1" :relation "reachable"
                           :producer "scan-b" :standing :inferred))
    (is (equal '("h2") (unreached-hosts g)))
    (is-true (run-rules-is-quiet g))))

(defun reach-pairs (g)
  (sort (mapcar (lambda (c) (concatenate 'string (claim-subject-key c)
                                         (claim-object-key c)))
                (append (derived g 'rt-claim "tc-base")
                        (derived g 'rt-claim "tc-step")))
        #'string<))

(test a-recursive-stratum-reaches-run-rules-fixpoint-after-a-retraction
  (with-rules-graph (g)
    (write-closure g)
    (graph-db.rules:enable-forward-rules g)
    (link g "a" "b")
    (link g "b" "c")
    (link g "c" "d")
    (is (equal '("ab" "ac" "ad" "bc" "bd" "cd") (reach-pairs g)))
    (retract-claim (first (claims-touching g 'rt-claim :node "b"
                                           :role :subject
                                           :relation "next")))
    (is (equal '("ab" "cd") (reach-pairs g)))
    (is-true (run-rules-is-quiet g))))

(test a-hand-retracted-derived-claim-is-re-opened
  "Review Focus 2: the derivation is still supported, so the step the
retraction's own commit triggers re-opens it; RUN-RULES agrees."
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (seed g)
    (let* ((h1 (derived-object g "web-hosts" "h1"))
           (before (graph-db:id h1)))
      (retract-claim h1)
      (let ((again (derived-object g "web-hosts" "h1")))
        (is (not (null again)))
        (when again
          (is (equalp before (graph-db:id again)))))
      (is (= 2 (length (derived g 'rt-claim "web-hosts" :current nil))))
      (is (null (getf (graph-db.rules:forward-rules-status g) :failures)))
      (is-true (run-rules-is-quiet g)))))

(defparameter *flagged-head*
  "(claim ?c rt-claim \"host\" ?h \"flagged\" nil nil)")
(defparameter *flagged-body*
  "(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
   (claim ?m rt-claim \"sys\" \"s\" \"mode\" \"val\" \"strict\")
   (claim-current ?m)")

(test a-premise-with-many-dependents-takes-the-targeted-path
  "Review Focus 5: one retracted premise cited by 300 derivations is
handled through provenance, each dependent re-derived with its head
bound, not by a full re-run of the rule."
  (with-rules-graph (g)
    (write-rule g :name "flagged" :version "1" :family "rt-claim"
                  :head *flagged-head* :body *flagged-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed-keys g 300 :relation "up")
    (with-transaction ((graph-db::transaction-manager g))
      (make-rt-claim-binary :graph g :subject-namespace :sys
                            :subject-key "s" :relation "mode"
                            :object-namespace :val :object-key "strict"
                            :producer "ops" :standing :asserted))
    (is (= 300 (length (derived g 'rt-claim "flagged"))))
    (retract-claim (first (claims-touching g 'rt-claim :sys "s"
                                           :role :subject
                                           :relation "mode")))
    (is (null (derived g 'rt-claim "flagged")))
    (is (= 300 (length (derived g 'rt-claim "flagged" :current nil))))
    (let ((reports (graph-db.rules::forward-state-last-reports
                    (graph-db.rules::%forward-state g))))
      (is (= 1 (length reports)))
      (when reports
        (is (eq :targeted (graph-db.rules:rule-report-path
                           (first reports))))
        (is (= 300 (graph-db.rules:rule-report-swept (first reports))))))
    (is-true (run-rules-is-quiet g))))

;;; Queue, nesting, failure, status (D5, spec §6, plan Task 6)

(defvar *slow-seconds* 0
  "How long SLOW-TRUE/1 sleeps per solution; set, not bound, since the
step may run in another thread.")

(graph-db::def-global-prolog-functor slow-true/1 (?x cont)
  "Test-only: true, after *SLOW-SECONDS*."
  (declare (ignore ?x))
  (when (plusp *slow-seconds*) (sleep *slow-seconds*))
  (funcall cont))

(graph-db::def-global-prolog-functor refuse-loudly/1 (?x cont)
  "Test-only: a refusal from inside a body."
  (declare (ignore ?x cont))
  (error 'graph-db.rules::rule-run-refusal :tag :rule
         :text "refuse-loudly said no"))

(defun commit-runs-web (g host)
  "HOST runs web, in its own transaction.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :host
                          :subject-key host :relation "runs"
                          :object-namespace :app :object-key "web"
                          :producer "scan-t" :standing :observed)))

(defparameter *slow-web-hosts-body*
  (concatenate 'string *web-hosts-body* " (slow-true ?p)"))

(test two-committing-threads-both-get-their-derivations-and-neither-blocks
  "D5: a commit that arrives while another thread's step runs queues
its delta and returns; the running step drains it."
  (with-rules-graph (g)
    (write-rule g :name "web-hosts" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body *slow-web-hosts-body*)
    (graph-db.rules:enable-forward-rules g)
    (setf *slow-seconds* 1.0)
    (unwind-protect
         (let ((a (bt:make-thread
                   (lambda ()
                     (let ((graph-db:*graph* g))
                       (commit-runs-web g "h1")))
                   :name "committer a"))
               (depth nil)
               (elapsed nil))
           (is-true (wait-until
                     (lambda ()
                       (getf (graph-db.rules:forward-rules-status g)
                             :running-p))
                     3))
           (let ((start (get-internal-real-time)))
             (commit-runs-web g "h2")
             (setf elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second)))
           (setf depth (getf (graph-db.rules:forward-rules-status g)
                             :queue-depth))
           (bt:join-thread a)
           (is (< elapsed 0.5) "the second commit waited ~,2F s" elapsed)
           (is (eql 1 depth))
           (is (equal '("h1" "h2")
                      (sort (mapcar #'claim-object-key
                                    (derived g 'rt-claim "web-hosts"))
                            #'string<))))
      (setf *slow-seconds* 0))))

(test a-nested-commit-defers-its-step-to-the-outermost
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (let ((inside nil))
      (with-transaction ((graph-db::transaction-manager g))
        (commit-runs-web g "h2")        ; nested: its own transaction
        (setf inside (length (derived g 'rt-claim "web-hosts")))
        (make-rt-claim-binary :graph g :subject-namespace :host
                              :subject-key "h1" :relation "runs"
                              :object-namespace :app :object-key "web"
                              :producer "scan-t" :standing :observed))
      (is (= 0 inside))
      (is (equal '("h1" "h2")
                 (sort (mapcar #'claim-object-key
                               (derived g 'rt-claim "web-hosts"))
                       #'string<))))))

(defparameter *refusing-web-hosts-body*
  (concatenate 'string *web-hosts-body* " (refuse-loudly ?p)"))

(test a-failing-rule-body-leaves-the-commit-durable-and-is-reported
  (with-rules-graph (g)
    (write-rule g :name "web-hosts" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body *refusing-web-hosts-body*)
    (graph-db.rules:enable-forward-rules g)
    (finishes (seed g))
    (is (= 2 (length (claims-touching g 'rt-claim :app "web"
                                      :role :object :relation "runs"))))
    (is (null (derived g 'rt-claim "web-hosts")))
    (let* ((status (graph-db.rules:forward-rules-status g))
           (failure (find "web-hosts" (getf status :failures)
                          :key #'car :test #'equal)))
      (is-true (getf status :enabled))
      (is (not (null failure)))
      (when failure
        (is (search "refuse-loudly" (cdr failure)))))
    (let ((r (find "web-hosts" (graph-db.rules:run-rules g)
                   :key #'graph-db.rules:rule-report-rule-name
                   :test #'string=)))
      (is (eq :refused (graph-db.rules:rule-report-outcome r))))))

;;; now/1, instant-offset/3, thresholds and timers (D3, spec §5.2,
;;; plan Task 7)

(defun wait-until (thunk seconds)
  "Poll THUNK every tenth of a second for up to SECONDS; its first
true value, or NIL."
  (loop repeat (ceiling (* seconds 10))
        do (let ((v (funcall thunk)))
             (when v (return v))
             (sleep 0.1))))

(defparameter *settled-head*
  "(claim ?c rtt-claim \"host\" ?h \"settled\" nil nil)")
(defparameter *settled-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (claim-recorded-at ?p ?r) (instant-offset ?r \"PT1S\" ?d)
   (now ?t) (instant>= ?t ?d)")

(defun settled-hosts (g)
  (mapcar #'claim-subject-key (derived g 'rtt-claim "settled")))

(test a-gain-threshold-derives-at-its-flip-instant-with-validity-from-it
  (with-rules-graph (g)
    (write-rule g :name "settled" :version "1" :family "rtt-claim"
                  :head *settled-head* :body *settled-body*)
    (graph-db.rules:enable-forward-rules g)
    (commit-runs-web g "h1")
    (is (null (settled-hosts g)))
    (is (eql 1 (getf (graph-db.rules:forward-rules-status g) :timers)))
    (is (equal '("h1") (wait-until (lambda () (settled-hosts g)) 3)))
    (let* ((premise (runs-web-premise g "h1"))
           (flip (local-time:timestamp+ (claim-recorded-at premise) 1 :sec))
           (c (first (derived g 'rtt-claim "settled"))))
      (is (local-time:timestamp= flip
                                 (bound-earliest
                                  (extent-start (claim-extent c))))))
    (is (eql 0 (getf (graph-db.rules:forward-rules-status g) :timers)))
    (is-true (run-rules-is-quiet g))))

(defparameter *fresh-head*
  "(claim ?c rtt-claim \"host\" ?h \"fresh\" nil nil)")
(defparameter *fresh-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (claim-recorded-at ?p ?r) (instant-offset ?r \"PT1S\" ?d)
   (now ?t) (instant< ?t ?d)")

(defun fresh-hosts (g)
  (mapcar #'claim-subject-key (derived g 'rtt-claim "fresh")))

(test a-loss-threshold-retracts-at-its-flip-instant
  (with-rules-graph (g)
    (write-rule g :name "fresh" :version "1" :family "rtt-claim"
                  :head *fresh-head* :body *fresh-body*)
    (graph-db.rules:enable-forward-rules g)
    (commit-runs-web g "h1")
    (is (equal '("h1") (fresh-hosts g)))
    (let* ((premise (runs-web-premise g "h1"))
           (flip (local-time:timestamp+ (claim-recorded-at premise) 1 :sec))
           (c (first (derived g 'rtt-claim "fresh"))))
      (is (local-time:timestamp= flip
                                 (bound-latest (extent-end (claim-extent c)))))
      (is-true (wait-until (lambda () (null (fresh-hosts g))) 3))
      (let ((again (first (derived g 'rtt-claim "fresh" :current nil))))
        (is (equalp (graph-db:id c) (graph-db:id again)))
        (is-false (claim-current-p again))))
    (is-true (run-rules-is-quiet g))))

(test a-reopened-store-still-fires-a-pending-threshold
  (with-rules-graph-dir (g dir)
    (write-rule g :name "settled" :version "1" :family "rtt-claim"
                  :head *settled-head* :body *settled-body*)
    (graph-db.rules:enable-forward-rules g)
    (commit-runs-web g "h1")
    (close-graph g)
    (is-false (graph-db.rules:forward-rules-enabled-p g))
    (let ((g2 (open-graph *graph-name* (namestring dir))))
      (unwind-protect
           (progn
             (graph-db.rules:enable-forward-rules g2)
             (is (eql 1 (getf (graph-db.rules:forward-rules-status g2)
                              :timers)))
             (is (equal '("h1")
                        (wait-until (lambda () (settled-hosts g2)) 3))))
        (close-graph g2)))))

(test closing-the-graph-stops-its-timers-and-drops-its-queue
  "Review Focus 3: CLOSE-GRAPH implies DISABLE-FORWARD-RULES; nothing
touches the closed store afterwards."
  (with-rules-graph (g)
    (write-rule g :name "settled" :version "1" :family "rtt-claim"
                  :head *settled-head* :body *settled-body*)
    (graph-db.rules:enable-forward-rules g)
    (commit-runs-web g "h1")
    (is (eql 1 (getf (graph-db.rules:forward-rules-status g) :timers)))
    (let ((delta (make-hash-table :test 'equal)))
      (setf (gethash "runs" delta) (list (cons :claim 1)))
      (graph-db.rules::%enqueue-delta (graph-db.rules::%forward-state g)
                                      delta))
    (close-graph g)
    (is (equal '(:enabled nil) (graph-db.rules:forward-rules-status g)))
    (is-true (wait-until
              (lambda ()
                (notany (lambda (th)
                          (string= "forward timer" (bt:thread-name th)))
                        (bt:all-threads)))
              3))
    (sleep 1.2)                         ; past the flip: no touch
    (is (equal '(:enabled nil) (graph-db.rules:forward-rules-status g)))))

(test instant-offset-refuses-months
  (is (equal '("2026-01-08T00:00:00.000000000Z")
             (select-flat (?y)
               (instant-offset "2026-01-01T00:00:00Z" "P1W" ?y))))
  (is (equal '("2026-01-02T01:01:01.000000000Z")
             (select-flat (?y)
               (instant-offset "2026-01-01T00:00:00Z" "P1DT1H1M1S" ?y))))
  (signals graph-db:query-precondition-error
    (select-flat (?y) (instant-offset "2026-01-01T00:00:00Z" "P1M" ?y)))
  (signals graph-db:query-precondition-error
    (select-flat (?y) (instant-offset "2026-01-01T00:00:00Z" "P1Y" ?y))))

(test now-is-withheld-from-free-text
  (with-rules-graph (g)
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:guard-query-text "(now ?t)" g))))

;;; Productions (spec §7, plan Task 8)

(defvar *fired* '()
  "What the test actions saw, newest first: (name event key).")

(defun note-firing (name)
  "An action that records its calls on *FIRED*."
  (lambda (event match)
    (push (list name event (getf match :key)) *fired*)))

(defun fired ()
  (reverse *fired*))

(defparameter *web-watch-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (claim-current ?p)")

(defmacro with-production ((name &rest keys) &body body)
  "DEF-PRODUCTION NAME around BODY, forgotten afterwards."
  `(unwind-protect
        (progn (graph-db.rules:def-production ,name ,@keys) ,@body)
     (graph-db.rules:undef-production ,name)))

(defun firings (g name)
  "The current FIRING claims of production NAME."
  (remove-if-not #'claim-current-p
                 (claims-by-producer g 'graph-db.rules:firing
                                     (format nil "production/~A" name))))

(test a-production-fires-added-once-per-instantiation-and-lost-once
  (with-rules-graph (g)
    (let ((*fired* '()))
      (with-production ("web-watch" :version "1" :family rt-claim
                                    :body *web-watch-body*
                                    :action (note-firing "web-watch"))
        (graph-db.rules:enable-forward-rules g)
        (commit-runs-web g "h1")
        (is (equal '(:added) (mapcar #'second (fired))))
        (commit-runs-web g "h2")
        (is (equal '(:added :added) (mapcar #'second (fired))))
        (is (= 2 (length (firings g "web-watch"))))
        (retract-claim (runs-web-premise g "h1"))
        (is (equal '(:added :added :lost) (mapcar #'second (fired))))
        (is (equal (third (first (fired))) (third (third (fired)))))
        (is (= 1 (length (firings g "web-watch"))))
        ;; RUN-RULES maintains the firing claims and fires no action.
        (is-true (run-rules-is-quiet g))
        (is (= 3 (length (fired))))))))

(test a-production-survives-a-premise-being-swept-and-re-derived
  "The instantiation key is the premises' identity keys, so a premise
deleted and written again is the same instantiation: the firing claim
is re-opened, not duplicated."
  (with-rules-graph (g)
    (let ((*fired* '()))
      (with-production ("web-watch" :version "1" :family rt-claim
                                    :body *web-watch-body*
                                    :action (note-firing "web-watch"))
        (graph-db.rules:enable-forward-rules g)
        (commit-runs-web g "h1")
        (let ((before (graph-db:id (first (firings g "web-watch")))))
          (with-transaction ((graph-db::transaction-manager g))
            (mark-deleted (runs-web-premise g "h1")))
          (is (null (firings g "web-watch")))
          (commit-runs-web g "h1")
          (is (equal '(:added :lost :added) (mapcar #'second (fired))))
          (is (= 1 (length (remove-duplicates (mapcar #'third (fired))
                                              :test #'equal))))
          (is (equalp before
                      (graph-db:id (first (firings g "web-watch"))))))))))

(test a-stored-production-naming-an-unregistered-action-is-refused-at-commit
  (with-rules-graph (g)
    (signals graph-db.rules:rule-compile-error
      (with-transaction ((graph-db::transaction-manager g))
        (graph-db.rules:make-production
         :graph g :name "ghost" :version "1" :family "rt-claim"
         :body *web-watch-body* :salience 0 :writes '()
         :action "nobody")))
    (is (null (graph-db:index-lookup g 'graph-db.rules:production
                                     '(graph-db.rules::name) "ghost")))))

(test a-production-that-writes-then-signals-keeps-both-and-is-reported
  "Review Focus 4: the action's write stands, the firing claim stands,
the failure is on the status, and the next commit's delta is still
processed."
  (with-rules-graph (g)
    (let ((*fired* '()))
      (with-production
          ("web-mark" :version "1" :family rt-claim
                      :body *web-watch-body* :writes '("marked")
                      :action (lambda (event match)
                                (declare (ignore match))
                                (when (eq event :added)
                                  (with-transaction
                                      ((graph-db::transaction-manager g))
                                    (make-rt-claim-unary
                                     :graph g :subject-namespace :host
                                     :subject-key "h1" :relation "marked"
                                     :producer "web-mark"
                                     :standing :inferred))
                                  (error "boom"))))
        (graph-db.rules:enable-forward-rules g)
        (finishes (commit-runs-web g "h1"))
        (is (= 1 (length (claims-touching g 'rt-claim :host "h1"
                                          :role :subject
                                          :relation "marked"))))
        (is (= 1 (length (firings g "web-mark"))))
        (let ((failure (find "web-mark"
                             (getf (graph-db.rules:forward-rules-status g)
                                   :failures)
                             :key #'car :test #'equal)))
          (is (not (null failure)))
          (when failure (is (search "boom" (cdr failure)))))
        (commit-runs-web g "h2")
        (is (= 2 (length (firings g "web-mark"))))))))

(test agenda-order-is-salience-then-epoch
  (with-rules-graph (g)
    (let ((*fired* '()))
      (with-production ("low" :version "1" :family rt-claim
                              :body *web-watch-body* :salience 0
                              :action (note-firing "low"))
        (with-production ("high" :version "1" :family rt-claim
                                 :body *web-watch-body* :salience 10
                                 :action (note-firing "high"))
          (graph-db.rules:enable-forward-rules g)
          (commit-runs-web g "h1")
          (is (equal '("high" "low") (mapcar #'first (fired)))))))))

(test firing-claims-are-readable-and-carry-the-production-version
  (with-rules-graph (g)
    (let ((*fired* '()))
      (with-production ("web-watch" :version "3" :family rt-claim
                                    :body *web-watch-body*
                                    :action (note-firing "web-watch"))
        (graph-db.rules:enable-forward-rules g)
        (commit-runs-web g "h1")
        (let ((f (first (firings g "web-watch"))))
          (is (not (null f)))
          (when f
            (is (string= "web-watch" (claim-relation f)))
            (is (string= "3" (claim-rule-version f)))
            (is (eq :production (claim-subject-namespace f)))
            (is (= 16 (length (claim-subject-key f))))
            (is (every (lambda (ch) (digit-char-p ch 16))
                       (claim-subject-key f)))
            ;; Its provenance names the premise.
            (is (= 1 (length (graph-db.rules:premises-of g f))))))))))

;;; Standing-query equivalence (spec §8, plan Task 9)

(defvar *delivered* '()
  "Agenda deliveries, newest first: (query event key).")

(defun deliver (query)
  (lambda (event match)
    (push (list query event (getf match :key)) *delivered*)))

(defun delivered-keys (query event)
  (loop for (q e k) in *delivered*
        when (and (string= q query) (eq e event)) collect k))

(defparameter *q1-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (claim-current ?p)")
(defparameter *q2-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a) (claim-current ?p)
   (claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)")
(defparameter *q3-body*
  "(claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)")

(defun q1-keys (g)
  (declare (ignore g))
  (mapcar #'claim-identity-key
          (select-flat (?p)
            (claim ?p rt-claim "host" ?h "runs" "app" "web")
            (claim-current ?p))))

(defun joined-keys (claims)
  "CLAIMS' identity keys joined with |, in the order given: a match's
:KEY for those premises in body order (#471)."
  (format nil "~{~A~^|~}" (mapcar #'claim-identity-key claims)))

(defun q2-keys (g)
  (declare (ignore g))
  (mapcar #'joined-keys
          (select (:max-inferences 10000000) (?p ?u)
            (claim ?p rt-claim "host" ?h "runs" "app" ?a) (claim-current ?p)
            (claim ?u rt-claim "host" ?h "up" nil nil) (claim-current ?u))))

(defun q3-keys (g)
  (declare (ignore g))
  (mapcar #'claim-identity-key
          (select-flat (?u)
            (claim ?u rt-claim "host" ?h "up" nil nil)
            (claim-current ?u))))

(defun host-claim (g host relation &optional object)
  (find-if (lambda (c) (or (null object)
                           (string= object (claim-object-key c))))
           (claims-touching g 'rt-claim :host host :role :subject
                            :relation relation)))

(defun standing-query-step (g i)
  "Commit number I of the script: a create, a second create, a
retraction, a commit touching nothing read, a deletion."
  (let ((host (format nil "k~2,'0d" i)))
    (ecase (mod i 5)
      (0 (commit-runs-web g host))
      (1 (with-transaction ((graph-db::transaction-manager g))
           (make-rt-claim-binary :graph g :subject-namespace :host
                                 :subject-key host :relation "runs"
                                 :object-namespace :app :object-key "db"
                                 :producer "scan-t" :standing :observed)))
      (2 (retract-claim (host-claim g (format nil "k~2,'0d" (- i 2))
                                    "runs" "web")))
      (3 (with-transaction ((graph-db::transaction-manager g))
           (make-rt-claim-unary :graph g :subject-namespace :host
                                :subject-key host :relation "noise"
                                :producer "scan-t" :standing :observed)))
      (4 (with-transaction ((graph-db::transaction-manager g))
           (mark-deleted (host-claim g host "up")))))))

(defun set-equal-p (a b)
  (and (= (length a) (length (remove-duplicates a :test #'string=)))
       (null (set-exclusive-or a b :test #'string=))))

(test standing-queries-deliver-exactly-the-set-differences-of-full-reevaluation
  "Spec §8: three queries as productions over forty commits; after
each commit the deliveries since the last equal the set differences of
re-evaluating the query from scratch, as sets, with no duplicate."
  (with-rules-graph (g)
    (let ((*delivered* '())
          (mismatches '())
          (total 0))
      (unwind-protect
           (progn
             (graph-db.rules:def-production "q1" :version "1"
               :family rt-claim :body *q1-body* :action (deliver "q1"))
             (graph-db.rules:def-production "q2" :version "1"
               :family rt-claim :body *q2-body* :action (deliver "q2"))
             (graph-db.rules:def-production "q3" :version "1"
               :family rt-claim :body *q3-body* :action (deliver "q3"))
             (seed-keys g 600 :relation "up")       ; q3 exceeds 500 rows
             (graph-db.rules:enable-forward-rules g)
             (is (= 600 (length (firings g "q3"))))
             (setf *delivered* '())
             (let ((prev (list (q1-keys g) (q2-keys g) (q3-keys g))))
               (dotimes (i 40)
                 (setf *delivered* '())
                 (standing-query-step g i)
                 (incf total (length *delivered*))
                 (let ((now (list (q1-keys g) (q2-keys g) (q3-keys g))))
                   (loop for q in '("q1" "q2" "q3")
                         for before in prev
                         for after in now
                         do (let ((added (set-difference after before
                                                         :test #'string=))
                                  (removed (set-difference before after
                                                           :test #'string=)))
                              (unless (and (set-equal-p
                                            (delivered-keys q :added) added)
                                           (set-equal-p
                                            (delivered-keys q :lost) removed))
                                (push (list i q :added added
                                            (delivered-keys q :added)
                                            :removed removed
                                            (delivered-keys q :lost))
                                      mismatches))))
                   (when (= 3 (mod i 5))
                     (is (null *delivered*)))
                   (setf prev now)))))
        (graph-db.rules:undef-production "q1")
        (graph-db.rules:undef-production "q2")
        (graph-db.rules:undef-production "q3"))
      (is (null mismatches) "mismatches: ~S" mismatches)
      (is (plusp total))
      (is-true (run-rules-is-quiet g)))))

;;; Final review fixes (plan ledger, Final:)

(test a-nested-commit-is-drained-by-a-claim-free-outer-commit
  "Review finding 1: the outermost commit drains whatever is queued,
even when it wrote no claim itself."
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (with-transaction ((graph-db::transaction-manager g))
      (commit-runs-web g "h2")          ; nested: queued, not run
      ;; The outer transaction writes a rule record, not a claim.
      (let ((c (copy (first (graph-db:index-lookup
                             g 'graph-db.rules:rule
                             '(graph-db.rules::name) "web-hosts")))))
        (setf (graph-db.rules:rule-version c) "2")
        (save c)))
    (is (equal '("h2") (mapcar #'claim-object-key
                               (derived g 'rt-claim "web-hosts"))))
    (is (eql 0 (getf (graph-db.rules:forward-rules-status g)
                     :queue-depth)))))

(test a-retried-derive-starts-its-change-list-afresh
  "Review finding 2: the derive thunk a VALIDATION-CONFLICT re-runs
clears the changes its aborted attempt noted, so a step never reports
an uncommitted claim or fires an action twice."
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (let ((graph-db.rules::*forward-changes*
              (list (cons :stale-from-an-aborted-attempt 1))))
        (graph-db.rules:run-rule g r)
        (is (= 2 (length graph-db.rules::*forward-changes*)))
        (is (notany (lambda (ch) (eq :stale-from-an-aborted-attempt
                                     (car ch)))
                    graph-db.rules::*forward-changes*))))))

(defparameter *echo-body*
  "(claim ?p rt-claim \"host\" ?h \"ping\" nil nil) (claim-current ?p)")

(test a-step-that-never-quiesces-is-stopped-at-the-rounds-bound
  "Review finding 3 (spec §4.3): an action that keeps writing what it
reads cannot pin the committing thread; the step stops after
*RULES-MAX-ROUNDS* drains and the status says so."
  (with-rules-graph (g)
    (let ((*fired* '())
          (graph-db.rules:*rules-max-rounds* 5))
      (with-production
          ("echo" :version "1" :family rt-claim :body *echo-body*
                  :writes '()
                  :action (lambda (event match)
                            (when (eq event :added)
                              (push (list "echo" event (getf match :key))
                                    *fired*)
                              (with-transaction
                                  ((graph-db::transaction-manager g))
                                (make-rt-claim-unary
                                 :graph g :subject-namespace :host
                                 :subject-key (format nil "h~D"
                                                      (length *fired*))
                                 :relation "ping" :producer "echo"
                                 :standing :inferred)))))
        (graph-db.rules:enable-forward-rules g)
        (finishes
          (with-transaction ((graph-db::transaction-manager g))
            (make-rt-claim-unary :graph g :subject-namespace :host
                                 :subject-key "h0" :relation "ping"
                                 :producer "scan-t" :standing :observed)))
        (let ((status (graph-db.rules:forward-rules-status g)))
          (is (<= (length *fired*) 7))
          (is (find :rounds (getf status :failures) :key #'car))
          (is (eql 0 (getf status :queue-depth)))
          (is-false (getf status :running-p)))))))

;;; A match's bindings and premises, the body-ordered instantiation
;;; key, MATCH-ROW and FIND-PRODUCTION (#466, #471)

(defvar *matches* '()
  "Full deliveries, newest first: (name event match).")

(defun note-match (name)
  (lambda (event match) (push (list name event match) *matches*)))

(defun matches (name &optional event)
  "The matches delivered to production NAME, oldest first, EVENT's only
when given."
  (loop for (n e m) in (reverse *matches*)
        when (and (string= n name) (or (null event) (eq e event)))
          collect m))

(defun binding-names (match)
  (mapcar #'car (getf match :bindings)))

(defun binding (match name)
  (cdr (assoc name (getf match :bindings) :test #'string=)))

(defun relation-of (x)
  (and x (claim-relation x)))

(defun commit-host-claim (g host relation producer &optional object)
  "One claim about HOST by PRODUCER, binary when OBJECT (an app) is
given, in its own transaction.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (if object
        (make-rt-claim-binary :graph g :subject-namespace :host
                              :subject-key host :relation relation
                              :object-namespace :app :object-key object
                              :producer producer :standing :observed)
        (make-rt-claim-unary :graph g :subject-namespace :host
                             :subject-key host :relation relation
                             :producer producer :standing :observed))))

(defparameter *runs-app-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a) (claim-current ?p)")

(test a-match-carries-every-body-variable-in-first-appearance-order
  "#466 item 1: :BINDINGS is the body's variables, premises included,
and nothing synthesised."
  (with-rules-graph (g)
    (let ((*matches* '()))
      (with-production ("row" :version "1" :family rt-claim
                              :body *runs-app-body*
                              :action (note-match "row"))
        (graph-db.rules:enable-forward-rules g)
        (commit-runs-web g "h1")
        (let ((m (first (matches "row" :added))))
          (is (equal '("?P" "?H" "?A") (binding-names m)))
          (is (equal (claim-identity-key (runs-web-premise g "h1"))
                     (and (binding m "?P")
                          (claim-identity-key (binding m "?P")))))
          (is (equal "h1" (binding m "?H")))
          (is (equal "web" (binding m "?A"))))))))

(defparameter *runs-and-up-body*
  "(claim ?a rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
   (claim ?b rt-claim \"host\" ?h \"up\" nil nil)")

(test premises-pair-with-their-own-variables-whatever-the-key-order
  "#466 item 2: ?a is the runs claim and ?b the up claim whichever
producer's identity key sorts first, and :PREMISES are in body order."
  (with-rules-graph (g)
    (let ((*matches* '()))
      (with-production ("pair" :version "1" :family rt-claim
                               :body *runs-and-up-body*
                               :action (note-match "pair"))
        (graph-db.rules:enable-forward-rules g)
        (commit-host-claim g "h1" "runs" "z-scan" "web")
        (commit-host-claim g "h1" "up" "a-scan")
        (commit-host-claim g "h2" "runs" "a-scan" "web")
        (commit-host-claim g "h2" "up" "z-scan")
        (is (= 2 (length (matches "pair" :added))))
        (dolist (m (matches "pair" :added))
          (is (equal "runs" (relation-of (binding m "?A"))))
          (is (equal "up" (relation-of (binding m "?B"))))
          (is (equal '("runs" "up")
                     (mapcar #'relation-of (getf m :premises)))))))))

(defparameter *self-join-body*
  "(claim ?a rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?a)
   (claim ?b rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?b)")

(defun self-join-rows (g)
  (declare (ignore g))
  (mapcar #'joined-keys
          (select (:max-inferences 10000000) (?a ?b)
            (claim ?a rt-claim "host" ?h "up" nil nil) (claim-current ?a)
            (claim ?b rt-claim "host" ?h "up" nil nil) (claim-current ?b))))

(defun up-claim-by (g host producer)
  (find producer (claims-touching g 'rt-claim :host host :role :subject
                                  :relation "up")
        :key #'claim-producer :test #'string=))

(test a-self-join-delivers-one-firing-per-row
  "#471: the instantiation key keeps body position, so (a,b) and (b,a)
are two instantiations; a retraction loses one firing per row; a
premise deleted and re-written re-opens its firings; a reopened store
keeps refraction."
  (with-rules-graph-dir (g dir)
    (let ((*matches* '()))
      (unwind-protect
           (progn
             (graph-db.rules:def-production "self" :version "1"
               :family rt-claim :body *self-join-body*
               :action (note-match "self"))
             (graph-db.rules:enable-forward-rules g)
             (commit-host-claim g "h1" "up" "scan-a")
             (commit-host-claim g "h1" "up" "scan-b")
             (is (= 4 (length (self-join-rows g))))
             (is (= 4 (length (firings g "self"))))
             (is (set-equal-p (self-join-rows g)
                              (mapcar (lambda (m) (getf m :key))
                                      (matches "self" :added))))
             (dolist (m (matches "self" :added))
               (is (equal (getf m :key)
                          (joined-keys (list (binding m "?A")
                                             (binding m "?B"))))))
             (retract-claim (up-claim-by g "h1" "scan-b"))
             (is (= 1 (length (self-join-rows g))))
             (is (= 3 (length (matches "self" :lost))))
             (is (= 1 (length (firings g "self"))))
             (is-true (run-rules-is-quiet g))
             (let ((all-before (claims-by-producer g 'graph-db.rules:firing
                                                   "production/self")))
               (is (= 4 (length all-before)))
               (with-transaction ((graph-db::transaction-manager g))
                 (mark-deleted (up-claim-by g "h1" "scan-b")))
               (commit-host-claim g "h1" "up" "scan-b")
               (is (= 4 (length (firings g "self"))))
               (is (= 4 (length (claims-by-producer
                                 g 'graph-db.rules:firing
                                 "production/self"))))
               (is (= 7 (length (matches "self" :added))))
               (is-true (run-rules-is-quiet g)))
             (close-graph g)
             (let ((g2 (open-graph *graph-name* (namestring dir)))
                   (delivered (length *matches*)))
               (unwind-protect
                    (let ((graph-db:*graph* g2))
                      (graph-db.rules:enable-forward-rules g2)
                      (is (= 4 (length (firings g2 "self"))))
                      (is (= delivered (length *matches*)))
                      (is-true (run-rules-is-quiet g2)))
                 (close-graph g2))))
        (graph-db.rules:undef-production "self")))))

(defparameter *any-variable-body*
  "(claim ?c rt-claim \"host\" ?instantiation-key \"runs\" \"app\" \"web\")
   (claim-current ?c)")

(test a-production-body-may-name-any-variable
  "#466 item 3: ?c and ?instantiation-key are the author's."
  (with-rules-graph (g)
    (let ((*matches* '()))
      (with-production ("anyvar" :version "1" :family rt-claim
                                 :body *any-variable-body*
                                 :action (note-match "anyvar"))
        (graph-db.rules:enable-forward-rules g)
        (commit-runs-web g "h1")
        (is (= 1 (length (matches "anyvar" :added))))
        (let ((m (first (matches "anyvar" :added))))
          (is (equal '("?C" "?INSTANTIATION-KEY") (binding-names m)))
          (is (equal "h1" (binding m "?INSTANTIATION-KEY"))))))))

(defun production-refusal (g name body)
  "The reason COMPILE-PRODUCTION refuses BODY under NAME, or NIL."
  (unwind-protect
       (progn
         (graph-db.rules:def-production name :version "1" :family rt-claim
           :body body :action (lambda (e m) (list e m)))
         (handler-case (progn (graph-db.rules:compile-production g name)
                              nil)
           (graph-db.rules:rule-compile-error (c)
             (graph-db.rules:rule-compile-error-reason c))))
    (graph-db.rules:undef-production name)))

(test production-refusals-name-the-documented-rule
  "#466 item 4: no claim/7 goal, nine premises, and a claim read only
under OR each refuse in the documented words, not as an unbound head
key."
  (with-rules-graph (g)
    (is (search "needs a claim/7 goal"
                (production-refusal g "none" "(= ?x 1)")))
    (is (search "at most eight"
                (production-refusal
                 g "nine"
                 (format nil "~{(claim ?p~D rt-claim \"host\" ?h \"up\" ~
nil nil)~^ ~}" '(1 2 3 4 5 6 7 8 9)))))
    (is (search "needs a claim/7 goal"
                (production-refusal
                 g "orred"
                 "(or (claim ?a rt-claim \"host\" ?h \"a\" nil nil)
                      (claim ?a rt-claim \"host\" ?h \"b\" nil nil))")))))

(defun match-rows (name)
  (mapcar (lambda (m) (nth-value 1 (graph-db.rules:match-row m)))
          (matches name :added)))

(test match-row-agrees-with-the-guarded-runner-cell-for-cell
  "#466 item 6: MATCH-ROW renders a match as RUN-GUARDED-PROLOG :DATA
renders that solution, for a single premise, a join and a self-join."
  (with-rules-graph (g)
    (let ((*matches* '()))
      (with-production ("row" :version "1" :family rt-claim
                              :body *runs-app-body*
                              :action (note-match "row"))
        (with-production ("pair" :version "1" :family rt-claim
                                 :body *runs-and-up-body*
                                 :action (note-match "pair"))
          (with-production ("self" :version "1" :family rt-claim
                                   :body *self-join-body*
                                   :action (note-match "self"))
            (graph-db.rules:enable-forward-rules g)
            (commit-host-claim g "h1" "runs" "z-scan" "web")
            (commit-host-claim g "h1" "up" "a-scan")
            (commit-host-claim g "h1" "up" "scan-b")
            (commit-host-claim g "h2" "runs" "a-scan" "db")
            (loop for (name body) in (list (list "row" *runs-app-body*)
                                           (list "pair" *runs-and-up-body*)
                                           (list "self" *self-join-body*))
                  do (multiple-value-bind (cols rows)
                         (graph-db.query:run-guarded-prolog body g)
                       (is (plusp (length rows)) "~A: no rows" name)
                       (is (equal cols
                                  (nth-value 0 (graph-db.rules:match-row
                                                (first (matches name
                                                                :added)))))
                           "~A: columns" name)
                       (is (null (set-exclusive-or rows (match-rows name)
                                                   :test #'equal))
                           "~A: rows ~S vs matches ~S" name rows
                           (match-rows name))))))))))

(test find-production-answers-the-stored-record-or-nil
  "#466 item 7."
  (with-rules-graph (g)
    (is (null (graph-db.rules:find-production g "stored")))
    (setf (gethash "stored-action" graph-db.rules:*production-actions*)
          (lambda (e m) (list e m)))
    (unwind-protect
         (progn
           (with-transaction ((graph-db::transaction-manager g))
             (graph-db.rules:make-production
              :graph g :name "stored" :version "1" :family "rt-claim"
              :body *runs-app-body* :salience 0 :writes '()
              :action "stored-action"))
           (let ((p (graph-db.rules:find-production g "stored")))
             (is (graph-db.rules:production-p p))
             (is (equal "stored" (and p (graph-db.rules:production-name p))))
             (with-transaction ((graph-db::transaction-manager g))
               (mark-deleted p)))
           (is (null (graph-db.rules:find-production g "stored"))))
      (remhash "stored-action" graph-db.rules:*production-actions*))))

;;; A free relation under a literal family (#469)

(defparameter *family-wide-body*
  "(claim ?p rt-claim \"host\" ?k ?r ?ons ?ok) (claim-current ?p)")

(defparameter *flag-from-up-head*
  "(claim ?c rt-claim \"host\" ?k \"flag\" nil nil)")
(defparameter *flag-from-up-body*
  "(claim ?p rt-claim \"host\" ?k \"up\" nil nil) (claim-current ?p)")

(test a-family-wide-read-fires-on-every-relation-of-the-family
  "#469: a production over every relation of rt-claim under host
compiles, fires :added on a claim of any relation -- a derived one in
the same step included, so it is stratified after the rule deriving
it -- and :lost on a retraction."
  (with-rules-graph (g)
    (let ((*matches* '()))
      (unwind-protect
           (progn
             (graph-db.rules:def-rule "flag-from-up" :version "1"
               :family rt-claim :head *flag-from-up-head*
               :body *flag-from-up-body*)
             (with-production ("wide" :version "1" :family rt-claim
                                      :body *family-wide-body*
                                      :action (note-match "wide"))
               (graph-db.rules:enable-forward-rules g)
               (commit-host-claim g "h1" "up" "scan-a")
               (is (equal '("flag" "up")
                          (sort (mapcar (lambda (m) (binding m "?R"))
                                        (matches "wide" :added))
                                #'string<)))
               (commit-host-claim g "h2" "runs" "scan-a" "web")
               (is (equal '("runs")
                          (mapcar (lambda (m) (binding m "?R"))
                                  (nthcdr 2 (matches "wide" :added)))))
               (is (equal '("web")
                          (mapcar (lambda (m) (binding m "?OK"))
                                  (nthcdr 2 (matches "wide" :added)))))
               (retract-claim (up-claim-by g "h1" "scan-a"))
               (is (= 2 (length (matches "wide" :lost))))
               (is (= 1 (length (firings g "wide"))))
               (is-true (run-rules-is-quiet g))))
        (graph-db.rules:undef-rule "flag-from-up")))))

(test a-claim-of-another-family-does-not-touch-a-family-wide-read
  (with-rules-graph (g)
    (let ((*matches* '()))
      (with-production ("wide" :version "1" :family rt-claim
                               :body *family-wide-body*
                               :action (note-match "wide"))
        (graph-db.rules:enable-forward-rules g)
        (with-transaction ((graph-db::transaction-manager g))
          (make-rtt-claim-binary :graph g :subject-namespace :host
                                 :subject-key "h1" :relation "runs"
                                 :object-namespace :app :object-key "web"
                                 :producer "deploy" :standing :observed
                                 :extent (interval (ts 2026 2 1)
                                                   (ts 2026 6 30))))
        (is (null (matches "wide")))
        (is (null (firings g "wide")))
        (is-true (run-rules-is-quiet g))))))

(test a-free-relation-stays-refused-without-a-literal-family-and-namespace
  "#469: the family unbound, or the namespace unbound, is still the
every-relation refusal."
  (with-rules-graph (g)
    (is (search "bind the relation"
                (production-refusal
                 g "nofam" "(claim ?p ?f \"host\" ?k ?r nil nil)")))
    (is (search "bind the relation"
                (production-refusal
                 g "nons" "(claim ?p rt-claim ?ns ?k ?r nil nil)")))))

(test a-family-wide-read-is-checked-against-a-writes-family
  "#469: a write names its family as (\"family\" \"relation\"); one in
another family compiles beside the read, one in the read family is
\"reads what it writes\", a bare relation is asked for its family."
  (with-rules-graph (g)
    (flet ((refusal (name writes)
             (unwind-protect
                  (progn
                    (graph-db.rules:def-production name :version "1"
                      :family rt-claim :body *family-wide-body*
                      :writes writes :action (lambda (e m) (list e m)))
                    (handler-case
                        (progn (graph-db.rules:compile-production g name)
                               nil)
                      (graph-db.rules:rule-compile-error (c)
                        (graph-db.rules:rule-compile-error-reason c))))
               (graph-db.rules:undef-production name))))
      (is (null (refusal "other" '(("rtt-claim" "flag")))))
      (is (search "reads what it writes"
                  (refusal "own" '(("rt-claim" "flag")))))
      (is (search "family" (refusal "bare" '("flag")))))))

(test a-write-of-the-declared-relation-into-another-family-is-reported
  "#469: the run-time check reads a pair-shaped write as family and
relation, so the declared relation written into a family other than
the declared one is outside the writes."
  (with-rules-graph (g)
    (with-production ("wide-writer" :version "1" :family rt-claim
                                    :body *family-wide-body*
                                    :writes '(("rtt-claim" "flag"))
                                    :action
                                    (lambda (event match)
                                      (declare (ignore match))
                                      (when (eq event :added)
                                        (with-transaction
                                            ((graph-db::transaction-manager
                                              g))
                                          (make-rtu-claim-unary
                                           :graph g :subject-namespace :host
                                           :subject-key "h1"
                                           :relation "flag"
                                           :producer "wide-writer"
                                           :standing :inferred)))))
      (graph-db.rules:enable-forward-rules g)
      (commit-host-claim g "h1" "up" "scan-a")
      (let ((failure (find "wide-writer"
                           (getf (graph-db.rules:forward-rules-status g)
                                 :failures)
                           :key #'car :test #'equal)))
        (is (not (null failure)))
        (when failure
          (is (search "outside its writes" (cdr failure))))))))

(defparameter *seen-head*
  "(claim ?c rtu-claim \"host\" ?k \"seen\" nil nil)")

(test a-def-rule-reads-a-family-wide-under-the-same-rules
  "#469: a plain rule whose head is in another family reads rt-claim
wide and derives from a claim of any relation; one whose head is in the
read family is refused."
  (with-rules-graph (g)
    (unwind-protect
         (progn
           (graph-db.rules:def-rule "seen" :version "1" :family rtu-claim
             :head *seen-head* :body *family-wide-body*)
           (graph-db.rules:enable-forward-rules g)
           (commit-host-claim g "h1" "up" "scan-a")
           (commit-host-claim g "h2" "runs" "scan-a" "web")
           (is (equal '("h1" "h2")
                      (sort (mapcar #'claim-subject-key
                                    (derived g 'rtu-claim "seen"))
                            #'string<)))
           (is-true (run-rules-is-quiet g)))
      (graph-db.rules:undef-rule "seen"))
    (signals graph-db.rules:rule-compile-error
      (graph-db.rules:compile-rule
       g (spec :name "own-family" :version "1" :family "rt-claim"
               :head *flag-from-up-head* :body *family-wide-body*)))))

;;; Standing queries: which bodies deliver exactly their rows (#470)

(defparameter *admitted-bodies*
  (list *runs-app-body* *runs-and-up-body* *self-join-body*
        *family-wide-body*
        "(claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)
         (not (claim ?d rt-claim \"host\" ?h \"down\" nil nil))"
        "(claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)
         (> ?h \"k10\")"
        "(claim ?u rt-claim \"host\" ?h \"up\" nil nil)
         (claim-standing ?u \"observed\")"
        "(claim ?u rt-claim \"host\" ?h \"up\" nil nil)
         (claim-producer ?u ?who) (claim-relation ?u ?r)"
        "(claim ?u rt-claim \"host\" ?h \"up\" nil nil)
         (claim-absent ?a rt-claim \"host\" ?h \"down\" nil nil)")
  "Bodies STANDING-QUERY-PROBLEMS admits: a production over each
delivers exactly the query's rows.")

(defparameter *refused-bodies*
  '(("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (claim-producer ?x \"p\")"
     "claim-producer")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (claim-standing ?p ?s)"
     "claim-standing")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
      (or (claim ?q rt-claim \"host\" ?h \"a\" nil nil)
          (claim ?q rt-claim \"host\" ?h \"b\" nil nil))"
     "or")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
      (once (claim ?q rt-claim \"host\" ?h \"b\" nil nil))"
     "once")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (retract ?p)" "retract")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (write ?h)" "write")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (read ?x)" "read")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (trigger \"x\")"
     "trigger")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (repeat)" "repeat")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (is-a ?p rt-claim)"
     "is-a")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
      (node-slot-value ?p \"standing\" ?s)" "node-slot-value")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil) (claimed ?p ?a ?b ?c)"
     "claimed")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
      (claim-valid-from ?p ?from)" "claim-valid-from")
    ("(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
      (now ?t) (claim-recorded-at ?p ?r) (instant< ?r ?t)" "now")
    ("(= ?x 1)" "claim/7")
    ("(claim ?p rt-claim \"host\" ?h ?r nil nil)
      (claim ?q ?f \"host\" ?h \"up\" nil nil)" "relation"))
  "Bodies STANDING-QUERY-PROBLEMS refuses, each with a word its reason
must carry: the issue's table (#470) and the admitted list's edges.")

(test standing-query-problems-admits-and-refuses-as-documented
  (with-rules-graph (g)
    (dolist (body *admitted-bodies*)
      (is (null (graph-db.rules:standing-query-problems body g))
          "admitted body refused: ~A~%~S" body
          (graph-db.rules:standing-query-problems body g)))
    (loop for (body word) in *refused-bodies*
          do (let ((problems (graph-db.rules:standing-query-problems body g)))
               (is (and problems
                        (some (lambda (p) (search word p)) problems))
                   "~A: expected a reason naming ~A, got ~S"
                   body word problems)))))

(defun guarded-rows (body g)
  (nth-value 1 (graph-db.query:run-guarded-prolog
                body g :max-inferences 10000000)))

(test a-refused-body-misses-rows-a-production-cannot-deliver
  "#470: three of the table's rows shown missing -- a generator over
the producer index, a mutable slot read into the row, a claim read
under OR -- each as a query whose rows change with no action call."
  (with-rules-graph (g)
    (let ((*matches* '())
          (producer-body
            "(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
             (claim-producer ?x \"p\")")
          (standing-body
            "(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
             (claim-standing ?p ?s)")
          (or-body
            "(claim ?p rt-claim \"host\" ?h \"up\" nil nil)
             (or (claim ?q rt-claim \"host\" ?h \"a\" nil nil)
                 (claim ?q rt-claim \"host\" ?h \"b\" nil nil))"))
      (with-production ("gen" :version "1" :family rt-claim
                              :body producer-body :action (note-match "gen"))
        (with-production ("mut" :version "1" :family rt-claim
                                :body standing-body
                                :action (note-match "mut"))
          (with-production ("orred" :version "1" :family rt-claim
                                    :body or-body
                                    :action (note-match "orred"))
            (graph-db.rules:enable-forward-rules g)
            (commit-host-claim g "h1" "up" "scan-t")
            (commit-host-claim g "h1" "noise" "p")
            ;; The up commit found no claim by p yet, and p's commits
            ;; touch nothing the body reads: rows 1, then 2, calls 0.
            (is (= 1 (length (guarded-rows producer-body g))))
            (is (null (matches "gen")))
            (commit-host-claim g "h2" "other" "p")
            (is (= 2 (length (guarded-rows producer-body g))))
            (is (null (matches "gen")))
            (let ((before (guarded-rows standing-body g)))
              (with-transaction ((graph-db::transaction-manager g))
                (let ((copy (graph-db::copy (up-claim-by g "h1" "scan-t"))))
                  (setf (claim-standing copy) :inferred)
                  (graph-db::save copy)))
              (is (not (equal before (guarded-rows standing-body g))))
              (is (= 1 (length (matches "mut")))))
            (is (null (guarded-rows or-body g)))
            (commit-host-claim g "h1" "b" "scan-t")
            (is (= 1 (length (guarded-rows or-body g))))
            (is (null (matches "orred")))))))))

(defvar *rows* (make-hash-table :test 'equal)
  "Firing object key -> the row delivered with its :ADDED.")

(defvar *row-deliveries* '()
  "Row deliveries, newest first: (query event cells).")

(defun deliver-row (query)
  "An action remembering each :ADDED row by (query . instantiation),
since productions over the same premises share the instantiation."
  (lambda (event match)
    (let ((key (cons query (claim-object-key (getf match :firing)))))
      (ecase event
        (:added (let ((cells (nth-value 1 (graph-db.rules:match-row match))))
                  (setf (gethash key *rows*) cells)
                  (push (list query :added cells) *row-deliveries*)))
        (:lost (push (list query :lost (gethash key *rows* :unknown))
                     *row-deliveries*))))))

(defun delivered-rows (query event)
  (loop for (q e cells) in *row-deliveries*
        when (and (string= q query) (eq e event)) collect cells))

(defun rows-equal-p (a b)
  (and (= (length a) (length (remove-duplicates a :test #'equal)))
       (null (set-exclusive-or a b :test #'equal))))

(defparameter *row-queries*
  '(("join" . "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a)
                (claim-current ?p)
                (claim ?u rt-claim \"host\" ?h \"up\" nil nil)
                (claim-current ?u)")
    ("not" . "(claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)
               (not (claim ?d rt-claim \"host\" ?h \"down\" nil nil))")
    ("cmp" . "(claim ?u rt-claim \"host\" ?h \"up\" nil nil) (claim-current ?u)
               (> ?h \"k10\")")
    ("standing" . "(claim ?u rt-claim \"host\" ?h \"up\" nil nil)
                    (claim-current ?u) (claim-standing ?u \"observed\")")
    ("wide" . "(claim ?p rt-claim \"host\" ?k ?r ?ons ?ok) (claim-current ?p)"))
  "Design §8's equivalence test, over rows (#470): a join, a NOT, a
comparison, a standing filter and a family-wide read.")

(defun row-query-step (g i)
  "Commit I of the script: hosts k00, k08, ... live a full life -- up,
runs web, a second up by another producer, down, the first up
retracted, the second up's standing changed, the second up deleted --
and every eighth commit is noise under host."
  (flet ((host (n) (format nil "k~2,'0d" (max 0 n))))
    (ecase (mod i 8)
      (0 (commit-host-claim g (host i) "up" "scan-t"))
      (1 (commit-host-claim g (host (- i 1)) "runs" "scan-t" "web"))
      (2 (with-transaction ((graph-db::transaction-manager g))
           (make-rt-claim-unary :graph g :subject-namespace :host
                                :subject-key (host (- i 2)) :relation "up"
                                :producer "other" :standing :inferred)))
      (3 (commit-host-claim g (host (- i 3)) "down" "scan-t"))
      (4 (retract-claim (up-claim-by g (host (- i 4)) "scan-t")))
      (5 (with-transaction ((graph-db::transaction-manager g))
           (let ((copy (graph-db::copy (up-claim-by g (host (- i 5))
                                                    "other"))))
             (setf (claim-standing copy) :observed)
             (graph-db::save copy))))
      (6 (with-transaction ((graph-db::transaction-manager g))
           (mark-deleted (up-claim-by g (host (- i 6)) "other"))))
      (7 (commit-host-claim g (host i) "noise" "scan-t")))))

(test standing-queries-deliver-exactly-the-row-differences-of-full-reevaluation
  "Design §8 over rows, not keys (#470): after each commit the rows
delivered as :ADDED and :LOST equal the set differences of the query's
rows before and after, cell for cell, with no duplicate; retractions,
deletions and a slot update included."
  (with-rules-graph (g)
    (let ((*rows* (make-hash-table :test 'equal))
          (*row-deliveries* '())
          (mismatches '())
          (total 0))
      (unwind-protect
           (progn
             (loop for (name . body) in *row-queries*
                   do (graph-db.rules:def-production name :version "1"
                        :family rt-claim :body body
                        :action (deliver-row name)))
             (graph-db.rules:enable-forward-rules g)
             (let ((prev (loop for (nil . body) in *row-queries*
                               collect (guarded-rows body g))))
               (dotimes (i 40)
                 (setf *row-deliveries* '())
                 (row-query-step g i)
                 (incf total (length *row-deliveries*))
                 (let ((now (loop for (nil . body) in *row-queries*
                                  collect (guarded-rows body g))))
                   (loop for (name . nil) in *row-queries*
                         for before in prev
                         for after in now
                         do (let ((added (set-difference after before
                                                         :test #'equal))
                                  (removed (set-difference before after
                                                           :test #'equal)))
                              (unless (and (rows-equal-p
                                            (delivered-rows name :added)
                                            added)
                                           (rows-equal-p
                                            (delivered-rows name :lost)
                                            removed))
                                (push (list i name :added added
                                            (delivered-rows name :added)
                                            :removed removed
                                            (delivered-rows name :lost))
                                      mismatches))))
                   (setf prev now)))))
        (loop for (name . nil) in *row-queries*
              do (graph-db.rules:undef-production name)))
      (is (null mismatches) "mismatches: ~S" mismatches)
      (is (< 40 total))
      (is-true (run-rules-is-quiet g)))))

(defun registered-functors ()
  "Every registered Prolog functor, as the GUI inventory spells them:
lowercase name/arity, interned keys only."
  (let ((names '()))
    (flet ((collect (table)
             (maphash (lambda (key value)
                        (declare (ignore value))
                        (when (and (symbolp key) (symbol-package key))
                          (pushnew (string-downcase (symbol-name key))
                                   names :test #'string=)))
                      table)))
      (collect graph-db::*prolog-global-functors*)
      (collect graph-db::*user-functors*))
    (sort names #'string<)))

(test every-registered-functor-has-a-standing-query-class
  "#470: the classification lives with the functor inventory, so a new
functor chooses whether a standing query may say it.  This suite's own
test functors are the only exceptions."
  (let* ((classified
           (mapcar #'car graph-db.rules:*standing-query-functor-classes*))
         (unclassified
           (set-difference
            (set-difference (registered-functors) classified
                            :test #'string=)
            '("slow-true/1" "refuse-loudly/1")
            :test #'string=)))
    (is (null unclassified)
        "functors with no entry in *STANDING-QUERY-FUNCTOR-CLASSES*: ~S"
        unclassified)))

;;; The rounds guard counts the step's own drains only (#472)

(defun commit-up-paced (g prefix n pause)
  "N \"up\" claims keyed PREFIX0.., each in its own transaction, PAUSE
seconds apart, on the calling thread.  Returns nothing."
  (let ((graph-db:*graph* g))
    (dotimes (i n)
      (with-transaction ((graph-db::transaction-manager g))
        (make-rt-claim-unary :graph g :subject-namespace :host
                             :subject-key (format nil "~A~D" prefix i)
                             :relation "up" :producer "scan-t"
                             :standing :observed))
      (sleep pause))))

(test foreign-commits-never-trip-the-rounds-guard
  "#472: two threads commit faster than a 30 ms action serves them, so
the running step's queue is never empty between drains.  Those drains
are foreign commits, not an action feeding its own read: nothing is
dropped at any bound."
  (with-rules-graph (g)
    (let ((calls 0)
          (lock (bt:make-lock "paced"))
          (rounds graph-db.rules:*rules-max-rounds*))
      ;; Set, not bound: the steps run on the writer threads.  Three
      ;; drains of a growing queue take about a dozen deltas; the
      ;; writers supply eighty, so the stream outlasts the bound.
      (setf graph-db.rules:*rules-max-rounds* 3)
      (unwind-protect
           (with-production ("paced" :version "1" :family rt-claim
                                     :body *q3-body*
                                     :action (lambda (event match)
                                               (declare (ignore match))
                                               (when (eq event :added)
                                                 (bt:with-lock-held (lock)
                                                   (incf calls)))
                                               (sleep 0.03)))
             (graph-db.rules:enable-forward-rules g)
             (let ((a (bt:make-thread
                       (lambda () (commit-up-paced g "a" 40 0.01))
                       :name "paced a"))
                   (b (bt:make-thread
                       (lambda () (commit-up-paced g "b" 40 0.01))
                       :name "paced b")))
               (bt:join-thread a)
               (bt:join-thread b))
             (let ((status (graph-db.rules:forward-rules-status g)))
               (is (eql 0 (getf status :queue-depth)))
               (is (null (find :rounds (getf status :failures) :key #'car)))
               (is (= 80 calls))
               (is (= 80 (length (firings g "paced"))))))
        (setf graph-db.rules:*rules-max-rounds* rounds)))))

(defparameter *pong-body*
  "(claim ?p rt-claim \"host\" ?h \"pong\" nil nil) (claim-current ?p)")

(defun commit-pong-elsewhere (g)
  "One \"pong\" claim committed on a helper thread and joined: a foreign
delta queued on whatever step is running.  Returns nothing."
  (bt:join-thread
   (bt:make-thread
    (lambda ()
      (let ((graph-db:*graph* g))
        (with-transaction ((graph-db::transaction-manager g))
          (make-rt-claim-unary :graph g :subject-namespace :host
                               :subject-key "p1" :relation "pong"
                               :producer "scan-t" :standing :observed))))
    :name "pong committer")))

(test the-rounds-guard-drops-only-the-deltas-the-action-fed
  "#472: when the echo production is stopped at the bound, a foreign
commit queued in the same drain still gets its step, and its production
fires."
  (with-rules-graph (g)
    (let ((*fired* '())
          (graph-db.rules:*rules-max-rounds* 5))
      ;; Bound, not set: this step runs on the test thread.
      (with-production ("pong" :version "1" :family rt-claim
                               :body *pong-body*
                               :action (note-firing "pong"))
        (with-production
            ("echo" :version "1" :family rt-claim :body *echo-body*
                    :writes '()
                    :action (lambda (event match)
                              (when (eq event :added)
                                (push (list "echo" event (getf match :key))
                                      *fired*)
                                (when (= 5 (count "echo" *fired*
                                                  :key #'first
                                                  :test #'string=))
                                  (commit-pong-elsewhere g))
                                (with-transaction
                                    ((graph-db::transaction-manager g))
                                  (make-rt-claim-unary
                                   :graph g :subject-namespace :host
                                   :subject-key (format nil "h~D"
                                                        (length *fired*))
                                   :relation "ping" :producer "echo"
                                   :standing :inferred)))))
          (graph-db.rules:enable-forward-rules g)
          (finishes
            (with-transaction ((graph-db::transaction-manager g))
              (make-rt-claim-unary :graph g :subject-namespace :host
                                   :subject-key "h0" :relation "ping"
                                   :producer "scan-t" :standing :observed)))
          (let ((status (graph-db.rules:forward-rules-status g)))
            (is (find :rounds (getf status :failures) :key #'car))
            (is (<= (count "echo" *fired* :key #'first :test #'string=) 7))
            (is (equal '(("pong" :added))
                       (loop for (name event) in (fired)
                             when (string= name "pong")
                               collect (list name event))))
            (is (eql 0 (getf status :queue-depth)))
            (is-false (getf status :running-p))))))))

;;; The per-identity resolver (#457, spec
;;; docs/superpowers/specs/2026-11-05-forward-step-reads-design.md §2)

(defun flagged-rule-and-compiled (g n)
  "The \"flagged\" rule written, N \"up\" premises and the strict mode
committed, the rule run once.  => (VALUES RECORD COMPILED)."
  (let ((r (write-rule g :name "flagged" :version "1" :family "rt-claim"
                       :head *flagged-head* :body *flagged-body*)))
    (seed-keys g n :relation "up")
    (with-transaction ((graph-db::transaction-manager g))
      (make-rt-claim-binary :graph g :subject-namespace :sys
                            :subject-key "s" :relation "mode"
                            :object-namespace :val :object-key "strict"
                            :producer "ops" :standing :asserted))
    (graph-db.rules:run-rule g r)
    (values r (graph-db.rules:compile-rule g r))))

(test claim-by-dedupe-key-resolves-a-derived-claim-retracted-or-not
  "Spec §2: a dedupe key resolves the producer's claim under it through
the subject-relation index, retracted as well as current, and NIL for
an identity nothing has."
  (with-rules-graph (g)
    (multiple-value-bind (r compiled) (flagged-rule-and-compiled g 3)
      (declare (ignore r))
      (let* ((family (graph-db.rules::compiled-rule-family compiled))
             (c (find "k00" (derived g 'rt-claim "flagged")
                      :key #'claim-subject-key :test #'string=))
             (key (graph-db.rules::%existing-key c family)))
        (is (equal '(:host "k00" "flagged" nil nil nil) key))
        (is (equalp (graph-db:id c)
                    (graph-db:id (graph-db.rules::%claim-by-dedupe-key
                                  g compiled key))))
        (retract-claim c)
        (let ((again (graph-db.rules::%claim-by-dedupe-key g compiled key)))
          (is (equalp (graph-db:id c) (graph-db:id again)))
          (is-false (claim-current-p again)))
        (is (null (graph-db.rules::%claim-by-dedupe-key
                   g compiled '(:host "nobody" "flagged" nil nil nil))))
        ;; Another producer's claim under the same endpoints is not it.
        (with-transaction ((graph-db::transaction-manager g))
          (make-rt-claim-unary :graph g :subject-namespace :host
                               :subject-key "k01" :relation "flagged"
                               :producer "hand" :standing :asserted))
        (let ((k01 (graph-db.rules::%claim-by-dedupe-key
                    g compiled '(:host "k01" "flagged" nil nil nil))))
          (is (string= "rule/flagged" (claim-producer k01))))))))

(test claim-by-dedupe-key-matches-a-temporal-claim-by-its-extent-start
  "Review Focus 1: a temporal family's dedupe key carries the extent
start, so two derived claims of one endpoint pair resolve apart."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let* ((r (write-host-version g))
           (compiled (progn (graph-db.rules:run-rule g r)
                            (graph-db.rules:compile-rule g r)))
           (family (graph-db.rules::compiled-rule-family compiled))
           (h1-v2 (remove-if-not
                   (lambda (c) (and (string= "h1" (claim-subject-key c))
                                    (string= "2" (claim-object-key c))))
                   (derived g 'rtt-claim "host-version"))))
      (is (= 2 (length h1-v2)))
      (dolist (c h1-v2)
        (let ((found (graph-db.rules::%claim-by-dedupe-key
                      g compiled (graph-db.rules::%existing-key c family))))
          (is (equalp (graph-db:id c) (graph-db:id found))))))))

(test a-lazy-existing-resolves-on-demand-and-caches-the-miss
  "Spec §2: the forward path's resolver starts empty, resolves a key
when asked and remembers the answer, a miss included; the full path's
is seeded and never resolves."
  (with-rules-graph (g)
    (multiple-value-bind (r compiled) (flagged-rule-and-compiled g 2)
      (declare (ignore r))
      (let ((lazy (graph-db.rules::%lazy-existing g compiled))
            (seeded (graph-db.rules::%seeded-existing g compiled))
            (hit '(:host "k00" "flagged" nil nil nil))
            (miss '(:host "nobody" "flagged" nil nil nil)))
        (is-true (graph-db.rules::%existing-lazy-p lazy))
        (is (zerop (hash-table-count (graph-db.rules::%existing-table lazy))))
        (is (string= "k00" (claim-subject-key
                            (graph-db.rules::%existing-claim lazy hit))))
        (is (null (graph-db.rules::%existing-claim lazy miss)))
        (is (= 2 (hash-table-count (graph-db.rules::%existing-table lazy))))
        (is-true (nth-value 1 (gethash miss
                                      (graph-db.rules::%existing-table
                                       lazy))))
        (is-false (graph-db.rules::%existing-lazy-p seeded))
        (is (= 2 (hash-table-count
                  (graph-db.rules::%existing-table seeded))))
        (is (null (graph-db.rules::%existing-claim seeded miss)))
        (is (= 2 (hash-table-count
                  (graph-db.rules::%existing-table seeded))))))))

;;; The reconcile walks the step's keys (#457 spec §3)

(defmacro with-counted-producer-reads ((counts) &body body)
  "BODY with CLAIMS-BY-PRODUCER counted into COUNTS, an EQUAL hash of
claim-class symbol -> calls; the original restored on exit."
  (let ((orig (gensym "ORIG")))
    `(let ((,counts (make-hash-table :test 'equal))
           (,orig (fdefinition 'graph-db.spacetime:claims-by-producer)))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db.spacetime:claims-by-producer)
                    (lambda (graph claim-class producer &rest keys)
                      (incf (gethash claim-class ,counts 0))
                      (apply ,orig graph claim-class producer keys)))
              ,@body)
         (setf (fdefinition 'graph-db.spacetime:claims-by-producer)
               ,orig)))))

(defun commit-strict-mode (g)
  "The strict mode claim, in its own transaction.  Returns it."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rt-claim-binary :graph g :subject-namespace :sys
                          :subject-key "s" :relation "mode"
                          :object-namespace :val :object-key "strict"
                          :producer "ops" :standing :asserted)))

(test a-forward-step-never-enumerates-the-producers-claims
  "Spec §3, the mutation gate for the claim reads: a one-claim commit's
forward step and a one-claim retraction's targeted step read the
touched rule's producer through CLAIMS-BY-PRODUCER zero times, and
derive and sweep exactly what they did before."
  (with-rules-graph (g)
    (write-rule g :name "flagged" :version "1" :family "rt-claim"
                  :head *flagged-head* :body *flagged-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed-keys g 50 :relation "up")
    ;; DERIVED itself reads the producer, so it stays outside the
    ;; counted windows.
    (let ((mode nil))
      (with-counted-producer-reads (counts)
        (setf mode (commit-strict-mode g))
        (is (eql 0 (gethash 'rt-claim counts 0))))
      (is (= 50 (length (derived g 'rt-claim "flagged"))))
      (with-counted-producer-reads (counts)
        (retract-claim mode)
        (is (eql 0 (gethash 'rt-claim counts 0))))
      (is (null (derived g 'rt-claim "flagged")))
      (is (= 50 (length (derived g 'rt-claim "flagged" :current nil)))))
    (is-true (run-rules-is-quiet g))))

(test the-per-key-reconcile-constructs-a-desired-key-that-stands-nowhere
  "Spec §3 item 1: the targeted path has no round pass, so the per-key
reconcile constructs a desired identity no claim has, counts it
DERIVED and notes it in the resolver."
  (with-rules-graph (g)
    (multiple-value-bind (r compiled) (flagged-rule-and-compiled g 1)
      (declare (ignore r))
      (let ((desired (make-hash-table :test 'equal))
            (key '(:host "zz" "flagged" nil nil nil))
            (report (graph-db.rules::%make-rule-report
                     :rule-name "flagged" :version "1"))
            (existing (graph-db.rules::%lazy-existing g compiled)))
        (setf (gethash key desired)
              (cons (list :subject-namespace :host :subject-key "zz"
                          :extent nil)
                    '()))
        (with-transaction ((graph-db::transaction-manager g))
          (multiple-value-bind (standing swept)
              (graph-db.rules::%reconcile-claims
               compiled g report desired (list key)
               :sweep-keys (make-hash-table :test 'equal)
               :existing existing)
            (is (= 1 (length standing)))
            (is (null swept))
            (is (string= "zz" (claim-subject-key (cdr (first standing)))))
            (is (eq (cdr (first standing))
                    (graph-db.rules::%existing-claim existing key)))))
        (is (= 1 (graph-db.rules:rule-report-derived report)))
        (is (= 0 (graph-db.rules:rule-report-kept report)))
        (is (= 2 (length (derived g 'rt-claim "flagged"))))))))

;;; Provenance per touched claim (#457 spec §3)

(test a-forward-step-never-enumerates-the-producers-derivation-records
  "Spec §3, the mutation gate for the provenance reads: neither the
forward step nor the targeted step reads the producer's DERIVATION
records through CLAIMS-BY-PRODUCER, and provenance is whole."
  (with-rules-graph (g)
    (write-rule g :name "flagged" :version "1" :family "rt-claim"
                  :head *flagged-head* :body *flagged-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed-keys g 5 :relation "up")
    (let ((mode nil))
      (with-counted-producer-reads (counts)
        (setf mode (commit-strict-mode g))
        (is (eql 0 (gethash 'graph-db.rules:derivation counts 0))))
      (let ((c (first (derived g 'rt-claim "flagged"))))
        (is (= 2 (length (graph-db.rules:premises-of g c))))
        (is (find "mode" (graph-db.rules:premises-of g c)
                  :key #'claim-relation :test #'string=)))
      (with-counted-producer-reads (counts)
        (retract-claim mode)
        (is (eql 0 (gethash 'graph-db.rules:derivation counts 0))))
      (is (null (derived g 'rt-claim "flagged")))
      (let ((c (first (derived g 'rt-claim "flagged" :current nil))))
        ;; D4: the records are retracted with the claim, not deleted.
        (is (= 2 (length (graph-db.rules:premises-of g c))))
        (is (every (lambda (r) (not (claim-current-p r)))
                   (claims-touching g 'graph-db.rules:derivation :claim
                                    (claim-identity-key c)
                                    :role :subject)))))
    (is-true (run-rules-is-quiet g))))

(test a-swept-claims-foreign-record-under-the-producer-is-retracted
  "Review Focus 2: a DERIVATION record under the rule's producer that is
not DERIVED-FROM, on a derived claim the targeted step sweeps, is
retracted with it, as the enumerating pass retracted it."
  (with-rules-graph (g)
    (write-rule g :name "flagged" :version "1" :family "rt-claim"
                  :head *flagged-head* :body *flagged-body*)
    (graph-db.rules:enable-forward-rules g)
    (seed-keys g 2 :relation "up")
    (let* ((mode (commit-strict-mode g))
           (c (first (derived g 'rt-claim "flagged")))
           (key (claim-identity-key c)))
      (with-transaction ((graph-db::transaction-manager g))
        (graph-db.rules::make-derivation-binary
         :graph g :subject-namespace :claim :subject-key key
         :relation "annotates" :object-namespace :claim :object-key "x"
         :producer "rule/flagged" :rule-version "1" :standing :inferred))
      (flet ((foreign ()
               (claims-touching g 'graph-db.rules:derivation :claim key
                                :role :subject :relation "annotates")))
        (is (= 1 (length (foreign))))
        (is-true (claim-current-p (first (foreign))))
        (retract-claim mode)
        (is (null (derived g 'rt-claim "flagged")))
        (is (= 1 (length (foreign))))
        (is-false (claim-current-p (first (foreign))))))))

;;; The step's cost on the status (#457 spec §4)

(test the-status-reports-the-last-steps-elapsed-time
  "Spec §4: :LAST-STEP-ELAPSED is NIL until a step has run, then the
wall seconds of the last drain."
  (with-rules-graph (g)
    (write-rule g :name "flagged" :version "1" :family "rt-claim"
                  :head *flagged-head* :body *flagged-body*)
    (graph-db.rules:enable-forward-rules g)
    (is (null (getf (graph-db.rules:forward-rules-status g)
                    :last-step-elapsed)))
    (seed-keys g 2 :relation "up")
    (let ((elapsed (getf (graph-db.rules:forward-rules-status g)
                         :last-step-elapsed)))
      (is (realp elapsed))
      (is (and (realp elapsed) (>= elapsed 0))))))

(test a-round-never-writes-the-seeded-resolver
  "Review finding 1: %ROUND-DELTA notes what it kept or constructed in a
lazy resolver only.  A seeded one is built once outside a cross-store
round's retried transaction, so a note there would hand the retry the
unwound attempt's unsaved claim as standing."
  (with-rules-graph (g)
    (multiple-value-bind (r compiled) (flagged-rule-and-compiled g 1)
      (declare (ignore r))
      (let ((desired (make-hash-table :test 'equal))
            (key '(:host "zz" "flagged" nil nil nil))
            (seeded (graph-db.rules::%seeded-existing g compiled))
            (lazy (graph-db.rules::%lazy-existing g compiled)))
        (setf (gethash key desired)
              (cons (list :subject-namespace :host :subject-key "zz"
                          :extent nil)
                    '()))
        (with-transaction ((graph-db::transaction-manager g))
          (multiple-value-bind (new count)
              (graph-db.rules::%round-delta compiled g desired seeded
                                            (list key))
            (is (= 1 count))
            (is (= 1 (length new)))
            (is-false (nth-value 1 (gethash key (graph-db.rules::%existing-table
                                                 seeded)))))
          (multiple-value-bind (new count)
              (graph-db.rules::%round-delta compiled g desired lazy
                                            (list key))
            (declare (ignore new))
            ;; The lazy one resolves the claim the first call wrote
            ;; (the overlay) and notes it; nothing is constructed twice.
            (is (= 0 count))
            (is-true (nth-value 1 (gethash key (graph-db.rules::%existing-table
                                                lazy))))))))))

(test a-commit-during-the-opt-in-queues-and-is-drained-after-it
  (with-rules-graph (g)
    (write-rule g :name "web-hosts" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body *slow-web-hosts-body*)
    (commit-runs-web g "h1")
    (setf *slow-seconds* 0.6)
    (unwind-protect
         (let ((thread (bt:make-thread
                        (lambda ()
                          (let ((graph-db:*graph* g))
                            (graph-db.rules:enable-forward-rules g))))))
           (is-true (wait-until
                     (lambda ()
                       (getf (graph-db.rules:forward-rules-status g)
                             :running-p))
                     3))
           (commit-runs-web g "h2")
           (bt:join-thread thread)
           (is (equal '("h1" "h2")
                      (sort (mapcar #'claim-object-key
                                    (derived g 'rt-claim "web-hosts"))
                            #'string<)))
           (is (zerop (getf (graph-db.rules:forward-rules-status g)
                            :queue-depth))))
      (setf *slow-seconds* 0))))

(defvar *late-events-464* nil)

(defun late-action-464 (event match)
  (declare (ignore event match))
  (push :first *late-events-464*))

(test a-rule-that-does-not-compile-is-noted-once-per-refresh-under-the-cap
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (unwind-protect
         (progn
           (graph-db.rules:def-rule "bad-464" :version "1" :family rt-claim
             :head *web-hosts-head*
             :body (concatenate 'string *web-hosts-body*
                                " (no-such-functor ?p)"))
           (commit-runs-web g "h1")
           (is (find "bad-464"
                     (getf (graph-db.rules:forward-rules-status g) :failures)
                     :key #'car :test #'equal))
           (let ((graph-db.rules::*forward-failures-kept* 2))
             (loop for host in '("h2" "h3")
                   do (setf (graph-db.rules::forward-state-cache-stamp
                             (graph-db.rules::%forward-state g)) nil)
                      (commit-runs-web g host))
             (is (<= (length (getf (graph-db.rules:forward-rules-status g)
                                   :failures))
                     2))))
      (graph-db.rules:undef-rule "bad-464"))))

(test a-production-action-given-as-a-symbol-is-looked-up-at-fire-time
  (with-rules-graph (g)
    (let ((*late-events-464* nil)
          (original (fdefinition 'late-action-464)))
      (unwind-protect
           (with-production ("late-464" :version "1" :family rt-claim
                                         :body *web-watch-body*
                                         :action 'late-action-464)
             (graph-db.rules:enable-forward-rules g)
             (commit-runs-web g "h1")
             (setf (fdefinition 'late-action-464)
                   (lambda (event match)
                     (declare (ignore event match))
                     (push :second *late-events-464*)))
             (commit-runs-web g "h2")
             (is (equal '(:first :second) (reverse *late-events-464*))))
        (setf (fdefinition 'late-action-464) original)))))

(test a-solutionless-run-reports-no-inherited-inferences
  (with-rules-graph (g)
    (let* ((rule (write-web-hosts g))
           (compiled (graph-db.rules:compile-rule g rule))
           (report (graph-db.rules::%make-rule-report :inferences 999)))
      (is (null (graph-db.rules::%solutions compiled g report)))
      (is (= 0 (graph-db.rules:rule-report-inferences report))))))

(test the-full-path-does-not-recompile-a-non-recursive-rule
  (with-rules-graph (g)
    (write-rule g :name "unreached" :version "1" :family "rt-claim"
                  :head *unreached-head* :body *unreached-body*)
    (graph-db.rules:enable-forward-rules g)
    (commit-runs-web g "h1")
    (commit-runs-web g "h2")
    (let ((original (fdefinition 'graph-db.rules:compile-rule))
          (calls 0))
      (unwind-protect
           (progn
             (setf (fdefinition 'graph-db.rules:compile-rule)
                   (lambda (&rest args)
                     (incf calls)
                     (apply original args)))
             (seed-one g)
             (is (= 0 calls))
             (is (= 2 (length (unreached-hosts g)))))
        (setf (fdefinition 'graph-db.rules:compile-rule) original)))))

(test a-match-error-lands-on-failures
  (with-rules-graph (g)
    (with-production ("match-464" :version "1" :family rt-claim
                                  :body *web-watch-body*
                                  :action (note-firing "match-464"))
      (graph-db.rules:enable-forward-rules g)
      (commit-runs-web g "h1")
      (let ((original (fdefinition 'graph-db.rules::%solutions)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db.rules::%solutions)
                     (lambda (&rest args)
                       (if (and (> (length args) 3)
                                (eq '= (caar (fourth args))))
                           (error "boom")
                           (apply original args))))
               (commit-runs-web g "h2")
               (is (find-if (lambda (failure)
                              (and (equal "match-464" (car failure))
                                   (search "boom" (cdr failure))))
                            (getf (graph-db.rules:forward-rules-status g)
                                  :failures))))
          (setf (fdefinition 'graph-db.rules::%solutions) original))))))
