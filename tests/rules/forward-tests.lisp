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
    (setf *slow-seconds* 0.6)
    (unwind-protect
         (let ((a (bt:make-thread
                   (lambda ()
                     (let ((graph-db:*graph* g))
                       (commit-runs-web g "h1")))
                   :name "committer a"))
               (depth nil)
               (elapsed nil))
           (sleep 0.15)                 ; A is inside its step by now
           (let ((start (get-internal-real-time)))
             (commit-runs-web g "h2")
             (setf elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second)))
           (setf depth (getf (graph-db.rules:forward-rules-status g)
                             :queue-depth))
           (bt:join-thread a)
           (is (< elapsed 0.4) "the second commit waited ~,2F s" elapsed)
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

(defun q2-keys (g)
  (declare (ignore g))
  (mapcar (lambda (row) (graph-db::%joined-identity-keys row))
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
