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

(defparameter *hosts-web-head*
  "(claim ?c rt-claim \"host\" ?h \"hosts-web\" nil nil)")
(defparameter *hosts-web-body*
  "(claim ?a rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")

(test a-forward-rule-derives-through-two-strata-in-one-step
  "HOSTS-WEB reads WEB-HOSTS' head; one commit of the base premises
derives both, the second rule written after the opt-in so the compiled
cache is refreshed by its commit."
  (with-rules-graph (g)
    (write-web-hosts g)
    (graph-db.rules:enable-forward-rules g)
    (write-rule g :name "hosts-web" :version "1" :family "rt-claim"
                  :head *hosts-web-head* :body *hosts-web-body*)
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
