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
