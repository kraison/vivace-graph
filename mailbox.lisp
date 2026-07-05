(in-package :graph-db)

(defstruct (mailbox
             (:conc-name mb-)
             (:constructor %make-mailbox))
  (lock (make-lock))
  (queue (make-queue)))

(defun make-mailbox ()
  #+sbcl (sb-concurrency:make-mailbox)
  #+lispworks (mp:make-mailbox)
  ;; ECL shares CCL's lock+queue struct mailbox (no native mailbox, and
  ;; trivial-timeout is a CCL/LispWorks-only dep) -- the peer-replication device
  ;; single-writer funnel needs a working mailbox on ECL (the ship target).
  #+(or ccl ecl) (%make-mailbox))

(defun send-message (mailbox message)
  #+sbcl (sb-concurrency:send-message mailbox message)
  #+lispworks(mp:mailbox-send mailbox message)
  #+(or ccl ecl) (with-lock ((mb-lock mailbox))
                   (enqueue (mb-queue mailbox) message)))

(defun receive-message (mailbox &key (timeout 1))
  #+sbcl (sb-concurrency:receive-message mailbox :timeout timeout)
  #+lispworks (mp:mailbox-read mailbox nil timeout)
  #+ccl (with-lock ((mb-lock mailbox) :timeout timeout)
          (let ((result nil))
            (handler-case
                (trivial-timeout:with-timeout (timeout)
                  (loop until result do
                       (setq result (dequeue (mb-queue mailbox)))))
              (com.metabang.trivial-timeout:timeout-error (c)
                (declare (ignore c))))
            result))
  ;; ECL: poll the queue under the lock until a message arrives or TIMEOUT
  ;; elapses (no trivial-timeout on ECL).  Assumes messages are non-NIL (a NIL
  ;; dequeue means empty), which holds for every graph-db mailbox use.
  #+ecl (let ((deadline (+ (get-internal-real-time)
                           (max 1 (round (* timeout internal-time-units-per-second))))))
          (loop
            (let ((msg (with-lock ((mb-lock mailbox))
                         (dequeue (mb-queue mailbox)))))
              (when msg (return msg)))
            (when (>= (get-internal-real-time) deadline) (return nil))
            (sleep 0.005))))
