(in-package :graph-db)

;;;; Custom read-write lock for SBCL / LispWorks / ECL.
;;;; (CCL is excluded in graph-db.asd and uses ccl:make-read-write-lock via the
;;;; shims in utilities.lisp.)
;;;;
;;;; Scaling design — targeted wakeup (fix #3):
;;;;   The lock keeps a FIFO writer-queue plus a count of active readers.  To
;;;;   avoid the O(N^2) thundering herd of the old "broadcast everyone on every
;;;;   write release" scheme, each queued writer carries its OWN private wake
;;;;   semaphore, and readers share a single condition variable:
;;;;     * On write release, if a writer is queued we signal EXACTLY the front
;;;;       writer's semaphore (one wakeup, FIFO).  A semaphore is level-triggered,
;;;;       so a signal that lands before the writer parks is not lost.
;;;;     * If no writer is queued, we broadcast the reader cv once, letting all
;;;;       waiting readers proceed together (O(R) once, never O(N^2)).
;;;;   The per-lock `semaphore' slot is unchanged: it is the "readers have
;;;;   drained" signal that the single ACTIVE writer waits on while readers exit.
;;;;
;;;; Reader eligibility is unchanged from the original: a reader proceeds iff
;;;; there is no active writer ((null lock-writer)).  This change alters only HOW
;;;; waiters are woken, never WHO may proceed.
;;;;
;;;; Old ECL (no :graph-db-ecl-modern-mp) keeps the (sleep 0.001) poll fallback
;;;; byte-for-byte: no waker is created, the reader cv is never touched, writers
;;;; and readers poll lock-writer / lock-readers exactly as before (works around
;;;; ECL 21.2.1 mp:wait-on-semaphore / condition-variable bugs).

(defun print-rw-lock (lock stream depth)
  (declare (ignore depth))
  (print-unreadable-object (lock stream :type t :identity t)
    (format stream "W: ~A, R: ~A"
            (lock-writer lock) (lock-readers lock))))

;;; ---------------------------------------------------------------------------
;;; Per-writer wake token.  One is created per acquire-write-lock call (never
;;; reused), so its waker is signalled at most once and waited on at most once.
;;; On old ECL the waker is NIL (poll path; ww-signal / ww-wait are never called).
;;; ---------------------------------------------------------------------------

(defstruct (writer-wait (:conc-name ww-))
  (thread nil)
  (waker #+sbcl      (sb-thread:make-semaphore)
         #+lispworks (mp:make-semaphore)
         #+(and ecl graph-db-ecl-modern-mp) (mp:make-semaphore)
         #+(and ecl (not graph-db-ecl-modern-mp)) nil))

(declaim (inline ww-signal ww-wait))

(defun ww-signal (ww)
  "Wake the single writer waiting on WW (modern impls only)."
  (declare (ignorable ww))
  #+sbcl      (sb-thread:signal-semaphore (ww-waker ww))
  #+lispworks (mp:semaphore-release (ww-waker ww))
  #+(and ecl graph-db-ecl-modern-mp) (mp:signal-semaphore (ww-waker ww))
  #+(and ecl (not graph-db-ecl-modern-mp)) nil)

(defun ww-wait (ww)
  "Block until WW's waker is signalled (modern impls only)."
  (declare (ignorable ww))
  #+sbcl      (sb-thread:wait-on-semaphore (ww-waker ww))
  #+lispworks (mp:semaphore-acquire (ww-waker ww))
  #+(and ecl graph-db-ecl-modern-mp) (mp:wait-on-semaphore (ww-waker ww))
  #+(and ecl (not graph-db-ecl-modern-mp)) nil)

;;; ---------------------------------------------------------------------------
;;; The lock.
;;; ---------------------------------------------------------------------------

(defstruct (rw-lock
             (:conc-name lock-)
             (:print-function print-rw-lock)
             (:predicate rw-lock-p))
  #+sbcl(lock (sb-thread:make-mutex) :type sb-thread:mutex)
  #+lispworks(lock (mp:make-lock) :type mp:lock)
  #+ecl(lock (mp:make-lock))
  (readers 0 :type integer)
  ;; "Readers have drained" signal that the ACTIVE writer waits on.  Unchanged.
  #+sbcl (semaphore (sb-thread:make-semaphore) :type sb-thread:semaphore)
  #+lispworks (semaphore (mp:make-semaphore) :type mp:semaphore)
  #+ecl (semaphore (mp:make-semaphore))
  ;; FIFO of WRITER-WAIT entries (thread + private waker).  enqueue / dequeue /
  ;; queue-front / empty-queue-p semantics unchanged; entries carry a waker now.
  (writer-queue (make-empty-queue) :type queue)
  (writer nil)
  ;; Reader condition variable: broadcast ONCE on the writer->free transition
  ;; when no writer is queued.  Modern impls only; old ECL polls and never uses it.
  #+lispworks (reader-cv (mp:make-condition-variable) :type mp:condition-variable)
  #+sbcl (reader-cv (sb-thread:make-waitqueue) :type sb-thread:waitqueue)
  #+(and ecl graph-db-ecl-modern-mp) (reader-cv (mp:make-condition-variable)))

(defun %next-in-queue-p (rw-lock thread)
  ;; Callers must already hold (lock-lock rw-lock).
  (and (not (empty-queue-p (lock-writer-queue rw-lock)))
       (eq thread (ww-thread (queue-front (lock-writer-queue rw-lock))))))

(defun %lock-unused-p (rw-lock)
  ;; Callers must already hold (lock-lock rw-lock).
  (and (= 0 (lock-readers rw-lock))
       (= #+sbcl 0 #+lispworks 1 #+ecl 0
          (#+sbcl sb-thread:semaphore-count
           #+lispworks mp:semaphore-count
           #+ecl mp:semaphore-count
           (lock-semaphore rw-lock)))
       (null (lock-writer rw-lock))
       (empty-queue-p (lock-writer-queue rw-lock))))

(defun next-in-queue-p (rw-lock thread)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (%next-in-queue-p rw-lock thread)))

(defun lock-unused-p (rw-lock)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (%lock-unused-p rw-lock)))

;;; ---------------------------------------------------------------------------
;;; Per-impl wakeup / wait primitives (caller holds lock-lock unless noted).
;;; ---------------------------------------------------------------------------

(defun %reader-wait (rw-lock)
  "Block the current reader on the reader cv (caller holds lock-lock).
Modern impls only; old ECL never calls this (it spin-sleeps instead)."
  (declare (ignorable rw-lock))
  #+sbcl      (sb-thread:condition-wait (lock-reader-cv rw-lock) (lock-lock rw-lock))
  #+lispworks (mp:condition-variable-wait (lock-reader-cv rw-lock) (lock-lock rw-lock))
  #+(and ecl graph-db-ecl-modern-mp)
  (mp:condition-variable-wait (lock-reader-cv rw-lock) (lock-lock rw-lock))
  #+(and ecl (not graph-db-ecl-modern-mp)) nil)

(defun %wake-next-writer (rw-lock)
  "Signal EXACTLY the front queued writer (caller holds lock-lock; queue non-empty).
Old ECL: the front writer polls, so nothing to signal."
  (declare (ignorable rw-lock))
  #+(or sbcl lispworks graph-db-ecl-modern-mp)
  (ww-signal (queue-front (lock-writer-queue rw-lock)))
  #+(and ecl (not graph-db-ecl-modern-mp)) nil)

(defun %wake-all-readers (rw-lock)
  "Broadcast the reader cv once (caller holds lock-lock; no writer queued).
Old ECL: readers poll, so nothing to signal."
  (declare (ignorable rw-lock))
  #+sbcl      (sb-thread:condition-broadcast (lock-reader-cv rw-lock))
  #+lispworks (mp:condition-variable-broadcast (lock-reader-cv rw-lock))
  #+(and ecl graph-db-ecl-modern-mp)
  (mp:condition-variable-broadcast (lock-reader-cv rw-lock))
  #+(and ecl (not graph-db-ecl-modern-mp)) nil)

(defun %wait-for-readers-to-drain (rw-lock)
  "We are the active writer; block until the remaining readers exit."
  #+lispworks (mp:semaphore-acquire (lock-semaphore rw-lock))
  #+sbcl      (sb-thread:wait-on-semaphore (lock-semaphore rw-lock))
  #+(and ecl graph-db-ecl-modern-mp) (mp:wait-on-semaphore (lock-semaphore rw-lock))
  ;; Old ECL: mp:wait-on-semaphore blocked indefinitely on 21.2.1 under load,
  ;; so poll lock-readers instead.
  #+(and ecl (not graph-db-ecl-modern-mp))
  (loop
     (with-recursive-lock-held ((lock-lock rw-lock))
       (when (= 0 (lock-readers rw-lock))
         (return)))
     (sleep 0.001)))

;;; ---------------------------------------------------------------------------
;;; Read lock.
;;; ---------------------------------------------------------------------------

(defun release-read-lock (rw-lock)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (when (eql 0 (decf (lock-readers rw-lock)))
      (when (lock-writer rw-lock)
        ;; The active writer is parked waiting for readers to drain; wake it.
        ;; (This targets exactly one thread — not part of the herd.)  Old ECL
        ;; can't (mp:wait-on-semaphore blocked indefinitely on 21.2.1); there
        ;; the writer polls lock-readers instead.
        #+sbcl (sb-thread:signal-semaphore (lock-semaphore rw-lock))
        #+lispworks (mp:semaphore-release (lock-semaphore rw-lock))
        #+graph-db-ecl-modern-mp (mp:signal-semaphore (lock-semaphore rw-lock))))))

(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  (declare (ignore max-tries))
  ;; A reader proceeds iff there is no active writer.  Modern impls block on the
  ;; reader cv while a writer holds the lock; old ECL spin-sleeps outside it.
  (loop
     (with-recursive-lock-held ((lock-lock rw-lock))
       (when (null (lock-writer rw-lock))
         (incf (lock-readers rw-lock))
         (return-from acquire-read-lock rw-lock))
       ;; Writer active: modern impls park on the reader cv (re-acquires the
       ;; lock on wake, then we re-test); old ECL falls through to the sleep.
       #+(or sbcl lispworks graph-db-ecl-modern-mp) (%reader-wait rw-lock))
     #+(and ecl (not graph-db-ecl-modern-mp)) (sleep 0.001)))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
        (if (rw-lock-p (acquire-read-lock ,rw-lock))
            (progn ,@body)
            (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-read-lock ,rw-lock)))

;;; ---------------------------------------------------------------------------
;;; Write lock.
;;; ---------------------------------------------------------------------------

(defun release-write-lock (rw-lock &key reading-p)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (if (%next-in-queue-p rw-lock (current-thread))
        (dequeue (lock-writer-queue rw-lock))
        (error "Cannot release lock I don't own!"))
    (if (%next-in-queue-p rw-lock (current-thread))
        ;; Recursive ownership: another entry of ours is still at the front.
        nil
        (progn
          (setf (lock-writer rw-lock) nil)
          (when reading-p
            ;; Downgrade: we revert to holding a read lock.
            (incf (lock-readers rw-lock)))
          ;; Targeted wakeup: hand off to exactly the next writer, or release
          ;; all readers if none is queued.
          (if (empty-queue-p (lock-writer-queue rw-lock))
              (%wake-all-readers rw-lock)
              (%wake-next-writer rw-lock))))))

(defun acquire-write-lock (rw-lock &key (max-tries 1000) reading-p (wait-p t))
  (declare (ignore max-tries))
  (let ((self (current-thread))
        (my-entry nil))
    (declare (ignorable my-entry))
    (with-recursive-lock-held ((lock-lock rw-lock))
      (cond
        ;; Recursive re-acquire by the active writer: push a fresh entry on the
        ;; FRONT so queue-front identity stays = self, and return immediately.
        ((and (%next-in-queue-p rw-lock self)
              (eq (lock-writer rw-lock) self))
         (enqueue-front (lock-writer-queue rw-lock)
                        (make-writer-wait :thread self))
         (return-from acquire-write-lock rw-lock))
        ;; Blocking acquire: enqueue our entry and fall through to the wait loop.
        (wait-p
         (setf my-entry (make-writer-wait :thread self))
         (enqueue (lock-writer-queue rw-lock) my-entry))
        ;; Non-blocking try (:wait-p nil): take it iff the lock is idle.
        (t
         (if (%lock-unused-p rw-lock)
             (progn
               (enqueue (lock-writer-queue rw-lock)
                        (make-writer-wait :thread self))
               (setf (lock-writer rw-lock) self)
               (when reading-p
                 (decf (lock-readers rw-lock)))
               (return-from acquire-write-lock rw-lock))
             (return-from acquire-write-lock nil)))))
    ;; --- Wait loop (reached only on the wait-p path; my-entry is set). ---
    (loop
       (when (eq (lock-writer rw-lock) self)
         (return-from acquire-write-lock rw-lock))
       (let ((internal-wait-p nil)
             (park-on-waker nil))
         (declare (ignorable park-on-waker))
         (handler-case
             (with-recursive-lock-held ((lock-lock rw-lock))
               (if (and (null (lock-writer rw-lock))
                        (%next-in-queue-p rw-lock self))
                   ;; Our turn: become the active writer.
                   (progn
                     (setf (lock-writer rw-lock) self)
                     (when reading-p
                       (decf (lock-readers rw-lock)))
                     (unless (eql 0 (lock-readers rw-lock))
                       (setf internal-wait-p t)))
                   ;; Not our turn yet: park on OUR private waker (modern) or
                   ;; spin-sleep (old ECL).
                   (progn
                     #+(or sbcl lispworks graph-db-ecl-modern-mp)
                     (setf park-on-waker t))))
           (error (c)
             (log:error "Got error ~A while acquiring write lock ~A"
                        c rw-lock)))
         (cond
           (internal-wait-p
            ;; We are the writer; block until the remaining readers drain.
            (%wait-for-readers-to-drain rw-lock))
           #+(or sbcl lispworks graph-db-ecl-modern-mp)
           (park-on-waker
            ;; Block on our private semaphore until release signals exactly us.
            (ww-wait my-entry))
           ;; Old ECL: didn't become writer this round; sleep before retrying.
           #+(and ecl (not graph-db-ecl-modern-mp))
           (t (sleep 0.001)))))))

(defmacro with-write-lock ((rw-lock &key reading-p) &body body)
  `(unwind-protect
        (if (rw-lock-p (acquire-write-lock ,rw-lock :reading-p ,reading-p))
            (progn ,@body)
            (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-write-lock ,rw-lock :reading-p ,reading-p)))

#|
(defun test-rw-locks ()
  (let ((lock (make-rw-lock)))
    (make-thread
     #'(lambda () (with-write-lock (lock)
                    (format t "1 got write lock.  Sleeping.~%")
                    (sleep 5)
                    (with-write-lock (lock)
                      (format t "1 acquired recursive lock.~%")
                      (sleep 5)
                      (with-write-lock (lock)
                        (format t "1 acquired recursive lock.~%")
                        (sleep 5)
                        (format t "1 releasing recursive write lock.~%"))
                      (format t "1 releasing recursive write lock.~%"))
                    (format t "1 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "2 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "3 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-write-lock (lock)
                    (format t "4 got write lock.  Sleeping.~%")
                    (sleep 5)
                    (with-write-lock (lock)
                      (format t "4 acquired recursive lock.~%")
                      (sleep 5)
                      (with-write-lock (lock)
                        (format t "4 acquired recursive lock.~%")
                        (sleep 5)
                        (format t "4 releasing recursive write lock.~%"))
                      (format t "4 releasing recursive write lock.~%"))
                    (format t "4 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-write-lock (lock)
                    (format t "5 got write lock.  Sleeping.~%")
                    (sleep 5)
                    (format t "5 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "6 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "7 got read lock~%") (sleep 5))))))
|#
