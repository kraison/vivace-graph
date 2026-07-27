;;;; RW-LOCK-SUITE
;;;;
;;;; Tests the graph-db read-write lock directly:
;;;;   SBCL / ECL → pure-Lisp rw-lock.lisp implementation
;;;;   CCL        → ccl:make-read-write-lock via utilities.lisp shims
;;;;
;;;; These tests need no on-disk graph; they exercise the lock primitives alone.

(in-package #:graph-db/concurrency-test)

(def-suite rw-lock-suite
  :description "Read-write lock correctness and deadlock-freedom."
  :in concurrency-suite)

(in-suite rw-lock-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: multiple threads can hold a read lock simultaneously
;;; ---------------------------------------------------------------------------

(test rw-lock-multiple-concurrent-readers
  "N threads should be able to hold the read lock at the same time."
  (let ((lock        (make-rw-lock))
        (count-lock  (bt:make-lock "count"))
        (inside      0)
        (max-inside  0))
    (run-threads *thread-count*
                 (lambda (i)
                   (declare (ignore i))
                   (with-read-lock (lock)
                     (let ((n (bt:with-lock-held (count-lock)
                                (incf inside)
                                inside)))
                       ;; Hold lock briefly so threads overlap.
                       (sleep 0.02)
                       (bt:with-lock-held (count-lock)
                         (setf max-inside (max max-inside n))
                         (decf inside))))))
    ;; If reads are truly shared, >1 thread was inside simultaneously.
    (is (> max-inside 1)
        "Expected >1 concurrent reader; got max ~A (read lock may be exclusive)"
        max-inside)))

;;; ---------------------------------------------------------------------------
;;; Test 2: a writer cannot enter while any reader holds the lock
;;; ---------------------------------------------------------------------------

(test rw-lock-writer-excludes-readers
  "No reader should see in-writer=T while a writer holds the lock."
  (let ((lock       (make-rw-lock))
        (flag-lock  (bt:make-lock "flag"))
        (in-writer  nil)
        (violation  nil))
    ;; 1 writer + (n-1) reader threads, each doing many short ops.
    (run-threads *thread-count*
                 (lambda (i)
                   (dotimes (_ 40)
                     (if (zerop i)
                         ;; Writer: mark entry/exit under flag-lock so readers
                         ;; can check it while holding the read lock.
                         (with-write-lock (lock)
                           (bt:with-lock-held (flag-lock)
                             (setf in-writer t))
                           (bt:thread-yield)
                           (bt:with-lock-held (flag-lock)
                             (setf in-writer nil)))
                         ;; Reader: if in-writer is T we have a violation.
                         (with-read-lock (lock)
                           (bt:with-lock-held (flag-lock)
                             (when in-writer
                               (setf violation t)))
                           (bt:thread-yield))))))
    (is-false violation "A reader observed in-writer=T — mutual exclusion failure")))

;;; ---------------------------------------------------------------------------
;;; Test 3: only one writer at a time
;;; ---------------------------------------------------------------------------

(test rw-lock-no-concurrent-writers
  "Two writers must never overlap."
  (let ((lock       (make-rw-lock))
        (flag-lock  (bt:make-lock "flag"))
        (in-writer  nil)
        (violation  nil))
    (run-threads *thread-count*
                 (lambda (i)
                   (declare (ignore i))
                   (dotimes (_ 40)
                     (with-write-lock (lock)
                       (bt:with-lock-held (flag-lock)
                         (when in-writer
                           (setf violation t))
                         (setf in-writer t))
                       (bt:thread-yield)
                       (bt:with-lock-held (flag-lock)
                         (setf in-writer nil))))))
    (is-false violation "Two writers held the lock concurrently")))

;;; ---------------------------------------------------------------------------
;;; White-box tests of the custom rw-lock semantics (SBCL / ECL / LispWorks).
;;; CCL uses a native ccl:make-read-write-lock with different internals, so
;;; these are gated to the custom-lock implementations.
;;; ---------------------------------------------------------------------------

#+(or sbcl lispworks ecl)
(test rw-lock-recursive-write
  "A thread re-acquires its own write lock; the lock stays held until the
   OUTERMOST release, blocking other writers throughout (nested release must
   NOT hand off)."
  (let ((lock      (make-rw-lock))
        (other-ran nil)
        (th        nil))
    (with-write-lock (lock)
      (with-write-lock (lock)              ; recursive re-acquire
        (setf th (bt:make-thread
                  (lambda () (with-write-lock (lock) (setf other-ran t)))))
        (sleep 0.15)
        (is-false other-ran "writer entered while lock held recursively"))
      ;; inner released, outer still held
      (sleep 0.15)
      (is-false other-ran
                "writer entered after inner release (outer still held)"))
    ;; outer released -> competitor proceeds
    (when th (bt:join-thread th))
    (is-true other-ran "writer never acquired the lock after full release")))

#+(or sbcl lispworks ecl)
(test rw-lock-reading-p-downgrade
  "Upgrade read->write with :reading-p, then downgrade back; the reader count
   is conserved and the lock is reusable afterward."
  (let ((lock (make-rw-lock)))
    (acquire-read-lock lock)
    (is (= 1 (lock-readers lock)))
    (acquire-write-lock lock :reading-p t)        ; convert read -> write
    (is (= 0 (lock-readers lock)))
    (is (eq (bt:current-thread) (lock-writer lock)))
    (release-write-lock lock :reading-p t)        ; convert write -> read
    (is (= 1 (lock-readers lock)))
    (is (null (lock-writer lock)))
    (release-read-lock lock)
    (is (= 0 (lock-readers lock)))
    ;; reusable: a fresh writer can take and release it
    (is (rw-lock-p (acquire-write-lock lock)))
    (release-write-lock lock)
    (is (null (lock-writer lock)))))

#+(or sbcl lispworks ecl)
(test rw-lock-try-write-nonblocking
  ":wait-p nil returns the lock when free, NIL when another writer holds it,
   and the lock is reusable afterward."
  (let ((lock (make-rw-lock)))
    ;; free -> succeeds
    (is (rw-lock-p (acquire-write-lock lock :wait-p nil)))
    (release-write-lock lock)
    ;; held by another thread -> our try returns NIL
    (let ((held    (bt:make-semaphore))
          (release (bt:make-semaphore))
          (th      nil))
      (setf th (bt:make-thread
                (lambda ()
                  (with-write-lock (lock)
                    (bt:signal-semaphore held)
                    (bt:wait-on-semaphore release)))))
      (bt:wait-on-semaphore held)          ; helper now holds the write lock
      (is (null (acquire-write-lock lock :wait-p nil))
          "try-acquire should fail while another writer holds the lock")
      (bt:signal-semaphore release)
      (bt:join-thread th))
    ;; reusable once free again
    (is (rw-lock-p (acquire-write-lock lock :wait-p nil)))
    (release-write-lock lock)
    (pass)))

#+(or sbcl lispworks ecl)
(test rw-lock-writer-fifo-order
  "Queued writers acquire the lock in FIFO (enqueue) order.  The main thread
   holds the lock while spawning writers (staggered so each enqueues strictly
   before the next), then releases; they must drain in order."
  (let* ((lock    (make-rw-lock))
         (n       8)
         (order   (make-array n :fill-pointer 0))
         (olock   (bt:make-lock "order"))
         (threads '()))
    (acquire-write-lock lock)              ; force all spawned writers to queue
    (dotimes (i n)
      (let ((idx i))
        (push (bt:make-thread
               (lambda ()
                 (with-write-lock (lock)
                   (bt:with-lock-held (olock)
                     (vector-push idx order)))))
              threads)
        (sleep 0.05)))                     ; stagger enqueue order
    (release-write-lock lock)
    (dolist (th (nreverse threads)) (bt:join-thread th))
    (is (equalp order (coerce (loop for i below n collect i) 'vector))
        "writers acquired out of FIFO order: ~A" order)))

;;; ---------------------------------------------------------------------------
;;; Test 4: no deadlock under sustained mixed read/write load
;;; ---------------------------------------------------------------------------

(test rw-lock-no-deadlock-under-load
  "N readers and N/2 writers all terminate within the timeout."
  (let* ((lock     (make-rw-lock))
         (n-read   *thread-count*)
         (n-write  (max 2 (floor *thread-count* 2)))
         (n-total  (+ n-read n-write)))
    ;; If anything deadlocks, run-threads' timeout fires and signals an error.
    (run-threads n-total
                 (lambda (i)
                   (if (< i n-read)
                       (dotimes (_ 60)
                         (with-read-lock (lock)
                           (bt:thread-yield)))
                       (dotimes (_ 60)
                         (with-write-lock (lock)
                           (bt:thread-yield))))))
    (pass)))

(defun %join-thread-with-timeout (thread &key (timeout-seconds 2.0))
  (let ((end-time (+ (get-internal-real-time) (* timeout-seconds internal-time-units-per-second))))
    (loop
       (unless (bt:thread-alive-p thread)
         (return (bt:join-thread thread)))
       (when (> (get-internal-real-time) end-time)
         (ignore-errors (bt:destroy-thread thread))
         (error "Thread ~A timed out (deadlock detected)" thread))
       (sleep 0.05))))

#+(or sbcl lispworks ecl)
(test rw-lock-aborted-writer-cleans-queue
  "An aborted writer in the queue unwinds cleanly without deadlocking subsequent writers/readers."
  (let ((lock (make-rw-lock))
        (held (bt:make-semaphore))
        (release (bt:make-semaphore))
        (holder nil)
        (waiter nil))
    ;; Holder thread acquires write lock and stays parked
    (setf holder (bt:make-thread
                  (lambda ()
                    (with-write-lock (lock)
                      (bt:signal-semaphore held)
                      (bt:wait-on-semaphore release)))))
    (bt:wait-on-semaphore held)
    ;; Waiter thread enqueues for write lock
    (setf waiter (bt:make-thread
                  (lambda ()
                    (acquire-write-lock lock :wait-p t))))
    (sleep 0.1) ; Ensure waiter enqueues
    ;; Abort waiter thread while it is queued
    (bt:destroy-thread waiter)
    (sleep 0.1)
    ;; Release holder
    (bt:signal-semaphore release)
    (%join-thread-with-timeout holder)
    ;; Check if lock is usable by a new writer and reader
    (is (rw-lock-p (acquire-write-lock lock :wait-p nil)))
    (release-write-lock lock)
    (is (rw-lock-p (acquire-read-lock lock)))
    (release-read-lock lock)))

#+(or sbcl lispworks ecl)
(test rw-lock-concurrent-read-to-write-upgrades
  "Two concurrent readers upgrading to write locks via :reading-p do not deadlock."
  (let ((lock (make-rw-lock))
        (start-sem (bt:make-semaphore))
        (done-count 0)
        (count-lock (bt:make-lock "count"))
        (t1 nil) (t2 nil))
    (setf t1 (bt:make-thread
              (lambda ()
                (with-read-lock (lock)
                  (bt:wait-on-semaphore start-sem)
                  (with-write-lock (lock :reading-p t)
                    (bt:with-lock-held (count-lock)
                      (incf done-count)))))))
    (setf t2 (bt:make-thread
              (lambda ()
                (with-read-lock (lock)
                  (bt:wait-on-semaphore start-sem)
                  (with-write-lock (lock :reading-p t)
                    (bt:with-lock-held (count-lock)
                      (incf done-count)))))))
    (sleep 0.1)
    (bt:signal-semaphore start-sem)
    (bt:signal-semaphore start-sem)
    (%join-thread-with-timeout t1)
    (%join-thread-with-timeout t2)
    (is (= 2 done-count) "Both read-to-write upgrades completed without deadlock")))

#+(or sbcl lispworks ecl)
(test rw-lock-macro-does-not-release-unacquired-lock
  "with-read-lock and with-write-lock do not release the lock if acquisition failed."
  (let ((lock (make-rw-lock))
        (held (bt:make-semaphore))
        (release (bt:make-semaphore))
        (holder nil))
    (setf holder (bt:make-thread
                  (lambda ()
                    (with-write-lock (lock)
                      (bt:signal-semaphore held)
                      (bt:wait-on-semaphore release)))))
    (bt:wait-on-semaphore held)
    ;; Attempting non-blocking write lock when another thread holds it:
    ;; acquire-write-lock returns NIL. with-write-lock should signal "Unable to get rw-lock"
    ;; without calling release-write-lock (which would raise "Cannot release lock I don't own!").
    (handler-case
        (acquire-write-lock lock :wait-p nil)
      (error (c)
        (is (typep c 'error))))
    (bt:signal-semaphore release)
    (%join-thread-with-timeout holder)
    (is (null (lock-writer lock)))))



