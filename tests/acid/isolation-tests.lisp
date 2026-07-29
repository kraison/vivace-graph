;;;; ACID-ISOLATION-SUITE
;;;;
;;;; Verifies isolation semantics under concurrent access:
;;;;   - No dirty reads (uncommitted writes are invisible)
;;;;   - Snapshot is frozen at transaction start
;;;;   - Write-write conflicts are detected and force correct retries

(in-package #:graph-db/acid-test)

(def-suite acid-isolation-suite
  :description "Transaction isolation: dirty reads, snapshot stability, conflict detection."
  :in acid-suite)

(in-suite acid-isolation-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: no dirty reads
;;;
;;; T1 writes V=99 inside a transaction but has not committed yet.  While T1 is
;;; in-flight, the main thread reads V and must see the original value (0).
;;; The graph-cache is only updated when apply-tx-writes runs inside the TM
;;; lock at commit time; pre-commit writes live in T1's local-cache only.
;;; ---------------------------------------------------------------------------

(test no-dirty-reads
  "A transaction's uncommitted write must not be visible to concurrent readers."
  (with-acid-graph (g)
    (let (vid)
      (with-transaction ()
        (setq vid (id (make-ac-item :value 0 :label "watched"))))
      (let ((t1-wrote (make-semaphore))
            (t2-read  (make-semaphore))
            t2-saw)
        ;; T1: writes V=99, signals T2 that it has written, waits before committing
        (make-thread
         (lambda ()
           (let ((*graph* g))
             (with-transaction ()
               (let* ((item (copy (lookup-vertex vid))))
                 (setf (slot-value item 'value) 99)
                 (save item))
               ;; Still inside with-transaction body (not committed yet)
               (signal-semaphore t1-wrote)
               (wait-on-semaphore t2-read))
             ;; Transaction commits here (body returned; %commit runs in cleanup)
           ))
         :name "acid-dirty-t1")
        ;; T2 (main): read V while T1 is in-flight
        (wait-on-semaphore t1-wrote)
        (setq t2-saw (slot-value (lookup-vertex vid) 'value))
        (signal-semaphore t2-read)
        (is (= 0 t2-saw)
            "T2 must see original value (0), not T1's uncommitted write (99); got ~D"
            t2-saw)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: snapshot frozen at start
;;;
;;; T1 opens a transaction and reads V (=0), caching it in its local-cache.
;;; T2 then commits V=99.  When T1 reads V again inside the same transaction,
;;; it must still see 0 — the local-cache provides snapshot isolation for
;;; repeated reads of the same vertex within one transaction.
;;; ---------------------------------------------------------------------------

(test snapshot-frozen-at-start
  "A vertex read twice in the same transaction must return the same value."
  (with-acid-graph (g)
    (let (vid)
      (with-transaction ()
        (setq vid (id (make-ac-item :value 0 :label "frozen"))))
      (let ((t1-read (make-semaphore))
            (t2-done (make-semaphore))
            t1-second-read)
        ;; T2: waits for T1 to read V, then commits V=99
        (make-thread
         (lambda ()
           (let ((*graph* g))
             (wait-on-semaphore t1-read)
             (with-transaction ()
               (let* ((item (copy (lookup-vertex vid))))
                 (setf (slot-value item 'value) 99)
                 (save item)))
             (signal-semaphore t2-done)))
         :name "acid-snapshot-t2")
        ;; T1: read V, signal T2, wait for T2 to commit, then re-read V
        (with-transaction ()
          ;; First read: puts V=0 in local-cache
          (slot-value (lookup-vertex vid) 'value)
          (signal-semaphore t1-read)
          (wait-on-semaphore t2-done)
          ;; Second read: must still come from local-cache (=0), not live cache
          (setq t1-second-read (slot-value (lookup-vertex vid) 'value)))
        (is (= 0 t1-second-read)
            "Snapshot must be frozen: expected 0, got ~D" t1-second-read)))))

;;; ---------------------------------------------------------------------------
;;; Test 3: write-write conflict forces retry with fresh value
;;;
;;; Two threads both increment the same counter.  The loser's conflict
;;; validation causes a retry; the retry reads the winner's committed value
;;; and increments from there.  Final value = initial + 2.
;;; ---------------------------------------------------------------------------

(test write-write-conflict-forces-retry
  "Two concurrent increments: conflict causes retry; final value = initial + 2."
  (with-acid-graph (g)
    (let (vid)
      (with-transaction ()
        (setq vid (id (make-ac-item :value 0 :label "contested"))))
      (run-threads 2
                   (lambda (i)
                     (declare (ignore i))
                     (with-transaction ()
                       (let* ((item (copy (lookup-vertex vid)))
                              (old  (slot-value item 'value)))
                         (setf (slot-value item 'value) (1+ old))
                         (save item)))))
      (is (= 2 (slot-value (lookup-vertex vid) 'value))
          "Final counter must be 2 (initial 0 + 2 successful increments)"))))

;;; ---------------------------------------------------------------------------
;;; Test 4: read-write conflict (stale read) forces validation failure / retry
;;;
;;; T1 reads Node X and writes Node Y.
;;; T2 concurrently modifies Node X and commits.
;;; T1's read-set (containing Node X) conflicts with T2's write-set (containing Node X).
;;; OCC validation for T1 MUST fail because T1 read a stale version of Node X.
;;; ---------------------------------------------------------------------------

(test read-write-conflict-forces-retry
  "T1 reads X and writes Y while T2 modifies X and commits: T1 validation fails / retries."
  (with-acid-graph (g)
    (let (x-id y-id)
      (with-transaction ()
        (setq x-id (id (make-ac-item :value 10 :label "X"))
              y-id (id (make-ac-item :value 100 :label "Y"))))
      (let ((t1-read-x (make-semaphore))
            (t2-committed-x (make-semaphore))
            (first-pass t))
        (make-thread
         (lambda ()
           (let ((*graph* g))
             (wait-on-semaphore t1-read-x)
             (with-transaction ()
               (let ((x (copy (lookup-vertex x-id))))
                 (setf (slot-value x 'value) 20)
                 (save x)))
             (signal-semaphore t2-committed-x)))
         :name "acid-read-write-t2")
        (with-transaction ()
          ;; T1 reads X
          (let ((x-val (slot-value (lookup-vertex x-id) 'value)))
            (when first-pass
              (setq first-pass nil)
              (signal-semaphore t1-read-x)
              (wait-on-semaphore t2-committed-x))
            ;; T1 writes Y based on its read of X
            (let ((y (copy (lookup-vertex y-id))))
              (setf (slot-value y 'value) (+ (slot-value y 'value) x-val))
              (save y))))
        ;; After T1 retries, it sees T2's updated X=20, so Y becomes 100 + 20 = 120 (not 100 + 10 = 110)
        (is (= 20 (slot-value (lookup-vertex x-id) 'value)))
        (is (= 120 (slot-value (lookup-vertex y-id) 'value))
            "T1 must re-read updated X (20) on retry, making Y = 100 + 20 = 120; got ~D"
            (slot-value (lookup-vertex y-id) 'value))))))



;;; ---------------------------------------------------------------------------
;;; Test 5: a COMMITTED transaction's reads must not abort a later writer (#92)
;;;
;;; VALIDATE used to carry a forward-validation clause,
;;;   (object-sets-intersect-p write-set (read-set other-transaction)),
;;; against OVERLAPPING-TRANSACTIONS -- which returns COMMITTED transactions
;;; only.  Forward validation is meaningful against transactions that are still
;;; ACTIVE and can still be invalidated; a committed one is finished, and since
;;; it committed first the serial order is other < this.  "Other read the old
;;; value, this writes the new one" is an ordinary read-then-write dependency.
;;; The clause prevented no anomaly and only produced retries.
;;; ---------------------------------------------------------------------------

(test committed-readers-do-not-abort-a-later-writer
  "T2 reads X and writes Y, then commits.  T1 -- started earlier, so T2 commits
inside T1's validation window -- then writes X, which T2 only READ.  T1 must
commit on its FIRST attempt: there is no anomaly to prevent, and a needless
retry is expensive (8 attempts, then a global transaction-manager lock)."
  (with-acid-graph (g)
    (let (x-id y-id z-id)
      (with-transaction ()
        (setq x-id (id (make-ac-item :value 1 :label "X"))
              y-id (id (make-ac-item :value 1 :label "Y"))
              z-id (id (make-ac-item :value 1 :label "Z"))))
      (let ((t1-started (make-semaphore))
            (t2-committed (make-semaphore))
            (first-pass t)
            (attempts 0))
        (make-thread
         (lambda ()
           (let ((*graph* g))
             (wait-on-semaphore t1-started)
             ;; T2 READS x and WRITES y.  It never writes x.
             (with-transaction ()
               (let ((xv (slot-value (lookup-vertex x-id) 'value))
                     (y (copy (lookup-vertex y-id))))
                 (setf (slot-value y 'value) (+ 100 xv))
                 (save y)))
             (signal-semaphore t2-committed)))
         :name "acid-committed-reader-t2")
        (with-transaction ()
          (incf attempts)
          ;; establish T1's window on an unrelated node, then let T2 commit
          (let ((z (copy (lookup-vertex z-id))))
            (setf (slot-value z 'value) 42)
            (save z))
          (when first-pass
            (setq first-pass nil)
            (signal-semaphore t1-started)
            (wait-on-semaphore t2-committed))
          ;; T1 writes X -- a node the committed T2 only READ
          (let ((x (copy (lookup-vertex x-id))))
            (setf (slot-value x 'value) 99)
            (save x)))
        (is (= 1 attempts)
            "T1 retried ~D time(s): a COMMITTED transaction's read-set must not ~
force a later writer to abort" (1- attempts))
        (is (= 99 (slot-value (lookup-vertex x-id) 'value))
            "T1's write must be durable")
        (is (= 101 (slot-value (lookup-vertex y-id) 'value))
            "T2's write must be durable")))))
