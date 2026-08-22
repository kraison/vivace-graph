;;;; The detach quiescence protocol (GH #170): a store refuses new
;;;; transactions and read pins, drains what is already in flight, leases
;;;; itself an epoch range from its system clock, journals the handoff,
;;;; and closes durably -- then can be reopened and rejoined later.
(in-package #:graph-db/test)

(def-suite detach-suite :in graph-db-suite
  :description "Quiescence, DETACH-STORE, and REATTACH-STORE.")
(in-suite detach-suite)

(defmacro with-clocked-store ((g clock sys) &body body)
  "One disk store, under a fresh *SYSTEM-DIRECTORY*, attached to its own
system clock -- the fixture DETACH-STORE / REATTACH-STORE need, adapted
from TESTS/SYSTEM-CLOCK-TESTS.LISP's
TWO-STORES-ON-ONE-CLOCK-GET-DISJOINT-ORDERED-EPOCHS."
  (let ((cdir (gensym)) (ddir (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,cdir)
         (with-temp-directory (,ddir)
           (let ((graph-db::*system-directory* (namestring ,sys))
                 (graph-db::*store-registry* nil))
             (let ((,clock (open-system-clock (namestring ,cdir))))
               (unwind-protect
                    (let ((,g (make-graph :detach-store-1
                                          (namestring ,ddir)
                                          :buffer-pool-size 1000
                                          :system-clock ,clock)))
                      (unwind-protect (progn ,@body)
                        (when (graph-db::graph-open-p ,g)
                          (let ((graph-db:*graph* ,g))
                            (ignore-errors
                             (close-graph ,g :snapshot-p nil))))
                        (collect-garbage)))
                 (close-system-clock ,clock)))))))))

(test detach-refuses-new-transactions-and-pins
  "During and after quiesce, a NEW transaction and a NEW read pin both
signal STORE-NOT-ACCEPTING-ERROR.  Nearest wrong implementation: close
without refusing -- the segfault-shaped hazard the spec names."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((detachment (graph-db:detach-store g)))
      (is (graph-db::store-detachment-p detachment))
      (signals graph-db:store-not-accepting-error
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db:make-vertex :generic nil :graph g)))
      (signals graph-db:store-not-accepting-error
        (graph-db:pin-read-epoch (graph-db::transaction-manager g))))))

(test detach-drains-in-flight-readers-first
  "A pinned reader taken BEFORE detach holds the drain; detach completes
only after the pin is released.  Mechanism: take a pin, start
DETACH-STORE in a second thread, assert it has NOT completed while the
pin is held, release the pin, join, assert detached."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((tm (graph-db::transaction-manager g))
           (pin (graph-db:pin-read-epoch tm))
           (done nil)
           (result nil)
           (thread (bt:make-thread
                    (lambda ()
                      (setq result (graph-db:detach-store g :timeout 5))
                      (setq done t)))))
      (unwind-protect
           (progn
             (sleep 0.3)
             (is (not done) "detach must not complete while the pin holds")
             (graph-db:unpin-read-epoch tm pin)
             (bt:join-thread thread)
             (is-true done)
             (is (graph-db::store-detachment-p result))
             (is (not (graph-db::graph-open-p g))))
        (ignore-errors (bt:join-thread thread))))
    clock))

(test detach-timeout-restores-service
  "A drain that cannot complete times out, signals
DETACH-DRAIN-TIMEOUT, and the store ACCEPTS transactions again -- a
failed detach must not strand the store half-dead."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((tm (graph-db::transaction-manager g))
           (pin (graph-db:pin-read-epoch tm)))
      (unwind-protect
           (signals graph-db:detach-drain-timeout
             (graph-db:detach-store g :timeout 1))
        (graph-db:unpin-read-epoch tm pin))
      (is (eq t (graph-db:accepting-p tm)))
      (with-transaction (tm)
        (graph-db:make-vertex :generic nil :graph g)))))

(test detach-journals-and-leases
  "The clock journal records :detach with the lease range; the clock's
next epoch is past LEASE-END (CLOCK-LEASE-EPOCHS semantics)."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (graph-db:detach-store g :lease-epochs 100)
    (let* ((records (journal-records clock))
           (record (find :detach records :key (lambda (r) (getf r :kind)))))
      (is-true record)
      (is-true (getf record :lease-start))
      (is-true (getf record :lease-end))
      (is (>= (clock-current-epoch clock) (getf record :lease-end))))))

(test detached-store-is-detached-to-the-resolver
  "#169 integration: after DETACH-STORE, RESOLVE-NODE-GRAPH on a node id
minted there reports :DETACHED and LOOKUP-VERTEX-ANYWHERE returns the
marker."
  (with-clocked-store (g clock sys)
    clock sys
    (let (v)
      (with-transaction ((graph-db::transaction-manager g))
        (setq v (graph-db:make-vertex :generic nil :graph g)))
      (graph-db:detach-store g)
      (is (eq :detached (nth-value 1 (graph-db:resolve-node-graph (id v)))))
      (is (graph-db:unresolved-node-p
           (graph-db:lookup-vertex-anywhere (id v)))))))

(test reattach-restores-service-and-journals
  "REATTACH-STORE reopens, the same node reads back, a new write
succeeds, and the journal carries the :attach record after the :detach
one."
  (with-clocked-store (g clock sys)
    sys
    (let (v vid)
      (with-transaction ((graph-db::transaction-manager g))
        (setq v (graph-db:make-vertex :generic nil :graph g)))
      (setq vid (id v))
      (let ((detachment (graph-db:detach-store g))
            (*system-clock* clock))
        (let ((g2 (graph-db:reattach-store detachment)))
          (unwind-protect
               (progn
                 (is (graph-db::vertex-p (lookup-vertex vid :graph g2)))
                 (with-transaction ((graph-db::transaction-manager g2))
                   (graph-db:make-vertex :generic nil :graph g2))
                 (let* ((records (journal-records clock))
                        (kinds (mapcar (lambda (r) (getf r :kind)) records))
                        (detach-pos (position :detach kinds))
                        (attach-pos (position :attach kinds
                                              :from-end t)))
                   (is-true detach-pos)
                   (is-true attach-pos)
                   (is (< detach-pos attach-pos))))
            (let ((graph-db:*graph* g2))
              (ignore-errors (close-graph g2 :snapshot-p nil)))))))))

(test pin-admission-is-atomic-with-quiesce
  "Fix round 1, GH #170: PIN-READ-EPOCH's accepting-p check and
%QUIESCE-TRANSACTION-MANAGER's flip must be one atomic operation --
otherwise a racing PIN-READ-EPOCH can slip a registration in AFTER the
drain has already reported success (i.e. after DETACH-STORE has begun
tearing down mmaps).  One thread hammers PIN-READ-EPOCH/UNPIN-READ-EPOCH
in a tight loop for the whole duration of a concurrent DETACH-STORE;
after DETACH-STORE returns, the read-pins table must be EMPTY (no
straggler survived the drain) and the hammer thread must have seen at
least one refusal.

This is a BEST-EFFORT race canary, not the proof -- the proof is the
LOCK DISCIPLINE: %QUIESCE-TRANSACTION-MANAGER's flip takes TM-LOCK then
READ-PINS-LOCK (see %SET-ACCEPTING-P); PIN-READ-EPOCH's check-and-
register runs as one critical section under READ-PINS-LOCK alone.  No
existing caller nests these two locks in the opposite order (see
%SET-ACCEPTING-P's docstring), so the flip and a pin attempt can never
interleave: either PIN-READ-EPOCH's critical section runs first (and
registers before REASON is visible, correctly holding the drain open),
or the flip's critical section runs first (and PIN-READ-EPOCH sees
REASON and refuses) -- never a check that reads T followed by a
registration that lands after the flip.  This test can only ever
demonstrate the ABSENCE of the race on this run; the lock discipline is
what guarantees its absence on every run."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((tm (graph-db::transaction-manager g))
           (stop nil)
           (refusals 0)
           (hammer (bt:make-thread
                    (lambda ()
                      (loop until stop do
                        (handler-case
                            (let ((tok (graph-db:pin-read-epoch tm)))
                              (graph-db:unpin-read-epoch tm tok))
                          (graph-db:store-not-accepting-error ()
                            (incf refusals))))))))
      (graph-db:detach-store g)
      (setq stop t)
      (bt:join-thread hammer)
      (is (plusp refusals) "the hammer thread must see the refusal")
      (is (zerop (hash-table-count (graph-db::read-pins tm)))
          "no straggler pin admitted after drain success"))))
