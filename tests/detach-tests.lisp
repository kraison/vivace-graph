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

;;; Shadow generations (GH #170 Task 2).

(test shadow-copy-is-consistent-and-reads-resume
  "SHADOW-STORE makes a consistent copy while the live store resumes
serving reads once it reopens; OPEN-SHADOW-GRAPH sees the same data."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g)
      (graph-db:make-vertex :generic nil :graph g)
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (progn
             (is-true (probe-file shadow-location))
             (let* ((tm2 (graph-db::transaction-manager g2))
                    (pin (graph-db:pin-read-epoch tm2))
                    (count nil))
               (unwind-protect
                    (setq count (length (graph-db:map-vertices
                                         #'identity g2 :collect-p t)))
                 (graph-db:unpin-read-epoch tm2 pin))
               (is (= 3 count)))
             (multiple-value-bind (start end)
                 (graph-db:clock-lease-epochs clock 1000)
               (let ((sg (graph-db:open-shadow-graph
                          shadow-location :detach-store-1
                          :lease (cons start end))))
                 (unwind-protect
                      (is (= 3 (length (graph-db:map-vertices
                                       #'identity sg :collect-p t))))
                   (let ((graph-db:*graph* sg))
                     (ignore-errors (close-graph sg :snapshot-p nil)))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test live-store-is-read-only-during-the-shadow-window
  "Kevin's ruling: after SHADOW-STORE a new write on the live graph
signals STORE-NOT-ACCEPTING-ERROR reason :SHADOW-LOAD while
PIN-READ-EPOCH keeps succeeding; ABANDON-SHADOW restores writes."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (let ((tm2 (graph-db::transaction-manager g2)))
             (handler-case
                 (progn
                   (with-transaction (tm2)
                     (graph-db:make-vertex :generic nil :graph g2))
                   (fail "write must be refused during the shadow window"))
               (graph-db:store-not-accepting-error (c)
                 (is (eq :shadow-load
                         (graph-db:store-not-accepting-reason c)))))
             (let ((pin (graph-db:pin-read-epoch tm2)))
               (graph-db:unpin-read-epoch tm2 pin))
             (graph-db:abandon-shadow g2 shadow-location)
             (is (not (probe-file shadow-location)))
             (with-transaction (tm2)
               (graph-db:make-vertex :generic nil :graph g2)))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test shadow-is-unregistered
  "While a shadow is open: *GRAPHS* has no second entry, the open-store
vector still maps the store-id to the LIVE graph, and RESOLVE-NODE-GRAPH
on a shadow-minted id resolves to the LIVE graph."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 1000)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end))))
               (unwind-protect
                    (progn
                      (is (= 1 (hash-table-count graph-db::*graphs*)))
                      (is (eq g2 (gethash :detach-store-1 graph-db::*graphs*)))
                      (is (eq g2 (svref graph-db::*store-id->graph*
                                       (graph-db::store-id g2))))
                      (let (vid)
                        (with-transaction ((graph-db::transaction-manager sg))
                          (setq vid (id (graph-db:make-vertex
                                        :generic nil :graph sg))))
                        (is (eq g2 (graph-db:resolve-node-graph vid)))))
                 (let ((graph-db:*graph* sg))
                   (ignore-errors (close-graph sg :snapshot-p nil))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test shadow-writes-draw-from-the-lease
  "A node written in the shadow gets a transaction id within
[lease-start, lease-end); exhausting a tiny lease signals
EPOCH-LEASE-EXHAUSTED. Nearest wrong implementation: shadow ids from
the store's own counter -- colliding with the live store's writes."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 3)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end))))
               (unwind-protect
                    (progn
                      (let (v)
                        (with-transaction ((graph-db::transaction-manager sg))
                          (setq v (graph-db:make-vertex
                                  :generic nil :graph sg)))
                        (is (<= start (graph-db::commit-epoch v)))
                        (is (< (graph-db::commit-epoch v) end)))
                      (signals graph-db:epoch-lease-exhausted
                        (dotimes (i 5)
                          (with-transaction ((graph-db::transaction-manager sg))
                            (graph-db:make-vertex :generic nil :graph sg)))))
                 (let ((graph-db:*graph* sg))
                   (ignore-errors (close-graph sg :snapshot-p nil))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test lease-survives-in-the-shadow-directory
  "lease.dat exists in the shadow dir and reads back the exact range --
the out-of-process survival requirement (GH #170 comment)."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 1000)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end))))
               (let ((graph-db:*graph* sg))
                 (ignore-errors (close-graph sg :snapshot-p nil))))
             (let ((file (merge-pathnames
                          "lease.dat"
                          (uiop:ensure-directory-pathname shadow-location))))
               (is-true (probe-file file))
               (let (form)
                 (with-open-file (in file)
                   (let ((*read-eval* nil))
                     (setq form (read in))))
                 (is (= start (getf form :lease-start)))
                 (is (= end (getf form :lease-end))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test discard-shadow-refuses-non-shadow-paths
  "THE deletion-safety test: DISCARD-SHADOW refuses any path that does
not end in \"-shadow\" -- it deletes trees, and this guard is the whole
safety story."
  (with-clocked-store (g clock sys)
    clock sys
    (signals error (graph-db:discard-shadow (graph-db::location g)))
    (is-true (probe-file (graph-db::location g)))))

;;; Fix round 1 (GH #170): lease resume-from-watermark, copy-failure
;;; recovery.

(test shadow-lease-resumes-from-its-own-watermark
  "RULED FIX: the resume cursor comes from the shadow's own durable
highest-transaction-id, never a persisted NEXT.  Write 2 nodes, close
the shadow, re-open it WITHOUT :lease (lease.dat path) and confirm the
next write's id is strictly past the pre-close ids and still within the
leased range.  Nearest wrong implementation: NEXT reset to START on
every open, re-minting an id already committed in the shadow."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 1000)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end)))
                   (max-id 0))
               (dotimes (i 2)
                 ;; COMMIT-EPOCH is stamped when the transaction commits,
                 ;; at the END of the WITH-TRANSACTION body -- read it
                 ;; only AFTER the block, or it is still the pre-commit
                 ;; default (0).
                 (let (v)
                   (with-transaction ((graph-db::transaction-manager sg))
                     (setq v (graph-db:make-vertex :generic nil :graph sg)))
                   (setq max-id (max max-id (graph-db::commit-epoch v)))))
               (let ((graph-db:*graph* sg))
                 (close-graph sg :snapshot-p nil))
               (let ((sg2 (graph-db:open-shadow-graph
                           shadow-location :detach-store-1)))
                 (unwind-protect
                      (let (v new-id)
                        (with-transaction
                            ((graph-db::transaction-manager sg2))
                          (setq v (graph-db:make-vertex
                                  :generic nil :graph sg2)))
                        (setq new-id (graph-db::commit-epoch v))
                        (is (> new-id max-id))
                        (is (<= start new-id))
                        (is (< new-id end)))
                   (let ((graph-db:*graph* sg2))
                     (ignore-errors
                      (close-graph sg2 :snapshot-p nil)))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test shadow-store-recovers-service-on-copy-failure
  "RULED FIX: a mid-copy failure must not leave the live store stranded
closed.  Deterministic failure: pre-create the shadow target as a plain
FILE (not a directory), so %COPY-DIRECTORY-TREE's
ENSURE-DIRECTORIES-EXIST signals.  SHADOW-STORE must re-signal that
error, but only after reopening the live store and restoring full
service (reads AND writes)."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((name (graph-db:graph-name g))
           (shadow-dir (graph-db::%shadow-location (graph-db:location g)))
           (bare-shadow (string-right-trim "/" (namestring shadow-dir))))
      (with-open-file (out bare-shadow :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
        (write-string "not a directory" out))
      (unwind-protect
           (progn
             (signals error (graph-db:shadow-store g))
             (let* ((g2 (graph-db:lookup-graph name))
                    (tm2 (graph-db::transaction-manager g2)))
               (is-true g2)
               (is (graph-db::graph-open-p g2))
               (is (eq t (graph-db:accepting-p tm2)))
               (let ((pin (graph-db:pin-read-epoch tm2)))
                 (graph-db:unpin-read-epoch tm2 pin))
               (with-transaction (tm2)
                 (graph-db:make-vertex :generic nil :graph g2))
               (setq g g2)))
        (ignore-errors (delete-file bare-shadow))))))
