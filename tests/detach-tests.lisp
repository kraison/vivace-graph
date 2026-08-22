;;;; The detach quiescence protocol (GH #170): a store refuses new
;;;; transactions and read pins, drains what is already in flight, leases
;;;; itself an epoch range from its system clock, journals the handoff,
;;;; and closes durably -- then can be reopened and rejoined later.
(in-package #:graph-db/test)

(def-suite detach-suite :in graph-db-suite
  :description "Quiescence, DETACH-STORE, and REATTACH-STORE.")
(in-suite detach-suite)

(defmacro with-clocked-store ((g clock sys &key recovery-policy) &body body)
  "One disk store, under a fresh *SYSTEM-DIRECTORY*, attached to its own
system clock -- the fixture DETACH-STORE / REATTACH-STORE need, adapted
from TESTS/SYSTEM-CLOCK-TESTS.LISP's
TWO-STORES-ON-ONE-CLOCK-GET-DISJOINT-ORDERED-EPOCHS.  :RECOVERY-POLICY
(GH #170 Task 4) is passed through to MAKE-GRAPH; NIL (default) persists
nothing, matching every pre-Task-4 use of this fixture."
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
                                          :system-clock ,clock
                                          :recovery-policy ,recovery-policy)))
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

;;; Swap (GH #170 Task 3).

(defun %directory-file-hashes (dir)
  "Map of relative-path-string -> sha256 digest, every regular file
under DIR.  Test-only byte-identity tool (GH #170)."
  (let ((root (uiop:ensure-directory-pathname dir))
        (table (make-hash-table :test 'equal)))
    (uiop:collect-sub*directories
     root t t
     (lambda (subdir)
       (dolist (file (uiop:directory-files subdir))
         (setf (gethash (namestring (uiop:enough-pathname file root)) table)
               (ironclad:digest-file :sha256 file)))))
    table))

(defun %directory-diff (before after)
  "(values ONLY-BEFORE ONLY-AFTER CHANGED), relative-path lists, comparing
two %DIRECTORY-FILE-HASHES tables (GH #170)."
  (let (only-before only-after changed)
    (maphash (lambda (k v)
               (multiple-value-bind (v2 present) (gethash k after)
                 (cond ((not present) (push k only-before))
                       ((not (equalp v v2)) (push k changed)))))
             before)
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (nth-value 1 (gethash k before))
                 (push k only-after)))
             after)
    (values only-before only-after changed)))

(defun %measured-exclusions (before after)
  "Derive an exclusion set for a REPEAT of the SAME round trip from a
%DIRECTORY-DIFF of one sample round trip.  A bare top-level file (no
\"/\") is excluded by its exact name (e.g. \".dirty\"); a file inside a
subdirectory is excluded by DIRECTORY PREFIX instead of its literal
name, because SNAPSHOT mints a fresh UUID'd filename under txn-log/ on
every close -- an exact-name exclusion would never match twice (GH
#170)."
  (let (exclusions)
    (multiple-value-bind (ob oa ch) (%directory-diff before after)
      (dolist (path (append ob oa ch))
        (let ((slash (position #\/ path :from-end t)))
          (pushnew (if slash (subseq path 0 (1+ slash)) path)
                   exclusions :test #'equal))))
    exclusions))

(defun %excluded-p (path exclusions)
  "True when PATH is covered by EXCLUSIONS: an exact match for a
top-level exclusion, or a prefix match for a directory exclusion (one
ending in \"/\") (GH #170)."
  (some (lambda (ex)
          (if (find #\/ ex)
              (and (<= (length ex) (length path))
                   (string= ex path :end2 (length ex)))
              (string= ex path)))
        exclusions))

(test swap-in-shadow-end-to-end
  "THE acceptance scenario: data A on the live store; SHADOW-STORE;
OPEN-SHADOW-GRAPH; write data B into the shadow; MEANWHILE the live
store serves a read and refuses a write reason :SHADOW-LOAD; close the
shadow; SWAP-IN-SHADOW; the new graph serves A+B and accepts a fresh
write; the retired dir exists; the journal carries :SWAP and :ATTACH."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (multiple-value-bind (start end)
          (graph-db:clock-lease-epochs clock 1000)
        (let ((sg (graph-db:open-shadow-graph
                   shadow-location :detach-store-1
                   :lease (cons start end))))
          (with-transaction ((graph-db::transaction-manager sg))
            (graph-db:make-vertex :generic nil :graph sg))
          (with-transaction ((graph-db::transaction-manager sg))
            (graph-db:make-vertex :generic nil :graph sg))
          ;; DURING the load: live serves a read, refuses a write.
          (let ((tm2 (graph-db::transaction-manager g2)))
            (let ((pin (graph-db:pin-read-epoch tm2)))
              (graph-db:unpin-read-epoch tm2 pin))
            (handler-case
                (progn
                  (with-transaction (tm2)
                    (graph-db:make-vertex :generic nil :graph g2))
                  (fail "write must be refused during the shadow window"))
              (graph-db:store-not-accepting-error (c)
                (is (eq :shadow-load (graph-db:store-not-accepting-reason c))))))
          (let ((graph-db:*graph* sg))
            (close-graph sg :snapshot-p nil))))
      (multiple-value-bind (new-graph retired-path)
          (graph-db:swap-in-shadow g2 shadow-location)
        (setq g new-graph)
        (is-true (probe-file retired-path))
        (is (= 3 (length (graph-db:map-vertices
                          #'identity new-graph :collect-p t))))
        (with-transaction ((graph-db::transaction-manager new-graph))
          (graph-db:make-vertex :generic nil :graph new-graph))
        (let* ((records (journal-records clock))
               (kinds (mapcar (lambda (r) (getf r :kind)) records)))
          (is-true (member :swap kinds))
          (is-true (member :attach kinds)))
        (ignore-errors
         (uiop:delete-directory-tree
          (uiop:ensure-directory-pathname retired-path)
          :validate t :if-does-not-exist :ignore))))))

(test swap-failure-before-rename-restores-service
  "A nonexistent shadow location fails SWAP-IN-SHADOW before ANYTHING
touches the live store (fix round 1): the precondition check runs
before the quiesce, so there is no outage at all, not merely a
recovered one.  Asserted by OBJECT IDENTITY, not just service: the
SAME graph object G stays the one LOOKUP-GRAPH returns, ACCEPTING-P
never leaves T, and a write succeeds immediately with no reopen in
between.  ABLATION: reverting SWAP-IN-SHADOW to validate AFTER
quiesce+close (the pre-fix ordering) makes the (EQ G ...) assertion
fail, because that ordering closes G and recovers onto a NEW graph
object -- a real, if brief, outage -- even though it also eventually
restores service."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((nonexistent (merge-pathnames
                        "nope-shadow/"
                        (uiop:ensure-directory-pathname
                         (graph-db::location g))))
          (tm (graph-db::transaction-manager g)))
      (signals error (graph-db:swap-in-shadow g nonexistent))
      (is (eq t (graph-db:accepting-p tm)))
      (is (eq g (graph-db:lookup-graph (graph-db:graph-name g))))
      (is (graph-db::graph-open-p g))
      (let ((pin (graph-db:pin-read-epoch tm)))
        (graph-db:unpin-read-epoch tm pin))
      (with-transaction (tm)
        (graph-db:make-vertex :generic nil :graph g)))))

;;; Fix round 1 (GH #170): a swap that completed its renames+journal but
;;; then fails on the follow-up reopen must be reported as a SUCCESS
;;; (recovered onto the new generation), not resignalled as a failure.

(test swap-completed-p-discriminates-progress
  "Pure unit test for the discrimination predicate itself: unset
PROGRESS means \"not yet complete, recover the OLD store and
resignal\"; PROGRESS marked complete means \"already succeeded,
recover the NEW one and return\"."
  (is (not (graph-db::%swap-completed-p (vector nil nil))))
  (is-true (graph-db::%swap-completed-p (vector t "/some/retired/path"))))

(test swap-in-shadow-1-progress-survives-a-post-rename-failure
  "%SWAP-IN-SHADOW-1 must mark PROGRESS complete BEFORE attempting the
follow-up OPEN-GRAPH, so a failure there is distinguishable from a
failure in the renames/journal themselves.  There is no clean way to
make SWAP-IN-SHADOW's own reopen fail once and then succeed on the
recovery retry without a purpose-built test hook in production code
(judged too invasive -- the same call the review flagged for
*SYSTEM-CLOCK*-unbinding): a corrupted file fails identically on every
subsequent open attempt, which only reproduces the already-tested
SHADOW-RECOVERY-FAILED path, never the \"recovered onto the new
generation\" happy path.  So this tests %SWAP-IN-SHADOW-1 directly
instead (honestly, per the review's own fallback): renames + journal
succeed, then the follow-up OPEN-GRAPH fails on a corrupted
schema.dat, and PROGRESS must already show completion when the error
propagates."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((name (graph-db:graph-name g))
          (location (graph-db::location g))
          (retired nil))
      (unwind-protect
           (progn
             (let ((graph-db:*graph* g))
               (close-graph g :snapshot-p nil))
             (with-temp-directory (shadow-dir)
               (let ((sg (make-graph name (namestring shadow-dir)
                                    :buffer-pool-size 1000)))
                 (with-transaction ((graph-db::transaction-manager sg))
                   (graph-db:make-vertex :generic nil :graph sg))
                 (let ((graph-db:*graph* sg))
                   (close-graph sg :snapshot-p nil)))
               ;; Corrupt the shadow's schema.dat: the reopen -- which
               ;; only runs AFTER both renames -- fails deterministically.
               (with-open-file (out (merge-pathnames "schema.dat" shadow-dir)
                                    :direction :output :if-exists :supersede)
                 (write-string "not a valid cl-store payload" out))
               (let ((progress (vector nil nil)))
                 (signals error
                   (graph-db::%swap-in-shadow-1
                    name location shadow-dir clock progress))
                 (is-true (aref progress 0))
                 (is-true (aref progress 1))
                 (setq retired (aref progress 1))
                 (is-true (probe-file retired)))))
        (when (and retired (probe-file retired))
          (ignore-errors
           (uiop:delete-directory-tree
            (uiop:ensure-directory-pathname retired)
            :validate t :if-does-not-exist :ignore)))))))

;;; Fix round 2 / GH #212: completion is the SECOND RENAME, not the
;;; :SWAP journal record after it -- a JOURNAL-APPEND failure must still
;;; be classified as a completed swap.

(test swap-in-shadow-1-progress-survives-a-journal-append-failure
  "No reliable way was found to make the REAL system clock's
JOURNAL-APPEND fail on just this one call without ALSO breaking the
recovery path's own :ATTACH journal-append -- both go through the same
CLOCK object, and ATTACH-TO-SYSTEM-CLOCK unconditionally re-journals on
every call, so any injection that breaks one breaks the other, turning
the intended \"recovered, warn, return\" case into a
SHADOW-RECOVERY-FAILED case instead.  Concretely: by the time
SWAP-IN-SHADOW runs in these tests, the clock's journal stream is
ALREADY open (WITH-CLOCKED-STORE's MAKE-GRAPH triggers an initial
:ATTACH record), so CHMOD'ing the clock's directory does nothing --
POSIX permission checks happen at OPEN, not at each WRITE to an
already-open descriptor -- and closing that shared stream directly
leaves it broken for the recovery call too, not just the one under
test.  So, per the coordinator's authorized fallback, this seam-tests
%SWAP-IN-SHADOW-1 directly with a MOCK clock: a real SYSTEM-CLOCK
struct (%MAKE-SYSTEM-CLOCK) whose LOCATION is a directory that does not
exist, so JOURNAL-APPEND's internal OPEN fails deterministically the
instant it tries to create the journal file there -- verified below to
actually signal.  PROGRESS must already be complete when that
propagates.  ABLATION: reverting the SETF to run AFTER JOURNAL-APPEND
(the pre-fix-round-2 placement) makes this test fail, since PROGRESS
would still be unset when the error propagates."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((name (graph-db:graph-name g))
          (location (graph-db::location g))
          (retired nil)
          (mock-clock (graph-db::%make-system-clock
                       :location "/nonexistent-dir-for-gh-170-212/"
                       :counter 1)))
      (unwind-protect
           (progn
             (let ((graph-db:*graph* g))
               (close-graph g :snapshot-p nil))
             (with-temp-directory (shadow-dir)
               (let ((sg (make-graph name (namestring shadow-dir)
                                    :buffer-pool-size 1000)))
                 (with-transaction ((graph-db::transaction-manager sg))
                   (graph-db:make-vertex :generic nil :graph sg))
                 (let ((graph-db:*graph* sg))
                   (close-graph sg :snapshot-p nil)))
               (let ((progress (vector nil nil)))
                 (signals error
                   (graph-db::%swap-in-shadow-1
                    name location shadow-dir mock-clock progress))
                 (is-true (aref progress 0)
                          "PROGRESS must be complete: both renames landed")
                 (is-true (aref progress 1))
                 (setq retired (aref progress 1))
                 (is-true (probe-file retired)
                          "the retired dir proves the first rename ran")
                 ;; The renames really did land: the shadow's data is now
                 ;; AT LOCATION, proving JOURNAL-APPEND -- not an earlier
                 ;; step -- is what failed.
                 (is-true (probe-file (merge-pathnames
                                       "schema.dat"
                                       (uiop:ensure-directory-pathname
                                        location)))))))
        (when (and retired (probe-file retired))
          (ignore-errors
           (uiop:delete-directory-tree
            (uiop:ensure-directory-pathname retired)
            :validate t :if-does-not-exist :ignore)))))))

;;; Fix round 3 (GH #170): %COPY-DIRECTORY-TREE must preserve sparse
;;; holes -- a fresh store reserves ~12GB of mostly-zero mmap regions,
;;; and a byte-for-byte copy materializes every one of them.

(defun %file-real-bytes (path)
  "REAL on-disk bytes for the file at PATH: STAT(2)'s ST_BLOCKS * 512,
the standard sparse-file idiom.  OSICAT is NOT a dependency of this
system (POSIX.LISP's own header explains why: it replaces OSICAT with
hand-rolled CFFI to avoid OSICAT's C grovel/wrapper, notably for ECL
cross-compilation) and this build's SB-POSIX:STAT has no ST_BLOCKS
accessor either -- so this uses SBCL's own SB-UNIX:UNIX-STAT, whose
14th value is ST-BLOCKS (confirmed against sbcl/src/code/unix.lisp's
%EXTRACT-STAT-RESULTS field order: dev ino mode nlink uid gid rdev size
atime mtime ctime blksize blocks) (GH #170).  SBCL-only:
SB-UNIX:UNIX-STAT is a bare SBCL symbol, so the whole body is #+SBCL --
otherwise it is a read-time package error on ECL/CCL (fold-in from Task
3's re-review; see tests/geometry-tests.lisp for the same #+sbcl idiom)."
  #+sbcl
  (multiple-value-bind (ok dev ino mode nlink uid gid rdev size
                        atime mtime ctime blksize blocks)
      (sb-unix:unix-stat (namestring path))
    (declare (ignore dev ino mode nlink uid gid rdev size
                     atime mtime ctime blksize))
    (unless ok (error "SB-UNIX:UNIX-STAT failed for ~A" path))
    (* 512 blocks))
  #-sbcl
  (error "%FILE-REAL-BYTES (sparse-file real-usage check) is SBCL-only; ~
no portable ST_BLOCKS accessor is wired up for this implementation."))

(defun %directory-usage (dir)
  "(values APPARENT-SIZES REAL-BYTES) for every regular file under DIR:
APPARENT-SIZES is a relpath -> byte-length hash table (the logical
FILE-LENGTH); REAL-BYTES is the sum of %FILE-REAL-BYTES over every file
(GH #170)."
  (let ((root (uiop:ensure-directory-pathname dir))
        (sizes (make-hash-table :test 'equal))
        (real 0))
    (uiop:collect-sub*directories
     root t t
     (lambda (subdir)
       (dolist (file (uiop:directory-files subdir))
         (setf (gethash (namestring (uiop:enough-pathname file root)) sizes)
               (with-open-file (in file :element-type '(unsigned-byte 8))
                 (file-length in)))
         (incf real (%file-real-bytes file)))))
    (values sizes real)))

(test shadow-store-copy-preserves-sparseness
  "%COPY-DIRECTORY-TREE must not materialize a fresh store's reserved
mmap holes: a near-empty store's shadow should use a SMALL FRACTION of
its apparent size on disk (< 1/100 here), while every file's apparent
LENGTH matches the source exactly -- MAPPED-FILE-LENGTH (mmap.lisp)
reads the file's actual on-disk length to size the initial mapping, so
a short file breaks remapping; this is confirmed by reading the code,
not merely assumed.  Also confirms sparseness doesn't corrupt: the
shadow still OPENS (OPEN-SHADOW-GRAPH) and reads the live data back."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (source-sizes source-real)
        (%directory-usage (graph-db::location g))
      (declare (ignore source-real))
      ;; .dirty exists only while G is OPEN (as it is here, pre-
      ;; SHADOW-STORE) and is gone by the time SHADOW-STORE's internal
      ;; CLOSE-GRAPH runs the copy -- not part of what the copy is
      ;; supposed to reproduce, so exclude it from the comparison
      ;; (same idiom as KILLED-LOADER-LEAVES-LIVE-BYTE-IDENTICAL's
      ;; measured exclusion list).
      (remhash ".dirty" source-sizes)
      (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
        (unwind-protect
             (progn
               (multiple-value-bind (shadow-sizes shadow-real)
                   (%directory-usage shadow-location)
                 ;; Every SOURCE file must have landed in the shadow at
                 ;; the exact same apparent size.  Not asserted the
                 ;; other way around: SHADOW-STORE's internal
                 ;; CLOSE-GRAPH :SNAPSHOT-P T takes a backup under
                 ;; txn-log/ as part of the close that PRECEDES the
                 ;; copy, so the shadow legitimately gains one file
                 ;; SOURCE-SIZES (measured before that close) never
                 ;; had -- same shape as the .dirty exclusion above.
                 (maphash (lambda (path size)
                            (is (eql size (gethash path shadow-sizes))
                                "~A: apparent size must match the source ~
exactly" path))
                          source-sizes)
                 (let ((apparent (loop for v being the hash-values
                                            of shadow-sizes
                                       sum v)))
                   (is (plusp apparent))
                   (is (< shadow-real (/ apparent 100))
                       "real ~A bytes should be a small fraction of ~
apparent ~A bytes" shadow-real apparent)))
               (multiple-value-bind (start end)
                   (graph-db:clock-lease-epochs clock 1000)
                 (let ((sg (graph-db:open-shadow-graph
                           shadow-location :detach-store-1
                           :lease (cons start end))))
                   (unwind-protect
                        (is (= 1 (length (graph-db:map-vertices
                                         #'identity sg :collect-p t))))
                     (let ((graph-db:*graph* sg))
                       (ignore-errors (close-graph sg :snapshot-p nil)))))))
          (let ((graph-db:*graph* g2))
            (ignore-errors (close-graph g2 :snapshot-p nil))))))))

(test killed-loader-leaves-live-byte-identical
  "ABANDON-SHADOW is the discard path exercised here; it must also
restore write service.  Nearest wrong implementation: the loader writes
into the live mmaps -- heap.dat diverges.  The exclusion list (files a
bare round trip legitimately touches, e.g. .dirty) is MEASURED here
first, from a bare SHADOW-STORE/ABANDON-SHADOW round trip on this same
store, not asserted."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((location (graph-db::location g))
           (snap0 (%directory-file-hashes location)))
      ;; Measure: a bare round trip, no write into the shadow, so any
      ;; diff on the live dir is purely the close+reopen the live store
      ;; itself goes through.
      (multiple-value-bind (shadow1 g1) (graph-db:shadow-store g)
        (graph-db:abandon-shadow g1 shadow1)
        (setq g g1))
      (let* ((snap1 (%directory-file-hashes location))
             (exclusions (%measured-exclusions snap0 snap1)))
        (is (member ".dirty" exclusions :test #'equal)
            "the bare round trip is expected to touch .dirty")
        ;; Real scenario: shadow, write garbage, kill the loader (never
        ;; close its handle), ABANDON-SHADOW.
        (multiple-value-bind (shadow2 g2) (graph-db:shadow-store g)
          (multiple-value-bind (start end)
              (graph-db:clock-lease-epochs clock 1000)
            (let ((sg (graph-db:open-shadow-graph
                       shadow2 :detach-store-1 :lease (cons start end))))
              (with-transaction ((graph-db::transaction-manager sg))
                (graph-db:make-vertex :generic nil :graph sg))))
          (graph-db:abandon-shadow g2 shadow2)
          (setq g g2)
          (is (not (probe-file shadow2)))
          (let ((snap2 (%directory-file-hashes location)))
            (multiple-value-bind (ob oa ch) (%directory-diff snap1 snap2)
              (flet ((uncovered (paths)
                       (remove-if (lambda (p) (%excluded-p p exclusions))
                                  paths)))
                (is (null (uncovered ob))
                    "files disappeared beyond the measured exclusion list")
                (is (null (uncovered oa))
                    "files appeared beyond the measured exclusion list")
                (is (null (uncovered ch))
                    "file content changed beyond the measured exclusion list"))))
          (with-transaction ((graph-db::transaction-manager g2))
            (graph-db:make-vertex :generic nil :graph g2)))))))

(test swap-in-shadow-drains-in-flight-readers-first
  "A pin held on the LIVE store across SWAP-IN-SHADOW delays it, same
drain mechanism as DETACH-DRAINS-IN-FLIGHT-READERS-FIRST -- confirms
SWAP-IN-SHADOW really quiesces rather than closing straight away."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (multiple-value-bind (start end)
          (graph-db:clock-lease-epochs clock 1000)
        (let ((sg (graph-db:open-shadow-graph
                   shadow-location :detach-store-1
                   :lease (cons start end))))
          (let ((graph-db:*graph* sg))
            (close-graph sg :snapshot-p nil))))
      (let* ((tm2 (graph-db::transaction-manager g2))
             (pin (graph-db:pin-read-epoch tm2))
             (done nil)
             (result nil)
             ;; SWAP-IN-SHADOW reopens (OPEN-GRAPH), which needs
             ;; *SYSTEM-DIRECTORY*/*STORE-REGISTRY* -- dynamic bindings
             ;; from WITH-CLOCKED-STORE's LET do not cross to a new
             ;; thread, so re-bind explicitly here (GH #170).
             (sysdir graph-db::*system-directory*)
             (registry graph-db::*store-registry*)
             (thread (bt:make-thread
                      (lambda ()
                        (let ((graph-db::*system-directory* sysdir)
                              (graph-db::*store-registry* registry))
                          (setq result (graph-db:swap-in-shadow
                                       g2 shadow-location))
                          (setq done t))))))
        (unwind-protect
             (progn
               (sleep 0.3)
               (is (not done)
                   "swap-in-shadow must not complete while a live pin holds")
               (graph-db:unpin-read-epoch tm2 pin)
               (bt:join-thread thread)
               (is-true done)
               (with-transaction ((graph-db::transaction-manager result))
                 (graph-db:make-vertex :generic nil :graph result))
               (setq g result))
          (ignore-errors (bt:join-thread thread)))))))

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

(test shadow-lease-consumed-exactly-to-its-boundary-exhausts-at-open
  "Fix round 2 (GH #170): the range is half-open [start,end) --
NEXT == END means fully consumed, not merely at the boundary.  Consume
a 2-epoch lease [n, n+2) with exactly 2 writes, close, and re-open from
lease.dat WITHOUT :lease: EPOCH-LEASE-EXHAUSTED must signal AT OPEN, not
be deferred to the first write.  Nearest wrong implementation: a
strict > check that lets NEXT == END silently reopen and only fails
later."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 2)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end))))
               (dotimes (i 2)
                 (with-transaction ((graph-db::transaction-manager sg))
                   (graph-db:make-vertex :generic nil :graph sg)))
               (let ((graph-db:*graph* sg))
                 (close-graph sg :snapshot-p nil))
               (signals graph-db:epoch-lease-exhausted
                 (graph-db:open-shadow-graph
                  shadow-location :detach-store-1))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

;;; Recovery policy + the WAL-suppressed fast path (GH #170 Task 4).

(defun %txn-file-count (graph)
  "Count of *.txn files in GRAPH's persistent transaction directory
(GH #170)."
  (length (directory
           (merge-pathnames
            "*.txn"
            (graph-db::persistent-transaction-directory graph)))))

(test recovery-policy-round-trips-through-make-graph
  "MAKE-GRAPH :RECOVERY-POLICY :DERIVABLE persists policy.dat, and
STORE-RECOVERY-POLICY reads it back; a store made without the keyword
has no policy.dat and reads back :AUTHORED (the documented absent-file
default)."
  (with-clocked-store (g clock sys :recovery-policy :derivable)
    clock sys
    (is (eq :derivable
            (graph-db:store-recovery-policy (graph-db::location g)))))
  (with-clocked-store (g clock sys)
    clock sys
    (is (eq :authored
            (graph-db:store-recovery-policy (graph-db::location g))))))

(test recovery-policy-file-rejects-garbage
  "A policy.dat that holds neither :DERIVABLE nor :AUTHORED is a hard
error, not a silent fall back to :AUTHORED -- a corrupt gate input for
:FAST-LOAD must never pass unnoticed.  SET-STORE-RECOVERY-POLICY itself
also refuses a bad value outright."
  (with-clocked-store (g clock sys)
    clock sys
    (signals error
      (graph-db:set-store-recovery-policy (graph-db::location g) :bogus))
    (with-open-file (out (merge-pathnames
                          "policy.dat"
                          (uiop:ensure-directory-pathname
                           (graph-db::location g)))
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (prin1 :bogus out))
    (signals error
      (graph-db:store-recovery-policy (graph-db::location g)))))

(test fast-load-on-authored-store-signals
  "OPEN-SHADOW-GRAPH :FAST-LOAD T against a shadow whose source store's
recovery policy is :AUTHORED (the default -- no :RECOVERY-POLICY was
ever given) signals FAST-LOAD-REQUIRES-DERIVABLE rather than silently
dropping the WAL.  ABLATION (recorded, not re-run here): remove the
gate in OPEN-SHADOW-GRAPH and this test is the one that fails."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 1000)
             (signals graph-db:fast-load-requires-derivable
               (graph-db:open-shadow-graph
                shadow-location :detach-store-1
                :lease (cons start end) :fast-load t)))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test fast-load-writes-no-txn-files-and-data-survives-reopen
  "OPEN-SHADOW-GRAPH :FAST-LOAD T against a :DERIVABLE source: writes
into the shadow leave its transaction directory EMPTY of .txn files
(PERSIST-TRANSACTION's WAL-suppressed no-op), yet the data reads back
after CLOSE-GRAPH + a plain reopen -- durability comes from the
heap/index writes themselves, which is what licenses discard-on-crash
for a shadow (the spec's whole premise for this path)."
  (with-clocked-store (g clock sys :recovery-policy :derivable)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 1000)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end) :fast-load t)))
               (is-true (graph-db::wal-suppressed-p sg))
               (dotimes (i 3)
                 (with-transaction ((graph-db::transaction-manager sg))
                   (graph-db:make-vertex :generic nil :graph sg)))
               (is (zerop (%txn-file-count sg)))
               (let ((graph-db:*graph* sg))
                 (close-graph sg :snapshot-p nil))
               (let ((sg2 (graph-db:open-shadow-graph
                           shadow-location :detach-store-1)))
                 (unwind-protect
                      (is (= 4 (length (graph-db:map-vertices
                                       #'identity sg2 :collect-p t))))
                   (let ((graph-db:*graph* sg2))
                     (ignore-errors
                      (close-graph sg2 :snapshot-p nil)))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test normal-graph-still-writes-txn-files
  "The no-leak pin: an ordinary (non-shadow) graph's commit really does
rename a .txn file into place -- CLEANUP-TRANSACTION deletes it right
after (the default *DELETE-COMMITTED-TRANSACTION-FILES* T), so bind
that NIL and look for the renamed *.committed file instead, proof that
FINALIZE-TX-PERSISTENCE's rename actually ran.  Nearest wrong
implementation: WAL suppression via a dynamic variable, which -- unlike
the per-graph WAL-SUPPRESSED-P slot this checks indirectly by observing
its effect -- would leak onto whatever graph happens to be committing
on the thread that last set it."
  (with-clocked-store (g clock sys)
    clock sys
    (is (= 0 (%txn-file-count g)))
    (let ((graph-db::*delete-committed-transaction-files* nil))
      (with-transaction ((graph-db::transaction-manager g))
        (graph-db:make-vertex :generic nil :graph g)))
    (is (= 1 (length (directory
                      (merge-pathnames
                       "*.committed"
                       (graph-db::persistent-transaction-directory g))))))))
