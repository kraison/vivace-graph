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
  "A nonexistent shadow location fails SWAP-IN-SHADOW before the first
rename; the live store is reopened (or was never closed) and still
serves reads and writes."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((nonexistent (merge-pathnames
                        "nope-shadow/"
                        (uiop:ensure-directory-pathname
                         (graph-db::location g)))))
      (signals error (graph-db:swap-in-shadow g nonexistent))
      (let* ((name (graph-db:graph-name g))
             (g2 (graph-db:lookup-graph name))
             (tm2 (graph-db::transaction-manager g2)))
        (is-true g2)
        (is (graph-db::graph-open-p g2))
        (is (eq t (graph-db:accepting-p tm2)))
        (let ((pin (graph-db:pin-read-epoch tm2)))
          (graph-db:unpin-read-epoch tm2 pin))
        (with-transaction (tm2)
          (graph-db:make-vertex :generic nil :graph g2))
        (setq g g2)))))

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
