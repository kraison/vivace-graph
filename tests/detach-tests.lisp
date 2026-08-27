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

;;; Vector-segment presize (GH #170 Task 5).
;;;
;;; A vertex on the DETACH-STORE-1 schema (same graph name WITH-CLOCKED-STORE
;;; uses everywhere else in this file) carrying one :VECTOR-INDEX slot, so
;;; OPEN-SHADOW-GRAPH's :EXPECTED-VECTORS wiring has a real segment to
;;; presize.  Declared once at load time, like SI-DOC in
;;; segment-integration-tests.lisp.
(def-vertex ds-vec-doc ()
  ((embedding :vector-index t))
  :detach-store-1)

(defun %ds-vec (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v)
      (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(test open-shadow-graph-expected-vectors-presizes-the-segment
  "OPEN-SHADOW-GRAPH :EXPECTED-VECTORS presizes every vector segment the
shadow's graph object carries: capacity is already >= the requested N right
after open, and a subsequent burst of vector-bearing writes on the shadow
does not grow the segment again.  Exercises the wiring end to end: a live
write creates the segment, SHADOW-STORE copies its file, and RESTORE-VECTOR-
SEGMENTS (inside OPEN-GRAPH, before :EXPECTED-VECTORS runs) is what
populates VECTOR-SEGMENTS for this hook to iterate."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (make-ds-vec-doc :embedding (%ds-vec 8 1.0) :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (multiple-value-bind (start end)
               (graph-db:clock-lease-epochs clock 10000)
             (let ((sg (graph-db:open-shadow-graph
                        shadow-location :detach-store-1
                        :lease (cons start end)
                        :expected-vectors 10000)))
               (unwind-protect
                    (let ((seg (gethash (cons 'ds-vec-doc 'embedding)
                                        (graph-db::vector-segments sg))))
                      (is (not (null seg))
                          "the ds-vec-doc/embedding segment was not carried ~
                           into the open shadow")
                      (is (>= (graph-db::segment-capacity seg) 10000))
                      (let ((cap-before (graph-db::segment-capacity seg)))
                        (with-transaction ((graph-db::transaction-manager sg))
                          (dotimes (i 500)
                            (make-ds-vec-doc
                             :embedding (%ds-vec 8 (float i 1.0)) :graph sg)))
                        (is (= cap-before (graph-db::segment-capacity seg))
                            "a burst within the presized headroom must not ~
                             grow the shadow's segment again (before ~D, ~
                             after ~D)"
                            cap-before (graph-db::segment-capacity seg))))
                 (let ((graph-db:*graph* sg))
                   (ignore-errors (close-graph sg :snapshot-p nil))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test open-shadow-graph-expected-vectors-is-a-no-op-with-no-segments
  "A shadow whose graph declares :VECTOR-INDEX nowhere used (only :GENERIC
nodes) has an EMPTY vector-segments table.  :EXPECTED-VECTORS must be a
clean no-op there, not an error -- OPEN-SHADOW-GRAPH must still return
normally with a usable graph."
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
                        :lease (cons start end)
                        :expected-vectors 10000)))
               (unwind-protect
                    (progn
                      (is (= 0 (hash-table-count
                                (graph-db::vector-segments sg))))
                      (is (= 1 (length (graph-db:map-vertices
                                        #'identity sg :collect-p t)))))
                 (let ((graph-db:*graph* sg))
                   (ignore-errors (close-graph sg :snapshot-p nil))))))
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
                (is (eq :shadow-load
                        (graph-db:store-not-accepting-reason c))))))
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
                    "file content changed beyond the measured ~
exclusion list"))))
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

(test recovery-policy-mismatch-warns-but-file-wins
  "Fix round 1 (GH #170 Task 4 review): OPEN-GRAPH's :RECOVERY-POLICY is
only a HINT once policy.dat exists.  MAKE-GRAPH a store :DERIVABLE,
close it, then OPEN-GRAPH the same location with a disagreeing
:AUTHORED: RECOVERY-POLICY-MISMATCH-WARNING must fire (caught here via
HANDLER-BIND + MUFFLE-WARNING rather than FIVEAM:SIGNALS, because
SIGNALS' HANDLER-CASE would unwind OUT of OPEN-GRAPH mid-call instead of
letting it finish and return the graph), and the effective policy stays
the FILE's :DERIVABLE -- STORE-RECOVERY-POLICY reads :DERIVABLE
afterward, not the disagreeing :AUTHORED that was passed.  There is no
further graph-side observable: the graph object caches no policy slot
of its own -- OPEN-SHADOW-GRAPH re-reads policy.dat fresh on every
:FAST-LOAD call -- so the file check above IS the behavioral check.
ABLATION (recorded, not re-run here): drop the WARN call in OPEN-GRAPH
and FIRED stays NIL."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let ((g (make-graph :detach-store-1 (namestring dir)
                             :buffer-pool-size 1000
                             :recovery-policy :derivable)))
          (let ((graph-db:*graph* g))
            (close-graph g :snapshot-p nil)))
        (let (g2 fired)
          (unwind-protect
               (progn
                 (handler-bind
                     ((graph-db:recovery-policy-mismatch-warning
                        (lambda (c) (setq fired c) (muffle-warning c))))
                   (setq g2 (open-graph :detach-store-1 (namestring dir)
                                        :buffer-pool-size 1000
                                        :recovery-policy :authored)))
                 (is-true fired
                          "OPEN-GRAPH must warn on a disagreeing ~
:RECOVERY-POLICY")
                 (is (eq :derivable
                         (graph-db:store-recovery-policy (namestring dir)))
                     "the FILE's policy must remain in effect, not the ~
disagreeing keyword")
                 (is-true (graph-db::graph-open-p g2)))
            (when g2
              (let ((graph-db:*graph* g2))
                (ignore-errors (close-graph g2 :snapshot-p nil))))))))))

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

;;; Whole-branch review fixes (GH #170, 2026-08-22).

(test detach-drain-timeout-restores-read-only-not-t
  "C1 (GH #170 review): a drain timeout during SHADOW-STORE's :READ-ONLY
window must restore ACCEPTING-P to :READ-ONLY, not hardcoded T --
hardcoded T would silently lift the shadow-window guard and let a write
land in the doomed generation.  Mechanism: shadow the store (G2 comes
back :READ-ONLY), open+close a shadow so SWAP-IN-SHADOW's precondition
is satisfiable, hold a read pin on G2 across a SWAP-IN-SHADOW call with
a 1-second timeout so its quiesce cannot drain, and confirm G2 is
STILL :READ-ONLY afterward -- a write still signals reason
:SHADOW-LOAD, not fully open again.  ABLATION (recorded, not re-run
here): reverting %QUIESCE-TRANSACTION-MANAGER's timeout branch to
\(%SET-ACCEPTING-P TRANSACTION-MANAGER T) makes the :READ-ONLY assertion
below fail."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (let ((tm2 (graph-db::transaction-manager g2)))
             (multiple-value-bind (start end)
                 (graph-db:clock-lease-epochs clock 1000)
               (let ((sg (graph-db:open-shadow-graph
                          shadow-location :detach-store-1
                          :lease (cons start end))))
                 (let ((graph-db:*graph* sg))
                   (close-graph sg :snapshot-p nil))))
             (let ((pin (graph-db:pin-read-epoch tm2)))
               (unwind-protect
                    (progn
                      (signals graph-db:detach-drain-timeout
                        (graph-db:swap-in-shadow
                         g2 shadow-location :timeout 1))
                      (is (eq :read-only (graph-db:accepting-p tm2))))
                 (graph-db:unpin-read-epoch tm2 pin)))
             (handler-case
                 (progn
                   (with-transaction (tm2)
                     (graph-db:make-vertex :generic nil :graph g2))
                   (fail "write must still be refused after the timeout"))
               (graph-db:store-not-accepting-error (c)
                 (is (eq :shadow-load
                         (graph-db:store-not-accepting-reason c)))))
             (graph-db:abandon-shadow g2 shadow-location)
             (with-transaction (tm2)
               (graph-db:make-vertex :generic nil :graph g2)))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test detach-refuses-replicated-graphs
  "I1 (GH #170 review): DETACH-STORE / SHADOW-STORE / SWAP-IN-SHADOW all
refuse a MASTER-GRAPH/SLAVE-GRAPH/PEER-GRAPH with
DETACH-UNSUPPORTED-GRAPH-ERROR -- their reopen paths use OPEN-GRAPH's
plain default arguments, which would silently strip replication/peer
configuration.  A bare MAKE-INSTANCE shell is enough: the check is a
TYPEP on the graph object made before anything else runs, so it never
touches any slot the shell leaves unbound."
  (dolist (g (list (make-instance 'graph-db::master-graph
                                  :graph-name :fake-master)
                   (make-instance 'graph-db::slave-graph
                                  :graph-name :fake-slave)
                   (make-instance 'graph-db::peer-graph
                                  :graph-name :fake-peer)))
    (signals graph-db:detach-unsupported-graph-error
      (graph-db:detach-store g))
    (signals graph-db:detach-unsupported-graph-error
      (graph-db:shadow-store g))
    (signals graph-db:detach-unsupported-graph-error
      (graph-db:swap-in-shadow g "/nonexistent-shadow/"))))

(test shadow-store-clears-stale-shadow-directory
  "I2 (GH #170 review): a pre-existing <location>-shadow/ from a
previous, never-cleaned SHADOW-STORE must be DISCARDED before the copy,
not merged with -- %COPY-DIRECTORY-TREE's per-file :SUPERSEDE never
clears the destination, so a stale .txn file left over there would be
replayed into the fresh shadow on its next open.  Pre-create the shadow
dir with a junk file, run SHADOW-STORE, and confirm the junk file is
gone."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let* ((shadow-dir (graph-db::%shadow-location (graph-db:location g)))
           (junk (merge-pathnames "junk.txt" shadow-dir)))
      (ensure-directories-exist shadow-dir)
      (with-open-file (out junk :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
        (write-string "stale" out))
      (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
        (unwind-protect
             (progn
               (is (not (probe-file junk)))
               (is-true (probe-file shadow-location)))
          (let ((graph-db:*graph* g2))
            (ignore-errors (close-graph g2 :snapshot-p nil))))))))

(test detach-store-failure-between-quiesce-and-close-restores-service
  "I3 (GH #170 review): a failure AFTER a successful quiesce but BEFORE
the durable close (CLOCK-LEASE-EPOCHS / JOURNAL-APPEND) must not strand
ACCEPTING-P at :DETACHING.  Mechanism: swap G's system clock for a mock
clock (same %MAKE-SYSTEM-CLOCK-with-invalid-location technique as
SWAP-IN-SHADOW-1-PROGRESS-SURVIVES-A-JOURNAL-APPEND-FAILURE) whose
%CLOCK-RESERVE write fails deterministically, so DETACH-STORE's
CLOCK-LEASE-EPOCHS call fails after the quiesce has already succeeded.
DETACH-STORE must re-signal AND leave the store fully accepting again,
still open."
  (with-clocked-store (g clock sys)
    clock sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (let ((tm (graph-db::transaction-manager g))
          (mock-clock (graph-db::%make-system-clock
                       :location "/nonexistent-dir-for-gh-170-i3/"
                       :counter 1)))
      (setf (graph-db::%graph-system-clock g) mock-clock)
      (unwind-protect
           (signals error (graph-db:detach-store g))
        ;; Restore the real clock so the fixture's own teardown can close
        ;; G normally regardless of this test's outcome.
        (setf (graph-db::%graph-system-clock g) clock))
      (is (eq t (graph-db:accepting-p tm)))
      (is (graph-db::graph-open-p g))
      (with-transaction (tm)
        (graph-db:make-vertex :generic nil :graph g)))))

(test open-graph-initial-accepting-state-refuses-write-immediately
  "I4 (GH #170 review): OPEN-GRAPH's :INITIAL-ACCEPTING-STATE sets the
fresh transaction manager's ACCEPTING-P at construction, before the
graph is registered in *GRAPHS* -- not published writable then flipped
after, which would leave a window for a racing writer to land a commit
in a generation meant to come up non-accepting.  A true race is not
practical to test at the unit level; the ordering itself is the proof
(the transaction manager did not even exist yet at registration time
under the pre-fix code path).  This checks the observable contract
instead: a graph opened with :INITIAL-ACCEPTING-STATE :READ-ONLY
refuses a write the INSTANT OPEN-GRAPH returns."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let ((g (make-graph :detach-store-1 (namestring dir)
                             :buffer-pool-size 1000)))
          (let ((graph-db:*graph* g))
            (close-graph g :snapshot-p nil)))
        (let ((g2 (open-graph :detach-store-1 (namestring dir)
                              :buffer-pool-size 1000
                              :initial-accepting-state :read-only)))
          (unwind-protect
               (progn
                 (is (eq :read-only
                         (graph-db:accepting-p
                          (graph-db::transaction-manager g2))))
                 (handler-case
                     (progn
                       (with-transaction
                           ((graph-db::transaction-manager g2))
                         (graph-db:make-vertex :generic nil :graph g2))
                       (fail "write must be refused immediately"))
                   (graph-db:store-not-accepting-error (c)
                     (is (eq :shadow-load
                             (graph-db:store-not-accepting-reason c))))))
            (let ((graph-db:*graph* g2))
              (ignore-errors (close-graph g2 :snapshot-p nil)))))))))

(test open-shadow-graph-failed-open-closes-cleanly-for-retry
  "M1 (GH #170 review): an error AFTER OPEN-GRAPH inside OPEN-SHADOW-
GRAPH (lease exhaustion here) must not leak the opened graph -- an mmap
left open plus a set .dirty marker would make a RETRY open fail on \"not
closed properly\" instead of just re-raising the exhaustion.  Exhaust a
2-epoch lease, close, confirm EPOCH-LEASE-EXHAUSTED at open (same shape
as SHADOW-LEASE-CONSUMED-EXACTLY-TO-ITS-BOUNDARY-EXHAUSTS-AT-OPEN), then
re-open the SAME shadow location with a FRESH lease and confirm it
succeeds cleanly (no .dirty refusal), with the data intact."
  (with-clocked-store (g clock sys)
    sys
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db:make-vertex :generic nil :graph g))
    (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
      (unwind-protect
           (progn
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
             (multiple-value-bind (start2 end2)
                 (graph-db:clock-lease-epochs clock 1000)
               (let ((sg2 (graph-db:open-shadow-graph
                          shadow-location :detach-store-1
                          :lease (cons start2 end2))))
                 (unwind-protect
                      (is (= 3 (length (graph-db:map-vertices
                                       #'identity sg2 :collect-p t))))
                   (let ((graph-db:*graph* sg2))
                     (ignore-errors (close-graph sg2 :snapshot-p nil)))))))
        (let ((graph-db:*graph* g2))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test swap-in-shadow-deletes-the-promoted-lease-file
  "M2 (GH #170 review): the promoted generation's lease.dat (copied in
from the shadow) is deleted right after the second rename -- it has no
meaning once the location is a plain, non-shadow live store."
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
      (multiple-value-bind (new-graph retired-path)
          (graph-db:swap-in-shadow g2 shadow-location)
        (is (not (probe-file
                  (merge-pathnames
                   "lease.dat"
                   (uiop:ensure-directory-pathname
                    (graph-db::location new-graph))))))
        (let ((graph-db:*graph* new-graph))
          (ignore-errors (close-graph new-graph :snapshot-p nil)))
        (ignore-errors
         (uiop:delete-directory-tree
          (uiop:ensure-directory-pathname retired-path)
          :validate t :if-does-not-exist :ignore))))))

;;; Round 2 (GH #170 re-review): I4's TM-before-registration reorder
;;; introduced a NEW critical -- the fresh TX-ID-COUNTER is seeded from
;;; LOAD-HIGHEST-TRANSACTION-ID before RECOVER-TRANSACTIONS runs, but
;;; recovery's APPLY-TRANSACTION persists a HIGHER watermark to disk
;;; without ever touching TX-ID-COUNTER.  Engine-wide (any plain,
;;; non-clocked store with a crash-recovery WAL tail), and invisible to
;;; the gate before this: there was no crash-recovery test at all.

(test crash-recovery-reseeds-the-tx-id-watermark
  "A plain (non-clocked) on-disk graph: commit two transactions, the
second with *DELETE-COMMITTED-TRANSACTION-FILES* NIL so its .txn file
survives as .committed instead of being deleted, then close cleanly.
Fabricate a crash: rename the survived .committed file back to .txn (a
WAL tail RECOVER-TRANSACTIONS will replay -- idempotent via
*ADD-TO-INDEXES-UNLESS-PRESENT-P*, the flag documented for exactly this
case), and REWIND transaction-id.dat (via the same
PERSIST-HIGHEST-TRANSACTION-ID writer) to the FIRST transaction's id --
simulating a crash between FINALIZE-TX-PERSISTENCE (durable .txn) and
APPLY-TRANSACTION (which would have advanced the persisted watermark).
Plant then clear .dirty, mirroring RECOVERY-FROM-DIRTY-MARKER, and
reopen (recovery runs). A transaction committed after that open must
mint an id STRICTLY GREATER than the replayed transaction's id, not
just past the stale pre-recovery watermark.

Also asserts the re-opened graph's REPLICATION-LOG-FILE name (parsed via
the same PARSE-REPLICATION-LOG-NAME the replication streaming code uses)
encodes the RE-SEEDED counter, not the rewound one -- round 3's fix: the
constructor's INITIALIZE-INSTANCE :AFTER derives that filename from the
SAME (then pre-recovery) counter value TX-ID-COUNTER was seeded from, so
re-seeding TX-ID-COUNTER alone would still leave the file's name
advertising a stale, lower minimum id than it will actually hold.

ABLATION (recorded, not re-run here): removing OPEN-GRAPH's re-seed
after RECOVER-TRANSACTIONS makes the final (> new-id id-b) assertion
fail -- the new transaction mints ID-A+1, which is <= ID-B.  ABLATION 2
(recorded, not re-run here): keeping the TX-ID-COUNTER re-seed but
dropping the REPLICATION-LOG-FILE re-derivation makes the filename
assertion fail -- it would still encode ID-A+1."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil)
            (path (namestring dir))
            id-a id-b)
        (let ((g (make-graph :gh-170-round2-recovery path
                             :buffer-pool-size 1000))
              va vb)
          ;; COMMIT-EPOCH is stamped at the END of the WITH-TRANSACTION
          ;; body, not when the node is created -- read it only AFTER the
          ;; block (same idiom as SHADOW-LEASE-RESUMES-FROM-ITS-OWN-
          ;; WATERMARK above), or it is still the pre-commit default 0.
          (with-transaction ((graph-db::transaction-manager g))
            (setq va (graph-db:make-vertex :generic nil :graph g)))
          (setq id-a (graph-db::commit-epoch va))
          (let ((graph-db::*delete-committed-transaction-files* nil))
            (with-transaction ((graph-db::transaction-manager g))
              (setq vb (graph-db:make-vertex :generic nil :graph g)))
            (setq id-b (graph-db::commit-epoch vb)))
          (let ((graph-db:*graph* g))
            (close-graph g :snapshot-p nil))
          ;; Fabricate the crash shape.
          (let* ((tx-dir (graph-db::persistent-transaction-directory g))
                 (committed-files
                  (directory (merge-pathnames "*.committed" tx-dir)))
                 (dirty (format nil "~A/.dirty" path)))
            (is (= 1 (length committed-files))
                "sanity: exactly the second commit's .txn survived as ~
.committed")
            (rename-file (first committed-files)
                         (make-pathname :type "txn"
                                        :defaults (first committed-files)))
            ;; Raw writer: PERSIST-HIGHEST-TRANSACTION-ID is monotonic
            ;; now (GH #177), and this fabricates a crashed, rewound
            ;; watermark on purpose.
            (graph-db::%write-highest-transaction-id id-a g)
            (is (= id-a (graph-db::load-highest-transaction-id g))
                "sanity: the watermark file is now stale")
            (with-open-file (out dirty :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
              (format out "~S" (get-universal-time)))
            (delete-file dirty)))
        (let ((g2 (open-graph :gh-170-round2-recovery path)))
          (unwind-protect
               (let (id-c vc)
                 (is (= (1+ id-b)
                        (graph-db::parse-replication-log-name
                         (graph-db::replication-log-file
                          (graph-db::transaction-manager g2))))
                     "the reopened graph's replication-log-file must ~
encode the RE-SEEDED counter (~D), not the rewound one" (1+ id-b))
                 (with-transaction ((graph-db::transaction-manager g2))
                   (setq vc (graph-db:make-vertex :generic nil :graph g2)))
                 (setq id-c (graph-db::commit-epoch vc))
                 (is (> id-c id-b)
                     "a post-recovery transaction must mint an id past ~
every replayed id, not just the pre-recovery watermark (got ~D, ~
replayed id was ~D)" id-c id-b))
            (let ((graph-db:*graph* g2))
              (ignore-errors (close-graph g2 :snapshot-p nil)))))))))

;;; Round 3 (GH #170 re-review): the publication window.  *GRAPHS*/
;;; %REGISTER-OPEN-STORE registration happens BEFORE RECOVER-TRANSACTIONS
;;; and every rebuild step in OPEN-GRAPH; a racing name-lookup writer
;;; could otherwise pin AND commit against a mid-recovery graph.  Fixed
;;; by starting every fresh transaction manager at :OPENING (admits
;;; pins, refuses new transactions with reason :OPENING) and flipping to
;;; the caller's requested :INITIAL-ACCEPTING-STATE only at the very end
;;; of OPEN-GRAPH, once GRAPH-OPEN-P is T.

(test open-graph-ends-fully-accepting-and-opening-refuses-transactions
  "Two-part unit probe (a true concurrent race against OPEN-GRAPH's own
internal window is disproportionate to test directly -- see the source-
order comment below instead, which is the actual proof):

1. An ordinary OPEN-GRAPH call ends with ACCEPTING-P T -- the :OPENING
   window is entirely internal to OPEN-GRAPH and never observable by a
   caller holding the returned graph.
2. CREATE-TRANSACTION against a transaction manager hand-set to
   :OPENING (the exact state every fresh TM starts OPEN-GRAPH in) signals
   STORE-NOT-ACCEPTING-ERROR reason :OPENING -- the same generic
   REASON-mirrors-the-flag behavior :DETACHING/:SWAPPING already get.
3. PIN-READ-EPOCH under that SAME :OPENING state succeeds -- OPEN-
   GRAPH's own rebuild/recovery scans run under WITH-READ-PIN and must
   not be refused by the very state meant to protect them.

The ORDERING PIN (the actual guarantee, not re-tested by a race here):
in OPEN-GRAPH's source (graph.lisp), the TRANSACTION-MANAGER is
constructed with :ACCEPTING-P :OPENING BEFORE the (SETF (GETHASH NAME
*GRAPHS*) GRAPH) / %REGISTER-OPEN-STORE call, and %SET-ACCEPTING-P to
INITIAL-ACCEPTING-STATE is the LAST form before OPEN-GRAPH returns,
after (SETF (GRAPH-OPEN-P GRAPH) T) -- so no window exists where the
graph is both externally reachable (via *GRAPHS*/LOOKUP-GRAPH) and
willing to accept a new transaction before every rebuild/recovery step
has completed."
  (with-clocked-store (g clock sys)
    clock sys
    (is (eq t (graph-db:accepting-p (graph-db::transaction-manager g))))
    (let ((tm (graph-db::transaction-manager g)))
      (graph-db::%set-accepting-p tm :opening)
      (unwind-protect
           (progn
             (handler-case
                 (progn
                   (with-transaction (tm)
                     (graph-db:make-vertex :generic nil :graph g))
                   (fail "a transaction under :OPENING must be refused"))
               (graph-db:store-not-accepting-error (c)
                 (is (eq :opening (graph-db:store-not-accepting-reason c)))))
             (let ((pin (graph-db:pin-read-epoch tm)))
               (graph-db:unpin-read-epoch tm pin)))
        (graph-db::%set-accepting-p tm t)))))

;;; Clock/journal hygiene batch (GH #177, #212).

(test persist-highest-transaction-id-is-monotonic
  "GH #177: PERSIST-HIGHEST-TRANSACTION-ID only moves the durable
watermark FORWARD -- a racing lower id must not clobber a higher one --
and returns whatever is on disk after the call.  The raw
%WRITE-HIGHEST-TRANSACTION-ID stays available for tests that fabricate
crash states (see CRASH-RECOVERY-RESEEDS-THE-TX-ID-WATERMARK)."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let ((g (make-graph :gh-177-monotonic (namestring dir)
                             :buffer-pool-size 1000)))
          (unwind-protect
               (progn
                 (is (= 100 (graph-db::persist-highest-transaction-id
                             100 g)))
                 (is (= 100 (graph-db::persist-highest-transaction-id
                             50 g))
                     "a lower id returns the standing watermark")
                 (is (= 100 (load-highest-transaction-id g))
                     "and must not have rewound the file")
                 (is (= 150 (graph-db::persist-highest-transaction-id
                             150 g)))
                 (is (= 150 (load-highest-transaction-id g)))
                 ;; The raw writer is the deliberate escape hatch --
                 ;; and it must rewind the cache along with the file,
                 ;; or the next persist would answer from a watermark
                 ;; the rewind erased (GH #237).
                 (graph-db::%write-highest-transaction-id 10 g)
                 (is (= 10 (load-highest-transaction-id g)))
                 (is (= 20 (graph-db::persist-highest-transaction-id
                            20 g))
                     "persist after a raw rewind must see the rewound ~
watermark, not a stale cached 150")
                 (is (= 20 (load-highest-transaction-id g))))
            (let ((graph-db:*graph* g))
              (ignore-errors (close-graph g :snapshot-p nil)))))))))

(test watermark-fast-path-touches-no-file
  "GH #237: once the cache holds a watermark >= the id, PERSIST-HIGHEST-
TRANSACTION-ID answers from the cache with no lock and no I/O.  Proof:
delete transaction-id.dat after seeding -- a lower-id persist must
still answer correctly WITHOUT recreating the file."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil))
        (let ((g (make-graph :gh-237-fastpath (namestring dir)
                             :buffer-pool-size 1000)))
          (unwind-protect
               (let ((file (graph-db::highest-transaction-id-file g)))
                 (is (= 100 (graph-db::persist-highest-transaction-id
                             100 g)))
                 (delete-file file)
                 (is (= 100 (graph-db::persist-highest-transaction-id
                             50 g))
                     "fast path answers from the cache")
                 (is (null (probe-file file))
                     "and performed no file I/O at all")
                 ;; A higher id takes the slow path and writes again.
                 (is (= 150 (graph-db::persist-highest-transaction-id
                             150 g)))
                 (is (= 150 (load-highest-transaction-id g))))
            (let ((graph-db:*graph* g))
              (ignore-errors (close-graph g :snapshot-p nil)))))))))

(test watermark-cache-seeds-from-disk-after-reopen
  "GH #237: a fresh graph object has an unknown cache; the first
persist seeds it from transaction-id.dat, so monotonicity holds
across close/open exactly as it did against the bare file."
  (with-temp-directory (sys)
    (with-temp-directory (dir)
      (let ((graph-db::*system-directory* (namestring sys))
            (graph-db::*store-registry* nil)
            (path (namestring dir)))
        (let ((g (make-graph :gh-237-reseed path
                             :buffer-pool-size 1000)))
          (graph-db::persist-highest-transaction-id 100 g)
          (let ((graph-db:*graph* g))
            (close-graph g :snapshot-p nil)))
        (let ((g2 (open-graph :gh-237-reseed path)))
          (unwind-protect
               (let ((base (load-highest-transaction-id g2)))
                 (is (>= base 100)
                     "the durable watermark survived the reopen")
                 (is (= base (graph-db::persist-highest-transaction-id
                              1 g2))
                     "a low persist on the fresh object seeds from ~
disk and refuses to rewind")
                 (is (= base (load-highest-transaction-id g2)))
                 (is (= (+ base 50)
                        (graph-db::persist-highest-transaction-id
                         (+ base 50) g2))))
            (let ((graph-db:*graph* g2))
              (ignore-errors (close-graph g2 :snapshot-p nil)))))))))

(test watermarks-are-per-graph
  "GH #237: each graph carries its own watermark lock and cache --
interleaved persists on two graphs never observe each other."
  (with-temp-directory (sys)
    (with-temp-directory (dir-a)
      (with-temp-directory (dir-b)
        (let ((graph-db::*system-directory* (namestring sys))
              (graph-db::*store-registry* nil))
          (let ((ga (make-graph :gh-237-per-graph-a (namestring dir-a)
                                :buffer-pool-size 1000))
                (gb (make-graph :gh-237-per-graph-b (namestring dir-b)
                                :buffer-pool-size 1000)))
            (unwind-protect
                 (progn
                   (is (not (eq (graph-db::watermark-lock ga)
                                (graph-db::watermark-lock gb)))
                       "no shared lock between graphs")
                   (is (= 100 (graph-db::persist-highest-transaction-id
                               100 ga)))
                   (is (= 5 (graph-db::persist-highest-transaction-id
                             5 gb)))
                   (is (= 100 (graph-db::persist-highest-transaction-id
                               50 ga))
                       "graph A's watermark unmoved by graph B's")
                   (is (= 200 (graph-db::persist-highest-transaction-id
                               200 gb)))
                   (is (= 100 (load-highest-transaction-id ga)))
                   (is (= 200 (load-highest-transaction-id gb))))
              (let ((graph-db:*graph* gb))
                (ignore-errors (close-graph gb :snapshot-p nil)))
              (let ((graph-db:*graph* ga))
                (ignore-errors (close-graph ga :snapshot-p nil))))))))))

(defun %mock-clock-failing-ceiling (journal-file)
  "A real SYSTEM-CLOCK struct whose JOURNAL-APPEND works (the journal
stream is pre-opened onto JOURNAL-FILE) but whose ceiling write fails
deterministically (LOCATION does not exist) -- so ATTACH-TO-SYSTEM-
CLOCK's CLOCK-OBSERVE-EPOCH is the first thing to die, AFTER any
:SWAP journal record and AFTER OPEN-GRAPH (GH #212)."
  (graph-db::%make-system-clock
   :location "/nonexistent-dir-for-gh-212-attach/"
   :counter 0
   :journal (open journal-file :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)))

(test swap-in-shadow-1-attach-failure-leaves-the-store-openable
  "GH #212: in %SWAP-IN-SHADOW-1, an ATTACH-TO-SYSTEM-CLOCK failure
AFTER its OPEN-GRAPH succeeded used to leave the new generation open
(registered, mmapped, .dirty on disk), so the recovery handler's
%REOPEN-AND-RESUME deterministically died on the .dirty marker.  Now
the just-opened graph is closed before the error propagates: nothing
stays registered, no .dirty remains, and the live location reopens
cleanly."
  (with-clocked-store (g clock sys)
    clock sys
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
               (with-temp-directory (jdir)
                 (let ((mock (%mock-clock-failing-ceiling
                              (merge-pathnames "mock-journal.log" jdir)))
                       (progress (vector nil nil)))
                   (unwind-protect
                        (progn
                          (signals error
                            (graph-db::%swap-in-shadow-1
                             name location shadow-dir mock progress))
                          (is-true (aref progress 0)
                                   "both renames landed before the attach")
                          (setq retired (aref progress 1))
                          (is-false (graph-db:lookup-graph name)
                                    "the failed generation must not stay ~
registered")
                          (is-false
                           (probe-file
                            (merge-pathnames
                             ".dirty"
                             (uiop:ensure-directory-pathname location)))
                           "no .dirty may survive the attach failure")
                          ;; The whole point: the live dir opens again.
                          (let ((g2 (open-graph
                                     name
                                     (namestring
                                      (uiop:ensure-directory-pathname
                                       location))
                                     :buffer-pool-size 1000
                                     :system-clock nil)))
                            (let ((graph-db:*graph* g2))
                              (close-graph g2 :snapshot-p nil))))
                     ;; No handle leak: the mock's pre-opened journal
                     ;; stream must not outlive the temp dir.
                     (ignore-errors
                      (close (graph-db::system-clock-journal mock))))))))
        (when (and retired (probe-file retired))
          (ignore-errors
           (uiop:delete-directory-tree
            (uiop:ensure-directory-pathname retired)
            :validate t :if-does-not-exist :ignore)))))))

(test reopen-and-resume-attach-failure-closes-the-reopened-graph
  "GH #212, the recovery helper itself: %REOPEN-AND-RESUME's attach
failing after its OPEN-GRAPH succeeded must close the reopened graph
before propagating, so the caller's next recovery attempt (or a manual
OPEN-GRAPH) is not refused on .dirty."
  (with-clocked-store (g clock sys)
    clock sys
    (let ((name (graph-db:graph-name g))
          (location (graph-db::location g)))
      (let ((graph-db:*graph* g))
        (close-graph g :snapshot-p nil))
      (with-temp-directory (jdir)
        (let ((mock (%mock-clock-failing-ceiling
                     (merge-pathnames "mock-journal.log" jdir))))
          (unwind-protect
               (progn
                 (signals error
                   (graph-db::%reopen-and-resume name location mock t))
                 (is-false (graph-db:lookup-graph name))
                 (is-false (probe-file
                            (merge-pathnames
                             ".dirty"
                             (uiop:ensure-directory-pathname location))))
                 (let ((g2 (open-graph
                            name
                            (namestring (uiop:ensure-directory-pathname
                                         location))
                            :buffer-pool-size 1000
                            :system-clock nil)))
                   (let ((graph-db:*graph* g2))
                     (close-graph g2 :snapshot-p nil))))
            ;; No handle leak (see the swap test above).
            (ignore-errors
             (close (graph-db::system-clock-journal mock)))))))))
