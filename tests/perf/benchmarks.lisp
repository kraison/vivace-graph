;;;; Performance benchmarks (SBCL is the target).  Each records into
;;;; *perf-report* via record / record-throughput; run-perf runs them all and
;;;; writes a report file.

(in-package #:graph-db/perf-test)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun insert-p-nodes (n &key (batch 1000))
  "Insert N p-nodes in BATCH-sized transactions; return a vector of their ids."
  (let ((ids (make-array n)) (i 0))
    (loop while (< i n) do
      (with-transaction ()
        (dotimes (k (min batch (- n i)))
          (setf (aref ids i) (id (make-p-node :val i :label "x")))
          (incf i))))
    ids))

(defun count-vertices (g)
  (let ((c 0))
    (map-vertices (lambda (v) (declare (ignore v)) (incf c)) g :vertex-type 'p-node)
    c))

(defun count-edges (g)
  (let ((c 0))
    (map-edges (lambda (e) (declare (ignore e)) (incf c)) g :edge-type 'p-knows)
    c))

;;; ---------------------------------------------------------------------------
;;; Benchmarks
;;; ---------------------------------------------------------------------------

(defun bench-crud ()
  "insert / lookup / scan / update / delete throughput on one vertex set."
  (let ((n (scale 20000)) (batch 1000))
    (with-perf-graph (g)
      (let (ids)
        (timed-ops ("insert-vertices" n)
          (setf ids (insert-p-nodes n :batch batch)))
        (timed-ops ("lookup-by-id" n)
          (loop for id across ids do (lookup-vertex id)))
        (timed-ops ("scan-vertices" (count-vertices g))
          (count-vertices g))
        (timed-ops ("update-vertices" n)
          (let ((j 0))
            (loop while (< j n) do
              (with-transaction ()
                (dotimes (k (min batch (- n j)))
                  (let ((v (copy (lookup-vertex (aref ids j)))))
                    (setf (slot-value v 'label) "y")
                    (save v))
                  (incf j))))))
        (let ((d (floor n 2)))
          (timed-ops ("delete-vertices" d)
            (let ((j 0))
              (loop while (< j d) do
                (with-transaction ()
                  (dotimes (k (min batch (- d j)))
                    (mark-deleted (lookup-vertex (aref ids j)))
                    (incf j)))))))))))

(defun bench-edges ()
  "edge insertion + edge scan throughput."
  (let ((v (scale 5000)) (e (scale 20000)) (batch 1000))
    (with-perf-graph (g)
      (let ((ids (insert-p-nodes v :batch batch)))
        (timed-ops ("insert-edges" e)
          (let ((j 0))
            (loop while (< j e) do
              (with-transaction ()
                (dotimes (k (min batch (- e j)))
                  (let ((a (lookup-vertex (aref ids (mod j v))))
                        (b (lookup-vertex (aref ids (mod (1+ j) v)))))
                    (make-p-knows :from a :to b))
                  (incf j))))))
        (timed-ops ("scan-edges" (count-edges g))
          (count-edges g))))))

(defun bench-view ()
  "view lookup (invoke-graph-view :key) throughput."
  (let ((n (scale 20000)) (batch 1000) (q (scale 20000)))
    (with-perf-graph (g :views t)
      (insert-p-nodes n :batch batch)
      (timed-ops ("view-lookup" q)
        (dotimes (i q) (invoke-graph-view 'p-node 'p-node-by-val :key (mod i n)))))))

(defun bench-unique ()
  "insert throughput WITH a :unique constraint (commit-boundary enforcement) vs the
same inserts with no constraint -- the delta is the enforcement overhead."
  (let ((n (scale 20000)) (batch 1000))
    (with-perf-graph (g)
      (timed-ops ("unique-insert" n)
        (let ((i 0))
          (loop while (< i n) do
            (with-transaction ()
              (dotimes (k (min batch (- n i)))
                (make-pu-node :uval i :label "u") (incf i)))))))
    (with-perf-graph (g)
      (timed-ops ("unique-baseline-plain-insert" n)
        (let ((i 0))
          (loop while (< i n) do
            (with-transaction ()
              (dotimes (k (min batch (- n i)))
                (make-p-node :val i :label "u") (incf i)))))))))

(defun bench-index ()
  "general ordered index: indexed-insert throughput (maintenance overhead),
index-lookup (equality) throughput, index-range scan throughput, and index-lookup
vs a full type scan for the same equality query."
  (let ((n (scale 20000)) (batch 1000) (q (scale 20000)))
    (with-perf-graph (g)
      (timed-ops ("indexed-insert" n)
        (let ((i 0))
          (loop while (< i n) do
            (with-transaction ()
              (dotimes (k (min batch (- n i)))
                (make-pi-node :ival i :label "x") (incf i))))))
      (timed-ops ("index-lookup-eq" q)
        (dotimes (i q) (index-lookup g 'pi-node 'ival (mod i n))))
      ;; range scans covering ~1% of the key space each
      (let ((span (max 1 (floor n 100))) (r (scale 2000)))
        (timed-ops ("index-range-1pct" r)
          (dotimes (i r)
            (let ((s (mod (* i span) n)))
              (index-range g 'pi-node 'ival :start s :end (+ s span))))))
      ;; full-scan equality for comparison (O(n) each) -- far fewer iterations
      (let ((fs (scale 200 20)))
        (timed-ops ("index-fullscan-eq" fs)
          (dotimes (i fs)
            (let ((target (mod i n)) (hit nil))
              (map-vertices (lambda (v)
                              (when (eql (slot-value v 'ival) target) (setq hit v)))
                            g :vertex-type 'pi-node)
              hit)))))))

(defun bench-prolog ()
  "prolog select throughput: type scan + edge join."
  (let ((n (scale 10000)) (batch 1000))
    (with-perf-graph (g)
      (let ((ids (insert-p-nodes n :batch batch)))
        ;; a chain of edges so the join has work
        (let ((e (floor n 2)) (j 0))
          (loop while (< j e) do
            (with-transaction ()
              (dotimes (k (min batch (- e j)))
                (make-p-knows :from (lookup-vertex (aref ids j))
                              :to (lookup-vertex (aref ids (mod (1+ j) n))))
                (incf j)))))
        (timed-ops ("prolog-is-a-scan" n)
          (select-flat (?x) (is-a ?x p-node)))
        (let ((cnt (count-edges g)))
          (timed-ops ("prolog-edge-join" cnt)
            (select-flat (?a) (p-knows ?a ?b))))))))

(defun bench-commit-overhead ()
  "Same op count, one big transaction vs one-op-per-transaction."
  (let ((n (scale 5000)))
    (with-perf-graph (g)
      (timed-ops ("commit-batched-1txn" n)
        (with-transaction () (dotimes (i n) (make-p-node :val i :label "b")))))
    (with-perf-graph (g)
      (timed-ops ("commit-per-op-Ntxn" n)
        (dotimes (i n) (with-transaction () (make-p-node :val i :label "p")))))))

(defun bench-concurrent-rw ()
  "T threads each doing M mixed insert+lookup ops on a shared graph."
  (let ((threads 8) (m (scale 4000)))
    (with-perf-graph (g)
      ;; seed some nodes to look up
      (let ((seed (insert-p-nodes (scale 5000) :batch 1000)))
        (timed-ops ("concurrent-rw" (* threads m))
          (let ((ts (loop repeat threads
                          collect (bordeaux-threads:make-thread
                                   (lambda ()
                                     (let ((*graph* g))
                                       (dotimes (i m)
                                         (if (evenp i)
                                             (with-transaction ()
                                               (make-p-node :val i :label "c"))
                                             (lookup-vertex
                                              (aref seed (mod i (length seed))))))))
                                   :name "perf-rw"))))
            (mapc #'bordeaux-threads:join-thread ts)))))))

(defun bench-disk-growth ()
  "Heap high-water (allocator USED bytes, not the preallocated file size) after N
inserts, then after updating each node once.  This is the key MVCC metric: today
updates free+reuse the old block (watermark ~flat); MVCC will retain old versions
(watermark rises).  heap.dat is the preallocated mmap region, so file size is
useless here -- we read (memory-pointer (heap g))."
  (let ((n (scale 20000)) (batch 1000))
    (with-perf-graph (g)
      (let ((ids (insert-p-nodes n :batch batch))
            (heap (graph-db::heap g)))
        (collect-garbage)
        (let ((used (graph-db::memory-pointer heap)))
          (record "heap-used-after-inserts" :bytes used :per-op (round used n)))
        (let ((j 0))
          (loop while (< j n) do
            (with-transaction ()
              (dotimes (k (min batch (- n j)))
                (let ((v (copy (lookup-vertex (aref ids j)))))
                  (setf (slot-value v 'label) "z")
                  (save v))
                (incf j)))))
        (collect-garbage)
        (let ((used (graph-db::memory-pointer heap)))
          (record "heap-used-after-updates" :bytes used :per-op (round used n)))))))

(defun bench-snapshot-restore-reopen ()
  "snapshot, replay (restore), and reopen wall-times on a populated graph."
  (let ((n (scale 20000)) (batch 1000))
    (with-temp-directory (d1)
      (with-temp-directory (d2)
        (let ((p1 (namestring d1)) (p2 (namestring d2)))
          (let ((g (make-graph *perf-graph-name* p1 :buffer-pool-size 4000)))
            (let ((*graph* g))
              (insert-p-nodes n :batch batch)
              (timed-seconds ("snapshot") (snapshot g)))
            (close-graph g :snapshot-p nil))
          ;; restore via replay into a fresh graph
          (let ((g2 (make-graph *perf-graph-name* p2 :buffer-pool-size 4000)))
            (unwind-protect
                 (let ((*graph* g2))
                   (timed-seconds ("restore-replay")
                     (replay g2 (merge-pathnames "txn-log/" d1) :graph-db/perf-test)))
              (close-graph g2 :snapshot-p nil)))
          ;; reopen the original
          (timed-seconds ("reopen")
            (let ((g3 (open-graph *perf-graph-name* p1)))
              (close-graph g3 :snapshot-p nil)))))))
  (collect-garbage))

(defun %run-multigraph-cell (n per-thread label)
  "One multigraph-commit cell: N graphs, one committing thread per
graph, PER-THREAD one-vertex txns each, spawned behind a start gate so
the timer excludes thread creation.  Asserts every worker's commits
landed, then records LABEL."
  (let ((graphs '()) (dirs '()))
    (unwind-protect
         (progn
           (dotimes (i n)
             (let ((d (make-temp-directory)))
               (push d dirs)
               ;; No :buffer-pool-size -- the pool is process-global
               ;; and already sized by the first bench that ran.
               (push (make-graph (intern (format nil "PERF-MG-~D" i)
                                         :keyword)
                                 (namestring d))
                     graphs)))
           ;; One warm-up commit per graph, in THIS thread: adopts
           ;; p-node into each store (needs *SYSTEM-DIRECTORY*,
           ;; thread-local) and keeps adoption out of the timing.
           (dolist (g graphs)
             (let ((*graph* g))
               (with-transaction ()
                 (make-p-node :val -1 :label "w" :graph g))))
           (let* ((gate (bordeaux-threads:make-semaphore))
                  (ts (mapcar
                       (lambda (g)
                         (bordeaux-threads:make-thread
                          (lambda ()
                            (bordeaux-threads:wait-on-semaphore gate)
                            (let ((*graph* g))
                              (dotimes (i per-thread)
                                (with-transaction ()
                                  (make-p-node :val i :label "m"
                                               :graph g)))))
                          :name "perf-mg-commit"))
                       graphs))
                  (start (get-internal-real-time)))
             (bordeaux-threads:signal-semaphore gate :count n)
             (mapc #'bordeaux-threads:join-thread ts)
             (let* ((elapsed (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second)))
                    (ops (* n per-thread)))
               (dolist (g graphs)
                 (let ((got (count-vertices g)))
                   (assert (= (1+ per-thread) got) ()
                           "~A: expected ~D vertices in ~A, counted ~D"
                           label (1+ per-thread) g got)))
               (record label
                       :ops ops :seconds (float-3 elapsed)
                       :ops/s (if (zerop elapsed)
                                  0
                                  (round (/ ops elapsed)))
                       :us/commit (float-3 (/ (* elapsed 1e6)
                                              per-thread))))))
      (dolist (g graphs) (ignore-errors (close-graph g :snapshot-p nil)))
      (collect-garbage)
      (dolist (d dirs)
        (uiop:delete-directory-tree d :validate t
                                    :if-does-not-exist :ignore)))))

(defun bench-multigraph-commit ()
  "Multi-graph commit contention (GH #237, #252): N graphs, one
committing thread per graph.  An nN cell measures EVERYTHING the
graphs share -- the global watermark lock, the process-global buffer
pool, GC, one filesystem -- so the n8:n1 ratio alone does not
implicate the lock.  The nN-rawwm control reruns the largest cell
with PERSIST-HIGHEST-TRANSACTION-ID swapped for the raw writer
(unconditional write, no global lock, no re-read): nN minus nN-rawwm
is the watermark lock+read's share.  The watermark-* pair prices one
call in isolation -- single-threaded, ascending ids, warm OS cache."
  (let* ((per-thread (scale 1000))
         (ns (if (eq *perf-scale* :small) '(1 4) '(1 2 4 8)))
         (n-max (car (last ns))))
    (dolist (n ns)
      (%run-multigraph-cell n per-thread
                            (format nil "commit-multigraph-n~D" n)))
    ;; Control cell: same load, watermark persist bypassed.
    (let ((orig (fdefinition 'graph-db::persist-highest-transaction-id)))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'graph-db::persist-highest-transaction-id)
                   (lambda (transaction-id graph)
                     (graph-db::%write-highest-transaction-id
                      transaction-id graph)))
             (%run-multigraph-cell
              n-max per-thread
              (format nil "commit-multigraph-n~D-rawwm" n-max)))
        (setf (fdefinition 'graph-db::persist-highest-transaction-id)
              orig)))
    ;; The watermark call in isolation (GH #237's concrete suspect).
    (let ((iters (scale 20000)))
      (with-perf-graph (g)
        (timed-ops ("watermark-persist-warm" iters)
          (loop for i from 1 to iters
                do (graph-db::persist-highest-transaction-id i g)))
        (timed-ops ("watermark-raw-write-warm" iters)
          (loop for i from 1 to iters
                do (graph-db::%write-highest-transaction-id i g)))))))

(defun %insert-v5-mix (e dead-pct batch)
  "Insert E p-knows edges; DEAD-PCT percent get a fresh never-created
v5 :to id (untagged -> the all-open-stores scan on emit, GH #244)."
  (let* ((v (min e 500))
         (ids (insert-p-nodes v :batch batch))
         (i 0))
    (loop while (< i e) do
      (with-transaction ()
        (dotimes (k (min batch (- e i)))
          (make-p-knows
           :from (lookup-vertex (aref ids (mod i v)))
           :to (if (< (mod i 100) dead-pct)
                   (graph-db::gen-vertex-id)
                   (lookup-vertex (aref ids (mod (1+ i) v)))))
          (incf i))))))

(defun %sweep-p-knows (g)
  "Emitted-edge count of one full typed map-edges sweep over G."
  (let ((c 0))
    (map-edges (lambda (e) (declare (ignore e)) (incf c)) g
               :edge-type 'p-knows)
    c))

(defun bench-v5-cross-store-scan ()
  "map-edges emit cost when a fraction F of edges carry a dead v5
endpoint, vs the number of open stores S (GH #244, #252).  Since PR
#243 a v5 endpoint miss in %ACTIVE-ENDPOINT-STATUS scans every open
store; F=0 should stay flat in S (hit path untouched), high F should
grow linearly in S.  Labels: v5scan-fF-sS with S = TOTAL open stores."
  (let ((e (scale 2000)) (sweeps 5) (batch 500))
    (dolist (dead-pct '(0 10 50))
      (with-perf-graph (g)
        (%insert-v5-mix e dead-pct batch)
        (let* ((live (- e (loop for i below e
                                count (< (mod i 100) dead-pct))))
               ;; Untimed warm sweep: first-touch table/cache faults
               ;; stay out of the s=1 cell.
               (warm (%sweep-p-knows g))
               (extras '())
               (extra-dirs '()))
          (assert (= live warm) ()
                  "v5scan-f~D: expected ~D live edges, warm sweep saw ~D"
                  dead-pct live warm)
          (unwind-protect
               (dolist (total '(1 2 4 8))
                 (loop while (< (1+ (length extras)) total)
                       do (let ((d (make-temp-directory))
                                (name (intern (format nil "PERF-VS-~D"
                                                      (length extras))
                                              :keyword)))
                            (push d extra-dirs)
                            ;; No :buffer-pool-size -- process-global,
                            ;; already sized (see %RUN-MULTIGRAPH-CELL).
                            (push (make-graph name (namestring d))
                                  extras)))
                 (collect-garbage)
                 (let ((start (get-internal-real-time))
                       (emitted 0))
                   (dotimes (r sweeps)
                     (setf emitted (%sweep-p-knows g)))
                   (let* ((elapsed (/ (- (get-internal-real-time) start)
                                      (float
                                       internal-time-units-per-second)))
                          (ops (* e sweeps)))
                     (assert (= live emitted) ()
                             "v5scan-f~D-s~D: expected ~D live edges, ~
                              emitted ~D"
                             dead-pct total live emitted)
                     (record (format nil "v5scan-f~D-s~D" dead-pct total)
                             :ops ops :seconds (float-3 elapsed)
                             :ops/s (if (zerop elapsed)
                                        0
                                        (round (/ ops elapsed)))
                             :us/edge (float-3 (/ (* elapsed 1e6) ops))
                             :emitted emitted))))
            (dolist (x extras)
              (ignore-errors (close-graph x :snapshot-p nil)))
            (collect-garbage)
            (dolist (d extra-dirs)
              (uiop:delete-directory-tree d :validate t
                                          :if-does-not-exist
                                          :ignore))))))))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun run-perf (&key (scale *perf-scale*) (tag "perf")
                      (output (report-pathname tag)))
  "Run the full perf suite at SCALE, record results, write a report to OUTPUT.
Measurement-only; always returns T."
  ;; A system directory for the stores these benchmarks open (GH #186).
  (let* ((*perf-scale* scale)
         (system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (reset-perf-report)
    (format t "~&=== graph-db perf (~A, scale ~A) ===~%" *lisp-impl* scale)
    (finish-output)
    (unwind-protect
         (progn
           (bench-crud)
           (bench-edges)
           (bench-view)
           (bench-unique)
           (bench-index)
           (bench-prolog)
           (bench-commit-overhead)
           (bench-concurrent-rw)
           (bench-disk-growth)
           (bench-snapshot-restore-reopen)
           (bench-multigraph-commit)
           (bench-v5-cross-store-scan)
           (write-perf-report output :tag tag))
      ;; system-dir and all bench scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run)))
  t)

;; Alias so the headless driver can call (graph-db/perf-test:perf-suite).
(defun perf-suite () (run-perf))
