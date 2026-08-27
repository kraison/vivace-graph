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
graphs share -- the process-global buffer pool, GC, one filesystem.
It originally measured the pre-#237 global watermark lock's convoy
(~89% of n8 commit latency); that fix landed, and the cell stays as
a regression sentinel for it.  The nN-rawwm control reruns the
largest cell with PERSIST-HIGHEST-TRANSACTION-ID swapped for the raw
writer: nN minus nN-rawwm now bounds any residual per-commit
watermark cost (~nothing post-fix).  The watermark-* pair prices one
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

(defun %clock-commit-cell (label n)
  "N one-vertex txns on a fresh graph; records LABEL with :us/commit.
The graph picks up GRAPH-DB:*SYSTEM-CLOCK* via MAKE-GRAPH's default, so
the caller's binding selects the clocked or local id path (GH #254).
An untimed warm batch precedes the timed loop (the v5-scan warm-sweep
idiom): the local-vs-clocked delta is the signal, so cold-start
asymmetry must stay out of both cells."
  (with-perf-graph (g)
    (dotimes (i 50)
      (with-transaction ()
        (make-p-node :val -1 :label "w")))
    (let ((start (get-internal-real-time)))
      (dotimes (i n)
        (with-transaction ()
          (make-p-node :val i :label "k")))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        (float internal-time-units-per-second))))
        (record label
                :ops n :seconds (float-3 elapsed)
                :ops/s (if (zerop elapsed) 0 (round (/ n elapsed)))
                :us/commit (float-3 (/ (* elapsed 1e6) n)))))))

(defun bench-system-clock ()
  "System-clock overhead (GH #168, #254): per-commit id-allocation cost
clocked vs local (fresh cells, same shape, for locality), the raw
CLOCK-NEXT-EPOCH allocator alone and under 4-thread contention, and
ATTACH-TO-SYSTEM-CLOCK on a store with a real watermark.  Every clock
holds a directory flock for its lifetime, so each cell closes its clock
in an unwind-protect -- a leaked fd would refuse later opens (GH #182)."
  (let ((n (scale 2000)))
    ;; Per-commit cost: local per-store counter vs image clock.
    (let ((graph-db:*system-clock* nil))
      (%clock-commit-cell "clock-commit-local" n))
    (with-temp-directory (cdir)
      (let ((clock (graph-db:open-system-clock (namestring cdir))))
        (unwind-protect
             (let ((graph-db:*system-clock* clock))
               (%clock-commit-cell "clock-commit-clocked" n))
          (graph-db:close-system-clock clock)))))
  ;; The epoch allocator in isolation (lock + counter + amortized
  ;; ceiling write every BLOCK-SIZE ids).
  (let ((iters (scale 50000)))
    (with-temp-directory (cdir)
      (let ((clock (graph-db:open-system-clock (namestring cdir))))
        (unwind-protect
             (timed-ops ("clock-epoch-alloc" iters)
               (dotimes (i iters)
                 (graph-db:clock-next-epoch clock)))
          (graph-db:close-system-clock clock)))))
  ;; Attach cost against a store with committed history (the watermark
  ;; read is the interesting part), on a reopened clock-less graph.
  ;; Looped: one attach is sub-millisecond, and a 0.0 :seconds record
  ;; would be un-comparable at the next re-bless (check-perf skips
  ;; zero-valued baseline metrics).  Re-attach is permitted with no
  ;; in-flight transactions and repeats the same watermark-load +
  ;; journal-append work, so ops/s here IS per-attach cost.
  (with-temp-directory (d)
    (let ((graph-db:*system-clock* nil))
      (let ((g (make-graph *perf-graph-name* (namestring d)
                           :buffer-pool-size 4000)))
        (let ((*graph* g))
          (insert-p-nodes (scale 2000) :batch 1000))
        (close-graph g :snapshot-p nil))
      (with-temp-directory (cdir)
        (let ((clock (graph-db:open-system-clock (namestring cdir)))
              (attaches (scale 100)))
          (unwind-protect
               (let ((g (open-graph *perf-graph-name* (namestring d))))
                 (unwind-protect
                      (timed-ops ("clock-attach" attaches)
                        (dotimes (i attaches)
                          (graph-db:attach-to-system-clock g clock)))
                   (close-graph g :snapshot-p nil)))
            (graph-db:close-system-clock clock))))))
  ;; Epoch allocation under contention: 4 threads on ONE clock, gated
  ;; start (thread creation excluded), aggregate ops/s.
  (let ((threads 4) (per (scale 5000)))
    (with-temp-directory (cdir)
      (let ((clock (graph-db:open-system-clock (namestring cdir))))
        (unwind-protect
             (let* ((gate (bordeaux-threads:make-semaphore))
                    (ts (loop repeat threads
                              collect (bordeaux-threads:make-thread
                                       (lambda ()
                                         (bordeaux-threads:wait-on-semaphore
                                          gate)
                                         (dotimes (i per)
                                           (graph-db:clock-next-epoch clock)))
                                       :name "perf-clock-alloc")))
                    (start (get-internal-real-time)))
               (bordeaux-threads:signal-semaphore gate :count threads)
               (mapc #'bordeaux-threads:join-thread ts)
               (record-throughput "clock-epoch-alloc-contended"
                                  (* threads per)
                                  (/ (- (get-internal-real-time) start)
                                     (float
                                      internal-time-units-per-second))))
          (graph-db:close-system-clock clock)))))
  (collect-garbage))

(defun %mem-view-total (g)
  "Sum of the p-mem-sum reduce view's per-key values in G."
  (let ((sum 0))
    (graph-db:map-reduced-view
     (lambda (k id v) (declare (ignore k id)) (incf sum v))
     'p-node 'p-mem-sum :graph g)
    sum))

(defun %assert-mem-graph (g n label)
  "Post-open invariants for BENCH-MEMORY-OPEN: node/edge counts AND the
reduce view's total match the built graph, so neither cell can time an
empty restore (GH #254)."
  (let ((*graph* g))
    (let ((vc (count-vertices g))
          (ec (count-edges g))
          (vt (%mem-view-total g)))
      (assert (= n vc) () "~A: expected ~D vertices, found ~D" label n vc)
      (assert (= n ec) () "~A: expected ~D edges, found ~D" label n ec)
      (assert (= n vt) () "~A: expected view total ~D, got ~D" label n vt))))

(defun bench-memory-open ()
  "Memory-graph open cost (the GH #50 premise, GH #254): image-restore
open vs the journal-replay + view-rebuild fallback, plus the checkpoint
itself, at (SCALE 5000) vertices + edges under an aggregate (reduce)
view.  Correctness trap: after a CLEAN close the journal is cleared and
graph.img is the ONLY durable record -- deleting the image would make
the \"rebuild\" open restore an empty graph.  So the rebuild cell closes
the builder with :SNAPSHOT-P NIL (no image ever written, journal
retained) and the restore cell reopens after an explicit checkpoint;
both cells assert the restored contents."
  (let ((n (scale 5000)) (batch 1000))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)))
        ;; Isolate the view registry: earlier benches register specs
        ;; under this graph name, and INSTALL-VIEWS at open would
        ;; rebuild them all (cf. tests/memory-graph-tests.lisp).
        (remhash *perf-graph-name* graph-db::*schema-view-metadata*)
        (unwind-protect
             (progn
               ;; Build: nodes + edges + a maintained reduce view, then
               ;; close WITHOUT checkpoint (journal retained, no image).
               (let ((g (graph-db::make-memory-graph *perf-graph-name*
                                                     loc)))
                 (unwind-protect
                      (let ((*graph* g))
                        (def-view p-mem-sum :lessp
                          (p-node :graph-db-perf-test)
                          (:map (lambda (v)
                                  (yield (floor (slot-value v 'val) 100)
                                         1)))
                          (:reduce (lambda (keys values)
                                     (declare (ignore keys))
                                     (reduce #'+ values))))
                        (let ((ids (insert-p-nodes n :batch batch))
                              (i 0))
                          (loop while (< i n) do
                            (with-transaction ()
                              (dotimes (k (min batch (- n i)))
                                (make-p-knows
                                 :from (lookup-vertex
                                        (aref ids (mod i n)))
                                 :to (lookup-vertex
                                      (aref ids (mod (1+ i) n))))
                                (incf i))))))
                   (close-graph g :snapshot-p nil)))
               (collect-garbage)
               ;; (a) rebuild-shaped open: full journal replay + view
               ;; rebuild from nodes (the v1 fallback path).
               (let (g2)
                 (timed-seconds ("memory-open-rebuild")
                   (setf g2 (graph-db::open-memory-graph
                             *perf-graph-name* loc)))
                 (unwind-protect
                      (progn
                        (%assert-mem-graph g2 n "memory-open-rebuild")
                        ;; Checkpoint at scale (writes graph.img,
                        ;; clears the journal).
                        (timed-seconds ("memory-checkpoint")
                          (graph-db::checkpoint-memory-graph g2)))
                   (close-graph g2 :snapshot-p nil)))
               (collect-garbage)
               ;; (b) image-restore open (structural; no map/reduce).
               (let (g3)
                 (timed-seconds ("memory-open-image-restore")
                   (setf g3 (graph-db::open-memory-graph
                             *perf-graph-name* loc)))
                 (unwind-protect
                      (%assert-mem-graph g3 n "memory-open-image-restore")
                   (close-graph g3 :snapshot-p nil))))
          (remhash *perf-graph-name* graph-db::*schema-view-metadata*)
          (collect-garbage))))))

(defun %insert-compact-mix (e batch)
  "Insert E p-knows edges bucketed by (MOD i 100): <30 a fresh dead v5
:TO id, 30-39 healthy then soft-deleted below, 40-49 an unassigned-tag
v8 id (:UNKNOWN -- kept by :CONSERVATIVE, collected by :TRUST-TAGS, so
the two policy cells genuinely differ; the #243 tests' idiom), else
healthy.  Returns the count of edges soft-deleted."
  (let* ((v (min e 500))
         (ids (insert-p-nodes v :batch batch))
         (del '())
         (i 0))
    (loop while (< i e) do
      (with-transaction ()
        (dotimes (k (min batch (- e i)))
          (let* ((bucket (mod i 100))
                 (to (cond ((< bucket 30) (graph-db::gen-vertex-id))
                           ((< bucket 50)
                            (if (< bucket 40)
                                (lookup-vertex (aref ids (mod (1+ i) v)))
                                (graph-db::gen-v8-uuid 4000)))
                           (t (lookup-vertex
                               (aref ids (mod (1+ i) v))))))
                 (edge (make-p-knows
                        :from (lookup-vertex (aref ids (mod i v)))
                        :to to)))
            (when (and (>= bucket 30) (< bucket 40))
              (push (id edge) del)))
          (incf i))))
    (let ((remaining del))
      (loop while remaining do
        (with-transaction ()
          (dotimes (k (min batch (length remaining)))
            (mark-deleted (lookup-edge (pop remaining)))))))
    (length del)))

(defun %count-live-p-knows (g)
  "Count of non-soft-deleted p-knows edges.  :INCLUDE-DELETED-P bypasses
the ACTIVE-EDGE-P emit filter, so dead-endpoint edges are counted too
and the sweep pays no endpoint-status cost."
  (let ((live 0))
    (map-edges (lambda (e) (unless (deleted-p e) (incf live)))
               g :edge-type 'p-knows :include-deleted-p t)
    live))

(defun %run-compact-cell (policy label)
  "One COMPACT-EDGES cell (GH #208, #243, #254): fresh v5-mix fixture
((SCALE 2000) edges: 30% dead-v5, 10% soft-deleted, 10% unknown-tag),
one EXTRA open store so v5 misses take the all-open-stores scan, POLICY
timed, :COMPACTED derived from live counts before/after and asserted
against the bucket arithmetic."
  (let* ((e (scale 2000)) (batch 500)
         (dead (loop for i below e count (< (mod i 100) 30)))
         (soft (loop for i below e count (<= 30 (mod i 100) 39)))
         (unknown (loop for i below e count (<= 40 (mod i 100) 49)))
         (expect (ecase policy
                   (:conservative dead)
                   (:trust-tags (+ dead unknown)))))
    (with-perf-graph (g)
      (let ((deleted (%insert-compact-mix e batch))
            (extra-dir (make-temp-directory))
            (extra nil))
        (assert (= soft deleted))
        (unwind-protect
             (progn
               (setf extra (make-graph :perf-compact-extra
                                       (namestring extra-dir)))
               (let ((before (%count-live-p-knows g))
                     (elapsed nil))
                 (assert (= (- e soft) before))
                 (setf elapsed
                       (timed-seconds (label)
                         (graph-db::compact-edges g :policy policy)))
                 (let* ((after (%count-live-p-knows g))
                        (compacted (- before after)))
                   (assert (= expect compacted) ()
                           "~A: expected ~D edges compacted, got ~D"
                           label expect compacted)
                   ;; Re-record with the derived count alongside the
                   ;; timing (last write wins for the label).
                   (record label
                           :seconds (float-3 elapsed)
                           :compacted compacted))))
          (when extra (ignore-errors (close-graph extra :snapshot-p nil)))
          (uiop:delete-directory-tree extra-dir :validate t
                                      :if-does-not-exist :ignore))))
    (collect-garbage)))

(defun bench-compact-edges ()
  "COMPACT-EDGES wall time under both policies over a controlled
dead-endpoint mix (GH #208, #243, #254).  :CONSERVATIVE DELETES the
dead-v5 group and DE-INDEXES the already-soft-deleted group;
:TRUST-TAGS additionally deletes the unknown-tag group.  :COMPACTED
counts only NEWLY-deleted edges -- the de-index work on the
soft-deleted group is in the timing but, by construction, invisible in
the live-count delta.  Each policy gets a fresh fixture (compaction is
destructive)."
  (%run-compact-cell :conservative "compact-conservative")
  (%run-compact-cell :trust-tags "compact-trust-tags"))

(defparameter *peer-bench-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(9 9 9 9 0 0 0 0 0 0 0 0 0 0 0 2))
  "Fixed device origin id (the tests/peer-replication schema idiom).")

(defun %peer-disclosable-p (vertex graph scope)
  "Perf export predicate: disclosable iff VAL >= 0 (the harness's
flag-slot idiom, on p-node's existing VAL slot)."
  (declare (ignore graph scope))
  (not (minusp (slot-value vertex 'val))))

(defun %peer-build-corpus (g n batch)
  "Root p-node + N children, each edged root->child; every 10th child
withheld (VAL -1).  Returns the root vertex."
  (let ((*graph* g) root)
    (with-transaction ()
      (setf root (make-p-node :val 0 :label "root" :graph g)))
    (let ((i 0))
      (loop while (< i n) do
        (with-transaction ()
          (dotimes (k (min batch (- n i)))
            (let ((c (make-p-node :val (if (zerop (mod i 10)) -1 i)
                                  :label "c" :graph g)))
              (make-p-knows :from root :to c :graph g))
            (incf i)))))
    root))

(defun %peer-write-node-create (stream graph node)
  "WRITE-NODE-AS-CREATE's exact three state-create packets (peer-meta,
tx-header, tx-write -- peer-streaming.lisp) to a binary STREAM instead
of a socket: the sockets-free export half."
  ;; Keep in lockstep with WRITE-NODE-AS-CREATE (peer-streaming.lisp).
  ;; No per-packet FORCE-OUTPUT here (WRITE-PACKET does one) -- another
  ;; reason this is serialization cost, not wire throughput.
  (graph-db::maybe-init-node-data node :graph graph)
  (setf (graph-db::bytes node)
        (graph-db::serialize (graph-db::data node)))
  (let* ((origin (or (graph-db::origin-id graph)
                     graph-db::+peer-null-origin+))
         (epoch (or (graph-db::commit-epoch node) 0))
         (write (make-instance 'graph-db::tx-create :node node))
         (txh (make-instance 'graph-db::tx-header
                             :transaction-id epoch
                             :writes (list write) :graph graph)))
    (write-sequence (graph-db::serialize-peer-meta
                     origin (id node) epoch
                     graph-db::+peer-op-state-create+)
                    stream)
    (write-sequence (graph-db::tx-header-vector txh epoch) stream)
    (write-sequence (graph-db::tx-write-vector write) stream)))

(defun %peer-apply-wire-file (dev path)
  "Device-side decode + apply of the state-create packet file at PATH
into DEV -- PEER-READ-AND-ENQUEUE's decode and the writer-loop's
:state-create decode+apply work (mailbox hop and manifest accumulation
excluded -- GH #260), single-threaded.  Returns the op count."
  ;; Two deliberate exclusions vs the real writer loop: the mailbox
  ;; hop (connection thread -> writer thread), and the created-manifest
  ;; PUSHNEW accumulation -- O(ops^2) in production, filed as GH #260.
  (let ((*graph* dev) (applied 0))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (loop
        (let ((meta (graph-db::read-stream-packet in)))
          (unless meta (return applied))
          (multiple-value-bind (origin op-id lamport op-class)
              (graph-db::deserialize-peer-meta meta)
            (declare (ignore op-id lamport op-class))
            (let* ((txh (graph-db::deserialize-tx-header-vector
                         (graph-db::read-stream-packet in)))
                   (writes
                     (loop repeat (graph-db::write-count txh)
                           collect (graph-db::deserialize-tx-write-vector
                                    (graph-db::read-stream-packet in)))))
              (graph-db::apply-peer-create-writes
               dev (graph-db::transaction-id txh) writes origin)
              (incf applied))))))))

(defun bench-peer-replication ()
  "Peer replication cost, sockets-free (GH #254).  Hub and device peers
cannot share one image -- *GRAPHS*, the schema registry and *GRAPH* are
process-global, which is why tests/peer-replication runs two OS
processes -- so this benches the two halves of the wire separately:
  peer-export-scope = SCOPE-NODE-SET closure + the exact state-create
  packets a pull serves (WRITE-NODE-AS-CREATE's), written to a file;
  peer-apply = the device-side packet decode + APPLY-PEER-CREATE-WRITES
  of that file into a fresh :device peer graph (the writer-loop
  :state-create path, single-threaded), cold device store included --
  that IS a first sync.
10% of children are withheld so DISCLOSABLE-P and the closed rule
(edge kept iff both endpoints disclosable) do real work."
  (let* ((n (scale 2000)) (batch 500)
         (withheld (loop for i below n count (zerop (mod i 10))))
         (disc (- n withheld))
         (vexp (1+ disc))               ; root + disclosable children
         (eexp disc)
         (total (+ vexp eexp)))
    (with-temp-directory (hub-dir)
      (with-temp-directory (dev-dir)
        (let ((wire (merge-pathnames "peer-wire.dat" hub-dir)))
          ;; Export half: hub-role peer graph.  Port 0 = ephemeral; the
          ;; accept loop is idle and joined by CLOSE-GRAPH.
          (let ((hub (make-graph *perf-graph-name* (namestring hub-dir)
                                 :peer-role :hub
                                 :replication-port 0
                                 :export-predicate #'%peer-disclosable-p
                                 :buffer-pool-size 4000)))
            (unwind-protect
                 (let ((*graph* hub)
                       (root (%peer-build-corpus hub n batch))
                       (vcount 0) (ecount 0) (exported 0))
                   (timed-ops ("peer-export-scope" total)
                     (graph-db::with-read-snapshot (hub)
                       (multiple-value-bind (vset eset)
                           (graph-db::scope-node-set hub (list root)
                                                     :main)
                         (setf vcount (hash-table-count vset)
                               ecount (hash-table-count eset))
                         (with-open-file
                             (out wire :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
                           (flet ((ship (id node)
                                    (declare (ignore id))
                                    (%peer-write-node-create out hub
                                                             node)
                                    (incf exported)))
                             ;; vertices before edges (the closed rule)
                             (maphash #'ship vset)
                             (maphash #'ship eset))))))
                   (assert (= vexp vcount) ()
                           "peer-export-scope: expected ~D vertices in ~
                            scope, got ~D" vexp vcount)
                   (assert (= eexp ecount) ()
                           "peer-export-scope: expected ~D edges in ~
                            scope, got ~D" eexp ecount)
                   (assert (= total exported) ()
                           "peer-export-scope: expected ~D nodes ~
                            shipped, wrote ~D" total exported))
              (ignore-errors (close-graph hub :snapshot-p nil))))
          (collect-garbage)
          ;; Apply half: fresh device-role peer graph.
          (let ((dev (make-graph :perf-peer-device (namestring dev-dir)
                                 :peer-role :device
                                 :origin-id *peer-bench-origin*
                                 :buffer-pool-size 4000)))
            (unwind-protect
                 (let ((*graph* dev) (applied 0))
                   ;; Warm-up commit adopts p-node/p-knows into this
                   ;; store (the %RUN-MULTIGRAPH-CELL idiom), out of the
                   ;; timing.
                   (with-transaction ()
                     (let ((a (make-p-node :val -1 :label "w"
                                           :graph dev))
                           (b (make-p-node :val -2 :label "w"
                                           :graph dev)))
                       (make-p-knows :from a :to b :graph dev)))
                   (timed-ops ("peer-apply" total)
                     (setf applied (%peer-apply-wire-file dev wire)))
                   (assert (= total applied) ()
                           "peer-apply: expected ~D ops, applied ~D"
                           total applied)
                   (let ((vc (count-vertices dev)) (ec (count-edges dev)))
                     (assert (= (+ vexp 2) vc) ()
                             "peer-apply: expected ~D vertices, got ~D"
                             (+ vexp 2) vc)
                     (assert (= (1+ eexp) ec) ()
                             "peer-apply: expected ~D edges, got ~D"
                             (1+ eexp) ec)))
              (ignore-errors (close-graph dev :snapshot-p nil))))))))
  (collect-garbage))

(defun %pv-vector (seed dim)
  "Deterministic pseudo-random single-float vector in [-1,1): an inline
LCG from SEED -- a fixed integer sequence, no RANDOM state (GH #254)."
  (let ((v (make-array dim :element-type 'single-float))
        (s (logand (+ (* 2654435761 (1+ seed)) 104729) #xFFFFFFFF)))
    (dotimes (i dim v)
      (setf s (logand (+ (* 1103515245 s) 12345) #xFFFFFFFF))
      (setf (aref v i)
            (- (/ (float (ash s -8) 1.0f0) 8388608.0f0) 1.0f0)))))

(defun %pv-insert (n dim batch &key (seed-base 0))
  "Insert N pv-nodes with dim-DIM embeddings; return their id vector."
  (let ((ids (make-array n)) (i 0))
    (loop while (< i n) do
      (with-transaction ()
        (dotimes (k (min batch (- n i)))
          (setf (aref ids i)
                (id (make-pv-node :label "v"
                                  :embedding (%pv-vector (+ seed-base i)
                                                         dim))))
          (incf i))))
    ids))

(defun %pv-knn-cell (g label queries ids n dim k seed-base)
  "Time QUERIES vector-search calls (k=K) over the N-vector segment,
querying with STORED vectors; then pin exact-match recall: a stored
vector's own query must return its node first at cosine ~1."
  (timed-ops (label queries)
    (dotimes (q queries)
      (vector-search g 'pv-node 'embedding
                     (%pv-vector (+ seed-base (mod (* q 7) n)) dim)
                     k)))
  (dotimes (probe 20)
    (let* ((i (mod (* probe (floor n 20)) n))
           (got (vector-search g 'pv-node 'embedding
                               (%pv-vector (+ seed-base i) dim) k)))
      (assert (= (min k n) (length got)) ()
              "~A: expected ~D results, got ~D" label (min k n)
              (length got))
      (assert (equalp (aref ids i) (cdr (first got))) ()
              "~A: exact-match query for node ~D did not return it first"
              label i)
      (assert (> (car (first got)) 0.9999) ()
              "~A: self-cosine ~F < 1" label (car (first got))))))

(defun bench-vector-search ()
  "Vector-segment throughput (GH #254): VECTOR-INSERT = transactional
inserts through the :VECTOR-INDEX apply path (encode + SEGMENT-PUT,
vectors/s); VECTOR-KNN-K10 = VECTOR-SEARCH cosine top-10 scans
(queries/s) -- the post-decode-tax fixed path scores straight off the
mmap.  Exact-match recall is asserted (a stored vector must return its
own node first), so neither cell can time an empty or wrong segment.
At :normal a 4x-larger segment repeats the kNN cell (scan cost is
O(n*dim))."
  (let ((n (scale 5000)) (dim 128) (batch 500) (q (scale 200)) (k 10))
    (with-perf-graph (g)
      (let (ids)
        (timed-ops ("vector-insert" n)
          (setf ids (%pv-insert n dim batch)))
        (let ((seg (gethash (cons 'pv-node 'embedding)
                            (graph-db::vector-segments g))))
          (assert (and seg (= n (graph-db::segment-live-count seg))) ()
                  "vector-insert: segment live-count /= ~D" n))
        (%pv-knn-cell g "vector-knn-k10" q ids n dim k 0)
        ;; Larger segment, :normal only: 5000 -> 20000 vectors.
        (when (eq *perf-scale* :normal)
          (let* ((extra (* 3 n)) (big (+ n extra))
                 (more (%pv-insert extra dim batch :seed-base n))
                 (all (make-array big)))
            (replace all ids) (replace all more :start1 n)
            (%pv-knn-cell g "vector-knn-k10-20k" 100 all big dim k
                          0)))))))

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
         (graph-db::*type-registry* nil)
         ;; A user image's global clock must not silently attach every
         ;; bench graph (GH #254); the clock cells bind their own.
         (graph-db:*system-clock* nil))
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
           ;; coverage benches, batch A (GH #254)
           (bench-system-clock)
           (bench-memory-open)
           (bench-compact-edges)
           ;; coverage benches, batch B (GH #254)
           (bench-peer-replication)
           (bench-vector-search)
           (write-perf-report output :tag tag))
      ;; system-dir and all bench scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run)))
  t)

;; Alias so the headless driver can call (graph-db/perf-test:perf-suite).
(defun perf-suite () (run-perf))
