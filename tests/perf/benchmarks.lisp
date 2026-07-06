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

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun run-perf (&key (scale *perf-scale*) (tag "perf")
                      (output (report-pathname tag)))
  "Run the full perf suite at SCALE, record results, write a report to OUTPUT.
Measurement-only; always returns T."
  (let ((*perf-scale* scale))
    (reset-perf-report)
    (format t "~&=== graph-db perf (~A, scale ~A) ===~%" *lisp-impl* scale)
    (finish-output)
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
    (write-perf-report output :tag tag))
  t)

;; Alias so the headless driver can call (graph-db/perf-test:perf-suite).
(defun perf-suite () (run-perf))
