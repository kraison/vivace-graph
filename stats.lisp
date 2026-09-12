(in-package :graph-db)

(defun graph-writes-report (graph)
  (let ((report nil))
    (maphash (lambda (time writes)
               (push (cons time writes) report))
             (write-stats graph))
    (sort report '< :key 'car)))

(defun graph-writes-report-last-minute (graph)
  (let ((count 0) (tally 0) (now (get-universal-time)))
    (loop
       for time from now downto (- now 60)
       for writes = (gethash time (write-stats graph) 0)
       do
         (incf count)
         (incf tally writes))
    (coerce (/ tally count) 'double-float)))

(defun graph-reads-report (graph)
  (let ((report nil))
    (maphash (lambda (time reads)
               (push (cons time reads) report))
             (read-stats graph))
    (sort report '< :key 'car)))

(defun graph-reads-report-last-minute (graph)
  (let ((count 0) (tally 0) (now (get-universal-time)))
    (loop
       for time from now downto (- now 60)
       for reads = (gethash time (read-stats graph) 0)
       do
         (incf count)
         (incf tally reads))
    (coerce (/ tally count) 'double-float)))

(defun graph-rw-report (&key (graph *graph*))
  (let ((writes (graph-writes-report graph))
        (reads (graph-reads-report graph)))
    (let ((start (if (< (car (first writes)) (car (first reads)))
                     (car (first writes))
                     (car (first reads))))
          (end (if (> (car (last1 writes)) (car (last1 reads)))
                   (car (last1 writes))
                   (car (last1 reads)))))
      (loop for time from start to end collecting
           (list time
                 (or (cdr (assoc time writes)) 0)
                 (or (cdr (assoc time reads)) 0))))))

(defun graph-stats (&key (graph *graph*) detail-p)
  (let ((report
         `((:free-memory . ,(free-memory))
           (:avg-writes-per-second . ,(graph-writes-report-last-minute graph))
           (:avg-reads-per-second . ,(graph-reads-report-last-minute graph))
           (:cache-size . ,(hash-table-count (cache graph)))
           (:vertex-count . ,(read-lhash-count (vertex-table graph)))
           (:edge-count . ,(read-lhash-count (edge-table graph)))
           (:buffer-pool . ,(if (buffer-pool-running-p)
                                (dump-buffer-pool-stats)
                                "DISABLED")))))
    (when detail-p
      (setq report
            (let ((writes-report (graph-writes-report graph))
                  (reads-report (graph-reads-report graph)))
              (nconc (list (cons :writes-detail writes-report)
                           (cons :reads-detail reads-report))
                     report))))
    report))

;;; RECORD-GRAPH-READ runs on every cached node lookup.  Reading the wall
;;; clock and locking the histogram per call was half of an adjacency
;;; read's profile (GH #373), so both are sampled: one fixnum increment
;;; per call, and every +STATS-SAMPLE+ calls the current second's bucket
;;; gains +STATS-SAMPLE+.  Per-graph totals are exact to within one
;;; sample; the plain INCF may lose a count under contention, which the
;;; report tolerates.

(defconstant +stats-sample+ 64
  "Reads or writes per histogram update in RECORD-GRAPH-READ / -WRITE; a
power of two, so the sample test is one LOGAND (GH #373).")

(defun record-graph-write (&optional (graph *graph*))
  "Count one write of GRAPH into its per-second histogram, sampled every
+STATS-SAMPLE+ calls (GH #373)."
  (when (zerop (logand (incf (write-sample-counter graph))
                       (1- +stats-sample+)))
    (incf (gethash (get-universal-time) (write-stats graph) 0)
          +stats-sample+)))

(defun record-graph-read (&optional (graph *graph*))
  "Count one read of GRAPH into its per-second histogram, sampled every
+STATS-SAMPLE+ calls (GH #373).  Trap: a graph read fewer than
+STATS-SAMPLE+ times reports no reads at all."
  (when (zerop (logand (incf (read-sample-counter graph))
                       (1- +stats-sample+)))
    (incf (gethash (get-universal-time) (read-stats graph) 0)
          +stats-sample+)))
