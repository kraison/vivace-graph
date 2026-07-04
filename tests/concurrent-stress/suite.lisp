;;;; Multi-threaded stress-test suite root: fixtures, thread runner, scale control,
;;;; and timing instrumentation (same design as graph-db/stress-test).

(in-package #:graph-db/concurrent-stress-test)

;;; ---------------------------------------------------------------------------
;;; Suite root
;;; ---------------------------------------------------------------------------

(def-suite concurrent-stress-suite
  :description "Multi-threaded scale and stability tests for graph-db.")

(defun run-concurrent-stress-tests ()
  "Run the concurrent-stress suite.  Returns T on all-pass."
  (log:config :error)
  (let ((results (run 'concurrent-stress-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Thread-count parameter
;;; ---------------------------------------------------------------------------

(defparameter *stress-thread-count* 16
  "Default thread count for concurrent-stress tests.  Can be reduced for
slower machines or bumped for deeper stress.  Must be ≥ 4.")

;;; ---------------------------------------------------------------------------
;;; Timing instrumentation (identical API to graph-db/stress-test)
;;; ---------------------------------------------------------------------------

(defparameter *lisp-impl*
  (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))

(defparameter *collect-timings* nil)

(defvar *timing-report* nil)

(defmacro with-timing ((label) &body body)
  (let ((start (gensym "START"))
        (end   (gensym "END")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (when *collect-timings*
           (let ((,end (get-internal-real-time)))
             (let ((elapsed (/ (- ,end ,start)
                               (float internal-time-units-per-second))))
               (pushnew (list ,label :seconds elapsed :impl *lisp-impl*)
                        *timing-report* :key #'car :test #'equal))))))))

(defun record-throughput (label ops elapsed)
  (when *collect-timings*
    (let ((ops/s (if (zerop elapsed) 0 (round (/ ops elapsed)))))
      (format t "~&~A: ~D ops in ~,2Fs (~:D ops/sec) [~A]~%"
              label ops elapsed ops/s *lisp-impl*)
      (pushnew (list label :ops ops :seconds elapsed :ops/s ops/s :impl *lisp-impl*)
               *timing-report* :key #'car :test #'equal))))

;;; Per-thread latency support — each thread appends its elapsed time to a
;;; shared vector; record-thread-latencies summarises min/mean/max.

(defun make-latency-table (n)
  "Return a vector of N slots for per-thread elapsed-time storage."
  (make-array n :initial-element nil))

(defun record-thread-latencies (label table)
  "Print and store per-thread latency summary (min/mean/max)."
  (when *collect-timings*
    (let* ((times (remove nil (coerce table 'list)))
           (n     (length times)))
      (when (plusp n)
        (let* ((mn  (apply #'min times))
               (mx  (apply #'max times))
               (avg (/ (reduce #'+ times) n)))
          (format t "~&~A latency: min=~,3Fs mean=~,3Fs max=~,3Fs (~D threads) [~A]~%"
                  label mn avg mx n *lisp-impl*)
          (pushnew (list label :latency-min mn :latency-mean avg :latency-max mx
                         :threads n :impl *lisp-impl*)
                   *timing-report* :key #'car :test #'equal))))))

;;; ---------------------------------------------------------------------------
;;; Temp-directory and GC helpers
;;; ---------------------------------------------------------------------------

(defun make-temp-directory ()
  (let ((dir (merge-pathnames
              (format nil "graph-db-cstress-~36R/" (random (expt 36 12)))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl  (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl  (ext:gc t))

;;; ---------------------------------------------------------------------------
;;; Schema
;;; ---------------------------------------------------------------------------

(defparameter *cstress-graph-name* :graph-db-concurrent-stress-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *cstress-graph-name* *schema-node-metadata*) nil))

(def-vertex cs-item ()
  ((value)
   (label))
  :graph-db-concurrent-stress-test)

(def-edge cs-link ()
  ()
  :graph-db-concurrent-stress-test)

;;; ---------------------------------------------------------------------------
;;; Graph fixture
;;; ---------------------------------------------------------------------------

(defmacro with-cstress-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *cstress-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 2000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; run-threads — barrier-synchronized deadlock timeout (60s, all impls)
;;; ---------------------------------------------------------------------------

;;; TIMEOUT is a deadlock detector, not a performance gate.  ECL previously needed
;;; a much larger budget (240s) because FULL-SYSTEM-STORM went GC-bound late in the
;;; suite: ECL GC mark-cost over the global buffer pool's *millions* of
;;; pre-allocated objects, which every GC marked (issue #43).  Fixed by #48 (the
;;; buffer pool is now bounded by *buffer-pool-size* + resizable) and #47 (the
;;; vertex/edge pools are gone on ECL, since ECL builds subclasses directly), so
;;; the pool no longer grows unbounded and the storm runs fast on ECL too
;;; (~4 s in isolation).  All impls now share the tight 60 s budget, so a genuine
;;; deadlock trips quickly everywhere.
(defun run-threads (n fn &key (timeout 60))
  "Spawn N threads, barrier-synchronize, release, join.  *GRAPH* is captured
and rebound in each child.  Errors in children are re-raised in the caller."
  (let* ((captured-graph *graph*)
         (arrival  (bt:make-semaphore))
         (go-gate  (bt:make-semaphore))
         (done     (bt:make-semaphore))
         (errors   (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((idx i))
        (bt:make-thread
         (lambda ()
           (let ((*graph* captured-graph))
             (unwind-protect
                  (handler-case
                      (progn
                        (bt:signal-semaphore arrival)
                        (bt:wait-on-semaphore go-gate)
                        (funcall fn idx))
                    (error (e)
                      (setf (aref errors idx) e)))
               (bt:signal-semaphore done))))
         :name (format nil "cstress-~D" i))))
    (dotimes (_ n)
      (unless (bt:wait-on-semaphore arrival :timeout 10)
        (error "run-threads: thread ~D did not start within 10s" _)))
    (bt:signal-semaphore go-gate :count n)
    (let* ((itps   (float internal-time-units-per-second 1.0d0))
           (start  (get-internal-real-time))
           (budget (* timeout itps)))
      (dotimes (_ n)
        (let* ((elapsed   (- (get-internal-real-time) start))
               (remaining (/ (max 0 (- budget elapsed)) itps)))
          (unless (bt:wait-on-semaphore done :timeout remaining)
            (error "run-threads: timed out after ~As (deadlock?)" timeout)))))
    (dotimes (i n)
      (when (aref errors i)
        (error "Thread ~D signaled: ~A" i (aref errors i))))))

;;; ---------------------------------------------------------------------------
;;; View registration helper
;;; ---------------------------------------------------------------------------

(defun define-cstress-views ()
  (def-view cs-item-by-value :lessp (cs-item :graph-db-concurrent-stress-test)
    (:map (lambda (item)
            (yield (slot-value item 'value) t))))
  (def-view cs-item-count :lessp (cs-item :graph-db-concurrent-stress-test)
    (:map    (lambda (item)
               (declare (ignore item))
               (yield :all 1)))
    (:reduce (lambda (keys values)
               (declare (ignore keys))
               (apply #'+ values)))))
