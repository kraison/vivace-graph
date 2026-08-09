;;;; S5: GEOS context-pool correctness under concurrency.
;;;;
;;;; Many threads hammer the seam (intersects / exact within / make-valid)
;;;; against a shared graph.  We assert: no thread errors; the pool never grows
;;;; past peak concurrency; a context is never checked out twice (debug flag);
;;;; every checkout is returned; concurrent results match a single-threaded run;
;;;; and geos-shutdown + re-init is clean.

(in-package #:graph-db/geos-test)

(def-suite geos-storm-suite
  :description "GEOS context pool under concurrent load."
  :in geos-suite)

(in-suite geos-storm-suite)

(defparameter *storm-threads* #-ecl 12 #+ecl 4)
(defparameter *storm-iterations* 30)

;;; Barrier thread runner: all threads start together, then are joined.
(defun storm-run (n fn)
  (let ((arrival (bordeaux-threads:make-semaphore))
        (gate    (bordeaux-threads:make-semaphore))
        (errors  (make-array n :initial-element nil))
        (threads '()))
    (dotimes (i n)
      (let ((idx i))
        (push (bordeaux-threads:make-thread
               (lambda ()
                 (bordeaux-threads:signal-semaphore arrival)
                 (bordeaux-threads:wait-on-semaphore gate)
                 (handler-case (funcall fn idx)
                   (error (e) (setf (aref errors idx) e))))
               :name (format nil "geos-storm-~D" idx))
              threads)))
    (dotimes (_ n) (bordeaux-threads:wait-on-semaphore arrival :timeout 10))
    (bordeaux-threads:signal-semaphore gate :count n)
    (dolist (th threads) (bordeaux-threads:join-thread th))
    errors))

;; Small region so the index bbox queries are cheap.
(defparameter *storm-aoi*
  (make-polygon '(((0d0 0d0) (0.01d0 0d0) (0.01d0 0.01d0) (0d0 0.01d0) (0d0 0d0)))))

(defun storm-poly (x0 y0 x1 y1)
  (make-polygon (list (list (list x0 y0) (list x1 y0)
                            (list x1 y1) (list x0 y1) (list x0 y0)))))

(defmacro with-storm-graph ((g) &body body)
  `(with-geos-graph (,g)
     (with-transaction ()
       (dotimes (k 12)
         (let ((o (* k 0.0005d0)))
           (make-geos-place :geom (make-point (+ 0.002d0 o) (+ 0.002d0 o)))
           (make-geos-place :geom (storm-poly (+ 0.001d0 o) 0.001d0
                                              (+ 0.004d0 o) 0.004d0)))))
     ,@body))

(defun storm-workload (g)
  "One unit of mixed GEOS work; returns the intersecting-id set (sorted)."
  (geometry-make-valid (make-polygon                       ; repair a bowtie
                        '(((0d0 0d0) (4d0 4d0) (4d0 0d0) (0d0 4d0) (0d0 0d0)))))
  (geometry-intersects-p *storm-aoi* (storm-poly 0.002d0 0.002d0 0.02d0 0.02d0))
  (length (find-nodes-within 'geos-place *storm-aoi* :graph g))
  (sort (mapcar (lambda (n) (format nil "~A" (id n)))
                (find-nodes-intersecting 'geos-place *storm-aoi* :graph g))
        #'string<))

(test pool-correct-under-concurrency
  "N threads x M iterations of mixed GEOS work: no errors, pool bounded by peak
concurrency, no double-checkout, all contexts returned, results deterministic."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t
         (with-storm-graph (g)
           (let ((reference (storm-workload g)))   ; single-threaded baseline
             (geos-shutdown)                        ; clean pool + counters
             (setf *geos-pool-debug* t)
             (unwind-protect
                  (let ((errors
                          (storm-run
                           *storm-threads*
                           ;; bordeaux-threads does not inherit dynamic bindings;
                           ;; rebind *graph* in each worker (transactions/lookups
                           ;; run in the caller's thread and default to *graph*).
                           (lambda (i)
                             (declare (ignore i))
                             (let ((*graph* g))
                               (dotimes (_ *storm-iterations*)
                                 (let ((got (storm-workload g)))
                                   (unless (equal got reference)
                                     (error "concurrent result diverged: ~A vs ~A"
                                            got reference)))))))))
                    (is (every #'null errors)
                        "no thread errored: ~A" (remove nil (coerce errors 'list)))
                    (is (= 0 *geos-pool-in-use*)
                        "all contexts returned (~D still out)" *geos-pool-in-use*)
                    (is (<= *geos-pool-created* *storm-threads*)
                        "pool bounded by concurrency: created ~D > ~D threads"
                        *geos-pool-created* *storm-threads*)
                    (is (<= *geos-pool-peak* *storm-threads*)
                        "peak checkouts ~D <= ~D threads"
                        *geos-pool-peak* *storm-threads*)
                    (is (= *geos-pool-created* (length *geos-pool*))
                        "every created context is back in the free list"))
               (setf *geos-pool-debug* nil)))))))

(test shutdown-and-reinit-clean
  "After a storm, geos-shutdown empties the pool and a fresh operation re-inits."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (geos-shutdown)
        (is (null *geos-pool*))
        (is (= 0 *geos-pool-created*))
        ;; re-init on demand
        (geometry-intersects-p *storm-aoi* *storm-aoi*)
        (is (>= *geos-pool-created* 1) "pool re-initialised after shutdown")
        (is (= 0 *geos-pool-in-use*)))))
