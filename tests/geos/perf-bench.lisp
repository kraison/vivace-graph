;;;; N4: synthetic spatial perf benchmark (measurement, not a pass/fail test).
;;;;
;;;; Generates random point + polygon nodes, measures spatial-index insert
;;;; throughput (the write-path hook indexes each), then times find-nodes-within
;;;; / find-nodes-intersecting in GEOS-exact mode vs the dependency-free fallback.
;;;; Call (run-geos-perf) from a REPL; it is NOT part of the FiveAM suite.
;;;;
;;;; Note: the real ~458k-image / 440-find dataset lives in the private ingestion
;;;; pipeline; this exercises the same code paths at a configurable synthetic scale.

(in-package #:graph-db/geos-test)

(defun %ms-since (start)
  (/ (* 1000d0 (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defun %rand (lo span) (+ lo (random (coerce span 'double-float))))

(defun %rand-point (lo-lon lo-lat span)
  (make-point (%rand lo-lon span) (%rand lo-lat span)))

(defun %rand-polygon (lo-lon lo-lat span size)
  "A small axis-aligned square at a random spot in the region."
  (let ((x (%rand lo-lon span)) (y (%rand lo-lat span)) (s (coerce size 'double-float)))
    (make-polygon (list (list (list x y) (list (+ x s) y)
                              (list (+ x s) (+ y s)) (list x (+ y s)) (list x y))))))

(defun run-geos-perf (&key (n-points 5000) (n-polygons 500) (batch 500)
                           (lo-lon 37.0d0) (lo-lat 49.0d0) (span 0.1d0))
  "Benchmark spatial index insert + query at a synthetic scale.  Returns a plist
of timings (ms) and prints a short report."
  (unless *geos-available-p*
    (format t "~&run-geos-perf: GEOS not available; skipping.~%")
    (return-from run-geos-perf nil))
  (let ((dir (make-temp-directory)) (results '()))
    (unwind-protect
         (let ((g (make-graph *geos-graph-name* (namestring dir) :buffer-pool-size 4000)))
           (unwind-protect
                (let ((*graph* g)
                      ;; AOI: a 0.02 x 0.02 deg window in the middle of the region.
                      (aoi (let ((c0 (+ lo-lon (* span 0.4d0))) (c1 (+ lo-lat (* span 0.4d0))))
                             (make-polygon (list (list (list c0 c1) (list (+ c0 0.02d0) c1)
                                                       (list (+ c0 0.02d0) (+ c1 0.02d0))
                                                       (list c0 (+ c1 0.02d0)) (list c0 c1)))))))
                  ;; --- insert (timed) ---
                  (let ((start (get-internal-real-time)) (done 0))
                    (loop while (< done n-points) do
                      (with-transaction ()
                        (dotimes (_ (min batch (- n-points done)))
                          (make-geos-place :geom (%rand-point lo-lon lo-lat span))))
                      (incf done (min batch (- n-points done))))
                    (let ((done2 0))
                      (loop while (< done2 n-polygons) do
                        (with-transaction ()
                          (dotimes (_ (min batch (- n-polygons done2)))
                            (make-geos-place :geom (%rand-polygon lo-lon lo-lat span 0.002d0))))
                        (incf done2 (min batch (- n-polygons done2)))))
                    (setf (getf results :insert-ms) (%ms-since start)
                          (getf results :n-nodes) (+ n-points n-polygons)))
                  ;; --- queries (timed) ---
                  (let ((s (get-internal-real-time)))
                    (setf (getf results :within-exact-n) (length (find-nodes-within 'geos-place aoi :graph g))
                          (getf results :within-exact-ms) (%ms-since s)))
                  (let ((s (get-internal-real-time)))
                    (setf (getf results :intersecting-exact-n)
                          (length (find-nodes-intersecting 'geos-place aoi :graph g))
                          (getf results :intersecting-exact-ms) (%ms-since s)))
                  (without-geos
                    (let ((s (get-internal-real-time)))
                      (setf (getf results :within-fallback-n) (length (find-nodes-within 'geos-place aoi :graph g))
                            (getf results :within-fallback-ms) (%ms-since s))))
                  (format t "~&=== graph-db/geos perf (~D points + ~D polygons) ===~%"
                          n-points n-polygons)
                  (format t "  index insert:        ~,1F ms (~,1F k nodes/s)~%"
                          (getf results :insert-ms)
                          (/ (getf results :n-nodes) (max 1d-3 (getf results :insert-ms))))
                  (format t "  find-within  (GEOS): ~,2F ms -> ~D hits~%"
                          (getf results :within-exact-ms) (getf results :within-exact-n))
                  (format t "  find-within  (fallback/centroid): ~,2F ms -> ~D hits~%"
                          (getf results :within-fallback-ms) (getf results :within-fallback-n))
                  (format t "  find-intersecting (GEOS): ~,2F ms -> ~D hits~%"
                          (getf results :intersecting-exact-ms) (getf results :intersecting-exact-n))
                  results)
             (ignore-errors (close-graph g :snapshot-p nil))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
