;;;; Profiling Suite Controller & Suite Results Runner
(in-package #:graph-db/profiler)

(defstruct profiling-suite-result
  (timestamp "" :type string)
  (runs '() :type list))

(defun get-iso-timestamp ()
  "Generate ISO-8601 timestamp string."
  (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mon day hr min sec)))

(defun run-full-profiling-suite (&key (subsystems :all) (sprof-mode :cpu) (scale 1.0))
  "Execute the full VivaceGraph profiling suite across all modules (or selected SUBSYSTEMS list).
SCALE adjusts the iteration work scale factor (default 1.0)."
  (let ((all-targets (if (eq subsystems :all)
                         '(:mmap :serialization :index :graph :transactions :views :spatial :prolog)
                         subsystems))
        (runs '()))
    
    (format t "~%========================================================================~%")
    (format t "STARTING VIVACE-GRAPH PERFORMANCE PROFILING SUITE (~A)~%" (get-iso-timestamp))
    (format t "Selected Subsystem Modules: ~{~A~^, ~}~%" all-targets)
    (format t "========================================================================~%")

    (dolist (mod all-targets)
      (format t "~&---> Profiling module ~A...~%" mod)
      (let ((res
              (case mod
                (:mmap (profile-mmap-subsystem :iterations (round (* 5000 scale)) :sprof-mode sprof-mode))
                (:serialization (profile-serialization-subsystem :iterations (round (* 5000 scale)) :sprof-mode sprof-mode))
                (:index (profile-index-subsystem :count (round (* 1000 scale)) :sprof-mode sprof-mode))
                (:graph (profile-graph-subsystem :vertex-count (round (* 500 scale)) :sprof-mode sprof-mode))
                (:transactions (profile-transactions-subsystem :tx-count (round (* 300 scale)) :sprof-mode sprof-mode))
                (:views (profile-views-subsystem :count (round (* 500 scale)) :sprof-mode sprof-mode))
                (:spatial (profile-spatial-subsystem :point-count (round (* 500 scale)) :sprof-mode sprof-mode))
                (:prolog (profile-prolog-subsystem :query-count (round (* 2000 scale)) :sprof-mode sprof-mode))
                (otherwise nil))))
        (when res
          (push res runs))))

    (let ((suite-res (make-profiling-suite-result
                      :timestamp (get-iso-timestamp)
                      :runs (nreverse runs))))
      (print-profiling-suite-summary suite-res)
      suite-res)))

(defun print-profiling-suite-summary (suite &optional (stream *standard-output*))
  "Print a clean human-readable comparative summary of all runs in SUITE."
  (format stream "~%========================================================================~%")
  (format stream "VIVACE-GRAPH PROFILING SUITE COMPARATIVE REPORT~%")
  (format stream "Timestamp: ~A~%" (profiling-suite-result-timestamp suite))
  (format stream "========================================================================~%")
  (format stream "  Module / Profile Run           | Real Time (ms) | CPU Time (ms) | Memory Consed~%")
  (format stream "------------------------------------------------------------------------~%")
  (dolist (run (profiling-suite-result-runs suite))
    (format stream "  ~30A | ~14,2F | ~13,2F | ~10,2F MB~%"
            (subseq (profiler-run-result-name run) 0 (min 30 (length (profiler-run-result-name run))))
            (profiler-run-result-real-time-ms run)
            (profiler-run-result-run-time-ms run)
            (/ (profiler-run-result-bytes-consed run) (* 1024.0 1024.0))))
  (format stream "========================================================================~%~%")
  suite)
