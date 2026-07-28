;;;; Package definition for graph-db/profiler
(in-package :cl-user)

(defpackage #:graph-db/profiler
  (:use #:cl #:alexandria)
  (:export
   ;; Registry & Subsystem Selection
   #:*subsystem-registry*
   #:*subsystem-aliases*
   #:*profiled-packages*
   #:*auto-refresh-registry*
   #:*hot-path-function-names*
   #:register-subsystem-functions
   #:get-subsystem-functions
   #:resolve-subsystem-key
   #:list-subsystems
   #:all-subsystems
   #:profileable-symbol-p
   #:classify-symbol-subsystem
   #:populate-all-graph-db-functions
   #:populate-hot-path-subsystem
   #:init-default-subsystem-registry
   #:refresh-subsystem-registry
   #:subsystem-coverage-report

   ;; SB-PROFILE instrumentation controls
   #:*profile-excluded-packages*
   #:*profile-excluded-names*
   #:profile-candidate-p

   ;; Result Data Structures
   #:sprof-sample-entry
   #:make-sprof-sample-entry
   #:sprof-sample-entry-p
   #:sprof-sample-entry-name
   #:sprof-sample-entry-self-samples
   #:sprof-sample-entry-self-pct
   #:sprof-sample-entry-total-samples
   #:sprof-sample-entry-total-pct

   #:sprof-result
   #:make-sprof-result
   #:sprof-result-p
   #:sprof-result-mode
   #:sprof-result-total-samples
   #:sprof-result-entries
   #:sprof-result-filtered-rows
   #:sprof-result-filtered-samples
   #:*sprof-harness-frames*
   #:internal-symbol-p

   #:profile-entry
   #:make-profile-entry
   #:profile-entry-p
   #:profile-entry-name
   #:profile-entry-calls
   #:profile-entry-seconds
   #:profile-entry-sec-per-call
   #:profile-entry-bytes
   #:profile-entry-bytes-per-call

   #:*sb-profile-call-overhead-seconds*
   #:*overhead-warn-fraction*
   #:measure-sb-profile-overhead
   #:profile-entry-overhead-ms
   #:profile-entry-overhead-fraction
   #:profile-entry-overhead-suspect-p
   #:profile-result-overhead-warnings
   #:profile-entry-usec-per-call
   #:profile-entry-total-ms
   #:format-usec
   #:format-bytes
   #:profile-result
   #:make-profile-result
   #:profile-result-p
   #:profile-result-entries

   #:profiler-run-result
   #:make-profiler-run-result
   #:profiler-run-result-p
   #:profiler-run-result-name
   #:profiler-run-result-subsystems
   #:profiler-run-result-real-time-ms
   #:profiler-run-result-run-time-ms
   #:profiler-run-result-bytes-consed
   #:profiler-run-result-gc-time-ms
   #:profiler-run-result-sprof
   #:profiler-run-result-profile

   ;; Core Profiling Operations & Macros
   #:with-sprof-profiling
   #:with-sb-profile-tracing
   #:profile-block
   #:profile-subsystem
   #:print-profiler-run-summary

   ;; Subsystem Profiling Modules
   #:profile-mmap-subsystem
   #:profile-serialization-subsystem
   #:profile-index-subsystem
   #:profile-graph-subsystem
   #:profile-transactions-subsystem
   #:profile-views-subsystem
   #:profile-spatial-subsystem
   #:profile-prolog-subsystem

   ;; Suite Execution & Results
   #:profiling-suite-result
   #:make-profiling-suite-result
   #:profiling-suite-result-p
   #:profiling-suite-result-timestamp
   #:profiling-suite-result-runs
   #:run-full-profiling-suite
   #:print-profiling-suite-summary

   ;; Stage 3 PDF & Visual Reporting Suite
   #:generate-pdf-report
   #:profile-and-generate-pdf

   ;; Real-World Cross-Subsystem Workload Profiling
   #:realworld-workload-result
   #:make-realworld-workload-result
   #:realworld-workload-result-p
   #:realworld-workload-result-name
   #:realworld-workload-result-description
   #:realworld-workload-result-target-subsystems
   #:realworld-workload-result-code-sample
   #:realworld-workload-result-run-result
   #:profile-realworld-ingestion-workload
   #:profile-realworld-spatial-traversal-workload
   #:profile-realworld-view-rollup-workload
   #:profile-realworld-prolog-inference-workload
   #:profile-realworld-concurrent-transactions-workload
   #:profile-realworld-complex-serialization-workload
   #:profile-realworld-geos-coverage-workload
   #:profile-realworld-large-polygon-materialization-workload
   #:profile-realworld-control-history-workload
   #:profile-realworld-acled-pin-workload
   #:profile-realworld-kb-vector-search-workload
   #:run-real-world-profiling-suite
   #:profile-and-generate-realworld-pdf))


