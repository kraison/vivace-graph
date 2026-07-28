;;;; Package definition for graph-db/profiler
(in-package :cl-user)

(defpackage #:graph-db/profiler
  (:use #:cl #:alexandria)
  (:export
   ;; Registry & Subsystem Selection
   #:*subsystem-registry*
   #:register-subsystem-functions
   #:get-subsystem-functions
   #:list-subsystems
   #:all-subsystems

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

   #:profile-entry
   #:make-profile-entry
   #:profile-entry-p
   #:profile-entry-name
   #:profile-entry-calls
   #:profile-entry-seconds
   #:profile-entry-sec-per-call
   #:profile-entry-bytes
   #:profile-entry-bytes-per-call

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
   #:print-profiler-run-summary))
