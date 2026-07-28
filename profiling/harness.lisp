;;;; Combined Profiling Harness for VivaceGraph Profiler
(in-package #:graph-db/profiler)

(defstruct profiler-run-result
  (name "Profile Run" :type string)
  (subsystems '() :type list)
  (real-time-ms 0.0d0 :type double-float)
  (run-time-ms 0.0d0 :type double-float)
  (bytes-consed 0 :type fixnum)
  (gc-time-ms 0.0d0 :type double-float)
  (sprof nil)
  (profile nil))

(defmacro profile-block ((&key (name "Profile Block")
                               (subsystems '(:all))
                               (sprof-mode :cpu)
                               (sprof-samples 100000)
                               (top-n 20))
                         &body body)
  "Execute BODY with full instrumentation: timing, memory allocation, SB-SPROF
sampling, and SB-PROFILE deterministic function tracing across SUBSYSTEMS."
  (let ((subsys-var (gensym "SUBSYS"))
        (syms-var (gensym "SYMS"))
        (t-start (gensym "TSTART"))
        (c-start (gensym "CSTART"))
        (gc-start (gensym "GCSTART"))
        (t-end (gensym "TEND"))
        (c-end (gensym "CEND"))
        (gc-end (gensym "GCEND"))
        (sprof-var (gensym "SPROF"))
        (prof-var (gensym "PROF"))
        (res-var (gensym "RES")))
    `(let* ((,subsys-var (if (listp ,subsystems) ,subsystems (list ,subsystems)))
            ;; Refresh first: the registry is otherwise a snapshot taken when
            ;; registry.lisp loaded, so any optional system loaded afterwards
            ;; (GRAPH-DB/GEOS in particular) would be silently untraceable.
            (,syms-var (progn
                         (when *auto-refresh-registry* (refresh-subsystem-registry))
                         (let ((acc '()))
                           (dolist (s ,subsys-var acc)
                             (setf acc (union acc (get-subsystem-functions s)))))))
            (,sprof-var nil)
            (,prof-var nil)
            (,res-var nil)
            (,t-start 0) (,c-start 0) (,gc-start 0)
            (,t-end 0) (,c-end 0) (,gc-end 0))
       (declare (ignorable ,res-var))
       
       (setf ,t-start (get-internal-real-time)
             ,c-start (get-internal-run-time)
             ,gc-start #+sbcl sb-ext:*gc-run-time* #-sbcl 0)
       
       (let ((b-start #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))
         (setf (values ,res-var ,prof-var)
               (with-sb-profile-tracing (,syms-var :reset t)
                 (if (eq ,sprof-mode :none)
                     (progn ,@body)
                     (let ((sprof-res (with-sprof-profiling (:mode ,sprof-mode
                                                             :max-samples ,sprof-samples
                                                             :top-n ,top-n)
                                        (progn ,@body))))
                       (setf ,sprof-var sprof-res)
                       ,res-var))))
         
         (setf ,t-end (get-internal-real-time)
               ,c-end (get-internal-run-time)
               ,gc-end #+sbcl sb-ext:*gc-run-time* #-sbcl 0)
         
         (let* ((b-end #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0)
                (bytes-delta (max 0 (- b-end b-start)))
                (real-ms (float (* (/ (- ,t-end ,t-start) internal-time-units-per-second) 1000.0) 1.0d0))
                (run-ms (float (* (/ (- ,c-end ,c-start) internal-time-units-per-second) 1000.0) 1.0d0))
                (gc-ms (float (* (/ (- ,gc-end ,gc-start) internal-time-units-per-second) 1000.0) 1.0d0)))
           
           (make-profiler-run-result
            :name ,name
            :subsystems ,subsys-var
            :real-time-ms real-ms
            :run-time-ms run-ms
            :bytes-consed bytes-delta
            :gc-time-ms gc-ms
            :sprof ,sprof-var
            :profile ,prof-var))))))

(defun profile-subsystem (subsystem-key workload-fn &key (name nil) (sprof-mode :cpu) (iterations 1))
  "Helper function to profile a specific SUBSYSTEM-KEY running WORKLOAD-FN for ITERATIONS."
  (let ((run-name (or name (format nil "Subsystem ~A (~D iter)" subsystem-key iterations))))
    (eval
     `(profile-block (:name ,run-name :subsystems '(,subsystem-key) :sprof-mode ,sprof-mode)
        (dotimes (_ ,iterations)
          (funcall ,workload-fn))))))

(defun print-profiler-run-summary (res &optional (stream *standard-output*))
  "Print a clean human-readable text summary of a PROFILER-RUN-RESULT to STREAM."
  (format stream "~%========================================================================~%")
  (format stream "PROFILER RUN SUMMARY: ~A~%" (profiler-run-result-name res))
  (format stream "Subsystems: ~{~A~^, ~}~%" (profiler-run-result-subsystems res))
  (format stream "========================================================================~%")
  (format stream "Wall-Clock Time:  ~10,3F ms~%" (profiler-run-result-real-time-ms res))
  (format stream "CPU Run Time:     ~10,3F ms~%" (profiler-run-result-run-time-ms res))
  (format stream "GC Time:          ~10,3F ms~%" (profiler-run-result-gc-time-ms res))
  (format stream "Memory Consed:    ~:D bytes (~,2F MB)~%"
          (profiler-run-result-bytes-consed res)
          (/ (profiler-run-result-bytes-consed res) (* 1024.0 1024.0)))
  
  ;; SPROF sampling results
  (let ((sprof (profiler-run-result-sprof res)))
    (when (and sprof (sprof-result-entries sprof))
      (format stream "~%--- Top Functions by Statistical Sampling (sb-sprof :mode ~A) ---~%"
              (sprof-result-mode sprof))
      (when (plusp (sprof-result-filtered-rows sprof))
        (format stream "NOTE: ~:D row(s) carrying ~:D self-sample(s) are not shown.~%"
                (sprof-result-filtered-rows sprof)
                (sprof-result-filtered-samples sprof)))
      (format stream "  Self %  |  Tot %   |  Self Smp  | Function Name~%")
      (format stream "------------------------------------------------------------------------~%")
      (dolist (e (sprof-result-entries sprof))
        (format stream "  ~5,1F%  |  ~5,1F%   |  ~8D  | ~A~%"
                (sprof-sample-entry-self-pct e)
                (sprof-sample-entry-total-pct e)
                (sprof-sample-entry-self-samples e)
                (sprof-sample-entry-name e)))))

  ;; SB-PROFILE deterministic tracing results
  (let ((prof (profiler-run-result-profile res)))
    (when (and prof (profile-result-entries prof))
      (format stream "~%--- Primitive Function Call & Allocation Tracing (sb-profile) ---~%")
      (dolist (w (profile-result-overhead-warnings
                  prof (profiler-run-result-real-time-ms res)))
        (format stream "  !! ~A~%" w))
      (format stream "     Calls |  Total ms |     us/call |      Consed | Bytes/Call | ! | Function Symbol~%")
      (format stream "----------------------------------------------------------------------------------------~%")
      (dolist (e (profile-result-entries prof))
        (format stream "  ~8:D | ~9,3F | ~11@A | ~11@A | ~10:D | ~A | ~A~%"
                (profile-entry-calls e)
                (profile-entry-total-ms e)
                (format-usec (profile-entry-usec-per-call e))
                (format-bytes (profile-entry-bytes e))
                (round (profile-entry-bytes-per-call e))
                (if (profile-entry-overhead-suspect-p e) "!" " ")
                (profile-entry-name e)))
      (when (some #'profile-entry-overhead-suspect-p (profile-result-entries prof))
        (format stream "  ! = time is materially instrumentation overhead; trust the call count, not the time.~%"))))
  (format stream "========================================================================~%~%")
  res)
