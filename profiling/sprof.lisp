;;;; SB-SPROF Statistical Profiling Wrapper for VivaceGraph Profiler
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(in-package #:graph-db/profiler)



(defstruct sprof-sample-entry
  (name "" :type string)
  (self-samples 0 :type fixnum)
  (self-pct 0.0 :type single-float)
  (total-samples 0 :type fixnum)
  (total-pct 0.0 :type single-float))

(defstruct sprof-result
  (mode :cpu :type symbol)
  (total-samples 0 :type fixnum)
  (entries '() :type list))

(defun parse-sprof-report-string (report-str &key (top-n 20))
  "Parse text output from (sb-sprof:report :type :flat) into a SPROF-RESULT object."
  (let ((entries '())
        (total-samples 0))
    (with-input-from-string (s report-str)
      (loop for line = (read-line s nil nil) while line do
        (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
          ;; Match total samples header line if present
          (when (or (search "samples" trimmed :test #'char-equal)
                    (search "Samples:" trimmed :test #'char-equal))
            (ppcre:register-groups-bind (n) ("(\\d+)\\s+samples" trimmed)
              (when n (setf total-samples (parse-integer n)))))
          ;; Match flat report table rows: self-cnt self-% tot-cnt tot-% name
          ;; Example format: "  45  15.20   102  34.50   GRAPH-DB::GET-BYTE"
          (ppcre:register-groups-bind (sc sp tc tp fname)
              ("^\\s*(\\d+)\\s+([0-9.]+)\\s+(\\d+)\\s+([0-9.]+)\\s+(.+)$" line)
            (let ((sc-val (parse-integer sc))
                  (sp-val (parse-float:parse-float sp :type 'single-float))
                  (tc-val (parse-integer tc))
                  (tp-val (parse-float:parse-float tp :type 'single-float)))
              (when (> sc-val 0)
                (push (make-sprof-sample-entry
                       :name fname
                       :self-samples sc-val
                       :self-pct sp-val
                       :total-samples tc-val
                       :total-pct tp-val)
                      entries)))))))
    (setf entries (nreverse entries))
    (when (and top-n (> (length entries) top-n))
      (setf entries (subseq entries 0 top-n)))
    (make-sprof-result :mode :cpu
                       :total-samples total-samples
                       :entries entries)))

(defmacro with-sprof-profiling ((&key (mode :cpu)
                                      (max-samples 100000)
                                      (sample-interval 0.001)
                                      (top-n 20))
                                &body body)
  "Execute BODY under SB-SPROF profiling (MODE can be :cpu, :alloc, or :time).
Returns SPROF-RESULT object."
  #+sbcl
  `(let ((out-str nil))
     (sb-sprof:with-profiling (:mode ,mode
                               :max-samples ,max-samples
                               :sample-interval ,sample-interval
                               :report nil)
       (progn ,@body))
     (setf out-str (with-output-to-string (s)
                     (let ((*standard-output* s))
                       (sb-sprof:report :type :flat))))
     (let ((sprof-res (parse-sprof-report-string out-str :top-n ,top-n)))
       (setf (sprof-result-mode sprof-res) ,mode)
       sprof-res))
  #-sbcl
  `(progn
     ,@body
     (make-sprof-result :mode ,mode :total-samples 0 :entries '())))

