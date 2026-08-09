;;;; SB-SPROF Statistical Profiling Wrapper for VivaceGraph Profiler
;;;;
;;;; FILTERING POLICY -- read before widening the filter again.
;;;;
;;;; A profiler must not silently discard the cost it was asked to measure.  An
;;;; earlier version of this file filtered any frame whose name contained
;;;; "foreign function", plus most SB-* frames, and defaulted that filtering ON.
;;;; The consequences were:
;;;;
;;;;   * The GEOS workload became unmeasurable.  Every GEOS operation is a CFFI
;;;;     call into libgeos_c, so its entire cost appears under "foreign
;;;;     function" -- exactly the rows being dropped.
;;;;   * GC and allocation cost, which surfaces through SB-VM/SB-KERNEL frames,
;;;;     disappeared from :alloc-mode runs.
;;;;   * "REMOVE-IF" was filtered outright, so application uses of a common CL
;;;;     function were hidden because the PROFILER happened to call it.
;;;;
;;;; The filter now removes only the profiler's OWN frames -- machinery that is
;;;; genuinely not part of the workload -- and never foreign calls or runtime
;;;; internals.  Whatever it does drop is COUNTED and reported, so suppression is
;;;; visible instead of silent.
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
  (entries '() :type list)
  ;; How many rows the filter removed, and how many samples they carried.
  ;; Non-zero values mean the table below is not the whole story.
  (filtered-rows 0 :type fixnum)
  (filtered-samples 0 :type fixnum))

(defparameter *sprof-harness-frames*
  '("SB-SPROF" "GRAPH-DB/PROFILER" "top level form" "EVAL-TLF"
    "SB-INT:SIMPLE-EVAL-IN-LEXENV" "SB-IMPL::%SIMPLE-EVAL")
  "Frames belonging to the profiling harness itself, not to the workload.
This list is deliberately short.  Anything that could plausibly be workload cost
-- foreign calls, GC, runtime internals, ordinary CL functions -- must NOT be
listed here.")

(defun internal-symbol-p (name)
  "Return T if NAME is a frame from the profiling harness itself.

Note this does NOT match foreign functions or SBCL runtime internals: those are
frequently where the real cost lives (all GEOS work is foreign, GC shows up as
runtime frames), so hiding them defeats the purpose of sampling."
  (some (lambda (frag) (search frag name :test #'char-equal))
        *sprof-harness-frames*))

(defun parse-sprof-report-string (report-str &key (top-n 30) (filter-internals t))
  "Parse text output from (sb-sprof:report :type :flat) into a SPROF-RESULT object.

FILTER-INTERNALS removes only profiling-harness frames (see INTERNAL-SYMBOL-P).
Whatever is removed is counted into the result's FILTERED-ROWS/FILTERED-SAMPLES
so the caller can tell that rows were suppressed."
  (let ((entries '())
        (total-samples 0)
        (filtered-rows 0)
        (filtered-samples 0))
    (with-input-from-string (s report-str)
      (loop for line = (read-line s nil nil) while line do
        (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
          ;; Match total samples header line
          (when (ppcre:scan "Number of samples:\\s+(\\d+)" trimmed)
            (ppcre:register-groups-bind (n) ("Number of samples:\\s+(\\d+)" trimmed)
              (when n (setf total-samples (parse-integer n)))))
          ;; Match flat report table rows: Nr self-cnt self-% tot-cnt tot-% cumul-cnt cumul-% calls name
          (ppcre:register-groups-bind (nr sc sp tc tp cc cp calls fname)
              ("^\\s*(\\d+)\\s+(\\d+)\\s+([0-9.]+)\\s+(\\d+)\\s+([0-9.]+)\\s+(\\d+)\\s+([0-9.]+)\\s+([0-9-]+)\\s+(.+)$" line)
            (declare (ignore nr cc cp calls))
            (let ((sc-val (parse-integer sc))
                  (sp-val (parse-float:parse-float sp :type 'single-float))
                  (tc-val (parse-integer tc))
                  (tp-val (parse-float:parse-float tp :type 'single-float)))
              (when (or (> sc-val 0) (> tc-val 0))
                (if (and filter-internals (internal-symbol-p fname))
                    (progn (incf filtered-rows)
                           (incf filtered-samples sc-val))
                    (push (make-sprof-sample-entry
                           :name fname
                           :self-samples sc-val
                           :self-pct sp-val
                           :total-samples tc-val
                           :total-pct tp-val)
                          entries))))))))
    (setf entries (nreverse entries))
    (when (and top-n (> (length entries) top-n))
      ;; Truncation is also suppression -- account for it.
      (let ((dropped (subseq entries top-n)))
        (incf filtered-rows (length dropped))
        (incf filtered-samples (reduce #'+ dropped :key #'sprof-sample-entry-self-samples
                                                   :initial-value 0)))
      (setf entries (subseq entries 0 top-n)))
    (make-sprof-result :mode :cpu
                       :total-samples total-samples
                       :entries entries
                       :filtered-rows filtered-rows
                       :filtered-samples filtered-samples)))

(defmacro with-sprof-profiling ((&key (mode :cpu)
                                      (max-samples 1000000)
                                      (sample-interval 0.00005)
                                      (top-n 30)
                                      (filter-internals t))
                                &body body)
  "Execute BODY under SB-SPROF profiling (MODE can be :cpu, :alloc, or :time).
Returns SPROF-RESULT object containing high-level function and method stack samples."
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
     (let ((sprof-res (parse-sprof-report-string out-str :top-n ,top-n :filter-internals ,filter-internals)))
       (setf (sprof-result-mode sprof-res) ,mode)
       sprof-res))
  #-sbcl
  `(progn
     ,@body
     (make-sprof-result :mode ,mode :total-samples 0 :entries '())))
