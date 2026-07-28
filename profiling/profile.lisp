;;;; SB-PROFILE Deterministic Tracing Wrapper for VivaceGraph Profiler
;;;;
;;;; NOTE ON GENERIC FUNCTIONS: this wrapper used to filter out every generic
;;;; function, on the apparent assumption that SB-PROFILE cannot trace them.  It
;;;; can -- it encapsulates the GF and reports calls aggregated over its methods.
;;;; The old filter removed the engine's hottest entry points (SERIALIZE,
;;;; DESERIALIZE, DESERIALIZE-HELP, LOOKUP-VERTEX, GET-BYTES, NODE-GEOMETRY and
;;;; every GEOS topology operation, ~461 symbols in all), which meant the
;;;; "serialization" workloads could not observe serialization at all.
(in-package #:graph-db/profiler)

(defstruct profile-entry
  (name "" :type string)
  (calls 0 :type fixnum)
  (seconds 0.0d0 :type double-float)
  (sec-per-call 0.0d0 :type double-float)
  (bytes 0 :type fixnum)
  (bytes-per-call 0.0d0 :type double-float))

(defstruct profile-result
  (entries '() :type list))

;;; Display helpers.
;;;
;;; SB-PROFILE reports seconds-per-call, and for the functions that matter most
;;; that number is on the order of 1e-7 -- so a "~,6F" seconds column printed
;;; 0.000000 for nearly every row and threw away the entire measurement.  The
;;; stored values stay in seconds (that is what SB-PROFILE gives us, and it is
;;; the honest unit for the total); presentation converts.

(defun profile-entry-usec-per-call (entry)
  "Microseconds per call.

Derived from TOTAL seconds / calls in preference to SB-PROFILE's own sec/call
column.  That column is rendered at six decimal places in the text report, so
any function faster than 1 us -- which is most of the hot ones -- arrives here
already rounded to exactly zero, and no change of display unit can recover it.
The seconds column is rendered at three decimals, so for a function called tens
of thousands of times it carries far more signal: 0.008 s over 91,200 calls is
0.088 us/call, where the sec/call column simply said 0.

Falls back to the reported per-call figure when the total rounds to zero, which
is the opposite case -- a handful of calls too fast to register in the totals
but still resolvable at microsecond granularity."
  (let ((calls (profile-entry-calls entry)))
    (if (and (plusp calls) (plusp (profile-entry-seconds entry)))
        (/ (* 1.0d6 (profile-entry-seconds entry)) calls)
        (* 1.0d6 (profile-entry-sec-per-call entry)))))

(defun profile-entry-total-ms (entry)
  "Total milliseconds attributed to ENTRY."
  (* 1.0d3 (profile-entry-seconds entry)))

(defun format-usec (usec)
  "Render USEC with a precision that keeps small values legible.
Sub-microsecond costs are exactly where the old seconds column collapsed to
zero, so they get more decimals, not fewer."
  (cond ((zerop usec) "0")
        ((< usec 1.0d0) (format nil "~,3F" usec))
        ((< usec 100.0d0) (format nil "~,2F" usec))
        ((< usec 100000.0d0) (format nil "~,1F" usec))
        (t (format nil "~:D" (round usec)))))

(defun format-bytes (bytes)
  "Render BYTES as a human-scaled string (B / KB / MB / GB)."
  (let ((b (float bytes 1.0d0)))
    (cond ((< b 1024) (format nil "~D B" (round b)))
          ((< b 1048576) (format nil "~,1F KB" (/ b 1024)))
          ((< b 1073741824) (format nil "~,1F MB" (/ b 1048576)))
          (t (format nil "~,2F GB" (/ b 1073741824))))))

(defparameter *profile-excluded-packages*
  '("COMMON-LISP" "BORDEAUX-THREADS" "SB-PROFILE" "SB-SPROF" "SB-IMPL" "SB-INT"
    "SB-KERNEL" "SB-C" "SB-VM" "SB-THREAD" "GRAPH-DB/PROFILER")
  "Never profile symbols homed in these packages.  Instrumenting CL or the
profiler's own machinery either explodes the run time or recurses.")

(defparameter *profile-excluded-names*
  '("LOCK" "UNLOCK" "GRAB-LOCK" "RELEASE-LOCK")
  "Symbol names never profiled, by name, in any package.

Encapsulating the locking primitives adds a function-call-plus-bookkeeping
layer INSIDE critical sections.  That does not just distort the numbers, it
changes contention behaviour, which is the very thing the transaction workloads
are trying to measure.")

(defun profile-candidate-p (sym)
  "True if SYM should be handed to SB-PROFILE."
  (and (profileable-symbol-p sym)
       (let ((pkg (symbol-package sym)))
         (and pkg
              (not (member (package-name pkg) *profile-excluded-packages*
                           :test #'string=))))
       (not (member (symbol-name sym) *profile-excluded-names* :test #'string=))))

(defun %install-sb-profile (symbols)
  "Instrument SYMBOLS with SB-PROFILE.  Returns the list actually instrumented.

Uses SB-PROFILE::PROFILE-1-FUN when available: the public PROFILE macro would
otherwise require one EVAL (and one compile) per symbol, which is measurable
overhead when instrumenting a thousand-plus functions."
  #+sbcl
  (let ((installed '())
        (fn (and (find-package "SB-PROFILE")
                 (let ((s (find-symbol "PROFILE-1-FUN" "SB-PROFILE")))
                   (and s (fboundp s) (fdefinition s))))))
    (dolist (sym symbols installed)
      (handler-case
          (progn
            (if fn (funcall fn sym) (eval `(sb-profile:profile ,sym)))
            (push sym installed))
        (error () nil))))
  #-sbcl (declare (ignore symbols))
  #-sbcl '())

;;; ---------------------------------------------------------------------------
;;; Raw counter collection (preferred over parsing the text report)
;;;
;;; SB-PROFILE keeps per-function counters in *PROFILED-FUN-NAME->INFO*, each
;;; PROFILE-INFO carrying a READ-STATS-FUN that returns
;;; (values calls ticks consing gc-time).  Reading those directly avoids the
;;; text report's rounding entirely: REPORT prints seconds at three decimals and
;;; sec/call at six, so any function faster than a microsecond -- most of the hot
;;; ones -- was printed as 0.000000 and its per-call cost was simply gone.
;;;
;;; INTERNAL-TIME-UNITS-PER-SECOND is 1,000,000 on SBCL, so a tick IS a
;;; microsecond; no precision is lost on the way in either.
;;; ---------------------------------------------------------------------------

(defun %profile-stats-table ()
  "SB-PROFILE's per-function info table, or NIL if this SBCL does not expose it."
  (let ((sym (and (find-package "SB-PROFILE")
                  (find-symbol "*PROFILED-FUN-NAME->INFO*" "SB-PROFILE"))))
    (and sym (boundp sym) (symbol-value sym))))

(defun %profile-stats-reader ()
  "SB-PROFILE's PROFILE-INFO-READ-STATS-FUN accessor, or NIL."
  (let ((sym (and (find-package "SB-PROFILE")
                  (find-symbol "PROFILE-INFO-READ-STATS-FUN" "SB-PROFILE"))))
    (and sym (fboundp sym) (fdefinition sym))))

(defun %profile-name-string (name)
  "Render a profiled function NAME package-qualified, as the text report does.
Bound to CL-USER so GRAPH-DB symbols print with their package prefix."
  (let ((*package* (or (find-package "COMMON-LISP-USER") *package*))
        (*print-readably* nil)
        (*print-pretty* nil))
    (prin1-to-string name)))

(defun collect-sb-profile-stats ()
  "Per-function PROFILE-ENTRYs read straight from SB-PROFILE's counters.

Returns NIL when the internals are unavailable, in which case the caller falls
back to parsing (SB-PROFILE:REPORT).  Must be called BEFORE UNPROFILE, which
discards the counters."
  (let ((tbl (%profile-stats-table))
        (reader (%profile-stats-reader))
        (entries '()))
    (when (and tbl reader)
      (maphash
       (lambda (name info)
         (ignore-errors
          (multiple-value-bind (calls ticks consing gc) (funcall (funcall reader info))
            (declare (ignore gc))
            (let ((calls (or calls 0))
                  (ticks (or ticks 0))
                  (consing (or consing 0)))
              (when (plusp calls)
                (let ((secs (/ (float ticks 1.0d0) internal-time-units-per-second)))
                  (push (make-profile-entry
                         :name (%profile-name-string name)
                         :calls calls
                         :seconds secs
                         :sec-per-call (/ secs calls)
                         :bytes consing
                         :bytes-per-call (/ (float consing 1.0d0) calls))
                        entries)))))))
       tbl)
      (sort entries #'> :key #'profile-entry-seconds))))

;;; ---------------------------------------------------------------------------
;;; Instrumentation-overhead accounting
;;;
;;; SB-PROFILE encapsulates every function it traces, so a function called a
;;; million times pays a million encapsulations.  Past some call volume the
;;; measurement costs more than the thing measured, and the reported time is
;;; mostly the profiler observing itself.
;;;
;;; This is not hypothetical.  Profiling vivace-graph's slot-access path
;;; (persistent-p / meta-p / ephemeral-p, ~1M calls each) attributed 3,214 ms
;;; across the traced functions in a workload that takes 887 ms uninstrumented
;;; -- i.e. more "measured" time than real time, which is impossible as cost and
;;; briefly produced a wrong bug report.  A profiler that can mislead this way
;;; must say so itself rather than relying on the reader to notice.
;;;
;;; SB-PROFILE's own SB-PROFILE::*OVERHEAD* is not usable for this: its :CALL
;;; slot (~6e-9 s) measures the TIMER read, not the encapsulation, and
;;; understates real per-call cost by roughly two orders of magnitude.  So we
;;; measure encapsulation empirically instead.
;;; ---------------------------------------------------------------------------

(defvar *sb-profile-call-overhead-seconds* nil
  "Measured per-call cost of SB-PROFILE encapsulation, in seconds.
NIL until MEASURE-SB-PROFILE-OVERHEAD has run; it is measured once and cached.")

(defparameter *overhead-warn-fraction* 0.25d0
  "Flag a profile entry when estimated instrumentation overhead reaches this
fraction of the time attributed to it.")

(defun %overhead-probe (x) (1+ x))

(defun measure-sb-profile-overhead (&key (iterations 200000) (force nil))
  "Measure and cache per-call SB-PROFILE encapsulation overhead, in seconds.

Times a trivial function with and without instrumentation and attributes the
difference to encapsulation.  Deliberately measured in THIS image, since the
cost depends on the host and the SBCL build."
  #+sbcl
  (when (or force (null *sb-profile-call-overhead-seconds*))
    (ignore-errors
     (let ((f #'%overhead-probe))
       ;; Unprofiled baseline.
       (ignore-errors (sb-profile:unprofile %overhead-probe))
       (let ((t0 (get-internal-real-time)))
         (dotimes (i iterations) (funcall f i))
         (let* ((bare (- (get-internal-real-time) t0)))
           (eval '(sb-profile:profile %overhead-probe))
           (let ((g (fdefinition '%overhead-probe))
                 (t1 (get-internal-real-time)))
             (dotimes (i iterations) (funcall g i))
             (let ((wrapped (- (get-internal-real-time) t1)))
               (ignore-errors (sb-profile:unprofile %overhead-probe))
               (ignore-errors (sb-profile:reset))
               (setf *sb-profile-call-overhead-seconds*
                     (max 0.0d0
                          (/ (- (float wrapped 1.0d0) (float bare 1.0d0))
                             internal-time-units-per-second
                             iterations))))))))))
  *sb-profile-call-overhead-seconds*)

(defun profile-entry-overhead-ms (entry)
  "Estimated milliseconds of ENTRY's reported time that is instrumentation."
  (let ((per-call (or *sb-profile-call-overhead-seconds* 0.0d0)))
    (* 1.0d3 per-call (profile-entry-calls entry))))

(defun profile-entry-overhead-fraction (entry)
  "Estimated instrumentation share of ENTRY's reported time, 0.0-1.0+.
Returns 0 when overhead has not been measured."
  (let ((total (profile-entry-total-ms entry)))
    (if (or (null *sb-profile-call-overhead-seconds*) (<= total 0.0d0))
        0.0d0
        (/ (profile-entry-overhead-ms entry) total))))

(defun profile-entry-overhead-suspect-p (entry)
  "True when ENTRY's reported time is materially instrumentation, not workload.
Such a row's TIME must not be quoted as a cost; its CALL COUNT is still exact."
  (>= (profile-entry-overhead-fraction entry) *overhead-warn-fraction*))

(defun profile-result-overhead-warnings (result &optional wall-clock-ms)
  "Human-readable warnings about instrumentation distortion in RESULT.
Returns a list of strings, empty when the run looks trustworthy."
  (let* ((entries (profile-result-entries result))
         (suspects (remove-if-not #'profile-entry-overhead-suspect-p entries))
         (attributed (reduce #'+ entries :key #'profile-entry-total-ms :initial-value 0.0d0))
         (warnings '()))
    (when suspects
      (push (format nil
                    "~D of ~D traced function(s) have an estimated instrumentation share >= ~D%~
~%    (highest: ~{~A~^, ~}).  Their CALL COUNTS are exact; their TIMES are not a cost."
                    (length suspects) (length entries)
                    (round (* 100 *overhead-warn-fraction*))
                    (mapcar #'profile-entry-name
                            (subseq (sort (copy-list suspects) #'>
                                          :key #'profile-entry-overhead-fraction)
                                    0 (min 3 (length suspects)))))
            warnings))
    (when (and wall-clock-ms (plusp wall-clock-ms) (> attributed wall-clock-ms))
      (push (format nil
                    "Attributed time (~,1F ms) EXCEEDS this run's wall clock (~,1F ms).  That is ~
impossible as real cost:~%    the run is dominated by instrumentation.  Re-run with a narrower ~
:SUBSYSTEMS set, or use sb-sprof."
                    attributed wall-clock-ms)
            warnings))
    (nreverse warnings)))

(defun parse-integer-clean (str)
  "Parse integer after removing commas or spaces."
  (let ((clean (ppcre:regex-replace-all "[,\\s]" str "")))
    (if (zerop (length clean)) 0 (parse-integer clean))))

(defun parse-sb-profile-report-string (report-str)
  "Parse text output from (sb-profile:report) into a PROFILE-RESULT object."
  (let ((entries '()))
    (with-input-from-string (s report-str)
      (loop for line = (read-line s nil nil) while line do
        (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
          ;; 6-column format: "seconds | gc | consed | calls | sec/call | name"
          (ppcre:register-groups-bind (sec gc consed calls spc fname)
              ("^\\s*([0-9.]+)\\s+\\|\\s*([0-9.]+)\\s+\\|\\s*([0-9,]+)\\s+\\|\\s*([0-9,]+)\\s+\\|\\s*([0-9.]+)\\s+\\|\\s*(.+)$" trimmed)
            (declare (ignore gc))
            (let* ((sec-val (parse-float:parse-float sec :type 'double-float))
                   (calls-val (parse-integer-clean calls))
                   (spc-val (parse-float:parse-float spc :type 'double-float))
                   (bytes-val (parse-integer-clean consed))
                   (bpc-val (if (plusp calls-val) (float (/ bytes-val calls-val) 1.0d0) 0.0d0)))
              (push (make-profile-entry
                     :name (string-trim '(#\Space) fname)
                     :calls calls-val
                     :seconds sec-val
                     :sec-per-call spc-val
                     :bytes bytes-val
                     :bytes-per-call bpc-val)
                    entries))))))
    (make-profile-result :entries (nreverse entries))))

(defmacro with-sb-profile-tracing ((function-symbols &key (reset t)) &body body)
  "Profile the specified FUNCTION-SYMBOLS with SB-PROFILE during BODY execution.
Generic functions are included.  Returns (values RESULT PROFILE-RESULT)."
  #+sbcl
  `(let* ((valid-syms (remove-if-not #'profile-candidate-p ,function-symbols))
          (report-str nil)
          (raw-entries nil)
          (body-result nil))
     (ignore-errors (sb-profile:unprofile))
     ;; Measure encapsulation overhead BEFORE installing our own trace set --
     ;; the probe profiles and unprofiles a function of its own, so it must not
     ;; run while the real set is installed.  Cached after the first call.
     (ignore-errors (measure-sb-profile-overhead))
     (%install-sb-profile valid-syms)
     (when ,reset
       (ignore-errors (sb-profile:reset)))
     (unwind-protect
          (setf body-result (progn ,@body))
       ;; Read the raw counters FIRST -- both REPORT and UNPROFILE can disturb
       ;; them, and the raw numbers carry precision the report has already
       ;; rounded away.
       (setf raw-entries (ignore-errors (collect-sb-profile-stats)))
       (unless raw-entries
         (setf report-str (ignore-errors
                            (with-output-to-string (s)
                              (let ((*trace-output* s)
                                    (*standard-output* s))
                                (sb-profile:report))))))
       (ignore-errors (sb-profile:unprofile)))
     (values body-result
             (if raw-entries
                 (make-profile-result :entries raw-entries)
                 (parse-sb-profile-report-string (or report-str "")))))
  #-sbcl
  `(values (progn ,@body) (make-profile-result :entries '())))
