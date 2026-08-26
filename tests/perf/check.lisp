;;;; Perf regression gating: blessed baselines + check-perf (GH #253).
;;;;
;;;; Baselines are per-host AND per-suite-generation artifacts (the 3.0
;;;; A/B rule: never compare across either).  A blessed baseline lives in
;;;; tests/perf/results/baseline-<host>-g<generation>.report and is
;;;; committed via a reviewed diff, never automatically.

(in-package #:graph-db/perf-test)

(defparameter *perf-tolerance* 0.15
  "Default allowed regression fraction per metric (GH #253).  Generous on
purpose; tighten per-label as variance.py data accumulates.")

(defparameter *perf-tolerance-overrides*
  ;; v5scan f0/f10 rows swing ~17-20% run-to-run at :normal scale (the
  ;; GH #244 evidence table + this branch's bless/check runs); widen
  ;; those alone rather than the global band.  f50 rows are calm.
  '(("v5scan-f0-s1" . 0.30)
    ("v5scan-f0-s2" . 0.30)
    ("v5scan-f0-s4" . 0.30)
    ("v5scan-f0-s8" . 0.30)
    ("v5scan-f10-s1" . 0.30)
    ("v5scan-f10-s2" . 0.30)
    ("v5scan-f10-s4" . 0.30)
    ("v5scan-f10-s8" . 0.30))
  "Alist of (label . tolerance) for known-noisy benches.  An entry here
takes precedence over CHECK-PERF's :TOLERANCE argument for its label.")

(defun label-tolerance (label &optional (default *perf-tolerance*))
  (or (cdr (assoc label *perf-tolerance-overrides* :test #'equal))
      default))

;;; ---------------------------------------------------------------------------
;;; Metric classes
;;; ---------------------------------------------------------------------------

(defparameter +metric-preference+ '(:us/commit :us/edge :ops/s :bytes :seconds)
  "Comparable metric keys, most preferred first.")

(defparameter *perf-primary-metric-overrides*
  ;; index-fullscan-eq records ~4 ops/s: one integer step is 25%, pure
  ;; quantization noise.  Gate its :seconds instead.
  '(("index-fullscan-eq" . :seconds))
  "Alist of (label . metric) forcing the compared metric for a label.")

(defun primary-metric (plist)
  "The most preferred metric key PLIST records, or NIL."
  (find-if (lambda (m) (getf plist m)) +metric-preference+))

(defun comparison-metric (label bplist cplist)
  "The metric CHECK-PERF compares for LABEL: the per-label override or
the baseline's primary metric, falling back down +METRIC-PREFERENCE+ to
the first key BOTH rows record with a nonzero numeric baseline value.
NIL if no such key is shared.  Latency metrics outrank :ops/s (matches
the units the #237/#244 evidence tables were argued in); either
direction is monotone-equivalent."
  (let ((primary (or (cdr (assoc label *perf-primary-metric-overrides*
                                 :test #'equal))
                     (primary-metric bplist))))
    (when primary
      (loop for m in (cons primary (remove primary +metric-preference+))
            for bv = (getf bplist m)
            for cv = (getf cplist m)
            when (and (numberp bv) (not (zerop bv)) (numberp cv))
              do (return m)))))

(defun metric-regression (metric baseline current)
  "Signed regression fraction: positive = worse.  :ops/s regresses on a
drop; latency/bytes metrics regress on a rise."
  (if (eq metric :ops/s)
      (/ (- baseline current) baseline)
      (/ (- current baseline) baseline)))

;;; ---------------------------------------------------------------------------
;;; Baseline convention + blessing
;;; ---------------------------------------------------------------------------

(defparameter *perf-results-directory* nil
  "Override for the baseline directory (tests bind it to scratch);
NIL = tests/perf/results/ in the source tree.")

(defun results-directory ()
  (or *perf-results-directory*
      (asdf:system-relative-pathname :graph-db "tests/perf/results/")))

(defun sanitize-host (name)
  "Lowercase NAME, [a-z0-9-] only."
  (string-downcase
   (substitute-if #\- (lambda (c) (not (or (alphanumericp c) (char= c #\-))))
                  name)))

(defun baseline-pathname (host generation)
  (merge-pathnames (format nil "baseline-~A-g~D.report"
                           (sanitize-host host) generation)
                   (results-directory)))

(defun report-meta (report key) (getf (cdr report) key))

(defun bless-perf-baseline (report-file)
  "Copy REPORT-FILE to the blessed baseline path for its host + the
CURRENT suite generation.  Refuses a wrong-generation or non-:normal
report.  The copy is deliberate state: commit it via a reviewed diff."
  (let* ((report (read-perf-report report-file))
         (gen (report-meta report :generation))
         (host (report-meta report :host))
         (scale (report-meta report :scale)))
    (unless report
      (error "~A is not a perf report file." report-file))
    (unless (eql gen *perf-suite-generation*)
      (error "Refusing to bless ~A: report generation ~S /= current ~S."
             report-file gen *perf-suite-generation*))
    (unless host
      (error "Refusing to bless ~A: no host stamp (pre-generation report)."
             report-file))
    (unless (eq scale :normal)
      (error "Refusing to bless ~A: scale ~S; baselines are :normal only."
             report-file scale))
    (let ((target (baseline-pathname host gen)))
      (uiop:copy-file report-file target)
      (format t "~&Blessed baseline: ~A~%" target)
      (format t "~&Reminder: a blessed baseline is a reviewed diff -- ~
                 commit it deliberately.~%")
      target)))

;;; ---------------------------------------------------------------------------
;;; check-perf
;;; ---------------------------------------------------------------------------

(define-condition perf-regression-error (error)
  ((failures :initarg :failures :reader perf-regression-failures))
  (:report (lambda (c s)
             (format s "check-perf: ~D metric~:P regressed past tolerance."
                     (length (perf-regression-failures c))))))

(defun %require-comparable (base cand baseline-path report-path)
  "Signal unless BASE and CAND share generation, host and scale.  Always
an error -- comparing incomparable reports is a setup mistake, not a perf
result."
  (flet ((need (key)
           (let ((b (report-meta base key)) (c (report-meta cand key)))
             (unless (equalp b c)
               (error "check-perf: ~(~A~) mismatch: baseline ~A has ~S, ~
                       report ~A has ~S.  Baselines are per-host, ~
                       per-generation, per-scale artifacts."
                      key baseline-path b report-path c)))))
    (need :generation)
    (need :host)
    (need :scale)))

(defun %fail-row (label key status)
  (format t "~&~38A ~10A ~12A ~12A ~9A ~6A ~A~%"
          label key "-" "-" "-" "-" status))

(defun check-perf (report-file &key baseline (tolerance *perf-tolerance*)
                                    error-p)
  "Compare REPORT-FILE against the blessed baseline for its host and
generation (or an explicit :BASELINE file).  Returns (values pass-p
failures); failures = list of (label metric baseline current pct).  A
label present in the baseline but missing from the report fails with
metric :MISSING; a label whose shared comparable metrics all vanished
fails with :METRIC-VANISHED.  New unbaselined labels are reported, not
failed.  :TOLERANCE replaces the 15% default; *PERF-TOLERANCE-OVERRIDES*
entries take precedence over it per label.  Never signals on regression
unless :ERROR-P is true; incomparable reports (host/generation/scale
mismatch, or a stampless pre-#253 report) always signal."
  (let ((cand (read-perf-report report-file)))
    (unless cand
      (error "~A is not a perf report file." report-file))
    (unless (and (report-meta cand :generation) (report-meta cand :host))
      (error "check-perf: ~A lacks generation/host stamps (pre-#253 report ~
              format).  Re-run run-perf on current sources." report-file))
    (let ((bpath (or baseline
                     (baseline-pathname (report-meta cand :host)
                                        (report-meta cand :generation)))))
      (unless (probe-file bpath)
        (error "check-perf: no blessed baseline at ~A.  Run run-perf at ~
                :normal on this host and bless-perf-baseline the report."
               bpath))
      (let ((base (read-perf-report bpath)))
        (%require-comparable base cand bpath report-file)
        (let ((be (getf (cdr base) :entries))
              (ce (getf (cdr cand) :entries))
              (failures '())
              (new-labels '()))
          (format t "~&=== check-perf: ~A  vs baseline ~A ===~%"
                  (report-meta cand :tag) (report-meta base :tag))
          (format t "~&~38A ~10A ~12A ~12A ~9A ~6A ~A~%"
                  "metric" "key" "baseline" "current" "regress%" "tol%"
                  "status")
          (format t "~&(regress% is signed toward worse: a drop for ~
                     ops/s, a rise for us/*, bytes, seconds)~%")
          (dolist (b be)
            (let* ((label (car b))
                   (c (assoc label ce :test #'equal))
                   (tol (label-tolerance label tolerance)))
              (cond
                ((null c)
                 (push (list label :missing nil nil nil) failures)
                 (%fail-row label :missing "FAIL (bench vanished)"))
                ;; baseline row records nothing comparable: nothing to gate
                ((null (primary-metric (cdr b))))
                (t
                 (let ((metric (comparison-metric label (cdr b) (cdr c))))
                   (if (null metric)
                       (progn
                         (push (list label :metric-vanished
                                     (primary-metric (cdr b)) nil nil)
                               failures)
                         (%fail-row label :vanished
                                    "FAIL (no shared metric)"))
                       (let* ((bv (getf (cdr b) metric))
                              (cv (getf (cdr c) metric))
                              (reg (metric-regression metric bv cv))
                              (bad (> reg tol)))
                         (when bad
                           (push (list label metric bv cv (* 100.0 reg))
                                 failures))
                         (format t "~&~38A ~10A ~12A ~12A ~8,1F% ~5D% ~A~%"
                                 label metric bv cv (* 100.0 reg)
                                 (round (* 100 tol))
                                 (if bad "FAIL" "ok")))))))))
          (dolist (c ce)
            (unless (assoc (car c) be :test #'equal)
              (push (car c) new-labels)
              (format t "~&~38A ~10A ~12A ~12A ~9A ~6A ~A~%"
                      (car c) (or (primary-metric (cdr c)) "-") "-"
                      (let ((m (primary-metric (cdr c))))
                        (if m (getf (cdr c) m) "-"))
                      "-" "-" "new, unbaselined")))
          (setf failures (nreverse failures))
          (if failures
              (format t "~&=== check-perf: FAIL (~D regression~:P)"
                      (length failures))
              (format t "~&=== check-perf: PASS"))
          (when new-labels
            (format t "; ~D new unbaselined label~:P" (length new-labels)))
          (format t " ===~%")
          (when (and error-p failures)
            (error 'perf-regression-error :failures failures))
          (values (null failures) failures))))))
