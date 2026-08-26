;;;; check-perf comparison-logic tests (GH #253).  Pure report-file
;;;; comparison only -- no benchmark runs.  Loaded into graph-db/test.

(in-package #:graph-db/test)

(def-suite perf-check-suite :in graph-db-suite
  :description "check-perf / bless-perf-baseline gating logic.")
(in-suite perf-check-suite)

(defvar *pc-gen* graph-db/perf-test:*perf-suite-generation*)

(defun %pc-write-report (dir name &key (gen *pc-gen*) (host "test-host")
                                       (scale :normal) (tag name) entries)
  "Fabricate a perf report file; ENTRIES = list of (label . plist)."
  (let ((path (merge-pathnames (format nil "~A.report" name) dir)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s ";;;; graph-db perf report (fabricated for tests)~%~%")
      (format s "~S~%" (list :perf-report :tag tag :impl "TEST" :scale scale
                             :generation gen :host host :entries entries)))
    path))

(defparameter *pc-base-entries*
  '(("insert-vertices" :ops 100 :seconds 1.0 :ops/s 1000)
    ("commit-multigraph-n8" :ops 100 :seconds 1.0 :ops/s 500 :us/commit 640.0)
    ("heap-used-after-inserts" :bytes 100000 :per-op 5)
    ("snapshot" :seconds 2.0)))

(test check-perf-identical-reports-pass
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
          (c (%pc-write-report d "cand" :entries *pc-base-entries*)))
      (multiple-value-bind (pass failures)
          (graph-db/perf-test:check-perf c :baseline b)
        (is-true pass)
        (is (null failures))))))

(test check-perf-throughput-regress-fails
  ;; :ops/s drop of 30% > 15% band.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "insert-vertices" cand :test #'equal)))
      (setf (getf (cdr row) :ops/s) 700)
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (multiple-value-bind (pass failures)
            (graph-db/perf-test:check-perf c :baseline b)
          (is-false pass)
          (is (= 1 (length failures)))
          (destructuring-bind (label metric bv cv pct) (first failures)
            (is (equal "insert-vertices" label))
            (is (eq :ops/s metric))
            (is (= 1000 bv))
            (is (= 700 cv))
            (is (< 29.0 pct 31.0))))))))

(test check-perf-latency-regress-fails
  ;; :us/commit is the primary metric where present; a 30% rise fails
  ;; even though :ops/s is unchanged.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "commit-multigraph-n8" cand :test #'equal)))
      (setf (getf (cdr row) :us/commit) 832.0)
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (multiple-value-bind (pass failures)
            (graph-db/perf-test:check-perf c :baseline b)
          (is-false pass)
          (is (eq :us/commit (second (first failures)))))))))

(test check-perf-improvement-passes
  ;; A big :ops/s gain and a :seconds drop are not regressions.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*)))
      (setf (getf (cdr (assoc "insert-vertices" cand :test #'equal))
                  :ops/s)
            2000)
      (setf (getf (cdr (assoc "snapshot" cand :test #'equal)) :seconds) 1.0)
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (is-true (graph-db/perf-test:check-perf c :baseline b))))))

(test check-perf-missing-label-fails
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
          (c (%pc-write-report d "cand"
                               :entries (remove "snapshot" *pc-base-entries*
                                                :key #'car :test #'equal))))
      (multiple-value-bind (pass failures)
          (graph-db/perf-test:check-perf c :baseline b)
        (is-false pass)
        (is (equal '("snapshot" :missing)
                   (subseq (first failures) 0 2)))))))

(test check-perf-new-label-not-a-failure
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
          (c (%pc-write-report
              d "cand"
              :entries (append *pc-base-entries*
                               '(("brand-new-bench" :ops 10 :seconds 0.1
                                  :ops/s 100))))))
      (multiple-value-bind (pass failures)
          (graph-db/perf-test:check-perf c :baseline b)
        (is-true pass)
        (is (null failures))))))

(test check-perf-collapse-with-lost-primary-fails
  ;; The candidate row loses the baseline's primary metric (:us/commit)
  ;; while :ops/s collapses 100x.  The fallback must catch it -- this
  ;; scenario silently PASSED before the comparison-metric fallback.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "commit-multigraph-n8" cand :test #'equal)))
      (setf (cdr row) '(:ops 100 :seconds 100.0 :ops/s 5))
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (multiple-value-bind (pass failures)
            (graph-db/perf-test:check-perf c :baseline b)
          (is-false pass)
          (is (= 1 (length failures)))
          (is (eq :ops/s (second (first failures)))))))))

(test check-perf-all-metrics-vanished-fails
  ;; A candidate row sharing NO comparable metric with the baseline row
  ;; is a failure, never a silent skip.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "commit-multigraph-n8" cand :test #'equal)))
      (setf (cdr row) '(:emitted 100))
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (multiple-value-bind (pass failures)
            (graph-db/perf-test:check-perf c :baseline b)
          (is-false pass)
          (is (equal '("commit-multigraph-n8" :metric-vanished)
                     (subseq (first failures) 0 2))))))))

(test check-perf-bytes-rise-fails-drop-passes
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*)))
      (flet ((with-bytes (v)
               (let* ((e (copy-tree *pc-base-entries*))
                      (row (assoc "heap-used-after-inserts" e
                                  :test #'equal)))
                 (setf (getf (cdr row) :bytes) v)
                 e)))
        (multiple-value-bind (pass failures)
            (graph-db/perf-test:check-perf
             (%pc-write-report d "rise" :entries (with-bytes 130000))
             :baseline b)
          (is-false pass)
          (is (eq :bytes (second (first failures)))))
        (is-true (graph-db/perf-test:check-perf
                  (%pc-write-report d "drop" :entries (with-bytes 70000))
                  :baseline b))))))

(test check-perf-latency-improvement-passes
  ;; A large :us/commit drop is an improvement, not a regression.
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "commit-multigraph-n8" cand :test #'equal)))
      (setf (getf (cdr row) :us/commit) 320.0)
      (is-true (graph-db/perf-test:check-perf
                (%pc-write-report d "cand" :entries cand)
                :baseline (%pc-write-report d "base"
                                            :entries *pc-base-entries*))))))

(test check-perf-primary-metric-override
  ;; With ~4 ops/s one integer step is 25% quantization noise; the
  ;; per-label primary override gates :seconds instead.
  (with-temp-directory (d)
    (let* ((base '(("index-fullscan-eq" :ops 200 :seconds 50.0 :ops/s 4)))
           (cand '(("index-fullscan-eq" :ops 200 :seconds 51.0 :ops/s 3)))
           (b (%pc-write-report d "base" :entries base))
           (c (%pc-write-report d "cand" :entries cand)))
      ;; default primary would be :ops/s: 4 -> 3 = 25% "regression"
      (multiple-value-bind (pass failures)
          (graph-db/perf-test:check-perf c :baseline b)
        (declare (ignore failures))
        (is-true pass)))))

(test check-perf-stampless-report-refused
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
          (c (%pc-write-report d "cand" :host nil
                               :entries *pc-base-entries*)))
      (signals error (graph-db/perf-test:check-perf c :baseline b)))))

(test check-perf-generation-mismatch-refused
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :gen (1- *pc-gen*)
                               :entries *pc-base-entries*))
          (c (%pc-write-report d "cand" :entries *pc-base-entries*)))
      (signals error (graph-db/perf-test:check-perf c :baseline b)))))

(test check-perf-host-mismatch-refused
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :host "other-host"
                               :entries *pc-base-entries*))
          (c (%pc-write-report d "cand" :entries *pc-base-entries*)))
      (signals error (graph-db/perf-test:check-perf c :baseline b)))))

(test check-perf-scale-mismatch-refused
  (with-temp-directory (d)
    (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
          (c (%pc-write-report d "cand" :scale :small
                               :entries *pc-base-entries*)))
      (signals error (graph-db/perf-test:check-perf c :baseline b)))))

(test check-perf-per-label-override
  ;; A 25% drop on a label with a 30% override passes; the same drop on
  ;; an un-overridden label fails.
  (with-temp-directory (d)
    (let* ((base '(("noisy-bench" :ops 100 :seconds 1.0 :ops/s 1000)
                   ("quiet-bench" :ops 100 :seconds 1.0 :ops/s 1000)))
           (cand '(("noisy-bench" :ops 100 :seconds 1.0 :ops/s 750)
                   ("quiet-bench" :ops 100 :seconds 1.0 :ops/s 750)))
           (graph-db/perf-test:*perf-tolerance-overrides*
             '(("noisy-bench" . 0.30)))
           (b (%pc-write-report d "base" :entries base))
           (c (%pc-write-report d "cand" :entries cand)))
      (multiple-value-bind (pass failures)
          (graph-db/perf-test:check-perf c :baseline b)
        (is-false pass)
        (is (= 1 (length failures)))
        (is (equal "quiet-bench" (first (first failures))))))))

(test check-perf-error-p-signals
  (with-temp-directory (d)
    (let* ((cand (copy-tree *pc-base-entries*))
           (row (assoc "insert-vertices" cand :test #'equal)))
      (setf (getf (cdr row) :ops/s) 100)
      (let ((b (%pc-write-report d "base" :entries *pc-base-entries*))
            (c (%pc-write-report d "cand" :entries cand)))
        (signals graph-db/perf-test:perf-regression-error
          (graph-db/perf-test:check-perf c :baseline b :error-p t))))))

(test bless-refuses-wrong-generation-and-scale
  ;; Baseline dir bound to scratch so even a broken refusal cannot
  ;; strand a file in the source tree.
  (with-temp-directory (d)
    (let ((graph-db/perf-test::*perf-results-directory* d)
          (old (%pc-write-report d "old" :gen (1- *pc-gen*)
                                 :entries *pc-base-entries*))
          (small (%pc-write-report d "small" :scale :small
                                   :entries *pc-base-entries*)))
      (signals error (graph-db/perf-test:bless-perf-baseline old))
      (signals error (graph-db/perf-test:bless-perf-baseline small)))))

(test bless-writes-sanitized-baseline
  (with-temp-directory (d)
    (let* ((graph-db/perf-test::*perf-results-directory* d)
           (r (%pc-write-report d "good" :host "Test_Host.9"
                                :entries *pc-base-entries*))
           (target (graph-db/perf-test:bless-perf-baseline r)))
      (is (equal (format nil "baseline-test-host-9-g~D" *pc-gen*)
                 (pathname-name target)))
      (is-true (probe-file target)))))
