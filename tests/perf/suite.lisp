;;;; Performance benchmark suite — harness, schema, report I/O.
;;;;
;;;; This is MEASUREMENT, not pass/fail.  run-perf executes a fixed set of
;;;; benchmarks, records each result into *perf-report*, writes a report file
;;;; (lisp-readable + a human table), and returns T.  compare-perf diffs two
;;;; report files (e.g., experiment-baseline vs an MVCC phase).
;;;;
;;;; Reuses the stress-suite patterns (scale knob, temp dirs, *lisp-impl*).

(in-package #:graph-db/perf-test)

;;; ---------------------------------------------------------------------------
;;; Scale + environment
;;; ---------------------------------------------------------------------------

(defparameter *perf-scale* :normal
  ":normal — authoritative baseline sizes.  :small — ~10x smaller, quick loop.")

(defun scale (normal-value &optional (small-value (max 1 (floor normal-value 10))))
  (ecase *perf-scale* (:normal normal-value) (:small small-value)))

(defparameter *lisp-impl*
  (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))

;;; ---------------------------------------------------------------------------
;;; Report collection
;;; ---------------------------------------------------------------------------

(defvar *perf-report* nil
  "List of (label . plist) entries.  plist keys: :ops :seconds :ops/s :bytes.")

(defun reset-perf-report () (setf *perf-report* nil))

(defun record (label &rest plist)
  "Record one metric entry (last write wins for a given label)."
  (setf *perf-report* (remove label *perf-report* :key #'car :test #'equal))
  (push (cons label plist) *perf-report*)
  ;; echo a readable line as we go
  (format t "~&  ~38A ~{~A ~A  ~}~%" label plist)
  (finish-output))

(defun record-throughput (label ops elapsed)
  (let ((ops/s (if (zerop elapsed) 0 (round (/ ops elapsed)))))
    (record label :ops ops :seconds (float-3 elapsed) :ops/s ops/s)))

(defun float-3 (x) (/ (fround (* x 1000)) 1000.0))

(defmacro timed-ops ((label ops-form) &body body)
  "Run BODY, time it (wall clock), and record-throughput LABEL over OPS-FORM ops."
  (let ((start (gensym)) (ops (gensym)) (elapsed (gensym)))
    `(let ((,start (get-internal-real-time))
           (,ops ,ops-form))
       (progn ,@body)
       (let ((,elapsed (/ (- (get-internal-real-time) ,start)
                          (float internal-time-units-per-second))))
         (record-throughput ,label ,ops ,elapsed)
         ,elapsed))))

(defmacro timed-seconds ((label) &body body)
  "Run BODY, record its wall-clock seconds under LABEL."
  (let ((start (gensym)) (elapsed (gensym)))
    `(let ((,start (get-internal-real-time)))
       (progn ,@body)
       (let ((,elapsed (/ (- (get-internal-real-time) ,start)
                          (float internal-time-units-per-second))))
         (record ,label :seconds (float-3 ,elapsed))
         ,elapsed))))

;;; ---------------------------------------------------------------------------
;;; Temp dirs / gc (same pattern as the stress suite)
;;; ---------------------------------------------------------------------------

;; SBCL's initial *RANDOM-STATE* is a fixed constant, so an unseeded (RANDOM ...)
;; produces the SAME name sequence in every image: two concurrent suite runs on a
;; shared host would share -- and delete -- each other's scratch dirs.  Seed from
;; entropy, lazily so a dumped image reseeds in each new process, and add a
;; counter so one image can never repeat a name either.
(defvar *scratch-random-state* nil)
(defvar *scratch-counter* 0)

(defun scratch-tag ()
  "A name fragment unique across concurrent processes and across calls."
  (unless *scratch-random-state*
    (setf *scratch-random-state* (make-random-state t)))
  (format nil "~36R-~36R"
          (random (expt 36 12) *scratch-random-state*)
          (incf *scratch-counter*)))

(defun make-temp-directory ()
  (let ((dir (merge-pathnames (format nil "graph-db-perf-~A/" (scratch-tag))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl  (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl  (ext:gc t))

(defun file-size (path)
  "Size of PATH in bytes, or 0 if missing."
  (if (probe-file path)
      (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))
      0))

;;; ---------------------------------------------------------------------------
;;; Perf schema (own types so we don't perturb other suites)
;;; ---------------------------------------------------------------------------

(defparameter *perf-graph-name* :graph-db-perf-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-graph-name* *schema-node-metadata*) nil))

(def-vertex p-node ()
  ((val)
   (label))
  :graph-db-perf-test)

(def-edge p-knows ()
  ()
  :graph-db-perf-test)

;; A :UNIQUE-constrained type and an :INDEX-ed type for the unique / general-index
;; benchmarks (bench-unique / bench-index).  Separate types so they don't perturb
;; the other benchmarks' node path.
(def-vertex pu-node ()
  ((uval :unique t)
   (label))
  :graph-db-perf-test)

(def-vertex pi-node ()
  ((ival :index t)
   (label))
  :graph-db-perf-test)

(defun define-perf-views ()
  "Define the perf view against the current *graph* (call after make-graph)."
  (def-view p-node-by-val :lessp (p-node :graph-db-perf-test)
    (:map (lambda (n)
            (when (slot-value n 'val)
              (yield (slot-value n 'val) nil))))))

(defmacro with-perf-graph ((g &key (dir nil dir-supplied) (views nil)) &body body)
  "Bind G + *GRAPH* to a fresh on-disk perf graph.  With :VIEWS t, define the
perf view (so view benchmarks have an index to maintain/query); default NIL so
write-path benchmarks measure the node path WITHOUT view-maintenance cost (the
MVCC-relevant signal).  If :DIR is given, bind it to the graph's temp dir."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-graph-name* (namestring ,d) :buffer-pool-size 4000))
             ,@(when dir-supplied `((,dir (namestring ,d)))))
         (declare (ignorable ,g ,@(when dir-supplied (list dir))))
         (unwind-protect
              (let ((*graph* ,g))
                (when ,views (define-perf-views))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Report I/O
;;; ---------------------------------------------------------------------------

(defun report-pathname (tag)
  (merge-pathnames
   (format nil "gdb-perf-~A-~D.report" tag (get-universal-time))
   (uiop:temporary-directory)))

(defun write-perf-report (path &key (tag "perf"))
  "Write *perf-report* to PATH: a human header/table + a lisp-readable form."
  (let ((entries (reverse *perf-report*)))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (format s ";;;; graph-db perf report~%")
      (format s ";;;; tag=~A impl=~A scale=~A~%~%" tag *lisp-impl* *perf-scale*)
      (dolist (e entries)
        (format s ";; ~38A ~{~A ~A  ~}~%" (car e) (cdr e)))
      (format s "~%~S~%"
              (list :perf-report :tag tag :impl *lisp-impl* :scale *perf-scale*
                    :entries entries)))
    (format t "~&~%Wrote perf report: ~A~%" path)
    path))

(defun read-perf-report (path)
  "Return the (:perf-report ...) plist read from PATH."
  (with-open-file (s path)
    (loop for form = (read s nil :eof)
          until (eq form :eof)
          when (and (consp form) (eq (car form) :perf-report))
            do (return form))))

(defun compare-perf (baseline-path candidate-path)
  "Print per-label deltas between two perf report files (candidate vs baseline)."
  (let* ((base (read-perf-report baseline-path))
         (cand (read-perf-report candidate-path))
         (be (getf (cdr base) :entries))
         (ce (getf (cdr cand) :entries)))
    (format t "~&=== perf compare: ~A  ->  ~A ===~%"
            (getf (cdr base) :tag) (getf (cdr cand) :tag))
    (format t "~&~38A ~12A ~12A ~10A~%" "metric" "baseline" "candidate" "delta%")
    (dolist (b be)
      (let* ((label (car b))
             (c (assoc label ce :test #'equal))
             (bk (cond ((getf (cdr b) :ops/s) :ops/s)
                       ((getf (cdr b) :bytes) :bytes)
                       ((getf (cdr b) :seconds) :seconds)
                       (t nil))))
        (when (and c bk)
          (let* ((bv (getf (cdr b) bk))
                 (cv (getf (cdr c) bk))
                 (delta (if (and (numberp bv) (numberp cv) (not (zerop bv)))
                            (* 100.0 (/ (- cv bv) bv))
                            0)))
            (format t "~&~38A ~12A ~12A ~+,1F%  (~A)~%"
                    label bv cv delta bk)))))))
