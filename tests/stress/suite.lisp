;;;; Stress-test suite root: fixtures, scale control, timing instrumentation.

(in-package #:graph-db/stress-test)

;;; ---------------------------------------------------------------------------
;;; Suite root
;;; ---------------------------------------------------------------------------

(def-suite stress-suite
  :description "Single-threaded scale and correctness stress tests for graph-db.")

(defun run-stress-tests ()
  "Run the stress suite.  Returns T on all-pass.
Called by (asdf:test-system :graph-db/stress-test)."
  (log:config :error)
  (let ((results (run 'stress-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Scale control
;;;
;;; :small  — 10x smaller volumes for CI / fast feedback
;;; :normal — full volumes; ~5 min total
;;; ---------------------------------------------------------------------------

(defparameter *stress-scale* :normal)

(defun scale (normal-value &optional (small-value (max 1 (floor normal-value 10))))
  "Return NORMAL-VALUE or SMALL-VALUE depending on *STRESS-SCALE*."
  (ecase *stress-scale*
    (:normal normal-value)
    (:small  small-value)))

;;; ---------------------------------------------------------------------------
;;; Implementation identification (captured once at load time for reports)
;;; ---------------------------------------------------------------------------

(defparameter *lisp-impl*
  (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))

;;; ---------------------------------------------------------------------------
;;; Timing instrumentation
;;;
;;; Active only when *COLLECT-TIMINGS* is true (off by default so stress
;;; tests can be run as pure correctness checks).  When active, each call
;;; to WITH-TIMING accumulates a (label . plist) entry in *TIMING-REPORT*.
;;; record-throughput produces a human-readable summary line as well.
;;; ---------------------------------------------------------------------------

(defparameter *collect-timings* nil)

(defvar *timing-report* nil
  "Alist of (label . (:ops N :seconds S :impl IMPL)).  Populated by record-throughput.")

(defmacro with-timing ((label) &body body)
  "Execute BODY; when *COLLECT-TIMINGS*, record wall-clock elapsed time
under LABEL.  Always evaluates BODY exactly once."
  (let ((start (gensym "START"))
        (end   (gensym "END")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (when *collect-timings*
           (let ((,end (get-internal-real-time)))
             (let ((elapsed (/ (- ,end ,start)
                               (float internal-time-units-per-second))))
               (pushnew (list ,label :seconds elapsed :impl *lisp-impl*)
                        *timing-report* :key #'car :test #'equal))))))))

(defun record-throughput (label ops elapsed)
  "Record OPS operations completing in ELAPSED seconds under LABEL.
Prints a summary line and pushes structured data into *TIMING-REPORT*."
  (when *collect-timings*
    (let ((ops/s (if (zerop elapsed) 0 (round (/ ops elapsed)))))
      (format t "~&~A: ~D ops in ~,2Fs (~:D ops/sec) [~A]~%"
              label ops elapsed ops/s *lisp-impl*)
      (pushnew (list label :ops ops :seconds elapsed :ops/s ops/s :impl *lisp-impl*)
               *timing-report* :key #'car :test #'equal))))

;;; ---------------------------------------------------------------------------
;;; Temp-directory helpers (same pattern as concurrency suite)
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
  (let ((dir (merge-pathnames (format nil "graph-db-stress-~A/" (scratch-tag))
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

;;; ---------------------------------------------------------------------------
;;; Graph fixture
;;; ---------------------------------------------------------------------------

(defparameter *stress-graph-name* :graph-db-stress-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *stress-graph-name* *schema-node-metadata*) nil))

(def-vertex s-item ()
  ((value)
   (label))
  :graph-db-stress-test)

(def-vertex s-widget ()
  ((name)
   (score))
  :graph-db-stress-test)

(def-vertex s-thing ()
  ((tag))
  :graph-db-stress-test)

(def-edge s-link ()
  ()
  :graph-db-stress-test)

;; :UNIQUE-constrained + :INDEX-ed types for the unique-stress / index-stress tests.
(def-vertex su-item ()
  ((ukey :unique t)
   (label))
  :graph-db-stress-test)

(def-vertex si-item ()
  ((ikey :index t)
   (label))
  :graph-db-stress-test)

(defmacro with-stress-graph ((g) &body body)
  "Bind G and *GRAPH* to a fresh on-disk graph; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *stress-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 2000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Storage-layer fixtures
;;; ---------------------------------------------------------------------------

(defmacro with-stress-lhash ((var &rest make-args) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (make-lhash :location ,dir ,@make-args)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-lhash ,var)))))))

(defmacro with-stress-memory ((var &key (size '(* 1024 1024 256))) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (create-memory
                    (namestring (merge-pathnames "heap.dat" ,dir))
                    ,size)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-memory ,var)))))))

(defun make-stress-integer-skip-list (heap &key duplicates-allowed-p)
  (make-skip-list :heap heap
                  :head-key most-negative-fixnum
                  :head-value 0
                  :tail-key most-positive-fixnum
                  :tail-value 0
                  :key-equal '=
                  :key-comparison '<
                  :key-serializer 'serialize
                  :key-deserializer 'deserialize
                  :value-serializer 'serialize
                  :value-deserializer 'deserialize
                  :value-equal 'equal
                  :duplicates-allowed-p duplicates-allowed-p))
