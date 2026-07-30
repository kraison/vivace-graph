;;;; Master suite, shared fixtures, and schema for ACID compliance tests.

(in-package #:graph-db/acid-test)

;;; ---------------------------------------------------------------------------
;;; Suite root
;;; ---------------------------------------------------------------------------

(def-suite acid-suite
  :description "ACID compliance tests for graph-db.")

(defun run-acid-tests ()
  "Run the ACID suite.  Returns T on all-pass.
Called by (asdf:test-system :graph-db/acid-test)."
  (log:config :error)
  (let ((results (run 'acid-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Temp-directory helpers
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
  (let ((dir (merge-pathnames (format nil "graph-db-acid-~A/" (scratch-tag))
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
;;; Thread runner (same pattern as tests/concurrency/helpers.lisp)
;;; ---------------------------------------------------------------------------

(defun run-threads (n fn &key (timeout 30))
  "Spawn N threads, release them simultaneously, join within TIMEOUT seconds."
  (let* ((captured-graph *graph*)
         (arrival  (make-semaphore))
         (go-gate  (make-semaphore))
         (done     (make-semaphore))
         (errors   (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((idx i))
        (make-thread
         (lambda ()
           (let ((*graph* captured-graph))
             (unwind-protect
                  (handler-case
                      (progn
                        (signal-semaphore arrival)
                        (wait-on-semaphore go-gate)
                        (funcall fn idx))
                    (error (e)
                      (setf (aref errors idx) e)))
               (signal-semaphore done))))
         :name (format nil "acid-test-~D" i))))
    (dotimes (_ n)
      (unless (wait-on-semaphore arrival :timeout 5)
        (error "run-threads: thread did not start within 5 s")))
    (signal-semaphore go-gate :count n)
    (let* ((itps  (float internal-time-units-per-second 1.0d0))
           (start (get-internal-real-time))
           (budget (* timeout itps)))
      (dotimes (_ n)
        (let* ((elapsed   (- (get-internal-real-time) start))
               (remaining (/ (max 0 (- budget elapsed)) itps)))
          (unless (wait-on-semaphore done :timeout remaining)
            (error "run-threads: timed out after ~As (deadlock?)" timeout)))))
    (dotimes (i n)
      (when (aref errors i)
        (error "Thread ~D signaled: ~A" i (aref errors i))))))

;;; ---------------------------------------------------------------------------
;;; Schema
;;; ---------------------------------------------------------------------------

(defparameter *acid-graph-name* :graph-db-acid-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *acid-graph-name* *schema-node-metadata*) nil))

(def-vertex ac-item ()
  ((value)
   (label))
  :graph-db-acid-test)

(def-edge ac-link ()
  ()
  :graph-db-acid-test)

;;; def-view must run inside a live graph; call define-acid-views after make-graph.
(defun define-acid-views ()
  (def-view ac-item-by-value :lessp (ac-item :graph-db-acid-test)
    (:map (lambda (item)
            (yield (slot-value item 'value) t)))))

;;; ---------------------------------------------------------------------------
;;; Graph fixture
;;; ---------------------------------------------------------------------------

(defmacro with-acid-graph ((g) &body body)
  "Bind G and *GRAPH* to a fresh on-disk acid-test graph; tear it down after."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *acid-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                (define-acid-views)
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))
