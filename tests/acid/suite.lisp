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
  ;; Type-ids come from the system-wide registry, so every store this suite
  ;; opens needs a system directory (GH #186).  One for the whole run, which
  ;; is the shape a real system has: many stores, one registry.
  (let* ((system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'acid-suite)))
           (explain! results)
           (results-status results))
      ;; system-dir and all test scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))

;;; ---------------------------------------------------------------------------
;;; Temp-directory helpers
;;; ---------------------------------------------------------------------------

(defun make-temp-directory ()
  "A fresh scratch dir under the shared per-run parent (GH #214)."
  (graph-db-test-scratch:make-scratch-directory "graph-db-acid"))

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
