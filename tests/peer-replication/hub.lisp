;;;; Peer-replication test -- HUB process.
;;;;
;;;; Driven by run-peer-test.sh.  Reads REPL_HUB_DIR, REPL_PORT and REPL_WORK
;;;; from the environment, builds a hub graph, registers the device with an
;;;; authority scope rooted at the site, and runs the scenario:
;;;;
;;;;   PHASE 1 (seed): commit a site -> survey -> {find1, find2} graph, with
;;;;     find2 NOT disclosable.  The device pulls its closed disclosable subgraph
;;;;     (site, survey, find1 + the two connecting edges; find2 + its edge omitted).
;;;;   PHASE 2 (purge): flip find1 to non-disclosable.  On the device's next pull
;;;;     the manifest diff PURGES find1 + its edge (scope EXIT).
;;;;
;;;; Peer replication can't be tested in one image (process-global *graphs*,
;;;; schema registry, *graph*), so the hub and device run as two OS processes
;;;; talking over a loopback socket -- exactly like tests/replication/.
;;;; Coordination is via flag files under REPL_WORK.

(require :asdf)

;;; Disable the interactive debugger (see device.lisp for the rationale): print
;;; the condition + a best-effort backtrace, then quit non-zero, so an unhandled
;;; error surfaces legibly instead of hanging or cascading.  (mine-action item 2.)
(flet ((bail (condition)
         (format *error-output* "~&=== UNHANDLED ~S ===~%~A~%"
                 (type-of condition) condition)
         (ignore-errors
           #+sbcl (sb-debug:print-backtrace :stream *error-output* :count 40)
           #+ecl  (si::tpl-backtrace))
         (ignore-errors (finish-output *error-output*))
         #+sbcl (sb-ext:exit :code 70 :abort t)
         #+ecl  (ext:quit 70)
         #+ccl  (ccl:quit 70)
         #-(or sbcl ecl ccl) (uiop:quit 70)))
  (setf *debugger-hook* (lambda (c hook) (declare (ignore hook)) (bail c)))
  #+ecl (setf ext:*invoke-debugger-hook*
              (lambda (c hook) (declare (ignore hook)) (bail c))))

(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(with-open-file (s (merge-pathnames "build.log"
                                    (or (uiop:getenv "REPL_WORK") #p"/tmp/"))
                   :direction :output
                   :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* s) (*error-output* s))
    (ql:quickload :graph-db :silent t)))
(in-package :graph-db)
(log:config :error)

(defun hflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (hflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (hflag name)) (return t))
    (sleep 0.1)))
(defun hexit (code) #+sbcl (sb-ext:exit :code code)
                    #+ccl (ccl:quit code)
                    #+ecl (ext:quit code)
                    #-(or sbcl ccl ecl) (uiop:quit code))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (let* ((dir  (uiop:getenv "REPL_HUB_DIR"))
           (port (parse-integer (uiop:getenv "REPL_PORT")))
           (g (make-graph :peer-test-app dir
                          :peer-role :hub
                          :replication-port port
                          :replication-key "peer-secret"
                          :export-predicate #'peer-test-disclosable
                          :buffer-pool-size 1000)))
      (let ((*graph* g)
            site)
        ;; PHASE 1 payload: site -> survey -> {find1 (open), find2 (closed)}.
        (with-transaction ()
          (let* ((s   (make-p-site   :name "Site-1"   :disclosable 1))
                 (sv  (make-p-survey :name "Survey-1" :disclosable 1))
                 (f1  (make-p-find   :name "Find-1"   :disclosable 1))
                 (f2  (make-p-find   :name "Find-2"   :disclosable 0)))  ; withheld
            (make-p-has-survey :from s  :to sv)
            (make-p-has-find   :from sv :to f1)
            (make-p-has-find   :from sv :to f2)
            (setq site s)))
        (format t "~&HUB: phase-1 committed (site, survey, find1 open, find2 closed)~%")
        (finish-output)
        ;; Register the device's authority scope, rooted at the site.
        (register-peer-device g :origin-id *device-origin*
                                :roots (list (id site)) :scope :hma)
        (write-flag "ready")

        ;; Wait for the device to pull + verify phase 1.
        (unless (wait-flag "phase1-verified")
          (format t "~&HUB: timed out waiting for phase-1 verify~%") (hexit 1))

        ;; PHASE 2: flip Find-1 to non-disclosable -> it leaves the device's scope.
        (flet ((by-name (n)
                 (first (remove-if-not
                         (lambda (x) (string= n (slot-value x 'name)))
                         (map-vertices #'identity g :collect-p t :vertex-type 'p-find)))))
          (with-transaction ()
            (let ((v (copy (by-name "Find-1"))))
              (setf (slot-value v 'disclosable) 0)
              (save v))))
        (format t "~&HUB: phase-2 committed (Find-1 -> non-disclosable)~%")
        (finish-output)
        (write-flag "phase2-ready")

        ;; Stay alive until the device finishes, then close cleanly.
        (wait-flag "device-done")
        (close-graph g :snapshot-p nil)
        (format t "~&HUB: closed~%") (finish-output)
        (hexit 0)))
  (error (c)
    (format t "~&HUB ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "ready"))       ; unblock the device so it can fail fast
    (hexit 1)))
