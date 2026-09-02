;;;; Peer-replication PUSH conflict test -- HUB process (B2d-2b).
;;;;
;;;; Driven by run-push-test.sh.  The hub is the system of record: it
;;;; commits a record (alert DANGER, note "hub-note"), the device pulls it,
;;;; edits it locally (releases the alert to SAFE + changes the note), and
;;;; PUSHES the two authored ops.  The hub RE-HOMES them through the merge
;;;; resolver (§5):
;;;;   ALERT: a release (dangerous -> SAFE) is REJECTED -- the hub keeps
;;;;          DANGER and surfaces a conflict.
;;;;   NOTE:   :lww -- the device's edit (higher lamport) wins.
;;;; The hub then verifies its own merged state (it is the process that re-homes; the
;;;; device can't read the hub graph, and the loser's own copy stays SAFE until B3).
;;;;
;;;; Two OS processes over a loopback socket (peer replication can't share one image).

(require :asdf)

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
;; Provenance: prove WHICH tree this process loaded (GH #260).
(format t "~&SOURCE: ~A~%" (asdf:system-source-directory :graph-db))
(finish-output)

;;; Type-ids come from the image-level registry in *SYSTEM-DIRECTORY* (GH
;;; #186), so this process needs one before it opens anything.  Its OWN,
;;; under REPL_WORK: these harnesses exist because hub and device are
;;; separate IMAGES, and a shared registry would quietly undo that.  Both
;;; ends evaluate one schema.lisp in one order, so their registries agree
;;; and the handshake's registry check (D15) passes -- which is the point.
;;;
;;; SETF, not a LET around the body: replication runs on threads that do
;;; not inherit dynamic bindings.
(setf *system-directory*
      (namestring
       (ensure-directories-exist
        (merge-pathnames
         "system-hub/"
         ;; Trailing slash: REPL_WORK has none, and MERGE-PATHNAMES
         ;; would otherwise treat its last component as a file name.
         (format nil "~A/" (or (uiop:getenv "REPL_WORK") "/tmp"))))))
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

(defvar *fails* 0)
(defun check (ok fmt &rest args)
  (if ok
      (format t "~&  ok   ~?~%" fmt args)
      (progn (incf *fails*) (format t "~&  FAIL ~?~%" fmt args)))
  (finish-output))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (let* ((dir  (uiop:getenv "REPL_HUB_DIR"))
           (port (parse-integer (uiop:getenv "REPL_PORT")))
           (g (make-graph :push-test-app dir
                          :peer-role :hub
                          :replication-port port
                          :replication-key "peer-secret"
                          :export-predicate #'push-disclosable
                          :merge-policy (push-merge-policy)
                          :buffer-pool-size 1000)))
      (let ((*graph* g) rec)
        (with-transaction ()
          (setq rec (make-pf-record :name "Rec-1" :note "hub-note"
                                   :alert "DANGER" :disclosable 1)))
        (format t "~&HUB: committed Rec-1 (alert DANGER, note hub-note)~%")
        (finish-output)
        (register-peer-device g :origin-id *device-origin*
                                :roots (list (id rec)) :scope :main)
        (write-flag "ready")

        ;; Wait for the device to pull, edit locally, and push.
        (unless (wait-flag "pushed")
          (format t "~&HUB: timed out waiting for the device push~%") (hexit 1))

        ;; The push re-homed on the session thread; verify the hub's merged state.
        (let ((f (first (map-vertices #'identity g :collect-p t :vertex-type
                          'pf-record))))
          (check (equal "DANGER" (slot-value f 'alert))
                 "re-home REJECTED the release -- alert stays DANGER (got ~S)"
                 (slot-value f 'alert))
          (check (equal "device-note" (slot-value f 'note))
                 "re-home took the device's :lww note (got ~S)" (slot-value f 'note))
          (check (>= (length (get-peer-conflicts g)) 1)
                 "the rejected release surfaced a conflict (~D)"
                 (length (get-peer-conflicts g)))
          (let ((c (first (get-peer-conflicts g))))
            (when c
              (check (equal "DANGER" (peer-conflict-kept-value c))
                     "conflict kept-value is DANGER (got ~S)" (peer-conflict-kept-value c))
              (check (equal "SAFE" (peer-conflict-loser-value c))
                     "conflict loser-value is the SAFE release (got ~S)"
                     (peer-conflict-loser-value c)))))
        (if (zerop *fails*)
            (progn (write-flag "hub-verified") (format t "~&HUB: PASS~%"))
            (progn (write-flag "hub-verified") (format t "~&HUB: FAIL (~D)~%" *fails*)))
        (finish-output)

        (wait-flag "device-done")
        (close-graph g :snapshot-p nil)
        (format t "~&HUB: closed~%") (finish-output)
        (hexit (if (zerop *fails*) 0 1))))
  (error (c)
    (format t "~&HUB ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "ready"))
    (ignore-errors (write-flag "hub-verified"))
    (hexit 1)))
