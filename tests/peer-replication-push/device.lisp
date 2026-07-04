;;;; Peer-replication PUSH conflict test -- DEVICE process (B2d-2b).
;;;;
;;;; Opens an empty device graph, pulls the find, edits it LOCALLY -- releases the
;;;; hazard (DANGER -> SAFE) and rewrites the note in a second commit (so its note
;;;; stamp has a higher lamport than the hub's) -- then PEER-SYNCs again, which pulls
;;;; (a no-op here) and then PUSHES the two authored ops to the hub.  The device keeps
;;;; its own SAFE value (a loser doesn't converge via pull until B3); the hub verifies
;;;; that its re-home rejected the release.  Exits 0 iff its local checks pass.

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
(log:config :error)

(defun dflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (dflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (dflag name)) (return t))
    (sleep 0.1)))
(defun dexit (code) #+sbcl (sb-ext:exit :code code)
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
    (progn
      (unless (wait-flag "ready")
        (format t "~&DEVICE: hub never became ready~%") (dexit 1))
      (let* ((dir  (uiop:getenv "REPL_DEVICE_DIR"))
             (port (parse-integer (uiop:getenv "REPL_PORT")))
             ;; REPL_DEVICE_MEMORY -> in-memory device (memory-peer-graph); the
             ;; authored-write / push path is identical (the ship mobile config).
             (g (if (uiop:getenv "REPL_DEVICE_MEMORY")
                    (make-memory-graph :push-test-app dir
                                       :peer-role :device
                                       :origin-id *device-origin*
                                       :peer-host "localhost"
                                       :replication-port port
                                       :replication-key "peer-secret"
                                       :merge-policy (push-merge-policy)
                                       :lazy (and (uiop:getenv "REPL_DEVICE_LAZY") t))
                    (make-graph :push-test-app dir
                                :peer-role :device
                                :origin-id *device-origin*
                                :peer-host "localhost"
                                :replication-port port
                                :replication-key "peer-secret"
                                :merge-policy (push-merge-policy)
                                :buffer-pool-size 1000))))
        (let ((*graph* g))
          (flet ((the-find ()
                   (first (map-vertices #'identity g :collect-p t :vertex-type 'pf-find))))
            ;; --- pull the seed ---
            (peer-sync g)
            (let ((f (the-find)))
              (check (and f (equal "DANGER" (slot-value f 'hazard)))
                     "pull: seeded find, hazard DANGER (got ~S)" (and f (slot-value f 'hazard)))
              (check (and f (equal "hub-note" (slot-value f 'note)))
                     "pull: seeded note hub-note (got ~S)" (and f (slot-value f 'note))))
            ;; --- edit locally: release the hazard, then rewrite the note ---
            (with-transaction ()
              (let ((v (copy (the-find)))) (setf (slot-value v 'hazard) "SAFE") (save v)))
            (with-transaction ()
              (let ((v (copy (the-find)))) (setf (slot-value v 'note) "device-note") (save v)))
            (let ((f (the-find)))
              (check (equal "SAFE" (slot-value f 'hazard))
                     "local edit applied on the device (hazard SAFE)")
              (check (equal "device-note" (slot-value f 'note))
                     "local note edit applied on the device"))
            ;; --- push: peer-sync pulls (no-op) then pushes the two authored ops ---
            (peer-sync g)
            (check (> (load-peer-push-ack g) 0)
                   "push-ack advanced past the pushed ops (got ~D)" (load-peer-push-ack g))
            ;; the loser's own copy stays SAFE (no convergence-via-pull until B3)
            (check (equal "SAFE" (slot-value (the-find) 'hazard))
                   "device keeps its own SAFE value after the push"))
          (write-flag "pushed")
          ;; Let the hub verify its re-homed state before we tear down the socket.
          (wait-flag "hub-verified")
          ;; --- LAZY: fault-on-access reopen of the authored subgraph ---
          (if (uiop:getenv "REPL_DEVICE_LAZY")
              (progn
                (checkpoint-memory-graph g)
                (close-graph g :snapshot-p nil)
                (let ((g2 (open-memory-graph :push-test-app dir :lazy t)))
                  (let ((*graph* g2))
                    (check (loop for v being the hash-values of
                                 (mem-table-data (vertex-table g2))
                                 always (lznode-p v))
                           "lazy: reopen built no live node (fault-on-access)")
                    (let ((f (first (map-vertices #'identity g2 :collect-p t
                                                          :vertex-type 'pf-find))))
                      (check (and f (equal "SAFE" (slot-value f 'hazard)))
                             "lazy: authored hazard SAFE survives reopen + materializes")
                      (check (and f (equal "device-note" (slot-value f 'note)))
                             "lazy: authored note survives reopen + materializes")))
                  (close-graph g2 :snapshot-p nil)))
              (close-graph g :snapshot-p nil))))
      (write-flag "device-done")
      (if (zerop *fails*)
          (progn (format t "~&DEVICE: PASS~%") (finish-output) (dexit 0))
          (progn (format t "~&DEVICE: FAIL (~D failed checks)~%" *fails*)
                 (finish-output) (dexit 1))))
  (error (c)
    (format t "~&DEVICE ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "pushed"))
    (ignore-errors (write-flag "device-done"))
    (dexit 1)))
