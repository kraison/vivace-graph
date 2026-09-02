;;;; Multi-device peer-replication test -- HUB process.
;;;;
;;;; Driven by run-multi-peer-test.sh.  Builds the hub graph, registers TWO
;;;; devices (scopes "alpha" and "bravo", both rooted at the shared depot), and
;;;; drives four phases, coordinating with both devices via flag files:
;;;;
;;;;   PHASE 1 (seed):   the closed authority-scoped graph (see schema.lisp).
;;;;   PHASE 2 (update): change the OVERLAP item fs1's status -> both devices
;;;;                     re-pull and converge (fan-out, not a conflict).
;;;;   PHASE 3 (re-task):fa1 team alpha->bravo: A PURGES it (left A's scope); B
;;;;                     still cannot see it (its only path is via inspection-a,
;;;;                     which B never traverses) -- per-device purge.
;;;;   PHASE 4 (re-enter):fa1 team bravo->alpha: A re-gains it (purge -> re-entry,
;;;;                     PT-2); B unchanged.
;;;;
;;;; Coordination flags under REPL_WORK: the hub writes "ready"/"pN-ready" and
;;;; waits for "a-pN"/"b-pN" from both devices before advancing.

(require :asdf)

(flet ((bail (condition)
         (format *error-output* "~&=== UNHANDLED ~S ===~%~A~%" (type-of condition) condition)
         (ignore-errors
           #+sbcl (sb-debug:print-backtrace :stream *error-output* :count 40)
           #+ecl  (si::tpl-backtrace))
         (ignore-errors (finish-output *error-output*))
         #+sbcl (sb-ext:exit :code 70 :abort t)
         #+ecl  (ext:quit 70)
         #+ccl  (ccl:quit 70)
         #-(or sbcl ecl ccl) (uiop:quit 70)))
  (setf *debugger-hook* (lambda (c hook) (declare (ignore hook)) (bail c)))
  #+ecl (setf ext:*invoke-debugger-hook* (lambda (c hook) (declare (ignore hook)) (bail c))))

(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(with-open-file (s (merge-pathnames "build.log" (or (uiop:getenv "REPL_WORK") #p"/tmp/"))
                   :direction :output :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* s) (*error-output* s))
    (ql:quickload :graph-db :silent t)))
(in-package :graph-db)

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
                                            :if-exists :supersede :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (hflag name)) (return t))
    (sleep 0.1)))
(defun wait-both (phase &optional (timeout 60))
  (and (wait-flag (format nil "a-~A" phase) timeout)
       (wait-flag (format nil "b-~A" phase) timeout)))
(defun hexit (code) #+sbcl (sb-ext:exit :code code) #+ecl (ext:quit code)
                    #+ccl (ccl:quit code) #-(or sbcl ccl ecl) (uiop:quit code))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (let* ((dir  (uiop:getenv "REPL_HUB_DIR"))
           (port (parse-integer (uiop:getenv "REPL_PORT")))
           (g (make-graph :peer-multi-app dir
                          :peer-role :hub
                          :replication-port port
                          :replication-key "multi-secret"
                          :export-predicate #'peer-multi-disclosable
                          :buffer-pool-size 1000)))
      (let ((*graph* g) depot)
        ;; PHASE 1 payload.
        (with-transaction ()
          (let* ((s   (make-m-depot   :name "Depot"     :team "both"))
                 (sa  (make-m-inspection :name "Inspection-A" :team "alpha"))
                 (sb  (make-m-inspection :name "Inspection-B" :team "bravo"))
                 (ss  (make-m-inspection :name "Inspection-S" :team "both"))
                 (fa1 (make-m-item   :name "Item-A1"  :team "alpha" :status
                        "open"))
                 (fa2 (make-m-item   :name "Item-A2"  :team "alpha" :status
                        "open"))
                 (fb1 (make-m-item   :name "Item-B1"  :team "bravo" :status
                        "open"))
                 (fs1 (make-m-item   :name "Item-S1"  :team "both"  :status
                        "open")))
            (make-m-has-inspection :from s  :to sa)
            (make-m-has-inspection :from s  :to sb)
            (make-m-has-inspection :from s  :to ss)
            (make-m-has-item   :from sa :to fa1)
            (make-m-has-item   :from sa :to fa2)
            (make-m-has-item   :from sb :to fb1)
            (make-m-has-item   :from ss :to fs1)
            (setq depot s)))
        (format t "~&HUB: phase-1 committed~%") (finish-output)
        (register-peer-device g :origin-id *device-a-origin* :roots (list (id
                                                         depot)) :scope "alpha")
        (register-peer-device g :origin-id *device-b-origin* :roots (list (id
                                                         depot)) :scope "bravo")
        (write-flag "ready")

        (flet ((vertex-by-name (n)
                 (first (remove-if-not
                         (lambda (x) (string= n (slot-value x 'name)))
                         (map-vertices #'identity g :collect-p t :vertex-type
                           'm-item))))
               (retask (node new-team)
                 (with-transaction ()
                   (let ((v (copy node)))
                     (setf (slot-value v 'team) new-team)
                     (save v))))
               (set-status (node new-status)
                 (with-transaction ()
                   (let ((v (copy node)))
                     (setf (slot-value v 'status) new-status)
                     (save v)))))

          ;; PHASE 2: update the overlap node fs1.
          (unless (wait-both "p1") (format t "~&HUB: timed out on p1~%") (hexit 1))
          (set-status (vertex-by-name "Item-S1") "done")
          (format t "~&HUB: phase-2 committed (Item-S1 -> done)~%")
              (finish-output)
          (write-flag "p2-ready")

          ;; PHASE 3: re-task Item-A1 alpha -> bravo.
          (unless (wait-both "p2") (format t "~&HUB: timed out on p2~%") (hexit 1))
          (retask (vertex-by-name "Item-A1") "bravo")
          (format t "~&HUB: phase-3 committed (Item-A1 -> bravo)~%")
              (finish-output)
          (write-flag "p3-ready")

          ;; PHASE 4: re-task Item-A1 bravo -> alpha (re-entry).
          (unless (wait-both "p3") (format t "~&HUB: timed out on p3~%") (hexit 1))
          (retask (vertex-by-name "Item-A1") "alpha")
          (format t "~&HUB: phase-4 committed (Item-A1 -> alpha)~%")
              (finish-output)
          (write-flag "p4-ready")

          ;; PHASE 5: in ONE step, a node ENTERING mid-stream + a SECOND edit to a
          ;; held node (both devices hold Item-S1; pull-cursor is now
          ;; non-zero, so this exercises a second op-stream delivery past
          ;; the advanced cursor).
          (unless (wait-both "p4") (format t "~&HUB: timed out on p4~%") (hexit 1))
          (with-transaction ()
            (let ((ss (first (remove-if-not
                              (lambda (x) (string= "Inspection-S" (slot-value x
                                                                    'name)))
                              (map-vertices #'identity g :collect-p t
                                :vertex-type 'm-inspection))))
                  (fs2 (make-m-item :name "Item-S2" :team "both" :status
                         "open")))
              (make-m-has-item :from ss :to fs2)))
          (set-status (vertex-by-name "Item-S1") "archived")
          (format t
            "~&HUB: phase-5 committed (Item-S2 created; Item-S1 -> archived)~%")
          (finish-output)
          (write-flag "p5-ready")

          (unless (wait-both "p5") (format t "~&HUB: timed out on p5~%") (hexit 1)))

        (write-flag "hub-done")
        (close-graph g :snapshot-p nil)
        (format t "~&HUB: closed~%") (finish-output)
        (hexit 0)))
  (error (c)
    (format t "~&HUB ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "ready") (write-flag "p2-ready")
                   (write-flag "p3-ready") (write-flag "p4-ready") (write-flag "p5-ready"))
    (hexit 1)))
