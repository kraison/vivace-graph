;;;; Multi-device peer-replication test -- DEVICE process (role via REPL_DEVICE_ID).
;;;;
;;;; Two copies run (REPL_DEVICE_ID = "a" scope "alpha", "b" scope "bravo").  Each
;;;; opens an empty device graph and verifies, across four hub-driven phases, that
;;;; it holds EXACTLY its authority-scoped closed subgraph:
;;;;
;;;;   P1 seed     -- scope isolation + overlap: A holds survey-a/fa1/fa2 (+ shared
;;;;                  site/survey-s/fs1) and NOT fb1; B holds survey-b/fb1 (+ shared)
;;;;                  and NOT fa1/fa2.
;;;;   P2 update   -- the overlap find fs1's status propagates to BOTH devices.
;;;;   P3 re-task  -- fa1 alpha->bravo: A PURGES fa1; B still cannot see it.
;;;;   P4 re-enter -- fa1 bravo->alpha: A re-gains fa1 (PT-2); B unchanged.
;;;;
;;;; Exits 0 only if every check passes.

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
(log:config :error)

(defparameter *id* (or (uiop:getenv "REPL_DEVICE_ID") "a"))   ; "a" or "b"
(defun dflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (dflag name) :direction :output
                                            :if-exists :supersede :if-does-not-exist :create)
                           (format s "~A" name)))
(defun done-flag (phase) (write-flag (format nil "~A-~A" *id* phase)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (dflag name)) (return t))
    (sleep 0.1)))
(defun dexit (code) #+sbcl (sb-ext:exit :code code) #+ecl (ext:quit code)
                    #+ccl (ccl:quit code) #-(or sbcl ccl ecl) (uiop:quit code))

(defvar *fails* 0)
(defun check (ok fmt &rest args)
  (if ok
      (format t "~&  ok   [~A] ~?~%" *id* fmt args)
      (progn (incf *fails*) (format t "~&  FAIL [~A] ~?~%" *id* fmt args)))
  (finish-output))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (progn
      (unless (wait-flag "ready")
        (format t "~&DEVICE[~A]: hub never became ready~%" *id*) (dexit 1))
      (let* ((dir    (uiop:getenv "REPL_DEVICE_DIR"))
             (port   (parse-integer (uiop:getenv "REPL_PORT")))
             (a-p    (string= *id* "a"))
             (origin (if a-p *device-a-origin* *device-b-origin*))
             (g (make-graph :peer-multi-app dir
                            :peer-role :device
                            :origin-id origin
                            :peer-host "localhost"
                            :replication-port port
                            :replication-key "multi-secret"
                            :buffer-pool-size 1000)))
        (let ((*graph* g))
          (labels ((names (type) (sort (mapcar (lambda (v) (slot-value v 'name))
                                               (map-vertices #'identity g :collect-p t
                                                                        :vertex-type type))
                                       #'string<))
                   (find-names () (names 'm-find))
                   (vcount () (length (map-vertices #'identity g :collect-p t)))
                   (ecount () (length (map-edges #'identity g :collect-p t)))
                   (find-status (n)
                     (let ((f (first (remove-if-not
                                      (lambda (v) (string= n (slot-value v 'name)))
                                      (map-vertices #'identity g :collect-p t :vertex-type 'm-find)))))
                       (and f (slot-value f 'status))))
                   (set= (got want) (equal got (sort (copy-list want) #'string<)))
                   (expect-finds (want phase)
                     (check (set= (find-names) want)
                            "~A: finds = ~S (got ~S)" phase want (find-names)))
                   (expect-vcount (n phase)
                     (check (= n (vcount)) "~A: ~D vertices (got ~D)" phase n (vcount))))

            ;; ---- P1: seed (scope isolation + overlap) ----
            (peer-sync g)
            (if a-p
                (progn
                  (expect-finds '("Find-A1" "Find-A2" "Find-S1") "p1")
                  (expect-vcount 6 "p1")
                  (check (= 5 (ecount)) "p1: 5 edges (got ~D)" (ecount))
                  (check (not (member "Find-B1" (find-names) :test 'string=))
                         "p1: isolation -- B's find withheld"))
                (progn
                  (expect-finds '("Find-B1" "Find-S1") "p1")
                  (expect-vcount 5 "p1")
                  (check (= 4 (ecount)) "p1: 4 edges (got ~D)" (ecount))
                  (check (not (member "Find-A1" (find-names) :test 'string=))
                         "p1: isolation -- A's find withheld")))
            (check (string= "open" (find-status "Find-S1")) "p1: overlap Find-S1 status open")
            (done-flag "p1")

            ;; ---- P2: overlap update propagates to BOTH ----
            (unless (wait-flag "p2-ready") (check nil "hub never readied p2"))
            (peer-sync g)
            (check (string= "cleared" (find-status "Find-S1"))
                   "p2: overlap Find-S1 status updated (got ~S)" (find-status "Find-S1"))
            (done-flag "p2")

            ;; ---- P3: re-task fa1 alpha->bravo (A purges; B unchanged) ----
            (unless (wait-flag "p3-ready") (check nil "hub never readied p3"))
            (peer-sync g)
            (if a-p
                (progn
                  (expect-finds '("Find-A2" "Find-S1") "p3")
                  (expect-vcount 5 "p3")
                  (check (not (member "Find-A1" (find-names) :test 'string=))
                         "p3: Find-A1 PURGED after leaving A's scope"))
                (progn
                  (expect-finds '("Find-B1" "Find-S1") "p3")
                  (check (not (member "Find-A1" (find-names) :test 'string=))
                         "p3: Find-A1 still not leaked to B (only path via undisclosed survey-a)")))
            (done-flag "p3")

            ;; ---- P4: re-task fa1 bravo->alpha (A re-enters; B unchanged) ----
            (unless (wait-flag "p4-ready") (check nil "hub never readied p4"))
            (peer-sync g)
            (if a-p
                (progn
                  (expect-finds '("Find-A1" "Find-A2" "Find-S1") "p4")
                  (expect-vcount 6 "p4")
                  (check (member "Find-A1" (find-names) :test 'string=)
                         "p4: Find-A1 re-entered A after re-disclosure (PT-2)"))
                (progn
                  (expect-finds '("Find-B1" "Find-S1") "p4")
                  (expect-vcount 5 "p4")))
            (done-flag "p4")

            ;; ---- P5: node entering mid-stream (Find-S2) + 2nd op-stream edit ----
            (unless (wait-flag "p5-ready") (check nil "hub never readied p5"))
            (peer-sync g)
            (check (member "Find-S2" (find-names) :test 'string=)
                   "p5: new Find-S2 entered scope (shared survey)")
            (check (string= "released" (find-status "Find-S1"))
                   "p5: Find-S1 2nd edit via op-stream past advanced cursor (got ~S)"
                   (find-status "Find-S1"))
            (if a-p
                (progn
                  (expect-finds '("Find-A1" "Find-A2" "Find-S1" "Find-S2") "p5")
                  (expect-vcount 7 "p5"))
                (progn
                  (expect-finds '("Find-B1" "Find-S1" "Find-S2") "p5")
                  (expect-vcount 6 "p5")))
            (done-flag "p5"))
          (close-graph g :snapshot-p nil)))
      (if (zerop *fails*)
          (progn (format t "~&DEVICE[~A]: PASS~%" *id*) (finish-output) (dexit 0))
          (progn (format t "~&DEVICE[~A]: FAIL (~D failed checks)~%" *id* *fails*)
                 (finish-output) (dexit 1))))
  (error (c)
    (format t "~&DEVICE[~A] ERROR: ~A~%" *id* c) (finish-output)
    (ignore-errors (done-flag "p1") (done-flag "p2") (done-flag "p3")
                   (done-flag "p4") (done-flag "p5"))
    (dexit 1)))
