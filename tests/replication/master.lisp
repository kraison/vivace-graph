;;;; Replication test -- MASTER process.
;;;;
;;;; Driven by run-replication-test.sh.  Reads its configuration from the
;;;; environment (REPL_MASTER_DIR, REPL_PORT, REPL_WORK), creates a master graph
;;;; (which auto-starts replication), and runs the scenario:
;;;;
;;;;   TX1  insert two vertices + an edge   -- committed BEFORE the slave
;;;;        connects, so the slave must CATCH UP from the log.
;;;;   TX2  update a vertex's slot          -- committed AFTER the slave
;;;;   TX3  delete the other vertex            connects, so these stream LIVE.
;;;;
;;;; Coordination with the slave is via flag files under REPL_WORK.

(require :asdf)
;; Bootstrap quicklisp if the running Lisp didn't auto-load it (CCL/ECL on some
;; hosts don't); harmless when ql is already present (e.g. SBCL).
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
;; Bind *standard-output*/*error-output* to a real file around quickload: ECL's
;; first build shells out (osicat C wrappers) via run-program, which needs a
;; stream backed by a real file handle -- under the harness's stdout redirect it
;; has none, giving COMPILE-FILE-ERROR.
(with-open-file (s (merge-pathnames "build.log"
                                    (or (uiop:getenv "REPL_WORK") #p"/tmp/"))
                   :direction :output
                   :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* s) (*error-output* s))
    (ql:quickload :graph-db :silent t)))
(in-package :graph-db)

;;; Type-ids come from the image-level registry in *SYSTEM-DIRECTORY* (GH
;;; #186), so this process needs one before it opens anything.  Its OWN,
;;; under REPL_WORK: this harness exists because master and slave are
;;; separate IMAGES, and a shared registry would quietly undo that.  Both
;;; ends evaluate one schema.lisp in one order, so they agree on every
;;; type-id -- which is what lets a raw id cross the wire.
;;;
;;; SETF, not a LET around the body: replication runs on threads that do
;;; not inherit dynamic bindings.
(setf *system-directory*
      (namestring
       (ensure-directories-exist
        (merge-pathnames
         "system-master/"
         ;; Trailing slash: REPL_WORK has none, and MERGE-PATHNAMES
         ;; would otherwise treat its last component as a file name.
         (format nil "~A/" (or (uiop:getenv "REPL_WORK") "/tmp"))))))
(log:config :error)

(defun mflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (mflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (mflag name)) (return t))
    (sleep 0.1)))
(defun mexit (code) #+sbcl (sb-ext:exit :code code)
                    #+ccl (ccl:quit code)
                    #+ecl (ext:quit code)
                    #-(or sbcl ccl ecl) (uiop:quit code))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (let* ((dir  (uiop:getenv "REPL_MASTER_DIR"))
           (port (parse-integer (uiop:getenv "REPL_PORT")))
           (g (make-graph :repl-test-app dir :master-p t
                          :replication-port port :replication-key "test-secret"
                          :buffer-pool-size 1000)))
      (let ((*graph* g))
        ;; TX1: catch-up payload (committed before the slave exists).  Includes
        ;; two geometry places -- one in the slave's AO (Kharkiv), one outside it
        ;; (Lviv) -- so the slave's subset filter + replicated spatial index can
        ;; be checked on the catch-up path.
        (with-transaction ()
          (let ((a (make-r-person :name "Alice" :age 30))
                (b (make-r-person :name "Bob"   :age 25)))
            (make-r-knows :from a :to b :since "2020"))
          (make-r-place :label "Kharkiv site" :location (make-point 37.1724d0 49.2020d0))
          (make-r-place :label "Lviv site"    :location (make-point 23.7183d0 50.0263d0))
          ;; Two places that will CROSS the slave's AO boundary on a later update:
          ;; Crosser-out starts IN the AO (slave holds it), Crosser-in starts OUT
          ;; (slave filters it).  See the live phase below.
          (make-r-place :label "Crosser-out" :location (make-point 37.1750d0 49.2050d0))
          (make-r-place :label "Crosser-in"  :location (make-point 23.7000d0 50.0000d0)))
        (format t "~&MASTER: TX1 committed (2 persons + 1 edge + 2 places)~%") (finish-output)
        (write-flag "ready")
        ;; wait for the slave to connect + verify catch-up
        (unless (wait-flag "connected")
          (format t "~&MASTER: timed out waiting for slave to connect~%")
          (mexit 1))
        ;; TX2 + TX3: live payload (streamed to the connected slave)
        (flet ((by-name (n)
                 (first (remove-if-not
                         (lambda (x) (string= n (slot-value x 'name)))
                         (map-vertices #'identity g :collect-p t
                                                  :vertex-type 'r-person)))))
          (with-transaction ()
            (let ((v (copy (by-name "Alice"))))
              (setf (slot-value v 'name) "Alice2")
              (save v)))
          (format t "~&MASTER: TX2 committed (Alice -> Alice2)~%") (finish-output)
          ;; A LIVE in-AO place (ordered before the age bumps, so once the slave
          ;; observes the final age=33 state it has also applied this place).
          (with-transaction ()
            (make-r-place :label "Kharkiv live" :location (make-point 37.1730d0 49.2030d0)))
          (format t "~&MASTER: live place committed (Kharkiv live)~%") (finish-output)
          ;; AO-boundary-crossing updates (ordered before the age bumps, so the
          ;; slave has applied them by the time it observes age=33).  Crosser-out
          ;; moves OUT of the AO (slave must DELETE it); Crosser-in moves INTO the
          ;; AO (slave must CREATE it -- it never held it before).
          (flet ((by-label (l)
                   (first (remove-if-not
                           (lambda (x) (string= l (slot-value x 'label)))
                           (map-vertices #'identity g :collect-p t :vertex-type 'r-place)))))
            (with-transaction ()
              (let ((v (copy (by-label "Crosser-out"))))
                (setf (slot-value v 'location) (make-point 23.7183d0 50.0263d0)) ; -> Lviv (out)
                (save v)))
            (with-transaction ()
              (let ((v (copy (by-label "Crosser-in"))))
                (setf (slot-value v 'location) (make-point 37.1740d0 49.2040d0)) ; -> Kharkiv (in)
                (save v))))
          (format t "~&MASTER: AO crossers committed (out->Lviv, in->Kharkiv)~%")
          (finish-output)
          ;; MVCC: update Alice2 repeatedly so the slave builds a real multi-
          ;; version prev-pointer chain (the 2nd+ update's old-node carries a
          ;; non-zero prev-pointer -- a MASTER heap address that must NOT be
          ;; copied into the slave's chain; the slave re-derives it locally).
          (dotimes (i 3)
            (with-transaction ()
              (let ((v (copy (by-name "Alice2"))))
                (setf (slot-value v 'age) (+ 31 i))   ; -> 31, 32, 33
                (save v))))
          (format t "~&MASTER: TX2b committed (Alice2 age -> 33 over 3 updates)~%")
          (finish-output)
          (mark-deleted (by-name "Bob"))    ; mark-deleted self-wraps a txn
          (format t "~&MASTER: TX3 committed (deleted Bob)~%") (finish-output))
        (write-flag "phase2done"))
      ;; stay alive until the slave is done (or 60s), then close cleanly
      (wait-flag "slave-done")
      (close-graph g :snapshot-p nil)
      (format t "~&MASTER: closed~%") (finish-output)
      (mexit 0))
  (error (c)
    (format t "~&MASTER ERROR: ~A~%" c) (finish-output)
    (mexit 1)))
