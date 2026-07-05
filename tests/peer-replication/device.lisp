;;;; Peer-replication test -- DEVICE process.
;;;;
;;;; Driven by run-peer-test.sh.  Reads REPL_DEVICE_DIR, REPL_PORT and REPL_WORK
;;;; from the environment, opens an EMPTY device graph, and verifies:
;;;;
;;;;   PHASE 1 (seed pull): after PEER-SYNC the device holds exactly its closed
;;;;     disclosable subgraph -- site + survey + Find-1 + the two connecting
;;;;     edges -- and NOT the withheld Find-2 (nor its edge).
;;;;   PHASE 2 (purge): after the hub flips Find-1 to non-disclosable, a second
;;;;     PEER-SYNC PURGES Find-1 + its edge (scope exit), leaving site + survey.
;;;;   SCHEMA COMPAT (WP-6/PT-6): the device runs at schema (1 3) against a hub at
;;;;     (1 0) throughout -- a minor drift that must still sync (same major).  A
;;;;     final major bump to (2 0) must be REJECTED (peer-schema-incompatible).
;;;;
;;;; Exits 0 only if every check passes.

(require :asdf)

;;; Disable the interactive debugger: print the condition (and a best-effort
;;; backtrace) then quit non-zero.  Without this an unhandled ERROR on ECL --
;;; especially one raised in a spawned thread, outside the main HANDLER-CASE --
;;; drops into a debugger with no tty and recurses into a STORAGE-EXHAUSTED
;;; cascade that buries the real error.  Set as a global (not LET) so spawned
;;; threads inherit it.  (mine-action item 2.)
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
             ;; REPL_DEVICE_MEMORY opts the device into the in-memory backend
             ;; (memory-peer-graph) instead of the on-disk one; the peer transport
             ;; and every check below are identical -- this exercises the exact
             ;; mobile ship path (SBCL hub <-> in-memory ECL device).
             ;; REPL_DEVICE_LAZY (with REPL_DEVICE_MEMORY) also opts the in-memory
             ;; device into fault-on-access: nodes materialize on first touch and a
             ;; reopen verification runs at the end (below).
             (g (if (uiop:getenv "REPL_DEVICE_MEMORY")
                    (make-memory-graph :peer-test-app dir
                                       :peer-role :device
                                       :origin-id *device-origin*
                                       :peer-host "localhost"
                                       :replication-port port
                                       :replication-key "peer-secret"
                                       :lazy (and (uiop:getenv "REPL_DEVICE_LAZY") t))
                    (make-graph :peer-test-app dir
                                :peer-role :device
                                :origin-id *device-origin*
                                :peer-host "localhost"
                                :replication-port port
                                :replication-key "peer-secret"
                                :buffer-pool-size 1000))))
        ;; Run the device a MINOR version ahead of the hub (1 3) vs (1 0): a
        ;; same-major drift that must still sync degraded-safe (WP-6/PT-6).
        (setf (peer-schema-version g) '(1 3))
        (let ((*graph* g))
          (flet ((vcount (type) (length (map-vertices #'identity g :collect-p t
                                                                  :vertex-type type)))
                 (ecount () (length (map-edges #'identity g :collect-p t)))
                 (find-names () (mapcar (lambda (v) (slot-value v 'name))
                                        (map-vertices #'identity g :collect-p t
                                                                 :vertex-type 'p-find))))
            ;; --- PHASE 1: seed pull ---
            (peer-sync g)
            (check (= 1 (vcount 'p-site))   "phase1: 1 site (got ~D)"   (vcount 'p-site))
            (check (= 1 (vcount 'p-survey)) "phase1: 1 survey (got ~D)" (vcount 'p-survey))
            (check (= 1 (vcount 'p-find))   "phase1: 1 find (got ~D)"   (vcount 'p-find))
            (check (member "Find-1" (find-names) :test 'string=)
                   "phase1: disclosable Find-1 present")
            (check (not (member "Find-2" (find-names) :test 'string=))
                   "phase1: withheld Find-2 absent (fail-closed)")
            (check (= 2 (ecount)) "phase1: 2 edges, Find-2's edge omitted (got ~D)" (ecount))
            (write-flag "phase1-verified")

            ;; --- PHASE 2: scope exit -> purge ---
            (unless (wait-flag "phase2-ready")
              (check nil "hub never readied phase 2"))
            (peer-sync g)
            (check (= 1 (vcount 'p-site))   "phase2: site retained (got ~D)" (vcount 'p-site))
            (check (= 1 (vcount 'p-survey)) "phase2: survey retained (got ~D)" (vcount 'p-survey))
            (check (= 0 (vcount 'p-find))
                   "phase2: Find-1 PURGED after leaving scope (finds=~D)" (vcount 'p-find))
            (check (= 1 (ecount))
                   "phase2: only site->survey edge remains (got ~D)" (ecount))

            ;; --- SCHEMA COMPAT: a major bump must be rejected ---
            (setf (peer-schema-version g) '(2 0))
            (let ((rejected
                    (handler-case (progn (peer-sync g) nil)
                      (peer-schema-incompatible-error () t))))
              (check rejected "schema: major mismatch (2 x) vs hub (1 x) rejected"))
            (check (= 1 (vcount 'p-survey))
                   "schema: device data intact after a rejected major-mismatch sync"))
          ;; --- LAZY: fault-on-access reopen of the peer-synced subgraph ---
          ;; Checkpoint (writes the VG-native image), close, reopen with :LAZY t,
          ;; and confirm (a) OPEN built NO live node (all LZNODE blobs) and (b) the
          ;; retained site+survey still materialize correctly on first access.
          (if (uiop:getenv "REPL_DEVICE_LAZY")
              (progn
                (checkpoint-memory-graph g)
                (close-graph g :snapshot-p nil)
                (let ((g2 (open-memory-graph :peer-test-app dir :lazy t)))
                  (let ((*graph* g2))
                    (check (loop for v being the hash-values of
                                 (mem-table-data (vertex-table g2))
                                 always (lznode-p v))
                           "lazy: reopen built no live node (fault-on-access)")
                    (check (= 1 (length (map-vertices #'identity g2 :collect-p t
                                                              :vertex-type 'p-site)))
                           "lazy: site survives native-image reopen + materializes")
                    (check (= 1 (length (map-vertices #'identity g2 :collect-p t
                                                              :vertex-type 'p-survey)))
                           "lazy: survey survives native-image reopen + materializes"))
                  (close-graph g2 :snapshot-p nil)))
              (close-graph g :snapshot-p nil))))
      (write-flag "device-done")
      (if (zerop *fails*)
          (progn (format t "~&DEVICE: PASS~%") (finish-output) (dexit 0))
          (progn (format t "~&DEVICE: FAIL (~D failed checks)~%" *fails*)
                 (finish-output) (dexit 1))))
  (error (c)
    (format t "~&DEVICE ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "device-done"))
    (dexit 1)))
