;;;; Peer-replication test -- DEVICE process.
;;;;
;;;; Driven by run-peer-test.sh.  Reads REPL_DEVICE_DIR, REPL_PORT and REPL_WORK
;;;; from the environment, opens an EMPTY device graph, and verifies:
;;;;
;;;;   PHASE 1 (seed pull): after PEER-SYNC the device holds exactly its closed
;;;; disclosable subgraph -- depot + inspection + Item-1 + the two connecting
;;;;     edges -- and NOT the withheld Item-2 (nor its edge).
;;;;   PHASE 2 (purge): after the hub flips Item-1 to non-disclosable, a second
;;;; PEER-SYNC PURGES Item-1 + its edge (scope exit), leaving depot +
;;;; inspection.
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
;;; threads inherit it.  (app-harness item 2.)
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
         "system-device/"
         ;; Trailing slash: REPL_WORK has none, and MERGE-PATHNAMES
         ;; would otherwise treat its last component as a file name.
         (format nil "~A/" (or (uiop:getenv "REPL_WORK") "/tmp"))))))
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
                                                                 :vertex-type
                                                                     'p-item))))
            ;; --- PHASE 1: seed pull ---
            (peer-sync g)
            (check (= 1 (vcount 'p-depot))   "phase1: 1 depot (got ~D)"
              (vcount 'p-depot))
            (check (= 1 (vcount 'p-inspection)) "phase1: 1 inspection (got ~D)"
              (vcount 'p-inspection))
            (check (= 1 (vcount 'p-item))   "phase1: 1 item (got ~D)"   (vcount
                                                                       'p-item))
            (check (member "Item-1" (find-names) :test 'string=)
                   "phase1: disclosable Item-1 present")
            (check (not (member "Item-2" (find-names) :test 'string=))
                   "phase1: withheld Item-2 absent (fail-closed)")
            (check (= 2 (ecount))
              "phase1: 2 edges, Item-2's edge omitted (got ~D)" (ecount))
            (write-flag "phase1-verified")

            ;; --- PHASE 2: scope exit -> purge ---
            (unless (wait-flag "phase2-ready")
              (check nil "hub never readied phase 2"))
            (peer-sync g)
            (check (= 1 (vcount 'p-depot))   "phase2: depot retained (got ~D)"
              (vcount 'p-depot))
            (check (= 1 (vcount 'p-inspection))
              "phase2: inspection retained (got ~D)" (vcount 'p-inspection))
            (check (= 0 (vcount 'p-item))
                   "phase2: Item-1 PURGED after leaving scope (items=~D)"
                       (vcount 'p-item))
            (check (= 1 (ecount))
                   "phase2: only depot->inspection edge remains (got ~D)"
                       (ecount))

            ;; --- SCHEMA COMPAT: a major bump must be rejected ---
            (setf (peer-schema-version g) '(2 0))
            (let ((rejected
                    (handler-case (progn (peer-sync g) nil)
                      (peer-schema-incompatible-error () t))))
              (check rejected "schema: major mismatch (2 x) vs hub (1 x) rejected"))
            (check (= 1 (vcount 'p-inspection))
                   "schema: device data intact after a rejected major-mismatch sync"))
          ;; --- LAZY: fault-on-access reopen of the peer-synced subgraph ---
          ;; Checkpoint (writes the VG-native image), close, reopen with :LAZY t,
          ;; and confirm (a) OPEN built NO live node (all LZNODE blobs) and (b) the
          ;; retained depot+inspection still materialize correctly on first
          ;; access.
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
                                                              :vertex-type
                                                                  'p-depot)))
                      "lazy: depot survives native-image reopen + materializes")
                    (check (= 1 (length (map-vertices #'identity g2 :collect-p t
                                                              :vertex-type
                                                                'p-inspection)))
                "lazy: inspection survives native-image reopen + materializes"))
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
