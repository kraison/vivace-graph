;;;; Replication test -- SLAVE process.
;;;;
;;;; Driven by run-replication-test.sh.  Reads REPL_SLAVE_DIR, REPL_PORT and
;;;; REPL_WORK from the environment, connects to the master, and verifies two
;;;; phases:
;;;;
;;;;   CATCH-UP: the pre-connect TX1 (2 vertices + 1 edge) replays in full,
;;;;             with EXACTLY 2 vertices and 1 edge (no double-apply).
;;;;   LIVE:     the post-connect update + delete stream through, leaving
;;;;             exactly 1 live vertex named "Alice2" and 0 edges (deleting
;;;;             Bob removes the incident edge).
;;;;
;;;; Exits 0 on success, 1 on any failed check or timeout.

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
(log:config :error)

(defun sflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (sflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (sflag name)) (return t))
    (sleep 0.1)))
(defun sexit (code) #+sbcl (sb-ext:exit :code code)
                    #+ccl (ccl:quit code)
                    #+ecl (ext:quit code)
                    #-(or sbcl ccl ecl) (uiop:quit code))

(defvar *fails* 0)
(defun check (ok fmt &rest args)
  (if ok
      (format t "~&  ok   ~?~%" fmt args)
      (progn (incf *fails*) (format t "~&  FAIL ~?~%" fmt args)))
  (finish-output))

(defun wait-until (predicate &optional (timeout 30))
  (dotimes (i (* timeout 10) (funcall predicate))
    (when (funcall predicate) (return t))
    (sleep 0.1)))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (progn
      (unless (wait-flag "ready")
        (format t "~&SLAVE: master never became ready~%") (sexit 1))
      (let* ((dir  (uiop:getenv "REPL_SLAVE_DIR"))
             (port (parse-integer (uiop:getenv "REPL_PORT")))
             ;; Subset replication: this slave's area of operations is a box over
             ;; Kharkiv Oblast.  Non-spatial nodes (r-person, edges) still
             ;; replicate in full; spatial nodes are filtered to the AO -- so the
             ;; Kharkiv places arrive and are indexed, the Lviv place does not.
             (ao (make-polygon '(((37.16 49.19) (37.19 49.19)
                                  (37.19 49.21) (37.16 49.21) (37.16 49.19)))))
             ;; :keep-revisions 2 (slave-local reaper config, independent of the
             ;; master) so replicated updates leave a retained version chain we
             ;; can walk to prove the slave built it from LOCAL heap addresses.
             (g (make-graph :repl-test-app dir :slave-p t :master-host "localhost"
                            :replication-port port :replication-key "test-secret"
                            :buffer-pool-size 1000 :keep-revisions 2
                            :replication-filter (make-spatial-replication-filter ao))))
        (let ((*graph* g))
          (flet ((live ()  (map-vertices #'identity g :collect-p t :vertex-type 'r-person))
                 (edges () (map-edges #'identity g :collect-p t)))
            ;; --- Phase 1: catch up TX1 ---
            (wait-until (lambda () (and (= 2 (length (live))) (= 1 (length (edges))))))
            (check (= 2 (length (live))) "catch-up: 2 vertices (got ~D)" (length (live)))
            (check (= 1 (length (edges))) "catch-up: 1 edge (got ~D)" (length (edges)))
            (let ((alice (first (remove-if-not
                                 (lambda (v) (string= "Alice" (slot-value v 'name)))
                                 (live)))))
              (check alice "catch-up: vertex named Alice present")
              (when alice
                (check (= 30 (slot-value alice 'age)) "catch-up: Alice age 30")
                (check (= 1 (length (outgoing-edges alice)))
                       "catch-up: Alice has 1 outgoing edge (got ~D)"
                       (length (outgoing-edges alice)))))
            ;; --- Subset replication + replicated spatial index (catch-up) ---
            ;; The in-AO places (Kharkiv site + Crosser-out) replicated and are in
            ;; the slave's spatial index; the out-of-AO places (Lviv site +
            ;; Crosser-in) were filtered out entirely.
            (let ((kh (find-nodes-near 'r-place 49.2020d0 37.1724d0 2000d0 :graph g))
                  (lv (find-nodes-near 'r-place 50.0263d0 23.7183d0 2000d0 :graph g)))
              (check (= 2 (length kh))
                     "catch-up subset: 2 in-AO places replicated + indexed (got ~D)"
                     (length kh))
              (check (= 0 (length lv))
                     "catch-up subset: out-of-AO places filtered out (got ~D)"
                     (length lv)))
            ;; signal master to send the live update + delete
            (write-flag "connected")
            (unless (wait-flag "phase2done")
              (check nil "master never finished phase 2"))
            ;; --- Phase 2: live update(s) + delete ---
            ;; Wait for the FINAL state (name Alice2, age 33 after 3 bumps).
            (wait-until (lambda ()
                          (let ((lv (live)))
                            (and (= 1 (length lv))
                                 (string= "Alice2" (slot-value (first lv) 'name))
                                 (eql 33 (slot-value (first lv) 'age))))))
            (check (= 1 (length (live))) "live: exactly 1 live vertex (got ~D)" (length (live)))
            (let ((v (first (live))))
              (when v
                (check (string= "Alice2" (slot-value v 'name))
                       "live: name update propagated (name=~S)" (slot-value v 'name))
                (check (eql 33 (slot-value v 'age))
                       "live: all 3 age updates propagated (age=~S)" (slot-value v 'age))
                ;; MVCC: the slave built its OWN version chain (the master's
                ;; prev-pointers were heap addresses meaningless here).  Walk it:
                ;; it must terminate quickly and every archived head must be a
                ;; sane local heap offset -- a copied master address would dangle
                ;; or fail to read.  (keep-revisions=0, so it should be short.)
                (let* ((heap (graph-db::heap g))
                       (hi (graph-db::memory-pointer heap))
                       (p (graph-db::prev-pointer v))
                       (n 0) (ok t))
                  (loop (when (zerop p) (return))
                        (incf n)
                        (when (or (> n 16) (>= p hi)) (setf ok nil) (return))
                        (handler-case
                            (multiple-value-bind (dp ep pv)
                                (graph-db::read-archived-head g p)
                              (declare (ignore ep))
                              (when (or (>= dp hi) (>= pv hi)) (setf ok nil) (return))
                              (setf p pv))
                          (error () (setf ok nil) (return))))
                  (check ok "live: slave version chain is local + well-formed (depth ~D)" n))))
            (check (= 0 (length (edges)))
                   "live: edge gone after deleting Bob (got ~D)" (length (edges)))
            ;; --- Replicated spatial index + AO-boundary reconciliation (live) ---
            ;; After the live phase the Kharkiv area holds: Kharkiv site (catch-up),
            ;; Kharkiv live, and Crosser-in (entered the AO).  Crosser-out LEFT the
            ;; AO and was deleted on the slave -> 3 in-AO places near Kharkiv.
            (let ((kh (find-nodes-near 'r-place 49.2020d0 37.1724d0 2000d0 :graph g)))
              (check (= 3 (length kh))
                     "live: 3 in-AO places indexed after live replication (got ~D)"
                     (length kh)))
            ;; The boundary crossers, by label: Crosser-out must be GONE (it moved
            ;; out of the AO -> reconciled to a delete on the slave); Crosser-in
            ;; must be PRESENT (it moved in -> reconciled to a create on the slave).
            (let ((labels (mapcar (lambda (p) (slot-value p 'label))
                                  (map-vertices #'identity g :collect-p t
                                                             :vertex-type 'r-place))))
              (check (not (member "Crosser-out" labels :test 'string=))
                     "live: Crosser-out deleted on slave after LEAVING the AO")
              (check (member "Crosser-in" labels :test 'string=)
                     "live: Crosser-in created on slave after ENTERING the AO"))
            ;; Exercise version-aware GC over the replicated chains; must not error.
            (check (handler-case (progn (graph-db::gc-heap g) t)
                     (error (c) (format t "~&  gc-heap error: ~A~%" c) nil))
                   "live: gc-heap walks replicated version chains cleanly")))
        (close-graph g :snapshot-p nil))
      (write-flag "slave-done")
      (if (zerop *fails*)
          (progn (format t "~&SLAVE: PASS~%") (finish-output) (sexit 0))
          (progn (format t "~&SLAVE: FAIL (~D failed checks)~%" *fails*)
                 (finish-output) (sexit 1))))
  (error (c)
    (format t "~&SLAVE ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "slave-done"))
    (sexit 1)))
