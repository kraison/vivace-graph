;;;; Coverage for two open-path hygiene fixes:
;;;;
;;;;  - GH #222: a slashless LOCATION namestring used to keep LOCATION as a
;;;;    FILE pathname, so every sidecar built with
;;;;    (MAKE-PATHNAME :defaults (LOCATION GRAPH)) landed in the store's
;;;;    PARENT directory instead of inside it.
;;;;  - GH #224: a MAKE-GRAPH/OPEN-GRAPH call that fails partway through used
;;;;    to leak every fd it had already opened, leaked open vector segments
;;;;    and a replication listener/thread, could leave the graph half-
;;;;    registered in *GRAPHS*, and could delete a .dirty marker even after
;;;;    recovery/rebuild had already mutated the store (review round 1: C1,
;;;;    I2, I2b, I3).
;;;;
;;;; Both fixes live in graph.lisp; see %ABORT-GRAPH-OPEN, %GRAPH-OPEN-STATE
;;;; and the MAKE-GRAPH/OPEN-GRAPH LOCATION-normalization comments there.

(in-package #:graph-db/test)

(def-suite open-hygiene-suite
  :description "LOCATION normalization and aborted-open fd hygiene."
  :in graph-db-suite)

(in-suite open-hygiene-suite)

;; A tiny schema of our own, so these tests don't share
;; *INTEGRATION-GRAPH-NAME*'s graph identity with the rest of the suite.
(def-vertex oh-thing () ((label :type string)) :oh-graph)
;; A :VECTOR-INDEX owner, for the vector-segment leak coverage (GH #224
;; review I2) -- same graph, so it shares OH-THING's schema/directory story.
(def-vertex oh-vec () ((embedding :vector-index t)) :oh-graph)

(defun %oh-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i))
                                                'single-float)))))

;;; ---------------------------------------------------------------------------
;;; fd-count helper
;;; ---------------------------------------------------------------------------

(defun %oh-fd-count ()
  "Open fd count for this process, via SBCL's DIRECTORY over
/proc/self/fd -- internally consistent across calls (a fixed small
offset from the readdir fd itself), which is all a before/after delta
needs.  Linux-only (see the #-LINUX SKIP in every caller).  CAVEAT (GH
#224 review M1): DIRECTORY resolves each /proc/self/fd/N entry as a
pathname, and a socket or pipe fd's target (e.g. \"socket:[12345]\") is
not a filesystem path -- SBCL silently drops those rather than
counting them.  A leaked listening socket (the I2b hazard) therefore
does NOT move this number; the I2b coverage below asserts call
ORDERING instead of relying on this counter to see a socket leak."
  (length (directory "/proc/self/fd/*")))

(defmacro %oh-skip-unless-linux (&body body)
  `(progn
     #-linux (skip "fd counting here is Linux-only (/proc/self/fd)")
     #+linux (progn ,@body)))

;;; ---------------------------------------------------------------------------
;;; Aborted-open injection: force one internal opener to signal, so
;;; MAKE-GRAPH/OPEN-GRAPH fail after several other components are already
;;; open.  MAKE-VEV-INDEX/OPEN-VEV-INDEX are the LAST resource MAKE-INSTANCE
;;; opens in both functions (vertex-table, edge-table, heap, indexes,
;;; ve-index-in and ve-index-out all precede it), so injecting there proves
;;; the abort path runs with real partial state already open.  Later tests
;;; inject at %REGISTER-OPEN-STORE and INSTALL-SECONDARY-INDEXES instead, to
;;; reach the GRAPH-branch of %ABORT-GRAPH-OPEN the early injection never
;;; exercises (GH #224 review I3).
;;; ---------------------------------------------------------------------------

(defmacro %oh-with-injected-failure (fn-name &body body)
  "Redefine the internal GRAPH-DB function named by FN-NAME (a symbol) to
unconditionally signal an error, run BODY, then always restore the
original definition -- even if BODY itself signals."
  (let ((orig (gensym "ORIG")) (fn (gensym "FN")))
    `(let* ((,fn ,fn-name)
            (,orig (fdefinition ,fn)))
       (unwind-protect
            (progn
              (setf (fdefinition ,fn)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (error "GH #224 test: injected open failure")))
              ,@body)
         (setf (fdefinition ,fn) ,orig)))))

(defun %oh-dirty-file (path)
  (merge-pathnames ".dirty" (uiop:ensure-directory-pathname path)))

;;; ---------------------------------------------------------------------------
;;; GH #222
;;; ---------------------------------------------------------------------------

(test slashless-location-keeps-sidecars-inside-the-store
  "A slashless LOCATION namestring must not scatter .dirty/heap.dat/
schema.dat into the store's parent directory, and a graph created and
read that way must reopen and read back correctly (GH #222)."
  (with-temp-directory (dir)
    (let* ((parent (uiop:temporary-directory))
           (slashless (string-right-trim "/" (namestring dir)))
           id)
      (let ((g (make-graph :oh-graph slashless :buffer-pool-size 1000)))
        (is (probe-file (%oh-dirty-file slashless))
            "~A/.dirty must exist while the graph is open" slashless)
        (is (not (probe-file (merge-pathnames ".dirty" parent)))
            "no .dirty must leak into the parent directory")
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-oh-thing :label "inside")))))
        (close-graph g :snapshot-p nil))
      (is (probe-file (merge-pathnames
                        "heap.dat"
                        (uiop:ensure-directory-pathname slashless)))
          "heap.dat must exist inside the store directory")
      (is (not (probe-file (merge-pathnames "heap.dat" parent)))
          "heap.dat must not leak into the parent directory")
      (is (not (probe-file (merge-pathnames "schema.dat" parent)))
          "schema.dat must not leak into the parent directory")
      (let ((g2 (open-graph :oh-graph slashless :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g2))
               (is (string= "inside" (slot-value (lookup-vertex id) 'label))
                   "data written under the slashless location must survive ~
                    a reopen through the same slashless string"))
          (close-graph g2 :snapshot-p nil))))
    (collect-garbage)))

;;; ---------------------------------------------------------------------------
;;; GH #224 -- early injection (before MAKE-INSTANCE returns; GRAPH is NIL
;;; throughout the abort, so these only exercise %ABORT-GRAPH-OPEN's RAW-list
;;; branch, not its GRAPH-slot branch).
;;; ---------------------------------------------------------------------------

(test aborted-make-graph-does-not-leak-fds
  "Ten aborted MAKE-GRAPH calls, each failing after most of its resources
are already open, must not leak fds and must not half-register the graph
(GH #224)."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir))
             (before (%oh-fd-count)))
        (is (plusp before) "fd counting looks broken (0 fds reported)")
        (%oh-with-injected-failure 'graph-db::make-vev-index
          (dotimes (i 10)
            (signals error
              (make-graph :oh-graph
                          (format nil "~A/g~D/" path i)
                          :buffer-pool-size 1000))))
        (let ((after (%oh-fd-count)))
          (is (<= after (+ before 3))
              "fd count grew from ~D to ~D across 10 aborted make-graph ~
               calls"
              before after))
        (is (null (graph-db:lookup-graph :oh-graph))
            "an aborted make-graph must not leave a half-registered graph"))
      (collect-garbage))))

(test aborted-open-graph-does-not-leak-fds
  "Ten aborted OPEN-GRAPH calls against a valid, cleanly-closed store must
not leak fds, must not half-register the graph, and must not corrupt the
store -- a subsequent un-injected OPEN-GRAPH still succeeds and reads the
data back (GH #224)."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir)) id)
        (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-oh-thing :label "survives")))))
          (close-graph g :snapshot-p nil))
        (let ((before (%oh-fd-count)))
          (is (plusp before) "fd counting looks broken (0 fds reported)")
          (%oh-with-injected-failure 'graph-db::open-vev-index
            (dotimes (i 10)
              (signals error
                (open-graph :oh-graph path :buffer-pool-size 1000))))
          (let ((after (%oh-fd-count)))
            (is (<= after (+ before 3))
                "fd count grew from ~D to ~D across 10 aborted open-graph ~
                 calls"
                before after)))
        (is (null (graph-db:lookup-graph :oh-graph))
            "an aborted open-graph must not leave a half-registered graph")
        (let ((g2 (open-graph :oh-graph path :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (string= "survives" (slot-value (lookup-vertex id)
                                                      'label))
                     "a subsequent un-injected open-graph must still read ~
                      back data written before the aborted opens"))
            (close-graph g2 :snapshot-p nil))))
      (collect-garbage))))

;;; ---------------------------------------------------------------------------
;;; GH #224 -- late injection: GRAPH is fully constructed, registered, and
;;; (for the OPEN-GRAPH case) past the point recovery/rebuild can mutate the
;;; store, before the injected failure fires.  These are what the early
;;; tests above cannot reach: %ABORT-GRAPH-OPEN's GRAPH-slot branch, the
;;; .dirty MUTATED-gated deletion rule (review C1), and the vector-segment /
;;; replication-ordering cleanup (review I2 / I2b).
;;; ---------------------------------------------------------------------------

(test aborted-make-graph-late-registration-cleans-up
  "A MAKE-GRAPH aborted at %REGISTER-OPEN-STORE -- GRAPH fully built, every
fd open, but nothing that writes against a PRE-EXISTING store's heap has
run (a fresh MAKE-GRAPH has no prior state to leave stale) -- must close
every resource, deregister, and delete the .dirty marker THIS open wrote,
so a subsequent OPEN-GRAPH succeeds directly with no recovery step (GH
#224 review C1, I3)."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir))
             (before (%oh-fd-count)))
        (%oh-with-injected-failure 'graph-db::%register-open-store
          (signals error
            (make-graph :oh-graph path :buffer-pool-size 1000)))
        (let ((after (%oh-fd-count)))
          (is (<= after (+ before 3))
              "fd count grew from ~D to ~D across one late-aborted ~
               make-graph"
              before after))
        (is (null (graph-db:lookup-graph :oh-graph))
            "an aborted make-graph must not leave a half-registered graph")
        (is (not (probe-file (%oh-dirty-file path)))
            "nothing mutating ran, so .dirty (this open's own) must be ~
             gone -- otherwise a later open would wrongly demand recovery")
        (let ((g (open-graph :oh-graph path :buffer-pool-size 1000)))
          (is (graph-db::graph-open-p g)
              "the recovered store must open cleanly")
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))

(test aborted-open-graph-early-registration-deletes-dirty
  "An OPEN-GRAPH aborted at %REGISTER-OPEN-STORE -- before recovery/rebuild
(GC-HEAP, RECOVER-TRANSACTIONS, etc.) has run, so nothing mutated the
store past what the previous CLOSE-GRAPH already made durable -- must
delete the .dirty marker THIS open wrote, and a subsequent OPEN-GRAPH must
succeed with no separate recovery step (GH #224 review C1, I3)."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir)) id)
        (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-oh-thing :label "early")))))
          (close-graph g :snapshot-p nil))
        (let ((before (%oh-fd-count)))
          (%oh-with-injected-failure 'graph-db::%register-open-store
            (signals error
              (open-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((after (%oh-fd-count)))
            (is (<= after (+ before 3))
                "fd count grew from ~D to ~D across one late-aborted ~
                 open-graph"
                before after)))
        (is (null (graph-db:lookup-graph :oh-graph))
            "an aborted open-graph must not leave a half-registered graph")
        (is (not (probe-file (%oh-dirty-file path)))
            "nothing mutating ran, so .dirty (this open's own) must be ~
             gone")
        (let ((g2 (open-graph :oh-graph path :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (string= "early" (slot-value (lookup-vertex id)
                                                   'label))
                     "the un-injected reopen must still read back the ~
                      data, with no manual recovery step needed"))
            (close-graph g2 :snapshot-p nil))))
      (collect-garbage))))

(test aborted-open-graph-late-injection-retains-dirty-when-mutated
  "An OPEN-GRAPH aborted at INSTALL-SECONDARY-INDEXES -- well past
GC-HEAP/RECOVER-TRANSACTIONS/REBUILD-SPATIAL-INDEXES/REBUILD-UNIQUE-
INDEXES, all of which can write against the store's heap -- must NOT
delete .dirty: the store now genuinely needs recovery, and a later open
adopting the OLD index roots against this now-mutated heap (with no
recovery pass to catch the mismatch) would be silently wrong (GH #224
review C1).  Once a recovery step clears .dirty (simulated here by
deleting it directly, mirroring what a real operator's recovery run
would leave behind), a following OPEN-GRAPH succeeds and the data
survives."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir)) id)
        (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-oh-thing :label "recovered")))))
          (close-graph g :snapshot-p nil))
        (let ((before (%oh-fd-count)))
          (%oh-with-injected-failure 'graph-db::install-secondary-indexes
            (signals error
              (open-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((after (%oh-fd-count)))
            (is (<= after (+ before 3))
                "fd count grew from ~D to ~D across one late-aborted ~
                 open-graph"
                before after)))
        (is (null (graph-db:lookup-graph :oh-graph))
            "an aborted open-graph must not leave a half-registered graph")
        (is (probe-file (%oh-dirty-file path))
            "mutating steps ran -- .dirty must remain so a later open ~
             demands recovery rather than silently reopening")
        (signals error
          (open-graph :oh-graph path :buffer-pool-size 1000))
        ;; The operator's recovery step: clear the sentinel.
        (delete-file (%oh-dirty-file path))
        (let ((g2 (open-graph :oh-graph path :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (string= "recovered" (slot-value (lookup-vertex id)
                                                       'label))
                     "after recovery, a following open-graph must still ~
                      read back data written before the aborted opens"))
            (close-graph g2 :snapshot-p nil))))
      (collect-garbage))))

(test aborted-open-graph-closes-vector-segments
  "An OPEN-GRAPH aborted after RESTORE-VECTOR-SEGMENTS has opened a
segment -- which marks the on-disk clean flag DIRTY the moment it opens,
regardless of whether it will rebuild -- must close that segment during
the abort (setting the flag back to clean) exactly like CLOSE-GRAPH's own
MAPHASH over VECTOR-SEGMENTS does.  Otherwise the segment leaks its fd
and, worse, the NEXT open finds the flag still dirty and pays a full
rebuild-from-nodes it never needed (GH #224 review I2)."
  (%oh-skip-unless-linux
    (with-temp-directory (dir)
      (let* ((path (namestring dir)) id)
        (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-oh-vec :embedding (%oh-embedding 8 1.0))))))
          (close-graph g :snapshot-p nil))
        (let ((before (%oh-fd-count)))
          (%oh-with-injected-failure 'graph-db::install-secondary-indexes
            (signals error
              (open-graph :oh-graph path :buffer-pool-size 1000)))
          (let ((after (%oh-fd-count)))
            (is (<= after (+ before 3))
                "fd count grew from ~D to ~D across one late-aborted ~
                 open-graph (vector segment left open?)"
                before after)))
        (delete-file (%oh-dirty-file path))
        (let ((rebuild-warned nil))
          (handler-bind
              ((warning (lambda (c)
                          (when (search "not closed cleanly"
                                        (princ-to-string c))
                            (setq rebuild-warned t))
                          (muffle-warning c))))
            (let ((g2 (open-graph :oh-graph path :buffer-pool-size 1000)))
              (unwind-protect
                   (let ((*graph* g2))
                     (is (not rebuild-warned)
                         "the vector segment must have been closed cleanly ~
                          by the abort -- a dirty-segment rebuild warning ~
                          means it leaked open")
                     (is (equalp (%oh-embedding 8 1.0)
                                 (slot-value (lookup-vertex id) 'embedding))
                         "the embedding must survive the aborted-then- ~
                          recovered reopen"))
                (close-graph g2 :snapshot-p nil))))))
      (collect-garbage))))

(test aborted-open-graph-stops-replication-before-other-teardown
  "%ABORT-GRAPH-OPEN must call STOP-REPLICATION before any other teardown
step -- specifically before CLOSE-REPLICATION-LOG -- so a master's
accept-loop thread and listening socket are torn down before anything
referencing the graph's mmaps is closed; otherwise a retried open on the
same port fails EADDRINUSE (GH #224 review I2b).  STOP-REPLICATION and
CLOSE-REPLICATION-LOG are both temporarily wrapped with call-order spies
rather than exercised through a real master listener, to keep this
regression fast and independent of networking."
  (with-temp-directory (dir)
    (let* ((path (namestring dir)) (order nil))
      (let ((g (make-graph :oh-graph path :buffer-pool-size 1000)))
        (close-graph g :snapshot-p nil))
      (let ((orig-stop (fdefinition 'graph-db::stop-replication))
            (orig-log (fdefinition 'graph-db::close-replication-log)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::stop-replication)
                     (lambda (g)
                       (push :stop-replication order)
                       (funcall orig-stop g)))
               (setf (fdefinition 'graph-db::close-replication-log)
                     (lambda (g)
                       (push :close-replication-log order)
                       (funcall orig-log g)))
               (%oh-with-injected-failure 'graph-db::install-secondary-indexes
                 (signals error
                   (open-graph :oh-graph path :buffer-pool-size 1000))))
          (setf (fdefinition 'graph-db::stop-replication) orig-stop)
          (setf (fdefinition 'graph-db::close-replication-log) orig-log)
          (ignore-errors (delete-file (%oh-dirty-file path)))))
      (setf order (nreverse order))
      (is (member :stop-replication order)
          "stop-replication must run during the abort")
      (is (member :close-replication-log order)
          "close-replication-log must run during the abort")
      (is (< (position :stop-replication order)
             (position :close-replication-log order))
          "stop-replication must run BEFORE close-replication-log"))
    (collect-garbage)))
