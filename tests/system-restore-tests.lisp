;;;; Whole-system restore across a shadow swap (GH #171).  Spec:
;;;; docs/superpowers/specs/2026-08-23-restore-171-design.md
(in-package #:graph-db/test)

(def-suite system-restore-suite :in graph-db-suite
  :description "RETIRED-GENERATIONS, PRUNE, PLAN/RESTORE-SYSTEM (GH #171).")
(in-suite system-restore-suite)

(defmacro with-restore-system ((g clock sys &key (policy :authored))
                               &body body)
  "WITH-CLOCKED-STORE's shape, with the store named :RESTORE-STORE-1 and a
persisted recovery POLICY.  G is SETQ-able: tests rebind it to whatever
SWAP-IN-SHADOW / RESTORE-SYSTEM return."
  (let ((cdir (gensym)) (ddir (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,cdir)
         (with-temp-directory (,ddir)
           (let ((graph-db::*system-directory* (namestring ,sys))
                 (graph-db::*store-registry* nil))
             (let ((,clock (open-system-clock (namestring ,cdir))))
               (unwind-protect
                    (let ((,g (make-graph :restore-store-1
                                          (namestring ,ddir)
                                          :buffer-pool-size 1000
                                          :system-clock ,clock
                                          :recovery-policy ,policy)))
                      (unwind-protect (progn ,@body)
                        ;; Close whatever is actually registered, not the
                        ;; stale local G -- a test that never re-SETQs G
                        ;; after RESTORE-SYSTEM/SWAP-IN-SHADOW reopens a
                        ;; new object under the same name, and closing the
                        ;; old one is a no-op that leaks the store-id
                        ;; across tests (GH #171).
                        (let ((live (graph-db:lookup-graph :restore-store-1)))
                          (when (and live (graph-db::graph-open-p live))
                            (let ((graph-db:*graph* live))
                              (ignore-errors
                               (close-graph live :snapshot-p nil)))))
                        (collect-garbage)))
                 (close-system-clock ,clock)))))))))

(defun %rs-write (g)
  "One generic vertex into G; returns the epoch the commit used."
  (with-transaction ((graph-db::transaction-manager g))
    (graph-db:make-vertex :generic nil :graph g))
  (graph-db::load-highest-transaction-id g))

(defun %rs-count (g)
  (length (graph-db:map-vertices #'identity g :collect-p t)))

(defun %rs-key-count (plist key)
  "How many times KEY appears as an indicator in PLIST -- 1 is what a
%ENTRY-WITH replacement must leave behind, 2 means a stale shadowed
value survived (GH #171)."
  (loop for (k nil) on plist by #'cddr count (eq k key)))

(defun %rs-swap (g clock &key (shadow-writes 1) (name :restore-store-1))
  "Shadow G, write SHADOW-WRITES vertices into the shadow, swap it in.
Returns (values NEW-GRAPH RETIRED-PATH)."
  (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
    (multiple-value-bind (start end) (graph-db:clock-lease-epochs clock 1000)
      (let ((sg (graph-db:open-shadow-graph
                 shadow-location name :lease (cons start end))))
        (dotimes (i shadow-writes)
          (with-transaction ((graph-db::transaction-manager sg))
            (graph-db:make-vertex :generic nil :graph sg)))
        (let ((graph-db:*graph* sg)) (close-graph sg :snapshot-p nil))))
    (graph-db:swap-in-shadow g2 shadow-location)))

(test retired-generations-joins-directory-and-journal
  "After one swap: exactly one generation for the store, PRESENT, JOURNALED,
its SWAP-EPOCH equal to the :SWAP record's :epoch, POLICY read from the
RETIRED directory's policy.dat."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng retired) (%rs-swap g clock)
      (setq g ng)
      (let ((gens (graph-db:retired-generations clock)))
        (is (= 1 (length gens)))
        (let ((gen (first gens))
              (swap (find :swap (journal-records clock)
                          :key (lambda (r) (getf r :kind)))))
          (is (eq :restore-store-1 (graph-db:generation-store gen)))
          (is (string= (string-right-trim "/" retired)
                       (graph-db:generation-retired gen)))
          (is (= (getf swap :epoch) (graph-db:generation-swap-epoch gen)))
          (is-true (graph-db:generation-journaled-p gen))
          (is-true (graph-db:generation-present-p gen))
          (is (eq :authored (graph-db:generation-policy gen))))))))

(test retired-generations-tolerates-a-missing-swap-record
  "The #212 shape: the retired directory exists but its :SWAP record was
never written.  The generation is still listed (JOURNALED NIL) and
SWAP-RECORD-MISSING-WARNING names the path.  Ablation: a reader that trusts
the journal alone lists nothing."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng retired) (%rs-swap g clock)
      (setq g ng)
      ;; Drop the :SWAP line from the journal file by rewriting it.
      (let ((file (graph-db::%clock-journal-file
                   (graph-db::system-clock-location clock)))
            (kept (remove :swap (journal-records clock)
                          :key (lambda (r) (getf r :kind)))))
        (close-system-clock clock)
        (with-open-file (out file :direction :output :if-exists :supersede)
          (let ((*print-readably* nil) (*print-pretty* nil))
            (dolist (r kept) (format out "~S~%" r))))
        (setq clock (open-system-clock
                     (namestring (graph-db::system-clock-location clock))))
        (let* ((warned nil)
               (gens (handler-bind
                         ((graph-db:swap-record-missing-warning
                            (lambda (w)
                              (setq warned
                                    (graph-db:swap-record-missing-path w))
                              (muffle-warning w))))
                       (graph-db:retired-generations clock))))
          (is (= 1 (length gens)))
          (is-false (graph-db:generation-journaled-p (first gens)))
          (is (string= (string-right-trim "/" retired) warned))
          (is (= (parse-integer retired
                                :start (1+ (position #\- retired :from-end t)))
                 (graph-db:generation-swap-epoch (first gens)))))))))

(test retired-generations-reports-a-pruned-directory
  "A :SWAP record whose directory is gone lists as PRESENT NIL."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng retired) (%rs-swap g clock)
      (setq g ng)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname retired)
                                  :validate t)
      (let ((gens (graph-db:retired-generations clock)))
        (is (= 1 (length gens)))
        (is-false (graph-db:generation-present-p (first gens)))
        (is-true (graph-db:generation-journaled-p (first gens)))))))

(defun %rs-gen-epochs (clock)
  (mapcar #'graph-db:generation-swap-epoch
          (graph-db:retired-generations clock)))

(test prune-deletes-generations-at-or-below-the-floor
  "Two swaps; floor = first swap's epoch: the first generation goes, the
second stays; a :RETIRE record names the deleted path."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (setq g ng)
      (multiple-value-bind (ng2 r2) (%rs-swap g clock)
        (setq g ng2)
        (destructuring-bind (e1 e2) (%rs-gen-epochs clock)
          (let ((gone (graph-db:prune-retired-generations clock e1)))
            (is (= 1 (length gone)))
            (is (= e1 (graph-db:generation-swap-epoch (first gone))))
            (is-false (probe-file (uiop:ensure-directory-pathname r1)))
            (is-true (probe-file (uiop:ensure-directory-pathname r2)))
            (is (equal (list e2) (%rs-gen-epochs clock)))
            (let ((retire (find :retire (journal-records clock)
                                :key (lambda (r) (getf r :kind)))))
              (is (string= (string-right-trim "/" r1)
                           (getf retire :retired)))
              (is (= e1 (getf retire :swap-epoch))))))))))

(test prune-refuses-an-authored-generation-inside-the-window
  "Floor below the swap epoch on an :AUTHORED store: RETENTION-REQUIRED-
ERROR naming the generation, directory untouched.  Ablation: no guard
deletes the only copy of authored data."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (setq g ng)
      (let* ((e1 (first (%rs-gen-epochs clock)))
             (c (handler-case
                    (progn (graph-db:prune-retired-generations clock (1- e1))
                           nil)
                  (graph-db:retention-required-error (c) c))))
        (is-true c)
        (when c
          (is (= 1 (length (graph-db:retention-required-generations c)))))
        (is-true (probe-file (uiop:ensure-directory-pathname r1)))))))

(test prune-deletes-a-derivable-generation-only-when-told
  (with-restore-system (g clock sys :policy :derivable)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (setq g ng)
      (let ((e1 (first (%rs-gen-epochs clock))))
        (is (null (graph-db:prune-retired-generations clock (1- e1))))
        (is-true (probe-file (uiop:ensure-directory-pathname r1)))
        (is (= 1 (length (graph-db:prune-retired-generations
                          clock (1- e1) :discard-derivable t))))
        (is-false (probe-file (uiop:ensure-directory-pathname r1)))))))

(test prune-dry-run-deletes-nothing
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (setq g ng)
      (let ((e1 (first (%rs-gen-epochs clock))))
        (is (= 1 (length (graph-db:prune-retired-generations
                          clock e1 :dry-run t))))
        (is-true (probe-file (uiop:ensure-directory-pathname r1)))
        (is-false (find :retire (journal-records clock)
                        :key (lambda (r) (getf r :kind))))))))

(defun %rs-entry (manifest store)
  (find store (getf manifest :stores)
        :key (lambda (e) (getf e :store))))

(test plan-selects-the-retained-generation-exactly
  "Write at E0, swap at E3 > T >= E0: the plan is :REWOUND :EXACT T with
:STATE-AT E0 and :FROM the retired path; nothing on disk changes."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (let* ((tt (1+ e0))
               (m (graph-db:plan-system-restore clock tt))
               (e (%rs-entry m :restore-store-1)))
          (is (= tt (getf m :requested)))
          (is (eq :rewound (getf e :action)))
          (is (eq t (getf e :exact)))
          (is (= e0 (getf e :state-at)))
          (is (string= (string-right-trim "/" r1) (getf e :from)))
          (is-true (graph-db::graph-open-p g)))))))

(test plan-marks-inexact-when-writes-follow-t
  "T before the generation's last commit: :EXACT NIL, :STATE-AT E0.  With
:REQUIRE-EXACT the plan refuses with reason :INEXACT instead."
  (with-restore-system (g clock sys)
    sys
    (let* ((t0 (%rs-write g))
           (e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((e (%rs-entry (graph-db:plan-system-restore clock t0)
                            :restore-store-1)))
          (is (eq :rewound (getf e :action)))
          (is (null (getf e :exact)))
          (is (= e0 (getf e :state-at))))
        (let ((c (handler-case
                     (progn (graph-db:plan-system-restore clock t0
                                                          :require-exact t)
                            nil)
                   (graph-db:restore-refused-error (c) c))))
          (is-true c)
          (when c
            (is (equal '((:restore-store-1 . :inexact))
                       (graph-db:restore-refused-reasons c)))))))))

(test plan-leaves-an-unaffected-store-unchanged
  "T after the swap: :UNCHANGED."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (declare (ignore r1))
      (setq g ng)
      (let* ((now (graph-db:clock-current-epoch clock))
             (e (%rs-entry (graph-db:plan-system-restore clock now)
                           :restore-store-1)))
        (is (eq :unchanged (getf e :action)))))))

(test plan-refuses-when-an-authored-generation-is-gone
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname r1)
                                    :validate t)
        (let ((c (handler-case
                     (progn (graph-db:plan-system-restore clock e0) nil)
                   (graph-db:restore-refused-error (c) c))))
          (is-true c)
          (when c
            (is (equal '((:restore-store-1 . :authored-generation-missing))
                       (graph-db:restore-refused-reasons c)))))))))

(test plan-rebuilds-a-derivable-store-whose-generation-is-gone
  "Derivable, generation pruned: with :REBUILD the plan is :REBUILT :EXACT
NIL; without it, refused with reason :NO-REBUILD."
  (with-restore-system (g clock sys :policy :derivable)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname r1)
                                    :validate t)
        (let ((e (%rs-entry (graph-db:plan-system-restore
                             clock e0 :rebuild (lambda (name graph)
                                                 (declare (ignore name graph))))
                            :restore-store-1)))
          (is (eq :rebuilt (getf e :action)))
          (is (null (getf e :exact))))
        (let ((c (handler-case
                     (progn (graph-db:plan-system-restore clock e0) nil)
                   (graph-db:restore-refused-error (c) c))))
          (is-true c)
          (when c
            (is (equal '((:restore-store-1 . :no-rebuild))
                       (graph-db:restore-refused-reasons c)))))))))

(test restore-puts-the-retained-generation-back-and-round-trips
  "1 vertex, swap (+1), restore to before the swap: 1 vertex readable,
store open and accepting; journal carries :RETIRE-LIVE then :RESTORE; the
post-swap generation is itself retained, and a restore to NOW-1 of that
state puts the 2-vertex generation back."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (is (= 2 (%rs-count g)))
        (let ((after-swap (graph-db:clock-current-epoch clock)))
          (let ((m (graph-db:restore-system clock e0)))
            (setq g (graph-db:lookup-graph :restore-store-1))
            (is (eq :rewound (getf (%rs-entry m :restore-store-1) :action)))
            (is (= 1 (%rs-count g)))
            (with-transaction ((graph-db::transaction-manager g))
              (graph-db:make-vertex :generic nil :graph g))
            (let ((kinds (mapcar (lambda (r) (getf r :kind))
                                 (journal-records clock))))
              (is-true (member :retire-live kinds))
              (is-true (member :restore kinds))
              (is (< (position :retire-live kinds)
                     (position :restore kinds)))))
          ;; Round trip: the 2-vertex generation was retired, not lost.
          (graph-db:restore-system clock after-swap)
          (setq g (graph-db:lookup-graph :restore-store-1))
          (is (= 2 (%rs-count g))))))))

(test restore-refuses-before-any-rename
  ":REQUIRE-EXACT on an inexact plan: nothing moved, graph still open and
accepting, no new journal records."
  (with-restore-system (g clock sys)
    sys
    (let ((t0 (%rs-write g)))
      (%rs-write g)
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (let ((n (length (journal-records clock))))
          (signals graph-db:restore-refused-error
            (graph-db:restore-system clock t0 :require-exact t))
          (is (= n (length (journal-records clock))))
          (is-true (probe-file (uiop:ensure-directory-pathname r1)))
          (is-true (graph-db::graph-open-p g))
          (with-transaction ((graph-db::transaction-manager g))
            (graph-db:make-vertex :generic nil :graph g)))))))

(test restore-writes-a-readable-manifest-and-warns-when-inexact
  (with-restore-system (g clock sys)
    sys
    (let ((t0 (%rs-write g)))
      (%rs-write g)
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let* ((warned nil)
               (m (handler-bind
                      ((graph-db:restore-inexact-warning
                         (lambda (w) (setq warned t) (muffle-warning w))))
                    (graph-db:restore-system clock t0)))
               (file (merge-pathnames
                      (format nil "restore-~D.manifest" (getf m :at))
                      (uiop:ensure-directory-pathname
                       (graph-db::system-clock-location clock)))))
          (is-true warned)
          (is-true (probe-file file))
          (is (equal m (graph-db:read-restore-manifest file))))))))

(test restore-rebuilds-a-derivable-store-through-the-callback
  "Generation pruned, :REBUILD supplied: the callback runs against a fresh
empty graph at the live location; its writes are what the store holds
afterwards; manifest :REBUILT."
  (with-restore-system (g clock sys :policy :derivable)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname r1)
                                    :validate t)
        (let* ((seen nil)
               (m (handler-bind
                      ((graph-db:restore-inexact-warning #'muffle-warning))
                    (graph-db:restore-system
                     clock e0
                     :rebuild (lambda (name graph)
                                (setq seen name)
                                (is (= 0 (%rs-count graph)))
                                (dotimes (i 3)
                                  (with-transaction
                                      ((graph-db::transaction-manager graph))
                                    (graph-db:make-vertex :generic nil
                                                          :graph graph))))))))
          (setq g (graph-db:lookup-graph :restore-store-1))
          (is (eq :restore-store-1 seen))
          (is (eq :rebuilt (getf (%rs-entry m :restore-store-1) :action)))
          (is (= 3 (%rs-count g)))
          (is (eq :derivable (graph-db:store-recovery-policy
                              (graph-db::location g)))))))))

(defmacro with-second-store ((g2 clock name dir-var &key (policy :derivable))
                             &body body)
  `(with-temp-directory (,dir-var)
     (let ((,g2 (make-graph ,name (namestring ,dir-var)
                            :buffer-pool-size 1000 :system-clock ,clock
                            :recovery-policy ,policy)))
       (unwind-protect (progn ,@body)
         (let ((live (graph-db:lookup-graph ,name)))
           (when (and live (graph-db::graph-open-p live))
             (let ((graph-db:*graph* live))
               (ignore-errors (close-graph live :snapshot-p nil)))))))))

(test restore-cascades-to-a-derivable-dependent-and-reports-an-authored-one
  "Store A (derivable) is rebuilt.  Store B (derivable) holds an edge into
A: B is rebuilt too (callback sees both names, A first).  Store C
(authored) holds an edge into A: untouched, manifest :DANGLING 1."
  (with-restore-system (ga clock sys :policy :derivable)
    sys
    (with-second-store (gb clock :restore-store-b bdir :policy :derivable)
      (with-second-store (gc clock :restore-store-c cdir :policy :authored)
        (let (a-id)
          (with-transaction ((graph-db::transaction-manager ga))
            (setq a-id (graph-db:id (graph-db:make-vertex :generic nil
                                                           :graph ga))))
          (dolist (gx (list gb gc))
            (with-transaction ((graph-db::transaction-manager gx))
              (let ((v (graph-db:make-vertex :generic nil :graph gx)))
                (graph-db:make-edge :generic (graph-db:id v) a-id nil nil
                                    :graph gx))))
          (let ((e0 (graph-db:clock-current-epoch clock)))
            (multiple-value-bind (ng r1) (%rs-swap ga clock)
              (setq ga ng)
              (uiop:delete-directory-tree
               (uiop:ensure-directory-pathname r1) :validate t)
              (let* ((order nil)
                     (m (handler-bind
                            ((graph-db:restore-inexact-warning
                               #'muffle-warning))
                          (graph-db:restore-system
                           clock e0
                           :rebuild (lambda (name graph)
                                      (declare (ignore graph))
                                      (push name order))))))
                (is (equal '(:restore-store-1 :restore-store-b)
                           (reverse order)))
                (is (eq :rebuilt (getf (%rs-entry m :restore-store-b)
                                       :action)))
                (is (eq :restore-store-1
                        (getf (%rs-entry m :restore-store-b)
                              :cascade-from)))
                (is (eq :unchanged (getf (%rs-entry m :restore-store-c)
                                         :action)))
                (is (= 1 (getf (%rs-entry m :restore-store-c)
                               :dangling)))))))))))

;;; Fix round 1 (GH #171): C1, C2, I4, I6.

(test plan-refuses-a-store-that-is-not-open
  "The target generation exists and would normally be REWOUND, but the
store itself was closed (not detached): C1 refuses :NOT-OPEN before any
rename -- not a LOCATION error on a NIL graph partway through a
multi-store restore."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((graph-db:*graph* g))
          (close-graph g :snapshot-p nil))
        (let ((c (handler-case
                     (progn (graph-db:plan-system-restore clock e0) nil)
                   (graph-db:restore-refused-error (c) c))))
          (is-true c)
          (when c
            (is (equal '((:restore-store-1 . :not-open))
                       (graph-db:restore-refused-reasons c)))))))))

(test plan-after-a-restore-does-not-see-the-consumed-generation
  "Restore to T once; planning to the SAME T again must not refuse
:AUTHORED-GENERATION-MISSING against a ghost entry for the generation
the restore just consumed (I4), AND must come back :UNCHANGED, not
:REWOUND (fix round 2): LIVE-FROM/%GENERATION-LIVE-AT's interval rule
recognizes that the remaining retired generation's live interval
starts AFTER T (at the original swap epoch), so it does not contain T
-- the CURRENT live directory is what covers T now, exactly as it
should after a completed restore.  Executing a :REWOUND plan here
would have been a real regression: it would revert the just-completed
restore back to the pre-restore (2-vertex) content."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (handler-bind
            ((graph-db:restore-inexact-warning #'muffle-warning))
          (graph-db:restore-system clock e0))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is (= 1 (%rs-count g)))
        (let* ((refused nil)
               (m (handler-case
                      (handler-bind
                          ((graph-db:restore-inexact-warning
                             #'muffle-warning))
                        (graph-db:plan-system-restore clock e0))
                    (graph-db:restore-refused-error (c) (setq refused c)
                      nil))))
          (is-false refused)
          (when m
            (is (eq :unchanged
                    (getf (%rs-entry m :restore-store-1) :action)))))))))

(test plan-after-a-restore-selects-the-post-swap-generation-by-interval
  "After restoring to T=e0, planning to an epoch INSIDE the retired
post-swap generation's live interval selects THAT generation: :REWOUND
with :FROM its path.  MID is a further write committed to the swapped-
in generation AFTER the swap and BEFORE the restore -- a concrete
epoch strictly between the swap's own epoch and the restore's later
:RETIRE-LIVE epoch, unlike the epoch read immediately after the swap
completes, which (nothing else advancing the clock in between) can
land EXACTLY ON the :RETIRE-LIVE epoch -- the half-open interval's
excluded upper boundary, which the companion test above already
covers by landing there and getting :UNCHANGED."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((mid (%rs-write g)))
          (let ((m0 (handler-bind
                        ((graph-db:restore-inexact-warning
                           #'muffle-warning))
                      (graph-db:restore-system clock e0))))
            (setq g (graph-db:lookup-graph :restore-store-1))
            (let* ((post-swap-path
                     (getf (%rs-entry m0 :restore-store-1) :retired-live))
                   (e (%rs-entry (graph-db:plan-system-restore clock mid)
                                 :restore-store-1)))
              (is-true post-swap-path)
              (is (eq :rewound (getf e :action)))
              (is (string= post-swap-path (getf e :from))))))))))

(defmacro with-posix-rename-failing-when ((old-var new-var test-form)
                                          &body body)
  "Install a wrapper on GRAPH-DB::%POSIX-RENAME for BODY's dynamic
extent that signals an error the FIRST time (OLD-VAR NEW-VAR) satisfies
TEST-FORM and delegates every other call -- including later ones -- to
the original.  Matched by PATH, not a raw call count: CLOSE-GRAPH's own
snapshot promotes sidecar .tmp files via this same primitive, and a
naive Nth-call counter would fail the WRONG rename depending on how
many sidecars a graph happens to carry (I6's fault-injection mechanism,
mirroring the FDEFINITION-swap idiom used throughout tests/, e.g.
SEGMENT-TESTS' %POSIX-MMAP wrapper) (GH #171)."
  (let ((orig (gensym "ORIG")) (fired (gensym "FIRED")))
    `(let ((,orig (fdefinition 'graph-db::%posix-rename))
           (,fired nil))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db::%posix-rename)
                    (lambda (,old-var ,new-var)
                      (declare (ignorable ,old-var ,new-var))
                      (if (and (not ,fired) ,test-form)
                          (progn
                            (setq ,fired t)
                            (error "injected rename failure for the ~
test"))
                          (funcall ,orig ,old-var ,new-var))))
              ,@body)
         (setf (fdefinition 'graph-db::%posix-rename) ,orig)))))

(defmacro with-reopen-failing-once (&body body)
  "Install a wrapper on GRAPH-DB::%REOPEN-AND-RESUME that fails its
FIRST call and delegates every later call to the original (GH #171)."
  (let ((orig (gensym "ORIG")) (fired (gensym "FIRED")))
    `(let ((,orig (fdefinition 'graph-db::%reopen-and-resume))
           (,fired nil))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db::%reopen-and-resume)
                    (lambda (name location clock reason)
                      (if ,fired
                          (funcall ,orig name location clock reason)
                          (progn
                            (setq ,fired t)
                            (error "injected reopen failure for the test")))))
              ,@body)
         (setf (fdefinition 'graph-db::%reopen-and-resume) ,orig)))))

(test restore-second-rename-failure-restores-service
  "The second rename (retained generation -> live) fails: the original
error is resignalled, the live directory is renamed back, the store
ends up open and accepting again, and the journal carries :RETIRE-LIVE
followed by :RETIRE-LIVE-ABORTED (I3a, I6)."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((live (graph-db::%trimmed-namestring
                     (graph-db::location g))))
          (with-posix-rename-failing-when
              (old new (string= (graph-db::%trimmed-namestring new) live))
            (signals error (graph-db:restore-system clock e0))))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is-true (graph-db::graph-open-p g))
        (is (eq t (graph-db:accepting-p
                   (graph-db::transaction-manager g))))
        (is (= 2 (%rs-count g)))
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db:make-vertex :generic nil :graph g))
        (let ((kinds (mapcar (lambda (r) (getf r :kind))
                             (journal-records clock))))
          (is-true (member :retire-live kinds))
          (is-true (member :retire-live-aborted kinds))
          (is (< (position :retire-live kinds)
                 (position :retire-live-aborted kinds))))))))

(test restore-recovers-onto-the-new-generation-after-a-reopen-failure
  "Both renames land; the follow-up reopen fails once: SWAP-RECOVERED-
WARNING is signalled (not a resignalled error), and the RESTORED
generation ends up live (I6)."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((warned nil))
          (with-reopen-failing-once
            (handler-bind
                ((graph-db:swap-recovered-warning
                   (lambda (w) (setq warned t) (muffle-warning w))))
              (graph-db:restore-system clock e0)))
          (is-true warned))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is-true (graph-db::graph-open-p g))
        (is (= 1 (%rs-count g)))))))

(test plan-handles-two-swaps-by-picking-the-generation-live-at-t
  "Swap twice.  T between the swaps selects the SECOND retired
generation (the one live at T), not the first."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (multiple-value-bind (ng r1) (%rs-swap g clock)
      (declare (ignore r1))
      (setq g ng)
      (let ((mid (%rs-write g)))
        (multiple-value-bind (ng2 r2) (%rs-swap g clock)
          (setq g ng2)
          (let ((e (%rs-entry (graph-db:plan-system-restore clock mid)
                              :restore-store-1)))
            (is (eq :rewound (getf e :action)))
            (is (string= (string-right-trim "/" r2) (getf e :from)))))))))

;;; Fix round 3 (GH #171): the ERAS model, and the guards round 2 left
;;; unexercised.

(test plan-selects-a-generation-by-an-inherited-era
  "Three events: swap@E1 retires r1, restore-to-E0@E2 promotes r1 and
retires r2, swap@E3 retires the promoted directory as r3.  E0's content
now sits in r3, not in r1 (consumed) and not in r2 (a later era), so
planning to E0 must be :REWOUND :FROM r3 -- the single-window model
answered :UNCHANGED here and lost the generation.  :EXACT is NIL because
the promoted generation took a further write after E2, which no rewind
to r3 can undo (spec R2).  :STATE-AT is that write's own epoch, which
also pins %REOPEN-AND-RESUME's location normalisation: unnormalised, the
restored store wrote transaction-id.dat into its PARENT directory and
the generation still reported the pre-restore watermark."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (handler-bind ((graph-db:restore-inexact-warning #'muffle-warning))
          (graph-db:restore-system clock e0))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is (= 1 (%rs-count g)))
        (let ((post (%rs-write g)))
          (multiple-value-bind (ng2 r3) (%rs-swap g clock)
            (setq g ng2)
            (let ((e (%rs-entry (graph-db:plan-system-restore clock e0)
                                :restore-store-1)))
              (is (eq :rewound (getf e :action)))
              (is (string= (string-right-trim "/" r3) (getf e :from)))
              (is (= post (getf e :state-at)))
              (is (null (getf e :exact))))))))))

(test plan-follows-a-chain-of-two-restores
  "Two consecutive restores: restore-to-E0 promotes r1 (retiring r2),
then restore-to-MID promotes r2 back (retiring r1's content as r3).  E0's
era is inherited through the chain, so planning to E0 selects r3, and
executing that plan yields the 1-vertex content again; MID is covered by
what is live now, so it plans :UNCHANGED."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((mid (%rs-write g)))
          (is (= 3 (%rs-count g)))
          (handler-bind ((graph-db:restore-inexact-warning #'muffle-warning))
            (graph-db:restore-system clock e0))
          (setq g (graph-db:lookup-graph :restore-store-1))
          (is (= 1 (%rs-count g)))
          (let ((m2 (handler-bind
                        ((graph-db:restore-inexact-warning #'muffle-warning))
                      (graph-db:restore-system clock mid))))
            (setq g (graph-db:lookup-graph :restore-store-1))
            (is (= 3 (%rs-count g)))
            (let ((r3 (getf (%rs-entry m2 :restore-store-1) :retired-live))
                  (e (%rs-entry (graph-db:plan-system-restore clock e0)
                                :restore-store-1)))
              (is-true r3)
              (is (eq :rewound (getf e :action)))
              (is (string= r3 (getf e :from)))
              (is (eq :unchanged
                      (getf (%rs-entry (graph-db:plan-system-restore clock mid)
                                       :restore-store-1)
                            :action)))
              (handler-bind
                  ((graph-db:restore-inexact-warning #'muffle-warning))
                (graph-db:restore-system clock e0))
              (setq g (graph-db:lookup-graph :restore-store-1))
              (is (= 1 (%rs-count g))))))))))

(test plan-refuses-a-replicated-graph-as-unsupported
  "I5's refusal, exercised: a MASTER-GRAPH registered under the store's
name is refused :UNSUPPORTED-GRAPH inside RESTORE-REFUSED-ERROR, not by
a bare DETACH-UNSUPPORTED-GRAPH-ERROR.  Refused for an epoch this store
would otherwise leave :UNCHANGED too -- the check runs on every open
clocked store, because the cascade can rebuild one the plan did not
name.  A bare MAKE-INSTANCE shell is enough (see DETACH-REFUSES-
REPLICATED-GRAPHS): the check is a TYPEP."
  (with-restore-system (g clock sys)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (declare (ignore r1))
        (setq g ng)
        (let ((shell (make-instance 'graph-db::master-graph
                                    :graph-name :restore-store-1
                                    :location (graph-db::location g)
                                    :system-clock clock)))
          (unwind-protect
               (progn
                 (setf (gethash :restore-store-1 graph-db::*graphs*) shell)
                 (dolist (epoch (list e0 (graph-db:clock-current-epoch clock)))
                   (let ((c (handler-case
                                (progn (graph-db:plan-system-restore
                                        clock epoch)
                                       nil)
                              (graph-db:restore-refused-error (c) c))))
                     (is-true c)
                     (when c
                       (is (equal '((:restore-store-1 . :unsupported-graph))
                                  (graph-db:restore-refused-reasons c)))))))
            (setf (gethash :restore-store-1 graph-db::*graphs*) g)))))))

(test rebuild-rolls-back-a-failed-retire-rename
  "%REBUILD-ONE-STORE's PRE-MAKE-GRAPH branch: the retire rename fails,
so nothing changed -- the live directory is renamed back and reopened
fully accepting, the REBUILD callback never runs, and the original error
is resignalled (I1)."
  (with-restore-system (g clock sys :policy :derivable)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname r1)
                                    :validate t)
        (let ((live (graph-db::%trimmed-namestring (graph-db::location g)))
              (ran nil))
          (with-posix-rename-failing-when
              (old new (string= (graph-db::%trimmed-namestring old) live))
            (signals error
              (graph-db:restore-system
               clock e0
               :rebuild (lambda (name graph)
                          (declare (ignore name graph))
                          (setq ran t)))))
          (is-false ran))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is-true (graph-db::graph-open-p g))
        (is (eq t (graph-db:accepting-p (graph-db::transaction-manager g))))
        (is (= 2 (%rs-count g)))
        (with-transaction ((graph-db::transaction-manager g))
          (graph-db:make-vertex :generic nil :graph g))))))

(test rebuild-journals-a-failed-callback
  "%REBUILD-ONE-STORE's POST-MAKE-GRAPH branch: the callback signals, so
the fresh (possibly half-populated) generation stays live by design and
a :RESTORE :MODE :REBUILT :FAILED T record names it and its retired
predecessor before the error propagates out of RESTORE-SYSTEM (I1)."
  (with-restore-system (g clock sys :policy :derivable)
    sys
    (let ((e0 (%rs-write g)))
      (multiple-value-bind (ng r1) (%rs-swap g clock)
        (setq g ng)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname r1)
                                    :validate t)
        (signals error
          (graph-db:restore-system
           clock e0
           :rebuild (lambda (name graph)
                      (declare (ignore name graph))
                      (error "injected rebuild-callback failure"))))
        (let ((r (find-if (lambda (r) (and (eq (getf r :kind) :restore)
                                           (getf r :failed)))
                          (journal-records clock))))
          (is-true r)
          (when r
            (is (eq :rebuilt (getf r :mode)))
            (is-true (getf r :retired-live))))
        (setq g (graph-db:lookup-graph :restore-store-1))
        (is-true (graph-db::graph-open-p g))
        (is (= 0 (%rs-count g)))))))

(test cascade-replaces-the-action-key-on-a-rewound-entry
  "%ENTRY-WITH must REPLACE, not shadow: store B plans :REWOUND :EXACT T,
then the cascade rebuilds it because it holds an edge into rebuilt store
A.  Its manifest entry must carry exactly one :ACTION (:REBUILT) and one
:EXACT (NIL) -- a shadowed :EXACT T behind the new one would tell a
reader the store is an exact instant when it is a rebuild."
  (with-restore-system (ga clock sys :policy :derivable)
    sys
    (with-second-store (gb clock :restore-store-b bdir :policy :derivable)
      (let (a-id)
        (with-transaction ((graph-db::transaction-manager ga))
          (setq a-id (graph-db:id (graph-db:make-vertex :generic nil
                                                        :graph ga))))
        (with-transaction ((graph-db::transaction-manager gb))
          (let ((v (graph-db:make-vertex :generic nil :graph gb)))
            (graph-db:make-edge :generic (graph-db:id v) a-id nil nil
                                :graph gb)))
        (let ((e0 (graph-db:clock-current-epoch clock)))
          (multiple-value-bind (ngb rb1) (%rs-swap gb clock
                                                   :name :restore-store-b)
            (declare (ignore rb1))
            (setq gb ngb)
            (let ((eb (%rs-entry (graph-db:plan-system-restore
                                  clock e0
                                  :rebuild (lambda (n g)
                                             (declare (ignore n g))))
                                 :restore-store-b)))
              (is (eq :rewound (getf eb :action)))
              (is (eq t (getf eb :exact))))
            (multiple-value-bind (nga ra1) (%rs-swap ga clock)
              (setq ga nga)
              (uiop:delete-directory-tree
               (uiop:ensure-directory-pathname ra1) :validate t)
              (let* ((m (handler-bind
                            ((graph-db:restore-inexact-warning
                               #'muffle-warning))
                          (graph-db:restore-system
                           clock e0
                           :rebuild (lambda (name graph)
                                      (declare (ignore name graph))))))
                     (eb (%rs-entry m :restore-store-b)))
                (is (= 1 (%rs-key-count eb :action)))
                (is (eq :rebuilt (getf eb :action)))
                (is (= 1 (%rs-key-count eb :exact)))
                (is (null (getf eb :exact)))
                (is (eq :restore-store-1 (getf eb :cascade-from)))))))))))

;;; Task 5 (GH #171): repair-interrupted-swap and the restore pre-check.

(test restore-refuses-while-a-swap-is-interrupted
  "Construct the between-renames layout by hand: live renamed away,
nothing at the live location, and no journal record at all -- the
shape a hard crash inside SWAP-IN-SHADOW's own rename pair leaves,
since it journals nothing until AFTER both renames land.  The plan
refuses with :INTERRUPTED-SWAP; the repair tool renames the stranded
directory back, returns :REPAIRED, journals :SWAP-ABORTED, and is
idempotent (:NOTHING-TO-DO on a second call)."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (let* ((live (graph-db::%trimmed-namestring (graph-db::location g)))
           (now (graph-db:clock-current-epoch clock))
           (stranded (format nil "~A-retired-~D" live (+ now 50))))
      (let ((graph-db:*graph* g)) (close-graph g :snapshot-p nil))
      (graph-db::%posix-rename live stranded)
      (let ((c (handler-case
                   (progn (graph-db:plan-system-restore clock now) nil)
                 (graph-db:restore-refused-error (c) c))))
        (is-true c)
        (when c
          (is (equal '((:restore-store-1 . :interrupted-swap))
                     (graph-db:restore-refused-reasons c)))))
      (is (eq :repaired (graph-db:repair-interrupted-swap
                         clock :restore-store-1 live)))
      (is-true (probe-file (uiop:ensure-directory-pathname live)))
      (is-false (probe-file (uiop:ensure-directory-pathname stranded)))
      (is-true (find :swap-aborted (journal-records clock)
                     :key (lambda (r) (getf r :kind))))
      (is (eq :nothing-to-do (graph-db:repair-interrupted-swap
                              clock :restore-store-1 live)))
      (setq g (open-graph :restore-store-1 (concatenate 'string live "/")
                          :system-clock clock))
      (is (= 1 (%rs-count g))))))

(test plan-and-retired-generations-are-sane-after-a-repair
  "After REPAIR-INTERRUPTED-SWAP, the store is an ordinary open store
again: RETIRED-GENERATIONS lists no generation for it (the repair
retired nothing -- :SWAP-ABORTED names no :RETIRED path), and planning
to NOW is :UNCHANGED, not a refusal and not a phantom rewind."
  (with-restore-system (g clock sys)
    sys
    (%rs-write g)
    (let* ((live (graph-db::%trimmed-namestring (graph-db::location g)))
           (now (graph-db:clock-current-epoch clock))
           (stranded (format nil "~A-retired-~D" live (+ now 50))))
      (let ((graph-db:*graph* g)) (close-graph g :snapshot-p nil))
      (graph-db::%posix-rename live stranded)
      (graph-db:repair-interrupted-swap clock :restore-store-1 live)
      (setq g (open-graph :restore-store-1 (concatenate 'string live "/")
                          :system-clock clock))
      (is (null (graph-db:retired-generations clock)))
      (let ((e (%rs-entry (graph-db:plan-system-restore
                           clock (graph-db:clock-current-epoch clock))
                          :restore-store-1)))
        (is (eq :unchanged (getf e :action)))))))

(test dangling-into-is-scoped-to-the-clock-and-guards-a-nil-tag
  "%DANGLING-INTO's two guards (C2, M3), unit-tested: a store attached to
a DIFFERENT clock holding an edge into store A is invisible to a scan
scoped to A's clock and visible to a scan scoped to its own; and a NIL
store tag matches nothing rather than every untagged legacy id."
  (with-restore-system (ga clock sys)
    sys
    (with-temp-directory (cdir2)
      (let ((clock2 (open-system-clock (namestring cdir2))))
        (unwind-protect
             (with-second-store (gb clock2 :restore-store-b bdir)
               (let (a-id)
                 (with-transaction ((graph-db::transaction-manager ga))
                   (setq a-id (graph-db:id (graph-db:make-vertex
                                            :generic nil :graph ga))))
                 (with-transaction ((graph-db::transaction-manager gb))
                   (let ((v (graph-db:make-vertex :generic nil :graph gb)))
                     (graph-db:make-edge :generic (graph-db:id v) a-id nil nil
                                         :graph gb)))
                 (let ((tag (graph-db:store-registry-id-for
                             :restore-store-1)))
                   (is-true tag)
                   (is (null (graph-db::%dangling-into clock tag nil)))
                   (is (equal '((:restore-store-b . 1))
                              (graph-db::%dangling-into clock2 tag nil)))
                   (is (null (graph-db::%dangling-into clock2 nil nil))))))
          (close-system-clock clock2))))))
