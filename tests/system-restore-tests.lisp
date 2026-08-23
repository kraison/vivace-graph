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

(defun %rs-swap (g clock &key (shadow-writes 1))
  "Shadow G, write SHADOW-WRITES vertices into the shadow, swap it in.
Returns (values NEW-GRAPH RETIRED-PATH)."
  (multiple-value-bind (shadow-location g2) (graph-db:shadow-store g)
    (multiple-value-bind (start end) (graph-db:clock-lease-epochs clock 1000)
      (let ((sg (graph-db:open-shadow-graph
                 shadow-location :restore-store-1 :lease (cons start end))))
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
