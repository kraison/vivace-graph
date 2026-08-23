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
                        (when (graph-db::graph-open-p ,g)
                          (let ((graph-db:*graph* ,g))
                            (ignore-errors
                             (close-graph ,g :snapshot-p nil))))
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
