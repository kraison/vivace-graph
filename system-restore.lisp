(in-package :graph-db)

;;; Whole-system restore across a shadow swap (GH #171).  Generations
;;; live on the filesystem as <location>-retired-<E3>; the journal only
;;; annotates them (spec R4).  See
;;; docs/superpowers/specs/2026-08-23-restore-171-design.md

(defstruct (generation (:constructor %make-generation))
  store location retired swap-epoch journaled-p present-p policy)

(define-condition swap-record-missing-warning (warning)
  ;; The #212 shape: renames landed, JOURNAL-APPEND did not.  Tolerated,
  ;; like #191's torn tail -- the directory name carries the epoch.
  ((path :initarg :path :reader swap-record-missing-path))
  (:report (lambda (c s)
             (format s "Retired generation ~A has no :SWAP journal ~
record (GH #212); its epoch is taken from the directory name."
                     (swap-record-missing-path c)))))

(defun %retired-suffix-epoch (name live-name)
  "NAME's epoch when NAME is LIVE-NAME-retired-<digits>, else NIL."
  (let ((prefix (concatenate 'string live-name "-retired-")))
    (when (and (> (length name) (length prefix))
               (string= prefix name :end2 (length prefix))
               (every #'digit-char-p (subseq name (length prefix))))
      (parse-integer name :start (length prefix)))))

(defun %retired-dirs-for (location)
  "((E3 . \"<location>-retired-E3\") ...) for LOCATION, ascending by E3."
  (let* ((live (%trimmed-namestring location))
         (parent (uiop:pathname-parent-directory-pathname
                  (uiop:ensure-directory-pathname live)))
         (live-name (car (last (pathname-directory
                                (uiop:ensure-directory-pathname live)))))
         (found nil))
    (dolist (dir (uiop:subdirectories parent))
      (let* ((name (car (last (pathname-directory dir))))
             (epoch (%retired-suffix-epoch name live-name)))
        (when epoch
          (push (cons epoch (%trimmed-namestring dir)) found))))
    (sort found #'< :key #'car)))

(defun %live-location-of-retired (path)
  "\"<loc>-retired-123\" -> \"<loc>\"."
  (let* ((s (%trimmed-namestring path))
         (pos (search "-retired-" s :from-end t)))
    (if pos (subseq s 0 pos) s)))

(defun %swap-records (clock)
  (remove :swap (journal-records clock)
          :key (lambda (r) (getf r :kind)) :test-not #'eq))

(defun %pruned-retired-paths (clock)
  "Paths PRUNE-RETIRED-GENERATIONS has already deleted and journaled
:RETIRE for -- distinct from a :SWAP record whose directory vanished
some other way, which stays listed PRESENT-P NIL per spec R4 (GH #171)."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (when (eq (getf r :kind) :retire)
        (setf (gethash (%trimmed-namestring (getf r :retired)) set) t)))
    set))

(defun retired-generations (clock)
  "Every retired generation known to CLOCK's system, as GENERATION
structs sorted by store then SWAP-EPOCH.  Filesystem is authoritative:
a directory without a :SWAP record is listed JOURNALED-P NIL and
warned (SWAP-RECORD-MISSING-WARNING); a record without a directory is
listed PRESENT-P NIL (GH #171, spec R4).  A generation PRUNE-RETIRED-
GENERATIONS has already deleted (journaled :RETIRE) is omitted
entirely, not merely marked absent."
  (let ((by-retired (make-hash-table :test 'equal))
        (locations (make-hash-table :test 'equal))
        (pruned (%pruned-retired-paths clock)))
    ;; Journal first: records name the live locations to scan.
    (dolist (r (%swap-records clock))
      (let ((retired (%trimmed-namestring (getf r :retired))))
        (setf (gethash (%live-location-of-retired retired) locations)
              (getf r :store))
        (setf (gethash retired by-retired)
              (%make-generation
               :store (getf r :store)
               :location (%live-location-of-retired retired)
               :retired retired
               :swap-epoch (getf r :epoch)
               :journaled-p t
               :present-p (and (probe-file
                                (uiop:ensure-directory-pathname retired))
                               t)))))
    ;; Then every open clocked store, so unjournaled directories are found.
    ;; Compared by LOCATION, not EQ: a clock reopened after
    ;; CLOSE-SYSTEM-CLOCK (e.g. to repair a torn journal) is a fresh
    ;; struct at the same path, and an open graph's SYSTEM-CLOCK slot
    ;; still points at the stale instance (GH #171).
    (maphash (lambda (name graph)
               (when (and (typep graph 'graph)
                          (graph-system-clock graph)
                          (string= (%trimmed-namestring
                                    (system-clock-location
                                     (graph-system-clock graph)))
                                   (%trimmed-namestring
                                    (system-clock-location clock))))
                 (setf (gethash (%trimmed-namestring (location graph))
                                locations)
                       name)))
             *graphs*)
    (maphash
     (lambda (location store)
       (loop for (epoch . dir) in (%retired-dirs-for location)
             unless (gethash dir by-retired)
               do (warn 'swap-record-missing-warning :path dir)
                  (setf (gethash dir by-retired)
                        (%make-generation :store store :location location
                                          :retired dir :swap-epoch epoch
                                          :journaled-p nil :present-p t))))
     locations)
    (let ((gens nil))
      (maphash (lambda (k gen)
                 (unless (gethash k pruned)
                   (setf (generation-policy gen)
                         (if (generation-present-p gen)
                             (store-recovery-policy (generation-retired gen))
                             (store-recovery-policy (generation-location gen))))
                   (push gen gens)))
               by-retired)
      (sort gens (lambda (a b)
                   (let ((sa (princ-to-string (generation-store a)))
                         (sb (princ-to-string (generation-store b))))
                     (or (string< sa sb)
                         (and (string= sa sb)
                              (< (generation-swap-epoch a)
                                 (generation-swap-epoch b))))))))))

(define-condition retention-required-error (error)
  ;; Authored data is never silently discarded inside the restore
  ;; window (spec R3, §9.2).
  ((generations :initarg :generations
                :reader retention-required-generations))
  (:report (lambda (c s)
             (format s "Refusing to prune ~D :AUTHORED generation~:P still ~
inside the restore window:~{ ~A~} (GH #171)."
                     (length (retention-required-generations c))
                     (mapcar #'generation-retired
                             (retention-required-generations c))))))

(defun %retired-suffix-p (path)
  "The deletion gate for retired generations, mirroring %SHADOW-SUFFIX-P."
  (and (search "-retired-" (%trimmed-namestring path)) t))

(defun %delete-generation (clock gen)
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname (generation-retired gen))
   :validate #'%retired-suffix-p :if-does-not-exist :ignore)
  (journal-append clock :retire
                  :store (generation-store gen)
                  :retired (generation-retired gen)
                  :swap-epoch (generation-swap-epoch gen)))

(defun prune-retired-generations (clock floor &key discard-derivable dry-run)
  "Delete retired generations the restore window no longer covers: those
with SWAP-EPOCH <= FLOOR.  Above FLOOR an :AUTHORED generation is
refused by name (RETENTION-REQUIRED-ERROR, before anything is deleted);
a :DERIVABLE one is deleted only with DISCARD-DERIVABLE.  DRY-RUN
returns what would go and touches nothing.  Each deletion journals
:RETIRE (GH #171, spec R3)."
  (let* ((gens (remove-if-not #'generation-present-p
                              (retired-generations clock)))
         (blocked (remove-if-not
                   (lambda (g) (and (> (generation-swap-epoch g) floor)
                                    (eq (generation-policy g) :authored)))
                   gens))
         (victims (remove-if-not
                   (lambda (g)
                     (or (<= (generation-swap-epoch g) floor)
                         (and discard-derivable
                              (eq (generation-policy g) :derivable))))
                   gens)))
    ;; A call that deletes nothing but was blocked by authored
    ;; generations is the refusal case; skipping authored ones above
    ;; FLOOR while pruning below it returns normally (GH #171).
    (when (and blocked (null victims))
      (error 'retention-required-error :generations blocked))
    (unless dry-run
      (dolist (g victims) (%delete-generation clock g)))
    victims))

(define-condition restore-refused-error (error)
  ;; Every refusal fires here, before any rename (spec §3).
  ((reasons :initarg :reasons :reader restore-refused-reasons)
   (epoch :initarg :epoch :reader restore-refused-epoch))
  (:report (lambda (c s)
             (format s "Restore to epoch ~D refused:~{ ~A~} (GH #171)."
                     (restore-refused-epoch c)
                     (mapcar (lambda (r) (format nil "~A=~A" (car r) (cdr r)))
                             (restore-refused-reasons c))))))

(defun %generation-state-epoch (retired)
  "The last epoch committed into RETIRED, from its transaction-id.dat;
0 when absent.  The closed generation's E0 (spec R2)."
  (let ((file (merge-pathnames "transaction-id.dat"
                               (uiop:ensure-directory-pathname retired)))
        (buf (make-byte-vector 8)))
    (if (probe-file file)
        (with-open-file (in file :element-type '(unsigned-byte 8))
          (unless (= 8 (read-sequence buf in))
            (error "Short read on ~A" file))
          (deserialize-uint64 buf 0))
        0)))

(defun %generation-live-at (gens epoch)
  "Among GENS (one store, ascending SWAP-EPOCH), the generation that was
live at EPOCH: the earliest with SWAP-EPOCH > EPOCH, or NIL when the
current generation already was."
  (find-if (lambda (g) (> (generation-swap-epoch g) epoch)) gens))

(defun %restore-plan-entries (clock epoch rebuild)
  "One manifest entry per store CLOCK knows about, plus the refusal list
for PLAN-SYSTEM-RESTORE to raise.  Returns (values ENTRIES REASONS)."
  (let ((by-store (make-hash-table :test 'equal))
        (entries nil) (reasons nil))
    (dolist (g (retired-generations clock))
      (push g (gethash (generation-store g) by-store)))
    ;; Open clocked stores with no generations at all are :UNCHANGED.
    (maphash (lambda (name graph)
               (when (and (typep graph 'graph)
                          (graph-system-clock graph)
                          (string= (%trimmed-namestring
                                    (system-clock-location
                                     (graph-system-clock graph)))
                                   (%trimmed-namestring
                                    (system-clock-location clock))))
                 (unless (nth-value 1 (gethash name by-store))
                   (setf (gethash name by-store) nil))))
             *graphs*)
    (maphash
     (lambda (store gens)
       (let* ((gens (sort (copy-list gens) #'< :key #'generation-swap-epoch))
              (target (%generation-live-at gens epoch)))
         (cond
           ((null target)
            (push (list :store store :action :unchanged) entries))
           ((generation-present-p target)
            (let ((e0 (%generation-state-epoch (generation-retired target))))
              (push (list :store store :action :rewound
                          :state-at e0 :exact (<= e0 epoch)
                          :from (generation-retired target)
                          :swap-epoch (generation-swap-epoch target))
                    entries)))
           ((eq (generation-policy target) :authored)
            (push (cons store :authored-generation-missing) reasons)
            (push (list :store store :action :refused
                        :reason :authored-generation-missing) entries))
           ((null rebuild)
            (push (cons store :no-rebuild) reasons)
            (push (list :store store :action :refused :reason :no-rebuild)
                  entries))
           (t
            (push (list :store store :action :rebuilt :exact nil
                        :state-at (clock-current-epoch clock))
                  entries)))))
     by-store)
    (values (nreverse entries) (nreverse reasons))))

(defun plan-system-restore (clock epoch &key require-exact rebuild)
  "The manifest RESTORE-SYSTEM would act on for EPOCH, with no side
effects.  Signals RESTORE-REFUSED-ERROR listing every (STORE . REASON):
:AUTHORED-GENERATION-MISSING, :NO-REBUILD, :INEXACT (only under
REQUIRE-EXACT), :INTERRUPTED-SWAP (Task 5).  REBUILD is the caller's
(lambda (name graph)) loader for :DERIVABLE stores (GH #171, spec R1/R2)."
  (multiple-value-bind (entries reasons)
      (%restore-plan-entries clock epoch rebuild)
    (when require-exact
      (dolist (e entries)
        (when (and (eq (getf e :action) :rewound) (not (getf e :exact)))
          (push (cons (getf e :store) :inexact) reasons))))
    (when reasons
      (error 'restore-refused-error :reasons (nreverse reasons) :epoch epoch))
    ;; :RESTORE T marks the manifest kind; without a paired value the
    ;; list is an odd-length plist and GETF signals malformed (GH #171).
    (list :restore t :requested epoch :at (clock-current-epoch clock)
          :clock (namestring (system-clock-location clock))
          :stores entries)))

;;; Execution (Task 4): rename-based generation swap per store, a
;;; caller-supplied REBUILD callback for derivable stores, cascade via
;;; store tags, and a readable manifest file (spec S3/S4).

(define-condition restore-inexact-warning (warning)
  ;; Spec S4: a mixed manifest is the inconsistent instant S9 exists to
  ;; record; this makes it impossible to miss.
  ((manifest :initarg :manifest :reader restore-inexact-manifest))
  (:report (lambda (c s)
             (format s "Restore to ~D is not an exact instant:~{ ~A~} ~
(GH #171)."
                     (getf (restore-inexact-manifest c) :requested)
                     (loop for e in (getf (restore-inexact-manifest c)
                                          :stores)
                           when (or (eq (getf e :action) :rebuilt)
                                    (and (eq (getf e :action) :rewound)
                                         (not (getf e :exact))))
                             collect (format nil "~A=~A" (getf e :store)
                                             (getf e :action)))))))

(defun %manifest-file (clock epoch)
  (merge-pathnames (format nil "restore-~D.manifest" epoch)
                   (uiop:ensure-directory-pathname
                    (system-clock-location clock))))

(defun %write-manifest (clock manifest)
  (with-open-file (out (%manifest-file clock (getf manifest :at))
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-readably* nil) (*print-pretty* nil))
      (prin1 manifest out)))
  manifest)

(defun read-restore-manifest (path)
  "PATH's manifest plist.  *READ-EVAL* NIL: data, never code (GH #171)."
  (with-open-file (in path)
    (let ((*read-eval* nil)) (read in))))

(defun %close-quiesced (graph timeout)
  "SWAP-IN-SHADOW's close sequence: quiesce as :RESTORING, close with
snapshot, no writer can land in the doomed generation."
  (%quiesce-transaction-manager (transaction-manager graph) :restoring
                                timeout)
  (let ((*graph* graph) (*quiesced-store-closing-p* t))
    (close-graph graph :snapshot-p t)))

(defun %restore-one-store (clock name entry timeout)
  "Rename live -> <loc>-retired-<Enow> (:RETIRE-LIVE), retained -> live
(:RESTORE), reopen + attach.  Commit point is the second rename, as in
%SWAP-IN-SHADOW-1: failure before it renames back and resignals; after
it, reopens the restored generation and warns (GH #171)."
  (let* ((graph (lookup-graph name))
         (live (%trimmed-namestring (location graph)))
         (from (getf entry :from))
         (retired (format nil "~A-retired-~D" live
                          (clock-current-epoch clock)))
         (done nil))
    (%close-quiesced graph timeout)
    (handler-case
        (progn
          (%posix-rename live retired)
          (journal-append clock :retire-live :store name :retired retired)
          (%posix-rename from live)
          (setq done t)
          (journal-append clock :restore :store name :from from
                          :retired-live retired
                          :requested-epoch (getf entry :requested)
                          :state-at (getf entry :state-at)
                          :exact (getf entry :exact))
          (%reopen-and-resume name live clock t))
      (error (original)
        (unless done
          (ignore-errors (%posix-rename retired live)))
        (handler-case (%reopen-and-resume name live clock t)
          (error (recovery)
            (error 'shadow-recovery-failed :original original
                                           :recovery recovery)))
        (if done
            (warn 'swap-recovered-warning :original original)
            (error original))))
    (list :retired-live retired)))

(defun %rebuild-one-store (clock name rebuild timeout)
  "Retire the live generation (:RETIRE-LIVE), MAKE-GRAPH a fresh one at
the same location with the same policy, run REBUILD on it, journal
:RESTORE :mode :rebuilt."
  (let* ((graph (lookup-graph name))
         (live (%trimmed-namestring (location graph)))
         (policy (store-recovery-policy live))
         (retired (format nil "~A-retired-~D" live
                          (clock-current-epoch clock))))
    (%close-quiesced graph timeout)
    (%posix-rename live retired)
    (journal-append clock :retire-live :store name :retired retired)
    (let ((fresh (make-graph name (concatenate 'string live "/")
                             :system-clock clock :recovery-policy policy)))
      (funcall rebuild name fresh)
      (journal-append clock :restore :store name :mode :rebuilt
                      :retired-live retired)
      (list :retired-live retired))))

(defun %dangling-into (store-id exclude)
  "((STORE-NAME . COUNT) ...) of edges in open clocked stores other than
EXCLUDE whose FROM or TO carries STORE-ID's tag (spec R5)."
  (let ((result nil))
    (maphash
     (lambda (name graph)
       (when (and (typep graph 'graph) (graph-system-clock graph)
                  (not (member name exclude)))
         (let ((n 0))
           (map-edges (lambda (e)
                        (when (or (eql (id-store-tag (from e)) store-id)
                                  (eql (id-store-tag (to e)) store-id))
                          (incf n)))
                      graph)
           (when (> n 0) (push (cons name n) result)))))
     *graphs*)
    result))

(defun restore-system (clock epoch &key require-exact rebuild (timeout 60))
  "Execute PLAN-SYSTEM-RESTORE's manifest for EPOCH: every refusal fires
before any rename.  Rewinds first, then rebuilds, then cascades: a
:DERIVABLE store holding edges into a rebuilt store is rebuilt in turn
(fixpoint); an :AUTHORED one is left alone and reported :DANGLING N.
Writes <clock-dir>/restore-<Enow>.manifest and signals RESTORE-INEXACT-
WARNING when any store is rebuilt or inexact.  Returns the manifest
(GH #171, spec S3-S4)."
  (let* ((manifest (plan-system-restore clock epoch
                                        :require-exact require-exact
                                        :rebuild rebuild))
         (entries (getf manifest :stores))
         (rebuilt nil))
    ;; NOTE: (SETF (GETF entry :NEW-KEY) v) on a key ENTRIES's element
    ;; does not already carry rebinds the local loop/FIND variable, not
    ;; the cons ENTRIES holds -- it never mutates the manifest.  Every
    ;; branch below therefore replaces the element positionally via
    ;; (SETF (NTH pos entries) ...) instead (GH #171).
    (dolist (e entries)
      (when (eq (getf e :action) :rewound)
        (let* ((pos (position (getf e :store) entries
                              :key (lambda (x) (getf x :store))))
               (extra (%restore-one-store
                      clock (getf e :store)
                      (list* :requested epoch e) timeout)))
          (setf (nth pos entries)
                (list* :retired-live (getf extra :retired-live) e)))))
    (dolist (e entries)
      (when (eq (getf e :action) :rebuilt)
        (%rebuild-one-store clock (getf e :store) rebuild timeout)
        (push (getf e :store) rebuilt)))
    ;; Cascade to a fixpoint over derivable dependents.
    (let ((queue (copy-list rebuilt)))
      (loop while queue do
        (let* ((source (pop queue))
               (tag (store-registry-id-for source)))
          (loop for (name . n) in (%dangling-into tag rebuilt) do
            (let* ((pos (position name entries
                                  :key (lambda (x) (getf x :store))))
                   (entry (nth pos entries)))
              (if (eq (store-recovery-policy
                       (location (lookup-graph name)))
                      :derivable)
                  (progn
                    (%rebuild-one-store clock name rebuild timeout)
                    (push name rebuilt) (push name queue)
                    (setf (nth pos entries)
                          (list* :action :rebuilt :exact nil
                                 :state-at (clock-current-epoch clock)
                                 :cascade-from source entry)))
                  (setf (nth pos entries)
                        (list* :dangling n entry))))))))
    (setf (getf manifest :at) (clock-current-epoch clock))
    (%write-manifest clock manifest)
    (when (some (lambda (e) (or (eq (getf e :action) :rebuilt)
                                (and (eq (getf e :action) :rewound)
                                     (not (getf e :exact)))))
                entries)
      (warn 'restore-inexact-warning :manifest manifest))
    manifest))
