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
