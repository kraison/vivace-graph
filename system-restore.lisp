(in-package :graph-db)

;;; Whole-system restore across a shadow swap (GH #171).  Generations
;;; live on the filesystem as <location>-retired-<E3>; the journal only
;;; annotates them (spec R4).  See
;;; docs/superpowers/specs/2026-08-23-restore-171-design.md

(defstruct (generation (:constructor %make-generation))
  store location retired swap-epoch live-from eras journaled-p present-p
  policy)

(define-condition swap-record-missing-warning (warning)
  ;; The #212 shape: renames landed, JOURNAL-APPEND did not.  Tolerated,
  ;; like #191's torn tail -- the directory name carries the epoch.
  ((path :initarg :path :reader swap-record-missing-path))
  (:report (lambda (c s)
             (format s "Retired generation ~A has no :SWAP or :RETIRE-LIVE ~
journal record (GH #212); its epoch is taken from the directory name."
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

(defun %retired-directory-records (clock)
  "Journal records that name a still-standing <live>-retired-<E> dir:
:SWAP (a completed shadow promotion) and :RETIRE-LIVE (RESTORE-SYSTEM
retiring the live generation before a rewind/rebuild) -- both carry
:STORE/:RETIRED/:EPOCH.  A :RETIRE-LIVE whose second rename then failed
was rolled back and is excluded via its :RETIRE-LIVE-ABORTED record
(GH #171)."
  (let ((aborted (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (when (eq (getf r :kind) :retire-live-aborted)
        (setf (gethash (%trimmed-namestring (getf r :retired)) aborted) t)))
    (remove-if (lambda (r)
                 (or (not (member (getf r :kind) '(:swap :retire-live)))
                     (gethash (%trimmed-namestring (getf r :retired))
                              aborted)))
               (journal-records clock))))

(defun %pruned-retired-paths (clock)
  "Paths omitted from RETIRED-GENERATIONS entirely, not merely marked
absent: ones PRUNE-RETIRED-GENERATIONS deleted (:RETIRE), and ones a
RESTORE-SYSTEM call has since CONSUMED by promoting them back to live
(:RESTORE :FROM) -- both leave the pool of restorable generations the
same way (GH #171)."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (case (getf r :kind)
        (:retire
         (setf (gethash (%trimmed-namestring (getf r :retired)) set) t))
        (:restore
         (when (getf r :from)
           (setf (gethash (%trimmed-namestring (getf r :from)) set) t)))))
    set))

(defun %restore-promotions (clock)
  "EQUAL hash STORE -> ((EPOCH . FROM-PATH) ...) ascending, one entry per
:RESTORE record that promoted a retired directory back to live.  A
:MODE :REBUILT restore carries no :FROM and makes no entry: its content
is fresh, so it inherits nothing (GH #171)."
  (let ((by-store (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (when (and (eq (getf r :kind) :restore) (getf r :from))
        (push (cons (getf r :epoch)
                    (%trimmed-namestring (getf r :from)))
              (gethash (getf r :store) by-store))))
    (maphash (lambda (store prs)
               (setf (gethash store by-store) (sort prs #'< :key #'car)))
             by-store)
    by-store))

(defun %assign-generation-eras (gens promotions)
  "Fill ERAS and LIVE-FROM for GENS -- one store, ascending SWAP-EPOCH,
UNFILTERED.  A generation's ERAS is ((FROM . TO) ...): its own live
window, plus, when a :RESTORE promoted a directory into the window that
generation then closed, that directory's eras as well -- recursively
through chains of restores.  Without inheritance a directory promoted by
one restore and retired again by a later swap would have its original
content era attributed to nobody (GH #171, fix round 3)."
  (let ((by-path (make-hash-table :test 'equal))
        (prev 0))
    (dolist (g gens)
      (let* ((to (generation-swap-epoch g))
             (promo (find-if (lambda (p) (and (<= prev (car p))
                                              (< (car p) to)))
                             promotions))
             (eras (cons (cons (if promo (car promo) prev) to)
                         (and promo (gethash (cdr promo) by-path)))))
        (setf (generation-eras g) eras
              (generation-live-from g) (car (first (last eras)))
              (gethash (generation-retired g) by-path) eras
              prev to)))))

(defun retired-generations (clock)
  "Every retired generation known to CLOCK's system, as GENERATION
structs sorted by store then SWAP-EPOCH.  Filesystem is authoritative:
a directory without a joining :SWAP/:RETIRE-LIVE record is listed
JOURNALED-P NIL and warned (SWAP-RECORD-MISSING-WARNING); a record
without a directory is listed PRESENT-P NIL (GH #171, spec R4).  A
generation PRUNE-RETIRED-GENERATIONS has deleted (:RETIRE) or a
RESTORE-SYSTEM call has since consumed by promoting it back to live
(:RESTORE :FROM) is omitted entirely, not merely marked absent -- but
such a generation still contributes its ERAS to whichever generation
inherited its content (see %ASSIGN-GENERATION-ERAS)."
  (let ((by-retired (make-hash-table :test 'equal))
        (locations (make-hash-table :test 'equal))
        (pruned (%pruned-retired-paths clock)))
    ;; Journal first: records name the live locations to scan.
    (dolist (r (%retired-directory-records clock))
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
    (let ((all nil))
      (maphash (lambda (k gen)
                 (declare (ignore k))
                 (setf (generation-policy gen)
                       (if (generation-present-p gen)
                           (store-recovery-policy (generation-retired gen))
                           (store-recovery-policy (generation-location gen))))
                 (push gen all))
               by-retired)
      (setf all
            (sort all
                  (lambda (a b)
                    (let ((sa (princ-to-string (generation-store a)))
                          (sb (princ-to-string (generation-store b))))
                      (or (string< sa sb)
                          (and (string= sa sb)
                               (< (generation-swap-epoch a)
                                  (generation-swap-epoch b))))))))
      ;; ERAS spans the FULL per-store event order, including a
      ;; generation a RESTORE has since consumed or PRUNE-RETIRED-
      ;; GENERATIONS has deleted -- dropping such a generation from the
      ;; returned list below must not erase what it taught its
      ;; successor about which eras that successor now covers (GH #171,
      ;; fix rounds 2 and 3; see %GENERATION-LIVE-AT).
      (let ((promotions (%restore-promotions clock))
            (per-store (make-hash-table :test 'equal)))
        (dolist (g all) (push g (gethash (generation-store g) per-store)))
        (maphash (lambda (store gens)
                   (%assign-generation-eras
                    (nreverse gens) (gethash store promotions)))
                 per-store))
      (remove-if (lambda (g) (gethash (generation-retired g) pruned))
                 all))))

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
  "The deletion gate for retired generations, mirroring %SHADOW-SUFFIX-P
(shadow-store.lisp:166-172): the LAST path component must itself end in
-retired-<digits>, not merely contain the substring anywhere -- an
unanchored SEARCH would also pass a live store literally named
.../foo-retired-bar-baz/ (GH #171, deferred Task 2 minor)."
  (let* ((name (car (last (pathname-directory
                           (uiop:ensure-directory-pathname
                            (%trimmed-namestring path))))))
         (pos (and name (search "-retired-" name :from-end t))))
    (and pos
         (> (length name) (+ pos (length "-retired-")))
         (every #'digit-char-p (subseq name (+ pos (length "-retired-")))))))

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
  "Among GENS (one store), the generation with a half-open era
[FROM, TO) containing EPOCH, or NIL when EPOCH is covered by the
CURRENT generation instead (:UNCHANGED).  Ties -- possible only while
an ancestor a restore promoted is still listed -- go to the matching
era with the LATEST FROM, i.e. the most recent directory holding that
content.  Era containment, not merely the earliest generation whose
SWAP-EPOCH exceeds EPOCH: those agree on a monotone swap chain, but a
RESTORE promotes an OLDER generation back to live and a later swap
retires it again, so a directory's content era is not always its own
last live window (GH #171, fix rounds 2 and 3)."
  (let ((best nil) (best-from nil))
    (dolist (g gens best)
      (dolist (era (generation-eras g))
        (when (and (<= (car era) epoch) (< epoch (cdr era))
                   (or (null best-from) (> (car era) best-from)))
          (setq best g best-from (car era)))))))

(defun %open-store-for-clock (name clock)
  "The graph registered as NAME if it is attached to CLOCK -- matched by
SYSTEM-CLOCK-LOCATION string, the comparison every scan in this file
uses -- else NIL (GH #171)."
  (let ((graph (lookup-graph name)))
    (when (and graph (typep graph 'graph) (graph-system-clock graph)
               (string= (%trimmed-namestring
                         (system-clock-location (graph-system-clock graph)))
                        (%trimmed-namestring (system-clock-location clock))))
      graph)))

(defun %supported-for-restore-p (graph)
  "NIL when GRAPH is a MASTER-GRAPH/SLAVE-GRAPH/PEER-GRAPH -- v1 scope,
same refusal %CHECK-DETACH-SUPPORTED enforces for DETACH-STORE/SHADOW-
STORE/SWAP-IN-SHADOW (GH #171)."
  (handler-case
      (progn (%check-detach-supported graph 'plan-system-restore) t)
    (detach-unsupported-graph-error () nil)))

(defun %known-locations (clock)
  "EQUAL hash LOCATION (trimmed namestring) -> STORE for every store
CLOCK's journal or *GRAPHS* names, used by %INTERRUPTED-SWAP-P's
pre-check so it can run even against a store that is currently closed.
Three sources, since none alone survives every crash shape: an
:ATTACH record's own :LOCATION (added GH #171 -- the only trace left
when a swap-in-shadow crash lands between its two renames, before any
:SWAP record exists); a :SWAP/:RETIRE-LIVE record's :RETIRED path,
reversed to its live location (older journals predating :ATTACH's
:LOCATION field); and any graph *GRAPHS* still holds open under this
clock right now."
  (let ((acc (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (when (eq (getf r :kind) :attach)
        (let ((loc (getf r :location)))
          (when loc
            (setf (gethash (%trimmed-namestring loc) acc) (getf r :store))))))
    (dolist (r (%retired-directory-records clock))
      (setf (gethash (%live-location-of-retired
                      (%trimmed-namestring (getf r :retired)))
                     acc)
            (getf r :store)))
    (maphash (lambda (name graph)
               (when (and (typep graph 'graph)
                          (graph-system-clock graph)
                          (string= (%trimmed-namestring
                                    (system-clock-location
                                     (graph-system-clock graph)))
                                   (%trimmed-namestring
                                    (system-clock-location clock))))
                 (setf (gethash (%trimmed-namestring (location graph)) acc)
                       name)))
             *graphs*)
    acc))

(defun %retire-live-completed-p (clock retired)
  "True when RETIRED (a :RETIRE-LIVE record's :RETIRED path) has a
paired completion: a later, non-:FAILED :RESTORE naming it as
:RETIRED-LIVE, or a :RETIRE-LIVE-ABORTED naming it as :RETIRED after a
rollback.  A :RESTORE :FAILED T is excluded even though it names
:RETIRED-LIVE: %REBUILD-ONE-STORE journals it when MAKE-GRAPH itself
never got far enough to create anything at the live location, so
nothing is actually there -- the retired directory is exactly as
stranded as if no :RESTORE record existed at all.  A :RETIRE-LIVE with
neither a completing :RESTORE nor a rollback is RESTORE-SYSTEM's own
rename-pair crash window, the same shape SWAP-IN-SHADOW's crash leaves
but one step later (GH #171, spec R6, review fix round 1)."
  (some (lambda (r)
          (or (and (eq (getf r :kind) :restore)
                   (not (getf r :failed))
                   (getf r :retired-live)
                   (string= (%trimmed-namestring (getf r :retired-live))
                            retired))
              (and (eq (getf r :kind) :retire-live-aborted)
                   (string= (%trimmed-namestring (getf r :retired))
                            retired))))
        (journal-records clock)))

(defun %interrupted-swap-p (clock location)
  "The retired path stranded between the two renames of a completed-
looking swap or restore attempt for LOCATION, or NIL when nothing is
stranded.  Fires only when LOCATION has no live directory -- pruning
never removes a live directory, and DETACH-STORE leaves one in place,
so a missing live directory for a store the journal or *GRAPHS* still
knows about is otherwise unexplained.

A retired directory for LOCATION counts as accounted for (not
stranded) when either: it is named by a :SWAP record (JOURNAL-APPEND
there runs only AFTER both renames land, so a :SWAP record IS
completion -- see %SWAP-IN-SHADOW-1); or it is named by a :RETIRE-LIVE
record that %RETIRE-LIVE-COMPLETED-P confirms was paired with a
:RESTORE or rolled back via :RETIRE-LIVE-ABORTED.  Anything else --
including a directory with NO journal record at all, the shape a hard
crash between SWAP-IN-SHADOW's renames leaves, since it journals
nothing until after both complete -- is stranded (GH #171, spec R6)."
  (let ((live (%trimmed-namestring location)))
    (unless (probe-file (uiop:ensure-directory-pathname live))
      (let ((accounted
              (loop for r in (journal-records clock)
                    for retired = (and (getf r :retired)
                                       (%trimmed-namestring (getf r :retired)))
                    when (and retired
                              (member (getf r :kind) '(:swap :retire-live))
                              (string= (%live-location-of-retired retired)
                                       live)
                              (or (eq (getf r :kind) :swap)
                                  (%retire-live-completed-p clock retired)))
                      collect retired)))
        (loop for (nil . dir) in (reverse (%retired-dirs-for live))
              unless (member dir accounted :test #'string=)
                return dir)))))

(defun repair-interrupted-swap (clock name location)
  "Rename a stranded pre-swap/pre-restore generation back to LOCATION
and journal :SWAP-ABORTED :STORE NAME :RESTORED-FROM the stranded path.
Returns :REPAIRED, or :NOTHING-TO-DO when %INTERRUPTED-SWAP-P finds
nothing to fix -- idempotent, and never touches a live directory
(GH #171, spec R6).  :SWAP-ABORTED names no :RETIRED path of its own,
so it retires nothing and is invisible to RETIRED-GENERATIONS and
%PRUNED-RETIRED-PATHS -- the directory it moved is live again, not a
generation to track."
  (let ((stranded (%interrupted-swap-p clock location)))
    (cond ((null stranded) :nothing-to-do)
          (t (%posix-rename stranded (%trimmed-namestring location))
             (journal-append clock :swap-aborted :store name
                             :restored-from stranded)
             :repaired))))

(defun %restore-plan-entries (clock epoch rebuild)
  "One manifest entry per store CLOCK knows about, plus the refusal list
for PLAN-SYSTEM-RESTORE to raise.  Returns (values ENTRIES REASONS);
REASONS is in reverse encounter order -- PUSH-accumulated, NOT
NREVERSEd here -- so a caller adding more reasons (e.g. :INEXACT) can
PUSH onto it and NREVERSE exactly once for a single deterministic order
(GH #171).

A store that needs a rename (:REWOUND or :REBUILT) but is not open
under CLOCK right now is refused :NOT-OPEN -- RESTORE-SYSTEM can only
quiesce and rename a live, attached graph, and this must be caught
before ANY store is renamed, not partway through a multi-store restore
(GH #171).  Likewise ANY open clocked MASTER-GRAPH/SLAVE-GRAPH/PEER-
GRAPH is refused :UNSUPPORTED-GRAPH -- whatever this EPOCH would do to
it -- rather than propagating DETACH-UNSUPPORTED-GRAPH-ERROR bare, so it
lands in the same RESTORE-REFUSED-ERROR as every other refusal."
  (let ((by-store (make-hash-table :test 'equal))
        (unsupported (make-hash-table :test 'equal))
        (entries nil) (reasons nil))
    (dolist (g (retired-generations clock))
      (push g (gethash (generation-store g) by-store)))
    ;; An interrupted swap wins over every other refusal for its store
    ;; and is checked first: the store's live directory is missing, so
    ;; without this it would otherwise fall through to :NOT-OPEN below
    ;; -- true, but not the actionable diagnosis (GH #171, spec R6).
    ;; %KNOWN-LOCATIONS finds the store even when it is closed, so this
    ;; runs whether or not it ever reached BY-STORE above.
    (maphash (lambda (loc store)
               (when (%interrupted-swap-p clock loc)
                 (push (cons store :interrupted-swap) reasons)
                 (push (list :store store :action :refused
                             :reason :interrupted-swap) entries)
                 (remhash store by-store)))
             (%known-locations clock))
    ;; Open clocked stores with no generations at all are :UNCHANGED.
    ;; The supported check runs on EVERY open clocked store, not only on
    ;; the ones this EPOCH plans to touch: the cascade can rebuild a
    ;; store the plan called :UNCHANGED, and that rebuild reopens it the
    ;; same way a rewind would (GH #171).
    (maphash (lambda (name graph)
               (declare (ignore graph))
               (let ((open-graph (%open-store-for-clock name clock)))
                 (when open-graph
                   (unless (nth-value 1 (gethash name by-store))
                     (setf (gethash name by-store) nil))
                   (unless (%supported-for-restore-p open-graph)
                     (setf (gethash name unsupported) t)))))
             *graphs*)
    (maphash
     (lambda (store gens)
       (let* ((gens (sort (copy-list gens) #'< :key #'generation-swap-epoch))
              (target (%generation-live-at gens epoch))
              (open-graph (and target (%open-store-for-clock store clock))))
         (cond
           ((gethash store unsupported)
            (push (cons store :unsupported-graph) reasons)
            (push (list :store store :action :refused
                        :reason :unsupported-graph)
                  entries))
           ((null target)
            (push (list :store store :action :unchanged) entries))
           ((null open-graph)
            (push (cons store :not-open) reasons)
            (push (list :store store :action :refused :reason :not-open)
                  entries))
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
    (values (nreverse entries) reasons)))

(defun plan-system-restore (clock epoch &key require-exact rebuild)
  "The manifest RESTORE-SYSTEM would act on for EPOCH, with no side
effects.  Signals RESTORE-REFUSED-ERROR listing every (STORE . REASON):
:AUTHORED-GENERATION-MISSING, :NO-REBUILD, :NOT-OPEN (a store needing a
rename is not open under CLOCK), :UNSUPPORTED-GRAPH (a MASTER-GRAPH/
SLAVE-GRAPH/PEER-GRAPH target), :INEXACT (only under REQUIRE-EXACT),
:INTERRUPTED-SWAP (a stranded rename pair; repair with
REPAIR-INTERRUPTED-SWAP before retrying).  REBUILD is the caller's
(lambda (name graph)) loader for :DERIVABLE stores (GH #171, spec
R1/R2)."
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
    ;; *PACKAGE* COMMON-LISP, not KEYWORD: keywords keep their printed
    ;; colon only when *package* is NOT the keyword package itself, and
    ;; a bare T/NIL in an entry stays unqualified since CL is its home
    ;; package; *PRINT-READABLY* stays NIL, as the journal's own
    ;; records already do (GH #171).
    (let ((*print-readably* nil) (*print-pretty* nil)
          (*package* (find-package :common-lisp)))
      (prin1 manifest out)))
  manifest)

(defun read-restore-manifest (path)
  "PATH's manifest plist.  *READ-EVAL* NIL: data, never code (GH #171)."
  (with-open-file (in path)
    (let ((*read-eval* nil) (*package* (find-package :common-lisp)))
      (read in))))

(defun %entry-with (entry &rest kvs)
  "ENTRY with each key in KVS applied, REPLACING (not shadowing) any
existing value: keys in KVS are stripped from ENTRY first, then KVS is
prepended.  Plain (SETF (GETF entry key) v) on a key ENTRY doesn't
already carry rebinds the local variable, not the cons ENTRIES still
holds elsewhere -- it never mutates the manifest (GH #171)."
  (let ((keys (loop for (k nil) on kvs by #'cddr collect k)))
    (append kvs
            (loop for (k v) on entry by #'cddr
                  unless (member k keys)
                    append (list k v)))))

(defun %journal-named-paths (clock)
  "EQUAL-hash set of every path a :RETIRED, :RETIRED-LIVE,
:RESTORED-FROM or :FROM key names in any journal record -- including a
rolled-back :RETIRE-LIVE-ABORTED's own path, which names no live
directory but must still never be re-minted (GH #171)."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (r (journal-records clock))
      (dolist (key '(:retired :retired-live :restored-from :from))
        (let ((v (getf r key)))
          (when v (setf (gethash (%trimmed-namestring v) set) t)))))
    set))

(defun %retired-path-for (clock live)
  "<LIVE>-retired-<E> for an E no directory already uses AND no journal
record already names.  Two retiring events with no commit between them
-- two RESTORE-SYSTEM calls in a row, or a rewind the cascade then
rebuilds -- otherwise compute the SAME name and the second rename fails
on the non-empty directory: consume epochs until the name is free.  The
journal check alone matters too: a rolled-back retry's aborted rename
names a path with no directory at all, and re-minting it would alias a
live :RETIRE-LIVE-ABORTED record onto a fresh attempt (GH #171)."
  (let ((named (%journal-named-paths clock)))
    (loop for path = (format nil "~A-retired-~D" live
                             (clock-current-epoch clock))
          while (or (probe-file (uiop:ensure-directory-pathname path))
                    (gethash path named))
          do (clock-next-epoch clock)
          finally (return path))))

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
it, reopens the restored generation and warns (GH #171).

If :RETIRE-LIVE was already journaled when the SECOND rename fails, the
rename-back is not enough on its own -- the journal still claims the
live generation moved to RETIRED, which it no longer has.  Once the
rename-back itself succeeds, a :RETIRE-LIVE-ABORTED record cancels it
(RETIRED-GENERATIONS' journal-join excludes any path a
:RETIRE-LIVE-ABORTED names)."
  (let* ((graph (lookup-graph name))
         (live (%trimmed-namestring (location graph)))
         (from (getf entry :from))
         (retired (%retired-path-for clock live))
         (done nil) (retire-live-ok nil))
    (%close-quiesced graph timeout)
    (handler-case
        (progn
          (%posix-rename live retired)
          (journal-append clock :retire-live :store name :retired retired)
          (setq retire-live-ok t)
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
          (let ((rolled-back
                  (ignore-errors (%posix-rename retired live) t)))
            (when (and retire-live-ok rolled-back)
              (ignore-errors
               (journal-append clock :retire-live-aborted
                               :store name :retired retired)))))
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
the same location, run REBUILD on it, journal :RESTORE :MODE :REBUILT.

Only :RECOVERY-POLICY carries over to the fresh MAKE-GRAPH call (v1
scope) -- :INDEX-BACKEND, :BUFFER-POOL-SIZE, spatial precision and
bucket counts do NOT; a caller relying on any of those must reopen and
reconfigure the rebuilt store itself afterward.

A failure in the rename or the :RETIRE-LIVE journal record itself is
rolled back: the live directory is renamed back and reopened fully
accepting before the original error is resignalled -- nothing changed.
A failure inside MAKE-GRAPH or REBUILD is NOT rolled back: the fresh,
possibly half-populated generation is left live, its predecessor
retired at :RETIRED-LIVE -- a :RESTORE :MODE :REBUILT :FAILED T journal
record names both before the original error is resignalled, so the
journal says what actually happened (GH #171)."
  (let* ((graph (lookup-graph name))
         (live (%trimmed-namestring (location graph)))
         (policy (store-recovery-policy live))
         (retired (%retired-path-for clock live)))
    (%close-quiesced graph timeout)
    (handler-case
        (progn
          (%posix-rename live retired)
          (journal-append clock :retire-live :store name :retired retired))
      (error (original)
        (ignore-errors (%posix-rename retired live))
        (handler-case (%reopen-and-resume name live clock t)
          (error (recovery)
            (error 'shadow-recovery-failed :original original
                                           :recovery recovery)))
        (error original)))
    (handler-case
        (let ((fresh (make-graph name (concatenate 'string live "/")
                                 :system-clock clock
                                 :recovery-policy policy)))
          (funcall rebuild name fresh)
          (journal-append clock :restore :store name :mode :rebuilt
                          :retired-live retired)
          (list :retired-live retired))
      (error (original)
        (ignore-errors
         (journal-append clock :restore :store name :mode :rebuilt
                         :failed t :retired-live retired))
        (error original)))))

(defun %dangling-into (clock store-id exclude)
  "((STORE-NAME . COUNT) ...) of edges in stores open under CLOCK, other
than EXCLUDE, whose FROM or TO carries STORE-ID's tag (spec R5).  A NIL
STORE-ID (a name STORE-REGISTRY-ID-FOR has never interned) matches
nothing, rather than every legacy v5 id (GH #171)."
  (let ((result nil))
    (when store-id
      (maphash
       (lambda (name graph)
         (when (and (not (member name exclude))
                    (%open-store-for-clock name clock))
           (let ((n 0))
             (map-edges (lambda (e)
                          (when (or (eql (id-store-tag (from e)) store-id)
                                    (eql (id-store-tag (to e)) store-id))
                            (incf n)))
                        graph)
             (when (> n 0) (push (cons name n) result)))))
       *graphs*))
    result))

(defun restore-system (clock epoch &key require-exact rebuild (timeout 60))
  "Execute PLAN-SYSTEM-RESTORE's manifest for EPOCH: every refusal fires
before any rename.  Rewinds first, then rebuilds, then cascades: a
:DERIVABLE store holding edges into a rebuilt store is rebuilt in turn
(fixpoint); an :AUTHORED one is left alone and reported :DANGLING N.
:RECOVERY-POLICY is the only option %REBUILD-ONE-STORE's MAKE-GRAPH call
carries forward for a rebuilt store (v1 scope; see its docstring for
what does not survive).  Writes <clock-dir>/restore-<Enow>.manifest and
signals RESTORE-INEXACT-WARNING when any store is rebuilt or inexact.
Returns the manifest (GH #171, spec S3-S4)."
  (let* ((manifest (plan-system-restore clock epoch
                                        :require-exact require-exact
                                        :rebuild rebuild))
         (entries (getf manifest :stores))
         (rebuilt nil))
    (dolist (e entries)
      (when (eq (getf e :action) :rewound)
        (let* ((pos (position (getf e :store) entries
                              :key (lambda (x) (getf x :store))))
               (extra (%restore-one-store
                      clock (getf e :store)
                      (list* :requested epoch e) timeout)))
          (setf (nth pos entries)
                (%entry-with e :retired-live (getf extra :retired-live))))))
    (dolist (e entries)
      (when (eq (getf e :action) :rebuilt)
        (let* ((pos (position (getf e :store) entries
                              :key (lambda (x) (getf x :store))))
               (extra (%rebuild-one-store
                      clock (getf e :store) rebuild timeout)))
          (setf (nth pos entries)
                (%entry-with e :retired-live (getf extra :retired-live)))
          (push (getf e :store) rebuilt))))
    ;; Cascade to a fixpoint over derivable dependents.  A store already
    ;; handled as :REWOUND above can still land here and get REBUILT: its
    ;; rewound generation may hold edges into a store that was just
    ;; rebuilt, whose pre-rebuild content no longer exists -- R5 treats
    ;; that as dangling regardless of how THIS store was restored, so
    ;; overwriting its action is correct, not a bug (GH #171).
    (let ((queue (copy-list rebuilt)))
      (loop while queue do
        (let* ((source (pop queue))
               (tag (store-registry-id-for source)))
          (loop for (name . n) in (%dangling-into clock tag rebuilt) do
            (let ((pos (position name entries
                                 :key (lambda (x) (getf x :store)))))
              (when pos
                (let ((entry (nth pos entries)))
                  (if (eq (store-recovery-policy
                           (location (lookup-graph name)))
                          :derivable)
                      (let ((extra (%rebuild-one-store
                                    clock name rebuild timeout)))
                        (push name rebuilt) (push name queue)
                        (setf (nth pos entries)
                              (%entry-with
                               entry
                               :action :rebuilt :exact nil
                               :state-at (clock-current-epoch clock)
                               :cascade-from source
                               :retired-live (getf extra :retired-live))))
                      ;; Accumulate: a second rebuilt source dangling
                      ;; into the same authored store must not erase
                      ;; the first's count (GH #171).
                      (setf (nth pos entries)
                            (%entry-with entry :dangling
                                        (+ n (or (getf entry :dangling)
                                                 0))))))))))))
    (setf (getf manifest :at) (clock-current-epoch clock))
    (%write-manifest clock manifest)
    (when (some (lambda (e) (or (eq (getf e :action) :rebuilt)
                                (and (eq (getf e :action) :rewound)
                                     (not (getf e :exact)))))
                entries)
      (warn 'restore-inexact-warning :manifest manifest))
    manifest))
