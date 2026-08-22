(in-package :graph-db)

;;; Shadow generations (GH #170).  A shadow is a consistent, unregistered
;;; copy of a live store's directory, opened for a bulk load under its own
;;; leased epoch range while the live store stays read-only.  See
;;; docs/superpowers/specs/2026-08-20-namespaces-design.md sec.8.

(defun %shadow-location (location)
  "LOCATION's shadow sibling: same parent, directory name suffixed
\"-shadow\" (GH #170)."
  (let* ((dir (uiop:ensure-directory-pathname location))
         (trimmed (string-right-trim "/" (namestring dir))))
    (uiop:ensure-directory-pathname (concatenate 'string trimmed "-shadow"))))

(defun %copy-directory-tree (source destination)
  "Plain recursive file copy, SOURCE to DESTINATION.  Called only while
the store is closed, so there are no mmap hazards.  NO shell-outs --
UIOP:COLLECT-SUB*DIRECTORIES walks SOURCE and UIOP:COPY-FILE moves each
file (GH #170)."
  (let ((source (uiop:ensure-directory-pathname source))
        (destination (uiop:ensure-directory-pathname destination)))
    (ensure-directories-exist destination)
    (uiop:collect-sub*directories
     source t t
     (lambda (dir)
       (ensure-directories-exist
        (merge-pathnames (uiop:enough-pathname dir source) destination))
       (dolist (file (uiop:directory-files dir))
         (uiop:copy-file
          file
          (merge-pathnames (uiop:enough-pathname file source) destination)))))
    destination))

(defun %shadow-suffix-p (path)
  "True when PATH's directory name ends in \"-shadow\" -- the entire
safety story for DISCARD-SHADOW, which deletes trees (GH #170)."
  (let ((trimmed (string-right-trim
                  "/" (namestring (uiop:ensure-directory-pathname path)))))
    (and (>= (length trimmed) 7)
         (string= "-shadow" trimmed :start2 (- (length trimmed) 7)))))

(define-condition shadow-recovery-failed (error)
  ;; The copy step failed AND the post-close recovery reopen also
  ;; failed: the store may be stuck closed and needs a manual
  ;; OPEN-GRAPH.  Both conditions are carried so neither is lost
  ;; (GH #170, fix round 1).
  ((original :initarg :original :reader shadow-recovery-failed-original)
   (recovery :initarg :recovery :reader shadow-recovery-failed-recovery))
  (:report (lambda (c s)
             (format s "SHADOW-STORE failed (~A) and the recovery ~
reopen ALSO failed (~A) -- the store is left closed; a manual ~
OPEN-GRAPH is required."
                     (shadow-recovery-failed-original c)
                     (shadow-recovery-failed-recovery c)))))

(defun %reopen-and-resume (name location clock reason)
  "OPEN-GRAPH + ATTACH-TO-SYSTEM-CLOCK + %SET-ACCEPTING-P REASON --
the resume sequence shared by SHADOW-STORE's happy path and its
copy-failure recovery path (GH #170)."
  (let ((reopened (open-graph name (namestring location)
                              :system-clock nil)))
    (attach-to-system-clock reopened clock)
    (%set-accepting-p (transaction-manager reopened) reason)
    reopened))

(defun shadow-store (graph &key (timeout 60))
  "Take a consistent shadow copy of GRAPH's store: quiesce (reason
:SWAPPING, TIMEOUT seconds to drain) -> CLOSE-GRAPH -> recursive file
copy to \"<location>-shadow/\" -> reopen GRAPH's own store and
ATTACH-TO-SYSTEM-CLOCK (service resumes) -> set the reopened graph's
ACCEPTING-P to :READ-ONLY (Kevin's ruling: reads and pins keep flowing;
a new write signals STORE-NOT-ACCEPTING-ERROR reason :SHADOW-LOAD, so no
write is ever silently discarded at swap).

Requires GRAPH be attached to a system clock -- the reopen needs it to
resume service and the shadow's own lease (see OPEN-SHADOW-GRAPH) is
drawn from the same clock.

Once CLOSE-GRAPH has run, the live store is durably closed; ANY error
from the copy or the happy-path reopen itself triggers a recovery
reopen (ACCEPTING-P restored to T, full service) BEFORE the original
error is re-signalled -- a failed shadow attempt must not leave the
live store stranded closed.  If recovery ALSO fails, SHADOW-RECOVERY-
FAILED is signalled instead, carrying both conditions (GH #170, fix
round 1).

Returns (values SHADOW-LOCATION REOPENED-GRAPH) on success.  The live
store is fully unavailable only for close+copy+reopen -- seconds --
and write-unavailable until a caller swaps it in or calls
ABANDON-SHADOW."
  (let* ((tm (transaction-manager graph))
         (name (graph-name graph))
         (location (location graph))
         (clock (graph-system-clock graph)))
    (unless clock
      (error "SHADOW-STORE requires GRAPH to be attached to a system ~
clock (GRAPH-SYSTEM-CLOCK is NIL) -- the shadow window resumes service ~
via ATTACH-TO-SYSTEM-CLOCK, which needs one."))
    (%quiesce-transaction-manager tm :swapping timeout)
    (let ((*graph* graph)
          (*quiesced-store-closing-p* t))
      (close-graph graph :snapshot-p t))
    (let ((shadow-location (%shadow-location location)))
      (handler-case
          (progn
            (%copy-directory-tree location shadow-location)
            (let ((reopened (%reopen-and-resume name location clock
                                                :read-only)))
              (values shadow-location reopened)))
        (error (original)
          (handler-case
              (%reopen-and-resume name location clock t)
            (error (recovery)
              (error 'shadow-recovery-failed
                     :original original :recovery recovery)))
          (error original))))))

(defun abandon-shadow (graph shadow-location)
  "The lifecycle exit that is not a swap: DISCARD-SHADOW the directory
and restore GRAPH's ACCEPTING-P to T (full service) (GH #170)."
  (discard-shadow shadow-location)
  (%set-accepting-p (transaction-manager graph) t)
  graph)

(defun %lease-file (shadow-location)
  (merge-pathnames "lease.dat"
                   (uiop:ensure-directory-pathname shadow-location)))

(defun %persist-lease (shadow-location start end)
  "Write (:LEASE-START START :LEASE-END END) readably to lease.dat, so
the RANGE survives outside this process.  Never NEXT -- see
OPEN-SHADOW-GRAPH's resume story: the cursor is derived from the
shadow's own durable highest-transaction-id on every open, not
persisted here, so this file never needs an fsync on the per-write hot
path (GH #170, fix round 1)."
  (with-open-file (out (%lease-file shadow-location)
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-readably* nil) (*print-pretty* nil))
      (prin1 (list :lease-start start :lease-end end) out))))

(defun %read-lease (shadow-location)
  "Read back lease.dat, or NIL if absent.  *READ-EVAL* NIL: this file
may have been copied from anywhere and is untrusted input (GH #170)."
  (let ((file (%lease-file shadow-location)))
    (when (probe-file file)
      (with-open-file (in file)
        (let ((*read-eval* nil))
          (read in nil nil))))))

(defun open-shadow-graph (shadow-location graph-name
                          &key lease fast-load expected-vectors
                               (buffer-pool-size nil))
  "OPEN-GRAPH :SHADOW-P T on SHADOW-LOCATION under GRAPH-NAME -- same
name as the live store, so its schema metadata instantiates identically
-- unregistered: not in *GRAPHS*, not in the open-store vector, no
replication.  STORE-ID is interned directly from the store registry, so
v8 node ids minted here carry the LIVE store's tag; RESOLVE-NODE-GRAPH
therefore still resolves them to the live graph.

LEASE is (START . END) -- from the STORE-DETACHMENT SHADOW-STORE's
caller holds, or from a direct CLOCK-LEASE-EPOCHS call when there was no
DETACH-STORE.  The RANGE is persisted as lease.dat in SHADOW-LOCATION so
it survives a crash/restart of this process; when LEASE is not given,
the persisted lease.dat supplies it instead (needed to reopen a shadow
across a restart).

The CURSOR (where allocation resumes) is never taken from LEASE or from
lease.dat -- it is derived fresh on every open from the shadow's own
durable state: NEXT = (MAX START (1+ LOAD-HIGHEST-TRANSACTION-ID)).  The
shadow's own committed transaction ids are the truth about what it has
already allocated; persisting a separate mutable cursor would need an
fsync on the bulk-load hot path AND would go stale the moment a crash
lost the last update.  If the derived NEXT is already past END, the
lease was fully consumed before this open -- EPOCH-LEASE-EXHAUSTED is
signalled immediately rather than quietly wrapping or reusing an id
(GH #170, fix round 1).

FAST-LOAD and EXPECTED-VECTORS are Task 4/5 hooks: accepted here, but
using either signals a clear \"arrives in a later task\" error rather
than silently doing nothing."
  (when fast-load
    (error "OPEN-SHADOW-GRAPH :FAST-LOAD is a Task 4 hook and is not ~
implemented yet (GH #170)."))
  (when expected-vectors
    (error "OPEN-SHADOW-GRAPH :EXPECTED-VECTORS is a Task 5 hook and is ~
not implemented yet (GH #170)."))
  (let* ((location
          (uiop:ensure-directory-pathname shadow-location))
         (open-args (list graph-name (namestring location)
                          :shadow-p t :system-clock nil)))
    (let ((graph (apply #'open-graph
                        (if buffer-pool-size
                            (append open-args
                                   (list :buffer-pool-size buffer-pool-size))
                            open-args))))
      (let* ((persisted (and (null lease) (%read-lease location)))
             (start (if lease (car lease) (getf persisted :lease-start)))
             (end (if lease (cdr lease) (getf persisted :lease-end))))
        (unless (and start end)
          (error "OPEN-SHADOW-GRAPH needs :LEASE (start . end); none was ~
given and ~A has no lease.dat." location))
        ;; Resume cursor: derived from the shadow's OWN durable state, not
        ;; persisted -- see the docstring and %PERSIST-LEASE (GH #170, fix
        ;; round 1).  LOAD-HIGHEST-TRANSACTION-ID is 0 on a shadow that has
        ;; never committed a write, so NEXT starts at START there.
        (let ((next (max start (1+ (load-highest-transaction-id graph)))))
          (when (> next end)
            (error 'epoch-lease-exhausted :name graph-name :end end))
          (setf (graph-epoch-lease graph)
                (make-epoch-lease :start start :next next :end end)))
        (%persist-lease location start end))
      graph)))

(defun discard-shadow (shadow-location)
  "Delete SHADOW-LOCATION's tree.  A shadow never registers (*GRAPHS*,
open-store vector), so there is nothing to close first -- just delete.
Gated HARD on the path ending in \"-shadow\" via %SHADOW-SUFFIX-P: this
function deletes directory trees, and that suffix check is the whole
safety story (GH #170)."
  (let ((dir (uiop:ensure-directory-pathname shadow-location)))
    (uiop:delete-directory-tree
     dir
     :validate #'%shadow-suffix-p
     :if-does-not-exist :ignore)))
