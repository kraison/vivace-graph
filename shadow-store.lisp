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

(defparameter *sparse-copy-chunk-size* (* 1024 1024)
  "Octets per read/write in %COPY-FILE-SPARSE.  1MB: big enough that the
per-chunk overhead is negligible, small enough that a single dirty
region near the end of an otherwise-empty multi-GB reservation doesn't
force writing the whole file (GH #170).")

(defun %all-zero-p (buffer count)
  "True when the first COUNT octets of BUFFER are all zero.

A typed loop, not CL:MISMATCH -- MISMATCH has no fast-path transform for
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) in SBCL and falls back to boxed,
per-element generic dispatch (~16ms/MB measured).  Each store reserves
~12GB of mostly-zero mmap regions (see %COPY-DIRECTORY-TREE), so every
shadow paid ~3+ minutes of pure CPU here -- the whole detach-suite's
'hang' (GH #170).  This typed DOTIMES compiles to a tight bounds-checked
loop (~0.8ms/MB measured, ~20x)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum count)
           (optimize (speed 3)))
  (dotimes (i count t)
    (unless (zerop (aref buffer i)) (return nil))))

(defun %copy-file-sparse (source destination)
  "Copy SOURCE to DESTINATION preserving holes: read in
*SPARSE-COPY-CHUNK-SIZE* octet chunks; an all-zero chunk is never
WRITE-SEQUENCEd, only skipped over via FILE-POSITION on the (freshly
created, hence already-zero) output -- seeking past unwritten bytes on
a fresh file leaves a hole instead of materializing them, which is the
entire point (see %COPY-DIRECTORY-TREE's docstring for why this
matters).  A bare seek never extends a file's on-disk length by itself
though (that needs an actual write at or past the target offset), so if
the copy ends in a skipped region DESTINATION would come up short --
handled by writing SOURCE's final byte whenever the last chunk
processed was skipped, which is a no-op when the last chunk was
written (the write already reached the true end) and otherwise pins
DESTINATION to SOURCE's exact length with one extra byte written (GH
#170)."
  (let* ((chunk-size *sparse-copy-chunk-size*)
         (buffer (make-array chunk-size :element-type '(unsigned-byte 8)))
         (length 0)
         (last-chunk-skipped-p nil))
    (with-open-file (in source :element-type '(unsigned-byte 8))
      (setf length (file-length in))
      (with-open-file (out destination :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede :if-does-not-exist :create)
        (loop
          (let ((n (read-sequence buffer in)))
            (when (zerop n) (return))
            (cond
              ((%all-zero-p buffer n)
               ;; FILE-POSITION is a FUNCTION, not a SETF place: call it
               ;; with the new position as a second argument.
               (file-position out (+ (file-position out) n))
               (setf last-chunk-skipped-p t))
              (t
               (write-sequence buffer out :end n)
               (setf last-chunk-skipped-p nil)))))
        ;; Pin the apparent LENGTH (see docstring): only needed when the
        ;; copy ends in a hole -- if the last chunk was written, the
        ;; write itself already put OUT at LENGTH.
        (when (and last-chunk-skipped-p (plusp length))
          (file-position out (1- length))
          (write-byte 0 out))))
    destination))

(defun %copy-directory-tree (source destination)
  "Recursive file copy, SOURCE to DESTINATION, preserving sparse holes
via %COPY-FILE-SPARSE.  Called only while the store is closed, so there
are no mmap hazards.  NO shell-outs -- UIOP:COLLECT-SUB*DIRECTORIES
walks SOURCE.

Load-bearing, not an optimization: a fresh, empty store already reserves
twelve ~1000MB mmap regions (heap.dat, indexes.dat, and a table.dat +
overflow.dat pair in each of the 5 lhash directories) that are almost
entirely unwritten zero bytes -- ~12GB apparent for ~0MB of real data. A
byte-for-byte copy (the previous UIOP:COPY-FILE implementation)
materializes every one of those holes, writing the full 12GB to disk per
shadow of an EMPTY store, worse for a store with real data in it. That
breaks the spec's \"copies in seconds\" premise and can exhaust disk
outright (GH #170)."
  (let ((source (uiop:ensure-directory-pathname source))
        (destination (uiop:ensure-directory-pathname destination)))
    (ensure-directories-exist destination)
    (uiop:collect-sub*directories
     source t t
     (lambda (dir)
       (ensure-directories-exist
        (merge-pathnames (uiop:enough-pathname dir source) destination))
       (dolist (file (uiop:directory-files dir))
         (%copy-file-sparse
          file
          (merge-pathnames (uiop:enough-pathname file source) destination)))))
    destination))

;;; Recovery policy (GH #170 Task 4).  A store's policy.dat records
;;; whether its state is DERIVABLE (rebuildable from elsewhere -- a
;;; shadow copied from a live store's data -- so a crash may discard it)
;;; or AUTHORED (the only durable record of its writes -- the default,
;;; and what every pre-#170 store implicitly is).  OPEN-SHADOW-GRAPH
;;; :FAST-LOAD gates on this alone.

(defun %policy-file (location)
  (merge-pathnames "policy.dat" (uiop:ensure-directory-pathname location)))

(defun store-recovery-policy (location)
  "Read LOCATION's recovery policy from policy.dat: :DERIVABLE or
:AUTHORED.  Absent file means :AUTHORED -- a store predating this
feature, or one nobody ever opted in, must not be treated as
derivable.  *READ-EVAL* NIL: untrusted input, same reasoning as
%READ-LEASE.  A file present but not one of the two keywords is a
hard error -- silently falling back to :AUTHORED would hide a corrupt
gate input (GH #170)."
  (let ((file (%policy-file location)))
    (if (probe-file file)
        (with-open-file (in file)
          ;; Full reader-control set, not just *READ-EVAL* (GH #234).
          (with-sidecar-input ()
            (let ((value (read in nil nil)))
              (unless (member value '(:derivable :authored))
                (error "~A does not hold a valid recovery policy (expected ~
:DERIVABLE or :AUTHORED, read ~S)." file value))
              value)))
        :authored)))

(defun set-store-recovery-policy (location policy)
  "Write POLICY (:DERIVABLE or :AUTHORED) readably to LOCATION's
policy.dat.  Errors on any other value -- this file is the sole gate
input for OPEN-SHADOW-GRAPH's :FAST-LOAD, so a bad value must not
silently authorize the WAL-free path (GH #170)."
  (unless (member policy '(:derivable :authored))
    (error "SET-STORE-RECOVERY-POLICY: POLICY must be :DERIVABLE or ~
:AUTHORED, got ~S." policy))
  (with-open-file (out (%policy-file location)
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    ;; Full printer-control set (GH #234).
    (with-sidecar-output ()
      (prin1 policy out)))
  policy)

(define-condition fast-load-requires-derivable (error)
  ;; The whole safety story for :FAST-LOAD: an :AUTHORED store's shadow
  ;; is the only durable record of anything written into it, so skipping
  ;; the WAL there would silently discard data on a crash (GH #170).
  ((location :initarg :location :reader fast-load-requires-derivable-location)
   (policy :initarg :policy :reader fast-load-requires-derivable-policy))
  (:report (lambda (c s)
             (format s "OPEN-SHADOW-GRAPH :FAST-LOAD T requires the ~
source store's recovery policy to be :DERIVABLE; ~A carries ~S."
                     (fast-load-requires-derivable-location c)
                     (fast-load-requires-derivable-policy c)))))

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
  "OPEN-GRAPH :INITIAL-ACCEPTING-STATE REASON + ATTACH-TO-SYSTEM-CLOCK --
the resume sequence shared by SHADOW-STORE's happy path and its
copy-failure recovery path.  REASON is applied to the fresh transaction
manager BEFORE the graph publishes to *GRAPHS*, not flipped after open
-- a post-open flip would leave a window where the just-reopened graph
is fully accepting and a racing writer could land a commit that belongs
in the doomed generation (GH #170, review finding I4).

LOCATION is normalised to a DIRECTORY pathname first: OPEN-GRAPH keeps
whatever it is handed, and a slashless string makes every
(MAKE-PATHNAME :defaults (LOCATION GRAPH)) sidecar -- transaction-id.dat
above all -- land in the store's PARENT directory (GH #171)."
  (let ((reopened (open-graph name (namestring
                                    (uiop:ensure-directory-pathname
                                     location))
                              :system-clock nil
                              :initial-accepting-state reason))
        (attached nil))
    ;; An attach failure must close the just-opened graph before
    ;; propagating: leaving it open (registered, .dirty on disk) makes
    ;; the NEXT open-graph here fail on the .dirty marker (GH #212).
    (unwind-protect
        (progn
          (attach-to-system-clock reopened clock)
          (setf attached t)
          reopened)
      (unless attached
        (let ((*graph* reopened))
          (ignore-errors (close-graph reopened :snapshot-p nil)))))))

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
ABANDON-SHADOW.

Refuses a MASTER-GRAPH/SLAVE-GRAPH/PEER-GRAPH with
DETACH-UNSUPPORTED-GRAPH-ERROR -- v1 scope, see that condition's
docstring (GH #170)."
  (%check-detach-supported graph 'shadow-store)
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
            ;; A stale "<location>-shadow/" from a previous, never-cleaned
            ;; SHADOW-STORE would otherwise MERGE (%COPY-DIRECTORY-TREE's
            ;; per-file :SUPERSEDE never clears the destination first) --
            ;; leftover .txn files there would then be replayed into the
            ;; fresh shadow on its next open.  Deleted via the same
            ;; %SHADOW-SUFFIX-P-validated path DISCARD-SHADOW uses (GH
            ;; #170, review finding I2).
            (when (probe-file shadow-location)
              (discard-shadow shadow-location))
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
    ;; Full printer-control set (GH #234).
    (with-sidecar-output ()
      (prin1 (list :lease-start start :lease-end end) out))))

(defun %read-lease (shadow-location)
  "Read back lease.dat, or NIL if absent.  *READ-EVAL* NIL: this file
may have been copied from anywhere and is untrusted input (GH #170)."
  (let ((file (%lease-file shadow-location)))
    (when (probe-file file)
      (with-open-file (in file)
        ;; Full reader-control set, not just *READ-EVAL* (GH #234).
        (with-sidecar-input ()
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
lost the last update.  If the derived NEXT is already AT OR PAST END
(the range is half-open [START,END), so NEXT = END means fully
consumed, not merely at the boundary), the lease was used up before
this open -- EPOCH-LEASE-EXHAUSTED is signalled immediately rather than
quietly wrapping, reusing an id, or deferring the failure to the first
write (GH #170, fix round 2).

FAST-LOAD (GH #170 Task 4): skip the .txn file and replication log for
every transaction committed against the returned graph -- the shadow's
heap/index writes are its only durable record until a later SWAP-IN-
SHADOW or DISCARD-SHADOW, so this is only safe when the SOURCE store's
recovery policy is :DERIVABLE (checked via STORE-RECOVERY-POLICY on
SHADOW-LOCATION -- the shadow dir carries the copied policy.dat, so
that copy is the gate input, not a fresh read of the live store).
:AUTHORED (including the no-policy-file default) signals
FAST-LOAD-REQUIRES-DERIVABLE instead of silently keeping the WAL.

EXPECTED-VECTORS (GH #170 Task 5), when given, presizes every vector
segment the shadow's graph object carries -- PRESIZE-VECTOR-SEGMENT on
each value of (VECTOR-SEGMENTS GRAPH), which OPEN-GRAPH has already
populated (RESTORE-VECTOR-SEGMENTS runs inside it, before this function
gets control) by opening or rebuilding whatever segment files the copy
carries.  A shadow whose graph declares no :VECTOR-INDEX slot, or one
whose owners have no live nodes yet, has an EMPTY vector-segments table
-- :EXPECTED-VECTORS is then a clean no-op, not an error: there is
nothing to presize, which is not a failure of the hook.  Any allocation
failure inside PRESIZE-VECTOR-SEGMENT (VECTOR-SEGMENT-CAPACITY-
EXHAUSTED) propagates unchanged, before OPEN-SHADOW-GRAPH sets up the
epoch lease below.

The lease (LEASE or lease.dat) and EXPECTED-VECTORS are validated
BEFORE OPEN-GRAPH runs, and every check made AFTER it (epoch-lease
exhaustion, PRESIZE-VECTOR-SEGMENT) runs under an UNWIND-PROTECT that
CLOSE-GRAPHs on any error -- an error exit used to leave the freshly
opened graph's mmaps held and its .dirty marker set, which made a
retried OPEN-SHADOW-GRAPH fail on \"not closed properly\" instead of
just re-raising the original problem (GH #170, review finding M1)."
  (when fast-load
    (let ((policy (store-recovery-policy shadow-location)))
      (unless (eq policy :derivable)
        (error 'fast-load-requires-derivable
               :location shadow-location :policy policy))))
  (when expected-vectors
    (check-type expected-vectors (integer 0)))
  (let* ((location (uiop:ensure-directory-pathname shadow-location))
         (persisted (and (null lease) (%read-lease location)))
         (start (if lease (car lease) (getf persisted :lease-start)))
         (end (if lease (cdr lease) (getf persisted :lease-end))))
    (unless (and start end)
      (error "OPEN-SHADOW-GRAPH needs :LEASE (start . end); none was ~
given and ~A has no lease.dat." location))
    (let* ((open-args (list graph-name (namestring location)
                            :shadow-p t :system-clock nil))
           (graph (apply #'open-graph
                        (if buffer-pool-size
                            (append open-args
                                   (list :buffer-pool-size buffer-pool-size))
                            open-args)))
           (ok nil))
      (unwind-protect
          (progn
            (when fast-load
              (setf (wal-suppressed-p graph) t))
            ;; Presize whatever segments this shadow actually carries (GH
            ;; #170 Task 5).  An empty VECTOR-SEGMENTS table (no
            ;; :VECTOR-INDEX owner has data yet) makes this loop a no-op,
            ;; by design -- see the docstring.
            (when expected-vectors
              (maphash (lambda (key segment)
                         (declare (ignore key))
                         (presize-vector-segment segment expected-vectors))
                       (vector-segments graph)))
            ;; Resume cursor: derived from the shadow's OWN durable state,
            ;; not persisted -- see the docstring and %PERSIST-LEASE (GH
            ;; #170, fix round 1).  LOAD-HIGHEST-TRANSACTION-ID is 0 on a
            ;; shadow that has never committed a write, so NEXT starts at
            ;; START there.
            (let ((next (max start (1+ (load-highest-transaction-id graph)))))
              ;; Half-open [start,end): NEXT == END means fully consumed,
              ;; not just "at the boundary" -- TM-NEXT-EPOCH's own runtime
              ;; check uses >= for the same reason (GH #170, fix round 2).
              (when (>= next end)
                (error 'epoch-lease-exhausted :name graph-name :end end))
              (setf (graph-epoch-lease graph)
                    (make-epoch-lease :start start :next next :end end)))
            (%persist-lease location start end)
            (setf ok t)
            graph)
        (unless ok
          (let ((*graph* graph))
            (ignore-errors (close-graph graph :snapshot-p nil))))))))

(defun %require-closed-shadow (shadow-location)
  "Signal a clear error unless SHADOW-LOCATION exists and carries no
.dirty marker -- SWAP-IN-SHADOW requires the shadow be CLOSE-GRAPH'd
before promotion (the contract: caller closes it, not this function).
Renaming a directory a graph still has mmapped open would corrupt
whatever ends up living at that name.  Called BEFORE any quiesce or
close of the live store (GH #170, fix round 1) -- a typo'd path or a
still-open shadow must cost nothing more than this PROBE-FILE, not a
live outage."
  (let ((dir (uiop:ensure-directory-pathname shadow-location)))
    (unless (probe-file dir)
      (error "SWAP-IN-SHADOW: shadow location ~A does not exist." dir))
    (when (probe-file (merge-pathnames ".dirty" dir))
      (error "SWAP-IN-SHADOW: ~A still carries .dirty -- close the ~
shadow (CLOSE-GRAPH) before swapping it in." dir))))

(defun %trimmed-namestring (location)
  "LOCATION (string or pathname) as a namestring with no trailing
slash -- %POSIX-RENAME's two arguments must name the directories
themselves, not \"dir/\" (GH #170)."
  (string-right-trim "/" (namestring (uiop:ensure-directory-pathname
                                      location))))

(define-condition swap-recovered-warning (warning)
  ;; Both renames were durably complete before something after them
  ;; failed -- the swap itself SUCCEEDED.  Recovering by reopening the
  ;; NEW generation and returning it normally is correct; this warning
  ;; exists so the caller can still notice the hiccup.  If ORIGINAL came
  ;; from JOURNAL-APPEND itself, the :SWAP record may be missing from
  ;; the clock's journal even though the swap happened -- #212/#171
  ;; territory (GH #170, fix rounds 1-2).
  ((original :initarg :original :reader swap-recovered-warning-original))
  (:report (lambda (c s)
             (format s "SWAP-IN-SHADOW's renames completed, but ~
something after them failed (~A); recovered by reopening the NEW ~
generation instead -- the swap itself succeeded, though its :SWAP ~
journal record may be missing (GH #212) if that is what failed."
                     (swap-recovered-warning-original c)))))

(defun %swap-completed-p (progress)
  "True when PROGRESS (see %SWAP-IN-SHADOW-1) marks BOTH RENAMES
already durably complete -- NOT the :SWAP journal record, which is
best-effort and can fail after the renames without undoing them (GH
#170 fix round 2, #212).  Pulled out as its own predicate so the
discrimination SWAP-IN-SHADOW's failure handler makes -- recover onto
the OLD store and resignal, vs. recover onto the NEW one and return --
is unit-testable on its own."
  (and (aref progress 0) t))

(defun %swap-in-shadow-1 (name location shadow-location clock progress)
  "The risky middle of SWAP-IN-SHADOW, run after the live store is
already closed: rename live away, rename shadow in, journal, reopen.
PROGRESS is a 2-element vector; element 0 is set true and element 1 set
to RETIRED-PATH the instant BOTH RENAMES are durably complete -- that,
not the journal record, is what \"the swap happened\" means (GH #170,
fix round 2 / #212): the data is only ever un-findable under its old OR
new name during the renames themselves, and once the second rename
lands, the live location holds the new generation no matter what
happens next.  JOURNAL-APPEND runs AFTER the flip and is therefore
best-effort: a failure there still means the swap succeeded, just
possibly without a :SWAP record (see SWAP-IN-SHADOW's docstring).  The
caller's HANDLER-CASE uses PROGRESS to discriminate a pre-completion
failure (recover the OLD store) from a post-completion one (recover the
NEW store).  Split out so that HANDLER-CASE reads cleanly."
  (let* ((live (%trimmed-namestring location))
         (shadow (%trimmed-namestring shadow-location))
         (retired-path (format nil "~A-retired-~D" live
                               (clock-current-epoch clock))))
    ;; Live renamed away FIRST: the data always exists under some name.
    ;; A crash between these two renames is #171's territory (see the
    ;; docstring) -- not recovered here.
    (%posix-rename live retired-path)
    (%posix-rename shadow live)
    ;; Completion is THIS point, not the journal record below (#212).
    (setf (aref progress 0) t (aref progress 1) retired-path)
    ;; The promoted generation's lease.dat (copied in from the shadow) has
    ;; no meaning once this location is a plain, non-shadow live store --
    ;; delete it so it cannot be mistaken for a live lease later (GH #170,
    ;; review finding M2).
    (ignore-errors
     (delete-file (merge-pathnames "lease.dat"
                                   (uiop:ensure-directory-pathname live))))
    (journal-append clock :swap :store name :retired retired-path)
    (let ((new-graph (open-graph name live :system-clock nil))
          (attached nil))
      ;; Attach failure closes NEW-GRAPH before propagating, or the
      ;; recovery reopen deterministically dies on .dirty (GH #212).
      (unwind-protect
          (progn
            (attach-to-system-clock new-graph clock)
            (setf attached t))
        (unless attached
          (let ((*graph* new-graph))
            (ignore-errors (close-graph new-graph :snapshot-p nil)))))
      (values new-graph retired-path))))

(defun swap-in-shadow (graph shadow-location &key (timeout 60))
  "Promote SHADOW-LOCATION into GRAPH's place: validate SHADOW-LOCATION
(exists, no .dirty) -> quiesce GRAPH (reason :SWAPPING, TIMEOUT seconds
to drain) -> CLOSE-GRAPH -> rename the live directory to
\"<location>-retired-<epoch>\" (EPOCH from CLOCK-CURRENT-EPOCH) ->
rename SHADOW-LOCATION to the live location -> JOURNAL-APPEND :SWAP
(:store name :retired retired-path) -> OPEN-GRAPH at the live location
+ ATTACH-TO-SYSTEM-CLOCK.  Returns (values NEW-GRAPH RETIRED-PATH); the
retired directory is kept, not deleted.

SHADOW-LOCATION must already be a CLOSED shadow (no .dirty marker) --
this function does not close an open shadow handle for you (GH #170).

Requires GRAPH be attached to a system clock, same as SHADOW-STORE.
The SHADOW-LOCATION validation runs FIRST, before anything touches the
live store: a typo'd path or a shadow that is still open must never
cost a read-and-write outage, only a PROBE-FILE and an immediate
signal -- GRAPH is untouched, ACCEPTING-P never leaves T (GH #170, fix
round 1).

Two further failure outcomes, discriminated by whether BOTH RENAMES
had already completed (see %SWAP-IN-SHADOW-1's PROGRESS argument) --
completion is the second rename, NOT the :SWAP journal record after it
(GH #170, fix round 2 / #212):

- NOT YET COMPLETE (a rename itself failed): the swap did not happen.
  Reopen the OLD store, restore ACCEPTING-P to T, and re-signal the
  original error, mirroring SHADOW-STORE's copy-failure recovery.
- ALREADY COMPLETE (the renames landed; JOURNAL-APPEND, OPEN-GRAPH, or
  ATTACH-TO-SYSTEM-CLOCK failed after them): the swap SUCCEEDED -- the
  live location already holds the new generation's files.  Resignalling
  here would be a lie: the caller would conclude the swap didn't happen
  and could, e.g., retry it against a shadow location that no longer
  exists.  Instead: reopen the NEW generation, signal
  SWAP-RECOVERED-WARNING (a WARNING, not an ERROR) carrying the original
  condition, and RETURN (values NEW-GRAPH RETIRED-PATH) normally.  The
  :SWAP journal record itself is therefore best-effort once the renames
  land: if JOURNAL-APPEND is what failed, the swap still succeeded but
  its journal record may be missing -- a gap for a human or #171's
  future recovery tooling to notice, tracked as #212.

Either recovery reopen itself failing is unchanged: SHADOW-RECOVERY-
FAILED, carrying both conditions.

A failure BETWEEN the two renames is NOT recovered here: the live data
sits at the retired path and the live location may be missing or
half-replaced; manual recovery is REPAIR-INTERRUPTED-SWAP (GH #171).

Refuses a MASTER-GRAPH/SLAVE-GRAPH/PEER-GRAPH with
DETACH-UNSUPPORTED-GRAPH-ERROR -- v1 scope, see that condition's
docstring (GH #170)."
  (%check-detach-supported graph 'swap-in-shadow)
  (let* ((tm (transaction-manager graph))
         (name (graph-name graph))
         (location (location graph))
         (clock (graph-system-clock graph)))
    (unless clock
      (error "SWAP-IN-SHADOW requires GRAPH to be attached to a system ~
clock (GRAPH-SYSTEM-CLOCK is NIL)."))
    ;; Validate BEFORE touching the live store at all (fix round 1).
    (%require-closed-shadow shadow-location)
    (%quiesce-transaction-manager tm :swapping timeout)
    (let ((*graph* graph)
          (*quiesced-store-closing-p* t))
      (close-graph graph :snapshot-p t))
    (let ((progress (vector nil nil)))
      (handler-case
          (%swap-in-shadow-1 name location shadow-location clock progress)
        (error (original)
          (if (%swap-completed-p progress)
              ;; Renames already durable: the swap SUCCEEDED (the :SWAP
              ;; journal record itself is best-effort, see above).
              ;; Recover onto the NEW generation and return normally.
              (handler-case
                  (let ((recovered (%reopen-and-resume name location clock t)))
                    (warn 'swap-recovered-warning :original original)
                    (return-from swap-in-shadow
                      (values recovered (aref progress 1))))
                (error (recovery)
                  (error 'shadow-recovery-failed
                         :original original :recovery recovery)))
              (progn
                (handler-case
                    (%reopen-and-resume name location clock t)
                  (error (recovery)
                    (error 'shadow-recovery-failed
                           :original original :recovery recovery)))
                (error original))))))))

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
