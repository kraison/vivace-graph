(in-package :graph-db)

#+lispworks(deftype word () '(unsigned-byte 64))
#+ecl(deftype word () '(unsigned-byte 64))
(cffi:defctype size :unsigned-int)
(deftype uint32 () '(integer 0 4294967295))
(deftype uint40 () '(integer 0 1099511627775))
(deftype uint64 () '(integer 0 18446744073709551615))

(defstruct (mapped-file
             (:conc-name m-)
             (:predicate mapped-file-p))
  path pointer fd
  ;; Length, in bytes, of the virtual-address window reserved for this mapping
  ;; (computed at open from the file's size then: see *mmap-reservation-multiplier*
  ;; and *mmap-min-reservation*).  POINTER is fixed at the base of that window
  ;; for the life of the mapping: the file is mapped into the head, and
  ;; extend-mapped-file maps more of it into the reserved tail with MAP_FIXED.
  ;; Because POINTER never moves and the reservation is never unmapped until
  ;; close, concurrent readers never fault and need no lock.  See
  ;; docs/mmap-remap-race-plan.md.
  ;;
  ;; RESERVED-SIZE can GROW without POINTER moving:
  ;; EXTEND-RESERVATION-IN-PLACE (below) claims the address range immediately
  ;; after the window, which is the cheap way a vector segment grows past its
  ;; reservation.  Readers are unaffected -- there is nothing for them to
  ;; observe.
  ;;
  ;; ONE EXCEPTION MOVES POINTER, and it is not a general one:
  ;; RELOCATE-VECTOR-SEGMENT-MAPPING (below) does move POINTER, for a vector
  ;; segment only, because a segment -- unlike the heap and the linear hash --
  ;; holds an rw-lock over every one of its readers.  It is the FALLBACK for
  ;; when the adjacent range above cannot be claimed.  Read its docstring
  ;; before calling it from anywhere else; the short version is: don't.
  (reserved-size 0 :type integer))

;;; Diagnostic: count SEGV-retries in the accessor :around methods.  With the
;;; stable-address mapping no remap moves the pointer, so this must stay 0 under
;;; concurrency; the regression test asserts it.  The :around handlers remain a
;;; cheap backstop.  Plain incf (a racy count is fine for a diagnostic).
(defparameter *mmap-segv-retries* 0)

(defstruct mpointer mmap loc)

(defmethod mapped-file-length ((mapped-file mapped-file))
  (%posix-file-size-fd (m-fd mapped-file)))

(defmethod set-byte :around (mf offset byte)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))))

;; Raw write.  Safe lock-free because the mapping's base pointer is stable for
;; its lifetime (see mmap-file / extend-mapped-file).
(declaim (inline %set-byte))
(defun %set-byte (mapped-file offset byte)
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  #+ecl
  (ffi:c-inline ((m-pointer mapped-file) offset byte)
                (:pointer-void :cl-index :unsigned-byte) :unsigned-byte
                "*((unsigned char *)(((char*)#0)+#1))=#2"
                :one-liner t)
  #-ecl
  (setf (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset) byte))

(defmethod set-byte ((mapped-file mapped-file) offset byte)
  ;;(log:debug "SET-BYTE: ~A ADDR ~A TO ~A" (m-path mapped-file) offset byte)
  ;; Lock-free: the pointer is stable (see mmap-file / extend-mapped-file).
  (%set-byte mapped-file offset byte))

;; Raw read.  Safe lock-free because the mapping's base pointer is stable.
(declaim (inline %get-byte))
(defun %get-byte (mapped-file offset)
  (declare (type word offset))
  #+ecl
  (ffi:c-inline ((m-pointer mapped-file) offset)
                (:pointer-void :cl-index) :unsigned-byte
                "*((unsigned char *)(((char*)#0)+#1))"
                :one-liner t)
  #-ecl
  (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset))

(defmethod get-byte ((mapped-file mapped-file) offset)
  ;; Lock-free: the pointer is stable (see mmap-file / extend-mapped-file).
  (%get-byte mapped-file offset))


(defmethod get-bytes ((mapped-file mapped-file) offset length)
  (declare (type word offset length))
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (%get-byte mapped-file (+ i offset))))
    vec))

(defmethod set-bytes :around (mf vec offset length)
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))
    #+ecl
    (ext:segmentation-violation (c)
      (incf *mmap-segv-retries*)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf vec offset length))))

(defmethod set-bytes ((mapped-file mapped-file) vec offset length)
  (declare (type word offset length))
  (dotimes (i length)
    (%set-byte mapped-file (+ i offset) (aref vec i)))
  vec)

(defmethod size-of ((mmap mapped-file))
  (%file-size (m-path mmap)))

(defun mmap-file (file &key (create-p t) (size (* 4096 25600)) reservation)
  "Use mmap() to map FILE into memory.

Reserves a virtual-address window (PROT_NONE, anonymous, MAP_NORESERVE — address
space only) and maps the file into the head of it with MAP_FIXED.  The returned
mapped-file's POINTER is the base of that window, and GROWTH NEVER MOVES IT:
EXTEND-MAPPED-FILE grows by re-mapping the file into the reserved window, so
concurrent readers never fault.  The file may grow up to the reservation, which
defaults to *MMAP-RESERVATION-MULTIPLIER* times SIZE (floored at
*MMAP-MIN-RESERVATION*); pass RESERVATION to override.

ONE THING CAN MOVE POINTER, and it is not growth:
RELOCATE-VECTOR-SEGMENT-MAPPING re-reserves a larger window and re-maps the
file into it, which is how a VECTOR SEGMENT grows past its reservation.  It is
callable ONLY by a subsystem that can exclude every reader of the mapping for
the duration (today: the vector segment, under its own rw-lock) — read that
function's contract before assuming a base pointer you cached is still valid.
For every other mapped file in this system the reservation is a hard ceiling
and POINTER is stable for the life of the mapping."
  (log:debug "Opening mmap ~A" file)
  (when (and (not create-p) (not (probe-file file)))
    (error "mmap-file: ~A does not exist and create-p is not true." file))
  (let ((fd (%posix-open
             file
             (if create-p
                 (logior +o-creat+ +o-rdwr+)
                 +o-rdwr+)))
        (base nil)
        (reserved nil)
        (ok nil))
    ;; Unwind cleanly on any failure once FD is open: an interrupted create
    ;; step, or a MAP_FAILED file-map (the second %POSIX-MMAP below) after the
    ;; anonymous window was already reserved, must not leak the fd or the VA
    ;; window.  Before this, a failed file-map left both open -- ordinarily a
    ;; ~1 GiB leak per retry, now up to *SEGMENT-MIN-RESERVATION* (16 GiB) for
    ;; a vector segment, so a retry loop against a nearly-full filesystem or
    ;; address space could exhaust it far faster than before.
    (unwind-protect
        (progn
          (when create-p
            (%posix-extend-file-backing fd size)
            ;; Belt-and-suspenders: set the mode explicitly to #o640 (owner rw,
            ;; group r) so database files are reopenable without being
            ;; world-accessible, even if the open() mode argument is not
            ;; honored on some platform.
            (%posix-fchmod fd #o640))
          ;; Make sure the file size is set right!
          (setq size (%posix-file-size-fd fd))
          (setq reserved (max (or reservation
                                  (* *mmap-reservation-multiplier* size))
                              *mmap-min-reservation*
                              size))
          ;; Reserve the address window with no access and no backing.
          (setq base (%posix-mmap
                      (cffi:null-pointer)
                      reserved
                      +prot-none+
                      (logior +map-private+
                              +map-anonymous+
                              +map-noreserve+)
                      -1
                      0))
          ;; Map the file over the head of the reservation (replaces the
          ;; PROT_NONE pages for [base, base+size); MAP_FIXED keeps the addr).
          (let* ((pointer (%posix-mmap
                           base
                           size
                           (logior +prot-read+ +prot-write+)
                           (logior +map-shared+ +map-fixed+)
                           fd
                           0))
                 (path (truename file))
                 (mf (make-mapped-file :path path
                                       :fd fd
                                       :pointer pointer
                                       :reserved-size reserved)))
            (setf ok t)
            mf))
      (unless ok
        (when base (ignore-errors (%posix-munmap base reserved)))
        (ignore-errors (%posix-close fd))))))

(defmethod sync-region ((mapped-file mapped-file) &key addr length
                        (sync +ms-sync+))
  (%posix-msync (or addr (m-pointer mapped-file))
                (or length (mapped-file-length mapped-file))
                sync))

(defmethod munmap-file ((mapped-file mapped-file) &key (save-p nil)
                        (sync +ms-sync+))
  (when save-p
    ;;(log:debug "Calling msync on ~S" mapped-file)
    ;; Only the file-backed head is dirty/syncable, not the reserved tail.
    (%posix-msync (m-pointer mapped-file)
                  (mapped-file-length mapped-file)
                  sync))
  ;;(log:debug "Calling munmap on ~S" mapped-file)
  ;; Release the whole reserved window (file mapping + PROT_NONE tail).
  (%posix-munmap (m-pointer mapped-file)
                 (if (plusp (m-reserved-size mapped-file))
                     (m-reserved-size mapped-file)
                     (mapped-file-length mapped-file)))
  (%posix-close (m-fd mapped-file))
  (setf (m-pointer mapped-file) nil)
  nil)

;; One platform-independent implementation: there is no mremap and no munmap of
;; the live region.  We grow the backing file, then re-map the whole file at the
;; SAME base address (MAP_FIXED, offset 0).  MAP_FIXED replacement is atomic, so
;; a concurrent reader of an existing offset never observes an unmapped address,
;; and the base pointer is unchanged — no lock and no SEGV.  See
;; docs/mmap-remap-race-plan.md.
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  (log:debug "EXTENDING MMAP ~A" mapped-file)
  (let* ((old-len (mapped-file-length mapped-file))
         (new-len (+ old-len length)))
    (when (> new-len (m-reserved-size mapped-file))
      (error "mmap reservation exhausted for ~A: need ~D bytes, reserved ~D.~%~
The reservation is computed at open from the file's size then, so reopening the ~
graph recomputes it against the now-larger file and grants fresh headroom.  To ~
raise it up front, bind GRAPH-DB::*MMAP-RESERVATION-MULTIPLIER* or ~
GRAPH-DB::*MMAP-MIN-RESERVATION* (or MAKE-GRAPH's heap/index size) before ~
opening the graph.  Exception: a VECTOR SEGMENT never reaches this error at ~
all -- its floor is GRAPH-DB:*SEGMENT-MIN-RESERVATION*, and %SEG-GROW ~
re-reserves and relocates its mapping (under the segment's write lock) before ~
calling this, so for segments the reservation is not a ceiling.  Only if that ~
relocation is disabled or fails does a segment signal, and it signals ~
VECTOR-SEGMENT-CAPACITY-EXHAUSTED, not this."
             (m-path mapped-file) new-len (m-reserved-size mapped-file)))
    ;; Extend the backing file first so the newly mapped pages have storage.
    (%posix-extend-file-backing (m-fd mapped-file) new-len)
    ;; Re-map [0, new-len) over the reserved window at the same base.
    (%posix-mmap (m-pointer mapped-file)
                 new-len
                 (logior +prot-read+ +prot-write+)
                 (logior +map-shared+ +map-fixed+)
                 (m-fd mapped-file)
                 0)
    mapped-file))

;;; ---------------------------------------------------------------------------
;;; Growing a reservation.  Two mechanisms, tried in this order by
;;; %SEG-ENSURE-RESERVATION:
;;;
;;;   1. EXTEND-RESERVATION-IN-PLACE -- claim the address range immediately
;;;      AFTER the window.  One syscall, M-POINTER does not move, no reader is
;;;      affected.  Works only when that range is free, which is RARER than the
;;;      design assumed -- see its docstring's "HOW OFTEN THIS WORKS".
;;;   2. RELOCATE-VECTOR-SEGMENT-MAPPING -- move the whole window elsewhere.
;;;      MOVES M-POINTER, so it is safe ONLY for a subsystem that can exclude
;;;      every one of its own readers.  READ THE CONTRACT BEFORE ADDING A
;;;      SECOND CALLER.
;;; ---------------------------------------------------------------------------

;;; Diagnostics: which of the two actually ran.  An operator (or a test) can
;;; otherwise not tell a free extension from a relocation, and they have very
;;; different costs and very different risk.  Plain INCF; a racy count is fine.
(defparameter *segment-adjacent-extensions* 0
  "Count of reservations grown IN PLACE by claiming the adjacent range.")
(defparameter *segment-relocations* 0
  "Count of reservations grown by RELOCATING the mapping (M-POINTER moved).")

;; NOTINLINE so a test can intercept this with an FDEFINITION swap.  ECL
;; compiles a same-file call into a direct C call, bypassing the symbol
;; entirely, so without this the fault injection in
;; SEGMENT-ADJACENT-CLAIM-AT-THE-WRONG-ADDRESS-IS-UNMAPPED-AND-FALLS-BACK
;; silently observed nothing on ECL and the test failed against correct code.
;; (%POSIX-MUNMAP is declaimed INLINE and cannot be hooked at all; this wrapper
;; is the supported observation point.)  Costs nothing: an error/teardown path.
(declaim (notinline %munmap-or-warn))
(defun %munmap-or-warn (addr length what)
  "munmap(2) LENGTH bytes at ADDR, reporting rather than ignoring a failure.
Returns munmap's return code (0 on success, -1 on error).

WHY THIS IS NOT `(ignore-errors (%posix-munmap ...))'.  Both callers are on the
RELOCATION path, whose characteristic failure mode is address-space pressure
 (RLIMIT_AS, VA exhaustion) inside a process that stays up for months.  A munmap
that the kernel refuses there leaks an ENTIRE reservation -- up to
*SEGMENT-MIN-RESERVATION* (16 GiB by default) of address space per occurrence --
which compounds precisely the condition that provoked the relocation, and would
otherwise do so with no trace at all.  It must be loud.

It must also not SIGNAL: one caller runs in an UNWIND-PROTECT cleanup form while
a real failure is already unwinding, where an error here would replace the
diagnosis with itself.  Hence log + WARN, never ERROR."
  (let ((rc (handler-case (%posix-munmap addr length)
              (error (e)
                (log:error "munmap of the ~A raised ~A" what e)
                -1))))
    (unless (eql rc 0)
      (log:error "munmap of the ~A (~D bytes at ~A) FAILED (rc ~A): that address ~
space is leaked for the life of the process."
                 what length addr rc)
      (warn "munmap of the ~A failed (rc ~A); ~D bytes of address space leaked."
            what rc length))
    rc))

(defun %round-up (n multiple)
  (* multiple (ceiling n multiple)))

(defun extend-reservation-in-place (mapped-file new-reservation)
  "Try to grow MAPPED-FILE's virtual-address reservation to at least
NEW-RESERVATION bytes by claiming the range IMMEDIATELY AFTER the current
window.  Returns TWO values, (SUCCESS-P REASON):

  (values T :ALREADY-COVERED) -- NEW-RESERVATION already fit inside the
    current reservation.  NOTHING was claimed, M-RESERVED-SIZE did not change,
    and *SEGMENT-ADJACENT-EXTENSIONS* was NOT incremented.  A caller that
    treats the primary value alone as \"an extension happened\" would be
    wrong here -- check REASON, not just SUCCESS-P, if that distinction
    matters to it.
  (values T :CLAIMED)         -- the adjacent range was actually claimed; see
    below.
  (values NIL :OCCUPIED)      -- the requested address is occupied by
    something else: either the kernel refused it outright (EEXIST under
    +MAP-FIXED-NOREPLACE+), or -- on a platform that ignores the flag -- the
    call silently placed the mapping elsewhere, which was then unmapped.
    Either way the diagnosis is address CONTENTION, not a mapping failure.
  (values NIL <condition>)    -- the mmap(2) call itself signalled, for a
    reason that is NOT specifically about that one address (e.g. ENOMEM /
    RLIMIT_AS).  The raw condition is returned so a caller can report the
    actual cause instead of assuming occupancy.
  (values NIL :NO-BASE)       -- MAPPED-FILE has no established mapping yet;
    there is nothing to extend adjacent to.

On a :CLAIMED success M-RESERVED-SIZE is larger and M-POINTER IS UNCHANGED: the
window simply got bigger, no byte moved, no reader was disturbed, and none of
RELOCATE-VECTOR-SEGMENT-MAPPING's cost or risk was paid.  On any NIL outcome
NOTHING is mutated -- not M-POINTER, not M-RESERVED-SIZE, and no mapping is
left behind -- so the caller can fall straight through to relocation.

HOW OFTEN THIS WORKS -- LESS THAN THE DESIGN ASSUMED, AND THAT IS MEASURED.  The
premise was that a sparse 64-bit address space leaves the adjacent range free
most of the time.  It does not.  Linux's default TOP-DOWN mmap allocator places a
mmap(NULL, ...) window flush against the BOTTOM of the existing mappings, so the
range immediately ABOVE a newly created window is occupied by construction.  With
a production-sized 16 GiB reservation on Linux 5.15 and on Linux 4.15 the window
ended at exactly the first byte of libssl.so.3, and claims of 1 page, 1 MiB,
1 GiB and 8 GiB were ALL refused.  The legacy bottom-up layout (ulimit -s
unlimited) behaved identically, and Darwin declines a hint the same way.  It
succeeds where the window happens to sit below a hole -- which does happen, but
is not something to plan around.  The reason to keep it is that a miss costs one
mmap on a path that is already rare; the reason not to rely on it is above.

WHY THIS IS SAFE ON EVERY PLATFORM, INCLUDING THE ONES WITHOUT THE FLAG.  The
claim asks for +MAP-FIXED-NOREPLACE+ where that constant exists (Linux 4.17+),
which makes the kernel REJECT rather than place the mapping elsewhere.  Where it
does not exist -- Darwin, and Linux before 4.17, which ignores the unknown bit
-- the address argument is merely an advisory HINT, so the mapping can land
somewhere other than where we asked.  It does NOT degrade to MAP_FIXED and it
does NOT evict the occupant; that was measured, see +MAP-FIXED-NOREPLACE+'s
comment in posix.lisp.  PLAIN MAP_FIXED IS NEVER PASSED HERE and must never be
added: it would silently replace whatever mapping already occupies the range --
another mmapped graph file, the Lisp heap, a shared library -- and the corruption
would be invisible until something read from it.

The safety property is therefore the POST-HOC ADDRESS COMPARISON below, not the
flag: if what came back is not exactly what was asked for, it is somebody else's
neighbourhood, so unmap it and fail.  That check is unconditional, which is why
this needs no kernel-version gate and no platform gate.

PAGE ALIGNMENT.  A reservation is whatever byte count the policy computed, so
the true end of the window is the current size rounded UP to a page.  Claiming
from anywhere else would either overlap the window's own last page or leave a
hole, and an unaligned ADDR is an outright EINVAL under MAP_FIXED_NOREPLACE.
The new size is recorded as (rounded-up old + rounded-up claim), which is what
MUNMAP-FILE and RELOCATE-VECTOR-SEGMENT-MAPPING must later release: a single
munmap spans both mappings, since they are adjacent by construction."
  (let* ((reserved (m-reserved-size mapped-file))
         (base (m-pointer mapped-file)))
    (cond
      ((null base) (values nil :no-base))
      ((<= new-reservation reserved) (values t :already-covered)) ; nothing to do
      (t
       (let* ((page (%posix-page-size))
              (aligned-reserved (%round-up reserved page))
              (want (+ (cffi:pointer-address base) aligned-reserved))
              (length (%round-up (- new-reservation reserved) page))
              (mmap-error nil)
              (got (handler-case
                       (%posix-mmap (cffi:make-pointer want)
                                    length
                                    +prot-none+
                                    (logior +map-private+
                                            +map-anonymous+
                                            +map-noreserve+
                                            +map-fixed-noreplace+)
                                    -1
                                    0)
                     ;; EEXIST where the flag is honoured, ENOMEM / RLIMIT_AS
                     ;; anywhere.  Either way: no mapping was made, fall back --
                     ;; but keep the condition so the caller can distinguish an
                     ;; actual mapping failure from mere address contention.
                     (error (e)
                       (log:debug "adjacent reservation claim for ~A refused: ~A"
                                  (m-path mapped-file) e)
                       (setf mmap-error e)
                       nil))))
         (cond
           ((null got) (values nil mmap-error))
           ((/= (cffi:pointer-address got) want)
            ;; Hint placement on a kernel without the flag: we were given a
            ;; perfectly good mapping somewhere useless.  Give it back.  This IS
            ;; occupancy -- the kernel could not honour the hint precisely
            ;; because something else already lives at WANT.
            (log:debug "adjacent reservation claim for ~A landed at ~D, not ~D; ~
releasing it and falling back to relocation"
                       (m-path mapped-file) (cffi:pointer-address got) want)
            (%munmap-or-warn got length "misplaced adjacent reservation claim")
            (values nil :occupied))
           (t
            (setf (m-reserved-size mapped-file) (+ aligned-reserved length))
            (incf *segment-adjacent-extensions*)
            (log:debug "extended ~A's reservation in place to ~D bytes (base ~D ~
unchanged)"
                       (m-path mapped-file) (m-reserved-size mapped-file)
                       (cffi:pointer-address base))
            (values t :claimed))))))))

(defun relocate-vector-segment-mapping (mapped-file new-reservation)
  "Move MAPPED-FILE's mapping into a fresh, larger reserved window: reserve
NEW-RESERVATION bytes of PROT_NONE anonymous address space, MAP_FIXED the file
into its head, publish the new base in M-POINTER, then munmap the old window.
Returns MAPPED-FILE.  A no-op (and not an error) if the current reservation
already covers NEW-RESERVATION.

⚠ THIS MOVES M-POINTER, WHICH THE WHOLE LOCK-FREE READ PATH ASSUMES NEVER
MOVES.  Everything else in this file exists to guarantee the opposite: MMAP-FILE
reserves a window once and EXTEND-MAPPED-FILE grows strictly INSIDE it, so
GET-BYTE/SET-BYTE and segment.lisp's SAP-based decoder can dereference the base
pointer with no lock and never fault (see the MAPPED-FILE docstring and
docs/mmap-remap-race-plan.md).  This function breaks that guarantee for the
duration of the call and then re-establishes it at a different address.

THE CALLER MUST HOLD WRITE-EXCLUSIVE ACCESS AGAINST *EVERY* READER OF THIS
MAPPING.  Not \"should\"; there is no other protection.  A reader that loaded
the old base before the swap dereferences unmapped memory after the munmap
below, which is a SIGSEGV at best and a read of some later, unrelated mapping
at worst.

WHO QUALIFIES: the vector segment, and nothing else in this codebase today --
hence the name.  Every public segment entry point takes the segment's own
rw-lock (SEGMENT-PUT/SEGMENT-REMOVE write; SEGMENT-GET/SEGMENT-SCAN/
SEGMENT-SCORE-SUBSET read), so %SEG-GROW, which runs under the write side, has
genuine exclusion over the segment's readers.

WHO DOES NOT QUALIFY -- THE HEAP (allocator.lisp) AND THE LINEAR HASH
(linear-hash.lisp).  Both call EXTEND-MAPPED-FILE, so both look like plausible
callers, and both would be CATASTROPHIC ones: the MAPPED-FILE layer has NO read
lock, and removing that lock is precisely what the stable-address design bought
(Phase 1's per-file read lock serialised every reader; Phase 3 removed it by
pinning the address).  If you are here from allocator.lisp or linear-hash.lisp,
the answer is not to relax this contract or to generalise the name -- it is that
those subsystems cannot relocate at all, and their reservation is a hard
ceiling.  See the spec's Part 4 and its \"Out of scope\".

On failure (either mmap) nothing is mutated: the old window is still mapped and
M-POINTER still points at it, so the caller may signal and the segment remains
usable at its current reservation."
  (let ((len (mapped-file-length mapped-file))
        (old-base (m-pointer mapped-file))
        (old-reserved (m-reserved-size mapped-file)))
    (let ((reserved (max new-reservation len))
          (new-base nil)
          (ok nil))
      (when (<= reserved old-reserved)
        (return-from relocate-vector-segment-mapping mapped-file))
      (unwind-protect
           (progn
             ;; Reserve the new window first.  If this fails (VA exhaustion,
             ;; RLIMIT_AS), the old mapping is untouched.
             (setq new-base (%posix-mmap
                             (cffi:null-pointer)
                             reserved
                             +prot-none+
                             (logior +map-private+
                                     +map-anonymous+
                                     +map-noreserve+)
                             -1
                             0))
             ;; Map the file over the head of the new window.  MAP_SHARED, so
             ;; while both windows are live they are views of the same pages --
             ;; there is no copy and no coherency window.
             (let ((pointer (%posix-mmap
                             new-base
                             len
                             (logior +prot-read+ +prot-write+)
                             (logior +map-shared+ +map-fixed+)
                             (m-fd mapped-file)
                             0)))
               ;; Publish, then release.  In this order: a stray reader that
               ;; sneaks past the caller's lock is far better off reading the
               ;; still-mapped old window than faulting on a freed one.
               (setf (m-pointer mapped-file) pointer
                     (m-reserved-size mapped-file) reserved
                     ok t)
               (incf *segment-relocations*)))
        (unless ok
          ;; Roll the reservation back; leave M-POINTER as it was.
          (when new-base
            (%munmap-or-warn new-base reserved "rolled-back new reservation"))))
      (%munmap-or-warn old-base old-reserved "released old segment window")
      mapped-file)))

(defmethod serialize-uint64 ((mf mapped-file) int offset)
  (declare (type word int offset))
  ;;(log:debug "MMAP: SERIALIZING UINT64 ~A TO ADDR ~A" int offset)
  (dotimes (i 8)
    (%set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  offset)

(defmethod deserialize-uint64 ((mf mapped-file) offset)
  "Decode a UINT64."
  (declare (type word offset))
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (%get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod deserialize-uint64 ((array array) offset)
  "Decode a UINT64."
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (aref array (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod incf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (incf int)
    (serialize-uint64 mf int offset)
    int))

(defmethod decf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (serialize-uint64 mf (decf int) offset)
    int))

(defmethod serialize-pointer ((mf mapped-file) pointer offset)
  ;;(log:debug "SERIALIZING POINTER ~A TO ADDR ~A" pointer offset)
  (serialize-uint64 mf pointer offset))

(defmethod deserialize-pointer ((mf mapped-file) offset)
  ;;(log:debug "DESERIALIZING POINTER AT ADDR ~A" offset)
  (deserialize-uint64 mf offset))

(defmethod serialize-uint32 ((mf mapped-file) int offset)
  (declare (type uint32 int))
  (declare (type word offset))
  (dotimes (i 4)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  offset)

(defmethod deserialize-uint32 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint32 int))
    (dotimes (i 4)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod serialize-uint40 ((mf mapped-file) int offset)
  (declare (type uint40 int))
  (declare (type word offset))
  (dotimes (i 5)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  offset)

(defmethod deserialize-uint40 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint40 int))
    (dotimes (i 5)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))
