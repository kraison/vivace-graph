(in-package :graph-db)

;;; ECL threading-primitive capability gate.
;;;
;;; The custom rw-lock (rw-lock.lisp) uses a `(sleep 0.001)` busy-poll on ECL
;;; instead of condition-variable blocking, a workaround for ECL 21.2.1 bugs
;;; (mp:wait-on-semaphore blocking indefinitely, condition-variable-broadcast
;;; missing waiters, condition-variable-timedwait unreliable before 23.09.09).
;;; Those are fixed in modern ECL, where the poll's ~1 ms/handoff floor is pure
;;; overhead.  Push :GRAPH-DB-ECL-MODERN-MP when running ECL >= 23.9.9 so the
;;; rw-lock can take the blocking path; older ECL keeps the safe poll fallback.
;;;
;;; eval-when so the feature is set before rw-lock.lisp is read/compiled (it
;;; loads after globals per graph-db.asd).  Default-to-safe: any parse failure or
;;; older version leaves the feature absent (poll path).  Validate on the target
;;; ECL: a wrong gate would reintroduce the 21.2.1 hangs.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((parts (mapcar (lambda (s) (or (parse-integer s :junk-allowed t) 0))
                       (uiop:split-string (lisp-implementation-version)
                                          :separator "."))))
    (destructuring-bind (&optional (major 0) (minor 0) (patch 0) &rest rest) parts
      (declare (ignore rest))
      (when (or (> major 23)
                (and (= major 23) (or (> minor 9)
                                      (and (= minor 9) (>= patch 9)))))
        (pushnew :graph-db-ecl-modern-mp *features*)))))

;;; ECL synchronized-hash-table capability gate (GH #101).
;;;
;;; SBCL and CCL mark every concurrently-accessed table :SYNCHRONIZED / :SHARED.
;;; ECL arms were written without an equivalent because ECL once had none -- see
;;; the note in skip-list.lisp, which found rehash races and SIGSEGVs and guarded
;;; that table with an explicit lock instead.  Modern ECL does support it:
;;; measured on 26.5.5, eight threads x 20k inserts over a deliberately
;;; undersized table complete losslessly with :SYNCHRONIZED T and LIVELOCK
;;; without it.
;;;
;;; PROBED, not version-gated: the version at which ECL gained this is not
;;; documented anywhere we can check, and the oldest supported ECL (21.2.1) is
;;; not installed on any host here, so a threshold would be a guess.  Constructing
;;; a table is the direct question.  Default-to-safe: if the keyword is rejected
;;; the feature stays absent and those tables are exactly as they are today.
;;;
;;; LIMITATION: this proves the keyword is ACCEPTED, not that it is HONOURED.  An
;;; ECL that accepted and ignored it would probe true and stay racy -- no worse
;;; than today, but not fixed either.  26.5.5 is verified honoured by the test
;;; above; re-run it when raising the floor to a version between 21.2.1 and 26.5.5.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (ignore-errors (hash-table-p (make-hash-table :test 'eql :synchronized t)))
    (pushnew :graph-db-ecl-sync-hash *features*)))

(defvar *cache-enabled* t)

(defparameter *index-backend* :skip-list
  "DEFAULT ordered-map backend for a NEW graph's heap-backed indexes -- views,
:unique, and the spatial index: :SKIP-LIST (the original; default) or :BPLUS-TREE
(mmap B+ tree -- better cold-cache locality, faster reads AND writes, ~2x smaller;
see docs/bplus-tree-experiment.md).  Both speak the same ordered-map protocol over
the same (payload . node-id) composite keys, so an index behaves identically on
either.  This is only the DEFAULT: MAKE-GRAPH / OPEN-GRAPH take an :INDEX-BACKEND
keyword to choose PER GRAPH (captured in the graph's INDEX-BACKEND slot, which every
index-creation path consults).  The choice is persisted per index (views: the
view-alist :BACKEND key; unique + spatial: their root sidecar), so reopening a graph
uses each index's own written backend -- flipping this never disturbs an existing
graph (a missing tag => :skip-list).  Set this (or pass :INDEX-BACKEND) from your
application's own config; graph-db does not read an ini file itself.")

(alexandria:define-constant +db-version+ 1)

(defvar *graph* nil)
(alexandria:define-constant +main-table-file+ "main.dat" :test 'equal)
(alexandria:define-constant +meta-file+ "meta.dat" :test 'equal)
(alexandria:define-constant +data-file+ "data.dat" :test 'equal)

(defvar *schema-node-metadata* (make-hash-table :test 'equal))
(alexandria:define-constant +max-node-types+ 65536)

;; v2 (2026): MVCC node head grew 15 -> 31 bytes (commit-epoch + prev-pointer).
;; Old (v1) graphs must be migrated via MIGRATE-GRAPH (snapshot + replay).
(alexandria:define-constant +storage-version+     #x02)
(alexandria:define-constant +fixed-integer-64+    #x01)
(alexandria:define-constant +data-magic-byte+     #x17)
(alexandria:define-constant +lhash-magic-byte+    #x18)
(alexandria:define-constant +overflow-magic-byte+ #x19)
(alexandria:define-constant +config-magic-byte+   #x20)
(alexandria:define-constant +null-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 0)
   :test 'equalp)
(alexandria:define-constant +max-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 255)
   :test 'equalp)
(alexandria:define-constant +key-bytes+ 16)
(alexandria:define-constant +value-bytes+ 8)
(alexandria:define-constant +bucket-size+ 24)
(alexandria:define-constant +data-extent-size+ (* 1024 1024 100))

;; Initial sizes (in bytes) of the two memory-mapped allocator regions a graph
;; creates: HEAP (node/edge data) and INDEXES.  Both grow on demand via
;; extend-mapped-file, so these are just starting sizes; tune them per workload
;; via MAKE-GRAPH's :heap-size / :index-size, or by rebinding these defaults.
(defparameter *default-heap-size* (* 1024 1024 1000)
  "Initial size, in bytes, of a new graph's heap (node/edge data) region.")
(defparameter *default-index-size* (* 1024 1024 1000)
  "Initial size, in bytes, of a new graph's indexes region.")

;; Each memory-mapped file reserves a virtual-address window up front (PROT_NONE,
;; MAP_NORESERVE — address space only, no committed memory) and maps the file
;; into the head of it.  Growth re-maps more of the file into the reserved window
;; with MAP_FIXED, so the base pointer never moves and concurrent readers never
;; fault or need a lock.  A file may grow up to its reservation; exceeding it
;; signals an error.  The reservation is proportional to the file's initial size
;; (with a floor) rather than a flat huge value: a graph has ~15-20 mapped files,
;; so a flat multi-GB reservation each would reserve enormous VA per graph (which
;; can fail on macOS).  See mmap.lisp.
(defparameter *mmap-reservation-multiplier* 8
  "Growth headroom: a mapped file reserves this multiple of its initial size.")
(defparameter *mmap-min-reservation* (* 1024 1024 1024)
  "Floor, in bytes, for a mapped file's virtual-address reservation.")

;; Vector segments need their own, much larger floor.  The general 8x rule above
;; was sized for the files it was written for: heap and index files, whose size
;; is set by the schema and the workload and which a graph has ~15-20 of.  A
;; vector segment is the first mapped file whose size tracks the CORPUS, so it
;; reaches 8x of whatever it happened to be at open far sooner -- and when it
;; does, the grow fails from inside APPLY-TRANSACTION.  There is at most one
;; segment per (vertex-type, slot), not 15-20 of them.
;;
;; The floor costs nothing real to RAM, disk, or Linux commit charge: a
;; reservation is PROT_NONE + MAP_NORESERVE anonymous address space, and on
;; 64-bit the address space it consumes is irrelevant.  Exception: RLIMIT_AS /
;; `ulimit -v` counts reserved address space regardless of MAP_NORESERVE, so a
;; process capped by one (e.g. a systemd unit's LimitAS=, as on odm) can fail
;; to open a graph outright even though nothing here is actually resident.
;;
;; At dimension 1024 (4,112 bytes per slot), capacity only ever advances by
;; doubling (%SEG-GROW) from CREATE-VECTOR-SEGMENT's 1024 default, so real
;; capacities are powers of two -- NOT the byte-exact 16 GiB / 4,112 =
;; 4,177,983.  The largest power-of-two capacity whose file still fits under a
;; 16 GiB floor is 2,097,152 (2^21, ~8.03 GiB of file): the next doubling,
;; 4,194,304 (2^22), needs 17,246,978,112 bytes, over the 17,179,869,184-byte
;; (16 GiB) floor.  A capacity-planning estimate must use the power-of-two
;; number, not the byte-exact one.
(defparameter *segment-min-reservation* (* 16 1024 1024 1024)
  "Floor, in bytes, for a VECTOR SEGMENT's virtual-address reservation.
Overrides *MMAP-MIN-RESERVATION* for segment files only; the multiplier still
applies, so a segment already larger than this floor divided by
*MMAP-RESERVATION-MULTIPLIER* still gets proportional headroom.")

;; The CHEAP way out of exhaustion, tried before relocation: claim the address
;; range immediately AFTER the segment's window (EXTEND-RESERVATION-IN-PLACE,
;; mmap.lisp) so the window simply grows.  M-POINTER never moves, nothing is
;; remapped, and no reader is disturbed.  When it cannot,
;; *SEGMENT-RELOCATE-ON-EXHAUSTION* below takes over.
;;
;; DO NOT EXPECT THIS TO FIRE OFTEN.  The design assumed a sparse 64-bit address
;; space means the adjacent range is usually free.  Measured, it usually is NOT:
;; Linux's default top-down mmap allocator places a mmap(NULL, ...) window flush
;; against the bottom of the existing mappings, so the range immediately ABOVE a
;; freshly created window is occupied by construction -- on both test hosts a
;; 16 GiB reservation ended exactly where libssl.so.3 begins, and claims of one
;; page through 8 GiB were all refused.  Darwin behaves the same way, and so does
;; Linux's legacy bottom-up layout.  It succeeds only where the window happens to
;; sit below a hole.  Keep it because a miss costs one mmap on an already-rare
;; path; do not plan capacity around it.  The lever that actually keeps a segment
;; from relocating is *SEGMENT-MIN-RESERVATION* above -- reserve more up front.
;;
;; Two reasons this is a knob rather than unconditional, exactly mirroring the
;; relocation switch below:
;;   1. an operator kill-switch, if claiming adjacent address space ever
;;      misbehaves on some platform: with this NIL the behaviour is precisely
;;      wave 2's -- straight to relocation;
;;   2. it is the only way left to exercise the RELOCATION path in a test.
;;      Once the adjacent claim usually succeeds, every pre-existing relocation
;;      test would quietly stop testing relocation and start testing this
;;      instead -- while still passing green.  The relocation tests therefore
;;      bind this to NIL, deliberately and visibly.
(defparameter *segment-extend-adjacent-on-exhaustion* t
  "When true (the default), a vector segment whose growth would exceed its
virtual-address reservation first tries to claim the range immediately after
its current window, growing the reservation IN PLACE without moving the
mapping.  When NIL, or when the range is not free, it falls back to
*SEGMENT-RELOCATE-ON-EXHAUSTION*.  Binding this to NIL by itself does not force
a hard abort on exhaustion -- *SEGMENT-RELOCATE-ON-EXHAUSTION* is still T by
default and will grow the segment by relocating it.  Both must be NIL to
disable growth past the reservation entirely.")

;; The floor above makes exhaustion rare; this makes it recoverable.  When a
;; segment's growth would pass its reservation, %SEG-GROW re-reserves a larger
;; window and RELOCATES the mapping into it (RELOCATE-VECTOR-SEGMENT-MAPPING,
;; mmap.lisp) instead of signalling.  That moves M-POINTER, which is only ever
;; safe for a subsystem that can exclude its own readers -- the segment can
;; (every public entry point takes its rw-lock), the heap and linear hash
;; cannot.  See docs/mmap-remap-race-plan.md Phase 3.
;;
;; Two reasons this is a knob rather than unconditional:
;;   1. an operator kill-switch: if relocation ever misbehaves on some platform,
;;      binding this to NIL used to restore the previous behaviour exactly --
;;      a clean PRE-DURABILITY abort with VECTOR-SEGMENT-CAPACITY-EXHAUSTED.
;;      Since wave 3 that is no longer true of THIS knob alone:
;;      *SEGMENT-EXTEND-ADJACENT-ON-EXHAUSTION* (above) runs FIRST, and left at
;;      its default T it can still grow the segment in place without ever
;;      reaching this knob.  Getting the strictly-safe abort back requires
;;      BOTH knobs bound to NIL;
;;   2. it is the only way left to exercise that abort path in a test.  With
;;      relocation enabled, exhaustion no longer happens, so the regression test
;;      that proves a capacity failure never leaves a persisted node without a
;;      segment entry would otherwise silently stop testing anything.
(defparameter *segment-relocate-on-exhaustion* t
  "When true (the default), a vector segment whose growth would exceed its
virtual-address reservation re-reserves a larger window and relocates its
mapping into it, under the segment's own write lock.  When NIL, such a grow
signals VECTOR-SEGMENT-CAPACITY-EXHAUSTED instead -- pre-durability on the
transaction path, so the transaction rolls back cleanly -- PROVIDED
*SEGMENT-EXTEND-ADJACENT-ON-EXHAUSTION* is ALSO NIL.  Left at its default T,
the adjacent claim it controls runs first and can still grow the segment in
place, bypassing this knob entirely; only binding BOTH to NIL restores the
strictly-safe pre-durability abort.")

;; Key namespaces
(defvar *vertex-namespace* (uuid:uuid-to-byte-array
                            (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB")))
(defvar *edge-namespace* (uuid:uuid-to-byte-array
                          (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A")))

;; Sentinel values for skip lists
(alexandria:define-constant +min-sentinel+ :gmin)
(alexandria:define-constant +max-sentinel+ :gmax)
;; For views, aggregrate key symbol
(alexandria:define-constant +reduce-master-key+ :gagg)

;; index-lists
(alexandria:define-constant +index-list-bytes+ 17)

;; ve-key / ve-index
(alexandria:define-constant +ve-key-bytes+ 18)
(alexandria:define-constant +null-ve-key+
    (make-array +ve-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-ve-key+
    (make-array +ve-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
  :test 'equalp)

;; vev-key / vev-index
(alexandria:define-constant +vev-key-bytes+ 34)
(alexandria:define-constant +null-vev-key+
    (make-array +vev-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-vev-key+
    (make-array +vev-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
   :test 'equalp)

;; Type bytes for serialization
(alexandria:define-constant +needs-lookup+ :needs-lookup)
(alexandria:define-constant +unknown+ 0)
(alexandria:define-constant +negative-integer+ 1)
(alexandria:define-constant +positive-integer+ 2)
(alexandria:define-constant +character+ 3)
(alexandria:define-constant +symbol+ 4)
(alexandria:define-constant +string+ 5)
(alexandria:define-constant +list+ 6)
(alexandria:define-constant +vector+ 7)
(alexandria:define-constant +single-float+ 8)
(alexandria:define-constant +double-float+ 9)
(alexandria:define-constant +ratio+ 10)
(alexandria:define-constant +t+ 11)
(alexandria:define-constant +null+ 12)
(alexandria:define-constant +blob+ 13) ;; Uninterpreted octets
(alexandria:define-constant +dotted-list+ 14)
(alexandria:define-constant +keyword+ 15)
(alexandria:define-constant +slot-key+ 16)
(alexandria:define-constant +id+ 17)
(alexandria:define-constant +vertex+ 18)
(alexandria:define-constant +edge+ 19)
(alexandria:define-constant +skip-list+ 20)
(alexandria:define-constant +ve-index+ 21)
(alexandria:define-constant +type-index+ 22)
(alexandria:define-constant +pcons+ 23)
(alexandria:define-constant +pqueue+ 24)
(alexandria:define-constant +mpointer+ 25)
(alexandria:define-constant +pcell+ 26)
(alexandria:define-constant +index-list+ 27)
(alexandria:define-constant +vev-index+ 28)
(alexandria:define-constant +bit-vector+ 29)
(alexandria:define-constant +bignum+ 30)
(alexandria:define-constant +float-vector+ 31)

;; Element type codes for a +float-vector+ payload's first byte.  The byte exists
;; so double-float and int8-quantised vectors can be added later without burning
;; another type tag.
(alexandria:define-constant +fv-single-float+ 1)
(alexandria:define-constant +fv-double-float+ 2)


;; --- Vector segment (Phase 2) on-disk layout ---------------------------------
;; A segment is a derived, mmap-backed index: one fixed-width single-float vector
;; per node, addressable by node id.  See docs/superpowers/specs/
;; 2026-07-20-vector-segments-design.md sec 5.
(alexandria:define-constant +segment-magic+ #x5647534547 ) ; "VGSEG" as bytes
(alexandria:define-constant +segment-format+ 1)
(alexandria:define-constant +segment-header-bytes+ 64)
(alexandria:define-constant +segment-id-array-offset+ 64)
;; A free slot's id-array cell holds this marker in its first 8 bytes; its second
;; 8 bytes hold the next free slot index.  Occupied cells hold a real 16-byte id,
;; whose first 8 bytes are never this value (ids are uuids; see note in
;; segment.lisp on why the marker is safe).
(alexandria:define-constant +free-slot-marker+ #xFFFFFFFFFFFFFFFF)
;; Sentinel "no slot" index -- terminates the free list and marks "id not found".
(alexandria:define-constant +no-slot+ #xFFFFFFFFFFFFFFFF)
;; The header's reserved uint64 (offset 56) doubles as a clean-shutdown flag:
;; +segment-clean+ means the file was closed cleanly and can be trusted as-is;
;; +segment-dirty+ means it is in use or was not closed (crash) and recovery
;; (later step) should rebuild from nodes.  Consulted only at open, never at
;; create.  No format-version bump: old segment files read this as 0 (dirty),
;; which correctly forces one rebuild.
(alexandria:define-constant +segment-clean-offset+ 56)
(alexandria:define-constant +segment-clean+ 1)
(alexandria:define-constant +segment-dirty+ 0)

;; User-defined type identifiers for serializing. Start at 100
(alexandria:define-constant +uuid+ 100)
(alexandria:define-constant +timestamp+ 101)
(alexandria:define-constant +geometry+ 102) ;; spatial extension (see geometry.lisp)

;; GEOS availability flags.  These are inert in core graph-db (no FFI, no libgeos
;; dependency).  The OPTIONAL `graph-db/geos' add-on system flips them at load
;; time when it successfully binds libgeos_c; the spatial refine seam
;; (geometry-ops.lisp) consults them to decide between exact GEOS topology and
;; the dependency-free fallbacks.  Core stays libgeos-free.
(defvar *geos-available-p* nil
  "True once the graph-db/geos add-on has loaded libgeos_c successfully.")
(defvar *geos-version* nil
  "GEOS C library version as a list (major minor patch), or NIL if unloaded.")
(defvar *geos-makevalid-available-p* nil
  "True when the loaded GEOS is new enough (>= 3.8) for GEOSMakeValid_r.")

(defparameter *initial-extents* 10)
(defparameter *max-locks* 10000)

(defvar *graph-hash* nil)

;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *functor* nil "The Prolog functor currently being compiled.")
(defvar *select-list* nil "Accumulator for prolog selects.")
(defvar *cont* nil "Continuation container for step-wise queries.")

#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))

#+lispworks
(defvar *prolog-global-functors* (make-hash-table :single-thread nil))
#+lispworks
(defvar *user-functors* (make-hash-table :single-thread nil :test 'eql))

#+ccl
(defvar *prolog-global-functors* (make-hash-table :shared t))
#+ccl
(defvar *user-functors* (make-hash-table :shared t :test 'eql))

#+ecl
(defvar *prolog-global-functors*
  (make-hash-table #+graph-db-ecl-sync-hash :synchronized
                   #+graph-db-ecl-sync-hash t))
#+ecl
(defvar *user-functors*
  (make-hash-table :test 'eql
                   #+graph-db-ecl-sync-hash :synchronized
                   #+graph-db-ecl-sync-hash t))

(defparameter *prolog-trace* nil)
(alexandria:define-constant +unbound+ :unbound)
(alexandria:define-constant +no-bindings+ '((t . t)) :test 'equalp)
(alexandria:define-constant +fail+ nil)
