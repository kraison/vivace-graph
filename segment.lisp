(in-package :graph-db)

;;; Vector segment: a derived, mmap-backed index holding one fixed-width
;;; single-float vector per node, addressable by node id.  See
;;; docs/superpowers/specs/2026-07-20-vector-segments-design.md sec 5.
;;;
;;; This file is the FILE FORMAT and its unit operations ONLY.  Transaction
;;; hooks, rebuild-from-nodes, and scan/score are later steps.
;;;
;;; The on-disk id array is authoritative.  ID->SLOT is a RAM-only hash rebuilt
;;; at open by sweeping it (sec 5.1); it is never persisted.

(defstruct (vector-segment (:constructor %make-vector-segment)
                           (:conc-name segment-)
                           (:predicate vector-segment-p))
  (mmap nil)                 ; a mapped-file (mmap.lisp)
  (dimension 0 :type fixnum) ; fixed at create time
  (id->slot nil)             ; equalp hash: 16-byte id vector -> slot index
  (clean-at-open nil)        ; the on-disk clean flag as it was when this segment opened
  ;; Per-segment reader/writer lock.  All PUBLIC mutations (segment-put,
  ;; segment-remove) take the write side; public reads (segment-get,
  ;; segment-scan, segment-score-subset) take the read side.  Never persisted --
  ;; created fresh by create/open.
  ;;
  ;; NON-RECURSIVE: lock at PUBLIC boundaries only.  The %SEG-* internals are
  ;; lock-free and assume the caller holds the lock -- segment-put ->
  ;; %seg-claim-slot -> %seg-grow nests, so locking inside %seg-grow would
  ;; self-deadlock.  Same idiom as the skip list.
  ;;
  ;; LOCK ORDER: the write side is only ever taken INSIDE the transaction
  ;; manager lock (mutations run on the apply path); the read side is taken
  ;; alone.  Never take the manager lock while holding a segment lock.
  (lock (make-rw-lock)))

(defun %seg-write-header (mmap &key magic format dimension element-type
                                    capacity live-count free-head)
  (serialize-uint64 mmap magic 0)
  (serialize-uint64 mmap format 8)
  (serialize-uint64 mmap dimension 16)
  (serialize-uint64 mmap element-type 24)
  (serialize-uint64 mmap capacity 32)
  (serialize-uint64 mmap live-count 40)
  (serialize-uint64 mmap free-head 48)
  (serialize-uint64 mmap 0 56))

(defun %seg-vblock-offset (capacity)
  "Byte offset of the vector block for CAPACITY slots."
  (+ +segment-id-array-offset+ (* capacity +key-bytes+)))

(defun %seg-file-bytes (capacity dimension)
  "Total bytes a segment file needs for CAPACITY slots of DIMENSION."
  (+ (%seg-vblock-offset capacity) (* capacity dimension 4)))

(defun create-vector-segment (path dimension &key (initial-capacity 1024))
  "Create a new vector segment at PATH holding DIMENSION-wide single-float
vectors, with room for INITIAL-CAPACITY slots.  DIMENSION is fixed for the life
of the segment.  Returns an open VECTOR-SEGMENT."
  (check-type dimension (integer 1))
  (check-type initial-capacity (integer 1))
  (let* ((bytes (%seg-file-bytes initial-capacity dimension))
         (mmap (mmap-file path :create-p t :size bytes)))
    (%seg-write-header mmap
                       :magic +segment-magic+
                       :format +segment-format+
                       :dimension dimension
                       :element-type +fv-single-float+
                       :capacity initial-capacity
                       :live-count 0
                       :free-head +no-slot+)
    ;; A freshly created (or freshly extended-by-mmap-file) region is zero-
    ;; filled, not free-marked: without this, the never-written tail
    ;; [0, initial-capacity) reads as an all-zero id at reopen-time sweep
    ;; (a real bug caught before this shipped -- see %seg-mark-free-range).
    ;; Marking it here makes "never written" and "removed" the same on-disk
    ;; state, which the sweep already knows how to skip.
    (%seg-mark-free-range mmap 0 initial-capacity)
    (%make-vector-segment :mmap mmap
                          :dimension dimension
                          :id->slot (make-hash-table :test 'equalp))))

(defun segment-capacity (segment)
  (deserialize-uint64 (segment-mmap segment) 32))

(defun segment-live-count (segment)
  (deserialize-uint64 (segment-mmap segment) 40))

(defun %seg-free-head (segment)
  (deserialize-uint64 (segment-mmap segment) 48))

(defun open-vector-segment (path)
  "Open an existing vector segment at PATH.  Validates magic, format, and
element-type, reads the header, and rebuilds the RAM id->slot map by sweeping
the id array (the on-disk id array is authoritative; the map is never
persisted)."
  (let ((mmap (mmap-file path :create-p nil)))
    (let ((magic (deserialize-uint64 mmap 0))
          (format (deserialize-uint64 mmap 8)))
      (unless (= magic +segment-magic+)
        (error "~A is not a vector segment (magic ~X)" path magic))
      (unless (= format +segment-format+)
        (error "vector segment ~A is format ~D, expected ~D"
               path format +segment-format+)))
    ;; Only single-float is ever written today, and every read path
    ;; (%seg-read-vector / %seg-write-vector) hard-assumes it; validate the
    ;; on-disk element-type so a future double-float/int8 segment (the whole
    ;; reason this header field exists) can't be silently misread as
    ;; single-float instead of signaling.
    (let ((etype (deserialize-uint64 mmap 24)))
      (unless (= etype +fv-single-float+)
        (error "vector segment ~A has element-type ~D, expected ~D (only ~
                single-float is supported)"
               path etype +fv-single-float+)))
    (let ((segment (%make-vector-segment
                    :mmap mmap
                    :dimension (deserialize-uint64 mmap 16)
                    :id->slot (make-hash-table :test 'equalp))))
      (%seg-rebuild-id->slot segment)
      ;; Capture the persisted clean flag (the recovery decision reads THIS), then
      ;; mark the file dirty for the new session.
      (let ((clean (= (deserialize-uint64 mmap +segment-clean-offset+) +segment-clean+)))
        (setf (segment-clean-at-open segment) clean)
        (serialize-uint64 mmap +segment-dirty+ +segment-clean-offset+)
        ;; Force the dirty flag to disk NOW, so a crash after this open reliably
        ;; leaves the segment marked dirty -> rebuild on next open.  Without this
        ;; the flip-to-dirty is an unsynced mmap store whose writeback timing is
        ;; the kernel's choice, so a hard crash could leave the on-disk flag still
        ;; reading clean, and an unsanctioned recovery (one that deletes .dirty and
        ;; reopens rather than snapshot/replay) would then TRUST a stale segment.
        ;; Defense-in-depth: the graph's .dirty marker is the primary crash guard
        ;; (open-graph refuses a crashed graph); this closes the residual window.
        ;; One msync per segment at open only -- open is not a hot path.
        (sync-region mmap :length +segment-header-bytes+))
      segment)))

(defun segment-clean-shutdown-p (segment)
  "True if the segment's on-disk state at open time was cleanly closed."
  (segment-clean-at-open segment))

(defun close-vector-segment (segment)
  "Release the segment's mmap."
  (when (segment-mmap segment)
    (serialize-uint64 (segment-mmap segment) +segment-clean+ +segment-clean-offset+)
    (munmap-file (segment-mmap segment))
    (setf (segment-mmap segment) nil))
  nil)

(defun %seg-rebuild-id->slot (segment)
  "Repopulate SEGMENT's RAM id->slot hash by sweeping the on-disk id array.
The id array is authoritative (sec 5.1).  A slot whose first 8 bytes are
+FREE-SLOT-MARKER+ is free and skipped; every other slot holds a real 16-byte id."
  (let ((mmap (segment-mmap segment))
        (cap (segment-capacity segment))
        (table (segment-id->slot segment)))
    (clrhash table)
    (dotimes (slot cap)
      (let ((first8 (deserialize-uint64 mmap (%seg-id-offset slot))))
        (unless (= first8 +free-slot-marker+)
          (let ((id (get-bytes mmap (%seg-id-offset slot) +key-bytes+)))
            (setf (gethash id table) slot)))))))

(defun %seg-id-offset (slot)
  (+ +segment-id-array-offset+ (* slot +key-bytes+)))

(defun %seg-mark-free-range (mmap start-slot end-slot)
  "Mark id-array cells [START-SLOT, END-SLOT) free: +FREE-SLOT-MARKER+ in both
8-byte halves.  The first half is what the rebuild sweep checks; the second
half doubles as a free-list \"next\" pointer of +NO-SLOT+ (same bit pattern),
so these cells are indistinguishable from a properly terminated free chain
even though they are not yet threaded onto one.  Used by CREATE-VECTOR-SEGMENT
for the initial capacity and by %SEG-GROW for newly added capacity, so a
never-written cell and a removed cell are the same on-disk state."
  (loop for slot from start-slot below end-slot
        for off = (%seg-id-offset slot)
        do (serialize-uint64 mmap +free-slot-marker+ off)
           (serialize-uint64 mmap +free-slot-marker+ (+ off 8))))

(defun %seg-vec-offset (segment slot)
  (+ (%seg-vblock-offset (segment-capacity segment))
     (* slot (segment-dimension segment) 4)))

(defun %seg-read-vector (segment slot)
  "Read slot SLOT's vector as a fresh (simple-array single-float (*))."
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (get-bytes (segment-mmap segment) off (* dim 4)))
         (v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v)
      (let ((bits 0) (b (* i 4)))
        (dotimes (k 4)
          (setf bits (dpb (aref bytes (+ b k)) (byte 8 (* k 8)) bits)))
        (setf (aref v i) (ieee-floats:decode-float32 bits))))))

(defun %seg-write-vector (segment slot vector)
  "Write VECTOR into slot SLOT's vector-block region."
  (declare (type (simple-array single-float (*)) vector))
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (make-array (* dim 4) :element-type '(unsigned-byte 8))))
    (dotimes (i dim)
      (let ((bits (ieee-floats:encode-float32 (aref vector i)))
            (b (* i 4)))
        (dotimes (k 4)
          (setf (aref bytes (+ b k)) (ldb (byte 8 (* k 8)) bits)))))
    (set-bytes (segment-mmap segment) bytes off (* dim 4))))

(defun %seg-check-id (id)
  ;; The free-list scheme marks a free cell by all-ones in its first 8 bytes, so
  ;; a real id whose first 8 bytes are all-ones would be misread as free after a
  ;; reopen (sec 5.1 rebuild).  Engine ids are uuids and never all-ones, but an
  ;; arbitrary caller-supplied id could be; reject it loudly rather than corrupt
  ;; silently.
  ;;
  ;; This MUST run before any slot is claimed (i.e. before %SEG-CLAIM-SLOT),
  ;; not inside %SEG-WRITE-ID after the fact: %seg-claim-slot can pop a slot
  ;; off the free list as a side effect, and if the id were only validated
  ;; afterward, a rejected put would still have popped and orphaned that slot.
  ;; SEGMENT-PUT calls this first, before claiming anything.
  (let ((first8 0))
    (dotimes (k 8) (setf first8 (dpb (aref id k) (byte 8 (* k 8)) first8)))
    (when (= first8 +free-slot-marker+)
      (error "node id's first 8 bytes are all-ones, colliding with the segment ~
              free-slot marker"))))

(defun %seg-write-id (segment slot id)
  "Write ID into slot SLOT's id-array cell.  Caller must have already
validated ID via %SEG-CHECK-ID."
  (set-bytes (segment-mmap segment) id (%seg-id-offset slot) +key-bytes+))

(defun %seg-slot-of (segment id)
  "Slot index storing ID, or NIL."
  (gethash id (segment-id->slot segment)))

(defun %seg-grow (segment)
  "Double the segment's capacity in place.  Because the vector block starts
after the id array and the id array's size is capacity*16, growing capacity
moves the vector block: extend the file, then relocate the existing vectors
from the OLD block offset to the NEW one, high slot first so the copy never
overwrites unread source bytes.  The base pointer never moves (extend-mapped-
file remaps into the reserved window), so a concurrent read never faults.
Returns OLD-CAP, the first fresh (unclaimed) slot index."
  (let* ((mmap (segment-mmap segment))
         (dim (segment-dimension segment))
         (old-cap (segment-capacity segment))
         (new-cap (* 2 old-cap))
         (old-vblock (%seg-vblock-offset old-cap))
         (new-vblock (%seg-vblock-offset new-cap))
         (needed (%seg-file-bytes new-cap dim))
         (have (mapped-file-length mmap)))
    (when (> needed have)
      (extend-mapped-file mmap (- needed have)))
    ;; Relocate vectors, HIGH slot first: new-vblock > old-vblock, so copying
    ;; slot i from old+i*w to new+i*w with i descending never overwrites a
    ;; not-yet-copied source region.
    (let ((w (* dim 4)))
      (loop for i from (1- old-cap) downto 0
            for src = (+ old-vblock (* i w))
            for dst = (+ new-vblock (* i w))
            do (set-bytes mmap (get-bytes mmap src w) dst w)))
    ;; The newly added id-array cells [old-cap, new-cap) currently sit where
    ;; stale vector bytes (already relocated above) or freshly extended file
    ;; bytes live -- neither is free-marked.  Mark them, same as create does
    ;; for the initial capacity, so an untouched cell never sweeps as a
    ;; phantom id.
    (%seg-mark-free-range mmap old-cap new-cap)
    (serialize-uint64 mmap new-cap 32)         ; capacity := new-cap
    old-cap))                                  ; first fresh slot index

(defun %seg-claim-slot (segment)
  "Return a slot index to write a NEW id into: the free-list head if any, else
the next slot past live-count, growing the segment first if capacity is
exhausted."
  (let* ((mmap (segment-mmap segment))
         (free-head (%seg-free-head segment)))
    (if (/= free-head +no-slot+)
        ;; Pop the free list: its cell's second 8 bytes hold the next free slot.
        (let ((next (deserialize-uint64 mmap (+ (%seg-id-offset free-head) 8))))
          (serialize-uint64 mmap next 48)   ; free-head := next
          free-head)
        (let ((cap (segment-capacity segment))
              (live (segment-live-count segment)))
          (if (>= live cap)
              (%seg-grow segment)              ; returns old-cap = first fresh slot
              live)))))

(defun segment-put (segment id vector)
  "Store VECTOR under the 16-byte ID.  Overwrites if ID is present; else takes a
free slot (or the next free index, growing the segment if necessary).  Returns
the slot index.  VECTOR's length must equal the segment's dimension, and ID's
first 8 bytes must not collide with the free-slot marker -- both are validated
up front, before any slot is claimed, so a rejected put never disturbs the
free list.

Takes the segment's WRITE lock: mutations are exclusive against concurrent
scans.  The %SEG-* internals it calls (including %seg-grow) are lock-free and
run under this lock."
  (with-write-lock ((segment-lock segment))
    (check-type vector (simple-array single-float (*)))
    (unless (= (length vector) (segment-dimension segment))
      (error "vector length ~D does not match segment dimension ~D"
             (length vector) (segment-dimension segment)))
    (%seg-check-id id)
    (let ((existing (%seg-slot-of segment id)))
      (if existing
          (progn (%seg-write-vector segment existing vector) existing)
          (let ((slot (%seg-claim-slot segment)))
            (%seg-write-id segment slot id)
            (%seg-write-vector segment slot vector)
            (setf (gethash id (segment-id->slot segment)) slot)
            (serialize-uint64 (segment-mmap segment)
                              (1+ (segment-live-count segment)) 40)
            slot)))))

(defun segment-get (segment id)
  "The vector stored under ID as a fresh (simple-array single-float (*)), or NIL.

Takes the segment's READ lock: shared against concurrent reads, exclusive
against a concurrent segment-put/segment-remove."
  (with-read-lock ((segment-lock segment))
    (let ((slot (%seg-slot-of segment id)))
      (when slot (%seg-read-vector segment slot)))))

(defun rebuild-vector-segment (graph owner-name slot-name)
  "Rebuild the (OWNER-NAME, SLOT-NAME) segment from live nodes: drop any current
segment/file, create a fresh one sized to the first conforming vector, and
segment-put every live node's conforming value.  Registers and returns the fresh
segment.  Run when quiescent (at open, before writes) -- it mutates outside the
transaction path, like rebuild-spatial-index.

OWNER-NAME must be the segment's OWNER -- the declaring class returned by
%VECTOR-INDEX-SLOT-OWNER-NAME / %SEGMENT-KEY (transactions.lisp), not
necessarily a node's exact runtime class.  One segment per owner spans its
subclasses (the engine's :UNIQUE / :INDEX convention), so this sweeps
MAP-VERTICES with its default :INCLUDE-SUBCLASSES-P T: every subclass
instance's vector is swept into the OWNER's segment, matching exactly what the
live apply path (APPLY-TX-WRITE-TO-VECTOR-SEGMENTS, via %SEGMENT-KEY) does on
create/update/delete."
  (let* ((key (cons owner-name slot-name))
         (table (vector-segments graph))
         (path (%segment-file graph owner-name slot-name)))
    (let ((old (gethash key table)))
      (when old (close-vector-segment old)))
    (remhash key table)
    (ignore-errors (delete-file path))
    (let ((seg nil))
      (map-vertices
       (lambda (node)
         (unless (deleted-p node)
           (let ((v (%node-segment-value node slot-name)))
             (when v
               (unless seg
                 (setf seg (create-vector-segment path (length v)))
                 (setf (gethash key table) seg))
               (segment-put seg (id node) v)))))
       graph :vertex-type owner-name)
      seg)))

(defun %id-less-p (a b)
  "Lexicographic order over two 16-byte node ids.  The engine has
UUID-ARRAY-EQUAL but no less-than, so this is it: first differing byte wins."
  (declare (type (array (unsigned-byte 8) (*)) a b))
  (dotimes (i +key-bytes+ nil)
    (let ((x (aref a i)) (y (aref b i)))
      (cond ((< x y) (return t))
            ((> x y) (return nil))))))

(defun %score-before-p (s1 id1 s2 id2)
  "The segment ranking order: score DESCENDING, node-id ASCENDING on a tie."
  (declare (type single-float s1 s2))
  (cond ((> s1 s2) t)
        ((< s1 s2) nil)
        (t (%id-less-p id1 id2))))

;;; Bounded top-k collector.  Never materialises one result per candidate: a
;;; scan offers every occupied slot and only k are retained.  k is small, so a
;;; linear scan of the k-element buffer beats a heap's bookkeeping.
;;;
;;; The tiebreak is carried through EVICTION, not applied only at the end.
;;; Eviction happens during iteration, so a score-only comparison at the k-th
;;; boundary would make the result depend on slot order -- which is meaningless
;;; under free-list reuse, and would make ranking differ between an incrementally
;;; built segment and a rebuilt one.
(defstruct (topk (:constructor %make-topk-raw))
  (k 0 :type fixnum)
  (count 0 :type fixnum)
  (scores nil :type (or null (simple-array single-float (*))))
  (ids nil :type (or null simple-vector)))

(defun %make-topk (k)
  (%make-topk-raw :k k
                  :scores (make-array (max k 1) :element-type 'single-float)
                  :ids (make-array (max k 1) :initial-element nil)))

(defun %topk-worst-index (c)
  "Index of the entry that ranks LAST under %SCORE-BEFORE-P."
  (let ((scores (topk-scores c)) (ids (topk-ids c)) (worst 0))
    (declare (type (simple-array single-float (*)) scores))
    (dotimes (i (topk-count c) worst)
      (when (%score-before-p (aref scores worst) (aref ids worst)
                             (aref scores i) (aref ids i))
        (setf worst i)))))

(defun %topk-offer (c score id)
  "Offer SCORE/ID; keep it only if it outranks the current worst."
  (declare (type single-float score))
  (when (plusp (topk-k c))
    (let ((scores (topk-scores c)) (ids (topk-ids c)))
      (cond ((< (topk-count c) (topk-k c))
             (setf (aref scores (topk-count c)) score
                   (aref ids (topk-count c)) id)
             (incf (topk-count c)))
            (t
             (let ((worst (%topk-worst-index c)))
               (when (%score-before-p score id (aref scores worst) (aref ids worst))
                 (setf (aref scores worst) score
                       (aref ids worst) id)))))))
  c)

(defun %topk-results (c)
  "Retained entries as (score . id) conses, best first."
  (let ((out '()))
    (dotimes (i (topk-count c))
      (push (list (aref (topk-scores c) i) (aref (topk-ids c) i)) out))
    (mapcar (lambda (row) (cons (first row) (second row)))
            (sort out (lambda (a b)
                        (%score-before-p (first a) (second a)
                                         (first b) (second b)))))))

(defun segment-remove (segment id)
  "Remove ID from the segment, pushing its slot onto the free list.  Returns T
if ID was present, NIL otherwise.  A freed slot's id-array cell is marked with
+FREE-SLOT-MARKER+ (first 8 bytes) and the previous free-head (second 8 bytes),
threading the free list; its vector-block bytes are left as-is (unreachable).

Takes the segment's WRITE lock: mutations are exclusive against concurrent
scans."
  (with-write-lock ((segment-lock segment))
    (let ((slot (%seg-slot-of segment id)))
      (if (null slot)
          nil
          (let ((mmap (segment-mmap segment))
                (old-head (%seg-free-head segment)))
            (serialize-uint64 mmap +free-slot-marker+ (%seg-id-offset slot))
            (serialize-uint64 mmap old-head (+ (%seg-id-offset slot) 8))
            (serialize-uint64 mmap slot 48)      ; free-head := slot
            (remhash id (segment-id->slot segment))
            (serialize-uint64 mmap (1- (segment-live-count segment)) 40)
            t)))))
