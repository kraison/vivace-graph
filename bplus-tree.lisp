(in-package :graph-db)

;;;; mmap-backed B+ tree -- an EXPERIMENTAL third ordered-map backend.
;;;;
;;;; Motivation is disk/mmap LOCALITY, not correctness (see
;;;; docs/next-work-handoff.md).  A skip-list lookup is ~log2(n) pointer hops to
;;;; randomly-located heap nodes -- each hop a potential page fault.  A B+ tree
;;;; packs hundreds of keys per fixed-size page, so a lookup touches log_B(n)
;;;; pages (B ~ 100), and leaf-linked range scans walk sequential-ish pages that
;;;; the OS can prefetch.  This file prototypes that structure over the SAME heap
;;;; the skip list uses (allocator.lisp / mmap.lisp) and implements the SAME
;;;; ordered-map generics (add/remove/find/update-in-skip-list + make-cursor /
;;;; make-range-cursor / cursor-next), so views / :unique / spatial can consume it
;;;; unchanged.  The immediate goal is a side-by-side perf comparison with the
;;;; skip list (tests/perf/bplus-bench.lisp).
;;;;
;;;; DESIGN (experiment-grade; see the hand-off for the harder follow-ups):
;;;;  * Fixed PAGE-SIZE pages, each a single heap allocation -> contiguous in the
;;;;    mmap region.  Slotted-page layout with a sorted slot directory and
;;;;    variable-length cells, so it carries the same variable-length composite
;;;;    (user-key . node-id) keys the skip-list consumers use.
;;;;  * Read-modify-write of whole pages (correctness-first; a page is bounded, so
;;;;    this is O(1) in n).  In-place cell edits are a later optimization.
;;;;  * Lazy delete: a removed key's cell is dropped from its leaf, with no merge
;;;;    or rebalance.  This is CORRECT -- separators are only ever lower bounds, so
;;;;    every leaf stays reachable and every key still maps to the right leaf; the
;;;;    tree just gets less full.  Rebalancing/merging is deferred.
;;;;  * Concurrency: one per-tree reader/writer lock (shared reads, exclusive
;;;;    writes) -- matches how VG already reaches indexes (under the view-group /
;;;;    manager write lock; reads under a read lock).  Page-latch crabbing / COW
;;;;    for fully lock-free reads is deferred until the tree proves itself.
;;;;  * Keys are stored serialized (raw bytes, NOT cl-store -- ECL-safe) and
;;;;    compared by DESERIALIZING and calling the same comparison / key-equal
;;;;    predicates the skip list uses.  The node-id fold makes keys duplicate-free.

;;; ---------------------------------------------------------------------------
;;; On-disk constants + page layout
;;; ---------------------------------------------------------------------------

;; Local magic bytes (not in globals.lisp -- an experiment branch avoids forcing
;; a full recompile; move them to globals if this graduates).
(alexandria:define-constant +bplus-tree-magic+ #xB1
  :documentation "First byte of a B+ tree header block.")
(alexandria:define-constant +bplus-node-magic+ #xB2
  :documentation "First byte of a B+ tree page.")
(alexandria:define-constant +bplus-format-version+ 1)

(alexandria:define-constant +bplus-default-page-size+ 4096
  :documentation "Page size in bytes; a single heap allocation per page.  4096
matches a typical OS page so a page fault pulls in exactly one node.")

;; Header block (the persisted root pointer + metadata; small, allocated once).
;; Every field is a full u64 in its own 8-byte slot so the serialize-uint64
;; writes never overlap (root 2-9, count 10-17, height 18-25, page-size 26-33).
(alexandria:define-constant +bplus-header-size+ 40)
(alexandria:define-constant +bpt-h-root-offset+ 2)       ; u64
(alexandria:define-constant +bpt-h-count-offset+ 10)     ; u64
(alexandria:define-constant +bpt-h-height-offset+ 18)    ; u64
(alexandria:define-constant +bpt-h-pagesize-offset+ 26)  ; u64

;; Page header (fixed, 16 bytes), then the u16 slot directory, then cells filling
;; downward from the end of the page.
;;   [0]      magic (+bplus-node-magic+)
;;   [1]      flags: bit0 = leaf-p
;;   [2..3]   u16 cell count
;;   [4..5]   u16 free pointer (lowest byte offset used by the cell area)
;;   [6..7]   reserved
;;   [8..15]  u64 link: leaf -> next-leaf address; internal -> P0 (leftmost child)
;;   [16..]   slot directory: <count> u16 cell offsets, ascending by key
(alexandria:define-constant +bpt-p-flags-offset+ 1)
(alexandria:define-constant +bpt-p-count-offset+ 2)
(alexandria:define-constant +bpt-p-free-offset+ 4)
(alexandria:define-constant +bpt-p-link-offset+ 8)
(alexandria:define-constant +bpt-p-slots-offset+ 16)

;;; ---------------------------------------------------------------------------
;;; Little-endian scalar accessors over an in-RAM page buffer
;;; ---------------------------------------------------------------------------

(declaim (inline buf-u16 (setf buf-u16) buf-u64 (setf buf-u64)))

(defun buf-u16 (buf offset)
  (logior (aref buf offset) (ash (aref buf (+ offset 1)) 8)))

(defun (setf buf-u16) (value buf offset)
  (setf (aref buf offset) (ldb (byte 8 0) value)
        (aref buf (+ offset 1)) (ldb (byte 8 8) value))
  value)

(defun buf-u64 (buf offset)
  (let ((n 0))
    (dotimes (i 8) (setf n (dpb (aref buf (+ offset i)) (byte 8 (* i 8)) n)))
    n))

(defun (setf buf-u64) (value buf offset)
  (dotimes (i 8) (setf (aref buf (+ offset i)) (ldb (byte 8 (* i 8)) value)))
  value)

;;; ---------------------------------------------------------------------------
;;; The B+ tree object
;;; ---------------------------------------------------------------------------

(defstruct (bplus-tree
             (:predicate bplus-tree-p)
             (:conc-name %bpt-)
             (:constructor %make-bplus-tree)
             (:print-function
              (lambda (bpt stream depth)
                (declare (ignore depth))
                (format stream "#<BPLUS-TREE ~A entries, height ~A, page ~A>"
                        (%bpt-count bpt) (%bpt-height bpt) (%bpt-page-size bpt)))))
  heap
  mmap
  address                               ; header block address
  root                                  ; root page address
  (height 1 :type (unsigned-byte 16))
  (count 0 :type (unsigned-byte 64))
  (page-size +bplus-default-page-size+ :type (unsigned-byte 32))
  (comparison '<)                       ; strict less-than over deserialized keys
  (key-equal '=)
  (value-equal 'equal)
  (key-serializer 'serialize)
  (key-deserializer 'deserialize)
  (value-serializer 'identity)
  (value-deserializer 'identity)
  (lock (make-rw-lock)))

(defmacro with-bpt-read-lock ((tree) &body body)
  `(with-read-lock ((%bpt-lock ,tree)) ,@body))
(defmacro with-bpt-write-lock ((tree) &body body)
  `(with-write-lock ((%bpt-lock ,tree)) ,@body))

(declaim (inline bpt-key< bpt-key=))
(defun bpt-key< (tree a b) (funcall (%bpt-comparison tree) a b))
(defun bpt-key= (tree a b) (funcall (%bpt-key-equal tree) a b))

;;; ---------------------------------------------------------------------------
;;; Raw page I/O -- one memcpy per page touch
;;; ---------------------------------------------------------------------------
;;; A whole page moves between the mmap region and a Lisp byte vector in a single
;;; libc memcpy (via CFFI), not a per-byte loop through the generic accessors.
;;; Safe here: single-writer under the tree write lock, and the mapping base is
;;; stable for the mapping's life (MAP_FIXED; see mmap.lisp), so there are no torn
;;; reads and the SEGV-retry :around is unnecessary.

;; Cold-cache instrumentation: when *BPT-PAGE-TRACE* is a hash-table, record every
;; page touched.  Distinct pages touched per op is the hardware-independent
;; predictor of cold page faults -- the quantity the locality experiment is about.
;; Inert (one special-var load + null test) when unbound/NIL.
(defvar *bpt-page-trace* nil)

(declaim (inline %bpt-note-page))
(defun %bpt-note-page (addr)
  (when *bpt-page-trace* (setf (gethash addr *bpt-page-trace*) t)))

(defun %bpt-check-addr (tree addr)
  "Signal a catchable error (instead of a raw memcpy SEGV) if ADDR is not a valid
in-bounds page address in the heap."
  (let ((heap (%bpt-heap tree)) (ps (%bpt-page-size tree)))
    (unless (and (integerp addr)
                 (>= addr (memory-data-offset heap))
                 (<= (+ addr ps) (memory-size heap)))
      (error "BPT: page address ~S out of heap bounds [~S,~S) (page-size ~S)"
             addr (memory-data-offset heap) (memory-size heap) ps))))

(defun %bpt-read-page (tree addr)
  (%bpt-note-page addr)
  (%bpt-check-addr tree addr)
  (let* ((ps (%bpt-page-size tree))
         (buf (make-byte-vector ps))
         (src (cffi:inc-pointer (m-pointer (%bpt-mmap tree)) addr)))
    (cffi:with-pointer-to-vector-data (dst buf)
      (cffi:foreign-funcall "memcpy" :pointer dst :pointer src :size ps :pointer))
    buf))

(defun %bpt-write-page (tree addr buf)
  (%bpt-check-addr tree addr)
  (let* ((ps (%bpt-page-size tree))
         (dst (cffi:inc-pointer (m-pointer (%bpt-mmap tree)) addr)))
    (cffi:with-pointer-to-vector-data (src buf)
      (cffi:foreign-funcall "memcpy" :pointer dst :pointer src :size ps :pointer))
    addr))

(defun %bpt-alloc-page (tree)
  "Allocate a fresh zeroed page from the heap; return its address."
  (let ((addr (allocate (%bpt-heap tree) (%bpt-page-size tree))))
    ;; ALLOCATE may hand back a reused (previously freed) block with stale bytes;
    ;; zero the header region we rely on.  Cell area is addressed via the slot
    ;; directory, so stale cell bytes are harmless once count/free are reset.
    (let ((mf (%bpt-mmap tree)))
      (dotimes (i +bpt-p-slots-offset+) (%set-byte mf (+ addr i) 0)))
    addr))

;;; ---------------------------------------------------------------------------
;;; Page decode / encode
;;; ---------------------------------------------------------------------------
;;; A decoded page is (values LEAF-P LINK ENTRIES).  An ENTRY is, in slot order:
;;;   leaf:     (dkey skey sval)        -- deserialized key, serialized key bytes,
;;;                                        serialized value bytes
;;;   internal: (dkey skey child-addr)  -- child holds keys >= dkey
;;; Keeping SKEY (the raw serialized key) avoids re-serializing on rewrite; DKEY
;;; is decoded once for comparison.

(defun %bpt-leaf-p (buf) (logbitp 0 (aref buf +bpt-p-flags-offset+)))

(defun %bpt-decode-page (tree buf)
  (let* ((leaf-p (%bpt-leaf-p buf))
         (n (buf-u16 buf +bpt-p-count-offset+))
         (link (buf-u64 buf +bpt-p-link-offset+))
         (kd (%bpt-key-deserializer tree))
         (entries '()))
    (dotimes (i n)
      (let* ((slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* i 2))))
             (klen (buf-u16 buf slot))
             (kstart (+ slot 2))
             (skey (subseq buf kstart (+ kstart klen)))
             (dkey (funcall kd skey)))
        (if leaf-p
            (let* ((voff (+ kstart klen))
                   (vlen (buf-u16 buf voff))
                   (vstart (+ voff 2))
                   (sval (subseq buf vstart (+ vstart vlen))))
              (push (list dkey skey sval) entries))
            (let ((child (buf-u64 buf (+ kstart klen))))
              (push (list dkey skey child) entries)))))
    (values leaf-p link (nreverse entries))))

(defun %bpt-entry-cell-size (leaf-p entry)
  "Bytes an ENTRY occupies as a cell (excluding its 2-byte slot)."
  (let ((klen (length (second entry))))
    (if leaf-p
        (+ 2 klen 2 (length (third entry)))  ; klen + key + vlen + val
        (+ 2 klen 8))))                       ; klen + key + child ptr

(defun %bpt-encode-page (tree leaf-p link entries)
  "Encode ENTRIES into a fresh page buffer.  Return the buffer, or NIL if the
entries do not fit in a page (caller must split)."
  (let* ((ps (%bpt-page-size tree))
         (n (length entries))
         (dir-end (+ +bpt-p-slots-offset+ (* n 2)))
         (total (+ dir-end
                   (reduce #'+ entries :initial-value 0
                                       :key (lambda (e) (%bpt-entry-cell-size leaf-p e))))))
    (when (> total ps)
      (return-from %bpt-encode-page nil))
    (let ((buf (make-byte-vector ps))
          (free ps))
      (setf (aref buf 0) +bplus-node-magic+
            (aref buf +bpt-p-flags-offset+) (if leaf-p 1 0)
            (buf-u16 buf +bpt-p-count-offset+) n
            (buf-u64 buf +bpt-p-link-offset+) link)
      (loop for e in entries
            for i from 0 do
        (let* ((skey (second e))
               (klen (length skey))
               (csize (%bpt-entry-cell-size leaf-p e))
               (cell (- free csize)))
          (setf (buf-u16 buf cell) klen)
          (dotimes (b klen) (setf (aref buf (+ cell 2 b)) (aref skey b)))
          (if leaf-p
              (let* ((sval (third e)) (vlen (length sval)) (voff (+ cell 2 klen)))
                (setf (buf-u16 buf voff) vlen)
                (dotimes (b vlen) (setf (aref buf (+ voff 2 b)) (aref sval b))))
              (setf (buf-u64 buf (+ cell 2 klen)) (third e)))
          (setf (buf-u16 buf (+ +bpt-p-slots-offset+ (* i 2))) cell
                free cell)))
      (setf (buf-u16 buf +bpt-p-free-offset+) free)
      buf)))

;;; ---------------------------------------------------------------------------
;;; Header persistence
;;; ---------------------------------------------------------------------------

(defun %bpt-write-header (tree)
  (let ((heap (%bpt-heap tree)) (addr (%bpt-address tree)))
    (set-byte heap addr +bplus-tree-magic+)
    (set-byte heap (+ addr 1) +bplus-format-version+)
    (serialize-uint64 heap (%bpt-root tree) (+ addr +bpt-h-root-offset+))
    (serialize-uint64 heap (%bpt-count tree) (+ addr +bpt-h-count-offset+))
    (serialize-uint64 heap (%bpt-height tree) (+ addr +bpt-h-height-offset+))
    (serialize-uint64 heap (%bpt-page-size tree) (+ addr +bpt-h-pagesize-offset+))
    addr))

(defun %bpt-sync-root (tree)
  "Persist root/height/count after a structural change."
  (let ((heap (%bpt-heap tree)) (addr (%bpt-address tree)))
    (serialize-uint64 heap (%bpt-root tree) (+ addr +bpt-h-root-offset+))
    (serialize-uint64 heap (%bpt-count tree) (+ addr +bpt-h-count-offset+))
    (serialize-uint64 heap (%bpt-height tree) (+ addr +bpt-h-height-offset+))))

(defun %bpt-sync-count (tree)
  (serialize-uint64 (%bpt-heap tree) (%bpt-count tree)
                    (+ (%bpt-address tree) +bpt-h-count-offset+)))

;;; ---------------------------------------------------------------------------
;;; Construction / open / close / delete
;;; ---------------------------------------------------------------------------

(defun make-bplus-tree (&key heap key-comparison key-equal (value-equal 'equal)
                          (key-serializer 'serialize) (key-deserializer 'deserialize)
                          (value-serializer 'identity) (value-deserializer 'identity)
                          (page-size +bplus-default-page-size+))
  "Create an empty on-disk B+ tree in HEAP.  KEY-COMPARISON is strict less-than
and KEY-EQUAL equality over DESERIALIZED keys (mirroring MAKE-SKIP-LIST).  Unlike
the skip list there are no head/tail sentinel keys -- an empty tree is an empty
leaf root."
  (let* ((address (allocate heap +bplus-header-size+))
         (tree (%make-bplus-tree
                :heap heap :mmap (memory-mmap heap) :address address
                :page-size page-size
                :comparison key-comparison :key-equal key-equal
                :value-equal value-equal
                :key-serializer key-serializer :key-deserializer key-deserializer
                :value-serializer value-serializer
                :value-deserializer value-deserializer)))
    ;; Allocate the root as an empty leaf.
    (let ((root (%bpt-alloc-page tree)))
      (%bpt-write-page tree root (%bpt-encode-page tree t 0 '()))
      (setf (%bpt-root tree) root
            (%bpt-height tree) 1
            (%bpt-count tree) 0))
    (%bpt-write-header tree)
    tree))

(defun open-bplus-tree (&key address heap key-comparison key-equal
                          (value-equal 'equal)
                          (key-serializer 'serialize) (key-deserializer 'deserialize)
                          (value-serializer 'identity) (value-deserializer 'identity))
  "Reopen a B+ tree whose header block is at ADDRESS in HEAP."
  (let ((magic (get-byte heap address)))
    (unless (= magic +bplus-tree-magic+)
      (error "Not a B+ tree at address ~S (magic ~X)" address magic)))
  (let* ((version (get-byte heap (+ address 1)))
         (root (deserialize-uint64 heap (+ address +bpt-h-root-offset+)))
         (count (deserialize-uint64 heap (+ address +bpt-h-count-offset+)))
         (height (deserialize-uint64 heap (+ address +bpt-h-height-offset+)))
         (page-size (deserialize-uint64 heap (+ address +bpt-h-pagesize-offset+))))
    (unless (= version +bplus-format-version+)
      (error "B+ tree format version ~S at ~S; expected ~S"
             version address +bplus-format-version+))
    (%make-bplus-tree
     :heap heap :mmap (memory-mmap heap) :address address
     :root root :count count :height height :page-size page-size
     :comparison key-comparison :key-equal key-equal :value-equal value-equal
     :key-serializer key-serializer :key-deserializer key-deserializer
     :value-serializer value-serializer :value-deserializer value-deserializer)))

(defun close-bplus-tree (tree) (declare (ignore tree)) nil)

(defun delete-bplus-tree (tree)
  "Free every page and the header block."
  (with-bpt-write-lock (tree)
    (labels ((free-subtree (addr)
               (multiple-value-bind (leaf-p link entries)
                   (%bpt-decode-page tree (%bpt-read-page tree addr))
                 (unless leaf-p
                   (free-subtree link)                 ; P0
                   (dolist (e entries) (free-subtree (third e))))
                 (free (%bpt-heap tree) addr))))
      (free-subtree (%bpt-root tree))
      (free (%bpt-heap tree) (%bpt-address tree))
      (setf (%bpt-root tree) nil (%bpt-heap tree) nil (%bpt-mmap tree) nil)
      nil)))

(defun bplus-tree-count (tree) (%bpt-count tree))

;;; ---------------------------------------------------------------------------
;;; Lean in-page search (the read path -- avoids decoding the whole page)
;;; ---------------------------------------------------------------------------
;;; INSERT/REMOVE rewrite a page, so they decode it wholesale.  FIND / descent
;;; only need to locate one slot, so they binary-search the sorted slot directory
;;; directly in the page buffer, deserializing only the ~log2(fanout) probed keys
;;; -- not all of them.  This is what keeps a warm point lookup cheap.

(defun %bpt-slot-key (tree buf slot)
  "Deserialize just the key of the cell at SLOT."
  (let ((klen (buf-u16 buf slot)))
    (funcall (%bpt-key-deserializer tree) (subseq buf (+ slot 2) (+ slot 2 klen)))))

(defun %bpt-page-bsearch (tree buf n dkey)
  "Binary-search the slot directory for DKEY.  Return (values IDX EXACT-P) where
IDX is the index of the greatest key <= DKEY (-1 if DKEY precedes all keys), and
EXACT-P is true when key[IDX] = DKEY."
  (let ((lo 0) (hi (1- n)) (res -1) (exact nil))
    (loop while (<= lo hi) do
      (let* ((mid (ash (+ lo hi) -1))
             (slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* mid 2))))
             (mk (%bpt-slot-key tree buf slot)))
        (cond ((bpt-key= tree dkey mk) (setf res mid exact t) (return))
              ((bpt-key< tree mk dkey) (setf res mid lo (1+ mid)))
              (t (setf hi (1- mid))))))
    (values res exact)))

(defun %bpt-slot-child (buf idx)
  "Child pointer of internal cell IDX (its trailing u64)."
  (let ((slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* idx 2)))))
    (buf-u64 buf (+ slot 2 (buf-u16 buf slot)))))

(defun %bpt-descend-leaf-addr (tree dkey)
  "Return (values LEAF-ADDR LEAF-BUF), binary-searching each internal node
without decoding it wholesale.  Returning the leaf's already-read BUF (not just
its address) matters: every caller used to immediately re-issue %BPT-READ-PAGE
on this same address, paying a second full-page memcpy + fresh allocation for a
page this function had just read -- one wasted ~page-size cons on every point
lookup, insert/delete descent, and range-cursor open (GH #97 localization)."
  (let ((addr (%bpt-root tree)))
    (loop
      (let ((buf (%bpt-read-page tree addr)))
        (if (%bpt-leaf-p buf)
            (return (values addr buf))
            (let ((n (buf-u16 buf +bpt-p-count-offset+)))
              (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
                (declare (ignore exact))
                (setf addr (if (< idx 0)
                               (buf-u64 buf +bpt-p-link-offset+)  ; P0
                               (%bpt-slot-child buf idx))))))))))

;;; ---------------------------------------------------------------------------
;;; Navigation
;;; ---------------------------------------------------------------------------

(defun %bpt-descend-to-leaf (tree dkey)
  "Return (values LEAF-ADDR LEAF-BUF LEAF-LINK LEAF-ENTRIES) for the leaf that
would hold DKEY.  Internal nodes are binary-searched (not decoded); only the
final leaf is decoded into entries (its caller needs them)."
  (multiple-value-bind (addr buf) (%bpt-descend-leaf-addr tree dkey)
    (multiple-value-bind (leaf-p link entries) (%bpt-decode-page tree buf)
      (declare (ignore leaf-p))
      (values addr buf link entries))))

;;; ---------------------------------------------------------------------------
;;; Find
;;; ---------------------------------------------------------------------------

(defun %bpt-find (tree dkey)
  "Return (values SVAL FOUND-P) for DKEY -- SVAL is the raw serialized value.
Fully lean: binary-search all the way down, decode only the one matching cell."
  (multiple-value-bind (addr buf) (%bpt-descend-leaf-addr tree dkey)
    (declare (ignore addr))
    (let ((n (buf-u16 buf +bpt-p-count-offset+)))
      (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
        (if exact
            (let* ((slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* idx 2))))
                   (voff (+ slot 2 (buf-u16 buf slot)))
                   (vlen (buf-u16 buf voff)))
              (values (subseq buf (+ voff 2) (+ voff 2 vlen)) t))
            (values nil nil))))))

;;; ---------------------------------------------------------------------------
;;; Slotted-page in-place cell edits (the write-cost path)
;;; ---------------------------------------------------------------------------
;;; A non-splitting insert / a delete edits ONE cell in the page buffer -- shift
;;; the sorted slot directory by one entry and (for insert) drop the new cell into
;;; the free gap -- instead of decoding every cell and re-encoding the whole page.
;;; Deletes leave the vacated cell bytes as a hole; an insert that no longer fits
;;; the contiguous free gap first COMPACTS (repacks live cells, reclaiming holes),
;;; and only splits when the page is genuinely full.  Cells are moved as raw bytes
;;; (SUBSEQ / REPLACE), never deserialized.  The split path still decodes (rare).

(declaim (inline %bpt-slot-off (setf %bpt-slot-off)))
(defun %bpt-slot-off (buf i)
  "Byte offset of cell I (its content), read from the slot directory."
  (buf-u16 buf (+ +bpt-p-slots-offset+ (* i 2))))
(defun (setf %bpt-slot-off) (value buf i)
  (setf (buf-u16 buf (+ +bpt-p-slots-offset+ (* i 2))) value))

(defun %bpt-cell-size-at (buf slot leaf-p)
  "Total bytes of the cell whose content starts at SLOT."
  (let ((klen (buf-u16 buf slot)))
    (if leaf-p
        (+ 2 klen 2 (buf-u16 buf (+ slot 2 klen)))  ; klen + key + vlen + val
        (+ 2 klen 8))))                              ; klen + key + child

(defun %bpt-cell-sval (buf slot)
  "The serialized value bytes of the leaf cell at SLOT."
  (let* ((klen (buf-u16 buf slot))
         (voff (+ slot 2 klen))
         (vlen (buf-u16 buf voff)))
    (subseq buf (+ voff 2) (+ voff 2 vlen))))

(defun %bpt-live-bytes (buf n leaf-p)
  "Sum of the live cells' sizes (excludes holes left by deleted cells)."
  (let ((sum 0))
    (dotimes (i n sum) (incf sum (%bpt-cell-size-at buf (%bpt-slot-off buf i) leaf-p)))))

(defun %bpt-compact-page (tree buf leaf-p)
  "Repack the live cells contiguously against the end of the page (reclaiming
holes) and reset the free pointer.  Cells move as raw bytes -- no decode."
  (let* ((ps (%bpt-page-size tree))
         (n (buf-u16 buf +bpt-p-count-offset+))
         (cells (make-array n))
         (free ps))
    (dotimes (i n)
      (let* ((slot (%bpt-slot-off buf i))
             (sz (%bpt-cell-size-at buf slot leaf-p)))
        (setf (aref cells i) (subseq buf slot (+ slot sz)))))
    (dotimes (i n)
      (let* ((bytes (aref cells i)) (sz (length bytes)) (cell (- free sz)))
        (replace buf bytes :start1 cell)
        (setf (%bpt-slot-off buf i) cell
              free cell)))
    (setf (buf-u16 buf +bpt-p-free-offset+) free)))

(defun %bpt-page-insert-at (tree buf leaf-p idx skey payload)
  "Insert a cell (SKEY + PAYLOAD, where PAYLOAD is the serialized value for a leaf
or the child address for an internal node) at slot position IDX, compacting first
if the free gap is too small.  Return T on success, NIL if the page is genuinely
full (caller must split).  On NIL the buffer is left UNCHANGED."
  (let* ((ps (%bpt-page-size tree))
         (n (buf-u16 buf +bpt-p-count-offset+))
         (klen (length skey))
         (csize (if leaf-p (+ 2 klen 2 (length payload)) (+ 2 klen 8)))
         (need (+ csize 2))                                  ; cell + its new slot
         (dir-end (+ +bpt-p-slots-offset+ (* n 2)))
         (free (buf-u16 buf +bpt-p-free-offset+)))
    (when (< (- free dir-end) need)
      ;; Not enough contiguous gap -- compact if the live data would fit, else full.
      (when (> (+ +bpt-p-slots-offset+ (* (1+ n) 2) (%bpt-live-bytes buf n leaf-p) csize) ps)
        (return-from %bpt-page-insert-at nil))
      (%bpt-compact-page tree buf leaf-p)
      (setf free (buf-u16 buf +bpt-p-free-offset+)))
    (let ((cell (- free csize)))
      (setf (buf-u16 buf cell) klen)
      (replace buf skey :start1 (+ cell 2))
      (if leaf-p
          (progn (setf (buf-u16 buf (+ cell 2 klen)) (length payload))
                 (replace buf payload :start1 (+ cell 2 klen 2)))
          (setf (buf-u64 buf (+ cell 2 klen)) payload))
      ;; Shift slots [IDX..n-1] up by one, then drop the new slot in at IDX.
      (loop for i from n downto (1+ idx)
            do (setf (%bpt-slot-off buf i) (%bpt-slot-off buf (1- i))))
      (setf (%bpt-slot-off buf idx) cell
            (buf-u16 buf +bpt-p-count-offset+) (1+ n)
            (buf-u16 buf +bpt-p-free-offset+) cell)
      t)))

(defun %bpt-page-delete-at (buf idx)
  "Remove slot IDX (shift the slot directory down); the vacated cell bytes become
a hole reclaimed on the next compaction.  Count decremented; free pointer unchanged."
  (let ((n (buf-u16 buf +bpt-p-count-offset+)))
    (loop for i from idx below (1- n)
          do (setf (%bpt-slot-off buf i) (%bpt-slot-off buf (1+ i))))
    (setf (buf-u16 buf +bpt-p-count-offset+) (1- n))
    t))

;;; ---------------------------------------------------------------------------
;;; Insert (recursive, with split propagation)
;;; ---------------------------------------------------------------------------
;;; A non-splitting insert edits one cell in place (%BPT-PAGE-INSERT-AT); only an
;;; overflowing page falls back to the decode/partition/encode split path.
;;; %BPT-INSERT returns:
;;;   :dup                             key already present (no-op)
;;;   :no-split                        inserted, page did not split
;;;   (SEP-DKEY SEP-SKEY RIGHT-ADDR)   page split; parent must insert this
;;;                                    separator pointing at the new right page.

(defun %bpt-insert-sorted-leaf (tree entries dkey skey sval)
  "Insert (DKEY SKEY SVAL) into leaf ENTRIES keeping key order.  Return the new
entry list, or :DUP if an equal key is already present."
  (let ((out '()) (rest entries) (placed nil))
    (loop
      (when (null rest)
        (unless placed (push (list dkey skey sval) out))
        (return (nreverse out)))
      (let ((e (car rest)))
        (cond ((and (not placed) (bpt-key= tree dkey (first e)))
               (return :dup))
              ((and (not placed) (bpt-key< tree dkey (first e)))
               (push (list dkey skey sval) out)
               (setf placed t))
              (t
               (push e out)
               (setf rest (cdr rest))))))))

(defun %bpt-insert-sorted-internal (tree entries dkey skey child)
  "Insert (DKEY SKEY CHILD) into internal ENTRIES keeping key order."
  (let ((out '()) (rest entries) (placed nil))
    (loop
      (when (null rest)
        (unless placed (push (list dkey skey child) out))
        (return (nreverse out)))
      (let ((e (car rest)))
        (if (and (not placed) (bpt-key< tree dkey (first e)))
            (progn (push (list dkey skey child) out) (setf placed t))
            (progn (push e out) (setf rest (cdr rest))))))))

(defun %bpt-store-or-split (tree addr leaf-p link entries)
  "Write ENTRIES back to page ADDR, splitting if they overflow.  Return :NO-SPLIT
or (SEP-DKEY SEP-SKEY RIGHT-ADDR)."
  (let ((buf (%bpt-encode-page tree leaf-p link entries)))
    (if buf
        (progn (%bpt-write-page tree addr buf) :no-split)
        ;; Overflow -> split.
        (let* ((n (length entries))
               (mid (floor n 2)))
          (when (< n 2)
            (error "B+ tree: single entry too large for a ~A-byte page"
                   (%bpt-page-size tree)))
          (if leaf-p
              ;; Leaf split: right keeps the upper half; its first key is copied
              ;; up as the separator (the key itself STAYS in the right leaf).
              (let* ((left (subseq entries 0 mid))
                     (right (subseq entries mid))
                     (right-addr (%bpt-alloc-page tree))
                     (sep (first right)))
                (%bpt-write-page tree right-addr
                                 (%bpt-encode-page tree t link right))   ; right.next = old next
                (%bpt-write-page tree addr
                                 (%bpt-encode-page tree t right-addr left)) ; left.next = right
                (list (first sep) (second sep) right-addr))
              ;; Internal split: the middle entry moves UP (removed from this
              ;; level); its child becomes the right node's P0.
              (let* ((left (subseq entries 0 mid))
                     (mid-entry (nth mid entries))
                     (right (subseq entries (1+ mid)))
                     (right-addr (%bpt-alloc-page tree)))
                (%bpt-write-page tree right-addr
                                 (%bpt-encode-page tree nil (third mid-entry) right))
                (%bpt-write-page tree addr
                                 (%bpt-encode-page tree nil link left))
                (list (first mid-entry) (second mid-entry) right-addr)))))))

(defun %bpt-split-leaf (tree addr buf dkey skey sval)
  "Overflowing leaf: decode BUF, add the new entry, partition into two pages.
Returns (SEP-DKEY SEP-SKEY RIGHT-ADDR)."
  (multiple-value-bind (lp link entries) (%bpt-decode-page tree buf)
    (declare (ignore lp))
    (%bpt-store-or-split tree addr t link
                         (%bpt-insert-sorted-leaf tree entries dkey skey sval))))

(defun %bpt-split-internal (tree addr buf sep-dkey sep-skey right-addr)
  "Overflowing internal node: decode BUF, add the separator, partition."
  (multiple-value-bind (lp link entries) (%bpt-decode-page tree buf)
    (declare (ignore lp))
    (%bpt-store-or-split tree addr nil link
                         (%bpt-insert-sorted-internal tree entries sep-dkey sep-skey right-addr))))

(defun %bpt-insert (tree addr dkey skey sval)
  (let* ((buf (%bpt-read-page tree addr))
         (n (buf-u16 buf +bpt-p-count-offset+)))
    (if (%bpt-leaf-p buf)
        ;; Leaf: locate the key; equal => duplicate no-op; else insert after the
        ;; last key < DKEY, in place if it fits, otherwise split.
        (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
          (if exact
              :dup
              (let ((ins (1+ idx)))
                (if (%bpt-page-insert-at tree buf t ins skey sval)
                    (progn (%bpt-write-page tree addr buf) :no-split)
                    (%bpt-split-leaf tree addr buf dkey skey sval)))))
        ;; Internal: descend to the right child, then absorb any split it returns.
        (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
          (declare (ignore exact))
          (let* ((child (if (< idx 0) (buf-u64 buf +bpt-p-link-offset+) (%bpt-slot-child buf idx)))
                 (res (%bpt-insert tree child dkey skey sval)))
            (cond ((eq res :dup) :dup)
                  ((eq res :no-split) :no-split)
                  (t
                   (destructuring-bind (sep-dkey sep-skey right-addr) res
                     ;; BUF is our snapshot; the recursion only touched descendants,
                     ;; so it is still valid.  Insert the separator in place or split.
                     (multiple-value-bind (sidx sx) (%bpt-page-bsearch tree buf n sep-dkey)
                       (declare (ignore sx))
                       (let ((sins (1+ sidx)))
                         (if (%bpt-page-insert-at tree buf nil sins sep-skey right-addr)
                             (progn (%bpt-write-page tree addr buf) :no-split)
                             (%bpt-split-internal tree addr buf sep-dkey sep-skey right-addr))))))))))))

(defun bpt-insert (tree dkey skey sval)
  "Insert DKEY -> value.  DKEY is the deserialized key; SKEY / SVAL the serialized
key / value bytes.  Duplicate keys are a no-op (returns NIL); returns T on insert."
  (with-bpt-write-lock (tree)
    (let ((res (%bpt-insert tree (%bpt-root tree) dkey skey sval)))
      (cond ((eq res :dup) nil)
            (t
             (when (consp res)
               ;; Root split: grow a new internal root.
               (destructuring-bind (sep-dkey sep-skey right-addr) res
                 (let ((new-root (%bpt-alloc-page tree))
                       (old-root (%bpt-root tree)))
                   (%bpt-write-page
                    tree new-root
                    (%bpt-encode-page tree nil old-root
                                      (list (list sep-dkey sep-skey right-addr))))
                   (setf (%bpt-root tree) new-root)
                   (incf (%bpt-height tree)))))
             (incf (%bpt-count tree))
             (%bpt-sync-root tree)
             t)))))

;;; ---------------------------------------------------------------------------
;;; Remove (with merge-on-delete rebalancing)
;;; ---------------------------------------------------------------------------
;;; A delete removes the leaf cell in place (cheap).  If the leaf then UNDERFLOWS
;;; (drops below half full, or empties), the parent MERGES it with an adjacent
;;; sibling when the two fit in one page, freeing the vacated page and dropping the
;;; separator -- which only ever SHRINKS the parent, so a merge never overflows.
;;; Underflow then propagates up; an internal root left with only its P0 child
;;; collapses (the tree shrinks by one level).  Merge-only (no borrow): an empty
;;; page always merges (its sibling alone fits), so empty pages never linger; a
;;; still-underfull node whose siblings are both too full to absorb it is simply
;;; left until a later delete shrinks a neighbour.  (Borrow/redistribute -- which
;;; would keep every node >= half full but can grow a variable-length separator
;;; and overflow the parent -- is a later refinement.)

(defun %bpt-usable (tree) (- (%bpt-page-size tree) +bpt-p-slots-offset+))

(defun %bpt-page-underflow-p (tree buf leaf-p)
  "True if BUF is a non-root page that should try to merge: empty, or under half
full by live bytes."
  (let ((n (buf-u16 buf +bpt-p-count-offset+)))
    (or (zerop n)
        (< (* 2 (%bpt-live-bytes buf n leaf-p)) (%bpt-usable tree)))))

(defun %bpt-child-decode (tree addr)
  "Decode the page at ADDR -> (values LEAF-P LINK ENTRIES)."
  (%bpt-decode-page tree (%bpt-read-page tree addr)))

(defun %bpt-rebalance-child (tree paddr cidx)
  "Try to MERGE the underflowed child (at parent slot CIDX; -1 = P0) with an
adjacent sibling: combine two adjacent children into the left one, free the right
one, and drop the separator from the parent.  Prefers the left sibling.  No-op if
neither adjacent merge fits in a page."
  (multiple-value-bind (pleaf plink pentries) (%bpt-decode-page tree (%bpt-read-page tree paddr))
    (declare (ignore pleaf))
    (let* ((pvec (coerce pentries 'vector))
           (nch (1+ (length pvec)))              ; child count: P0 + one per entry
           (ci (1+ cidx)))                        ; child index in [0..nch-1]; 0 = P0
      (labels ((child-addr (k) (if (= k 0) plink (third (aref pvec (1- k)))))
               (try (left-idx)                    ; merge children[left-idx] & [left-idx+1]
                 (let* ((sep (aref pvec left-idx))          ; separator (key . right-addr)
                        (left-addr (child-addr left-idx))
                        (right-addr (third sep)))
                   (multiple-value-bind (leaf-p llink lentries) (%bpt-child-decode tree left-addr)
                     (multiple-value-bind (rlp rlink rentries) (%bpt-child-decode tree right-addr)
                       (declare (ignore rlp))
                       (let* ((combined (if leaf-p
                                            (append lentries rentries)
                                            ;; internal: the parent separator drops
                                            ;; DOWN, paired with RIGHT's P0 child.
                                            (append lentries
                                                    (list (list (first sep) (second sep) rlink))
                                                    rentries)))
                              (new-link (if leaf-p rlink llink))  ; leaf: absorb right's next
                              (buf (%bpt-encode-page tree leaf-p new-link combined)))
                         (when buf                            ; fits -> perform the merge
                           (%bpt-write-page tree left-addr buf)
                           (free (%bpt-heap tree) right-addr)
                           (%bpt-write-page
                            tree paddr
                            (%bpt-encode-page tree nil plink
                                              (append (subseq pentries 0 left-idx)
                                                      (subseq pentries (1+ left-idx)))))
                           t)))))))
        (cond ((>= (1- ci) 0)                     ; has a left sibling -> prefer it
               (or (try (1- ci))
                   (when (<= (1+ ci) (1- nch)) (try ci))))
              ((<= (1+ ci) (1- nch)) (try ci))     ; only a right sibling
              (t nil))))))

(defun %bpt-delete (tree addr dkey sval sval-p veq root-p)
  "Recursive delete.  Returns (values STATUS UNDERFLOW-P) where STATUS is :REMOVED
or :NOT-FOUND, and UNDERFLOW-P says whether ADDR is now underfull (never for the
root).  A child's underflow is resolved by merging it here before we report our own."
  (let* ((buf (%bpt-read-page tree addr))
         (n (buf-u16 buf +bpt-p-count-offset+)))
    (if (%bpt-leaf-p buf)
        (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
          (if (and exact
                   (or (not sval-p)
                       (funcall veq sval (%bpt-cell-sval buf (%bpt-slot-off buf idx)))))
              (progn
                (%bpt-page-delete-at buf idx)
                (%bpt-write-page tree addr buf)
                (values :removed (and (not root-p) (%bpt-page-underflow-p tree buf t))))
              (values :not-found nil)))
        (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
          (declare (ignore exact))
          (let ((child (if (< idx 0) (buf-u64 buf +bpt-p-link-offset+) (%bpt-slot-child buf idx))))
            (multiple-value-bind (status child-underflow)
                (%bpt-delete tree child dkey sval sval-p veq nil)
              (if (eq status :not-found)
                  (values :not-found nil)
                  (progn
                    (when child-underflow (%bpt-rebalance-child tree addr idx))
                    ;; Re-read: a merge may have shrunk this node.
                    (let ((buf2 (%bpt-read-page tree addr)))
                      (values :removed
                              (and (not root-p) (%bpt-page-underflow-p tree buf2 nil))))))))))))

(defun %bpt-maybe-collapse-root (tree)
  "While the root is an internal node holding only its P0 child, make P0 the new
root and drop a level."
  (loop
    (let ((buf (%bpt-read-page tree (%bpt-root tree))))
      (if (and (not (%bpt-leaf-p buf)) (zerop (buf-u16 buf +bpt-p-count-offset+)))
          (let ((p0 (buf-u64 buf +bpt-p-link-offset+)) (old (%bpt-root tree)))
            (free (%bpt-heap tree) old)
            (setf (%bpt-root tree) p0)
            (when (> (%bpt-height tree) 1) (decf (%bpt-height tree))))
          (return)))))

(defun bpt-remove (tree dkey &optional (sval nil sval-p) value-equal)
  "Remove DKEY from its leaf.  With SVAL given, only remove when the stored value
also matches (via VALUE-EQUAL or the tree's).  Returns T if a key was removed.
Merges underfull pages on the way up and collapses a degenerate root."
  (with-bpt-write-lock (tree)
    (let ((veq (or value-equal (%bpt-value-equal tree))))
      (multiple-value-bind (status)
          (%bpt-delete tree (%bpt-root tree) dkey sval sval-p veq t)
        (if (eq status :removed)
            (progn
              (%bpt-maybe-collapse-root tree)
              (decf (%bpt-count tree))
              (%bpt-sync-root tree)
              t)
            nil)))))

;;; ---------------------------------------------------------------------------
;;; Cursors -- leaf-linked forward scan (the range-scan locality payoff)
;;; ---------------------------------------------------------------------------

(defclass bplus-cursor (cursor)
  ((tree :initarg :tree :accessor bpc-tree)
   (buf :initarg :buf :accessor bpc-buf)                ; raw page buffer
   (count :initarg :count :accessor bpc-count)          ; number of entries in page
   (index :initarg :index :accessor bpc-index)          ; current slot index
   (next-addr :initarg :next-addr :accessor bpc-next-addr) ; link of sibling leaf
   (end :initarg :end :accessor bpc-end)                ; deserialized end key, or NIL
   (bounded-p :initarg :bounded-p :accessor bpc-bounded-p)))

(defun %bpt-decode-page-entry (tree buf slot leaf-p)
  "Decode just one entry at SLOT in page BUF."
  (let* ((klen (buf-u16 buf slot))
         (kstart (+ slot 2))
         (skey (subseq buf kstart (+ kstart klen)))
         (dkey (funcall (%bpt-key-deserializer tree) skey)))
    (if leaf-p
        (let* ((voff (+ kstart klen))
               (vlen (buf-u16 buf voff))
               (vstart (+ voff 2))
               (sval (subseq buf vstart (+ vstart vlen))))
          (list dkey skey sval))
        (let ((child (buf-u64 buf (+ kstart klen))))
          (list dkey skey child)))))

(defun %bpt-leftmost-leaf (tree)
  "Return (values BUF COUNT NEXT-ADDR) for the first (leftmost) leaf."
  (let ((addr (%bpt-root tree)))
    (loop
      (let* ((buf (%bpt-read-page tree addr))
             (leaf-p (%bpt-leaf-p buf))
             (link (buf-u64 buf +bpt-p-link-offset+))
             (count (buf-u16 buf +bpt-p-count-offset+)))
        (if leaf-p
            (return (values buf count link))
            (setf addr link))))))

(defun %bpt-leaf-at (tree dkey)
  "Return (values BUF COUNT NEXT-ADDR START-INDEX) positioned at the first
entry with key >= DKEY in the leaf that would hold DKEY."
  (multiple-value-bind (addr buf) (%bpt-descend-leaf-addr tree dkey)
    (declare (ignore addr))
    (let* ((count (buf-u16 buf +bpt-p-count-offset+))
           (link (buf-u64 buf +bpt-p-link-offset+)))
      (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf count dkey)
        (declare (ignore exact))
        (let ((start 0))
          (if (< idx 0)
              (setf start 0)
              (let* ((slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* idx 2))))
                     (mk (%bpt-slot-key tree buf slot)))
                (if (bpt-key< tree mk dkey)
                    (setf start (1+ idx))
                    (setf start idx))))
          (values buf count link (min count (max 0 start))))))))

(defun %bpt-materialize (tree entry)
  "Build a SKIP-NODE from a leaf ENTRY so consumers (views/spatial/unique) read
%SN-KEY / %SN-VALUE unchanged."
  (%make-skip-node :key (first entry)
                   :value (funcall (%bpt-value-deserializer tree) (third entry))))

(defmethod cursor-next ((c bplus-cursor) &optional eoc)
  (with-bpt-read-lock ((bpc-tree c))
    (let ((tree (bpc-tree c)))
      (loop
        (when (>= (bpc-index c) (bpc-count c))
          ;; Advance to the next leaf via the sibling link (sequential pages).
          (if (zerop (bpc-next-addr c))
              (return eoc)
              (let* ((next-addr (bpc-next-addr c))
                     (buf (%bpt-read-page tree next-addr))
                     (count (buf-u16 buf +bpt-p-count-offset+))
                     (link (buf-u64 buf +bpt-p-link-offset+)))
                (setf (bpc-buf c) buf
                      (bpc-count c) count
                      (bpc-index c) 0
                      (bpc-next-addr c) link))))
        (if (>= (bpc-index c) (bpc-count c))
            (when (zerop (bpc-next-addr c)) (return eoc))
            (let* ((idx (bpc-index c))
                   (buf (bpc-buf c))
                   (slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* idx 2))))
                   (entry (%bpt-decode-page-entry tree buf slot t)))
              (incf (bpc-index c))
              (if (and (bpc-bounded-p c)
                       (bpt-key< tree (bpc-end c) (first entry)))
                  (return eoc)
                  (return (%bpt-materialize tree entry)))))))))

(defmethod make-cursor ((tree bplus-tree) &key &allow-other-keys)
  (with-bpt-read-lock (tree)
    (multiple-value-bind (buf count next-addr) (%bpt-leftmost-leaf tree)
      (make-instance 'bplus-cursor :tree tree :buf buf :count count :index 0
                                   :next-addr next-addr :end nil :bounded-p nil))))

(defmethod make-range-cursor ((tree bplus-tree) start end &key &allow-other-keys)
  (with-bpt-read-lock (tree)
    (multiple-value-bind (buf count next-addr idx) (%bpt-leaf-at tree start)
      (make-instance 'bplus-cursor :tree tree :buf buf :count count :index idx
                                   :next-addr next-addr :end end :bounded-p t))))


;;; ---------------------------------------------------------------------------
;;; Ordered-map generics -- the drop-in protocol (dispatch on BPLUS-TREE)
;;; ---------------------------------------------------------------------------
;;; So views / :unique / spatial (which call ADD/REMOVE/FIND/UPDATE-IN-SKIP-LIST
;;; and MAKE-RANGE-CURSOR) can run against a B+ tree unchanged.  The names keep
;;; the -SKIP-LIST suffix only because that is the existing generic protocol.

(defmethod add-to-skip-list ((tree bplus-tree) key value)
  (bpt-insert tree
              key
              (funcall (%bpt-key-serializer tree) key)
              (funcall (%bpt-value-serializer tree) value)))

(defmethod remove-from-skip-list ((tree bplus-tree) key &optional (value nil value-p))
  (if value-p
      (bpt-remove tree key (funcall (%bpt-value-serializer tree) value))
      (bpt-remove tree key)))

(defmethod find-in-skip-list ((tree bplus-tree) key &optional preds succs)
  (declare (ignore preds succs))
  (with-bpt-read-lock (tree)
    (multiple-value-bind (sval found-p) (%bpt-find tree key)
      (if found-p
          (values (%make-skip-node
                   :key key :value (funcall (%bpt-value-deserializer tree) sval))
                  0)
          (values nil -1)))))

(defmethod update-in-skip-list ((tree bplus-tree) key value &optional old-value)
  (declare (ignore old-value))
  ;; Duplicate-free composite keys: replace = remove + add.
  (bpt-remove tree key)
  (add-to-skip-list tree key value))

;;; ---------------------------------------------------------------------------
;;; Backend-agnostic view-index protocol
;;; ---------------------------------------------------------------------------
;;; A view (and the :unique / spatial indexes) persists a heap-backed ordered map
;;; and reopens it by address.  Both the skip list and the B+ tree qualify; the
;;; in-RAM MEM-SKIP-LIST does NOT (it has no heap pointer and is rebuilt on open).
;;; These generics let VIEWS.LISP treat "which backend" uniformly, so a view's
;;; ordered map can be a skip list or a B+ tree with no other code change.

(defgeneric view-index-p (index)
  (:documentation "True if INDEX is a persistent, heap-backed ordered-map index
(skip list or B+ tree) -- i.e. one that owns a heap address and is freed
explicitly.  NIL for the in-RAM mem-skip-list and everything else.")
  (:method (index) (declare (ignore index)) nil)
  (:method ((index skip-list)) t)
  (:method ((index bplus-tree)) t))

(defgeneric view-index-address (index)
  (:documentation "INDEX's heap address (persisted as the view pointer).")
  (:method ((index skip-list)) (%sl-address index))
  (:method ((index bplus-tree)) (%bpt-address index)))

(defgeneric delete-view-index (index)
  (:documentation "Free INDEX's heap storage.")
  (:method ((index skip-list)) (delete-skip-list index))
  (:method ((index bplus-tree)) (delete-bplus-tree index)))

(defgeneric view-index-backend-tag (index)
  (:documentation "Keyword naming INDEX's backend, persisted so reopen selects
the right opener.")
  (:method ((index skip-list)) :skip-list)
  (:method ((index bplus-tree)) :bplus-tree))

;;; ---------------------------------------------------------------------------
;;; Shared heap-index factory (views + :unique + spatial)
;;; ---------------------------------------------------------------------------
;;; One CREATE and one OPEN for every heap-backed composite-key index, so the
;;; skip-list-vs-B+-tree choice lives in exactly one place.  Defined here (before
;;; spatial-index.lisp / views.lisp) so all three consumers can call it.  The
;;; composite-key codec symbols (VIEW-KEY-SERIALIZE / REDUCE-* -- defined later in
;;; views.lisp) are referenced only as quoted runtime function designators, so
;;; there is no load-order problem.  (*INDEX-BACKEND* -- the DEFAULT backend -- is
;;; defined in globals.lisp so the graph class's INDEX-BACKEND slot can default to it.)

(defun make-heap-index (backend heap comparison)
  "Create a fresh heap-backed composite-key ordered map (skip list or B+ tree) with
the shared view/unique/spatial codec.  COMPARISON is REDUCE-COMP-LESSP or
REDUCE-COMP-GREATERP (it also picks the skip list's head/tail sentinels)."
  (ecase backend
    (:skip-list
     (let ((greaterp (eq comparison 'reduce-comp-greaterp)))
       (make-skip-list
        :heap heap :duplicates-allowed-p nil
        :key-equal 'reduce-equal :key-comparison comparison
        :head-key (if greaterp (list +max-sentinel+ +max-key+) (list +min-sentinel+ +null-key+))
        :head-value nil
        :tail-key (if greaterp (list +min-sentinel+ +null-key+) (list +max-sentinel+ +max-key+))
        :tail-value nil
        :value-equal 'equal
        :key-serializer 'view-key-serialize :key-deserializer 'view-key-deserialize
        :value-serializer 'serialize :value-deserializer 'deserialize)))
    (:bplus-tree
     (make-bplus-tree
      :heap heap :key-equal 'reduce-equal :key-comparison comparison
      :value-equal 'equal
      :key-serializer 'view-key-serialize :key-deserializer 'view-key-deserialize
      :value-serializer 'serialize :value-deserializer 'deserialize))))

(defun open-heap-index (backend &key address heap comparison)
  "Reopen a persisted heap-backed composite-key index at ADDRESS with BACKEND's
opener and the shared codec.  BACKEND defaults to :skip-list for a pre-B+-tree
sidecar/alist with no tag."
  (ecase (or backend :skip-list)
    (:skip-list
     (open-skip-list :address address :heap heap :duplicates-allowed-p nil
                     :key-equal 'reduce-equal :key-comparison comparison
                     :value-equal 'equal
                     :key-serializer 'view-key-serialize :key-deserializer 'view-key-deserialize
                     :value-serializer 'serialize :value-deserializer 'deserialize))
    (:bplus-tree
     (open-bplus-tree :address address :heap heap
                      :key-equal 'reduce-equal :key-comparison comparison
                      :value-equal 'equal
                      :key-serializer 'view-key-serialize :key-deserializer 'view-key-deserialize
                      :value-serializer 'serialize :value-deserializer 'deserialize))))
