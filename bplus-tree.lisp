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
  "Return the address of the leaf that would hold DKEY, binary-searching each
internal node without decoding it wholesale."
  (let ((addr (%bpt-root tree)))
    (loop
      (let ((buf (%bpt-read-page tree addr)))
        (if (%bpt-leaf-p buf)
            (return addr)
            (let ((n (buf-u16 buf +bpt-p-count-offset+)))
              (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
                (declare (ignore exact))
                (setf addr (if (< idx 0)
                               (buf-u64 buf +bpt-p-link-offset+)  ; P0
                               (%bpt-slot-child buf idx))))))))))

;;; ---------------------------------------------------------------------------
;;; Navigation
;;; ---------------------------------------------------------------------------

(defun %bpt-choose-child (tree link entries dkey)
  "For an internal node with leftmost child LINK (P0) and ENTRIES, return the
child address to descend into for DKEY: P0 if DKEY < first key, else the child of
the last entry whose key <= DKEY."
  (let ((child link))
    (dolist (e entries child)
      (if (bpt-key< tree dkey (first e))
          (return child)
          (setf child (third e))))))

(defun %bpt-descend-to-leaf (tree dkey)
  "Return (values LEAF-ADDR LEAF-BUF LEAF-LINK LEAF-ENTRIES) for the leaf that
would hold DKEY.  Internal nodes are binary-searched (not decoded); only the
final leaf is decoded into entries (its caller needs them)."
  (let* ((addr (%bpt-descend-leaf-addr tree dkey))
         (buf (%bpt-read-page tree addr)))
    (multiple-value-bind (leaf-p link entries) (%bpt-decode-page tree buf)
      (declare (ignore leaf-p))
      (values addr buf link entries))))

;;; ---------------------------------------------------------------------------
;;; Find
;;; ---------------------------------------------------------------------------

(defun %bpt-find (tree dkey)
  "Return (values SVAL FOUND-P) for DKEY -- SVAL is the raw serialized value.
Fully lean: binary-search all the way down, decode only the one matching cell."
  (let ((addr (%bpt-descend-leaf-addr tree dkey)))
    (let* ((buf (%bpt-read-page tree addr))
           (n (buf-u16 buf +bpt-p-count-offset+)))
      (multiple-value-bind (idx exact) (%bpt-page-bsearch tree buf n dkey)
        (if exact
            (let* ((slot (buf-u16 buf (+ +bpt-p-slots-offset+ (* idx 2))))
                   (voff (+ slot 2 (buf-u16 buf slot)))
                   (vlen (buf-u16 buf voff)))
              (values (subseq buf (+ voff 2) (+ voff 2 vlen)) t))
            (values nil nil))))))

;;; ---------------------------------------------------------------------------
;;; Insert (recursive, with split propagation)
;;; ---------------------------------------------------------------------------
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

(defun %bpt-insert (tree addr dkey skey sval)
  (multiple-value-bind (leaf-p link entries)
      (%bpt-decode-page tree (%bpt-read-page tree addr))
    (if leaf-p
        (let ((new (%bpt-insert-sorted-leaf tree entries dkey skey sval)))
          (if (eq new :dup)
              :dup
              (%bpt-store-or-split tree addr t link new)))
        (let ((child (%bpt-choose-child tree link entries dkey)))
          (let ((res (%bpt-insert tree child dkey skey sval)))
            (cond ((eq res :dup) :dup)
                  ((eq res :no-split) :no-split)
                  (t ;; child split -> insert separator here
                   (destructuring-bind (sep-dkey sep-skey right-addr) res
                     (let ((new (%bpt-insert-sorted-internal
                                 tree entries sep-dkey sep-skey right-addr)))
                       (%bpt-store-or-split tree addr nil link new))))))))))

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
;;; Remove (lazy: drop the leaf cell, no merge/rebalance)
;;; ---------------------------------------------------------------------------

(defun bpt-remove (tree dkey &optional (sval nil sval-p) value-equal)
  "Remove DKEY from its leaf.  With SVAL given, only remove when the stored value
also matches (via VALUE-EQUAL or the tree's).  Returns T if a key was removed."
  (with-bpt-write-lock (tree)
    (multiple-value-bind (addr buf link entries) (%bpt-descend-to-leaf tree dkey)
      (declare (ignore buf))
      (let ((veq (or value-equal (%bpt-value-equal tree)))
            (found nil))
        (let ((kept (remove-if (lambda (e)
                                 (when (and (not found)
                                            (bpt-key= tree dkey (first e))
                                            (or (not sval-p)
                                                (funcall veq sval (third e))))
                                   (setf found t)))
                               entries)))
          (when found
            ;; Leaf never overflows on removal, so encode always fits.
            (%bpt-write-page tree addr (%bpt-encode-page tree t link kept))
            (decf (%bpt-count tree))
            (%bpt-sync-count tree))
          found)))))

;;; ---------------------------------------------------------------------------
;;; Cursors -- leaf-linked forward scan (the range-scan locality payoff)
;;; ---------------------------------------------------------------------------

(defclass bplus-cursor (cursor)
  ((tree :initarg :tree :accessor bpc-tree)
   (entries :initarg :entries :accessor bpc-entries)   ; vector of current-leaf entries
   (index :initarg :index :accessor bpc-index)
   (next-addr :initarg :next-addr :accessor bpc-next-addr) ; link of current leaf
   (end :initarg :end :accessor bpc-end)               ; deserialized end key, or NIL
   (bounded-p :initarg :bounded-p :accessor bpc-bounded-p)))

(defun %bpt-leftmost-leaf (tree)
  "Return (values ENTRIES-VECTOR NEXT-ADDR) for the first (leftmost) leaf."
  (let ((addr (%bpt-root tree)))
    (loop
      (multiple-value-bind (leaf-p link entries)
          (%bpt-decode-page tree (%bpt-read-page tree addr))
        (if leaf-p
            (return (values (coerce entries 'vector) link))
            (setf addr link))))))          ; leftmost child == P0 == link

(defun %bpt-leaf-at (tree dkey)
  "Return (values ENTRIES-VECTOR NEXT-ADDR START-INDEX) positioned at the first
entry with key >= DKEY in the leaf that would hold DKEY."
  (multiple-value-bind (addr buf link entries) (%bpt-descend-to-leaf tree dkey)
    (declare (ignore addr buf))
    (let ((v (coerce entries 'vector)) (idx 0))
      (loop while (and (< idx (length v))
                       (bpt-key< tree (first (aref v idx)) dkey))
            do (incf idx))
      (values v link idx))))

(defun %bpt-materialize (tree entry)
  "Build a SKIP-NODE from a leaf ENTRY so consumers (views/spatial/unique) read
%SN-KEY / %SN-VALUE unchanged."
  (%make-skip-node :key (first entry)
                   :value (funcall (%bpt-value-deserializer tree) (third entry))))

(defmethod cursor-next ((c bplus-cursor) &optional eoc)
  (with-bpt-read-lock ((bpc-tree c))
    (let ((tree (bpc-tree c)))
      (loop
        (when (>= (bpc-index c) (length (bpc-entries c)))
          ;; Advance to the next leaf via the sibling link (sequential pages).
          (if (zerop (bpc-next-addr c))
              (return eoc)
              (multiple-value-bind (leaf-p link entries)
                  (%bpt-decode-page tree (%bpt-read-page tree (bpc-next-addr c)))
                (declare (ignore leaf-p))
                (setf (bpc-entries c) (coerce entries 'vector)
                      (bpc-index c) 0
                      (bpc-next-addr c) link))))
        (if (>= (bpc-index c) (length (bpc-entries c)))
            (when (zerop (bpc-next-addr c)) (return eoc))  ; empty trailing leaf
            (let ((e (aref (bpc-entries c) (bpc-index c))))
              (incf (bpc-index c))
              (if (and (bpc-bounded-p c)
                       (bpt-key< tree (bpc-end c) (first e)))  ; key > end -> stop
                  (return eoc)
                  (return (%bpt-materialize tree e)))))))))

(defmethod make-cursor ((tree bplus-tree) &key &allow-other-keys)
  (with-bpt-read-lock (tree)
    (multiple-value-bind (entries next-addr) (%bpt-leftmost-leaf tree)
      (make-instance 'bplus-cursor :tree tree :entries entries :index 0
                                   :next-addr next-addr :end nil :bounded-p nil))))

(defmethod make-range-cursor ((tree bplus-tree) start end &key &allow-other-keys)
  (with-bpt-read-lock (tree)
    (multiple-value-bind (entries next-addr idx) (%bpt-leaf-at tree start)
      (make-instance 'bplus-cursor :tree tree :entries entries :index idx
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
