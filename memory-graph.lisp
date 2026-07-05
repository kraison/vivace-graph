(in-package :graph-db)

;;;; In-memory storage backend for VivaceGraph (`memory-graph`).
;;;;
;;;; A general-purpose, explicitly-constructed in-memory graph: nodes are live
;;;; objects in a hash table (no mmap heap, no per-read deserialization), and the
;;;; adjacency indexes are in-RAM mirrors of the ve/vev/type-index shapes.
;;;; Durability is the SAME journal (.txn) + snapshot machinery the on-disk graph
;;;; uses -- the heap was always just a materialized cache of the journal.
;;;;
;;;; This is Phase 2, Step 1 (see docs/memory-graph-design.md and GH #50): the
;;;; class + table skeleton + MAKE-MEMORY-GRAPH that constructs and closes
;;;; cleanly.  Node-ops, indexes, views and the spatial index arrive in the
;;;; following increments.
;;;;
;;;; Design decisions in force (Kevin, 2026-07-03):
;;;;   1. Dispatch by overriding the generic node-ops; the lhash/allocator/mmap
;;;;      path is left untouched.  MEMORY-GRAPH-MIXIN is the dispatch hook.
;;;;   2. Adjacency = in-RAM mirror of ve/vev/type-index (NOT edges on vertices).
;;;;   3. Durability = full journal (reuse .txn + recover-transactions).
;;;;   4. Views + spatial are first-class, rebuilt in-RAM on open (v1).
;;;;   MVCC version chains are dropped on a memory-graph (single-writer, atomic
;;;;   node-swap, lock-free reads); the WITH-READ-SNAPSHOT API is kept.

;;; ---------------------------------------------------------------------------
;;; mem-table -- the in-RAM replacement for an LHASH vertex/edge table:
;;; id (16-byte uuid, EQUALP) -> live node object.  EQUALP keys are the
;;; portable choice across SBCL/CCL/ECL (uuid arrays compare by content), the
;;; same convention the algorithms projection uses.
;;; ---------------------------------------------------------------------------

(defstruct (mem-table (:constructor %make-mem-table) (:predicate mem-table-p))
  (data (make-hash-table :test 'equalp
                         #+sbcl :synchronized #+sbcl t
                         #+ccl :shared #+ccl t
                         #+lispworks :single-thread #+lispworks nil))
  ;; Writes are single-threaded (the transaction-manager lock serializes commit
  ;; apply), so this lock only guards the rare non-commit mutation; reads are
  ;; lock-free and see a whole node via atomic slot replacement.
  (lock (make-recursive-lock "mem-table"))
  ;; Back-references for lazy (fault-on-access) materialization: KIND (:vertex or
  ;; :edge) picks the node constructor, GRAPH gives the schema for type-id->class.
  ;; A lazy table's values are LZNODEs (parsed head + deferred data blob) until
  ;; first touch; see MEM-MATERIALIZE.  Both stay NIL for a non-lazy table.
  (kind nil)
  (graph nil))

(declaim (inline make-mem-table mem-table-get mem-table-put mem-table-rem))
(defun make-mem-table (&key kind graph) (%make-mem-table :kind kind :graph graph))

(defun mem-table-get (table key)
  "The node stored under KEY, or NIL."
  (values (gethash key (mem-table-data table))))

(defun mem-table-put (table key node)
  "Publish NODE under KEY (atomic slot replacement; lock-free readers see the
whole old or new node, never a torn one)."
  (setf (gethash key (mem-table-data table)) node))

(defun mem-table-rem (table key)
  (remhash key (mem-table-data table)))

(defun mem-table-count (table)
  (hash-table-count (mem-table-data table)))

(defun map-mem-table (fn table &key collect-p)
  "Call FN on each node in TABLE.  With COLLECT-P, return the list of results."
  (let ((acc '()))
    (maphash (lambda (k v) (declare (ignore k))
               (let ((r (funcall fn v))) (when collect-p (push r acc))))
             (mem-table-data table))
    (when collect-p (nreverse acc))))

;;; ---------------------------------------------------------------------------
;;; Classes.  Backend (memory) and replication role are orthogonal axes, so the
;;; backend is a MIXIN: node-op methods specialize on MEMORY-GRAPH-MIXIN (more
;;; specific than the base GRAPH methods, so they win), while replication methods
;;; keep specializing on the role classes.
;;;   memory-graph        -- standalone, no replication
;;;   memory-peer-graph   -- in-memory device/hub peer (added when the peer path
;;;                          is wired; declared here so the lattice is explicit)
;;; ---------------------------------------------------------------------------

(defclass memory-graph-mixin ()
  ((lazy-p :initarg :lazy :accessor lazy-p :initform nil
           :documentation "When true, OPEN restores nodes as LZNODE blobs (parsed
head + deferred data) and materializes each live node only on first touch
(fault-on-access) -- open pays no MAKE-INSTANCE, which is ~85% of eager open cost
on ECL (#50).  Implies the VG-native image format (per-node blobs); the default
(NIL) keeps the eager cl-store path unchanged."))
  (:documentation "Storage-backend mixin marking a graph whose tables/indexes/
views live in RAM as live objects rather than in mmap'd files.  The node-op
generics dispatch on this class."))

(defclass memory-graph (memory-graph-mixin graph)
  ()
  (:documentation "A standalone in-memory graph (no replication)."))

;; The in-memory peer: memory storage backend x peer replication role.  The peer
;; transport (peer-sync / peer-writer-loop / peer-enqueue-write, in
;; graph-db/replication) dispatches on PEER-GRAPH, and every node mutation funnels
;; through the shared apply seam (apply-transaction / apply-tx-write), so the
;; memory apply methods (specialized on MEMORY-GRAPH-MIXIN) maintain the in-RAM
;; store for both pulled and locally-authored ops.  The peer's own durable state
;; (applied-op-ids lhash, lamport / field-stamps / conflict files) stays
;; file-backed, orthogonal to the in-RAM node storage.
(defclass memory-peer-graph (memory-graph-mixin peer-graph)
  ()
  (:documentation "An in-memory peer-graph (hub or device)."))

(defgeneric memory-graph-p (thing)
  (:method ((graph memory-graph-mixin)) graph)
  (:method (thing) (declare (ignore thing)) nil))

;;; ---------------------------------------------------------------------------
;;; Construction.  MAKE-MEMORY-GRAPH mirrors MAKE-GRAPH's wiring (schema,
;;; transaction-manager, journal, .dirty, registration) but builds in-RAM tables
;;; instead of mmap heap/lhash storage.  Indexes/views/spatial are wired in the
;;; following increments; for now their slots hold NIL, which CLOSE-GRAPH's
;;; type-guarded teardown tolerates.
;;; ---------------------------------------------------------------------------

(defun make-mem-spatial-index (&key (precision 7))
  "A spatial-index whose skip-list slot is an in-RAM mem-skip-list, keyed by the
same composite (cell . node-id) as the on-disk index (duplicate-free).  Every
spatial op -- insert / remove / query-bbox / query-radius -- goes through
spatial-index-skip-list -> add-to-skip-list / make-range-cursor, which dispatch
to the mem list, so all of spatial-index.lisp (and the base
apply-tx-write-to-spatial-index maintenance) runs UNCHANGED on a memory-graph.
The composite key also removes the duplicate-key %mem-find overshoot that made
in-RAM REMOVE silently drop the wrong node."
  (%make-spatial-index
   :skip-list (make-mem-skip-list :key-comparison 'reduce-comp-lessp
                                  :key-equal 'reduce-equal
                                  :value-equal 'equal :duplicates-allowed-p nil
                                  :head-key (list +min-sentinel+ +null-key+) :head-value nil
                                  :tail-key (list +max-sentinel+ +max-key+)  :tail-value nil)
   :heap nil :precision precision))

(defun %make-empty-memory-graph (name location &key (class 'memory-graph)
                                                 replication-key replication-port
                                                 lazy)
  "A fresh CLASS shell with empty in-RAM tables (no schema/txn wiring yet).
Shared by MAKE-MEMORY-GRAPH and OPEN-MEMORY-GRAPH; CLASS is MEMORY-GRAPH or
MEMORY-PEER-GRAPH.  With LAZY, the node tables materialize on first touch."
  (let ((graph (make-instance class
                              :graph-name name
                              :location location
                              :lazy lazy
                              :replication-key replication-key
                              :replication-port replication-port
                              :views
                              #+sbcl (make-hash-table :synchronized t)
                              #+ccl (make-hash-table :shared t)
                              #+lispworks (make-hash-table :single-thread nil)
                              #+ecl (make-hash-table)
                              :cache (make-id-table :synchronized t :weakness :value)
                              :vertex-table (make-mem-table)
                              :edge-table (make-mem-table)
                              :heap nil
                              :indexes nil
                              :ve-index-in (make-mem-ve-index)
                              :ve-index-out (make-mem-ve-index)
                              :vev-index (make-mem-vev-index)
                              :vertex-index (make-mem-type-index)
                              :edge-index (make-mem-type-index)
                              :spatial-index (make-mem-spatial-index))))
    ;; Wire the node tables back to the graph + their kind, so lazy materialization
    ;; (MEM-MATERIALIZE) is self-contained (needs the schema + the right ctor).
    (setf (mem-table-kind (vertex-table graph)) :vertex
          (mem-table-graph (vertex-table graph)) graph
          (mem-table-kind (edge-table graph)) :edge
          (mem-table-graph (edge-table graph)) graph)
    graph))

(defun %write-dirty-marker (path)
  (with-open-file (out (format nil "~A/.dirty" path) :direction :output
                                                     :if-exists :supersede)
    (format out "~S" (get-universal-time))))

(defun %validate-peer-role (peer-role origin-id)
  (when (and peer-role (not (member peer-role '(:hub :device))))
    (error ":PEER-ROLE must be :HUB or :DEVICE, got ~S" peer-role))
  (when (and (eq peer-role :device) (null origin-id))
    (error "a :DEVICE memory-peer-graph requires a hub-minted :ORIGIN-ID")))

(defun %init-memory-peer-slots (graph mode path peer-role origin-id peer-host
                                export-predicate device-registry merge-policy
                                reference-classes peer-schema-version)
  "Populate a memory-peer-graph's peer slots, mirroring MAKE-GRAPH/OPEN-GRAPH's
peer branch.  The peer state is file/lhash-backed (orthogonal to the in-RAM node
store): the durable Lamport clock, per-field stamps, conflict records, and the
applied-op-id dedup lhash (opened when MODE is :OPEN and it already exists)."
  (setf (peer-role graph) peer-role
        (origin-id graph) origin-id
        (peer-host graph) peer-host
        (export-predicate graph) export-predicate
        (merge-policy graph) merge-policy
        (device-registry graph) device-registry
        (reference-classes graph) reference-classes
        (peer-schema-version graph) peer-schema-version
        (lamport-counter graph) (load-lamport-counter graph)
        (field-stamps graph) (load-field-stamps graph)
        (node-origins graph) (load-node-origins graph)   ; #6 :ORIGIN partitions
        (peer-conflicts graph) (load-peer-conflicts graph)
        (applied-op-ids graph)
        (let ((loc (format nil "~A/applied-ops/" path)))
          (if (and (eq mode :open) (probe-file (format nil "~Astruct.dat" loc)))
              (open-lhash loc)
              (make-lhash :location loc :buckets 8)))))

(defun make-memory-graph (name location
                          &key package replication-port replication-key
                            peer-role origin-id peer-host lazy
                            export-predicate device-registry merge-policy
                            reference-classes (peer-schema-version '(1 0)))
  "Create a brand-new in-memory graph named NAME.  LOCATION is used for the
durable journal, cl-store image and schema (the RAM structures are rebuilt from
them on OPEN-MEMORY-GRAPH).  With :PEER-ROLE (:HUB or :DEVICE, a :DEVICE also
needs a hub-minted :ORIGIN-ID) a MEMORY-PEER-GRAPH is built and the peer
replication path is wired.  With :LAZY, the graph uses the VG-native image format
and materializes nodes on first touch (fault-on-access) for a near-instant open.
Registers the graph and returns it."
  (%validate-peer-role peer-role origin-id)
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (graph (%make-empty-memory-graph
                 name path
                 :class (if peer-role 'memory-peer-graph 'memory-graph)
                 :lazy lazy
                 :replication-key replication-key
                 :replication-port replication-port)))
    (let ((*graph* graph))
      (init-schema graph)
      (update-schema graph)
      (%write-dirty-marker path)
      (setf (gethash name *graphs*) graph))
    (when peer-role
      (%init-memory-peer-slots graph :make path peer-role origin-id peer-host
                               export-predicate device-registry merge-policy
                               reference-classes peer-schema-version))
    (setf (transaction-manager graph)
          (make-instance 'transaction-manager :graph graph))
    (ensure-directories-exist (persistent-transaction-directory graph))
    (init-replication-log graph)
    (start-replication graph :package package)
    (setf (graph-open-p graph) t)
    graph))

;;; ---------------------------------------------------------------------------
;;; Durability (design §7).  Three tiers, all reusing the existing journal:
;;;
;;;   * .txn WAL   -- retained per commit (RETAIN-COMMITTED-TRANSACTION-P => T),
;;;                   so it survives a crash; the memory backend has no heap, so
;;;                   the journal is the only durable record between checkpoints.
;;;   * cl-store image (graph.img) -- a whole-tables pickle written at each clean
;;;                   close and restored in one CL-STORE:RESTORE on open.  This is
;;;                   the fast clean-open path; it is LOCAL only (cl-store is not
;;;                   portable across Lisp implementations), so it is an
;;;                   optimization, never the interchange format.
;;;   * s-expr snapshot -- the portable/cross-impl compaction path (RECREATE-GRAPH),
;;;                   wired in a later increment.
;;;
;;; INVARIANT: the journal holds exactly the committed transactions applied SINCE
;;; the last image.  A clean-close checkpoint writes a new image (the full current
;;; RAM state) and THEN clears the journal, so OPEN = restore image + replay the
;;; (now-empty-after-clean-close, non-empty-after-crash) journal tail, with no
;;; double apply.  Replay is idempotent regardless (atomic mem-table-put), so a
;;; crash between image-write and journal-clear is safe.
;;; ---------------------------------------------------------------------------

(defmethod retain-committed-transaction-p ((graph memory-graph-mixin))
  (declare (ignore graph))
  t)

(defun memory-image-file (location)
  (format nil "~A/graph.img" location))

;;; Derived-structure persistence (#50 / mine-action perf): rebuilding the derived
;;; structures on open -- above all the aggregate (reduce) VIEWS -- is the dominant
;;; on-device open cost (~23 s for the app's eo-find rollups, paid every open).  So
;;; the image pickles them too, as FLAT dumps (no mem struct with its rw-lock /
;;; function refs goes on the wire), and open restores them STRUCTURALLY -- direct
;;; skip-list / index inserts, no map / reduce / geohash recompute.

(defvar *memory-image-view-dump* :none
  "Bound during OPEN-MEMORY-GRAPH to the image's per-view entry dumps (or :NONE
for a v1 / absent image), so RESTORE-VIEWS repopulates the view skip-lists
structurally instead of regenerating them.")

;; Unique constraints (#6): the dump/load helpers live in unique-constraint.lisp
;; (loaded after this file); forward-declared so the image codec here compiles clean.
;; The "was it loaded?" flag is defined HERE because OPEN-MEMORY-GRAPH binds it.
(declaim (ftype (function (t) t) %dump-unique-indexes rebuild-unique-indexes)
         (ftype (function (t t) t) %load-unique-indexes))
(defvar *memory-image-unique-loaded* nil
  "Bound NIL by OPEN-MEMORY-GRAPH; set T by the image restore when the unique-index
dump was loaded, so OPEN skips the fresh-graph / crash REBUILD-UNIQUE-INDEXES.")

(defun %dump-mem-index (data-ht)
  "Flat (key . id-list) dump of a mem-*-index's DATA hashtable."
  (let ((acc '()))
    (maphash (lambda (k il)
               (let ((ids '()))
                 (maphash (lambda (id v) (declare (ignore v)) (push id ids))
                          (mem-index-list-ids il))
                 (push (cons k ids) acc)))
             data-ht)
    acc))

(defun %load-mem-index (data-ht dump)
  (dolist (pair dump)
    (let ((il (make-mem-index-list)))
      (dolist (id (cdr pair)) (mem-index-list-add il id))
      (setf (gethash (car pair) data-ht) il))))

(defun %dump-mem-skip-list (sl)
  "In-order (key . value) dump of a mem-skip-list (the spatial grid, a view)."
  (let ((c (make-cursor sl)) (acc '()))
    (loop for node = (cursor-next c :eoc) until (eq node :eoc)
          do (push (cons (%sn-key node) (%sn-value node)) acc))
    (nreverse acc)))

(defun %load-mem-skip-list (sl entries)
  (dolist (e entries) (add-to-skip-list sl (car e) (cdr e))))

(defun %dump-views (graph)
  "Per-view (class-name view-name entries) for every mem-backed view."
  (let ((acc '()))
    (maphash (lambda (class-name view-group)
               (maphash (lambda (view-name view)
                          (when (mem-skip-list-p (view-skip-list view))
                            (push (list class-name view-name
                                        (%dump-mem-skip-list (view-skip-list view)))
                                  acc)))
                        (view-group-table view-group)))
             (views graph))
    acc))

;;; ===========================================================================
;;; VG-native image format (v3) + fault-on-access (lazy) materialization.
;;;
;;; WHY a second format: on ECL, restoring the image is ~85% MAKE-INSTANCE cost
;;; (building the live CLOS nodes), ~15% deserialize, ~0.5% byte-parse -- so the
;;; wire format is NOT the lever (a cl-store->native swap is a wash).  The lever is
;;; NOT building the nodes you don't touch.  cl-store can't do that (it inlines a
;;; whole instance per node); the native format stores each node as a compact
;;; record -- head fields packed raw + the SAME data blob VG's on-disk heap writes
;;; ((serialize (data node))) -- so open can stop at the blob (an LZNODE) and defer
;;; MAKE-INSTANCE to first touch.  SERIALIZE-MULTIPLE is O(k^2) per call, so every
;;; SERIALIZE here is on ONE small object (a node's data alist / one key / value),
;;; never the aggregate.
;;; ===========================================================================

;;; ---- write: growable little-endian byte buffer ----
(defun ni-mkbuf () (make-array 65536 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
(declaim (inline ni-u8 ni-uint ni-bytes ni-blob ni-lisp))
(defun ni-u8 (buf b) (vector-push-extend b buf))
(defun ni-uint (buf n nbytes)
  (dotimes (i nbytes) (vector-push-extend (ldb (byte 8 (* i 8)) n) buf)))
(defun ni-bytes (buf vec)
  (let ((n (length vec))) (dotimes (i n) (vector-push-extend (aref vec i) buf))))
(defun ni-blob (buf vec) (ni-uint buf (length vec) 4) (ni-bytes buf vec))
(defun ni-lisp (buf val) (ni-blob buf (serialize val)))

;;; ---- read: cursor over a byte array ----
(defstruct (ric (:constructor ni-ric (bytes))) bytes (i 0 :type fixnum))
(declaim (inline ri-u8 ri-uint ri-bytes ri-blob ri-lisp))
(defun ri-u8 (rc) (prog1 (aref (ric-bytes rc) (ric-i rc)) (incf (ric-i rc))))
(defun ri-uint (rc nbytes)
  (let ((n 0) (i (ric-i rc)) (b (ric-bytes rc)))
    (dotimes (k nbytes) (setf n (dpb (aref b (+ i k)) (byte 8 (* k 8)) n)))
    (setf (ric-i rc) (+ i nbytes)) n))
(defun ri-bytes (rc n)
  (let* ((i (ric-i rc)) (v (subseq (ric-bytes rc) i (+ i n))))
    (setf (ric-i rc) (+ i n)) v))
(defun ri-blob (rc) (ri-bytes rc (ri-uint rc 4)))
(defun ri-lisp (rc) (deserialize (ri-blob rc)))

;;; ---- LZNODE: a not-yet-materialized node.  Parsed head (cheap; ~0.5% of open)
;;; + the deferred data blob.  Deliberately NOT a node instance -- MAKE-INSTANCE is
;;; the ~85% cost we defer to first touch (MEM-MATERIALIZE).  A lazy mem-table's
;;; values are LZNODEs until touched, then swapped for the live node.
(defstruct (lznode (:constructor make-lznode) (:predicate lznode-p))
  (type-id 0) (deleted-p nil) (revision 0) (commit-epoch 0)
  from to (weight 1.0) (data-blob nil))

;;; node record = raw head fields + length-prefixed data blob (edges add from/to/
;;; weight before data).  Writer handles a live node OR an LZNODE (untouched nodes
;;; pass their blob straight through -- no re-serialize on checkpoint).
(defun ni-node (buf id x edge-p)
  (if (lznode-p x)
      (progn
        (ni-uint buf (lznode-type-id x) 2) (ni-blob buf id)
        (ni-u8 buf (if (lznode-deleted-p x) 1 0))
        (ni-uint buf (lznode-revision x) 4) (ni-uint buf (lznode-commit-epoch x) 8)
        (when edge-p (ni-blob buf (lznode-from x)) (ni-blob buf (lznode-to x))
              (ni-lisp buf (lznode-weight x)))
        (ni-blob buf (lznode-data-blob x)))
      (progn
        (ni-uint buf (type-id x) 2) (ni-blob buf id)
        (ni-u8 buf (if (deleted-p x) 1 0))
        (ni-uint buf (revision x) 4) (ni-uint buf (commit-epoch x) 8)
        (when edge-p (ni-blob buf (from x)) (ni-blob buf (to x)) (ni-lisp buf (weight x)))
        (ni-blob buf (serialize (data x))))))

(defun ri-node (rc edge-p)
  "Read one record into (values ID LZNODE) -- head parsed, data blob deferred."
  (let* ((type-id (ri-uint rc 2)) (id (ri-blob rc)) (del (= 1 (ri-u8 rc)))
         (rev (ri-uint rc 4)) (ce (ri-uint rc 8))
         (from (when edge-p (ri-blob rc))) (to (when edge-p (ri-blob rc)))
         (weight (if edge-p (ri-lisp rc) 1.0))
         (blob (ri-blob rc)))
    (values id (make-lznode :type-id type-id :deleted-p del :revision rev
                            :commit-epoch ce :from from :to to :weight weight
                            :data-blob blob))))

;;; ---- materialize: LZNODE -> live node (the deferred MAKE-INSTANCE + deserialize)
(defun %lznode->node (table id lz)
  (let* ((graph (mem-table-graph table))
         (edge-p (eq (mem-table-kind table) :edge))
         (type-id (lznode-type-id lz))
         (class (node-type-name
                 (lookup-node-type-by-id type-id (if edge-p :edge :vertex) :graph graph)))
         (blob (lznode-data-blob lz))
         (data (when blob (deserialize blob))))
    (if edge-p
        (%make-edge :id id :type-id type-id :revision (lznode-revision lz)
                    :commit-epoch (lznode-commit-epoch lz) :deleted-p (lznode-deleted-p lz)
                    :from (lznode-from lz) :to (lznode-to lz) :weight (lznode-weight lz)
                    :data data :bytes blob :written-p t :data-pointer 0 :class class)
        (%make-vertex :id id :type-id type-id :revision (lznode-revision lz)
                      :commit-epoch (lznode-commit-epoch lz) :deleted-p (lznode-deleted-p lz)
                      :data data :bytes blob :written-p t :data-pointer 0 :class class))))

(defun mem-materialize (table id lz)
  "Build the live node from LZ and atomically swap it into TABLE under ID (so later
lookups return the live object).  Returns the node."
  (let ((node (%lznode->node table id lz)))
    (mem-table-put table id node)
    node))

;;; ---- derived-structure sections (id-based; identical whether lazy or eager) ----
(defun ni-index (buf dump keyfn)     ; dump = list of (key . id-list)
  (ni-uint buf (length dump) 4)
  (dolist (pair dump)
    (funcall keyfn buf (car pair))
    (let ((ids (cdr pair)))
      (ni-uint buf (length ids) 4)
      (dolist (id ids) (ni-blob buf id)))))
(defun ri-index (rc keyrd)
  (let ((n (ri-uint rc 4)) (acc '()))
    (dotimes (i n)
      (let ((key (funcall keyrd rc)) (m (ri-uint rc 4)) (ids '()))
        (dotimes (j m) (push (ri-blob rc) ids))
        (push (cons key (nreverse ids)) acc)))
    (nreverse acc)))
(defun ni-key-type (buf k) (ni-uint buf k 2))
(defun ri-key-type (rc) (ri-uint rc 2))
(defun ni-key-ve (buf k) (ni-blob buf (ve-key-id k)) (ni-uint buf (ve-key-type-id k) 2))
(defun ri-key-ve (rc) (let ((id (ri-blob rc)) (ti (ri-uint rc 2))) (make-ve-key :id id :type-id ti)))
(defun ni-key-vev (buf k)
  (ni-blob buf (vev-key-out-id k)) (ni-blob buf (vev-key-in-id k)) (ni-uint buf (vev-key-type-id k) 2))
(defun ri-key-vev (rc)
  (let ((o (ri-blob rc)) (in (ri-blob rc)) (ti (ri-uint rc 2)))
    (make-vev-key :out-id o :in-id in :type-id ti)))
;; Tagged value codec for spatial values / view keys: VG's SERIALIZE cannot
;; round-trip a bare (unsigned-byte 8) array (ids/uuids -- the on-disk path always
;; handles those as raw bytes), and view keys are composite lists that CONTAIN id
;; arrays.  So encode byte arrays as raw blobs (tag 1), lists element-wise (tag 2),
;; and everything else via SERIALIZE (tag 3).
(defun ni-val (buf v)
  (cond
    ((typep v '(array (unsigned-byte 8) (*))) (ni-u8 buf 1) (ni-blob buf v))
    ((and (consp v) (null (cdr (last v))))    (ni-u8 buf 2) (ni-uint buf (length v) 4)
                                              (dolist (e v) (ni-val buf e)))
    ((null v)                                 (ni-u8 buf 2) (ni-uint buf 0 4))
    (t                                        (ni-u8 buf 3) (ni-blob buf (serialize v)))))
(defun ri-val (rc)
  (ecase (ri-u8 rc)
    (1 (ri-blob rc))
    (2 (let ((n (ri-uint rc 4))) (loop repeat n collect (ri-val rc))))
    (3 (deserialize (ri-blob rc)))))

(defun ni-pairs (buf dump)           ; skip-list dump = list of (key . value)
  (ni-uint buf (length dump) 4)
  (dolist (p dump) (ni-val buf (car p)) (ni-val buf (cdr p))))
(defun ri-pairs (rc)
  (let ((n (ri-uint rc 4)) (acc '()))
    (dotimes (i n) (push (cons (ri-val rc) (ri-val rc)) acc))
    (nreverse acc)))
(defun ni-views (buf graph)
  (let ((dump (%dump-views graph)))
    (ni-uint buf (length dump) 4)
    (dolist (vd dump)
      (destructuring-bind (class-name view-name entries) vd
        (ni-lisp buf class-name) (ni-lisp buf view-name) (ni-pairs buf entries)))))
(defun ri-views (rc)
  (let ((n (ri-uint rc 4)) (acc '()))
    (dotimes (i n) (push (list (ri-lisp rc) (ri-lisp rc) (ri-pairs rc)) acc))
    (nreverse acc)))

(defparameter *native-image-magic* #(86 71 77 73)) ; "VGMI"

(defun %native-image-p (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (and (>= (file-length s) 4)
         (loop for b across *native-image-magic* always (eql b (read-byte s))))))

(defun write-memory-image-native (graph)
  "Write GRAPH's full state in the VG-native (v3) format: per-node blob records +
the same structural derived dumps.  Untouched LZNODEs pass their blob through."
  (let ((buf (ni-mkbuf)))
    (ni-bytes buf *native-image-magic*) (ni-uint buf 5 4)   ; v4 added :unique; v5 = composite-key spatial pairs
    (ni-uint buf (load-highest-transaction-id graph) 8)
    (let ((vt (mem-table-data (vertex-table graph)))
          (et (mem-table-data (edge-table graph))))
      (ni-uint buf (hash-table-count vt) 4)
      (maphash (lambda (id x) (ni-node buf id x nil)) vt)
      (ni-uint buf (hash-table-count et) 4)
      (maphash (lambda (id x) (ni-node buf id x t)) et))
    (ni-index buf (%dump-mem-index (mem-type-index-data (vertex-index graph))) #'ni-key-type)
    (ni-index buf (%dump-mem-index (mem-type-index-data (edge-index graph)))   #'ni-key-type)
    (ni-index buf (%dump-mem-index (mem-ve-index-data (ve-index-in graph)))    #'ni-key-ve)
    (ni-index buf (%dump-mem-index (mem-ve-index-data (ve-index-out graph)))   #'ni-key-ve)
    (ni-index buf (%dump-mem-index (mem-vev-index-data (vev-index graph)))     #'ni-key-vev)
    (let ((idx (spatial-index graph)))
      (ni-pairs buf (if idx (%dump-mem-skip-list (spatial-index-skip-list idx)) '())))
    (ni-views buf graph)
    (ni-val buf (%dump-unique-indexes graph))   ; unique constraints (#6)
    (with-open-file (s (memory-image-file (location graph)) :direction :output
                       :element-type '(unsigned-byte 8) :if-exists :supersede
                       :if-does-not-exist :create)
      (write-sequence buf s))))

(defun restore-memory-image-native (graph file)
  "Restore a VG-native (v3) image.  When GRAPH is LAZY, node tables receive LZNODE
blobs (no MAKE-INSTANCE -- fault-on-access); otherwise each node is materialized
now.  Indexes/spatial/views are restored structurally either way.  Returns
:STRUCTURAL and stashes the view dump in *MEMORY-IMAGE-VIEW-DUMP*."
  (let* ((bytes (with-open-file (s file :element-type '(unsigned-byte 8))
                  (let ((a (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence a s) a)))
         (rc (ni-ric bytes))
         (lazy (lazy-p graph))
         (vtable (vertex-table graph))
         (etable (edge-table graph)))
    (ri-bytes rc 4)                                ; magic
    (let ((ver (ri-uint rc 4)))                    ; format version
      ;; v5 changed the spatial dump to composite (cell . id) keys AND added the
      ;; :unique dump -- a mid-stream layout change that cannot be read positionally
      ;; by older parsers or vice versa.  Reject a stale image loudly rather than
      ;; misparse it; the memory graph then rebuilds from its transaction journal.
      (unless (= ver 5)
        (error "Unsupported memory-image format v~D (this build writes/reads v5). ~
Delete the stale image at ~A and reopen to rebuild from the journal."
               ver file)))
    (ri-uint rc 8)                                 ; highest-tx-id
    (let ((nv (ri-uint rc 4)))
      (dotimes (i nv)
        (multiple-value-bind (id lz) (ri-node rc nil)
          (mem-table-put vtable id (if lazy lz (%lznode->node vtable id lz))))))
    (let ((ne (ri-uint rc 4)))
      (dotimes (i ne)
        (multiple-value-bind (id lz) (ri-node rc t)
          (mem-table-put etable id (if lazy lz (%lznode->node etable id lz))))))
    (%load-mem-index (mem-type-index-data (vertex-index graph)) (ri-index rc #'ri-key-type))
    (%load-mem-index (mem-type-index-data (edge-index graph))   (ri-index rc #'ri-key-type))
    (%load-mem-index (mem-ve-index-data (ve-index-in graph))    (ri-index rc #'ri-key-ve))
    (%load-mem-index (mem-ve-index-data (ve-index-out graph))   (ri-index rc #'ri-key-ve))
    (%load-mem-index (mem-vev-index-data (vev-index graph))     (ri-index rc #'ri-key-vev))
    (let ((idx (spatial-index graph)) (sp (ri-pairs rc)))
      (when idx (%load-mem-skip-list (spatial-index-skip-list idx) sp)))
    (setf *memory-image-view-dump* (or (ri-views rc) '()))
    ;; unique constraints (#6): present only in v4+ images -- guard on remaining bytes
    ;; so a v3 image (pre-#6) restores cleanly.
    (when (< (ric-i rc) (length (ric-bytes rc)))
      (%load-unique-indexes graph (ri-val rc)))
    :structural))

(defun write-memory-image (graph)
  "Persist GRAPH's full in-RAM state -- node tables AND every derived structure
(ve/vev/type indexes, spatial grid, view ordered-maps) -- to its image file in one
write.  A LAZY graph uses the VG-native format (per-node blobs, so open can defer
materialization); otherwise the cl-store v2 format.  Persisting the derived
structures (vs rebuilding on open) is what keeps OPEN-MEMORY-GRAPH fast."
  (if (lazy-p graph)
      (write-memory-image-native graph)
      (cl-store:store
       (list :version 3   ; v3: composite-key spatial dump (v2 keyed cells by bare string)
             :highest-tx-id (load-highest-transaction-id graph)
             :vertices (map-mem-table #'identity (vertex-table graph) :collect-p t)
             :edges (map-mem-table #'identity (edge-table graph) :collect-p t)
             :type-vertex (%dump-mem-index (mem-type-index-data (vertex-index graph)))
             :type-edge   (%dump-mem-index (mem-type-index-data (edge-index graph)))
             :ve-in       (%dump-mem-index (mem-ve-index-data (ve-index-in graph)))
             :ve-out      (%dump-mem-index (mem-ve-index-data (ve-index-out graph)))
             :vev         (%dump-mem-index (mem-vev-index-data (vev-index graph)))
             :spatial     (let ((idx (spatial-index graph)))
                            (and idx (%dump-mem-skip-list (spatial-index-skip-list idx))))
             :views       (%dump-views graph)
             :unique      (%dump-unique-indexes graph))   ; #6
       (memory-image-file (location graph)))))

(defun %rebuild-derived-from-nodes (graph vertices edges)
  "v1 fallback: rebuild indexes + spatial from the restored nodes (idempotent;
mem-index-list is a set, deleted nodes stay indexed and scans filter them)."
  (let ((*graph* graph))
    (dolist (v vertices) (add-node-to-indexes v graph :unless-present t))
    (dolist (e edges)     (add-node-to-indexes e graph :unless-present t))
    (let ((idx (spatial-index graph)))
      (when idx
        (flet ((reindex (n)
                 (let ((geom (node-geometry n)))
                   (when (and geom (not (deleted-p n)))
                     (spatial-index-insert idx (id n) geom)))))
          (dolist (v vertices) (reindex v))
          (dolist (e edges)    (reindex e)))))))

(defun restore-memory-image (graph)
  "Populate GRAPH's mem-tables -- and, for a v2 image, its derived structures --
from its cl-store image if one exists (the schema must already be restored so the
node classes are defined).  Returns :STRUCTURAL when a v2 image restored the
indexes/spatial/views directly (no rebuild), :REBUILT when a v1 (nodes-only) image
was rebuilt from the nodes, or NIL when no image was present.  The per-view dump is
stashed in *MEMORY-IMAGE-VIEW-DUMP* for RESTORE-VIEWS."
  (setf *memory-image-view-dump* :none)
  (let ((file (memory-image-file (location graph))))
    (when (probe-file file)
      (when (%native-image-p file)
        (return-from restore-memory-image (restore-memory-image-native graph file)))
      (destructuring-bind (&key version highest-tx-id vertices edges
                                type-vertex type-edge ve-in ve-out vev spatial views
                                unique
                           &allow-other-keys)
          (cl-store:restore file)
        (declare (ignore highest-tx-id))
        (dolist (v vertices) (mem-table-put (vertex-table graph) (id v) v))
        (dolist (e edges)     (mem-table-put (edge-table graph) (id e) e))
        (cond
          ((and (eql version 3) type-vertex)
           ;; Structural restore -- direct index/skip-list inserts, no map/reduce/
           ;; geohash recompute.  This is what keeps open fast (#50).  A v2 image
           ;; (bare-string spatial keys) falls through to the rebuild branch below,
           ;; which re-indexes geometry through the current composite-key insert.
           (%load-mem-index (mem-type-index-data (vertex-index graph)) type-vertex)
           (%load-mem-index (mem-type-index-data (edge-index graph))   type-edge)
           (%load-mem-index (mem-ve-index-data (ve-index-in graph))    ve-in)
           (%load-mem-index (mem-ve-index-data (ve-index-out graph))   ve-out)
           (%load-mem-index (mem-vev-index-data (vev-index graph))     vev)
           (let ((idx (spatial-index graph)))
             (when (and idx spatial)
               (%load-mem-skip-list (spatial-index-skip-list idx) spatial)))
           (setf *memory-image-view-dump* (or views '()))
           (when unique (%load-unique-indexes graph unique))   ; #6
           :structural)
          (t
           (%rebuild-derived-from-nodes graph vertices edges)
           :rebuilt))))))

(defun clear-memory-journal (graph)
  "Delete every retained .txn journal file (called after an image checkpoint)."
  (dolist (f (directory (make-pathname
                         :name :wild :type "txn"
                         :defaults (persistent-transaction-directory graph))))
    (ignore-errors (delete-file f))))

(defun checkpoint-memory-graph (graph)
  "Persist GRAPH's current in-RAM state to its cl-store image now, then clear the
superseded journal -- the same checkpoint CLOSE-GRAPH does, but callable at any
time.

IMPORTANT for a peer DEVICE: pulled state is applied directly (APPLY-PEER-CREATE-
WRITES) and is NOT journaled, so between opens it is durable ONLY through this
image.  Call CHECKPOINT-MEMORY-GRAPH after a PEER-SYNC so the pulled subgraph
survives a restart (or an unclean shutdown) without having to re-sync -- otherwise
the next OPEN-MEMORY-GRAPH restores whatever the last image held (possibly empty)
and the app re-cold-syncs.  Cheap: ~0.06 s / 0.2 MB for ~800 nodes."
  (check-type graph memory-graph-mixin)
  (write-memory-image graph)
  (clear-memory-journal graph)
  graph)

(defun open-memory-graph (name location
                          &key package replication-port replication-key
                            peer-role origin-id peer-host lazy regenerate-views
                            export-predicate device-registry merge-policy
                            reference-classes (peer-schema-version '(1 0)))
  "Reopen the in-memory graph NAME from LOCATION: restore the schema, restore the
image checkpoint (if any), then replay the retained .txn journal tail.  Tolerates a
.dirty marker (a memory-graph always rebuilds from its durable journal + image, so
an unclean shutdown is recovered, not an error).  :PEER-ROLE and the peer keys
mirror MAKE-MEMORY-GRAPH, reopening a MEMORY-PEER-GRAPH.  With :LAZY, nodes restore
as deferred blobs and materialize on first touch (needs a VG-native image)."
  (%validate-peer-role peer-role origin-id)
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (schema-file (format nil "~A/schema.dat" location))
         (graph (%make-empty-memory-graph
                 name path
                 :class (if peer-role 'memory-peer-graph 'memory-graph)
                 :lazy lazy
                 :replication-key replication-key
                 :replication-port replication-port)))
    (let ((*graph* graph) (*memory-image-view-dump* :none)
          (*memory-image-unique-loaded* nil))
      (if (probe-file schema-file)
          (progn
            (setf (schema graph) (cl-store:restore schema-file))
            (restore-schema-locks (schema graph)))
          (init-schema graph))
      (setf (schema-lock (schema graph)) (make-recursive-lock))
      (update-schema graph)
      (%write-dirty-marker path)
      (setf (gethash name *graphs*) graph)
      ;; Restore the checkpoint, then replay the journal tail committed after it.
      ;; Recovery runs BEFORE the transaction-manager is installed (the reaper
      ;; tolerates the unbound slot); the retain hook keeps the journal.
      (if (eq (restore-memory-image graph) :structural)
          ;; v2 image: views/indexes/spatial were restored structurally.  Create +
          ;; populate the views from the image dump, THEN replay the journal tail so
          ;; its authored ops update the already-restored derived structures
          ;; incrementally (fast open -- no map/reduce regen; #50).
          (progn (restore-views graph)
                 (recover-transactions graph))
          ;; v1 / no image: load the journal tail into the tables first, then
          ;; rebuild views in-RAM from ALL restored nodes (rebuild-on-open fallback).
          (progn (recover-transactions graph)
                 (restore-views graph)))
      ;; Reconcile the declarative view registry (issue #49) after views are
      ;; restored AND the journal tail is replayed, so any regenerate sees all nodes.
      (install-views graph)
      (when regenerate-views
        (regenerate-all-views graph))
      ;; Unique constraints (issue #6): the image restore (structural or cl-store)
      ;; loads the unique indexes -- so scan-rebuild only when it did NOT (a fresh
      ;; graph, or a pre-#6 v3 image / crash fallback).  This is the durable path on
      ;; the memory backend: no open-time scan, no lazy-node materialization.
      (unless *memory-image-unique-loaded*
        (rebuild-unique-indexes graph)))
    (when peer-role
      (%init-memory-peer-slots graph :open path peer-role origin-id peer-host
                               export-predicate device-registry merge-policy
                               reference-classes peer-schema-version))
    (setf (transaction-manager graph)
          (make-instance 'transaction-manager :graph graph))
    (ensure-directories-exist (persistent-transaction-directory graph))
    (init-replication-log graph)
    (start-replication graph :package package)
    (setf (graph-open-p graph) t)
    graph))

;; Clean-close checkpoint: write the image, then clear the (now-superseded)
;; journal.  Runs BEFORE the base CLOSE-GRAPH removes .dirty and nils the tables.
;; With :SNAPSHOT-P NIL the caller skips the checkpoint; the journal is retained,
;; so the graph is still fully recoverable on the next open (just not compacted).
(defmethod close-graph :before ((graph memory-graph-mixin) &key (snapshot-p t))
  (when (and (graph-open-p graph) snapshot-p)
    (write-memory-image graph)
    (clear-memory-journal graph)))

;;; ---------------------------------------------------------------------------
;;; Step 2 -- node-op overrides for the mem-table path.
;;;
;;; The heap/allocator helpers (MAYBE-WRITE-TO-HEAP, ARCHIVE-NODE-VERSION,
;;; REAP-NODE-CHAIN) are plain DEFUNs, so the memory path simply never invokes
;;; them: a memory node keeps DATA live and DATA-POINTER 0.  That single fact
;;; makes ENSURE-NODE-BYTES a natural no-op (it only touches the heap when
;;; data-pointer > 0), and makes RESOLVE-VERSION-AT-EPOCH and REAP-OLD-VERSIONS
;;; short-circuit on the empty (prev-pointer 0) version chain without ever
;;; dereferencing the (nil) heap.  So the only overrides Step 2 needs are the two
;;; write appliers, the table lookup, and a no-op spatial pass (the real indexes,
;;; views and spatial index arrive in the next increments).
;;; ---------------------------------------------------------------------------

;; Read: the mem-table IS the authoritative store -- return the live node.  On a
;; LAZY table an untouched node is an LZNODE blob; materialize it on first touch
;; (fault-on-access) and swap the live node in, so subsequent lookups are direct.
(defmethod lookup-node ((table mem-table) key graph)
  (declare (ignore graph))
  (let ((v (mem-table-get table key)))
    (if (lznode-p v) (mem-materialize table key v) v)))

;; Create: publish the live node into the mem-table.  No heap write, no index
;; update yet (Step 3), no version chain (MVCC dropped).  Mirrors the on-disk
;; tx-create's node-head bookkeeping (revision / written-p / commit-epoch) so the
;; OCC validator and the read guards behave identically.
(defmethod apply-tx-write ((write tx-create) (graph memory-graph-mixin))
  (let ((table (tx-write-table write graph))
        (node (node write)))
    ;; DATA-POINTER 0 is essential: a node pulled from a hub arrives carrying the
    ;; HUB's heap address, but a memory node has no heap (data lives on the node),
    ;; so leaving it non-zero makes ensure-node-bytes / maybe-init-node-data try to
    ;; read the nil heap.  The wire/serialized bytes are already present, so the
    ;; data stays live.
    (setf (revision node) 0
          (written-p node) t
          (commit-epoch node) *commit-epoch*
          (prev-pointer node) 0
          (data-pointer node) 0)
    (mem-table-put table (id node) node)
    ;; Index maintenance mirrors the on-disk tx-create: type-index for every node,
    ;; plus ve/vev adjacency for edges (add-node-to-indexes dispatches).  Update /
    ;; delete don't touch the indexes (parity); the id stays and scans filter it.
    (add-node-to-indexes node graph
                         :unless-present *add-to-indexes-unless-present-p*))
  write)

;; Update (and, by inheritance, delete -- the node then carries DELETED-P):
;; publish the new immutable node by atomic slot replacement, so a lock-free
;; reader sees the whole old or new node, never a torn one.  No archive:
;; prev-pointer stays 0.
(defmethod apply-tx-write ((write tx-update) (graph memory-graph-mixin))
  (let ((new-node (node write))
        (old-node (old-node write))
        (table (tx-write-table write graph)))
    (setf (revision new-node) (ldb (byte 32 0) (1+ (revision old-node)))
          (commit-epoch new-node) *commit-epoch*
          (prev-pointer new-node) 0
          (data-pointer new-node) 0)      ; never a heap address (see tx-create)
    (mem-table-put table (id new-node) new-node))
  write)

;; Spatial index maintenance (Step 4b): a memory-graph now carries a mem-backed
;; spatial-index (make-mem-spatial-index), so the BASE apply-tx-writes-to-spatial-
;; index runs unchanged -- no memory override needed (the Step-2 no-op is gone).

;;; ---------------------------------------------------------------------------
;;; Portable s-expr SNAPSHOT.  Durability on a memory-graph is the cl-store image
;;; (above) plus the retained journal; the SNAPSHOT generic is the separate
;;; portable / cross-impl compaction path (RECREATE-GRAPH reads its s-exprs).  It
;;; is deferred to a later increment.  CLOSE-GRAPH still calls it under :SNAPSHOT-P
;;; T, so provide a no-op (the cl-store checkpoint is done by the CLOSE-GRAPH
;;; :BEFORE method above, independently of this).
;;; ---------------------------------------------------------------------------

(defmethod snapshot ((graph memory-graph-mixin) &key &allow-other-keys)
  nil)

;;; ---------------------------------------------------------------------------
;;; Step 3 -- in-RAM index mirrors (decision #2: mirror ve/vev/type-index shapes).
;;;
;;; Each on-disk index maps a key to an INDEX-LIST (a heap pcons chain of ids);
;;; the in-RAM analogue maps the same key to a MEM-INDEX-LIST -- a hash set of
;;; ids (equalp on uuid arrays).  The high-level scans (MAP-VERTICES / MAP-EDGES /
;;; OUTGOING-EDGES / traversal) are unchanged: their primitives (GET-TYPE-INDEX-
;;; LIST, LOOKUP-VE-*-INDEX-LIST, LOOKUP-VEV-INDEX-LIST, MAP-INDEX-LIST, MAP-LHASH)
;;; are generic and dispatch onto these mem types.  Because a MEM-INDEX-LIST is a
;;; set, adds are inherently pushnew (dedup), so replay/rebuild is idempotent.
;;;
;;; Only tx-create touches the indexes (mirroring the on-disk path); a delete is a
;;; soft-delete that leaves the id in the index, and the scans filter it via the
;;; per-node DELETED-P / ACTIVE-EDGE-P guard after LOOKUP -- exact parity.
;;; ---------------------------------------------------------------------------

(defstruct (mem-index-list (:constructor %make-mem-index-list)
                           (:predicate mem-index-list-p))
  (ids (make-hash-table :test 'equalp)))  ; id (uuid) -> t ; set semantics

(declaim (inline make-mem-index-list))
(defun make-mem-index-list () (%make-mem-index-list))
(defun mem-index-list-add (mil id) (setf (gethash id (mem-index-list-ids mil)) t))
(defun mem-index-list-del (mil id) (remhash id (mem-index-list-ids mil)))

(defmethod map-index-list (fn (il mem-index-list) &key collect-p include-deleted-p)
  ;; The set holds ids only; the caller filters deleted nodes after LOOKUP, so
  ;; INCLUDE-DELETED-P is irrelevant here (mirrors how map-vertices/edges work).
  (declare (ignore include-deleted-p))
  (let ((acc '()))
    (maphash (lambda (id v)
               (declare (ignore v))
               (let ((r (funcall fn id))) (when collect-p (push r acc))))
             (mem-index-list-ids il))
    (when collect-p (nreverse acc))))

;;; type-index: type-id (integer) -> mem-index-list of node ids.
(defstruct (mem-type-index (:constructor %make-mem-type-index)
                           (:predicate mem-type-index-p))
  (data (make-hash-table :test 'eql
                         #+sbcl :synchronized #+sbcl t
                         #+ccl :shared #+ccl t
                         #+lispworks :single-thread #+lispworks nil)))
(defun make-mem-type-index () (%make-mem-type-index))

(defun %mem-ti-list (idx type-id &optional create)
  (or (gethash type-id (mem-type-index-data idx))
      (when create
        (setf (gethash type-id (mem-type-index-data idx)) (make-mem-index-list)))))

(defmethod get-type-index-list ((idx mem-type-index) (type-id integer))
  (%mem-ti-list idx type-id))

(defmethod type-index-push ((uuid array) (type-id integer) (idx mem-type-index)
                            &key unless-present)
  (declare (ignore unless-present))     ; set semantics => always pushnew
  (mem-index-list-add (%mem-ti-list idx type-id t) uuid))

(defmethod type-index-remove ((uuid array) (type-id integer) (idx mem-type-index))
  (let ((il (%mem-ti-list idx type-id)))
    (when il (mem-index-list-del il uuid))))

;;; ve-index: ve-key -> mem-index-list of edge ids (in and out are separate
;;; instances, as on disk).  ve-key / vev-key are structs, so EQUALP hashing keys
;;; them by (id-array, type-id) content -- the same identity VE-KEY-EQUAL uses.
(defstruct (mem-ve-index (:constructor %make-mem-ve-index)
                         (:predicate mem-ve-index-p))
  (data (make-hash-table :test 'equalp
                         #+sbcl :synchronized #+sbcl t
                         #+ccl :shared #+ccl t
                         #+lispworks :single-thread #+lispworks nil)))
(defun make-mem-ve-index () (%make-mem-ve-index))

(defmethod ve-index-push ((idx mem-ve-index) (key ve-key) (id array)
                          &key unless-present heap)
  ;; HEAP accepted for lambda-list congruence with the on-disk method; the
  ;; in-memory index-list needs no heap.
  (declare (ignore unless-present heap))
  (let ((il (or (gethash key (mem-ve-index-data idx))
                (setf (gethash key (mem-ve-index-data idx)) (make-mem-index-list)))))
    (mem-index-list-add il id)))

(defmethod ve-index-remove ((idx mem-ve-index) (key ve-key) (id array))
  (let ((il (gethash key (mem-ve-index-data idx))))
    (when il (mem-index-list-del il id))))

(defmethod lookup-ve-in-index-list ((key ve-key) (graph memory-graph-mixin))
  (gethash key (mem-ve-index-data (ve-index-in graph))))

(defmethod lookup-ve-out-index-list ((key ve-key) (graph memory-graph-mixin))
  (gethash key (mem-ve-index-data (ve-index-out graph))))

;;; vev-index: vev-key -> mem-index-list of edge ids.  add-to-vev-index inlines
;;; its lhash ops on disk (no vev-index-push generic), so override it directly.
(defstruct (mem-vev-index (:constructor %make-mem-vev-index)
                          (:predicate mem-vev-index-p))
  (data (make-hash-table :test 'equalp
                         #+sbcl :synchronized #+sbcl t
                         #+ccl :shared #+ccl t
                         #+lispworks :single-thread #+lispworks nil)))
(defun make-mem-vev-index () (%make-mem-vev-index))

(defmethod add-to-vev-index ((edge edge) (graph memory-graph-mixin) &key unless-present)
  (declare (ignore unless-present))
  (let* ((idx (vev-index graph))
         (key (make-vev-key :in-id (to edge) :out-id (from edge)
                            :type-id (type-id edge)))
         (il (or (gethash key (mem-vev-index-data idx))
                 (setf (gethash key (mem-vev-index-data idx)) (make-mem-index-list)))))
    (mem-index-list-add il (id edge))))

(defmethod remove-from-vev-index ((edge edge) (graph memory-graph-mixin))
  (let* ((idx (vev-index graph))
         (key (make-vev-key :in-id (to edge) :out-id (from edge)
                            :type-id (type-id edge)))
         (il (gethash key (mem-vev-index-data idx))))
    (when il (mem-index-list-del il (id edge)))))

(defmethod lookup-vev-index-list ((key vev-key) (graph memory-graph-mixin))
  (gethash key (mem-vev-index-data (vev-index graph))))

;; Hard node removal from the table -- used by the peer scope-exit purge
;; (PEER-PURGE-NODE), which drops a node that left the device's authority scope
;; straight from the vertex/edge table.  (The rest of that purge --
;; remove-from-{ve,vev,type}-index, remove-from-views, spatial-index-remove -- is
;; already generic and runs on the mem backing.)
(defmethod lhash-remove ((table mem-table) key)
  (mem-table-rem table key))

;;; Untyped MAP-VERTICES / MAP-EDGES scan: walk the mem-table, yielding the same
;;; (id . node) cons the lhash method yields.  An untyped scan inherently touches
;;; every node, so on a LAZY table it materializes each LZNODE (swapping the live
;;; node in) -- callers always receive live nodes.
(defmethod map-lhash (fn (table mem-table) &key collect-p)
  (let ((result nil))
    (maphash (lambda (id v)
               (let* ((node (if (lznode-p v) (mem-materialize table id v) v))
                      (r (funcall fn (cons id node))))
                 (when collect-p (push r result))))
             (mem-table-data table))
    (when collect-p (nreverse result))))

;;; ---------------------------------------------------------------------------
;;; Step 4c -- views on the in-RAM skip-list.
;;;
;;; A view's ordered map is created through MAKE-VIEW-SKIP-LIST (the seam added in
;;; views.lisp); a memory-graph returns an in-RAM mem-skip-list with the same view
;;; comparison/sentinels the heap list uses, so all view maintenance (add-to-view
;;; / update-in-views / remove-from-views) and querying (map-view /
;;; invoke-graph-view / map-reduced-view) run UNCHANGED -- add/remove/find/update-
;;; in-skip-list and the cursors are all generic now.
;;; ---------------------------------------------------------------------------

(defmethod make-view-skip-list ((graph memory-graph-mixin) view)
  (make-mem-skip-list
   :key-equal 'reduce-equal
   :key-comparison (if (eql :greaterp (view-sort-order view))
                       'reduce-comp-greaterp 'reduce-comp-lessp)
   :value-equal 'equal
   :head-key (if (eql :greaterp (view-sort-order view))
                 (list +max-sentinel+ +max-key+) (list +min-sentinel+ +null-key+))
   :head-value nil
   :tail-key (if (eql :greaterp (view-sort-order view))
                 (list +min-sentinel+ +null-key+) (list +max-sentinel+ +max-key+))
   :tail-value nil
   :duplicates-allowed-p nil))

;; On open, the base RESTORE-VIEWS reconstructs the view OBJECTS from views.dat (a
;; memory-graph's views have no heap pointer, so it opens no skip-list -- the
;; view-skip-list is left NIL).  Then, if the image carried a view dump
;; (*MEMORY-IMAGE-VIEW-DUMP*, a v2 image), each view's mem-skip-list is rebuilt via
;; MAKE-VIEW-SKIP-LIST and repopulated STRUCTURALLY from the dumped entries -- no
;; map, no reduce, no node scan (the fast-open path, #50).  Otherwise it falls back
;; to REGENERATE-ALL-VIEWS (rebuild-on-open) from the restored nodes.
(defmethod restore-views ((graph memory-graph-mixin))
  (call-next-method)
  (let ((*graph* graph)
        (dump *memory-image-view-dump*))
    (if (listp dump)
        (dolist (vd dump)
          (destructuring-bind (class-name view-name entries) vd
            (let ((view (lookup-view graph class-name view-name)))
              (when view
                (let ((sl (make-view-skip-list graph view)))
                  (setf (view-skip-list view) sl)
                  (%load-mem-skip-list sl entries))))))
        (regenerate-all-views graph))))
