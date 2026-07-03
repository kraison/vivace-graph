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
  (lock (make-recursive-lock "mem-table")))

(declaim (inline make-mem-table mem-table-get mem-table-put mem-table-rem))
(defun make-mem-table () (%make-mem-table))

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
  ()
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
  "A spatial-index whose skip-list slot is an in-RAM mem-skip-list (geohash
string -> node id, duplicates allowed).  Every spatial op -- insert / remove /
query-bbox / query-radius -- goes through spatial-index-skip-list ->
add-to-skip-list / make-range-cursor, which dispatch to the mem list, so all of
spatial-index.lisp (and the base apply-tx-write-to-spatial-index maintenance)
runs UNCHANGED on a memory-graph.  All the geohash covering math is reused as-is."
  (%make-spatial-index
   :skip-list (make-mem-skip-list :key-comparison #'string< :key-equal #'string=
                                  :value-equal #'equalp :duplicates-allowed-p t
                                  :head-key +spatial-min-key+ :head-value +null-key+
                                  :tail-key +spatial-max-key+ :tail-value +max-key+)
   :heap nil :precision precision))

(defun %make-empty-memory-graph (name location &key (class 'memory-graph)
                                                 replication-key replication-port)
  "A fresh CLASS shell with empty in-RAM tables (no schema/txn wiring yet).
Shared by MAKE-MEMORY-GRAPH and OPEN-MEMORY-GRAPH; CLASS is MEMORY-GRAPH or
MEMORY-PEER-GRAPH."
  (make-instance class
                 :graph-name name
                 :location location
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
                 :spatial-index (make-mem-spatial-index)))

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
        (peer-conflicts graph) (load-peer-conflicts graph)
        (applied-op-ids graph)
        (let ((loc (format nil "~A/applied-ops/" path)))
          (if (and (eq mode :open) (probe-file (format nil "~Astruct.dat" loc)))
              (open-lhash loc)
              (make-lhash :location loc :buckets 8)))))

(defun make-memory-graph (name location
                          &key package replication-port replication-key
                            peer-role origin-id peer-host
                            export-predicate device-registry merge-policy
                            reference-classes (peer-schema-version '(1 0)))
  "Create a brand-new in-memory graph named NAME.  LOCATION is used for the
durable journal, cl-store image and schema (the RAM structures are rebuilt from
them on OPEN-MEMORY-GRAPH).  With :PEER-ROLE (:HUB or :DEVICE, a :DEVICE also
needs a hub-minted :ORIGIN-ID) a MEMORY-PEER-GRAPH is built and the peer
replication path is wired.  Registers the graph and returns it."
  (%validate-peer-role peer-role origin-id)
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (graph (%make-empty-memory-graph
                 name path
                 :class (if peer-role 'memory-peer-graph 'memory-graph)
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

(defun write-memory-image (graph)
  "Pickle GRAPH's live vertex/edge tables to its cl-store image in one write."
  (cl-store:store
   (list :version 1
         :highest-tx-id (load-highest-transaction-id graph)
         :vertices (map-mem-table #'identity (vertex-table graph) :collect-p t)
         :edges (map-mem-table #'identity (edge-table graph) :collect-p t))
   (memory-image-file (location graph))))

(defun restore-memory-image (graph)
  "Populate GRAPH's mem-tables from its cl-store image if one exists (the schema
must already be restored so the node classes are defined).  Returns T if an image
was restored, NIL if none was present."
  (let ((file (memory-image-file (location graph))))
    (when (probe-file file)
      (destructuring-bind (&key version highest-tx-id vertices edges)
          (cl-store:restore file)
        (declare (ignore version highest-tx-id))
        (dolist (v vertices) (mem-table-put (vertex-table graph) (id v) v))
        (dolist (e edges)     (mem-table-put (edge-table graph) (id e) e))
        ;; The image pickles only the node tables; the indexes are derived, so
        ;; rebuild them from the restored nodes (cheap; the "rebuild on open"
        ;; shortcut of design §6, here for the ve/vev/type indexes).  Idempotent:
        ;; mem-index-list is a set.  Deleted nodes stay indexed (parity), scans
        ;; filter them.
        (let ((*graph* graph))
          (dolist (v vertices) (add-node-to-indexes v graph :unless-present t))
          (dolist (e edges)     (add-node-to-indexes e graph :unless-present t))
          ;; Rebuild the spatial index too (derived; the image pickles only nodes).
          (let ((idx (spatial-index graph)))
            (when idx
              (flet ((reindex (n)
                       (let ((geom (node-geometry n)))
                         (when (and geom (not (deleted-p n)))
                           (spatial-index-insert idx (id n) geom)))))
                (dolist (v vertices) (reindex v))
                (dolist (e edges)    (reindex e)))))))
      t)))

(defun clear-memory-journal (graph)
  "Delete every retained .txn journal file (called after an image checkpoint)."
  (dolist (f (directory (make-pathname
                         :name :wild :type "txn"
                         :defaults (persistent-transaction-directory graph))))
    (ignore-errors (delete-file f))))

(defun open-memory-graph (name location
                          &key package replication-port replication-key
                            peer-role origin-id peer-host
                            export-predicate device-registry merge-policy
                            reference-classes (peer-schema-version '(1 0)))
  "Reopen the in-memory graph NAME from LOCATION: restore the schema, restore the
cl-store image checkpoint (if any), then replay the retained .txn journal tail.
Tolerates a .dirty marker (a memory-graph always rebuilds from its durable
journal + image, so an unclean shutdown is recovered, not an error).  :PEER-ROLE
and the peer keys mirror MAKE-MEMORY-GRAPH, reopening a MEMORY-PEER-GRAPH."
  (%validate-peer-role peer-role origin-id)
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (schema-file (format nil "~A/schema.dat" location))
         (graph (%make-empty-memory-graph
                 name path
                 :class (if peer-role 'memory-peer-graph 'memory-graph)
                 :replication-key replication-key
                 :replication-port replication-port)))
    (let ((*graph* graph))
      (if (probe-file schema-file)
          (progn
            (setf (schema graph) (cl-store:restore schema-file))
            (restore-schema-locks (schema graph)))
          (init-schema graph))
      (setf (schema-lock (schema graph)) (make-recursive-lock))
      (update-schema graph)
      ;; Checkpoint first, then the journal tail committed after it.
      (restore-memory-image graph)
      (%write-dirty-marker path)
      (setf (gethash name *graphs*) graph)
      ;; Recovery runs BEFORE the transaction-manager is installed (the reaper
      ;; tolerates the unbound slot); the retain hook keeps the journal.
      (recover-transactions graph)
      ;; Rebuild views in-RAM from the restored nodes (design §6).
      (restore-views graph))
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

;; Read: the mem-table IS the authoritative store -- return the live node.
(defmethod lookup-node ((table mem-table) key graph)
  (declare (ignore graph))
  (mem-table-get table key))

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
                          &key unless-present)
  (declare (ignore unless-present))
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
;;; (id . node) cons the lhash method yields.
(defmethod map-lhash (fn (table mem-table) &key collect-p)
  (let ((result nil))
    (maphash (lambda (id node)
               (let ((r (funcall fn (cons id node))))
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

;; On open, the base RESTORE-VIEWS reconstructs the view objects from views.dat
;; (a memory-graph's views have no heap pointer, so it opens no skip-list -- the
;; view-skip-list is left NIL), then REGENERATE-ALL-VIEWS builds each view's
;; mem-skip-list (via MAKE-VIEW-SKIP-LIST above) and repopulates it by scanning
;; the restored nodes -- the design-§6 rebuild-on-open for views.
(defmethod restore-views ((graph memory-graph-mixin))
  (call-next-method)
  (let ((*graph* graph))
    (regenerate-all-views graph)))
