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

(defun %make-empty-memory-graph (name location)
  "A fresh MEMORY-GRAPH shell with empty in-RAM tables (no schema/txn wiring yet).
Shared by MAKE-MEMORY-GRAPH and OPEN-MEMORY-GRAPH."
  (make-instance 'memory-graph
                 :graph-name name
                 :location location
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
                 :ve-index-in nil
                 :ve-index-out nil
                 :vev-index nil
                 :vertex-index nil
                 :edge-index nil
                 :spatial-index nil))

(defun %write-dirty-marker (path)
  (with-open-file (out (format nil "~A/.dirty" path) :direction :output
                                                     :if-exists :supersede)
    (format out "~S" (get-universal-time))))

(defun make-memory-graph (name location &key package)
  "Create a brand-new in-memory graph named NAME.  LOCATION is still used for the
durable journal, cl-store image and schema (the RAM structures are rebuilt from
them on OPEN-MEMORY-GRAPH).  Registers the graph and returns it."
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (graph (%make-empty-memory-graph name path)))
    (let ((*graph* graph))
      (init-schema graph)
      (update-schema graph)
      (%write-dirty-marker path)
      (setf (gethash name *graphs*) graph))
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
        (dolist (e edges)     (mem-table-put (edge-table graph) (id e) e)))
      t)))

(defun clear-memory-journal (graph)
  "Delete every retained .txn journal file (called after an image checkpoint)."
  (dolist (f (directory (make-pathname
                         :name :wild :type "txn"
                         :defaults (persistent-transaction-directory graph))))
    (ignore-errors (delete-file f))))

(defun open-memory-graph (name location &key package)
  "Reopen the in-memory graph NAME from LOCATION: restore the schema, restore the
cl-store image checkpoint (if any), then replay the retained .txn journal tail.
Tolerates a .dirty marker (a memory-graph always rebuilds from its durable
journal + image, so an unclean shutdown is recovered, not an error)."
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (schema-file (format nil "~A/schema.dat" location))
         (graph (%make-empty-memory-graph name path)))
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
      (recover-transactions graph))
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
    (setf (revision node) 0
          (written-p node) t
          (commit-epoch node) *commit-epoch*
          (prev-pointer node) 0)
    (mem-table-put table (id node) node))
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
          (prev-pointer new-node) 0)
    (mem-table-put table (id new-node) new-node))
  write)

;; The spatial index is a first-class v1 feature, but it arrives in Step 4; until
;; then a memory-graph has no spatial-index, so its apply pass is a no-op.
(defmethod apply-tx-writes-to-spatial-index (writes (graph memory-graph-mixin))
  (declare (ignore writes))
  nil)

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
