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

(defun make-memory-graph (name location &key package)
  "Create a brand-new in-memory graph named NAME.  LOCATION is still used for the
durable journal, snapshot and schema (the RAM structures are rebuilt from them on
open).  Registers the graph and returns it."
  (ensure-directories-exist location)
  (let* ((path (pathname location)))
    (let ((graph (make-instance 'memory-graph
                                :graph-name name
                                :location path
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
                                :spatial-index nil)))
      (let ((*graph* graph))
        (init-schema graph)
        (update-schema graph)
        (with-open-file (out (format nil "~A/.dirty" path) :direction :output
                                                           :if-exists :supersede)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph))
      (setf (transaction-manager graph)
            (make-instance 'transaction-manager :graph graph))
      (ensure-directories-exist (persistent-transaction-directory graph))
      (init-replication-log graph)
      (start-replication graph :package package)
      (setf (graph-open-p graph) t)
      graph)))

;;; ---------------------------------------------------------------------------
;;; Snapshot stub.  A memory-graph rebuilds from snapshot + journal on open; the
;;; real snapshot writer arrives with the durability increment.  For now CLOSE-
;;; GRAPH's default :SNAPSHOT-P T must not error, so provide a no-op that logs.
;;; ---------------------------------------------------------------------------

(defmethod snapshot ((graph memory-graph-mixin) &key &allow-other-keys)
  (log:info "memory-graph snapshot: not yet implemented (Step 1 skeleton); no-op")
  nil)
