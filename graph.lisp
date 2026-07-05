(in-package :graph-db)

;; Defined in unique-constraint.lisp (loaded after this file); declared so the
;; OPEN-GRAPH / CLOSE-GRAPH hooks below compile without forward-reference warnings.
(declaim (ftype (function (t) t)
                rebuild-unique-indexes save-unique-index-roots restore-unique-index-roots))

(defun spatial-index-root-file (location)
  (format nil "~A/spatial-index.root" location))

(defun init-spatial-index (graph &key (precision 7))
  "Create GRAPH's spatial index (at geohash PRECISION) in its indexes heap and
persist the skip-list root pointer + precision to a sidecar file (mirrors how
views persist their pointer).  PRECISION is read back by RESTORE-SPATIAL-INDEX."
  (let ((idx (make-spatial-index (indexes graph) :precision precision
                                 :backend (graph-index-backend graph))))
    (setf (spatial-index graph) idx)
    (cl-store:store (list :format +spatial-index-format+
                          :address (spatial-index-address idx)
                          :precision (spatial-index-precision idx)
                          ;; backend tag -> RESTORE reopens with the right opener
                          :backend (spatial-index-backend idx))
                    (spatial-index-root-file (location graph)))
    idx))

(defun restore-spatial-index (graph)
  "Reopen GRAPH's spatial index from its root sidecar.  A sidecar written in the
older on-disk format (v1: bare-string keys, duplicate cells), or a graph that
predates the spatial index, is rebuilt from live node geometries into the current
composite-key format (RESTORE runs after the node tables are open)."
  (let ((file (spatial-index-root-file (location graph))))
    (if (probe-file file)
        (destructuring-bind (&key address precision format (backend :skip-list)
                             &allow-other-keys)
            (cl-store:restore file)
          (if (eql format +spatial-index-format+)
              (setf (spatial-index graph)
                    (open-spatial-index (indexes graph) address :precision (or precision 7)
                                        :backend backend))
              ;; v1 / unversioned on disk -> rebuild into v2 (drops old, re-scans nodes).
              (rebuild-spatial-index graph :precision (or precision 7))))
        (init-spatial-index graph))))

(defun make-graph (name location &key master-p slave-p master-host
                                   replication-port replication-key package
                                   replay-txn-dir (buffer-pool-p t)
                                   (buffer-pool-size 100000)
                                   (vertex-buckets 8)
                                   (edge-buckets 8)
                                   (heap-size *default-heap-size*)
                                   (index-size *default-index-size*)
                                   (keep-revisions 0)
                                   (spatial-precision 7)
                                   replication-filter
                                   peer-role origin-id peer-host
                                   export-predicate device-registry merge-policy
                                   reference-classes (peer-schema-version '(1 0))
                                   (index-backend *index-backend*))
  "Create a brand-new graph named NAME with its on-disk files under the
directory LOCATION, register it (so LOOKUP-GRAPH and *GRAPH* can find it), and
return it.  The directory is created if necessary and must not already contain
a graph; use OPEN-GRAPH to reopen an existing one.

Keyword arguments:
  :MASTER-P / :SLAVE-P    create a replication master or slave.  Both require
                          :REPLICATION-PORT; a slave also requires :MASTER-HOST.
  :REPLICATION-PORT, :REPLICATION-KEY, :MASTER-HOST, :REPLAY-TXN-DIR
                          replication configuration (see Chapter 10 of the
                          manual).
  :BUFFER-POOL-P          whether to start the shared node buffer pool (default T).
  :BUFFER-POOL-SIZE       buffer pool size (default 100000).
  :VERTEX-BUCKETS / :EDGE-BUCKETS
                          initial linear-hash bucket counts (default 8).
  :HEAP-SIZE / :INDEX-SIZE
                          initial sizes (bytes) of the heap and indexes regions
                          (default *DEFAULT-HEAP-SIZE* / *DEFAULT-INDEX-SIZE*).
                          Both grow on demand, so these are only starting sizes.
  :SPATIAL-PRECISION      geohash precision of the spatial index grid (default 7,
                          ~150 m cells; 9 ~ 5 m).  Persisted with the index and
                          read back on OPEN-GRAPH.  See Chapter 13.
  :INDEX-BACKEND          ordered-map engine for this graph's heap-backed indexes
                          (views, :unique, spatial): :SKIP-LIST (default) or
                          :BPLUS-TREE (mmap B+ tree -- better cold-cache locality,
                          faster reads and writes, ~2x smaller).  Defaults to the
                          global *INDEX-BACKEND*.  Each index also records its own
                          backend, so OPEN-GRAPH reopens existing indexes with the
                          engine they were written with regardless of this; the
                          value here governs indexes CREATED on this graph.  Wire
                          it from your app's own config.  See
                          docs/bplus-tree-experiment.md.
  :REPLICATION-FILTER     (slaves only) a predicate (NODE) -> boolean; the slave
                          applies only replicated writes whose node it accepts,
                          so it holds just a subset (e.g. its area of operations).
                          See MAKE-SPATIAL-REPLICATION-FILTER.

A .dirty marker file is written on creation; always CLOSE-GRAPH to flush data
to disk and remove it."
  (when (and replay-txn-dir (not slave-p))
    (error ":REPLAY-TXN-DIR is only for slave graphs"))
  (when (and (or slave-p master-p) (not replication-port))
    (error ":REPLICATION-PORT is required for master and slave graphs"))
  (when (and slave-p (not master-host))
    (error ":MASTER-HOST required for slave graphs"))
  ;; Peer replication (hub-and-spoke) is a separate transport from master/slave.
  (when (and peer-role (or master-p slave-p))
    (error ":PEER-ROLE is mutually exclusive with :MASTER-P / :SLAVE-P"))
  (when (and peer-role (not (member peer-role '(:hub :device))))
    (error ":PEER-ROLE must be :HUB or :DEVICE, got ~S" peer-role))
  (when (and (eq peer-role :device) (null origin-id))
    (error "a :DEVICE peer-graph requires a hub-minted :ORIGIN-ID"))
  (ensure-directories-exist location)
  (let* ((path (pathname location))
         (dirty-file (format nil "~A/.dirty" location)))
    (unless (probe-file path)
      (error "Unable to open graph location ~A" path))
    (when buffer-pool-p
      (ensure-buffer-pool buffer-pool-size))
    (let* ((heap (create-memory
                  (format nil "~A/heap.dat" path)
                  heap-size))
           (graph
            (make-instance
             (cond (slave-p 'slave-graph)
                   (master-p 'master-graph)
                   (peer-role 'peer-graph)
                   (t 'graph))
             :graph-name name
             :location path
             :index-backend index-backend
             :views
             #+sbcl (make-hash-table :synchronized t)
             #+ccl (make-hash-table :shared t)
             #+lispworks (make-hash-table :single-thread nil)
             #+ecl (make-hash-table)
             :cache
             (make-id-table :synchronized t :weakness :value)
             :replication-key replication-key
             :replication-port replication-port
             :vertex-table (make-vertex-table
                            (format nil "~A/vertex/" path)
                            :base-buckets vertex-buckets)
             :edge-table (make-edge-table
                          (format nil "~A/edge/" path)
                          :base-buckets edge-buckets)
             :heap heap
             :indexes (create-memory
                       (format nil "~A/indexes.dat" path)
                       index-size)
             :ve-index-in (make-ve-index
                           (format nil "~A/ve-index-in/" path))
             :ve-index-out (make-ve-index
                            (format nil "~A/ve-index-out/" path))
             :vev-index (make-vev-index
                         (format nil "~A/vev-index/" path)))))
      (setf (vertex-index graph)
            (make-type-index
             (format nil "~A/vertex-index.dat" path) heap))
      (setf (edge-index graph)
            (make-type-index
             (format nil "~A/edge-index.dat" path) heap))
      ;; (MVCC: the lhash value-finalizer that copied node bytes under the bucket
      ;; lock is gone; read paths now materialize bytes under a read pin instead.)
      (let ((*graph* graph))
        (init-schema graph)
        ;; MVCC: graph-wide default retained-version count (per-type overrides via
        ;; def-vertex/def-edge :keep-revisions).  Set before update-schema persists.
        (setf (schema-keep-revisions (schema graph)) keep-revisions)
        (update-schema graph)
        (init-spatial-index graph :precision spatial-precision)
        (with-open-file (out dirty-file :direction :output)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph))
      (when slave-p
        (setf (master-host graph) master-host)
        ;; Set the subset filter before replay/replication so the slave applies
        ;; only its subset from the very first transaction.
        (when replication-filter
          (setf (replication-filter graph) replication-filter))
        (when replay-txn-dir
          (let ((*graph* graph))
            (replay graph replay-txn-dir package))))
      (when peer-role
        (setf (peer-role graph) peer-role
              (origin-id graph) origin-id
              (peer-host graph) peer-host
              (export-predicate graph) export-predicate
              (merge-policy graph) merge-policy
              (device-registry graph) device-registry
              (reference-classes graph) reference-classes
              (peer-schema-version graph) peer-schema-version
              ;; B1/PT-8: reload the durable Lamport clock so it never resets on
              ;; restart (0 for a fresh graph, the persisted value on reopen).
              (lamport-counter graph) (load-lamport-counter graph)
              ;; B2b: recover per-field Lamport stamps (v1 in-memory snapshot).
              (field-stamps graph) (load-field-stamps graph)
              ;; #6: recover the :ORIGIN-scope per-node origin partitions.
              (node-origins graph) (load-node-origins graph)
              ;; B3: recover the durable conflict records for the review surface.
              (peer-conflicts graph) (load-peer-conflicts graph)
              ;; WP-3: durable applied-op-id dedup index -- op-id (16-byte uuid key)
              ;; -> lamport (uint64 value), the make-lhash defaults.
              (applied-op-ids graph)
              (make-lhash :location (format nil "~A/applied-ops/" path)
                          :buckets 8)))
      (setf (transaction-manager graph)
            (make-instance 'transaction-manager
                           :graph graph))
      (ensure-directories-exist (persistent-transaction-directory graph))
      (init-replication-log graph)
      (start-replication graph :package package)
      (setf (graph-open-p graph) t)
      graph)))

(defun open-graph (name location &key master-p slave-p master-host replication-port
                   replication-key package (buffer-pool-p t) (gc-heap-p t)
                   (buffer-pool-size 100000)
                   (accept-versions (list +storage-version+))
                   keep-revisions regenerate-views
                   peer-role origin-id peer-host
                   export-predicate device-registry merge-policy
                   reference-classes (peer-schema-version '(1 0))
                   (index-backend *index-backend*))
  "Open the existing graph named NAME whose files live under directory
LOCATION, register it, and return it.  Use this to reopen a graph created
earlier with MAKE-GRAPH; the keyword arguments mirror MAKE-GRAPH's.

Signals an error if LOCATION holds a .dirty marker, which means the graph was
not closed cleanly and must be recovered first (see RECOVER-TRANSACTIONS and
the backup/recovery chapter).  By default the heap is garbage-collected
(:GC-HEAP-P) and outstanding transactions are recovered on open.  Views are
reconciled against their declarative definitions and kept as-is unless changed
(see DEF-VIEW); pass :REGENERATE-VIEWS T to force-rebuild every view on open.
Always CLOSE-GRAPH when finished."
  (when (and peer-role (or master-p slave-p))
    (error ":PEER-ROLE is mutually exclusive with :MASTER-P / :SLAVE-P"))
  (when (and peer-role (not (member peer-role '(:hub :device))))
    (error ":PEER-ROLE must be :HUB or :DEVICE, got ~S" peer-role))
  (when (and (eq peer-role :device) (null origin-id))
    (error "a :DEVICE peer-graph requires a hub-minted :ORIGIN-ID"))
  (ensure-directories-exist location)
  (let ((path (pathname location))
        (dirty-file (format nil "~A/.dirty" location))
        (schema-file (format nil "~A/schema.dat" location)))
    (unless (probe-file path)
      (error "Unable to open graph location ~A" path))
    (when (probe-file dirty-file)
      (error "~A exists;  graph not closed properly.  Run recovery." dirty-file))
    (log:info "Opening graph.")
    (when buffer-pool-p
      (log:info "Initializing buffer pool.")
      (ensure-buffer-pool buffer-pool-size))
    (let* ((heap (open-memory (format nil "~A/heap.dat" path)
                              :accept-versions accept-versions))
           (graph
            (make-instance
             (cond (slave-p 'slave-graph)
                   (master-p 'master-graph)
                   (peer-role 'peer-graph)
                   (t 'graph))
             :graph-name name
             :location path
             :index-backend index-backend
             :views
             #+sbcl (make-hash-table :synchronized t)
             #+ccl (make-hash-table :shared t)
             #+lispworks (make-hash-table :single-thread nil)
             #+ecl (make-hash-table)
             :cache
             (make-id-table :synchronized t :weakness :value)
             :replication-key replication-key
             :replication-port replication-port
             :vertex-table (open-lhash
                            (format nil "~A/vertex/" path))
             :edge-table (open-lhash
                          (format nil "~A/edge/" path))
             :heap heap
             :indexes (open-memory
                       (format nil "~A/indexes.dat" path)
                       :accept-versions accept-versions)
             :ve-index-in (open-ve-index
                           (format nil "~A/ve-index-in/" path))
             :ve-index-out (open-ve-index
                            (format nil "~A/ve-index-out/" path))
             :vev-index (open-vev-index
                         (format nil "~A/vev-index/" path)))))
      (let ((*graph* graph))
        (setf (vertex-index graph)
              (open-type-index (format nil "~A/vertex-index.dat" path) heap))
        (setf (edge-index graph)
              (open-type-index (format nil "~A/edge-index.dat" path) heap))
        ;; (MVCC: no lhash value-finalizer; read paths materialize node bytes
        ;; under a read pin -- see ENSURE-NODE-BYTES.)
        (if (probe-file schema-file)
            (progn
              (setf (schema graph)
                    (cl-store:restore schema-file))
              ;; Locks aren't persisted; rebuild the per-class rw-locks for the
              ;; restored types (otherwise schema-class-locks is nil and
              ;; def-vertex/def-edge and with-*-locked-class fail -- issue #32).
              (restore-schema-locks (schema graph)))
            (init-schema graph))
        (setf (schema-lock (schema graph)) (make-recursive-lock))
        ;; MVCC: optional override of the persisted graph-wide keep-revisions.
        (when keep-revisions
          (setf (schema-keep-revisions (schema graph)) keep-revisions))
        (update-schema graph)
        (restore-views graph)
        ;; Reconcile the declarative view registry against the restored views
        ;; (issue #49): keep unchanged persisted indexes O(1), rebuild changed ones.
        ;; Runs after RESTORE-VIEWS (and UPDATE-SCHEMA) so the node types the views
        ;; scan are already instantiated.  :REGENERATE-VIEWS forces a full rebuild.
        (install-views graph)
        (when regenerate-views
          (regenerate-all-views graph))
        (restore-spatial-index graph)
        (with-open-file (out dirty-file :direction :output)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph)
        (when gc-heap-p
          (gc-heap graph))
        (recover-transactions graph)
        ;; Unique constraints (issue #6): reopen the persistent unique skip-lists from
        ;; the sidecar (durable, no scan); only rebuild from nodes if there is no
        ;; sidecar -- a fresh graph, or a crash before CLOSE-GRAPH saved the roots.
        (unless (restore-unique-index-roots graph)
          (rebuild-unique-indexes graph)))
      (when slave-p
        (setf (master-host graph) master-host))
      (when peer-role
        (setf (peer-role graph) peer-role
              (origin-id graph) origin-id
              (peer-host graph) peer-host
              (export-predicate graph) export-predicate
              (merge-policy graph) merge-policy
              (device-registry graph) device-registry
              (reference-classes graph) reference-classes
              (peer-schema-version graph) peer-schema-version
              ;; B1/PT-8: recover the durable Lamport clock (monotonic across
              ;; restarts -- a reset would lose LWW races on post-restart writes).
              (lamport-counter graph) (load-lamport-counter graph)
              ;; B2b: recover per-field Lamport stamps (v1 in-memory snapshot).
              (field-stamps graph) (load-field-stamps graph)
              ;; #6: recover the :ORIGIN-scope per-node origin partitions.
              (node-origins graph) (load-node-origins graph)
              ;; B3: recover the durable conflict records for the review surface.
              (peer-conflicts graph) (load-peer-conflicts graph)
              ;; WP-3: open the applied-op-id index (create it if this peer-graph
              ;; predates the index, so reopening an older graph upgrades cleanly).
              (applied-op-ids graph)
              (let ((loc (format nil "~A/applied-ops/" path)))
                (if (probe-file (format nil "~Astruct.dat" loc))
                    (open-lhash loc)
                    (make-lhash :location loc :buckets 8)))))
      (setf (transaction-manager graph)
            (make-instance 'transaction-manager
                           :graph graph))
      (ensure-directories-exist (persistent-transaction-directory graph))
      (init-replication-log graph)
      (start-replication graph :package package)
      (setf (graph-open-p graph) t)
      graph)))

(defmethod close-graph ((graph graph) &key (snapshot-p t))
  "Cleanly close GRAPH: stop replication, flush and unmap all on-disk
structures (heap, indexes, vertex/edge tables), remove the .dirty marker, and
deregister it.  With :SNAPSHOT-P true (the default) a snapshot backup is taken
first.  Returns GRAPH.  Must be called with *GRAPH* bound to GRAPH (the
snapshot path relies on it).  Failing to close a graph leaves its .dirty marker
in place, forcing recovery on the next OPEN-GRAPH."
  (when (graph-open-p graph)
    (stop-replication graph)
    (remhash (graph-name graph) *graphs*)
    ;; Unique constraints (#6): persist the on-disk unique skip-lists' roots while the
    ;; heap is still open, so OPEN can reopen them without a scan.  No-op on memory.
    (save-unique-index-roots graph)
    (when snapshot-p
      (log:info "Snapshotting ~A" graph)
      (snapshot graph))
    (when (type-index-p (vertex-index graph))
      (log:info "Closing ~A" (vertex-index graph))
      (close-type-index (vertex-index graph)))
    (when (type-index-p (edge-index graph))
      (log:info "Closing ~A" (edge-index graph))
      (close-type-index (edge-index graph)))
    (when (vev-index-p (vev-index graph))
      (log:info "Closing ~A" (vev-index graph))
      (close-vev-index (vev-index graph)))
    (when (ve-index-p (ve-index-in graph))
      (log:info "Closing ~A" (ve-index-in graph))
      (close-ve-index (ve-index-in graph)))
    (when (ve-index-p (ve-index-out graph))
      (log:info "Closing ~A" (ve-index-out graph))
      (close-ve-index (ve-index-out graph)))
    (when (lhash-p (vertex-table graph))
      (log:info "Closing ~A" (vertex-table graph))
      (close-lhash (vertex-table graph)))
    (when (lhash-p (edge-table graph))
      (log:info "Closing ~A" (edge-table graph))
      (close-lhash (edge-table graph)))
    (when (memory-p (indexes graph))
      (log:info "Closing ~A" (indexes graph))
      (close-memory (indexes graph)))
    (when (memory-p (heap graph))
      (log:info "Closing ~A" (heap graph))
      (close-memory (heap graph)))
    (setf (heap graph) nil
          (vertex-table graph) nil
          (edge-table graph) nil)
    (let ((dirty-file (format nil "~A/.dirty" (location graph))))
      (delete-file dirty-file))
    (close-replication-log graph)
    (setf (graph-open-p graph) nil))
  graph)

;;; ---------------------------------------------------------------------------
;;; Peer replication (WP-3): applied-op-id dedup index lifecycle + API.
;;; The index is a durable op-id (16-byte uuid) -> lamport (uint64) lhash, checked
;;; before apply so a re-homed op bouncing back via the hub feed is not duplicated
;;; (design §3 #2, §6).  Created/opened in MAKE-GRAPH/OPEN-GRAPH (peer-role branch);
;;; closed here.
;;; ---------------------------------------------------------------------------

(defmethod close-graph :after ((graph peer-graph) &key (snapshot-p t))
  (declare (ignore snapshot-p))
  ;; B2b: snapshot the per-field Lamport stamps (v1 substrate persists on close).
  (ignore-errors (persist-field-stamps graph))
  ;; #6: snapshot the :ORIGIN-scope per-node origin partitions.
  (ignore-errors (persist-node-origins graph))
  ;; B3: snapshot the durable conflict records for the review surface.
  (ignore-errors (persist-peer-conflicts graph))
  (when (and (slot-boundp graph 'applied-op-ids)
             (lhash-p (applied-op-ids graph)))
    (close-lhash (applied-op-ids graph))
    (setf (applied-op-ids graph) nil)))

(defgeneric op-applied-p (graph op-id)
  (:documentation "True if the authored op identified by OP-ID (a 16-byte uuid) has
already been applied to GRAPH, so a re-homed op bouncing back is not re-applied.")
  (:method ((graph peer-graph) op-id)
    (and (lhash-p (applied-op-ids graph))
         (lhash-get (applied-op-ids graph) op-id)
         t)))

(defgeneric record-applied-op (graph op-id lamport)
  (:documentation "Record that OP-ID has been applied to GRAPH, stamping LAMPORT (NIL
=> 0).  Idempotent.  WP-3 invariant (design §12 PT-3): callers must record within the
same committed transaction as the apply, so a crash cannot leave the index and the
graph disagreeing.")
  (:method ((graph peer-graph) op-id lamport)
    (let ((index (applied-op-ids graph))
          (v (or lamport 0)))
      ;; Single-writer on the apply path (design §6, PT-5), so check-then-write is
      ;; race-free here and avoids depending on DUPLICATE-KEY-ERROR (defined later
      ;; in the load order than this file).
      (if (lhash-get index op-id)
          (lhash-update index op-id v)
          (lhash-insert index op-id v)))
    op-id))
