(in-package :graph-db)

;; Defined in unique-constraint.lisp (loaded after this file); declared so the
;; OPEN-GRAPH / CLOSE-GRAPH hooks below compile without forward-reference warnings.
(declaim (ftype (function (t) t)
                rebuild-unique-indexes save-unique-index-roots restore-unique-index-roots
                rebuild-secondary-indexes save-secondary-index-roots
                restore-secondary-index-roots install-secondary-indexes
                ;; spatial-query.lisp / spatial-registry.lisp (both load later)
                rebuild-spatial-indexes report-degraded-spatial-indexes
                install-spatial-indexes))

;;; ---------------------------------------------------------------------------
;;; Spatial index persistence -- the v3 sidecar.
;;;
;;; One index per (declaring-class . geometry-slot) (spatial-registry.lisp), each
;;; a heap-backed ordered map in the graph's INDEXES memory.  Their root addresses
;;; are persisted to spatial-indexes.dat, exactly as views, :unique and the general
;;; ordered indexes persist theirs, so OPEN-GRAPH REOPENS them by address instead
;;; of allocating fresh ones.
;;;
;;; This is not merely a speed-up.  GC-HEAP sweeps heap.dat only; nothing ever
;;; reclaims indexes.dat.  An open that allocated fresh ordered maps ORPHANED the
;;; previous run's with no root address left to free them by, so the indexes region
;;; grew without bound -- a megabyte per open on a few hundred polygons -- until it
;;; was exhausted.  Reopening by address is what closes that.
;;; ---------------------------------------------------------------------------

(defun %atomic-cl-store (object path)
  "CL-STORE OBJECT to PATH torn-write-safe (GH #63): write to PATH.tmp in the
same directory, then rename(2) (%POSIX-RENAME) into place.  A crash mid-write
leaves only the .tmp file behind; PATH itself is always either the previous
complete sidecar or the new one, never a partial write.  Shared by the
spatial/unique/secondary index sidecar writers -- the only three that
CL-STORE:STORE a live index sidecar in place."
  (let ((tmp (format nil "~A.tmp" (namestring path))))
    (cl-store:store object tmp)
    (%posix-rename tmp path)))

(defun spatial-indexes-root-file (location)
  (format nil "~A/spatial-indexes.dat" location))

(defun spatial-index-root-file (location)
  "The PRE-v3 single-index sidecar.  Its presence with no spatial-indexes.dat is
the migration signal; it is never written any more, and is left in place rather
than renamed -- the old RESTORE-SPATIAL-INDEX treats a missing file as an EMPTY
index rather than a rebuild, so renaming would make a downgrade fail silently.
Downgrade after migration is unsupported either way."
  (format nil "~A/spatial-index.root" location))

(defun save-spatial-index-roots (graph &key (complete t))
  "Persist every spatial index's root, precision, backend, insert cap and precision
histogram, plus a COMPLETE marker.  Called at CLOSE-GRAPH and by the rebuild/
regenerate admin ops -- NOT on the commit path.  It deliberately never runs while a
transaction holds the manager lock: it once fired on an index creation and on a
coarsest-precision decrease, but that put CL-STORE file I/O on the post-durability
commit path under that lock (a convoy point and a failure-injection point after the
data is already durable).  Crash-correctness of the histogram now comes from
OPEN-GRAPH re-deriving the spatial indexes from the recovered nodes after WAL
replay, not from an incremental write here.

:COMPLETE distinguishes a sidecar that names every index the registry is supposed
to hold from one written partway through a multi-index operation
(REBUILD-SPATIAL-INDEXES, REGENERATE-SPATIAL-INDEX): those bracket their own work
with an immediate :COMPLETE NIL save and a closing :COMPLETE T save once every
index is back in place, so a crash in between leaves a sidecar that is perfectly
readable and internally well-formed, but RESTORE-SPATIAL-INDEX-ROOTS must still
refuse to trust it -- the alternative is a reopen that silently treats an index the
crash never got to recreate as legitimately empty, forever.

No-op on a graph with no INDEXES heap -- a memory-graph, whose indexes are in-RAM
mem-skip-lists with no heap address to persist.  Its spatial registry rides the
checkpoint image instead."
  (when (and (indexes graph) (spatial-indexes graph))
    (let ((roots '()))
      (maphash (lambda (key idx)
                 ;; Only a heap-backed ordered map has an address; skip anything
                 ;; else rather than signalling, so a hybrid graph still closes.
                 (when (view-index-p (spatial-index-skip-list idx))
                   (push (list (car key) (cdr key)
                               (spatial-index-address idx)
                               (spatial-index-precision idx)
                               (spatial-index-backend idx)
                               (spatial-index-max-cells idx)
                               (copy-seq (spatial-index-precision-counts idx)))
                         roots)))
               (spatial-indexes graph))
      (%atomic-cl-store (list :format +spatial-index-format+
                              :complete complete
                              :indexes roots)
                        (spatial-indexes-root-file (location graph)))))
  nil)

(defun restore-spatial-index-roots (graph)
  "Reopen the spatial indexes from the v5 sidecar -- no node scan.  Returns T
when one was present, current, and COMPLETE; NIL to fall back to REBUILD-
SPATIAL-INDEXES (a fresh graph, a pre-v5 graph, a crash before any root was
written, a sidecar too damaged to read, or one a crashed rebuild left marked
:COMPLETE NIL).  No-op returning NIL on a graph with no INDEXES heap.

An UNREADABLE sidecar falls back rather than propagating.  Unlike the unique and
secondary sidecars -- written only at CLOSE-GRAPH -- this one is also written at
index creation and on a coarsening, i.e. from inside a commit, so a crash CAN tear
it; and the nodes remain authoritative, so a rebuild always reconstructs the truth.
Refusing to open a graph whose node data is entirely intact would be the wrong
trade.  Only the read is guarded: a well-formed sidecar whose RECORDS are
malformed still signals, because that is a code defect, not a torn write.

A well-formed but INCOMPLETE sidecar is the second, independent reason to fall
back: readable and internally consistent, but naming fewer indexes than the
registry is supposed to hold because the rebuild that wrote it never reached its
closing save (see SAVE-SPATIAL-INDEX-ROOTS).  A sidecar with no :COMPLETE key at
all -- the shape written before this marker existed -- reads as complete via the
DESTRUCTURING-BIND default below, not as a special case, so a graph already on the
current format is never forced into a needless rebuild by this change.

A COMPLETE v3 or v4 sidecar is stale but still ADOPTED before the fall-back --
see the comment at the OPEN below."
  (let ((file (and (indexes graph) (spatial-indexes-root-file (location graph)))))
    (when (and file (probe-file file))
      (destructuring-bind (&key format indexes (complete t) &allow-other-keys)
          (handler-case (cl-store:restore file)
            (error (e)
              (warn "Spatial index sidecar ~A is unreadable (~A); re-deriving the ~
                     indexes from live node geometries, which are authoritative."
                    file e)
              nil))
        ;; A COMPLETE v3 or v4 sidecar has v5's layout and names LIVE ordered
        ;; maps; only their CONTENT is stale (v3's multipolygon cells, GH #103;
        ;; v4's untagged entries, GH #104).  Open those roots too -- not to
        ;; trust them, but so REBUILD-SPATIAL-INDEXES can FREE them: the
        ;; registry is the only place it looks for storage to reclaim, so
        ;; skipping this strands every stale index in a region GC-HEAP never
        ;; sweeps.  Returning NIL is what routes to that rebuild.
        (when (and complete (member format (list +spatial-index-format+ 3 4)))
          (dolist (r indexes)
            (destructuring-bind (owner slot address precision backend max-cells
                                 &optional counts)
                r
              (setf (gethash (cons owner slot) (spatial-indexes graph))
                    (open-spatial-index (indexes graph) address
                                        :precision precision :backend backend
                                        :max-cells max-cells
                                        :precision-counts counts))))
          (eql format +spatial-index-format+))))))

(defun all-node-classes-with-vector-index-slots (graph)
  "The node classes of GRAPH that declare at least one :VECTOR-INDEX slot.  Note
this can list several classes (a whole hierarchy) for the SAME segment -- a
:VECTOR-INDEX slot's effective-slot inheritance means both the declaring class
and its subclasses report it (node-class.lisp).  Callers that open/rebuild
segments must dedup by owner (see ALL-VECTOR-SEGMENT-OWNER-KEYS) so one shared
owner segment is not opened/rebuilt once per subclass."
  (loop for nt in (all-node-types graph)
        for name = (if (node-type-p nt) (node-type-name nt) nt)
        for class = (and name (find-class name nil))
        when (and class (class-finalized-p class)
                  (node-vector-index-slots class))
          collect class))

(defun all-vector-segment-owner-keys (graph)
  "The distinct (OWNER-NAME . SLOT-NAME) segment keys across GRAPH's
:VECTOR-INDEX slots.  Several classes in a hierarchy report the same slot
(effective-slot inheritance -- see ALL-NODE-CLASSES-WITH-VECTOR-INDEX-SLOTS),
but under Model B they share ONE owner segment (%VECTOR-INDEX-SLOT-OWNER-NAME),
so each (owner, slot) pair is returned exactly once regardless of how many
concrete classes declare it -- the open/rebuild path must open or rebuild the
owner segment ONCE, not once per subclass."
  (let ((seen (make-hash-table :test 'equal))
        (keys '()))
    (dolist (class (all-node-classes-with-vector-index-slots graph))
      (dolist (slot (node-vector-index-slots class))
        (let ((key (cons (%vector-index-slot-owner-name class slot) slot)))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (push key keys)))))
    (nreverse keys)))

(defun %vector-segment-owner-has-nodes-p (graph owner-name)
  "True when OWNER-NAME's vertex type index holds at least one live entry,
subclasses included -- i.e. when REBUILD-VECTOR-SEGMENT's MAP-VERTICES sweep of
OWNER-NAME could visit anything at all.

Reads the TYPE INDEX only and stops at the first id: it never calls
LOOKUP-VERTEX, so it does not deserialize a single node, where the sweep it
guards deserializes every one of them.  (Same non-local exit out of
MAP-INDEX-LIST that INDEX-LIST-MEMBER-P uses.)

Deliberately conservative: an index list holding only DELETED nodes still
answers T (the memory backend keeps deleted ids indexed by design), so this can
only ever permit a sweep that finds nothing -- never suppress one that would
have found something."
  (dolist (type-id (resolve-node-type-ids owner-name :vertex :graph graph) nil)
    (let ((il (get-type-index-list (vertex-index graph) type-id)))
      (when il
        (map-index-list (lambda (id)
                          (declare (ignore id))
                          (return-from %vector-segment-owner-has-nodes-p t))
                        il)))))

(defun restore-vector-segments (graph)
  "For every distinct (OWNER, SLOT) :VECTOR-INDEX segment, open the existing
segment as-is if it was cleanly closed, else rebuild it from nodes (a rebuild
sweeps in every subclass instance too -- see REBUILD-VECTOR-SEGMENT).  Runs at
open, before the graph accepts writes (quiescent).  Keyed by owner, not by
concrete class, so a shared owner segment is opened/rebuilt exactly once even
when several subclasses declare the same :VECTOR-INDEX slot.

A MISSING segment file is REBUILT, not skipped.  The vertices are present and
authoritative and the segment is derived from them, so an absent file is exactly
the case REBUILD-VECTOR-SEGMENT exists for.  Skipping it -- the old behaviour --
opened the graph clean with a permanently empty vector index, no warning and no
error, so VECTOR-SEARCH returned nothing for a corpus that was entirely intact
in the vertices; it also made \"delete the segment file and let it rebuild\", the
intuitive operator recovery, a silent no-op.  Recovering vectors from a file
that should have existed does warn: the file did not go missing by itself.

But an absent file is ALSO the normal state of a slot that has never been
written -- segments are created lazily on the first conforming write -- and
there the rebuild would sweep the whole corpus, find nothing, create nothing,
and so leave no file for the NEXT open to find either: the same fruitless sweep
on every open thereafter, forever.  Nothing about that state is an error, so it
must not cost anything.  The sweep is therefore gated on
%VECTOR-SEGMENT-OWNER-HAS-NODES-P: an owner class with no nodes in the type
index is skipped outright, in O(1) and without deserializing a node.

That gate bounds the empty-owner case exactly; it does NOT bound the other
never-written shape, an owner class that is FULL of nodes none of which has a
vector yet -- the migration window REBUILD-VECTOR-SEGMENT-BATCHED exists for.
That case still pays one counting sweep per open (no segment is created, so no
puts and no second pass), and it self-terminates the moment any vector is
written, because that write creates the file.  Bounding it too would take a
persisted \"nothing to index here\" marker, which is a new on-disk artifact
whose staleness would reintroduce exactly the silent-empty-index bug above;
deliberately not done here."
  (dolist (key (all-vector-segment-owner-keys graph))
    (destructuring-bind (owner-name . slot) key
      (let ((path (%segment-file graph owner-name slot)))
        (cond
          ((probe-file path)
           (let ((seg (open-vector-segment path)))
             (if (segment-clean-shutdown-p seg)
                 (setf (gethash (cons owner-name slot) (vector-segments graph)) seg)
                 (progn
                   (close-vector-segment seg)
                   ;; Warn BEFORE, not after: on a large corpus this rebuild is
                   ;; the reason the open appears to hang, and an operator who
                   ;; only learns about it once it finishes has been told
                   ;; nothing useful.  The missing-file branch below warns after
                   ;; instead, because there the recovered count IS the
                   ;; diagnostic and a graph that legitimately has no vectors
                   ;; must stay silent.
                   (warn "vector segment ~A was not closed cleanly; rebuilding it ~
                          from the vertices, which are authoritative.  This scans ~
                          every ~A node and can take a while on a large corpus."
                         path owner-name)
                   (rebuild-vector-segment graph owner-name slot)))))
          ((%vector-segment-owner-has-nodes-p graph owner-name)
           (let ((seg (rebuild-vector-segment graph owner-name slot)))
             (when seg
               (warn "vector segment file ~A was missing at open; rebuilt ~D ~
                      entries from the vertices, which are authoritative."
                     path (segment-live-count seg)))))
          ;; No file and no nodes of the owner type: nothing to index, nothing
          ;; to warn about, and nothing to scan.
          (t nil))))))

(defun vector-search (graph class-name slot-name query-vector k)
  "Top-K nodes of CLASS-NAME (and its subclasses) whose SLOT-NAME vector is
nearest QUERY-VECTOR by cosine, as (score . node-id) conses, best first.

Resolves the OWNER segment for (CLASS-NAME, SLOT-NAME) -- under Model B one
segment per declaring class holds the whole hierarchy -- and scans it.  Returns
NIL when no segment exists yet: segments are created lazily on the first
conforming write, so a declared-but-never-written slot simply has nothing to
search.

NIL is AMBIGUOUS, deliberately.  It means any of: (a) the legitimate lazy case
above; (b) CLASS-NAME names no class; (c) SLOT-NAME is not a :VECTOR-INDEX slot
on CLASS-NAME or any ancestor -- in which case %VECTOR-INDEX-SLOT-OWNER-NAME
falls back to the queried class itself and the lookup misses a key nothing was
ever stored under.  So a typo in either name is indistinguishable from an empty
index and reports \"nothing indexed\" rather than signalling.  If you are
debugging an unexpectedly empty result, check the declaration before the data."
  (let* ((class (find-class class-name nil))
         (owner (and class (%vector-index-slot-owner-name class slot-name)))
         (segment (and owner
                       (gethash (cons owner slot-name) (vector-segments graph)))))
    (when segment
      (segment-scan segment query-vector k))))

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
                                   (spatial-max-cells +spatial-insert-max-cells+)
                                   replication-filter
                                   peer-role origin-id peer-host
                                   export-predicate device-registry merge-policy
                                   reference-classes (peer-schema-version '(1 0))
                                   (index-backend *index-backend*)
                                   spatial-index-backend)
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
  :SPATIAL-PRECISION      default geohash precision of the spatial index grids
                          (default 7, ~150 m cells; 9 ~ 5 m).  Each index persists
                          its own precision in spatial-indexes.dat and reopens at
                          it, so this is the value NEW indexes are created with.
                          See Chapter 13.
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
  :SPATIAL-INDEX-BACKEND  engine for SPATIAL indexes specifically, overriding
                          :INDEX-BACKEND for them alone.  NIL (default) means
                          follow :INDEX-BACKEND.  Use it to keep B+ trees for
                          views and :UNIQUE while spatial indexes use the skip
                          list: a spatial query is a handful of SHORT prefix
                          range scans (one per covering geohash cell, most
                          returning nothing), and the B+ tree's range-scan
                          advantage is per ENTRY, so it does not survive that
                          shape -- measured ~600 KB consed for a zero-row query
                          vs ~115 KB on the skip list, and slower at every corpus
                          size (GH #91).  Governs only NEWLY created spatial
                          indexes; existing ones reopen on their persisted tag.
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
             :spatial-index-backend spatial-index-backend
             :views
             #+sbcl (make-hash-table :synchronized t)
             #+ccl (make-hash-table :shared t)
             #+lispworks (make-hash-table :single-thread nil)
             #+ecl (make-hash-table #+graph-db-ecl-sync-hash :synchronized
                                     #+graph-db-ecl-sync-hash t)
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
        (setf (graph-default-spatial-precision graph) (or spatial-precision 7))
        (setf (graph-default-spatial-max-cells graph) (or spatial-max-cells +spatial-insert-max-cells+))
        ;; REBUILD-SPATIAL-INDEXES persists the (empty, on a fresh graph) sidecar
        ;; itself once it finishes; a trailing save here would just be a redundant
        ;; duplicate of the exact same state.
        (rebuild-spatial-indexes graph)
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
                   (index-backend *index-backend*)
                   spatial-index-backend
                   ;; Default geohash precision for spatial indexes CREATED on this
                   ;; graph (MAKE-GRAPH takes the same keyword).  Existing indexes
                   ;; reopen at their own persisted precision from the v3 sidecar,
                   ;; so this governs only indexes created after the open -- and,
                   ;; for a pre-v3 graph, the ones its migration re-derives.
                   (spatial-precision 7)
                   (spatial-max-cells +spatial-insert-max-cells+))
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
             :spatial-index-backend spatial-index-backend
             :views
             #+sbcl (make-hash-table :synchronized t)
             #+ccl (make-hash-table :shared t)
             #+lispworks (make-hash-table :single-thread nil)
             #+ecl (make-hash-table #+graph-db-ecl-sync-hash :synchronized
                                     #+graph-db-ecl-sync-hash t)
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
        (setf (graph-default-spatial-precision graph) (or spatial-precision 7))
        (setf (graph-default-spatial-max-cells graph) (or spatial-max-cells +spatial-insert-max-cells+))
        ;; Spatial indexes: reopen from the v5 sidecar by root address (no node
        ;; scan, and -- critically -- no fresh allocation orphaning last run's
        ;; ordered maps in a region GC-HEAP never sweeps).
        (if (restore-spatial-index-roots graph)
            (report-degraded-spatial-indexes graph)
            (progn
              ;; No current sidecar: a fresh graph, a pre-v3 one whose single
              ;; index must be re-derived per (owner . slot), a v3 one whose
              ;; multipolygon cells predate GH #103, or a v4 one whose entries
              ;; carry no type tag (GH #104).  Index only -- node data is
              ;; untouched and nothing is re-fetched.
              (when (probe-file (spatial-index-root-file (location graph)))
                (log:info "Spatial index sidecar is pre-v3; re-deriving per-class ~
                           indexes from live node geometries (index only)."))
              ;; REBUILD-SPATIAL-INDEXES persists the completed sidecar itself; a
              ;; trailing save here would just be a redundant duplicate.  This
              ;; matters more than usual on THIS path: it is the automatic pre-v3
              ;; migration that runs before .DIRTY is written below, so a crash
              ;; here must leave the sidecar readable as INCOMPLETE, not as a
              ;; complete-looking empty one -- see SAVE-SPATIAL-INDEX-ROOTS.
              (rebuild-spatial-indexes graph)))
        ;; Reconcile the declared :SPATIAL-PRECISION against what just came back:
        ;; rebuild any index whose declared grid precision no longer matches the
        ;; persisted one (a mixed-precision index would silently miss on query).
        ;; This CREATES nothing -- indexes are lazy, on first geometry-valued
        ;; insert -- it only re-grids one already-present when its declaration
        ;; changed while the graph was closed.
        (install-spatial-indexes graph)
        ;; Vector segments (Phase 2 step 6): reopen-or-rebuild, same story as
        ;; the spatial index.  Runs after UPDATE-SCHEMA so node classes are
        ;; instantiated/finalized, and before the graph starts taking writes.
        (restore-vector-segments graph)
        (with-open-file (out dirty-file :direction :output)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph)
        (when gc-heap-p
          (gc-heap graph))
        ;; A non-empty WAL tail means this open is a CRASH RECOVERY: the .txn files
        ;; RECOVER-TRANSACTIONS is about to replay were durable but never cleanly
        ;; applied.  Capture that BEFORE the replay marks/consumes them.
        (let ((crash-recovery-p (and (recovery-transaction-files graph) t)))
          (recover-transactions graph)
          ;; The spatial index restored above came from a sidecar written at the
          ;; last CLEAN close -- it predates the writes just replayed, and its
          ;; histogram cannot be repaired by replay, because replay's idempotent
          ;; (add-unless-present) inserts hit cells that may already be on disk and
          ;; so skip %COUNT-CELL.  A too-fine clamp would then silently miss a
          ;; coarsely-stored geometry.  Re-derive every spatial index from the now-
          ;; authoritative (replayed) nodes; this reconstructs the histogram exactly
          ;; and re-persists the sidecar.  Only on recovery -- a clean open has no
          ;; WAL tail, so the fast sidecar restore above stands untouched.  (This is
          ;; why the commit path no longer writes the sidecar on a coarsening or an
          ;; index creation: crash-correctness comes from this rebuild, not from
          ;; CL-STORE I/O under the transaction-manager lock.)
          (when crash-recovery-p
            (rebuild-spatial-indexes graph)))
        ;; Unique constraints (issue #6): reopen the persistent unique skip-lists from
        ;; the sidecar (durable, no scan); only rebuild from nodes if there is no
        ;; sidecar -- a fresh graph, or a crash before CLOSE-GRAPH saved the roots.
        (unless (restore-unique-index-roots graph)
          (rebuild-unique-indexes graph))
        ;; General ordered indexes: same reopen-or-rebuild story as unique.
        (unless (restore-secondary-index-roots graph)
          (rebuild-secondary-indexes graph))
        ;; Build any def-index'd index not covered by the sidecar (declared before
        ;; this graph existed, or added since the last close); no-op otherwise.
        (install-secondary-indexes graph))
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
    (save-secondary-index-roots graph)
    ;; Spatial indexes (v3 sidecar): the addresses are already durable from
    ;; creation, but the precision histogram is only in RAM, so this close is
    ;; what makes the coarsest-precision clamp survive the reopen intact.
    (save-spatial-index-roots graph)
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
    ;; Vector segments (Phase 2 step 6): close every registered segment (each
    ;; marks itself clean on close, read back by RESTORE-VECTOR-SEGMENTS as
    ;; the open-as-is-vs-rebuild decision on the next open).
    (maphash (lambda (k seg) (declare (ignore k)) (close-vector-segment seg))
             (vector-segments graph))
    (clrhash (vector-segments graph))
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
