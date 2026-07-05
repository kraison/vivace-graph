(in-package :graph-db)

(defvar *graphs*
  #+sbcl
  (make-hash-table :test 'equal :synchronized t)
  #+lispworks
  (make-hash-table :test 'equal :single-thread nil)
  #+ccl
  (make-hash-table :test 'equal :shared t)
  #+ecl
  (make-hash-table :test 'equal))

(defclass graph ()
  ((graph-name :accessor graph-name :initarg :graph-name)
   (graph-open-p :accessor graph-open-p :initarg :graph-open-p :initform nil)
   (location :accessor location :initarg :location)
   (txn-log :accessor txn-log :initarg :txn-log)
   (txn-file :accessor txn-file :initarg :txn-file)
   (txn-lock :accessor txn-lock :initarg :txn-lock :initform (make-recursive-lock))
   (transaction-manager :accessor transaction-manager :initarg :transaction-manager)
   (replication-key :accessor replication-key :initarg :replication-key)
   (replication-port :accessor replication-port :initarg :replication-port)
   (vertex-table :accessor vertex-table :initarg :vertex-table)
   (edge-table :accessor edge-table :initarg :edge-table)
   (heap :accessor heap :initarg :heap)
   (indexes :accessor indexes :initarg :indexes)
   (schema :accessor schema :initarg :schema)
   (cache :accessor cache :initarg :cache)
   (ve-index-in :accessor ve-index-in :initarg :ve-index-in)
   (ve-index-out :accessor ve-index-out :initarg :ve-index-out)
   (vev-index :accessor vev-index :initarg :vev-index)
   (vertex-index :accessor vertex-index :initarg :vertex-index)
   (edge-index :accessor edge-index :initarg :edge-index)
   (spatial-index :accessor spatial-index :initarg :spatial-index :initform nil)
   ;; Which ordered-map backend NEW heap-backed indexes (views, :unique, spatial)
   ;; on THIS graph are built with: :SKIP-LIST or :BPLUS-TREE.  Defaults to the
   ;; global *INDEX-BACKEND* at creation; overridable per graph via MAKE-GRAPH /
   ;; OPEN-GRAPH :INDEX-BACKEND.  Reopen of EXISTING indexes ignores this (each
   ;; index carries its own persisted backend tag); this only governs new ones.
   (index-backend :accessor graph-index-backend :initarg :index-backend
                  :initform *index-backend*)
   ;; Unique constraints (issue #6): (owner-class . slot-name) -> UNIQUE-INDEX.
   ;; A derived structure (v1: rebuilt on open); enforcement is at the commit
   ;; boundary.  See unique-constraint.lisp / docs/unique-constraint-design.md.
   (unique-indexes :accessor unique-indexes :initarg :unique-indexes :initform nil)
   (views-lock :accessor views-lock :initarg :views-lock
               :initform (make-recursive-lock))
   (views :accessor views :initarg :views)
   (write-stats :accessor write-stats :initarg :write-stats
                :initform
                #+ccl (make-hash-table :test 'eq :shared t)
                #+lispworks (make-hash-table :test 'eq :single-thread nil)
                #+ecl (make-hash-table :test 'eq)
                #+sbcl (make-hash-table :test 'eq :synchronized t))
   (read-stats :accessor read-stats :initarg :read-stats
               :initform
               #+ccl (make-hash-table :test 'eq :shared t)
               #+lispworks (make-hash-table :test 'eq :single-thread nil)
               #+ecl (make-hash-table :test 'eq)
               #+sbcl (make-hash-table :test 'eq :synchronized t))))

(defmethod print-object ((graph graph) stream)
  (print-unreadable-object (graph stream :type t :identity t)
    (format stream "~S ~S" (graph-name graph) (location graph))))

;; True while a read pin is held on the current thread (dynamic extent of a
;; WITH-READ-PIN).  Lets a nested LOOKUP-OBJECT skip its own eager byte
;; materialization: the enclosing pinned scope already protects the node, so its
;; data can stay lazy (or be materialized by the scan only when it escapes).
(defvar *read-pinned-p* nil)

;; MVCC read-epoch pin.  Defined here (early) because MAP-VERTICES / MAP-EDGES
;; use it and are compiled before transactions.lisp, which defines the
;; PIN-READ-EPOCH / UNPIN-READ-EPOCH functions this expands to (resolved at
;; runtime).  Holds a pin for BODY's dynamic extent so the reaper retains any
;; version BODY may dereference; a no-op when GRAPH has no transaction-manager
;; yet (open/recovery, when no reaper races).
(defmacro with-read-pin ((graph) &body body)
  (alexandria:with-gensyms (g tm tok)
    ;; A pin nested inside another (e.g. LOOKUP-VERTEX called from a MAP-VERTICES
    ;; scan) is a no-op: the outer pin was taken at an earlier (smaller) epoch, so
    ;; its floor is at least as conservative and already protects this read.  This
    ;; keeps per-node lock traffic off scans -- only the outermost reader pins.
    `(if *read-pinned-p*
         (progn ,@body)
         (let* ((,g ,graph)
                (,tm (and (slot-boundp ,g 'transaction-manager)
                          (transaction-manager ,g))))
           (if ,tm
               (let ((,tok (pin-read-epoch ,tm))
                     (*read-pinned-p* t))
                 (unwind-protect (progn ,@body)
                   (unpin-read-epoch ,tm ,tok)))
               (progn ,@body))))))

(defclass master-graph (graph)
  ((replication-mbox :accessor replication-mbox :initarg :replication-mbox)
   (replication-listener :accessor replication-listener :initarg :replication-listener)
   (stop-replication-p :accessor stop-replication-p :initarg :stop-replication-p :initform nil)
   (slaves :accessor slaves :initarg :slaves :initform ())
   (slaves-lock :accessor slaves-lock :initarg :slaves-lock :initform (make-recursive-lock))))

(defclass slave-graph (graph)
  ((master-host :accessor master-host :initarg :master-host)
   (slave-socket :accessor slave-socket :initarg :slave-socket)
   (stop-replication-p :accessor stop-replication-p :initarg :stop-replication-p :initform nil)
   (slave-thread :accessor slave-thread :initarg :slave-thread :initform nil)
   (master-txn-id :accessor master-txn-id :initarg :master-txn-id)
   ;; Subset replication: when non-nil, a predicate (NODE) -> generalized boolean.
   ;; Each replicated transaction's writes are filtered through it on apply, so
   ;; the slave holds only the subset it accepts (e.g. its area of operations).
   ;; See MAKE-SPATIAL-REPLICATION-FILTER for the spatial case.
   (replication-filter :accessor replication-filter :initarg :replication-filter
                       :initform nil)))

;; Peer replication (hub-and-spoke, offline-first).  A SIBLING of master/slave --
;; deliberately NOT a subclass of either -- so the master/slave transport is left
;; exactly as it is.  A peer-graph is a plain graph with extra peer state; until the
;; full-system transport file (peer-streaming.lisp) specializes START-REPLICATION /
;; STOP-REPLICATION on it, it inherits the base no-op methods and behaves like an
;; ordinary embedded graph.  See docs/peer-replication-design.md (design v2) and
;; docs/peer-replication-branch-a-plan.md (WP-1).
(defclass peer-graph (graph)
  ((peer-role :accessor peer-role :initarg :peer-role :initform :device
              :documentation "Either :HUB (the system of record many devices sync
              against) or :DEVICE (an offline-first replica that pulls an
              authority-scoped subgraph and -- Branch B -- pushes authored changes).")
   (origin-id :accessor origin-id :initarg :origin-id :initform nil
              :documentation "This replica's stable 16-byte origin UUID, hub-minted.
              Stamped onto authored ops and preserved across re-homing (design §3).")
   (peer-host :accessor peer-host :initarg :peer-host :initform nil
              :documentation "Device role: the hub host to connect to (the port is
              REPLICATION-PORT, inherited from GRAPH).  Unused for the hub role.")
   (export-predicate :accessor export-predicate :initarg :export-predicate :initform nil
                     :documentation "Hub role: app-supplied DISCLOSABLE-P
                     (VERTEX GRAPH DEVICE-SCOPE) -> boolean, run under the export read
                     snapshot to build each device's closed authority-scoped subgraph
                     (design §7).  The engine never embeds disclosure policy itself.")
   (device-registry :accessor device-registry :initarg :device-registry :initform nil
                    :documentation "Hub role: app-owned registry keyed by device
                    ORIGIN-ID, holding clearance scope + per-device cursors/manifest.
                    The engine only reads it.")
   (lamport-counter :accessor lamport-counter :initarg :lamport-counter :initform 0
                    :documentation "Durable, monotonic logical clock for conflict
                    ordering (Branch B).  Advanced on every authored op and to
                    MAX(local,received)+1 on receipt (PT-8).  Loaded on open, persisted
                    on every advance, so a crash cannot reset it and lose LWW races.")
   (lamport-lock :accessor lamport-lock :initarg :lamport-lock
                 :initform (make-recursive-lock "lamport clock")
                 :documentation "Serializes LAMPORT-COUNTER read-modify-write across
                 the minter (local authoring) and the observer (received ops), which
                 on a hub run on different threads.  Always innermost.")
   (field-stamps :accessor field-stamps :initarg :field-stamps :initform nil
                 :documentation "Branch B per-field Lamport stamps (B2b): node-id ->
                 alist (slot . (lamport . origin)), the (lamport,origin) of the op
                 that last wrote each field on THIS replica -- the LWW comparison
                 basis.  Behind the GET/SET-FIELD-STAMP API so the substrate (an
                 in-memory map persisted on open/close for v1) can later become the
                 heap-backed side store.  NIL until the peer branch inits it.")
   (field-stamps-lock :accessor field-stamps-lock :initarg :field-stamps-lock
                      :initform (make-recursive-lock "field-stamps")
                      :documentation "Serializes FIELD-STAMPS mutation/read (the hub
                      merges on many session threads).")
   (merge-policy :accessor merge-policy :initarg :merge-policy :initform nil
                 :documentation "Branch B: the app-supplied MERGE-POLICY (field-bucket
                 + safety-merge), consulted when an applied authored op diverges from
                 a locally-held node.  Mirrors EXPORT-PREDICATE -- domain (bucketing +
                 safety semantics) in the app, mechanism in the engine.  NIL = no
                 merge (an incoming edit just overwrites, i.e. Branch A behaviour).")
   (reference-classes :accessor reference-classes :initarg :reference-classes :initform nil
                      :documentation "Hub role: a list of vertex-type names shipped to EVERY
                      device by CLASS -- global reference data (e.g. the ordnance catalogue)
                      that is not reachable from any device's site roots.  SCOPE-NODE-SET
                      unions every disclosable vertex of these classes (subclass-inclusive)
                      into the pulled set, independent of the roots walk.")
   (peer-conflicts :accessor peer-conflicts :initarg :peer-conflicts :initform nil
                   :documentation "Branch B: surfaced field conflicts retained for the
                   app review surface (a list of PEER-CONFLICT for now; B3 makes it a
                   durable enumeration API + MVCC loser retention).")
   (peer-conflicts-lock :accessor peer-conflicts-lock :initform (make-recursive-lock "peer-conflicts"))
   (applied-op-ids :accessor applied-op-ids :initarg :applied-op-ids :initform nil
                   :documentation "Durable OP-ID -> lamport dedup index (WP-3), checked
                   before apply so a re-homed op bouncing back is not duplicated.  NIL
                   until WP-3 wires it.")
   (node-origins :accessor node-origins :initarg :node-origins :initform nil
                 :documentation "Unique-constraint :ORIGIN scope (#6): node-id ->
                 the authoring origin captured when the node was first created on
                 this replica, FIXED for the node's lifetime.  An :ORIGIN-scoped
                 unique value partitions by this origin, so two devices minting the
                 same value are distinct composite keys (no coordination).  Set-once
                 in %UIX-CLAIM, read by %NODE-ORIGIN; persisted on open/close like
                 FIELD-STAMPS.  NIL on a non-peer graph (which has one origin, so
                 :ORIGIN falls back to the graph origin).")
   (node-origins-lock :accessor node-origins-lock :initform (make-recursive-lock "node-origins"))
   (peer-schema-version :accessor peer-schema-version :initarg :peer-schema-version
                        :initform '(1 0)
                        :documentation "This replica's (MAJOR MINOR) schema version
                        for the peer handshake (WP-6).  The peer gate is a same-MAJOR
                        COMPATIBILITY check, not digest-equality: additive (minor)
                        drift still syncs degraded-safe, only a MAJOR mismatch rejects
                        the pull (design §14 / PT-6).  The major/minor bump policy is
                        app-owned -- the app SETFs this after open; the engine only
                        compares majors.")
   (peer-writer-mailbox :accessor peer-writer-mailbox :initarg :peer-writer-mailbox
                        :initform nil
                        :documentation "Device role: the single-writer funnel (WP-8).
                        The socket receive thread ENQUEUES decoded peer ops here; one
                        writer thread drains and applies them, so all device mutations
                        go through a single writer and never contend on OCC (PT-5).")
   (peer-writer-thread :accessor peer-writer-thread :initarg :peer-writer-thread
                       :initform nil)
   (peer-writer-progress :accessor peer-writer-progress :initform 0
                         :documentation "Device role: a monotonically increasing count
                        of peer ops the writer has applied.  PEER-SYNC watches it to tell
                        a slow-but-healthy apply (progress advancing) from a wedged writer
                        (alive but stalled), so the barrier wait need not race a wall clock.")
   (stop-replication-p :accessor stop-replication-p :initarg :stop-replication-p
                       :initform nil)
   (peer-thread :accessor peer-thread :initarg :peer-thread :initform nil)))

(defgeneric graph-p (thing)
  (:method ((graph graph)) graph)
  (:method (thing) nil))

(defgeneric master-graph-p (thing)
  (:method ((graph master-graph)) graph)
  (:method (thing) nil))

(defgeneric slave-graph-p (thing)
  (:method ((graph slave-graph)) graph)
  (:method (thing) nil))

(defgeneric peer-graph-p (thing)
  (:method ((graph peer-graph)) graph)
  (:method (thing) nil))

(defgeneric journals-own-feed-p (graph)
  (:documentation "True if GRAPH appends its OWN committed transactions to a
replication feed that downstream replicas consume.  A master journals for its
slaves; a peer-graph journals for hub/device sync (a device's feed is its push
feed, a hub's feed is what devices pull).  A slave does NOT journal (it only
applies an upstream feed), and a plain graph has no feed.  This is the gate used
by FINALIZE-TX-PERSISTENCE -- generalizing it from MASTER-GRAPH-P is what makes a
peer journal its writes (peer-replication WP-2).")
  (:method ((graph graph)) nil)
  (:method ((graph master-graph)) t)
  (:method ((graph peer-graph)) t))

(defgeneric init-schema (graph))
(defgeneric update-schema (graph-or-name))
(defgeneric snapshot (graph &key &allow-other-keys))
(defgeneric scan-for-unindexed-nodes (graph))
(defgeneric start-replication (graph &key package))
(defgeneric stop-replication (graph))

;; Base no-op methods: a plain (non-replicated) graph has no transport, but
;; make-graph/open-graph always call these.  They live here in the core so the
;; embeddable engine (graph-db/core) can open a graph without the network
;; replication transport.  The real master-graph/slave-graph methods (usocket
;; listener/slave threads) are in transaction-streaming.lisp (full graph-db).
(defmethod start-replication ((graph graph) &key package)
  (declare (ignore package))
  ;; noop
  )

(defmethod stop-replication ((graph graph))
  ;; noop
  )

(defun lookup-graph (name)
  (gethash name *graphs*))
