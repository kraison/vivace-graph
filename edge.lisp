(in-package :graph-db)

(alexandria:define-constant +edge-header-size+
    ;; Size, in bytes, of the standard node header, plus two vertex
    ;; ids and a 64-bit weight
    (+ +node-header-size+
       16
       16
       8))

(defclass edge (node)
  ((from :accessor from :initform +null-key+ :initarg :from
         :type (simple-array (unsigned-byte 8) (16))
         :persistent nil :ephemeral nil :meta t)
   (to :accessor to :initform +null-key+ :initarg :to
       :type (simple-array (unsigned-byte 8) (16))
       :persistent nil :ephemeral nil :meta t)
   (weight :accessor weight :initform 1.0 :initarg :weight :type float
           :persistent nil :ephemeral nil :meta t))
  (:metaclass node-class))

(defmethod print-object ((node node) stream)
  (format stream "#<~A ~S REV ~S (~S -> ~S)>"
          (type-of node) (string-id (id node))
          (revision node) (string-id (from node))
          (string-id (to node))))

(defun %make-edge (&key id type-id revision deleted-p data-pointer data bytes from
                     to weight written-p heap-written-p type-idx-written-p
                     ve-written-p vev-written-p views-written-p
                     commit-epoch prev-pointer (class 'edge) graph)
  ;; ECL ONLY: construct the target CLASS directly (ECL's CHANGE-CLASS leaks per
  ;; call -- #47), giving up the node buffer pool on ECL.  SBCL/CCL/LispWorks
  ;; keep the pooled base EDGE + CHANGE-CLASS path (no leak, pool is a perf win).
  (let ((edge #+ecl (let ((*initializing-node* t)) (make-instance class))
              #-ecl (get-edge-buffer)))
    ;; GET-EDGE-BUFFER may hand back a pool-warmed instance that already
    ;; carries an untagged v5 id (MAKE-EDGE-BUFFER has no graph to tag
    ;; with) -- the +NULL-KEY+ check alone would never fire and every
    ;; tagged store would silently get untagged ids.  A known store tag
    ;; always wins and mints fresh, even over a pooled id (GH #169).
    (cond (id
           (setf (id edge) id))
          ((and graph (store-id graph))
           (setf (id edge) (gen-edge-id (store-id graph))))
          ((equalp +null-key+ (id edge))
           (setf (id edge) (gen-edge-id))))
    (when from (setf (from edge) from))
    (when to (setf (to edge) to))
    (when weight (setf (weight edge) weight))
    (when type-id (setf (type-id edge) type-id))
    (when revision (setf (revision edge) revision))
    (when commit-epoch (setf (commit-epoch edge) commit-epoch))
    (when prev-pointer (setf (prev-pointer edge) prev-pointer))
    ;; Flags
    (when deleted-p (setf (deleted-p edge) deleted-p))
    (when written-p (setf (written-p edge) written-p))
    (when heap-written-p (setf (heap-written-p edge) heap-written-p))
    (when type-idx-written-p (setf (type-idx-written-p edge) type-idx-written-p))
    (when ve-written-p (setf (ve-written-p edge) ve-written-p))
    (when vev-written-p (setf (vev-written-p edge) vev-written-p))
    (when views-written-p (setf (views-written-p edge) views-written-p))

    (when data-pointer (setf (data-pointer edge) data-pointer))
    (when data (setf (data edge) data))
    (when bytes (setf (bytes edge) bytes))
    ;; Non-ECL: promote the pooled base EDGE to its subclass (unchanged; no leak
    ;; on these impls).  On ECL EDGE is already CLASS.
    #-ecl (change-node-class edge class)
    ;; ECL: EDGE is already CLASS; run the initform pass CHANGE-CLASS
    ;; would have performed (GH #312).
    #+ecl (%apply-missing-initforms edge)
    edge))

(defun serialize-edge-head (mf e offset)
  ;; Build the whole edge head (node head + from + to + weight) in one vector
  ;; and move it with a single SET-BYTES (see SERIALIZE-NODE-HEAD).
  (let ((vec (make-byte-vector +edge-header-size+))
        (i 0))
    (setq i (pack-node-head vec 0 e))      ;; i now past the node head
    (replace vec (from e) :start1 i)       (incf i 16)
    (replace vec (to e)   :start1 i)       (incf i 16)
    (pack-uint vec i (ieee-floats:encode-float64 (weight e)) 8)
    (set-bytes mf vec offset +edge-header-size+)
    (+ offset (1- +edge-header-size+))))

(defun deserialize-edge-head (mf offset)
  (multiple-value-bind
        (deleted-p written-p heap-written-p type-idx-written-p views-written-p
                   ve-written-p vev-written-p type-id revision pointer
                   commit-epoch prev-pointer offset)
      (funcall *node-head-reader* mf offset)
    (let* ((subclass (if (eq type-id 0)
                         'edge
                         (let ((type-meta (lookup-node-type-by-id
                                           type-id :edge)))
                           (node-type-name type-meta))))
           (e (%make-edge
               :class subclass
               :deleted-p deleted-p
               :written-p written-p
               :heap-written-p heap-written-p
               :type-idx-written-p type-idx-written-p
               :views-written-p views-written-p
               :ve-written-p ve-written-p
               :vev-written-p vev-written-p
               :type-id type-id
               :revision revision
               :data-pointer pointer
               :commit-epoch commit-epoch
               :prev-pointer prev-pointer
               :from (let ((vec (get-buffer 16)))
                       (dotimes (i 16)
                         (setf (aref vec i) (get-byte mf (incf offset))))
                       vec)
               :to (let ((vec (get-buffer 16)))
                     (dotimes (i 16)
                       (setf (aref vec i) (get-byte mf (incf offset))))
                     vec)
               :weight (let ((int 0))
                         (dotimes (i 8)
                           (setq int (dpb (get-byte mf (incf offset))
                                          (byte 8 (* i 8)) int)))
                         (ieee-floats:decode-float64 int)))))
      e)))

(defun make-edge-table (location &key (key-test 'uuid-array-equal)
                                   (base-buckets (expt 2 18)))
  (let ((table
         (make-lhash :test key-test
                     :location location
                     :value-bytes +edge-header-size+
                     :bucket-size 24
                     :buckets base-buckets
                     :key-serializer 'serialize-key
                     :key-deserializer 'deserialize-key
                     :value-serializer 'serialize-edge-head
                     :value-deserializer 'deserialize-edge-head)))
    table))

(defmethod lookup-edge ((id string) &key (graph *graph*))
  (lookup-edge (read-id-array-from-string id) :graph graph))

(defmethod lookup-edge ((id array) &key (graph *graph*))
  "Return the edge with the given ID (a 16-byte id array or its string form) in
GRAPH, or NIL if none.  Returns it regardless of its deleted flag; the
generated LOOKUP-<type> functions filter deleted edges."
  (lookup-object id (edge-table graph) *transaction* graph))

(defmethod add-to-ve-index ((edge edge) (graph graph) &key unless-present)
  (let ((in-ve-key (make-ve-key :id (to edge) :type-id (type-id edge)))
        (out-ve-key (make-ve-key :id (from edge) :type-id (type-id edge))))
    (ve-index-push (ve-index-in graph) in-ve-key (id edge)
                   :unless-present unless-present :heap (heap graph))
    (ve-index-push (ve-index-out graph) out-ve-key (id edge)
                   :unless-present unless-present :heap (heap graph))))

(defmethod remove-from-ve-index ((edge edge) (graph graph))
  (let ((in-ve-key (make-ve-key :id (to edge) :type-id (type-id edge)))
        (out-ve-key (make-ve-key :id (from edge) :type-id (type-id edge))))
    (ve-index-remove (ve-index-in graph) in-ve-key (id edge))
    (ve-index-remove (ve-index-out graph) out-ve-key (id edge))))

(defmethod add-to-vev-index ((edge edge) (graph graph) &key unless-present)
  (let ((vev-key (make-vev-key :in-id (to edge)
                               :out-id (from edge)
                               :type-id (type-id edge)))
        (table (vev-index-table (vev-index graph))))
    ;;(log:debug "add-to-vev-index: ~A" vev-key)
    ;;(log:debug "add-to-vev-index: EDGE: ~A" edge)
    (with-locked-hash-key (table vev-key)
      (let ((index-list (%lhash-get table vev-key)))
        (if index-list
            (progn
              ;;(log:debug "add-to-vev-index: Got ~A" index-list)
              (if unless-present
                  (index-list-pushnew (id edge) index-list)
                  (index-list-push (id edge) index-list))
              (%lhash-update table vev-key index-list)
              ;;(log:debug "add-to-vev-index: AFTER PUSH: ~A" index-list)
              )
            (progn
              (setq index-list
                    (make-index-list (heap graph) (id edge)))
              ;;(log:debug "add-to-vev-index: Made new ~A" index-list)
              (%lhash-insert table vev-key index-list)))
        (cache-index-list (vev-index graph) vev-key index-list)))))

(defmethod remove-from-vev-index ((edge edge) (graph graph))
  (let ((vev-key (make-vev-key :in-id (to edge)
                               :out-id (from edge)
                               :type-id (type-id edge)))
        (table (vev-index-table (vev-index graph))))
    (with-locked-hash-key (table vev-key)
      (let ((index-list (%lhash-get table vev-key)))
        (when index-list
          ;;(log:debug "Removing ~A from ~A" edge index-list)
          (remove-from-index-list (id edge) index-list)
          (%lhash-update table vev-key index-list)
          (cache-index-list (vev-index graph) vev-key index-list))))))

(defmethod add-to-type-index ((edge edge) (graph graph) &key unless-present)
  (type-index-push (id edge) (type-id edge) (edge-index graph)
                   :unless-present unless-present))

(defmethod remove-from-type-index ((edge edge) (graph graph))
  (type-index-remove (id edge) (type-id edge) (edge-index graph)))

(defun make-edge (type from to weight data &key id revision deleted-p
                  retry-p
                  (graph *graph*))
  "Create and persist an edge of the type named/identified by TYPE (a node type
name, integer id, or :GENERIC) from vertex FROM to vertex TO in GRAPH, with the
given WEIGHT and slot DATA; return it.  FROM and TO may be vertices, id arrays,
or id strings.  Must run inside a transaction.  You normally call the generated
MAKE-<type> constructor (e.g. (MAKE-FOLLOWS :FROM a :TO b)) instead.  :RETRY-P
regenerates the id on a duplicate-key collision."
  (when (stringp id)
    (setq id (read-id-array-from-string id)))
  (typecase from
    (string (setq from (read-id-array-from-string from)))
    (vertex (setq from (id from))))
  (typecase to
    (string (setq to (read-id-array-from-string to)))
    (vertex (setq to (id to))))
  (let ((type-meta (or (and (eq type :generic) :generic)
                       (and (eq 0 type) :generic)
                       (and (integerp type)
                            (lookup-node-type-by-id type :edge :graph graph))
                       (lookup-node-type-by-name type :edge :graph graph))))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'edge
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (e (%make-edge
                   :class subclass
                   :id id ;; (or id (gen-edge-id))
                   :type-id (if (eq type-meta :generic)
                                0
                                (node-type-id type-meta))
                   :revision (or revision 0)
                   :deleted-p deleted-p
                   :written-p nil
                   :from from
                   :to to
                   :weight weight
                   :bytes bytes
                   :data data
                   :graph graph)))
          (setf (bytes e) bytes)
          ;; Stamped from birth: the edge is live for the whole creating
          ;; transaction, long before commit stamps it (GH #53).
          (setf (node-graph e) graph)
          (handler-case
              (create-node e graph)
            (duplicate-key-error (c)
              (if retry-p
                  (let ((*print-pretty* nil))
                    (log:error "EDGE: Duplicate key error: ~A. Retrying MAKE-EDGE"
                               (id e))
                    (make-edge type from to weight data
                               :id (gen-edge-id (and graph (store-id graph)))
                               :revision revision
                               :deleted-p deleted-p :graph graph))
                  (error c)))))
        (error "Unknown edge type ~A" type))))

(defmethod copy-edge ((edge edge))
  (let ((e (copy-node edge)))
    (setf (slot-value e 'from) (slot-value edge 'from)
          (slot-value e 'to) (slot-value edge 'to)
          (slot-value e 'weight) (slot-value edge 'weight))
    e))

(defmethod save-edge ((edge edge) &key (graph *graph*))
  ;; you must copy the edge before writing to its slots,
  ;; in case others are reading it!
  (let ((class-name (class-name (class-of edge))))
    (if (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (multiple-value-bind (new old)
              (save-node edge (edge-table graph) :graph graph)
            (%update-in-views graph new old class-name)
            new))
        (multiple-value-bind (new old)
            (save-node edge (edge-table graph) :graph graph)
          (declare (ignore old))
          new))))

(defmethod delete-edge ((edge edge) &key (graph *graph*))
  (when (deleted-p edge)
    (error 'edge-already-deleted-error
           :node edge))
  (delete-node edge graph))

(defun %another-store-open-p (graph)
  "True when any open graph other than GRAPH is registered in *GRAPHS*.
Gates the v5 cross-store scan in %ACTIVE-ENDPOINT-STATUS (GH #208)."
  (maphash (lambda (name g)
             (declare (ignore name))
             (when (and (not (eq g graph)) (graph-open-p g))
               (return-from %another-store-open-p t)))
           *graphs*)
  nil)

(defun %active-endpoint-status (id graph)
  "(values VERTEX-OR-NIL STATUS) for edge endpoint ID against GRAPH,
for ACTIVE-EDGE-P and COMPACT-EDGES.  STATUS is one of:
  :FOUND    -- a live table holds the vertex (GRAPH's own, or another
     open store's -- including a v5 id found by the all-open-stores
     scan, which runs on a miss whenever another store is open;
     measured ~3.5us per open store, affordable here (GH #208)).
  :MISSING  -- disproved without trusting a tag: a same-store v8
     miss, or a v5 miss after (or with no stores to) scan -- a
     completed v5 scan IS disproof, there is no tag to mistrust,
     though it covers OPEN stores only: a v5 vertex in a detached
     store is invisible (a tagless id cannot name it) and its edge
     compacts even under :CONSERVATIVE.
  :ABSENT-IN-STORE -- the tag resolves to an open store whose own
     table misses.
  :DETACHED -- the registry knows the tag; its store is not open.
  :UNKNOWN  -- this system's registry never assigned the tag.
Trap (GH #208, #209): a tag indexes THIS system's registry, but ids
travel -- peer hubs hold device-minted v8 ids verbatim, restores cross
systems -- so :ABSENT-IN-STORE and :UNKNOWN are disproof only where
the tag is trusted.  ACTIVE-EDGE-P counts both (and :DETACHED) as
live; only COMPACT-EDGES' explicit :TRUST-TAGS policy collects them.
RESOLVE-NODE-GRAPH / LOOKUP-VERTEX-ANYWHERE live in interface.lisp,
loaded after this file; resolved at runtime like PIN-READ-EPOCH in
graph-class.lisp."
  (let ((v (lookup-vertex id :graph graph)))
    (if v
        (values v :found)
        (let ((tag (id-store-tag id)))
          (cond
            ((and tag (not (eql tag (store-id graph))))
             (multiple-value-bind (other status) (resolve-node-graph id)
               (ecase status
                 (:resolved
                  (let ((r (lookup-vertex id :graph other)))
                    (if r
                        (values r :found)
                        (values nil :absent-in-store))))
                 (:detached (values nil :detached))
                 (:unknown (values nil :unknown)))))
            ((and (null tag) (%another-store-open-p graph))
             (let ((r (lookup-vertex-anywhere id)))
               (if (vertex-p r)
                   (values r :found)
                   (values nil :missing))))
            (t (values nil :missing)))))))

(defmethod active-edge-p ((edge edge) &key (graph *graph*))
  (flet ((endpoint-active-p (id)
           (multiple-value-bind (v status) (%active-endpoint-status id graph)
             (ecase status
               (:found (not (deleted-p v)))
               ;; Not disprovable without trusting the tag -> live
               ;; (GH #208, #209); COMPACT-EDGES :TRUST-TAGS is the
               ;; explicit opt-in.
               ((:detached :unknown :absent-in-store) t)
               (:missing nil)))))
    (and (not (deleted-p edge))
         (endpoint-active-p (from edge))
         (endpoint-active-p (to edge)))))

(defmethod edge-exists-p (edge-type (vertex1 vertex) (vertex2 vertex)
                          &key (graph *graph*))
  (let ((type-meta (or (and (integerp edge-type)
                            (lookup-node-type-by-id edge-type :edge :graph graph))
                       (lookup-node-type-by-name edge-type :edge :graph graph))))
    (when type-meta
      (let* ((vev-key (make-vev-key :in-id (id vertex2)
                                    :out-id (id vertex1)
                                    :type-id (node-type-id type-meta)))
             (index-list (lookup-vev-index-list vev-key graph)))
        (when index-list
          (map-index-list
           (lambda (edge-id)
             (let ((edge (lookup-edge edge-id :graph graph)))
               (when (and edge (written-p edge)
                          (active-edge-p edge :graph graph))
                 (return-from edge-exists-p edge))))
           index-list))))))

(defun map-edges (fn graph &key collect-p edge-type include-edge-types vertex
                             direction include-deleted-p to-vertex from-vertex
                             exclude-edge-types (include-subclasses-p t))
  "Call FN on edges of GRAPH.

Narrow the set with :EDGE-TYPE (a single type name or numeric type-id) and/or
:INCLUDE-EDGE-TYPES (a list of either) -- their union is visited; with no type
given, EVERY edge type is visited.  :EXCLUDE-EDGE-TYPES (a list) removes types
from that set.  Unless :INCLUDE-SUBCLASSES-P is NIL (default T) each named type
also matches its subtypes (see RESOLVE-NODE-TYPE-IDS) -- mirroring MAP-VERTICES.
Restrict to a vertex's adjacent edges with :VERTEX plus :DIRECTION (:OUT or :IN),
or to a specific endpoint pair with :FROM-VERTEX and :TO-VERTEX.  Deleted edges
are skipped unless :INCLUDE-DELETED-P.  With :COLLECT-P, collect and return FN's
values; otherwise return NIL.  This drives OUTGOING-EDGES / INCOMING-EDGES.

Per-type walks OVERLAP by default: :INCLUDE-SUBCLASSES-P T expands a parent type
over its subtypes, so summing parent + subtypes double-counts them.  A per-type
sum is comparable to the untyped total only with :INCLUDE-SUBCLASSES-P NIL on
every non-leaf type (GH #219).

NOTE: the fully-untyped, non-adjacency scan (no type and no vertex/endpoint)
walks the raw edge lhash, which reads LIVE edge versions and so BYPASSES MVCC
snapshot isolation -- intended for back-end / admin passes run while the graph is
quiescent.  Every typed or adjacency scan goes through an index + LOOKUP-EDGE and
is snapshot-consistent.  (Generic, type-0 edges appear only in this untyped scan;
typed/adjacency scans skip the 0 sentinel, as they always have.)"
  ;; Bind *GRAPH* to GRAPH so the value-deserializer (deserialize-edge-head)
  ;; resolves type-ids against the right schema even when mapping a graph that
  ;; isn't the current *GRAPH* (see the note in MAP-VERTICES).
  (let* ((result nil)
         (*graph* graph)
         ;; Collected edges escape the scan pin -> materialize before FN; a
         ;; side-effect scan runs FN inside the pin so its lazy reads are safe.
         (fn (if collect-p
                 (let ((user-fn fn))
                   (lambda (e) (ensure-node-bytes e graph) (funcall user-fn e)))
                 fn))
         (requested (append (when edge-type (list edge-type)) include-edge-types))
         (excluded (when exclude-edge-types
                     (resolve-node-type-ids exclude-edge-types :edge
                                            :include-subclasses-p include-subclasses-p
                                            :graph graph)))
         ;; Type-ids to scan: the resolved (subclass-expanded) union for a typed
         ;; query, or EVERY edge type for an untyped one.  The all-types list is
         ;; deliberately NOT subclass-expanded -- each concrete type-id is visited
         ;; exactly once, the guard against double-counting a subtype (which would
         ;; otherwise be hit directly AND under its parent).
         (type-ids (if requested
                       (resolve-node-type-ids requested :edge
                                              :include-subclasses-p include-subclasses-p
                                              :graph graph)
                       (list-edge-types graph))))
    (with-read-pin (graph) ; retain whatever versions this scan observes
      (flet ((emit (edge)
               (when (and edge (written-p edge)
                          ;; Explicit :GRAPH, not dynamic *GRAPH* --
                          ;; the wrong-graph pattern (GH #208 unit).
                          (or include-deleted-p
                              (active-edge-p edge :graph graph)))
                 (if collect-p (push (funcall fn edge) result) (funcall fn edge))))
             (keep-type (tid) (and (plusp tid) (not (member tid excluded)))))
        (cond
          ;; a specific endpoint pair -> vev-index per type-id
          ((and to-vertex from-vertex)
           (dolist (tid type-ids)
             (when (keep-type tid)
               (let* ((vev-key (make-vev-key :in-id (id to-vertex)
                                             :out-id (id from-vertex)
                                             :type-id tid))
                      (il (lookup-vev-index-list vev-key graph)))
                 (when il
                   (map-index-list
                    (lambda (eid) (emit (lookup-edge eid :graph graph))) il))))))
          ;; a vertex's adjacent edges -> ve-index (in/out) per type-id
          (vertex
           (dolist (tid type-ids)
             (when (keep-type tid)
               (let* ((ve-key (make-ve-key :id (id vertex) :type-id tid))
                      (il (cond ((eq direction :out)
                                 (lookup-ve-out-index-list ve-key graph))
                                ((eq direction :in)
                                 (lookup-ve-in-index-list ve-key graph))
                                (t (error "Unknown direction: ~S" direction)))))
                 (when il
                   (map-index-list
                    (lambda (eid) (emit (lookup-edge eid :graph graph))) il))))))
          ;; typed, no adjacency -> type-index per type-id
          (requested
           (dolist (tid type-ids)
             (when (keep-type tid)
               (let ((il (get-type-index-list (edge-index graph) tid)))
                 (when il
                   (map-index-list
                    (lambda (eid) (emit (lookup-edge eid :graph graph))) il))))))
          ;; fully untyped -> live lhash scan (see NOTE); per-edge exclude
          (t
           (map-lhash
            #'(lambda (pair)
                (let ((edge (cdr pair)))
                  (when (and edge (written-p edge)
                             ;; Explicit :GRAPH -- see EMIT above.
                             (or include-deleted-p
                                 (active-edge-p edge :graph graph))
                             (not (member (type-id edge) excluded)))
                    (setf (id edge) (car pair))
                    ;; The deserializer builds these; a side-effect scan never
                    ;; sees ENSURE-NODE-BYTES (GH #53).
                    (setf (node-graph edge) graph)
                    (if collect-p (push (funcall fn edge) result) (funcall fn edge)))))
            (edge-table graph))))))
    (when collect-p (nreverse result))))

(defmethod outgoing-edges ((vertex vertex) &key (graph *graph*) edge-type
                                             include-edge-types
                                             (include-subclasses-p t)
                                             include-deleted-p)
  "Return a list of edges directed out of VERTEX (i.e. whose FROM is VERTEX) in
GRAPH.  :EDGE-TYPE restricts to one edge type and :INCLUDE-EDGE-TYPES to a list
of types (their union); with neither, all edge types are returned.  Unless
:INCLUDE-SUBCLASSES-P is NIL (default T) each named type also matches its
subtypes.  :INCLUDE-DELETED-P includes soft-deleted edges (excluded by default)."
  (map-edges 'identity graph :vertex vertex :edge-type edge-type
             :include-edge-types include-edge-types
             :include-subclasses-p include-subclasses-p :direction :out
             :collect-p t :include-deleted-p include-deleted-p))

(defmethod incoming-edges ((vertex vertex) &key (graph *graph*) edge-type
                                             include-edge-types
                                             (include-subclasses-p t)
                                             include-deleted-p)
  "Return a list of edges directed into VERTEX (i.e. whose TO is VERTEX) in
GRAPH.  :EDGE-TYPE restricts to one edge type and :INCLUDE-EDGE-TYPES to a list
of types (their union); with neither, all edge types are returned.  Unless
:INCLUDE-SUBCLASSES-P is NIL (default T) each named type also matches its
subtypes.  :INCLUDE-DELETED-P includes soft-deleted edges (excluded by default)."
  (map-edges 'identity graph :vertex vertex :edge-type edge-type
             :include-edge-types include-edge-types
             :include-subclasses-p include-subclasses-p :direction :in
             :collect-p t :include-deleted-p include-deleted-p))

;; Ill-typed dispatch on the adjacency generics signals the engine's own
;; QUERY-PRECONDITION-ERROR instead of the implementation's
;; no-applicable-method condition -- ECL's is a bare SIMPLE-ERROR the GUI
;; classifier cannot distinguish from a defect (GH #309).
(defmethod no-applicable-method ((gf (eql #'outgoing-edges)) &rest args)
  (declare (ignore args))
  (error 'query-precondition-error
         :reason "OUTGOING-EDGES needs a vertex as its first argument"))

(defmethod no-applicable-method ((gf (eql #'incoming-edges)) &rest args)
  (declare (ignore args))
  (error 'query-precondition-error
         :reason "INCOMING-EDGES needs a vertex as its first argument"))

(define-condition compact-trust-tags-on-peer-error (error)
  ((graph :initarg :graph :reader compact-trust-tags-peer-graph))
  (:report
   (lambda (c s)
     (format s "COMPACT-EDGES :POLICY :TRUST-TAGS refused on peer ~
graph ~S: peer tables hold foreign-minted v8 ids whose tags index ~
ANOTHER system's registry, so an unrecognized tag is not disproof of ~
liveness there (GH #209)."
             (compact-trust-tags-peer-graph c)))))

(define-condition compact-trust-tags-no-registry-error (error)
  ((graph :initarg :graph :reader compact-trust-tags-registry-graph))
  (:report
   (lambda (c s)
     (format s "COMPACT-EDGES :POLICY :TRUST-TAGS refused on ~S: no ~
store registry is loaded (*SYSTEM-DIRECTORY* is NIL), so a ~
registered-but-detached tag cannot be told apart from an unassigned ~
one -- :TRUST-TAGS would delete edges to reattachable stores ~
(GH #208, #209)."
             (compact-trust-tags-registry-graph c)))))

(defmethod compact-edges ((graph graph) &key (policy :conservative))
  "Delete and de-index GRAPH's edges whose endpoints are disproved.
:POLICY :CONSERVATIVE (default) compacts only tag-free disproof --
a soft-deleted edge's indexes, a :MISSING endpoint, or a :FOUND
endpoint that is itself deleted.  :POLICY :TRUST-TAGS additionally
compacts :UNKNOWN and :ABSENT-IN-STORE endpoints, treating this
system's tag space as authoritative -- refused on a PEER-GRAPH
(COMPACT-TRUST-TAGS-ON-PEER-ERROR), whose tables hold foreign-tagged
ids, and with no store registry loaded (*SYSTEM-DIRECTORY* NIL,
COMPACT-TRUST-TAGS-NO-REGISTRY-ERROR), where :DETACHED cannot be told
from :UNKNOWN (GH #208, #209).  A :DETACHED endpoint is never
compacted under either policy: its store may reattach.  Returns NIL.
Trap: runs the untyped live-lhash scan -- use on a quiescent graph
only."
  (ecase policy ((:conservative :trust-tags)))
  (when (eq policy :trust-tags)
    (when (typep graph 'peer-graph)
      (error 'compact-trust-tags-on-peer-error :graph graph))
    ;; No registry -> :detached is unreachable (RESOLVE-NODE-GRAPH
    ;; needs *SYSTEM-DIRECTORY* to confirm a tag), so every detached
    ;; tag would classify :UNKNOWN and be collected (GH #208, #209).
    (unless *system-directory*
      (error 'compact-trust-tags-no-registry-error :graph graph)))
  (flet ((endpoint-dead-p (id)
           (multiple-value-bind (v status) (%active-endpoint-status id graph)
             (ecase status
               (:found (deleted-p v))
               (:missing t)
               (:detached nil)
               ((:unknown :absent-in-store) (eq policy :trust-tags))))))
    (map-edges (lambda (edge)
                 (when (or (deleted-p edge)
                           (endpoint-dead-p (from edge))
                           (endpoint-dead-p (to edge)))
                   (unless (deleted-p edge)
                     (delete-edge edge :graph graph))
                   (remove-from-type-index edge graph)
                   (remove-from-ve-index edge graph)
                   (remove-from-vev-index edge graph)))
               graph
               :include-deleted-p t)))
