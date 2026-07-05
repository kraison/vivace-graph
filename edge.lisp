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
                     commit-epoch prev-pointer (class 'edge))
  ;; ECL ONLY: construct the target CLASS directly (ECL's CHANGE-CLASS leaks per
  ;; call -- #47), giving up the node buffer pool on ECL.  SBCL/CCL/LispWorks
  ;; keep the pooled base EDGE + CHANGE-CLASS path (no leak, pool is a perf win).
  (let ((edge #+ecl (let ((*initializing-node* t)) (make-instance class))
              #-ecl (get-edge-buffer)))
    (cond (id
           (setf (id edge) id))
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
                   :data data)))
          (setf (bytes e) bytes)
          (handler-case
              (create-node e graph)
            (duplicate-key-error (c)
              (if retry-p
                  (let ((*print-pretty* nil))
                    (log:error "EDGE: Duplicate key error: ~A. Retrying MAKE-EDGE"
                               (id e))
                    (make-edge type from to weight data
                               :id (gen-edge-id)
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

(defmethod active-edge-p ((edge edge) &key (graph *graph*))
  (and (not (deleted-p edge))
       (let ((from (lookup-vertex (from edge) :graph graph)))
         (if (vertex-p from)
             (not (deleted-p from))
             nil))
       (let ((to (lookup-vertex (to edge) :graph graph)))
         (if (vertex-p to)
             (not (deleted-p to))
             nil))))

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
                          (or include-deleted-p (active-edge-p edge)))
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
                             (or include-deleted-p (active-edge-p edge))
                             (not (member (type-id edge) excluded)))
                    (setf (id edge) (car pair))
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

(defmethod compact-edges ((graph graph))
  (map-edges (lambda (edge)
               (unless (active-edge-p edge)
                 (unless (deleted-p edge)
                   (delete-edge edge :graph graph))
                 (remove-from-type-index edge graph)
                 (remove-from-ve-index edge graph)
                 (remove-from-vev-index edge graph)))
             graph
             :include-deleted-p t))
