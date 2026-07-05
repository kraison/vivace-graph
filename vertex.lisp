(in-package :graph-db)

(defclass vertex (node)
  ()
  (:metaclass node-class))

(defmethod print-object ((node vertex) stream)
  (format stream "#<~A ~S REV ~S>"
          (type-of node) (string-id (id node))
          (revision node)))

(defgeneric vertex-p (thing)
  (:method ((thing vertex)) t)
  (:method (thing) nil))

(defun %make-vertex (&key id type-id revision deleted-p data-pointer data bytes
                     written-p heap-written-p type-idx-written-p views-written-p
                     commit-epoch prev-pointer (class 'vertex))
  ;; ECL ONLY: construct the target CLASS directly, because ECL's CHANGE-CLASS
  ;; retains memory on every call -- a base+change-class per node read leaked
  ;; ~unboundedly (#47).  This gives up the node buffer pool on ECL (a perf
  ;; trade), which is fine: the pooled base instances only ever existed to be
  ;; CHANGE-CLASSed.  SBCL/CCL/LispWorks have no such leak, so they KEEP the
  ;; pooled base buffer + CHANGE-CLASS path unchanged (the pool is a real perf
  ;; win there).  *INITIALIZING-NODE* is bound as CHANGE-NODE-CLASS does, so
  ;; persistent-slot init leaves the (empty) data alist alone.
  (let ((vertex #+ecl (let ((*initializing-node* t)) (make-instance class))
                #-ecl (get-vertex-buffer)))
    (cond (id
           (setf (id vertex) id))
          ((equalp +null-key+ (id vertex))
           (setf (id vertex) (gen-vertex-id))))
    (when type-id (setf (type-id vertex) type-id))
    (when revision (setf (revision vertex) revision))
    (when commit-epoch (setf (commit-epoch vertex) commit-epoch))
    (when prev-pointer (setf (prev-pointer vertex) prev-pointer))

    (when deleted-p (setf (deleted-p vertex) deleted-p))
    (when written-p (setf (written-p vertex) written-p))
    (when heap-written-p (setf (heap-written-p vertex) heap-written-p))
    (when type-idx-written-p (setf (type-idx-written-p vertex) type-idx-written-p))
    ;; For edges only
    ;;(when ve-written-p (setf (ve-written-p vertex) ve-written-p))
    ;;(when vev-written-p (setf (vev-written-p vertex) vev-written-p))
    (when views-written-p (setf (views-written-p vertex) views-written-p))

    (when data-pointer (setf (data-pointer vertex) data-pointer))
    (when data (setf (data vertex) data))
    (when bytes (setf (bytes vertex) bytes))
    ;; Non-ECL: promote the pooled base VERTEX to its subclass (unchanged
    ;; behaviour; no leak on these impls).  On ECL VERTEX is already CLASS.
    #-ecl (change-node-class vertex class)
    vertex))

(defun serialize-vertex-head (mf v offset)
  (serialize-node-head mf v offset))

(defun deserialize-vertex-head (mf offset)
  (multiple-value-bind
        (deleted-p written-p heap-written-p type-idx-written-p views-written-p
                   ve-written-p vev-written-p type-id revision pointer
                   commit-epoch prev-pointer)
      (funcall *node-head-reader* mf offset)
    (declare (ignore ve-written-p vev-written-p))
    (let* ((subclass (if (eq type-id 0)
                         'vertex
                         (let ((type-meta (lookup-node-type-by-id
                                           type-id :vertex)))
                           (node-type-name type-meta))))
           (v (%make-vertex :class subclass
                            :deleted-p deleted-p
                            :written-p written-p
                            :heap-written-p heap-written-p
                            :type-idx-written-p type-idx-written-p
                            :views-written-p views-written-p
                            :type-id type-id
                            :revision revision
                            :data-pointer pointer
                            :commit-epoch commit-epoch
                            :prev-pointer prev-pointer)))
      v)))

(defun make-vertex-table (location &key (key-test 'uuid-array-equal)
                                     (base-buckets (expt 2 17)))
  (let ((table
         (make-lhash :test key-test
                     :location location
                     :value-bytes +node-header-size+
                     :bucket-size 24
                     :buckets base-buckets
                     :key-serializer 'serialize-key
                     :key-deserializer 'deserialize-key
                     :value-serializer 'serialize-vertex-head
                     :value-deserializer 'deserialize-vertex-head)))
    table))

(defmethod lookup-vertex ((id string) &key (graph *graph*))
  (lookup-vertex (read-id-array-from-string id) :graph graph))

(defmethod lookup-vertex ((id array) &key (graph *graph*))
  "Return the vertex with the given ID (a 16-byte id array or its string form)
in GRAPH, or NIL if none.  Returns the vertex regardless of its deleted flag;
the generated LOOKUP-<type> functions filter deleted nodes for you."
  (lookup-object id (vertex-table graph) *transaction* graph))

(defmethod add-to-type-index ((vertex vertex) (graph graph)
                              &key unless-present)
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (add-to-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-push (id vertex) (type-id vertex) (vertex-index graph)
                   :unless-present unless-present))

(defmethod remove-from-type-index ((vertex vertex) (graph graph))
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (remove-from-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-remove (id vertex) (type-id vertex) (vertex-index graph)))

(defun make-vertex (type-id data &key id deleted-p revision retry-p (graph *graph*))
  "Create and persist a vertex of the type named/identified by TYPE-ID (a node
type name, its integer id, or :GENERIC) in GRAPH, returning it.  DATA is the
slot data stored on the node.  Must run inside a transaction.  You normally
call the generated MAKE-<type> constructor (e.g. MAKE-USER) rather than this
directly.  :ID supplies an id (one is generated otherwise); :RETRY-P regenerates
the id on a duplicate-key collision."
  (let ((type-meta (or (and (eq type-id :generic) :generic)
                       (and (eq 0 type-id) :generic)
                       (and (integerp type-id)
                            (lookup-node-type-by-id type-id :vertex :graph graph))
                       (lookup-node-type-by-name type-id :vertex :graph graph))))
    (when (stringp id)
      (setq id (read-id-array-from-string id)))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'vertex
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (v (%make-vertex :class subclass
                                :id id ;; (or id (gen-vertex-id))
                                :type-id (if (eq type-meta :generic)
                                             0
                                             (node-type-id type-meta))
                                :revision (or revision 0)
                                :deleted-p deleted-p
                                :written-p nil
                                :bytes bytes
                                :data data)))
          (setf (bytes v) bytes)
          (handler-case
              (create-node v graph)
            (duplicate-key-error (c)
              (if retry-p
                  (let ((*print-pretty* nil))
                    (log:error "VERTEX: Duplicate key error: ~A. Retrying MAKE-VERTEX" (id v))
                    (make-vertex type-id data
                                 :id (gen-vertex-id)
                                 :revision revision
                                 :deleted-p deleted-p :graph graph))
                  (error c)))))
        (error "Unknown vertex type ~A" type-id))))

(defun copy-vertex (vertex)
  (copy-node vertex))

(defmethod delete-vertex ((vertex vertex) &key (graph *graph*))
  (when (deleted-p vertex)
    (error 'vertex-already-deleted-error
           :node vertex))
  (delete-node vertex graph))

(defun map-vertices (fn graph &key collect-p vertex-type include-vertex-types
                                exclude-vertex-types include-deleted-p
                                (include-subclasses-p t))
  "Call FN on vertices of GRAPH.

Narrow the set with :VERTEX-TYPE (a single type name or numeric type-id) and/or
:INCLUDE-VERTEX-TYPES (a list of either) -- their union is visited; with no type
given, EVERY vertex is visited.  :EXCLUDE-VERTEX-TYPES (a list) removes types
from that set.  Unless :INCLUDE-SUBCLASSES-P is NIL (default T) each named type
also matches its subtypes (see RESOLVE-NODE-TYPE-IDS).  Deleted vertices are
skipped unless :INCLUDE-DELETED-P.  With :COLLECT-P, collect and return FN's
values as a list; otherwise return NIL.

NOTE: the fully-untyped scan (no :VERTEX-TYPE and no :INCLUDE-VERTEX-TYPES) walks
the raw vertex lhash, which reads LIVE node versions and so BYPASSES MVCC
snapshot isolation.  It is intended for back-end / admin passes (backup, GC,
reindex) run while the graph is quiescent; a typed scan goes through the type
index + LOOKUP-VERTEX and is snapshot-consistent.  (This is why IS-A/2 enumerates
per-type instead of using the untyped scan.)"
  ;; Bind *GRAPH* to the GRAPH argument: the lhash value-deserializer
  ;; (deserialize-vertex-head) resolves a node's type-id -> class via *GRAPH*'s
  ;; schema, so mapping a graph that isn't the current *GRAPH* would otherwise
  ;; fail (NO-APPLICABLE-METHOD on SCHEMA/VERTEX-TABLE with NIL).
  (let* ((result nil)
         (*graph* graph)
         ;; When collecting, each node ESCAPES the scan pin, so materialize its
         ;; bytes before FN sees it.  For a side-effect scan FN runs inside the
         ;; pin, so its lazy reads are already safe and we don't pre-read bytes.
         (fn (if collect-p
                 (let ((user-fn fn))
                   (lambda (node) (ensure-node-bytes node graph) (funcall user-fn node)))
                 fn)))
    (with-read-pin (graph)        ; retain whatever versions this scan observes
      (flet ((scan-type-id (type-id)
               (let ((index-list (get-type-index-list (vertex-index graph) type-id)))
                 (when index-list
                   (map-index-list
                    (lambda (id)
                      (let ((vertex (lookup-vertex id :graph graph)))
                        ;; vertex can be nil if it appears in the type-index before
                        ;; lhash-insert completes (commit race); skip it.
                        (when (and vertex
                                   (written-p vertex)
                                   (or include-deleted-p (not (deleted-p vertex))))
                          (if collect-p
                              (push (funcall fn vertex) result)
                              (funcall fn vertex)))))
                    index-list)))))
        (let ((requested (append (when vertex-type (list vertex-type))
                                 include-vertex-types)))
          (if requested
              (let ((type-ids (resolve-node-type-ids
                               requested :vertex
                               :include-subclasses-p include-subclasses-p
                               :graph graph))
                    (excluded (when exclude-vertex-types
                                (resolve-node-type-ids
                                 exclude-vertex-types :vertex
                                 :include-subclasses-p include-subclasses-p
                                 :graph graph))))
                (dolist (tid type-ids)
                  (unless (member tid excluded)
                    (scan-type-id tid))))
              ;; fully untyped: live lhash scan (see NOTE)
              (map-lhash #'(lambda (pair)
                             (let ((vertex (cdr pair)))
                               (when (and (written-p vertex)
                                          (or include-deleted-p (not (deleted-p vertex))))
                                 (setf (id vertex) (car pair))
                                 (if collect-p
                                     (push (funcall fn vertex) result)
                                     (funcall fn vertex)))))
                         (vertex-table graph))))))
    (when collect-p (nreverse result))))

(defmethod compact-vertices ((graph graph))
  (map-vertices (lambda (vertex)
                  (when (deleted-p vertex)
                    (remove-from-type-index vertex graph)))
                graph
                :include-deleted-p t))
