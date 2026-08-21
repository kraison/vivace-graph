(in-package :graph-db)

(defstruct schema
  (lock (make-recursive-lock))
  (type-table
   #+sbcl (make-hash-table :test 'eql :synchronized t)
   #+ccl (make-hash-table :test 'eql :shared t)
   #+lispworks (make-hash-table :test 'eql :single-thread nil)
   #+ecl (make-hash-table :test 'eql
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))
  (class-locks
   #+sbcl (make-hash-table :test 'eql :synchronized t)
   #+ccl (make-hash-table :test 'eql :shared t)
   #+lispworks (make-hash-table :test 'eql :single-thread nil)
   #+ecl (make-hash-table :test 'eql
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))
  (next-edge-id 1 :type (unsigned-byte 32))
  (next-vertex-id 1 :type (unsigned-byte 32))
  ;; MVCC: graph-wide default number of prior node versions the reaper retains
  ;; regardless of epoch safety (0 = keep none beyond what active readers need).
  ;; Appended last so cl-store can still restore pre-MVCC schema.dat.
  (keep-revisions 0 :type (unsigned-byte 32)))

;; ECL's DEFSTRUCT defines a SETF *expander* for each accessor but no callable
;; (SETF SCHEMA-LOCK) function.  OPEN-GRAPH (graph.lisp) is compiled before
;; this struct and emits a call to the function form (setf (schema-lock ...)),
;; so on ECL we must provide that function.  The inner SETF here expands via
;; the defstruct setf-expander (a direct slot store), so there is no recursion.
#+ecl
(defun (setf schema-lock) (new-value schema)
  (setf (schema-lock schema) new-value))

;; Same ECL workaround for SCHEMA-KEEP-REVISIONS: MAKE-GRAPH / OPEN-GRAPH
;; (graph.lisp, compiled before this struct) emit (setf (schema-keep-revisions ...))
;; as a function call, which ECL's defstruct does not provide.
#+ecl
(defun (setf schema-keep-revisions) (new-value schema)
  (setf (schema-keep-revisions schema) new-value))

(defstruct node-type
  name
  parent-type
  id
  graph-name
  slots
  package
  constructor
  ;; MVCC: per-type override for how many prior versions the reaper retains.
  ;; NIL = inherit the graph-level schema default.  Appended last for cl-store
  ;; restore compatibility with pre-MVCC schema.dat.
  (keep-revisions nil))

(defgeneric instantiate-node-type (node-type-def graph))

(defmacro with-write-locked-class ((name graph) &body body)
  `(let ((rw-lock (gethash ,name (schema-class-locks (schema ,graph)))))
     (with-write-lock (rw-lock)
       ,@body)))

(defmacro with-read-locked-class ((name graph) &body body)
  `(let ((rw-lock (gethash ,name (schema-class-locks (schema ,graph)))))
     (with-read-lock (rw-lock)
       ,@body)))

(defun list-edge-types (&optional (graph *graph*))
  (nconc (list 0)
         (loop
            for key being the hash-keys
            in (gethash :edge (schema-type-table (schema graph)))
            if (numberp key)
            collecting key)))

(defun list-vertex-types (&optional (graph *graph*))
  (nconc (list 0)
         (loop
            for key being the hash-keys
            in (gethash :vertex (schema-type-table (schema graph)))
            if (numberp key)
            collecting key)))

(defmethod init-schema ((graph graph))
  (let ((schema (make-schema)))
    (setf (schema graph) schema)
    (setf (gethash :edge (schema-type-table (schema graph)))
          #+sbcl (make-hash-table :test 'eql :synchronized t)
          #+ccl (make-hash-table :test 'eql :shared t)
          #+lispworks (make-hash-table :test 'eql :single-thread nil)
          #+ecl (make-hash-table :test 'eql
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))
    (setf (gethash :vertex (schema-type-table (schema graph)))
          #+sbcl (make-hash-table :test 'eql :synchronized t)
          #+ccl (make-hash-table :test 'eql :shared t)
          #+lispworks (make-hash-table :test 'eql :single-thread nil)
          #+ecl (make-hash-table :test 'eql
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))
    (setf (gethash 'edge (schema-class-locks schema))
          (make-rw-lock))
    (setf (gethash 'vertex (schema-class-locks schema))
          (make-rw-lock))
    (slot-value graph 'schema)))

(defmethod save-schema ((schema schema) (graph graph))
  (with-recursive-lock-held ((schema-lock schema))
    (let ((schema-file (format nil "~A/schema.dat" (location graph))))
      (let ((locks (schema-class-locks schema))
            (schema-lock (schema-lock schema)))
        (setf (schema-class-locks schema) nil)
        (setf (schema-lock schema) nil)
        (cl-store:store (schema graph) schema-file)
        (setf (schema-lock schema) schema-lock)
        (setf (schema-class-locks schema) locks)
        schema))))

(defmethod restore-schema-locks ((schema schema))
  "Recreate the runtime per-class rw-locks for a schema just restored from disk.
Locks are never persisted (SAVE-SCHEMA nils CLASS-LOCKS before cl-store, since
the lock objects are runtime-only), so a restored schema's CLASS-LOCKS slot is
nil.  Rebuild it with the base VERTEX / EDGE locks plus one rw-lock per
already-registered node type, keyed by the type NAME -- the same key
WITH-WRITE-LOCKED-CLASS / INSTANTIATE-NODE-TYPE use.  This preserves the
persisted type-ids (unlike re-running INIT-SCHEMA from scratch)."
  (let ((locks #+sbcl (make-hash-table :test 'eql :synchronized t)
               #+ccl (make-hash-table :test 'eql :shared t)
               #+lispworks (make-hash-table :test 'eql :single-thread nil)
               #+ecl (make-hash-table :test 'eql
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t)))
    (setf (gethash 'vertex locks) (make-rw-lock)
          (gethash 'edge locks) (make-rw-lock))
    ;; The type-table nests sub-tables (by parent type) whose VALUES include the
    ;; node-type metas; make a lock for each registered type by name.
    (maphash (lambda (parent sub)
               (declare (ignore parent))
               (when (hash-table-p sub)
                 (maphash (lambda (k v)
                            (declare (ignore k))
                            (when (node-type-p v)
                              (setf (gethash (node-type-name v) locks)
                                    (make-rw-lock))))
                          sub)))
             (schema-type-table schema))
    (setf (schema-class-locks schema) locks)
    schema))

(defun %normalize-parent-type (parent)
  (cond ((or (eql parent :edge) (eql parent 'edge)) :edge)
        ((or (eql parent :vertex) (eql parent 'vertex)) :vertex)
        (t (error "Unknown parent type ~S" parent))))

(defun assign-type-id (name parent)
  "The type-id for NAME under PARENT, minted if this system has not seen it
before.  Ids come from the image-level registry, not the per-graph counters
this replaced, so one symbol names one id in every store of the system and no
two symbols share one (GH #186).  Keyed on the package-qualified symbol.

REGISTRY-INTERN, never REGISTRY-ID-FOR: the latter is lock-free, so its NIL
is a hint, not proof of absence, and only INTERN re-reads under the lock.
Signals SYSTEM-DIRECTORY-REQUIRED when *SYSTEM-DIRECTORY* is NIL."
  (registry-intern (ensure-type-registry) name
                   (%normalize-parent-type parent)))

(defmethod schema-string-representation ((schema schema))
  "Return a string representation of SCHEMA. Two schemas with the same
node structure will have EQUALP string representations. This is meant
for a quick test of replication compatibility, not guaranteed equality
testing."
  (with-output-to-string (stream)
    (loop with parent-alist = (alexandria:hash-table-alist
                               (schema-type-table schema))
          for (parent . table) in (sort parent-alist #'string<
                                        :key 'car)
          do
          (format stream "~A~%" parent)
          (loop with node-types = (remove-if-not #'node-type-p
                                                 (alexandria:hash-table-values table))
                for node-type in (sort node-types #'string<
                                       :key 'node-type-name)
                do
                (format stream "  ~A~%" (node-type-name node-type))
                (loop with slots = (mapcar 'first (node-type-slots node-type))
                      for slot in (sort slots #'string<)
                      do (format stream "   ~A~%" slot))))))

(defmethod schema-digest ((schema schema))
  "Return a digest of the string representation of SCHEMA. Used in
replication for a quick schema compatibility check."
  (with-output-to-string (stream)
    (map nil
         (lambda (octet)
           (format stream "~(~2,'0X~)" octet))
         ;; :utf-8 (canonical) not :utf8 -- CCL's external-format normalizer
         ;; rejects :utf8.  Same octets on every impl, so the digest is stable.
         (md5:md5sum-string (schema-string-representation schema)
                             :external-format :utf-8))))

(defmethod all-node-types ((graph graph))
  (let ((types nil))
    (maphash (lambda (parent table)
               (push (intern (symbol-name parent)) types)
               (maphash (lambda (child ctable)
                          (declare (ignore ctable))
                          (when (and (not (keywordp child))
                                     (not (numberp child)))
                            (push child types)))
                        table))
             (schema-type-table (schema graph)))
    types))

(defun lookup-node-type-by-id (id parent &key (graph *graph*))
  (assert (and (integerp id) (>= id 0) (< id +max-node-types+)))
  (let ((meta (gethash id (gethash parent (schema-type-table (schema graph))))))
    meta))

(defun lookup-node-type-by-name (name parent &key (graph *graph*))
  (let ((id (gethash name (gethash parent (schema-type-table (schema graph))))))
    (when id
      (lookup-node-type-by-id id parent :graph graph))))

(defmethod update-node-type ((meta node-type) (graph graph))
  (setf (gethash (node-type-id meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        meta)
  (setf (gethash (node-type-name meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        (node-type-id meta))
  (setf (gethash (intern (symbol-name (node-type-name meta)) :keyword)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        (node-type-id meta))
  (finalize-inheritance (find-class (node-type-name meta)))
  (save-schema (schema graph) graph))

(defmacro def-node-type (name parent-types slot-specs graph-name &key keep-revisions)
  "Define a persistent node type NAME for the graph named GRAPH-NAME.  This is
the machinery behind DEF-VERTEX and DEF-EDGE; you normally use those instead.

PARENT-TYPES is a single-inheritance superclass list ending in VERTEX or EDGE.
SLOT-SPECS are CLOS-style slot definitions (a bare symbol, or (name :type ...)
etc.); an :accessor and :initarg are supplied automatically when omitted.

Expands to a (defclass ... (:metaclass node-class)) plus generated helpers:
MAKE-<NAME> (constructor), LOOKUP-<NAME> (id -> node, skipping deleted unless
:include-deleted-p), and <NAME>-P (predicate).  For edges it also defines the
Prolog functors <NAME>/2 and <NAME>/3.  The type metadata is registered under
GRAPH-NAME and instantiated into the graph if it already exists, so a type may
be defined before or after the graph is created."
  (with-gensyms (meta graph metas pos)
    (let* ((constructor (intern (format nil "MAKE-~A" name)))
           (predicate (intern (format nil "~A-P" name)))
           (lookup-fn (intern (format nil "LOOKUP-~A" name))))
      (setq slot-specs
            (mapcar (lambda (spec)
                      (let ((s1
                             (if (listp spec)
                                 (if (find :accessor spec)
                                     spec
                                     (append spec (list :accessor (first spec))))
                                 (list spec :accessor spec))))
                        (if (find :initarg s1)
                            s1
                            (append s1 (list :initarg (intern (symbol-name (first s1)) :keyword))))))
                    slot-specs))
      `(progn
         ;; No cross-graph uniqueness check: type-ids are system-wide as of
         ;; #186, so one class may be instantiated in more than one store.
         (defclass ,name (,@parent-types)
           (,@slot-specs)
           (:metaclass node-class))
         (let* ((,meta
                 (make-node-type
                  :name ',name
                  :parent-type
                  ',(intern (symbol-name (last1 parent-types)) :keyword)
                  :graph-name ',graph-name
                  :slots ',slot-specs
                  :package (package-name *package*)
                  :constructor ',constructor
                  :keep-revisions ,keep-revisions)))
           ;; FIXME: why is this necessary when inheriting from another node subclass?
           ;;(unless (class-finalized-p (find-class ',name))
           (finalize-inheritance (find-class ',name))
           ;;)
           (defun ,predicate (thing)
             (typep thing ',name))
           (defun ,lookup-fn (id &key include-deleted-p)
             (let ((thing ,(if (eql (last1 parent-types) 'edge)
                               `(lookup-edge id)
                               `(lookup-vertex id))))
               (when (and (typep thing ',name)
                          (or include-deleted-p
                              (not (deleted-p thing))))
                 thing)))
           ,(let ((args (if (eql (last1 parent-types) 'edge)
                            '(&rest make-args
                              &key (graph *graph*) id deleted-p revision from to weight &allow-other-keys)
                            '(&rest make-args
                              &key (graph *graph*) id deleted-p revision &allow-other-keys))))
                 `(defun ,constructor ,args
                    (let ((slots (remove-if
                                  'null
                                  (mapcar
                                   (lambda (slot-name)
                                     (let ((key (intern (symbol-name slot-name) :keyword)))
                                       (let ((pos (position key make-args)))
                                         (when pos
                                           (cons key (nth (1+ pos) make-args))))))
                                   (data-slots (find-class ',name))))))
                      ,(if (eql (last1 parent-types) 'edge)
                           `(make-edge (node-type-id
                                        (lookup-node-type-by-name ',name :edge
                                                                  :graph graph))
                                       from to weight
                                       slots ;(list ,@slots)
                                       :id id :revision revision :deleted-p deleted-p
                                       :graph graph)
                           `(make-vertex (node-type-id
                                          (lookup-node-type-by-name ',name :vertex
                                                                    :graph graph))
                                         slots ;(list ,@slots)
                                         :id id :revision revision :deleted-p deleted-p
                                         :graph graph)))))
           ,(when (eql (last1 parent-types) 'edge)
                  (let ((functor-name (intern (format nil "~A/2" name))))
                    `(def-global-prolog-functor ,functor-name (from to cont)
                       (setq from (var-deref from)
                             to (var-deref to))
                       (when *prolog-trace*
                         (format t "TRACE: ~A(~S ~S)~%" ',functor-name from to))
                       (cond ((and (not (graph-db::var-p from)) (not (graph-db::var-p to)))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (funcall cont)))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :from-vertex from
                                         :to-vertex to
                                         :edge-type ',name))
                             ((not (graph-db::var-p from))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (to edge))))
                                               (when (unify to v2)
                                                 (funcall cont)))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex from
                                         :direction :out
                                         :edge-type ',name))
                             ((not (graph-db::var-p to))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (from edge))))
                                               (when (unify from v2)
                                                 (funcall cont)))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex to
                                         :direction :in
                                         :edge-type ',name))
                             (t
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (funcall cont)))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :edge-type ',name))))))
           ,(when (eql (last1 parent-types) 'edge)
                  (let ((functor-name (intern (format nil "~A/3" name))))
                    `(def-global-prolog-functor ,functor-name (from to weight cont)
                       (setq from (var-deref from)
                             to (var-deref to)
                             weight (var-deref weight))
                       (when *prolog-trace*
                         (format t "TRACE: ~A(~S ~S ~S)~%" ',functor-name from to weight))
                       (cond ((and (not (graph-db::var-p from)) (not (graph-db::var-p to)))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (when (unify weight (weight edge))
                                                       (funcall cont))))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :from-vertex from
                                         :to-vertex to
                                         :edge-type ',name))
                             ((not (graph-db::var-p from))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (to edge))))
                                               (when (unify to v2)
                                                 (when (unify weight (weight edge))
                                                   (funcall cont))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex from
                                         :direction :out
                                         :edge-type ',name))
                             ((not (graph-db::var-p to))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (from edge))))
                                               (when (unify from v2)
                                                 (when (unify weight (weight edge))
                                                   (funcall cont))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex to
                                         :direction :in
                                         :edge-type ',name))
                             (t
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (when (unify weight (weight edge))
                                                       (funcall cont))))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :edge-type ',name)))))
                  )
           ;; Replace in place, preserving position.  The type-id reason is
           ;; historical: ids came from this list's order until #186 moved
           ;; assignment to the registry, which keys on the name.  Position
           ;; still governs the order UPDATE-SCHEMA instantiates types in, so
           ;; a moved entry reorders schema replay (GH #53).
           (let* ((,metas (gethash ',graph-name *schema-node-metadata*))
                  (,pos (position ',name ,metas :key #'node-type-name)))
             (if ,pos
                 (setf (nth ,pos ,metas) ,meta)
                 (setf (gethash ',graph-name *schema-node-metadata*)
                       (append ,metas (list ,meta)))))
           (let ((,graph (lookup-graph ',graph-name)))
             (when ,graph
               (instantiate-node-type ,meta ,graph)))
           )))))

(defmacro def-vertex (name parent-types slot-specs graph-name &key keep-revisions)
  "Define a vertex (node) type NAME for the graph named GRAPH-NAME.

PARENT-TYPES is a list of other vertex types to inherit from (often empty);
VERTEX is appended automatically.  SLOT-SPECS are CLOS-style typed slots.
Generates MAKE-NAME / LOOKUP-NAME / NAME-P and slot accessors.  Example:
  (def-vertex user () ((username :type string)) :social-app)
:KEEP-REVISIONS N overrides, for this type, how many prior MVCC versions the
reaper retains (NIL = inherit the graph default).
See DEF-NODE-TYPE for full details and DEF-EDGE for relationships."
  `(def-node-type ,name (,@parent-types vertex) ,slot-specs ,graph-name
     :keep-revisions ,keep-revisions))

(defmacro def-edge (name parent-types slot-specs graph-name &key keep-revisions)
  "Define an edge (relationship) type NAME for the graph named GRAPH-NAME.

Like DEF-VERTEX but the type inherits from EDGE, so its constructor also takes
:FROM, :TO and :WEIGHT.  Generates MAKE-NAME / LOOKUP-NAME / NAME-P, slot
accessors, and the Prolog query functors NAME/2 and NAME/3.  :KEEP-REVISIONS N
overrides this type's retained-version count (NIL = inherit the graph default).
Example:
  (def-edge follows () () :social-app)
  ... (make-follows :from alice :to bob)"
  `(def-node-type ,name (,@parent-types edge) ,slot-specs ,graph-name
     :keep-revisions ,keep-revisions))

(defmethod node-type-diff ((meta1 node-type) (meta2 node-type))
  (let ((new-slots (set-difference (node-type-slots meta2)
                                   (node-type-slots meta1)
                                   :test 'equalp))
        (removed-slots (set-difference (node-type-slots meta1)
                                       (node-type-slots meta2)
                                       :test 'equalp)))
    (values (or new-slots removed-slots
                ;; A changed :keep-revisions also counts as a redefinition.
                (not (eql (node-type-keep-revisions meta1)
                          (node-type-keep-revisions meta2))))
            new-slots removed-slots)))

(defmethod instantiate-node-type ((meta node-type) (graph graph))
  (with-recursive-lock-held ((schema-lock (schema graph)))
    (let ((cl (find-class (node-type-name meta) nil)))
      ;; Drop EVERY memoized CLASS-SLOTS-derived answer for this class and its
      ;; subclasses, not just the geometry one for this class: re-instantiating a
      ;; node type is a schema mutation, and a subclass's effective slots change
      ;; with its superclass.  (FINALIZE-INHERITANCE :AFTER normally covers this;
      ;; this keeps the guarantee on any path that reaches here without it.)
      (when (typep cl 'node-class) (%invalidate-node-class-caches cl)))
    ;; Check if this type exists and if it differs from old spec
    (log:debug "Looking up ~A: ~A ~A" meta (node-type-name meta) (node-type-parent-type meta))
    ;; :GRAPH GRAPH, not the ambient *GRAPH*: this branch decides whether the
    ;; type is new TO THIS STORE, and since #186 that decides whether it takes
    ;; the store's existing id or asks the registry for one.  *GRAPH* is bound
    ;; to GRAPH on the open/create paths but is NIL (or another store) when a
    ;; DEF-VERTEX is evaluated at runtime against an already-open graph.
    (let ((old-meta (lookup-node-type-by-name (node-type-name meta)
                                              (node-type-parent-type meta)
                                              :graph graph)))
      (if (node-type-p old-meta)
          (multiple-value-bind (changes-p new-slots removed-slots)
              (node-type-diff old-meta meta)
            (setf (node-type-id meta) (node-type-id old-meta))
            (if changes-p
                (progn
                  ;; FIXME: what to do with slot changes-p
                  (log:debug "REMOVED SLOTS FOR ~S:~% ~S"
                             (node-type-name meta) removed-slots)
                  (log:debug "NEW SLOTS FOR ~S:~% ~S"
                             (node-type-name meta) new-slots)
                  (update-node-type meta graph))
                old-meta))
          ;; Else new TO THIS STORE: the id comes from the system-wide
          ;; registry, keyed on the type's NAME (GH #186).  Assigned here and
          ;; not in UPDATE-NODE-TYPE because only this branch knows the store
          ;; has no id of its own yet -- the redefinition branch above must
          ;; keep the one already written into every node of that type.
          ;; Unconditional, not guarded on (NULL (NODE-TYPE-ID META)):
          ;; *SCHEMA-NODE-METADATA* holds one META per graph-name, and it
          ;; outlives the store, so a second, fresh store opened under that
          ;; name would find the first store's id still on it and adopt it
          ;; instead of asking the registry.
          (progn
            (setf (gethash (node-type-name meta)
                           (schema-class-locks (schema graph)))
                  (make-rw-lock))
            (setf (node-type-id meta)
                  (assign-type-id (node-type-name meta)
                                  (node-type-parent-type meta)))
            (update-node-type meta graph))))))


(defmethod update-schema ((graph-name symbol))
  (let ((graph (lookup-graph graph-name)))
    (if graph
        (update-schema graph)
        (error "Cannot update schema for graph ~A: graph not open!" graph-name))))

(defmethod update-schema ((graph graph))
  ;; *SCHEMA-UPDATE-SUPPRESSED* is bound by MIGRATE-GRAPH alone, which
  ;; installs by hand the schema each of its two opens is to have: minting
  ;; registry ids here for a schema discarded on the next form is how the
  ;; registry ends up holding ids no store uses (GH #186).
  (unless *schema-update-suppressed*
    (with-recursive-lock-held ((schema-lock (schema graph)))
      (let ((node-metadata (gethash (graph-name graph)
                                    *schema-node-metadata*)))
        ;; The list is maintained oldest-first (GH #53); apply in order.
        (dolist (meta node-metadata)
          (instantiate-node-type meta graph)))
      (save-schema (schema graph) graph))))
