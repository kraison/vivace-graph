(in-package :graph-db)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass node-class (standard-class)
  ;; Cached slot categorization; see %NODE-SLOT-INFO below.  NIL means "not
  ;; computed yet"; it is never written with anything but a complete,
  ;; freshly-built NODE-SLOT-INFO or NIL.
  ((slot-info :initform nil :accessor %node-class-slot-info)))

(defmethod validate-superclass ((class node-class) (super standard-class))
  "Node classes may inherit from ordinary classes."
  t)

(defclass node-slot-definition (standard-slot-definition)
  ((persistent :accessor persistent-p :initarg :persistent :initform t :allocation :instance)
   (indexed :accessor indexed-p :initarg :index :initform nil :allocation :instance)
   (ephemeral :accessor ephemeral-p :initarg :ephemeral :initform nil :allocation :instance)
   (meta :accessor meta-p :initarg :meta :initform nil :allocation :instance)
   ;; Uniqueness constraint (issue #6).  UNIQUE-SPEC is the :UNIQUE slot option:
   ;;   NIL (default) | T/EQUAL | EQUALP | a function designator / lambda form
   ;; (a 1-arg canonicalizer; uniqueness is EQUAL on the canonical key).  SCOPE is
   ;; the cross-peer scope: :LOCAL (default) or :ORIGIN.  See
   ;; docs/unique-constraint-design.md.
   (unique :accessor unique-spec :initarg :unique :initform nil :allocation :instance)
   (unique-scope :accessor unique-scope :initarg :scope :initform :local
                 :allocation :instance)
   (vector-index :accessor vector-index-p :initarg :vector-index :initform nil
                 :allocation :instance)
   ;; Geohash grid precision for this geometry slot's spatial index, or NIL for
   ;; the graph default.  A type-as-hint option: it means nothing on a slot that
   ;; never holds a geometry.  It is the ONLY precision-declaration surface (there
   ;; is deliberately no per-index macro); see %SPATIAL-PRECISION-FOR /
   ;; %DECLARED-SPATIAL-PRECISION in spatial-registry.lisp for how it resolves.
   (spatial-precision :accessor spatial-precision-spec :initarg :spatial-precision
                      :initform nil :allocation :instance)
   ;; Max cells cap for this geometry slot's spatial index, or NIL for the graph
   ;; default.  Declared via the :SPATIAL-MAX-CELLS slot option.
   (spatial-max-cells :accessor spatial-max-cells-spec :initarg :spatial-max-cells
                      :initform nil :allocation :instance)))

(defmethod persistent-p (slot-def)
  nil)

(defmethod indexed-p (slot-def)
  nil)

(defmethod unique-spec (slot-def)
  nil)

(defmethod unique-scope (slot-def)
  :local)

(defmethod vector-index-p (slot-def)
  nil)

(defmethod spatial-precision-spec (slot-def)
  nil)

(defmethod spatial-max-cells-spec (slot-def)
  nil)

(defmethod ephemeral-p (slot-def)
  nil)

(defmethod meta-p (slot-def)
  nil)

(defclass node-direct-slot-definition
    (standard-direct-slot-definition node-slot-definition)
  ())

(defclass node-effective-slot-definition
    (standard-effective-slot-definition node-slot-definition)
  ())

;;; ---------------------------------------------------------------------------
;;; Per-class slot categorization, computed once (GH #87)
;;;
;;; PERSISTENT-P / EPHEMERAL-P / META-P are generic functions, and the four
;;; SLOT-*-USING-CLASS :AROUND methods in primitive-node.lisp consult them on
;;; EVERY slot access.  Walking CLASS-SLOTS and consing a fresh name list per
;;; access cost ~28 list rebuilds and ~532 predicate dispatches per node
;;; materialized -- about 59% of one profiler workload -- to recompute answers
;;; that cannot change once the class is finalized.
;;;
;;; So compute the breakdown once and cache it on the class.  The correctness
;;; obligation is invalidation: see %INVALIDATE-NODE-SLOT-INFO below for the two
;;; places a class definition can change out from under the cache.
;;; ---------------------------------------------------------------------------

(defstruct (node-slot-info (:conc-name %nsi-))
  ;; slot-name -> the keyword naming its entry in a node's DATA alist.  Holds
  ;; ONLY persistent slots, so GETHASH answers "is this persistent?" and "what
  ;; keyword?" in one lookup -- which is all the hot path needs, because every
  ;; non-persistent branch of those :AROUND methods just calls the next method.
  (persistent-keywords (make-hash-table :test 'eq) :type hash-table)
  (persistent-names nil :type list)
  (ephemeral-names nil :type list)
  (meta-names nil :type list)
  (data-names nil :type list))

(defun %compute-node-slot-info (class)
  "Build CLASS's NODE-SLOT-INFO.  Each list is filtered independently, exactly
as the separate walks it replaces did, so a slot carrying more than one flag
still appears in each list it qualifies for."
  (let ((keywords (make-hash-table :test 'eq))
        (persistent '()) (ephemeral '()) (meta '()) (data '()))
    (dolist (slot (class-slots class))
      (let ((name (slot-definition-name slot))
            (persistentp (persistent-p slot))
            (ephemeralp (ephemeral-p slot)))
        (when persistentp
          (push name persistent)
          (setf (gethash name keywords) (intern (symbol-name name) :keyword)))
        (when ephemeralp (push name ephemeral))
        (when (meta-p slot) (push name meta))
        (when (or persistentp ephemeralp) (push name data))))
    (make-node-slot-info :persistent-keywords keywords
                         :persistent-names (nreverse persistent)
                         :ephemeral-names (nreverse ephemeral)
                         :meta-names (nreverse meta)
                         :data-names (nreverse data))))

(defun %node-slot-info (class)
  "CLASS's cached slot categorization, computing it on first use.
Deliberately unlocked: two threads racing here build equal structures and one
SETF wins, which is harmless.  What must never happen is publishing a partially
filled structure, so the SETF stores an already-complete one."
  (or (%node-class-slot-info class)
      (setf (%node-class-slot-info class) (%compute-node-slot-info class))))

(defvar *node-class-cache-invalidators* '()
  "Functions of one NODE-CLASS, run whenever its effective slots may have changed.

VG supports RUNTIME SCHEMA MUTATION -- DEF-VERTEX / DEF-EDGE can be evaluated
against a live image to add or redefine a type -- so anything that memoizes a
CLASS-SLOTS-derived answer must be dropped when that happens.  Register the
dropper here rather than adding another FINALIZE-INHERITANCE :AFTER method:
a second :AFTER with the same specializer would REPLACE this file's, silently
disabling the invalidation it was meant to add.

Each function is called for the changed class AND for every node-class subclass
of it -- the walk is done by %INVALIDATE-NODE-CLASS-CACHES -- so an invalidator
only has to handle the one class it is given.")

(defun %invalidate-node-class-caches (class)
  "Drop every memoized view of CLASS's effective slots, and its subclasses'.

The subclass walk is the part that is easy to miss: a subclass's effective slots
are recomputed when a SUPERCLASS is redefined, so invalidating only the class
that was redefined leaves subclasses serving stale answers."
  (setf (%node-class-slot-info class) nil)
  (dolist (fn *node-class-cache-invalidators*)
    (funcall fn class))
  (dolist (sub (class-direct-subclasses class))
    (when (typep sub 'node-class)
      (%invalidate-node-class-caches sub))))

(defmethod finalize-inheritance :after ((class node-class))
  "Effective slots have just been recomputed, so any cached view of them is stale."
  (%invalidate-node-class-caches class))

(defmethod reinitialize-instance :after ((class node-class) &rest initargs
                                         &key &allow-other-keys)
  "Class redefinition (a re-evaluated DEF-VERTEX / DEF-EDGE / DEFCLASS)."
  (declare (ignore initargs))
  (%invalidate-node-class-caches class))

(defun %persistent-slot-keyword (class slot-name)
  "The keyword naming SLOT-NAME's entry in a node's DATA alist, or NIL when
SLOT-NAME is not a persistent slot of CLASS.  This is the whole question the
SLOT-*-USING-CLASS :AROUND methods need to ask."
  (values (gethash slot-name (%nsi-persistent-keywords (%node-slot-info class)))))

;;; The four readers below return the CACHED lists, not fresh copies as they did
;;; before #87.  Treat them as read-only: SORT / NREVERSE / DELETE on a returned
;;; list corrupts the class's cache.  COPY-LIST first if you need to mutate.

(defmethod data-slots ((instance node-class))
  "Return a list of managed slot names for an instance."
  (%nsi-data-names (%node-slot-info instance)))

(defmethod meta-slot-names ((instance node-class))
  "Return a list of metadata slot names for an instance."
  (%nsi-meta-names (%node-slot-info instance)))

(defmethod persistent-slot-names ((instance node-class))
  "Return a list of persistent slot names for an instance."
  (%nsi-persistent-names (%node-slot-info instance)))

(defmethod ephemeral-slot-names ((instance node-class))
  "Return a list of ephemeral slot names for an instance.
Note that this is empty for every node class as things stand -- :EPHEMERAL never
reaches the effective slot; see the EPHEMERAL-DECLARATION-IS-CURRENTLY-INERT
test in tests/node-class-tests.lisp."
  (%nsi-ephemeral-names (%node-slot-info instance)))

(defmethod direct-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (log:trace "direct-slot-definition-class for ~A" class)
  (find-class 'node-direct-slot-definition))

(defmethod effective-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (log:trace "effective-slot-definition-class for ~A" class)
  (find-class 'node-effective-slot-definition))

(defmethod compute-effective-slot-definition :around
    ((class node-class) slot-name direct-slots)
  "Ensure inheritance from direct slot definition of persistent, indexed,
   and ephemeral properties."
  (log:trace "compute-effective-slot-definition for ~A / ~A: ~A" class slot-name direct-slots)
  (let ((slot (call-next-method)))
    ;;(log:debug "  SLOT: ~A" slot)
    ;; Test :EPHEMERAL against the DIRECT slots only.  CALL-NEXT-METHOD returns a
    ;; freshly built effective slot and the standard method does not carry custom
    ;; slot-definition slots across, so the effective slot always arrives with
    ;; EPHEMERAL NIL and PERSISTENT T (their initforms).  The old middle clause
    ;; therefore always matched and :EPHEMERAL never took effect (GH #90).
    (cond ((or (meta-p slot) (some 'meta-p direct-slots))
           (setf (slot-value slot 'meta) t)
           (setf (slot-value slot 'persistent) nil))
          ((some 'ephemeral-p direct-slots)
           (setf (slot-value slot 'ephemeral) t)
           (setf (slot-value slot 'persistent) nil))
          (t
           (setf (slot-value slot 'persistent) t)))
    ;; Inherit the :INDEX spec (T or a canonicalizer) from the declaring direct slot,
    ;; so an :INDEX slot on a parent indexes across its subclasses (general index).
    (let ((i (find-if #'indexed-p direct-slots)))
      (when (or (indexed-p slot) i)
        (setf (slot-value slot 'indexed) (or (indexed-p slot) (indexed-p i)))))
    ;; Inherit the uniqueness constraint from the declaring direct slot (issue #6),
    ;; so a :UNIQUE slot on a parent enforces across its subclasses.
    (let ((u (find-if #'unique-spec direct-slots)))
      (when (or (unique-spec slot) u)
        (setf (slot-value slot 'unique) (or (unique-spec slot) (unique-spec u))
              (slot-value slot 'unique-scope) (or (and u (unique-scope u))
                                                  (unique-scope slot)))))
    ;; Inherit the :VECTOR-INDEX flag from the declaring direct slot, so a
    ;; :VECTOR-INDEX slot on a parent is indexed across its subclasses (like
    ;; :INDEX / :UNIQUE above).
    (let ((vi (find-if #'vector-index-p direct-slots)))
      (when (or (vector-index-p slot) vi)
        (setf (slot-value slot 'vector-index) (or (vector-index-p slot)
                                                  (and vi (vector-index-p vi))))))
    ;; Inherit :SPATIAL-PRECISION from the declaring direct slot, so a geometry
    ;; slot on a parent carries one grid precision across its subclasses -- the
    ;; same shared-index semantics as :INDEX / :UNIQUE / :VECTOR-INDEX above.
    (let ((sp (find-if #'spatial-precision-spec direct-slots)))
      (when (or (spatial-precision-spec slot) sp)
        (setf (slot-value slot 'spatial-precision)
              (or (spatial-precision-spec slot)
                  (and sp (spatial-precision-spec sp))))))
    ;; Inherit :SPATIAL-MAX-CELLS from the declaring direct slot.
    (let ((smc (find-if #'spatial-max-cells-spec direct-slots)))
      (when (or (spatial-max-cells-spec slot) smc)
        (setf (slot-value slot 'spatial-max-cells)
              (or (spatial-max-cells-spec slot)
                  (and smc (spatial-max-cells-spec smc))))))
    slot))

(defun %indexed-slot-owner-name (class slot-name)
  "The most-general node-class in CLASS's precedence list that declares SLOT-NAME as
an :INDEX direct slot -- the cross-subtype index owner (an :INDEX slot on a parent is
one shared index across its subclasses).  Lives here rather than in index.lisp so both
the general ordered index and the spatial index can reach it: index.lisp loads after
transactions.lisp, which needs this on the spatial maintenance path."
  (let ((owner (loop for c in (reverse (class-precedence-list class))
                     when (and (typep c 'node-class)
                               (find-if (lambda (ds)
                                          (and (eq (slot-definition-name ds) slot-name)
                                               (indexed-p ds)))
                                        (class-direct-slots c)))
                     return c)))
    (class-name (or owner class))))

(defmethod find-all-subclasses ((class class))
  ;;(log:debug "Finding subclasses for ~A" class)
  (let ((result nil))
    (labels ((find-them (class)
               (let ((subclasses (class-direct-subclasses class)))
                 ;;(log:debug "Found subclasses for ~A: ~A" class subclasses)
                 (dolist (subclass subclasses)
                   (unless (find subclass result)
                     (push subclass result)
                     (find-them subclass))))))
      (find-them class)
      result)))

(defmethod find-all-subclass-names ((class class))
  (mapcar 'class-name (find-all-subclasses class)))

(defun resolve-node-type-ids (designator kind &key (include-subclasses-p t)
                                                (graph *graph*))
  "Resolve a node-type DESIGNATOR -- a type name (symbol), a numeric type-id, or
a LIST of either -- into a deduplicated list of integer type-ids of KIND
\(:VERTEX or :EDGE) registered in GRAPH.

When INCLUDE-SUBCLASSES-P (the default), each named type is expanded to itself
PLUS every CLOS subclass of it that is registered as a type of KIND.  This
expansion is necessary because a node is indexed only under its OWN type-id (the
type/ve/vev indexes are keyed by exact type-id), so a parent-type query must scan
each subtype's index explicitly -- this is the same compensation MAP-VERTICES has
always performed, here factored out so MAP-EDGES can share it.

Designators that resolve to no registered type of KIND are skipped (so the 0
sentinel and cross-graph subclasses simply drop out).  Order of first appearance
is preserved."
  (let ((seen (make-hash-table))
        (ids nil))
    (labels ((add-id (id)
               (when (and id (not (gethash id seen)))
                 (setf (gethash id seen) t)
                 (push id ids)))
             (add-one (d)
               (let ((meta (if (integerp d)
                               (lookup-node-type-by-id d kind :graph graph)
                               (lookup-node-type-by-name d kind :graph graph))))
                 (when meta
                   (add-id (node-type-id meta))
                   (when include-subclasses-p
                     (let ((class (find-class (node-type-name meta) nil)))
                       (when class
                         (dolist (sub (find-all-subclass-names class))
                           (let ((sub-meta (lookup-node-type-by-name sub kind
                                                                     :graph graph)))
                             (when sub-meta
                               (add-id (node-type-id sub-meta))))))))))))
      (if (listp designator)
          (dolist (d designator) (add-one d))
          (add-one designator)))
    (nreverse ids)))

(defmethod find-ancestor-classes ((class-name symbol))
  (find-ancestor-classes (find-class class-name)))

(defmethod find-ancestor-classes ((class node-class))
  ;; remove-if (non-destructive): on CCL the list returned by
  ;; compute-class-precedence-list shares structure with the class's stored
  ;; CPL slot, so a destructive delete-if mutates the class's own CPL --
  ;; breaking method dispatch on any superclass for multi-level subclasses.
  (remove-if (lambda (class)
               (find (class-name class)
                     #+sbcl '(edge vertex node STANDARD-OBJECT SB-PCL::SLOT-OBJECT T)
                     #+lispworks '(edge vertex node standard-object T)
                     #+ccl '(edge vertex node STANDARD-OBJECT T)
                     #+ecl '(edge vertex node standard-object T)))
             (compute-class-precedence-list class)))

(defmethod find-graph-parent-classes ((class node-class))
  (let ((classes
         (remove-if (lambda (class)
                      (or (eq (class-name class) 'vertex)
                          (eq (class-name class) 'edge)
                          (eq (class-name class) 'primitive-node)))
                    (class-direct-superclasses class))))
    (remove-duplicates
     (nconc classes
            (mapcan 'find-graph-parent-classes classes)))))
)

(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id :meta t
       :type (simple-array (unsigned-byte 8) (16)) :persistent nil)
   (type-id :accessor type-id :initform 1 :initarg :type-id :meta t
            :type (unsigned-byte 16) :persistent nil)
   (revision :accessor revision :initform 0 :initarg :revision :meta t
             :type (unsigned-byte 32) :persistent nil)
   (%revision-table :accessor %revision-table :initform (make-hash-table :test 'eq)
                    :initarg :revision-table :meta t :persistent nil)
   (heap-written-p :accessor heap-written-p :initform nil :initarg :heap-written-p
                   :type boolean :meta t :persistent nil)
   (type-idx-written-p :accessor type-idx-written-p :initform nil :meta t
                       :initarg :type-idx-written-p :type boolean :persistent nil)
   (ve-written-p :accessor ve-written-p :initform nil :initarg :ve-written-p
                 :type boolean :meta t :persistent nil)
   (vev-written-p :accessor vev-written-p :initform nil :initarg :vev-written-p
                  :type boolean :meta t :persistent nil)
   (views-written-p :accessor views-written-p :initform nil :meta t
                    :initarg :views-written-p :type boolean :persistent nil)
   (written-p :accessor written-p :initform nil :initarg :written-p :type boolean
              :meta t :persistent nil)
   (data-pointer :accessor data-pointer :initform 0 :initarg :data-pointer
                 :type (unsigned-byte 64) :meta t :persistent nil)
   ;; MVCC (v2 head): commit-epoch = the committing transaction-id when this
   ;; version was written (global monotonic; for snapshot reads + the reaper).
   ;; prev-pointer = LOCAL heap address of the previous version's archived head
   ;; (0 = none).  Both are serialized in the node head; see serialize-node-head.
   (commit-epoch :accessor commit-epoch :initform 0 :initarg :commit-epoch
                 :type (unsigned-byte 64) :meta t :persistent nil)
   (prev-pointer :accessor prev-pointer :initform 0 :initarg :prev-pointer
                 :type (unsigned-byte 64) :meta t :persistent nil)
   (deleted-p :accessor deleted-p :initform nil :initarg :deleted-p :type boolean
              :meta t :persistent nil)
   (data :accessor data :initarg :data :initform nil :meta t :persistent nil)
   (bytes :accessor bytes :initform :init :initarg :bytes :meta t :persistent nil)
   ;; Home graph; NIL = unknown -> caller falls back to *GRAPH* (GH #53)
   (graph :accessor node-graph :initform nil :initarg :graph
          :meta t :persistent nil))
  (:metaclass node-class))

;; A node's GRAPH is a live object, not data: cl-store does not honour
;; :PERSISTENT NIL, so storing it would pull the whole graph into the image
;; and restore a phantom graph (GH #53).
(defmethod cl-store:serializable-slots ((object node))
  (remove 'graph (call-next-method) :key #'slot-definition-name))

(defun node-home-graph (node &optional (default *graph*))
  "NODE's graph, or DEFAULT when unknown. Use instead of a bare *GRAPH* when
resolving a node's heap, tables or schema (GH #53)."
  (if (slot-boundp node 'graph)
      (or (node-graph node) default)
      default))
