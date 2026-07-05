(in-package :graph-db)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass node-class (standard-class) nil)

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
                 :allocation :instance)))

(defmethod persistent-p (slot-def)
  nil)

(defmethod indexed-p (slot-def)
  nil)

(defmethod unique-spec (slot-def)
  nil)

(defmethod unique-scope (slot-def)
  :local)

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

(defmethod data-slots ((instance node-class))
  "Return a list of managed slot names for an instance."
  (map 'list 'slot-definition-name
       (remove-if-not #'(lambda (i)
                          (or (persistent-p i) (ephemeral-p i)))
                      (class-slots instance))))

(defmethod meta-slot-names ((instance node-class))
  "Return a list of metadata slot names for an instance."
  ;;(log:debug "meta-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (meta-p i))

               (class-slots instance)))))
    ;;(log:debug "meta-slot-names(~A): ~A" instance names)
    names))

(defmethod persistent-slot-names ((instance node-class))
  "Return a list of persistent slot names for an instance."
  ;;(log:debug "persistent-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (persistent-p i))
                             (class-slots instance)))))
    ;;(log:debug "persistent-slot-names(~A): ~A" instance names)
    names))

(defmethod ephemeral-slot-names ((instance node-class))
  "Return a list of persistent slot names for an instance."
  ;;(log:debug "ephemeral-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (ephemeral-p i))
                             (class-slots instance)))))
    ;;(log:debug "ephemeral-slot-names(~A): ~A" instance names)
    names))

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
    (cond ((or (meta-p slot) (some 'meta-p direct-slots))
           (setf (slot-value slot 'meta) t)
           (setf (slot-value slot 'persistent) nil))
          ((or (persistent-p slot) (some 'persistent-p direct-slots))
           (setf (slot-value slot 'persistent) t))
          (t
           (setf (slot-value slot 'persistent) nil)
           (setf (slot-value slot 'ephemeral) t)))
    (when (or (indexed-p slot) (some 'indexed-p direct-slots))
      (setf (slot-value slot 'indexed) t)
      ;; FIXME: Generate index if needed
      )
    ;; Inherit the uniqueness constraint from the declaring direct slot (issue #6),
    ;; so a :UNIQUE slot on a parent enforces across its subclasses.
    (let ((u (find-if #'unique-spec direct-slots)))
      (when (or (unique-spec slot) u)
        (setf (slot-value slot 'unique) (or (unique-spec slot) (unique-spec u))
              (slot-value slot 'unique-scope) (or (and u (unique-scope u))
                                                  (unique-scope slot)))))
    slot))

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
   (bytes :accessor bytes :initform :init :initarg :bytes :meta t :persistent nil))
  (:metaclass node-class))
