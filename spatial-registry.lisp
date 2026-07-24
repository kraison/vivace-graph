(in-package :graph-db)

;;;; The graph's per-(owner-class . slot) spatial index registry.
;;;;
;;;; The spatial index is the fourth member of the (owner . slot) index family --
;;;; alongside UNIQUE-INDEXES, VECTOR-SEGMENTS and SECONDARY-INDEXES -- and follows
;;;; the same shape: one index per DECLARING class per geometry slot, keyed by
;;;; (owner-name . slot-name), created lazily, persisted as a sidecar of roots.
;;;;
;;;; Loaded late so it can see both the MOP helpers and the graph; TRANSACTIONS.LISP,
;;;; GRAPH.LISP and SPATIAL-QUERY.LISP reach it through DECLAIM FTYPE forward
;;;; declarations, exactly as graph.lisp already does for the unique and secondary
;;;; index functions.

(defun class-spatial-index-keys (class graph)
  "The (OWNER-NAME . SLOT-NAME) keys covering CLASS's geometry.  Each :INDEX-marked
slot resolves to the most general node-class declaring it, so a slot on a mixin
yields ONE key shared by every subclass.  A class with an application-supplied
NODE-GEOMETRY method instead yields (METHOD-OWNER . NIL), resolved by the same
most-general rule -- the same key %NODE-SPATIAL-OWNER-NAME hands the write path, so
a scope on such a class finds exactly what the write path wrote.

GRAPH is accepted for symmetry with CLASS-SECONDARY-INDEX-DESCRIPTORS and for the
DEF-SPATIAL-INDEX registry added in a later task."
  (declare (ignorable graph))
  (when (class-finalized-p class)
    (let ((slot-keys (loop for slot-name in (node-geometry-index-slots class)
                           collect (cons (%indexed-slot-owner-name class slot-name)
                                         slot-name)))
          (method-owner (%node-geometry-method-owner-name class)))
      (if method-owner
          (cons (cons method-owner nil) slot-keys)
          slot-keys))))

(defun spatial-index-for (graph owner-name slot-name)
  "GRAPH's spatial index for (OWNER-NAME . SLOT-NAME), or NIL if none has been
CREATED yet.  NB: indexes are created lazily on first geometry-valued insert, so
NIL here does NOT mean the slot is unindexed."
  (let ((reg (spatial-indexes graph)))
    (and reg (gethash (cons owner-name slot-name) reg))))

;;; ---------------------------------------------------------------------------
;;; DEF-SPATIAL-INDEX: the out-of-band declaration surface (mirrors DEF-INDEX)
;;; ---------------------------------------------------------------------------

(defvar *schema-spatial-metadata* (make-hash-table)
  "graph-name (symbol) -> list of SPATIAL-INDEX-SPECs (newest first).")

(defstruct (spatial-index-spec (:constructor make-spatial-index-spec))
  owner-name slot-name graph-name precision)

(defvar *spatial-precision-conflicts-warned* nil
  "NIL, or a hash-table INSTALL-SPATIAL-INDEXES binds for the duration of one
install.  While bound, %WARN-ON-PRECISION-CONFLICT reports each conflicting
 (owner . slot) at most ONCE, so an open that both CREATES a declared index and
then re-checks it for staleness -- two independent resolutions of the same
precision -- does not report the same conflict twice.")

(defun %registered-spatial-specs (graph)
  "DEF-SPATIAL-INDEX specs for GRAPH, de-duped by (owner . slot), newest-wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-spatial-metadata*))
      (let ((k (cons (spatial-index-spec-owner-name spec)
                     (spatial-index-spec-slot-name spec))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %slot-option-precision (owner-name slot-name)
  "The :SPATIAL-PRECISION declared on OWNER-NAME's SLOT-NAME, or NIL."
  (let ((class (ignore-errors (find-class owner-name nil))))
    (when (and class (class-finalized-p class))
      (let ((slot (find slot-name (class-slots class) :key #'slot-definition-name)))
        (and slot (spatial-precision-spec slot))))))

(defun %def-spatial-precision (graph owner-name slot-name)
  "The precision a DEF-SPATIAL-INDEX declares for (OWNER-NAME . SLOT-NAME), or NIL."
  (let ((spec (find-if (lambda (s)
                         (and (eq (spatial-index-spec-owner-name s) owner-name)
                              (eq (spatial-index-spec-slot-name s) slot-name)))
                       (%registered-spatial-specs graph))))
    (and spec (spatial-index-spec-precision spec))))

(defun %precision-conflict-unreported-p (owner-name slot-name)
  "True when (OWNER-NAME . SLOT-NAME)'s precision conflict has not been reported
yet in the current INSTALL-SPATIAL-INDEXES pass -- always true outside one, where
each resolution stands on its own.  Marks it reported as a side effect."
  (let ((seen *spatial-precision-conflicts-warned*))
    (or (null seen)
        (let ((key (cons owner-name slot-name)))
          (unless (gethash key seen)
            (setf (gethash key seen) t))))))

(defun %warn-on-precision-conflict (graph owner-name slot-name macro-precision)
  "Warn when a DEF-SPATIAL-INDEX precision is overridden by a slot option.  The
slot option wins -- MOP-first, matching CLASS-SECONDARY-INDEX-DESCRIPTORS -- and
this warning is what keeps the losing declaration from being a silent no-op.
Returns the slot option's precision (NIL when it declares none), i.e. the winner
of the two, so callers resolve and warn in one pass."
  (let ((slot-precision (%slot-option-precision owner-name slot-name)))
    (when (and slot-precision macro-precision
               (/= slot-precision macro-precision)
               (%precision-conflict-unreported-p owner-name slot-name))
      (warn "Spatial precision conflict on ~S.~S in ~S: the :SPATIAL-PRECISION ~
             slot option (~D) wins over DEF-SPATIAL-INDEX (~D).  Declare it in ~
             one place: the slot option for what the schema states, ~
             DEF-SPATIAL-INDEX for what it does not."
            owner-name slot-name (graph-name graph)
            slot-precision macro-precision))
    slot-precision))

(defmacro def-spatial-index (owner-class slot graph-name &key precision)
  "Declare a spatial index on OWNER-CLASS.SLOT in GRAPH-NAME (spanning
OWNER-CLASS's subclasses), optionally at a specific geohash :PRECISION.
Declarative and idempotent like DEF-INDEX and DEF-VIEW.

Use the (slot :spatial-precision N) slot option for what the schema declares and
this macro for what it does not: this can also index a slot NOT marked :INDEX,
and needs no change to an already-persisted class definition.  When both declare
a precision the SLOT OPTION wins and a warning is signalled -- do not declare it
twice.  To adopt a changed precision, the index is rebuilt automatically at the
next open."
  `(let ((spec (make-spatial-index-spec
                :owner-name ',owner-class :slot-name ',slot
                :graph-name ',graph-name :precision ,precision)))
     (push spec (gethash ',graph-name *schema-spatial-metadata*))
     (let ((g (lookup-graph ',graph-name)))
       (when g (%spatial-index-for g ',owner-class ',slot)))
     spec))

(defun %declared-spatial-precision (graph owner-name slot-name)
  "The precision EXPLICITLY declared for (OWNER-NAME . SLOT-NAME), or NIL when
neither declaration surface names one.  Precedence: slot option > DEF-SPATIAL-INDEX.
MOP-first, matching CLASS-SECONDARY-INDEX-DESCRIPTORS, so one rule covers both
halves of :INDEX; the conflict warning is what makes the losing declaration audible.

Distinct from %SPATIAL-PRECISION-FOR, which falls back to the graph default: the
NIL here is what tells INSTALL-SPATIAL-INDEXES that nothing has been DECLARED about
this index, so its persisted precision must be left alone."
  (let ((macro-precision (%def-spatial-precision graph owner-name slot-name)))
    (or (%warn-on-precision-conflict graph owner-name slot-name macro-precision)
        macro-precision)))

(defun %spatial-precision-for (graph owner-name slot-name)
  "The geohash precision (OWNER-NAME . SLOT-NAME)'s index is created with.
Precedence: slot option > DEF-SPATIAL-INDEX > the graph default."
  (or (%declared-spatial-precision graph owner-name slot-name)
      (graph-default-spatial-precision graph)
      7))

(defgeneric make-graph-spatial-index (graph &key precision)
  (:documentation "Create ONE spatial index for GRAPH at PRECISION.  A normal graph
gets a heap-backed ordered map in its INDEXES memory, following the graph's chosen
:INDEX-BACKEND; a memory-graph overrides this to return an in-RAM mem-skip-list
index.  The same seam MAKE-VIEW-SKIP-LIST provides for views -- it is what lets
%SPATIAL-INDEX-FOR be the ONE creation site for both backends.")
  (:method ((graph graph) &key (precision 7))
    (make-spatial-index (indexes graph) :precision precision
                                        :backend (graph-index-backend graph)))
  (:method ((graph memory-graph-mixin) &key (precision 7))
    (make-mem-spatial-index :precision precision)))

(defun %spatial-index-for (graph owner-name slot-name)
  "Get-or-create GRAPH's spatial index for (OWNER-NAME . SLOT-NAME).  This is the
ONE place an index is created, so every maintenance path and every rebuild agree on
its precision and cap.

A newly created index is persisted to the sidecar IMMEDIATELY, not just at close:
its root address is stable for the life of the index, so recording it at creation
costs one small write and means a crash before CLOSE-GRAPH still reopens the index
by address rather than orphaning it (SAVE-SPATIAL-INDEX-ROOTS is a no-op on a
memory-graph, whose in-RAM indexes have no address to record).

Exception: while *SPATIAL-REBUILD-IN-PROGRESS* is bound (REBUILD-SPATIAL-INDEXES /
REGENERATE-SPATIAL-INDEX), this per-creation save is a no-op -- the caller is
already bracketing the whole multi-index operation with its own :COMPLETE NIL /
:COMPLETE T saves, and a save here would both be redundant (K extra sidecar writes
for K indexes recreated) and wrong: an intermediate :COMPLETE T write partway
through the rebuild would defeat the very bracket that makes a crash safe to
re-derive from."
  (let ((reg (spatial-indexes graph))
        (key (cons owner-name slot-name)))
    (or (gethash key reg)
        (let ((idx (make-graph-spatial-index
                    graph
                    :precision (%spatial-precision-for graph owner-name slot-name))))
          (setf (gethash key reg) idx)
          (unless *spatial-rebuild-in-progress*
            (save-spatial-index-roots graph))
          idx))))

(defun all-spatial-indexes (graph)
  "Every spatial index GRAPH currently holds."
  (let ((result '()))
    (when (spatial-indexes graph)
      (maphash (lambda (k idx) (declare (ignore k)) (push idx result))
               (spatial-indexes graph)))
    result))

(defun node-spatial-index (graph node slot-name)
  "The index NODE's SLOT-NAME geometry belongs in, created if absent.  Resolves the
owner through %NODE-SPATIAL-OWNER-NAME, so a NIL SLOT-NAME (a custom NODE-GEOMETRY
method) lands in the method owner's index, not a per-subclass one."
  (%spatial-index-for graph
                      (%node-spatial-owner-name (class-of node) slot-name)
                      slot-name))

(defun %class-geometry-slots-declared-p (class-name)
  "True when CLASS-NAME is a scopeable spatial class: it declares at least one
:INDEX-marked slot, OR it carries an application-supplied NODE-GEOMETRY method.

Both are first-class ways to be spatially indexed, so both must be scopeable --
otherwise overriding NODE-GEOMETRY would leave a class reachable only through
:ALL, which is the unscoped query this API exists to forbid.  Distinguishes a
declared-but-empty index (a legitimate empty result) from a class that is not
spatially indexed at all (an error).  Direct mirror of %SLOT-INDEX-DECLARED-P."
  (let ((class (ignore-errors (find-class class-name nil))))
    (and class (class-finalized-p class)
         (or (node-geometry-index-slots class)
             (%node-geometry-method-owner-name class))
         t)))

(defun %resolve-spatial-scope (scope graph)
  "Resolve SCOPE -- a class name, a list of class names, or :ALL -- to
 (values INDEXES TYPE-NAMES).

INDEXES is the set of live spatial indexes to scan; TYPE-NAMES is the class list
results must satisfy, or NIL for :ALL (no filtering).  A named class contributes
every (owner . slot) index covering its geometry, so a slot declared on a mixin
resolves to the ancestor's shared index -- and the type filter is what then keeps
a sibling subclass's nodes out of the answer.  Indexes are deduped by KEY, not by
struct identity, so two classes sharing an ancestor's index scan it once.

An index that does not exist yet contributes nothing: indexes are created lazily on
the first geometry-valued write, so a declared-but-empty index is a legitimate
empty result, not a fault.

Signals when a named class is not spatially indexed at all: that is a programming
error, and catching it is the reason the scope is required."
  (if (eq scope :all)
      (values (all-spatial-indexes graph) nil)
      (let* ((names (if (listp scope) scope (list scope)))
             (keys (make-hash-table :test 'equalp))
             (indexes '()))
        (dolist (name names)
          (unless (%class-geometry-slots-declared-p name)
            (error "~S is not a spatially indexed class in ~S: it declares no ~
                    :INDEX-marked geometry slot and has no NODE-GEOMETRY method."
                   name (graph-name graph)))
          (dolist (key (class-spatial-index-keys (find-class name) graph))
            (unless (gethash key keys)
              (setf (gethash key keys) t)
              (let ((idx (spatial-index-for graph (car key) (cdr key))))
                (when idx (push idx indexes))))))
        (values indexes names))))

(defun %scope-admits-p (node type-names)
  "True when NODE satisfies the scope's type filter (always, for :ALL)."
  (or (null type-names)
      (some (lambda (n) (typep node n)) type-names)))

;;; ---------------------------------------------------------------------------
;;; Reconciling the declarations against what is actually on disk (called at open)
;;; ---------------------------------------------------------------------------

(defun install-spatial-indexes (graph)
  "Build any DEF-SPATIAL-INDEX registered for GRAPH that is missing from its
registry, and rebuild any index whose PERSISTED precision no longer matches an
EXPLICITLY DECLARED one.  The mirror of INSTALL-SECONDARY-INDEXES / INSTALL-VIEWS,
called at open right after the restore-or-rebuild.

The rebuild is not optional and is not deferred to the user, unlike DEF-INDEX's
changed-canonicalizer contract: an index holding cells at two precisions
reintroduces the covering-precision miss the clamp exists to prevent, so leaving
it would be silently wrong.  It is bounded to the one owner's nodes.

Only an EXPLICIT declaration -- the :SPATIAL-PRECISION slot option or
DEF-SPATIAL-INDEX -- triggers that rebuild.  The graph default deliberately does
not: it is the precision new indexes are CREATED at, not a statement about the
existing ones, and OPEN-GRAPH documents that they reopen at their own persisted
precision.  Were the default to count, reopening a graph created with
:SPATIAL-PRECISION 5 and merely forgetting the keyword would silently re-grid
every index in it."
  (let ((*spatial-precision-conflicts-warned* (make-hash-table :test 'equal)))
    ;; 1. Declared but never created -- a DEF-SPATIAL-INDEX evaluated before this
    ;;    graph existed, or added since the last close.  %SPATIAL-INDEX-FOR is the
    ;;    one creation site, so it picks up the declared precision itself.
    (dolist (spec (%registered-spatial-specs graph))
      (let ((owner (spatial-index-spec-owner-name spec))
            (slot (spatial-index-spec-slot-name spec)))
        (unless (spatial-index-for graph owner slot)
          (%spatial-index-for graph owner slot))))
    ;; 2. Created earlier at a precision the declarations have since changed.  The
    ;;    stale set is collected FIRST and regenerated after: REGENERATE-SPATIAL-
    ;;    INDEX remhashes and re-adds its key, and mutating a hash-table under
    ;;    MAPHASH is undefined.
    (let ((stale '()))
      (maphash (lambda (key idx)
                 (let ((declared (%declared-spatial-precision graph (car key)
                                                              (cdr key))))
                   (when (and declared
                              (not (eql declared (spatial-index-precision idx))))
                     (push (list (car key) (cdr key)
                                 (spatial-index-precision idx) declared)
                           stale))))
               (spatial-indexes graph))
      (dolist (s stale)
        (destructuring-bind (owner slot was now) s
          (log:info "Spatial index ~S.~S declared precision ~D but was written at ~
                     ~D; rebuilding that index." owner slot now was)
          (regenerate-spatial-index graph owner slot)))))
  nil)
