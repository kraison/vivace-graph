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

GRAPH is accepted for symmetry with CLASS-SECONDARY-INDEX-DESCRIPTORS, whose
callers pass one; the spatial keys are derived entirely from the class."
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
CREATED yet.  NB: an index is created lazily, on the first geometry-valued insert
that belongs in it, so NIL here does NOT mean the slot is unindexed -- and that is
true whether or not the slot declares a :SPATIAL-PRECISION.  Nothing creates an
index at open: INSTALL-SPATIAL-INDEXES only ever REBUILDS one the sidecar already
restored."
  (let ((reg (spatial-indexes graph)))
    (and reg (gethash (cons owner-name slot-name) reg))))

;;; ---------------------------------------------------------------------------
;;; Declaring a spatial index's grid precision
;;;
;;; ONE surface: the (slot :spatial-precision N) slot option.  Spatial joins
;;; :UNIQUE and :VECTOR-INDEX, which are slot options and nothing else; only
;;; :INDEX also has a macro, and DEF-INDEX earns it because secondary-index
;;; maintenance is DESCRIPTOR-driven (%IX-CLAIM walks the MOP-union-registry and
;;; reads the slot by name), so it really can index a slot not marked :INDEX.
;;; Spatial maintenance is NODE-GEOMETRY-driven, and NODE-GEOMETRY scans
;;; :INDEX-marked slots only -- a macro here could never reach an unmarked slot,
;;; so there is nothing for a second surface to say that the slot option cannot.
;;; ---------------------------------------------------------------------------

(defun %check-spatial-precision (precision owner-name slot-name)
  "Signal unless PRECISION is a geohash precision %MAKE-SPATIAL-INDEX will accept.
Checked here, at resolution, rather than left to the structure slot's (INTEGER 1 12)
type: this is the only point that still knows WHICH class and slot declared the bad
value, and the structure check is both unhelpful (a raw type error at the first
geometry-valued write) and, at low safety on ECL, possibly absent."
  (unless (typep precision '(integer 1 12))
    (error "Invalid :SPATIAL-PRECISION ~S declared on ~S.~S: expected an integer ~
            between 1 and 12."
           precision owner-name slot-name))
  precision)

(defun %declared-spatial-precision (owner-name slot-name)
  "The precision EXPLICITLY declared for (OWNER-NAME . SLOT-NAME) by the
:SPATIAL-PRECISION slot option, or NIL when the slot declares none.

Distinct from %SPATIAL-PRECISION-FOR, which falls back to the graph default, and
deliberately kept separate from it: the NIL here is what tells
INSTALL-SPATIAL-INDEXES that nothing has been DECLARED about this index, so its
persisted precision must be left alone.  Folding the default in would make
reopening a graph created with :SPATIAL-PRECISION 5 -- without repeating the
keyword -- silently re-grid every index in it at 7, contradicting OPEN-GRAPH."
  (let ((class (ignore-errors (find-class owner-name nil))))
    (when (and class (class-finalized-p class))
      (let* ((slot (find slot-name (class-slots class) :key #'slot-definition-name))
             (precision (and slot (spatial-precision-spec slot))))
        (when precision
          (%check-spatial-precision precision owner-name slot-name))))))

(defun %spatial-precision-for (graph owner-name slot-name)
  "The geohash precision (OWNER-NAME . SLOT-NAME)'s index is created with.
Precedence: the :SPATIAL-PRECISION slot option, then GRAPH's default, then 7."
  (or (%declared-spatial-precision owner-name slot-name)
      (graph-default-spatial-precision graph)
      7))

(defun %check-spatial-max-cells (max-cells owner-name slot-name)
  (unless (and (integerp max-cells) (>= max-cells 1))
    (error "Invalid :spatial-max-cells ~S declared for ~A . ~A: must be a positive integer"
           max-cells owner-name slot-name))
  max-cells)

(defun %declared-spatial-max-cells (owner-name slot-name)
  "The max-cells cap EXPLICITLY declared for (OWNER-NAME . SLOT-NAME) by the
:SPATIAL-MAX-CELLS slot option, or NIL when the slot declares none."
  (let ((class (ignore-errors (find-class owner-name nil))))
    (when (and class (class-finalized-p class))
      (let* ((slot (find slot-name (class-slots class) :key #'slot-definition-name))
             (max-cells (and slot (spatial-max-cells-spec slot))))
        (when max-cells
          (%check-spatial-max-cells max-cells owner-name slot-name))))))

(defun %spatial-max-cells-for (graph owner-name slot-name)
  "The max-cells cap (OWNER-NAME . SLOT-NAME)'s index is created with.
Precedence: the :SPATIAL-MAX-CELLS slot option, then GRAPH's default, then +spatial-insert-max-cells+ (16384)."

  (or (%declared-spatial-max-cells owner-name slot-name)
      (graph-default-spatial-max-cells graph)
      +spatial-insert-max-cells+))

(defgeneric make-graph-spatial-index (graph &key precision max-cells)
  (:documentation "Create ONE spatial index for GRAPH at PRECISION and MAX-CELLS.  A normal graph
gets a heap-backed ordered map in its INDEXES memory, following the graph's chosen
:INDEX-BACKEND; a memory-graph overrides this to return an in-RAM mem-skip-list
index.  The same seam MAKE-VIEW-SKIP-LIST provides for views -- it is what lets
%SPATIAL-INDEX-FOR be the ONE creation site for both backends.")
  (:method ((graph graph) &key (precision 7) (max-cells +spatial-insert-max-cells+))
    (make-spatial-index (indexes graph) :precision precision
                                        :max-cells max-cells
                                        ;; :SPATIAL-INDEX-BACKEND wins when set;
                                        ;; NIL (the default) follows the graph's
                                        ;; general :INDEX-BACKEND.  See GH #91 for
                                        ;; why a graph may want these to differ.
                                        :backend (or (graph-spatial-index-backend graph)
                                                     (graph-index-backend graph))))
  (:method ((graph memory-graph-mixin) &key (precision 7) (max-cells +spatial-insert-max-cells+))
    (make-mem-spatial-index :precision precision :max-cells max-cells)))

(defun %spatial-index-for (graph owner-name slot-name)
  "Get-or-create GRAPH's spatial index for (OWNER-NAME . SLOT-NAME).  This is the
ONE place an index is created, so every maintenance path and every rebuild agree on
its precision and cap.

A newly created index is NOT persisted here.  It used to be (an immediate sidecar
save so a crash before CLOSE-GRAPH still named it), but that put CL-STORE file I/O
on the hot commit path, under the transaction-manager lock, on the post-durability
side of the commit -- a convoy point and a failure-injection point after the
transaction is already durable.  Instead the sidecar is written only at CLOSE-GRAPH
(and by the rebuild/regenerate admin ops), and a crash -- which forces recovery --
has OPEN-GRAPH re-derive every spatial index from the recovered nodes after the WAL
replay.  So a crash never trusts a mid-session sidecar for spatial: it rebuilds
from authoritative node data, which also moots the old partial-sidecar hazard (the
sidecar naming fewer indexes than exist)."
  (let ((reg (spatial-indexes graph))
        (key (cons owner-name slot-name)))
    (or (gethash key reg)
        (let ((idx (make-graph-spatial-index
                    graph
                    :precision (%spatial-precision-for graph owner-name slot-name)
                    :max-cells (%spatial-max-cells-for graph owner-name slot-name))))
          (setf (gethash key reg) idx)
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

(defun %scope-type-tags (names graph)
  "The set of index-entry type tags (%SPATIAL-TYPE-TAG) NAMES admits, as an EQL
hash table -- the pre-filter that lets a scoped query reject a candidate from
the index entry alone, before %NODE-BY-ID materialises it (GH #104).

RESOLVE-NODE-TYPE-IDS expands each name to its registered CLOS subclasses, which
is exactly what makes this agree with %SCOPE-ADMITS-P's TYPEP.  Both kinds are
resolved because an index owner need only be a NODE-CLASS, so a mixin inherited
by a vertex and an edge branch can share one index.

Returns NIL -- meaning DO NOT pre-filter -- when nothing resolved, rather than
an empty set meaning \"admit nothing\".  A class can be spatially declared (a
finalized CLOS class with a geometry slot) yet unregistered in THIS graph's
schema, and an empty set would silently turn that into zero results; falling
through to TYPEP leaves such a query as correct, and as slow, as it was."
  (let ((tags (make-hash-table :test 'eql)))
    (dolist (name names)
      (dolist (id (resolve-node-type-ids name :vertex :graph graph))
        (setf (gethash (%spatial-type-tag id nil) tags) t))
      (dolist (id (resolve-node-type-ids name :edge :graph graph))
        (setf (gethash (%spatial-type-tag id t) tags) t)))
    (when (plusp (hash-table-count tags)) tags)))

(defun %resolve-spatial-scope (scope graph)
  "Resolve SCOPE -- a class name, a list of class names, or :ALL -- to
 (values INDEXES TYPE-NAMES TAGS).

INDEXES is the set of live spatial indexes to scan; TYPE-NAMES is the class list
results must satisfy, or NIL for :ALL (no filtering); TAGS is the same filter
expressed as index-entry type tags, applied inside the scan (see
%SCOPE-TYPE-TAGS), or NIL when it cannot be resolved.  A named class contributes
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
      (values (all-spatial-indexes graph) nil nil)
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
        (values indexes names (%scope-type-tags names graph)))))

(defun %scope-admits-p (node type-names)
  "True when NODE satisfies the scope's type filter (always, for :ALL)."
  (or (null type-names)
      (some (lambda (n) (typep node n)) type-names)))

;;; ---------------------------------------------------------------------------
;;; Reconciling the declarations against what is actually on disk (called at open)
;;; ---------------------------------------------------------------------------

(defun install-spatial-indexes (graph)
  "Rebuild any of GRAPH's spatial indexes whose PERSISTED precision no longer
matches the one its slot option DECLARES.  The mirror of
INSTALL-SECONDARY-INDEXES / INSTALL-VIEWS, called at open right after the
restore-or-rebuild.  It creates nothing: indexes are still created lazily, at the
first geometry-valued write, declared precision or not.

The rebuild is not optional and is not deferred to the user, unlike DEF-INDEX's
changed-canonicalizer contract: an index holding cells at two precisions
reintroduces the covering-precision miss the clamp exists to prevent, so leaving
it would be silently wrong.  It is bounded to the one owner's nodes.

Only an EXPLICIT declaration -- the :SPATIAL-PRECISION slot option -- triggers
that rebuild.  The graph default deliberately does not: it is the precision new
indexes are CREATED at, not a statement about the existing ones, and OPEN-GRAPH
documents that they reopen at their own persisted precision.  Were the default to
count, reopening a graph created with :SPATIAL-PRECISION 5 and merely forgetting
the keyword would silently re-grid every index in it."
  ;; The registry is :SYNCHRONIZED, so its lock is held for the whole MAPHASH:
  ;; collect the KEYS first and resolve the declarations outside, so nothing that
  ;; can signal (%CHECK-SPATIAL-PRECISION) or otherwise re-enter runs under it.
  ;; The regenerate below already had to be staged this way for a second reason --
  ;; REGENERATE-SPATIAL-INDEX remhashes and re-adds its key, and mutating a
  ;; hash-table under MAPHASH is undefined.
  (let ((entries '()) (stale '()))
    (maphash (lambda (key idx)
               (push (cons key (spatial-index-precision idx)) entries))
             (spatial-indexes graph))
    (dolist (e entries)
      (destructuring-bind (key . persisted) e
        (let ((declared (%declared-spatial-precision (car key) (cdr key))))
          (when (and declared (not (eql declared persisted)))
            (push (list (car key) (cdr key) persisted declared) stale)))))
    (dolist (s stale)
      (destructuring-bind (owner slot was now) s
        (log:info "Spatial index ~S.~S declares precision ~D but was written at ~
                   ~D; rebuilding that index." owner slot now was)
        (regenerate-spatial-index graph owner slot))))
  nil)
