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
  "The (OWNER-NAME . SLOT-NAME) keys covering CLASS's geometry-index slots.  Each
:INDEX-marked slot resolves to the most general node-class declaring it, so a slot
on a mixin yields ONE key shared by every subclass.  GRAPH is accepted for symmetry
with CLASS-SECONDARY-INDEX-DESCRIPTORS and for the DEF-SPATIAL-INDEX registry added
in a later task."
  (declare (ignorable graph))
  (when (class-finalized-p class)
    (loop for slot-name in (node-geometry-index-slots class)
          collect (cons (%indexed-slot-owner-name class slot-name) slot-name))))

(defun spatial-index-for (graph owner-name slot-name)
  "GRAPH's spatial index for (OWNER-NAME . SLOT-NAME), or NIL if none has been
CREATED yet.  NB: indexes are created lazily on first geometry-valued insert, so
NIL here does NOT mean the slot is unindexed."
  (let ((reg (spatial-indexes graph)))
    (and reg (gethash (cons owner-name slot-name) reg))))

(defun %spatial-precision-for (graph owner-name slot-name)
  "The geohash precision (OWNER-NAME . SLOT-NAME)'s index is created with.  For now
the graph default; the slot option and DEF-SPATIAL-INDEX surfaces are layered on in
a later task."
  (declare (ignorable owner-name slot-name))
  (or (graph-default-spatial-precision graph) 7))

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
memory-graph, whose in-RAM indexes have no address to record)."
  (let ((reg (spatial-indexes graph))
        (key (cons owner-name slot-name)))
    (or (gethash key reg)
        (let ((idx (make-graph-spatial-index
                    graph
                    :precision (%spatial-precision-for graph owner-name slot-name))))
          (setf (gethash key reg) idx)
          (save-spatial-index-roots graph)
          idx))))

(defun all-spatial-indexes (graph)
  "Every spatial index GRAPH currently holds."
  (let ((result '()))
    (when (spatial-indexes graph)
      (maphash (lambda (k idx) (declare (ignore k)) (push idx result))
               (spatial-indexes graph)))
    result))

(defun node-spatial-index (graph node slot-name)
  "The index NODE's SLOT-NAME geometry belongs in, created if absent."
  (%spatial-index-for graph
                      (%indexed-slot-owner-name (class-of node) slot-name)
                      slot-name))

(defun %resolve-spatial-scope (scope graph)
  "Resolve SCOPE to (values INDEXES TYPE-NAMES): the spatial indexes to scan, and
the class list results must satisfy (NIL = no filtering).

Task 2 handles only :ALL; class-name and class-list scopes arrive in Task 4."
  (ecase scope
    (:all (values (all-spatial-indexes graph) nil))))

(defun %scope-admits-p (node type-names)
  "True when NODE satisfies the scope's type filter (always, for :ALL)."
  (or (null type-names)
      (some (lambda (n) (typep node n)) type-names)))
