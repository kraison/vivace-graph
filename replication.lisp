(in-package :graph-db)

;;; ===========================================================================
;;; Peer replication (WP-5): authority-scoped closed-subgraph export + manifest
;;; reconciliation.  Pure graph logic, no network -- the peer transport
;;; (peer-streaming.lisp) ships what these produce.  See
;;; docs/peer-replication-design.md §6/§7 and docs/peer-replication-branch-a-plan.md.
;;;
;;; A device's working set is the CLOSED subgraph reachable from its root(s),
;;; bounded by edge type, with two fail-closed rules:
;;;   - keep a vertex iff DISCLOSABLE-P (the app-supplied export predicate);
;;;   - keep an edge iff BOTH endpoints are disclosable (no dangling refs, and no
;;;     leaking the existence/id of undisclosed work).
;;; DISCLOSABLE-P may do bounded graph reads (e.g. walk to a tasking project), so
;;; the whole walk runs under one read snapshot for consistency.  Disclosure
;;; policy lives entirely in the app's predicate; the engine stays domain-agnostic.
;;; ===========================================================================

(defgeneric disclosable-p (graph vertex device-scope)
  (:documentation "True if VERTEX may be disclosed to a device with DEVICE-SCOPE.
Dispatches to GRAPH's app-supplied EXPORT-PREDICATE (a function of
VERTEX, GRAPH, DEVICE-SCOPE); with no predicate everything is disclosable (a full,
unfiltered replica -- useful for tests).  May do bounded graph reads and is called
under the export read snapshot.  Kept a first-class entry point so the app can
reuse the same disclosure test outside replication (e.g. a CoT/web projection).")
  (:method ((graph peer-graph) vertex device-scope)
    (let ((pred (export-predicate graph)))
      (if pred (and (funcall pred vertex graph device-scope) t) t)))
  (:method ((graph graph) vertex device-scope)
    (declare (ignore vertex device-scope))
    t))

(defun %resolve-vertex (v graph)
  "Coerce V (a vertex or a 16-byte id) to a vertex in GRAPH."
  (if (typep v 'vertex) v (lookup-vertex v :graph graph)))

(defun scope-node-set (graph roots device-scope &key edge-types)
  "Compute the closed authority-scoped subgraph of GRAPH reachable from ROOTS (a
list of vertices or ids), following only EDGE-TYPES (NIL = all types), under one
read snapshot.  Returns (values vertex-table edge-table), each an id-table mapping
node-id -> node, holding exactly the disclosable closure."
  (let ((vset (make-id-table))
        (eset (make-id-table))
        (dcache (make-id-table))      ; id -> :yes / :no  (memoize disclosable-p)
        (refs (and (typep graph 'peer-graph) (reference-classes graph)))
        (queue '()))
    (labels ((disc (v)
               (let ((cached (gethash (id v) dcache)))
                 (case cached
                   (:yes t)
                   (:no nil)
                   (t (let ((d (disclosable-p graph v device-scope)))
                        (setf (gethash (id v) dcache) (if d :yes :no))
                        d)))))
             (reference-vertex-p (v)
               "True if V belongs to a REFERENCE-CLASS (subclass-inclusive)."
               (some (lambda (class) (typep v class)) refs))
             (visit (v)
               (when (and v (not (gethash (id v) vset)) (disc v))
                 (setf (gethash (id v) vset) v)
                 ;; Reference-class vertices are TERMINAL in the walk: ship them, but never
                 ;; traverse OUT of them -- no matter how they were reached.  The reference-set
                 ;; pass below adds them without enqueueing for exactly this reason; making it a
                 ;; property of WHAT the vertex IS (rather than of HOW it was added) closes the
                 ;; other door.  Otherwise an edge INTO the catalogue -- e.g. FIND-OF-TYPE, which
                 ;; a device needs so a pulled find carries its ordnance classification -- fans the
                 ;; walk back out along that vertex's INCOMING edges to every find in the database
                 ;; that shares the type, paying a DISCLOSABLE-P (and its survey/project traversal)
                 ;; on each one before rejecting it.  Correct, but O(all finds) per sync.
                 (unless (reference-vertex-p v)
                   (push v queue)))))
      (with-read-snapshot (graph)
        (dolist (r roots) (visit (%resolve-vertex r graph)))
        (loop while queue do
          (let ((v (pop queue)))
            (dolist (e (append
                        (outgoing-edges v :graph graph :include-edge-types edge-types)
                        (incoming-edges v :graph graph :include-edge-types edge-types)))
              ;; the other endpoint is whichever of from/to is not V
              (let ((other (lookup-vertex (if (uuid-array-equal (from e) (id v))
                                              (to e)
                                              (from e))
                                          :graph graph)))
                (when (and other (disc other))
                  ;; both endpoints disclosable -> keep the edge (closed rule)
                  (setf (gethash (id e) eset) e)
                  (visit other))))))
        ;; Reference-set (hub role): ship global reference data by CLASS -- every
        ;; disclosable vertex of a REFERENCE-CLASS (subclass-inclusive), independent of
        ;; the roots walk.  Added to VSET but NOT enqueued, so its own edges are never
        ;; followed -> no walk-back into out-of-scope neighbours.  E.g. the ordnance
        ;; catalogue: the device gets every ordnance-type so it can classify new finds,
        ;; while the hub-only ordnance-detail/country neighbours stay behind.
        (dolist (class (and (typep graph 'peer-graph) (reference-classes graph)))
          (map-vertices (lambda (v)
                          (when (and (not (gethash (id v) vset)) (disc v))
                            (setf (gethash (id v) vset) v)))
                        graph :vertex-type class :include-subclasses-p t))))
    (values vset eset)))

(defun reconcile-manifest (graph roots device-scope manifest &key edge-types)
  "Diff the current closed subgraph against MANIFEST (a sequence of node-ids the
device is known to already hold).  Returns (values creates purges): CREATES are
node objects newly in scope (ship as membership-creates), PURGES are ids in
MANIFEST no longer in scope (ship as purges).  Per PT-4 the caller advances the
stored manifest only on the device's ack, so a dropped purge re-emits."
  (multiple-value-bind (vset eset)
      (scope-node-set graph roots device-scope :edge-types edge-types)
    (let ((scope (make-id-table)) (mset (make-id-table))
          (creates '()) (purges '()))
      (flet ((collect (id node) (setf (gethash id scope) node)))
        (maphash #'collect vset)
        (maphash #'collect eset))
      (map nil (lambda (id) (setf (gethash id mset) t)) manifest)
      (maphash (lambda (id node) (unless (gethash id mset) (push node creates))) scope)
      (map nil (lambda (id) (unless (gethash id scope) (push id purges))) manifest)
      (values creates purges))))

(defun filtered-backup (graph roots device-scope stream
                        &key edge-types include-deleted-p)
  "Write the closed authority-scoped subgraph to STREAM in the BACKUP plist format
(a device seeds it via RECREATE-GRAPH).  Returns the number of nodes written.
Tombstoned nodes are skipped unless INCLUDE-DELETED-P."
  (multiple-value-bind (vset eset)
      (scope-node-set graph roots device-scope :edge-types edge-types)
    (let ((count 0))
      (flet ((dump (id node)
               (declare (ignore id))
               (when (or include-deleted-p (not (deleted-p node)))
                 (maybe-init-node-data node :graph graph)
                 (backup node stream :include-deleted-p include-deleted-p)
                 (incf count))))
        (maphash #'dump vset)
        (maphash #'dump eset))
      count)))
