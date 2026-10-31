(in-package :graph-db)

;; LOOKUP-VERTEX is defined in vertex.lisp, which loads after GRAPH-CLASS but
;; before this file -- these two live here (not graph-class.lisp, alongside
;; the UNRESOLVED-NODE struct and STORE-DETACHED-ERROR condition they use)
;; because they call it (GH #169).
(defun resolve-node-graph (id &key class-hint)
  "The open store holding ID, as (values GRAPH STATUS STORE-ID) with
STATUS one of :RESOLVED, :DETACHED (registry knows the tag, no open
graph carries it) or :UNKNOWN.  A v8 id indexes the open-store vector,
O(1); a v5 id is tried against each open store's vertex table -- the
no-flag-day fallback (GH #169, D5).  For a v5 id STORE-ID is the
holding store's id (or NIL when untagged/unknown).  ID may be a
32-hex-digit string, coerced like LOOKUP-VERTEX (GH #209).  A tag whose
store the registry once knew but *SYSTEM-DIRECTORY* is unbound at
query time also reports :UNKNOWN, not :DETACHED -- :DETACHED requires
a live registry to confirm the tag (GH #169, #209).  Trap: a tag is an
index into THIS system's registry only; a foreign-minted v8 id (peer
hub, cross-system restore) whose tag coincides with a local store-id
resolves to the WRONG graph yet reports :RESOLVED (GH #209)."
  (when (stringp id)
    (setq id (read-id-array-from-string id)))
  (let ((tag (id-store-tag id)))
    (if tag
        (let ((graph (svref *store-id->graph* tag)))
          (cond (graph (values graph :resolved tag))
                ((and *system-directory*
                      (store-registry-name-for tag))
                 (values nil :detached tag))
                (t (values nil :unknown tag))))
        (progn
          ;; GH #244: the edge-occupancy hint ORDERS the scan -- stores
          ;; known to hold CLASS-HINT's edges are tried first, the rest
          ;; still follow, so disproof coverage is unchanged (the hint
          ;; carries no completeness marker, so it must never RESTRICT
          ;; a scan whose miss is treated as disproof).
          (dolist (graph (%v5-scan-candidates class-hint))
            (when (and (graph-open-p graph)
                       (lookup-vertex id :graph graph))
              (return-from resolve-node-graph
                (values graph :resolved (store-id graph)))))
          (values nil :unknown nil)))))

(defun %v5-scan-candidates (class-hint)
  "Every open graph, hinted stores first (GH #244): CLASS-HINT (an edge
class name, or NIL) consults EDGE-TYPE-STORES; graphs it names come
first, every other open graph follows.  Pure ordering -- the full set is
always returned, so a stale or incomplete hint costs lookups, never an
answer."
  (let* ((hinted (when class-hint
                   (loop for name in (edge-type-stores class-hint)
                         for g = (lookup-graph name)
                         when g collect g)))
         (rest '()))
    (maphash (lambda (name graph)
               (declare (ignore name))
               (unless (member graph hinted :test #'eq)
                 (push graph rest)))
             *graphs*)
    (append hinted rest)))

(defun lookup-vertex-anywhere (id &key (if-detached :marker) class-hint)
  "ID's vertex from whichever open store holds it; an UNRESOLVED-NODE
marker when its store is detached (or, with :IF-DETACHED :ERROR, a
STORE-DETACHED-ERROR -- the explicit-access half of D8); NIL when no
store known to the system holds it (GH #169)."
  (multiple-value-bind (graph status tag)
      (resolve-node-graph id :class-hint class-hint)
    (ecase status
      (:resolved (lookup-vertex id :graph graph))
      (:detached
       (let ((name (store-registry-name-for tag)))
         (ecase if-detached
           (:marker (make-unresolved-node id tag name))
           (:error (error 'store-detached-error :name name :id tag)))))
      (:unknown nil))))

(defgeneric copy (node)
  (:documentation
   "Return a mutable copy of vertex or edge NODE, registered with the current
transaction.  This is the first step of the copy-modify-save update pattern:
copy the node inside a WITH-TRANSACTION, SETF its slots, then SAVE the copy.
Signals COPYING-UNCOMMITTED-NODE if NODE was created in this same
transaction -- it has no committed version yet, and is already writable
without a copy.")
  (:method (thing)
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((node node))
    ;; A node created in THIS transaction has no committed version to update
    ;; against; copying it built a tx-update whose OLD-NODE was a pending
    ;; create, which committed cleanly and left the graph unopenable
    ;; (GH #135).  One method for both VERTEX and EDGE so the guard can't
    ;; drift between them; %COPY (transactions.lisp) carries the same
    ;; vertex/edge dispatch this used to duplicate.  DELETE-NODE calls
    ;; %COPY directly and so bypasses this guard.
    ;; CREATE-SET is a TX-only reader; a non-TX *TRANSACTION* (e.g. replay's
    ;; RESTORE-TRANSACTION) has none, so is trusted unconditionally here
    ;; rather than signalling NO-APPLICABLE-METHOD (GH #135).
    ;; NODE-CREATED-IN-TRANSACTION-P checks by EQ, not shared id: a
    ;; re-created id (:ID is accepted by MAKE-<TYPE>) must not let an
    ;; unrelated instance skip the guard.
    (when (and *transaction*
               (typep *transaction* 'tx)
               (node-created-in-transaction-p node *transaction*))
      (error 'copying-uncommitted-node :node node))
    (%copy node)))

(defgeneric mark-deleted (node)
  (:documentation
   "Soft-delete vertex or edge NODE: set its deleted flag so it no longer
appears in queries, adjacency, or type scans (it remains on disk).  Wraps a
transaction automatically.  This is the standard way to delete a node.")
  (:method (thing)
    (error "Cannot delete ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex))
    (delete-vertex vertex))
  (:method ((edge edge))
    (delete-edge edge)))

(defgeneric save (object &key graph)
  (:documentation
   "Persist OBJECT (a vertex or edge copy) to GRAPH within the current
transaction; the final step of the copy-modify-save update pattern.  OBJECT
must have been produced by COPY in this transaction.  See UPDATE-NODE.")
  (:method (thing &key graph)
    (declare (ignore graph))
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex) &key (graph *graph*))
    (update-node vertex graph))
  (:method ((edge edge) &key (graph *graph*))
    (update-node edge graph)))

