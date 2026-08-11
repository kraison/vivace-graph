(in-package :graph-db)

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

