(in-package :graph-db)

;;; ===========================================================================
;;; Peer replication Branch B: the per-field conflict resolver (B2a).
;;;
;;; When a replica applies an AUTHORED update to a node it also holds, the field
;;; values may have diverged (the ETL laptop, the hub, and mobile peers all edit
;;; the same nodes).  This resolves that divergence field-by-field, dispatching on
;;; an app-declared BUCKET.  Domain semantics (which field is which bucket, what a
;;; SAFE value is) live in the app's conflict policy (mine-action's
;;; conflict-policy.lisp); the engine supplies the mechanism -- the MERGE-POLICY
;;; seam, mirroring the DISCLOSABLE-P/EXPORT-PREDICATE seam.
;;;
;;; This file is the PURE core (no storage, no network): given the local node's
;;; data, the incoming op's new/old data, per-field Lamport stamps, and the
;;; policy, it produces the merged data + the stamps to persist + the conflicts to
;;; surface.  B2b/c/d wire in the stamp store, the policy slot, and the push
;;; transport that calls this.
;;;
;;; Buckets (design §11, as the app collapsed them in bucketing v1):
;;;   :create         new node by UUID -- never reaches the field resolver
;;;   :lww            (default) per-field last-writer-wins by (Lamport, origin)
;;;   :safety         BINARY safety class -- the app's SAFETY-MERGE decides the
;;;                   winner + whether to SURFACE (toward-danger auto-applies;
;;;                   only a release surfaces, keeping the dangerous local value)
;;;   :geometry       concurrent polygon/point edits SURFACE (never silent LWW)
;;;   :safety-surface edge-only in the app; no field case (folded to :geometry here)
;;; ===========================================================================

(defstruct (merge-policy (:constructor %make-merge-policy))
  "The app-supplied conflict contract the Branch B resolver dispatches through.
FIELD-BUCKET-FN is (NODE-TYPE SLOT) -> bucket keyword (default :lww).
SAFETY-MERGE-FN is (SLOT LOCAL INCOMING LOCAL-NEWER-P) -> (values WINNER SURFACE-P),
required once any field is bucketed :safety.
EDGE-BUCKET-FN is (EDGE-TYPE) -> bucket keyword (default :structural), consulted on an
authored EDGE DELETE at re-home; :safety-edge means a still-live safety-bearing edge's
removal SURFACES + is KEPT (B3-2)."
  (field-bucket-fn (lambda (type slot) (declare (ignore type slot)) :lww)
                   :type function)
  (safety-merge-fn nil)
  (edge-bucket-fn (lambda (edge-type) (declare (ignore edge-type)) :structural)
                  :type function))

(defun make-merge-policy (&key field-bucket safety-merge edge-bucket)
  "Build a MERGE-POLICY from the app's FIELD-BUCKET, SAFETY-MERGE and EDGE-BUCKET fns."
  (%make-merge-policy
   :field-bucket-fn (or field-bucket
                        (lambda (type slot) (declare (ignore type slot)) :lww))
   :safety-merge-fn safety-merge
   :edge-bucket-fn (or edge-bucket
                       (lambda (edge-type) (declare (ignore edge-type)) :structural))))

(defun merge-policy-edge-bucket (policy edge-type)
  "The bucket the app assigns EDGE-TYPE (default :structural)."
  (funcall (merge-policy-edge-bucket-fn policy) edge-type))

(defstruct (peer-conflict (:constructor make-peer-conflict))
  "A surfaced field conflict retained for the app's review surface: on SLOT
(bucket BUCKET) of NODE-ID the merge KEPT KEPT-VALUE (authored by KEPT-ORIGIN) and
set aside LOSER-VALUE (authored by LOSER-ORIGIN at LOSER-LAMPORT).  OP-ID is the
incoming op that surfaced it (set by the apply/re-home caller; the idempotency key
with NODE-ID + SLOT).  RESOLVED-P is the app review-workflow flag (B3)."
  node-id slot bucket kept-value kept-origin loser-value loser-origin loser-lamport
  op-id (resolved-p nil))

(defun %alist-val (slot alist) (cdr (assoc slot alist)))

(defun peer-stamp-newer-p (a-lamport a-origin b-lamport b-origin)
  "Lexicographic (LAMPORT, ORIGIN): T iff stamp A is strictly later than B.  Origin
breaks a Lamport tie so every replica picks the same LWW winner (convergence).  A
NIL origin sorts lowest (an unstamped local field always loses a real op)."
  (cond ((> a-lamport b-lamport) t)
        ((< a-lamport b-lamport) nil)
        ((and a-origin b-origin)
         (let ((c (peer-compare-ids a-origin b-origin))) (> c 0)))
        (t (and a-origin (not b-origin)))))       ; stamped beats unstamped on a tie

(defun peer-compare-ids (a b)
  "Lexicographic byte compare of two 16-byte ids: -1 / 0 / 1."
  (dotimes (i 16 0)
    (let ((x (aref a i)) (y (aref b i)))
      (cond ((< x y) (return -1)) ((> x y) (return 1))))))

;;; ---------------------------------------------------------------------------
;;; B2b: the per-field (lamport . origin) stamp store.
;;;
;;; Behind this GET/SET API so the substrate can change.  v1 (C) is an in-memory
;;; map node-id -> alist (slot . (lamport . origin)), persisted on open/close;
;;; final target (B) is a heap-backed side store.  A crash between persists loses
;;; recent stamps -> local fields read as unstamped -> the next op wins the LWW
;;; (safe: the loser is retained via MVCC; :safety resolution barely uses stamps).
;;; ---------------------------------------------------------------------------

(defun ensure-field-stamps (graph)
  (or (field-stamps graph)
      (setf (field-stamps graph) (make-hash-table :test 'equalp))))

(defun node-field-stamps (graph node-id)
  "The alist slot -> (lamport . origin) of NODE-ID's stamps (a fresh copy; NIL if none)."
  (and (field-stamps graph)
       (with-recursive-lock-held ((field-stamps-lock graph))
         (copy-alist (gethash node-id (field-stamps graph))))))

(defun node-field-stamp (graph node-id slot)
  "(values LAMPORT ORIGIN) for SLOT of NODE-ID; (0 NIL) if unstamped."
  (let ((s (cdr (assoc slot (node-field-stamps graph node-id)))))
    (if s (values (car s) (cdr s)) (values 0 nil))))

(defun set-node-field-stamp (graph node-id slot lamport origin)
  "Record that SLOT of NODE-ID was last written at (LAMPORT . ORIGIN) on this replica."
  (with-recursive-lock-held ((field-stamps-lock graph))
    (let* ((tbl (ensure-field-stamps graph))
           (alist (gethash node-id tbl))
           (pair (assoc slot alist)))
      (if pair
          (setf (cdr pair) (cons lamport origin))
          (setf (gethash node-id tbl) (acons slot (cons lamport origin) alist)))))
  (values lamport origin))

(defun apply-field-stamp-updates (graph node-id stamp-updates)
  "Persist MERGE-AUTHORED-FIELDS's STAMP-UPDATES (slot -> (lamport . origin)) for NODE-ID."
  (dolist (u stamp-updates)
    (set-node-field-stamp graph node-id (car u) (cadr u) (cddr u))))

(defun remove-node-field-stamps (graph node-id)
  "Drop all of NODE-ID's stamps (on purge/delete)."
  (when (field-stamps graph)
    (with-recursive-lock-held ((field-stamps-lock graph))
      (remhash node-id (field-stamps graph)))))

;;; Persistence (v1: a printed snapshot on open/close; ids/origins as hex so the
;;; form is portable + human-legible).

(defgeneric field-stamps-file (graph)
  (:method (graph)
    (make-pathname :name "field-stamps" :type "dat" :defaults (location graph))))

(defun persist-field-stamps (graph)
  (when (field-stamps graph)
    (with-recursive-lock-held ((field-stamps-lock graph))
      (with-open-file (s (field-stamps-file graph) :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)))
            (write
             (loop for id being the hash-keys of (field-stamps graph) using (hash-value alist)
                   collect (cons (peer-id->hex id)
                                 (loop for (slot . stamp) in alist
                                       collect (list slot (car stamp)
                                                     (and (cdr stamp) (peer-id->hex (cdr stamp)))))))
             :stream s))))))
  graph)

(defun load-field-stamps (graph)
  (let ((tbl (make-hash-table :test 'equalp))
        (file (field-stamps-file graph)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)) (*read-eval* nil))
            (dolist (entry (read s nil nil))
              (setf (gethash (peer-hex->id (car entry)) tbl)
                    (loop for (slot lamport origin-hex) in (cdr entry)
                          collect (cons slot (cons lamport
                                                   (and origin-hex (peer-hex->id origin-hex)))))))))))
    (setf (field-stamps graph) tbl)))

;;; ---------------------------------------------------------------------------
;;; Node-origins side-store (#6, unique :ORIGIN scope): node-id -> the authoring
;;; origin captured at CREATE, fixed for the node's lifetime.  Same substrate and
;;; persistence shape as FIELD-STAMPS (an in-memory map snapshotted on open/close).
;;; ---------------------------------------------------------------------------

;; The NIL fallback for a plain graph (including a peer graph reopened without
;; :PEER-ROLE) is in graph-class.lisp -- it must load for core-only consumers.
;; %NODE-ORIGIN then falls back to the graph origin, i.e. :ORIGIN == :LOCAL.

(defun ensure-node-origins (graph)
  (or (node-origins graph)
      (setf (node-origins graph) (make-hash-table :test 'equalp))))

(defun get-node-origin (graph node-id)
  "The creation origin recorded for NODE-ID, or NIL if none (or not a peer graph)."
  (and (node-origins graph)
       (with-recursive-lock-held ((node-origins-lock graph))
         (gethash node-id (node-origins graph)))))

(defun set-node-origin (graph node-id origin)
  "Record NODE-ID's creation ORIGIN once; a later call never overwrites it (the
partition is fixed at create).  No-op when ORIGIN is NIL."
  (when origin
    (with-recursive-lock-held ((node-origins-lock graph))
      (let ((tbl (ensure-node-origins graph)))
        (unless (gethash node-id tbl)
          (setf (gethash node-id tbl) origin)))))
  origin)

(defun remove-node-origin (graph node-id)
  "Drop NODE-ID's origin (on purge/hard-delete)."
  (when (node-origins graph)
    (with-recursive-lock-held ((node-origins-lock graph))
      (remhash node-id (node-origins graph)))))

(defgeneric node-origins-file (graph)
  (:method (graph)
    (make-pathname :name "node-origins" :type "dat" :defaults (location graph))))

(defun persist-node-origins (graph)
  (when (node-origins graph)
    (with-recursive-lock-held ((node-origins-lock graph))
      (with-open-file (s (node-origins-file graph) :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)))
            (write
             (loop for id being the hash-keys of (node-origins graph) using (hash-value origin)
                   collect (cons (peer-id->hex id) (and origin (peer-id->hex origin))))
             :stream s))))))
  graph)

(defun load-node-origins (graph)
  (let ((tbl (make-hash-table :test 'equalp))
        (file (node-origins-file graph)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)) (*read-eval* nil))
            (dolist (entry (read s nil nil))
              (setf (gethash (peer-hex->id (car entry)) tbl)
                    (and (cdr entry) (peer-hex->id (cdr entry)))))))))
    (setf (node-origins graph) tbl)))

(defun authored-changed-slots (write)
  "The slots an authored WRITE actually changed -- all of a create's data, or the
diff of an update's new vs old.  Used to stamp locally-authored edits."
  (etypecase write
    (tx-delete '())                                     ; a delete stamps nothing
    (tx-create (mapcar #'car (data (node write))))
    (tx-update (let ((new (data (node write))) (old (data (old-node write))))
                 (loop for (slot . val) in new
                       unless (equalp val (cdr (assoc slot old)))
                       collect slot)))))

(defun merge-authored-fields (policy node-type node-id
                              local-data new-data old-data
                              incoming-lamport incoming-origin local-stamps)
  "Resolve one incoming authored update against local state, per field, by bucket.

LOCAL-DATA   the receiver's current data alist (slot . value).
NEW-DATA     the op's post-edit data; OLD-DATA its base (what the author saw).
             Only slots the author actually changed (new /= old) are resolved --
             an unchanged carried field never clobbers a concurrent local edit.
INCOMING-*   the op's Lamport stamp + 16-byte origin id (one per op).
LOCAL-STAMPS an alist slot -> (lamport . origin) for the receiver's fields.

Returns (values MERGED-DATA STAMP-UPDATES CONFLICTS):
  MERGED-DATA   LOCAL-DATA with the fields the incoming op WON applied;
  STAMP-UPDATES alist slot -> (lamport . origin) to persist for fields that took a
                new value (so future LWW compares against the right stamp);
  CONFLICTS     list of PEER-CONFLICT to retain/surface."
  (let ((merged (copy-alist local-data))
        (stamp-updates '())
        (conflicts '()))
    (labels ((setf-field (slot val)
               (let ((p (assoc slot merged)))
                 (if p (setf (cdr p) val) (push (cons slot val) merged))))
             (stamp (slot lam org) (push (cons slot (cons lam org)) stamp-updates))
             (surface (slot bucket kept kept-org loser loser-org loser-lam)
               (push (make-peer-conflict :node-id node-id :slot slot :bucket bucket
                                         :kept-value kept :kept-origin kept-org
                                         :loser-value loser :loser-origin loser-org
                                         :loser-lamport loser-lam)
                     conflicts)))
      (dolist (pair new-data)
        (let ((slot (car pair)))
          (when (not (equalp (cdr pair) (%alist-val slot old-data)))  ; author changed it
            (let* ((incoming (cdr pair))
                   (local (%alist-val slot local-data))
                   (ls (cdr (assoc slot local-stamps)))
                   (local-lamport (or (car ls) 0))
                   (local-origin (cdr ls))
                   (bucket (funcall (merge-policy-field-bucket-fn policy) node-type slot)))
              (cond
                ;; Values agree -> fast-forward: adopt the value (already equal),
                ;; advance the field clock if the incoming stamp is later.  No conflict.
                ((equalp local incoming)
                 (when (peer-stamp-newer-p incoming-lamport incoming-origin
                                           local-lamport local-origin)
                   (stamp slot incoming-lamport incoming-origin)))
                (t
                 (ecase bucket
                   (:lww
                    (when (peer-stamp-newer-p incoming-lamport incoming-origin
                                              local-lamport local-origin)
                      (setf-field slot incoming)
                      (stamp slot incoming-lamport incoming-origin)))
                   (:safety
                    (multiple-value-bind (winner surface-p)
                        (funcall (merge-policy-safety-merge-fn policy) slot local incoming
                                 (peer-stamp-newer-p local-lamport local-origin
                                                     incoming-lamport incoming-origin))
                      (let ((took-incoming (equalp winner incoming)))
                        (setf-field slot winner)
                        (when took-incoming (stamp slot incoming-lamport incoming-origin))
                        (when surface-p
                          (surface slot bucket
                                   winner (if took-incoming incoming-origin local-origin)
                                   (if took-incoming local incoming)
                                   (if took-incoming local-origin incoming-origin)
                                   (if took-incoming local-lamport incoming-lamport))))))
                   ((:geometry :safety-surface)
                    ;; Concurrent divergent edit -> keep local live, surface the loser.
                    (surface slot bucket
                             local local-origin incoming incoming-origin incoming-lamport)))))))))
      (values merged stamp-updates conflicts))))

;;; ===========================================================================
;;; B3: the durable conflict-record store + enumeration/resolution API.
;;;
;;; A surfaced conflict is retained for the app's REVIEW SURFACE -- the loser value +
;;; origin + Lamport, not just the kept current state (design §11.E: accountability is
;;; first-class, and this is what closes the B2 "loser doesn't converge via pull" gap
;;; -- resolution is human review, read from here, not silent auto-convergence).
;;;
;;; Retention is DURABLE (survives close/reopen) and IDEMPOTENT (a re-applied op that
;;; surfaces the same (node, slot, causing op-id) does not duplicate the record).  The
;;; substrate mirrors the field-stamp store: an in-memory list persisted as a printed
;;; snapshot; ids as hex, arbitrary field VALUES serialized to hex so any value type
;;; (string / number / keyword / geometry struct) round-trips.
;;; ===========================================================================

(defun peer-bytes->hex (bytes)
  "A byte vector as a lowercase hex string."
  (with-output-to-string (s)
    (loop for b across bytes do (format s "~2,'0x" b))))

(defun peer-hex->bytes (hex)
  "Inverse of PEER-BYTES->HEX."
  (let ((v (make-byte-vector (floor (length hex) 2))))
    (dotimes (i (length v) v)
      (setf (aref v i)
            (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun peer-value->hex (value)
  "Serialize an arbitrary conflict field VALUE to a hex string (handles every node
value type, incl. geometry structs, via the engine serializer)."
  (peer-bytes->hex (serialize value)))

(defun peer-hex->value (hex)
  "Inverse of PEER-VALUE->HEX."
  (values (deserialize (peer-hex->bytes hex))))

(defun peer-conflict-same-p (a b)
  "The idempotency key: same node, slot, and causing op-id."
  (and (equalp (peer-conflict-node-id a) (peer-conflict-node-id b))
       (eql (peer-conflict-slot a) (peer-conflict-slot b))
       (equalp (peer-conflict-op-id a) (peer-conflict-op-id b))))

(defun record-peer-conflict (graph conflict)
  "Retain CONFLICT for the app review surface -- idempotently (a re-applied op that
surfaces the same (node, slot, op-id) does not duplicate it) and durably (persisted
so the surface survives restart).  Returns CONFLICT."
  (with-recursive-lock-held ((peer-conflicts-lock graph))
    (unless (find-if (lambda (c) (peer-conflict-same-p c conflict)) (peer-conflicts graph))
      (push conflict (peer-conflicts graph))
      (persist-peer-conflicts graph)))
  conflict)

(defun get-peer-conflicts (graph &key node-id slot (include-resolved t))
  "A snapshot list of the retained conflicts (newest first), optionally filtered to
one NODE-ID and/or SLOT, and -- with INCLUDE-RESOLVED nil -- to the open ones only."
  (with-recursive-lock-held ((peer-conflicts-lock graph))
    (remove-if-not
     (lambda (c)
       (and (or (null node-id) (equalp node-id (peer-conflict-node-id c)))
            (or (null slot) (eql slot (peer-conflict-slot c)))
            (or include-resolved (not (peer-conflict-resolved-p c)))))
     (copy-list (peer-conflicts graph)))))

(defun count-peer-conflicts (graph &key (include-resolved nil))
  "How many conflicts are retained (open ones by default)."
  (length (get-peer-conflicts graph :include-resolved include-resolved)))

(defun resolve-peer-conflict (graph conflict)
  "Mark CONFLICT resolved (the app has adjudicated it) and persist.  Returns CONFLICT."
  (with-recursive-lock-held ((peer-conflicts-lock graph))
    (setf (peer-conflict-resolved-p conflict) t)
    (persist-peer-conflicts graph))
  conflict)

(defun clear-resolved-peer-conflicts (graph)
  "Drop the resolved conflicts from the retained set and persist."
  (with-recursive-lock-held ((peer-conflicts-lock graph))
    (setf (peer-conflicts graph)
          (remove-if #'peer-conflict-resolved-p (peer-conflicts graph)))
    (persist-peer-conflicts graph)))

;;; Persistence (a printed snapshot; ids as hex, values serialized to hex).

(defgeneric peer-conflicts-file (graph)
  (:method (graph)
    (make-pathname :name "conflicts" :type "dat" :defaults (location graph))))

(defun peer-conflict->printable (c)
  (list :node (peer-id->hex (peer-conflict-node-id c))
        :slot (peer-conflict-slot c)
        :bucket (peer-conflict-bucket c)
        :kept (peer-value->hex (peer-conflict-kept-value c))
        :kept-origin (and (peer-conflict-kept-origin c)
                          (peer-id->hex (peer-conflict-kept-origin c)))
        :loser (peer-value->hex (peer-conflict-loser-value c))
        :loser-origin (and (peer-conflict-loser-origin c)
                           (peer-id->hex (peer-conflict-loser-origin c)))
        :loser-lamport (peer-conflict-loser-lamport c)
        :op-id (and (peer-conflict-op-id c) (peer-id->hex (peer-conflict-op-id c)))
        :resolved (peer-conflict-resolved-p c)))

(defun printable->peer-conflict (pl)
  (make-peer-conflict
   :node-id (peer-hex->id (getf pl :node))
   :slot (getf pl :slot)
   :bucket (getf pl :bucket)
   :kept-value (peer-hex->value (getf pl :kept))
   :kept-origin (and (getf pl :kept-origin) (peer-hex->id (getf pl :kept-origin)))
   :loser-value (peer-hex->value (getf pl :loser))
   :loser-origin (and (getf pl :loser-origin) (peer-hex->id (getf pl :loser-origin)))
   :loser-lamport (getf pl :loser-lamport)
   :op-id (and (getf pl :op-id) (peer-hex->id (getf pl :op-id)))
   :resolved-p (getf pl :resolved)))

(defun persist-peer-conflicts (graph)
  (with-recursive-lock-held ((peer-conflicts-lock graph))
    (with-open-file (s (peer-conflicts-file graph) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*package* (find-package :graph-db)))
          (write (mapcar #'peer-conflict->printable (peer-conflicts graph)) :stream s)))))
  graph)

(defun load-peer-conflicts (graph)
  (let ((file (peer-conflicts-file graph)))
    (setf (peer-conflicts graph)
          (when (probe-file file)
            (with-open-file (s file :direction :input)
              (with-standard-io-syntax
                (let ((*package* (find-package :graph-db)) (*read-eval* nil))
                  (mapcar #'printable->peer-conflict (read s nil nil)))))))))
