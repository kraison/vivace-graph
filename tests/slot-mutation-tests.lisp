;;;; The slot-mutation contract (GH #135): which node a transaction may write.
;;;;
;;;; EVERY test here closes and reopens the graph.  In-session reads are served
;;;; by the node cache, so a write/read in one image passes against a graph that
;;;; is being destroyed -- that is why #135 survived the rest of the suite.

(in-package #:graph-db/test)

(def-suite slot-mutation-suite
  :description "GH #135: the slot-mutation contract, asserted after reopen."
  :in graph-db-suite)

(in-suite slot-mutation-suite)

(defparameter *sm-graph-name* :graph-db-slot-mutation-test)

(def-vertex sm-thing ()
  ((name :type string)
   (note :type string)
   (e1 :ephemeral t)
   (m1 :meta t))
  :graph-db-slot-mutation-test)

;; FROM/TO/WEIGHT are CLOS :META slots on the EDGE class itself, not part of
;; DATA -- COPY-EDGE copies them, COPY-NODE does not (GH #135).
(def-edge sm-link ()
  ()
  :graph-db-slot-mutation-test)

(defmacro with-sm-graph ((g dir) &body body)
  "A fresh on-disk graph in DIR (a bound temp directory), closed on exit."
  `(let ((,g (make-graph *sm-graph-name* (namestring ,dir)
                         :buffer-pool-size 1000)))
     (unwind-protect (let ((*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g))
       (collect-garbage))))

(defmacro with-sm-reopen ((g dir) &body body)
  "Reopen the graph in DIR.  Signals if it cannot be opened -- which is the
symptom pattern B produces, so this is load-bearing, not scaffolding."
  `(let ((,g (open-graph *sm-graph-name* (namestring ,dir)
                         :buffer-pool-size 1000)))
     (unwind-protect (let ((*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g))
       (collect-garbage))))

;; A persistent slot with an :INITFORM.  On a DISK graph COPY-NODE's
;; DATA-POINTER let MAYBE-INIT-NODE-DATA materialize the alist before any
;; :INITFORM could fire against it; a MEMORY graph has no DATA-POINTER (0),
;; so it was the only backend that ever ran the :INITFORM path (GH #135).
(def-vertex sm-defaulted ()
  ((name :type string)
   (gap :initform "FILLER"))
  :graph-db-slot-mutation-test)

(defmacro with-sm-memory-graph ((g dir) &body body)
  "A fresh MEMORY graph in DIR (a bound temp directory), closed on exit."
  `(let ((,g (graph-db::make-memory-graph *sm-graph-name* (namestring ,dir))))
     (unwind-protect (let ((*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g :snapshot-p nil))
       (collect-garbage))))

(test created-node-mutation-survives-reopen
  "PATTERN A (GH #135).  A node created and then SETF'd in the same transaction
must persist the mutation.  APPLY-TX-WRITE (tx-create) wrote construction-time
BYTES and dropped it; the tx-update path has always re-serialized."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "A")))
            (setq id (id n))
            (setf (note n) "set-after-create"))))
      (with-sm-reopen (g dir)
        (let ((n (lookup-vertex id :graph g)))
          (is (not (null n)) "the node itself must survive reopen")
          (is (equal "set-after-create" (note n))
              "the post-create mutation must survive reopen"))))))

(test created-node-mutation-survives-memory-graph-journal-replay
  "CRITICAL (GH #135).  Pattern A on a MEMORY graph with NO clean-close
checkpoint: the retained .txn file IS the durable record (see
DURABILITY-CRASH-RECOVERY, memory-graph-tests.lisp), and %COMMIT serializes
it in PREPARE-TX-PERSISTENCE, which runs BEFORE APPLY-TRANSACTION -- so a
refresh placed only in APPLY-TX-WRITE (tx-create) fixed the graph's own live
heap but left the journaled bytes stale.  A memory graph keeps every
committed .txn as its durable journal until a clean-close checkpoint (the
Android/ECL production backend), so this is the gap that survived the first
pass at #135: no crash needed, just no checkpoint yet."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph *sm-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (let ((n (make-sm-thing :name "A")))
              (setq id (id n))
              (setf (note n) "set-after-create")))))
      ;; g intentionally NOT closed/checkpointed (simulated crash) -- the
      ;; .txn journal is the only durable record of this commit.
      (let ((g2 (graph-db::open-memory-graph *sm-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((n (lookup-vertex id :graph g2)))
                 (is (not (null n)) "the node itself must survive replay")
                 (is (equal "set-after-create" (note n))
                     "the post-create mutation must survive journal replay")))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test create-then-delete-same-transaction-still-works
  "GH #135.  DELETE-NODE copies internally, but calls %COPY directly rather
than the public COPY, so it is exempt from the create-set guard COPY now
carries (see PATTERN-B-COPY-OF-CREATED-NODE-SIGNALS below).
Create-then-MARK-DELETED in one transaction worked before the guard and
must keep working -- this is the gate that catches a future change to the
guard silently breaking it."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "doomed")))
            (setq id (id n))
            (mark-deleted n))))
      (with-sm-reopen (g dir)
        (let ((n (lookup-vertex id :graph g)))
          (is (not (null n)) "the node itself must survive reopen")
          (is (deleted-p n) "the deletion must survive reopen"))))))

(test edge-endpoints-and-weight-survive-mark-deleted-same-transaction
  "GH #135.  FROM/TO/WEIGHT are CLOS :META slots on EDGE, not part of DATA --
COPY-EDGE copies them, COPY-NODE does not.  DELETE-NODE's internal %COPY
must dispatch to COPY-EDGE for an edge (the same dispatch the public COPY
uses), or a soft-deleted edge's endpoints go to +NULL-KEY+ and its weight
to the class default, silently breaking COMPACT-EDGES and the peer purge
path (both key their VE/VEV unindex off (FROM EDGE)/(TO EDGE)).  Exercised
here in the SAME transaction as the edge's own creation -- the shape that
sends DELETE-NODE through %COPY instead of the guarded public COPY."
  (with-temp-directory (dir)
    (let (aid bid eid)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((a (make-sm-thing :name "A"))
                (b (make-sm-thing :name "B")))
            (setq aid (id a) bid (id b))
            (let ((e (make-sm-link :from a :to b :weight 3.5)))
              (setq eid (id e))
              (mark-deleted e)))))
      (with-sm-reopen (g dir)
        (let ((e (lookup-edge eid :graph g)))
          (is (not (null e)) "the edge itself must survive reopen")
          (is (deleted-p e) "the deletion must survive reopen")
          (is (equalp aid (from e)) "FROM must survive MARK-DELETED")
          (is (equalp bid (to e)) "TO must survive MARK-DELETED")
          (is (= 3.5 (weight e)) "WEIGHT must survive MARK-DELETED"))))))

(test edge-endpoints-and-weight-survive-mark-deleted-later-transaction
  "GH #135, companion to the same-transaction case above.  MARK-DELETED on an
edge created in an EARLIER, already-committed transaction also goes through
DELETE-NODE's %COPY (the create-set guard never applies here regardless --
the edge is not in this later transaction's create-set -- but FROM/TO/WEIGHT
must survive either way, and this is the ordinary shape that pattern-D-style
tests already cover for the equivalent vertex case)."
  (with-temp-directory (dir)
    (let (aid bid eid)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((a (make-sm-thing :name "A"))
                (b (make-sm-thing :name "B")))
            (setq aid (id a) bid (id b))
            (setq eid (id (make-sm-link :from a :to b :weight 3.5)))))
        (with-transaction ()
          (mark-deleted (lookup-edge eid :graph g))))
      (with-sm-reopen (g dir)
        (let ((e (lookup-edge eid :graph g)))
          (is (not (null e)) "the edge itself must survive reopen")
          (is (deleted-p e) "the deletion must survive reopen")
          (is (equalp aid (from e)) "FROM must survive MARK-DELETED")
          (is (equalp bid (to e)) "TO must survive MARK-DELETED")
          (is (= 3.5 (weight e)) "WEIGHT must survive MARK-DELETED"))))))

(test pattern-b-copy-of-created-node-signals
  "PATTERN B (GH #135).  COPY of a node created in this same transaction built
a tx-update whose OLD-NODE was a pending create; it committed and closed
cleanly and the graph then could not be opened at all.  It signals at the
public COPY now."
  (with-temp-directory (dir)
    (with-sm-graph (g dir)
      (with-transaction ()
        (let ((n (make-sm-thing :name "A")))
          (signals graph-db:copying-uncommitted-node
            (copy n)))))))

(test graph-opens-after-a-rejected-pattern-b
  "The point of the guard: the transaction is refused, so no half-built node
reaches disk.  OPEN-GRAPH alone succeeding is not enough to show that -- the
damage in the un-guarded bug was in the NODE, not the graph, and surfaces
only when a DATA slot is read (LOOKUP-VERTEX alone deserializes just the
node head) -- so this asserts on a READ of a data slot, not merely that
reopen or the lookup finishes.  The COPY error is caught right where it is
raised, inside the transaction, so the surrounding CREATE still commits
normally (a non-local exit out of WITH-TRANSACTION would roll back the
whole transaction, including the create -- that is a different scenario
from what corrupted the graph in the original bug).  Before the guard, this
sequence committed, closed, and then reported open: ok, read back:
DESERIALIZATION-ERROR (see repro-135-deserialization.lisp)."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "A")))
            (setq id (id n))
            (ignore-errors
             (let ((c (copy n)))
               (setf (note c) "B")
               (save c))))))
      (with-sm-reopen (g dir)
        (let ((n (lookup-vertex id :graph g)))
          (is (not (null n)) "the create must survive: only the rejected
copy/save is discarded, not the surrounding transaction")
          (is (equal "A" (name n))
              "reading a DATA slot must succeed cleanly, not signal
DESERIALIZATION-ERROR"))))))

(test pattern-d-setf-on-looked-up-node-signals
  "PATTERN D (GH #135).  LOOKUP-NODE returns the SHARED cached instance, so a
SETF on it mutates state every other reader and thread can see, is never
persisted, and reads back correctly until restart.  This is what COPY exists
to prevent."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (with-transaction ()
          (signals graph-db:mutating-unregistered-node
            (setf (note (lookup-vertex id :graph g)) "no copy")))))))

(test setf-outside-any-transaction-signals
  "The same write with no transaction at all: also unregistered, also signals."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (signals graph-db:mutating-unregistered-node
          (setf (note (lookup-vertex id :graph g)) "no txn"))))))

(test setf-on-a-copy-is-allowed
  "PATTERN C is unchanged: a copy registered by COPY is writable, and persists."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (with-transaction ()
          (let ((c (copy (lookup-vertex id :graph g))))
            (setf (note c) "via copy")
            (save c))))
      (with-sm-reopen (g dir)
        (is (equal "via copy" (note (lookup-vertex id :graph g))))))))

(test ephemeral-and-meta-slots-stay-mutable
  "The guard covers PERSISTENT slots only.  Ephemeral and meta slots are real
CLOS slots holding per-instance state and must stay freely writable -- guarding
them would be a regression."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (let ((n (lookup-vertex id :graph g)))
          (finishes (setf (e1 n) :ephemeral-ok))
          (finishes (setf (m1 n) :meta-ok))
          (is (eq :ephemeral-ok (e1 n)))
          (is (eq :meta-ok (m1 n))))))))

(test copy-and-mark-deleted-survive-initform-on-memory-graph
  "GH #135.  COPY (and MARK-DELETED, which copies internally) must survive a
persistent :INITFORM slot on a MEMORY graph -- the one backend with no
DATA-POINTER to mask a COPY-NODE bug against a missing alist entry."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-memory-graph (g dir)
        (with-transaction ()
          (setq id (id (make-sm-defaulted :name "x"))))
        (finishes
         (with-transaction ()
           (let ((c (copy (lookup-vertex id :graph g))))
             (setf (name c) "y")
             (save c))))
        (is (equal "y" (name (lookup-vertex id :graph g))))
        ;; ECL never applies a persistent slot's :INITFORM: %MAKE-VERTEX
        ;; builds via MAKE-INSTANCE, then the caller's (SETF DATA)
        ;; overwrites the alist the initform wrote.  Pre-existing, not
        ;; this branch's doing.  GH #137.
        #-ecl
        (is (equal "FILLER" (gap (lookup-vertex id :graph g)))
            "the untouched :INITFORM-defaulted slot must survive the copy")
        (finishes
         (with-transaction ()
           (mark-deleted (lookup-vertex id :graph g))))
        (is (deleted-p (lookup-vertex id :graph g)))))))

(test copy-survives-a-makunbound-initform-slot-on-memory-graph
  "GH #135 (the durable-brick case).  SLOT-MAKUNBOUND on a persistent
:INITFORM slot drops its DATA-alist entry permanently on a MEMORY graph --
surviving close/reopen, since CL-STORE restore never re-runs initforms.  That
stays true here: this fix changes no semantics (the disk/memory divergence on
reopen is a separate, tracked bug).  A node in that state must still be
COPYable and deletable -- that is the brick this escape fixes -- with the
slot continuing to read NIL, not the class default."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph *sm-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-sm-defaulted :name "x"))))
          (slot-makunbound (lookup-vertex id :graph g) 'gap))
        (close-graph g :snapshot-p t))
      (let ((g (graph-db::open-memory-graph *sm-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g))
               (is (null (gap (lookup-vertex id :graph g)))
                   "precondition: still unbound after reopen")
               (finishes
                (with-transaction ()
                  (let ((c (copy (lookup-vertex id :graph g))))
                    (setf (name c) "z")
                    (save c))))
               (is (equal "z" (name (lookup-vertex id :graph g))))
               (is (null (gap (lookup-vertex id :graph g)))
                   "must stay unbound -- resurrecting the default would be a
semantic change this task must not make")
               (finishes
                (with-transaction ()
                  (mark-deleted (lookup-vertex id :graph g))))
               (is (deleted-p (lookup-vertex id :graph g))))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test initializing-node-escape-still-permits-writes
  "GH #135.  *INITIALIZING-NODE* is the engine-internal escape (bound by
CHANGE-NODE-CLASS and, on ECL, around construction) that the guard in
(SETF SLOT-VALUE-USING-CLASS) defers to.  Pins that the escape still
suppresses CHECK-SLOT-MUTATION-ALLOWED, not merely that the guard denies
without it -- a future edit could drop the escape and every CHANGE-CLASS
path (and ECL construction) would start signalling.  EXEMPT from the
plan's close-and-reopen rule (task 5 brief): this asserts on signalling,
not persistence, so a reopen adds cost and no signal."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (let ((n (lookup-vertex id :graph g)))
          ;; Not a copy and not created here: guarded WITHOUT the escape...
          (signals graph-db:mutating-unregistered-node
            (setf (note n) "denied"))
          ;; ...and permitted WITH it.
          (finishes
           (let ((graph-db::*initializing-node* t))
             (setf (note n) "permitted")))
          (is (equal "permitted" (note n))))))))

(test construction-does-not-trip-the-guard
  "GH #135.  MAKE-<TYPE> builds an alist and hands it to MAKE-VERTEX as DATA;
the DATA slot is assigned as a whole, so no persistent-slot SETF (and hence
no CHECK-SLOT-MUTATION-ALLOWED call) happens while a node is under
construction -- the guard cannot fire before %CREATE-NODE registers the node
in the transaction's create-set.  Pins that, since a constructor rewrite that
started SETF-ing slots per-field would signal on every write in the engine.
EXEMPT from the plan's close-and-reopen rule (task 5 brief): this asserts on
construction succeeding, not persistence, so a reopen adds cost and no
signal."
  (with-temp-directory (dir)
    (with-sm-graph (g dir)
      (finishes
       (with-transaction ()
         (make-sm-thing :name "built" :note "at construction")))
      (with-transaction ()
        (let ((n (first (map-vertices #'identity g :vertex-type 'sm-thing
                                                   :collect-p t))))
          (is (equal "at construction" (note n))))))))

(test class-redefinition-with-live-instance-does-not-signal-on-read
  "GH #135.  Redefining a class to add a persistent :INITFORM slot while an
instance is live in the node cache fires UPDATE-INSTANCE-FOR-REDEFINED-CLASS
lazily on its next slot access -- CLOS applies the new slot's :INITFORM the
same way CHANGE-CLASS does, through the guarded funnel, with no transaction
at all.  A read of an unrelated, already-existing slot must not signal."
  (with-temp-directory (dir)
    (let (id cached)
      (with-sm-graph (g dir)
        (eval '(def-vertex sm-redef-thing () ((alpha :type string))
                :graph-db-slot-mutation-test))
        (with-transaction ()
          (setq id (id (graph-db::make-vertex 'sm-redef-thing
                                              '((:alpha . "x"))
                                              :graph g))))
        (setq cached (lookup-vertex id :graph g))
        (eval '(def-vertex sm-redef-thing ()
                ((alpha :type string) (beta :initform "DEFAULT"))
                :graph-db-slot-mutation-test))
        (finishes (slot-value cached 'alpha))
        (is (equal "x" (slot-value cached 'alpha)))))))

(test create-set-guard-checks-identity-not-id
  "IMPORTANT-2 (GH #135).  OBJECT-SET-MEMBER-P keys the create-set by node id
alone (GETHASH (ID OBJECT) ...), so any instance carrying a created node's
id passed the guard -- including the SHARED cached instance a re-created id
leaves stale.  MAKE-<TYPE> accepts :ID, so any deterministic-id or
upsert-by-recreate pattern reaches this: re-create under an id already held
by a live SHARED node, and the guard let SHARED's own mutation through, lost
silently on reopen.  The guard now checks by EQ against the node CREATE-NODE
actually registered for that id."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "original")))))
      (with-sm-reopen (g dir)
        (let ((shared (lookup-vertex id :graph g)))
          (with-transaction ()
            ;; Re-create under SHARED's own id: legal (MAKE-<TYPE> accepts
            ;; :ID), and this transaction's create-set now holds an entry
            ;; for ID -- but the entry's NODE is the FRESH instance, not
            ;; SHARED.
            (make-sm-thing :id id :name "re-created")
            (signals graph-db:mutating-unregistered-node
              (setf (note shared) "leaked")))))
      (with-sm-reopen (g dir)
        (let ((n (lookup-vertex id :graph g)))
          (is (equal "re-created" (name n))
              "the re-create must have gone through")
          (is (not (equal "leaked" (note n)))
              "SHARED's rejected mutation must not have reached disk"))))))

(test copy-guard-checks-identity-not-id
  "IMPORTANT-2, the COPY half (GH #135).  COPY's create-set check has the
same id-vs-identity hazard as the SETF guard above: an id-keyed check
would treat any instance carrying a created node's id as newly created in
this transaction.  Re-create a node under an id already held by a live
SHARED instance, then COPY SHARED itself -- a legitimate copy of an
already-committed node.  An id-keyed check rejects it with
COPYING-UNCOMMITTED-NODE, because the create-set now has an entry for
SHARED's id (even though that entry's NODE is the fresh re-created
instance, not SHARED).  The identity check must not reject it."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "original")))))
      (with-sm-reopen (g dir)
        (let ((shared (lookup-vertex id :graph g))
              (copied nil))
          (with-transaction ()
            ;; Re-create under SHARED's own id: this transaction's
            ;; create-set now holds an entry for ID, but the entry's NODE
            ;; is the FRESH instance, not SHARED.
            (make-sm-thing :id id :name "re-created")
            (finishes (setq copied (copy shared))))
          (is (equal "original" (name copied))
              "COPY of the shared committed instance must succeed"))))))

(test setf-permitted-under-a-non-tx-transaction
  "IMPORTANT-1 (GH #135).  CHECK-SLOT-MUTATION-ALLOWED reads COPIES and
CREATE-SET, both TX-only readers.  With *TRANSACTION* bound to a
RESTORE-TRANSACTION (the class RECREATE-GRAPH's replay uses,
transaction-restore.lisp), the SETF guard signalled NO-APPLICABLE-METHOD
instead of behaving.  It now tests (TYPEP *TRANSACTION* 'TX) first and
trusts a non-TX transaction unconditionally, mirroring CREATE-NODE's own
guard (transactions.lisp)."
  (with-temp-directory (dir)
    (let (id)
      (with-sm-graph (g dir)
        (with-transaction () (setq id (id (make-sm-thing :name "X"))))
        (let ((n (lookup-vertex id :graph g))
              (graph-db:*transaction*
                (make-instance 'graph-db::restore-transaction
                               :transaction-id 999999999)))
          (finishes (setf (note n) "under a restore-transaction"))
          (is (equal "under a restore-transaction" (note n))))))))
