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
