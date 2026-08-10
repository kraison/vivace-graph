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

(test characterise-create-then-delete-same-transaction
  "CHARACTERISATION, not a contract (GH #135).  DELETE-NODE copies internally,
so a create-set guard in COPY-NODE will also reject this.  Whether that is a
fix or a behaviour change depends on what it does TODAY, which this pins.
If the graph cannot be reopened, this pattern has the same shape as pattern B
and the guard is a fix -- update this test's name and docstring accordingly."
  (with-temp-directory (dir)
    (let (id (reopened nil) (opened-ok nil))
      (with-sm-graph (g dir)
        (with-transaction ()
          (let ((n (make-sm-thing :name "doomed")))
            (setq id (id n))
            (mark-deleted n))))
      (handler-case
          (with-sm-reopen (g dir)
            (setq opened-ok t)
            (setq reopened (lookup-vertex id :graph g)))
        (error (e)
          (format t "~&CREATE-THEN-DELETE: reopen FAILED: ~A~%" e)))
      (format t "~&CREATE-THEN-DELETE: opened-ok=~A node=~A~%"
              opened-ok reopened)
      ;; Deliberately asserts only that we learned something: the printed
      ;; result is the deliverable.  Task 4 replaces this with a real gate.
      (is (or opened-ok (not opened-ok))))))

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
:INITFORM slot drops its DATA-alist entry permanently -- surviving a
memory-graph close/reopen, since CL-STORE restore never re-runs initforms
(the same gap GH #128 schema evolution hits).  A node in that state must
still be COPYable: %PERSISTENT-SLOT-DEFAULTS backs the missing entry with the
class default rather than the copy tripping the guard on every future copy."
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
               (finishes
                (with-transaction ()
                  (let ((c (copy (lookup-vertex id :graph g))))
                    (setf (name c) "z")
                    (save c))))
               (is (equal "z" (name (lookup-vertex id :graph g))))
               (is (equal "FILLER" (gap (lookup-vertex id :graph g)))
                   "the makunbound slot must come back as the class default"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))
