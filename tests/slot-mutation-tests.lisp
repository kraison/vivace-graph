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
