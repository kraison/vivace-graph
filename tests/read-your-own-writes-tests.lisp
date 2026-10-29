;;;; tests/read-your-own-writes-tests.lisp -- a transaction sees its own
;;;; writes (GH #408).  Spec: docs/superpowers/specs/
;;;; 2026-10-29-read-your-own-writes-design.md.

(in-package #:graph-db/test)

(def-suite read-your-own-writes-suite
  :description "GH #408: reads inside a transaction see its own writes."
  :in graph-db-suite)

(in-suite read-your-own-writes-suite)

(defparameter *ryow-graph-name* :graph-db-ryow-test)

(def-vertex ryow-node ()
  ((k :type string :accessor ryow-node-k)
   (tag :type string :accessor ryow-node-tag :index t))
  :graph-db-ryow-test)

(def-edge ryow-link () () :graph-db-ryow-test)

(def-index ryow-node (k tag) :graph-db-ryow-test)

(defmacro with-ryow-graph ((g) &body body)
  "A fresh on-disk graph with *GRAPH* bound to it, closed on exit."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ryow-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defmacro ryow-both-caches (() &body body)
  "Run BODY with the shared node cache on, then off: the cache hides
identity bugs (CLAUDE.md, Tests).  BODY makes its own graph."
  `(dolist (on '(t nil))
     (let ((graph-db::*cache-enabled* on))
       ,@body)))

;;; --- Task 1: the overlay core and the switch -----------------------------

(test ryow-overlay-effective-write-last-wins
  "Create then delete of one id leaves a TX-DELETE and no live node;
update then update leaves the later copy (spec §4.3)."
  (with-ryow-graph (g)
    (with-transaction ()
      (let ((v (make-ryow-node :k "a" :tag "t")))
        (mark-deleted v)
        (let ((ov (graph-db::overlay-of graph-db::*transaction*)))
          (is (typep (graph-db::overlay-effective-write ov (id v))
                     'graph-db::tx-delete))
          (is (null (graph-db::overlay-live-nodes ov))))))
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "v1" :tag "t")))
      (with-transaction ()
        (let ((c1 (copy a))) (setf (ryow-node-k c1) "v2") (save c1))
        (let ((c2 (copy a))) (setf (ryow-node-k c2) "v3") (save c2))
        (let* ((ov (graph-db::overlay-of graph-db::*transaction*))
               (w (graph-db::overlay-effective-write ov (id a))))
          (is (equal "v3" (ryow-node-k (graph-db::node w))))
          (is (= 1 (length (graph-db::overlay-live-nodes ov)))))))))

(test ryow-switch-off-under-commit-validators
  "*READ-OWN-WRITES* is NIL inside %COMMIT's validators and inside
VALIDATE-WRITES, T in the transaction body (spec §4.6)."
  (with-ryow-graph (g)
    (let* ((seen :unset)
           (graph-db::*commit-validators*
             (list (lambda (tx graph) (declare (ignore tx graph))
                     (setf seen graph-db::*read-own-writes*)))))
      (with-transaction ()
        (is (eq t graph-db::*read-own-writes*))
        (make-ryow-node :k "a" :tag "t"))
      (is (null seen))
      (setf seen :unset)
      (with-transaction ()
        (make-ryow-node :k "b" :tag "t")
        (graph-db:validate-transaction g)
        (is (null seen))
        (setf seen :unset))
      (is (null seen)))))
