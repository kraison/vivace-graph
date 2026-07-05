;;;; Shared fixtures for the graph-db/algorithms suite: a tiny schema, scratch
;;;; graph lifecycle, and a helper that populates a graph from a node/edge spec.

(in-package #:graph-db/algorithms-test)

;; Register the test schema once; clear first so reloading this file doesn't
;; double-register the type metadata for our test graph.
(eval-when (:load-toplevel :execute)
  (setf (gethash :graph-db-algorithms-test graph-db::*schema-node-metadata*) nil))

(def-vertex an ()
  ((name :type string))
  :graph-db-algorithms-test)

(def-edge ae ()
  ()
  :graph-db-algorithms-test)

;; A second, unrelated edge type, for the edge-type-list shortest-path test.
(def-edge ae2 ()
  ()
  :graph-db-algorithms-test)

;;; ---- scratch graph lifecycle -------------------------------------

(defun make-temp-directory ()
  (let ((dir (merge-pathnames
              (format nil "gda-test-~36R/" (random (expt 36 12)))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,var :validate t)))))

(defmacro with-algo-graph ((g) &body body)
  "Bind G (and *GRAPH*) to a fresh on-disk graph of the test schema, run BODY,
then tear it down."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph :graph-db-algorithms-test
                             (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil)))))))

;;; ---- population helper -------------------------------------------

(defun populate (names edges)
  "Within an open transaction, create one AN vertex per NAME and one AE edge per
EDGE spec.  NAMES is a list of strings.  EDGES is a list of (FROM-NAME TO-NAME
&optional WEIGHT) -- WEIGHT defaults to 1.0.  Returns a string->vertex hash."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (n names)
      (setf (gethash n h) (make-an :name n)))
    (loop for (a b w) in edges do
      (make-ae :from (gethash a h) :to (gethash b h)
               :weight (coerce (or w 1.0) 'float)))
    h))

(defmacro with-populated-graph ((g h names edges) &body body)
  "Build a scratch graph G, populate it (returning the name->vertex hash in H)
from NAMES/EDGES inside a transaction, then run BODY."
  `(with-algo-graph (,g)
     (let ((,h nil))
       (with-transaction ()
         (setf ,h (populate ,names ,edges)))
       ,@body)))

(defun path-names (vertices)
  "Map an ordered vertex list to its ordered list of name strings."
  (mapcar (lambda (v) (slot-value v 'name)) vertices))
