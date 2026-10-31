;;;; tests/query/suite.lisp -- runner + fixture for graph-db/query
;;;; (GH #322).

(in-package #:graph-db/query-test)

(def-suite query-suite
  :description "The guarded query runner, called directly (GH #322).")

(defun run-query-tests ()
  "Run the suite; T when every check passed.  Invoked by
(asdf:test-system :graph-db/query-test)."
  (log:config :error)
  (let* ((system-dir (graph-db-test-scratch:make-scratch-directory
                      "graph-db-query-sys"))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'query-suite)))
           (explain! results)
           (results-status results))
      (graph-db-test-scratch:cleanup-scratch-run))))

;; A schema in a package that USES NOTHING: the case the head-resolution
;; change (spec SS6) exists for.  Domain-neutral per repo policy #197.
(defpackage #:graph-db/query-test.schema (:use))

(defparameter *graph-name* :query-test-graph)

(eval-when (:load-toplevel :execute)
  (setf (gethash *graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex graph-db/query-test.schema::qt-item ()
  ((label :type string) (rank))
  :query-test-graph)

(def-edge graph-db/query-test.schema::qt-links ()
  ()
  :query-test-graph)

(defmacro with-query-graph ((g) &body body)
  `(let* ((dir (graph-db-test-scratch:make-scratch-directory
               "graph-db-query"))
          (,g (make-graph *graph-name* (namestring dir)
                          :buffer-pool-size 1000)))
     (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g)))))

(defun seed (g)
  "Three items and two links; returns the items in rank order."
  (with-transaction ((graph-db::transaction-manager g))
    (let ((a (graph-db/query-test.schema::make-qt-item
              :graph g :label "a" :rank 1))
          (b (graph-db/query-test.schema::make-qt-item
              :graph g :label "b" :rank 2))
          (c (graph-db/query-test.schema::make-qt-item
              :graph g :label "c" :rank 3)))
      (graph-db/query-test.schema::make-qt-links :graph g :from a :to b)
      (graph-db/query-test.schema::make-qt-links :graph g :from b :to c)
      (list a b c))))
