;;;; Transactions & ACID Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-transactions-subsystem (&key (tx-count 500) (sprof-mode :cpu))
  "Profile transaction engine, OCC validation, commit, rollback, and conflict checks."
  (let* ((temp-dir #P"/tmp/vg-profiler-txn-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let ((graph-db:*graph* graph))
           (profile-block (:name (format nil "Transactions & ACID OCC Engine (~:D txns)" tx-count)
                           :subsystems '(:transactions)
                           :sprof-mode sprof-mode)
             ;; 1. Small discrete transactions
             (dotimes (i tx-count)
               (graph-db:with-transaction ()
                 (graph-db:make-vertex 'graph-db/profiler::prof-node
                                        (list :value i :label "txn-test"))))

             
             ;; 2. Read-set vs Write-set validation checks
             (let ((s1 (graph-db::make-object-set nil))
                   (s2 (graph-db::make-object-set nil)))
               (graph-db:with-transaction ()
                 (let ((v1 (graph-db/profiler::make-prof-node :value 1 :label "v1"))
                       (v2 (graph-db/profiler::make-prof-node :value 2 :label "v2")))
                   (graph-db::add-to-object-set v1 s1)
                   (graph-db::add-to-object-set v2 s2)
                   (graph-db::add-to-object-set v1 s2)))
               (dotimes (_ 10000)
                 (graph-db::object-sets-intersect-p s1 s2)))))



      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))
