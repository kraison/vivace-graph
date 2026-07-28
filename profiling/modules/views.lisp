;;;; Views Engine Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-views-subsystem (&key (count 1000) (sprof-mode :cpu))
  "Profile map/reduce view creation, indexing, regeneration, and query dispatch."
  (let* ((temp-dir #P"/tmp/vg-profiler-views-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let ((graph-db:*graph* graph))
           (graph-db::install-views graph)
           (profile-block (:name (format nil "Views & Map/Reduce Indexing (~:D nodes)" count)
                           :subsystems '(:views)
                           :sprof-mode sprof-mode)
             ;; Populate nodes (automatically indexed by views engine)
             (graph-db:with-transaction ()
               (dotimes (i count)
                 (graph-db/profiler::make-prof-node :value i :label (format nil "cat-~D" (mod i 10)))))


             ;; Query view
             (dotimes (_ 100)
               (graph-db:invoke-graph-view 'graph-db/profiler::prof-node
                                           'graph-db/profiler::prof-view
                                           :graph graph))))



      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))
