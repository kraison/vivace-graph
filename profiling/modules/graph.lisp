;;;; Graph Core & Node/Edge Lookup Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-graph-subsystem (&key (vertex-count 1000) (sprof-mode :cpu))
  "Profile high-level vertex & edge creation, lookup, and graph storage lifecycle."
  (let* ((temp-dir #P"/tmp/vg-profiler-graph-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let ((graph-db:*graph* graph))
           (profile-block (:name (format nil "Graph Storage & Lookup (~:D vertices)" vertex-count)
                           :subsystems '(:graph-core)
                           :sprof-mode sprof-mode)
             ;; 1. Concurrent vertex creation inside transactions
             (let ((vertices nil))
               (graph-db:with-transaction ()
                 (dotimes (i vertex-count)
                   (let ((v (graph-db/profiler::make-prof-node :value i :label (format nil "node-~D" i))))
                     (push v vertices))))

               ;; 2. Vertex lookups by ID
               (dolist (v vertices)
                 (graph-db:lookup-vertex (graph-db::id v)))

               ;; 3. Edge creation between vertices
               (let ((v1 (first vertices))
                     (v2 (second vertices)))
                 (when (and v1 v2)
                   (graph-db:with-transaction ()
                     (dotimes (i (floor vertex-count 2))
                       (graph-db/profiler::make-prof-link v1 v2 :weight 1.0 :label (format nil "rel-~D" i)))))))))
      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))


