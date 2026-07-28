;;;; Skip-List & B+ Tree Index Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-index-subsystem (&key (count 2000) (sprof-mode :cpu))
  "Profile ordered-map index backends (Skip-List vs B+ Tree)."
  (let* ((temp-dir #P"/tmp/vg-profiler-index-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :index-prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let* ((graph-db:*graph* graph)
                (heap (graph-db::heap graph)))
           (profile-block (:name (format nil "Index Backends (~:D entries)" count)
                           :subsystems '(:skip-list :bplus-tree)
                           :sprof-mode sprof-mode)
             ;; 1. Skip-List insertions & lookups
             (let ((sl (graph-db::make-skip-list :heap heap
                                                  :key-equal 'equal
                                                  :key-comparison 'string<
                                                  :key-serializer 'graph-db::serialize
                                                  :key-deserializer 'graph-db::deserialize
                                                  :value-serializer 'graph-db::serialize
                                                  :value-deserializer 'graph-db::deserialize)))

               (dotimes (i count)
                 (graph-db::add-to-skip-list sl (format nil "key-~8,'0D" i) i))
               (dotimes (i count)
                 (graph-db::%find-in-skip-list sl (format nil "key-~8,'0D" i))))

             ;; 2. B+ Tree insertions, binary searches, & decoding
             (let ((bpt (graph-db::make-bplus-tree :heap heap :key-equal 'equal :key-comparison 'string<)))
               (dotimes (i count)
                 (let* ((key (format nil "bkey-~8,'0D" i))
                        (skey (graph-db::serialize key))
                        (sval (graph-db::serialize i)))
                   (graph-db::bpt-insert bpt key skey sval)))
               (dotimes (i count)
                 (let ((key (format nil "bkey-~8,'0D" i)))
                   (graph-db::%bpt-descend-to-leaf bpt key))))))



      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))
