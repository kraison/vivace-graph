;;;; Spatial Index & GEOS Integration Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-spatial-subsystem (&key (point-count 1000) (sprof-mode :cpu))
  "Profile spatial indexing, geohashing, bounding box queries, and GEOS predicates."
  (let* ((temp-dir #P"/tmp/vg-profiler-spatial-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :spatial-prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let* ((graph-db:*graph* graph)
                (heap (graph-db::heap graph)))
           (profile-block (:name (format nil "Spatial Geohash & GEOS Bridge (~:D points)" point-count)
                           :subsystems '(:spatial :geos)
                           :sprof-mode sprof-mode)
             ;; 1. Spatial index creation & insertion
             (let ((sp-idx (graph-db::make-spatial-index heap)))
               (dotimes (i point-count)
                 (let ((lat (- 37.77 (float (/ i 1000.0) 1.0)))
                       (lon (- -122.41 (float (/ i 1000.0) 1.0)))
                       (node-id (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
                   (graph-db:spatial-index-insert sp-idx node-id (graph-db::make-point lon lat))))

               ;; 2. Bounding Box Queries
               (dotimes (_ 500)
                 (graph-db:spatial-index-query-bbox sp-idx -123.0 37.0 -122.0 38.0)))



             ;; 3. GEOS C Bridge geometry operations
             (when (fboundp 'graph-db::geometry-intersects-p)
               (let ((wkt1 "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))")
                     (wkt2 "POLYGON((5 5, 5 15, 15 15, 15 5, 5 5))"))
                 (dotimes (_ 1000)
                   (ignore-errors
                     (graph-db::geometry-intersects-p wkt1 wkt2)))))))
      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))
