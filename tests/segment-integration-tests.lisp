;;;; Tests for vector-segment transaction integration (Phase 2 step 3).

(in-package #:graph-db/test)

(def-suite segment-integration-suite
  :description "vector-index declaration, apply-path maintenance, recovery."
  :in graph-db-suite)

(in-suite segment-integration-suite)

;; Declared once at load time, like the other integration schema in this file.
(def-vertex si-doc ()
  ((title :type string)
   (embedding :vector-index t))
  :graph-db-integration-test)

(def-vertex si-sub (si-doc)
  ((extra))
  :graph-db-integration-test)

(test vector-index-slot-is-recognised
  "A :vector-index slot reports vector-index-p on the effective slot, and the
option is inherited by a subclass."
  (let ((doc-slot (find 'embedding (graph-db::class-slots (find-class 'si-doc))
                        :key #'graph-db::slot-definition-name))
        (sub-slot (find 'embedding (graph-db::class-slots (find-class 'si-sub))
                        :key #'graph-db::slot-definition-name)))
    (is (graph-db::vector-index-p doc-slot))
    (is (graph-db::vector-index-p sub-slot)
        "a :vector-index slot on the parent must apply to the subclass")))

(test node-vector-index-slots-lists-declared-slots
  "node-vector-index-slots returns the :vector-index slot names of a class."
  (is (member 'embedding
              (graph-db::node-vector-index-slots (find-class 'si-doc))))
  (is (null (graph-db::node-vector-index-slots
             (find-class 'graph-db::vertex)))
      "a class with no :vector-index slot has none"))

(test graph-has-empty-vector-segments-table
  "A fresh graph exposes an empty vector-segments hash."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (is (hash-table-p (graph-db::vector-segments g)))
             (is (= 0 (hash-table-count (graph-db::vector-segments g)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))
