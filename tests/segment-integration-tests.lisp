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

(defun %si-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(defun %si-segment (graph slot)
  (gethash (cons 'si-doc slot) (graph-db::vector-segments graph)))

(test create-populates-the-segment
  "Creating a node with a conforming :vector-index value, through a transaction,
lazily creates the segment and stores the vector under the node id."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0))))))
             (let ((seg (%si-segment g 'embedding)))
               (is (not (null seg)) "segment was not created on insert")
               (let ((back (graph-db::segment-get seg id)))
                 (is (typep back '(simple-array single-float (*)))
                     "vector not stored (got ~S)" back)
                 (is (= 8 (length back)))
                 (is (every #'= (%si-embedding 8 1.0) back)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test create-without-conforming-value-makes-no-segment
  "A node whose :vector-index slot is nil creates no segment."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction () (make-si-doc :title "no-vec")))
             (is (null (%si-segment g 'embedding))
                 "a nil embedding must not create a segment"))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test update-overwrites-the-vector (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) (%si-embedding 8 5.0))
                 (save v))))
           (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
             (is (typep back '(simple-array single-float (*))))
             (is (every #'= (%si-embedding 8 5.0) back))))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test clearing-the-value-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) nil)
                 (save v))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "an update to nil must remove the segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test delete-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction () (mark-deleted (lookup-vertex id :graph g))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "deleting a node must remove its segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test wrong-dimension-signals-and-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             ;; a 9-dim vector into an established 8-dim segment must signal
             (signals error
               (let ((*graph* g))
                 (with-transaction ()
                   (make-si-doc :title "bad" :embedding (%si-embedding 9 2.0))))))
           ;; the good node is still there; the bad transaction rolled back
           (is (every #'= (%si-embedding 8 1.0)
                      (graph-db::segment-get (%si-segment g 'embedding) id)))
           (is (= 1 (graph-db::segment-live-count (%si-segment g 'embedding)))
               "the rolled-back insert must not have landed in the segment"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))
