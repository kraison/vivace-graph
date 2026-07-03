;;;; Regression tests for the in-memory storage backend (memory-graph).
;;;;
;;;; Covers Steps 1-4: node CRUD with live-object reads, the ve/vev/type indexes
;;;; (map-vertices / map-edges / adjacency), durability (cl-store image + journal
;;;; replay round-trip), the spatial index, and map / map-reduce views -- each the
;;;; FiveAM form of a verification done interactively while building the backend.

(in-package #:graph-db/test)

(defparameter *mem-test-graph-name* :graph-db-memory-test)

;; Clean slate so reloading doesn't double-register the type metadata.
(eval-when (:load-toplevel :execute)
  (setf (gethash *mem-test-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex m-person ()
  ((name :type string)
   (age))
  :graph-db-memory-test)

(def-edge m-knows ()
  ()
  :graph-db-memory-test)

;; A geometry-bearing type: the :INDEX slot opts into spatial indexing.
(def-vertex m-place ()
  ((label)
   (geom :index t))
  :graph-db-memory-test)

(defmacro with-test-memory-graph ((g) &body body)
  "Fresh memory-graph in a temp dir; closed (no checkpoint) + GC'd afterward."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (graph-db::make-memory-graph *mem-test-graph-name* (namestring ,dir))))
         (declare (ignorable ,g))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(def-suite memory-graph-suite
  :description "In-memory storage backend: CRUD, indexes, durability, spatial, views."
  :in graph-db-suite)

(in-suite memory-graph-suite)

;;; --- construction / CRUD -----------------------------------------------------

(test constructs-as-memory-graph
  (with-test-memory-graph (g)
    (is-true (graph-db::memory-graph-p g))
    (is-true (graph-db::graph-open-p g))
    (is (typep (graph-db::vertex-table g) 'graph-db::mem-table))))

(test create-and-lookup-vertex
  (with-test-memory-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-m-person :name "Alice" :age 30))))
      (let ((v (lookup-vertex id)))
        (is-true v)
        (is (string= "Alice" (slot-value v 'name)))
        (is (= 30 (slot-value v 'age)))))))

(test lookup-returns-the-same-live-object
  "The whole point: a read returns the live node object, not a deserialized copy."
  (with-test-memory-graph (g)
    (let ((v (with-transaction () (make-m-person :name "Bob" :age 40))))
      (is (eq v (lookup-vertex (id v)))))))

(test update-bumps-revision
  (with-test-memory-graph (g)
    (let ((id (id (with-transaction () (make-m-person :name "Carol" :age 20)))))
      (is (= 0 (graph-db::revision (lookup-vertex id))))
      (with-transaction ()
        (let ((c (copy (lookup-vertex id))))
          (setf (slot-value c 'age) 21)
          (save c)))
      (is (= 21 (slot-value (lookup-vertex id) 'age)))
      (is (= 1 (graph-db::revision (lookup-vertex id)))))))

(test mark-deleted-is-filtered
  (with-test-memory-graph (g)
    (let ((id (id (with-transaction () (make-m-person :name "Dan")))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is-true (deleted-p (lookup-vertex id)))
      (is (zerop (length (map-vertices #'identity g :vertex-type 'm-person :collect-p t)))))))

;;; --- indexes / traversal -----------------------------------------------------

(test map-vertices-typed-and-untyped
  (with-test-memory-graph (g)
    (with-transaction ()
      (make-m-person :name "A") (make-m-person :name "B") (make-m-person :name "C"))
    (is (= 3 (length (map-vertices #'identity g :vertex-type 'm-person :collect-p t))))
    (is (= 3 (length (map-vertices #'identity g :collect-p t))))))

(test edge-adjacency
  (with-test-memory-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-m-person :name "A")
              b (make-m-person :name "B")
              c (make-m-person :name "C")))
      (with-transaction ()
        (make-m-knows :from a :to b)
        (make-m-knows :from a :to c)
        (make-m-knows :from b :to c))
      (is (= 2 (length (outgoing-edges (lookup-vertex (id a))))))
      (is (= 2 (length (incoming-edges (lookup-vertex (id c))))))
      (is (= 1 (length (outgoing-edges (lookup-vertex (id b)))))))))

;;; --- durability --------------------------------------------------------------

(test durability-clean-roundtrip
  "Close (cl-store image checkpoint) then reopen: nodes, indexes and adjacency
are all restored."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) ids)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (let ((a (make-m-person :name "A")) (b (make-m-person :name "B")))
              (make-m-knows :from a :to b)
              (setq ids (list (id a) (id b))))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 2 (graph-db::mem-table-count (graph-db::vertex-table g2))))
               (is (= 1 (graph-db::mem-table-count (graph-db::edge-table g2))))
               (is (string= "A" (slot-value (lookup-vertex (first ids)) 'name)))
               (is (= 1 (length (outgoing-edges (lookup-vertex (first ids)))))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test durability-crash-recovery
  "A commit with no clean close survives: the retained .txn journal replays on
the next open."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction () (setq id (id (make-m-person :name "Survivor" :age 99))))))
      ;; g intentionally NOT closed (simulated crash).
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((v (lookup-vertex id)))
                 (is-true v)
                 (is (string= "Survivor" (slot-value v 'name)))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; --- spatial index -----------------------------------------------------------

(test spatial-bbox-and-reopen
  "Geometry vertices auto-index; bbox query returns the in-window set; the index
is rebuilt on reopen."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (make-m-place :label "kharkiv" :geom (graph-db::make-point 36.3d0 50.0d0))
            (make-m-place :label "lviv"    :geom (graph-db::make-point 24.0d0 49.8d0))
            (make-m-place :label "london"  :geom (graph-db::make-point -0.1d0 51.5d0)))
          (let ((hits (graph-db::spatial-index-query-bbox
                       (graph-db::spatial-index g) 22.0d0 48.0d0 40.0d0 51.0d0)))
            (is (= 2 (length hits)))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((hits (graph-db::spatial-index-query-bbox
                          (graph-db::spatial-index g2) 22.0d0 48.0d0 40.0d0 51.0d0)))
               (is (= 2 (length hits))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; --- views -------------------------------------------------------------------

(test map-and-reduce-views-with-reopen
  "A map view (sorted) and a map-reduce view (aggregate) are maintained through
transactions and rebuilt in-RAM on reopen."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (def-view m-by-name :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (slot-value p 'name) (slot-value p 'age)))))
          (def-view m-by-decade :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
            (:reduce (lambda (keys values) (declare (ignore keys)) (reduce #'+ values))))
          (with-transaction ()
            (make-m-person :name "carol" :age 30)
            (make-m-person :name "alice" :age 25)
            (make-m-person :name "bob"   :age 35)))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               ;; map view: names in sorted order
               (is (equal '("alice" "bob" "carol")
                          (map-view (lambda (k id v) (declare (ignore id v)) k)
                                    'm-person 'm-by-name :graph g2 :collect-p t)))
               ;; map-reduce: decade 2 -> 1 (alice), decade 3 -> 2 (bob, carol)
               (is (equal '((2 . 1) (3 . 2))
                          (map-reduced-view (lambda (k id v) (declare (ignore id)) (cons k v))
                                            'm-person 'm-by-decade :graph g2 :collect-p t))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
