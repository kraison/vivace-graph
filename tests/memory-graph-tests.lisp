;;;; Regression tests for the in-memory storage backend (memory-graph).
;;;;
;;;; Covers Steps 1-4: node CRUD with live-object reads, the ve/vev/type indexes
;;;; (map-vertices / map-edges / adjacency), durability (cl-store image + journal
;;;; replay round-trip), the spatial index, and map / map-reduce views -- each the
;;;; FiveAM form of a verification done interactively while building the backend.

(in-package #:graph-db/test)

(defparameter *mem-test-graph-name* :graph-db-memory-test)

;; Clean slate so reloading doesn't double-register the type/view metadata.
(eval-when (:load-toplevel :execute)
  (setf (gethash *mem-test-graph-name* graph-db::*schema-node-metadata*) nil)
  (remhash *mem-test-graph-name* graph-db::*schema-view-metadata*))

(defun reset-mem-view-registry ()
  "Isolate a test's view registry from earlier tests: the global
*SCHEMA-VIEW-METADATA* accumulates specs by graph-name across the whole run, so
INSTALL-VIEWS at open would otherwise (re)build every view any prior test defined
over m-person -- which, on a lazy graph, materializes nodes and breaks the
fault-on-access invariant.  Call at the start of any test that reopens a graph."
  (remhash *mem-test-graph-name* graph-db::*schema-view-metadata*))

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

;; A SECOND geometry-bearing type declaring its OWN geometry slot, so it lands in
;; a DISTINCT (owner . slot) spatial index from M-PLACE -- the per-index image
;; round-trip needs two indexes to prove it keeps them apart.
(def-vertex m-region ()
  ((label)
   (extent :type geometry :index t))
  :graph-db-memory-test)

;; A geometry-LESS type with an indexed SCALAR slot.  NODE-GEOMETRY returns NIL for
;; it (a string is not a geometry), so it is never spatially indexed -- yet its
;; :INDEX slot is exactly what the old lazy-open spatial REBUILD over-approximated
;; as "may be spatial", faulting every such node in and defeating :LAZY.
(def-vertex m-tag ()
  ((label :type string :index t))
  :graph-db-memory-test)

;; A :vector-index slot, for the vector-segment reopen test below (GH #58).
(def-vertex m-doc ()
  ((title :type string)
   (embedding :vector-index t))
  :graph-db-memory-test)

;; A :unique slot, isolated to its own type so the v5->v6 migration test below
;; (GH #65) can't interact with any other test's duplicate-name fixtures.
(def-vertex m-widget ()
  ((sku :type string :unique t))
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
                       (graph-db::spatial-index-for g 'm-place 'geom) 22.0d0 48.0d0 40.0d0 51.0d0)))
            (is (= 2 (length hits)))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((hits (graph-db::spatial-index-query-bbox
                          (graph-db::spatial-index-for g2 'm-place 'geom) 22.0d0 48.0d0 40.0d0 51.0d0)))
               (is (= 2 (length hits))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; --- vector segments -----------------------------------------------------

(defun %m-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(test vector-segment-survives-reopen
  "GH #58: a :vector-index slot's segment must survive a clean close/reopen of a
memory graph, not just live while the image is up.  OPEN-MEMORY-GRAPH did not
call RESTORE-VECTOR-SEGMENTS at all, so before the fix VECTOR-SEARCH returns
nothing on the reopened graph even though the segment file (written by ordinary
live maintenance, same as the on-disk backend) is sitting right there on disk."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) near far)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (setq near (id (make-m-doc :title "near" :embedding (%m-embedding 8 1.0))))
            (setq far (id (make-m-doc :title "far" :embedding (%m-embedding 8 50.0)))))
          ;; Confirm the segment is live and correct BEFORE the reopen, so a
          ;; later empty result can only be the reopen's fault.
          (let ((hits (vector-search g 'm-doc 'embedding (%m-embedding 8 1.0) 5)))
            (is (= 2 (length hits)))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((hits (vector-search g2 'm-doc 'embedding (%m-embedding 8 1.0) 5)))
               (is (= 2 (length hits))
                   "vector segment did not survive the reopen -- got ~S" hits)
               (is (member far (mapcar #'cdr hits) :test #'equalp)
                   "the far doc ~A is missing from the reopened segment" far)
               (when hits
                 (is (equalp near (cdr (first hits)))
                     "the nearest doc should rank first; got ~S" hits)))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test memory-image-round-trips-per-class-spatial-indexes
  "The image carries one spatial record per (owner . slot): a memory graph with TWO
distinct geometry classes reopens with BOTH indexes restored STRUCTURALLY.  The
canary -- a raw index entry under an id no live node has -- is what proves it: a
rebuild-from-nodes could not reproduce it, so its survival across the reopen means
the indexes were loaded from the image, not re-derived by scanning the nodes."
  (reset-mem-view-registry)
  (with-temp-directory (dir)
    (let ((loc (namestring dir))
          (canary (graph-db::gen-vertex-id))
          place-id region-id)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction ()
            (setq place-id (id (make-m-place :label "kharkiv"
                                             :geom (graph-db::make-point 36.3d0 50.0d0))))
            (setq region-id (id (make-m-region
                                 :label "east"
                                 :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))))
          ;; Canary straight into M-PLACE's index at a corner nothing else occupies.
          (graph-db::spatial-index-insert
           (graph-db::spatial-index-for g 'm-place 'geom)
           canary (graph-db::make-point 10d0 10d0)))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((place-ix  (graph-db::spatial-index-for g2 'm-place 'geom))
                   (region-ix (graph-db::spatial-index-for g2 'm-region 'extent)))
               (is (graph-db::spatial-index-p place-ix))
               (is (graph-db::spatial-index-p region-ix))
               (is (not (eq place-ix region-ix))
                   "the two classes keep their own indexes across a reopen")
               (is (has-p place-id
                          (graph-db::spatial-index-query-bbox
                           place-ix 36.0d0 49.9d0 36.5d0 50.1d0)))
               (is (has-p region-id
                          (graph-db::spatial-index-query-bbox
                           region-ix 30.0d0 48.0d0 31.0d0 49.0d0)))
               (is (has-p canary
                          (graph-db::spatial-index-query-bbox
                           place-ix 9.0d0 9.0d0 11.0d0 11.0d0))
                   "the canary survived -- the index was RESTORED structurally, not ~
                    rebuilt from the live nodes"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test memory-coarse-geometry-survives-a-reopen
  "The image carries each index's PRECISION HISTOGRAM, not just its cells.  A
geometry too large to cover within +SPATIAL-INSERT-MAX-CELLS+ is stored coarsely
and the query clamp is lowered to match; that clamp is derived from the histogram,
which lives only in RAM between closes.  An image that dropped it would reopen with
the clamp back at the configured precision, and a prefix range scan would sort PAST
the coarser stored key -- a silent miss on an index that is physically intact."
  (reset-mem-view-registry)
  (handler-bind ((warning #'muffle-warning))    ; coarsening is EXPECTED here
    (with-temp-directory (dir)
      (let ((loc (namestring dir)) big-id)
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
          (let ((*graph* g))
            (with-transaction ()
              ;; ~1 degree square: far more than 16384 cells at precision 7, so the
              ;; insert cover is capped and the entry stored coarsely.
              (setq big-id (id (make-m-region
                                :label "big"
                                :extent (scope-rect 10d0 40d0 11d0 41d0)))))
            (let ((idx (graph-db::spatial-index-for g 'm-region 'extent)))
              (is (< (graph-db::spatial-index-coarsest-precision idx)
                     (graph-db::spatial-index-precision idx))
                  "the oversized polygon really did coarsen the index")))
          (close-graph g :snapshot-p t))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
          (unwind-protect
               (let ((idx (graph-db::spatial-index-for g2 'm-region 'extent)))
                 (is (< (graph-db::spatial-index-coarsest-precision idx)
                        (graph-db::spatial-index-precision idx))
                     "the clamp came back from the image, not reset to the ~
                      configured precision")
                 (is (has-p big-id
                            (graph-db::spatial-index-query-bbox
                             idx 10.4d0 40.4d0 10.6d0 40.6d0))
                     "a small window inside the coarsely-stored polygon still finds ~
                      it after the reopen"))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test lazy-reopen-keeps-geometryless-indexed-scalars-lazy
  "Restoring spatial STRUCTURALLY removes the lazy-open REBUILD that used to fault in
every node of any class with ANY :INDEX slot.  A class with an indexed SCALAR slot
and no geometry now reopens with its nodes STILL LZNODE blobs -- the fault-on-access
property the Android field device depends on -- while a geometry class's index is
still restored and queryable without a scan."
  (reset-mem-view-registry)
  (flet ((n-materialized (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::node-p v)))
         (n-lznodes (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::lznode-p v))))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)))
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
          (let ((*graph* g))
            (with-transaction ()
              (dotimes (i 10) (make-m-tag :label (format nil "t~D" i)))
              (make-m-place :label "site" :geom (graph-db::make-point 36.3d0 50.0d0))))
          (close-graph g :snapshot-p t))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (= 11 (graph-db::mem-table-count (graph-db::vertex-table g2))))
                 ;; The whole point: open faulted in NONE of them.  Before this task,
                 ;; the spatial rebuild materialized all 11 (10 indexed-scalar + 1
                 ;; geometry) because NODE-GEOMETRY-INDEX-SLOTS returns scalar :INDEX
                 ;; slots too.
                 (is (= 0 (n-materialized g2))
                     "no node was materialized on lazy open")
                 (is (= 11 (n-lznodes g2))
                     "every node is still a deferred LZNODE blob")
                 ;; ...and the geometry index came back structurally, queryable.
                 (is (= 1 (length (graph-db::spatial-index-query-bbox
                                   (graph-db::spatial-index-for g2 'm-place 'geom)
                                   22.0d0 48.0d0 40.0d0 51.0d0)))
                     "the geometry index restored structurally, still findable")
                 ;; And the query did not have to materialize any node either.
                 (is (= 0 (n-materialized g2))
                     "a spatial bbox query returns ids without materializing nodes"))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

;;; --- views -------------------------------------------------------------------

(test map-and-reduce-views-with-reopen
  "A map view (sorted) and a map-reduce view (aggregate) are maintained through
transactions and rebuilt in-RAM on reopen."
  (reset-mem-view-registry)
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

(test checkpoint-persists-without-clean-close
  "CHECKPOINT-MEMORY-GRAPH writes the image + clears the journal, so state survives
a restart with NO clean close of the checkpointed instance -- the durability a peer
device needs for pulled state (applied directly, never journaled).  This is the
on-device reopen gap: without a post-sync checkpoint the next open restores an
empty/stale image and the app re-cold-syncs."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) id)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (with-transaction () (setq id (id (make-m-person :name "keep" :age 7))))
          ;; checkpoint (image) but DO NOT close g -- simulated crash after.
          (graph-db::checkpoint-memory-graph g)))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 1 (graph-db::mem-table-count (graph-db::vertex-table g2))))
               (is (string= "keep" (slot-value (lookup-vertex id) 'name))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test open-restores-derived-structurally-without-regen
  "A v2 image restores views / indexes / spatial STRUCTURALLY: OPEN-MEMORY-GRAPH does
NOT call regenerate-all-views (the ~23 s on-device view rebuild, #50 / mine-action
open-bench), yet the reduce view and node tables come back correct.  This is the
boot-latency fix -- rebuild-on-open is gone for a clean/checkpointed image."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) (regen 0))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (def-view mt-decade :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
            (:reduce (lambda (keys values &optional r)
                       (declare (ignore keys r)) (reduce #'+ values))))
          (with-transaction ()
            (dotimes (i 20) (make-m-person :name (format nil "p~D" i) :age (+ 20 i)))))
        (close-graph g :snapshot-p t))
      (let ((orig (fdefinition 'graph-db::regenerate-all-views)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::regenerate-all-views)
                     (lambda (&rest a) (incf regen) (apply orig a)))
               (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
                 (unwind-protect
                      (let ((*graph* g2))
                        (is (= 0 regen)) ; structural restore -> no rebuild-on-open
                        (is (= 20 (graph-db::mem-table-count (graph-db::vertex-table g2))))
                        ;; ages 20..39 -> decade 2 (20-29): 10, decade 3 (30-39): 10
                        (is (equal '((2 . 10) (3 . 10))
                                   (map-reduced-view
                                    (lambda (k id v) (declare (ignore id)) (cons k v))
                                    'm-person 'mt-decade :graph g2 :collect-p t))))
                   (ignore-errors (close-graph g2 :snapshot-p nil))
                   (collect-garbage))))
          (setf (fdefinition 'graph-db::regenerate-all-views) orig))))))

(test structural-restore-plus-journal-tail
  "The v2 image restores views structurally, THEN the journal tail (authored writes
committed after the checkpoint) updates the restored views incrementally on open --
restore-views runs before recover-transactions in the structural path.  Checkpoint
10 (decade 2), journal 5 more (decade 3) uncheckpointed, crash-open: the view must
reflect BOTH the image aggregate and the replayed tail."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (def-view mt-tail :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
            (:reduce (lambda (keys values &optional r)
                       (declare (ignore keys r)) (reduce #'+ values))))
          (with-transaction () (dotimes (i 10) (make-m-person :name "a" :age 25)))
          (graph-db::checkpoint-memory-graph g)                   ; image: decade 2 x10
          (with-transaction () (dotimes (i 5) (make-m-person :name "b" :age 35)))
          ;; simulated crash: no close -- the 5 decade-3 nodes are journaled only
          ))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 15 (graph-db::mem-table-count (graph-db::vertex-table g2))))
               (is (equal '((2 . 10) (3 . 5))
                          (map-reduced-view
                           (lambda (k id v) (declare (ignore id)) (cons k v))
                           'm-person 'mt-tail :graph g2 :collect-p t))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test lazy-restore-plus-journal-tail
  "The LAZY (fault-on-access) crash-recovery path: a stale native-image checkpoint
plus a journal tail authored after it -- the device's real crash path (checkpoint
after peer-sync, author record-finds that journal, killed before the next
checkpoint).  On open the image's 10 nodes restore as LZNODE blobs (unmaterialized)
while recover-transactions replays the 5 journaled nodes as LIVE nodes; the two
coexist in the table and the reduce view reflects BOTH the image aggregate and the
replayed tail."
  (reset-mem-view-registry)
  (flet ((n-materialized (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::node-p v)))
         (n-lznodes (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::lznode-p v))))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)))
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
          (let ((*graph* g))
            (def-view lt-tail :lessp (m-person :graph-db-memory-test)
              (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
              (:reduce (lambda (keys values &optional r)
                         (declare (ignore keys r)) (reduce #'+ values))))
            (with-transaction () (dotimes (i 10) (make-m-person :name "a" :age 25)))
            (graph-db::checkpoint-memory-graph g)            ; native image: decade 2 x10
            (with-transaction () (dotimes (i 5) (make-m-person :name "b" :age 35)))
            ;; simulated crash: no close -- the 5 decade-3 nodes are journaled only
            ))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
          (unwind-protect
               (let ((*graph* g2))
                 (is (= 15 (graph-db::mem-table-count (graph-db::vertex-table g2))))
                 ;; mixed state: the 10 image nodes are still blobs; the 5 replayed
                 ;; journal-tail nodes were applied live by recover-transactions.
                 (is (= 10 (n-lznodes g2)))
                 (is (= 5 (n-materialized g2)))
                 (is (equal '((2 . 10) (3 . 5))
                            (map-reduced-view
                             (lambda (k id v) (declare (ignore id)) (cons k v))
                             'm-person 'lt-tail :graph g2 :collect-p t))))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test reduce-view-remove-re-reduces
  "Deleting a node from a map-reduce view re-reduces the aggregate via
GET-NON-AGGREGATE-PAIRS / GET-ALL-AGGREGATE-PAIRS on the mem-skip-list -- the
maintenance path the on-device eo-find rollup views hit during peer-sync (adding
never calls them, so the earlier map-reduce test missed this)."
  (with-test-memory-graph (g)
    (declare (ignorable g))
    (def-view m-cnt :lessp (m-person :graph-db-memory-test)
      (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
      (:reduce (lambda (keys values &optional rereduce)
                 (declare (ignore keys rereduce))
                 (reduce #'+ values))))
    (let (ids)
      (with-transaction ()
        (setq ids (list (id (make-m-person :name "a" :age 30))
                        (id (make-m-person :name "b" :age 31))
                        (id (make-m-person :name "c" :age 32)))))
      ;; decade 3 aggregate = 3
      (is (equal '((3 . 3))
                 (map-reduced-view (lambda (k id v) (declare (ignore id)) (cons k v))
                                   'm-person 'm-cnt :graph g :collect-p t)))
      ;; delete one -> remove-from-view -> re-reduce over the remaining pairs
      (with-transaction () (mark-deleted (lookup-vertex (first ids))))
      (is (equal '((3 . 2))
                 (map-reduced-view (lambda (k id v) (declare (ignore id)) (cons k v))
                                   'm-person 'm-cnt :graph g :collect-p t))))))

;;; --- lazy (fault-on-access) --------------------------------------------------

(test lazy-open-defers-materialization
  "Fault-on-access (:LAZY t): OPEN builds NO live nodes -- the vertex table holds
LZNODE blobs, materialized to live nodes only on first touch (open pays no
MAKE-INSTANCE, ~85% of eager open on ECL, #50).  A reduce (aggregate) view is
answered WITHOUT materializing any node.  Asserts table STATE directly (robust to
compiler inlining of the materializer)."
  (reset-mem-view-registry)
  (flet ((all-lznodes-p (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 always (graph-db::lznode-p v)))
         (n-materialized (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::node-p v))))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)) last-id)
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
          (let ((*graph* g))
            (def-view lz-decade :lessp (m-person :graph-db-memory-test)
              (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
              (:reduce (lambda (keys values &optional r)
                         (declare (ignore keys r)) (reduce #'+ values))))
            (with-transaction ()
              (dotimes (i 20)
                (setq last-id (id (make-m-person :name (format nil "p~D" i) :age (+ 20 i)))))))
          (close-graph g :snapshot-p t))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
          (unwind-protect
               (let ((*graph* g2))
                 (is-true (all-lznodes-p g2))   ; open built no live node
                 (is (= 20 (graph-db::mem-table-count (graph-db::vertex-table g2))))
                 (is (equal '((2 . 10) (3 . 10))
                            (map-reduced-view
                             (lambda (k id v) (declare (ignore id)) (cons k v))
                             'm-person 'lz-decade :graph g2 :collect-p t)))
                 (is-true (all-lznodes-p g2))   ; aggregate materialized no node
                 (let ((v (lookup-vertex last-id)))
                   (is-true (graph-db::node-p v))                 ; materialized on touch
                   (is-true (graph-db::node-p                     ; and swapped into the table
                             (graph-db::mem-table-get (graph-db::vertex-table g2) last-id)))
                   (is (= 1 (n-materialized g2)))                 ; exactly one built
                   (is (string= "p19" (slot-value v 'name)))))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test lazy-roundtrip-all-features
  "A :LAZY graph round-trips CRUD, edge adjacency, spatial and a reduce view through
the VG-native image; every query returns correct data with nodes materializing on
access."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)) a b)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
        (let ((*graph* g))
          (def-view lz-cnt :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
            (:reduce (lambda (keys values &optional r)
                       (declare (ignore keys r)) (reduce #'+ values))))
          (with-transaction ()
            (setq a (id (make-m-person :name "Ann" :age 30))
                  b (id (make-m-person :name "Bo"  :age 31)))
            (make-m-knows :from a :to b))
          (with-transaction ()
            (make-m-place :label "site" :geom (graph-db::make-point 36.3d0 50.0d0))))
        (close-graph g :snapshot-p t))
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
        (unwind-protect
             (let ((*graph* g2))
               ;; vertex data materializes correctly on lookup
               (is (string= "Ann" (slot-value (lookup-vertex a) 'name)))
               ;; edge adjacency (ve-index restored; endpoints materialize on access)
               (is (= 1 (length (outgoing-edges (lookup-vertex a)))))
               (is (= 1 (length (incoming-edges (lookup-vertex b)))))
               ;; spatial bbox (restored structurally; hit id -> materialize)
               (is (= 1 (length (graph-db::spatial-index-query-bbox
                                 (graph-db::spatial-index-for g2 'm-place 'geom) 22.0d0 48.0d0 40.0d0 51.0d0))))
               ;; reduce view (decade 3 = Ann + Bo = 2), no node materialization
               (is (equal '((3 . 2))
                          (map-reduced-view (lambda (k id v) (declare (ignore id)) (cons k v))
                                            'm-person 'lz-cnt :graph g2 :collect-p t))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; --- declarative def-view (#49) on the memory backend ---------------------
;;; (*view49-regens* + the regenerate-view :after counter are defined in
;;; view-tests.lisp, which loads before this file in the graph-db/test system.)

(test def-view-memory-restart-is-o1
  "Issue #49 on the in-memory (eager) backend: reopening does NOT rebuild an unchanged
view -- the restored index is kept (O(1)) and the reduce view is correct."
  (reset-mem-view-registry)
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (def-view m49-dec :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
            (:reduce (lambda (keys vals &optional r)
                       (declare (ignore keys r)) (reduce #'+ vals))))
          (with-transaction () (dotimes (i 20) (make-m-person :name "x" :age (+ 20 i)))))
        (close-graph g :snapshot-p t))
      (let ((*view49-regens* (list 0)))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
          (unwind-protect
               (progn
                 (is (= 0 (car *view49-regens*))
                     "memory restart keeps the view (no rebuild)")
                 (is (equal '((2 . 10) (3 . 10))
                            (map-reduced-view
                             (lambda (k id v) (declare (ignore id)) (cons k v))
                             'm-person 'm49-dec :graph g2 :collect-p t))))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test def-view-memory-lazy-restart-no-rebuild
  "Issue #49 x fault-on-access: reopening a LAZY memory graph keeps the view WITHOUT
rebuilding -- proven by the nodes staying LZNODE blobs (a rebuild would scan and thus
materialize them), while the reduce view still answers correctly."
  (reset-mem-view-registry)
  (flet ((all-lznodes-p (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 always (graph-db::lznode-p v))))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)))
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
          (let ((*graph* g))
            (def-view lz49-dec :lessp (m-person :graph-db-memory-test)
              (:map (lambda (p) (yield (floor (slot-value p 'age) 10) 1)))
              (:reduce (lambda (keys vals &optional r)
                         (declare (ignore keys r)) (reduce #'+ vals))))
            (with-transaction () (dotimes (i 20) (make-m-person :name "x" :age (+ 20 i)))))
          (close-graph g :snapshot-p t))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
          (unwind-protect
               (progn
                 (is (equal '((2 . 10) (3 . 10))
                            (map-reduced-view
                             (lambda (k id v) (declare (ignore id)) (cons k v))
                             'm-person 'lz49-dec :graph g2 :collect-p t))
                     "lazy reduce view is correct after restart")
                 (is-true (all-lznodes-p g2)
                          "install-views did NOT rebuild: nodes remain unmaterialized"))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

;;; --- v5 -> v6 native-image migration (GH #65) --------------------------------
;;;
;;; #65's premise was that a version-mismatched image is safe to discard because
;;; the transaction journal can rebuild it -- false for a memory graph: a clean
;;; close writes the image and THEN clears the journal, so after that the image is
;;; the ONLY durable copy.  The real fix is migrating v5 (3.0's pre-per-(owner .
;;; slot) spatial format) forward on read.  %WRITE-V5-TEST-IMAGE below is the
;;; ACTUAL v5 writer, recovered from 714eb3ba^ (before the per-index spatial
;;; section existed), reusing every codec it shares with today's writer -- NI-NODE
;;; / NI-INDEX / NI-PAIRS / NI-VIEWS / NI-VAL are byte-identical between v5 and v6,
;;; only the version number and the spatial section differ -- so the test below
;;; exercises the real historical byte layout, not a hand-authored guess at it.

(defun %write-v5-test-image (graph)
  "Write GRAPH's current state as a v5 native image -- a v5 CHECKPOINT, exactly like
CHECKPOINT-MEMORY-GRAPH/CLOSE-GRAPH's clean-close path: write the image, THEN clear
the retained journal, so a subsequent OPEN-MEMORY-GRAPH's RECOVER-TRANSACTIONS has
nothing to replay on top of the restored (still-LZNODE, on a lazy graph) state.
Skipping the journal clear would replay the original CREATEs and materialize every
node regardless of the spatial migration's own selectivity, confusing what the test
is actually checking.

The image itself is what WRITE-MEMORY-IMAGE-NATIVE wrote before 714eb3ba: its
spatial section is a single, unconditionally-empty flat pair list (v5 restore
rebuilt the spatial indexes from the nodes instead of reading it).  Everything else
is the same call sequence as today's writer."
  (let ((buf (graph-db::ni-mkbuf)))
    (graph-db::ni-bytes buf graph-db::*native-image-magic*)
    (graph-db::ni-uint buf 5 4)
    (graph-db::ni-uint buf (graph-db::load-highest-transaction-id graph) 8)
    (let ((vt (graph-db::mem-table-data (graph-db::vertex-table graph)))
          (et (graph-db::mem-table-data (graph-db::edge-table graph))))
      (graph-db::ni-uint buf (hash-table-count vt) 4)
      (maphash (lambda (id x) (graph-db::ni-node buf id x nil)) vt)
      (graph-db::ni-uint buf (hash-table-count et) 4)
      (maphash (lambda (id x) (graph-db::ni-node buf id x t)) et))
    (graph-db::ni-index buf (graph-db::%dump-mem-index
                              (graph-db::mem-type-index-data (graph-db::vertex-index graph)))
                         #'graph-db::ni-key-type)
    (graph-db::ni-index buf (graph-db::%dump-mem-index
                              (graph-db::mem-type-index-data (graph-db::edge-index graph)))
                         #'graph-db::ni-key-type)
    (graph-db::ni-index buf (graph-db::%dump-mem-index
                              (graph-db::mem-ve-index-data (graph-db::ve-index-in graph)))
                         #'graph-db::ni-key-ve)
    (graph-db::ni-index buf (graph-db::%dump-mem-index
                              (graph-db::mem-ve-index-data (graph-db::ve-index-out graph)))
                         #'graph-db::ni-key-ve)
    (graph-db::ni-index buf (graph-db::%dump-mem-index
                              (graph-db::mem-vev-index-data (graph-db::vev-index graph)))
                         #'graph-db::ni-key-vev)
    (graph-db::ni-pairs buf '())          ; v5's flat spatial section: always empty
    (graph-db::ni-views buf graph)
    (graph-db::ni-val buf (graph-db::%dump-unique-indexes graph))
    (with-open-file (s (graph-db::memory-image-file (graph-db::location graph))
                       :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede :if-does-not-exist :create)
      (write-sequence buf s))
    (graph-db::clear-memory-journal graph)))

(test v5-memory-image-migrates-on-open
  "A real v5 native image -- nodes, an indexed scalar slot, two distinct spatial
classes, a map view and a unique constraint -- restores fully on the current
(v6-writing) build.  The spatial section v5 couldn't carry comes back by
rebuilding from the just-restored nodes; everything else (the part that is NOT
derivable, and whose loss is unrecoverable) comes through the wire untouched."
  (reset-mem-view-registry)
  (with-temp-directory (dir)
    (let ((loc (namestring dir))
          place-id region-id tag-id person-id)
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (let ((*graph* g))
          (def-view m65-by-name :lessp (m-person :graph-db-memory-test)
            (:map (lambda (p) (yield (slot-value p 'name) (slot-value p 'age)))))
          (with-transaction ()
            (setq person-id (id (make-m-person :name "alice" :age 30)))
            (make-m-person :name "bob" :age 40)
            (setq tag-id (id (make-m-tag :label "urgent")))
            (setq place-id (id (make-m-place :label "kharkiv"
                                             :geom (graph-db::make-point 36.3d0 50.0d0))))
            (setq region-id (id (make-m-region
                                 :label "east"
                                 :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))
            (make-m-widget :sku "SKU-1")
            (make-m-widget :sku "SKU-2")))
        ;; Write the REAL v5 layout, then close WITHOUT a checkpoint so the clean
        ;; close doesn't overwrite it with today's v6 format.
        (%write-v5-test-image g)
        (close-graph g :snapshot-p nil))
      (is-true (graph-db::%native-image-p (graph-db::memory-image-file loc))
               "the file on disk really is a v5/v6-shaped native image")
      (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc)))
        (unwind-protect
             (let ((*graph* g2))
               (is (= 7 (graph-db::mem-table-count (graph-db::vertex-table g2)))
                   "all 7 vertices survived")
               (is (string= "alice" (slot-value (lookup-vertex person-id) 'name)))
               ;; indexed scalar slot (m-tag) survived
               (is (string= "urgent" (slot-value (lookup-vertex tag-id) 'label)))
               ;; spatial: not on v5's wire -- rebuilt from nodes, still two
               ;; distinct per-class indexes, both queryable
               (let ((place-ix  (graph-db::spatial-index-for g2 'm-place 'geom))
                     (region-ix (graph-db::spatial-index-for g2 'm-region 'extent)))
                 (is (not (eq place-ix region-ix))
                     "the two classes still keep their own indexes post-migration")
                 (is (has-p place-id
                            (graph-db::spatial-index-query-bbox
                             place-ix 36.0d0 49.9d0 36.5d0 50.1d0)))
                 (is (has-p region-id
                            (graph-db::spatial-index-query-bbox
                             region-ix 30.0d0 48.0d0 31.0d0 49.0d0))))
               ;; view
               (is (equal '("alice" "bob")
                          (map-view (lambda (k id v) (declare (ignore id v)) k)
                                    'm-person 'm65-by-name :graph g2 :collect-p t)))
               ;; unique constraint: rejects a duplicate SKU post-migration
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-m-widget :sku "SKU-1"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test v5-memory-image-lazy-open-migrates-selectively
  "A LAZY v5 image migrates too -- v5 itself proved this is safe: its own deleted
%REBUILD-MEMORY-SPATIAL-INDEXES touched an LZNODE only when its class MIGHT carry a
geometry, via NODE-GEOMETRY-INDEX-SLOTS -- which returns every :INDEX-marked slot,
scalars included, so a class like M-TAG (an indexed STRING) is over-approximated
and materialized too, same as v5 always did.  Plain M-PERSON, with no :INDEX slot
at all, is the one guaranteed to stay a deferred LZNODE blob -- the property
fault-on-access depends on.  A test that only checked the query would pass even if
migration materialized everything; this also pins down what did and didn't get
touched (GH #65)."
  (reset-mem-view-registry)
  (flet ((n-materialized (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::node-p v)))
         (n-lznodes (g)
           (loop for v being the hash-values of
                 (graph-db::mem-table-data (graph-db::vertex-table g))
                 count (graph-db::lznode-p v))))
    (with-temp-directory (dir)
      (let ((loc (namestring dir)) place-id region-id)
        (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc :lazy t)))
          (let ((*graph* g))
            (with-transaction ()
              (dotimes (i 5) (make-m-tag :label (format nil "t~D" i)))
              (dotimes (i 3) (make-m-person :name (format nil "p~D" i) :age (+ 20 i)))
              (setq place-id (id (make-m-place :label "kharkiv"
                                               :geom (graph-db::make-point 36.3d0 50.0d0))))
              (setq region-id (id (make-m-region
                                   :label "east"
                                   :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))))
          (%write-v5-test-image g)
          (close-graph g :snapshot-p nil))
        (let ((g2 (graph-db::open-memory-graph *mem-test-graph-name* loc :lazy t)))
          (unwind-protect
               (progn
                 (is (= 10 (graph-db::mem-table-count (graph-db::vertex-table g2))))
                 ;; spatial: migrated and queryable, even lazily
                 (is (has-p place-id
                            (graph-db::spatial-index-query-bbox
                             (graph-db::spatial-index-for g2 'm-place 'geom)
                             36.0d0 49.9d0 36.5d0 50.1d0)))
                 (is (has-p region-id
                            (graph-db::spatial-index-query-bbox
                             (graph-db::spatial-index-for g2 'm-region 'extent)
                             30.0d0 48.0d0 31.0d0 49.0d0)))
                 ;; the 3 plain M-PERSON nodes (no :INDEX slot at all) are untouched
                 (is (= 3 (n-lznodes g2))
                     "nodes with no :INDEX slot were never touched by the migration")
                 ;; the 2 geometry nodes AND the 5 M-TAG (:INDEX scalar,
                 ;; over-approximated, same as v5 always did) were materialized
                 (is (= 7 (n-materialized g2))
                     "geometry nodes plus the over-approximated :INDEX-scalar class"))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

(test unsupported-memory-image-version-message-is-corrected
  "The version-mismatch error names the actual remedy -- open with a build that
reads the image's version, or migrate it -- instead of the old advice to delete
the image and 'rebuild from the journal': after a clean close that journal is
empty, and deleting the image discards the ONLY durable copy of the graph
(GH #65)."
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (let ((g (graph-db::make-memory-graph *mem-test-graph-name* loc)))
        (close-graph g :snapshot-p nil))
      ;; Hand-craft a bogus-version image: magic + version 99 (LE u32).  The
      ;; version check fires before anything else is read, so no body is needed.
      (with-open-file (s (graph-db::memory-image-file loc) :direction :output
                         :element-type '(unsigned-byte 8) :if-exists :supersede
                         :if-does-not-exist :create)
        (write-sequence graph-db::*native-image-magic* s)
        (write-byte 99 s) (write-byte 0 s) (write-byte 0 s) (write-byte 0 s))
      (let ((msg (handler-case
                     (progn (graph-db::open-memory-graph *mem-test-graph-name* loc) nil)
                   (error (e) (princ-to-string e)))))
        (is-true msg "opening an unknown-version image signals an error")
        (when msg
          (is (search "v5" msg))
          (is (search "v6" msg))
          (is (not (search "Delete" msg)))
          (is (not (search "delete" msg))))))))
