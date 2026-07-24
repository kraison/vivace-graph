;;;; Per-(owner . slot) spatial index registry and scoped queries.

(in-package #:graph-db/test)

;; Two spatially-indexed classes with NO common geometry-declaring ancestor, so
;; they land in separate indexes.  ZONE's polygon contains every PROBE point --
;; the discriminating case from the change request.
(def-vertex scope-probe ()
  ((geom :type geometry :index t))
  :graph-db-integration-test)

(def-vertex scope-zone ()
  ((extent :type geometry :index t))
  :graph-db-integration-test)

(def-suite spatial-scope-suite
  :description "Per-class spatial indexes: registry, scoping, declaration."
  :in graph-db-suite)

(in-suite spatial-scope-suite)

;; %MAKE-GEOMETRY is internal to GRAPH-DB (not exported, not imported here).
(defun scope-rect (min-lon min-lat max-lon max-lat)
  (graph-db::%make-geometry
   :kind :polygon
   :coordinates (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(test registry-separates-declaring-classes
  "Two classes declaring their own geometry slot get two distinct indexes."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-ix (spatial-index-for g 'scope-probe 'geom))
          (zone-ix  (spatial-index-for g 'scope-zone 'extent)))
      (is (spatial-index-p probe-ix))
      (is (spatial-index-p zone-ix))
      (is (not (eq probe-ix zone-ix))))))

(test node-geometry-reports-its-slot
  "NODE-GEOMETRY returns the geometry AND the slot it came from."
  (with-test-graph (g)
    (declare (ignore g))
    (let (node)
      (with-transaction ()
        (setq node (make-scope-zone :extent (scope-rect 0d0 0d0 1d0 1d0))))
      (multiple-value-bind (geom slot) (node-geometry node)
        (is (geometryp geom))
        (is (eq slot 'extent))))))

(test unindexed-geometry-slot-creates-no-index
  "A slot that never holds a geometry never creates an index (§4.1)."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (is (null (spatial-index-for g 'scope-zone 'extent)))))

;;; ---------------------------------------------------------------------------
;;; v3 sidecar: roots persist, reopen is address-based (no node scan, no leak).
;;; ---------------------------------------------------------------------------

(test roots-survive-a-clean-reopen
  "A clean close persists every index root; reopen finds the same nodes with no
node scan."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) zone-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq zone-id (id (make-scope-zone
                               :extent (scope-rect 0d0 0d0 1d0 1d0)))))
          (close-graph g :snapshot-p nil)))
      (is (probe-file (graph-db::spatial-indexes-root-file path))
          "CLOSE-GRAPH wrote the v3 sidecar")
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-zone 'extent)))
               (is (spatial-index-p idx))
               (is (has-p zone-id
                          (spatial-index-query-bbox idx 0.1d0 0.1d0 0.2d0 0.2d0))))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test reopen-does-not-grow-the-index-heap
  "The whole point of the sidecar: OPEN-GRAPH REOPENS the ordered maps by address
instead of allocating fresh ones and orphaning the previous run's.  GC-HEAP only
ever sweeps heap.dat, so an orphan in indexes.dat is unreclaimable -- the leak
shows up as the indexes heap's bump pointer climbing on every open."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (marks '()))
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (dotimes (i 200)
              (make-scope-probe :geom (make-point (+ 37d0 (* i 1d-3))
                                                  (+ 49d0 (* i 1d-3))))
              (make-scope-zone :extent (scope-rect (+ 20d0 (* i 1d-3))
                                                   (+ 44d0 (* i 1d-3))
                                                   (+ 20.01d0 (* i 1d-3))
                                                   (+ 44.01d0 (* i 1d-3))))))
          (close-graph g :snapshot-p nil)))
      (dotimes (i 3)
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (push (graph-db::memory-pointer (graph-db::indexes g)) marks)
            (close-graph g :snapshot-p nil)
            (collect-garbage))))
      (destructuring-bind (third second first) marks
        (is (= first second)
            "indexes.dat high-water grew between open 1 and 2: ~D -> ~D"
            first second)
        (is (= second third)
            "indexes.dat high-water grew between open 2 and 3: ~D -> ~D"
            second third)))))

(test pre-v3-sidecar-migrates-on-open
  "A pre-v3 graph -- spatial-index.root present, spatial-indexes.dat absent --
re-derives its per-(owner . slot) indexes from live node geometries at open, and
writes the v3 sidecar so the NEXT open is address-based.  Index only: the nodes
are untouched.  The old file is left in place, not renamed."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      ;; Fabricate the pre-v3 on-disk shape: no v3 sidecar, an old single-index
      ;; root file in its place.  (Its address is never read on this path -- the
      ;; file's mere presence is the migration signal.)
      (delete-file (graph-db::spatial-indexes-root-file path))
      (cl-store:store (list :format 2 :address 0 :precision 7 :backend :skip-list)
                      (graph-db::spatial-index-root-file path))
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-probe 'geom)))
               (is (spatial-index-p idx) "the index was re-derived from the nodes")
               (is (has-p probe-id
                          (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                    37.19d0 49.21d0)))
               (is (probe-file (graph-db::spatial-indexes-root-file path))
                   "migration wrote the v3 sidecar")
               (is (probe-file (graph-db::spatial-index-root-file path))
                   "the pre-v3 file is left in place, not renamed"))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test torn-sidecar-falls-back-to-a-rebuild
  "A truncated sidecar re-derives the indexes instead of refusing to open.

This one is written from inside a commit (index creation, coarsening), not only at
CLOSE-GRAPH, so a crash can tear it -- and the nodes it is derived from are still
entirely intact."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      ;; Truncate the sidecar mid-record, as an interrupted write would.
      (let* ((file (graph-db::spatial-indexes-root-file path))
             (bytes (with-open-file (in file :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                        (read-sequence b in)
                        b))))
        (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence bytes out :end (floor (length bytes) 2))))
      (handler-bind ((warning #'muffle-warning))    ; the torn-sidecar warning
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (let ((idx (spatial-index-for g 'scope-probe 'geom)))
                 (is (spatial-index-p idx) "the index was re-derived from the nodes")
                 (is (has-p probe-id
                            (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                      37.19d0 49.21d0))))
            (close-graph g :snapshot-p nil)
            (collect-garbage)))))))

(test degraded-clamp-survives-a-reopen
  "The sidecar carries the PRECISION HISTOGRAM, not just the root address.

A geometry too large to cover within +SPATIAL-INSERT-MAX-CELLS+ is stored at a
coarser precision, and the query's covering clamp is lowered to match so it stays
findable.  That clamp is derived from the histogram, which lives only in RAM
between closes -- so a sidecar that persisted the address alone would reopen with
the clamp back at the configured precision, and a prefix range scan would sort
PAST the coarser stored key: a silent miss on an index that is physically intact."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) big-id)
      (handler-bind ((warning #'muffle-warning))    ; coarsening is EXPECTED here
        (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              ;; ~1 degree square: far more than 16384 cells at precision 7, so
              ;; the insert cover is capped and stored coarsely.
              (setq big-id (id (make-scope-zone
                                :extent (scope-rect 10d0 40d0 11d0 41d0)))))
            (let ((idx (spatial-index-for g 'scope-zone 'extent)))
              (is (< (spatial-index-coarsest-precision idx)
                     (spatial-index-precision idx))
                  "the oversized polygon really did coarsen the index"))
            (close-graph g :snapshot-p nil)))
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (let ((idx (spatial-index-for g 'scope-zone 'extent)))
                 (is (< (spatial-index-coarsest-precision idx)
                        (spatial-index-precision idx))
                     "the clamp came back from the sidecar, not reset to the ~
                      configured precision")
                 (is (has-p big-id
                            (spatial-index-query-bbox idx 10.4d0 40.4d0
                                                      10.6d0 40.6d0))
                     "a small window inside the coarsely-stored polygon still ~
                      finds it after the reopen"))
            (close-graph g :snapshot-p nil)
            (collect-garbage)))))))

(test regenerate-one-index-leaves-the-others
  "REGENERATE-SPATIAL-INDEX rebuilds exactly one (owner . slot) index."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-before (spatial-index-for g 'scope-probe 'geom)))
      (regenerate-spatial-index g 'scope-zone 'extent)
      ;; The untouched index is the SAME struct; the regenerated one is fresh.
      (is (eq probe-before (spatial-index-for g 'scope-probe 'geom)))
      (is (spatial-index-p (spatial-index-for g 'scope-zone 'extent))))))

(test regenerate-one-index-reindexes-its-own-nodes
  "The regenerated index still answers for every live node of its owner class."
  (with-test-graph (g)
    (let (zone-id)
      (with-transaction ()
        (setq zone-id (id (make-scope-zone
                           :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))))
      (is (= 1 (regenerate-spatial-index g 'scope-zone 'extent)))
      (let ((idx (spatial-index-for g 'scope-zone 'extent)))
        (is (has-p zone-id
                   (spatial-index-query-bbox idx 30d0 48d0 31d0 49d0)))))))

(test regenerate-all-spatial-indexes-persists-roots
  "REGENERATE-SPATIAL-INDEXES rebuilds every index and re-saves the sidecar, so a
reopen sees the NEW roots rather than the freed ones."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (is (eq g (regenerate-spatial-indexes g)))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-probe 'geom)))
               (is (spatial-index-p idx))
               (is (has-p probe-id
                          (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                    37.19d0 49.21d0))))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))
