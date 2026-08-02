;;;; Tests for the geohash spatial index (spatial-index.lisp).

(in-package #:graph-db/test)

(def-suite spatial-index-suite
  :description "Geohash spatial index insert/query/remove/persistence."
  :in graph-db-suite)

(in-suite spatial-index-suite)

;; Node ids are 16-byte (unsigned-byte 8) arrays (uuids); the index stores them
;; as opaque bytes.  BID makes a distinct stand-in id; HAS-P tests membership
;; with EQUALP (byte-array equality).
(defun bid (n)
  (let ((a (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref a 0) n)
    a))

(defun has-p (id candidates)
  (member id candidates :test 'equalp))

;;; Coordinates from the demining dataset (Kharkiv Oblast EO finds).
(defparameter *eo-a* '(37.1724312d0 49.2020584d0))   ; lon lat
(defparameter *eo-b* '(37.1773283d0 49.2036314d0))   ; ~400 m from A
(defparameter *far*  '(23.7182919d0 50.0263233d0))   ; Lviv Oblast, ~1000 km

(defun pt (lonlat) (make-point (first lonlat) (second lonlat)))

(test insert-and-query-bbox
  "A window over the AO returns the local finds as candidates, not distant ones."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx (bid 1) (pt *eo-a*))
      (spatial-index-insert idx (bid 2) (pt *eo-b*))
      (spatial-index-insert idx (bid 3) (pt *far*))
      (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
        (is (has-p (bid 1) cands))
        (is (has-p (bid 2) cands))
        (is (not (has-p (bid 3) cands)))))))

(test query-radius-and-refine
  "Radius query is a prefilter; geodesic-distance gives the exact answer."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 9))
          (coords (list (cons (bid 1) *eo-a*) (cons (bid 2) *eo-b*) (cons (bid 3) *far*))))
      (loop for (id . c) in coords do (spatial-index-insert idx id (pt c)))
      (let* ((lat (second *eo-a*)) (lon (first *eo-a*))
             (cands (spatial-index-query-radius idx lat lon 600d0))
             (within (remove-if-not
                      (lambda (id)
                        (let ((c (cdr (assoc id coords :test 'equalp))))
                          (<= (geodesic-distance lat lon (second c) (first c)) 600d0)))
                      cands)))
        (is (has-p (bid 1) cands) "self must be a candidate")
        (is (not (has-p (bid 3) cands)) "distant point filtered by bbox")
        (is (has-p (bid 1) within))
        (is (has-p (bid 2) within))
        (is (not (has-p (bid 3) within)))))))

(test polygon-occupies-cells
  "A task-area polygon is a candidate for windows that overlap it, not others."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (aoi (make-polygon '(((37.170 49.200) (37.180 49.200)
                                (37.180 49.206) (37.170 49.206)
                                (37.170 49.200))))))
      (spatial-index-insert idx (bid 10) aoi)
      (is (has-p (bid 10) (spatial-index-query-bbox idx 37.172d0 49.201d0 37.174d0 49.203d0)))
      (is (not (has-p (bid 10) (spatial-index-query-bbox idx 23.70d0 50.00d0 23.75d0 50.05d0)))))))

(test multipolygon-spans-parts
  "Both parts of a multipolygon are reachable; a gap between them is not."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (mp (make-multipolygon '((((37.10 49.10) (37.11 49.10) (37.11 49.11) (37.10 49.11) (37.10 49.10)))
                                   (((37.50 49.50) (37.51 49.50) (37.51 49.51) (37.50 49.51) (37.50 49.50)))))))
      (spatial-index-insert idx (bid 7) mp)
      (is (has-p (bid 7) (spatial-index-query-bbox idx 37.10d0 49.10d0 37.11d0 49.11d0)))   ; part A
      (is (has-p (bid 7) (spatial-index-query-bbox idx 37.50d0 49.50d0 37.51d0 49.51d0)))   ; part B
      (is (not (has-p (bid 7) (spatial-index-query-bbox idx 37.30d0 49.30d0 37.31d0 49.31d0))))))) ; gap

(test large-bbox-does-not-blow-up
  "REGRESSION: a continent-sized query window must not enumerate (and cons) the
fixed-precision grid until the heap is exhausted.  Before the prefix-range-scan
fix, querying a precision-7 index with a whole-country bbox (~19 x 9 degrees) made
GEOHASH-COVERING emit ~10^8 cells and killed the process.  The window is covered
with a bounded set of coarse cells now, so this returns promptly with the right
candidates: points inside the window present, a point well outside absent."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (kharkiv '(37.1724312d0 49.2020584d0))   ; lon lat, in UA
          (lviv    '(23.7182919d0 50.0263233d0))   ; in UA
          (london  '(-0.1276d0 51.5072d0)))         ; well outside UA
      (spatial-index-insert idx (bid 1) (pt kharkiv))
      (spatial-index-insert idx (bid 2) (pt lviv))
      (spatial-index-insert idx (bid 3) (pt london))
      ;; whole-of-Ukraine window -- the exact shape that used to OOM
      (let ((cands (spatial-index-query-bbox idx 22d0 44d0 41d0 53d0)))
        (is (has-p (bid 1) cands) "Kharkiv point inside the window")
        (is (has-p (bid 2) cands) "Lviv point inside the window")
        (is (not (has-p (bid 3) cands)) "London is outside the window")))))

(test large-bbox-covering-precision-is-bounded
  "The covering chosen for a huge window stays coarse (so the cell count is
bounded), while a tiny window still resolves to the index's full precision."
  ;; +spatial-query-max-cells+ and %covering-precision are internal tuning knobs
  ;; (not part of the public spatial API), so reach them with graph-db:: here.
  (let ((max-cells graph-db::+spatial-query-max-cells+))
    ;; huge window -> coarse covering, bounded cell count
    (is (<= (length (geohash-covering 22d0 44d0 41d0 53d0 :max-cells max-cells))
            max-cells))
    ;; the adaptive precision for a continent is far below storage precision 7
    (is (< (graph-db::%covering-precision 19d0 9d0 max-cells) 7))
    ;; a metre-scale window wants precision >= storage precision (clamped to 7)
    (is (>= (graph-db::%covering-precision 0.0001d0 0.0001d0 max-cells) 7))))

(test remove-clears-entries
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (g (pt *eo-a*)))
      (spatial-index-insert idx (bid 5) g)
      (is (has-p (bid 5) (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
      (spatial-index-remove idx (bid 5) g)
      (is (not (has-p (bid 5) (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))))))

(test remove-among-many-same-cell
  "Many nodes share ONE cell; removing arbitrary (including middle) ids removes
exactly those and leaves the rest.  Guards the duplicate-key defects the
composite (cell . id) key fixed: an O(n) from-the-head rescan on the on-disk
list, and a silent %MEM-FIND overshoot in RAM that dropped the wrong node (a
middle same-cell remove used to no-op).  Runs on BOTH backends."
  (labels ((exercise (idx)
             (let ((g (pt *eo-a*)) (n 30) (removed '(15 4 22 9 17 0 29)))
               (dotimes (i n) (spatial-index-insert idx (bid i) g))
               (is (= n (length (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
                   "all n distinct ids indexed under the shared cell")
               (dolist (i removed) (spatial-index-remove idx (bid i) g))
               (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
                 (is (= (- n (length removed)) (length cands))
                     "exactly the removed ids are gone")
                 (dolist (i removed)
                   (is (not (has-p (bid i) cands)) "a removed id must be absent"))
                 (dotimes (i n)
                   (unless (member i removed)
                     (is (has-p (bid i) cands) "a surviving id must remain")))))))
    (with-temp-memory (heap)                 ; on-disk backend
      (exercise (make-spatial-index heap :precision 7)))
    (exercise (graph-db::make-mem-spatial-index :precision 7))))  ; in-RAM backend

(test persistence-reopen-from-disk
  "An index reopened from its on-disk heap at its root address still answers."
  (with-temp-directory (dir)
    (let ((path (namestring (merge-pathnames "spx-heap.dat" dir)))
          (addr nil))
      (let* ((heap (create-memory path (* 1024 1024 16)))
             (idx (make-spatial-index heap :precision 7)))
        (unwind-protect
             (progn
               (spatial-index-insert idx (bid 1) (pt *eo-a*))
               (spatial-index-insert idx (bid 2) (pt *far*))
               (setf addr (spatial-index-address idx)))
          (close-memory heap)))
      (let ((heap (open-memory path)))
        (unwind-protect
             (let ((idx (open-spatial-index heap addr :precision 7)))
               (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
                 (is (has-p (bid 1) cands))
                 (is (not (has-p (bid 2) cands)))))
          (close-memory heap))))))

;;; --- §7: bounded insert cover + self-healing clamp -------------------------

(defun big-poly (min-lon min-lat max-lon max-lat)
  "An axis-aligned rectangle polygon, as one exterior ring (lon lat pairs)."
  (graph-db::%make-geometry
   :kind :polygon
   :coordinates (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(test insert-caps-oversized-cover
  "A ~18 x 8 degree polygon indexes in bounded time and space at p=7."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx (bid 1) (big-poly 22.1d0 44.4d0 40.2d0 52.4d0))
      ;; Uncapped this would enumerate ~7.7e7 cells and exhaust the heap.
      (is (<= (loop for p from 1 to 12
                    sum (aref (spatial-index-precision-counts idx) p))
              (spatial-index-max-cells idx)))
      ;; The cover was coarsened, so the clamp dropped below storage precision.
      (is (< (spatial-index-coarsest-precision idx) 7)))))

(test clamp-finds-coarse-and-fine-together
  "A small query inside a coarsely-stored polygon returns BOTH it and a
finely-stored point in the same index -- the mixed case a single-node test
would pass by accident."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx (bid 1) (big-poly 22.1d0 44.4d0 40.2d0 52.4d0))
      (spatial-index-insert idx (bid 2) (pt *eo-a*))
      (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
        (is (has-p (bid 1) cands))
        (is (has-p (bid 2) cands))))))

(test clamp-self-heals-on-remove
  "Deleting the oversized geometry restores the clamp with no rebuild."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (poly (big-poly 22.1d0 44.4d0 40.2d0 52.4d0)))
      (spatial-index-insert idx (bid 1) poly)
      (is (< (spatial-index-coarsest-precision idx) 7))
      (spatial-index-remove idx (bid 1) poly)
      (is (= (spatial-index-coarsest-precision idx) 7)))))

(test insert-remove-symmetry-under-coarsening
  "Remove computes the same cell set insert did, so nothing is orphaned."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (poly (big-poly 22.1d0 44.4d0 40.2d0 52.4d0)))
      (spatial-index-insert idx (bid 1) poly)
      (spatial-index-remove idx (bid 1) poly)
      (is (zerop (loop for p from 1 to 12
                       sum (aref (spatial-index-precision-counts idx) p))))
      (is (null (spatial-index-query-bbox idx 22.1d0 44.4d0 40.2d0 52.4d0))))))

(defun %speck-parts (n)
  "N tiny (0.001-degree) polygon parts, well separated, for a multipolygon whose
area is dominated by some other big part."
  (loop for i from 0 below n
        for x = (+ 20d0 (* i 0.5d0))
        collect (list (list (list x 50d0) (list (+ x 0.001d0) 50d0)
                            (list (+ x 0.001d0) 50.001d0) (list x 50.001d0)
                            (list x 50d0)))))

(defun %speck-cluster (n)
  "N tiny parts packed into a ~0.1-degree cluster -- an archipelago whose part
COUNT, not its extent, is what overruns a cell budget."
  (loop for i from 0 below n
        for x = (+ 37d0 (* i 0.001d0))
        collect (list (list (list x 49d0) (list (+ x 0.0005d0) 49d0)
                            (list (+ x 0.0005d0) 49.0005d0) (list x 49.0005d0)
                            (list x 49d0)))))

(test gh-103-sliver-part-keeps-the-index-precision
  "REGRESSION (GH #103): one sliver part in one multipolygon used to collapse
the WHOLE index's query precision to 1.  The old per-part budget was MAX-CELLS
split by bbox area and floored at 1, so a part under 1/MAX-CELLS of the total
floored to a one-cell budget -- and one cell for a real geometry means precision
1, which clamps every query on the index (SPATIAL-INDEX-QUERY-BBOX).  A mainland
plus an island is the shape that did it in the field, and nothing here is near
the cap: ~1,000 cells against 16,384."
  (with-temp-memory (heap)
    (let* ((idx (make-spatial-index heap :precision 5))
           (mainland '(((36d0 49d0) (37.3d0 49d0) (37.3d0 50.3d0)
                        (36d0 50.3d0) (36d0 49d0))))
           (island '(((37.5d0 49.5d0) (37.51d0 49.5d0) (37.51d0 49.51d0)
                      (37.5d0 49.51d0) (37.5d0 49.5d0))))
           (mp (make-multipolygon (list mainland island))))
      (spatial-index-insert idx (bid 1) mp)
      (is (zerop (aref (spatial-index-precision-counts idx) 1))
          "no cell was stored at precision 1")
      (is (= 5 (spatial-index-coarsest-precision idx))
          "queries still cover at the configured precision")
      ;; both parts remain reachable -- the sliver is indexed, just not coarsely
      (is (has-p (bid 1) (spatial-index-query-bbox
                          idx 36.5d0 49.5d0 36.6d0 49.6d0)))
      (is (has-p (bid 1) (spatial-index-query-bbox
                          idx 37.5d0 49.5d0 37.51d0 49.51d0))))))

(test multipolygon-coarsens-on-the-total-not-per-part
  "MAX-CELLS bounds a multipolygon's TOTAL cover, not each part's share: one
8x8-degree part plus nine 0.001-degree specks are covered at ONE precision, the
finest whose total fits.  The specks cost 4 cells each at any precision, so
they neither coarsen the big part nor get coarsened by it -- the big part gets
exactly the grid it would get alone."
  (let* ((max-cells 4096)
         (big '(((0d0 0d0) (8d0 0d0) (8d0 8d0) (0d0 8d0) (0d0 0d0))))
         (mp (make-multipolygon (cons big (%speck-parts 9))))
         (alone (make-multipolygon (list big)))
         (cells (graph-db::%geometry-cells mp 9 max-cells))
         (big-cells (graph-db::%geometry-cells alone 9 max-cells)))
    (is (= 1 (length (remove-duplicates (mapcar #'length cells))))
        "one precision across every part")
    (is (subsetp big-cells cells :test #'string=)
        "the specks did not coarsen the large part")
    (is (> (length (first cells)) 1) "not collapsed to precision 1")))

(test multipolygon-past-any-budget-covers-its-envelope-once
  "A multipolygon with more parts than MAX-CELLS can hold at ANY precision --
each part costs at least one cell however coarse the grid -- covers its whole
envelope ONCE rather than collapsing to precision 1.  Bounded by construction
and still far more selective than the coarsest grid; the price is that the gaps
between the parts are indexed, which the caller's exact predicate refines away."
  (let* ((max-cells 64)
         (mp (make-multipolygon (%speck-cluster 100)))
         (cells (graph-db::%geometry-cells mp 7 max-cells)))
    (is (<= (length cells) max-cells) "the cover is still bounded")
    (is (> (length (first cells)) 1) "not collapsed to precision 1")
    (is (null (set-exclusive-or cells (graph-db::%bbox-cells mp 7 max-cells)
                                :test #'string=))
        "the fall-back is exactly the single-envelope cover")))

(test single-part-geometry-cells-are-unchanged
  "A geometry with ONE bbox gets exactly %BBOX-CELLS' cover.  The GH #103 rework
must not move a cell for the kinds it never touched: that is what makes the
format bump a multipolygon-only concern, and what keeps SPATIAL-INDEX-REMOVE
symmetric for every already-indexed point, polygon and linestring."
  (let ((poly (make-polygon '(((22.1d0 44.4d0) (40.2d0 44.4d0) (40.2d0 52.4d0)
                               (22.1d0 52.4d0) (22.1d0 44.4d0))))))
    (dolist (max-cells '(16384 4096 64))
      (is (null (set-exclusive-or (graph-db::%geometry-cells poly 7 max-cells)
                                  (graph-db::%bbox-cells poly 7 max-cells)
                                  :test #'string=))))))

(test multipolygon-insert-remove-symmetry
  "REMOVE recomputes exactly the cells INSERT wrote for a multipolygon, so the
whole-geometry cell budget must be a pure deterministic function of the geometry
-- no residual entries after removal."
  (with-temp-memory (heap)
    (let* ((idx (make-spatial-index heap :precision 7))
           (big '(((0d0 0d0) (8d0 0d0) (8d0 8d0) (0d0 8d0) (0d0 0d0))))
           (mp (make-multipolygon (cons big (%speck-parts 9)))))
      (spatial-index-insert idx (bid 8) mp)
      (spatial-index-remove idx (bid 8) mp)
      (is (zerop (loop for p from 1 to 12
                       sum (aref (spatial-index-precision-counts idx) p)))
          "no orphaned cell entries after removing the multipolygon")
      (is (null (spatial-index-query-bbox idx 0d0 0d0 8d0 8d0))))))

(test double-remove-does-not-orphan-a-surviving-node
  "REGRESSION: removing an already-absent entry is a SUPPORTED no-op elsewhere in
this engine (APPLY-PEER-PURGE documents idempotent purge; RECOVER-TRANSACTIONS
re-applies every unmarked .txn on crash recovery even when the heap already
reflects it), so SPATIAL-INDEX-REMOVE must tolerate it too.  Before the fix, it
called %UNCOUNT-CELL unconditionally, so a double-remove decremented a
precision-counts bin regardless of whether anything was actually removed from
the store.

Two nodes are inserted under the IDENTICAL oversized polygon, so their entries
land in the exact same (coarsely-covered) precision bin with EXACTLY equal
counts -- both cover exactly the same cells, by construction.  Removing one of
them once is legitimate and must not disturb the other.  Removing it a SECOND
time touches nothing in the store, but a histogram that decrements anyway
drives that shared bin to zero -- exactly cancelling out the surviving node's
share -- and the coarsest-precision clamp self-heals to a level that is too
FINE for the surviving node's coarsely-stored cells.  SPATIAL-INDEX-QUERY-BBOX's
prefix range scan then walks past them: a small window well inside the
surviving node's footprint (same window and geometry as
CLAMP-FINDS-COARSE-AND-FINE-TOGETHER, which establishes that the clamp is what
makes this window find the oversized geometry at all) silently returns
nothing for it.  The existing INSERT-REMOVE-SYMMETRY-UNDER-COARSENING test
cannot catch this: %UNCOUNT-CELL floors its counts at zero, so
`(zerop (sum counts))` looks identical whether the counts are exactly right or
were driven to zero early by a spurious extra decrement.  Only a query-level
assertion -- can a node inserted earlier still be found -- exposes the bug.
Runs on BOTH backends."
  (labels ((exercise (idx)
             (let ((poly (big-poly 22.1d0 44.4d0 40.2d0 52.4d0)))
               (spatial-index-insert idx (bid 1) poly)  ; A -- must survive throughout
               (spatial-index-insert idx (bid 2) poly)  ; B -- identical geometry/cells
               (is (< (spatial-index-coarsest-precision idx) 7)
                   "the shared geometry is coarsely covered, not at full precision")
               (flet ((a-found-p ()
                        (has-p (bid 1)
                               (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0))))
                 (is (a-found-p) "A is findable before B is touched at all")
                 (spatial-index-remove idx (bid 2) poly)   ; legitimate remove of B
                 (is (a-found-p) "A survives B's real removal")
                 (spatial-index-remove idx (bid 2) poly)   ; DOUBLE remove of B: a no-op
                 (is (a-found-p)
                     "A must still be findable after B's double-remove -- A's own entries
were never touched by either call")))))
    (with-temp-memory (heap)                 ; on-disk backend
      (exercise (make-spatial-index heap :precision 7)))
    (exercise (graph-db::make-mem-spatial-index :precision 7))))
