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
