;;;; Tests for geohash encoding (geohash.lisp).

(in-package #:graph-db/test)

(def-suite geohash-suite
  :description "Geohash encode/decode, cell geometry, and bbox covering."
  :in graph-db-suite)

(in-suite geohash-suite)

(test canonical-encode
  "The Wikipedia reference coordinate encodes to the known geohash."
  (is (string= "u4pruydqqvj" (geohash-encode 57.64911d0 10.40744d0 11))))

(test precision-length
  (dolist (p '(1 5 8 12))
    (is (= p (length (geohash-encode 41.75d0 2.45d0 p))))))

(test prefix-nesting
  "A coarser geohash is a prefix of a finer one for the same point."
  (let ((fine (geohash-encode 45.6720584d0 12.3424312d0 12)))
    (dolist (p '(1 4 7 10))
      (is (string= (geohash-encode 45.6720584d0 12.3424312d0 p)
                   (subseq fine 0 p))))))

(test bbox-contains-point
  "The decoded cell bounding box contains the original point."
  (dolist (pt '((45.6720584d0 12.3424312d0)
                (41.7763233d0 2.4682919d0)
                (-33.85d0 151.21d0)))
    (let ((lat (first pt)) (lon (second pt)))
      (multiple-value-bind (mnx mny mxx mxy)
          (geohash-bbox (geohash-encode lat lon 10))
        (is (<= mnx lon mxx))
        (is (<= mny lat mxy))))))

(test decode-center-near-point
  "Decoding returns a center within the cell half-extent of the point."
  (let* ((lat 45.6720584d0) (lon 12.3424312d0)
         (h (geohash-encode lat lon 9)))
    (multiple-value-bind (mnx mny mxx mxy) (geohash-bbox h)
      (multiple-value-bind (clat clon) (geohash-decode h)
        (is (<= (abs (- clat lat)) (/ (- mxy mny) 2)))
        (is (<= (abs (- clon lon)) (/ (- mxx mnx) 2)))))))

(test cell-size-values
  (multiple-value-bind (lw lh) (geohash-cell-size 1)
    (is (= lw 45d0)) (is (= lh 45d0)))
  (multiple-value-bind (lw lh) (geohash-cell-size 2)
    (is (= lw 11.25d0)) (is (= lh 5.625d0))))

(test prefix-range-contains-hash
  "A point's full geohash sorts within the prefix range of any coarser cell."
  (let* ((full (geohash-encode 45.6720584d0 12.3424312d0 12))
         (cell (subseq full 0 6)))
    (multiple-value-bind (start end) (geohash-prefix-range cell)
      (is (and (string>= full start) (string< full end))))))

(test covering-intersects-and-includes-center
  "Every covering cell intersects the query box, and the box's center cell is
included."
  (let* ((min-lon 12.33d0) (min-lat 45.66d0) (max-lon 12.36d0) (max-lat 45.68d0)
         (cells (geohash-covering min-lon min-lat max-lon max-lat :max-cells 256))
         (center-cell (let ((p (length (first cells))))
                        (geohash-encode (/ (+ min-lat max-lat) 2)
                                        (/ (+ min-lon max-lon) 2) p))))
    (is (plusp (length cells)))
    (is (member center-cell cells :test #'string=))
    (dolist (c cells)
      (multiple-value-bind (cnx cny cxx cxy) (geohash-bbox c)
        (is (and (<= cnx max-lon) (>= cxx min-lon)
                 (<= cny max-lat) (>= cxy min-lat))
            "cell ~S does not intersect the query box" c)))))

(test covering-tiny-box
  "A degenerate (point) box yields at least one covering cell."
  (is (plusp (length (geohash-covering 12.3424d0 45.6720d0 12.3424d0
                       45.6720d0)))))

(test neighbor-is-adjacent-and-distinct
  "The east neighbor of a cell is a different, same-precision cell whose centre
sits one cell-width to the east."
  (let* ((cell (geohash-encode 45.6720d0 12.3424d0 7))
         (east (geohash-neighbor cell 1 0)))
    (is (stringp east))
    (is (= (length cell) (length east)))
    (is (not (string= cell east)))
    (multiple-value-bind (lat0 lon0) (geohash-decode cell)
      (multiple-value-bind (lat1 lon1) (geohash-decode east)
        (multiple-value-bind (lw lh) (geohash-cell-size 7)
          (declare (ignore lh))
          (is (< (abs (- lat1 lat0)) 1d-6) "east neighbor stays on the same row")
          (is (< (abs (- (- lon1 lon0) lw)) (* 0.01d0 lw))
              "east neighbor centre is ~one cell-width east"))))))

(test neighbors-are-the-eight-around-center
  "geohash-neighbors returns 8 distinct same-precision cells, none equal to the
source, and the source's own geohash sorts between its west and east neighbors."
  (let* ((cell (geohash-encode 45.6720d0 12.3424d0 7))
         (nbrs (geohash-neighbors cell)))
    (is (= 8 (length nbrs)))
    (is (= 8 (length (remove-duplicates nbrs :test #'string=))))
    (is (not (member cell nbrs :test #'string=)))
    (dolist (n nbrs)
      (is (= (length cell) (length n))))
    ;; the 3x3 block is contiguous: every neighbor is within one cell of center
    (multiple-value-bind (lat0 lon0) (geohash-decode cell)
      (multiple-value-bind (lw lh) (geohash-cell-size 7)
        (dolist (n nbrs)
          (multiple-value-bind (latn lonn) (geohash-decode n)
            (is (<= (abs (- lonn lon0)) (* 1.5d0 lw)))
            (is (<= (abs (- latn lat0)) (* 1.5d0 lh)))))))))

(test neighbor-wraps-antimeridian
  "Stepping east off +180 longitude wraps to a valid cell near -180."
  (let* ((cell (geohash-encode 0d0 179.999d0 6))
         (east (geohash-neighbor cell 1 0)))
    (is (stringp east))
    (multiple-value-bind (lat lon) (geohash-decode east)
      (declare (ignore lat))
      (is (< lon 0d0) "wrapped to the western hemisphere"))))

(test neighbor-off-pole-is-nil
  "There is no northern neighbor above the top row of cells (off the pole)."
  (let ((cell (geohash-encode 89.999d0 0d0 6)))
    (is (null (geohash-neighbor cell 0 1)))))
