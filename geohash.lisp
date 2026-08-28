(in-package :graph-db)

;;; Geohash encoding for the spatial index.
;;;
;;; Public, general-purpose (no domain knowledge).  A geohash maps a (lat, lon)
;;; point to a base-32 string whose prefixes are nested bounding cells -- so an
;;; ordered store (our skip list) can answer window/proximity queries by prefix
;;; range scans.  Longitude bits and latitude bits are interleaved, longitude
;;; first; every 5 bits become one base-32 character.
;;;
;;; The chosen alphabet omits a, i, l, o to avoid ambiguity (standard geohash).

(alexandria:define-constant +geohash-base32+ "0123456789bcdefghjkmnpqrstuvwxyz"
  :test 'string=)

(defun geohash-encode (lat lon &optional (precision 12))
  "Encode (LAT, LON) to a PRECISION-character geohash string."
  (declare (type real lat lon) (type (integer 1 22) precision))
  (let ((lat-lo -90d0) (lat-hi 90d0) (lon-lo -180d0) (lon-hi 180d0)
        (even-bit t) (bit 0) (idx 0)
        (chars (make-array precision :element-type 'character :fill-pointer 0)))
    (loop while (< (fill-pointer chars) precision) do
      (if even-bit
          (let ((mid (/ (+ lon-lo lon-hi) 2)))
            (if (>= lon mid)
                (progn (setf idx (logior (ash idx 1) 1)) (setf lon-lo mid))
                (progn (setf idx (ash idx 1)) (setf lon-hi mid))))
          (let ((mid (/ (+ lat-lo lat-hi) 2)))
            (if (>= lat mid)
                (progn (setf idx (logior (ash idx 1) 1)) (setf lat-lo mid))
                (progn (setf idx (ash idx 1)) (setf lat-hi mid)))))
      (setf even-bit (not even-bit))
      (when (= (incf bit) 5)
        (vector-push (char +geohash-base32+ idx) chars)
        (setf bit 0 idx 0)))
    (coerce chars 'simple-string)))

(defun geohash-bbox (hash)
  "Bounding cell of HASH as (values min-lon min-lat max-lon max-lat)."
  (let ((lat-lo -90d0) (lat-hi 90d0) (lon-lo -180d0) (lon-hi 180d0) (even-bit t))
    (loop for c across hash
          for cd = (position (char-downcase c) +geohash-base32+) do
            (unless cd (error "Invalid geohash character ~C in ~S" c hash))
            (dolist (mask '(16 8 4 2 1))
              (if even-bit
                  (let ((mid (/ (+ lon-lo lon-hi) 2)))
                    (if (plusp (logand cd mask)) (setf lon-lo mid) (setf lon-hi mid)))
                  (let ((mid (/ (+ lat-lo lat-hi) 2)))
                    (if (plusp (logand cd mask)) (setf lat-lo mid) (setf lat-hi mid))))
              (setf even-bit (not even-bit))))
    (values lon-lo lat-lo lon-hi lat-hi)))

(defun geohash-decode (hash)
  "Center of HASH's cell as (values lat lon)."
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geohash-bbox hash)
    (values (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2))))

(defun geohash-cell-size (precision)
  "Cell dimensions at PRECISION as (values lon-width lat-height) in degrees."
  (let ((lon-bits (ceiling (* 5 precision) 2))
        (lat-bits (floor (* 5 precision) 2)))
    (values (/ 360d0 (expt 2 lon-bits))
            (/ 180d0 (expt 2 lat-bits)))))

(defun geohash-prefix-range (cell)
  "Half-open key range (values START END) such that any full geohash with prefix
CELL sorts in [START, END).  END appends a character just above the alphabet so
the range scan over an ordered store captures the whole cell."
  (values cell (concatenate 'string cell (string (code-char (1+ (char-code #\z)))))))

(defun geohash-neighbor (cell dlon dlat)
  "The geohash cell adjacent to CELL stepped DLON cells in longitude and DLAT
cells in latitude (each typically -1, 0, or 1), at CELL's precision.  Returns
NIL when the step runs past a pole (no latitudinal neighbor there); longitude
wraps at the antimeridian.  Implemented by stepping a whole cell from CELL's
centre and re-encoding, so it is robust to float boundary rounding."
  (let ((precision (length cell)))
    (multiple-value-bind (min-lon min-lat max-lon max-lat) (geohash-bbox cell)
      (multiple-value-bind (lw lh) (geohash-cell-size precision)
        (let ((nlat (+ (/ (+ min-lat max-lat) 2) (* dlat lh)))
              (nlon (+ (/ (+ min-lon max-lon) 2) (* dlon lw))))
          (when (<= -90d0 nlat 90d0)
            ;; wrap longitude into [-180, 180)
            (setf nlon (- (mod (+ nlon 180d0) 360d0) 180d0))
            (geohash-encode nlat nlon precision)))))))

(defun geohash-neighbors (cell)
  "The up-to-8 distinct geohash cells adjacent to CELL (same precision),
excluding CELL itself.  Cells off a pole are omitted; longitude wraps at the
antimeridian.  Enables cell-boundary-spanning proximity (a point near a cell
edge has near neighbours in the adjacent cells) and ring-expansion kNN."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (dlat '(-1 0 1))
      (dolist (dlon '(-1 0 1))
        (unless (and (zerop dlat) (zerop dlon))
          (let ((nb (geohash-neighbor cell dlon dlat)))
            (when (and nb (not (string= nb cell)) (not (gethash nb seen)))
              (setf (gethash nb seen) t)
              (push nb result))))))
    (nreverse result)))

(defun %covering-cell-count (dlon dlat precision)
  "Cells a DLON x DLAT degree box needs at PRECISION.  The single estimate the
spatial layer budgets with: %COVERING-PRECISION inverts it and %GEOMETRY-CELLS
sums it across a multipolygon's parts (GH #103), so the two cannot disagree
about whether a cover fits."
  (multiple-value-bind (lw lh) (geohash-cell-size precision)
    (* (+ 1 (ceiling dlon lw)) (+ 1 (ceiling dlat lh)))))

;;; Coordinate clamps (GH #279).
;;;
;;; Every one of these clamps with RATIONAL bounds and coerces AFTERWARDS.
;;; The order is the whole point: a caller's number may be a bignum or a
;;; ratio, and coercing that to a double signals FLOATING-POINT-OVERFLOW
;;; before any clamp on the result could see it.  CL compares a rational
;;; against a float exactly, so (min 180 <bignum>) is 180 and the overflow
;;; never happens.  A coordinate outside the globe is meaningless anyway,
;;; and a radius past half the circumference already means "everything".

(defun clamp-latitude (lat)
  "LAT clamped to [-90, 90] as a double-float."
  (float (max -90 (min 90 lat)) 1d0))

(defun clamp-longitude (lon)
  "LON clamped to [-180, 180] as a double-float."
  (float (max -180 (min 180 lon)) 1d0))

(defun clamp-radius-metres (radius)
  "RADIUS clamped to [0, 20_100_000] metres -- just past half the
earth's circumference, beyond which a radius query means every node.
Stays RATIONAL until the bound is applied, so a bignum cannot overflow
on the way in."
  (float (max 0 (min 20100000 radius)) 1d0))

(defun %covering-precision (dlon dlat max-cells)
  "Finest precision whose grid covers a DLON x DLAT degree box in <= MAX-CELLS
cells, FLOORED AT 1.

The floor is not a fit guarantee, and callers must not read it as one: precision
1 is the coarsest grid there is, so when even it exceeds MAX-CELLS this still
returns 1 and the caller's grid walk is larger than it budgeted for.  What makes
that bounded is GEOHASH-COVERING clamping its box to the globe before walking --
at precision 1 the whole planet is 60 grid steps (GH #279)."
  (let ((best 1))
    (loop for p from 1 to 12 do
      (if (<= (%covering-cell-count dlon dlat p) max-cells)
          (setf best p)
          (return)))
    best))

(defun geohash-covering (min-lon min-lat max-lon max-lat
                         &key precision (max-cells 256))
  "List of distinct geohash cells (strings) covering the given bounding box.
Used to turn a map viewport into a set of prefix range scans.  PRECISION is
chosen adaptively to stay under MAX-CELLS; an explicitly supplied one is still
LOWERED until the grid fits.

Both bounds are load-bearing, because this walks the grid: the cost is
(1+nlon)x(1+nlat) GEOHASH-ENCODE calls no matter how few DISTINCT cells come
back, so MAX-CELLS bounds the answer, not the work.  Before GH #279 an explicit
PRECISION skipped the check entirely and the box was unclamped, so a spatial
query with a client-supplied radius drove a quadratic grid walk: radius 2e10 m
became a ~180,000-degree span and 24.6 s of CPU on a FIVE-node index, with the
query deadline unable to fire inside the call.

The box is clamped to the globe first -- no geohash cell exists outside it, so
that cannot lose a result.  Lowering the precision cannot either: a coarser cell
still COVERS the box, and callers scan each returned cell as a prefix range,
which reaches every finer cell nested inside it."
  ;; Clamp BEFORE coercing -- see the clamp helpers above.  Coercing
  ;; first overflowed on a bignum or ratio coordinate, which let an
  ;; unauthenticated caller manufacture a FLOATING-POINT-OVERFLOW and,
  ;; through it, a spurious server-fault alarm (GH #279).
  (let* ((min-lon (clamp-longitude min-lon))
         (max-lon (clamp-longitude max-lon))
         (min-lat (clamp-latitude min-lat))
         (max-lat (clamp-latitude max-lat))
         (dlon (max 0d0 (- max-lon min-lon)))
         (dlat (max 0d0 (- max-lat min-lat)))
         (fits (%covering-precision dlon dlat max-cells))
         (p (if precision (min precision fits) fits))
         (seen (make-hash-table :test 'equal))
         (cells '()))
    (multiple-value-bind (lw lh) (geohash-cell-size p)
      (let ((nlon (+ 1 (ceiling dlon lw)))
            (nlat (+ 1 (ceiling dlat lh))))
        (dotimes (i (1+ nlon))
          (let ((lon (min max-lon (+ min-lon (* i lw)))))
            (dotimes (j (1+ nlat))
              (let* ((lat (min max-lat (+ min-lat (* j lh))))
                     (cell (geohash-encode lat lon p)))
                (unless (gethash cell seen)
                  (setf (gethash cell seen) t)
                  (push cell cells))))))))
    cells))
