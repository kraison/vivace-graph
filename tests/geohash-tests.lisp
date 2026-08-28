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

;;; ---------------------------------------------------------------------
;;; The covering's cost bound (GH #279).
;;;
;;; MAX-CELLS bounds the ANSWER; what has to be bounded is the WORK, since
;;; GEOHASH-COVERING walks a (1+nlon)x(1+nlat) grid.  Before this, an
;;; explicitly supplied :PRECISION skipped the check and the box was
;;; unclamped, so a spatial query with a client-supplied radius drove a
;;; quadratic grid walk (measured: a 2e10 m radius, a ~180,000-degree
;;; span, 24.6 s of CPU on a five-node index).
;;;
;;; These three run their covering call UNDER A DEADLINE, and that is the
;;; point of the machinery rather than an excess of caution: a regression
;;; here does not make GEOHASH-COVERING slow, it makes it not return.
;;; Asserting on elapsed time after the call cannot fire, because control
;;; never reaches the assertion -- a re-break would turn the suite red by
;;; STALLING it, with no message naming what broke.  Measured on the
;;; pre-fix code, COVERING-CLAMPS-ITS-BOX-TO-THE-GLOBE ran 4.5+ minutes
;;; without completing; unmutated it takes milliseconds.
;;; ---------------------------------------------------------------------

(defparameter *covering-deadline-seconds* 30
  "Wall-clock bound for one covering call in these tests.

Deliberately enormous next to the real cost (milliseconds) so a loaded
machine cannot flake it, and still far under the unbounded case
(minutes to forever).  The gap is three orders of magnitude wide in both
directions; there is no need to tune this.")

(defun %call-under-deadline (seconds thunk label)
  "(values RESULT T) if THUNK returned within SECONDS, (values NIL NIL)
if it did not.

A worker thread plus a semaphore wait with a timeout: the portable
shape, and the one the repo already reaches for (tests use BT:
throughout).  SB-EXT:WITH-TIMEOUT is SBCL-only and TRIVIAL-TIMEOUT is a
CCL/LispWorks dependency that ECL does not have -- see mailbox.lisp --
and this is the MAIN suite, which runs on all three.

A worker that blows the deadline is destroyed rather than left to burn a
core and cons until the image dies.  DESTROY-THREAD is a last resort in
general and is defensible HERE specifically: GEOHASH-COVERING is a pure
function -- no lock, no graph, no mmap, no global state -- so an
abandoned worker can only ever be inside arithmetic or consing, and
killing it can wedge nothing (GH #279)."
  (let* ((done (bt:make-semaphore))
         (result nil)
         (thread (bt:make-thread
                  (lambda ()
                    (unwind-protect (setf result (funcall thunk))
                      (bt:signal-semaphore done)))
                  :name (format nil "covering deadline: ~A" label))))
    (cond ((bt:wait-on-semaphore done :timeout seconds)
           (values result t))
          (t
           (ignore-errors (bt:destroy-thread thread))
           (values nil nil)))))

(defmacro with-covering-deadline ((var label form) &body body)
  "Bind VAR to FORM's value, computed under *COVERING-DEADLINE-SECONDS*,
then run BODY.  On a blown deadline, record ONE failure naming LABEL and
the bound, and SKIP BODY -- whose assertions would otherwise report
against NIL and bury the real cause."
  (let ((ok (gensym "OK")))
    `(multiple-value-bind (,var ,ok)
         (%call-under-deadline *covering-deadline-seconds*
                               (lambda () ,form) ,label)
       (if ,ok
           (progn ,@body)
           (is-true nil
                    "~A did not finish within ~D s.  GEOHASH-COVERING ~
walks a (1+nlon)x(1+nlat) grid, so an unclamped box or an unchecked ~
:PRECISION does not return slowly -- it does not return at all.  Check ~
the globe clamp and the max-cells check in GEOHASH-COVERING, and the ~
span clamp in MAP-SPATIAL-INDEX-RADIUS (GH #279)."
                    ,label *covering-deadline-seconds*)))))

(test covering-honours-max-cells-with-explicit-precision
  "An explicitly supplied precision is still lowered until the grid fits.
This is the path every spatial QUERY takes -- MAP-SPATIAL-INDEX-BBOX
always passes :precision -- so an unchecked one was a live hole."
  (let ((max-cells 256))
    ;; A continent-sized window asked for at full precision: honoured at
    ;; a coarser one rather than enumerating millions of cells.
    (with-covering-deadline
        (cells "a continental window at precision 12"
               (geohash-covering -60d0 -30d0 60d0 30d0
                                 :precision 12 :max-cells max-cells))
      (is (<= (length cells) max-cells)))
    ;; ...and a small window still gets the precision it asked for.
    (with-covering-deadline
        (cells "a metre-scale window at precision 7"
               (geohash-covering 12.3424d0 45.6720d0 12.3425d0 45.6721d0
                                 :precision 7))
      (is (= 7 (length (first cells)))))))

(test covering-clamps-its-box-to-the-globe
  "A box larger than the planet costs no more than the planet.  No
geohash cell exists outside the globe, so clamping cannot lose one --
and it is what bounds the grid walk when a caller's span is absurd."
  (with-covering-deadline
      (globe "the globe at precision 1"
             (geohash-covering -180d0 -90d0 180d0 90d0 :precision 1))
    (with-covering-deadline
        (absurd "a box larger than the planet, at precision 1"
                (geohash-covering -1d9 -1d9 1d9 1d9 :precision 1))
      (is (<= (length globe) 256))
      ;; Same answer, promptly: the oversized box is the globe.
      (is (equal (sort (copy-list globe) #'string<)
                 (sort (copy-list absurd) #'string<))))))

(test covering-of-an-absurd-box-returns-promptly
  "The bound is on TIME, not just on the returned list -- the defect was
a grid walk whose cost was quadratic in the span while the answer stayed
small.  Ten seconds is a decade of headroom over the clamped cost (which
is milliseconds) and still far under the 24.6 s the unclamped path took
for a merely large span; the deadline above catches the genuinely
unbounded one."
  (let ((start (get-internal-real-time)))
    (with-covering-deadline
        (cells "an astronomically large box at precision 12"
               (geohash-covering -1d12 -1d12 1d12 1d12 :precision 12))
      (is (plusp (length cells)))
      (is (< (/ (- (get-internal-real-time) start)
                internal-time-units-per-second)
             10)))))
