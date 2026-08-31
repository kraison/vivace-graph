;;;; GEOS implementations of the topology refine seam.
;;;;
;;;; These :AROUND methods take over the generic functions declared in core
;;;; geometry-ops.lisp WHEN GEOS is available; otherwise they CALL-NEXT-METHOD to
;;;; the dependency-free default.  All GEOS work happens inside WITH-GEOS-CONTEXT
;;;; (exclusive context checkout) with RAII geometry cleanup.

(in-package :graph-db)

(defun %geos-bool (result operation)
  "Interpret a GEOS predicate char result: 1 true, 0 false, anything else (2 =
exception) signals GEOS-ERROR."
  (case result
    (1 t)
    (0 nil)
    (t (error 'geos-error
              :message (or *geos-last-error*
                           (format nil "GEOS ~A returned ~A" operation result))))))

(defmethod geometry-intersects-p :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (%geos-bool (%geos-intersects (geos-ctx-handle ctx) ga gb)
                      'intersects)))
      (call-next-method)))

(defmethod geometry-contains-geometry-p :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (%geos-bool (%geos-contains (geos-ctx-handle ctx) ga gb)
                      'contains)))
      (call-next-method)))

(defun geometry-valid-p (g)
  "True if geometry G is topologically valid per GEOS (no self-intersections
etc.).  Requires graph-db/geos; signals GEOS-REQUIRED-FOR-OPERATION otherwise."
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (%geos-bool (%geos-is-valid (geos-ctx-handle ctx) gg) 'is-valid)))
      (error 'geos-required-for-operation :operation 'geometry-valid-p)))

;;; ---- GEOSMakeValid's GEOMETRYCOLLECTION result (GH #163) ---------------

(defun %geos-polygonal-parts (handle geom acc)
  "GEOM's polygonal parts consed onto ACC, descending into collections.

⚠ EVERY POINTER COLLECTED IS BORROWED from GEOM (%GEOS-GET-GEOMETRY-N):
destroying one double-frees when GEOM itself is destroyed."
  (let ((tid (%geos-geom-type-id handle geom)))
    (cond ((or (= tid +geos-polygon+) (= tid +geos-multipolygon+))
           (cons geom acc))
          ((= tid +geos-geometrycollection+)
           (dotimes (i (%geos-get-num-geometries handle geom) acc)
             (let ((sub (%geos-get-geometry-n handle geom i)))
               (unless (cffi:null-pointer-p sub)
                 (setf acc (%geos-polygonal-parts handle sub acc))))))
          (t acc))))

(defun %geos-union-of-borrowed (ctx parts)
  "The VG geometry for the union of PARTS, a non-empty list of BORROWED
GEOS geometry pointers, folded pairwise with GEOSUnion_r.

⚠ Destroys only the intermediates it creates itself; PARTS are the
caller's (in practice their parent collection's) to free."
  (let ((handle (geos-ctx-handle ctx))
        (acc (first parts))
        (owned nil))
    (unwind-protect
         (progn
           (dolist (p (rest parts))
             (let ((next (%geos-union handle acc p)))
               (when (cffi:null-pointer-p next)
                 (error 'geos-error
                        :message (or *geos-last-error*
                                     "GEOSUnion returned NULL")))
               (when owned (%geos-geom-destroy handle acc))
               (setf acc next owned t)))
           (geos->geometry ctx acc))
      (when owned (%geos-geom-destroy handle acc)))))

(defun %geos-repaired->geometry (ctx valid)
  "GEOSMakeValid's result VALID as a VG geometry.  VALID stays the
caller's to destroy.

A repair that splits its input across dimensions answers with a
GEOMETRYCOLLECTION of polygonal AND linear parts; the polygons are the
repaired area and the lines are the slivers it shed, so the answer is
the polygons' union (GH #163).

⚠ A REPAIR WITH NO AREA IN IT SIGNALS GEOS-ERROR rather than yielding
an empty polygon.  Nothing was repaired, and an empty geometry is not
that -- it is the MEASUREMENT 'covers nothing', which %OVERLAP-FRACTION
reads as a zero-measure subject and answers with fraction 1.0 in every
candidate region.  The caller wants %REPAIRED's fallback to the
original, not a fabricated share.

⚠ EMPTINESS IS TESTED ON THE RESULT, ON BOTH PATHS, and not on the
part list: POLYGON EMPTY's type id is +GEOS-POLYGON+, so a no-parts
test passes it through inside a collection AND a top-level one is not a
collection at all.  No reachable input is known for either -- linework
mode, GEOSMakeValid_r's default, keeps degenerate linework as LINES
rather than collapsing it to an empty polygon -- so this is defensive
consistency, not a live bug; a guard with a gap in it reads as covered
(GH #163)."
  (let* ((handle (geos-ctx-handle ctx))
         (repaired
           (if (/= (%geos-geom-type-id handle valid)
                   +geos-geometrycollection+)
               (geos->geometry ctx valid)
               (let ((parts (nreverse
                             (%geos-polygonal-parts handle valid '()))))
                 (and parts (%geos-union-of-borrowed ctx parts))))))
    (when (or (null repaired) (geometry-empty-p repaired))
      (error 'geos-error
             :message "GEOSMakeValid returned a repair with no area in it"))
    repaired))

(defmethod geometry-make-valid :around ((g geometry))
  ;; Requires GEOS >= 3.8 (GEOSMakeValid_r).  When unavailable, fall through to
  ;; the default method, which signals GEOS-REQUIRED-FOR-OPERATION.
  (if (and *geos-available-p* *geos-makevalid-available-p*)
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (let ((valid (%geos-make-valid (geos-ctx-handle ctx) gg)))
            (when (cffi:null-pointer-p valid)
              (error 'geos-error
                     :message (or *geos-last-error* "GEOSMakeValid returned NULL")))
            (unwind-protect (%geos-repaired->geometry ctx valid)
              (%geos-geom-destroy (geos-ctx-handle ctx) valid)))))
      (call-next-method)))

(defmethod geometry-distance-exact :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (cffi:with-foreign-object (out :double)
            (let ((rc (%geos-distance (geos-ctx-handle ctx) ga gb out)))
              (when (zerop rc)
                (error 'geos-error
                       :message (or *geos-last-error* "GEOSDistance failed")))
              (cffi:mem-ref out :double)))))
      (call-next-method)))

;;; ---- constructive (overlay) operations ---------------------------------

(defun %geos-type-dimension (tid)
  "Topological dimension of GEOS type id TID: 2 areal, 1 linear, 0 puntal,
NIL for a GEOMETRYCOLLECTION, whose parts need not share one."
  (cond ((or (= tid +geos-polygon+) (= tid +geos-multipolygon+)) 2)
        ((or (= tid +geos-linestring+) (= tid +geos-linearring+)
             (= tid +geos-multilinestring+))
         1)
        ((or (= tid +geos-point+) (= tid +geos-multipoint+)) 0)
        (t nil)))

(defun %geos-parts-of-dimension (handle geom want acc)
  "GEOM's parts whose dimension is WANT (2 areal, 1 linear, 0 puntal),
consed onto ACC, descending into collections.

⚠ EVERY POINTER COLLECTED IS BORROWED from GEOM, exactly as in
%GEOS-POLYGONAL-PARTS: destroying one double-frees with GEOM."
  (let ((tid (%geos-geom-type-id handle geom)))
    (cond ((= tid +geos-geometrycollection+)
           (dotimes (i (%geos-get-num-geometries handle geom) acc)
             (let ((sub (%geos-get-geometry-n handle geom i)))
               (unless (cffi:null-pointer-p sub)
                 (setf acc (%geos-parts-of-dimension handle sub want acc))))))
          ;; EQL, not =: %GEOS-TYPE-DIMENSION answers NIL for a type id it
          ;; does not classify, and = would signal on it.
          ((eql (%geos-type-dimension tid) want) (cons geom acc))
          (t acc))))

(defun %geos-collection->geometry (ctx res)
  "RES as a VG geometry, reducing a GEOMETRYCOLLECTION to the union of its
HIGHEST-DIMENSION parts.  RES stays the caller's to destroy.

An overlay of two VALID inputs can still answer with a collection: two
polygons overlapping in an AREA and separately meeting at a point or edge
intersect in POLYGON + LINESTRING (GH #164).  The VG GEOMETRY type has no
collection kind, so this signalled and REGISTER-GEOMETRY refused the whole
subject -- 1,560 claims over ten consecutive days of a deployed series.

⚠ HIGHEST DIMENSION, NOT \"THE POLYGONS\".  The measure a caller takes
follows its subject: %MEASURE-FN uses AREA for a polygon and LENGTH for a
LINESTRING (spacetime, design §13).  Keeping only areal parts would hand a
LINE subject an empty geometry -- length 0, read as a mere touch -- and
silently drop an overlap it really has.  The dimension below the top one
is always the boundary contact, and it carries none of the measure.

⚠ EMPTY IS A RESULT, NOT A FAILURE.  Unlike %GEOS-REPAIRED->GEOMETRY,
which signals when a repair kept no area, an overlay legitimately answers
\"they share nothing\" and callers depend on reading that as measure 0
(GH #105).  A repair that repaired nothing and an intersection that
intersected in nothing are different facts."
  (let ((handle (geos-ctx-handle ctx)))
    (if (/= (%geos-geom-type-id handle res) +geos-geometrycollection+)
        (geos->geometry ctx res)
        (let ((parts (loop for want from 2 downto 0
                           for p = (nreverse (%geos-parts-of-dimension
                                              handle res want '()))
                           when p return p)))
          (if (null parts)
              ;; An EMPTY collection: no parts of any dimension.  Answer
              ;; with the empty geometry the caller's measure reads as 0.
              (make-polygon '())
              (%geos-union-of-borrowed ctx parts))))))

(defun %geos-overlay (op-fn a b operation)
  "Run a binary GEOS op returning a new geometry, and convert it back to VG.
A GEOMETRYCOLLECTION result is reduced by %GEOS-COLLECTION->GEOMETRY."
  (with-geos-context (ctx)
    (with-geos-geoms ((ga ctx a) (gb ctx b))
      (let ((res (funcall op-fn (geos-ctx-handle ctx) ga gb)))
        (when (cffi:null-pointer-p res)
          (error 'geos-error
                 :message (or *geos-last-error*
                              (format nil "GEOS ~A returned NULL" operation))))
        (unwind-protect (%geos-collection->geometry ctx res)
          (%geos-geom-destroy (geos-ctx-handle ctx) res))))))

(defmethod geometry-union :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-union a b 'union) (call-next-method)))

(defmethod geometry-intersection :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-intersection a b 'intersection)
      (call-next-method)))

(defmethod geometry-difference :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-difference a b 'difference)
      (call-next-method)))

(defmethod geometry-buffer :around ((g geometry) width &optional (quadrant-segments 8))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (let ((res (%geos-buffer (geos-ctx-handle ctx) gg
                                   (coerce width 'double-float) quadrant-segments)))
            (when (cffi:null-pointer-p res)
              (error 'geos-error :message (or *geos-last-error* "GEOSBuffer returned NULL")))
            (unwind-protect (geos->geometry ctx res)
              (%geos-geom-destroy (geos-ctx-handle ctx) res)))))
      (call-next-method)))

(defmethod geometry-area :around ((g geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (cffi:with-foreign-object (out :double)
            (when (zerop (%geos-area (geos-ctx-handle ctx) gg out))
              (error 'geos-error :message (or *geos-last-error* "GEOSArea failed")))
            (cffi:mem-ref out :double))))
      (call-next-method)))

;;; ---- geodesic distance between geometries (real metres) ----------------

(defmethod geometry-geodesic-distance :around ((a geometry) (b geometry))
  ;; Point-point is handled by the core default (haversine) -- but routing it
  ;; through GEOS nearest-points gives the same answer, so we let GEOS handle all
  ;; cases when available for consistency, and fall back otherwise.
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (let ((seq (%geos-nearest-points (geos-ctx-handle ctx) ga gb)))
            (when (cffi:null-pointer-p seq)
              (error 'geos-error
                     :message (or *geos-last-error* "GEOSNearestPoints failed")))
            (unwind-protect
                 (cffi:with-foreign-objects ((x1 :double) (y1 :double)
                                             (x2 :double) (y2 :double))
                   ;; seq holds 2 points: idx 0 on A, idx 1 on B.  Coords are
                   ;; (lon lat) = (x y); haversine takes (lat lon).
                   (%geos-coordseq-getx (geos-ctx-handle ctx) seq 0 x1)
                   (%geos-coordseq-gety (geos-ctx-handle ctx) seq 0 y1)
                   (%geos-coordseq-getx (geos-ctx-handle ctx) seq 1 x2)
                   (%geos-coordseq-gety (geos-ctx-handle ctx) seq 1 y2)
                   (geodesic-distance (cffi:mem-ref y1 :double) (cffi:mem-ref x1 :double)
                                      (cffi:mem-ref y2 :double) (cffi:mem-ref x2 :double)))
              (%geos-coordseq-destroy (geos-ctx-handle ctx) seq)))))
      (call-next-method)))
