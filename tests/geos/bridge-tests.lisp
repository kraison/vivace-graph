;;;; S1: geometry <-> WKT bridge round-trips, and the context pool basics.

(in-package #:graph-db/geos-test)

(def-suite geos-bridge-suite
  :description "geometry->wkt / wkt->geometry / GEOS round-trip + context pool."
  :in geos-suite)

(in-suite geos-bridge-suite)

;;; ---- coordinate comparison ---------------------------------------------

(defun coords-approx-equal (a b &optional (eps 1d-9))
  "Recursively compare two coordinate structures (numbers, vectors, or nested lists)."
  (cond ((and (numberp a) (numberp b)) (<= (abs (- a b)) eps))
        ((and (typep a '(simple-array double-float (*))) (consp b))
         (let ((n (/ (length a) 2)))
           (and (= n (length b))
                (loop for i from 0 below n
                      for c in b
                      always (and (<= (abs (- (aref a (* 2 i)) (first c))) eps)
                                  (<= (abs (- (aref a (1+ (* 2 i))) (second c))) eps))))))
        ((and (consp a) (typep b '(simple-array double-float (*))))
         (coords-approx-equal b a eps))
        ((and (typep a 'sequence) (typep b 'sequence) (= (length a) (length b)))
         (every (lambda (x y) (coords-approx-equal x y eps)) a b))
        ((and (listp a) (listp b) (= (length a) (length b)))
         (every (lambda (x y) (coords-approx-equal x y eps)) a b))
        (t nil)))

(defun geom-approx-equal (a b)
  (and (eq (geometry-kind a) (geometry-kind b))
       (coords-approx-equal (geometry-coordinates a) (geometry-coordinates b))))

;;; sample geometries (lon lat), Kharkiv-ish; polygon rings are pre-closed.
(defun sample-point () (make-point 37.1724312d0 49.2020584d0))
(defun sample-line ()
  (make-linestring '((37.10d0 49.10d0) (37.20d0 49.15d0) (37.30d0 49.05d0))))
(defun sample-polygon ()
  (make-polygon '(((37.16d0 49.19d0) (37.19d0 49.19d0)
                   (37.19d0 49.21d0) (37.16d0 49.21d0) (37.16d0 49.19d0)))))
(defun sample-polygon-with-hole ()
  (make-polygon '(((0d0 0d0) (10d0 0d0) (10d0 10d0) (0d0 10d0) (0d0 0d0))
                  ((3d0 3d0) (3d0 6d0) (6d0 6d0) (6d0 3d0) (3d0 3d0)))))
(defun sample-multipolygon ()
  (make-multipolygon '((((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0) (0d0 0d0)))
                       (((5d0 5d0) (7d0 5d0) (7d0 7d0) (5d0 7d0) (5d0 5d0))))))

;;; ---- pure parser round-trip (no GEOS) ----------------------------------

(test wkt-pure-round-trip-all-kinds
  "geometry->wkt then wkt->geometry recovers each geometry (parser + writer are
inverses), independent of GEOS."
  (dolist (g (list (sample-point) (sample-line) (sample-polygon)
                   (sample-polygon-with-hole) (sample-multipolygon)))
    (is (geom-approx-equal g (wkt->geometry (geometry->wkt g)))
        "pure round-trip failed for ~A: ~A" (geometry-kind g) (geometry->wkt g))))

(test wkt-writer-shapes
  "The emitted WKT has the expected leading keyword and structure."
  (is (eql 0 (search "POINT (" (geometry->wkt (sample-point)))))
  (is (eql 0 (search "LINESTRING (" (geometry->wkt (sample-line)))))
  (is (eql 0 (search "POLYGON ((" (geometry->wkt (sample-polygon)))))
  (is (eql 0 (search "MULTIPOLYGON (((" (geometry->wkt (sample-multipolygon))))))

(test wkt-closes-open-rings
  "An open exterior ring is closed in the emitted WKT (first vertex repeated)."
  (let* ((open (make-polygon '(((0d0 0d0) (4d0 0d0) (4d0 4d0) (0d0 4d0)))))  ; not closed
         (parsed (wkt->geometry (geometry->wkt open)))
         (ring (first (geometry-coordinates parsed))))
    (let ((n-verts (if (arrayp ring) (/ (length ring) 2) (length ring)))
          (first-x (if (arrayp ring) (aref ring 0) (first (first ring))))
          (first-y (if (arrayp ring) (aref ring 1) (second (first ring))))
          (last-x (if (arrayp ring) (aref ring (- (length ring) 2)) (first (car (last ring)))))
          (last-y (if (arrayp ring) (aref ring (- (length ring) 1)) (second (car (last ring))))))
      (is (and (= first-x last-x) (= first-y last-y)) "ring not closed: ~A" ring)
      (is (= 5 n-verts) "expected 5 vertices after closure, got ~D" n-verts))))



;;; ---- GEOS round-trip (writer/reader through libgeos) -------------------

(test geos-round-trip-all-kinds
  "VG geometry -> GEOS -> WKT -> VG geometry recovers each geometry through the
real libgeos reader/writer."
  (if (not *geos-available-p*)
      (skip "GEOS not available")
      (with-geos-context (ctx)
        (dolist (g (list (sample-point) (sample-line) (sample-polygon)
                         (sample-polygon-with-hole) (sample-multipolygon)))
          (let ((back (graph-db::geos->geometry
                       ctx (graph-db::geometry->geos ctx g))))
            (is (geom-approx-equal g back)
                "GEOS round-trip failed for ~A" (geometry-kind g)))))))

;;; ---- context pool ------------------------------------------------------

(test context-pool-reuses-and-balances
  "Repeated WITH-GEOS-CONTEXT reuses a single context (created once) and always
returns it to the pool (in-use back to 0)."
  (if (not *geos-available-p*)
      (skip "GEOS not available")
      (progn
        (geos-shutdown)                 ; start from a clean pool
        (dotimes (_ 25)
          (with-geos-context (ctx)
            (graph-db::%geos-geom-destroy
             (graph-db::geos-ctx-handle ctx)
             (graph-db::geometry->geos ctx (sample-point)))))
        (is (= 1 *geos-pool-created*) "one context reused (created ~D)" *geos-pool-created*)
        (is (= 0 *geos-pool-in-use*) "all checked back in (~D still out)" *geos-pool-in-use*)
        (is (= 1 (length *geos-pool*)) "one free context pooled"))))

(test context-pool-returns-on-error
  "A non-local exit from the body still returns the context to the pool."
  (if (not *geos-available-p*)
      (skip "GEOS not available")
      (progn
        (geos-shutdown)
        (ignore-errors
         (with-geos-context (ctx)
           (graph-db::geos-ctx-handle ctx)   ; touch ctx, then bail
           (error "boom")))
        (is (= 0 *geos-pool-in-use*) "context returned despite the error")
        (is (= 1 (length *geos-pool*))))))

(test geos-shutdown-clears-pool
  "geos-shutdown destroys pooled contexts and resets counters."
  (if (not *geos-available-p*)
      (skip "GEOS not available")
      (progn
        (with-geos-context (ctx) (graph-db::geos-ctx-handle ctx))
        (geos-shutdown)
        (is (= 0 *geos-pool-created*))
        (is (null *geos-pool*)))))

;;; ---- EMPTY geometries (GH #105) ----------------------------------------
;;;
;;; GEOS emits an empty result as "POLYGON EMPTY" -- no parenthesis -- and the
;;; type dispatch used to read the keyword as "everything before the first
;;; paren", i.e. the whole string.  Every EMPTY form fell through to the
;;; unsupported-type error, and an empty intersection is the NORMAL result for
;;; two disjoint polygons, so this fired on the common case.

(test wkt-parses-every-empty-form
  "Each EMPTY form parses to its own kind, carrying no coordinates.  Pure
parser -- no GEOS needed."
  (dolist (spec '(("POINT EMPTY" :point)
                  ("LINESTRING EMPTY" :linestring)
                  ("POLYGON EMPTY" :polygon)
                  ("MULTIPOLYGON EMPTY" :multipolygon)))
    (destructuring-bind (wkt kind) spec
      (let ((g (handler-case (wkt->geometry wkt)
                 (error (e) e))))
        (is (geometryp g) "~A did not parse: ~A" wkt g)
        (when (geometryp g)
          (is (eq kind (geometry-kind g))
              "~A parsed as ~A" wkt (geometry-kind g))
          (is (geometry-empty-p g)
              "~A parsed with coordinates ~A" wkt (geometry-coordinates g)))))))

(test empty-point-is-not-the-origin
  "POINT EMPTY must NOT become a point at (0, 0).  Null island is a real
location; returning it turns \"there is nothing here\" into \"there is
something here, off the coast of Africa\" -- and GEOMETRY->WKT would then write
it back out as POINT (0 0), so the falsehood persists."
  (let ((g (wkt->geometry "POINT EMPTY")))
    (is (geometry-empty-p g))
    (is (string= "POINT EMPTY" (geometry->wkt g))
        "an empty point must write back as EMPTY, got ~A" (geometry->wkt g))))

(test wkt-empty-is-case-and-space-insensitive
  "The type token is read as the first token, so leading/trailing space and
lower case still dispatch."
  (dolist (wkt '("  polygon empty  " "Polygon Empty" "POLYGON    EMPTY"))
    (let ((g (handler-case (wkt->geometry wkt) (error (e) e))))
      (is (geometryp g) "~S did not parse: ~A" wkt g)
      (when (geometryp g)
        (is (eq :polygon (geometry-kind g)))
        (is (geometry-empty-p g))))))

(test wkt-empty-round-trips
  "GEOMETRY->WKT and WKT->GEOMETRY stay inverses over the empty geometries, the
way they already are over the populated ones."
  (dolist (g (list (graph-db::%make-geometry :kind :point :coordinates nil)
                   (make-linestring '())
                   (make-polygon '())
                   (make-multipolygon '())))
    (let ((back (handler-case (wkt->geometry (geometry->wkt g)) (error (e) e))))
      (is (geometryp back) "~A round-trip signalled: ~A" (geometry-kind g) back)
      (when (geometryp back)
        (is (eq (geometry-kind g) (geometry-kind back)))
        (is (geometry-empty-p back))))))

(test unsupported-wkt-type-still-signals
  "The fallthrough must still reject a type this parser does not implement --
and name just the type token, not the whole string."
  (signals geos-error (wkt->geometry "GEOMETRYCOLLECTION EMPTY"))
  (signals geos-error (wkt->geometry "TRIANGLE ((0 0, 1 0, 1 1, 0 0))")))
