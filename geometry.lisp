(in-package :graph-db)

;;; Geometry value type for the spatial extension.
;;;
;;; Part of VivaceGraph's public, general-purpose spatial layer (no domain
;;; knowledge lives here).  Coordinates are WGS84 and stored in (LON LAT) order
;;; -- GIS x,y / GeoJSON convention -- as DOUBLE-FLOATs.  KIND is one of :POINT
;;; :LINESTRING :POLYGON :MULTIPOLYGON.
;;;
;;; Internal coordinate storage in the COORDINATES slot uses packed
;;; (simple-array double-float (*)) vectors for rings (3-4x memory reduction,
;;; zero-consing spatial operations):
;;;   :point         -> #(lon lat)
;;;   :linestring    -> #(lon0 lat0 lon1 lat1 ...)
;;;   :polygon       -> (ring ...) where ring = #(lon0 lat0 ...)
;;;   :multipolygon  -> (polygon ...) where polygon = (ring ...)
;;;
;;; Public coordinate accessors:
;;;   (geometry-coordinates g)        -> packed array representation (in-memory & serialization)
;;;   (geometry-coordinate-pairs g)   -> classic nested list of (lon lat) pairs
;;;   (do-geometry-coordinates (lon lat) g &body body) -> zero-allocation vertex iteration macro
;;;   (map-geometry-coordinates fn g) -> functional vertex traversal (~32 B/vertex due to funcall boxing)
;;;
;;; The wire format reuses the generic byte protocol:
;;;   [+geometry+][len-header][serialized kind-code][serialized coordinates]
;;; built with SERIALIZE-MULTIPLE.  The payload uses the fast float-vector codec
;;; (+fv-double-float+).

(defstruct (geometry (:constructor %make-geometry) (:predicate geometryp))
  (kind :point :type symbol)
  coordinates)



(defparameter +geometry-kinds+ '(:point :linestring :polygon :multipolygon)
  "Ordered list; a geometry's KIND is serialized as its position here.")

(defun geometry-kind-code (kind)
  (or (position kind +geometry-kinds+)
      (error "Unknown geometry kind ~S" kind)))

(defun geometry-code-kind (code)
  (or (nth code +geometry-kinds+)
      (error "Unknown geometry kind code ~S" code)))

(declaim (inline %df))
(defun %df (x)
  "Coerce X to DOUBLE-FLOAT (all stored coordinates are double-floats)."
  (coerce x 'double-float))

(defun %coord-vec (coords)
  "Convert COORDS (a list of (lon lat) pairs, or a packed double-float array) into a packed (simple-array double-float (*)) array."
  (cond
    ((typep coords '(simple-array double-float (*)))
     coords)
    ((consp coords)
     (let* ((n (length coords))
            (vec (make-array (* 2 n) :element-type 'double-float)))
       (loop for c in coords
             for idx from 0 by 2
             do (setf (aref vec idx) (%df (first c))
                      (aref vec (1+ idx)) (%df (second c))))
       vec))
    ((vectorp coords)
     (let* ((len (length coords))
            (vec (make-array len :element-type 'double-float)))
       (dotimes (i len)
         (setf (aref vec i) (%df (aref coords i))))
       vec))
    (t (make-array 0 :element-type 'double-float))))

(defun %normalize-coordinates (kind coordinates)
  "Ensure COORDINATES for KIND use packed double-float arrays."
  (ecase kind
    (:point
     (cond
       ((typep coordinates '(simple-array double-float (*)))
        coordinates)
       ((consp coordinates)
        (make-array 2 :element-type 'double-float
                      :initial-contents (list (%df (first coordinates)) (%df (second coordinates)))))
       (t coordinates)))
    (:linestring
     (%coord-vec coordinates))
    (:polygon
     (if (consp coordinates)
         (mapcar #'%coord-vec coordinates)
         coordinates))
    (:multipolygon
     (if (consp coordinates)
         (mapcar (lambda (p) (if (consp p) (mapcar #'%coord-vec p) p)) coordinates)
         coordinates))))

;;; -------------------------------------------------------------------------
;;; Constructors
;;; -------------------------------------------------------------------------

(defun make-point (lon lat)
  (let ((vec (make-array 2 :element-type 'double-float)))
    (setf (aref vec 0) (%df lon)
          (aref vec 1) (%df lat))
    (%make-geometry :kind :point :coordinates vec)))

(defun make-linestring (coords)
  "COORDS: a list of (lon lat) or a packed double-float array."
  (%make-geometry :kind :linestring :coordinates (%coord-vec coords)))

(defun make-polygon (rings)
  "RINGS: a list of rings, each a list of (lon lat) or a packed double-float array."
  (%make-geometry :kind :polygon :coordinates (mapcar #'%coord-vec rings)))

(defun make-multipolygon (polygons)
  "POLYGONS: a list of polygons, each a list of rings."
  (%make-geometry :kind :multipolygon
                  :coordinates (mapcar (lambda (p) (mapcar #'%coord-vec p)) polygons)))

;;; -------------------------------------------------------------------------
;;; Accessors
;;; -------------------------------------------------------------------------

(defun geometry-lon (g)
  "Longitude of a :POINT geometry."
  (let ((c (geometry-coordinates g)))
    (if (arrayp c)
        (aref c 0)
        (%df (first c)))))

(defun geometry-lat (g)
  "Latitude of a :POINT geometry."
  (let ((c (geometry-coordinates g)))
    (if (arrayp c)
        (aref c 1)
        (%df (second c)))))

(defun geometry-empty-p (g)
  "True when G holds no coordinates -- the EMPTY geometry of its KIND, which is
preserved (an empty polygon is still :POLYGON).

Emptiness is a real answer, not a failure: the intersection of two disjoint
polygons IS empty, and only a genuine failure signals (GH #105).  Callers that
must tell \"nothing there\" from \"could not compute\" test this rather than the
condition.  GEOMETRY->WKT writes such a geometry as \"<TYPE> EMPTY\".

Not simply (NULL (GEOMETRY-COORDINATES G)): an empty linestring's coordinates
are a zero-length vector, not NIL, and the nested kinds hold their emptiness one
or two levels down."
  (labels ((emptyp (c)
             (cond ((null c) t)
                   ((and (vectorp c) (not (stringp c))) (zerop (length c)))
                   ((consp c) (every #'emptyp c))
                   (t nil))))
    (emptyp (geometry-coordinates g))))

(defun geometry-bbox (g)
  "Axis-aligned bounding box of G as (values min-lon min-lat max-lon max-lat)."
  (let ((min-lon nil) (min-lat nil) (max-lon nil) (max-lat nil))
    (labels ((visit (lon lat)
               (when (or (null min-lon) (< lon min-lon)) (setf min-lon lon))
               (when (or (null max-lon) (> lon max-lon)) (setf max-lon lon))
               (when (or (null min-lat) (< lat min-lat)) (setf min-lat lat))
               (when (or (null max-lat) (> lat max-lat)) (setf max-lat lat)))
             (walk-ring (r)
               (cond
                 ((typep r '(simple-array double-float (*)))
                  (loop for i from 0 below (length r) by 2
                        do (visit (aref r i) (aref r (1+ i)))))
                 ((consp r)
                  (dolist (c r)
                    (if (and (consp c) (numberp (first c)))
                        (visit (%df (first c)) (%df (second c)))
                        (walk-ring c)))))))
      (if (eq (geometry-kind g) :point)
          (visit (geometry-lon g) (geometry-lat g))
          (let ((coords (geometry-coordinates g)))
            (if (consp coords)
                (dolist (item coords) (walk-ring item))
                (walk-ring coords))))
      (values min-lon min-lat max-lon max-lat))))

(defmacro do-geometry-coordinates ((lon lat) geometry &body body)
  "Iterate over every (LON LAT) double-float vertex in GEOMETRY, binding LON and LAT.
This macro form inlines the traversal loops and operates directly on packed float arrays
with zero memory allocations (0 bytes/vertex)."
  (let ((g-var (gensym "GEOM"))
        (c-var (gensym "COORDS"))
        (ring-var (gensym "RING"))
        (poly-var (gensym "POLY"))
        (v-var (gensym "VEC"))
        (i-var (gensym "I"))
        (fn-var (gensym "FN"))
        (visit-vec-var (gensym "VISIT-VEC"))
        (visit-ring-var (gensym "VISIT-RING")))
    `(let ((,g-var ,geometry))
       (flet ((,fn-var (,lon ,lat)
                (declare (type double-float ,lon ,lat))
                ,@body))
         (declare (inline ,fn-var)
                  (dynamic-extent #',fn-var))
         (labels ((,visit-vec-var (,v-var)
                    (declare (type (simple-array double-float (*)) ,v-var))
                    (loop for ,i-var from 0 below (length ,v-var) by 2
                          do (,fn-var (aref ,v-var ,i-var) (aref ,v-var (1+ ,i-var)))))
                  (,visit-ring-var (,ring-var)
                    (cond
                      ((typep ,ring-var '(simple-array double-float (*)))
                       (,visit-vec-var ,ring-var))
                      ((consp ,ring-var)
                       (dolist (c ,ring-var)
                         (if (and (consp c) (numberp (first c)))
                             (,fn-var (%df (first c)) (%df (second c)))
                             (,visit-ring-var c)))))))
           (ecase (geometry-kind ,g-var)
             (:point
              (,fn-var (geometry-lon ,g-var) (geometry-lat ,g-var)))
             (:linestring
              (let ((,c-var (geometry-coordinates ,g-var)))
                (,visit-ring-var ,c-var)))
             (:polygon
              (let ((,c-var (geometry-coordinates ,g-var)))
                (dolist (,ring-var ,c-var)
                  (,visit-ring-var ,ring-var))))
             (:multipolygon
              (let ((,c-var (geometry-coordinates ,g-var)))
                (dolist (,poly-var ,c-var)
                  (dolist (,ring-var ,poly-var)
                    (,visit-ring-var ,ring-var)))))))))))


(declaim (inline map-geometry-coordinates))
(defun map-geometry-coordinates (fn g)
  "Call FN with (LON LAT) double-floats for every vertex in geometry G.
Note: Passing coordinates across a dynamic funcall boundary boxes double-floats (~32 bytes/vertex).
For zero-allocation vertex iteration, use DO-GEOMETRY-COORDINATES."
  (declare (type (or function symbol) fn))
  (do-geometry-coordinates (lon lat) g
    (funcall fn lon lat)))


(defun %vec->pairs (vec)
  "Convert a 1D double-float vector #(lon0 lat0 lon1 lat1 ...) to a list of (lon lat) double-float pairs."
  (if (typep vec '(simple-array double-float (*)))
      (loop for i from 0 below (length vec) by 2
            collect (list (aref vec i) (aref vec (1+ i))))
      (if (consp vec)
          (mapcar (lambda (c) (if (consp c) (list (%df (first c)) (%df (second c))) c)) vec)
          vec)))

(defun geometry-coordinate-pairs (g)
  "Return the coordinates of G as nested lists of (lon lat) double-float pairs.
Restores the classic pre-6e5e368 list-of-pairs structure for callers and exporters."
  (ecase (geometry-kind g)
    (:point
     (list (geometry-lon g) (geometry-lat g)))
    (:linestring
     (%vec->pairs (geometry-coordinates g)))
    (:polygon
     (mapcar #'%vec->pairs (geometry-coordinates g)))
    (:multipolygon
     (mapcar (lambda (p) (mapcar #'%vec->pairs p)) (geometry-coordinates g)))))


;;; -------------------------------------------------------------------------
;;; Serialization (reuses the generic byte protocol; no core dispatch changes)
;;; -------------------------------------------------------------------------

(defmethod serialize ((g geometry))
  (serialize-multiple +geometry+
                      (geometry-kind-code (geometry-kind g))
                      (geometry-coordinates g)))

(defmethod deserialize-help ((become (eql +geometry+)) (bytes array))
  (declare (type (array (unsigned-byte 8)) bytes))
  (let* ((parts (extract-all-subseqs bytes))
         (kind (geometry-code-kind (deserialize (first parts))))
         (raw-coords (deserialize (second parts))))
    (%make-geometry :kind kind
                    :coordinates (%normalize-coordinates kind raw-coords))))


(defmethod deserialize-help-mmap ((become (eql +geometry+)) (p mpointer)
                                  n-bytes header-length)
  (declare (ignore header-length))
  (deserialize-help +geometry+
                    (get-bytes (mpointer-mmap p) (mpointer-loc p) n-bytes)))
