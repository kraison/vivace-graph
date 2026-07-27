(in-package :graph-db)

;;; Geometry value type for the spatial extension.
;;;
;;; Part of VivaceGraph's public, general-purpose spatial layer (no domain
;;; knowledge lives here).  Coordinates are WGS84 and stored in (LON LAT) order
;;; -- GIS x,y / GeoJSON convention -- as DOUBLE-FLOATs.  KIND is one of :POINT
;;; :LINESTRING :POLYGON :MULTIPOLYGON, with COORDINATES shaped accordingly:
;;;   :point         -> (lon lat)
;;;   :linestring    -> ((lon lat) (lon lat) ...)
;;;   :polygon       -> (ring ...) where ring = ((lon lat) ...); the first ring
;;;                     is the exterior boundary, any others are holes
;;;   :multipolygon  -> (polygon ...) where polygon = (ring ...)
;;;
;;; The wire format reuses the generic byte protocol:
;;;   [+geometry+][len-header][serialized kind-code][serialized coordinates]
;;; built with SERIALIZE-MULTIPLE, so EXTRACT-LENGTH's variable-length branch
;;; decodes it with no change to the serialization dispatch core.  The payload
;;; encoding may later be compacted (e.g. a flat double-float array) WITHOUT
;;; changing the type tag, the struct, or this public API.

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
