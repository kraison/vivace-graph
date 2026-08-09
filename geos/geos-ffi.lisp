;;;; GEOS FFI: foreign-library definition, libgeos_c bindings, and soft loading.
;;;;
;;;; Part of the OPTIONAL `graph-db/geos' add-on -- the only place in the project
;;;; that touches libgeos.  Core graph-db never depends on this file.  Loading it
;;;; attempts to bind libgeos_c, but NEVER crashes the image: if the library is
;;;; absent, *geos-available-p* simply stays NIL and the spatial refine seam in
;;;; geometry-ops.lisp falls back to the dependency-free predicates.
;;;;
;;;; All bindings use the reentrant `_r' GEOS C API: every operation takes a
;;;; GEOSContextHandle_t first arg, and a handle must never be used by two
;;;; threads at once (see geos-context.lisp for the borrow/return pool).

(in-package :graph-db)

;;; --------------------------------------------------------------------------
;;; Foreign library
;;; --------------------------------------------------------------------------

;; Help CFFI find a Homebrew/MacPorts/Linux-packaged libgeos_c without the user
;; having to set DYLD_/LD_LIBRARY_PATH.  Guarded so we only add real dirs.
(eval-when (:load-toplevel :execute)
  (dolist (dir '("/opt/homebrew/lib" "/usr/local/lib" "/opt/local/lib"
                 "/usr/lib" "/usr/lib/x86_64-linux-gnu"))
    (when (probe-file dir)
      (pushnew (pathname (concatenate 'string dir "/"))
               cffi:*foreign-library-directories*
               :test #'equal))))

(cffi:define-foreign-library libgeos-c
  (:darwin (:or "libgeos_c.dylib" "libgeos_c.1.dylib"))
  (:unix (:or "libgeos_c.so.1" "libgeos_c.so"))
  (t (:default "libgeos_c")))

;;; --------------------------------------------------------------------------
;;; Version (non-reentrant; safe to call with no context)
;;; --------------------------------------------------------------------------

;; const char *GEOSversion(void);  e.g. "3.13.1-CAPI-1.19.2"
(cffi:defcfun ("GEOSversion" %geos-version) :string)

(defun %parse-geos-version (string)
  "Parse the leading dotted version (e.g. \"3.13.1\" from
\"3.13.1-CAPI-1.19.2\") into a list (major minor patch).  NIL on failure."
  (when (stringp string)
    (let* ((dash (position #\- string))
           (head (subseq string 0 (or dash (length string))))
           (parts (mapcar (lambda (s) (parse-integer s :junk-allowed t))
                          (uiop:split-string head :separator "."))))
      (when (and (first parts) (integerp (first parts)))
        (list (or (first parts) 0) (or (second parts) 0) (or (third parts) 0))))))

(defun %geos-version>= (version major minor)
  "True if VERSION (a (maj min patch) list) is at least MAJOR.MINOR."
  (and (consp version)
       (let ((vmaj (first version)) (vmin (second version)))
         (or (> vmaj major) (and (= vmaj major) (>= vmin minor))))))

;;; --------------------------------------------------------------------------
;;; Soft loading
;;; --------------------------------------------------------------------------

(defun load-geos ()
  "Attempt to load libgeos_c and record availability.  Returns T on success.
Never signals: on any failure it logs a warning, leaves *geos-available-p* NIL,
and the spatial seam uses its dependency-free fallbacks."
  (handler-case
      (progn
        (cffi:use-foreign-library libgeos-c)
        (let ((version (%parse-geos-version (%geos-version))))
          (setf *geos-version* version
                *geos-makevalid-available-p* (%geos-version>= version 3 8)
                *geos-available-p* t)
          ;; (Re)loading libgeos invalidates any GEOS context handles created
          ;; against the prior load, so discard the pool -- otherwise pooled
          ;; contexts created earlier corrupt when reused after a reload.  Guarded
          ;; with FBOUNDP because the initial LOAD-GEOS (at the bottom of this
          ;; file) runs before geos-context.lisp defines GEOS-SHUTDOWN; at that
          ;; point the pool is empty anyway, so skipping it is correct.
          (when (fboundp 'geos-shutdown) (funcall 'geos-shutdown))
          (log:info "GEOS loaded: version ~A (makeValid ~:[unavailable~;available~])"
                    version *geos-makevalid-available-p*)
          t))
    (error (e)
      (setf *geos-available-p* nil
            *geos-version* nil
            *geos-makevalid-available-p* nil)
      (log:warn "libgeos_c not available; spatial topology will use ~
dependency-free fallbacks. (~A)" e)
      nil)))

;;; --------------------------------------------------------------------------
;;; Reentrant (`_r') bindings
;;;
;;; Every call takes a GEOSContextHandle_t first; a handle must never be used by
;;; two threads concurrently (the context pool in geos-context.lisp enforces
;;; exclusive checkout).  Pointers are opaque :pointer; geometries returned by
;;; readers/ops must be freed with GEOSGeom_destroy_r, strings from writers with
;;; GEOSFree_r.
;;; --------------------------------------------------------------------------

;; Context lifecycle.  GEOSContextHandle_t GEOS_init_r(void);
(cffi:defcfun ("GEOS_init_r" %geos-init) :pointer)
(cffi:defcfun ("GEOS_finish_r" %geos-finish) :void (handle :pointer))

;; Message handlers.  typedef void (*GEOSMessageHandler_r)(const char*, void*);
;; (non-variadic -- CFFI can define the callback).  The setters return the prior
;; handler pointer, which we ignore.
(cffi:defcfun ("GEOSContext_setErrorMessageHandler_r" %geos-set-error-handler) :pointer
  (handle :pointer) (callback :pointer) (userdata :pointer))
(cffi:defcfun ("GEOSContext_setNoticeMessageHandler_r" %geos-set-notice-handler) :pointer
  (handle :pointer) (callback :pointer) (userdata :pointer))

;; WKT reader.
(cffi:defcfun ("GEOSWKTReader_create_r" %geos-wktreader-create) :pointer
  (handle :pointer))
(cffi:defcfun ("GEOSWKTReader_destroy_r" %geos-wktreader-destroy) :void
  (handle :pointer) (reader :pointer))
(cffi:defcfun ("GEOSWKTReader_read_r" %geos-wktreader-read) :pointer
  (handle :pointer) (reader :pointer) (wkt :string))

;; WKT writer.  write_r returns a malloc'd C string (free with GEOSFree_r).
(cffi:defcfun ("GEOSWKTWriter_create_r" %geos-wktwriter-create) :pointer
  (handle :pointer))
(cffi:defcfun ("GEOSWKTWriter_destroy_r" %geos-wktwriter-destroy) :void
  (handle :pointer) (writer :pointer))
(cffi:defcfun ("GEOSWKTWriter_write_r" %geos-wktwriter-write) :pointer
  (handle :pointer) (writer :pointer) (geom :pointer))
;; Keep full precision on output (don't trim/round -- it would corrupt round-trips).
(cffi:defcfun ("GEOSWKTWriter_setTrim_r" %geos-wktwriter-set-trim) :void
  (handle :pointer) (writer :pointer) (trim :char))

;; Geometry + buffer freeing.
(cffi:defcfun ("GEOSGeom_destroy_r" %geos-geom-destroy) :void
  (handle :pointer) (geom :pointer))
(cffi:defcfun ("GEOSFree_r" %geos-free) :void
  (handle :pointer) (buffer :pointer))

;; Binary predicates: return char 1 (true) / 0 (false) / 2 (exception).
(cffi:defcfun ("GEOSIntersects_r" %geos-intersects) :char
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSContains_r" %geos-contains) :char
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSWithin_r" %geos-within) :char
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSCovers_r" %geos-covers) :char
  (handle :pointer) (g1 :pointer) (g2 :pointer))

;; Validity: char 1 valid / 0 invalid / 2 exception.
(cffi:defcfun ("GEOSisValid_r" %geos-is-valid) :char
  (handle :pointer) (geom :pointer))
;; GEOSMakeValid_r (GEOS >= 3.8): returns a new, valid geometry (or NULL).
(cffi:defcfun ("GEOSMakeValid_r" %geos-make-valid) :pointer
  (handle :pointer) (geom :pointer))
;; int GEOSDistance_r(handle, g1, g2, double *out);  1 ok / 0 exception.
;; PLANAR distance in coordinate units (degrees for lon/lat -- NOT metres).
(cffi:defcfun ("GEOSDistance_r" %geos-distance) :int
  (handle :pointer) (g1 :pointer) (g2 :pointer) (out :pointer))

;; Constructive (overlay) ops: each returns a NEW geometry (or NULL).
(cffi:defcfun ("GEOSUnion_r" %geos-union) :pointer
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSIntersection_r" %geos-intersection) :pointer
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSDifference_r" %geos-difference) :pointer
  (handle :pointer) (g1 :pointer) (g2 :pointer))
;; GEOSBuffer_r(handle, g, width, quadsegs): width in coordinate units (degrees).
(cffi:defcfun ("GEOSBuffer_r" %geos-buffer) :pointer
  (handle :pointer) (geom :pointer) (width :double) (quadsegs :int))
;; int GEOSArea_r(handle, g, double *out);  area in squared coordinate units.
(cffi:defcfun ("GEOSArea_r" %geos-area) :int
  (handle :pointer) (geom :pointer) (out :pointer))

;; Nearest points: a 2-point coordinate sequence (closest pair), or NULL.
(cffi:defcfun ("GEOSNearestPoints_r" %geos-nearest-points) :pointer
  (handle :pointer) (g1 :pointer) (g2 :pointer))
(cffi:defcfun ("GEOSCoordSeq_getX_r" %geos-coordseq-getx) :int
  (handle :pointer) (seq :pointer) (idx :unsigned-int) (out :pointer))
(cffi:defcfun ("GEOSCoordSeq_getY_r" %geos-coordseq-gety) :int
  (handle :pointer) (seq :pointer) (idx :unsigned-int) (out :pointer))
(cffi:defcfun ("GEOSCoordSeq_destroy_r" %geos-coordseq-destroy) :void
  (handle :pointer) (seq :pointer))

;; Coordinate sequence creation and setting.
(cffi:defcfun ("GEOSCoordSeq_create_r" %geos-coordseq-create) :pointer
  (handle :pointer) (size :unsigned-int) (dims :unsigned-int))
(cffi:defcfun ("GEOSCoordSeq_setX_r" %geos-coordseq-setx) :int
  (handle :pointer) (seq :pointer) (idx :unsigned-int) (val :double))
(cffi:defcfun ("GEOSCoordSeq_setY_r" %geos-coordseq-sety) :int
  (handle :pointer) (seq :pointer) (idx :unsigned-int) (val :double))

;; Direct geometry constructors.
(cffi:defcfun ("GEOSGeom_createPoint_r" %geos-create-point) :pointer
  (handle :pointer) (seq :pointer))
(cffi:defcfun ("GEOSGeom_createLineString_r" %geos-create-linestring) :pointer
  (handle :pointer) (seq :pointer))
(cffi:defcfun ("GEOSGeom_createLinearRing_r" %geos-create-linear-ring) :pointer
  (handle :pointer) (seq :pointer))
(cffi:defcfun ("GEOSGeom_createPolygon_r" %geos-create-polygon) :pointer
  (handle :pointer) (shell :pointer) (holes :pointer) (nholes :unsigned-int))
(cffi:defcfun ("GEOSGeom_createCollection_r" %geos-create-collection) :pointer
  (handle :pointer) (type :int) (geoms :pointer) (ngeoms :unsigned-int))


;; Attempt the load when this add-on system is loaded.  Inert for core graph-db,
;; which never loads this file.
(eval-when (:load-toplevel :execute)
  (load-geos))
