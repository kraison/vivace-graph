;;;; Bridge between VivaceGraph `geometry' structs and GEOS geometries, via WKT.
;;;;
;;;; We own the geometry struct, so geometry->wkt is a pure, exact Lisp emitter
;;;; (full double-float precision, lon/lat = WKT x y with NO axis swap, explicit
;;;; ring closure).  GEOS parses/writes WKT through a per-context reader/writer.
;;;; wkt->geometry is a small parser used to bring GEOS results (e.g. makeValid
;;;; output) back into a VG geometry.  Coordinate-sequence construction is a
;;;; possible later optimization; WKT is the simplest correct path.

(in-package :graph-db)

(defparameter +wkt-whitespace+ '(#\Space #\Tab #\Newline #\Return))

;;; --------------------------------------------------------------------------
;;; geometry -> WKT  (pure)
;;; --------------------------------------------------------------------------

(defun %wkt-num (x)
  "Format X as a plain decimal preserving the double's value.  15 fractional
digits exceed a double's resolution across the lon/lat range, so reading the
string back yields the same double; trailing zeros are trimmed for tidiness."
  (let ((s (format nil "~,15F" (coerce x 'double-float))))
    (when (find #\. s)
      (setf s (string-right-trim "0" s))
      (when (char= (char s (1- (length s))) #\.)
        (setf s (concatenate 'string s "0"))))
    s))

(defun %coord->wkt (c)
  "C is a (lon lat) pair -> \"lon lat\"."
  (concatenate 'string (%wkt-num (first c)) " " (%wkt-num (second c))))

(defun %close-ring (ring)
  "Ensure RING's first and last vertices coincide (WKT requires closed rings)."
  (if (and ring (rest ring)
           (let ((f (first ring)) (l (car (last ring))))
             (and (= (first f) (first l)) (= (second f) (second l)))))
      ring
      (append ring (list (first ring)))))

(defun %ring->wkt (ring)
  (format nil "(~{~A~^, ~})" (mapcar #'%coord->wkt (%close-ring ring))))

(defun %polygon-body->wkt (rings)
  "RINGS = exterior + holes -> \"((ext), (hole), ...)\"."
  (format nil "(~{~A~^, ~})" (mapcar #'%ring->wkt rings)))

(defun geometry->wkt (g)
  "Emit the WKT string for geometry G (lon/lat axis order, full precision)."
  (ecase (geometry-kind g)
    (:point
     (let ((c (geometry-coordinates g)))
       (if c (format nil "POINT (~A)" (%coord->wkt c)) "POINT EMPTY")))
    (:linestring
     (let ((cs (geometry-coordinates g)))
       (if cs (format nil "LINESTRING (~{~A~^, ~})" (mapcar #'%coord->wkt cs))
           "LINESTRING EMPTY")))
    (:polygon
     (let ((rings (geometry-coordinates g)))
       (if rings (format nil "POLYGON ~A" (%polygon-body->wkt rings))
           "POLYGON EMPTY")))
    (:multipolygon
     (let ((polys (geometry-coordinates g)))
       (if polys
           (format nil "MULTIPOLYGON (~{~A~^, ~})"
                   (mapcar #'%polygon-body->wkt polys))
           "MULTIPOLYGON EMPTY")))))

;;; --------------------------------------------------------------------------
;;; WKT -> geometry  (pure, minimal parser)
;;; --------------------------------------------------------------------------

(defun %parse-coord-list (str)
  "Parse \"x y, x y, ...\" into a list of (lon lat) double-float pairs."
  (let ((*read-default-float-format* 'double-float))
    (loop for piece in (uiop:split-string str :separator ",")
          for trimmed = (string-trim +wkt-whitespace+ piece)
          unless (string= trimmed "")
            collect (let ((nums (remove "" (uiop:split-string trimmed :separator " ")
                                        :test #'string=)))
                      (list (coerce (read-from-string (first nums)) 'double-float)
                            (coerce (read-from-string (second nums)) 'double-float))))))

(defun %skip-ws (s i)
  (loop while (and (< i (length s)) (member (char s i) +wkt-whitespace+)) do (incf i))
  i)

(defun %parse-wkt-group (s i)
  "Parse a parenthesized group starting at S[I]=#\\( .  Returns (values node
next-index): a coordinate list when the group holds coordinates, else a list of
sub-groups (for nested POLYGON/MULTIPOLYGON structure)."
  (incf i)                              ; consume (
  (setf i (%skip-ws s i))
  (if (char= (char s i) #\()
      (let ((subs '()))                 ; group of groups
        (loop
          (setf i (%skip-ws s i))
          (multiple-value-bind (sub j) (%parse-wkt-group s i)
            (push sub subs) (setf i j))
          (setf i (%skip-ws s i))
          (cond ((char= (char s i) #\,) (incf i))
                ((char= (char s i) #\)) (incf i) (return))
                (t (error 'geos-error :message "Malformed WKT group"))))
        (values (nreverse subs) i))
      (let ((end (position #\) s :start i)))   ; leaf: coordinate list
        (unless end (error 'geos-error :message "Unterminated WKT group"))
        (values (%parse-coord-list (subseq s i end)) (1+ end)))))

(defun wkt->geometry (wkt)
  "Parse a WKT string (POINT/LINESTRING/POLYGON/MULTIPOLYGON, with EMPTY) into a
VG geometry.  Used to bring GEOS results back into VG.  Signals GEOS-ERROR on an
unsupported type."
  (let* ((s (string-trim +wkt-whitespace+ wkt))
         (paren (position #\( s))
         (kw (string-upcase (string-trim +wkt-whitespace+
                                         (subseq s 0 (or paren (length s)))))))
    (flet ((emptyp () (or (null paren) (search "EMPTY" (string-upcase s)))))
      (cond
        ((string= kw "POINT")
         (if (emptyp) (make-point 0d0 0d0)
             (let ((c (first (nth-value 0 (%parse-wkt-group s paren)))))
               (make-point (first c) (second c)))))
        ((string= kw "LINESTRING")
         (if (emptyp) (make-linestring '())
             (make-linestring (nth-value 0 (%parse-wkt-group s paren)))))
        ((string= kw "POLYGON")
         (if (emptyp) (make-polygon '())
             (make-polygon (nth-value 0 (%parse-wkt-group s paren)))))
        ((string= kw "MULTIPOLYGON")
         (if (emptyp) (make-multipolygon '())
             (make-multipolygon (nth-value 0 (%parse-wkt-group s paren)))))
        (t (error 'geos-error
                  :message (format nil "Unsupported WKT geometry type: ~A" kw)))))))

;;; --------------------------------------------------------------------------
;;; GEOS read/write wrappers + RAII
;;; --------------------------------------------------------------------------

(defun %read-wkt (ctx wkt)
  "Parse WKT into a GEOS geometry pointer (caller must destroy it)."
  (let ((g (%geos-wktreader-read (geos-ctx-handle ctx) (geos-ctx-reader ctx) wkt)))
    (when (cffi:null-pointer-p g)
      (error 'geos-error :message (or *geos-last-error*
                                      (format nil "GEOS could not parse WKT: ~A" wkt))))
    g))

(defun %write-wkt (ctx geom)
  "Write a GEOS geometry pointer to a Lisp WKT string (frees the C buffer)."
  (let ((cstr (%geos-wktwriter-write (geos-ctx-handle ctx) (geos-ctx-writer ctx) geom)))
    (when (cffi:null-pointer-p cstr)
      (error 'geos-error :message (or *geos-last-error* "GEOS WKT write failed")))
    (unwind-protect (cffi:foreign-string-to-lisp cstr)
      (%geos-free (geos-ctx-handle ctx) cstr))))

(defun %coords->geos-coordseq (ctx coords &key closed-p)
  "Convert a list of (lon lat) pairs into a GEOSCoordSeq pointer."
  (let* ((handle (geos-ctx-handle ctx))
         (n (length coords))
         (need-close (and closed-p
                          (> n 0)
                          (let ((f (first coords)) (l (car (last coords))))
                            (or (/= (first f) (first l)) (/= (second f) (second l))))))
         (len (+ n (if need-close 1 0)))
         (seq (%geos-coordseq-create handle len 2)))
    (when (cffi:null-pointer-p seq)
      (error 'geos-error :message "GEOSCoordSeq_create_r failed"))
    (loop for c in coords
          for idx from 0
          do (%geos-coordseq-setx handle seq idx (coerce (first c) 'double-float))
             (%geos-coordseq-sety handle seq idx (coerce (second c) 'double-float)))
    (when need-close
      (let ((f (first coords)))
        (%geos-coordseq-setx handle seq n (coerce (first f) 'double-float))
        (%geos-coordseq-sety handle seq n (coerce (second f) 'double-float))))
    seq))

(defun %ring->geos-linear-ring (ctx ring)
  "Convert a list of (lon lat) ring vertices into a GEOS LinearRing geometry pointer."
  (let ((handle (geos-ctx-handle ctx))
        (seq (%coords->geos-coordseq ctx ring :closed-p t)))
    (let ((geom (%geos-create-linear-ring handle seq)))
      (when (cffi:null-pointer-p geom)
        (%geos-coordseq-destroy handle seq)
        (error 'geos-error :message "GEOSGeom_createLinearRing_r failed"))
      geom)))

(defun %polygon-body->geos-polygon (ctx rings)
  "RINGS = (exterior . holes).  Returns a GEOS Polygon geometry pointer."
  (let* ((handle (geos-ctx-handle ctx))
         (shell (%ring->geos-linear-ring ctx (first rings)))
         (holes (rest rings))
         (nholes (length holes)))
    (if (zerop nholes)
        (let ((poly (%geos-create-polygon handle shell (cffi:null-pointer) 0)))
          (when (cffi:null-pointer-p poly)
            (%geos-geom-destroy handle shell)
            (error 'geos-error :message "GEOSGeom_createPolygon_r failed"))
          poly)
        (cffi:with-foreign-object (h-arr :pointer nholes)
          (let ((created-holes '()))
            (unwind-protect
                 (progn
                   (loop for h in holes
                         for idx from 0
                         for h-geom = (%ring->geos-linear-ring ctx h)
                         do (push h-geom created-holes)
                            (setf (cffi:mem-aref h-arr :pointer idx) h-geom))
                   (let ((poly (%geos-create-polygon handle shell h-arr nholes)))
                     (when (cffi:null-pointer-p poly)
                       (error 'geos-error :message "GEOSGeom_createPolygon_r failed"))
                     (setf shell nil created-holes nil)
                     poly))
              (when shell (%geos-geom-destroy handle shell))
              (dolist (h created-holes) (%geos-geom-destroy handle h))))))))

(defun geometry->geos (ctx g)
  "Build a GEOS geometry pointer directly from VG geometry G (caller destroys it)."
  (let ((handle (geos-ctx-handle ctx)))
    (ecase (geometry-kind g)
      (:point
       (let ((c (geometry-coordinates g)))
         (if c
             (let ((seq (%coords->geos-coordseq ctx (list c))))
               (let ((geom (%geos-create-point handle seq)))
                 (when (cffi:null-pointer-p geom)
                   (%geos-coordseq-destroy handle seq)
                   (error 'geos-error :message "GEOSGeom_createPoint_r failed"))
                 geom))
             (let ((seq (%geos-coordseq-create handle 0 2)))
               (%geos-create-point handle seq)))))
      (:linestring
       (let ((cs (geometry-coordinates g)))
         (if cs
             (let ((seq (%coords->geos-coordseq ctx cs)))
               (let ((geom (%geos-create-linestring handle seq)))
                 (when (cffi:null-pointer-p geom)
                   (%geos-coordseq-destroy handle seq)
                   (error 'geos-error :message "GEOSGeom_createLineString_r failed"))
                 geom))
             (let ((seq (%geos-coordseq-create handle 0 2)))
               (%geos-create-linestring handle seq)))))
      (:polygon
       (let ((rings (geometry-coordinates g)))
         (if rings
             (%polygon-body->geos-polygon ctx rings)
             (let ((shell (%geos-create-linear-ring handle (%geos-coordseq-create handle 0 2))))
               (%geos-create-polygon handle shell (cffi:null-pointer) 0)))))
      (:multipolygon
       (let* ((polys (geometry-coordinates g))
              (npolys (length polys)))
         (if (zerop npolys)
             (%geos-create-collection handle 6 (cffi:null-pointer) 0) ; GEOS_MULTIPOLYGON = 6
             (cffi:with-foreign-object (p-arr :pointer npolys)
               (let ((created-polys '()))
                 (unwind-protect
                      (progn
                        (loop for p in polys
                              for idx from 0
                              for p-geom = (%polygon-body->geos-polygon ctx p)
                              do (push p-geom created-polys)
                                 (setf (cffi:mem-aref p-arr :pointer idx) p-geom))
                        (let ((mpoly (%geos-create-collection handle 6 p-arr npolys)))
                          (when (cffi:null-pointer-p mpoly)
                            (error 'geos-error :message "GEOSGeom_createCollection_r failed"))
                          (setf created-polys nil)
                          mpoly))
                   (dolist (p created-polys) (%geos-geom-destroy handle p)))))))))))


(defun geos->geometry (ctx geom)
  "Convert a GEOS geometry pointer back into a VG geometry."
  (wkt->geometry (%write-wkt ctx geom)))

(defmacro with-geos-geom ((var ctx geometry) &body body)
  "Bind VAR to a GEOS geometry built from VG GEOMETRY for BODY; destroy it after."
  (let ((c (gensym "CTX")))
    `(let* ((,c ,ctx)
            (,var (geometry->geos ,c ,geometry)))
       (unwind-protect (progn ,@body)
         (%geos-geom-destroy (geos-ctx-handle ,c) ,var)))))

(defmacro with-geos-geoms (bindings &body body)
  "Nest WITH-GEOS-GEOM over BINDINGS (each (var ctx geometry))."
  (if (null bindings)
      `(progn ,@body)
      `(with-geos-geom ,(first bindings)
         (with-geos-geoms ,(rest bindings) ,@body))))
