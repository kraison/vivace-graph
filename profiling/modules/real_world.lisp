;;;; Real-World Cross-Subsystem Profiler Module for Vivace-Graph
;;;;
;;;; These workloads are modeled on a MEASURED production dataset of a
;;;; downstream application team; the counts reflect measured production scale:
;;;;
;;;;   parcels        481   geometry: 451 polygon, 24 multipolygon, 4 point
;;;;   parcel rings         outer ring: min 5, median 13, p90 38, max 176
;;;;   items        9,699   geometry: 100% POINT
;;;;   items/parcel   ~20   (9,699 / 481)
;;;;   passes         467
;;;;   stations       481
;;;;   markers      1,503
;;;;   map payload  ~514 KB GeoJSON for a country-wide viewport query
;;;;
;;;; Two corrections this data forced on the previous version of this file:
;;;;
;;;;   1. The old workloads built their data almost entirely from MAKE-POINT.
;;;;      Production geometry is dominated by POLYGONS, and that app's hot read
;;;;      path walks them via GEOMETRY-COORDINATE-PAIRS to emit GeoJSON.  A
;;;;      point-only corpus cannot exercise the packed double-float coordinate
;;;;      path at all -- which is precisely the code that issues #79/#81/#82/#83
;;;;      rewrote.
;;;;
;;;;   2. The downstream app uses the Prolog engine EXACTLY ZERO times
;;;;      (verified by grep across its sources).  The Prolog workload's claim
;;;;      to model domain rule inference was fiction.  It is kept here because
;;;;      Prolog is a real graph-db subsystem worth profiling, but it is now
;;;;      labelled as a synthetic engine workload, not an application one.
;;;;
;;;; Conversely, GEOS topology (make-valid / union / intersection / difference)
;;;; is used heavily by the app -- tile clipping, area rollups, remainder
;;;; computation -- and had no workload at all.  Workload 7 adds it.
(in-package #:graph-db/profiler)

(defstruct realworld-workload-result
  (name "" :type string)
  (description "" :type string)
  (target-subsystems '() :type list)
  (code-sample "" :type string)
  (run-result nil))

;;; ---------------------------------------------------------------------------
;;; Measured production shape
;;; ---------------------------------------------------------------------------

(defparameter *rw-parcel-ring-vertices* 13
  "Median outer-ring vertex count of a production parcel boundary (measured).")

(defparameter *rw-parcel-ring-p90-vertices* 38
  "p90 outer-ring vertex count -- used for the tail of the generated corpus.")

(defparameter *rw-items-per-parcel* 20
  "Measured mean items per parcel (9,699 / 481).")

(defparameter *rw-large-zone-vertices* 2200
  "Vertex count of a country-scale control polygon.  The downstream app
holds control-zone boundaries at roughly this size; they are the documented
worst case for whole-record node materialization (issue #50) and the reason
GEOMETRY-COORDINATE-PAIRS deserialization was optimised in #81/#82.")

;;; --- Event-feed + retrieval-corpus shape (measured in production) -----------
;;;
;;; These are the SLOWEST paths in the application.  Measured end-to-end against
;;; production before these workloads were written:
;;;
;;;   zone-history profile (one pin)   1,125 ms   <-- dominates the page
;;;   geo-scope query r=5 km            169 ms    (462 events)
;;;   geo-scope query r=25 km           195 ms    (812 events)
;;;
;;; Event graph:      286,198 event rows
;;;                     6,828 control zones  (all MULTIPOLYGON;
;;;                                           median 1,480 vertices, max 4,111)
;;;                     1,707 daily snapshots  (~4 zones per day)
;;; Retrieval corpus:  23,193 chunk vectors, dimension 1,024, single-float

(defparameter *rw-control-zone-vertices* 1480
  "Median vertex count of a production control zone (measured; max 4,111).")

(defparameter *rw-control-zones-per-day* 4
  "Zones in a daily snapshot (6,828 zones / 1,707 snapshots).")

(defparameter *rw-control-window-days* 1500
  "Days walked by the app's zone-history profile for a full-history pin.

The walk is DATE-driven, not spatial-index-driven, deliberately: a day holds
about four country-scale polygons, so an index lookup would add overhead for
zero selectivity.  Cost is therefore days x zones-per-day node materializations
plus the same number of point-in-polygon tests.")

(defparameter *rw-pin-radius-m* 5000.0d0
  "Radius of a dropped-pin event query -- a 10 km diameter circle.")

(defparameter *rw-kb-vector-dim* 1024
  "Corpus embedding dimension (measured from the segment header).")

;;; ---------------------------------------------------------------------------
;;; Geometry generators
;;; ---------------------------------------------------------------------------

(defun %rw-ring (center-lon center-lat radius n)
  "A closed ring of N distinct vertices approximating a circle.
Returns a list of (lon lat) pairs with the first vertex repeated last, which is
the shape MAKE-POLYGON expects and what real parcel boundaries look like."
  (let ((pts '()))
    (dotimes (i n)
      (let ((theta (* 2.0d0 pi (/ (float i 1.0d0) n))))
        (push (list (+ center-lon (* radius (cos theta)))
                    (+ center-lat (* radius (sin theta))))
              pts)))
    (setf pts (nreverse pts))
    (append pts (list (copy-list (first pts))))))

(defun %rw-parcel-polygon (i)
  "A parcel boundary polygon at production scale.
Every 10th parcel gets the p90 vertex count so the corpus has a realistic tail
rather than a uniform shape."
  (let* ((n (if (zerop (mod i 10))
                *rw-parcel-ring-p90-vertices*
                *rw-parcel-ring-vertices*))
         (lon (+ 11.0d0 (* 0.01d0 (mod i 100))))
         (lat (+ 45.0d0 (* 0.01d0 (floor i 100)))))
    (graph-db:make-polygon (list (%rw-ring lon lat 0.004d0 n)))))

(defun %rw-large-zone-polygon (i)
  "A country-scale control polygon (~2,200 vertices)."
  (graph-db:make-polygon
   (list (%rw-ring (+ 5.0d0 (* 0.5d0 i)) 45.0d0 2.5d0 *rw-large-zone-vertices*))))

(defun %rw-item-point (i j)
  "An item point inside parcel I's footprint.
Production items are 100% points."
  (graph-db:make-point (+ 11.0d0 (* 0.01d0 (mod i 100)) (* 0.0002d0 (mod j 17)))
                       (+ 45.0d0 (* 0.01d0 (floor i 100)) (* 0.0002d0 (mod j 13)))))

(defun %rw-ds-zone-multipolygon (i)
  "A production-scale control zone: a MULTIPOLYGON of two country-scale parts
totalling roughly *RW-CONTROL-ZONE-VERTICES* vertices, matching the measured
production shape (all zones are multipolygons; median 1,480 vertices)."
  (let ((half (max 4 (floor *rw-control-zone-vertices* 2))))
    (graph-db:make-multipolygon
     (list (list (%rw-ring (+ 6.0d0 (* 0.35d0 i)) 44.5d0 2.2d0 half))
           (list (%rw-ring (+ 8.5d0 (* 0.35d0 i)) 45.5d0 1.6d0 half))))))

(defun %rw-ds-date (day)
  "A YYYY-MM-DD string DAY days after an arbitrary epoch, without pulling in a
date library."
  (multiple-value-bind (y m d)
      ;; Deliberately naive 30-day months: the profile walk only needs distinct,
      ;; ordered, correctly-shaped date keys, not a real calendar.
      (let* ((total day)
             (y (+ 2020 (floor total 360)))
             (rem (mod total 360)))
        (values y (1+ (floor rem 30)) (1+ (mod rem 30))))
    (format nil "~4,'0D-~2,'0D-~2,'0D" y m d)))

(defun %rw-kb-vector (i dim)
  "A conforming embedding: vector segments store (simple-array single-float (*))."
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (j dim v)
      (setf (aref v j) (float (/ (mod (+ (* i 31) (* j 17)) 1000) 1000.0) 1.0f0)))))

(defun %rw-geos-available-p ()
  "True when the optional GRAPH-DB/GEOS layer is loaded AND libgeos_c resolved.
Looked up by name so this file still compiles in an image without the add-on."
  (let ((s (find-symbol "GEOS-AVAILABLE-P" "GRAPH-DB")))
    (and s (fboundp s) (ignore-errors (funcall s)) t)))

;;; ---------------------------------------------------------------------------
;;; Schema -- mirrors the downstream app's production graph shape
;;; ---------------------------------------------------------------------------

(graph-db:def-vertex rw-node ()
  ((name :type string)
   (item-key :type string)
   (centroid :type geometry :index t)
   (geom :type geometry :index t)
   (value)
   (label))
  :rw-graph)

(graph-db:def-edge rw-link ()
  ((label))
  :rw-graph)

;; Parcel: an operator-flown area.  BOUNDARY is the polygon the map renders.
;;
;; Both CENTROID and BOUNDARY are declared :INDEX T deliberately, because
;; the downstream app's corresponding class declares exactly that pair, as does
;; its station class (centroid + extent).  The engine indexes only the FIRST
;; geometry-valued slot and warns that the rest are INERT, so in production the
;; spatial index is keyed on parcel CENTROIDS, not on boundary polygons.
;;
;; That is reproduced rather than "fixed" here on purpose: a workload that
;; indexed the polygon would measure an index the application does not actually
;; have.  When reading Workload 2, remember the bbox stage resolves candidates
;; by centroid and the polygon work happens afterwards, during refinement and
;; GeoJSON emission.
(graph-db:def-vertex rw-parcel ()
  ((parcel-key :type string)
   (name :type string)
   (parcel-date :type string)
   (source :type string)
   (centroid :type geometry :index t)
   (boundary :type geometry :index t))
  :rw-graph)

;; Item: always a point in production.
(graph-db:def-vertex rw-item ()
  ((item-key :type string)
   (item-family :type string)
   (confidence :type string)
   (geom :type geometry :index t))
  :rw-graph)

;; Station: the authored container a parcel may be claimed into.
(graph-db:def-vertex rw-station ()
  ((name :type string)
   (source :type string)
   (centroid :type geometry :index t)
   (extent :type geometry :index t))
  :rw-graph)

;; Control zone: a country-scale polygon plus one small scalar.
;;
;; BOUNDARY is deliberately NOT :index t.  The app's zone class does the
;; same thing on purpose -- a country-scale polygon must never become a spatial
;; index candidate for point queries.  It also isolates workload 8: the cost
;; measured there is whole-record materialization, not spatial indexing.
(graph-db:def-vertex rw-zone ()
  ((zone-name :type string)
   (control :type string)
   (boundary :type geometry))
  :rw-graph)

;; Snapshot control zone: a country-scale MULTIPOLYGON plus one small scalar.
;;
;; EXTENT is deliberately NOT :index t, mirroring the app's zone class:
;; a country-scale polygon must never become a candidate for point queries, and
;; the control-profile walk never consults the spatial index anyway.
(graph-db:def-vertex rw-ds-zone ()
  ((zone-state :type string)
   (zone-date :type string)
   (extent :type geometry))
  :rw-graph)

;; Event row: a point plus the scalars the filters test.
(graph-db:def-vertex rw-event ()
  ((event-id :type string)
   (event-date :type string)
   (event-type :type string)
   (ev-lat :type double-float)
   (ev-lon :type double-float)
   (geom :type geometry :index t))
  :rw-graph)

;; Corpus chunk: text plus a 1024-dimension embedding in a vector segment.
(graph-db:def-vertex rw-kb-chunk ()
  ((chunk-text :type string)
   (doc-id :type string)
   (embedding :vector-index t))
  :rw-graph)

(graph-db:def-vertex rw-complex-node ()
  ((name :type string)
   (report-text :type string)
   (embedding :type vector)
   (float-coords :type vector)
   (metadata :type list)
   (status :type string)
   (score :type double-float)
   (centroid :type geometry :index t))
  :rw-graph)

;;; ---------------------------------------------------------------------------
;;; Shared scaffolding
;;; ---------------------------------------------------------------------------

(defmacro with-rw-graph ((graph-var dir) &body body)
  "Bind GRAPH-VAR to a fresh :RW-GRAPH at DIR, run BODY, then close and delete.
Always closes with :SNAPSHOT-P NIL -- a snapshot would add unrelated I/O to the
measurement, and the graph is discarded immediately afterwards anyway."
  (let ((d (gensym "DIR")))
    `(let* ((,d ,dir)
            (,graph-var (progn
                          (ignore-errors
                           (uiop:delete-directory-tree ,d :validate t
                                                          :if-does-not-exist :ignore))
                          (graph-db:make-graph :rw-graph ,d))))
       (unwind-protect (progn ,@body)
         #+sbcl (ignore-errors (sb-profile:unprofile))
         (ignore-errors
          (let ((graph-db:*graph* ,graph-var))
            (graph-db:close-graph ,graph-var :snapshot-p nil)))
         (ignore-errors
          (uiop:delete-directory-tree ,d :validate t :if-does-not-exist :ignore))))))

(defun %rw-populate-parcels
    (graph n &key (items-per-parcel *rw-items-per-parcel*))
  "Build N parcels with production-shaped polygon boundaries, each linked to
ITEMS-PER-PARCEL point items.  Returns the list of parcel vertices."
  (let ((graph-db:*graph* graph)
        (parcels '()))
    (graph-db:with-transaction ()
      (dotimes (i n)
        (let ((sv (make-rw-parcel
                   :parcel-key (format nil "sp-~D" i)
                   :name (format nil "Parcel ~D" i)
                   :parcel-date "20241108"
                   :source "safepro"
                   :centroid (graph-db:make-point
                              (+ 11.0d0 (* 0.01d0 (mod i 100)))
                              (+ 45.0d0 (* 0.01d0 (floor i 100))))
                   :boundary (%rw-parcel-polygon i))))
          (push sv parcels)
          (dotimes (j items-per-parcel)
            (let ((fd (make-rw-item
                       :item-key (format nil "sp-~D|item-~D" i j)
                       :item-family (if (evenp j) "TYPE-A" "TYPE-B")
                       :confidence (if (zerop (mod j 3)) "Confirmed" "Suspected")
                       :geom (%rw-item-point i j))))
              (make-rw-link :from sv :to fd))))))
    (nreverse parcels)))

;;; --- Workload 1: Parcel/Item Bulk Ingestion Pipeline ---
(defun profile-realworld-ingestion-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile end-to-end bulk ingestion at the measured parcel:item ratio."
  (let* ((count (max 1 (floor (* scale 200))))
         (run-res nil)
         (code-sample "(graph-db:with-transaction ()
  (dotimes (i count)
    (let ((sv (make-rw-parcel :parcel-key (format nil \"sp-~D\" i)
                              :centroid (graph-db:make-point lon lat)
                              ;; median 13-vertex boundary, p90 38 every 10th
                              :boundary (%rw-parcel-polygon i))))
      (dotimes (j 20)                       ; measured items-per-parcel
        (make-rw-link :from sv
                      :to (make-rw-item
                           :item-key (format nil \"sp-~D|item-~D\" i j)
                                        :geom (%rw-item-point i j)))))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-ingest/")
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 1: ~
                                   Parcel/Item Bulk Ingestion Pipeline ~
                                   (~:D parcels, ~:D items)"
                                          count (* count *rw-items-per-parcel*))
                            :subsystems (or subsystems '(:mmap-storage :graph-storage :transactions
                                          :spatial :views :serialization))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (%rw-populate-parcels graph count))))
    (make-realworld-workload-result
     :name "Workload 1: Parcel/Item Bulk Ingestion Pipeline"
     :description
     (concatenate 'string
      "Bulk ingestion at the measured production ratio: each parcel carries a "
      "production-shaped boundary polygon (median 13 vertices, p90 38) and ~20 "
      "point items joined by edges, all inside one ACID transaction with "
      "automatic geohash spatial indexing.")
     :target-subsystems '(:mmap-storage :graph-storage :transactions :spatial :views :serialization)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 2: Map Viewport Query (the app's hottest read path) ---
(defun profile-realworld-spatial-traversal-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile the map viewport read path: bbox spatial query -> node materialization
-> polygon coordinate walk for GeoJSON emission.

This mirrors the downstream app's viewport GeoJSON query, which produces
~514 KB of GeoJSON for a country-wide viewport.  The coordinate walk is
the part that a
point-only corpus could never exercise."
  (let* ((count (max 1 (floor (* scale 200))))
         (query-iters (max 1 (floor (* scale 50))))
         (run-res nil)
         (code-sample "(dotimes (k query-iters)
  (let ((hits (graph-db:find-nodes-intersecting 'rw-parcel viewport
                                                :graph graph)))
    (dolist (sv hits)
      ;; materialize + walk the packed coordinates, exactly as the GeoJSON
      ;; encoder does
      (let ((rings (graph-db:geometry-coordinate-pairs (slot-value sv 'boundary))))
        (dolist (ring rings) (length ring)))
      (graph-db:map-edges (lambda (e) (graph-db:lookup-vertex (graph-db:to e) :graph graph))
                          graph :vertex sv :direction :out))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-spatial/")
      (%rw-populate-parcels graph count)
      (let ((viewport (graph-db:make-polygon
                       (list (list (list 10.9d0 44.9d0) (list 12.6d0 44.9d0)
                                   (list 12.6d0 46.6d0) (list 10.9d0 46.6d0)
                                   (list 10.9d0 44.9d0))))))
        (setf run-res
              (profile-block (:name (format nil "Real-World Workload 2: Spatial Map Viewport Query (~:D viewport queries)" query-iters)
                              :subsystems (or subsystems '(:spatial :graph-storage :index-backends
                                            :serialization :mmap-storage))
                              :sprof-mode sprof-mode
                              :top-n 30)
                (let ((graph-db:*graph* graph)
                      (vertices 0))
                  (dotimes (k query-iters vertices)
                    (let ((hits (graph-db:find-nodes-intersecting
                                 'rw-parcel viewport :graph graph)))
                      (dolist (sv hits)
                        ;; The GeoJSON emission path: walk every coordinate pair.
                        (let ((g (ignore-errors (slot-value sv 'boundary))))
                          (when g
                            (dolist (ring (graph-db:geometry-coordinate-pairs g))
                              (incf vertices (length ring)))))
                        ;; Item-listing path: traverse to this parcel's items.
                        (graph-db:map-edges
                         (lambda (e) (graph-db:lookup-vertex (graph-db:to e) :graph graph))
                         graph :vertex sv :direction :out)))))))))
    (make-realworld-workload-result
     :name "Workload 2: Spatial Map Viewport Query"
     :description
     (concatenate 'string
      "The application's hottest read path: a viewport bounding-box query "
      "resolves candidate parcels, materializes each one, walks its packed "
      "double-float boundary coordinates as the GeoJSON encoder does, and "
      "traverses out-edges to its items. Exercises the spatial index, node "
      "materialization, the packed-coordinate accessor and edge traversal "
      "together.")
     :target-subsystems '(:spatial :graph-storage :index-backends :serialization :mmap-storage)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 3: Analytical View Rollup ---
(defun profile-realworld-view-rollup-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile secondary-index view materialization and map/reduce analytical rollups.
The app declares 27 views; rollups back the station and parcel summary panels."
  (let* ((count (max 1 (floor (* scale 500))))
         (run-res nil)
         (code-sample "(graph-db:def-view rw-item-by-family :lessp
    (rw-item :rw-graph)
  (:map (lambda (n)
          (let ((k (slot-value n 'item-family))) (when k (yield k 1))))))
(graph-db:install-views graph)
(dotimes (_ 10)
  (graph-db:invoke-graph-view 'rw-item 'rw-item-by-family :graph graph))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-views/")
      (let ((graph-db:*graph* graph))
        (graph-db:with-transaction ()
          (dotimes (i count)
            (make-rw-item :item-key (format nil "sp-~D|item-~D" (mod i 50) i)
                          :item-family (if (evenp i) "TYPE-A" "TYPE-B")
                          :confidence (if (zerop (mod i 3)) "Confirmed" "Suspected")
                          :geom (%rw-item-point (mod i 50) i)))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 3: ~
                                   Analytical View Rollup (~:D items)"
                                  count)
                            :subsystems (or subsystems '(:views :serialization :index-backends :mmap-storage))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph))
                (graph-db:def-view rw-item-by-family :lessp (rw-item :rw-graph)
                  (:map (lambda (n)
                          (let ((k (slot-value n 'item-family)))
                            (when k (graph-db:yield k 1))))))
                (graph-db:install-views graph)
                (dotimes (_ 10)
                  (graph-db:invoke-graph-view 'rw-item 'rw-item-by-family
                                              :graph graph))))))
    (make-realworld-workload-result
     :name "Workload 3: Analytical View Rollup"
     :description
     (concatenate 'string
      "Secondary-index view installation, tuple sorting, key (de)serialization "
      "and live map/reduce aggregation over items grouped by item family -- "
      "the shape behind the station and parcel summary panels.")
     :target-subsystems '(:views :serialization :index-backends :mmap-storage)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 4: Prolog engine (SYNTHETIC -- not an application workload) ---
(defun profile-realworld-prolog-inference-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile Prolog rule compilation, unification and term dereferencing.

NOT an application workload: the downstream app does not use the Prolog engine
at all (zero call sites in its sources).  Retained because Prolog
is a genuine graph-db subsystem; do not read its numbers as
application-representative."
  (let* ((count (max 1 (floor (* scale 3000))))
         (run-res nil)
         (code-sample "(graph-db::unify '(?x 1 2) '(?x 1 2))
(graph-db::deref-exp '?x)
(graph-db::prolog-compile (graph-db::make-functor :name ...))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-prolog/")
      (let ((graph-db:*graph* graph))
        (dotimes (i (max 1 (floor (* scale 500))))
          (make-rw-node :item-key (format nil "sp-~D|item-1" i))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 4: Prolog Engine, synthetic (~:D queries)" count)
                            :subsystems (or subsystems '(:prolog :graph-storage :index-backends))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph))
                (dotimes (i count)
                  (graph-db::unify '(graph-db::?x 1 2) '(graph-db::?x 1 2))
                  (graph-db::deref-exp 'graph-db::?x)
                  (let ((f (graph-db::make-functor
                            :name (graph-db::make-functor-symbol
                                   (format nil "prof-pred-~D" (mod i 50)) 1))))
                    (graph-db::prolog-compile f)))))))
    (make-realworld-workload-result
     :name "Workload 4: Prolog Engine (synthetic)"
     :description
     (concatenate 'string
      "Prolog predicate unification, term dereferencing and rule compilation. "
      "SYNTHETIC: the downstream app makes no use of the Prolog engine, so "
      "unlike the other workloads this one is not derived from production "
      "behaviour and should not be treated as application-representative.")
     :target-subsystems '(:prolog :graph-storage :index-backends)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 5: Concurrent Field-Operator Transactions ---
(defun profile-realworld-concurrent-transactions-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile many small write transactions and OCC validation.
Models field operators claiming parcels and editing station records: many short
transactions rather than one bulk load."
  (let* ((tx-count (max 1 (floor (* scale 500))))
         (run-res nil)
         (code-sample "(dotimes (i tx-count)
  (graph-db:with-transaction ()
    (make-rw-station :name (format nil \"Site-~D\" i)
                  :source \"authored\"
                  :extent (%rw-parcel-polygon i))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-tx/")
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 5: Concurrent Field-Operator Transactions (~:D txns)" tx-count)
                            :subsystems (or subsystems '(:transactions :mmap-storage :graph-storage
                                          :index-backends :spatial))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph))
                (dotimes (i tx-count)
                  (graph-db:with-transaction ()
                    (make-rw-station :name (format nil "Station-~D" i)
                                  :source "authored"
                                  :centroid (graph-db:make-point
                                             (+ 11.0d0 (* 0.01d0 (mod i 100)))
                                             (+ 45.0d0 (* 0.01d0 (floor i 100))))
                                  :extent (%rw-parcel-polygon i))))))))
    (make-realworld-workload-result
     :name "Workload 5: Concurrent Field-Operator Transactions"
     :description
     (concatenate 'string
      "Many short write transactions creating authored station records with "
      "polygon extents, as field operators do when claiming parcels and "
      "editing stations. Exercises OCC read-set/write-set validation, "
      "per-transaction log records and index page locking.")
     :target-subsystems '(:transactions :mmap-storage :graph-storage :index-backends :spatial)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 6: Complex Node Serialization & Deserialization ---
(defun profile-realworld-complex-serialization-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile large complex node creation, binary serialization, mmap writing, and
cache-bypassed deserialization.  Models a field report record: long narrative
text plus an embedding vector plus geometry."
  (let* ((count (max 1 (floor (* scale 300))))
         (read-passes 5)
         (text-sample (make-string 25000 :initial-element #\X))
         (float-vec (make-array 512 :element-type 'double-float
                                    :initial-element 3.141592653589793d0))
         (coords-vec (make-array 256 :element-type 'single-float :initial-element 1.4142f0))
         (node-ids '())
         (run-res nil)
         (code-sample "(graph-db:with-transaction ()
  (dotimes (i count)
    (make-rw-complex-node :report-text (make-string 25000)
                          :embedding (make-array 512 :element-type 'double-float)
                          :float-coords (make-array 256 :element-type 'single-float)
                          :centroid (graph-db:make-point lon lat))))
;; deserialization across 5 passes, cache bypassed
(dotimes (_ 5)
  (let ((graph-db::*cache-enabled* nil))
    (dolist (id node-ids) (graph-db:lookup-vertex id :graph graph))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-complex/")
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 6: Complex Node Serialization & Deserialization (~:D nodes, 25KB text, 512 floats, ~D passes)" count read-passes)
                            :subsystems (or subsystems '(:serialization :graph-core :mmap-storage :transactions))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph))
                (graph-db:with-transaction ()
                  (dotimes (i count)
                    (let ((n (make-rw-complex-node
                              :name (format nil "Complex-Station-~D" i)
                              :report-text text-sample
                              :embedding float-vec
                              :float-coords coords-vec
                              :metadata
                              `((:priority . "CRITICAL") (:sector . ,i)
                                          (:inspector . "Unit-9"))
                              :status "ACTIVE"
                              :score (+ 100.0d0 i)
                              :centroid (graph-db:make-point
                                         (+ 11.0d0 (/ i 100.0d0))
                                         (+ 45.0d0 (/ i 100.0d0))))))
                      (push (graph-db:id n) node-ids))))
                (dotimes (_ read-passes)
                  (let ((graph-db::*cache-enabled* nil))
                    (dolist (id node-ids)
                      (graph-db:lookup-vertex id :graph graph))))))))
    (make-realworld-workload-result
     :name "Workload 6: Complex Node Serialization & Deserialization"
     :description
     (concatenate 'string
      "Large complex vertex creation, multi-slot binary serialization (25KB "
      "narrative text, 512-element double-float embedding), mmap heap writing, "
      "and cache-bypassed deserialization with CLOS instantiation.")
     :target-subsystems '(:serialization :graph-core :mmap-storage :transactions)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 7: GEOS Coverage Remainder (NEW) ---
(defun profile-realworld-geos-coverage-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile the GEOS topology path behind coverage analysis.

The downstream app calls GEOMETRY-MAKE-VALID, -UNION, -INTERSECTION,
-DIFFERENCE and -CONTAINS across tile clipping, area rollups and remainder
computation.  None of it had any profiler coverage before."
  (let* ((count (max 2 (floor (* scale 60))))
         (run-res nil)
         (code-sample "(let* ((valid (mapcar #'graph-db:geometry-make-valid
                      parcel-polys))
       (covered (reduce #'graph-db:geometry-union valid))
       (remaining (graph-db:geometry-difference station-extent covered)))
  (graph-db:geometry-area remaining))"))
    ;; No graph is needed: GEOS topology operates on geometry values directly.
    (when (%rw-geos-available-p)
      (let ((polys (loop for i from 0 below count
                         collect (%rw-parcel-polygon i)))
            (extent (graph-db:make-polygon (list (%rw-ring 11.5d0 45.5d0 1.2d0 64)))))
        (setf run-res
              (profile-block (:name (format nil "Real-World Workload 7: ~
                                     GEOS Coverage Remainder (~:D polygons)"
                                    count)
                              :subsystems (or subsystems '(:geos :spatial :graph-core))
                              :sprof-mode sprof-mode
                              :top-n 30)
                (let* ((valid (mapcar (lambda (p)
                                        (or (ignore-errors (graph-db:geometry-make-valid p)) p))
                                      polys))
                       (covered (reduce (lambda (a b)
                                          (or (ignore-errors (graph-db:geometry-union a b)) a))
                                        valid))
                       (remaining (ignore-errors
                                   (graph-db:geometry-difference extent covered))))
                  ;; The remainder answer: how much area is still uncovered.
                  (list (ignore-errors (graph-db:geometry-area covered))
                        (and remaining (ignore-errors (graph-db:geometry-area remaining)))
                        ;; per-tile intersects test, as the route planner does
                        (loop for p in valid
                              count (ignore-errors
                                     (graph-db:geometry-intersects-p p extent)))))))))
    (if run-res
        (make-realworld-workload-result
         :name "Workload 7: GEOS Coverage Remainder"
         :description
         (concatenate 'string
          "The GEOS topology path behind coverage analysis: repair every "
          "parcel polygon with make-valid, union them into a covered area, "
          "subtract that from the station extent, and take geodesic areas. "
          "Also exercises the per-tile intersects test used by the route "
          "planner. This subsystem is used heavily by the application and "
          "previously had no workload.")
         :target-subsystems '(:geos :spatial :graph-core)
         :code-sample code-sample
         :run-result run-res)
        (make-realworld-workload-result
         :name "Workload 7: GEOS Coverage Remainder (SKIPPED)"
         :description
         (concatenate 'string
          "SKIPPED: libgeos_c is not available in this image, so the GEOS "
          "topology layer could not be profiled. Load GRAPH-DB/GEOS and ensure "
          "libgeos_c is on the library path.")
         :target-subsystems '(:geos :spatial)
         :code-sample code-sample
         :run-result nil))))

;;; --- Workload 8: Large-Polygon Whole-Record Materialization (NEW) ---
(defun profile-realworld-large-polygon-materialization-workload
    (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile the documented worst case: materializing a node that carries a
country-scale polygon in order to read one small scalar slot.

This is the shape of a downstream issue on deserialization cost.
Node materialization is
whole-record, so reading a zone's one-word CONTROL string pays full
deserialization of its ~2,200-vertex boundary every time the node is faulted in.
It is the workload that justifies the packed-coordinate work in #79/#81/#82 and
the read-path optimisation in #83, and nothing in the profiler exercised it."
  (let* ((zones (max 2 (floor (* scale 8))))
         (passes (max 1 (floor (* scale 25))))
         (ids '())
         (run-res nil)
         (code-sample ";; read ONE small scalar slot per pass; materialization is whole-record
(dotimes (_ passes)
  (let ((graph-db::*cache-enabled* nil))
    (dolist (id zone-ids)
      (let ((z (graph-db:lookup-vertex id :graph graph)))
        (slot-value z 'control)))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-bigpoly/")
      (let ((graph-db:*graph* graph))
        (graph-db:with-transaction ()
          (dotimes (i zones)
            (let ((z (make-rw-zone :zone-name (format nil "Zone-~D" i)
                                   :control (if (evenp i) "STATE-A" "STATE-B")
                                   :boundary (%rw-large-zone-polygon i))))
              (push (graph-db:id z) ids)))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 8: Large-Polygon Whole-Record Materialization (~:D zones x ~:D vertices, ~:D passes)"
                                          zones *rw-large-zone-vertices* passes)
                            :subsystems (or subsystems '(:serialization :mmap-storage :graph-core :spatial))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph)
                    (n 0))
                (dotimes (_ passes n)
                  ;; Cache bypassed: every pass pays a fresh deserialization,
                  ;; which is what a cold or cache-evicted read costs.
                  (let ((graph-db::*cache-enabled* nil))
                    (dolist (id ids)
                      (let ((z (graph-db:lookup-vertex id :graph graph)))
                        (when (and z (ignore-errors (slot-value z 'control)))
                          (incf n))))))))))
    (make-realworld-workload-result
     :name "Workload 8: Large-Polygon Whole-Record Materialization"
     :description
     (concatenate 'string
      "The downstream deserialization-cost issue in miniature: repeatedly "
      "fault in nodes that carry a country-scale (~2,200 vertex) boundary "
      "polygon purely to read a one-word scalar slot. Because materialization "
      "is whole-record, the polygon is fully deserialized every time. This "
      "isolates deserialization cost from GEOS and from spatial indexing, and "
      "is the workload the packed double-float coordinate representation was "
      "introduced to improve.")
     :target-subsystems '(:serialization :mmap-storage :graph-core :spatial)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 9: Zone Control History (NEW) ---
(defun profile-realworld-control-history-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile zone-control-history generation -- the application's slowest
path.

Mirrors the downstream app's zone-history profile: for every day in the window,
fetch that day's handful of country-scale control zones, materialize each one and
test whether the anchor point falls inside it.  Measured at 1,125 ms per pin on
production.

The walk is date-driven and never consults the spatial index -- faithfully
reproduced here, because that is a deliberate design decision in the application
(a day holds ~4 polygons, so an index adds overhead for no selectivity).  What
this workload isolates is therefore the real cost: days x zones-per-day
whole-record materializations of ~1,480-vertex multipolygons, plus the same
number of point-in-polygon tests."
  (let* ((days (max 2 (floor (* scale 90))))
         (walk (max 2 (floor (* scale *rw-control-window-days*))))
         (per-day *rw-control-zones-per-day*)
         (by-date (make-hash-table :test #'equal))
         (run-res nil)
         (code-sample "(dolist (date dates)                      ; ~1500 days
  (let ((zones (control-zones-for-date date)))     ; ~4 country-scale zones
    (dolist (z zones)
      ;; whole-record materialization of a ~1480-vertex multipolygon,
      ;; to answer one point-in-polygon question
      (graph-db:geometry-contains-point-p (extent z) ref-lon ref-lat))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-control/")
      (let ((graph-db:*graph* graph))
        (graph-db:with-transaction ()
          (dotimes (d days)
            (let ((date (%rw-ds-date d))
                  (ids '()))
              (dotimes (k per-day)
                (let ((z (make-rw-ds-zone
                          :zone-state (nth k '("state-a" "state-b"
                                               "state-c" "state-d"))
                          :zone-date date
                          :extent (%rw-ds-zone-multipolygon (+ (* d per-day) k)))))
                  (push (graph-db:id z) ids)))
              (setf (gethash date by-date) (nreverse ids))))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 9: ~
                                   Zone Control History ~
                                   (~:D days walked, ~:D zones x ~:D vertices)"
                                  walk (* days per-day)
                                  *rw-control-zone-vertices*)
                            :subsystems (or subsystems '(:serialization :mmap-storage :graph-core
                                          :spatial :geos))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph)
                    (ref-lon 7.0d0)
                    (ref-lat 44.6d0)
                    (hits 0))
                (dotimes (i walk hits)
                  ;; Dates cycle over the built window, exactly as a real walk
                  ;; revisits the same zone rows across a long history.
                  (let ((ids (gethash (%rw-ds-date (mod i days)) by-date)))
                    (dolist (id ids)
                      (let ((z (graph-db:lookup-vertex id :graph graph)))
                        (when z
                          (let ((ext (ignore-errors (slot-value z 'extent))))
                            (when (and ext
                                       (ignore-errors
                                        (graph-db:geometry-contains-point-p
                                         ext ref-lon ref-lat)))
                              (incf hits))))))))))))
    (make-realworld-workload-result
     :name "Workload 9: Zone Control History"
     :description
     (concatenate 'string
      "Zone-control-history generation, the application's slowest path "
      "(measured 1,125 ms per dropped pin). Walks every day of a ~1,500-day "
      "window; each day materializes ~4 country-scale multipolygon control "
      "zones (median 1,480 vertices) and runs a point-in-polygon test against "
      "the anchor. Deliberately index-free, matching the application, so the "
      "measurement isolates whole-record materialization plus containment "
      "cost.")
     :target-subsystems '(:serialization :mmap-storage :graph-core :spatial :geos)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 10: Event Dropped-Pin Radius Query (NEW) ---
(defun profile-realworld-event-pin-workload
    (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile the dropped-pin event query: a 10 km diameter circle over a large
point corpus.

Mirrors the downstream app's geo-scope query: candidates come from the spatial
index (never a full scan -- production holds 286,198 events), then are precisely
refined by haversine distance and filtered.  Measured at 169 ms for r=5 km
returning 462 events."
  (let* ((events (max 100 (floor (* scale 20000))))
         (queries (max 1 (floor (* scale 25))))
         (run-res nil)
         (code-sample "(dolist (pair (graph-db:find-nodes-near 'event
                                        ref-lat ref-lon 5000d0))
  (let ((ev (car pair)))                     ; spatial-index candidate
    (when (<= (%haversine-m ref-lat ref-lon (lat ev) (lon ev)) radius-m)
      (when (passes-filters-p ev filters)    ; precise refine + filter
        (push (id ev) ids)))))"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-events/")
      (let ((graph-db:*graph* graph))
        (graph-db:with-transaction ()
          (dotimes (i events)
            ;; Spread over a ~1.5 degree box so a 5 km pin selects a realistic
            ;; minority of the corpus rather than all or nothing.
            (let ((lon (+ 11.0d0 (* 1.5d0 (/ (mod (* i 7919) 1000) 1000.0d0))))
                  (lat (+ 43.0d0 (* 1.5d0 (/ (mod (* i 6271) 1000) 1000.0d0)))))
              (make-rw-event
               :event-id (format nil "EV-~D" i)
               :event-date (%rw-ds-date (mod i 900))
               :event-type (nth (mod i 4) '("type-a" "type-b"
                                            "type-c" "type-d"))
               :ev-lat lat :ev-lon lon
               :geom (graph-db:make-point lon lat))))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 10: ~
                                   Event Dropped-Pin Radius Query ~
                                   (~:D events, ~:D pins @ ~,1F km diameter)"
                                          events queries
                                          (/ (* 2 *rw-pin-radius-m*) 1000.0))
                            :subsystems (or subsystems '(:spatial :graph-storage :index-backends
                                          :serialization :mmap-storage))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph)
                    (kept 0))
                (dotimes (q queries kept)
                  (let ((ref-lon (+ 11.2d0 (* 0.1d0 (mod q 10))))
                        (ref-lat (+ 43.2d0 (* 0.1d0 (mod q 7)))))
                    (dolist (pair (graph-db:find-nodes-near
                                   'rw-event ref-lat ref-lon
                                   *rw-pin-radius-m* :graph graph))
                      (let ((ev (car pair)))
                        ;; Refine + filter, as the application does: the index is
                        ;; a cell-granular prefilter, not an answer.
                        (when (and ev
                                   (<= (or (cdr pair) 0d0) *rw-pin-radius-m*)
                                   (string/= (ignore-errors (slot-value ev 'event-type))
                                             "type-d"))
                          (incf kept))))))))))
    (make-realworld-workload-result
     :name "Workload 10: Event Dropped-Pin Radius Query"
     :description
     (concatenate 'string
      "The dropped-pin event query: a 10 km diameter circle resolved against a "
      "large point corpus. Candidates come from the geohash spatial index "
      "(never a full scan) and are then precisely refined by distance and "
      "filtered by event attributes -- the index is a cell-granular prefilter, "
      "not the answer. Measured at 169 ms / 462 events for r=5 km against 286k "
      "production events.")
     :target-subsystems '(:spatial :graph-storage :index-backends :serialization :mmap-storage)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Workload 11: Corpus Vector Retrieval (NEW) ---
(defun profile-realworld-kb-vector-search-workload (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Profile corpus retrieval: cosine top-K over the chunk vector segment.

The production corpus holds 23,193 chunk embeddings of dimension 1,024
in a single mmap-backed vector segment.  This profiles the graph-db half of RAG
retrieval -- segment scan and cosine scoring -- and deliberately excludes query
embedding, which is a network call to an embedding model, not engine work."
  (let* ((chunks (max 50 (floor (* scale 5000))))
         (queries (max 1 (floor (* scale 50))))
         (dim *rw-kb-vector-dim*)
         (k 5)
         (run-res nil)
         (code-sample "(graph-db:vector-search graph 'rw-kb-chunk 'embedding query-vector 5)
;; cosine top-K over an mmap-backed segment of 1024-dimension single-floats"))
    (with-rw-graph (graph #P"/tmp/vg-profiler-rw-kb/")
      (let ((graph-db:*graph* graph))
        (graph-db:with-transaction ()
          (dotimes (i chunks)
            (make-rw-kb-chunk :chunk-text (format nil "chunk ~D" i)
                              :doc-id (format nil "doc-~D" (mod i 200))
                              :embedding (%rw-kb-vector i dim)))))
      (setf run-res
            (profile-block (:name (format nil "Real-World Workload 11: ~
                                   Corpus Vector Retrieval ~
                                   (~:D chunks x ~:D dims, ~:D queries, top-~D)"
                                          chunks dim queries k)
                            :subsystems (or subsystems '(:graph-core :mmap-storage :serialization))
                            :sprof-mode sprof-mode
                            :top-n 30)
              (let ((graph-db:*graph* graph)
                    (found 0))
                (dotimes (q queries found)
                  (let ((hits (ignore-errors
                               (graph-db:vector-search
                                graph 'rw-kb-chunk 'embedding
                                (%rw-kb-vector (+ 100000 q) dim) k))))
                    (incf found (length hits))))))))
    (make-realworld-workload-result
     :name "Workload 11: Corpus Vector Retrieval"
     :description
     (concatenate 'string
      "The graph-db half of RAG retrieval: cosine top-K over an mmap-backed "
      "vector segment of 1,024-dimension single-float embeddings (production "
      "holds 23,193). Query embedding is excluded on purpose -- it is a "
      "network call to an embedding model, not engine work, and including it "
      "would swamp the measurement.")
     :target-subsystems '(:graph-core :mmap-storage :serialization)
     :code-sample code-sample
     :run-result run-res)))

;;; --- Master Real-World Suite Runner ---
(defun run-real-world-profiling-suite (&key (scale 1.0) (sprof-mode :cpu) subsystems)
  "Execute all real-world cross-subsystem workloads and return a list of
REALWORLD-WORKLOAD-RESULT objects."
  (format t "~%========================================================================~%")
  (format t "STARTING VIVACE-GRAPH REAL-WORLD CROSS-SUBSYSTEM SUITE (~A)~%"
          (local-time:format-timestring nil (local-time:now)))
  (format t "Workloads: Parcel/Item Ingestion, Map Viewport Query, ~
             View Rollup,~%")
  (format t "           Prolog (synthetic), Concurrent Txns, Complex Serialization,~%")
  (format t "           GEOS Coverage Remainder, ~
             Large-Polygon Materialization,~%")
  (format t "           Zone Control History, Event Dropped-Pin, ~
             Corpus Vector Retrieval~%")
  (format t "Scale: ~,1F | SPROF Mode: ~A | Subsystems: ~A~%" scale sprof-mode
          (or subsystems "per-workload defaults"))
  (format t "Data shape from a downstream application's measured ~
             production data.~%")
  (format t "========================================================================~%")

  (let ((results '()))
    (push (profile-realworld-ingestion-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-spatial-traversal-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-view-rollup-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-prolog-inference-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-concurrent-transactions-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-complex-serialization-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-geos-coverage-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-large-polygon-materialization-workload
           :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-control-history-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)
    (push (profile-realworld-event-pin-workload
           :scale scale :sprof-mode sprof-mode :subsystems subsystems)
          results)
    (push (profile-realworld-kb-vector-search-workload :scale scale :sprof-mode sprof-mode :subsystems subsystems) results)

    (let ((final-list (nreverse results)))
      (format t "~%========================================================================~%")
      (format t "VIVACE-GRAPH REAL-WORLD CROSS-SUBSYSTEM COMPARATIVE REPORT~%")
      (format t "========================================================================~%")
      (format t "  Workload Test Name             | Real Time (ms) | CPU Time (ms) | Memory Consed~%")
      (format t "------------------------------------------------------------------------~%")
      (dolist (res final-list)
        (let ((r (realworld-workload-result-run-result res))
              (nm (realworld-workload-result-name res)))
          (if r
              (format t "  ~30A | ~14,2F | ~13,2F | ~10,2F MB~%"
                      (subseq nm 0 (min 30 (length nm)))
                      (profiler-run-result-real-time-ms r)
                      (profiler-run-result-run-time-ms r)
                      (/ (profiler-run-result-bytes-consed r) 1048576.0d0))
              (format t "  ~30A | ~14A | ~13A | ~13A~%"
                      (subseq nm 0 (min 30 (length nm))) "SKIPPED" "SKIPPED" "SKIPPED"))))
      (format t "========================================================================~%")

      ;; Print Primitive Breakdown and Complete SPROF Hotspots for Each Workload
      (dolist (res final-list)
        (let* ((r (realworld-workload-result-run-result res))
               (prof (and r (profiler-run-result-profile r)))
               (p-entries (and prof (profile-result-entries prof)))
               (sprof (and r (profiler-run-result-sprof r)))
               (s-entries (and sprof (sprof-result-entries sprof))))
          (format t "~%========================================================================~%")
          (format t "WORKLOAD RESULTS: ~A~%" (realworld-workload-result-name res))
          (format t "========================================================================~%")
          (if (null r)
              (format t "  ~A~%" (realworld-workload-result-description res))
              (progn
                ;; SB-PROFILE Table
                (format t "--- [SB-PROFILE] Primitive Function Call & Allocation Tracing ---~%")
                (if p-entries
                    (progn
                      ;; Surface instrumentation distortion BEFORE the table, so
                      ;; nobody quotes a number the profiler already knows is
                      ;; mostly its own overhead.
                      (dolist (w (profile-result-overhead-warnings
                                  prof (profiler-run-result-real-time-ms r)))
                        (format t "  !! ~A~%" w))
                      (format t "     Calls |  Total ms |     us/call |      Consed | Bytes/Call | ! | Primitive Symbol~%")
                      (format t "----------------------------------------------------------------------------------------~%")
                      (dolist (e p-entries)
                        (format t "  ~8:D | ~9,3F | ~11@A | ~11@A | ~10:D | ~A | ~A~%"
                                (profile-entry-calls e)
                                (profile-entry-total-ms e)
                                (format-usec (profile-entry-usec-per-call e))
                                (format-bytes (profile-entry-bytes e))
                                (round (profile-entry-bytes-per-call e))
                                (if (profile-entry-overhead-suspect-p e) "!" " ")
                                (profile-entry-name e)))
                      (when (some #'profile-entry-overhead-suspect-p p-entries)
                        (format t "  ! = time is materially instrumentation overhead; trust the call count, not the time.~%")))
                    (format t "  No primitive tracing entries recorded.~%"))

                ;; SB-SPROF Unfiltered Stack Table
                (format t "~%--- [SB-SPROF] Statistical Sampling (Complete Unfiltered Stack & Method Entries) ---~%")
                (if s-entries
                    (progn
                      (format t "Total Samples Collected: ~:D~%" (sprof-result-total-samples sprof))
                      (when (plusp (sprof-result-filtered-rows sprof))
                        (format t "NOTE: ~:D row(s) carrying ~:D self-sample(s) are not shown (harness frames + top-N truncation).~%"
                                (sprof-result-filtered-rows sprof)
                                (sprof-result-filtered-samples sprof)))
                      (format t "  Self %  | Self Samples | Total % | Total Samples | Function / Method / Stack Entry~%")
                      (format t "----------------------------------------------------------------------------------------~%")
                      (dolist (e s-entries)
                        (format t "  ~5,1F%  | ~12D | ~5,1F%  | ~13D | ~A~%"
                                (sprof-sample-entry-self-pct e)
                                (sprof-sample-entry-self-samples e)
                                (sprof-sample-entry-total-pct e)
                                (sprof-sample-entry-total-samples e)
                                (sprof-sample-entry-name e))))
                    (format t "  No statistical sampling samples recorded.~%"))))))
      (format t "========================================================================~%")
      final-list)))
