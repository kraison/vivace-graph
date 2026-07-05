(in-package :graph-db)

;;; Geohash spatial index for VivaceGraph (public, general-purpose).
;;;
;;; A skip list keyed by fixed-precision geohash strings maps grid cells to the
;;; ids of nodes whose geometry occupies them.  Because every key has the same
;;; precision, a cell lookup is an exact key match (duplicates allowed: many
;;; nodes per cell, and an extended geometry occupies several cells).
;;;
;;; Queries are a FILTER: they return the candidate node ids whose cells meet
;;; the query window; the caller REFINES with the exact predicates in
;;; geometry-ops.lisp (point-in-polygon, geodesic-distance).  This is the
;;; standard filter/refine design and keeps the index independent of node
;;; semantics -- it stores ids and geometry only.
;;;
;;; The skip list lives in a caller-supplied MEMORY (heap); its root address
;;; (SPATIAL-INDEX-ADDRESS) is what a host persists to reopen the index, exactly
;;; as views persist their skip-list pointer.

(defstruct (spatial-index (:constructor %make-spatial-index) (:predicate spatial-index-p))
  skip-list
  heap
  (precision 7 :type (integer 1 12)))

;; Geohash uses only the base32 alphabet (max char #\z), so "" sorts before and
;; "{" (#\{ = 123) sorts after every possible key.
(alexandria:define-constant +spatial-min-key+ "" :test 'string=)
(alexandria:define-constant +spatial-max-key+ "{" :test 'string=)

;; On-disk sidecar format version.  v1 (unversioned) keyed the skip list by a bare
;; geohash string with the node-id as the value (DUPLICATE keys -> O(n) remove); v2
;; keys by the composite (cell . node-id), duplicate-free.  RESTORE-SPATIAL-INDEX
;; rebuilds a v1 index into v2 on open (a re-scan of node geometries).
(alexandria:define-constant +spatial-index-format+ 2 :test '=)

;; A bbox query covers its window with at most this many (coarse) cells, each of
;; which becomes ONE prefix range scan.  Bounding the covering set is what keeps a
;; continent-sized window cheap; the constant trades scan count against per-scan
;; over-coverage (a coarser cell pulls in a slightly wider candidate margin, which
;; the caller's exact predicate then refines away).
(alexandria:define-constant +spatial-query-max-cells+ 256 :test '=)

;; The skip list is keyed by the COMPOSITE (cell . node-id) -- a geohash string
;; paired with the node's 16-byte uuid -- exactly like a view's (key . id) key,
;; and stored duplicate-free.  Folding the node-id into the key (rather than
;; keeping many duplicate `cell' keys with the id as the value) is what makes
;; REMOVE O(log n) and correct on both backends: duplicate-key removal was O(n)
;; on disk (find-kv rescans from the head) and silently wrong in RAM (find
;; overshoots a taller same-key node).  A cell lookup becomes a prefix range
;; scan over [(cell null-id) .. (cell max-id)] and the node-id is read back from
;; the key's second element; the skip-node value is unused (NIL).  The composite
;; codec is VIEW-KEY-SERIALIZE (payload string + 16-byte id), shared with views
;; and unique indexes.
;; Created through the shared MAKE-HEAP-INDEX (bplus-tree.lisp), so the spatial
;; index follows the graph's chosen backend (skip list or B+ tree) like views and
;; unique.  INIT-SPATIAL-INDEX passes (GRAPH-INDEX-BACKEND GRAPH).
(defun %spatial-make-sl (heap backend)
  (make-heap-index backend heap 'reduce-comp-lessp))

(defun make-spatial-index (heap &key (precision 7) (backend *index-backend*))
  "Create a new spatial index in HEAP (a MEMORY).  PRECISION sets the geohash
grid resolution (7 ~ 150 m cells, 9 ~ 5 m).  BACKEND (:skip-list / :bplus-tree)
picks the ordered-map engine."
  (%make-spatial-index :skip-list (%spatial-make-sl heap backend)
                       :heap heap :precision precision))

(defun open-spatial-index (heap address &key (precision 7) (backend *index-backend*))
  "Reopen the spatial index whose ordered map is rooted at ADDRESS in HEAP, with
BACKEND's opener.  PRECISION must match the value used at creation.  BACKEND
defaults to the current *INDEX-BACKEND* for the raw API; RESTORE-SPATIAL-INDEX
passes the tag persisted in the sidecar (authoritative -- a pre-B+-tree sidecar
has no tag and restores as :skip-list)."
  (%make-spatial-index
   :skip-list (open-heap-index backend :address address :heap heap
                               :comparison 'reduce-comp-lessp)
   :heap heap :precision precision))

(defun spatial-index-address (idx)
  "Root heap address of IDX's ordered map -- persist this to reopen the index."
  (view-index-address (spatial-index-skip-list idx)))

(defun spatial-index-backend (idx)
  "Backend tag of IDX's ordered map -- persist alongside the address."
  (view-index-backend-tag (spatial-index-skip-list idx)))

(defun delete-spatial-index (idx)
  "Free the index's ordered map from its heap."
  (delete-view-index (spatial-index-skip-list idx)))

(defun %bbox-cells (geom precision)
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
    (geohash-covering min-lon min-lat max-lon max-lat :precision precision)))

(defun %geometry-cells (geom precision)
  "The geohash cells (strings) GEOM occupies at PRECISION.  A point yields one
cell; a polygon/linestring yields the grid over its bbox.  A multipolygon is
covered PART BY PART (not by one overall bbox) so the empty gaps between
separated parts -- e.g. a city-scale task area -- are not indexed."
  (if (eq (geometry-kind geom) :multipolygon)
      (let ((seen (make-hash-table :test 'equal)) (cells '()))
        (dolist (poly (geometry-coordinates geom) cells)
          (dolist (c (%bbox-cells (%make-geometry :kind :polygon :coordinates poly)
                                  precision))
            (unless (gethash c seen)
              (setf (gethash c seen) t)
              (push c cells)))))
      (%bbox-cells geom precision)))

(defun spatial-index-insert (idx node-id geom)
  "Index NODE-ID under every cell GEOM occupies.  NODE-ID is a node's 16-byte
uuid; it is folded into the composite key (cell . node-id) and the skip-node
value is unused (NIL)."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)) node-id)
      (add-to-skip-list sl (list cell node-id) nil))))

(defun spatial-index-remove (idx node-id geom)
  "Remove NODE-ID's entries for GEOM (using the same cells INSERT produced).
Each (cell . node-id) is a unique composite key, so REMOVE takes the O(log n)
duplicate-free path."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)))
      (remove-from-skip-list sl (list cell node-id)))))

(defun spatial-index-query-bbox (idx min-lon min-lat max-lon max-lat)
  "Candidate node-ids whose indexed cells meet the query bounding box.  A
cell-granular FILTER -- refine with exact geometry predicates.

The window is covered with as FEW cells as possible: the covering precision is
chosen adaptively to stay under +SPATIAL-QUERY-MAX-CELLS+, and never exceeds the
index's storage precision.  Each covering cell drives ONE ordered PREFIX RANGE
SCAN of the skip list (geohash keys are prefix-nested, so every stored cell inside
a coarse cell sorts within its prefix range).  A continent-sized window is thus a
handful of range scans rather than the millions of fixed-precision cell probes
that used to enumerate -- and cons -- an empty grid until the heap was exhausted.
A coarse covering cell can extend past the bbox, so the candidate set may include
a thin margin of nearby nodes; that is fine for a filter (the caller refines with
the exact predicate) and matches the index's existing filter/refine contract."
  (let* ((sl (spatial-index-skip-list idx))
         (cover-prec (min (spatial-index-precision idx)
                          (%covering-precision (max 0d0 (- max-lon min-lon))
                                               (max 0d0 (- max-lat min-lat))
                                               +spatial-query-max-cells+)))
         (seen (make-hash-table :test 'equalp))
         (result '()))
    (dolist (cell (geohash-covering min-lon min-lat max-lon max-lat
                                    :precision cover-prec))
      (multiple-value-bind (start end) (geohash-prefix-range cell)
        ;; Composite bounds: every (c . id) with START <= c < END sorts inside
        ;; [(START null-id) .. (END null-id)] (END is the synthetic above-alphabet
        ;; prefix cap, so no real key equals it).  The node-id is the key's 2nd
        ;; element now, not the skip-node value.
        (let ((cursor (make-range-cursor sl (list start +null-key+)
                                         (list end +null-key+))))
          (when cursor
            (do ((node (cursor-next cursor) (cursor-next cursor)))
                ((null node))
              (let ((nid (second (%sn-key node))))
                (unless (gethash nid seen)
                  (setf (gethash nid seen) t)
                  (push nid result))))))))
    result))

(defun spatial-index-query-radius (idx lat lon radius-m)
  "Candidate node-ids within ~RADIUS-M metres of (LAT, LON), via a bounding-box
prefilter.  Refine with GEODESIC-DISTANCE for an exact radius."
  (let* ((dlat (/ radius-m 111320d0))
         (dlon (/ radius-m (* 111320d0 (max 0.01d0 (cos (deg->rad lat)))))))
    (spatial-index-query-bbox idx (- lon dlon) (- lat dlat) (+ lon dlon) (+ lat dlat))))
