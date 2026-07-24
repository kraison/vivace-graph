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

;; An insert covers its geometry with at most this many cells.  A SAFETY NET,
;; not a selectivity knob -- per-index precision is that knob.  Sized so nothing
;; that works today changes behaviour: a site-scale 0.05 degree polygon is ~1,444
;; cells at p=7 and a city-scale 0.1 degree one ~5,476, both well under.  Per
;; index and PERSISTED (graph.lisp), never a mutable global: SPATIAL-INDEX-REMOVE
;; recomputes cells from the geometry, so a cap that drifted between an insert and
;; its matching remove would orphan entries permanently.
(alexandria:define-constant +spatial-insert-max-cells+ 16384 :test '=)

(defstruct (spatial-index (:constructor %make-spatial-index) (:predicate spatial-index-p))
  skip-list
  heap
  (precision 7 :type (integer 1 12))
  (max-cells +spatial-insert-max-cells+ :type (integer 1 *))
  ;; COUNTS[p] = how many stored cell entries sit at geohash precision P (1..12);
  ;; element 0 is unused.  A cell string's length IS its precision, so no separate
  ;; bookkeeping is needed at the call sites.
  (precision-counts (make-array 13 :element-type 'fixnum :initial-element 0)
                    :type (simple-array fixnum (13)))
  ;; Cached lowest occupied level -- the query's covering-precision clamp (see
  ;; SPATIAL-INDEX-QUERY-BBOX).  Equals PRECISION when the index is empty.
  (coarsest 7 :type (integer 1 12)))

(defun spatial-index-coarsest-precision (idx)
  "The finest covering precision a query may use against IDX: the lowest geohash
precision at which any cell is currently stored (its own PRECISION when empty).
Covering more finely than this would prefix-range-scan PAST a coarser stored key,
which sorts before the range start -- the silent-miss hole this closes."
  (spatial-index-coarsest idx))

(defun %count-cell (idx cell)
  "Record one stored CELL, lowering the cached coarsest level if it opens a new
coarser one.  O(1)."
  (let ((counts (spatial-index-precision-counts idx))
        (p (length cell)))
    (when (and (zerop (aref counts p)) (< p (spatial-index-coarsest idx)))
      (setf (spatial-index-coarsest idx) p))
    (incf (aref counts p))))

(defun %uncount-cell (idx cell)
  "Un-record one stored CELL.  When the level empties and it was the coarsest,
rescan for the new lowest occupied level -- this is what makes the clamp
SELF-HEALING: delete the oversized geometry and selectivity returns with no
rebuild.  The rescan is 12 iterations and runs only on that transition."
  (let ((counts (spatial-index-precision-counts idx))
        (p (length cell)))
    (when (plusp (aref counts p))
      (decf (aref counts p)))
    (when (and (zerop (aref counts p)) (= p (spatial-index-coarsest idx)))
      (setf (spatial-index-coarsest idx)
            (or (loop for q from 1 to 12 when (plusp (aref counts q)) return q)
                (spatial-index-precision idx))))))

;; Geohash uses only the base32 alphabet (max char #\z), so "" sorts before and
;; "{" (#\{ = 123) sorts after every possible key.
(alexandria:define-constant +spatial-min-key+ "" :test 'string=)
(alexandria:define-constant +spatial-max-key+ "{" :test 'string=)

;; On-disk sidecar format version.  v1 (unversioned) keyed the skip list by a bare
;; geohash string with the node-id as the value (DUPLICATE keys -> O(n) remove); v2
;; keys by the composite (cell . node-id), duplicate-free, one index per GRAPH; v3
;; is one index per (declaring-class . geometry-slot), each with its own precision,
;; insert cap and precision histogram.  A v1/v2 sidecar triggers an index-only
;; re-derivation from live node geometries at open (RESTORE-SPATIAL-INDEX-ROOTS).
(alexandria:define-constant +spatial-index-format+ 3 :test '=)

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

(defun make-spatial-index (heap &key (precision 7) (backend *index-backend*)
                                     (max-cells +spatial-insert-max-cells+))
  "Create a new spatial index in HEAP (a MEMORY).  PRECISION sets the geohash
grid resolution (7 ~ 150 m cells, 9 ~ 5 m).  BACKEND (:skip-list / :bplus-tree)
picks the ordered-map engine.  MAX-CELLS bounds the cells one insert may cover;
it is fixed for the life of the index (see +SPATIAL-INSERT-MAX-CELLS+)."
  (%make-spatial-index :skip-list (%spatial-make-sl heap backend)
                       :heap heap :precision precision
                       :max-cells max-cells :coarsest precision))

(defun open-spatial-index (heap address &key (precision 7) (backend *index-backend*)
                                             (max-cells +spatial-insert-max-cells+)
                                             precision-counts)
  "Reopen the spatial index whose ordered map is rooted at ADDRESS in HEAP, with
BACKEND's opener.  The caller must supply PRECISION and MAX-CELLS matching the
values used at creation; this constructor cannot check or recover them itself.
The v3 sidecar (SAVE-SPATIAL-INDEX-ROOTS in GRAPH.LISP) persists all four
alongside the address, so OPEN-GRAPH always has them to hand.  PRECISION-COUNTS
is the persisted histogram; omitting it (the default) starts from an empty one,
which sets the coarsest-precision clamp to PRECISION -- the FINEST clamp, so a
capped coarse entry already in the store would be silently unreachable.  That is
why the histogram is persisted, and re-persisted whenever the coarsest occupied
precision DROPS."
  (let ((idx (%make-spatial-index
              :skip-list (open-heap-index backend :address address :heap heap
                                          :comparison 'reduce-comp-lessp)
              :heap heap :precision precision
              :max-cells max-cells :coarsest precision)))
    (when precision-counts
      (replace (spatial-index-precision-counts idx) precision-counts)
      (setf (spatial-index-coarsest idx)
            (or (loop for p from 1 to 12
                      when (plusp (aref (spatial-index-precision-counts idx) p))
                        return p)
                precision)))
    idx))

(defun spatial-index-address (idx)
  "Root heap address of IDX's ordered map -- persist this to reopen the index."
  (view-index-address (spatial-index-skip-list idx)))

(defun spatial-index-backend (idx)
  "Backend tag of IDX's ordered map -- persist alongside the address."
  (view-index-backend-tag (spatial-index-skip-list idx)))

(defun delete-spatial-index (idx)
  "Free the index's ordered map from its heap."
  (delete-view-index (spatial-index-skip-list idx)))

(defun %bbox-cells (geom precision max-cells)
  "The cells covering GEOM's bbox, at the FINEST precision that stays within
MAX-CELLS and never exceeds PRECISION.  A pure function of (geom, precision,
max-cells) -- which is what makes SPATIAL-INDEX-REMOVE recompute exactly the set
SPATIAL-INDEX-INSERT wrote."
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
    (let ((p (min precision
                  (%covering-precision (max 0d0 (- max-lon min-lon))
                                       (max 0d0 (- max-lat min-lat))
                                       max-cells))))
      (geohash-covering min-lon min-lat max-lon max-lat :precision p))))

(defun %geometry-cells (geom precision max-cells)
  "The geohash cells (strings) GEOM occupies.  A point yields one cell; a
polygon/linestring yields the capped grid over its bbox.  A multipolygon is
covered PART BY PART (not by one overall bbox) so the empty gaps between
separated parts are not indexed, each part drawing on an equal share of
MAX-CELLS so one huge part cannot starve the rest."
  (if (eq (geometry-kind geom) :multipolygon)
      (let* ((parts (geometry-coordinates geom))
             (budget (max 1 (floor max-cells (max 1 (length parts)))))
             (seen (make-hash-table :test 'equal))
             (cells '()))
        (dolist (poly parts cells)
          (dolist (c (%bbox-cells (%make-geometry :kind :polygon :coordinates poly)
                                  precision budget))
            (unless (gethash c seen)
              (setf (gethash c seen) t)
              (push c cells)))))
      (%bbox-cells geom precision max-cells)))

(defun spatial-index-insert (idx node-id geom)
  "Index NODE-ID under every cell GEOM occupies.  NODE-ID is a node's 16-byte
uuid; it is folded into the composite key (cell . node-id) and the skip-node
value is unused (NIL).  ADD-TO-SKIP-LIST is a duplicate-key no-op (returns NIL)
when this exact (cell . node-id) is already stored -- e.g. a replayed or
double-applied insert -- and PRECISION-COUNTS is only bumped when an entry was
actually added, so the histogram tracks what the store physically holds."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)
                                   (spatial-index-max-cells idx))
                  node-id)
      (when (add-to-skip-list sl (list cell node-id) nil)
        (%count-cell idx cell)))))

(defun spatial-index-remove (idx node-id geom)
  "Remove NODE-ID's entries for GEOM (using the same cells INSERT produced).
Each (cell . node-id) is a unique composite key, so REMOVE takes the O(log n)
duplicate-free path.  REMOVE-FROM-SKIP-LIST is a supported no-op (returns NIL)
when the (cell . node-id) is not present -- e.g. a double-applied remove from
transaction replay or an idempotent peer purge -- and PRECISION-COUNTS is only
decremented when an entry was actually removed.  Decrementing unconditionally
would drift the histogram below what the store really holds, and because
%UNCOUNT-CELL floors at zero the corruption is silent: the coarsest-precision
clamp can end up finer than a level that is still physically populated, and
SPATIAL-INDEX-QUERY-BBOX's prefix range scan then misses it -- see
SPATIAL-INDEX-COARSEST-PRECISION."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)
                                   (spatial-index-max-cells idx)))
      (when (remove-from-skip-list sl (list cell node-id))
        (%uncount-cell idx cell)))))

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
the exact predicate) and matches the index's existing filter/refine contract.

The covering precision is additionally clamped to the coarsest precision at which
any cell is currently stored (SPATIAL-INDEX-COARSEST-PRECISION), which is what
lets an oversized geometry be stored coarsely without becoming unfindable."
  (let* ((sl (spatial-index-skip-list idx))
         (cover-prec (min (spatial-index-precision idx)
                          (%covering-precision (max 0d0 (- max-lon min-lon))
                                               (max 0d0 (- max-lat min-lat))
                                               +spatial-query-max-cells+)
                          ;; Never cover FINER than the coarsest stored cell: a
                          ;; prefix range scan [cell, cell+"{") reaches only keys
                          ;; at or finer than CELL, and a coarser stored key sorts
                          ;; before the range start.  Without this, a capped insert
                          ;; would be silently invisible.
                          (spatial-index-coarsest-precision idx)))
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
