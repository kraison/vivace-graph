(in-package :graph-db)

;;; Index-backed spatial queries (public spatial extension).
;;;
;;; These hit the graph's spatial index for candidate node ids, resolve them to
;;; live nodes, and refine with the exact geometry-ops predicates using each
;;; node's NODE-GEOMETRY.  A node is matched by its representative point: the
;;; point itself for a :POINT geometry, the bounding-box centre otherwise (so
;;; the EO-find-in-task-area case is exact; extended geometries are approximate).
;;;
;;; Both a Lisp API (FIND-NODES-WITHIN / FIND-NODES-NEAR) and Prolog functors
;;; (FIND-WITHIN/2, FIND-NEAR/4) are provided; the functors yield matching nodes
;;; so they compose with graph traversal in a query, e.g.:
;;;   (select-flat (?f) (is-a ?f eo-find) (find-near ?f 49.20 37.17 500.0))

;; Forward references: the (owner . slot) registry lives in spatial-registry.lisp,
;; which loads after this file (it needs the MOP helpers, the graph and the
;; memory-graph backend).  Same idiom graph.lisp uses for the unique/secondary
;; index functions.
(declaim (ftype (function (t t) t) %resolve-spatial-scope %scope-admits-p))
(declaim (ftype (function (t) t) all-spatial-indexes))
(declaim (ftype (function (t t t) t) %spatial-index-for))
;; Forward reference: SAVE-SPATIAL-INDEX-ROOTS lives in graph.lisp, which loads
;; before this file but does not depend on it (and vice versa) -- declared for the
;; same reason graph.lisp declares REBUILD-SPATIAL-INDEXES.  (&REST, not &KEY: its
;; single :COMPLETE keyword doesn't need spelling out here.)
(declaim (ftype (function (t &rest t) t) save-spatial-index-roots))

(defvar *spatial-rebuild-in-progress* nil
  "Bound to T for the duration of a multi-index spatial rebuild (REBUILD-SPATIAL-
INDEXES, REGENERATE-SPATIAL-INDEX).  While bound, %SPATIAL-INDEX-FOR's ordinary
per-creation sidecar save (spatial-registry.lisp) is a no-op: the rebuild brackets
the whole operation with its own :COMPLETE NIL / :COMPLETE T saves instead, so a
crash anywhere in between is caught by the completeness marker rather than by
trusting whichever per-index saves happened to land first.")

(defun %node-by-id (id graph)
  "Resolve a spatial-index id (uuid bytes) to its live node, or NIL."
  (or (lookup-vertex id :graph graph)
      (lookup-edge id :graph graph)))

(defun %geometry-rep-point (geom)
  "Representative (values lat lon) for GEOM: the point itself for a :POINT,
otherwise the centre of its bounding box."
  (if (eq (geometry-kind geom) :point)
      (values (geometry-lat geom) (geometry-lon geom))
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
        (values (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2)))))

(defun %node-within-area-p (area geom)
  "True if node geometry GEOM lies within AREA.  Exact for a :POINT (always) and
-- with the graph-db/geos add-on -- for extended geometries too.  Without GEOS,
extended geometries fall back to the representative-point approximation (the
historical behaviour), so results are unchanged when the add-on is absent."
  (if (eq (geometry-kind geom) :point)
      (geometry-contains-point-p area (geometry-lon geom) (geometry-lat geom))
      (if *geos-available-p*
          (geometry-contains-geometry-p area geom)
          (multiple-value-bind (lat lon) (%geometry-rep-point geom)
            (geometry-contains-point-p area lon lat)))))

(defmacro %do-scoped-candidates ((node-var scope graph &key bbox radius) &body body)
  "Run BODY with NODE-VAR bound to each live, scope-admitted node whose id came
back from every index in SCOPE.  Dedups by node id across indexes, so a node
reachable through two of its own slot-indexes is visited once."
  (let ((indexes (gensym "IX")) (types (gensym "TY")) (seen (gensym "SEEN"))
        (idx (gensym "I")) (id (gensym "ID")))
    `(multiple-value-bind (,indexes ,types) (%resolve-spatial-scope ,scope ,graph)
       (let ((,seen (make-hash-table :test 'equalp)))
         (dolist (,idx ,indexes)
           (dolist (,id ,(if bbox
                             `(destructuring-bind (mnl mnt mxl mxt) ,bbox
                                (spatial-index-query-bbox ,idx mnl mnt mxl mxt))
                             `(destructuring-bind (lat lon r) ,radius
                                (spatial-index-query-radius ,idx lat lon r))))
             (unless (gethash ,id ,seen)
               (setf (gethash ,id ,seen) t)
               (let ((,node-var (%node-by-id ,id ,graph)))
                 (when (and ,node-var (not (deleted-p ,node-var))
                            (%scope-admits-p ,node-var ,types))
                   ,@body)))))))))

(defun find-nodes-within (area &key (graph *graph*))
  "List of live nodes whose geometry lies within AREA (a :POLYGON or
:MULTIPOLYGON geometry).  A :POINT node is judged exactly; an extended-geometry
node is judged exactly when graph-db/geos is loaded, otherwise by its
representative point (bbox centre).

TRANSITIONAL: scoped to :ALL, which reproduces the single-index behaviour this
replaced.  A required scope argument lands in the next increment, and changes only
this lambda list and %RESOLVE-SPATIAL-SCOPE -- not the loop below."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node :all graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (%node-within-area-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-intersecting (area &key (graph *graph*))
  "List of live nodes whose geometry INTERSECTS AREA (any geometry kind).  Exact
with the graph-db/geos add-on; without it, extended-geometry candidates use a
COARSE bounding-box overlap test (point candidates are always exact)."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node :all graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (geometry-intersects-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-near (lat lon radius &key (graph *graph*))
  "List of (NODE . DISTANCE-METRES) for live nodes within RADIUS of (LAT, LON),
nearest first."
  (let ((result '()))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (%do-scoped-candidates (node :all graph :radius (list lat lon radius))
        (let ((geom (node-geometry node)))
          (when geom
            (multiple-value-bind (nlat nlon) (%geometry-rep-point geom)
              (let ((d (geodesic-distance lat lon nlat nlon)))
                (when (<= d radius)
                  (push (cons node d) result))))))))
    (sort result #'< :key #'cdr)))

(def-global-prolog-functor find-within/2 (?node ?area cont)
  "Yield each indexed node whose geometry lies within the bound :POLYGON or
:MULTIPOLYGON ?AREA."
  (let ((node-var (var-deref ?node))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-within area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-intersects/2 (?node ?area cont)
  "Yield each indexed node whose geometry intersects the bound ?AREA geometry."
  (let ((node-var (var-deref ?node))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-intersecting area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-near/4 (?node ?lat ?lon ?radius cont)
  "Yield each indexed node within ?RADIUS metres of (?LAT, ?LON)."
  (let ((node-var (var-deref ?node))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (radius (var-deref ?radius)))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (dolist (nd (find-nodes-near lat lon radius :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))

(defun find-nearest-k (lat lon k &key (graph *graph*) (max-radius 2.5d4))
  "List of (NODE . DISTANCE-METRES) for the K nodes nearest (LAT, LON), nearest
first (fewer than K if the graph holds fewer indexed nodes within MAX-RADIUS).

Correctness: FIND-NODES-NEAR returns every node within a given radius sorted by
distance, so once a radius encloses at least K nodes, those K are the global K
nearest -- anything outside the radius is farther than everything inside it.  We
start from one grid cell's size and double the radius until K are enclosed (or
MAX-RADIUS is reached), then keep the K closest.

MAX-RADIUS is a deliberate bound (default 25 km): kNN is \"K nearest within
MAX-RADIUS\".  Each widening re-runs the window query, whose cost grows with the
number of indexed nodes the window encloses (the bbox query covers a window with
a bounded set of coarse cells and range-scans them, so empty space is free);
widen MAX-RADIUS only if you accept scanning the larger candidate set."
  (let ((indexes (%resolve-spatial-scope :all graph)))
    (when (and indexes (numberp lat) (numberp lon) (integerp k) (plusp k))
      ;; Seed off the FINEST precision in scope: seeding from a coarse index would
      ;; make the very first widening an enormous sweep.
      ;; (LOOP MAXIMIZE, not REDUCE :KEY -- ANSI leaves it unspecified whether
      ;; REDUCE applies :KEY to a one-element sequence, and one index is the
      ;; common case.)
      (let* ((prec (loop for i in indexes maximize (spatial-index-precision i)))
             ;; seed radius: the index cell's latitude extent in metres
             (r (max 1d0 (* (nth-value 1 (geohash-cell-size prec)) 111320d0)))
             (found '()))
        (loop
          (setf found (find-nodes-near lat lon r :graph graph))
          (when (or (>= (length found) k) (>= r max-radius))
            (return))
          (setf r (min max-radius (* r 2d0))))
        (subseq found 0 (min k (length found)))))))

(def-global-prolog-functor find-nearest/4 (?node ?lat ?lon ?k cont)
  "Yield each of the ?K nodes nearest (?LAT, ?LON), nearest first."
  (let ((node-var (var-deref ?node))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (k (var-deref ?k)))
    (when (and (numberp lat) (numberp lon) (integerp k))
      (dolist (nd (find-nearest-k lat lon k :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))

(defun make-spatial-replication-filter (area)
  "Return a predicate (NODE) -> generalized boolean for use as a slave graph's
REPLICATION-FILTER (see MAKE-GRAPH :replication-filter).  It accepts a node when
it has no geometry (so non-spatial data -- schema, reference data -- replicates
in full) or when its geometry's representative point lies within AREA (a
:polygon / :multipolygon).  A field slave then receives only the nodes for its
area of operations, plus all non-spatial nodes."
  (lambda (node)
    (let ((geom (node-geometry node)))
      (or (null geom)
          (multiple-value-bind (lat lon) (%geometry-rep-point geom)
            (geometry-contains-point-p area lon lat))))))

(defun report-degraded-spatial-indexes (graph)
  "Warn ONCE PER INDEX whose coarsest occupied precision is below the precision it
was configured with -- i.e. an oversized geometry has capped its insert cover and
widened every query's covering clamp (§7.2).

%SPATIAL-INDEX-NODE warns at the moment of the coarsening, but that warning fires
on the transition only, and no rebuild path routes through it.  Without this, an
index that comes back up still degraded -- through a reopen from the sidecar, or a
rebuild -- would be silent forever after that one original warning.  Emitted per
INDEX, never per node: a rebuild must not produce a warning storm."
  (when (spatial-indexes graph)
    (maphash (lambda (key idx)
               (let ((configured (spatial-index-precision idx))
                     (coarsest (spatial-index-coarsest-precision idx)))
                 (when (< coarsest configured)
                   (warn "Spatial index ~S.~S is DEGRADED: configured precision ~D, ~
                          but cells are stored as coarsely as ~D, so every query on ~
                          it covers at precision ~D.  Delete the oversized ~
                          geometries (the clamp is self-healing) or call ~
                          (REGENERATE-SPATIAL-INDEX graph '~S '~S)."
                         (car key) (cdr key) configured coarsest coarsest
                         (car key) (cdr key)))))
             (spatial-indexes graph)))
  nil)

(defun rebuild-spatial-indexes (graph)
  "Rebuild GRAPH's spatial indexes from scratch: drop every current index, then
re-index each live node into the (owner . slot) index its geometry slot selects.
Returns the number of nodes indexed.

Use this to adopt the per-class scheme on a graph that predates it, to change grid
precision (set GRAPH-DEFAULT-SPATIAL-PRECISION first), or to repair.  It mutates
the indexes directly (outside the transaction write path), so run it when the
graph is quiescent -- analogous to REGENERATE-VIEW.

The sidecar is bracketed :COMPLETE NIL / :COMPLETE T around the ENTIRE rebuild,
not saved once per index as it is created along the way: *SPATIAL-REBUILD-IN-
PROGRESS* suppresses %SPATIAL-INDEX-FOR's ordinary per-creation save for the
duration, so a crash anywhere between the two brackets -- including before the
first index is even recreated, or on a graph whose geometry-bearing nodes are all
deleted, so nothing gets recreated at all -- leaves a sidecar that reads back
:COMPLETE NIL, and RESTORE-SPATIAL-INDEX-ROOTS re-derives on the next open rather
than trusting a file naming a freed address or missing an index the crash never
reached.

The closing save lives HERE, inside the function, not left to callers as it once
was: this function is exported, and a bare external call that relied on a caller's
after-the-fact save would strand the sidecar on :COMPLETE NIL, forcing a needless
full rebuild on every subsequent open until something happened to save it complete."
  (with-recursive-lock-held ((txn-lock graph))
    ;; Bind *GRAPH*: NODE-GEOMETRY reads slots, and a node read that falls back to
    ;; the dynamic current graph must resolve in GRAPH, not in whatever the caller
    ;; happened to have current (the wrong-graph bug class).
    (let ((*graph* graph)
          (count 0))
      (dolist (idx (all-spatial-indexes graph))
        ;; Only a heap-backed ordered map owns storage that must be freed; a
        ;; memory-graph's in-RAM index is simply dropped with the registry entry.
        (when (view-index-p (spatial-index-skip-list idx))
          (delete-spatial-index idx)))
      (clrhash (spatial-indexes graph))
      ;; Mark the sidecar INCOMPLETE before any reindexing below.  It now names
      ;; zero indexes -- momentarily true, and, if a crash intervenes before the
      ;; closing save below runs, the last thing a reopen will read back.  The
      ;; :COMPLETE NIL marker (not the emptiness of :INDEXES) is what makes
      ;; RESTORE-SPATIAL-INDEX-ROOTS refuse to trust it.
      (save-spatial-index-roots graph :complete nil)
      (let ((*spatial-rebuild-in-progress* t))
        (flet ((reindex (node)
                 (unless (deleted-p node)
                   (multiple-value-bind (geom slot) (node-geometry node)
                     (when geom
                       (spatial-index-insert
                        (%spatial-index-for
                         graph (%indexed-slot-owner-name (class-of node) slot) slot)
                        (id node) geom)
                       (incf count))))))
          (map-vertices #'reindex graph)
          (map-edges #'reindex graph)))
      (report-degraded-spatial-indexes graph)
      ;; Every index named above is back in place; mark the sidecar COMPLETE
      ;; again.  A crash before this point re-derives from scratch on the next
      ;; open; a crash after it (or none at all) reopens by address, as usual.
      (save-spatial-index-roots graph)
      count)))

(defun regenerate-spatial-index (graph owner-name slot-name)
  "Drop and rebuild ONE spatial index, re-deriving its precision histogram from
live nodes.  This is the manual recovery for an index whose selectivity was
degraded by an oversized insert (§7.2) -- reach for this rather than
REGENERATE-SPATIAL-INDEXES, which rebuilds every index in the graph.  Returns the
number of nodes indexed.

WARNS (but still returns 0, not an error) when OWNER-NAME does not name any
vertex or edge type registered in GRAPH: RESOLVE-NODE-TYPE-IDS silently skips an
unresolvable designator, so the scan below would otherwise just find nothing --
a return value indistinguishable from a real index whose nodes were all deleted.
That is a plausible mistake, since a shared index may be declared on an ancestor
class with several subclasses (§4).

The sidecar is bracketed :COMPLETE NIL / :COMPLETE T the same way REBUILD-SPATIAL-
INDEXES is: this function deletes and remhashes the OLD index before recreating
it, so a crash in between would otherwise leave the sidecar naming an address
DELETE-SPATIAL-INDEX just freed.  Marking the WHOLE sidecar incomplete for a
single-index regenerate is heavier than strictly necessary -- it forces every
OTHER index to re-derive too, on the next open after a crash, since the v3 format
has no per-index completeness granularity -- but it is the safe direction."
  (with-recursive-lock-held ((txn-lock graph))
    ;; Bind *GRAPH*: NODE-GEOMETRY reads slots, and a node read that falls back to
    ;; the dynamic current graph must resolve in GRAPH (the wrong-graph bug class).
    (let ((*graph* graph)
          (key (cons owner-name slot-name)))
      ;; Mark the sidecar INCOMPLETE before touching the old index's storage: a
      ;; crash between here and the closing save below would otherwise leave the
      ;; sidecar naming a freed address, exactly the freed-root window §7.2 and
      ;; the fix in 6e1462b closed for REBUILD-SPATIAL-INDEXES.
      (save-spatial-index-roots graph :complete nil)
      (unless (or (resolve-node-type-ids owner-name :vertex :graph graph)
                  (resolve-node-type-ids owner-name :edge :graph graph))
        (warn "REGENERATE-SPATIAL-INDEX: ~S is not a registered vertex or edge ~
               type in ~S, so the (~S . ~S) index will be rebuilt from ZERO ~
               nodes -- indistinguishable from a real index whose nodes were all ~
               deleted.  Check the class name and that it is declared on this ~
               graph."
              owner-name graph owner-name slot-name))
      (let ((old (gethash key (spatial-indexes graph))))
        ;; Only a heap-backed ordered map owns storage that must be freed; a
        ;; memory-graph's in-RAM index is simply dropped with the registry entry.
        (when (and old (view-index-p (spatial-index-skip-list old)))
          (delete-spatial-index old)))
      (remhash key (spatial-indexes graph))
      (let ((count 0))
        (let ((*spatial-rebuild-in-progress* t))
          (flet ((reindex (node)
                   (unless (deleted-p node)
                     (multiple-value-bind (geom slot) (node-geometry node)
                       (when (and geom (eq slot slot-name)
                                  (eq (%indexed-slot-owner-name (class-of node) slot)
                                      owner-name))
                         (spatial-index-insert
                          (%spatial-index-for graph owner-name slot-name)
                          (id node) geom)
                         (incf count))))))
            ;; OWNER-NAME is the DECLARING class, and its subclasses share the
            ;; index, so the typed scan must include them -- MAP-VERTICES/MAP-EDGES
            ;; do by default.  FIND-CLASS guards SUBTYPEP against a name that no
            ;; longer designates a class (a sidecar entry whose class was never
            ;; redefined in this image); the vertex scan then simply finds nothing.
            (let ((class (find-class owner-name nil)))
              (if (and class (subtypep class 'edge))
                  (map-edges #'reindex graph :edge-type owner-name)
                  (map-vertices #'reindex graph :vertex-type owner-name)))))
        ;; The (owner . slot) index is back in place; mark the sidecar COMPLETE
        ;; again.  A crash before this point re-derives EVERY index from scratch
        ;; on the next open; a crash after it (or none at all) reopens by address.
        (save-spatial-index-roots graph)
        (report-degraded-spatial-indexes graph)
        count))))

(defun regenerate-spatial-indexes (graph)
  "Drop every spatial index and rebuild it on GRAPH's CURRENT :INDEX-BACKEND,
persisting the new roots.  The parallel of REGENERATE-ALL-VIEWS /
REGENERATE-SECONDARY-INDEXES for an in-place backend switch.

REBUILD-SPATIAL-INDEXES persists the completed sidecar itself; no trailing save
is needed here."
  (rebuild-spatial-indexes graph)
  graph)
