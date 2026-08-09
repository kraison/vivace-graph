;;;; Per-(owner . slot) spatial index registry and scoped queries.

(in-package #:graph-db/test)

;; Two spatially-indexed classes with NO common geometry-declaring ancestor, so
;; they land in separate indexes.  ZONE's polygon contains every PROBE point --
;; the discriminating case from the change request.
(def-vertex scope-probe ()
  ((geom :type geometry :index t))
  :graph-db-integration-test)

(def-vertex scope-zone ()
  ((extent :type geometry :index t))
  :graph-db-integration-test)

(def-suite spatial-scope-suite
  :description "Per-class spatial indexes: registry, scoping, declaration."
  :in graph-db-suite)

(in-suite spatial-scope-suite)

;; %MAKE-GEOMETRY is internal to GRAPH-DB (not exported, not imported here).
(defun scope-rect (min-lon min-lat max-lon max-lat)
  (graph-db::%make-geometry
   :kind :polygon
   :coordinates (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(test registry-separates-declaring-classes
  "Two classes declaring their own geometry slot get two distinct indexes."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-ix (spatial-index-for g 'scope-probe 'geom))
          (zone-ix  (spatial-index-for g 'scope-zone 'extent)))
      (is (spatial-index-p probe-ix))
      (is (spatial-index-p zone-ix))
      (is (not (eq probe-ix zone-ix))))))

(test node-geometry-reports-its-slot
  "NODE-GEOMETRY returns the geometry AND the slot it came from."
  (with-test-graph (g)
    (declare (ignore g))
    (let (node)
      (with-transaction ()
        (setq node (make-scope-zone :extent (scope-rect 0d0 0d0 1d0 1d0))))
      (multiple-value-bind (geom slot) (node-geometry node)
        (is (geometryp geom))
        (is (eq slot 'extent))))))

(test unindexed-geometry-slot-creates-no-index
  "A slot that never holds a geometry never creates an index (§4.1)."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (is (null (spatial-index-for g 'scope-zone 'extent)))))

;;; ---------------------------------------------------------------------------
;;; v3 sidecar: roots persist, reopen is address-based (no node scan, no leak).
;;; ---------------------------------------------------------------------------

(test roots-survive-a-clean-reopen
  "A clean close persists every index root; reopen finds the same nodes with no
node scan."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) zone-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq zone-id (id (make-scope-zone
                               :extent (scope-rect 0d0 0d0 1d0 1d0)))))
          (close-graph g :snapshot-p nil)))
      (is (probe-file (graph-db::spatial-indexes-root-file path))
          "CLOSE-GRAPH wrote the v3 sidecar")
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-zone 'extent)))
               (is (spatial-index-p idx))
               (is (has-p zone-id
                          (spatial-index-query-bbox idx 0.1d0 0.1d0 0.2d0 0.2d0))))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test reopen-does-not-grow-the-index-heap
  "The whole point of the sidecar: OPEN-GRAPH REOPENS the ordered maps by address
instead of allocating fresh ones and orphaning the previous run's.  GC-HEAP only
ever sweeps heap.dat, so an orphan in indexes.dat is unreclaimable -- the leak
shows up as the indexes heap's bump pointer climbing on every open."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (marks '()))
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (dotimes (i 200)
              (make-scope-probe :geom (make-point (+ 37d0 (* i 1d-3))
                                                  (+ 49d0 (* i 1d-3))))
              (make-scope-zone :extent (scope-rect (+ 20d0 (* i 1d-3))
                                                   (+ 44d0 (* i 1d-3))
                                                   (+ 20.01d0 (* i 1d-3))
                                                   (+ 44.01d0 (* i 1d-3))))))
          (close-graph g :snapshot-p nil)))
      (dotimes (i 3)
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (push (graph-db::memory-pointer (graph-db::indexes g)) marks)
            (close-graph g :snapshot-p nil)
            (collect-garbage))))
      (destructuring-bind (third second first) marks
        (is (= first second)
            "indexes.dat high-water grew between open 1 and 2: ~D -> ~D"
            first second)
        (is (= second third)
            "indexes.dat high-water grew between open 2 and 3: ~D -> ~D"
            second third)))))

(test pre-v3-sidecar-migrates-on-open
  "A pre-v3 graph -- spatial-index.root present, spatial-indexes.dat absent --
re-derives its per-(owner . slot) indexes from live node geometries at open, and
writes the v3 sidecar so the NEXT open is address-based.  Index only: the nodes
are untouched.  The old file is left in place, not renamed."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      ;; Fabricate the pre-v3 on-disk shape: no v3 sidecar, an old single-index
      ;; root file in its place.  (Its address is never read on this path -- the
      ;; file's mere presence is the migration signal.)
      (delete-file (graph-db::spatial-indexes-root-file path))
      (cl-store:store (list :format 2 :address 0 :precision 7 :backend :skip-list)
                      (graph-db::spatial-index-root-file path))
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-probe 'geom)))
               (is (spatial-index-p idx) "the index was re-derived from the nodes")
               (is (has-p probe-id
                          (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                    37.19d0 49.21d0)))
               (is (probe-file (graph-db::spatial-indexes-root-file path))
                   "migration wrote the v3 sidecar")
               (is (probe-file (graph-db::spatial-index-root-file path))
                   "the pre-v3 file is left in place, not renamed"))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

(test torn-sidecar-falls-back-to-a-rebuild
  "A truncated sidecar re-derives the indexes instead of refusing to open.

This one is written from inside a commit (index creation, coarsening), not only at
CLOSE-GRAPH, so a crash can tear it -- and the nodes it is derived from are still
entirely intact."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      ;; Truncate the sidecar mid-record, as an interrupted write would.
      (let* ((file (graph-db::spatial-indexes-root-file path))
             (bytes (with-open-file (in file :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                        (read-sequence b in)
                        b))))
        (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence bytes out :end (floor (length bytes) 2))))
      (handler-bind ((warning #'muffle-warning))    ; the torn-sidecar warning
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (let ((idx (spatial-index-for g 'scope-probe 'geom)))
                 (is (spatial-index-p idx) "the index was re-derived from the nodes")
                 (is (has-p probe-id
                            (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                      37.19d0 49.21d0))))
            (close-graph g :snapshot-p nil)
            (collect-garbage)))))))

(test degraded-clamp-survives-a-reopen
  "The sidecar carries the PRECISION HISTOGRAM, not just the root address.

A geometry too large to cover within +SPATIAL-INSERT-MAX-CELLS+ is stored at a
coarser precision, and the query's covering clamp is lowered to match so it stays
findable.  That clamp is derived from the histogram, which lives only in RAM
between closes -- so a sidecar that persisted the address alone would reopen with
the clamp back at the configured precision, and a prefix range scan would sort
PAST the coarser stored key: a silent miss on an index that is physically intact."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) big-id)
      (handler-bind ((warning #'muffle-warning))    ; coarsening is EXPECTED here
        (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              ;; ~1 degree square: far more than 16384 cells at precision 7, so
              ;; the insert cover is capped and stored coarsely.
              (setq big-id (id (make-scope-zone
                                :extent (scope-rect 10d0 40d0 11d0 41d0)))))
            (let ((idx (spatial-index-for g 'scope-zone 'extent)))
              (is (< (spatial-index-coarsest-precision idx)
                     (spatial-index-precision idx))
                  "the oversized polygon really did coarsen the index"))
            (close-graph g :snapshot-p nil)))
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (let ((idx (spatial-index-for g 'scope-zone 'extent)))
                 (is (< (spatial-index-coarsest-precision idx)
                        (spatial-index-precision idx))
                     "the clamp came back from the sidecar, not reset to the ~
                      configured precision")
                 (is (has-p big-id
                            (spatial-index-query-bbox idx 10.4d0 40.4d0
                                                      10.6d0 40.6d0))
                     "a small window inside the coarsely-stored polygon still ~
                      finds it after the reopen"))
            (close-graph g :snapshot-p nil)
            (collect-garbage)))))))

(test regenerate-one-index-leaves-the-others
  "REGENERATE-SPATIAL-INDEX rebuilds exactly one (owner . slot) index."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-before (spatial-index-for g 'scope-probe 'geom)))
      (regenerate-spatial-index g 'scope-zone 'extent)
      ;; The untouched index is the SAME struct; the regenerated one is fresh.
      (is (eq probe-before (spatial-index-for g 'scope-probe 'geom)))
      (is (spatial-index-p (spatial-index-for g 'scope-zone 'extent))))))

(test regenerate-one-index-reindexes-its-own-nodes
  "The regenerated index still answers for every live node of its owner class."
  (with-test-graph (g)
    (let (zone-id)
      (with-transaction ()
        (setq zone-id (id (make-scope-zone
                           :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))))
      (is (= 1 (regenerate-spatial-index g 'scope-zone 'extent)))
      (let ((idx (spatial-index-for g 'scope-zone 'extent)))
        (is (has-p zone-id
                   (spatial-index-query-bbox idx 30d0 48d0 31d0 49d0)))))))

(test regenerate-all-spatial-indexes-persists-roots
  "REGENERATE-SPATIAL-INDEXES rebuilds every index and re-saves the sidecar, so a
reopen sees the NEW roots rather than the freed ones."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) probe-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq probe-id (id (make-scope-probe
                                :geom (make-point 37.1724d0 49.2020d0)))))
          (is (eq g (regenerate-spatial-indexes g)))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((idx (spatial-index-for g 'scope-probe 'geom)))
               (is (spatial-index-p idx))
               (is (has-p probe-id
                          (spatial-index-query-bbox idx 37.16d0 49.19d0
                                                    37.19d0 49.21d0))))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Fix round 1: REBUILD-SPATIAL-INDEXES stranding a freed root; REGENERATE-
;;; SPATIAL-INDEX unable to tell "no such index" from "no live nodes".
;;; ---------------------------------------------------------------------------

(test rebuild-with-no-live-nodes-does-not-strand-a-freed-root
  "REBUILD-SPATIAL-INDEXES on a graph whose geometry-bearing nodes are all deleted
must persist the resulting EMPTY registry immediately.  Before the fix, the
sidecar was only ever rewritten as a side effect of %SPATIAL-INDEX-FOR creating a
replacement index -- so with nothing left to reindex, it kept naming the address
DELETE-SPATIAL-INDEX had just freed.  That sidecar is perfectly READABLE, so the
unreadable-sidecar fallback in RESTORE-SPATIAL-INDEX-ROOTS never gets a chance to
catch it: a crash before the next clean CLOSE-GRAPH would have OPEN-SPATIAL-INDEX
map freed pages on the next open."
  (with-test-graph (g)
    (let (zone)
      (with-transaction ()
        (setq zone (make-scope-zone
                    :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))
      ;; %SPATIAL-INDEX-FOR persisted this index's root the moment it was
      ;; created, so the sidecar is already up to date and names a live address.
      (is (spatial-index-p (spatial-index-for g 'scope-zone 'extent)))
      (mark-deleted zone)
      (is (= 0 (rebuild-spatial-indexes g))
          "no live geometry-bearing node is left to reindex")
      ;; Simulate the crash-then-reopen WITHOUT going through CLOSE-GRAPH -- which
      ;; itself re-saves the roots from whatever is currently in RAM and would mask
      ;; exactly the bug under test.  RESTORE-SPATIAL-INDEX-ROOTS is what OPEN-GRAPH
      ;; calls to repopulate the registry from the sidecar against an
      ;; already-mapped INDEXES heap, so resetting the in-RAM registry and rerunning
      ;; it against the SAME heap is a faithful reopen of just the sidecar contract.
      (clrhash (spatial-indexes g))
      (is (graph-db::restore-spatial-index-roots g)
          "the sidecar is well-formed and readable -- exactly the crash path the ~
           unreadable-sidecar fallback cannot cover")
      (is (= 0 (hash-table-count (spatial-indexes g)))
          "the sidecar must not still name the index REBUILD-SPATIAL-INDEXES just ~
           freed -- the registry must come back correctly EMPTY, not mapping a ~
           freed heap address")
      (is (null (spatial-index-for g 'scope-zone 'extent))))))

(test regenerate-unregistered-owner-warns
  "REGENERATE-SPATIAL-INDEX warns when OWNER-NAME does not name any vertex or edge
type registered on the graph.  Without the warning, the 0 it (correctly) returns
is indistinguishable from a real index whose nodes were all deleted -- a plausible
operator mistake, since a shared index may be declared on an ancestor class."
  (with-test-graph (g)
    (let ((warnings '()) count)
      (handler-bind ((warning (lambda (w) (push w warnings) (muffle-warning w))))
        (setq count (regenerate-spatial-index g 'no-such-spatial-owner 'extent)))
      (is (= 0 count) "still a diagnostic, not an error -- 0 is still returned")
      (is (= 1 (length warnings))
          "expected exactly one warning about the unregistered owner, got ~D"
          (length warnings))
      (let ((msg (princ-to-string (first warnings))))
        (is (search "NO-SUCH-SPATIAL-OWNER" msg)
            "the warning must name the owner: ~S" msg)
        (is (search "EXTENT" msg) "the warning must name the slot: ~S" msg)))))

(test regenerate-registered-owner-with-no-live-nodes-does-not-warn
  "The new warning is precise: a REAL index whose owner class IS registered, just
with no live geometry-bearing nodes left, must NOT warn -- only an owner that
resolves to no registered type at all should."
  (with-test-graph (g)
    (let (zone warnings)
      (with-transaction ()
        (setq zone (make-scope-zone
                    :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))
      (mark-deleted zone)
      (let (count)
        (handler-bind ((warning (lambda (w) (push w warnings) (muffle-warning w))))
          (setq count (regenerate-spatial-index g 'scope-zone 'extent)))
        (is (= 0 count))
        (is (null warnings)
            "SCOPE-ZONE is a registered type; no live nodes is not a diagnostic")))))

;;; ---------------------------------------------------------------------------
;;; Fix round 2: an incomplete v3 sidecar (a rebuild that crashed partway
;;; through) must not be trusted.  6e1462b closed the freed-root window by
;;; having REBUILD-SPATIAL-INDEXES persist the now-empty registry immediately
;;; after CLRHASH -- correct for that purpose, but it means a rebuild that
;;; crashes BEFORE recreating every index now leaves a readable, well-formed,
;;; INCOMPLETE sidecar: the next open trusted it, returned T, and left whichever
;;; index the crash never reached silently unindexed forever.
;;; ---------------------------------------------------------------------------

(test crash-mid-rebuild-forces-a-full-rederive
  "REBUILD-SPATIAL-INDEXES marks the sidecar :COMPLETE NIL before reindexing and
:COMPLETE T only once every index is back in place.  A crash in between -- here,
after index A (SCOPE-PROBE/GEOM) has been recreated and repopulated but before
index B (SCOPE-ZONE/EXTENT) is ever touched -- must leave a sidecar that
RESTORE-SPATIAL-INDEX-ROOTS refuses to trust, even though it reads back cleanly
and is internally well-formed.  Before the fix nothing marked it incomplete: the
next open would have seen a readable format-3 file, returned T, and B would have
stayed silently unindexed forever -- indistinguishable from a legitimate
declared-but-empty index."
  (with-test-graph (g)
    (let (probe-id zone-id)
      (with-transaction ()
        (setq probe-id (id (make-scope-probe
                            :geom (make-point 37.1724d0 49.2020d0))))
        (setq zone-id (id (make-scope-zone
                           :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))))
      ;; Both indexes are live and queryable before the simulated crash.
      (is (spatial-index-p (spatial-index-for g 'scope-probe 'geom)))
      (is (spatial-index-p (spatial-index-for g 'scope-zone 'extent)))
      ;; Reproduce REBUILD-SPATIAL-INDEXES's own opening moves by hand: drop
      ;; every current index, clear the registry, mark the sidecar INCOMPLETE --
      ;; exactly what the function does immediately after CLRHASH.
      (dolist (idx (all-spatial-indexes g))
        (when (graph-db::view-index-p (graph-db::spatial-index-skip-list idx))
          (delete-spatial-index idx)))
      (clrhash (spatial-indexes g))
      (graph-db::save-spatial-index-roots g :complete nil)
      ;; Now reproduce the reindexing loop, but ONLY for A -- this IS the crash:
      ;; B is never recreated.  (%SPATIAL-INDEX-FOR no longer writes a sidecar on
      ;; index creation, so nothing overwrites the incomplete marker set above
      ;; with a premature :COMPLETE T.)
      (spatial-index-insert
       (graph-db::%spatial-index-for g 'scope-probe 'geom)
       probe-id (make-point 37.1724d0 49.2020d0))
      ;; -- crash here; the closing COMPLETE save at the end of REBUILD-SPATIAL-
      ;; INDEXES never runs. --
      ;; Simulate the reopen WITHOUT going through CLOSE-GRAPH (which would
      ;; re-save from RAM and mask the very bug under test): reset the in-RAM
      ;; registry and ask RESTORE-SPATIAL-INDEX-ROOTS to repopulate it against
      ;; the same heap, exactly as OPEN-GRAPH does.
      (clrhash (spatial-indexes g))
      (is (null (graph-db::restore-spatial-index-roots g))
          "an INCOMPLETE sidecar must not be trusted, even though it reads back ~
           cleanly and is internally well-formed")
      (is (= 0 (hash-table-count (spatial-indexes g)))
          "RESTORE-SPATIAL-INDEX-ROOTS must not partially populate the registry ~
           from an incomplete sidecar")
      ;; OPEN-GRAPH's own fallback when RESTORE-SPATIAL-INDEX-ROOTS returns NIL:
      ;; rebuild from the live nodes, which remain authoritative.
      (rebuild-spatial-indexes g)
      (let ((probe-ix (spatial-index-for g 'scope-probe 'geom))
            (zone-ix (spatial-index-for g 'scope-zone 'extent)))
        (is (spatial-index-p probe-ix))
        (is (spatial-index-p zone-ix)
            "index B must come back after the fallback rebuild, not stay ~
             silently unindexed forever")
        (is (has-p probe-id
                   (spatial-index-query-bbox probe-ix 37.16d0 49.19d0
                                             37.19d0 49.21d0)))
        (is (has-p zone-id
                   (spatial-index-query-bbox zone-ix 22d0 44d0 41d0 53d0))
            "B's node must actually be findable, not merely present as a struct")))))

(test sidecar-without-complete-key-restores-normally
  "A sidecar plist with no :COMPLETE key at all -- the exact shape SAVE-SPATIAL-
INDEX-ROOTS wrote before this marker existed -- must still restore normally: the
DESTRUCTURING-BIND (COMPLETE T) default is what keeps a graph already on the v3
format from being forced into a needless rebuild by this change."
  (with-test-graph (g)
    (let (probe-id)
      (with-transaction ()
        (setq probe-id (id (make-scope-probe
                            :geom (make-point 37.1724d0 49.2020d0)))))
      ;; The commit path no longer writes the sidecar (it is written at CLOSE-GRAPH
      ;; and by the rebuild/regenerate ops); write it explicitly here, then rewrite
      ;; it in the PRE-MARKER shape, with no :COMPLETE key in the plist at all.
      (graph-db::save-spatial-index-roots g)
      (let* ((file (graph-db::spatial-indexes-root-file (namestring (graph-db:location g))))
             (plist (cl-store:restore file)))
        (is (getf plist :complete)
            "sanity check: the current SAVE-SPATIAL-INDEX-ROOTS does write ~
             :COMPLETE T here")
        (cl-store:store (list :format (getf plist :format)
                              :indexes (getf plist :indexes))
                        file))
      (clrhash (spatial-indexes g))
      (is (graph-db::restore-spatial-index-roots g)
          "a sidecar with no :COMPLETE key must restore as complete, not fall ~
           back to a rebuild")
      (let ((idx (spatial-index-for g 'scope-probe 'geom)))
        (is (spatial-index-p idx))
        (is (has-p probe-id
                   (spatial-index-query-bbox idx 37.16d0 49.19d0
                                             37.19d0 49.21d0)))))))

;;; ---------------------------------------------------------------------------
;;; The required query scope (§6).  A scope both SELECTS the indexes scanned and
;;; FILTERS the results by type -- both halves are needed, because a geometry
;;; slot declared on a mixin gives its subclasses ONE shared index.
;;; ---------------------------------------------------------------------------

(test scope-excludes-the-other-class-both-directions
  "A query scoped to A returns no B nodes even though B's polygon contains
every A point, and scoping to B returns no A nodes."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (let ((probes (find-nodes-within 'scope-probe window :graph g))
            (zones  (find-nodes-within 'scope-zone window :graph g)))
        (is (= 1 (length probes)))
        (is (every #'scope-probe-p probes))
        (is (= 1 (length zones)))
        (is (every #'scope-zone-p zones))))))

(test scope-accepts-a-class-list-and-dedups
  "A list scope unions the named classes; :ALL unions everything."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (is (= 2 (length (find-nodes-within '(scope-probe scope-zone) window :graph g))))
      (is (= 2 (length (find-nodes-within :all window :graph g)))))))

;; A vertex with no geometry of any kind -- neither an :INDEX-marked geometry
;; slot nor a NODE-GEOMETRY method.  This, not GEO-PLACE, is what "not a spatial
;; class" means: GEO-PLACE overrides NODE-GEOMETRY and IS scopeable.
(def-vertex scope-aspatial ()
  ((label :type string))
  :graph-db-integration-test)

(test unscoped-class-signals-declared-empty-returns-nil
  "A class with no geometry at all signals; a declared-but-empty one is NIL."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (signals error (find-nodes-within 'scope-aspatial window :graph g))
      ;; SCOPE-ZONE is declared but nothing was written: empty, not an error.
      (is (null (find-nodes-within 'scope-zone window :graph g))))))

(test custom-node-geometry-classes-are-scopeable
  "Overriding NODE-GEOMETRY is a documented extension point, so such a class is
scopeable by name -- not reachable only through :ALL.  GEO-PLACE (defined in
spatial-hook-tests.lisp) has a hand-written method and no :INDEX-marked slot."
  (with-test-graph (g)
    (with-transaction ()
      (make-geo-place :loc (make-point 37.1724d0 49.2020d0))
      (make-scope-probe :geom (make-point 37.1730d0 49.2025d0)))
    (let ((window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)))
      (let ((places (find-nodes-within 'geo-place window :graph g)))
        (is (= 1 (length places)))
        (is (every #'geo-place-p places)))
      ;; ...and scoping to the slot-declared class still excludes it.
      (is (every #'scope-probe-p (find-nodes-within 'scope-probe window :graph g))))))

;;; A geometry-bearing PARENT whose geometry comes from a hand-written
;;; NODE-GEOMETRY method, plus a subclass that inherits that method.  The method
;;; is the only geometry declaration either class has, so both are keyed
;;; (SCOPE-SITE . NIL) -- the METHOD OWNER, resolved most-general-first, exactly
;;; as an :INDEX slot declared on a parent is.
(def-vertex scope-site ()
  ((where))
  :graph-db-integration-test)

(def-vertex scope-outpost (scope-site)
  ()
  :graph-db-integration-test)

(defmethod node-geometry ((s scope-site))
  (slot-value s 'where))

(test method-owner-shares-one-index-across-a-hierarchy
  "A NODE-GEOMETRY method on a PARENT gives its subclasses ONE shared index, and a
scope on the parent finds the subclass's nodes.

Keying such a node by its OWN class -- which is what falling back to
%INDEXED-SLOT-OWNER-NAME with a NIL slot does -- would scatter the hierarchy
across per-subclass indexes and make this parent scope miss the outpost entirely."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-site :where (make-point 37.1724d0 49.2020d0))
      (make-scope-outpost :where (make-point 37.1730d0 49.2025d0)))
    (is (spatial-index-p (spatial-index-for g 'scope-site nil))
        "the shared index is keyed by the METHOD OWNER")
    (is (null (spatial-index-for g 'scope-outpost nil))
        "no per-subclass index was created")
    (let ((window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)))
      (let ((both (find-nodes-within 'scope-site window :graph g)))
        (is (= 2 (length both)) "the parent scope spans the subclass")
        (is (some #'scope-outpost-p both)))
      ;; ...and the type filter is still what discriminates within that one index.
      (let ((subs (find-nodes-within 'scope-outpost window :graph g)))
        (is (= 1 (length subs)))
        (is (every #'scope-outpost-p subs))))))

(test method-owner-unindex-is-symmetric
  "Deleting a subclass node removes it from the METHOD OWNER's index -- the key its
insert used.  An insert and a remove that disagreed about the owner would leave
the entry behind forever, un-removable, since nothing else ever visits that key."
  (with-test-graph (g)
    (let (outpost)
      (with-transaction ()
        (make-scope-site :where (make-point 37.1724d0 49.2020d0))
        (setq outpost (make-scope-outpost :where (make-point 37.1730d0 49.2025d0))))
      (let ((window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)))
        (is (= 2 (length (find-nodes-within 'scope-site window :graph g))))
        (with-transaction () (mark-deleted (lookup-vertex (id outpost))))
        (is (= 1 (length (find-nodes-within 'scope-site window :graph g)))
            "the deleted subclass node is gone from the shared index")
        ;; The index entry itself must be gone, not merely filtered out by the
        ;; liveness check the query applies on top of the candidate ids.
        (is (not (has-p (id outpost)
                        (spatial-index-query-bbox
                         (spatial-index-for g 'scope-site nil)
                         37.0d0 49.0d0 37.5d0 49.5d0)))
            "the raw index entry was removed, not just filtered by deleted-p")))))

(test method-owner-survives-a-rebuild
  "REBUILD-SPATIAL-INDEXES re-derives the same (METHOD-OWNER . NIL) key the write
path used.  A rebuild that resolved the owner differently would quietly move every
such node into a different index than queries and removes look in."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-site :where (make-point 37.1724d0 49.2020d0))
      (make-scope-outpost :where (make-point 37.1730d0 49.2025d0)))
    (is (= 2 (rebuild-spatial-indexes g)))
    (is (spatial-index-p (spatial-index-for g 'scope-site nil)))
    (is (null (spatial-index-for g 'scope-outpost nil)))
    (is (= 2 (length (find-nodes-within 'scope-site
                                        (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)
                                        :graph g))))))

(test prolog-scope-shapes
  "The Prolog functors accept a symbol scope, a LITERAL LIST of class names, and
:ALL.  The list is the shape that needed pinning: if the query compiler mangled it
-- into its first element, or into a term that never resolves to class names --
the documented Prolog scope would have to be restricted to symbol-or-:ALL, with
multi-class queries routed through a disjunction instead.

The wide radius on the list case is deliberate.  It encloses BOTH nodes (the zone
polygon's centre is ~480 km from the probe point), so a list scope that had
silently collapsed to its first element would return 1 here rather than 2 -- which
the narrow-radius symbol case above cannot distinguish."
  (with-test-graph (g)
    (let ((*graph* g))
      (with-transaction ()
        (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
        (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
      (is (= 1 (length (select-flat (?n) (find-near ?n scope-probe
                                                    49.2020d0 37.1724d0 500.0d0)))))
      (is (= 2 (length (select-flat (?n) (find-near ?n (scope-probe scope-zone)
                                                    49.2020d0 37.1724d0 1.0d6))))
          "a literal list scope survives the Prolog compiler and unions both classes")
      (is (= 1 (length (select-flat (?n) (find-near ?n (scope-probe)
                                                    49.2020d0 37.1724d0 1.0d6))))
          "a one-element literal list still resolves to a valid scope")
      (is (<= 1 (length (select-flat (?n) (find-near ?n :all
                                                     49.2020d0 37.1724d0 500.0d0))))))))

;;; ---------------------------------------------------------------------------
;;; Eager scope validation (§6).  Signalling on an unscopeable class is the whole
;;; point of a required scope, so it must not be reachable only when the payload
;;; argument happens to be well-formed.
;;; ---------------------------------------------------------------------------

(test bad-scope-signals-even-with-a-bad-payload
  "Every entry point resolves -- and therefore VALIDATES -- the scope BEFORE its
payload guard.  Guarding on the payload first made (FIND-NODES-WITHIN 'BOGUS NIL)
quietly return NIL, so a call that was wrong in BOTH arguments reported neither.

The second half pins the other side of the line: a payload that is junk on a
SCOPEABLE class is still an empty result, not an error."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (signals error (find-nodes-within 'scope-aspatial nil :graph g))
    (signals error (find-nodes-intersecting 'scope-aspatial nil :graph g))
    (signals error (find-nodes-near 'scope-aspatial nil nil nil :graph g))
    (signals error (find-nearest-k 'scope-aspatial nil nil nil :graph g))
    (is (null (find-nodes-within 'scope-probe nil :graph g)))
    (is (null (find-nodes-intersecting 'scope-probe nil :graph g)))
    (is (null (find-nodes-near 'scope-probe nil nil nil :graph g)))
    (is (null (find-nearest-k 'scope-probe nil nil nil :graph g)))))

;;; ---------------------------------------------------------------------------
;;; §5: the ONE declaration surface -- the :SPATIAL-PRECISION slot option -- its
;;; inheritance, its range check, and the open-time reconcile that adopts a
;;; changed declaration.
;;; ---------------------------------------------------------------------------

(def-vertex scope-coarse ()
  ((extent :type geometry :index t :spatial-precision 3))
  :graph-db-integration-test)

;; Adds nothing of its own: the geometry slot AND its declared precision have to
;; arrive by effective-slot inheritance.
(def-vertex scope-coarse-sub (scope-coarse)
  ()
  :graph-db-integration-test)

(test slot-option-sets-index-precision
  "A :SPATIAL-PRECISION slot option is the index's grid precision."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-coarse :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((idx (spatial-index-for g 'scope-coarse 'extent)))
      (is (= 3 (spatial-index-precision idx)))
      ;; At p=3 a country-scale polygon is ~98 cells, so the cap never fires and
      ;; the clamp stays at the index's own precision.
      (is (= 3 (spatial-index-coarsest-precision idx))))))

(test slot-option-inherits-to-a-subclass
  "The :SPATIAL-PRECISION option inherits through COMPUTE-EFFECTIVE-SLOT-DEFINITION,
exactly as :INDEX / :UNIQUE / :VECTOR-INDEX do -- so a geometry slot declared on a
parent carries ONE grid precision across every subclass sharing its index.

Asserted on the SUBCLASS's own effective slot, not only through the shared index.
That inheritance is what a per-implementation reader conditional missing a branch
would silently drop, leaving the option NIL on that implementation while a
same-class test went on passing everywhere."
  (let ((slot (find 'extent (graph-db::class-slots (find-class 'scope-coarse-sub))
                    :key #'graph-db::slot-definition-name)))
    (is (not (null slot)) "the subclass really does have the inherited slot")
    (is (eql 3 (spatial-precision-spec slot))
        "the effective slot carries the parent's declared precision"))
  ;; ...and the write path puts a subclass node in the PARENT's index, at that
  ;; precision -- the behaviour the inheritance exists for.
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-coarse-sub :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (is (null (spatial-index-for g 'scope-coarse-sub 'extent))
        "no per-subclass index was created")
    (is (= 3 (spatial-index-precision (spatial-index-for g 'scope-coarse 'extent))))))

;; An out-of-range precision on a class that is NOT a graph node type: the range
;; check lives in the RESOLVER, and this pins its message without putting a class
;; the write path would choke on into the integration graph's schema.
(defclass scope-bad-precision ()
  ((extent :initarg :extent :index t :spatial-precision 15))
  (:metaclass graph-db::node-class))

(defclass scope-float-precision ()
  ((extent :initarg :extent :index t :spatial-precision 5.0))
  (:metaclass graph-db::node-class))

(test out-of-range-precision-is-rejected-by-name
  "%MAKE-SPATIAL-INDEX's slot type is (INTEGER 1 12), but reaching it means a raw
structure type error at the first geometry-valued write -- and at low safety on
ECL, possibly no error at all.  The resolver checks instead, because it is the
last point that still knows WHICH class and slot declared the bad value.

Both an out-of-range integer and a non-integer are rejected, and the report names
the class and the slot: 15 and 5.0 are typos a schema author makes, and 'the value
15 is not of type (INTEGER 1 12)' does not say where to go and fix it."
  (graph-db::finalize-inheritance (find-class 'scope-bad-precision))
  (graph-db::finalize-inheritance (find-class 'scope-float-precision))
  (let ((err (handler-case
                 (progn (graph-db::%declared-spatial-precision
                         'scope-bad-precision 'extent)
                        nil)
               (error (e) (princ-to-string e)))))
    (is (and err (search "SCOPE-BAD-PRECISION" (string-upcase err))
             (search "EXTENT" (string-upcase err)))
        "an out-of-range integer is refused, naming the class and the slot"))
  (signals error (graph-db::%declared-spatial-precision
                  'scope-float-precision 'extent)))

;; The staleness fixture, kept OFF the classes other tests assert on: the test
;; redefines it mid-run and puts it back, and a failure between the two would
;; otherwise poison whatever else read that precision.
(def-vertex scope-restale ()
  ((extent :type geometry :index t :spatial-precision 6))
  :graph-db-integration-test)

(test declared-precision-change-rebuilds-that-index-at-open
  "A declared precision that no longer matches the PERSISTED one rebuilds that ONE
index at open, rather than waiting to be asked.  An index holding cells at two
precisions reintroduces the covering-precision miss the clamp exists to prevent,
so adopting the change lazily would be silently wrong.

With the slot option as the only declaration surface, changing a declaration means
REDEFINING THE CLASS -- so that is what this does, between the close and the
reopen, and it puts the definition back in an UNWIND-PROTECT.  SCOPE-RESTALE
exists only for this: it is redefined twice per run, and no other test reads it.

The untouched index is the control, and it is pinned by a CANARY: an entry under
an id no live node has, written straight into the index before the close.  A
restore brings the persisted ordered map back with the canary in it; a rebuild
repopulates from live nodes only, so the canary cannot survive one.

Two weaker pins were tried and rejected, both of which PASS even under a
whole-graph rebuild and so assert nothing:

  - The index's PRECISION.  %SPATIAL-PRECISION-FOR yields the 7 default for a slot
    with no declaration whether the index was restored or rebuilt from scratch.
    (Kept below as a sanity check, no longer as the claim.)
  - The index's ROOT ADDRESS.  REBUILD-SPATIAL-INDEXES frees the old ordered map
    and immediately allocates a new one, and the binned allocator hands back the
    block it just freed -- measured, same address before and after, for both
    REGENERATE-SPATIAL-INDEX and REBUILD-SPATIAL-INDEXES.  Address identity is
    not index identity here.

Warnings are muffled throughout, for two reasons.  A country-scale polygon at
precision 5 or 6 is far past +SPATIAL-INSERT-MAX-CELLS+, so the EXPECTED coarsening
warning fires on every insert and on every rebuild; and redefining a class at
runtime redefines its generated constructor, which is a STYLE-WARNING on some
implementations.  Neither bears on what is under test -- SPATIAL-INDEX-PRECISION
reports the CONFIGURED precision, which the clamp never moves.

TEST HYGIENE.  The UNWIND-PROTECT below restores the CLASS, not global schema
state: each run pushes two more SCOPE-RESTALE metas onto *SCHEMA-NODE-METADATA*,
and nothing prunes them, so every later UPDATE-SCHEMA iterates a slightly longer
list.  It is bounded (two per run) and benign (newest-wins on resolution, and the
type-id is preserved across the redefinition), but do not read the UNWIND-PROTECT
as putting the schema registry back the way it was."
  (handler-bind ((warning #'muffle-warning))
    (with-temp-directory (dir)
      (let ((path (namestring dir))
            (canary (graph-db::gen-vertex-id))
            zone-id)
        (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq zone-id (id (make-scope-restale
                                 :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))
              (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
            (is (= 6 (spatial-index-precision
                      (spatial-index-for g 'scope-restale 'extent))))
            (is (= 7 (spatial-index-precision
                      (spatial-index-for g 'scope-probe 'geom))))
            ;; The canary: an id no node has, in a corner of the world nothing
            ;; else occupies, so a rebuild of this index cannot reproduce it.
            (spatial-index-insert (spatial-index-for g 'scope-probe 'geom)
                                  canary (make-point 10d0 10d0))
            (close-graph g :snapshot-p nil)))
        (unwind-protect
             (progn
               ;; Edit the schema: the same slot, at a different precision.
               (def-vertex scope-restale ()
                 ((extent :type geometry :index t :spatial-precision 5))
                 :graph-db-integration-test)
               (let ((g (open-graph *integration-graph-name* path)))
                 (unwind-protect
                      (let ((zone-ix (spatial-index-for g 'scope-restale 'extent)))
                        (is (= 5 (spatial-index-precision zone-ix))
                            "the index adopted the newly declared precision at open")
                        (is (has-p zone-id
                                   (spatial-index-query-bbox zone-ix
                                                             30d0 48d0 31d0 49d0))
                            "and was repopulated from the live nodes, not left empty")
                        (is (has-p canary
                                   (spatial-index-query-bbox
                                    (spatial-index-for g 'scope-probe 'geom)
                                    9d0 9d0 11d0 11d0))
                            "the index nobody redeclared still holds its canary --
it was RESTORED from the sidecar, not repopulated from live nodes")
                        (is (= 7 (spatial-index-precision
                                  (spatial-index-for g 'scope-probe 'geom)))
                            "and still reports its own precision"))
                   (close-graph g :snapshot-p nil)
                   (collect-garbage))))
          ;; Back to 6, whatever happened above.
          (def-vertex scope-restale ()
            ((extent :type geometry :index t :spatial-precision 6))
            :graph-db-integration-test))))))

(test graph-default-precision-does-not-rebuild-a-persisted-index
  "The graph default is the precision for indexes CREATED after the open, not a
declaration ABOUT the existing ones: reopening without :SPATIAL-PRECISION (so, at
the 7 default) must NOT silently rebuild an index the graph was created at 5.

Only an explicit declaration -- the :SPATIAL-PRECISION slot option -- is a
statement about a particular index, and only that triggers the rebuild.  Treating
the fallback default as a declaration would make OPEN-GRAPH's documented
\"existing indexes reopen at their own persisted precision\" false, and would
silently re-grid a graph whose owner merely forgot the keyword.

Warnings are muffled for the same reason as the test above: at precision 5 this
polygon coarsens, and that is expected and beside the point here."
  (handler-bind ((warning #'muffle-warning))
    (with-temp-directory (dir)
      (let ((path (namestring dir)) zone-id)
        (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000
                                                           :spatial-precision 5)))
          (let ((*graph* g))
            (with-transaction ()
              (setq zone-id (id (make-scope-zone
                                 :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))))
            (is (= 5 (spatial-index-precision
                      (spatial-index-for g 'scope-zone 'extent))))
            (close-graph g :snapshot-p nil)))
        (let ((g (open-graph *integration-graph-name* path)))
          (unwind-protect
               (let ((idx (spatial-index-for g 'scope-zone 'extent)))
                 (is (= 5 (spatial-index-precision idx))
                     "the persisted precision survived a reopen at the default")
                 (is (has-p zone-id
                            (spatial-index-query-bbox idx 30d0 48d0 31d0 49d0))))
            (close-graph g :snapshot-p nil)
            (collect-garbage)))))))

;;; ---------------------------------------------------------------------------
;;; A geometry-bearing EDGE.  Edges are spatially indexed exactly as vertices are
;;; -- REBUILD-SPATIAL-INDEXES maps both, and REGENERATE-SPATIAL-INDEX has an edge
;;; path -- so every teardown path has to release them symmetrically.
;;; ---------------------------------------------------------------------------

(def-edge scope-route ()
  ((path :type geometry :index t))
  :graph-db-integration-test)

(test peer-purge-releases-a-geometry-edge
  "PEER-PURGE-NODE's EDGE branch must unindex the edge's geometry, exactly as its
VERTEX branch does and as it already does for the unique index, the general
ordered index and the views in BOTH branches.

A purge leaves no tombstone and no node, so nothing ever visits that index key
again: a missed unindex orphans the entry permanently, and the id it names now
resolves to nothing -- a device that purged an undisclosed geometry-bearing edge
would keep answering spatial queries with its ghost."
  (with-test-graph (g)
    (let (edge-id)
      (with-transaction ()
        (let ((a (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
              (b (make-scope-probe :geom (make-point 37.1730d0 49.2025d0))))
          (setq edge-id (id (make-scope-route
                             :from a :to b :weight 1.0
                             :path (make-point 37.1727d0 49.2022d0))))))
      (let ((idx (spatial-index-for g 'scope-route 'path)))
        (is (spatial-index-p idx) "the edge's geometry was indexed on create")
        (is (has-p edge-id
                   (spatial-index-query-bbox idx 37.17d0 49.20d0 37.18d0 49.21d0))))
      (graph-db::peer-purge-node g (lookup-edge edge-id))
      (let ((idx (spatial-index-for g 'scope-route 'path)))
        (is (not (has-p edge-id
                        (spatial-index-query-bbox idx 37.17d0 49.20d0
                                                  37.18d0 49.21d0)))
            "the purged edge's spatial entry must be gone from the index itself")))))

;;; ---------------------------------------------------------------------------
;;; §8: the inert second geometry slot.
;;;
;;; NODE-GEOMETRY picks the FIRST indexed slot whose runtime value is a geometry,
;;; so a class declaring two of them silently indexes one and drops the other.
;;; This cannot be caught at finalization (the ':type geometry' symbol is read in
;;; the application's package and is not reliably EQ to GRAPH-DB:GEOMETRY, and a
;;; user need not declare a type at all), so the check is value-based, on the
;;; maintenance path -- bounded per class -- with AUDIT-SPATIAL-SLOTS as the
;;; exhaustive sweep.
;;; ---------------------------------------------------------------------------

(def-vertex scope-two-geoms ()
  ((centroid :type geometry :index t)
   (outline :type geometry :index t))
  :graph-db-integration-test)

;; The sampler retires a class for the LIFE OF THE IMAGE once it has warned or
;; spent its budget, so both tests below reset SCOPE-TWO-GEOMS's counter first.
;; Without it the pair is order-dependent -- whichever ran first would consume
;; the budget and leave the other asserting against a retired class -- and
;; FiveAM does not promise a run order.  Resetting one class's counter does not
;; weaken either check: each test then exercises the state it names.
(defun reset-two-geoms-sampler ()
  (remhash (find-class 'scope-two-geoms)
           graph-db::*node-geometry-multi-sample-counts*))

(test warns-on-a-second-inert-geometry-slot
  "A node with two geometry-valued indexed slots warns and names the winner.

The warning TEXT is checked, not merely that some warning was signaled: this
same write path also warns when an oversized insert coarsens an index, so a bare
SIGNALS WARNING here could pass on the wrong condition entirely."
  (with-test-graph (g)
    (declare (ignore g))
    (reset-two-geoms-sampler)
    (let ((text nil))
      (signals warning
        (handler-bind ((warning (lambda (c)
                                  (unless text (setf text (princ-to-string c))))))
          (with-transaction ()
            (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                                  :outline (scope-rect 35d0 55d0 36d0 56d0)))))
      (let ((up (string-upcase (or text ""))))
        (is (search "INERT" up) "the warning is the inert-slot one, not the clamp's")
        (is (search "SCOPE-TWO-GEOMS" up) "and it names the class")
        ;; Both CENTROID and OUTLINE appear in the ~S-printed slot list regardless
        ;; of which one wins, so checking for either symbol alone proves nothing
        ;; about which slot the warning names as the winner.  "ONLY ~S" is the
        ;; format string's own words around the winning slot specifically, so this
        ;; is the assertion that actually discriminates first-wins from second-wins.
        (is (search "ONLY CENTROID" up) "and CENTROID -- the first slot -- is named as the winner")))))

(test warns-once-per-class-not-once-per-node
  "The sampler retires a class the moment it fires, so a bulk load of a
mis-declared class does not emit one warning per node."
  (with-test-graph (g)
    (declare (ignore g))
    (reset-two-geoms-sampler)
    (let ((count 0))
      (handler-bind ((warning (lambda (c)
                                (when (search "INERT" (string-upcase
                                                       (princ-to-string c)))
                                  (incf count))
                                (muffle-warning c))))
        (with-transaction ()
          (dotimes (i 5)
            (make-scope-two-geoms
             :centroid (make-point (+ 30d0 (* i 0.001d0)) 50d0)
             :outline (scope-rect 35d0 55d0 36d0 56d0)))))
      (is (= 1 count) "exactly one inert-slot warning for five bad nodes"))))

;; One geometry slot, two indexed SCALARS.  This is the shape the check must stay
;; silent on, and it is the common one: :INDEX is the general ordered-index option
;; as well as the spatial opt-in, so NODE-GEOMETRY-INDEX-SLOTS hands back LABEL and
;; RANK here alongside GEOM.  Without a class of this shape in the fixture, both
;; the silence test and the read-only test below would assert nothing.
(def-vertex scope-mixed-index ()
  ((geom :type geometry :index t)
   (label :type string :index t)
   (rank :type integer :index t))
  :graph-db-integration-test)

(test one-geometry-slot-and-indexed-scalars-stay-silent
  "The check must not cry wolf on a class that is working as intended.
NODE-GEOMETRY-INDEX-SLOTS returns EVERY :INDEX-marked slot -- :INDEX is also the
general ordered-index option -- so a class with one geometry slot and several
indexed scalars is the ordinary case and must stay silent.

This is the shape four classes in the requesting team's schema have, minus the
second geometry; getting it wrong would bury the real finding in noise."
  (with-test-graph (g)
    (remhash (find-class 'scope-mixed-index)
             graph-db::*node-geometry-multi-sample-counts*)
    (let ((warned nil))
      (handler-bind ((warning (lambda (c)
                                (when (search "INERT" (string-upcase
                                                       (princ-to-string c)))
                                  (setf warned (princ-to-string c)))
                                (muffle-warning c))))
        (with-transaction ()
          (make-scope-mixed-index :geom (make-point 37.1724d0 49.2020d0)
                                  :label "alpha" :rank 3)))
      (is (null warned)
          "one geometry slot plus two indexed scalars never warns")
      (is (equal '(geom)
                 (graph-db::node-geometry-slots-with-values
                  (first (map-vertices #'identity g :vertex-type 'scope-mixed-index
                                                    :collect-p t))))
          "and only the geometry slot is reported as geometry-valued"))))

;; Two :INDEX geometry slots PLUS an application-supplied NODE-GEOMETRY method --
;; the engine's own documented workaround for wanting more than one geometry-
;; valued input (see NODE-GEOMETRY's docstring): combine them into one geometry
;; and return it alone, with no second (slot-name) value.  Such a node is indexed
;; under (METHOD-OWNER . NIL) -- the "first indexed slot wins" default resolution
;; never runs at all -- so this is NOT the inert-slot shape, even though
;; NODE-GEOMETRY-SLOTS-WITH-VALUES (a raw slot-value walk, blind to NODE-GEOMETRY
;; methods) still finds both CENTROID and OUTLINE holding geometries.  Both the
;; sampler and the audit must recognize that and stay silent.
(def-vertex scope-two-geoms-combined ()
  ((centroid :type geometry :index t)
   (outline :type geometry :index t))
  :graph-db-integration-test)

(defmethod node-geometry ((s scope-two-geoms-combined))
  ;; Stand-in for a real combinator (e.g. a bounding union): what the check under
  ;; test cares about is only that this returns ONE value, not which geometry.
  (or (slot-value s 'centroid) (slot-value s 'outline)))

(defun reset-two-geoms-combined-sampler ()
  (remhash (find-class 'scope-two-geoms-combined)
           graph-db::*node-geometry-multi-sample-counts*))

(test custom-node-geometry-method-silences-the-sampler
  "The documented single-method workaround must not trip the inert-slot warning.

Without the guard at %SPATIAL-INDEX-NODE (only sample when NODE-GEOMETRY handed
back a SLOT name), this class would warn on every node: NODE-GEOMETRY-SLOTS-WITH-
VALUES finds CENTROID and OUTLINE both holding geometries regardless of the
method, exactly as it does for SCOPE-TWO-GEOMS."
  (with-test-graph (g)
    (declare (ignore g))
    (reset-two-geoms-combined-sampler)
    (let ((warned nil))
      (handler-bind ((warning (lambda (c)
                                (when (search "INERT" (string-upcase
                                                       (princ-to-string c)))
                                  (setf warned (princ-to-string c)))
                                (muffle-warning c))))
        (with-transaction ()
          (make-scope-two-geoms-combined
           :centroid (make-point 30d0 50d0)
           :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (is (null warned)
          "a custom NODE-GEOMETRY method opts the class out of the sampler's check"))))

(test custom-node-geometry-method-silences-the-audit
  "AUDIT-SPATIAL-SLOTS must skip a class with a custom NODE-GEOMETRY method too,
independent of the sampler ever having run -- an operator who wires the audit
into a schema test suite must not get a permanent false failure for following
the engine's own documented workaround."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-two-geoms-combined
       :centroid (make-point 30d0 50d0)
       :outline (scope-rect 35d0 55d0 36d0 56d0)))
    (is (null (assoc 'scope-two-geoms-combined (audit-spatial-slots g)))
        "a method-supplied geometry is not the 'first slot wins' shape the audit checks for")))

;; A custom NODE-GEOMETRY method that reports a SLOT name -- the two-value case
;; the generic's own docstring documents -- is NOT the "combine into one and hide"
;; workaround SCOPE-TWO-GEOMS-COMBINED stands in for.  Such a node IS keyed
;; (CLASS . SLOT) by %NODE-SPATIAL-OWNER-NAME, "first indexed slot wins" DOES
;; apply to it, and a second geometry-valued indexed slot on it IS genuinely
;; inert -- exactly as it would be under the default method.  This is the shape
;; finding 1 named: gating the audit on "does the class carry ANY custom method"
;; (rather than on NODE-GEOMETRY's own per-node SLOT return, the sampler's actual
;; condition) would silently exempt this class from the audit forever, with no
;; sampler backstop reaching it either once its two-geometry node falls outside
;; the sampling window.
(def-vertex scope-two-geoms-reporting ()
  ((centroid :type geometry :index t)
   (outline :type geometry :index t))
  :graph-db-integration-test)

(defmethod node-geometry ((s scope-two-geoms-reporting))
  ;; Two values: a geometry AND a slot name, unlike SCOPE-TWO-GEOMS-COMBINED's
  ;; one-value return.  The slot named here need not be the class's actual
  ;; winning slot in general, but making it CENTROID lets this test assert the
  ;; warning names the same winner the default method would have chosen.
  (values (or (slot-value s 'centroid) (slot-value s 'outline)) 'centroid))

(test custom-two-value-node-geometry-method-is-still-audited
  "A custom NODE-GEOMETRY method returning (VALUES GEOM SLOT-NAME) opts INTO the
'first slot wins' rule, not out of it -- AUDIT-SPATIAL-SLOTS must report a class
shaped this way exactly as it would a default-method class with the same two
geometry-valued indexed slots.  This is the discriminating case for finding 1:
before the fix, the audit's gate asked only whether the class carried a custom
NODE-GEOMETRY method at all, which wrongly exempted this shape along with the
genuine SCOPE-TWO-GEOMS-COMBINED workaround.

This class also has a real SLOT name, so -- unlike SCOPE-TWO-GEOMS-COMBINED --
the WRITE-path sampler fires its own inert-slot warning on creation, exactly as
it would for a default-method class shaped this way; that warning is muffled
here since it is not what this test is checking."
  (with-test-graph (g)
    (handler-bind ((warning #'muffle-warning))
      (with-transaction ()
        (make-scope-two-geoms-reporting
         :centroid (make-point 30d0 50d0)
         :outline (scope-rect 35d0 55d0 36d0 56d0))))
    (let ((entry (assoc 'scope-two-geoms-reporting (audit-spatial-slots g))))
      (is (not (null entry))
          "a two-value custom method does not exempt its class from the audit")
      (is (eq 'centroid (second entry)) "and the winner is the slot the method named")
      (is (equal '(outline) (cddr entry)) "with the other geometry slot reported inert"))))

(test audit-finds-what-the-sampler-missed
  "AUDIT-SPATIAL-SLOTS sweeps every node, so it reports a class whose only
two-geometry node lies beyond the sampling window.

The premise is asserted, not assumed: the load below spends the class's whole
sampling budget on single-geometry nodes, and the two-geometry node that follows
must draw NO warning.  Muffling alone would hide a sampler that had in fact
caught it, and the test would then pass without the audit doing any work.

Note that the window is a CREATION-order one.  MAP-VERTICES walks the vertex
table in linear-hash order, so the two-geometry node -- created 66th of 66 --
comes back around the middle of the sweep (measured: 36th).  That is why the
sampler's silence is asserted directly rather than inferred from position."
  (with-test-graph (g)
    (reset-two-geoms-sampler)
    (with-transaction ()
      (dotimes (i (1+ graph-db::*node-geometry-multi-sample-limit*))
        (make-scope-two-geoms :centroid (make-point (+ 30d0 (* i 0.001d0)) 50d0))))
    (let ((warned 0))
      (handler-bind ((warning (lambda (c)
                                (when (search "INERT" (string-upcase
                                                       (princ-to-string c)))
                                  (incf warned))
                                (muffle-warning c))))
        (with-transaction ()
          (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                                :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (is (zerop warned)
          "the sampler is retired by now, so this node really is beyond its window"))
    (let ((report (audit-spatial-slots g)))
      (is (assoc 'scope-two-geoms report))
      (is (eq 'centroid (second (assoc 'scope-two-geoms report))))
      (is (equal '(outline) (cddr (assoc 'scope-two-geoms report)))
          "the inert slots follow the winner in the report"))))

(test audit-reports-nothing-on-a-healthy-graph
  "The audit is a diagnostic, not a lint that always finds something: a graph
whose classes each carry ONE geometry slot reports the empty list.

SCOPE-MIXED-INDEX (one geometry slot, two indexed SCALARS) is in the fixture on
purpose: SCOPE-PROBE and SCOPE-ZONE alone each have exactly one indexed slot
period, so neither could catch the audit wrongly walking non-geometry :INDEX
slots into its notion of 'more than one geometry-valued slot' -- that noise is
the actual risk an all-geometry fixture can't defend against."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))
      (make-scope-mixed-index :geom (make-point 37.1730d0 49.2025d0)
                              :label "delta" :rank 2))
    (is (null (audit-spatial-slots g)))))

(test audit-is-read-only
  "AUDIT-SPATIAL-SLOTS must not create, mutate or persist anything -- it is safe
to call on a production graph.  It reads slots, and a slot read must never be
what brings an index into existence.

SCOPE-MIXED-INDEX is what gives this teeth.  %SPATIAL-INDEX-FOR CREATES a missing
index as a side effect, so an audit that resolved an index per indexed slot -- the
natural way to write it wrong -- would mint (SCOPE-MIXED-INDEX . LABEL) and
 (SCOPE-MIXED-INDEX . RANK) spatial indexes over scalar slots.  Those keys do not
exist beforehand, so the key-set comparison below catches it.  A fixture of
geometry-only classes would not: every key it could resolve already exists.

The key-set and root-address checks only see the in-memory registry, though, and
SAVE-SPATIAL-INDEX-ROOTS (a call the audit must never make) writes to the v3
sidecar file, not to any in-memory structure the checks above would notice.  A
prior version of this test tried to catch that by snapshotting the sidecar
file's FILE-WRITE-DATE and length across the call -- but FILE-WRITE-DATE is
integer universal time (one-second granularity), this whole test runs well
inside one second, and SAVE-SPATIAL-INDEX-ROOTS re-serializes the SAME roots to
the SAME file, so an unchanged registry produces identical length AND identical
write-date whether or not the call happened.  Confirmed directly, not assumed:
inserting a bare (SAVE-SPATIAL-INDEX-ROOTS GRAPH) as the first form of
AUDIT-SPATIAL-SLOTS and re-running this test left all four of its assertions
passing.  A check that cannot fail on the exact bug it names is not a check.

So instead of inspecting the file, this counts calls to SAVE-SPATIAL-INDEX-ROOTS
itself, via the FDEFINITION-swap idiom this suite already uses for %POSIX-MMAP
and REBUILD-UNIQUE-INDEXES: zero calls is the only correct count for a read-only
sweep, at any timer resolution, on any backend.

The same idiom also proves the audit commits no write, full stop, rather than
merely that it happens not to touch the two things this test otherwise inspects:
every node create, update and delete in this engine funnels through
APPLY-TRANSACTION, the shared apply path transaction commit, replay and
replication all go through, so a zero count on it during the audit is direct
evidence no node was mutated -- not an inference from the absence of symptoms
in an unrelated registry and a file stat."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-mixed-index :geom (make-point 37.1730d0 49.2025d0)
                              :label "beta" :rank 7))
    (flet ((keys (graph)
             (sort (loop for k being the hash-keys of (spatial-indexes graph)
                         collect (princ-to-string k))
                   #'string<)))
      (let ((keys-before (keys g))
            (addr-before (spatial-index-address
                          (spatial-index-for g 'scope-probe 'geom)))
            (save-calls 0)
            (apply-calls 0)
            (orig-save (fdefinition 'graph-db::save-spatial-index-roots))
            (orig-apply (fdefinition 'graph-db::apply-transaction)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::save-spatial-index-roots)
                     (lambda (&rest args)
                       (incf save-calls)
                       (apply orig-save args)))
               (setf (fdefinition 'graph-db::apply-transaction)
                     (lambda (&rest args)
                       (incf apply-calls)
                       (apply orig-apply args)))
               (audit-spatial-slots g)
               (is (equal keys-before (keys g)) "no index was created or dropped")
               (is (= addr-before (spatial-index-address
                                   (spatial-index-for g 'scope-probe 'geom)))
                   "and the surviving index is the same one, at the same root")
               (is (zerop save-calls)
                   "and the sidecar was never re-saved")
               (is (zerop apply-calls)
                   "and no transaction was ever applied -- the audit commits no write"))
          (setf (fdefinition 'graph-db::save-spatial-index-roots) orig-save)
          (setf (fdefinition 'graph-db::apply-transaction) orig-apply))))))

;; A geometry-bearing EDGE with a second geometry slot: the audit maps edges as
;; well as vertices, and a mis-declared edge class is exactly as inert.
(def-edge scope-two-geom-route ()
  ((track :type geometry :index t)
   (corridor :type geometry :index t))
  :graph-db-integration-test)

(test audit-covers-edges-too
  "AUDIT-SPATIAL-SLOTS maps edges as well as vertices."
  (with-test-graph (g)
    (handler-bind ((warning #'muffle-warning))
      (with-transaction ()
        (let ((a (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
              (b (make-scope-probe :geom (make-point 37.1730d0 49.2025d0))))
          (make-scope-two-geom-route
           :from a :to b :weight 1.0
           :track (make-point 37.1727d0 49.2022d0)
           :corridor (scope-rect 37.17d0 49.20d0 37.18d0 49.21d0)))))
    (let ((entry (assoc 'scope-two-geom-route (audit-spatial-slots g))))
      (is (not (null entry)) "the mis-declared edge class is reported")
      (is (equal '(track corridor) (rest entry))))))

;;; ---------------------------------------------------------------------------
;;; The spatial sidecar is written at CLOSE-GRAPH and by the rebuild/regenerate
;;; admin ops -- NEVER on the commit path.  It used to fire on an index creation
;;; and on a coarsest-precision decrease, which put CL-STORE file I/O under the
;;; transaction-manager lock, on the post-durability side of the commit.  Crash-
;;; correctness of the histogram now comes from OPEN-GRAPH re-deriving the spatial
;;; indexes from the recovered nodes after WAL replay, so the commit path is free
;;; of it.
;;; ---------------------------------------------------------------------------

(test commit-path-does-not-write-the-spatial-sidecar
  "Committing a node whose geometry COARSENS its index -- the exact case that used
to force a synchronous SAVE-SPATIAL-INDEX-ROOTS under the transaction-manager lock
-- must not write the sidecar at all.  No CL-STORE runs on the commit path."
  (with-test-graph (g)
    (let ((saves 0)
          (orig (fdefinition 'graph-db::save-spatial-index-roots)))
      (unwind-protect
           (progn
             (setf (fdefinition 'graph-db::save-spatial-index-roots)
                   (lambda (&rest args) (incf saves) (apply orig args)))
             (with-transaction ()
               ;; a country-scale extent: capped and coarsened at the default p7
               (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
             (is (< (spatial-index-coarsest-precision
                     (spatial-index-for g 'scope-zone 'extent))
                    7)
                 "precondition: the big extent coarsened its index below p7")
             (is (zerop saves)
                 "no SAVE-SPATIAL-INDEX-ROOTS on the commit path"))
        (setf (fdefinition 'graph-db::save-spatial-index-roots) orig)))))

(test coarse-geometry-survives-crash-recovery
  "PROPERTY (not a mutation-discriminator): a country-scale geometry whose index
would be COARSENED is still findable after a crash + recovery, even though the
commit path no longer persists the histogram.  Phase 1 crashes after the lhash
write but before spatial maintenance (via *AFTER-APPLY-TX-WRITES-HOOK*), leaving a
durable-but-unindexed WAL entry; Phase 2 reopens, OPEN-GRAPH replays the WAL and --
because the WAL tail is non-empty -- re-derives the spatial indexes from the
recovered node.

Scope of what this pins: in THIS injectable scenario the index did not exist
pre-crash, so the WAL replay alone reconstructs the histogram (its inserts are all
fresh, so %COUNT-CELL runs), and the recovery rebuild is exercised but not strictly
necessary.  The rebuild's necessity is the narrower case where a coarse cell
reached disk before the crash, so replay's idempotent re-insert is a dup no-op that
skips %COUNT-CELL and leaves a stale-restored clamp too fine -- a state that needs
a post-spatial-apply crash hook (which the engine deliberately does not expose) or
file surgery to construct, so it is covered by reasoning about the mmap durability
model rather than by an in-process mutation test.  What this test guarantees is the
user-visible property: the recovery path yields a correct, queryable spatial index."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          zone-id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (setf graph-db::*after-apply-tx-writes-hook*
                     (lambda () (error "simulated crash before spatial apply")))
               (handler-case
                   (with-transaction ()
                     (setq zone-id (id (make-scope-zone
                                        :extent (scope-rect 22.1d0 44.4d0
                                                            40.2d0 52.4d0)))))
                 (error () nil)))
          (setf graph-db::*after-apply-tx-writes-hook* nil)
          (ignore-errors (close-graph g :snapshot-p nil))))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((idx (spatial-index-for g2 'scope-zone 'extent)))
                 (is (spatial-index-p idx)
                     "the zone index exists after recovery")
                 (is (< (spatial-index-coarsest-precision idx) 7)
                     "recovery reconstructed the coarsened histogram")
                 (is (has-p zone-id
                            (spatial-index-query-bbox idx 30d0 48d0 30.1d0 48.1d0))
                     "the coarse geometry is findable after recovery")))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(def-vertex scope-custom-cap ()
  ((extent :type geometry :index t :spatial-max-cells 64))
  :graph-db-integration-test)

(test spatial-max-cells-slot-option-and-graph-default
  "Tests +spatial-insert-max-cells+ default of 16384, the :spatial-max-cells slot option, and make-graph :spatial-max-cells keyword."
  (is (= 16384 graph-db::+spatial-insert-max-cells+))

  (with-temp-directory (dir)
    (ensure-directories-exist dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :spatial-max-cells 512
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (is (= 512 (graph-default-spatial-max-cells g)))
             (with-transaction ()
               (make-scope-site :where (make-point 37.1724d0 49.2020d0))
               (make-scope-custom-cap :extent (make-point 37.1730d0 49.2025d0)))
             (let ((site-idx (spatial-index-for g 'scope-site nil))
                   (cap-idx (spatial-index-for g 'scope-custom-cap 'extent)))

               (is (spatial-index-p site-idx))
               (is (spatial-index-p cap-idx))
               (is (= 512 (spatial-index-max-cells site-idx))
                   "un-declared slot gets graph default max-cells")
               (is (= 64 (spatial-index-max-cells cap-idx))
                   "declared slot gets :spatial-max-cells slot option")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))



;;; ---------------------------------------------------------------------------
;;; Type tags on the index entries (GH #104)
;;;
;;; A shared index -- the normal outcome of declaring a geometry slot on a
;;; mixin -- used to make a scoped query materialise EVERY candidate the index
;;; returned and only then reject it by type, so the cost tracked its population
;;; rather than the answer.  The entry now carries its node's type tag and the
;;; scan filters on it, so the tests below assert the cost model, not just the
;;; results: a scope must TOUCH only its own nodes.
;;; ---------------------------------------------------------------------------

;; One geometry slot on a shared ancestor, two sibling subclasses -- so all
;; three classes land in the (SCOPE-SHARED . GEOM) index.
(def-vertex scope-shared ()
  ((geom :type geometry :index t))
  :graph-db-integration-test)

(def-vertex scope-shared-a (scope-shared) () :graph-db-integration-test)
(def-vertex scope-shared-b (scope-shared) () :graph-db-integration-test)

(defun scope-entry-tags (idx node-id geom)
  "The stored VALUE of every entry IDX holds for NODE-ID under GEOM's cells."
  (let ((sl (graph-db::spatial-index-skip-list idx)))
    (loop for cell in (graph-db::%geometry-cells
                       geom (spatial-index-precision idx)
                       (spatial-index-max-cells idx))
          for node = (graph-db::find-in-skip-list sl (list cell node-id))
          when node collect (graph-db::%sn-value node))))

(defmacro counting-materialisations ((counter) &body body)
  "Run BODY with COUNTER counting %NODE-BY-ID calls -- i.e. how many candidates
the query actually materialised."
  (let ((orig (gensym "ORIG")))
    `(let ((,counter 0)
           (,orig (fdefinition 'graph-db::%node-by-id)))
       (unwind-protect
            (progn
              (setf (fdefinition 'graph-db::%node-by-id)
                    (lambda (id graph)
                      (incf ,counter)
                      (funcall ,orig id graph)))
              ,@body)
         (setf (fdefinition 'graph-db::%node-by-id) ,orig)))))

(test vertex-and-edge-tags-are-disjoint
  "The tag carries the KIND as well as the type-id: vertex and edge type-ids
both start at 1, and an index owner need only be a NODE-CLASS, so one index can
hold both kinds."
  (is (/= (graph-db::%spatial-type-tag 1 nil)
          (graph-db::%spatial-type-tag 1 t)))
  (is (= (graph-db::%spatial-type-tag 1 nil)
         (graph-db::%spatial-type-tag 1 nil))))

(test write-path-tags-each-entry-with-its-node-type
  "Every cell an insert writes carries the node's own type tag, and two sibling
subclasses sharing one index get DIFFERENT tags -- that difference IS the
filter."
  (with-test-graph (g)
    (let (a-id b-id (pt (make-point 37.1724d0 49.2020d0)))
      (with-transaction ()
        (setq a-id (id (make-scope-shared-a :geom pt)))
        (setq b-id (id (make-scope-shared-b :geom pt))))
      (let ((idx (spatial-index-for g 'scope-shared 'geom)))
        (is (spatial-index-p idx) "both subclasses share the ancestor's index")
        (let ((a-tags (scope-entry-tags idx a-id pt))
              (b-tags (scope-entry-tags idx b-id pt)))
          (is (not (null a-tags)) "A's entries were found")
          (is (not (null b-tags)) "B's entries were found")
          (is (every #'integerp a-tags) "every entry carries a tag")
          (is (every #'integerp b-tags))
          (is (null (intersection a-tags b-tags))
              "sibling subclasses must not share a tag"))))))

(test scoped-query-materialises-only-its-own-type
  "THE regression: with 20 B nodes and 1 A node in one shared index and one
window, a query scoped to A must materialise exactly ONE node.  Before GH #104
it materialised all 21 -- and on a real corpus, 39,409 to answer 206."
  (with-test-graph (g)
    (let ((pt (make-point 37.1724d0 49.2020d0)))
      (with-transaction ()
        (make-scope-shared-a :geom pt)
        (dotimes (i 20)
          (make-scope-shared-b :geom (make-point (+ 37.1724d0 (* i 1d-4))
                                                 49.2020d0))))
      (let ((window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)))
        ;; Sanity: the index really does hold all 21 candidates.
        (is (= 21 (length (find-nodes-within 'scope-shared window :graph g)))
            "the ancestor scope sees every node in the shared index")
        (let (result)
          (counting-materialisations (calls)
            (setq result (find-nodes-within 'scope-shared-a window :graph g))
            (is (plusp calls)
                "sanity: the counter must actually intercept %NODE-BY-ID")
            (is (= 1 calls)
                "a scoped query must materialise its OWN nodes only, not every ~
                 candidate the shared index returns"))
          (is (= 1 (length result)))
          (is (every #'scope-shared-a-p result)))))))

(test untagged-entries-are-still-found
  "An entry written before the tag existed (an index not yet rebuilt) stores
NIL, and NIL must be ADMITTED -- the TYPEP filter then decides it.  Rejecting it
would turn a stale index into silently empty results, not merely slow ones."
  (with-test-graph (g)
    (let ((pt (make-point 37.1724d0 49.2020d0))
          (window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0))
          id)
      (with-transaction ()
        (setq id (id (make-scope-shared-a :geom pt))))
      (let ((idx (spatial-index-for g 'scope-shared 'geom)))
        ;; Re-write the entries the way a pre-#104 build did: no tag.
        (spatial-index-remove idx id pt)
        (spatial-index-insert idx id pt)
        (is (every #'null (scope-entry-tags idx id pt))
            "sanity: the entries really are untagged")
        (let ((result (find-nodes-within 'scope-shared-a window :graph g)))
          (is (= 1 (length result))
              "an untagged entry must fall through to the TYPEP filter")
          (is (equalp id (id (first result)))))
        ;; ...and the type filter is still applied to it, by TYPEP.
        (is (null (find-nodes-within 'scope-shared-b window :graph g))
            "falling through must not mean skipping the filter")))))

(test v4-sidecar-forces-a-rebuild-that-tags
  "A v4 sidecar names LIVE ordered maps whose entries carry no tag.  It must be
ADOPTED (so the rebuild can free its storage) but NOT trusted, and the rebuild
that follows must leave every entry tagged."
  (with-test-graph (g)
    (let ((pt (make-point 37.1724d0 49.2020d0)) id)
      (with-transaction ()
        (setq id (id (make-scope-shared-a :geom pt))))
      (graph-db::save-spatial-index-roots g)
      (let* ((file (graph-db::spatial-indexes-root-file
                    (namestring (graph-db:location g))))
             (plist (cl-store:restore file)))
        (is (= 5 (getf plist :format)) "sanity: this build writes v5")
        (cl-store:store (list :format 4 :complete t
                              :indexes (getf plist :indexes))
                        file))
      (clrhash (spatial-indexes g))
      (is (null (graph-db::restore-spatial-index-roots g))
          "a v4 sidecar must route to the rebuild, not be trusted")
      (is (plusp (hash-table-count (spatial-indexes g)))
          "...but its roots must still be ADOPTED, or the rebuild has no ~
           storage to free and orphans it")
      (rebuild-spatial-indexes g)
      (let ((idx (spatial-index-for g 'scope-shared 'geom)))
        (is (every #'integerp (scope-entry-tags idx id pt))
            "the rebuild must write the tags the v4 index lacked")))))
