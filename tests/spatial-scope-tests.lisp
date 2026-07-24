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
      ;; B is never recreated.  *SPATIAL-REBUILD-IN-PROGRESS*, bound exactly as
      ;; REBUILD-SPATIAL-INDEXES binds it, suppresses %SPATIAL-INDEX-FOR's
      ;; ordinary per-creation save, so nothing overwrites the incomplete marker
      ;; with a premature :COMPLETE T either.
      (let ((graph-db::*spatial-rebuild-in-progress* t))
        (spatial-index-insert
         (graph-db::%spatial-index-for g 'scope-probe 'geom)
         probe-id (make-point 37.1724d0 49.2020d0)))
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
      ;; %SPATIAL-INDEX-FOR already wrote a (now :COMPLETE T) sidecar the moment
      ;; the index was created; rewrite it in the PRE-MARKER shape, with no
      ;; :COMPLETE key in the plist at all.
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
