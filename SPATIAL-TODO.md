# Spatial Extension — TODO

Status of the geohash-backed spatial extension (geometry type, index, write-path
maintenance, queries). Merged into `experiment`; full suite green on SBCL
(1371/0) and ECL (1366/0, 1 pre-existing skip). See manual Chapter 13.

## Done

- `geometry.lisp` — geometry value type (point/linestring/polygon/multipolygon),
  WGS84 `(lon lat)` doubles, serialization (type tag 102), `geometry-bbox`.
- `geohash.lisp` — encode/decode/bbox/cell-size/covering/prefix-range.
- `geometry-ops.lisp` — haversine distance, point-in-polygon (+holes),
  multipolygon containment, bbox overlap.
- `spatial-index.lisp` — heap-backed geohash index, per-index precision
  (default 7), insert/remove/query-bbox/query-radius, persisted via the
  `spatial-indexes.dat` sidecar (v3; one record per `(owner . slot)` index).
- Graph lifecycle — `make-graph`/`open-graph`/`close-graph` create/reopen the index.
- Write-path hook (`transactions.lisp`) — auto-maintains the index on
  create/update/delete via the `node-geometry` protocol.
- Pure Prolog predicates — `geo-distance/5`, `geo-near/5`, `geo-within/3`.
- Index-backed queries — `find-nodes-within` / `find-nodes-near` (Lisp);
  `find-within/3`, `find-near/5` (Prolog, yield nodes; scoped as of 3.0.0).
- Public API exported from the `graph-db` package.
- Manual Chapter 13 + README (see "Doc fixes" below for two corrections).

## Known limitations (current design choices)

- **`find-within` on extended geometries is approximate WITHOUT GEOS** — matches
  by representative point (the point itself for `:point`; bbox centre otherwise).
  With the optional `graph-db/geos` add-on it is **exact** (GEOS containment); the
  centroid path is only the dependency-free fallback.
- **polygon↔polygon ops need GEOS** — intersects + exact containment + validity
  repair (`make-valid`) live in the optional `graph-db/geos` add-on; core has only
  point-in-polygon + a coarse bbox-overlap fallback for intersects.
- **Distance is haversine** (~0.5% vs ellipsoid); `geometry-distance-exact` (GEOS)
  is PLANAR in coordinate units (degrees for lon/lat), not metres — use it for
  ordering, not real distance.
- **Grid precision** — geohash (Z-order), not Hilbert. *(As of 3.0.0 precision is
  per-index via the `:spatial-precision` slot option; `make-graph :spatial-precision`
  remains the graph-wide default, 7 ≈ 150 m cells.)*
- **No regenerate/rebuild** of the index (views have `regenerate-view`). *(Done —
  see `rebuild-spatial-indexes` / `regenerate-spatial-index`.)*

## TODO

### P1 — Functional gaps for the platform
- [x] **Subset-replication filter** — DONE (prototype): a `replication-filter`
      predicate slot on `slave-graph` (set via `make-graph :replication-filter`);
      `apply-transaction` runs `filter-writes` on a slave so it applies only its
      subset (txn id still advances). `make-spatial-replication-filter` builds an
      area-of-operations predicate (accepts non-spatial nodes + spatial nodes in
      the area). Unit-tested, and validated end-to-end in the `tests/replication/`
      harness (in-AO replicated/indexed, out-of-AO filtered). **Remaining:**
      AO-boundary-crossing *updates* (a node moving in/out of the AO) are not yet
      reconciled.
- [x] **`find-intersects` query** — DONE (`graph-db/geos`): `find-nodes-intersecting`
      (Lisp) + `find-intersects/3` (Prolog, scoped as of 3.0.0), index bbox candidates refined by the
      `geometry-intersects-p` seam (exact with GEOS, coarse bbox fallback without).
- [x] **`rebuild-spatial-indexes`** — DONE (`spatial-query.lisp`): drop + recreate the
      per-`(owner . slot)` indexes, re-index every live node with a `node-geometry`;
      returns the count. Mirrors `regenerate-view`. Per-index recovery is
      `regenerate-spatial-index` (owner, slot); the whole-graph backend-switch form is
      `regenerate-spatial-indexes`. *(Superseded the 2.0 singular whole-graph rebuild function in 3.0.0.)*
- [x] **Exact extended-geometry containment** — DONE (`graph-db/geos`):
      `find-nodes-within` routes non-point candidates through the exact
      `geometry-contains-geometry-p` (GEOS) seam, dropping the centroid
      approximation; without the add-on it falls back to the old centroid path.

### P2 — Accuracy & robustness
- [x] **GEOS integration** — DONE: evaluated `cl-geos` (unmaintained since 2018;
      no buffer/makeValid/distance; unsafe shared global context; won't load on
      macOS arm64) and instead built an OPTIONAL in-house CFFI binding
      `graph-db/geos` to libgeos_c's reentrant `_r` API. Provides exact
      `geometry-intersects-p` / `geometry-contains-geometry-p` / `geometry-make-valid`
      / `geometry-valid-p` / `geometry-distance-exact` behind a generic-function
      refine seam (dependency-free fallbacks in core; core stays libgeos-free).
      Threads share a borrow/return **context pool** (`with-geos-context`) — never
      two threads per context. Green on SBCL+ECL incl. a concurrency storm and a
      shapely oracle cross-check. *(Remaining: union/buffer + geodesic polygon
      distance not yet bound; centroid fallback still used when the add-on is absent.)*
- [x] **Geohash neighbors/adjacent** — DONE (`geohash.lisp`): `geohash-neighbor`
      (step one cell in lon/lat, wraps the antimeridian, NIL off a pole) and
      `geohash-neighbors` (the 8 surrounding same-precision cells). Covered by
      `geohash-suite`.
- [x] **kNN (`find-nearest-k`)** — DONE (`spatial-query.lisp`): the K nearest
      nodes to a point, nearest-first, via radius-doubling over `find-nodes-near`
      (correct because everything inside radius r is nearer than anything outside
      it). Bounded by `:max-radius` (default 25 km) since the fixed-precision
      geohash index enumerates cells per window — unbounded kNN is not supported.
      Prolog `find-nearest/5` (scoped as of 3.0.0). Covered by `spatial-query-suite`.
- [x] **Snapshot → restore → spatial-query test** — DONE (backup-suite): replay
      into a fresh graph re-applies nodes through the write-path hook, repopulating
      the spatial index (verified queryable; empty before replay).
- [x] **Replicated-index test** — DONE (tests/replication harness): the slave
      maintains its spatial index on replicated apply (catch-up + live).  The same
      run also covers end-to-end subset filtering (in-AO places replicated/indexed,
      out-of-AO filtered).
- [x] Point-in-polygon **boundary semantics** — DONE: documented the half-open
      PNPOLY rule (a point on an edge shared by two polygons lands in exactly one
      of them — no double-count, no gap; which side wins is not part of the
      contract) in `geometry-ops.lisp`, and pinned it with `geometry-ops-suite`
      tests (edge tiling XOR, determinism, interior/exterior unambiguous).

### P3 — Performance & scale
- [ ] **Load the real ~458k-image / 440-find dataset** onto a spatial graph;
      measure index insert + query at scale (no spatial perf coverage yet).
- [ ] **Compact geometry serialization** (flat double-array vs nested-list); tag,
      struct, and API stay stable.
- [ ] **Hilbert-curve migration** behind the same query API if geohash range
      fragmentation hurts at scale.
- [x] Concurrency/stress coverage for the index — DONE
      (`tests/concurrency/spatial-tests.lisp`, `concurrent-spatial-suite`): N-thread
      concurrent inserts, interleaved insert+query, cell-moving updates, and
      concurrent deletes, all asserting index consistency. Green on SBCL + ECL.

### P4 — Polish, docs, cross-impl
- [ ] **CCL** verification on Linux (only SBCL + ECL run so far).
- [ ] **H3 density helper + aggregation view** (contamination-heatmap building
      block); geohash was used for the index instead.
- [x] Wire the **`:index` slot flag** so a geometry slot opts into indexing
      declaratively (vs hand-written `node-geometry`) — DONE: `node-geometry`'s
      default scans the node's `:index`-marked slots and returns the first whose
      *value* is a geometry (robust to the app package of the `:type geometry`
      symbol; `(slot :type geometry :index t)` is now enough). An explicit
      `node-geometry` method still takes precedence. Covered by
      `index-slot-flag-auto-wires-node-geometry`.
- [x] **`example.lisp`** — DONE: MERCHANT gains a `(location :type geometry
      :index t)` slot (auto-indexed), and the script ends with a spatial
      walkthrough — `find-nodes-near`, `find-nearest-k`, `find-nodes-within`, and
      a Prolog `find-near` composed with `is-a`. Runs clean end-to-end.
- [ ] Cleanup — delete the merged `spatial-index` branch + `spatial-index-prerebase`
      tag.

### Done (this pass)
- [x] `make-graph :spatial-precision` and `:spatial-max-cells` keywords (persisted; read back by `open-graph`).
- [x] Per-index `:spatial-max-cells` slot option and `make-graph :spatial-max-cells` keyword added (issue #80). Global `+spatial-insert-max-cells+` default retained at 16384 for backwards compatibility.
- [x] Per-index `:spatial-max-cells` slot option added to `def-vertex` slot specs.
- [x] Issue #79 Fix #1: Direct GEOS C API geometry construction without WKT text round-trip (~27x speedup, predicate time reduced from 4.64ms to 0.17ms on 2.2k vertex geometries).
- [x] Issue #79 Fix #2: Packed coordinate array storage (`simple-array double-float (*)`). Reduces memory footprint by 3-4x and enables zero-consing bounding box and point-in-polygon operations. Full backward compatibility preserved for reading legacy nested list serializations from disk.
- [x] Issue #84: Exported `geometry-coordinate-pairs` (pair-shaped nested list accessor restoring classic `(lon lat)` structure for exporters/callers) and `map-geometry-coordinates` (vertex iterator). Corrected header documentation.
- [x] Issue #85: Added and exported `do-geometry-coordinates ((lon lat) geometry &body body)` macro form for zero-allocation unboxed double-float register iteration over geometry vertices (25x faster than `geometry-coordinate-pairs`).
- [x] Issue #83: Substrate performance optimizations for index read path:
  - Removed `:around` CLOS method dispatch from `get-byte` / `get-bytes`.
  - Added fast direct-mapped array node cache to `skip-list` (eliminating weak hash table lock contention and GC overhead).
  - Implemented lazy leaf page entry decoding for `bplus-tree` (`bplus-cursor`), eliminating wholesale decoding of leaf entries.
  - Added ASCII string decoding fast path `%octets-to-string-fast` in `serialize.lisp` and zero-allocation slice decoding in `view-key-deserialize`.
  - Benchmark results: B+ tree spatial query latency dropped from 2.608 ms to 0.744 ms (**3.5x speedup**, 58% less consing). Skip-list latency dropped from 1.329 ms to 1.116 ms.
- [x] Chapter 13 caveat fixes: corrected the "prefix range scans" description
      (queries do same-precision exact-cell lookups) and the precision/max-cells config claims.




