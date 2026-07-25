# Changelog

All notable changes to VivaceGraph are recorded here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html):
`MAJOR.MINOR.PATCH` — MAJOR for incompatible changes (including on-disk storage
format bumps), MINOR for backward-compatible features, PATCH for backward-compatible
fixes. The `## [Unreleased]` section accumulates changes on the `experiment` branch
between releases; cutting a release renames it to the new version and dates it.

## [Unreleased]

> The next release is **3.0.0** (MAJOR). Per this file's SemVer preamble, MAJOR is
> mandatory here on two independent grounds: the spatial-index changes below are a
> breaking public-API change *and* an on-disk format bump (the spatial sidecar goes to
> format v3). Existing on-disk graphs still open — the spatial index re-derives itself
> automatically at first open — but stale call sites and old Prolog arities do not.

### Added
- **Vector segment: a dense on-disk index for `:vector-index` slots.** A slot
  declared `:vector-index t` in `def-vertex`/`def-edge` gets a dedicated
  mmap vector segment, maintained automatically by the transaction apply path
  (create/update/delete) — no parallel write path or cache to keep in sync.
- `vector-search` (graph, class-name, slot-name, query-vector, k) — top-k
  nodes of `class-name` (and its subclasses) by cosine similarity against
  `slot-name`'s vector segment, as `(score . node-id)` conses. Returns `nil`
  when no segment exists yet (declared-but-never-written slot).
- `segment-scan` and `segment-score-subset` — lower-level segment query
  primitives (`vector-search` is built on `segment-scan`).
- `rebuild-vector-segment-batched` (graph, owner-name, slot-name &key
  batch-size progress-fn) — additive, resumable, batched (re)population of a
  `:vector-index` segment from live nodes; skips ids already present. The
  migration path for a corpus written before the slot was declared
  `:vector-index`, distinct from crash recovery's `rebuild-vector-segment`
  (full drop-and-rebuild), which `restore-vector-segments` still uses.
- `vector-segment-capacity-exhausted` — an exported condition (readers
  `vsce-owner`, `vsce-slot`, `vsce-path`, `vsce-required`, `vsce-reserved`,
  `vsce-needed-bytes`, `vsce-reason`) signalled when a vector segment must grow
  past its mmap reservation and cannot relocate to a larger one — see the
  "Changed" entries below for when that can still happen and what its report
  now advises.
- **Spatial index — a per-`(owner . slot)` index registry.** The graph's single
  spatial index becomes a registry of per-declaring-class, per-geometry-slot
  indexes, mirroring `:unique` / `:vector-index` / `:index`. `spatial-indexes`
  (graph accessor) is the registry keyed `(owner-name . slot-name)`, and
  `spatial-index-for` (graph, owner, slot) reaches one index. One spatial index
  is created per declaring class per geometry slot, lazily, on the first
  geometry-valued insert. A geometry slot on a *mixin* gives its subclasses ONE
  shared index — separated at query time by the required type filter, not by
  storage — which is narrower than "per class"; a class that overrides
  `node-geometry` is indexed under `(owner . NIL)` and is still scopeable by name.
  Motivated by the mine-action team's spatial-index change request (CR-1).
- **`:spatial-precision` slot option — per-index geohash precision.** A geometry
  slot may declare `(slot :type geometry :index t :spatial-precision N)`; that
  index is built on an `N`-level geohash grid instead of the graph default (7).
  This is the *only* precision-declaration surface (there is deliberately no
  separate per-index declaration macro). Changing a declared precision rebuilds
  that one index automatically at open (bounded to the owner's nodes), because a
  mixed-precision index would silently miss on query.
- **`audit-spatial-slots` (graph)** — a read-only, exhaustive sweep that reports
  every class carrying more than one geometry-valued indexed slot and names the
  winning slot, for wiring into a schema test suite (see the inert-slot warning
  under Fixed).
- **New spatial maintenance and query entry points.** `rebuild-spatial-indexes`
  (all indexes, the migration/repair sweep), `regenerate-spatial-index`
  (one `(owner slot)` index — the manual recovery for a degraded index),
  `regenerate-spatial-indexes` (all — the spatial half of an index-backend
  switch), and `install-spatial-indexes` (adopts a changed declared precision at
  open; creates nothing). Per-index introspection: `spatial-index-max-cells`,
  `spatial-index-precision-counts`, `spatial-index-coarsest-precision`.
- **Prolog: scoped spatial functors `find-within/3`, `find-intersects/3`,
  `find-near/5`, `find-nearest/5`.** The scope rides in second position
  (`(find-near ?node scope lat lon radius)`); it accepts a class symbol, a list
  of class symbols, or `:all`, and type-filters the yielded nodes, so the `is-a`
  goal these queries once needed is gone.

### Removed
- **BREAKING: `spatial-index` (the single whole-graph spatial-index accessor).**
  There is no longer one index for it to name; use `spatial-indexes` /
  `spatial-index-for`.
- **BREAKING: the old singular, whole-graph spatial rebuild function.** There is
  no longer one index to rebuild; replaced by `rebuild-spatial-indexes` (rebuild
  every index) and `regenerate-spatial-index` (rebuild one `(owner slot)` index).
- **BREAKING: the previous unscoped Prolog spatial arities** — `find-within` and
  `find-intersects` at arity 2, `find-near` and `find-nearest` at arity 4.
  Replaced by the scoped `/3` and `/5` forms above. The old arities are removed
  rather than left to signal, so a stale query fails at goal entry with an
  unknown-functor error that names the problem, instead of binding a scope-shaped
  argument as an area.

### Fixed
- **A vector segment could not grow past its mmap reservation without
  corrupting the transaction.** The growth attempt failed inside the apply path,
  after the transaction was already durable, so the segment and the nodes
  disagreed. The capacity a transaction needs is now validated *before*
  durability, under the segment's read lock, and an over-large transaction is
  rejected with `vector-segment-capacity-exhausted` and rolled back cleanly —
  nothing is journaled and the segment is untouched.
- **Automatic crash recovery of a vector segment could not complete above
  131,072 entries.** `rebuild-vector-segment` created the fresh segment at the
  1024-entry default, and a segment's address-space reservation is derived from
  its file size *at create time*, so a ~4 MB fresh file reserved only the 1 GiB
  floor and in-place doubling ran out of reservation at 131,072 entries — while
  `restore-vector-segments` calls exactly this rebuild whenever the segment's
  clean-shutdown flag is unset. A rebuild is now created at the corpus size, so
  its reservation is derived from a realistic file (and ~8 doubling-and-relocate
  passes disappear from every rebuild).
- **Spatial insert could blow up on a country-scale geometry, and coarsening it
  naively would silently lose nodes (CR-2).** An insert now caps its geohash cover
  at 16384 cells — a per-index, *persisted* bound, so insert and remove always
  compute the identical cell set and no entry is ever orphaned. Coarsening the
  stored cover is unsafe on its own, because geohash prefixes nest one way: a query
  covering a small box at a fine precision would sort *past* a coarsely-stored
  polygon and miss it. The query therefore clamps its covering precision to the
  coarsest precision actually stored, tracked by a per-index histogram
  (`spatial-index-precision-counts` / `spatial-index-coarsest-precision`). The
  clamp is **self-healing**: delete the oversized node and its cells decrement,
  the coarse level empties, and selectivity returns on its own with no rebuild. A
  `warn` fires on each *decrease* of an index's coarsest precision (rare, loud,
  and names the node, class, slot, bbox, and the recovery path); a `log:info`
  marks the recovery. The histogram is rewritten synchronously only when the
  coarsest precision decreases (the unsafe direction); an emptied level rides the
  ordinary close-time write, because reopening too-coarse merely over-covers.
  A multipolygon splits the cap across its parts in proportion to each part's
  bounding-box *area* (with a floor of one cell per part), so a small part keeps
  full precision and only a genuinely large one is coarsened — an equal 1/N split
  coarsened small parts needlessly and, past 16384 parts, collapsed the whole
  index's query clamp to precision 1.
- **A class with two geometry-valued indexed slots silently indexed only one
  (CR-3.1).** A node reaches spatial maintenance, is indexed by its first
  geometry-valued indexed slot, and every other geometry slot was inert with no
  signal. A value-based warning now fires on the write path — sampled over a
  class's first 64 nodes, so it costs nothing steady-state — naming the class,
  every geometry slot found, and which one wins. `audit-spatial-slots` (above) is
  the exhaustive read-only counterpart for classes whose two-geometry nodes lie
  beyond the sampling window. (The declared-`:type geometry` form the request
  first asked for is not buildable: the engine cannot compare the type symbol
  reliably across application packages, so the check is value-based.)

### Changed
- **A vector segment now grows its reservation *in place* before falling back to
  relocating.** On exhaustion `%seg-ensure-reservation` first tries to claim the
  address range immediately after the current window
  (`extend-reservation-in-place`, `mmap.lisp`): one `mmap`, `m-pointer` never
  moves, nothing is copied or re-mapped, and no reader can observe anything at
  all.
  **How often it actually fires: less than the design assumed.** Measured with a
  production-sized (16 GiB) reservation on Linux 5.15 and 4.15, the claim failed
  at every size tried, because the default top-down `mmap` allocator places a
  `mmap(NULL, …)` window flush against the bottom of the existing mappings —
  `libssl.so.3` sat at the window's exact end on both hosts — so the range
  immediately *above* a newly created window is occupied by construction. The
  legacy bottom-up layout behaved identically, and Darwin likewise. It succeeds
  only for a window that happens to sit below a hole. Relocation (the entry
  below) therefore remains the workhorse; this is an opportunistic saving that
  costs one `mmap` on an already-rare path when it misses.
  The claim passes `MAP_FIXED_NOREPLACE` where the constant exists (Linux
  4.17+), which makes the kernel reject cleanly instead of placing the mapping
  somewhere useless. **The safety property is not that flag**, which older Linux
  and Darwin simply ignore, leaving the address an advisory hint: it is the
  unconditional post-hoc check that the address returned is exactly the address
  requested, with a `munmap` and a fallback when it is not. Plain `MAP_FIXED` is
  never passed and must never be added — it would evict whatever occupies the
  range. (Measured, since an earlier revision of the design asserted the
  opposite: Linux 5.15 honours the flag and returns `EEXIST`; Linux 4.15 ignores
  it and places the mapping elsewhere; in neither case is the occupant touched.)
  `*segment-extend-adjacent-on-exhaustion*` (exported, default `t`) switches it
  off, which is also what keeps the relocation tests genuinely exercising
  relocation. `*segment-adjacent-extensions*` and `*segment-relocations*` count
  which path ran. **Binding this knob to NIL by itself does not stop a segment
  from growing past its reservation** — it only removes the adjacent shortcut,
  leaving `*segment-relocate-on-exhaustion*` (below) to grow it by relocating
  instead. Getting the hard, pre-durability abort back requires BOTH knobs NIL;
  see the correction to that entry's description below.
- **A vector segment's mmap reservation is no longer a growth ceiling.** When a
  doubling would pass the reservation, the segment now reserves a larger
  address window, re-maps its file into it, and releases the old window —
  completing the "re-reserve + relocate under the write lock" step
  `docs/mmap-remap-race-plan.md` Phase 3 planned but never implemented. This
  moves `m-pointer`, which the lock-free read path otherwise depends on never
  moving, and is therefore **segment-only**: every public segment entry point
  takes the segment's own rw-lock, so `%seg-grow` has real exclusion over its
  readers. The heap (`allocator.lisp`) and linear hash (`linear-hash.lisp`) have
  no such lock — for them the reservation remains a hard ceiling, and the
  primitive is named `relocate-vector-segment-mapping` so calling it from either
  reads as wrong at the call site.
  `*segment-relocate-on-exhaustion*` (exported, default `t`) switches this
  behaviour off — but, since the adjacent re-reservation entry above shares the
  same exhaustion path and runs FIRST, this knob alone no longer restores the
  previous strictly-safe pre-durability abort: left at its default,
  `*segment-extend-adjacent-on-exhaustion*` can still grow the segment in place
  without ever reaching this one. **Both knobs must be bound to NIL** to get
  that abort back.
  `vector-segment-capacity-exhausted` now fires only when relocation is
  disabled or fails (address space exhausted), and carries two new slots —
  `vsce-path` (for the direct `segment-put` path, which has no owner/slot) and
  `vsce-reason` — with a report that says which of the two happened. Both
  `munmap`s on the relocation path (the rollback and the old-window release)
  check their return code and warn: a refused `munmap` leaks an entire
  reservation, in a long-lived process, on the one path whose failure mode *is*
  address-space pressure.
- **The pre-durability capacity check now *grows* the segment instead of
  rejecting the transaction** (`validate-vector-segment-capacity` →
  `ensure-vector-segment-capacity`). Once exhaustion became recoverable, a check
  that refused any transaction needing more than the current reservation was
  over-eager. Growing in the same manager-locked region, before
  `finalize-tx-persistence`, keeps wave 1's guarantee rather than weakening it
  to a heuristic: since commits are serialised under the manager lock, no other
  *commit* can consume the capacity in between, so `apply-transaction` cannot
  need to grow **absent a concurrent lock-free mutator**. One exists:
  `rebuild-vector-segment-batched` deliberately runs *without* the manager lock
  and raises `live-count` via `segment-put`. If it interleaves, apply's grow
  branch is reachable after all — but it then *relocates* and succeeds, so the
  wave-1 failure mode (a persisted node with no segment entry) returns only if
  relocation is switched off or genuinely fails at that moment. This is not a
  regression: wave 1's validate-only version had the identical hole. The
  reservation for the full target capacity is now pre-flighted **once**, before
  any doubling runs, so an unrecoverable transaction aborts having changed
  nothing at all and the diagnostic still names the owner and slot.
  Two accepted consequences, documented at the
  function: a transaction that fails later leaves an over-sized segment (harmless
  — capacity is not semantic, `live-count` and the id array are untouched), and a
  crash mid-grow leaves the segment dirty so `restore-vector-segments` rebuilds
  it (the existing path, made survivable above 131k entries by wave 1).
- **Vector segments now get their own address-space reservation floor,
  `*segment-min-reservation*` (16 GiB), instead of inheriting the general
  `*mmap-min-reservation*` (1 GiB).** The general rule — 8× the file's size at
  open, floored — was written for heap and index files, whose size is set by the
  schema and the workload, and of which a graph has 15–20. A vector segment is
  the first mapped file whose size tracks the *corpus*, so it reached that
  ceiling far sooner (roughly once per 8× of growth), and hitting it aborts a
  transaction. Both call sites now pass an explicit reservation —
  `create-vector-segment` and `open-vector-segment`, neither of which passed
  one before (both simply took `mmap-file`'s general default) — computed as
  `max(*segment-min-reservation*, *mmap-reservation-multiplier* × size)`, so a
  segment already larger than `floor ÷ multiplier` still gets proportional
  headroom rather than being capped at the floor. A reservation is `PROT_NONE`
  `MAP_NORESERVE` anonymous address space: no RAM, no disk, no Linux commit
  charge, so on 64-bit the larger floor costs nothing real — except `RLIMIT_AS`
  / `ulimit -v`, which counts reserved address space regardless of
  `MAP_NORESERVE` and can make a graph fail to open under a capped process
  (e.g. a systemd unit's `LimitAS=`). At dimension 1024, capacity only ever
  advances by doubling, so the largest power-of-two capacity whose file still
  fits under the 16 GiB floor is 2,097,152 slots, not the byte-exact
  4,177,983 (the next doubling, 4,194,304, needs 17,246,978,112 bytes, over
  the 17,179,869,184-byte floor). (When this landed, reaching the floor still
  meant `vector-segment-capacity-exhausted`; the *relocation* entry above then
  removed the ceiling outright, so today the floor is only the point at which a
  segment starts relocating.)
  `*segment-min-reservation*` is exported — it is the one knob that actually
  raises this ceiling, unlike `*mmap-min-reservation*`, which segment files
  no longer consult.
- **A missing vector-segment file is now rebuilt at open, not ignored.**
  `restore-vector-segments` used to skip a segment whose file was absent, so a
  graph whose segment file had been lost (or deleted by an operator expecting a
  rebuild) opened clean with a permanently empty vector index, no warning and no
  error, and `vector-search` returned nothing for a corpus that was entirely
  intact in the vertices. The vertices are authoritative, so the segment is
  rebuilt from them and the recovery is reported with a warning. A graph that
  has simply never stored a vector is *not* swept: an owner class with no nodes
  in the type index is skipped outright, so a declared-but-never-written
  `:vector-index` slot costs nothing at open.
- **An unclean (dirty-flag) segment rebuild at open now warns before it
  starts**, so a multi-minute rebuild of a large segment is not silent.
- **Snapshot/replay lost specialized vectors (issue #56).** `snapshot` → `replay`
  could not round-trip a graph whose nodes had a vector-valued slot that was not
  a byte vector: the restore readtable overrode `#(` to coerce *every* vector to
  `(unsigned-byte 8)` — necessary because node ids are byte vectors, fatal for
  anything else — so a `single-float` slot (e.g. a `:vector-index` embedding)
  aborted the restore with `The value 1.0 is not of type (UNSIGNED-BYTE 8)`.
  The snapshot text format now records a vector's element type explicitly:
  `backup` writes any vector whose element type is not `T` as
  `#V(<element-type> e1 e2 ...)` (e.g. `#V(SINGLE-FLOAT 1.0 1.25)`,
  `#V((UNSIGNED-BYTE 8) 37 22)`), and `#(...)` now reads back as a plain
  `simple-vector`. Strings are unaffected. `*print-readably*` was deliberately
  *not* used: SBCL's `#A((3) SINGLE-FLOAT ...)` is an SBCL extension, and a
  snapshot must restore on SBCL, CCL, ECL and LispWorks alike.

### Changed
- **The snapshot text format changed, one-way compatibly.** Snapshots written by
  this version are **not** readable by older graph-db versions (they contain
  `#V` literals older readers do not know). Snapshots written by older versions
  **are** readable by this one: ids, and an edge's `from`/`to`, are coerced back
  to id byte vectors at the consumption site in `recreate-graph`. One thing old
  files cannot give back is the *element type* of a node's own vector-valued
  slots — that was never written down — so such a slot restores from an old
  snapshot as a plain `simple-vector`; re-snapshot afterwards to record the
  types. Nothing about the on-disk graph format changed.
- **BREAKING: every spatial query now takes a required scope as its first
  positional argument.** `find-nodes-within`, `find-nodes-intersecting`,
  `find-nodes-near`, and `find-nearest-k` gain a mandatory first argument — a
  node-class name, a list of class names, or `:all` — that both *selects* which
  per-`(owner . slot)` indexes are scanned and *type-filters* the results. The
  filter is what makes a scoped query correct when sibling subclasses share a
  mixin-owned index. A required positional argument makes every stale call site a
  compile-time warning on SBCL and ECL, which is the safest way to land a
  deliberate break. Requested by the mine-action team (CR-1): they needed to
  query one class's geometry without dredging up another's.
  *Known limitation:* a scope resolves the named class's own geometry slots, so
  scoping to a parent does not reach an index a *subclass* declares on an extra
  geometry slot of its own (a node stored there is still a `parent` by type, but
  the parent scope will not scan it). Scope to the subclass or use `:all`; the
  general fix rides with GitHub #60.
- **BREAKING: the spatial sidecar is now `spatial-indexes.dat`, format v3**
  (was `spatial-index.root`, a single plist). It records one entry per
  `(owner . slot)` index — address, precision, backend, insert cap, and precision
  histogram — and is written on every index creation and at `close-graph`. A
  pre-v3 graph (the old file present, or `:format` ≠ 3) **re-derives its spatial
  indexes automatically at first open**: one `map-vertices` + `map-edges` sweep
  routes each node into the `(owner . slot)` index its geometry slot selects, so
  the contents come out identical to what the single index held, merely
  partitioned. **Index only — node data is untouched and nothing is re-fetched.**
  The old `spatial-index.root` is left in place, but **downgrade after migration
  is unsupported**: an older build would reopen it as a silently stale (or empty)
  index.
- **The memory-graph image bumped, and lost a per-open cost.** Both in-memory
  formats moved — the cl-store image (v4) and the native/lazy image the ECL
  device uses (v6) — so the spatial payload is now one structural record per
  `(owner . slot)`, carrying that index's precision, insert cap, and histogram,
  restored directly into its own `mem-skip-list` the way views are. This
  **removes** the rebuild-from-nodes that a memory-graph previously ran on every
  open: that pass filtered over *all* `:index` slots and so faulted in every lazy
  node blob of any class with any indexed slot — most of the corpus on a field
  device (issue #50 `:lazy` mode). Measured 0 of 11 nodes materialized on reopen,
  against 11 of 11 before. Known limitation: a changed `:spatial-precision`
  declaration is **not** adopted on a memory reopen (doing so would re-materialize
  exactly those lazy nodes); a memory index reopens at its persisted precision
  until a forced rebuild — correct, only over-covering, never missing.

## [2.1.1] - 2026-07-06

A bug-fix release.

### Fixed
- **ECL: cross-graph `edge-exists-p` / adjacency read the wrong heap.** A ve/vev
  index lookup for a graph other than the current `*graph*` deserialized its
  index-list against `(heap *graph*)` — the `deserialize-index-list` default — instead
  of the owning graph's heap. So a cold `(edge-exists-p … :graph B)` (or a generated
  `make-<type>`'s type-id resolution) while `*graph*` named a *different* graph walked
  the wrong heap and returned NIL, and the per-index cache was then poisoned with the
  mis-bound list. Fixed by binding `*graph*` to the owning graph at the
  `lookup-vev-index-list` / `lookup-ve-in-index-list` / `lookup-ve-out-index-list`
  read boundaries. It manifested on **ECL** (SBCL's cache/timing masked it in the
  regression test), but the underlying flaw was implementation-independent. Full test
  suite green on both SBCL and ECL.

## [2.1.0] - 2026-07-05

A large, backward-compatible feature release: a pluggable ordered-index backend
(an mmap B+ tree alongside the skip list), `:unique` slot constraints, offline-first
peer replication, an in-memory backend, idempotent views, a modernized Prolog engine
with a safe web query surface, and cross-cutting correctness fixes. Existing on-disk
v2 graphs open without migration.

### Added
- **Pluggable ordered-index backend — an mmap B+ tree (opt-in).** All heap-backed
  ordered indexes (map/reduce views, `:unique` constraints, the spatial index) are
  now built on a shared ordered-map protocol with two interchangeable backends: the
  skip list (default) and a new page-oriented (4 KB slotted-page) **B+ tree**
  (`bplus-tree.lisp`). The backend is a per-graph choice — `:index-backend
  :bplus-tree` on `make-graph`/`open-graph`, or the global `graph-db:*index-backend*`
  — and each index persists the backend it was written with, so a graph reopens
  every index on its own engine. On disk the B+ tree beats the skip list on every
  operation once warm (page-packed keys → far fewer cache-line and page misses,
  sequential in-leaf range scans, less space), with in-place cell edits and
  merge-on-delete rebalancing. Existing graphs migrate in place via
  `regenerate-all-views` / `regenerate-unique-indexes` / `regenerate-spatial-indexes`
  (or snapshot + replay). (Manual Chapter 3.)
- **`:unique` slot constraints (issue #6).** `def-vertex` / `def-edge` slots may
  carry `:unique t | equal | equalp | <canonicalizer>` (the value is the uniqueness
  key — identity, case/edge folding, or an arbitrary canonical form). Enforced at
  the commit boundary: a violation aborts the whole transaction with
  `unique-constraint-violation`; NULL-exempt (SQL-style); shared across subclasses
  of the declaring type; commits racing for the same value are serialized so exactly
  one wins. Backed by a persistent, per-graph unique index (skip-list or B+ tree;
  in-RAM map on a memory-graph) reopened with the graph — not rebuilt by scanning on
  open. (Manual Chapter 8. Distributed cross-device arbitration is tracked in #51.)
- **Peer replication — offline-first, hub-and-spoke sync (Chapter 16).** A
  bidirectional *peer* mode for mobile/edge fleets, alongside the existing
  master/slave replication: each device is synced only the authorized subset of the
  graph it may see (`:export-predicate`), authors locally while disconnected, and
  reconciles on reconnect. Closed-subgraph export + manifest reconciliation, node
  re-homing, per-node origin identity, Lamport clocks, and a pluggable
  conflict-resolution policy (`:origin` partitioning by default). Runs on both the
  on-disk and in-memory backends (verified SBCL hub ↔ ECL device).
- **In-memory backend — `make-memory-graph` (issue #50, Chapter 15).** An in-RAM
  storage backend that holds the whole graph as live Lisp objects, eliminating
  per-read deserialization and pcons-chain walking — lowest-latency reads when the
  graph fits in memory (aimed at mobile/ECL). Durable via the same journal plus a
  checkpoint image; eager or fault-on-access (lazy) open. The graph model,
  `with-transaction`, OCC validation, views, spatial, `:unique`, peer replication,
  and the Prolog engine all work against it unchanged.
- **Idempotent `def-view` (issue #49).** `def-view` is now declarative and
  idempotent: redefining a view with an unchanged definition is O(1) at open (no
  rescan), and a changed definition is diffed and rebuilt automatically via a
  two-phase registry. `open-graph` / `open-memory-graph` install views on open and
  accept `:regenerate-views t`; `regenerate-all-views` forces a full rebuild.
- **Streaming results: `select` `:callback` + NDJSON web responses (issue #44).**
  `select` accepts `:callback FN`, which hands each result row to `FN` as it is
  produced -- consing nothing onto a result list -- and returns the row count.
  An embedded consumer can stream an unbounded result set with constant memory.
  The web layer uses it: a query with `format=ndjson` (a parameter for
  `def-query`, a body field for the pattern query) streams each row as its own
  JSON object on its own line (`application/x-ndjson`) instead of buffering a
  JSON array.
- **ISO exceptions: `catch/3` + `throw/1` (issue #45).** `throw(Ball)` raises a
  ball and `catch(Goal, Catcher, Recovery)` recovers from one that unifies with
  `Catcher`, propagating others to an outer catch.  Only `Goal` is protected --
  a throw in the continuation after `catch/3` succeeds is not caught (the
  continuation-swallowing trap, handled with a per-frame marker).  Built-in
  errors now carry an ISO-style ball so they are catchable: an unknown predicate
  is an `existence_error`, an uninstantiated meta-call an `instantiation_error`,
  a non-callable goal a `type_error`.  The error vocabulary is keywords
  (`(:error (:existence-error :procedure foo/2) Ctx)`) so a ball unifies
  regardless of the query's package.  Resource (budget/timeout) and permission
  (effect-policy) errors are deliberately **not** catchable, so a bounded,
  untrusted query cannot `catch(Goal, _, true)` to swallow its own enforcement.
- **Prolog control-flow core (issue #45, Phase 0).** `not`/`\+`, `if` (the
  two- and three-argument `Cond -> Then [; Else]` soft cut), `once`, and `forall`
  are now first-class compiler constructs: they expand through `compile-body`, so
  they thread bindings and compose with conjunction and cut instead of routing
  through the runtime `call/1` functors.  Each opaque construct (`not`, `once`,
  the condition of `if`) is a proper cut barrier, while a cut in a `Then`/`Else`
  branch or in the tail after the construct still cuts the enclosing clause.
  A non-static (meta-call variable) sub-goal, e.g. `(not ?G)`, transparently
  falls back to the runtime functor, so existing behavior is preserved.
- **Compiled `call/N` and a runtime meta-call solver (issue #45, Phase 0.2).**
  `(call Goal Extra...)` now appends the extra arguments to `Goal` (call/N).
  When `Goal` is a static template the call compiles inline through
  `compile-body`, so it composes with cut and the control constructs (e.g.
  `(call (or ...))`, `(call (g-knows ?a) ?b)`).  When `Goal` is a variable, the
  new `%solve` runtime solver proves it -- handling conjunction, disjunction,
  call/N and atomic/compound goals.  `call/1` and the control runtime functors
  (`not`/`if`/`once`/`forall`) route through `%solve`.
- **All-solutions aggregation (issue #45, Phase 0.3).** New `findall/3` (collects
  every template instance in order, always succeeds, `[]` on no solutions).
  `bagof/3` and `setof/3` now **group by the goal's free (witness) variables** --
  the variables in the goal but not in the template and not existentially
  quantified -- yielding one solution per witness binding (and still failing when
  the goal has no solutions).  The `^` operator (`(^ Var Goal)`, nestable, accepts
  a single var or a list) marks variables as existential so they are not treated
  as witnesses.  `setof` sorts each group by the standard order of terms and
  removes duplicates.
- **Query resource bounds (issue #45, Phase 0.4).** Queries can now be bounded by
  a maximum inference count (`:max-inferences` select option / `*inference-budget*`
  / `*default-inference-budget*`) and a wall-clock timeout (`:timeout` seconds /
  `*default-query-timeout*`).  Exceeding either aborts the query with a catchable
  `prolog-resource-error`, so a runaway, non-terminating, or cyclic-recursive
  query fails cleanly instead of hanging or overflowing the Lisp control stack.
  Both default to nil (unlimited): trusted queries are unchanged, untrusted ones
  (e.g. the planned #44 web surface) opt in.  Solution count remains bounded by
  the existing `:limit`.
- **Effect partitioning / query effect policy (issue #45, Phase 1).** The
  side-effecting Prolog functors are now tagged by effect -- `:write` (graph
  mutation: `retract`), `:eval` (arbitrary Lisp: `lisp`/`lispp`/`is`/`trigger`),
  `:io` (`read`/`write`/`nl`) -- and check the per-query policy before acting.
  The `:effects` select option (or `*allowed-effects*` / `*default-allowed-effects*`)
  is `t` for all (the default) or a list of permitted tags; a disallowed effect
  aborts with a catchable `prolog-permission-error`.  Reads and pure logic are
  always allowed, so `:effects nil` is a safe read-only query mode (the basis for
  exposing queries to untrusted callers).  The check is transitive -- an effect
  reached through a user rule or meta-call is gated the same way.
- **Snapshot query mode (issue #45, Phase 1).** `select` accepts `:snapshot t`,
  which runs the query under a single consistent MVCC read snapshot: every read
  resolves at one epoch, so the result is stable against concurrent writers (a
  vertex committed after the query started is invisible to it).  Implemented as
  a lightweight read transaction (`with-read-snapshot` / `call-with-read-snapshot`)
  that registers active for the query's extent -- holding the reaper's retention
  floor -- and is discarded without commit or validation.  It inherits an
  enclosing transaction if one is already active.  Together with the resource
  bounds, the effect policy, and `:limit`/`:skip`, this gives a query surface
  safe to expose to untrusted callers.

### Changed
- **Unknown Prolog predicates are now noisy.** A goal naming an undefined
  predicate signals a `prolog-error` -- on both the compiled query path and the
  dynamic meta-call path -- instead of silently yielding no answers (the
  compiled path) or aborting with an opaque message (the old `call/1`).  This
  surfaces mistyped predicate names instead of letting them masquerade as empty
  results.  (A future `catch/3` + ISO `existence_error` will make this
  recoverable; see #45.)

- **`select-count` / `select` `:count`.** `select-count` (already exported but
  never implemented) now returns the integer number of solutions to a query
  without projecting or consing any per-solution bindings; the underlying
  `select` `:count t` option does the same and composes with `:limit` and
  `:skip` (so a capped or offset count counts the rows `select` would return).

- **`def-query` -- named parameterized queries over the web (issue #44).** A new
  `def-query` registers a server-authored, read-only graph query as a REST
  endpoint at `POST /graph/:graph/query/<name>`.  The author declares typed,
  named parameters (`:string`/`:integer`/`:number`/`:boolean`/`:keyword`),
  result variables, and the query goals; the client supplies only the
  parameters.  Each query runs through `select` with safe defaults the author
  may override -- read-only (`:effects nil`), and a result limit, inference
  budget, and wall-clock timeout.  A read-only query runs under a lightweight
  MVCC read snapshot; a query whose `:effects` permit side effects instead runs
  inside a `with-transaction`, so its writes flatten into one transaction that
  provides the same snapshot and commits on success (or rolls back if a bound or
  permission error aborts it).  Responses are a JSON array of objects keyed by the camelCase result
  names; a missing/malformed parameter is a 400, a resource-bound breach a 400,
  a forbidden effect a 403, and an unknown query a 404.  Parameter values are
  injected through a new pure `param/2` functor (and `*query-params*`), so
  injection works under the read-only policy.
- **Constrained JSON pattern queries (issue #44, tier 2).** Clients may POST an
  ad-hoc, read-only query as a JSON object to `POST /graph/:graph/query` -- no
  server-authored template and no client Lisp.  The body is a
  `{match, where, select, limit, skip}` document of typed pattern objects
  (`{vertex,type}`, `{edge,from,to}`, `{slot,name,bind|value}`,
  `{compare,args}`) compiled to a bounded `select`.  Type/edge names are
  resolved against the live schema (an unknown one is a 400), which also
  determines the package the query compiles in; only a fixed set of safe pattern
  kinds is expressible (no arbitrary predicate naming).  The query runs read-only
  (`:effects nil`), under one MVCC snapshot, with the client `:limit` capped at
  `*query-default-limit*` and the inference/time budgets applied; a malformed
  query or a bound breach is a 400.  Results use the same JSON array-of-objects
  shape as `def-query`.

### Fixed
- **Wrong-graph (`*graph*`) leaks across the core node/index/query layer.** A class
  of latent bug where code holding a specific graph resolved node ids or schema
  type-ids through the dynamic `*graph*` instead — so it operated on the *wrong*
  graph whenever `*graph*` differed (a reopened graph, a second open graph, a
  snapshot/replay target, or a slave/peer graph). Fixed in `map-view` /
  `invoke-graph-view`, `traverse`, `edge-exists-p`, the generated `make-<type>`
  type-id resolution, the `ve-index` index-list heap (a foreign-heap allocation that
  could corrupt adjacency), and `apply-transaction` (now binds `*graph*` to the
  target as a structural guard). Makes the snapshot/replay-into-a-fresh-graph idiom
  correct for graphs with views. A cross-graph regression test guards it.
- **REST procedure/query POST routes never worked over HTTP.** Their ningle
  handlers were quoted lambdas (`'(lambda (params) ...)`) -- a *list*, not a
  function -- so the server returned the list verbatim and the response was
  malformed, and the handler also referenced the route capture
  (`procedure-name`) as an unbound free variable instead of reading it from the
  params.  Replaced with real closures that pull the capture via
  `(get-param params :procedure-name)` / `:query-name`.  Surfaced by the new
  end-to-end HTTP tests (the existing tests exercised handlers in-process).
- **REST procedures were broken on ECL.** `*rest-procedures*` had `#+sbcl`/
  `#+ccl`/`#+lispworks` initforms but no `#+ecl` branch, so on ECL the variable
  was declared special but left unbound; `def-rest-procedure` and
  `call-rest-procedure` then failed with an `unbound-variable` error.  Added the
  `#+ecl` branch so REST procedures work on ECL like the other implementations.
- **`node-slot-value/3` swallowed downstream query errors.** Its `handler-case`
  wrapped `(funcall cont)` -- the continuation -- so an error raised by any goal
  *after* a `node-slot-value` (e.g. a `prolog-permission-error` from a denied
  write, or a `prolog-resource-error`) was caught and silently turned into a
  non-match.  The guard now wraps only the slot read; the continuation runs
  outside it so downstream errors propagate.
- **Prolog `if/3` else-semantics (issue #45).** `(if Test Then Else)` now runs
  `Else` only when `Test` has no solution; previously it also ran `Else` when
  `Test` succeeded but `Then` failed.  The runtime `if/3` functor (the meta-call
  path) was corrected to match.
- **Prolog `or` binding propagation.** A variable first bound inside a disjunct
  (e.g. `(or (= ?x 1) (= ?x 2))`) was lost across the disjunction's shared
  continuation because `=` had been optimized to a compile-time alias.  The `or`
  compiler macro now seeds its fresh variables so they bind on the trail at
  runtime and are visible to the continuation.
- **ECL spatial-index concurrency (issue #42).** The skip list guarded every
  operation -- reads included -- with one recursive lock on ECL, so concurrent
  spatial queries ran sequentially and timed out under high parallelism.  Replaced
  it with a per-skip-list reader/writer lock: shared read lock for readers (find,
  cursor scans, map, count), exclusive write lock for mutators.  Writers never run
  concurrently with readers (torn-read safety preserved); concurrent readers now
  run in parallel.  No-op on non-ECL (those keep the lock-free design).

## [2.0.0] - 2026-06-11

A major release: MVCC versioned nodes, a geohash spatial extension, a full
cross-implementation port (SBCL/CCL/ECL) with a comprehensive automated test
suite, and an ACID-compliance audit.

### Added
- **MVCC — immutable, versioned nodes (issue #19).** Each update now retains the
  prior version of a node in a heap-backed version chain instead of freeing it in
  place. Old versions are reclaimed by a lazy, epoch-gated reaper once no active
  reader or transaction can still observe them. Configurable retention via
  `:keep-revisions` (per node type, with a graph-level default), and
  snapshot-isolation reads for transactional lookups. As a bonus this dissolves
  the long-standing node-data read-after-free race at its source.
- **Spatial extension.** A geohash-backed, heap-resident spatial index answers
  proximity and area queries over nodes that carry a `geometry`. Declarative
  opt-in via a `:index t` geometry slot (`node-geometry` auto-wiring), bounding-box
  and radius queries, k-nearest-neighbour search, and geohash neighbour
  enumeration. Optional `graph-db/geos` integration adds exact topology, validity
  repair, and accurate distance via libgeos. (See Chapter 13 of the manual.)
- **ECL support.** The full `:graph-db` system — including the REST layer — builds
  and runs on ECL 26.5.5; the entire test suite is green on ECL (macOS arm64 and
  Linux x86_64), matching SBCL.
- **Automated test suite (FiveAM).** New `graph-db/test`, `graph-db/concurrency-test`,
  `graph-db/acid-test`, `graph-db/stress-test`, `graph-db/concurrent-stress-test`,
  and a `graph-db/perf-test` performance-benchmark system. Replaces the previous
  ad-hoc REPL-driven tests.
- **ACID-compliance audit** with dedicated regression tests (atomicity,
  consistency, isolation, durability) and broad concurrency coverage across
  SBCL/CCL/ECL.
- `migrate-graph` for upgrading a pre-MVCC (v1) on-disk graph to the v2 format via
  a logical snapshot + replay.
- This `CHANGELOG.md`.

### Changed
- **On-disk storage format bumped to v2.** The node head grows from 15 to 31 bytes
  (append-only: `commit-epoch` + `prev-pointer`). v1 graphs cannot be opened
  directly by v2 code — use `migrate-graph` (logical snapshot + replay). New
  graphs are stamped v2 automatically.
- Stable-address memory mapping: `extend-mapped-file` remaps in place
  (`MAP_FIXED`) so the base pointer never moves, enabling lock-free reads across
  SBCL/CCL/ECL.
- Project version 1.0 → 2.0.

### Fixed
- **Persistent-slot `slot-boundp` / `slot-makunbound` (issue #41).** These MOP
  generic functions were never specialized for the node metaclass, so they
  inspected the always-unbound backing CLOS slot; `slot-boundp` on a persistent
  slot was always NIL and `slot-makunbound` was a no-op on the value. Both now
  consult the data alist.
- **ECL concurrency regression from the #41 fix.** ECL's `change-class` invokes
  `slot-makunbound-using-class` on alist-backed persistent slots during node
  construction, which cleared a freshly-created node's data and triggered racy
  lazy re-materialization of the shared cached node (transient NIL slot reads). A
  dynamic guard (`*initializing-node*` / `change-node-class`) suppresses the
  destructive alist edit during (re)initialization; explicit `slot-makunbound` is
  unaffected.
- Numerous concurrency-correctness fixes surfaced by the new suites: rw-lock
  wakeup herd / FIFO behaviour, skip-list torn-read SIGSEGV on ECL (now
  serialized), insert lost-update ordering, schema class-lock rebuild on
  `open-graph` (issue #32), and replication socket/threading fixes.

### Known issues
- **ECL only**, under the high thread-count parallelism of many-core Linux hosts:
  `CONCURRENT-SPATIAL-INSERT-AND-QUERY` can deadlock (issue #42) and
  `FULL-SYSTEM-STORM` can flakily time out (issue #43). Both are timeouts, not data
  corruption; SBCL and CCL are unaffected, and ECL is green at lower parallelism.

### Compatibility
- ECL **26.5.5** is required (earlier releases such as 21.2.1 are no longer
  supported).
- CCL is supported on Linux x86_64 only; there is no usable CCL on Apple-Silicon
  macOS.
- LispWorks support is currently **untested** (no license access; the free
  Personal Edition's heap is too small to compile VivaceGraph).

[Unreleased]: https://github.com/kraison/vivace-graph/compare/v2.1.1...HEAD
[2.1.1]: https://github.com/kraison/vivace-graph/compare/v2.1.0...v2.1.1
[2.1.0]: https://github.com/kraison/vivace-graph/compare/v2.0...v2.1.0
[2.0.0]: https://github.com/kraison/vivace-graph/releases/tag/v2.0
