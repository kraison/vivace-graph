# In-memory storage backend for VivaceGraph (`memory-graph`)

**Status:** design / plan (pre-implementation)
**Audience:** graph-db engine
**Motivation:** eliminate per-read deserialization and on-disk pcons/index-list
chain-walking on mobile/ECL (and provide a general-purpose in-memory graph for
non-replicated use: analytics, ephemeral scratch graphs, fast tests).

---

## 1. Problem and framing

On mobile/ECL the on-disk engine pays two costs the workload doesn't need:

1. **Per-read deserialization.** The node object cache is weak-valued; ECL GC
   evicts it between queries, so hot reads re-deserialize node data (heap →
   plist) every time. A `site-finds` query over ~720 finds takes ~5 s.
2. **On-disk adjacency chain-walking.** Every `outgoing-edges` / adjacency step
   deserializes a linked list of 25-byte `pcons` records from the heap
   (`ve-index` / `vev-index` values are `index-list`s → pcons chains).

Neither cost is mmap *file I/O* per se — a warm mmap is RAM-speed. The costs are
**serialization on the read path** and **byte-chain navigation**. An in-memory
backend that keeps nodes as **live objects** and adjacency as **live Lisp sets**
removes both.

### Key architectural fact this rests on

Durability is already decoupled from the heap. On commit, `%commit` →
`persist-tx` writes the `.txn` journal and the replication log and **does not
touch the heap**; the heap write (`maybe-write-to-heap`) is a separate step in
`apply-tx-write`. `open-graph` already rebuilds derived state from the journal
(`recover-transactions` replays `.txn` files through the *same*
`apply-transaction` seam; `recreate-graph` rebuilds from a snapshot s-expr).

So the heap/indexes/views are a **materialized cache of the journal**. An
in-memory backend keeps the journal + `apply-transaction` + OCC + replication
unchanged, and swaps only what those write *into*.

---

## 2. Locked decisions

| # | Decision | Choice |
|---|----------|--------|
| 1 | Dispatch seam | **Override the generic node-ops** (methods dispatching on graph/table/index type). The low-level `lhash`/allocator/mmap path is left byte-for-byte untouched. |
| 2 | Adjacency representation | **In-RAM mirror of the ve/vev/type-index structure** (same keys and semantics), *not* edges embedded in vertices. |
| 3 | Durability | **Full journal** of all applied writes (reuse `.txn` + `recover-transactions`). Measure, then revisit an authored-ops-only scope. |
| 4 | Views / spatial | **Both are first-class v1 features**, maintained live on write and **rebuilt in-RAM on open** in v1. Rebuild-on-open (rather than a persistent in-RAM representation) is the only v1 shortcut, shared by both, and is the first follow-up. The **spatial index is NOT deferred** — it is vital to the mobile app long term. |
| — | Branch | Merge `peer-replication` into `experiment`, then branch the memory-graph work off `experiment`. |

### MVCC decision (from the audit)

The app never opens a read transaction and never passes `:snapshot t`; all reads
are naked (`map-vertices` / `view-lookup-node`) and hit `lookup-object`'s
null-transaction path, which returns **current committed state** and never
touches the version chain. On-device MVCC is therefore pure *write-side* overhead
today (`archive-node-version` + `reap-old-versions`) for a read benefit nothing
takes.

**memory-graph drops the version chains** but **keeps the `with-read-snapshot` /
`:snapshot` API**:

- Update = build the new immutable node object, then **atomically swap** the
  table slot. Readers are lock-free and always see a whole, consistent
  *individual* node (never torn).
- Cross-node consistency across a multi-scan report is the only gap, and the app
  already lives with that gap on the mmap backend.
- `with-read-snapshot` on a memory-graph = a brief **writer-excluding read-lock**
  (single writer, so this is cheap and rare) for the few internal consumers
  (peer-streaming, authority sync) that need a stable multi-node view.
- `resolve-version-at-epoch` on a memory-graph returns the current node
  (identity); `*snapshot-reads-p*` is effectively a no-op.

OCC is **unaffected** — `validate` compares tx-ids/`commit-epoch`/revision on the
current node, not the chain, and the transaction-manager lock still serializes
commits. We drop version *archival*, not concurrency control.

---

## 3. Class model

Storage-backend and replication-role are orthogonal axes, so backend is a
**mixin**, not a point in the role lattice:

```
graph
├── master-graph          (role)
├── slave-graph           (role)
└── peer-graph            (role, peer-replication branch)

memory-graph-mixin        (backend)   <-- new

;; concrete, instantiable:
memory-graph        (memory-graph-mixin graph)          ; standalone, no replication
memory-peer-graph   (memory-graph-mixin peer-graph)     ; in-memory device peer
```

- Storage node-ops specialize on `memory-graph-mixin` (more specific than the
  base `graph` methods, so they win).
- Replication methods keep specializing on the role classes
  (`peer-graph`, `master-graph`) — they live on different generic functions, so
  the two axes compose without method conflicts.
- Where a single GF is shared (`apply-transaction`), replication already uses a
  qualified `:after` method (`replicate-transaction`); backend logic overrides
  the primary method. No clash.

Construction: `make-memory-graph (name location &key ...)` (and the peer variant
via the existing peer constructor + `:backend :memory`). `location` is still
used — for the journal, snapshot, and schema — even though there is no heap/
index mmap tree.

---

## 4. In-RAM structures (decision #2 — mirror the index shapes)

New table/index objects, each implementing the **existing generic interface** so
callers above the seam are unchanged. Each is just a Lisp hashtable (portable
`equalp` on UUID arrays, per the ECL/algorithms convention) whose values are
adjustable id-vectors / id hash-sets instead of lhash+pcons chains.

| On-disk today | In-RAM replacement | Value shape |
|---|---|---|
| `lhash` vertex-table / edge-table | `mem-table` | `id (uuid) → live node object` |
| `ve-index` (in/out), backed by lhash→index-list→pcons | `mem-ve-index` | `(uuid,type-id) → id-set` |
| `vev-index` | `mem-vev-index` | `(from,to,type) → id-set` |
| `type-index` (65,536-slot mmap) | `mem-type-index` | `type-id → id-set` |
| `heap` / `indexes` (`memory`/allocator) | *gone* | data lives on the node |
| views (skip-list over indexes heap) | `mem-view` (§6) | `mem-ordered-map` |
| `spatial-index` (geohash skip-list over indexes heap) | `mem-spatial-index` (§6) | `mem-ordered-map`: geohash-string → id, duplicates |

Deletes the 131,072-slot eager type-index build and its ~1 MB mmap files
outright. `mem-*-index` key/id-set semantics match ve/vev exactly, so
closed-subgraph export / manifest reconciliation (which walk ve/vev) work
unchanged.

`data` vs `bytes`: on a memory node, `data` (the live plist/alist) is
authoritative and always present; `bytes` is computed lazily only when the
journal or wire needs it (`persist-tx` / serialize). `ensure-node-bytes` and
`maybe-write-to-heap` become no-ops.

---

## 5. Method-override inventory (decision #1)

All of these are already generic with a `graph`/`table`/`index` argument, which
is exactly why override-at-node-ops is the low-churn seam. `memory-graph-mixin`
(or the `mem-*` table/index types) get specializations:

**Write / apply path**
- `apply-tx-write` `(tx-create memory-graph-mixin)` / `(tx-update …)` /
  `(tx-delete …)` — set node live in `mem-table`, update `mem-*-index`, **no**
  heap write, **no** `archive-node-version`.
- `maybe-write-to-heap`, `maybe-allocate-for-node` → no-op.
- `archive-node-version`, `reap-old-versions` → no-op.
- `apply-tx-writes-to-spatial-index` `(memory-graph-mixin)` → maintain the live
  `mem-spatial-index` (insert/remove cells for changed geometry). **Not** a
  no-op — spatial is a first-class v1 feature.
- `apply-transaction` primary body reused; its sub-calls are the overridden ones
  above. `replicate-transaction` `:after` on peer/master still fires.

**Read / lookup path**
- `lookup-node` `((table mem-table) key (graph memory-graph-mixin))` →
  `gethash`. Returns a live object; no `ensure-node-bytes`.
- `ensure-node-bytes` → identity/no-op.
- `resolve-version-at-epoch` → current node (identity).
- `call-with-read-snapshot` → grab the writer-excluding read-lock.

**Index update/query**
- `add-node-to-indexes` / `remove-node-from-indexes`.
- `add-to-ve-index` / `remove-from-ve-index`,
  `add-to-vev-index` / `remove-from-vev-index`,
  `type-index-push` / `type-index-remove`.
- `lookup-ve-in-index-list` / `lookup-ve-out-index-list`,
  `lookup-vev-index-list`, `get-type-index-list` — return the id-set (or a small
  shim that presents it the way `outgoing-edges` / `incoming-edges` /
  `map-vertices` / `map-edges` expect).

**Views / spatial**
- `init-spatial-index` / `restore-spatial-index` `(memory-graph-mixin)` — create a
  `mem-spatial-index` (no heap address / `.root` sidecar); rebuilt on open in v1.
- `add-to-views` / `update-in-views` / `remove-from-views` — maintain `mem-view`s.

**Open / close / durability**
- `make-memory-graph` / `open-memory-graph` — build empty `mem-*` structures,
  restore snapshot, replay journal tail, rebuild views + spatial index (§6).
- `close-graph` `(memory-graph-mixin)` — flush/`take-snapshot`, drop `.dirty`,
  release nothing mmap.
- `persist-tx`, `recover-transactions`, `take-snapshot`, `recreate-graph`,
  `persist-highest-transaction-id` — **reused as-is** (they operate on the
  journal/snapshot, not the heap).

Everything above the seam — `with-transaction`, `%commit`, OCC `validate`, the
`peer-writer-loop` single-writer funnel, `peer-enqueue-write`, the `.txn` format,
replication decode/apply, `select`/`do-query`/`map-vertices` — is untouched.

---

## 6. Views and spatial — one shared primitive (decision #4)

Both views and the spatial index reduce to the **same missing piece**: an in-RAM
ordered map with **range cursors** matching the existing skip-list cursor surface
(`make-range-cursor` / `cursor-next` / `%sn-key` / `%sn-value`,
duplicates-allowed). Views need it for range/paging queries; the spatial index
needs it because a bbox query is a set of **ordered prefix-range scans** over
geohash keys (a hashtable cannot do this). So we build one component —
`mem-ordered-map` — and both features fall out.

- **`mem-ordered-map` (v1 core, the meatiest new piece).** A pure-Lisp ordered
  container (in-RAM skip-list of object-refs is the natural choice) exposing the
  **same cursor protocol** the on-disk skip-list does, so `map-view` /
  `invoke-graph-view` / `map-reduced-view` and `spatial-index-query-bbox` consume
  it with minimal change. Two build options: (a) a fresh pure-Lisp skip-list
  mirroring the cursor API (cleaner), or (b) parameterize `skip-list.lisp` to use
  in-RAM nodes instead of heap addresses (bigger refactor). Default to (a) for v1.

- **Views (`mem-view`).** Maintained live on write (`add-to-views` etc. → the
  view's `mem-ordered-map`) and, in v1, **rebuilt on open** by scanning all nodes
  through each `map-fn`. Trivial at target scale (767 vertices / 1527 edges) and
  it kills the ~23 s first-population on-disk skip-list rebuild.

- **Spatial index (`mem-spatial-index`) — first-class, NOT deferred.** All the
  geohash covering math is storage-agnostic and reused verbatim
  (`%geometry-cells`, `%bbox-cells`, `geohash-covering`, `geohash-prefix-range`,
  `%covering-precision`, `geometry-bbox`, `geodesic-distance`). Only the backing
  store changes: `mem-spatial-index` holds a `mem-ordered-map` (geohash-string →
  node-id, duplicates allowed) instead of the heap skip-list. The public surface
  is unchanged — `spatial-index-insert` / `-remove` / `-query-bbox` /
  `-query-radius`. Maintained live via the overridden
  `apply-tx-writes-to-spatial-index`; rebuilt on open in v1 by scanning
  geometry-bearing nodes. `spatial-index-address` and the `.root` sidecar have no
  meaning for a memory-graph.

- **Follow-up (the one v1 shortcut, shared by both):** a **persistent** in-RAM
  representation (or fast serialized snapshot) so open doesn't re-scan to rebuild
  views and the spatial grid. Rebuild-on-open is correct and fine at current
  scale; the follow-up is about open latency as data grows.

---

## 7. Durability and recovery (decision #3 — full journal)

- **Write:** unchanged `%commit` → `persist-tx` serializes each write into the
  `.txn` journal (this is the one serialization we keep; it's cheap and
  necessary). We skip the *second* serialization into the heap and skip building
  on-disk pcons/index-list chains.
- **Compaction:** existing `take-snapshot` (s-expr) + `recreate-graph`.
- **Open:** create empty `mem-*` structures → restore latest snapshot via
  `recreate-graph` → replay tail `.txn` via `recover-transactions` → rebuild
  views (§6). All existing machinery, retargeted at RAM.
- **`.dirty`:** kept for parity, though a memory-graph always rebuilds from
  durable artifacts on open regardless.
- **Later lever (not v1):** a peer device's replicated state is re-pullable from
  the hub, so we could journal only locally-authored (not-yet-pushed) writes and
  rebuild replicated state by re-pull on open — cutting write amplification.
  Deferred until v1's full-journal cost is measured.

---

## 8. Concurrency

- **Peer device:** already single-writer via `peer-writer-loop`; user writes
  funnel through `peer-enqueue-write`. memory-graph inherits this unchanged.
- **Standalone memory-graph:** the transaction-manager lock in `%commit`
  serializes commits — that *is* the single writer. No peer writer-loop required.
- **Reads:** lock-free, returning the current committed node object; update
  publishes the new node by atomic hashtable-slot swap. `with-read-snapshot`
  takes the writer-excluding read-lock for the rare consumer that needs a stable
  multi-node view.

---

## 9. Phasing

- **Phase 0 — measure (before/independent).** Instrument a `site-finds` query
  (existing `mem-probe` harness) to confirm the cost split is deserialize +
  pcons-walk (validates the whole premise). Also gives a baseline to compare the
  backend against.
- **Phase 1 — strong resident cache (optional early win, mmap kept).** Flip the
  device node cache from weak-value to a fully-pinned strong cache and keep
  `data` live. Small, reversible, likely captures most of the *read* win without
  the backend. Good de-risking step; can ship independently.
- **Phase 2 — `memory-graph` backend (this doc).**
  1. `memory-graph-mixin` class + `make-memory-graph` / `open-memory-graph`
     skeleton (empty tables, journal/snapshot wired, no indexes/views yet).
  2. `mem-table` + node-op overrides (`apply-tx-write`, `lookup-node`,
     `maybe-write-to-heap`, `ensure-node-bytes`, MVCC no-ops). Get single-node
     CRUD + journal replay green.
  3. `mem-ve-index` / `mem-vev-index` / `mem-type-index` + index overrides. Get
     `map-vertices` / `map-edges` / `outgoing-edges` / traversal green.
  4. **`mem-ordered-map`** (the shared range-cursor primitive), then `mem-view`
     and `mem-spatial-index` on top of it, both maintained live + rebuilt on open.
     Get `invoke-graph-view` / `map-view` and `spatial-index-query-bbox` /
     `-query-radius` green. Spatial is in v1, not deferred.
  5. `with-read-snapshot` lock; wire peer role (`memory-peer-graph`);
     end-to-end hub↔in-memory-device pull + authored write.
- **Phase 3 — follow-ups.** Persistent in-RAM view + spatial representation (retire
  the rebuild-on-open shortcut in §6); authored-ops-only journaling; possible
  convergence with the algorithms Mode-A projection (a native memory-graph could
  subsume it).

---

## 10. Testing

- **Backend-parity matrix (primary strategy).** Parameterize the existing
  storage/graph FiveAM bodies by a graph constructor and run each against both
  an on-disk graph and a `memory-graph`; assert identical results. This reuses
  the ACID/graph/query suites to pin behavioral equivalence.
- **Durability tests.** commit → simulate crash (leave `.dirty`) →
  `open-memory-graph` → assert full state via snapshot+journal replay; snapshot
  compaction round-trip.
- **Replication test.** Extend the existing peer test (currently 12/12 across
  impl combos) with an in-memory device peer: hub (mmap) ↔ `memory-peer-graph`
  pull + authored-write push, assert convergence.
- **ECL first-class.** Run the full matrix on ECL (26.5.5 mac / 21.2.1 linux) —
  this backend's entire reason for existing is ECL/mobile.

---

## 11. graph-utils / algorithms reuse

Reuse **idioms**, not the transient projection structs:

- Bidirectional id map and `equalp`-on-UUID keying (proven portable in
  `algorithms/projection.lisp`).
- The `mem-*-index` id-set structures are the OLTP analog of the projection's
  adjacency — but we keep VG's typed (uuid,type) keys, not dense-id matrices,
  because we retain live node objects and typed adjacency + export semantics.
- **Convergence (Phase 3):** a native `memory-graph` *is* the in-memory graph the
  Mode-A projection currently fakes; the algorithms could run on it directly with
  no copy. Keep memory-graph adjacency cheaply enumerable to enable this.

---

## 12. Notes / smaller decisions

- **Branch (decided):** merge `peer-replication` into `experiment` first, then
  branch the memory-graph work off `experiment`. The in-memory *device*
  (`memory-peer-graph`) depends on the `peer-writer-loop` funnel that the
  `peer-replication` merge brings into `experiment`.
- **Spatial (decided):** first-class in v1 — the mobile app depends on it long
  term; see §6. Not deferred.
- **`bytes` laziness:** decide whether to pre-serialize `bytes` on create (needed
  for the journal anyway) or fully lazily. Minor; default to journal-time
  serialize.

---

## 13. As-built addendum — the checkpoint image and its versions

Not part of the original design: §7 above specifies snapshot + journal, and the
single-write **checkpoint image** was added during implementation (GH #50, #65).
Recorded here because its version numbers are an operational concern for the
Android field build, which is the only deployment that opens a pre-existing
image.

Two formats, chosen by `lazy-p`:

| graph | format | writer | current version |
|---|---|---|---|
| non-lazy | cl-store plist | `write-memory-image` | **v5** |
| `:lazy` (the field device) | VG-native blobs | `write-memory-image-native` | **v7** |

`restore-memory-image-native` reads v5, v6 and v7; anything else signals through
`%signal-unsupported-memory-image-version` (the image is a cleanly-closed memory
graph's *only* durable record — the journal is cleared on checkpoint — so it
refuses loudly rather than opening onto an empty graph). The cl-store reader
restores structurally at v5 only; anything older falls through to
`%rebuild-derived-from-nodes`.

**Version history and what each bump migrates:**

- **native v5 → v6** (GH #65): the spatial section became one record per
  `(owner . slot)` instead of one flat pair list. v5's section was empty by
  design, so v5 rebuilds spatial from nodes on open.
- **native v6 → v7, cl-store v4 → v5** (GH #104): identical layout — the pair
  codec always round-tripped values — but a spatial entry's value now carries
  its node's type tag. A v6 image's `NIL` values would restore an index no
  scoped query can filter on, so **pre-v7 images rebuild their spatial indexes
  from live nodes at open**.

Two consequences worth knowing before shipping a build:

- The pre-v7 rebuild is **lazy-safe**:
  `%rebuild-memory-spatial-indexes-from-nodes` materialises an LZNODE only when
  its class could carry geometry, so fault-on-access survives for everything
  else. It is still a one-time cost on the first open after the upgrade.
- That rebuild re-derives cells through the current `%geometry-cells`, so it
  also lands GH #103's multipolygon cover fix on memory graphs. Before the
  bump those graphs kept the old budget's cells until someone called
  `regenerate-spatial-indexes` by hand.

A rebuild cannot be replaced by re-inserting over the existing index: a repeat
`(cell . node-id)` is a duplicate-key no-op, so the tag would stay `NIL`. This is
why the version bump exists rather than an in-place upgrade.
