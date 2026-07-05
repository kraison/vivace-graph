# B+ tree experiment — mmap B+ tree vs skip-list

**Branch:** `bplus-tree` (off `experiment`). **Status:** prototype + side-by-side
benchmark complete. This is Piece 1 of `docs/next-work-handoff.md`.

## What was built

`bplus-tree.lisp` — an **mmap-backed B+ tree**, a third implementation of VG's
ordered-map protocol, living in the same `indexes.dat` heap the skip list uses
(`allocator.lisp` / `mmap.lisp`). It implements the same generics the skip list
and `mem-skip-list` do — `add-to-skip-list`, `remove-from-skip-list`,
`find-in-skip-list`, `update-in-skip-list`, `make-cursor`, `make-range-cursor`,
`cursor-next` — dispatching on `bplus-tree`, and returns `skip-node` objects from
its cursors, so **views / `:unique` / spatial can consume it unchanged** (that
wiring is the next step; the benchmark exercises the tree directly).

### Design (experiment-grade)

- **Fixed `PAGE-SIZE` pages** (default 4096 = one OS page), each a single heap
  allocation → contiguous in the mmap region. **Slotted-page** layout: 16-byte
  header + a sorted u16 slot directory + variable-length cells growing down from
  the page end. Carries the same **variable-length composite `(user-key .
  node-id)` keys** the skip-list consumers use.
- **Leaf pages** are singly linked (`next-leaf` pointer in the header) → range
  scans walk sequential-ish pages. **Internal pages** hold `P0` (leftmost child)
  in the header + `(key, child)` cells.
- **Read path is lean:** `find` / descent **binary-search the slot directory in
  the page buffer**, deserializing only the ~log₂(fanout) probed keys — not the
  whole page. A whole page moves in/out in a **single `memcpy`** (CFFI), not a
  per-byte loop.
- **Write path is correctness-first:** insert/remove **read-modify-write the whole
  page** (a page is bounded, so this is O(1) in n). Splits propagate up; a root
  split grows height. This RMW is the reason insert/remove are slow warm (see
  results) — it is the obvious first optimization, not a fundamental cost.
- **Lazy delete:** a removed key's cell is dropped from its leaf, **no merge or
  rebalance**. Correct — separators are only lower bounds, so every leaf stays
  reachable and every key still maps to the right leaf; the tree just gets less
  full. Rebalancing is deferred.
- **Concurrency:** one per-tree reader/writer lock (shared reads, exclusive
  writes) — matches how VG already reaches indexes (under the view-group /
  manager write lock; reads under a read lock). Page-latch crabbing / COW for
  fully lock-free reads is deferred until the tree earns it.
- **Persistence:** a 32-byte header block (magic, format version, root addr,
  count, height, page-size) — same shape as the skip-list header. `open-bplus-
  tree` reopens from its address; a format-version byte lets an old skip-list-
  format index be detected and rebuilt.
- **ECL-safe encoding:** keys/values stored as raw serialized bytes (VG
  `serialize`, not cl-store), compared by deserializing and calling the same
  comparison / key-equal predicates the skip list uses.

## Benchmark

`tests/perf/bplus-bench.lisp` (`(graph-db::bplus-bench)`), registered in the
`graph-db/perf-test` system. Builds a skip list and a B+ tree over an **identical
shuffled integer key set** at each N and measures:

- **Warm throughput** (µs/op, in-process, everything cached): insert, point
  lookup, range scan, remove.
- **Cold locality** — the headline metric: **distinct 4 KB pages touched** along
  each structure's *real* search path (counted independently of any in-RAM node
  cache). One cold touch of a page not in the OS cache = one page fault, so this
  is the hardware-independent predictor of cold-cache cost, measurable without
  root (no cache-drop needed). Traced symmetrically on both read paths.
- **Structure:** bytes/key, B+ tree height, skip-list max level.

### Results (SBCL 2.5.5, Apple M3, page 4096 B)

Ratio column = **skip-list / b+tree**; **>1× means the B+ tree wins**.

| metric | N | skip-list | b+tree | sl/bp |
|---|--:|--:|--:|--:|
| **insert** µs/op | 10k | 7.63 | 57.4 | 0.13× |
|  | 100k | 8.68 | 91.0 | 0.10× |
| **point-lookup** µs/op (warm) | 10k | 5.11 | **2.97** | **1.72×** |
|  | 100k | 12.68 | **4.11** | **3.09×** |
| **range-scan** µs/entry (1000) | 10k | 0.62 | **0.45** | **1.38×** |
|  | 100k | 2.22 | **0.45** | **4.92×** |
| **remove** µs/op | 10k | 9.21 | 31.7 | 0.29× |
|  | 100k | 9.95 | 35.4 | 0.28× |
| **pages / point-lookup** (cold) | 10k | 17.3 | **2.0** | **8.6×** |
|  | 100k | 22.0 | **3.0** | **7.3×** |
|  | 500k | 27.3 | **3.0** | **9.1×** |
|  | 1M | 26.6 | **3.0** | **8.9×** |
| **pages / 1000-scan** (cold) | 10k | 156 | **7.9** | **19.7×** |
|  | 100k | 741 | **8.3** | **89×** |
|  | 500k | 942 | **8.7** | **108×** |
|  | 1M | **969** | **8.3** | **117×** |
| **warm point-lookup** µs | 1M | 30.5 | **5.0** | **6.1×** |
| **warm range-scan** µs/ent | 1M | 2.63 | **0.46** | **5.7×** |
| **bytes/key** | 1M | 52.0 | **22.8** | 2.3× smaller |
| height / max-level | 1M | 53 lvls | **3** | — |

**500k row in full** — insert 16.0 / 99.1 µs (0.16×), point-lookup 30.9 / **4.8** µs
(**6.5×**), range-scan 2.66 / **0.46** µs/ent (**5.8×**), remove 15.4 / 34.8 µs
(0.44×). The skip-list's warm point lookup *degrades to 31 µs* at 500k (it now
pointer-chases ~27 scattered nodes) while the B+ tree holds at ~5 µs — the warm
gap widens with N, mirroring the cold page-touch gap.

The monotonic progression is the story: skip-list cold pages/scan climb
156 → 741 → 942 → 969 as its nodes scatter across ever more pages, while the B+
tree stays flat at ~8 (contiguous leaves); warm point-lookup speedup grows 1.7×
→ 3.1× → 6.5× → 6.1×, and the skip-list's warm point lookup *degrades to ~30 µs*
at 500k–1M (pointer-chasing ~27 scattered nodes) while the B+ tree holds at ~5 µs.

### The 1M "anomaly" — root-caused: stale benchmark files (RESOLVED)

For a while the full bench at N=1e6 tripped a *deterministic* wild-address SEGV
(`0x1899…0F8D1`), always **before the build even started**. It was **not** a B+
tree bug (every operation passed at 1e6 standalone, alongside a skip list, and
under forced heap growth; the page-I/O bounds guard never fired). The real cause:

- The bench names its heap files `bench-{sl,bp}-<N>.dat`. An early `heap-mb 1024`
  run created **1 GB** files, then crashed (leaving them behind).
- `create-memory` **does not truncate** an existing file — it opens it and maps
  it at its *current* size. So a later `heap-mb 256` run remapped the stale 1 GB
  file while recording `memory-size` = 256 MB — a size/header mismatch whose
  first mapped access faulted at a wild address, deterministically.

Fix: the bench now `delete-file`s each path before `create-memory`. With fresh
files the full bench runs clean through **1e6** (numbers above). A permanent
bounds-check guard was also added to the B+ tree page I/O (turns any bad-address
`memcpy` into a catchable error). Sharp edge worth noting: `create-memory` is
only safe on a *fresh* path — it silently reuses (does not truncate) an existing
file.

## Reading the results

- **Locality (the thesis) is confirmed decisively.** Cold, the B+ tree faults
  **7–9× fewer pages per point lookup** and **20–117× fewer per range scan**, and
  the skip-list's range-scan page count **grows with N** (156 → 741 → 942 → 969
  from 10k → 1M) as its nodes scatter across ever more pages, while the B+ tree
  stays flat (~8 pages, contiguous leaves). This is exactly where the win matters
  most: cold cache, large indexes, mobile/ECL.
- **Warm point lookup and range scan also win** (1.7–3× and 1.4–4.9×) once the
  read path binary-searches within the page instead of decoding it wholesale —
  fewer, denser objects beat pointer-chasing even when everything is cached. (An
  earlier version that eagerly decoded every cell on the way down was ~10× slower
  warm; the lean search fixed that. Lesson: the eager decode, not the structure,
  was the cost.)
- **The B+ tree is ~2.4× more space-efficient** (22 vs 51 bytes/key) — no
  per-node tower pointers, no per-node allocation header.
- **Insert/remove *were* 3–7× slower warm** — the whole-page read-modify-write.
  **Fixed by A1 (in-place cell edits, below): the B+ tree is now faster than the
  skip list on writes too.**

### A1 — in-place cell edits (write-cost killer, done)

A non-splitting insert / a delete now edits **one cell** in the page buffer —
shift the sorted slot directory by one and drop the new cell into the free gap
(compacting to reclaim delete-holes only when the gap is too small; splitting only
when the page is genuinely full) — instead of decoding every cell and re-encoding
the whole page. Cells move as raw bytes, never deserialized; the split path still
decodes (rare). Result at 500k (ratio = skip-list ÷ b+tree, >1 = B+ tree wins):

| metric | before A1 | after A1 | vs skip-list |
|---|--:|--:|--:|
| insert µs/op | ~99 | **6.9** | **2.33×** |
| remove µs/op | ~35 | **6.4** | **2.48×** |

Reads/locality/space are unchanged (point-lookup 4.9×, range-scan 6.2×, cold
pages 8–109× fewer, 2.3× smaller). **The B+ tree now wins on every operation.**
Verified: the smoke + a randomized 200k/300k-op churn test vs a reference model
(point/count/full-scan/range), and the `view-suite` still 56/56 on both backends.

### A3 — merge-on-delete (space reclamation, done)

A delete removes the leaf cell in place; if the leaf then **underflows** (empty,
or under half full) the parent **merges** it with an adjacent sibling when the two
fit in one page — freeing the vacated page and dropping the separator. Underflow
propagates up, and an internal root left with only its `P0` child **collapses**
(the tree loses a level). It is **merge-only**: a merge only ever *shrinks* the
parent, so it can never overflow it — unlike borrow/redistribute, which can grow
a variable-length separator and overflow the parent (deferred as a refinement). An
empty page always merges (its sibling alone fits), so **empty pages never linger**;
a still-underfull node whose neighbours are both too full is simply left until a
later delete shrinks one.

Verified: the churn test now **drains** every key after the random phase and
asserts the tree collapses to a single empty leaf (count 0, **height 1**, empty
scan) and still works after refill — 0 failures at 200k + 300k ops on tiny (256 B)
and realistic (4 KB) pages; `view-suite` still 56/56 on both backends.

## Verdict

The B+ tree **wins on the axes that motivated the experiment** — cold-cache
locality, range-scan locality, and space — and, with a lean in-page search, is
also **faster on warm reads**. Its only regression (write throughput) is a
known, addressable prototype artifact. This supports proceeding to wire it in
behind the unchanged ordered-map interface (`make-view-skip-list` + the `open-*`
paths + sidecar root pointers) and, per the hand-off, building **Piece 2 (the
general ordered index)** on it.

## Drop-in behind `make-view-skip-list` (validated)

The B+ tree is a **selectable backend for heap-backed indexes**, proving the
"drop-in" claim end to end. The switch is one special variable:

```lisp
(defparameter *index-backend* :skip-list)   ; or :bplus-tree
```

One shared factory — `make-heap-index` / `open-heap-index` (in `bplus-tree.lisp`,
before all three consumers) — creates/reopens every heap-backed composite-key
index, dispatching on `*index-backend*`. Views (`make-view-skip-list`), the
`:unique` index (`make-unique-skip-list`), and the spatial index
(`%spatial-make-sl`) all route through it. The choice is **persisted per index**
(views: a `:backend` key in the view alist; unique + spatial: a tag in their root
sidecar) so a graph reopens each index with the backend it was written with — an
existing graph is never disturbed by flipping the switch (a missing tag ⇒
`:skip-list`). The lifecycle touches that weren't already generic — create,
heap-address, type check, delete, and reopen — go through a small backend-agnostic
protocol (`view-index-p` / `view-index-address` / `delete-view-index` /
`view-index-backend-tag`, plus `open-heap-index`) implemented by both the skip list
and the B+ tree. Everything else (add/remove/find/update, cursors,
`%sn-key`/`%sn-value`, map-reduce roll-ups, the unique `uix-*` ops, the spatial
cell scans) was already the shared ordered-map protocol, so **no consumer logic
changed**.

Scope: this governs **views, `:unique`, and spatial** — every heap-backed index.

**Choosing it (Phase C).** The backend is a **per-graph** property, exposed on the
graph entry points:

```lisp
(make-graph name loc :index-backend :bplus-tree)   ; or :skip-list (default)
(open-graph name loc)                              ; reopens each index as written
```

`make-graph` / `open-graph` take an `:index-backend` keyword (default: the global
`graph-db:*index-backend*`); it is captured in the graph's `index-backend` slot,
which every index-creation path consults. Reopen ignores it for *existing* indexes
(each carries its own persisted backend tag) and applies it only to indexes created
afterward. graph-db doesn't read an ini file itself — set `*index-backend*` or pass
`:index-backend` from your application's own config. Validated: a graph made with
each backend builds its view + spatial (+ unique) indexes on that engine, the views
work, and after close/reopen (no keyword) every index comes back on the engine it
was written with.

**Validation.** Views (map/map-reduce, asc/desc, `:key`/range/paging, delete,
**reopen**), unique (enforcement, cross-subtype, NULL-exempt, **durable reopen
from sidecar**), and the full spatial suite set (index/query/hook/intersect/
prolog/graph-spatial, incl. **reopen**) pass **identically under both backends**:

| suite | skip-list | b+tree |
|---|--:|--:|
| SBCL `view-suite` | 56/56 | 56/56 |
| ECL `view-suite`  | 56/56 | 56/56 |
| SBCL `unique-constraint-suite` | 30/30 | 30/30 |
| SBCL spatial suites (6, 174 checks) | ✓ | ✓ |

The whole `graph-db-suite` is **1956/0** on SBCL with the default (skip-list)
backend — the wiring is a **zero-regression** change. The ECL pass matters
specifically because the B+ tree's page I/O is a raw CFFI `memcpy` through
`with-pointer-to-vector-data`; it round-trips the view codec correctly on ECL,
including reopen.

## Migrating an existing (skip-list) graph to B+ trees

`:index-backend` governs only *new* indexes, so a graph already on disk keeps its
skip lists until you rebuild them. A backend switch changes only the **derived
indexes** — the node data is untouched — so the clean migration is an **in-place
reindex**: reopen with the new backend and rebuild the three index families from
the live nodes.

```lisp
(let ((g (open-graph name loc :index-backend :bplus-tree)))
  (regenerate-all-views g)        ; views      -> B+ tree
  (regenerate-unique-indexes g)   ; :unique    -> B+ tree
  (rebuild-spatial-index g)       ; spatial    -> B+ tree
  (close-graph g))                ; persists each index's new backend tag
;; henceforth (open-graph name loc) reopens everything as B+ trees.
```

Each of these frees the old backing store and rebuilds via a type-scan
(`map-vertices`/`map-edges`, which is backend-agnostic), then persists the new
backend tag, so a subsequent plain `open-graph` reopens on B+ trees. Verified
end-to-end: all three index types switch, view lookups + unique enforcement +
spatial queries stay correct, and it survives a plain reopen.

> **`regenerate-unique-indexes`** was added for this: `rebuild-unique-indexes` is
> get-or-create, so on a reopened graph it would repopulate the *existing*
> skip-list rather than switch backends (views/spatial already delete-then-rebuild;
> unique didn't). The new function frees the old unique stores first, rebuilds on
> the current backend, and re-persists the sidecar tags.

**Snapshot + replay into a fresh graph also works** (verified end-to-end: snapshot
a skip-list graph, `make-graph … :index-backend :bplus-tree`, adopt the source
schema, `replay` — all three index families rebuild on the target's backend
because replay re-inserts nodes through the maintained `apply-transaction` path,
and view lookups / unique enforcement / spatial queries are correct through a
reopen). For a *pure backend switch* the in-place reindex above is simpler (one
graph, node data untouched), but the snapshot/replay idiom is sound.

> **Bug fixed here (`map-view`, backend-independent).** Chasing this exposed a real
> flaw in the view-query path: `invoke-graph-view`/`map-view` read the index from
> their `:graph` argument but resolved node ids via the view's `lookup-<type>`,
> which uses the dynamic `*graph*`. So `invoke-graph-view … :graph G` looked nodes
> up in `*graph*` instead of `G` — wrong (or a crash on `(vertex-table nil)`)
> whenever the two differ, e.g. querying a just-reopened graph, a second graph, or
> a replay target. `map-view` now binds `*graph*` to its `graph` argument.

### Before it graduates from a prototype
1. ~~**In-place insert/delete** to kill the whole-page RMW write cost.~~ **DONE (A1).**
2. **Concurrency:** page-latch crabbing or a COW/versioned-root story to restore
   fully lock-free reads (today: one per-tree rw-lock). *Chosen: COW, deferred —
   land A1 + A3 under the rw-lock first, then COW as its own phase.*
3. ~~**Rebalancing/merge** on delete (today: lazy, space-leaky under churn).~~
   **DONE (A3): merge-only.** Borrow/redistribute (keep every node ≥ half full) is
   the remaining refinement.
4. **Reopen + format version** wired through the sidecars, with detect-and-rebuild
   of an old skip-list-format index (template: `restore-spatial-index`).
5. **Both impls:** re-run the suite on ECL (raw-bytes encoding is already
   ECL-safe; verify no `#+ecl` gaps).
6. **Max key size:** a key+value cell must fit in a page; add a guard/definition
   (skip list has no such limit).

## Files
- `bplus-tree.lisp` — the tree (add/remove/find/update, cursors, the ordered-map
  generics, persistence).
- `tests/perf/bplus-bench.lisp` — the side-by-side benchmark (`bplus-bench`).
- `graph-db.asd` — `bplus-tree` in `graph-db/core`; `bplus-bench` in
  `graph-db/perf-test`.
