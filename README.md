VivaceGraph
===============

VivaceGraph is an open source graph database written in pure Common Lisp.

VG takes design inspiration from CouchDB, neo4j and AllegroGraph.  It implements an ACID-compliant object graph model with user-defined indexes and map-reduce views.  As of the MVCC release it also keeps immutable, versioned nodes CouchDB-style — each update retains the prior version, with configurable retention and snapshot-isolation reads (see Chapter 12 of the manual).  A geohash-backed spatial extension answers proximity and area queries over nodes that carry a geometry (Chapter 13).  An optional graph-algorithms add-on (`graph-db/algorithms`) brings shortest paths (Dijkstra/A*), ranking (PageRank, HITS), components, max-flow, random-graph generators, and GML/Pajek/Graphviz I/O, with the streaming algorithms also exposed as Prolog predicates (Chapter 14).  It also implements a master / slave replication scheme for redundancy and horizontal read scaling, and — for offline-first / mobile fleets — a bidirectional *peer* replication mode (hub-and-spoke) that syncs each device only the authorized subset of the graph it may see, with local authoring while disconnected (Chapter 16).  Besides the default on-disk engine, an optional *in-memory* backend (`make-memory-graph`) keeps the whole graph as live Lisp objects for lowest-latency reads when it fits in RAM, with a choice of eager or fault-on-access (lazy) open (Chapter 15).  Beyond map-reduce views, a slot may be indexed by value — ordered, unique, or as a dense embedding vector answering cosine k-nearest-neighbour search (Chapter 8).  Several graphs may be open in one image under a defined, enforced contract (Chapter 17).  Querying the graph is accomplished via a number of Lisp methods or via a Prolog-like query language.

VivaceGraph runs on SBCL (>= 1.045), ECL (>= 26.5.5), and Clozure CL (CCL, Linux x86_64). As of 3.0.0 the full test suite — seven suites, ~5,400 checks — passes with zero failures on SBCL 2.6.6 and ECL 26.5.5, on both macOS arm64 and Linux x86_64. One ECL-only test (the full-system mixed storm) can intermittently time out late in the concurrent-stress suite on many-core Linux hosts — cumulative GC/resource state slowing tests run later in the same image, not the test itself (issue #43). (Earlier ECL releases such as 21.2.1 are no longer supported — 26.5.5 is required.)

**CCL has a known concurrency limitation as of 3.0.0** ([#118](https://github.com/kraison/vivace-graph/issues/118)): a vector-segment writer starves against sustained concurrent scanners, so the two segment growing-writes tests do not terminate there. CCL alone does not use this repo's `rw-lock.lisp`, whose FIFO writer queue makes writer starvation structurally impossible — it uses `ccl:make-read-write-lock`, which offers no writer fairness. The shims long predate this release; vector segments are simply the first workload that provokes it. Because the main suite does not terminate on CCL, **3.0.0 is not fully validated there** — treat CCL as best-effort for this release. SBCL and ECL are unaffected.

LispWorks support is currently **untested** for lack of access to a license; the free Personal Edition's heap limit is too small to compile VivaceGraph. The codebase still carries `#+lispworks` conditionalization, but its status is unknown until it can be exercised on a current LispWorks.

A note on CCL: it works on Linux x86_64, but **not on Apple-Silicon macOS** — the Clozure ARM64 port has been stalled for several years and macOS support for Intel (x86_64) binaries is nearly gone, so there is no usable CCL on M-series Macs. On Apple Silicon, use SBCL or ECL.

To get started, please see example.lisp.

### Documentation

A comprehensive developer's manual lives in [`docs/vivace-graph-v3-doc.org`](docs/vivace-graph-v3-doc.org), covering getting started, the storage engine and object model, transactions, the Prolog query language, views, the REST API, replication, backup/recovery, MVCC / versioned nodes, spatial queries, vector indexes and cosine kNN search, graph algorithms, the in-memory backend (eager vs. lazy), and offline-first peer replication, plus an API reference.

This manual was written by [Gwang-Jin Kim (@gwangjinkim)](https://github.com/gwangjinkim) — the project's first thorough documentation, and a great piece of work. Many thanks to him. It has been adopted here and is maintained alongside the code; newer chapters (such as Chapter 12 on MVCC) are maintainer additions written in his style.

### Announcement, 2026-08-09 — VivaceGraph 3.0.0 (breaking)

A major release. It adds vector similarity search, general ordered indexes, and a defined multi-graph contract; it reworks the spatial extension from a single graph-wide index into a registry of per-class indexes; and it is breaking on both the public API and the on-disk format. Existing graphs still open in place — what migrates does so automatically at first open — but stale call sites and old Prolog arities do not survive.

**What's new:**

- **Vector indexes and cosine kNN search.** A slot declared `:vector-index t` gets a dedicated mmap *vector segment*, maintained by the transaction apply path itself — no parallel write path and no cache to keep in sync. `vector-search` returns the top-k nodes of a class (and its subclasses) by cosine similarity. Segments grow past their initial address-space reservation by re-reserving and relocating under the segment's own write lock, so the reservation is no longer a ceiling. (Manual Chapter 8.)
- **General ordered indexes — index a slot by its value (`:index` / `def-index`).** The non-unique counterpart to `:unique`, on the same machinery: `(slot :index t)`, or `(def-index user email :social-app :canonicalize string-downcase)` away from the class, declarative and idempotent like `def-view`. Query with `index-lookup` (equality), `index-range` and `map-index` (ordered ranges). Durable — the root is persisted in a sidecar and reopened by address, not rebuilt by scanning every node at open. (Chapter 8.)
- **Multi-graph support is now a defined, enforced contract (#53).** Running several graphs in one image mostly worked before; it now has stated semantics and tests. A read-write transaction belongs to exactly one graph — touching a node whose home graph differs signals the new `cross-graph-transaction-error`, where a cross-graph read previously returned `NIL` for a node that exists. Nodes carry their home graph, so a node's heap resolves through *its own* graph rather than the ambient `*graph*` (it previously dereferenced its offset in the wrong memory-mapped file). Read-only snapshots are per graph and compose. Node class names must now be unique across all graph schemas. (Chapter 17, "Multiple Graphs in One Image".)
- **Per-`(owner-class . slot)` spatial indexes**, replacing the single graph-wide one, with per-index geohash precision declared by a `:spatial-precision` slot option, a bounded insert-side cell cover, and a self-healing query clamp.
- **`:ephemeral` slot option now works (#90)** — it had no effect at all previously; such slots were categorized persistent and stored like any other.
- **`:spatial-index-backend`** picks the ordered-map backend for spatial indexes independently of the rest of the graph — measurements favour the skip list for spatial workloads even where the B+ tree wins elsewhere (#91).

**What breaks (API):**

- **Every spatial query now takes a required scope as its first argument** — a node-class name, a list of names, or `:all`. `(find-nodes-within area)` becomes `(find-nodes-within 'my-class area)`; likewise `find-nodes-intersecting`, `find-nodes-near`, and `find-nearest-k`. The scope both selects which per-class indexes are scanned and type-filters the results. Stale call sites are a compile-time warning on SBCL and ECL.
- **The single graph-wide spatial-index accessor and the old singular whole-graph rebuild function are gone.** There is no longer one index to name or rebuild: use `spatial-indexes` / `spatial-index-for`, and `rebuild-spatial-indexes` (all) / `regenerate-spatial-index` (one).
- **The old unscoped Prolog arities are gone** — `find-within`/`find-intersects` at arity 2 and `find-near`/`find-nearest` at arity 4 — replaced by the scoped `find-within/3`, `find-intersects/3`, `find-near/5`, `find-nearest/5` (the scope rides in second position). A stale query fails at goal entry with an unknown-functor error.
- **`*TRANSACTION*` is `NIL` inside a read-only snapshot** (`with-read-snapshot`, and `select`/`do-query` with `:snapshot t`). Snapshots populate the new `*read-snapshots*` instead; call `read-transaction` to ask which transaction a read of a given graph actually resolves through. Code that tested `*transaction*` to detect "am I inside a query" sees `NIL` there.
- **Node class names are now globally unique across graph schemas**; a collision signals `duplicate-node-class-error` where the second definition previously replaced the first class's slots in silence.

**Performance.** A scoped spatial query now costs its results rather than the whole index's population (#104 — every index entry carries its node's type tag and the scope is applied *inside* the range scan, so a candidate outside the scope is never consed or materialized; previously a 206-result query on a shared 296,932-point index cost the same 120 ms as a 29,739-result one); a view's map/reduce source is compiled once instead of once per node (#89 — ~3.6x faster writes, ~85% less allocation); point-in-polygon stops boxing every coordinate (#86 — one workload from 300.6 MB to 33.1 MB); slot categorization is cached per class (#87); the B+ tree stops re-reading the leaf page its descent just read (#97).

**What migrates automatically:** the spatial sidecar goes to `spatial-indexes.dat`, format v5, and the memory-graph image to v7 (native) / v5 (cl-store). An existing graph **re-derives its spatial indexes from the live node geometries at first open** — index only, node data untouched, nothing re-fetched — so you open in place with no separate migration step. The snapshot text format also changed one-way compatibly: this version reads older snapshots, but older versions cannot read snapshots written by this one.

**Downgrade after migration is unsupported:** an older build reopens the stale `spatial-index.root` as a silently empty or out-of-date index. Snapshot first if you need a fallback.

**Known limitations ship documented rather than hidden** — see the "Known limitations" section of the changelog for the six carried into 3.0.0, most notably that the general ordered index is not yet reachable from Prolog (#102) and that a scan-then-write transaction serializes the graph (#92, deferred to 3.1).

See Chapters 8, 13 and 17 of the developer's manual and [`CHANGELOG.md`](CHANGELOG.md) for the full story.

### Announcement, 2026-07-06 — VivaceGraph 2.1.1 (bug fix)

A bug-fix release. Fixes an **ECL-only** bug where `edge-exists-p` (and a generated `make-<type>`'s type resolution) could fail when operating on a graph other than the current `*graph*`: a ve/vev index read deserialized its index-list against the wrong heap. No API or on-disk format change; upgrade in place. See [`CHANGELOG.md`](CHANGELOG.md).

### Announcement, 2026-07-05 — Repository renamed to `vivace-graph`

This repository was renamed from **`vivace-graph-v3`** to **`vivace-graph`**. The "v3" marked the third ground-up redesign over 20+ years of work; that history has served its purpose, and the plain name is clearer going forward.

**Nothing you depend on changes.** GitHub permanently redirects the old URLs — web, `git clone` / `fetch` / `push` over both HTTPS and SSH, and the API — so existing clones keep working untouched. The ASDF system is still `graph-db`, so `(ql:quickload :graph-db)` and `:depends-on (:graph-db)` are unaffected. To tidy an existing checkout, you can repoint the remote:

```
git remote set-url origin git@github.com:kraison/vivace-graph.git
```

### Announcement, 2026-07-05 — VivaceGraph 2.1.0

A large, **backward-compatible** feature release. Highlights:

- **Pluggable ordered-index backend.** Views, `:unique` constraints and the spatial index can now run on a page-oriented **B+ tree** instead of the skip list — selected per graph with `:index-backend :bplus-tree` on `make-graph` / `open-graph` (default stays `:skip-list`). On disk the B+ tree wins on every operation once warm. Each index remembers its own backend, and an existing graph is migrated in place with `regenerate-all-views` / `regenerate-unique-indexes` / `regenerate-spatial-indexes`. (Manual Chapter 3.)
- **`:unique` slot constraints (issue #6).** Declare `(slot … :unique t | equal | equalp | <canonicalizer>)`; enforced atomically at the commit boundary, NULL-exempt, backed by a durable per-graph index. (Chapter 8.)
- **Offline-first peer replication (Chapter 16).** A bidirectional hub-and-spoke *peer* mode for mobile/edge fleets: each device syncs only the authorized subset it may see, authors locally while disconnected, and reconciles on reconnect.
- **In-memory backend — `make-memory-graph` (issue #50, Chapter 15).** The whole graph as live Lisp objects for lowest-latency reads, with the same API; eager or lazy open.
- **Modernized Prolog engine + safe web query surface (issues #44/#45).** First-class control flow (`\+`, `->`/`;`, `once`, `forall`, `call/N`), `findall`/`bagof`/`setof`, ISO `catch`/`throw`, per-query resource bounds and effect policies, and read-only JSON/`def-query` HTTP endpoints.
- **Correctness.** A codebase-wide sweep fixed a class of "wrong-graph" bugs (operations resolving against the dynamic `*graph*` instead of the graph in hand).

**No migration required** — existing on-disk v2 graphs open unchanged. See [`CHANGELOG.md`](CHANGELOG.md) for the full list.

### Announcement, 2026-06-06 — MVCC and storage format v2 (breaking)

The MVCC release adds immutable, versioned nodes (issue #19): each update now retains the previous version of a node in a heap-backed chain, reclaimed by a lazy, epoch-gated reaper according to a configurable `:keep-revisions` policy. This brings configurable history, snapshot-isolation reads for transactional lookups, and — as a bonus — finally dissolves the long-standing node-data read-after-free race at its source.

**This bumps the on-disk storage format to v2 (the node head grew from 15 to 31 bytes).** A v2 build will refuse to open a pre-MVCC (v1) graph and direct you to migrate. To migrate an existing graph, use the new `migrate-graph` function (a logical snapshot + replay that leaves the original untouched):

```lisp
;; load your schema (def-vertex / def-edge) first, then:
(migrate-graph :my-app "/path/to/old-v1-graph/" "/path/to/new-v2-graph/" :package :my-app)
```

See Chapter 12 of the developer's manual for the full story (versioning, retention, snapshot reads, and migration).

### Announcement, 2024-10-15

The author is still volunteering in Ukraine, and is looking for help maintaining this codebase while he is away (and after he returns).  Please let kevin@chatsubolabs.com know if you are interested in helping out — there is now a comprehensive developer's manual (see above) and a growing automated test suite, but plenty of open issues remain.

### Announcement, 2016-12-12

Folks, I recently checked in a few breaking changes to the VG3 repo that
you should be aware of.  In particular, the hashing scheme used for
vertices and edges was not distributing keys very well and graphs would
slow down terribly as they grew very large (1,000,000+ nodes).  I have
updated the UUID generation code as well as the hash functions for
vertices, edges, ve indices and vev indices.  My performance tests show
an improvement in loading a 10,000,000 node snapshot from 2.5 days to
about 2 hours.  Hash key distribution is largely responsible for this
change, but so is an optimization made to the hashing function.  As of
commit 58f87d60e767d868cf30b8e6f1ec0bfc9d6d0b1e , existing graphs will
not work.  I suggest that you take a snapshot of your graph(s) and
reload them using the REPLAY function.  Please let me know if you have
any questions or concerns.
