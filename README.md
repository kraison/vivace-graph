VivaceGraph
===============

VivaceGraph is an open source graph database written in pure Common Lisp.

VG takes design inspiration from CouchDB, neo4j and AllegroGraph.  It implements an ACID-compliant object graph model with user-defined indexes and map-reduce views.  As of the MVCC release it also keeps immutable, versioned nodes CouchDB-style — each update retains the prior version, with configurable retention and snapshot-isolation reads (see Chapter 12 of the manual).  A geohash-backed spatial extension answers proximity and area queries over nodes that carry a geometry (Chapter 13).  An optional graph-algorithms add-on (`graph-db/algorithms`) brings shortest paths (Dijkstra/A*), ranking (PageRank, HITS), components, max-flow, random-graph generators, and GML/Pajek/Graphviz I/O, with the streaming algorithms also exposed as Prolog predicates (Chapter 14).  It also implements a master / slave replication scheme for redundancy and horizontal read scaling, and — for offline-first / mobile fleets — a bidirectional *peer* replication mode (hub-and-spoke) that syncs each device only the authorized subset of the graph it may see, with local authoring while disconnected (Chapter 16).  Besides the default on-disk engine, an optional *in-memory* backend (`make-memory-graph`) keeps the whole graph as live Lisp objects for lowest-latency reads when it fits in RAM, with a choice of eager or fault-on-access (lazy) open (Chapter 15).  Querying the graph is accomplished via a number of Lisp methods or via a Prolog-like query language.

VivaceGraph runs on SBCL (>= 1.045), ECL (>= 26.5.5), and Clozure CL (CCL, Linux x86_64). The full test suite passes on SBCL (macOS arm64 and Linux x86_64), CCL (Linux x86_64), and ECL 26.5.5 (macOS arm64 and Linux x86_64). One ECL-only test (the full-system mixed storm) can intermittently time out late in the concurrent-stress suite on many-core Linux hosts — cumulative GC/resource state slowing tests run later in the same image, not the test itself (issue #43); every other suite is green on all three implementations. (Earlier ECL releases such as 21.2.1 are no longer supported — 26.5.5 is required.)

LispWorks support is currently **untested** for lack of access to a license; the free Personal Edition's heap limit is too small to compile VivaceGraph. The codebase still carries `#+lispworks` conditionalization, but its status is unknown until it can be exercised on a current LispWorks.

A note on CCL: it works on Linux x86_64, but **not on Apple-Silicon macOS** — the Clozure ARM64 port has been stalled for several years and macOS support for Intel (x86_64) binaries is nearly gone, so there is no usable CCL on M-series Macs. On Apple Silicon, use SBCL or ECL.

To get started, please see example.lisp.

### Documentation

A comprehensive developer's manual lives in [`docs/vivace-graph-v3-doc.org`](docs/vivace-graph-v3-doc.org), covering getting started, the storage engine and object model, transactions, the Prolog query language, views, the REST API, replication, backup/recovery, MVCC / versioned nodes, spatial queries, graph algorithms, the in-memory backend (eager vs. lazy), and offline-first peer replication, plus an API reference.

This manual was written by [Gwang-Jin Kim (@gwangjinkim)](https://github.com/gwangjinkim) — the project's first thorough documentation, and a great piece of work. Many thanks to him. It has been adopted here and is maintained alongside the code; newer chapters (such as Chapter 12 on MVCC) are maintainer additions written in his style.

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

- **Pluggable ordered-index backend.** Views, `:unique` constraints and the spatial index can now run on a page-oriented **B+ tree** instead of the skip list — selected per graph with `:index-backend :bplus-tree` on `make-graph` / `open-graph` (default stays `:skip-list`). On disk the B+ tree wins on every operation once warm. Each index remembers its own backend, and an existing graph is migrated in place with `regenerate-all-views` / `regenerate-unique-indexes` / `rebuild-spatial-index`. (Manual Chapter 3.)
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
