# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

VivaceGraph (`graph-db`) is an ACID-compliant object graph database written in pure Common Lisp, taking design inspiration from CouchDB, neo4j and AllegroGraph. It supports user-defined indexes, map-reduce views, master/slave replication, and a Prolog-like query language. The on-disk store is built from scratch on memory-mapped files — there is no external storage engine.

Supported implementations: SBCL (>= 1.045), Clozure CL (CCL; Linux x86_64 only — see below), ECL (>= 21.2.1; full suite green on ECL 21.2.1 and 26.5.5, macOS arm64 + Linux), and LispWorks. CCL is usable on Linux but **not on Apple-Silicon macOS**: the Clozure ARM64 port has been stalled for ~5 years, and macOS support for Intel (x86_64) binaries is nearly gone, so there is no working CCL on an M-series Mac (development here is on an M3). On Apple Silicon, develop/test with SBCL or ECL; reach for CCL only on Linux. Code is heavily conditionalized with `#+sbcl` / `#+ccl` / `#+ecl` / `#+lispworks` reader macros — when editing low-level code (locks, MOP, mmap, hash tables) you usually must handle all four. ECL has its own gotchas worth knowing before touching that code: it has no custom hash-table tests (use `equalp`, never `:test`/`:hash-function`), cl-store can't serialize structs without the shim in `cl-store-ecl.lisp`, `defstruct` makes a setf *expander* but no `(setf accessor)` function, and any per-impl reader-conditional that wraps a value or a macro `,@body` must include an `#+ecl` branch or the form silently becomes empty.

## Loading, building, running

This is an ASDF system loaded through Quicklisp; there is no separate build step.

```lisp
(ql:quickload :graph-db)   ; compile + load the whole system
(in-package :graph-db)
```

Dependencies (bordeaux-threads, alexandria, cffi, osicat, uuid, cl-store, hunchentoot, ningle, clack, log4cl, usocket, etc.) must be resolvable by Quicklisp/ASDF. `osicat` and `cffi` mean a working C toolchain is required (the mmap layer binds `mmap`/`munmap`).

**Component load order matters.** `graph-db.asd` encodes a strict dependency chain from the storage primitives up to the query/REST layers; if you add a file, place it in the right position and declare its `:depends-on`.

`example.lisp` is the canonical end-to-end walkthrough (schema definition → views → transactional inserts → queries) and is the best starting point for understanding the public API.

`docs/vivace-graph-v3-doc.org` is a full developer's manual (storage engine, object model, transactions, Prolog queries, views, REST, replication, backup/recovery, API reference). Originally written by Gwang-Jin Kim (@gwangjinkim) and adopted into this repo; keep it in sync when behavior changes.

## Tests

**There IS an automated test framework** — FiveAM, ~50 files across `tests/` and its
subdirectories, **3,359 checks** as of 2026-08-08. Run a suite through ASDF:

```
(asdf:test-system :graph-db)              ; the main suite
(asdf:test-system :graph-db/concurrency-test)
(asdf:test-system :graph-db/acid-test)
(asdf:test-system :graph-db/stress-test)
```

**SBCL needs `--dynamic-space-size 16384`** to run a suite. The default 1 GiB heap dies
with "Heap exhausted, game over" partway in — `make-graph`'s type index eagerly builds
131,072 index-lists, and a suite creates many graphs in one image.

Each `test-op` errors on failure, so they are safe to gate on. Run the main suite before
committing an engine change; the app that consumes this engine has its own 5115-check suite
that will not catch a defect in here.

⚠ **This line said "there is no automated test framework" until 2026-08-08, long after the
suites existed.** The consuming app's CLAUDE.md records the same false claim propagating
into five documents and costing a feature its verification. Correct stale facts *here*, at
the source, not only in whatever tripped over them.

The older ad-hoc REPL exercises still exist alongside the suites:

- `test.lisp` — allocator/mmap exercises (`test-bins`, `test-alloc`) and vertex/edge stress tests (`vertex-test`, `edge-test`, `stress-test`); creates a graph under `/var/tmp/graph/`.
- `test-lhash.lisp` — linear-hash benchmarks and `sb-profile` profiling helpers (SBCL-only).
- `test-mop.lisp`, `xach-test.lisp` — focused experiments.

To run one, e.g.:

```lisp
(ql:quickload :graph-db) (in-package :graph-db)
(test-alloc)
```

Tests write scratch databases under `/var/tmp/`; clean those up between runs if state seems stale.

## Architecture (bottom-up)

The system is layered. Lower layers know nothing of graph semantics; higher layers build on them.

1. **Storage primitives** — `mmap.lisp` (CFFI/osicat memory-mapped files, with SEGV-retry `:around` methods on `set-byte`/`get-byte`), `allocator.lisp` (binned heap allocator over an mmap'd region), `buffer-pool.lisp`, `serialize.lisp` (custom binary (de)serialization; type tag bytes are defined in `globals.lisp`), `pcons.lisp`/`pmem.lisp` (persistent cons cells).
2. **On-disk collections** — `linear-hash.lisp` (linear hashing, the main key→offset table), `skip-list.lisp` + `skip-list-cursors.lisp`, `index-list.lisp`.
3. **Graph indexes** — `ve-index.lisp` (vertex↔edge adjacency, in/out), `vev-index.lisp` (vertex-edge-vertex), `type-index.lisp` (nodes by type). A graph keeps separate `ve-index-in`, `ve-index-out`, and `vev-index` instances.
4. **Graph model** — `graph-class.lisp`/`graph.lisp` (the `graph`/`master-graph`/`slave-graph` classes and `make-graph`/`open-graph`/`close-graph`), `schema.lisp` (node-type registry, per-class rw-locks), `node-class.lisp` + `primitive-node.lisp` + `vertex.lisp` + `edge.lisp` (the MOP-based persistent object model), `views.lisp` (map/map-reduce indexes), `gc.lisp`.
5. **Transactions** — `transactions.lisp` (ACID with read-set/write-set validation and retry; `*maximum-transaction-attempts*` then exclusive lock), `transaction-restore.lisp`, `transaction-log-streaming.lisp`, `transaction-streaming.lisp`. Replication: `backup.lisp`, `replication.lisp`, `txn-log.lisp`.
6. **Query** — a full embedded Prolog engine: `functor.lisp`, `prologc.lisp` (compiler), `prolog-functors.lisp` (built-in predicates). `select` / `select-flat` / `select-one` / `do-query` are the query entry points. `interface.lisp` + `traverse.lisp` provide the Lisp-method query API (`map-vertices`, `map-edges`, `traverse`, `outgoing-edges`, etc.).
7. **REST** — `rest.lisp` exposes the graph over HTTP (hunchentoot/ningle/clack); `start-rest` / `stop-rest` / `def-rest-procedure`.

### On-disk layout

Each graph is a directory. `make-graph`/`open-graph` create/expect: `heap.dat`, `indexes.dat`, `vertex/` and `edge/` (linear-hash tables), `ve-index-in/`, `ve-index-out/`, `vev-index/`, `vertex-index.dat`, `edge-index.dat`, `schema.dat`, and a `.dirty` marker file.

**The `.dirty` file is the crash sentinel.** It is written on open and deleted on clean `close-graph`. If it exists when you call `open-graph`, the graph was not closed properly and `open-graph` signals an error — recovery (`recover-transactions`, and possibly `snapshot`/`replay`/`restore`) is required. Always `close-graph` to release mmaps and remove `.dirty`.

### Defining and using a graph (public API shape)

- Schema is defined with `def-vertex` / `def-edge` (CLOS-like, with typed slots and single inheritance), associated with a named graph. See `example.lisp`.
- `def-view` defines secondary indexes; the `:map` lambda calls `yield`, and an optional `:reduce` lambda makes it a map-reduce view. Query views with `invoke-graph-view` / `map-view` / `map-reduced-view`.
- All mutations go through `with-transaction`. `make-<type>` constructors, `save`, `update-node`, `delete-node`/`mark-deleted` operate inside a transaction.
- `*graph*` is the dynamically-bound "current graph" used by most operations when no explicit graph is passed.

## Conventions / gotchas

- The compatibility note in `README.md` (2016-12-12) records a breaking change to the UUID/hash scheme: graphs created before commit `58f87d6` are incompatible and must be re-loaded via snapshot + `replay`.
- Concurrency uses bordeaux-threads plus a custom `rw-lock.lisp` (SBCL/LispWorks/ECL; CCL uses its native `make-read-write-lock` via shims in `utilities.lisp`); per-class and per-view rw-locks are managed via `with-write-locked-class` / `with-read-locked-class` / `with-*-locked-view-group`.
- Logging is log4cl (`log:info`, `log:error`, …); `example.lisp` shows configuring it to `/var/tmp/graph.log`.
- `demo/` contains an external application (`social-shopping`) that depends on `graph-db` — it is illustrative usage, not part of this system, and pulls in many extra dependencies.
