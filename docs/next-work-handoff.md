# Hand-off: B+ tree experiments, then the general ordered index

**Written 2026-07-05.** This picks up after the `:unique` constraint (#6) + spatial
composite-key + skip-list perf work landed on `experiment`. Do the two pieces **in
this order**: (1) B+ tree experiments, then (2) the general ordered index.

---

## 0. Where things stand

- **Branch:** everything is merged to `experiment` (merge `b7f2b44`), pushed to origin.
  `master` is the release branch; do engine work on a branch off `experiment`.
- **Green:** full suite SBCL **1956/0**, ECL **1950/0** (the one ECL "fail" is the
  intentional `MIGRATE-V1-GRAPH-TO-V2` skip — an SBCL-cl-store'd fixture ECL can't read).
  All four multi-process peer harnesses pass (see §5).
- **Supported impls:** SBCL + ECL on this Mac (Apple Silicon). CCL is Linux-only. Test
  both SBCL and ECL for anything touching storage/serialization.

### What just landed (context you'll build on)

1. **Skip-list `remove`/`update` were O(n)** — `%find-kv-in-skip-list` re-walked the base
   list from the head. Fixed for **duplicate-free** lists by routing
   `remove-from-skip-list` through the O(log n) `%find-in-skip-list` when
   `duplicates-allowed-p` is nil (`skip-list.lisp`). Duplicate-key lists keep the old
   `%find-kv` path (correct-but-O(n)); the fix below removes the last such list.
2. **Composite-key indexing is now the engine idiom.** Views, the `:unique` index, and
   the spatial index all key their skip-list by a **duplicate-free composite
   `(user-key . node-id)`** instead of a duplicate-key `user-key -> id` map. Folding the
   id into the key makes every op O(log n) and correct on both backends. This is the
   pattern the general ordered index will reuse wholesale.
3. **`less-than` / `greater-than` now order lists lexicographically** (`utilities.lisp`).
   The base case `(less-than nil nil)` used to return T, so an equal/prefix list ranked
   strictly less than itself — corrupting any skip-list keyed by a composite `(list ...)`
   key. Only surfaced once `:origin` unique keys became `(origin-token . value)` lists.
4. **`:unique` constraints** (#6): declarative `:unique <spec>` slot option, commit-
   boundary enforcement, durable on both backends, `:scope :local | :origin` (peer
   partitioning). Manual Chapter 8. Deferred: global-scope hub arbiter → **GH #51**.

---

## 1. Shared substrate: the ordered-map "seam" (READ THIS FIRST)

Both pieces build on one abstraction. Views, unique, and spatial never call a concrete
structure — they go through a set of **generic functions** that an ordered-map backend
implements:

- `add-to-skip-list (sl key value)`
- `remove-from-skip-list (sl key &optional value)`
- `find-in-skip-list (sl key &optional preds succs)`
- `update-in-skip-list (sl key value &optional old-value)`
- `make-cursor (sl)` / `make-range-cursor (sl start end)` → `cursor-next`

There are **already two implementations** of this protocol:
- `skip-list.lisp` — the on-disk, heap-backed skip list (mmap pages via `allocator.lisp`).
- `mem-skip-list.lisp` — an in-RAM skip list for `memory-graph` (same protocol, different
  storage). This is the **proof that a second backend drops in cleanly.**

`make-view-skip-list (graph view)` (`views.lisp`) is the dispatch point: a normal graph
gets a heap skip-list; a `memory-graph` overrides it to return a `mem-skip-list`. The
unique and spatial indexes both build their lists through this same path.

**Composite-key codec** (shared by views/unique/spatial):
- Key on disk is `(user-key . node-id)` where `node-id` is a 16-byte uuid.
- `view-key-serialize` / `view-key-deserialize` (`views.lisp`): serialize `(payload . id)`
  as `[16-byte id][serialize(payload)]`. **Gotcha:** VG's `serialize` cannot round-trip a
  raw `(unsigned-byte 8)` array, so the id is stored as raw bytes and the payload is
  serialized; the skip-node **value is unused (nil)** — read the id back from
  `(second (%sn-key node))`, not the value.
- Ordering: `reduce-comp-lessp` compares `(first key)` via `less-than`, tie-breaks on the
  id via `key-vector<`. Equality: `reduce-equal`.
- A cell/prefix lookup is a **range cursor** over `[(list user-key +null-key+)
  (list user-key +max-key+)]` — see `uix-lookup` (`unique-constraint.lisp`) and
  `spatial-index-query-bbox` (`spatial-index.lisp`) for the two live examples.

---

## 2. PIECE 1 — B+ tree experiments (do this first)

### Why (motivation is *locality*, not correctness)

The O(n) `remove` bug is already fixed by composite keys, so this is **not** a bug hunt.
The open question is **mmap/disk locality**:

- A skip-list lookup is `~log2(n)` pointer hops to **randomly-located** nodes — each hop a
  potential page fault, poor cache behavior; worst exactly where it matters (cold cache,
  mobile/ECL, large indexes). Range scans hop pointers across pages.
- A B+ tree packs hundreds of keys per page → `log_B(n)` page touches with high fanout,
  and **leaf-linked range scans are sequential pages** (ideal for mmap + OS prefetch).
- The skip-list also carries concurrency machinery (per-node locks, a node cache,
  SEGV-retry) that is an in-memory strength paid for on disk without the locality payoff.

### The experiment

1. **Prototype an mmap'd B+ tree** on its own branch off `experiment`. It lives in the
   graph's `indexes.dat` heap (same `allocator.lisp` / `mmap.lisp` the skip list uses).
2. **Implement the ordered-map generics** from §1 for it (a third backend). If the
   protocol is satisfied, views/unique/spatial/general-index consume it unchanged — verify
   by running their suites against a B+ tree-backed `make-view-skip-list`.
3. **Cold-cache benchmark** vs the skip list for the real workloads: view maintenance,
   unique lookup/insert/remove, spatial prefix-range scans, large-index point lookups.
   Measure cold (drop caches / fresh process) — that's where the locality win shows.
   Reuse the harness style from this session's perf probes (isolate the op, vary n,
   report µs/op; the skip-list perf numbers are in commit messages `f3dce63`/`b2b803b`).
4. **Decide:** if the B+ tree wins on disk (expected), plan migrating the substrate under
   the unchanged interface. The composite-key uniformity means every consumer already
   speaks `(user-key . id)` keys, so the swap is localized to `make-view-skip-list` +
   `open-*` paths + persistence (root pointers in the sidecars).

### Key files
- `skip-list.lisp`, `skip-list-cursors.lisp` — the interface + cursor protocol to match.
- `mem-skip-list.lisp` — the second-backend precedent (how to implement the generics
  cleanly for a non-heap store).
- `allocator.lisp`, `mmap.lisp` — the heap/paging the B+ tree nodes live in.
- `views.lisp` (`make-view-skip-list`, `open-skip-list` call sites) — the dispatch seam.

### Watch out for
- **Concurrency model.** The skip list is lock-free-ish with per-node locks; VG reads are
  lock-free with a stable-address mmap (see memory `mmap-remap-race`). A B+ tree needs a
  page-latching or COW story for concurrent readers during splits/merges. Match VG's
  "lock-free reads, single-writer-ish commit" model — don't regress read concurrency.
- **Persistence + reopen.** Root page pointer goes in the same sidecars views/unique/
  spatial use; add a format version so an old skip-list-format index can be detected and
  rebuilt (the spatial migration in `graph.lisp` `restore-spatial-index` is the template).
- **Both impls.** ECL has no custom hash tests and cl-store struct quirks; keep the B+ tree
  node encoding in the raw-bytes style the skip list uses (`read-skip-node` etc.), not
  cl-store.

---

## 3. PIECE 2 — General ordered index (after the B+ tree)

### What it is

Generalize the `:unique` machinery into a **non-enforcing ordered secondary index**: a
duplicate-free composite `(slot-value . node-id)` map maintained on the commit apply path,
supporting **equality lookup AND range scans**. It is literally *"the unique index minus
enforcement"* — same composite key, same codec, same backends, same persistence. A
non-unique slot just means many nodes share a `slot-value` prefix, retrieved by range-
scanning that prefix (exactly how the spatial index retrieves many ids per cell now).

### Two use cases (the user wants both)
1. **Query accelerator for Prolog/Datalog `select`** — equality and range predicates on an
   indexed slot resolve via the index instead of a full scan. Wire into the query layer
   (`prologc.lisp` / `prolog-functors.lisp` / `interface.lisp`).
2. **User-facing "nodes ordered by slot X" API** — ordered iteration / range queries over a
   slot, e.g. `map-index` / `index-range`.

### Design starting points
- **Template:** `unique-constraint.lisp` — copy the index struct, `%unique-index-for`
  (get-or-create), the backend-agnostic `uix-lookup`/`uix-put`/`uix-remove`/`uix-count`,
  and the apply hooks (`apply-tx-write-to-unique-indexes`), then **drop the `validate`
  enforcement** (an ordered index never rejects). Keep the maintenance half.
- **Declarative registration** like `def-view` / `install-views` (the #49 two-phase
  registry in `views.lisp`) — a `def-index` that is idempotent across restarts.
- **`index.lisp` is a DEAD stub** (`make-string-index`, not in the `.asd`, zero callers) —
  the abandoned first attempt. Either delete it or repurpose the name; don't assume it
  works.
- **Peer replication:** maintain on the pull-apply paths too, exactly like unique
  (`apply-peer-create-writes` / `apply-peer-authored-op` already call the unique
  maintenance; add the ordered-index maintenance alongside). No enforcement, so no
  `:origin`/conflict machinery needed — simpler than unique.
- **Backend:** builds on whatever §1 substrate wins (skip list today, B+ tree if it wins).
  That is the reason to do the B+ tree first — so the general index is built on the final
  substrate rather than migrated later.

### Key files
- `unique-constraint.lisp` (the template), `views.lisp` (registration + `make-view-skip-list`
  + composite codec), `transactions.lisp` (apply hooks: `apply-tx-writes-to-*`),
  `peer-streaming.lisp` (pull-apply maintenance), `interface.lisp`/`traverse.lisp` +
  `prolog-functors.lisp` (query surface), `index.lisp` (dead stub to reckon with).

---

## 4. Cross-cutting gotchas (bit us this session)

- **`serialize` can't round-trip a raw ub8 array** (ids/uuids). Store the id in the
  composite key (raw bytes via `view-key-serialize`), store `nil` as the skip-node value.
- **`def-vertex` hard-codes the graph name at macroexpansion.** In tests, the graph you
  `make-graph` MUST be named the same as the `def-vertex`'s graph, or you get
  "NIL not of type NODE-TYPE".
- **ECL:** no custom hash-table tests (use `equalp`), cl-store needs the struct shim, and
  any per-impl reader-conditional wrapping a value/`,@body` needs an `#+ecl` branch.
- **Peer graphs reopen as plain `graph`** unless you pass `:peer-role`/`:origin-id` to
  `open-graph`. Peer-only slots (`node-origins`, `field-stamps`) have base-class nil
  fallbacks so plain-graph access doesn't crash.
- **`less-than`/`greater-than` on lists** are now correct — rely on them, but if you add a
  new key shape, add a direct ordering test (`skip-list-tests.lisp` has one).

---

## 5. How to build, test, verify

```lisp
(ql:quickload :graph-db)          ; or :graph-db/test for the suite
(in-package :graph-db)
```

- **Full in-process suite** (both impls). Runner scripts used this session live in the
  session scratchpad; the essence is:
  `(ql:quickload :graph-db/test) (fiveam:run! 'graph-db/test::graph-db-suite)`.
  Run under `sbcl --non-interactive --load <runner>` and `ecl --load <runner>`.
- **Multi-process peer harnesses** (hub + device as separate OS processes — the ONLY
  faithful peer test; the in-process suite does not cover the socket paths):
  - `tests/peer-replication/run-peer-test.sh` (Branch A state-sync + purge)
  - `tests/peer-replication-push/run-push-test.sh` (authored push + hub re-home)
  - `tests/peer-replication-multi/run-multi-peer-test.sh`
  - `tests/replication/run-replication-test.sh` (master/slave)
  - Ship config: `REPL_HUB_LISP_CMD="sbcl --non-interactive --load"
    REPL_DEVICE_LISP_CMD="ecl --load" bash tests/peer-replication/run-peer-test.sh`
- **Live REPL bridge:** `tools/lisp-eval.sh` (eval server) / the MCP `lisp` tools — drive
  the live image for incremental dev+profiling (how the perf bugs were found). See memory
  `lisp-repl-bridge`.
- **Standing rule:** show the full diff for review before any `git commit`; push is
  explicit-only; engine work on a branch (this session used `unique-constraint`, merged to
  `experiment`).

---

## 6. Deferred / related
- **GH #51** — hub arbiter for global-scope (`:local`/`:hub`) cross-device unique
  collisions (keep-winner + surface-loser + compensate-forward). Not needed for the two
  pieces above.
- Composite / multi-slot uniqueness; `:hub` reserve-online — deferred, not blocking.
