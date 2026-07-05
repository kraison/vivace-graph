# Plan: MVCC Phase C — true snapshot-isolation scans via entry-level versioned indexes

## Status

DESIGN DRAFT for review. Builds on the completed MVCC work (P1–P5 + P4 "Option A"
lookup/typed-scan snapshot isolation, all on `experiment`). Supersedes the
"Option C (versioned indexes)" line item that P4 deferred. Do the work on a new
branch `mvcc-phase-c` off `experiment`; full SBCL suite per phase, then the
cross-impl matrix (SBCL/CCL/ECL on odm) + the two-process replication harness,
exactly as MVCC P1–P5 were validated.

## Background — what is and isn't already snapshot-consistent

MVCC today gives **value** consistency but not **membership** consistency on
scans:

- Node heads are append-only version chains keyed by `commit-epoch` /
  `prev-pointer`; `resolve-version-at-epoch` (`transactions.lisp:444`) returns
  the version visible to a reader at a given start epoch.
- `lookup-vertex` / `lookup-edge` (transactional branch of `lookup-object`,
  `transactions.lisp:200`) resolve at the reader's `start-tx-id`, gated by
  `*snapshot-reads-p*`.
- Therefore **lookups, typed scans, and adjacency traversal are value-correct**
  under a snapshot: every id they enumerate resolves to the right version.

The gap is *which ids get enumerated*. The enumeration backbone — `index-list`
(type / ve / vev indexes) and the `linear-hash` id→head table — is **live and
unversioned**. Per-id resolution fixes values but cannot reconstruct membership
as of an epoch:

1. The set of ids comes from the live index; only "created-after-E" is filtered
   (resolve returns NIL when the chain bottoms out). Any **physical removal** of
   an entry (compaction / future hard-delete) drops an id that existed at E —
   resolve can't recover an id it never enumerates.
2. The walk is not atomic over a concurrently mutating structure.
3. Secondary views / aggregates read live.

## The reframe that makes this tractable

Two properties of the existing code shrink "versioned indexes" from a rewrite to
a format bump:

1. **The value side is already MVCC** (above) — we only need *membership* as-of-E.
2. **The indexes are already append-only with soft-tombstone entries.**
   `index-list` entries are persistent cons cells (`pcons`). `remove-from-index-list`
   (`index-list.lisp:189`) does **not** unlink — it calls `mark-pcons-deleted`
   (`pcons.lisp:33`), flipping bit 0 of a flags byte. `%map-index-list`
   (`index-list.lisp:73`) skips deleted entries. So the live index is already a
   **monotonic superset of every historical membership**; nothing is physically
   removed in normal operation.

The *only* reason the live index can't serve a snapshot scan is that the tombstone
is a **single binary bit**: it can say "dead" but not "dead as of which epoch."
A reader at epoch E cannot tell whether an entry died before or after E.

**Phase C = promote that binary `deleted-p` bit to an epoch pair** — the same
append-only header bump we did for the node head v1→v2.

## Core mechanism — entry-level MVCC on the pcons header

Today a pcons is 25 bytes (`pcons.lisp`):

```
 off  0  car   (16)   the id (uuid)
     16  cdr    (8)   next entry (local heap address)
     24  flags  (1)   bit0 = deleted-p
```

Append two epoch fields (append-only, so a v1 reader shim reads the first 25
bytes and treats epochs as 0 / "unknown"):

```
 off  0  car          (16)
     16  cdr           (8)
     24  flags         (1)   bit0 deleted-p (retained for back-compat)
     25  birth-epoch   (8)   committing epoch when this entry was added       [*]
     33  dead-epoch    (8)   committing epoch when removed; 0 = still live
```

`[*]` birth-epoch is provisional — see the **C-spike** decision gate below.

Visibility predicate for a reader at epoch E (computed entirely from the entry,
**no node lookup**):

```
visible-as-of(E) := birth-epoch < E  AND  (dead-epoch = 0 OR dead-epoch >= E)
```

- **push** (`index-list` insert; called by `type-index-push`, `add-to-ve-index`,
  `add-to-vev-index`) stamps `birth-epoch = *commit-epoch*`.
- **remove** (`remove-from-index-list`) stamps `dead-epoch = *commit-epoch*`
  (in addition to the legacy bit).
- **`map-index-list` gains `:as-of EPOCH`** and applies the predicate. Typed
  scans (`map-vertices` typed branch), adjacency (`outgoing-edges` /
  `incoming-edges` / `traverse`), and the generated edge functors all route
  through `map-index-list`, so they become true SI scans once they pass the
  reader's epoch down.

### Why this also removes the cost objection

Membership filtering touches only the entry's epoch bytes, not the node, so a
scan that narrows 100k entries to 12 hits resolves 12 nodes, not 100k. Only the
entries actually returned get a node `resolve-version-at-epoch`. Combined with
**C-0** (read-set-free resolve), SI scans stop bloating the OCC read-set and stop
materializing every node — making them *cheaper and more correct* than the
"resolve every id in the untyped scan" approach we rejected.

## Replication constraint (EXPLICIT — and an open issue to resolve)

`birth-epoch` / `dead-epoch` are **LOCAL**, exactly like `prev-pointer` and the
node `commit-epoch` today. They are derived from the local committing epoch in
the **shared apply path** (`apply-transaction` / `apply-tx-write`), which runs on
master commit AND slave replay. They are **NEVER transmitted on the wire**; the
wire carries logical ops (id + serialized data + op kind). Each node builds its
own correct entry epochs the same way it already builds its own version chains.
The reaper runs independently per node.

### OPEN ISSUE (flagged for a near-term separate discussion)

Because epochs are **node-local counters**, an epoch value is **not portable
across replicas**: epoch `E` on the master is a different point in history than
epoch `E` on a slave. This is invisible to normal reads/scans (each replica
resolves against its own epochs) but it **breaks a replica-portable time-travel
API** (C-3): "give me the graph as-of E" means different things on different
nodes, and "as-of a wall-clock time T" has no local mapping.

Candidate directions to discuss before C-3 ships (NOT decided here):
- a **logical/wall-clock commit timestamp** stamped alongside the local epoch
  (replicated as data, used only for as-of *addressing*, never for ordering /
  reaper gating);
- a **replicated epoch map** (master epoch → slave epoch) maintained by the
  apply path;
- declare time-travel **node-local only** (an as-of query is meaningful only
  against the node you issue it to) and document it.

C-1 and C-2 do **not** depend on resolving this; only C-3's cross-replica
semantics do. Hold a design conversation before starting C-3.

## Scope / boundaries (LOCKED)

- **Untyped (all-types) `map-vertices` / `map-edges` stay LIVE.** They are not
  `index-list`-backed (they walk the `linear-hash` id→head table), and their only
  callers are admin/maintenance paths (backup, `check-data-integrity`, GC
  `map-all-nodes`, `rebuild-spatial-index`, `compact-*`) that want live state and
  should not pay a read-set/memory cost. No user use case for an untyped SI scan
  has been identified, so the boundary is locked: **true SI scans cover typed
  scans + adjacency** (the actual query surface).
- **Skip-list-backed views** (`views.lisp`) are a separate structure; versioning
  them is out of scope for Phase C (left as live aggregates, or a later phase).
- **OCC writes are unchanged** — still serializable-via-abort. Phase C is about
  read membership consistency, not write isolation.

## Migration (DECIDED: forced snapshot + replay)

The pcons header grows 25→41 bytes — a format break for every existing graph's
index cells. Existing graphs migrate via **logical snapshot + replay** (the same
mechanism as the head v2 migration and the 2016 UUID break): snapshot reads
through a v1 pcons shim (`*pcons-head-reader*` analogous to the head
`*node-head-reader*`), replay rebuilds index entries as v2 with epochs derived
locally. Bump the storage format version and gate `open-graph` with a clear
"migrate-me" error (precedent: `+storage-version+`, `:accept-versions`).

---

## Phases

### C-0 — Read-set-free resolve path (self-contained, ship first)

Independent of the format change and useful immediately (it also retires the
documented read-set caveat in `with-read-snapshot`).

1. Split `resolve-version-at-epoch` use into two callers: the OCC read-write path
   keeps `add-to-object-set` (read-set tracking for validation); a new read-only
   path resolves **without** registering the read-set. Gate on transaction kind
   (the read-only snapshot txn from `call-with-read-snapshot` vs a read-write
   `with-transaction`) or an explicit `*register-reads-p*` dynamic.
2. Wire the read-only path into `lookup-object` when the active transaction is a
   read snapshot, and into the typed-scan/adjacency `map-index-list` consumers.
3. Validate: `with-read-snapshot` over a large typed scan no longer grows the
   read-set (assert read-set count 0 / unregistered); OCC behavior on read-write
   txns unchanged (acid + concurrency suites green).

### C-spike — birth-epoch: store vs reconstruct (DECISION GATE, before C-1 format lock)

Kevin's concern: is the per-entry `birth-epoch` (8 bytes) worth it vs.
reconstructing birth from the node's own version chain?

- **Store (8 bytes/entry):** membership filtering is a pure header read; no node
  touch for filtered-out entries. Cost: +8 bytes/entry on disk + the migration.
- **Reconstruct (0 bytes):** birth is derivable — an id is "born before E" iff
  `resolve-version-at-epoch(node, E)` is non-NIL. Cost: a node head read per
  *enumerated* entry, even ones that filter out, partially defeating the
  membership-cheapness win.

Experiment (extend `graph-db/perf-test`, `tests/perf/`): a typed scan over a
large index where most entries are NOT visible at the reader epoch (heavy
churn / many post-snapshot inserts), measured both ways:
- variant A: birth-epoch stored, filter from header only;
- variant B: dead-epoch stored, birth reconstructed via a node-head read.

Compare scan throughput + heap-used watermark. **Decision rule:** store
birth-epoch unless variant B is within noise on throughput AND the 8 bytes are a
meaningful fraction of index size at target scale. `dead-epoch` is required
regardless (no chain-equivalent — the chain says when a node was deleted, but the
*index entry* removal is the membership event). Lock the C-1 header layout on the
spike result.

### C-1 — pcons epoch header + `:as-of` enumeration (the meat)

1. **pcons format v2**: append `dead-epoch` (and `birth-epoch` per C-spike) after
   the flags byte; serialize/deserialize-pcons codecs; +pcons-header-size+
   25→33 or 41. v1 reader shim (`*pcons-head-reader*`) for migration.
2. **stamp on write**: `index-list` push stamps birth; `remove-from-index-list`
   stamps dead (`= *commit-epoch*`, already bound in the shared apply path).
   Keep the legacy `deleted-p` bit in sync (v1 readers + back-compat).
3. **`:as-of` enumeration**: `map-index-list` / `%map-index-list` gain `:as-of`
   applying `visible-as-of(E)`. Default (no `:as-of`) = current live behavior.
4. **route the reader epoch**: typed `map-vertices`/`map-edges`, `outgoing-edges`/
   `incoming-edges`, `traverse`, and the generated `<edge>/2,3` + `is-a/2`
   functors pass the active transaction's `start-tx-id` as `:as-of` when
   `*snapshot-reads-p*` and a snapshot txn is active; otherwise live.
5. **migration**: storage-version bump + snapshot/replay path + `open-graph`
   migrate-me error.
6. Validate: new mvcc tests — a typed scan + an adjacency traversal under a
   pinned snapshot do NOT see entries created after the snapshot AND DO see
   entries deleted after the snapshot (true SI membership, both directions);
   full suite + cross-impl + replication harness green.

### C-2 — Epoch-gated reclamation + safe compaction

1. **Reaper extension**: physically free pcons cells whose `dead-epoch <
   reap-safe-floor` (`transactions.lisp:496`, the SAME floor the node reaper
   uses). Walk per-key index-lists during the existing lazy-at-commit reap;
   unlink + free reclaimable suffixes/cells, repairing `cdr` pointers in place
   (mirror `reap-node-chain`'s pointer repair).
2. **`gc.lisp` awareness**: ensure `open-graph`'s heap GC roots live AND
   reader-retained index cells (mirror `map-version-chain-addresses` for the
   node chains) so retained-but-dead cells aren't reclaimed as garbage on reopen.
3. **Make `compact-*` real and safe**: rewrite `compact-vertices` / `compact-edges`
   (currently fixed-but-dead, `vertex.lisp` / `edge.lisp`) to stamp `dead-epoch`
   via `remove-from-index-list` (already the path) — reclamation then waits for
   the floor, so compaction can NEVER yank an entry a live reader's snapshot
   needs. Export them once safe.
4. Validate: a long-held read pin keeps dead index entries from being reclaimed
   (mirror `read-pin-retains-versions-until-released`); after release the reaper
   collapses to steady state; compaction under concurrent readers is RAF-clean
   (full-system-storm-style test that deletes + compacts + scans).

### C-3 — Time-travel / history API (rides the same surface)

Once entries carry epochs and nodes carry chains, "as-of" is mostly surface:

1. `lookup-vertex` / `lookup-edge` `:as-of EPOCH`; `map-vertices` / `map-edges`
   `:as-of EPOCH`; `node-history` (newest→oldest up to keep-revisions); a
   `with-as-of` macro pinning a graph epoch for an extent.
2. `select` `:as-of` option (parallels the existing `:snapshot t`), wrapping the
   query in an as-of read snapshot.
3. **BLOCKED ON the replication open issue above** for any cross-replica
   semantics. A node-local as-of API can ship without resolving it (document the
   limitation); a replica-portable / wall-clock as-of cannot.
4. Validate: as-of reads reconstruct historical membership + values within the
   retained window; beyond `keep-revisions` the API reports "reaped" rather than
   lying.

---

## Affected files (by phase)

- C-0: `transactions.lisp` (resolve split, read-set gating).
- C-1: `pcons.lisp` (header v2 + codecs + shim), `index-list.lisp` (push/remove
  stamp, `:as-of` map), `type-index.lisp` / `ve-index.lisp` / `vev-index.lisp`
  (epoch passthrough), `vertex.lisp` / `edge.lisp` (typed scan + adjacency
  `:as-of`), `prolog-functors.lisp` + `schema.lisp` (generated functors),
  `backup.lisp` (snapshot/replay migration), `globals.lisp` (format version /
  header size constants).
- C-2: `transactions.lisp` (reaper), `gc.lisp` (rooting), `vertex.lisp` /
  `edge.lisp` (`compact-*`), `package.lisp` (export compaction).
- C-3: `interface.lisp` / `traverse.lisp` (as-of API), `prologc.lisp` (`select
  :as-of`), `package.lisp` (exports), docs.

## Validation each phase

unit + acid + concurrency + stress + concurrent-stress on SBCL, then the
cross-impl matrix (SBCL/CCL/ECL on odm) + the two-process replication harness
(`tests/replication/`). full-system-storm must stay clean. Show the full diff
(fenced ```diff block) before each commit. Document migration in README
(precedent: the head v2 + 2016 UUID notes).

## Open questions / decisions

1. **Migration** — DECIDED: forced snapshot+replay, storage-version bump,
   migrate-me error. (Kevin, 2026-06-21.)
2. **Replication / epoch portability** — OPEN, flagged above; hold a design
   conversation before C-3.
3. **Untyped-lhash SI** — DECIDED: out of scope, boundary locked. (Kevin.)
4. **birth-epoch store vs reconstruct** — DECIDED via the C-spike decision gate
   (measure before locking the C-1 header). (Kevin.)
