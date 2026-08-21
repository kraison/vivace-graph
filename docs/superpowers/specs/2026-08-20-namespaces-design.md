# Namespaces: stores, packages, and one clock — design

**Status:** agreed 2026-08-20, not built. Supersedes the two-axis framing and
agreed-shape point 3 of `docs/namespace-design-discussion.md` (which carries a banner
pointing here). Tracked by GH #110.

**Decision log:** `docs/namespace-design-decisions-2026-08-20.md` records each decision
D1–D13 with the reasoning and the alternatives rejected. This document is the
implementable form of the same content, organised by subsystem. Where an implementer
wants to know *why*, the log is the answer; where they want to know *what*, this is.

## 1. Goal

"Multiple graphs" conflates three orthogonal things. Separate them:

| Axis | Today | Decision |
|---|---|---|
| Physical file partitioning | a graph | **keep** — becomes a *store* |
| Transactional and schema isolation | a graph | **drop** — one clock, one type-id space |
| Ontological scoping | a graph (by global class-name ban) | **keep** — becomes a *package* |

The parked design named only the first two and folded the third into the axis it
discarded. Recovering it as its own axis, with its own mechanism, is what this design adds.

## 2. The model

**A store** is a set of mmap'd files — heap, vertex and edge tables, indexes, txn-log.
It owns the transaction manager, the WAL, and its own durability. It is the unit of
snapshot, backup, recovery policy, and detach/attach.

**A namespace** is a Lisp package: a set of symbols. It is the unit of naming, ontological
scoping, and runtime schema definition. It has **no transactional identity whatsoever**.

**Expected shape: many small namespaces, few stores.** Stores are chosen for write cadence,
recovery policy, and detachability. Namespaces are chosen for meaning.

Two consequences implementers must internalise:

- Two namespaces in the same store are **already in one transaction domain at one clock**.
  `cross-graph-transaction-error` fires across *stores*, and stores only. Co-locating things
  that share an invariant is how you give them transactional consistency.
- A single namespace **may span stores**. A package's authored nodes can live in a durable
  store and its derived nodes in a disposable one, without fracturing the ontology.

## 3. Naming and schema

### 3.1 Ontological separation is expressed, not enforced

The engine does not refuse a cross-namespace reference; it makes one lexically obvious.
`fish::species` and `primate::species` are distinct classes that legitimately share a
symbol-name.

Packages **express**; they do not enforce. `::` bypasses a package and nothing stops a node
holding a foreign id. Semantic enforcement, where wanted, is the ontology subsystem (#109
and its units) and is **opt-in**.

### 3.2 No syntax change

`def-node-type` already records `:package (package-name *package*)` into the persisted
`node-type` meta. The package therefore comes from the ambient `in-package`, and the
existing trailing argument keeps its position and becomes the class's **default store**:

```lisp
(in-package :fish)
(def-vertex species () ((latin-name :type string)) :taxonomy)
```

Existing single-package/single-graph code is unchanged and *is* the one-to-one case.
Decoupling is additive: obtained by writing a second `in-package`, never by accident.

### 3.3 A class is instantiable in any store

Placement is a property of the node, not the class.

**`%check-node-class-graph-unique` is deleted, not scoped.** It exists only because
type-ids were per-graph, which is what made a class-name collision dangerous (#53). With a
global type-id space, a type-id denotes the same class in every store and the check has no
remaining job. The residual hazard — reading a node under the wrong store and dereferencing
the wrong heap offset — is a different bug, already closed by the 3.0 contract having nodes
carry their home graph.

`*schema-node-metadata*` stops being keyed by graph-name. The registry becomes global; each
store instantiates the types it holds.

### 3.4 The global type registry

One type-id space across the image, keyed by package-qualified symbol — structurally
unique, unlike today's enforced-by-convention global class name.

**`type-id` widens from 16 to 32 bits.** Rejected: 24 (bit-packing to save ~1 MB) and 64
(doubles `ve-key` for nothing). 32 aligns and matches `revision`.

**Consequence — the type-index must become sparse.** `make-type-index` allocates densely as
`(* +max-node-types+ +index-list-bytes+)`: ~1.1 MB at 16 bits, ~73 GB at 32. It becomes a
keyed structure. This also retires the `(make-array +max-node-types+)` of **65,536 mutexes**
allocated per index per store today regardless of type count.

**As built (#166), this is a growable array rather than a keyed structure.** It starts at
4,096 types and doubles in place via `extend-mapped-file`, which never relocates the base
address, so a concurrent reader is never left holding a moved pointer. That satisfies the
requirement above — nothing is allocated for type-ids not in use — by a different mechanism
than the text anticipated. Two consequences the design did not state: growth is bounded by
the 1 GiB mmap reservation, so the first type-id at or above 2²⁵ signals rather than growing
(loud, and unreachable while ids are per-graph); and locking became **256 fixed stripes** by
`(mod type-id 256)`, so two type-ids sharing a stripe now serialize against each other.

**The consequence analysis above missed one narrowing, and the omission is instructive.** It
enumerated the type-index and stopped at structures whose *size* scales with
`+max-node-types+`. It did not ask the different question — *where else is a type-id
serialised at a fixed width?* — which would have found `memory-graph.lisp`'s VG-native image
packing it at 2 bytes with no range check (#187, fixed: format v8, readers fork on version,
`ni-type-id` signals rather than truncating). Unit 1b should ask that second question
explicitly of every remaining wire and file format before assuming this class is closed.

**Known residual:** widening moves the hard ceiling out of reach and leaves a soft one at
CLOS class count — 100k runtime types means 100k finalized classes with interned accessors.
If genuinely high-cardinality runtime types are ever wanted, the alternative (types sharing
a type-id and discriminating on a slot) returns. This buys time, not infinity.

**Known defect to fix in passing:** `update-node-type` registers a third key per type as
`(intern (symbol-name (node-type-name meta)) :keyword)`. That alias is package-blind, so
`:SPECIES` from two packages collides in a shared type table.

**The registry is persisted and distributed, not recomputed (D14).** Today's determinism is
free: `*schema-node-metadata*` is keyed per graph and populated in source order, so two hosts
loading the same `schema.lisp` assign identical ids and replication can ship a raw `uint16`
with no name crossing the wire. A counter spanning graphs destroys that — assignment becomes
dependent on which stores an image opens and in what order, so a hub and a device holding
different subsets diverge and a node materialises as the wrong class on the receiver.

So the global registry is an **append-only, image-level object in the system directory**,
beside the clock and lifecycle journal of §6 and §9.1. Hosts read it; none recompute it. New
assignments are made by the hub, distributed through the type-table the hub **already** ships
in its auth-ok plist — today a Kotlin/SQLite accommodation, since those peers cannot evaluate
`schema.lisp`. Under a global space a Lisp device cannot rely on evaluation either, so that
table becomes the normal path rather than a special case.

**A replication handshake refuses a peer whose registry disagrees (D15)**, naming the
conflicting symbols. An image with no hub is its own authority — the common case, since peer
replication is off by default — so two such images can independently assign the same symbol
different ids. Silent reconciliation would mean a data migration triggered by a network
handshake; a disagreement between two populated stores is an operator event.

### 3.5 Runtime schema is metadata, never source

Restart must never `load` code written at runtime. Metadata is diffable, versionable,
migratable and safe; source is an arbitrary-code-execution surface where a data surface
suffices, and it has a bootstrap ordering problem (the package must exist before the file
defining it can be read).

**The ingredients are already persisted and nothing consumes them.** `node-type` carries
`name`, `parent-type`, `slots`, `package` and `constructor` — everything a `defclass` needs.
But `instantiate-node-type` calls `(find-class (node-type-name meta) nil)` and assumes the
class exists, because `def-node-type` expanded to a `defclass` at load time. **The engine
cannot currently rebuild a class from disk.** A class-from-metadata path is required.

**Boundary: a runtime-defined type may declare structure; it may not ship behaviour.**
Slots, types, indexes, unique constraints and value constraints are data. Function-valued
schema options are not — a closure cannot be serialised. A runtime-defined type that needs
a function **names a pre-registered one**. Invariant: *restart never evaluates data.*

## 4. Placement

The class's declared store is a **default**, overridable at any individual write.

Rejected: explicit-at-every-write (omit it and you silently get `*graph*`); default per
namespace; and a placement *rule* function — placement determines recovery policy, so a bug
in a placement rule is a bug that quietly loses data at the next unattended rebuild.
Placement stays visible at the call site.

**An edge is a node and is placed the same way** — not by its `from` store, not by its `to`
store, but by its own class default. This answers "whose store holds a cross-store edge"
with no special case, and places the relation by *policy*: a derived claim defaults to a
disposable store while an authored assertion defaults to a durable one.

## 5. Identity and resolution

### 5.1 Node ids are already global

Ids are v5 UUIDs over time plus 32 random bytes. **A node id already identifies a node
globally**; the store is where you *find* it, not part of what it is. Cross-store edge
endpoints therefore never needed widening — only a resolver. This retires the "one-way
door" framing in `namespace-design-handoff.md`.

### 5.2 A v5 namespace cannot serve as the resolver

The v5 namespace is a hash *input* (`SHA1(namespace || name)`), not a recoverable field.
Recompute-and-compare is also unavailable: `generate-uuid-name` builds the name from
time-of-day plus random bytes and **that name is never stored**. Per-store v5 namespaces
would buy separation at generation time and nothing for resolution.

### 5.3 The resolver is a tagged UUIDv8

RFC 9562 defines **v8 for vendor-defined layouts** — everything but version and variant is
ours. New ids are v8 carrying a store field plus random fill; existing ids stay v5.

**The resolver reads the version nibble: 8 → mask and index the open-store vector, O(1);
5 → fall back to the per-store scan.** No flag day, no rewrite of existing data, and the
fast path widens as data turns over.

v8 rather than stealing bits from a v5 in place: an id that claims to be v5 and is not will
bite someone later, and nothing in the engine can detect it (there is no stored name to
validate against).

**Field width:** stores stay few, so 8–12 bits suffices and preserves entropy. Namespaces
may proliferate into the hundreds and **never touch the id**.

**Consequences.** The tag is a *stable numeric store-id* from a registry, never reused, so
renaming a store is free. **A node's store is immutable for life** — the id is the key in
the vertex table, both ve-indexes, the vev-index and the type-index. "Move a node to another
store" is copy-and-delete with a new id. Re-homing a *class* does not re-home existing
*nodes*.

### 5.4 Inbound and outbound cross-store lookup

Indexes live in stores, and stores stay few, so an unhinted sweep is a handful of lookups.
The edge class's default store is the hint the parked design wanted; since a write may
override placement, the exact form is a small per-edge-class **store-occupancy set**
maintained on write. It fails safe — a lost or stale set costs a wasted lookup, never a
wrong answer, and the fallback is the full sweep.

## 6. Time

**Status: implemented (#168).** `system-clock.lisp` (clock, leases,
journal), `attach-to-system-clock` (watermark), `peer-observe-epoch`
(foreign epochs), `recreate-graph` (restore no longer bypasses it), and
the cross-store pin in `call-with-read-snapshot` cover every bullet
below, for explicitly-nested `with-read-snapshot` calls -- `*read-
snapshots*` is keyed by graph, so a read that walks into another store
without its own nested snapshot gets only the momentary pin inside
`lookup-object`, not an extent-length one. User-facing docs:
`docs/vivace-graph-v3-doc.org`, Chapter 17,
starting at "The image-level system clock (optional)". The bullets below
are kept as the original design record, including the audit finding.

One image-level clock (#94), adopted now rather than deferred: co-locating things that share
invariants makes cross-store skew rarer, not impossible, and the failure mode is silently
wrong derived data whose provenance does not record the skew.

- **The clock is image-level, not store-level.** A *system* — a directory of N stores —
  owns one clock. Opening any subset opens the system, so a store opened alone still
  allocates from the shared counter. A store's WAL remains self-contained. **This holds
  only while one process owns the directory, which #168 did not enforce** — two images both
  read the ceiling, both reserved, and both issued, silently. Closed by #182: an advisory
  `flock` held for the clock's lifetime, kernel-released on process death, refusing
  immediately and by name. Scope there is exclusion only; the counter needed no recovery
  path, because the ceiling is persisted a block ahead of issuance.
- **Detach takes an epoch lease.** A detaching store is granted `[E1, E2)` and allocates
  inside it while offline; the global clock skips past `E2`. One handshake at detach, one at
  reattach, no coordination in between — which is what keeps a separate-process bulk load
  working. Transaction ids are 64-bit (`load-highest-transaction-id` reads 8 bytes via
  `deserialize-uint64`), so a generous lease makes exhaustion unreachable.
- **Existing data migrates by watermark, not rewrite.** Start the global clock above the
  maximum of every store's counter. Below the watermark epochs are not cross-store
  comparable; above it they are. This degrades rather than breaks: an old version stays
  visible at any snapshot above the watermark, and the relative order of two pre-migration
  epochs in different stores is meaningless *but unobservable*, since no snapshot can be
  taken between them. Stated limitation: you cannot snapshot into the pre-migration past
  across stores. **No record is rewritten.**
- **Cost:** a cross-store `with-read-snapshot` must register its read pin with every
  participating store, so a long cross-store query delays reaping in each store it touched.
  Per-store reaping must fold in foreign pins.
- **Epoch-density audit: complete, clean** (2026-08-20, recorded on #168). No code assumes
  contiguous ids. Consumers either take a max/min over an actual collection, filter an
  existing collection by comparison, or use `(1+ x)` as an exclusive *lower bound*.
  `replication-log-ranges` derives log N's end from log N+1's start, which looked dangerous
  and is not: it over-approximates, and monotonicity still puts log N's ids in
  `[start_N, start_{N+1})`.
- **What the audit did find: `recreate-graph` is a second id allocator that bypasses the
  transaction manager.** `transaction-restore.lisp:133-152` mints ids by `(incf tx-id)` from
  `(load-highest-transaction-id graph)` — a *per-store* scalar — and persists a per-store
  high-water mark. Under a global clock it would allocate epochs **below** the global
  counter, putting distinct events in different stores at the same epoch: precisely what
  this section exists to prevent, arriving through the back door. **Re-pointing it at the
  global clock is in scope for the unit.** This is load-bearing, not incidental — logical
  replay is the proven per-store recovery path and §9 depends on restore semantics.
  **Done** — `recreate-graph` now allocates through `tm-next-epoch`, same
  as every other path.
- **Stated property, not a surprise: the global clock is not purely local.**
  `peer-observe-epoch` advances the counter to strictly exceed a *foreign* epoch carried by
  a pulled node — another image's clock. Under a per-graph counter that was local
  bookkeeping; under an image-level clock a peer sync can advance the whole image's clock.
  Mechanically sound (Lamport-style max, 64-bit ids, leases allocated ahead of the current
  clock), but it must be documented rather than discovered.
- **Three counters exist; one is in scope.** The transaction id (global, this section); the
  per-graph `lamport` counter for peer conflict resolution, which orders events across
  *devices* — separate images — and **stays per-graph**; and the per-node 32-bit `revision`,
  untouched. Stated so nobody globalises the lamport counter by analogy.

**Sequencing:** the debt is monotonic. Every write before the migration lands below the
watermark permanently. This does not block other units, but its cost accrues daily.

## 7. Edges across stores

Cross-store edges are **ordinary edges** — no declaration mechanism, no permission list, no
checked exception. The closure rule of agreed-shape point 3 demotes from engine invariant to
convention.

**`backup` includes a dangling cross-store edge and warns.** Rejected: omitting it (loses
connectivity with no signal) and refusing (turns a routine backup into an operator event
over a condition the design explicitly permits).

**A read reaching an edge into a detached store returns an unresolved marker carrying the
store id.** Rejected: signalling (makes every traversal an error site, and will be
caught-and-ignored within a month) and silent skipping (a subgraph that looks complete and
is not). The tagged id is what makes the marker possible: the scan resolver could only ever
report "not found anywhere" and could not distinguish an absent store from a missing node.

**Explicit versus incidental access.** A call naming a detached store **signals**
(`store-detached-error`) — the caller asked for something specific. Incidental traversal
into one yields the marker — that caller merely walked there.

## 8. Bulk load and detach

**Detach is a quiescence protocol over the existing pin machinery:** refuse new pins and
transactions on the store, drain the in-flight ones, close, hand over. The hazard is not
concurrency in the abstract — the server holds live node objects, buffer-pool pages,
spatial-index handles and cache entries into the store's mmap, and **a stale node
dereferenced after close is a segfault, not a condition**. `pin-read-epoch` /
`reap-safe-floor` already exist to prove no reader is mid-flight.

**Bulk load builds a shadow generation and swaps it in.** The loader writes new files while
readers continue on the current generation at the current epoch; reattach is an atomic swap
plus a brief quiesce. The store is unavailable for the swap, not for the load. A crashed
load stops being a recovery event — discard the shadow; the live generation was never
touched. Copy-first is not a real cost: a multi-gigabyte store copies in seconds against a
load measured in tens of minutes, so this beats any copy-on-write scheme.

**Recovery policy licenses the fast path.** Non-transactional bulk apply — direct heap and
index writes, no WAL, no MVCC versions — is available exactly to stores whose policy is
*derivable*, because a crash mid-load is repaired by redoing the load. Authored stores load
transactionally.

**The vector-segment capacity hazard improves.** A capacity failure inside APPLY leaves a
persisted node with no segment entry — invisible to retrieval, with `store-count` still
correct. A bulk load knows N upfront and **presizes the segment**, turning a mid-apply
failure into an upfront allocation.

**In-process detach becomes viable for the first time.** The separate-process pattern
existed to stop a live server holding the store. The epoch lease still accommodates
out-of-process later without redesign.

## 9. Restore

### 9.1 The system journal

The image-level clock object of §6 doubles as a small append-only **system journal** of
store lifecycle events: create, detach at `E1` with lease `[E1,E2)`, swap at `E3`, attach,
retire. No new file — §6 requires the object regardless.

### 9.2 Retention is keyed on recovery policy

- **Authored store — mandatory.** The pre-swap generation is retained for at least the
  system restore window under the normal retention policy; a swap refuses to discard it
  while the window still covers it.
- **Derivable store — optional.** A restore predating the swap may rebuild from source.

### 9.3 Whole-system restore to T

1. Read the journal; find every swap with `E3 > T`.
2. For each store so affected, the current generation is post-swap and must be replaced.
3. Retained generation present → use it; rewind physically to T.
4. Absent and store derivable → rebuild, **mark rebuilt-not-rewound**, and **cascade**.
5. Absent and store authored → **refuse.** Authored data is never silently approximated.
6. Emit a **manifest**: per store, rewound-to-T or rebuilt-at-now, plus any cascade.

**Rebuild is not equivalent to restore.** Restoring a retained generation preserves node ids
(`backup` preserves `:id` and `:revision`); rebuilding from source mints fresh UUIDs, so
every cross-store reference *into* the rebuilt store dangles. The store-id tag survives (same
store, new generation); the node ids do not. Hence the cascade in step 4: dependents are
invalidated and repaired by regeneration. This is agreed-shape point 4 working as designed,
and it is why point 5 keys authored cross-boundary assertions on **external identity** rather
than node id. Anything holding a cross-store reference must be either regenerable or
externally keyed; no third option survives a rebuild.

**Step 6 is load-bearing.** A restore that silently mixes rewound and rebuilt stores produces
exactly the inconsistent instant §6 exists to prevent, with nothing in the data recording it.

**T inside a detach window** (`E1 <= T < E2`) resolves cleanly: the old generation was frozen
at its last committed epoch `E0` and every reader saw `E0` throughout, so the restore yields
that generation rewound to `E0`, noted in the manifest.

**Rejected: forbidding restore across a swap.** Fail-closed and trivial, but it silently
truncates the restore window whenever any store is bulk-loaded. The restore window is a
system property; one store's routine maintenance must not shorten it.

## 10. Migration

Three migrations, each chosen for a reversible failure mode.

| What | Mechanism | Rewrites data? | Status |
|---|---|---|---|
| 32-bit `type-id` | new head-codec version; `migrate-graph` reads the old and replays | logical replay | **Implemented** (#166, unit 1a) |
| Global epoch | watermark above every store's counter | no | not built |
| Tagged store id | v8 for new ids, v5 fallback for old | no | not built |

The type-id widening is the only change to the **graph's** on-disk format in this design.
It is no longer the only durable-format change: #187 bumped the memory-graph native image
(`VGMI`) to v8 for the same widening, reading v5-v8 so no image is orphaned. That was not
foreseen here — see §3.4. The widening has a proven precedent: `*node-head-reader*` is already a dispatch variable and
`deserialize-node-head-v1` already reads the pre-MVCC 15-byte head so `migrate-graph` can
back up and replay a v1 graph. Widening adds a v3 reader the same way. The head goes 31 → 33
bytes (flags 1, type-id 2→**4**, revision 4, data-pointer 8, commit-epoch 8, prev-pointer 8)
and `ve-key` goes 18 → 20; both are rebuilt by the replay.

**Implemented as of #166, unit 1a** (widen `type-id` to 32 bits, sparse type-index, v3
head codec, migration — ids stay per-graph): the head is 33 bytes and `ve-key`/`vev-key`
are 20/36 bytes exactly as designed above, storage format is `+storage-version+` `#x03`,
and `migrate-graph` carries a v1 *or* v2 source to v3 through one version-detecting path
(it reads the source's own stamped version rather than being told). This row is done; the
other two rows in this table, and §3.4's global type registry, are not — those are unit
1b (#186) and later units, unaffected by this change.

**Deployment gate.** A type-id migration lands on hosts that may be behind the current
engine. Every consumer's version floor and each deployed engine version must be checked
before this ships. That is a release gate, not a design gate.

## 11. Units

| # | Unit | Issue | Depends on | Status |
|---|---|---|---|---|
| 1a | Widen `type-id` to 32 bits, sparse type-index, v3 head codec, migration — **ids stay per-graph** | #166 | — | **Done** |
| 1b | Global type-ids: canonical registry (D14), distribution, handshake guard (D15), delete the uniqueness check | #186 | 1a | In progress |
| 2 | Packages as namespaces; store/namespace decoupling; placement defaults | #167 | **1b**, #190 | Blocked |
| 3 | Image-level clock, system journal, epoch leases | #168 | — | **Done** |
| 4 | Tagged UUIDv8 store field, resolver, detached-read marker | #169 | 3 | Not started |
| 5 | Detach quiescence protocol and shadow bulk load | #170 | 3, 4, #191 | Blocked |
| 6 | Restore: retention policy, algorithm, manifest, cascade | #171 | 5, #191 | Blocked |
| 7 | Runtime schema from persisted metadata | #172 | 1b, 2 | Blocked |

Defects found while building the units above, which gate later ones: #187 (memory-image
narrowing, **done**), #182 (clock had no cross-process exclusion, **done**), #190
(package-blind type alias, gates unit 2), #191 (a torn record makes the whole lifecycle
journal unreadable, gates units 5 and 6), #193 (whether `flock` should replace `.dirty`
generally — exploration, not scheduled).

Units 1a and 3 have no dependencies and may start in parallel. Unit 1 was split:
1a is the only on-disk format change in this design, and a migration failure should be
unambiguous rather than tangled with a semantic one. 1a is verifiable on its own terms —
same data, same behaviour, wider field. Unit 3 should start early
because its migration debt accrues daily (§6).

## 12. Acceptance

- Per-store snapshot cost tracks that store's write cadence, not the whole image's.
- Differential recovery policy is preserved: derivable stores may rebuild unattended,
  authored ones must refuse.
- A cross-store query resolves at one instant, and any restore that could not provide one
  says so in its manifest.
- A class may be instantiated in more than one store (closes cl-llm#20).
- A namespace may be created at runtime without creating a store, allocating files, or
  producing a new operational object.
- Restart never evaluates persisted schema as code.
- A traversal into a detached store returns a marker naming the store, never a wrong answer
  and never a segfault.
- An existing deployment migrates without any record being rewritten except by logical
  replay.

## 13. Open items and required audits

- Whether the shadow swap supersedes logical replay as the per-store restore mechanism. The
  trade is real: logical replay needs no journal and no retention but is not atomic. Since
  unit 5 requires the journal anyway, the marginal cost of consolidating is zero.
- **Identity-slot indexing audit** — external-key slots used for cross-store assertion
  resolution must be `:unique` or at least `:index t`, else assertions resolve by scan.
  Largely answered by the multi-slot index work (#107), which made these keys composite.
- Naming: v2 had a `namespaces.lisp` for RDF namespaces; the term is reused here differently.

## 14. Traceability

| Decision | Section |
|---|---|
| D1 expressed not enforced | 3.1, 7 |
| D2 closure rule is convention | 7 |
| D3 store/namespace decoupled | 2, 3.2 |
| D4 class instantiable in any store | 3.3 |
| D5 tagged UUIDv8 resolver | 5 |
| D6 runtime schema as metadata | 3.5 |
| D7 placement default per class | 4, 5.4 |
| D8 detached-read marker | 7 |
| D9 global epoch | 6 |
| D10 backup includes-and-warns | 7 |
| D11 32-bit type-id | 3.4, 10 |
| D12 shadow bulk load | 8 |
| D13 restore interaction | 9 |
| D14 registry persisted and distributed | 3.4 |
| D15 handshake refuses on registry disagreement | 3.4 |
