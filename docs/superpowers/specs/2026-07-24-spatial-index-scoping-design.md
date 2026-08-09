# Spatial index: per-class scoping, per-index precision, and a bounded insert cover

**Date:** 2026-07-24
**Branch:** `experiment`
**Status:** IMPLEMENTED on `experiment` (8-task SDD run, commits `f77ed21`..`1396b2a`, unpushed/untagged as of 2026-07-24); awaiting the 3.0.0 release cut. Plan: `docs/superpowers/plans/2026-07-24-spatial-index-scoping.md`
**Responds to:** `mine-action/docs/superpowers/specs/2026-07-24-vg-spatial-index-change-request.md`
**Release impact:** **MAJOR** — breaking public API plus an on-disk format bump (3.0.0)

---

## 1. Summary

The graph's single spatial index becomes a **registry of per-`(owner-class . slot)` indexes**,
mirroring `secondary-indexes`, `unique-indexes` and `vector-segments`. Spatial queries gain a
**required scope** argument. Geohash precision becomes **per index**. The insert-side cell cover
gains a **bounded, persisted cap** and a **self-healing query-side clamp** that keeps the bound from
silently losing nodes. A **value-based warning** reports node classes whose second indexed geometry
slot is inert.

Deferred to a follow-on (§13): indexing a node under *every* indexed geometry slot (the change
request's CR-3.2), and multi-resolution query probing.

---

## 2. What was verified

Every claim in the change request was checked against engine source. All three are accurate.

| Claim | Verdict | Evidence |
| --- | --- | --- |
| CR-1: no way to scope a spatial index below the graph | **True** | `graph-class.lisp:34` is one slot; `node-geometry` matches on value, not name (`transactions.lisp:964`) |
| CR-2: insert-side cover has no size cap | **True** | `spatial-index.lisp:90` passes storage precision straight to `geohash-covering`, bypassing its `max-cells` default |
| CR-3: a node is indexed by one geometry slot; others go inert | **True** | the `return` at `transactions.lisp:971` |

### 2.1 Three corrections

**(a) The fix requested for CR-2 would silently lose nodes.** Geohash prefix nesting is
one-directional. A query does a *prefix range scan* `[cell, cell+"{")` per covering cell
(`spatial-index.lisp:148`). This is correct today only because every stored key sits at the index's
storage precision while `cover-prec` is `min(storage-prec, …)` — so stored keys are always at least
as fine as the covering cell.

Coarsening the insert inverts that. Store a polygon as `"u8k"` (precision 3); query a small box
inside it, get `cover-prec` 7 and covering cell `"u8kabcd"`, range `["u8kabcd", "u8kabcd{")`.
`"u8k"` sorts *before* the range start and the node is invisible. The change request's own test
property 3 — "the cap must not lose the node" — is exactly what the naive fix fails. §7 fixes this
with a persisted, self-healing clamp on the query's covering precision.

**(b) CR-3.1 cannot be implemented as specified.** It asks for a finalization-time warning on
classes declaring more than one `:index` slot "whose type is geometry." The engine deliberately
refuses to compare the declared `:type geometry` symbol — `transactions.lisp:897-902` explains that
the symbol is read in the application's package and is not reliably `eq` to `graph-db:geometry`. At
finalization we cannot know which slots will hold geometry values. §8 gives a value-based
equivalent that is strictly more accurate.

**(c) CR-3.2 is substantially larger than described.** `node-geometry` is not only the insert hook;
it is the **refine predicate** at `spatial-query.lisp:52`, `:67`, `:80`, the replication filter at
`:169`, the memory-graph rebuild at `memory-graph.lisp:616`, and peer purge at
`peer-streaming.lisp:1034`. Indexing a site under its extent without changing the refine path would
still drop it, because refine tests only the centroid; and purge would leak index entries. It needs
a plural `node-geometries` protocol through all six sites plus a ruling on what distance means in
`find-nodes-near` for a multi-geometry node. Deferred to §13.

### 2.2 Two places the request is pessimistic

**The asymmetry is worse than reported, and it lives inside `:index` itself.** `index.lisp:153-160`
documents the split explicitly: the same `:index t` gives a *scalar* slot a per-`(owner . slot)`
ordered index via `%indexed-slot-owner-name`, and gives a *geometry* slot one graph-wide spatial
index — dispatched at runtime on value type. One slot option, two scoping rules, decided by what
you store in it.

**But that also means per-class spatial indexes are assembly, not invention.** The ownership scheme,
the `(owner . slot)` registry, the sidecar-of-roots, restore-or-rebuild at open, and `regenerate-*`
all exist three times over. Spatial becomes the fourth mirror.

### 2.3 One thing that already works

CR-1's option 2 ("an opt-out") already exists: **omit `:index`**. A geometry value is excluded from
the ordered secondary index by `%indexable-value-p`, so `:index t` on a geometry slot does nothing
*but* spatial indexing. mine-action already uses this idiom at `schema.lisp:273` and
`forensics-schema.lisp:110`. It does not unblock them, because their design §4.1 needs the zones
spatially *queryable*, just not mixed with ACLED — which is CR-1 option 1.

---

## 3. Decisions locked

| Decision | Choice |
| --- | --- |
| Scoping scheme | Per-declaring-class, keyed `(owner-class . slot-name)` |
| Query default | **Explicit scope required**; unscoped signals |
| Scope values | class symbol, list of class symbols, or `:all` |
| API shape | Required **positional** first argument |
| Insert slot choice | **Per-node** (preserves today's semantics) |
| Precision declaration | The `:spatial-precision` slot option only — no macro (§5.1) |
| Precision precedence | slot option > graph default; one surface, so no conflict exists |
| Index creation | Lazy, on first geometry-valued insert — unconditionally (§4.1) |
| Insert-cover clamp | Per-precision histogram, so degradation self-heals when the oversized node leaves |
| Migration | Automatic on format mismatch; index-only re-derivation |

Keying on `(owner . slot)` rather than owner alone is deliberate. Today a class has at most one
*effective* geometry, so the slot component is redundant. Under the deferred CR-3.2 it becomes
essential. Paying for it now makes that follow-on a maintenance-path change with the on-disk format
already correct.

---

## 4. Registry, ownership, and on-disk format

**Graph slot.** `(spatial-index …)` on `graph` becomes `spatial-indexes` — a synchronized hash
table keyed `(owner-name . slot-name)`, a direct mirror of `secondary-indexes` and
`vector-segments`. The `spatial-index` accessor is **removed**, not deprecated: there is no longer a
single index for it to name. `spatial-index-for` (graph, owner, slot) replaces it.

The per-index `spatial-index` struct (`spatial-index.lisp:20`) stays; it already carries its own
`precision`, and §7 adds two fields.

**Ownership** is resolved per geometry slot by reusing `%indexed-slot-owner-name` (`index.lisp:42`)
verbatim — the most general `node-class` in the precedence list declaring that slot `:index`.

> **Consequence worth stating loudly, because it is the same trap the requesting team fell into:**
> per-declaring-class indexes do **not** separate sibling subclasses of a shared mixin. A
> `geo-indexed-mixin` that owns `geom` gives `acled-event` and a future FIRMS class *one* index,
> separated only by the type filter of §6, not by storage. A class that takes no mixin and declares
> its own geometry slot gets its own index. That is narrower than "per class."

**Sidecar.** `spatial-index.root` (one plist) becomes `spatial-indexes.dat` (a list of records),
mirroring `secondary-indexes.dat`:

```lisp
(:format 3
 :indexes ((owner-name slot-name address precision backend max-cells precision-counts) …))
```

Written on every index creation **and** at `close-graph`. Secondary indexes write only at close, so
a crash costs them a full rebuild; spatial addresses are stable (`%bpt-address` is a header address
and root splits sync into it — verified), so writing at creation is strictly better and free.

### 4.1 Indexes are created lazily, and an empty one is nearly free

**Lazily.** An index is created on the **first geometry-valued insert** for its `(owner . slot)`,
mirroring `%ix-claim`'s gate-before-create (`index.lisp:236`, whose comment reads "Gate BEFORE
%SLOT-INDEX-FOR so a geometry slot never creates an ordered index"). A slot marked `:index` that
never holds a geometry therefore never creates a spatial index at all. This matters under the
deferred CR-3.2, where a class may declare several geometry slots and populate only some: the
unpopulated ones cost nothing, because they do not exist.

This holds **universally**: nothing is created at open. An earlier revision of this document carved
out an exception for explicitly declared indexes, which were built eagerly by an install pass; that
pass went away with the `def-spatial-index` macro (§5.1), so the lazy rule is unconditional again.
`install-spatial-indexes` survives, but it only *rebuilds* an index whose declared precision no
longer matches its persisted one — it never creates one.

**Nearly free.** A spatial index is built by `make-heap-index` into the graph's **shared
`indexes.dat` heap** (`spatial-index.lisp:60`, `graph.lisp:17`), not as a separately-mmap'd file. It
has **no address-space reservation of any kind**. An empty one is a skip-list head/tail sentinel
pair, or a B+ tree header plus one 4 KiB page (`+bplus-default-page-size+`).

This is worth stating explicitly because the neighbouring vector-segment subsystem *does* reserve —
`max(8 × size, *mmap-min-reservation*, size)`, a **1 GiB floor** per segment
(`docs/superpowers/specs/2026-07-22-segment-reservation-exhaustion-design.md` §1). Spatial indexes
do nothing analogous, and nothing here should be reasoned about by analogy to segments.

§6's declared-but-empty error contract depends on this rule: an index that does not exist yet is the
normal state of a declared-but-unpopulated slot, not a fault.

---

## 5. Declaration and precision resolution

**One surface: the slot option.**

```lisp
(def-vertex deepstate-zone ()
  ((extent :type geometry :index t :spatial-precision 3))
  :mine-action-forensics)
```

Precision resolves as **slot option > graph default (7)**. There is no second surface and therefore
no precedence rule, no conflict, and nothing to warn about.

### 5.1 Why there is no `def-spatial-index` macro

An earlier draft of this document specified one, mirroring `def-index`, and it was built and then
removed. The reasoning is worth keeping, because "add a declarative macro" will look like an obvious
improvement to the next reader.

Spatial is one of four slot-based index kinds in this engine, and the surfaces are not uniform:

| | Slot option | Macro | Maintenance reads |
| --- | --- | --- | --- |
| `:unique` | yes | none | `class-unique-slots` — MOP only |
| `:vector-index` | yes | none | `node-vector-index-slots` — MOP only |
| `:index` (ordered) | yes | `def-index` | `class-secondary-index-descriptors` — MOP ∪ registry |
| spatial | yes | **none** | `node-geometry` — MOP only |

Two of the three have no macro at all. Only `:index` does, and the reason `def-index` can genuinely
index a slot *not* marked `:index` is that its maintenance is **descriptor-driven**: `%ix-claim`
iterates the union and reads `(slot-value node (first d))` by name.

Spatial maintenance is `node-geometry`-driven, and that function scans `:index`-marked slots only.
So a spatial macro could set an index's precision but could never reach an unmarked slot — while
its docstring, copied from `def-index`, claimed it could. Declaring on an unmarked slot eagerly
created and persisted an index that no node was ever written into, and a query scoped to that class
then signalled "not a spatially indexed class": a completely silent no-op, which is precisely the
CR-3 failure class this whole change request exists to eliminate.

Making the claim true would mean giving `%spatial-index-node` a descriptor loop over declared slots
alongside the `node-geometry` path. That widens the commit hot path, and it delivers a slice of
CR-3.2 (a node indexed under more than one geometry slot) through a side door, in a release where
CR-3.2 is deliberately deferred and versioned — including the refine-path problem CR-3.2 has to
solve, which a declared slot would inherit unsolved.

The macro also had a second defect with no good answer at this scope: naming a *subclass* rather
than the declaring owner built an orphan index that the write path never used and no scope ever
scanned, silently.

So the macro is gone and spatial matches `:unique` and `:vector-index`. Revisit only together with
CR-3.2, where the refine question gets answered once for both.

**Precision change on an existing index.** The *persisted* precision is authoritative for reopening.
If the declared precision differs, that one index is **rebuilt automatically at open** — bounded by
`map-vertices :vertex-type owner`, not a whole-graph scan — and logged.

This differs from `def-index`'s "call `regenerate-secondary-indexes` yourself" contract for a
changed canonicalizer, on purpose. A stale canonicalizer gives wrong lookups; a *mixed-precision*
index reintroduces the §2.1(a) query-miss bug directly, so leaving it to the user would be silently
wrong.

---

## 6. Query API

```lisp
(find-nodes-within       scope area &key (graph *graph*))
(find-nodes-intersecting scope area &key (graph *graph*))
(find-nodes-near         scope lat lon radius &key (graph *graph*))
(find-nearest-k          scope lat lon k &key (graph *graph*) (max-radius 2.5d4))
```

`scope` is a class-name symbol, a list of class-name symbols, or `:all`. Class-before-payload
matches `index-lookup` and `map-index`. A required positional argument makes every stale call site a
**compile-time** warning on SBCL and ECL, which is the safest way to land a deliberate break.

**Resolution.** A scope resolves to a set of `(owner . slot)` keys, then to live indexes:

- class `C` → for each of `C`'s geometry-index slot names (MOP `:index` slots plus any applicable
  slots), resolve the owner with `%indexed-slot-owner-name`, collect
  `(owner . slot)`
- class `C` with an **application-supplied `node-geometry` method** → `(method-owner . NIL)`.
  Overriding `node-geometry` is a documented extension point (`example.lisp`), and such a method
  returns a computed geometry with no slot name, so there is no `(owner . slot)` key to derive.
  These classes are indexed under a NIL slot and are **scopeable by name like any other** — leaving
  them reachable only through `:all` would push a documented feature onto exactly the unscoped
  query this design forbids. The method owner is resolved most-general-first, mirroring
  `%indexed-slot-owner-name`, so a method on a parent gives its subclasses one shared index rather
  than scattering them per-subclass
- a list → the union of each element's keys
- `:all` → every key in the registry

**Type filter.** Results are filtered to `(typep node C)` for some `C` in scope; `:all` filters
nothing. This is what makes the request's test property 1 hold in the case that actually matters —
where A and B share a mixin-owned index and storage separation alone does not discriminate. It costs
nothing: the node is in hand at refine time.

**Dedup.** Each index already dedups internally (`spatial-index.lisp:144`); the scope loop adds an
outer `seen` on node id, so a node reachable through two of its own slot-indexes is returned once.
That outer dedup is what lets the deferred CR-3.2 drop in unchanged.

**Error contract**, reusing `%require-index`'s shape (`index.lisp:471`):

- scope names a class with **no geometry at all** — neither an `:index`-marked geometry slot nor a
  `node-geometry` method → **signal**. A programming error, and the reason the scope is required
- declared, but the index holds no entries yet → return `NIL`. A legitimately empty result must not
  look like a bug

**Prolog.** New arities carry the scope in second position: `find-within/3`, `find-intersects/3`,
`find-near/5`, `find-nearest/5`.

```lisp
(select-flat (?f) (is-a ?f eo-find) (find-near ?f eo-find 49.20 37.17 500.0))
```

The old `/2` and `/4` forms are **removed** rather than left to signal, so a stale query fails at
**goal entry** with unknown-functor, rather than binding against the wrong arity. Note this is
*not* compile time: `prologc.lisp:198-212` emits the functor lookup as runtime code inside the
clause body, so a stale query macroexpands and compiles cleanly and fails when the goal is first
entered. (An earlier draft of this document claimed query-compile time; that was wrong, and it was
a stated reason for removing the arities rather than leaving them to signal. Removal is still
preferred — an unknown functor names the problem, where a surviving `/2` would silently bind a
scope-shaped argument as an area.) The scope
argument accepts the same three shapes, **including a literal list** — confirmed, not assumed: a
test passes `(scope-probe scope-zone)` through the query compiler with a radius wide enough that a
collapse-to-first-element would show in the results. The contingency this document previously
carried (restrict Prolog scope to symbol-or-`:all` and use a disjunction for multiple classes) is
therefore not needed.

**Unchanged.** `make-spatial-replication-filter` reads `node-geometry` directly and never touches
the index. Each index computes its own `cover-prec` as `min(own precision, adaptive)`, so a scope
spanning a p=3 and a p=7 index needs no coordination.

**Changed.** `find-nearest-k` seeds its search radius from cell size (`spatial-query.lisp:139`).
Across a mixed-precision scope it seeds from the **finest** precision present. Correctness is
unaffected either way — the loop widens until `k` are enclosed — but seeding off a p=3 index would
make the first query a 156 km sweep. Finest-precision seeding degrades to exactly today's behaviour
for a single-index scope.

---

## 7. CR-2: bounded insert cover

### 7.1 The bound

`%bbox-cells` picks its covering precision adaptively, as the query path already does:

```lisp
(min precision (%covering-precision dlon dlat max-cells))
```

For a multipolygon, per part, with the budget split `(max 1 (floor max-cells (length parts)))`, so
one huge part cannot starve the rest and the result stays a pure function of the geometry.

**`max-cells` is per-index and persisted, not a global constant.** This is load-bearing.
`spatial-index-remove` recomputes cells from the geometry; if the cap could drift between an insert
and its matching remove — a tunable special, or a constant changed across engine versions —
removals would compute a different cell set and orphan entries permanently. Pinning it per index at
creation makes insert/remove symmetric by construction. Changing it forces the same rebuild as
changing precision.

**Default 16384.** Sized as a safety net, not a selectivity knob — per-index precision is that knob.
The job is turning 7.7 × 10⁷ into something finite; anything in the 10³–10⁴ range does that. 16384 is
chosen so nothing that works today changes behaviour: a site-scale 0.05° polygon is ~1,444 cells at
p=7 and a city-scale 0.1° one ~5,476, both well under. The query cap stays 256; the asymmetry is
deliberate, since insert is paid once per write and finer storage is strictly better there.

### 7.2 The clamp

Each index carries `precision-counts`: a 12-element vector counting how many stored cell entries
exist at each geohash precision. `coarsest-precision` is **derived** from it — the lowest occupied
level — and cached, recomputed only when a counter crosses zero:

```lisp
(loop for p from 1 to 12 when (plusp (aref counts p)) return p)
```

`spatial-index-insert` increments `counts[p]` per cell written; `spatial-index-remove` decrements.
Because §7.1's persisted `max-cells` makes insert and remove compute identical cell sets, the
counters stay balanced by construction. The query becomes:

```lisp
(min (spatial-index-precision idx)
     (%covering-precision dlon dlat +spatial-query-max-cells+)
     (spatial-index-coarsest-precision idx))
```

The third term is the correctness fix. It guarantees every stored cell is at or finer than the
covering cell, so the prefix range scan still reaches it — closing the hole in §2.1(a).

**Self-healing.** A histogram rather than a monotonic high-water means the clamp *recovers*: delete
the oversized polygon, its cells decrement, that level empties, and `coarsest-precision` rises back
on its own. No rebuild, no operator action. An index degraded by one bad insert un-degrades when the
bad node leaves — which is what anyone would expect and what a high-water mark would not have
delivered.

**Durability, and why the asymmetry is safe.** Drift in the two directions has opposite
consequences, and only one of them needs a synchronous write:

- Losing a **decrease** (a newly-occupied coarser level) reopens with a too-*fine* clamp → the query
  covers finer than a stored coarse cell → **misses**. Unsafe.
- Losing an **increase** (a level emptied) reopens with a too-*coarse* clamp → the query
  over-covers → more candidates, exact refine unchanged → **correct, merely slower**. Safe.

So the sidecar is rewritten synchronously **only when `coarsest-precision` decreases** — the rare,
exceptional path, exactly as before. Increases ride the ordinary close-time write. Any residual
drift is corrected by any rebuild, which recomputes the histogram from scratch.

Test 7 (insert/remove symmetry) is what keeps this honest: an imbalance in the counters is the one
thing that could push the clamp in the unsafe direction, and a residual-entry assertion catches it.

### 7.3 What this means in practice

For the motivating case the clamp **never fires**. A `deepstate-zone.extent` index at p=3 stores an
18.1° × 8.0° polygon at `min(3, 4) = 3` — the index's own precision, no coarsening at all.

Reference numbers for a Ukraine-sized (18.1° × 8.0°) polygon:

| precision | cell size | cells |
| --- | --- | --- |
| 3 | 1.406° (~156 km) | ~98 |
| 4 | 0.352° × 0.176° | ~2,491 |
| 5 | 0.0439° (~4.9 km) | ~76,000 |
| 7 | 0.001373° (~153 m) | ~7.7 × 10⁷ |

At p=3, ~5,868 zones cost ~575k index entries. **Per-index precision is the real fix; the cap and
clamp are the safety net for anyone whose grid does not match their data.** When the clamp does
fire, one oversized insert degrades that index's selectivity until rebuilt — honest signalling, and
exactly the cost the deferred multi-resolution work removes.

### 7.4 Observability

`warn` on every *decrease* of `coarsest-precision`, naming node id, class, slot, bbox, and requested
vs granted precision; `log:info` per coarsened insert. Warning per node would mean thousands of
warnings on a bulk ingest; warning per decrease is loud, rare, and still names a specific node.

**The warning must state the recovery path**, or an operator reads "selectivity degraded", removes
the offending node, and cannot tell whether anything improved. Two routes, and the text names both:

- **Automatic** — removing every node stored at that precision raises `coarsest-precision` back on
  its own (§7.2). This is the normal path and needs no intervention.
- **Manual** — `regenerate-spatial-index` (graph, owner, slot) rebuilds that **one** index and
  recomputes its histogram from live nodes. Note the singular: regenerating every spatial index in
  the graph to clear one degraded index is a bad trade, so the per-index form is the one to reach
  for. `regenerate-spatial-indexes` (plural) remains, for a backend switch.

A symmetric `log:info` fires when `coarsest-precision` rises, so the recovery is as visible in the
log as the degradation was.

---

## 8. CR-3.1: inert-slot warning

Value-based, since the declared-type version is not buildable (§2.1(b)).

A node reaching spatial maintenance runs the geometry loop *without* the early return, counts
geometry-valued indexed slots, and if there are ≥2, warns naming the class, every geometry slot
found, and which one wins.

**Sampled over a class's first 64 nodes, not just its first.** The full loop runs while a per-class
counter on `*node-geometry-slot-cache*` is under 64, and stops early the moment it fires; after that
every write takes today's early-return path. Checking only the first node would miss any class whose
first node happens to be unrepresentative — the common case for a schema where `centroid` is
populated at creation and `extent` is filled in later. Checking *always* is the wrong trade in the
other direction: `node-geometry-index-slots` returns **all** `:index` slots, scalars included, so
dropping the early return permanently would cost a slot read per indexed slot on every spatial
write. 64 is bounded, negligible, and covers the realistic case.

**`audit-spatial-slots` (graph)** does the §9 migration sweep *without* rebuilding: it visits every
live node, reports every class with more than one geometry-valued indexed slot, and names the winner
for each. Available on demand and in CI — the answer for classes added long after migration, and the
thing to wire into a schema test suite.

Two honest caveats remain:

1. Sampling is bounded, not exhaustive. A class where fewer than 1 in 64 nodes binds two geometry
   slots still slips past the sampler. `audit-spatial-slots` is what closes that, for anyone who
   runs it.
2. **The migration sweep of §9 is a free audit of everything present at that moment.** It visits
   every node, so it surfaces all four of mine-action's affected classes (`admin-area`, `site`,
   `survey`, `hazard-area`) on first open — precisely when they want to know.

This warning is scoped to the present semantics. When CR-3.2 lands, multiple indexed geometry slots
become the *intended* configuration and this warning is **removed**, not suppressed.

---

## 9. Migration

**Trigger** is the filename: `spatial-index.root` present and `spatial-indexes.dat` absent →
re-derive. So does `:format` ≠ 3.

**Re-derivation** is `rebuild-spatial-indexes`: one `map-vertices` + `map-edges` sweep routing each
node to its `(owner . slot)` index by the same per-node rule used today, so contents come out
identical to what the single index held, merely partitioned.

The requesting team's hard constraint is met: **index only, nodes untouched, nothing re-fetched.**

**Automatic**, not opt-in. The entry point is exported so it can be forced, but a graph silently
running on a stale-format index is the failure mode being closed. Cost is one full scan at open;
~286k vertices is real but one-time.

**The old `spatial-index.root` is left in place**, with `spatial-indexes.dat` taking precedence.
Renaming it was considered and rejected: old `restore-spatial-index` treats a missing file as
`init-spatial-index`, which yields a *silently empty* index rather than a rebuild, so renaming would
make a downgrade fail quietly. Leaving it means a downgrade gets a stale-but-valid v2 index instead.
Neither is good; **downgrade after migration is documented as unsupported** rather than engineered
for.

---

## 10. Memory-graph

The image's `:spatial` payload becomes one record per `(owner . slot)` — carrying that index's
precision, insert cap and precision histogram, exactly like the on-disk sidecar — and each index
restores **structurally** into its own `mem-skip-list`, as views do.

There are **two** memory image formats, and both had to move: the cl-store image (version bumped so
an old single-index payload routes to the nodes-only rebuild rather than being destructured as a
record list) and the **native/lazy format the ECL device actually uses** (also bumped). The native
one was the path still rebuilding-from-nodes on open, which is what made this more than a cl-store
edit — see §10.1.

`%rebuild-derived-from-nodes` remains as the v1 (pre-registry image) fallback, keying each node
through `%node-spatial-owner-name` so its owner rule matches the write path (§6). The histogram
reloads into a fresh `(simple-array fixnum (13))` and `coarsest` is re-derived from it, identical to
the on-disk `open-spatial-index` — so a coarsely-stored geometry survives a memory reopen and the
§7.2 clamp is not defeated.

### 10.1 Why this removed a cost rather than adding a feature

Between Task 2 and this point, a memory-graph rebuilt its spatial index from live nodes on every
open, gated by a filter over `node-geometry-index-slots`. That function returns **every** `:index`
slot, scalars included, so the filter faulted in every node of any class with any indexed slot —
materializing the lazy node blobs that issue #50's `:lazy` mode exists to keep on disk. For an
Android field device whose finds carry an indexed scalar (`sku`, `ordnance-class`), that was most of
the corpus, on every open.

Structural restore reloads the skip-lists directly and touches no node blob: measured at **0 of 11**
nodes materialized on reopen, against 11 of 11 before. The three workaround helpers Task 2 added
(`%mem-lznode-may-be-spatial-p`, `%rebuild-memory-spatial-indexes`, `%custom-node-geometry-classes`)
are deleted. There is no sound *narrower* filter — declared-type matching is exactly what the engine
refuses to do — so removing the rebuild is the only real fix, and this is where it lands.

### 10.2 Known limitation: precision re-declaration on a memory reopen

The on-disk backend adopts a changed `:spatial-precision` declaration at open, because
`install-spatial-indexes` rebuilds the affected index. The memory backend does **not** call
`install-spatial-indexes` — doing so would re-materialize the very lazy nodes §10.1 stopped
materializing — so a memory index reopens at its *persisted* precision and a changed declaration is
not adopted until a forced rebuild.

This is a resolution limitation, not a correctness one: the histogram and its clamp round-trip
faithfully, so the index over-covers at the old grid (correct, slightly slower) rather than missing
anything. Closing it properly needs a lazy-aware regenerate, deferred with CR-3.2 and
multi-resolution (§13). **The requesting application must be told: changing a `:spatial-precision`
declaration will not re-grid a memory-graph index on reopen.**

---

## 11. Test plan

The change request's six properties, with one correction and one deferral:

1. **CR-1 discrimination** — two node classes with indexed geometry in one graph; a query scoped to
   A returns no B nodes even when B's geometry contains every A node. Both directions.
2. **CR-1 migration** — a graph created under the old scheme opens under the new one and its
   spatial queries return the same results, without rebuilding the graph.
3. **CR-2 bounded insert** — a ~15° × ~9° bbox inserts in bounded time and memory, and a small
   query inside it still returns the node. **Asserted in the mixed form**: a coarse-stored node
   *and* a fine-stored node in the same index, both returned. A single-node version of this test can
   pass by accident; the mixed form is what catches the naive fix.
4. **CR-2 observability** — exceeding the cap emits a warning identifying the node.
5. **CR-3.1** — a class with two geometry-valued indexed slots warns and names the winner.
6. **CR-3.2** — deferred with the feature (§13).

Added here:

7. **Insert/remove symmetry under coarsening** — insert an oversized geometry, delete the node,
   assert zero residual entries. The orphan hazard the persisted `max-cells` exists to prevent;
   nothing else catches it.
8. **High-water durability** — coarse insert, unclean close, reopen, small query still finds it.
9. **Precision-change auto-rebuild** — declared ≠ persisted, reopen, identical results.
10. **Precision declaration** — a `:spatial-precision` slot option sets the index's grid, inherits
    to a subclass that declares no slots of its own, and is range-checked by name; the graph
    default is *not* treated as a declaration, so reopening a graph without repeating
    `:spatial-precision` must not re-grid it.
11. **Scope error contract** — undeclared class signals; declared-but-empty returns `NIL`.
12. **Dedup** — a node reachable through two of its own slot-indexes is returned once.
13. **Prolog scope shapes** — symbol, list, `:all` through the query compiler.
14. **Clamp self-heals** — insert an oversized geometry (clamp drops), delete it, assert
    `coarsest-precision` returns to the index precision and a fine query regains its old
    selectivity, **with no rebuild**. The property the histogram exists for.
15. **Histogram durability asymmetry** — a lost *increase* (unclean close after a delete that
    emptied a level) reopens correct-but-coarse and still returns every node; a *decrease* survives
    an unclean close (this is test 8, re-stated as the unsafe direction).
16. **Lazy creation** — a slot marked `:index` that never holds a geometry creates no index at all;
    a scope naming it returns `NIL` rather than signalling.
17. **`audit-spatial-slots`** — finds a class the 64-node sampler missed, by populating the second
    geometry slot only on a node beyond the sampling window.

Matrix: both backends (skip-list, B+ tree), memory-graph, SBCL + ECL (CCL on Linux).

---

## 12. Note for the mine-action-android team

**The peer wire format does not change.** Verified, not assumed: `scope-node-set`
(`replication.lisp:37`) is traversal-based, not spatial; geometry rides as ordinary node data; and
spatial indexes are local derived structures on both sides. TLV, framing, node-head, geometry
encoding and handshake are untouched. The frozen contract holds.

What changes is **query semantics the SQLite peer must mirror**. The current Kotlin design is
already most of the way there:

1. **Explicit scoping is already how the peer works.** `point_rtree` is generic, and
   `Projections.kt:43-46` instructs callers that they *must* filter by `type_id` on the join. §6's
   required-scope contract matches that. Worth confirming rather than assuming.

2. **`point_rtree` indexes Points only — and that is the real exposure, in the follow-on, not
   here.** Today the engine's first-bound-slot rule indexes a `site` by its `centroid`, so a
   point-only R-tree agrees with the hub. **That agreement is coincidental, not enforced.** When
   CR-3.2 lands and a site is also indexed under its `extent`, the hub will return sites by
   footprint and the device will not. The table is already
   `rtree(id, min_lon, max_lon, min_lat, max_lat)` — a bbox tree — so covering polygons is a
   projection change (store the geometry's bbox instead of the point), not a schema redesign.

3. **The slot-choice rule has no wire representation.** "First geometry-valued `:index` slot in
   effective-slot order" is a Lisp MOP concept; no type *name* is on the wire, and neither is slot
   order. The projection hardcodes `GEOM` and happens to match. The clean fix is to ship the
   geometry-slot ordering in the **type table already being added to the auth-ok plist** by the
   native-SQLite Plan 6 work — same change already in flight, and it turns a coincidence into a
   contract.

4. **Precision is not a conformance concern.** Geohash on the engine side, R-tree on the device
   side, different candidate sets — final results agree provided both refine exactly. The
   bbox-edge comment at `Queries.kt:26` shows the device already reasons in filter/refine terms.

5. **ECL is dropped on Android**, which takes the device off the ECL spatial paths entirely. The
   engine retains ECL support and the test matrix keeps exercising it.

**Action:** nothing blocking on this work. Before CR-3.2 ships, extend `point_rtree` to non-Point
geometries and consume the geometry-slot ordering from the type table.

---

## 13. Deferred

**CR-3.2 — index a node under every indexed geometry slot.** Agreed as the more correct semantics
and wanted; explicitly breaking, so it ships versioned. It needs a plural `node-geometries` protocol
through all six `node-geometry` consumers (§2.1(c)), a ruling on distance semantics for a
multi-geometry node in `find-nodes-near`, and removal of the §8 warning. It needs **no format
change** — same registry, same sidecar shape, same query union — but it does need a re-derivation,
since a node with two bound geometry slots currently sits in one index and must end up in two. The
existing rebuild-on-format-mismatch path handles that with a version bump.

**Multi-resolution query probing.** Removes the §7.2 clamp's selectivity cost entirely: for each
covering cell, additionally probe its ancestor prefixes with *exact-cell* range scans
(`[(cell, +null-key+), (cell, +max-key+)]` — the idiom already used by `ix-lookup`,
`index.lisp:167`) rather than prefix ranges. Ancestors dedupe to a handful, so the added cost is
small. Purely a refinement of the query path: no format change, and it can land whenever the clamp's
cost is felt.

---

## 14. Documentation obligations (definition of done)

This work is not complete until all of the following are updated. It is a **breaking** change on
both API and on-disk format, so the paperwork is not optional.

### 14.1 In this repository

- **`CHANGELOG.md`** — `## [Unreleased]` gains `### Changed` (breaking) and `### Fixed` entries.
  Per the file's own SemVer preamble, breaking API plus an on-disk format bump means the next
  release is **3.0.0**, not 2.2.0.
- **`docs/vivace-graph-v3-doc.org`** — Chapter 13 (the spatial chapter, from line 1876) needs a
  substantial rewrite: making a node type spatial, per-index precision and both declaration
  surfaces, the required scope on every query entry point, the new Prolog arities, and the
  filter/refine note at line 1961 which currently describes a single graph-wide index. Chapter 3
  also mentions the spatial sidecar (line 431) and `rebuild-spatial-index` (line 442).
- **`README.md`** — a compatibility note in the style of the existing 2016-12-12 UUID entry:
  what breaks, what migrates automatically, and that downgrade after migration is unsupported.
- **`SPATIAL-TODO.md`** — 11 references to reconcile; close what this delivers.
- **`example.lisp`** — 5 call sites.
- **Tests** — ~80 call sites across `tests/spatial-query-tests.lisp`,
  `tests/spatial-intersect-tests.lisp`, `tests/geos/`, `tests/replication/slave.lisp`,
  `tests/backup-tests.lisp`, plus the `tests/package.lisp` and `tests/concurrency/package.lisp`
  import lists.
- **`package.lisp`** — export `spatial-indexes`, `spatial-index-for`, `install-spatial-indexes`,
  `rebuild-spatial-indexes`, `regenerate-spatial-index` (per-index, §7.4),
  `regenerate-spatial-indexes` (all), `audit-spatial-slots` (§8), the new Prolog functors; remove
  `spatial-index` and `rebuild-spatial-index`.

### 14.2 GitHub project documentation

- File an issue for this work referencing the change request, and link the deferred items (§13) as
  their own issues so CR-3.2 and multi-resolution are tracked rather than remembered.
- Cut the release following the ritual recorded for 2.1.0: `experiment` → `master` merge, tag
  `v3.0.0`, GitHub release notes drawn from the CHANGELOG, with the breaking changes and the
  automatic migration called out at the top.

### 14.3 Claude project memory

- New memory under `/Users/kraison/.claude/projects/-Users-kraison-work-vivace-graph-v3/memory/`
  recording the outcome and the non-obvious constraints — the prefix-nesting asymmetry that makes a
  coarse insert unsafe without the clamp, the insert/remove determinism requirement on `max-cells`,
  and the mixin-does-not-separate-siblings consequence.
- Index line in `MEMORY.md`.
- Update `vector-segments.md` and `general-index-design.md` cross-links, since spatial now joins the
  same `(owner . slot)` family.

### 14.4 Cross-repository

- **mine-action** — `docs/spatial-index-audit.md` and the DeepState design's §4.1, which currently
  documents the wrong premise and was explicitly waiting on this decision.
- **mine-action-android** — §12 of this document, delivered to that team.
