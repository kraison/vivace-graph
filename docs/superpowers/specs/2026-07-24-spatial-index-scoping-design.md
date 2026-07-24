# Spatial index: per-class scoping, per-index precision, and a bounded insert cover

**Date:** 2026-07-24
**Branch:** `experiment`
**Status:** design approved, not yet implemented
**Responds to:** `mine-action/docs/superpowers/specs/2026-07-24-vg-spatial-index-change-request.md`
**Release impact:** **MAJOR** — breaking public API plus an on-disk format bump (3.0.0)

---

## 1. Summary

The graph's single spatial index becomes a **registry of per-`(owner-class . slot)` indexes**,
mirroring `secondary-indexes`, `unique-indexes` and `vector-segments`. Spatial queries gain a
**required scope** argument. Geohash precision becomes **per index**. The insert-side cell cover
gains a **bounded, persisted cap** and a **query-side clamp** that keeps the bound from silently
losing nodes. A **value-based warning** reports node classes whose second indexed geometry slot is
inert.

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
with a persisted high-water clamp.

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
| Precision declaration | **Both** a slot option and a `def-spatial-index` macro |
| Precision precedence | slot option > macro > graph default (MOP-first, matching `index.lisp`), with a warning on conflict |
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
 :indexes ((owner-name slot-name address precision backend max-cells coarsest-precision) …))
```

Written on every index creation **and** at `close-graph`. Secondary indexes write only at close, so
a crash costs them a full rebuild; spatial addresses are stable (`%bpt-address` is a header address
and root splits sync into it — verified), so writing at creation is strictly better and free.

---

## 5. Declaration and precision resolution

Two surfaces:

```lisp
;; slot option
(def-vertex deepstate-zone ()
  ((extent :type geometry :index t :spatial-precision 3))
  :mine-action-forensics)

;; macro
(def-spatial-index deepstate-zone extent :mine-action-forensics :precision 3)
```

`def-spatial-index` mirrors `def-index`: it registers a spec, builds immediately if the graph is
open, and otherwise is built at open by an install pass. Like `def-index`, it can also declare a
spatial index on a slot *not* marked `:index`.

**Precedence: slot option > `def-spatial-index` > graph default (7).**

This is **MOP-first**, matching `class-secondary-index-descriptors` (`index.lisp:110`) exactly. One
rule across both halves of `:index`: a reader who learns how the ordered secondary index resolves a
conflict does not have to remember that spatial goes the other way. The point of this document is
not to create the next asymmetry.

When both surfaces are present and disagree, **warn once at install**. This matters more than the
direction of the rule: the losing declaration is loud either way, so a `def-spatial-index` that is
overridden by a slot option is never a silent no-op.

**Usage guideline — the two surfaces are complementary, not competing.** Use the slot option for
what the schema declares, and `def-spatial-index` for what it does not. A slot with no
`:spatial-precision` can be tuned freely out-of-band; a slot that declares one is stating its
precision as part of the schema, and changing it is a schema edit. Declaring the same thing twice is
what the warning exists to discourage.

An inversion (macro wins) was considered and rejected. It would buy out-of-band retuning of a slot
that already declares a precision, and per-graph divergence — the CLOS class and its slot option are
global, while `def-spatial-index` is graph-scoped like `def-index` and `def-view`, since
`def-node-type` registers metadata per graph name (`schema.lisp:422`). Neither is a demonstrated
need here, and both are outweighed by having one resolution rule. Revisit on evidence if a class is
ever genuinely registered into two graphs wanting different grids.

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
  `def-spatial-index` spec), resolve the owner with `%indexed-slot-owner-name`, collect
  `(owner . slot)`
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

- scope names a class with **no** geometry-index slot declared → **signal**. A programming error,
  and the reason the scope is required
- declared, but the index holds no entries yet → return `NIL`. A legitimately empty result must not
  look like a bug

**Prolog.** New arities carry the scope in second position: `find-within/3`, `find-intersects/3`,
`find-near/5`, `find-nearest/5`.

```lisp
(select-flat (?f) (is-a ?f eo-find) (find-near ?f eo-find 49.20 37.17 500.0))
```

The old `/2` and `/4` forms are **removed** rather than left to signal, so a stale query fails at
query-compile time with unknown-functor instead of at runtime inside a solution loop. The scope
argument accepts the same three shapes; a test pins whether a literal list survives the Prolog
compiler's argument handling. If it does not, Prolog scope is documented as symbol-or-`:all` and
multi-class queries use a disjunction.

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

Each index carries `coarsest-precision`: the minimum precision ever used for a stored cell,
initialized to the index precision and monotonically decreasing. The query becomes:

```lisp
(min (spatial-index-precision idx)
     (%covering-precision dlon dlat +spatial-query-max-cells+)
     (spatial-index-coarsest-precision idx))
```

The third term is the correctness fix. It guarantees every stored cell is at or finer than the
covering cell, so the prefix range scan still reaches it — closing the hole in §2.1(a).

**Durability.** Crashing after a coarse insert but before persisting the new high-water would reopen
with a too-fine `coarsest-precision` and silently miss. The sidecar is therefore rewritten
**whenever the high-water decreases** — a small `cl-store` write on a genuinely exceptional path,
not per insert.

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

`warn` on every *decrease* of the high-water mark, naming node id, class, slot, bbox, and requested
vs granted precision; `log:info` per coarsened insert. Warning per node would mean thousands of
warnings on a bulk ingest; warning per decrease is loud, rare, and still names a specific node.

---

## 8. CR-3.1: inert-slot warning

Value-based, since the declared-type version is not buildable (§2.1(b)).

The first node of a class to reach spatial maintenance runs the geometry loop *without* the early
return, counts geometry-valued indexed slots, and if there are ≥2, warns naming the class, every
geometry slot found, and which one wins. A per-class flag on `*node-geometry-slot-cache*` makes it
once-ever; every subsequent write takes today's early-return path. Zero steady-state cost.

Two honest caveats:

1. It samples one node. A class whose first node has only `centroid` bound will not warn even if a
   later node has both.
2. **The migration sweep of §9 is the real audit.** It visits every node, so it surfaces all four
   of mine-action's affected classes (`admin-area`, `site`, `survey`, `hazard-area`) on first open —
   precisely when they want to know.

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

`%rebuild-derived-from-nodes` and the image dump's `:spatial` key (`memory-graph.lisp:604`) both
assume one index; both become a map keyed `(owner . slot)`, with a memory-image version bump so an
old image re-derives. Each index gets its own `mem-skip-list`, as views do.

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
10. **Both-surfaces precedence** — slot option and `def-spatial-index` disagree; the slot option
    wins and a warning is emitted. Also the non-conflicting case: a slot with no
    `:spatial-precision` takes the macro's value.
11. **Scope error contract** — undeclared class signals; declared-but-empty returns `NIL`.
12. **Dedup** — a node reachable through two of its own slot-indexes is returned once.
13. **Prolog scope shapes** — symbol, list, `:all` through the query compiler.

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
- **`package.lisp`** — export `spatial-indexes`, `spatial-index-for`, `def-spatial-index`,
  `rebuild-spatial-indexes`, `regenerate-spatial-indexes`, the new Prolog functors; remove
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
