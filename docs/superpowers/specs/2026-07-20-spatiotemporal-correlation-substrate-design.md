# Spatiotemporal correlation substrate — design

Date: 2026-07-20
Status: approved design, not yet planned
Repos: `vivace-graph-v3` (engine, most of the work), `cl-llm` (ranking + surface)

## 1. Summary

Build a **correlation substrate** in VivaceGraph: given an anchor (a field finding, a
candidate SHA, a map selection), find and rank related items across heterogeneous
sources using **space, time, structure, and semantic similarity as composable
signals**.

This project began as "should we build a specialised storage format for document
embeddings?" The answer is yes, but the investigation showed that the embedding store
is a supporting layer, not the centre of gravity. The primary deliverable is the
**spatiotemporal join**; vector similarity is one feature in a ranking function over an
already-narrow candidate set.

## 2. Motivation

### 2.1 The measured problem

Embeddings currently reach the mmap store through the generic vector path
(`serialize.lisp:221`), which allocates a fresh byte vector per element. A 1024-dim
embedding is 1024 separate `serialize` calls. Deserialisation
(`extract-all-subseqs`, `serialize.lisp:219`) conses a subsequence per element and
boxes every float.

The schema declares embeddings as `(simple-array double-float (*))`
(`cl-llm/vivace/schema.lisp`, `ensure-chunk-class`). No embedding provider emits more
than fp32, so half of every stored embedding is zero-information padding.

At 1M vectors × 1024 dims:

| representation | raw bytes | ~one streaming pass @20 GB/s |
|---|---|---|
| double-float (today) | 8.6 GB | ~430 ms |
| single-float | 4.3 GB | ~215 ms |
| int8 quantised | 1.1 GB | ~55 ms |

The default `:cache` strategy (`cl-llm/vivace/store.lisp`) mirrors every chunk into an
in-RAM `rag:memory-store`. At 1M vectors that is 8.6 GB of embeddings alone, before
chunk text and CLOS overhead. It does not survive the target corpus size.

### 2.2 Why an ANN index is *not* the answer

Every planned query shape filters before it scores:

- **A — semantic search** over unlocated reference material (IMAS standards, doctrine,
  manuals). Unfiltered, but the pool is small and roughly bounded.
- **B — map-scoped search.** A viewport is a highly selective spatial filter. A 2%
  selection of 1M vectors is ~20k candidates: ~20M multiply-adds, single-digit ms.
- **C — evidential correlation** (the priority). A spatiotemporal join narrows to a
  small candidate set; embedding similarity ranks within it.

**No planned query scans a million vectors unfiltered.** HNSW is therefore out of scope.
The architecture must keep it *addable* (see §7.2) without requiring it.

### 2.3 Additional cost, independent of the above

- `rag:cosine` (`cl-llm/rag/store.lisp:18`) recomputes the query vector's own norm once
  per candidate and runs untyped, boxing every float.
- `store-search` (`cl-llm/vivace/store.lisp:89`) builds a full hit list for the entire
  corpus and `stable-sort`s it, rather than using a bounded top-k heap.
- `vertex->chunk` (`cl-llm/vivace/store.lisp:99`) deserialises `TEXT` and `METADATA` for
  every scored candidate, when only the embedding and node id are needed.

## 3. Non-goals

- **HNSW / ANN indexing.** Not needed by any planned query shape. Kept addable, not built.
- **Android / on-device.** ECL has been dropped there; the device peer is a Kotlin
  SQLite store. Out of scope entirely.
- **Composite keys in the general ordered index.** Spatiotemporal composites live in the
  spatial index (§7.4). The general index ships v1 as already designed.
- **Replacing structured query with semantic search.** ACLED-style records are
  structured; their fields are modelled as graph structure with real indexes.
  Embeddings apply to free text only (§5.3).

## 4. Architecture

Six layers, bottom-up.

| layer | name | repo |
|---|---|---|
| L0 | dense vector storage | vivace-graph-v3 |
| L1 | vector segments | vivace-graph-v3 |
| L2 | temporal index | vivace-graph-v3 (via general ordered index) |
| L3 | spatiotemporal join | vivace-graph-v3 |
| L4 | ranking + fusion | cl-llm |
| L5 | query surface | both |

## 5. Data model

### 5.1 Pools and segments

Content divides by *filterability*, not by kind:

- **Located pool** — field findings, ACLED events, front-line movement, historical
  incident records. Large and growing; always queried with a spatial and/or temporal
  filter.
- **Unlocated pool** — IMAS standards, doctrine, manuals, equipment documentation.
  Small and roughly bounded; queried globally.

A **segment** is the unit of vector storage: a set of vectors addressable by node id,
scannable independently. Segments correspond to sources (field findings, ACLED,
front-movement, doctrine), which differ in provenance, update cadence, trust level and
embedding characteristics.

Segments are the hedge that keeps the ANN decision deferrable and *per-segment*: any
segment that later outgrows flat scan can gain its own index without touching the
storage layer or other segments.

**Segments retire the `:cache` strategy.** The current default mirrors every chunk into
an in-RAM `rag:memory-store`; §2.1 measures that at 8.6 GB for 1M vectors. Phase 1 halves
it to 4.3 GB, which also does not survive the target corpus. Phase 2 makes segments the
default backing for search, and `:cache` becomes a small-store convenience rather than the
default path. Concretely: `:cache` stops being the default at **~250k vectors** (~1 GB of
embeddings at 1024 dims single-float), and `make-graph-store` should warn once above that
threshold until segments land.

### 5.2 Geography

Text chunks do not carry geometry. Geography belongs to the entity a document is
*about*. Chunks acquire location by edge traversal to a geo-located node. Some content
has no location at all (the unlocated pool), and a map-scoped query must decide
explicitly whether it surfaces (§7.5).

**Open dependency: those edges do not exist yet.** In the current mine-action knowledge
graph all 23,193 chunks live in a separate graph with no links to sites, hazard areas,
surveys, or EO finds. Shapes B and C are therefore gated not only on L0–L3 but on
**someone deciding what creates chunk→entity edges** — extraction-time entity linking,
operator curation, or a matching pass. That is plausibly a larger and less certain piece
of work than the storage layer, and it is a hard prerequisite for Phase 3 (§8) even
though it is not engine work. It needs an owner and an approach before Phase 3 can be
scheduled, even if the answer is "later".

### 5.3 Structured sources

ACLED records carry event type, actors, date, coordinates, and fatality counts, plus a
prose note. The structured fields are modelled as graph structure with indexes and
queried through Prolog / the index API. Only the prose note is embedded.

Front-line movement is **time-varying geometry**: dated polygons. "Was this point behind
the line on date D" is a point-in-dated-polygon query plus reasoning over the date
sequence.

## 6. Query shapes

- **A. Semantic search.** Embed query → scan unlocated segment(s) → top-k.
- **B. Map-scoped search.** Viewport → spatial filter → score candidates by embedding →
  fuse with unlocated backfill.
- **C. Evidential correlation** (priority). Anchor → spatiotemporal join across segments
  → rank by weighted features → fuse across segments.

## 7. Detailed design

### 7.1 L0 — dense vector storage

A new serialisation type tag, `+float-vector+` (allocated in `globals.lisp` alongside
the existing tags):

- Layout: tag byte, element type byte, dimension, then a contiguous block.
- Element type: `single-float` primary. `double-float` accepted on write and narrowed,
  with the narrowing logged once per store, not per vector.
- Read path returns a typed view onto the mmap region without copying or boxing.
- An optional int8-quantised companion representation for cheap pre-ranking, stored
  alongside the full-precision block. Quantisation is **symmetric — a per-vector scale,
  no zero-point.** A zero-point makes dot products carry cross terms needing
  sum-of-elements corrections, which is awkward for the one job this representation has
  (fast approximate pre-ranking). Scale-only keeps pre-ranking a clean scaled dot.

Vectors are **L2-normalised at ingest**, so cosine reduces to a dot product. Today only
the mock embedder normalises; provider vectors pass through `as-embedding` untouched.

**This is a data-semantics change, not just a performance change, and it is not covered
by "existing serialized data must still read".** Two things about already-stored
embeddings become wrong rather than merely slow:

1. **Element type.** Existing vectors were written through the generic vector path as
   T-vectors of boxed `double-float`s. The new tag affects new writes only — old rows
   still decode as T-vectors, so any code declaring the slot `single-float` is wrong for
   them.
2. **Normalisation.** Today `cosine` recomputes both norms per call, so it is correct on
   any input. Once it is a bare dot product it is correct **only** if every stored vector
   is unit-norm. A non-unit stored vector scales its own score and the ranking silently
   reorders — nothing errors, nothing logs.

**Policy: migrate on open.** A store detects non-conforming embeddings when it hydrates
and rewrites them, or refuses to open under an explicit `:error` policy. There is
deliberately no "ignore" option, because the failure mode it would permit is a wrong
answer rather than a slow one.

Empirically the current mine-action corpus is safe on point 2 — bge-m3 returns unit
vectors (sampled norms 0.999999912, 0.999999939, 1.000000261) — but that is a property of
one embedder, not of the design. Point 1 applies regardless of embedder.

### 7.2 L1 — vector segments

A segment stores vectors keyed by node id, decoupled from node payload so scoring never
deserialises `TEXT` or `METADATA`.

Two operations:

- `segment-scan (segment query-vector k)` — bounded top-k heap over the whole segment.
  Used by shapes A and B.
- `segment-score-subset (segment query-vector node-ids)` — score a *given* candidate id
  set. Used by shape C, and the operation that makes HNSW unnecessary.

`segment-score-subset` is the seam that keeps ANN addable: an index, if ever needed,
becomes a candidate *proposer* feeding the same scoring path. Nothing downstream may
assume it has seen all N vectors of a segment.

### 7.3 L2 — temporal index

Delivered by the general ordered index (`docs/general-index-design.md`), unchanged from
its current v1 design. This project needs:

| capability | used for |
|---|---|
| `index-range` on a timestamp slot | temporal filtering with no spatial anchor |
| `index-range` on numeric slots | ranking features (fatalities, confidence) |
| rebuild-on-open + persistence | correctness across restarts |

Composite keys are **not** required from the general index.

### 7.4 L3 — spatiotemporal join

VG has no temporal index today; space is a first-class indexed axis and time is a slot
you scan and filter. For shape C, time is co-primary with space.

**Time ships as a refine predicate, not as part of the index key.**

The spatial index is explicitly a *filter*: it returns candidate ids whose cells meet the
query window, and the caller refines with exact predicates
(`spatial-index.lisp:10–14`). Time fits that contract directly — spatially anchored
candidates carry their own timestamps, and filtering them is a refine step alongside the
existing `geodesic-distance` / point-in-polygon refinement.

For shape C this is sufficient and near-free: an anchor plus a radius covers few cells
and yields a small candidate set, so reading timestamps off those candidates costs
approximately nothing.

**Consequence:** shape C requires no temporal index. Phase 3 is *not* gated on the
general ordered index for the spatially-anchored case (§8).

An earlier revision of this design put a time bucket into the index key —
`(cell, node-id)` → `(cell, time-bucket, node-id)`. That is **deferred** (§11). An
adversarial review found:

- It pays only when the spatial filter is weak — wide area, narrow window (e.g. "all
  shelling in this oblast in March 2022"). Those queries are real but unmeasured, and are
  not the priority shape.
- The key codec is `VIEW-KEY-SERIALIZE` (payload string + 16-byte id), **shared with
  views and unique indexes** (`spatial-index.lisp:43–53`); a third key element means
  touching shared code.
- The workaround of folding time into the payload string is a correctness trap: prefix
  range scans cap the upper bound with the synthetic key `"{"` (`spatial-index.lisp:28`),
  so any separator character sorting above `#\{` (ASCII 123) — including the obvious
  `#\|` (124) — places every timestamped entry outside its own cell's range. Queries
  would silently return nothing, and only for timestamped nodes.
- `spatial-index-remove (idx node-id geom)` (`spatial-index.lisp:117`) derives cells from
  geometry; with time in the key it also needs the *old* time bucket, or a corrected
  timestamp leaks index entries permanently. That is a maintenance API break affecting
  every caller in the transaction hooks.

The structural conclusion still holds and should be recorded: **if** a time dimension is
ever added to an index key, it belongs in the spatial index rather than the general
ordered one, because two-dimensional space is not range-scannable on a single ordered key
— geohash *is* the space-filling curve that makes it so, and a general composite index
cannot expand a bbox into a cell set.

The join entry point:

```lisp
(correlate anchor &key radius window segments graph)
```

returning candidate node ids grouped by segment.

**Backward compatibility (hard requirements).** Deferring the key change satisfies these
trivially — no index change, no compatibility risk. They are recorded because they bind
any future attempt:

1. **Time is optional.** Nodes with no timestamp index spatially exactly as today.
   Mine-action field data must not change.
2. **Existing on-disk indexes still open.** The precedent is `+spatial-index-format+`
   with **rebuild on open** (`spatial-index.lisp:30–34`), the same path used for v1→v2 —
   not reading old keys with new code. That costs existing graphs a one-time index
   rebuild on upgrade, which is acceptable; a forced manual migration is not.
3. **Existing query API unchanged.** `find-nodes-within`, `find-nodes-intersecting`,
   `find-nodes-near` and `find-nearest-k` keep their signatures. Time-aware variants are
   additive. Note this covers the *query* API only — the maintenance path
   (`spatial-index-remove`) would break, per §7.4.

### 7.5 L4 — ranking and fusion

A weighted feature scorer over candidates:

| feature | source |
|---|---|
| spatial distance | join |
| time delta | join |
| embedding cosine | `segment-score-subset` |
| event type / structural match | graph |
| source reliability | segment metadata |

Weights are configuration, not constants — different anchors warrant different
weightings, and tuning requires real query traffic.

Cross-segment fusion reuses existing machinery. `reciprocal-rank-fusion`
(`cl-llm/rag/hybrid.lisp:15`) already takes a *list* of ranked lists, so extending from
two sources to N is free. `dense-preserving-fusion` (`:54`) already implements "keep the
primary ranking, let a secondary source recover a bounded number of items it never
surfaced, without mixing incomparable score scales" — which is exactly the semantics for
unlocated content backfilling into a map-scoped result.

**Policy for unlocated content in map-scoped queries:** always present, bounded backfill
(the `*backfill-max*` pattern). Chosen because its behaviour is predictable enough for a
user to form a stable mental model, and because burying a relevant IMAS clearance
procedure is more costly than burying one more nearby report.

### 7.6 L5 — query surface

Prolog predicates plus a Lisp API, so cl-llm tooling can drive correlation against a
schema it understands. Follows the existing spatial Prolog integration pattern.

## 8. Phasing

| phase | work | depends on |
|---|---|---|
| 1 | L0 dense vector storage; the §2.3 cheap wins | — |
| 2 | L1 segments formalised (incl. `segment-score-subset`) | phase 1 |
| 3 | **L3 join + L4 ranking** | phase 2 |
| 4 | general ordered index → L2; shapes A and B | its own design (in progress) |
| 5 | L5 surface, cl-llm tool integration | phases 3–4 |

Phase 1 is fully independent and unblocks every later phase. **It does not fully resolve
the performance problem that opened this investigation**, and should not be benchmarked as
if it does: the dominant cost named in §2.3 — `vertex->chunk` deserialising `TEXT` and
`METADATA` for every scored candidate — needs Phase 2 segments, because a node's slots
materialise together. Phase 1's real wins are one codec allocation instead of ~1024 per
vector, a typed dot product, bounded top-k, and not constructing `rag:chunk` objects for
candidates that lose.

**The priority deliverable is not externally gated.** An earlier revision put the general
ordered index ahead of the join, because time was to be an indexed axis. With time as a
refine predicate (§7.4), shape C needs only the existing spatial index plus embedding
scoring — so phase 3 follows directly from phases 1–2, both of which are ours to
schedule.

The general ordered index is still needed, but for shapes A and B (temporal filtering
where there is no spatial anchor, and numeric ranking features), which moves it to phase
4. Its design closing is no longer on the critical path.

## 9. Testing

Follows the existing FiveAM suites in `tests/`.

- **L0:** round-trip of dense vectors across all element types and dimensions; the
  double→single narrowing path; int8 quantisation error bounds; reopen after close;
  both index backends (skip-list and B+ tree).
- **L1:** `segment-scan` top-k correctness against a brute-force reference;
  `segment-score-subset` agreeing with `segment-scan` on the same id set; scoring
  touching no node payload (assert `TEXT` is never deserialised).
- **L3:** join correctness against a brute-force spatial+temporal filter; window boundary
  conditions (inclusive/exclusive endpoints); nodes with no timestamp excluded from
  time-windowed results but still returned by spatial-only queries; candidate-set size
  instrumentation asserted present, since the deferral in §7.4 depends on it.
- **L4:** ranking determinism and tie-breaking; fusion behaviour with N > 2 lists;
  bounded backfill honouring its slot cap.
- **Cross-impl:** SBCL and ECL, per the existing matrix.

## 10. Risks

- **Selectivity assumptions are unverified.** "A viewport selects ~2%" is an estimate. If
  the corpus is geographically clustered, wide-zoom queries degrade toward full scan.
  Mitigation: instrument candidate-set sizes from the first phase that produces them, so
  the ANN decision is forced by data rather than guessed.
- **Time-as-refine assumes anchored candidate sets stay small.** True for shape C (anchor
  plus radius). If wide-area/narrow-window planner queries become common, the candidate
  set grows and the deferred composite key comes back into scope. Mitigation: instrument
  candidate-set size and window selectivity together, so the trigger is observed rather
  than argued.
- **Two reversals occurred on the index sub-problem** during design (where composite keys
  belong, then whether they are needed at all). Both came from reasoning about index
  structure abstractly rather than against the filter/refine contract the code states at
  `spatial-index.lisp:10–14`. Mitigation: treat that contract as the authority for any
  future indexing proposal here.

## 11. Deferred

- **Time-bucket dimension in the spatial index key** (§7.4). Deferred behind measurement:
  build it when instrumentation shows wide-area/narrow-window queries with candidate sets
  large enough to matter. If built, it must clear the three compatibility constraints, the
  shared-codec problem, the separator-ordering trap, and the `spatial-index-remove` API
  break — all documented in §7.4.
- HNSW / ANN indexing per segment, if any segment ever outgrows flat scan.
- Composite keys in the general ordered index, for non-spatial multi-column cases.
- Automatic index selection (planner rewriting scan-and-filter into range scans), already
  noted as out of scope in `general-index-design.md`.
- Learned or tuned ranking weights, which need query traffic that does not yet exist.
