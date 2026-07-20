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

### 5.2 Geography

Text chunks do not carry geometry. Geography belongs to the entity a document is
*about*. Chunks acquire location by edge traversal to a geo-located node. Some content
has no location at all (the unlocated pool), and a map-scoped query must decide
explicitly whether it surfaces (§7.5).

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
  alongside the full-precision block. Quantisation parameters (scale, zero-point) are
  stored per vector.

Vectors are **L2-normalised at ingest**, so cosine reduces to a dot product. Note that
today only the mock embedder normalises; provider vectors pass through `as-embedding`
untouched. Normalisation becomes an explicit ingest step.

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

Naive intersection of "everything within radius R" with "everything in window W"
materialises two large id sets to keep a small overlap, and which side to drive from
depends on selectivity not knowable statically. Instead, the spatial index gains an
optional time dimension in its key:

```
(cell, node-id)  →  (cell, time-bucket, node-id)
```

A bbox or radius query already expands to N cell scans (`%bbox-cells` /
`%geometry-cells`, `spatial-index.lisp:90–107`); each scan becomes narrowed by a time
range. The join is then a single ordered range scan per cell.

This belongs in the spatial index rather than the general one because two-dimensional
space is not range-scannable on a single ordered key — geohash *is* the space-filling
curve that makes it so, and a general composite index cannot expand a bbox into a cell
set.

API, mirroring the existing `precision` knob (`spatial-index.lisp:60`):

```lisp
(make-spatial-index heap &key (precision 7) (time-precision :day) backend)
```

Bucket granularity is a parameter, not a constant. Day buckets are the default: ACLED is
daily and front-line movement is daily-to-weekly.

The join entry point:

```lisp
(correlate anchor &key radius window segments graph)
```

returning candidate node ids grouped by segment.

**Backward compatibility (hard requirements):**

1. **Time is optional.** Nodes with no timestamp index spatially exactly as today.
   Mine-action field data must not change.
2. **Existing on-disk indexes still open.** An index without the time dimension reads
   back and works; migration is via the existing `rebuild-spatial-index`
   (`spatial-query.lisp:174`), never forced.
3. **Existing query API unchanged.** `find-nodes-within`, `find-nodes-intersecting`,
   `find-nodes-near` and `find-nearest-k` keep their signatures. Time-aware variants are
   additive.

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
| 2 | general ordered index → L2 | its own design (in progress) |
| 3 | **L3 join + L4 ranking** | phase 2 |
| 4 | L1 segments formalised; shapes A and B | phase 1 |
| 5 | L5 surface, cl-llm tool integration | phases 3–4 |

Phase 1 is fully independent and carries standalone value: it resolves the performance
problem that opened this investigation and unblocks every later phase. It is the right
work to do while the general index design closes.

The priority deliverable (phase 3) is **gated on the general ordered index**, which is
still in design. Phase 3 cannot start immediately regardless of priority.

## 9. Testing

Follows the existing FiveAM suites in `tests/`.

- **L0:** round-trip of dense vectors across all element types and dimensions; the
  double→single narrowing path; int8 quantisation error bounds; reopen after close;
  both index backends (skip-list and B+ tree).
- **L1:** `segment-scan` top-k correctness against a brute-force reference;
  `segment-score-subset` agreeing with `segment-scan` on the same id set; scoring
  touching no node payload (assert `TEXT` is never deserialised).
- **L3:** spatiotemporal join correctness against brute-force filter; time-bucket
  boundary conditions; **the three backward-compatibility constraints as explicit
  regression tests** — a v1 on-disk index opening and querying unchanged, untimestamped
  nodes indexing and querying as before, and the four existing `find-nodes-*` signatures
  unchanged.
- **L4:** ranking determinism and tie-breaking; fusion behaviour with N > 2 lists;
  bounded backfill honouring its slot cap.
- **Cross-impl:** SBCL and ECL, per the existing matrix.

## 10. Risks

- **Selectivity assumptions are unverified.** "A viewport selects ~2%" is an estimate. If
  the corpus is geographically clustered, wide-zoom queries degrade toward full scan.
  Mitigation: instrument candidate-set sizes from the first phase that produces them, so
  the ANN decision is forced by data rather than guessed.
- **Time-bucket granularity is a guess.** Day buckets suit ACLED and front movement;
  other sources may want coarser or finer. Mitigation: parameterised, and measured once
  real query traffic exists.
- **Spatial index key change touches live mine-action data.** Mitigation: the three hard
  compatibility constraints in §7.4, each with an explicit regression test.
- **Phase 3 is externally gated.** The general ordered index is not yet closed.
  Mitigation: phase 1 is independent and worth doing regardless.

## 11. Deferred

- HNSW / ANN indexing per segment, if any segment ever outgrows flat scan.
- Composite keys in the general ordered index, for non-spatial multi-column cases.
- Automatic index selection (planner rewriting scan-and-filter into range scans), already
  noted as out of scope in `general-index-design.md`.
- Learned or tuned ranking weights, which need query traffic that does not yet exist.
