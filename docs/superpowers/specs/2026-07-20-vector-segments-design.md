# Vector segments (Phase 2) — design

Date: 2026-07-20
Status: approved design, not yet planned
Repos: `vivace-graph-v3` (engine, most of the work), `cl-llm` (store integration)
Follows: `2026-07-20-spatiotemporal-correlation-substrate-design.md` §7.2 (L1)

## 1. Summary

A **vector segment** is a derived, per-`(graph, class, slot)` index holding one
fixed-width `single-float` vector per node, addressable by node id, stored
contiguously, and maintained on the transaction apply path. It exists so that
scoring never loads a node.

Phase 1 removed the per-element boxing from the codec and the per-candidate
allocation from the scoring loop. What remains is that `map-chunk-vertices`
materialises **every vertex** — the whole data alist, `TEXT` and `METADATA`
included — to reach one slot. Segments remove that.

Segments are also the seam that keeps ANN addable: `segment-score-subset` scores a
*given* candidate set, so an index or a quantised pre-rank later becomes a candidate
*proposer* without touching layout, sync, or API.

## 2. Motivation, and what is not yet proven

### 2.1 The remaining cost

`store-search` on `scan-graph-store` now scores the `EMBEDDING` slot directly with
no coercion in the loop, and builds `rag:chunk` objects only for survivors. The
per-candidate work that remains is loading the vertex itself.

### 2.2 This is an attribution claim, and it is unverified

**No post-Phase-1 query latency has been measured.** The claim "node loading
dominates" is inference from reading the code, not measurement. This design's own
history is a warning here: four separate conclusions in Phase 1 that were reached by
reading code and not running it turned out wrong or overstated.

Section 10 therefore gates the work: the attribution experiment runs **first**, and
can invalidate the rest of this document.

### 2.3 Sizing

Reference corpus (mine-action, measured during Phase 1): 19,973 chunks at 1024
dimensions. Design target (from the Phase 1 spec §2.1): 1M vectors.

At 1M × 1024 `single-float`, a segment is **4.3 GB**, streaming in roughly 215 ms —
against a node-loading scan that must touch every `TEXT` blob in the corpus.

## 3. Non-goals

- **ANN indexing.** Still out of scope; the segment is the seam it would plug into.
- **int8 quantisation.** The Phase 1 spec commits to symmetric per-vector scale
  (§7.1) and it is the eventual answer for space. Not in Phase 2: it would make the
  segment a candidate *proposer* requiring a rerank stage, which is a second new
  thing to get right in the same phase. It drops in later without changing layout,
  sync, or API.
- **Making the segment authoritative.** See §4.
- **ECL.** Out of scope for this project. SBCL only.

## 4. Architecture

A segment is a **derived index, not storage**. The node keeps its `EMBEDDING` slot;
the segment holds a copy.

This is forced, not chosen. **Peer replication ships node data, and that wire format
is a frozen external contract** (the Kotlin SQLite peer). If a vector lived only in
a segment, replicated peers would silently stop receiving embeddings — a failure
that would surface as degraded retrieval on devices, far from its cause.

Being derived buys two properties worth stating explicitly:

- **Every corruption state is recoverable by deleting the file.** A stale, corrupt,
  or format-obsolete segment is rebuilt from the nodes.
- **The invariant is checkable.** "The segment equals what a rebuild would produce"
  is a property a test can assert directly (§11).

The cost is a second copy: 4.3 GB per 1M vectors, on top of the same in nodes. Disk
on the hub is not a constraint.

The precedent is exact — the spatial index is also an id-keyed side structure that
must never disagree with the nodes, and it is maintained on the transaction apply
path at `transactions.lisp:858-873`.

## 5. On-disk layout

One file per segment under the graph directory. Three regions:

| region | contents |
|---|---|
| header | magic, format version, dimension, element type, capacity, live count, free-list head |
| id array | `capacity` × 16-byte node id; a sentinel marks a free slot and carries the next-free index |
| vector block | `capacity` × `dimension` × 4 bytes, contiguous |

- **Growth** reuses the engine's stable-address mmap (`MAP_FIXED` reserve, pointer
  never moves), so growing a segment cannot invalidate a pointer held by an
  in-flight scan. That machinery exists and was hardened for this hazard.
- **Deletes** push the slot onto a free list threaded through the id array, so the
  free list costs no separate structure and reclaim is O(1) inside the transaction.
  There is **no compaction pass** — density stays high because reuse refills holes.
- **Dimension** is fixed per segment, established by the first vector and validated
  on every write.
- **Slot order is meaningless.** Reuse scrambles it immediately. Anything wanting
  ordered iteration reads the id array, never the slot index.

### 5.1 The id→slot map is derived too

Updates and deletes need id→slot. Rather than persist a second structure that can
drift from the first, **the on-disk id array is the authority** and the map is an
in-RAM hash rebuilt at open by sweeping it.

At 1M vectors that sweep reads 16 MB and the map costs roughly 40–60 MB resident.
Cheap, and it makes id→slot drift structurally impossible rather than merely
unlikely.

## 6. Synchronisation

Three hooks on the transaction apply path, mirroring the spatial index's placement:

| transaction event | segment operation |
|---|---|
| create | `segment-put (id vector)` — take a free slot, or extend |
| update, value still present and conforming | overwrite in place; the slot index does not move, because width is fixed |
| update, value now absent or non-conforming | **treat as a delete** — `segment-remove (id)` |
| delete | `segment-remove (id)` — push the slot onto the free list |

Applied only for classes declaring a vector index on a slot.

Two cases the table makes explicit because they are easy to leave undefined:

- **An update that clears or invalidates the embedding removes the segment entry.**
  Otherwise the segment would keep scoring a vector the node no longer has — silent
  wrong results, with the node correct. This is the drift failure mode from §13, and
  it is reachable through an ordinary slot write.
- **A write whose dimension disagrees with the segment signals.** Dimension is fixed
  per segment (§5); a mismatch is a caller error, not something to coerce or skip.
  Because the write happens inside the transaction, signalling rolls the transaction
  back, so the node and segment cannot diverge as a result.

Running inside the transaction is the point: the segment write inherits OCC
validation and replication/txn-log participation. Phase 1's Task 6 established what
happens otherwise — a raw `(setf (slot-value ...))` on a live node bypasses the
write-set entirely and *appears* to work, surviving only via `close-graph`'s
snapshot in the single-writer case.

**Rebuild on open** when the segment file is absent or its format version is stale,
following the `+spatial-index-format+` v1→v2 precedent.

**Rebuild runs quiescent** — at open, before the graph accepts writes. There is no
supported way to rebuild a live segment. This is a decision, not an omission: a
rebuild walks `map-vertices`, and a typed scan skips ids whose lookup returns `NIL`
mid-commit (`vertex.lisp:207-216`). That behaviour is snapshot-correct for a reader,
but a *rebuild* that skipped a concurrently-committing node would bake the omission
into the segment permanently — a missing vector, silently, with the node intact.
Requiring quiescence removes the possibility rather than reasoning about the odds.

## 7. API

```lisp
(segment-scan segment query-vector k)                ; → ((score . node-id) ...) best first
(segment-score-subset segment query-vector node-ids) ; → ((score . node-id) ...)
```

Both return **ids, not nodes.** Nothing loads a node until a caller wants payload —
that is the entire win. A 1M-vector scan touches 4.3 GB of contiguous floats and
zero `TEXT` blobs.

`segment-score-subset` is the extension seam. An ANN index or an int8 pre-rank later
proposes candidates into it. **Nothing downstream may assume it has seen all N
vectors** — the same constraint the Phase 1 spec states for this seam.

Ranking is **score descending, ties broken by node id ascending.** Stated concretely
rather than as "a stable secondary key" because the engine layer has only node ids —
`document-id`, which cl-llm's `rank-before-p` uses, is not visible here. Both
operations must use this order, and the tiebreak must be carried through *eviction*,
not applied only to the final sort: eviction happens during iteration, so a
score-only comparison would make results depend on slot order, which §5 says is
meaningless.

cl-llm re-ranks survivors by its own comparator after hydrating chunks. Because that
happens on k items rather than N, the two orders never need to be reconciled — but
the engine's must be deterministic, or the same query returns different results
across rebuilds.

## 8. Declaration

A vector-specific registry, shaped **identically** to the general ordered index's
descriptor — `(owner-class slot extra)` — per `docs/general-index-design.md` §4.

Two sources, one maintenance path, matching the established pattern: a MOP slot
option, and a `def-vector-index` macro registering into a per-graph-name registry,
idempotent across restarts.

Deliberately **not** building the shared `class-secondary-index-descriptors`
machinery now: its other consumer (the general ordered index) does not exist yet, so
that would be infrastructure on speculation. Same descriptor shape means unification
later is mechanical.

## 9. cl-llm integration

- **`:segment` becomes the default strategy** for `make-graph-store`. `:scan`
  remains as the no-index fallback; `:cache` remains available.
- **`:cache` stops being the default**, satisfying the Phase 1 spec's §5.1
  commitment. The ~250k warn-once is still implemented, but it now guards an
  explicit choice rather than warning about our own default.
- **`vertex->chunk`'s `as-embedding` coercion is retired.** Task 6 kept it because
  post-hydrate `store-add` could introduce a non-conforming embedding; Task 7's
  write-side enforcement in `validate-chunks`, plus the matching fix in
  `memory-store`'s `store-add`, closed that path on both stores. The test
  `vertex-to-chunk-coerces-general-vector-embedding` retires with it.

## 10. Measurement — gating, and first

An attribution experiment, not a benchmark. The design rests on the claim that node
loading dominates; that claim is unverified.

| measure | isolates |
|---|---|
| full `store-search` | the number being improved |
| `map-chunk-vertices` with a no-op function | node loading alone |
| scoring vectors already resident in RAM | scoring alone |

Run at the reference corpus shape (19,973 × 1024) and at a synthetic 250k, so the
curve's bend is visible.

**If loading ≈ full search**, the attribution holds, the segment is the right fix,
and its ceiling is known before it is built. **If scoring dominates instead**, the
attribution is wrong and this design must be revisited before implementation, not
after. Record the numbers in this document either way.

### 10.1 Results (measured)

Measured with `cl-llm.bench:run-attribution` / `report-attribution`
(`cl-llm` branch `segment-attribution-benchmark`, commit `d4be10f`), invoked as:

```
sbcl --dynamic-space-size 8192 --non-interactive \
     --eval '(ql:quickload :cl-llm/bench)' \
     --eval '(cl-llm.bench:report-attribution (cl-llm.bench:run-attribution N 1024 :runs 5))'
```

Machine: Apple M3 Pro, macOS 26.5.1 (Darwin 25.5.0, arm64). SBCL 2.5.5
(`(lisp-implementation-version)`). A/B/C/D are the median of 5 warm runs (one
discarded warm-up); E is a single cold-process-reopen sample (see the
docstrings in `bench/attribution.lisp` for exactly what "cold" does and does
not mean — it is a lower bound on true disk-cold I/O, not the number itself).

Both the reference shape and the 250,000-row synthetic completed on the first
attempt — no fallback to smaller sizes was needed.

| n | dim | A (ms) | B (ms) | C (ms) | D (ms) | E (ms) | B/A | D/C |
|---|---|---|---|---|---|---|---|---|
| 19,973 | 1024 | 2287.4 | 2101.9 | 14.9 | 14.8 | 2649.3 | 91.9% | 99.3% |
| 250,000 | 1024 | 34166.6 | 31707.3 | 183.4 | 179.7 | 33127.3 | 92.8% | 98.0% |

Predicted segment latency (D) and predicted win (A − D):

| n | predicted segment latency (D) | predicted win (A − D) |
|---|---|---|
| 19,973 | 14.8 ms | 2272.6 ms |
| 250,000 | 179.7 ms | 33986.9 ms |

No larger size was attempted and none failed — 250,000 × 1024 (the largest
size called for by the plan) ran to completion without exhausting memory or
disk (peak disk usage during the run left 13 GiB free of 926 GiB; the graph's
`heap.dat`/`indexes.dat` grew to ~1.2 GB, consistent with ~1 GB of embeddings
plus text and index overhead).

**Reading the two watch conditions from §2.2/§10:**
- **Loading dominates at both sizes**: B is 91.9% of A at 19,973 and 92.8% of
  A at 250,000 — both comfortably over the report's own 60% threshold, and the
  fraction is stable (even slightly growing) as the corpus scales 12.5×. This
  is the design's central premise, borne out at both points on the curve.
- **D ≈ C, not D ≪ C, at both sizes**: D is 99.3% of C at 19,973 and 98.0% of
  C at 250,000 — well above the report's 70% "contiguity matters" threshold,
  meaning contiguity itself buys next to nothing over scoring the same
  already-resident vectors scattered. A segment's predicted win (A − D) comes
  almost entirely from *not loading whole nodes to reach the embedding* (i.e.
  from keeping vectors resident and avoiding per-node deserialization/pointer
  chasing), not from sequential-scan speed. **This is a different
  justification than the one implied by "contiguous storage is faster to
  scan" — it must be read as "segments win by skipping node materialization,
  not by being a tighter memory layout,"** and the design write-up (§4
  onward) should be understood in that light rather than as a cache-locality
  argument.

**Reading the magnitude, not just the threshold.** The gate asked "does B ≥
60% of A." It is 92%. But the absolute numbers say more than the ratio: a
*warm* search over the 19,973-chunk reference corpus takes **2.3 seconds**, and
250,000 chunks takes **34 seconds** — extrapolating linearly, the 1M-vector
design target is roughly **140 seconds per query**. The mmap `:scan` path is not
slightly slow; it is unusable for RAG at the target scale. So the segment is not
an optimisation of a working system — it is what makes the mmap store viable for
retrieval at all. That reframes the priority: this is load-bearing for the whole
correlation substrate, not a nicety.

**The three-way comparison the benchmark actually measured.** C scores vectors
already resident in RAM, and `cached-graph-store`'s search
(`vivace/store.lisp:302`) delegates to exactly such an in-RAM `memory-store`. So
C is not a synthetic — it is the current `:cache` strategy's real cost. The three
strategies therefore stand as:

| strategy | warm search @ 19,973 | memory model |
|---|---|---|
| `:scan` (load nodes per query) | ~2287 ms (A) | pageable, but 150× too slow |
| `:cache` (in-RAM mirror) | ~15 ms (C) | fast, but 4.3 GB of GC-scanned live objects at 1M |
| `:segment` (predicted) | ~15 ms (D) | fast **and** a pageable mmap |

This is what "the win is resident memory, not scan speed" means concretely: the
segment does **not** beat `:cache` on speed — they are equal. It beats `:cache`
on memory model, giving `:cache`'s latency without `:cache`'s unpageable
footprint, which is precisely the reason the Phase 1 spec §2.1 said `:cache`
"does not survive the target corpus size." Against `:scan` the win is 150×
speed; against `:cache` the win is pageability. The spec's original
contiguous-scan-bandwidth rationale (§2.3, "~215 ms for 4.3 GB") is **not** where
the value comes from and should be corrected — scoring resident vectors is already
sub-second; the 2.3 s is entirely node materialization.

**Determination: attribution holds, decisively — node loading is 92% of warm
search at both measured sizes, and the absolute latency (2.3 s at 20k, ~140 s
extrapolated at 1M) makes the segment load-bearing rather than optional. Build
it (§4–§9). Two corrections to the design's stated rationale, both from the
D ≈ C result: (1) the win is skipping node materialization, not tighter memory
layout or faster scanning; (2) the segment's advantage over the existing `:cache`
strategy is pageable memory at equal speed, not lower latency — §2.3 and §5
should be read in that light.**

## 11. Testing

Ordinary unit coverage: put/get/remove, free-list reuse, dimension validation,
growth across an mmap boundary, header round-trip.

Three that carry the design:

- **The segment never disagrees with the nodes.** After an arbitrary sequence of
  creates, updates and deletes through real transactions, segment contents must
  equal what a fresh rebuild from the nodes produces. This is *the* invariant and
  deserves a randomised sequence, not three hand-picked cases.
- **Rebuild reproduces exactly.** Delete the file, reopen, confirm identical
  reconstruction. This is what makes "derived, not authoritative" a safety property
  rather than a claim.
- **`segment-scan` matches a brute-force scan over nodes**, and
  `segment-score-subset` agrees with `segment-scan` on the same id set, with
  tie-break parity carried through eviction — Phase 1's subtlest bug.

**Standing rule, from Phase 1:** for any check whose comment claims it catches a
specific regression, write the mutant and confirm the test fails. Four of five Phase
1 plan defects were checks that could not fire; every one would have been caught by
this.

## 12. Order of work

1. Measure and attribute (§10) — **gates everything after it**
2. Segment file format and unit operations, no engine integration
3. Transaction hooks and rebuild-on-open
4. `segment-scan` / `segment-score-subset`
5. cl-llm integration: `:segment` default, `:cache` demotion, `vertex->chunk` retirement

**Plan step 1 by itself.** Because §10 can invalidate steps 2–5, writing one
implementation plan across all five would mean planning work that the first step
might delete. Step 1 is small, self-contained, and produces a number; steps 2–5 get
their own plan once that number exists and this document has been updated with it.

A gate nobody can fail is not a gate. If the measurement shows scoring dominates,
the correct outcome is revising this design — not building segments anyway because
they were already planned.

## 13. Risks

- **The attribution may be wrong.** §10 exists to find out cheaply. Mitigation is
  sequencing, not analysis.
- **A second copy of every vector doubles embedding storage.** Accepted; disk on the
  hub is not a constraint, and int8 halves it twice over later.
- **Segment/node drift is the failure mode that matters.** It would be silent —
  wrong or missing search results, with the nodes still correct. Mitigations: the
  write path is inside the transaction, the invariant is directly asserted (§11), and
  every corruption state is recoverable by rebuild.
- **Growth during an in-flight scan.** Mitigated by the existing stable-address mmap;
  a test should exercise growth concurrent with a scan rather than assuming it.
- ~~Rebuild concurrent with writes could produce an incomplete segment.~~
  **Resolved (§6): rebuild runs quiescent, at open, before the graph accepts
  writes.** No supported live rebuild.

## 14. Deferred

- int8 quantised segments (symmetric, per-vector scale) — halves space and quadruples
  scan speed; drops into `segment-score-subset` as a candidate proposer.
- ANN indexing per segment, if any segment outgrows flat scan.
- Unifying the vector-index registry with `class-secondary-index-descriptors` once
  the general ordered index exists.
- Compaction, should a workload ever produce sustained low density that free-list
  reuse does not refill.
