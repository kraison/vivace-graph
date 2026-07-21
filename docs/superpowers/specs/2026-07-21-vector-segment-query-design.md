# Vector segment query layer (Phase 2, Step 4) — design

Date: 2026-07-21
Status: approved design, not yet planned
Repo: `vivace-graph-v3` (engine, branch `experiment`)
Follows: `2026-07-20-vector-segments-design.md` (§7 L1); Steps 1–3 built the segment
file format, transaction integration, and recovery.

## 1. Summary

The query layer over vector segments: `segment-scan` (bounded top-k cosine over a whole
segment), `segment-score-subset` (score a caller-supplied candidate id set — the seam
that keeps ANN addable), a graph-level `vector-search` entry point, and the concurrency
control that makes concurrent reads safe against a growing commit.

This step delivers the operations the whole segment structure was built for. Step 1's
attribution benchmark established the payoff: node loading is ~92% of `store-search`
cost, scoring ~1% — so scanning the contiguous `single-float` block directly, **never
touching node payload**, is where the win comes from.

## 2. What Steps 1–3 delivered, and what this rests on

The segment is an mmap-backed file: header + id array (threaded free list) + contiguous
vector block; `segment-put`/`segment-get`/`segment-remove`, in-place growth, id→slot map
rebuilt at open. Transaction integration (Model B): one segment per **declaring (owner)
class**, spanning subclasses, keyed `(owner-name . slot)`, maintained on the apply path
under the transaction manager write lock, with validate-phase dimension rollback and
rebuild-from-nodes recovery. `graph-db-suite` 2324/2324.

Two properties this step depends on:

- **`segment-get` reads a slot's vector without materializing a node** — the same
  no-payload access `segment-scan` sweeps.
- **`live-count` is occupancy, not an iterator.** The scan walks `[0, capacity)` and
  skips free cells (first 8 bytes = free marker). It never iterates `[0, live-count)`.

## 3. Concurrency: a per-segment reader-writer lock

### 3.1 The hazard

Until now every segment access ran serialized under the transaction manager lock (single
writer, no concurrent readers), so `%seg-grow` flipping `capacity` last was safe.
`segment-scan` is the **first concurrent reader**, and it can race a growing commit.

The race is a **memory-ordering** one, not a logical-sequence one. `%seg-grow` relocates
the whole vector block to a higher offset (copy, not move — the old block stays intact),
then stores `capacity` last (offset 32). Sequentially that is safe: whichever `capacity`
a reader observes, the corresponding block is consistent. But without a barrier, a
scanning thread on another core can observe the `capacity` store **before** the relocated
vector bytes are visible in its view — capacity says "new," the new block is not there
yet. The engine's "stable base pointer → no fault" guarantee does **not** cover this:
no-fault ≠ no-race.

A non-growing `segment-put` has a smaller version of the same problem — overwriting a
multi-kilobyte vector with `set-bytes` is not atomic, so a lock-free scan could read one
candidate mid-write and score garbage.

### 3.2 The fix: a per-segment rw-lock

A `lock` slot on the `vector-segment` struct (`make-rw-lock`), rebuilt at open (locks are
never persisted). **All mutations take the write side** — `segment-put`,
`segment-remove`, `%seg-grow`. **Scans take the read side** — `segment-scan`,
`segment-score-subset`. Multiple scans run concurrently; a mutation briefly excludes them.

Rejected alternatives: a lock-free seqlock with explicit acquire/release barriers over
mmap'd foreign memory (correct in principle, but subtle memory-ordering code that is hard
to test and easy to get wrong — and its only advantage, zero read-lock overhead, is
illusory here); and COW/double-buffering (more machinery than the problem warrants).

**Why the rw-lock is cheap on the read path:** the read side is *shared* (all scans
concurrent), and mutations are rare relative to scans (writes batched at ingest, growth
amortized O(log n)). An uncontended shared-lock acquire is tens of nanoseconds, taken
**once per scan, not per vector**, against a ~50–200 ms scan. Negligible.

### 3.3 Lock ordering (deadlock-freedom, stated as an invariant)

The write side is only ever taken **inside** the transaction manager lock (mutations run
on the apply path). The read side is taken **alone** (a scan touches no other lock). So:

```
write path:  manager-lock  ⊃  segment-write-lock
read path:   segment-read-lock   (nothing else)
```

No lock-order cycle is possible — a scan never takes the manager lock; a mutator never
takes the segment lock before the manager lock. This invariant is written as a code
comment at both lock sites, because a later change that takes the manager lock while
holding a segment lock would reintroduce a cycle silently.

## 4. `segment-scan`

```lisp
(segment-scan segment query-vector k) → ((score . node-id) ...)   ; best first
```

Under the read lock:

1. Normalize `query-vector` once (the caller does not have to). `|q| = 0` → return empty.
2. Sweep `[0, capacity)`, skipping free cells.
3. For each occupied slot, compute **full cosine** `dot / (|q| · |v|)`, computing `|v|`
   per candidate. A zero-norm stored vector scores `0` (no divide-by-zero), matching
   `rag:cosine`'s convention.
4. Feed each `(score, node-id)` into a bounded top-k collector (§6).
5. Return the top `k` as `(score . node-id)` conses, best first.

**Full cosine, not a bare dot** (decided): correct for any vectors regardless of
normalization, ~2% of scan cost (scoring was never the bottleneck), and *identical to a
bare dot on unit-normalized data* — so it agrees with cl-llm's `:cache`/`:scan` stores on
all data that actually flows through cl-llm, while staying correct for a general engine
caller who did not normalize. It touches only the vector block and id array — no node
payload.

## 5. `segment-score-subset`

```lisp
(segment-score-subset segment query-vector node-ids) → ((score . node-id) ...)
```

The same read lock, query normalization, and full-cosine scoring, but over only the given
ids (resolved through `id→slot`), silently skipping any id absent from the segment.
Returned best-first (not necessarily bounded — the caller supplied the candidate set).

This is the **extension seam**: a future ANN index or int8 pre-rank proposes candidates,
and this scores them exactly. Nothing here assumes it has seen all N vectors of the
segment — the same constraint the Phase 1 spec states for this seam.

## 6. The engine top-k collector

Phase 1 built a bounded collector in cl-llm; the engine cannot depend upward, so it gets
its own in `graph-db`, same design:

- A bounded buffer; when full, evict the current worst if the challenger beats it.
- Total order: **score descending, node-id ascending.** node-id compare is lexicographic
  over the 16-byte id array. The engine has `uuid-array-equal` (`linear-hash.lisp:15`) but
  **no** lexicographic less-than over ids, so a small `%id-less-p` (an unsigned-byte loop,
  first differing byte wins) is part of this task — do not assume a comparator exists.
- **The tiebreak is carried through *eviction*, not applied only to the final sort.**
  Eviction happens during iteration, so a score-only comparison at the k-th boundary
  would make results depend on slot order — which is meaningless under free-list reuse.
  This was Phase 1's subtlest bug; carrying the tiebreak through eviction is what makes
  ranking **deterministic across rebuilds**.

## 7. `vector-search` — the graph-level entry point

```lisp
(vector-search graph class-name slot-name query-vector k) → ((score . node-id) ...)
```

Resolves the owner segment for `(class-name, slot-name)` via the Model-B keying
(`%vector-index-slot-owner-name` → `(owner . slot)` → `vector-segments` lookup) and calls
`segment-scan`. If no owner segment exists yet (nothing indexed for that slot), returns
empty. This is the reachable-from-a-graph API; Step 5 wires cl-llm's `:segment` store to
it. Returning `(score . node-id)` — the caller (cl-llm) maps ids to nodes/chunks and
applies its own `document-id` tiebreak; the engine layer has only node ids.

## 8. Ranking contract

- **`(score . node-id)`, best first.** score is full cosine in `[-1, 1]`.
- **Order: score descending, node-id ascending.** node-id is the deterministic tiebreak
  the engine can compute (document-id is cl-llm's concern, applied on top).
- **Deterministic across rebuilds.** Because the tiebreak is total and carried through
  eviction, the same query returns the same ranking whether the segment was built
  incrementally or rebuilt from nodes, regardless of slot iteration order.

## 9. Testing

Mechanical coverage: top-k correctness vs a brute-force reference; `segment-score-subset`
agreeing with `segment-scan` on the same id set; zero-norm query and zero-norm stored
vector; `k` larger than occupancy; empty segment; `k = 0`.

Two that carry the step:

- **The concurrency test — the reason this step exists.** A scanning thread running
  continuously against a writer thread doing *growing* commits (enough inserts to force
  several `%seg-grow` relocations), asserting every scan returns a self-consistent result:
  no torn vector (no score outside `[-1, 1]`, no NaN), and no committed id missing beyond
  the single in-flight commit. **Sabotage proof:** run the same test against a build with
  the segment lock removed and confirm it fails (torn reads / bad scores) — proving the
  lock is load-bearing, not decorative.
- **Ranking determinism across a rebuild.** Scan a segment, `rebuild-vector-segment`, scan
  again with the same query, assert identical `(score . node-id)` ranking including ties.
  Ties Step-3 recovery to Step-4 query.

**Standing discipline (nine vacuous assertions have shipped in this project):** every
scoring/ranking assertion is guarded so it cannot pass on a NIL or empty `segment-get` /
scan result (`typep`/`length` before any `every`/`loop`). The two load-bearing gates (the
concurrency test and the tiebreak-determinism test) carry mandatory sabotage proofs —
break the mechanism, confirm the test fails, restore.

## 10. Scope

- **In:** `segment-scan`, `segment-score-subset`, the engine top-k collector,
  `vector-search`, the per-segment rw-lock (taken write by all mutations, read by scans).
- **Out:** the cl-llm `:segment` store strategy and `vertex->chunk` retirement (Step 5);
  ANN/HNSW; int8 quantization; any query surface beyond `vector-search` (no Prolog
  predicate, no REST endpoint in this step).
- **SBCL only. ECL out of scope.**

## 11. Risks

- **The concurrency fix is the risk.** A memory-ordering race is exactly the class of bug
  that passes a single-threaded suite and fails in production. Mitigation: the rw-lock
  removes the race by construction (no lock-free reasoning), and the concurrency test with
  its sabotage proof demonstrates the lock is doing the work.
- **Full cosine vs cl-llm's bare dot** could look like a ranking divergence. It is not, on
  normalized data (they are equal on unit vectors); it only differs on un-normalized data,
  where cl-llm never sends any. Step 5's ranking-consistency tests will confirm.
- **`vector-search` resolving to a not-yet-created segment** must return empty, not error —
  lazy creation means an unqueried-but-declared slot has no segment. Tested.
- **Deferred beyond this step:** a peer multi-writer merge feeding conflicting-dimension
  vectors into one owner segment would surface as a mid-apply error (not reachable today;
  no peer config uses `:vector-index`). Noted for when peer replication and vector search
  meet.

## 12. Addendum (as-built, 2026-07-21): the decode tax

The step shipped as designed, but the first end-to-end measurement of the delivered
`segment-scan` found it **~100x slower than the contiguous-float scan this whole phase was
justified by**. Recorded here because the omission — not the code — is the reusable lesson.

Measured on this tree, SBCL 2.5.5 / macOS arm64 (M3), 20,000 vectors x 1024 dims:

| | as first delivered | after the fix |
| --- | --- | --- |
| full `segment-scan` | 1632.7 ms | **35.3 ms** |
| of which `%seg-read-vector` x20000 | 1613.5 ms (98.8%) | 21.1 ms |
| pure scoring x20000 | 17.4 ms | 17.6 ms |

Step 1's attribution measured the reference contiguous scan at 14.8 ms at this shape, and
the "pure scoring" line reproduces it — so the scoring code was always correct and always
fast, and §4's "scoring is ~2% of scan cost" was never wrong. The 1613 ms was **decode**,
a cost the attribution benchmark never measured because it scanned a Lisp array rather
than the mmap.

Root cause: `%seg-read-vector` was written in Step 2 as a correctness-oriented unit
operation — per candidate it read the vector one byte at a time through the mmap layer's
SEGV-retry `:around`, allocated two fresh arrays, and called `ieee-floats:decode-float32`
per element. Step 4 reused it verbatim inside the hot loop. Every task-scoped review saw
"the existing accessor, used correctly" and was right to; **no task brief carried Step 1's
numbers**, so nothing in the per-task process could have caught it. It took reading all
eight commits against the original premise.

The fix (SBCL fast path, original decoder retained as the `#-sbcl` fallback) reads
float32s directly off the stable base pointer, proved bit-identical over 20,000 real
vectors and a 1,024,288-pattern sweep including every exponent and both signs. Two details
worth preserving:

- **The SEGV-retry guard was kept, at one `handler-case` per vector instead of per byte.**
  The stable-address mapping (reserved window + `MAP_FIXED`) is why `*mmap-segv-retries*`
  stays 0, which makes the guard a backstop — not a licence to delete it.
- **Exponent-255 words are still routed through `ieee-floats:decode-float32`.** A bare
  `sap-ref-single` would turn the all-ones free-marker bytes that `%seg-grow` writes into a
  quiet NaN, silently downgrading the concurrency test's primary torn-read detector from an
  error to a plausible-looking score. Such patterns can never be legitimately stored
  (`encode-float32` refuses them), so the branch never fires on real data.

**Process lesson:** an attribution benchmark that measures a *proxy* for the hot path
(a Lisp array standing in for the mmap) establishes an upper bound on the win, not the win.
When a later step reuses an existing accessor inside that hot path, the benchmark must be
re-run end-to-end against the real implementation before the premise is treated as
delivered.

## 13. Addendum (2026-07-21): the 1M measurement — scan cost is a CLIFF, not a slope

Measured on the delivered (post-§12) scan. dim 1024, capacity preallocated to 2^20 so no
`%seg-grow` relocation runs; the same segment scanned at each checkpoint, so live count is
the only variable. SBCL 2.5.5, macOS arm64 (M3), **18 GB RAM**.

| live vectors | scan (median of 5) | ms per 1k | note |
| --- | --- | --- | --- |
| 100,000 | 192.4 ms | 1.92 | CPU-bound |
| 250,000 | 452.7 ms | 1.81 | CPU-bound |
| 500,000 | 939.7 ms | 1.88 | CPU-bound, spread widening (880-1185) |
| **1,000,000** | **11,298 ms** | **11.30** | **I/O-bound** |

Linear extrapolation from 500k predicts ~1.9 s at 1M. **The actual is 11.3 s — 6x worse.**

Confirmed architectural, not a benchmark artifact: re-measured in a FRESH process against a
pre-built, closed segment (no dirty-page writeback competing), six consecutive scans gave
8938 / 9295 / 7917 / 9371 / 9702 / 9747 ms — **flat, not improving with repetition**. Cache
warming would show a descending curve. Write pressure accounted for only ~2 s of the
original 11.3 s.

The mechanism: at dim 1024 the vector block is `n * 4 KB`, so 1M vectors = **4 GB**. Below
the machine's available page cache the scan is CPU-bound at a stable ~1.85 ms/1k; above it,
every query re-reads the block from disk (~445 MB/s observed, i.e. disk throughput). The
free-cell sweep is a function of capacity, not live count, and stays ~25-35 ms throughout —
never a factor.

**The cliff position is hardware-dependent, but its SHAPE is not.** It sits where
`live * dim * 4` crosses available page cache. A 64 GB hub keeps 4 GB resident and would
still see ~1.9 s at 1M; this 18 GB laptop does not. So "how many vectors can we scan" has
no fixed answer — it is `available_cache / (dim * 4)`, and performance does not degrade
gracefully as you approach it, it falls off.

**Consequences for what comes next:**

- Brute-force scan is comfortably interactive to ~500k on this hardware (sub-second) and is
  the right answer there. It is not the right answer at 1M on a memory-constrained host.
- **int8 quantization is now the highest-leverage next move, ahead of ANN.** It is a 4x
  size reduction (1M x 1024 int8 = 1 GB, resident on any of these machines), which moves the
  cliff out 4x and returns the scan to CPU-bound — and it is far simpler than an ANN index.
- It composes with what Step 4 already built: scan the int8 block for candidates, then
  rescore them exactly against the float32 block through `segment-score-subset` — the
  two-stage design the §5 extension seam was specified for. That seam is already
  implemented and lock-proven.
- Step 5 (the cl-llm `:segment` store) should record which side of the cliff the deployment
  target sits on rather than assuming a single latency number.

**Process lesson, the same one as §12 in a different costume:** a benchmark at 20k measured
a CPU-bound regime and told us nothing about the regime that actually matters. Scaling
projections across a memory hierarchy boundary are not projections, they are guesses —
measure at the target size.

### 13.1 Correction: the deployment host is odm (192 GB) — ANN, not int8

The §13 recommendation above was written from an 18 GB laptop and reached for int8 because
that machine's binding constraint was page cache. **The actual deployment host is `odm`,
with 192 GB of RAM**, which changes the conclusion (Kevin, 2026-07-21):

- At dim 1024 the cliff sits at roughly `192 GB / 4 KB` = **~48M vectors**. The corpus will
  not approach that. **Memory is not, and was never going to be, the binding constraint on
  the real host.**
- The constraint that DOES bind on odm is CPU: the scan is ~1.85 ms per 1k vectors when
  fully resident, so 1M vectors is **~1.9 s per query** even with zero paging. That is too
  slow for interactive chat, and no amount of RAM fixes it.
- Therefore **int8 quantization is the wrong lever here** — it reduces bytes, and bytes are
  not the problem on a 192 GB host. **ANN/HNSW is the right direction when the time comes**,
  because it reduces the number of vectors *examined*, which is the actual bottleneck.
- Unchanged: ANN composes with `segment-score-subset` exactly as §5 specifies — the ANN
  index proposes candidates, the seam rescores them exactly against the float32 block.

§13's measurements stand as measured; only its recommendation is superseded. The cliff
remains a real property of the design worth knowing (it will matter for any
memory-constrained deployment, e.g. anything Android-side), it just does not govern odm.

NOT YET MEASURED ON ODM: the ~1.9 s/1M CPU-bound figure is derived from this laptop's
CPU-bound regime, not measured on odm's hardware. Given that two projections in this
work were already wrong by 6x and 100x, treat it as a hypothesis until measured there.
