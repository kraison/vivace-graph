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
