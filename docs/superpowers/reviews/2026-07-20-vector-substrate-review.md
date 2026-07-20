# Review: spatiotemporal correlation substrate + dense vector storage

**Date:** 2026-07-20
**Reviewer:** Claude (Opus 4.8), reviewing from the **mine-action consumer side** — the demining
knowledge substrate that is today's only production user of `cl-llm.rag.vivace`.
**Verdict:** architecture sound, phasing honest, **two issues to fix before Task 6 is built.**

## What was reviewed

| document | repo | at |
|---|---|---|
| `docs/superpowers/specs/2026-07-20-spatiotemporal-correlation-substrate-design.md` | vivace-graph-v3 | branch `experiment`, `7c98c0c` |
| `docs/superpowers/plans/2026-07-20-dense-vector-storage.md` | vivace-graph-v3 | same |

Code read at `cl-llm` `bbf29aa` (main). Measurements taken against a **live 23,193-chunk store**
(19,973 text + 3,220 image-caption chunks, bge-m3 @ 1024 dims, `cached-graph-store`), so the
numbers below are observed, not estimated.

---

## 1. BUG — Task 6 re-normalises every candidate inside the scoring loop

**Severity: high.** This inverts Phase 1's stated purpose.

Plan Task 6, Step 5, the replacement `store-search` on `scan-graph-store`:

```lisp
(let ((e (rag:as-embedding (%slot vertex "EMBEDDING"))))
  (rag::collect-candidate collector (rag:cosine query-vector e) ...))
```

After Task 4, `as-embedding` (`cl-llm/rag/embed.lisp:7`) allocates a fresh array **and**
L2-normalises it. So this performs, **per candidate, per query**: one allocation, one `sqrt`, and
1024 divisions — precisely the per-element cost Tasks 1–3 exist to eliminate. The scoring inner
loop ends up *more* expensive than it is today.

`as-embedding` is only in that path because VG currently returns the slot as a T-vector of boxed
doubles. Tasks 1–3 make the slot come back as `(simple-array single-float (*))` directly. The plan
already acknowledges this — Task 4 Step 4 instructs updating `vertex->chunk`'s docstring because
its "VG deserialises it as a T-vector" claim "stops being true" — and then leaves the coercion in
the hot loop.

**Fix:** score `(%slot vertex "EMBEDDING")` directly; delete the `as-embedding` call from the
scoring path. Add an assertion (or a `declaim`) that the slot value is already
`(simple-array single-float (*))`, so a regression back to the generic path fails loudly instead
of silently re-boxing.

**Same pattern, lower stakes:** `cl-llm/vivace/store.lisp:113` (`hydrate`, runs once — harmless but
now redundant) and `cl-llm/vivace/schema.lisp:77` (`vertex->chunk`, once per *surviving* candidate
after Task 6 — acceptable, but unnecessary once the slot type is guaranteed).

**Suggested test:** assert that a `store-search` over N candidates performs zero embedding
allocations — or, more simply, that the value scored is `eq` to the slot value.

---

## 2. MIGRATION HAZARD — normalisation is a data-semantics change, and neither document treats it as one

**Severity: high (latent). Not currently biting; would bite silently.**

The plan moves L2 normalisation from **query time** to **ingest time**:

- today `cosine` (`cl-llm/rag/store.lisp:18`) computes `na` and `nb` per call — self-normalising,
  correct on any input;
- after Task 5 it is a bare dot product, correct **only if every stored vector is unit-norm**.

For a store whose vectors are not unit-norm, `dot(unit_query, unnormalised_stored)` scales each
candidate by its own magnitude. Results silently reorder. Nothing errors, nothing logs.

The plan's Global Constraint *"Existing serialized data must still read"* covers the **codec** and
misses the **semantics**. Old data reads back perfectly and ranks wrong.

**Measured, so you can size the real risk.** Two independent samples:

| what | n | min | max |
|---|---|---|---|
| bge-m3 **provider output** (fresh `embed` calls) | 3 | 0.999999912 | 1.000000261 |
| **vectors already stored in the graph** (via `store-search`) | 12 | 0.999999730 | 1.000000510 |

So bge-m3 emits unit vectors and the persisted ones are unit too — **the current mine-action corpus
is safe, and this change is a no-op for it.** Stored element type is confirmed
`(SIMPLE-ARRAY DOUBLE-FLOAT (1024))`, i.e. exactly the fp64 padding §2.1 identifies.

But that safety is a property of **one embedder**, not of the design: `as-embedding`
(`rag/embed.lisp:7`) does no normalising at all today, so nothing in the system enforces or checks
it. Any provider returning unnormalised vectors yields a store whose ranking this change silently
corrupts — with no error, no log, and no failing test.

**Recommended, all cheap:**
1. State the precondition explicitly in the spec (§7.1 mentions normalisation as an ingest step but
   does not flag the compatibility consequence for **already-stored** vectors).
2. Assert it at ingest — `|v| ≈ 1` within tolerance, error or warn-once otherwise.
3. Decide and document what happens to a pre-existing unnormalised store: migrate on open, refuse to
   open, or require re-embedding. Any of the three is fine; silence is not.
4. Add a test: an unnormalised store must be migrated or rejected, never quietly mis-ranked.

Context for why this is worth the paranoia: a structurally identical class of failure — an operation
that reports success while silently doing the wrong thing to stored data — cost us a full recovery
cycle in mine-action this week (see `kraison/cl-llm#8`).

---

## 3. What is genuinely strong (keep these)

- **The reframing.** "This began as *should we build a specialised embedding store?* — the answer is
  yes, but the embedding store is a supporting layer, not the centre of gravity." Correct, and
  correctly load-bearing for everything downstream.
- **§2.2 is the best section in either document.** *"No planned query scans a million vectors
  unfiltered"* is the right reason to skip HNSW, and `segment-score-subset` as the explicit seam —
  an index later becomes a candidate *proposer* feeding the same scoring path — is the right hedge.
  The instruction that nothing downstream may assume it has seen all N vectors is the detail that
  makes it actually work.
- **The §7.4 adversarial review.** The separator-ordering trap is an excellent catch: prefix range
  scans cap the upper bound with `"{"` (ASCII 123), so any separator sorting above it — including
  the obvious `#\|` (124) — puts every timestamped entry outside its own cell's range. That bug
  returns *nothing*, silently, and *only* for timestamped nodes. Finding it on paper is worth a lot.
  The `spatial-index-remove` API-break observation is equally sharp.
- **§10.3 diagnoses its own process failure.** Naming that two reversals both came from reasoning
  about index structure abstractly rather than against the filter/refine contract at
  `spatial-index.lisp:10–14`, and promoting that contract to authority for future proposals, is rare
  and worth preserving as a norm.
- **Deferrals are measurement-gated.** Instrumenting candidate-set sizes so the ANN decision is
  forced by data is the right discipline, and §9 correctly makes that instrumentation a *tested*
  requirement rather than a hope.
- **The plan's TDD discipline is real** — notably *"Verify this in Task 1 Step 2 rather than
  assuming it"* for `extract-length`, and Scope Notes that state plainly what Phase 1 does **not**
  deliver.

---

## 4. Smaller findings

**4.1 §8 oversells Phase 1 relative to the plan's own Scope Notes.**
§8 says Phase 1 *"resolves the performance problem that opened this investigation."* The plan's
Scope Notes correctly say otherwise: the dominant cost named in §2.3 — `vertex->chunk` deserialising
`TEXT` and `METADATA` for every scored candidate — needs Phase 2 segments, because a node's slots
materialise together. Phase 1's real wins are fewer codec allocations, a typed dot, bounded top-k,
and (genuinely valuable) not constructing `rag:chunk` objects for candidates that lose. Recommend
§8 adopt the plan's framing so nobody benchmarks Phase 1 against the wrong expectation.

**4.2 The `:cache` strategy has no retirement decision.**
§2.1 states 8.6 GB of embeddings "does not survive the target corpus size." Phase 1 halves it to
4.3 GB — which also does not survive. Presumably segments replace it in Phase 2; the design should
say so, and say at roughly what N the cache store stops being the default.

**4.3 Prefer symmetric int8 quantisation.**
§7.1 stores "quantisation parameters (scale, zero-point) per vector." A per-vector zero-point makes
dot products carry cross terms requiring sum-of-elements corrections — awkward for the pre-ranking
use case. Scale-only (symmetric) keeps pre-ranking a clean scaled dot. Worth pinning now, since the
element-type byte is already reserved for it.

**4.4 NaN / Inf behaviour is unspecified.**
The plan does not say what `ieee-floats:encode-float32` does with a NaN or infinity. Today such a
value round-trips as a boxed double; after the change it may signal or produce garbage. Embeddings
should never contain them — a malformed provider response is the realistic path. One test, plus a
decision (reject at ingest vs. encode faithfully).

**4.5 Dimension derivation truncates silently.**
`(floor (1- payload-length) 4)` (plan, Task 2 Step 3) yields a short vector rather than an error on
misaligned or corrupt payload. `(zerop (mod (1- len) 4))` is a one-line guard on a codec that will
outlive everyone's memory of this document.

**4.6 `search-matches-brute-force` uses a different total order than the collector.**
The test's reference sorts by score alone (`#'> :key #'car`); the collector orders by
(score DESC, document-id ASC). Ties are effectively impossible with `sin`/`cos` of integers so it
passes today, but the test cannot detect a tie-break regression — which is the very thing Task 6's
own commentary identifies as the subtle risk. Consider making the reference use the same comparator.

**4.7 top-k collector is O(k) per candidate.**
`%top-k-worst-index` scans all k slots per candidate once full. Fine at k=5–8 (our production k is
8), and the plan defends the choice honestly. The crossover where a real heap wins is unstated —
worth a sentence, since k is caller-supplied.

---

## 5. One forward-looking note for the KB-into-graph fold

§5.2 says chunks acquire geography **by edge traversal to a geo-located node**. Worth flagging
early: in the current mine-action knowledge graph, **no such edges exist**. All 23,193 chunks live
in a separate graph with no links to sites, hazard areas, surveys, or EO finds.

So shape B (map-scoped search) and shape C (evidential correlation) are gated not only on L0–L3 but
on **someone deciding what creates chunk→entity edges** — extraction-time entity linking, operator
curation, or a matching pass. That is plausibly a larger and less certain piece of work than the
storage layer, and it is currently invisible in the phasing. Recommend it get named as an explicit
dependency of Phase 3, even if the answer is "later."

---

## 6. Suggested order of action

1. Fix §1 (drop `as-embedding` from the scoring loop) — small, and Task 6 is wrong without it.
2. Decide §2 (normalisation precondition + guard + stored-data policy) — small, prevents a silent
   class of wrongness.
3. Adopt the plan's Phase-1 framing in spec §8 (4.1); add the cache-retirement sentence (4.2).
4. Fold 4.3–4.7 in as they come up; none block.
5. Name the chunk→entity edge question (§5) as a Phase 3 dependency.

Nothing here argues against the architecture. The layering, the ANN deferral, and the
time-as-refine-predicate decision all look right, and the documents are unusually candid about
their own uncertainty — §10's risk list and §10.3's self-critique in particular. The two issues
above are both of the "correct design, incorrect step" kind, and both are cheap to fix now and
expensive to discover later.
