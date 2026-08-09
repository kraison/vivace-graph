# Review 2: what landed for Phase 1 (dense vector storage + normalisation)

**Date:** 2026-07-20
**Reviewer:** Claude (Opus 4.8), mine-action consumer side.
**Follows:** `2026-07-20-vector-substrate-review.md` (design/plan review).
**Reviewed:** vivace-graph-v3 `experiment` `7c98c0c..8517cd9` (5 commits) ·
cl-llm `main` `bbf29aa..45908a3` (9 commits).

**Verdict: the implementation is better than the plan it came from.** Both blocking findings
are fixed, and two of the fixes are more thorough than what I proposed. **One new issue —
the migration is a single unbounded transaction that will not survive the spec's own target
corpus size (§2 below).** Everything else is notes.

---

## 1. Verified empirically, not just read

Both of my blocking findings were about things that *would* go wrong, so I tested rather than
re-read. Subject: a clone of mine-action's real pre-figures knowledge graph — **19,973 chunks,
every embedding stored as `(simple-array double-float (1024))`** — i.e. exactly the legacy
shape the migration exists for. Run on SBCL 2.5.5, 8 GB dynamic space.

| | result |
|---|---|
| first open (migration runs) | **OK — 32.5 s**, 19,973 chunks |
| RSS across that open | **575 MB → 1346 MB (+771 MB)** |
| stored element type after | **`(SIMPLE-ARRAY SINGLE-FLOAT (1024))`** |
| norm after | **1.00000000** |
| retrieval after | works; top hit score 0.726610 |
| clean close | OK |
| **second open** | **9.9 s** |
| **second `migrate-embeddings`** | **0 victims — idempotent** |

Two things this settles:

- **The declared-slot-type concern from Review 1 was unfounded.** I flagged that changing the
  `:type` to `single-float` while persisted values were `double-float` might make the migration
  unreachable (open fails before migration runs). It does not — graph-db does not enforce the
  CLOS slot type on load, so a legacy store opens and migrates cleanly. Good news, and worth
  recording so nobody re-litigates it.
- **The migration is genuinely idempotent**, which is the property that makes it safe to run at
  every open. `%needs-migration-p`'s 1e-4 tolerance comfortably absorbs the re-normalisation
  drift the code documents elsewhere.

Test suites, run here: **cl-llm rag 186/186**, **cl-llm vivace 68/68** (up from 150 and 53 —
substantial new coverage, not just the same tests passing).

---

## 2. NEW — the migration is one unbounded transaction; it will not scale to the target corpus

**Severity: high for large legacy stores. Not a correctness bug; an operational cliff.**

`migrate-embeddings` (`cl-llm/vivace/store.lisp`) collects **every** victim into a list, then
performs **one** `gdb:with-transaction` containing a `gdb:copy` + `gdb:save` per victim. The
transaction is held open across the entire corpus.

Measured above: **19,973 chunks cost +771 MB RSS and ~22.6 s** (32.5 s first open minus 9.9 s
reopen). That is **~40 KB per chunk** — expected, since `gdb:copy` copies the whole vertex,
`TEXT` and `METADATA` included, not just the embedding slot.

Extrapolating to the **1M vectors the spec itself targets** (§2.1): **~38 GB and ~19 minutes in
a single transaction.** That does not work on any machine this project runs on. For reference,
this host OOM-killed at a 4 GB heap earlier today and now runs at 8 GB.

**The failure mode is the bad part.** If the migration dies partway — OOM, kill, timeout — the
transaction rolls back, so *nothing* is migrated. The next open tries the identical operation
and fails identically. The store is stuck: **it never makes progress, and it is never openable.**
And there is deliberately no `:ignore` policy (correctly — silent wrong ranking is worse), while
`:error` refuses to open. So a store too large to migrate atomically **cannot be opened under
either policy**.

**Recommended:**
1. **Batch the writes** — N victims per transaction (a few thousand), committing as it goes, so
   progress is durable and peak memory is bounded by the batch, not the corpus.
2. **Make it resumable by construction.** Because `%needs-migration-p` is a per-vector predicate,
   batching is automatically resumable: an interrupted run leaves earlier batches migrated and
   the next open picks up the remainder. That turns the current all-or-nothing cliff into a
   restartable job for free.
3. **Log progress** (every N, or once with a total). A 19-minute silent startup is
   indistinguishable from a hang, and operators will kill it — which today loses all the work.
4. Consider streaming the victim scan rather than materialising the full list first; at 1M
   vertices the list alone is significant before any copying starts.

Batching does weaken atomicity — a half-migrated store becomes observable. That is fine here:
the store is *already* mixed during migration, `%needs-migration-p` is per-vector, and scoring a
not-yet-migrated vector is the pre-existing behaviour, not new damage. Resumability is worth more
than all-or-nothing for a one-way upgrade.

---

## 3. Both blocking findings from Review 1: fixed, correctly

**3.1 `as-embedding` in the scoring loop — fixed.** `store-search` on `scan-graph-store` now
scores `(%slot vertex "EMBEDDING")` directly under a
`(declare (type (simple-array single-float (*)) e))`, with a comment stating exactly why the
coercion must not return. The type declaration is the belt-and-braces I hoped for: a regression
to the generic path fails loudly instead of silently re-boxing.

**3.2 Normalisation as a data-semantics change — fixed, and more thoroughly than I asked.**
I recommended a documented precondition plus an ingest-time assertion. What landed is a full
migration path with an explicit policy variable, and the design note deserves quoting:

> `:MIGRATE` rewrites them in place (default). `:ERROR` refuses to open the store. There is
> deliberately no `:IGNORE` — scoring after Phase 1 is a bare dot product, so an unnormalised
> stored vector ranks WRONG rather than merely slow, and a silent wrong answer is the failure
> mode this guards.

That is the correct reading of the risk. Two further details show real care:

- **`migrate-embeddings` uses `gdb:copy` + `gdb:save`, not a raw `(setf (slot-value ...))`**, with
  a comment explaining that a bare slot-value setf bypasses the transaction write-set (populated
  only by `UPDATE-NODE`), and therefore gets no OCC validation and no replication/txn-log
  participation — *even though it would appear to work* via `close-graph`'s snapshot in the
  single-writer case. That is precisely the failure class behind `kraison/cl-llm#8`. Someone
  learned the lesson and wrote it down where the next person will find it.
- **`validate-chunks` normalises write-side**, closing the gap where a `store-add` after `hydrate`
  would otherwise never be migrated and would blow up at query time under the new type
  declaration — far from the call that caused it.

---

## 4. Smaller findings from Review 1: all addressed

| finding | outcome |
|---|---|
| 4.1 §8 oversells Phase 1 | spec reframed to match the plan's Scope Notes |
| 4.2 `:cache` retirement undecided | **exceeded** — concrete threshold: `:cache` stops being default at ~250k vectors (~1 GB at 1024-dim single-float), with a warn-once. See §5.1 below. |
| 4.3 int8 zero-point | adopted: symmetric, per-vector scale only, with the cross-terms rationale |
| 4.4 NaN/Inf unspecified | **exceeded** — see below |
| 4.5 dimension truncation | alignment guard added, with the "codec will outlive our memory of this plan" reasoning |
| 4.6 tie-break test order | **exceeded** — see below |
| 4.7 top-k O(k) crossover | noted |
| §5 chunk→entity edges | named as a hard Phase 3 prerequisite needing an owner |

**4.4 went further than I flagged.** `as-embedding` now carries *two* independent guards, because
SBCL (traps enabled) and ECL (traps disabled) fail differently: a `handler-case` on
`arithmetic-error` for the trapping path, plus `finite-single-float-p` for the silent path. The
ordering rationale is right and non-obvious — unordered `(= x x)` first, short-circuiting before
the ordered `<=` that would itself trap on a NaN operand — and the comment correctly notes that
self-equality alone is insufficient because **infinity is self-equal under IEEE 754**.

**4.6 found a deeper bug than mine.** I flagged that the brute-force reference used a different
comparator than the collector. They identified that the *original tie-break test never
discriminated at all*: a case whose correct answer is "reject the challenger" is also produced by
a plain score-only `>` (a tie never beats the incumbent under strict `>`), so it passed by
coincidence without consulting the tiebreak. The replacement forces a challenger that must **win**
on tiebreak, tested in both arrival orders. Plus new `k=0` and empty-corpus cases.

**Two bugs they caught in their own work**, both real and both the kind that survive review:
- `52d20cf` — `%needs-migration-p`'s norm check was a tautology: running the vector through
  `as-embedding` before measuring its norm renormalises it to ~1.0, so the check was always
  true-by-~1.0 and would have **silently skipped migrating every already-single-float,
  non-unit-norm vector forever.**
- `8517cd9` — `subtypep` on the element type was too permissive: it also admits adjustable,
  displaced and fill-pointered single-float vectors, which `%serialize-float-vector`'s own
  `(simple-array single-float (*))` declaration forbids. Now `typep` against the full type, so
  non-simple vectors correctly fall through to the generic branch instead of hitting a
  serialization error.

---

## 5. Remaining notes (none blocking)

**5.1 The `:cache` 250k threshold is documented but not implemented.** The spec commits to
`make-graph-store` warning once above ~250k vectors "until segments land"; no such warning exists
in `vivace/store.lisp` yet. Fine as a Phase 2 commitment — just flagging doc-ahead-of-code so it
doesn't get lost.

**5.2 `vertex->chunk` still calls `as-embedding` per surviving candidate.** Now redundant given
write-side enforcement, and the code says so at length, names the test that must be retired with
it (`CHUNK-VERTEX-COERCES-GENERAL-VECTOR-EMBEDDING`), and defers the removal explicitly. That is
the right call — the cost is one allocation and ~1 ULP of drift per *survivor* (k, not N).
Recording it only so the deferral doesn't become permanent by forgetting.

**5.3 Migration runs at open with no read-only escape.** Combined with §2, opening a legacy store
purely to inspect it triggers a long, memory-heavy write. `:error` at least lets you detect the
situation without writing. A `:defer` (open read-only, score nothing) would be a useful third
policy for tooling, but is not needed for the production path.

**5.4 Underflow edge case in `as-embedding`.** If a vector's components are finite but tiny enough
that the single-float norm underflows to 0, `(zerop norm)` treats it as a zero vector and returns
it unnormalised — a non-unit vector reaching storage through the guard. Vanishingly unlikely with
real embedders; noting for completeness since the whole point of the change is that non-unit
stored vectors rank wrong.

---

## 6. Summary

Phase 1 landed in good shape. The migration is the notable addition — it goes beyond what
Review 1 asked for and is correct, idempotent, and verified against a real legacy store. Its one
weakness is scale: **single-transaction, all-or-nothing, unresumable, unbounded in memory.** At
mine-action's current size (23k chunks, ~900 MB and ~26 s extrapolated) it is fine. At the 1M
vectors the design targets it is not, and the failure mode leaves the store permanently
unopenable rather than degraded.

Batching it is a contained change and makes it resumable for free, because `%needs-migration-p` is
already per-vector. I would do that before any store materially larger than mine-action's meets
this code.
