# Vector-segment growth past the mmap reservation

**Date:** 2026-07-22
**Status:** design, not yet implemented
**Affects:** `mmap.lisp`, `segment.lisp`, `transactions.lisp`, `globals.lisp`, `posix.lisp`
**Origin:** found while migrating mine-action's knowledge base to the `:segment` store. Not
hypothetical — the ceiling is reachable by the corpus growth that migration was done for.

---

## 1. The problem

Four findings share one cause: a reservation sized by a rule that was never aimed at a
corpus-sized file. §1.2 and §1.4 are the dangerous ones — both lose vectors silently.

### 1.1 A per-session growth ceiling

`open-vector-segment` maps its file with no explicit reservation:

```lisp
(let ((mmap (mmap-file path :create-p nil)))   ; segment.lisp:102
```

so it inherits `mmap-file`'s general default (`mmap.lisp:192-195`):

```
reserved = max(reservation || 8 × size, *mmap-min-reservation* (1 GiB), size)
```

computed **once, at open, from the file's size at that moment**. `%seg-grow` doubles capacity
(`segment.lisp:334-351`), so a session affords roughly **three doublings — 8× growth in slots**,
then `extend-mapped-file` signals (`mmap.lisp:253-258`).

Measured on mine-action's knowledge graph today:

| | |
| --- | --- |
| file | 134,742,080 B |
| capacity / live | 32,768 slots / 23,193 used |
| reservation (8 × file) | 1,077,936,640 B |
| grows that succeed | 32,768 → 65,536 → 131,072 → 262,144 |
| first grow that fails | 262,144 → 524,288 (needs 2,155,872,320 B) |
| **ceiling this session** | **262,144 chunks** |
| ceiling after a restart at that size | ~2,097,152 |

The ceiling self-heals across restarts, because reopening recomputes the reservation from the
now-larger file. So it is not a hard wall — it is a wall you hit once per session, roughly every
8× of growth.

The 8× rule is reasonable for the files it was written for: heap and index files sized by schema
and workload. The vector segment is the first mapped file whose size tracks the **corpus**, and
it inherited a default that was never aimed at it.

### 1.2 The dangerous part: the failure lands after durability

In `commit` (`transactions.lisp:2340-2368`) the order is:

1. `validate` / `validate-unique-constraints` / `validate-vector-segment-dimensions`
   — all pre-durability; a failure here aborts cleanly and nothing is journaled.
2. **`finalize-tx-persistence`** — the durable record now exists.
3. `apply-transaction` → `apply-tx-write-to-vector-segments` → `segment-put` → `%seg-claim-slot`
   → `%seg-grow` → `extend-mapped-file` → **error**.

So a reservation-exhaustion error fires at step 3, when the node write is already durable. The
result is a **chunk vertex that exists with no segment entry** — invisible to `vector-search`
permanently, while `store-count` (which counts vertices) still reports it. Silent, and not
self-correcting.

This class of bug was already understood here. `transactions.lisp:973-978` says so explicitly,
and it is exactly why the *dimension* check was deliberately placed before
`finalize-tx-persistence`:

> … with no corresponding segment entry. Checking here, under the same manager lock as
> VALIDATE-UNIQUE-CONSTRAINTS and before anything is journaled, makes the whole transaction —
> node write included — roll back cleanly on a mismatch.

Capacity was simply never given the same treatment. Dimension mismatch is checkable from the
write itself; capacity depends on runtime state, so it was easier to miss.

Aggravating factor downstream: mine-action's ingest loops wrap each document in `handler-case`
and continue, so a run that hits this does not halt — it proceeds, and reports a count.

### 1.3 Why it aborts rather than extending

Not an oversight. `extend-mapped-file` *does* extend — it maps more of the file into the
reserved tail with `MAP_FIXED`, at the same base. The reservation is the price of lock-free
reads (`mmap.lisp:13-20`):

> POINTER is fixed at the base of that window for the life of the mapping … Because POINTER
> never moves and the reservation is never unmapped until close, concurrent readers never fault
> and need no lock.

Growing past the window means moving the mapping, which invalidates every reader's base pointer.
With no read lock in the `mapped-file` layer, there is no safe moment to do that.

### 1.4 The rebuild path stalls at HALF the incremental ceiling

Found by asking "what is the manual recovery procedure?" — the answer exposed a worse problem
than the one being fixed.

`rebuild-vector-segment` drops the file and creates a fresh one with
`(create-vector-segment path (length v))` (`segment.lisp:475`) — dimension only, so
`initial-capacity` takes its default of **1024** (`segment.lisp:61`). A 1024-slot file is 4.2 MB,
so `8 × size` is negligible and the reservation lands on the **1 GiB floor**:

```
fresh file            4,210,752 B
reservation           1,073,741,824 B   (the floor)
doubling stalls at    131,072 slots     (262,144 would need 1,077,936,192 B)
```

So **a from-scratch rebuild cannot exceed 131,072 chunks** — half the incremental ceiling of
§1.1, because incremental growth benefits from a reservation computed against an already-large
file while a rebuild starts from nothing.

This is not merely a manual-recovery trap. `restore-vector-segments` calls
`rebuild-vector-segment` automatically whenever the clean-shutdown flag is unset — i.e. **after a
hard crash**. Above 131,072 chunks, automatic crash recovery cannot complete. And a hard crash is
not exotic here: mine-action recorded a heap-exhaustion OOM *during the shutdown snapshot* that
left both graphs dirty.

Sizing the rebuild's initial capacity to the live node count fixes this and is independently
worth doing: it also removes ~8 doubling-and-relocate passes from every rebuild.

### 1.5 A missing segment file is a silent no-op

`restore-vector-segments` (`graph.lisp:85-93`) wraps everything in `(when (probe-file path) …)`.
If the file is absent, it does **nothing** — no rebuild, no warning, no error. The class keeps its
`:vector-index` slot, the graph opens clean, and `vector-search` returns nothing for a corpus that
is entirely present in the vertices.

That makes "delete the segment file and let it rebuild" — the intuitive operator recovery — not a
slow path but a **silent no-op**. Combined with §1.4 it means neither obvious manual recovery is
safe.

### 1.6 This was already planned

`docs/mmap-remap-race-plan.md`, Phase 3 — the phase that shipped:

> LMDB-style: reserve a large virtual range once … grow with in-place `mremap` … **Handle
> reservation exhaustion (re-reserve + relocate under the write lock — rare).**

Phase 3 landed except for that final sentence. This design is that sentence, plus the safety net
that should exist regardless.

### 1.7 Why the segment is tractable when the heap is not

Relocation requires excluding readers. The `mapped-file` layer has **no read lock** — removing it
is what Phase 3 was for — so implementing relocate for the heap would reintroduce the cost the
phase existed to remove.

**The segment already has the lock.** Every public entry point is guarded by its per-segment
rw-lock: write-exclusive on `segment-put` (397) and `segment-remove` (680); shared on
`segment-get` (419), `segment-scan` (763), `segment-score-subset` (818). A write-exclusive
relocate is therefore safe *inside the segment layer today*, with no new locking anywhere else
and no cost to the heap.

---

### 1.8 What the manual procedure is today

There is no documented procedure. What exists:

**Restart — the only lever that works without touching internals, and it is usually enough.**
Reopening recomputes the reservation from the file's *current* size, so each restart buys roughly
another 8× of headroom. Growth is therefore: fill, restart, fill, restart. It is self-healing and
costs nothing, which is why §1.1's ceiling is a nuisance rather than a wall. **It is also
undocumented** — nothing tells an operator that restarting is the remedy, and the error message
(§ Part 5) points at a variable that does not exist.

**Raising the multiplier before open — works, but is internal-symbol surgery.**
`*mmap-reservation-multiplier*` and `*mmap-min-reservation*` are `defparameter`s
(`globals.lisp:92,94`) and are **not exported**. So the procedure is to
`(setf graph-db::*mmap-reservation-multiplier* 64)` before `open-graph`. Two caveats: it is
undocumented internal API, and it applies to **every mapped file in the graph** — heap, indexes,
linear hashes — not just the segment. That is harmless on 64-bit (reservations are `PROT_NONE` /
`MAP_NORESERVE`), but it is not the surgical change it looks like.

**Do NOT delete the segment file.** Per §1.5 that is a silent no-op, and per §1.4 even a
successfully triggered rebuild stalls at 131,072 slots.

Parts 2 and 6 below replace both levers with something supportable.

---

## 2. Design

Four parts. They are independent and land in this order deliberately: the cheap safety net first,
so that while the real fix is being built, hitting the wall costs an error message instead of
missing vectors.

### Part 1 — Pre-durability capacity validation (the safety net)

Add `validate-vector-segment-capacity (tx graph)` beside `validate-vector-segment-dimensions`,
called from the same manager-locked region **before `finalize-tx-persistence`**.

For each `(owner, slot)` key written by the transaction:

- count the **new** slots the transaction needs — writes whose node id is not already in the
  segment's `id->slot`, deduplicated within the transaction (mirroring the `intra` hash the
  dimension check already uses);
- required capacity = `live-count + new-slots`;
- simulate `%seg-grow`'s doubling from the current capacity until it reaches that; if the
  resulting `%seg-file-bytes` exceeds `m-reserved-size`, signal.

Notes:

- **Conservative on purpose.** Ignore the free list, so a transaction that would in fact reuse
  freed slots may abort slightly early. Aborting early is recoverable; aborting late is not.
- A `(owner, slot)` with no committed segment cannot exhaust — creation sizes the file.
- Reads `segment-capacity` / `segment-live-count` directly, without the segment's rw-lock, which
  is consistent with the "lock at public boundaries only" rule: this runs under the manager lock,
  and `apply-transaction` (the only mutator) runs under it too, so no concurrent mutation exists.
- Signal a **distinct condition type** (e.g. `vector-segment-capacity-exhausted`) carrying the
  owner, slot, required capacity, and reservation. Callers should be able to distinguish "grow
  the reservation and retry" from a genuine data error.

**This part alone closes the data-integrity hole.** Everything below is about not hitting it.

### Part 2 — An explicit reservation floor for segments

Pass an explicit `:reservation` from `open-vector-segment` and `create-vector-segment`, from a new
`*segment-min-reservation*` in `globals.lisp`.

The reservation is `PROT_NONE` + `MAP_NORESERVE` anonymous address space: no RAM, no disk, no
commit charge. On 64-bit hosts a large floor is genuinely free.

Make it a configurable global rather than a hardcoded constant — deployments differ, and a
reservation policy that is free on a 192 GB server is not obviously free everywhere. But there is
no longer a constrained in-process consumer to design around: the only live consumers are 64-bit
hosts (see §6), so a generous default is appropriate.

### Part 3 — Adjacent re-reservation (an opportunistic saving, not the fix)

On exhaustion, before giving up, try to claim the address range **immediately after** the current
window:

```
mmap(base + reserved, additional, PROT_NONE,
     MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED_NOREPLACE, -1, 0)
```

If it returns the requested address, the window simply got bigger: `m-pointer` never moved, no
reader is affected, no lock is needed beyond what the caller already holds.

⚠ **Correction (2026-07-22, measured — this section previously claimed "on a sparse 64-bit
address space this usually succeeds", which is false for the shape that matters.)** With a 16 GiB
segment reservation, adjacent claims of 1 page, 1 MiB, 1 GiB and 8 GiB were **all refused** — on
Linux 5.15 *and* 4.15. The reason is that top-down `mmap` allocation packs each new window flush
against existing mappings: on both hosts `libssl.so.3` began at the reservation's exact end.
Legacy bottom-up layout and Darwin behave the same way. A large reservation is precisely the case
where there is nothing adjacent to claim.

So **Part 3 is an opportunistic saving, not the mechanism.** Part 4's relocation remains the
workhorse, and the lever that actually prevents relocation is Part 2's reservation floor — making
exhaustion rare — not Part 3. Part 3 is still worth having: a miss costs one extra `mmap` on an
already-rare path, and it wins whenever a segment happens to sit at the top of the layout.

**Correction (2026-07-22, measured — an earlier revision of this section was wrong).**

This section previously claimed that a kernel ignoring `MAP_FIXED_NOREPLACE` would "silently
succeed by replacing" the occupied range, making Part 3 dangerous on old kernels. **That is not
what happens**, and it was asserted rather than tested.

Measured with a C probe on two hosts (map a page, write a sentinel, try to reclaim the same
address with the flag, inspect the sentinel):

| host | kernel | result |
| --- | --- | --- |
| hypnos | 5.15.0-179 | **honoured** — rejected with `EEXIST`, sentinel intact |
| odm | 4.15.0-213 | **ignored → advisory hint placement**: mapping landed at a *different* address, sentinel **intact** |

An unknown mmap flag is ignored, which leaves the address argument as a *hint* — it does **not**
imply `MAP_FIXED`. Nothing is clobbered. So:

- **The safety property is a post-hoc address comparison**, and it is complete on every kernel:
  claim the range, and if the returned address is not exactly the one requested, `munmap` it and
  fall back. No kernel-version gate is required.
- Never pass plain `MAP_FIXED` as a fallback. *That* would clobber — but nothing requires it.
- On a kernel without the flag Part 3 is merely **useless** (the claim usually lands elsewhere and
  is unwound), not dangerous. That is why odm cannot exercise it, and why the flag is still worth
  passing where available: it turns a wasted map-then-unmap into a clean rejection.
- macOS has no `MAP_FIXED_NOREPLACE`; the address check alone still makes the attempt safe there,
  just less efficient.

**Recommendation:** implement Part 3 unconditionally, guarded by the address check rather than by
platform or kernel version. Pass `MAP_FIXED_NOREPLACE` where the constant is available so the
kernel rejects cleanly instead of placing elsewhere; where it is not, the address check still
makes the attempt safe. Part 4 remains the fallback whenever the adjacent range cannot be
claimed.

### Part 4 — Re-reserve and relocate, under the segment's write lock

The documented Phase-3 fallback, for when Part 3 cannot obtain adjacent space.

Under the segment's **existing** write lock:

1. reserve a new, larger `PROT_NONE` window anywhere;
2. `MAP_FIXED`-map the file into its head;
3. update `m-pointer` and `m-reserved-size`;
4. `munmap` the old window.

Safe because every segment reader holds the shared side of the same lock.

⚠ **This primitive must be segment-scoped, not a general `mapped-file` capability.** Anything that
moves `m-pointer` is only safe for a subsystem that can exclude its own readers. The heap and the
linear hash cannot. Name and document it so that constraint is unmissable — e.g.
`relocate-mapped-file` with a docstring stating that the caller must hold write-exclusive access
over every reader of that mapping, and a note that the heap does not qualify.

### Part 5 (trivial) — the error message names a variable that does not exist

`mmap.lisp:256` tells the operator to *"Raise `*mmap-reservation-size*`"*. No such variable exists
— the knobs are `*mmap-reservation-multiplier*` and `*mmap-min-reservation*` (and, after Part 2,
`*segment-min-reservation*`). The stale name also appears in the `mapped-file` docstring
(`mmap.lisp:15`). This is the message someone reads during an incident; fix it.

### Part 6 — Make rebuild and recovery survivable (fixes §1.4 and §1.5)

Two small changes, but they close the worst hole in the set: an automatic crash-recovery path that
cannot complete above 131,072 chunks.

**6a. Size a rebuild's capacity to the corpus.** `rebuild-vector-segment` currently calls
`(create-vector-segment path (length v))`, taking the 1024 default. Count the live conforming
nodes for the (owner, slot) first — the rebuild already sweeps `map-vertices`, so the count is
available — and pass it as `:initial-capacity`, with headroom. Effects:

- the fresh file is corpus-sized, so `8 × size` clears the 1 GiB floor and the rebuild no longer
  stalls at 131,072;
- ~8 doubling-and-relocate passes disappear from every rebuild, so it is also materially faster;
- the created segment's reservation is derived from a realistic size, which is the same principle
  as Part 2.

**6b. A missing segment file must not be a silent no-op.** `restore-vector-segments` skips
entirely when `probe-file` fails. It should rebuild — the vertices are present and authoritative,
so a missing derived index is exactly the case rebuild exists for. At an absolute minimum it must
**warn loudly**; opening clean with a permanently empty vector index and no diagnostic is the
worst available behaviour.

Also pass an explicit `:reservation` at **create** time, not only at open (Part 2 as written
covers `open-vector-segment`; `create-vector-segment` needs it too, or a fresh segment starts life
on the general default again).

---

## 3. Testing

The invariant under test is: **after any capacity failure, the graph's vertices and the segment
agree.** That is what is currently violated.

- **Part 1:** open a segment with a deliberately tiny reservation, commit a transaction that
  would overflow it, assert (a) the transaction aborts, (b) the node is **not** persisted —
  re-read after the abort, and (c) `live-count` is unchanged. The negative — that nothing was
  journaled — is the point; asserting only that an error was signalled would pass against the
  current broken behaviour.
- **Part 2:** assert the reservation actually applied is the configured floor, on both the open
  and the create path.
- **Part 3:** force exhaustion with a small reservation, assert growth succeeds and
  `m-pointer` is **unchanged**. Separately assert that attempting to claim an occupied adjacent
  range does not replace it — construct the collision deliberately.
- **Part 4:** assert `m-pointer` changed, data survives the move intact, and a reader blocked on
  the lock during relocation sees consistent data after. Run under the existing concurrency
  harness on SBCL and CCL.
- **Part 6a:** build a segment past 131,072 entries, mark it dirty (simulate a hard crash), reopen,
  and assert the automatic rebuild **completes** with `live-count == node count`. Against today's
  code this test fails — that is the point of writing it.
- **Part 6b:** delete the segment file, reopen, and assert the index is rebuilt (or, at minimum,
  that a warning is emitted) rather than the graph opening clean with an empty vector index.
- **Regression:** a full ingest crossing at least two doublings, ending with
  `live-count == store-count`.

---

## 4. Sequencing

1. **Part 1 + Part 6 + Part 5** — small, no locking changes, no platform risk. Part 1 converts
   silent vector loss into a clean abort; Part 6 makes crash recovery possible above 131k chunks
   and stops a missing index file from passing unnoticed. Land before any large ingest.
2. **Part 2** — one argument plus a configurable global; pushes the ceiling out of practical range
   on servers, and replaces the undocumented `setf graph-db::*mmap-reservation-multiplier*` lever
   of §1.8. Consider exporting the knobs, or documenting the restart remedy, so §1.8 stops being
   folklore.
3. **Part 3** — an opportunistic saving, not the fix; land alongside Part 4. Both premises this
   item originally rested on turned out false: there is no `MAP_FIXED` clobber hazard to budget
   for (an ignored `MAP_FIXED_NOREPLACE` falls back to hint placement, never eviction — the
   address check alone makes the attempt safe on every platform), and adjacent space is usually
   NOT available, so this is not "the real fix" — Part 2's reservation floor is what actually keeps
   exhaustion rare, and Part 4's relocation is the workhorse that catches what Part 3 misses.
4. **Part 4** — fallback, and the completion of the Phase-3 plan.

Parts 1, 2 and 6 are worth landing on their own even if 3 and 4 never happen. **Part 6 is
arguably the most urgent of all of them**: §1.2 needs 262k chunks to bite, but §1.4 needs only
131k *and* a hard crash — and a hard crash during the shutdown snapshot has already happened once
on the dev hub.

## 5. Out of scope

- **Doing any of this for the heap or linear hash.** They share the ceiling but not the rw-lock,
  so relocation is not available to them; raising their reservations is a separate decision with
  different trade-offs.
- **Multi-extent indirection** (an extent table with per-access address lookup). It would remove
  the ceiling entirely, but puts a branch and an indirection into `%get-byte` — the hot path for
  every uint64 decode — which is precisely the cost Phase 3 was designed to eliminate.
- **ANN indexing.** Unrelated; `segment-score-subset` already provides that seam.

## 6. Deployment note

vivace-graph's live consumers are mine-action's two hosts: a macOS dev hub and odm (Linux, 192 GB).
Both are 64-bit, which is what makes Part 2's generous reservation floor uncontroversial and Part
3's Linux-first scope acceptable.

The Android field app is **not** a consumer of this code. It no longer embeds vivace-graph — the
ECL port was abandoned as too unstable, and the device now runs a **SQLite-based replication
peer** instead. So nothing here needs an ECL or mobile release gate. ECL remains a supported
implementation in this repo, but with no live consumer exercising it, do not spend effort making
Parts 3 or 4 work there; if an ECL path is awkward, skip it and fall through, as with macOS.
