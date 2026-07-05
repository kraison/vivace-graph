# Peer replication — B3-2 edge conflicts: decision needed

**Status:** B3-1 (durable conflict enumeration API) is shipped on branch
`peer-replication`. Edge conflicts (B3-2) are the remaining B3 piece, and the design
explicitly tags them as *"needs app decisions."* This doc lays out the specific
mine-action decisions the engine can't make on its own, plus three build options and a
recommendation, so the app side can review and pick a direction.

Audience: the mine-action app side + engine. Written 2026-07-01.

---

## Where we are

**B3-1 (done)** handles **vertex field** conflicts through the per-field resolver:

- `:lww` — per-field last-writer-wins by `(lamport, origin)`.
- `:safety` — binary "dangerous unless safe": a release (dangerous→safe) is rejected
  (keep dangerous + surface); a re-open toward danger auto-applies.
- `:geometry` / `:safety-surface` — divergence surfaces (keep local, record the loser);
  geometry equality is `equalp` (the locked Q1 decision).

Every surfaced conflict is **durably retained** (loser value + origin + Lamport +
causing op-id), **idempotent**, **enumerable/filterable**, and **resolvable** — the app
review surface reads it. This is also how the "loser doesn't converge via pull" gap is
closed by design: resolution is human review of retained losers, not silent
auto-convergence.

**Edges are the gap.** In this engine edges are UUID-keyed nodes. Creation merges by
**union** (two replicas creating edges never conflict). So the *only* edge conflict is
**removal** — and the locked note says removal of a **safety-bearing edge** must
**surface**, while every other edge is plain remove-wins.

Safety-bearing edges (codified app-side in `*safety-bearing-edges*`):

- `site-has-hazard-area`
- `zone-of`
- `nts-task-of`
- `find-observed-in`
- `find-at-site`

Everything else = plain remove-wins (today's behavior, unchanged).

Note: "remove a find" is **not** an edge removal — it is the `eo-find.deleted`
**field** (`:safety`), already handled by the B3-1 vertex resolver. So find deletion is
not part of this decision.

---

## The app decisions the engine can't make for you

These are why the design tagged edges "needs app decisions." Each needs a mine-action
answer.

### Decision 1 — Always-surface, or only-on-contention?

- **Always:** removing a safety-bearing edge *always* records a conflict (a human
  confirms every safety-relationship deletion). Simple; needs no concurrency detection.
- **Only on contention:** surface only when the removal races a concurrent
  re-assert/keep on another replica. Matches the "surface-on-divergence" model, but
  edges have no per-field stamp or base-state, so the engine would need a **new
  edge-level tombstone/stamp** to detect "concurrent." More machinery.

### Decision 2 — Surface-and-keep, or surface-and-remove?

- **Surface-and-keep (fail-safe):** don't drop a safety relationship without sign-off —
  keep the edge live, record the removal as a pending conflict. Consistent with the
  vertex `:safety` release direction (keep the dangerous state, surface).
- **Surface-and-remove:** apply the removal, record it for post-hoc audit. Less
  fail-safe, but the graph converges immediately.

### Decision 3 — `find-of-type` multiplicity (the `:safety-surface` case)

Today, reclassifying a find leaves it carrying **both** type edges (union keeps both).
Two ways to resolve:

- **Keep as edges + surface:** surface "this find has 2 conflicting type edges — pick
  one" for human resolution.
- **PT-7.1 refactor:** make `find-of-type` a **vertex-reference slot** instead of an
  edge, so it becomes an ordinary `:safety-surface` field merge (diverge → surface +
  keep local) with no edge machinery at all. This is the app-side change flagged as
  "Kevin's-call-on-timing."

---

## The three build options

### Option A — Build the edge-removal seam now (engine mechanism, app supplies the list)

Add an app seam analogous to `disclosable-p` / `merge-policy`: a predicate the engine
consults on every re-homed/pulled edge **delete**. If the edge type is safety-bearing →
record a conflict (and, per Decision 2, keep or remove). Everything else → plain
remove-wins (today's behavior). Defers `find-of-type` multiplicity to PT-7.1.

- **Pro:** delivers the safety-critical half; the app just wires its
  `*safety-bearing-edges*` list into the seam. Small, testable engine change.
- **Con:** needs Decisions 1 and 2 pinned first. Leaves `find-of-type` multiplicity for
  later.

### Option B — Do the PT-7.1 refactor first, then the removal seam

Convert `find-of-type` from an edge to a vertex-reference `:safety-surface` slot
app-side, so it folds into the existing field resolver with zero new edge code. Then add
the removal seam for the remaining structural safety-bearing edges.

- **Pro:** unifies the model — one conflict path (fields), fewer moving parts long-term.
- **Con:** more app-side surgery (schema + ETL + any queries over `find-of-type` edges),
  and it's a data migration on existing finds.

### Option C — Pause B3 at B3-1 (shipped) and coordinate first

The conflict API is the foundation the review surface needs and is already in. Stop
here, settle Decisions 1–3 on the app side, then pick A or B.

---

## Recommendation

**Option A, with Decision 1 = always-surface and Decision 2 = surface-and-keep.**

Rationale: for safety data the fail-safe default is "never silently drop a safety
relationship, and always leave a human-visible record." Always-surface avoids building
edge-concurrency detection you may not need, and surface-and-keep matches how the vertex
`:safety` bucket already behaves (keep the dangerous state, surface the change).
`find-of-type` / PT-7.1 is a separate, heavier app decision that shouldn't gate the
structural-edge safety work.

---

## What the engine will build once decided (Option A sketch)

- An app seam on the peer-graph (alongside `merge-policy`): e.g.
  `safety-bearing-edge-p (edge-type)` supplied by the app from `*safety-bearing-edges*`.
- In the re-home and pull-apply paths, an edge `tx-delete` whose edge type satisfies the
  predicate records a `peer-conflict` (bucket `:safety-edge`) via the existing B3-1
  store, and — per Decision 2 — either keeps the edge live or applies the delete.
- Reuses the B3-1 conflict record wholesale (node-id = edge id, kept/loser = presence),
  so the app review surface enumerates edge conflicts exactly like field conflicts.

Send back the answers to Decisions 1–3 (or just "Option A, always-surface,
surface-and-keep") and the engine side builds it.
