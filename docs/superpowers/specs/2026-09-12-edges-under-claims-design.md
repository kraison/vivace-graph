# Edges under claims — the endpoint abstraction's second implementation

**Issue:** kraison/vivace-graph#367. **Depends on:** the namespaces epic
(#110, closed 2026-08-24), the claim record (#131) and endpoint
resolution (#132). **Engine baseline:** `experiment` at b787516.
**Date:** 2026-09-12. **Status:** approved by the owner 2026-09-12
(rulings R1–R6, sections 1–12); **amended the same day** after the
engine team's review (kraison/blackboard
`docs/superpowers/notes/2026-09-12-stores-vs-namespaces.md`): the
consumer that motivated cross-store continuation runs one store with
many namespaces, so R6 is reversed and U3 is deferred. **Corrected the
same day** on the engine review's three mechanical findings (#367
comment of 2026-09-12): §4.1 index visibility and class filter, §5 no
prune, §6.1–6.2 where adjacency is indexed, §11 to match. Written as
the handoff for planning and implementation; §13 is the build order.

---

## 1. What this is

A claim is a reified relation: a node carrying its subject and object as
`(namespace, external-key)` **values**, plus provenance — producer,
standing, validity extent, transaction extent. The substrate programme
(kraison/cl-llm `docs/superpowers/specs/2026-08-09-spatiotemporal-
substrate-programme-design.md`, §6.1) defined the endpoints as an
*interface with two implementations*: "today — (namespace,
external-key), resolved by lookup, no edges; after namespaces — real
edges to the endpoints, one snapshot clock, resolution by traversal",
with the invariant that the semantics are identical either way.

This document specifies the second implementation. Every precondition
§6.1 named is met: one image-level clock (#168), tagged ids with O(1)
endpoint resolution and the detached-read marker (#169), cross-store
edges as ordinary edges (namespaces design §7; #169, #208, #209),
packages as namespaces and store/namespace decoupling (#167), runtime
schema (#172). The claim record design's reason for "there are no
edges" (§2 of `2026-08-10-claim-record-endpoint-abstraction-design.md`:
a read-write transaction is single-graph, so an edge to a node in
another store could not be created) no longer holds — the guard at
`transactions.lisp:347,2920,2994,3017` protects touching a foreign
**node** in a read-write transaction; an edge that references a foreign
id never touches it (`tests/store-resolver-tests.lisp`,
`v5-cross-store-edge-is-visible`).

### 1.1 Boundary rule

Nothing here names a tenant's concepts. A decision justified only by
what one consumer needs belongs in that consumer.

### 1.2 What it buys

Native adjacency from a source through its claims and on to what they
relate it to, `traverse` over claim topology, and Prolog goals over it —
while every relation keeps the claim's producer, validity, supersession
and retraction. Consumers that want an attributed, versioned model with
graph-speed reads no longer choose between the claim layer and the
engine's edges.

---

## 2. Rulings

| # | Ruling | Why |
|---|---|---|
| **R1** | **Endpoint edges, not claim-as-edge.** The claim stays a node and gains one edge to each resolved endpoint: `subject-of` to its subject's node, `object-of` to its object's. | This is what §6.1 wrote. A unary claim keeps one edge, a binary two; identity tuples, unique constraints, indexes, temporal families and count indexes are untouched; a claim whose endpoint has no node stays key-only and legal. "The binary claim is the edge" would need one edge class per relation string, would split a family across the vertex and edge metaclasses, and would give every consumer's read path two implementations. A single-hop per-relation edge, if ever needed, is a derived materialisation on top of this. |
| **R2** | **Edges are derived; the key slots are the truth.** Linked at write time when resolution is possible, repaired and backfilled by an idempotent sweep. No link-on-source-arrival hook. | A hook would write into other stores after a source commits; measurement decides whether it is ever needed. |
| **R3** | **Retraction keeps the edges.** Edge-based reads filter by currency. | Nothing believed is erased; an as-of traversal must work through edges as `claims-touching :as-of` does through keys. |
| **R4** | **One shipped pair** of edge classes in `graph-db.spacetime`, no default store, placed per claim in the claim's store. | The claim record design refused shipped classes because `def-vertex` bound a class to one store; after #167 a class is instantiable in any store and a type may have no default store (`schema.lisp:339-402`, #167 R1; #172 R4). Keeps `def-claim-classes` from growing and gives every consumer the same functors. |
| **R5** | **Same-store auto-link in the constructor; caller-resolved endpoints for the rest.** No post-commit hooks. | `resolve-endpoint` refuses to run inside a read-write transaction (`spacetime/resolve.lisp:9`, design §4.1); a same-store `index-lookup` is legal there. Consumers keep working unchanged and opt in to cross-store links by resolving first, which the programme's §6.2 rule already asks of claim generation. |
| **R6** | **Cross-store traversal continuation is deferred** (amended 2026-09-12; it was in scope as its own unit). | The blackboard, the consumer that asked for it, keeps one durable store with a package per principal — the engine's own criterion for a store boundary is recovery policy, write cadence or detachability, none of which separates one agent from another (namespaces design §2, §4; the review note above). Cross-store adjacency is indexed in the edge's store and costs one probe per store that might hold such edges, a cost nothing here needs to pay. The gap the namespaces epic left as "#170+ work" (`traverse.lisp:67-70`) stays its own future unit; §8 records the design for it. |

---

## 3. The edge classes

```lisp
(graph-db:def-edge subject-of () () nil)   ; claim -> subject node
(graph-db:def-edge object-of  () () nil)   ; claim -> object node
```

in package `graph-db.spacetime`, exported. No slots of their own: the
claim carries everything; the edge is a resolved pointer. Direction is
claim → endpoint — the claim points at what it is about — so from a
source node its claims are the **incoming** edges, and a claim's two
outgoing edges are its endpoints.

**Placement.** `make-subject-of` / `make-object-of` are always called
with `:graph` = the claim's own graph. The claim and its edges commit
in one single-store transaction; an endpoint in another store is a
foreign id on the edge, an ordinary cross-store edge (namespaces §7).
The generated constructor refuses without `:graph`
(`default-store-not-open-error`, `schema.lisp:339`), which is the
intended contract for a class with no default store.

**Not identity.** No edge appears in any `def-unique` tuple or
`def-index` declaration. The `(namespace, key)` slots remain the claim's
identity and the complete index; an edge is a cache of one resolution.

**Weight** is unused and stays at the engine default.

**Keep-revisions** inherits the store default. An edge is versioned like
any node; an as-of read at an epoch before the edge existed does not see
it, which is correct for derived data.

**Version floor.** A store in which edges have been created carries the
two types in its `schema.dat`. An engine build without
`graph-db.spacetime`'s classes refuses to open it with
`schema-classes-not-loaded` (#144) — nothing is corrupt; load the
subsystem. The sweep (§5) is opt-in per store, so an operator chooses
when a store crosses that floor by running it — but write-time linking
(§4.1) is always on, so a store crosses the floor at its first
linkable write regardless. Harmless: a claim store already carries its
tenant's `def-claim-classes` types, which the same build must load
anyway.

---

## 4. The write path

### 4.1 Same-store auto-link

`def-claim-classes` wraps each arity's `make-<name>` already
(`spacetime/claim.lisp:333`, standing check). The wrapper gains, after
the claim node is built inside the caller's transaction:

For each endpoint — the subject, and for a binary claim the object:

1. `(namespace-sources namespace)` — the source classes registered for
   the endpoint's namespace (`spacetime/resolve.lisp`, #132). None:
   stop; the claim is key-only.
2. For each class whose identity-key index **exists in the
   transaction's graph** (`%require-index` answers; a class's default
   store is only a default after #167, so `source-facets-graph` is not
   the test): one `graph-db:index-lookup` on that slot for the key,
   **plus** the transaction's own uncommitted creates of that class
   (next paragraph). A class with no index in this store is skipped
   here (R5).
3. Exactly one distinct node across those classes: create the edge
   with `:from` the claim, `:to` the node, `:graph` the transaction's
   graph. Zero: no edge. More than one: no edge, and signal the
   `endpoint-link-skipped` warning (§7).

**Linking never fails a claim write.** Every refusal above leaves the
claim exactly as the first implementation would have written it.

**Same-transaction visibility.** `index-lookup` reads the persistent
secondary index, which is maintained in the commit *apply* phase
(`index.lisp:585-600`, "Maintenance (APPLY, post-durability)"), so it
cannot see a source created in the transaction that is still open. The
wrapper therefore also scans the transaction's own creates through the
commit view — `make-commit-view` / `view-writes`
(`value-constraint.lisp:151-179`), the overlay `claims-touching` uses
for the same problem (GH #324) — keeping any node of a candidate class
whose identity key is `string=` the endpoint's key. Index hits and
overlay hits are unioned by id before step 3's count. That is what
makes a source and a claim about it created in one transaction link.

### 4.2 Caller-resolved endpoints

`make-<name>` accepts two new keys, `:subject-node` and
`:object-node`, each a node the caller obtained with
`resolve-endpoint` **before** opening its transaction (programme §6.2:
"claim generation resolves its endpoints before opening its write
transaction"). For each one given:

1. Verify: the node's class is in `(namespace-sources namespace)` for
   the claim's namespace, and the value of that class's identity key
   slot on the node is `string=` the claim's key. Otherwise signal
   `endpoint-mismatch` (an `error`, §7) — the write fails, because a
   caller that hands the engine the wrong node has a bug.
2. Create the edge with `:to` the node's id and `:graph` the
   transaction's graph. The node may live in any store; its id is a
   foreign id on the edge.

A given `:subject-node` / `:object-node` takes precedence over §4.1's
lookup for that endpoint. Resolution is the caller's; verification is
the engine's.

### 4.3 Retraction, update, regeneration

`retract-claim` closes the transaction extent and touches no edge
(R3). An in-place update (`copy`, mutate, `save`) keeps the node id and
therefore its edges. A regeneration's sweep-then-insert deletes the old
claim node — the engine reaps a deleted node's edges through
`active-edge-p` — and the new node links at write.

---

## 5. The sweep

```lisp
(link-claim-endpoints graph &key family since limit)
  => (values linked unresolved ambiguous skipped-namespaces)
```

Idempotent. `family` names one claim parent class (default: every
registered family, `*claim-families*`). `since` is a transaction epoch:
only claims whose commit epoch is at or above it are visited, so an
operator can sweep exactly what a regeneration wrote. `limit` bounds
one call; the return says how far it got.

Two phases per batch, because `resolve-endpoint` cannot run inside a
write transaction and a write transaction must not cross stores:

1. **Read**, under `with-read-snapshot` on `graph`: collect the batch's
   claims, and for each endpoint that has no edge, `resolve-endpoint`.
   `resolve-endpoint`'s conditions are handled here: `unknown-
   namespace` and `unopened-source-graph` skip that namespace for the
   whole call and count it; `ambiguous-endpoint` counts the claim as
   ambiguous and moves on.
2. **Write**, one short transaction on `graph`: create the missing
   edges.

**The sweep does not prune.** A deleted source's edges are already
invisible to every edge-based read: `active-edge-p` (`edge.lisp:373`)
classifies the endpoint through `%active-endpoint-status` and treats
`:found`-but-deleted and `:missing` as inactive while keeping
`:detached`, `:unknown` and `:absent-in-store` live — the exact rule an
earlier draft of this section restated, and one `resolve-node-graph`
cannot express (it answers `:resolved` / `:detached` / `:unknown`,
never `:missing`). Reclaiming the space is `compact-edges`
(`edge.lisp:628`, `:policy :conservative` by default), which collects
soft-deleted and `:missing` endpoints and never `:detached`. An
operator who wants the edges gone after a regeneration runs it; the
sweep has one job, linking, and stays idempotent for that reason.

**Backfill** of an existing store is `(link-claim-endpoints graph)`.
Nothing runs implicitly: the engine never sweeps on open.

---

## 6. Reads

`claims-touching` is unchanged: key-indexed, complete, the truth. Every
edge-based read below sees **linked claims only**, and its docstring
says so.

### 6.1 Lisp

- `(claim-endpoints claim) => (values subject-node object-node)` — from
  the claim's outgoing `subject-of` / `object-of` edges; NIL for an
  endpoint that is not linked; a cross-store endpoint through
  `lookup-vertex-anywhere`, so it may be an `unresolved-node` marker.
- `(node-claims node &key family (role :either) current relation at
  during as-of) => list of claims` — from the node's incoming
  `subject-of` / `object-of` edges; the adjacency twin of
  `claims-touching`, with the same filters and the same meaning for
  each (`spacetime/claim-query.lisp:281`). `family` filters on the
  claim's parent class; default all. **Which store's adjacency:**
  an edge's in- and out-index entries live in the *edge's* store
  (`edge.lisp:171-173`), i.e. the claim's, never the endpoint's. So
  `node-claims` runs `map-edges :direction :in` on each claim family's
  graph (`family`, or every registered family) keyed by the node's id,
  not on `(node-graph node)`. In a one-store deployment the two are
  the same graph; the distinction is what the deferred §8 builds on.

### 6.2 `traverse`

Within a store, `traverse` already walks these edges: a source's
claims are its `:in` edges of type `subject-of` / `object-of`, and a
claim's endpoints are its `:out` edges. Nothing to add for the in-store
case. Two facts a caller must know: the source and the claims must be
in one store, for the reason §6.1 gives; and `traverse` collects
nothing without `:edge-type` — the result filter is `(typep edge
edge-type)` (`traverse.lisp:83`), so `(traverse source :edge-type
'(or subject-of object-of) ...)` is the form, as every existing
`traverse` test already passes one. Across stores: §8.

### 6.3 Prolog

`def-edge` installs `subject-of/2` and `object-of/2` (claim, node)
(`prolog-functors.lisp:1192`). `graph-db.spacetime` installs, the way
`def-global-prolog-functor` does:

- `related/3` — `(related ?subject ?relation ?object)`, nodes for the
  endpoints and a canonical relation string; solves through the
  subject's incoming `subject-of` edges, the claim's `relation` slot,
  and its outgoing `object-of` edge; **current claims only**
  (`claim-current-p`). Any argument may be unbound; the bound endpoint
  drives the scan. With both endpoints unbound it scans claims by the
  `claim-relation` index when the relation is bound and refuses (a
  guard error) when nothing is bound, as the guard already refuses an
  unbounded generator.
- `claimed/4` — `(claimed ?claim ?subject ?relation ?object)`, the same
  with the claim node exposed and **no currency filter**, for history
  and for reading provenance off the claim in the same query.

Both are global functors, visible under the guarded runner, effects
off, under its budgets.

---

## 7. Errors and conditions

| condition | kind | when |
|---|---|---|
| `endpoint-link-skipped` | `warning` | §4.1 found several candidate nodes; the claim is written unlinked. Carries the claim, the namespace, the key and the classes. |
| `endpoint-mismatch` | `error` | §4.2 was handed a node that is not a registered source of the namespace or whose key is not the claim's. The write fails. |
| `resolve-endpoint`'s own | as today | inside the sweep only; counted, never propagated out of `link-claim-endpoints`. |

A write-time link never signals an `error` for the derived edge; the
sweep never signals for what it could not do — its counts are the
report.

---

## 8. Cross-store continuation (deferred; recorded for the future unit)

**Deferred by the 2026-09-12 amendment (R6).** Nothing below is built
by #367. It is kept because it was reviewed and because the engine's
own gap is real; the unit that picks it up starts here. Within one
store, `traverse` and the functors of §6 already do everything this
section adds.

**`traverse`** (`traverse.lisp:37-113`) would continue into any *open*
store:

- The far endpoint of an edge is fetched with `lookup-vertex-anywhere`
  (already the case) and, when it is a `vertex` whose `node-graph` is
  another open store, its adjacency is walked with `map-edges` on
  **that** graph, not the starting one. The `(eq (node-graph
  to-vertex) graph)` gate at `traverse.lisp:72-75` becomes "is an open
  store".
- Uniqueness is by node id across stores (`equalp` on the byte
  vector); a node reached through two stores is visited once.
- Each store entered gets its own nested `with-read-snapshot` for the
  traversal's extent (composition per namespaces §6; `transactions.
  lisp:3439-3445`), so the reaper in every store touched holds — the
  named cost of a long cross-store read.
- A `:detached` endpoint still yields the `unresolved-node` marker in
  the results and is not walked; an `:unknown` endpoint is dropped, as
  today.
- `:as-of` across stores is meaningful only above the clock watermark
  (namespaces §6, "you cannot snapshot into the pre-migration past
  across stores"); below it the traversal refuses with `as-of-refused`
  rather than answer from incomparable epochs.

**Edge functors.** `%edge-functor/2` and `/3` (`prolog-functors.lisp:
1106`) resolve endpoints with `lookup-vertex` on the current graph, so
a cross-store endpoint unifies with nothing. The future unit switches
them to `lookup-vertex-anywhere`. `related/3` and `claimed/4` (§6.3)
use `lookup-vertex` on the claim's graph in #367; a caller-resolved
cross-store endpoint (§4.2) is linked but does not unify until then,
and §6.3's docstrings say so. A marker never unifies with a node
variable.

**Snapshot scope under the guarded runner.** `run-guarded-prolog`
takes one snapshot on its graph (`query/guard.lisp`). A store entered
mid-query gets the momentary pin inside `lookup-object` — per-store
consistency, not one instant across stores, which programme §6.2
already accepts for bundle assembly. Stated in the runner's docstring.

---

## 9. Measurement

The programme's §11 discipline: measure, do not guess. On the memory
tenant's own data (kraison/cl-llm memory, the largest claim store in
use), before and after linking:

1. **Write cost.** A regeneration sweep's claim writes per second,
   first implementation versus §4.1 linking on. Reported, not gated.
2. **Read benefit.** A two-hop neighbourhood — every object related to
   a subject through current claims, then every object related to
   those — as `related/3` under the guarded runner, versus
   `claims-touching` plus `resolve-endpoint` per hop. **Acceptance:**
   the edge path is faster; the ratio is recorded in the unit's issue.
3. **Sweep cost.** `link-claim-endpoints` over the whole store, wall
   time and counts, recorded once as the backfill baseline.

---

## 10. Consumers and compatibility

No store format changes. The claim API above the abstraction does not
change: `claims-touching`, `retract-claim`, the constructors' existing
keys, `claim-current-p`, the temporal reads.

- **kraison/cl-llm memory** (`recall`, currency, trace): unchanged;
  gains the fast path after one sweep of its store. Passing
  `:subject-node` / `:object-node` from its `conclude` is a cl-llm
  follow-up issue, not this unit's.
- **The second tenant** (private): unchanged.
- **kraison/blackboard** (#4, the structure tier): one durable store,
  a package per principal; resumes on top of U1 as structure plus
  claims with native traversal. Its note
  `docs/superpowers/notes/2026-09-12-stores-vs-namespaces.md` is the
  record of why.

---

## 11. Testing

Offline, in the subsystem's own suite (`graph-db/spacetime` tests),
temporary stores under one system directory:

- The shipped classes have no default store and are placed by
  `:graph` in any store; `make-subject-of` without `:graph` refuses.
- Same-store auto-link: a source and a binary claim about it, in one
  transaction and in two; both endpoints linked; `claim-endpoints`
  answers the nodes.
- Key-only: a claim about a namespace with no registered source is
  written, readable through `claims-touching`, absent from
  `node-claims`, and `claim-endpoints` answers NIL.
- Ambiguity at write: two candidate nodes → unlinked, the warning
  signalled once, the write committed.
- Caller-resolved: a claim in store A linked to a source in store B
  through `:object-node`; the mismatch refusal for a wrong node and for
  a wrong key.
- Retraction keeps both edges; `related/3` no longer answers;
  `claimed/4` still does; an as-of read before the retraction answers.
- Regeneration: the old claim's edges are gone after its delete; the
  new claim is linked.
- The sweep: idempotent (second run links nothing), links a claim
  written before its source, honours `:since`, counts every category,
  never signals, and does not prune: after a source is deleted its
  claim's edge is absent from `node-claims` and `related/3` through
  `active-edge-p`, still present to `map-edges :include-deleted-p`,
  and collected by `compact-edges`; a detached source's edge survives
  all three.
- Same-transaction visibility (§4.1): a source created and a claim
  about it written in one transaction link — the overlay, not the
  index, finds the source — and the same pair in two transactions
  links through the index.
- Traversal: within a store from source to source through a claim;
  a caller-resolved cross-store endpoint (§4.2) is linked, and a walk
  reaching it lands the far vertex (or the detached marker) in the
  results without walking past it — today's behaviour, pinned so the
  deferred unit knows what it changes.
- Functors: `subject-of/2`, `related/3`, `claimed/4` within a store
  under the guarded runner; a cross-store endpoint does not unify
  (pinned for the same reason).
- The existing spacetime suite and the cl-llm memory suite pass
  unchanged (the memory suite runs in cl-llm's CI against `experiment`
  HEAD).

---

## 12. Out of scope

Link on source arrival (R2); the claim as an edge and per-relation edge
types (R1); **cross-store traversal continuation and cross-store
unification in the functors (R6 as amended; §8 is its design record)**;
changing any consumer's write path (cl-llm follow-up); as-of across
stores below the watermark; running the sweep implicitly on open; a
spatial or vector index on the new edges.

---

## 13. Build order (the handoff)

| unit | contents | depends on |
|---|---|---|
| **U1** | §3 classes; §4 write path (auto-link, caller-resolved, conditions); §6 in-store reads and functors; §11's in-store tests | — |
| **U2** | §5 sweep and backfill; §9 measurement; §11's sweep tests | U1 |
| *U3 (deferred)* | §8 cross-store continuation in `traverse` and the functors, snapshot composition — its own future issue, not part of #367 | U1 |

#367 is U1 then U2. Each unit is one PR against
`experiment` with its own issue as a sub-issue of #367; CI runs the
full suite on push (`docs/ci.md`). Files touched: `spacetime/claim.lisp`
(classes, wrapper), a new `spacetime/link.lisp` (sweep), `spacetime/
claim-query.lisp` (`node-claims`, `claim-endpoints`), a new
`spacetime/functors.lisp` (`related/3`, `claimed/4`),
`spacetime/package.lisp`,
`docs/vivace-graph-v3-doc.org` (the spacetime chapter gains "Edges
under claims"), and this spec's Built notes as each unit lands.

## Built — U1 (#369, 8c7674e)

§3, §4.1–4.3, §6.1–6.3, §7 and §11's in-store bullets, on
`feat/edges-under-claims`. Two deviations from §13's file list, both
recorded here: the write-time linker lives in `spacetime/link.lisp`
(U2's sweep joins it) because it needs `namespace-sources`, which loads
after `claim.lisp`; and `node-claims` takes `:graph`, the store holding
the claims, defaulting to the node's own — §6.1's "each claim family's
graph" is not derivable, since a family is registered by class and a
class is instantiable in any store (#167). `claim-endpoints` takes
`:graph` too, defaulting to the claim's own store, for the same
reason. Unary claims are not solutions of `related/3` / `claimed/4`
(no object); `subject-of/2` reaches them. `claims-touching`'s filter
tail is now `%narrow-claims`, shared with `node-claims`. The
write-time linker's derived-link branch
runs under one handler that `log:warn`s a failed link (GH #369) and
never signals — the caller-verified path stays outside it, so
`endpoint-mismatch` still fails the write. `link.lisp` reads
`*namespace-sources*` directly, so an unregistered namespace (the
common case for existing families) costs no condition per write. An
unbound identity slot counts as "no key". `traverse` re-reaches the
start vertex along a back-edge under global uniqueness, pinned in
`tests/spacetime/endpoint-edge-tests.lisp`, as
`tests/traverse-tests.lisp` already documents.

## Built — U2 (#372, a109be8)

§5 and §9 on `feat/edges-under-claims-u2`. Deviations from §5's text:
the return gains a fifth value, `more-p`, because `:limit` bounds the
claims with a missing edge examined per call and a caller needs to
know whether to call again. `:limit` bounds both the work and the
scan: collection stops at the first claim with a missing edge past the
window, and `more-p` means exactly "such a claim exists beyond this
call's window" — not that progress remains, since permanently
unresolvable claims are re-collected every call. The documented
backfill loop is `(loop while (and more-p (plusp linked)))`; `:since`
is the cursor for a regeneration's writes. The write phase re-checks
each claim is still present, not deleted and still unlinked, which is
what makes SEQUENTIAL sweeps idempotent; two sweeps on one graph cannot
race, because `link-claim-endpoints` holds a per-graph lock for its
whole body (the re-check records no read when it finds no edge, so OCC
cannot validate it -- a phantom). A write-time link cannot race the
sweep on the same claim: write-time linking runs only as the claim is
created, and the sweep visits committed claims.
Added beyond §5: `*link-claims-at-write*` (default T), the switch §9's
measurement needs and a bulk loader wants. §9 was measured on a
synthetic store of the memory tenant's shape (`bench-claim-linking`,
`tests/perf/spacetime-bench.lisp`), not on the tenant's own data, which
lives in another system; the bench is the harness a tenant run reuses.
Measured: write cost ~0.4× throughput with linking on; sweep 8000
edges in ~3.2 s; two-hop read via the Lisp edge path ~0.4× and via
`related/3` under the guarded runner ~0.03× of `claims-touching` +
`resolve-endpoint` — §9's read acceptance is NOT met at
N=2000/M=4000/K=200. Hypothesis (engine follow-up): `active-edge-p`
validates both endpoints of every emitted edge. Full record on #372.

## 14. Traceability

Programme §6.1 (two implementations) → §1, R1. Programme §6.2 (resolve
before the write transaction) → §4.2, §5. Claim record design §2 ("no
edges") → §1, superseded by namespaces §7. Namespaces §6 (one clock,
composed snapshots, watermark) → §8. Namespaces §7 (cross-store edges
ordinary; detached marker) → §3, §5, §8. #172 R4 (no default store) →
R4, §3. #132 (`resolve-endpoint`, `namespace-sources`) → §4, §5.
