# Graphs as namespaces — parked design discussion

**Status:** PARKED, not built, not agreed as a commitment. This is a record of a design
conversation held 2026-07-29 between Kevin and Claude while brainstorming a
"SpatioTemporalGraphRAG" system spanning mine-action, vivace-graph-v3 and cl-llm. It was
parked deliberately so the app-side design could proceed; it is written down so the reasoning
survives.

**Origin:** the app needs to relate records that live in three separate graphs (mine-action ops
+ CAT-UXO munitions, forensics ACLED/DeepState/FIRMS, and the knowledge base). The VG 3.0
contract allows cross-graph *reads* and explicitly excludes cross-graph *write* transactions.
The question was whether to collapse the graphs, keep them separate and allow linking, or
something else.

## The core observation

"Multiple graphs" in VG today conflates two orthogonal things:

1. **Physical file partitioning** — separate data files per domain.
2. **Transactional and schema isolation** — separate `*graph*` machinery, separate transaction
   managers, separate snapshot clocks, separate type-id numbering spaces.

Nearly everything valuable about the current arrangement comes from (1). Nearly everything
painful comes from (2). The proposal is to keep (1) and drop (2): **a graph becomes a
namespace** — a name that points at a set of data files — while transactions, schema and the
snapshot clock become shared across the image.

## Why not simply collapse into one graph

Examined and rejected as the whole answer, though the case was closer than it first appeared.

Arguments for separation that did **not** survive scrutiny:

- *"Separate files mean more efficient querying within each domain."* Largely false in VG's
  architecture. Queries are already type-scoped (the spatial-index audit found all four
  index call sites type-filter). The measured costs are per-node and invariant to heap
  population — VG #86 (boxing in `point-in-ring-p`, ~87% of a representative workload's
  allocation) and VG #87 (~28 list rebuilds and ~532 generic dispatches per node
  materialized). Index depth is logarithmic. The one real effect is heap locality on bulk
  type scans, which is second-order and mitigated by the fact that bulk sources arrive in
  batches.
- *"Licensing forces the forensics boundary."* Investigated: ACLED/DeepState terms are
  attribution-plus-caution and derived products are fine. Attribution already travels
  per-record via the `attribution` slot on every forensics class, so the graph boundary was
  never what enforced it.

Arguments that **did** survive, and which the namespace design must therefore preserve:

- **Snapshot cost tracks write cadence.** `backup` (backup.lisp:120) is a full logical walk of
  every vertex and every edge; there is no incremental or differential path. A merged graph
  re-serializes 286k static ACLED events on every snapshot taken to capture ops changes.
  Per-namespace snapshot solves this.
- **Differential recovery policy.** Derivable data may be rebuilt unattended; non-derivable
  data must not be. This was exercised for real on 2026-07-20: an OOM dirtied both graphs, ops
  auto-rebuilt harmlessly because it is pipeline-derivable, and the knowledge graph correctly
  refused and required deliberate recovery. Note that ops is *already* a mix — sites,
  hazard-areas, nts-tasks, projects, persons, teams and survey-meta are authored and restored
  from JSON sidecars — so the whole-graph "ops is derivable" policy is an existing
  approximation that the app patches outside the engine.

## Why this is the safer engine investment

Both remaining paths need engine work, and they are not comparable in risk.

| | Keep separate, allow linking | Namespaces |
|---|---|---|
| Atomicity | 2PC across two WALs | one transaction manager |
| Endpoint identity | `node-id` → `(graph, node-id)`: an **on-disk format change**, one-way door | unchanged |
| Type-ids | stay per-graph; #53 stays latent | one global space: a **data migration**, replayable and reversible |
| Snapshot clock | two clocks, read-skew a reasoner can fire on | one clock |
| Failure mode | silently wrong data | "slower than expected" / "refused to start" |

A global type-id space is the *structural* fix for
[#53](https://github.com/kraison/vivace-graph/issues/53) rather than a patch. Today ops
type-id 3 is `admin-raion` and forensics type-id 3 is `acled-event`; independent numbering is
exactly why a node read under the wrong ambient `*graph*` materialized as the wrong class.

## Agreed shape

1. **A graph becomes a namespace.** Physical file partitioning kept; transactional and schema
   isolation dropped. One transaction manager, one snapshot clock, one global type-id space.

2. **Two restore modes, and they are different operations rather than one operation at two
   scopes.**
   - *Whole-system restore* = physical rewind of the log and all heaps together.
   - *Per-namespace restore* = **logical replay** — export and re-apply as new transactions
     against the running system, not a rewind. This sidesteps the shared-log consistency
     problem entirely (a rewind of one namespace would leave the shared log asserting a
     history that namespace no longer has). `backup` already produces exactly this, preserving
     `:id` and `:revision`, and logical replay is already the proven recovery path.
   - Confirmed acceptable by Kevin. If per-namespace *rewind* is ever wanted, transactions must
     carry a namespace touch-set and the operation must fail-closed when the sets are not
     disjoint since the snapshot point.

3. **Source namespaces are closed — no cross-namespace edges.** This is what keeps a
   per-namespace logical backup self-consistent. Without it, a namespace backup either
   contains edges referencing nodes that are not in the backup, or omits connectivity that
   dangles from the other side; either way the self-consistency that per-namespace snapshot
   exists to provide is lost.

4. **Cross-namespace edges exist only in a derived namespace.** Cross-source relations are
   reified as *claim nodes*; the edges to the endpoints hang off the claim; the claim lives in
   the derived namespace; therefore every cross-namespace edge lives in exactly one place by
   construction. The derived namespace is **wholly disposable** — a dangle after a partial
   restore is a signal, not a corruption, and the repair is regeneration.

5. **Operator assertions are source data, keyed by external identity.** Operators need to
   assert cross-boundary links directly, and an authored link cannot be regenerated from a
   rule — so a dangle there would be data loss rather than staleness. Resolved by keying on
   *external* identity rather than node id: `event-id-cnty` (ACLED's own), `source-id`
   (DeepState's own), `cat-uxo-id`, KB content checksums and `image-sha` are all immune to
   anything a graph can do to itself. A node id is a location; an external key is an identity.
   An assertion is therefore an **ops-namespace node** (operators work in ops, whichever way
   the link points) holding the far endpoint's namespace and external key, with only
   intra-namespace edges. The closure rule holds unbroken.

   Consequence: the two tiers collapse. Everything in the derived namespace is regenerable;
   only the *source* differs — a rule over source data for derived claims, a durable authored
   record for asserted ones. Neither is authoritative. No path to data loss: an ops backup
   carries the assertions complete, a derived backup carries materialisations nobody needs to
   trust, and restoring ops alone brings assertions back and regenerates the rest.

## Open items

- **Exclusive / detached bulk-load mode per namespace.** The largest unresolved piece. Under a
  single writer held by the live server, bulk ingest must run in-process — the 286k ACLED
  backfill ran in a dedicated sbcl with `MINE_ACTION_FORENSICS=false` precisely to avoid that.
  Splitting data files by namespace is what makes a detach/load/reattach path *possible*; a
  fully merged single graph has no seam to detach along. Cost: transactions touching a
  detached namespace must fail rather than block. Sharpened by the vector segment's capacity
  ceiling, whose failure lands inside the transaction APPLY path and leaves a persisted node
  with no segment entry — invisible to retrieval, with `store-count` still looking correct.
- **Inbound cross-namespace index lookup.** "Given this vertex, which claims touch it?" needs
  the edge index of a namespace other than the vertex's own. Simplest workable answer:
  consult every namespace's edge index (a handful of lookups at 3–4 namespaces, no bookkeeping
  to keep correct). A schema-level "namespaces that may hold edges of type T" hint narrows it
  later without introducing per-node state.
- **Identity-slot indexing audit.** Assertion resolution becomes an external-key lookup rather
  than a pointer chase, so `event-id-cnty` and its siblings must be `:unique` or at minimum
  `:index t` in their owning namespace. Unindexed, an assertion resolves by scan and the design
  is unusable.
- **Type-id renumbering migration** for existing graphs.
- Naming: v2 had a `namespaces.lisp` for RDF namespaces; the term is reused here in a different
  sense.
