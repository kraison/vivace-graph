# Graphs as namespaces — decisions of 2026-08-20

**Status:** AGREED with Kevin in session, not yet built. This document records
decisions that **amend** `namespace-design-discussion.md` (the parked 2026-07-29
record). Where the two disagree, this one is current.

Read the parked doc first for the arguments that still hold — why collapsing to one
graph was rejected, why namespaces beat cross-graph edges, the restore-mode split.
This file records only what changed and what is newly decided.

## What is superseded

- **The two-axis framing.** The parked doc says "multiple graphs" conflates *physical
  file partitioning* with *transactional and schema isolation* — keep the first, drop
  the second. There are **three** axes, not two, and the third was folded into the one
  thrown away:
  1. Physical file partitioning — **keep** (snapshot cost tracks write cadence;
     differential recovery policy).
  2. Transactional isolation — **drop** (one transaction manager, one snapshot clock,
     one global type-id space).
  3. **Ontological scoping** — **keep**, and give it its own mechanism (packages).
     The parked doc had no name for this; it is Kevin's stated second reason for
     multiple graphs ("a graph about fish species shouldn't talk about primates").

- **Agreed-shape point 3, "source namespaces are closed — no cross-namespace edges."**
  Demoted from engine invariant to **convention**. See D2.

## Decisions

### D1 — Ontological separation is *expressed*, not *enforced*

Packages are the load-bearing mechanism. The engine does not refuse a cross-namespace
reference; it makes one lexically obvious. `fish::species` and `primate::species` are
distinct classes that may legitimately share a symbol-name.

Consequences:
- Cross-namespace edges are **ordinary edges** — no declaration mechanism, no checked
  exception, no per-edge-type permission list.
- The S4 ontology constraints (#109 and its units — domain/range #156, disjointness
  #157) become **opt-in semantics**, not the enforcement backbone.
- Packages express; they do not enforce. `::` bypasses a package, and nothing stops a
  node in one namespace holding an id from another. Anyone adopting packages believing
  they enforce will skip building the thing that does.

### D2 — The closure rule is a convention, and `backup` needs a policy

Under D1 an operator can create an edge from one store into another, so a per-store
logical backup can contain an edge whose far endpoint is not in the backup. This was
previously prevented; it is now merely discouraged.

**Open decision, not yet made:** whether `backup` includes such an edge dangling,
omits it, or includes-and-warns. Leaning include-and-warn — a dangling edge on restore
is diagnosable, whereas silent omission loses data with no signal.

### D3 — Store and namespace are decoupled; many namespaces per store

A **store** is a set of mmap'd files (heap, vertex/edge tables, indexes, txn-log) and
is the unit of snapshot, backup, recovery policy and detach/attach. A **namespace** is
a package: a set of symbols, and the unit of naming, ontological scoping and runtime
schema definition.

One-to-one makes each axis pay the other's costs: separating two ontologies would force
two heaps and two snapshot units, and partitioning one dataset by year for snapshot cost
would force two ontologies for what are the same classes.

**Expected shape: many small namespaces, few stores.** Stores are chosen for write
cadence and recovery policy; namespaces are chosen for meaning.

**No syntax change.** `def-node-type` already records `:package (package-name *package*)`
into the persisted `node-type` meta (`schema.lisp`) — every consumer already captures it
at every definition site; nothing consumes it yet. So the package comes from `*package*`
at macroexpansion and the existing trailing argument stays the **store**:

```lisp
(in-package :fish)
(def-vertex species () ((latin-name :type string)) :taxonomy)
```

Existing code — one package, one graph name — keeps working unchanged and *is* the
one-to-one case. Decoupling is purely additive: you get it by writing a second
`in-package`, never by accident.

### D4 — A class is *instantiable in any store*, not *placed in one*

Placement is a property of the node, not the class. This is the version of D3 that
matters, because it is what cl-llm#20 asks for: one chunk class, two stores.

Consequences:
- **`%check-node-class-graph-unique` (`schema.lisp`) is deleted, not scoped.** It exists
  only because type-ids were per-graph — ops type-id 3 being `admin-raion` while
  forensics type-id 3 is `acled-event` is what made a name collision dangerous. With a
  global type-id space, type-id 3 means the same class everywhere and the check has no
  remaining job. The residual hazard (reading a node under the wrong store, hitting the
  wrong heap offset) is a different bug, already closed by the 3.0 contract having nodes
  carry their home graph.
- `*schema-node-metadata*`'s keying by graph-name goes away; the registry becomes global
  and each store instantiates the types it holds. A store carrying entries for types it
  does not use costs essentially nothing — the type-index is already dense across all
  65536 slots at 17 bytes each (~1.1 MB per store) regardless.

### D5 — The endpoint resolver is a tagged UUIDv8, not a v5 namespace

**A v5 namespace cannot be recovered.** It is a hash *input*: `SHA1(namespace || name)`,
one-way. The usual escape — recompute and compare against a candidate namespace — is
also closed here, because `generate-uuid-name` (`node-id.lisp:12`) builds the name from
time-of-day plus 32 random bytes and **that name is never stored**. What v5 buys is
collision-domain separation at generation time, which VG already uses that way
(`*vertex-namespace*` / `*edge-namespace*`, `globals.lisp:222-224`) — and note that even
there VG never recovers "vertex or edge?" from an id; it knows from which table it read.

Making the v5 namespace per-store would give clean separation and buy **nothing** for
resolution.

What works instead: **put the store in the id as a field.** A UUID is 128 bits, of which
v5 fixes only 6 (4 version + 2 variant). Reserve a store field from the remaining 122:

- **RFC 9562 (2024) defines UUIDv8 for vendor-defined layouts** — everything but the
  version and variant bits is ours. New ids are v8 with a store field plus random fill;
  old ids stay v5.
- The resolver reads the version nibble: **8 → mask and index a small vector of open
  stores, O(1); 5 → fall back to the per-store scan.** No flag day, no rewrite of
  existing claims, an honest fast path that widens as data turns over.
- v8 is the only honest way to do this. Stealing bits from a v5 in place produces ids
  that claim to be v5 and are not; nothing in VG validates that today (it cannot, with
  no stored name), but quietly lying in the version field will bite someone later.
- Field width: stores stay few, so 8–12 bits is ample and keeps more entropy. Namespaces
  may proliferate into the hundreds and **never touch the id at all** — the resolver's
  job is "where is the data", which is the store; the class comes from the global
  type-id.

Consequences:
- **The tag must be a stable numeric store-id**, assigned once from a registry and never
  reused, so renaming a store is free.
- **A node's store becomes immutable for life** — the id is the key in the vertex table,
  both ve-indexes, the vev-index and the type-index, and cannot be rewritten in place.
  "Move this node to another store" becomes copy-and-delete with a new id. Note the
  interaction with cl-llm#20: re-homing a *class* does not re-home existing *nodes*.
- **Detach/attach gets a better failure signal.** A tagged edge into a detached store
  resolves to "store 7, currently unavailable" rather than to nothing; the scan-based
  resolver can only say "not found anywhere" and cannot distinguish a missing node from
  an absent store.

### D6 — Runtime schema is persisted as metadata, never as source

Restart must never `load` Lisp that an LLM wrote. Metadata is diffable, versionable,
migratable and safe; source is an arbitrary-code-execution surface where a data surface
will do, and it has a bootstrap ordering problem (the package must exist before the file
defining it can be read).

**The ingredients are already persisted and nothing consumes them.** `node-type` carries
`name`, `parent-type`, `slots` (full slot-specs), `package` and `constructor` — every
ingredient for a `defclass`. But `instantiate-node-type` (`schema.lisp:499`) calls
`(find-class (node-type-name meta) nil)` and assumes the class already exists, because
`def-node-type` expanded to a `defclass` at load time. **VG cannot currently rebuild a
class from disk.** A real class-from-metadata path is needed; it is much closer than it
looks.

**The boundary: a runtime-defined type may declare structure; it may not ship behaviour.**
Slots, types, indexes, unique constraints and value constraints are data. But the
spacetime source-onboarding facets include function-valued entries (`:precision-fn`,
`:confidence-fn`, `:method-fn`) and a closure cannot be cl-stored. A runtime-defined
source that needs a derivation function **names a pre-registered one** rather than
supplying a lambda. Invariant: *restart never evaluates data.*

This is the strongest argument for packages — stronger than ontological separation. A
runtime-defined type needs a home for its symbol; without packages you need name-mangling
(`fish/species`) leaking into every user-facing name, and with them `intern` in the
namespace's package is exactly the primitive for materialising a persisted type meta into
a live class.

### D7 — Placement is a default per class, overridable at the write

`def-vertex` / `def-edge` keep their trailing argument, which becomes the class's
**default store** rather than a binding. Any individual write may override it.

Chosen over the alternatives — explicit at every write (a footgun: omit it and you
silently get `*graph*`), default per namespace, or a placement *rule* function. The rule
function was tempting for the memory case ("derived goes to the disposable store") but
placement determines recovery policy, so a bug in a placement rule is a bug that quietly
loses data at the next unattended rebuild. Placement stays visible at the call site.

Exactly backward compatible: every existing `def-vertex` keeps its trailing graph name,
every existing call site writes where it always did, and nothing in mine-action or odm
changes behaviour. It also makes cl-llm#20's fix a call-site change rather than a
redefinition — the chunk class keeps its default store and the second store is reached by
passing `:graph`.

**Corollary — this answers "whose store holds a cross-namespace edge" with no special
case.** An edge is a node; it is placed by D7 like any other. Not the `from` endpoint's
store, not the `to` endpoint's — its own class default, overridable. So a derived
`descends-from` claim defaults to the disposable store while an authored operator
assertion defaults to the durable one, and each lands where its *policy* says it should
rather than where its endpoints happen to live.

**Corollary — D7 supplies the index hint the parked design wanted, for free.** The parked
design proposed consulting every namespace's edge index, narrowed later by a schema-level
"namespaces that may hold edges of type T" hint. Under decoupling, indexes live in stores
and stores stay few, so the unhinted sweep is a handful of lookups — and the edge class's
default store *is* the hint. Since a write may override placement, the hint can be stale,
so the exact form should be a small per-edge-class **store-occupancy set** maintained on
write: the lookup consults only the stores that class has actually occupied. It fails
safe — a lost or stale set costs a wasted lookup, never a wrong answer, and the fallback
is the full sweep.

## Newly identified constraints

- **Two global monotonic id spaces with no reclamation.** `type-id` is
  `(unsigned-byte 16)`. Today each graph has its own 65536; globally, all stores share
  one. Retired type-ids cannot safely be reclaimed while any persisted node references
  them, so **65536 becomes a lifetime budget for the whole system**, not a per-store
  ceiling. An LLM defining types at runtime consumes it monotonically. Store-ids have the
  same property. Decide the policy now rather than discovering it at 60,000.

- **`(intern (symbol-name ...) :keyword)` in `update-node-type` is package-blind.** The
  type table registers three keys per type and the third is a keyword alias, so
  `:SPECIES` from `fish` and `:SPECIES` from `primate` collide in a shared type table. A
  small fix, but representative of the class of thing the package route will keep turning
  up: every place the engine reduces a symbol to its name.

- **Detach granularity is the store, not the namespace.** A cost of D3 that one-to-one
  did not have. Bulk-load detach takes every namespace in that store with it, so store
  boundaries are now chosen by *three* things — write cadence, recovery policy, and
  detachability.

## Still open

- The `backup` policy of D2 (edge placement is answered by D7).
- **What a *read* does when a store is detached.** The parked design specifies that
  writes touching a detached namespace fail rather than block; it says nothing about a
  traversal that reaches an edge into one. Needs an answer — unresolved-marker, signal,
  or skip — and it is the same question whether the edge is direct or reified.
- Detached / exclusive bulk-load mode, the parked design's largest open item, now
  reframed as a per-*store* operation.
- One transaction manager and one snapshot clock (#94) — a correctness gate.
- Type-id renumbering migration for existing graphs.
- Inbound cross-namespace index lookup ("which claims touch this vertex?"), whose shape
  is driven by the retrieval and agent-memory API.
