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

### D8 — A read into a detached store resolves to an unresolved marker

The parked design specifies only the write side (transactions touching a detached
namespace fail rather than block). A traversal that reaches an edge into a detached store
returns an **unresolved marker carrying the store id** — "there is an edge here, its far
end is in store 7, which is offline" — and the caller decides.

Rejected: *signal* (makes every traversal an error site, and will be caught-and-ignored
somewhere within a month) and *skip silently* (returns a subgraph that looks complete and
is not — a wrong answer wearing a right answer's clothes).

D5's tagged id is what makes the marker possible at all: the scan-based resolver could
only ever report "not found anywhere" and could not distinguish an absent store from a
missing node. That distinction is the whole difference between a degraded answer and a
wrong one.

**Detach is not confined to the source stores.** The memory layer bulk-loads too —
ingesting a pre-existing knowledge base, and restoring a corrupt store — so any store may
be offline mid-traversal and every traversal path must handle the marker.

### D9 — The global cross-graph epoch (#94) is in scope now, not deferred

Kevin's call, overruling the demotion argued above: co-locating things that share
invariants makes cross-store skew *rarer*, not impossible, and the failure mode is
silently-wrong derived data. Reasoning over a skewed pair records a conclusion whose
provenance does not say it was skewed. Deferring it means every day of new data is written
under per-store clocks.

Design sketch, to be confirmed before implementation:

- **The clock is image-level, not store-level.** A *system* (a directory of N stores) owns
  one clock. Opening any subset of stores opens the system, so a store opened alone still
  allocates from the shared clock.
- **Detach takes an epoch lease.** A detaching store is granted a range `[E1, E2)` at
  detach and allocates within it while offline; the global clock skips past `E2`. One
  handshake at detach, one at reattach, and no cross-process coordination in between —
  which is what keeps the dedicated-SBCL bulk-load path (the real 286k ACLED case) working.
  Epochs are 64-bit, so a lease of 2^32 makes exhaustion a non-issue.
- **Existing data migrates by watermark, not by rewrite.** Start the global clock above the
  max of every store's current counter. Epochs below the watermark are not cross-store
  comparable; epochs above it are. This degrades gracefully rather than breaking: an old
  node's version is still visible at any snapshot above the watermark, and the relative
  order of two pre-migration epochs in different stores is meaningless but also
  unobservable, since no snapshot can be taken between them. The stated limitation is that
  you cannot snapshot *into* the pre-migration past across stores. No record is rewritten.
- **Cost, named honestly:** a cross-store `with-read-snapshot` must register its read pin
  with every participating store, so a long cross-store query delays reaping in every store
  it touched. Per-store reaping must fold in pins from cross-store snapshots that include
  it, not just its own.
- **Audit needed:** each store's WAL will now contain epoch *gaps* (epochs consumed by
  other stores). `load-highest-transaction-id` takes a max and is fine; the peer/pull cursor
  code compares epochs and takes maxima and looks fine; but anything assuming epoch density
  must be found before this lands rather than after.

**Sequencing consequence: the cost of delay is monotonic.** Every write between now and the
migration lands below the watermark and can never be cross-store snapshotted. That is the
concrete argument for doing this early in the namespace work rather than late — not that it
blocks anything, but that its debt accrues daily.

### D10 — `backup` includes a dangling cross-store edge and warns

Chosen over omit and over refuse. A dangling edge on restore is diagnosable and
repairable; silent omission loses connectivity with no signal; refusing turns a routine
backup into an operator event over a condition D1 explicitly permits.

### D11 — `type-id` widens from 16 to 32 bits

Rejected 24 (bit-packing games to save ~1 MB across a corpus the size of mine-action's)
and 64 (doubles the `ve-key` for nothing). 32 aligns, matches `revision`, and needs no
packing tricks.

**This is the first on-disk format change in this design** — everything else so far was
additive or watermarked. It is affordable because the codebase has done exactly this
migration once already: `*node-head-reader*` (`primitive-node.lisp:124`) is a dispatch
variable, `deserialize-node-head-v1` reads the pre-MVCC 15-byte head, and `migrate-graph`
rebinds it so a v1 graph can be logically backed up and replayed into v2. Widening adds a
v3 reader the same way. The head goes 31 → 33 bytes (flags 1, type-id 2→**4**, revision 4,
data-pointer 8, commit-epoch 8, prev-pointer 8) and `ve-key` goes 18 → 20; both are rebuilt
by the replay, so the migration is **logical replay, not an in-place rewrite** — the
"replayable and reversible" property the parked design asked for.

**The real cost is not the field, it is the type-index.** `make-type-index` allocates
densely: `(* +max-node-types+ +index-list-bytes+)`. At 16 bits that is 65536 × 17 ≈ 1.1 MB;
at 32 bits it would be ~73 GB. **Widening therefore forces the type-index to become
sparse** — a keyed structure rather than an array indexed by type-id.

That is a cleanup worth doing on its own account. Today every type-index also allocates
`(make-array +max-node-types+)` filled with **65,536 mutexes** (`type-index.lisp:13`),
per index, per store, regardless of how many types the store actually uses. The widening
pays for removing a cost nobody had noticed.

**What widening does not buy: unlimited types.** Each type remains a CLOS class —
finalized, with interned accessors and a schema entry. 100k runtime-defined types means
100k CLOS classes. Widening moves the *hard* ceiling out of reach and leaves a *soft* one
around CLOS and memory. If the memory layer ever wants genuinely high-cardinality types,
the discarded option — runtime types sharing a type-id and discriminating on a slot —
comes back. Widening buys time, not infinity.

### D12 — Bulk load builds a shadow store and swaps it in

Rather than taking a store offline for the duration of a load, the loader writes a **new
generation of files** while readers continue on the current generation at the current
epoch. Reattach is an atomic swap plus a brief quiesce.

This changes the cost profile: the store is unavailable for the swap, not for the 68
minutes of a DeepState sweep or the hours of a 286k backfill. And a crashed load stops
being a recovery event — discard the shadow, the live store was never touched. Copying
first is not a real cost: the 3.8 GB forensics graph copies in about 6 seconds against a
load measured in tens of minutes, so shadow-by-copy beats any copy-on-write cleverness.

**Detach remains a quiescence protocol**, and the pin machinery is the tool: refuse new
pins and transactions on S, drain the existing ones, close, hand over. The hazard is not
concurrency in the abstract — the server holds live node objects, buffer-pool pages,
spatial-index handles and cache entries into S's mmap, and a stale node dereferenced after
close is a **segfault, not a condition**. `pin-read-epoch` / `reap-safe-floor` already
exist to prove no reader is mid-flight.

**Access semantics split by intent.** Explicit access naming a detached store *signals*
(`store-detached-error`) — the caller asked for something specific. Incidental traversal
into one yields D8's marker — that caller merely walked there, and an exception from the
middle of a graph walk is worse than a degraded answer.

**Recovery policy licenses the fast path.** Non-transactional bulk apply — direct heap and
index writes, no WAL, no MVCC versions — is available exactly to stores whose policy is
*derivable*, because a crash mid-load is repaired by redoing the load. Authored stores
load transactionally. The differential recovery policy the parked design preserved turns
out to already encode this; it does new work here without being extended.

**The vector-segment ceiling improves rather than worsens.** The parked doc flagged it as
sharpening this problem (a capacity failure inside APPLY leaves a persisted node with no
segment entry — invisible to retrieval, `store-count` still correct). A bulk load knows N
upfront and can **presize the segment**, turning a mid-apply failure into an upfront
allocation that fails before anything is written.

**In-process detach becomes viable for the first time.** The dedicated SBCL with
`MINE_ACTION_FORENSICS=false` existed precisely to stop the server holding the graph.
D9's lease still accommodates out-of-process later without redesign.

### D13 — The swap's interaction with whole-system restore

Whole-system restore is a *physical rewind of the log and all heaps together* (agreed
shape point 2). A shadow swap breaks that naively: after the swap, the shared WAL holds
transactions applied to a generation of S that no longer exists. Restoring to a point
before the detach wants the old S, which may have been deleted.

**Resolution: the pre-swap generation is a snapshot artifact, and retention is keyed on
recovery policy.**

- **Authored store — retention is mandatory.** The pre-swap generation is retained for at
  least the system's restore window and is subject to the same retention policy as any
  other backup. The swap refuses to discard it while the window still covers it.
- **Derivable store — retention is optional.** A restore predating the swap may instead
  rebuild the store from its source.

**Where the swap is recorded.** D9 already introduces an image-level durable object for the
global clock. The same object becomes a small append-only **system journal** of store
lifecycle events: create, detach at `E1` with lease `[E1,E2)`, swap at `E3`, attach, retire.
No new file — D9 requires the object regardless.

**Restore to T, algorithm:**

1. Read the journal; find every swap with `E3 > T`.
2. For each store so affected, the current generation is post-swap and must be replaced by
   the pre-swap one.
3. Pre-swap generation retained → use it; rewind physically to T.
4. Not retained, store derivable → rebuild, and **mark it rebuilt-not-rewound**.
5. Not retained, store authored → **refuse.** Authored data must never be silently
   approximated; this is the differential recovery policy again.
6. Emit a **manifest** naming, per store, rewound-to-T or rebuilt-at-now.

Step 6 is not bookkeeping. A restore that silently mixes rewound and rebuilt stores
produces exactly the inconsistent instant D9 exists to prevent, and nothing in the data
would record that it happened — the same failure shape that put #94 back in scope. Making
it *recorded* rather than silent is the fix, as it was there.

**T falling inside a detach window** (`E1 <= T < E2`) resolves cleanly: the old generation
was frozen at its last committed epoch `E0` and every reader saw `E0` throughout, so the
restore yields the pre-swap generation rewound to `E0`, noted in the manifest.

**Rejected: forbidding restore across a swap.** Fail-closed and simple, but it silently
truncates the restore window every time any store is bulk-loaded — backfill forensics and
discover the *ops* restore window collapsed to today. The restore window is a system
property; one store's maintenance must not shorten it.

**Consequence to decide separately:** the shadow swap is arguably a better per-store
restore than the logical replay of agreed-shape point 2 — same machinery as bulk load, and
atomic. The trade is real: logical replay needs no journal and no retention, but is not
atomic and writes its history forward as new transactions. Since D12 requires the journal
anyway, the marginal cost of using shadow for restore too is zero. Left open rather than
folded in.

## Newly identified constraints

- **Two global monotonic id spaces with no reclamation.** Retired type-ids cannot be
  safely reclaimed while any persisted node references them, so the type-id space is a
  whole-system lifetime budget rather than a per-store ceiling. Resolved for type-ids by
  D11 (widen to 32 bits). Store-ids have the same property but stay tiny, so no action.

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

- Whether the shadow swap supersedes logical replay as the per-store restore mechanism
  (see D13's closing note).
- Two-phase commit across stores (#93) — still deferred; D9 takes the clock, not
  cross-store atomicity.
- Type-id renumbering migration for existing graphs.
- Inbound cross-namespace index lookup ("which claims touch this vertex?"), whose shape
  is driven by the retrieval and agent-memory API.
