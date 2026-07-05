# Unique constraints for VivaceGraph (`:unique`) — design (issue #6)

**Status:** v1 **implemented** on branch `unique-constraint` (`unique-constraint.lisp`):
slot-level `:unique` (`t`/`equal`/`equalp`/canonicalizer), commit-boundary enforcement
(check in `validate` / maintain in `apply`), NULL-exempt, cross-subtype, on-disk **and**
memory backends, `:local` + degenerate `:origin`. **Durable on both backends.** Memory: the unique index rides the checkpoint image
(dumped at checkpoint, restored on open, no scan, lazy-safe — it does not materialize
nodes). On-disk: each unique index is a persistent heap skip-list (view-style composite
`(key id)` key), maintained incrementally in `apply` (mmap-durable, journal-replayable);
its root address is saved to a sidecar (`unique-indexes.dat`) at close and reopened at
open — no scan.  (A cl-store of the index *contents* on close would be wrong: stale
after a crash, since nodes committed since the last close are in the heap but not the
sidecar — so only the address is persisted; the contents live in the mmap skip-list.)
`rebuild-unique-indexes` remains the fallback (fresh graph / no sidecar / crash). The
design below is the reference; this note records what is built.

## Motivation

Add a per-slot uniqueness constraint to `def-vertex` / `def-edge`:

```lisp
(def-vertex user () ((username :type string :unique t)) :social-app)
```

so that creating or updating a node to a value already held by another live node
of that type signals an error instead of silently producing a duplicate.

## The key realization: this is a persistent index, not a view

A unique constraint is a **persistent unique index whose insert enforces**, exactly
like a standard store — *not* a map/reduce view with bolted-on enforcement.

VG already has almost the entire primitive:

- **Per-slot index property** — `node-class.lisp` defines an `indexed` slot with
  `:initarg :index`; slots already opt into indexing declaratively.
- **On-disk indexes and views are persistent and opened in place** — `open-graph`
  opens the ve/vev/type indexes at their stored locations and reopens each view's
  skip-list at its persisted pointer (graph.lisp). Nothing is rebuilt on open. (The
  memory backend persists its derived structures too; see
  `docs/memory-graph-design.md`.)
- **Unique ordered maps exist** — the skip-list takes `duplicates-allowed-p` (default
  NIL) and defines a `skip-list-duplicate-error` condition. **The rejection is
  currently disabled**: on a duplicate insert into a unique skip-list the error is
  commented out and replaced with a `log:error` + silent no-op (skip-list.lisp).

So a unique index is a *normal persistent index* keyed by the value, maintained in
`apply` and journal-replayable like every other index. Only the enforcement is
missing.

### Why the one-line "fix" is wrong (and how #7 is sidestepped)

The tempting fix is to uncomment `skip-list-duplicate-error`. **That is wrong**, and
it is the same trap as issue #7. Index maintenance runs in `apply-transaction`
(`apply-tx-writes` and friends), which is **after `finalize-tx-persistence`** — i.e.
after the write-ahead journal is durable. The persistent index is a *materialized
cache of the journal*; on recovery the journal replays through `apply` and re-mutates
it, so the index mutation **must** stay post-durability to be crash-consistent. But
re-arming the error there fires it *after* the commit is already durable — too late
to abort cleanly. (This is exactly why a *view* can't enforce uniqueness: view
maintenance is post-durability too. #6 does not need transaction-bound views; it
sidesteps #7 by enforcing earlier in the pipeline.)

### Where the check lives (`validate`) vs where maintenance lives (`apply`)

The whole `validate → durability → apply` region of `%commit` runs inside **one**
`with-transaction-manager-lock` (transactions.lisp): `validate` (OCC check) →
`finalize-tx-persistence` (durability) → `apply-transaction` (heap + indexes + views).
Because commits serialize there, split the constraint across the durability boundary:

```
(with-transaction-manager-lock (tm)            ; one serialized region
   (validate tx)                               ; + unique CHECK: index lookup, abort on hit  ← PRE-durability
   (finalize-tx-persistence tx tmp)            ; durability
   (apply-transaction tx graph))               ; unique INSERT: normal index maintenance     ← POST-durability, replayable
```

- **Check = a lookup at `validate`** (pre-durability). A hit aborts before anything is
  journaled — the existing `unwind-protect` in `%commit` cleans the temp file. This is
  the enforcement, and it also catches an intra-transaction duplicate by scanning the
  transaction's own write-set.
- **Insert = ordinary index maintenance at `apply`** (post-durability), so it is
  derived from the journal and crash-consistent, like ve/vev/type-index.

`validate` and `apply` are in the **same** manager-locked region, so no other commit
can slip between the lookup and the insert — no phantom, and OCC's id-based read/write
sets (which cannot see two different-id nodes claiming the same value) are not relied
on. The skip-list's own dup rejection becomes a defensive backstop, not the mechanism.

Distinguish the two failure modes: a transient OCC `validation-conflict` retries (up
to `*maximum-transaction-attempts*`, then under an exclusive lock); a
`unique-constraint-violation` **fails fast** — retrying cannot help, the duplicate is
really there.

## The `:unique` argument — a uniqueness *key*, not a raw predicate

`:unique` takes an argument that specifies how values are compared for equality:

| Form | Meaning |
|---|---|
| `t` | Uniqueness on the raw slot value, compared as `equal` (VG's serializable content equality). The default. |
| `equal` / `equalp` | The named standard equivalence. |
| a function designator or lambda | A **1-arg canonicalizer**: `key := (funcall fn value)`; uniqueness is `equal` on `key`. |

The uniqueness comparison is therefore always **`equal` on a canonical key**, which is
what a persistent index can enforce portably. Examples:

```lisp
(username :unique t)                       ; exact
(email    :unique #'string-downcase)       ; case-insensitive
(handle   :unique (lambda (s) (string-trim " " (string-downcase s))))
(sku      :unique equalp)                  ; case-folded strings, = across number types
```

### Why a canonicalizer and not a raw 2-arg equality predicate

A unique index is backed by a persistent map keyed by the (serialized) canonical key.
On-disk that is the linear-hash (keyed by serialized bytes); in RAM it is a hash
table. This choice is deliberate:

1. **Correctness.** Uniqueness requires a genuine *equivalence relation*. Every
   practical one — case-fold, trim, Unicode-normalize, round a number, sort a set — is
   expressible as canonicalization to a comparable key. Relations that are *not*
   canonicalizable (e.g. "within epsilon") are not transitive, so they are not valid
   uniqueness keys anyway.
2. **Indexability.** A raw 2-arg predicate gives equality but neither a hash nor a
   total order, so it cannot back an index — it forces an O(n) scan of every node of
   the type at each commit. A canonicalizer yields an O(1)/O(log n) index.
3. **Portability, incl. ECL.** ECL does not support custom hash-table tests beyond the
   standard four (see CLAUDE.md). The portable path is *canonicalize → key it with a
   standard test*. `equalp` is realized by canonicalizing (case-folding strings, etc.)
   before the byte key; a custom lambda by applying it before the byte key. So every
   `:unique` form reduces to "byte-equality of the serialized canonical key" on-disk,
   or an `equal`/`equalp` hash in RAM — no per-index custom test required.

Note the canonicalizer is **1-arg** — it receives the *slot value*, not the node.
Multi-slot (composite) uniqueness is a **class-level** declaration and is deferred (see
below), because a slot option only sees its own value.

## Semantics decisions

- **NULL / unbound is exempt.** A nil or unbound unique slot is not indexed and never
  collides (SQL-like: many NULLs allowed). Only bound values are enforced.
- **Enforced across subtypes.** `:unique` on a parent-class slot is one index for the
  whole type hierarchy (username unique across `user` and its subclasses).
- **MVCC / `deleted-p`.** The index maps canonical-key → *logical* node id (the UUID,
  stable across versions). "Held" means held by a **live, non-deleted** version.
  `mark-deleted` releases the claim (so a value can be reused after deletion); an
  update that changes the value releases the old key and claims the new one. The
  release differs slightly between the on-disk version-chain model and the memory
  backend's atomic node-swap, but the index contract is the same.
- **Persistence.** The unique index is persistent and opened in place like the other
  indexes (not rebuilt on open). On the memory backend it is one of the persisted
  derived structures (checkpoint image), restored structurally on open — consistent
  with #49/#50, so a lazy open does not have to scan-and-materialize to rebuild it.

## Replication: uniqueness is a per-field scope, not a global invariant

Global uniqueness across offline peers is **provably unenforceable without
coordination** (a unique constraint is not I-confluent; Bailis et al., VLDB 2015). So
local enforcement (above) is the base, and each `:unique` field additionally declares
a **cross-peer scope**:

| Scope | Model | Use for |
|---|---|---|
| `:origin` | **Partition** the key space by `origin-id` (the canonical key is origin-prefixed), so collisions are impossible by construction — zero coordination. | Anything VG mints (find/site codes). **The recommended default.** |
| `:local` | Unique **within a graph** only; cross-peer collisions are allowed and flow through the existing peer conflict policy. | User data where a late rename is acceptable. |
| `:hub` | **Hub-authoritative** (reservation): the value is provisional on a device until the hub accepts it on push; a collision is rejected. | A genuinely global, human-chosen key (rare). |

### `:hub` — "provisional" resolves by compensating forward, never by rollback

The important property: a hub rejection resolves at **field** granularity, not
**transaction** granularity — so there is **no rewind, and no transaction dependency
graph**.

This works because **identity is the node's immutable id (UUID), not its unique
value.** Edges point to ids; later transactions carry ids; nothing downstream
references "the node whose username is alice." So changing `alice → alice-2` is a
local field edit that breaks nothing — a dependency cascade only appears if downstream
logic keys off the unique *value*, which the scope choice lets you avoid (use `:origin`
for values other logic depends on).

Mechanically, reusing existing peer machinery:

1. The device commits locally and durably; the op that set the value sits in the feed
   as **pending-ack** (VG already tracks per-op push-ack).
2. On push the hub runs the same commit-boundary check. Collision → it **NAKs that one
   op** and reports the winner.
3. The device appends a **compensating authored op** that re-keys the field
   (deterministic auto-rename / user prompt / tie-break) — just another op in the feed.
   The original op stays in history; the rename supersedes the value. This is the same
   winner-keeps / loser-renames shape the peer **conflict policy** (Branch B, per-field
   Lamport) already implements; `:hub` uniqueness is one more conflict type routed
   through that seam. No new distributed-rollback subsystem.

So "provisional" = *durable now, possibly renamed on hub ack*, and the app tolerates a
late rename of that one field (Dropbox "conflicted copy").

**The one exception — irreversible external effects.** If the value was externalized
before the hub weighed in (printed on a physical label, sent to another system, told
to a human), a rename has real-world consequences no protocol can undo. Such a field
cannot be optimistic: it must **reserve online before use** (username-registration
style), accepting that it cannot be finalized offline. Rare, and a per-field decision.

### The decision tree

```
Is the value VG-minted (an id/code we generate)?
  └─ yes → :origin   (partition; collision-free by construction; free)
  └─ no (user-meaningful, must be globally unique)
        Can the value be safely re-keyed after the fact?
          └─ yes → :local / optimistic + compensate  (offline-friendly; late rename)
          └─ no  → :hub reserve-online               (blocks offline; honest cost of global uniqueness)
```

## v1 scope vs deferred

**v1:**
- Slot-level `:unique <spec>` where `<spec>` ∈ `t` | `equal` | `equalp` | canonicalizer.
- Commit-boundary enforcement (check in `validate`, maintain in `apply`) on
  **locally-authored** commits, on-disk **and** memory backends.
- NULL-exempt, enforced across subtypes, persistent index (memory: in the derived set).
- Cross-peer scope: `:origin` and `:local`. A `unique-constraint-violation` condition.

**Done since v1 (commit `f81759c`) — peer-replication gaps 1+2:**
- Uniqueness index is now **maintained on replicated/pulled applies**:
  `apply-peer-create-writes` (state-sync) and `apply-peer-authored-op` (authored) run
  `apply-tx-writes-to-unique-indexes`, and `peer-purge-node` releases a purged node's
  unique keys. A device's index therefore reflects everything it holds, and a local
  commit is enforced against pulled values. (Enforcement itself is still on the
  authoring commit / hub re-home, both of which go through `%commit`.)
- **Real per-node `:origin`** (Option A): a `node-origins` side-store on peer graphs
  (node-id → the authoring origin captured once at create, fixed for life), persisted
  like `field-stamps`. `%node-origin` reads it; `:origin` keys on `(origin-token . value)`
  so two devices minting the same value are distinct keys. Non-peer graphs have one
  origin, so `:origin` == `:local`.
- Fixed a latent `less-than`/`greater-than` bug (equal/nested lists ranked strictly
  less-than themselves) that the `:origin` composite key surfaced.

**Still deferred:**
- **Global-scope (`:local`/`:hub`) cross-device collision arbiter** — the hub keep-winner
  + surface-loser + compensate-forward flow. Tracked as **GH #51**. (The hub already
  enforces on re-home via `%commit`, but a violation there currently aborts the push
  session rather than resolving it.)
- `:hub` reserve-online path (hub-authoritative reservation + provisional/ack flow).
- Composite / multi-slot uniqueness (a class-level declaration).
- Range queries on a unique field (would want a skip-list backing rather than a hash).

## Acceptance criteria (v1)

- [x] `:unique t` on a slot rejects a create/update that duplicates another live
      node's value with `unique-constraint-violation`; the transaction aborts cleanly
      (nothing journaled), distinct from a retriable OCC `validation-conflict`.
- [x] `:unique <canonicalizer>` (e.g. `#'string-downcase`) enforces equality on the
      canonical key; `:unique equalp` case-folds.
- [x] Concurrent commits racing the same value: exactly one wins; the other gets a
      `unique-constraint-violation` (verified under the manager lock, no phantom).
- [x] NULL/unbound values never collide; `mark-deleted` releases the value for reuse;
      an update that changes the value releases the old key.
- [x] Works on the on-disk **and** memory (eager + lazy) backends; the unique index is
      persisted/opened, not rebuilt on open.
- [x] `:scope :origin` makes two origins' identical raw values non-colliding (per-node
      origin under peer replication; a device also enforces pulled values).
- [x] Docs (`docs/vivace-graph-v3-doc.org` Chapter 8 + Chapter 16) updated.

## Implementation sketch (files likely touched)

- `schema.lisp` / `node-class.lisp` — parse `:unique <spec>` (and `:scope`) in the slot
  spec; register a unique index per constrained slot in the node-type metadata.
- new `unique-index.lisp` (or fold into an existing index file) — the persistent
  canonical-key → id map (on-disk linear-hash / in-RAM hash), with
  `claim`/`release`/`lookup`; a `unique-constraint-violation` condition.
- `transactions.lisp` — a `validate-unique-constraints` step in `%commit`'s
  manager-locked region right after `validate` and before `finalize-tx-persistence`;
  an `apply-tx-writes-to-unique-indexes` step in `apply-transaction` (create claims,
  update re-keys, delete releases).
- `memory-graph.lisp` — a mem unique index + inclusion in the persisted derived
  structures (image dump / structural restore).
- `graph.lisp` — open/restore the unique index alongside the other indexes.
