# Multi-graph support in one image — design

**Date:** 2026-07-29
**Status:** approved, not yet implemented
**Target:** 3.0.0

## 1. Why

VivaceGraph already lets an application open several graphs in one Lisp image, and
mine-action does exactly that (ops / knowledge / forensics). What it does not have is a
*contract* for what that means. The behaviour today is a mix of things that work, things
that silently do the wrong thing, and things that were fixed on one code path but not its
twin.

This spec defines the contract, and the mechanisms that enforce it, so multi-graph use is
a supported configuration rather than a configuration that happens to mostly work.

## 2. What was measured first

Every claim below was reproduced before being designed against. This section is the
evidence, not background.

**Cross-graph reads work outside a transaction and silently fail inside one.** With node
`A` in graph `ga` and `*graph*` bound to `gb`:

| context | `(lookup-vertex a-id :graph ga)` |
|---|---|
| outside a transaction | `:FOUND` |
| **inside a transaction on `gb`** | **`NIL`** |
| inside a transaction on `ga` | `:FOUND` |

`LOOKUP-OBJECT` has two methods. The `(transaction null)` one binds `*GRAPH*` to the
requested graph and carries a comment citing #53 ("type-ids are per-graph, so without this
a cross-graph read materializes the wrong class"). The transactional method does
`(lookup-node table id (graph transaction))`, discarding the caller's `:GRAPH`. The fix
landed on one path and not the other. The failure is a `NIL` for a node that exists —
indistinguishable from "no such node".

**Two graphs defining a class with the same name silently clobber each other.**
Defining `dup-thing` under `:dg-one` with slot `alpha`, then under `:dg-two` with `beta`
and `gamma`:

```
*** NO ERROR SIGNALLED ***
slots now:            ... BETA GAMMA
is ALPHA still a slot? NIL
one class object shared by both graphs? T
```

Graph one's nodes still carry `(:ALPHA . …)` in their stored data alists, but the class no
longer declares the slot, so that data is on disk and unreachable through the API.

**Same-graph redefinition works correctly and must keep working.**

```
before: slots=(ALPHA)       registry-entries=1  alpha="one"
after:  slots=(ALPHA BETA)  registry-entries=2  alpha="one"  beta="two"
```

Note `registry-entries` going 1 → 2: `DEF-VERTEX` does `(push meta (gethash graph-name
*schema-node-metadata*))`, so a redefinition **accumulates** rather than replaces.

**Node instances are pooled but never recycled** — CORRECTED 2026-07-29 after review.
On SBCL/CCL/LispWorks a node comes from `GET-VERTEX-BUFFER` and is `CHANGE-CLASS`ed; ECL
builds fresh instances (avoiding the #47 leak). But `RELEASE-BUFFER` handles byte vectors
only, keyed by length — there is **no return path for node instances**. The `:VERTEX` /
`:EDGE` pools are only ever filled with fresh `MAKE-INSTANCE` results by
`MAKE-VERTEX-BUFFER` / `MAKE-EDGE-BUFFER`, so each pooled instance is used once.

An earlier draft of this spec claimed a missed stamp would leave the *previous occupant's*
graph. That is wrong: it leaves `NIL`. The consequence is still a defect — `NIL` silently
re-arms the `*GRAPH*` fallback this design exists to remove, so a missed site quietly
reintroduces the original bug rather than announcing itself — but it is a silent
regression, not a confidently wrong answer.

## 3. Contract: transaction scope

**A read-write transaction belongs to exactly one graph** — the one whose transaction
manager it was created against. Touching a node whose home graph differs, read or write,
signals `CROSS-GRAPH-TRANSACTION-ERROR`.

This is not a limitation chosen for convenience; it is what the current implementation
*means*. The transaction manager is a slot on `GRAPH`. `WITH-TRANSACTION` defaults to
`(transaction-manager *graph*)`. The `TX` object has a single `graph` slot. Each graph has
its own tx-id counter, its own committed-transaction set that `OVERLAPPING-TRANSACTIONS`
validates against, and its own WAL. A transaction spanning two graphs would need a global
ordering across independent counters and two-phase commit across two WALs; without 2PC a
crash between the two commits leaves one committed and the other not. Atomicity and
durability are defined per graph, and this spec keeps that definition rather than
weakening it.

Applications needing to write to several graphs use one transaction per graph and own the
sequencing. That coordination is explicitly **not** atomic, and the manual must say so.

## 4. Contract: read consistency

**A read-only snapshot is per graph, and several may be active at once.** A cross-graph
query holds one snapshot per participating graph. Each graph is internally consistent and
repeatable for the duration; there is deliberately **no single instant across graphs**.
Two graphs may be observed at different logical moments.

That limit is a documented property, not a defect to be worked around. A single
cross-graph instant needs a global epoch shared by every transaction manager — the same
class of work as multi-graph transactions, and out of scope here (§11).

Prolog queries need no transaction to read across graphs: `SELECT` runs
`(funcall func #'prolog-ignore)` with no transaction, and that path already honours an
explicit `:GRAPH`. Only `:SNAPSHOT T` wraps the query in `CALL-WITH-READ-SNAPSHOT`. So
`:SNAPSHOT T` does not merely fail to help a cross-graph query — it is what *breaks* it
today, by binding a single-graph transaction that then swallows the requested graph.

**The rule is that the kind of transaction decides**, because the two contracts above
give different answers for the same call:

- a **read-only snapshot** on A may read B — resolving through B's own snapshot if one is
  active, otherwise reading non-transactionally;
- a **read-write transaction** on A may not touch B at all — error, read or write.

A rule keyed on "is there a transaction" cannot express this. It must key on *which kind*.

## 5. Node ownership

Nodes carry their home graph in `NODE-GRAPH`: a `:META T :PERSISTENT NIL` slot on the root
`NODE` class, so it is a real CLOS slot rather than an alist entry and is never
serialized. A graph is not a value that can be written to disk, and a node's home is
re-established on every materialization.

`NIL` means **unknown** and falls back to `*GRAPH*`, preserving today's behaviour for any
path not yet stamped. `NODE-HOME-GRAPH` is the accessor to use wherever a node's heap,
tables or schema are resolved.

There is a **third state**: because the slot is excluded from cl-store (below), a restored
node has it UNBOUND, and an unbound read signals rather than falling back.
`NODE-HOME-GRAPH` must therefore treat unbound as unknown too, so the documented
graph-or-fallback contract is actually true rather than true-by-luck of a stamp two lines
downstream.

Ownership is needed regardless of the read paths: it is how cross-graph misuse is
*detected* for the error in §3.

**The slot must be stamped on every materialization path**, because a missed site leaves
`NIL` and silently falls back to `*GRAPH*` — the very bug this removes.

Stamp at the FUNNELS, not the leaves. This list is the complete enumeration — §12 depends
on it being complete, so anything added later belongs here too.

On-disk backend: `MAKE-VERTEX` / `MAKE-EDGE` (creation — a node is otherwise unstamped for
the whole body of the transaction that creates it), `FINALIZE-NODE`, `ENSURE-NODE-BYTES`,
`LOOKUP-NODE` (**both** the cache-hit and miss branches), `COPY-NODE` (which enumerates the
slots it copies), the untyped `MAP-VERTICES` and `MAP-EDGES` scans (their nodes come
straight from the deserializer and a side-effect scan never reaches `ENSURE-NODE-BYTES`),
and `APPLY-TX-WRITE` for `TX-UPDATE` — the update/delete funnel, which unlike its
`TX-CREATE` sibling never reaches `FINALIZE-NODE`.

The **memory backend needs its own stamps** and is the higher-value case, being the
Android/mine-action consumer: `LOOKUP-NODE ((table mem-table) key graph)` (which declared
`graph` ignored), `APPLY-TX-WRITE` for both `TX-CREATE` and `TX-UPDATE` on
`MEMORY-GRAPH-MIXIN` (full overrides, so `FINALIZE-NODE` never runs there), `%LZNODE->NODE`,
and `RESTORE-MEMORY-IMAGE` — see below for why restore needs one. Without these the
feature is structurally inert for memory graphs.

**`NODE-GRAPH` must be excluded from cl-store.** `WRITE-MEMORY-IMAGE`'s non-lazy branch
cl-stores the node CLOS objects themselves, and cl-store walks every slot regardless of
`:PERSISTENT NIL` — so the slot would drag the live graph, its schema, its tables, locks
and threads into the image, and restore would hand back a phantom graph. `:PERSISTENT NIL`
governs only VG's own binary codec; the second serializer needs an explicit exclusion.

## 6. Read resolution and enforcement

`*TRANSACTION*` keeps its present meaning: **the** read-write transaction, necessarily
single-graph, still exported, still bound in the same three places
(`CALL-WITH-TRANSACTION`, `CALL-WITH-READ-SNAPSHOT`, the restore path). Rather than
replace it with a registry, add `*READ-SNAPSHOTS*`, mapping graph → read-only snapshot.
`CALL-WITH-READ-SNAPSHOT` populates an entry instead of rebinding `*TRANSACTION*`; nesting
on the same graph inherits, exactly as its docstring already promises.

Resolution for a read of graph `G`:

1. If a read-write `*TRANSACTION*` is active:
   - on **G** — use it;
   - on any **other** graph — signal `CROSS-GRAPH-TRANSACTION-ERROR`.
2. Otherwise `G`'s entry in `*READ-SNAPSHOTS*` — use it.
3. Otherwise read non-transactionally.

Step 1 is deliberately exhaustive: **an active read-write transaction forbids cross-graph
access outright, even when a read-only snapshot for `G` happens to be in
`*READ-SNAPSHOTS*`.** It does not fall through to step 2. Reading another graph from
inside a read-write transaction is a programming error under §3 regardless of whether a
snapshot makes it technically answerable, and silently satisfying it from a different
consistency domain is exactly the kind of "works until it doesn't" behaviour this spec
exists to remove.

Cross-graph reads are therefore available from a read-only snapshot or from no
transaction at all — never from inside a read-write transaction.

**Scope of "a read" here: through the transactional path, not literally every read.**
§3's "touching a node ... read or write, signals" means every access that funnels through
`LOOKUP-OBJECT` (so `LOOKUP-VERTEX`/`LOOKUP-EDGE` and typed scans) or through a mutation
entry point (`CREATE-NODE`, `SAVE`/`UPDATE-NODE`, `DELETE-NODE`/`MARK-DELETED`). Two paths
read a node's storage without going through that check and are accepted carve-outs, not
gaps to close: `COPY-NODE`'s `MAYBE-INIT-NODE-DATA`, which resolves through the node's own
home graph and so can fault in bytes from a graph other than the active transaction's (the
misuse is still caught one step later, at the following `SAVE`); and the fully untyped
`MAP-VERTICES`/`MAP-EDGES` scan (no `:VERTEX-TYPE`), which walks the lhash directly and
never reaches `LOOKUP-OBJECT` — consistent with the already-documented, pre-existing
carve-out that untyped scans bypass MVCC entirely and are meant for quiescent, read-only
passes (manual, Chapter 12, "A note on scans"). Scanning a foreign graph that way from inside a
read-write transaction reads without a pin; that is the caller's responsibility, not
something the engine checks (tracked as GH #96, disposition: document, not close).

That is the enforcement point, and it is the exact site that returns `NIL` today.

Write enforcement sits at `SAVE` / `UPDATE-NODE` / `DELETE-NODE` / `MARK-DELETED`,
comparing `(node-graph node)` against the transaction's graph.

## 7. Class names are globally unique across graphs

`DEF-VERTEX` / `DEF-EDGE` signal `DUPLICATE-NODE-CLASS-ERROR` when the class name is
already registered under a **different** graph name. Plain error, no restart: silently
sharing a class across schemas is the thing that corrupts reads, and a restart in a
non-interactive load is just something a `HANDLER-BIND` swallows. Re-homing a class means
removing the old definition first.

Two details that decide whether the implementation is correct:

**The guard must be emitted before the `DEFCLASS` form.** The macro currently expands to
`DEFCLASS` first and registration after, so a check placed later fires only once the class
has already been clobbered.

**The check is on graph-name identity, not presence.** "Is this name already registered?"
is the natural phrasing and is wrong: a same-graph redefinition adds a second entry under
the same key (§2), so a presence test rejects exactly the case §7 must preserve. The check
is "is this name registered under a graph-name *other than* this one".

Vertex-vs-edge is irrelevant — they share the CL class namespace, so the same name as a
vertex in A and an edge in B is the same collision.

**Feasibility — CORRECTED 2026-07-30 by the whole-branch review.** The original scan
excluded `demo/` and under-counted. There are **three real collisions**: `example.lisp`
(`:test-graph`) and `demo/schema.lisp` (`:social-shopping`) both define `customer`,
`merchant` and `product`. Neither file is in the ASD, so no suite breaks — but
`example.lisp` is the canonical walkthrough named in CLAUDE.md, and loading it alongside the
demo now hard-errors. That is a real user-facing consequence, not the "nothing breaks" the
first scan claimed. The two harmless cases also stand: a docstring example in
`schema.lisp`, and `xach-test.lisp`, which is not in the ASD.

## 8. Schema registry: replace, don't accumulate

`DEF-VERTEX` pushes a new meta onto `(gethash graph-name *schema-node-metadata*)` on every
evaluation, so redefining a type accumulates metas without bound. This is not cosmetic:
`UPDATE-SCHEMA` does `(dolist (meta (reverse node-metadata)) (instantiate-node-type meta
graph))`, replaying **every historical version of every type** on graph open. N
redefinitions cost N instantiations of that type at every open, forever.

Fix: replace the existing meta for that node-type name **in place**, preserving its
position in the list.

Position matters. `UPDATE-SCHEMA` applies oldest → newest, and `INSTANTIATE-NODE-TYPE`
assigns type-ids in application order. Replacing in place keeps a redefined type's type-id
assignment order stable; remove-then-push would move it to the front and could hand it a
different type-id on a fresh graph. Since §7 makes names unique within a graph, keying the
replacement on the node-type name is sufficient.

## 9. What does not change

Validation, `OVERLAPPING-TRANSACTIONS`, the retry and exclusive-lock fallback, the WAL,
MVCC epochs, and nested-transaction independence are all untouched. No on-disk format
change. A single-graph application sees no behavioural difference except that a
previously-silent `NIL` becomes an error, and a previously-silent class clobber becomes an
error.

## 10. Testing

- **Ownership under pooling** — nodes materialized from a recycled buffer must report the
  correct graph. This is the test that fails if a stamping point is missed.
- **Cross-graph reads under a wrong `*GRAPH*`** — slot reads, `NODE-TO-ALIST`, `COPY`, and
  writes landing in the right graph, each with `*GRAPH*` deliberately bound elsewhere.
- **The enforcement error** — a read-write transaction touching a foreign node signals;
  a read-only snapshot on A reading B does not.
- **Composed snapshots** — two snapshots active at once, each internally consistent.
- **Class-name uniqueness** — a second graph reusing a name errors; same-graph
  redefinition still succeeds, with existing node data readable and the new slot usable.
- **Registry replacement** — repeated redefinition leaves one meta, and type-ids stay
  stable across a redefinition on a fresh graph.

Every test must be shown to fail without its corresponding change.

Suites: full graph-db suite, plus acid and concurrency, on **both SBCL and ECL**. ECL
matters specifically here because it is the implementation that does *not* pool nodes, so
it exercises the opposite branch of §5.

## 11. Out of scope — file as issues

- **Multi-graph transactions** with two-phase commit across per-graph WALs and a global
  tx-id ordering. The future target.
- **A global cross-graph epoch**, giving cross-graph queries a single instant.
- **Answered 2026-07-30:** no consumer needs atomic cross-graph writes. mine-action uses
  two graph names (`:mine-action`, 7 sites; `:mine-action-forensics`, 1) and has no code
  writing to two graphs in one operation — no `save`/`update-node` call passes an explicit
  `:graph` at all. The single-graph contract in §3 costs it nothing.

## 12. Risks

- **Missed stamping point.** A path that does not set `NODE-GRAPH` leaves `NIL`, which
  silently falls back to `*GRAPH*` — reintroducing the original bug without any signal.
  This is why §5 enumerates the sites rather than leaving them to judgement, and why the
  test must be shown to fail with *each* stamp removed, not just one: a test satisfied by
  the `FINALIZE-NODE` stamp alone passes with the others deleted.
- **`*READ-SNAPSHOTS*` lifetime.** Entries must be removed on unwind, or a snapshot
  outlives its dynamic extent and pins the reaper's floor.
- **Enforcement in the wrong place** could reject legitimate single-graph work. The error
  must fire only when a node's home is known *and* differs.
