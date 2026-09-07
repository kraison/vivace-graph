# Node-local time travel (MVCC Phase C-3): design

**Issue:** kraison/vivace-graph#115 (C-3), under the Phase C tracker
#117. **Plan:** `docs/mvcc-phase-c-plan.md`, whose C-3 section this
spec supersedes in detail. **Prior decisions:** #116 (time travel is
node-local, decided 2026-09-06), #347 (`:as-of-epoch` on the claim
API, inclusive comparison, reaped told from created-after by the
oldest retained revision), #53 (read snapshots compose per graph).
**Date:** 2026-09-07. **Status:** approved in review, sections 1–4.

## 0. Problem

The engine retains versions (`:keep-revisions`) and resolves a
transactional read at its start epoch, but nothing lets a caller name
an epoch: a snapshot is always "now". `vertex-history` hands out the
chain of one vertex, and the claim layer reconstructs one claim at an
epoch by walking it (#347), but there is no read of the graph as of a
past epoch — no values, no membership, no adjacency, no query. The
Phase C plan put that API last (C-3), after the versioned index
(C-1) and epoch-gated reclamation (C-2), because it assumed the API
needed them. It does not: the machinery in place today answers
correctly, at a cost. This unit ships the API on that machinery and
leaves C-0 through C-2 as the performance track behind it.

## 1. Rulings

| # | Ruling | Why |
|---|--------|-----|
| R1 | Build C-3 now on the existing version chains and index tombstones; C-0/C-spike/C-1/C-2 stay on #117 as a later performance track with this API unchanged. | Correctness is available today; the only thing C-1 buys the API is cost. Consumers (cl-llm#53, the blackboard) need the surface, not the speed. |
| R2 | An as-of read is a read-only snapshot transaction started at E+1, so it is inclusive: the version live at E is the newest with commit epoch ≤ E. | Same reading of "as of E" as #347's `:as-of-epoch`; `resolve-version-at-epoch` is a strict-start predicate, so E+1 makes it inclusive without a second predicate. |
| R3 | Node-local, per #116: an epoch names a point in the issuing node's own history. Epochs compare across stores only under one `system-clock`. | Decided; recorded again here because the API is where a caller would first expect otherwise. |
| R4 | Absence and reaping are told apart and reaping is reported, never absorbed: the oldest retained version's `revision` is 0 for "did not exist at E" and above 0 for "existed, reaped". | #347's discriminator, promoted to the engine. A silent NIL for a reaped node is a lie about history. |
| R5 | No storage format change, no migration. | Nothing on disk moves; the replication harness and the cross-impl matrix are not gated on this unit. |
| R6 | The untyped scan (raw lhash walk) is refused under an as-of snapshot rather than answering live. | It bypasses MVCC today by documented design; under a named epoch a live answer is wrong, and refusing is one check. |
| R7 | The in-memory backend refuses as-of reads. | It keeps no version chains (prev-pointer is always 0), so it has no history to travel. |

## 2. The as-of snapshot (`transactions.lisp`)

### 2.1 Opening

`create-transaction` gains `:start-epoch`. When given, the transaction's
`start-tx-id` is that value instead of `tm-current-epoch`; it must not
exceed the manager's next epoch (`tm-current-epoch`). Everything
downstream — `resolve-version-at-epoch` with `start-tx-id`,
`minimum-start-transaction-id`, the reaper's floor,
`prune-committed-transactions` — reads the slot it already reads,
so a transaction started at E+1 observes versions with commit epoch < E+1
and holds the floor at E+1 for its extent.

`call-with-read-snapshot` gains `&key as-of if-reaped`. With `:as-of E`
it registers a read-only transaction with `:start-epoch (1+ E)` and a
read pin, exactly as today's snapshot does, and records the transaction
in `*read-snapshots*` under the graph. The transaction is an `as-of-tx`
(subclass of `tx`) carrying the requested epoch, the `if-reaped` policy
(`:error`, the default, or `:skip`) and a skipped counter. The macro:

```lisp
(with-as-of ((graph) epoch &key (if-reaped :error)) body...)
```

is `call-with-read-snapshot` with `:as-of`. `(latest-epoch graph)`, new,
returns the newest epoch an as-of read of GRAPH may name: one below the
manager's next epoch. `commit-epoch` of any committed node is another
valid epoch.

### 2.2 Refusals

`as-of-refused` (an `error`) carries the graph, the epoch and a
`reason`:

| reason | when |
|--------|------|
| `:future-epoch` | E ≥ the manager's next epoch (`tm-current-epoch`), i.e. above `latest-epoch`: nothing has committed there yet. |
| `:read-write-transaction` | `*transaction*` covers the graph: a read-write transaction's snapshot is its own start, and a second epoch inside it would answer two questions at once. |
| `:snapshot-active` | `*read-snapshots*` already holds a snapshot of the graph at a different epoch, or a plain (current) snapshot. The same epoch inherits. |
| `:untyped-scan` | an untyped `map-vertices`/`map-edges` under an as-of snapshot (R6). |
| `:no-version-history` | the graph is a memory graph (R7). |

A plain `with-read-snapshot` opened inside an as-of extent inherits the
as-of snapshot: the graph is already snapshotted, which is today's rule.

### 2.3 Cost, stated

An open as-of snapshot holds two floors at E+1 for its extent: the
reaper's (versions live at E are retained on every node updated
meanwhile) and the committed-transaction prune's (validation records
back to E+1 are kept). Both are the same costs a long-running read-write
transaction imposes today. Versions reaped before the snapshot opened are
gone; the API reports them (§4), it cannot recover them.

## 3. Resolution and enumeration

### 3.1 Values

Reads under the snapshot go through the transactional `lookup-object`
method as today, resolving each node at `start-tx-id` = E+1. The
resolved version is cached in the transaction's local cache, so reads
are repeatable within the extent. `*snapshot-reads-p*` stays the switch
it is.

### 3.2 Reaped versus absent (R4)

When `resolve-version-at-epoch` finds no version old enough it also
reports the oldest retained version it walked to. On an `as-of-tx` the
lookup then decides: oldest retained `revision` 0 → the node did not
exist at E → NIL; above 0 → the version at E was reaped past
`:keep-revisions` → per the policy: `:error` signals
`version-reaped-error` (id, requested epoch, oldest retained commit
epoch, oldest retained revision); `:skip` increments the snapshot's
counter and answers NIL. `(as-of-skipped-count graph)` reads the counter
inside the extent and is NIL when no as-of snapshot of GRAPH is open.

Two limits of the discriminator, documented rather than fixed. A node
created after E whose creation version was itself reaped reports as
reaped, not absent: with revision 0 gone the engine cannot tell the two
apart, and "cannot answer" is the conservative report (#347 has the
same). `revision` is 32 bits and wraps; after 2^32 updates to one node
the discriminator reads a wrapped 0 as the create.

A `:keep-revisions` of 0 (the default) therefore answers only at the
latest epoch; anything older on an updated node is reaped. The docs say
so in the first paragraph: keep-revisions is the depth of time travel.

### 3.3 Membership

The type index and the adjacency indexes (ve, vev) keep a soft-deleted
node's entry in place — `delete-node` writes a version with the deleted
flag set and leaves the pcons alone — and a pcons removed later by
compaction stays in the chain flagged deleted (`mark-pcons-deleted`).
So membership at E is recoverable by walking every entry and resolving
each id at E:

- Under an as-of snapshot, `map-vertices` and `map-edges` pass
  `:include-deleted-p t` to `map-index-list` so tombstoned entries are
  visited, resolve each id through the lookup path (§3.1, §3.2), and
  apply the caller's own `:include-deleted-p` to the resolved version.
  A node deleted after E resolves to its pre-deletion version and is
  included; one deleted at or before E resolves to the deleted version
  and is excluded; one created after E resolves to NIL and is skipped;
  one reaped follows the policy.
- `active-edge-p` reads endpoints through the same lookup, so an edge
  whose endpoint was deleted after E is active at E.
- `outgoing-edges`, `incoming-edges`, `traverse`, `edge-exists-p`, the
  generated `lookup-<type>`/`map-<type>` functions, and the Prolog
  functors that enumerate through them (`is-a/2`, `outgoing-edges/n`,
  `incoming-edges/n`, the generated edge functors) inherit all of this
  with no change of their own. `is-a/2` with the type unbound already
  enumerates per type; that stays.
- An untyped scan is refused (R6).

This is the tombstone walk C-1's `:as-of` enumeration will replace: its
cost is every entry ever indexed under a type, resolved one by one. The
plan records that the API does not change when C-1 lands.

Value indexes (slot indexes, the spacetime endpoint and producer
indexes) are not covered: their membership is live. A node hard-removed
from one after E is not enumerated from it at E (#345). The docs name
this bound.

### 3.4 Per-call keywords

`lookup-vertex`, `lookup-edge`, `map-vertices`, `map-edges` gain
`:as-of` (and `:if-reaped`). Given, they run under a fresh as-of
snapshot of the call's graph for the call's extent, with the same
refusals as `with-as-of`. Inside an as-of extent on the same graph at the
same epoch they inherit it, policy included.

### 3.5 `select`

`select` gains `:as-of E` and `:if-reaped`, parallel to `:snapshot t`:
the query runs under `call-with-read-snapshot :as-of E` on `*graph*`.
`:snapshot` together with `:as-of` is a macroexpansion-time error; the
guarded runner does not expose either.

### 3.6 The claim layer

`claims-touching` and `claims-by-producer` under an as-of snapshot read
each candidate through the lookup path, so a claim created after E
resolves to NIL and drops out, one updated (retracted) after E reads as
its version at E, and reaped follows the extent's policy. Their
`:as-of-epoch` axis keeps its own resolution through `vertex-history`
and agrees with the snapshot on the version chosen (both inclusive at E).
Candidate membership stays live (§3.3, #345).

## 4. History

`vertex-history` stays. `edge-history (graph id &key limit)` is its edge
twin, and `node-history (node &key limit)` dispatches on a node's class;
all return `(version . commit-epoch)` newest first. A history is what the
store retains: it never signals; its oldest entry's `revision` above 0
means the chain was cut by reaping, and the docstring says so.

## 5. Surface

New exports from `graph-db`: `with-as-of`, `latest-epoch`, `edge-history`,
`node-history`, `as-of-skipped-count`, `as-of-refused` with readers
`as-of-refused-graph`, `as-of-refused-epoch`, `as-of-refused-reason`;
`version-reaped-error` with readers `version-reaped-id`,
`version-reaped-epoch`, `version-reaped-oldest-epoch`,
`version-reaped-oldest-revision`. Extended: `call-with-read-snapshot`
(`:as-of`, `:if-reaped`), `lookup-vertex`, `lookup-edge`, `map-vertices`,
`map-edges` (`:as-of`, `:if-reaped`), `select` (`:as-of`, `:if-reaped`),
`create-transaction` (`:start-epoch`, internal).

## 6. Testing

In `tests/mvcc-tests.lisp` (suite `mvcc-suite`) unless noted; each names
its mechanism, and the membership tests are the cases today's
`snapshot-hides-nodes-created-after-start` does not cover.

- Values: three updates on one vertex under `:keep-revisions 3`;
  `lookup-vertex :as-of` at each commit epoch returns that version
  (inclusive); at the epoch before creation returns NIL.
- Membership, both directions: a typed `map-vertices` at E excludes a
  vertex created after E and includes one deleted after E; the same at
  the deletion epoch excludes it. The same for `outgoing-edges` on an
  edge created after and one deleted after E, and for an edge whose
  endpoint was deleted after E.
- Consistency: under `with-as-of`, lookups and a typed scan agree with
  each other across a concurrent commit; `select :as-of E` equals the
  same query run at E.
- Reaped: `:keep-revisions 1`, three updates; as-of the first epoch
  signals `version-reaped-error` naming the oldest retained epoch;
  `:if-reaped :skip` returns NIL and `as-of-skipped-count` is 1;
  `:keep-revisions 0` answers only at `latest-epoch`.
- Refusals: `as-of-refused` for an epoch at `(1+ (latest-epoch g))`,
  inside `with-transaction`, a second epoch inside `with-as-of`, inside a
  plain `with-read-snapshot`, an untyped scan, and a memory graph
  (`tests/memory-graph-tests.lisp`); `select` with both `:snapshot` and
  `:as-of` fails to expand.
- Floor: an open as-of snapshot keeps the version chain from being
  reaped, mirroring `read-pin-retains-versions-until-released`; after
  the extent the chain collapses.
- Composition (`tests/multi-graph-tests.lisp`): two stores under one
  system clock, `with-as-of` on both; `*read-snapshots*` holds two
  entries, each store answers at its own epoch, and the entries are gone
  after the extent.
- Claim layer (`tests/spacetime/claim-query-tests.lisp`): under
  `with-as-of E`, `claims-touching` drops a claim created after E and
  returns the pre-retraction version of one retracted after E; its
  `:as-of-epoch E` result agrees on `claim-commit-epoch`.

CI's full SBCL suite is the gate (R5).

## 7. Documentation

- New `docs/time-travel.md`: the epoch model, node-local (#116),
  inclusive semantics, the API, keep-revisions as the depth of time
  travel, reaped reporting, the value-index bound, the tombstone-walk
  cost and C-1 as its successor.
- `docs/mvcc-phase-c-plan.md`: status and the C-3 section point here;
  C-0 through C-2 recorded as the performance track behind a shipped API.
- `docs/transaction-time-design.md`: cross-reference from the two-axes
  section. `docs/vivace-graph-v3-doc.org` chapter 12: a subsection after
  "Snapshot-isolation reads". `README.md`: the MVCC paragraph. `CHANGELOG`
  entry under Unreleased.

## 8. Issues

#115 closes with the merge. #117 records the reordering: C-3 shipped
ahead of C-0 through C-2, which are unchanged in content and become the
performance track. #113 (versioned index) gains a note that its `:as-of`
enumeration replaces the tombstone walk of §3.3 behind this API. #116
stays open as the deferred record.

## 9. Out of scope

- Replica-portable or wall-clock as-of (#116).
- Versioned value indexes (#345) and the untyped scan under an epoch.
- C-0, C-spike, C-1, C-2 (#117).
- The guarded query surface (`docs/guarded-query.md`): no `:as-of` there
  until a consumer asks.
- Writes inside an as-of extent: the snapshot is read-only, as today.
