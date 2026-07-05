# Peer Replication — Branch A Design (resumable per-origin change feed + bidirectional peer transport)

**Status:** design v2. App side **approved** the scope + Branch A/B split (`mine-action/docs/peer-replication-response.md`, 2026-06-30). This revision folds in their five decisions and the engine-affecting concerns, plus the first pressure-test findings. Branch `peer-replication` off `experiment`. Nothing built yet.
**Companions:** `mine-action/docs/mobile-architecture.md` (app architecture), `docs/peer-replication-proposal.md` (the approved proposal).

Branch A delivers app **Phase 1** (sync contract) and **Phase 2** (read-only authority-scoped
pull → usable field app, zero conflict risk), and lays the foundation Phase 3 (push + conflict,
**Branch B**) builds on. Branch B is *reserved for* throughout, not built here.

---

## 1. Locked decisions

From the discussion and the app side's response:

1. **Origin identity = a hub-minted, hub-stored UUID per device.** Engine treats origin as an
   opaque 16-byte id. The device registry (`device → person/team → clearance scope`) is
   **app-owned**; the engine consumes `origin-id + predicate + cursors` only.
2. **Star topology; devices never sync peer-to-peer.** Every cursor is a **scalar per directed
   feed**, never a vector. No vector clocks anywhere.
3. **A new `peer-graph` abstraction, separate from `master-graph`/`slave-graph`.** The existing
   master/slave classes and protocol are left **byte-for-byte as they are**.
4. **Logical clock = plain Lamport** (scalar, tie-broken by origin id). Confirmed sufficient:
   hub-and-spoke linearizes at the hub, creates are UUID-keyed, and safety fields resolve by
   *semantics, not clock* (§11). Vector clocks buy only concurrency detection we never need.
5. **Authority filter = a per-vertex `disclosable-p` predicate that may do bounded graph reads**,
   evaluated under the export read snapshot. Disclosure is *not* a local slot — it rides a 2-hop
   tasking walk (`eo-find →find-observed-in→ survey →survey-tasked-under→ project[disclosure]`).
   The predicate stays in app code; the engine supplies the traversal seam + snapshot.
6. **Closed-subgraph export, fail-closed.** Ship a vertex only if `disclosable-p`; ship an edge
   only if **both** endpoints are disclosable; otherwise **omit** (never a read-only stub — a
   stub leaks the existence/id of undisclosed work). Scope changes emit **creates** (entered
   scope) and **purges** (left scope), the latter distinct from deletes.

---

## 2. What already exists (build on, don't rebuild)

The master/slave path is already a durable, ordered, resumable change feed. Branch A
*generalizes* it; it does not start from scratch.

| Capability | Where | Reuse |
|---|---|---|
| Durable ordered op log, monotonic, gap-free per graph | `replication-*.log`, `transaction-log-streaming.lisp` | The per-origin feed. The per-graph `tx-id` **is** the per-origin sequence. |
| Resumable "everything after cursor N" | `applicable-replication-logs`, `stream-replication-log` (`transaction-log-streaming.lisp:55`) | Pull cursor mechanism, unchanged. |
| Logical, address-free, UUID-keyed op format | `serialize-transaction-node` / `deserialize-transaction-node-vector` (`transactions.lisp:1157`, `:1222`) | The wire unit; heap addresses re-derived locally on apply. |
| Receive-and-persist a streamed txn | `stream-transaction-to-disk` (`transaction-streaming.lisp:349`) | Reused in **both** directions. |
| Apply with full index/view/spatial rebuild | `apply-transaction` / `apply-tx-writes` (`transactions.lisp:958`) | Apply primitive on both ends. |
| Tombstone propagation | `tx-delete` (`transactions.lisp:636`) | Authored deletes (but note §11.B: deleting an `eo-find` is a *safety* event). |
| Logical export / import (portable plists) | `backup` / `recreate-graph` (`backup.lisp:38`, `transaction-restore.lisp:46`) | Basis for the authority-scoped seed. |
| Type-bounded reachability | `map-edges`/`map-vertices` edge-type lists (commit `b4ea1b0`) | Bounds the working-set walk. |
| Snapshot-consistent reads | MVCC `with-read-snapshot` (landed) | Export + `disclosable-p` run under one snapshot. |

---

## 3. Four identities — keep them distinct

The single most important conceptual discipline. Conflating any two of these is a class of bug.

1. **Node UUID** — identity of *the thing* (vertex/edge). Already in VG. Idempotency key for
   **state-sync** ops (§5).
2. **Original-op id** — identity of *an authored change event*. A UUID minted by the authoring
   replica, **stable across re-homing** (the hub does not re-mint it). Dedup key so an op a device
   authored, that bounces back via the hub feed, is recognized and not re-applied (app concern D).
   New wire field in **Branch A**.
3. **Per-origin feed sequence** — *position in a directed feed*. The existing per-graph `tx-id`
   (`assign-transaction-id`, `transactions.lisp:160`). Drives the resumable cursor. Re-homing
   re-assigns it (a device op gets a fresh hub-seq on the hub feed).
4. **Lamport stamp** — *conflict order* for per-field LWW. Travels with the field and **survives
   re-homing**. Branch B acts on it; Branch A reserves the wire slot (may be 0).

Identity #2 (dedup) and #4 (ordering) are different jobs: op-id answers "have I applied this
event?"; Lamport answers "which of two conflicting values wins?". They are never the same field.

---

## 4. Two op classes — authored vs state-sync

The pull feed carries two kinds of traffic, with different rules. Separating them resolves the
purge/delete/re-entry hazards (§11, pressure-test 2).

- **Authored ops** — a user/automation change. Carry origin-id + **original-op id** + Lamport.
  Deduped by op-id, mergeable (Branch B), and **bidirectional** (devices author them; the hub
  re-homes them). Creates, field edits, authored deletes.
- **State-sync ops** — the hub asserting *current membership* of a device's authority scope.
  Keyed by **node UUID** (idempotent by identity), **hub→device only**, **never deduped by op-id,
  never propagated back, never merged**. Two forms: a **membership-create** (a node that entered
  scope ships as a full create) and a **purge** (a node that left scope is removed locally). A
  purge means "remove from *your* replica"; the node still exists on the hub.

This is why a purge followed by a later re-entry works: the re-entry is a node-UUID-keyed
state-sync create, not an op-id-deduped authored replay, so stale dedup state can't suppress it.

---

## 5. Re-homing (the conceptual core, refined)

The hub is system of record. Sync is **not** a symmetric merge of two equal logs:

- **Pull:** the device consumes the hub's canonical feed (authored ops, origin=hub) **plus** the
  hub's state-sync ops for that device's scope.
- **Push:** the device ships its locally-authored ops; the hub **applies them as new hub-origin
  transactions and re-journals them** under fresh hub-seqs, **preserving the original-op id +
  origin + Lamport**. Device E later pulls them as hub-origin authored ops.

A find authored on device D as `(node=F, op-id=OP, origin=D, D-seq=5, lamport=L)` becomes, after
the hub applies it, `(node=F, op-id=OP, origin=D, hub-seq=9001, lamport=L)` — same op-id and
authorship, new feed position. **Only the hub re-journals.** A device journals its *own authored*
writes (for push) but **never re-journals ops it pulled** — that asymmetry (hub journals, device
applies) is what prevents an infinite loop. It mirrors master/slave, where the master journals and
the slave applies; the only addition is that a device *also* journals its own authored writes.

---

## 6. Cursors + dedup (hub-and-spoke ⇒ scalars)

- **Device** persists: `pull-cursor` (highest hub-seq applied), `push-ack` (highest D-seq the hub
  acknowledged → drives retransmit), and an **applied-op-id index** (for bounce-back dedup).
- **Hub**, per device D, persists: `apply-cursor` (highest D-seq applied from D — idempotency on
  re-delivery), a **per-device manifest** (set of node-ids currently in D's scope → drives
  membership diff, §7), and optionally `pull-ack` (for feed pruning). Plus its own applied-op-id
  index.

The device `pull-cursor` is precisely today's slave `highest-transaction-id` handshake
(`transaction-streaming.lisp:339`). Branch A adds the symmetric `push-ack`, the op-id index, and
the manifest. **Cursors advance only after the op commits**, so a dropped link resumes from the
last committed cursor; the op-id index makes a resumed overlap (or a bounce-back) idempotent even
when the cursor alone would re-apply.

Applied-op-id index growth is bounded by pruning below a confirmed low-water (a device may forget
an authored op-id once it has observed it bounce back; hub op-ids are covered by the pull cursor).
Detail for implementation, not a blocker.

---

## 7. Authority-scoped export — closed subgraph + membership diff

§8 of the app doc is non-negotiable: a captured device must never *contain* undisclosed data, so
filtering happens at **export, on the hub**, before bytes leave (not apply-side like the existing
`slave-graph` `replication-filter`).

**The seam.** The engine calls an app-supplied predicate `(disclosable-p vertex graph
device-scope) → boolean`, evaluated under the export read snapshot, **allowed to do bounded graph
reads** (follow `survey-tasked-under` to the project). Kept as a **first-class standalone
argument** — the app reuses the same predicate for the CoT/ATAK projection and web rendering
(app concern F), so the export filter must not bury it inside the replication path.

**Invariant the predicate must satisfy (pressure-test 2):** `disclosable-p` must be
**downward-closed along the disclosure-determining edges** — if a node is disclosable, the
ancestor it derives disclosure from (its tasking project) is disclosable and ships too. Otherwise
closed-subgraph would omit a structural edge the device needs (a survey shipped without its
tasking edge). The app's chain satisfies this (a survey is disclosable *because* its project is),
but it must hold by construction; we state it as a contract on the predicate.

**Two mechanisms, because op-log filtering can't see scope *exits*:**

1. **Manifest reconciliation (membership).** On each connection, recompute the device's
   authority-scoped **closed subgraph** under one read snapshot and diff against the stored
   per-device manifest → emit **membership-creates** (in manifest? no, in scope? yes),
   **purges** (in manifest, no longer in scope), and updates for changed-and-still-in nodes. This
   catches re-tasking (a survey moved `hma-project → ad-hoc-project` purges that survey *and its
   transitively-dependent finds/edges* from a device cleared only for HMA). Usually the diff is
   empty (cheap); cost is bounded by the working-set size, acceptable at site scale.
2. **Cursor-resumed authored-op stream (volume).** After reconciliation, stream the hub's
   in-scope authored ops after `pull-cursor` for the high-volume normal activity, filtered by
   `disclosable-p`. Keeps resumability for the common case.

**Seed** (initial working set) is mechanism 1 from an empty manifest: a filtered `backup` over the
type-bounded reachability walk + `disclosable-p`, replayed on the device via `recreate-graph`.

**Purge vs delete vs structural-edge-removal** — three distinct semantics, kept separate:
- *Purge* (§4 state-sync): hub→device scope-exit, node lives on hub, not propagated back.
- *Authored delete* (Branch B, app concern B): e.g. `eo-find.deleted NIL→T` is a **safety** event
  that surfaces and propagates — not a silent tombstone.
- *Structural edge removal*: UUID-keyed tombstone, create-merge bucket.

---

## 8. The `peer-graph` class and session

A sibling class, not a subclass of master/slave:

```
(defclass peer-graph (graph)
  ((peer-role         :reader peer-role)        ; :hub or :device
   (origin-id         :reader origin-id)        ; this replica's 16-byte UUID
   (peer-endpoint     ...)                      ; device: hub host/port
   (device-registry   ...)                      ; hub: app-owned; origin-id -> {scope, cursors, manifest}
   (export-predicate  ...)                      ; the disclosable-p seam (app-supplied)
   (applied-op-ids    ...)                       ; durable dedup index
   (stop-replication-p ...)
   (peer-thread ...)))
```

Symmetric session (one connection, both directions), modeled on but distinct from
`make-slave-session-handler` / `slave-loop` (`transaction-streaming.lisp:260`, `:413`):

1. **Handshake.** Reuse the plist handshake + **schema-digest check** verbatim
   (`transaction-streaming.lisp:317`) — partial replicas are *data*-filtered, not *schema*-filtered,
   so both ends carry the same v2 schema and digest. Device presents origin-id + `pull-cursor` +
   `push-ack`; hub authenticates against the registry and loads the device's scope + `apply-cursor`
   + manifest.
2. **Pull.** Manifest reconciliation (§7.1) then cursor-resumed in-scope authored ops (§7.2).
3. **Push.** Device streams authored ops after `apply-cursor`; hub receives with
   `stream-transaction-to-disk` and routes each to the **re-home apply** (§5). Branch A wires the
   transport + the *simple* re-home apply (last-applied-wins); Branch B swaps in the merge. In
   Phase 2 the device is read-only, so push exists but is exercised only once Phase 3 lands.
4. **Cursor + manifest advance**, post-commit (§6).

`start-peer` / `stop-peer` mirror `start-replication` / `stop-replication` on the new class,
leaving master/slave methods untouched.

---

## 9. Concrete change set (Branch A)

Rough dependency order:

1. **Peer journaling.** Generalize the journaling condition in `finalize-tx-persistence`
   (`transactions.lisp:1517`) from `(when (master-graph-p (graph tm)) …)` to also fire for a
   `peer-graph` device — so a device journals its own committed writes into a push feed exactly as
   a master journals for slaves. Smallest, highest-leverage change.
2. **Original-op id on the wire.** Add the stable op-id (identity #2) to the peer op header, minted
   at authoring, preserved through re-homing; add the durable **applied-op-id index** + dedup check
   before apply.
3. **`peer-graph` class + make/open/close plumbing** (`graph-class.lisp`, `graph.lisp`), reusing
   `init-replication-log` / `close-replication-log` (`graph.lisp:146`, `:294`).
4. **Peer protocol v1** — a new protocol constant distinct from `*replication-protocol-version*`,
   carrying origin-id + op-id + reserved Lamport from day one, plus the **op-class tag**
   (authored vs state-sync) and a **purge** op type.
5. **Symmetric peer session** (§8).
6. **Re-home apply** (§5): wrap received remote writes as a fresh local-origin transaction, commit
   through the normal path so they re-journal, preserving op-id + origin + Lamport. Branch A simple;
   Branch B merges.
7. **Authority-scoped export** (§7): `disclosable-p` seam (standalone, snapshot-evaluated, bounded
   reads); closed-subgraph filtered `backup`; per-device manifest + reconciliation diff producing
   creates/purges; cursor-resumed in-scope op stream.
8. **Apply decoupled from the socket thread.** Today `slave-loop` applies inline on the receive
   thread (`transaction-streaming.lisp:445`). For the device single-writer funnel (app §6), peer
   apply must be a callable the app's writer thread schedules — the receive thread only enqueues.
   **Required for funnel correctness on ECL, not just ergonomic** (PT-5).
9. **Schema *compatibility* handshake** (PT-6). Replace digest-equality with a semver-style
   compatibility check so an older-but-compatible offline device can at least *push* authored data
   (never strand field-sourced safety regressions). Pairs with an app-side additive-only schema
   discipline.

Two crash-consistency invariants thread through the above: the **applied-op-id index update is
atomic with the apply** (PT-3, part of item 2), and the **per-device manifest advances only on
device ack** (PT-4, part of item 7).

Out of Branch A: everything in §11; plus app/OS/schema items (single-writer funnel, blobs, tiles,
encryption-at-rest, GPS provenance).

---

## 10. Forward hooks reserved for Branch B (no wire break later)

- Peer op header already carries origin-id + op-id + a Lamport slot.
- Re-home apply is a single generic function; Branch B specializes it to detect divergence and
  dispatch on the field's **conflict bucket** (§11).
- Conflict retention reuses MVCC `archive-node-version` to keep the loser; a `conflicts` index +
  enumeration API (read by the app review surface) lands in Branch B.

---

## 11. Branch B notes (folded from the app response; not built in Branch A)

The app's per-field bucketing (`mine-action/docs/peer-replication-response.md` "Schema bucketing
v1", all `(confirm)` items resolved 2026-06-30) drives Branch B.

**Five resolver primitives** (the buckets collapse to these — the merge dispatch is a switch over
them, keyed by an app-declared `(node-type, field) → bucket` registry):

| Primitive | Buckets | Rule | App seam |
|---|---|---|---|
| **merge-by-UUID** | C, structural edges | Union by node/edge UUID; never conflicts. | — |
| **clock-LWW** | L | Higher (Lamport, origin) wins, per field. | — |
| **meet-on-rank** | **S-ordered** | Auto-apply toward danger (the meet); surface toward safe. Ignores Lamport. | **danger-rank fn per field** |
| **surface-on-divergence** | **S-surface** *and* **G** | Apply if no divergence; if divergent, surface + record both, **no auto-winner**. | divergence/equality test |
| **immutable** | I | Reject post-create edits. | — |

- **A. Safety merge resolves by *semantics, NEVER by Lamport* — and S has two flavors.**
  *S-ordered* (`hazard-area.hazard-status`, `hazard-zone.disposition`, `eo-find.confidence`,
  `site.work-stage`) has a dangerous→safe ordering; merge = greatest-lower-bound toward danger
  (commutative/associative/idempotent ⇒ **order-independent convergence without relying on
  Lamport**, CRDT-ish; a regression toward danger wins even with a *lower* Lamport). Each
  S-ordered field needs an **app-supplied danger-rank function** over its value domain (a second
  domain seam alongside `disclosable-p`). *S-surface* (`eo-find.find-of-type` — an ordnance
  taxonomy with no linear danger order) has **no auto-winner**: any divergence is surfaced and
  both values recorded. **Mechanically S-surface ≡ G** (geometry): same "apply-if-agree,
  surface-if-diverge" resolver. **Merge dispatch keys on the field's bucket; an S field never
  falls through to clock-LWW.** Advancing toward "released" only ever happens by an explicit
  authored action; a toward-safe S-ordered merge still records a surfaced conflict.
  - **Blocked:** `site.work-stage` is S-ordered but its IMAS stage set is a free-form string
    today — app must enumerate + rank it before that field's merge can be coded.
  - **Unknown-value contract (value-domain drift, §14):** when the danger-rank fn cannot rank an
    incoming S-value (a new ordnance subtype / work-stage label the device's code predates), the
    resolver **treats it as lattice-bottom (maximally dangerous) and surfaces — never auto-advances
    toward safe**. So value-domain growth is made safe by the *resolver*, not by the digest.
- **B. `eo-find.deleted` is a safety field, not a structural tombstone.** `NIL→T` (hiding a possible
  danger marker) **surfaces**; `T→NIL` (restoring) auto-applies. This is an *authored op* (§4),
  distinct from a disclosure *purge* (§7).
- **C. Geometry (G) surfaces on concurrent edit; never silent-LWW a polygon**
  (`hazard-zone.boundary`, `site.extent`, `survey.boundary`, `nts-observation` position, and
  **`eo-find` position `lat/lon/mgrs/geom` — now correctable, so bucket G**;
  `hazard-area.original-boundary` stays immutable). Single-editor edits (no divergence) apply
  normally. Same resolver as S-surface (above).
- **E. Conflict records retain the loser** — value + origin + Lamport + authoring person — for *all*
  surfaced conflicts (accountability is first-class). The enumeration API exposes losers, not just
  current state ("X set *cleared* at T; Y concurrently set *CHA*; we kept *CHA*").
- **Bucket registry seam.** The engine needs an app-declared map `(node-type, field) → bucket`
  (and for deletes, `node-type → delete-bucket`, since deleting an `eo-find` is S but removing a
  structural edge is C), plus the per-S-ordered-field **danger-rank fn** and per-S-surface/G-field
  **divergence test**. The merge dispatch reads the registry + seams; all policy stays in app
  config. Mirrors the `disclosable-p` seam pattern (§7) — engine generic, domain in app code.
- **`project.disclosure` is L-but-alarm:** a scalar edit, but a change **re-scopes devices** →
  triggers membership reconciliation + purge (§7). A *feed* problem, not a merge problem.

App `(confirm)` items still open (do not block Branch A): `find-of-type` LWW vs surface;
`site.work-stage` safety-or-not; `nts-task.reporting-status`; `hazard-zone` (re)signature atomic
with `disposition`; whether a corrected `eo-find` position is permitted at all.

---

## 12. Pressure-test log

**PT-1 — re-homing ↔ idempotency (app concern D): PASS, with required invariants.**
Sound iff: (a) a stable original-op id rides the wire from authoring through re-home and back
(§3 #2); (b) every replica checks a durable applied-op-id index before apply (§6); (c) a device
never re-journals pulled ops — only authored ones (§5); (d) op-id / Lamport / per-origin-seq /
node-UUID are kept distinct (§3). Verified failure modes handled: hub crash before `apply-cursor`
advance → re-push re-deduped by op-id (cursor alone would double-apply — this is *why* op-id dedup
is required); concurrent third-device edit to the same node pulls through without loss.

**PT-2 — membership change / closed-subgraph / purge (decision #5): PASS, surfaced 3 refinements
now folded in.** (1) `disclosable-p` must be **downward-closed** along disclosure-determining edges,
else closed-subgraph dangles (§7 invariant). (2) Scope *exits* can't be detected by op-log
filtering → need **manifest reconciliation** (§7.1); a single re-task purges a transitive subtree.
(3) Purge/re-entry only works because membership traffic is **node-UUID-keyed state-sync, not
op-id-deduped authored replay** (§4) — otherwise stale dedup state would suppress a re-entry.

**PT-3 — crash consistency of durable sync state: PASS, one hard invariant.** Each replica now
keeps several durable artifacts beyond the graph: the push feed, `pull-cursor`, `push-ack`, the
**applied-op-id index**, and (hub) the per-device manifest. Walking the crash matrix: every torn
write is covered by *re-delivery + op-id dedup* **iff the applied-op-id index entry is committed in
the same transaction as the apply**. If it is a separate write, a crash between graph-commit and
index-write re-applies on restart — harmless for creates and idempotent merges (meet, same-stamp
LWW), but **not** for surface-on-divergence (S-surface/G), which would re-raise an already-resolved
conflict. ⇒ **Invariant: op-id index update is atomic with the apply; cursors/acks/manifest advance
strictly post-commit.** (Branch B corollary: conflict records need op-id-pair idempotency.)

**PT-4 — manifest must track ACKED device state, not hub-intended state: PASS, disclosure-critical
invariant.** If the hub advances a device's manifest when it *sends* a purge (not when the device
*acks* it), a link drop mid-purge leaves the hub believing D no longer holds survey S while D still
holds it; the next diff sees S neither in-scope nor in-manifest and **emits no purge — D retains
undisclosed data forever (fail-closed VIOLATION).** ⇒ **Invariant: the per-device manifest advances
only on device ack of the specific create/purge.** Safe because purges are idempotent state-sync
(PT-2): over-emitting a purge is harmless, under-emitting is a disclosure breach — so the discipline
is "never advance manifest until acked," which can only over-emit. Surfaced prominently to the app
side.

**PT-5 — single-writer funnel ↔ OCC (device vs hub asymmetry): PASS, elevates §9.8 to required.**
*Device* is single-writer (UI + applied remote ops), so OCC never contends — correct by
construction — **but only if the receive thread enqueues to the writer instead of applying inline**
(today's `slave-loop` applies inline, `transaction-streaming.lisp:445`). Inline apply = a second
writer = OCC contention on ECL's weak spot. ⇒ **§9.8 (decouple apply from socket thread) is required
for funnel correctness on ECL, not just ergonomics.** *Hub* is multi-writer (many device sessions +
web app) via the existing OCC/retry. Branch A (last-applied-wins) is fine. **Branch B corollary: the
re-home merge must run read-merge-write *inside* the transaction body**, so an OCC retry re-merges
against fresh current state (a precomputed merge would retry against stale state). Perf note: re-homing
multiplies hub write-transaction volume → relevant to the commit-lock convoy work; flag for the Stage 5
perf suite.

**PT-6 — schema drift while offline: FAIL as designed; needs a Branch A change.** The handshake
requires schema-digest *equality* (`transaction-streaming.lisp:317`). A device offline across a
`def-vertex`/`def-edge` change reconnects with a stale digest → handshake fails → it can neither pull
nor push, **stranding possibly weeks of authored field data — including safety regressions.** For an
offline-first safety system that is unacceptable. ⇒ **Branch A must replace digest-equality with a
schema *compatibility* check**: hard-gate only on **non-additive** (major) change; **additive**
change (incl. value-domain growth) is handled degraded-safe, not blocked. The full policy — push
lossless/always-accepted, pull degraded-safe, value-domain drift, and the drain-and-update barrier —
is §14 (app input 2026-06-30).

**PT-7 — edge conflicts: the bucketing model is vertex-field-shaped; edges need their own model.
Surfaced a modeling fork for the app.** Confirmed VG edges are **endpoint-immutable** (`from`/`to`
in the indexed edge head, `edge.lisp:11`; `ve-`/`vev-index` keyed on them; no re-target path, no
functional-edge concept). Three findings:
1. **`find-of-type` reclassification is delete+create, not a field edit.** "F is IED not AXO" =
   tombstone edge(F→AXO) + create edge(F→IED). Two devices reclassifying differently leave F with
   **multiple live `find-of-type` edges** — the divergence manifests as edge *multiplicity*, which
   per-field S-surface comparison cannot see (there is no single "edge target" to compare).
   **Decision fork for the app:** (a) model classification as a **vertex reference slot**
   (`find-of-type-id`) on `eo-find` → folds cleanly into S-surface field-merge; or (b) add
   **functional-edge semantics** to the engine (at-most-one live edge of a type from a node;
   a concurrent second surfaces) — net-new, broader machinery. **Lean: (a)** — "which type" is a
   functional attribute, not a multigraph relationship.
2. **Structural edge add/remove is an OR-Set problem.** Concurrent add(X) vs remove(X) → add-wins or
   remove-wins? Default remove-wins (last tombstone) is fine for most structural edges, **but a
   removal that drops a *safety-bearing* linkage should surface, not silently win** — so edge
   *removal* needs a per-edge-type bucket, mirroring the node delete-bucket.
3. **Concurrent re-task is multiplicity AND disclosure-scope conflict at once** (the spicy case).
   `survey-tasked-under` retask = remove old + create new tasking edge; two devices retasking S to
   different projects leave S tasked under two projects, so **S's scope membership becomes
   ambiguous — which devices hold it?** This couples PT-2 membership with edge-multiplicity.
   Resolution must be deterministic + **fail-closed**: `disclosable-p` evaluated over the (possibly
   multi-tasking) current edge set must withhold S unless cleared for *all* its taskings, and the
   retask conflict surfaces for human resolution. App defines the rule; engine just evaluates
   `disclosable-p` over whatever edges currently exist.

**PT-8 — Lamport persistence / monotonicity across restart: PASS, with invariants; clock skew is a
non-issue by design.** The replica's Lamport counter must be **durable and monotonic across
restart** — if it resets to 0 after a crash, the device's post-restart authored ops get tiny stamps
and **silently lose every LWW race (safety data dropped)**. ⇒ persist the counter (or recover a safe
lower bound = max stamp in the durable feed). On receiving any op, advance to
`max(counter, received-stamps)+1`; on the hub, re-homing advances the hub counter past incoming
device stamps **while preserving each field's authored stamp** (local-counter vs per-field-stamp
stay distinct — the four-identities discipline, §3). **Wall-clock skew is irrelevant** because
Lamport is logical — a real benefit given EW/GPS-denied field devices may carry a bad RTC.

**Positive finding — divergence detection rides existing wire data.** Per-field divergence detection
(for L/S/G) needs the *base* the author saw. The existing `tx-update` **already ships `old-node`**
(`transactions.lisp:631`), so divergence = "does my local current field differ from the op's
`old-node` field?" The base is already on the wire — **Branch B needs no new per-field base markers**,
reducing its wire churn.

---

## 13. Open items for detailed design

- **Manifest reconciliation cost/triggering:** every-connection full diff (v1, simple, correct) vs.
  change-driven incremental re-eval (optimization). Lean v1 = full diff under snapshot.
- **Applied-op-id index pruning** low-water policy (§6).
- **Push-ack protocol detail:** explicit ack packet vs. piggyback on next handshake.
- **Op-class + purge wire encoding** in peer protocol v1 (§9.4).
- **Lamport counter durability** (PT-8): persist + monotonic-on-recover. Slot reserved in Branch A,
  activated in Branch B; the persistence must land wherever stamps are first minted.
- **Edge-conflict model (PT-7) — needs app decisions:**
  - `find-of-type` modeling fork: vertex reference slot (lean) vs. functional-edge engine support.
  - Per-edge-type **removal bucket** (remove-wins vs. surface) for safety-bearing edges.
  - `disclosable-p` over a **multi-tasking** survey: fail-closed resolution + retask conflict surfacing.
- App-side deliverables (from their response): IMAS `work-stage` value set + danger rank (unblocks
  that S-ordered field); `disclosable-p` signature against the export seam; device-registry schema.

---

## 14. Schema evolution, value-domain drift & the push/pull asymmetry (app input 2026-06-30)

Schema change has **two axes**, and the structural digest only sees one:

- **Structural drift** — new/removed types and fields. *Additive* (new type/field) is safe;
  *non-additive* (removed field) is breaking.
- **Value-domain drift** — a new *value* in an existing field's domain (a new ordnance subtype, a
  new `work-stage` label). **Structurally additive (just a new vertex/value) yet invisible to a
  structural digest** — and it lands on the S buckets. This fires *routinely* (the ordnance taxonomy
  grows often); it is not an edge case.

### The governing asymmetry

- **Push = lossless + always-accepted.** A device's field-sourced data (especially safety
  regressions) must never be stranded or dropped by a schema gap. The push path accepts any
  compatible (non-additive-equal) device.
- **Pull = degraded-safe.** A stale device pulls what it can and **preserves unknown fields/types
  on round-trip** (it must not drop a field it doesn't understand, or a later push would erase the
  hub's newer data), and is told **it is behind**.

### Engine requirements this creates

1. **S-resolver unknown-value handling (§11.A).** Unknown S-value ⇒ lattice-bottom (max danger) +
   surface; never auto-advance. This is what makes value-domain drift safe *without* the digest
   seeing it. On a **device** the degraded behavior is "render as max-danger/unknown + flag behind";
   the **hub** (current schema) does the real S-merge.
2. **Per-field, schema-version-aware merge (mandatory; kills whole-node overwrite for Branch B).**
   `tx-update` ships the whole node today; a stale device's whole-node push omits the hub's newer
   fields. The hub must merge **per field**, treating a field **outside the author's schema version
   as "not addressed" → hub value preserved** (distinct from "author cleared it to nil"). Requires
   the hub to know the **author's schema version**. (Branch B; Branch A is read-only pull, unaffected.)
3. **Hub fills safe defaults** for additive fields a stale device omits on create — and an
   **S-field's safe default is its max-danger value**, so an omitted safety field lands dangerous,
   never nil.
4. **Compatibility gate, not equality gate** (refines PT-6): hard-block only on non-additive (major)
   mismatch; additive/value-domain drift passes through degraded-safe.

### Non-additive changes: the drain-and-update barrier

Additive-only handles normal evolution, but **genuine non-additive changes are planned** — the
satellite-table extractions (boundary/footprint/QA pulled off `survey`/`flight`/`hazard-area`)
**remove inline fields → breaking**. These must be **sequenced through a hard barrier, never slipped
in**: do not deploy a non-additive hub schema while any device may hold un-pushed old-schema data.

**Operational truth to state plainly: you cannot drain a device you cannot reach.** The hub tracks
push-ack per device, but a device offline for weeks has un-pushed data the hub can't see. So a
non-additive deploy requires **physical reconciliation of every *enrolled* device** (recall /
in-person update for field units), not just "all connected devices drained." The engine's part is to
**expose the signal** — per-device last-pushed-schema-version + an "un-drained / behind" flag, from
the app-owned registry — so the barrier can be enforced operationally; it does not auto-resolve.

### Split across branches

- **Branch A:** compatibility gate (#4) replacing digest-equality; per-device last-pushed-schema
  version exposed for the barrier signal. (Pull is read-only, so round-trip preservation isn't yet
  exercised, but the wire/version plumbing is laid here.)
- **Branch B:** per-field schema-version-aware merge (#2), unknown-value S handling (#1), safe-default
  fill (#3).
- **App/operational:** safe-default + max-danger values per field; the drain-and-update deploy
  discipline; "you're behind" UX.
