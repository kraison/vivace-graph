# Engine Proposal: Peer Replication for the Offline Field App

**To:** mine-action (app side)
**From:** vivace-graph-v3 (engine side)
**Re:** the engine asks in `mobile-architecture.md` §11 — what we'll build, what's deferred, what we need from you
**Status:** **APPROVED** by the app side 2026-06-30 (`mine-action/docs/peer-replication-response.md`). Their five decisions and engine concerns are folded into `docs/peer-replication-design.md` v2 (the live artifact); this proposal is kept as the historical outbound record.

---

## TL;DR

Your central worry — *"does VG expose a durable, monotonic change feed with a resumable
cursor, or is that net-new and the single largest piece of engine work?"* — has a good
answer: **it substantially already exists.** VG's master/slave replication is a durable,
ordered, resumable op-log feed today. The engine work is to **generalize** that proven
mechanism into a bidirectional, multi-device, authority-filtered form — not to build a feed
from scratch.

We propose splitting the engine work into two branches:

- **Branch A** delivers your **Phase 1** (sync contract) and **Phase 2** (read-only,
  authority-scoped pull) *end-to-end*, with **zero conflict risk**. A technician can take a
  fully-synced, authority-filtered, read-only site into the field — map, classifier, offline
  reads — before we touch the hard problem.
- **Branch B** delivers your **Phase 3** conflict policy (creates merge / per-field LWW /
  safety-biased regress-only) and the conflict-review surface's engine support.

The full technical design is in the engine repo at `docs/peer-replication-design.md`. This
document is the app-facing summary and the list of decisions we need from you.

---

## How this maps to your engine asks (§11)

| Your ask | Status | What it means |
|---|---|---|
| 1. Durable ordered change feed + resumable cursor | **Exists; generalize** | The replication log + "everything after cursor N" streaming already does this. Net-new: per-origin namespacing, journaling a device's *own* writes, and the push direction. Reclassify from "single largest piece" to "generalize a working mechanism." |
| 2. Authority-scoped subgraph export + import | **Net-new, small** | Built from existing parts: a type-bounded reachability walk (already landed) + your disclosure predicate, emitting the existing portable export format; import is unchanged. |
| 3. Idempotent apply-remote-ops, single-writer-friendly | **Partly exists** | Apply + index/view rebuild exist; creates are already idempotent by UUID. Net-new: decouple apply from the network thread so it runs on your single-writer funnel. |
| 4. Per-field logical-clock versioning | **Net-new — this is Branch B** | The biggest genuinely new piece. Lamport stamps + divergence detection + the three-bucket resolver + conflict records. |
| 5. Snapshot-consistent reads for export | **Done** | MVCC `with-read-snapshot` has landed; export runs under a read snapshot. |

Note your own observation holds: **client-stable UUID identity is already satisfied**, so
creates merge freely and most field activity is conflict-free out of the box.

---

## Answers to your open questions

- **"Change feed: exists today or net-new?"** — Exists as transport + durable log + resumable
  cursor; net-new is only per-origin namespacing, journaling local writes, and the push
  direction. This shrinks your Phase 1 estimate materially.
- **"Plain Lamport vs vector clocks per field?"** — **Plain Lamport (scalar, tie-broken by
  origin id) is provably sufficient.** Hub-and-spoke linearizes at the hub; per-field LWW only
  needs a deterministic total order; safety fields decide by semantic regress-only comparison,
  not by clock. Vector clocks buy only concurrency *detection*, which your policy never needs.
  **Recommend committing to plain Lamport.**
- **"Authority filtering granularity?"** — Expressible as a predicate over the type-bounded
  reachability walk (edge-type bound + per-vertex authority check). Start with an
  edge-type/property predicate; escalate to per-vertex ACL only where a property predicate
  can't express the rule. Both options stay open; the predicate lives in *your* code, the
  engine stays domain-agnostic.

---

## What the engine commits to (Branch A)

1. A **device-origin change feed**: a device durably journals its own committed writes into a
   resumable, per-origin op log — the same mechanism the master uses for slaves today.
2. A new **`peer-graph`** abstraction (hub and device roles), **separate from master/slave** —
   the existing master/slave replication is left exactly as it is and keeps working.
3. A **symmetric peer session**: one connection, pull + push, dual resumable cursors, resumes
   from the last committed cursor after a dropped link.
4. **Authority-scoped export** *on the hub, before bytes leave* — your §8 disclosure rule
   ("the device must never *contain* data the technician isn't cleared for") enforced at the
   source, not by UI hiding. Both the initial site working-set seed and the ongoing feed are
   filtered by the device's authority.
5. **Re-homing semantics**: the hub applies device ops as new hub-origin transactions and
   re-journals them, so other devices see them — while the device's authorship is preserved in
   the op's conflict metadata for Branch B.

Out of Branch A, into Branch B: per-field merge, safety-biased resolution, conflict records +
the enumeration API your review surface reads. The Branch A wire format **reserves space for
the Lamport stamp + origin now**, so Branch B adds merge without breaking the protocol.

## What is explicitly *not* engine work

Per your doc, these stay on the app/schema/OS side and the engine deliberately stays out:
the **single-writer funnel** (app discipline — the engine just exposes an apply seam),
**blobs/photos** (content-addressed outside the heap; the graph stores only a hash),
**map tiles / PMTiles** (fully outside VG), **encryption-at-rest** (Android Keystore over the
VG files), and **GPS position provenance** (a `def-vertex` schema addition on your side).

---

## What we need from you to proceed

In rough priority:

1. **The per-field conflict-bucket enumeration for the whole v2 schema** (your §5). This is the
   single most important input to Branch B, and your doc already calls it the most important
   design artifact. Every field → one of: *create (merge)* / *scalar (per-field LWW)* /
   *safety-critical (regress-only + surface)*. We need this before writing merge code; it can
   proceed in parallel with Branch A.
2. **Confirm plain Lamport** as the logical clock (we recommend it; see above).
3. **Authority predicate shape**: can disclosure be expressed as an edge-type / property
   predicate over the working-set traversal, or do you need per-vertex ACL evaluation at export
   time? (Drives the export filter's interface. Either is supportable; we want your lean.)
4. **Device registry ownership**: the hub mints + stores device origin UUIDs and their
   authority scope. Is that registry app-owned (we lean here — engine just consumes origin-id +
   predicate + cursors) or should the engine hold it?
5. **Membership-change + boundary-edge policy** for the ongoing filtered feed: when a node is
   edited *into* a device's authority scope it must ship as a create (not a skipped update);
   and an edge whose far endpoint is outside scope needs a rule (ship a read-only stub vs.
   widen the set). We'll propose defaults; we want your safety read on them.

---

## The phasing this enables (matches your §12)

- **Phase 1 (sync contract):** Branch A feed + cursors + peer session. Engine-led.
- **Phase 2 (read-only field app):** Branch A authority-scoped pull. **Shippable and useful on
  its own** — read-only site in the field, zero conflict risk. Engine-led.
- **Phase 3 (writes + push + conflict v1):** Branch B. Needs your schema bucketing (ask #1).
- **Phases 4–6 (background replicator, security, GPS-denied):** mostly app/Android/schema; the
  engine pieces (resumable cursor, apply seam) land in Branches A/B.

---

## The ask

**Approve this scope and the Branch A/B split.** On approval we begin pressure-testing the
engine design here — a skeptical pass on the re-homing/idempotency interaction and the
membership-change edge case in particular — and turn Branch A into an implementation plan with
concrete function signatures. Decisions #1–#5 above can be gathered in parallel and don't block
the start of Branch A.
