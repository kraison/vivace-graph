# Peer Replication — Branch A Implementation Plan

**Status:** plan, pre-code. Branch `peer-replication` off `experiment`. Companion to
`docs/peer-replication-design.md` v2 (the design; section refs below point into it).
**Scope:** app Phase 1 (sync contract) + Phase 2 (read-only authority-scoped pull). Push transport
+ simple re-home apply are wired and unit-tested, but the device is **read-only** until Branch B —
so the merge is a stub (last-applied-wins) and per-field conflict handling is out of scope here.

The PT invariants from design §12 are **hard requirements**, called out inline as `[PT-n]`.

---

## 0. Dependencies already in place

- MVCC landed → `with-read-snapshot` for consistent export (design §2, ask 5: done).
- Existing transport reused verbatim: `read-packet`/`write-packet`/`read-plist-packet`/
  `write-plist-packet`, the 8-byte-size packet framing, `stream-transaction-to-disk`
  (`transaction-streaming.lisp`).
- Existing op serialization reused **unchanged inside the peer envelope**:
  `serialize-transaction-node` / `deserialize-transaction-node-vector`, the `tx-header` +
  `tx-write` format (`transactions.lisp:1118`+).
- master/slave classes + protocol are **not touched** (design §1.3).

New source file `peer-streaming.lisp` (sibling to `transaction-streaming.lisp`); `replication.lisp`
is currently empty and is a candidate home for the small class/lifecycle bits. Add the new file to
`graph-db.asd` immediately after `transaction-streaming.lisp` in the load order.

---

## 1. Peer protocol v1 — wire format (WP-0)

A new packet type, distinct from the master/slave packets, so the existing protocol is untouched.
The peer envelope **wraps** an existing serialized tx (header + writes); we reuse that machinery and
only add peer metadata around it.

```
+peer-protocol-version+   = 1            ; independent of *replication-protocol-version* (=2)
+peer-tx-type-code+       = char-code #\P
+peer-ack-type-code+      = char-code #\A

peer-tx packet:
   8  bytes  total size            (existing framing convention)
   1  byte   flags                 (unused)
   1  byte   type  = #\P
  16  bytes  origin-id             [design §3 #2 author] minted at authoring, stable thru re-home
  16  bytes  op-id                 [design §3 #2] dedup key, stable thru re-home
   1  byte   op-class              0=authored 1=state-sync-create 2=purge   [design §4]
   8  bytes  lamport               [design §3 #4] reserved; hub authored ops carry hub stamp, else 0
   --- payload, by op-class ---
   authored | state-sync-create:   existing tx bytes (tx-header + tx-writes), verbatim
   purge:                          8-byte node-id count, then N*16-byte node UUIDs   [design §4,§7]

peer-ack packet:  size, flags, type=#\A, then a plist:
   (:pull-cursor <hub-seq> :push-ack <device-seq> :manifest-acked (<node-id> ...))
```

Handshake plist gains (over the master/slave handshake, design §8.1):
```
device → hub:  (:origin-id <uuid> :pull-cursor <n> :push-ack <n>
                :schema-version (<major> <minor>) :replication-key <k>)
hub → device:  (:peer-protocol-version 1 :name <g> :schema-version (<major> <minor>)
                :schema-digest <d>)        ; digest kept as within-major integrity check
```

Schema gate is **compatibility, not equality** `[PT-6]`: same `major` ⇒ proceed (additive /
value-domain drift handled degraded-safe, design §14); different `major` ⇒ reject pull, and (Branch
B) still accept push. `schema-version` derivation is partly app-owned — coordinate the major/minor
bump rules with the app side.

---

## 2. Work packages

### WP-1 — `peer-graph` class + lifecycle
`graph-class.lisp`: `defclass peer-graph (graph)` with slots from design §8 (`peer-role`,
`origin-id`, `peer-endpoint`, `device-registry`, `export-predicate`, `lamport-counter`,
`stop-replication-p`, `peer-thread`). `graph-p`/(new) `peer-graph-p` predicates.
`graph.lisp`: `make-peer-graph` / open-as-peer / close paths, reusing `init-replication-log` /
`close-replication-log` (`graph.lisp:146`,`:294`). New generics:
```
(start-peer (graph peer-graph) &key) → thread        ; mirrors start-replication; master/slave untouched
(stop-peer  (graph peer-graph))
```

### WP-2 — per-origin journaling (the one-line leverage point)
`transactions.lisp:1517`: replace `(when (master-graph-p (graph tm)) …)` with
`(when (journals-own-feed-p (graph tm)) …)`; add
```
(journals-own-feed-p (g graph)) → bool   ; t for master-graph; t for peer-graph in :device role
```
so master behavior is byte-identical and a device journals its own committed writes to its push
feed. Authoring on a device mints op-id + advances `lamport-counter` and stamps the journaled peer
envelope. Introduce a `peer-transaction` (or peer slots on the txn object) to carry
`origin-id`/`op-id`/`lamport`.

### WP-3 — applied-op-id index `[PT-3]`
Durable `op-id → lamport` map (reuse `linear-hash`). API:
```
(op-applied-p graph op-id) → bool
(record-applied-op graph op-id lamport)        ; MUST commit atomically with the apply
```
**Invariant `[PT-3]`:** `record-applied-op` is part of the same committed transaction as the apply,
so a crash between graph-commit and index-write cannot leave a gap (the surface-on-divergence
resolvers are not idempotent under re-apply). Implement by writing the index entry within
`apply-transaction`'s committed effects, not as a separate post-commit write. Pruning is deferred
(design §13).

### WP-4 — symmetric peer session (`peer-streaming.lisp`)
Hub: `peer-server-accept-loop` + `make-peer-session-handler` (model on `server-accept-loop` /
`make-slave-session-handler`, `transaction-streaming.lisp:295`,`:260`). Device: `peer-device-loop`
(model on `slave-loop`,`:413`). Session shape (design §8):
1. handshake (WP-6 compat check + device-registry auth);
2. **pull**: manifest reconcile (WP-5) then cursor-resumed in-scope authored ops;
3. **push**: receive device ops after `apply-cursor` → re-home apply (WP-7) — skeleton in Phase 2;
4. acks; cursors/manifest advance **post-commit / post-ack** `[PT-3,PT-4]`.

### WP-5 — authority-scoped export + manifest reconciliation
The `disclosable-p` seam (design §7, kept standalone for the app's CoT/web reuse, concern F):
```
(disclosable-p vertex graph device-scope) → bool      ; app-supplied; runs under export snapshot,
                                                       ; may do bounded graph reads
```
Closed-subgraph builder (design §6 closed rule, §7 invariant — predicate must be downward-closed):
```
(scope-node-set graph root(s) device-scope &key edge-types) → set<node-id>
   ; reachable from root bounded by edge-types, vertex kept iff disclosable-p,
   ; edge kept iff BOTH endpoints disclosable; runs under one with-read-snapshot
(reconcile-manifest graph device) → (values creates purges updates)
   ; creates = scope \ manifest ; purges = manifest \ scope ; updates = (∩) changed-since-cursor
(filtered-backup graph device stream)                  ; seed = reconcile from empty manifest
```
**Invariant `[PT-4]`:** the per-device manifest advances **only on device ack** of each
create/purge — never on send. Over-emitting a purge is harmless (idempotent state-sync); under-
emitting is a disclosure breach.

### WP-6 — schema-compat handshake `[PT-6]`
Compatibility check (same-major) replacing digest-equality; expose per-device
`last-pushed-schema-version` for the drain-and-update barrier signal (design §14). Digest retained
as a within-major integrity check.

### WP-7 — re-home apply (simple; merge is Branch B)
```
(apply-peer-op graph peer-op)         ; generic; Branch B specializes for divergence/merge
```
Hub path: wrap the op's writes in a fresh hub-origin transaction, **preserve op-id/origin/lamport**,
commit through the normal path (re-journals via WP-2, records op-id via WP-3), advance `apply-cursor`
post-commit. Branch A = last-applied-wins. `[PT-5 corollary]` the read-merge-write happens *inside*
the txn so a future Branch B merge survives OCC retry. Device never re-journals pulled ops (design §5).

### WP-8 — apply decoupled from the socket thread `[PT-5]`
The device receive thread **enqueues** received ops; a single writer drains + applies (the app's
single-writer funnel). Engine exposes `apply-peer-op` as a callable + an internal default writer for
standalone use, and lets the app supply its own writer. Required even in Phase 2 (read-only pull
still applies remote ops).

---

## 3. Milestones

- **M1 — foundation (no transport).** WP-0, WP-1, WP-2, WP-3. Unit-test in one image: author on a
  device-role peer-graph, confirm the push feed carries op-id + lamport; confirm `op-applied-p` /
  `record-applied-op` dedup and crash-atomicity.
- **M2 — pull MVP (= app Phase 2).** WP-4 (pull), WP-5, WP-6, WP-8. Two graph dirs in one image,
  peered over loopback: seed a closed authority-scoped subgraph onto the device, verify it contains
  exactly the disclosable closure; flip `disclosable-p`, reconnect, verify purge; bump schema minor,
  verify sync still proceeds. **Ships the read-only field app.**
- **M3 — push skeleton + re-home.** WP-4 (push), WP-7. Synthetic device writes pushed + re-homed on
  the hub, op-id round-trip dedup verified `[PT-1]`; device remains read-only pending Branch B.
  **Demonstrates the full Phase-1 sync contract.**

---

## 4. Tests

FiveAM `graph-db/peer-test` system under `tests/` (consistent with the existing suite). Core cases:
re-home op-id round-trip dedup `[PT-1]`; membership create/purge + re-entry `[PT-2]`; crash matrix
(torn cursor / index / manifest) `[PT-3,PT-4]`; single-writer funnel apply `[PT-5]`; schema-compat
sync `[PT-6]`. Run the matrix on SBCL + ECL (CCL on Linux only).

---

## 5. Carried open items (design §13) — resolve during build, don't block start

- Manifest reconciliation: every-connection full diff (v1) vs. incremental (optimization).
- Applied-op-id index pruning low-water policy.
- Push-ack: explicit `#\A` packet (planned above) vs. piggyback on next handshake.
- `schema-version` major/minor bump rules — **needs app coordination** (WP-6).

## 6. Needs from the app side (parallel; none block M1/M2)

- `disclosable-p` signature + the multi-tasking / retask fail-closed rule (design §13, PT-7.3).
- `find-of-type` modeling fork decision (lean: vertex reference slot) (PT-7.1).
- IMAS `work-stage` value set + danger rank (Branch B, but unblocks that S field).
- Device-registry schema + the `schema-version` scheme.
