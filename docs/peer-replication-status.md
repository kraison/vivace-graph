# Peer Replication — Session Handoff / Status

**Purpose:** let a fresh Claude Code session resume the peer-replication work exactly where the
previous one stopped, and use the newly-attached **Lisp MCP** for verification.
**Branch:** `peer-replication` (off `experiment`), pushed to `origin`.
**Last updated:** 2026-06-30.

Read these first, in order: this file → `docs/peer-replication-design.md` (v2, the authoritative
design + §12 pressure-test invariants) → `docs/peer-replication-branch-a-plan.md` (WP breakdown,
peer-protocol-v1 wire layout, milestones). The auto-loaded memory `peer-replication.md` summarizes
the same state.

---

## 1. Where we are

**Milestone M1 (foundation) COMPLETE; M2 (pull MVP) COMPLETE + green on the SBCL-hub ↔ ECL-device
ship config.** Committed on `peer-replication` (M1 also pushed; the M2 + ECL-fix commits are local —
push is explicit-only):

| commit | what |
|---|---|
| `240c733` | docs: design v2 + approved proposal |
| `4c51351` | docs: Branch A implementation plan |
| `b0fb61f` | **WP-1** peer-graph class (sibling of master/slave) + make/open-graph plumbing |
| `60949e9` | **WP-2** device journaling (`journals-own-feed-p`) + **WP-3** applied-op-id index |
| `5bab412` | **WP-0** peer-meta envelope + op-id/lamport minting at authoring |
| `2f4112d` | **WP-5** authority-scoped closed-subgraph export (`replication.lisp`) |
| `82f5513` | **WP-4 + WP-6 + WP-8** hub/device pull transport (`peer-streaming.lisp`) + 2-process test |
| `17e89b9` | docs: status — M2 complete |
| `59f52b1` | **ECL fix**: SBCL hub ↔ ECL device green (3 cross-impl/ECL gaps + harness) |

Net: a `peer-graph` constructs/lifecycles; a device journals its own commits into a push feed with
a per-transaction peer-meta envelope (origin-id + op-id + lamport + op-class); a durable op-id index
dedups; `scope-node-set`/`reconcile-manifest`/`filtered-backup` compute the closed authority-scoped
subgraph a device should hold; and **`peer-streaming.lisp` ships it over the wire** — a hub serves
authority-scoped pulls and a device pulls its closed disclosable subgraph and applies it read-only,
through a single-writer funnel, behind a same-major schema-compat handshake. Master/slave/plain
graphs are untouched throughout. M2 is verified end-to-end by a two-process loopback test
(`tests/peer-replication/`, 12/12 green) on **all four impl combos** (SBCL↔SBCL, ECL↔ECL,
**SBCL hub ↔ ECL device** = the Android ship config, ECL↔SBCL); the existing master/slave test still
passes 16/16; and the full FiveAM suite is 0-fail on SBCL (1641) and ECL (1635).

**What `82f5513` added** (the network half — design §8/§9, plan WP-4/6/8):
- `peer-streaming.lisp`: peer-protocol v1 wire codec (`#\M` peer-meta for tx-bearing ops, `#\U`
  self-describing purge packet, `#\p` plist for handshake/control/ack; id lists ride the plist
  channel as concatenated hex); **WP-6** same-major schema-compat gate (not digest-equality, PT-6);
  authority-scoped pull (state-creates vertices-before-edges + purges, manifest advances **only on
  device ack**, PT-4); **WP-8** single-writer apply funnel (socket thread decodes+enqueues, one
  writer applies, PT-5); `start/stop-replication` specialized on `peer-graph` (hub accept loop /
  device writer — no `graph.lisp` lifecycle changes).
- `graph-class.lisp`: `peer-graph` gains `peer-schema-version` (app-settable, default `(1 0)`) +
  the peer-writer mailbox/thread slots.
- `tests/peer-replication/`: two-process harness (hub + device, like `tests/replication/`, since
  process-global `*graphs*`/schema/`*graph*` preclude an in-image two-graph test).

**What `59f52b1` fixed** (the SBCL-hub ↔ ECL-device split — the ship config — crashed; three real
ECL-path gaps, each masked by the next, **all latent for master/slave too** since no ECL replica was
ever exercised):
1. **Cross-impl plist strings** (the `(car "PEER-TEST-APP")` crash). The plist channel serializes
   under `*print-readably* t`; SBCL's `(symbol-name (graph-name g))` is a BASE-STRING that prints as
   a `#A((n) BASE-CHAR . "…")` array literal ECL's reader can't parse. `peer-streaming.lisp`:
   `peer-portable-string` coerces string values to a general `character` string; all peer plist
   writes go through `peer-write-plist`. The shared master/slave `serialize-packet-plist` is **not**
   touched.
2. **`mailbox.lisp` had no `#+ecl` branch** → `make-mailbox`/`send`/`receive` were NIL/no-ops on ECL,
   silently dropping every op in the WP-8 funnel. ECL now reuses CCL's lock+queue struct mailbox with
   a poll-based `receive-message` (`trivial-timeout` is CCL/LispWorks-only).
3. **`maybe-init-node-data` dereferenced a foreign `data-pointer`** (→ `deserialize #(0 0)` →
   `STORAGE-EXHAUSTED` cascade). A wire/disk node carries the author's `data-pointer` (a heap address
   meaningless locally); on ECL `change-class` (in `deserialize-vertex/edge-head`) reads a persistent
   slot during re-init → `node-slot-value` → `maybe-init-node-data` → read THIS heap at the hub's
   address. `primitive-node.lisp`: `maybe-init-node-data` no-ops while `*initializing-node*` is bound
   (defvar relocated above it); data is set explicitly afterward (wire path) or materialized lazily
   later (lhash path). **NB: this also means a real master→ECL-slave was broken the same way and is
   now fixed — worth a dedicated ECL-slave regression if that path is ever used.**

Harness (mine-action "item 2"): `hub.lisp`/`device.lisp` bind `*debugger-hook*` (+ ECL
`ext:*invoke-debugger-hook*`) early to print the condition + backtrace then quit, so an ECL error is
legible instead of a STORAGE-EXHAUSTED recursion cascade. `run-peer-test.sh` gained
`REPL_HUB_LISP_CMD` / `REPL_DEVICE_LISP_CMD` for the mixed split.

**Working tree:** the M2 work is committed. The **pre-existing unrelated working-tree edits**
(edge.lisp, index-list.lisp, rest.lisp, skip-list*.lisp, vertex.lisp, vev-index.lisp, views.lisp)
still predate this work and remain uncommitted — **do not stage or commit them**; they are not part
of peer-replication. Stage only the specific files you change.

---

## 2. How to verify

**End-to-end peer sync (the primary check):** the two-process loopback harness
```
tests/peer-replication/run-peer-test.sh                              # SBCL ↔ SBCL (default)
REPL_LISP_CMD="ecl --load" tests/peer-replication/run-peer-test.sh   # ECL ↔ ECL
# the Android ship config — SBCL hub, ECL device (the one that matters):
REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication/run-peer-test.sh
```
Prints each check and `RESULT: PASS`/`FAIL`. It runs the hub and device as **two OS processes**
(the only faithful setup — process-global `*graphs*`/schema/`*graph*` preclude an in-image
two-graph test, exactly as for `tests/replication/`). Covers seed = the closed disclosable closure
(fail-closed omission), scope-exit purge, and schema-compat (minor drift syncs, major bump
rejected). **Warm both fasl caches first** when running a mixed split (a cold simultaneous compile
makes the hub miss the device's 60s `wait-flag "ready"`): `sbcl --non-interactive --eval
'(ql:quickload :graph-db)'` and `ecl --eval '(require :asdf)' --eval '(ql:quickload :graph-db)'
--eval '(ext:quit)'`. Don't forget the master/slave regression:
`tests/replication/run-replication-test.sh`. The fix in `59f52b1` touches core
(`maybe-init-node-data`), so also run the full suite: `(graph-db/test::run-tests)` on SBCL and ECL.

**Multi-device (3-process: 1 hub + 2 devices) — `tests/peer-replication-multi/run-multi-peer-test.sh`.**
Same impl overrides as above (`REPL_HUB_LISP_CMD` / `REPL_DEVICE_LISP_CMD`, both devices share the
latter); it warms the fasl caches itself. Two devices with DISTINCT scopes ("alpha"/"bravo") rooted
at a shared site, over 4 phases: **scope isolation** (A holds data B doesn't, and vice versa),
**overlap** (a node both hold; a hub update fans out to both), **per-device re-task purge** (a node
leaves one device's scope → purged there, never leaked to the other through an undisclosed survey),
and **purge → re-entry** (PT-2). Green on SBCL×3, **SBCL hub + 2 ECL devices** (concurrent pulls),
and all-ECL. This is still Branch-A read-only (no two-writer conflicts — that's Branch B).

**Single-graph pieces (warm Lisp MCP image):** a Lisp MCP (`cl-mcp-server`) is attached. Discover it
with `ToolSearch` (keywords: `lisp`, `eval`, `sbcl`) → `mcp__lisp__evaluate-lisp` / `load-system` /
`load-file`. **NOTE:** pass the package via the tool's `package` arg (e.g. `"GRAPH-DB"`), not an
`(in-package …)` form — the reader reads all forms in one package. Use it to keep a **warm graph-db
image** for anything that needs only one graph — the wire codec round-trips, `scope-node-set`
closure/fail-closed, `apply-peer-purge` — but the actual hub↔device sync must go through the
two-process harness above (an in-image two-graph sync does NOT work). After editing a `*.lisp`,
`load-file` it (compile t) to recompile; if you reload `graph-class.lisp` it re-runs the
`start/stop-replication` `defgeneric`s and drops the `peer-graph` methods, so reload
`peer-streaming.lisp` after it.

Bring the system up in the image once:
```lisp
(ql:quickload :graph-db)   ; the repo is symlinked into ~/quicklisp/local-projects, so this
(in-package :graph-db)     ; picks up your edits; re-quickload after editing a file to recompile
```

**Fallback (no MCP / quick one-shot):** the previous session used `sbcl --non-interactive
--disable-debugger --load <script>` via the Bash tool. Pattern that works:
```
sbcl --non-interactive --disable-debugger --load /path/to/smoke.lisp 2>/dev/null | grep -E '<markers>'
```
with `(load "~/quicklisp/setup.lisp")` + `(ql:quickload :graph-db)` at the top. The smoke scripts
from last session lived in that session's scratchpad (now gone) — recreate them in YOUR scratchpad,
or better, start the FiveAM `graph-db/peer-test` system (see plan §4). Verified facts to rely on:
`uuid:uuid-to-byte-array (uuid:make-v4-uuid)` makes a 16-byte id; `def-vertex NAME (supers) (slots)
GRAPH-NAME` (keyword graph names) generates `make-NAME`; graphs need `*graph*` bound for
transactions; `close-graph g :snapshot-p nil` to skip the snapshot.

Quick live sanity check once the image is up:
```lisp
(peer-graph-p (make-instance 'peer-graph))            ; => T
(journals-own-feed-p (make-instance 'peer-graph))     ; => T   (master T, slave/plain NIL)
;; build a hub with an export-predicate, create the 2-hop chain, then
;; (scope-node-set hub (list site-id) "hma")          ; => closed disclosable subgraph
```

---

## 3. WP-4 (transport) — DONE (commit `82f5513`)

M2's network half shipped: a hub serves an authority-scoped closed subgraph to a device, which
applies it read-only through a single-writer funnel, behind a same-major schema-compat handshake.
This is the shippable read-only field app (app Phase 2). All in **`peer-streaming.lisp`** (full
`graph-db` system, after `transaction-streaming.lisp`), built distinct from but reusing the
master/slave packet primitives. Sub-step status:

1. **`start-replication`/`stop-replication` on `peer-graph`** — DONE. Hub role → `peer-server-
   accept-loop` on `replication-port`; device role → starts the single-writer thread; pulls are
   driven on demand by `peer-sync` (a Branch A device is read-only, so it pulls when it has
   connectivity rather than holding a live stream). Reuses the existing generics, so make/open/
   close-graph already call them — no `graph.lisp` changes.
2. **Handshake** — DONE. Hub announces (`peer-hub-handshake-plist`); device sends `origin-id` +
   cursors + `schema-major/minor` + key; hub authenticates against `device-registry`. **WP-6**:
   gate is same-major *compatibility*, not digest-equality (design §14 / PT-6); digest carried as a
   within-major integrity signal; `peer-schema-version` is an app-settable slot (default `(1 0)`).
3. **Pull phase** — DONE (membership path). `scope-node-set` (WP-5) → state-creates (vertices before
   edges, closed rule) + purges (manifest \ scope). This is the design-blessed every-connection
   full-diff v1 (§13): idempotent creates carry both new-and-updated nodes. Manifest advances **only
   on device ack** (PT-4). **DEFERRED:** the cursor-resumed in-scope *authored-op* stream (a
   volume/latency optimization) — see §3a; ongoing changes already converge via reconciliation.
4. **WP-8** — DONE. The receive side decodes + ENQUEUES (`peer-read-and-enqueue`); a single
   `peer-writer-loop` applies. No inline apply on the socket thread (PT-5).
5. **Push phase** — NOT YET (device is read-only in Phase 2); the hub-side re-home apply is
   **WP-7 / M3**. `apply-peer-authored-op` is present on the device for forward-compat but the hub
   does not emit authored ops yet.

### 3a. Immediate next task: the cursor-resumed authored-op stream (rest of WP-4.3)

The one deferred piece of WP-4. Implement the hub-side **in-scope authored-op stream** so live
edits to *existing* in-scope nodes flow without a full re-pull:
- A **peer-feed disk reader** over `replication-*.log` (entries are `[peer-meta][tx-header][writes]`
  for a peer-graph) that, given the device `pull-cursor`, forwards authored ops whose touched
  node-ids are in the device's scope id-set (computed during reconciliation).
- The device already has `apply-peer-authored-op` (op-id dedup PT-1, advances `pull-cursor`,
  `record-applied-op`) — **harden its PT-3 crash-atomicity** by persisting the authored op to a
  `.txn` first (mirror `stream-transaction-to-disk`) so the op-id-index update commits with the
  apply.
- Optional live tailing (broadcast like the slave mailbox) for same-connection streaming.

Reuse verbatim: `read-packet`/`write-packet`, the 8-byte framing, `deserialize-peer-meta`. Model the
log reader on `stream-replication-log`/`stream-all-packets` (`transaction-log-streaming.lisp`) but
read the peer-meta packet first.

### 3b. Then M3: push + re-home

WP-7 re-home apply (hub applies a device op as a fresh hub-origin transaction, preserving
op-id/origin/lamport, re-journaling; `apply-peer-op` generic, simple last-applied-wins — merge is
Branch B). Wire the device→hub push (device streams authored ops after `apply-cursor`).

---

## 4. Hard invariants (from §12 pressure-tests — do not regress)

- **PT-3:** the applied-op-id index update must be atomic with the apply (same committed txn).
- **PT-4:** the per-device manifest advances ONLY on device ack — a dropped purge must re-emit
  (fail-closed disclosure). Over-emitting a purge is safe (idempotent); under-emitting is a breach.
- **PT-5:** device apply must run on the single writer, not the socket thread.
- **PT-6:** schema gate is compatibility (same-major), not equality; push must accept an
  older-but-compatible device so field safety data is never stranded.
- **§3 four identities** kept distinct: node-UUID / op-id (dedup) / per-origin feed-seq (cursor) /
  lamport (order).
- **§4 two op classes:** authored (op-id-deduped, bidirectional) vs state-sync (node-UUID-keyed,
  hub→device only, never deduped/merged/propagated — purges + membership-creates).
- **§7 closed-subgraph, fail-closed:** vertex iff disclosable, edge iff BOTH endpoints disclosable;
  `disclosable-p` must be downward-closed along disclosure-determining edges.
- Re-homing: only the hub re-journals; a device never re-journals ops it pulled (prevents loops).

---

## 5. Open items / app-side asks (do NOT block the next task)

Engine TODO (deferred): the cursor-resumed authored-op stream + its PT-3 crash-atomicity (§3a, the
immediate next task); **manifest/cursors are in-memory** in the `peer-device` struct — durable
persistence across hub restart is still a §13 open item; Lamport counter persistence (PT-8,
Branch B); applied-op-id index pruning; manifest reconciliation cost model; origin-id persistence
across reopen (currently passed in). App-side (parallel): `disclosable-p` signature + multi-tasking/
retask fail-closed rule (PT-7.3); `find-of-type` modeling fork (lean: vertex reference slot,
PT-7.1); edge-removal bucket; IMAS `work-stage` value set + danger rank; device-registry schema;
`schema-version` major/minor scheme.

---

## 6. Conventions (carry these)

- Engine work on branch `peer-replication`. **Push is explicit-only.**
- **Before every `git commit`**: show the COMPLETE staged diff in the assistant message in a fenced
  ```diff block under a `## 📋 DIFF FOR REVIEW` header (Kevin's standing rule). Stage only files you
  changed; never the pre-existing unrelated working-tree edits (§1).
- **Lisp = spaces only, never tabs** (tab-width 8 if converting). Match surrounding style.
- Commit messages end with `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Use your own session scratchpad for temp/test files, not the old session's paths.

---

## 7. Ready-to-paste continuation prompt for the new session

> Resume the VivaceGraph peer-replication work on branch `peer-replication`. Read
> `docs/peer-replication-status.md`, then `docs/peer-replication-design.md` (v2) and
> `docs/peer-replication-branch-a-plan.md`. M1 (WP-0/1/2/3) + WP-5 are done and pushed; **M2 (pull
> MVP: WP-4 transport + WP-6 schema-compat + WP-8 single-writer apply) is done in `peer-streaming.lisp`
> (`82f5513`), and the SBCL-hub ↔ ECL-device ship config is green after the ECL/cross-impl fixes
> (`59f52b1`) — both commits LOCAL, not pushed.** Verified by `tests/peer-replication/run-peer-test.sh`
> 12/12 on all four impl combos, master/slave 16/16, full suite 0-fail SBCL+ECL. Next is the
> **cursor-resumed authored-op stream** (§3a — the
> one deferred piece of WP-4: a peer-feed disk reader over `replication-*.log` forwarding in-scope
> authored ops after `pull-cursor`, plus PT-3 crash-atomicity for `apply-peer-authored-op`), then
> **M3** (WP-7 re-home apply + device→hub push). Use the attached Lisp MCP (`cl-mcp-server`,
> `mcp__lisp__*` — pass package via the `package` arg, not `in-package`) for single-graph checks;
> the hub↔device sync goes through the two-process harness (`tests/peer-replication/`), NOT an
> in-image two-graph test. Honor the diff-before-commit rule and don't touch the pre-existing
> unrelated working-tree edits.
