# memory-graph — handoff for the ECL mobile app side

**Branch:** `memory-graph` (off `experiment`). **Status:** the in-memory storage
backend is complete through the peer device path and validated on SBCL **and**
ECL. Ready to try on the device.

## What it is

A drop-in, in-memory-only storage backend for VivaceGraph. Nodes are live Lisp
objects in a hash table (no mmap heap, **no per-read deserialization**), adjacency
is in-RAM sets (no pcons chain-walking), and durability is the journal + a
cl-store image checkpoint. Same public API as an on-disk graph — schema,
transactions, queries, views, spatial, and peer replication all work unchanged.

It is a separate graph *class*, chosen explicitly at construction; the on-disk
engine is untouched.

## API — the only change the app makes

Swap `make-graph` / `open-graph` for `make-memory-graph` / `open-memory-graph`.
For the device, that's the one call in `ma-device.lisp`:

```lisp
;; on-disk device (today):
(make-graph :peer-test-app dir :peer-role :device :origin-id oid
            :peer-host host :replication-port port :replication-key key
            :buffer-pool-size 256)

;; in-memory device (new) -- same args, minus :buffer-pool-size (no heap/pool):
(make-memory-graph :peer-test-app dir :peer-role :device :origin-id oid
                   :peer-host host :replication-port port :replication-key key)
```

`open-memory-graph` mirrors it (same peer keys). Everything downstream —
`peer-sync`, `record-find` / authored `with-transaction` writes, `map-vertices`,
views, `spatial-index-query-bbox` — is identical. `close-graph` writes the image
checkpoint; open restores it (or replays the journal after a crash).

`location` (dir) is still required: it holds the durable journal, the cl-store
image, the schema, and the peer state (lamport clock, field-stamps, applied-op-id
index).

## Device durability — **call `checkpoint-memory-graph` after `peer-sync`**

This is the fix for the reopen-restore issue in `memory-backend-perf.md`.

**Pulled state is applied directly and is NOT journaled** — between opens it is
durable *only* through the cl-store image, which is written at clean close. If the
app's close runs on a nearly-empty instance (the 5 KB image you saw), the next open
restores empty and re-cold-syncs. The image round-trip itself is fine (verified:
800 nodes → 188 KB → 800 restored); the gap is purely *when* the image gets written.

So on the device, after a sync:

```lisp
(peer-sync g)
(graph-db::checkpoint-memory-graph g)   ; write image + clear journal, now
```

`checkpoint-memory-graph` = the same checkpoint `close-graph` does, callable any
time (~0.06 s / 0.2 MB for ~800 nodes). Call it after every sync (and it's cheap
enough to call after a batch of `record-find`s too). Then the next
`open-memory-graph` restores the full subgraph and the app should **not** re-sync.

**Restore-vs-resync decision:** don't use a fresh-detection heuristic — check the
restored node count directly. Right after `open-memory-graph`, if
`(graph-db::mem-table-count (graph-db::vertex-table g))` is what you expect, restore
worked; only re-sync if it's ~0. (An app-side `nativeQuery("open-bench")` that opens
and returns that count would settle the on-device measurement cleanly.)

## What's validated

- **FiveAM regression suite** (`tests/memory-graph-tests.lisp`): 27/27 on **SBCL
  and ECL** — CRUD, indexes/adjacency, durability (clean + crash), spatial, views.
- **Ship config (SBCL hub ↔ ECL in-memory device)**, two-process harness:
  - **Pull** (`tests/peer-replication/`): authority-scoped subgraph, fail-closed
    withholding, scope-exit purge, schema-compat rejection — PASS.
  - **Push** (`tests/peer-replication-push/`): authored writes apply + push,
    per-field LWW, conflict surfacing — PASS.

Run them yourself:
```bash
# pull:
REPL_DEVICE_MEMORY=1 REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
  REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication/run-peer-test.sh
# push:
REPL_DEVICE_MEMORY=1 REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
  REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication-push/run-push-test.sh
```
(`REPL_DEVICE_MEMORY=1` is the only new knob; it makes the harness open a
`memory-peer-graph` device.)

## ECL note (important)

ECL 26.5.5 cannot compile its own generated C under Apple clang 21 (the current
Xcode). On Kevin's Mac this is worked around in `~/.eclrc` (points ECL at
Homebrew `llvm@18`). The **Android NDK build uses NDK clang (~18)**, a different
toolchain, so it should be unaffected — but confirm on the first device build.

## Fast open — derived structures are persisted (no rebuild-on-open)

**This is the boot-latency fix** for the ~36 s open in `memory-backend-perf.md`.
That open was dominated by rebuilding the aggregate (`eo-find`) views from the nodes
(~23 s) on *every* open. The image is now **v2**: it pickles the ve/vev/type indexes,
the spatial grid, and every view's ordered-map (as flat dumps), and open restores
them **structurally** — direct inserts, **no map / reduce / geohash recompute**.
`open-memory-graph` no longer calls `regenerate-all-views` for a clean/checkpointed
image (verified: 0 calls, view + spatial still correct). The journal tail committed
after the checkpoint is still replayed on top, so authored writes since the last
checkpoint update the restored views/indexes incrementally.

Nothing changes in the app API — just re-run a checkpoint (or clean close) so the
device writes a v2 image, and the next open skips the rebuild. A v1 image still opens
(it falls back to rebuild-on-open), so old images aren't a hard break.

## Fault-on-access (`:lazy t`) — near-instant open

The remaining open cost after the fix above is **`make-instance`** — building the live
CLOS node objects. On ECL that's ~85% of the restore time (measured), and it's
unavoidable *per node you actually touch*. The `:lazy` flag stops paying it for nodes
you **don't** touch:

```lisp
(make-memory-graph :peer-test-app dir :peer-role :device :origin-id oid
                   :peer-host host :replication-port port :replication-key key
                   :lazy t)                 ; <-- opt in
(open-memory-graph  :peer-test-app dir :peer-role :device ... :lazy t)
```

A lazy graph writes a **VG-native image** (per-node blobs) and, on open, loads each
node as a lightweight blob (`lznode`) with **no `make-instance`**. The live object is
built on first lookup and cached; later reads are full-speed. Measured on ECL:

| | eager open | **lazy open** |
|---|---|---|
| 800 nodes | 502 ms | **7 ms** (~71×) |
| 2000 nodes | 1269 ms | **18 ms** (~70×) |

What this means for the app's boot:

- **Open is ~instant** regardless of scope size.
- **Aggregate/reduce views (`summary`) touch zero nodes** — they read the persisted
  aggregates, so they're instant with no materialization at all.
- **Rendering N finds** materializes exactly those N (deferred off the open path); the
  total node-build cost is conserved but spread to what's on screen, and never paid for
  nodes the user doesn't view.
- **Lower RAM** — untouched nodes stay compact blobs instead of live CLOS instances.

Caveats: `:lazy t` requires the VG-native image (a lazy graph writes it automatically;
just checkpoint/close once on this build). A full scan (`map-vertices` over everything)
still materializes everything — lazy wins for partial working sets, which is the field
case. `:lazy` composes with the peer device (it's just a storage-mode flag).

**Proven end-to-end** on the ship config (SBCL hub ↔ ECL in-memory device): both
two-process harnesses pass with a lazy device, including a fault-on-access reopen that
confirms open builds no live node and the synced/authored data materializes on access.
Reproduce with the extra `REPL_DEVICE_LAZY=1` knob:

```bash
# pull:
REPL_DEVICE_MEMORY=1 REPL_DEVICE_LAZY=1 REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
  REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication/run-peer-test.sh
# push:
REPL_DEVICE_MEMORY=1 REPL_DEVICE_LAZY=1 REPL_HUB_LISP_CMD="sbcl --non-interactive --load" \
  REPL_DEVICE_LISP_CMD="ecl --load" tests/peer-replication-push/run-push-test.sh
```

## Known gaps / caveats (v1)
- **cl-store image is local-only** (not portable across Lisp impls) — it's the
  fast clean-open path; the journal is the crash-safety net; the portable s-expr
  snapshot is a deferred compaction path.
- **MVCC is dropped** on a memory-graph (single-writer + atomic node-swap,
  lock-free reads); the `with-read-snapshot` API is kept. The app does only naked
  reads, so this is invisible to it.
- Not yet run: the 3-process multi-device pull harness with a memory device (only
  1-device pull + push were run). The device.lisp change to add `REPL_DEVICE_MEMORY`
  is mechanical if you want it.

## Where to look

- `memory-graph.lisp` — the backend (classes, constructors, node-ops, durability,
  peer wiring). `mem-skip-list.lisp` — the in-RAM ordered map (views + spatial).
- `docs/memory-graph-design.md` — full design + rationale. GH issue #50.
