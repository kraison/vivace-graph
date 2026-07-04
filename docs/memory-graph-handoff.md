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

## Known gaps / caveats (v1)

- **Rebuild-on-open**: indexes, views and the spatial grid are rebuilt from the
  restored nodes on open (not pickled). Fine at the app's scale (~800 nodes,
  ~40 ms); a persistent representation is a documented follow-up if open latency
  ever matters.
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
