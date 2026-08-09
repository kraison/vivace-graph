# Vector segment transaction integration (Phase 2, Step 3) — design

Date: 2026-07-21
Status: approved design, not yet planned
Repo: `vivace-graph-v3` (engine, branch `experiment`)
Follows: `2026-07-20-vector-segments-design.md` (§4–§8); Step 2 built the standalone
segment file format.

## 1. Summary

Wire the standalone vector segment (Step 2) onto the transaction apply path so a
segment stays in sync with node creates, updates, and deletes — mirroring how the
spatial index is maintained. Add the `:vector-index` slot declaration, per-`(class,
slot)` segment ownership on the graph, lazy segment creation, and crash recovery by
rebuild-from-nodes.

This step does **not** add query (`segment-scan` / `segment-score-subset`) — that is
Step 4.

## 2. What Step 2 delivered, and what this rests on

The segment is a standalone mmap-backed file: header + id array (with a threaded free
list) + contiguous single-float vector block; `create-vector-segment`,
`open-vector-segment`, `close-vector-segment`, `segment-put`, `segment-get`,
`segment-remove`, and in-place growth across the mmap boundary. `segment-suite`
115/115; full `graph-db-suite` 2231/2231.

Two Step-2 properties this step depends on:

- **Growth keeps the base pointer stable** (`extend-mapped-file` / MAP_FIXED reserve),
  so a segment can grow without invalidating a held pointer.
- **`live-count` is occupancy, not an iterator.** Occupied slots are dense
  `[0, live-count)` *only* when the free list is empty. Any sweep — including
  rebuild-from-nodes here — must walk `[0, capacity)` and skip free cells.

## 3. The recovery contract (settled by investigation, not assumption)

An investigation of the crash-recovery path (`.superpowers/sdd/recovery-investigation.md`)
established, with file:line evidence:

- `open-graph` **hard-errors** on the `.dirty` marker (`graph.lisp:248-249`) and stops.
  There is no automatic txn-log replay in the crash case.
- `recover-transactions` *does* replay through `apply-transaction`
  (`transactions.lisp:2270-2280` → `964-993`), so a hook in that path rides along — but
  for on-disk graphs the `.txn` files are deleted right after each apply
  (`retain-committed-transaction-p` is nil), so the replay set is only the last in-flight
  transaction, **not a retained journal**.
- Every existing derived index (spatial, secondary, unique) recovers by **explicit
  rebuild-from-nodes**, triggered by a missing file or a stale format version — never by
  the `.dirty` marker or by replay.

Therefore the segment's contract is:

- **Maintenance** rides the apply-path hook (§6): correct for live commits and for the
  narrow in-flight replay.
- **No per-commit msync.** A crashed segment cannot be trusted regardless — growth flips
  `capacity` last, so a crash mid-grow leaves a torn file that no flush would make
  consistent. Flushing buys nothing on recovery.
- **Recovery is rebuild-from-nodes**, mirroring `rebuild-spatial-index`.
- **Trigger:** a per-segment **clean-shutdown flag** in the header (§7). The segment is
  more exposed to torn writes than the spatial index (a separate file with non-atomic
  growth, versus the spatial index living inside the transactional `indexes.dat` heap),
  so it self-certifies rather than trusting the file unconditionally on open.

## 4. Segment ownership on the graph

Segments are per-`(owner-class, slot)` — a graph can have several, unlike the single
spatial index. The graph gains a `vector-segments` slot on `graph-class` (accessor
`vector-segments`, initform an `equal` hash table), keyed by `(owner-class-name .
slot-name)`, value a `vector-segment`.

**One segment per *declaring* class, spanning subclasses (the engine convention).** A
`:vector-index` slot is declared on some class; the segment is owned by that declaring
class and holds every instance of it **and its subclasses**. This mirrors `:unique` and
`:index` exactly — a `:unique` slot on a parent enforces across subclasses, keyed by
`(owner . slot)` (`unique-constraint.lisp:61` `%unique-slot-owner-name`); the general
ordered index likewise keys on the declaring ancestor. The `:vector-index` inheritance
rule in `node-class.lisp` already states this ("indexed across its subclasses, like
`:index` / `:unique`").

The **owner** of a `(class, slot)` is the highest ancestor in the class-precedence-list
whose *direct* slots declare that slot with `:vector-index` — resolved by
`%vector-index-slot-owner-name`, a mirror of `%unique-slot-owner-name`. So a subclass
instance's vector lands in the *ancestor's* segment, not a per-subclass one. Every
maintenance path (create/update/delete/validate) and rebuild keys through a single
`%segment-key (node slot)` → `(owner-name . slot)` helper, so the model is applied in
exactly one place.

Consequences of one-segment-per-owner: all instances of the hierarchy share one
**dimension** (one logical index, one embedding model). A subclass instance whose
embedding has a different dimension hits the dimension-mismatch rollback (§6) — which is
correct: you cannot mix dimensions in one kNN index.

The keying is by class *name* and slot *name* (symbols), not the class object, so it
survives schema reload and matches how the rest of the schema layer keys per-graph
metadata.

**Not offered: per-subclass (exact-class) segments.** An alternative model — one segment
per concrete class, so a subclass indexes separately from its parent — was considered and
rejected. It fragments one logical index across N segments, forces a hierarchy query to
fan out and merge, and contradicts the engine's own convention for `:unique`/`:index`. No
use case wanted it. `:vector-index` is a plain boolean; there is no scope option.

## 5. Declaration: the `:vector-index` slot option

A new slot option, plumbed exactly as `:unique` was (the template is in `node-class.lisp`):

- Add a `vector-index` slot to `node-slot-definition` (`:initarg :vector-index`,
  `:initform nil`), with a reader `vector-index-p` and a default method returning nil.
- Thread it through effective-slot inheritance in `compute-effective-slot-definition`,
  alongside the existing `indexed` / `unique` propagation.
- `node-vector-index-slots (class)` returns the names of `:vector-index` slots, cached
  per class, mirroring `node-geometry-index-slots` (`transactions.lisp:813`).

Declaration example:

```lisp
(def-vertex rag-chunk ()
  ((text :type string)
   (embedding :vector-index t))
  :my-graph)
```

**Value-type gating.** Like the spatial index, maintenance does not trust the declared
type. A declared slot feeds a segment only when its runtime value is a conforming
`(simple-array single-float (*))`. A declared slot whose value is nil or non-conforming
is simply not indexed (and removes any existing entry — §6).

**Dimension is discovered, not declared.** The segment is created lazily (§6) from the
first conforming vector, taking that vector's length as its fixed dimension. An empty
declared slot has no segment until something is written — correct, because there is
nothing to index yet.

**Not in this step:** a `def-vector-index` macro for declaring on a class you do not own
(spec §8 mentioned it). YAGNI — the slot option covers the case at hand; the macro can
be added later without changing anything here.

## 6. Maintenance hooks

`apply-tx-write-to-vector-segments`, a generic dispatched on `tx-create` / `tx-update` /
`tx-delete`, mirroring `apply-tx-write-to-spatial-index` (`transactions.lisp:854-873`),
plus the plural `apply-tx-writes-to-vector-segments`. The plural is added to the
`apply-transaction` body (`transactions.lisp:983-991`) immediately after
`apply-tx-writes-to-spatial-index`.

For a node, `node-vector-index-slots (class-of node)` gives the candidate slots; for each,
read the runtime value and act on whether it is a conforming vector:

| tx type | old value | new value | action |
|---|---|---|---|
| create | — | conforming | `%ensure-segment` then `segment-put` |
| create | — | absent/non-conforming | nothing |
| update | any | conforming | `segment-put` (overwrites; creates if first) |
| update | was indexed | absent/non-conforming | `segment-remove` |
| delete | was indexed | — | `segment-remove` |

Reading the slot value follows the spatial index's rule (`transactions.lisp:842-848`):
read through `slot-value` directly, do **not** gate on `slot-boundp` (persistent slots
read as unbound on the backing CLOS slot).

**Lazy creation.** `%ensure-segment (graph class-name slot-name dimension)` returns the
existing segment or creates one (file named under the graph directory, e.g.
`vseg-<class>-<slot>.dat`) with the given dimension, registers it in `vector-segments`,
and returns it. Called on the first conforming insert.

**Dimension mismatch signals.** `segment-put` already signals when a vector's length is
not the segment's dimension (Step 2). Because maintenance runs inside the transaction
write lock, that signal rolls the whole transaction back — node and segment cannot
diverge. A caller inserting a wrong-dimension embedding into an established slot is a
caller error, surfaced loudly.

**Locking: nothing new.** `apply-transaction` runs under the transaction manager's write
lock (`with-transaction-lock`, `transactions.lisp:966` → a write lock on the single
`transaction-lock`), so all applies are serialized. A `segment-put` that escalates into
growth is exclusive against other writers for free — the review's "hold an exclusive
lock across the whole put" is already satisfied. The torn-read window during growth is a
**reader-writer** hazard, and there are no segment readers in this step (query is Step 4;
rebuild runs quiescent). Reader-writer safety is an explicit **Step 4 prerequisite**, not
this step's concern.

## 7. Open, recovery, and the clean-shutdown flag

**Header flag.** Store the `clean-shutdown` flag in the header's reserved uint64 at
**offset 56**, which Step 2 writes as a literal `0` (`segment.lisp:29`) and never reads —
so this needs **no format-version bump**; existing v1 segment files remain readable, and
an old file simply reads offset-56 as `0` (dirty), which correctly forces one rebuild on
first open under the new code.

**Semantics:** flag `1` = "cleanly closed, trust the file"; flag `0` = "in use or
crashed, rebuild on next open." Step 2's `create-vector-segment` already writes offset 56
as `0`, which is correct — a freshly created segment *is* in use until a clean close flips
it to `1`. The flag is consulted only **at open**, never at create.
`close-vector-segment` sets it clean (1) just before unmapping; a crash leaves it `0`.

**Ordering matters: read the persisted flag before anything overwrites it.** The
open-versus-rebuild decision reads the flag value on disk from the previous session; only
*after* deciding to keep a segment does the in-use marking flip it to `0`. So
`open-vector-segment` reads the full header (including offset 56) and exposes that
pre-open value, the graph-open logic decides on it, and a kept segment is then re-marked
dirty for the new session.

**At graph open**, for each `(class, slot)` the schema declares `:vector-index`:

1. File absent → nothing yet (a fresh graph, or a slot never written). The segment is
   created lazily on first write. No rebuild.
2. File present, and (format stale **or** the persisted flag was dirty) →
   **rebuild-from-nodes** (discard the on-disk file, build fresh).
3. File present, format current, persisted flag clean → keep it open, then re-mark it
   dirty for this session.

In every kept-or-created case the segment is dirty (`0`) for the session and becomes clean
(`1`) only at `close-vector-segment`.

**`rebuild-vector-segment (graph class-name slot-name)`** mirrors `rebuild-spatial-index`
(`spatial-query.lisp:174`): create a fresh segment file, `map-vertices` over the class,
and for each node with a conforming value `segment-put` it. Runs **quiescent** — at open,
before the graph accepts writes (spec §6; a rebuild that raced a commit could bake in a
skipped mid-commit node). Register the rebuilt segment in `vector-segments`.

**`close-graph`** closes every registered segment (setting each clean), alongside its
existing index teardown.

## 8. File-by-file plan (informative — the implementation plan will detail tasks)

| file | change |
|---|---|
| `node-class.lisp` | add the `vector-index` slot option + `vector-index-p` + inheritance |
| `transactions.lisp` | `node-vector-index-slots`; `apply-tx-write-to-vector-segments` (3 methods) + plural; call it in `apply-transaction` |
| `graph-class.lisp` | `vector-segments` slot + accessor on the graph |
| `graph.lisp` | open path: per-declared-slot open-or-rebuild; `close-graph` closes segments |
| `segment.lisp` | clean-shutdown flag in the header; set/clear on open/close; `%ensure-segment`; `rebuild-vector-segment` |
| `tests/…` | the §9 tests |

## 9. Testing

- **The invariant.** After an arbitrary sequence of creates, updates, and deletes through
  real transactions, the segment's contents equal what a fresh rebuild-from-nodes
  produces. This is *the* consistency property and deserves a randomised sequence, not
  three hand-picked cases.
- **Clear-value-removes-entry.** An update that sets a `:vector-index` slot to nil (or a
  non-conforming value) removes the segment entry; a subsequent `segment-get` for that id
  returns nil.
- **Dimension-mismatch rollback.** Inserting a wrong-dimension vector into an established
  segment signals and rolls the transaction back — the node write does not land either.
- **Lazy creation.** A declared slot has no segment file until the first conforming write;
  the file appears after it.
- **Rebuild reproduces.** Delete the segment file, reopen, confirm the rebuilt segment
  matches what was there.
- **Dirty-flag triggers rebuild.** Simulate an unclean close (leave the flag dirty),
  reopen, confirm the segment was rebuilt rather than trusted.
- **Rebuild is quiescent** — a rebuild path exists that does not run concurrently with
  writes; assert the open sequence rebuilds before accepting the first transaction.

**Standing discipline (from Phases 1–2): every consistency assertion must be able to
fail.** Guard NIL/empty before any `every`/`some`/`loop` that could pass vacuously (the
`(every #'= expected nil) => T` trap has recurred seven times). For the load-bearing
gates — the invariant test and the rebuild test — include a **sabotage proof**:
deliberately break the maintenance (e.g. skip the `segment-remove` on delete) and confirm
the test fails, then restore.

## 10. Scope fence

- **In:** declaration, ownership, apply-path maintenance, lazy creation, recovery
  (rebuild + dirty flag), close.
- **Out:** `segment-scan` / `segment-score-subset` and any query surface (Step 4); the
  cl-llm `:segment` store strategy and `vertex->chunk` retirement (Step 5); the
  `def-vector-index` macro; int8 quantisation; reader-writer locking for concurrent scan
  (a Step 4 prerequisite, flagged here so it is not forgotten).
- **SBCL only. ECL out of scope.**

## 11. Risks

- **Segment/node drift is the failure mode that matters** — silent wrong or missing
  results with the node correct. Mitigations: maintenance is inside the transaction write
  lock; the invariant is directly asserted (§9) with a sabotage proof; every corruption
  state is recoverable by rebuild.
- **A rebuild that races a commit** could bake in a skipped mid-commit node
  (`vertex.lisp:207-216` — a typed scan skips ids whose lookup returns NIL mid-commit).
  Mitigation: rebuild runs quiescent, at open, before writes (§7).
- **The clean-shutdown flag must actually be set/cleared on every path** — an open that
  forgets to set dirty, or a close that forgets to set clean, defeats the trigger.
  Mitigation: a test that simulates unclean close and asserts rebuild.
- **Reader-writer during growth is deferred, not solved.** If any Step-4 scan reads a
  segment concurrently with a growing commit, the torn-read window is real. Flagged as a
  Step 4 prerequisite (§6, §10).
