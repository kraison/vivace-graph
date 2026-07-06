# General ordered index — design

**Status:** design agreed 2026-07-06 (Q&A with Kevin); building on branch `general-index`
off `experiment`. This is **piece (2)** of `docs/next-work-handoff.md` (after the B+ tree
work landed and 2.1.0 shipped).

## 1. What it is

A **non-enforcing ordered secondary index** on a node slot: a duplicate-free composite
`(slot-value . node-id)` map maintained on the commit apply path, supporting **equality
lookup** and **ascending range scans**. It is literally **`:unique` minus enforcement**
(`unique-constraint.lisp` is the template): same composite-key codec, same backends, same
persistence and peer-maintenance shape — drop the `validate` (pre-durability enforcement)
half and the `:origin`/scope partitioning, keep the `apply` (maintenance) half, and add a
range/iteration API. A non-unique slot just means many nodes share a `slot-value` prefix,
retrieved by range-scanning that prefix (exactly how the spatial index retrieves many ids
per cell).

It rides the **ordered-map seam**, so it is backend-agnostic: skip list or B+ tree per the
graph's `:index-backend`, for free.

## 2. Decisions (locked)

- **Scope — typed, `(named-class-subtree, slot)`.** An index is declared on a node type;
  it holds that class *and its subclasses* (a subtype IS-A). Rooted at the class you name
  (not the slot's introducing class, unlike `:unique` whose enforcement must cover the whole
  family) — so `(def-index user email)` indexes the `user` subtree even if `email` is
  declared on a parent `person`; say `(def-index person email)` for all persons.
- **Slot typing not required (type-as-hint).** Any slot is indexable. An optional
  type/comparator hint validates values at insert, advertises range support, and later lets
  us pick an order-preserving encoding. Absent it: equality-first; range via `less-than`;
  "meaningful only for homogeneous values" is the documented contract.
- **NIL / unbound values not indexed** (NULL-exempt, like `:unique`; unbound and explicit
  `nil` both = "no value"). Ordered/range APIs omit value-less nodes by construction; also
  guards against a giant `(nil . id)` clump.
- **Declaration — both surfaces.** `(slot :index t)` is the single-slot shortcut (inherits
  the slot's `:type` as the hint) that *desugars to* a `def-index`. `def-index` is the full,
  idempotent form (reuse the #49 two-phase registry) and the home for future composite/
  multi-slot keys and custom collation.
- **Collation.** Optional canonicalizer hint (reuse `:unique`'s `%resolve-*` mechanism —
  case-fold etc.; caveat: a canonicalized index orders by the canonical form, so you lose
  original-case ordering from that same index). **Descending is query-time**, not stored
  (reverse the scan), and in v1 is "range-scan then reverse the batch" because cursors are
  forward-only.
- **API.** `index-lookup` / `index-range` / `map-index`, each taking an explicit `graph`
  and resolving ids **in that graph** (bind `*graph*` / pass `:graph` — the wrong-graph
  discipline from the 2026-07 audit, baked in).
- **Prolog — deferred, seam identified.** Mirror `spatial-query.lisp` (which wires the
  spatial index into Prolog as index-backed predicates); the predicates are thin wrappers
  over the v1 API. Automatic index selection (compiler rewriting scan-and-filter → range
  scan) is a separate later project.

## 3. Data model

**Composite key** — identical to views/unique/spatial: on disk the ordered-map key is
`(canonical-value . node-id)` (`node-id` a 16-byte uuid), ordered by `reduce-comp-lessp`
(compare `(first key)` via `less-than`, tie-break the id via `key-vector<`); the skip-node
**value is unused (nil)** — read the id back from `(second (%sn-key node))`. Folding the id
into the key keeps the map duplicate-free, so a value shared by N nodes is N distinct keys
under one value-prefix, and remove is O(log n) and node-specific.

**Index struct** (`index.lisp`, mirroring `unique-index`):

```
(defstruct (slot-index (:constructor %make-slot-index))
  owner-name slot-name canonicalizer   ; identity + the type-as-hint canonicalizer
  table skip-list)                     ; exactly one set: memory hash-of-lists vs on-disk ordered map
```

Difference from `unique-index`: **not unique**, so the memory-backend `table` maps
`canonical-value -> (list-of ids)` (not a single id), and the on-disk `skip-list` holds the
composite `(value . id)` with **many ids per value-prefix**. No `spec`/`test`/`scope`/origin
fields.

**Registry** — a per-graph hash `secondary-indexes` (new `graph-class.lisp` slot), keyed by
`(owner-name . slot-name)`, exactly like `unique-indexes`.

## 4. Declaration

Two sources feed one maintenance path:

1. **`(slot :index t | <canonicalizer>)`** — the `:index` MOP slot option **already exists**
   (`node-class.lisp` `indexed`/`indexed-p`, propagated through
   `compute-effective-slot-definition` with a standing "FIXME: generate index" hook). Read
   via MOP at apply time, exactly like `class-unique-slots` reads `unique-spec`. Owner = the
   ancestor that declares the slot (shared index across subclasses). Extend `indexed` to
   optionally carry a canonicalizer (like `:unique`'s spec) instead of just `t`.
2. **`(def-index class slot &key canonicalize type)`** — a macro that registers an index
   *definition* `(owner-class slot canonicalizer)` in a schema-level, per-graph-name registry
   (like `def-view`/`def-vertex` metadata), **idempotent** across restarts via the #49
   two-phase pattern: a definition unchanged from last open is a no-op; a changed/new one is
   built and the removed ones dropped. Owner = the *named* class; applies to a node of class
   C when `C` is `subtypep` the owner and the slot is present.

`class-secondary-index-descriptors (class graph)` = the MOP `:index` slots of `class` ∪ the
`def-index` definitions applicable to `class`, returning `(owner-name slot-name canonicalizer)`
descriptors — the single input to maintenance (cache per class, like `class-unique-slots`).

## 5. Maintenance (apply, post-durability — no enforcement)

Copy `unique-constraint.lisp`'s apply half verbatim, dropping enforcement and origin:

- `ix-lookup (six value)` → **list of ids** for `value` (memory: the bucket list; on-disk:
  range-cursor over `[(value +null-key+) (value +max-key+)]`, collecting `(second (%sn-key n))`).
- `ix-range (six &key start end)` → ordered ids in `[start,end]` (a range cursor; open-ended
  when start/end omitted).
- `ix-put (six value id)` / `ix-remove (six value id)` — the composite `(value id)` add/remove
  (memory: push/delete in the value's bucket).
- `%ix-claim` / `%ix-release` (create / update-new / delete-old), and `apply-tx-write-to-
  secondary-indexes` methods on `tx-create` / `tx-update` / `tx-delete`, gathered by
  `apply-tx-writes-to-secondary-indexes`, called from `apply-transaction` alongside the
  view/spatial/unique passes (`transactions.lisp` ~987). **No `validate-*` — an ordered index
  never rejects.** `%index-key` = `(canonicalizer value)` (or `value`), NIL-exempt.

## 6. Query API (`interface.lisp` or `index.lisp`)

All bind `(*graph* graph)` so id resolution and any accessor reads hit the queried graph.

- `(index-lookup graph class slot value &key collect-p)` → the nodes of `class`(+subclasses)
  whose `slot` = `value`. Equality via `ix-lookup`, then `lookup-node` each id **in `graph`**.
- `(index-range graph class slot &key start end collect-p)` → nodes in `[start,end]`, ascending.
- `(map-index fn graph class slot &key start end)` → call `fn` on each node in order (the
  streaming form; `index-range` is `map-index` + collect, mirroring `map-vertices`).

Deleted nodes are filtered (`ix-remove` runs on delete, but guard reads anyway, like
`edge-exists-p`). Descending v1 = collect a range then `nreverse`.

## 7. Persistence & reopen

Mirror `:unique` exactly (it already solved both backends):

- **On-disk:** each `slot-index` is a persistent heap ordered map; save `(owner slot
  canonicalizer address backend-tag)` to a `secondary-indexes.dat` sidecar at `close-graph`;
  reopen via `open-heap-index` at `open-graph` (no node scan). Fall back to a node-scan
  rebuild if the sidecar is absent/stale (fresh graph or crash-before-save).
- **Memory:** ride the #50 checkpoint image — `%dump-secondary-indexes` /
  `%load-secondary-indexes`, bumping the image format version, so open skips the rebuild.
- **`regenerate-secondary-indexes (graph)`** — the in-place backend-migration entry (drop +
  rebuild on the current `:index-backend`), parallel to `regenerate-all-views` /
  `regenerate-unique-indexes` / `rebuild-spatial-index`; add it to the reindex recipe in the
  manual's Chapter 3.

Wire `restore`-or-`rebuild` into `open-graph` / `open-memory-graph` and `save` into
`close-graph`, next to the unique calls (`graph.lisp` ~325/375, and the memory-graph opener).

## 8. Peer replication

Maintain on the pull-apply paths too, exactly like unique but **simpler** — no enforcement,
no `:origin`/conflict machinery: add `apply-tx-writes-to-secondary-indexes` alongside the
unique maintenance in `apply-peer-create-writes` / `apply-peer-authored-op`
(`peer-streaming.lisp`), and release purged nodes' keys in `peer-purge-node`.

## 9. File-by-file build plan

| File | Change |
|---|---|
| `index.lisp` | **Rewrite** the dead stub into the engine: `slot-index` struct, registry get-or-create, `ix-*` ops (incl. range), `%ix-claim/release` + apply-tx methods, `class-secondary-index-descriptors`, `def-index` macro + idempotent registry, rebuild-on-open, persistence (disk sidecar + memory dump/load), `regenerate-secondary-indexes`, and the `index-lookup`/`index-range`/`map-index` API. |
| `graph-db.asd` | Add `index` after `unique-constraint` (same deps) + `index-tests`. |
| `node-class.lisp` | Let `:index` carry a canonicalizer (like `:unique`), and inherit it in `compute-effective-slot-definition` (the `indexed` branch, replacing the FIXME). |
| `graph-class.lisp` | Add the `secondary-indexes` graph slot (like `unique-indexes`). |
| `transactions.lisp` | Call `apply-tx-writes-to-secondary-indexes` in `apply-transaction`; export the new symbols in the local `eval-when`. |
| `graph.lisp` | `restore`-or-`rebuild` at open, `save` at close (next to unique). |
| `memory-graph.lisp` | Dump/load in the checkpoint image; rebuild-on-open guard. |
| `peer-streaming.lisp` | Maintain on pull-apply; release on purge. |
| `package.lisp` | Export `def-index`, `index-lookup`, `index-range`, `map-index`, `rebuild-secondary-indexes`, `regenerate-secondary-indexes`. |
| `tests/index-tests.lisp` | New FiveAM suite (below). |
| Manual Ch. 3/8 | Document `def-index` / `:index t` and add `regenerate-secondary-indexes` to the migration recipe. |

## 10. Build order (each step compiles + tests green before the next)

1. Engine for the **`:index t` MOP path** only (mirrors `:unique`: descriptors, registry,
   `ix-*`, apply hooks) + the API + rebuild-on-open. Testable immediately with a `:index t`
   slot — this is the fastest path to a green test.
2. **`def-index`** macro + idempotent registry + the descriptor union.
3. **On-disk persistence** (sidecar) + `regenerate-secondary-indexes`.
4. **Memory-backend** dump/load.
5. **Peer** pull-apply maintenance.
6. Docs.

## 11. Test plan (`tests/index-tests.lisp`, mirror `unique-constraint-tests.lisp`)

Equality lookup returns all sharing nodes; range scan returns the right ordered subset;
NIL/unbound excluded; update moves a node between values; delete removes it; subclasses are
included; a value shared by many nodes; reopen restores (on-disk sidecar + memory image);
dual-backend (skip list + B+ tree); the wrong-graph guard (query graph B while `*graph*`=A,
per the audit's `cross-graph-ops` test). SBCL + ECL.

## 11a. Known limitation: LAZY memory graphs

A **lazy** (fault-on-access) memory-graph does NOT rebuild its secondary indexes on
reopen: rebuilding scans every node, which would materialize the LZNODE blobs and
defeat the whole point of lazy open (a geometry `:index` slot alone trips the scan).
Non-lazy memory graphs and on-disk graphs are unaffected. A lazy graph still
maintains its indexes *in-session* via the apply path; the proper fix is to persist
the secondary indexes in the checkpoint image (mirroring the `:unique` v1.1 → v1.2
progression: rebuild-on-open first, then image persistence). Until then, `index-lookup`
on a reopened lazy graph returns an empty result — use a non-lazy memory graph if you
need general indexes to survive a lazy reopen.

## 12. Deferred (designed-for, not built)

Multi-slot/composite keys (codec already polymorphic — `less-than` orders lists); true
reverse-streaming cursors (`cursor-prev` / doubly-linked leaves); automatic index selection
in the Prolog compiler; `:type`-driven order-preserving on-disk key encoding.
