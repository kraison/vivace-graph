# Read-your-own-writes inside a transaction — design

Issue: kraison/vivace-graph#408. Status: draft for Kevin's approval,
2026-10-29. Scope decided with him the same day: strategy A below, views
(#7) stay out, the overlay's interface is shaped so strategy D can replace
its internals later.

## 1. Intent

Inside `with-transaction`, a read today returns the state committed before
the transaction began. None sees the transaction's own creates, updates or
deletes (the ten-row table in #408). "Replace the edge", written as
drop-then-make, is therefore right once per transaction and wrong twice,
and every new writer in a consumer app meets the gap again because
read-after-write is what code assumes.

After this work, a read inside a transaction answers as the transaction
will commit: its own writes on top of the snapshot it already reads, with a
delete masking the committed node. This is the same "store ∪ writes, writes
win, deleted is absent" that the commit view (`make-commit-view`,
`value-constraint.lisp`) already gives validation, cardinality, rules and
spacetime. The engine's read paths are the last users of committed state
that should have been using it.

### Why it was deferred, and why that no longer holds

The only recorded reason (comment on #7, 2026-06-05) bundled this with the
MVCC rework because an overlay "interacts with the OCC validation and the
per-view-group locking". MVCC P1–P5 shipped; Phase C (versioned index
membership, `docs/mvcc-phase-c-plan.md`) is a draft. Phase C never subsumed
this work: an uncommitted write can never sit in the shared on-disk
indexes, since other transactions would see it and rollback would need
undo, so read-your-own-writes needs a private in-memory overlay regardless.
Meanwhile #324 and #358 built that overlay by hand inside spacetime, and
#401 wrote the gap into the documented contract. The OCC interaction turns
out to be trivial (§4.5); the view-group locking is #7's problem and is out
of scope.

## 2. Goals, non-goals, anti-goals

**Goals**

- Every row of the #408 table reads correctly inside the transaction that
  wrote it: `lookup-vertex` / `lookup-edge` and the generated
  `lookup-<type>`; `map-vertices` and `map-edges` in their typed and
  adjacency forms (so `outgoing-edges`, `incoming-edges`, `traverse` and
  the Prolog functors that call them); `edge-exists-p`; `index-lookup`,
  `map-index` and `index-range` on `:index` and `def-index` slots.
- A delete in the transaction masks the committed node in all of those.
- An update that moves a secondary-index key is found under the new key
  and not under the old one.
- Nothing changes for a read outside a transaction, under a read snapshot
  with no transaction, or under `:as-of`.
- OCC validation, the write-set, the `.txn` log, replication and recovery
  are byte-for-byte unchanged. The commit path's validators keep reading
  what they read today (§4.6). The code changes are the read paths, the
  three write entry points that feed the overlay, and one binding in each
  place that already composes its own overlay.

**Non-goals (documented as such, each with its issue)**

- Views (#7): a view overlay means re-running the map over the write set
  and merging, and reduce makes that a different design.
- The fully untyped `map-vertices` / `map-edges` lhash scans: already
  documented as admin paths that bypass MVCC; they stay committed-only.
- Entry-level index reads that resolve no node: `index-count`,
  `map-index-prefixes`, `index-count`'s unique and count-index cousins.
  #361 already does a write-set delta for spacetime's counts; a general
  one is a follow-up issue filed with this work. (Lifted for
  `index-count` and `map-index-prefixes` by #409; the count-index
  cousins go with #411.)
- Spatial and vector-segment queries, unique-index membership: their
  resolution goes through `lookup-*` and so gains value visibility for
  free; their membership stays committed-only. Follow-up issues.
- A nested `with-transaction` (an independent commit, per #401) does not
  see the outer transaction's writes. Unchanged, stated in the manual.
- Snapshot *membership* of committed entries (#345, Phase C). Orthogonal.

**Anti-goals (what would make this a failure even if the tests pass)**

- A write path that gets slower, or a commit that serialises more. The
  overlay is built from the write-set the transaction already keeps; no
  write does extra work beyond one hash-table store.
- An own write leaking into the shared node cache (`graph-cache`) or into
  another transaction's reads. Own writes live only in the transaction's
  private `local-cache` and overlay.
- A `copy` of an own write that archives the wrong pre-image (§4.4).
  #135 made the graph unopenable from exactly this class of mistake.
- A read inside a transaction that costs O(write-set) per visited node.
  O(write-set) per *scan* is the ceiling for this unit; §6 says how D
  removes even that.

## 3. Constraints

- SBCL is release-gating; CCL, ECL and LispWorks arms must keep compiling
  (`#+` conditionals in any hash-table code; `make-id-table` already hides
  the id-hash difference).
- Lisp style per the user CLAUDE.md: spaces, 80 columns, terse comments
  that point at this document.
- The main suite before each commit of an engine change; CI runs the full
  suite on push to `experiment` (`docs/ci.md`).
- Every test on the #408 table must be shown non-vacuous by ablation:
  remove the overlay call it protects and watch it go red (memory:
  mutation testing is the gate).
- The node cache hides identity bugs; tests that compare two lookups bind
  `*cache-enabled*` to nil (CLAUDE.md, Tests).

## 4. Design

### 4.1 Two layers

The ten rows split by what fixes them.

| Layer | Rows | Mechanism |
|---|---|---|
| Values | `lookup-*` of a created node; slot value after `save`; deleted edge still live in a scan | the transaction's own writes in its `local-cache` |
| Membership | created node missing from type scan, adjacency, `edge-exists-p`, `index-lookup`; moved index key | an overlay consulted at each enumeration point |

Both rest on one fact: every transactional read funnels through the
transaction method of `lookup-object` (`transactions.lisp`), which consults
`local-cache` before the store, and every typed or adjacency scan
enumerates ids from an index list and resolves each through `lookup-vertex`
or `lookup-edge`. Fix the funnel and the enumeration points, and the
Prolog functors (58 call sites into these entry points), `traverse`,
`outgoing-edges`, `incoming-edges` and `active-edge-p`'s endpoint check
follow without a change.

### 4.2 Layer 1: own writes in the local cache

`%create-node`, `update-node` and `delete-node` (`transactions.lisp`) each
store the node they register — the created node, the saved copy, the
deleted copy — in `(local-cache *transaction*)` under its id, replacing
whatever committed version an earlier read cached there. Nothing else
changes in `lookup-object`:

- a cache hit returns before snapshot resolution, so an own write is never
  resolved against an epoch (it has none);
- a cache hit returns before the read-set store, so an own write never
  enters the read-set (§4.5);
- the shared `graph-cache` is never written on this path.

`lookup-vertex` keeps returning a node "regardless of its deleted flag";
the deleted copy comes back and the generated `lookup-<type>`, the scans
and `active-edge-p` filter it as they filter a committed tombstone.

Retry and rollback need no work: `call-with-transaction` creates a fresh
transaction per attempt, and the cache dies with the transaction.

### 4.3 Layer 2: the overlay at the enumeration points

The overlay is the write set seen through one small interface, so that
its first implementation (a scan of the write set per query) can be
replaced by maintained buckets (strategy D, §6) without touching a caller.

```
(tx-overlay tx)                     ; the overlay object, built lazily
(overlay-effective-write ov id)     ; the write that wins for ID, or NIL
(overlay-nodes ov kind type-ids)
   ; own live nodes of KIND (:vertex / :edge) whose type-id is in TYPE-IDS:
   ; creates and updates, never a node whose effective write deletes it
(overlay-edges-at ov vertex-id direction type-ids)
(overlay-edges-between ov from-id to-id type-ids)
   ; own live edges matching the adjacency key
(overlay-index-members ov six key &key prefix lo hi)
   ; (key . node) pairs for own live nodes whose %index-tuple-key for SIX
   ; matches KEY (or PREFIX, or the [LO,HI] range)
```

"Effective write" follows `make-commit-view`: the write set is keyed by id
and `add-to-object-set` replaces, so a create followed by a delete leaves a
`tx-delete`, and the node is absent. The overlay is built from
`(writes tx)` — creates then write-set, later entries win — exactly as the
commit view builds its table today. Builder's choice whether to share the
commit view's table or build a sibling; the rule must be one function.

**One rule at every enumeration point.** The committed walk skips every id
that has an effective own write (one hash probe per id,
`overlay-effective-write`); the overlay then supplies every own live node
that matches the walk's shape, from its *current* state. So an own update
is emitted once, by the overlay, at its re-derived type, endpoints or
index key, and an own delete is emitted by neither side. The rule does not
depend on which slots an update may change, and each ablation removes one
call: the skip, or the supply. Per point:

- **`map-vertices`, typed form** (`vertex.lisp`): after the type-id walk,
  emit `overlay-nodes :vertex type-ids`. The scans' `written-p` filter
  must not apply to these — a created node has `written-p` nil until
  commit — so the overlay's nodes go through the deleted filter and `fn`
  only. `include-deleted-p` applies to them as to committed nodes.
- **`map-edges`, typed form** (`edge.lisp`): the same with `:edge` and the
  edge filter (`active-edge-p`, whose endpoint lookups already resolve an
  own-created endpoint through layer 1).
- **`map-edges` with `:vertex` + `:direction`**: append
  `overlay-edges-at`. With `:from-vertex` + `:to-vertex`:
  `overlay-edges-between`.
- **`edge-exists-p`**: after the vev walk finds nothing, return the first
  of `overlay-edges-between` for the single type-id.
- **`index-lookup`** (`index.lisp`): the committed walk skips own ids, so
  an own update that moved its key (the #358 case) is not found under the
  old key; `overlay-index-members` for `key`/`prefix` finds it under the
  new one. `collect-p nil` short-circuits on either side.
- **`map-index` / `index-range`**: the overlay's members within
  `[start, end]` are **merged in key order** with the cursor walk, since
  `map-index` promises ascending order. The order is the secondary skip
  list's own: `%index-comp-lessp` over the deserialised `(v1 … vn id)`
  list (`index.lisp`, the comparator the backend is built with), never
  the serialised octets.

The builder still guards emission by id, because
`*add-to-indexes-unless-present-p*` replay paths exist.

Under `:as-of`, nothing changes: `call-with-read-snapshot` already refuses
an as-of read inside a read-write transaction on the same graph, so the
overlay is unreachable there. A plain `with-read-snapshot` inside a
transaction inherits the transaction and so sees its writes; that is the
transaction's view, and is stated in the manual.

### 4.4 Copying your own write

Today a lookup never returns an uncommitted node, so `copy` of one cannot
happen. After layer 1 it can, and the current `copy-node` would register
`copies[new] = the uncommitted copy`, so `update-node` builds a
`tx-update` whose `old-node` is uncommitted and `apply-tx-write` archives
the wrong pre-image. This is the #135 class of fault.

Rule, for both `copy-node` and `delete-node`: when NODE is (by `eq`) the
`node` of a `tx-update` in this transaction's write-set, the pre-image is
`(old-node that-write)` — the committed original. `copy-node` registers
`copies[new]` to it, so the eventual `tx-update` chains to the real
pre-image and the write set, keyed by id, simply replaces the earlier
update. `delete-node` builds its `tx-delete` with that `old-node` instead
of its argument; without this, save-then-`mark-deleted` of the looked-up
copy would archive an uncommitted head (cutting the version chain) and
release the copy's *new* secondary-index key, orphaning the committed
entry under the old one. `copy` of an own-*created* node keeps signalling
`copying-uncommitted-node` (#135): it is already writable. A `tx-delete`'s
node is a tombstone the generated lookups filter; copying it falls under
the same rule (it is a `tx-update` subclass) and the builder tests that a
copy-then-save of a node deleted earlier in the transaction un-deletes it,
which is the commit-view semantics and what the write set already does if
the user holds the objects.

### 4.5 OCC, validation, snapshot reads

- The read-set gains nothing from reading an own write (layer 1 returns
  before the store). The write-set already validates the id against
  concurrent committers, so serialisability is exactly what it was.
- The write-set, create-set, `writes` ordering, serialisation and replay
  are untouched. The overlay is derived state.
- `*record-reads*` nil scans (#92) behave as before for committed nodes;
  overlay additions were never reads.
- `*snapshot-reads-p*` resolution never sees an own write.

### 4.6 Code that composes its own overlay reads committed state

The commit view's evaluators are hand-rolled overlays: "committed scan
plus write-set delta". Cardinality's `view-edge-count` is `map-edges`
plus `%overlay-edge-delta`; spacetime's `%name-count` is `map-index` plus
`%created-under`; `validate-writes` builds a commit view from *proposed*
writes that need not be the ambient transaction's. And `%commit` runs its
validators inside `call-with-transaction`'s cleanup with `*transaction*`
still bound, so every one of them would run under the new overlay and
double-count an own edge or claim.

Rule: **the engine's overlay is on for user reads and off underneath a
commit view.** A special `*read-own-writes*`, `defvar`ed in
`globals.lisp` (the defvar-before-bind trap) with default `t`, gates
the reads, not the writes: own writes still go into the local cache, but
with the switch nil the enumeration points add nothing and
`lookup-object` passes over a cache entry that is an own write, so a
validator sees the committed version as today. It is bound to nil:

- around the validation region of `%commit`, so the commit path reads
  exactly what it reads today, validators and `*commit-validators*` hooks
  alike;
- around `validate-writes` (hence `validate-transaction`), which promises
  to answer "as `%commit` would".

Those two are the only bindings. The design first listed
`view-edge-count` and spacetime's `%name-count` / `%name-admitted-p`
too; implementation showed both dead: every caller of the first already
runs under one of the two bindings or with no transaction, and the
second pair runs only under an as-of snapshot, which is refused inside
a read-write transaction. No test could kill them, so they went
(commits 7b3523c, bf5a68b).

Consumers that already dedup by id (`%overlay-transaction`,
`%post-commit-subject-claims`, link's `%same-store-candidates`) need
nothing. Deleting the hand-rolled overlays in favour of the engine's is a
follow-up issue, not this unit: binding the switch is mechanical and
leaves their suites as the proof that nothing moved.

Rules' `claim/7` appends index hits to `*claim-derived-this-run*` without
a dedup (`rules/facts.lisp`), safe today because `%exclude-producers`
drops a rule's own claims; under `*rules-naive-rounds*` (the test
reference mode) an own derivation can now be re-found through the index
in the same transaction. The rules suite under this change is on the
test list; if the naive-rounds reference goes red, the fix is a dedup by
id there, not a binding of the switch, because a rule's fixpoint *wants*
to see its own writes.

### 4.7 Documentation that changes with the code

- Manual (`docs/vivace-graph-v3-doc.org`), transactions chapter: a new
  "What a transaction sees" section with the layer table, the non-goals
  list and the copy rule; the sentence "indexes update at commit" stays,
  qualified.
- `docs/codebase-notes.md`: the transaction idiom gains one line.
- `docs/rules.md` §327–371: the two sentences about the gap.
- #401's contract text: the bullet "no read-your-own-writes" becomes the
  new rule with its non-goals.
- CHANGELOG entry under unreleased.
- Spacetime's `%overlay-transaction` (dedups by id) keeps working
  unchanged; `%name-count` and cardinality's `view-edge-count` keep
  working under the §4.6 binding. Deleting those hand-rolled overlays in
  favour of the engine's is a follow-up issue filed with this work.

## 5. Tests

A new file `tests/read-your-own-writes-tests.lisp` in the main suite
(grep `tests/` for name clashes before naming; the suite shares one
package). The #408 table is the matrix: one test per row, each ablated by
removing the single overlay or cache call it protects. Beyond the table:

- the issue's drop-then-make repro, two passes, exactly one edge after
  commit;
- create then `mark-deleted` in one transaction: invisible to every entry
  point, and the commit leaves nothing;
- `copy` of an own update: the archived pre-image is the committed
  version (read it back through `:as-of` after commit);
- `copy` of an own create still signals `copying-uncommitted-node`;
- save then `mark-deleted` of the looked-up copy: the archived pre-image
  is the committed version and the committed secondary-index entry is
  released, not the copy's;
- a `def-cardinality :max 1` vertex gains one edge in a transaction and
  commits: the validator counts it once (and `validate-transaction`
  inside the transaction agrees); the cardinality, value-constraint,
  evaluator, spacetime and rules suites stay green, including a run with
  `*rules-naive-rounds*`;
- `map-index` order with own writes interleaved among committed keys;
- a moved key: absent under the old key, present under the new, in
  `index-lookup`, `map-index` and `index-range`;
- nested `with-transaction` does not see the outer's writes;
- after rollback (non-local exit) a new transaction sees nothing of it;
- the same checks with `*cache-enabled*` nil;
- a read inside the transaction adds nothing to the read-set, and a
  concurrent commit to an unrelated node does not force a retry
  (`tests/acid/isolation-tests.lisp` has the two-thread shape);
- nothing changes outside a transaction: the existing suites are the
  regression net; `tests/perf/` gets one scan-inside-a-transaction case
  with a 1,000-write set so D has a baseline (`docs/perf-baselines.md`).

## 6. Left to the builder, and the path to D

- Whether the overlay shares `commit-view`'s table or builds its own;
  one id→effective-write rule either way.
- The first implementation may scan the write set per query. If
  `tests/perf/` shows the scan-in-a-transaction case regressing against
  the baseline, the builder buckets lazily: build type/adjacency/index
  buckets on the first overlay query after a write and mark them dirty on
  the next write. That is most of D behind the same interface; full D
  (buckets maintained on every write) is a later unit if a bulk-import
  shape ever needs it.
- The merge comparator for `map-index` (§4.3), confirmed from the skip
  list rather than assumed.
- File order: the overlay functions live where `lookup-object` can call
  nothing of them (layer 1 needs only the cache) and the scans can; a
  new file after `transactions.lisp` in `graph-db.asd`'s chain is the
  natural place, with `index.lisp` already later in the chain.
  `*read-own-writes*` goes in `globals.lisp` (CLAUDE.md, engine traps).
- Layer 1's store in `%create-node` belongs in the `tx` path only:
  `recovery-transaction` has its own `%create-node` method and no
  `local-cache` slot, and replay must not change. `update-node` and
  `delete-node` already require a `tx` through `copies`.
- Work on a branch off `experiment`, one commit per layer plus one for
  docs, pushed with the next batch; the issue closes with the landing
  SHA.
