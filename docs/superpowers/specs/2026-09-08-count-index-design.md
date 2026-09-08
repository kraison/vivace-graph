# A counting index: design

**Issue:** kraison/vivace-graph#361, the cost half of #358, on top of
#350 (spec `2026-09-07-claim-vocabulary-design.md`, which this
supersedes for the three vocabulary functions' mechanism). **Prior
decisions:** #107 (multi-slot indexes, canonical tuples, the sidecar),
#139/#140 (named schema identity, one identity rule across registries),
#147 (reclaim at open), #324 (reads inside a transaction see what it
will commit), #345 (value indexes keep live membership), #113
(epoch-stamped entries, later). **Date:** 2026-09-08. **Status:**
approved in review, sections 1–3; amended 2026-09-08 against the engine
facts note (`docs/superpowers/notes/2026-09-08-count-index-engine-facts.md`),
which overturned eight assumptions of the first draft (§X there).

## 0. Problem

#350 derives a claim family's vocabulary from its ordered indexes by a
distinct-prefix walk. That is sub-linear for names but not for counts:
a `:current` count must resolve every entry under a name to tell a
current claim from a retracted one, and each of the three functions
resolves every entry once per index it walks. A tenant listing
namespaces, keys and relations with `:current t` pays about five node
resolutions per claim; its own single walk of the family pays one, so
the tenant kept the walk (kraison/cl-llm#68). A listing should be a
lookup, and a count should cost nothing at read time.

## 1. Rulings

| # | Ruling | Why |
|---|--------|-----|
| R1 | Counters are live at commit granularity: a reader under a read snapshot or an as-of extent sees later commits. Snapshot-consistent counts are not built here; they ride #113's epoch-stamped entries. | Every value index in the engine has the same bound (#345, `docs/time-travel.md`); versioned counters would touch the reaper and the epoch machinery for a consumer that reads under no snapshot older than its own call. |
| R2 | A generic engine facility, `def-count-index`, not a spacetime-only structure — with its own registry, graph slot, sidecar, comparator and key codec, on the `def-unique` pattern. | The secondary machinery cannot host it: its registry readers are kind-blind and would build a count spec as a real secondary index, its built-object table is keyed `(owner . slot-names)` and the family declares both kinds on the same slots, its comparator and key codec assume a trailing id. Two registries, one identity rule, is the engine's existing pattern. |
| R3 | Every leading prefix of the tuple is counted, keyed by depth first. | Namespace-level answers become lookups and the keys under a namespace one contiguous range; five in-place counter updates per claim write is small beside the index inserts a claim already pays. |
| R4 | A key is removed when its `all` counter reaches 0. | The map's keys are then exactly the names with a committed claim, so a listing needs no filter. |
| R5 | The three vocabulary functions keep their signatures and semantics; only their mechanism changes. | #350's tests are the contract; nothing a consumer sees moves. |
| R6 | The unreleased `claim-relation` secondary index (#350) is withdrawn from `def-claim-classes`, by an emitted `unregister-index-spec` form, not by deleting its declaration. | The relation count index answers everything it was added for; deleting the form withdraws nothing in an image that already ran the older macro. |
| R7 | Under an open as-of extent each name is still confirmed by one node resolution at the epoch; counts there are live (R1). | Keeps #350's as-of behaviour for names at O(names) without pretending counters are versioned. |
| R8 | A counter is never re-applied: any apply that runs with `*add-to-indexes-unless-present-p*` true (crash-recovery replay, device state-sync re-pull) marks the graph's count indexes stale instead of counting, and a stale index is rebuilt by scan — at open after a crash-recovery replay, and lazily on the first count query otherwise. | A set index absorbs a replayed insert; a counter doubles it. Both re-apply paths announce themselves through that one special, and a scan is authoritative and idempotent. |
| R9 | The counters are serialised by the transaction manager's lock (commit apply runs inside it) and by the device's single writer thread (the replication paths); the maintenance takes no lock of its own. | On SBCL the backend's read/write locks are no-ops; nesting one deadlocks on ECL; the two funnels already serialise every index write. |

## 2. The count index (engine)

### 2.1 Declaration and registry

```lisp
(def-count-index owner-class (slot ...) graph-name
                 &key name current-p canonicalize)
(undef-count-index owner-class graph-name &key slots name)
```

Declarative and idempotent like `def-index`, on the `def-unique`
pattern: a `count-index-spec` struct (owner, slot names, graph name,
canonicalize, name, `current-p` as a symbol) in its own
`*schema-count-metadata*` table keyed by graph name, sharing
`%spec-identity` (a named declaration replaces in place; an unnamed one
is identified by its slots), with `register-count-index-spec`,
`unregister-count-index-spec`, `%registered-count-index-specs`, and a
`%count-index-spec-declared-p` that fails safe towards keeping like the
secondary one. Withdrawal warns through `%withdrawn-p` when nothing
matched. Built objects live in a new graph slot, `count-indexes`, a
hash keyed `(owner . slot-names)`; the key space is separate from
`secondary-indexes`, so a family may declare both kinds on the same
slots. CURRENT-P is stored as a symbol and `funcall`ed at maintenance
time, never resolved at declaration; NIL keeps only the total.

The applicable declarations for a node's class are found as
`%applicable-index-descriptors` finds them: the owner is the class or
an ancestor and every slot exists in the class.

### 2.2 Shape

One ordered map per built index, created by a third generic,
`make-count-skip-list`, on the graph's index backend (heap skip list,
B+ tree, or memory skip list). Keys are `(depth v1 ... vk)` for every
leading prefix of the canonical tuple, `depth` = k, so all depth-1
entries are contiguous and the depth-2 entries under one depth-1 value
form one range. The comparator is `%index-value-lessp` (the id-free
lexicographic order by `less-than`, a strict prefix before its
extension); key equality is `equal` on the whole list; the sentinels
are the one-element `(:gmin)` and `(:gmax)`. The key codec is plain
`serialize`/`deserialize`; the value codec is the one every heap index
already has. Null components are stored as `+null-component+` (#107)
and read back as NIL.

The value is the counter pair `(all . current)`, two non-negative
integers; `current` is NIL when CURRENT-P is NIL, so the pair is then
the list `(all)`. A key whose `all` reaches 0 is removed (R4).

### 2.3 Maintenance

A fourth pass in `apply-transaction` and in the two replication apply
functions, immediately after `apply-tx-writes-to-secondary-indexes`,
plus an explicit release in `peer-purge-node` beside its direct
`%ix-release` — the one maintenance site that bypasses the generic.
The pass is a generic with methods on all three write kinds (a
`tx-delete` is a `tx-update` subclass, so each is specialised):

- `tx-create`: for each prefix of the new node's tuple, `all` += 1
  and, if `(funcall current-p node)`, `current` += 1.
- `tx-delete`: the old node's contribution subtracted once (the
  secondary method releases twice; a counter must not); keys at 0 are
  removed.
- `tx-update`: if the old and new tuples differ, subtract the old
  node's contribution from the old tuple's prefixes and add the new
  node's to the new tuple's; otherwise `current` += `(pred new)` −
  `(pred old)`, `all` unchanged. A tuple whose every component is null
  (`%index-tuple-key` → NIL) contributes nothing; a geometry component
  (`%tuple-indexable-p` false) likewise.

Each counter step is read-then-branch, because `update-in-skip-list`
is an upsert on the two on-disk backends and a no-op for an absent key
on the memory backend: `find-in-skip-list`, then `add-to-skip-list`
when absent, else `update-in-skip-list` with the OLD value passed as
its fourth argument so the in-place path is taken (without it every
update is a remove-plus-add with a deferred heap free). No lock of the
pass's own (R9).

**Idempotency (R8).** When `*add-to-indexes-unless-present-p*` is
true the pass does not count; it sets `count-indexes-stale-p` on the
graph. `rebuild-count-indexes` clears it: it empties every built map
and applies the create rule to every live node of each owner class
(typed scan, deleted nodes skipped, one bad node tolerated as
`%build-index-for-spec` tolerates it). It runs at open when the graph
was crash-recovered (the replay precedes the sidecar restore and would
otherwise be discarded), and lazily from `count-index-lookup` /
`map-count-index` when the flag is set. The flag is in memory only:
a crash between a state-sync re-pull and a close leaves a sidecar whose
counts the crash-recovery rebuild at the next open replaces anyway.

### 2.4 Persistence

A separate sidecar, `count-indexes.dat`, written by
`save-count-index-roots` at close (unguarded, like the other two saves:
a stale root is silently wrong) and read by
`restore-count-index-roots` at open, with records `(owner slot-names
address backend-tag)`; the `current-p` symbol and canonicalizers are
not stored, they are re-resolved from the live declaration at restore
like `%owner-slot-canonicalizer`. A record whose declaration is
withdrawn is reclaimed at open (#147); a missing sidecar falls to
`rebuild-count-indexes`; `install-count-indexes` builds any declared
index the sidecar did not cover. The memory graph has no sidecar and
rebuilds at open unless lazy, where nothing is built and every query
answers empty. `regenerate-secondary-indexes` (the backend switch)
regenerates the count maps too. No node-format change, and the
secondary sidecar is untouched in both directions.

### 2.5 Query primitives

Exported from `graph-db`:

```lisp
(count-index-lookup graph class-name slot-name tuple)
  ;; => (values all current); 0 0 for an absent name
(map-count-index fn graph class-name slot-name &key (depth 1) prefix)
  ;; FN called with (components all current), index order,
  ;; over the entries at DEPTH whose leading components equal PREFIX
```

TUPLE and PREFIX are canonicalised as `%index-key` canonicalises a
query value; a full-arity all-null tuple answers 0 0. An undeclared
index signals `query-precondition-error`; a declared-but-empty one
(including a lazy memory graph) answers 0 0 and calls FN zero times.
Both first rebuild if the stale flag is set (§2.3), then position in
O(log n) and read O(entries returned); neither resolves a node.

### 2.6 Bounds

Live at commit granularity (R1). Inside an open transaction the map
holds the pre-transaction membership; a caller that needs the
transaction's own writes overlays them (§3.3). A walk with
`map-count-index` is not an atomic snapshot: entries can change between
steps, as with every cursor read. A caller that mutates a count map
outside the two serialising funnels (R9) gets no promise.

## 3. Spacetime on top

### 3.1 Declarations

`def-claim-classes` emits, before its declarations,
`(graph-db:unregister-index-spec ',parent ',graph-name :name 'claim-relation)`
(R6; silent when nothing matched), and declares, with
`:current-p 'claim-current-p`:

| slots | owner | `:name` |
|---|---|---|
| `(subject-namespace subject-key)` | parent | `claim-subject-count` |
| `(object-namespace object-key)` | binary | `claim-object-count` |
| `(relation)` | parent | `claim-relation-count` |

An existing family drops the `claim-relation` record at open (the
reconciliation of #147) and builds the three count indexes over its
claims.

### 3.2 The three functions

Signatures and semantics as #350 §4 (R5). Mechanism:

- `claim-namespaces`: `map-count-index` at depth 1 over the subject
  count index (parent class) and the object count index (binary class),
  merged and de-duplicated by the index collation, counts summed under
  `:either`; with `:current t` an entry is kept only if its `current`
  counter is above 0, and that counter is the count.
- `claim-keys`: the same at depth 2 with `:prefix (list namespace)`,
  merged, then `%paginate`.
- `claim-relations`: depth 1 over the relation count index.

Names are the stored components with `+null-component+` read back as
NIL. No node is resolved on the committed path. Of #350's helpers,
`%refuse-vocabulary-axis`, `%vocabulary-sources`, `%vocabulary-view`,
`%view-resolve`, `%claim-tuple`, `%name-lessp` and `%merge-names` stay;
`%vocabulary-key`, `%name-admitted-p`, `%name-count` and `%walk-names`
go; `%created-under` becomes a per-name delta.

### 3.3 Inside a transaction

What the transaction will commit (#324): the map's answer adjusted by
the transaction's writes, bucketed by name once per call — a created
claim adds to its names' `all` and, if current, `current`; a deleted
claim subtracts the old version's contribution; an updated claim moves
`current` by the predicate's flip on its committed version, and moves
both counters if its tuple changed. Names introduced only by the
transaction's creates are added in index order; a name whose adjusted
`all` is 0 is dropped. Bounded by the write set.

### 3.4 Under an as-of extent

The map is live (R1). When an as-of snapshot of the graph is open
(`graph-db::%as-of-snapshot`, unexported), each name is confirmed by
resolving one node under it at the epoch — the subject or object
secondary index range of #350, first live hit — and dropped when none
resolves; O(names). Counts are the live counters and the manual says
so. `:as-of` and `:as-of-epoch` as keyword arguments stay refused with
`query-precondition-error`.

### 3.5 What stays

`map-index-prefixes` and `index-count` (#350) stay in the engine as
general primitives; the vocabulary no longer calls them. #350's
`claim-subject`, `claim-object`, `claim-producer` and
`claim-subject-relation` secondary indexes are untouched.

## 4. Testing

Engine, a new `tests/count-index-tests.lisp` (suite `count-index-suite`
under `graph-db-suite`, not on `*slow-suites*`, fixtures as
`index-tests.lisp`, owner class `ix-claim`, which already carries two
secondary indexes so a count index on `(ns key)` proves the two kinds
coexist on one owner and slots), each test naming its mechanism:

- counters at every depth after creates, including a duplicate tuple;
- delete removes the key when `all` reaches 0 and leaves a sibling;
- an update that flips CURRENT-P moves `current` and leaves `all`;
- an update that changes an indexed slot moves both counters to the new
  tuple and removes the emptied old key;
- `map-count-index` at each depth and under a prefix, in index order;
  `count-index-lookup` on an absent name is 0 0; an undeclared index
  refuses; a full-arity all-null tuple is 0 0;
- a null component is stored and read back as NIL;
- a counter crossing a serialization-length boundary (255 → 256) keeps
  counting on the heap backend;
- sidecar round trip: close, reopen, same counters; build-at-open for a
  declaration the stored graph predates, done with a withdrawn open and
  close so the sidecar cannot restore it (the #350 test's shape);
  `undef-count-index` withdraws: the lookup refuses, the record is
  reclaimed on the next open;
- **idempotency (R8):** applying the same create writes twice under
  `*add-to-indexes-unless-present-p*` leaves the counters equal to one
  application (the stale flag and the lazy rebuild), red under an
  ablation that counts anyway; a crash-recovered open (a `.txn` left on
  disk, no sidecar save) reports the replayed tail;
- the cost claim: a listing over N nodes performs zero node resolutions
  (a probe on `%node-by-id` or an `:around` on `lookup-vertex`, with a
  control), red under an ablation that resolves;
- the memory graph (including the first increment of a name, the
  backend whose update is not an upsert) and the B+ tree backend for
  the counter round trip.

Spacetime (`tests/spacetime/vocabulary-tests.lisp`): every #350 test
passes unchanged (R5); new tests pin that a `:current` listing under a
plain read snapshot resolves no node; that `current` moves on
retraction and on a same-transaction create-then-delete; that a family
opened over pre-existing claims lists relations from the count index
built at open, with the withdrawn `claim-relation` record gone even
when the old declaration was registered in the image first (R6's
unregister); the as-of test keeps R7's confirmation honest by ablation.

Replication (`tests/peer-index-tests.lisp`, suite `peer-index-suite`):
an authored create, a retraction as a `tx-update` with an old node, a
state-sync create applied twice (R8), and a purge each leave the
device's counters equal to a locally committed graph's.

CI's full SBCL suite is the gate.

## 5. Documentation

- `docs/general-index-design.md`: a new section beside §6a for the count
  index — declaration, key shape, maintenance rules, idempotency and
  the stale flag, persistence, bounds.
- `docs/vivace-graph-v3-doc.org`, "Vocabulary: what a family names":
  rewritten for lookups; the fifth-index sentence goes.
- `CHANGELOG.md`: an Unreleased entry naming `def-count-index`, the
  three family declarations, the withdrawn `claim-relation` index, and
  the cost change.
- The #350 spec gains a Status note: mechanism superseded by this spec
  for the three functions.

## 6. Issues

#361 closes with the merge and closes #358's cost half (the
identity-update bound stays on #358). kraison/cl-llm#68 gets a
follow-up: collapse the hybrid onto the engine once the pin moves.

## 7. Out of scope

- Versioned or snapshot-consistent counters (#113).
- A Prolog surface for counts.
- Producers: `claims-by-producer` and the `claim-producer` index stay
  as they are; a producer count index is one declaration a tenant can
  add.
- Changing `claims-touching` or its overlay (#358's other half).
- Persisting the stale flag: a crash after a re-pull is covered by the
  crash-recovery rebuild.
