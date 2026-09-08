# A counting index: design

**Issue:** kraison/vivace-graph#361, the cost half of #358, on top of
#350 (spec `2026-09-07-claim-vocabulary-design.md`, which this
supersedes for the three vocabulary functions' mechanism). **Prior
decisions:** #107 (multi-slot indexes, canonical tuples, the sidecar),
#324 (reads inside a transaction see what it will commit), #345 (value
indexes keep live membership), #113 (epoch-stamped entries, later).
**Date:** 2026-09-08. **Status:** approved in review, sections 1–3.

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
| R2 | A generic engine facility, `def-count-index`, not a spacetime-only structure. | The engine already holds every piece a counter needs: the schema registry, maintenance dispatch on the three write kinds, the sidecar, the rebuild scan, an ordered map with a value slot. A counting index is a general thing, and a spacetime-private map would be the one index-like structure outside rebuild-at-open and sidecar reconciliation. |
| R3 | Every leading prefix of the tuple is counted, keyed by depth first. | Namespace-level answers become lookups and the keys under a namespace one contiguous range; five in-place counter updates per claim write is small beside the index inserts a claim already pays. |
| R4 | A key is removed when its `all` counter reaches 0. | The map's keys are then exactly the names with a committed claim, so a listing needs no filter. |
| R5 | The three vocabulary functions keep their signatures and semantics; only their mechanism changes. | #350's tests are the contract; nothing a consumer sees moves. |
| R6 | The unreleased `claim-relation` secondary index (#350) is withdrawn from `def-claim-classes`. | The relation count index answers everything it was added for; keeping both would pay two inserts for one question. |
| R7 | Under an open as-of extent each name is still confirmed by one node resolution at the epoch; counts there are live (R1). | Keeps #350's as-of behaviour for names at O(names) without pretending counters are versioned. |

## 2. The count index (engine)

### 2.1 Declaration

```lisp
(def-count-index owner-class (slot ...) graph-name
                 &key name current-p canonicalize)
(undef-count-index owner-class graph-name &key slots name)
```

Declarative and idempotent like `def-index`: it registers an
`index-spec` of kind `:count` in the same schema registry under the
same named identity (`%spec-identity`), builds now when the graph is
open and at open otherwise (`install-secondary-indexes`), and is
withdrawn by `undef-count-index` exactly as `undef-index` withdraws.
CURRENT-P names a function of one node; NIL keeps only the total.
CANONICALIZE is the per-component canonicaliser `def-index` takes.

### 2.2 Shape

One ordered map per declaration, on the graph's index backend like a
secondary index (heap skip list, B+ tree, or memory skip list). Keys
are `(depth v1 ... vk)` for every leading prefix of the canonical
tuple, `depth` = k, so all depth-1 entries are contiguous and the
depth-2 entries under one depth-1 value form one range; within a depth
the order is the index collation (`%index-comp-lessp` without the id
tie-break). Null components are stored as `+null-component+` as in
#107. The value is the counter pair `(all . current)`, two
non-negative integers; `current` is absent (NIL) when CURRENT-P is NIL.
A key whose `all` reaches 0 is removed (R4).

### 2.3 Maintenance

A fourth method set on `apply-tx-write-to-secondary-indexes`, so the
commit apply and the two replication apply paths inherit it:

- `tx-create`: for each prefix of the new node's tuple, `all` += 1 and,
  if `(funcall current-p node)`, `current` += 1.
- `tx-delete`: the same from the old node, subtracted; keys at 0 are
  removed.
- `tx-update`: if the old and new tuples differ, subtract the old
  node's contribution from the old tuple's prefixes and add the new
  node's to the new tuple's; otherwise `current` += `(pred new)` −
  `(pred old)`, `all` unchanged. A node that is not indexable
  (`%tuple-indexable-p` false, every component null) contributes
  nothing, on either side.

Counters update in place through the backend's update path
(`update-in-skip-list`); the maintenance takes no lock of its own,
the backend's write lock covers the read-modify-write per key. The
rebuild scan (`%build-index-for-spec`, `rebuild-secondary-indexes`)
visits every live node of the owner class and applies the create rule.

### 2.4 Persistence

The sidecar gains a record kind: a count index's root is saved by
`save-secondary-index-roots` and restored by
`restore-secondary-index-roots`, reconciled against the live schema
through `%index-spec-declared-p` like the others; a missing or
withdrawn record falls to the rebuild scan at open. No node-format
change.

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
query value. An undeclared index signals `query-precondition-error`,
a declared-but-empty one answers 0 and calls FN zero times. Both are
O(log n) to position and O(entries returned) to read; neither resolves
a node.

### 2.6 Bounds

Live at commit granularity (R1). Inside an open transaction the map
holds the pre-transaction membership; a caller that needs the
transaction's own writes overlays them (§3.3). A walk with
`map-count-index` is not an atomic snapshot: entries can change
between steps, as with every cursor read.

## 3. Spacetime on top

### 3.1 Declarations

`def-claim-classes` declares, with `:current-p 'claim-current-p`:

| slots | owner | `:name` |
|---|---|---|
| `(subject-namespace subject-key)` | parent | `claim-subject-count` |
| `(object-namespace object-key)` | binary | `claim-object-count` |
| `(relation)` | parent | `claim-relation-count` |

and no longer declares the `claim-relation` secondary index (R6). An
existing family drops that record at open (the reconciliation of
#147) and builds the three count indexes over its claims.

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
NIL. No node is resolved on the committed path.

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
(`%as-of-snapshot`), each name is confirmed by resolving one node under
it at the epoch — the subject or object index range of #350, first
live hit — and dropped when none resolves; O(names). Counts are the
live counters and the manual says so. `:as-of` and `:as-of-epoch` as
keyword arguments stay refused with `query-precondition-error`.

### 3.5 What stays

`map-index-prefixes` and `index-count` (#350) stay in the engine as
general primitives; the vocabulary no longer calls them. #350's
`claim-subject`, `claim-object`, `claim-producer` and
`claim-subject-relation` indexes are untouched.

## 4. Testing

Engine, a new `tests/count-index-tests.lisp` (suite `count-index-suite`
under `graph-db-suite`, fixtures as `index-tests.lisp`), each test
naming its mechanism:

- counters at every depth after creates, including a duplicate tuple;
- delete removes the key when `all` reaches 0 and leaves a sibling;
- an update that flips CURRENT-P moves `current` and leaves `all`;
- an update that changes an indexed slot moves both counters to the new
  tuple and removes the emptied old key;
- `map-count-index` at each depth and under a prefix, in index order;
  `count-index-lookup` on an absent name is 0 0; an undeclared index
  refuses;
- a null component is stored and read back as NIL;
- sidecar round trip: close, reopen, same counters; build-at-open for a
  declaration the stored graph predates, done with a withdrawn open and
  close so the sidecar cannot restore it (the #350 test's shape);
- `undef-count-index` withdraws: the lookup refuses, the sidecar record
  is dropped on the next open;
- the cost claim: a listing over N nodes performs zero node resolutions
  (the `lookup-vertex` probe of cl-llm#68's test), red under an
  ablation that resolves;
- memory graph and B+ tree backends for the counter round trip.

Spacetime (`tests/spacetime/vocabulary-tests.lisp`): every #350 test
passes unchanged; new tests pin that a `:current` listing under a plain
read snapshot resolves no node; that `current` moves on retraction and
on a same-transaction create-then-delete; that a family opened over
pre-existing claims lists relations (the count index built at open, and
the withdrawn `claim-relation` record gone); the as-of test keeps R7's
confirmation honest by ablation.

Replication (beside `tests/peer-index-tests.lisp`): a streamed create,
retract and delete leave the receiving peer's counters equal to the
sender's.

CI's full SBCL suite is the gate.

## 5. Documentation

- `docs/general-index-design.md`: a new section beside §6a for the count
  index — declaration, key shape, maintenance rules, persistence,
  bounds.
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
