# A vocabulary index over a claim family: design

**Issue:** kraison/vivace-graph#350. **Prior decisions:** #107 (multi-slot
indexes, prefix ranges), #302 (the `(subject relation)` index), #324
(reads inside a transaction see what it will commit), #345 (value
indexes keep live membership), #115 (`with-as-of`), #160 (relations are
canonical strings). **Date:** 2026-09-07. **Status:** approved in
review, sections 1–4; amended 2026-09-07 against the engine facts note
(`docs/superpowers/notes/2026-09-07-claim-vocabulary-engine-facts.md`);
implemented on `feat/claim-vocabulary`.

## 0. Problem

`claims-touching` answers for one named endpoint and `claims-by-producer`
for one producer. Nothing lets a caller ask a claim family what
vocabulary it holds: which namespaces are in use, which relations, which
keys under a namespace, and how many claims each carries. A tenant that
needs a discovery step before an exact read walks every claim vertex of
the family under a snapshot, linear in the family. That is fine at
thousands of claims and will not stay fine.

## 1. Rulings

| # | Ruling | Why |
|---|--------|-----|
| R1 | Names are derived from the ordered indexes the family already maintains, by a seek-and-skip walk of distinct leading prefixes. No counting structure is added to the write path. | The indexes already hold every name in sorted order; a maintained counter would add write cost, a stored structure and MVCC bookkeeping to answer a question a range walk answers well enough. |
| R2 | Counts are opt-in and are the size of a name's prefix range: a bounded walk of index entries, no node resolution. | Discovery ranks a listing; it does not need counts to be free. |
| R3 | A single-slot `(relation)` index joins the family's declared indexes. | The only index carrying relation puts it third, so distinct relations cannot be skipped to; relation is the smallest vocabulary and the one asked about first. `def-index` builds it on next open of an existing family, so no migration. |
| R4 | `:current` follows `claims-touching`: the default lists every name the indexes hold, retracted claims included; `:current t` keeps a name only if a claim under it is current. | One convention across the read API, and the cheap path stays the default. |
| R5 | Inside an open transaction the answer is what the transaction will commit, through the commit-view overlay `claims-touching` uses. | #324's rule; a listing that hides the caller's own asserts misleads the retract-then-assert idiom. |
| R6 | `:as-of` and `:as-of-epoch` are refused in this cut. | Value indexes keep live membership (#345, `docs/time-travel.md` Bounds); an as-of listing would need the epoch-stamped index of #113. |
| R7 | Every reported name is confirmed by resolving one live node under it. | Index membership is live while resolution goes through the MVCC read path: under an open `with-as-of` extent the resolved version can be deleted or absent while the entry stands (#345), a crash before the index sidecar is saved leaves `rebuild-secondary-indexes` authoritative, and the engine's own `index-lookup` keeps a `deleted-p` guard. A phantom name is a silent wrong answer. |

## 2. Engine: the distinct-prefix walk

New in `index.lisp`, domain-neutral, alongside `map-index` and
`index-range`:

```lisp
(map-index-prefixes fn graph class-name slot-name &key arity start)
(index-count graph class-name slot-name value &key prefix)
```

`map-index-prefixes` calls FN with each distinct leading prefix of
ARITY components (default 1) held by the index, as a component list, in
index order, once each. START, a prefix of at most ARITY components,
begins the walk at that prefix. `index-count` returns the number of
entries whose tuple equals VALUE, or with `:prefix t` starts with it; 0
when the prefix is absent.

### 2.1 Mechanism

The composite key of an index entry is `(v1 ... vn id)`, and
`%index-bounds` gives, for a prefix P of fewer than n components, the
range `[P, P + max-sentinels + +max-key+]` that holds every tuple
starting with P (#107). The walk is:

1. Open a range cursor from START (or the head key) to the tail key and
   take its first entry; its first ARITY components are the first
   prefix. A range cursor seeks: `find-in-skip-list` on the heap and
   memory skip lists, `%bpt-leaf-at` on the B+ tree, all logarithmic.
2. Report the prefix, then open a new range cursor from the prefix's
   high bound (`%index-bounds` with `:prefix t`) to the tail key and take
   its first entry. Its key is strictly past every tuple sharing the
   prefix, so its first ARITY components are the next prefix.
3. Stop when a cursor is empty.

Each hop is one seek and one entry read, so the walk costs the number of
distinct prefixes times log n. The walk takes no lock of its own:
`make-range-cursor` and `cursor-next` own whatever locking each backend
has, and an outer lock would nest and deadlock on ECL
(`skip-list.lisp` on nesting). A multi-hop walk is therefore not an
atomic snapshot: entries can come and go between hops, and the
docstring says so. It never uses `ix-map`'s open-ended path, which is a
full scan.

`index-count` is `ix-lookup`'s range, counted instead of collected.

### 2.2 Null components

A tuple with a null component is stored with `+null-component+` in that
position (#107 §"null-bearing tuples"), and it sorts before every real
value, so a null-leading prefix is the first the walk reports. The walk
reports such a prefix with NIL in that component; a NIL in START is
mapped the other way. The caller decides what a NIL name means.

### 2.3 Deletion

A deleted node's entries are removed at commit apply
(`apply-tx-write-to-secondary-indexes` on `tx-delete`), so the walk does
not see them on the normal path. Inside an open transaction nothing has
been applied yet, which is what §4.4 is for; the other ways an entry and
its node can disagree are R7's.

## 3. The relation index

`def-claim-classes` declares, beside `claim-subject`, `claim-object`,
`claim-producer` and `claim-subject-relation`:

```lisp
(graph-db:def-index ,parent (relation) ,graph-name :name claim-relation)
```

`def-index` is declarative and idempotent: for an open graph it builds
the index now, otherwise `install-` builds it at open. A family whose
graph predates this unit gets the index on its next open, in one pass
over the family. No format change, no migration.

## 4. Surface (`spacetime/claim-query.lisp`)

Three functions, exported from `graph-db.spacetime` beside
`claims-touching`:

```lisp
(claim-namespaces graph claim-class &key (role :either) current counts)
(claim-relations  graph claim-class &key current counts)
(claim-keys       graph claim-class namespace
                  &key (role :either) current counts limit offset)
```

CLAIM-CLASS is the family's PARENT class name, as for `claims-touching`;
an unknown family signals what `claims-touching` signals.

### 4.1 Sources

- Namespaces: the arity-1 prefixes of `claim-subject` (`:role :subject`)
  and of `claim-object` (`:role :object`). The subject index is declared
  on the family's parent class and the object index on its binary class,
  so the object walk passes `claim-family-binary`, as `claims-touching`
  does; passing the parent signals.
- Keys under NAMESPACE: the arity-2 prefixes of the same two indexes
  starting at `(namespace)`, stopping at the first prefix whose namespace
  differs.
- Relations: the arity-1 prefixes of `claim-relation`.

`:role :either` (the default) merges the subject and object name lists
by the index collation and drops duplicates; the lists are short
(distinct names, not claims), so a sort by the same collation is the
merge.

### 4.2 Shape

Without `:counts`, each element is the name as stored in the claim's
slot: the engine canonicalises nothing, the tenant already does. With
`:counts t`, each element is `(name . count)`, where count is the number
of claims under that name in the requested role, and under `:either` the
sum over both roles. A caller wanting subject and object counts apart
calls twice, one role each; one return shape beats three.

Order is index order, the collation the index gives the slot's values:
for keyword namespaces that is `string<` on the symbol name, and a string
sorts after every symbol. `claim-keys` applies `:limit` and `:offset`
after the merge, so two calls page one deterministic list, and returns
`%paginate`'s second value (entries existed past the cut).

### 4.3 `:current`

Default NIL: every name the indexes hold, retracted claims included; they
are the record of what was believed. `:current t`: a name is kept only
if a claim under it satisfies `claim-current-p`, found by resolving
claims in the name's range until one does (usually the first, worst case
all of that name's claims); with `:counts t` every entry in the range is
resolved and only current claims are counted.

In either mode a name is confirmed by resolving one live node in its
range (R7); a name with none is dropped.

### 4.4 Inside a transaction

With `graph-db::*transaction*` open on GRAPH, the answer is what the
transaction will commit, through `make-commit-view` as
`%overlay-transaction` does today:

- a name introduced by a claim of the family the transaction created is
  added at its place in index order;
- confirmation (R7, §4.3) resolves through `view-node`, so a name whose
  claims the transaction deleted, or under `:current` retracted, drops;
- a count is the index count adjusted by the transaction's writes under
  that name: created claims add, deleted claims subtract, and under
  `:current` retracted claims subtract.

The adjustment walks `view-writes` once per call and is bounded by the
write set.

### 4.5 Refusals and bounds

`:as-of` and `:as-of-epoch` are accepted in the lambda list and refused
(R6): a call passing either signals a `query-precondition-error` naming
the reason, the typed refusal this file uses for the epoch axis. An open `with-as-of` extent on
GRAPH is not refused: it affects only the resolution step (confirmation,
`:current`), while name membership is live, the value-index bound
`docs/time-travel.md` states.

## 5. Testing

Engine (`tests/index-tests.lisp`, suite `index-suite`), each naming its
mechanism:

- `map-index-prefixes` on a three-slot index reports each distinct
  arity-1 and arity-2 prefix once, in order, and honours `:start`.
- The walk is sub-linear: with a counting wrapper around the range-cursor
  seek, a walk over N entries under K prefixes performs K+1 seeks; a
  control walk that visits every entry performs N. Proven non-vacuous by
  ablation (replace the hop with a linear step and the count assertion
  goes red).
- `index-count` equals the range size, `:prefix t` counts a prefix, and
  an absent prefix counts 0.
- A tuple with a null component is reported with NIL in that position.
- `def-index` declared on a class with existing instances builds the
  index on open (the relation index's contract).

Spacetime (`tests/spacetime/`, suite `spacetime-suite`):

- namespaces by `:subject`, `:object` and merged `:either`, with counts
  summing over roles;
- keys under a namespace, `:limit`/`:offset` paging stable across two
  calls, and a namespace with no keys answering NIL;
- relations from a family opened over pre-existing claims (the index was
  built on open);
- `:current t` drops a name whose only claim was retracted and counts
  only current claims; a deleted claim's name disappears without
  `:current` (its entries are removed at commit);
- inside a transaction, a name asserted in it is listed, one whose only
  claim it deleted is not, and counts are adjusted;
- `:as-of` and `:as-of-epoch` are refused.

CI's full SBCL suite is the gate.

## 6. Documentation

- `docs/general-index-design.md`: the distinct-prefix walk and
  `index-count`.
- `docs/vivace-graph-v3-doc.org`, the spacetime chapter's query section
  (the one whose examples call `claims-touching`, around line 5767),
  gains a "Vocabulary" subsection: the three functions, `:current`, the
  transaction rule, the as-of bound, and the relation index.
- `CHANGELOG.md` entry under Unreleased/Added.
- Docstrings carry the `:current` and transaction rules and the one trap:
  names are live membership.

## 7. Issues

#350 closes with the merge. A cl-llm issue records the consumer swap:
`memory/vocabulary.lisp` moves from its walk onto the three calls, with
its taxonomy tests as the acceptance test. #113 is unaffected; an as-of
listing would ride its epoch-stamped entries.

## 8. Out of scope

- Counting structures on the write path, and exact counts under an
  as-of snapshot.
- Canonicalisation of names; case-folding; a Prolog surface.
- `:as-of` on the listing (R6).
- Vocabulary for producers: `claims-by-producer` already exists and
  `claim-producer` is a single-slot index, so a producer listing is one
  `map-index-prefixes` call a tenant can make today.
