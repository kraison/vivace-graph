# Spatial index: every indexed geometry slot, and the slot a query tests

**Date:** 2026-11-01
**Branch:** `experiment`
**Status:** DESIGN, awaiting approval
**Issue:** kraison/vivace-graph#60 (CR-3.2 of the 3.0.0 spatial work; the
parent-scope asymmetry folded into it)
**Predecessor:** `docs/superpowers/specs/2026-07-24-spatial-index-scoping-design.md`
(§6 query API, §8 inert-slot warning, §12 peer note, §13 deferred)
**Release impact:** **MAJOR**. The default behaviour of an existing query
changes on one class of schema (see §4), and the spatial sidecar format is bumped.

---

## 1. Summary

Today a node is spatially indexed under **one** geometry: the first `:index t`
slot whose runtime value is a geometry, as chosen by `node-geometry`. A class
with a second indexed geometry slot has that slot silently inert. 3.0.0 shipped
a warning that reports this.

After this change a node is indexed under **every** `:index t` geometry slot
that holds a value, each in its own `(owner . slot)` index, and **every spatial
query names the slot it tests**. There is no union semantics in the query path:
a query against the centroid is a centroid query, a query against the outline
is an outline query. A class with one indexed geometry slot behaves exactly as
today. A query that names no slot on a class with several signals an error,
unless a transition switch resolves it to the first declared slot.

Two consequences ride along: a scope to a parent class now reaches an index a
subclass declares on its own extra slot (the asymmetry folded into #60), and
the 3.0.0 inert-slot warning and its audit function go away, because several
indexed geometry slots become the intended configuration.

## 2. Why

The consuming application has five node classes that each carry a centroid
point and an outline polygon, both marked `:index t`. The centroid is declared
first, so only it is indexed. Containment questions ("which region contains
this point?") must test the outline and today scan every node of the class.
Distance questions ("nearest place to this point", reported as "8 km NE of X")
must test the centroid. Overlap questions ("surveys whose outline crosses this
site") must test the outline and today match the centroid, missing a survey
whose outline crosses the site but whose centre lies outside it.

Why "any geometry matches" is wrong, and not merely imprecise: for a node whose
centroid lies inside its own outline, which is nearly every node, "any matches"
equals "outline matches". An intersects query returns the same result either
way, and the minimum distance is always the distance to the outline, which is
zero for any point inside it. Under union semantics a centroid query cannot be
written at all: "nearest place" becomes "nearest place outline", zero
kilometres for every point inside a town. Concave outlines, whose centroid lies
outside the shape, add false hits in the other direction. So the slot is named
by the caller, per query.

## 3. Goals, non-goals, anti-goals

**Goals**

- A node is present in the spatial index of every indexed geometry slot that
  holds a geometry, and absent from the index of every slot that does not.
  Create, update (including a slot going from geometry to nil, and the
  reverse), delete and rebuild all maintain this.
- Each spatial query tests exactly one named geometry per node, chosen by the
  caller. Candidate selection, the refine predicate, and the distance reported
  by `find-nodes-near` and the ordering of `find-nearest-k` all use that slot.
- A scope to a parent class, a list, or `:all` is subclass-inclusive on the
  index side as it already is on the type-filter side.
- A single-slot class, with or without a slot argument, behaves as in 4.1.0.
- Existing graphs re-derive their spatial indexes once on first open.

**Non-goals**

- Changing how distance is measured against a geometry. `find-nodes-near`
  measures to the geometry's representative point (its bbox centre) today, and
  continues to. For a centroid slot this is exact; for an outline it is the
  distance to the outline's bbox centre. Only *which* geometry is measured
  changes.
- Changing the replication filter's test (rep-point containment in the
  subscribed area). Only the set of geometries it considers changes (§7).
- The peer wire format. Nothing here touches TLV, framing, node-head layout or
  the handshake. The peer's local query semantics must mirror the hub's; see
  §11.
- A per-class declared "primary" geometry slot. Explicitly rejected for this
  release: a primary can be added later without breaking anyone, a default can
  never be taken away.

**Anti-goals** (would make this a failure even if it works)

- Any query path where a node matches because *some* geometry of it matches.
- An unnamed query on a multi-slot class silently picking a slot in the
  default configuration.
- A multi-slot class whose second slot is indexed on the write path but
  unreachable from a query (the 4.1.0 state in reverse).

## 4. Decisions locked (as decided with Kevin)

1. **Every `:index t` geometry slot is indexed**, unconditionally. There is no
   switch on the write path.
2. **Each query names the slot it tests**, via a `:slot` keyword on the four
   Lisp entry points and a new arity on each Prolog predicate (§6).
3. **An unnamed query on a multi-slot class is an error** in the default
   configuration (`query-precondition-error`, the same condition the required
   scope uses).
4. **A transition switch** relaxes rule 3 for the consuming application's port:
   `*spatial-unnamed-slot-policy*`, default `:error`; `:first-declared`
   resolves an unnamed query on a multi-slot class to the first declared
   indexed geometry slot in effective-slot order and signals one CL warning per
   class naming the slot it chose, so the port has a worklist. The switch
   covers query resolution only (the four entry points, the Prolog predicates,
   the default `node-geometry` method, spacetime registration). It is removed
   in the next major release after the port completes; the CHANGELOG says so.
5. **Subset replication ships a node when any of its geometries lies in the
   subscribed area.** Replication is about having the data on the device, not
   a semantic question, so union is right there and only there.
6. **Shipped as a major release** (5.0.0 is my call; the number is Kevin's at
   release time), with no on-disk node format change and a spatial sidecar
   format bump so existing graphs re-derive.
7. **The #60 comment and this document's public artefacts stay domain-neutral**:
   point versus outline, no application vocabulary.

## 5. Model

### 5.0 What a geometry slot is (my call)

The engine has no class-level notion of a geometry slot today:
`node-geometry-index-slots` returns every `:index t` slot, scalars included,
and the write path picks by runtime value. A per-class "multi-slot" predicate
needs a declaration-level answer, so:

**A geometry slot is an `:index t` slot whose declared `:type` admits
`graph-db:geometry`**: the declared type `T` is not `t`, and either
`(subtypep 'graph-db:geometry T)` holds (so `geometry` and `(or null
geometry)` qualify) or `T` is a symbol named `"GEOMETRY"` in some other
package. The symbol-name rule is what makes the predecessor's package concern
(its §2.1(b)) a non-issue: the symbol need not be `eq`, only spelled the same.
The documented declaration has always been `(slot :type geometry :index t)`,
and the examples and the consuming application write exactly that.

Consequence, recorded in the CHANGELOG: an `:index t` slot with **no** type
declaration that happens to hold geometry values was spatially indexed in
4.1.0 and is an ordinary secondary index now. Declare the type to keep it
spatial.

`node-geometry-index-slots` becomes this declared-type list, in effective-slot
order, with its existing cache and invalidation. The write path still tests
each declared geometry slot's value with `geometryp` (a nil slot is simply
absent from its index), so no slot is read that was not read before.

### 5.1 The plural protocol

A new generic function, exported:

```lisp
(node-geometries node) => list of (slot-name . geometry)
```

The default method on `node` enumerates `node-geometry-index-slots` of the
node's class in effective-slot order and collects each slot whose value passes
`geometryp`. A class carrying an application-supplied `node-geometry` method
(detected as today through `%node-geometry-method-owner-name`) yields a single
entry `((slot-or-nil . geometry))` built from that method's two values, so the
custom-method extension point and its `(method-owner . nil)` key are unchanged.
A two-value method that returns a declared `:index` slot's name is keyed
`(class . slot)` as today, so `:slot` with that name reaches it.

**An application `node-geometry` method is the authority for its class**, and
it yields one geometry per node. Such a node is therefore in exactly one
spatial index, whichever the method chose for it, whatever the class's slots
declare. A method may choose a different declared slot per node (the
consuming application's "outline when present, else centroid"), and each node
lands in that slot's index, as today. Consumers that wrote a `node-geometry`
method only to choose between two indexed slots (the 4.1.0 workaround for the
inert second slot) must delete it to get both slots indexed; the CHANGELOG's
re-check list says so. A method that deliberately indexes one geometry and
ignores another declared slot keeps working as written.

Applications may specialise `node-geometries` directly; a specialising method
is the authority for that class on both the write path and the refine path.

Every engine consumer of `node-geometry` moves to `node-geometries` or to the
slot-resolved accessor below. After this change the engine calls
`node-geometry` only (a) to detect and honour an application-supplied method
and (b) from `node-geometry`'s own default method. The list of consumers, with
what each becomes, is §8.

`node-geometry` stays exported as the single-geometry convenience and extension
point. Its default method returns the geometry of the class's **unnamed
resolution** (§5.3): the one slot on a single-slot class, the policy's answer
on a multi-slot class. It therefore signals on a multi-slot class under
`:error`, exactly as a query does.

The slot-resolved accessor is a plain function, exported:

```lisp
(node-slot-geometry node slot-name) => geometry or nil
```

It returns the geometry under `slot-name` in `(node-geometries node)`, so it
honours a specialised `node-geometries`. A nil `slot-name` names the
custom-method geometry.

### 5.2 Index membership

For a live node N, N's id is present in index `(owner . slot)` for exactly the
pairs `(slot . geometry)` in `(node-geometries N)`, with `owner` resolved as
today by `%node-spatial-owner-name`. The write path:

- **create**: insert under every pair;
- **update**: unindex every pair of the old node, index every pair of the new
  one (a slot that went nil leaves its index; a slot that gained a geometry
  joins one; a slot whose geometry changed is removed and re-inserted, since
  its cell cover may differ);
- **delete / mark-deleted / purge**: unindex every pair;
- **rebuild, regenerate, memory-graph rebuild**: insert under every pair, with
  `regenerate-spatial-index`'s per-slot filter kept.

The transaction overlay's own-node candidates (`%overlay-spatial-candidates`)
match each of an own node's keys against the scope's key set, not just one.

### 5.3 Scope resolution

A query's scope resolves to a set of `(owner . slot)` keys plus a type filter,
as today. What changes is how a class contributes keys.

**With `:slot S`:**

- a named class C contributes `(owner . S)` for C and for every subclass of C
  that indexes a geometry slot named S (owner resolved per declaring class as
  today). A subclass that does not index S contributes nothing and is not an
  error. If neither C nor any subclass indexes S, signal
  `query-precondition-error`: the caller named a slot that does not exist in
  that scope;
- a class with an application-supplied `node-geometry` method contributes
  `(owner . S)` if S is one of its declared geometry slots (a two-value
  method may have keyed nodes there) and otherwise nothing; if it is the
  named class and has no such slot (a one-value method, or S unknown), it
  signals the same error;
- `:all` contributes every `(owner . S)` key the type registry's classes
  declare, whether or not its index exists yet; an index that does not exist
  contributes nothing and the result is empty, not an error, as today;
- a list is the union of its members, each checked as above.

**Without `:slot`** ("unnamed"):

- each class in the scope (the named classes and, for `:all`, every class in
  the graph's type registry that is spatially indexed by declaration or by
  method) is resolved: a class with one geometry slot contributes that key; a
  class with an application-supplied `node-geometry` method contributes its
  nil key **and** every declared geometry-slot key, as `class-spatial-index-keys`
  does today, and is never multi-slot, since each of its nodes has one
  geometry; otherwise the class is **multi-slot** and the policy applies:
  `:error` signals `query-precondition-error` naming the class and its
  geometry slots; `:first-declared` picks the first in effective-slot order
  and warns once per class. For `:all` under `:error`, one multi-slot class
  anywhere in the type registry makes the unnamed query signal; that is the
  rule applied to the classes `:all` names, not an exception to it;
- subclass inclusion applies here too: a parent scope reaches a subclass's own
  single indexed slot even when its name differs from the parent's. A subclass
  that adds a second indexed geometry slot is multi-slot and is resolved by the
  policy like any other class in the scope.

"Multi-slot" is a property of the class: more than one `:index t` geometry slot
in its effective slots, regardless of which hold values on a given node. Under
`:first-declared` a node whose first declared slot is nil and whose second is
bound was found by an unnamed query in 4.1.0 and is not found now. The
CHANGELOG states this; the consuming application's classes always bind the
first slot.

The resolved key set is also used for `find-nearest-k`'s finest-precision
seeding, unchanged.

### 5.4 Refine

A candidate reaches the refine predicate from one or more of the scope's
indexes. The geometry it is tested against is the value, from
`(node-geometries node)`, of the first pair whose `(owner . slot)` key is in
the scope's resolved key set. With `:slot S` that is the node's S geometry.
Without `:slot` it is the class's unnamed resolution, which is a single key per
class by §5.3. The same geometry is what `find-nodes-near` measures and what
`find-nearest-k` orders by. A candidate with no geometry under any scope key
(possible for an own node in the transaction overlay, or a stale entry) is
dropped, as a nil `node-geometry` drops it today.

The whole-area refine semantics are unchanged: exact for points, exact for
extended geometries when `graph-db/geos` is loaded, rep-point or bbox
otherwise, per the existing docstrings.

## 6. API

### 6.1 Lisp

```lisp
(find-nodes-within       scope area           &key slot (graph *graph*))
(find-nodes-intersecting scope area           &key slot (graph *graph*))
(find-nodes-near         scope lat lon radius &key slot (graph *graph*))
(find-nearest-k          scope lat lon k      &key slot (graph *graph*)
                                                   (max-radius 2.5d4))
```

`slot` is a slot-name symbol, or absent. Results are as today: a list of nodes,
or `(node . metres)` pairs nearest first.

New exports: `node-geometries`, `node-slot-geometry`,
`*spatial-unnamed-slot-policy*`.

Removed exports (they describe a state that no longer exists):
`audit-spatial-slots`, `node-geometry-slots-with-values`,
`*node-geometry-multi-sample-limit*` and its counts table, and
`%maybe-warn-inert-geometry-slots`. The CHANGELOG names them so a schema test
suite that calls `audit-spatial-slots` learns why it stopped compiling.

### 6.2 Prolog

Each predicate gains a sibling of one more arity with the slot in third
position, after the scope. The existing arities stay and mean "unnamed":

```
find-within/3     (?node ?scope ?area)            find-within/4     (?node ?scope ?slot ?area)
find-intersects/3 (?node ?scope ?area)            find-intersects/4 (?node ?scope ?slot ?area)
find-near/5       (?node ?scope ?lat ?lon ?radius) find-near/6      (?node ?scope ?slot ?lat ?lon ?radius)
find-nearest/5    (?node ?scope ?lat ?lon ?k)      find-nearest/6   (?node ?scope ?slot ?lat ?lon ?k)
```

The slot argument must be bound at goal entry; an unbound slot signals the
same error the Lisp entry points do for a bad slot. `geo-distance/5`,
`geo-near/5` and `geo-within/3` compare plain values and are unchanged.

### 6.3 Spacetime registration

`register-geometry` queries the registry with `find-nodes-intersecting` and
then reads each region's geometry; `register-node` reads the source node's
geometry through its source contract's `:registration` facet. Both move to
the slot-resolved path:

- `register-geometry` gains `:slot`, passed to the intersects query and used
  through `node-slot-geometry` for each region's overlap fraction;
- the `:registration` facet gains two **optional** keys (my call: optional,
  so every existing `def-source` form stays valid): `:registry-slot`, the
  slot `register-node` passes to `register-geometry`, and `:geometry-fn`, a
  function of the source node returning the geometry to register. A source
  whose geometry choice is per record ("the outline when there is one, else
  the centroid") expresses it there, in the style of the facet's existing
  `:precision-fn`. Absent, each falls back to the unnamed resolution, hence
  to the policy.

Single-slot registries and sources behave as today. The spacetime example and
manual chapter say where the two keys go.

### 6.4 Errors and warnings

All signals are `query-precondition-error` with a reason that names the class,
the slot asked for when there was one, and the class's indexed geometry slots.
REST already maps that condition to HTTP 400. The `:first-declared` warning is
a CL `warn` once per class per image, cleared with the class's other spatial
memos on redefinition, matching the 3.0.0 warning it replaces.

## 7. Replication filter

`make-spatial-replication-filter` accepts a node when `(node-geometries node)`
is empty, or when the representative point of **any** of its geometries lies in
the area. It takes no slot. The rep-point test itself is unchanged and
remains a known coarseness: an outline that overlaps the area with its bbox
centre outside it does not ship. That predates this work and is out of scope;
it is noted in the manual's subset-replication section rather than fixed here.

## 8. Consumers of `node-geometry`, and what each becomes

From the engine read of 2026-11-01 (file:line as of `experiment` 7975e90):

| Consumer | Today | After |
|---|---|---|
| `%spatial-index-node` (transactions.lisp:1531) | one `(geom slot)`, runs the sampler | loop over `node-geometries`; sampler gone |
| `%spatial-unindex-node` (:1584) | one pair | loop over `node-geometries` of the old node |
| `%overlay-spatial-candidates` (spatial-query.lisp:70) | one key per own node | every key of an own node against the scope keys |
| refine in `find-nodes-within` / `-intersecting` / `-near` (:157, :173, :193) | `node-geometry` | scope-key-resolved geometry (§5.4) |
| `find-nearest-k` (:238) | via `find-nodes-near` | passes `:slot` through |
| `make-spatial-replication-filter` (:302) | first geometry's rep point | any geometry's rep point (§7) |
| `rebuild-spatial-indexes` (:376), `regenerate-spatial-index` (:440) | one pair | every pair; regenerate keeps its slot filter |
| `audit-spatial-slots` (:464) | the inert-slot audit | removed |
| memory-graph rebuilds (memory-graph.lisp:826, :948) | one pair | every pair |
| `%custom-node-geometry-classes` (:777) | detects custom methods | unchanged |
| spacetime `register-geometry` (spacetime/register.lisp:136), `register-node` (:411) | `node-geometry` | §6.3 |
| `node-geometry-index-slots` (transactions.lisp:1259) | every `:index t` slot | declared geometry slots only (§5.0) |
| `class-spatial-index-keys` (spatial-registry.lisp:15) | the class's own keys | subclass-inclusive (§5.3); its memo in `%spatial-scope-entry` already clears on type definition, which covers a subclass defined later |
| GUI index inventory (gui/api.lisp:357) | one row per registry key | unchanged shape, more rows |

## 9. Persistence and migration

- **Spatial sidecar**: `+spatial-index-format+` goes 5 to 6. The reader
  accepts v6 only as complete; anything older takes the existing
  rebuild-on-open path, so a 4.1.0 graph re-derives every spatial index on
  first open and a multi-slot node ends up in two indexes. Record layout is
  unchanged; the bump exists to force the re-derivation.
- **Memory graph**: the image's per-key spatial records are restored as stored
  for the current version. An image written before this change must rebuild
  its spatial indexes from nodes on load, the same way the v5 image did; how
  that is keyed (image version or the sidecar format recorded in the image) is
  the builder's.
- **No node format change.** Geometry rides as ordinary slot data.
- **Crash recovery** already rebuilds spatial indexes after replay; nothing
  new.

## 10. Test plan

Mutation-tested (each new test is shown failing against the behaviour it
protects before the change lands; see the house rule). Existing spatial suites
(`tests/spatial-*.lisp`, `tests/graph-spatial-tests.lisp`,
`tests/concurrency/spatial-tests.lisp`, `tests/geos/query-tests.lisp`,
`tests/spacetime/`) stay green; a test that encoded "only the first slot is
indexed" is rewritten to the new contract, not deleted.

New tests, as behaviours:

1. A two-slot class: a node is present in both indexes after create; present
   in one after an update nils the other slot; in neither after delete; both
   after a rebuild and after a reopen of a 4.1.0-format sidecar.
2. Centroid-inside-outline fixture: `find-nodes-within` with `:slot
   'centroid` and with `:slot 'outline` return different sets where they
   should (a node whose outline crosses the area but whose centroid is
   outside, and a concave outline whose centroid is outside the shape).
3. `find-nodes-near` with `:slot 'centroid` reports the centroid distance;
   with `:slot 'outline`, the outline rep-point distance; `find-nearest-k`
   orders by the named slot.
4. Unnamed query on the two-slot class: signals under `:error`; under
   `:first-declared` returns the first-slot answer and warns once per class.
5. Single-slot class: results byte-identical with and without `:slot`, and
   under both policies, to 4.1.0's (the existing suite is that oracle).
6. Parent scope: `base` with `loc`, `special (base)` adding `precise-loc`.
   `(find-nodes-within 'base area :slot 'precise-loc)` finds the `special`
   node; `(find-nodes-within 'base area)` signals under `:error` because
   `special` is multi-slot; `:all :slot 'precise-loc` finds it; a bad slot
   name signals.
7. Custom `node-geometry` class: unchanged results for a one-value and a
   two-value method (the existing `scope-two-geoms-reporting` shape); a
   per-node slot choice lands each node in its chosen index and `:slot` on
   either declared slot reaches the nodes keyed there; `:slot` naming a slot
   the class does not declare signals; `:all :slot 'x` skips it.
7a. Geometry-slot detection: a class with one geometry slot and two indexed
    scalars (the 3.0.0 `scope-mixed-index` fixture) is single-slot; a slot
    typed `(or null geometry)` and one typed by a same-named symbol in
    another package both count; an untyped `:index t` slot holding a
    geometry is not spatially indexed and the CHANGELOG line is the record.
8. Prolog: each new arity against the same fixtures as its Lisp twin, with a
   literal list scope, as the 3.0.0 tests did.
9. Replication filter: a node whose outline rep point is in the area and
   centroid is not ships; a node with neither does not.
10. Transaction overlay: an own node written in the open transaction with
    both slots bound is a candidate under either `:slot`.
11. Spacetime: a two-slot region class registers by the named slot.
12. Concurrency: the existing concurrent spatial test extended to a two-slot
    class, so the per-slot insert/unindex pairs are exercised under contention.

## 11. Peer note (domain-neutral, for the device's SQLite peer)

The wire does not change. What the peer must mirror is that a hub query now
names the geometry slot it tests, and that a node may be in several spatial
indexes. The peer's point-only R-tree agreed with the hub only because the hub
indexed each node once; after this change the hub answers outline queries the
device cannot. The predecessor spec §12 already names the fix: carry the
geometry slot *names* (not just an ordering) in the type table the peer already
receives, and index non-point geometries by bbox. Nothing in this repository
blocks on it; it is recorded on #60 for the peer's owners.

## 12. Documentation obligations

- `CHANGELOG.md` `[Unreleased]`: Changed (every slot indexed, `:slot`, the
  error, the policy switch and its planned removal, the `:first-declared`
  first-slot-nil difference), Removed (the four exports), Added (the two
  functions, the four Prolog arities, the two facet keys), with a numbered
  re-check list for a consumer's port: every spatial call site names its
  slot; every `node-geometry` method written to choose between indexed slots
  is deleted; every registration source says which geometry it registers.
- `docs/vivace-graph-v3-doc.org` Chapter 13 (making a node type spatial,
  querying, caveats, subset replication) and the API reference's Spatial
  section; the spacetime registration section for `:slot`.
- `examples/example.lisp` shows a two-slot class and one query per slot;
  `tests/load-examples.sh` keeps it loading.
- `docs/codebase-notes.md` if it names `node-geometry` as the spatial idiom.
- The predecessor spec's §8 and §13 get a one-line "superseded by" pointer.
- #60: the decisions in §4, the peer note, and the closing comment naming the
  landing SHA.

## 13. Left to the builder

- Internal shape of the resolved scope (whether keys carry their slot name to
  the refine as a set or as a per-class table), as long as §5.4 holds.
- How the per-class "multi-slot" property and the unnamed resolution are
  memoised, given the existing `%spatial-scope-entry` memo and its clearing.
- Whether `node-slot-geometry` reads the slot directly on the default path
  or always goes through `node-geometries`; the contract is only that a
  specialised `node-geometries` is honoured.
- The memory-graph migration keying (§9).
- Test file placement and fixture classes; the 3.0.0 suites' fixtures are the
  starting point.
- Facts the builder check confirmed from the code, kept here so the plan does
  not rediscover them: the sidecar reader's adoption list at `graph.lisp:141`
  must gain 5 so a rebuild frees v5 roots; subclass enumeration is MOP-based
  because the `%spatial-scope-entry` memo is keyed by class name without a
  graph; REST exposes no spatial query, so no parameter is added there;
  `def-global-prolog-functor` keys on name/arity, so the new arities coexist
  with the old.
