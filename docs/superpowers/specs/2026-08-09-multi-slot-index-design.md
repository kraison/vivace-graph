# Multi-slot indexes — design

**Date:** 2026-08-09
**Status:** design agreed in brainstorming; not planned, not built.
**Issue:** #107. **Programme:** unit M of the SpatioTemporal substrate programme
(kraison/cl-llm#15), phase 1. **Branch:** build on a new branch off `experiment`.

## 1. What this is

Extend the general ordered index from a single slot to a **tuple of slots**, with
leading-prefix lookup, and add a class-level **multi-slot unique constraint**.

`def-index` already declares itself the home for this work — `index.lisp:431`: *"is the
home for future composite / multi-slot indexes."* The v1 scope note recorded composite
as "deferred but designed-for: codec already polymorphic … the deferred part is
query-planner leading-prefix matching." That remains an accurate statement of the
residual work.

### 1.1 Why now

Two consumers land immediately after this, both keyed on tuples:

- **Endpoint identity** of the form `(namespace, external-key)`.
- **Record identity** of the form `(subject, object, relation, rule-version)`, which
  must *upsert* rather than duplicate across repeated regeneration at 10^5–10^6 scale.

A view can key on anything, which is why views carry the arbitrary-key load today — but
a view **cannot enforce**. Per the `:unique` design (#6), enforcement is a
commit-boundary check: `validate-unique-constraints` runs at `transactions.lisp:2968`,
inside the `with-transaction-manager-lock` region opened at `transactions.lisp:2956`.
Views are post-durability, which is why #7 exists and why a view can never enforce. A
multi-slot unique index is therefore the only route to an arbitrary composite key
**under the transaction system's protection**.

Retrofitting an identity scheme after the records exist in volume is a migration rather
than a change, which is why this is sequenced first.

## 2. What exists today

- **The key is already composite.** `slot-index` (`index.lisp:117`) backs each index
  with an ordered map keyed by `(canonical-value id)` under `reduce-comp-lessp`.
  Equality lookup is a prefix range over `[(value +null-key+) .. (value +max-key+)]`
  (`ix-lookup`, `index.lisp:154`).
- **List ordering already exists.** `less-than` (`utilities.lisp:248`) orders lists
  lexicographically, with explicit base cases making a shorter list sort **before** a
  longer one sharing its prefix. That is exactly what a prefix scan needs; no new
  ordering is required.
- **`:unique` already builds composite keys.** `%unique-key`
  (`unique-constraint.lisp:179`) returns `(origin-token k)` under `:origin` scope.
- **Two sentinel families, in different positions.** `+null-key+` / `+max-key+`
  (`globals.lisp:88`, `:91`) are 16-byte all-zero / all-255 arrays occupying the **id**
  position. `+min-sentinel+` (`:gmin`) and `+max-sentinel+` (`:gmax`)
  (`globals.lisp:228-229`) are the generic ordering sentinels for **value** positions.
  Conflating the two is easy and wrong.
- **NULL is exempt.** `%index-key` (`index.lisp:136`) returns NIL for a null value and
  `%indexable-value-p` (`index.lisp:143`) excludes NIL — so NIL already means *not
  indexable* and cannot be reused as a stored marker.
- **`:unique` is a MOP slot option** (`node-class.lisp:24`), inherited from the
  declaring direct slot (`node-class.lisp:233-239`). Nothing about that extends to a
  tuple.

## 3. The four decisions

### 3.1 Key encoding — flat

`(v1 … vn id)`, not `((v1 … vn) id)`.

Flat makes today's single-slot `(value id)` **literally the n=1 case**, so every
existing on-disk index stays valid and nothing rebuilds. Nested would make single-slot
`((value) id)` — a codec change forcing a regenerate across every index in every graph,
bought for nothing.

Flat also builds prefix ranges directly. For an index on `(a b c)`, a prefix lookup on
`(a b)` scans `(a b)` → `(a b +max-sentinel+ +max-key+)`, valid because a shorter list
sorts before any longer list sharing its prefix.

Internally every index normalises its slot list to a list of length ≥ 1, so there is one
code path rather than a single-slot special case.

**What flat costs, which the v1 scope note understated.** "The codec is already
polymorphic" is true of `less-than` on lists, but **three things are hardcoded to a
two-element key** and must be generalised:

- `reduce-comp-lessp` (`views.lisp:355`) — `(first …)` via `less-than`, `(second …)` via
  `key-vector<`;
- `reduce-equal` (`views.lisp:350`) — `(first …)` via `equal`, `(second …)` via
  `equalp`;
- the head / tail sentinel keys `(list +min-sentinel+ +null-key+)` and
  `(list +max-sentinel+ +max-key+)`, which are arity-dependent and appear on the
  memory-graph path at `memory-graph.lisp:1263-1267`.

M therefore adds `%index-comp-lessp` and `%index-equal`: compare components `0 … n-2`
with `less-than` / `equal`, and the final component (the id) with `key-vector<` /
`equalp`. **At n = 2 these are order-identical to `reduce-comp-lessp` /
`reduce-equal`**, which is what preserves the no-rebuild property — an existing
single-slot index reopens under the new comparator and orders exactly as before.

Views keep `reduce-comp-lessp` untouched; only index and unique skip-lists move to the
generalised pair. The comparator is **passed at open** (`index.lisp:134`) and held in
the skip-list struct at runtime, never persisted, so swapping it involves no on-disk
change.

Head / tail keys become arity-derived, which means `make-secondary-skip-list`
(`index.lisp:124`) and the memory-graph `make-view-skip-list` method must both learn the
index's arity — today neither takes it.

### 3.2 A third sentinel for a null component

`+null-component+` (`:gnull`), with `less-than` methods placing it **above
`+min-sentinel+` and below every real value**.

That ordering is what makes a null-bearing tuple appear in a prefix scan of its
populated leading components. It must be distinct from:

- **`+min-sentinel+`**, which stays a pure range bound — otherwise "exact match where
  component 2 is null" is indistinguishable from "lower bound of a prefix range at
  position 2";
- **`NIL`**, which `%index-key` already uses to mean *not indexable*.

### 3.3 NULL semantics — index stores, unique exempts

The two mechanisms want **opposite** answers, and each follows its own logic:

- **Ordinary index: store `+null-component+`.** A tuple with a null component is
  indexed, so it stays findable by prefix on the components it does have.
- **Unique constraint: exempt.** Any tuple containing a NULL is exempt from the
  constraint, matching SQL's "unknown never equals unknown" and today's single-slot
  rule.

**The forcing case.** The claim record this work exists to serve has `object`
**optional** — unary claims exist as normal operation, not as a defect. So identity
tuples routinely carry a null component. Exempting them from the *index* would make a
unary claim invisible to prefix lookup on its populated components, which is the
retrieval layer's hot path. Erroring would make unary claims unwritable. Storing a
marker in the index while exempting them from uniqueness is the only combination that
serves both.

Consequence, stated so it is not discovered later: two unary claims sharing subject,
relation and rule-version **do not collide** on a unique tuple. That is deliberate — a
null object means "there is no object", and SQL semantics decline to equate two
unknowns.

### 3.4 Declaration surface — a parallel `def-unique`

```lisp
(def-index  claim (subject-ns subject-key relation rule-version) :my-graph)
(def-unique claim (subject-ns subject-key relation rule-version) :my-graph)
```

A list in the slot position; a bare symbol keeps working verbatim, normalised to a
1-list at macroexpansion. `:canonicalize` generalises to a positional list, a NIL entry
meaning identity; a single function stays valid for the single-slot form.

`def-unique` is a parallel macro rather than `def-index … :unique t`. The deciding
argument is §3.3: the two have opposite null rules, so a flag would silently switch
semantics a reader would reasonably assume were unchanged. A separate name makes the
difference visible at the call site. `def-unique` additionally carries `:scope`, since
uniqueness has an origin axis (`:local` / `:hub` / `:origin`) that indexes do not.

## 4. Query API

```lisp
(index-lookup g 'claim '(subject-ns subject-key relation rule-version)
              (list ns key) :prefix t)
```

- The slot argument accepts a symbol or a list; the value argument likewise.
- A value list **shorter than the index arity signals an error unless `:prefix t`**.
- `index-range` / `map-index` take tuple `:start` / `:end`.
- No fourth query entry point.

The explicit `:prefix` is deliberate. The length alone does carry the information, but a
wrong-length list would then be indistinguishable from an intended prefix and would
silently return a superset. Silent-wrong-answer is this project's documented dominant
defect class; a superset returned without complaint is exactly the failure shape to
design out.

## 5. Registry and maintenance surface

**Three registries key on `(owner . slot-name)`** and become `(owner . slot-list)`, with
symbols normalised on the way in: `*schema-index-metadata*` (`index.lisp:64`), the
graph's `secondary-indexes`, and the descriptor dedup in
`class-secondary-index-descriptors` (`index.lisp:100`). Every lookup helper that builds
one of those conses follows.

**Maintenance runs from three call sites, not one.**
`apply-tx-writes-to-secondary-indexes` is invoked at `transactions.lisp:1714` *and* at
`peer-streaming.lisp:781` and `:818` — the peer pull-apply paths. A multi-slot index
that only updates the first will silently fail to index pulled nodes. The same is true
of `apply-tx-writes-to-unique-indexes` (`transactions.lisp:1713`,
`peer-streaming.lisp:780`, `:817`).

## 6. Migration — one seam, and it is a reader shim

`save-secondary-index-roots` (`index.lisp:299`) persists index roots to a sidecar keyed
by `(owner . slot)`. `restore-secondary-index-roots` (`index.lisp:333`) must **accept a
bare symbol and normalise it to a 1-list**, so existing sidecars restore untouched.

That is the only *on-disk* backward-compatibility point, and it is a reader shim
rather than a format bump. **No graph rebuilds. No storage-version change.**

That property rests entirely on the generalised comparator being order-identical at
n = 2 (§3.1), so it is a hard gate rather than an aspiration: the plan must prove that
an existing single-slot index reopens under `%index-comp-lessp` and returns the same
results in the same order **before** anything else builds on it. If the implementation
finds itself needing a rebuild or a version bump, the flat-key decision has been
violated somewhere and should be revisited rather than worked around.

## 7. Out of scope

- **Expression / computed-key indexes.** Views keep the derived-key cases; this covers
  tuples of raw slot values.
- **Automatic index selection in the Prolog compiler** — scan-and-filter rewritten to an
  index range scan. Separately deferred (needs pattern detection plus a cost model).
- **Exposing any of this to Prolog.** #102 is its own issue; this work builds the API it
  would wrap.
- **The #51 hub arbiter** for `:hub`-scoped uniqueness. `def-unique` carries `:scope`
  because uniqueness has that axis, but multi-slot introduces no new behaviour there.

## 8. Failure modes

| Failure | Handling |
|---|---|
| Value list shorter than arity, no `:prefix t` | Signals. Never a silent superset. |
| Tuple with a null component, ordinary index | Indexed under `+null-component+`; findable by prefix. |
| Tuple with a null component, unique | Exempt; never collides. |
| Two unary claims, same populated components | Both permitted — see §3.3. |
| Existing single-slot sidecar restored | Symbol normalised to a 1-list; no rebuild. |
| Pulled node from a peer | Indexed — maintenance covers all three apply sites (§5). |
| `:gmin` used as a stored value | Cannot happen; `+null-component+` is a distinct sentinel (§3.2). |

## 9. Testing

A case per decision, plus three traps that are the specific ways this can go silently
wrong:

- A tuple whose null component **still resolves** under a prefix scan of its populated
  leading components.
- Two unary claims that must **not** collide on a unique tuple.
- A short value list without `:prefix t` that **signals** rather than returning a
  superset.

Plus, per the discipline the general ordered index was built under: both index backends
(skip-list, B+ tree) and the memory-graph backend behave identically; a
concurrent-writer test for the unique path, mirroring the existing 8-thread / 1-commit /
7-violation case; and the peer pull-apply path exercised through the two-process
harness, which is a shell script outside the FiveAM matrix and must be run deliberately.

Green on SBCL. ECL per the standing demoted-to-periodic policy — say explicitly when it
was skipped.

## 10. Open items

- **Per-component canonicalizer ordering** is positional, so a canonicalized index
  orders by the canonical form in that component. Inherited from the single-slot caveat;
  worth restating in the manual rather than resolving.
- **Arity is not currently recorded in the sidecar.** §6 normalises a symbol to a 1-list
  on read, which covers every existing sidecar, but the implementation should confirm no
  other reader infers arity from key length at restore time.
