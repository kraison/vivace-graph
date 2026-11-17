# Touched-at index: a change-feed cursor costs what changed

Unit 7 of the Prolog Engine 5.0 scope
(`2026-11-09-prolog-engine-5.0-scope.md`); GH #392, from #391.

## Intent

A tenant's change feed is one goal list per namespace:

```lisp
(claim ?c belief "ns" ?k ?rel ?ons ?okey)
(claim-touched-at ?c ?t) (instant> ?t "<cursor>")
```

Today it answers correctly and costs the namespace's whole history:
`claim/7` enumerates every key's claims, then the pair filters. After
this unit the same text costs the claims touched after the cursor, read
in touched order from an ordered index, and the `claim/7` goal filters
each one. Nothing a reader wrote changes.

## Goals

- An ordered index per claim family on the instant `claim-touched-at/2`
  answers, kept by the engine's own index maintenance on every write
  that moves it.
- A Lisp reader and a Prolog generator over it, in ascending touched
  order.
- The feed's existing text takes the index, through a compile-time
  rewrite beside the one `claim-after/9` already has.

## Non-goals

- `instant>=`, `instant<`, a cursor inside `or`/`not`/`if`, or a goal
  between the three: paid for by the comparison, as for `claim-after/9`.
- A recorded-at or retracted-at index of their own: touched-at covers
  the feed; the other two stay readers.
- Cost-based reordering of anything else. This is one rule, one shape.
- A page cursor inside the generator (`:limit`): the runner's own limit
  and the cursor move the feed.

## Decisions

### D1. The index is a general ordered index on the extent slot

`def-claim-classes` declares, on the PARENT so both arities share it:

```lisp
(graph-db:def-index ,parent (transaction-extent-sexp) ,graph-name
    :name claim-touched-at :canonicalize extent-sexp-touched-key)
```

`extent-sexp-touched-key (sexp)` (spacetime/claim.lisp, beside
`extent-sexp-start-key`) keys a stored transaction extent by its
touched instant: the end's latest edge when the end is bounded, else the
start's earliest edge, as `(day sec nsec)` through `%timestamp-key`;
`:unbounded` when neither edge is a timestamp. A NIL slot (a claim
predating the axis, GH #148) is not indexed: the arity-1 all-null rule.
The on-disk key is `((day sec nsec) . id)`, the composite-key idiom, so
one instant shared by N claims is N rows and a remove is node-specific.

Maintenance is the index's own: creation is a `tx-create`,
`retract-claim`'s save of the closed extent and `register-node`'s
re-open of a retracted claim are `tx-update`s, and
`apply-tx-write-to-secondary-indexes` releases the old key and claims
the new one. No write path changes. A store opened with the new
declaration builds the index once, the general index's idempotent
two-phase; a `transaction-extent-sexp` the constraint already forbids
to change is exactly the slot a secondary index is cheapest on.

### D2. The Lisp surface

In spacetime/claim-query.lisp:

- `claim-touched-at (claim)` → a `local-time:timestamp` or NIL: the
  instant the transaction record last changed, by D1's rule; NIL for a
  pre-axis claim, a reaped claim, or an extent with no timestamp edge.
  `claim-touched-at/2` answers this value as a string, so the functor
  and the key share one definition.
- `claims-touched-after (graph claim-class after &key limit offset)` →
  the family's claims, both arities, whose touched-at is strictly after
  AFTER (a timestamp or an RFC 3339 string), ascending by touched
  instant, retracted claims included, an open transaction's own writes
  included at their keys (`map-index`, GH #408); `:unbounded` rows and
  claims whose `claim-touched-at` is NIL dropped. `:limit`/`:offset`
  page as `claims-by-producer` does, second value T past the cut. The
  scan starts at AFTER's key inclusive and drops the equal instant by
  value, so a cursor read back from the feed never repeats its row.
  `map-index` runs `:start` through the canonicalizer, which reads a
  stored sexp, so the start bound is a synthetic open extent at AFTER
  (`extent->sexp` of `%open-transaction-extent`), whose touched key is
  AFTER's; the canonicalizer's domain stays "a stored extent sexp".
  An unregistered CLAIM-CLASS signals `unknown-claim-family`; a graph
  whose schema does not declare the family signals the index's own
  `query-precondition-error`, as `claims-by-producer` does.

Exported from `graph-db.spacetime` with the other readers.

### D3. The functor: `claim-touched-after/4`

`(claim-touched-after ?c ?t ?after ?family)` in rules/facts.lisp:

- ?c a claim node: a filter. Binds ?t to its touched instant string
  and succeeds when that instant is after ?after (`%instant-arg`; an
  unbound or NIL ?after fails, a malformed one signals as the instant
  comparisons do).
- ?c unbound, ?after bound: a generator over `claims-touched-after`
  from ?after in every store in scope (`*claim-scope*`, GH #332). With
  ?family bound (a parent class name, `%family-or-ill-typed`) it reads
  that family under `%scope-lookup`'s store rule: the own store
  signals when its schema lacks the family, a foreign store contributes
  nothing. With ?family unbound it reads every registered family and
  skips, on every store, a family that store does not declare: the
  reader named no family, so no family can be ill-typed for it, and an
  image hosting two tenants' families under two graph names must not
  refuse one tenant's feed over the other's schema. The per-store,
  per-family lists are merged into **one ascending sequence by touched
  instant**, ties in scope order then family order, because a feed
  that advances its cursor to the largest ?t of a page must never skip
  an older-but-after-cursor row of a second store. Candidates pass
  `%exclude-producers`, and this run's own derivation (`%derived-all`)
  touched after ?after follows last, so a fixpoint round reads the same
  facts whichever goal generates (GH #333). Each solution binds ?c and
  ?t.
- ?c unbound, ?after unbound: no solutions, which is what the unfused
  pair answers. Never a cost-unbounded refusal: the index is the route.

Writable by hand from text and from Lisp, as `claim-after/9` is;
refused as a rule premise by `compile-rule` with the sibling message
("write claim-touched-at and the comparison, the compiler fuses them");
standing class `:withheld`. `explain-goals` reports the hand-written
goal, as it does `claim-after/9` (the explainers do not run the
rewriters): "that claim" with ?c bound, else "touched-at index of
FAMILY from the cursor" or "touched-at index of every family from the
cursor".

`compile-rule` also refuses a rule body in which a `claim-touched-at`
goal's claim variable is bound by no `claim/7` or `claim-absent/7`
premise of the body ("claim-touched-at reads a premise's claim: bind ?c
with claim/7 first"). Today such a pair is a dead filter that yields
nothing; after D4 it would be shape 2, a generator the reads and strata
analysis never counted. The rules lane orders generators first, so
this is exactly the condition under which shape 2 would fuse there.

### D4. The rewrite

`%fuse-touched-cursor`, a second entry on `*clause-body-rewriters*`
beside `%fuse-key-bounds`, over a goal list's top level. The pair is
`(claim-touched-at ?c ?t)` followed at once by `(instant> ?t B)`, with
?c and ?t named variables and B a string literal or a variable. Two
shapes fuse:

1. **Claim first.** The goal right before the pair is a `claim/7` goal
   whose first argument is ?c, ?c occurs in no earlier top-level goal
   at any depth, and B is not a variable of that `claim/7` goal. The
   three goals become
   `(claim-touched-after ?c ?t B FAMILY) (claim ...) (instant> ?t B)`,
   FAMILY the `claim/7` goal's second argument as written. The
   generator runs first, `claim/7` takes its bound-claim route and
   filters, the comparison stays.
2. **Pair first.** ?c occurs in no earlier top-level goal: the pair
   becomes `(claim-touched-after ?c ?t B nil) (instant> ?t B)`, every
   family.

Anything else is left alone: ?c bound by an earlier goal (the filter
arm would equal the pair), a goal between the three, the pair inside
`or`, `not`, `if`, `once` or `forall`, `instant>=`. The reorder in
shape 1 is sound because ?c is fresh: the conjunction's solution set is
the same, and D3's exclusion and derivation rules keep it so inside a
fixpoint round. Solution order changes to ascending touched instant,
the order a feed wants; docs/rules.md "Solution order" says so, and
`*clause-body-rewriters*`' docstring, which promises the same order
today, is amended to "the same solutions; the order only where the
rewriter's own documentation says so".

A lifted literal (unit 6) arrives as a variable bound by
`%bind-literals`, the first goal, so a cached shape fuses the same way.
A `select` compiled before `graph-db/rules` loaded is not rewritten
until recompiled, as for `claim-after/9`.

### D5. Measurement

`bench-change-feed-cursor` (unit 1b) as it stands: N claims, a cursor
past 95 % of them, the claim-first text through `run-guarded-prolog`
with `:limit 1000`. Before rows at bc44486 on odm, third of three runs,
on #392 before the code changes; after rows at the unit's head, same
host, both tables on the issue, reports under `tests/perf/results/`.

### D6. Docs

docs/rules.md: the functor table row, a "The touched-at route" passage
in "Routes, and what refuses", the change-feed paragraph's last sentence
replaced, "Solution order" extended, the fusion rules beside
`claim-after/9`'s. docs/general-index-design.md §12: the paragraph moves
to §4 as built. CHANGELOG, docs/codebase-notes.md's claim bullet,
docs/perf-baselines.md's unit entry.

## Left to the builder

- Whether `claims-touched-after` collects through `index-range` or
  walks `map-index` with an early stop at `:limit`; the generator
  collects before it yields, as every `claim/7` route does (a cursor
  held across a continuation is not house style, index.lisp §350).
- The explain test's exact wording, matched to the strings above.
- Test fixtures: tests/spacetime for D1 and D2 (the index exists after
  `def-claim-classes`; a retraction and a re-open move the row; the
  pre-axis and `:unbounded` cases; an open transaction's own write),
  tests/rules for D3 and D4 (both shapes take the index, proven by
  `counting-lookups` (tests/rules/suite.lisp): zero `%scope-lookup`
  calls where the namespace route pays one per key; exclusion under
  `*claim-exclude-producers*`; two stores in scope in one ascending
  sequence; an own store lacking a family with ?family unbound; the
  unbound-cursor case; both rule-body refusals; the explain row; the
  guarded text, compiled on its second call).
