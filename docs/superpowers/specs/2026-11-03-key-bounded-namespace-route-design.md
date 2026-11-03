# A key bound after `claim/7`'s namespace route starts the route at the bound

**Issue:** kraison/vivace-graph#451. **Date:** 2026-11-03. **Status:** approved
design, option 1 of the issue ("push the bound down"), chosen by Kevin.

## Intent

Paging a namespace by key is the paging the docs recommend
(`docs/guarded-query.md` "Paging", `docs/rules.md` "Solution order"):

```
(select (claim ?c belief "ns" ?k "rel" ?ons ?okey)
        (> ?k "last-key-seen")
        … :limit 500)
```

Today `%namespace-route` (rules/facts.lisp) walks every key in the
vocabulary list, does one index lookup per key, and unifies each claim
into the continuation, where the `>` goal rejects it. A page's cost
follows the depth of its bound, not its `:limit`: on a 30,000-claim
relation the last page costs ten times the first and 6–12% of deep
pages blow a 5 s guarded timeout (the issue's table). Reading one
relation end to end is minutes of guarded-query time.

After this change the route skips a key the `>` would reject before it
reads that key's claims. A key-bounded page then costs one string
comparison per earlier key plus its own keys' claims, at any depth.
Existing goal text gets the speed-up with no edit.

## Goals

- A key-bounded page on a namespace route costs about the same at any
  depth.
- Goal text written as the docs recommend today is what gets faster.
  No caller has to adopt a new form.
- The rewrite can never change an answer: every claim it skips is one
  the `>` goal would have rejected.
- The engine's Prolog compiler stays ignorant of claims.

## Non-goals

- A snapshot cursor. A key bound remains no cursor, as "Paging" says.
- Bounds other than a strict lower bound on a string key. `>=`, `<`,
  `<=`, numeric bounds and bounds on both ends stay as they are:
  correct and paid for by the filter. Paging uses `>` on the last key
  seen; nothing else is measured as slow.
- A faster key pass. The vocabulary list is read and merged once per
  call today; that pass stays and is milliseconds at 30,000 keys.

## Anti-goals

- Fusing a comparison across a cut, a `not`, an `or`, an `if`, or any
  goal that sits between the `claim/7` goal and the comparison. A skip
  there can change which solution a cut commits to. Anything the
  rewrite is unsure of, it leaves alone.
- A second collation. The skip must use the comparison the `>` goal
  uses, never the index's ordering, so the two cannot disagree.

## Constraints

- Public repository: domain-neutral text throughout.
- `graph-db/rules` depends on `graph-db/spacetime` and `graph-db/query`;
  `graph-db/core` depends on neither. The hook the rewrite needs lives
  in core; the rewrite itself lives in rules.
- 80 columns, spaces, terse comments that point here.
- The GUI's functor-inventory tripwire (tests/gui/gui-tests.lisp) does
  not list the rules functors, since the GUI lane never loads rules;
  the new functor follows `claim/7` and is not listed either.
- The peer wire is untouched. Nothing here serialises.

## Design

### 1. `claim-after/9`, the fused form

A new global Prolog functor in rules/facts.lisp:

```
(claim-after ?c ?family ?sns ?skey ?rel ?ons ?okey ?skey-after ?okey-after)
```

The first seven arguments are `claim/7`'s, with the same routes, in the
same order, including the empty fast paths, the walk and its
cost-unbounded refusal. The last two are strict lower bounds on the
subject key and the object key, read through `%prolog-index-bound`, so
an unbound variable or an explicit `NIL` is "no bound".

A bound is consulted in exactly one place: the namespace route for its
own key. The subject route (`sns` bound, `skey` unbound) applies
`?skey-after`; the object route applies `?okey-after`. Every other
route ignores both, so a key that turns out bound at run time behaves
as `claim/7` does, and the `>` goal that is still in the body does the
filtering.

The route's skip predicate is the `>` goal's own predicate: a key is
read when `(%ordered-p #'> #'string> key bound)` is true, where `key`
is the value the route will unify into the key variable
(`claim-subject-key` or `claim-object-key`, which is also the
vocabulary index's entry). A key that is not a string is skipped
because `>` fails on it. This makes "skipped" and "would be rejected"
the same set by construction; no reasoning about the index collation
is needed, and none is allowed (anti-goal).

This run's own derivation (`%derived-all`) follows the route unfiltered,
as today; the `>` goal filters it.

Implementation: `%namespace-route` gains an `&key after` whose default
is "no bound". The loop over `%scope-keys` skips keys the predicate
rejects. `claim/7`'s body moves into a shared function that takes the
two bounds, and `claim/7` calls it with both `NIL`.

Writing `claim-after` directly in goal text is allowed and documented.
It is the issue's option 2, for free: a caller who wants the bound
visible writes it; nobody has to.

### 2. The rewrite

A clause-body rewriter: `(claim …) (> ?k B)` becomes
`(claim-after … B NIL) (> ?k B)` when the comparison bounds the subject
key, `(claim-after … NIL B) (> ?k B)` when it bounds the object key.
The comparison stays. Correctness never depends on the route taking the
skip.

Conditions, all required:

- A goal is a call of `claim/7`, recognised as rules/compile.lisp's
  `%engine-goal-p` recognises it: a symbol head named `CLAIM`, homed in
  `claim/7`'s package, with seven arguments.
- Its fourth argument (`?skey`) or its seventh (`?okey`) is a Prolog
  variable, by `variable-p`.
- The very next goal in the same conjunction is `(> V B)` with `V` that
  same variable, by `eq`, and `B` a string literal or a Prolog
  variable. `>` is recognised the way the compiler will resolve it: a
  symbol named `>`.
- The two goals are top-level goals of the body being compiled. The
  rewriter sees only that list; a body nested in `or`, `not`, `if`,
  `once` or `forall` is compiled by that construct's compiler macro
  and is never offered to it.

One comparison per key. When both keys are variables and the two next
goals bound them in turn, both are fused. A `>=`, `<`, numeric literal,
a cut, or any other goal between the two leaves the `claim/7` goal as
written.

### 3. The hook

`compile-clause` (prologc.lisp) is the one place every body passes:
`select` and `?-` queries, `<-` clauses, and the rules runner, which
compiles its bodies through `run-query-goals` and so through `select`.
It gains:

```lisp
(defvar *clause-body-rewriters* '()
  "Functions (GOALS) -> GOALS applied in order to a clause body before it
is compiled. A rewriter returns an equivalent body: same solutions, same
order. Registered by subsystems (rules/facts.lisp); core registers none.")
```

applied to the clause's body before `compile-body`. Rules pushes
`%fuse-key-bounds` onto it at load time, once (`pushnew`). A rewriter
runs on every clause compiled, so it must be cheap on bodies it does not
touch: one pass, no consing when nothing matches.

Compile-time only. The rewrite happens before the guard's cost refusal
and after the guard's validation, so a free-text query is validated as
written and compiled as fused.

### 4. Budget and timeout

Skipped keys call no goal, so they cost no inference and are not
preempted by `%tick`. A run of skipped keys is one string comparison
each: 30,000 keys is milliseconds, inside any budget a page runs under.
The route's preemption between keys, "the work between two `%TICK`s is
one key's claims", is unchanged for the keys that are read.

### 5. Documentation

- `docs/rules.md` "Solution order": the key-bound sentence now says the
  route starts after the bound when the `>` goal immediately follows
  the `claim/7` goal, names `claim-after/9` as the form it compiles to
  and the explicit way to write it, and lists what is not fused.
- `docs/guarded-query.md` "Paging": the "page by key" bullet says a
  deep page costs its own keys' claims plus one comparison per earlier
  key, and the condition (the `>` goal right after the indexed goal).
- `CHANGELOG.md` `[Unreleased]`, under Added: `claim-after/9` and the
  rewrite, with the condition; under Changed, the paging cost.
- `claim/7`'s docstring points at `claim-after/9`.

## Acceptance

- **Equivalence.** Over the rules fixture, every combination of fused
  and unfused shape answers the same rows in the same order: subject
  bound, object bound, a bound that excludes everything, a bound that
  excludes nothing, a non-string bound, a bound given through a
  variable, and a variable the run binds before the goal.
- **Cost.** A fixture of N keys, one claim each, and a bound at key
  N − 5: under an inference budget that covers the five keys' goals
  but not N, the fused query answers and the same query with the `>`
  separated from `claim/7` by a `true` goal signals the resource
  error. Removing the skip from the route makes the fused query signal
  too (mutation).
- **Rewriter.** Unit tests on `%fuse-key-bounds`: fuses the adjacent
  subject case, the object case and both; leaves alone a separated
  comparison, a `>=`, a numeric bound, a `<`, a non-variable key, and a
  body with no `claim/7`; returns the body `eq` when nothing matches.
- **Perf.** A bench in `tests/perf/` over N keys (N = 20,000 at normal
  scale, 2,000 at small) records the first page, the last page and
  their ratio through `run-guarded-prolog` with `:limit 500`. Reported,
  not gated, as the spacetime bench is. **The proposal: the ratio
  should come in under 3.** That figure is mine, not the issue's; the
  issue says "a small factor".
- **Docs** as in §5. Every suite the branch touches green: rules,
  prolog-functor, query, gui (the inventory is unchanged), the perf
  system loads.

## Calls I made

- `>` only. `>=` is not fused (non-goal). Cost if wrong: a caller
  paging inclusively stays slow and reads the docs to learn why.
- Adjacent goals only, top level only. Cost if wrong: a caller whose
  `>` comes two goals later stays slow; the docs say where to put it.
- The fused functor is public and has an arity of nine, with both
  bounds, rather than two functors or an operator argument. Cost if
  wrong: an argument list that is long for the hand-written case.
- The hook is a generic rewriter list in core, not a claim-specific
  seam. Cost if wrong: one more extension point to document.
- The skip predicate is `%ordered-p` applied to the stored key, not a
  position in the sorted key list. Cost: one comparison per earlier key
  rather than a binary search. It buys the equivalence by construction.
- The perf bench measures through `run-guarded-prolog`, the consumer's
  path, and the perf system gains `graph-db/rules` as a dependency to
  reach `claim/7`.

## Left to the builder

The shared body function's name; how `%namespace-route` threads the
bound; the bench's claim shape and labels, within the perf README's
rules (a new bench does not bump the suite generation); test names,
grepped for first (`tests/` shares one package per suite).
