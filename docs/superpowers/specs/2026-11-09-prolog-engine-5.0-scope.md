# Prolog Engine board in 5.0: release scope

**Status:** proposed 2026-11-09; the order, the 5.1 fallback and the
before-and-after rule agreed by Kevin the same day. Tracker: GH #45 (the
epic), board https://github.com/users/kraison/projects/7.

## Intent

Forward chaining landed on `experiment` through October and November
(#333 fixpoint rules, #456 forward evaluation, #477 `retire-rule`). The
rules layer now leans on the Prolog engine for every round of every
rule, and the next release, **5.0.0** (the Unreleased section already
carries a MAJOR change, #60), is to carry the rest of the Prolog Engine
board with it: the engine the rules run on catches up with the layer
built on top of it.

Kevin asked for the board's items in the next release and opted out of
the brainstorm questions, so every scoping call below is the author's,
marked as such, for him to overrule in review.

## What is on the board

| Issue | Item | State on 2026-11-09 |
|---|---|---|
| #102 | index-backed generators | done (79360f5) |
| #328 | one home for the row-cap and probe rule | open, small |
| #366 | export the screened reader | open, small |
| #383 | export the pre-guarded runner | open, small |
| #121 | clause indexing for asserted clauses | open; `<-` still recompiles the whole functor |
| #124 | 4-port tracer and `explain` | open; only `*prolog-trace*` exists |
| #122 | Datalog stratum, tabling, magic sets | Unit 1 landed as #333; Unit 2 (tabling for `select`, magic sets) open |
| #123 | graph-native power | bullet 1 done via #102; bullets 2 and 3 no code; bullet 4 marked do-not-pull-in |
| #376 | compiled-form cache | open |
| #392 | ordered index on the transaction extent | open |
| #45 | the epic | closes with the rest |

## The units, in order

Seven units, one PR each into `experiment`, one issue each closed by
hand with the landing SHA after the post-merge slow run is read
(CLAUDE.md, "Working in this repo"). Units 2 to 7 each get a design
spec and a plan under `docs/superpowers/` when they start; this
document fixes what each contains and leaves out.

### Unit 1: query exports (#328, #366, #383)

The three small items, one plan:
`docs/superpowers/plans/2026-11-09-query-exports.md`. First because a
day of work unblocks embedders of the guard (the blackboard, a Lisp
service taking s-expressions from peers) and it needs no design.
**Calls (author's):** the cap and probe rule moves to `query/dsl.lisp`
as `clamp-row-cap` and `probe-limit`, exported from `graph-db`; the
screened reader is exported as `scan-query-text` plus
`with-screened-forms` / `call-with-screened-forms`, with the scratch
package's lifetime inside the call; `*prolog-max-query-length*` is
enforced by the screen itself, so `run-guarded-prolog` and
`guard-query-text` refuse an overlong text as the GUI always did; the
pre-guarded runner is `run-guarded-goals`. Method: local-agent.

### Unit 2: clause indexing (#121)

Asserting a clause compiles that clause, not the functor; calls with a
bound first argument reach only the clauses whose first argument can
match; per-arity clause lists replace `clauses-with-arity`'s scan.
**Calls:** the cut stays correct across the change (today's cut is a
`return-from` of the functor's block, so a per-clause closure needs the
dispatcher's block as its cut barrier; the design says how); indexing
is on the first argument's principal functor or atom, with the
variable-headed clauses merged in source order, which is the WAM's
first-argument indexing and enough for fact tables. Acceptance is the
issue's: a measured before/after on a functor with a realistic clause
count, on a named host, third run, plus the full suite. Engine core:
written inline, not by a worker.

### Unit 3: tracer and `explain` (#124)

Before tabling, because the issue is right that tabling is opaque
without it. **Calls (revised 2026-11-09 with the design):** the 4-port
trace is a runtime hook checked where every goal call already checks
`*prolog-trace*`, so an untraced call costs what it costs today and
nothing is recompiled; `explain` takes the guard's `(vars goals)` and
reports, per goal, the functor and the access path it would take for
that binding pattern, read from a registry the index-backed routes
(#102, spacetime's `claim/7` routes) declare themselves in, and for an
asserted functor how many clauses unit 2's index leaves. Acceptance per
the issue: a readable 4-port trace of a recursive query, and `explain`
confirming index use. Inline. Design:
`2026-11-09-tracer-explain-design.md`.

### Unit 4: tabling for `select` (#122, Unit 2)

The flagship, and the largest unit. **Calls:** a `table` declaration
per predicate; variant tabling with answer tables scoped to one query
(no persistent tables in 5.0); a tabled strongly connected component
iterates its tables to completion rather than suspending and resuming
consumers (SLG-lite), which is what gives termination on cycles and
polynomial evaluation; the deferred trampoline question is answered by
this design, since consumers of a table iterate it instead of
recursing, and the stack bound remains documented for untabled
recursion. **Magic sets are left out of 5.0** and filed as their own
issue against the rules layer: `run-rules` already evaluates bottom-up
and semi-naive (#333), and tabling supplies the binding-passing that
magic sets emulate for the top-down `select` side, so the remaining
case is a bound query over a stored rule set at volume, which has no
workload yet. #122's own warning stands: tabling is sized against a
synthetic cyclic graph in the suite and against the spine on ma-dev
for the third-run measurement. Inline, with its own design spec.

### Unit 5: aggregation and graph predicates (#123, bullets 2 and 3)

`aggregate_all/3` with `count`, `sum`, `min`, `max`, `bag` and `set`;
`reachable/3` and `k_hop/4` as tabled predicates over the edge
functors, so reachability is built once, on Unit 4; `shortest_path/4`
as Dijkstra in Lisp over `traverse.lisp`'s adjacency with a weight
slot. The guard admits them under the existing budgets. **Bullet 4,
cost-based join reordering, stays out**, as the issue says. Inline.

### Unit 6: compiled-form cache (#376)

As the issue describes: canonical shape with lifted literals, a
bounded per-image cache of compiled `select` functions invalidated on
schema change and functor redefinition, measured on the two-hop
`related/3` bench and a rules-round bench. The rules layer's
`%solutions` is the first consumer. Inline for the compiler change;
the bench may go to a worker.

### Unit 7: touched-at index (#392)

An ordered index on the transaction extent's touched-at instant, per
family, keyed `(touched-at . node-id)`, maintained on claim creation
and `retract-claim`; `claim-touched-at/2` with the claim unbound and
the cursor bound becomes a generating route the guard admits. Rules
and spacetime layer, not engine core: local-agent with a design note.

## Order and why

1 banks the small items and unblocks consumers. 2 and 3 make 4 cheaper
to build and to debug; 3 reads unit 2's dispatcher for its clause events
and candidate counts, so it branches from `experiment` once unit 2 has
landed (PR #483). 4
before 5 because 5's reachability is a tabled predicate. 6 and 7 are
performance work with their own benches and slip without taking
anything with them.

## The release

5.0.0 is cut by `docs/ci.md`'s "Cutting a release" once the seven
units are on `experiment` and green. **Agreed 2026-11-09:** if a
release date arrives before Unit 7, Units 6 and 7 move to 5.1 and 5.0
ships with 1 to 5; nothing else on the board makes sense to split.
Sizes are the author's estimates:

| Unit | Sessions |
|---|---|
| 1 | 1 |
| 2 | 2 |
| 3 | 2 |
| 4 | 4 to 6 |
| 5 | 2 |
| 6 | 2 |
| 7 | 1 to 2 |

## Before and after

Kevin's observation on 2026-11-09: most of these units are performance
work, so each needs cases to compare before and after. The rule for
this scope: **the bench comes before the code.**

- **Unit 1b, the Prolog bench**, lands right after Unit 1 and before
  Unit 2 starts: `tests/perf/prolog-bench.lisp` in `graph-db/perf-test`,
  reported, not gated, as `rules-bench.lisp` is. One case per
  performance unit, each sized by `scale`:
  - asserted clauses: assert N facts of one functor with `<-`, then
    call it with a bound first argument (Unit 2, #121);
  - a repeated guarded query shape with a changing literal, through
    `run-guarded-prolog` (Unit 6, #376), beside the existing two-hop
    `related/3` case in `spacetime-bench.lisp`;
  - a recursive predicate over a cyclic graph under `select`'s
    inference budget (Unit 4, #122), which before tabling records the
    bound it hits and after records a terminating closure;
  - a change-feed cursor goal over a namespace with N claims and a
    cursor past most of them (Unit 7, #392);
  - `reachable` and `shortest_path` over a generated graph (Unit 5,
    #123), which before the unit is the hand-written `select` closure
    the predicate replaces.
- The bench's third-run numbers on odm at the pre-unit commit go on
  the unit's issue before its code changes; the unit's plan ends with
  the same bench, same host, third run, and both tables on the issue.
  The release notes for 5.0 quote the pairs.
- `docs/perf-baselines.md`'s comparability rule applies: same host,
  same generation, or the numbers are not compared.

## Non-goals

- Magic sets (filed separately when Unit 4 starts).
- Cost-based join reordering (#123 bullet 4).
- Persistent (cross-query) answer tables.
- Any change to the peer replication wire (`*peer-protocol-version*`).
- ECL and CCL parity for the new engine code is verified in dedicated
  sessions, per CLAUDE.md; the `#+ecl` and `#+ccl` arms are written,
  not skipped.
