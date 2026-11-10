# Aggregation and graph predicates: design

**Issue:** GH #123, bullets 2 and 3; unit 5 of the Prolog Engine 5.0
scope (`2026-11-09-prolog-engine-5.0-scope.md`). **Status:** proposed
2026-11-10; builder check pending; the calls are the author's, Kevin's
review pending. **Depends on:** unit 4 (#122, tabling, on
`experiment` at 4d2fd2c).

## Intent

#123's bullet 2 asks for first-class aggregation and bullet 3 for
graph-algorithm predicates. Half of bullet 3 already exists: the
optional `graph-db/algorithms` system exposes `shortest-path/4`
(Dijkstra over the edge `weight` slot, a Fibonacci heap, A* with a
heuristic), `distance/3`, a Lisp `reachable/2` and the ranking
predicates as Prolog functors (`algorithms/prolog.lisp`, manual
Chapter 14), with their own suite. The scope's line "`shortest_path/4`
as Dijkstra in Lisp over `traverse.lisp`'s adjacency" is therefore
done, and this unit does not build it twice. What is missing, and
what this unit builds:

- `aggregate-all/3`: count, sum, min, max, bag and set over a goal's
  solutions, admissible in guarded query text, where today the only
  aggregate is `findall/3` plus a `lisp` call, and `findall` is
  withheld from free text.
- `reachable/3`: the transitive closure over one edge type, as a
  tabled predicate built on unit 4, so that a closure in a `select`,
  a guarded query or a rule body terminates on a cycle and composes
  with other goals (`not`, `aggregate-all`, a slot test on the far
  node). `reachable/2` stays what it is: a bounded yes/no check
  between two bound vertices, in Lisp.
- `k-hop/4`: the distinct nodes exactly K hops from a vertex along
  one edge type, where today a reader writes K chained edge goals and
  gets one row per path.

Measured on the unit 1b bench's cases plus the ones this unit adds,
before and after, on odm, third run; and the spine row deferred from
unit 4, run where the application's schema is.

## Goals

- `(aggregate-all SPEC GOAL ?result)` with SPEC `count`, `(sum ?e)`,
  `(min ?e)`, `(max ?e)`, `(bag ?t)`, `(set ?t)`, `(min ?e ?w)` and
  `(max ?e ?w)`, over every solution of GOAL, binding nothing of
  GOAL's; `count`, `sum`, `bag` and `set` always succeed (0, 0, nil,
  nil over no solution), `min` and `max` fail over none, as SWI's
  `aggregate_all/3` does.
- The static form, GOAL written out in the query, compiles inline like
  `not` and `findall`'s cousins do not: no run-time meta-call, so the
  guard admits it in free text and under `:effects nil`. The dynamic
  form, GOAL in a variable, runs through the runtime functor and stays
  out of free text like `findall`.
- `(reachable ?from ?to TYPE)` enumerates every vertex reachable from
  `?from` by one or more TYPE edges, each once, terminating on a cycle;
  with `?to` bound it checks; the table is one per start and query.
- `(k-hop ?from K ?to TYPE)` enumerates the distinct vertices exactly K
  TYPE-hops from `?from`; K bound, a non-negative integer (K = 0 is
  `?from` itself).
- All three are pure reads: they run under `:effects nil`, under the
  budgets (`reachable` ticks per consumed answer through the tabling
  entry; `k-hop` and the static aggregate run inside the goal's own
  ticks), and the guard admits them in free text.
- The before/after pairs on #123: the hand-written three-hop select
  against `k-hop/4`; the Lisp BFS against `shortest-path/4`; a
  hand-written left-recursive tabled closure against `reachable/3`;
  `findall` plus `lisp length` against `aggregate-all count`.

## Non-goals

- Bullet 4, cost-based join reordering: out, as #123 says.
- `aggregate/3` with grouping by free variables (SWI's bagof-style
  `aggregate/3`): `bagof`/`setof` already group by witness; a grouped
  aggregate is a `setof` of witnesses with `aggregate-all` per group,
  and nobody has asked.
- View-backed aggregation ("backed by views where possible"): a
  `def-view` with a `:reduce` is the existing answer for a stored
  aggregate; `aggregate-all` is the query-time one.
- A trampoline, SLG, or any change to the tabling engine: unit 4's
  `reachable/3` is a client of it.
- Undirected or multi-type `reachable`/`k-hop`: `outgoing-edges/4`
  needs a bound edge type, and `:direction :both` is what the Lisp
  `shortest-path` family offers; a reader who wants either calls those.
- Rebuilding `shortest-path/4`, `distance/3` or `reachable/2`; moving
  `graph-db/algorithms` into the core.

## Today's mechanics

- `findall/3`, `bagof/3`, `setof/3` are global functors over
  `%findall-list` (prolog-functors.lisp:523-571), which `%solve`s the
  goal at run time; the guard withholds all three from free text
  (`*prolog-excluded-predicates*`, query/guard.lisp:291) because a
  goal reached through a variable interns its head. `not`, `once`,
  `if`, `forall` have compiler macros that expand a static goal inline
  and runtime functors for the dynamic form; the guard admits them and
  requires every argument to be a parenthesised goal
  (`*prolog-goal-argument-control*`, :311, enforced at :550).
- Unit 4: a construct that reads a goal's tables as closed binds the
  negation floor (`with-negation-floor`, prologc.lisp); `%findall-list`
  binds it. `table-functor` declares a user functor tabled; its
  clauses are `<-` clauses; the manual says to write the
  left-recursive form for closure.
- `outgoing-edges/4 (from type edge to)` enumerates with any of
  `from`/`to` bound and TYPE a bound symbol; every arm requires the
  symbol, so an unbound TYPE fails (prolog-functors.lisp:757-811).
- The functor inventory is pinned by `prolog-functor-inventory-is-pinned`
  (tests/gui/gui-tests.lisp:1534); its message classifies a new
  functor: one that can be handed a goal in an ordinary argument goes
  on the excluded list, a control construct whose arguments are goals
  on the goal-argument list, anything else on the reviewed inventory.
- `graph-db/algorithms` is optional and depends on `graph-db/core`;
  `graph-db/perf-test` does not load it.

## Design

### D1. `aggregate-all/3`

**Spec terms.** `count` or `(count)`; `(sum ?e)`, `(min ?e)`,
`(max ?e)` with `?e` a term that is a number when GOAL succeeds (an
unbound or non-numeric `?e` on a solution signals `prolog-error` with a
`(:type-error :number ...)` ball, as `is/2` would); `(bag ?t)` and
`(set ?t)` with any template; `(min ?e ?w)` and `(max ?e ?w)` bind
`?result` to the list `(E W)` of the extreme and its witness (SWI's
`min(X, W)`), the first such on ties. `set` is `%sort-unique` (the
`setof` order). Numbers compare with `<`; a mixed integer/float sum is
whatever `+` gives.

**Static form, a compiler macro** `aggregate-all` (prologc.lisp, beside
`not` and `forall`): when SPEC is a symbol or a list headed by one of
the eight words and GOAL is `static-goal-p`, the expansion compiles
GOAL under an opaque barrier (as `not` does, with `*functor*` bound to
the barrier and the cut flag nil) with a continuation that, for
`count`, increments a counter, else pushes `(deref-exp template)` onto
an accumulator, then returns nil so GOAL backtracks; the whole runs
under `with-negation-floor` (it reads GOAL's tables as closed, unit 4
D4); bindings are undone to the trail mark; the result is computed and
unified with `?result`, and the rest of the body compiles after it
with the clause's own cut scope (the `%commit`-style threading `if`
uses is not needed: nothing of GOAL's bindings survives). An
unrecognised SPEC or a non-static GOAL makes the macro return `:pass`,
so the runtime functor takes the call.

**Dynamic form, a global functor** `aggregate-all/3` (prolog-functors
.lisp, after `setof/3`): the same result over `(%findall-list template
goal)`, where the template is the spec's term (or `t` for count); the
spec is dereferenced and validated the same way; an unknown spec
signals `prolog-error` with a `(:domain-error :aggregate-spec ...)`
ball. It is excluded from free text (`*prolog-excluded-predicates*`
gains `"AGGREGATE-ALL"`), which never applies to the static form: the
guard's check at :550 runs on the goal's name, so the exclusion must be
keyed off the goal position being static. **Guard rule:**
`*prolog-goal-argument-control*` entries may name the goal positions:
`("AGGREGATE-ALL" 2)` means argument 2 must be a parenthesised goal and
the other arguments are ordinary terms; a bare name keeps today's
"every argument" meaning. `aggregate-all` goes there, not on the
excluded list, because with the goal static the compile-time path is
the only one taken and the runtime functor is never reached from free
text; the inventory test's classification text gains this third case.
The runtime functor is still registered (the dynamic form from Lisp),
so the whitelist contains it; the position rule, not the exclusion, is
what keeps a variable goal out.

**Effects:** none. `aggregate-all` is a read; under `:effects nil` it
runs; `(sum ?e)` does no `eval`, only `+`.

### D2. `reachable/3`

Two clauses, asserted at load time in the `graph-db` package by a new
`prolog-graph-predicates.lisp` (after `prolog-tabling.lisp` and
`prolog-functors.lisp` in the system chain: it needs `<-`,
`table-functor` and `outgoing-edges/4`), then tabled:

```lisp
(<- (reachable ?a ?b ?type) (outgoing-edges ?a ?type ?e ?b))
(<- (reachable ?a ?c ?type) (reachable ?a ?b ?type)
                            (outgoing-edges ?b ?type ?e ?c))
(table-functor 'reachable 3)
```

Left-recursive (unit 4 D8: one table per start, constant stack). A
`select` in any package resolves `reachable` to this functor through
`make-functor-symbol`'s fallback to `graph-db`, as it resolves `is-a`.
TYPE must be bound to an edge-type symbol: `outgoing-edges/4` fails on
an unbound one, so `reachable` with TYPE unbound fails rather than
errs; the manual says so. `?from` bound is the intended shape; both
unbound builds the full closure (one table) and both bound checks
membership of the start's closure (the start's table). Reflexivity:
`?from` itself is reachable only through a cycle back to it.

The functor is a *user* functor homed in `graph-db`: `reset-functor`
and `delete-functor` on it are possible and unsupported; the inventory
test pins `reachable/3` on the reviewed list. A schema that defines an
edge type named `reachable` collides at arity 2 only (`%edge-functor/2`
and `/3`), as any built-in name would.

### D3. `k-hop/4`

A global functor `k-hop/4 (from k to type cont)` in the same file: a
breadth-first expansion of exactly K levels from the bound vertex
`from` over TYPE edges (`map-edges :vertex v :direction :out :edge-type
type`), with a per-run `equalp` id set so each vertex is visited once
per level and emitted once at level K; `?to` bound filters, unbound
enumerates. K must be a bound non-negative integer and `from` a bound
vertex, else `prolog-error` with an instantiation or type ball; TYPE a
bound symbol, else the call fails like `outgoing-edges/4`. Written in
Lisp, not as tabled clauses: the clause form needs `is/2` to count
down, which is an `:eval` effect the guard refuses under `:effects
nil`, and a level-BFS with a visited set is the right algorithm for
"nodes at distance K". Cost is bounded by K times the graph; no
cost-unbounded flag.

### D4. `shortest-path/4` and the rest of bullet 3

Stays in `graph-db/algorithms`, unchanged. This unit: adds
`:graph-db/algorithms` to `graph-db/perf-test`'s dependencies so the
bench measures `shortest-path/4` beside the Lisp BFS row; adds a line
to Chapter 6's built-in table pointing at Chapter 14 for the algorithm
predicates; and notes in Chapter 14 that `reachable/2` (both bound, a
Dijkstra check) and `reachable/3` (enumerating, tabled, one edge type)
are different predicates.

### D5. Tabling and negation

`aggregate-all` over a goal inside the goal's own recursive tabled
component is refused with `prolog-unstratified-error` exactly as
`findall` is (unit 4 D4: the static form binds the floor; the dynamic
form goes through `%findall-list`, which binds it). `reachable/3` is
engine-defined, so no user rule can be inside its component; `(not
(reachable ?a ?b t))` and `(aggregate-all count (reachable ?a ?x t)
?n)` in any rule are stratified and work. `k-hop/4` is a plain functor
with no table.

### D6. Guard, effects, budgets

All three are admitted in free text: `reachable/3` and `k-hop/4`
through the registry (the inventory test pins them), `aggregate-all`
through the positional goal-argument rule of D1. None is
cost-unbounded: `reachable` is preempted per consumed answer and per
clause goal; `k-hop` and the static aggregate run GOAL's own ticks.
None requires an effect.

### D7. Trace and explain

`aggregate-all`'s static form fires no port of its own (as `not` and
`findall` do not); GOAL's goals fire theirs. `reachable/3` fires
`:call`/`:exit`/`:redo`/`:fail`, `:clause` and `:table` like any tabled
user functor; `explain-goals` reports it as `every clause: 2, tabled`
(or `first-argument index: ...` with the start bound). `k-hop/4` fires
the four ports.

### D8. Cross-implementation

Nothing implementation-specific: a compiler macro, two global functors,
two clauses, an `equalp` hash. ECL and CCL in their sessions.

## Tests

`tests/aggregation-tests.lisp`, suite `aggregation-suite`, names
prefixed `ag-`; `tests/graph-predicate-tests.lisp`, suite
`graph-predicate-suite`, names prefixed `gp-`; both registered in
`graph-db.asd` after `tabling-tests` and both using the `tb-with-functors`
pattern for anything they assert (a third copy is not acceptable: the
helper moves to `tests/suite.lisp` as `with-dropped-functors`, and the
two existing copies stay).

- `ag-count-sum-min-max`: facts `ag-v` 3, 1, 2: count 3, sum 6, min 1,
  max 3; over `(ag-v 9)` count 0, sum 0, `(bag ?x)` nil, `(set ?x)`
  nil, and `(min ?x)` fails (the select returns no row).
- `ag-bag-and-set`: `ag-v` 3, 1, 2, 1: bag `(3 1 2 1)` in solution
  order, set `(1 2 3)`.
- `ag-witness-forms`: `ag-w` (a 3) (b 1) (c 3): `(max ?s ?n)` gives
  `(3 a)`, `(min ?s ?n)` gives `(1 b)`.
- `ag-leaves-no-bindings`: `(aggregate-all count (ag-v ?x) ?n)` leaves
  `?x` unbound; the body after it sees `?n`.
- `ag-non-number-signals`: `(sum ?x)` over a symbol fact signals
  `prolog-error` with a type-error ball, catchable by `catch/3`.
- `ag-dynamic-goal`: `(= ?g (ag-v ?x)) (aggregate-all count ?g ?n)`
  gives 3 through the runtime functor; `(aggregate-all (nonsense ?x)
  ...)` signals `prolog-error`.
- `ag-static-form-compiles-inline`: with the port hook, no `:call` for
  `AGGREGATE-ALL/3` fires on the static form; one fires on the dynamic
  form.
- `ag-under-effects-nil`: `(select (:effects nil) ...)` with a static
  aggregate works.
- `ag-reads-tables-as-closed`: tabled `(<- (ag-p ?x) (ag-q ?x)
  (aggregate-all count (ag-p ?y) ?n) (> ?n 0))` is refused with
  `prolog-unstratified-error`; an aggregate over an independent tabled
  predicate inside a recursive one works.
- `ag-guard-admits-static-and-refuses-dynamic` (in the query suite or
  the gui suite, where `run-guarded-prolog` lives): `(aggregate-all
  count (is-a ?x g-person) ?n)` as text answers; `(aggregate-all count
  ?g ?n)` is refused naming the goal position; `(findall ...)` stays
  refused.
- `gp-reachable-closure-on-a-cycle`: six-ring of `g-knows` persons:
  from one, six rows, each once; `(reachable ?a ?b g-knows)` both
  unbound, 36 rows; a bound target that is reachable succeeds once, an
  unreachable one (a seventh person) fails.
- `gp-reachable-needs-a-bound-type`: `(reachable ?a ?x ?t)` returns no
  row and signals nothing.
- `gp-reachable-composes`: `(not (reachable ?a ?b g-knows))` in a
  select over two persons; `(aggregate-all count (reachable ?a ?x
  g-knows) ?n)` gives 6; `explain-goals` on `(reachable ?a ?x g-knows)`
  contains `tabled`.
- `gp-k-hop-exact-and-distinct`: a ring with chords: level 0 is the
  start; level 1 the direct successors; level 3 each node once where
  the three-edge select has one row per path; a bound `?to` at the
  right level succeeds once and at another fails.
- `gp-k-hop-arguments`: K unbound or negative or `from` unbound signals
  `prolog-error`; TYPE unbound fails.
- `gp-inventory`: the gui suite's tripwire updated: `reachable/3`,
  `k-hop/4` on the reviewed list, `aggregate-all/3` on the positional
  goal-argument list.
- Mutation checks: the static macro's floor removed (`ag-reads-tables-
  as-closed` fails); the visited set in `k-hop` removed (the distinct
  assertion fails); `:pass` on a non-static goal removed (the dynamic
  test fails to compile the query).

## Measurement

Before, recorded on odm before the code (task 1): the existing
`k-hop-3-select` (0.049 s, 2,041/s) and `shortest-path-lisp-bfs`
(658/s) rows from the unit 1b report, re-run at the pre-unit commit,
plus two new cases in `bench-graph-predicates`: `reachable-closure-
handwritten` (a left-recursive tabled `pbp-lpath/2` over the chords
ring from 100 starts: the closure a reader writes today) and
`aggregate-count-findall` (`findall` plus `(lisp ?n (length ?l))` over
the two-hop neighbourhood of 100 starts). After: `k-hop-3-predicate`,
`shortest-path-predicate` (`shortest-path/4` over the same pairs, with
`graph-db/algorithms` loaded), `reachable-predicate`, `aggregate-count-
predicate`; third run on odm, both tables on #123, `docs/perf-
baselines.md` entry.

The spine row deferred from unit 4: `reachable/3` over the spine's
edge type on a copy of ma-dev's store, run in the application's own
image on odm (where its schema is loaded), three runs, the third on
#123 in domain-neutral terms (edge type name and counts, no domain
content). If the application image cannot run it inside the unit's
budget, the row is deferred again with the reason, and the unit does
not wait on it.

## Acceptance (#123, bullets 2 and 3)

- `aggregate-all/3` with count, sum, min, max, bag, set and the
  witness forms, admitted in free text in its static form: the `ag-`
  tests and the guard test.
- `reachable/3` terminating on a cycle and `k-hop/4` distinct at
  level K: the `gp-` tests.
- `shortest-path/4` measured beside the BFS row; nothing rebuilt.
- Before/after on odm, third run, both tables on #123; full SBCL suite
  green on the post-merge run.

## Left to the builder

- The exact shape of the positional goal-argument entries and how the
  guard's `:550` loop reads them.
- Whether the static macro's accumulator is a list or a vector, and how
  `count` avoids consing.
- The `with-dropped-functors` helper's home and the two existing copies'
  fate (leave them; a third copy is the line).
- The manual's examples (Chapter 6: an `aggregate-all` row and a
  `reachable`/`k-hop` row in the built-in tables with one example
  each; Chapter 14: the `reachable/2` versus `/3` note).
