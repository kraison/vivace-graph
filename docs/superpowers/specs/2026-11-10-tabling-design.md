# Tabling for `select`: design

**Issue:** GH #122 (unit 2 of its 2026-09-06 split), unit 4 of the
Prolog Engine 5.0 scope (`2026-11-09-prolog-engine-5.0-scope.md`).
**Status:** proposed 2026-11-10; the calls are the author's, Kevin's
review pending. **Depends on:** units 2 (#121, the per-clause
dispatcher) and 3 (#124, the port hook), both on `experiment`.

## Intent

A recursive rule over a cyclic graph does not terminate today. The
unit 1b bench's 200-node ring with `(<- (pbp-path ?a ?c) (p-knows ?a
?b) (pbp-path ?b ?c))` exhausts SBCL's control stack in 0.09 s before
it reaches a 200,000-inference budget, and returns nothing
(`tests/perf/results/prolog-before-odm-55240fd.report`). The rules
layer answers recursion bottom-up (#333), but an ad-hoc `select`, a
guarded query from the web surface and unit 5's `reachable/3` all run
top-down through the compiler, and for them #122 asks for tabling: a
declared predicate's calls are memoised per query, a call that recurs
into itself reads the answers found so far instead of descending
again, and the evaluation iterates until no new answer appears. The
query then terminates on a cycle with the complete answer set, in time
polynomial in the answer count, and its stack depth no longer follows
the derivation's length.

## Goals

- `(table-functor 'name arity)` declares a user functor tabled;
  `untable-functor` undoes it; `functor-tabled-p` reports it. The
  declaration may come before or after the functor's clauses and
  survives `<-`, `reset-functor` and `make-functor :clauses`.
- A tabled call terminates on a cyclic graph with the complete, duplicate
  free answer set: the ring bench records `:complete` and 200 solutions
  where it recorded `:stack-exhausted` and 0.
- Left-recursive and right-recursive forms of the same rule give the
  same answers; the left-recursive form runs in stack depth independent
  of the derivation length (D8).
- Answer tables live for one query (`select`, `?-`, `run-query-goals`,
  the rules layer's `%solutions`) and are dropped with it: no
  persistent tables, no cross-query staleness.
- Untabled predicates compile and run exactly as today: the tabled
  path is a different functor function, not a check in every call.
- Negation over a tabled predicate inside its own recursive component
  is refused with a `prolog-error`, not answered wrongly (#122:
  "stratified negation enforced, not assumed").
- `:max-inferences`, `:timeout`, `:limit`, `:callback`, `:count`, the
  port hook, `explain-goals` and `profile-goals` keep working through a
  tabled call.

## Non-goals

- SLG with suspension and resumption of consumers (true SLG). This is
  linear tabling: an incomplete table is re-evaluated, never resumed.
- Subsumptive tabling (answering `p(a,X)` from a table for `p(X,Y)`).
  Variant tabling only.
- Finer strongly connected components than "everything first called
  while a leader is evaluating" (D3). The cost is re-evaluation of
  independent sub-tables in each of the leader's rounds, not a wrong
  answer.
- Persistent or shared tables; tables across `:write` effects within
  one query (a `retract/1` after a tabled call reads stale answers; the
  guard's `:effects nil` queries cannot do this).
- Tabling a built-in or an edge functor directly. Wrap it in a `<-`
  rule and table that; unit 5 defines `reachable/3` that way.
- Magic sets. Filed separately against the rules layer (the issue is
  linked from #122 when this unit starts), per the scope.
- Changing the compiler's stack behaviour for untabled recursion: its
  bound is documented, not lifted (D8).

## Today's mechanics

- A user functor is a `functor` struct (functor.lisp): `name fn clauses
  lock arity dispatcher clause-set`. Its `fn` is the dispatcher built
  once by `%make-dispatcher` (prologc.lisp:968), which walks the clause
  set's candidates for the call's first argument, undoes bindings
  between them and tail-calls the last. `%add-compiled-clause`
  (prologc.lisp:1040) installs the dispatcher as `fn` unless it already
  is; `prolog-compile` (:222) resets the functor to the null function
  and re-adds every clause; `reset-functor` goes through it.
- `compile-call` (prologc.lisp:301) fetches `fn` at run time
  (`get-functor-fn`, else the global functor table), ticks the budget,
  and calls it with the arguments and the continuation, through
  `%ported-call` when the port hook is set. `%solve`
  (prolog-functors.lisp:247) does the same for a dynamic goal. So
  whatever a functor's `fn` is, every static and dynamic call reaches
  it: a tabled functor needs only a different `fn`.
- A query root (`select` prologc.lisp:1356, `?-` :1107, the two other
  `*trail*` rebinding sites at :1396 and :1512) binds a fresh `*trail*`,
  `*var-counter*`, the budget specials and `*seen-table*`, builds a
  clause-less functor for the goals and runs it with `prolog-ignore` as
  the final continuation. `run-query-goals` (query/dsl.lisp:315) and
  the rules layer's `%solutions` both `eval` a `select` form.
- A solution is delivered by calling the continuation with the
  bindings in place; failure is a return. `undo-bindings` to a saved
  trail fill-pointer unbinds. `deref-copy` (prologc.lisp:831) copies a
  term with fresh variables; `deref-exp` dereferences in place.
- Negation: the `not` compiler macro (prologc.lisp:653) runs the goal
  under a barrier block with a continuation that sets a flag and
  returns; `not/1` (prolog-functors.lisp:420) is the dynamic form;
  `forall` compiles to nested `not`; `if`'s test and `once` commit to a
  first solution but do not negate.
- A cut inside a clause sets the dispatcher's cut cell and returns from
  the clause block (unit 2, D3); `once`, `not` and `if` are barriers.

## Design

### D1. The declaration

A `tabled` slot on the `functor` struct, default `nil`. `table-functor
name arity` finds or makes the functor (`make-functor-symbol name arity
:define t`, then `make-functor`), sets `tabled` to `t` and installs the
tabled entry (D3) as `fn` under the functor's lock. `untable-functor`
clears the slot and reinstalls the dispatcher (or the null function
when there are no clauses). `functor-tabled-p name arity` reads the
slot of a registered functor, `nil` for none.

`%add-compiled-clause` and `prolog-compile` install `fn` through one
helper, `%install-functor-fn`, which picks the tabled entry when the
slot is set and the dispatcher otherwise; today's `(unless (eq fn
dispatcher) (set-functor-fn ...))` moves into it. The tabled entry
calls the functor's `dispatcher` slot directly, so the dispatcher is
built even for a tabled functor and `functor-dispatcher` keeps meaning
"the clause walk".

A global functor (`def-global-prolog-functor`) cannot be tabled:
`table-functor` signals a `prolog-error` naming it, as `add-clause`
does for a clause on one. The three functions are exported from
`graph-db`. There is no Prolog-side directive; the guard's query text
cannot declare a table, by design (the declaration is a schema-level
decision, like a view).

### D2. Tables, variant keys and answers

`*tables*` is a special, `defvar`ed in globals.lisp, bound by every
query root (the four `*trail*` sites, in the same `let*`) to a fresh
`equal` hash table; its value outside a query is `nil`. A tabled call
that finds `*tables*` nil (a `%solve` from Lisp with no query around
it) binds one for its own extent, so a tabled predicate never runs
without a table and never leaks one past the query.

**Key.** The call's variant key is the dereferenced argument list with
each atom canonicalised so that `equal` on keys implies `prolog-equal`
on the arguments, and each unbound variable replaced by `(:var . i)`
numbered by first occurrence, so `p(X,Y)`, `p(A,B)` share a key and
`p(X,X)` has another:

| argument | key |
|---|---|
| unbound variable | `(:var . i)`, i by first occurrence |
| number | its `rational` (1 and 1.0 are `prolog-equal`) |
| string | the string (`string=`) |
| symbol, character, `nil` | itself |
| node | `(:node . id-as-list)` (nodes are equal by id) |
| timestamp | `(:ts . universal-time nanoseconds)` |
| array | `(:array . contents-as-list)` |
| cons | the cons of the parts' keys |
| any other object | itself under `equal` |

A string and a keyword are `prolog-equal` case-insensitively but take
different keys: the two calls get two tables, each correct. Sharing
is a performance property; the soundness property is that one key
never serves two calls that could differ, and the table above keeps it.

**Table.** `(defstruct table key answers seen complete round leader
negation-level)`: `answers` an adjustable vector of answer tuples in
insertion order (so consumption is by index and sees answers added
while it runs), `seen` an `equal` hash of answer keys (canonicalised
as above, variables numbered) for duplicate elimination, `complete` a
boolean, `round` the leader's round number at which this table was
last evaluated, `leader` the table that completes it (itself for a
leader), `negation-level` the depth of negation at creation (D4).

**Answer.** An answer is the argument tuple as it stands when the
clause body succeeds: `deref-copy` of the argument list, so it holds
fresh variables where the answer is non-ground, and a flag `ground-p`
set when the copy contains none. Consuming an answer unifies each
argument with the stored term when `ground-p`, else with a
`deref-copy` of it (fresh variables per consumer), under the caller's
trail; bindings are undone between answers.

### D3. Evaluation: leaders, followers, consumers

The tabled entry is one function per functor, closed over the functor,
of `(args... cont)`. On a call it computes the key, looks the table up
in `*tables*` and takes one of four paths. `*table-stack*` is a special
holding the tables whose evaluation is in progress, innermost first;
`*leader*` the current leader table or `nil`.

1. **Complete table.** Consume: for each answer in order, unify, call
   `cont`, undo. Return.
2. **No table, no leader** (`*leader*` nil, or the current leader is
   separated from this call by a negation, D4). The call is a
   **leader**. Create the table with `leader` itself, push it, and
   iterate:
   - set `round` to the next round number; mark `changed` nil;
   - evaluate: call the dispatcher with the original arguments and the
     continuation *add-answer*, which copies the argument tuple into
     the table unless `seen` has its key, sets `changed` when it adds,
     and returns `nil` (so the dispatcher backtracks through every
     clause);
   - if any table whose `leader` is this one gained an answer in the
     round, repeat; else mark every such table `complete`, pop, and
     consume this table into `cont` as in path 1.

   The repeat condition is a counter on the leader, bumped by every
   add-answer under it, compared before and after the round.
3. **No table, a leader active.** The call is a **follower** in the
   leader's component: create the table with `leader` the current
   leader and `round` the leader's round, push it, evaluate once with
   add-answer, pop, then consume its answers so far into `cont`. It is
   not marked complete; the leader's final round will be.
4. **Incomplete table** (created in this query by paths 2 or 3):
   - on `*table-stack*`: an ancestor; a **looping consumer**. Consume
     the answers so far into `cont` and return. New answers the
     ancestor finds later reach this call site in the leader's next
     round, which is why rounds repeat until nothing changes.
   - not on the stack and `round` is the leader's current round: it
     was already evaluated this round; consume into `cont`.
   - not on the stack and `round` is older: re-evaluate as a follower
     (set `round`, push, evaluate once, pop), then consume into `cont`.

Consumption always reads the vector by index up to its length at each
step, so a consumer running during evaluation also delivers answers
that arrive while it runs; this is what makes the left-recursive ring
converge in two rounds (one that finds everything, one that finds
nothing new).

Soundness and completeness, for a definite program: every answer in a
table was derived by a clause from answers and facts, so it is a
consequence; a leader completes only after a round that derived
nothing new while every follower was evaluated at least once in that
round or consumed from a table evaluated in it, so the tables are
closed under the clauses, hence the least fixpoint. Duplicate
elimination is by variant, so two derivations of one answer are one
answer; solution order is first-derivation order and is deterministic
for one program and graph.

**Budget and deadline.** The clause bodies run through `compile-call`,
which ticks; a round that cannot finish inside `:max-inferences` or
`:timeout` signals `prolog-resource-error` out of the leader as any
query does, and D5's unwind drops the tables. Consumption of a complete
table also ticks once per answer delivered (an explicit `%tick` in the
consume loop), so a huge table under a small budget still trips it.

### D4. Negation and stratification

`*negation-level*` is a special, 0 at a query root, incremented around
the goal of the `not` compiler macro, `not/1`, and therefore `forall`
(which compiles to `not`). `if`'s test and `once` do not increment:
they commit, they do not negate.

A table records `negation-level` at creation. Path 2's condition "no
leader" also holds when `*negation-level*` is greater than the current
leader's `negation-level`: a tabled call first reached under a negation
inside a leader's evaluation starts its own, nested leader, so an
independent predicate under `not` completes on its own (the common,
stratifiable case). Path 4 then checks: consuming an incomplete table
whose `leader` is not the current leader, or whose `negation-level` is
less than the current `*negation-level*`, means a negation stands
between a table's evaluation and a consumer that feeds it; the
program is not stratified through that call, and the entry signals
`prolog-error` with reason "tabled NAME/ARITY is called under negation
inside its own recursive component (unstratified)" and an ISO
`permission_error`-shaped ball `(:permission-error :negate :procedure
NAME/ARITY)`, so `catch/3` can take it. A cut, `once` or `if` across a
tabled call is not negation and is not refused (D5).

This refuses exactly the cases where negation-as-failure over an
incomplete table could answer wrongly, and nothing else that this
design can decide. It does not do a static stratification of the
program: `run-rules` already does that for rule sets (#333); for
`select`, the refusal is at the call.

### D5. Cut, commit, limits and non-local exits

- **A cut inside a tabled predicate's clause** commits that clause in
  that evaluation as today (unit 2, D3); the answers found before and
  after are tabled. The table is the predicate's answer set under its
  clauses with their cuts, deterministic per program.
- **A cut, `once`, `if` or `:limit` across a tabled call** (the consumer
  stops early) is a non-local exit through the entry: `return-from` a
  barrier block, or `throw :prolog-limit-reached`. During *consumption*
  of a complete table nothing is harmed: the table stays complete. During
  *evaluation* (a leader round or a follower's single evaluation) the
  continuation is add-answer, which never exits non-locally; only an
  error or the budget can leave a round early. The leader's iteration is
  wrapped in `unwind-protect`: on an abnormal exit every table whose
  `leader` is this one is removed from `*tables*`, so a later call in the
  same query (after `catch/3` recovered, say) starts afresh instead of
  reading a half-built table.
- **The trail.** The entry saves the fill-pointer on entry and undoes to
  it between answers and before returning, as the dispatcher does.
  Add-answer copies the tuple before the dispatcher undoes the clause's
  bindings, so the stored answer is independent of the trail.

### D6. Scope and concurrency

Tables are per query, held in a special bound at the root: two threads
running queries never share one, and a query's tables are garbage when
it returns. The `tabled` slot is a functor property under the functor's
lock, like its clauses; a running query that read the entry before an
`untable-functor` finishes its call through the entry it fetched, as a
query that fetched a dispatcher before an assert does today.

### D7. Trace and explain

- **Ports.** A tabled call fires `:call`, `:exit` per delivered answer,
  `:redo`, `:fail` through `%ported-call` exactly as any call: the entry
  is the `fn` `compile-call` wraps. The clause evaluation inside fires
  `:clause` events as now. The entry fires one new port, `:table`, with
  args `(:complete n)`, `(:leader round)`, `(:follower)`, `(:consume n)`
  so a trace shows whether a call was answered from a table or evaluated
  it; `%print-port` prints it as `TABLE NAME/ARITY complete 12`.
  `profile-goals` counts it under the functor as `tables`.
- **`explain-goals`.** A user functor's line gains `, tabled` after the
  clause count when the functor is tabled; nothing else changes.
- **Budget accounting.** `profile-goals`' inference count includes the
  consume ticks.

### D8. The trampoline question, answered

#45 Phase 0.4 bounded deep recursion with a budget instead of a
trampoline, and #122 reopens the question. The answer this design
gives:

- A **left-recursive** tabled rule, `(<- (path ?a ?c) (path ?a ?b)
  (edge ?b ?c))` with a bound first argument, has one table per
  distinct first argument. The recursive call is a looping consumer
  that iterates a vector; the evaluation's Lisp depth is the clause
  body's, not the derivation's. A chain or ring of any length runs in
  constant stack, in rounds proportional to the longest derivation
  that a single round's in-flight consumption does not already cover
  (two for a ring, as measured by the bench).
- A **right-recursive** tabled rule, `(<- (path ?a ?c) (edge ?a ?b)
  (path ?b ?c))`, has one table per reachable node; each is a follower
  evaluated inside its caller's evaluation, so the Lisp depth is the
  number of distinct calls on the stack at once: the ring's
  circumference, the chain's length. It terminates on a cycle where it
  did not, and its depth is bounded by the number of distinct nodes
  rather than by the number of paths, but a chain longer than the
  control stack can hold still exhausts it. The manual says so and
  shows the left-recursive form as the one to write for closure over
  large graphs.
- **Untabled recursion** keeps unit 2's property, tail position for a
  last-goal call, and the budget; nothing else changes. No trampoline
  is built: the top-down compiler's calling convention stays direct
  Lisp calls, and the rules layer remains the engine for bulk
  derivation.

### D9. Cross-implementation

`equal` hash tables, adjustable vectors, specials and `unwind-protect`
only: nothing implementation-specific. The `#+ecl` user-functors lock
covers `table-functor`'s registry access as it covers `make-functor`'s.
Verified on SBCL; ECL and CCL catch up in dedicated sessions per
CLAUDE.md.

## Tests

`tests/tabling-tests.lisp`, suite `tabling-suite` in `graph-db-suite`,
names prefixed `tb-`, functors dropped before and after each test as
`ci-with-functors` does (shared helper lifted to a common place or
duplicated as `tb-with-functors`). Each is a behaviour a reader can
check against this document:

- `tb-declaration-round-trips`: `table-functor` before clauses, after
  clauses, after `reset-functor`; `functor-tabled-p`; `untable-functor`
  restores the dispatcher (same answers, `:table` port no longer fires);
  a global functor is refused.
- `tb-ring-terminates`: a 6-node ring as asserted facts, right-recursive
  `path/2`, `(select (?x) (path a ?x))` returns the six nodes once each;
  untabled, the same query under `:max-inferences 5000` signals
  `prolog-resource-error` (the before, kept as the contrast).
- `tb-left-and-right-recursion-agree`: same facts, both rule shapes,
  same answer set; all-unbound call `(path ?x ?y)` returns the 36 pairs.
- `tb-mutual-recursion`: `even/odd` over a chain of successor facts,
  two tabled functors in one component; answers complete.
- `tb-nonground-answers`: a tabled predicate with a clause `(<- (p ?x
  ?x))`: `(p ?a ?b)` yields one answer binding the two together; two
  consumers get independent fresh variables.
- `tb-answers-dedupe-by-variant`: two derivations of one answer, one
  solution; `1` and `1.0` one answer; two node objects with one id one
  answer.
- `tb-tables-scope-to-the-query`: a tabled predicate over facts; assert
  a new fact between two queries; the second query sees it. Inside one
  query, two calls with the same key evaluate once (count `:table`
  `:leader` events).
- `tb-negation-is-stratified-or-refused`: `(<- (p ?x) (q ?x) (not (r
  ?x)))` with `r` tabled and independent: answers correct; `(<- (p ?x)
  (not (p ?x)))` with `p` tabled: `prolog-error` with the permission
  ball, and `catch/3` can take it; `forall` over an independent tabled
  predicate works.
- `tb-cut-and-once-across-a-table`: `(once (path a ?x))` returns one
  answer and a following `(path a ?y)` in the same query still returns
  all six (the table completed before the cut); `:limit 2` on a tabled
  query returns two.
- `tb-budget-trips-inside-a-round`: a tabled closure over a 50-node
  ring under `:max-inferences 100` signals `prolog-resource-error`; a
  later call in the same query (under `catch/3`) re-evaluates (no stale
  half table).
- `tb-trace-and-explain-see-tables`: `with-prolog-ports` collects a
  `:table` event per tabled call; `explain-goals` prints `tabled` on the
  line.
- `tb-dynamic-call-is-tabled`: `(call ?g)` with `?g` bound to a tabled
  goal goes through the table (`:table` event fires).
- `tb-mutation-checks` are not tests but the gate the plan names:
  removing the `round` check (path 4, second bullet) must break
  `tb-mutual-recursion`'s evaluation count; removing the `seen` check
  must break `tb-answers-dedupe-by-variant`; removing the
  `unwind-protect` must break `tb-budget-trips-inside-a-round`.

Existing suites that must stay green: clause-indexing (51),
tracer-explain (60), prolog-functor (93), prolog-stress (60), rules
(1147), query (122), concurrency (1680).

## Measurement

The bench is already in the suite (unit 1b): `recursive-ring-closure`
records `:stack-exhausted`, 0 solutions, 0.09 s at the before commit
(#122's 2026-11-09 comment). The plan's last task:

- `pbp-assert-path-clauses` tables `pbp-path/2` after asserting, and
  the case's after row reads `:complete`, 200 solutions, with its time.
- A second case, `tabled-chain-closure`, added before the code: the
  same `pbp-path/2` over a 200-node chain (acyclic, so the untabled
  recursion terminates), timed untabled and tabled in one run. That
  is the tabling tax on a query that did not need it, the number a
  reader wants when deciding whether to declare a table.
- Third run on odm at scale `:normal`, posted on #122 beside the before
  row; `docs/perf-baselines.md` gets the entry.
- The spine measurement #122 and the scope ask for: a containment
  closure over the spine's claims on a copy of ma-dev's store opened in
  the bench image on odm (ma-dev is on odm), third run, as a report on
  #122. If the copy cannot be taken inside the unit's budget, the row is
  deferred to unit 5, whose `reachable/3` is the predicate that would
  run over it, and the deferral is said on #122.

## Acceptance (#122, unit 2 as rescoped)

- Recursive queries terminate on a cyclic graph rather than exhausting a
  bound: `tb-ring-terminates` and the bench row.
- Stratified negation is enforced, not assumed: D4,
  `tb-negation-is-stratified-or-refused`.
- Measured against a real corpus on a named host, third run: the ring
  on odm; the spine row or its stated deferral.
- Full SBCL suite green on the post-merge run; the cross-impl matrix in
  its dedicated session.

## Left to the builder

- The exact struct and special names beyond those given; whether the
  tabled entry is a closure per functor or one function taking the
  functor (one closure per functor, built by `table-functor`, is the
  author's expectation, mirroring the dispatcher).
- How the consume loop delivers `:exit` ports: through `%ported-call`
  as today (the entry is the wrapped function), nothing extra.
- Whether `tb-with-functors` is lifted into a shared test helper file
  or duplicated; two copies in two files is acceptable, a third is not.
- The manual section's worked example (the ring, both rule shapes, the
  trace of a consumer hit).
