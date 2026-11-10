# Tabling for `select`: design

**Issue:** GH #122 (unit 2 of its 2026-09-06 split), unit 4 of the
Prolog Engine 5.0 scope (`2026-11-09-prolog-engine-5.0-scope.md`).
**Status:** proposed 2026-11-10, revised the same day after a builder
check (its findings are settled in D2 to D5 below: component detection
with early completion replaces the coarse leader, the negation check
covers every construct that reads a table as closed, a follower's
evaluation unwinds like a leader's, the refusal is uncatchable); the
calls are the author's, Kevin's review pending. **Depends on:** units
2 (#121, the per-clause dispatcher) and 3 (#124, the port hook), both
on `experiment`.

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
- A tabled call terminates on a cyclic graph with the complete,
  duplicate-free answer set: the ring bench records `:complete` and 200
  solutions where it recorded `:stack-exhausted` and 0.
- Left-recursive and right-recursive forms of the same rule give the
  same answers; the left-recursive form runs in stack depth independent
  of the derivation length (D8).
- Answer tables live for one query (`select`, `?-`, `run-query-goals`,
  the rules layer's `%solutions`) and are dropped with it: no
  persistent tables, no cross-query staleness.
- Untabled predicates compile and run exactly as today: the tabled
  path is a different functor function, not a check in every call.
- A construct that reads a tabled predicate's answers as closed
  (negation, `if` with an else arm, `findall`, `bagof`, `setof`,
  `forall`) inside that predicate's own recursive component is refused
  with an uncatchable error, not answered wrongly (#122: "stratified
  negation enforced, not assumed"); the same construct over an
  independent tabled predicate works.
- `:max-inferences`, `:timeout`, `:limit`, `:callback`, `:count`, the
  port hook, `explain-goals` and `profile-goals` keep working through a
  tabled call.

## Non-goals

- SLG with suspension and resumption of consumers (true SLG). This is
  linear tabling: an incomplete table is re-evaluated, never resumed.
- Subsumptive tabling (answering `p(a,X)` from a table for `p(X,Y)`).
  Variant tabling only.
- Persistent or shared tables; tables across `:write` effects within
  one query (a `retract/1` after a tabled call reads stale answers; the
  guard's `:effects nil` queries cannot do this).
- Tabling a built-in or an edge functor directly. Wrap it in a `<-`
  rule and table that; unit 5 defines `reachable/3` that way.
- A static stratification check of the program. `run-rules` does one
  for rule sets (#333); for `select`, the refusal is at the call, and
  it is exact for the calls a query makes, not for the program.
- Magic sets: #486, against the rules layer, per the scope.
- Changing the compiler's stack behaviour for untabled recursion: its
  bound is documented, not lifted (D8).

## Today's mechanics

- A user functor is a `functor` struct (functor.lisp): `name fn clauses
  lock arity dispatcher clause-set`. Its `fn` is the dispatcher built
  once by `%make-dispatcher` (prologc.lisp:968), which walks the clause
  set's candidates for the call's first argument, undoes bindings
  between them and tail-calls the last. `%add-compiled-clause`
  (prologc.lisp:1040) installs the dispatcher as `fn` unless it already
  is; `prolog-compile` (:222) resets the functor through
  `prolog-compile-null` (:865, which sets `fn` directly) and re-adds
  every clause; `reset-functor` goes through it.
- `compile-call` (prologc.lisp:301) fetches `fn` at run time
  (`get-functor-fn`, else the global functor table), ticks the budget,
  and calls it with the arguments and the continuation, through
  `%ported-call` when the port hook is set. `%solve`
  (prolog-functors.lisp:247) does the same for a dynamic goal. So
  whatever a functor's `fn` is, every static and dynamic call reaches
  it: a tabled functor needs only a different `fn`.
- Two live query roots bind a fresh `*trail*`, `*var-counter*`, the
  budget specials and `*seen-table*`: `select` (prologc.lisp:1396) and
  `?-` (:1116); the two other `*trail*` sites are inside a `#| |#`
  block. `run-query-goals` (query/dsl.lisp:315) and the rules layer's
  `%solutions` both `eval` a `select` form.
- A solution is delivered by calling the continuation with the
  bindings in place; failure is a return. `undo-bindings` to a saved
  trail fill-pointer unbinds. `deref-copy` (prologc.lisp:831) copies a
  term with fresh variables; `deref-exp` dereferences in place.
- Constructs that read a goal's answers as closed: the `not` compiler
  macro (prologc.lisp:653) and `not/1` (prolog-functors.lisp:420);
  `forall` compiles to nested `not`, `forall/2` (:383) is its own loop;
  `if` with an else arm (:698, three-argument form; `if/3` :360) runs
  the else when the test has no solution; `findall/3`, `bagof/3`,
  `setof/3` (:523-560) collect every solution. `once`, two-argument
  `if` and the cut only prune: they never conclude anything from a
  goal having no further solution.
- `catch/3` (prolog-functors.lisp:321) declines
  `prolog-resource-error` and `prolog-permission-error` so a bounded
  query cannot swallow its own enforcement.
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

`%add-compiled-clause`, `prolog-compile` and `prolog-compile-null`
install `fn` through one helper, `%install-functor-fn`, which picks the
tabled entry when the slot is set and the dispatcher (or the null
function) otherwise; today's `(unless (eq fn dispatcher)
(set-functor-fn ...))` moves into it. The tabled entry calls the
functor's `dispatcher` slot directly, so the dispatcher is built even
for a tabled functor and `functor-dispatcher` keeps meaning "the clause
walk". A tabled functor with no clauses answers nothing, as an untabled
one does.

A global functor (`def-global-prolog-functor`) cannot be tabled:
`table-functor` signals a `prolog-error` naming it, as `add-clause`
does for a clause on one. The three functions are exported from
`graph-db`. There is no Prolog-side directive; the guard's query text
cannot declare a table, by design (the declaration is a schema-level
decision, like a view). `make-functor-symbol :define t` interns in the
run-time `*package*`, as `<-` does, so a declaration made under another
package tables a clause-less twin; the docstring and the manual say to
declare in the package the clauses were asserted in.

### D2. Tables, variant keys and answers

`*tables*` is a special, `defvar`ed in globals.lisp, bound by both
query roots (in the same `let*` as `*trail*`) to a fresh `equal` hash
table; its value outside a query is `nil`. A tabled call that finds
`*tables*` nil (a `%solve` from Lisp with no query around it) binds one
for its own extent, so a tabled predicate never runs without a table
and never leaks one past the query. The other per-query specials of D3
and D4 (`*table-stack*`, `*round*`, `*answers-added*`,
`*negation-floor*`) are bound at the roots beside it.

**Key.** The call's variant key is the dereferenced argument list with
each atom canonicalised so that `equal` on keys implies `prolog-equal`
on the arguments, and each unbound variable replaced by a marker
numbered by first occurrence, so `p(X,Y)`, `p(A,B)` share a key and
`p(X,X)` has another. The markers are conses headed by uninterned
symbols held in constants (`+var-tag+`, `+node-tag+`, `+ts-tag+`,
`+array-tag+`), so no user term can equal one:

| argument | key |
|---|---|
| unbound variable | `(+var-tag+ . i)`, i by first occurrence |
| number | its `rational` (1 and 1.0 are `prolog-equal`) |
| string | the string (`string=`) |
| symbol, character, `nil` | itself |
| node | `(+node-tag+ . id-as-list)` (nodes are equal by id) |
| timestamp | `(+ts-tag+ universal-time nanoseconds)` |
| array | `(+array-tag+ . contents-as-list)` |
| cons | the cons of the parts' keys |
| any other object | itself under `equal` |

A string and a keyword are `prolog-equal` case-insensitively but take
different keys: the two calls get two tables, each correct. Sharing
is a performance property; the soundness property is that one key
never serves two calls that could differ, and the table above keeps it.

**Table.** `(defstruct table key answers seen complete created-at
round index leader)`: `answers` an adjustable vector of answer tuples
in insertion order (so consumption is by index and sees answers added
while it runs); `seen` an `equal` hash of answer keys (canonicalised
as above, variables numbered) for duplicate elimination; `complete` a
boolean; `created-at` a per-query serial; `round` the global round
number (D3) at which this table was last evaluated; `index` its
position in `*table-stack*` while it is being evaluated; `leader` the
provisional leader (D3), `nil` for none.

**Answer.** An answer is the argument tuple as it stands when the
clause body succeeds: `deref-copy` of the argument list, so it holds
fresh variables where the answer is non-ground, and a flag `ground-p`
set when the copy contains none. Consuming an answer unifies each
argument with the stored term when `ground-p`, else with a
`deref-copy` of it (fresh variables per consumer), under the caller's
trail; bindings are undone between answers.

### D3. Evaluation: components, early completion, rounds

The tabled entry is one closure per functor, of `(args... cont)`. It
computes the key, looks the table up in `*tables*` and takes one of
the paths below. The per-query state: `*table-stack*`, the tables
whose evaluation is in progress, innermost first, rebound with `let`
around each evaluation (never pushed and popped, so any exit leaves it
right); `*round*`, a counter every leader round increments;
`*answers-added*`, a counter every stored answer increments.

**Evaluate** a table T means: bind `*table-stack*` to T on top, set
T's `index` to its depth and T's `round` to `*round*`, set T's
`leader` to `nil`, call the dispatcher with the original arguments and
the continuation *add-answer* (copy the tuple into T unless `seen` has
its key; on adding, bump `*answers-added*`; return `nil`, so the
dispatcher backtracks through every clause). While T evaluates, two
things lower T's `leader`:

- a **looping consumer** inside T's extent reading a table A that is
  on the stack: A is an ancestor; T's `leader` becomes the lower of
  its current `leader` and A (lower means smaller `index`);
- a child table C evaluated inside T's extent that ends incomplete:
  T's `leader` becomes the lower of its `leader` and C's `leader`.

When the dispatcher returns, T's `leader` is the lowest table on the
stack that T's evaluation depended on, or `nil`.

- **T depends on nothing below itself** (`leader` nil, or `leader` is
  T): T leads a component. Iterate: increment `*round*`, note
  `*answers-added*`, evaluate T again (which re-evaluates the stale
  tables under it, path 4 below), and repeat while `*answers-added*`
  moved. Then mark complete T and every incomplete table with
  `created-at` greater than T's: those were all created inside T's
  extent and depend on T (a table created there that depended on
  nothing below itself completed on its own, earlier). The first
  evaluation counts as round one, so a table whose first evaluation
  added nothing completes without a second.
- **T depends on an ancestor** (`leader` below T): T stays incomplete,
  its `leader` propagates to the table below it on the stack, and the
  ancestor's component will complete it.

The call paths:

1. **Complete table.** Consume: for each answer in order, `%tick`,
   unify, call `cont`, undo. Return.
2. **No table.** Create it (`created-at` next serial), evaluate as
   above, then consume into `cont`: all answers when T completed,
   the answers so far when it did not.
3. **Incomplete table on the stack** (an ancestor): a looping consumer.
   Lower the top table's `leader` to it, consume the answers so far
   into `cont`, return. New answers the ancestor finds later reach this
   call site in the component's next round.
4. **Incomplete table not on the stack:** if its `round` is `*round*`
   it was evaluated in this round; lower the top table's `leader` to
   its `leader` (it depends on what that table depended on) and consume.
   Otherwise re-evaluate it as in path 2 (it may now complete on its
   own if what it depended on has completed), then consume.

Consumption always reads the vector by index up to its length at each
step, so a consumer running during evaluation also delivers answers
that arrive while it runs; this is what makes the left-recursive ring
converge in two rounds (one that finds everything, one that finds
nothing new).

Soundness and completeness, for a definite program: every answer in a
table was derived by a clause from answers and facts, so it is a
consequence. Calls are monotone across rounds (tables only grow and
the dispatcher reruns the same clauses), so every table reached in one
round is reached in the next and re-evaluated at first touch; a leader
completes only after a round that stored nothing anywhere, so every
evaluation in that round read final tables and the component's tables
are closed under the clauses: the least fixpoint. A table completed
early depended on nothing still open, so its closure was already
final. Duplicate elimination is by variant, so two derivations of one
answer are one answer; solution order is first-derivation order and is
deterministic for one program and graph. A cut inside a tabled clause
breaks the monotonicity argument; D5 says what the table then means.

**Budget and deadline.** The clause bodies run through `compile-call`,
which ticks; a round that cannot finish inside `:max-inferences` or
`:timeout` signals `prolog-resource-error` out of the query, and D5's
unwind drops the tables. Consumption ticks once per answer delivered,
so a huge table under a small budget still trips it. For the record:
the right-recursive 200-ring under the bench's 200,000 budget costs
about 105,000 ticks over its rounds, so `:complete` fits with headroom.

### D4. Negation and stratification

A construct that concludes something from a goal having no (further)
solution reads the goal's tables as closed: `not` (compiler macro and
`not/1`), `forall` (both forms), `if` with an else arm (compiler macro
and `if/3`; the two-argument form only prunes), `findall/3`, `bagof/3`,
`setof/3`. Each binds `*negation-floor*` around the goal it reads to
the current depth of `*table-stack*` (the floor is the deepest ancestor
that is *outside* the construct). `once`, two-argument `if` and the
cut do not touch it.

A tabled call under a floor is sound exactly when the tables it reads
do not depend on an ancestor at or below the floor. So the entry
refuses, before consuming an incomplete table T (paths 3 and 4), when
T's lowest open dependency sits at or below the floor: for path 3, T
itself (its `index` is less than the floor); for path 4, the end of
T's `leader` chain, followed while the leader is on the stack (its
`index` is less than the floor). A table that completed on its own
(D3's early completion) is complete, never refused: an independent
predicate under `not`, or collected by `findall`, inside a recursive
one works, and a nested component that starts under the floor
completes under it and is consumed as complete.

The refusal is `prolog-unstratified-error`, a `prolog-error` subclass
with reason "tabled NAME/ARITY is read as closed inside its own
recursive component (unstratified)" and ball `(:permission-error
:negate :procedure NAME/ARITY)`. `catch/3` declines it as it declines
resource and permission errors, for the same reason: a guarded query
must not swallow the enforcement and go on deriving from the recovery
branch inside the round. The two builder-check programs the coarse
design answered wrongly are tests: `p(X) :- q(X), (p(X) -> fail ;
true)` spelled with three-argument `if`, and `p(a) :- findall(Y, p(Y),
[])`; both are refused.

### D5. Cut, commit, limits and non-local exits

- **A cut inside a tabled predicate's clause** commits that clause in
  that evaluation as today (unit 2, D3). The table is the predicate's
  answer set under its clauses with their cuts, with the first answer
  in insertion order stable across rounds; a clause a cut skips in a
  later round because an earlier clause now has an answer leaves its
  earlier-round answers in the table. The manual says a cut in a tabled
  clause means "prune this evaluation", not "the answer set as Prolog
  would stream it", and shows `once` on the consumer side as the way to
  take one answer.
- **A cut, `once`, `if` or `:limit` across a tabled call** (the consumer
  stops early) is a non-local exit through the entry during
  *consumption*: `return-from` a barrier block, or `throw
  :prolog-limit-reached`. A complete table stays complete. A consumer
  of an incomplete table (paths 3 and 4) leaving early does not change
  the table either; the component's next round does not depend on
  consumers finishing.
- **A non-local exit during an evaluation** (a `throw/1` caught by a
  `catch/3` outside the table, an error, the budget) leaves tables
  half-built. Every evaluation, leader or follower, is wrapped in
  `unwind-protect`: on an abnormal exit the table being evaluated and
  every incomplete table with a greater `created-at` are removed from
  `*tables*`, so a later call in the same query (after `catch/3`
  recovered) starts afresh instead of reading a half-built table or
  completing it later from a stale `round`. The builder check's program
  (`q` with a throwing middle clause, `p` catching it in one clause and
  calling `q` plain in another) is the test: `q`'s table must end
  `{1, 2}`, not `{1}`.
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
  args `(:complete n)` (answered from a complete table), `(:leader
  rounds)` (led a component to completion), `(:follower n)` (evaluated,
  left incomplete, n answers so far) or `(:consume n)` (read an
  incomplete table without evaluating), so a trace shows whether a call
  was answered from a table or evaluated it; `%print-port` prints it as
  `TABLE NAME/ARITY complete 12`. `profile-goals`' port `ecase`
  (prolog-explain.lisp:224) gains the branch and counts it under the
  functor as `tables`.
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
  (path ?b ?c))`, has one table per reachable node; each is evaluated
  inside its caller's evaluation, so the Lisp depth is the number of
  distinct calls on the stack at once: the ring's circumference, the
  chain's length. It terminates on a cycle where it did not, and its
  depth is bounded by the number of distinct nodes rather than by the
  number of paths, but a chain longer than the control stack can hold
  still exhausts it. The manual says so and shows the left-recursive
  form as the one to write for closure over large graphs.
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
  a global functor is refused; a tabled functor with no clauses answers
  nothing.
- `tb-ring-terminates`: a 6-node ring as asserted facts, right-recursive
  `path/2`, `(select (?x) (path a ?x))` returns the six nodes once each;
  untabled, the same query under `:max-inferences 5000` signals
  `prolog-resource-error` (the before, kept as the contrast).
- `tb-left-and-right-recursion-agree`: same facts, both rule shapes,
  same answer set; all-unbound call `(path ?x ?y)` returns the 36 pairs.
- `tb-mutual-recursion`: `even/odd` over a chain of successor facts,
  two tabled functors in one component; answers complete; the
  component's leader is one of them (`:leader` fires once).
- `tb-independent-subtable-completes-early`: `p` recursive over a
  ring, calling tabled `q` that depends on facts only: `q` fires
  `:leader` once and `:complete` thereafter, never `:follower`.
- `tb-nonground-answers`: a tabled predicate with a clause `(<- (p ?x
  ?x))`: `(p ?a ?b)` yields one answer binding the two together; two
  consumers get independent fresh variables.
- `tb-answers-dedupe-by-variant`: two derivations of one answer, one
  solution; `1` and `1.0` one answer; two node objects with one id one
  answer; a user term shaped like a key marker is its own answer.
- `tb-tables-scope-to-the-query`: a tabled predicate over facts; assert
  a new fact between two queries; the second query sees it. Inside one
  query, two calls with the same key evaluate once (count `:leader`
  events).
- `tb-negation-is-stratified-or-refused`: `(<- (p ?x) (q ?x) (not (r
  ?x)))` with `r` tabled and independent: answers correct; `(<- (p ?x)
  (not (p ?x)))` with `p` tabled: `prolog-unstratified-error`, and a
  `catch/3` around it does not take it; `forall` over an independent
  tabled predicate works; the stratified program the coarse design
  refused (a negated branch calling a variant the positive branch
  already opened) answers correctly.
- `tb-if-else-and-findall-read-as-closed`: the two builder-check
  programs (D4) are refused; `findall` over an independent tabled
  predicate inside a recursive one works; two-argument `if` and `once`
  over the recursive predicate itself are not refused.
- `tb-cut-and-once-across-a-table`: `(once (path a ?x))` returns one
  answer and a following `(path a ?y)` in the same query still returns
  all six (the table completed before the cut); `:limit 2` on a tabled
  query returns two.
- `tb-abandoned-tables-are-dropped`: the D5 program; `q`'s answers
  through the plain clause are `{1, 2}`.
- `tb-budget-trips-inside-a-round`: a tabled closure over a 50-node
  ring under `:max-inferences 100` signals `prolog-resource-error` out
  of the query.
- `tb-trace-and-explain-see-tables`: `with-prolog-ports` collects a
  `:table` event per tabled call; `explain-goals` prints `tabled` on the
  line; `profile-goals` reports the count.
- `tb-dynamic-call-is-tabled`: `(call ?g)` with `?g` bound to a tabled
  goal goes through the table (`:table` event fires).
- Mutation checks the plan names as its gate: removing the `round`
  check (path 4) must raise `tb-mutual-recursion`'s evaluation count;
  removing the `seen` check must break `tb-answers-dedupe-by-variant`;
  removing the `unwind-protect` must break
  `tb-abandoned-tables-are-dropped`; removing the floor binding in the
  `if` compiler macro must break `tb-if-else-and-findall-read-as-closed`.

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
  `tb-negation-is-stratified-or-refused`,
  `tb-if-else-and-findall-read-as-closed`.
- Measured against a real corpus on a named host, third run: the ring
  on odm; the spine row or its stated deferral.
- Full SBCL suite green on the post-merge run; the cross-impl matrix in
  its dedicated session.

## Left to the builder

- The exact struct and special names beyond those given; the tabled
  entry is one closure per functor, built by `%make-tabled-entry`,
  mirroring the dispatcher.
- How `*negation-floor*` is bound in the compiler macros (a `let` in
  the emitted code around the goal's compiled form) versus the dynamic
  functors (a `let` around the `call/1`); both are mechanical.
- Whether `tb-with-functors` is lifted into a shared test helper file
  or duplicated; two copies in two files is acceptable, a third is not.
- The manual section's worked example (the ring, both rule shapes, the
  trace of a consumer hit, the cut note of D5).
