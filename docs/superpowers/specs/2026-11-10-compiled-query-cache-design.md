# Compiled query cache: design

**Issue:** GH #376; unit 6 of the Prolog Engine 5.0 scope
(`2026-11-09-prolog-engine-5.0-scope.md`). **Status:** proposed
2026-11-10; builder check pending; the calls are the author's, Kevin's
review pending. **Depends on:** units 1 to 5 on `experiment` (0729954);
#373 (the interpreter switch this replaces for repeated shapes).

## Intent

Every client query reaches the engine through `run-query-goals`
(query/dsl.lisp), which builds a `(select ...)` form and `eval`s it.
#373 measured a native compile of that form at 18 to 20 ms, most of a
small query's time, and switched the `eval` to SBCL's interpreter: a
two-hop read went from about 55 to 700 to 1,100 queries per second.
Interpreted solving is still about twice as slow as compiled solving,
and the hot shapes repeat: a consumer's fixed templates with changing
keys, a rule's body re-run every fixpoint round, the blackboard's
reads. This unit compiles a shape once and runs it compiled from then
on: a bounded per-image cache of compiled `select` functions keyed on
the query's shape, with the literals lifted out so `(find-by-slot ?s
pb-thing thing-id "t-3")` and `... "t-4"` are one entry.

## Goals

- A query shape seen twice through `run-query-goals` runs compiled from
  its second execution; the first runs interpreted as today. Rows,
  order, budgets, limits, errors and effects are identical between the
  two paths: the cache is invisible except in time.
- Literals (strings, numbers, characters) in goal arguments are lifted
  into a per-execution parameter vector; variables are renamed by first
  appearance; the shape plus the per-image functor generation is the
  key. Options that vary per call (`:limit`, `:skip`, the budgets, the
  callback) are read from specials at run time, never baked.
- The cache is bounded (default 256 entries, least recently used
  evicted), thread-safe, and emptied by `clear-query-cache`; an entry
  compiled before a user functor was asserted, reset, deleted, tabled
  or untabled, or an edge type's functors installed, is never used
  again (the generation moves).
- Consumers get it without a change: `run-guarded-prolog` and
  `run-guarded-goals` (the GUI, the blackboard), the JSON pattern DSL,
  and `graph-db/rules`' `%solutions` (every round of `run-rule` and the
  forward step).
- Measured on odm, third run: `guarded-shape-repeat` (the unit 1b case,
  1,208/s before), `two-hop-related` (spacetime-bench, the consumer's
  read), and a new rules re-run case, before and after.

## Non-goals

- Caching across images (a persistent compiled cache), or across
  `*type-registry*` bindings beyond what the generation covers.
- Background compilation on another thread: the second hit compiles
  synchronously (about 20 ms once per shape); the threshold is a
  parameter so a later measurement can move it.
- Attaching explain plans to cache entries (#124's note); `explain-text`
  and `profile-text` keep their own paths.
- Changing `select`'s expansion or the compiler's calling convention;
  the cached function is the same `select` form compiled instead of
  interpreted.
- Lifting literals out of `aggregate-all`'s spec position, out of a
  `lisp`/`lispp`/`is` expression, or out of option plists: a spec word
  is structural and the guard refuses the eval functors anyway.

## Today's mechanics

- `run-query-goals (vars goals graph &key package limit skip format
  callback)` binds `*graph*`, `*package*`, computes the row cap, and in
  a lambda `(eval `(select (:effects nil :snapshot t :limit ,cap :skip
  ,skip :max-inferences ,*query-default-max-inferences* :timeout
  ,*query-default-timeout* :callback *pattern-query-callback*) ,vars
  ,@goals))` under `sb-ext:*evaluator-mode* :interpret`; the callback
  reaches the form through the special `*pattern-query-callback*`
  (query/dsl.lisp:315-371).
- `select`'s option forms are evaluated at run time inside its
  expansion (`(*select-limit* ,(cdr (assoc :limit options)))` in the
  `let*`), so a form naming a special reads the special when the query
  runs; the options plist is read at macroexpansion time, so the
  *forms* must be there, not the values.
- `select` creates and deletes a transient functor per execution
  (`prolog-gensym`, `make-functor`, `set-functor-fn`, `delete-functor`,
  `release-prolog-symbol`); its name symbol is uninterned.
- `compile-call` resolves a goal head to a functor symbol at compile
  time (`make-functor-symbol`, by registry lookup, falling back to
  `*package*`) and fetches the functor's function at run time, so a
  compiled form survives the functor's redefinition; the guard
  canonicalises every head and schema name before `run-query-goals`
  sees them (GH #322), so the symbols a compiled form names are
  stable. Variables from the guard are symbols of a scratch package
  deleted after the call (`run-guarded-prolog`) or uninterned
  (`guard-query-text`, the rules' path): a cache key must not hold
  them.
- `claim/7`'s route is chosen at run time from which arguments are
  bound (`%claim-route`, rules/facts.lisp:305); `%fuse-key-bounds`
  rewrites `claim/7` followed by a `>` on a key *variable* at
  macroexpansion time and carries the bound's argument through, bound
  or not. First-argument indexing on a bound-at-run-time argument is
  "by value at run time" (clause indexing design D4). So a literal
  that becomes a variable bound at run time changes no route.
- `%install-functor-fn` is the one site that sets a user functor's
  function (unit 4); `delete-functor` unregisters; `%install-edge-
  functors` registers an edge type's `/2` and `/3` (prolog-functors
  .lisp:1223); `set-functor-fn` is the query roots' own install and a
  test hook.
- Benches: `bench-guarded-shape-repeat` (tests/perf/prolog-bench.lisp,
  500 `run-guarded-prolog` calls of one shape with a changing key),
  `two-hop-related` in `bench-claim-linking` (spacetime-bench.lisp:150),
  `bench-forward-step-reads` in rules-bench.lisp; no bench re-runs a
  rule.

## Design

### D1. The canonical shape and the parameter vector

`(%canonical-query vars goals)` → `(values shape params)`.

- **Variables** are renamed by first appearance across `vars` then
  `goals`: the n-th distinct variable becomes the symbol `?Vn` of
  `graph-db`. `?` (anonymous) stays `?` (the compiler gensyms it).
- **Literals**: a string, number or character in a goal argument
  (walked into nested lists, which is how a compound term is written)
  becomes the next parameter: the shape holds `(%param . i)` and
  `params` gets the literal at index i. Two equal literals are two
  parameters (position, not value, is the shape). Symbols, keywords,
  `nil` and `t` stay in the shape: they are schema names, functor
  names, slot names and spec words, canonical since the guard.
- **Not walked:** argument 1 of an `aggregate-all` goal (the spec
  term), and the whole argument list of `lisp`, `lispp` and `is` goals
  (an expression, not a term). Everything else is walked by the same
  rule, control constructs included (`not`, `and`, `if`, `forall`,
  `once`, `aggregate-all`'s goal).
- The shape is an `equal`-comparable tree of symbols, `(%param . i)`
  conses and `?` ; `vars` canonicalised the same way is its head, so
  two queries with the same goals and different projections are two
  entries.

The cached form is the `select` over the canonical goals with the
parameters bound first: `(select (OPTIONS) (?V0 ...) (%bind-params
?P0 ... ?Pk) ,@canonical-goals)` where each `(%param . i)` in a goal
was replaced by the variable `?Pi` (after the `?V`s in the renaming,
so a parameter variable never collides), and `%bind-params/N` is a
family of internal global functors, or one functor over a list, that
unifies each `?Pi` with `(svref *query-params* i)`. From then on the
compiled code sees `?Pi` as an ordinary variable bound at run time:
`(= ?x ?P0)` unifies, `(> ?x ?P0)` compares, `(find-by-slot ?s pb-thing
thing-id ?P0)` looks up by value, `claim/7` routes by bound-ness. A
goal list with no literal has no `%bind-params` goal.

### D2. Options read at run time

The cached form's options name specials: `(:effects nil :snapshot t
:limit *query-run-limit* :skip *query-run-skip* :max-inferences
*query-default-max-inferences* :timeout *query-default-timeout*
:callback *pattern-query-callback*)`. `run-query-goals` binds
`*query-run-limit*` to the cap and `*query-run-skip*` to the skip (two
new specials in globals.lisp) around both paths, and the two budget
specials are already what it reads. So one entry serves every limit,
offset and budget; the interpreted first run uses the same form, so
the two paths are one form evaluated two ways.

### D3. The cache

`query/compiled-cache.lisp` (after dsl.lisp's dependencies; `run-query-
goals` calls into it):

- `*compiled-query-cache*`: an `equal` hash from `(generation . shape)`
  to an entry `(defstruct (cached-query) shape fn hits last-used
  compiling)`; `*compiled-query-cache-size*` 256;
  `*compile-query-after*` 2 (compile when a shape's hit count reaches
  it); `*compiled-query-cache-lock*` (bordeaux); a monotone `*cache-
  clock*` for recency.
- `(%cached-query-function shape)` → the compiled function or `nil`:
  under the lock, find or create the entry, bump `hits` and `last-used`,
  evict the least recently used when the table is over size; when
  `hits` reaches the threshold and `fn` is `nil` and nobody is
  compiling it, mark `compiling` and compile *outside the lock*
  (`(compile nil `(lambda () ,form))` under `*package*` `graph-db`),
  then store `fn` under the lock. A second thread that finds
  `compiling` set runs interpreted this time. Returns `fn` when set.
- `run-query-goals`: canonicalise; bind `*query-params*` to the
  vector, `*query-run-limit*`, `*query-run-skip*`; if the cache gives a
  function, `funcall` it; else `eval` the canonical form under the
  interpreter, as today. `%cached-query-function` is called with the
  shape only; the form is built from the shape on demand.
- `*functor-generation*` (globals.lisp, a fixnum): incremented by
  `%install-functor-fn` and `delete-functor` when the functor's name
  symbol is interned (the query roots' transient functors are
  uninterned and must not move it), by `%install-edge-functors`, and
  by `set-functor-fn` on an interned name. The key carries it, so a
  stale entry is simply never found again and ages out. `clear-query-
  cache` empties the table. `query-cache-stats` → a plist `(:entries n
  :compiled n :hits n :misses n :compiles n :evictions n)` for tests and
  the GUI's stats page later.
- Correctness does not depend on the generation (a compiled form looks
  every functor up at run time); it exists so a redefined functor's
  entry is not kept warm forever and so a reviewer can reason about
  staleness in one sentence.

### D4. Errors, budgets, effects, snapshots

The compiled function is the same form: `:effects nil` refuses the same
effects, `:snapshot t` opens the same snapshot per execution, the
budgets trip through `%tick` identically, `catch/3` and the resource,
permission and unstratified errors propagate identically, tabling's
tables are bound fresh per execution inside `select`. A compile error
(a form the compiler rejects that the interpreter accepted: none is
known) is logged and the entry marked `fn` `:failed`, so the shape
stays interpreted. The guard still runs per request before the cache
is consulted; the cache never sees text.

### D5. Consumers

- `run-guarded-prolog` / `run-guarded-goals` / `%guarded-rows`: through
  `run-query-goals`; nothing changes in them.
- The JSON pattern DSL: `run-pattern-query` → `run-query-goals`.
- `graph-db/rules` `%solutions`: through `run-query-goals` with the
  rule's goals (and the fixpoint's `rule-delta` variants, each its own
  shape that repeats every round). A rule run twice compiles its shape
  on the second `%solutions`, and every later round and run uses it.
- `profile-goals` evaluates its own form (it needs the hook around the
  whole run); unchanged. `explain-goals` runs nothing; unchanged.

### D6. Cross-implementation

`compile nil` is standard; the interpreter switch is `#+sbcl` as
today, and on ECL/CCL the "interpreted" first run is whatever `eval`
does there (ECL compiles `eval`'d lambdas; the cache then saves the
repeated compile). The lock is bordeaux-threads. The `equal` hash and
the struct have no per-implementation arms.

## Tests

`tests/query/compiled-cache-tests.lisp` in the `graph-db/query-test`
system (the runner is `run-query-goals`), suite `query-suite`, names
prefixed `cq-`:

- `cq-canonical-shape`: `(find-by-slot ?s qt-item label "a")` and the
  same with `"b"` canonicalise to one shape with one parameter; a
  different projection, a different goal order, a symbol in place of
  the string, and `(?x ?y)` versus `(?y ?x)` are different shapes;
  `?` stays `?`; nested lists are walked; `aggregate-all`'s spec
  `(sum ?r)` and a `lisp` expression are not lifted; two equal literals
  are two parameters.
- `cq-second-hit-compiles`: `query-cache-stats` before and after three
  `run-guarded-prolog` calls of one shape with three keys: one entry,
  one compile, hits 3; rows identical each time and identical to the
  interpreted first call.
- `cq-same-rows-either-way`: a battery over the query fixture
  (`qt-item`, `qt-links`): a literal in `=`, in `>`, in `find-by-slot`,
  a two-hop edge walk, `aggregate-all count` with a literal in its
  goal, `(sum ?r)`, `not` over a goal with a literal, `k-hop` with a
  literal K, `once`, a `:limit` and `:offset` page: for each, the rows
  from the first (interpreted) and the third (compiled) call are
  `equal`.
- `cq-options-are-per-call`: one shape, `:limit 1` then `:limit 3`
  then `:offset 1`: one entry, the right rows each time.
- `cq-budget-trips-compiled`: a shape run three times, the third under
  `:max-inferences 5`, signals `prolog-resource-error`.
- `cq-effects-nil-holds-compiled`: a shape whose third run is through
  the compiled path still refuses nothing it should accept and accepts
  nothing it should refuse (a pure read answers; the guard refuses
  `retract` before the cache either way).
- `cq-generation-moves`: a cached shape over an asserted functor; `<-`
  another clause; the next run returns the new clause's rows and the
  stats show a second entry (the old generation's entry is not
  reused); `table-functor`, `untable-functor`, `delete-functor` and
  `def-edge` each move the generation; a `select` from Lisp (a
  transient root functor) does not.
- `cq-bounded-and-lru`: `*compiled-query-cache-size*` bound to 3,
  four shapes, the first evicted; touching the second before the fourth
  keeps it.
- `cq-two-threads-one-shape`: two threads run the same shape ten times
  each; both get the right rows; stats show one compile.
- `cq-rules-rounds-use-the-cache` (tests/rules): `run-rule` twice on a
  seeded graph; the stats show the rule's shape compiled once and hit
  on the second run; the report's derived counts match the uncached
  run (`clear-query-cache` between the baseline and the measured runs).
- `cq-clear`: `clear-query-cache` empties the stats.
- Mutation checks: the parameter lifting replaced by baking the first
  literal (`cq-second-hit-compiles` returns the first key's rows for
  the second key); the generation left out of the key
  (`cq-generation-moves` returns the old rows); the lock removed and
  the two-thread test run under `--dynamic-space-size` with a yield in
  the compile step (the compile count exceeds one or a hash-table error
  surfaces); the `aggregate-all` spec exclusion removed (the
  canonicalisation test's spec assertion fails).

## Measurement

Before, on odm at the pre-unit commit (task 1): `guarded-shape-repeat`
re-run (1,475/s at unit 3's run), `two-hop-related` from
`bench-claim-linking` (its spacetime-bench baseline), and a new
`rules-rerun` case in rules-bench.lisp: one rule over the forward-step
fixture's claims run `(scale 20)` times through `run-rule`, ops per
second. After: the same three, third run on odm, both tables on #376,
`docs/perf-baselines.md` entry. Expected: the shape-repeat and two-hop
rows move toward the compiled solving rate (#373's note: the Lisp edge
path at about 8,000/s minus guard overhead); the rules re-run row shows
the rounds' repeated shapes compiled once.

## Acceptance (#376)

- A repeated shape runs compiled from its second execution with rows,
  order, budgets, limits and errors identical to the interpreted path
  (the `cq-` tests).
- Literals lifted: one entry per shape, not per key (`cq-canonical-
  shape`, `cq-second-hit-compiles`).
- Bounded, invalidated on functor redefinition and edge-type
  installation, thread-safe (`cq-bounded-and-lru`, `cq-generation-
  moves`, `cq-two-threads-one-shape`).
- The three consumers covered (`run-guarded-prolog`, the DSL through
  `run-query-goals`, the rules' `%solutions`).
- Before/after on odm, third run; full SBCL suite green on the
  post-merge run.

## Left to the builder

- Whether `%bind-params` is one functor over a list or a family by
  arity; the exact struct and special names beyond those given.
- The LRU bookkeeping (a clock counter and a scan at eviction is fine
  at 256 entries).
- Where `query-cache-stats` counts hits and misses (the lookup).
- The manual's section (Chapter 6, after "Watching and explaining a
  query": what is cached, what moves the generation, the knobs,
  `clear-query-cache` and the stats) and the codebase-notes bullet.
