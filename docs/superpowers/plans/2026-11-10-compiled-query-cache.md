# Compiled Query Cache Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A query shape seen twice through `run-query-goals` runs
compiled from its second execution, with literals lifted so one entry
serves every key, rows identical to the interpreted path, a bounded
thread-safe cache invalidated when a user functor's content changes,
and before/after rows on odm.

**Architecture:** `%canonical-query` renames variables and lifts
literals into a vector bound by an internal `%bind-literals` compiler
macro; the cached form reads limit, skip, budgets and the callback
from specials. `query/compiled-cache.lisp` keeps a locked LRU keyed on
`(generation . shape)` and compiles on the second hit; the engine
bumps `*functor-generation*` where a user functor's content changes.
`run-query-goals` consults it; the consumers are unchanged.

**Tech Stack:** Common Lisp (SBCL verified; `#+sbcl` compile, `eval`
elsewhere), bordeaux-threads, FiveAM, the perf harness.

**Spec:** `docs/superpowers/specs/2026-11-10-compiled-query-cache-design.md`
(D1 to D6 below refer to its sections).

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the
  spec section or issue (user CLAUDE.md).
- Specials bound by earlier-compiled files are `defvar`ed in
  `globals.lisp`: `*query-literals*`, `*query-run-limit*`,
  `*query-run-skip*`, `*functor-generation*` (engine trap, GH #92).
- Test names unique across `tests/`: prefix `cq-`; the query-test
  system's suite is `query-suite`, run through
  `tests/run-system-tests.sh graph-db/query-test` (133 checks today);
  one rules test through `tests/rules/run-test.sh`.
- Suites that stay green at every commit: `graph-db/query-test`,
  `prolog-functor-suite` (93), `tabling-suite` (85), `aggregation-suite`
  (41), `clause-indexing-suite` (51); before the hand-off also
  `graph-db/rules-test` (1151), `graph-db/gui-test` (708; the
  `%bind-literals` exclusion changes the control table),
  `graph-db/concurrency-test` (1680).
- The worktree is `.worktrees/compiled-cache` on branch
  `compiled-cache` off `experiment` 0729954.
- Measurement before code: task 1 posts the before rows on #376 before
  task 2 starts.
- `*query-params*` and `param/2` are untouched (builder check 2).

## Review Focus

1. **A shape whose first and second calls differ only in a literal's
   type** (`"3"` then `3`): both are lifted, one entry, each run
   correct. Pinned in `cq-same-rows-either-way` (task 3).
2. **A compiled shape run while `*functor-generation*` moves between
   lookup and execution** (another thread asserts): the execution
   already in flight is correct (functors are fetched at run time);
   the next lookup misses. Pinned in `cq-generation-moves` (task 3).
3. **A `select` error inside a compiled run** (budget, unstratified,
   unknown functor at run time): the cache entry survives and the next
   run behaves the same. Pinned in `cq-budget-trips-compiled` (task 3).
4. **The rules' `rule-delta` variant goals** (one shape per variant,
   repeated every round): compiled once each, correct derivations.
   Pinned in `cq-rules-rounds-use-the-cache` (task 4).
5. **A query with no literal and no variable** (`(is-a ?x qt-item)`
   with `?x` only): no `%bind-literals` goal, still cached. Pinned in
   `cq-canonical-shape` (task 2).

---

### Task 1: The before rows

**Files:**
- Modify: `tests/perf/rules-bench.lisp` (new `bench-rules-rerun`, in
  its case list)
- Modify: `docs/perf-baselines.md`

**Interfaces:**
- Produces: `bench-rules-rerun`: the forward-step fixture's store at
  `(scale 2000)` claims with one rule written through `write-rule`'s
  equivalent in the bench, `run-rule` `(scale 20)` times, `timed-ops
  ("rules-rerun" k)`; recorded beside `forward-step-*`.

- [ ] **Step 1: Write the case and smoke it**

Run: the rules bench's runner at small scale (see `rules-bench.lisp`'s
header for the call; `tests/perf/run-bench.sh` is Prolog-only, so a
one-line `sbcl --eval` as unit 5's task 1 did)
Expected: a `rules-rerun` row.

- [ ] **Step 2: Third run on odm at normal scale, post the before**

Three runs of `bench-guarded-shape-repeat` (`run-prolog-bench :only`),
`bench-claim-linking` (spacetime-bench, for `two-hop-related`) and
`bench-rules-rerun`, tag `before-u6`; the third reports kept as
`tests/perf/results/{prolog,spacetime,rules}-before-u6-odm-0729954.report`;
the three rows on #376 beside the unit 1b row; a baselines entry.

- [ ] **Step 3: Commit**

```bash
git add tests/perf/rules-bench.lisp tests/perf/results/ docs/perf-baselines.md
git commit -m "perf: rules re-run case and the unit 6 before rows (#376)"
```

### Task 2: The canonical shape and the literal binding

**Files:**
- Modify: `globals.lisp` (`*query-literals*`, `*query-run-limit*`,
  `*query-run-skip*`, `*functor-generation*`)
- Modify: `prologc.lisp` (`%bind-literals` compiler macro after
  `%unfloor`)
- Create: `query/compiled-cache.lisp` (`%canonical-query`,
  `%cached-query-form`); register in `graph-db/query` after `dsl`,
  with `guard` depending on it or the system `:serial t` as it is
- Modify: `query/dsl.lisp` (`run-query-goals` evaluates the canonical
  form, interpreted, with the specials bound: no cache yet)
- Modify: `query/guard.lisp` (`*prolog-excluded-predicates*` gains
  `"%BIND-LITERALS"`)
- Modify: `tests/gui/gui-tests.lisp` if the inventory tripwire sees a
  change (it should not: a compiler macro is not a functor)
- Create: `tests/query/compiled-cache-tests.lisp`; register in
  `graph-db/query-test` after `guard-tests`

**Interfaces:**
- Produces: `(%canonical-query vars goals)` → `(values shape
  literals)` per D1, SHAPE `(canonical-vars . canonical-goals)` with
  `(%lit . i)` markers, LITERALS a simple-vector;
  `(%cached-query-form shape)` → the `select` form per D1 and D2 with
  `(%bind-literals ?L0 ...)` first when LITERALS is non-empty;
  compiler macro `%bind-literals` emitting
  `(when (unify ?Li (svref *query-literals* i)) ...)` nested over the
  body.
- Consumes: `replace-?-vars` (not called: `?` is kept), `compile-body`,
  `unify`, `static-goal-p`.

- [ ] **Step 1: Write the failing tests**

`cq-canonical-shape` (the spec's cases, plus Review Focus 5),
`cq-form-binds-literals` (the form of a two-literal shape starts with
`%bind-literals` over `?L0 ?L1`, and a literal-free shape's does not),
`cq-interpreted-canonical-rows` (a battery of guarded texts over the
`qt-item` fixture returns the same rows through `run-query-goals` as
before the change: compare against `select` run directly from Lisp
with the literals in place).

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-system-tests.sh graph-db/query-test`
Expected: FAIL, `%canonical-query` undefined.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run to verify they pass**

Run: `tests/run-system-tests.sh graph-db/query-test`;
`tests/run-suite.sh prolog-functor-suite`; `aggregation-suite`
Expected: PASS; 93 and 41 unchanged.

- [ ] **Step 5: Mutation checks**

Rename variables only in `vars` (not in goals): `cq-canonical-shape`'s
"same shape" assertion fails. Lift inside the `aggregate-all` spec: the
spec assertion fails. Emit `%bind-literals` as a functor call through
`compile-call`: `cq-no-extra-tick` (task 3) will catch it; here note it.

- [ ] **Step 6: Commit**

```bash
git add globals.lisp prologc.lisp query/compiled-cache.lisp query/dsl.lisp query/guard.lisp graph-db.asd tests/query/compiled-cache-tests.lisp
git commit -m "query: canonical shapes with lifted literals, bound by %bind-literals (#376)"
```

### Task 3: The cache, the generation, the consumers

**Files:**
- Modify: `query/compiled-cache.lisp` (the struct, the table, the
  lock, `%cached-query-function`, `clear-query-cache`,
  `query-cache-stats`, `*compiled-query-cache-size*`,
  `*compile-query-after*`)
- Modify: `query/dsl.lisp` (`run-query-goals` calls
  `%cached-query-function` and `funcall`s or `eval`s)
- Modify: `prologc.lisp` (`%add-compiled-clause` bumps), `functor.lisp`
  (`reset-functor`, `delete-functor` with content), `prolog-tabling.lisp`
  (`table-functor`, `untable-functor`), `prolog-functors.lisp`
  (`%install-edge-functors`)
- Modify: `query/package.lisp` (export `clear-query-cache`,
  `query-cache-stats`, the two knobs)
- Modify: `tests/query/compiled-cache-tests.lisp`

**Interfaces:**
- Produces: `(%cached-query-function shape)` → a function or `nil`
  per D3; `(clear-query-cache)`; `(query-cache-stats)` → the D3 plist;
  `*functor-generation*` moved at the six sites of D3.
- Consumes: task 2's names; `bt:make-lock`, `bt:with-lock-held`.

- [ ] **Step 1: Write the failing tests**

`cq-second-hit-compiles`, `cq-same-rows-either-way` (plus Review Focus
1), `cq-options-are-per-call`, `cq-budget-trips-compiled` (plus Review
Focus 3: after the trip, a fourth run under the normal budget answers),
`cq-effects-nil-holds-compiled`, `cq-generation-moves` (plus Review
Focus 2 with a thread asserting between a lookup and the run, if it
can be arranged without a hook; else the sequential form),
`cq-bounded-and-lru`, `cq-two-threads-one-shape`, `cq-clear`,
`cq-param-still-works`, `cq-no-extra-tick`.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-system-tests.sh graph-db/query-test`
Expected: FAIL, `query-cache-stats` undefined.

- [ ] **Step 3: Implement**

The compile step is `#+sbcl (compile nil ...)`, `#-sbcl (eval ...)`
(D6); the LRU a clock counter with a scan at eviction.

- [ ] **Step 4: Run to verify they pass**

Run: `tests/run-system-tests.sh graph-db/query-test`;
`tests/run-suite.sh tabling-suite`, `clause-indexing-suite`,
`prolog-functor-suite`; `tests/run-system-tests.sh graph-db/gui-test`
Expected: PASS; 85, 51, 93, 708 unchanged.

- [ ] **Step 5: Mutation checks**

Bake the first literal instead of lifting (`cq-second-hit-compiles`
returns the first key's rows for the second). Leave the generation out
of the key (`cq-generation-moves` returns the old rows). Remove the
lock and put a `(sleep 0.05)` in the compile step
(`cq-two-threads-one-shape` sees two compiles). Restore each.

- [ ] **Step 6: Commit**

```bash
git add query/compiled-cache.lisp query/dsl.lisp query/package.lisp prologc.lisp functor.lisp prolog-tabling.lisp prolog-functors.lisp tests/query/compiled-cache-tests.lisp
git commit -m "query: the compiled query cache, bounded and generation-keyed (#376)"
```

### Task 4: The rules lane, documentation, the after measurement, the hand-off

**Files:**
- Modify: `tests/rules/forward-tests.lisp` (`cq-rules-rounds-use-the-cache`)
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 6, after "Watching
  and explaining a query": a section "The compiled query cache")
- Modify: `docs/codebase-notes.md` (one bullet: repeated shapes are
  cached; literals, not shapes, vary; what moves the generation)
- Modify: `CHANGELOG.md` (`## [Unreleased]`, Added)
- Modify: `docs/perf-baselines.md`
- Add: the three after reports

- [ ] **Step 1: The rules test**

`cq-rules-rounds-use-the-cache` per the spec; run through
`tests/rules/run-test.sh`.
Expected: on the task 3 engine it passes (the cache is already in);
its RED is `clear-query-cache` before the second run making the
stats show two compiles instead of one, as a mutation.

- [ ] **Step 2: Docs**

- [ ] **Step 3: Third run on odm, record, post**

The three cases, tag `after-u6`, third reports kept; the before/after
table on #376; the baselines entry.

- [ ] **Step 4: Whole-branch suites and commit**

Run: `tests/run-system-tests.sh graph-db/rules-test graph-db/query-test graph-db/concurrency-test graph-db/gui-test`
Expected: every system green.

```bash
git add tests/rules/forward-tests.lisp docs/vivace-graph-v3-doc.org docs/codebase-notes.md CHANGELOG.md tests/perf/results/ docs/perf-baselines.md
git commit -m "query: compiled query cache measured; manual, notes, changelog (#376)"
```

- [ ] **Step 5: Hand-off**

The fresh whole-branch reviewer, fixes committed, then the `Ready for
review:` comment on #376 with `git diff experiment...compiled-cache`,
the suite counts, the mutation checks performed, and the method.

---

## Self-review

- **Spec coverage:** D1, D2 task 2; D3 task 3; D4 task 3's budget and
  effects tests; D5 tasks 3 (guarded, DSL through `run-query-goals`)
  and 4 (rules); D6 task 3's compile step; Measurement tasks 1 and 4.
  Every test the spec lists has a task.
- **Step scan:** each code step names its signature, file and values;
  bodies are the builder's.
- **Type consistency:** `shape`/`literals` from task 2 are what task 3
  keys and binds; `%bind-literals` named once; the four specials named
  in the Global Constraints and used by tasks 2 and 3.
- **Review Focus:** five items, each pinned to a task's test.
- **Proportion:** shorter than the spec; no code bodies.
