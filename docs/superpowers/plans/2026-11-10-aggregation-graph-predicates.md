# Aggregation and Graph Predicates Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `aggregate-all/3` (count, sum, min, max, bag, set, witness
forms) admissible in guarded text in its static form; a tabled
`reachable/3` over one edge type and a level-BFS `k-hop/4`; the
existing `shortest-path/4` measured, not rebuilt; before/after on odm.

**Architecture:** A compiler macro expands a static `aggregate-all`
inline with an internal `%aggregate-step` goal threaded through the
goal's compile walk, under the negation floor; a runtime functor serves
the dynamic form. The guard gains positional entries (a literal spec
in position 1, a static goal in position 2). `reachable/3` is two `<-`
clauses asserted at load time in `graph-db` and tabled, guarded by an
internal `%closure-args/2`; `k-hop/4` is a Lisp functor. The rules
lane's standing classes and the gui inventory are updated with each
functor.

**Tech Stack:** Common Lisp (SBCL verified; `#+ecl`/`#+ccl` arms kept),
FiveAM, `tests/perf/prolog-bench.lisp`.

**Spec:** `docs/superpowers/specs/2026-11-10-aggregation-graph-predicates-design.md`
(D1 to D9 below refer to its sections).

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the
  spec section or issue (user CLAUDE.md).
- Test names and helpers are unique across `tests/`: aggregation tests
  prefixed `ag-`, graph-predicate tests `gp-`, rules tests `rl5-`; the
  shared helper `with-dropped-functors` goes in `tests/suite.lisp`; the
  `ci-`/`tb-` copies stay.
- The worktree is `.worktrees/aggregation` on branch `aggregation` off
  `experiment` 4d2fd2c; `tests/run-suite.sh SUITE` from the worktree.
- Suites that stay green at every commit: `tabling-suite` (85),
  `prolog-functor-suite` (93), `clause-indexing-suite` (51),
  `tracer-explain-suite` (60); before the hand-off also `guard-suite`
  (`tests/query/guard-tests.lisp`), the gui suite
  (`graph-db/gui-test`, for the inventory tripwire), rules (1147),
  query (122), concurrency (1680) through `tests/run-system-tests.sh`.
- Every source commit touches docs; the manual, notes and CHANGELOG in
  task 5; earlier commits carry docstrings and the spec reference.
- Measurement before code: task 1 posts the before rows on #123 before
  task 2 starts.
- `graph-db/perf-test` gains no dependency (D4).

## Review Focus

1. A **cut inside `aggregate-all`'s goal** (`(aggregate-all count (and
   (ag-v ?x) !) ?n)`): the barrier makes it local, count 1, the clause's
   remaining clauses still tried. Pinned in `ag-cut-inside-the-goal`
   (task 2).
2. **Nested aggregates and negation** (`(aggregate-all count (and (ag-v
   ?x) (not (aggregate-all count (ag-v ?y) 0))) ?n)`): accumulators are
   per-expansion gensyms and floors nest. Pinned in `ag-nests` (task 2).
3. **`reachable` from a vertex with no outgoing edge, and a self-loop**:
   no row, and one row (itself) respectively. Pinned in
   `gp-reachable-closure-on-a-cycle` (task 4).
4. **`k-hop` with K past the graph's reach, and K = 0**: no row, and
   the start itself. Pinned in `gp-k-hop-exact-and-distinct` (task 4).
5. **A schema edge type named like a spec word** (`count`, `set`) in
   guarded text: the spec position reads the literal word, the goal
   position resolves the edge functor; both work in one query. Pinned
   in `ag-guard-admits-static-and-refuses-dynamic` (task 3).

---

### Task 1: The bench's before rows

**Files:**
- Modify: `tests/perf/prolog-bench.lisp` (`bench-graph-predicates`,
  the case list)
- Modify: `tests/perf/run-bench.sh` (no new function names: the case
  is the existing `bench-graph-predicates`)
- Modify: `docs/perf-baselines.md`

**Interfaces:**
- Produces: in `bench-graph-predicates`, after the two existing rows:
  `reachable-closure-handwritten` (100 starts over the chords ring, a
  left-recursive tabled `pbp-lpath/2` asserted and tabled by
  `pbp-assert-lpath-clauses`, `:max-inferences 200000` per select,
  `timed-ops` label with ops the start count) and
  `aggregate-count-findall` (100 starts, `(findall ?c (and (p-knows ?s
  ?b) (p-knows ?b ?c)) ?l) (lisp ?n (length ?l))`); then, each only
  when its functor is registered (`(gethash 'graph-db::k-hop/4
  graph-db::*prolog-global-functors*)` etc.): `k-hop-3-predicate`,
  `reachable-predicate` (`(reachable ?s ?x p-knows)`),
  `aggregate-count-predicate` (`(aggregate-all count (and ...) ?n)`),
  `shortest-path-predicate` (`(shortest-path ?a ?b ?p ?c)` when
  `graph-db::shortest-path/4` is registered).

- [ ] **Step 1: Write the two before cases and the four conditional after cases**

The conditional selects must compile in an image without the
functors: build them as forms and `eval` them under `*package*`
`graph-db/perf-test` only when the functor is registered, as
`pbp-set-path-tabled` guards with `fboundp`.

- [ ] **Step 2: Smoke at small scale**

Run: `tests/perf/run-bench.sh bench-graph-predicates`
Expected: the four existing rows plus `reachable-closure-handwritten`
and `aggregate-count-findall`; no `*-predicate` rows.

- [ ] **Step 3: Third run on odm at normal scale, post the before**

Three runs, tag `before-u5`, `:only 'bench-graph-predicates`; the
third report to `tests/perf/results/prolog-before-u5-odm-<sha>.report`
(sha: the engine commit, 4d2fd2c); the six rows on #123 beside the
unit 1b rows; a `docs/perf-baselines.md` entry.

- [ ] **Step 4: Commit**

```bash
git add tests/perf/prolog-bench.lisp tests/perf/results/ docs/perf-baselines.md
git commit -m "perf: graph-predicate and aggregate before rows (#123)"
```

### Task 2: `aggregate-all/3`, static and dynamic

**Files:**
- Modify: `prologc.lisp` (`%aggregate-spec`, `%aggregate-result`, the
  `aggregate-all` and `%aggregate-step` compiler macros after `forall`)
- Modify: `prolog-functors.lisp` (`aggregate-all/3` after `setof/3`)
- Modify: `tests/suite.lisp` (`with-dropped-functors`)
- Create: `tests/aggregation-tests.lisp`; register in `graph-db.asd`
  after `tabling-tests`

**Interfaces:**
- Produces: `(%aggregate-spec spec)` → `(values kind template)` with
  KIND one of `:count :sum :min :max :bag :set :min-w :max-w` and
  TEMPLATE the term to collect (`t` for count; `(e w)` for the witness
  forms), or `prolog-error` with ball `(:domain-error :aggregate-spec
  SPEC)`; words matched by `symbol-name`.
  `(%aggregate-result kind items)` → `(values result ok)`: count the
  length, sum `+` over numbers (a non-number signals `prolog-error`
  with ball `(:type-error :number X)`), min/max by `<` with OK nil over
  no item, bag the list in solution order, set `%sort-unique`, the
  witness forms the first extreme's `(e w)`.
  Compiler macro `aggregate-all` on `(spec goal result)`: `:pass`
  unless `(static-goal-p goal)`; else expands per D1 with
  `(%aggregate-step ACC KIND TEMPLATE)` appended to GOAL inside the
  walk, ACC a gensym bound to `(list 0)` for count or `nil` otherwise.
  Compiler macro `%aggregate-step`: compiles `(compile-arg template
  bindings)` and emits the push or incf, returning nil.
  Functor `aggregate-all/3 (spec goal result cont)`: `%aggregate-spec`
  on the dereferenced spec, `(%findall-list template goal)`,
  `%aggregate-result`, unify, cont.
  `(with-dropped-functors ((name arity)...) body)` in `tests/suite.lisp`,
  the `ci-with-functors` shape.
- Consumes: `static-goal-p`, `compile-body`, `compile-arg`,
  `with-negation-floor`, `%findall-list`, `%sort-unique`, `deref-exp`,
  `undo-bindings`, `prolog-gensym`, `type-error-ball`, `iso-ball`.

- [ ] **Step 1: Write the failing tests**

`tests/aggregation-tests.lisp`, suite `aggregation-suite` in
`graph-db-suite`, under `with-test-graph` where a graph is needed:
`ag-count-sum-min-max`, `ag-bag-and-set`, `ag-witness-forms`,
`ag-leaves-no-bindings`, `ag-non-number-signals`,
`ag-template-bound-inside-the-goal`, `ag-spec-words-match-by-name`,
`ag-dynamic-goal`, `ag-static-form-compiles-inline` (port hook: no
`:call` for `AGGREGATE-ALL/3` on the static form, one on the dynamic),
`ag-under-effects-nil`, `ag-reads-tables-as-closed`,
`ag-cut-inside-the-goal`, `ag-nests`, each as the spec's Tests section
states them with its exact values.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-suite.sh aggregation-suite`
Expected: FAIL, `aggregate-all` unknown functor.

- [ ] **Step 3: Implement**

The validator and the result function first, then the runtime functor
(so the dynamic test passes), then the two compiler macros.

- [ ] **Step 4: Run to verify they pass**

Run: `tests/run-suite.sh aggregation-suite`, then `tabling-suite`,
`prolog-functor-suite`, `clause-indexing-suite`, `tracer-explain-suite`
Expected: PASS; 85, 93, 51, 60 unchanged.

- [ ] **Step 5: Mutation checks**

Replace `%aggregate-step` threading by a closing lambda (the
template read outside the walk): `ag-template-bound-inside-the-goal`
fails. Remove the floor: `ag-reads-tables-as-closed` fails. Make the
macro expand a non-static goal: `ag-dynamic-goal` fails to compile.
Restore each.

- [ ] **Step 6: Commit**

```bash
git add prologc.lisp prolog-functors.lisp tests/suite.lisp tests/aggregation-tests.lisp graph-db.asd
git commit -m "prolog: aggregate-all/3, inline when static (#123)"
```

### Task 3: The guard, explain and the rules lane for `aggregate-all`

**Files:**
- Modify: `query/guard.lisp` (`*prolog-excluded-predicates*` :291,
  `*prolog-goal-argument-control*` :311 and its loop at :550,
  `%routes-to-engine-control-p` :461's docstring)
- Modify: `prolog-explain.lisp` (an `aggregate-all` branch beside the
  `not` handling, ~:142)
- Modify: `prologc.lisp` (`%static-goal-functors` :1370 descends into
  `aggregate-all`'s goal)
- Modify: `rules/standing.lisp` (`("aggregate-all/3" . :meta)`;
  `%classify-goal` walks position 2)
- Modify: `tests/gui/gui-tests.lisp` (`*reviewed-functor-inventory*`
  :1345; `%added-functor-message`'s third case)
- Modify: `tests/query/guard-tests.lisp`, `tests/rules/forward-tests.lisp`,
  `tests/tracer-explain-tests.lisp`

**Interfaces:**
- Produces: `*prolog-goal-argument-control*` entries are a name or
  `(name :spec P :goal Q)`; `%guard-spec-term (x ctx)` admits a literal
  spec term (D1's grammar) and returns it with the words canonicalised
  to `graph-db`'s symbols, refusing anything else naming the position;
  `*prolog-excluded-predicates*` gains `"%AGGREGATE-STEP"` and
  `"%UNFLOOR"`.
- Consumes: task 2's names.

- [ ] **Step 1: Write the failing tests**

- `ag-guard-admits-static-and-refuses-dynamic` in
  `tests/query/guard-tests.lisp` (the spec's cases; `g-person` facts
  through `run-guarded-prolog` as that file does), plus Review Focus 5
  with an edge type named `count` asserted in the test schema if the
  fixture allows, else over the existing `g-knows` with a `(set ?x)`
  spec beside a `(g-knows ?a ?b)` goal.
- `rl5-aggregate-in-a-rule-body` in `tests/rules/forward-tests.lisp`:
  a rule whose body counts with `aggregate-all` derives through
  `run-rules`; `every-registered-functor-has-a-standing-query-class`
  stays green.
- `te-explain-aggregate-all` in `tests/tracer-explain-tests.lisp`: the
  spec's explain case.
- The gui tripwire: `prolog-functor-inventory-is-pinned` with
  `aggregate-all/3` on the positional list and the two internal
  macros excluded; `%added-functor-message` names the third case.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-suite.sh guard-suite`; `tests/rules/run-test.sh
rl5-aggregate-in-a-rule-body`; `tests/run-suite.sh tracer-explain-suite`;
`tests/run-system-tests.sh graph-db/gui-test`
Expected: the guard refuses `count` as "not a functor"; the rules
class test lists `aggregate-all/3`; the explain row marks `?x` bound;
the tripwire lists `aggregate-all/3`, `%aggregate-step`, `%unfloor`.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run to verify they pass**

Run: the four commands of step 2
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add query/guard.lisp prolog-explain.lisp prologc.lisp rules/standing.lisp tests/gui/gui-tests.lisp tests/query/guard-tests.lisp tests/rules/forward-tests.lisp tests/tracer-explain-tests.lisp
git commit -m "prolog: the guard, explain and rules lane admit aggregate-all (#123)"
```

### Task 4: `reachable/3` and `k-hop/4`

**Files:**
- Create: `prolog-graph-predicates.lisp` (`%closure-args/2`, `k-hop/4`,
  the two `reachable` clauses and `table-functor` at load); register in
  `graph-db.asd` after `prolog-functors` with `:depends-on
  ("prolog-functors" "prolog-tabling")`
- Modify: `package.lisp` (no new exports: the functors are reached by
  name through the registry, like `outgoing-edges/4`)
- Modify: `rules/standing.lisp` (`reachable/3`, `k-hop/4`
  `:store-read`; `%closure-args/2` `:pure`)
- Modify: `tests/gui/gui-tests.lisp` (the reviewed list)
- Create: `tests/graph-predicate-tests.lisp`; register after
  `aggregation-tests`
- Modify: `tests/rules/forward-tests.lisp` (`rl5-reachable-in-a-rule-body`,
  `rl5-k-hop-in-a-rule-body`)

**Interfaces:**
- Produces: `%closure-args/2 (from type cont)`: cont when FROM is a
  vertex and TYPE a symbol, else `prolog-error` with
  `(instantiation-error-ball)` and a reason naming `reachable/3`.
  `k-hop/4 (from k to type cont)`: K a non-negative integer and FROM a
  vertex (else `prolog-error`, instantiation or `(:type-error :integer
  K)` ball), TYPE a symbol (else fail); level BFS over `map-edges
  :vertex v :direction :out :edge-type type` with an `equalp` set of
  visited ids; each level-K vertex unified with TO once, cont, undo.
  `reachable/3`: the two clauses of D2, tabled at load.
- Consumes: `<-` (via `add-clause` with `*functor*` nil and `*package*`
  `graph-db`), `table-functor`, `outgoing-edges/4`, `map-edges`,
  `lookup-vertex`, `id`, `vertex-p`.

- [ ] **Step 1: Write the failing tests**

`tests/graph-predicate-tests.lisp`, suite `graph-predicate-suite`:
`gp-reachable-closure-on-a-cycle` (the spec's cases plus Review Focus
3: a lone person gives no row, a person with a `g-knows` to itself
gives one), `gp-reachable-needs-a-bound-start-and-type`,
`gp-reachable-composes`, `gp-k-hop-exact-and-distinct` (plus Review
Focus 4), `gp-k-hop-arguments`; `rl5-reachable-in-a-rule-body` and
`rl5-k-hop-in-a-rule-body` in the rules suite; the gui tripwire's
list gains the three names.

- [ ] **Step 2: Run them to verify they fail**

Run: `tests/run-suite.sh graph-predicate-suite`
Expected: FAIL, `reachable` and `k-hop` unknown functors.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run to verify they pass**

Run: `tests/run-suite.sh graph-predicate-suite`, `tabling-suite`,
`prolog-functor-suite`; `tests/rules/run-test.sh` on the two `rl5-`
tests and on `every-registered-functor-has-a-standing-query-class`;
`tests/run-system-tests.sh graph-db/gui-test`
Expected: PASS.

- [ ] **Step 5: Mutation checks**

Remove the visited set from `k-hop`: the distinct assertion fails.
Remove `%closure-args` from the first clause: the instantiation test
fails (an error of another class or no error). Restore.

- [ ] **Step 6: Commit**

```bash
git add prolog-graph-predicates.lisp graph-db.asd rules/standing.lisp tests/gui/gui-tests.lisp tests/graph-predicate-tests.lisp tests/rules/forward-tests.lisp
git commit -m "prolog: reachable/3 tabled and k-hop/4 (#123)"
```

### Task 5: Documentation, the after measurement, the spine row, the hand-off

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (Chapter 6: a row for
  `aggregate-all` in "Logic, Control, and Lisp Integration" with one
  example after the table, rows for `reachable` and `k-hop` in "Node
  and Edge Predicates" with a pointer to Chapter 14 for the algorithm
  predicates; the "Tabled predicates" section mentions `reachable/3`
  as the engine's own tabled predicate; Chapter 14: the `reachable/2`
  versus `/3` note; the guard section's list of what free text may use
  gains the three)
- Modify: `docs/codebase-notes.md` (one bullet: count with
  `aggregate-all`, close over with `reachable`, not `findall` plus
  `lisp`)
- Modify: `CHANGELOG.md` (`## [Unreleased]`, Added)
- Modify: `docs/perf-baselines.md`
- Add: `tests/perf/results/prolog-after-u5-odm-<sha>.report`

- [ ] **Step 1: Write the docs**

- [ ] **Step 2: Third run on odm, record, post**

Three runs at `:scale :normal`, `:only 'bench-graph-predicates`, after
`(ql:quickload "graph-db/algorithms")` in the same image, tag
`after-u5`; the third report kept; the before/after table on #123 with
all ten rows; the baselines entry.

- [ ] **Step 3: The spine row**

In the application's own image on odm (ma-dev; the remote-lisp skill,
read-only evaluation): a `select` with `(reachable ?s ?x TYPE)` from
the spine's root over its containment edge type, three runs, the third
on #123 as edge type, node count, row count and seconds, nothing
else. If the image cannot run it inside an hour, a comment deferring it
again with the reason.

- [ ] **Step 4: Whole-branch suites and commit**

Run: `tests/run-system-tests.sh graph-db/rules-test graph-db/query-test graph-db/concurrency-test graph-db/gui-test`
Expected: every system green.

```bash
git add docs/vivace-graph-v3-doc.org docs/codebase-notes.md CHANGELOG.md tests/perf/results/ docs/perf-baselines.md
git commit -m "prolog: aggregation and graph predicates measured; manual, notes, changelog (#123)"
```

- [ ] **Step 5: Hand-off**

The fresh whole-branch reviewer, fixes committed, then the `Ready for
review:` comment on #123 with `git diff experiment...aggregation`, the
suite counts, the mutation checks performed, and the method (inline).

---

## Self-review

- **Spec coverage:** D1 tasks 2 and 3; D2, D3 task 4; D4 tasks 1 and
  5; D5 task 2's floor test; D6 task 3's guard tests and task 4's
  inventory; D7 task 3's explain test, task 4's port behaviour by
  construction; D8 by construction; D9 tasks 3 and 4; Measurement
  tasks 1 and 5. Every test the spec lists has a task.
- **Step scan:** each code step names its signature, file and the
  spec's values; bodies left to the builder.
- **Type consistency:** `%aggregate-spec`/`%aggregate-result` are
  shared by the macro (task 2) and the functor (task 2) and consumed by
  the guard's grammar (task 3); `%closure-args/2` named once (task 4).
- **Review Focus:** five items, each pinned to a task's test.
- **Proportion:** shorter than the spec; no code bodies.
