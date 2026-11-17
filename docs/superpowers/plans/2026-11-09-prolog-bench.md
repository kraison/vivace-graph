# Prolog Bench Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `tests/perf/prolog-bench.lisp`, one measured case per
performance unit of the Prolog Engine 5.0 scope, runnable alone, so
each unit's "before" is recorded before its code changes (unit 1b).

**Architecture:** One new file in `graph-db/perf-test`, its own graph
name and schema so no existing bench's work changes (the generation
rule), five bench functions reported through `record`, `timed-ops` and
`timed-seconds`, and `run-prolog-bench`, which binds what `run-perf`
binds and runs the five, writing a report. `run-perf` gains the five
in its list. A script runs one bench function alone at small scale and
is the verify command.

**Tech Stack:** Common Lisp (SBCL), the perf harness in
`tests/perf/suite.lisp`, `graph-db/spacetime` claims, `graph-db/query`'s
`run-guarded-prolog`.

**Spec:** `docs/superpowers/specs/2026-11-09-prolog-engine-5.0-scope.md`,
"Before and after".

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments pointing at the GH
  issue (`~/.claude/CLAUDE.md`).
- Reported, not gated: no `check-perf` baseline, no assertion on a
  number. `*perf-suite-generation*` is not bumped: nothing existing
  changes its work or labels.
- Own names everywhere: graph name `:graph-db-perf-prolog`, source
  `pbp-thing`, claim classes `pbp-claim`, namespace `:pbp-things`,
  functors `pbp-fact`, `pbp-path`; schema the `rules-bench.lisp` way.
- Every size goes through `scale` so `:small` runs in seconds.
- Asserted clauses (`<-`) live in the image for the rest of it; the
  functors are this file's own, so nothing else sees them.
- Docs travel with code: `tests/perf/README.md` ("What lives here") and
  `CHANGELOG.md` `## [Unreleased]` `### Added`.
- Public repo: domain-neutral text.

## Review Focus

1. A bench that signals (the recursive case before tabling hits
   `prolog-resource-error` by design) must still record its row and let
   the next bench run: `handler-case` inside the case, the outcome in
   the record. Task 1's recursive case is written that way and the
   verify runs it.
2. `run-prolog-bench` must bind a system directory and a null
   `*system-clock*` as `run-perf` does, or the stores fail to open in a
   user image: the verify runs it from a fresh image.
3. `:small` scale must finish in under a minute on odm: the verify is
   timed and the plan records the time in the PR.

---

### Task 1: The bench file, the runner and the script

**Files:**
- Create: `tests/perf/prolog-bench.lisp`
- Create: `tests/perf/run-bench.sh`
- Modify: `graph-db.asd:429-437` (add `(:file "prolog-bench")` after
  `rules-bench` with a comment naming #45)
- Modify: `tests/perf/benchmarks.lisp:1020` (after
  `(bench-forward-step-reads)` add the five calls in the order below)
- Modify: `tests/perf/README.md` "What lives here", `CHANGELOG.md`

**Interfaces:**
- Produces, in `graph-db/perf-test`:
  - `bench-clause-assert ()`: unit 2, #121.
  - `bench-guarded-shape-repeat ()`: unit 6, #376.
  - `bench-recursive-ring ()`: unit 4, #122.
  - `bench-change-feed-cursor ()`: unit 7, #392.
  - `bench-graph-predicates ()`: unit 5, #123.
  - `run-prolog-bench (&key (scale *perf-scale*) (tag "prolog") (output (report-pathname tag)))`:
    binds `*perf-scale*`, a `make-temp-directory` as
    `graph-db::*system-directory*`, `graph-db::*type-registry*` nil and
    `graph-db:*system-clock*` nil exactly as `run-perf` does
    (`benchmarks.lisp:986-992`), `reset-perf-report`, runs the five in
    the order above inside an `unwind-protect` and
    `write-perf-report output :tag tag`; returns `t`.
  - `tests/perf/run-bench.sh FUNCTION`: a fresh SBCL with
    `--dynamic-space-size 16384`, `CL_SOURCE_REGISTRY` on this checkout
    as `tests/run-system-tests.sh` sets it, `(ql:quickload
    :graph-db/perf-test :silent t)`, then
    `(graph-db/perf-test::FUNCTION :scale :small)`; exits 0 when the
    call returns, 1 on an unhandled error, 2 without an argument.

The five cases. Each is one `defun` with a docstring naming its unit and
issue, and the "before" it measures.

- [ ] **Step 1: Schema and fixture.** Copy `rules-bench.lisp:8-33`'s
  shape with the names above: `*perf-prolog-graph-name*`, the
  `def-source pbp-thing`, `def-claim-classes pbp-claim`,
  `with-perf-prolog-graph ((g) &body)`, `pbp-key (i)` as `"k~5,'0D"`,
  and `pbp-insert-claims (n &key (batch 500))` making unary claims under
  `:pbp-things`, relation `"rel"`, producer `"perf"`, standing
  `:inferred`.

- [ ] **Step 2: `bench-clause-assert`** (unit 2, #121). `n` is
  `(scale 500)`, `k` is `(scale 200)`. Inside `with-perf-prolog-graph`:
  `timed-ops ("clause-assert" n)` asserting `n` clauses
  `(pbp-fact "k00042" 42)` through `(graph-db::add-clause (list 'pbp-fact
  (pbp-key i) i))` with `graph-db::*functor*` bound to nil, as the `<-`
  macro does (`prologc.lisp:807`); then `timed-ops ("clause-bound-first-arg" k)`
  running `k` times `(select-one (?v) (pbp-fact "k00250" ?v))`-shaped
  lookups with the key varying over the clauses (build the form and
  `eval` it, or use `select-flat` with a literal key list; either is
  acceptable, say which in the docstring). Before #121 the first number
  falls with `n` (a full recompile per assert) and the second is a
  linear scan.

- [ ] **Step 3: `bench-guarded-shape-repeat`** (unit 6, #376). `n`
  claims by `pbp-insert-claims` with `n` `(scale 2000)`, then
  `timed-ops ("guarded-shape-repeat" k)` with `k` `(scale 500)` calls of
  `graph-db.query:run-guarded-prolog` on the text
  `(claim ?c pbp-claim "pbp-things" "k00123" "rel" ?ons ?o)` with the
  key changing each call; `:limit 10`. One warm-up call untimed. Before
  #376 each call compiles the shape afresh.

- [ ] **Step 4: `bench-recursive-ring`** (unit 4, #122). A ring of `n`
  `p-node` vertices joined by `p-knows` edges, `n` `(scale 200)`, built
  as `bench-prolog` builds its chain (`benchmarks.lisp:157-167`) but
  with the last edge closing the ring; this needs `with-perf-graph`
  from `suite.lisp`, not the claim graph. Two clauses, asserted once
  per image (guard with `(unless (graph-db::lookup-functor ...))` or
  accept the re-assert; say which):
  `(<- (pbp-path ?a ?b) (p-knows ?a ?b))` and
  `(<- (pbp-path ?a ?c) (p-knows ?a ?b) (pbp-path ?b ?c))`. Then
  `timed-seconds ("recursive-ring-closure")` around a
  `(select (:max-inferences 200000) (?x) (pbp-path <first-node> ?x))`
  wrapped in `handler-case` for `graph-db:prolog-resource-error`;
  `record "recursive-ring-outcome" :outcome` `:bound` or `:complete`
  and `:solutions` the count (0 when bound). Before tabling the ring
  loops to the bound; after, `:complete` with `n` solutions.

- [ ] **Step 5: `bench-change-feed-cursor`** (unit 7, #392). Insert
  `(scale 20000)` claims in two parts: the first 95%, then read the
  instant of the last inserted claim with `run-guarded-prolog` on
  `(claim ?c pbp-claim "pbp-things" "<its key>" "rel" ?ons ?o)
  (claim-touched-at ?c ?t)` (the instant is a 30-character string,
  `docs/rules.md` "claim-recorded-at"), then the remaining 5%. Then
  `timed-seconds ("change-feed-cursor")` around one `run-guarded-prolog`
  of `(claim ?c pbp-claim "pbp-things" ?k "rel" ?ons ?o)
  (claim-touched-at ?c ?t) (instant> ?t "<instant>")` with `:limit 1000`
  and `graph-db::*query-default-max-inferences*` bound to 10000000;
  `record "change-feed-cursor-rows" :rows` the count. Before #392 the
  goal walks every claim; after, a range scan from the cursor.

- [ ] **Step 6: `bench-graph-predicates`** (unit 5, #123). On the same
  ring-with-chords graph as Step 4 (reuse its builder; add a chord from
  node `i` to node `(mod (* 3 i) n)` for every tenth `i`):
  `timed-ops ("k-hop-3-select" k)` with `k` `(scale 100)` runs of
  `(select-flat (?d) (p-knows <start> ?b) (p-knows ?b ?c) (p-knows ?c ?d))`
  from varying starts, the hand-written three-hop that `k_hop/4` will
  replace; and `timed-ops ("shortest-path-lisp-bfs" k)` runs of a
  breadth-first search in Lisp over `outgoing-edges` from one node to
  another, `k` pairs, the loop `shortest_path/4` will wrap. The BFS is
  a local `labels` function in this file, not an export.

- [ ] **Step 7: `run-prolog-bench` and the `run-perf` list.** As the
  Interfaces block says. In `benchmarks.lisp` the five calls go after
  `(bench-forward-step-reads)` under a comment `;; the Prolog Engine 5.0
  before-and-after cases (GH #45)`.

- [ ] **Step 8: `tests/perf/run-bench.sh`.** As the Interfaces block
  says; model it on `tests/run-system-tests.sh`.

- [ ] **Step 9: Run it.**
  Run: `time tests/perf/run-bench.sh run-prolog-bench`
  Expected: exit 0; ten `record` lines echoed (two per case); a report
  file under `tests/perf/results/` whose name the script's output shows
  (do not commit it; the measurement run is the next task). Note the
  wall time for the PR.

- [ ] **Step 10: Docs.** `tests/perf/README.md` "What lives here": a
  line for `prolog-bench.lisp` (the Prolog Engine 5.0 before-and-after
  cases, GH #45, one per unit) and one for `run-bench.sh` (one bench
  function alone at small scale). CHANGELOG `### Added`: **The Prolog
  bench** (#45): `tests/perf/prolog-bench.lisp`, five cases, one per
  performance unit of the 5.0 scope, and `tests/perf/run-bench.sh` to
  run one bench alone.

- [ ] **Step 11: Commit.**

```bash
git add tests/perf/prolog-bench.lisp tests/perf/run-bench.sh graph-db.asd tests/perf/benchmarks.lisp tests/perf/README.md CHANGELOG.md
git commit -m "perf: the Prolog bench, one before-and-after case per 5.0 unit (#45)"
```

### Task 2: The "before" measurement (the session, not a worker)

- [ ] On odm, from the merged `experiment`, run
  `tests/perf/run-bench.sh run-prolog-bench` three times at `:normal`
  scale (edit the script's scale by an argument if needed, or call
  `run-prolog-bench` from a REPL with `:scale :normal`); record the
  third run's ten rows, host, SBCL version and commit, on #121, #376,
  #122, #392 and #123, each its own two rows, and the whole table on
  #45. Commit the third run's report file under `tests/perf/results/`
  as `prolog-before-odm-<commit>.report`.

### Landing

- Whole-branch review, PR into `experiment`, post-merge run read; the
  bench's issue is #45 (comment, no close).
