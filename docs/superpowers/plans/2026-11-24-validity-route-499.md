# Validity Route Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A page of claims current and valid at T costs in proportion to
the claims valid at T, not to the history filed under its namespace and
relation (vivace-graph#499).

**Architecture:**
- A new ordered index, `claim-validity`, on every claim family.
- A fused functor, `claim-current-valid/10`, whose validity route reads
  that index's open bucket in key order and merges in the claims that
  closed at or after T. It falls back to `claim-after/9`'s route
  otherwise.
- A clause-body rewriter that fuses `claim/7` (or `claim-after/9`) with
  `claim-current` and `claim-valid-at`, keeping both filter goals.

**Tech Stack:** Common Lisp (SBCL), FiveAM, the engine's multi-slot
`def-index` and `map-index-prefixes`.

**Spec:** `docs/superpowers/specs/2026-11-24-validity-route-design.md`
(approved 2026-11-24). Read it first; this plan cites its sections.

## Global Constraints

- Public repository: domain-neutral text in code, tests, docs and
  commits.
- Lisp: spaces only, never tabs. Every line is 80 columns or fewer,
  docstrings included. Comments are terse and point at the spec or an
  issue.
- Core (`index.lisp`, `prologc.lisp`, …) gains nothing. The index is
  declared in spacetime, the route and the rewrite live in rules.
- SBCL needs `--dynamic-space-size 16384`; the runners pass it. ECL is
  not gating.
- Never run the full suite by hand. Run the systems a task names with
  `tests/run-system-tests.sh SYSTEM…`; CI runs the rest on push.
- Grep a suite's directory before naming a test or helper: a suite's
  files share one package, and a reused name silently replaces the
  first.
- A function a test swaps with `(setf fdefinition)` is declaimed
  `notinline`.
- A new test file goes in its system's `:components` in `graph-db.asd`,
  in order, with a `; GH #499` comment.
- One commit per task, staged by explicit path. Nothing is pushed
  without Kevin's word.

## Review Focus

1. **One key holding open claims, claims closed after T and retracted
   claims, in two stores, paged with `:offset` inside the key.** It must
   answer today's rows in today's order. Pinned in Task 3,
   `one-key-in-two-stores-pages-as-today`.
2. **T within one second of a close end, and exactly at it.** The end
   key must order at nanosecond precision. Pinned in Task 2,
   `the-end-key-orders-inside-a-second`, and Task 3's equivalence T
   list.
3. **A retracted claim that is re-asserted.** It must come back to the
   open bucket. Pinned in Task 2, `a-reassertion-returns-to-the-open-bucket`.
4. **A store closed and reopened.** The index survives through the
   sidecar, and the route answers the same after the reopen. Pinned in
   Task 2, `the-index-survives-a-reopen`.
5. **The fallback over a long history inside a transaction.** It must be
   refused at the inference budget, not run unbounded, because it ticks
   per candidate. Pinned in Task 3,
   `the-fallback-is-refused-at-the-budget`.

---

### Task 1: The before case

Spec §6, and the "Perf" acceptance. The bench is written and run on the
base commit before any engine change.

**Files:**
- Modify: `tests/perf/rules-bench.lisp` (append)
- Modify: `tests/perf/run-bench.sh` (two `case` arms)
- Modify: `tests/perf/README.md` (the `rules-bench.lisp` line)

**Interfaces:**
- Produces: `bench-valid-at-page ()` and `bench-claim-writes ()` in
  `graph-db/perf-test`, each recording through `record` /
  `timed-seconds` as `bench-claim-key-paging` does.

- [ ] **Step 1: The fixture.**
  - Use a new temporal family, `pv-claim`, on its own graph name
    `:graph-db-perf-validity`, declared the way `pf-claim` is.
  - `pv-seed (g closed open)`: CLOSED keys `"a00000"…`, then OPEN keys
    `"b00000"…`, one relation `"rel"`, namespace `:pv-things`. Each is
    a binary claim with an open validity extent.
  - Each closed key's claim is then closed the way an absence closes it:
    `copy`, `setf claim-extent` to the interval [start, close − 1 ns],
    `save`. That is followed by a unary claim on the same key and
    relation whose extent is the instant of the close.
  - Batches of 500 per transaction.
- [ ] **Step 2: `bench-valid-at-page`.**
  - For CLOSED in 0, `(scale 3000)` and `(scale 30000)`, with OPEN 600,
    page the issue's text through `run-guarded-prolog`:
    `(claim ?c pv-claim "pv-things" ?k "rel" ?ons ?o) (claim-current
    ?c) (claim-valid-at ?c "<T>")` with `:limit 50`, T after every close.
  - Record two figures for each N: the least `:max-inferences` that
    answers it, found by bisection within 1% like the issue's table
    (labels `valid-at-page-budget-<n>`), and the seconds at a generous
    budget (`valid-at-page-seconds-<n>`).
  - Record `valid-at-page-budget-ratio`, the largest N's budget over
    N = 0's.
- [ ] **Step 3: `bench-claim-writes`.** Write `(scale 20000)` `pv-claim`
  claims in batches of 500, then close each one as Step 1 does. Record
  `claim-writes-per-second` and `claim-closes-per-second`.
- [ ] **Step 4: Wire the runner.** Add `bench-valid-at-page` and
  `bench-claim-writes` arms to `run-bench.sh`, each calling the function
  at small scale as the other arms do. Update the README's
  `rules-bench.lisp` line to name both and GH #499.
- [ ] **Step 5: Verify.** `tests/perf/run-bench.sh bench-valid-at-page`
  and `tests/perf/run-bench.sh bench-claim-writes` each print their
  figures and exit 0. Also run `tests/check-build-warnings.sh`.
- [ ] **Step 6: Commit** the three files: `perf: the validity-route
  before case's benches (#499)`.
- [ ] **Step 7 (session, on odm): record the before case.**
  - Run both benches at normal scale on this commit.
  - Paste the figures into "Before case" at the end of this plan, and
    into a comment on #499 naming the host and commit.
  - Commit the plan.

### Task 2: The index

Spec §1.

**Files:**
- Modify: `spacetime/claim.lisp`. Add the canonicalizers beside
  `extent-sexp-touched-key`, and the `def-index` in `def-claim-classes`
  beside `claim-touched-at`.
- Create: `tests/spacetime/validity-index-tests.lisp`
- Modify: `graph-db.asd`: the new file after `touched-tests` in
  `graph-db/spacetime-test`.
- Modify: `docs/vivace-graph-v3-doc.org`: the claim chapter's list of a
  family's indexes.
- Modify: `CHANGELOG.md` `[Unreleased]` Added: the index.

**Interfaces:**
- Produces (in `graph-db.spacetime`, internal):
  - `extent-sexp-currency-key (sexp)`: returns `+currency-current+` for
    an open transaction period and `+currency-retracted+` for a closed
    one. It returns its argument unchanged when the argument is already
    one of those two.
  - `extent-sexp-validity-end-key (sexp)`: returns `+validity-open+`
    when the end's latest edge is `:unbounded` (an unknown end counts as
    open), else `(%timestamp-key latest)`. It returns its argument
    unchanged when the argument is already canonical (`+validity-open+`
    or a `(day sec nsec)` list).
  - The constants `+currency-current+`, `+currency-retracted+` and
    `+validity-open+`.
  - The index `claim-validity` on slots `(subject-namespace relation
    transaction-extent-sexp extent-sexp subject-key)`, with
    `:canonicalize (nil nil extent-sexp-currency-key
    extent-sexp-validity-end-key nil)`.
- Two traps for Task 3 to rely on:
  - A NIL slot is stored as `graph-db::+null-component+` and never
    reaches a canonicalizer. An unstamped claim therefore sits under a
    null currency, and a claim with no extent under a null validity end.
  - `+validity-open+` sorts before every `(day sec nsec)` key under
    `graph-db::%index-comp-lessp`. So the open bucket is the prefix
    `(ns rel cur +validity-open+)`, and the claims closed at or after T
    begin at `(ns rel cur (%timestamp-key T))`.

- [ ] **Step 1: Write the failing tests** in
  `tests/spacetime/validity-index-tests.lisp`, using the suite's
  existing claim helpers (`make-u`, `make-b`, and a temporal family if
  the suite has one; otherwise declare one in this file):
  - `the-currency-key-reads-the-transaction-period`: open gives current,
    closed gives retracted, and each canonical value comes back
    unchanged.
  - `the-end-key-is-open-or-the-latest-edge`: an unbounded end and an
    unknown end give `+validity-open+`; an exact end gives
    `%timestamp-key` of it; canonical values come back unchanged.
  - `the-end-key-orders-inside-a-second`: two ends 1 ns apart in the
    same second order by `%index-comp-lessp` as the instants do.
  - `open-sorts-before-every-closed-end`: in `%index-comp-lessp`, a
    tuple with `+validity-open+` in the fourth position sorts before the
    same tuple with any end key there.
  - `a-close-moves-the-entry`: one claim, open, appears under the open
    prefix (`index-lookup` on `claim-validity`'s full tuple). After a
    copy / `setf claim-extent` / `save` close it is gone from there and
    found under its end key.
  - `a-retraction-moves-the-entry`: after `retract-claim`, the claim is
    under `+currency-retracted+`.
  - `a-reassertion-returns-to-the-open-bucket`: retract, re-assert the
    same identity (the path `register-node` re-opens), and the claim is
    under current and open again.
  - `unstamped-and-extentless-claims-sit-under-the-null-component`: a
    claim with no transaction extent is found with a null currency
    component, and one with no extent with a null end component.
  - `the-rebuilt-index-equals-the-maintained-one`: after a mix of
    writes, closes and retractions, the full entry list from
    `map-index-prefixes` at arity 5 equals the list after
    `regenerate-secondary-indexes`.
  - `the-index-survives-a-reopen`: write and close claims, close the
    graph, reopen it, and the entry list is unchanged.
- [ ] **Step 2: Run, expect failures.**
  `tests/run-system-tests.sh graph-db/spacetime-test`. The new tests
  fail (undefined functions or index); the others pass.
- [ ] **Step 3: Implement.** The canonicalizers and constants, then the
  `def-index` with `:name claim-validity`. Parse each slot with
  `sexp->extent`, as `extent-sexp-touched-key` does. Docstrings carry
  the idempotence trap.
- [ ] **Step 4: Run, expect green.**
  `tests/run-system-tests.sh graph-db/spacetime-test graph-db/rules-test`
  (the rules suite's families gain the index too) and
  `tests/check-build-warnings.sh`.
- [ ] **Step 5: Docs.** Add the index to the manual's list of a
  family's indexes, and an Added line to the CHANGELOG.
- [ ] **Step 6: Commit** the five files: `spacetime: the claim-validity
  index (#499)`.

### Task 3: The functor and the validity route

Spec §2, §4. The functor's contract must hold on every route before the
rewrite exists. Tests write it by hand.

**Files:**
- Modify: `rules/facts.lisp`
- Create: `tests/rules/validity-tests.lisp`
- Modify: `tests/rules/suite.lisp`: the fixture helpers below
- Modify: `graph-db.asd`: the new file after `touched-tests` in
  `graph-db/rules-test`.

**Interfaces:**
- Consumes: Task 2's index, constants and traps.
- Produces:
  - `claim-current-valid/10` (`def-global-prolog-functor`), with
    arguments `(?c ?family ?sns ?skey ?rel ?ons ?okey ?skey-after
    ?okey-after ?at)` and the contract of spec §2.
  - `%validity-route (family sns rel at skey-after emit)`: returns T
    when it took the route and answered, NIL when the caller must fall
    back. It is called before any candidate is emitted, so a NIL never
    follows emitted rows.
  - `*validity-closed-cap*`, a `defvar` in rules/facts.lisp defaulting
    to 10000 (spec, "Calls I made"). It counts distinct index tuples
    across the stores in scope.
  - `%resolve-validity-candidates`, declaimed `notinline`: the one
    function that turns index tuples into nodes, so a test can count
    resolutions.
  - `claim-current-valid/10`'s docstring states the contract and points
    at the spec.
- The route, as spec §2 says, per store in `%scope-graphs` and per
  currency in (`+currency-current+`, NIL):
  - **Closed set:** `map-index-prefixes` on `claim-validity` at arity 5,
    `:start (sns rel cur (%timestamp-key T))`. Leave by `return-from`
    when the first three components change. Skip tuples whose end is the
    null component. If more than `*validity-closed-cap*` tuples are
    collected across all stores, answer NIL.
  - **Open bucket:** `map-index-prefixes` at arity 5, `:start (sns rel
    cur +validity-open+)`, leaving when the fourth component is no
    longer open. Skip a key with `%key-after-p` on the tuple's key,
    without resolving it.
  - **Merge** by key. Within a key, emit the own store's candidates in
    id order (the index's own order), then each foreign store's.
    Candidates are resolved by `index-lookup` on the full tuple. A
    foreign store without the family contributes nothing, as in
    `%scope-lookup`. Then `%derived-all`.
- The fallback conditions are spec §2's list. Inside a read-write
  transaction, test `%transaction-covers-graph-p`; for a snapshot or
  as-of read, test `*read-snapshots*`.
- `%tick` is called once per candidate resolved, on the validity route
  and on the fallback (spec §4).

- [ ] **Step 1: Fixture helpers** in `tests/rules/suite.lisp`, each name
  grepped first:
  - `seed-validity (g)`: on `rtt-claim` under `:vt`, relation `"rel"`,
    at fixed `ts` instants:
    - open claims; claims closed before T₀ and claims closed after T₀;
    - instant claims at, before and after T₀;
    - a retracted open claim and a retracted closed one;
    - a key holding three producers' claims;
    - an unstamped claim (no transaction extent), written as the
      spacetime tests write one.
  - `seed-validity-b (g)`: a few claims in the second store
    (`:graph-db-rules-b`), sharing keys with the first.
  - `close-validity (claim end)`: copy, set the extent to end at END,
    save, the way an absence closes a claim.
  - `counting-resolutions ((counter) &body body)`, modelled on
    `counting-lookups`, which swaps `%resolve-validity-candidates`.
- [ ] **Step 2: Write the failing tests** in
  `tests/rules/validity-tests.lisp`:
  - `the-fused-functor-answers-as-the-three-goals`: for each T in
    (before everything, T₀, exactly a close end, 1 ns before it, inside
    the same second, after everything, an instant claim's instant),
    `claim-current-valid` by hand equals `claim-after` followed by
    `(true) (claim-current ?c) (claim-valid-at ?c T)`, compared with
    `equal` (same rows, same order). That holds with no key bound, a
    subject key bound and an object key bound.
  - `one-key-in-two-stores-pages-as-today`: with `*claim-scope*` over
    both stores, paging one key's rows by `:limit 2 :offset k` gives
    the same sequence fused and unfused.
  - `t-through-a-variable-bound-earlier`: `(= ?t "<T>")` before the goal
    gives the same rows as the literal.
  - `the-validity-route-reads-no-closed-history`: seed 300 closed keys
    ahead of 30 open ones. A 10-row fused page answers under
    `:max-inferences 200`, and `counting-resolutions` counts no more
    than the candidates the page needed, 10 to 12. The same page through
    `claim-after` plus the filters signals
    `graph-db::prolog-resource-error` under the same budget.
  - `each-fallback-answers-the-same`: a free T, `:as-of` at the current
    epoch − 1 (after one more commit), `(select (:snapshot t) …)`, a
    read-write transaction (including one where another thread's commit
    closes a claim after the transaction began, compared with the
    unfused query in the same transaction), and a closed set over the
    cap. For the cap test, bind `*validity-closed-cap*` to 2, so the
    test needs no 10,001 claims.
  - `the-fallback-is-refused-at-the-budget`: inside a read-write
    transaction over 300 closed keys, the fused page signals
    `prolog-resource-error` at `:max-inferences 200`, because each
    candidate it reads ticks.
  - `a-retracted-or-unstamped-claim-is-answered-as-claim-current-says`:
    the unstamped claim is answered, and the retracted ones are not, on
    both routes.
- [ ] **Step 3: Run, expect failures.**
  `tests/rules/run-test.sh RULES-SUITE` fails on the new tests only.
- [ ] **Step 4: Implement** the functor, `%validity-route`,
  `%resolve-validity-candidates` and the fallback. Share code with
  `%claim-goal` as the spec leaves to you, keeping `claim/7`'s and
  `claim-after/9`'s behaviour unchanged.
- [ ] **Step 5: Run, expect green.**
  `tests/run-system-tests.sh graph-db/rules-test graph-db/query-test`
  and `tests/check-build-warnings.sh`.
- [ ] **Step 6: Mutation check.** In a scratch edit, make
  `%validity-route` always answer NIL. Expect
  `the-validity-route-reads-no-closed-history` to fail, then restore
  the edit. Do the same with the `%tick` per candidate removed: expect
  `the-fallback-is-refused-at-the-budget` to fail. Report both in the
  result.
- [ ] **Step 7: Commit** the four files: `rules:
  claim-current-valid/10 and the validity route (#499)`.

### Task 4: The rewrite, rule bodies, explain and the docs

Spec §3, §5, §7.

**Files:**
- Modify: `rules/facts.lisp`: `%fuse-validity` and its registration
- Modify: `rules/standing.lisp`: `("claim-current-valid/10" . :withheld)`
- Modify: `rules/compile.lisp`: the refusal beside `claim-after/9`'s
- Modify: `tests/rules/validity-tests.lisp`, `tests/rules/compile-tests.lisp`,
  `tests/rules/explain-tests.lisp`
- Modify: `docs/rules.md`, `docs/guarded-query.md`, `CHANGELOG.md`
- Modify: the docstrings of `claim/7` and `claim-valid-at/2`

**Interfaces:**
- Consumes: `claim-current-valid/10` (Task 3); `%fuse-key-bounds`'s
  output shape, `(claim-after … SA OA)` followed by its kept `>` goals.
- Produces: `%fuse-validity (goals) → goals`, registered on
  `*clause-body-rewriters*` so that it runs after `%fuse-key-bounds` and
  `%fuse-touched-cursor`. Its conditions are spec §3's list. It returns
  its input `eq` when nothing fuses.

- [ ] **Step 1: Write the failing tests.**
  - `the-validity-rewrite-fuses-both-orders`: `(claim …) (claim-current
    ?c) (claim-valid-at ?c "T")` and the reverse filter order both
    become `(claim-current-valid … nil nil "T")` followed by the two
    filters, unchanged.
  - `the-validity-rewrite-fuses-behind-a-key-bound`: the
    `%fuse-key-bounds` output `(claim-after … "k1" nil) (> ?k "k1")
    (claim-current ?c) (claim-valid-at ?c ?t)` becomes `(claim-current-valid
    … "k1" nil ?t) (> ?k "k1") …`.
  - `the-validity-rewrite-leaves-every-other-shape-alone` (`eq`):
    - only one filter; a `(true)` between; the filters on another
      variable;
    - `?c` bound earlier, e.g. `(claim-touched-after …)` first;
    - a T variable the claim goal binds, e.g. `(claim-valid-at ?c ?ok)`
      with `?ok` the claim goal's object key;
    - a numeric T; no claim goal; `'()`.
  - `the-rewriters-run-in-order`: the positions of `%fuse-validity`,
    `%fuse-key-bounds` and `%fuse-touched-cursor` in
    `*clause-body-rewriters*` put `%fuse-validity` last to run.
  - `the-recommended-valid-at-text-reads-no-closed-history`: Task 3's
    cost test, written as the three goals plus `(> ?k B)` under
    `select`. It answers under the same budget, so the rewrite fused it.
  - `the-guard-path-is-fused-for-valid-at`: the same text through
    `run-guarded-prolog` answers under the same budget.
  - `a-fused-rule-body-sees-this-runs-derivations`: a rule whose body is
    the three goals, over a family the same run derives into, gives the
    same derived claims as the rule with a `(true)` between the claim
    goal and its filters. This is the spec's "a derivation in the run"
    case: `%derived-all` follows the route.
  - In compile-tests: `a-rule-may-not-write-claim-current-valid`. The
    refusal text says to write the three goals.
  - In explain-tests: `explain-names-the-validity-row`. A hand-written
    `claim-current-valid` explains as "validity index from the key
    bound, or claim-after's route; decided at run time". The
    three-goal text still explains as the subject vocabulary route.
- [ ] **Step 2: Run, expect failures.** `tests/rules/run-test.sh
  RULES-SUITE`.
- [ ] **Step 3: Implement** the rewriter, its registration, the
  standing entry, the rule refusal and the explain row.
- [ ] **Step 4: Run, expect green.**
  `tests/run-system-tests.sh graph-db/rules-test graph-db/query-test
  graph-db/gui-test` (the functor inventory is unchanged, as #451's
  was) and `tests/check-build-warnings.sh`.
- [ ] **Step 5: Docs**, spec §7:
  - `docs/rules.md`: the route-table row, the fused shape and its
    conditions, the explain note, and the race under "Paging".
  - `docs/guarded-query.md` "Paging": what a valid-at page costs, and
    what keeps today's cost.
  - CHANGELOG: Added (the functor and the rewrite) and Changed (the
    page cost).
  - The two docstrings point at `claim-current-valid/10`.
- [ ] **Step 6: Commit**: `rules: the validity rewrite, its rule and
  explain handling, and the docs (#499)`.

### Task 5 (session, on odm): the after case and the store measurement

Spec §6. Run in the session: it needs the store copy and cl-llm on odm.

- [ ] **Step 1:** Rerun Task 1's benches at normal scale on the branch
  head. Put the figures beside the before case below. Acceptance:
  `valid-at-page-budget-ratio` ≤ 1.5. Report the write-cost change.
- [ ] **Step 2:** Make sure there is room: about 3× the copy's 2.1 GB.
  Copy `~/.cl-llm-memory/working-snapshot-20261103-c1411c8` to
  `/var/tmp/vg499-store/`. If the copy holds a `.dirty` sentinel, stop
  and report it; don't recover it.
- [ ] **Step 3:** In a fresh SBCL with the base engine (`experiment` at
  86222cf) and cl-llm's memory system, time `open-graph` on the copy and
  close it cleanly. Re-copy, then do the same with the branch head, which
  builds `claim-validity` at open. The difference is the build. Record
  the index's sidecar or heap growth, and scale both figures to the
  live store's 4.5 GB.
- [ ] **Step 4:** Add a Changed line to the CHANGELOG with the write
  cost and the build figure. Post all the figures on #499 and on
  kraison/blackboard#38. Delete `/var/tmp/vg499-store/`.
- [ ] **Step 5: Commit** the CHANGELOG and this plan's results.
- [ ] **Step 6:** Hand the branch to Kevin for review. After his merge
  and push words, read the push run on `experiment` (`gh run list -R
  kraison/vivace-graph --branch experiment`). The unit counts as landed
  only when it is green; then close #499 with the landing SHA.

## Before case

Recorded 2026-11-24 on odm (SBCL 2.6.6, perf generation 5), at commit
3b0b1f4, the base engine with the benches, at normal scale. Another
worker's SBCL build was running at the same time, so the seconds are
noisy; the inference budgets are exact.

| closed claims ahead | least budget, 50-row page | seconds |
|---|---|---|
| 0 | 164 | 0.004 |
| 3,000 | 12,217 | 0.229 |
| 30,000 | 120,554 | 2.35 |

`valid-at-page-budget-ratio` is 735.1. The 30,000 row is over a
100,000-inference budget. The 3,000 row matches the issue's own table,
which was measured through a consumer's guarded queries.

Writes: 20,000 claims at 545 a second; 20,000 validity closes at 471 a
second.
