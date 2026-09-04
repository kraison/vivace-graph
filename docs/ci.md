# CI: the full suite runs itself

`.github/workflows/test.yml` runs the test suite (main, concurrency,
ACID, spacetime, query, rules, geos, algorithms, gui; 12GiB heap per
`tests/README.md`) on every push to `experiment` or `master` and on
pull requests.  A push runs all of it; a pull request runs the main
suite's fast tier and every other lane in full (see below).

- **The main suite is tiered** (GH #340).  Timing all 143 children of
  `graph-db-suite` on 2026-09-04 found three of them are **71% of the
  wall clock for 8% of the checks** -- `system-restore-suite` (706s),
  `detach-suite` (677s) and `type-id-width-suite` (174s), which build
  and tear down real on-disk stores at 2-5 seconds per check against
  0.13 for everything else.  So:
  - a **pull request** runs `graph-db/fast-test`, the same suite minus
    those three: ~10 minutes, 92% of the checks;
  - a **push to `experiment`** runs `graph-db` in full, ~36 minutes,
    and remains the gate `experiment -> master` promotion waits on.

  The list lives in `*slow-suites*` in `tests/suite.lisp`, beside the
  suites rather than in this workflow, so marking a new slow one is a
  one-line change there.  `run-tests` refuses a name that is not a
  child of `graph-db-suite`, so a suite renamed out from under the list
  fails loudly instead of quietly excluding nothing.

  What this trades: a regression only those three catch lands on
  `experiment` and is caught by the push run minutes later, rather than
  on the pull request.  #340 tracks making them fast enough to delete
  the tier.

  To re-measure, time each child of `graph-db-suite` individually --
  `fiveam:run` on each name from `(suite-children 'graph-db-suite)` --
  and compare the total against the step's own duration.

- **Runners are self-hosted** (personal-account runners are
  per-repo).  Primary: the `ma-dev` host, running as its `sitrep`
  user under a niced systemd --user unit (`gh-runner.service`) so
  interactive work and services always win contention.  Secondary:
  an arm64 mac lane, advisory only (`continue-on-error`) -- its
  value is the #218 class of bug, arm64-only failures.
- **The habit this replaces**: running the 35-minute suite by hand
  on every change.  Local runs are the affected subsystem's suite;
  the push gets the full matrix; `experiment -> master` promotion
  waits for the green check.
- **One live run per ref**: the workflow sets `concurrency` with
  `cancel-in-progress`, keyed on the ref.  The runner is serial, so a
  superseded run does not just waste 40 minutes of its own -- it costs
  whatever is behind it that whole wait.  A fixup pushed to a PR now
  cancels that PR's earlier run instead of queueing behind it, and two
  merges landing close together leave one `experiment` run, the later
  one, which contains both.  Groups are per-ref, so a PR can cancel
  only its own earlier run.
  The cost: when merges land together, the earlier commit never gets a
  run of its own, so a red head does not say which merge broke it.
  Promotion is unaffected -- it gates on the head, which always runs.
  Re-run a cancelled commit with `gh run rerun <id>` if you need to
  bisect.
- The runner neutralises quicklisp `local-projects` and pins the
  source registry to the workspace checkout plus
  `cl-temporal-extent` refreshed to master head each run (in
  `~/ci-deps-vivace-graph`, this runner's own dir -- shared-user
  checkouts race), so the tested tree is exactly the pushed tree
  against current deps, never a host pin (policy extended
  2026-09-01).
- **Each lane is its own `sbcl` process, and one tripwire depends
  on that**: `gui-tests.lisp`'s `prolog-functor-inventory-is-pinned`
  compares the registered Prolog functors against a hand-reviewed
  list, in both directions (GH #279).  `graph-db/gui` does not
  depend on `graph-db/rules`, so the rules functors are absent from
  the gui lane and the list must not name them -- but an image that
  loads both subsystems WILL trip it, with a message naming the
  seven `claim*` functors.  That is the tripwire working: classify
  them, do not delete the check (`docs/rules.md`, GH #330).
- The geos lane needs `libgeos_c` on the runner host; the suite
  SKIPS (green) where it is absent, so a green geos lane on a
  bare host proves nothing -- keep the library installed.
- NOT in CI: the stress suites (`stress-test`,
  `concurrent-stress-test`; long, load-sensitive on a shared
  host) and `perf-test` (measurement, not pass/fail --
  `docs/perf-baselines.md`).  Run those deliberately.
- Fork PRs require approval before workflows run on these machines
  (repository default; do not relax it).
