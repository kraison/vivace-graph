# CI: the full suite runs itself

`.github/workflows/test.yml` runs the full test suite (main,
concurrency, ACID, spacetime, query, rules, geos, algorithms, gui;
12GiB heap per `tests/README.md`) on every push
to `experiment` or `master` and on pull requests.

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
