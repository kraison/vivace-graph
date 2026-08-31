# CI: the full suite runs itself

`.github/workflows/test.yml` runs the full test suite (main,
concurrency, ACID; 12GiB heap per `tests/README.md`) on every push
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
  source registry to the workspace checkout plus the host's
  `cl-temporal-extent`, so the tested tree is exactly the pushed
  tree, never a host pin.
- Fork PRs require approval before workflows run on these machines
  (repository default; do not relax it).
