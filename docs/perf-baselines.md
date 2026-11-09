# Perf baselines and regression checks (GH #253)

The perf suite (`tests/perf/`, system `graph-db/perf-test`) is
measurement-only by default. This document is the ritual that turns its
reports into regression gates.

## Running the suite

```
sbcl --dynamic-space-size 16384
```

```lisp
(ql:quickload :graph-db/perf-test)
(graph-db/perf-test:run-perf :tag "my-tag")   ; :normal scale by default
```

`run-perf` writes a report file (path printed at the end) stamped with
tag, impl, scale, **suite generation** and **host**.

On a fresh host, make sure ASDF resolves `graph-db` to the intended
checkout (Quicklisp `local-projects` symlink, or push the repo dir onto
`asdf:*central-registry*`) and verify with
`(asdf:system-source-directory :graph-db)` before trusting a run.

## The stale-fasl rule (learned 2026-09-02)

**Never benchmark the incrementally-recompiled working checkout after a
heavy edit day.** ASDF's incremental rebuilds can leave a fasl set that
runs measurably slower than a clean build of the *same tree* — observed
at 2.4× on `prolog-is-a-scan` (27k vs 68k ops/s, four stable samples
each side), which burned an afternoon of bisecting before the clean
control exposed it. It is 3.0.0's `rsync -a` stale-fasl lesson in a new
costume. Before any measurement that will be compared or blessed: run
from a fresh `git worktree` (its own fasl namespace), or purge
`~/.cache/common-lisp/<impl>/<path-to-checkout>` and rebuild.

## The comparability rule

Never compare across suite generations or hosts (learned in the 3.0
A/B). `*perf-suite-generation*` in `tests/perf/suite.lisp` is bumped
whenever a change alters an existing bench's work or labels (adding a
new bench does not bump). Baselines are therefore per-host AND
per-generation files:

```
tests/perf/results/baseline-<host>-g<generation>.report
```

`check-perf` refuses to compare reports whose generation, host, or
scale differ — that is a setup mistake, not a perf result.

## Blessing a baseline

```lisp
(graph-db/perf-test:bless-perf-baseline "/path/to/run.report")
```

Copies the report to the blessed path above. It refuses a
wrong-generation or non-`:normal` report. Before blessing, eyeball the
candidate against historical norms (`results/` or the outgoing
baseline): a run whose early cells are anomalously *fast* — an
unusually idle host — makes a baseline every normal run fails against.
The g4 re-bless caught exactly this and blessed run 2 of 3 instead.

Blessing is deliberate: the new baseline file **must be committed via a reviewed diff**, never
automatically — re-blessing after an intentional perf change is part of
that change's review.

## Checking a run

```lisp
(graph-db/perf-test:check-perf "/path/to/run.report")
;; => (values pass-p failures), prints a per-label table
```

- Finds the blessed baseline for the report's host + generation
  (`:baseline` overrides explicitly).
- Per-label primary metric: `:us/commit`/`:us/edge` where recorded,
  else `:ops/s`, else `:bytes`, else `:seconds`. Throughput regresses
  on a drop, latency/bytes on a rise.
- Tolerance: `*perf-tolerance*` (15%) — generous on purpose; natural
  run-to-run variance at `:normal` scale is ~5–10% on most benches
  (see `variance.py`). Known-noisy labels get per-label overrides in
  `*perf-tolerance-overrides*` (seeded with the `v5scan-f0-*` /
  `v5scan-f10-*` rows, ~17–20% swing in the #252 evidence runs and
  the first bless/check pair) rather than widening the global band.
- A label in the baseline but missing from the run **fails** (a bench
  silently vanished); a new label not in the baseline is reported as
  "new, unbaselined", not a failure.
- Never signals on regression by default (measurement culture:
  report, don't crash); pass `:error-p t` for gating callers — a CLI
  gate wraps the resulting `perf-regression-error` into a nonzero
  exit code (e.g. `handler-case` → `(uiop:quit 1)`).
- If the candidate lost the baseline's primary metric, the next
  shared comparable metric is gated instead; a row sharing no
  comparable metric fails as `:metric-vanished`. `index-fullscan-eq`
  gates `:seconds` (its ~4 ops/s quantizes at 25% per step) via
  `*perf-primary-metric-overrides*`.

## Release ritual

Before cutting a release (the `experiment` → `master` merge + tag):

1. Fresh SBCL, `--dynamic-space-size 16384`, `run-perf` at `:normal`
   on the baseline host.
2. `check-perf` the report against the blessed baseline. A failure is
   either a real regression (fix it or record the accepted trade-off in
   the CHANGELOG) or an intentional change (re-bless — reviewed diff).
3. After a release that bumped the suite generation, run + bless a
   fresh baseline so the next cycle has one.

New benches added since the last bless show as "new, unbaselined" —
re-bless to adopt them.

(The perf-suite vs `profiling/` harness split is documented separately —
GH #255.)

## Cases with a reason of their own

- **`scan-in-tx` against `scan-committed`** (`bench-tx-scan`, GH #408).
  The same 200 adjacency scans, first inside the transaction that wrote
  1,000 edges, then after it commits. The gap is the own-write overlay:
  O(pending writes) per scan in this unit. First measurement, odm, SBCL
  2.6.6, generation 5, 2026-10-29: 1,695 ops/s in the transaction
  against 5,263 committed (3.1×). If that ratio grows with the write set
  in a real workload, the fix is maintained per-transaction buckets
  behind the same overlay functions (spec 2026-10-29 §6), not a bigger
  scan.

- **The Prolog Engine 5.0 "before"** (`run-prolog-bench`, GH #45).
  `results/prolog-before-odm-55240fd.report` is the third of three
  normal-scale runs on odm, SBCL 2.6.6, generation 5, 2026-11-09, taken
  before any unit's code changed: 500 clause asserts at 16/s and 5,000
  bound-first-argument lookups at 50,000/s (#121); 500 repeated guarded
  shapes at 1,208/s (#376); a change-feed cursor over 20,000 claims in
  6.9 s (#392); 100 three-hop selects at 2,041/s and 100 Lisp BFS paths
  at 658/s (#123); and the recursive ring closure exhausting the control
  stack in 0.09 s with no solution (#122). Each unit reruns the same
  bench when it lands and posts both rows on its issue.
- **Unit 2, clause indexing, after** (#121):
  `results/prolog-after-u2-odm-6e416f7.report`, same host, SBCL and
  generation, 2026-11-09, third of three runs. 500 asserts at 430/s
  (was 16/s; runs 1 and 2: 488/s, 289/s), 5,000 bound lookups at
  263,144/s (was 49,998/s); the new `clause-assert-10k` case asserts
  10,000 clauses in 19.6 s (510/s; runs 1 and 2: 25.3 s, 25.5 s) and
  answers 20,000 bound lookups at 192,298/s. An assert extends the
  previous immutable clause set (one bucket replaced, the rest shared)
  rather than rebuilding it, so the per-assert rate at 10,000 clauses
  matches the rate at 500. The dispatcher tail-calls its last
  candidate and a clause closure's body is its last form, so a
  tail-recursive rule keeps constant stack: a probe recursing 2,000,000
  deep passes on the unchanged engine and on this one, where a first
  version that read a cut flag after the clause block died at 7,000.
  `recursive-ring-closure` still exhausts the stack (its recursion
  runs inside edge-generator frames, not a tail call); it does so in
  0.065 s against 0.09 s before, a smaller frame per level, not a
  change of outcome. The other rows are unchanged within noise.