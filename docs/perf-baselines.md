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
