# tests/perf/ — the throughput benchmark suite

System `graph-db/perf-test`. Answers **"did it get slower?"** — throughput
trends over time. It does not tell you *why* something is slow; that is the
`profiling/` harness (`graph-db/profiler`). The split, and how to run each,
is Chapter 19 of `docs/vivace-graph-v3-doc.org`.

```
sbcl --dynamic-space-size 16384
```

```lisp
(ql:quickload :graph-db/perf-test)
(graph-db/perf-test:run-perf :tag "my-tag")
```

Measurement-only by default: `run-perf` always returns T and writes a report
file (path printed at the end). Regression *gating* is the separate
`check-perf` / `bless-perf-baseline` step — the ritual, tolerance bands and
release wiring are in `docs/perf-baselines.md` (GH #253).

## What lives here

- `suite.lisp` — harness: scale knob, report schema/IO, `compare-perf`, and
  `*perf-suite-generation*` (see below).
- `benchmarks.lisp` — the benches themselves; `run-perf` is the entry point.
- `check.lisp` — `check-perf` + `bless-perf-baseline` (GH #253).
- `check-tests.lisp` — FiveAM tests for the checker (run by the main suite).
- `bplus-bench.lisp` — B+ tree vs skip-list side-by-side (`graph-db::bplus-bench`).
- `variance.py` — run-to-run variance analysis over report files.
- `results/` — committed report files (see below).

## Generations, and the one comparison rule

Every report is stamped with host, impl, scale and **suite generation**
(`*perf-suite-generation*` in `suite.lisp`, bumped whenever a change alters
an existing bench's work or labels). **Never compare reports across suite
generations or hosts** — `check-perf` refuses to; such a diff is a setup
mistake, not a perf result.

## results/

- `baseline-<host>-g<gen>.report` — the blessed baselines `check-perf` gates
  against; committed via reviewed diff only (`docs/perf-baselines.md`).
- `release-<tag>-<host>-r<N>.report` + `release-comparison-<date>.md` — the
  release-measurement records: each tag runs its *own* suite and only
  verified-identical benches are compared (the intersection method — the
  comparison doc is the worked example and recipe).
- Everything else is historical runs (2.1.1/3.0 A-B, MVCC phases), kept as
  the record their era's analysis cites.
