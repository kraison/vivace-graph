# Release-baseline comparison: v2.1.1 / v3.0.0 / experiment (GH #263)

Date 2026-08-27. Host `odm`, SBCL 2.6.6, fresh image per run, three
`:normal` runs per tree, **round-robin interleaved** (v2.1.1 → v3.0.0 →
experiment, ×3) so host-state drift spreads fairly. Each tree ran **its
own** `tests/perf/` suite (the intersection method — see #263): trees
at `v2.1.1` (18 labels), `v3.0.0` (24), `experiment@85496c3` (58).
Every intersecting bench implementation was verified **byte-identical**
across the three tags before the runs (Phase-0 survey on #263), with
one exception, `reopen`, noted below. Values are medians of three;
raw reports: `release-*-odm-r{1,2,3}.report` alongside this file.

This is a dated judgment document over a verified intersection — NOT a
`check-perf` gate. The never-compare-across-generations rule stands;
this exercise defines its comparable subset explicitly.

## Headline: experiment vs v3.0.0 (24-label intersection)

Reads, scans, queries and index operations are flat to slightly better
(±5%). **The commit-heavy cells regressed**, and the cause is known:

| label | v3.0.0 | experiment | delta | note |
|---|---|---|---|---|
| commit-per-op-Ntxn (ops/s) | 1933 | 1272 | **−34.2%** | low variance; real |
| concurrent-rw (ops/s) | 1520 | 1007 | **−33.8%** | real |
| restore-replay (s) | 3.815 | 4.636 | **−21.5%** | real |
| snapshot (s) | 1.726 | 1.966 | −13.9% | plausible, cause unconfirmed |
| commit-batched-1txn (ops/s) | 6587 | 5688 | −13.6% | CV >10%, read with care |

Attribution: #177 (landed post-3.0.0, PR #236) made
`persist-highest-transaction-id` monotonic — a read + conditional
write per commit under the process-global watermark lock. The #252
microbenchmarks measured the isolated cost at ~120 µs/commit and the
8-graph convoy at ~89% of commit latency; these release numbers are
the same fingerprint in the wild (per-op commits, threaded r/w on one
graph, and replay applying transactions one by one). **#237's fix
(cached watermark + per-graph lock) is corroborated and elevated.**
The `bench-multigraph-commit` control cell will measure the recovery
when it lands.

Everything else in the 24-label set (full table in the #263 comment):
insert/scan/update/delete vertices, insert/scan edges, both prolog
cells, all four index cells, both unique cells, the heap-bytes cells
(byte-identical values), and `reopen` sit between −10% and +9%, most
within ±5%. `lookup-by-id` measured −9.8% but its CV was 36–53% on
ALL THREE trees this day — that cell's number is noise, not signal.

## Three-way (18-label intersection)

The published 3.0-vs-2.1.1 result reconfirms on this host at the same
magnitudes: v3.0.0 over v2.1.1 ranges +13.8% (prolog-edge-join) to
+535% (update-vertices), `reopen` +71.5%. experiment over v2.1.1
remains strongly positive everywhere (+14% to +539%) EXCEPT
`commit-per-op-Ntxn` (+30.1%, eroded from +97.6% by the #237 cost)
and `concurrent-rw` (−13.6%, inverted by it).

## Caveats

- **`reopen`** is the one EQUIVALENT-BUT-CHANGED label: its timed
  region includes `update-schema` over the perf schema, which has 2
  types at v2.1.1, 4 at v3.0.0, 5 at experiment — a small bias
  *against* the newer trees. Recorded since the original A/B
  (commit 72a07f7's note); the +2.7%/+71.5% readings above carry it.
- **Same-day noise**: `lookup-by-id` (CV up to 53%) is excluded from
  interpretation; other CV>10% cells are flagged in the #263 variance
  data. Contended cells (`commit-batched-1txn`, the head-only
  multigraph/clock-contended cells) wobble as their tolerance
  overrides already record.
- v2.1.1's suite has an unseeded-random scratch hazard: its runs were
  strictly sequential with an isolated TMPDIR.

## Reproducing

Per tree: check out the tag in a worktree, pin ASDF with
`(:source-registry (:directory <tree-root>) :inherit-configuration)`
(the `:tree` form is ambiguous from the main checkout — it sees the
release worktrees' `.asd`s), verify `system-source-directory`, run
`(graph-db/perf-test:run-perf :tag ...)` under
`--dynamic-space-size 16384` with an isolated TMPDIR, three fresh
images, interleaved across trees. Compare with `compare-perf` (reads
all report eras); `check-perf` correctly refuses cross-generation
input.
