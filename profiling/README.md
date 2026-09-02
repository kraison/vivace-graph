# Vivace-Graph Performance Profiling Tool

This directory contains the source code for the **Vivace-Graph Performance Profiling Tool**, a modular benchmarking and PDF reporting suite built using SBCL's `sb-sprof` and `sb-profile` tools alongside `cl-typesetting` and `cl-pdf`.

This harness answers **"why is it slow?"** (where the time goes). The
throughput suite in `tests/perf/` (`graph-db/perf-test`) answers the other
question — **"did it get slower?"** — with report files, per-host baselines
and `check-perf` gating. See:

- Chapter 19 of [`docs/vivace-graph-v3-doc.org`](../docs/vivace-graph-v3-doc.org)
  — the perf-vs-profiling split and how to run each.
- [`docs/perf-baselines.md`](../docs/perf-baselines.md) — the baseline /
  regression-gating ritual for the throughput suite.

For the complete usage guide, API reference, and examples, see:
- [`docs/profiler-guide.md`](../docs/profiler-guide.md)

---

## Directory Layout

- [`package.lisp`](./package.lisp): Package definition (`graph-db/profiler`) and exports.
- [`registry.lisp`](./registry.lisp): Subsystem function registry for function tracing.
- [`sprof.lisp`](./sprof.lisp): `sb-sprof` statistical sample profiling wrapper and parser.
- [`profile.lisp`](./profile.lisp): `sb-profile` deterministic tracing wrapper and parser.
- [`harness.lisp`](./harness.lisp): Core unified `profile-block` macro harness.
- [`modules/`](./modules/): Subsystem-specific profiler modules:
  - `mmap.lisp` (SAP byte access & memory arena allocation)
  - `serialization.lisp` (Binary codecs & key encoders)
  - `index.lisp` (Skip-List & B+ Tree index backends)
  - `graph.lisp` (Vertex & edge creation, lookup, UUID hash tables)
  - `transactions.lisp` (Transaction commit & OCC conflict validation)
  - `views.lisp` (View map/reduce indexing & tuple sorting)
  - `spatial.lisp` (Geohash cell math & GEOS CFFI operations)
  - `prolog.lisp` (Prolog engine, predicate compilation, & unification)
  - `suite.lisp` (Full suite runner `run-full-profiling-suite`)
- [`reporting/`](./reporting/):
  - `pdf.lisp` (PDF generation & 2-pass vector graph rendering engine)

---

## Quick Usage

```lisp
;; Load system
(ql:quickload "graph-db/profiler")

;; Run full suite in REPL
(graph-db/profiler:run-full-profiling-suite)

;; Run full suite & generate PDF report
(graph-db/profiler:profile-and-generate-pdf
  :output-file "/tmp/vivace_graph_profiling_report.pdf"
  :scale 1.0)
```

---

## SB-PROFILE coverage

Check what a run will actually trace **before** trusting its numbers:

```lisp
(graph-db/profiler:subsystem-coverage-report)
```

Things worth knowing:

- **Generic functions are profiled.** SB-PROFILE encapsulates a GF and reports
  calls aggregated across its methods. This matters because the engine's hottest
  entry points are all generic: `serialize`, `deserialize`, `deserialize-help`,
  `lookup-vertex`, `get-bytes`, `node-geometry`, and every GEOS topology
  operation — 461 symbols. They were previously filtered out, which left the
  serialization workloads structurally unable to observe serialization.
- **Subsystem keys accept aliases.** `:graph-storage` and `:index-backends` were
  requested by the workloads but registered by nothing, so they silently traced
  zero functions. They now resolve via `*subsystem-aliases*`.
- **`graph-db/geos` is a hard dependency.** Without it the GEOS layer is not
  fbound and only one `:geos` function exists to trace.
- **The registry rebuilds per run.** It used to be a snapshot taken when
  `registry.lisp` loaded, so anything loaded afterwards was untraceable. Set
  `*auto-refresh-registry*` to `nil` to pin a hand-built registry.
- **Inline functions can never be traced.** ~24 functions are `declaim inline`,
  so their call sites are open-coded and no encapsulation is possible. Use
  `sb-sprof` to see them.
- **Completeness costs fidelity.** Instrumenting ~1,900 functions adds per-call
  overhead everywhere and inflates the very thing you are measuring. Use the
  curated `:hot-path` subsystem for low-distortion runs and `:all` when coverage
  matters more than precision.

## Units and precision

Per-call cost is reported in **microseconds**, and comes from SB-PROFILE's raw
counters (`*PROFILED-FUN-NAME->INFO*`), not from parsing `sb-profile:report`.

This matters more than it sounds. The text report prints totals at three decimal
places and sec/call at six, so every function faster than 1 µs — which is most of
the hot ones — printed as `0.000000`, and no change of display unit could recover
it. Reading the counters directly (a tick *is* a microsecond, since
`internal-time-units-per-second` is 1e6 on SBCL) took zero-valued per-call rows in
a full suite run from **807 down to 7**. Text parsing remains as a fallback if a
future SBCL stops exposing the internals.

## The profiler warns when it is measuring itself

`sb-profile` encapsulates every function it traces, so a function called a
million times pays a million encapsulations. Past some call volume the reported
time is mostly the profiler observing itself.

This is not theoretical — it produced a wrong bug report. Tracing the
slot-access path (`persistent-p`/`meta-p`/`ephemeral-p`, ~1M calls each)
attributed **3,214 ms across a workload that takes 887 ms uninstrumented**: more
measured time than real time.

So every run now measures its own encapsulation cost (~0.49 µs/call on the dev
host — note SBCL's own `sb-profile::*overhead*` reports the *timer* cost of
~0.006 µs and understates this by ~80×), estimates each row's instrumentation
share, and flags the rows it cannot vouch for:

```
     Calls |  Total ms |  us/call |   Consed | Bytes/Call | ! | Symbol
   342,000 |     77.50 |    0.227 |      0 B |          0 | ! | GRAPH-DB::PERSISTENT-P
```

`!` means the estimated overhead is ≥ `*overhead-warn-fraction*` (25%) of the
reported time. A share over 100% — common for million-call functions — means the
estimate exceeds the whole reported time. **Call counts remain exact; only the
times are compromised.** A run whose attributed time exceeds its own wall clock
is called out explicitly.

The warnings appear in the console tables and in the PDF. To measure something
in this class, use `sb-sprof` sampling or a before/after wall-clock comparison
against a `:subsystems '(:nothing)` baseline, which traces nothing.

## Real-world workloads

`modules/real_world.lisp` models the **measured** production shape of a
downstream application: 481 parcels (451 polygon / 24 multipolygon / 4 point;
outer ring median 13 vertices, p90 38, max 176), 9,699 point items (~20 per
parcel), 467 passes, 481 stations.

| # | Workload | Notes |
|---|----------|-------|
| 1 | Parcel/Item Bulk Ingestion | polygons + points at the measured ratio |
| 2 | Spatial Map Viewport Query | the app's hottest read path |
| 3 | Analytical View Rollup | items grouped by item family |
| 4 | Prolog Engine | **synthetic** — the app does not use Prolog |
| 5 | Concurrent Field-Operator Txns | sequential; measures per-txn cost, not contention |
| 6 | Complex Node Serialization | 25 KB text + 512 double-floats |
| 7 | GEOS Coverage Remainder | **new**; skipped if libgeos_c is absent |
| 8 | Large-Polygon Materialization | **new**; the issue #50 shape |
| 9 | Zone Control History | **new**; the app's slowest path (1,125 ms/pin) |
| 10 | Event Dropped-Pin Radius Query | **new**; 10 km diameter over 286k events |
| 11 | Corpus Vector Retrieval | **new**; cosine top-K over 1024-dim segment |

Workloads 9–11 model the event-history page and the retrieval corpus — the
slowest part of the application. Their shape is measured, not assumed:

| Path | Production latency | Shape |
|---|---|---|
| zone-history profile | **1,125 ms** per pin | ~1,500 days × ~4 zones |
| geo-scope query r=5 km | 169 ms (462 events) | of 286,198 events |
| geo-scope query r=25 km | 195 ms (812 events) | " |
| corpus vector segment | — | 23,193 vectors × 1,024 dims |

Control zones are all multipolygons, median 1,480 vertices (max 4,111). The
zone-history walk is deliberately index-free — a day holds only ~4 polygons,
so an index would add overhead for no selectivity — and the workload reproduces
that rather than "fixing" it.

Two caveats when reading results:

- Workload 4 is **not** application-representative. The downstream app makes
  zero use of the Prolog engine; the workload is retained only because Prolog
  is a real graph-db subsystem.
- Workload 5 is **sequential** despite the name. It measures per-transaction
  overhead, not contention between concurrent writers.
