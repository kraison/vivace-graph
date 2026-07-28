# Vivace-Graph Performance Profiling Tool

This directory contains the source code for the **Vivace-Graph Performance Profiling Tool**, a modular benchmarking and PDF reporting suite built using SBCL's `sb-sprof` and `sb-profile` tools alongside `cl-typesetting` and `cl-pdf`.

For the complete usage guide, API reference, and examples, see:
- [`docs/profiler-guide.md`](file:///Users/kraison/work/vivace-graph-v3/docs/profiler-guide.md)

---

## Directory Layout

- [`package.lisp`](file:///Users/kraison/work/vivace-graph-v3/profiling/package.lisp): Package definition (`graph-db/profiler`) and exports.
- [`registry.lisp`](file:///Users/kraison/work/vivace-graph-v3/profiling/registry.lisp): Subsystem function registry for function tracing.
- [`sprof.lisp`](file:///Users/kraison/work/vivace-graph-v3/profiling/sprof.lisp): `sb-sprof` statistical sample profiling wrapper and parser.
- [`profile.lisp`](file:///Users/kraison/work/vivace-graph-v3/profiling/profile.lisp): `sb-profile` deterministic tracing wrapper and parser.
- [`harness.lisp`](file:///Users/kraison/work/vivace-graph-v3/profiling/harness.lisp): Core unified `profile-block` macro harness.
- [`modules/`](file:///Users/kraison/work/vivace-graph-v3/profiling/modules/): Subsystem-specific profiler modules:
  - `mmap.lisp` (SAP byte access & memory arena allocation)
  - `serialization.lisp` (Binary codecs & key encoders)
  - `index.lisp` (Skip-List & B+ Tree index backends)
  - `graph.lisp` (Vertex & edge creation, lookup, UUID hash tables)
  - `transactions.lisp` (Transaction commit & OCC conflict validation)
  - `views.lisp` (View map/reduce indexing & tuple sorting)
  - `spatial.lisp` (Geohash cell math & GEOS CFFI operations)
  - `prolog.lisp` (Prolog engine, predicate compilation, & unification)
  - `suite.lisp` (Full suite runner `run-full-profiling-suite`)
- [`reporting/`](file:///Users/kraison/work/vivace-graph-v3/profiling/reporting/):
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
