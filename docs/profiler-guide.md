# Vivace-Graph Performance Profiling Tool: Complete Usage Guide

The **Vivace-Graph Performance Profiling Tool** is a reusable, modular performance measurement and reporting suite built using SBCL's internal profiling utilities (`sb-sprof` and `sb-profile`) and `cl-typesetting`/`cl-pdf`. It measures execution dynamics across all system layers—from low-level `mmap` memory arenas to high-level Prolog query execution.

---

## 1. Quick Start

### Loading the Profiler
Load the dedicated ASDF system in your Common Lisp REPL:

```lisp
(ql:quickload "graph-db/profiler")
```

### Running the Profiling Suite
Execute all 8 subsystem benchmarks with default scaling (1.0) and print a comparative REPL report:

```lisp
(graph-db/profiler:run-full-profiling-suite)
```

### End-to-End Execution & PDF Report Generation
Run the full benchmark suite and automatically compile a multi-page PDF report complete with vector graphs, performance tables, and technical analysis:

```lisp
(graph-db/profiler:profile-and-generate-pdf
  :output-file "/tmp/vivace_graph_profiling_report.pdf"
  :scale 1.0
  :sprof-mode :cpu)
```

---

## 2. Profiling Architecture

The profiling suite operates across 3 integrated stages:

```
[ Stage 1: Core Instrumentation ]
  - Function Registry (get-subsystem-functions)
  - Statistical Profiling (with-sprof-profiling / SB-SPROF)
  - Deterministic Tracing (with-sb-profile-tracing / SB-PROFILE)
  - Unified Macro (profile-block)

[ Stage 2: Modular Subsystem Profilers ]
  - MMAP & Storage Arena
  - Serialization & Binary Codecs
  - Index Backends (Skip-List & B+ Tree)
  - Graph Node/Edge Storage & Lookup
  - Transactions & ACID OCC Validation
  - Views & Map/Reduce Indexing
  - Spatial Geohash & GEOS CFFI Bridge
  - Prolog Engine & Functor Unification

[ Stage 3: Visual PDF Reporting Suite ]
  - Executive Summary & System Health Matrix
  - 2-Pass Vector Bar Charts (Real Time & Memory Allocation)
  - SB-SPROF Hotspot Tables & SB-PROFILE Function Tracing Tables
  - Per-Subsystem Technical Analysis & Optimization Recommendations
```

---

## 3. Subsystem Modules

The profiler covers 8 core subsystems in `vivace-graph`:

| Subsystem Module | Subsystem Key | Benchmarked Operations |
| :--- | :--- | :--- |
| **MMap & Memory Storage** | `:mmap-storage` | SAP byte/slice access, heap arena block allocation (`allocate`, `free`). |
| **Serialization & Codecs** | `:serialization` | Double-float, UUID, string octet encoding/decoding, key serialization. |
| **Index Backends** | `:index-backends` | Skip-List node traversals/locks, B+ Tree page decode & binary search. |
| **Graph Storage & Lookup** | `:graph-storage` | Vertex/edge creation, type index lookups, UUID hash table indexing. |
| **Transactions & ACID OCC** | `:transactions` | Transaction start/commit, read-set vs write-set conflict validation. |
| **Views & Map/Reduce** | `:views` | View map/reduce indexing, tuple sorting, view index updates. |
| **Spatial Index & GEOS** | `:spatial` | Geohash cell calculation, bounding box queries, GEOS CFFI operations. |
| **Prolog Query Solver** | `:prolog` | Functor symbol creation, predicate compilation, term unification (`unify`, `deref-exp`). |

---

## 4. API Reference

### 4.1 Profiling Block Harness (`profile-block`)

Unified macro executing code with full instrumentation: timing, memory allocation, `sb-sprof` sampling, and `sb-profile` deterministic tracing.

```lisp
(graph-db/profiler:profile-block (:name "Custom Index Benchmark"
                                  :subsystems '(:index-backends)
                                  :sprof-mode :cpu)
  ;; Code to profile
  (dotimes (i 1000)
    (graph-db::make-skip-list)))
```

**Parameters**:
- `:name` (*string*): Human-readable name for the benchmark block.
- `:subsystems` (*list* or *symbol*): Subsystem key(s) to monitor (e.g. `'(:graph-storage :views)` or `:all`).
- `:sprof-mode` (*symbol*): `:cpu` (default), `:alloc`, or `:time`.

---

### 4.2 Modular Benchmark Suite (`run-full-profiling-suite`)

Run all or selected subsystem profiler modules.

```lisp
(graph-db/profiler:run-full-profiling-suite
  :subsystems :all
  :scale 0.5
  :sprof-mode :cpu)
```

**Parameters**:
- `:subsystems` (*list* or *symbol*): Subsystems to profile (default `:all`).
- `:scale` (*float*): Iteration scaling factor (default `1.0`). Use `0.1` for quick checks or `2.0` for heavy benchmarks.
- `:sprof-mode` (*symbol*): `:cpu`, `:alloc`, or `:time`.

---

### 4.3 PDF Report Generation (`generate-pdf-report` & `profile-and-generate-pdf`)

Generates a publication-grade PDF report with vector performance graphs, comparative matrices, and function call tracing tables using `cl-typesetting` and `cl-pdf`.

```lisp
;; 1. Generate PDF from an existing suite result
(graph-db/profiler:generate-pdf-report suite-result
  :output-file "/tmp/my_report.pdf"
  :title "Vivace-Graph Performance Benchmark")

;; 2. End-to-end wrapper (runs suite and generates PDF)
(graph-db/profiler:profile-and-generate-pdf
  :output-file "/tmp/vivace_graph_profiling_report.pdf"
  :scale 1.0
  :sprof-mode :cpu)
```

---

## 5. Registering Custom Subsystem Functions

To add custom functions to a subsystem's tracing registry for `sb-profile` monitoring:

```lisp
(in-package #:graph-db/profiler)

(register-subsystem-function :graph-storage 'graph-db:find-vertex)
(register-subsystem-function :prolog 'graph-db:prolog-compile)
```

---

## 6. Sample REPL Output

```text
========================================================================
VIVACE-GRAPH PROFILING SUITE COMPARATIVE REPORT
Timestamp: 2026-07-28T14:10:11Z
========================================================================
  Module / Profile Run           | Real Time (ms) | CPU Time (ms) | Memory Consed
------------------------------------------------------------------------
  MMap & Memory Storage Profilin |         294.52 |        294.22 |      69.32 MB
  Serialization & Binary Codecs  |          42.99 |         43.05 |      17.18 MB
  Index Backends (500 entries)   |         100.88 |        100.87 |      35.90 MB
  Graph Storage & Lookup (250 ve |         118.03 |        118.07 |      68.53 MB
  Transactions & ACID OCC Engine |          94.38 |         76.97 |      17.01 MB
  Views & Map/Reduce Indexing (2 |         342.91 |        343.32 |     145.62 MB
  Spatial Geohash & GEOS Bridge  |         141.81 |        141.90 |      57.84 MB
  Prolog Query Solver (1,000 que |           6.77 |          6.77 |       2.69 MB
========================================================================
```
