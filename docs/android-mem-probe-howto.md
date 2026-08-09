# mem-probe: on-device memory split (for the Android/ECL harness)

**Purpose.** Answer the open question from `android-memory-analysis.md` §5 with numbers
*from the AOT aarch64 runtime*, before anyone designs a fix. Two questions in particular:

1. **How much of the ~800 MB-at-open is the type index?** The engine eagerly materializes
   **one `index-list` per possible type — `+max-node-types+` = 65536 — for BOTH the vertex and
   edge type indexes, at open, regardless of how many types the schema uses** (plus 65536 locks
   each). That is 131,072 `index-list` structs + 131,072 lock objects, structural, non-weak,
   present on every impl. On a desktop 500-vertex graph it is essentially the entire non-baseline
   footprint. `mem-probe` reports it as the `STRUCTURAL` total. This is a *different* root cause
   than the weak-value cache and is not blocked on the ECL bug — but we need the on-device MB.

2. **Is the node cache actually strong on the AOT runtime under a real query?** On *desktop*
   native ECL 26.5.5 the isolated weak-value defect reproduces (a lone `:weakness :value` entry is
   retained across GC), yet the *full engine's* node cache did **not** grow unbounded in a synthetic
   lookup workload — it collapsed after GC like SBCL. So desktop can't settle it. Only the
   on-device probe, over a real `site-finds`/`site-detail` query, can.

The tool lives at `tools/mem-probe.lisp` in the engine repo. It is **not** in `graph-db.asd`;
it is instrumentation you compile into the field build (or paste into `ma-device.lisp`).

---

## 1. Wire it into the build

`mem-probe.lisp` is just `(in-package :graph-db)` + function defs. For the AOT build, add it as a
source file compiled **after** `graph-db/core`, or paste its defuns into `ma-device.lisp`. It uses
only core accessors (`cache`, `heap`/`memory-cache`, `ve-index`/`vev-index`, `type-index`,
`skip-list`, `views`, `spatial-index`), so `graph-db/core` is enough — no REST, no GEOS.

## 2. Reuse the harness's heap FFI (one line at startup)

The probe's built-in heap reader tries `GC_get_heap_size` via cffi, which should match what the
harness already logs. If the harness has its own wrapper, inject it so the numbers are identical:

```lisp
(in-package :graph-db)
(setf *probe-heap-fn* (lambda () (ma-device::gc-heap-size)))       ; GC_get_heap_size (high-water)
(setf *probe-free-fn* (lambda () (ma-device::gc-free-bytes)))      ; GC_get_free_bytes (free list)
```

`GC_get_heap_size` is **high-water** — it never shrinks when objects die, so it CANNOT tell a real
leak from a transient allocation peak. The probe now also reads `GC_get_free_bytes` and reports
**LIVE = heap − free**, which is the number that actually answers "is this retained?". If the AOT
build doesn't resolve `GC_get_free_bytes` by name, wire `*probe-free-fn*` to a harness FFI wrapper;
without it, LIVE shows `[n/a]` and you're back to high-water only.

Pass the harness's log stream to every probe via `:stream` (defaults to `*standard-output*`, which
on Android may go nowhere) — e.g. the stream backing `files/ecl-out.log`.

## 3. The one call that produces the split

`run-cache-split` runs the whole A/B/C sequence. `QUERY` is a 0-arg thunk that runs ONE real query
(e.g. the data-dense `site-finds`); its result is discarded on purpose before the B probe's GC.

```lisp
(in-package :graph-db)
(run-cache-split
  :graph *graph*
  :stream *ecl-log-stream*                       ; the stream behind files/ecl-out.log
  :query (lambda () (ma::site-finds <the-dense-site-id>)))   ; your real query, result dropped
```

It prints three census blocks and a summary line, and returns the three plists.

### Reading the output

```
==== mem-probe [A: after-open (no query yet)] (post-GC)
  heap: <X> MB
  entries: <e> evictable + 131072 structural
    node-cache            ..   EVICTABLE
    vertex-type-cache  65536   STRUCTURAL
    edge-type-cache    65536   STRUCTURAL
    ... skip-list node caches ...
==== mem-probe [B: after-query + drop + GC] ...
==== mem-probe [C: after clear-all-caches + GC] ...
---- split: open-baseline=<A> MB  query-retained(B)=<B> MB  evictable-reclaimed(B->C)=<B-C> MB
```

- **A.LIVE** = the true open-time baseline. `structural` is fixed at 131072 (the eager type index);
  if the evictable cache counts are ~0 (they were, round 1), A.LIVE is base image + compiled code +
  those 131072 type-index structs/locks.
- **LIVE(B) − LIVE(A)** = did the query TRULY retain, or just high-water the heap? If LIVE returns
  to ~A after the query result is dropped+GC'd, the +100–180 MB heap jump is a transient peak Boehm
  won't unmap (an RSS/allocation-volume problem), not a leak. If LIVE stays high, something retains
  it — find the references.
- **LIVE(B) − LIVE(C)** = MB the evictable read-through caches were pinning. Round 1 this was 0 (the
  caches were already empty), which ruled the node/read-through caches OUT for this workload.

Round-1 result (720-find graph, S24 Ultra): node-cache and all read-through caches were **empty**
through A/B/C; open high-water ≈ 785 MB; each heavy query high-watered +112–176 MB and never came
down. So the bounded-cache / weak-value track does nothing here. Round 2 (this LIVE metric) decides
whether the per-query growth is a leak or Boehm high-water, and confirms the type index dominates
A.LIVE.

Round-2 result: **LIVE genuinely grew +107–151 MB per query and stayed after drop+GC** (not just
high-water), while clear-all-caches reclaimed only ~5 MB. So it's real retained *or* fragmented
memory, and it is NOT in any VG cache. Open-baseline LIVE ≈ 750 MB.

## Round 3: leak vs fragmentation (mem-probe-repeat)

The remaining question: does that +150 MB/query keep accumulating (a true leak → OOM) or plateau
(non-moving Boehm fragmentation / high-water → bounded)? Run the SAME query N times and watch LIVE:

```lisp
(in-package :graph-db)
(mem-probe-repeat
  :graph *graph* :n 8 :stream *ecl-log-stream*
  :query (lambda () (ma::site-finds <the-dense-site-id>)))   ; same dense query each iter
```

It logs `heap / free / LIVE / dLIVE` per iteration. Read `dLIVE`:
- **stays positive and roughly steady** across all 8 → a **true leak** (reachable per-query
  accumulation; next: an ECL `(room)` / typed census to name the type — Prolog compiled lambdas,
  interned symbols, cl-json buffers are the prime suspects).
- **large on iter 0–1 then → ~0** → **Boehm fragmentation / high-water**, bounded; the fix is
  reducing query allocation churn (app slim-query + engine buffer reuse) and/or Boehm tuning
  (`GC_set_free_space_divisor`, `GC_enable_unmap`), not chasing a leak.

Send back the 8-row table for `site-finds` (and ideally `site-detail`).

## 4. Notes / safety

- **Read-only safe.** `clear-all-caches` clears only read-through caches (re-read from the mmap
  heap on next access); it deliberately leaves the `type-index` caches alone (those *are* the index
  — clearing them live would hide every node).
- Every probe forces `(ext:gc t)` first (`:gc nil` to see a transient peak instead).
- For a warm-restart profile, run `run-cache-split` right after open with import skipped — the
  analysis doc notes the heavy queries recur every launch, so A and B should reproduce cold-vs-warm.
- Re-run per scenario (`site-detail` vs `site-finds`) by swapping the `:query` thunk.

Send back the three blocks + the summary line for each scenario and we'll pick between the
bounded-cache and lazy-type-index directions from measured numbers.
