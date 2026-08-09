# VivaceGraph on-device (Android / ECL): memory analysis + proposals

**From:** the Mine Action app side (VG-on-ECL Android field client).
**Audience:** the VG engine team.
**Status:** **RESOLVED (2026-07-02)** by graph-db/core `#47` — see the banner directly below. Everything
from §0 on is the diagnostic record (rounds 1–3) that led to the fix; kept for context.

---

## STATUS — RESOLVED (2026-07-02) by graph-db/core #47

The investigation below (rounds 1–3) traced the on-device footprint to a **per-read `change-class`** on
ECL. VG's fix — commit `30d1837`, *"build nodes as their subclass directly on ECL (no per-read
change-class)"* (`edge.lisp` / `vertex.lisp` / `buffer-pool.lisp`) — **resolves it.** Verified on the AOT
build (Galaxy S24, clean recompile of `graph-db/core`), `mem-probe-repeat` N=8 over the dense queries:

| metric | before (leaking) | after #47 |
|---|---|---|
| dLIVE / query (×8) | +143–169 MB, steady | **≈ 0 (−1 … +2.5 MB), flat** |
| site-finds LIVE iter 0→7 | 896 → 1999 MB | **268.6 → 267.1 MB** |
| site-detail LIVE iter 0→7 | 850 → 1695 MB | **270.0 → 271.2 MB** |
| open-baseline LIVE (A) | ~750 MB | **269.3 MB** |
| query-retained (B−A) | +151 MB | **+0.4 MB** |
| repeat high-water | → 2386 MB | **plateaus ~462 MB** |

Two takeaways: (1) the per-read `change-class` was **both** the monotonic leak **and** a large baseline
inflator — killing it flattened the per-query curve *and* cut open-baseline LIVE ~2.8×. (2) The 131072
structural type-index count is unchanged, so it is now only a **small** slice of the 269 MB baseline (the
old ~750 MB was mostly change-class'd node objects retained from boot's traversal, not the index).
**⇒ the lazy/paged type index (§4.3) is now a minor, non-urgent follow-on, not the primary lever;** the
node-cache/weak-value track (§0-bis, §4.0/4.1) stays closed.

---

## 0. TL;DR — MEASURED on-device (2026-07-01, LIVE follow-up 2026-07-02); supersedes the weak-value hypothesis

We ran the §5 split on the AOT aarch64 build (Galaxy S24, ECL 26.5.5) with `mem-probe.lisp`'s
`run-cache-split`, over the two dense real queries. **Three findings that redirect the fix** (1–2 from
2026-07-01; 3 from the LIVE follow-up 2026-07-02):

**1. The node cache is NOT strong on-device.** After the dense `site-finds` query (720 finds) +
dropping its result + GC, the **node-cache count is 0** — in BOTH `site-finds` and `site-detail`
scenarios. The full engine's cache **collapses via GC** on the AOT runtime; the isolated
`:weakness :value` no-op (§0-bis, still real) does **not** manifest as unbounded node retention here.
⇒ a bounded/evictable node cache would reclaim ~nothing on-device (measured `evictable-reclaimed
B→C = 0.0 MB`). The weak-value proposals (§4.0/§4.1) are **not the on-device lever.**

**2. The ~800 MB open baseline is the eager type index — FIXED, not per-vertex.** At open every
evictable cache is ≈0, yet heap = ~770–820 MB, and the only large structural thing is the
**131,072 `index-list` structs + 131,072 locks** (65536 = `+max-node-types+`, ×{vertex,edge}). That
overhead is **constant regardless of graph size** — the earlier "~1 MB/vertex" was 766 MB ÷ 767
vertices, a coincidence misattributing fixed overhead to per-vertex data. ⇒ the **lazy/paged type
index (§4.3) is the primary lever.**

**3. A dense query RETAINS ~110–150 MB — outside every probed cache (LIVE follow-up).** `GC_get_heap_size`
alone is a **high-water** metric (Boehm never unmaps), so it can't separate retention from a transient
peak. Adding `GC_get_free_bytes` gives **LIVE = heap − free**, which *does* shrink when memory frees.
Re-run with LIVE (both scenarios, post-GC): the query grows LIVE by **+107–151 MB and it STAYS** —
`clear-all-caches` recovers only ~5 MB and node-cache is 0. So this is **real retention, not transient**
(correcting our first read), held **somewhere the census doesn't cover** — not the node cache, not the
evictable read-through caches, not the fixed type index. That unknown retention path is now a lever in its
own right. (The high-water A→B of +160–224 MB = this real retention **plus** a smaller pure-transient tail
Boehm won't unmap.)

### Measured census — LIVE = heap−free (post-GC; `structural` = 131072 every block; `evictable` ≈0 at A)

| scenario | A open LIVE (high-water) | B after-query LIVE | **dLIVE retained** | reclaimed by clear-all (B→C) | node-cache @B |
|---|---|---|---|---|---|
| `site-finds` (720 finds) | 750.7 MB (914.6) | 901.9 MB | **+151.2 MB** | 5.5 MB | **0** |
| `site-detail` | 757.2 MB (927.4) | 863.9 MB | **+106.6 MB** | 5.3 MB | **0** |

*(The 2026-07-01 heap-only run — before the LIVE hook — read: site-finds A/B 766.8/958.8, site-detail
816.6/896.6 MB, evictable-reclaimed 0.0 MB. Same shape; LIVE just resolves how much of the A→B bump is
truly retained.)*

### Revised priorities
1. **Lazy/paged type index (§4.3)** — materialize the ~24 used schema types, not 65536×2. Attacks the
   baseline directly; independent of the ECL weak-value bug.
2. **The ~110–150 MB per-query LIVE retention (finding 3)** — find what a dense query retains post-GC
   *outside* the node/evictable caches and the type index. Candidates to instrument next: spatial-index
   materialization, view skip-list internals, deserialized node data pinned off `(cache graph)`, or
   allocator structures not counted as a "cache." Separately, Boehm `GC_gcollect_and_unmap` would return
   the pure-transient tail so the reported heap tracks LIVE rather than the high-water peak.
3. **Bounded node cache (§4.0/§4.1)** — DEMOTED for on-device (the cache already collapses); may still
   matter on SBCL/server.

Harness: `mem-probe.lisp` is compiled into the field build and reachable as a `mem-probe` query
(`run-cache-split` over site-finds/site-detail), re-runnable per scenario via an intent extra.

---

## 0-bis. Prior hypothesis (weak-value cache) — a real ECL defect, but measured NOT to be the driver

The node cache `(cache graph)` is a **weak-on-value** hash table (`make-id-table` →
`#+ecl (make-hash-table :weakness :value)`), by design self-limiting: a node stays cached only
while something *else* references it, otherwise it's collected. **On ECL 26.5.5 that weakness is a
silent no-op** — `:weakness :value` tables (and `ext:make-weak-pointer`) never collect; the table is
effectively **strong**. So on-device every node a query traversal touches is retained forever →
the ~800 MB–1.1 GB / ~1 MB-per-vertex footprint. SBCL honors weak-value, which is why the
server/desktop is fine and the phone isn't.

Empirically, on ECL 26.5.5 (rigorous off-stack test, both `:weakness` tables and weak-pointers):

| weakness | result |
|---|---|
| `:key` | collected — **honored** |
| `:value` | **retained — NO-OP** |
| `:key-and-value` | collected (via the key side) |
| `:key-or-value` | collected (via the key side) |
| `ext:make-weak-pointer` | **retained — NO-OP** |
| `trivial-garbage` weak-value table / weak-pointer | **retained — NO-OP** (delegates to native) |

So: value-position weak references don't work on ECL, which (a) breaks the cache and (b) rules out a
weak-pointer-based hand-rolled workaround. **`trivial-garbage` doesn't help either** — on ECL it's a
thin shim over the same native `(make-hash-table :weakness :value)` (verified: the table it returns
reports `ext:hash-table-weakness → VALUE` and retains identically). No Lisp-level route to value-
weakness exists on this runtime. The fix is an **explicitly bounded cache on ECL**, plus (worth doing)
reporting the weak-value/weak-pointer defect upstream to ECL. Repro script is in §7.

---

## 1. Context

We AOT-cross-compile `graph-db/core` under ECL to `aarch64-linux-android` and run a **read-only**
on-device field client: the phone opens a local `:mine-action` graph, imports a disclosure-scoped
"site working-set" as a JSON bootstrap, and answers the same GeoJSON/JSON queries the web app uses.
No HTTP server, no live replication on-device (yet). `:graph-db/core` only — **no GEOS** (the
read-only query layer's geometry rendering + geodesic area are GEOS-free; topology ops fall back).

The functionally exciting news: **it works** — the real schema + query layer runs on a Galaxy S24
and a Pixel 7 Pro, rendering real sites over a satellite basemap. The problem is **memory**.

**Test data (a real 2-site working-set):** 767 vertices / 1527 edges — 2 sites, 41 surveys,
~720 EO-finds, geometry (survey boundary polygons + find points). ~521 KB of JSON.

---

## 2. What we measured

Instrumented on the Pixel 7 Pro (12 GB) by FFI'ing Boehm's `GC_get_heap_size` and logging
heap + wall-time at phase boundaries (import / snapshot / per-query). All heap figures are the
**Boehm/Lisp heap** (`GC_get_heap_size`), not mmap/RSS, unless noted.

| phase | Lisp heap | wall time | notes |
|---|---|---|---|
| after graph open (before real queries) | **~800 MB – 1.1 GB** | — | ~1 MB/vertex for a 767-vtx graph |
| import (batched, GC between batches) | +65 MB (→ 440 MB) | **22 s** | fine; `ext:gc` after frees ~0 |
| snapshot after import | **+473 MB retained** (→ 913 MB) | **~30 s** | GC after does NOT reclaim it |
| `site-detail` (data-dense site) | +128 MB | **5–6 s** | 145 KB JSON result |
| `site-finds` (data-dense site) | +200 MB | **7.5–8.8 s** | 276 KB JSON, ~700 finds |

- **Net RSS: ~1.8–2 GB warm, spikes to ~3.5 GB cold** (import+snapshot transient). Survives on a
  12 GB phone; not safe on lower-RAM devices.
- **`buffer-pool-size` is not the lever:** dropping it 4000 → 256 made the baseline *worse*
  (802 → 1111 MB), consistent with it being a recycling pool of pre-allocated buffers (smaller pool
  ⇒ more fresh allocation), not a page cache.
- **ECL's GC works on-device** — the import's per-batch `(ext:gc t)` collected cleanly (heap stayed
  at 440 MB across 767 inserts). So this is not a "GC is broken on AOT" problem.
- The heavy queries **recur every launch** (they're not a one-time import cost) — a warm restart
  (import skipped) still opens to ~800 MB and pays the same `site-detail`/`site-finds` cost.

Reproduction: the instrumentation (GC FFI + timing) lives in the app repo's `ma-device.lisp`
(logs to the app's `files/ecl-out.log`), so we can re-run any scenario on request.

---

## 3. Analysis

### 3.1 Deserialization is *not* the main cost
The hot path is already lean. `serialize.lisp` is a fixed-layout binary format: `deserialize-help`
dispatches on a type byte and reads fixed offsets (`deserialize-uint64 bytes 0/8/16`), and there is
already a **`deserialize-help-mmap`** variant that decodes directly from the mmap'd bytes. So a node
deserialize is a few field reads + a struct alloc — cheap. The genuine exception is **variable-length
geometry**: materializing a survey polygon's coordinate vector is real allocation regardless of
caching, and it's a big part of why `site-finds`/survey reads are heavy.

### 3.2 The cost is *retention*, not reading
Two retention behaviors compound:

1. **Nodes double-store.** `maybe-init-node-data` (primitive-node.lisp) sets **both**
   `(bytes node)` (the raw serialized bytes read from the heap mmap) **and** `(data node)` (the
   deserialized structure). A materialized node holds the bytes *and* the parsed data.

2. **The node cache is meant to be self-limiting, but isn't on ECL.** `lookup-node` stashes every
   materialized node in `(cache graph)`, a **weak-on-value** table (`make-id-table` →
   `#+ecl (make-hash-table :weakness :value)`). The design is correct: a node should stay cached
   only while referenced elsewhere, else fall out and be collected — no eviction needed. **But ECL
   26.5.5 doesn't honor value-weakness (§0), so the table is strong** and every node a traversal
   touches is kept forever. For a query over ~700 finds + 41 polygons, that whole set
   (bytes + deserialized data + geometry vectors) accumulates and is never released.

So the ~1 MB/vertex isn't the cost of *reading* a node — it's the cost of *never letting go* of one,
because the mechanism meant to let go is a no-op on this runtime.

### 3.3 The ~800 MB at open is (probably) eager index materialization
The ~800 MB is present **after open, before the app runs any real query**, so it's not (only) the
node cache filling on traversal — it points at the indexes (skip-lists / ve-index / vev-index /
spatial-index; `open-graph` → `init-spatial-index`) being materialized into the Lisp heap on open.
We're **not certain** of the split — that's the first thing worth measuring (see §5).

---

## 4. Proposals (roughly by leverage; all lean on seams that already exist)

0. **(Root-cause fix) Give ECL an explicitly bounded cache, since it can't do weak-value.** On ECL
   the weak-on-value `(cache graph)` never evicts, so cap it: a size-bounded LRU (or even a simple
   FIFO/clock with a hard entry count), conditionalized `#+ecl` so SBCL keeps its working weak-value
   table. Evicted nodes' `bytes` + `data` (+ geometry) become garbage the (working) GC reclaims; the
   **OS page cache still holds the mmap'd bytes**, so a re-read is a cheap `deserialize-help-mmap`,
   not a disk hit. This is the direct fix for the measured footprint and needs no new format. Also
   worth: **file the ECL bug** (`:weakness :value` and `ext:make-weak-pointer` not collecting on
   26.5.5) so it's fixed upstream long-term — but VG shouldn't block on that.

1. **A bounded cache is the right default anyway.** Even where weak-value works (SBCL), a hard cap
   protects against pathological working sets; the weak table is an optimization on top, not the only
   guardrail. Consider a bounded LRU everywhere with weak-value as the SBCL fast-path.

2. **Stop double-storing `bytes` + `data`.** Keep one. For scan-heavy paths (`map-vertices` /
   `map-edges`, which the finds query hammers), read just the needed fields *transiently* via
   `deserialize-help-mmap` and don't cache the node at all — `*cache-enabled*` already gates this,
   so a `with-uncached-scan` wrapper may be most of the work.

3. **Lazy / paged indexes** (pending the §5 measurement). If open eagerly materializes index
   structures into Lisp, making them mmap-resident / demand-paged would cut the open-time baseline
   independently of the node cache. This is the bigger change; measure before committing.

4. **(App-side, complementary — we'll do this regardless.)** A slim map-finds query: point +
   confidence only, skipping the per-find ordnance-type + survey lookups and full geometry that make
   `site-finds` 8.8 s / 276 KB. This attacks the geometry/lookup floor from our side.

---

## 5. Recommended first step: measure the split

Before choosing between "bounded cache" and "lazy indexes," a **phased allocation breakdown** in the
engine would tell us where the footprint actually is:

- heap after `open-graph` with **no** traversals (isolates index/open materialization),
- delta from a full `map-vertices`/`map-edges` scan with `*cache-enabled*` on vs off (isolates the
  node cache + double-store),
- delta from materializing geometry specifically.

Our bet: **bounded-LRU node cache + uncached scans** buys most of it and is contained work with knobs
that already exist (`*cache-enabled*`, `(cache graph)`). But we'd rather measure than guess, and the
index question could change the answer.

---

## 6. Caveats / non-goals

- **Geometry is the floor.** Any query that *returns* big polygons must materialize them; caching
  strategy can't remove that, only avoid *retaining* it.
- **Cache-light trades retained memory for transient GC garbage.** That's viable here (GC collects
  on-device), but write-heavy or hot-loop-access workloads still want a cache — hence *bounded LRU*,
  not *cache-off*. The desktop/server profile (where the aggressive caching was designed to be fast)
  is unchanged; this is about making the cache **tunable/bounded** so the phone can dial it down.
- We are **not** asking for a rewrite or a new serialization format — the format and the mmap-direct
  read path are already good. The ask is bounded/evictable caching + not double-storing, with the
  index question as a follow-on.

---

## 7. Reproduce the ECL weakness defect

Runs on stock ECL 26.5.5 (Homebrew), no VG needed. Value stays cached across GC despite `:weakness
:value`; the key side works, proving weakness is wired but value-position isn't.

```lisp
;; ecl --norc --load thisfile.lisp
(defun stuff (h key) (setf (gethash key h) (list (make-array 500))) (values))
(defun churn () (let ((j nil)) (dotimes (i 500000) (setf j (cons i j)))) (values))

(let ((h (make-hash-table :weakness :value :test 'eq)) (k (list 'key)))
  (stuff h k)                                   ; value referenced ONLY by the (weak) table
  (dotimes (i 8) (ext:gc t)) (churn) (dotimes (i 8) (ext:gc t))
  (format t "~&weak-VALUE count=~A  [expect 0, get 1 on ECL 26.5.5]~%" (hash-table-count h)))

(let ((h (make-hash-table :weakness :key :test 'eq)))     ; control: key weakness DOES work
  (setf (gethash (gensym) h) t)
  (dotimes (i 8) (ext:gc t)) (churn) (dotimes (i 8) (ext:gc t))
  (format t "weak-KEY   count=~A  [expect 0, get 0]~%" (hash-table-count h)))

(let ((wp (ext:make-weak-pointer (list (make-array 500)))))
  (dotimes (i 8) (ext:gc t)) (churn) (dotimes (i 8) (ext:gc t))
  (format t "weak-POINTER alive=~A  [expect NIL, get the object on ECL 26.5.5]~%"
          (and (ext:weak-pointer-value wp) t)))
```

On-device (AOT aarch64) the same runtime version is used, and the symptom matches (unbounded cache
growth), so we're confident the AOT build behaves the same — but it's worth a 2-line on-device
confirmation if you want it; the app-side harness can log `(hash-table-count (cache *graph*))` before
and after a `(ext:gc t)` on a warm graph.
