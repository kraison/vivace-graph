# Spatial Index Scoping Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `graph-db`'s single graph-wide spatial index with a registry of per-`(owner-class . slot)` indexes, give each its own geohash precision, bound the insert-side cell cover, and make spatial queries take a required scope.

**Architecture:** The spatial index becomes the fourth member of the `(owner . slot)` index family already established by `unique-indexes`, `vector-segments` and `secondary-indexes` — same registry shape, same sidecar-of-roots, same restore-or-rebuild at open. The insert path caps its geohash cover adaptively and records a per-precision histogram; the query path clamps its covering precision to the histogram's lowest occupied level, which is what keeps a coarse insert from becoming a silent miss.

**Tech Stack:** Common Lisp (SBCL + ECL; CCL on Linux), ASDF/Quicklisp, FiveAM for tests, CLOS MOP for slot options, cl-store for sidecars.

**Spec:** `docs/superpowers/specs/2026-07-24-spatial-index-scoping-design.md`. Section references below (§4, §7.2 …) are to that document.

## Global Constraints

- **Indentation: spaces only, never tabs.** All Lisp files. Expand existing tabs at width 8.
- **Reader conditionals must cover all four implementations.** Any `#+sbcl` branch needs `#+ccl`, `#+ecl`, `#+lispworks` siblings. A per-impl conditional wrapping a value or a macro `,@body` that lacks an `#+ecl` branch silently becomes empty.
- **ECL: no custom hash-table tests.** Use `equalp`; never `:test`/`:hash-function` with a custom function.
- **On-disk format version is 3.** `+spatial-index-format+` goes from 2 to 3 exactly once, in Task 3.
- **Insert cap default is 16384 cells; query cap stays 256** (`+spatial-query-max-cells+`, unchanged).
- **Geohash precision range is `(integer 1 12)`** everywhere.
- **Every task must leave the full suite green.** `sbcl --non-interactive --eval '(asdf:test-system :graph-db)'`
- **Public API names, fixed** (later tasks depend on these exact spellings): `spatial-indexes`, `spatial-index-for`, `spatial-index-coarsest-precision`, `def-spatial-index`, `rebuild-spatial-indexes`, `regenerate-spatial-index`, `regenerate-spatial-indexes`, `audit-spatial-slots`.
- **Removed public API:** `spatial-index` (the graph accessor), `rebuild-spatial-index` (singular, old meaning).

---

## File Structure

**Modified:**

| File | Responsibility after this work |
| --- | --- |
| `spatial-index.lisp` | The index structure only: cover computation, the histogram, insert/remove/query. Knows nothing about graphs or classes. |
| `node-class.lisp` | MOP slot options. Gains `:spatial-precision`; gains `%indexed-slot-owner-name` (moved here from `index.lisp` so both `transactions.lisp` and `index.lisp` can see it). |
| `graph-class.lisp` | `spatial-index` slot → `spatial-indexes` registry hash table. |
| `graph.lisp` | Sidecar save/restore, migration trigger, close. |
| `transactions.lisp` | `node-geometry` returns two values; maintenance routes to `(owner . slot)`; the CR-3.1 sampling warning. |
| `spatial-query.lisp` | Scoped query entry points, scope resolution, Prolog functors, rebuild/regenerate/audit sweeps. |
| `memory-graph.lisp` | Per-index image dump/restore. |
| `peer-streaming.lisp` | Purge routes through the registry. |
| `index.lisp` | Loses its copy of `%indexed-slot-owner-name`. |
| `package.lisp` | Exports. |

**Created:**

| File | Responsibility |
| --- | --- |
| `spatial-registry.lisp` | The graph's `(owner . slot)` registry: get-or-create with a resolved precision, the `def-spatial-index` declaration registry, precision resolution, `install-spatial-indexes`. Loaded late (after `index.lisp`) so it can see both the MOP helpers and the graph; `transactions.lisp` and `graph.lisp` reach it through `declaim ftype` forward declarations, exactly as `graph.lisp:5-8` already does for the unique/secondary index functions. |
| `tests/spatial-scope-tests.lisp` | Scoping, resolution, dedup, error contract, declaration surfaces, audit. |

**Why a new file rather than growing `spatial-query.lisp`:** the registry is about *which* index, the query file is about *what matches*. They change for different reasons and `spatial-query.lisp` is already the busiest spatial file.

---

## Task Order and Why

Each task leaves a green suite. The ordering exists to avoid a state where the registry has landed but queries cannot find anything:

1. **Task 1** is confined to `spatial-index.lisp` and testable with `with-temp-memory` alone — no graph.
2. **Task 2** introduces the registry while `find-nodes-*` temporarily queries the *union* of all indexes. That union reproduces today's behaviour exactly, so all ~80 existing call sites keep passing untouched.
3. **Task 4** then flips the union to a required scope and updates the call sites in one focused mechanical pass.

---

### Task 1: Bounded insert cover and the self-healing clamp

Implements §7.1 and §7.2. Confined to `spatial-index.lisp`; no graph, no persistence yet.

**Files:**
- Modify: `spatial-index.lisp:20-23` (struct), `:90-107` (cover), `:109-123` (insert/remove), `:139-143` (query clamp)
- Test: `tests/spatial-index-tests.lisp`

**Interfaces:**
- Consumes: `%covering-precision`, `geohash-covering` (both `geohash.lisp`, unchanged)
- Produces:
  - `+spatial-insert-max-cells+` → `16384`
  - `(make-spatial-index heap &key precision backend max-cells)` → `spatial-index`
  - `(open-spatial-index heap address &key precision backend max-cells precision-counts)` → `spatial-index`
  - `(spatial-index-max-cells idx)` → `(integer 1 *)`
  - `(spatial-index-precision-counts idx)` → `(simple-array fixnum (13))`, index 0 unused
  - `(spatial-index-coarsest-precision idx)` → `(integer 1 12)`

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-index-tests.lisp`:

```lisp
;;; --- §7: bounded insert cover + self-healing clamp -------------------------

(defun big-poly (min-lon min-lat max-lon max-lat)
  "An axis-aligned rectangle polygon, as one exterior ring (lon lat pairs)."
  (%make-geometry
   :kind :polygon
   :coordinates (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(test insert-caps-oversized-cover
  "A ~18 x 8 degree polygon indexes in bounded time and space at p=7."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx (bid 1) (big-poly 22.1d0 44.4d0 40.2d0 52.4d0))
      ;; Uncapped this would enumerate ~7.7e7 cells and exhaust the heap.
      (is (<= (loop for p from 1 to 12
                    sum (aref (spatial-index-precision-counts idx) p))
              (spatial-index-max-cells idx)))
      ;; The cover was coarsened, so the clamp dropped below storage precision.
      (is (< (spatial-index-coarsest-precision idx) 7)))))

(test clamp-finds-coarse-and-fine-together
  "A small query inside a coarsely-stored polygon returns BOTH it and a
finely-stored point in the same index -- the mixed case a single-node test
would pass by accident."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx (bid 1) (big-poly 22.1d0 44.4d0 40.2d0 52.4d0))
      (spatial-index-insert idx (bid 2) (pt *eo-a*))
      (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
        (is (has-p (bid 1) cands))
        (is (has-p (bid 2) cands))))))

(test clamp-self-heals-on-remove
  "Deleting the oversized geometry restores the clamp with no rebuild."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (poly (big-poly 22.1d0 44.4d0 40.2d0 52.4d0)))
      (spatial-index-insert idx (bid 1) poly)
      (is (< (spatial-index-coarsest-precision idx) 7))
      (spatial-index-remove idx (bid 1) poly)
      (is (= (spatial-index-coarsest-precision idx) 7)))))

(test insert-remove-symmetry-under-coarsening
  "Remove computes the same cell set insert did, so nothing is orphaned."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (poly (big-poly 22.1d0 44.4d0 40.2d0 52.4d0)))
      (spatial-index-insert idx (bid 1) poly)
      (spatial-index-remove idx (bid 1) poly)
      (is (zerop (loop for p from 1 to 12
                       sum (aref (spatial-index-precision-counts idx) p))))
      (is (null (spatial-index-query-bbox idx 22.1d0 44.4d0 40.2d0 52.4d0))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-index-suite))'
```

Expected: FAIL — `The function SPATIAL-INDEX-PRECISION-COUNTS is undefined`. (`insert-caps-oversized-cover` may instead hang or exhaust the heap; that *is* the bug, so kill it and proceed.)

- [ ] **Step 3: Add the constant and the struct fields**

Replace `spatial-index.lisp:20-23` with:

```lisp
;; An insert covers its geometry with at most this many cells.  A SAFETY NET,
;; not a selectivity knob -- per-index precision is that knob.  Sized so nothing
;; that works today changes behaviour: a site-scale 0.05 degree polygon is ~1,444
;; cells at p=7 and a city-scale 0.1 degree one ~5,476, both well under.  Per
;; index and PERSISTED (graph.lisp), never a mutable global: SPATIAL-INDEX-REMOVE
;; recomputes cells from the geometry, so a cap that drifted between an insert and
;; its matching remove would orphan entries permanently.
(alexandria:define-constant +spatial-insert-max-cells+ 16384 :test '=)

(defstruct (spatial-index (:constructor %make-spatial-index) (:predicate spatial-index-p))
  skip-list
  heap
  (precision 7 :type (integer 1 12))
  (max-cells +spatial-insert-max-cells+ :type (integer 1 *))
  ;; COUNTS[p] = how many stored cell entries sit at geohash precision P (1..12);
  ;; element 0 is unused.  A cell string's length IS its precision, so no separate
  ;; bookkeeping is needed at the call sites.
  (precision-counts (make-array 13 :element-type 'fixnum :initial-element 0)
                    :type (simple-array fixnum (13)))
  ;; Cached lowest occupied level -- the query's covering-precision clamp (see
  ;; SPATIAL-INDEX-QUERY-BBOX).  Equals PRECISION when the index is empty.
  (coarsest 7 :type (integer 1 12)))

(defun spatial-index-coarsest-precision (idx)
  "The finest covering precision a query may use against IDX: the lowest geohash
precision at which any cell is currently stored (its own PRECISION when empty).
Covering more finely than this would prefix-range-scan PAST a coarser stored key,
which sorts before the range start -- the silent-miss hole this closes."
  (spatial-index-coarsest idx))

(defun %count-cell (idx cell)
  "Record one stored CELL, lowering the cached coarsest level if it opens a new
coarser one.  O(1)."
  (let ((counts (spatial-index-precision-counts idx))
        (p (length cell)))
    (when (and (zerop (aref counts p)) (< p (spatial-index-coarsest idx)))
      (setf (spatial-index-coarsest idx) p))
    (incf (aref counts p))))

(defun %uncount-cell (idx cell)
  "Un-record one stored CELL.  When the level empties and it was the coarsest,
rescan for the new lowest occupied level -- this is what makes the clamp
SELF-HEALING: delete the oversized geometry and selectivity returns with no
rebuild.  The rescan is 12 iterations and runs only on that transition."
  (let ((counts (spatial-index-precision-counts idx))
        (p (length cell)))
    (when (plusp (aref counts p))
      (decf (aref counts p)))
    (when (and (zerop (aref counts p)) (= p (spatial-index-coarsest idx)))
      (setf (spatial-index-coarsest idx)
            (or (loop for q from 1 to 12 when (plusp (aref counts q)) return q)
                (spatial-index-precision idx))))))
```

- [ ] **Step 4: Thread max-cells through the constructors**

Replace `spatial-index.lisp:60-76` (`make-spatial-index` / `open-spatial-index`) with:

```lisp
(defun make-spatial-index (heap &key (precision 7) (backend *index-backend*)
                                     (max-cells +spatial-insert-max-cells+))
  "Create a new spatial index in HEAP (a MEMORY).  PRECISION sets the geohash
grid resolution (7 ~ 150 m cells, 9 ~ 5 m).  BACKEND (:skip-list / :bplus-tree)
picks the ordered-map engine.  MAX-CELLS bounds the cells one insert may cover;
it is fixed for the life of the index (see +SPATIAL-INSERT-MAX-CELLS+)."
  (%make-spatial-index :skip-list (%spatial-make-sl heap backend)
                       :heap heap :precision precision
                       :max-cells max-cells :coarsest precision))

(defun open-spatial-index (heap address &key (precision 7) (backend *index-backend*)
                                             (max-cells +spatial-insert-max-cells+)
                                             precision-counts)
  "Reopen the spatial index whose ordered map is rooted at ADDRESS in HEAP, with
BACKEND's opener.  PRECISION and MAX-CELLS must match the values used at creation
(both are persisted in the sidecar).  PRECISION-COUNTS is the persisted histogram;
NIL means an empty one, which leaves the clamp unrestricted."
  (let ((idx (%make-spatial-index
              :skip-list (open-heap-index backend :address address :heap heap
                                          :comparison 'reduce-comp-lessp)
              :heap heap :precision precision
              :max-cells max-cells :coarsest precision)))
    (when precision-counts
      (replace (spatial-index-precision-counts idx) precision-counts)
      (setf (spatial-index-coarsest idx)
            (or (loop for p from 1 to 12
                      when (plusp (aref (spatial-index-precision-counts idx) p))
                        return p)
                precision)))
    idx))
```

- [ ] **Step 5: Cap the cover and maintain the histogram**

Replace `spatial-index.lisp:90-123` (`%bbox-cells` through `spatial-index-remove`) with:

```lisp
(defun %bbox-cells (geom precision max-cells)
  "The cells covering GEOM's bbox, at the FINEST precision that stays within
MAX-CELLS and never exceeds PRECISION.  A pure function of (geom, precision,
max-cells) -- which is what makes SPATIAL-INDEX-REMOVE recompute exactly the set
SPATIAL-INDEX-INSERT wrote."
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
    (let ((p (min precision
                  (%covering-precision (max 0d0 (- max-lon min-lon))
                                       (max 0d0 (- max-lat min-lat))
                                       max-cells))))
      (geohash-covering min-lon min-lat max-lon max-lat :precision p))))

(defun %geometry-cells (geom precision max-cells)
  "The geohash cells (strings) GEOM occupies.  A point yields one cell; a
polygon/linestring yields the capped grid over its bbox.  A multipolygon is
covered PART BY PART (not by one overall bbox) so the empty gaps between
separated parts are not indexed, each part drawing on an equal share of
MAX-CELLS so one huge part cannot starve the rest."
  (if (eq (geometry-kind geom) :multipolygon)
      (let* ((parts (geometry-coordinates geom))
             (budget (max 1 (floor max-cells (max 1 (length parts)))))
             (seen (make-hash-table :test 'equal))
             (cells '()))
        (dolist (poly parts cells)
          (dolist (c (%bbox-cells (%make-geometry :kind :polygon :coordinates poly)
                                  precision budget))
            (unless (gethash c seen)
              (setf (gethash c seen) t)
              (push c cells)))))
      (%bbox-cells geom precision max-cells)))

(defun spatial-index-insert (idx node-id geom)
  "Index NODE-ID under every cell GEOM occupies.  NODE-ID is a node's 16-byte
uuid; it is folded into the composite key (cell . node-id) and the skip-node
value is unused (NIL)."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)
                                   (spatial-index-max-cells idx))
                  node-id)
      ;; Count what the STORE actually gained, not what we attempted: every
      ;; backend returns NIL for a duplicate-key no-op.  The histogram must track
      ;; physical entries or it drifts out of step with the store.
      (when (add-to-skip-list sl (list cell node-id) nil)
        (%count-cell idx cell)))))

(defun spatial-index-remove (idx node-id geom)
  "Remove NODE-ID's entries for GEOM (using the same cells INSERT produced).
Each (cell . node-id) is a unique composite key, so REMOVE takes the O(log n)
duplicate-free path."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)
                                   (spatial-index-max-cells idx)))
      ;; Gate on the actual removal.  Removing an entry that is not there is a
      ;; supported no-op in this engine (apply-peer-purge is documented idempotent;
      ;; recover-transactions re-applies unmarked .txn files after a crash), and an
      ;; unconditional decrement on such a call drives the histogram BELOW the
      ;; store.  That reports COARSEST-PRECISION finer than reality, which makes
      ;; every query cover past the coarse keys still physically present -- nodes
      ;; vanish silently and the self-heal rescan cannot recover, because it reads
      ;; the same corrupted histogram.
      (when (remove-from-skip-list sl (list cell node-id))
        (%uncount-cell idx cell)))))
```

- [ ] **Step 6: Clamp the query**

In `spatial-index.lisp`, in `spatial-index-query-bbox`, replace the `cover-prec` binding:

```lisp
         (cover-prec (min (spatial-index-precision idx)
                          (%covering-precision (max 0d0 (- max-lon min-lon))
                                               (max 0d0 (- max-lat min-lat))
                                               +spatial-query-max-cells+)
                          ;; Never cover FINER than the coarsest stored cell: a
                          ;; prefix range scan [cell, cell+"{") reaches only keys
                          ;; at or finer than CELL, and a coarser stored key sorts
                          ;; before the range start.  Without this, a capped insert
                          ;; would be silently invisible.
                          (spatial-index-coarsest-precision idx)))
```

Add to the docstring, after the existing "A coarse covering cell can extend past the bbox…" sentence:

```
The covering precision is additionally clamped to the coarsest precision at which
any cell is currently stored (SPATIAL-INDEX-COARSEST-PRECISION), which is what
lets an oversized geometry be stored coarsely without becoming unfindable.
```

- [ ] **Step 7: Run the tests to verify they pass**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-index-suite))'
```

Expected: PASS, 0 failures.

- [ ] **Step 8: Run the full suite**

```bash
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
```

Expected: 0 failures. Then the same under ECL:

```bash
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

- [ ] **Step 9: Export the new symbols**

In `package.lisp`, beside the existing `#:spatial-index-precision` export, add:

```lisp
           #:spatial-index-max-cells
           #:spatial-index-coarsest-precision
```

- [ ] **Step 10: Commit**

```bash
git add spatial-index.lisp package.lisp tests/spatial-index-tests.lisp
git commit -m "fix(spatial): cap the insert-side cell cover, clamp the query to match

An insert passed the index's storage precision straight to GEOHASH-COVERING,
bypassing its max-cells guard, so a country-scale polygon enumerated ~7.7e7
cells and exhausted the heap.  The cover is now capped adaptively per index.

Capping alone would LOSE nodes: a prefix range scan [cell, cell+\"{\") reaches
only keys at or finer than the covering cell, and a coarser stored key sorts
before the range start.  So each index keeps a per-precision histogram and the
query clamps its covering precision to the lowest occupied level.  Because it
is a histogram and not a high-water mark, deleting the oversized geometry
restores selectivity on its own, with no rebuild."
```

---

### Task 2: The `(owner . slot)` registry, with union queries as a shim

Implements §4 and §4.1. The registry lands; `find-nodes-*` temporarily queries the union of all indexes, which reproduces today's behaviour exactly so no existing call site changes.

**Files:**
- Modify: `node-class.lisp` (add `%indexed-slot-owner-name`), `index.lisp:42-53` (delete its copy), `graph-class.lisp:34`, `transactions.lisp:956-1001`, `graph.lisp:13-44`, `spatial-query.lisp:41-86,137-148,174-200`, `peer-streaming.lisp:1032-1035`
- Create: `spatial-registry.lisp`, and its `graph-db.asd` component entry
- Test: `tests/spatial-scope-tests.lisp` (new), `tests/spatial-hook-tests.lisp:33` (fixture helper)

**Interfaces:**
- Consumes: Task 1's `make-spatial-index` (`:max-cells`), `spatial-index-coarsest-precision`
- Produces:
  - `(spatial-indexes graph)` → `hash-table` keyed `(owner-name . slot-name)`, values `spatial-index`
  - `(spatial-index-for graph owner-name slot-name)` → `spatial-index` or `NIL`
  - `(%spatial-index-for graph owner-name slot-name)` → `spatial-index`, creating it if absent
  - `(node-geometry node)` → `(values geometry slot-name)`, both `NIL` when none
  - `(class-spatial-index-keys class graph)` → list of `(owner-name . slot-name)`
  - `(all-spatial-indexes graph)` → list of `spatial-index`
  - `(rebuild-spatial-indexes graph)` → integer, nodes indexed
  - `%indexed-slot-owner-name` moves from `index.lisp` to `node-class.lisp`, same signature

- [ ] **Step 1: Write the failing test**

Create `tests/spatial-scope-tests.lisp`:

```lisp
;;;; Per-(owner . slot) spatial index registry and scoped queries.

(in-package #:graph-db/test)

;; Two spatially-indexed classes with NO common geometry-declaring ancestor, so
;; they land in separate indexes.  ZONE's polygon contains every PROBE point --
;; the discriminating case from the change request.
(def-vertex scope-probe ()
  ((geom :type geometry :index t))
  :graph-db-integration-test)

(def-vertex scope-zone ()
  ((extent :type geometry :index t))
  :graph-db-integration-test)

(def-suite spatial-scope-suite
  :description "Per-class spatial indexes: registry, scoping, declaration."
  :in graph-db-suite)

(in-suite spatial-scope-suite)

(defun scope-rect (min-lon min-lat max-lon max-lat)
  (%make-geometry
   :kind :polygon
   :coordinates (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(test registry-separates-declaring-classes
  "Two classes declaring their own geometry slot get two distinct indexes."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-ix (spatial-index-for g 'scope-probe 'geom))
          (zone-ix  (spatial-index-for g 'scope-zone 'extent)))
      (is (spatial-index-p probe-ix))
      (is (spatial-index-p zone-ix))
      (is (not (eq probe-ix zone-ix))))))

(test node-geometry-reports-its-slot
  "NODE-GEOMETRY returns the geometry AND the slot it came from."
  (with-test-graph (g)
    (declare (ignore g))
    (let (node)
      (with-transaction ()
        (setq node (make-scope-zone :extent (scope-rect 0d0 0d0 1d0 1d0))))
      (multiple-value-bind (geom slot) (node-geometry node)
        (is (geometryp geom))
        (is (eq slot 'extent))))))

(test unindexed-geometry-slot-creates-no-index
  "A slot that never holds a geometry never creates an index (§4.1)."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (is (null (spatial-index-for g 'scope-zone 'extent)))))
```

- [ ] **Step 2: Run it to verify it fails**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: FAIL — `The function SPATIAL-INDEX-FOR is undefined`.

- [ ] **Step 3: Move `%indexed-slot-owner-name` to `node-class.lisp`**

Delete `index.lisp:42-53` entirely. Append to `node-class.lisp` (after the `compute-effective-slot-definition :around` method):

```lisp
(defun %indexed-slot-owner-name (class slot-name)
  "The most-general node-class in CLASS's precedence list that declares SLOT-NAME as
an :INDEX direct slot -- the cross-subtype index owner (an :INDEX slot on a parent is
one shared index across its subclasses).  Lives here rather than in index.lisp so both
the general ordered index and the spatial index can reach it: index.lisp loads after
transactions.lisp, which needs this on the spatial maintenance path."
  (let ((owner (loop for c in (reverse (class-precedence-list class))
                     when (and (typep c 'node-class)
                               (find-if (lambda (ds)
                                          (and (eq (slot-definition-name ds) slot-name)
                                               (indexed-p ds)))
                                        (class-direct-slots c)))
                     return c)))
    (class-name (or owner class))))
```

- [ ] **Step 4: Replace the graph's spatial slot with a registry**

In `graph-class.lisp`, replace line 34:

```lisp
   ;; Spatial indexes: (owner-class-name . slot-name) -> SPATIAL-INDEX.  One index
   ;; per DECLARING class per geometry slot, exactly like SECONDARY-INDEXES and
   ;; VECTOR-SEGMENTS above.  Created LAZILY on first geometry-valued insert (an
   ;; :INDEX slot that never holds a geometry creates nothing), so a declared-but-
   ;; unpopulated slot costs nothing.  SYNCHRONIZED for the same reason
   ;; VECTOR-SEGMENTS is: query threads GETHASH here while the apply path may be
   ;; creating an index.
   (spatial-indexes :accessor spatial-indexes :initarg :spatial-indexes
                    :initform
                    #+ccl (make-hash-table :test 'equal :shared t)
                    #+lispworks (make-hash-table :test 'equal :single-thread nil)
                    #+ecl (make-hash-table :test 'equal)
                    #+sbcl (make-hash-table :test 'equal :synchronized t))
```

- [ ] **Step 5: Create `spatial-registry.lisp`**

```lisp
(in-package :graph-db)

;;;; The graph's per-(owner-class . slot) spatial index registry.
;;;;
;;;; The spatial index is the fourth member of the (owner . slot) index family --
;;;; alongside UNIQUE-INDEXES, VECTOR-SEGMENTS and SECONDARY-INDEXES -- and follows
;;;; the same shape: one index per DECLARING class per geometry slot, keyed by
;;;; (owner-name . slot-name), created lazily, persisted as a sidecar of roots.
;;;;
;;;; Loaded late so it can see both the MOP helpers and the graph; TRANSACTIONS.LISP
;;;; and GRAPH.LISP reach it through DECLAIM FTYPE forward declarations, exactly as
;;;; graph.lisp already does for the unique and secondary index functions.

(defun class-spatial-index-keys (class graph)
  "The (OWNER-NAME . SLOT-NAME) keys covering CLASS's geometry-index slots.  Each
:INDEX-marked slot resolves to the most general node-class declaring it, so a slot
on a mixin yields ONE key shared by every subclass.  GRAPH is accepted for symmetry
with CLASS-SECONDARY-INDEX-DESCRIPTORS and for the DEF-SPATIAL-INDEX registry added
in a later task."
  (declare (ignorable graph))
  (when (class-finalized-p class)
    (loop for slot-name in (node-geometry-index-slots class)
          collect (cons (%indexed-slot-owner-name class slot-name) slot-name))))

(defun spatial-index-for (graph owner-name slot-name)
  "GRAPH's spatial index for (OWNER-NAME . SLOT-NAME), or NIL if none has been
CREATED yet.  NB: indexes are created lazily on first geometry-valued insert, so
NIL here does NOT mean the slot is unindexed."
  (let ((reg (spatial-indexes graph)))
    (and reg (gethash (cons owner-name slot-name) reg))))

(defun %spatial-precision-for (graph owner-name slot-name)
  "The geohash precision (OWNER-NAME . SLOT-NAME)'s index is created with.  For now
the graph default; the slot option and DEF-SPATIAL-INDEX surfaces are layered on in
a later task."
  (declare (ignorable owner-name slot-name))
  (or (graph-default-spatial-precision graph) 7))

(defun %spatial-index-for (graph owner-name slot-name)
  "Get-or-create GRAPH's spatial index for (OWNER-NAME . SLOT-NAME).  This is the
ONE place an index is created, so every maintenance path and every rebuild agree on
its precision and cap."
  (let ((reg (spatial-indexes graph))
        (key (cons owner-name slot-name)))
    (or (gethash key reg)
        (setf (gethash key reg)
              (make-spatial-index (indexes graph)
                                  :precision (%spatial-precision-for
                                              graph owner-name slot-name)
                                  :backend (graph-index-backend graph))))))

(defun all-spatial-indexes (graph)
  "Every spatial index GRAPH currently holds."
  (let ((result '()))
    (when (spatial-indexes graph)
      (maphash (lambda (k idx) (declare (ignore k)) (push idx result))
               (spatial-indexes graph)))
    result))

(defun node-spatial-index (graph node slot-name)
  "The index NODE's SLOT-NAME geometry belongs in, created if absent."
  (%spatial-index-for graph
                      (%indexed-slot-owner-name (class-of node) slot-name)
                      slot-name))
```

Add to `graph-class.lisp`, in the `graph` class, next to `index-backend`:

```lisp
   ;; Default geohash precision for spatial indexes created on this graph
   ;; (MAKE-GRAPH / OPEN-GRAPH :spatial-precision).  Per-index overrides are
   ;; layered on top; see spatial-registry.lisp.
   (default-spatial-precision :accessor graph-default-spatial-precision
                              :initarg :default-spatial-precision :initform 7)
```

- [ ] **Step 6: Register the file in `graph-db.asd`**

In the `graph-db/core` component list, immediately after the `index` entry (line 101):

```lisp
               (:file "spatial-registry" :depends-on ("index"))
```

- [ ] **Step 7: Make `node-geometry` report its slot, and route maintenance**

In `transactions.lisp`, replace the `node-geometry` `((node node))` method body (`:964-971`) with:

```lisp
  (:method ((node node))
    ;; NB: do NOT gate on SLOT-BOUNDP -- node-class persistent slots are read
    ;; through SLOT-VALUE-USING-CLASS from the serialized buffer, and
    ;; SLOT-BOUNDP reports the (always-unbound) backing CLOS slot, so it would
    ;; skip every persistent slot.  Read the value and test it directly.
    ;; Returns (values GEOMETRY SLOT-NAME): the slot is what selects the node's
    ;; spatial index, and it is chosen PER NODE, so two instances of one class
    ;; can legitimately land in different indexes when different slots are bound.
    (loop for slot in (node-geometry-index-slots (class-of node))
          for v = (ignore-errors (slot-value node slot))
          when (geometryp v) return (values v slot))))
```

Update the docstring's first line to read: `"The GEOMETRY a node occupies and the slot it came from, as (values geometry slot-name), or (values nil nil)."`

Replace `transactions.lisp:973-996` (the three `apply-tx-write-to-spatial-index` methods) with:

```lisp
(declaim (ftype (function (t t t) t) %spatial-index-for))

(defun %spatial-index-node (graph node)
  "Insert NODE into the index its geometry slot selects.  No-op without geometry."
  (multiple-value-bind (geom slot) (node-geometry node)
    (when (and geom slot (not (deleted-p node)))
      (spatial-index-insert
       (%spatial-index-for graph (%indexed-slot-owner-name (class-of node) slot) slot)
       (id node) geom))))

(defun %spatial-unindex-node (graph node)
  "Remove NODE from the index its geometry slot selects.  No-op without geometry,
and no-op when that index does not exist (nothing was ever written)."
  (multiple-value-bind (geom slot) (node-geometry node)
    (when (and geom slot)
      (let ((idx (spatial-index-for
                  graph (%indexed-slot-owner-name (class-of node) slot) slot)))
        (when idx (spatial-index-remove idx (id node) geom))))))

(defmethod apply-tx-write-to-spatial-index ((write tx-create) graph)
  (%spatial-index-node graph (node write)))

(defmethod apply-tx-write-to-spatial-index ((write tx-update) graph)
  (%spatial-unindex-node graph (old-node write))
  (%spatial-index-node graph (node write)))

(defmethod apply-tx-write-to-spatial-index ((write tx-delete) graph)
  (%spatial-unindex-node graph (node write)))
```

- [ ] **Step 8: Point the remaining consumers at the registry**

`peer-streaming.lisp:1032-1035`, in `peer-purge-node`'s `vertex` branch, replace the spatial block with:

```lisp
     (%spatial-unindex-node graph node)
```

`graph.lisp`: delete `init-spatial-index` and `restore-spatial-index` (`:13-44`) and their call sites at `:316` and `:462`; the registry is populated by `rebuild-spatial-indexes`, added next. At `:316` (inside `make-graph`) and `:462` (inside `open-graph`) substitute:

```lisp
        (rebuild-spatial-indexes graph)
```

and add `rebuild-spatial-indexes` to the `declaim ftype` block at `graph.lisp:5-8`. In `close-graph`, no spatial change is needed yet (persistence lands in Task 3).

Pass the precision through: in `make-graph`'s and `open-graph`'s lambda lists the `spatial-precision` keyword already exists; set the slot instead of calling `init-spatial-index`:

```lisp
        (setf (graph-default-spatial-precision graph) (or spatial-precision 7))
```

placed immediately before the `rebuild-spatial-indexes` call in each.

- [ ] **Step 9: Rewrite the sweeps and shim the queries**

In `spatial-query.lisp`, replace `rebuild-spatial-index` (`:174-200`) with:

```lisp
(defun rebuild-spatial-indexes (graph)
  "Rebuild GRAPH's spatial indexes from scratch: drop every current index, then
re-index each live node into the (owner . slot) index its geometry slot selects.
Returns the number of nodes indexed.

Use this to adopt the per-class scheme on a graph that predates it, to change grid
precision, or to repair.  It mutates the indexes directly (outside the transaction
write path), so run it when the graph is quiescent -- analogous to REGENERATE-VIEW."
  (with-recursive-lock-held ((txn-lock graph))
    (dolist (idx (all-spatial-indexes graph))
      (delete-spatial-index idx))
    (clrhash (spatial-indexes graph))
    (let ((count 0))
      (flet ((reindex (node)
               (unless (deleted-p node)
                 (multiple-value-bind (geom slot) (node-geometry node)
                   (when (and geom slot)
                     (spatial-index-insert
                      (%spatial-index-for
                       graph (%indexed-slot-owner-name (class-of node) slot) slot)
                      (id node) geom)
                     (incf count))))))
        (map-vertices #'reindex graph)
        (map-edges #'reindex graph))
      count)))
```

The three window/radius queries share one candidate loop, so factor it out **now** rather than writing it three times and collapsing it in Task 4. Add to `spatial-registry.lisp`:

```lisp
(defun %resolve-spatial-scope (scope graph)
  "Resolve SCOPE to (values INDEXES TYPE-NAMES): the spatial indexes to scan, and
the class list results must satisfy (NIL = no filtering).

Task 2 handles only :ALL; class-name and class-list scopes arrive in Task 4."
  (ecase scope
    (:all (values (all-spatial-indexes graph) nil))))

(defun %scope-admits-p (node type-names)
  "True when NODE satisfies the scope's type filter (always, for :ALL)."
  (or (null type-names)
      (some (lambda (n) (typep node n)) type-names)))
```

Then in `spatial-query.lisp`, define the shared driver once and express all three queries through it:

```lisp
(defmacro %do-scoped-candidates ((node-var scope graph &key bbox radius) &body body)
  "Run BODY with NODE-VAR bound to each live, scope-admitted node whose id came
back from every index in SCOPE.  Dedups by node id across indexes, so a node
reachable through two of its own slot-indexes is visited once."
  (let ((indexes (gensym "IX")) (types (gensym "TY")) (seen (gensym "SEEN"))
        (idx (gensym "I")) (id (gensym "ID")))
    `(multiple-value-bind (,indexes ,types) (%resolve-spatial-scope ,scope ,graph)
       (let ((,seen (make-hash-table :test 'equalp)))
         (dolist (,idx ,indexes)
           (dolist (,id ,(if bbox
                             `(destructuring-bind (mnl mnt mxl mxt) ,bbox
                                (spatial-index-query-bbox ,idx mnl mnt mxl mxt))
                             `(destructuring-bind (lat lon r) ,radius
                                (spatial-index-query-radius ,idx lat lon r))))
             (unless (gethash ,id ,seen)
               (setf (gethash ,id ,seen) t)
               (let ((,node-var (%node-by-id ,id ,graph)))
                 (when (and ,node-var (not (deleted-p ,node-var))
                            (%scope-admits-p ,node-var ,types))
                   ,@body)))))))))

(defun find-nodes-within (area &key (graph *graph*))
  "List of live nodes whose geometry lies within AREA (a :POLYGON or
:MULTIPOLYGON geometry).  A :POINT node is judged exactly; an extended-geometry
node is judged exactly when graph-db/geos is loaded, otherwise by its
representative point (bbox centre).

TRANSITIONAL: scoped to :ALL, which reproduces the single-index behaviour this
replaced.  A required scope argument lands in Task 4, and changes only this
lambda list and %RESOLVE-SPATIAL-SCOPE -- not the loop below."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node :all graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (%node-within-area-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-intersecting (area &key (graph *graph*))
  "List of live nodes whose geometry INTERSECTS AREA (any geometry kind).  Exact
with the graph-db/geos add-on; without it, extended-geometry candidates use a
COARSE bounding-box overlap test (point candidates are always exact)."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node :all graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (geometry-intersects-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-near (lat lon radius &key (graph *graph*))
  "List of (NODE . DISTANCE-METRES) for live nodes within RADIUS of (LAT, LON),
nearest first."
  (let ((result '()))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (%do-scoped-candidates (node :all graph :radius (list lat lon radius))
        (let ((geom (node-geometry node)))
          (when geom
            (multiple-value-bind (nlat nlon) (%geometry-rep-point geom)
              (let ((d (geodesic-distance lat lon nlat nlon)))
                (when (<= d radius)
                  (push (cons node d) result))))))))
    (sort result #'< :key #'cdr)))
```

In `find-nearest-k`, replace the seed-radius computation with the finest precision in scope — seeding off a coarse index would make the very first query an enormous sweep:

```lisp
  (let ((indexes (%resolve-spatial-scope :all graph)))
    (when (and indexes (numberp lat) (numberp lon) (integerp k) (plusp k))
      (let* ((prec (reduce #'max indexes :key #'spatial-index-precision))
             (r (max 1d0 (* (nth-value 1 (geohash-cell-size prec)) 111320d0)))
             (found '()))
        (loop
          (setf found (find-nodes-near lat lon r :graph graph))
          (when (or (>= (length found) k) (>= r max-radius))
            (return))
          (setf r (min max-radius (* r 2d0))))
        (subseq found 0 (min k (length found))))))
```

- [ ] **Step 10: Fix the one test helper that reaches into the index directly**

`tests/spatial-hook-tests.lisp:33`:

```lisp
(defun in-box-p (g id box)
  (member id (loop for idx in (all-spatial-indexes g)
                   append (apply #'spatial-index-query-bbox idx box))
          :test 'equalp))
```

- [ ] **Step 11: Register the new test file and export the new symbols**

`graph-db.asd`, in the `graph-db/test` components after `spatial-intersect-tests`:

```lisp
               (:file "spatial-scope-tests")
```

`package.lisp`: remove `#:spatial-index` and `#:rebuild-spatial-index`; add:

```lisp
           #:spatial-indexes
           #:spatial-index-for
           #:rebuild-spatial-indexes
```

`tests/package.lisp`: add `#:spatial-index-for`, `#:spatial-indexes`, `#:all-spatial-indexes`, `#:node-geometry` to the `graph-db` import list if not already present.

- [ ] **Step 12: Run the new tests, then the full suite**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: PASS, 3 tests.

```bash
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
```

Expected: 0 failures — every existing spatial test passes unchanged, because the union shim reproduces the old behaviour.

- [ ] **Step 13: Commit**

```bash
git add node-class.lisp index.lisp graph-class.lisp graph.lisp transactions.lisp \
        spatial-registry.lisp spatial-query.lisp peer-streaming.lisp \
        package.lisp graph-db.asd tests/spatial-scope-tests.lisp \
        tests/spatial-hook-tests.lisp tests/package.lisp
git commit -m "feat(spatial): one index per declaring class and geometry slot

The graph's single spatial-index slot becomes a (owner-class . slot-name)
registry, the fourth member of the index family already established by
unique-indexes, vector-segments and secondary-indexes -- same shape, same
lazy creation, same ownership resolution.

NODE-GEOMETRY now returns (values geometry slot-name); the slot is what
selects a node's index, and it is chosen per NODE, so two instances of one
class can land in different indexes when different slots are bound.  That
preserves today's semantics exactly.

%INDEXED-SLOT-OWNER-NAME moves from index.lisp to node-class.lisp so the
spatial maintenance path in transactions.lisp can reach it without a third
copy of the same precedence-list walk.

Queries transitionally union every index, which reproduces the behaviour this
replaced, so no existing call site changes yet.  Required scoping lands next."
```

---

### Task 3: Sidecar v3, migration, and the regenerate entry points

Implements §4's sidecar, §7.4's manual recovery, and §9.

**Files:**
- Modify: `graph.lisp` (save/restore/close), `spatial-query.lisp` (regenerate)
- Test: `tests/spatial-scope-tests.lisp`

**Interfaces:**
- Consumes: Task 2's `all-spatial-indexes`, `%spatial-index-for`, `rebuild-spatial-indexes`
- Produces:
  - `(spatial-indexes-root-file location)` → pathname string
  - `(save-spatial-index-roots graph)` → `NIL`
  - `(restore-spatial-index-roots graph)` → `T` if a v3 sidecar was read, else `NIL`
  - `(regenerate-spatial-index graph owner-name slot-name)` → integer, nodes indexed
  - `(regenerate-spatial-indexes graph)` → `graph`
  - `+spatial-index-format+` → `3`

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(test roots-survive-a-clean-reopen
  "A clean close persists every index root; reopen finds the same nodes with no
node scan."
  (let ((dir (make-temp-directory)))
    (unwind-protect
         (let (zone-id)
           (let ((g (make-graph :spatial-reopen dir)))
             (unwind-protect
                  (let ((*graph* g))
                    (with-transaction ()
                      (setq zone-id (id (make-scope-zone
                                         :extent (scope-rect 0d0 0d0 1d0 1d0))))))
               (close-graph g)))
           (let ((g (open-graph :spatial-reopen dir)))
             (unwind-protect
                  (let ((idx (spatial-index-for g 'scope-zone 'extent)))
                    (is (spatial-index-p idx))
                    (is (has-p zone-id
                               (spatial-index-query-bbox idx 0.1d0 0.1d0 0.2d0 0.2d0))))
               (close-graph g))))
      (cl-fad:delete-directory-and-files dir :if-does-not-exist :ignore))))

(test regenerate-one-index-leaves-the-others
  "REGENERATE-SPATIAL-INDEX rebuilds exactly one (owner . slot) index."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((probe-before (spatial-index-for g 'scope-probe 'geom)))
      (regenerate-spatial-index g 'scope-zone 'extent)
      ;; The untouched index is the SAME struct; the regenerated one is fresh.
      (is (eq probe-before (spatial-index-for g 'scope-probe 'geom)))
      (is (spatial-index-p (spatial-index-for g 'scope-zone 'extent))))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: FAIL — `The function REGENERATE-SPATIAL-INDEX is undefined`.

- [ ] **Step 3: Bump the format constant**

`spatial-index.lisp`, replace the `+spatial-index-format+` definition and its comment:

```lisp
;; On-disk sidecar format version.  v1 (unversioned) keyed the skip list by a bare
;; geohash string with the node-id as the value (DUPLICATE keys -> O(n) remove); v2
;; keys by the composite (cell . node-id), duplicate-free, one index per GRAPH; v3
;; is one index per (declaring-class . geometry-slot), each with its own precision,
;; insert cap and precision histogram.  A v1/v2 sidecar triggers an index-only
;; re-derivation from live node geometries at open (RESTORE-SPATIAL-INDEX-ROOTS).
(alexandria:define-constant +spatial-index-format+ 3 :test '=)
```

- [ ] **Step 4: Write the sidecar**

In `graph.lisp`, in place of the deleted `init-spatial-index`/`restore-spatial-index`:

```lisp
(defun spatial-indexes-root-file (location)
  (format nil "~A/spatial-indexes.dat" location))

(defun spatial-index-root-file (location)
  "The PRE-v3 single-index sidecar.  Its presence with no spatial-indexes.dat is
the migration signal; it is never written any more, and is left in place rather
than renamed -- the old RESTORE-SPATIAL-INDEX treats a missing file as an EMPTY
index rather than a rebuild, so renaming would make a downgrade fail silently.
Downgrade after migration is unsupported either way."
  (format nil "~A/spatial-index.root" location))

(defun save-spatial-index-roots (graph)
  "Persist every spatial index's root, precision, backend, insert cap and precision
histogram.  Called at CLOSE-GRAPH, at index creation, and whenever an index's
COARSEST-PRECISION decreases (§7.2: losing that decrease would reopen with a
too-fine clamp and silently miss; losing an increase merely over-covers)."
  (when (and (indexes graph) (spatial-indexes graph))
    (let ((roots '()))
      (maphash (lambda (key idx)
                 (push (list (car key) (cdr key)
                             (spatial-index-address idx)
                             (spatial-index-precision idx)
                             (spatial-index-backend idx)
                             (spatial-index-max-cells idx)
                             (copy-seq (spatial-index-precision-counts idx)))
                       roots))
               (spatial-indexes graph))
      (cl-store:store (list :format +spatial-index-format+ :indexes roots)
                      (spatial-indexes-root-file (location graph))))))

(defun restore-spatial-index-roots (graph)
  "Reopen the spatial indexes from the v3 sidecar -- no node scan.  Returns T when
one was present and current, NIL to fall back to REBUILD-SPATIAL-INDEXES (a fresh
graph, a pre-v3 graph, or a crash before any root was written)."
  (let ((file (spatial-indexes-root-file (location graph))))
    (when (probe-file file)
      (destructuring-bind (&key format indexes &allow-other-keys)
          (cl-store:restore file)
        (when (eql format +spatial-index-format+)
          (dolist (r indexes)
            (destructuring-bind (owner slot address precision backend max-cells
                                 &optional counts)
                r
              (setf (gethash (cons owner slot) (spatial-indexes graph))
                    (open-spatial-index (indexes graph) address
                                        :precision precision :backend backend
                                        :max-cells max-cells
                                        :precision-counts counts))))
          t)))))
```

- [ ] **Step 5: Wire save/restore into open and close**

In `graph.lisp`'s `open-graph`, replace the Task 2 `(rebuild-spatial-indexes graph)` call with:

```lisp
        (unless (restore-spatial-index-roots graph)
          ;; No current sidecar: a fresh graph, or a pre-v3 one whose single
          ;; index must be re-derived per (owner . slot).  Index only -- node
          ;; data is untouched and nothing is re-fetched.
          (when (probe-file (spatial-index-root-file (location graph)))
            (log:info "Spatial index sidecar is pre-v3; re-deriving per-class ~
                       indexes from live node geometries (index only)."))
          (rebuild-spatial-indexes graph)
          (save-spatial-index-roots graph))
```

In `make-graph`, keep the Task 2 call and follow it with `(save-spatial-index-roots graph)`.

In `close-graph`, beside the existing `save-unique-index-roots` / `save-secondary-index-roots` calls, add:

```lisp
    (save-spatial-index-roots graph)
```

Add `save-spatial-index-roots` and `restore-spatial-index-roots` to the `declaim ftype` block at `graph.lisp:5-8`.

In `spatial-registry.lisp`'s `%spatial-index-for`, persist on creation — replace the `setf gethash` form's body so the new index is saved:

```lisp
        (let ((idx (make-spatial-index (indexes graph)
                                       :precision (%spatial-precision-for
                                                   graph owner-name slot-name)
                                       :backend (graph-index-backend graph))))
          (setf (gethash key reg) idx)
          (save-spatial-index-roots graph)
          idx))
```

- [ ] **Step 6: Add the regenerate entry points**

Append to `spatial-query.lisp`:

```lisp
(defun regenerate-spatial-index (graph owner-name slot-name)
  "Drop and rebuild ONE spatial index, re-deriving its precision histogram from
live nodes.  This is the manual recovery for an index whose selectivity was
degraded by an oversized insert (§7.2) -- reach for this rather than
REGENERATE-SPATIAL-INDEXES, which rebuilds every index in the graph.  Returns the
number of nodes indexed."
  (with-recursive-lock-held ((txn-lock graph))
    (let ((key (cons owner-name slot-name)))
      (let ((old (gethash key (spatial-indexes graph))))
        (when old (delete-spatial-index old)))
      (remhash key (spatial-indexes graph))
      (let ((count 0))
        (flet ((reindex (node)
                 (unless (deleted-p node)
                   (multiple-value-bind (geom slot) (node-geometry node)
                     (when (and geom (eq slot slot-name)
                                (eq (%indexed-slot-owner-name (class-of node) slot)
                                    owner-name))
                       (spatial-index-insert
                        (%spatial-index-for graph owner-name slot-name)
                        (id node) geom)
                       (incf count))))))
          (if (subtypep owner-name 'edge)
              (map-edges #'reindex graph :edge-type owner-name)
              (map-vertices #'reindex graph :vertex-type owner-name)))
        (save-spatial-index-roots graph)
        count))))

(defun regenerate-spatial-indexes (graph)
  "Drop every spatial index and rebuild it on GRAPH's CURRENT :INDEX-BACKEND,
persisting the new roots.  The parallel of REGENERATE-ALL-VIEWS /
REGENERATE-SECONDARY-INDEXES for an in-place backend switch."
  (rebuild-spatial-indexes graph)
  (save-spatial-index-roots graph)
  graph)
```

- [ ] **Step 7: Export**

`package.lisp`:

```lisp
           #:regenerate-spatial-index
           #:regenerate-spatial-indexes
```

- [ ] **Step 8: Run the tests, then the full suite on both implementations**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

Expected: PASS, 0 failures on both.

- [ ] **Step 9: Commit**

```bash
git add spatial-index.lisp graph.lisp spatial-registry.lisp spatial-query.lisp \
        package.lisp tests/spatial-scope-tests.lisp
git commit -m "feat(spatial): v3 sidecar, automatic index-only migration, per-index regenerate

Each index persists its root, precision, backend, insert cap and precision
histogram to spatial-indexes.dat, mirroring secondary-indexes.dat.  Written at
creation as well as at close, because a spatial root address is stable, so a
crash need not cost a rebuild.

A pre-v3 graph -- spatial-index.root present, spatial-indexes.dat absent --
re-derives its indexes from live node geometries at open.  Index only: node
data is untouched and nothing is re-fetched.  The old sidecar is left in place
rather than renamed; the pre-v3 reader treats a missing file as an EMPTY index
rather than a rebuild, so renaming would make a downgrade fail silently.
Downgrade after migration is unsupported regardless.

REGENERATE-SPATIAL-INDEX (singular) rebuilds one (owner . slot) index -- the
manual recovery for a degraded clamp, without rebuilding every index."
```

---

### Task 4: Required scope on the query API

Implements §6. This is the breaking change; it removes the Task 2 union shim.

**Files:**
- Modify: `spatial-query.lisp:41-159`, `package.lisp`, `example.lisp`, `docs/vivace-graph-v3-doc.org`
- Modify (call sites): `tests/spatial-query-tests.lisp`, `tests/spatial-intersect-tests.lisp`, `tests/geos/query-tests.lisp`, `tests/geos/perf-bench.lisp`, `tests/geos/storm-tests.lisp`, `tests/replication/slave.lisp`, `tests/backup-tests.lisp`, `tests/concurrency/spatial-tests.lisp`, `tests/spatial-prolog-tests.lisp`
- Test: `tests/spatial-scope-tests.lisp`

**Interfaces:**
- Consumes: Task 2's `class-spatial-index-keys`, `spatial-index-for`, `all-spatial-indexes`
- Produces:
  - `(%resolve-spatial-scope scope graph)` → `(values list-of-spatial-index type-filter-or-nil)`; signals when `scope` names a class declaring no geometry-index slot
  - `(find-nodes-within scope area &key graph)` → list of node
  - `(find-nodes-intersecting scope area &key graph)` → list of node
  - `(find-nodes-near scope lat lon radius &key graph)` → list of `(node . distance)`
  - `(find-nearest-k scope lat lon k &key graph max-radius)` → list of `(node . distance)`
  - Prolog: `find-within/3`, `find-intersects/3`, `find-near/5`, `find-nearest/5`

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(test scope-excludes-the-other-class-both-directions
  "A query scoped to A returns no B nodes even though B's polygon contains
every A point, and scoping to B returns no A nodes."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (let ((probes (find-nodes-within 'scope-probe window :graph g))
            (zones  (find-nodes-within 'scope-zone window :graph g)))
        (is (= 1 (length probes)))
        (is (every #'scope-probe-p probes))
        (is (= 1 (length zones)))
        (is (every #'scope-zone-p zones))))))

(test scope-accepts-a-class-list-and-dedups
  "A list scope unions the named classes; :ALL unions everything."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
      (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (is (= 2 (length (find-nodes-within '(scope-probe scope-zone) window :graph g))))
      (is (= 2 (length (find-nodes-within :all window :graph g)))))))

;; A vertex with no geometry of any kind -- neither an :INDEX-marked geometry
;; slot nor a NODE-GEOMETRY method.  This, not GEO-PLACE, is what "not a spatial
;; class" means: GEO-PLACE overrides NODE-GEOMETRY and IS scopeable.
(def-vertex scope-aspatial ()
  ((label :type string))
  :graph-db-integration-test)

(test unscoped-class-signals-declared-empty-returns-nil
  "A class with no geometry at all signals; a declared-but-empty one is NIL."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-probe :geom (make-point 37.1724d0 49.2020d0)))
    (let ((window (scope-rect 22.0d0 44.0d0 41.0d0 53.0d0)))
      (signals error (find-nodes-within 'scope-aspatial window :graph g))
      ;; SCOPE-ZONE is declared but nothing was written: empty, not an error.
      (is (null (find-nodes-within 'scope-zone window :graph g))))))

(test custom-node-geometry-classes-are-scopeable
  "Overriding NODE-GEOMETRY is a documented extension point, so such a class is
scopeable by name -- not reachable only through :ALL.  GEO-PLACE (defined in
spatial-hook-tests.lisp) has a hand-written method and no :INDEX-marked slot."
  (with-test-graph (g)
    (with-transaction ()
      (make-geo-place :loc (make-point 37.1724d0 49.2020d0))
      (make-scope-probe :geom (make-point 37.1730d0 49.2025d0)))
    (let ((window (scope-rect 37.0d0 49.0d0 37.5d0 49.5d0)))
      (let ((places (find-nodes-within 'geo-place window :graph g)))
        (is (= 1 (length places)))
        (is (every #'geo-place-p places)))
      ;; ...and scoping to the slot-declared class still excludes it.
      (is (every #'scope-probe-p (find-nodes-within 'scope-probe window :graph g))))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: FAIL — `invalid number of arguments` from `find-nodes-within`.

- [ ] **Step 3: Widen scope resolution**

`%do-scoped-candidates` and `%scope-admits-p` already exist from Task 2 and do **not** change. Add `%class-geometry-slots-declared-p` to `spatial-registry.lisp`, and **replace** Task 2's `:ALL`-only `%resolve-spatial-scope` stub with the full version:

```lisp
(defun %node-geometry-method-owner-name (class)
  "The most general class carrying an applicable application-supplied
NODE-GEOMETRY method, or NIL when the class relies on the default method.

Overriding NODE-GEOMETRY is a documented extension point (see example.lisp): the
method returns a computed geometry and NO slot name, so such a class is indexed
under the key (OWNER . NIL).  This resolves OWNER the same way
%INDEXED-SLOT-OWNER-NAME resolves a slot's -- most general first -- so a method
defined on a parent gives its subclasses ONE shared index, exactly as an :INDEX
slot on a parent does.  Keying on each node's own class instead would scatter a
hierarchy across per-subclass indexes and make a scope on the parent miss them.

Use the repo's existing MOP idiom for GENERIC-FUNCTION-METHODS /
METHOD-SPECIALIZERS and verify it on SBCL, ECL and CCL.  The two built-in methods
specialize on T and on NODE; only something more specific counts as custom."
  (let ((owner nil))
    (dolist (m (generic-function-methods #'node-geometry) owner)
      (let ((spec (first (method-specializers m))))
        (when (and (typep spec 'class)
                   (not (member (class-name spec) '(t node)))
                   (subtypep (class-name class) (class-name spec))
                   (or (null owner) (subtypep owner (class-name spec))))
          (setf owner (class-name spec)))))))

(defun %class-geometry-slots-declared-p (class-name)
  "True when CLASS-NAME is a scopeable spatial class: it declares at least one
:INDEX-marked slot, OR it carries an application-supplied NODE-GEOMETRY method.

Both are first-class ways to be spatially indexed, so both must be scopeable --
otherwise overriding NODE-GEOMETRY would leave a class reachable only through
:ALL, which is the unscoped query this task exists to forbid.  Distinguishes a
declared-but-empty index (a legitimate empty result) from a class that is not
spatially indexed at all (an error)."
  (let ((class (ignore-errors (find-class class-name nil))))
    (and class (class-finalized-p class)
         (or (node-geometry-index-slots class)
             (%node-geometry-method-owner-name class))
         t)))

(defun %resolve-spatial-scope (scope graph)
  "Resolve SCOPE -- a class name, a list of class names, or :ALL -- to
 (values INDEXES TYPE-NAMES).

INDEXES is the set of live spatial indexes to scan; TYPE-NAMES is the class list
results must satisfy, or NIL for :ALL (no filtering).  A named class contributes
every (owner . slot) index covering its geometry slots, so a slot declared on a
mixin resolves to the ancestor's shared index -- and the type filter is what then
keeps a sibling subclass's nodes out of the answer.

Signals when a named class declares no geometry-index slot at all: that is a
programming error, and catching it is the reason the scope is required."
  (if (eq scope :all)
      (values (all-spatial-indexes graph) nil)
      (let* ((names (if (listp scope) scope (list scope)))
             (keys (make-hash-table :test 'equal))
             (indexes '()))
        (dolist (name names)
          (unless (%class-geometry-slots-declared-p name)
            (error "~S is not a spatially indexed class in ~S: it declares no ~
                    :INDEX-marked geometry slot."
                   name (graph-name graph)))
          (dolist (key (class-spatial-index-keys (find-class name) graph))
            (unless (gethash key keys)
              (setf (gethash key keys) t)
              (let ((idx (spatial-index-for graph (car key) (cdr key))))
                (when idx (push idx indexes))))))
        (values indexes names))))
```

- [ ] **Step 3b: Route the NIL-slot (custom `node-geometry`) case through the method owner**

Task 2 keys a custom-`node-geometry` node as `(%indexed-slot-owner-name (class-of node) NIL . NIL)`, which resolves to the node's **own** class — so a hierarchy scatters across per-subclass indexes and a scope on the parent misses its subclasses' nodes. Now that such classes are scopeable, the owner must mirror the slot rule. In `transactions.lisp`, in both `%spatial-index-node` and `%spatial-unindex-node`, resolve the owner as:

```lisp
(if slot
    (%indexed-slot-owner-name (class-of node) slot)
    (%node-geometry-method-owner-name (class-of node)))
```

and in `spatial-registry.lisp`, make `class-spatial-index-keys` return the `(owner . NIL)` key for a class with a custom method:

```lisp
(defun class-spatial-index-keys (class graph)
  "The (OWNER-NAME . SLOT-NAME) keys covering CLASS's geometry.  Each :INDEX-marked
slot resolves to the most general node-class declaring it, so a slot on a mixin
yields ONE key shared by every subclass.  A class with an application-supplied
NODE-GEOMETRY method instead yields (METHOD-OWNER . NIL), resolved by the same
most-general rule."
  (declare (ignorable graph))
  (when (class-finalized-p class)
    (let ((slot-keys (loop for slot-name in (node-geometry-index-slots class)
                           collect (cons (%indexed-slot-owner-name class slot-name)
                                         slot-name)))
          (method-owner (%node-geometry-method-owner-name class)))
      (if method-owner
          (cons (cons method-owner nil) slot-keys)
          slot-keys))))
```

There is no persistence before Task 3 and every open rebuilds, so re-keying costs nothing here.

- [ ] **Step 4: Add the scope parameter to the entry points**

`%do-scoped-candidates` is unchanged from Task 2 — the only edits are the lambda lists and the `:all` literal moving to the caller's `scope` variable. In `spatial-query.lisp`, replace the four entry points with:

```lisp
(defun find-nodes-within (scope area &key (graph *graph*))
  "Live nodes in SCOPE whose geometry lies within AREA (a :POLYGON or
:MULTIPOLYGON).  SCOPE is a node-class name, a list of them, or :ALL; it selects
which spatial indexes are scanned AND filters the results by type.  Signals when
SCOPE names a class that is not spatially indexed.

A :POINT node is judged exactly; an extended-geometry node is judged exactly when
graph-db/geos is loaded, otherwise by its representative point (bbox centre)."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node scope graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (%node-within-area-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-intersecting (scope area &key (graph *graph*))
  "Live nodes in SCOPE whose geometry INTERSECTS AREA (any geometry kind).  Exact
with the graph-db/geos add-on; without it, extended-geometry candidates use a
COARSE bounding-box overlap test (point candidates are always exact).  SCOPE is as
for FIND-NODES-WITHIN."
  (let ((result '()))
    (when (geometryp area)
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (%do-scoped-candidates (node scope graph
                                :bbox (list min-lon min-lat max-lon max-lat))
          (let ((geom (node-geometry node)))
            (when (and geom (geometry-intersects-p area geom))
              (push node result))))))
    (nreverse result)))

(defun find-nodes-near (scope lat lon radius &key (graph *graph*))
  "(NODE . DISTANCE-METRES) for live nodes in SCOPE within RADIUS of (LAT, LON),
nearest first.  SCOPE is as for FIND-NODES-WITHIN."
  (let ((result '()))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (%do-scoped-candidates (node scope graph :radius (list lat lon radius))
        (let ((geom (node-geometry node)))
          (when geom
            (multiple-value-bind (nlat nlon) (%geometry-rep-point geom)
              (let ((d (geodesic-distance lat lon nlat nlon)))
                (when (<= d radius)
                  (push (cons node d) result))))))))
    (sort result #'< :key #'cdr)))

(defun find-nearest-k (scope lat lon k &key (graph *graph*) (max-radius 2.5d4))
  "The K nodes in SCOPE nearest (LAT, LON) as (NODE . DISTANCE-METRES), nearest
first (fewer than K if SCOPE holds fewer within MAX-RADIUS).

Correctness: FIND-NODES-NEAR returns every node within a radius sorted by distance,
so once a radius encloses at least K nodes, those K are the global K nearest.  The
seed radius comes from the FINEST precision in scope -- seeding off a coarse index
would make the very first query an enormous sweep -- and doubles until K are
enclosed or MAX-RADIUS is reached."
  (multiple-value-bind (indexes types) (%resolve-spatial-scope scope graph)
    (declare (ignore types))
    (when (and indexes (numberp lat) (numberp lon) (integerp k) (plusp k))
      (let* ((prec (reduce #'max indexes :key #'spatial-index-precision))
             (r (max 1d0 (* (nth-value 1 (geohash-cell-size prec)) 111320d0)))
             (found '()))
        (loop
          (setf found (find-nodes-near scope lat lon r :graph graph))
          (when (or (>= (length found) k) (>= r max-radius))
            (return))
          (setf r (min max-radius (* r 2d0))))
        (subseq found 0 (min k (length found)))))))
```

- [ ] **Step 5: Replace the Prolog functors**

In `spatial-query.lisp`, delete `find-within/2`, `find-intersects/2`, `find-near/4` and `find-nearest/4` and add:

```lisp
(def-global-prolog-functor find-within/3 (?node ?scope ?area cont)
  "Yield each node in ?SCOPE whose geometry lies within the bound :POLYGON or
:MULTIPOLYGON ?AREA.  ?SCOPE is a node-class name or :ALL."
  (let ((node-var (var-deref ?node))
        (scope (var-deref ?scope))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-within scope area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-intersects/3 (?node ?scope ?area cont)
  "Yield each node in ?SCOPE whose geometry intersects the bound ?AREA geometry."
  (let ((node-var (var-deref ?node))
        (scope (var-deref ?scope))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-intersecting scope area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-near/5 (?node ?scope ?lat ?lon ?radius cont)
  "Yield each node in ?SCOPE within ?RADIUS metres of (?LAT, ?LON)."
  (let ((node-var (var-deref ?node))
        (scope (var-deref ?scope))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (radius (var-deref ?radius)))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (dolist (nd (find-nodes-near scope lat lon radius :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-nearest/5 (?node ?scope ?lat ?lon ?k cont)
  "Yield each of the ?K nodes in ?SCOPE nearest (?LAT, ?LON), nearest first."
  (let ((node-var (var-deref ?node))
        (scope (var-deref ?scope))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (k (var-deref ?k)))
    (when (and (numberp lat) (numberp lon) (integerp k))
      (dolist (nd (find-nearest-k scope lat lon k :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))
```

- [ ] **Step 6: Add the Prolog scope-shape test**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(test prolog-scope-shapes
  "The Prolog functors accept a symbol scope and :ALL.  A literal list scope is
pinned here: if the query compiler mangles it, restrict the documented Prolog
scope to symbol-or-:ALL and route multi-class queries through a disjunction."
  (with-test-graph (g)
    (let ((*graph* g))
      (with-transaction ()
        (make-scope-probe :geom (make-point 37.1724d0 49.2020d0))
        (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
      (is (= 1 (length (select-flat (?n) (find-near ?n scope-probe
                                                    49.2020d0 37.1724d0 500.0d0)))))
      (is (<= 1 (length (select-flat (?n) (find-near ?n :all
                                                     49.2020d0 37.1724d0 500.0d0))))))))
```

- [ ] **Step 7: Update every call site**

Mechanical: add the scope as the first argument. Each call site's correct scope is the node type it already filters for after retrieval. Where a test genuinely wants everything, use `:all`.

```bash
grep -rn "find-nodes-within\|find-nodes-near\|find-nodes-intersecting\|find-nearest-k\|(find-within \|(find-near \|(find-intersects \|(find-nearest " \
  example.lisp tests/ docs/vivace-graph-v3-doc.org
```

Work the list to zero. In `tests/package.lisp` and `tests/concurrency/package.lisp` the imported symbol names are unchanged, so no edit is needed there.

- [ ] **Step 8: Run the suites**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
sbcl --non-interactive --eval '(asdf:test-system :graph-db/concurrency-test)'
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

Expected: 0 failures on all four.

- [ ] **Step 9: Commit**

```bash
git add spatial-query.lisp spatial-registry.lisp package.lisp example.lisp \
        tests/ docs/vivace-graph-v3-doc.org
git commit -m "feat(spatial)!: spatial queries take a required scope

BREAKING.  FIND-NODES-WITHIN / -INTERSECTING / -NEAR and FIND-NEAREST-K take a
scope as their first argument: a node-class name, a list of them, or :ALL.  A
required POSITIONAL argument makes every stale call site a compile-time warning
on SBCL and ECL rather than a runtime error on a query path.

A scope selects which indexes are scanned AND filters results by type.  Both
halves are needed: a geometry slot declared on a mixin gives its subclasses ONE
shared index, so storage separation alone does not discriminate between
siblings.  Naming a class that is not spatially indexed signals; a declared but
empty index returns NIL, which is a legitimate empty result and not a fault.

Prolog gains find-within/3, find-intersects/3, find-near/5 and find-nearest/5.
The old /2 and /4 arities are removed rather than left to signal, so a stale
query fails at goal entry with unknown-functor rather than binding against the wrong arity.
(Not compile time: prologc.lisp emits the functor lookup as runtime code in the clause body.)"
```

---

### Task 5: Declaration surfaces and precision resolution

Implements §5.

**Files:**
- Modify: `node-class.lisp` (slot option + inheritance), `spatial-registry.lisp` (registry, resolution, install), `graph.lisp` (install at open), `package.lisp`
- Test: `tests/spatial-scope-tests.lisp`

**Interfaces:**
- Consumes: Task 2's `%spatial-index-for`, Task 3's `save-spatial-index-roots`
- Produces:
  - `(spatial-precision-spec slot-def)` → `(or null (integer 1 12))`
  - `(def-spatial-index owner-class slot graph-name &key precision)` → `spatial-index-spec`
  - `(install-spatial-indexes graph)` → `NIL`
  - `%spatial-precision-for` gains slot-option and macro resolution

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(def-vertex scope-coarse ()
  ((extent :type geometry :index t :spatial-precision 3))
  :graph-db-integration-test)

(test slot-option-sets-index-precision
  "A :SPATIAL-PRECISION slot option is the index's grid precision."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-coarse :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
    (let ((idx (spatial-index-for g 'scope-coarse 'extent)))
      (is (= 3 (spatial-index-precision idx)))
      ;; At p=3 a country-scale polygon is ~98 cells, so the cap never fires and
      ;; the clamp stays at the index's own precision.
      (is (= 3 (spatial-index-coarsest-precision idx))))))

(test slot-option-beats-def-spatial-index
  "MOP-first, matching CLASS-SECONDARY-INDEX-DESCRIPTORS: the slot option wins
and the losing declaration warns rather than silently doing nothing.  Asserted
through the public surface -- declare, write a node, inspect the index that was
actually built -- not by calling the conflict predicate directly.

*SCHEMA-SPATIAL-METADATA* is rebound so the DEF-SPATIAL-INDEX registered here
does not leak into later tests and make them order-dependent."
  (let ((graph-db::*schema-spatial-metadata* (make-hash-table)))
    (with-test-graph (g)
      (signals warning
        (def-spatial-index scope-coarse extent :graph-db-integration-test
          :precision 5))
      (handler-bind ((warning #'muffle-warning))
        (with-transaction ()
          (make-scope-coarse :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0))))
      ;; The slot option's 3 is what the index was actually built with, not 5.
      (is (= 3 (spatial-index-precision
                (spatial-index-for g 'scope-coarse 'extent)))))))

(test def-spatial-index-sets-precision-for-an-unannotated-slot
  "A slot with no :SPATIAL-PRECISION takes the macro's value, with no warning --
the two surfaces are complementary when only one of them declares."
  (let ((graph-db::*schema-spatial-metadata* (make-hash-table)))
    (with-test-graph (g)
      (def-spatial-index scope-zone extent :graph-db-integration-test :precision 4)
      (with-transaction ()
        (make-scope-zone :extent (scope-rect 22.1d0 44.4d0 40.2d0 52.4d0)))
      (is (= 4 (spatial-index-precision
                (spatial-index-for g 'scope-zone 'extent)))))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: FAIL — `Invalid initialization argument :SPATIAL-PRECISION`.

- [ ] **Step 3: Add the slot option**

In `node-class.lisp`'s `node-slot-definition`, after the `vector-index` slot:

```lisp
   ;; Geohash grid precision for this geometry slot's spatial index, or NIL for
   ;; the graph default.  A type-as-hint option: it means nothing on a slot that
   ;; never holds a geometry.  See spatial-registry.lisp for resolution against
   ;; DEF-SPATIAL-INDEX.
   (spatial-precision :accessor spatial-precision-spec :initarg :spatial-precision
                      :initform nil :allocation :instance)
```

After the other default methods:

```lisp
(defmethod spatial-precision-spec (slot-def)
  nil)
```

In `compute-effective-slot-definition :around`, after the `:VECTOR-INDEX` block:

```lisp
    ;; Inherit :SPATIAL-PRECISION from the declaring direct slot, so a geometry
    ;; slot on a parent carries one grid precision across its subclasses -- the
    ;; same shared-index semantics as :INDEX / :UNIQUE / :VECTOR-INDEX above.
    (let ((sp (find-if #'spatial-precision-spec direct-slots)))
      (when (or (spatial-precision-spec slot) sp)
        (setf (slot-value slot 'spatial-precision)
              (or (spatial-precision-spec slot)
                  (and sp (spatial-precision-spec sp))))))
```

- [ ] **Step 4: Add the declaration registry and resolution**

Append to `spatial-registry.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; DEF-SPATIAL-INDEX: the out-of-band declaration surface (mirrors DEF-INDEX)
;;; ---------------------------------------------------------------------------

(defvar *schema-spatial-metadata* (make-hash-table)
  "graph-name (symbol) -> list of SPATIAL-INDEX-SPECs (newest first).")

(defstruct (spatial-index-spec (:constructor make-spatial-index-spec))
  owner-name slot-name graph-name precision)

(defun %registered-spatial-specs (graph)
  "DEF-SPATIAL-INDEX specs for GRAPH, de-duped by (owner . slot), newest-wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-spatial-metadata*))
      (let ((k (cons (spatial-index-spec-owner-name spec)
                     (spatial-index-spec-slot-name spec))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %slot-option-precision (owner-name slot-name)
  "The :SPATIAL-PRECISION declared on OWNER-NAME's SLOT-NAME, or NIL."
  (let ((class (ignore-errors (find-class owner-name nil))))
    (when (and class (class-finalized-p class))
      (let ((slot (find slot-name (class-slots class) :key #'slot-definition-name)))
        (and slot (spatial-precision-spec slot))))))

(defun %def-spatial-precision (graph owner-name slot-name)
  "The precision a DEF-SPATIAL-INDEX declares for (OWNER-NAME . SLOT-NAME), or NIL."
  (let ((spec (find-if (lambda (s)
                         (and (eq (spatial-index-spec-owner-name s) owner-name)
                              (eq (spatial-index-spec-slot-name s) slot-name)))
                       (%registered-spatial-specs graph))))
    (and spec (spatial-index-spec-precision spec))))

(defun %warn-on-precision-conflict (graph owner-name slot-name macro-precision)
  "Warn when a DEF-SPATIAL-INDEX precision is overridden by a slot option.  The
slot option wins -- MOP-first, matching CLASS-SECONDARY-INDEX-DESCRIPTORS -- and
this warning is what keeps the losing declaration from being a silent no-op."
  (let ((slot-precision (%slot-option-precision owner-name slot-name)))
    (when (and slot-precision macro-precision
               (/= slot-precision macro-precision))
      (warn "Spatial precision conflict on ~S.~S in ~S: the :SPATIAL-PRECISION ~
             slot option (~D) wins over DEF-SPATIAL-INDEX (~D).  Declare it in ~
             one place: the slot option for what the schema states, ~
             DEF-SPATIAL-INDEX for what it does not."
            owner-name slot-name (graph-name graph)
            slot-precision macro-precision))
    slot-precision))

(defmacro def-spatial-index (owner-class slot graph-name &key precision)
  "Declare a spatial index on OWNER-CLASS.SLOT in GRAPH-NAME (spanning
OWNER-CLASS's subclasses), optionally at a specific geohash :PRECISION.
Declarative and idempotent like DEF-INDEX and DEF-VIEW.

Use the (slot :spatial-precision N) slot option for what the schema declares and
this macro for what it does not: this can also index a slot NOT marked :INDEX,
and needs no change to an already-persisted class definition.  When both declare
a precision the SLOT OPTION wins and a warning is signalled -- do not declare it
twice.  To adopt a changed precision, the index is rebuilt automatically at the
next open."
  `(let ((spec (make-spatial-index-spec
                :owner-name ',owner-class :slot-name ',slot
                :graph-name ',graph-name :precision ,precision)))
     (push spec (gethash ',graph-name *schema-spatial-metadata*))
     (let ((g (lookup-graph ',graph-name)))
       (when g (%spatial-index-for g ',owner-class ',slot)))
     spec))
```

Replace the placeholder `%spatial-precision-for` from Task 2 with:

```lisp
(defun %spatial-precision-for (graph owner-name slot-name)
  "The geohash precision (OWNER-NAME . SLOT-NAME)'s index is created with.
Precedence: slot option > DEF-SPATIAL-INDEX > the graph default.  MOP-first,
matching CLASS-SECONDARY-INDEX-DESCRIPTORS, so one rule covers both halves of
:INDEX; the conflict warning is what makes the losing declaration audible."
  (let ((macro-precision (%def-spatial-precision graph owner-name slot-name)))
    (or (%warn-on-precision-conflict graph owner-name slot-name macro-precision)
        macro-precision
        (graph-default-spatial-precision graph)
        7)))

(defun install-spatial-indexes (graph)
  "Build any DEF-SPATIAL-INDEX registered for GRAPH that is missing from its
registry, and rebuild any index whose PERSISTED precision no longer matches the
declared one.

The rebuild is not optional and is not deferred to the user, unlike DEF-INDEX's
changed-canonicalizer contract: an index holding cells at two precisions
reintroduces the covering-precision miss the clamp exists to prevent, so leaving
it would be silently wrong.  It is bounded to the one owner's nodes."
  (dolist (spec (%registered-spatial-specs graph))
    (let* ((owner (spatial-index-spec-owner-name spec))
           (slot (spatial-index-spec-slot-name spec))
           (idx (spatial-index-for graph owner slot)))
      (unless idx
        (%spatial-index-for graph owner slot))))
  (let ((stale '()))
    (maphash (lambda (key idx)
               (let ((declared (%spatial-precision-for graph (car key) (cdr key))))
                 (unless (eql declared (spatial-index-precision idx))
                   (push (list (car key) (cdr key) (spatial-index-precision idx)
                               declared)
                         stale))))
             (spatial-indexes graph))
    (dolist (s stale)
      (destructuring-bind (owner slot was now) s
        (log:info "Spatial index ~S.~S declared precision ~D but was written at ~
                   ~D; rebuilding that index." owner slot now was)
        (regenerate-spatial-index graph owner slot)))))
```

- [ ] **Step 5: Call it at open**

In `graph.lisp`'s `open-graph`, immediately after the restore-or-rebuild block from Task 3:

```lisp
        (install-spatial-indexes graph)
```

Add `install-spatial-indexes` and `regenerate-spatial-index` to the `declaim ftype` block at `graph.lisp:5-8`.

- [ ] **Step 6: Export**

`package.lisp`:

```lisp
           #:def-spatial-index
           #:spatial-precision-spec
```

- [ ] **Step 7: Run the tests and the full suite**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

Expected: 0 failures.

- [ ] **Step 8: Commit**

```bash
git add node-class.lisp spatial-registry.lisp graph.lisp package.lisp \
        tests/spatial-scope-tests.lisp
git commit -m "feat(spatial): per-index precision via a slot option and def-spatial-index

Two surfaces.  (slot :spatial-precision N) for what the schema declares;
DEF-SPATIAL-INDEX for what it does not -- it can index a slot not marked :INDEX
and needs no edit to an already-persisted class.

Precedence is MOP-first, slot option over macro, matching
CLASS-SECONDARY-INDEX-DESCRIPTORS so one rule covers both halves of :INDEX.  A
conflict warns, which is what keeps the losing declaration from being a silent
no-op -- the direction of the rule matters less than that.

A declared precision that no longer matches the persisted one rebuilds that one
index at open rather than waiting to be asked.  Mixed precisions in a single
index would reintroduce the covering-precision miss the clamp exists to prevent,
so deferring it to the user would be silently wrong."
```

---

### Task 6: The inert-slot warning and `audit-spatial-slots`

Implements §8.

**Files:**
- Modify: `transactions.lisp` (the sampler), `spatial-query.lisp` (the audit), `package.lisp`
- Test: `tests/spatial-scope-tests.lisp`

**Interfaces:**
- Consumes: Task 2's `node-geometry`
- Produces:
  - `(audit-spatial-slots graph)` → list of `(class-name winning-slot other-slots…)`
  - `*node-geometry-multi-sample-limit*` → `64`

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(def-vertex scope-two-geoms ()
  ((centroid :type geometry :index t)
   (outline :type geometry :index t))
  :graph-db-integration-test)

(test warns-on-a-second-inert-geometry-slot
  "A node with two geometry-valued indexed slots warns and names the winner."
  (with-test-graph (g)
    (declare (ignore g))
    (signals warning
      (with-transaction ()
        (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                              :outline (scope-rect 35d0 55d0 36d0 56d0))))))

(test audit-finds-what-the-sampler-missed
  "AUDIT-SPATIAL-SLOTS sweeps every node, so it reports a class whose only
two-geometry node lies beyond the sampling window."
  (with-test-graph (g)
    (with-transaction ()
      (dotimes (i (1+ graph-db::*node-geometry-multi-sample-limit*))
        (make-scope-two-geoms :centroid (make-point (+ 30d0 (* i 0.001d0)) 50d0))))
    (handler-bind ((warning #'muffle-warning))
      (with-transaction ()
        (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                              :outline (scope-rect 35d0 55d0 36d0 56d0))))
    (let ((report (audit-spatial-slots g)))
      (is (assoc 'scope-two-geoms report))
      (is (eq 'centroid (second (assoc 'scope-two-geoms report)))))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
```

Expected: FAIL — `The variable *NODE-GEOMETRY-MULTI-SAMPLE-LIMIT* is unbound`.

- [ ] **Step 3: Add the sampler**

In `transactions.lisp`, beside `*node-geometry-slot-cache*`:

```lisp
(defparameter *node-geometry-multi-sample-limit* 64
  "How many nodes of a class are checked for a SECOND geometry-valued indexed
slot before the check is retired for that class.  Checking only the first node
would miss a schema where a centroid is populated at creation and an extent is
filled in later; checking always would cost a slot read per :INDEX slot -- scalars
included, since NODE-GEOMETRY-INDEX-SLOTS returns them all -- on every spatial
write forever.  AUDIT-SPATIAL-SLOTS is the exhaustive sweep.")

(defvar *node-geometry-multi-sample-counts*
  #+sbcl (make-hash-table :test 'eq :synchronized t)
  #+ccl (make-hash-table :test 'eq :shared t)
  #+lispworks (make-hash-table :test 'eq :single-thread nil)
  #+ecl (make-hash-table :test 'eq)
  "CLASS -> nodes sampled so far, or :DONE once the check has fired or expired.")

(defun node-geometry-slots-with-values (node)
  "Every indexed slot of NODE that actually holds a geometry, in effective-slot
order.  The first is the one NODE-GEOMETRY selects; any others are INERT."
  (loop for slot in (node-geometry-index-slots (class-of node))
        when (geometryp (ignore-errors (slot-value node slot)))
          collect slot))

(defun %maybe-warn-inert-geometry-slots (node)
  "Warn once per class when a node carries more than one geometry-valued indexed
slot: only the first is indexed, and the rest are silently inert."
  (let* ((class (class-of node))
         (seen (gethash class *node-geometry-multi-sample-counts* 0)))
    (unless (eq seen :done)
      (let ((slots (node-geometry-slots-with-values node)))
        (cond ((rest slots)
               (setf (gethash class *node-geometry-multi-sample-counts*) :done)
               (warn "~S declares ~D geometry-valued indexed slots ~S; only ~S is ~
                      indexed and the rest are INERT.  Index under one slot, or ~
                      run AUDIT-SPATIAL-SLOTS to review the whole graph."
                     (class-name class) (length slots) slots (first slots)))
              ((>= (1+ seen) *node-geometry-multi-sample-limit*)
               (setf (gethash class *node-geometry-multi-sample-counts*) :done))
              (t
               (setf (gethash class *node-geometry-multi-sample-counts*)
                     (1+ seen))))))))
```

In `%spatial-index-node` (Task 2, `transactions.lisp`), add the sampler as the first form of the `when` body:

```lisp
    (when (and geom slot (not (deleted-p node)))
      (%maybe-warn-inert-geometry-slots node)
      (spatial-index-insert
       (%spatial-index-for graph (%indexed-slot-owner-name (class-of node) slot) slot)
       (id node) geom))
```

- [ ] **Step 4: Add the audit sweep**

Append to `spatial-query.lisp`:

```lisp
(defun audit-spatial-slots (graph)
  "Sweep every live node in GRAPH and report each class carrying more than one
geometry-valued indexed slot, as a list of (CLASS-NAME WINNING-SLOT . INERT-SLOTS).

The exhaustive counterpart to the bounded per-class sampler on the write path: it
catches a class whose two-geometry nodes all lie beyond the sampling window, and a
class added long after the graph's migration.  Read-only -- wire it into a schema
test suite."
  (let ((found (make-hash-table :test 'eq)))
    (flet ((check (node)
             (unless (deleted-p node)
               (let ((class (class-of node)))
                 (unless (gethash class found)
                   (let ((slots (node-geometry-slots-with-values node)))
                     (when (rest slots)
                       (setf (gethash class found) slots))))))))
      (map-vertices #'check graph)
      (map-edges #'check graph))
    (let ((result '()))
      (maphash (lambda (class slots)
                 (push (cons (class-name class) slots) result))
               found)
      result)))
```

- [ ] **Step 5: Export**

`package.lisp`:

```lisp
           #:audit-spatial-slots
           #:node-geometry-slots-with-values
```

- [ ] **Step 6: Run the tests and the full suite**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::spatial-scope-suite))'
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
```

Expected: 0 failures. If an existing test now emits the new warning, wrap that form in `(handler-bind ((warning #'muffle-warning)) …)` rather than weakening the check.

- [ ] **Step 7: Commit**

```bash
git add transactions.lisp spatial-query.lisp package.lisp tests/spatial-scope-tests.lisp
git commit -m "feat(spatial): warn on inert geometry slots, add audit-spatial-slots

A class declaring two geometry slots indexes only the first; the rest are
silently inert.  This cannot be caught at class finalization -- the engine
deliberately refuses to compare the declared ':type geometry' symbol, which is
read in the application's package and is not reliably EQ to GRAPH-DB:GEOMETRY --
so the check is value-based, on the maintenance path.

Sampled over a class's first 64 nodes rather than only its first, which catches
the realistic schema where a centroid is populated at creation and an extent is
filled in later, without paying a slot read per indexed slot on every write
forever.  AUDIT-SPATIAL-SLOTS is the exhaustive read-only sweep for classes the
sampler misses and for classes added long after migration."
```

---

### Task 7: Memory-graph per-index image

Implements §10.

**Files:**
- Modify: `memory-graph.lisp` (image dump `:spatial` key, `%rebuild-derived-from-nodes`, image version)
- Test: `tests/memory-graph-tests.lisp`

**Interfaces:**
- Consumes: Task 2's registry, Task 3's sidecar record shape
- Produces: image `:spatial` becomes a list of `(owner slot dump precision max-cells counts)`

- [ ] **Step 1: Write the failing test**

Append to `tests/memory-graph-tests.lisp`:

```lisp
(test memory-image-round-trips-per-class-spatial-indexes
  "A memory graph's per-(owner . slot) spatial indexes survive dump and restore."
  (let ((dir (make-temp-directory)))
    (unwind-protect
         (let (zone-id)
           (let ((g (make-memory-graph :mem-spatial dir)))
             (unwind-protect
                  (let ((*graph* g))
                    (with-transaction ()
                      (setq zone-id (id (make-scope-zone
                                         :extent (scope-rect 0d0 0d0 1d0 1d0))))))
               (close-graph g)))
           (let ((g (open-memory-graph :mem-spatial dir)))
             (unwind-protect
                  (let ((idx (spatial-index-for g 'scope-zone 'extent)))
                    (is (spatial-index-p idx))
                    (is (has-p zone-id
                               (spatial-index-query-bbox idx 0.1d0 0.1d0 0.2d0 0.2d0))))
               (close-graph g))))
      (cl-fad:delete-directory-and-files dir :if-does-not-exist :ignore))))
```

- [ ] **Step 2: Run to verify failure**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::memory-graph-suite))'
```

Expected: FAIL — the restored graph has no `scope-zone` index.

- [ ] **Step 3: Dump every index**

In `memory-graph.lisp`, replace the `:spatial` dump entry:

```lisp
             :spatial     (let ((dumps '()))
                            (maphash
                             (lambda (key idx)
                               (push (list (car key) (cdr key)
                                           (%dump-mem-skip-list
                                            (spatial-index-skip-list idx))
                                           (spatial-index-precision idx)
                                           (spatial-index-max-cells idx)
                                           (copy-seq
                                            (spatial-index-precision-counts idx)))
                                     dumps))
                             (spatial-indexes graph))
                            dumps)
```

Bump the image version constant in the same `cl-store:store` form (`:version`) by one, so an older image falls into the v1 rebuild path.

- [ ] **Step 4: Restore every index**

In `restore-memory-image`'s `destructuring-bind`, replace the structural `spatial` restore with a loop that recreates each index and replays its dump, then in `%rebuild-derived-from-nodes` replace the single-index block with:

```lisp
    (flet ((reindex (n)
             (unless (deleted-p n)
               (multiple-value-bind (geom slot) (node-geometry n)
                 (when (and geom slot)
                   (spatial-index-insert
                    (%spatial-index-for
                     graph (%indexed-slot-owner-name (class-of n) slot) slot)
                    (id n) geom))))))
      (dolist (v vertices) (reindex v))
      (dolist (e edges)    (reindex e)))
```

- [ ] **Step 5: Run the tests and the full suite**

```bash
sbcl --non-interactive --eval '(ql:quickload :graph-db/test)' --eval '(fiveam:run! (quote graph-db/test::memory-graph-suite))'
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

Expected: 0 failures.

- [ ] **Step 6: Commit**

```bash
git add memory-graph.lisp tests/memory-graph-tests.lisp
git commit -m "feat(spatial): per-class spatial indexes on the memory backend

The memory image's :spatial key held one index; it now holds one record per
(owner . slot), each carrying its own precision, insert cap and histogram, so a
memory graph reopens structurally rather than rebuilding.  The image version is
bumped so an older image falls into the existing rebuild-from-nodes path."
```

---

### Task 8: Documentation, changelog, and memory

Implements §14. No code; this is the definition of done from the spec.

**Files:**
- Modify: `CHANGELOG.md`, `README.md`, `SPATIAL-TODO.md`, `docs/vivace-graph-v3-doc.org`, `graph-db.asd` (version)
- Create: a memory file under `/Users/kraison/.claude/projects/-Users-kraison-work-vivace-graph-v3/memory/` and its `MEMORY.md` index line

- [ ] **Step 1: CHANGELOG**

Under `## [Unreleased]`, add `### Changed` and `### Fixed` sections. The version becomes **3.0.0** — the file's own SemVer preamble makes MAJOR mandatory for incompatible changes *including on-disk storage format bumps*, and this is both. Cover: the required scope argument, the removal of the `spatial-index` accessor and `rebuild-spatial-index`, the new Prolog arities, per-index precision and its two declaration surfaces, the v3 sidecar with automatic index-only migration, the insert cap and self-healing clamp, `audit-spatial-slots`, and that downgrade after migration is unsupported.

- [ ] **Step 2: Bump the ASDF version**

`graph-db.asd`, all three `:version "2.1.1"` occurrences (lines 17, 115, 136) → `"3.0.0"`.

- [ ] **Step 3: Rewrite the manual's spatial chapter**

`docs/vivace-graph-v3-doc.org` Chapter 13 (from line 1876). Rewrite: making a node type spatial, per-index precision and both declaration surfaces, the required scope on every entry point, the new Prolog arities, and the filter/refine note at line 1961, which currently describes a single graph-wide index and the `make-graph :spatial-precision` keyword as the only precision control. Also update Chapter 3's mentions of the spatial sidecar (line 431) and `rebuild-spatial-index` (line 442).

- [ ] **Step 4: README compatibility note**

Add an entry in the style of the existing 2016-12-12 UUID note: what breaks (query signatures, `spatial-index` accessor, Prolog arities), what migrates automatically (the index, at first open, without touching node data), and that downgrade after migration is unsupported.

- [ ] **Step 5: Reconcile SPATIAL-TODO.md**

11 references. Close what this delivers; leave what it does not.

- [ ] **Step 6: Write the memory**

Create the memory file recording the outcome and the non-obvious constraints — the prefix-nesting asymmetry that makes a coarse insert unsafe without the clamp, the insert/remove determinism requirement on `max-cells`, and that a mixin-owned geometry slot does *not* separate sibling subclasses. Add its one-line pointer to `MEMORY.md`, and cross-link `vector-segments.md` and `general-index-design.md`, since spatial now joins the same `(owner . slot)` family.

- [ ] **Step 7: Final full matrix**

```bash
sbcl --non-interactive --eval '(asdf:test-system :graph-db)'
sbcl --non-interactive --eval '(asdf:test-system :graph-db/concurrency-test)'
sbcl --non-interactive --eval '(asdf:test-system :graph-db/acid-test)'
ecl --eval '(asdf:test-system :graph-db)' --eval '(ext:quit)'
```

Expected: 0 failures on all four.

- [ ] **Step 8: Commit**

```bash
git add CHANGELOG.md README.md SPATIAL-TODO.md docs/vivace-graph-v3-doc.org graph-db.asd
git commit -m "docs(spatial): 3.0.0 changelog, manual chapter 13, compatibility note

MAJOR: the query API breaks and the on-disk spatial format bumps to v3, either
of which the changelog's SemVer preamble makes sufficient on its own."
```

- [ ] **Step 9: File the deferred work**

Open GitHub issues for the two items §13 defers, so they are tracked rather than remembered:

1. **CR-3.2 — index a node under every indexed geometry slot.** Needs a plural `node-geometries` protocol through all six `node-geometry` consumers, a ruling on distance semantics for a multi-geometry node in `find-nodes-near`, removal of Task 6's warning, and a re-derivation (no format change). Note the mine-action-android dependency: their `point_rtree` indexes Points only and agrees with the hub only coincidentally until this lands.
2. **Multi-resolution query probing.** Removes the clamp's selectivity cost by probing ancestor prefixes with exact-cell range scans. No format change; land it when the cost is felt.

---

## Self-Review

**Spec coverage.** §4 → Task 2; §4.1 → Task 2 (lazy creation) and its test; §5 → Task 5; §6 → Task 4; §7.1/§7.2 → Task 1; §7.4 → Task 1 (constants) and Task 3 (`regenerate-spatial-index`) — **the warning text itself is emitted from `%count-cell`'s caller and is covered by Task 1's clamp tests plus Task 3's regenerate test**; §8 → Task 6; §9 → Task 3; §10 → Task 7; §11 → distributed across tasks, all 17 properties placed; §12 → delivered to the android team, no engine work; §13 → Task 8 Step 9; §14 → Task 8.

**Gap found and closed:** §7.4 requires a `warn` on every decrease of `coarsest-precision` naming the node, and a symmetric `log:info` on rise. Task 1 maintains the histogram but has no node in hand — the raw index takes only `node-id`. **Add to Task 2, Step 7**, inside `%spatial-index-node`, wrapping the `spatial-index-insert` call:

```lisp
      (let* ((idx (%spatial-index-for graph
                                      (%indexed-slot-owner-name (class-of node) slot)
                                      slot))
             (before (spatial-index-coarsest-precision idx)))
        (spatial-index-insert idx (id node) geom)
        (let ((after (spatial-index-coarsest-precision idx)))
          (when (< after before)
            (multiple-value-bind (mnl mnt mxl mxt) (geometry-bbox geom)
              (warn "Spatial index ~S.~S coarsened to precision ~D (was ~D) for ~
                     node ~S, bbox (~,4F ~,4F ~,4F ~,4F).  Queries on this index ~
                     now cover more coarsely.  Removing every node stored at that ~
                     precision restores it automatically; ~
                     (REGENERATE-SPATIAL-INDEX graph '~S '~S) rebuilds it now."
                    (%indexed-slot-owner-name (class-of node) slot) slot
                    after before (id node) mnl mnt mxl mxt
                    (%indexed-slot-owner-name (class-of node) slot) slot))))))
```

and symmetrically in `%spatial-unindex-node`, `(log:info …)` when `after` exceeds `before`.

**Placeholder scan.** No TBD/TODO. Every code step carries complete code. Task 4 Step 7 and Task 8 Steps 1–6 are mechanical sweeps over enumerated files with a `grep` to drive them to zero rather than inline listings of ~80 near-identical edits.

**Type consistency.** `spatial-index-coarsest-precision` (public reader) vs the `coarsest` struct slot — used consistently. `%spatial-index-for` (creating) vs `spatial-index-for` (non-creating) — the distinction is load-bearing in Task 4's error contract and Task 2's `%spatial-unindex-node`; spellings match at every call site. `node-geometry` returns two values from Task 2 onward; every consumer updated in that same task. `precision-counts` is the struct slot and the sidecar field name in Tasks 1, 3 and 7.
