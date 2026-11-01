# Multi-slot spatial indexing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Index a node under every declared geometry slot, and make every spatial query name the slot it tests.

**Architecture:** A declared-type test replaces value sniffing as the definition of a geometry slot. A plural `node-geometries` protocol feeds the write path, the rebuilds and the refine; the scope resolver gains a `:slot` and a per-class unnamed resolution governed by `*spatial-unnamed-slot-policy*`. The sidecar format bump forces one re-derivation.

**Tech Stack:** Common Lisp (SBCL primary; keep `#+ccl` / `#+ecl` / `#+lispworks` arms), FiveAM, the cl-mcp REPL.

**Spec:** `docs/superpowers/specs/2026-11-01-multi-slot-spatial-indexing-design.md` (read it first; section numbers below are its).

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments that point at the spec section or issue (`~/.claude/CLAUDE.md`).
- Branch `multi-slot-spatial-60` (exists, off `experiment`). One commit per task, attribution lines from the session reminder. No push.
- Run only the suites a task touches, with `tests/run-suite.sh <suite>` (fresh image, correct fixtures). Never `fiveam:run!` directly. One FASL cache per checkout: do not run two SBCL builds of this tree at once.
- A new test is shown failing before the change that makes it pass (mutation gate). A 3.0.0 test that encoded "first slot wins" is rewritten to the new contract, never deleted or weakened.
- All new conditions are `query-precondition-error` (`globals.lisp:496`) with a `:reason` naming the class, the slot asked for when there was one, and the class's geometry slots.
- `*spatial-unnamed-slot-policy*` is `defvar`ed in `globals.lisp` (the engine trap: a special an earlier file binds must be defined there), default `:error`, other value `:first-declared`.
- Use the live REPL (`lisp-repl` skill) to check arglists and evaluate; after Task 1 the loaded image must be reloaded from this worktree's `graph-db.asd` (see CLAUDE.md "A worktree loads the main checkout's code" if you work in one; this plan assumes the main checkout on the branch).
- Public issue: any comment on #60 stays domain-neutral (point versus outline).

## Review Focus

Inputs the spec implies but no task's tests exercised until this list was written. Each line's test is in the task named.

1. An update that swaps the two slots' values (centroid gets the old outline, outline the old centroid): each index must end holding the node under the right geometry, not stale cells. Task 3.
2. `regenerate-spatial-index` on one slot of a two-slot class rebuilds only that index and leaves the other index's entries intact. Task 3.
3. `:slot` given as a keyword or string instead of a symbol names a slot nothing declares and must signal the bad-slot error, not fall through to an empty result. Task 4.
4. A class redefined at runtime from one geometry slot to two: the next unnamed query signals under `:error` (memo invalidation), and with the second slot removed again it works unnamed. Task 4.
5. `find-nearest-k` with `:slot` on a scope whose index does not exist yet but whose open transaction holds an own node with that slot bound: the own node is the answer (the #410 rule). Task 5.

---

### Task 1: A geometry slot is identified by its declared type

**Files:**
- Modify: `transactions.lisp:1259-1284` (`node-geometry-index-slots`)
- Modify fixtures that relied on value detection, adding `:type geometry` to the slot: `tests/memory-graph-tests.lisp:37` (`m-place`'s `geom`, and the second geometry type declared just below it if it is untyped too), `tests/node-class-tests.lisp:201, 208, 247` (`pgeom`, `pgeom2`, `g`; the assertions there stay as they are), `tests/spatial-scope-tests.lisp:707, 711` (the two `defclass` precision fixtures)
- Test: `tests/spatial-scope-tests.lisp` (new tests appended after `one-geometry-slot-and-indexed-scalars-stay-silent`, line ~1025)

**Interfaces:**
- Produces: `(%geometry-slot-p slot-definition) => boolean` in `transactions.lisp`, just above `node-geometry-index-slots`. True when `(indexed-p slot)` and the declared type `T` = `(slot-definition-type slot)` satisfies: `T` is not `t`, and either `(subtypep 'geometry T)` is true, or `T` is a symbol with `(string= (symbol-name T) "GEOMETRY")`. `subtypep` on an unknown type specifier can signal on some implementations: wrap it in `ignore-errors`.
- Produces: `node-geometry-index-slots (class)` now returns only the names of slots passing `%geometry-slot-p`, in effective-slot order, same cache and invalidation. Update its docstring: the package concern is met by the symbol-name rule (spec §5.0).

- [ ] **Step 1: Write the failing tests**

Append to `tests/spatial-scope-tests.lisp`:

```lisp
(defpackage #:graph-db/test-other-pkg (:use) (:export #:geometry))

(def-vertex scope-typed-variants ()
  ((plain :type geometry :index t)
   (nullable :type (or null geometry) :index t)
   (foreign :type graph-db/test-other-pkg:geometry :index t)
   (untyped :index t)
   (label :type string :index t))
  :graph-db-integration-test)

(test geometry-slots-are-identified-by-declared-type
  "Spec §5.0: :INDEX T plus a type admitting GEOMETRY, by SUBTYPEP or by a
symbol spelled GEOMETRY in another package.  Untyped and scalar indexed slots
are not geometry slots."
  (is (equal '(plain nullable foreign)
             (graph-db::node-geometry-index-slots
              (find-class 'scope-typed-variants))))
  (is (equal '(geom)
             (graph-db::node-geometry-index-slots
              (find-class 'scope-mixed-index)))))

(test untyped-indexed-slot-holding-a-geometry-is-not-spatial
  "Spec §5.0 consequence: a geometry written to an untyped :INDEX T slot is an
ordinary secondary-index value now, so it creates no spatial index."
  (with-test-graph (g)
    (with-transaction ()
      (make-scope-typed-variants :untyped (make-point 12.3424d0 45.6720d0)))
    (is (null (spatial-index-for g 'scope-typed-variants 'untyped)))))
```

- [ ] **Step 2: Run them and watch them fail**

Run: `tests/run-suite.sh spatial-scope-suite 2>&1 | tail -30`
Expected: `geometry-slots-are-identified-by-declared-type` fails (the list includes `untyped` and `label`); `untyped-indexed-slot-holding-a-geometry-is-not-spatial` fails (an index exists).

- [ ] **Step 3: Implement `%geometry-slot-p` and narrow `node-geometry-index-slots`**

Keep the "do not cache before finalization" guard. The class-level `slot-definition-type` is on the effective slot; `node-class.lisp:263-276` already inherits per-slot options from the declaring direct slot, and the type is inherited by the standard effective-slot computation.

- [ ] **Step 4: Run the suites that declare geometry slots**

Run: `for s in spatial-scope-suite spatial-hook-suite spatial-query-suite graph-spatial-suite memory-graph-suite node-class-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done`
Expected: all pass. If a suite name differs, read `def-suite` at the top of its file. A failure in a suite not listed here that declares an untyped `:index t` geometry slot means one more fixture to type: `grep -n ':index t' tests -r | grep -v geometry` lists the candidates.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp tests/spatial-scope-tests.lisp
git commit -m "spatial: a geometry slot is an :index t slot declared to admit GEOMETRY (#60, spec §5.0)"
```

---

### Task 2: The plural protocol and the unnamed-slot policy

**Files:**
- Modify: `globals.lisp` (new `defvar` near line 490, next to `query-precondition-error`)
- Modify: `transactions.lisp:1286-1361` (remove the sampler and warning), `:1393` (invalidator), `:1420-1450` (`node-geometry`), add the new functions after it
- Modify: `spatial-query.lisp:464-539` (remove `audit-spatial-slots`)
- Modify: `package.lisp:663-669, 675` (exports), `tests/package.lisp:108-110` (imports)
- Test: `tests/spatial-scope-tests.lisp:924-1130` (rewrite the warning and audit tests)

**Interfaces:**
- Produces, in `globals.lisp`: `(defvar *spatial-unnamed-slot-policy* :error)` with a docstring giving the two values and the removal promise (spec §4.4).
- Produces, in `transactions.lisp`, all exported except the `%` ones:
  - `(defgeneric node-geometries (node))` => list of `(slot-name . geometry)`, effective-slot order. Default method on `node`: if `(%node-geometry-method-owner-name (class-of node))` is non-nil, call `node-geometry` and return `(list (cons slot geom))` when `geom` is non-nil, else `nil`; otherwise collect `(cons slot value)` for each slot in `node-geometry-index-slots` whose `(ignore-errors (slot-value node slot))` passes `geometryp`. Method on `t` returns `nil`.
  - `(node-slot-geometry node slot-name)` => geometry or nil: `(cdr (assoc slot-name (node-geometries node)))`.
  - `(%multi-geometry-slot-class-p class)` => true when the class has no custom `node-geometry` method and more than one slot in `node-geometry-index-slots`.
  - `(%unnamed-spatial-slot class)` => the slot name an unnamed query resolves to for a class with **no** custom method: the only slot when one; when several, under `:error` signal `query-precondition-error`; under `:first-declared` warn once per class (see below) and return the first. Signals a plain `error` for a class with no geometry slot (callers check first).
  - `(%warn-first-declared-once class slot)`: a CL `warn` once per class per image, text containing the class name, `"FIRST-DECLARED"` and the slot chosen, tracked in a synchronized hash table `*spatial-first-declared-warned*` (same `#+` table idiom as `*node-geometry-slot-cache*`), cleared for the class in `%invalidate-node-class-slot-caches`.
  - `node-geometry` default method on `node` becomes: with a custom method this method is not reached; otherwise `(let ((slot (%unnamed-spatial-slot class))) (values (node-slot-geometry node slot) slot))` when the class has geometry slots, else `(values nil nil)`. Docstring rewritten: single-geometry convenience and extension point; multi-slot classes follow the policy (spec §5.1).
- Removes: `*node-geometry-multi-sample-limit*`, `*node-geometry-multi-sample-counts*`, `node-geometry-slots-with-values`, `%maybe-warn-inert-geometry-slots`, the call at `transactions.lisp:1546`, `audit-spatial-slots`, and their exports/imports.

- [ ] **Step 1: Rewrite the 3.0.0 tests and add the new ones**

In `tests/spatial-scope-tests.lisp`, delete `reset-two-geoms-sampler`, `reset-two-geoms-combined-sampler`, and the tests `warns-on-a-second-inert-geometry-slot`, `warns-once-per-class-not-once-per-node`, `custom-node-geometry-method-silences-the-sampler`, `custom-node-geometry-method-silences-the-audit`, `custom-two-value-node-geometry-method-is-still-audited`, `audit-finds-what-the-sampler-missed` (read to its end first; keep any fixture it defines that later tests use). Rewrite `one-geometry-slot-and-indexed-scalars-stay-silent` to assert `(equal '((geom . <the point>)) ...)` shape via `(mapcar #'car (node-geometries node))` = `'(geom)`. Keep the fixtures `scope-two-geoms`, `scope-two-geoms-combined`, `scope-two-geoms-reporting`. Add:

```lisp
(test node-geometries-lists-every-bound-geometry-slot
  (with-test-graph (g)
    (declare (ignore g))
    (let (n)
      (with-transaction ()
        (setq n (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                                      :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (is (equal '(centroid outline) (mapcar #'car (node-geometries n))))
      (is (eq :point (geometry-kind (node-slot-geometry n 'centroid))))
      (is (eq :polygon (geometry-kind (node-slot-geometry n 'outline))))
      (is (null (node-slot-geometry n 'nothing))))))

(test node-geometries-of-a-custom-method-class-is-its-one-geometry
  (with-test-graph (g)
    (declare (ignore g))
    (let (combined reporting)
      (with-transaction ()
        (setq combined (make-scope-two-geoms-combined
                        :centroid (make-point 30d0 50d0)
                        :outline (scope-rect 35d0 55d0 36d0 56d0))
              reporting (make-scope-two-geoms-reporting
                         :centroid (make-point 30d0 50d0)
                         :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (is (equal '(nil) (mapcar #'car (node-geometries combined))))
      (is (equal '(centroid) (mapcar #'car (node-geometries reporting)))))))

(test unnamed-node-geometry-on-a-multi-slot-class-follows-the-policy
  (with-test-graph (g)
    (declare (ignore g))
    (let (n)
      (with-transaction ()
        (setq n (make-scope-two-geoms :centroid (make-point 30d0 50d0)
                                      :outline (scope-rect 35d0 55d0 36d0 56d0))))
      (let ((graph-db::*spatial-unnamed-slot-policy* :error))
        (signals query-precondition-error (node-geometry n)))
      (let ((graph-db::*spatial-unnamed-slot-policy* :first-declared)
            (warnings 0))
        (handler-bind ((warning (lambda (c)
                                  (when (search "FIRST-DECLARED"
                                                (string-upcase (princ-to-string c)))
                                    (incf warnings))
                                  (muffle-warning c))))
          (remhash (find-class 'scope-two-geoms)
                   graph-db::*spatial-first-declared-warned*)
          (is (eq 'centroid (nth-value 1 (node-geometry n))))
          (is (eq 'centroid (nth-value 1 (node-geometry n)))))
        (is (= 1 warnings) "one warning per class, not per call")))))
```

Note: the `with-transaction` creating a `scope-two-geoms` node must not itself signal; Task 3 makes the write path plural. Until then the write path calls `node-geometry`, which now signals under `:error`. So in this task also do Step 3's minimal write-path change: `%spatial-index-node` and `%spatial-unindex-node` loop over `node-geometries` (Task 3 finishes the rest).

- [ ] **Step 2: Run and watch the new tests fail**

Run: `tests/run-suite.sh spatial-scope-suite 2>&1 | tail -30`
Expected: the three new tests fail with undefined `node-geometries` / `node-slot-geometry`.

- [ ] **Step 3: Implement**

The `defvar` in `globals.lisp`; the functions and generic in `transactions.lisp`; the write-path loops (keep the coarsening warning per insert, keep `%node-spatial-owner-name` as the one owner computation); the removals; exports and imports; `tests/package.lisp` imports `node-geometries`, `node-slot-geometry` (they are exported, so the import list that shadows internal symbols needs only the `%` ones it uses).

- [ ] **Step 4: Run the touched suites**

Run: `for s in spatial-scope-suite spatial-hook-suite spatial-query-suite graph-spatial-suite subset-replication-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done`
Expected: all pass. `grep -rn 'audit-spatial-slots\|node-geometry-slots-with-values\|multi-sample' --include=*.lisp . | grep -v .worktrees` prints nothing.

- [ ] **Step 5: Commit**

```bash
git add globals.lisp transactions.lisp spatial-query.lisp package.lisp tests/package.lisp tests/spatial-scope-tests.lisp
git commit -m "spatial: NODE-GEOMETRIES, NODE-SLOT-GEOMETRY and the unnamed-slot policy; the inert-slot warning and audit go (#60, spec §5.1)"
```

---

### Task 3: Every geometry slot is maintained on the write path, the rebuilds and the overlay

**Files:**
- Modify: `transactions.lisp` (`%spatial-index-node`, `%spatial-unindex-node`, finished from Task 2)
- Modify: `spatial-query.lisp:57-78` (`%overlay-spatial-candidates`), `:357-400` (`rebuild-spatial-indexes`), `:414-460` (`regenerate-spatial-index`)
- Modify: `memory-graph.lisp:796-844`, `:940-955`
- Test: new file `tests/spatial-multi-slot-tests.lisp`, added to `graph-db.asd` after `(:file "spatial-scope-tests")` (line ~700, append-don't-insert discipline), suite `spatial-multi-slot-suite :in graph-db-suite`

**Interfaces:**
- Consumes: `node-geometries`, `%node-spatial-owner-name`, `%spatial-index-for`, `spatial-index-for`, `spatial-index-insert (idx id geom tag)`, `spatial-index-remove (idx id geom)`.
- Produces: `(%node-spatial-keys node)` in `transactions.lisp` => list of `((owner . slot) . geometry)` for the node, built from `node-geometries` and `%node-spatial-owner-name`. Every maintenance path below iterates it.
- `%overlay-spatial-candidates (ov keys types)`: an own node is a candidate when any of its `%node-spatial-keys` keys is in `keys` (`equalp`). `keys` is always a list after Task 4; until then keep the `:all` arm.
- `regenerate-spatial-index (graph owner-name slot-name)`: re-inserts a node when `(assoc (cons owner-name slot-name) (%node-spatial-keys node) :test #'equalp)`.

- [ ] **Step 1: Write the failing tests**

New file, header `(in-package #:graph-db/test)`, fixture:

```lisp
(def-vertex ms-area ()
  ((centroid :type geometry :index t)
   (outline  :type geometry :index t))
  :graph-db-integration-test)

(defun ms-rect (min-lon min-lat max-lon max-lat)
  (make-polygon (list (list (list min-lon min-lat) (list max-lon min-lat)
                            (list max-lon max-lat) (list min-lon max-lat)
                            (list min-lon min-lat)))))

(defun ms-in-index-p (g slot id min-lon min-lat max-lon max-lat)
  (let ((idx (spatial-index-for g 'ms-area slot)))
    (and idx (has-p id (spatial-index-query-bbox idx min-lon min-lat
                                                 max-lon max-lat)))))
```

Tests (assertions as named; windows chosen so the centroid's cell and the outline's cells are disjoint, e.g. centroid `(make-point 30d0 50d0)`, outline `(ms-rect 35d0 55d0 36d0 56d0)`, centroid window `29.9 49.9 30.1 50.1`, outline window `34.9 54.9 36.1 56.1`):

- `create-indexes-both-slots`: after create, `ms-in-index-p` true for `'centroid` in the centroid window and `'outline` in the outline window; false crosswise.
- `update-that-nils-a-slot-leaves-its-index`: update with `:outline nil` (use the pattern other tests use for `update-node` or `(setf (slot-value ...))` plus `save` inside `with-transaction`); afterwards centroid present, outline absent, and `(spatial-index-for g 'ms-area 'outline)` still exists.
- `update-that-swaps-the-slots-reindexes-each` (Review Focus 1): set centroid to a point inside the old outline window and outline to a rect around the old centroid; each index holds the id only in its new window.
- `delete-removes-from-both-indexes`.
- `rebuild-recreates-both-indexes`: `(rebuild-spatial-indexes g)` returns 2 for one node (count is per insert); both present.
- `regenerate-one-slot-leaves-the-other` (Review Focus 2): `(regenerate-spatial-index g 'ms-area 'outline)` => 1, centroid index still answers.
- `overlay-own-node-is-a-candidate-under-either-slot`: inside one `with-transaction`, create the node then call `(find-nodes-within 'ms-area (ms-rect 29d0 49d0 31d0 51d0) :slot 'centroid)` and `(... :slot 'outline)` with the outline's window; each returns the node. This test needs Task 4's `:slot`; write it now, mark it with `(skip "Task 4")` at the top, and un-skip in Task 4.

- [ ] **Step 2: Run and watch them fail**

Run: `tests/run-suite.sh spatial-multi-slot-suite 2>&1 | tail -30`
Expected: `create-indexes-both-slots` fails on the outline check; the others fail accordingly.

- [ ] **Step 3: Implement `%node-spatial-keys` and switch every path to it**

The memory-graph rebuilds and `%rebuild-derived-from-nodes` loop the same way. `%mem-lznode-may-be-spatial-p` already gates on `node-geometry-index-slots`, now declared geometry slots: unchanged.

- [ ] **Step 4: Run the touched suites**

Run: `for s in spatial-multi-slot-suite spatial-scope-suite spatial-hook-suite spatial-query-suite memory-graph-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done` (confirm the memory-graph suite's name from `tests/`).
Expected: all pass except the skipped overlay test.

- [ ] **Step 5: Commit**

```bash
git add transactions.lisp spatial-query.lisp memory-graph.lisp graph-db.asd tests/spatial-multi-slot-tests.lisp
git commit -m "spatial: maintain every geometry slot's index on write, rebuild, regenerate and overlay (#60, spec §5.2)"
```

---

### Task 4: Scope resolution takes a slot, is subclass-inclusive, and applies the policy

**Files:**
- Modify: `spatial-registry.lisp:15-33` (`class-spatial-index-keys`), `:170-220` (memo and `%spatial-scope-entry`), `:245-284` (`%resolve-spatial-scope`)
- Modify: `spatial-query.lisp:79-141` (`%do-scoped-candidates`), `:143-200` (the three entry points), `:238-280` (`find-nearest-k`)
- Test: `tests/spatial-multi-slot-tests.lisp`

**Interfaces:**
- Consumes: `%multi-geometry-slot-class-p`, `%unnamed-spatial-slot`, `node-geometry-index-slots`, `%node-geometry-method-owner-name`, `%indexed-slot-owner-name`, `all-node-types (graph)` (`schema.lisp:204`).
- Produces, in `spatial-registry.lisp`:
  - `(%class-and-subclasses class)` => the class and every transitive `class-direct-subclasses` member that is a finalized `node-class`.
  - `(%class-keys-for-slot class slot)` => `(owner . slot)` keys for `class` and its subclasses that declare `slot` among `node-geometry-index-slots`; owner via `%indexed-slot-owner-name` per declaring class; deduped `equalp`.
  - `(%class-unnamed-keys class)` => for a custom-method class: `(class-spatial-index-keys class nil)`; for a class with one geometry slot: its key; for a multi-slot class: the key of `(%unnamed-spatial-slot class)` (which signals or warns per policy); plus, for every subclass, the same rule applied to the subclass (so a subclass's own single slot is reached, and a multi-slot subclass is governed by the policy).
  - `%spatial-scope-entry` memo key becomes `(cons class-name slot-designator)` in an `equal` table, where the designator is the slot name or `:unnamed`; value `(declared-p . keys)`. Under `:first-declared` the warning fires on resolution, so a memo hit does not re-warn; that is the "once per class" the spec asks for.
  - `(%resolve-spatial-scope scope graph &key (slot nil slot-p))` => `(values indexes type-names tags keys)`. With `slot-p`: a named class whose `%class-keys-for-slot` is empty signals the bad-slot error; `:all` collects `%class-keys-for-slot` over every finalized node class in `(all-node-types graph)`. Without: `%class-unnamed-keys` per named class; `:all` applies it to every spatially indexed class in `(all-node-types graph)`. `keys` is always a list; `type-names` is `nil` for `:all`.
  - `class-spatial-index-keys (class graph)` keeps its meaning (the class's own keys) and its export; its docstring points at `%class-keys-for-slot` for the scope.
- Produces, in `spatial-query.lisp`:
  - `%do-scoped-candidates ((node-var geom-var scope graph &key slot bbox radius (guard t)) &body body)`: resolves with `:slot`, and binds `geom-var` to `(%scope-geometry node keys)` = the geometry of the first `%node-spatial-keys` entry whose key is in `keys`; a candidate with none is skipped before `body`.
  - `find-nodes-within (scope area &key slot (graph *graph*))`, `find-nodes-intersecting (scope area &key slot (graph *graph*))`, `find-nodes-near (scope lat lon radius &key slot (graph *graph*))`, `find-nearest-k (scope lat lon k &key slot (graph *graph*) (max-radius 2.5d4))`. Their refine uses `geom-var`, never `node-geometry`. `find-nearest-k` passes `:slot` to both its resolve and its `find-nodes-near` calls.

- [ ] **Step 1: Write the failing tests**

Fixtures added to `tests/spatial-multi-slot-tests.lisp`:

```lisp
;; Centroid inside the outline (the ordinary case) and a concave outline whose
;; centroid falls outside it (spec §2).
(def-vertex ms-base () ((loc :type geometry :index t)) :graph-db-integration-test)
(def-vertex ms-special (ms-base) ((precise-loc :type geometry :index t))
  :graph-db-integration-test)
(def-vertex ms-only-sub (ms-base) () :graph-db-integration-test)
```

Tests:

- `within-by-centroid-and-by-outline-differ`: one `ms-area` with centroid `(30 50)` and outline `(ms-rect 29 49 31 51)`; a second with centroid `(40 60)` and outline `(ms-rect 29.5 49.5 30.5 50.5)` (outline crosses the query area, centroid far away). Query area `(ms-rect 29.8 49.8 30.2 50.2)`: `:slot 'centroid` returns only the first; `:slot 'outline` with `find-nodes-intersecting` returns both; `find-nodes-within` with `:slot 'outline` returns neither (neither outline lies within), with `:slot 'centroid` the first. (Skip the exact polygon checks when `graph-db::*geos-available-p*` is nil, as `tests/spacetime/register-tests.lisp` does.)
- `near-distance-is-measured-to-the-named-slot`: node centroid `(30 50)`, outline `(ms-rect 30.5 50.5 30.7 50.7)`; `(find-nodes-near 'ms-area 50d0 30d0 1d5 :slot 'centroid)` gives distance `< 1d0`; with `:slot 'outline` the distance is within 1 % of `(geodesic-distance 50d0 30d0 50.6d0 30.6d0)`.
- `nearest-k-orders-by-the-named-slot`: two nodes whose centroid order and outline order from the query point are opposite; `find-nearest-k ... 2 :slot 'centroid` and `:slot 'outline` return them in opposite orders.
- `unnamed-query-on-a-multi-slot-class-follows-the-policy`: under `:error`, `(signals query-precondition-error (find-nodes-within 'ms-area area))` and the same for `-intersecting`, `-near`, `find-nearest-k`; under `:first-declared` the within result equals the `:slot 'centroid` result and one warning fires (reset `*spatial-first-declared-warned*` and clear `*spatial-scope-memo*` first).
- `single-slot-class-is-unchanged-with-or-without-slot`: `scope-probe` nodes from `spatial-scope-tests`' fixture: `(find-nodes-within 'scope-probe area)` equals `(... :slot 'geom)` under both policies, and no warning fires.
- `parent-scope-reaches-a-subclass-slot`: an `ms-special` with `loc` far away and `precise-loc` inside the area, an `ms-only-sub` with `loc` inside. `(find-nodes-within 'ms-base area :slot 'precise-loc)` returns the `ms-special`; `(find-nodes-within 'ms-base area :slot 'loc)` returns the `ms-only-sub` only; unnamed `'ms-base` signals under `:error` (the subclass is multi-slot); `(find-nodes-within :all area :slot 'precise-loc)` returns the `ms-special`; `(find-nodes-within 'ms-only-sub area)` works unnamed.
- `bad-slot-signals` (Review Focus 3): `:slot 'nothing`, `:slot :centroid` and `:slot "centroid"` on `'ms-area` each signal `query-precondition-error` whose text names `MS-AREA`.
- `custom-method-class-with-slot`: `scope-two-geoms-reporting` node; `:slot 'centroid` finds it, `:slot 'outline` returns nil (declared but the method never keys there), `:slot 'nothing` signals; `scope-site` (one-value method) with `:slot 'where` signals; `(find-nodes-within :all area :slot 'precise-loc)` does not signal because of them.
- `redefining-a-class-to-two-slots-flips-the-unnamed-rule` (Review Focus 4): inside the test `(def-vertex ms-redef () ((a :type geometry :index t)) :graph-db-integration-test)`, create a node, unnamed query works; redefine with a second slot `b`, unnamed query signals; redefine back, works. Follow `tests/spatial-scope-tests.lisp:739-840` (`scope-restale`) for the in-test redefinition idiom.
- Un-skip `overlay-own-node-is-a-candidate-under-either-slot`.

- [ ] **Step 2: Run and watch them fail**

Run: `tests/run-suite.sh spatial-multi-slot-suite 2>&1 | tail -40`
Expected: every new test fails: `:slot` is an unknown keyword argument.

- [ ] **Step 3: Implement the resolver, the macro and the four entry points**

Error reasons, as format strings: `"~S declares no geometry slot named ~S (its geometry slots: ~S)"`, `"~S has ~D geometry slots ~S: name the one to test with :SLOT, or bind *SPATIAL-UNNAMED-SLOT-POLICY* to :FIRST-DECLARED"`. Keep `%scope-admits-p` and the tag pre-filter as they are.

- [ ] **Step 4: Run the suites**

Run: `for s in spatial-multi-slot-suite spatial-scope-suite spatial-query-suite spatial-intersect-suite graph-spatial-suite spatial-hook-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done`
Expected: all pass. The 3.0.0 test at `tests/spatial-scope-tests.lisp` that pinned "parent scope does not reach the subclass's extra slot" (grep `Known limitation` / `precise`) is rewritten to the new rule, not deleted.

- [ ] **Step 5: Commit**

```bash
git add spatial-registry.lisp spatial-query.lisp tests/spatial-multi-slot-tests.lisp tests/spatial-scope-tests.lisp
git commit -m "spatial: queries name the slot they test; scopes are subclass-inclusive; unnamed queries follow the policy (#60, spec §5.3-5.4, §6.1)"
```

---

### Task 5: Prolog arities, the replication filter, and the own-node nearest case

**Files:**
- Modify: `spatial-query.lisp:201-237, 282-305` (functors and filter)
- Modify: `package.lisp:684-691` (export `find-within/4 find-intersects/4 find-near/6 find-nearest/6`)
- Test: `tests/spatial-multi-slot-tests.lisp`, `tests/subset-replication-tests.lisp`

**Interfaces:**
- Produces: `find-within/4 (?node ?scope ?slot ?area)`, `find-intersects/4 (?node ?scope ?slot ?area)`, `find-near/6 (?node ?scope ?slot ?lat ?lon ?radius)`, `find-nearest/6 (?node ?scope ?slot ?lat ?lon ?k)`, each a `def-global-prolog-functor` next to its `/3` or `/5` twin, passing `(var-deref ?slot)` as `:slot`. An unbound `?slot` derefs to a variable, which is not a symbol naming a slot, so the Lisp function's bad-slot error is what fires: no separate check.
- `make-spatial-replication-filter (area)`: accepts when `(node-geometries node)` is empty, or when any pair's `%geometry-rep-point` is contained in `area`.

- [ ] **Step 1: Write the failing tests**

In `tests/spatial-multi-slot-tests.lisp`, one test per arity against the `within-by-centroid-and-by-outline-differ` fixture, e.g.:

```lisp
(test prolog-find-within/4-tests-the-named-slot
  (with-test-graph (g)
    (declare (ignore g))
    ;; fixture as in within-by-centroid-and-by-outline-differ
    (let ((area (ms-rect 29.8d0 49.8d0 30.2d0 50.2d0)))
      (is (= 1 (length (select-flat (?n) (find-within ?n ms-area centroid ?area)
                         ;; bind ?area via (is ?area <area>) as spatial-prolog-tests does
                         ))))
      (is (= 2 (length (select-flat (?n) (find-intersects ?n ms-area outline ?area))))))))
```

and `prolog-literal-list-scope-with-slot` passing `(ms-area ms-base)` as the scope literal with `:slot loc`-equivalent, as the 3.0.0 list-scope test did. Plus `nearest-k-finds-an-own-node-before-any-index-exists` (Review Focus 5): fresh graph, inside one transaction create an `ms-area` and call `(find-nearest-k 'ms-area 50d0 30d0 1 :slot 'outline)`; the own node is returned.

In `tests/subset-replication-tests.lisp` add `filter-accepts-a-node-by-any-geometry`: an `ms-area` whose centroid is outside `*area*` and whose outline's bbox centre is inside is accepted; one with both outside is rejected.

- [ ] **Step 2: Run and watch them fail**

Run: `tests/run-suite.sh spatial-multi-slot-suite 2>&1 | tail -20; tests/run-suite.sh subset-replication-suite 2>&1 | tail -5`
Expected: unknown functor for the `/4` and `/6` goals; the filter test fails on the first node.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run the suites**

Run: `for s in spatial-multi-slot-suite subset-replication-suite spatial-prolog-suite prolog-functor-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add spatial-query.lisp package.lisp tests/spatial-multi-slot-tests.lisp tests/subset-replication-tests.lisp
git commit -m "spatial: slot-taking Prolog arities; the replication filter ships on any geometry (#60, spec §6.2, §7)"
```

---

### Task 6: Persistence: sidecar format 6 and the memory image

**Files:**
- Modify: `spatial-index.lisp:78-91` (`+spatial-index-format+` 5 → 6, history comment)
- Modify: `graph.lisp:141` (adoption list gains 5)
- Modify: `memory-graph.lisp:722-770, 834-900` (image version 8 → 9; a v6, v7 or v8 image discards its spatial section and calls `%rebuild-memory-spatial-indexes-from-nodes`, as v5 does at `:893`)
- Test: `tests/spatial-multi-slot-tests.lisp`, the memory-graph test file

**Interfaces:**
- Consumes: `save-spatial-index-roots`, `restore-spatial-index-roots`, `spatial-indexes-root-file`.

- [ ] **Step 1: Write the failing tests**

- `a-format-5-sidecar-is-rederived-into-both-indexes`: make a graph on disk, create an `ms-area`, close cleanly; rewrite the sidecar with `cl-store:store` of the restored plist with `:format 5` and `:indexes` holding only the centroid record (the shape at `graph.lisp:~80-98`); reopen; both `(spatial-index-for g 'ms-area 'centroid)` and `'outline` answer their windows; the sidecar now reads `:format 6`.
- `a-v8-memory-image-rebuilds-its-spatial-indexes`: a memory graph with an `ms-area`, closed (image written); patch the version field to 8 (find the offset from `write-memory-image-native`'s header writer); reopen; both indexes answer. If patching proves fragile, instead bind the writer's version to 8 through a special variable the writer reads (add `*memory-image-write-version*`, default 9, internal) and write the image under it.

- [ ] **Step 2: Run and watch them fail**

Expected: reopen trusts the patched sidecar/image and the outline index is missing.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run the suites**

Run: `for s in spatial-multi-slot-suite spatial-scope-suite reopen-suite memory-graph-suite; do tests/run-suite.sh $s 2>&1 | tail -3; done` (confirm suite names).
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add spatial-index.lisp graph.lisp memory-graph.lisp tests/spatial-multi-slot-tests.lisp tests/<memory test file>
git commit -m "spatial: sidecar format 6 and memory image v9 force a one-time re-derivation (#60, spec §9)"
```

---

### Task 7: Spacetime registration names its slots

**Files:**
- Modify: `spacetime/register.lisp:69-150` (`register-geometry`), `:367-470` (`register-node`)
- Modify: `spacetime/source.lisp:187-197` (facet validation)
- Test: `tests/spacetime/register-tests.lisp`, `tests/spacetime/source-tests.lisp`

**Interfaces:**
- Produces: `register-geometry (geometry registry &key slot (registry-graph graph-db:*graph*))`; passes `:slot` to `find-nodes-intersecting`, and reads each region's geometry with `(node-slot-geometry region slot)` when `slot` is given, else `(graph-db:node-geometry region)`.
- `:registration` facet: two optional keys validated when present: `:registry-slot` (a non-keyword symbol) and `:geometry-fn` (a non-keyword symbol naming a function of one argument, or nil). Add an `opt-symbol` local to the validator in the style of `req-symbol`, which passes when the key is absent (`+facet-absent+`).
- `register-node`: `geometry` = `(funcall (getf facet :geometry-fn) node)` when non-nil, else `(graph-db:node-geometry node)`; calls `register-geometry` with `:slot (getf facet :registry-slot)`.

- [ ] **Step 1: Write the failing tests**

In `register-tests.lisp`: a region class `ct-region2` with `centroid` and `geom` both `:type geometry :index t`; `a-two-slot-registry-registers-by-the-named-slot`: `(register-geometry (make-point 1d0 1d0) 'ct-region2 :slot 'geom :registry-graph g)` registers to the region whose `geom` contains the point and not to one whose `centroid` only is near; without `:slot` it signals `graph-db:query-precondition-error` under `:error`. In `source-tests.lisp`: a `def-source` whose `:registration` facet carries `:registry-slot geom :geometry-fn st-source-outline` macroexpands without `missing-source-facet`; `:registry-slot :geom` (a keyword) is rejected with the facet's own condition.

- [ ] **Step 2: Run and watch them fail**

Run: `tests/run-suite.sh spacetime-suite 2>&1 | tail -20` (or the spacetime test system's own runner, `graph-db/spacetime-test`'s `run-spacetime-tests`; see `graph-db.asd:598-640`).

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run the spacetime suite**

Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add spacetime/register.lisp spacetime/source.lisp tests/spacetime/register-tests.lisp tests/spacetime/source-tests.lisp
git commit -m "spacetime: registration names the registry slot and the source geometry (#60, spec §6.3)"
```

---

### Task 8: Concurrency coverage for per-slot maintenance

**Files:**
- Modify: `tests/concurrency/spatial-tests.lisp`

- [ ] **Step 1: Add the test**

Define a two-slot vertex in that file's package (its own graph name; follow the file's existing fixture), and a test modelled on the file's existing writer/reader loop: N writer threads create and update nodes with both slots bound in clusters A (centroid) and B (outline); reader threads query `:slot 'centroid` over `*a-box*` and `:slot 'outline` over `*b-box*`; at the end every live node is in exactly its two windows and in neither crosswise, and no reader observed a node under the wrong slot.

- [ ] **Step 2: Run it**

Run: `tests/run-suite.sh concurrent-spatial-suite 2>&1 | tail -5` (if the runner only loads the main test system, use `(asdf:test-system :graph-db/concurrency-test)` in a fresh SBCL with `--dynamic-space-size 16384`, foreground, under ten minutes).
Expected: pass.

- [ ] **Step 3: Commit**

```bash
git add tests/concurrency/spatial-tests.lisp
git commit -m "tests(concurrency): per-slot spatial maintenance under contention (#60)"
```

---

### Task 9: Documentation, example, changelog, issue

**Files:**
- Modify: `CHANGELOG.md` (`## [Unreleased]`, line 12)
- Modify: `docs/vivace-graph-v3-doc.org:3254-3330` (making a type spatial, the limitation at 3282, the inert-slot paragraph at 3303, the query examples), `:3392-3405` (subset replication), `:6380-6400` (the `:registration` facet), `:7031-7100` (API reference entries for `node-geometry`, the four finders, `audit-spatial-slots`, the filter)
- Modify: `examples/example.lisp:50-57, 244-275`
- Modify: `docs/superpowers/specs/2026-07-24-spatial-index-scoping-design.md` §8 and §13 (one-line "superseded by" pointers)
- Modify: `docs/codebase-notes.md:34` if it describes the spatial idiom

- [ ] **Step 1: CHANGELOG**

Under `[Unreleased]`, with the spec's §12 content: **Changed** (every declared geometry slot indexed; `:slot` on the four finders; unnamed multi-slot query signals; `*spatial-unnamed-slot-policy*` and its removal in the next major; a geometry slot is now identified by declared type, so an untyped `:index t` slot holding geometries is no longer spatial; under `:first-declared` a node whose first declared slot is nil is not found unnamed; sidecar 6 and memory image v9 re-derive once), **Removed** (`audit-spatial-slots`, `node-geometry-slots-with-values`, `*node-geometry-multi-sample-limit*`, the write-path inert-slot warning), **Added** (`node-geometries`, `node-slot-geometry`, `find-within/4`, `find-intersects/4`, `find-near/6`, `find-nearest/6`, the `:registry-slot` and `:geometry-fn` facet keys), and a numbered **re-check list for a consumer's port**: every spatial call site names its slot; every `node-geometry` method written to choose between indexed slots is deleted; every registration source says which geometry it registers; schema tests that called `audit-spatial-slots` are dropped. Mark the release MAJOR.

- [ ] **Step 2: Manual**

Rewrite 3282 and 3303 to the new contract; add a two-slot example and one query per slot under "Querying"; state the policy variable; update the replication paragraph (any geometry, rep-point coarseness noted); add the two facet keys to the `:registration` block and a sentence under it; update the API reference signatures and remove the `audit-spatial-slots` entry; add `node-geometries` and `node-slot-geometry` entries.

- [ ] **Step 3: Example**

Give `merchant` a second slot `(service-area :type geometry :index t)`, set it on two merchants, and add one `find-nodes-intersecting 'merchant area :slot 'service-area` next to the existing `find-nodes-within ... :slot 'location` queries (the existing calls on `merchant` must now name `:slot` since the class is multi-slot). Run `tests/load-examples.sh` and confirm it exits 0.

- [ ] **Step 4: Verify the docs hook and commit**

```bash
tests/load-examples.sh 2>&1 | tail -3
git add CHANGELOG.md docs/vivace-graph-v3-doc.org examples/example.lisp docs/superpowers/specs/2026-07-24-spatial-index-scoping-design.md docs/codebase-notes.md
git commit -m "docs: multi-slot spatial indexing, the :slot argument and the unnamed-slot policy (#60)"
```

- [ ] **Step 5: Issue**

Comment on #60: what landed per task (SHAs), the peer note from spec §11 in one paragraph, and that the switch's removal is tracked by a new issue (file it: "Remove `*spatial-unnamed-slot-policy*` after consumers port", labelled, referencing #60). Do not close #60 until the branch is on `experiment` and CI is green; then close with the landing SHA.
