# Widen `type-id` to 32 Bits — Implementation Plan (#166)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Widen `type-id` from 16 to 32 bits and make the type-index stop pre-allocating
for the whole id space — **without any semantic change**. Type-ids stay **per-graph**.

**Architecture:** The field widens in four coupled places (node head, `ve-key`, the CLOS
slot, the schema counters), which must move together or the build is inconsistent. A v2
legacy reader lands alongside the v3 writer so existing data stays readable, following the
`*node-head-reader*` / `deserialize-node-head-v1` precedent already in the tree. The
type-index keeps its O(1) offset arithmetic but stops sizing for the entire space, and
swaps 65,536 mutexes for a bounded stripe set. Migration is logical replay via a
`migrate-graph` variant.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-08-20-namespaces-design.md` §3.4 and §10.
Decision log: `docs/namespace-design-decisions-2026-08-20.md` D11, and the spike record.

## Global Constraints

- **Lisp: spaces only, never tabs. Hard 80-column limit** — code, comments, docstrings and
  strings alike. Verify on **added** lines (`git diff | grep '^+'`); these files carry
  pre-existing long lines that are out of scope.
- **Comments are terse** — state the non-obvious fact, reference an issue or spec section,
  do not narrate.
- **No semantic change.** Ids remain per-graph. No global registry, no distribution, no
  replication change — that is #186. If you find yourself touching `peer-*`, stop.
- **`ve-key`'s big-endian byte order is a convention, not a requirement.** Four comments
  say *"Big endian ints for easy comparison in `ve-key-lessp`"* — but **`ve-key-lessp` does
  not exist**; it was removed. The ve-index is an `lhash` created `:test 've-key-equal`, so
  the bytes are only ever compared for **equality**. Keep MSB-first anyway (it costs
  nothing and a future ordered index may want it) and **fix those four stale comments** to
  say the ordering is conventional.
- **Baseline to preserve:** `graph-db/test` **3684 checks / 3674 pass / 10 skip / 0 fail**,
  `graph-db/spacetime-test` 342, `graph-db/geos-test` 185.
- ⚠ **The baseline is load-order dependent.** "10 skips" identifies the GEOS-**unloaded**
  population. A runner that `load-system`s `graph-db/geos-test` before the main suite pulls
  in `graph-db/geos`, flips `*geos-available-p*`, and ~9 GEOS-gated tests inside the main
  suite execute rather than skip — giving ~3837 with 1 skip. **Not comparable.** Run
  `graph-db/test` alone in its own image.
- Known flake: `POINT-IN-RING-PACKED-CONSES-NOTHING` (#174) — `sb-ext:get-bytes-consed` is
  process-wide. That failing alone is not yours; anything else is.

---

## File Structure

| File | Change |
|---|---|
| `primitive-node.lisp` | `+node-header-size+` 31→33; `pack-node-head` writes 4 bytes; `deserialize-node-head` reads 4; **new** `deserialize-node-head-v2` legacy reader |
| `ve-index.lisp` | `ve-key`'s `type-id` type; four serialisation sites 2→4 bytes; **two hardcoded `18`s** at `:109` and `:126` |
| `vev-index.lisp` | `vev-key`'s `type-id` type; four serialisation sites at `:102, :120, :138, :152` |
| `buffer-pool.lisp` | pooled buffer sizes: **18 is `ve-key`, 34 is `vev-key`** |
| `globals.lisp` | `+ve-key-bytes+` 18→20; `+vev-key-bytes+` 34→36; `+max-node-types+` becomes an initial sizing hint |
| `node-class.lisp` | the `type-id` slot's `:type` |
| `schema.lisp` | `next-edge-id` / `next-vertex-id` types |
| `type-index.lisp` | lock striping; grow-on-demand sizing |
| `backup.lisp` | the v2→v3 migration variant |
| `tests/type-id-width-tests.lisp` (new) | the whole unit's tests |

---

### Task 1: Widen the field everywhere, and add the v2 legacy reader

**Files:** Modify `primitive-node.lisp`, `ve-index.lisp`, **`vev-index.lisp`**,
**`buffer-pool.lisp`**, `globals.lisp`, `node-class.lisp`, `schema.lisp`. Create `tests/type-id-width-tests.lisp`. Register it and
run.

**Interfaces:**
- Produces: `deserialize-node-head-v2` — the *current* 31-byte reader, kept for migration,
  with the same value shape as `deserialize-node-head`.
- `+node-header-size+` becomes 33; `+ve-key-bytes+` becomes 20.

**⚠ These seven files must change together.** Widening the head without the CLOS slot gives
a type error; widening the slot without the schema counters caps assignment at 65535. There
is no useful intermediate state, so this is one task.

- [ ] **Step 1: Write the failing tests**

```lisp
;;;; type-id widened to 32 bits (GH #166).  See
;;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §3.4.
(in-package #:graph-db/test)

(def-suite type-id-width-suite :in graph-db-suite
  :description "type-id is 32 bits wide, on disk and in memory.")
(in-suite type-id-width-suite)

(test node-head-is-33-bytes
  (is (= 33 graph-db::+node-header-size+)))

(test ve-key-is-20-bytes
  (is (= 20 graph-db::+ve-key-bytes+)))

(test node-head-round-trips-a-type-id-above-16-bits
  ;; The whole point: 70000 does not fit in the old 2-byte field.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "head.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128))
           (v (graph-db::%make-vertex :type-id 70000 :revision 7
                                      :data-pointer 12345)))
      (unwind-protect
           (progn
             (graph-db::serialize-node-head mf v 0)
             (multiple-value-bind (d w h ti vw ve vev type-id revision)
                 (graph-db::deserialize-node-head mf 0)
               (declare (ignore d w h ti vw ve vev))
               (is (= 70000 type-id))
               (is (= 7 revision))))
        (graph-db::munmap-file mf)))))

(test ve-key-round-trips-a-type-id-above-16-bits
  (let* ((k (graph-db::make-ve-key :id (graph-db::gen-vertex-id)
                                   :type-id 70000))
         (vec (graph-db::serialize-ve-key k))
         (back (graph-db::deserialize-ve-key vec)))
    (is (= 70000 (graph-db::ve-key-type-id back)))))

(test ve-key-type-id-stays-big-endian
  ;; Convention, not a requirement -- the ve-index is a hash table, so these
  ;; bytes are only compared for equality.  Pinned so the convention is not
  ;; lost by accident.
  (let* ((k (graph-db::make-ve-key :id (graph-db::gen-vertex-id)
                                   :type-id #x01020304))
         (vec (graph-db::serialize-ve-key k)))
    (is (equal '(#x01 #x02 #x03 #x04)
               (list (aref vec 16) (aref vec 17)
                     (aref vec 18) (aref vec 19))))))

(test vev-key-round-trips-a-type-id-above-16-bits
  ;; The vev-index carries its own type-id.  Omitting it truncates silently.
  (let* ((k (graph-db::make-vev-key :out-id (graph-db::gen-vertex-id)
                                    :in-id (graph-db::gen-vertex-id)
                                    :type-id 70000))
         (vec (graph-db::serialize-vev-key k))
         (back (graph-db::deserialize-vev-key vec)))
    (is (= 70000 (graph-db::vev-key-type-id back)))))

(test key-width-constants-match-their-buffers
  ;; buffer-pool.lisp pre-allocates by size; 18 was ve-key and 34 vev-key.
  (is (= 20 graph-db::+ve-key-bytes+))
  (is (= 36 graph-db::+vev-key-bytes+)))

(test v2-legacy-reader-still-reads-a-31-byte-head
  ;; Migration depends on this: the OLD layout must remain readable.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "v2.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128)))
      (unwind-protect
           (let ((i 0))
             ;; Hand-write a v2 head: flags(1) type-id(2 LE) revision(4)
             ;; data-pointer(8) commit-epoch(8) prev-pointer(8).
             (graph-db::set-byte mf 0 0)
             (setf i 1)
             (setf i (graph-db::pack-uint-to-mmap mf i 513 2))
             (setf i (graph-db::pack-uint-to-mmap mf i 9 4))
             (setf i (graph-db::pack-uint-to-mmap mf i 4096 8))
             (setf i (graph-db::pack-uint-to-mmap mf i 77 8))
             (graph-db::pack-uint-to-mmap mf i 0 8)
             (multiple-value-bind (d w h ti vw ve vev type-id revision ptr epoch)
                 (graph-db::deserialize-node-head-v2 mf 0)
               (declare (ignore d w h ti vw ve vev))
               (is (= 513 type-id))
               (is (= 9 revision))
               (is (= 4096 ptr))
               (is (= 77 epoch))))
        (graph-db::munmap-file mf)))))

(test schema-can-assign-a-type-id-above-16-bits
  (let ((s (graph-db::make-schema)))
    (setf (graph-db::schema-next-vertex-id s) 70000)
    (is (= 70000 (graph-db::get-next-type-id s :vertex)))
    (is (= 70001 (graph-db::schema-next-vertex-id s)))))
```

**Note for the implementer:** `pack-uint-to-mmap` may not exist. If not, write the v2 head
in the test with `graph-db::set-byte` directly, or add a small test-local helper — do **not**
add a production helper just for a test. Say which you did in your report.

- [ ] **Step 2: Run the tests, watch them fail**

Expected: FAIL on the width assertions (31 ≠ 33, 18 ≠ 20) and on
`DESERIALIZE-NODE-HEAD-V2` being undefined. If a test errors for a *different* reason —
a wrong accessor name, a missing helper — fix the test and re-run until it fails for the
right reason.

- [ ] **Step 3: Implement**

`primitive-node.lisp`:
- `+node-header-size+` 31 → **33**.
- In `pack-node-head`, the type-id write becomes 4 bytes:
  `(setq i (pack-uint vec (1+ i) (type-id n) 4))`.
- In `deserialize-node-head`, the type-id read becomes `(dotimes (i 4) …)` and its
  `declare` widens to `(integer 0 4294967295)`.
- **Add `deserialize-node-head-v2`** — a byte-for-byte copy of the *current*
  `deserialize-node-head` (2-byte type-id, 31-byte head), with a docstring saying it exists
  for `migrate-graph` and is not on any live read path. Model it on
  `deserialize-node-head-v1` directly above.

`ve-index.lisp` — **four** sites, all big-endian, MSB first, **plus two literals**:
- `serialize-ve-key-mmap`: write bytes 3,2,1,0 of the type-id.
- `deserialize-ve-key-mmap`: read four bytes MSB-first.
- `serialize-ve-key` / `deserialize-ve-key` (the vector forms): indices 16–19.
- The `ve-key` struct's `type-id` type becomes `(integer 0 4294967295)`.
- **Replace the hardcoded `18` at `:109` (`(get-buffer 18)`) and at `:126`** (the bare `18`
  in `deserialize-ve-key`'s return) with `+ve-key-bytes+`.
- Fix the four stale `ve-key-lessp` comments per the constraint above.

`vev-index.lisp` — **the same treatment**, and it is easy to miss because the plan's first
draft omitted it entirely: the `vev-key` struct's `type-id` type, and the four 2-byte sites
at `:102`, `:120`, `:138`, `:152`. Its key layout is out-id(16) in-id(16) type-id, so the
type-id starts at offset 32.

`buffer-pool.lisp` — the pool pre-allocates by size and serves 8/16/18/24/34. **18 is
`ve-key` and 34 is `vev-key`**; both must follow the widths. `serialize.lisp:403`'s 18 is a
UUID (tag + length + 16) and is **unrelated** — leave it.

`globals.lisp`: `+ve-key-bytes+` 18 → **20**, `+vev-key-bytes+` 34 → **36**. Leave
`+max-node-types+` alone in this task — Task 2 owns it.

`node-class.lisp`: the `type-id` slot's `:type` becomes `(unsigned-byte 32)`.

`schema.lisp`: `next-edge-id` and `next-vertex-id` become `(unsigned-byte 32)`.

- [ ] **Step 4: Run the tests, watch them pass**

Then the full `graph-db/test` **alone, GEOS unloaded**. Expect the baseline 3684/3674/10/0
plus your new checks. **Every graph the suite creates is fresh, so it is written in v3 and
read in v3** — a pre-existing on-disk graph would not be readable, which is what Task 3 is
for. If a pre-existing count moves, stop and report.

- [ ] **Step 5: Commit**

```bash
git add primitive-node.lisp ve-index.lisp vev-index.lisp buffer-pool.lisp \
        globals.lisp node-class.lisp schema.lisp graph-db.asd \
        tests/type-id-width-tests.lisp tests/package.lisp
git commit -m "feat(schema): widen type-id to 32 bits, add the v2 head reader (#166)"
```

---

### Task 2: Type-index — bounded locks, sized for what is used

**Files:** Modify `type-index.lisp`, `globals.lisp`. Test in
`tests/type-id-width-tests.lisp`.

**Interfaces:** Consumes Task 1's widened field. `make-type-index` / `open-type-index` keep
their signatures.

**The problem.** `make-type-index` allocates `(* +max-node-types+ +index-list-bytes+)` — at
16 bits that is 65536 × 17 ≈ 1.1 MB, which is tolerable. At 32 bits it would be ~73 GB. It
also builds `(make-array +max-node-types+)` filled with **65,536 mutexes**, per index, per
store, regardless of how many types the store actually uses.

**The approach — keep the arithmetic, shrink the allocation.** Type-ids are assigned
sequentially from 1, so the *used* range is dense and small. Keep `(* type-id
+index-list-bytes+)` O(1) offset arithmetic; size the file for the types actually assigned,
with room to grow, and grow it on demand when a type-id lands past the end. Most
deployments never grow.

**Locks: stripe them.** Replace the per-type array with a fixed set (256), indexed by
`(mod type-id +type-index-lock-stripes+)`. Two types sharing a stripe now serialise against
each other; the critical sections are a push or a remove on one index-list, so the
contention is negligible and the memory is bounded. **State this trade in a comment** — it
is a real behaviour change, not a pure optimisation.

- [ ] **Step 1: Write the failing tests**

```lisp
(test type-index-does-not-preallocate-the-whole-space
  ;; The file is sized for the types in use, not for the id space.
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h.dat" dir)) (* 1024 1024)))
           (idx (graph-db::make-type-index path heap)))
      (unwind-protect
           (is (< (with-open-file (s path :element-type '(unsigned-byte 8))
                    (file-length s))
                  (* 1024 1024)))          ; well under the old ~1.1 MB
        (graph-db::close-type-index idx)))))

(test type-index-grows-for-a-large-type-id
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti2.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h2.dat" dir)) (* 1024 1024)))
           (idx (graph-db::make-type-index path heap))
           (id (graph-db::gen-vertex-id)))
      (unwind-protect
           (progn
             (graph-db::type-index-push id 70000 idx)
             (is (graph-db::index-list-member-p
                  (graph-db::get-type-index-list idx 70000) id)))
        (graph-db::close-type-index idx)))))

(test type-index-locks-are-bounded
  (is (<= graph-db::+type-index-lock-stripes+ 1024)))
```

- [ ] **Step 2: Run, watch them fail**

Expected: the sizing test fails (the file is ~1.1 MB), `+TYPE-INDEX-LOCK-STRIPES+` is
undefined, and the grow test fails or errors past the end of the mapping.

- [ ] **Step 3: Implement**

Add to `globals.lisp`:

```lisp
;; Type-index lock striping (GH #166).  One mutex per type-id cost 65,536 of
;; them per index per store; two types sharing a stripe now serialise, which is
;; a push or a remove on one index-list.
(alexandria:define-constant +type-index-lock-stripes+ 256)

;; Initial type-index capacity in TYPES, not the id ceiling.  The file grows on
;; demand; type-ids are assigned sequentially so the used range stays dense.
(alexandria:define-constant +type-index-initial-types+ 4096)
```

Rework `type-index.lisp`:
- `locks` becomes `(map-into (make-array +type-index-lock-stripes+) 'make-mutex)`,
  selected by `(mod type-id +type-index-lock-stripes+)`.
- `make-type-index` sizes the mmap at `(* +type-index-initial-types+ +index-list-bytes+)`
  and records the current capacity.
- `%ti-list` checks the type-id against capacity and grows the mapping before computing the
  offset. Follow whatever `segment.lisp` does to grow a mapped file — **read it first**;
  do not invent a second mechanism.
- Drop the eager `dotimes (i +max-node-types+)` initialisation loops in both
  `make-type-index` and `open-type-index`; initialise a slot when it is first touched, the
  way the existing `#+ecl` lazy branch already does. That branch's comment explains the
  reasoning (GH #46) — the non-ECL side can now use it too.

- [ ] **Step 4: Run, watch them pass**, then the full suite as in Task 1.

- [ ] **Step 5: Commit**

```bash
git add type-index.lisp globals.lisp tests/type-id-width-tests.lisp
git commit -m "perf(type-index): stripe the locks, size for types in use (#166)"
```

---

### Task 3: The v2 → v3 migration

**Files:** Modify `backup.lisp`. Test in `tests/type-id-width-tests.lisp`.

**Interfaces:** Consumes Task 1's `deserialize-node-head-v2`.

`migrate-graph` already does exactly this shape for v1 → v2: open the old graph read-only
with a head shim by rebinding `*node-head-reader*`, snapshot it logically, create a fresh
graph, replay through `make-vertex` / `make-edge`. **Read it before writing anything.**

Three things to get right:

1. **Snapshots carry class *names*, not type-ids.** `backup` writes `(type-of v)`. So replay
   re-resolves names against the target's schema and the widening is transparent. Do not add
   type-id remapping; there is nothing to remap.
2. **Keep the schema copy.** v1 → v2 copies the old schema *"so type-ids are preserved"*, and
   that is correct here too — this unit does **not** renumber. (#186 is where the copy comes
   out.) Preserving ids means a v3 graph's type-ids match its v2 predecessor's exactly, which
   makes the migration verifiable by comparison.
3. **`OLD-LOCATION` stays byte-for-byte untouched** and `NEW-LOCATION` must be empty. That is
   the rollback story and it must not regress.

- [ ] **Step 1: Write the failing test**

**Model it on the existing migration test.** `tests/mvcc-tests.lisp:116` and `:175` already
exercise `migrate-graph` for v1 → v2 — read them first and follow their shape rather than
inventing one. Build a graph, snapshot it, migrate it, and assert every node survives with
its id, revision, type and slot values intact, and that the source directory is unchanged.

**Then prove the test is not vacuous:** make the migration skip the head-reader rebinding so
it reads v2 bytes with the v3 reader, confirm the test **FAILS**, restore byte-for-byte and
confirm it **PASSES**. Report both observations. Six tests in the sibling unit (#168) passed
while proving nothing; a green migration test is not evidence.

- [ ] **Step 2: Run, watch it fail.**
- [ ] **Step 3: Implement** the v2 → v3 path alongside the v1 → v2 one.
- [ ] **Step 4: Run, watch it pass**, then the full suite.
- [ ] **Step 5: Commit.**

---

### Task 4: Documentation

**Files:** `docs/vivace-graph-v3-doc.org`, `CHANGELOG.md`, and the spec's §10 status.

Docs travel with the code here and a `PreToolUse` hook enforces it on push.

Cover: that `type-id` is now 32 bits and what that means for the ceiling; that the type-index
no longer pre-allocates and what the lock-striping trade is; **the migration procedure**, and
prominently that **`migrate-graph` leaves the source directory untouched, so rollback is
repointing at the old directory rather than a restore** — unlike the packed-coordinate change
(#79), whose rollback needs snapshot replay. An operator reading this at the wrong hour needs
that sentence.

Note the deployment gate: production is below two engine floors already
(kraison/mine-action#117), so this migration lands on a host that needs a code bump first.

**Do not** claim ids are global. They are not; that is #186.

---

## Testing

Force ASDF to the worktree or you silently test the main checkout:

```lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(asdf:initialize-source-registry
 '(:source-registry
   (:directory "/home/raison/work/vivace-graph-v3/.worktrees/166-global-type-ids/")
   :inherit-configuration))
(dolist (s '(:graph-db :graph-db/test)) (asdf:clear-system s))
(let ((dir (namestring (asdf:system-source-directory (asdf:find-system :graph-db)))))
  (format t "~&SOURCE-DIR: ~a~%" dir)
  (unless (search "166-global-type-ids" dir)
    (format *error-output* "~&FATAL: wrong tree~%") (uiop:quit 2)))
```

Iterate on `type-id-width-suite` in the **foreground**. Background the full suite with the
Bash tool's `run_in_background` parameter — **never** `nohup` or a trailing `&`, which
detaches it so the harness cannot track it. **Never run two SBCL processes at once.**

## Self-Review

**Spec coverage.** §3.4's widening and sparse type-index are Tasks 1–2; §10's migration is
Task 3. §3.4's *global registry* is deliberately absent — that is #186.

**Deliberately out of scope:** globalisation, the canonical registry, distribution, the
handshake guard, and deleting `%check-node-class-graph-unique` (whose rationale still holds
while ids are per-graph).

**Type consistency.** `deserialize-node-head-v2` is defined in Task 1 and consumed in Task 3.
`+type-index-lock-stripes+` and `+type-index-initial-types+` are defined and consumed in
Task 2.

**Risk.** Task 1 changes the on-disk format; the suite's graphs are all fresh so it will pass
without Task 3 even though existing data would not be readable. That is expected within the
task boundary — do not let a green Task 1 suggest migration is unnecessary.
