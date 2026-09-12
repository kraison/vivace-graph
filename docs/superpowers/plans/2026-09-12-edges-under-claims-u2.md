# Edges under claims, U2 (#372, tracker #367) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The idempotent linking sweep `link-claim-endpoints` that backfills and repairs the derived `subject-of` / `object-of` edges U1 introduced, a write-time linking switch so bulk loads can defer to it, and the §9 measurement recorded as a perf bench.

**Architecture:** Two phases per call, because `resolve-endpoint` refuses to run inside a read-write transaction and a write transaction must not cross stores: a read phase under `with-read-snapshot` collects the family's claims (optionally at or above a commit epoch, optionally bounded), and for every endpoint that has no edge calls `resolve-endpoint`, handling its conditions into counts; then one short write transaction on the claim's graph creates the missing edges, re-checking each claim is still unlinked so two sweeps racing stay idempotent. The sweep never prunes (`active-edge-p` hides a dead endpoint's edge; `compact-edges` reclaims it) and never signals: its counts are the report. A special `*link-claims-at-write*` (default T) lets a bulk writer skip write-time linking and sweep afterwards, and is what the measurement toggles.

**Tech Stack:** SBCL, FiveAM, `graph-db/core`, `graph-db/spacetime`, `graph-db/perf-test` (gains a `graph-db/spacetime` dependency for the bench).

**Spec:** `docs/superpowers/specs/2026-09-12-edges-under-claims-design.md` at `79777fc` — §5 (the sweep), §9 (measurement), §11 (the sweep bullets), §13 U2, and the U1 Built note. Every `file:line` below was verified against `experiment` `79777fc` during planning (2026-09-12); match on the quoted forms, not the numbers. #368 (cross-store continuation) is not this plan.

## Global Constraints

- Lisp: spaces only, hard 80 columns on every line, terse comments naming `GH #372` or a spec section (invariant + why, one or two lines; detail lives in the issue); docstrings state what / returns / the one trap. No test-fixture names in production source.
- Branch `feat/edges-under-claims-u2`, worktree `/home/raison/work/vg-367` (clone `/home/raison/work/vg-c3`, on `experiment`). Never build in the clone.
- Never run `pkill`, `pgrep -f` or `kill`. One SBCL build at a time in this worktree. Never the full suite by hand; the suites below only. CI runs the full suite on push (`docs/ci.md`).
- No store format change. The sweep creates only `subject-of` / `object-of` edges, only in the claim's own graph, only from a claim to a node `resolve-endpoint` returned.
- **The sweep never signals** for what it could not do and **never prunes** (spec §5): `resolve-endpoint`'s conditions are counted, a failed edge construction is logged, a missing or deleted endpoint's edge is left for `compact-edges`.
- **Idempotent**: a second call on an unchanged store links nothing.
- Write-time linking stays the default: `*link-claims-at-write*` is T unless a caller binds it.
- Every new `graph-db.spacetime` symbol a test uses unqualified goes on `spacetime/package.lisp`'s export list; engine symbols are written `graph-db:` (or `graph-db::` for internals) in spacetime tests and in the perf bench.
- Existing tests keep passing; the baseline counts recorded in Task 1 never drop. The perf suite's generation is NOT bumped (adding a bench does not bump; `tests/perf/suite.lisp` docstring).
- Docs travel with the code (Task 6). Every commit message names `#372`.
- Commit trailers on every commit, fixed text (not the implementer's model name):
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01Dai2CTvMRE41v7RzAUnWmn
  ```

## Running the suites

Write once to `/home/raison/work/vg-c3-notes/vg-372-suites.lisp` (outside every checkout):

```lisp
;; The suites #372 touches, CI-style, in a fresh image (docs/ci.md).
(ql:quickload '(:graph-db/spacetime-test :graph-db/query-test) :silent t)
(format t "~&== graph-db loaded from ~a~%"
        (asdf:system-source-directory :graph-db))
(log:config :error)
(let ((ok t))
  (unless (graph-db/spacetime-test:run-spacetime-tests) (setf ok nil))
  (unless (graph-db/query-test:run-query-tests) (setf ok nil))
  (sb-ext:exit :code (if ok 0 1)))
```

Run from the worktree root, foreground, timeout 600000 ms:

```bash
cd /home/raison/work/vg-367
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-372-suites.lisp \
  > /home/raison/work/vg-c3-notes/vg-372-suites.log 2>&1; echo "exit=$?"
grep -E "loaded from|Did [0-9]+ checks|Fail:|^== " \
  /home/raison/work/vg-c3-notes/vg-372-suites.log
```

A single spacetime test while iterating (`run-spacetime-tests` binds the system directory itself; FiveAM's `run` does not, so bind it here):

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/spacetime-test :silent t)' \
  --eval '(in-package :graph-db/spacetime-test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

Always read back `Did N checks`. Fresh image after any export or ASDF change.

## Engine facts every task relies on

- **What U1 left in `spacetime/link.lisp`** (read the file first; ~130 lines): `%binary-claim-p`, `%identity-slot`, `%transaction-creates`, `%uncommitted-creates`, `%same-store-candidates`, `%verify-endpoint-node`, `%single-same-store-candidate`, `%link-endpoint (claim graph ctor namespace key given creates)` — creates one edge with `(funcall ctor :from claim :to (graph-db:id node) :graph graph)` under a `handler-case` that `log:warn`s and returns NIL — and `%link-claim-at-write (claim &key subject-node object-node)`. The sweep reuses none of the candidate machinery (it resolves through `resolve-endpoint`, which may cross stores) but follows the same never-signal rule.
- **The constructor wrapper** is emitted by `def-claim-classes` in `spacetime/claim.lisp` (the `mapcar` lambda beginning `(let ((%raw (fdefinition ',ctor)))`); its last form is `(%link-claim-at-write c :subject-node subject-node :object-node object-node)` and it returns the claim. No file inside `graph-db/spacetime` expands `def-claim-classes` at load time, so a variable defined in `claim.lisp` before the macro is defined before any expansion runs.
- `resolve-endpoint (namespace key)` (`spacetime/resolve.lisp:9`) returns the node or NIL; signals `unknown-namespace` (nothing registered), `unopened-source-graph` (a registered class's default graph is not open), `ambiguous-endpoint` (two records); signals `resolution-in-transaction` when `graph-db:*transaction*` is bound. Under `graph-db:with-read-snapshot` `*transaction*` is NOT bound (read snapshots live in `*read-snapshots*`), so resolution is legal there and may read another open store.
- `graph-db:with-read-snapshot ((&optional (graph '*graph*)) &body)` (`transactions.lisp:3527`) pins reads of GRAPH to one MVCC snapshot.
- `graph-db:map-vertices (fn graph &key vertex-type include-subclasses-p ...)` (`vertex.lisp:203`): with `:vertex-type PARENT` and the default `include-subclasses-p t` it visits both arity subclasses. A parent class that is not instantiated in GRAPH: check first with `(graph-db:lookup-node-type-by-name parent :vertex :graph graph)` → NIL means no claims of that family live here.
- `claim-commit-epoch (claim)` (`spacetime/claim-query.lisp:140`) → the committing transaction's id, or NIL for a reaped claim or one not yet committed. Within one store it is monotonic per commit, so `:since` compares it directly; no clock is needed (a clock only makes epochs comparable ACROSS stores, GH #347).
- Adjacency is indexed at commit apply in the EDGE's store (`edge.lisp:168-174`, `transactions.lisp:987-996`); `graph-db:outgoing-edges (vertex &key graph edge-type include-deleted-p ...)` (`edge.lisp:565`) filters through `active-edge-p` unless `:include-deleted-p t`. So "has no edge" = `(null (graph-db:outgoing-edges claim :graph graph :edge-type type))`, and an edge whose endpoint was soft-deleted is invisible to that call but visible with `:include-deleted-p t`.
- `graph-db::compact-edges (graph &key (policy :conservative))` (`edge.lisp:628`; NOT exported) deletes and de-indexes edges whose endpoint is `:missing` or `:found`-and-deleted, never `:detached`. After it, even `:include-deleted-p t` no longer lists the edge (it is removed from the ve-index).
- A write transaction on a specific graph: `(graph-db:with-transaction ((graph-db::transaction-manager graph)) ...)` (`transaction-manager` is NOT exported; the spacetime tests already write it that way).
- Generated edge constructors accept `:from` / `:to` as vertices OR id arrays (`make-edge`, `edge.lisp:227-235`); pass ids from the sweep, since the claim objects were read under a snapshot that has ended.
- `graph-db:mark-deleted (node)` inside a transaction soft-deletes a node (used in `tests/spacetime/vocabulary-tests.lisp:82`).
- Detaching a store in tests: `(close-graph sg :snapshot-p nil)`; `resolve-endpoint` then signals `unopened-source-graph` for that store's classes (`spacetime/resolve.lisp:44-47`); `with-source-graph`'s cleanup tolerates a second close (`ignore-errors`).
- Perf suite (`tests/perf/`): `run-perf` (`benchmarks.lisp:964`) binds `*system-directory*`, `*type-registry*` and `*system-clock*`, then calls each `bench-*` in a fixed list, then `write-perf-report`; helpers `timed-ops ((label ops-form) &body)`, `timed-seconds ((label) &body)`, `record (label &rest plist)`, `scale (normal &optional small)`, `with-temp-directory`, `collect-garbage`, `make-temp-directory` (`suite.lisp`). `with-perf-graph` opens the shared perf schema's graph `:graph-db-perf-test`; a bench that needs its own schema uses its own graph name so it does not alter existing benches' work (the generation rule). `graph-db/perf-test` depends on `:graph-db` (which pulls `graph-db/query`, so `graph-db.query:run-guarded-prolog` is loadable there), `:graph-db/test-scratch`, `:bordeaux-threads` (`graph-db.asd:412-427`); its package uses only `cl` and imports named `graph-db` symbols (`tests/perf/package.lisp`).
- Tests reuse from `tests/spacetime/endpoint-edge-tests.lisp` (loads before the new file): `*ee-graph-name*`, `with-ee-graph`, `ee-thing (id)`, `ee-b (&key subject object relation producer subject-namespace object-namespace subject-node object-node)`, `same-node-p`, `ee-linked-to (claim edge-type g)`, `make-ee-twin`, `with-source-graph` / `make-st-report` (from `source-tests.lisp`), `claim-endpoints`, `node-claims`.
- Docs anchors: manual `docs/vivace-graph-v3-doc.org` section `*** Edges under claims: derived adjacency to the endpoints (GH #369)` (line ~5912), whose last paragraph ends "…which lands with GH #367 U2."; `CHANGELOG.md` `## [Unreleased]` → `### Added`, first bullet is "**Edges under claims** (#367, U1 #369)"; spec `## Built — U1 (#369, 8c7674e)` precedes `## 14. Traceability`.

## File structure

- `spacetime/claim.lisp` — modify: `(defvar *link-claims-at-write* t)` before `def-claim-classes`; the wrapper consults it.
- `spacetime/link.lisp` — modify: the sweep — `%family-parents-in`, `%sweep-collect`, `%sweep-resolve`, `%sweep-write`, `link-claim-endpoints`.
- `spacetime/package.lisp` — modify: export `*link-claims-at-write*`, `link-claim-endpoints`.
- `tests/spacetime/link-sweep-tests.lisp` — create; `graph-db.asd` spacetime-test components: after `endpoint-edge-tests`.
- `tests/perf/spacetime-bench.lisp` — create: `bench-claim-linking`; `graph-db.asd` perf-test: `:graph-db/spacetime` dependency, the component, and the call in `run-perf`; `tests/perf/package.lisp` unchanged (the bench writes `graph-db.spacetime:` qualified).
- Docs: manual section (replace the last sentence, add the sweep and the switch), CHANGELOG bullet, spec Built note U2, `tests/perf/README.md` one line.

---

### Task 1: Baseline

**Files:**
- Create: `/home/raison/work/vg-c3-notes/vg-372-suites.lisp` (the runner above)

- [ ] **Step 1: Run the runner on the unchanged branch (79777fc)**

Expected: `exit=0`, `graph-db loaded from /home/raison/work/vg-367/`, spacetime `Did 872 checks.` / `Fail: 0`, query `Did 54 checks.` / `Fail: 0`.

- [ ] **Step 2: Record both counts** in `/home/raison/work/vg-c3-notes/vg-372-sdd-ledger.md` (create it: heading `# vg-372 SDD ledger`, section `## Baseline (79777fc)`, the grep lines verbatim). No later run may report fewer for either suite.

---

### Task 2: The write-time switch

**Files:**
- Modify: `spacetime/claim.lisp` (insert before `(defmacro def-claim-classes`; change the wrapper's last form)
- Modify: `spacetime/package.lisp` (export list)
- Modify: `graph-db.asd` (`graph-db/spacetime-test` components)
- Create: `tests/spacetime/link-sweep-tests.lisp`

**Interfaces:**
- Produces: `graph-db.spacetime:*link-claims-at-write*` (special, default T). When NIL, `make-<family>-unary/-binary` build the claim and skip `%link-claim-at-write`; `:subject-node` / `:object-node` are still stripped from the raw constructor's arguments but NOT verified (nothing is linked).
- Produces: the test file with `in-suite spacetime-suite` and the first test; later tasks append to it.

- [ ] **Step 1: Write the failing test**

Create `tests/spacetime/link-sweep-tests.lisp`:

```lisp
;;;; The linking sweep and the write-time switch (GH #372, spec sec.5,
;;;; sec.11).  Fixtures come from endpoint-edge-tests.lisp, which loads
;;;; first (graph-db.asd): WITH-EE-GRAPH, EE-THING, EE-B, EE-LINKED-TO,
;;;; SAME-NODE-P, MAKE-EE-TWIN; WITH-SOURCE-GRAPH / MAKE-ST-REPORT from
;;;; source-tests.lisp.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test the-write-time-switch-defers-linking
  "Spec sec.9's knob: with *LINK-CLAIMS-AT-WRITE* NIL a claim about
present sources is written key-only, and no edge type is adopted."
  (is-true *link-claims-at-write*)
  (with-ee-graph (g)
    (let (c)
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (let ((*link-claims-at-write* nil))
        (with-transaction () (setq c (ee-b))))
      (is (null (ee-linked-to c 'subject-of g)))
      (is (null (ee-linked-to c 'object-of g)))
      (is (= 1 (length (claims-touching g 'ee-claim :ee-things "t-1"))))
      ;; The default is untouched outside the binding: a second claim
      ;; links as U1 does.
      (let ((c2 (with-transaction () (ee-b :relation "r2"))))
        (is-true (ee-linked-to c2 'subject-of g))))))
```

- [ ] **Step 2: Wire the file into ASDF**

In `graph-db.asd`, `graph-db/spacetime-test` components, change the last entry

```lisp
               (:file "endpoint-edge-tests"))            ; GH #369
```
to
```lisp
               (:file "endpoint-edge-tests")             ; GH #369
               (:file "link-sweep-tests"))               ; GH #372
```

- [ ] **Step 3: Run the test to verify it fails**

Single-test command with `the-write-time-switch-defers-linking`. Expected: a failure naming `*link-claims-at-write*` (unbound variable or reader error on the unexported symbol).

- [ ] **Step 4: Define the switch and consult it**

In `spacetime/claim.lisp`, immediately after the two `def-edge` forms (before `(defmacro def-claim-classes`), insert:

```lisp
(defvar *link-claims-at-write* t
  "When true (the default) every MAKE-<claim> links its endpoints in the
same transaction (spec sec.4).  Bind to NIL around a bulk load to write
claims key-only and LINK-CLAIM-ENDPOINTS afterwards; the sec.9
measurement toggles it (GH #372).  Trap: :SUBJECT-NODE / :OBJECT-NODE
are then ignored, not verified.")
```

In the wrapper lambda, replace

```lisp
                             (check-standing (claim-standing c))
                             ;; Derived edges, in the same transaction
                             ;; (GH #369, spec sec.4.1).
                             (%link-claim-at-write
                              c :subject-node subject-node
                                :object-node object-node))))))))
```
with
```lisp
                             (check-standing (claim-standing c))
                             ;; Derived edges, in the same transaction
                             ;; (GH #369, spec sec.4.1); a bulk loader
                             ;; may defer them to the sweep (GH #372).
                             (if *link-claims-at-write*
                                 (%link-claim-at-write
                                  c :subject-node subject-node
                                    :object-node object-node)
                                 c))))))))
```

- [ ] **Step 5: Export**

In `spacetime/package.lisp`, after the `;; edges under claims (GH #369)` block's last line (`#:related/3 #:claimed/4`), add:

```lisp
   ;; the linking sweep (GH #372)
   #:*link-claims-at-write* #:link-claim-endpoints
```

- [ ] **Step 6: Run the test to verify it passes**

Fresh image (export + ASDF change). Expected: PASS, 5 checks.

- [ ] **Step 7: Run the runner.** Expected: spacetime 877 checks, `Fail: 0`; query 54.

- [ ] **Step 8: Commit**

```bash
git add spacetime/claim.lisp spacetime/package.lisp graph-db.asd \
        tests/spacetime/link-sweep-tests.lisp
git commit -m "feat(spacetime): *LINK-CLAIMS-AT-WRITE* -- defer write-time linking to the sweep (#372)"
```

---

### Task 3: The sweep

**Files:**
- Modify: `spacetime/link.lisp` (append)
- Test: `tests/spacetime/link-sweep-tests.lisp` (append)

**Interfaces:**
- Consumes: `*link-claims-at-write*` (Task 2); U1's `make-subject-of` / `make-object-of`, `%binary-claim-p`; `resolve-endpoint`, `claim-commit-epoch`, `*claim-families*` / `claim-family-parent`.
- Produces: `(link-claim-endpoints graph &key family since limit) => (values linked unresolved ambiguous skipped-namespaces more-p)`.
  - `linked`: edges created and committed by this call.
  - `unresolved`: endpoints `resolve-endpoint` answered NIL for.
  - `ambiguous`: endpoints that signalled `ambiguous-endpoint`.
  - `skipped-namespaces`: a list of namespace keywords skipped for the whole call after `unknown-namespace` or `unopened-source-graph`.
  - `more-p`: T when `:limit` stopped the visit while further claims with a missing edge remained.
  - Internal helpers `%family-parents-in (graph family)`, `%sweep-collect (graph parents since limit)`, `%sweep-resolve (graph claims)`, `%sweep-write (graph plan)`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spacetime/link-sweep-tests.lisp`:

```lisp
(defun ee-sweep (g &rest keys)
  "LINK-CLAIM-ENDPOINTS on G; the five values as a list."
  (multiple-value-list (apply #'link-claim-endpoints g keys)))

(defun ee-unlinked-b (&rest keys)
  "A binary claim written with linking OFF, inside its own transaction."
  (let ((*link-claims-at-write* nil))
    (with-transaction () (apply #'ee-b keys))))

(test sweep-links-a-claim-written-before-its-source
  "Spec sec.5: the claim came first; the sweep repairs it."
  (with-ee-graph (g)
    (let (c s o)
      (with-transaction () (setq c (ee-b)))
      (is (null (ee-linked-to c 'subject-of g)))
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (is (equal '(2 0 0 nil nil) (ee-sweep g)))
      (multiple-value-bind (cs co) (claim-endpoints c)
        (is-true (same-node-p s cs))
        (is-true (same-node-p o co))))))

(test sweep-is-idempotent
  (with-ee-graph (g)
    (with-transaction () (ee-b))
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    (is (equal '(2 0 0 nil nil) (ee-sweep g)))
    (is (equal '(0 0 0 nil nil) (ee-sweep g)))
    ;; A claim linked at write is not touched either.
    (with-transaction () (ee-b :relation "r2"))
    (is (equal '(0 0 0 nil nil) (ee-sweep g)))))

(test sweep-counts-every-category-and-never-signals
  "Spec sec.5 / sec.7: unknown namespace -> skipped for the call; no
such key -> unresolved; two candidates -> ambiguous; the claim stays
as it was in every case."
  (with-ee-graph (g)
    (with-transaction () (ee-thing "t-1"))
    (with-transaction ()
      (ee-thing "dup")
      (make-ee-twin :twin-id "dup"))
    (let (k1 k2 k3)
      ;; object in an unregistered namespace (subject t-1 links)
      (with-transaction ()
        (setq k1 (ee-b :object-namespace :ee-nowhere :object "x")))
      ;; object key nobody has
      (with-transaction () (setq k2 (ee-b :object "t-missing")))
      ;; subject with two candidates, written unlinked (warning muffled)
      (handler-bind ((endpoint-link-skipped #'muffle-warning))
        (with-transaction () (setq k3 (ee-b :subject "dup" :object "t-1"))))
      (destructuring-bind (linked unresolved ambiguous skipped more)
          (ee-sweep g)
        (is (= 0 linked))
        (is (= 1 unresolved))
        (is (= 1 ambiguous))
        (is (equal '(:ee-nowhere) skipped))
        (is (null more)))
      (is (null (ee-linked-to k1 'object-of g)))
      (is (null (ee-linked-to k2 'object-of g)))
      (is (null (ee-linked-to k3 'subject-of g)))
      (is-true (ee-linked-to k3 'object-of g))
      ;; Idempotent on the same failures.
      (is (equal '(0 1 1 (:ee-nowhere) nil) (ee-sweep g))))))

(test sweep-honours-since
  "Spec sec.5: :SINCE is a commit epoch; only claims at or above it are
visited, so an operator can sweep exactly what a regeneration wrote."
  (with-ee-graph (g)
    (let (a b)
      (setq a (ee-unlinked-b :relation "a"))
      (setq b (ee-unlinked-b :relation "b"))
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (let ((eb (claim-commit-epoch b)))
        (is-true (> eb (claim-commit-epoch a)))
        (is (equal '(2 0 0 nil nil) (ee-sweep g :since eb)))
        (is-true (ee-linked-to b 'subject-of g))
        (is (null (ee-linked-to a 'subject-of g)))
        (is (equal '(2 0 0 nil nil) (ee-sweep g)))
        (is-true (ee-linked-to a 'subject-of g))))))

(test sweep-family-restricts-and-an-absent-family-is-nothing
  (with-ee-graph (g)
    (ee-unlinked-b)
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    ;; CT-CLAIM is registered (claim-tests.lisp) but not in this store.
    (is (equal '(0 0 0 nil nil) (ee-sweep g :family 'ct-claim)))
    (is (equal '(2 0 0 nil nil) (ee-sweep g :family 'ee-claim)))
    (signals unknown-claim-family (link-claim-endpoints g :family 'nope))))

(test sweep-limit-bounds-the-work-and-reports-more
  "LIMIT bounds claims with a missing edge examined per call; MORE-P
says whether further such claims remained."
  (with-ee-graph (g)
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    (dolist (r '("r1" "r2" "r3")) (ee-unlinked-b :relation r))
    (destructuring-bind (linked u a s more) (ee-sweep g :limit 2)
      (declare (ignore u a s))
      (is (= 4 linked))
      (is-true more))
    (destructuring-bind (linked u a s more) (ee-sweep g :limit 2)
      (declare (ignore u a s))
      (is (= 2 linked))
      (is (null more)))))
```

- [ ] **Step 2: Run one to verify it fails**

`sweep-links-a-claim-written-before-its-source` — expected: undefined function `link-claim-endpoints`.

- [ ] **Step 3: Write the sweep**

Append to `spacetime/link.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; The sweep (GH #372, spec sec.5): read under a snapshot, resolve
;;; through RESOLVE-ENDPOINT (which may cross stores and refuses a
;;; write transaction), then one short write transaction.  Never prunes,
;;; never signals: the counts are the report.
;;; ---------------------------------------------------------------------------

(defun %family-parents-in (graph family)
  "Parent class names whose family has claims in GRAPH: FAMILY's alone
(UNKNOWN-CLAIM-FAMILY if unregistered), else every registered family
whose parent type is instantiated in GRAPH."
  (if family
      (let ((parent (claim-family-parent (claim-family family))))
        (when (graph-db:lookup-node-type-by-name parent :vertex :graph graph)
          (list parent)))
      (let ((parents '()))
        (maphash (lambda (parent fam)
                   (declare (ignore fam))
                   (when (graph-db:lookup-node-type-by-name
                          parent :vertex :graph graph)
                     (push parent parents)))
                 *claim-families*)
        parents)))

(defun %missing-endpoints (claim graph)
  "The (CTOR TYPE NAMESPACE KEY) entries of CLAIM's endpoints that have
no edge in GRAPH; a unary claim has at most one."
  (let ((out '()))
    (unless (graph-db:outgoing-edges claim :graph graph
                                           :edge-type 'subject-of)
      (push (list #'make-subject-of 'subject-of
                  (claim-subject-namespace claim) (claim-subject-key claim))
            out))
    (when (and (%binary-claim-p claim)
               (null (graph-db:outgoing-edges claim :graph graph
                                                    :edge-type 'object-of)))
      (push (list #'make-object-of 'object-of
                  (claim-object-namespace claim) (claim-object-key claim))
            out))
    (nreverse out)))

(defun %sweep-collect (graph parents since limit)
  "Claims of PARENTS in GRAPH with a missing edge, at or above commit
epoch SINCE, at most LIMIT of them: (VALUES ((CLAIM . MISSING)...) MORE-P).
Runs inside the caller's read snapshot."
  (let ((work '()) (n 0) (more nil))
    (dolist (parent parents)
      (graph-db:map-vertices
       (lambda (c)
         (when (or (null since)
                   (let ((e (claim-commit-epoch c))) (and e (>= e since))))
           (let ((missing (%missing-endpoints c graph)))
             (when missing
               (if (and limit (>= n limit))
                   (setf more t)
                   (progn (push (cons c missing) work) (incf n)))))))
       graph :vertex-type parent))
    (values (nreverse work) more)))

(defun %sweep-resolve (work)
  "Resolve every missing endpoint in WORK: (VALUES PLAN UNRESOLVED
AMBIGUOUS SKIPPED), PLAN a list of (CLAIM-ID CTOR TYPE NODE-ID).  A
namespace that signals UNKNOWN-NAMESPACE or UNOPENED-SOURCE-GRAPH is
skipped for the rest of the call (spec sec.5 step 1)."
  (let ((plan '()) (unresolved 0) (ambiguous 0) (skipped '()))
    (dolist (entry work)
      (destructuring-bind (claim . missing) entry
        (dolist (m missing)
          (destructuring-bind (ctor type namespace key) m
            (unless (member namespace skipped)
              (handler-case
                  (let ((node (resolve-endpoint namespace key)))
                    (if node
                        (push (list (graph-db:id claim) ctor type
                                    (graph-db:id node))
                              plan)
                        (incf unresolved)))
                ((or unknown-namespace unopened-source-graph) ()
                  (push namespace skipped))
                (ambiguous-endpoint () (incf ambiguous))))))))
    (values (nreverse plan) unresolved ambiguous (nreverse skipped))))

(defun %sweep-write (graph plan)
  "Create PLAN's edges in one transaction on GRAPH; the number committed.
Each claim is re-checked as still present and still unlinked for that
edge type, so two sweeps racing, or a sweep racing a write-time link,
stay idempotent.  A construction failure is logged, never signalled."
  (if (null plan)
      0
      (let ((n 0))
        (handler-case
            (graph-db:with-transaction ((graph-db::transaction-manager graph))
              (dolist (step plan)
                (destructuring-bind (claim-id ctor type node-id) step
                  (let ((c (graph-db:lookup-vertex claim-id :graph graph)))
                    (when (and c
                               (null (graph-db:outgoing-edges
                                      c :graph graph :edge-type type)))
                      (funcall ctor :from claim-id :to node-id :graph graph)
                      (incf n))))))
          (error (c)
            (log:warn "GH #372: sweep on ~A wrote nothing: ~A"
                      (graph-db:graph-name graph) c)
            (setf n 0)))
        n)))

(defun link-claim-endpoints (graph &key family since limit)
  "Link every claim in GRAPH whose endpoint now resolves: the idempotent
sweep and backfill (GH #372, spec sec.5).  (VALUES LINKED UNRESOLVED
AMBIGUOUS SKIPPED-NAMESPACES MORE-P): edges committed; endpoints with no
node; endpoints with several; namespaces skipped for the whole call
(unregistered, or a source store not open); whether LIMIT stopped the
visit with work remaining.

FAMILY is one parent class name (default: every family with claims in
GRAPH).  SINCE is a commit epoch (CLAIM-COMMIT-EPOCH): only claims at or
above it are visited, so a regeneration's writes can be swept alone.
LIMIT bounds the claims WITH A MISSING EDGE examined in one call; loop
while MORE-P, noting that endpoints that stay unresolved are examined
again each call.

Never prunes -- ACTIVE-EDGE-P hides a dead endpoint's edge and
COMPACT-EDGES reclaims it -- and never signals for what it could not do.
Trap: must not be called inside a read-write transaction
(RESOLUTION-IN-TRANSACTION); run it between transactions."
  (let ((parents (%family-parents-in graph family)))
    (if (null parents)
        (values 0 0 0 '() nil)
        (multiple-value-bind (work more)
            (graph-db:with-read-snapshot (graph)
              (%sweep-collect graph parents since limit))
          (multiple-value-bind (plan unresolved ambiguous skipped)
              (graph-db:with-read-snapshot (graph)
                (%sweep-resolve work))
            (values (%sweep-write graph plan)
                    unresolved ambiguous skipped more))))))
```

Note on the two snapshots: collection and resolution are separate `with-read-snapshot` forms so `resolve-endpoint`'s cross-store reads happen with GRAPH's pin held (the composition the namespaces design §6 describes) while the claim objects are not carried across; only ids reach the write phase.

- [ ] **Step 4: Run the six tests**

Each individually. Expected: all PASS. If `sweep-honours-since` fails on the epoch comparison, print `(claim-commit-epoch a)` / `(claim-commit-epoch b)` in the test to confirm both are non-NIL integers before assuming a sweep defect.

- [ ] **Step 5: Run the runner.** Expected: spacetime `Fail: 0`, more than 877 checks; query 54.

- [ ] **Step 6: Commit**

```bash
git add spacetime/link.lisp tests/spacetime/link-sweep-tests.lisp
git commit -m "feat(spacetime): LINK-CLAIM-ENDPOINTS -- the idempotent linking sweep and backfill (#372)"
```

---

### Task 4: No prune, detached stores, compaction — pinned

**Files:**
- Test: `tests/spacetime/link-sweep-tests.lisp` (append)

No engine change expected; these pin spec §5's "never prunes" and §11's sweep bullets against the engine's real behaviour. If a pin fails, the sweep (Task 3) is what changes — not the assertion, unless the engine fact in the plan is wrong, in which case report DONE_WITH_CONCERNS with the observed value.

- [ ] **Step 1: Write the tests**

```lisp
(test sweep-does-not-prune-a-dead-endpoints-edge
  "Spec sec.5: ACTIVE-EDGE-P hides the edge, COMPACT-EDGES reclaims it,
the sweep leaves it alone and does not re-link a deleted source."
  (with-ee-graph (g)
    (let (c o)
      (with-transaction () (ee-thing "t-1") (setq o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (is-true (ee-linked-to c 'object-of g))
      (with-transaction () (graph-db:mark-deleted o))
      ;; Hidden from the active read, present to the raw one.
      (is (null (ee-linked-to c 'object-of g)))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'object-of
                        :include-deleted-p t))))
      (is (null (nth-value 1 (claim-endpoints c))))
      ;; The sweep sees a missing object edge, resolves nobody, prunes
      ;; nothing.
      (is (equal '(0 1 0 nil nil) (ee-sweep g)))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'object-of
                        :include-deleted-p t))))
      ;; Reclaiming is COMPACT-EDGES' job.
      (graph-db::compact-edges g)
      (is (null (graph-db:outgoing-edges c :graph g :edge-type 'object-of
                                            :include-deleted-p t)))
      (is-true (ee-linked-to c 'subject-of g)))))

(test sweep-skips-a-detached-source-store-and-keeps-its-edges
  "Spec sec.5: a namespace whose source store is not open is skipped and
counted; an existing cross-store edge survives."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n c1 c2)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            ;; linked across stores by the caller
            (setq c1 (ee-b :object-namespace :st-reports :object "r-1"
                           :object-node n))
            ;; same endpoint, left for the sweep
            (setq c2 (ee-b :object-namespace :st-reports :object "r-1"
                           :relation "later"))))
        (is-true (ee-linked-to c1 'object-of g))
        (is (null (ee-linked-to c2 'object-of g)))
        ;; Open: the sweep resolves across stores and links C2.
        (is (equal '(1 0 0 nil nil) (ee-sweep g)))
        (is (equalp (id n) (ee-linked-to c2 'object-of g)))
        ;; Detached: a third claim cannot be resolved; the namespace is
        ;; skipped, the two existing edges survive.
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-b :object-namespace :st-reports :object "r-1"
                  :relation "latest")))
        (close-graph sg :snapshot-p nil)
        (is (equal '(0 0 0 (:st-reports) nil) (ee-sweep g)))
        (is-true (ee-linked-to c1 'object-of g))
        (is-true (ee-linked-to c2 'object-of g))))))
```

The third claim in the second test is written while `sg` is still open, with the object in `:st-reports`; write-time linking is same-store only, so it stays unlinked and gives the detached sweep something to skip.

- [ ] **Step 2: Run both.** Expected: PASS.

If `(0 1 0 nil nil)` in the first test comes back as `(0 0 0 nil nil)`, the engine treats a soft-deleted source's index entry as gone before the sweep resolves it — then `resolve-endpoint` returned NIL and `unresolved` should be 1; check that `index-lookup` skips deleted nodes (`index.lisp:994`, `(not (deleted-p node))`) and report what you saw.

- [ ] **Step 3: Run the runner.** Expected: `Fail: 0`, counts up; query 54.

- [ ] **Step 4: Commit**

```bash
git add tests/spacetime/link-sweep-tests.lisp
git commit -m "test(spacetime): the sweep never prunes; detached source stores are skipped and their edges kept (#372)"
```

---

### Task 5: The §9 measurement as a perf bench

**Files:**
- Create: `tests/perf/spacetime-bench.lisp`
- Modify: `graph-db.asd` (`graph-db/perf-test`: `:depends-on`, components), `tests/perf/benchmarks.lisp` (`run-perf`'s bench list), `tests/perf/README.md` (one line)
- Create (outside the repo): `/home/raison/work/vg-c3-notes/vg-372-measure.lisp`

**Interfaces:**
- Produces: `graph-db/perf-test::bench-claim-linking`, recording labels `claim-writes-unlinked`, `claim-writes-linked` (ops/s), `claim-sweep-backfill` (seconds, plus `:linked` count), `two-hop-related` and `two-hop-claims-touching` (ops/s over the same K sources) and `two-hop-speedup` (`:ratio`, claims-touching time ÷ related time; > 1 means the edge path is faster).

- [ ] **Step 1: Write the bench**

Create `tests/perf/spacetime-bench.lisp`:

```lisp
;;;; Edges under claims: the spec sec.9 measurement (GH #372).  Own graph
;;;; name and own schema so no existing bench's work changes (the
;;;; generation rule, suite.lisp).  Reported, not gated: the numbers go
;;;; to the unit's issue.

(in-package #:graph-db/perf-test)

(defparameter *perf-claim-graph-name* :graph-db-perf-claims)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-claim-graph-name* *schema-node-metadata*) nil))

(graph-db.spacetime:def-source pb-thing :graph-db-perf-claims
    ((thing-id :initarg :thing-id :accessor pb-thing-id))
  :identity     (:namespace :pb-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "perf fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(graph-db.spacetime:def-claim-classes pb-claim :graph-db-perf-claims)

(defmacro with-perf-claim-graph ((g) &body body)
  "A fresh on-disk graph holding PB-THING sources and PB-CLAIM claims."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-claim-graph-name* (namestring ,d)
                             :buffer-pool-size 4000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun pb-key (i) (format nil "t-~D" i))

(defun pb-insert-things (n &key (batch 500))
  (let ((i 0))
    (loop while (< i n) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- n i)))
          (make-pb-thing :thing-id (pb-key i))
          (incf i))))))

(defun pb-insert-claims (m n &key (batch 500))
  "M binary claims over N things: claim j relates thing j mod N to
thing (j*7+1) mod N with one of three relations."
  (let ((j 0))
    (loop while (< j m) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- m j)))
          (make-pb-claim-binary
           :subject-namespace :pb-things :subject-key (pb-key (mod j n))
           :object-namespace :pb-things
           :object-key (pb-key (mod (1+ (* 7 j)) n))
           :relation (aref #("likes" "knows" "cites") (mod j 3))
           :producer "perf" :standing :inferred)
          (incf j))))))

(defun pb-two-hop-related (g key)
  "Objects two current hops from KEY through RELATED/3, under the
guarded runner (the consumer's read path)."
  (length
   (nth-value 1
     (graph-db.query:run-guarded-prolog
      (format nil "(is-a ?s pb-thing) (node-slot-value ?s thing-id ~S)
                   (related ?s ?r ?o) (related ?o ?r2 ?o2)" key)
      g :format :raw :limit 500))))

(defun pb-two-hop-touching (g key)
  "The same neighbourhood through CLAIMS-TOUCHING plus RESOLVE-ENDPOINT
per hop -- the first implementation's read path."
  (let ((count 0))
    (dolist (c (graph-db.spacetime:claims-touching
                g 'pb-claim :pb-things key :role :subject :current t))
      (let ((o (graph-db.spacetime:resolve-endpoint
                (graph-db.spacetime:claim-object-namespace c)
                (graph-db.spacetime:claim-object-key c))))
        (when o
          (dolist (c2 (graph-db.spacetime:claims-touching
                       g 'pb-claim :pb-things (pb-thing-id o)
                       :role :subject :current t))
            (when (graph-db.spacetime:resolve-endpoint
                   (graph-db.spacetime:claim-object-namespace c2)
                   (graph-db.spacetime:claim-object-key c2))
              (incf count))))))
    count))

(defun bench-claim-linking ()
  "Spec sec.9 (GH #372): claim write cost with linking off vs on, the
backfill sweep's cost, and a two-hop read through RELATED/3 versus
CLAIMS-TOUCHING + RESOLVE-ENDPOINT.  Same N/M/K on both sides."
  (let ((n (scale 2000)) (m (scale 4000)) (k (scale 200)))
    ;; 1. write cost, linking OFF, then the backfill on that store
    (with-perf-claim-graph (g)
      (pb-insert-things n)
      (let ((graph-db.spacetime:*link-claims-at-write* nil))
        (timed-ops ("claim-writes-unlinked" m)
          (pb-insert-claims m n)))
      (let (linked)
        (timed-seconds ("claim-sweep-backfill")
          (setf linked (graph-db.spacetime:link-claim-endpoints g)))
        (record "claim-sweep-backfill-linked" :edges linked)))
    ;; 2. write cost, linking ON, then the two read paths on that store
    (with-perf-claim-graph (g)
      (pb-insert-things n)
      (timed-ops ("claim-writes-linked" m)
        (pb-insert-claims m n))
      (let ((keys (loop for i below k collect (pb-key (* i 3))))
            t-related t-touching)
        (setf t-related
              (timed-ops ("two-hop-related" k)
                (dolist (key keys) (pb-two-hop-related g key))))
        (setf t-touching
              (timed-ops ("two-hop-claims-touching" k)
                (dolist (key keys) (pb-two-hop-touching g key))))
        (record "two-hop-speedup"
                :ratio (if (zerop t-related)
                           0
                           (float-3 (/ t-touching t-related))))))))
```

`timed-ops` returns the elapsed seconds (see its expansion in `suite.lisp`); the ratio uses those two values. `make-pb-thing` / `make-pb-claim-binary` / `pb-thing-id` are generated into this package by the two macros.

- [ ] **Step 2: Wire it**

`graph-db.asd`, `graph-db/perf-test`:

```lisp
  :depends-on (:graph-db :graph-db/spacetime :graph-db/test-scratch
               :bordeaux-threads)
```
and add after `(:file "bplus-bench")`:
```lisp
               ;; edges under claims, spec sec.9 (GH #372)
               (:file "spacetime-bench")
```
(fix the closing parens). In `tests/perf/benchmarks.lisp`, `run-perf`, after `(bench-vector-search)` add `(bench-claim-linking)`. In `tests/perf/README.md`'s "What lives here" list add: `- spacetime-bench.lisp — edges-under-claims write/sweep/read measurement (GH #372).`

- [ ] **Step 3: Write the measurement driver**

Create `/home/raison/work/vg-c3-notes/vg-372-measure.lisp`:

```lisp
;; The sec.9 measurement alone, at :normal scale, with run-perf's
;; bindings (GH #372).  Prints the records; nothing is gated.
(ql:quickload :graph-db/perf-test :silent t)
(in-package :graph-db/perf-test)
(let* ((*perf-scale* :normal)
       (system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (graph-db:*system-clock* nil))
  (reset-perf-report)
  (unwind-protect (bench-claim-linking)
    (graph-db-test-scratch:cleanup-scratch-run)))
(sb-ext:exit :code 0)
```

- [ ] **Step 4: Run it**

```bash
cd /home/raison/work/vg-367
sbcl --dynamic-space-size 8192 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-372-measure.lisp \
  > /home/raison/work/vg-c3-notes/vg-372-measure.log 2>&1; echo "exit=$?"
grep -E "claim-|two-hop" /home/raison/work/vg-c3-notes/vg-372-measure.log
```

Foreground, timeout 600000 ms. Expected: `exit=0` and six record lines. Run it twice; report both. If `two-hop-speedup`'s ratio is below 1, that is a RESULT to report, not a failure to fix — spec §9 says reported, not gated, but names "the edge path is faster" as the acceptance to record.

- [ ] **Step 5: Confirm the perf suite still loads**

```bash
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/perf-test :silent t)' \
  --eval '(princ (fboundp (quote graph-db/perf-test::bench-claim-linking)))' 2>&1 | tail -3
```
Expected: `T`, no warnings. Do NOT run `run-perf` (the whole suite is long).

- [ ] **Step 6: Record the numbers**

Append to `/home/raison/work/vg-c3-notes/vg-372-sdd-ledger.md` a `## Measurement (sec.9)` section with both runs' six lines, the host (`hostname`), and the commit SHA. The controller posts them to #372.

- [ ] **Step 7: Commit**

```bash
git add tests/perf/spacetime-bench.lisp tests/perf/benchmarks.lisp \
        tests/perf/README.md graph-db.asd
git commit -m "perf(spacetime): bench-claim-linking -- write cost, backfill sweep, two-hop read via RELATED/3 vs CLAIMS-TOUCHING (#372)"
```

---

### Task 6: Docs travel with the code

**Files:**
- Modify: `docs/vivace-graph-v3-doc.org` (the `*** Edges under claims` section's last paragraph)
- Modify: `CHANGELOG.md` (`## [Unreleased]` → `### Added`, a new bullet after the "Edges under claims" bullet)
- Modify: `docs/superpowers/specs/2026-09-12-edges-under-claims-design.md` (Built note U2, before `## 14. Traceability`)

- [ ] **Step 1: Manual**

In the "Edges under claims" section, replace the sentence beginning "Backfilling a store written before this landed is the sweep" (through "lands with GH #367 U2.") with:

```org
Backfilling a store written before edges existed, or repairing claims
whose sources arrived later, is the sweep:

#+BEGIN_SRC lisp
(link-claim-endpoints graph &key family since limit)
;; => (values linked unresolved ambiguous skipped-namespaces more-p)
#+END_SRC

It reads under a snapshot, resolves each missing endpoint with
~resolve-endpoint~ — so it may link across stores, which write-time
linking never does — and creates the edges in one short transaction,
re-checking each claim is still unlinked so it is idempotent. ~:since~
is a commit epoch (~claim-commit-epoch~), which lets an operator sweep
exactly what a regeneration wrote; ~:limit~ bounds one call's work and
~more-p~ says whether to call again. It never prunes and never signals:
a namespace with no registered source or a source store that is not
open is skipped for the call and named in ~skipped-namespaces~; the
other counts are the report. Nothing runs on open. A bulk loader can
bind ~*link-claims-at-write*~ to NIL, write key-only, and sweep once
afterwards; the write-cost, sweep-cost and read-benefit numbers behind
that trade are ~bench-claim-linking~ in the perf suite (GH #372).
```

- [ ] **Step 2: CHANGELOG**

After the "**Edges under claims** (#367, U1 #369)" bullet, add:

```markdown
- **Linking sweep and backfill** (#367, U2 #372): `link-claim-endpoints`
  links every claim whose endpoint now resolves — across stores through
  `resolve-endpoint`, under a snapshot, then one short write
  transaction; idempotent; `:since` a commit epoch, `:limit` with a
  `more-p` return; never prunes (`compact-edges` reclaims), never
  signals (its counts are the report); nothing runs on open.
  `*link-claims-at-write*` (default T) lets a bulk load defer linking to
  the sweep. `bench-claim-linking` in the perf suite records the spec's
  §9 numbers; results on #372.
```

- [ ] **Step 3: Spec Built note**

Before `## 14. Traceability`:

```markdown
## Built — U2 (#372, <SHA of Task 5's commit>)

§5 and §9 on `feat/edges-under-claims-u2`. Deviations from §5's text:
the return gains a fifth value, `more-p`, because `:limit` bounds the
claims with a missing edge examined per call and a caller needs to
know whether to call again (endpoints that stay unresolved are
re-examined each call; `:since` is the cursor for a regeneration's
writes); the write phase re-checks each claim is still unlinked, so two
sweeps racing, or a sweep racing a write-time link, stay idempotent.
Added beyond §5: `*link-claims-at-write*` (default T), the switch §9's
measurement needs and a bulk loader wants. §9 was measured on a
synthetic store of the memory tenant's shape (`bench-claim-linking`,
`tests/perf/spacetime-bench.lisp`), not on the tenant's own data, which
lives in another system; the bench is the harness a tenant run reuses.
Numbers are on #372, not here.
```

- [ ] **Step 4: Wrap check** — every added line in the three files ≤ 80 columns (org prose ~78), then commit:

```bash
git add docs/vivace-graph-v3-doc.org CHANGELOG.md \
        docs/superpowers/specs/2026-09-12-edges-under-claims-design.md
git commit -m "docs: the linking sweep -- manual ch.18, changelog, spec Built note U2 (#372)"
```

---

### Task 7: Hand-off

- [ ] **Step 1: Final runner** from a fresh image; both suites green; counts in the ledger.
- [ ] **Step 2: Comment on #372**: commits, the deviations (`more-p`, `*link-claims-at-write*`, synthetic-store measurement), the six measurement lines from both runs with host and SHA, and "ready for PR". Do NOT push.
- [ ] **Step 3: Tell Kevin** the branch is ready to push; a tenant-data run of `bench-claim-linking`'s harness is a cl-llm follow-up.

## Self-review against the spec

- §5 signature and semantics: Task 3 (`family`, `since`, `limit`; two phases; the three condition handlings; no prune; idempotent; backfill = one call; nothing on open — no code touches `open-graph`). Fifth return value recorded as a deviation (Task 6).
- §9: Task 5 (write cost on/off; read benefit `related/3` vs `claims-touching`+`resolve-endpoint`, acceptance recorded; sweep cost). Synthetic data recorded as a deviation.
- §11 sweep bullets: idempotent ✓ T3; links a claim written before its source ✓ T3; honours `:since` ✓ T3; counts every category, never signals ✓ T3; does not prune / detached survives ✓ T4; plus `:limit`, `:family` ✓ T3, the switch ✓ T2.
- Existing suites: runner every task; perf suite loads ✓ T5.
- Names consistent: `link-claim-endpoints`, `*link-claims-at-write*`, `%family-parents-in`, `%missing-endpoints`, `%sweep-collect`, `%sweep-resolve`, `%sweep-write`, `ee-sweep`, `ee-unlinked-b`, `bench-claim-linking`, `with-perf-claim-graph`, `pb-key`, `pb-insert-things`, `pb-insert-claims`, `pb-two-hop-related`, `pb-two-hop-touching`.
