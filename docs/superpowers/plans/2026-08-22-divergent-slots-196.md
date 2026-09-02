# Divergent Slot-Set Warning (#196) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Defining one class name in two stores with *different* slot sets
warns (a `style-warning` naming both stores and the consequence); identical
slot sets — the feature — stay silent.

**Architecture:** A new condition `divergent-node-type-redefinition`
(subtype of `style-warning`) and a helper `%warn-if-divergent-across-stores`
that scans `*schema-node-metadata*` for the same class symbol registered
under a different graph-name with a non-`equal` slot list. `def-node-type`
calls the helper when its meta is built. Metadata-level only — no graph
need be open, no error, no behavior change.

**Tech Stack:** Common Lisp (SBCL primary; nothing impl-conditional),
FiveAM.

**Spec:** GH #196 (authoritative statement: warning not error; the design
deliberately permits the redefinition), namespaces design
`docs/superpowers/specs/2026-08-20-namespaces-design.md` §3.3, and GH #53
(the historical failure this re-guards: second definition silently
replaced the first class's slots, leaving stored data unreachable through
the API).

## Global Constraints

- Lisp: spaces only, never tabs; hard 80-column limit (a 96-column line is
  a defect).
- Comments terse: invariant + `(GH #196)`; history in the issue.
- SBCL suites need `--dynamic-space-size 16384`.
- Quicklisp resolves `:graph-db` to the MAIN checkout: every SBCL run in
  the worktree must first
  `--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (truename ".")) :inherit-configuration))'`.
- Suite counts are TOTAL CHECKS. Baseline on `experiment` (post-#204
  merge, b019679): 3997 checks / 3987 pass / 10 skip / 0 fail. This plan
  adds 3 checks → 4000 / 3990 / 10 / 0. Any other delta must be accounted
  check by check.
- Ablate every guard (nearest wrong implementations are named per test).
- ECL demoted: verify on SBCL only, say so.
- Host safety: live `ma-dev-server` on this host — NEVER
  `pkill`/`killall`/`pgrep` on sbcl or paths; kill only a PID you started;
  one SBCL at a time; never touch `/data0`. Never end a turn to "wait" on
  a background run — bounded foreground stretches
  (`timeout 570 bash -c 'while kill -0 <PID> 2>/dev/null; do sleep 15; done'`).
- Worktree `.worktrees/196-divergent-slots`, branch `196-divergent-slots`
  off `experiment` (b019679). PR against `experiment`. Pushing is
  explicit-only.

---

### Task 1: Condition, helper, def-node-type hook, tests

**Files:**
- Modify: `schema.lisp` (new condition + helper directly above
  `def-node-type` ~line 296; one call inside the macro's expansion; the
  stale "No cross-graph uniqueness check" comment at ~line 328 gains a
  pointer)
- Modify: `package.lisp` (export the condition name, next to
  `#:ambiguous-node-type-name` ~line 179)
- Modify: `tests/global-type-id-tests.lisp` (three tests appended — this
  file owns the multi-store area and already holds
  `a-class-may-be-instantiated-in-more-than-one-store`)

**Interfaces:**
- Produces: condition `divergent-node-type-redefinition` (subtype of
  `style-warning`; exported) with readers `divergent-type-name`,
  `divergent-type-graph-name`, `divergent-type-other-graphs`; helper
  `%warn-if-divergent-across-stores (meta)`. Task 2 documents these.

- [ ] **Step 1: Write the failing tests**

Append to `tests/global-type-id-tests.lisp`:

```lisp
;;; ---------------------------------------------------------------------------
;;; Divergent slot sets across stores warn; identical ones stay silent
;;; (GH #196, re-guarding GH #53's failure mode after
;;; %CHECK-NODE-CLASS-GRAPH-UNIQUE was correctly deleted by #186).
;;;
;;; The DEF-VERTEX forms are EVAL'd inside the tests, not written at
;;; toplevel: the divergent pair would otherwise warn at load time of
;;; this file, polluting every suite run's output.  No graph is opened
;;; and no system directory is needed -- the check is metadata-level.
;;; ---------------------------------------------------------------------------

(test divergent-slot-sets-across-stores-warn
  "Same class symbol, two stores, DIFFERENT slots: the second definition
must signal.  Nearest wrong implementation: never warn (the guard
deleted by #186 with nothing in its place)."
  (eval '(def-vertex gdiv-probe () ((div-a :type string)) :gdiv-store-a))
  (signals graph-db:divergent-node-type-redefinition
    (eval '(def-vertex gdiv-probe () ((div-b :type string))
             :gdiv-store-b))))

(test identical-slot-sets-across-stores-stay-silent
  "Same class symbol, two stores, SAME slots: the feature, and it must
stay silent.  Nearest wrong implementation: warn on any cross-store
redefinition regardless of the slots."
  (eval '(def-vertex gdiv-same () ((div-c :type string)) :gdiv-store-c))
  (is-true
   (handler-case
       (progn (eval '(def-vertex gdiv-same () ((div-c :type string))
                       :gdiv-store-d))
              t)
     (graph-db:divergent-node-type-redefinition () nil))))

(test divergence-warning-is-a-style-warning
  "STYLE-WARNING, not a bare WARNING: the design deliberately permits the
redefinition (GH #196), so it must be muffleable by severity class."
  (is (subtypep 'graph-db:divergent-node-type-redefinition
                'style-warning)))
```

- [ ] **Step 2: Run to verify failure**

From the worktree:

```
sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(asdf:initialize-source-registry (list :source-registry (list :tree (truename ".")) :inherit-configuration))' \
  --eval '(ql:quickload :graph-db)' \
  --eval '(asdf:load-system :graph-db/test)' \
  --eval '(fiveam:run! (quote graph-db/test::global-type-id-suite))'
```

Expected: the test file cannot reference `graph-db:divergent-node-type-
redefinition` until the condition+export exist — as in #190, it is
acceptable to land the condition + export first (the first hunk of
Step 3), then run and observe the three tests fail BEHAVIORALLY:
`divergent-slot-sets-across-stores-warn` fails (nothing signals),
`identical-...-stay-silent` passes vacuously (pin), `...-is-a-style-warning`
passes once the condition exists (pin). The signaling test is the RED.

- [ ] **Step 3: Implement**

In `schema.lisp`, directly above `def-node-type` (after
`lookup-node-type-by-id`/`update-node-type` region, ~line 296), add:

```lisp
(define-condition divergent-node-type-redefinition (style-warning)
  ((name :initarg :name :reader divergent-type-name)
   (graph-name :initarg :graph-name :reader divergent-type-graph-name)
   (other-graphs :initarg :other-graphs
                 :reader divergent-type-other-graphs))
  (:report
   (lambda (c s)
     (format s "Node type ~S is being defined for ~S with a slot set ~
that differs from its definition for ~{~S~^, ~}.  All of these name ONE ~
CLOS class, so the last definition loaded determines the slots; data ~
stored under the other slot set stays on disk but becomes unreachable ~
through the API (GH #196, GH #53).  Keep the slot sets identical, or ~
use different type names."
             (divergent-type-name c)
             (divergent-type-graph-name c)
             (divergent-type-other-graphs c)))))

(defun %warn-if-divergent-across-stores (meta)
  "STYLE-WARNING when META's class symbol is already registered under a
DIFFERENT graph-name with a non-EQUAL slot list.  Identical slots are the
multi-store feature and stay silent; a same-store redefinition is schema
evolution and is not this guard's business (GH #196)."
  (let ((divergent nil))
    (maphash
     (lambda (graph-name metas)
       (unless (eq graph-name (node-type-graph-name meta))
         (let ((other (find (node-type-name meta) metas
                            :key #'node-type-name)))
           (when (and other
                      (not (equal (node-type-slots other)
                                  (node-type-slots meta))))
             (push graph-name divergent)))))
     *schema-node-metadata*)
    (when divergent
      (warn 'divergent-node-type-redefinition
            :name (node-type-name meta)
            :graph-name (node-type-graph-name meta)
            :other-graphs (nreverse divergent)))))
```

In `def-node-type`'s expansion, immediately after the `(let* ((,meta
(make-node-type ...)))` binding closes and before
`(finalize-inheritance ...)`, insert one form:

```lisp
           (%warn-if-divergent-across-stores ,meta)
```

Update the comment at ~line 328 from:

```lisp
         ;; No cross-graph uniqueness check: type-ids are system-wide as of
         ;; #186, so one class may be instantiated in more than one store.
```

to:

```lisp
         ;; No cross-graph uniqueness check: type-ids are system-wide as of
         ;; #186, so one class may be instantiated in more than one store.
         ;; Divergent slot sets across stores warn instead (GH #196).
```

In `package.lisp`, next to `#:ambiguous-node-type-name` add:

```lisp
           #:divergent-node-type-redefinition
           #:divergent-type-name
           #:divergent-type-graph-name
           #:divergent-type-other-graphs
```

- [ ] **Step 4: Run the focused suite, then the full suite; reconcile**

Focused: `global-type-id-suite` all green including the 3 new checks.
Full suite (same invocation shape, `(asdf:test-system :graph-db)`):
expected exactly 4000 checks / 3990 pass / 10 skip / 0 fail. Watch test
OUTPUT cleanliness too: the existing multi-store fixtures (`dual-type` in
two stores, identical slots; `shared-type` likewise) must NOT now print
warnings during the load or run — if they do, the guard is wrong, not the
fixtures.

- [ ] **Step 5: Ablation — actually run both**

(a) Make `%warn-if-divergent-across-stores` a no-op: confirm
`divergent-slot-sets-across-stores-warn` FAILS. (b) Drop the
`(not (equal ...))` clause (warn on any cross-store redefinition):
confirm `identical-slot-sets-across-stores-stay-silent` FAILS — and note
whether the full-suite load now warns on `dual-type`/`shared-type`
(it should, which double-confirms the clause is load-bearing). Revert,
re-run focused suite green.

- [ ] **Step 6: Commit**

```bash
git add schema.lisp package.lisp tests/global-type-id-tests.lisp
git commit -m "feat(schema): warn when one class diverges across stores (#196)"
```

---

### Task 2: Documentation

**Files:**
- Modify: `CHANGELOG.md` (`## [Unreleased]` → `### Added`, creating the
  heading only if absent; match existing conventions)
- Modify: `docs/vivace-graph-v3-doc.org` (~line 2961: the "Both
  definitions define the *same CLOS class*..." passage)
- Modify: `docs/superpowers/specs/2026-08-20-namespaces-design.md` §3.3
  (one-sentence fixed-note, same style as §3.4's #190 note)

**Interfaces:**
- Consumes: the condition name `divergent-node-type-redefinition` from
  Task 1 (documented verbatim).

- [ ] **Step 1: CHANGELOG**

Under `## [Unreleased]` / `### Added`:

```markdown
- Defining one class name in two stores with *different* slot sets now
  signals `divergent-node-type-redefinition` (a `style-warning`): both
  definitions name one CLOS class, so the last one loaded determines the
  slots and the earlier store's data becomes unreachable through the API
  (the GH #53 failure). Identical slot sets — the multi-store feature —
  stay silent. (#196)
```

- [ ] **Step 2: Manual + spec**

In `docs/vivace-graph-v3-doc.org` at the "Both definitions define the
*same CLOS class*, so the last one loaded determines its slots" passage
(~2961), append one sentence in the file's prose style: as of #196,
declaring *different* slot sets signals a ~divergent-node-type-
redefinition~ style-warning naming both stores; identical slot sets stay
silent.

In the namespaces spec §3.3, add a one-line note (mirroring §3.4's
"**Fixed (#190):**" style): **Guarded (#196):** divergent slot sets
across stores now signal `divergent-node-type-redefinition`; identical
sets stay silent.

- [ ] **Step 3: Commit**

```bash
git add CHANGELOG.md docs/vivace-graph-v3-doc.org \
        docs/superpowers/specs/2026-08-20-namespaces-design.md
git commit -m "docs: divergent slot sets across stores warn (#196)"
```

- [ ] **Step 4 (controller): whole-branch review, then PR**

Whole-branch review on a capable model over the branch range; PR against
`experiment` titled "Warn when one class diverges across stores (#196)".
Full diff to Kevin before push; pushing is explicit-only.

---

## Self-Review

- Spec coverage: #196 asks for (a) a style-warning at `def-node-type` when
  a name registered under a different graph-name is defined with a
  non-`equal` slot set — Task 1; (b) a test pinning the divergent case —
  Task 1's first test; the silent identical case and severity class are
  pinned too. Warning-not-error honored (style-warning).
- The check runs at def time with no graph open (metadata scan), so the
  warning also fires for stores not yet created — intended: the metadata
  is what `update-schema` will replay.
- Types: reader names match between condition definition, exports, and
  docs.
- Placeholders: none.
