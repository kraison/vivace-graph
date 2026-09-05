# rules S2: the rule record, compile, run, provenance — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A rule is a stored, versioned producer: `def-rules-schema` gives a store the `rule` record and the `derivation` family; `compile-rule` reads a rule's head and body through the guard and refuses recursion; `run-rule` sweeps the rule's previous derivation and derives afresh in one validated transaction, each derived claim carrying its premises as provenance and a validity extent that is the intersection of theirs.

**Architecture:** Three new files in `rules/` on top of S1's `facts.lisp`. `schema.lisp` is one macro expanding to a `def-source` and a `def-claim-classes`, per store. `compile.lisp` turns text into a `compiled-rule` through a new `graph-db.query:guard-query-text` export (screen, read, guard, scratch package deleted), validates the head shape, orders `claim-producer` generators first, and walks a relation graph over every rule in scope to refuse cycles; the same compile runs as a commit validator so a bad rule is never stored. `run.lisp` runs the body under `run-query-goals`' rails inside the sweep's transaction, constructs the derived claims through the family's own constructor, writes one `derived-from` claim per premise, and turns every refusal into a `rule-report`. `extent-intersection` is one PR against cl-temporal-extent, done first.

**Tech Stack:** SBCL 2.6.6, ASDF, FiveAM, `graph-db/spacetime`, `graph-db/query`, `graph-db/rules` (S1), `local-time`, `cl-temporal-extent` 0.3.0.

**Spec:** `docs/superpowers/specs/2026-09-04-rules-as-producers-design.md` §5–§9, §11 (epic #304, sub-issue #331). S1's record: `docs/superpowers/decisions/2026-09-04-rules-s1-rulings.md`, `docs/superpowers/notes/2026-09-04-rules-s1-engine-api-facts.md`, `docs/rules.md`, and the handoff `docs/superpowers/handoffs/2026-09-05-rules-s2.md`.

## Global Constraints

- Lisp: spaces only; hard 80-column limit; terse comments pointing at the spec section or #331. Docstrings say what it does, what it returns, the one trap.
- `graph-db/rules` depends on `graph-db/spacetime` and `graph-db/query` only. Core is untouched except for the one `graph-db.query` export this plan names (`guard-query-text`, spec §13's "slice exports what it needs").
- `graph-db.rules` `(:use #:cl)` only. **Never import spacetime's `claim-producer`, `claim-relation`, `claim-standing`, `claim-rule-version` accessors** (ruling R4; `rules/package.lisp` header). Write every spacetime and graph-db symbol qualified.
- The S1 deviations stand (handoff): the seven functors stay homed in `graph-db`; the cost-unbounded rule is per goal.
- **Worktrees.** vivace-graph work happens in `/home/raison/work/vivace-graph-v3/.worktrees/rules-s2` (branch `feat/rules-s2`, based on `docs/handoff-2026-09-05` = experiment `00b38fc` + the handoff commit). cl-temporal-extent work happens in `/home/raison/work/cl-temporal-extent/.worktrees/extent-intersection` (branch `feat/extent-intersection`, from `origin/master` `1ca7765`). The main checkouts are shared with other live sessions: **never build, edit or commit there.**
- **Run every suite in a subprocess, in the foreground, both worktrees first on the registry:**

  ```
  cd /home/raison/work/vivace-graph-v3/.worktrees/rules-s2
  sbcl --dynamic-space-size 4096 --non-interactive \
    --eval '(push #p"/home/raison/work/cl-temporal-extent/.worktrees/extent-intersection/" asdf:*central-registry*)' \
    --eval '(push #p"/home/raison/work/vivace-graph-v3/.worktrees/rules-s2/" asdf:*central-registry*)' \
    --eval '(ql:quickload :graph-db/rules-test :silent t)' \
    --eval '(asdf:test-system :graph-db/rules-test)'
  ```

  Always target the `-test` system; read the `Did N checks.` line and record the count. `asdf:test-system` on a non-test system is a silent no-op. Never background a run; never run two suites at once from one worktree (shared FASL cache). The temporal-extent suite: `cd <extent worktree> && sbcl --non-interactive --eval '(push #p"<extent worktree>/" asdf:*central-registry*)' --eval '(ql:quickload :cl-temporal-extent/tests :silent t)' --eval '(asdf:test-system :cl-temporal-extent/tests)'`.
- Baselines at `00b38fc`: `graph-db/rules-test` 67/0, `graph-db/query-test` 33/0, `graph-db/spacetime-test` 653/0, `graph-db/fast-test` 4950/0. Before the branch is called done: rules, query, spacetime, gui (635/0) and `graph-db/fast-test` all green, counts recorded; the full `graph-db` (5370/0) once before the PR.
- **After every scripted edit to a test file**, `git diff <base> -- <file> | grep -c '^-[^-]'` and expect 0 for an append (handoff: "deleted tests keep suites green").
- **Every negative test names its mechanism and has a control**: a refusal test asserts the condition class *and* that the same shape succeeds when the one thing changes. Ask of every test: would it fail if the thing it is named after broke?
- No push until Kevin says so. Commit trailers: `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` and `Claude-Session: https://claude.ai/code/session_01DeVU44qpXuW4oUz7hnDMNU`. Docs travel with the code in the same commit (the push hook enforces it).
- Issues: #331 gets the decisions below as a comment when the branch is complete; a merge never auto-closes an issue on `experiment`, close by hand with the merge SHA.

## Rulings taken while planning (record them in the decision file at the end)

Each deviates from or refines the spec; each is reversible; none was put to Kevin. Order of authority while executing: spec > these > the S1 facts note > the task text.

- **P1 — one `rule` vertex per rule NAME, not per version.** Spec §5 says "one vertex per rule version" and, in the same section, `def-source` identity `(:namespace :rule :key-slot name)`. `def-source` emits `def-unique` on the key slot (`spacetime/source.lisp:229`), so the identity facet itself makes a second record with the same name a uniqueness violation. The two sentences cannot both hold; the identity facet wins because it is the mechanism. A new version is `copy`, set `version`, `save`; the engine's revision history is the record of the old text, and `rule-version` on every derived claim is the record of which version derived it. `run-rules` therefore has exactly one candidate per name. **Cost if wrong:** two versions of one rule cannot coexist as records; reversing means a composite key slot and a policy for which enabled version runs.
- **P2 — compile per run, not on `open-graph`.** Spec §6 says compilation runs on `open-graph`. `open-graph` is a `defun` with no hook (`graph.lisp:1103`); adding one is a core change. A rule compiles in milliseconds, rules are few, and `run-rules` runs seldom, so `run-rule` compiles the rule it is about to run and `run-rules` compiles every candidate first. A rule that fails to compile is reported (`outcome :refused`, refusal tag `:rule`) and skipped; the store opens regardless, which is what §6 wanted. **Cost if wrong:** compile cost on every run; a cache is a later addition behind the same functions.
- **P3 — a rule write is refused at commit when it does not compile.** Spec §6: "compilation runs ... on every rule write", §5: "validated like any other write". `%validate-rule-writes` joins `graph-db:*commit-validators*` (the `%validate-extent-disjointness` shape, `spacetime/temporal.lisp:69-115`) and signals `rule-compile-error`, a `graph-db:constraint-violation`, for a written `rule` vertex that does not compile against the post-commit rule set — the cycle check included. So the store never holds a rule that could not run *at the time it was written*; P2 covers drift after that (a `def-rule` added later, a schema change). **Cost if wrong:** an operator cannot park a half-written rule in the store; they use `enabled NIL` for that, and a disabled rule still has to compile.
- **P4 — `run-rule` always runs under a resource bound.** #331's carried finding: inside `run-rule`'s transaction a family walk would join every scanned node to the read set. `run-rule` binds `*query-default-max-inferences*` / `*query-default-timeout*` from `*rules-max-inferences*` / `*rules-timeout*` (defaults: the DSL's, 1000000 and 30) and signals a plain error if an operator has NILed both — so an unrouted `claim/7` goal in a body is refused as cost-unbounded (`prolog-cost-unbounded-error` → report refusal `:budget`) and the walk is unreachable from `run-rule`. `:allow-cost-unbounded` is never passed. This is the "refuse the walk the way a budget refuses it" option the issue argued for.
- **P5 — `claim-producer` generators are moved to the front of the body.** Handoff: goal order is load-bearing and `compile-rule` is the place to fix it. A body goal `(claim-producer ?v "p")` with `?v` a variable and a string producer is a generator; running it first is never wrong (a later `claim/7` on `?v` takes its node route) and turns an unrouted `claim/7` on `?v` into a routed one. Other goals keep their order. **Cost if wrong:** a rule author who relied on a filter-direction `claim-producer` after an indexed `claim/7` gets the same rows via the producer route, possibly slower. Recorded in `docs/rules.md`.
- **P6 — the recursion graph is over relation names, and an unbound relation reads everything.** Spec §6 keys the graph on relations. A body `claim/7` whose relation argument is a variable reads every relation, so it gets an edge to every head relation in scope, its own included — such a rule is always the one-node cycle and is refused with a message saying to bind the relation. Families are ignored in the graph (a rule deriving relation R in family F1 that reads R in F2 is refused): conservative, and spec-literal.
- **P7 — a solution's extent is intersected whatever the family's temporality.** Spec §8 states the `:premises` policy for temporal families. Applying it to a non-temporal family too costs nothing and records the validity the premises had; the derived claim's identity ignores the extent there, so duplicates collapse by endpoints alone and the first solution's extent is kept. `:none` derives no extent anywhere.
- **P8 — `run-rules graph` includes a `def-rule` only when the store carries its family.** `def-rule`s are image-wide; a family the store's schema does not declare cannot be swept or derived into (`query-precondition-error` from the producer index). Filter by `(graph-db.query:schema-type-names graph :vertex)`; a def-rule that is filtered out is not reported. `run-rule` on such a rule by name is refused with tag `:rule`.
- **P9 — `extent-intersection` takes standing, semantics and precision from keywords, defaulting to A's standing and semantics and the coarser precision.** The library must not assume `:inferred`; the rules caller passes `:semantics :validity :standing :inferred`. An instant on either side gives an instant (the point narrowed to the other extent's hull); a result that is certainly empty (`extents-disjoint-p`) is NIL. Fuzzy bounds combine coordinate-wise: the later start (max of earliests, max of latests), the earlier end (min of both), `:unbounded` read as −∞ in an earliest and +∞ in a latest.

---

## File structure

| file | responsibility |
|---|---|
| `~/work/cl-temporal-extent/…/src/allen.lisp`, `src/package.lisp`, `tests/allen-tests.lisp`, `README.md`, `CHANGELOG.md`, `cl-temporal-extent.asd` | `extent-intersection` (Task 1); version 0.3.0 |
| `query/guard.lisp`, `query/package.lisp` | export `guard-query-text` (Task 2) |
| `rules/package.lisp` | exports for S2 (Task 2) |
| `rules/schema.lisp` | `def-rules-schema`: the `rule` record and the `derivation` family (Task 2) |
| `rules/compile.lisp` | `rule-spec`, `def-rule`, `compile-rule`, `rule-compile-error`, the cycle check, the commit validator (Task 3) |
| `rules/run.lisp` | `run-rule`, `run-rules`, `rule-report`, `premises-of`, `dependents-of` (Task 4) |
| `rules/facts.lisp` | the explicit-NIL `?c` gate fix (Task 5) |
| `graph-db.asd` | components, the `cl-temporal-extent` 0.3.0 floor (Task 2) |
| `tests/rules/suite.lisp` | `def-rules-schema` on the test store, temporal seed, reopenable fixture (Task 2) |
| `tests/rules/schema-tests.lisp`, `compile-tests.lisp`, `run-tests.lisp` | the S2 suite (Tasks 2–4) |
| `docs/rules.md`, `CHANGELOG.md`, `docs/superpowers/decisions/2026-09-05-rules-s2-rulings.md` | the contract, the entry, the record (Task 5) |

---

### Task 0: Recon — verify what this plan assumes before any code

S1's recon found six defects in its plan before execution, three blocking. Budget the same here. Dispatch readers with the S1 facts note as the template; each claim below is to be confirmed or refuted **from source in the `rules-s2` worktree** (or the extent worktree), quoting the form. Adversarially verify each correction. Write the result as `docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md` (pinned to the worktree HEAD) and amend the tasks below before Task 1 starts.

- [ ] **A1** `graph-db:constraint-violation` (`package.lisp:423`) is a condition class with no required initargs, so `(define-condition rule-compile-error (graph-db:constraint-violation) ...)` with its own slots is legal. Find its `define-condition`.
- [ ] **A2** A `:check` slot option names a schema function by symbol: confirm how `register-schema-function` keys the registry (`runtime-schema.lisp:242`) and that writing `:check graph-db.spacetime:canonical-relation-p` in a `def-source` slot spec expanded in package `graph-db.rules` resolves to the function spacetime registered at `spacetime/claim.lisp:81-83`. If the registry is keyed by symbol and `canonical-relation-p` is not exported, the slot spec must write `graph-db.spacetime::canonical-relation-p`. Quote both forms.
- [ ] **A3** `def-source` inside another macro: the seven facet checks run at macroexpansion of `def-source` itself (`spacetime/source.lisp:196-211`), so `(def-rules-schema :g)` expanding to `(def-source rule :g …)` works when the outer form is compiled from a file in package `graph-db.rules`. Confirm `def-vertex`'s `NAME` package comes from the symbol, not `*package*`, so `rule` is `graph-db.rules::rule` and its constructor `graph-db.rules::make-rule`, whatever package the caller expands in (`schema.lisp:763-811`, `%schema-symbol-package`).
- [ ] **A4** One class in two stores: `def-node-type`'s comment says "one class may be instantiated in more than one store" (`schema.lisp:790`). Confirm that a second `(def-rules-schema :other)` after `(def-rules-schema :g)` registers the `rule` type, its `name` index, its `def-unique` and the `derivation` family under `:other` **without** unregistering them from `:g` (`%register-node-type-meta`, `register-index-spec`, `register-unique-tuple-spec`, `def-claim-classes`'s per-graph forms). Note what `%warn-if-cross-file-clobber` says when both expansions are in one test file (expected: silent, same file).
- [ ] **A5** `graph-db::run-query-goals` (`query/dsl.lisp:302`): with `:format :raw :callback`, the callback receives one list per solution aligned with `vars`; nodes arrive as node objects; a variable bound to a keyword arrives as the keyword. Confirm `select/2`'s projection (`prolog-functors.lisp:587-626`) turns only `GRAPH-DB`-homed symbols into strings — a `KEYWORD` passes.
- [ ] **A6** `select`'s `:snapshot t` inside an open transaction (`prologc.lisp:1030`, `call-with-read-snapshot`): confirm the body of `run-query-goals` reads inside `run-rule`'s `with-transaction` and does not open a second transaction or signal. Confirm `index-lookup` inside that transaction answers from the committed store, so claims `mark-deleted` by the sweep earlier in the same transaction are still generated by `claim/7` (docs/rules.md "What the functors do not see"). This is why the cycle check must be strict: the only claims a body could re-read after its own sweep are its own.
- [ ] **A7** `mark-deleted` then a fresh claim with the same identity tuple in one transaction commits cleanly under `def-unique` and `%validate-extent-disjointness` (`tests/spacetime/claim-query-tests.lisp:139-146` is the sweep-then-insert precedent). Quote the test.
- [ ] **A8** `graph-db:writes`, `graph-db::node` (of a write), `graph-db:make-commit-view`, `graph-db:view-node`, `graph-db:deleted-p`, `graph-db:mark-deleted`, `graph-db:copy`, `graph-db:save`, `graph-db:graph-name`, `graph-db:map-vertices` `:record-reads`: confirm each name and export (`package.lisp`).
- [ ] **A9** `graph-db.spacetime` exports `missing-claim-identity-component`, `spacetime-error`, `extent-sexp-start-key`, `claim-identity-key`, `split-claim-identity-key`, `claims-by-producer`, `delete-claims-by-producer`, `extent-disjointness-violation`, `def-source`, `def-claim-classes` (`spacetime/package.lisp`). Confirm `extent->sexp`, `make-interval`, `exact-bound`, `extents-disjoint-p` are re-exported there too.
- [ ] **A10** `graph-db::variable-p` (`prologc.lisp:278`) is name-based, so a scratch-package symbol that is uninterned after `delete-package` is still a variable to the compiler, still `EQ` across the goals it appears in, and `select` accepts it in `vars`. Confirm `replace-?-vars` (`prologc.lisp:773`) only replaces the symbol `graph-db::?` by `EQ`, so a bare `?` in rule text read into a scratch package is a *named* variable there (note it as a documented trap, not a fix).
- [ ] **A11** The guard's `%guard-goal` canonical head for `claim` is `(intern "CLAIM" (find-package :graph-db))` = `graph-db::claim`; `(claim-producer …)` likewise `graph-db::claim-producer`. A schema symbol argument (the family) comes back as the schema's own class symbol, `EQ` to the `*claim-families*` key. NIL in an argument position comes back as NIL (`%guard-symbol`).
- [ ] **A12** `extent-sexp-start-key` (`spacetime/claim.lisp`): signature, what it returns for an interval and an instant sexp, and that `claim-identity-key`'s temporal field is `(prin1-to-string (extent-sexp-start-key sexp))` under `*print-case* :downcase` — so the dedupe key in Task 4 can use `extent-sexp-start-key` directly.
- [ ] **A13** `graph-db:*commit-validators*` functions are called as `(funcall fn tx graph)` inside the manager-locked commit region (`transactions.lisp:3441`); a condition signalled there unwinds through `with-transaction` to the caller unchanged (`tests/spacetime/temporal-tests.lisp:149-152`). Confirm the region tolerates the validator creating and deleting a package and calling `make-commit-view`.
- [ ] **A14** cl-temporal-extent: `%effective-start`/`%effective-end` are internal to `temporal-extent` (`src/allen.lisp`), `bound-compare` is exported, `%make-bound` bypasses the reversed check, `make-bound` signals `invalid-bound` on a reversed range. `+precisions+` is ordered coarse to fine.
- [ ] **A15** `ql:quickload` with both worktrees pushed onto `asdf:*central-registry*` loads `cl-temporal-extent` from the extent worktree (check `(asdf:system-source-directory :cl-temporal-extent)` in the same image) and satisfies a `(:version :cl-temporal-extent "0.3.0")` floor once the `.asd` says 0.3.0.

Record every correction in the note's §C with the finding → correction map, then edit the task text below to match. Do not start Task 1 until the note is committed.

---

### Task 1: `extent-intersection` in cl-temporal-extent

Repo: `/home/raison/work/cl-temporal-extent/.worktrees/extent-intersection`, branch `feat/extent-intersection`. Public repo: the issue and commit text stay domain-neutral (no rules, claims or stores). File the issue first: **"extent-intersection: the intersection constructor the algebra lacks"** in kraison/cl-temporal-extent, body: "`extents-intersect-p` says whether two extents possibly share an instant; nothing constructs the extent they share. Add `(extent-intersection a b &key precision semantics standing) => extent or NIL` with the library's closed-interval semantics: NIL when the pair is certainly disjoint, an instant when either side is one, fuzzy bounds combined coordinate-wise." Note the issue number as `#N` below. **Filed: kraison/cl-temporal-extent#5** — write `#5` wherever `#N` appears.

**Files:**
- Modify: `src/allen.lisp` (append after `extents-intersect-p`), `src/package.lisp` (export), `tests/allen-tests.lisp` (append), `README.md`, `CHANGELOG.md`, `cl-temporal-extent.asd` (version `0.3.0`)

**Interfaces:**
- Produces: `temporal-extent:extent-intersection (a b &key precision semantics standing) => temporal-extent or NIL`.

- [ ] **Step 1: Write the failing tests** — append to `tests/allen-tests.lisp`:

```lisp
;;; extent-intersection (#N): the extent two extents share, or NIL.

(test intersection-of-overlapping-exact-intervals-is-the-overlap
  (let* ((a (exact-interval (ts 2026 1 1) (ts 2026 3 31)))
         (b (exact-interval (ts 2026 2 1) (ts 2026 6 30)))
         (r (extent-intersection a b)))
    (is-true r)
    (is (eq :interval (extent-kind r)))
    (is (timestamp= (ts 2026 2 1) (bound-earliest (extent-start r))))
    (is (timestamp= (ts 2026 3 31) (bound-latest (extent-end r))))
    (is-true (bound-exact-p (extent-start r)))
    (is-true (bound-exact-p (extent-end r)))
    ;; Commutative in the bounds.
    (let ((s (extent-intersection b a)))
      (is (timestamp= (bound-earliest (extent-start r))
                      (bound-earliest (extent-start s))))
      (is (timestamp= (bound-latest (extent-end r))
                      (bound-latest (extent-end s)))))))

(test intersection-of-disjoint-extents-is-nil
  (let ((a (exact-interval (ts 2026 1 1) (ts 2026 1 31)))
        (b (exact-interval (ts 2026 3 1) (ts 2026 3 31))))
    (is (null (extent-intersection a b)))
    (is (null (extent-intersection b a)))
    ;; Control: the same A against something it does touch is not NIL.
    (is-true (extent-intersection a (exact-interval (ts 2026 1 15)
                                                    (ts 2026 2 15))))))

(test meeting-intervals-intersect-in-their-boundary-instant
  "Intervals are closed, so [1,2] and [2,3] share the instant 2."
  (let ((r (extent-intersection (exact-interval (ts 2026 1 1) (ts 2026 1 2))
                                (exact-interval (ts 2026 1 2) (ts 2026 1 3)))))
    (is-true r)
    (is-true (extent-instant-p r))
    (is (timestamp= (ts 2026 1 2) (bound-earliest (extent-start r))))))

(test containment-intersects-to-the-inner-extent
  (let* ((outer (exact-interval (ts 2026 1 1) (ts 2026 12 31)))
         (inner (exact-interval (ts 2026 3 1) (ts 2026 3 31)))
         (r (extent-intersection outer inner)))
    (is (timestamp= (ts 2026 3 1) (bound-earliest (extent-start r))))
    (is (timestamp= (ts 2026 3 31) (bound-latest (extent-end r))))))

(test an-instant-intersects-an-interval-as-itself-or-not-at-all
  (let ((i (exact-interval (ts 2026 1 1) (ts 2026 1 31)))
        (inside (make-instant (exact-bound (ts 2026 1 10))))
        (outside (make-instant (exact-bound (ts 2026 2 10)))))
    (let ((r (extent-intersection inside i)))
      (is-true (extent-instant-p r))
      (is (timestamp= (ts 2026 1 10) (bound-earliest (extent-start r)))))
    (is (null (extent-intersection outside i)))
    ;; Both orders.
    (is-true (extent-instant-p (extent-intersection i inside)))))

(test a-fuzzy-instant-is-narrowed-to-the-interval
  "A point known only to lie in [Jan 1, Jan 20], intersected with
[Jan 10, Jan 31], is a point in [Jan 10, Jan 20]."
  (let* ((p (make-instant (make-bound (ts 2026 1 1) (ts 2026 1 20))))
         (i (exact-interval (ts 2026 1 10) (ts 2026 1 31)))
         (r (extent-intersection p i)))
    (is-true (extent-instant-p r))
    (is (timestamp= (ts 2026 1 10) (bound-earliest (extent-start r))))
    (is (timestamp= (ts 2026 1 20) (bound-latest (extent-start r))))))

(test an-open-end-is-narrowed-by-the-other-extents-end
  (let* ((open (make-interval (exact-bound (ts 2026 1 1)) (unknown-bound)))
         (closed (exact-interval (ts 2026 2 1) (ts 2026 3 1)))
         (r (extent-intersection open closed)))
    (is (timestamp= (ts 2026 2 1) (bound-earliest (extent-start r))))
    (is (timestamp= (ts 2026 3 1) (bound-latest (extent-end r))))
    (is-true (bound-exact-p (extent-end r)))))

(test fuzzy-bounds-combine-coordinate-wise
  "Starts take the later of each coordinate, ends the earlier."
  (let* ((a (make-interval (make-bound (ts 2026 1 1) (ts 2026 1 10))
                           (make-bound (ts 2026 3 1) (ts 2026 3 10))))
         (b (make-interval (make-bound (ts 2026 1 5) (ts 2026 1 20))
                           (make-bound (ts 2026 2 20) (ts 2026 3 5))))
         (r (extent-intersection a b)))
    (is (timestamp= (ts 2026 1 5) (bound-earliest (extent-start r))))
    (is (timestamp= (ts 2026 1 20) (bound-latest (extent-start r))))
    (is (timestamp= (ts 2026 2 20) (bound-earliest (extent-end r))))
    (is (timestamp= (ts 2026 3 5) (bound-latest (extent-end r))))))

(test intersection-metadata-defaults-and-keywords
  (let* ((a (make-interval (exact-bound (ts 2026 1 1))
                           (exact-bound (ts 2026 3 1))
                           :precision :day :semantics :validity
                           :standing :observed))
         (b (make-interval (exact-bound (ts 2026 2 1))
                           (exact-bound (ts 2026 4 1))
                           :precision :month :semantics :event
                           :standing :asserted))
         (r (extent-intersection a b))
         (k (extent-intersection a b :semantics :transaction
                                     :standing :inferred
                                     :precision :second)))
    ;; Defaults: A's semantics and standing, the coarser precision.
    (is (eq :validity (extent-semantics r)))
    (is (eq :observed (extent-standing r)))
    (is (eq :month (extent-precision r)))
    (is (eq :transaction (extent-semantics k)))
    (is (eq :inferred (extent-standing k)))
    (is (eq :second (extent-precision k)))))

(test intersection-is-nil-exactly-when-disjoint-over-exact-intervals
  "Property over every pair of small exact intervals: NIL iff
EXTENTS-DISJOINT-P, and a non-NIL result touches both inputs and is
[max start, min end]."
  (loop for as from 1 to 4 do
    (loop for ae from (1+ as) to 5 do
      (loop for bs from 1 to 4 do
        (loop for be from (1+ bs) to 5 do
          (let* ((a (exact-interval (ts 2026 1 as) (ts 2026 1 ae)))
                 (b (exact-interval (ts 2026 1 bs) (ts 2026 1 be)))
                 (r (extent-intersection a b)))
            (is (eq (null r) (extents-disjoint-p a b))
                "[~D,~D] vs [~D,~D]" as ae bs be)
            (when r
              (is-false (extents-disjoint-p r a))
              (is-false (extents-disjoint-p r b))
              (is (timestamp= (ts 2026 1 (max as bs))
                              (bound-earliest (extent-start r))))
              (is (timestamp= (ts 2026 1 (min ae be))
                              (bound-latest (extent-end r)))))))))))

(test intersection-rejects-a-non-extent
  (let ((i (exact-interval (ts 2026 1 1) (ts 2026 1 2))))
    (signals type-error (extent-intersection nil i))
    (signals type-error (extent-intersection i nil))))
```

- [ ] **Step 2: Run to verify they fail** — the suite command from Global Constraints (extent variant). Expected: each new test fails with `EXTENT-INTERSECTION` undefined; count the `Did N checks.` line before (baseline) and after.

- [ ] **Step 3: Implement** — append to `src/allen.lisp`:

```lisp
;;; Intersection -- the extent two extents share (#N).

(defun %later-start (a b)
  "The later of two START bounds, coordinate-wise: the later earliest,
the later latest.  :UNBOUNDED is -inf in an earliest and +inf in a
latest."
  (flet ((later-earliest (x y)
           (cond ((eq x :unbounded) y)
                 ((eq y :unbounded) x)
                 ((local-time:timestamp< x y) y)
                 (t x)))
         (later-latest (x y)
           (cond ((or (eq x :unbounded) (eq y :unbounded)) :unbounded)
                 ((local-time:timestamp< x y) y)
                 (t x))))
    (%make-bound (later-earliest (bound-earliest a) (bound-earliest b))
                 (later-latest (bound-latest a) (bound-latest b)))))

(defun %earlier-end (a b)
  "The mirror of %LATER-START for END bounds."
  (flet ((earlier-earliest (x y)
           (cond ((or (eq x :unbounded) (eq y :unbounded)) :unbounded)
                 ((local-time:timestamp< x y) x)
                 (t y)))
         (earlier-latest (x y)
           (cond ((eq x :unbounded) y)
                 ((eq y :unbounded) x)
                 ((local-time:timestamp< x y) x)
                 (t y))))
    (%make-bound (earlier-earliest (bound-earliest a) (bound-earliest b))
                 (earlier-latest (bound-latest a) (bound-latest b)))))

(defun %coarser-precision (a b)
  "The coarser of two precisions; +PRECISIONS+ runs coarse to fine."
  (if (<= (position a +precisions+) (position b +precisions+)) a b))

(defun extent-intersection (a b &key precision semantics standing)
  "The extent A and B share, or NIL when they certainly share no instant
(EXTENTS-DISJOINT-P).  Closed-interval semantics: meeting intervals
share their boundary instant, which comes back as an instant.  An
instant on either side gives an instant, narrowed to where it can lie
inside the other extent.  Fuzzy bounds combine coordinate-wise -- the
later start, the earlier end -- on the EFFECTIVE bounds (#2).
PRECISION defaults to the coarser of the two, SEMANTICS and STANDING
to A's; the library does not decide what an intersection means to a
caller.  Signals TYPE-ERROR on a non-extent, as EXTENTS-DISJOINT-P
does."
  (check-type a temporal-extent)
  (check-type b temporal-extent)
  (when (extents-disjoint-p a b)
    (return-from extent-intersection nil))
  (let ((start (%later-start (%effective-start a) (%effective-start b)))
        (end (%earlier-end (%effective-end a) (%effective-end b)))
        (precision (or precision
                       (%coarser-precision (extent-precision a)
                                           (extent-precision b))))
        (semantics (or semantics (extent-semantics a)))
        (standing (or standing (extent-standing a))))
    (cond ((or (extent-instant-p a) (extent-instant-p b))
           ;; A point somewhere in START's earliest .. END's latest;
           ;; MAKE-BOUND's reversed check is the guard that the
           ;; disjointness test above was right.
           (make-instant (make-bound (bound-earliest start)
                                     (bound-latest end))
                         :precision precision :semantics semantics
                         :standing standing))
          (t
           (ecase (bound-compare start end)
             (:= (make-instant start :precision precision
                                     :semantics semantics
                                     :standing standing))
             ((:< :ambiguous)
              (make-interval start end :precision precision
                                       :semantics semantics
                                       :standing standing))
             ;; Unreachable past the disjointness test; ECASE keeps
             ;; that claim honest rather than returning junk.
             (:> nil))))))
```

Add `#:extent-intersection` to the `;; allen` export group in `src/package.lisp` after `#:extents-intersect-p`.

- [ ] **Step 4: Run to verify they pass** — same command; expected all green, `Did N checks.` = baseline + the new checks (count them: 11 tests). If `fuzzy-bounds-combine-coordinate-wise` or the `:=` instant case fails, the fault is in the coordinate helpers, not the tests: re-derive from the docstring, do not weaken the assertion.

- [ ] **Step 5: Docs and version** — `cl-temporal-extent.asd` `:version "0.3.0"`. README: after the paragraph on `extents-disjoint-p`/the algebra (grep for "disjoint" or the "## The three ideas" section's end), add one paragraph: "**`extent-intersection`** constructs the extent two extents share, or NIL when they certainly share none — meeting closed intervals share their boundary instant, a point narrows to where it can lie in the other extent, fuzzy bounds combine coordinate-wise. Standing and semantics default to the first argument's; the library does not decide what an intersection *means*." CHANGELOG under `## [Unreleased]` → `### Added`: "**`extent-intersection`** (#N): `(extent-intersection a b &key precision semantics standing) => extent or NIL` … System version bumped to 0.3.0 so a consumer can declare the floor."

- [ ] **Step 6: Commit** (in the extent worktree):

```bash
git add src/allen.lisp src/package.lisp tests/allen-tests.lisp README.md CHANGELOG.md cl-temporal-extent.asd
git commit -m "feat(allen): extent-intersection, the constructor extents-intersect-p implied (#N)

..."
```

Record the check count in the commit body. Do not push; the PR and merge are Kevin's call, and vivace-graph's CI (which tracks cl-temporal-extent master) stays red on the rules lane until it merges — say so in the handoff.

---

### Task 2: The store schema — `def-rules-schema`, the `guard-query-text` export, the fixture

**Files:**
- Create: `rules/schema.lisp`, `tests/rules/schema-tests.lisp`
- Modify: `rules/package.lisp`, `query/guard.lisp` (append), `query/package.lisp`, `graph-db.asd` (`graph-db/rules` and `graph-db/rules-test` components; the `:cl-temporal-extent` floor in `graph-db/spacetime` to `"0.3.0"`), `tests/rules/suite.lisp`, `tests/rules/package.lisp`

**Interfaces:**
- Produces: `graph-db.rules:def-rules-schema (graph-name)`; class `graph-db.rules:rule` with constructor `make-rule` (`:graph :name :version :family :head :body :extent-policy :enabled`), accessors `rule-name rule-version rule-family rule-head rule-body rule-extent-policy rule-enabled`, predicate `rule-p`; claim family `graph-db.rules:derivation` (constructors `make-derivation-binary` / `make-derivation-unary` in `graph-db.rules`); `graph-db.query:guard-query-text (text graph) => (values vars goals)`; fixture `with-rules-graph ((g) &body)` unchanged, new `with-rules-graph-dir ((g dir) &body)`, `seed-temporal (g)`.

- [ ] **Step 1: `guard-query-text`** — append to `query/guard.lisp` after `run-guarded-prolog`, and add `#:guard-query-text` to `query/package.lisp`'s export list under a new comment `;; the compile half, for a caller that keeps the goals (GH #331)`:

```lisp
(defun guard-query-text (text graph)
  "TEXT screened, read and guarded against GRAPH exactly as
RUN-GUARDED-PROLOG does before it runs: (VALUES VARS GOALS), VARS in
first-appearance order.  For a caller that keeps the goals rather than
running them once -- graph-db/rules compiles a rule's text through
this (GH #331).  The scratch package the read interned into is deleted
before returning, so every ?variable comes back UNINTERNED; they stay
EQ across the goals, which is all the compiler asks of a variable.
Refusals signal PROLOG-GUARD-ERROR, as for RUN-GUARDED-PROLOG."
  (let ((scratch (%make-scratch-package)))
    (unwind-protect
         (%read-guarded-forms text scratch (%guard-context graph scratch))
      (delete-package scratch))))
```

- [ ] **Step 2: Failing tests for the export** — append to `tests/query/guard-tests.lisp` (package `graph-db/query-test`, fixture `with-query-graph`, schema `qt-item`/`qt-links` per `tests/query/suite.lisp`):

```lisp
;; GUARD-QUERY-TEXT (GH #331): the compile half of RUN-GUARDED-PROLOG
;; for a caller that keeps the goals.
(test guard-query-text-returns-guarded-goals-with-uninterned-vars
  (with-query-graph (g)
    (multiple-value-bind (vars goals)
        (graph-db.query:guard-query-text
         "(is-a ?p qt-item) (qt-links ?p ?f)" g)
      (is (= 2 (length vars)))
      (is (every (lambda (v) (null (symbol-package v))) vars))
      ;; The same ?P in both goals is one symbol.
      (is (eq (second (first goals)) (second (second goals))))
      ;; Heads are the engine's own symbols, not the scratch package's.
      (is (eq (find-package :graph-db) (symbol-package (first (first goals)))))
      (is (eq 'graph-db/query-test.schema::qt-item
              (third (first goals)))))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:guard-query-text "(retract ?x)" g))
    (signals graph-db.query:prolog-guard-error
      (graph-db.query:guard-query-text "(is-a ?p graph-db::vertex)" g))))
```

Check the exact form the `qt-links` edge functor takes in `tests/query/guard-tests.lisp` before copying; if the schema's edge goal is written differently there, match it. Run the query suite (`graph-db/query-test`, baseline 33/0): the new test must fail on `guard-query-text` undefined before Step 1 lands, then pass. Record the count.

- [ ] **Step 3: `rules/package.lisp`** — replace the `defpackage` (keep the header comment and its R4 warning):

```lisp
(defpackage #:graph-db.rules
  (:use #:cl)
  (:export
   ;; the store schema (spec §5, §9)
   #:def-rules-schema #:rule #:make-rule #:rule-p #:rule-name
   #:rule-version #:rule-family #:rule-head #:rule-body
   #:rule-extent-policy #:rule-enabled #:derivation
   ;; the in-image escape hatch (spec §5)
   #:def-rule #:undef-rule #:find-def-rule #:rule-spec #:rule-spec-p
   #:rule-spec-name #:rule-spec-version #:rule-spec-family
   #:rule-spec-head #:rule-spec-body #:rule-spec-extent-policy
   #:rule-spec-enabled
   ;; compiling (spec §6)
   #:compile-rule #:compiled-rule #:compiled-rule-p #:compiled-rule-spec
   #:compiled-rule-relation #:compiled-rule-reads
   #:rule-compile-error #:rule-compile-error-rule
   #:rule-compile-error-reason
   ;; running (spec §7)
   #:run-rule #:run-rules
   #:*rules-max-inferences* #:*rules-timeout* #:*rules-max-solutions*
   #:rule-report #:rule-report-p #:rule-report-rule-name
   #:rule-report-version #:rule-report-outcome #:rule-report-derived
   #:rule-report-swept #:rule-report-disjoint-premises
   #:rule-report-refusals #:rule-report-inferences
   #:rule-report-elapsed
   ;; provenance (spec §9)
   #:premises-of #:dependents-of))
```

- [ ] **Step 4: `rules/schema.lisp`**

```lisp
;;;; rules/schema.lisp -- the rule record and the derivation family, per
;;;; store (spec §5, §9; GH #331).

(in-package #:graph-db.rules)

(defmacro def-rules-schema (graph-name)
  "Declare in the store GRAPH-NAME the RULE record (spec §5) and the
DERIVATION provenance family (spec §9).  Both are per store, as every
DEF-VERTEX is, so a store that holds rules evaluates this once beside
its own schema; GRAPH-NAME is the literal keyword DEF-CLAIM-CLASSES
takes.  One RULE per NAME: the identity facet's uniqueness is the
mechanism (ruling P1); a new version is COPY, SETF RULE-VERSION, SAVE.
NAME and VERSION are canonical strings ([a-z0-9-]+), FAMILY the parent
class's name as a string, HEAD and BODY guarded Prolog text."
  (check-type graph-name keyword)
  `(progn
     (graph-db.spacetime:def-source rule ,graph-name
         ((name :type string
                :check graph-db.spacetime::canonical-relation-p)
          (version :type string
                   :check graph-db.spacetime::canonical-relation-p)
          (family :type string)
          (head :type string)
          (body :type string)
          (extent-policy :initform :premises)
          (enabled :initform t))
       :identity (:namespace :rule :key-slot name)
       :space :none
       :time :none
       :attribution :none
       :sensitivity (:class :internal)
       :registration :none
       :indexed-text (:text-fn rule-body))
     (graph-db:def-value-constraint rule extent-policy ,graph-name
       :one-of '(:premises :none) :required t
       :name rule-extent-policy)
     (graph-db.spacetime:def-claim-classes derivation ,graph-name)
     ',graph-name))
```

Recon A2 decides `::` vs `:` on `canonical-relation-p`; A3 decides whether `:type string` slots need `:initform`. The `derivation` family is non-temporal (spec §9); its claims are binary, subject and object both namespace `:claim`.

- [ ] **Step 5: ASDF** — in `graph-db.asd`, `graph-db/rules` `:components ((:file "package") (:file "facts") (:file "schema") (:file "compile") (:file "run"))` — add `schema` now and the other two in Tasks 3/4 as their files appear (a listed file that does not exist breaks the load). `graph-db/rules-test` components: `package suite facts-tests schema-tests` (+ `compile-tests`, `run-tests` later). Change `graph-db/spacetime`'s `(:version :cl-temporal-extent "0.2.0")` to `"0.3.0"` and update the S1 description string of `graph-db/rules` to "Claims as Prolog facts, and rules as versioned producers.  docs/rules.md; GH #304."

- [ ] **Step 6: Fixture** — in `tests/rules/suite.lisp`, after the `rtf-claim` declaration add:

```lisp
;; S2: the rule record and the derivation family on the test store
;; (spec §5, §9).
(graph-db.rules:def-rules-schema :graph-db-rules-test)

(defmacro with-rules-graph-dir ((g dir) &body body)
  "WITH-RULES-GRAPH with the directory bound to DIR, for a test that
closes and reopens the store."
  `(let* ((,dir (graph-db-test-scratch:make-scratch-directory
                 "graph-db-rules"))
          (,g (make-graph *graph-name* (namestring ,dir)
                          :buffer-pool-size 1000)))
     (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
       (ignore-errors (close-graph ,g)))))

(defun seed-temporal (g)
  "Three deployments of web, by producer \"deploy\": h1 twice with a gap,
h2 once.  With SEED's two version runs these are the premises the S2
temporal rules intersect.  Returns nothing."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h1"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 2 1) (ts 2026 6 30)))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h1"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 8 1) (ts 2026 9 30)))
    (make-rtt-claim-binary :graph g :subject-namespace :app
                           :subject-key "web" :relation "deployed-on"
                           :object-namespace :host :object-key "h2"
                           :producer "deploy" :standing :observed
                           :extent (interval (ts 2026 5 1)
                                             (ts 2026 5 31)))))

(defun write-rule (g &rest args)
  "A RULE record written in its own transaction; ARGS are MAKE-RULE's
keywords.  Returns the record."
  (with-transaction ((graph-db::transaction-manager g))
    (apply #'graph-db.rules:make-rule :graph g args)))
```

`tests/rules/package.lisp`: add `#:copy #:save #:open-graph #:mark-deleted` to the `:import-from #:graph-db` list. Do not `:use` `graph-db.rules`; write its symbols qualified in the tests, so a reader sees which package a name is from.

- [ ] **Step 7: Failing schema tests** — `tests/rules/schema-tests.lisp`:

```lisp
;;;; tests/rules/schema-tests.lisp -- the rule record (spec §5, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(test a-rule-record-writes-and-reads-back
  (with-rules-graph (g)
    (let ((r (write-rule g :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                         :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")))
      (is (graph-db.rules:rule-p r))
      (is (string= "web-hosts" (graph-db.rules:rule-name r)))
      (is (eq :premises (graph-db.rules:rule-extent-policy r)))
      (is (eq t (graph-db.rules:rule-enabled r)))
      ;; The identity facet: found by name through its own index.
      (is (eq (graph-db:id r)
              (graph-db:id (first (graph-db:index-lookup
                                   g 'graph-db.rules:rule
                                   '(graph-db.rules::name)
                                   "web-hosts"))))))))

(test one-rule-per-name-is-the-identity-facets-uniqueness
  "Ruling P1: a second record with the same name is a uniqueness
violation; a new version is COPY, SETF, SAVE."
  (with-rules-graph (g)
    (let ((r (write-rule g :name "web-hosts" :version "1"
                         :family "rt-claim"
                         :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                         :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")))
      (signals graph-db:unique-constraint-violation
        (write-rule g :name "web-hosts" :version "2"
                    :family "rt-claim"
                    :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                    :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")"))
      (with-transaction ((graph-db::transaction-manager g))
        (let ((c (copy r)))
          (setf (graph-db.rules:rule-version c) "2")
          (save c)))
      (is (string= "2" (graph-db.rules:rule-version
                        (first (graph-db:index-lookup
                                g 'graph-db.rules:rule
                                '(graph-db.rules::name) "web-hosts"))))))))

(test a-rule-name-and-extent-policy-are-validated-at-commit
  (with-rules-graph (g)
    (signals graph-db:value-constraint-violation
      (write-rule g :name "Not Canonical" :version "1" :family "rt-claim"
                  :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                  :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")"))
    (signals graph-db:value-constraint-violation
      (write-rule g :name "ok" :version "1" :family "rt-claim"
                  :extent-policy :sometimes
                  :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                  :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")"))
    ;; Control: the canonical shape commits.
    (finishes
      (write-rule g :name "ok" :version "1" :family "rt-claim"
                  :extent-policy :none
                  :head "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                  :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")"))))

(test the-derivation-family-is-declared-on-the-store
  (with-rules-graph (g)
    (is (claim-family 'graph-db.rules:derivation))
    (is-false (claim-family-temporal-p
               (claim-family 'graph-db.rules:derivation)))
    (with-transaction ((graph-db::transaction-manager g))
      (graph-db.rules::make-derivation-binary
       :graph g :subject-namespace :claim :subject-key "a|b|c"
       :relation "derived-from" :object-namespace :claim
       :object-key "d|e|f" :producer "rule/x" :standing :inferred))
    (is (= 1 (length (claims-touching g 'graph-db.rules:derivation
                                      :claim "d|e|f" :role :object))))))
```

Note: until Task 3 lands its commit validator, these rules are not compiled at write. That is fine here; Task 3's tests cover it. The S1 tests must keep passing unchanged: `seed` is untouched and the new schema adds no claims to `rt-claim`/`rtt-claim`.

- [ ] **Step 8: Run** — rules suite: expected red on the four new tests before Steps 3–6, green after; `Did N checks.` = 67 + the new checks. Query suite green with its new test. Diffstat check on `facts-tests.lisp` (untouched: 0 removed lines).

- [ ] **Step 9: Commit**

```bash
git add rules/package.lisp rules/schema.lisp query/guard.lisp query/package.lisp graph-db.asd tests/rules tests/query/guard-tests.lisp
git commit -m "feat(rules): the rule record and the derivation family, per store (#331)"
```

Docs travel with code: add to `docs/rules.md` a one-paragraph "Slice 2" placeholder section naming `def-rules-schema` and the record's slots — Task 5 finishes it. The push hook needs a doc change per source push, not per commit, but keeping each commit self-describing is cheaper than remembering later.

---

### Task 3: `compile-rule` — the spec, the escape hatch, the guard, the cycle check, the write validator

**Files:**
- Create: `rules/compile.lisp`, `tests/rules/compile-tests.lisp`
- Modify: `graph-db.asd` (add the two files), `docs/rules.md` (a "Compiling" subsection)

**Interfaces:**
- Consumes: `graph-db.query:guard-query-text`, `graph-db.rules:rule` and its accessors, `graph-db.spacetime:claim-family`.
- Produces:
  - `(defstruct rule-spec name version family head body extent-policy enabled source)` — `family` a lowercase string; `source` `:stored` or `:def-rule`.
  - `(def-rule name &key version family head body (extent-policy :premises) (enabled t))` → registers a `rule-spec` in `*def-rules*`; `(undef-rule name)`; `(find-def-rule name)`.
  - `(rule-spec-of thing)` — a `rule` vertex → spec; a spec → itself.
  - `(compile-rule graph rule &key others) => compiled-rule`, signals `rule-compile-error`.
  - `(defstruct compiled-rule spec family relation head-c head-sns head-skey head-ons head-okey unary-p vars premise-vars goals reads)` — `reads` a list of relation strings or `:any`.
  - `(rules-in-scope graph &key view) => list of rule-spec` — enabled stored rules plus every def-rule (P8 filtering is `run-rules`' job, not this one's: a def-rule the store cannot run still constrains the cycle graph).
  - `%validate-rule-writes (tx graph)` on `graph-db:*commit-validators*`.

- [ ] **Step 1: Failing tests** — `tests/rules/compile-tests.lisp`:

```lisp
;;;; tests/rules/compile-tests.lisp -- compile-rule (spec §6, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defparameter *web-hosts-head*
  "(claim ?c rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")
(defparameter *web-hosts-body*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")

(defun spec (&rest args)
  "A def-rule-shaped RULE-SPEC without registering it."
  (apply #'graph-db.rules::%make-rule-spec :source :def-rule args))

(test compile-rule-reads-the-head-and-the-body
  (with-rules-graph (g)
    (let ((c (graph-db.rules:compile-rule
              g (spec :name "web-hosts" :version "1" :family "rt-claim"
                      :head *web-hosts-head* :body *web-hosts-body*))))
      (is (graph-db.rules:compiled-rule-p c))
      (is (string= "hosted-on" (graph-db.rules:compiled-rule-relation c)))
      (is (equal '("runs") (graph-db.rules:compiled-rule-reads c)))
      (is (eq (claim-family 'rt-claim)
              (graph-db.rules::compiled-rule-family c)))
      ;; One premise variable, ?P; the head's ?H is a body variable.
      (is (= 1 (length (graph-db.rules::compiled-rule-premise-vars c))))
      (is (member (graph-db.rules::compiled-rule-head-okey c)
                  (graph-db.rules::compiled-rule-vars c)))
      ;; A literal namespace in the head is interned now, not at run.
      (is (eq :app (graph-db.rules::compiled-rule-head-sns c))))))

(defmacro refuses (reason-substring &rest spec-args)
  "The compile is refused and the reason names REASON-SUBSTRING."
  `(with-rules-graph (g)
     (let ((c (handler-case
                  (progn (graph-db.rules:compile-rule g (spec ,@spec-args))
                         nil)
                (graph-db.rules:rule-compile-error (c) c))))
       (is-true c "compiled when it should have been refused")
       (when c
         (is (search ,reason-substring
                     (graph-db.rules:rule-compile-error-reason c))
             "reason ~S does not mention ~S"
             (graph-db.rules:rule-compile-error-reason c)
             ,reason-substring)))))

(test the-head-must-be-one-claim-pattern
  (refuses "exactly one" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h) (claim-current ?c)"
           :body *web-hosts-body*)
  (refuses "claim/7" :name "r" :version "1" :family "rt-claim"
           :head "(claim-producer ?c \"p\")" :body *web-hosts-body*)
  (refuses "?c" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
           :body *web-hosts-body*)
  (refuses "family" :name "r" :version "1" :family "rtt-claim"
           :head *web-hosts-head* :body *web-hosts-body*)
  (refuses "relation" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"app\" \"web\" ?r \"host\" ?h)"
           :body *web-hosts-body*)
  (refuses "?z" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?z)"
           :body *web-hosts-body*)
  (refuses "object" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" nil)"
           :body *web-hosts-body*)
  (refuses "namespace" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"App\" \"web\" \"x\" \"host\" ?h)"
           :body *web-hosts-body*))

(test the-body-goes-through-the-guard
  ;; An effecting functor, a package-qualified name, an unknown name.
  (refuses "retract" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head*
           :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\") (retract ?p)")
  (refuses "package-qualified" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head*
           :body "(claim ?p graph-db::rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")
  (refuses "empty" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body "   "))

(test a-rule-that-reads-its-own-relation-is-refused
  (refuses "cycle" :name "r" :version "1" :family "rt-claim"
           :head "(claim ?c rt-claim \"app\" \"web\" \"runs\" \"host\" ?h)"
           :body *web-hosts-body*)
  ;; An unbound relation reads everything, its own included (P6).
  (refuses "bind the relation" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head*
           :body "(claim ?p rt-claim \"host\" \"h1\" ?r \"app\" ?a)"))

(test a-cycle-across-two-rules-is-refused-and-named
  (with-rules-graph (g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
                :body "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")
    (let ((c (handler-case
                 (progn
                   (write-rule g :name "b" :version "1" :family "rt-claim"
                               :head "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"
                               :body "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
                   nil)
               (graph-db.rules:rule-compile-error (c) c))))
      (is-true c)
      (when c
        (is (search "x" (graph-db.rules:rule-compile-error-reason c)))
        (is (search "y" (graph-db.rules:rule-compile-error-reason c)))))
    ;; The refused write left nothing behind.
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "b")))
    ;; Control: b reading a third relation is not a cycle.
    (finishes
      (write-rule g :name "b" :version "1" :family "rt-claim"
                  :head "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"
                  :body "(claim ?p rt-claim \"app\" \"web\" \"z\" \"host\" ?h)"))))

(test a-def-rule-joins-the-cycle-graph-and-collides-by-name
  (with-rules-graph (g)
    (graph-db.rules:def-rule "b" :version "1" :family rt-claim
      :head "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"
      :body "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
    (unwind-protect
         (progn
           (is (graph-db.rules:rule-spec-p (graph-db.rules:find-def-rule "b")))
           (signals graph-db.rules:rule-compile-error
             (write-rule g :name "a" :version "1" :family "rt-claim"
                         :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
                         :body "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"))
           ;; Same name as a def-rule: a collision, whatever the text.
           (signals graph-db.rules:rule-compile-error
             (write-rule g :name "b" :version "1" :family "rt-claim"
                         :head *web-hosts-head* :body *web-hosts-body*)))
      (graph-db.rules:undef-rule "b"))
    (is (null (graph-db.rules:find-def-rule "b")))
    ;; Control: with the def-rule gone both writes commit.
    (finishes
      (write-rule g :name "a" :version "1" :family "rt-claim"
                  :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
                  :body "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"))))

(test a-stored-rule-that-does-not-compile-is-refused-at-write
  (with-rules-graph (g)
    (signals graph-db.rules:rule-compile-error
      (write-rule g :name "bad" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body "(retract ?p)"))
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "bad")))
    ;; A disabled rule still has to compile (spec §6: compiled, not run).
    (signals graph-db.rules:rule-compile-error
      (write-rule g :name "bad" :version "1" :family "rt-claim"
                  :enabled nil :head *web-hosts-head* :body "(retract ?p)"))))

(test claim-producer-generators-move-to-the-front
  "Ruling P5: (claim-producer ?v \"p\") is a generator and runs first."
  (with-rules-graph (g)
    (let* ((c (graph-db.rules:compile-rule
               g (spec :name "r" :version "1" :family "rt-claim"
                       :head "(claim ?c rt-claim \"host\" ?h \"scanned\" \"app\" ?a)"
                       :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a) (claim-producer ?p \"scan-a\")")))
           (goals (graph-db.rules::compiled-rule-goals c)))
      (is (string= "CLAIM-PRODUCER" (symbol-name (first (first goals)))))
      (is (string= "CLAIM" (symbol-name (first (second goals)))))
      ;; The filter direction is left where it was: a bound ?P is not
      ;; a generator.
      (let* ((d (graph-db.rules:compile-rule
                 g (spec :name "r" :version "1" :family "rt-claim"
                         :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
                         :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\") (claim-producer ?p ?who)")))
             (goals (graph-db.rules::compiled-rule-goals d)))
        (is (string= "CLAIM" (symbol-name (first (first goals)))))))))
```

- [ ] **Step 2: Run to verify they fail** — `compile-rule` undefined. Record the count.

- [ ] **Step 3: `rules/compile.lisp`**

```lisp
;;;; rules/compile.lisp -- a rule as text, and text as a compiled rule
;;;; (spec §5-§6, GH #331).  Every spacetime and engine symbol is written
;;;; qualified: see package.lisp's header for why nothing is imported.

(in-package #:graph-db.rules)

(defstruct (rule-spec (:constructor %make-rule-spec))
  "One rule as text: the slots of a RULE record, whichever way it came
in.  FAMILY is the parent class's name, lowercase; SOURCE is :STORED or
:DEF-RULE."
  name version family head body (extent-policy :premises) (enabled t)
  source)

(defvar *def-rules* (make-hash-table :test 'equal)
  "DEF-RULE's registry, name -> RULE-SPEC: rules that live in the image
rather than in a store (spec §5).  Image-wide, so RUN-RULES filters it
by the store's families (ruling P8).")

(defun %family-string (family)
  "FAMILY -- a symbol or a string -- as the lowercase name a RULE stores."
  (string-downcase (string family)))

(defun %register-def-rule (name &key version family head body
                                     (extent-policy :premises)
                                     (enabled t))
  (check-type name string)
  (check-type version string)
  (check-type head string)
  (check-type body string)
  (setf (gethash name *def-rules*)
        (%make-rule-spec :name name :version version
                         :family (%family-string family)
                         :head head :body body
                         :extent-policy extent-policy :enabled enabled
                         :source :def-rule))
  name)

(defmacro def-rule (name &key version family head body
                              (extent-policy :premises) (enabled t))
  "Register a rule in the image, not in a store (spec §5): the same
producer rule/NAME, the same compile and RUN-RULE as a stored rule,
without a record.  FAMILY is the parent class symbol, unevaluated.
Compiled per store when run, not here -- the cycle check needs the
store's other rules.  Returns NAME."
  `(%register-def-rule ,name :version ,version :family ',family
                       :head ,head :body ,body
                       :extent-policy ,extent-policy :enabled ,enabled))

(defun undef-rule (name)
  "Forget the DEF-RULE NAME; T when there was one."
  (and (remhash name *def-rules*) t))

(defun find-def-rule (name)
  "The DEF-RULE NAME's RULE-SPEC, or NIL."
  (values (gethash name *def-rules*)))

(defun rule-spec-of (thing)
  "THING as a RULE-SPEC: a RULE record is read into one, a spec passes."
  (etypecase thing
    (rule-spec thing)
    (rule (%make-rule-spec :name (rule-name thing)
                           :version (rule-version thing)
                           :family (%family-string (rule-family thing))
                           :head (rule-head thing) :body (rule-body thing)
                           :extent-policy (rule-extent-policy thing)
                           :enabled (rule-enabled thing)
                           :source :stored))))

(define-condition rule-compile-error (graph-db:constraint-violation)
  ((rule :initarg :rule :reader rule-compile-error-rule)
   (reason :initarg :reason :reader rule-compile-error-reason))
  (:report (lambda (c s)
             (format s "Rule ~A does not compile: ~A"
                     (rule-compile-error-rule c)
                     (rule-compile-error-reason c))))
  (:documentation "A rule the compiler refused (spec §6).  A
CONSTRAINT-VIOLATION, because a RULE write is refused at commit with it
(ruling P3)."))

(defun %refuse (spec format-string &rest args)
  (error 'rule-compile-error :rule (rule-spec-name spec)
                             :reason (apply #'format nil format-string args)))

(defstruct (compiled-rule (:constructor %make-compiled-rule))
  "A rule ready to run: the guarded goals and what the head derives.
HEAD-* are the head's argument terms -- a keyword (namespace), a string
(key or relation), NIL, or a body variable.  VARS is SELECT's variable
list, PREMISE-VARS the ?c of every body CLAIM/7 goal, READS the
relations the body reads or :ANY."
  spec family relation
  head-c head-sns head-skey head-ons head-okey unary-p
  vars premise-vars goals reads)

;;; Reading the text

(defun %variable-p (x)
  (graph-db::variable-p x))

(defun %engine-goal-p (goal name arity)
  "GOAL is a call of the engine functor NAME/ARITY, by the canonical
symbol the guard rebuilds a head into."
  (and (consp goal)
       (symbolp (first goal))
       (eq (symbol-package (first goal)) (find-package :graph-db))
       (string= (symbol-name (first goal)) name)
       (= arity (1- (length goal)))))

(defun %guard (spec graph)
  "HEAD then BODY through the guard as one text, so a variable shared
between them reads as one symbol: (VALUES VARS GOALS)."
  (handler-case
      (graph-db.query:guard-query-text
       (format nil "~A~%~A" (rule-spec-head spec) (rule-spec-body spec))
       graph)
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "~A" (graph-db.query:prolog-guard-error-reason c)))))

(defun %head-goal-count (spec graph)
  "How many goals the HEAD text alone is -- the guard refuses an empty
text, which here is a refusal in its own words."
  (handler-case
      (length (nth-value 1 (graph-db.query:guard-query-text
                            (rule-spec-head spec) graph)))
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "head: ~A" (graph-db.query:prolog-guard-error-reason c)))))

(defun %body-variables (goals)
  "Every ?variable in GOALS, once each."
  (let ((vars '()))
    (labels ((walk (x)
               (cond ((%variable-p x) (pushnew x vars))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk goals))
    vars))

(defun %head-namespace (spec term body-vars what)
  "A head namespace term: a canonical string is interned as its keyword
now (rules are validated content, so this growth is bounded by the
rules that compile); a body variable passes; anything else refuses."
  (cond ((and (stringp term)
              (graph-db.spacetime::canonical-relation-p term))
         (intern (string-upcase term) :keyword))
        ((stringp term)
         (%refuse spec "~A namespace ~S is not canonical ([a-z0-9-]+)"
                  what term))
        ((and (%variable-p term) (member term body-vars)) term)
        ((%variable-p term)
         (%refuse spec "~A namespace ~A is not bound by the body"
                  what (symbol-name term)))
        (t (%refuse spec "~A namespace must be a string or a body ~
variable, not ~S" what term))))

(defun %head-key (spec term body-vars what)
  (cond ((stringp term) term)
        ((and (%variable-p term) (member term body-vars)) term)
        ((%variable-p term)
         (%refuse spec "~A key ~A is not bound by the body"
                  what (symbol-name term)))
        (t (%refuse spec "~A key must be a string or a body variable, ~
not ~S" what term))))

(defun %parse-head (spec head body-vars)
  "The head's seven arguments checked against spec §6; returns a plist
of the COMPILED-RULE head slots."
  (unless (%engine-goal-p head "CLAIM" 7)
    (%refuse spec "the head must be a claim/7 pattern, not ~S"
             (if (consp head) (string-downcase (string (first head))) head)))
  (destructuring-bind (?c fam sns skey rel ons okey) (rest head)
    (unless (%variable-p ?c)
      (%refuse spec "the head's ?c must be an unbound variable, not ~S" ?c))
    (when (member ?c body-vars)
      (%refuse spec "the head's ?c ~A must not appear in the body"
               (symbol-name ?c)))
    (let ((family (handler-case (graph-db.spacetime:claim-family fam)
                    (graph-db.spacetime:unknown-claim-family ()
                      (%refuse spec "~S is not a claim family" fam)))))
      (unless (string-equal (symbol-name fam) (rule-spec-family spec))
        (%refuse spec "the head's family ~(~A~) is not the rule's ~
family ~A" fam (rule-spec-family spec)))
      (unless (and (stringp rel)
                   (graph-db.spacetime::canonical-relation-p rel))
        (%refuse spec "the head's relation must be a canonical string, ~
not ~S" rel))
      (let ((unary (and (null ons) (null okey))))
        (when (and (not unary) (or (null ons) (null okey)))
          (%refuse spec "the head's object pair must be both NIL ~
(a unary claim) or both given"))
        (list :family family :relation rel :head-c ?c
              :head-sns (%head-namespace spec sns body-vars "subject")
              :head-skey (%head-key spec skey body-vars "subject")
              :head-ons (and (not unary)
                             (%head-namespace spec ons body-vars "object"))
              :head-okey (and (not unary)
                              (%head-key spec okey body-vars "object"))
              :unary-p unary)))))

(defun %generator-goal-p (goal)
  "(claim-producer ?v \"p\"): a generator, to run first (ruling P5)."
  (and (%engine-goal-p goal "CLAIM-PRODUCER" 2)
       (%variable-p (second goal))
       (stringp (third goal))))

(defun %order-body (goals)
  (append (remove-if-not #'%generator-goal-p goals)
          (remove-if #'%generator-goal-p goals)))

(defun %body-reads (goals)
  "The relations the body's CLAIM/7 goals read, or :ANY when one of
them leaves the relation unbound (ruling P6)."
  (let ((reads '()))
    (dolist (goal goals (nreverse reads))
      (when (%engine-goal-p goal "CLAIM" 7)
        (let ((rel (fifth goal)))
          (if (stringp rel)
              (pushnew rel reads :test #'string=)
              (return-from %body-reads :any)))))))

(defun %premise-vars (goals)
  (loop for goal in goals
        when (and (%engine-goal-p goal "CLAIM" 7)
                  (%variable-p (second goal)))
          collect (second goal) into vars
        finally (return (remove-duplicates vars))))

;;; The rule set a compile is checked against

(defun %stored-rules (graph &key view)
  "Every RULE record in GRAPH, as specs.  With VIEW (a commit view),
records the transaction writes replace or remove their committed
version, so the set is the store as it will be after the commit."
  (let* ((committed (graph-db:map-vertices #'identity graph
                                           :vertex-type 'rule
                                           :collect-p t))
         (nodes (if view
                    (loop for r in committed
                          for n = (graph-db:view-node view (graph-db:id r))
                          when n collect n)
                    committed)))
    (when view
      (dolist (w (graph-db:view-writes view))
        (let ((n (graph-db:view-node view (graph-db:id w))))
          (when (and n (typep n 'rule)
                     (null (graph-db:view-old-node view n))
                     (not (find (graph-db:id n) nodes
                                :key #'graph-db:id :test #'equalp)))
            (push n nodes)))))
    (mapcar #'rule-spec-of
            (remove-if #'graph-db:deleted-p nodes))))

(defun rules-in-scope (graph &key view)
  "The specs a compile checks a rule against (spec §6): every enabled
stored rule of GRAPH -- through VIEW when a commit is in flight -- plus
every DEF-RULE.  A def-rule the store cannot run still constrains the
cycle graph; RUN-RULES is what filters by family (ruling P8)."
  (append (remove-if-not #'rule-spec-enabled (%stored-rules graph :view view))
          (loop for spec being the hash-values of *def-rules*
                when (rule-spec-enabled spec) collect spec)))

(defun %edges (spec graph)
  "SPEC's (head-relation . reads) for the cycle graph, or NIL when the
spec's text does not guard -- such a rule cannot run and constrains
nothing."
  (handler-case
      (multiple-value-bind (vars goals) (%guard spec graph)
        (declare (ignore vars))
        (let ((head (first goals)))
          (when (%engine-goal-p head "CLAIM" 7)
            (cons (fifth head) (%body-reads (rest goals))))))
    (rule-compile-error () nil)))

(defun %check-cycle (spec relation reads graph others)
  "Refuse when SPEC's head RELATION reaches itself through READS and
OTHERS' edges (spec §6), naming the path.  :ANY reads every head
relation in scope, the rule's own included (ruling P6)."
  (let* ((edges (list (cons relation reads)))
         (heads (list relation)))
    (dolist (o others)
      (let ((e (%edges o graph)))
        (when e
          (push e edges)
          (pushnew (car e) heads :test #'string=))))
    (labels ((successors (rel)
               (let ((out '()))
                 (dolist (e edges out)
                   (when (string= (car e) rel)
                     (setf out (union out (if (eq (cdr e) :any)
                                              heads
                                              (cdr e))
                                      :test #'string=))))))
             (path-to (target from seen)
               (dolist (next (successors from))
                 (cond ((string= next target)
                        (return (list next)))
                       ((not (member next seen :test #'string=))
                        (let ((p (path-to target next (cons next seen))))
                          (when p (return (cons next p)))))))))
      (when (eq reads :any)
        (%refuse spec "a body claim/7 goal leaves its relation unbound, ~
so the rule reads every relation, its own ~S included: bind the relation"
                 relation))
      (let ((path (path-to relation relation (list relation))))
        (when path
          (%refuse spec "deriving ~S closes a cycle: ~{~A~^ -> ~}"
                   relation (cons relation path)))))))

(defun compile-rule (graph rule &key (others nil others-p))
  "RULE (a RULE record or a RULE-SPEC) compiled against GRAPH's schema
and the rules in scope (spec §6): head and body through the guard, the
head checked as one claim/7 pattern, claim-producer generators moved
to the front (P5), recursion refused over every rule in OTHERS --
default RULES-IN-SCOPE minus this one -- with the cycle named.
=> COMPILED-RULE; signals RULE-COMPILE-ERROR.  A name held by both a
stored rule and a DEF-RULE is a collision, refused."
  (let* ((spec (rule-spec-of rule))
         (name (rule-spec-name spec))
         (others (remove name
                         (if others-p others (rules-in-scope graph))
                         :key #'rule-spec-name :test #'string=)))
    (when (and (eq (rule-spec-source spec) :stored) (find-def-rule name))
      (%refuse spec "a def-rule of the same name exists in the image"))
    (when (and (eq (rule-spec-source spec) :def-rule)
               (find name (%stored-rules graph)
                     :key #'rule-spec-name :test #'string=))
      (%refuse spec "a stored rule of the same name exists in the store"))
    (unless (= 1 (%head-goal-count spec graph))
      (%refuse spec "the head must be exactly one claim/7 pattern"))
    (multiple-value-bind (vars goals) (%guard spec graph)
      (declare (ignore vars))
      (let* ((head (first goals))
             (body (%order-body (rest goals)))
             (body-vars (%body-variables body)))
        (when (null body)
          (%refuse spec "the body is empty"))
        (let* ((parsed (%parse-head spec head body-vars))
               (reads (%body-reads body))
               (premise-vars (%premise-vars body))
               (head-vars (remove-if-not #'%variable-p
                                         (list (getf parsed :head-sns)
                                               (getf parsed :head-skey)
                                               (getf parsed :head-ons)
                                               (getf parsed :head-okey)))))
          (%check-cycle spec (getf parsed :relation) reads graph others)
          (apply #'%make-compiled-rule
                 :spec spec
                 :vars (remove-duplicates (append head-vars premise-vars))
                 :premise-vars premise-vars
                 :goals body :reads reads
                 parsed))))))

;;; The write validator (ruling P3)

(defun %validate-rule-writes (tx graph)
  "GRAPH-DB:*COMMIT-VALIDATORS*: every RULE record this transaction
writes compiles against the store as it will be after the commit, so
a rule that cannot run is never stored (spec §6, ruling P3)."
  (let ((written (loop for w in (graph-db:writes tx)
                       for node = (graph-db::node w)
                       when (and (typep node 'rule)
                                 (not (graph-db:deleted-p node)))
                         collect node)))
    (when written
      (let ((scope (rules-in-scope
                    graph :view (graph-db:make-commit-view graph tx))))
        (dolist (r written)
          (compile-rule graph r :others scope))))))

(pushnew '%validate-rule-writes graph-db:*commit-validators*)
```

Recon items that bear on this file: A1 (the condition superclass), A8 (`view-writes`, `view-old-node`, `view-node`, `writes`, `node`), A10–A11 (variables and canonical heads), A13 (the validator region). `%stored-rules`' `map-vertices` keeps `:record-reads` at its default: the rule set is small and a commit racing a rule edit *should* conflict.

- [ ] **Step 4: Run to verify they pass** — rules suite green; record the count. If `the-head-must-be-one-claim-pattern`'s `"?c"` case fails because the guard refuses a bound-`?c` head differently, read the refusal text and match the test's substring to the real message — the test names the mechanism, the wording is the implementation's.

- [ ] **Step 5: Docs** — `docs/rules.md`: a "Compiling a rule" subsection: the head shape rules as a list, "the body is guarded exactly as free text is", the generator reorder (P5), the recursion refusal and the unbound-relation rule (P6), the name collision, and that a `rule` write that does not compile is refused at commit with `rule-compile-error` (P3). Add `rule-compile-error` to the conditions a caller sees.

- [ ] **Step 6: Commit**

```bash
git add rules/compile.lisp tests/rules/compile-tests.lisp graph-db.asd docs/rules.md
git commit -m "feat(rules): compile-rule through the guard, recursion refused, bad rules refused at write (#331)"
```

---

### Task 4: `run-rule`, `run-rules`, the report, provenance

**Files:**
- Create: `rules/run.lisp`, `tests/rules/run-tests.lisp`
- Modify: `graph-db.asd` (add both files), `docs/rules.md` ("Running" and "Provenance" subsections)

**Interfaces:**
- Consumes: `compile-rule`, `compiled-rule-*`, `rule-spec-*`, `rules-in-scope`, `find-def-rule`, `%stored-rules`; `temporal-extent:extent-intersection`; `graph-db::run-query-goals`; `graph-db.spacetime:delete-claims-by-producer`, `claim-identity-key`, `split-claim-identity-key`, `claims-touching`, `claim-extent`, `extent->sexp`, `extent-sexp-start-key`, `claim-current-p`; `graph-db::%namespace-keyword` (S1, `rules/facts.lisp`).
- Produces: `*rules-max-inferences*`, `*rules-timeout*`, `*rules-max-solutions*`; `(defstruct rule-report rule-name version outcome derived swept disjoint-premises refusals inferences elapsed)`; `(run-rule graph rule) => rule-report`; `(run-rules graph) => list`; `(premises-of graph claim) => claims`; `(dependents-of graph claim &key current) => claims`; `(rule-producer name) => "rule/NAME"`.

- [ ] **Step 1: Failing tests** — `tests/rules/run-tests.lisp`:

```lisp
;;;; tests/rules/run-tests.lisp -- run-rule, provenance, validity
;;;; (spec §7-§9, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defun derived (g family name)
  "The live claims rule NAME derived into FAMILY, current only."
  (remove-if-not #'claim-current-p
                 (claims-by-producer g family
                                     (graph-db.rules::rule-producer name))))

(defun write-web-hosts (g &rest args)
  (apply #'write-rule g :name "web-hosts" :version "1" :family "rt-claim"
         :head *web-hosts-head* :body *web-hosts-body* args))

(test run-rule-derives-claims-that-name-the-rule
  (with-rules-graph (g)
    (seed g)
    (let* ((r (write-web-hosts g))
           (report (graph-db.rules:run-rule g r)))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 2 (graph-db.rules:rule-report-derived report)))
      (is (= 0 (graph-db.rules:rule-report-swept report)))
      (is (string= "web-hosts" (graph-db.rules:rule-report-rule-name report)))
      (let ((claims (derived g 'rt-claim "web-hosts")))
        (is (= 2 (length claims)))
        (is (equal '("h1" "h2")
                   (sort (mapcar #'claim-object-key claims) #'string<)))
        (dolist (c claims)
          (is (string= "rule/web-hosts" (claim-producer c)))
          (is (string= "1" (claim-rule-version c)))
          (is (eq :inferred (claim-standing c)))
          (is (string= "hosted-on" (claim-relation c)))
          (is (eq :app (claim-subject-namespace c)))
          (is (eq :host (claim-object-namespace c)))
          ;; rt-claim premises carry no extent, so neither does this.
          (is (null (claim-extent c)))))
      ;; The derived claims are Prolog facts like any other.
      (is (= 2 (select (:count t :max-inferences 1000) (?h)
                 (claim ?c rt-claim "app" "web" "hosted-on" "host" ?h)))))))

(test rerunning-sweeps-the-previous-derivation
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      ;; A premise goes away; the rerun must not keep h2.
      (with-transaction ((graph-db::transaction-manager g))
        (mark-deleted (first (claims-touching g 'rt-claim :host "h2"
                                              :role :subject
                                              :relation "runs"))))
      (let ((report (graph-db.rules:run-rule g r)))
        (is (= 2 (graph-db.rules:rule-report-swept report)))
        (is (= 1 (graph-db.rules:rule-report-derived report)))
        (is (equal '("h1") (mapcar #'claim-object-key
                                   (derived g 'rt-claim "web-hosts"))))
        ;; The old derivation records went with the old claims.
        (is (= 1 (length (claims-by-producer
                          g 'graph-db.rules:derivation
                          "rule/web-hosts"))))))))

(test a-new-version-leaves-no-old-version-claim
  (with-rules-graph (g)
    (seed g)
    (let ((r (write-web-hosts g)))
      (graph-db.rules:run-rule g r)
      (with-transaction ((graph-db::transaction-manager g))
        (let ((c (copy r)))
          (setf (graph-db.rules:rule-version c) "2")
          (save c)))
      (let ((report (graph-db.rules:run-rule g "web-hosts")))
        (is (string= "2" (graph-db.rules:rule-report-version report)))
        (is (= 2 (graph-db.rules:rule-report-derived report))))
      (let ((claims (derived g 'rt-claim "web-hosts")))
        (is (= 2 (length claims)))
        (is (every (lambda (c) (string= "2" (claim-rule-version c))) claims))
        (is (zerop (select-count (?c) (claim-producer ?c "rule/web-hosts")
                                      (claim-rule-version ?c "1")
                                      (claim-current ?c))))))))

(test provenance-names-the-premises-and-dependents-are-findable
  (with-rules-graph (g)
    (seed g)
    (graph-db.rules:run-rule g (write-web-hosts g))
    (let* ((d (find "h1" (derived g 'rt-claim "web-hosts")
                    :key #'claim-object-key :test #'string=))
           (premise (first (claims-touching g 'rt-claim :host "h1"
                                            :role :subject
                                            :relation "runs")))
           (premises (graph-db.rules:premises-of g d)))
      ;; h1 runs web is the one premise of "web hosted-on h1".
      (is (= 1 (length premises)))
      (is (string= (claim-identity-key premise)
                   (claim-identity-key (first premises))))
      (is (string= "web" (claim-object-key (first premises))))
      ;; The derivation record itself: (:claim derived) derived-from
      ;; (:claim premise), by the rule.
      (let ((recs (claims-touching g 'graph-db.rules:derivation
                                   :claim (claim-identity-key d)
                                   :role :subject)))
        (is (= 1 (length recs)))
        (is (string= "derived-from" (claim-relation (first recs))))
        (is (string= "rule/web-hosts" (claim-producer (first recs))))
        (is (string= "1" (claim-rule-version (first recs)))))
      ;; Retract the premise: its dependents are still findable, and
      ;; nothing was re-derived.
      (retract-claim premise)
      (let ((deps (graph-db.rules:dependents-of g premise)))
        (is (= 1 (length deps)))
        (is (string= (claim-identity-key d)
                     (claim-identity-key (first deps))))
        (is-true (claim-current-p (first deps))))
      ;; Control: a claim nothing derived from has no dependents.
      (is (null (graph-db.rules:dependents-of g d))))))

(defparameter *host-version-head*
  "(claim ?c rtt-claim \"host\" ?h \"runs-version\" \"ver\" ?v)")
(defparameter *host-version-body*
  "(claim ?d rtt-claim \"app\" \"web\" \"deployed-on\" \"host\" ?h)
   (claim ?r rtt-claim \"app\" \"web\" \"version\" \"ver\" ?v)")

(defun write-host-version (g)
  (write-rule g :name "host-version" :version "1" :family "rtt-claim"
              :head *host-version-head* :body *host-version-body*))

(defun claim-bounds (c)
  "The exact start and end timestamps of C's extent."
  (let ((e (claim-extent c)))
    (values (bound-earliest (extent-start e))
            (bound-latest (extent-end e)))))

(test validity-is-the-intersection-of-the-premises
  "Spec §8: h1 is deployed Feb-Jun and Aug-Sep; web is version 1
Jan-Mar and 2 Apr-Dec; h2 is deployed in May only."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((report (graph-db.rules:run-rule g (write-host-version g))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      ;; (h1,1)=[Feb1,Mar31] (h1,2)=[Apr1,Jun30] (h1,2)=[Aug1,Sep30]
      ;; (h2,2)=[May1,May31]; (h2,1) never held at once.
      (is (= 4 (graph-db.rules:rule-report-derived report)))
      (is (= 1 (graph-db.rules:rule-report-disjoint-premises report)))
      (let ((claims (derived g 'rtt-claim "host-version")))
        (is (= 4 (length claims)))
        (flet ((run (host ver start)
                 (find-if (lambda (c)
                            (and (string= host (claim-subject-key c))
                                 (string= ver (claim-object-key c))
                                 (local-time:timestamp=
                                  start (claim-bounds c))))
                          claims)))
          (let ((h1v1 (run "h1" "1" (ts 2026 2 1))))
            (is-true h1v1)
            (when h1v1
              (multiple-value-bind (s e) (claim-bounds h1v1)
                (is (local-time:timestamp= (ts 2026 2 1) s))
                (is (local-time:timestamp= (ts 2026 3 31) e)))
              (is (eq :validity (extent-semantics (claim-extent h1v1))))
              (is (eq :inferred (extent-standing (claim-extent h1v1))))
              ;; Two premises: the deployment and the version run.
              (is (= 2 (length (graph-db.rules:premises-of g h1v1))))))
          (is-true (run "h1" "2" (ts 2026 4 1)))
          (is-true (run "h1" "2" (ts 2026 8 1)))
          (let ((h2v2 (run "h2" "2" (ts 2026 5 1))))
            (is-true h2v2)
            (when h2v2
              (is (local-time:timestamp= (ts 2026 5 31)
                                         (nth-value 1 (claim-bounds h2v2))))))
          (is (null (find "1" (remove "h1" claims :key #'claim-subject-key
                                                  :test #'string=)
                          :key #'claim-object-key :test #'string=))))
        ;; The derived claims answer CLAIM-VALID-AT like any temporal claim.
        (is (equal '("1")
                   (select-flat (?v) (claim ?c rtt-claim "host" "h1"
                                            "runs-version" "ver" ?v)
                                     (claim-valid-at ?c "2026-03-01T00:00:00Z"))))))))

(test extent-policy-none-derives-without-an-extent
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    ;; rt-claim is not temporal, so a claim without an extent is legal
    ;; there; the same premises, :none, no extent.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "hv-flat" :version "1"
                                 :family "rt-claim" :extent-policy :none
                                 :head "(claim ?c rt-claim \"host\" ?h \"ran-version\" \"ver\" ?v)"
                                 :body *host-version-body*))))
      (is (= 4 (graph-db.rules:rule-report-derived report)))
      (is (= 0 (graph-db.rules:rule-report-disjoint-premises report)))
      (is (every (lambda (c) (null (claim-extent c)))
                 (derived g 'rt-claim "hv-flat"))))
    ;; Control: :premises on the same non-temporal family attaches the
    ;; intersection and drops the disjoint pair (ruling P7).
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "hv-when" :version "1"
                                 :family "rt-claim"
                                 :head "(claim ?c rt-claim \"host\" ?h \"ran-version-when\" \"ver\" ?v)"
                                 :body *host-version-body*))))
      ;; Identity ignores the extent here, so (h1,2) collapses to one.
      (is (= 3 (graph-db.rules:rule-report-derived report)))
      (is (= 1 (graph-db.rules:rule-report-disjoint-premises report)))
      (is (every #'claim-extent (derived g 'rt-claim "hv-when"))))))

(test a-temporal-family-refuses-a-derivation-with-no-extent
  (with-rules-graph (g)
    (seed g)
    ;; Premises without extents into a temporal family: nothing to
    ;; intersect, so the constructor refuses, and the report names it.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "no-extent" :version "1"
                                 :family "rtt-claim"
                                 :head "(claim ?c rtt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)"
                                 :body *web-hosts-body*))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (= 0 (graph-db.rules:rule-report-derived report)))
      (is (equal 'rtt-claim (car (first (graph-db.rules:rule-report-refusals report)))))
      (is (search "extent" (cdr (first (graph-db.rules:rule-report-refusals report)))))
      (is (null (derived g 'rtt-claim "no-extent"))))))

(test a-refused-derivation-leaves-the-previous-one-intact
  "Spec §7.5: the sweep unwinds with the refusal."
  (with-rules-graph (g)
    (seed g)
    (seed-temporal g)
    (let ((r (write-host-version g)))
      (graph-db.rules:run-rule g r)
      (let ((before (mapcar #'claim-identity-key
                            (derived g 'rtt-claim "host-version"))))
        (is (= 4 (length before)))
        ;; A second producer's version 2 run overlapping scan-a's makes
        ;; (h1, 2) derivable twice with overlapping validity: one base
        ;; tuple, two live runs, EXTENT-DISJOINTNESS-VIOLATION at commit.
        (with-transaction ((graph-db::transaction-manager g))
          (make-rtt-claim-binary :graph g :subject-namespace :app
                                 :subject-key "web" :relation "version"
                                 :object-namespace :ver :object-key "2"
                                 :producer "scan-c" :standing :observed
                                 :extent (interval (ts 2026 3 1)
                                                   (ts 2026 4 30))))
        (let ((report (graph-db.rules:run-rule g r)))
          (is (eq :refused (graph-db.rules:rule-report-outcome report)))
          (is (= 0 (graph-db.rules:rule-report-swept report)))
          (is (eq 'rtt-claim
                  (car (first (graph-db.rules:rule-report-refusals report)))))
          (is (search "overlapping"
                      (cdr (first (graph-db.rules:rule-report-refusals report))))))
        (is (equal (sort (copy-list before) #'string<)
                   (sort (mapcar #'claim-identity-key
                                 (derived g 'rtt-claim "host-version"))
                         #'string<)))
        (is (= 8 (length (claims-by-producer g 'graph-db.rules:derivation
                                             "rule/host-version"))))))))

(test an-unrouted-body-goal-is-refused-not-walked
  "Ruling P4: RUN-RULE always binds a budget, so the family walk is
refused inside the transaction rather than joining every claim to the
read set."
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "walker" :version "1"
                                 :family "rt-claim"
                                 :head "(claim ?c rt-claim \"host\" ?h \"has-app\" \"app\" ?a)"
                                 :body "(claim ?p rt-claim \"host\" ?h \"runs\" ?ons ?a)"))))
      (is (eq :refused (graph-db.rules:rule-report-outcome report)))
      (is (eq :budget (car (first (graph-db.rules:rule-report-refusals report)))))
      (is (search "cost-unbounded"
                  (cdr (first (graph-db.rules:rule-report-refusals report))))))
    ;; Control: the same rule with the producer generator in front routes.
    (let ((report (graph-db.rules:run-rule
                   g (write-rule g :name "routed" :version "1"
                                 :family "rt-claim"
                                 :head "(claim ?c rt-claim \"host\" ?h \"has-app\" \"app\" ?a)"
                                 :body "(claim ?p rt-claim \"host\" ?h \"runs\" ?ons ?a) (claim-producer ?p \"scan-a\")"))))
      (is (eq :derived (graph-db.rules:rule-report-outcome report)))
      (is (= 2 (graph-db.rules:rule-report-derived report))))
    ;; An exhausted budget is the same refusal class.
    (let ((graph-db.rules:*rules-max-inferences* 1))
      (is (eq :budget
              (car (first (graph-db.rules:rule-report-refusals
                           (graph-db.rules:run-rule g "routed")))))))
    ;; No bound at all is an operator error, not a report.
    (let ((graph-db.rules:*rules-max-inferences* nil)
          (graph-db.rules:*rules-timeout* nil))
      (signals error (graph-db.rules:run-rule g "routed")))))

(test a-stored-rule-and-a-def-rule-with-one-text-derive-one-set
  (with-rules-graph (g)
    (seed g)
    (let* ((r (write-web-hosts g))
           (stored (progn (graph-db.rules:run-rule g r)
                          (sort (mapcar #'claim-identity-key
                                        (derived g 'rt-claim "web-hosts"))
                                #'string<))))
      (is (= 2 (length stored)))
      ;; Same name is a collision while the record exists; delete it.
      (with-transaction ((graph-db::transaction-manager g))
        (mark-deleted r))
      (graph-db.rules:def-rule "web-hosts" :version "1" :family rt-claim
        :head *web-hosts-head* :body *web-hosts-body*)
      (unwind-protect
           (let ((report (graph-db.rules:run-rule g "web-hosts")))
             (is (= 2 (graph-db.rules:rule-report-swept report)))
             (is (equal stored
                        (sort (mapcar #'claim-identity-key
                                      (derived g 'rt-claim "web-hosts"))
                              #'string<))))
        (graph-db.rules:undef-rule "web-hosts")))))

(test run-rules-runs-in-dependency-order-and-skips-the-disabled
  (with-rules-graph (g)
    (seed g)
    ;; Written in the wrong order: "hosts-web" reads what "web-hosts"
    ;; derives, and a disabled third rule is compiled but not run.
    (write-rule g :name "hosts-web" :version "1" :family "rt-claim"
                :head "(claim ?c rt-claim \"host\" ?h \"hosts-web\" nil nil)"
                :body "(claim ?x rt-claim \"app\" \"web\" \"hosted-on\" \"host\" ?h)")
    (write-web-hosts g)
    (write-rule g :name "parked" :version "1" :family "rt-claim"
                :enabled nil
                :head "(claim ?c rt-claim \"app\" \"web\" \"parked\" \"host\" ?h)"
                :body *web-hosts-body*)
    (let ((reports (graph-db.rules:run-rules g)))
      (is (equal '("web-hosts" "hosts-web")
                 (mapcar #'graph-db.rules:rule-report-rule-name reports)))
      (is (every (lambda (r) (eq :derived (graph-db.rules:rule-report-outcome r)))
                 reports))
      (is (= 2 (graph-db.rules:rule-report-derived (second reports))))
      (is (null (derived g 'rt-claim "parked")))
      (is (= 2 (select-count (?h) (claim ?c rt-claim "host" ?h "hosts-web"
                                         ?a ?b)
                                  (claim-producer ?c "rule/hosts-web")))))))

(test run-rules-reports-a-rule-that-no-longer-compiles-and-the-store-opens
  "Spec §6: a rule that fails to compile is reported and skipped, never
refused at open.  Here a DEF-RULE added after the write closes a cycle
with a stored rule."
  (with-rules-graph-dir (g dir)
    (seed g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)"
                :body "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")
    (write-web-hosts g)
    (graph-db.rules:def-rule "b" :version "1" :family rt-claim
      :head "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)"
      :body "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
    (unwind-protect
         (progn
           (close-graph g)
           (let ((g2 (open-graph *graph-name* (namestring dir))))
             (unwind-protect
                  (let* ((graph-db:*graph* g2)
                         (reports (graph-db.rules:run-rules g2))
                         (by-name (lambda (n)
                                    (find n reports
                                          :key #'graph-db.rules:rule-report-rule-name
                                          :test #'string=))))
                    (is (= 3 (length reports)))
                    (is (eq :refused (graph-db.rules:rule-report-outcome
                                      (funcall by-name "a"))))
                    (is (eq :rule (car (first (graph-db.rules:rule-report-refusals
                                               (funcall by-name "a"))))))
                    (is (eq :refused (graph-db.rules:rule-report-outcome
                                      (funcall by-name "b"))))
                    (is (eq :derived (graph-db.rules:rule-report-outcome
                                      (funcall by-name "web-hosts"))))
                    (is (= 2 (graph-db.rules:rule-report-derived
                              (funcall by-name "web-hosts")))))
               (ignore-errors (close-graph g2)))))
      (graph-db.rules:undef-rule "b"))))

(test run-rules-skips-a-def-rule-whose-family-the-store-lacks
  "Ruling P8: RTF-CLAIM is registered in the image and indexed in no
graph here."
  (with-rules-graph (g)
    (seed g)
    (graph-db.rules:def-rule "foreign" :version "1" :family rtf-claim
      :head "(claim ?c rtf-claim \"app\" \"web\" \"x\" \"host\" ?h)"
      :body "(claim ?p rtf-claim \"host\" ?h \"runs\" \"app\" \"web\")")
    (unwind-protect
         (progn
           (is (null (graph-db.rules:run-rules g)))
           (let ((report (graph-db.rules:run-rule g "foreign")))
             (is (eq :refused (graph-db.rules:rule-report-outcome report)))
             (is (eq :rule (car (first (graph-db.rules:rule-report-refusals
                                        report)))))))
      (graph-db.rules:undef-rule "foreign"))))

(test the-report-carries-cost
  (with-rules-graph (g)
    (seed g)
    (let ((report (graph-db.rules:run-rule g (write-web-hosts g))))
      (is (plusp (graph-db.rules:rule-report-inferences report)))
      (is (typep (graph-db.rules:rule-report-elapsed report) 'real))
      (is (>= (graph-db.rules:rule-report-elapsed report) 0)))))
```

The arithmetic in `validity-is-the-intersection-of-the-premises` and `extent-policy-none-derives-without-an-extent` is worked from `seed` (`tests/rules/suite.lisp`) and `seed-temporal` (Task 2); re-derive it before trusting it, and put the derivation in the test's docstring if it changes. Ruling R6 is the precedent: a count reachable by no correct implementation is a plan defect.

- [ ] **Step 2: Run to verify they fail** — `run-rule` undefined. Record the count.

- [ ] **Step 3: `rules/run.lisp`**

```lisp
;;;; rules/run.lisp -- sweep, derive, record provenance, report
;;;; (spec §7-§9, GH #331).

(in-package #:graph-db.rules)

(defvar *rules-max-inferences* nil
  "Inference budget for one RUN-RULE, or NIL for the DSL's
*QUERY-DEFAULT-MAX-INFERENCES* (spec §7).")

(defvar *rules-timeout* nil
  "Wall-clock seconds for one RUN-RULE's body, or NIL for the DSL's
*QUERY-DEFAULT-TIMEOUT*.")

(defvar *rules-max-solutions* 100000
  "Solutions one RUN-RULE may collect before it is refused rather than
silently truncated.")

(defstruct (rule-report (:constructor %make-rule-report))
  "What one RUN-RULE did (spec §7).  OUTCOME is :DERIVED or :REFUSED;
on :REFUSED the transaction unwound, the sweep included, so DERIVED
and SWEPT are 0 and the previous derivation stands.  REFUSALS is a
list of (TAG . TEXT): TAG a claim family name for a commit refusal,
else :RULE (compile), :BUDGET (the rails), :SOLUTIONS (the cap).
INFERENCES is the count at the last solution; ELAPSED is seconds."
  rule-name version (outcome :derived) (derived 0) (swept 0)
  (disjoint-premises 0) (refusals '()) (inferences 0) (elapsed 0))

(defun rule-producer (name)
  "The producer string rule NAME writes as: rule/NAME."
  (format nil "rule/~A" name))

(defun %resolve-rule (graph rule)
  "RULE as a RULE-SPEC: a record or spec passes through; a name finds
the stored rule, else the def-rule, else signals."
  (etypecase rule
    ((or rule rule-spec) (rule-spec-of rule))
    (string
     (let ((stored (first (graph-db:index-lookup
                           graph 'rule '(name) rule))))
       (cond ((and stored (not (graph-db:deleted-p stored)))
              (rule-spec-of stored))
             ((find-def-rule rule))
             (t (error "No rule named ~S in ~S or the image."
                       rule (graph-db:graph-name graph))))))))

(defun %store-has-family-p (graph family)
  "GRAPH's schema carries FAMILY's parent class (ruling P8)."
  (and (member (graph-db.spacetime:claim-family-parent family)
               (graph-db.query:schema-type-names graph :vertex))
       t))

;;; Evaluating the body

(defun %solutions (compiled graph report)
  "Every solution of COMPILED's body under RUN-QUERY-GOALS' rails
inside the current transaction (spec §7.2, ruling P4): a list of rows
aligned with COMPILED-RULE-VARS.  Refuses past *RULES-MAX-SOLUTIONS*."
  (let ((max-inferences (or *rules-max-inferences*
                            graph-db::*query-default-max-inferences*))
        (timeout (or *rules-timeout* graph-db::*query-default-timeout*))
        (rows '()))
    (unless (or max-inferences timeout)
      (error "RUN-RULE needs a resource bound: *RULES-MAX-INFERENCES* ~
or *RULES-TIMEOUT* (spec §7, ruling P4)."))
    (let ((graph-db::*query-default-max-inferences* max-inferences)
          (graph-db::*query-default-timeout* timeout)
          (graph-db::*query-default-limit* (1+ *rules-max-solutions*)))
      (graph-db::run-query-goals
       (compiled-rule-vars compiled) (compiled-rule-goals compiled) graph
       :limit (1+ *rules-max-solutions*) :format :raw
       :callback (lambda (row)
                   (setf (rule-report-inferences report)
                         graph-db::*inference-count*)
                   (push row rows))))
    (when (> (length rows) *rules-max-solutions*)
      (error 'rule-run-refusal :tag :solutions
             :text (format nil "more than ~D solutions" *rules-max-solutions*)))
    (nreverse rows)))

(define-condition rule-run-refusal (error)
  ((tag :initarg :tag :reader rule-run-refusal-tag)
   (text :initarg :text :reader rule-run-refusal-text))
  (:report (lambda (c s) (format s "~A: ~A" (rule-run-refusal-tag c)
                                 (rule-run-refusal-text c))))
  (:documentation "RUN-RULE's own refusal, caught by RUN-RULE itself and
turned into a report entry; never escapes."))

(defun %term-value (term row vars)
  "TERM's value in this solution: a literal passes, a variable reads
its column."
  (if (%variable-p term)
      (nth (position term vars) row)
      term))

(defun %namespace (value what)
  "VALUE -- a keyword, or the wire string a CLAIM/7 goal bound -- as
the keyword a claim stores.  A string naming no keyword this image
recorded cannot come from a claim, so it is a refusal, not an intern."
  (cond ((keywordp value) value)
        ((graph-db::%namespace-keyword value))
        (t (error 'rule-run-refusal :tag :rule
                  :text (format nil "~A namespace ~S names no namespace ~
of this image" what value)))))

(defun %key (value what)
  (cond ((stringp value) value)
        ((integerp value) (format nil "~D" value))
        (t (error 'rule-run-refusal :tag :rule
                  :text (format nil "~A key must be a string, not ~S"
                                what value)))))

(defun %premise-extent (premises policy)
  "The validity extent a derived claim gets from PREMISES (spec §8):
(VALUES EXTENT DISJOINT-P).  Under :NONE nothing; under :PREMISES the
intersection of the premises that have one, NIL when none has, and
DISJOINT-P when they never held at once."
  (if (eq policy :none)
      (values nil nil)
      (let ((extents (remove nil (mapcar #'graph-db.spacetime:claim-extent
                                         premises))))
        (if (null extents)
            (values nil nil)
            (let ((acc (first extents)))
              (dolist (e (rest extents) (values acc nil))
                (setf acc (temporal-extent:extent-intersection
                           acc e :semantics :validity :standing :inferred))
                (when (null acc)
                  (return (values nil t)))))))))

(defun %dedupe-key (family sns skey rel ons okey extent)
  "The derived claim's identity, as SPEC §7.3 collapses duplicates on:
the endpoints and relation, plus the extent start for a temporal
family -- what CLAIM-IDENTITY-KEY keys on, less the producer, which is
constant here."
  (list sns skey rel ons okey
        (and (graph-db.spacetime:claim-family-temporal-p family)
             extent
             (graph-db.spacetime:extent-sexp-start-key
              (graph-db.spacetime:extent->sexp extent)))))

(defun %constructor (family unary-p)
  "MAKE-<PARENT>-UNARY or -BINARY, interned where the parent is."
  (let ((parent (graph-db.spacetime:claim-family-parent family)))
    (fdefinition (intern (format nil "MAKE-~A-~A" parent
                                 (if unary-p "UNARY" "BINARY"))
                         (symbol-package parent)))))

(defun %derive (compiled graph report)
  "Spec §7.3-§7.4, inside the transaction: one claim per distinct
solution, one DERIVED-FROM record per (claim, premise)."
  (let* ((spec (compiled-rule-spec compiled))
         (family (compiled-rule-family compiled))
         (vars (compiled-rule-vars compiled))
         (producer (rule-producer (rule-spec-name spec)))
         (version (rule-spec-version spec))
         (policy (rule-spec-extent-policy spec))
         (unary-p (compiled-rule-unary-p compiled))
         (claims (make-hash-table :test 'equal))   ; dedupe key -> (args . premises)
         (order '()))
    (dolist (row (%solutions compiled graph report))
      (let* ((premises (remove-if-not #'graph-db::node-p
                                      (mapcar (lambda (v) (%term-value v row vars))
                                              (compiled-rule-premise-vars compiled))))
             (sns (%namespace (%term-value (compiled-rule-head-sns compiled)
                                           row vars) "subject"))
             (skey (%key (%term-value (compiled-rule-head-skey compiled)
                                      row vars) "subject"))
             (ons (and (not unary-p)
                       (%namespace (%term-value (compiled-rule-head-ons compiled)
                                                row vars) "object")))
             (okey (and (not unary-p)
                        (%key (%term-value (compiled-rule-head-okey compiled)
                                           row vars) "object"))))
        (multiple-value-bind (extent disjoint-p)
            (%premise-extent premises policy)
          (if disjoint-p
              (incf (rule-report-disjoint-premises report))
              (let ((key (%dedupe-key family sns skey
                                      (compiled-rule-relation compiled)
                                      ons okey extent)))
                (let ((entry (gethash key claims)))
                  (if entry
                      (setf (cdr entry)
                            (union (cdr entry) premises
                                   :key #'graph-db:id :test #'equalp))
                      (progn
                        (setf (gethash key claims)
                              (cons (list :subject-namespace sns
                                          :subject-key skey
                                          :object-namespace ons
                                          :object-key okey
                                          :extent extent)
                                    premises))
                        (push key order)))))))))
    (let ((ctor (%constructor family unary-p)))
      (dolist (key (nreverse order))
        (destructuring-bind (args . premises) (gethash key claims)
          (let* ((claim (apply ctor :graph graph
                               :relation (compiled-rule-relation compiled)
                               :producer producer :rule-version version
                               :standing :inferred
                               (if unary-p
                                   (list :subject-namespace
                                         (getf args :subject-namespace)
                                         :subject-key (getf args :subject-key)
                                         :extent (getf args :extent))
                                   args)))
                 (derived-key (graph-db.spacetime:claim-identity-key claim))
                 (seen '()))
            (incf (rule-report-derived report))
            (dolist (p premises)
              (let ((pkey (graph-db.spacetime:claim-identity-key p)))
                (unless (member pkey seen :test #'string=)
                  (push pkey seen)
                  (make-derivation-binary
                   :graph graph :subject-namespace :claim
                   :subject-key derived-key :relation "derived-from"
                   :object-namespace :claim :object-key pkey
                   :producer producer :rule-version version
                   :standing :inferred))))))))
    report))

(defun %sweep (compiled graph report)
  "Spec §7.1: the rule's previous claims and their derivation records."
  (let* ((spec (compiled-rule-spec compiled))
         (producer (rule-producer (rule-spec-name spec)))
         (parent (graph-db.spacetime:claim-family-parent
                  (compiled-rule-family compiled))))
    (setf (rule-report-swept report)
          (graph-db.spacetime:delete-claims-by-producer graph parent producer))
    (graph-db.spacetime:delete-claims-by-producer graph 'derivation producer)
    report))

(defun %violation-family (c)
  "The claim family a commit refusal names, as the report's tag, else
the condition's class name."
  (typecase c
    (graph-db.spacetime:extent-disjointness-violation
     (graph-db.spacetime::edv-claim-class c))
    (graph-db:unique-constraint-violation
     (%parent-of (graph-db::ucv-class-name c)))
    (graph-db:value-constraint-violation
     (%parent-of (graph-db::vcv-class-name c)))
    (t (type-of c))))

(defun %parent-of (class-name)
  "CLASS-NAME's claim family parent when it is an arity subclass, else
itself."
  (let ((f (find-if (lambda (f) (subtypep class-name
                                          (graph-db.spacetime:claim-family-parent f)))
                    (alexandria:hash-table-values
                     graph-db.spacetime::*claim-families*))))
    (if f (graph-db.spacetime:claim-family-parent f) class-name)))

(defun run-rule (graph rule)
  "Sweep RULE's previous derivation and derive afresh, in one
transaction (spec §7).  RULE is a RULE record, a RULE-SPEC, or a name
(stored first, then DEF-RULE).  => RULE-REPORT.  A refusal of any kind
-- compile, the rails, a commit constraint, a missing extent -- is
reported, never signalled, and unwinds the whole run so the previous
derivation stands; an operator error (no resource bound, no such rule)
signals."
  (let* ((spec (%resolve-rule graph rule))
         (report (%make-rule-report :rule-name (rule-spec-name spec)
                                    :version (rule-spec-version spec)))
         (start (get-internal-real-time)))
    (flet ((refuse (tag text)
             (setf (rule-report-outcome report) :refused
                   (rule-report-derived report) 0
                   (rule-report-swept report) 0
                   (rule-report-disjoint-premises report) 0)
             (setf (rule-report-refusals report)
                   (append (rule-report-refusals report)
                           (list (cons tag text))))))
      (handler-case
          (let ((compiled (compile-rule graph spec)))
            (unless (%store-has-family-p graph (compiled-rule-family compiled))
              (error 'rule-run-refusal :tag :rule
                     :text (format nil "~S does not carry family ~A"
                                   (graph-db:graph-name graph)
                                   (rule-spec-family spec))))
            (graph-db:with-transaction (:graph graph)
              (%sweep compiled graph report)
              (%derive compiled graph report)))
        (rule-compile-error (c)
          (refuse :rule (rule-compile-error-reason c)))
        (rule-run-refusal (c)
          (refuse (rule-run-refusal-tag c) (rule-run-refusal-text c)))
        (graph-db:prolog-error (c)
          (refuse :budget (princ-to-string c)))
        (graph-db:constraint-violation (c)
          (refuse (%violation-family c) (princ-to-string c)))
        (graph-db.spacetime:missing-claim-identity-component (c)
          (refuse (graph-db.spacetime:claim-family-parent
                   (graph-db.spacetime:claim-family
                    (intern (string-upcase (rule-spec-family spec))
                            (symbol-package
                             (graph-db.spacetime:claim-family-parent
                              (compiled-rule-family
                               (compile-rule graph spec)))))))
                  (princ-to-string c)))
        (graph-db:query-precondition-error (c)
          (refuse :rule (princ-to-string c)))))
    (setf (rule-report-elapsed report)
          (/ (- (get-internal-real-time) start)
             (float internal-time-units-per-second 1.0d0)))
    report))
```

**Simplify the `missing-claim-identity-component` handler before writing it**: the family is known before the transaction opens, so bind `family-name` from the compiled rule outside the `handler-case` (compile once, in a `let` before the `with-transaction`, with the `rule-compile-error` handler around that) and use it in the handler. The form above shows the intent; the implementer writes the clean version: compile in its own `handler-case`, then run in a second whose handlers can see `compiled`.

```lisp
(defun %dependency-order (compiled-rules)
  "COMPILED-RULES sorted so a rule runs after every rule deriving a
relation it reads (spec §7).  Cycles were refused at compile, so this
terminates; ties keep the input order."
  (let ((pending (copy-list compiled-rules))
        (done '())
        (derived '()))
    (loop while pending do
      (let ((ready (find-if
                    (lambda (c)
                      (let ((reads (compiled-rule-reads c)))
                        (every (lambda (r)
                                 (or (member r derived :test #'string=)
                                     (notany (lambda (o)
                                               (string= r (compiled-rule-relation o)))
                                             pending)))
                               (if (eq reads :any) '() reads))))
                    pending)))
        (unless ready
          (error "RUN-RULES: no runnable rule among ~S -- a cycle the ~
compiler should have refused."
                 (mapcar (lambda (c) (rule-spec-name (compiled-rule-spec c)))
                         pending)))
        (setf pending (remove ready pending))
        (push ready done)
        (pushnew (compiled-rule-relation ready) derived :test #'string=)))
    (nreverse done)))

(defun run-rules (graph)
  "Every enabled rule GRAPH can run -- its stored rules, plus the
DEF-RULEs whose family it carries (ruling P8) -- each through RUN-RULE
in dependency order (spec §7).  A rule that does not compile is
reported :REFUSED and skipped; the rest still run.  => the reports, in
the order run."
  (let ((reports '())
        (compiled '()))
    (dolist (spec (rules-in-scope graph))
      (handler-case
          (let ((c (compile-rule graph spec)))
            (when (or (eq (rule-spec-source spec) :stored)
                      (%store-has-family-p graph (compiled-rule-family c)))
              (push c compiled)))
        (rule-compile-error (c)
          (push (%make-rule-report
                 :rule-name (rule-spec-name spec)
                 :version (rule-spec-version spec)
                 :outcome :refused
                 :refusals (list (cons :rule (rule-compile-error-reason c))))
                reports))))
    (dolist (c (%dependency-order (nreverse compiled)))
      (push (run-rule graph (compiled-rule-spec c)) reports))
    (nreverse reports)))

;;; Provenance reads (spec §9)

(defun %claim-by-identity-key (graph key)
  "The claim KEY names, resolved through the subject-relation index of
every family this graph carries; a current one over a retracted one.
NIL when nothing in the store has that identity now."
  (multiple-value-bind (producer sns skey rel) 
      (graph-db.spacetime:split-claim-identity-key key)
    (declare (ignore producer))
    (let ((found '()))
      (dolist (family (alexandria:hash-table-values
                       graph-db.spacetime::*claim-families*))
        (handler-case
            (dolist (c (graph-db:index-lookup
                        graph (graph-db.spacetime:claim-family-parent family)
                        graph-db::+claim-subject-relation-index-slots+
                        (list sns skey rel)))
              (when (string= key (graph-db.spacetime:claim-identity-key c))
                (push c found)))
          (graph-db:query-precondition-error () nil)))
      (or (find-if #'graph-db.spacetime:claim-current-p found)
          (first found)))))

(defun premises-of (graph claim)
  "The claims CLAIM was derived from (spec §9): its DERIVED-FROM
records' objects, resolved back to claims.  A premise whose identity
no longer exists in the store is dropped."
  (let ((records (graph-db.spacetime:claims-touching
                  graph 'derivation :claim
                  (graph-db.spacetime:claim-identity-key claim)
                  :role :subject)))
    (remove nil (mapcar (lambda (r)
                          (%claim-by-identity-key
                           graph (graph-db.spacetime:claim-object-key r)))
                        records))))

(defun dependents-of (graph claim &key current)
  "Every derived claim whose provenance names CLAIM (spec §9) -- one
CLAIMS-TOUCHING on the object endpoint, then the subjects resolved.
With CURRENT, only dependents still believed.  Nothing is re-derived."
  (let* ((records (graph-db.spacetime:claims-touching
                   graph 'derivation :claim
                   (graph-db.spacetime:claim-identity-key claim)
                   :role :object))
         (claims (remove nil
                         (mapcar (lambda (r)
                                   (%claim-by-identity-key
                                    graph (graph-db.spacetime:claim-subject-key r)))
                                 records))))
    (if current
        (remove-if-not #'graph-db.spacetime:claim-current-p claims)
        claims)))
```

Notes for the implementer: `%claim-by-identity-key` borrows S1's `graph-db::+claim-subject-relation-index-slots+` so the slot symbols are spacetime's (ruling R3). `graph-db::node-p` is internal. `alexandria` is a `graph-db/core` dependency. `%solutions` reads `graph-db::*inference-count*` inside the callback because `select` rebinds it for the query's extent. `%parent-of` handles the arity subclass a `unique-constraint-violation` names. If `run-query-goals`' `:limit` cap turns out to be applied through `*query-default-limit*` only (recon A5), drop the `:limit` argument.

- [ ] **Step 4: Run to verify they pass** — rules suite green; record the count. Then run `graph-db/spacetime-test` (653/0) and `graph-db/query-test` too: `%validate-rule-writes` is on the image-wide validator list and must be inert for stores without rules.

- [ ] **Step 5: Docs** — `docs/rules.md`: "Running a rule" (the transaction, the sweep, the rails and P4, the report fields and tags, what refuses and that the previous derivation stands, duplicates, `*rules-max-solutions*`), "Validity" (§8 as shipped, P7), "Provenance" (`derivation` family shape, `premises-of`, `dependents-of`, that a retracted premise's dependents are findable and nothing is re-derived), "run-rules" (order, P8, disabled rules, compile failures reported). The "What the functors do not see" paragraph gains its S2 sentence: a body cannot see the sweep, which is why the cycle check is strict.

- [ ] **Step 6: Commit**

```bash
git add rules/run.lisp tests/rules/run-tests.lisp graph-db.asd docs/rules.md
git commit -m "feat(rules): run-rule sweeps and derives in one transaction, with provenance and premise-extent validity (#331)"
```

---

### Task 5: The parked S1 items, the record, the changelog, the full runs

**Files:**
- Modify: `rules/facts.lisp` (`claim-producer/2`'s unbound gate), `tests/rules/facts-tests.lisp` (append one test), `docs/rules.md` (the NIL-`?p` sentence; a complete S2 read-through), `docs/superpowers/decisions/2026-09-04-rules-s1-rulings.md` (one sentence), `CHANGELOG.md`, `docs/ci.md` (if any lane or number changed), `docs/superpowers/handoffs/2026-09-05-rules-s2.md` (status line at the top)
- Create: `docs/superpowers/decisions/2026-09-05-rules-s2-rulings.md`

- [ ] **Step 1: The explicit-NIL `?c` gate** (S1 "Deferred out of S1"). In `rules/facts.lisp`, `claim-producer/2` tests `(null c-arg)` for "unbound", which folds an explicit NIL into it. Add

```lisp
(defun %unbound-p (x)
  "X is an unbound Prolog variable -- not a bound NIL, which
%PROLOG-INDEX-BOUND folds into the same value."
  (let ((v x)) (graph-db::var-p (var-deref v))))
```

and use `(%unbound-p ?c)` where `(null c-arg)` meant unbound (both the generator and the refusal clause). Test, appended to `tests/rules/facts-tests.lisp`:

```lisp
;; S1's deferred gate: a ?C bound to NIL is a bound non-node and fails
;; the goal, it is not the generator's unbound case.
(test a-nil-c-is-bound-not-unbound
  (with-rules-graph (g)
    (seed g)
    (is (zerop (select-count () (claim-producer nil "scan-a"))))
    (is (zerop (select-count (?p) (claim-producer nil ?p))))
    ;; Control: unbound really generates.
    (is (= 2 (select (:count t :max-inferences 1000) (?c)
               (claim-producer ?c "scan-b"))))))
```

Run the diffstat check on `facts-tests.lisp`: 0 removed lines.

- [ ] **Step 2: `docs/rules.md`'s untrue sentence** — "A bound `?p` naming no producer is the empty answer instead, the way an unresolvable namespace is" holds for a string; a `?p` bound to NIL reaches the neither-bound refusal. Rewrite: "A bound `?p` that is a string naming no producer is the empty answer; a `?p` bound to NIL is not a producer name and takes the neither-bound path."

- [ ] **Step 3: The S1 decision record's framing** — first paragraph of `docs/superpowers/decisions/2026-09-04-rules-s1-rulings.md`, add: "Numbered as in the session notes; the gaps are rulings that were routine and not transcribed, so this is a subset, not the whole."

- [ ] **Step 4: `docs/superpowers/decisions/2026-09-05-rules-s2-rulings.md`** — P1–P9 from this plan's header, verbatim, plus every ruling taken during execution, each with Decision / Evidence / Cost if wrong in the S1 file's shape. Say at the top that they were taken without Kevin in the loop and which ones deviate from the spec (P1, P2, P8 deviate; the rest refine).

- [ ] **Step 5: `CHANGELOG.md`** under `## [Unreleased]` → `### Added`, after the S1 entry:

"**`graph-db/rules`, rules as versioned producers** (#304, #331): `def-rules-schema` gives a store the `rule` record (`def-source`, identity `:rule`/`name`, one record per name) and the `derivation` provenance family; `def-rule` is the in-image escape hatch. `compile-rule` reads a rule's head and body through the guard's own screen, reader and whitelist (new export `graph-db.query:guard-query-text`), checks the head as one `claim/7` pattern, moves `claim-producer` generators first, and refuses recursion over every rule in scope with the cycle named; a `rule` write that does not compile is refused at commit (`rule-compile-error`). `run-rule` sweeps the rule's previous derivation and derives afresh in one validated transaction under the query rails, each derived claim `producer rule/<name>`, `rule-version`, `standing :inferred`, its validity the intersection of its premises' (`extent-intersection`, cl-temporal-extent 0.3.0), one `derived-from` claim per premise; a refusal unwinds the whole run and the `rule-report` names it. `run-rules` runs every enabled rule in dependency order; `premises-of` and `dependents-of` read provenance back. `docs/rules.md`."

Under `### Changed`: "`graph-db/spacetime` now requires cl-temporal-extent 0.3.0 (`extent-intersection`)."

- [ ] **Step 6: The full read-through of `docs/rules.md`** — the file's opening says slice 1 "is all of it today"; rewrite the framing for S2, keep every S1 paragraph that is still true, and check every S2 claim against the shipped code (not the plan). Update the handoff file's first lines: S2's status and the cl-temporal-extent dependency on merge order.

- [ ] **Step 7: Runs, in this order, counts recorded in the commit body**: `graph-db/rules-test`, `graph-db/query-test`, `graph-db/spacetime-test`, `graph-db/gui-test`, `graph-db/fast-test`; then the full `graph-db` once (36 minutes; foreground, or `setsid nohup … &` with a watcher on the log, never a plain background job). Green means `Did N checks.` with 0 failures on the *-test* system; a run that skips the GEOS lane on this host is expected (ten skips).

- [ ] **Step 8: Commit**

```bash
git add rules/facts.lisp tests/rules/facts-tests.lisp docs CHANGELOG.md
git commit -m "docs(rules): S2 contract, decision record, the S1 nits; explicit-NIL ?c gate (#331)"
```

- [ ] **Step 9: Whole-branch review** — a fresh reviewer over `git diff docs/handoff-2026-09-05..HEAD`, with the spec §5–§9 and this plan's rulings; then one batched fix task for what it finds (ruling R25's shape). Then the #331 comment: what landed, the rulings by name, the check counts, and that cl-temporal-extent `feat/extent-intersection` must merge before this PR's CI can be green.

---

## Self-review against the spec

- §5 rule record: Task 2 (`def-rules-schema`, slots, facets, value constraint) — with P1 on version records. `def-rule`: Task 3. "A stored rule and a `def-rule` with the same text derive identical claims": Task 4's test.
- §6 compile: Task 3 — guard, head shape, recursion with the cycle named, one-node case, compile on write (P3); compile on open replaced by P2 and tested (`run-rules-reports-a-rule-that-no-longer-compiles-and-the-store-opens`).
- §7 run: Task 4 — sweep both families, rails with effects off and one snapshot, duplicates collapse, provenance, refusal unwinds with the report naming the rule; `run-rules` in dependency order; report fields.
- §8 validity: Task 1 (`extent-intersection`) and Task 4 (`%premise-extent`, `disjoint-premises`, the temporal refusal).
- §9 provenance: Task 4 (`derivation` shape, `premises-of`, `dependents-of`).
- §11 S2 tests: each bullet has a named test in Task 4, plus Task 3's compile refusals.
- §13: the one `graph-db.query` export (Task 2); `extent-intersection`'s own PR (Task 1).
- Handoff's three S2 traps: `:record-reads` (P4, tested), goal order (P5, tested), the package warning (Global Constraints; nothing imported).
- Type consistency: `compiled-rule-*` slot names match between Task 3's struct and Task 4's readers; `rule-spec-of`, `rules-in-scope`, `%stored-rules`, `rule-producer`, `%make-rule-spec` are the shared internal names.
