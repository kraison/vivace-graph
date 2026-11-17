# Forward Rules Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `graph-db/rules` derives claims and fires productions from each
commit's claim writes, incrementally, with retraction, without the
caller running `run-rules`.

**Architecture:** A post-commit seam in core hands the committed
transaction to the rules layer; the layer turns its claim writes into a
weighted delta, queues it per graph, and the runner thread drains the
queue: touched Datalog strata run through the existing fixpoint
machinery seeded with the delta and reconciled partially, lost
derivations are found through provenance or by a bounded full re-run,
then productions fire from an agenda with durable `firing` claims.
Thresholds against `now/1` schedule their own re-evaluation.

**Tech Stack:** Common Lisp (SBCL primary; CCL/ECL/LispWorks arms where
core is touched), FiveAM, bordeaux-threads, local-time.

**Spec:** `docs/superpowers/specs/2026-11-04-forward-rules-design.md`
(D1–D5 in its §1 are binding).

## Global Constraints

- Lisp: spaces only, 80 columns hard, terse comments that point at the
  spec section or GH issue (`~/.claude/CLAUDE.md`).
- No form this plan adds runs inside `with-transaction-manager-lock`
  (spec §10 item 10). The core seam is placed per spec §6.
- A lost derivation is **retracted**, never deleted (D4), in
  `run-rules` and forward mode alike.
- Currency is the body's business: no implicit `claim-current` anywhere
  (spec §4.2).
- Never run the full suite by hand. Per-test runs use the script Task 1
  adds; the rules suite as a whole is
  `sbcl --dynamic-space-size 16384 --non-interactive --eval '(ql:quickload :graph-db/rules-test :silent t)' --eval '(asdf:test-system :graph-db/rules-test)'`
  and is run once per task at `task-done`; CI runs everything on push.
- One SBCL at a time in this checkout (FASL cache).
- FiveAM test names are symbols in `graph-db/rules-test`; grep
  `tests/rules/` before naming one.
- Docs travel with code: `docs/rules.md`, the manual's rules chapter
  (`docs/vivace-graph-v3-doc.org`, search "graph-db/rules"), and
  `CHANGELOG.md` are updated in the task that changes the behaviour.
- Public repo: issue text, commit messages and docs stay
  domain-neutral.

## Review Focus

1. A transaction that creates a claim and deletes it before commit: the
   delta must net to zero for that claim and touch nothing. Test in
   Task 4.
2. A user retracts a rule's derived claim by hand: the next forward step
   touching that rule, and `run-rules`, re-open it (the derivation is
   still supported), and the status reports nothing. Test in Task 5.
3. `close-graph` with a non-empty queue or a pending timer: the step and
   the timer must not touch a closed graph; `disable-forward-rules` is
   implied by close. Test in Task 7.
4. A production action that writes, then signals: the write stands, the
   `firing` claim stands, the failure is on the status, and the next
   entrant's delta is still processed. Test in Task 8.
5. A retracted premise cited by many derivations (hundreds): the targeted
   re-derive path, not a full re-run, and bounded by
   `*rules-max-inferences*`. Test in Task 5.

---

## File structure

- Modify `transactions.lisp`: `*after-commit-hooks*` and its call site in
  `call-with-transaction` (Task 3).
- Modify `rules/run.lisp`: retracting sweep with re-open (Task 1);
  `%variants` generalisation, partial reconcile, `run-forward-strata`,
  lost-derivation paths (Tasks 4, 5).
- Modify `rules/facts.lisp`: `claim-absent/7` (Task 2); `now/1`,
  `instant-offset/3` (Task 7).
- Modify `rules/compile.lisp`: `claim-absent` in reads (Task 2);
  threshold goals (Task 7); productions in `%edges`/strata (Task 8).
- Create `rules/delta.lisp`: writes to delta, touch test (Task 4).
- Create `rules/forward.lisp`: opt-in table, per-graph state, queue,
  runner, status, compiled cache (Tasks 4, 6).
- Create `rules/thresholds.lisp`: flip instants, duration parsing, the
  timer thread (Task 7).
- Create `rules/productions.lisp`: `def-production`, action registry,
  `compile-production`, agenda, `firing` claims (Task 8).
- Modify `rules/schema.lisp`: `production` record, `firing` family
  (Task 8). Modify `rules/package.lisp`, `graph-db.asd` as files appear.
- Create `tests/rules/forward-tests.lisp`, `tests/rules/run-test.sh`;
  modify `tests/rules/run-tests.lisp`, `fixpoint-tests.lisp`,
  `facts-tests.lisp`, `compile-tests.lisp`, `suite.lisp`.

---

### Task 1: The sweep retracts and re-opens (D4)

**Files:**
- Modify: `rules/run.lisp` (`%reconcile-claims` ~388, `%reconcile-provenance` ~437, `%refresh-kept` ~210)
- Modify: `tests/rules/run-tests.lisp` (`rerunning-sweeps-the-previous-derivation` ~58), `tests/rules/fixpoint-tests.lisp` (any `deleted-p`/count assertion on swept claims)
- Create: `tests/rules/run-test.sh`
- Modify: `docs/rules.md` (the sentence "a retracted derived claim whose identity is derived again stays retracted"), `CHANGELOG.md` (Changed)

**Interfaces:**
- Produces: a swept derived claim and its `derived-from` records are `retract-claim`ed; a re-derived identity whose claim is retracted is re-opened in place (same node, transaction extent re-opened as `register.lisp:263-268` does). `rule-report-swept` counts retractions.

- [ ] **Step 1: Add the per-test runner**

`tests/rules/run-test.sh NAME` runs one FiveAM test of the rules suite
in a fresh image with the fixture `run-rules-tests` binds. Body:

```bash
#!/usr/bin/env bash
set -u
[ $# -ge 1 ] || { echo "usage: $0 TEST-NAME" >&2; exit 2; }
HERE="$(cd "$(dirname "$0")/../.." && pwd -P)"
exec sbcl --dynamic-space-size 16384 --non-interactive \
  --eval '(ql:quickload :graph-db/rules-test :silent t)' \
  --eval "(let* ((d (graph-db-test-scratch:make-scratch-directory
                      \"graph-db-rules-sys\"))
                 (graph-db::*system-directory* (namestring d))
                 (graph-db::*type-registry* nil))
            (log:config :error)
            (unwind-protect
                 (let ((r (fiveam:run 'graph-db/rules-test::$1)))
                   (fiveam:explain! r)
                   (uiop:quit (if (fiveam:results-status r) 0 1)))
              (graph-db-test-scratch:cleanup-scratch-run)))"
```

`chmod +x`. Verify: `tests/rules/run-test.sh run-rule-derives-claims-that-name-the-rule` exits 0.

- [ ] **Step 2: Write the failing tests**

In `run-tests.lisp`, change `rerunning-sweeps-the-previous-derivation`
so the swept claim is asserted present and retracted, and add:

```lisp
(test a-swept-derivation-is-retracted-with-its-provenance
  ;; seed, run web-hosts, retract the h2 premise (retract-claim),
  ;; run again:
  (is (= 1 (rule-report-swept report2)))
  (let ((c (find "h2" (derived g 'rt-claim "web-hosts" :current nil)
                 :key #'claim-object-key :test #'string=)))
    (is (not (null c)))
    (is (not (claim-current-p c)))
    (is (every (lambda (r) (not (claim-current-p r)))
               (derived-from-records g c)))))

(test a-re-derived-identity-re-opens-the-retracted-claim
  ;; ... then re-assert the h2 premise and run a third time:
  (is (= 1 (rule-report-kept report3)))
  (is (eq (id c-before) (id c-after)))   ; same node, not a second claim
  (is (claim-current-p c-after)))
```

`derived` takes a new `:current` key (default `t`) in the fixture;
`derived-from-records` is a fixture helper over `claims-touching` on the
`derivation` family.

- [ ] **Step 3: Run them**

Run: `tests/rules/run-test.sh a-swept-derivation-is-retracted-with-its-provenance`
Expected: FAIL (the claim is gone: `c` is NIL).

- [ ] **Step 4: Implement**

In `%reconcile-claims`: read the producer's claims **including
retracted** ones; a key in `desired` whose claim is retracted is
re-opened (copy, reset transaction extent to open at `%st-now`, save)
and counted as kept; a key not in `desired` whose claim is current is
`retract-claim`ed and counted as swept; a retracted claim not in
`desired` is left alone and counted nowhere. The same for
`%reconcile-provenance` over `derived-from` records (retract instead of
`mark-deleted`; re-open a retracted record that is wanted again). Keep
the "constructed fresh" path for identities with no claim at all.

- [ ] **Step 5: Run the two tests and the fixpoint suite's sweep tests**

Run: `tests/rules/run-test.sh a-re-derived-identity-re-opens-the-retracted-claim`, then `tests/rules/run-test.sh rules-suite` (the whole suite; fix any test that asserted deletion).
Expected: PASS; suite green.

- [ ] **Step 6: Docs and commit**

`docs/rules.md`: the retained-retracted sentence now says a swept
derivation is retracted and re-opened when derived again.
`CHANGELOG.md` Changed: "run-rules retracts a derivation it no longer
derives instead of deleting it; provenance stays readable."

```bash
git add rules/run.lisp tests/rules/run-tests.lisp tests/rules/fixpoint-tests.lisp tests/rules/suite.lisp tests/rules/run-test.sh docs/rules.md CHANGELOG.md
git commit -m "rules: the sweep retracts a lost derivation and re-opens it when derived again (D4)"
```

---

### Task 2: `claim-absent/7`

**Files:**
- Modify: `rules/facts.lisp` (after `claim-current/1`), `rules/compile.lisp` (`%body-reads` ~277, `%premise-vars` ~321)
- Modify: `tests/rules/facts-tests.lisp`, `tests/rules/compile-tests.lisp`, `docs/rules.md` (functor table and a paragraph beside `not/1`)

**Interfaces:**
- Produces: global functor `claim-absent/7 (?c family ?sns ?skey ?rel ?ons ?okey)`: `claim/7`'s routes, filtered to `claim-current-p` claims whose `claim-standing` is `:searched-empty` or `:determined-empty`. `%body-reads` counts its relation as a read; `%premise-vars` counts its `?c` as a premise.

- [ ] **Step 1: Failing tests**

```lisp
(test claim-absent-answers-determined-absences-only
  ;; five unary rt-claims on h1..h5, relation "scanned", standings
  ;; :searched-empty :determined-empty :uncovered :indeterminate
  ;; :observed; a sixth subject h6 has no claim.
  (is (equal '("h1" "h2")
             (sort (select-flat (?k) (claim-absent ?c rt-claim "host" ?k
                                                   "scanned" ?o ?ok))
                   #'string<))))

(test claim-absent-is-a-positive-read-for-strata
  (let ((c (compile-rule g (write-rule g :name "unscanned" ...
             :body "((claim-absent ?c rt-claim \"host\" ?h \"scanned\" ?o ?k))"))))
    (is (member "scanned" (compiled-rule-reads c) :test #'string=))
    (is (null (compiled-rule-negative-reads c)))))
```

- [ ] **Step 2: Run** `tests/rules/run-test.sh claim-absent-answers-determined-absences-only`. Expected: FAIL, unknown functor.
- [ ] **Step 3: Implement** the functor by delegating to `%claim-goal` with a wrapping continuation that tests standing and currency; extend `%engine-goal-p` call sites in `%body-reads`/`%premise-vars` to accept `"CLAIM-ABSENT"` 7.
- [ ] **Step 4: Run both tests.** Expected: PASS.
- [ ] **Step 5: Docs and commit.** `docs/rules.md`: table row and the "two negations" paragraph (spec D2 wording).

```bash
git add rules/facts.lisp rules/compile.lisp tests/rules/facts-tests.lisp tests/rules/compile-tests.lisp docs/rules.md
git commit -m "rules: claim-absent/7, the determined-absence goal beside not/1 (D2)"
```

---

### Task 3: The post-commit seam in core

**Files:**
- Modify: `transactions.lisp` (`call-with-transaction` ~3260; a `defvar` beside `*commit-validators*` ~15)
- Create: `tests/rules/forward-tests.lisp` (suite file registered in `graph-db.asd`'s `graph-db/rules-test` components and `tests/rules/suite.lisp`'s package)

**Interfaces:**
- Produces: `graph-db:*after-commit-hooks*`, a list of functions of one argument, the committed `tx`, called by `call-with-transaction` after its retry loop has returned a completed transaction, outside the `let` that binds `*transaction*` and outside every lock; not called when the body unwound. The tx's `writes` are readable in the hook.

- [ ] **Step 1: Failing tests**

```lisp
(test after-commit-hooks-run-once-outside-the-transaction
  ;; push a hook recording (graph-db::*transaction*, tx state,
  ;; (length (writes tx))) ; commit a seed claim
  (is (= 1 (length *seen*)))
  (is (null (first (first *seen*))))          ; *transaction* unbound
  (is (eq :committed (second (first *seen*))))
  (is (= 1 (third (first *seen*)))))

(test after-commit-hooks-do-not-run-on-abort
  ;; body signals; (is (null *seen*)))

(test after-commit-hooks-run-after-the-retry-exhausted-path
  ;; bind *maximum-transaction-attempts* to 0 so the locked path is
  ;; taken; the hook asserts it does NOT hold the manager lock:
  ;; (bt:acquire-lock (graph-db::lock tm) nil) succeeds from a helper
  ;; thread while the hook runs.
  )
```

- [ ] **Step 2: Run** the first. Expected: FAIL, `*after-commit-hooks*` unbound.
- [ ] **Step 3: Implement.** Restructure `call-with-transaction` so the loop returns `(values result tx)` into locals; after the loop, `(when completed (dolist (h *after-commit-hooks*) (funcall h tx)))`; return `result`. The `let` binding `*transaction*` stays inside `call-transaction-fun`. Mirror in any other `call-with-transaction` method (grep `defmethod call-with-transaction`).
- [ ] **Step 4: Run the three tests.** Expected: PASS.
- [ ] **Step 5: Commit** (`[skip-docs]` is not needed: add one sentence to the manual's transactions chapter naming the seam).

```bash
git add transactions.lisp tests/rules/forward-tests.lisp tests/rules/suite.lisp tests/rules/package.lisp graph-db.asd docs/vivace-graph-v3-doc.org
git commit -m "transactions: *after-commit-hooks*, called after the retry loop outside every lock (forward rules §6)"
```

---

### Task 4: Delta, opt-in, and forward derivation of positive deltas

**Files:**
- Create: `rules/delta.lisp`, `rules/forward.lisp`
- Modify: `rules/run.lisp` (`%variants` ~556, `%run-stratum` ~766, `%reconcile-claims`), `rules/package.lisp`, `graph-db.asd`
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Consumes: `*after-commit-hooks*` (Task 3).
- Produces:
  - `(claim-delta writes graph)` → `equal` hash, relation string → list of `(claim . weight)`, per spec §3's table; a claim created and deleted in the same transaction nets out (absent from the table).
  - `(delta-touches-p compiled delta)` → boolean over `reads`, `negative-reads`.
  - `(enable-forward-rules graph)`, `(disable-forward-rules graph)`, `(forward-rules-enabled-p graph)`; `enable-forward-rules` runs `run-rules` once so an enabled store starts at the fixpoint (**ruling**: spec §5.3 says threshold rules run once; running all rules once is the same cost class and makes acceptance 1 hold for a store with pre-existing premises).
  - `forward-state` struct in `rules/forward.lisp`: `graph queue lock running-p failures compiled cache-stamp timers`.
  - `(forward-step graph)`: drains the queue (Task 6 adds the concurrency contract; here single-threaded).
  - `(run-forward-strata graph compiled-rules delta)` in `run.lisp` → list of `rule-report`, implementing spec §4.1's three differences for strata touched by +1 entries.
  - `%variants` gains `&key (all-goals nil)`: with it, every `claim/7` and `claim-absent/7` goal with a variable `?c` is a substitution site.
  - `%reconcile-claims`/`%reconcile-provenance` gain `&key sweep-keys`: NIL keeps today's full sweep; a hash of head identity keys restricts the sweep to those (the partial reconcile).
  - A per-graph compiled-rule cache in `forward-state`, invalidated by `def-rule`/`undef-rule` and by any committed write of a `rule` node (checked in the hook from `writes`).

- [ ] **Step 1: Failing tests**

```lisp
(test a-delta-nets-a-claim-created-and-deleted-in-one-transaction)
  ;; with-transaction: make-rt-claim-binary then mark-deleted it;
  ;; (is (zerop (hash-table-count (claim-delta (writes tx) g))))

(test a-forward-rule-derives-after-the-commit-of-its-last-premise
  (with-rules-graph (g)
    (write-web-hosts g)
    (enable-forward-rules g)
    (seed g)                              ; premises arrive by commit
    (is (= 2 (length (derived g 'rt-claim "web-hosts"))))
    (is (every (lambda (r) (zerop (rule-report-derived r)))
               (run-rules g)))))          ; nothing left to do

(test a-forward-rule-derives-through-two-strata-in-one-step)
  ;; rule B reads A's head; one commit; both derived without run-rules

(test an-opted-out-graph-derives-nothing-at-commit)
```

- [ ] **Step 2: Run** the second. Expected: FAIL, `enable-forward-rules` undefined.
- [ ] **Step 3: Implement `rules/delta.lisp`** per the Produces block; a claim node is one whose class is a `claim-family-parent` subclass (`graph-db.spacetime::*claim-families*`); retraction detection compares `claim-current-p` of `old-node` and `node`.
- [ ] **Step 4: Implement `run-forward-strata`** in `run.lisp`: for each stratum in `%strata-order` of the touched compiled rules, run the stratum through `%run-stratum`'s machinery with: seed = the delta's +1 claims (`*rule-delta*` keyed by relation), `*claim-exclude-producers*` unbound, `%variants ... :all-goals t` for every rule, and `:sweep-keys` = an empty hash (nothing swept on the positive path). Factor `%run-stratum` so both callers share `run-to-fixpoint` with the seed and exclusion as parameters rather than duplicating it.
- [ ] **Step 5: Implement `rules/forward.lisp`**: the opt-in table (`equal` hash graph → `forward-state`), the hook function (installed once by `enable-forward-rules` into `*after-commit-hooks*`; it looks the tx's graph up, returns at once if not enabled, builds the delta, enqueues, calls `forward-step`), `forward-step` draining the queue by calling `run-forward-strata` inside its own `with-transaction`, and the compiled cache.
- [ ] **Step 6: Run the four tests, then the suite.** Expected: PASS; suite green (the generalised `%variants` with `all-goals nil` must leave fixpoint tests unchanged).
- [ ] **Step 7: Docs and commit.** `docs/rules.md`: new section "Forward evaluation" (opt-in, the delta table, what a step does, the run-rules repair). `CHANGELOG.md` Added.

```bash
git add rules/delta.lisp rules/forward.lisp rules/run.lisp rules/package.lisp graph-db.asd tests/rules/forward-tests.lisp docs/rules.md CHANGELOG.md
git commit -m "rules: forward derivation at commit -- delta, opt-in, positive deltas through the fixpoint machinery (spec §3, §4.1)"
```

---

### Task 5: Lost derivations

**Files:**
- Modify: `rules/run.lisp` (`run-forward-strata`), `rules/forward.lisp`
- Test: `tests/rules/forward-tests.lisp`; `docs/rules.md` (currency paragraph)

**Interfaces:**
- Consumes: `dependents-of` (`run.lisp` ~1414), `%reconcile-claims :sweep-keys`, `claim-identity-key`.
- Produces: in `run-forward-strata`, for −1 entries: (a) non-recursive rule citing the claim: collect `(dependents-of graph claim)` filtered to that rule's producer, re-derive each with its head identity bound (a `%desired` call with the head terms bound from the dependent's key), pass the dependents' identity keys as `:sweep-keys`; (b) a recursive stratum citing it, or any ±1 on a rule's `negative-reads`: run that stratum/rule through `run-rules`'s full path (`run-rule` or `%run-stratum`) with the full sweep. Lost derived claims re-enter the loop as −1 entries for later strata.

- [ ] **Step 1: Failing tests**

```lisp
(test a-forward-rule-loses-its-derivation-when-a-current-premise-is-retracted)
  ;; body has (claim-current ?c) on the premise; retract-claim h2's
  ;; premise in a transaction; derived h2 claim is now not current,
  ;; its provenance names the premise; run-rules changes nothing.
(test a-restored-premise-re-opens-the-lost-derivation)
  ;; same node id as before (acceptance 1).
(test a-derivation-lost-in-one-stratum-is-lost-in-the-next)
(test a-not-rule-derives-when-its-blocker-is-retracted-and-loses-when-one-arrives)
(test a-recursive-stratum-reaches-run-rules-fixpoint-after-a-retraction)
  ;; the fixpoint suite's closure fixture; retract one edge; compare
  ;; the derived set with a fresh run-rules on a copy.
(test a-hand-retracted-derived-claim-is-re-opened)        ; Review Focus 2
(test a-premise-with-many-dependents-takes-the-targeted-path)  ; RF 5
  ;; 300 dependents; count %desired invocations via a counting hook
  ;; or assert rule-report-rounds = 1 and inferences below the full
  ;; re-run's measured count.
```

- [ ] **Step 2: Run** the first. Expected: FAIL (derivation still current).
- [ ] **Step 3: Implement** per Produces.
- [ ] **Step 4: Run all seven, then the suite.** Expected: PASS.
- [ ] **Step 5: Docs and commit.** `docs/rules.md`: "a rule meant to track retraction says `claim-current` on each premise it tracks" and the three-case table from spec §4.2.

```bash
git add rules/run.lisp rules/forward.lisp tests/rules/forward-tests.lisp docs/rules.md
git commit -m "rules: lost derivations -- targeted re-derive through provenance, full re-run for recursion and NOT (spec §4.2)"
```

---

### Task 6: Queue, nesting, failure, status

**Files:**
- Modify: `rules/forward.lisp`, `rules/package.lisp`
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Produces:
  - `forward-step` contract (D5): under `forward-state-lock`, if `running-p` is set, return; else set it and loop: pop all queued deltas, release the lock, run them, re-take the lock, if the queue is empty clear `running-p` and return, else continue.
  - The hook enqueues without running when `graph-db::*transaction*` is bound (nested commit); the outermost commit's hook drains.
  - `(forward-rules-status graph)` → plist `(:enabled t :queue-depth n :running-p bool :failures (list of (rule-name . condition-text)) :timers n)`.
  - Failures: a condition out of `run-forward-strata` is caught in `forward-step`, pushed on `failures`, logged with `log:error`; the step continues with the next delta.

- [ ] **Step 1: Failing tests**

```lisp
(test two-committing-threads-both-get-their-derivations-and-neither-blocks)
  ;; a slow def-rule body (a functor that sleeps 0.2 s, test-only);
  ;; thread A commits premise a, thread B commits premise b while A's
  ;; step runs; B's commit returns before A's step ends (measure);
  ;; both derived afterwards; forward-rules-status showed queue-depth
  ;; 1 at some point (poll it from the test thread).
(test a-nested-commit-defers-its-step-to-the-outermost)
(test a-failing-rule-body-leaves-the-commit-durable-and-is-reported)
  ;; def-rule whose body calls a test functor that signals; the
  ;; with-transaction returns normally; the premise is present;
  ;; (getf (forward-rules-status g) :failures) names the rule;
  ;; run-rules then reports :refused for it, nothing else changes.
```

- [ ] **Step 2: Run** the first. Expected: FAIL (second commit blocks or a delta is lost).
- [ ] **Step 3: Implement** per Produces.
- [ ] **Step 4: Run the three, then the suite.** Expected: PASS.
- [ ] **Step 5: Docs and commit.** `docs/rules.md` Forward evaluation: the queue contract and the read-after-derive caveat (spec §6 wording); `forward-rules-status`.

```bash
git add rules/forward.lisp rules/package.lisp tests/rules/forward-tests.lisp docs/rules.md
git commit -m "rules: per-graph delta queue, nested-commit deferral, step failure reporting, forward-rules-status (D5, spec §6)"
```

---

### Task 7: `now/1`, `instant-offset/3`, thresholds and timers

**Files:**
- Create: `rules/thresholds.lisp`
- Modify: `rules/facts.lisp`, `rules/compile.lisp` (`compiled-rule` gains slot `thresholds`), `rules/forward.lisp` (timer thread in `forward-state`; `disable-forward-rules` stops it; `close-graph` of an enabled graph disables)
- Test: `tests/rules/forward-tests.lisp`, `tests/rules/compile-tests.lisp`; `docs/rules.md`

**Interfaces:**
- Produces:
  - `now/1`: binds `?t` to `*evaluation-instant*` (a special bound by `run-rules` and by `forward-step` to `(local-time:now)` once per call/step); withheld from free text.
  - `instant-offset/3 (?x duration ?y)`: `?y` = `?x` plus an ISO-8601 duration of the forms `PnW`, `PnD`, `PTnH`, `PTnM`, `PTnS` and their combinations; `P1M`/`P1Y` are refused (`query-precondition-error`, "months and years are not fixed durations").
  - Compile: `compiled-rule-thresholds`, a list of `(goal direction source)` where `direction` ∈ `(:gain :loss)` and `source` says how to compute the flip instant from a bound claim (which `claim-*-at` functor and which offset). Any `?t` use outside the allowed shape → `rule-compile-error` naming the goal.
  - `(flip-instant threshold bindings)` → timestamp; `(schedule-threshold state compiled premise-claims instant)`; the timer thread (`bt:make-thread`, one per `forward-state`, condition-wait with timeout on a sorted list) enqueues a +1 delta of the premise claims and calls `forward-step`.
  - `enable-forward-rules` schedules timers for every threshold rule's current matches (the run-rules it performs evaluates them).
  - Extent: a gain threshold sets the derived claim's validity start to the flip instant (via `%premise-extent`'s policy input); a loss threshold's match is retracted at the instant.

- [ ] **Step 1: Failing tests**

```lisp
(test a-gain-threshold-derives-at-its-flip-instant-with-validity-from-it)
  ;; rule: (claim-recorded-at ?c ?r) (instant-offset ?r "PT1S" ?d)
  ;;       (now ?t) (instant>= ?t ?d) ; commit premise; not derived
  ;; immediately; derived within 2 s; claim-valid-from = ?d.
(test a-loss-threshold-retracts-at-its-flip-instant)
(test a-reopened-store-still-fires-a-pending-threshold)   ; acceptance 5
(test an-unbounded-now-comparison-is-refused-at-compile)   ; acceptance 6
  ;; (now ?t) (instant< ?t ?x) with ?x from claim-object-key
(test closing-the-graph-stops-its-timers-and-drops-its-queue) ; RF 3
(test instant-offset-refuses-months)
```

- [ ] **Step 2: Run** the first. Expected: FAIL, unknown functor `now/1`.
- [ ] **Step 3: Implement** per Produces; the ISO duration parser is a small hand-written reader in `thresholds.lisp`.
- [ ] **Step 4: Run the six, then the suite.** Expected: PASS.
- [ ] **Step 5: Docs and commit.** `docs/rules.md`: functor rows, the threshold shape and the refusal, timers are in-memory and rebuilt by `enable-forward-rules`.

```bash
git add rules/thresholds.lisp rules/facts.lisp rules/compile.lisp rules/forward.lisp rules/package.lisp graph-db.asd tests/rules/forward-tests.lisp tests/rules/compile-tests.lisp docs/rules.md
git commit -m "rules: now/1, instant-offset/3 and self-scheduling thresholds (D3, spec §5.2)"
```

---

### Task 8: Productions

**Files:**
- Create: `rules/productions.lisp`
- Modify: `rules/schema.lisp` (`production` record via `def-source`, slots `name version family body salience writes action`; `(def-claim-classes firing ,graph-name)`), `rules/compile.lisp` (`%edges`/`%strata` include productions; `%validate-rule-writes` sibling for `production` records), `rules/forward.lisp` (agenda after the Datalog strata of each iteration), `rules/package.lisp`, `graph-db.asd`
- Test: `tests/rules/forward-tests.lisp`, `tests/rules/schema-tests.lisp`, `tests/rules/compile-tests.lisp`; `docs/rules.md`, manual

**Interfaces:**
- Produces:
  - `(def-production name &key version family body (salience 0) writes action)`; `action` a function designator registered under `name` in `*production-actions*`; `(undef-production name)`.
  - Stored `production` record: `action` is a string naming a registered action; a write naming none is refused at commit (`%validate-production-writes` on `*commit-validators*`).
  - `compiled-production` struct: `spec goals vars premise-vars reads negative-reads writes stratum`; `compile-production (graph spec)`; a body with no `claim/7` goal is refused; one stratification graph: `%edges` adds `writes → reads` edges for productions; a cycle through a production is `rule-compile-error`.
  - Instantiation key: the premise claims' identity keys sorted and joined with `|`, then hashed to a hex string by a small FNV-1a-64 implemented in `productions.lisp` (`sxhash` is not stable across images and no hashing library is a dependency). The hex string is `firing`'s subject key; the joined plain string is kept in the match plist so a test can read which premises fired.
  - `firing` claim: unary, relation = production name, producer = `production/NAME`, standing `:inferred`, `rule-version` = production version; retracted on `:lost`, re-opened on regain (Task 1's path).
  - Agenda: `(run-agenda graph entries)` sorted by `(salience desc, epoch asc, key asc)`; the action is called `(funcall action event match)` with `match` a plist `(:bindings alist :premises claims)`; a signalling action is caught, recorded on `failures`, agenda continues. An action may `with-transaction`; its commit's hook enqueues on the running step (Task 6's contract gives this for free since `running-p` is set). A write outside `writes` is detected from that delta and recorded as a `rule-run-refusal` on `failures`.

- [ ] **Step 1: Failing tests**

```lisp
(test a-production-fires-added-once-per-instantiation-and-lost-once)
(test a-production-survives-a-premise-being-swept-and-re-derived)  ; identity keys
(test a-production-reading-what-it-writes-is-refused)
(test a-stored-production-naming-an-unregistered-action-is-refused-at-commit)
(test a-production-that-writes-then-signals-keeps-both-and-is-reported) ; RF 4
(test agenda-order-is-salience-then-epoch)
(test firing-claims-are-readable-and-carry-the-production-version)
```

- [ ] **Step 2: Run** the first. Expected: FAIL, `def-production` undefined.
- [ ] **Step 3: Implement** per Produces.
- [ ] **Step 4: Run the seven, then the suite.** Expected: PASS.
- [ ] **Step 5: Docs and commit.** `docs/rules.md` "Productions"; manual; `CHANGELOG.md` Added.

```bash
git add rules/productions.lisp rules/schema.lisp rules/compile.lisp rules/forward.lisp rules/package.lisp graph-db.asd tests/rules/forward-tests.lisp tests/rules/schema-tests.lisp tests/rules/compile-tests.lisp docs/rules.md docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "rules: productions -- def-production, one stratification graph, durable firing claims, the agenda (spec §7)"
```

---

### Task 9: Standing-query equivalence and the closing docs

**Files:**
- Test: `tests/rules/forward-tests.lisp`
- Modify: `docs/rules.md`, `docs/superpowers/specs/2026-09-06-recursive-rules-fixpoint-design.md` (notes on R5, R7, R10), `docs/vivace-graph-v3-doc.org`, `CHANGELOG.md`

**Interfaces:**
- Consumes: everything above.

- [ ] **Step 1: Failing test**

```lisp
(test standing-queries-deliver-exactly-the-set-differences-of-full-reevaluation
  ;; three queries as productions whose actions append (event rows) to
  ;; a log; a scripted sequence of 40 commits over seeded data:
  ;; creates, retractions (bodies say claim-current), a query whose
  ;; result exceeds 500 rows, a commit touching nothing read;
  ;; after each commit, select the query fresh and diff against the
  ;; previous result; the production's deliveries since the last
  ;; commit must equal (added . removed) as sets.
  )
```

- [ ] **Step 2: Run.** Expected: FAIL until the harness is written, then (if Tasks 4–8 are right) PASS on first complete run; a mismatch is a defect in an earlier task, fixed there with its own test.
- [ ] **Step 3: Docs.** The fixpoint spec's R5/R7/R10 notes; `docs/rules.md` cross-links; the manual's rules chapter gets the forward section; `CHANGELOG.md` entries consolidated under `[Unreleased]`.
- [ ] **Step 4: Run the suite.** Expected: green.
- [ ] **Step 5: Commit.**

```bash
git add tests/rules/forward-tests.lisp docs/rules.md docs/superpowers/specs/2026-09-06-recursive-rules-fixpoint-design.md docs/vivace-graph-v3-doc.org CHANGELOG.md
git commit -m "rules: standing-query equivalence test and the forward-evaluation docs (spec §8, §9)"
```

---

## Self-review notes

- Spec coverage: D1 (Task 4 follow-on transaction, Task 6 hook outside
  `*transaction*`), D2 (Task 2), D3 (Task 7), D4 (Task 1), D5 (Task 6);
  §3 delta and cache (Task 4), §4.1 (Task 4), §4.2 (Task 5), §4.3 loop
  (Tasks 4, 6, 8), §4.4 (Task 6), §5.3 opt-in (Task 4, close in Task
  7), §6 seam and nesting (Tasks 3, 6), §7 (Task 8), §8 (Task 9), §9
  docs across tasks. Acceptance 10 is a review item, not a test.
- Type consistency: `forward-state` slots named in Task 4 are the ones
  Tasks 6–8 extend; `:sweep-keys` and `:all-goals` keywords are used
  only as defined in Task 4; `run-forward-strata` is the single entry
  Tasks 5 and 6 build on.
- Proportion: the plan carries signatures, tests and rulings, not
  bodies; the one code block (the runner script) is exact copy.
