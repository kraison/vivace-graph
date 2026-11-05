# Forward step reads (#457) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A forward or targeted rules step reads claims in proportion to
what it derives, keeps or sweeps, never to the touched producer's whole
derivation.

**Architecture:** `%stratum-run`'s `existing` table becomes a resolver
that the forward path fills lazily through the subject-relation index;
`%reconcile-claims` and `%reconcile-provenance` walk the step's own keys
and claims when given `:sweep-keys`; the full path keeps its enumerating
reads. One status field and one perf bench make the cost visible.

**Tech Stack:** Common Lisp (SBCL primary), FiveAM, the `graph-db/rules`
and `graph-db/perf-test` systems.

**Spec:** `docs/superpowers/specs/2026-11-05-forward-step-reads-design.md`

## Global Constraints

- Lisp: spaces only, 80 columns, terse comments that point at the spec.
- Every `#+sbcl`/`#+ccl`/`#+ecl`/`#+lispworks` arm stays; nothing here
  needs a new one.
- A test that swaps a function with `(setf fdefinition)` names a
  function in another file than its callers, or one declaimed
  `notinline` (CLAUDE.md engine traps).
- Test and helper names are unique across `tests/rules/` (grep first).
- Run only the rules suite per task:
  `tests/rules/run-test.sh rules-suite` (780 checks today); one test:
  `tests/rules/run-test.sh TEST-NAME`. CI runs the rest on push.
- Commit per task; docs travel with the code (Task 5 carries them).

## Review Focus

1. A temporal family: the dedupe key carries the extent start, so the
   resolver must match a claim by it, not by endpoints alone. Task 1's
   resolver test uses `rtt-claim`.
2. A derivation record under the producer that is not `derived-from`
   and whose subject the step swept must still be retracted. Task 3
   pins it.
3. Two current claims of one dedupe key under one producer: the per-key
   path keeps the first; accepted in spec §3, no test, named in
   `docs/rules.md`.
4. A key `%round-delta` constructed this run is resolved again at the
   fixpoint through the table, so `kept` counts it and `finish-run`'s
   `(decf kept derived)` stays right. Task 2 asserts the report counts
   of an existing forward test are unchanged.
5. The targeted path's dependents were read outside its transaction:
   every sweep key is resolved inside it through the resolver, never
   retracted as the object the caller holds. Task 2 keeps `sweep` a
   key -> T table; the construct branch of the per-key reconcile is
   pinned by a unit test with a desired key that stands nowhere.

---

### Task 1: The per-identity resolver

**Files:**
- Modify: `rules/run.lisp` (`%seeded-existing` ~908, `%round-delta`
  ~787, `%stratum-run` struct ~895, `reset-run` ~1055,
  `%claim-by-identity-key` ~1842 as the model)
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Produces: `(defstruct (%existing (:constructor %make-existing)) table
  lazy-p graph compiled)`; `(%claim-by-dedupe-key graph compiled key)
  => claim | NIL`; `(%existing-claim existing key) => claim | NIL`;
  `(%note-existing existing key claim)`; `(%seeded-existing graph
  compiled) => %existing` (table full, `lazy-p` NIL); `(%lazy-existing
  graph compiled) => %existing`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test claim-by-dedupe-key-prefers-the-current-claim-over-a-retracted-twin
  ;; two rt-claim unary claims, same producer "rule/x", same key; retract
  ;; one; (%claim-by-dedupe-key g compiled key) is the current one;
  ;; an identity nothing has => NIL.
  ...)

(test claim-by-dedupe-key-matches-a-temporal-claim-by-its-extent-start
  ;; rtt-claim: two claims differing only in extent start; the key with
  ;; one start resolves to that claim, not the other.
  ...)

(test a-lazy-existing-caches-what-it-resolves
  ;; (%lazy-existing g compiled): first %EXISTING-CLAIM resolves and the
  ;; key is now in the table (a miss is cached too, as NIL); a seeded one
  ;; has lazy-p NIL.
  ...)
```

`compiled` comes from `(graph-db.rules::compile-rule g spec)` of a rule
written with `write-rule` (see `tests/rules/forward-tests.lisp` ~314).

- [ ] **Step 2: Run them to see them fail**

Run: `tests/rules/run-test.sh claim-by-dedupe-key-prefers-the-current-claim-over-a-retracted-twin`
Expected: FAIL, `%CLAIM-BY-DEDUPE-KEY` undefined.

- [ ] **Step 3: Implement the resolver in `rules/run.lisp`**

`%claim-by-dedupe-key`: `index-lookup` on the family parent over
`graph-db::+claim-subject-relation-index-slots+` with `(sns skey rel)`
from the key, keep candidates whose producer is `(rule-producer spec)`
and whose `%existing-key` is `EQUAL` to the key, current first. The
struct and `%existing-claim`/`%note-existing` as in Interfaces;
`%seeded-existing` returns a seeded struct; `%round-delta` calls
`%existing-claim` and `%note-existing`s the claim it kept or
constructed; `reset-run` builds `%lazy-existing` when `forward-delta`,
else `%seeded-existing`.

- [ ] **Step 4: Run the rules suite**

Run: `tests/rules/run-test.sh rules-suite`
Expected: all green, count 780 + the new checks.

- [ ] **Step 5: Commit**

```bash
git add rules/run.lisp tests/rules/forward-tests.lisp
git commit -m "rules: the per-identity resolver; the forward path seeds nothing (#457 spec §2)"
```

### Task 2: The reconcile walks the step's keys

**Files:**
- Modify: `rules/run.lisp` (`%reconcile-claims` ~512, `finish-run`
  ~1195, `%rederive-dependents` ~1660)
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Consumes: Task 1's `%existing-claim`, `%lazy-existing`.
- Produces: `%reconcile-claims` gains `&key existing`; with
  `:sweep-keys` it walks `order` and `sweep-keys` (key -> T), every key
  resolved through `existing`. A test macro
  `with-counted-producer-reads ((counts) &body)`: `counts` an EQUAL
  hash of claim-class symbol -> number of `claims-by-producer` calls,
  the swap restored on exit.

- [ ] **Step 1: Write the failing tests**

```lisp
(test a-forward-step-never-enumerates-the-producers-claims
  ;; "flagged" rule as in a-premise-with-many-dependents-takes-the-
  ;; targeted-path; enable; seed 50 "up" claims; inside
  ;; with-counted-producer-reads commit the "mode" claim (forward step)
  ;; then retract it (targeted step).
  (is (eql 0 (gethash 'rt-claim counts 0)))
  ;; and the derivation is as before:
  (is (= 50 (length (derived g 'rt-claim "flagged" :current nil))))
  (is (null (derived g 'rt-claim "flagged"))))
```

```lisp
(test the-per-key-reconcile-constructs-a-desired-key-that-stands-nowhere
  ;; inside one transaction: %reconcile-claims with a hand-built DESIRED
  ;; of one key no claim has, ORDER of it, :sweep-keys an empty table and
  ;; :existing a lazy resolver => one claim constructed, report derived
  ;; = 1, and the claim is in the resolver's table afterwards.
  )
```

The existing `a-premise-with-many-dependents-takes-the-targeted-path`
assertions (`swept` = 300, path `:targeted`) are the Review Focus 4
check and stay.

- [ ] **Step 2: Run it to see it fail**

Run: `tests/rules/run-test.sh a-forward-step-never-enumerates-the-producers-claims`
Expected: FAIL, `rt-claim` count 2 or more.

- [ ] **Step 3: Implement the per-key reconcile**

In `%reconcile-claims`, when `sweep-p`: for each key of `order`,
`(%existing-claim existing key)`; found: the kept branch as today;
none: construct, `%note-existing` it. Then each key of `sweep-keys`
absent from `desired`: `%existing-claim`; retract when current. Without
`sweep-p`: the body as it stands. `finish-run` passes `:existing
(%stratum-run-existing run)`; `%rederive-dependents` passes `:existing
(%lazy-existing graph compiled)` and keeps `sweep` as key -> T.

- [ ] **Step 4: Run the rules suite**

Run: `tests/rules/run-test.sh rules-suite`
Expected: green.

- [ ] **Step 5: Commit**

```bash
git add rules/run.lisp tests/rules/forward-tests.lisp
git commit -m "rules: the forward and targeted reconcile resolve per key, never the producer (#457 spec §3)"
```

### Task 3: Provenance per touched claim

**Files:**
- Modify: `rules/run.lisp` (`%reconcile-provenance` ~577)
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Consumes: Task 2's `with-counted-producer-reads`.

- [ ] **Step 1: Write the failing tests**

```lisp
(test a-forward-step-never-enumerates-the-producers-derivation-records
  ;; same fixture as Task 2's test;
  (is (eql 0 (gethash 'graph-db.rules::derivation counts 0)))
  ;; provenance intact: premises-of a derived claim names its "up"
  ;; premise and the "mode" claim.
  )

(test a-swept-claims-foreign-record-under-the-producer-is-retracted
  ;; after the forward derivation, hand-write a DERIVATION binary claim:
  ;; relation "other", subject (:claim <derived identity key>), object
  ;; (:claim "x"), producer "rule/flagged"; retract "mode"; the hand
  ;; record is no longer claim-current-p.
  )
```

- [ ] **Step 2: Run them to see them fail**

Run: `tests/rules/run-test.sh a-forward-step-never-enumerates-the-producers-derivation-records`
Expected: FAIL, count 1 or more.

- [ ] **Step 3: Implement**

With `sweep-p`, replace the `claims-by-producer` loop by: for each
claim of `standing` and of `swept`, `(claims-touching graph 'derivation
:claim (claim-identity-key claim) :role :subject)`, each record kept
only when its producer is the rule's, then the same `cond` as today.
Without `sweep-p`, unchanged.

- [ ] **Step 4: Run the rules suite**

Run: `tests/rules/run-test.sh rules-suite`
Expected: green.

- [ ] **Step 5: Commit**

```bash
git add rules/run.lisp tests/rules/forward-tests.lisp
git commit -m "rules: provenance reconciled per touched derived claim on the delta paths (#457 spec §3)"
```

### Task 4: `:last-step-elapsed` on the status

**Files:**
- Modify: `rules/forward.lisp` (`forward-state`, `forward-step`,
  `forward-rules-status`)
- Test: `tests/rules/forward-tests.lisp`

**Interfaces:**
- Produces: `forward-rules-status` plist gains `:last-step-elapsed`, a
  double-float of seconds or NIL.

- [ ] **Step 1: Write the failing test**

```lisp
(test the-status-reports-the-last-steps-elapsed-time
  ;; enabled, before any commit: (getf status :last-step-elapsed) is NIL;
  ;; after one committed premise: a number >= 0.
  )
```

- [ ] **Step 2: Run it to see it fail**

Run: `tests/rules/run-test.sh the-status-reports-the-last-steps-elapsed-time`
Expected: FAIL, NIL after the commit.

- [ ] **Step 3: Implement**

A `last-step-elapsed` slot (NIL) on `forward-state`; `forward-step` sets
it in its `unwind-protect` cleanup from `get-internal-real-time` taken
before the loop, under the state lock; the status reads it.

- [ ] **Step 4: Run the rules suite**

Run: `tests/rules/run-test.sh rules-suite`
Expected: green.

- [ ] **Step 5: Commit**

```bash
git add rules/forward.lisp tests/rules/forward-tests.lisp
git commit -m "rules: forward-rules-status reports the last step's elapsed time (#457 spec §4)"
```

### Task 5: The bench and the docs

**Files:**
- Modify: `tests/perf/rules-bench.lisp` (new `bench-forward-step-reads`),
  `tests/perf/benchmarks.lisp:1019` (call it after
  `bench-claim-key-paging`), `docs/rules.md` ("Forward evaluation /
  Cost" ~213, the status plist ~1164),
  `docs/superpowers/specs/2026-11-04-forward-rules-design.md` §4.1
  item 3.

**Interfaces:**
- Produces: labels `forward-step-small`, `forward-step-large` (seconds)
  and `forward-step-scale-ratio` (`:ratio` large over small).

- [ ] **Step 1: Write the bench**

Own graph name `:graph-db-perf-forward` with its own `def-source`,
`def-claim-classes` and `def-rules-schema` (the generation rule). N =
`(scale 20000)`, overridable by `:n`; for each of N/10 and N: fresh
graph, N base unary claims under relation "base" in batches of 500,
one rule `derived(?k) :- base(?k)` written as the forward tests write
one, `enable-forward-rules` under `*rules-max-solutions*` and the
query budgets bound above N, a check that N claims stand under the
rule, one untimed warm-up commit, then the wall time of one
transaction committing one more base claim (the step runs on the
committing thread before `with-transaction` returns), checked to have
derived its claim. Record the ratio in milliseconds.

- [ ] **Step 2: Run it at default scale**

Run (in the perf image, see `tests/perf/README.md`):
`(graph-db/perf-test::bench-forward-step-reads)` after
`(ql:quickload :graph-db/perf-test)`.
Expected: three records; ratio near 1.

- [ ] **Step 3: Run it at N = 10^5 and put the numbers on #457**

Same call with `:n 100000`; if it runs over ten minutes, under
`systemd-run --user`. Comment on #457 with the three numbers.

- [ ] **Step 4: Docs**

`docs/rules.md` Cost paragraph: a step resolves each identity it
derives through the subject-relation index and reads provenance per
touched derived claim; the full path still reads the producer; the
duplicate-current-claim note (spec §3); the bench name. The status
plist gains `:last-step-elapsed`. The forward spec's §4.1 item 3 gets
one sentence pointing here.

- [ ] **Step 5: Commit**

```bash
git add tests/perf/rules-bench.lisp tests/perf/benchmarks.lisp docs/rules.md docs/superpowers/specs/2026-11-04-forward-rules-design.md
git commit -m "rules: forward-step bench and the cost docs (#457 spec §5, §6)"
```
