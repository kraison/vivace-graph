# Registration cutover (task 6b) — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The place spine stops computing registrations itself and asks the
substrate, without relabelling, double-writing or mis-counting a single one
of its 345,939 deployed claims.

**Architecture:** One engine change (`:method-fn`, so a source whose method
is a per-record fact can say so), three tenant prerequisites, then the
cutover of three rules, then the deletion of what they no longer do — and
last, a proof against deployed data that the substrate writes what the old
path wrote.

**Tech Stack:** Common Lisp (SBCL), ASDF, FiveAM, `graph-db`,
`graph-db/spacetime`, `graph-db/geos`.

**Spec:** `docs/superpowers/specs/2026-08-19-registration-design.md`
(engine) and mine-action's `docs/spine-open-items.md` §9, which is the
cutover's hazard list and was written from deployed data. Executors read
both. §9 is authoritative on what the tenant does today.

## Global Constraints

- **80 columns, hard**, counted in **codepoints not bytes**. Spaces only,
  never tabs.
- Comments terse, pointing at a doc, a GH issue or a SHA.
- **A live mine-action server owns `/data0/`.** Never open, write to, or
  delete anything there. Verification runs against a `cp -a` copy. Never
  stop or signal the server.
- **Before writing in the tenant repo**, check `git status --short` and
  `git branch --show-current`: expect clean, on `main`. If not, STOP —
  another session works there. Stage explicit paths; never `git add -A`.
- **Never run two SBCLs at once.** Check `ps -C sbcl -o pid,cmd` first;
  the resident `run-server` and `cl-mcp` processes are not builds.
- Run tests detached and poll by PID: `nohup sbcl --non-interactive ... >
  /tmp/log 2>&1 &`, `echo $! > /tmp/x.pid`, then SEPARATELY
  `P=$(cat /tmp/x.pid); while kill -0 $P 2>/dev/null; do sleep 20; done;
  grep -E 'Did [0-9]+ check|Pass:|Fail:|Skip:' /tmp/log`, timeout
  ≥ 900000 ms. Never `pgrep -f 'sbcl --non-interactive'`.
- **Baselines:** engine `graph-db/spacetime-test` **325 / 0 skips / 0
  fails**, `graph-db/test` **3684 / 10 skips / 0 fails**; tenant
  **5621 / 1 skip / 0 fails**.
- Do not push. Do not bump any version.
- **A test whose whole value is that it would fail must be shown to fail.**

## The three decisions this plan implements

Settled with Kevin before planning:

1. **`site`'s method becomes a per-record function.** `:method` stays and
   `:method-fn` joins the facet as a required key whose value may be
   `nil` — exactly like `:precision-fn` and `:confidence-fn`. Non-nil
   wins. The alternative, retracting the deployed `centroid-within`
   population, destroys history.
2. **`geoconfirmed-placemark` stays on the tenant path.** Its relation
   varies per concrete subclass and `source-contract` keys on the exact
   class. Prove the mechanism on the three sources that have facets.
3. **IMSMA ids are normalised at ingest**, not by a facet key function. A
   stored id with surrounding whitespace is a data defect; the substrate
   reading the declared identity slot raw is correct.

---

### Task 1: `:method-fn` in the facet, and `register-node` using it

**Files (engine):**
- Modify: `spacetime/source.lisp` (`%check-facet`'s `:registration` clause)
- Modify: `spacetime/register.lisp` (`register-node`)
- Modify: `tests/spacetime/source-tests.lisp` and
  `tests/spacetime/register-tests.lisp`
- Modify: `docs/superpowers/specs/2026-08-19-registration-design.md` §3
- Modify: `docs/vivace-graph-v3-doc.org` (the facet's shape)

**Interfaces:**
- Produces, relied on by Task 4: the facet accepts `:method-fn <fname>`,
  required key, `nil` permitted. When non-nil, `register-node` calls it
  with the subject node and writes the result as the claim's `method`;
  when `nil`, `:method`'s string is written as today.

- [ ] **Step 1: Write the failing tests**

In `tests/spacetime/source-tests.lisp`, a facet omitting `:method-fn`
entirely is refused (mirror the existing missing-key tests — read one and
match its condition and shape).

In `tests/spacetime/register-tests.lisp`, using the existing `ctr-*`
fixture: give the test source a `:method-fn` reading a special, register,
and assert the stored `claim-method` is what the function returned, not
the facet's `:method` string. Then set the special to a different value,
re-register, and assert the stored method **changed** — the update branch
is what Task 4's `site` will exercise on every re-ingest.

- [ ] **Step 2: Run and confirm RED.** Expect the facet test to fail
      because the current clause accepts a facet with no `:method-fn`, and
      the register tests to fail on the written method.

- [ ] **Step 3: Implement.** Add `(req :method-fn)` to the `:registration`
      clause — `req`, not `req-symbol`, because `nil` must be permitted;
      that is the same choice `:precision-fn` and `:confidence-fn` already
      make, and Task 3 of the previous unit recorded why. In
      `register-node`, resolve the method once alongside precision and
      confidence, under the subject's graph binding, with the same
      `%call-or-nil` helper.

- [ ] **Step 4: Run and confirm GREEN.** Report the count.

- [ ] **Step 5: Prove it load-bearing.** Make the resolution ignore
      `:method-fn` and always use `:method`. The two new register tests
      must go RED while the existing method assertion in
      `registering-a-node-writes-one-claim-per-region` stays GREEN as the
      control. Restore, re-run, confirm green and a clean `git status`.

- [ ] **Step 6: Update the spec's §3 block and the manual's facet shape**
      in the same commit — nine keys become ten.

- [ ] **Step 7: Commit** (no `[skip-docs]`; this commit carries its docs).

---

### Task 2: `node-geometry` on `site`

Without this the cutover silently registers by whichever indexed geometry
slot the default method reaches first. `site` declares both `centroid` and
`extent` as `:index t`.

**Files (tenant):**
- Modify: `src/schema.lisp` (or wherever `site` is defined — find it)
- Modify: `tests/spine-tests.lisp`

**Interfaces:**
- Produces, relied on by Task 4: `(graph-db:node-geometry <site>)` returns
  the site's `extent` when it has one, else its `centroid`.

- [ ] **Step 1: Write the failing test.** A site with both slots returns
      the extent; a site with only a centroid returns the centroid; a site
      with neither returns `NIL`. **And the assertion that matters:** the
      geometry returned is the same one `register-site` queries with —
      read `register-site`'s `(if ext (%places-intersecting-extent ext …)
      (%places-containing-point anchor …))` and assert your method agrees
      with that choice, so the two cannot drift.

- [ ] **Step 2: Run and confirm RED.** Say in your report what the default
      method returned before your change — that is the bug this prevents,
      and it is worth recording rather than assuming.

- [ ] **Step 3: Implement** a `graph-db:node-geometry` method on `site`.
      Model it on the existing one for `spine-place` (`src/spine-schema.lisp`).

- [ ] **Step 4: Run and confirm GREEN**, then the full tenant suite.

- [ ] **Step 5: Commit** with a docs note in `docs/spine-open-items.md` §9
      marking this prerequisite closed.

---

### Task 3: normalise IMSMA ids at ingest

**Files (tenant):** the IMSMA ingest path — find it with
`git grep -n 'imsma-id' src/ | head`, and read `%imsma-area-facts`'s
`nonblank` use before changing anything.

- [ ] **Step 1: Audit before fixing.** Against a **copy** of the deployed
      imsma graph, count ids that differ from their `nonblank` form and
      ids that are blank. Report the actual counts. **If both are zero,
      say so** — the fix is then a guard against a hypothetical, which is
      still worth having but should be described as one rather than as a
      repair.

- [ ] **Step 2: Write the failing test** — an ingested record whose id has
      surrounding whitespace is stored trimmed; a blank id is refused or
      skipped, matching whatever the ingest already does with bad rows
      (read it; do not invent a new policy).

- [ ] **Step 3: Run and confirm RED.**

- [ ] **Step 4: Implement** the normalisation at ingest.

- [ ] **Step 5: Run and confirm GREEN**, then the full tenant suite.

- [ ] **Step 6: Commit**, and record in §9 that the substrate may now read
      the identity slot raw because the data is clean at rest.

---

### Task 4: cut the three rules over to `register-node`

The dangerous task. Read §9 twice before starting.

**Files (tenant):** `src/spine-register.lisp`, `src/spine-backfill.lisp`,
and whatever else calls the three rules — find every caller first.

- [ ] **Step 1: Write the failing tests.** For each of `register-site`,
      `register-acled-event` and `register-imsma-hazard-area`: the claims
      written after the cutover are identical to what the rule wrote
      before — same relation, method, confidence, precision, fraction,
      rule-version and standing — for a subject with an extent AND for one
      with only a centroid. Build both cases; the centroid-only case is
      the one §9 warns would be relabelled.

- [ ] **Step 2: Run and confirm RED.**

- [ ] **Step 3: Cut each rule over**, one at a time, running the suite
      between them. Each rule keeps its own graph-crossing discipline and
      its own precision/confidence functions; the candidate query, the
      fraction math and the upsert come from `st:register-node`.

- [ ] **Step 4: Negate the flag, per rule, in the right position.**
      ⚠ `st:register-node` returns `(values claims-written EVALUATED-P)`
      where `T` is **good**. The tenant's rules report `GEOS-SKIPPED-P`
      where `T` is **bad**, and they do not agree on where it sits:
      `register-site` and `register-ssts-clearance-area` return
      `(values n skipped)`; `register-imsma-hazard-area` returns
      `(values n AGREEMENT skipped)` — the flag is **third**;
      `register-acled-event` returns a bare `n` and has no flag at all.
      There is no single mechanical rewrite. Preserve each rule's existing
      arity and flag position exactly, and negate.

- [ ] **Step 5: Prove the negation.** Force a refusal (bind
      `graph-db::*geos-available-p*` to `nil` around a polygon subject) and
      assert `spine-backfill`'s `:sites-geos-skipped` **increments** and
      the pass's `:coverage` downgrades to `:partial`. Then assert a
      successful run leaves the counter at zero **in the same test**. A
      counter that only ever goes up looks identical to one wired
      backwards; both directions must be pinned.

- [ ] **Step 6: Run the full tenant suite.** Report against 5621.

- [ ] **Step 7: Commit** with `docs/spine-open-items.md` updated.

---

### Task 5: delete what the tenant no longer does

- [ ] **Step 1:** Delete the now-dead candidate query, fraction math and
      upsert from `src/spine-register.lisp`, and any helper left with no
      callers. **Keep:** `geoconfirmed`'s whole path, and
      `imsma-hazard-area`'s exact-key claim (relation `coded-in`, method
      `exact-key`) — an authority-code join, not a geometric registration.

- [ ] **Step 2:** Delete the duplicated confidence/precision literals §9
      records — `%site-place-confidence` and its inlined `(if ext 0.95d0
      0.7d0)` twin, and the same shape for the imsma pair — leaving one
      source of truth, which is now the facet's function.

- [ ] **Step 3:** Run the full tenant suite and confirm it is still 5621
      plus whatever the new tests added, with nothing lost.

- [ ] **Step 4:** Grep for callers of everything you deleted, in `src/`
      and `tests/`, and report that the list is empty.

- [ ] **Step 5: Commit** with docs.

---

### Task 6: prove it against deployed data, and close the parked minor

The acceptance test for the whole cutover.

- [ ] **Step 1: Fix the parked minor first.**
      `tools/verify-spine-migration.lisp`'s check-1 `OK-P` omits
      `(plusp frac-informative)`, so a total loss of `fraction` would
      print PASS and exit 0 — the banner asserts more than the check can
      support for exactly the failure mode `fraction` has. Add the third
      conjunct. Exercise the new exit path against an **empty temp
      directory**, which drives both `(plusp n)` false branches, the FAIL
      banner and exit 1 in seconds — the previous round claimed this
      needed deployed data and that was wrong.

- [ ] **Step 2: Equivalence against the deployed copy.** On a `cp -a` copy
      of the spine graph, for a sample of subjects of each kind, compare
      the claims the cutover writes against the claims already there:
      same identity tuple, same method, confidence, precision, fraction,
      rule-version, standing. Report the sample size and every difference,
      not just a verdict. **A difference is a finding, not something to
      explain away** — the centroid-only relabelling §9 predicts is
      exactly what this must catch if the cutover got it wrong.

- [ ] **Step 3: Record the result** in §9, replacing the "left for task
      6b" list with what was actually done.

- [ ] **Step 4: Commit.**

---

## Self-Review

**Decision coverage.** `:method-fn` → Task 1. `geoconfirmed` stays →
Task 5 Step 1's explicit keep. IMSMA ids at ingest → Task 3.

**Hazard coverage against §9.** The relabelling risk → Tasks 1 and 4
Step 1's centroid-only case, and Task 6 Step 2's equivalence check. The
`node-geometry` prerequisite → Task 2. The inverted flag and its three
positions → Task 4 Steps 4-5. The `nonblank` key divergence → Task 3. The
duplicated literals → Task 5 Step 2. The one 0.0-fraction claim → left
alone deliberately; the substrate drops zero-fraction touches and the
retraction pass clears it, which §9 already records.

**Placeholders.** None. Where a name must be read rather than guessed —
the site definition's file, the ingest path, the callers of each rule —
the step says to find it and report what was found.

**The riskiest step** is Task 4 Step 3, and it is deliberately "one at a
time, running the suite between them" rather than a single sweep.
