# Segment Reservation — Wave 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Remove the vector segment's growth ceiling, rather than only making hitting it safe (which wave 1 did).

**Architecture:** Task 1 gives segments their own reservation floor so exhaustion becomes rare. Task 2 makes exhaustion recoverable by re-reserving and relocating under the segment's existing write lock — completing `mmap-remap-race-plan.md` Phase 3's unimplemented final sentence.

**Spec:** [`../specs/2026-07-22-segment-reservation-exhaustion-design.md`](../specs/2026-07-22-segment-reservation-exhaustion-design.md), Parts 2 and 4.

**Wave 1 is done** (`20200c9`): capacity is validated pre-durability, and rebuilds are corpus-sized. Suite baseline **2559 checks, 0 fail**.

## Why Part 3 is deferred — now confirmed, not just assumed

The spec ordered Parts 2 → 3 → 4 by ambition. Part 3 (adjacent re-reservation) requires
`MAP_FIXED_NOREPLACE`, which is Linux-only, so it was deferred as untestable on macOS.

**odm cannot exercise Part 3**: it runs kernel **4.15.0-213**, and `MAP_FIXED_NOREPLACE` needs
**4.17+**, so the flag is ignored there.

⚠ **Correction (2026-07-22, measured).** An earlier revision of this paragraph said the ignored
flag "degrades to plain `MAP_FIXED` and *replaces* whatever occupies the target address" — a
clobber hazard. **That was wrong, and it was asserted rather than tested.** A C probe on odm
shows the ignored flag falls back to *advisory hint* placement: the mapping landed at a
different address and the sentinel in the occupied page was **intact**. An unknown mmap flag is
ignored, which leaves the address as a hint; it does not imply `MAP_FIXED`.

So Part 3 on a pre-4.17 kernel is **useless, not dangerous** — the claim usually lands elsewhere
and is unwound by a post-hoc address check. See the spec's Part 3 for the corrected analysis.

Part 4 is plain POSIX, testable on both hosts, and **on its own removes the ceiling**. Part 3 was
only ever an optimisation that avoids the relocation cost. It stays deferred until a host with a
4.17+ kernel exists.

## Global Constraints

- **This is the engine.** mine-action (macOS dev hub, odm/Linux) depends on it. Branch `experiment`; do not create branches, push, or merge.
- **Suite baseline: 2559 checks, 100% pass.** Run with:
  ```
  cd /Users/kraison/work/vivace-graph-v3
  sbcl --dynamic-space-size 8192 --non-interactive \
    --eval '(ql:register-local-projects)' \
    --eval '(ql:quickload :graph-db/test :silent t)' \
    --eval '(funcall (read-from-string "graph-db/test::run-tests"))' 2>&1 | tail -8
  ```
- **Spaces, never tabs.** Ignore ECL — no live consumer.
- **Search boundary:** only within `/Users/kraison/work/vivace-graph-v3`.
- `git add` only each task's named files. Unrelated untracked paths exist (`.local/`, `tools/`, `docs/android-*.md`, `docs/ecl-change-class-leak-report.md`). Never `git add -A`.
- Add a `CHANGELOG.md` `[Unreleased]` entry in each task — the wave-1 precedent.

---

### Task 1: A reservation floor for segments (spec Part 2)

**Files:** `globals.lisp`, `segment.lisp`, `tests/segment-integration-tests.lisp`, `tests/segment-tests.lisp`, `CHANGELOG.md`

**Interfaces:** produces `*segment-min-reservation*`. Task 2 consumes nothing from it.

- [ ] **Step 1: Understand the arithmetic before writing anything**

`mmap-file` computes `reserved = max(reservation ∥ multiplier×size, *mmap-min-reservation*, size)`
(`mmap.lisp:192-195`), **at open/create time from the file's size then**. Segments currently pass
no `:reservation`, so they inherit the general 8× rule meant for schema-sized index files.

Measured effect of a floor at dimension 1024 (4,112 bytes per slot):

| floor | slots |
| --- | --- |
| 4 GiB | 1,044,495 |
| 8 GiB | 2,088,991 |
| **16 GiB** | **4,177,983** |
| 32 GiB | 8,355,967 |

Reservation is `PROT_NONE` + `MAP_NORESERVE` anonymous address space — no RAM, no disk, no commit
charge. On 64-bit, a large floor costs nothing real.

- [ ] **Step 2: Add the global**

In `globals.lisp`, beside `*mmap-reservation-multiplier*` and `*mmap-min-reservation*` (lines
92-95), add `*segment-min-reservation*` defaulting to **16 GiB**, with a comment explaining: the
general 8× rule is sized for schema-sized index files, whereas a vector segment's size tracks the
corpus; and that the reservation is address space only, so the floor is free on 64-bit.

- [ ] **Step 3: Pass it at BOTH call sites**

`segment.lisp:68` (`create-vector-segment`) and `segment.lisp:102` (`open-vector-segment`).

Pass `(max *segment-min-reservation* (* *mmap-reservation-multiplier* size))` — **not** the bare
floor. Preserving the multiplier matters: a segment already larger than `floor / multiplier` must
still get proportional headroom rather than being capped at the floor.

- [ ] **Step 4: ⚠ Fix wave 1's test, which this change breaks**

`capacity-exhaustion-signals-and-rolls-back` (in `tests/segment-integration-tests.lisp`) forces
exhaustion by binding `*mmap-min-reservation*` to 64 KiB and `*mmap-reservation-multiplier*` to 1.
Once segments consult `*segment-min-reservation*`, that test's segment gets 16 GiB and **can never
exhaust**, so the test would silently stop testing anything — it would still pass, having verified
nothing.

Bind `*segment-min-reservation*` low in that test too. Then **verify the test still discriminates**
by temporarily disabling the `validate-vector-segment-capacity` call site (`#+(or)`) and
confirming it fails, exactly as was done when it was written. Restore afterwards. Report both
outcomes — a test that passes without being re-proven is the failure mode this whole spec exists
to prevent.

Check the rest of the suite for any other test that binds the mmap reservation globals and would
be similarly neutered.

- [ ] **Step 5: Write tests for the floor itself**

Assert, reading `(m-reserved-size (segment-mmap seg))`:
- a freshly **created** segment gets at least `*segment-min-reservation*`;
- a **reopened** segment gets at least it too (the open path is a separate call site and is the one
  that was originally missed);
- when `multiplier × size` exceeds the floor, the **larger** value wins — bind the floor small and
  the multiplier large to construct this, so Step 3's `max` is genuinely covered rather than
  assumed.

- [ ] **Step 6: Run the full suite**

Expected 2562+ checks, 0 failures. Any drop is a regression.

- [ ] **Step 7: Commit**

Print the full diff in a `## 📋 DIFF FOR REVIEW` block, then commit:
`feat(segment): a reservation floor sized for corpora, not schemas`

---

### Task 2: Re-reserve and relocate under the write lock (spec Part 4)

**Files:** `mmap.lisp`, `segment.lisp`, `transactions.lisp`, `tests/segment-tests.lisp`, `CHANGELOG.md`

**Interfaces:** consumes Task 1's floor (only in that exhaustion becomes rarer). Produces a relocation primitive in `mmap.lisp` and changes when `validate-vector-segment-capacity` signals.

**Read `../specs/…-design.md` §1.3 and §1.7 before starting.** The reservation exists because
`m-pointer` never moves, which is what makes reads lock-free. Relocation is safe *for the segment*
only because it already holds an rw-lock at every public entry point — a property the heap and
linear hash do not have.

- [ ] **Step 1: The relocation primitive**

Add to `mmap.lisp`: reserve a new, larger `PROT_NONE` window; `MAP_FIXED`-map the file into its
head; update `m-pointer` and `m-reserved-size`; `munmap` the old window.

**Its docstring must state the safety contract in the strongest terms available:** callers must
hold write-exclusive access over *every* reader of that mapping, and **the heap and linear hash do
not qualify** — they have no read lock, which is precisely what Phase 3 bought. Name it so misuse
looks wrong at the call site.

- [ ] **Step 2: Call it from `%seg-grow` on exhaustion**

`%seg-grow` already runs under the segment write lock (`segment-put` at `segment.lisp:397`,
`segment-remove` at 680). On exhaustion, relocate instead of signalling; signal only if relocation
itself fails (VA exhaustion / ENOMEM).

- [ ] **Step 3: ⚠ Resolve the interaction with wave 1's pre-durability check**

**This is the design question of this task; do not skip past it.**

`validate-vector-segment-capacity` currently aborts when the required capacity would exceed the
*current* reservation. Once Step 2 makes that recoverable, the check becomes over-eager: it would
roll back transactions that would now succeed.

Two candidates. **I recommend (a); take (b) only if you find a defect in the reasoning.**

- **(a) Grow during validation — recommended.** Perform the grow (including relocation) in the
  pre-durability region, so `apply-transaction` provably cannot need to grow. This is airtight
  rather than probabilistic: validation and apply both run under the manager lock and are
  serialised, so nothing can consume the capacity in between.

  Trade-offs, both acceptable, but state them in the code rather than leaving them implied:
  - A transaction that later fails (only `finalize-tx-persistence` remains after this point)
    leaves an over-sized segment. Capacity is not semantic and `live-count` is untouched, so the
    segment stays consistent; the file is merely larger than it needed to be.
  - A crash mid-grow leaves the segment dirty, so `restore-vector-segments` rebuilds it at open.
    That is the existing recovery path and is not made worse — but note wave 1 is what made that
    rebuild survivable above 131k, so (a) leans on wave 1 having landed.

- **(b) Keep the check, raise its bound.** Validate against what relocation could plausibly
  achieve. Rejected unless (a) proves unworkable: "plausibly" is not knowable, so it trades a
  guarantee for a heuristic — and the guarantee is the entire point of wave 1.

**Whichever you choose, the wave-1 invariant must survive: a failure must never leave a persisted
node with no segment entry.** There is a test for exactly that; it must still pass, and must still
**discriminate** — re-prove it, because making exhaustion recoverable is precisely the kind of
change that silently neuters an exhaustion test. That has already happened twice in this work.

- [ ] **Step 4: Tests**

- relocation moves `m-pointer`, and the data is byte-identical afterwards;
- a reader blocked on the segment lock during relocation sees consistent data after it;
- growth past the original reservation now **succeeds** rather than signalling — the direct
  inverse of wave 1's exhaustion test;
- the wave-1 exhaustion test still discriminates under whatever Step 3 chose (re-prove it by
  disabling the relevant code path, as in Task 1 Step 4).

Run the concurrency suite (`graph-db/concurrency-test`) as well as the main one; this changes
memory mapping under a lock.

**Run on both hosts.** A change that moves `m-pointer` deserves two platforms. An isolated
sandbox exists on odm (Linux, SBCL 2.1.11 — vs the dev hub's macOS, SBCL 2.5.5):

- `~/part4-test/vivace-graph-v3` on `raison@odm.chatsubo.net`, branch `part4`
- run with `~/part4-test/run-suite.sh`, which fences `CL_SOURCE_REGISTRY`,
  `ql:*local-project-directories*`, `ASDF_OUTPUT_TRANSLATIONS` and `TMPDIR`, and **asserts the
  loaded `graph-db` is the sandbox copy**, exiting 9 otherwise
- move code there by `git bundle` — **never push to origin**, never touch
  `~/work/vivace-graph-v3`, `~/quicklisp/local-projects/`, `/var/tmp/mine-action`, or `systemctl`

Record the sandbox's own baseline before changing anything: odm's older SBCL could have a
pre-existing failure that would otherwise be mistaken for this change's.

- [ ] **Step 5: Full suite + concurrency suite, then commit**

Print the diff in a `## 📋 DIFF FOR REVIEW` block, then commit:
`feat(segment): re-reserve and relocate instead of failing on exhaustion`

---

## Out of scope

- **Part 3** — adjacent re-reservation. Deferred; see the note at the top.
- Anything for the heap or linear hash. They share the ceiling but not the rw-lock, so relocation is not available to them.
