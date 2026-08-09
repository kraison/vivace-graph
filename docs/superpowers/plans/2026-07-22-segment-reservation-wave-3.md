# Segment Reservation — Wave 3 (Part 3: adjacent re-reservation)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** On reservation exhaustion, try to claim the address range immediately *after* the current window before relocating. When it works, the window simply grows: `m-pointer` never moves, no reader is affected, and none of Part 4's copy cost is paid.

**Architecture:** One new helper in `mmap.lisp`, called from `%seg-ensure-reservation` ahead of the existing relocation. Relocation stays as the fallback.

**Spec:** [`../specs/2026-07-22-segment-reservation-exhaustion-design.md`](../specs/2026-07-22-segment-reservation-exhaustion-design.md), Part 3 — **read its corrected version**, not your memory of it.

**Waves 1 and 2 are done** (`84f21fe`). Baselines, identical on all three hosts: main **2636 / 0 fail**, concurrency **1664 / 0 fail**.

## The correction that makes this simpler than it was designed

An earlier revision of the spec claimed that a kernel ignoring `MAP_FIXED_NOREPLACE` degrades to
plain `MAP_FIXED` and **clobbers** the occupied range, and called that the whole difficulty of
Part 3. **That was asserted, never tested, and it is wrong.** A C probe on two kernels:

| host | kernel | result |
| --- | --- | --- |
| hypnos | 5.15.0-179 | **honoured** — rejected `EEXIST`, sentinel intact |
| odm | 4.15.0-213 | **ignored → advisory hint**: landed at a *different* address, sentinel **intact** |

An unknown mmap flag is ignored, which leaves the address argument a *hint*; it does not imply
`MAP_FIXED`. So:

- **The safety property is a post-hoc address comparison** — did we get back exactly the address
  we asked for? — and it is complete on every kernel, with **no version gate**.
- **Never pass plain `MAP_FIXED`.** That would clobber. Nothing here requires it.
- The flag is still worth passing where defined: it converts a wasted map-then-unmap into a clean
  rejection.

## Global Constraints

- **This is the engine.** Branch `experiment`; no branches, no push, no merge.
- **Spaces, never tabs.** Ignore ECL — no live consumer.
- **Search boundary:** `/Users/kraison/work/vivace-graph-v3` (plus the hypnos sandbox path when testing there).
- `git add` only files you change. Unrelated untracked paths exist (`.local/`, `tools/`, `docs/android-*.md`, `docs/ecl-change-class-leak-report.md`). Never `git add -A`.
- Add a `CHANGELOG.md` `[Unreleased]` entry.

## ⚠ The trap this wave will spring — read before writing any code

Part 4's relocation tests pass today because exhaustion *forces* a relocation. Once adjacent
extension usually succeeds, **those tests stop exercising relocation** and start silently
testing the adjacent path instead — while still passing.

This exact failure mode has now bitten four times in this line of work (a `>=` assertion the
broken code satisfied; a test the new reservation floor neutered; `store-count` as an acceptance
criterion that could not fail; an exhaustion test made unreachable by Part 4). **Assume it will
happen here and design against it:**

- Add a control (mirroring `*segment-relocate-on-exhaustion*`) that disables the adjacent attempt,
  and use it to keep Part 4's relocation tests genuinely exercising relocation.
- After implementing, **re-prove that every pre-existing relocation test still discriminates** —
  disable the relocation code path and confirm those tests fail. Report the outcomes verbatim.
- A green suite is not evidence. It is the thing that hides this.

---

### Task 1: Adjacent re-reservation

**Files:** `posix.lisp`, `mmap.lisp`, `segment.lisp`, `globals.lisp`, `package.lisp`, `tests/segment-tests.lisp`, `CHANGELOG.md`

- [ ] **Step 1: The constant**

`posix.lisp` already defines mmap flags with a platform conditional
(`+map-anonymous+`, `+map-noreserve+` use `#+graph-db-posix-linux`). Add `MAP_FIXED_NOREPLACE`
(`#x100000` on Linux) the same way. On platforms without it, contribute **no** flag bit rather
than substituting `+map-fixed+` — the address check carries the safety there.

- [ ] **Step 2: The helper**

Add to `mmap.lisp` a function that attempts to extend a mapping's reservation **in place** by
claiming the range immediately after it:

- compute the end of the current window (`m-pointer` + `m-reserved-size`);
- `mmap` there: length = the shortfall, `PROT_NONE`, `MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE`
  plus the new flag where available;
- **if the returned address is not exactly the requested one, `munmap` what you got and return
  failure** — this is the safety property, and it must run on every platform, not just where the
  flag is missing;
- on success, add the shortfall to `m-reserved-size` and return success. `m-pointer` must not
  change.

Document why plain `MAP_FIXED` is never used, with the probe result as the reason.

- [ ] **Step 3: Wire it into `%seg-ensure-reservation`**

Try adjacent extension first; on failure fall back to the existing relocation; on that failure,
signal as today. Keep the existing condition and its diagnostics; consider recording which path
was taken so an operator can tell a cheap extension from a relocation.

- [ ] **Step 4: The control**

Add a global mirroring `*segment-relocate-on-exhaustion*` that disables the adjacent attempt.
Export it. It is both an operator kill-switch and — per the trap above — the only way to keep
Part 4's relocation tests honest.

- [ ] **Step 5: Tests**

1. **Adjacent extension succeeds**: force a small reservation, grow, assert `m-reserved-size`
   increased and **`m-pointer` is unchanged** — the direct inverse of Part 4's relocation test.
2. **Adjacent range occupied → falls back cleanly.** Deliberately map something at the adjacent
   address with a **sentinel value** in it, then force a grow. Assert: the fallback relocation
   happened (`m-pointer` changed), the segment's data is intact, **and the occupying mapping's
   sentinel is still intact.** This is the test that directly disproves the clobber hazard I
   mischaracterised — write it even though the probe already showed it, because the probe tested
   the kernel and this tests *our code*.
3. **Address-check safety net**: fault-inject `%posix-mmap` (the `fdefinition`-swap idiom already
   used in `tests/segment-tests.lisp`) to return a *different* address than requested. Assert we
   unmap it, do **not** touch `m-reserved-size`, and fall through to relocation.
4. **Re-prove Part 4 still discriminates** (the trap): with the adjacent attempt disabled, the
   existing relocation tests must still fail when the relocation path is disabled.

- [ ] **Step 6: Run everything, on all three hosts**

Baselines are identical everywhere: main **2636 / 0**, concurrency **1664 / 0**.

- **Dev hub** (macOS, SBCL 2.5.5) — no `MAP_FIXED_NOREPLACE`, so this is the platform where the
  **address check alone** is the safety property. It must pass here.
  ```
  cd /Users/kraison/work/vivace-graph-v3
  sbcl --dynamic-space-size 8192 --non-interactive --eval '(ql:register-local-projects)' \
    --eval '(ql:quickload :graph-db/test :silent t)' \
    --eval '(funcall (read-from-string "graph-db/test::run-tests"))' 2>&1 | tail -6
  ```
  and the same with `:graph-db/concurrency-test` / `graph-db/concurrency-test::run-concurrency-tests`.
- **hypnos** (Linux 5.15, SBCL 2.6.5) — the flag **is** honoured; this is where the adjacent path
  actually succeeds. Sandbox: `~/part3-test/vivace-graph-v3`, branch `part3`, runner
  `~/part3-test/run-suite.sh [main|concurrency]`. `ssh -p 2222 raison@hypnos.chatsubo.net`.
  Move commits with `git bundle` (`git bundle create /tmp/vg-p3.bundle <base>..experiment`,
  `scp -P 2222`, then `git fetch /tmp/vg-p3.bundle experiment:<branch>` in the sandbox).
  **Do not touch `~/work/vivace-graph-v3` or `~/quicklisp/local-projects/` there.**
- **odm** (Linux 4.15, SBCL 2.1.11) — the flag is **ignored**, so this is where the fallback path
  is exercised for real. Sandbox `~/part3-test`-equivalent is `~/part4-test`; runner
  `~/part4-test/run-suite.sh`. ⚠ **A production `ma-server` is live on odm.** Never push, never
  touch `~/work/vivace-graph-v3`, `~/quicklisp/local-projects/`, or `/var/tmp/mine-action`, never
  run `systemctl`, never kill a process.

Do not pipe the remote runners to `tail` — they buffer and you will see nothing until the end.

- [ ] **Step 7: Commit**

Print the full diff in a `## 📋 DIFF FOR REVIEW` block, then commit:
`feat(segment): extend the reservation in place before relocating`

---

## Out of scope

- Anything for the heap or linear hash — they share the ceiling but not the rw-lock.
- Changing Part 4. Relocation stays as the fallback; this only makes it rarer.
