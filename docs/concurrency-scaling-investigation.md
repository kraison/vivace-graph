# Concurrency scaling investigation — CCL/ECL stress-suite timeouts

**Status:** root-caused; recommendation = *fix the contention* (specific patches
below) **and** document an interim concurrency ceiling.

**Scope:** the intermittent `run-threads: timed out after Ns (deadlock?)`
failures in `tests/concurrency` and `tests/concurrent-stress` on CCL 1.13 and
ECL ≥ 26.5.5 at high `*stress-thread-count*` (16/32/64). SBCL is unaffected.
`SEGV = 0` throughout — this is **not** the mmap remap race fixed in `749baeb`.

---

## Update 2026-06-04 — validated on real hardware; fix #1 landed

The convoy is **confirmed on the target hardware** (odm, 32-core, CCL 1.13 /
ECL 26.5.5); the original 4-core + SBCL-microbench analysis was correct.

Real-HW baseline @64 threads, `concurrent-stress`, instantaneous CPU sampled on
32 cores (3200 % = all cores):

| impl | result | CPU median | signature |
|---|---|---|---|
| SBCL | 17/17 PASS | ~113 % | bursty; ~3.5 min |
| CCL  | 3/15 FAIL  | ~181 % | sustained sub-saturation; ~7.7 min; 6 timeouts |
| ECL  | 9/15 FAIL  | ~100 % | worst (≈1 core of 32); ~14.5 min |

Sub-saturation CPU + always-completes ⇒ convoy, not deadlock. Confirmed.

### Fix #1 (commit `f3b01ef`): tx-log persistence out of the commit lock

Diagnostic — skip *all* persistence, CCL@64: **3/15 → 12/15**. So the commit-path
file I/O is a **primary** convoy contributor (fix #1's direction was right).

Real-HW lesson: the in-lock **syscall count**, not data volume, drives it:

| in-lock file ops per commit | CCL@64 |
|---|---|
| 0 (skip persistence) | 12/15 |
| 1 (rename only) | 11/18 |
| ~4 (in-lock header patch via kept-open stream) | 5/16 |
| ~7 (reopen + patch) | 3/15 |

Only **rename-only** relieves it (the kept-open header-patch variant also *errored*
under load). So fix #1 writes the `.txn` file *before* the lock and only renames
under it; the header `transaction-id` becomes a placeholder (the **filename** is
authoritative; recovery reads it from there). Replication is unaffected (it reads
the replication-log, where masters patch the real id into the in-memory bytes).
See the TODO above `finalize-tx-persistence` — flagged to revisit for
correctness/maintainability.

Result @64: CCL `concurrent-stress` **3/15 → 11/18**. Gate at shipped config
(`*stress-thread-count*` 16): SBCL + ECL all green; CCL test/acid/
concurrent-stress green, concurrency green except an intermittent
`skip-list-concurrent-inserts` timeout.

### Residual lever = the rw-lock (fixes #2 / #3)

The remaining timeouts persist with *zero* persistence (no-persist still left 3
timeouts) and hit **non-transactional** tests (`skip-list-concurrent-inserts`) —
so the residual convoy is the **rw-lock** (Sections 3.1 / 4), not the commit
path. ECL additionally still shows the `(sleep 0.001)` poll floor (~100 % CPU
@64). Fixes #2 (modernize the ECL rw-lock path) and #3 (reader fast-path /
native rwlock) are next.

### Interim ceiling — unchanged at ~16

Fix #1 relieves the commit-lock component, but the rw-lock convoy remains and
even 16 threads intermittently trips one concurrency-suite timeout on CCL.
Re-measure and lift after the rw-lock work.

### Tried and reverted: CCL on `rw-lock.lisp` (2026-06-04)

Moving CCL off its native `ccl:make-read-write-lock` onto the custom lock
was tried and reverted the same day. `rw-lock-suite` passed, but
`concurrent-stress` got worse: 8 of 15 failing, against about 4 of 12 on
the native lock. The cause is `acquire-write-lock` allocating a wake
semaphore per writer on every call, cheap on SBCL and costly on CCL.
Separately, the main CCL convoy was in the transaction-manager and
type-index locks, not the rw-lock. A #118 fix that moves CCL to
`rw-lock.lisp` therefore needs a pooled waker first.

What landed instead (2026-10-31, #118): CCL keeps its native lock, and
the shim adds a queued-writer count that new readers wait on before
taking the read side. The scanners of `%scan-race-round` re-enter back
to back and the native lock admits them over a waiting writer; with the
gate the reader count drains within one scan and the writer proceeds.
A thread that already holds the lock on either side skips the gate.

---

## 1. Verdict (answers the three hypotheses)

| Hypothesis | Verdict |
|---|---|
| **(a) genuine deadlock / livelock in a lock path** | **No.** Under a saturated 64-thread lock workload the process holds a *steady, sustained* ~138 % CPU (1.4 of 4 cores) and **always runs to completion**. A true deadlock is ~0 % CPU and never completes; a hot livelock would be ~400 % (all cores spinning). ~138 % sustained = threads mostly *parked*, one progressing at a time. |
| **(b) lock-convoy / starvation from mutex-heavy serialization** | **Yes — primary cause.** Two serialization points: (1) the custom `rw-lock` (`rw-lock.lisp`) collapses **14–18×** in throughput from 4→64 threads because *every* reader and writer must grab one internal mutex (`lock-lock`) and every write-release does a `condition-broadcast` thundering-herd; (2) the **global transaction-manager lock** serializes the entire commit (validate + **disk persist** + apply) across all threads. |
| **(c) inherent CCL/ECL scheduler limits** | **Secondary / aggravating.** ECL's older `mp` primitives forced the `(sleep 0.001)` polling workarounds in `rw-lock.lisp` (a 1 ms-per-handoff floor); CCL's scheduler/GC add overhead at 64 threads. These *amplify* a design-level convoy; they are not the root cause. |

The timeouts are a **throughput ceiling**, not a hang. At ~2,500 contended
lock-handoffs/sec (measured), a phase that needs ~80k–150k contended
acquisitions takes 30–60 s and trips the `run-threads` budget — exactly the
observed symptom, and exactly why it is *intermittent* (it depends on how the
scheduler interleaves the convoy on any given run).

---

## 2. Reproduction constraints in this environment

This analysis was performed on a 4-core Linux container. The target conditions
(32 cores; CCL 1.13; ECL 26.5.5) could **not** be faithfully reproduced here:

- Only **4 cores** — cannot exhibit 32-core scaling divergence.
- The OS package manager ships only **ECL 21.2.1** — the *unstable* version the
  `rw-lock.lisp` workarounds were written *against*. Running it would measure
  the workaround, not the modern runtime.
- No CCL/ECL ≥ 26 binaries available without an out-of-band download/build.

So the lock paths were **isolated and reproduced standalone on SBCL 2.2.9**
(stable, available), by porting the exact `rw-lock.lisp` write/read algorithm
into a dependency-free harness (`docs/rw-lock-microbench.lisp`) with a compile
switch between the *blocking* path (`condition-wait`/`broadcast`, used by
SBCL/LispWorks/modern-ECL) and the *polling* path (`(sleep 0.001)`, the current
`#+ecl` code). This measures the lock's intrinsic scaling independent of the
graph stack and independent of the Lisp.

> To validate on the real targets, run `tests/concurrent-stress` on a 32-core
> box under CCL 1.13 and ECL 26.5.5 while sampling with
> `top -H -p <pid>` / `ps -o %cpu,nlwp`. The CPU signature (≈0 % vs high but
> sub-saturation) will confirm the convoy vs a deadlock, as it did here.

---

## 3. Evidence

### 3.1 The rw-lock does not scale — in *either* strategy

Each thread does 2,000 acquire/release cycles of one shared write lock
(tiny critical section), SBCL 2.2.9, 4 cores:

```
mode=BLOCK  threads=  4  handoffs/s = 33,898
mode=BLOCK  threads=  8  handoffs/s = 22,989
mode=BLOCK  threads= 16  handoffs/s = 13,769
mode=BLOCK  threads= 32  handoffs/s =  6,228
mode=BLOCK  threads= 64  handoffs/s =  2,489     <- 14x collapse vs 4 threads
mode=POLL   threads= 64  handoffs/s =  2,575     <- ECL path: ~same here
```

Read-mostly (95 % reads, 5 % writes) — readers *should* run in parallel:

```
mode=BLOCK  threads=  4  ops/s = 1,000,000
mode=BLOCK  threads= 16  ops/s =   270,270
mode=BLOCK  threads= 64  ops/s =    55,672     <- 18x collapse, despite 95% reads
```

The read-read collapse is the tell: in this rw-lock, readers do **not** run
concurrently — `acquire-read-lock`/`release-read-lock` each grab the single
internal `lock-lock` mutex to touch the `readers` counter
(`rw-lock.lisp:50-79`). The mutex, not the data, is the bottleneck.

### 3.2 It is slow progress, not a deadlock (CPU sampling)

`ps -o %cpu,nlwp` sampled every 2 s during the 64-thread write run:

```
t=2s ..t=24s : %CPU steady ~138%   threads=66   (then completes, wall ~51s)
```

Steady ~138 % for the entire run, then completion. Not 0 % (deadlock), not
~400 % (hot spin). Classic convoy: most threads blocked/sleeping, ~1.4 cores of
useful + wakeup-churn work.

---

## 4. Where the contention lives (lock map)

| Lock | Impl mapping | On the hot path? | Problem |
|---|---|---|---|
| **custom `rw-lock`** (`rw-lock.lisp`) | SBCL/LispWorks/**ECL** | view-group locks (`views.lisp`), class locks (`schema.lisp`), index-list (`index-list.lisp:30`), allocator cache/memory (`allocator.lisp:30,33`) | single internal mutex serializes all ops; `condition-broadcast` thundering-herd (O(N²) wakeups); **ECL** replaces blocking with `(sleep 0.001)` polling → 1 ms/handoff floor |
| **native rwlock** | **CCL** (`ccl:make-read-write-lock`, `utilities.lisp:480`) | same call sites as above | correct & blocking, but still convoys under the same access pattern at 64 threads |
| **transaction-manager lock** (`make-recursive-lock`, native on all impls) | all | **every commit** (`transactions.lisp:1361-1384`) | the *whole* commit — `validate` + `persist-transaction` (**disk I/O**) + `apply-transaction` — runs inside one global recursive lock. Serializes all writers and includes I/O in the critical section. |
| allocator bin-locks, type-index lock, prolog gensym lock | native per-impl | allocation / type scan / gensym | fine (native blocking, well-partitioned) |

### Why SBCL passes but CCL/ECL time out

- **SBCL:** futex-backed mutex/condvar make each convoy step cheap; the *real*
  workload spreads across many per-class/per-view/per-index locks (far less
  per-lock contention than the microbench), so it stays under the 30–60 s
  budget even though the same scaling weakness is present.
- **CCL:** native rwlock is fine per-op, but the **global commit convoy** plus
  CCL scheduler/GC overhead at 64 threads on 32 cores intermittently pushes a
  write-heavy phase (`MIXED-READ-WRITE-STORM`) past budget.
- **ECL:** gets *both* the global commit convoy *and* the custom rw-lock's
  `(sleep 0.001)` polling on every contended class/view/index **read** lock. On
  many cores the 1 ms floor caps reader throughput hard — the worst case, which
  matches ECL being the most frequent/severe failer (`SKIP-LIST-CONCURRENT-INSERTS`,
  per-thread debugger at 64).

The `rw-lock.lisp` `#+ecl` polling is gated on the *implementation*, not the
*version*. It was written for ECL 21.2.1 bugs (see the comments at
`rw-lock.lisp:57,68,162`: `mp:wait-on-semaphore` blocks indefinitely;
`condition-variable-broadcast` misses waiters; `condition-variable-timedwait`
unreliable before 23.09.09). On ECL 26.5.5 those primitives are fixed, yet the
code still takes the slow polling path.

---

## 5. Decision: fix the contention (and ship an interim ceiling)

This is a real, fixable **design-level convoy**, not an unavoidable runtime
limit, so the right long-term answer is to **fix it**. Until the fixes land and
are validated on the target hardware, **document a supported ceiling** so CI is
green and users have a clear expectation.

### 5.1 Recommended fixes, ranked by leverage / risk

1. **Shrink the global commit critical section (highest leverage, CCL+ECL).**
   In `%commit` (`transactions.lisp:1361`), `persist-transaction` does file I/O
   *inside* the transaction-manager lock. Write the per-transaction log file
   **before** taking the lock (it is keyed by the tx and only made visible by
   `mark-as-committed`), and keep only `validate` + id-assignment +
   `apply-transaction` + prune under the lock. This removes disk latency from
   the serialized path and should directly relieve the CCL convoy.
   *Risk: medium* — must preserve the ordering/durability invariants documented
   at `transactions.lisp:1366-1383`; needs the ACID suite (`tests/acid`) to
   confirm.

2. **Modernize the ECL rw-lock path (high leverage, ECL).**
   Replace the `#+ecl (sleep 0.001)` polling with the same
   `condition-wait`/`condition-broadcast` blocking the SBCL branch uses, gated
   on ECL version (`#+ecl` *and* a `(>= ecl-version 23.9.9)` feature check), so
   21.2.1 keeps the safe fallback. Removes the 1 ms/handoff floor.
   *Risk: medium* — **must be validated on ECL 26.5.5**; if the version gate is
   wrong it could reintroduce the 21.2.1 hangs. Do not merge unvalidated.

3. **Fix the rw-lock's intrinsic non-scaling (largest payoff, all impls).**
   The single internal mutex + broadcast caps *every* implementation
   (Section 3.1). Two options: (a) an atomic reader-count fast path
   (CAS-increment when no writer) so read-read never serializes on `lock-lock`;
   or (b) replace the hand-rolled lock with each runtime's native rwlock
   (CCL already uses native; SBCL has `sb-thread`'s frlock/`with-mutex`; ECL 26
   has working `mp` rwlocks). Prefer (b) where a correct native rwlock exists.
   *Risk: higher* — core primitive; needs the full concurrency + ACID suites on
   every impl.

4. **Replace `condition-broadcast` with targeted wakeup.** Even on the blocking
   path, broadcasting wakes all N waiters per release (thundering herd). Waking
   only the next queued writer / the waiting readers cuts O(N²) → O(N).

### 5.1.1 Update 2026-06-04 — fixes #2 + #4/#3-Part-1 landed

- **#2 (modernize ECL rw-lock path): LANDED** (commit `ffb0dfb`). ECL ≥ 23.9.9
  (`:graph-db-ecl-modern-mp`) now blocks on `condition-variable`/`semaphore`
  instead of `(sleep 0.001)`; 21.2.1 keeps the poll. Validated on ECL 26.5.5.

- **#4 (targeted wakeup) = #3 Part 1: LANDED** (commit `321b0bc`). The
  `condition-broadcast`-everyone scheme is replaced: each queued writer carries
  its own private wake semaphore (release signals exactly the front writer,
  FIFO), and readers share a cv broadcast only on the writer→free transition
  with an empty queue. O(N²) herd → O(N). Semantics preserved exactly. Old ECL
  keeps the poll fallback.
  - **Microbench A/B (SBCL):** write-contention @64 threads 14,016 → 323,006
    handoffs/s (~23×; the 14–18× collapse is **gone** — the curve is flat 4→64).
    Read-mostly @64 288,439 → 2,759,073 ops/s (~9.6×, flat).
  - **This re-scopes #3 option (a):** read-read serialization on `lock-lock` is
    **not** the residual bottleneck once the herd is removed (read-mostly no
    longer collapses). The atomic reader fast-path is therefore **deferred behind
    a benchmark gate** (and wouldn't help ECL, which has no atomics). Option (b)
    — native rwlocks — is moot for the custom-lock impls (SBCL/ECL lack a drop-in
    recursive+downgradeable rwlock; CCL already native).
  - **Validation (odm 32-core):** SBCL (2.1.11) + ECL (26.5.5) concurrency +
    acid + concurrent-stress all green; rw-lock-suite 20/20. CCL is a no-op for
    this change (rw-lock.lisp excluded from its build) and keeps a **separate,
    pre-existing intermittent** concurrent-stress flake (varying test each run) —
    that needs distinct work on CCL's native-lock / view-consistency path.

### 5.2 Interim supported concurrency ceiling

Until 1–3 land and are validated on a 32-core box with CCL 1.13 / ECL 26.5.5:

- **SBCL:** no ceiling observed; 64+ threads fine.
- **CCL / ECL:** **supported ceiling ≈ 16 concurrent writer threads** per graph
  (the shipped `*stress-thread-count*` default). Write-heavy workloads above
  this convoy on the commit lock and may exceed the `run-threads` budget. Read-
  heavy workloads tolerate more but still degrade super-linearly.

This matches the current shipped defaults (`*stress-thread-count*` = 16; the
per-test cap `(min *stress-thread-count* 16)` in `mixed-storm.lisp`). The
investigation confirms 16 is the right interim number and explains *why*.

---

## 6. How to reproduce

Standalone lock microbenchmark (no graph-db deps; SBCL):

```sh
sbcl --non-interactive --load docs/rw-lock-microbench.lisp           # scaling table
sbcl --non-interactive --load docs/rw-lock-microbench.lisp readmostly # read-mostly
# while it runs, in another shell:  watch -n1 'ps -o %cpu,nlwp -C sbcl'
```

On the real targets (32-core Linux, CCL 1.13 / ECL 26.5.5):

```lisp
(ql:quickload :graph-db/concurrent-stress-test)
(setf graph-db/concurrent-stress-test:*stress-thread-count* 64)
(graph-db/concurrent-stress-test:run-concurrent-stress-tests)
;; in a shell:  top -H -p <lisp-pid>   ; ~0% CPU = deadlock, high-but-not-saturated = convoy
```
