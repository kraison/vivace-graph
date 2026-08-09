# Android device crash — intermittent ECL thread-creation race on `:disk` warm reopen

**From:** mine-action app team
**Date:** 2026-07-06 (revised — supersedes the earlier draft of this file, which was mis-scoped)
**Engine:** vivace-graph-v3, today's `main` (`de9cdf2`) — **but NOT a regression; see below**
**Severity:** intermittent (~1 in 5), device- and backend-specific, with a clean app-side workaround

---

## TL;DR

The mine-action device peer (mine-action-core + graph-db, AOT-compiled to `aarch64-android`
on **ECL 26.5.5**, Boehm GC) intermittently crashes **only** when it **reopens an existing
`:disk` graph** on one specific device (a low-end tablet). The failing thread comes up with an
invalid ECL environment:

```
Internal thread error in:
pthread_getspecific() failed.  [22: Invalid argument]
Did you forget to call `ecl_import_current_thread'?
Exitting thread.
```

It is **a race in ECL per-thread environment setup**, tripped when graph-db spawns its
`buffer-pool-thread` **and** the peer-writer thread **concurrently** during the (~40 s) `:disk`
warm-open index rebuild, on a resource-constrained device. It reproduces **~1 launch in 5**.

**It is NOT a regression and NOT `:lazy`.** We proved (matrix below) that `:lazy` — the mode the
field actually uses — never fails, `:disk` *fresh* sync-from-0 never fails, older engines behave
the same, and capable devices never fail. So this is low-priority, but it's a real race and we'd
like your read on whether the concurrent thread-spawn on `:disk` open can be serialized.

---

## Environment

| | |
|---|---|
| Engine | vivace-graph-v3 `de9cdf2` (also repro'd on `c068b54`; **not** version-specific) |
| Lisp | **ECL 26.5.5**, cross-compiled `aarch64-android`, **Boehm-Weiser conservative GC** |
| Failing device | Samsung Galaxy Tab A8 (SM-X200), **Android 14, ~3.5 GB RAM** |
| Passing devices | Galaxy S24 (Android 16, 11 GB), Galaxy Tab Active5 (Android 16, 5.5 GB) — never crash |
| Role | read-mostly peer: opens local graph, pulls authored feed from the SBCL hub |
| Data | real site "Izium Forest - Prevail": 1 site, 41 surveys, **734 finds** |

The device runs ECL on a JVM-attached worker thread; JNI marshals requests to it.

---

## Symptom + exact trigger

The crash lands during peer bring-up, right where graph-db spawns threads. The `[ECL-THREADS …]`
lines below are our own `mp:all-processes` probes — note `buffer-pool-thread` (a `:disk`-only
thread) present at the moment of failure:

```
[MEMPROBE] boot open backend=DISK fresh=NIL secs=39.70   <- warm reopen: 40s index rebuild
[ECL-THREADS post-boot]  buffer-pool-thread | ... | TOP-LEVEL
[ECL-THREADS sync-start] buffer-pool-thread | ... | TOP-LEVEL
Internal thread error in:
pthread_getspecific() failed.  [22: Invalid argument]
Did you forget to call `ecl_import_current_thread'?
Exitting thread.
```

The same underlying failure has two faces depending on the host thread model:

1. **ECL on the JVM worker** → `pthread_getspecific() failed [EINVAL]` → `Exitting thread.`
2. **ECL on a raw `pthread`** (we tried moving the worker off the JVM thread) → a hard **SIGSEGV**
   with this native backtrace:

```
#00 __memcpy_aarch64_simd        libc        <- fault addr 0x0 (reads from NULL)
#01 _ecl_alloc_env+100           libecl.so   <- allocating a NEW thread's cl_env
#02 mp_process_enable+164        libecl.so
#03 mp_process_run_function+252  libecl.so   <- graph-db spawning a thread
#04..#11 libmavg.so (graph-db AOT, inside the :disk warm-open path)
#12 cl_funcall / ecl_apply_from_stack_frame
```

Both say the same thing: **a newly-spawned ECL thread's `cl_env` (installed via
`pthread_setspecific`, copied in `_ecl_alloc_env`) is missing/half-initialized** when the thread
starts. That is a classic thread-creation race.

---

## The matrix that scopes it (all on the failing Tab A8)

We ran every combination. Only one cell fails:

| Backend | Graph state | Result |
|---|---|---|
| `:lazy` | warm reopen | ✅ works (many runs) |
| `:lazy` | **fresh sync-from-0** (peer-writer applies all 734 finds) | ✅ works |
| `:disk` | **fresh sync-from-0** | ✅ works (`buffer-pool-thread` spawns fine) |
| `:disk` | **warm reopen** | ❌ **crashes ~1 in 5** |

And across engines / devices:
- `c068b54` (yesterday) and `de9cdf2` (today) behave identically → **not a regression**.
- S24 (Android 16 / 11 GB) and Active5 (Android 16 / 5.5 GB) **never** crash → device-specific.

The distinguishing factor of the failing cell: **`:disk` warm reopen is slow (~40 s** — it
rebuilds indexes from the mmap'd store**) and spawns `buffer-pool-thread` + the peer-writer
concurrently during that heavy phase.** `:lazy` and `:disk`-fresh open fast with less concurrent
thread pressure, so they almost never hit the race window; the low-RAM/older-ART Tab A8 hits it
~20 % of the time.

*(Historical note: our earlier draft claimed "`:disk` always crashes / not today's commits."
That was wrong — we had been testing warm `:disk` reopens the whole time, and the deterministic-
looking failures were just an unlucky streak of this race on a heavier graph. Please disregard
the prior draft's conclusions.)*

---

## What we'd like from you

The crash is in ECL's runtime (`_ecl_alloc_env` / `pthread_setspecific`), but the **trigger is
graph-db creating threads concurrently on `:disk` warm-open**. Questions:

1. On `:disk` warm-open, can the **`buffer-pool-thread` and the peer-writer be spawned
   serially** (and ideally *after* the index rebuild settles), rather than concurrently during
   the rebuild? Reducing concurrent `mp:process-run-function` calls during open would likely
   close the race.
2. Is there a known ECL 26.5.5 constraint on **concurrent `mp:process-run-function`** (e.g. it
   needs serialization around `_ecl_alloc_env` on secondary threads), or a recommended guard
   (a mutex around thread creation)?
3. Anything about the ~40 s warm-reopen index rebuild that could be doing thread work while a
   dynamic binding it copies into the child env (`*graph*` and friends) is transiently unbound?

## App-side mitigation (already in place)

The field build runs **`:lazy`**, which never hits this. So this is not blocking us — it's a
robustness question for `:disk` on low-end hardware. If you'd rather treat it as ECL-runtime and
out of scope, our fallbacks are: keep constrained devices on `:lazy`, and/or serialize our own
open-time thread creation.

## Repro recipe

1. Device core at `de9cdf2` (or `c068b54`), enroll the Tab A8 as an Izium peer, let it sync a
   `:disk` graph from 0 (this succeeds).
2. Force-stop and relaunch on `:disk` repeatedly. ~1 in 5 warm reopens dies with the
   `pthread_getspecific() failed` message above. `:lazy` reopens never do.

*(The two VG working-tree files currently modified — `memory-graph.lisp`, `peer-streaming.lisp`
— are our temporary `[ENG-*]` stage-marker probes; ignore/discard them.)*
