# Cross-process exclusion on the system clock — design

**Issue:** kraison/vivace-graph#182
**Depends on:** nothing. #168 (the clock) is merged.
**Blocks:** #186 (global type registry), #170 (detach quiescence)
**Related:** #191 (torn journal tail — same file, different failure, out of scope here)

## 1. Goal

`open-system-clock` must refuse when another live process holds the clock directory,
loudly and immediately, instead of silently issuing a second stream of epochs.

Everything else about the clock stays as #168 built it.

## 2. The defect

`open-system-clock` (`system-clock.lisp:57`) calls `ensure-directories-exist`, reads the
persisted ceiling, reserves a block, and returns. There is no marker, no lock, and no
refusal. The clock's only lock is a single `bordeaux-threads` recursive lock, held at six
sites, and the file contains zero OS-level exclusion primitives — so all existing mutual
exclusion is **intra-image**.

Two images pointed at one clock directory therefore both read the same ceiling, both
reserve from it, and both begin issuing. That destroys the single property the clock
exists to provide: that no two transactions in the system share an epoch. There is no
error, nothing in the logs, and no way to detect it afterward except by finding two
records with the same epoch and different provenance.

Stores do not have this hole: `open-graph` signals when `.dirty` is present
(`graph.lisp:546`).

## 3. Scope: exclusion only, not crash recovery

**The clock counter is already crash-safe, and this design does not add recovery to it.**

`%write-clock-ceiling` persists `ceiling + block-size` *before* any id in that block is
issued, into a fixed 8-byte file opened `:if-exists :overwrite` — chosen over `:supersede`
precisely so a crash cannot leave it short or absent. A crashed process's successor reads
the *higher* persisted ceiling and resumes above it: at most `block-size` ids are wasted,
and none is ever reissued. #168 pins this with `clock-survives-crash-without-reissuing`
and `clock-survives-crash-after-refilling-its-block`.

So a clean/unclean flag for the counter would force a manual recovery step for a condition
already handled correctly and automatically. It is rejected on those grounds.

This is the substantive difference from the store convention. `.dirty` answers two
questions at once — *is someone holding this?* and *did the last holder exit cleanly?* —
and can, because a store's blast radius is one store. For the clock the second question has
no consequence, and a stale marker would block **every image in the system**.

## 4. Mechanism

An advisory `flock(2)` lock on `<clock-dir>/.lock`, acquired `LOCK_EX | LOCK_NB` during
`open-system-clock` and held for the clock's lifetime by retaining the fd in the
`system-clock` struct. `close-system-clock` closes the fd, which releases the lock.

**The kernel releases the lock when the holding process dies.** That is the property being
bought, and it is why the design is a lock rather than a marker file: staleness becomes
impossible by construction rather than something to detect.

`LOCK_EX` = 2, `LOCK_NB` = 4, `LOCK_UN` = 8 on both Linux and Darwin, so unlike several
existing constants in `posix.lisp` these need no platform conditional.

### 4.1 Rejected mechanisms

| Mechanism | Why not |
|---|---|
| `O_EXCL` lock file | The file outlives the process, reintroducing stale-marker detection — the problem this design exists to avoid. `posix.lisp` lacks `+o-excl+`, but adding it is not the obstacle; the semantics are. |
| `fcntl(F_SETLK)` | Better over NFS, but **any** `close()` on that file anywhere in the process drops every lock the process holds on it. Too easy to trip from unrelated code. |
| A `.dirty`-shaped marker | Conflates exclusion with crash recovery (§3), and cannot distinguish a live holder from a dead one. |
| Liveness probe (pid + boot-id) | Self-healing without new FFI, but pid reuse can make a dead holder look alive, and the probe is platform-specific. Strictly worse than letting the kernel answer. |

### 4.2 Non-blocking is deliberate

`LOCK_NB` is not an optimization. Without it a second image blocks indefinitely inside
startup, which presents as a hang with no diagnostic. An immediate, named refusal is
diagnosable; a hang is not.

## 5. Surface

- `open-system-clock` gains one failure mode and signals `system-clock-in-use`, naming the
  directory. **No new arguments and no mode flag.**
- `close-system-clock` additionally closes the lock fd.
- No other entry point changes. `clock-next-epoch`, `clock-lease-epochs`,
  `clock-observe-epoch`, `journal-append` and `journal-records` are untouched.

### 5.1 Exclusive only

There is no shared/read-only mode. Nothing in the tree wants concurrent read access:
#170 and #171 consume the journal from **inside** the owning image. A `:read-only` mode
taking `LOCK_SH` would add a second code path plus a mode check on every mutating entry
point, to serve a consumer that does not exist. Anything wanting to inspect a live system
reads the files directly, outside the API and at its own risk.

### 5.2 errno must be read

`flock` returning −1 must distinguish `EWOULDBLOCK`/`EAGAIN` — held, the expected case —
from a genuine failure such as `EBADF` or `ENOLCK`. Nothing in the tree reads `errno`
today, so this is new. Conflating the two would report "another image holds this
directory" when the real fault is unrelated, which is a worse diagnostic than the silence
being fixed.

## 6. Testing

**Exclusion is honestly testable in-process.** `flock` locks attach to the *open file
description*, not the process: Linux's `flock(2)` states that file descriptors obtained
from separate `open()` calls are treated independently, and one may be denied by a lock the
same process holds via another. So opening the clock twice in one image and asserting the
second signals is a real test of the mechanism, not a simulation of it.

This matters because the subprocess alternative is slow, and slow tests get skipped.

| Test | Pins |
|---|---|
| Second `open-system-clock` on a held directory signals `system-clock-in-use` | The refusal itself |
| Reopen after `close-system-clock` succeeds | Release on clean close |
| `clock-lease-epochs` still works while the lock is held | The #170 path is not broken by the guard — a lease-holder is inside the owning image |
| Epochs stay monotonic across a close/reopen cycle | The lock did not disturb the ceiling protocol |

**Required ablation:** remove the `flock` call and confirm the second-open test fails. A
guard test that passes with the guard removed proves nothing.

## 7. Stated limitations

- **NFS.** `flock` over NFS is unreliable on older configurations. This design assumes one
  image per system directory on one host, which the surrounding design already assumes;
  the limitation is stated rather than engineered around.
- **Advisory, not mandatory.** A process that never calls `open-system-clock` — reading or
  writing the files directly — is not stopped. The guard binds users of the API.

## 8. Out of scope

- **#191, the torn journal tail.** Same file, unrelated failure: #182 is two live
  processes, #191 is one process losing power. Folding them together would put an
  exclusion bug and a durability bug in one diff.
- **The lease escape hatch for #170.** The contract is that a lease-holding process never
  opens the clock directory at all. That is #170's to honour; this unit makes violating it
  loud. **Open question flagged for #170:** if a lease-holder ever needs to read the
  lifecycle journal, this design blocks it, and #170 must then either take the journal
  through the owning image or revisit §5.1.
- **Global type registry placement (#186).** The registry lands in this directory and
  inherits this protection; it is not designed here.

## 9. Acceptance

- A second `open-system-clock` on a held directory signals, naming the directory, without
  blocking.
- A holder that dies leaves no residue: the next open succeeds with no operator action.
- An unclean shutdown still requires no manual recovery of the counter, exactly as today.
- `clock-lease-epochs` and every other clock entry point behave as before.
- The refusal is distinguishable from an unrelated `flock` failure.
