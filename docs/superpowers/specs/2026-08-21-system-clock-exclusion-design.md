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

An advisory `flock(2)` lock on `<clock-dir>/system-clock.lock`, named as a
visible sibling of `system-clock.dat`, acquired `LOCK_EX | LOCK_NB` during
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

The obvious objection — that this strands #170's lease-holder — does not hold. See §8.1.

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
- **#170's detach protocol.** This unit makes a violation of its contract loud; it does not
  implement it. See §8.1 — the contract is not merely compatible with exclusive-only
  locking, it is forced by it.
- **Global type registry placement (#186).** The registry lands in this directory and
  inherits this protection; it is not designed here.

### 8.1 Why exclusive-only does not constrain #170

Recorded because it looks like a conflict and is not, and the reasoning is otherwise
re-derived every time someone reads §5.1.

A lease-holder does **not** need the clock directory, and cannot be given it. What
`clock-lease-epochs` hands out is a range `[start, end)` that the owning clock has
**already skipped past** — so no coordination with the clock is possible or required for
the lifetime of the lease. That is the whole mechanism. The holder needs a number range and
a directory to write a shadow generation into; both are handed to it at detach.

Journal access is already gated on holding the directory, by construction rather than by
convention: `journal-append` takes a `clock` as its first argument, and a `system-clock`
struct is produced only by `open-system-clock`. The sole in-tree caller
(`transactions.lisp:3000`) sits inside `attach-to-system-clock`, in the owning image.

The case that appears to break this is **who records `:SWAP`**. The holder cannot: §8 of
the namespaces design defines reattach as an atomic swap *plus a brief quiesce*, and
quiescing means refusing new pins and draining in-flight ones against live node objects,
buffer-pool pages and mmap handles held **by the owning image**. Only the owner can do
that, so the swap is necessarily an owning-image operation and records its own journal
entry. The holder signals "shadow ready" out of band.

Two adjacent cases, for completeness:

- **The holder crashes mid-load.** Nobody needs to know how many epochs it consumed: the
  owning clock skipped the entire range at lease time. Unused epochs are wasted, which at
  64 bits is free. No journal read is required to reconcile.
- **The holder outlives an owning-image restart.** The restarted image must learn a lease
  is outstanding, so it reads the journal *while holding the lock*. Sequential access, not
  concurrent.

Note also that the namespaces design's §8 makes **in-process** detach the primary case —
*"In-process detach becomes viable for the first time... The epoch lease still accommodates
out-of-process later without redesign."* In-process, the holder **is** the owning image and
no exclusion question arises at all.

**Left to #170:** an out-of-process holder should persist its lease range in its own shadow
directory, so the range survives the holder's own restart without consulting the clock.

## 9. Acceptance

- A second `open-system-clock` on a held directory signals, naming the directory, without
  blocking.
- A holder that dies leaves no residue: the next open succeeds with no operator action.
- An unclean shutdown still requires no manual recovery of the counter, exactly as today.
- `clock-lease-epochs` and every other clock entry point behave as before.
- The refusal is distinguishable from an unrelated `flock` failure.
