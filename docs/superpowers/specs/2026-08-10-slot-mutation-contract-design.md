# The slot-mutation contract — design

**Status:** approved, not implemented
**Issue:** #135 (plus the shared-cache hazard it generalizes to)
**Target:** 3.1.0

## The problem

`UPDATE-NODE`'s docstring states the contract precisely: its argument must be "a COPY
(made with COPY inside the current transaction) of an existing node". `SAVE` guards one
side of that — passing a non-copy signals `MODIFYING-NON-COPY`. Nothing guards the rest,
and the unguarded cases fail worse than the guarded one.

Four patterns, all of which read back **correctly for the rest of the session** because
the node cache serves `data`. None is visible until a restart.

| | commit | same-session read | after reopen |
|---|---|---|---|
| A. create + `setf`, no copy | ok | correct | value silently `NIL` |
| B. create + `copy` + `setf` + `save`, same txn | ok | correct | **`DESERIALIZATION-ERROR` on open** |
| C. create in txn 1; `copy` + `setf` + `save` in txn 2 | ok | correct | correct |
| D. `setf` on a looked-up node, no copy | n/a | correct | value lost; shared instance mutated |

### Why A happens

`MAYBE-INITIALIZE-BYTES` (`transactions.lisp:414`) serializes `data` into `bytes` **only**
when `bytes` is `:init` or `NIL`. Construction sets `bytes`. A later `setf` on a
persistent slot updates `data` and never `bytes`. `MAYBE-WRITE-TO-HEAP` writes `(bytes
node)`.

The update path knows this and compensates explicitly — `transactions.lisp:813`,
`(setf (bytes new-node) (serialize (data new-node)))`, with a comment explaining exactly
why. **The create path never does.** So `APPLY-TX-WRITE (tx-create)` persists
construction-time bytes and discards every post-construction mutation.

This is an asymmetry between two sibling methods, not a contract the caller broke.

### Why B happens — mechanism NOT established

`COPY` of a not-yet-created node produces a `tx-update` whose `old-node` is the pending
create, and the node's stored bytes end up as `#(0 0)` — reading it back signals
`DESERIALIZATION-ERROR: no applicable method for DESERIALIZE-HELP (0 #())`. Two zero
bytes means a pointer or length was written as zero. **Which one, and where, is not yet
known.** Do not state a mechanism in code comments or the changelog until it is.

An earlier revision of this spec claimed `ARCHIVE-NODE-VERSION` races a create that is
"simultaneously establishing" the node's heap state. That is **wrong**: `(writes
transaction)` concatenates the create-set *before* the write-set and `APPLY-TRANSACTION`
applies them sequentially, so the `tx-create` always writes a valid head before the
`tx-update` reads it. There is no interleaving.

**Reproducing it — read this before writing a test.** `OPEN-GRAPH` **succeeds**. The
damage is in the node, not the graph, so a test that opens and closes without reading the
node back sees nothing and passes. The issue body's phrasing ("`open-graph` signals") is
imprecise; a repro built to that sentence reported ten consecutive clean runs against a
graph whose node was destroyed. **Assert on reading the node back after reopen.** The
canonical repro is `repro-135-deserialization.lisp` (untracked, at the root of the main
working tree).

### Why D is the worst of them

`LOOKUP-NODE` (`primitive-node.lisp:252`) returns the **shared cached instance** —
`(gethash key (cache graph))`, a per-graph node cache. Two lookups of the same id return
the same object, across threads. `SAVE-NODE` directly below it carries the comment "you
must copy the node before writing to its slots, in case others are reading it!"

So `(setf (user-email (lookup-user id)) "…")` mutates process-wide shared state visible
to every other reader and thread, with no transaction, no guard, and no persistence.

**This is why `COPY` exists.** It is not ceremony; it is the mechanism that keeps a
mutation off the shared instance. Nothing in the engine says so, and neither
`example.lisp` nor the manual demonstrates it.

### Why it survived the test suite

Every pattern reads correctly in-session. A test that writes and reads within one image
passes while the graph is being destroyed. 3,359 checks did not catch #135.

## The contract

> A persistent slot may be written only on a node the current transaction is entitled to
> mutate: a **copy** registered via `COPY`, or a node **created in this same
> transaction**. Anything else signals.

| Pattern | After |
|---|---|
| A | **legal and correct** — the create path persists the mutation |
| B | signals at the `COPY` |
| C | correct, unchanged |
| D | signals at the `SETF` |

The rule is expressible with no new bookkeeping: `COPY` already registers in `(copies
*transaction*)`, `%CREATE-NODE` already registers in `(create-set *transaction*)`, and
`(id tx-write)` delegates to its node — so both membership tests are O(1) hash lookups
on the node's id.

### Deliberate exclusions

- **Ephemeral and meta slots are unaffected.** The `:around` routes only *persistent*
  slots to `NODE-SLOT-VALUE`; ephemeral and meta are real CLOS slots reaching
  `CALL-NEXT-METHOD`. They are per-instance state and legitimately mutable. The guard
  never sees them.
- **`*INITIALIZING-NODE*` remains the internal escape**, exactly as it already works for
  `SLOT-MAKUNBOUND-USING-CLASS` (`primitive-node.lisp:451`). `CHANGE-NODE-CLASS` and the
  ECL construction paths (`vertex.lisp:27`, `edge.lisp:35`) already bind it.

## Design

### 1. The setf guard

Goes in the persistent branch of the `(setf SLOT-VALUE-USING-CLASS)` `:around`
(`primitive-node.lisp:406`). That is the single funnel: `(setf (NODE-SLOT-VALUE …))` has
exactly one caller. Guarding there covers accessors, `SLOT-VALUE` and `WITH-SLOTS`
uniformly — which matters, because 4 of mine-action's 6 mutation sites go through
`SLOT-VALUE` and an accessor-only guard would miss them.

```lisp
(if slot-keyword-name
    (progn
      (unless *initializing-node*
        (check-slot-mutation-allowed instance slot-name))
      (setf (node-slot-value instance slot-keyword-name) new-value))
    (call-next-method))
```

`CHECK-SLOT-MUTATION-ALLOWED` signals `MUTATING-UNREGISTERED-NODE` unless `*TRANSACTION*`
is bound **and** the node is either in `(copies *transaction*)` or `(object-set-member-p
node (create-set *transaction*))`. An unbound `*TRANSACTION*` therefore signals: that is
pattern D outside a transaction, the shared-cache case.

**Construction does not trip this guard** (verified, `schema.lisp:309-331`): the generated
`MAKE-<TYPE>` builds an *alist* from its initargs and hands it to `MAKE-VERTEX` /
`MAKE-EDGE` as `data`. No persistent-slot `SETF` occurs while a node is being built, so
the guard cannot fire before `%CREATE-NODE` registers the node. The one exception is
already covered: ECL binds `*INITIALIZING-NODE*` around `MAKE-INSTANCE`
(`vertex.lisp:27`, `edge.lisp:35`).

### 2. The copy guard (pattern B)

`COPY-NODE` signals when its argument is in the current transaction's create-set. The
error lands at the `COPY` call — the point of the mistake — and names the simpler correct
action, since under this contract no copy is needed there at all.

**`DELETE-NODE` copies internally** (`transactions.lisp:2632`), so this also makes
create-then-`MARK-DELETED` in one transaction signal. That pattern has the identical
shape to B — a `tx-delete` whose `old-node` is a pending create — and is therefore
expected to be broken today.

**This must be verified empirically before the guard is finalized**, not assumed. If it
corrupts, the guard is a fix and earns its own changelog entry. If it somehow works, the
placement is reconsidered. Either way the outcome is pinned by a test.

### 3. The create-path fix

Add the re-serialize to `APPLY-TX-WRITE (tx-create)`, before `MAYBE-WRITE-TO-HEAP` and
guarded on `data` being non-nil, mirroring what `tx-update` already does.

This is deliberately the *surgical* fix rather than the root one. The root defect is that
`bytes` is a derived cache of `data` with no invalidation on write; fixing that would make
`transactions.lisp:813` redundant and prevent this bug class entirely. It also changes an
invariant every `bytes` consumer relies on (replication, the txn log, `COPY-NODE`'s
deliberate bytes-copy). **Filed as a follow-up issue**, not bundled here: the guard is
where the user-facing value is, and it should land with risk that can be fully
characterised.

### 4. New conditions

- `MUTATING-UNREGISTERED-NODE` — carries the node and slot.
- `COPYING-UNCOMMITTED-NODE` — carries the node.

Both reports name the offending node and state the correct action.

## Testing

**Every regression test here must close and reopen the graph and assert after reopen.**
In-session reads are served by the node cache and pass against a graph that is being
destroyed. This is the property that let #135 through 3,359 checks.

- Patterns A, B, C, D, each asserted after reopen: A persists correctly; B and D signal;
  C unchanged.
- Create-then-`MARK-DELETED` in one transaction: first as an experiment characterising
  current behaviour, then pinned either way.
- Ephemeral and meta slots remain freely mutable — guarding them would be a regression.
- The `*INITIALIZING-NODE*` escape still works: `CHANGE-NODE-CLASS`, and the ECL
  construction paths.
- The graph opens cleanly after each pattern — "won't open" is B's actual symptom.

The guard sits on the MOP slot path and its escape is ECL-specific in two places, so this
needs the **four-config matrix** (SBCL/ECL × macOS arm64/Linux x86_64), not a single SBCL
run.

## Documentation

The half that prevents recurrence.

- **`example.lisp`** gains a worked update section. It currently contains zero
  occurrences of `COPY`, `SAVE` or `UPDATE-NODE`: the canonical walkthrough never shows
  how to change a node.
- **Manual**: a section stating the contract, the four-pattern table, and — missing
  everywhere today — *why* `COPY` exists (`lookup-*` returns the shared cached instance).
- **CHANGELOG**: `Fixed` for A and B, and a prominent `Changed` for the guard, since code
  that appeared to work will now error.

## Downstream impact

mine-action (131k lines, 262 files) was audited: all 324 persistent slot names extracted
from `def-vertex`/`def-edge`, every `(setf (<slot> …))` grepped repo-wide, all 53 distinct
`(setf (…` targets in `src/` enumerated to confirm the rest are `gethash`/`getf`/`aref`/
struct accessors, and the `SLOT-VALUE` and `WITH-SLOTS` routes checked separately.

**Zero violations.** Every persistent-slot mutation is pattern C:

| Site | Form |
|---|---|
| `spine-register.lisp:134` | `(copy existing)` → setf → `save` |
| `site-report-store.lisp:369` | `(copy existing)` → setf → `save` |
| `nts-survey.lisp` ×4 | `(copy …)` → `setf slot-value` → `save` |
| `loader.lisp:20` (`APPLY-INITARGS`) | 40+ callers, every one `(apply-initargs (copy X) …)` |

Two observations beyond the compatibility answer.

mine-action did not absorb this contract by discipline — it **routed around** it.
`UPSERT` and `APPLY-INITARGS` exist so no caller has to remember copy-then-save. The
largest consumer paid to hide the contract, which is the clearest evidence that it is
clunky. An engine-level ergonomic form was considered and deliberately deferred: it would
add public API to a contract being changed in the same release.

Both hand-written sites carry comments about traps their author had already hit —
`spine-register.lisp` explains that `COPY` must run *inside* the transaction or `SAVE`
signals `MODIFYING-NON-COPY`. The pain is real and already documented in the consumer,
just not in the engine.

Because the blast radius is empirically zero, the guard is a **hard error with no escape
hatch**. There is nothing to migrate, and a disabled guard fails silently in exactly the
case it exists for.

## Version

**3.1.0 (MINOR).** The conditions are additive and no *correct* program changes
behaviour — the guard rejects only code that was already losing data or corrupting the
graph. It needs a prominent `Changed` entry because code that appeared to work will now
error.

## Out of scope

- The `bytes`-invalidation root fix (follow-up issue).
- An ergonomic `UPSERT`-shaped macro in the engine.
- #131's spacetime workaround, which moved extent assignment to construction time and no
  longer depends on this gap.
