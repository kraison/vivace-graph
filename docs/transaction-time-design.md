# Transaction time on the claim record — design (issue #148)

**Status:** v1 **implemented** on branch `experiment` (`spacetime/claim.lisp`,
`spacetime/claim-query.lisp`, `spacetime/conditions.lisp`): a second temporal
extent on every claim, stamped at construction, immutable at the accessor.
This is unit S1's extension of the claim record; see
`docs/spatiotemporal-substrate-programme.md`. Design spec:
`docs/superpowers/specs/2026-08-18-transaction-time-design.md`.

## Motivation

A claim could say when a fact was **true** (`claim-extent`), or, by
convention through a tenant's own `:extra-slots`, when it was **recorded** —
never both through one substrate mechanism, and never in a way two tenants
would agree on. `+claim-shared-slots+` carried one temporal slot, and the
unary uniqueness constraint forbids the workaround of one claim per axis, so
the limit was structural. #148 asked for a second axis on the substrate
itself, so tenants stop each inventing their own `recorded-at`.

## The two axes

`claim-extent` keeps meaning **validity** — when the fact was true in the
world. The new `claim-transaction-extent` means **when the substrate learned
it** — the audit axis. Both are backed by the same `(:temporal-extent 1 …)`
sexp shape, decoded by the same `extent->sexp`/`sexp->extent` pair
(`spacetime/extent.lisp`), so there is no second codec and a codec bug is
fixed in both axes at once. The two slots never share a name —
`extent-sexp` vs. `transaction-extent-sexp`
(`spacetime/claim.lisp:44-50`) — so neither accessor can be crossed onto the
other's slot without also being visibly wrong at the definition site.

Nothing that reads a claim today changes: a tenant declaring neither axis
stays fully supported, and `claim-extent` and every existing consumer behave
exactly as before.

## Why an interval, not an instant

The transaction value is stored as a full interval (`semantics :transaction`)
rather than a single timestamp, even though today only its start is ever
supplied. Bitemporal transaction time is classically a period —
`[recorded, superseded)` — and `bound` already accepts `:unbounded` at either
endpoint (`spacetime/bound.lisp`). `%open-transaction-extent` therefore
builds `[timestamp, :unbounded)`:

```lisp
(defun %open-transaction-extent (timestamp)
  (make-interval (exact-bound timestamp) (unknown-bound)
                 :semantics :transaction :standing :asserted))
```

(`spacetime/claim.lisp:86-90`.) The open interval costs nothing to express
today, and it is deliberately the seam retention will use later: closing the
end when a claim is superseded, rather than changing the slot's shape, is
what makes `:as-of` (out of scope here, see below) a later unit's problem
rather than a migration.

## Open versus unknown, carried by the extent's standing

"Still believed" and "we do not know when belief ended" are both an
unbounded end bound, so the bound alone cannot tell them apart. The extent's
own `standing` — the same closed vocabulary `+standings+` already uses for a
claim's own belief state (`spacetime/standing.lisp`) — carries the
distinction instead of a new flag:

| Case | End bound | Extent standing |
|---|---|---|
| Recorded, still believed (the default) | `:unbounded` | `:asserted` |
| Recorded, end genuinely unknown | `:unbounded` | `:indeterminate` |
| Claim predates the axis | *(slot absent)* | — see "Absence" below |

This reuses vocabulary the substrate already has rather than adding a
parallel one, matching the absence-versus-value discipline `standing`
already follows (#130). The extent's standing is independent of the
**claim's** own `claim-standing` — a claim can be `:inferred` while the
record of when it was inferred is `:asserted`.

**`:indeterminate` is overloaded, so it cannot signal absence by itself.**
The table above lists `:indeterminate` as a legitimate standing for a
*recorded* claim whose end is genuinely unknown — the same value
`claim-recorded-at` returns for a claim that predates the axis entirely
(see "Absence" below). A caller asking "does this claim carry a stamp at
all" must test `claim-recorded-at`'s first value for `NIL`, not compare
its standing to `:indeterminate`.

## Stamping

The substrate stamps every claim; the caller may override only at
construction, through the `MAKE-<class>` wrapper `def-claim-classes` emits
(`spacetime/claim.lisp:241-256`), via `%claim-encode-transaction-arg`
(`spacetime/claim.lisp:102-134`).

**What a caller may pass**, mirroring the existing `:extent`/`:extent-sexp`
pattern:

- `:recorded-at <timestamp>` — shorthand; builds the open interval
  `[timestamp, :unbounded)`, standing `:asserted`.
- `:transaction-extent <temporal-extent>` — full control, for an ingest path
  that knows a closed period or a non-`:asserted` standing.
- `:transaction-extent-sexp <sexp>` — the raw stored form, accepted for
  symmetry with the other two.

Passing more than one of these three **keys** signals — checked by counting
which keys are present (`%plist-key-p`), not which values are non-`NIL`:

```lisp
(let ((n (count-if (lambda (k) (%plist-key-p args k))
                   '(:transaction-extent :recorded-at
                     :transaction-extent-sexp))))
  (when (> n 1)
    (error "Pass only one of :TRANSACTION-EXTENT, :RECORDED-AT or ~
:TRANSACTION-EXTENT-SEXP.")))
```

(`spacetime/claim.lisp:109-114`.) That distinction matters: `:recorded-at
nil` supplied alongside `:transaction-extent <e>` still conflicts, even
though one of the two values is `NIL` — a review-caught fix, see below.

**Default, and the NIL case.** Neither key given: `%stamp-now` prepends a
fresh `[now, :unbounded)` stamp, standing `:asserted`
(`spacetime/claim.lisp:92-100`). A key that **is** present but whose value is
`NIL` — e.g. `:recorded-at nil` — is treated identically to the key being
absent, not as "leave unstamped": every one of the three branches in
`%claim-encode-transaction-arg`'s `cond` falls through to `%stamp-now` when
its value is `NIL`. This was not the original shape; the first cut let a
present-but-`NIL` value silently produce an unstamped claim on two of the
three paths and a spurious `INVALID-BOUND` error on the third, caught in
review and fixed in `fe70b7b`. Nothing a tenant does — omitting the initarg,
or passing it explicitly `NIL` — can leave a new claim unstamped.

**Why an override exists at all.** This programme ingests documents and
field data that carry their own source system's recorded-at. Forcing every
claim to "now" would record a falsehood for that ingest path; the
alternative — a per-tenant `:extra-slots` field for the real value — is
exactly the divergence this unit exists to end.

## Regeneration re-stamps

Supersession in this substrate *is* regeneration — sweep, then insert,
with no second mechanism
(`docs/superpowers/specs/2026-08-10-claim-record-endpoint-abstraction-design.md`
§6.4). A producer's re-run marks every claim it owns deleted
(`delete-claims-by-producer`), then re-inserts its current output as
brand-new claims in a second transaction. Each new claim gets a fresh
transaction stamp — there is no update-in-place path that could let a
stamp survive a producer's re-run.

For rule-produced claims — this substrate's primary population —
`claim-recorded-at` therefore reports **when the producer last ran**, not
"when the substrate first learned it." That value is correct under
bitemporal semantics for that version of the claim: it really was
(re)recorded at that instant. But a tenant regenerating nightly will find
every claim that producer touched stamped last night, indistinguishable
from one whose belief genuinely changed for the first time that night —
and nothing currently warns them of that.

## Immutability, and its honest limit — accessor-level only

Transaction time is an audit field: once written it should not change.
`(setf claim-transaction-extent)` enforces that, but only at the accessor:

```lisp
(defun (setf claim-transaction-extent) (extent claim)
  (when (claim-transaction-extent-sexp claim)
    (error 'transaction-extent-immutable))
  (setf (claim-transaction-extent-sexp claim)
        (and extent (extent->sexp extent)))
  extent)
```

(`spacetime/claim-query.lisp:61-71`.) It refuses a second `setf` through the
decoded accessor on a claim that already carries a stamp, signalling
`transaction-extent-immutable` (`spacetime/conditions.lisp:95-103`).

**This is not tamper-proof, and the doc says so plainly rather than
implying otherwise.** Writing the raw slot,
`(setf (claim-transaction-extent-sexp claim) ...)`, bypasses the guard
entirely — nothing checks it, nothing refuses it. This is reachable by
more than an abstract "any code with a handle on the claim object":
`rest-put-vertex` (`rest.lisp:666-681`) copies the node inside a
transaction and writes **every** named data slot through `slot-value`, so
an authenticated REST client can clear or rewrite any claim's stamp over
the wire, no Lisp access required. Contrast `standing` on that same
endpoint: it *is* caught at commit by #149's value constraint, while
`transaction-extent-sexp` is caught by nothing — precisely the gap the
#109 constraint family, below, would close. (`graph-db/spacetime` depends
only on `graph-db/core`, so `rest.lisp` is absent from a tenant image
unless it also loads `:graph-db` and starts the REST server.) Real,
engine-level enforcement needs a "this slot may not change after creation"
constraint family, evaluated on the write path the way
`validate-value-constraints` now evaluates enumerated values for `standing`
(#149, `docs/value-constraint-design.md`). That family does not exist yet;
it is recorded here as the first concrete requirement for #109's remaining
units, not built in this unit.

## Absence: a claim predating the axis

A claim written before this unit has no `transaction-extent-sexp` key in its
`DATA` alist at all, and reads `NIL`:

```lisp
(defun claim-recorded-at (claim)
  (let ((e (claim-transaction-extent claim)))
    (if (null e)
        (values nil :indeterminate)
        (values (bound-earliest (extent-start e)) (extent-standing e)))))
```

(`spacetime/claim-query.lisp:73-80`.) `claim-transaction-extent` returns
`NIL`; `claim-recorded-at` returns `(values nil :indeterminate)`. `NIL` is
never read as "recorded at the epoch" — that would be a fabricated audit
record, worse than admitting the date is unknown.

**No migration pass rewrites an existing store.** Stamping a legacy claim
with the time a migration happened to run would itself be a fabrication.
This is a deliberate contrast with #149's `standing` guard: there, a `NIL`
or invalid value on a `:required` slot **was** damage — something the audit
pass (`check-value-constraints`) exists to find and something a `deletion
plus re-create` repairs. Here, absence is not damage; it is a meaningful,
permanent state that a legacy claim is allowed to carry indefinitely.
`tests/spacetime/claim-transaction-tests.lisp`'s
`a-claim-predating-the-axis-reports-indeterminate-not-the-epoch` pins this by
writing the raw slot to `NIL` on a copy of a real claim and confirming both
return values.

## The measured answer: does an old store open cleanly (Task 1)

`+claim-shared-slots+` gaining `transaction-extent-sexp` means **every claim
family's parent class gains a persistent slot**, in every tenant image that
re-evaluates its `def-claim-classes` form. The design's whole absence story
— an old node simply lacks the key and reads `NIL` — assumed that opening a
store written before the slot existed is benign. Given #144 (opening a graph
without its schema loaded dies with a bare `CLASS-NOT-FOUND`), that
assumption was tested on a real reopened store before any production code
was written, per the plan's Task 1 gate.

The probe added a slot to a throwaway claim family (`def-claim-classes`
re-emitted with one more slot, the same mechanism `+claim-shared-slots+`
uses) and reopened a store written before the slot existed. It was run
twice: once with both phases in the same SBCL image, and — the load-bearing
case, since a genuine upgrade needs the on-disk schema and the in-image
class to actually disagree — again split across two separate SBCL
processes, so the process that opened the store had *never* held the
pre-extra-slot class definition. **Both runs came back identical and
benign**, verbatim:

```
PROBE-OPENED: t
PROBE-CLAIM-FOUND: T
PROBE-OLD-SLOT: :INFERRED
PROBE-NEW-SLOT: NIL
PROBE-NEW-SLOT-WRITABLE: :OK
```

The graph opened; the pre-existing claim was found; its old slot
(`standing`) read back `:INFERRED` unchanged; the new slot read `NIL`; and
the new slot was writable via `copy`/`setf`/`save` on that old node. No
warnings in either phase, in either run.

**Nothing in this plan's test suite re-proves that measurement.** Every test
in `tests/spacetime/claim-transaction-tests.lisp` builds a fresh graph in a
temp directory (`with-claim-graph`/`with-temp-directory`) for the lifetime of
one test. A claim "predating the axis" only ever appears in this suite via
`a-claim-predating-the-axis-reports-indeterminate-not-the-epoch` writing the
raw `claim-transaction-extent-sexp` slot to `NIL` on a claim created in the
same session — a simulation of absence, never a store genuinely written by
an older schema and reopened under a newer one. Task 1's probe is therefore
the **only** evidence that an actual schema upgrade behaves this way, and
its artefacts (`/tmp/tt-schema-probe*.lisp`, `/tmp/tt-probe-store/`) were
deleted after the run, per the plan — they were throwaway by design, not
committed anywhere. A reader deciding whether to trust an upgrade against a
real production store should weigh this measurement, not the test suite, and
should be aware the measurement itself is not preserved as a re-runnable
artefact.

## The measured storage cost

Task 1's probe above answers schema compatibility, not price — whether a
persistent class can gain a slot is a different question from what that
slot costs once every claim carries one. Measured separately: the
default stamp's `extent->sexp` form serializes to **147 bytes** through
`graph-db:serialize`, and the `transaction-extent-sexp` slot key itself
costs another **26 bytes** — **173 bytes added to every claim,
unconditionally, with no opt-out.** At a million claims that is roughly
173 MB, before counting anything else the claim already carries. About
50 of those 147 bytes are two copies of the same timestamp:
`exact-bound` stores `earliest = latest`, so the open interval's start
bound serializes the same instant twice.

## GH #153: the pre-2000 codec defect (found here, fixed separately)

`serialize.lisp`'s `LOCAL-TIME:TIMESTAMP` codec has a pre-existing,
unrelated defect that lands squarely on this unit's headline use case.
Encoding writes `day-of` (signed; day 0 is local-time's epoch, 2000-03-01)
through `LDB`, which correctly captures its two's-complement bit pattern
even when negative:

```lisp
(defmethod serialize ((ts timestamp))
  ...
  (dotimes (i 8)
    (setf (aref v (incf offset)) (ldb (byte 8 (* i 8)) (day-of ts))))
  ...)
```

But decoding reads it back with `deserialize-uint64` — unsigned:

```lisp
(defmethod deserialize-help ((become (eql +timestamp+)) (bytes array))
  (make-timestamp :day (deserialize-uint64 bytes 0)
                  :sec (deserialize-uint64 bytes 8)
                  :nsec (deserialize-uint64 bytes 16)))
```

(`serialize.lisp:214-228`.) **Any timestamp before 2000-03-01 is silently
corrupted on read** — no error at write, none at read; a negative `day-of`
comes back as a huge positive integer, and any code that later decodes that
integer (e.g. `local-time:decode-timestamp`) either produces a nonsense date
or signals a `TYPE-ERROR` far from the original write. Filed as
[kraison/vivace-graph#153](https://github.com/kraison/vivace-graph/issues/153)
(public, domain-neutral); not fixed here — it is a core codec change
touching every persisted timestamp in the engine, out of this unit's file
list, and deserves its own review.

This is the sharpest caveat in the unit, because it is not abstract: the
whole point of `:recorded-at` is letting an ingest path record a source
system's own time, and historical documents plausibly predate 2000-03-01.
**This unit found the defect and #153 has since fixed it**, so ingesting a
genuinely old recorded-at now works. The account below is kept because it
records how the defect was found and why two of this plan's own tests carry
the stand-in dates they do — they hit it directly and had to move off
pre-2000 values while it stood:

- `recorded-at-overrides-the-default-stamp` originally used
  `1999-12-31T23:59:58Z` and hit plain read-side corruption
  (`DESERIALIZE` on the stored timestamp came back as a different instant
  entirely).
- `a-refused-overwrite-leaves-the-original-stamp` hit the same defect
  through a different path: a `TYPE-ERROR`
  (`(INTEGER -1000000 1000000)`, datum `50505469855535109`) surfacing
  inside `close-graph`'s snapshot write, immediately after the claim
  carrying the pre-2000 stamp was created — not on read at all, but on the
  write-side snapshot serialization that also round-trips through the same
  codec.

Both were moved to safe post-2000 stand-in dates (`2001-06-15T12:00:00Z`
and `2003-07-04T16:20:11Z` respectively), each distinct from every other
date already used in the file, with a docstring note against a future
"tidy this back to something historical" edit reintroducing the failure.

**The defect is read-side (and write-path-serialization-side) only — the
bytes actually written to disk are a faithful two's-complement
representation of the correct day.** A signed read recovers every pre-2000
timestamp already on disk, which is what let #153 be fixed with no migration
— deploying the corrected read repairs existing stores.

## API surface

New exports from `graph-db.spacetime` (`spacetime/package.lisp:50-52`):

```
claim-transaction-extent-sexp
claim-transaction-extent
claim-recorded-at
transaction-extent-immutable
```

No existing export changes meaning.

## What this unit deliberately does not do

- **No retention of superseded versions**, and so no `:as-of` query. The
  open interval is the seam that lands on later, by closing the end bound
  rather than changing the slot's shape.
- **No engine-level immutability.** `(setf claim-transaction-extent)`
  refuses an overwrite; the raw slot does not. Engine-level enforcement
  belongs to a "may-not-change-after-creation" constraint family, #109's
  remaining units.
- **No index on transaction time.** Nothing queries by it yet; adding one
  before a query needs it would be building an index for nobody.
- **No change to the Allen algebra.** It already operates on any extent, so
  both axes are already comparable with the operators that exist.
- **No fix to GH #153 *in this unit*.** The codec defect was filed rather
  than fixed here, deliberately: it touches every persisted timestamp in the
  engine and deserved its own change and its own regression tests. It has
  since been fixed.
- **Claim identity is unchanged.** `(producer, subject-namespace,
  subject-key, relation)` stays the unary key; transaction time is not
  part of it, so regenerating a claim — sweep, then insert, see
  "Regeneration re-stamps" above, not an in-place update — still lands on
  the same identity rather than creating a new one per transaction-time
  value, and "the current belief" stays well defined.

## Acceptance criteria

- [x] Every newly created claim, via `MAKE-<class>`, carries a
      transaction extent, with no tenant action required — including when
      a tenant passes an initarg key with an explicit `NIL` value.
- [x] `:recorded-at` and `:transaction-extent` let an ingest path record a
      source system's own time, distinct from the construction-time clock
      — confirmed post-2000, and pre-2000 once #153 was fixed.
- [x] Passing more than one of `:recorded-at`, `:transaction-extent`,
      `:transaction-extent-sexp` signals, checked by key presence rather
      than value.
- [x] `claim-extent` and every existing consumer behave exactly as before
      (independence confirmed by ablation: crossing the accessor onto the
      other slot goes red).
- [x] `(setf claim-transaction-extent)` refuses a second write on an
      already-stamped claim, confirmed by ablation and by a close-and-
      reopen check that the refusal left the original stamp intact.
- [x] A legacy claim (simulated: raw slot written `NIL`) reports
      `(values nil :indeterminate)` from `claim-recorded-at`, never the
      epoch; no store is rewritten.
- [x] The spacetime suite's check count rose by exactly the number of
      checks added, with no failures, at every task boundary, measured in
      a fresh process.
- [ ] A genuine cross-session schema upgrade is measured once (Task 1,
      throwaway probe, not committed) and found benign — not re-proven by
      the committed test suite. See "The measured answer" above.
