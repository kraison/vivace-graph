# Transaction time on the claim record — design (issue #148)

A claim can currently say when a fact was **true** or when it was
**recorded**, never both. `+claim-shared-slots+` carries one temporal slot,
and the unary uniqueness constraint forbids the obvious workaround of one
claim per axis, so the limit is structural rather than stylistic. See #148
for the full statement of the problem.

This design adds the second axis to the substrate, so that tenants stop
inventing a `recorded-at` apiece through `:extra-slots` — the divergence
#148 objects to.

## Scope: a stamp now, as-of later

The deliverable is that **every claim records when it was recorded**, and
that the shape chosen does not foreclose reconstructing past belief.

In scope: the record shape, who assigns the value, what absence means, and
the accessors. Out of scope: retaining superseded versions, and any
`:as-of` query. That is a later unit and overlaps MVCC Phase C (#113-#117)
rather than sitting beside it.

The test of this design is therefore not "can we answer as-of questions" —
we cannot yet, by choice — but "when as-of arrives, is this a matter of
closing an interval rather than changing a shape".

## The record

One new slot on `+claim-shared-slots+`, mirroring `extent-sexp`:

```lisp
(transaction-extent-sexp :initarg :transaction-extent-sexp
                         :accessor claim-transaction-extent-sexp
                         :initform nil)
```

It holds the same versioned `(:temporal-extent 1 …)` sexp the validity
extent uses. **No new codec, no new core type byte** — the existing
`extent->sexp` / `sexp->extent` pair serves both axes, and a bug fixed in
one is fixed in both.

The decoded accessors mirror `claim-extent` exactly:

- `claim-transaction-extent` — the decoded `temporal-extent`, or `NIL`.
- `(setf claim-transaction-extent)` — stores the sexp, with the
  immutability rule below.
- `claim-recorded-at` — convenience returning two values, the recorded
  timestamp and the extent's standing. Without it every consumer reaches
  through `extent-start` into `bound-earliest` for the common case.

`claim-extent` keeps meaning **validity**. Nothing that reads a claim today
changes, which is what keeps #148's constraint true: a tenant declaring
neither axis stays fully supported.

The stored value is an **interval**, not an instant, with
`semantics :transaction`. Transaction time is a point *when it begins*, but
the classical bitemporal record is a period — `[recorded, superseded)` —
and `bound` already accepts `:unbounded` at either endpoint. The open
interval therefore costs nothing to express today and becomes the retention
mechanism later by having its end set rather than its shape changed.

## Open versus unknown, and why standing carries it

"Still believed" and "we do not know when belief ended" are different facts.
Both have an unbounded upper endpoint, so the endpoint cannot distinguish
them. The extent's own `standing` does:

| Case | End bound | Extent standing |
|---|---|---|
| Recorded, still believed | `:unbounded` | `:asserted` |
| Recorded, end genuinely unknown | `:unbounded` | `:indeterminate` |
| Claim predates stamping | *(slot absent)* | — see below |

This reuses the vocabulary the substrate already has rather than adding a
flag, and it is the same absence-versus-value discipline the standing work
was built on (#130, design §3.4). A reader that cannot tell an open period
from an unknown one cannot answer "was this conclusion reasonable given what
was known then", which is the whole reason #148 exists.

The extent's standing is independent of the **claim's** standing, exactly as
it already is for validity — a claim may be `:inferred` while the record of
when it was inferred is `:asserted`.

## Stamping

The substrate stamps; the caller may override at creation only.

**Where.** In the `MAKE-<class>` wrapper `def-claim-classes` already emits,
beside `check-standing`. That wrapper is the single construction path — the
raw constructor is redefined on every expansion so it cannot double-wrap —
and it is where claim-level validation already lives.

**What a caller may pass.** Mirroring the existing `:extent` initarg, which
`%claim-encode-extent-arg` converts to `:extent-sexp`:

- `:recorded-at <timestamp>` — shorthand, builds the open interval.
- `:transaction-extent <temporal-extent>` — full control, for an ingest
  path that knows a closed period or a non-`:asserted` standing.

Passing both, or passing either alongside `:transaction-extent-sexp`,
signals — the same rule `%claim-encode-extent-arg` already applies to
`:extent` and `:extent-sexp`.

**Default.** Neither given: stamp the current time, standing `:asserted`.
Nothing a tenant does can leave a new claim unstamped.

**Why an override exists.** This programme ingests documents and field data
that carry their source system's recorded-at. Forcing those to "now" would
record a falsehood, and the alternative — a per-tenant slot for the real
value — is the divergence this unit exists to end.

## Immutability, and the honest limit of it

Transaction time is an audit field: once written it must not change. This
design enforces that **at the accessor**, and states plainly that accessor
enforcement is not the same as the engine refusing it.

`(setf claim-transaction-extent)` signals when the claim already carries a
transaction extent. Setting `claim-transaction-extent-sexp` directly
bypasses it, as does any write reaching the raw slot.

Full enforcement needs a constraint family that says "this slot may not
change after creation", evaluated on the write path the way
`validate-value-constraints` now evaluates enumerated values (#149). That is
squarely #109's remaining units, and building it here would widen this unit
past what it is for.

**Recorded as a known limitation, not as solved:** until that family exists,
a determined or careless tenant can overwrite a stamp, and nothing in the
engine will refuse it. This is the first concrete requirement for #109's
next unit and should be cited there.

*Closed by #158:* `def-claim-classes` now declares the stamp as a
`:transition` constraint, `transaction-extent-step`, enforced at commit on
every write path including `rest-put-vertex`. The accessor guard remains
as the fast-fail with the better error site.

**Addendum (#162).** One change after creation is sanctioned: *closing*
the period. `retract-claim` writes `[recorded, at)` — the `[recorded,
superseded)` named above — and is the only writer that does so; the
accessor still refuses a replacement. Re-registration of a retracted
claim re-opens it with a fresh stamp, so the closed period survives only
in MVCC: the "no retention of superseded versions" limitation below, not
a new one. #158's write-once family should therefore treat the start as
immutable and the end as closeable once.

## Absence, and claims already on disk

A claim written before this unit has no such key in its `DATA` alist and
reads `NIL`. **`NIL` must never be read as "recorded at the epoch."**

`claim-transaction-extent` returns `NIL` for such a claim, and
`claim-recorded-at` returns `(values nil :indeterminate)` — we genuinely do
not know when it was recorded, and `:indeterminate` is the vocabulary's word
for exactly that.

**No migration pass rewrites existing stores.** Stamping a legacy claim with
the time the migration ran would fabricate an audit record, which is worse
than admitting ignorance. Absence here is a meaningful state, not damage —
a deliberate contrast with #149, where absence over a `:required` slot *was*
damage and the audit pass existed to find it.

## What does not change

**Claim identity.** `(producer, subject-namespace, subject-key, relation)`
stays the unary key. Under a stamp model no two live claims differ only by
transaction time — one claim carries one stamp, updated in place — so the
key remains an identity and "the current belief" stays well defined. When
retention arrives, versions belong to that mechanism, not to claim identity.
#148 raised this as needing revisiting; this is the revisit, and the answer
is no change.

**MVCC epochs.** Transaction time is a wall-clock stamp that coexists with
epochs rather than reusing them. #116 records that epochs are node-local and
so cannot address an instant across replicas, which disqualifies them as the
value here. The two answer different questions — an epoch orders writes
within a store, a transaction time says when the world learned something —
and conflating them would make the audit record unusable in a replicated
deployment.

## API surface

New exports from `graph-db.spacetime`:

```
claim-transaction-extent-sexp
claim-transaction-extent
claim-recorded-at
```

No existing export changes meaning.

`+claim-shared-slots+` gains one entry, so **every claim family's parent
class gains a persistent slot**. The list is expanded at macroexpansion
time, so a tenant's existing `def-claim-classes` form picks the slot up when
that form is next evaluated — not retroactively in a running image.

⚠ **This is a schema change to an existing persistent class, and the
implementation plan must establish what that costs before writing code.**
Specifically: whether a graph whose on-disk schema predates the new slot
opens cleanly, and what `def-vertex` does when a class gains a slot between
sessions. #144 (opening a graph without its schema file loaded dies with a
bare `CLASS-NOT-FOUND`) is evidence this area has sharp edges. The absence
handling above assumes the benign answer — an old node simply lacks the key
and reads `NIL` — and that assumption is load-bearing for the whole
migration story, so it gets measured on a real reopened store rather than
reasoned about.

## Testing

Each item is a property that must go red for a stated reason. Two tests in
the last unit read as correct while pinning nothing (#149's review), so a
test whose whole value is that it would fail must be **shown** to fail.

1. A claim created without either initarg carries a transaction extent whose
   start is an exact bound, standing `:asserted`, end unbounded.
2. `:recorded-at` overrides the default, and the stored start is the value
   passed rather than the current time — ablate by passing a timestamp far
   from now, so a stamp that ignored the argument cannot pass.
3. `:transaction-extent` accepts a closed period and a non-`:asserted`
   standing.
4. Passing conflicting initargs signals.
5. Validity and transaction extents are independent: setting one does not
   read or write the other. Ablate by making the accessor read the wrong
   slot and confirm this goes red — the two slots hold the same sexp shape,
   so a crossed accessor is invisible to a test that only checks decoding.
6. **The stamp survives a close and reopen.** The in-session read is not the
   test; the node cache has made two tests in this programme vacuous by
   serving the right answer from memory.
7. A claim with no transaction slot reads `NIL` and
   `(values nil :indeterminate)` — never the epoch.
8. `(setf claim-transaction-extent)` signals on a claim that already has
   one, and the store still holds the original after the refusal.
9. Both arities. #149's guard was declared once on the parent and reached
   binary only through `subtypep`; a test covering one arity looked complete
   and was not.
10. Every new test file carries `(in-suite …)`, and the suite's check count
    rises by the number of checks added. A file that loads without entering
    the suite passes by name while never running.

## Acceptance criteria

- Every newly created claim carries a transaction extent, with no tenant
  action required.
- An ingest path can record a source system's recorded-at without a
  tenant-local slot.
- A legacy claim reports indeterminate rather than a fabricated time, and no
  store is rewritten.
- `claim-extent` and every current consumer behave exactly as before.
- The spacetime suite's check count rises by exactly the number of checks
  added, with no failures, measured in a fresh process.

## What this deliberately does not do

- **No retention of superseded versions**, and so no `:as-of` query. The
  open interval is the seam where that lands.
- **No engine-level immutability**, as set out above.
- **No index on transaction time.** Nothing queries by it yet; adding one
  before a query needs it would be building an index for nobody.
- **No change to the Allen algebra.** It already operates on any extent, so
  both axes are comparable with the operators that exist.
