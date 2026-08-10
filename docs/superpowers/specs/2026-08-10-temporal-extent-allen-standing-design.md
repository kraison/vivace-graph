# Temporal extents, the Allen algebra, and standing — design

**Unit:** S1a, the first of three decomposed from #108.
**Issue:** [#130](https://github.com/kraison/vivace-graph/issues/130).
**Programme:** `cl-llm/docs/superpowers/specs/2026-08-09-spatiotemporal-substrate-programme-design.md`.
**Siblings:** #131 (the claim record), #132 (the onboarding contract and registration).

---

## 1. What this is

The temporal half of `graph-db/spacetime`: value types and total functions for
time with provenance, and the Allen interval algebra over them. No graph
storage of its own, no claim, no index, no query surface.

It exists in the engine rather than above it because the engine must eventually
be able to *index* a temporal extent, and an index cannot accelerate a type it
cannot see. Everything else about this unit is pure computation and could have
lived anywhere.

### 1.1 The decomposition, and why

#108 as filed holds five things: `standing`, `temporal-extent` + Allen, the
claim record, the source onboarding contract, and the registration API. They
form a dependency chain rather than independent subsystems, so this is not a
case where one spec would produce wasted work. It is a case where one spec
would produce a *violated boundary*: by the time a single pass reached
registration it would find the shape underdetermined without a concrete source
and reach for the tenant it knows.

Split three ways, built in order: **S1a** (this document, #130) — pure value
types and total functions; **S1b** (#131) — the claim record, where graph
classes, indexes and the multi-graph transaction contract bite; **S1c** (#132)
— the contract and registration, which carry the highest boundary risk.

### 1.2 Boundary rule

Nothing in this unit may name a concept from any tenant application. This is a
PR review checklist item. A design decision justified *only* by what one tenant
needs belongs in that tenant.

---

## 2. What the API is shaped for

The consumer is the **reasoner and the bundle assembler**, not a filter.

Three plausible consumers want three different APIs. A filter wants a predicate
and cares only about true/false. A reasoner wants the relation as a term it can
unify and chain. A bundle assembler wants the relation *plus its
qualification*, because it renders "occurred while X" and "may have occurred
while X" differently.

Building for the third gives the first two cheaply; building for the first
cannot be extended to the third without changing every call site. So the
primitive is the qualified relation, and predicates are conveniences over it.

---

## 3. The types

### 3.1 One mechanism for three kinds of not-knowing

An endpoint is not a timestamp. It is a **range within which the timestamp
lies**. That single change absorbs three separate problems:

| | encoding |
|---|---|
| imprecision — *"sometime in January 2026"* | the range spans the month |
| open-endedness — *"since March, still true"* | end range runs March → `:unbounded` |
| total ignorance — *"we have no idea when"* | both ranges `(:unbounded, :unbounded)` |

The third case matters most. An extent with no knowable interval yields the
full set of relations still consistent with it — "we cannot say", expressed in
the algebra's own terms. It composes with everything downstream instead of
requiring a special case at each consumer, and it is never `nil`.

That set is all thirteen for an **interval** of wholly unknown extent. For an
**instant** of wholly unknown position it is the five §3.3.1 marks reachable:
fewer, and correct — the endpoint coupling constrains the answer even when the
position does not.

### 3.2 The records

```
temporal-extent
  kind        :instant | :interval
  start       bound
  end         bound          ; :instant -- start and end are ONE bound
  precision   :year | :month | :day | :hour | :minute | :second | :nsec
  semantics   open vocabulary -- :event, :observation, :validity, ...
  standing    standing

bound
  earliest    timestamp | :unbounded
  latest      timestamp | :unbounded
```

`timestamp` is `local-time:timestamp`, which the engine already serializes
(`serialize.lisp:214`) and orders (`utilities.lisp:310`). A bound whose
`earliest` and `latest` are equal is exact.

`:unbounded` is **polarised by position**: in `earliest` it denotes negative
infinity, in `latest` positive infinity. A bound of `(:unbounded, :unbounded)`
is therefore "anywhere in time", not an error and not an empty range.

`:unbounded` never satisfies a strict inequality, so it can never *produce* a
definite verdict — but a comparison involving it can still *be* definite when
the other pair of endpoints settles it. `[2030, :unbounded]` against
`[:unbounded, 2020]` is definitely `:>`, because 2030 > 2020 regardless of how
far either side runs. Only the comparison that no endpoint pair can settle is
`:ambiguous`.

**A `:interval` may not be value-degenerate.** `make-interval` signals
`invalid-extent` when its two bounds compare `:=` — both exact and equal —
directing the caller to `make-instant`.

Without that rule a point in time has two spellings, `kind :instant` and a
collapsed `:interval`, and only one of them works: the signature table in §4.1
assumes `start < end` strictly, so a degenerate interval against another
degenerate interval computes `(:= := := :=)`, matches no row, and yields the
empty set. Found by §7.1's soundness property on its first run, which is what
that property is for.

Rejecting is the fix rather than widening the table, because extra rows would
re-open the disjointness collision §3.3.1 resolves. The bad state becomes
unrepresentable rather than handled — the same move §3.4 makes for standing.

Intervals whose endpoint *ranges* merely overlap stay legal: their ordering is
uncertain, not collapsed, and rejecting them would forbid a legitimate record.

**Intervals are closed, `[start, end]`.** This is Allen's own convention and it
is what makes `meets` mean anything: A's end and B's start are the same
instant, not merely adjacent. Granule ends are therefore the last representable
instant of the granule — `2026-01-31T23:59:59.999999999Z` for January at month
precision — which is what the UTC arithmetic in §3.5 produces.

**`precision` never enters comparison.** It is what *produced* a bound's width
at construction, retained for rendering and provenance. The width already
encodes it; feeding it to the algebra as well would make two sources of truth
for one fact, and they would drift.

**`semantics` is an open vocabulary and does not gate comparison.** Comparing
event time against validity time is the main use case — *did this happen while
that was true?* — so the algebra computes across semantics and reports both in
the result. Refusing to compare unlike semantics would break the primary query;
deciding which comparisons are meaningful is a consumer's judgement, not the
substrate's.

### 3.3 Why `kind` exists

"January 2026" and "sometime in January 2026" are different extents that a
naive four-timestamp encoding renders identically.

The first is an **interval with exact endpoints** — Jan 1 to Jan 31. Two of
them genuinely are `equals`.

The second is an **instant whose position is uncertain** — one timestamp
somewhere in that month. Two of them can only be `before`, `after`, or
`equals`.

Store start and end as independent ranges and the instant case silently admits
`overlaps`, `during`, `starts` — relations that require the endpoints to move
independently, which for an instant they cannot. That is over-reporting
uncertainty: a quieter failure than under-reporting, but still a wrong answer,
and one that would survive review because the output *looks* appropriately
humble.

`kind :instant` couples the two endpoints to one bound. This is what "an
instant is a degenerate interval" has to mean concretely.

**One option is explicitly ruled out.** Giving an instant a granule-width
interval — "sometime in January" *becomes* the January interval — would restore
`start < end` and let classical Allen apply untouched. It also makes the two
extents above identical again, which is the bug this section exists to prevent.
Recorded so it is not re-proposed as a simplification.

### 3.3.1 Degenerate intervals and the thirteen

Allen's thirteen relations are jointly exhaustive and pairwise disjoint **only
where `start < end` strictly**. `kind :instant` puts us outside that domain by
construction, and the collision is not hypothetical: an instant at
`2026-01-02T00:00:00Z` against `[Jan 2, Jan 3]` satisfies both `meets`
(`e1 = s2`) and `starts` (`s1 = s2 ∧ e1 < e2`).

Returning both would look like honest uncertainty while actually being a
definitional collision — indistinguishable, downstream, from a real ambiguity,
and therefore worse than the failure §3.3 prevents.

**Resolution: one closed vocabulary, with degenerate cases folded onto it by
documented rule.** For an instant against an interval:

| instant lies | relation |
|---|---|
| before the interval | `:before` |
| coincident with its start | `:starts` |
| strictly inside | `:during` |
| coincident with its end | `:finishes` |
| after the interval | `:after` |

`:meets`, `:overlaps`, `:contains`, `:finished-by`, `:started-by`, `:equals`
and the remaining inverses are **unreachable** when either side is an instant
and the other an interval. Instant against instant yields only `:before`,
`:equals`, `:after`.

The tie-break is principled rather than arbitrary: under closed intervals a
point at B's start *is* inside B, so `starts` — coincident beginning plus
containment — states strictly more than `meets` does, and `meets` states
nothing `starts` does not.

The alternative considered was a Vilain-style point-interval sub-algebra with
its own relation names. Rejected because `temporal-relation` would stop being
one closed set, and every consumer — including S4's Prolog functor — would have
to branch on which algebra produced a result.

### 3.4 Standing is orthogonal to precision

`standing` is `observed | inferred | asserted | searched-empty | uncovered |
indeterminate`, as its own field.

An `observed` month-precision extent is **fully observed and badly bounded**.
Those are different facts: standing records *how we came to know*, precision
records *how sharply it is pinned*. Folding imprecision into `indeterminate`
standing would destroy both.

Standing is a **type, not a convention**. The three absence cases — a source
looked and found nothing; no source covers this; we could not find out — must
be distinguishable by construction. The absence-vs-value defect class has
seven-plus confirmed instances in the reference application, silent every time;
the canonical one coerced a never-computed gap to `0d0`, so a 4.1%-surveyed
area rendered downstream as a confident claim of full coverage. The API must
make that collapse **unrepresentable**, not merely reviewed against.

---

### 3.5 Granules are computed in UTC

A precision-P record like "January 2026" denotes the granule
`[2026-01-01T00:00:00.000000000Z, 2026-01-31T23:59:59.999999999Z]`. The
constructor derives those two timestamps from the precision.

**Pinned to UTC, always.** `local-time:encode-timestamp` uses
`local-time:*default-timezone*` unless told otherwise, so an unpinned
constructor would put January's boundaries at a different absolute instant on a
host in Helsinki than on one in UTC — and the same source record would compare
differently on two machines. Every construction passes
`:timezone local-time:+utc-zone+` explicitly.

The arithmetic, verified against `local-time` before this spec was written:

```lisp
(let* ((z local-time:+utc-zone+)
       (start (local-time:encode-timestamp 0 0 0 0 1 month year :timezone z))
       (end   (local-time:timestamp- (local-time:timestamp+ start 1 :month)
                                     1 :nsec)))
  ...)
```

Zero the parts below the precision to get the granule start; add one unit of
the precision and subtract one nanosecond to get the granule end. Uniform
across `:year` through `:nsec`, and it gets February and leap years right
without a table.

## 4. The algebra

### 4.1 How the set is computed

Allen's thirteen relations are fully determined by the signs of four endpoint
comparisons: `s1?s2`, `s1?e2`, `e1?s2`, `e1?e2`.

With range-valued endpoints each comparison yields `<`, `>`, `=`, or
**ambiguous** — ambiguous exactly when the two ranges overlap. Enumerate the
sign-vectors consistent with those four results, map each to its relation, and
that is the set.

Closed form. No search. Bounded by thirteen. `kind :instant` enters here as the
constraint coupling an extent's endpoints, pruning the vectors a decoupled
encoding would wrongly admit (§3.3).

### 4.2 The thirteen

Seven base relations and six inverses. These keywords are the vocabulary; they
are closed, unlike `semantics` and `standing`.

```
:before  :meets  :overlaps  :starts  :during  :finishes  :equals
:after   :met-by :overlapped-by :started-by :contains :finished-by
```

`:equals` is its own inverse, which is why thirteen and not fourteen.

Not all thirteen are reachable for every pair: §3.3.1 restricts which apply
when either extent is an instant. The vocabulary stays closed regardless.

### 4.3 Surface

```
temporal-relation
  relations   set of the thirteen keywords, never empty
  standings   set of the contributing standings          ; see 4.4
  semantics   set of the contributing semantics          ; see 3.2

(allen-relations a b)   => temporal-relation   ; the set, always
(allen-relation a b)    => keyword | nil       ; the relation iff singleton
(allen-definite-p a b)  => boolean             ; iff RELATIONS is a singleton
(extent-before-p a b)   => boolean             ; one per relation, 13 total
```

`relations` is never empty: two extents always stand in at least one Allen
relation, and total ignorance is all thirteen rather than none. An empty set
would be a bug, and is worth asserting internally.

Predicates are set membership. A filter never handles a set; a reasoner never
loses one. `temporal-relation` is a value, so S4's Prolog functor (out of scope
here) is not precluded.

### 4.4 Standing propagation

**A `temporal-relation` carries the set of contributing standings, not a
collapsed weakest value.**

This departs from the "inherits the weaker standing of its two endpoints"
wording in #108 and §4 of the programme design, both amended alongside this
spec. Two reasons.

*There is no defensible total order.* `observed` is clearly strongest and
`indeterminate` clearly weakest, but **`asserted` versus `inferred` cannot be
ordered**: an operator's assertion is a human commitment, a rule's inference is
reproducible and rule-versioned. Neither dominates. Forcing them onto one axis
means inventing an answer and then imposing it on every tenant through the
substrate.

*The absence cases are not weaker values.* They are the states where there is
no interval at all. An extent standing `searched-empty` yields all thirteen
relations, and what a consumer needs is the *reason* — which a collapse to a
single weakest label preserves only by accident.

A set can always be collapsed downstream by a consumer that knows its own
ordering. A collapse cannot be un-done. This is the same argument that produced
the relation set in §4.1, applied to the second axis.

---

## 5. Packaging

Follows `graph-db/geos`:

```
(defsystem graph-db/spacetime
  :depends-on (:graph-db/core)
  :pathname "spacetime/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "standing")
               (:file "bound")
               (:file "extent")
               (:file "allen")))
```

`:graph-db/core`, not `:graph-db` — nothing here needs the network leaves, and
the embeddable core build should be able to load it. A `graph-db/spacetime-test`
system sits beside it, mirroring `graph-db/geos-test`, with a `:perform
test-op` that fails the build on a red suite.

---

## 6. Storage

**No core change.**

`serialize` is a generic function, so a subsystem can add a method — but
`deserialize-help` dispatches on a type byte, and reserving one means editing
core's constants.

Instead, an extent encodes as a list of values core already serializes:
keywords, `local-time:timestamp`s, and `:unbounded` as a keyword. The extent
provides its own `extent->sexp` / `sexp->extent` pair, and S1b's claim class —
which knows its slot holds an extent — reconstructs on read.

This is what boundary rule 4 asks for: `graph-db/core` gains nothing from this
programme except, eventually, an interval index, and only once measured. If
profiling later shows the list encoding costs something real, a type byte is a
small, separate core change made against evidence rather than anticipation.

---

## 7. Testing

### 7.1 Soundness — the property that matters

For any two extents, instantiate concrete timestamps within each endpoint
range, compute classical Allen on those exact values, and assert the result is
a **member** of the returned set. Over many random instantiations.

**The set may never omit the truth.** If it can, the algebra produces
confidently-wrong answers, which is the failure this unit exists to prevent.

### 7.2 Completeness — no over-reporting

Every relation in the set must be achieved by *some* instantiation. Checkable
by enumeration on a coarse grid. This is the test that catches the decoupled-
instant bug of §3.3, which soundness alone would pass.

### 7.3 Exactness

Two **interval** extents with exact endpoints return singletons equal to
classical Allen, and the thirteen relations are jointly exhaustive and pairwise
disjoint there.

Where either side is an **instant**, exact endpoints must still return a
singleton — the one §3.3.1's table names — and the relations that section marks
unreachable must never appear. That is the assertion that catches a
reintroduced `meets`/`starts` collision.

### 7.4 Absence-vs-value conformance

The category §11 of the programme design mandates. For every constructor:

- "no interval" is distinguishable from "zero-length interval at the epoch";
- an unbounded extent yields relations rather than an error or a default —
  thirteen for an interval, the five reachable ones for an instant (§3.1);
- no accessor returns a value that a caller could mistake for a measurement.

---

## 8. Not in this unit

- The claim record and the endpoint abstraction — #131.
- The source onboarding contract and the registration API — #132.
- Any interval index. §7 of the programme design says measure first whether a
  composite `(place, valid-start)` index already serves the temporal access
  path.
- The Prolog functor. S4 (#109).
- Any query planner or LLM-facing surface. Those live above this layer.

---

## 9. Acceptance

S1a is **not proven by its own test suite.** P2's two tenants are the
acceptance test for P1 as a whole, one of them declaring `space: none`. A
substrate with one tenant becomes that tenant's library no matter how carefully
it is reviewed.

What S1a's suite establishes is narrower and still worth stating: the algebra
is sound, complete, exact on exact input, and cannot represent the
absence-as-value collapse.

---

## 10. Version floor

A new opt-in subsystem adds no version floor for existing consumers: nothing in
`graph-db/core` changes, so `graph-db` and `graph-db/geos` consumers are
unaffected and no production host needs bumping. The floor applies only to a
consumer that wants to `:depends-on (:graph-db/spacetime)`.
