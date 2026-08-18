# Declarative value constraints for VivaceGraph (`def-value-constraint`) — design (issue #149)

**Status:** v1 **implemented** on branch `experiment` (`value-constraint.lisp`):
slot-level `:one-of` / `:required`, commit-boundary enforcement inside `%COMMIT`'s
manager-locked region, NULL-exempt (matching `def-unique`), enforced across
subtypes via `subtypep`, an audit pass over live vertices. This is unit 1 of the
ontology epic (#109); see `docs/spatiotemporal-substrate-programme.md`.

## Motivation

A slot whose value is drawn from a closed enumeration — the spacetime substrate's
`standing` vocabulary is the motivating case — needs that closure enforced, not
merely documented. `standing` was checked once, at construction
(`check-standing`, `spacetime/standing.lisp:40`), inside the generated
`MAKE-<NAME>` wrapper. But `standing` also carries a plain `:accessor
claim-standing`, so the ordinary graph-db update idiom — `copy`, `setf`, `save`
— walked straight past the check. A probe recorded in the design spec (GH #149)
showed the result was not merely wrong in memory: an invalid standing committed,
survived a close and reopen, and was durable on disk.

`def-value-constraint` declares that closure once, per `(class, slot)`, and
enforces it at every write, regardless of which accessor produced it:

```lisp
(def-value-constraint ct-claim standing :graph-db-claim-test
  :one-of +standings+
  :required t
  :name standing-vocabulary)
```

This is the real declaration `def-claim-classes` emits, on the claim parent
class, at `spacetime/claim.lisp:146`. `standing` lives on the parent; `:one-of`
names `+standings+` (`spacetime/standing.lisp:6`) rather than repeating its
seven keywords, so the constraint and `standingp` cannot drift apart. `:required
t` additionally makes a `NIL` standing a violation — see "Null semantics"
below.

## The key realization: no index needed, deliberately simpler than `def-unique`

A value constraint is a predicate over one node's own slot: no cross-node
lookup, no intra-transaction dedup table, no rebuild-on-open, no sidecar, no
reconciliation. Everything #139/#140 built for schema *declarations* —
per-`(class, slot)` registration, identity by `:name`, replace-in-place on
re-registration, inheritance by `subtypep` — applies unchanged; none of the
index machinery `def-unique` needed does. This is worth saying explicitly,
because a reader arriving from `docs/unique-constraint-design.md` or the index
subsystem will look for the missing half and should find its absence
documented rather than assume an oversight.

## Where enforcement happens

`validate-value-constraints (tx graph)` runs inside `%COMMIT`'s
`with-transaction-manager-lock` region (`transactions.lisp:3058`), immediately
after `validate-unique-constraints` (line 3055) and — like it — after `VALIDATE`
(the OCC check) and before `finalize-tx-persistence` / `apply-transaction`.

This placement, not a construction-time check, is what makes the guard strong:
because it runs inside the one serialized region every commit passes through,
it sees every write on every path, not only the constructor. And because it
runs *before* `finalize-tx-persistence`, a violation aborts before anything is
journaled — the `unwind-protect` around `%commit` drops the temp file, so a
refused value never reaches disk. A predicate wired only into the constructor,
as `check-standing` was, cannot make either guarantee: it never sees a
`copy`/`setf`/`save`, and even if it did, checking after journaling is too late
to abort cleanly.

`check-standing` is not removed by this. It stays at construction as a
fast-fail with a better error site (reject before a transient object is even
built); it stops being the *only* thing standing there.

## Null semantics

Without `:required`, `NIL` is exempt: `:one-of` alone reads *"if present, it
must be one of these"*, matching `def-unique`'s null rule (SQL
unknown-never-equals-unknown). This is deliberate, not incidental — GH #107
named the trap of two neighbouring macros disagreeing about nulls, where a
reader who has learned one macro's rule assumes the other shares it.

`:required t` closes the other half: a `NIL` slot violates. `standing` ships
with both, because `:one-of` alone would only have closed the wrong-value half
of the probed hole — `(setf (claim-standing c) nil)` followed by `save` would
still have committed.

## `:one-of` is evaluated; the slot and `:name` are quoted

```lisp
(def-value-constraint ct-claim standing :graph-db-claim-test
  :one-of +standings+   ; EVALUATED
  :required t
  :name standing-vocabulary)   ; QUOTED
```

`:one-of` is evaluated so a caller can name an existing vocabulary constant —
`+standings+` — instead of duplicating its contents inline. The slot and
`:name` are quoted, as they name symbols rather than compute values.

**The staleness consequence:** because `:one-of` is evaluated, its value is
captured into the spec *at registration time*. Editing `+standings+` after the
fact does not retroactively change an already-registered constraint — the spec
holds the list it saw when the `def-value-constraint` form last ran, not a
reference to the variable. The fix is to re-evaluate the declaration. That is
safe and idempotent: registration replaces any existing spec of the same
identity (owner, slot, `:name`) rather than adding to the registry
(`register-value-constraint-spec`, GH #139), so re-running the form after
editing the constant simply installs the corrected `:one-of` in place.

## Where it is enforced, and inheritance

Declaring a constraint on a parent class covers every subclass: applicability
is decided by `class-value-constraint-specs`, which matches `(subtypep
(class-name class) owner)` — exactly the rule `class-unique-tuple-specs`
already uses for `def-unique`. This is load-bearing for claims: `standing`
lives on the parent class that `def-claim-classes` builds, and the parent
generates both a `-unary` and a `-binary` subclass. One declaration on the
parent therefore guards both arities without a second declaration on either.

## `check-value-constraints` — the audit path

The write-path check (`validate-value-constraints`) and the audit pass share
one evaluator, `%value-constraint-violations`: the write path signals on the
first violation it finds; the audit path collects every one, over every live
vertex, without signalling.

```lisp
(check-value-constraints graph &key vertex-type)
;; => (values violations checked-count spec-count)
```

The audit path exists because the probe that justified this unit proved
invalid values are already writable today — any store predating this
constraint may already hold damage the write-path guard cannot retroactively
see. `:vertex-type` narrows the scan to one type and keeps it snapshot-
consistent (`map-vertices`); the untyped scan reads live node versions and
bypasses MVCC, so it is meant for admin passes over a quiescent graph, not for
use inside a live transaction.

**`spec-count` is part of the answer, not a diagnostic.** Zero violations over
zero registered specs is an *unchecked* graph, not a clean one — the graph may
carry no declarations in this image at all, and a caller that prints "OK"
without reading `spec-count` would be reporting a population of zero as if it
were a clean population. This follows GH #129's rule for schema state:
consult-only, never conjure, and loud about the difference. `spec-count`
also counts every declaration registered on the graph, not only the ones
applicable to what was actually scanned — a spec on class B inflates
`spec-count` even when `:vertex-type` narrowed the scan to class A, or when
the spec sits on an edge class the vertex-only scan never visits. Reading
`spec-count` as "specs relevant to this scan" rather than "specs registered on
this graph" is the mistake to avoid.

## Retraction, and its trap (GH #152)

`undef-value-constraint` withdraws a declaration by `:name` or `:slot`, both
keyword arguments (a graph name is itself a keyword, so a positional form
could not tell a slot from a graph without guessing). Like `def-value-
constraint`, it **quotes** `:name` rather than evaluating it. That means a
caller who reads the same characters from a different package interns a
*different* symbol — package-qualification is invisible at the call site — and
the withdrawal silently matches nothing: `unregister-value-constraint-spec`
returns `NIL`, and nothing else reports the miss. `undef-index` and
`undef-unique` quote `:name` identically, so this is not specific to value
constraints; it is filed engine-wide as GH #152. It bites hardest exactly where
this subsystem cares most: schema emitted by a macro (`def-claim-classes`) is
registered under the macro's own package, not the caller's, so a caller
retracting from outside that package is the case most likely to hit it. See
GH #152 for the proposed fix; it is not re-argued here.

## What unit 1 deliberately does NOT do

- **Other claim slots.** `claim-extent`, `claim-producer`, `claim-relation` and
  the rest of the claim accessors have the *same* unguarded update path that
  the probe exposed for `standing` — `def-claim-classes` declares a value
  constraint only on `standing`. Whether extent well-formedness (or any other
  claim slot) becomes declarative is a later unit's question, not an oversight
  in this one.
- **Multi-slot / cross-slot constraints.** A value constraint is slot-only.
  Generalising it is deferred until a unit actually needs it (design spec,
  GH #149 Q2).
- **Units 2–4** — cardinality beyond `:required`, domain/range, disjointness.
  Deliberately left unfiled until unit 1's shape informs theirs, rather than
  guessed at up front.
- **Retroactive repair.** `check-value-constraints` reports damage; it does
  not fix it. Deletion, or deletion plus re-create, is the repair path — see
  the upgrade hazard below.

## An upgrade hazard

A store that already holds claims with an invalid or `NIL` standing — exactly
the damage the probe behind #149 showed was possible before this unit — will
now have **every** subsequent update to those nodes rejected, including an
update meant to fix something else entirely. The constraint evaluates the
node's whole current state, not the delta a write proposes, so a write cannot
partially repair a node that already fails the constraint on an untouched
field. Deletion still works (a delete claims nothing and is skipped by the
constraint check), so deletion followed by re-creation is the repair path for
an already-damaged node. `check-value-constraints` (the audit pass, above) is
how such damage is found before it blocks an unrelated write. This is by
design, not a defect, but it is not obvious from the code, so it is recorded
here.

## A known gap, pre-existing and untested

Nothing in the current suite proves that re-evaluating a `def-claim-classes`
form does not *grow* the registry — i.e. that re-running the macro replaces
each of its named declarations in place rather than stacking a duplicate.
`def-claim-classes` emits six named declarations (one value constraint, two
`def-unique`, three `def-index`); this gap applies equally to all six, not
only the value constraint. It is exactly the failure mode a stable `:name`
exists to prevent (GH #139, #140): identity by name is what makes
re-declaration a replacement rather than an accumulation, and nothing
currently exercises re-evaluating the whole macro to confirm that holds in
practice. Recorded as known and untested, not as solved.

## Acceptance criteria

- [x] `def-value-constraint` on a slot rejects a `copy`/`setf`/`save` that
      violates a declared `:one-of`, signalling `value-constraint-violation`,
      distinct from the retriable OCC `validation-conflict`.
- [x] `:required t` additionally rejects a `NIL` value on the same path;
      without it, `NIL` is exempt.
- [x] The rejection happens before anything is journaled — a refused write is
      not durable, confirmed by close-and-reopen, not by an in-session read
      (the node cache has made earlier tests in this programme vacuous by
      serving the right answer from memory).
- [x] A constraint declared on a parent class rejects a bad write on every
      subclass, confirmed at both claim arities.
- [x] A constraint does not reject any valid value: every member of
      `+standings+` commits.
- [x] `check-value-constraints` finds a pre-existing invalid value written
      before the constraint was declared, and reports the population it
      checked (`checked-count`, `spec-count`), not a bare pass/fail.
- [x] `undef-value-constraint` withdraws enforcement; re-declaring restores
      it.
- [ ] Re-evaluating `def-claim-classes` does not grow the registry — untested,
      see "A known gap" above.
