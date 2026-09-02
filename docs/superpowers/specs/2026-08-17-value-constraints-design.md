# Declarative value constraints — design

**Status:** approved, not implemented
**Issue:** #149 (unit 1 of the ontology epic, #109)
**Branch:** `experiment` (no release; see #109)

## The problem

The closed standing vocabulary is the spacetime substrate's central invariant.
It is enforced at construction and **nowhere else**, so any update path writes
straight past it.

`check-standing` (`spacetime/standing.lisp:40`) has exactly three call sites:

| site | when it fires |
|---|---|
| `spacetime/extent.lisp:52` | building an extent |
| `spacetime/extent.lisp:60` | building an extent |
| `spacetime/claim.lisp:177` | inside the generated `MAKE-<NAME>` wrapper — **construction only** |

The `standing` slot carries a plain `:accessor claim-standing`, and
`(setf claim-standing)` is a defined function. So the ordinary graph-db update
idiom — `copy`, `setf`, `save` — never meets the check.

### Measured, not argued

Probe against a real on-disk claim graph, 2026-08-17:

```
A. construct with :nonsense            -> REJECTED (INVALID-STANDING)
B. valid claim created, standing       =  :INFERRED
C. copy + (setf claim-standing) + save -> ACCEPTED
D. reads back as                       =  :NONSENSE   standingp => NIL
E. after close + reopen from disk      =  :NONSENSE   standingp => NIL
```

(E) is the line that matters. The invalid value is **durable** — it is on disk
and survives reopening the graph. This is not a node-cache artifact, which is
the failure mode that has made two earlier tests in this programme vacuous.

This is the defect shape the programme keeps meeting: an invalid value quietly
becoming a definite one, with nothing reporting that it happened.

### Why this justifies the unit

#149 set the bar explicitly: *subsuming `check-standing` is the test, not the
goal — the formalism earns its place only if the declarative form is at least
as strict and reports better. If it is merely equivalent, the honest outcome is
to say so and keep the hand-written one.*

It is **strictly stronger**. A commit-time constraint is evaluated where
`validate-unique-constraints` already is, so it sees every write regardless of
which accessor produced it.

## Design

### The macro

```lisp
(def-value-constraint ct-claim standing :graph-db-claim-test
  :one-of +standings+
  :required t
  :name standing-vocabulary)
```

Per-`(class, slot)`, keyed exactly as `def-index` and `def-unique` are (#149
Q2). Named from day one, reusing the #139/#140 identity machinery.

**`:one-of` is EVALUATED**, unlike `:slots` and `:name`, which are quoted. That
is what lets it name `+standings+` instead of duplicating the vocabulary — the
constraint and `standingp` must not be able to drift apart. The consequence: the
list value is captured into the spec at registration, so editing `+standings+`
does not retroactively change a registered constraint. Re-evaluating the
`def-value-constraint` form fixes that, and is idempotent (replace-by-identity,
#139).

### Simpler than `def-unique`, deliberately

A value constraint is a predicate over one node's own slot. It needs **no
index**: no cross-node lookup, no intra-transaction dedup table, no
rebuild-on-open, no sidecar, no reconciliation. Everything #139/#140 built for
schema *declarations* applies; none of the index machinery does.

This is worth saying out loud because the two neighbouring subsystems are much
heavier, and a reader coming from them will look for the missing half.

### Registry

Mirrors `*schema-unique-metadata*` (`unique-constraint.lisp:270`):

- `*schema-value-constraint-metadata*` — graph-name → list of specs, newest first
- `(defstruct value-constraint-spec owner-name slot-name graph-name one-of required name)`
- `value-constraint-spec-identity` → `%spec-identity` (index.lisp), the one shared identity rule
- `register-value-constraint-spec` — replace-in-place by identity
- `unregister-value-constraint-spec` / `undef-value-constraint` — keyword args, not positional, for the reason `undef-index` learned: a graph name is itself a keyword

`class-value-constraint-specs (class graph)` mirrors `class-unique-tuple-specs`
(`unique-constraint.lisp:344`): applicable when `(subtypep (class-name class)
owner)` and the slot exists on the class.

**Inheritance is therefore free, and load-bearing here.** `standing` lives on the
parent claim class; `def-claim-classes` generates `<parent>`, `<parent>-unary`
and `<parent>-binary`. One declaration on the parent covers both arities.

### Null semantics

`:one-of` alone reads *"if present, it must be one of these"* — `NIL` is exempt,
matching `def-unique`'s null rule (SQL unknown-never-equals-unknown). Diverging
from that rule would be exactly the trap GH #107 called out: two neighbouring
macros disagreeing about nulls where a reader would assume they agree.

`:required t` makes `NIL` a violation.

Both ship in unit 1 (#149 §4, option C). `:one-of` alone closes only the
wrong-value half of the hole probed above; `(setf (claim-standing c) nil)` +
`save` would still commit. Pulling the first half of unit 2 forward is a small
amount of extra surface and means the unit ships having actually closed the hole
its own probe found.

### Where it is enforced

`validate-value-constraints (tx graph)`, called from `%COMMIT`'s manager-locked
region at `transactions.lisp:3053`, immediately alongside
`validate-unique-constraints` — after VALIDATE, before durability, so a
violation aborts before anything is journaled.

Deleted nodes claim nothing and are skipped, as in `validate-unique-constraints`.

### Violation shape

One evaluator, two consumers (#149 Q3):

```lisp
(%value-constraint-violations node graph)
;; => list of records: spec, node-id, slot, actual, expected, reason
;; reason is :not-in-vocabulary or :missing
```

- **Write path** — `validate-value-constraints` signals
  `value-constraint-violation` (a `graph-db` error, sibling to
  `unique-constraint-violation` at `unique-constraint.lisp:45`) carrying the
  first record.
- **Audit path** — `check-value-constraints (graph)` walks live nodes and
  *collects* records without signalling.

The audit path is not speculative tooling. The probe proves invalid values are
writable today, so any existing store may already hold them, and a
guard that only protects future writes would leave that undetectable.

### Reporting

This is the "reports better" half of the acceptance bar, and the reason
`:one-of` is an enumeration rather than `:satisfies <predicate>`:

```
value-constraint-violation: CT-CLAIM.STANDING on node <id>:
  expected one of :OBSERVED :INFERRED :ASSERTED :SEARCHED-EMPTY
                  :DETERMINED-EMPTY :UNCOVERED :INDETERMINATE
  got :NONSENSE
```

A predicate could only report that `standingp` returned `NIL`.

### Undeclared / unbuilt

There is nothing to build, so `def-unique`'s "unbuilt" case does not arise. The
analogous hazard is a graph whose constraints were never declared in this image.
Following GH #129's rule — consult-only, never conjure, and **loud** — a graph
opened with no value-constraint declarations is not silently treated as
constraint-free where one was expected; `check-value-constraints` reports the
population it checked, never a bare `OK` over zero specs.

## What unit 1 does NOT do

Stated so the boundary is not rediscovered as a surprise:

- **Other claim slots.** `(setf claim-extent)`, `claim-producer`, `claim-relation`
  and the rest are equally unguarded on the update path. Whether extent
  well-formedness becomes declarative is a later unit's question.
- **`check-standing` is not removed.** It stays at construction as a fast-fail
  with a better error site. It stops being the *only* thing standing there.
- **Multi-slot / cross-slot constraints.** Slot-only now; generalise when a unit
  needs it (#149 Q2).
- **Units 2–4** — cardinality beyond `:required`, domain/range, disjointness.
  Deliberately unfiled until unit 1 teaches their shape.
- **Retroactive repair.** `check-value-constraints` reports damage; it does not
  fix it.

## Testing

TDD, RED observed before each implementation step. This is called out because
#102 in this repo was implemented test-and-code-together, never observed RED,
and had to establish non-vacuity by ablation afterwards. Not repeating that.

The suite must exercise, at minimum:

1. **The probe, as a regression test.** `copy` + `setf` + `save` of an invalid
   standing must now be REJECTED. This test must fail before the change — it is
   the reason the unit exists.
2. **Durability of the guard**, not just the in-session result: create, mutate,
   commit-rejected, close, reopen, assert the stored value is still valid. The
   node cache has made two earlier tests in this programme vacuous by serving
   the right answer from memory.
3. **`:required`** — `(setf slot nil)` + `save` rejected when `:required t`,
   accepted when absent.
4. **Null exemption** — without `:required`, a `NIL` slot commits.
5. **Inheritance** — a constraint declared on the parent rejects a bad write to
   *both* `-unary` and `-binary`.
6. **The constraint does not reject valid writes.** A guard bought by refusing
   everything is not a guard; every member of `+standings+` must commit.
7. **`undef-value-constraint`** withdraws enforcement; re-declaring restores it;
   re-evaluating a declaration does not grow the registry (#139).
8. **Audit path** finds a pre-existing invalid value written before the
   constraint was declared, and reports the population it checked.
9. **The suite is entered.** `(in-suite ...)` present — a missing one made
   `schema-retraction-tests.lisp` pass by name while never running, caught only
   because the check count did not move.

## Files

| file | change |
|---|---|
| `value-constraint.lisp` | new — macro, registry, evaluator, validator, audit |
| `package.lisp` | export `def-value-constraint`, `undef-value-constraint`, `check-value-constraints`, `value-constraint-violation` and its readers |
| `transactions.lisp` | one call in `%COMMIT`, beside `validate-unique-constraints` (line 3053) |
| `tests/value-constraint-tests.lisp` | new |
| `tests/package.lisp` | explicit `:import-from` for the new symbols — the test package does not `:use`, which has cost this programme a misdiagnosed "unbound" once already |
| `graph-db.asd` | add both components — one file holds the main and test systems |
| `docs/value-constraint-design.md` | reader-facing doc; docs travel with the code |

## Open questions

None blocking. The null decision (option C) was the last one and is settled
above.
