# Epoch axis (#347): rulings taken while executing the plan

Plan: `docs/superpowers/plans/2026-09-05-epoch-axis.md`. Recon:
`docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md`.
Branch `feat/epoch-axis`, 3a8ca96..2528d1a; `graph-db/spacetime-test`
709 checks, 0 failures (653 before the branch).

Each ruling names what it costs if wrong.

1. **No `declare` inside a `with-clocked-stores` body.** The fixture
   splices the body after its own `is` form, so `(declare (ignorable b))`
   is a compile error there; removed from the plan's tests. `b` is
   referenced by the fixture's assertion, so no warning. Cost: none.
2. **`check-type as-of-epoch` on both readers**, beside the existing
   check-types (a Task 2 reviewer minor, folded into Task 3). Widened to
   `(or null unsigned-byte)` in the final wave. Cost: one line each.
3. **The manual edit removes two physical lines, not one.** The replaced
   sentence straddled an Org wrap; the plan's self-check was wrong, the
   edit is right. Cost: none.
4. **One fix wave for the final review's three Important findings and
   the cheap minors**; three minors parked: no initforms on
   `epoch-axis-unavailable`'s slots (same shape as the sibling
   `query-param-error`), the rules subsystem swallowing
   `query-precondition-error` (inert until a rules reader takes the
   axis; a docstring clause records it), and untested `(plusp e)` /
   NIL-history branches (cheap coverage, not a defect). Cost: a later
   small commit if any parked item bites.
5. **Deferred minors kept as they are**: the two readers repeat a
   four-line precondition block (mirrors the pre-existing `if as-of`
   duplication; a helper if a third reader appears); the exclusivity
   test asserts `simple-error`, which already discriminates from the
   refusal's `query-precondition-error`; the clockless-reader test is
   positive-shaped with a control.

## Corrections to the issue's assumptions, as delivered

- Part 1 needs no persisted field: `commit-epoch` is already stamped on
  every version; the work is the export.
- `:as-of-epoch E` compares `<=` against each version's commit epoch;
  the engine's `resolve-version-at-epoch` is a strict snapshot-start
  predicate and is not used (it also needs `*graph*` bound and takes no
  read pin; the resolver walks `vertex-history`).
- Reaped versus created-after-E is told by the oldest retained
  version's `revision` (0 is the create), sharper than the wall-clock
  rule.
- `:as-of-epoch` bypasses the open-transaction overlay: an uncommitted
  write has no epoch. `claim-commit-epoch` answers NIL for one.
- A claim created with an already-closed transaction period (a
  replicated or restored belief retracted upstream) reads as retracted
  at every epoch; documented and pinned.
- Part 3 (`split-claim-identity-key`) is a no-op.
- Cost note for consumers: each candidate walks its whole retained chain
  through `vertex-history`, as `:as-of` already does.
