# Recursive rules under fixpoint iteration: design

**Issue:** kraison/vivace-graph#333 (rules S4), closing #304's last
acceptance criterion. **Parent:** #122 Phase 3, of which this is unit 1;
unit 2 (SLG tabling for `select`, magic sets) stays on #122, gated on a
`select`-side workload. **Prior spec:** `2026-09-04-rules-as-producers-design.md`
(§6 refuses recursion "until #122"; §12 defers tabling).
**Date:** 2026-09-06. **Status:** approved in review, sections 1–4.

## 0. Problem

A rule whose head relation is reachable from its own body is refused at
compile (`%check-cycle`, spec §6), so the closures the substrate exists
for cannot be written as rules: a transitive relation over a document
supersession family, containment over a region hierarchy. Top-down
evaluation of such a rule would loop on a cyclic claim graph and fail on
a Phase 1 resource bound; a reified-claim model that keeps contradictions
is cyclic-capable by design, so this is the difference between a reasoner
that refuses and one that answers.

## 1. Rulings

| # | Ruling | Why |
|---|--------|-----|
| R1 | Phase 3 splits: unit 1 is fixpoint iteration in `graph-db/rules`; unit 2 (tabling for `select`, magic sets) waits for a `select`-side workload. | `run-rule` already evaluates a body to all solutions and materialises claims; recursion is iteration over that, and the only real workload is rule-shaped. |
| R2 | Measured on two real corpora on ma-dev: a supersession-chain closure over the knowledge store (thousands of claims, cyclic-capable through curation error) and a containment closure over the spine (~300k claims, a tree). | The issue's precondition: size against real volume, not estimates. The first proves termination on cycles; the second proves cost. |
| R3 | Semi-naive iteration: later rounds evaluate only body variants bound to the previous round's new claims. Naive re-evaluation is the test reference; magic sets are unit 2's. | Rounds × full evaluation is what a 300k-claim corpus would condemn; the delta is a small internal generator. |
| R4 | Stratified negation is a compile-time refusal; `:any` reads stay refused. | An unstratified fixpoint is undefined; an unbound relation puts every rule in one stratum. |
| R5 | The sweep moves from per-run to per-fixpoint. | A per-round sweep would delete claims derived earlier in the same run. |
| R6 | `run-rule` on one rule of a recursive stratum runs the stratum. | A single rule's fixpoint alone is partial; no new API. |
| R7 | A round's `claim/7` reads of the stratum's own relations exclude the stratum's producers, round 0 included; the delta's bound `?c` is never filtered. | Round 0 must be base-only and every round must recompute the derivation from scratch, or a stale closure from a previous run becomes its own premise. |
| R8 | The run's own derivation so far (kept claims and this run's constructions alike) is indexed like `claim/7`'s own routes and unioned into a plain read's candidates, and into `claim-producer/2`'s generator for an excluded producer; the delta's bound `?c` is unaffected. | A rule with two or more recursive goals must see every other goal's derivation this run, not only the one goal the delta substitutes into. |
| R9 | A recursive rule runs its body variants only, every round including round 0; a stratum member with no recursive goal runs its full body once, at round 0. | An empty round-0 delta then answers nothing at no cost, so an unanchored two-goal closure is not refused as cost-unbounded. |

## 2. Strata (compile time, `rules/compile.lisp`)

The compiler keeps building the relation dependency graph it builds
today: an edge from each rule's head relation to every relation a body
`claim/7` goal reads, over every enabled rule in scope (`rules-in-scope`,
stored rules plus `def-rule`s). Instead of refusing a cycle it computes
the strongly connected components; a **stratum** is one component.

Each `compiled-rule` gains:

- `stratum` — the sorted list of rule names in its component (a single
  name for a non-recursive rule).
- `recursive-goals` — for each body `claim/7` goal, whether the relation
  it reads is in `stratum`; the positions the fixpoint restricts.

Refused at compile, naming the rule and the path, exactly as
`rule-compile-error` does today:

- **Unstratified negation**: a `not` goal (the engine's `not/1`, the only
  negation a body may use) whose enclosed goals read a relation in the
  rule's own stratum. A `not` over a relation of an earlier stratum
  compiles.
- **`:any` reads**: a body `claim/7` goal with its relation unbound, as
  today (ruling P6).

`%check-cycle` is replaced by `%strata`; the "closes a cycle" refusal
is gone. Compile stays single-store: a cycle through another store's
rules is not detected (S3-P5, unchanged).

`%dependency-order` orders **strata**: a stratum is ready when no
pending stratum derives a relation it reads (a stratum's reads exclude
its own relations); ties keep input order, and rules inside a stratum
keep input order. A store with no recursive rule orders exactly as
today, one rule per stratum.

## 3. The fixpoint loop (run time, `rules/run.lisp`)

`run-rules` runs strata in order. A one-rule, non-recursive stratum is
today's `run-rule`: evaluate, reconcile, one transaction. A recursive
stratum runs through one `%run-stratum` call for every rule of the
stratum together, in **rounds**:

- **Every round, round 0 included, a rule with a recursive `claim/7`
  goal runs its `%variants` only** (R9): for each such goal, a body
  variant where that one goal is answered from the fixpoint's internal
  delta generator and every other goal is unchanged. Round 0's delta
  is empty, so a variant answers nothing there at no cost -- the
  reason an unanchored two-goal closure such as `t(x, z) :- t(x, y),
  t(y, z)` is not refused as cost-unbounded: neither goal ever reaches
  the unindexed scan. A stratum member with **no** recursive goal --
  sharing the stratum only because it derives one of its relations --
  runs its full body once, at round 0, and not again.
- **A round's evaluation excludes the stratum's own producers from
  every plain `claim/7` read of a stratum relation, round 0 included**
  (R7): round 0 is base facts only, and a later round's plain reads
  still cannot answer from what the stratum wrote on a previous run.
  The fixpoint therefore recomputes the whole derivation from scratch
  each time `run-rules` runs it; a stale closure left standing from
  before is a premise for nothing.
- **What the exclusion removes, the run's own derivation restores**
  (R8): an index of this run's derivation so far -- claims kept from
  before and newly constructed alike -- built the same way `claim/7`'s
  own subject and object routes are, unioned into a plain read's
  candidates and into `claim-producer/2`'s generator for an excluded
  producer. Only the delta's bound `?c` is exempt from both R7 and R8.
  Without R8 a rule with two or more recursive goals would see the
  delta only on whichever goal a variant substitutes and nothing on
  the others, since the delta generator answers for one goal position
  per variant -- an incomplete fixpoint.
- **A round's delta** is every identity first derived this run,
  whether constructed just now or already standing from before (kept,
  its node reused); it is written as it is found, per round, so the
  next round's delta and derived-this-run reads see it. The stratum
  stops at a round that derives nothing new. Identities are finite
  over a finite store and every round either adds one or stops, so the
  loop terminates; a **rounds cap** (`*rules-max-rounds*`, default
  1000) guards a defect in the delta logic, and reaching it is a
  refusal tagged `:rounds` naming the count.

The delta generator is an internal functor bound dynamically by the
loop (like `*claim-scope*`), never registered where the guard could
admit it from free text, so a rule body cannot name it. Its solutions
carry the same `(identity-key . store-name)` premise shape `%desired`
records today.

**Reconcile at the fixpoint** (R5): the stratum's desired set is the
union over rounds, per rule; after the last round, one pass per rule
sweeps the producer's claims outside that set and rewrites provenance
for the kept and new ones. Round writes are what let later rounds read
earlier rounds' own derivations, since `claim/7` reads committed
claims -- subject to the R7 exclusion above.

**Budget and refusals, and the transaction shape.** Each round's
evaluation is a `select` under the run's resource bounds. A
**single-store** stratum runs the whole fixpoint -- every round and
the final reconcile -- in **one transaction**: a budget refusal, a
commit constraint, or the rounds cap leaves the previous derivation
standing, exactly as a refused rule does today, with `derived`,
`kept` and `swept` all reading 0. A **cross-store** stratum keeps the
S3 shape at round granularity instead of run granularity: each
round's evaluation runs under `%under-snapshots`, and that round's
write commits in its own transaction on the own store, since a
foreign read inside a transaction is refused (GH #53) and a later
round must see what the earlier ones committed. A refusal partway
through a cross-store stratum leaves standing whatever rounds
committed before it: `derived` counts what they wrote and `rounds`
names how many, while `kept` and `swept` still read 0, since the
fixpoint's reconcile -- the only place either is set -- is itself
inside the transaction that just unwound. The not-serialised-against-
a-later-premise trap is per round now, and documented as such.

## 4. Reports, schema, surface

- `rule-report` gains `rounds` (1 for a non-recursive rule) and
  `stratum` (the rule names it ran with). `derived`/`kept`/`swept` are
  totals over the run.
- A refusal in a stratum is reported on the rule it happened in; the
  stratum's other rules report `:refused` with a text naming that rule.
- **No schema change.** Recursion is derived by the compiler, never
  declared. The visible change: a rule reading its own head relation
  compiles.
- `run-rule` on one rule of a recursive stratum runs the stratum and
  returns that rule's report (R6).
- `select` and the guarded surface are unchanged; a recursive `<-`
  predicate there still hits the resource bound (unit 2).
- Docs: `docs/rules.md` "Recursive rules" section; the S1–S3 spec's §6
  and §12 amended to point here; `CHANGELOG.md`.

## 5. Testing

`tests/rules/` (the rules suite, in CI's full run):

- Strata: two mutually recursive rules in one stratum; a self-reading
  rule a one-rule stratum; a non-recursive store orders as today.
- Refusals: `not` over the own stratum refused naming the path; `not`
  over an earlier stratum compiles; `:any` still refused.
- Fixpoint: transitive closure of a chain relation over a small cyclic
  claim graph terminates with the exact closure; a naive full
  re-evaluation per round in the test derives the same identity set
  (the delta's reference).
- Rounds and reconcile: `rounds` = chain length + 1; a premise retracted
  between runs sweeps exactly the stale derived claims; a round-0 claim
  survives to the end.
- Entry points: `run-rule` on one rule runs the stratum; cap 1 on a
  two-round closure refuses naming the count; a budget refusal in
  round 2 leaves the previous derivation standing.
- Cross-store: a closure whose base relation is in a foreign store
  reaches the fixpoint under snapshots.

## 6. Measurement (acceptance)

On ma-dev (odm), engine at this unit's merge, third of three runs
recorded: (a) a closure over the knowledge store's document-supersession
family — rounds, claims derived, elapsed, and whether a cycle was
present; (b) a containment closure over the spine's ~300k claims —
the same numbers. Rule text and full numbers live in the private
tenant's runbook; this spec's closing comment on #333 carries the
domain-neutral summary. A delta round costing more than a naive round
is a recorded finding, not a silent design change.

## 7. Out of scope

SLG tabling for `select`/guarded queries and magic sets (#122, unit 2);
the trampoline question (reopens with unit 2); incremental
re-derivation on premise commit (kraison/blackboard#5); cross-store
cycle detection (S3-P5).
