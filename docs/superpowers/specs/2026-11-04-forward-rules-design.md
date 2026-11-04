# Forward evaluation of rules: derive and fire at commit

**Status:** design, awaiting Kevin's approval (2026-11-04); a builder
check reads it first.
**Prior specs:** `2026-09-04-rules-as-producers-design.md` (the rule
record, `run-rule`, the derivation family),
`2026-09-06-recursive-rules-fixpoint-design.md` (strata, semi-naive
rounds, `rule-delta/2`). This design adds a second way to run the same
rules; it changes neither the rule text nor what a derivation records.

## 0. Problem

`graph-db/rules` derives claims from claims, with provenance, to a
fixpoint, but only when a caller runs `run-rules`. Every consumer that
needs a derivation to track its premises therefore recomputes it by
hand: standing queries re-run in full on every commit and are diffed
against their last result; findings are opened, updated, closed and
reopened by comparing a fresh evaluation with the stored one; time
thresholds are polled. Each of those is a truth-maintenance system
written once per consumer, and each has had the retraction case wrong
at least once.

The design: a committed transaction's claim writes become a **delta**,
the delta is pushed through the bodies of the rules that read those
relations, derived claims are reconciled as `run-rule` already does, and
rules whose right-hand side is Lisp fire once per new match and once
more when a match is lost. Three decisions were taken in discussion on
2026-11-04 and are not reopened here (§1).

## 1. Decisions already taken

| # | Decision | Alternative held in reserve |
|---|----------|-----------------------------|
| D1 | Derived claims are written in a **follow-on transaction** with the rule as producer, started as soon as the committing transaction has returned. Readers may see premises without conclusions for that window. | Per-stratum `:atomic`, derived inside the committing transaction. Not in this unit; the hook point (§6) is chosen so it can be added without moving anything. |
| D2 | **Two negations.** `not/1` stays negation as failure. A new goal, `claim-absent/7`, succeeds only on a current claim whose standing is a determined absence (`:searched-empty`, `:determined-empty`); `:uncovered`, `:indeterminate` and no claim at all answer nothing. | The well-founded three-valued model. Documented as a later option; no rule written against D2 would change. |
| D3 | **Self-scheduling thresholds.** `now/1` binds the evaluation instant; a comparison against it compiles to one agenda timer per match at the instant the comparison flips. A comparison the compiler cannot bound is a compile-time refusal. | Tick facts. Rejected: wholesale re-evaluation per tick, resolution equal to the tick, churn in the store. |

## 2. Goals and non-goals

Goals:

- A rule's derived claims track their premises across commits without
  the caller running anything: a new premise derives, a retracted
  premise retracts, in the order the commits happened.
- A production fires exactly once per instantiation, and once more when
  that instantiation is lost; the record of firing is durable and
  readable back.
- Nothing runs under the transaction-manager lock, and nothing a rule
  does can fail the commit that triggered it: the commit is durable
  before the forward step starts.
- `run-rules` stays correct and idempotent as the repair tool: a store
  whose forward step was interrupted is brought back to the fixpoint by
  one `run-rules`.

Non-goals (named so a builder does not build them):

- Beta memories kept between commits. Joins re-run against the store's
  indexes; materializing a partial join for a hot rule is a later,
  per-rule option.
- A general Z-set operator algebra. Positive deltas derive through the
  existing semi-naive variants; negative deltas retract through the
  derivation family's premise references (§4.2). That is justification
  based retraction, and it is what the consumers asked for.
- Multi-store scope for the forward step; `run-rule`'s snapshot scope
  (S3) is unchanged and remains the way to read another store.
- Recency or specificity heuristics in conflict resolution.
- CHR-style simpagation syntax; geometric functors; anything from the
  conceptual-spaces discussion.
- Peers and slaves. The forward step runs where the commit happened; a
  replica applying the feed does not run rules.

## 3. The delta

A graph opts in with `(enable-forward-rules graph)`; `run-rules` on a
graph that has not opted in is unchanged, and the forward step costs an
opted-out graph nothing.

After a transaction on an opted-in graph commits, its writes
(`tx-create`, `tx-update`, `tx-delete`; `transactions.lisp`) are
scanned for nodes of a claim class. The delta is a table, relation
string to a list of `(claim . weight)`:

| write | weight |
|-------|--------|
| `tx-create` of a claim | +1 |
| `tx-update` whose old node is current and whose new node is retracted | −1 |
| `tx-update` of any other slot of a claim | −1 for the old node, +1 for the new |
| `tx-delete` of a claim | −1 |

A rule is **touched** by a delta when a delta relation is in its
`reads`, its `negative-reads`, or the relations its `claim-absent/7`
goals read (§5.1). The writes of the follow-on transaction itself are a
delta too; the forward step loops until a delta touches no rule (§4.3).

## 4. The Datalog step

A rule whose head is a claim is a Datalog rule, as today. Touched rules
run by stratum, in the strata order `run-rules` already computes.

### 4.1 Positive deltas

The stratum's rules run exactly as a fixpoint round runs today
(`%run-stratum`, R9 and R10 of the fixpoint spec), with one change:
round 0's `*rule-delta*` is seeded with the commit delta's +1 claims
instead of the stratum's base facts. Body variants substitute
`rule-delta/2` for one recursive or non-recursive goal in turn; the
existing machinery handles a non-recursive rule's variants the same way
(every body `claim/7` goal is a substitution site, not only the
recursive ones: **this generalisation is the builder's main engine
change**). Derived claims are constructed, deduplicated and
provenance-recorded by `%desired` and `%reconcile-claims` unchanged.

### 4.2 Negative deltas

A −1 claim may invalidate derivations that cite it and may enable
derivations a `not` was blocking. The two are handled differently:

- **Cited premise retracted.** The derivation family records every
  derived claim's premise references (`%premise-ref`). For each −1
  claim, the derived claims whose provenance cites it are re-derived
  with their head key bound; those no longer derivable are retracted
  through the reconcile's sweep, which already writes the retraction
  and rewrites provenance. This is the justification step, and it is
  recursive through strata: a retracted derived claim is a −1 delta for
  the next stratum.
- **Negated premise retracted.** A −1 claim in a rule's
  `negative-reads` means a `not` that was false may now be true. There
  is no delta form of a negated goal, so the rule runs in full, with
  the reconcile. **Ruling:** full re-run for negation-touched rules in
  this unit; the cost is one `run-rule` per such commit and is bounded
  by the existing `*rules-max-inferences*` and `*rules-timeout*`.

### 4.3 The loop and its bounds

The step iterates: derive, collect the follow-on transaction's writes as
the next delta, until no rule is touched or `*rules-max-rounds*` is
reached, which signals `rule-run-refusal` as a fixpoint does today. Each
iteration is one follow-on transaction. A rule that is refused at
compile is skipped and reported, as `run-rules` reports it.

### 4.4 Failure

The commit that triggered the step is durable before it starts. An error
inside the step is caught at the step's boundary, logged, and recorded
as a `rule-report` with `:refused`/`:failed` status on the graph's
forward-rules state (readable by `forward-rules-status`); the store may
then be behind the fixpoint until the next commit touches the same rules
or a caller runs `run-rules`, which repairs it. A step failure never
unwinds the caller's `with-transaction`.

## 5. Two functors

### 5.1 `claim-absent/7`

`(claim-absent ?c family ?sns ?skey ?rel ?ons ?okey)`: `claim/7`'s
arguments and routes, restricted to current claims whose standing is
`:searched-empty` or `:determined-empty`. It is a positive read: for
stratification it is in `reads`, not `negative-reads`, and a −1 delta
on its relation goes through §4.2's first case. Documented beside
`not/1` in `docs/rules.md` as the negation to use when "we checked and
there was nothing" must not be confused with "we never looked".

### 5.2 `now/1` and thresholds

`(now ?t)` binds `?t` to the step's evaluation instant (one instant per
step, taken when the step starts). It is a global functor withheld from
free text, like `rule-delta/2`.

A body may compare `now` only in the form
`(instant<op> ?x ?t)` or `(instant<op> ?t ?x)` where `?t` is bound by
`now/1` and `?x` by one of `claim-valid-from/2`, `claim-valid-to/2`,
`claim-recorded-at/2`, `claim-touched-at/2`, `claim-retracted-at/2`,
or by the new `instant-offset/3` `(instant-offset ?x duration ?y)`
over one of those (a duration is an ISO-8601 duration string). The
compiler records, per such goal, how to compute the **flip instant** of
a match from its bound claims. Any other use of `?t` is a
`rule-compile-error` naming the goal.

When a threshold rule derives (or fails to derive) for a match, the step
schedules a timer for that match at its flip instant. When the timer
fires, the rule runs for that match alone: it is a positive delta
consisting of the match's premise claims. Timers are in memory; on
`enable-forward-rules` (and so on open) every threshold rule runs once
in full, which both repairs the store and rebuilds the timers. A derived
claim whose body has a threshold gets validity from the flip instant
under the `:premises` extent policy.

## 6. Where the step runs

`with-transaction`'s exit, after `%commit` has returned and the
transaction's file is cleaned up, calls `forward-step` for the
transaction's graph when it is opted in and the transaction wrote a
claim. The call is on the committing thread, outside every lock, after
`cleanup-transaction`. **Ruling:** the committing thread, not a worker,
so a caller's next read on the same thread sees the derivations; D1's
window is the duration of the step. The step opens its own
`with-transaction`; re-entry is prevented by a per-graph flag, and a
commit made from inside a production or a Datalog body is an error.

The `:atomic` option held in reserve under D1 would hook before
`%commit` instead, with the same step function; nothing here precludes
it.

## 7. Productions

`(def-production name &key version family body salience writes action)`
in the image, and a stored `production` record beside `rule` with the
same slots (`def-rules-schema` grows it). `body` is guarded Prolog text
as a rule's is; `action` is a function of two arguments, an event, one
of `:added` or `:lost`, and the match (a plist of the body's variables
with their bindings, the premise claims among them). `writes` is the
list of relations the action may write, used only for stratification: a
production reading a relation it writes is a compile-time refusal, and
a production is ordered after the Datalog strata that derive what it
reads.

**Instantiation and refraction.** An instantiation is the production's
name and version plus the sorted ids of its premise claims. The
forward step records each firing as a claim of a new unary family
`firing` (relation = production name, subject key = a digest of the
premise ids, producer = the production), so refraction is durable,
auditable, and survives restart without a side table; a match that is
lost retracts its `firing` claim and delivers `:lost`. The action is
called once per transition. A production body with no `claim/7` goal
has no premises and is refused.

**The agenda.** Fired instantiations are queued, not run inline: the
Datalog step finishes and commits, then the agenda runs. Ordering:
higher `salience` first (an integer, default 0), then the triggering
commit's epoch, then the instantiation key. **Ruling:** the agenda runs
on the committing thread after the Datalog step, in this unit; moving
it to a per-graph worker is a contained later change and the ordering
rule does not depend on which thread runs it. An action that signals is
caught: the `firing` claim is kept (the instantiation did fire), the
condition is logged and recorded on the forward-rules state, and the
agenda continues.

## 8. Standing queries: the first consumer

A standing query is a production whose body is the query and whose
action receives the rows that appeared and the rows that disappeared.
Where a consumer today re-runs every query on every commit and diffs
the rows, a production delivers the same additions and, unlike that
consumer, the removals, with no result cap, no self-match guard
(stratification refuses a production that reads what it writes), and no
work for commits that touch nothing the query reads.

The acceptance test runs both side by side: a set of queries over a
store receiving a scripted sequence of commits, including retractions
and a query whose results exceed any fixed cap; the productions'
`:added` and `:lost` deliveries must equal the set differences of
full re-evaluation after each commit.

## 9. Docs that travel with this

`docs/rules.md` (a new section, "Forward evaluation", and
`claim-absent/7`, `now/1`, `instant-offset/3` in the functor table);
the manual's rules chapter; `CHANGELOG.md` Added; the fixpoint spec's
R10 gets a note that forward mode seeds round 0 differently.

## 10. Acceptance

1. A rule's derived claim appears after the commit that supplies its
   last premise, without `run-rules`, and is retracted after the commit
   that retracts any cited premise; both carry provenance as today.
2. A chain through two strata derives in the same forward step.
3. A rule with a `not` over a retracted blocker derives after that
   retraction (the full re-run path).
4. `claim-absent/7` derives on `:searched-empty` and `:determined-empty`
   and derives nothing on `:uncovered`, `:indeterminate` or no claim.
5. A threshold rule derives at its flip instant, with validity from
   that instant, and a store reopened before the instant still derives
   at it.
6. A `now` comparison the compiler cannot bound is refused at compile
   with the goal named.
7. A production fires `:added` once per instantiation across repeated
   commits of the same premises, `:lost` once on retraction, and its
   `firing` claims are readable; a production reading what it writes is
   refused.
8. The standing-query equivalence test of §8 passes, removals included.
9. An error in a Datalog body or a production leaves the triggering
   commit durable and the caller's `with-transaction` returning
   normally; `forward-rules-status` reports it; `run-rules` repairs the
   store.
10. No new form inside `with-transaction-manager-lock`, and an opted-out
    graph's commit path is unchanged (reviewed).
11. The existing rules, fixpoint and scope suites stay green.

## 11. Calls I made

- Justification-based retraction through the derivation family instead
  of a weight algebra: the provenance already names the premises, and
  the consumers want "why was this retracted" more than they want the
  algebra.
- Full re-run for negation-touched rules (§4.2): correct, bounded, and
  the only place this unit pays a recompute; a delta form of `not`
  belongs with the well-founded option.
- `firing` as a claim family rather than a side table: refraction that
  can be read, swept and replicated like any derived fact.
- The committing thread for both the Datalog step and the agenda, with
  the worker as a contained later change.
- A single evaluation instant per step for `now/1`.

## 12. Left to the builder

- The exact `forward-rules-status` shape; the digest for a firing key.
- Whether threshold timers live in one image-wide wheel or per graph.
- How the match plist names premise claims (by body variable is the
  obvious choice).
- The generalisation of body variants to non-recursive goals (§4.1)
  and where in `compile.lisp` the flip-instant extractor lives.
