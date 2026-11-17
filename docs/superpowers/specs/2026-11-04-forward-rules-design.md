# Forward evaluation of rules: derive and fire at commit

**Status:** approved 2026-11-04; implemented on branch
`forward-rules` 2026-11-05 (#456; plan
`docs/superpowers/plans/2026-11-04-forward-rules.md`, whose ledger
records the rulings taken while building). Builder check done; its
findings are folded in, and the two questions it left for Kevin are
decided (D4, D5 in §1).
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
relations, derived claims are reconciled against what the step touched,
and rules whose right-hand side is Lisp fire once per new match and once
more when a match is lost. Three decisions were taken in discussion on
2026-11-04 and are not reopened here (§1).

## 1. Decisions already taken

| # | Decision | Alternative held in reserve |
|---|----------|-----------------------------|
| D1 | Derived claims are written in a **follow-on transaction** with the rule as producer, started as soon as the committing transaction has returned. Readers may see premises without conclusions for that window. | Per-stratum `:atomic`, derived inside the committing transaction. Not in this unit; the hook point (§6) is chosen so it can be added without moving anything. |
| D2 | **Two negations.** `not/1` stays negation as failure. A new goal, `claim-absent/7`, succeeds only on a current claim whose standing is a determined absence (`:searched-empty`, `:determined-empty`); `:uncovered`, `:indeterminate` and no claim at all answer nothing. | The well-founded three-valued model. Documented as a later option; no rule written against D2 would change. |
| D3 | **Self-scheduling thresholds.** `now/1` binds the evaluation instant; a comparison against it compiles to one agenda timer per match at the instant the comparison flips. A comparison the compiler cannot bound is a compile-time refusal. | Tick facts. Rejected: wholesale re-evaluation per tick, resolution equal to the tick, churn in the store. |
| D4 | **A lost derivation is retracted, not deleted** (§4.2): the sweep becomes `retract-claim`, in forward mode and in `run-rules` alike, with a re-open path when the identity is derived again. | `mark-deleted`, today's sweep. Rejected: the store could not say why a derivation is gone. |
| D5 | **Concurrent steps queue** (§6): a per-graph delta queue, the first thread in is the runner and drains it, later entrants enqueue and return. | A per-graph step lock. Rejected: commits would wait on another thread's step. |

## 2. Goals and non-goals

Goals:

- A rule's derived claims track their premises across commits without
  the caller running anything: a new premise derives, a lost premise
  retracts, in the order the commits happened. "Lost" is defined by the
  body (§4.2): forward mode and `run-rules` evaluate the same body the
  same way, so the two modes always agree on the fixpoint.
- A production fires exactly once per instantiation, and once more when
  that instantiation is lost; the record of firing is durable and
  readable back.
- Nothing this design adds runs under the transaction-manager lock, and
  nothing a rule does can fail the commit that triggered it: the commit
  is durable before the forward step starts.
- `run-rules` stays correct and idempotent as the repair tool: a store
  whose forward step was interrupted is brought back to the fixpoint by
  one `run-rules`, and `run-rules` never undoes a forward result.

Non-goals (named so a builder does not build them):

- Beta memories kept between commits. Joins re-run against the store's
  indexes; materializing a partial join for a hot rule is a later,
  per-rule option.
- A general Z-set operator algebra, DRed, or support counting. Positive
  deltas derive through semi-naive variants; lost derivations are found
  through the derivation family's premise references, and the cases
  that technique cannot settle fall back to a full re-run (§4.2).
- Multi-store scope for the forward step; `run-rule`'s snapshot scope
  (S3) is unchanged and remains the way to read another store.
- Recency or specificity heuristics in conflict resolution.
- CHR-style simpagation syntax, geometric functors, or any new body
  vocabulary beyond the three functors in §5.
- Peers and slaves. The forward step runs where the commit happened; a
  replica applying the feed does not run rules.
- A core open hook. Core does not know `graph-db/rules` exists and this
  unit keeps it so (§5.3).

## 3. The delta

A graph opts in with `(enable-forward-rules graph)` (§5.3); `run-rules`
on a graph that has not opted in is unchanged, and the forward step
costs an opted-out graph nothing.

After a transaction on an opted-in graph commits, its writes
(`tx-create`, `tx-update` with `old-node`, `tx-delete`;
`transactions.lisp`) are scanned for nodes of a claim class. The delta
is a table, relation string to a list of `(claim . weight)`:

| write | weight |
|-------|--------|
| `tx-create` of a claim | +1 |
| `tx-update` whose old node is current and whose new node is retracted (`retract-claim`'s shape) | −1 |
| `tx-update` of any other slot of a claim | −1 for the old node, +1 for the new |
| `tx-delete` of a claim | −1 |

A rule is **touched** by a delta when a delta relation, at either
weight, is in its `reads`, its `negative-reads`, or the relations its
`claim-absent/7` goals read. The writes of the step's own follow-on
transactions, and of any transaction a production commits (§7), are
deltas too; the step loops until a delta touches no rule (§4.3).

Compiled rules are cached per graph and invalidated by a write to the
`rule` or `production` record and by `def-rule`, `undef-rule` and
`def-production`; compiling every rule on every commit is not
acceptable (builder's finding; the cache shape is the builder's).

## 4. The Datalog step

A rule whose head is a claim is a Datalog rule, as today. Touched rules
run by stratum, in the strata order `run-rules` already computes.
Productions are nodes of the same stratification graph (§7).

### 4.1 Positive deltas: a stratum run with three differences

The stratum's rules run through the fixpoint machinery (`%run-stratum`)
with three changes from a `run-rules` round, each the inverse of a
fixpoint-spec ruling that assumed a from-scratch run:

1. **Seed.** Round 0's `*rule-delta*` is the commit delta's +1 claims,
   not `%stratum-base-facts` (R10).
2. **Exclusion off.** `*claim-exclude-producers*` is not bound (R7): the
   stratum's standing derivation from earlier commits is a valid
   premise now, since the step extends it rather than recomputing it.
3. **Partial reconcile.** `%reconcile-claims` and
   `%reconcile-provenance` today sweep every claim of the rule's
   producer whose key is not in `desired`; in forward mode `desired`
   holds only what this step derived, so that sweep would delete the
   rule's whole standing derivation on every commit. The step's
   reconcile is an upsert of `desired` plus a sweep restricted to the
   head identities §4.2 found unsupported. The step's `rule-report`
   counts (`derived`, `kept`, `swept`) are per step. The reads behind
   that reconcile are per identity, not per producer, since #457
   (`2026-11-05-forward-step-reads-design.md`).

Body variants substitute `rule-delta/2` for one `claim/7` goal in turn.
Today `%variants` substitutes only goals that read a stratum relation
(recursive goals); forward mode substitutes **every** body `claim/7`
goal of every touched rule, recursive or not. **This generalisation is
the builder's main change to `run.lisp`.**

### 4.2 Lost derivations

A −1 claim may make a derivation unsupported, and a ±1 claim under a
`not` may make one unsupported or newly supported. Three cases, by cost:

| case | method |
|------|--------|
| −1 claim cited as a premise by a derivation of a **non-recursive** rule | Re-derive the citing derived claims with their head identity bound (the derivation family's premise references name them, `%premise-ref`); those no longer derivable are swept. Recursive through strata: a swept derived claim is a −1 delta for the strata that read it. |
| −1 claim cited by a derivation of a **recursive** stratum | Full re-run of that stratum through `run-rules`'s own path. A bound re-derivation could find support in another derived claim that is itself supported only through the lost one (an unfounded cycle); DRed and support counting are non-goals, so the stratum pays the recompute. |
| ±1 claim in a rule's `negative-reads` | Full re-run of that rule. There is no delta form of a negated goal in either direction (a +1 under a `not` must retract; a −1 under it may derive), and provenance does not cite negated claims. |

**Currency is the body's business, in both modes.** `claim/7`
generates retracted claims by design (`docs/rules.md`, "Retracted
claims are generated"), and `claim-current/1` is how a body says "still
believed". A retracted premise therefore makes a derivation unsupported
only when the body filters that premise with `claim-current/1`; the −1
delta still triggers the re-derivation, which then succeeds and sweeps
nothing. Forward mode does not add implicit currency: doing so would
make `run-rules` and the forward step disagree, and `run-rules` would
then undo forward results. `docs/rules.md` tells rule authors in so
many words that a rule meant to track retraction says `claim-current`
on each premise it tracks.

**The sweep retracts (D4).** Today the reconcile's sweep is
`mark-deleted`: the derived claim and its `derived-from` records vanish,
and "why is this no longer derived" has no answer in the store. The
sweep becomes `retract-claim` on the derived claim, keeping it and its
provenance with a transaction-period end; the `derived-from` records
are retracted with it, not deleted. This applies to `run-rules` as
well, so the two modes agree. It needs a re-open path: today "a
retracted derived claim whose identity is derived again stays
retracted" (`docs/rules.md`), so when a swept identity is in `desired`
again the reconcile re-opens it (as `register-node` re-opens a
registration) rather than constructing a second claim, and the
`docs/rules.md` sentence changes to say so. Downstream bodies that must
not read a retracted conclusion say `claim-current`, as for any claim.

### 4.3 The loop and its bounds

One step: drain the pending deltas, run the touched Datalog strata in
order, commit the follow-on transaction, run the agenda (§7), collect
the writes of the follow-on and of anything the agenda committed as the
next delta, repeat until a delta touches no rule or
`*rules-max-rounds*` is reached, which signals `rule-run-refusal` as a
fixpoint does today. Each iteration is one follow-on transaction plus
the agenda's own. A rule that is refused at compile is skipped and
reported, as `run-rules` reports it.

### 4.4 Failure

The commit that triggered the step is durable before it starts. An error
inside the step is caught at the step's boundary, logged, and recorded
with the failing rule's `rule-report` on the graph's forward-rules state
(readable by `forward-rules-status`); the store may then be behind the
fixpoint until the next commit touches the same rules or a caller runs
`run-rules`, which repairs it. A step failure never unwinds the caller's
`with-transaction`.

## 5. Two functors and the opt-in

### 5.1 `claim-absent/7`

`(claim-absent ?c family ?sns ?skey ?rel ?ons ?okey)`: `claim/7`'s
arguments and routes, restricted to current claims whose standing is
`:searched-empty` or `:determined-empty`. It is a positive read: for
stratification it is in `reads`, not `negative-reads`, and a −1 delta
on its relation goes through §4.2's first row. Documented beside
`not/1` in `docs/rules.md` as the negation to use when "we checked and
there was nothing" must not be confused with "we never looked".

### 5.2 `now/1` and thresholds

`(now ?t)` binds `?t` to the evaluation instant: one instant per forward
step, and one instant per `run-rules` call, taken when each starts. It
is a global functor withheld from free text, like `rule-delta/2`.

A body may compare `now` only in the form `(instant<op> ?x ?t)` or
`(instant<op> ?t ?x)` where `?t` is bound by `now/1` and `?x` by one of
`claim-valid-from/2`, `claim-valid-to/2`, `claim-recorded-at/2`,
`claim-touched-at/2`, `claim-retracted-at/2`, or by the new
`instant-offset/3` `(instant-offset ?x duration ?y)` over one of those
(a duration is an ISO-8601 duration string). The compiler records, per
such goal, how to compute the match's **flip instant** from its bound
claims, and which **direction** the comparison flips: a *gain* (the
goal is false before the instant and true from it, as `instant>= ?t ?x`)
or a *loss* (true before, false from, as `instant< ?t ?x`). Any other
use of `?t` is a `rule-compile-error` naming the goal.

The flip instant is a function of the premises, never of the evaluation
instant, so both modes compute it identically. A derived claim whose
body has a gain threshold takes its validity start from the flip
instant under the `:premises` extent policy; one whose body has a loss
threshold is retracted at the flip instant, with that instant as its
validity end.

Timers: when a step or a `run-rules` evaluates a threshold goal for a
match, it schedules a timer for that match at its flip instant if the
instant is in the future. When the timer fires it enqueues a positive
delta consisting of the match's premise claims (§6), so the rule runs
for that match alone. Timers are in memory; `enable-forward-rules` runs
every threshold rule once in full, which both repairs the store and
rebuilds the timers.

### 5.3 The opt-in is a rules-layer call

`enable-forward-rules` is called by the application after
`open-graph`, as `def-rules-schema` and `run-rules` are; core has no
open hook and this unit adds none. It installs the graph in the rules
layer's opted-in table, compiles and caches the rules, runs the
threshold rules once (§5.2), and returns. `disable-forward-rules`
removes the graph and drops its timers and pending deltas. Acceptance 5
reopens a store and calls `enable-forward-rules` again.

## 6. Where the step runs, and how steps meet

**Hook point.** Not inside `call-transaction-fun`: on the
retry-exhausted path that whole function runs under
`with-transaction-manager-lock`, and `cleanup-transaction` runs inside
it. The hook is in `call-with-transaction` after the retry loop has
returned and outside the `let` that binds `*transaction*`, so the step's
own `with-transaction` is not nested in the committed one's dynamic
extent. The committed transaction is captured out of the thunk; its
`writes` remain readable after commit. The step runs when the graph is
opted in and the writes include a claim.

**Nesting.** `create-transaction` never joins an outer transaction, so a
`with-transaction` nested in another on the same thread is its own
commit. A step triggered while this thread has another transaction
open is not run then: its delta is enqueued and the step runs when the
thread's outermost `with-transaction` returns.

**The step's own transactions** can exhaust their attempts and take the
engine's existing retry-exhausted fallback under the manager lock,
exactly as `run-rules` can today. That is the engine's existing
behaviour, not a form this design adds; acceptance 10 is read that way.

**Concurrent steps queue (D5).** Two threads committing claim writes
on one graph, a timer firing, and a nested commit deferred to its
outermost are all entrants to one per-graph delta queue. The thread
that finds no step running becomes the runner and drains the queue
until it is empty, re-checking under the queue's lock before it leaves
so a delta enqueued during its last iteration is not stranded; a thread
that finds a step running enqueues and returns at once. No entrant ever
drops a delta, and no commit waits on another thread's step. The cost
is read-after-derive: a caller whose delta another thread ran is not
guaranteed to see its derivations on its next read; a caller that needs
that guarantee calls `run-rules` on its own thread, which stays
idempotent. `forward-rules-status` reports the queue depth and the
running step. The alternatives, skip (loses deltas) and a per-graph
step lock (commits wait), are rejected.

The `:atomic` option held in reserve under D1 would hook before
`%commit` instead, with the same step function; nothing here precludes
it.

## 7. Productions

`(def-production name &key version family body salience writes action)`
in the image, and a stored `production` record beside `rule`
(`def-rules-schema` grows it) with slots `name`, `version`, `family`,
`body`, `salience`, `writes` and `action`, where the stored `action` is
a **name** looked up in a registry only `def-production` populates. A
`production` write whose `action` names nothing registered is refused
at commit (the sibling of `%validate-rule-writes`), so a record written
over REST cannot name arbitrary image code; the guard model of
`query/guard.lisp` is preserved. `body` is guarded Prolog text as a
rule's is and must contain at least one `claim/7` goal (a body with no
premises is refused). `action` is a function of two arguments: an
event, `:added` or `:lost`, and the match, a plist of the body's
variables with their bindings, the premise claims among them.

**One stratification graph.** `writes` lists the relations the action
may write. A production is ordered after the Datalog strata that derive
what it reads, and a Datalog rule reading a relation a production
writes is ordered after it. A cycle through a production (it writes
what it reads, or what a rule reads whose head it reads) is a
compile-time refusal naming the path, as a rule cycle through `not` is.

**Actions may write.** An action may open its own `with-transaction`.
That commit's claim writes are a delta enqueued on the running step
(§4.3), never a nested step and never dropped. An action that writes a
relation not in its `writes` is a `rule-run-refusal` recorded on the
forward-rules state; the write stands (it is committed) and
`run-rules` is the repair.

**Instantiation and refraction.** An instantiation is the production's
name and version plus the **identity keys** of its premise claims in
body order (sorted until #471, which made a self-join's two orders one
instantiation and lost a row) (`claim-identity-key`, which survives a premise being swept and
re-derived or deleted and re-registered; node ids do not). The step
records each firing as a binary claim of a new family `firing`
(relation = production name, subject key = a digest of those identity
keys, object key = the keys in the clear since #471, producer = the
production), so refraction is durable, auditable,
and survives restart without a side table. A match that is lost
retracts its `firing` claim (D4) and delivers `:lost`; a match regained
re-opens it and delivers `:added` again, so the firing history of an
instantiation is readable as the claim's transaction periods. The
action is called once per transition.

**The agenda.** Fired instantiations are queued, not run inline: the
Datalog strata of the iteration finish and commit, then the agenda
runs. Ordering: higher `salience` first (an integer, default 0), then
the triggering commit's epoch, then the instantiation key. The agenda
runs on the step's thread in this unit; moving it to a per-graph worker
is a contained later change and the ordering rule does not depend on
which thread runs it. An action that signals is caught: the `firing`
claim is kept (the instantiation did fire), the condition is logged and
recorded on the forward-rules state, and the agenda continues.

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
(with `claim-current` in the bodies) and a query whose results exceed
any fixed cap; the productions' `:added` and `:lost` deliveries must
equal the set differences of full re-evaluation after each commit.

## 9. Docs that travel with this

`docs/rules.md` (a new section, "Forward evaluation"; `claim-absent/7`,
`now/1`, `instant-offset/3` in the functor table; the currency
paragraph for rule authors); the manual's rules chapter;
`CHANGELOG.md` Added, and Changed for the sweep becoming a retraction
in `run-rules` (D4, a visible change for anyone reading swept
derivations back); the fixpoint spec's R7 and R10 get a note that
forward mode inverts them, and its R5 that the sweep retracts.

## 10. Acceptance

1. A rule whose body says `claim-current` on a premise derives after
   the commit that supplies its last premise, without `run-rules`, and
   loses the derivation after the commit that retracts that premise;
   the lost derivation is a retracted claim whose provenance still
   names the premise, and a third commit restoring the premise re-opens
   it rather than constructing a second claim. A `run-rules` after any
   of the three changes nothing.
2. A chain through two strata derives in the same forward step; a
   derivation lost in one stratum is lost in the stratum that read it.
3. A rule with a `not` derives after the commit that retracts its
   blocker, and loses its derivation after a commit that supplies one.
4. `claim-absent/7` derives on `:searched-empty` and `:determined-empty`
   and derives nothing on `:uncovered`, `:indeterminate` or no claim.
5. A gain-threshold rule derives at its flip instant with validity from
   that instant; a loss-threshold rule loses its derivation at the
   instant; a store reopened and re-enabled before the instant still
   does both.
6. A `now` comparison the compiler cannot bound is refused at compile
   with the goal named.
7. A production fires `:added` once per instantiation across repeated
   commits of the same premises and across a premise being swept and
   re-derived, `:lost` once on retraction, and its `firing` claims are
   readable; a production reading what it writes is refused; a stored
   `production` naming an unregistered action is refused at commit.
8. The standing-query equivalence test of §8 passes, removals included.
9. An error in a Datalog body or a production leaves the triggering
   commit durable and the caller's `with-transaction` returning
   normally; `forward-rules-status` reports it; `run-rules` repairs the
   store.
10. No form this design adds runs inside `with-transaction-manager-lock`,
    and an opted-out graph's commit path is unchanged (reviewed).
11. A commit from a nested `with-transaction` defers its step to the
    outermost commit on that thread; a recursive stratum touched by a
    −1 delta reaches the same fixpoint `run-rules` reaches.
12. Two threads committing premises concurrently both get their
    derivations, no commit blocks on the other's step, and
    `forward-rules-status` showed a queue depth above zero at some
    point (a test with a slow production holding the runner).
13. The existing rules, fixpoint and scope suites stay green, with
    their sweep assertions updated from deleted to retracted.

## 11. Calls I made

- Justification-based retraction through the derivation family for
  non-recursive rules, and a full re-run for recursive strata and for
  anything under a `not`: the cheap case where it is sound, the engine's
  existing path where it is not.
- Currency stays the body's business in both modes (§4.2).
- One stratification graph for rules and productions; actions may
  write, and their commits join the running step's queue.
- `firing` as a claim family rather than a side table, keyed by
  identity keys.
- The step and the agenda on the committing thread, with the worker as
  a contained later change.
- A single evaluation instant per step and per `run-rules` call; the
  flip instant computed from premises only.
- The opt-in is a rules-layer call after `open-graph`; no core hook.
- The stored `action` is a registry name validated at commit.

## 12. Left to the builder

- The exact `forward-rules-status` shape; the digest for a firing key.
- Whether threshold timers live in one image-wide wheel or per graph.
- How the match plist names premise claims (by body variable is the
  obvious choice).
- The per-graph compiled-rule cache and its invalidation.
- The generalisation of body variants to non-recursive goals (§4.1)
  and where in `compile.lisp` the flip-instant extractor lives.
