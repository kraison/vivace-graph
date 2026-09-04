# Decisions: rules S1 (#330, epic #304)

The controller rulings S1 was executed under, transcribed from the
session's working notes so the record outlives the worktree. Each was
taken during execution, without Kevin in the loop, after a
source-verified recon pass; each carries the cost of being wrong as it
was stated at the time. Nothing here is re-derived or improved —
where a ruling deviates from the spec, this file is the deviation
record.

Order of authority while S1 ran: spec > these rulings > the recon's
API facts > the task brief.

## R1 (blocking) — `rules/facts.lisp` is `(in-package #:graph-db)`

**Decision.** The file registering the global claim functors is homed
in `graph-db`, not `graph-db.rules`. The seven `name/arity` symbols
become `graph-db` exports; spacetime symbols in that file are written
qualified. The subsystem, its pathname and its `graph-db.rules`
package are unchanged.

**Evidence.** `def-global-prolog-functor` splices the name as read and
`export`s from `*package*`; `make-functor-symbol` probes only the goal
head symbol's own package and then `GRAPH-DB`. Under
`(in-package #:graph-db.rules)` every raw `select` in the suite would
fail with "unknown Prolog functor CLAIM/7". Every entry in
`*prolog-global-functors*` is `GRAPH-DB`-homed except the per-schema
edge functors, which are per-schema by design.

**Declared deviation** from spec §3, which names the subsystem's
package as `graph-db.rules`.

**Cost if wrong.** The file's home package differs from the spec's
wording. Reversing it is mechanical, but every downstream consumer
writing a raw `select` would then have to import seven `name/arity`
symbols — which is why it was not chosen.

## R2 (blocking) — every `select` goal takes an OPTIONS group

**Decision.** Write `(select () (?o) (claim …))`. `select-flat`,
`select-count`, `select-first` and `select-one` really are
`(vars &rest goals)`.

**Evidence.** The live macro is `(defmacro select (options vars &rest
goals))`; the two-group form is inside a block comment and is not
defined. `(select (?o) GOAL)` compiles without error, parsing `(?o)`
as OPTIONS and the goal as VARS, and returns one junk row.

**Cost if wrong.** Tests pass vacuously on junk rows.

## R3 (blocking) — index slot-name lists are `graph-db.spacetime` symbols

**Decision.** Write the slot lists with explicit
`graph-db.spacetime::` markers (the slot symbols are not exported),
bound once as `defparameter`s near the top of `facts.lisp` rather than
repeated at each call site.

**Evidence.** `index-lookup` compares the slot-name list with `EQUAL`,
i.e. `EQ` per symbol, against what `def-index` stored — symbols read
in `graph-db.spacetime`. Read from `graph-db`, `'(subject-namespace …)`
is a different symbol, no index matches, and `%require-index` signals
`query-precondition-error`.

**Cost if wrong.** Every generator route signals instead of answering.

## R4 — `rules/package.lisp` has no imports and no exports

**Decision.** `(defpackage #:graph-db.rules (:use #:cl))`, with a
comment saying S2's rule record and `def-rule` live here.

**Evidence.** R1 leaves nothing in it for S1, and — the trap — the
plan's import list pulls spacetime's `claim-relation`,
`claim-producer`, `claim-standing` and `claim-rule-version`
*accessors* into the package. Those names collide with four *functor*
names through `symbol-package`: the guard's `(intern "CLAIM-PRODUCER"
home)` would return the imported spacetime symbol,
`make-functor-symbol` would probe `GRAPH-DB.SPACETIME` for
`CLAIM-PRODUCER/2`, miss, and the goal would fail as unknown. The
warning now lives in `rules/package.lisp`'s header.

**Cost if wrong.** An empty package file lands a slice early.
Harmless.

## R5 — `%unify-claim` checks the family's PARENT class

**Decision.** `%unify-claim` yields nothing unless the claim is
`typep` the family's **parent** class; `claim-family-binary` then only
decides unary vs binary.

**Evidence.** Testing only the binary class lets a claim of another
family — one `claim-producer/2` bound to `?c` — unify as a unary claim
of the asked family, with NIL endpoints. Spec §4 says `claim/7` yields
"claims of `family`".

**Cost if wrong.** Cross-family leakage.

## R6 — the `scan-a` count is 2, not 3

**Decision.**
`(is (= 2 (select-count (?c) (claim-producer ?c "scan-a") (claim ?c
rt-claim ?s ?k ?r ?a ?b))))`.

**Evidence.** Counted from the seed: `scan-a` wrote 2
`rt-claim-binary` and 2 `rtt-claim-binary` = 4 claims; filtered to
family `rt-claim` by the second goal, the answer is 2. The plan's 3 is
reachable by no correct implementation.

**Cost if wrong.** A red test on correct code, or a green one on R5's
bug.

## R7 — the cost-unbounded rule is per goal, not per functor

**Decision.** A `claim/7` goal no index route covers is refused as
cost-unbounded when a resource bound is in effect, and otherwise walks
the family. Do **not** use `declare-functor-cost-unbounded`.

**Evidence.** Spec §4 says nothing-bound is "refused as cost-unbounded
(#285)" full stop; the plan refines it, and that refinement is
recorded in the plan's Global Constraints and in the session handoff's
"Decisions that bind S1", so it is an approved amendment rather than
drift. Through the guard it is always a refusal, because
`run-query-goals` always binds a budget, so the guarded surface
matches the spec exactly. `declare-functor-cost-unbounded` classifies
a whole functor, and `%excluded-predicate-p` would then withhold
`claim` from free text entirely — breaking the guarded surface the
slice exists to provide.

**Cost if wrong.** A Lisp caller can walk a family without a bound.

## R9 — `%ill-typed-condition-p` resolves `unknown-claim-family` at call time

**Decision.** Deferred `find-symbol` in `GRAPH-DB.SPACETIME`, guarded
by `find-package`, resolved on first call and cached.
`graph-db/query` gains no dependency on spacetime.

**Evidence.** The `*no-applicable-method-type*` pattern the plan
copies is a `defvar` evaluated when `query/guard.lisp` loads. `SB-PCL`
always exists then; `GRAPH-DB.SPACETIME` need not —
`graph-db/query` depends only on `graph-db/core` and must load
standalone. A load-time `find-symbol` would cache NIL forever.

**Cost if wrong.** An ill-typed family name is reported as an engine
fault (500) instead of client input (400), and no test in the query
suite would catch it.

## R10 — `%unbound-claim-scan` passes `:collect-p t`

**Decision.** `(map-vertices #'identity *graph* :vertex-type …
:collect-p t)`, using the return value; do not push into a closure
variable.

**Evidence.** `map-vertices` applies `ensure-node-bytes` only when
collecting; nodes pushed out of a side-effect scan escape the read pin
with lazy, unpinned data blocks.

**Cost if wrong.** Intermittent corruption or stale reads under
concurrency — the worst class of bug to ship, and invisible in a
single-threaded test.

## R11 — `claim-valid-at/2` calls the existing helper

**Decision.** Call `graph-db.spacetime::%claim-validity-touches-p`
with the probe built the way `claims-touching` builds it,
`(make-instant (exact-bound at))`.

**Evidence.** That helper is exactly the predicate `claims-touching
:at` uses, and it already answers NIL for a claim with no extent. The
plan re-derives it inline, which would also pass NIL to
`extents-disjoint-p`, whose `check-type` signals rather than
answering. Spec §11 requires the functor and `claims-touching :at` to
agree; sharing the predicate is how that stays true.

**Cost if wrong.** A type error on extent-less claims, and two copies
of a temporal predicate that must never diverge.

## R17 — a keyword namespace argument answers symmetrically

**Decision.** When a namespace argument arrives already bound to a
keyword, `%unify-claim` unifies it against that keyword. An unbound
argument still binds to the lowercase wire string.

**Evidence.** `%namespace-keyword` accepts a keyword for the lookup,
but unifying always against the string form silently answers nothing:
`prolog-equal` has no symbol-vs-string method, so `(claim ?c rt-claim
:host "h1" "runs" ?a ?b)` found the right candidates through the index
and then yielded no rows. Silence is the worst of the three possible
behaviours, and the plan's §4 amendment says the point is that results
are symmetric with inputs.

**Cost if wrong.** `claim/7` accepts one more input shape than spec
§4's wire form describes. It cannot reach the guarded surface —
colons are refused pre-read — so the blast radius is Lisp callers
only.

## R18 — `claim/7` does not honour `:allow-cost-unbounded`

**Decision.** A `claim/7` goal with no index route, under a budget or
deadline, is refused unconditionally; `:allow-cost-unbounded t` does
not reach it. Do not add a special variable to `prologc.lisp`.

**Evidence.** `%refuse-cost-unbounded` receives `allow-p` as a literal
threaded from `select`'s option at query-compile time; no special
variable carries it, so a run-time refusal inside a functor body
cannot see it. `claim/7` is deliberately not
`declare-functor-cost-unbounded`'d (R7), so it never reaches that
machinery. Spec §3 keeps core untouched, and threading a variable is
its own change with its own issue.

**Cost if wrong.** A caller who knows the walk is affordable cannot
ask for it under a budget; they drop the budget or bind an endpoint.
A follow-up issue is filed against the engine for the general escape
hatch (kraison/vivace-graph#334).

## R19 — the GUI functor inventory is checked, never weakened

**Decision.** Read `prolog-functor-inventory-is-pinned` before
deciding. If it is a subset check, add the seven `name/arity` names to
the reviewed inventory; if it is an equality check, adding them would
redden the gui lane, where no rules system is loaded — leave it alone
and note in `docs/ci.md` that an image loading both subsystems trips
it. Never weaken or skip the tripwire.

**Evidence.** CI runs each suite in its own `sbcl` process and
`graph-db/gui` does not depend on `graph-db/rules`, so the rules
functors never appear in the gui lane. It is an equality check, so the
second branch was taken.

**Cost if wrong.** Either a red gui lane, or a tripwire that stops
covering an image loading both.

## R21 — budget a test only when the test's subject IS a route

**Decision.** Keep `select-flat` / `select-count` unbudgeted for the
filter tests, whose routes Task 2's budgeted tests already prove;
budget the one case whose own subject is an index route,
`claim-producer/2`'s generator with `?c` unbound. And ask of every new
test explicitly: would this fail if the thing it is named after broke?
Where a filter's assertion would pass with the filter deleted,
strengthen it.

**Evidence.** A `claim/7` goal in an unbudgeted `select` cannot be
distinguished from the family walk, which is what made budgets
necessary in Task 2; that reasoning does not extend to goals whose
subject is a filter. `select-flat` / `select-count` take no options
group, so budgeting them means rewriting them, which is noise where
the route is not the point.

**Cost if wrong.** A filter test that passes vacuously.

## R22 — the producer scan skips foreign families

**Decision.** `%producer-candidates` catches the
`query-precondition-error` `%require-index` signals and treats it as
"this graph does not carry that family", not as a fault. Confirm by
running that the catch actually fires rather than claiming it works.

**Evidence.** `*claim-families*` is one image-wide global, not
per-graph, so a `claim-producer` generator iterating it will name
families whose parent class this graph's schema does not carry. The
suite now declares a third family bound to a different graph name so
the swallowed-condition path is exercised.

**Cost if wrong.** Not stated in the ruling.

## R25 — a batched fifth task collects the deferred minors

**Decision.** After Task 4 is reviewed clean, dispatch one batched
Task 5 carrying the minors the three task reviews had deferred, review
it as a task, and only then run the final whole-branch review. Do not
fold them into Task 4.

**Evidence.** The minors were individually small, all in the same two
files, and none belonged to the task loop. Handing them to the final
review would order a single fix wave anyway; doing that wave first
means the final reviewer sees a finished branch instead of a branch
plus a to-do list. Task 4 already owned the guard change, the CI lane,
the docs and a four-suite run, and mixing them would blur its review
surface.

**Cost if wrong.** One extra dispatch and review cycle.

---

## Deferred out of S1

Three items the final whole-branch review saw and deliberately did not
fix here: `rules/facts.lisp`'s unbound-`?c` gate folds an explicit NIL
into "unbound" and the correct `(var-p (var-deref ?c))` belongs to
S2's first touch; `prolog-cost-unbounded-error`'s report text tells
the caller to pass `:allow-cost-unbounded t`, which R18 explains does
not work for `claim/7`, and fixing it means touching core that spec §3
forbids for this slice (kraison/vivace-graph#334);
`spacetime/claim-query.lisp`'s unknown `:name` initarg is pre-existing
and outside the diff (kraison/vivace-graph#335).
