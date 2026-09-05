# graph-db/rules: claims as Prolog facts

`graph-db/rules` (GH #304) is where a rule becomes a registered,
versioned producer: claims derived from other claims, validated like
any other write, readable back with their provenance. The design is
`docs/superpowers/specs/2026-09-04-rules-as-producers-design.md`.

**Slice 1 (GH #330) is all of it today**: the Prolog view of claims,
seven global functors a `select` or a guarded free-text query can read
claims as facts through. The rule record, `compile-rule` and `run-rule`
are slice 2 (GH #331). Nothing here writes.

## Loading it

```lisp
(ql:quickload :graph-db/rules)
```

`:depends-on (:graph-db/spacetime :graph-db/query)`; no web package.
Loading it registers the functors in `*prolog-global-functors*`, and
the guard enumerates that registry per call, so the very next
`run-guarded-prolog` admits them. No whitelist edit adds them;
withholding one from free text is `*prolog-excluded-predicates*`' job
(`query/guard.lisp`), not the whitelist's -- and that lever is
deliberately left unpulled here, for the reason under the
cost-unbounded rule below.

## Why the functors are homed in `graph-db`

The subsystem's package is `graph-db.rules` (spec §3), but
`rules/facts.lisp` is `(in-package #:graph-db)` and the seven
`name/arity` symbols -- `claim/7`, `claim-current/1`,
`claim-valid-at/2`, `claim-producer/2`, `claim-standing/2`,
`claim-relation/2`, `claim-rule-version/2` -- are `graph-db` exports.

`def-global-prolog-functor` splices the name as read and exports it
from `*package*`; the engine's `make-functor-symbol` resolves a goal
head first in the head symbol's own package and then in `graph-db`. A
functor homed anywhere else is unreachable from a raw `select` written
in any other package, so every consumer would have to import seven
`name/arity` symbols to write one goal. Homing them in `graph-db` is
what "global Prolog functor" already means here -- every entry in the
registry is `graph-db`-homed except the per-schema edge functors.

Declared deviation from spec §3's package wording. The subsystem, its
pathname and its `graph-db.rules` package are unchanged.

## The functors

```lisp
(claim ?c family ?sns ?skey ?rel ?ons ?okey)
(claim-current ?c)          (claim-valid-at ?c instant)
(claim-producer ?c ?p)      (claim-standing ?c ?s)
(claim-relation ?c ?r)      (claim-rule-version ?c ?v)
```

| functor | answers |
|---|---|
| `claim/7` | a claim of `family`, and its endpoints |
| `claim-current/1` | true while the transaction period is open |
| `claim-valid-at/2` | true when the validity extent covers `instant` |
| `claim-producer/2` | the producer -- also a generator, below |
| `claim-standing/2` | the standing, as a lowercase string |
| `claim-relation/2` | the relation |
| `claim-rule-version/2` | the rule version, or NIL |

`family` is the **parent** class name a `def-claim-classes` registered,
never an arity subclass. The registry is `eq`-keyed on the symbol as
read at the `def-claim-classes` call site, so a raw `select` in another
package writes it qualified (`my-schema::host-claim`); through the
guard the schema's own canonical symbol is what reaches the goal.

- **Namespaces cross as strings**: `"host"`, not `:host`. The guard
  refuses every colon before `READ` runs, so a keyword-spelled
  namespace in free text is a refusal, not a match
  (`docs/guarded-query.md`). An unbound namespace argument binds to
  the keyword's downcased name; an argument a Lisp caller already
  bound to a keyword unifies against that keyword, so results are
  symmetric with inputs either way. `claim-standing/2` answers in the
  same lowercase wire shape (`"inferred"`).
- **A unary claim binds `?ons` and `?okey` to NIL.**
- **Retracted claims are generated**, matching `claims-touching`'s
  default. `claim-current/1` is the goal that means "still believed".
- `claim-rule-version/2` answers NIL as a **solution**, not a failure,
  so a claim no rule wrote is still returned.
- `claim-valid-at/2` takes an ISO-8601 string or a `local-time`
  timestamp, and shares `claims-touching :at`'s own predicate and
  probe so the two cannot diverge (spec §11). A claim with no validity
  extent never matches. A malformed instant **fails the goal** rather
  than signalling, so a caller cannot tell a bad timestamp from no
  match.
- `claim-producer/2` with `?c` unbound and `?p` a producer name
  generates from the producer index of **every** claim family in the
  image, so pair it with a `claim/7` goal to restrict a family. **Goal
  order is load-bearing**: the `claim-producer` goal must come
  *before* the `claim/7` goal it feeds, or `?c` is already bound by
  the time it runs and it filters instead of generating. Both examples
  below are the filter direction.
- `claim-producer/2` with **neither** argument bound has no index to
  generate from and no walk to fall back to. Under a resource bound it
  signals `prolog-cost-unbounded-error`, exactly as an unrouted
  `claim/7` does; with no bound in effect it fails, answering nothing.
  A bound `?p` naming no producer is the empty answer instead, the way
  an unresolvable namespace is.
- A non-node `?c` fails every filter; none of them signals.

## Two examples

Guarded, from free text:

```lisp
(graph-db.query:run-guarded-prolog
 "(claim ?c host-claim \"host\" \"h1\" \"runs\" \"app\" ?o)
  (claim-current ?c) (claim-producer ?c ?p)"
 graph)
=> (values ("c" "o" "p")
           (("N1a2b3c..." "web" "scan-a") ("N4d5e6f..." "db" "scan-a"))
           nil)
```

Raw, in the image, from any package:

```lisp
(graph-db:select (:max-inferences 1000) (?o ?p)
  (claim ?c my-schema::host-claim "host" "h1" "runs" "app" ?o)
  (claim-producer ?c ?p))
=> (("web" "scan-a") ("db" "scan-a"))
```

`select`'s first group is OPTIONS and is required; `select-flat`,
`select-count`, `select-first` and `select-one` take `(vars . goals)`.

## Routes, and what refuses

`claim/7` picks a route from what is bound, in this order:

| bound | route |
|---|---|
| `?c` to a claim node | that claim |
| subject namespace, key and relation | the subject-relation index |
| subject namespace and key | the subject index |
| object namespace and key | the object index |
| a namespace naming no keyword this image recorded | empty, at once |
| none of the above | the family walk -- see below |

**The empty fast path is not a refusal.** A bound namespace argument
that resolves to no keyword -- a name no claim was recorded under, a
non-wire spelling like `"HOST"`, a number -- answers zero solutions and
interns nothing; query text cannot grow the `KEYWORD` package.

**Any shape the table does not route reaches the walk** -- the walk is
the `cond`'s last clause, not a nothing-bound special case. A bound
namespace this image recorded, with an unbound key, a bound key with an
unbound namespace, and a non-node `?c` with nothing else bound all land
there. A namespace that resolves to nothing is the row above instead.

**A bound key with an unbound namespace has no route.** The namespace
is the leading slot of both endpoint indexes and an index is only
usable from a prefix, so `(claim ?c f ?ns "h1" ?r ?ons ?ok)` falls
through to the walk and, under a budget, refuses. Bind the namespace;
it is almost always a literal. Not a defect -- the shape of the index.

**The cost-unbounded rule (GH #285).** An unrouted goal has no index
to generate from, and `%tick` cannot preempt inside one functor call,
so a family walk would run past any budget already in effect:

- under a resource bound -- an inference budget or a deadline -- the
  goal signals `prolog-cost-unbounded-error`;
- with no bound in effect, it walks the family, which a caller who
  could already call `map-vertices` may do.

`run-guarded-prolog` binds both budgets from
`*query-default-max-inferences*` and `*query-default-timeout*`
(`query/dsl.lisp`), so while those hold a value -- they are `defvar`s,
and an operator who NILs both reopens the walk on the guarded surface
too -- an unrouted goal there is **always** a refusal, exactly as spec
§4 says. Otherwise the walk is reachable only from an in-image
`select` with neither `:max-inferences` nor `:timeout`.

A caller who knows the walk is affordable says so with
`:allow-cost-unbounded t`, and gets it under a budget
(kraison/vivace-graph#334): `select` binds `*allow-cost-unbounded*` for
the query's dynamic extent, and the refusal here reads the same value
`%refuse-cost-unbounded`'s static one does. Binding a namespace and its
key together, so the goal routes, remains the cheaper answer.

The option reaches `claim-producer/2`'s neither-bound refusal too, but
there it buys silence rather than a walk: that goal has no index to
generate from and nothing to fall back to, so opting out of the refusal
leaves the empty answer an unbudgeted query already gets.

`claim/7` is deliberately **not** `declare-functor-cost-unbounded`'d.
That classifies a whole functor, and `%excluded-predicate-p` would
then withhold `claim` from free text entirely -- breaking the guarded
surface this slice exists to provide. Unboundedness here is a per-goal
property, not a per-functor one.

## Unknown names

- **An unregistered family** signals `unknown-claim-family`, which
  `run-guarded-prolog` reports as `prolog-ill-typed-error` -- client
  input, not `prolog-server-fault`. The arity subclasses are schema
  type names the guard admits but no family is keyed on, so
  `host-claim-unary` is the shape a caller gets this from.
- **An unknown namespace** is the empty fast path above, not an error.
- **A rule that does not compile** signals
  `graph-db.rules:rule-compile-error`, a
  `graph-db:constraint-violation` -- deterministic, so retrying the
  same write against the same schema is refused again.
  `rule-compile-error-rule` is the rule's name,
  `rule-compile-error-reason` the sentence saying what is wrong.

## What the functors do not see

They read the **committed** store. `index-lookup` does not see the
writes of a transaction it runs inside, and unlike `claims-touching`
-- which overlays the open transaction's write set (GH #324) -- these
do not compensate. Uniform across every route, so a query inside a
`with-transaction` sees the snapshot and nothing of its own writes.
Slice 2's `run-rule` derives claims inside the transaction it also
reads in, so it is the first caller this bites (GH #331).

This is also why the cycle check is strict rather than a fixpoint: **a
body cannot see the sweep**, nor the claims the same run constructs, so
a rule that read its own relation would read the *previous* run's
derivation and one `run-rule` would never settle. `compile-rule`
refuses that instead of iterating (GH #331).

## Tests and CI

FiveAM system `graph-db/rules-test` (`tests/rules/`), on-disk stores,
its own `sbcl` process -- CI lane `rules suite` in
`.github/workflows/test.yml`. Note the tripwire `docs/ci.md` records:
an image that loads `graph-db/rules` **and** `graph-db/gui-test` fails
`prolog-functor-inventory-is-pinned`, because that check is an equality
against a hand-reviewed list and the gui lane loads no rules. Classify
the seven functors there if you build such an image; never weaken the
check.

## The store's rule schema (GH #331)

`graph-db.rules:def-rules-schema (graph-name)` declares the store's
`rule` record (`name version family head body extent-policy enabled`)
and the `derivation` claim family, both per store like any `def-source`
or `def-claim-classes` call. `name` is the identity key -- one live
`rule` per name -- so a new version is `copy`, `setf rule-version`,
`save`, not a second write. A second `def-rules-schema` call (a
multi-store image) rebinds `make-rule`'s default store, so every
constructor call after that must pass `:graph` explicitly.

A store that never evaluated it holds no rules, and says so rather than
erring: `run-rules` on such a store reports nothing.

## Compiling a rule

`graph-db.rules:compile-rule (graph rule &key others)` turns a `rule`
record -- or a `rule-spec`, which is what `def-rule` registers -- into
a `compiled-rule`, or signals `rule-compile-error` (spec §6). Head and
body go through the guard as one text, so a variable shared between
them reads as one symbol.

**The head is exactly one `claim/7` pattern.** Written out,
`(claim ?c family sns skey rel ons okey)`:

- A second goal in the head, or any other functor, is refused.
- `?c` is an unbound variable that must not appear in the body: it
  names the claim the rule derives.
- `family` is the rule's own `family` slot, spelled as the schema's
  parent class name.
- `rel` is a literal canonical relation (`[a-z0-9-]+`). A variable
  there is refused -- a rule must say what it derives.
- The namespaces are canonical strings, interned as keywords at
  compile time, or body variables; the keys are strings or body
  variables. A head variable the body does not bind is refused.
- The object pair is both `nil` (a unary claim) or both given.

**The body is guarded exactly as free text is** -- the same character
screen, the same functor whitelist, the same refusals
`run-guarded-prolog` gives (`query/guard.lisp`). Two consequences:

- **No colon anywhere in the rule text.** The screen refuses `:`
  before the reader runs, so a body can name no keyword and no
  package-qualified symbol. Namespaces and standings are written as
  the lowercase wire strings the functors answer in (`"host"`,
  `"inferred"`).
- **A bare `?` is refused.** Read into the guard's scratch package
  every `?` in one text is the *same* named variable, not the engine's
  anonymous one, so all of them would have to unify. `compile-rule`
  refuses it rather than let a rule mean something other than it
  reads (recon note A10,
  `docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md`).

An effecting functor such as `retract` is **not** a compile refusal:
there is no static effect registry, so the guard admits the goal and
running it is what refuses (same note, A16).

**Generators move to the front (ruling P5).** A body goal
`(claim-producer ?v "p")` -- variable first, literal producer second --
generates from the producer index, so `compile-rule` runs it before
the rest of the body and a later `claim/7` on `?v` takes its node
route instead of walking the family. Every other goal keeps its order;
`(claim-producer ?p ?who)` is a filter and is left where it was.

**Recursion is refused, and the cycle is named (ruling P6).** The
cycle graph is over relation names: a rule's head relation points at
every relation its body reads, and so does every other enabled rule in
scope -- the store's rules plus every `def-rule`. A path from the head
relation back to itself is refused, spelling the path
(`deriving "y" closes a cycle: y -> x -> y`). A body `claim/7` that
leaves its relation unbound reads *every* relation, its own included,
so it is always a one-node cycle; that refusal says to bind the
relation.

**A name belongs to one source.** A stored `rule` and a `def-rule` of
the same name is a collision, refused whichever arrives second.

**A `rule` write that does not compile is refused at commit** (ruling
P3). `%validate-rule-writes` sits on `graph-db:*commit-validators*`
and compiles every written `rule` against the store as the commit will
leave it -- the cycle check included -- so the store never holds a
rule that could not run when it was written. `enabled nil` is not an
exemption: a disabled rule is compiled, only not run. The validator is
inert until some store has evaluated `def-rules-schema`, which is what
makes the `rule` class.

### `def-rule`, the in-image escape hatch

`(def-rule "web-hosts" :version "1" :family rt-claim :head ... :body
...)` registers a rule in the image rather than in a store (spec §5).
`family` is the parent class symbol, unevaluated; every other argument
is evaluated. `undef-rule` forgets one and `find-def-rule` returns its
`rule-spec`. A `def-rule` is compiled per store, when it runs, because
the cycle check needs that store's other rules -- but it constrains
the cycle graph of every store in the image, so a `def-rule` can be
the reason a stored rule's write is refused.

## Running a rule

`graph-db.rules:run-rule (graph rule) => rule-report` derives `rule`
afresh and reconciles the result with its previous derivation, in **one
transaction** (spec §7). `rule` is a `rule` record, a `rule-spec`, or a
name -- looked up in the store first, then among the `def-rule`s.

**Reconcile, not sweep-then-insert (ruling P10).** `run-rule` evaluates
the body first, then compares the identities it derived against the
claims `rule/<name>` already holds:

- an identity derived again is **kept** -- the same node, so its id and
  its version chain survive -- with its `rule-version` refreshed by
  `copy`/`save` when the rule's version moved;
- an identity no longer derived is **swept** (`mark-deleted`);
- an identity not held before is **derived** (constructed).

The order matters and is not a preference. `mark-deleted` releases a
unique key only *post*-durability, while `validate-unique-constraints`
runs *pre*-durability, so a sweep and a re-insert of an unchanged claim
in one transaction always collide under `def-unique` (recon note C1,
`tests/spacetime/claim-query-tests.lisp`). Deriving first and keeping
what is unchanged has no such collision, and it is sound because a body
reads the committed store either way (recon note A6).

A **retracted** derived claim whose identity is derived again stays
retracted: keeping is not re-assertion. `run-rule` writes no
transaction extent, so `retract-claim`'s closed period stands until
something re-asserts it.

**Duplicates collapse.** Two solutions with the same head endpoints and
relation are one claim; for a temporal family the extent *start* joins
that key, exactly as `claim-identity-key` does -- so two solutions that
differ only in extent **kind** (an instant at T against an interval
starting at T) collapse too, which is what the family's own identity
rule would have forced anyway (recon note C5, ruling P11).

**The rails, always (ruling P4).** The body runs through
`run-query-goals`: `:effects nil`, one snapshot (inherited from the
open transaction), and a resource bound. `*rules-max-inferences*` and
`*rules-timeout*` are the operator's; `nil` on either falls back to the
DSL's `*query-default-max-inferences*` / `*query-default-timeout*`.
If the *effective* pair is `nil` -- both rule variables and both DSL
defaults -- `run-rule` signals a plain error rather than walking a
family unbounded. `*rules-max-solutions*` (100000) caps the collected
solutions; past it the run is refused rather than silently truncated.

**The report** (`rule-report`):

| field | meaning |
|---|---|
| `rule-name`, `version` | the rule as run |
| `outcome` | `:derived` or `:refused` |
| `derived`, `kept`, `swept` | the reconcile's three counts |
| `disjoint-premises` | solutions dropped: premises never held at once |
| `refusals` | a list of `(tag . text)` |
| `inferences` | the count at the last solution |
| `elapsed` | seconds |

A refusal's `tag` is a **claim family name** for a refusal the commit or
a constructor raised (`extent-disjointness-violation`,
`unique-constraint-violation`, `missing-claim-identity-component`),
else one of:

- `:rule` -- the rule's own fault: it no longer compiles, its family is
  not in this store, an effecting goal was refused at run
  (`prolog-permission-error`, recon note A16), or a head term is not a
  namespace or key this image knows;
- `:budget` -- the rails: the inference budget, the timeout, or a goal
  refused as `cost-unbounded`;
- `:solutions` -- the `*rules-max-solutions*` cap.

**Nothing refuses by signalling.** Every refusal is reported, and every
one unwinds the transaction, so **the previous derivation stands
untouched** -- `derived`, `kept` and `swept` all read 0 on a
`:refused` report. Only an operator error signals: no resource bound,
or no rule of that name in the store or the image.

## Validity of a derived claim

Under `:extent-policy :premises` (the default) a derived claim's
validity extent is the **intersection** of the validity extents of its
premises -- the claims bound to the `?c` variables of the body's
`claim/7` goals for that solution (spec §8). `extent-intersection`
(cl-temporal-extent 0.3.0) does the work, with `:semantics :validity`
and `:standing :inferred`. A premise with no extent contributes
nothing; if no premise has one, the derived claim has none.

- An **empty** intersection means the premises never held at once. No
  claim is derived for that solution and `disjoint-premises` counts it.
- A **temporal** family with no extent to attach refuses at
  construction (`missing-claim-identity-component`), reported with the
  family as the tag.

`:extent-policy :none` derives claims with no extent at all, for a
non-temporal family.

**The policy is orthogonal to the family's temporality (ruling P7).**
`:premises` on a *non*-temporal family still intersects and still drops
the disjoint solutions -- the extent is attached, it simply plays no
part in that family's identity, so two solutions differing only in
extent collapse to one claim.

## Provenance

Every (derived claim, premise) pair is one binary claim of the
`derivation` family (spec §9): subject `(:claim . <derived identity
key>)`, relation `"derived-from"`, object `(:claim . <premise identity
key>)`, producer `rule/<name>`, the rule's `rule-version`, standing
`:inferred`. Identity keys, not node ids, so provenance survives a
premise's retraction and regeneration. The records reconcile exactly as
the claims do: a pair still asked for is kept and re-versioned, one no
longer asked for is deleted.

- `(premises-of graph claim) => claims` -- the claims `claim` was
  derived from. A premise whose identity no longer exists in the store
  is dropped rather than faked.
- `(dependents-of graph claim &key current) => claims` -- every derived
  claim whose provenance names `claim`. With `:current`, only those
  still believed.

**Retracting a premise does not re-derive anything.** Its dependents
stay current and stay findable through `dependents-of`; deciding what
to do about them is the caller's, and in a multi-agent setting
kraison/blackboard's.

## `run-rules`

`graph-db.rules:run-rules (graph) => list of rule-report` runs every
enabled rule the store can run, in dependency order: a rule that reads
relation R runs after every rule that derives R (spec §7). Cycles were
refused at compile, so the order always exists; ties keep the order the
rules came in.

- **A disabled rule is not in scope at all.** `rules-in-scope` filters
  on `enabled`, stored rules and `def-rule`s alike, so `run-rules`
  neither compiles a disabled rule nor counts it in the cycle graph --
  and a rule it would otherwise have made cyclic compiles fine while it
  stays disabled (ruling T3-R5). The one place `enabled` buys no
  exemption is the *write*: `%validate-rule-writes` compiles every
  `rule` record it commits, disabled or not, so a disabled rule is
  still a rule that could run.
- **A `def-rule` runs only where the store carries its family (ruling
  P8).** `*def-rules*` is image-wide; a rule whose family this store
  never declared is skipped silently, because it is not this store's
  rule. `run-rule` called on it directly still answers -- with a
  `:refused` report tagged `:rule`.
- **A rule that no longer compiles is reported and skipped**, never
  refused at open. A `def-rule` registered after a store's rules were
  written can close a cycle with one of them; both then read
  `:refused` with a `:rule` tag and every other rule still runs.
