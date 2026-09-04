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
  image, so pair it with a `claim/7` goal to restrict a family. With
  neither argument bound it fails: there is no index to generate from.
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
namespace with an unbound key, a bound key with an unbound namespace,
and a non-node `?c` with nothing else bound all land there.

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

`run-guarded-prolog` always binds both budgets, so on the guarded
surface an unrouted goal is **always** a refusal, exactly as spec §4
says. The walk is reachable only from an in-image `select` with
neither `:max-inferences` nor `:timeout`.

That refusal is unconditional: `:allow-cost-unbounded t` does not
reach it. The option is threaded as a literal at query-compile time
and no special variable carries it into a functor body
(kraison/vivace-graph#334). A caller who knows the walk is affordable
drops the budget, or binds a namespace and its key together so the
goal routes.

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

## What the functors do not see

They read the **committed** store. `index-lookup` does not see the
writes of a transaction it runs inside, and unlike `claims-touching`
-- which overlays the open transaction's write set (GH #324) -- these
do not compensate. Uniform across every route, so a query inside a
`with-transaction` sees the snapshot and nothing of its own writes.
Slice 2's `run-rule` derives claims inside the transaction it also
reads in, so it is the first caller this bites (GH #331).

## Tests and CI

FiveAM system `graph-db/rules-test` (`tests/rules/`), on-disk stores,
its own `sbcl` process -- CI lane `rules suite` in
`.github/workflows/test.yml`. Note the tripwire `docs/ci.md` records:
an image that loads `graph-db/rules` **and** `graph-db/gui-test` fails
`prolog-functor-inventory-is-pinned`, because that check is an equality
against a hand-reviewed list and the gui lane loads no rules. Classify
the seven functors there if you build such an image; never weaken the
check.
