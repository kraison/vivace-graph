# graph-db/rules: claims as Prolog facts

`graph-db/rules` (GH #304) is where a rule becomes a registered,
versioned producer: claims derived from other claims, validated like
any other write, readable back with their provenance. The design is
`docs/superpowers/specs/2026-09-04-rules-as-producers-design.md`.

**Slice 1 (GH #330)** is the Prolog view of claims: the global
functors a `select`, or a guarded free-text query, reads claims as
facts through (seven at first; GH #388 added the extent functors and
the instant comparisons, GH #389 the vocabulary routes). **Slice 2
(GH #331)** is the rule itself -- the stored `rule` record and the
`derivation` provenance family
(`def-rules-schema`), `def-rule` as the in-image escape hatch,
`compile-rule`, and `run-rule` / `run-rules` -- from "The store's rule
schema" down. Slice 1 only reads; `run-rule` is the one thing here
that writes, and it writes claims like any other producer.

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
`rules/facts.lisp` is `(in-package #:graph-db)` and the `name/arity`
symbols -- `claim/7`, `claim-after/9`, `claim-current/1`,
`claim-valid-at/2`,
`claim-valid-from/2`, `claim-valid-to/2`, `claim-recorded-at/2`,
`claim-retracted-at/2`, `claim-touched-at/2`,
`claim-producer/2`, `claim-standing/2`, `claim-relation/2`,
`claim-rule-version/2`, and `instant</2`, `instant>/2`, `instant<=/2`,
`instant>=/2`, `instant=/2` -- are `graph-db` exports.

`def-global-prolog-functor` splices the name as read and exports it
from `*package*`; the engine's `make-functor-symbol` resolves a goal
head first in the head symbol's own package and then in `graph-db`. A
functor homed anywhere else is unreachable from a raw `select` written
in any other package, so every consumer would have to import a dozen
`name/arity` symbols to write one goal. Homing them in `graph-db` is
what "global Prolog functor" already means here -- every entry in the
registry is `graph-db`-homed except the per-schema edge functors.

Declared deviation from spec §3's package wording. The subsystem, its
pathname and its `graph-db.rules` package are unchanged.

## The functors

```lisp
(claim ?c family ?sns ?skey ?rel ?ons ?okey)
(claim-after ?c family ?sns ?skey ?rel ?ons ?okey skey-after okey-after)
(claim-absent ?c family ?sns ?skey ?rel ?ons ?okey)
(claim-current ?c)          (claim-valid-at ?c instant)
(claim-valid-from ?c ?from) (claim-valid-to ?c ?to)
(claim-recorded-at ?c ?at)  (claim-retracted-at ?c ?at)
(claim-touched-at ?c ?at)
(claim-producer ?c ?p)      (claim-standing ?c ?s)
(claim-relation ?c ?r)      (claim-rule-version ?c ?v)
(instant< ?a ?b) (instant> ?a ?b) (instant<= ?a ?b) (instant>= ?a ?b)
(instant= ?a ?b)
(instant-offset ?x duration ?y)   (now ?t)      ; rule bodies only
```

| functor | answers |
|---|---|
| `claim/7` | a claim of `family`, and its endpoints |
| `claim-after/9` | `claim/7`'s rows with strict lower bounds on both keys, the namespace route started at the bound; "Solution order" (GH #451) |
| `claim-absent/7` | `claim/7`'s rows restricted to current claims of standing `:searched-empty` or `:determined-empty`: the determined absences (D2) |
| `claim-current/1` | true while the transaction period is open |
| `claim-valid-at/2` | true when the validity extent covers `instant` |
| `claim-valid-from/2` | the validity start, as an instant string (GH #388) |
| `claim-valid-to/2` | the validity end, as an instant string, or NIL when open |
| `claim-recorded-at/2` | when the claim was recorded, as an instant string |
| `claim-retracted-at/2` | when it was retracted, as an instant string, or NIL while open (GH #391) |
| `claim-touched-at/2` | the later of the two: the one cursor a change feed needs |
| `claim-producer/2` | the producer -- also a generator, below |
| `claim-standing/2` | the standing, as a lowercase string |
| `claim-relation/2` | the relation |
| `claim-rule-version/2` | the rule version, or NIL |
| `instant</2` and friends | order two instants by value (GH #388) |
| `instant-offset/3` | `?y` is `?x` plus an ISO-8601 duration of weeks, days, hours, minutes and seconds (`P1W`, `P2DT3H`, `PT90S`); months and years are refused (#456) |
| `now/1` | the evaluation instant, one per `run-rules` call and per forward step; a rule body's functor only, refused in free text (#456) |

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
  It does **not** mean "currently valid": a claim whose validity a
  successor closed is not retracted and still passes. Nor does
  `claim-valid-at/2` mean "held now": it reads validity only, so a
  retracted claim still answers it. "Held now" is **both** --
  `claim-current/1` and `claim-valid-at/2` with the present instant
  (GH #388, #391).
- `claim-rule-version/2` answers NIL as a **solution**, not a failure,
  so a claim no rule wrote is still returned.
- `claim-valid-at/2` takes an RFC 3339 / ISO-8601 string or a
  `local-time` timestamp, and shares `claims-touching :at`'s own
  predicate and probe so the two cannot diverge (spec §11). A claim
  with no validity extent never matches. A malformed instant -- a
  string that does not parse, or a number -- **signals**
  `query-precondition-error`, which `run-guarded-prolog` answers as
  `prolog-ill-typed-error` (GH #388; before it, the goal failed
  silently and a bad timestamp looked like no match).
- `claim-valid-from/2` and `claim-valid-to/2` (GH #388) bind the
  validity extent's two ends as **strings in one spelling**: RFC 3339,
  UTC, nanosecond precision, 30 characters --
  `"2026-01-01T00:00:00.000000000Z"` -- so any two answers order
  lexically in time order and cross a wire unchanged. A fuzzy bound
  answers its outer edge (the earliest start, the latest end), the
  envelope `claim-valid-at/2`'s "possibly contains" already uses; an
  unbounded edge -- an open end -- answers NIL as a **solution**, so
  `(claim-valid-to ?c nil)` selects the open-ended claims. A claim
  with no extent fails both.
- `claim-recorded-at/2` binds the start of the claim's transaction
  extent in the same spelling -- when it was recorded, not when it
  became true -- or NIL for a claim predating the transaction-time
  axis (GH #148). With `instant>/2` it is a change feed: every claim
  recorded after the last instant a reader saw -- but not every
  change. `retract-claim` closes the transaction period in place and
  records nothing new, so a retraction is invisible to a cursor on
  `claim-recorded-at/2` alone (GH #391).
- `claim-retracted-at/2` (GH #391) binds the transaction extent's
  **end** in the same spelling: the instant `retract-claim` closed the
  period, the latest edge of a fuzzy end, or NIL as a **solution**
  while it is open -- so `(claim-retracted-at ?c nil)` selects the
  open claims, and with `instant>/2` it is the retraction feed.
- `claim-touched-at/2` (GH #391) binds the later of the two: the
  retracted-at when the period is closed, the recorded-at otherwise
  (an end never precedes its start). One cursor sees every write that
  moves a claim's transaction record -- record, supersession,
  cross-producer outdating, retraction -- so a reader keeps one
  per-namespace cursor instead of a cursor plus a periodic full
  re-read. Both goals still visit every claim of the namespace under
  `max-inferences`; an index that makes the cursor goal proportional
  to what changed is #392.
- `instant</2`, `instant>/2`, `instant<=/2`, `instant>=/2`,
  `instant=/2` compare two instants **by value**, each a string in any
  RFC 3339 spelling or a timestamp, so a client's `"2026-02-01"`
  compares correctly against the functors' 30-character answer -- the
  plain `</2` on two strings is lexical (GH #387) and right only when
  both sides share one spelling. An unbound or NIL side fails; a
  malformed one signals as `claim-valid-at/2` does.
- `claim-producer/2` with `?c` **unbound** -- not bound to NIL, which
  is a bound non-node and simply fails -- and `?p` a producer name
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
  A bound `?p` that is a string naming no producer is the empty
  answer; a `?p` bound to NIL is not a producer name and takes the
  neither-bound path.
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
| subject namespace, key unbound (relation bound or not) | the vocabulary route: keys from the vocabulary index, each key's claims from the subject (-relation) index (GH #389) |
| object namespace, key unbound | the vocabulary route on the object side |
| none of the above | the family walk -- see below |

**The vocabulary routes (GH #389).** "Every claim about a subject in
namespace N" is the natural question of a claim store, and until
GH #389 it fell to the walk. Now `claim/7` reads the keys filed under
the namespace from the vocabulary index (`claim-keys`, GH #350) --
cost proportional to the number of *keys*, never of claims -- and
generates each key's claims from the same index the bound-key routes
read, in ascending key order. It is bounded where the walk is not:
the work between two `%tick`s is one key's claims, so a budget
preempts between keys; no up-front pricing refuses a large namespace,
because the first page of it is exactly what paging (`:offset`, GH
#387) is for. Both namespaces bound and neither key takes the subject
side. Keys with no claim of a bound relation cost one empty lookup
each.

**The empty fast path is not a refusal.** A bound namespace argument
that resolves to no keyword -- a name no claim was recorded under, a
non-wire spelling like `"HOST"`, a number -- answers zero solutions and
interns nothing; query text cannot grow the `KEYWORD` package.

**Any shape the table does not route reaches the walk** -- the walk is
the `cond`'s last clause, not a nothing-bound special case. A bound
key with an unbound namespace, and a non-node `?c` with nothing else
bound, land there. A namespace that resolves to nothing is the empty
row instead; one that resolves, with its key unbound, is the
vocabulary route (GH #389).

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

## Solution order (GH #387)

Solutions come in goal-execution order: the first goal's solutions in
the order its route produces them, and for each, the second goal's,
depth-first. `claim/7`'s routes produce, in this order:

- the stores in scope, own store first (`*claim-scope*`);
- within a store, for the vocabulary routes, keys in **ascending index
  order** (the engine's collation, `string<` for string keys);
- within one key -- or for the bound-key routes -- that key's claims
  in the index's own order, which is stable while the store is
  unchanged but not otherwise specified;
- this run's own derivation last (the fixpoint's delta, GH #333).

The order is stable across calls while no write lands, which is what
makes `run-guarded-prolog`'s `:offset` a usable page (GH #387); a
write between two pages can shift a row across the boundary. A caller
who wants a boundary a write cannot move pages by **key** instead: on
a vocabulary route, `(> ?k "last-key-seen")` after the `claim/7` goal
-- `>/2` orders two strings lexically since GH #387 -- restarts after
that key. When the `>` goal is the very next goal after the `claim/7`
goal, the compiler fuses the two into `claim-after/9`, whose namespace
route skips the earlier keys before reading their claims, so a page
costs one comparison per earlier key plus its own keys' claims, at any
depth (GH #451). The comparison stays in the body, so a claim the route
skips is always one the comparison would have rejected. Not fused, and
so paid for by the comparison alone: `>=`, `<`, a numeric bound, any
goal or cut between the two, a comparison inside `or`, `not`, `if`,
`once` or `forall`.
A `select` is rewritten when its file compiles, so a file compiled
before `graph-db/rules` loaded is not fused until recompiled; a guarded
query is compiled per request and always is.

`claim-after/9` may be written by hand:
`(claim-after ?c family sns ?skey rel ons ?okey skey-after okey-after)`
answers the `claim/7` rows whose subject key is `>` `skey-after` and
whose object key is `>` `okey-after`, by `>/2`'s predicate; a `nil` or
unbound bound admits every row, and an object bound excludes a unary
claim. A rule body may not name it -- `compile-rule` refuses, since its
reads and strata analyses recognise `claim/7` by name -- and writes
`claim/7` and the comparison instead, which the rule's run fuses.
Neither offset nor key bound is a snapshot cursor;
`docs/guarded-query.md` "Paging" has the trade-off.

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

They read the store as the open transaction will commit it: since
GH #408 `index-lookup`, the typed scans and the adjacency walks return
the transaction's own creates, saves and deletes on top of its
snapshot, as `claims-touching` already did with its own overlay
(GH #324), and so do `index-count` and `map-index-prefixes` (GH #409)
and the count index (GH #411). Only the untyped scans still read
committed membership. Slice 2's `run-rule` derives claims inside the transaction
it also reads in; its producer exclusion keeps a rule from re-finding
its own derivations (GH #331, #333).

This is why a rule that reads its own relation cannot be run by
reading it: **a body cannot see the sweep**, nor the claims the same
run constructs, so a plain read would answer from the *previous*
run's derivation. Slice 2 refused such a rule at compile; the
fixpoint runs it instead, feeding the recursive goal from an
in-memory delta and excluding the stratum's own producers from the
index routes (GH #333, "Recursive rules").

**A scope widens what they read, not when.** With
`graph-db::*claim-scope*` bound to a list of open stores -- own store
first, NIL for `*graph*` alone -- `claim/7`'s three indexed routes and
its family walk read all of them and answer the union, as
`claim-producer/2`'s generator does, still the committed state of each.
The other two routes have no store to widen over: a `?c` already bound
to a claim node answers that node, and a namespace naming no keyword is
the empty fast path. A store in scope whose schema never declared the
family contributes nothing; the own store still refuses, as slice 1
documented. The trap is the engine's, not ours: inside a read-write
transaction on one store every read of another signals
`cross-graph-transaction-error` (GH #53), so bind the scope outside a
transaction. Full section in slice 3 (GH #332); `run-rule`'s `:scope`
is under "Running a rule".

**A snapshot hides an insert, not a delete.** Secondary-index
*membership* is not snapshot-versioned: `%ix-release` removes the entry
outright, post-durability, and `index-lookup`'s only snapshot-aware
step is resolving an id it has already found. So under a read snapshot
a claim inserted after it is correctly invisible -- and a claim deleted
after it is invisible too, though the snapshot's epoch predates the
delete. Not new with a scope: equally true of a single-store run
(recon note O1). Settled by test (kraison/vivace-graph#345): the
index-backed read also checks the resolved version against the key, so
a claim re-indexed after the snapshot is never answered under its new
value with its old version. Versioned membership is #113's scope.

**A cross-store evaluation runs under no transaction at all.**
`run-rule` opens its write transaction only after the body has been
evaluated, so `*transaction*` is `nil` throughout it. A Lisp caller
who reaches for `claims-touching` there gets no transaction overlay
either -- the same committed state the functors read (recon note B9).

## Tests and CI

FiveAM system `graph-db/rules-test` (`tests/rules/`), on-disk stores,
its own `sbcl` process -- CI lane `rules suite` in
`.github/workflows/test.yml`. Note the tripwire `docs/ci.md` records:
an image that loads `graph-db/rules` **and** `graph-db/gui-test` fails
`prolog-functor-inventory-is-pinned`, because that check is an equality
against a hand-reviewed list and the gui lane loads no rules. Classify
the rules functors there (the `claim*` and `instant*` families) if you
build such an image; never weaken the check.

## The store's rule schema (GH #331)

`graph-db.rules:def-rules-schema (graph-name)` declares the store's
`rule` record (`name version family head body extent-policy enabled`)
and the `derivation` claim family, both per store like any `def-source`
or `def-claim-classes` call. `name` is the identity key -- one live
`rule` per name -- so a new version is `copy`, `setf rule-version`,
`save`, not a second write. A second `def-rules-schema` call (a
multi-store image) rebinds `make-rule`'s default store, so every
constructor call after that must pass `:graph` explicitly.

`name` and `version` are canonical strings (`[a-z0-9-]+`) and
`extent-policy` is one of `:premises` / `:none`; both are commit-time
constraints, so a raw slot write is refused too.

A store that never evaluated it holds no rules, and says so rather than
erring: `run-rules` on such a store reports nothing, and `run-rule` by
name says there is no such rule rather than reaching an index this
schema does not carry.

Once a store has evaluated it, **`graph-db/rules` must be loaded before
every later `open-graph` of that store**: `rule` and the `derivation`
classes are this system's, and a persisted node type with no CLOS class
in the image is `schema-classes-not-loaded` at open (`schema.lisp`,
GH #144).

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
- **A bare `?` is refused, in the head as well as the body.** Read
  into the guard's scratch package every `?` in one text is the *same*
  named variable, not the engine's anonymous one, so all of them would
  have to unify. `compile-rule` refuses it rather than let a rule mean
  something other than it reads (recon note A10,
  `docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md`).
  The head is scanned separately from the body, so the "bound by the
  body" check still sees only body variables.

An effecting functor such as `retract` is **not** a compile refusal:
there is no static effect registry, so the guard admits the goal and
running it is what refuses (same note, A16).

**Generators move to the front (ruling P5).** A body goal
`(claim-producer ?v "p")` -- variable first, literal producer second --
generates from the producer index, so `compile-rule` runs it before
the rest of the body and a later `claim/7` on `?v` takes its node
route instead of walking the family. Every other goal keeps its order;
`(claim-producer ?p ?who)` is a filter and is left where it was.

**Recursion compiles; the cycle is a stratum (GH #333).** The
dependency graph is over relation names: a rule's head relation points
at every relation its body reads -- under a `not` as well -- and so
does every other enabled rule in scope, the store's enabled rules plus
every enabled `def-rule`. A path from the head relation back to itself
is no longer refused: the compiler takes the strongly connected
components and a rule's component is its **stratum** (see "Recursive
rules"). Two things are still refused, naming the rule: a `not` whose
goal reads a relation of the rule's own stratum, and a body `claim/7`
that leaves its relation unbound -- it would read *every* relation,
its own included, so that refusal says to bind the relation. The graph
is the rule's own store's, plus the image's `def-rule`s -- a `:scope`
does not widen it (see "Cross-store scope").

**A name belongs to one source.** A stored `rule` and a `def-rule` of
the same name is a collision, refused whichever arrives second.

**A `rule` write that does not compile is refused at commit** (ruling
P3). `%validate-rule-writes` sits on `graph-db:*commit-validators*`
and compiles every written `rule` against the store as the commit will
leave it -- the stratification checks included -- so the store never
holds a rule that could not run when it was written. `enabled nil` is not an
exemption: a disabled rule is compiled, only not run. The validator is
inert until some store has evaluated `def-rules-schema`, which is what
makes the `rule` class.

### `def-rule`, the in-image escape hatch

`(def-rule "web-hosts" :version "1" :family rt-claim :head ... :body
...)` registers a rule in the image rather than in a store (spec §5).
`family` is the parent class symbol, unevaluated; every other argument
is evaluated. `undef-rule` forgets one and `find-def-rule` returns its
`rule-spec`. A `def-rule` is compiled per store, when it runs, because
the strata need that store's other rules -- but it constrains the
dependency graph of every store in the image, so a `def-rule` can be
the reason a stored rule's write is refused.

## Running a rule

`graph-db.rules:run-rule (graph rule &key scope) => rule-report`
derives `rule` afresh and reconciles the result with its previous
derivation (spec §7). `rule` is a `rule` record, a `rule-spec`, or a
name -- looked up in the store first, then among the `def-rule`s.
Without a `scope`, or with one naming only `graph`, it is **one
transaction**; with another store in it the body is evaluated first and
only the reconcile is transactional (see `:scope` below). `run-rule` on
a disabled rule, or a `def-rule` whose family this store lacks, is a
`:refused` report tagged `:rule` -- on every path, whether or not the
rule is part of a recursive stratum (GH #333).

**Reconcile, not sweep-then-insert (ruling P10).** `run-rule` evaluates
the body first, then compares the identities it derived against the
claims `rule/<name>` already holds:

- an identity derived again is **kept** -- the same node, so its id and
  its version chain survive -- with its `rule-version` **and its
  validity extent** brought to what this run derives, in one
  `copy`/`save` when either moved. A kept claim's extent follows its
  premises: a premise's open end that has since closed, an extent
  change under `:premises` in a non-temporal family, or a rule moved
  from `:extent-policy :none` to `:premises`, all reach the kept claim
  rather than waiting for something to sweep it. The extent *start*
  cannot move -- the dedupe key carries it for a temporal family -- and
  a refreshed extent that overlaps a sibling run is refused at commit
  like any other;
- an identity no longer derived is **swept** (`mark-deleted`);
- an identity not held before is **derived** (constructed).

The order matters and is not a preference. `mark-deleted` releases a
unique key only *post*-durability, while `validate-unique-constraints`
runs *pre*-durability, so a sweep and a re-insert of an unchanged claim
in one transaction always collide under `def-unique` (recon note C1,
`tests/spacetime/claim-query-tests.lisp`). Deriving first and keeping
what is unchanged has no such collision, and it is sound because a body
reads the committed store either way (recon note A6).

**A sweep retracts; it never deletes** (D4 of the forward-rules
design, `docs/superpowers/specs/2026-11-04-forward-rules-design.md`
§4.2). A derivation the rule no longer derives is `retract-claim`ed
together with its `derived-from` records, so the claim stays on
record, `dependents-of` still answers through it, and
`claims-by-producer` still returns it; `claim-current-p` tells it from
a believed one. `rule-report-swept` counts these retractions. A
**retracted** derived claim whose identity is derived again -- whether
a sweep retracted it or a caller did by hand -- is **re-opened** in
place: same node, transaction period re-opened as of now, counted as
kept. The derivation is supported, so it is believed. A retracted
claim the rule still does not derive is left as it is and counted
nowhere. Note for a family with a `def-unique` over a subset of the
identity: a retracted claim still occupies its key, so a later
derivation that differs only outside that subset collides at commit
exactly as a same-run sweep-and-insert does.

**Duplicates collapse.** Two solutions with the same head endpoints and
relation are one claim; for a temporal family the extent *start* joins
that key, exactly as `claim-identity-key` does -- so two solutions that
differ only in extent **kind** (an instant at T against an interval
starting at T) collapse too, which is what the family's own identity
rule would have forced anyway (recon note C5, ruling P11).

**The rails, always (ruling P4).** The body runs through
`run-query-goals`: `:effects nil`, one snapshot -- inherited from the
open transaction on the single-store path and from the composed read
snapshots on the cross-store one -- and a resource bound.
`*rules-max-inferences*` and `*rules-timeout*` are the operator's;
`nil` on either falls back to the DSL's
`*query-default-max-inferences*` / `*query-default-timeout*`.
If the *effective* pair is `nil` -- both rule variables and both DSL
defaults -- `run-rule` signals a plain error rather than walking a
family unbounded. `*rules-max-solutions*` (100000) caps the collected
solutions; past it the run is refused rather than silently truncated.

**`:scope` -- the stores the body may read (spec §10, GH #332).** A
list of open stores. `graph` is put first whatever the caller wrote,
and a store named twice is read once: named twice it would answer every
route twice and so double every solution. The rule **writes `graph`
alone** -- neither a derived claim nor a `derivation` record ever lands
in another store. `nil`, or a scope of `graph` alone, is slice 2
exactly.

The two paths differ in *where* the body runs, and the engine forces
the difference: inside a read-write transaction on A, every read of B
signals `cross-graph-transaction-error`, snapshot or no snapshot
(GH #53).

- **`nil`, or `graph` alone.** The body is evaluated inside the write
  transaction, as slice 2 had it, and that transaction serialises the
  run against concurrent writers.
- **Another store in scope.** The body is evaluated *before* the write
  transaction, under one composed read snapshot per store in scope
  (own store first); the reconcile then runs in `graph`'s transaction
  as before. Each store is internally consistent, but the run is
  **not** serialised against a premise committed after the snapshots
  were taken -- the next run sees such a premise, this one does not.

**Call it outside a transaction.** A foreign store in `:scope` while
the caller holds a transaction of its own is an operator error,
signalled before anything is evaluated, because the engine refuses that
read whatever snapshot is in force and the run would otherwise die
part-way through with `cross-graph-transaction-error` (GH #53, ruling
S3-F1).

Under a shared system clock (`open-system-clock`, GH #168) those
snapshots take their epochs from one counter, so the epochs are
*comparable*, and equal when nothing commits between the two
acquisitions. Without a shared clock each store is consistent on its
own and the epochs are not comparable at all. The engine deliberately
provides no single instant across stores
(`call-with-read-snapshot`'s own docstring, GH #53), so neither does
this.

**The retry.** `call-with-transaction` re-invokes its thunk on a
`validation-conflict`. With a foreign store in scope only the reconcile
is inside that thunk, so a cross-store **evaluation is not repeated** on
a conflict -- the retry reconciles the same solution set. The
single-store path re-evaluates, as slice 2 did. Either way the report's
counts are per attempt and never cumulative.

A store in a scope must be **keyword-named**: `method` (below) is
`(string-downcase (symbol-name (graph-name g)))` and nothing in
`make-graph` coerces the name (recon note B5). A scope holding
something that is not an open store signals, before the rule is even
resolved.

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
| `rounds` | the fixpoint rounds run, 1 outside a recursive stratum |
| `stratum` | its stratum's rules, its own name alone for a rule with none |

Every count is a total over the run: `disjoint-premises` included,
across each variant and round of a recursive stratum.

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

The vocabulary is closed: a `constraint-violation` none of the three
family cases name is tagged `:rule`, not with its own class name.

**Nothing refuses by signalling.** Every refusal is reported and
**the previous derivation stands untouched.** For a single rule, or a
recursive stratum's single-store run, `derived`, `kept` and `swept`
all read 0 on a `:refused` report: a refusal raised inside the write
transaction unwinds it; one raised during a cross-store evaluation
unwinds the composed snapshots instead, no transaction being open yet;
the report is the same either way, and neither path wrote anything.
A recursive stratum's cross-store run is the one exception: each
round commits in its own transaction, so a refusal partway through
leaves earlier rounds' claims standing -- `derived` counts what they
wrote and `rounds` names how many, while `kept` and `swept` still read
0, since the fixpoint's reconcile (the only place either is set) is
itself inside the transaction that just unwound. Only an operator
error signals: no resource bound, no rule of that name in the store or
the image, a `:scope` that is not a list of open, keyword-named
stores, or a foreign store in `:scope` inside the caller's
transaction.

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
extent collapse to one claim. **The first solution's extent is the one
kept** (ruling T4-R3): the premises of the collapsed solutions are
unioned, and their extents are not re-intersected. Re-intersecting
would narrow a claim's validity by the accident of how many ways it was
derived, which is not what "the intersection of its premises" means for
a claim derived twice over.

## Provenance

Every (derived claim, premise) pair is one binary claim of the
`derivation` family (spec §9): subject `(:claim . <derived identity
key>)`, relation `"derived-from"`, object `(:claim . <premise identity
key>)`, producer `rule/<name>`, the rule's `rule-version`, standing
`:inferred`. Identity keys, not node ids, so provenance survives a
premise's retraction and regeneration. The records reconcile exactly as
the claims do: a pair still asked for is kept and re-versioned, one no
longer asked for is deleted. **One record per pair**: the records
`rule/<name>` holds are the rule's alone, so a second record naming a
pair already kept, or a record under that producer whose relation is
not `derived-from`, is swept with them.

**`method` names the premise's store (spec §10, GH #332).** A record
whose premise came from another store in the scope carries that store's
name -- the downcased graph name, cl-llm's `store-name` convention --
and one whose premise is in the rule's own store carries `nil`. A kept
record's `method` is **refreshed** to the store its premise now comes
from, in the same `copy`/`save` as its `rule-version`. Refreshed and
not swept-and-rewritten: `method` is *not* part of the family's
identity tuple, so rewriting a record whose pair is unchanged would
collide on `def-unique` exactly as the reconcile order above avoids
for the claims themselves.

That `method` is outside the identity tuple has a second consequence:
two stores holding one identity key contribute **one** record. The
rule's own store wins, else the first store in scope order, and the
other store's name is lost.

The reconcile never touches a node from another store. A premise leaves
the evaluation as `(identity-key . store-name)`, both computed inside
the snapshot that read it -- a node `index-lookup` returns under a
snapshot skips `ensure-node-bytes`, so reading its slots once the
snapshot has exited reads that store's heap with no read pin in force
(recon note C4).

Both reads filter the records on `derived-from`, so a `derivation`
record of another relation -- one a foreign writer left under the
producer, which the next run sweeps -- is never read as provenance.

- `(premises-of graph claim &key scope) => claims` -- the claims
  `claim` was derived from. `scope` defaults to `(list graph)`, and
  each record's `method` decides where its premise is resolved: the
  store of that name in `scope`, `graph` when `method` is `nil`, and
  **nowhere when the named store is not in `scope`** -- the premise is
  dropped, never looked for in `graph` instead, so a caller who forgets
  the scope sees fewer premises and never wrong ones. A premise whose
  identity no longer exists in the store it resolves in is dropped too,
  rather than faked. A foreign store is read here, so call this outside
  a transaction: inside one, a `scope` naming a store other than
  `graph` is an operator error, signalled before any record is read
  (GH #53, ruling S3-F1).
- `(dependents-of graph claim &key current) => claims` -- every derived
  claim whose provenance names `claim`. With `:current`, only those
  still believed. No `scope`: the records and their subjects are
  `graph`'s whatever store `claim` lives in, and a premise is named by
  its identity key.

**Retracting a premise does not re-derive anything.** Its dependents
stay current and stay findable through `dependents-of`; deciding what
to do about them is the caller's, and in a multi-agent setting
kraison/blackboard's.

## Cross-store scope (GH #332)

A **scope** is the list of open stores a rule's body may read. It is a
run-time argument, never a slot on the rule: `run-rule graph rule
:scope`, `run-rules graph :scope`, and `graph-db::*claim-scope*` for a
Lisp caller writing a raw `select`. The rule's own store goes first
whatever the caller wrote, a store named twice is read once, and `nil`
or a scope naming the own store alone behaves exactly as slice 2 did.
Every store in a scope must be **keyword-named** -- `method` below is
the downcased `symbol-name` -- and a scope holding anything else, or
anything that is not an open store, signals before the rule is even
resolved.

**A rule writes its own store, only.** Every derived claim and every
`derivation` record lands in `graph`, whatever the scope. A wider scope
changes what the body can see and nothing about where the result goes.

**Both stores declare the family.** Compile and the strata stay
single-store (spec §6): a rule's text is validated against its own
store's schema. So a family read from another store must be declared
under both store names -- `(def-claim-classes fam :store-a)` and
`(def-claim-classes fam :store-b)`; the family registry is keyed on the
family symbol, the indexes are per store. A store in scope whose schema
never declared a family a goal names contributes nothing to that goal,
while the rule's own store still refuses an ill-typed goal as slice 1
documented.

**Where the body runs depends on the scope**, and the engine forces
that: inside a read-write transaction on A every read of B signals
`cross-graph-transaction-error` (GH #53). The own store alone is
evaluated inside the write transaction; another store in scope is
evaluated before it, under one composed read snapshot per store. Under
a shared system clock (GH #168) those snapshots take their epochs from
one counter -- comparable, and equal when nothing commits between the
acquisitions -- but never one instant, which the engine deliberately
provides for no cross-store read. The full contract, with the retry and
what a cross-store run is *not* serialised against, is under "Running a
rule".

**A premise from another store is named in its record**: the
`derived-from` record's `method` is that store's downcased name, `nil`
for the rule's own store. Nothing else crosses -- a premise leaves the
evaluation as an identity key plus that name, so the reconcile touches
no foreign node. Reading provenance back takes the same scope:
`(premises-of graph claim :scope (list a b))` resolves each premise in
the store its record names and drops the ones whose store is not in
scope. See "Provenance".

**The walk reads the scope too.** A goal that routes to no index walks
the family in every store in scope, and is refused as cost-unbounded
under a resource bound exactly as it is single-store. A scope neither
adds a refusal nor removes one.

**A Lisp caller binds the special itself.** `graph-db::*claim-scope*`
around a raw `select` buys the same reads with no rule involved. The
trap is the engine's rather than this system's: bind it **outside** a
transaction, or the first foreign read signals.

**Known limit: no cross-store cycle detection.** The cycle check is
over the rule's own store, so A's rule reading a relation B's rule
derives -- and B's reading one A's derives -- is neither refused at
compile nor settled at run. Recursion within one store is handled;
see "Recursive rules" below.

## Two negations

A body has two ways to say "not there", and they mean different
things (D2 of the forward-rules design,
`docs/superpowers/specs/2026-11-04-forward-rules-design.md` §5.1):

- `(not (claim ?c family ...))` is **closed-world**: no claim of that
  shape is on record. It is stratified negation: the relation under
  it must belong to an earlier stratum, and a rule that says it of its
  own stratum is refused at compile.
- `(claim-absent ?c family sns skey rel ons okey)` is a **determined
  absence**: somebody looked and found nothing. It answers the current
  claims of standing `:searched-empty` or `:determined-empty` under
  that relation, exactly `claim/7`'s routes with a filter, and it is a
  positive read for stratification, so a rule may use it freely. The
  weak absences, `:uncovered` and `:indeterminate`, and a retracted
  absence are not answers.

Say the second when the store records searches; say the first when
silence is the only evidence there is.

## Recursive rules

A rule may read its own head relation, directly or through other
rules (GH #333). The compiler no longer refuses the cycle: it computes
**strata**, the strongly connected components of the relation
dependency graph, and `compiled-rule-stratum` /
`compiled-rule-stratum-relations` say which rules and relations a rule
runs with. Two things are still refused at compile, naming the rule
and the relation: a `not` whose goal reads a relation in the rule's
own stratum (unstratified negation has no fixpoint; a `not` over an
earlier stratum is fine), and a `claim/7` goal that leaves its
relation unbound (it would read every relation, its own included).

`run-rules` runs strata in dependency order. A stratum with a
genuinely recursive rule runs through `%run-stratum`, every rule of
the stratum together, in **rounds**:

- **A recursive rule -- one with a `claim/7` goal reading a relation
  of its own stratum through an unbound `?c` -- runs only its
  variants, round 0 included.** For each such goal, a variant answers
  it from the fixpoint's internal delta generator instead and leaves
  every other goal unchanged. Round 0's delta is empty, so a variant
  answers nothing there at no cost -- which is why an unanchored
  two-goal closure such as `t(x, z) :- t(x, y), t(y, z)` is not
  refused as cost-unbounded: neither goal ever reaches the unindexed
  scan. A rule that merely shares the stratum -- deriving one of its
  relations without reading any of them recursively -- runs its full
  body once, at round 0, and not again.
- **Round 0's delta is the stratum's base facts.** A recursive rule
  reads the delta *where its body reads the relation*, so without a
  seed a `reaches` claim somebody observed -- one no rule of the
  stratum wrote -- would be a premise for nothing and the closure
  would answer over rule-derived facts alone. Before round 0 the loop
  collects every claim of a stratum relation, in every store in
  scope, whose producer is none of the stratum's, and hands them to
  round 0 as its delta. **Current or not**, as `claim/7` answers
  them: the goal a variant substitutes has to answer what the goal it
  replaces would, or which of two recursive goals the fixpoint feeds
  would change what the rule means -- a body that wants currency says
  `claim-current`, exactly as it must of a plain read. They are
  premises like any other; they are never the producer's claims, so
  no reconcile keeps or sweeps them, and they are **not** added to
  the run's own derivation index -- nothing excludes a base fact from
  a plain read, so indexing it would answer it twice.
  The cost is one typed family walk per family the stratum derives
  into, per store, per stratum run -- the shape `claim/7`'s own
  fallback walk uses, minus the cost-unbounded refusal, this being
  the loop's own walk and not a goal a budget must preempt -- and
  that walk **records its reads**, so on the single-store path the
  whole family joins the write transaction's read set: the
  scan-and-write shape the engine warns falls back to the global lock
  under a concurrent writer (`map-vertices`, GH #92). Both belong in
  the measurement; a per-relation index would retire them
  (kraison/vivace-graph#350's sibling). Under a cross-store scope the
  walk runs under round 0's own snapshots.
- **A plain read of a stratum's own relation excludes the stratum's
  producers** (`*claim-exclude-producers*`, bound around every round,
  round 0 included). Round 0 sees base facts only, and a later
  round's plain reads still cannot answer from what the stratum wrote
  on a previous run: the fixpoint recomputes the whole derivation
  from scratch every time `run-rules` runs it, so a stale closure
  left standing from before is a premise for nothing. The exclusion
  is a list of `(producer . relation)` pairs, not of producers: a
  body reading a stratum producer's `derivation` records -- another
  family, another relation -- still sees them.
- **What exclusion removes, the run's own derivation restores.**
  `*claim-derived-this-run*` indexes this run's derivation so far --
  claims kept from before and newly constructed alike -- the same way
  `claim/7`'s subject and object routes are indexed, and unions it
  into a plain read's candidates and into `claim-producer/2`'s
  generator for an excluded producer. Only the delta's bound `?c` is
  never filtered by either mechanism. Without this a rule with two or
  more recursive goals would see the delta on whichever goal a
  variant substitutes and nothing on the others, since the delta
  generator only ever answers for the one goal position a variant
  binds -- an incomplete fixpoint.
- **A round's delta** is every identity first derived this run,
  whether constructed just now or already standing from before (kept,
  its node reused); it is written as it is found, per round, so the
  next round's delta and derived-this-run reads see it. The stratum
  stops at a round that derives nothing new. That is semi-naive
  evaluation: the answer is the same as re-evaluating everything each
  round, at a fraction of the cost. A kept identity is brought to
  this run's version and extent *before* it enters the delta, since
  it is the premise of everything a later round derives from it.
- `*rules-naive-rounds*` re-evaluates every rule in full each round
  instead of running the variants: a **debugging switch**, and a
  reference only over a closure a semi-naive run already committed.
  A from-scratch single-store naive run sees **none** of its own
  output -- it reads the relation directly rather than through the
  delta, and an index read does not see the open transaction's own
  writes -- so it is no oracle for a fixpoint it has to build.
- The sweep of claims no longer derived, and provenance, happen once
  per rule, at the fixpoint -- so a claim derived in an early round is
  never swept by a later one.

**Transactions.** A **single-store** stratum runs to its fixpoint --
every round and the final reconcile -- in **one transaction**: a
refusal in any round (the budget, a commit constraint, the rounds cap
`*rules-max-rounds*`, default 1000, tagged `:rounds`) leaves the
previous derivation standing, `derived`/`kept`/`swept` all reading 0,
exactly as for any other refused rule. A **cross-store** stratum
evaluates each round under snapshots and commits that round in its
own transaction on the own store, because a foreign read inside a
transaction is refused (GH #53) and a later round must see what the
earlier ones committed. A refusal there leaves standing whatever
rounds committed before it: `derived` counts what they wrote and
`rounds` names how many, while `kept` and `swept` still read 0 -- the
reconcile that would set them is itself inside the transaction that
just unwound.

`rule-report-rounds` is the rounds run (1 for a non-recursive rule),
`rule-report-stratum` the rules it ran with. A refusal anywhere in a
stratum's evaluation stops the whole stratum; every rule of it reports
the same `:refused` outcome, carrying whichever refusal fired first --
the report does not say which rule's own goal signalled it.

`run-rule` on one rule of a recursive stratum runs the whole stratum
and returns that rule's report -- on **any** member, the base case of
a closure included: what decides is whether the stratum is recursive,
not whether the named rule is. Deciding that costs a compile of every
member of the rule's stratum, since recursiveness is a property of the
compiled rule; a member that does not compile refuses the whole call
(above).

`select` and the guarded query surface are unchanged: a recursive
`<-` predicate there still runs top-down under the resource bounds
(tabling for that is kraison/vivace-graph#122).

## `run-rules`

`graph-db.rules:run-rules (graph &key scope) => list of rule-report`
runs every enabled rule the store can run, passing `scope` through to
each, in dependency order. `scope` is checked once at entry, so a scope
that is not open, keyword-named stores signals even on a store with no
runnable rule. The order is: a rule that reads relation R runs after
every rule that derives R (spec §7). Strata are the compiler's
components, so the order always exists; ties keep the order the rules
came in. Both the compile and that order stay single-store, so a
cycle that runs through another store's rules is not detected.

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
  refused at open. Each of the two compile refusals is the *offending
  rule's own*, so a `def-rule` registered after a store's rules were
  written refuses at most the rule that carries the fault: a `not`
  over the rule's own stratum refuses the rule containing the `not`
  (the new `def-rule` can be what pulled the negated relation into
  that stratum, but the refusal is still the negating rule's), and a
  `claim/7` goal with its relation unbound refuses the rule holding
  that goal, in a stratum or not. A `def-rule` that merely joins a
  stratum -- closing an ordinary cycle -- compiles, and every other
  rule runs regardless.
- **`run-rule` refuses a whole recursive stratum when a member does
  not compile**, where `run-rules` reports that member and runs the
  rest. The fixpoint needs every rule of the stratum: one missing is
  an incomplete answer, not a smaller one.

## Forward evaluation

Design: `docs/superpowers/specs/2026-11-04-forward-rules-design.md`
(decisions D1–D5 in its §1 are binding). Tracking: #456.

`run-rules` is pull: somebody calls it. Forward evaluation is push:
once a store opts in, every commit's claim writes are turned into a
**delta** and the rules that read what changed run from it, in the
committing thread, after the commit returns (D1: the derivation is a
follow-on transaction, not part of the committing one). Nothing calls
`run-rules`.

```lisp
(graph-db.rules:enable-forward-rules graph)    ; runs run-rules once
(graph-db.rules:forward-rules-enabled-p graph) ; => T
(graph-db.rules:disable-forward-rules graph)   ; drops the queue
```

`enable-forward-rules` registers the store and runs `run-rules` once,
so a store with premises committed before the opt-in starts at its
fixpoint. The opt-in is per open graph object and lives in the image:
reopen the store, opt in again. Disable and close do not wait for a
step running on another user thread, even though status then says disabled.
Stop the caller's committers before disabling or closing the graph.

**The delta.** `claim-delta` renders a committed transaction's writes
(every node of a registered claim family) as a table, relation →
list of `(claim . weight)`:

| write | weight |
|---|---|
| create | +1 |
| delete | −1 (the old version) |
| update that closes the transaction period (`retract-claim`) | −1 |
| update that re-opens it | +1 |
| any other update | −1 old version, +1 new version |

A claim created and deleted in one transaction nets to nothing. A
transaction that writes no claim queues nothing; one that writes a
`rule` record invalidates the compiled-rule cache.

**A step.** Strata run in dependency order, as `run-rules` runs them.
A stratum the delta does not touch -- none of its rules reads a
relation the delta names, positively or under a `not` -- is skipped.
A stratum touched only by +1 entries on positive reads runs the
**forward path**: every rule runs the variants of its body in which
one `claim/7` or `claim-absent/7` goal answers from the delta instead
of the store (the fixpoint machinery's `rule-delta/2`, now generated
for every claim goal, not only the recursive ones), no producer is
excluded, and the reconcile sweeps nothing and only adds provenance:
the step derived from a delta, not from the whole store, so a claim it
did not meet is not thereby lost. What a stratum constructed, re-opened or
retracted joins the delta the later strata see.

**Lost derivations.** A −1 entry (a deleted or retracted claim) is
handled by one of three cases, the spec's §4.2 table:

| the lost claim is read by | what runs |
|---|---|
| a non-recursive rule, positively | the **targeted path**: `dependents-of` names the derived claims whose provenance cites it; each is re-derived with its head bound and kept, re-opened, or retracted; provenance is reconciled for those claims alone |
| a recursive stratum, positively | the stratum re-runs in full through the fixpoint, with the full sweep; provenance cannot bound a closure's loss |
| any rule under `not`, or a rule reading an unbound relation (a +1 there too) | that stratum runs the **full path**, `run-rule` or the fixpoint with the full sweep |

A lost claim a stratum itself derived -- a derived claim somebody
retracted by hand -- is re-checked on the targeted path too, and
re-opened when its premises still stand (D4: the derivation is
supported, so it is believed). A retracted premise loses its
dependents only if their bodies notice: **currency is the body's
business**. A rule meant to track retraction says `claim-current` on
each premise it tracks, exactly as it must under `run-rules`; a body
that does not says that a retracted premise still supports its
conclusion, and the targeted re-derivation honours that. Lost derived
claims re-enter the step as −1 entries for the later strata, so a loss
propagates down the strata in the same step.

Each step's reports carry `rule-report-path`: `:forward`, `:targeted`
or `:full`. Each stratum commits
its own transaction, as `run-rules` does, and those commits do not
queue themselves again.

**The queue (D5).** Each enabled graph has one queue of pending
deltas. The committing thread's hook queues its delta and then
drains the queue, unless a step is already running on another thread,
in which case it returns at once and the running step takes the new
delta before it finishes. So a commit never waits on another commit's
derivation, derivations are serialised per graph, and one step may
run on a thread other than the one that committed the premise. The
caveat that follows: **do not expect a derivation the moment your
`with-transaction` returns** when other threads commit concurrently;
poll `forward-rules-status` for an empty queue, or read the derived
claims on the next commit's step. A commit made inside another
transaction (a nested `with-transaction`) queues only; the outermost
commit's hook, whatever graph it committed on and whether or not it
wrote a claim, drains every enabled graph's queue, so a body that
opened a transaction never has a step run inside it. A body that
unwinds after a nested commit leaves that delta queued until the next
outermost commit in the image, or an explicit `forward-step`. A step
that never quiesces -- an action that keeps writing what the
production reads -- is stopped after `*rules-max-rounds*` consecutive
drains that took a delta an action committed: those deltas are
dropped with a `:rounds` failure on the status, and the deltas other
threads queued meanwhile are kept and run. A foreign commit never
counts toward the bound, however steady the stream (#472).

**Failure (spec §4.4).** A rule that refuses in a step -- the rails,
a constraint, a bad instant, a functor's own refusal -- is reported
on the status and the step goes on with the next stratum and the
next delta; the commit that queued the delta was durable before the
step began and stays so. A condition that escapes the step is logged
and recorded the same way under the name `:step`.

```lisp
(graph-db.rules:forward-rules-status graph)
;; => (:enabled t :queue-depth 0 :running-p nil
;;     :failures (("web-hosts" . "rule: ...")) :timers 0
;;     :last-step-elapsed 0.004d0)
```

The last hundred failures are kept, newest first; `:last-step-elapsed`
is the wall seconds of the last `forward-step` drain, NIL before one.
`run-rules` after a failure reports the same refusal and repairs the
rest.

**Thresholds (D3).** A body may say `(now ?t)` and compare `?t` with
an instant of a premise claim: one of `claim-valid-from/2`,
`claim-valid-to/2`, `claim-recorded-at/2`, `claim-touched-at/2`,
`claim-retracted-at/2`, or `instant-offset/3` over one of them, with
`instant<`, `instant<=`, `instant>` or `instant>=`. Any other use of
`?t` is refused at compile, naming the goal: the instant a match flips
at must be a function of its premises, never of the clock. Each such
comparison is a **threshold** with a direction, a **gain** (false
before the instant, true from it: `(instant>= ?t ?d)`) or a **loss**
(`(instant< ?t ?d)`), and `compiled-rule-thresholds` lists them.

```lisp
;; "settled": a host has run web for a second
(claim ?p rt-claim "host" ?h "runs" "app" "web")
(claim-recorded-at ?p ?r) (instant-offset ?r "PT1S" ?d)
(now ?t) (instant>= ?t ?d)
```

Under the `:premises` extent policy on a temporal family, a gain sets
the derived claim's validity start to the flip instant and a loss sets
its end. When a step or a `run-rules` evaluates a threshold rule, it
also evaluates the body without its thresholds for the matches the
delta reaches and schedules a **timer** at every flip instant still
ahead. When the timer fires the match's premise claims re-enter the
step as a delta: +1 for a gain, so the rule derives; +1 and −1 for a
loss, so the targeted path retracts what no longer holds. Timers are
in memory, per enabled graph, on one thread named "forward timer";
`disable-forward-rules` and `close-graph` stop it, and
`enable-forward-rules` rebuilds the timers from the store when it runs
`run-rules`. `forward-rules-status` counts them under `:timers`.

**Productions (spec §7).** A production is a body and an action: when
the body matches, the action runs. It is the OPS5 idea on top of the
Datalog layer, and it shares the layer's machinery: a production is
compiled as a rule whose head is a unary claim of the `firing` family
(subject namespace `production`, subject key the instantiation's key,
relation the production's name, producer `production/NAME`, rule
version the production's), so forward evaluation derives, retracts
and re-opens firings exactly as it does derived claims, provenance
names each firing's premises, and `run-rules` repairs them.

```lisp
(graph-db.rules:def-production "web-watch"
  :version "1" :family rt-claim :salience 10
  :body "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" \"web\")
         (claim-current ?p)"
  :writes '("watched")
  :action (lambda (event match) ...))
```

The **instantiation key** is the premises' identity keys **in body
order**, each with its own `|` escaped, joined with `|`, hashed to
sixteen hex digits by a small FNV-1a-64 written in the rules layer
(`sxhash` is not stable across images and no hashing library is a
dependency). Body order is what makes a self-join's `(a,b)` and
`(b,a)` two instantiations, as they are two rows of the query (#471).
The firing claim is binary: subject key the hash, object namespace
`instantiation`, object key the joined keys in the clear, so a match
reads its key off the firing and keeps it after a premise is deleted.
A premise deleted and written again with the same identity is the same
instantiation: the firing is re-opened, not duplicated. The body needs
at least one top-level `claim/7` goal and at most eight premises, and
a body with neither is refused in those words (#466).

A stored production is a `production` record (`make-production`,
slots `name version family body salience writes action`), `action` a
string naming a function registered in `*production-actions*`; a
record naming no registered action, or whose body does not compile,
is refused at commit. `def-production` registers both the spec and
the action under the production's name in the image. `:action` takes a
function, or a symbol that is looked up at fire time, so an action
redefined at the REPL is the one that runs (#464).

**One stratification graph.** A production's `writes` are edges from
each written relation to its reads, so a rule reading what a
production writes runs in a later stratum, and a production that
reads what it writes, or closes a cycle through its writes, is a
`rule-compile-error`.

**The agenda.** After a step's strata have run, the firings the step
changed become agenda entries, `:added` for a derived or re-opened
firing and `:lost` for a retracted one, sorted by salience descending,
then arrival, then key, and each is fired:

```lisp
(funcall action event match)
;; match => (:key "k1|k2" :premises (claims) :bindings (("?H" . "h1") ...)
;;           :firing claim)
```

`:premises` are the premise claims that still exist, in body order;
`:key` their identity keys in body order joined with `|`, read from the
firing claim (so a `:lost` match whose premise is gone still has it);
`:bindings` every variable the body text names, in first-appearance
order, each premise variable bound to its own premise -- `(("?P" .
claim) ("?H" . "h1") ...)` -- for an `:added` match, and NIL when the
body no longer matches or a premise is gone. Nothing synthesised
appears, and a body may name `?c` or `?instantiation-key` (#466).
`(match-row match)` renders a match as `run-guarded-prolog :format
:data` renders that solution, `(values columns cells)`: columns the
camelCase field names, a node its id string; both NIL without
bindings. `(find-production graph name)` answers the stored
`production` record or NIL. An action may `with-transaction`; its commit queues a delta the running
step takes next. A write to a relation outside the production's
`writes` is recorded on `forward-rules-status`'s failures and the
write stands; so is an action that signals, and the agenda goes on.
Matches present at enable and repairs by `run-rules` get firing claims
but no `:added` delivery; consumers read the firing claims for the initial
set. A firing's history is readable through version history only: re-opening
a kept derivation overwrites its transaction extent with an open-from-now one.

**Repair.** `run-rules` is the forward mode's repair: it computes the
same fixpoint from scratch and reconciles fully, so after any step it
reports nothing derived and nothing swept. If it does not, that is a
defect in the step.

**Cost.** A step reads in proportion to what it touches, not to the
rule's standing derivation (#457): each identity the delta derives is
resolved through the subject-relation index (the claims of one subject
under one relation, from every producer), each swept or kept derived
claim's provenance through the derivation family's subject index, and
nothing enumerates the producer. The derivation itself is proportional
to the delta's solutions. On the targeted path, `*rules-max-inferences*`
bounds each dependent's query separately, so the step total is that bound
times the dependents it re-derives. `forward-rules-status`'s `:last-step-elapsed`
is the last step's wall time; `bench-forward-step-reads` in
`tests/perf/rules-bench.lisp` runs a one-claim step against two
derived-set sizes and reports the ratio (near 1). The full path --
`run-rules`, `run-rule`, and a step's `:full` mode for a recursive
stratum that lost a premise or a touched `not` -- still reads the whole
producer, which is its job. One difference from that pass: two
*current* claims of one identity under one producer (a hand-made
duplicate) are both met by the full path, which sweeps the second,
and only one by the per-key path, which leaves the other alone.
