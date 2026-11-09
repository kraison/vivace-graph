# The 4-port tracer and `explain`: design

**Issue:** GH #124, unit 3 of the Prolog Engine 5.0 scope
(`2026-11-09-prolog-engine-5.0-scope.md`). **Status:** proposed
2026-11-09; the calls are the author's, Kevin's review pending.

## Intent

The engine has one debugging aid, `*prolog-trace*`, a flag that makes
the compiler and a few functors print what they are doing. Unit 4
(tabling) will be hard to reason about without watching resolution, and
since #102 and unit 2 a query's cost depends on which access path each
goal takes, which nothing today reports. This unit gives the engine the
standard 4-port box model (call, exit, redo, fail) as a hook that costs
nothing when unset, a `trace-prolog` that prints it, an `explain` that
says per goal which access path the engine will take for that binding
pattern, and a `profile` that runs a query under the hook and counts.

## Goals

- A port hook `(port functor args depth)` fired at CALL, EXIT, REDO and
  FAIL of every compiled goal call and every dynamic meta-call, plus a
  CLAUSE event when the unit 2 dispatcher tries a candidate clause.
- When the hook is unset, a goal call costs what it costs today: the
  one special-variable read `*prolog-trace*` already costs there.
- `trace-prolog` prints the ports, indented by depth, for every functor
  or for the named ones; `untrace-prolog` stops it.
- `explain-goals` reports, statically and without running anything,
  per goal: the functor it resolves to, which arguments are bound at
  that point, and the access path the functor will take; for an
  asserted functor, how many of its clauses the first-argument index
  leaves as candidates.
- `profile-goals` runs the query under the hook and reports per
  functor the port counts and the time between its calls and fails.
- Both have a text entry through the guard, for a consumer that holds
  query text.

## Non-goals

- Ports for the inline control constructs (`=`, `not`, `once`, `if`,
  `forall`, `and`, `or`, static `call`): they compile inline, not
  through `compile-call`, and so have no call site to instrument.
  `explain` still walks into them for binding analysis.
- Spy points, leashing, interactive stepping, retry: the hook makes
  them possible later; none is built here.
- A GUI surface for `explain` or the trace: the Lisp and text entries
  are the deliverable; the workbench is a follow-up issue.
- A cost model or any reordering (#123 bullet 4 stays out).

## Today's mechanics

- `compile-call` (prologc.lisp:245) emits, per goal:
  `(%tick)`, `(when *prolog-trace* (format ...))`, then
  `(funcall func args... cont)` or the existence error.
- `%solve` (prolog-functors.lisp:~240) is the dynamic meta-call: it
  resolves the functor and `apply`s it to the arguments and `cont`.
- `trace-prolog` / `untrace-prolog` set and clear `*prolog-trace*`;
  `%edge-functor/2` and a dozen functors print under it.
- Access paths are chosen inside each functor from which arguments
  dereference to non-variables: `is-a/2` (node bound: a type check;
  type bound: the type index; neither: every type), `node-slot-value/3`
  (node bound: a slot read; unbound: every vertex), `%edge-functor/2`
  (both ends: vev lookup; from: ve-out; to: ve-in; neither: every edge
  of the type), the `find-*` index predicates (one index each),
  `claim/7` (`%claim-goal`'s `cond`, rules/facts.lisp:300: bound claim,
  subject-relation index, subject index, object index, empty namespace,
  subject or object vocabulary route, family walk), and since unit 2
  an asserted functor's dispatcher (`%candidates`).
- `%static-goal-functors` (prologc.lisp:1007) already walks a goal list
  through the control constructs to name its functors.

## Design

### D1. The port hook

`*prolog-port-hook*` (globals.lisp): nil, or a function of four
arguments, `(port functor args depth)`. `port` is `:call`, `:exit`,
`:redo`, `:fail`, or `:clause`; `functor` the `NAME/ARITY` symbol;
`args` a fresh list of the goal's arguments as passed (dereferenced by
the hook if it wants values); `depth` the number of goal calls open
above this one. For `:clause`, `args` is `(position count)`: the
candidate the dispatcher is about to try and how many it has.

`compile-call` emits

```lisp
(let ((hook *prolog-port-hook*))
  (if hook
      (%ported-call hook 'FUNCTOR func (list ARGS...) cont)
      (funcall func ARGS... cont)))
```

in place of today's `*prolog-trace*` print and bare funcall, so the
untraced path reads one special and branches, as it does now.
`%ported-call` (prologc.lisp) fires `:call`, binds `*prolog-port-depth*`
one deeper, applies `func` to the arguments and a wrapped continuation
that fires `:exit`, calls `cont`, then fires `:redo` when `cont`
returns, and fires `:fail` when `func` returns. `%solve` does the same
for a dynamic meta-call; the unit 2 dispatcher fires `:clause` before
each candidate when the hook is set (one special read per dispatch).
A traced call is not a tail call; an untraced one is, as unit 2 left it.

The `*prolog-trace*` prints inside functors stay; `trace-prolog` no
longer sets that flag, and the manual says `(setf *prolog-trace* t)` is
the compiler's own chatter, separate from the ports.

### D2. `trace-prolog`

`(trace-prolog &rest functors)` installs the printing hook on
`*prolog-port-hook*` globally: with no arguments every functor, else
only those named (`'ancestor/2`, or `'ancestor` for every arity). One
line per port to `*trace-output*`:

```
  [2] CALL ancestor/2 (alice ?3)
  [3]   CALL parent/2 (alice ?4)
  [3]   EXIT parent/2 (alice bob)
  [2] EXIT ancestor/2 (alice bob)
  [2] REDO ancestor/2 (alice bob)
  ...
  [2] FAIL ancestor/2 (alice ?3)
```

Arguments are printed dereferenced (`deref-exp`), nodes as their
printed form, with `*print-length*` and `*print-level*` bound to 8 and
3 so a long list cannot flood the stream. `:clause` prints as
`  [2]   CLAUSE ancestor/2 2/2`. `untrace-prolog` sets the hook to nil.
`call-with-prolog-ports (hook thunk)` and `with-prolog-ports ((port
functor args depth) &body)` bind the hook dynamically for one extent,
which is what `profile-goals` and tests use.

### D3. `explain-goals`

```lisp
(explain-goals vars goals &key (graph *graph*) stream)
  => a list of rows, one plist per goal in evaluation order
```

Each row: `:goal` (the form), `:functor` (the `NAME/ARITY` symbol it
resolves to by `make-functor-symbol`'s rules, or nil), `:kind` (one of
`:user`, `:global`, `:control`, `:unknown`), `:bound` (a list of
booleans, one per argument), `:path` (a string, the access path), and
for a `:user` functor `:candidates` `(k . n)`: how many of its `n`
clauses the first-argument index leaves for this call. With `stream`,
the rows are also printed as a table, one goal per line.

**Binding analysis** is static and left-to-right: a literal is bound; a
variable is bound once an earlier goal mentioned it, except that a
`not` or `forall` body binds nothing for the goals after it, and the
anonymous `?` is never bound. The walk follows `%static-goal-functors`'
rules through `and`, `or`, `not`, `if`, `once`, `forall` and a static
`call`; a goal whose head is a variable (a dynamic meta-call) is one
row with `:kind :control` and the path "meta-call, decided at run
time". `vars` are unbound at the start.

**Access paths** come from a registry: `(register-access-path
functor-symbol function)`, the function of `(bound-list args graph)`
returning the path string. Core registers:

| functor | bound | path |
|---|---|---|
| `is-a/2` | node | "type check" |
| | type only, a known type | "type index: TYPE" |
| | neither | "every vertex, type by type" |
| `node-slot-value/3` | node | "slot read" |
| | node unbound | "every vertex of every type" |
| an edge functor `E/2`, `E/3` | from and to | "vev index: one lookup" |
| | from | "ve-out index of FROM" |
| | to | "ve-in index of TO" |
| | neither | "every E edge" |
| `find-by-slot/4`, `find-by-unique/4`, `find-slot-range/5` | | "ordered index on CLASS.SLOT" (or "no such index" when the literal names none) |
| `find-within/3,4`, `find-intersects/3,4`, `find-near/5,6`, `find-nearest/5,6` | | "spatial index" |
| an asserted functor | first argument bound to a literal | "first-argument index: K of N clauses" |
| | first argument unbound or a variable | "every clause: N" |
| any functor declared cost-unbounded | | its path suffixed ", cost-unbounded" |
| a functor with no registered path | | "no path declared" |

`graph-db/rules` registers `claim/7` and `claim-after/9` with
`%claim-goal`'s own order, factored into `%claim-route`, a function
from the six bound flags (and the literal namespaces, to tell an
unknown one) to a route keyword that both `%claim-goal`'s `cond` and
the path function switch on: `:bound-claim`, `:subject-relation-index`,
`:subject-index`, `:object-index`, `:empty-namespace`,
`:subject-vocabulary`, `:object-vocabulary`, `:family-walk`, printed as
docs/rules.md's route table names them. The edge functors register
their path when `%edge-functor/2` and `/3` build them (the generator
knows the type), so every schema's edges are covered without a
per-schema step.

The `:user` row's `:candidates` is computed by the same
`%clause-index-key` and bucket lookup the dispatcher uses, on the
literal; a bound variable (bound by an earlier goal, value unknown) is
reported as "first-argument index: by value at run time, N clauses".

### D4. `profile-goals`

```lisp
(profile-goals vars goals &key (graph *graph*) stream limit
                               max-inferences timeout)
  => (values solutions rows)
```

Runs `(select ...)` on the goals under a hook that keeps, per functor,
the four port counts and the summed wall time from each `:call` to its
`:fail` (inclusive of callees, like a flat profile's "total"), and the
number of `:clause` events. `rows` is one plist per functor sorted by
time; with `stream`, a table. The select runs with `:effects nil` and
`:snapshot t`, the guard's rails, so a profile never writes.

### D5. The text entries

`graph-db.query:explain-text (text graph &key stream)` and
`profile-text (text graph &key stream limit max-inferences timeout)`
screen, read and guard `text` with `guard-query-text` and call the
two above on the result, so a consumer holding query text (the
blackboard, the GUI later) gets the same answer the engine gives the
Lisp caller, under the same refusals.

### D6. Cross-implementation

Plain CL throughout: a special, a few closures, `get-internal-real-time`.
No new reader conditional.

## Tests

In a new `tests/tracer-explain-tests.lisp` of the main suite:

- **Ports.** `(ci-g 1) (ci-g 2)` asserted; a hook that records
  `(port functor depth)` for `(select-flat (?x) (ci-g ?x))` sees
  exactly `CALL, EXIT, REDO, EXIT, REDO, FAIL` for `ci-g/1`, depth 0;
  a nested call shows depth 1 inside. Removing the `:redo` firing (the
  mutation check) must fail it.
- **Dynamic meta-call** ports: the same sequence through `(call ?g)`.
- **Clause events:** two candidates give `(0 2) (1 2)`; a keyed call
  that the index narrows gives one.
- **`trace-prolog` filtering:** with `'ci-g` named, `is-a/2`'s ports do
  not print; the output has the `[depth] PORT functor` shape.
- **No hook, no change:** `*prolog-port-hook*` nil and the k-hop bench
  row unchanged within noise (measurement, not a test).
- **`explain-goals`** rows for: `is-a` in its three forms; an edge
  functor in its four; `node-slot-value` both; `find-by-slot` with a
  real and a missing index; an asserted functor keyed, unbound, and
  bound-by-earlier-goal; an unknown functor (`:unknown`, no error);
  binding through `not` (the variable is unbound after); a dynamic
  meta-call row.
- **`claim/7` routes** (rules suite): the eight routes from the bound
  pattern, and `%claim-route` agreeing with what `%claim-goal` then
  does (a hook-counted run of each pattern reaches the route's reader).
- **`profile-goals`:** counts match the ports test; time is a
  non-negative number; `solutions` equals `select`'s.
- **Text entries** (query suite): `explain-text` on a guarded text
  gives the same rows as `explain-goals` on `guard-query-text`'s
  output; a refused text signals `prolog-guard-error`.

## Measurement

The unit 1b bench before and after, third run on odm: `k-hop-3-select`
and `clause-bound-first-arg` (the untraced call path), expected
unchanged; `guarded-shape-repeat` unchanged. One traced measurement for
the record: the same k-hop query under `trace-prolog` to a null stream,
to put a number on what tracing costs when on.

## Acceptance (#124)

- A 4-port trace of a recursive query readable without reading the
  compiler: the manual's example is `ancestor/2` over three facts, the
  trace in the text.
- `explain` reports the chosen access path per goal, so index use is
  confirmable: a `find-by-slot` goal says which index; an edge goal
  says which adjacency index; a `claim/7` goal says which route.
- Full SBCL suite green in CI.

## Left to the builder

- Whether `%ported-call` takes the argument list consed at the call
  site or receives the arguments spread and conses inside; either is
  only on the traced path.
- The table layout of `explain-goals`' and `profile-goals`' printed
  form.
- Where `register-access-path` for the edge functors lives
  (`%edge-functor/2` itself or the schema registration that calls it).
