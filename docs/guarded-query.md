# graph-db/query: running untrusted query text safely

`graph-db/query` (GH #322) is a web-free ASDF subsystem,
`:depends-on (:graph-db/core)` only, holding the two reviewed ways to
run query text an operator did not write: the JSON pattern DSL
(package `graph-db`, `query/dsl.lisp`, GH #44/#278) and the free-text
Prolog guard (package `graph-db.query`, `query/guard.lisp`, GH #279).
Both used to sit above `graph-db/core`, never below it, but not in
the same place: the DSL lived in the web system (`rest.lisp`, then
`query-dsl.lisp`), kept out of core by one ningle line setting an
ndjson content type; the guard lived inside `graph-db/gui`. Either
way, any consumer that only wanted a bounded query tool -- not a GUI,
not ningle -- had to load a web stack to reach them. A tenant that
hands a language model a query tool depends on `graph-db/core` and
`graph-db/spacetime` and must not pull one in (kraison/cl-llm#14 unit
2); homing both here is what makes that possible.

## Loading it

```lisp
(ql:quickload :graph-db/query)
```

Pulls in `graph-db/core` and nothing web-facing. The exported guard
API lives in `graph-db.query`; the DSL's exports (`compile-pattern-query`,
`run-query-goals`, `run-pattern-query`, `def-query`, and friends) stay
in `graph-db` as before.

## `run-guarded-prolog`

```lisp
(run-guarded-prolog text graph &key limit max-inferences timeout
                                     (format :data))
  => (values columns rows truncated-p)
```

Screens `text` character-by-character before the reader ever sees it,
reads it into a per-call scratch package that uses nothing, walks and
rebuilds the resulting forms out of canonical, whitelisted symbols, and
runs the result through the DSL's own runner (`:effects nil`, one
snapshot, the inference/time/row bounds) -- all before deleting the
scratch package in an `unwind-protect`, on every exit path including a
refusal.

- **Columns** are the query's `?variables` in first-appearance order,
  as camelCase wire strings without the `?` (`?min-age` becomes
  `"minAge"`, via the DSL's `%query-var-field`) -- the same spelling
  the GUI's wire format already used.
- **Rows** are one list per solution, in solution order, shaped by
  `format`:
  - `:data` (the default): every cell through `%query-value->json` --
    a node becomes its id string, everything else a string, a number,
    `t`, or `nil`. `nil` covers both an unbound variable and an empty
    slot; the two are not distinguished. Example:

    ```lisp
    (run-guarded-prolog
     "(is-a ?i qt-item) (node-slot-value ?i label ?l)" graph)
    => (values ("i" "l")
               (("N1a2b3c..." "a") ("N4d5e6f..." "b") ("N789abc..." "c"))
               nil)
    ```
  - `:raw`: the values Prolog actually bound, nodes included. For
    in-image Lisp callers only -- nothing raw is meant to cross a wire.
    Example:

    ```lisp
    (run-guarded-prolog "(is-a ?i qt-item)" graph :format :raw)
    => (values ("i") ((#<vertex qt-item ...>) (#<vertex qt-item ...>)
                       (#<vertex qt-item ...>))
               nil)
    ```

**Budgets and truncation.** `limit` is clamped to
`*query-default-limit*` (1000). `max-inferences` and `timeout` bind
`*query-default-max-inferences*` and `*query-default-timeout*` for the
call; left out, the specials' current values apply. Below the cap,
the runner asks the DSL for one row past it: `truncated-p` is `t`
when that extra row shows up (more solutions exist than were
returned) and `nil` when the result set exactly fills or falls under
the cap. At the ceiling -- `limit` omitted or `>=
*query-default-limit*` -- there is no room for the extra row
(`%probe`), so an exactly-full page there reads as `truncated-p` `t`
too; only a page short of 1000 rows is `nil` at the ceiling.

## The condition contract

| what happened | signaled as | caller sees | logged |
|---|---|---|---|
| guard refusal (bad char, unregistered functor, qualified or string head, ...) | `prolog-guard-error` | `prolog-guard-error-reason`, names the token, client-safe | no |
| engine's own reviewed conditions (`prolog-error` family, `query-param-error`) | passed through unchanged | the original condition and report | no |
| ill-typed goal arguments | `prolog-ill-typed-error` | fixed text, "ill-typed query" | yes, "ill-typed query" label |
| anything else -- an engine defect | `prolog-server-fault` | fixed text, "internal error" | yes, "UNEXPECTED SERVER FAULT" label |

`prolog-ill-typed-error` and `prolog-server-fault` deliberately carry no
detail: the conditions they stand in for can report engine internals --
a store's keyword name, a generic-function name, an ANSI section
reference -- to a client that is, on the free-text surface, otherwise
unauthenticated. The detail goes to the log, never to the caller.

## Head resolution (GH #322, second finding)

A goal's head resolves by lookup before it interns anything: an
already-registered functor in the head symbol's own package first,
then in `graph-db`, where the engine's built-ins are registered under
their literal name -- which is why a CL-inherited head like `>`, `atom`,
or `write` (whose home package is always `common-lisp`, never
`graph-db`) still finds the engine's functor. This is what lets one
goal list name a global functor and a schema's own edge functor
together, since each now resolves in its own home instead of a single
shared `*package*` binding. Only `<-` (`add-clause`) skips both lookups
and interns straight into `*package*`, via a `:define t` argument, so a
schema-package clause defines its own functor rather than silently
landing on an existing `graph-db` functor of the same name; the guard's
rebuilt heads (`graph-db::is-a`, `schema::follows`, ...) each take the
lookup path.

## What the guard admits

The whitelist, the exclusions, and the control-word list are derived
from the live image, not hand-listed here -- see `query/guard.lisp`'s
header and the block comments above `*prolog-excluded-predicates*`,
`*prolog-goal-argument-control*`, and `*prolog-cost-unbounded-predicates*`
for exactly what is admitted and why each exclusion exists.

Two things a new caller reliably gets wrong first:

- **Type and slot names are bare, never keyword-spelled.** Write
  `(is-a ?x qt-item)`, not `(is-a ?x :qt-item)`. The character screen
  refuses every `:` outright, before the reader runs -- a
  keyword-spelled type name is a refusal, not a match.
- **Package-qualified anything is refused for the same reason:** the
  screen refuses `:` textually so that `READ` can never intern into
  `graph-db` or a schema package from client text.

## Callers

The GUI's free-text Prolog workbench (`gui/prolog.lisp`, behind
`START-GUI`'s `:allow-prolog` flag) is a caller: it calls
`run-guarded-prolog` with the request's limit and `:data`, and wraps
the result in its JSON envelope. The REST `/graph/:graph-name/query`
route is a caller of the DSL side of this subsystem, running the JSON
pattern DSL (`run-pattern-query`) rather than free-text Prolog -- it
does not go through `run-guarded-prolog`.

## Consumers outside this repo

kraison/cl-llm's `cl-llm/agent/prolog` query tool re-homes onto
`graph-db/query` (filed at landing); kraison/blackboard slice 1's
`select` and slice 3's structure tier build directly on
`run-guarded-prolog` and `schema-type-names`.
