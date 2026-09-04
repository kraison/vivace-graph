# graph-db/query: the guarded query subsystem — design

Issue: #322 (both findings). Status: approved in discussion 2026-09-03.

## 1. Purpose

The engine has two reviewed ways to run untrusted query text under its
rails — the JSON pattern DSL (#44, #278) and the free-text Prolog guard
(#279) — and both sit above `graph-db/core`: the DSL in the web system
because of one ningle line, the guard inside the GUI. A consumer that
wants to hand a language model a bounded query tool depends on core and
`graph-db/spacetime` and must not pull a web stack (kraison/cl-llm#14
unit 2, kraison/cl-llm#23); today it loads `graph-db/gui` and calls
four internal symbols.

This unit homes both in a web-free subsystem, exports one runner that
returns data, and fixes the head-resolution defect that made the guard
depend on a single package binding.

## 2. Systems and files

- New system **`graph-db/query`**, `:depends-on (:graph-db/core)`,
  `:pathname "query/"`, components `("dsl" "guard")`, serial.
  - `query/dsl.lisp` is `query-dsl.lisp` moved, minus the ndjson
    header line (§5). Package `graph-db`, every export unchanged.
  - `query/guard.lisp` is the guard pipeline moved out of
    `gui/prolog.lisp`: the screen, the readtable and reader, the
    functor whitelist and control-word table, the schema-name table,
    the guard walk, the non-finite screen, the ill-typed classifier,
    the guarded runner, and the schema-type-names helper from
    `gui/api.lisp`. Package `graph-db.query` (§3).
- `graph-db` (the web system) depends on `graph-db/query` and drops its
  `query-dsl` component. `rest.lisp` is unchanged except §5.
- `graph-db/gui` reaches the subsystem through `graph-db`.
  `gui/prolog.lisp` keeps the flag, the request-body helpers, the
  envelope and the handler, and calls `graph-db.query:run-guarded-prolog`.
  `gui/api.lisp` calls `graph-db.query:schema-type-names`.
- `graph-db/core` is untouched except §6.

## 3. Package and exports

`graph-db.query` uses `cl` and imports what it needs from `graph-db` by
name. Exports:

| symbol | what |
|---|---|
| `run-guarded-prolog` | §4 |
| `prolog-guard-error`, `prolog-guard-error-reason` | a refusal, with the client-facing reason |
| `prolog-ill-typed-error` | a shape client input is known to produce |
| `prolog-server-fault` | anything else; the report is logged, never returned |
| `*prolog-max-query-length*` (4096), `*prolog-max-depth*` (32) | the screen's limits, unchanged |
| `schema-type-names` | `(graph parent)` → sorted class-name symbols for `:vertex` or `:edge` |

The DSL's exports stay where they are, in `graph-db`:
`compile-pattern-query`, `run-query-goals`, `run-pattern-query`,
`def-query`, `query-param-error`, the three `*query-default-*`
specials, `query-results->json`, `decode-dsl-json`.

## 4. The exported runner

```lisp
(run-guarded-prolog text graph
                    &key limit max-inferences timeout (format :data))
  => (values columns rows truncated-p)
```

- **Pipeline**, unchanged in order and in what it admits: screen the
  raw characters (`%scan-query-text`), read into a per-call scratch
  package that uses nothing (`%read-query-forms`), screen non-finite
  numbers, guard and rebuild the forms from canonical symbols
  (`%guard-query`), run through `run-query-goals` with `:effects nil`,
  one snapshot, the bounds. The scratch package is deleted in an
  `unwind-protect` on every path.
- **Budgets.** `max-inferences` and `timeout` bind
  `*query-default-max-inferences*` and `*query-default-timeout*` for
  the call; absent, the specials' values apply. `limit` is clamped to
  `*query-default-limit*` (1000); the runner asks for one row past the
  cap and reports `truncated-p` T when it arrives, the GUI's existing
  probe rule, now in one place.
- **Columns** are the query's variables in first-appearance order, as
  camelCase wire strings without the `?` (`%query-var-field`).
- **Rows** are lists, one per solution, in solution order.
  - `:data`: every cell through `%query-value->json`, so a node is its
    id string and every other value is a string, a number, `t` or NIL.
    NIL is an unbound variable or an empty slot; the two are not
    distinguished, as today.
  - `:raw`: the values Prolog bound, nodes included. For in-image Lisp
    callers only; nothing raw crosses a wire.
- **Conditions**, the GUI's contract moved verbatim: `prolog-guard-error`
  for a refusal; the engine's own reviewed `prolog-error` family and
  `query-param-error` pass through with their messages; a condition
  shape client input is known to produce (`%ill-typed-condition-p`)
  becomes `prolog-ill-typed-error`; any other `error` becomes
  `prolog-server-fault`. The latter two are logged under distinct
  labels with the original report; neither carries it.
- **Package.** The runner no longer passes `:package` to
  `run-query-goals` for head resolution — §6 makes the guard's
  canonical heads self-resolving. `%schema-package` stays for the DSL
  path only.

The GUI's `%run-guarded-prolog` becomes: `run-guarded-prolog` with the
request's limit and `:data`, the columns and rows encoded with
`query-results->json`'s row shape, and that JSON handed to
`%query-envelope` exactly as the runner's string was before. Its HTTP
behaviour — status codes, the fixed fault text, the 403 flag — is
unchanged, and the GUI suite is the proof.

## 5. The ndjson line

`emit-query-results`' `:ndjson` arm stops setting the content type; it
returns the newline-delimited text. `run-pattern-query` is unchanged.
Two callers ask for ndjson and both set
`Content-Type: application/x-ndjson` on ningle's response themselves,
through one shared helper (`%set-ndjson-content-type`), after their
query has produced its text so an errored or refused query never gets
an ndjson-labelled error body: the REST `/query` route (when the
decoded request's `format` is `ndjson`) and `def-query`'s generated
route (when the client's `?format=ndjson`). Same wire behaviour; the
REST tests that check the header are the proof.

## 6. Head resolution in core (#322, second finding)

`compile-call` (`prologc.lisp`) derives the functor symbol `NAME/ARITY`
through `make-functor-symbol`, which interns into `*package*`. A goal
list whose canonical heads live in two packages — the engine's for
global functors, a schema's for its edge functors — therefore cannot be
compiled under one binding, which is why the GUI binds the schema
package and a tenant whose schema package does not use `graph-db` had
to bind `graph-db` and refuse edge-typed stores (kraison/cl-llm#14
unit 2).

Change (revised after review: a rebind-`*package*` first attempt
broke every built-in whose name collides with a `COMMON-LISP` symbol
— `>`, `atom`, `write`, ... — since such a symbol's home package is
always `COMMON-LISP`, never `graph-db`, so per-symbol-package
rebinding can never find the engine's functor for them):
`make-functor-symbol` resolves by LOOKUP before interning — an
already-registered functor in the head symbol's own package, then in
`graph-db` (where `def-global-prolog-functor` registers built-ins
under their literal name, so the `>`/`atom`/`write` case is found
there) — and only interns into `*package*` when neither lookup hits.
Definitions keep the old rule exactly: `<-` (`add-clause`) passes
`make-functor-symbol` a `:define t` argument that skips both lookups
and interns straight into `*package*`, because a lookup there would
let a schema-package clause silently land on an existing `graph-db`
functor of the same name instead of defining its own. `compile-call`
passes the head through unchanged.

Consequences, argued once here:

- A head read at a REPL already lives in `*package*`, so REPL `select`
  behaves exactly as before, including its known requirement to be in
  `graph-db` or a package that uses it.
- A functor defined with `<-` in a user package is called from that
  package by symbols read there; unchanged, now guaranteed by
  `:define t` rather than incidentally by a shared `*package*`.
- The guard's rebuilt heads — `graph-db::is-a`, `schema::follows` —
  each resolve in their own home (falling back to `graph-db` for a
  CL-inherited comparison or `write`/`atom`/etc.). The GUI's
  `:package` binding becomes redundant for the guard and is removed
  there; `run-query-goals` keeps the keyword because
  `%dsl-resolve-type` and `%compile-match-pattern` use it for type
  names.
- `prolog-compiler-macro` still interns a string head into `graph-db`
  by name; the guard refuses string heads before it, as today.

## 7. Tests

- New system `graph-db/query-test` (`tests/query/`), FiveAM, on-disk
  graphs under the scratch parent, run by `asdf:test-system
  :graph-db/query-test`:
  - each screened token refused with the named reason, and the same
    text minus the token accepted (non-vacuity);
  - an unregistered functor, a package-qualified head, a string head
    refused;
  - `limit` clamped and `truncated-p` T with one row past the cap,
    NIL at the cap;
  - a `max-inferences` breach surfaces as `prolog-resource-error`;
  - `:data` cells are strings, numbers, `t` or NIL and a node is its
    id; `:raw` returns the node;
  - the scratch package is gone after a refusal and after a success;
  - **the motivating case**: a schema whose package does not use
    `graph-db`, with a vertex type and an edge type, queried with a
    global functor and the edge functor in one goal list, returns rows.
    This test fails before §6 and passes after.
- `graph-db/gui-test` and `graph-db/test` unchanged; they are the
  regression net for the move and for §5.
- CI (`.github/workflows/test.yml`) runs the new suite; `docs/ci.md`
  lists it.

## 8. Consumers

- kraison/cl-llm `cl-llm/agent/prolog`: depend on `graph-db/query`
  instead of `graph-db/gui`, call `run-guarded-prolog` with `:data`,
  drop the JSON parse and the edge-typed-store refusal. Filed there
  when this lands.
- kraison/blackboard slice 1 and 3: `select` and the structure tier
  build on the exported runner and `schema-type-names`.

## 9. Docs

`docs/guarded-query.md` (the exported API and the condition contract,
with the head-resolution rule); CHANGELOG `[Unreleased]` — Added for the
subsystem and runner, Fixed for §6; `docs/ci.md`; a pointer from
`docs/superpowers/specs/2026-08-27-vg-gui-v1-design.md`'s Prolog
section and from `docs/spatiotemporal-substrate-programme.md`'s related
issues.

## 10. Out of scope

What the guard admits (the whitelist, the exclusions, the control
words); the DSL's JSON schema; REST routes; the GUI's frontend.
