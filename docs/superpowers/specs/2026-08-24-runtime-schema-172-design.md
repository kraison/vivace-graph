# Runtime schema from persisted metadata (#172) — unit design

Unit 7 of the namespaces epic (#110), the last build unit. Implements
`2026-08-20-namespaces-design.md` §3.5 (decision D6). The developer
experience is fixed by `docs/runtime-schema-example.lisp` (Kevin-approved
2026-08-24, including open points A/B/C) — this spec argues from that
example; where they disagree, the example wins unless a ruling below
says otherwise. Public repo: neutral names.

## 1. What already holds

- `node-type` metas persist everything a `defclass` needs (name,
  parent-type, slots, package, constructor name, keep-revisions), but
  `instantiate-node-type` assumes `(find-class name)` exists — the
  engine cannot rebuild a class from disk.
- `def-node-type` expands to: `defclass` (metaclass `node-class`) +
  `defun`s for `make-/lookup-/-p` + two `def-global-prolog-functor`s
  for edges + registration into `*schema-node-metadata*`. The functor
  macro just installs into `*prolog-global-functors*` — a runtime path
  can install closures the same way.
- Value constraints (`def-value-constraint`) are already pure data
  (`:one-of` lists, `:required`) — no closure crosses the boundary
  today. The function-by-name seam is therefore built for *new*
  function-valued options, not to retrofit existing ones.
- Placement, adoption and the occupancy hint (#167) apply to any
  registered type; runtime types ride them unchanged.

## 2. Rulings

**R1 — One installation path for source and runtime types.**
`def-node-type` is refactored so its expansion becomes: the literal
`defclass` form (kept — the compiler must see the class) plus a call to
a new shared functional core, `%install-node-type (meta)`, which does
everything else — helper functions installed via `(setf fdefinition)`
closures (constructor, lookup, predicate), edge Prolog functors
installed as closures into `*prolog-global-functors*`, registration,
instantiation into an open default store. The runtime path is then
`ensure-class` (metaclass `node-class`, accessors/initargs derived from
the same slot-spec normalization) + the *same* `%install-node-type`.
Equivalence between a source-defined and a runtime-defined type is a
property of sharing the path, pinned by a test, not a promise.
*Cost if wrong:* generated helpers become closures instead of compiled
`defun`s for source types too — they only parse args and call
`make-vertex`/`make-edge`, so the cost is negligible; if profiling ever
disagrees, the macro can go back to emitting `defun`s while still
delegating their bodies to shared builders.

**R2 — The system manifest (approved point A).**
`schema-manifest.dat` lives beside the type registry under
`*system-directory*`, append-only, one readable line per record,
printed with `*package*` = CL (symbols package-qualified), read with
`*read-eval*` NIL, torn/malformed tail lines skipped. Two record kinds:
`(:namespace NAME :nicknames (...) :time T)` and
`(:type NAME :kind :vertex|:edge :parents (...) :slots (...)
:default-store S :keep-revisions K :provenance :source|:runtime
:time T)`. Appended by `%install-node-type` (both paths — so the
manifest describes the WHOLE schema, giving `describe-schema` its
`[source]`/`[runtime]` provenance) and by `ensure-namespace`.
Last-record-per-name wins on read. No system directory ⇒ manifest
skipped, fail-safe: runtime types then live for the session only and
`materialize-schema` of that directory later finds nothing — the same
in-image-only degradation as the occupancy sidecar. A failed append
must never abort the definition (guarded like #167's sidecar).
*Cost if wrong:* the manifest duplicates what stores' schema.dat hold;
divergence is impossible for slots (same meta object writes both) and
tolerable for provenance (advisory).

**R3 — `materialize-schema` (the load-order answer).** A macro wrapping
`eval-when (:compile-toplevel :load-toplevel :execute)` around a
functional core: read the manifest at DIR; `ensure-namespace` every
namespace record; for each type record whose class does NOT exist,
build it via the runtime path; a class that already exists (defined by
source earlier in the load) is left alone — **source wins** — with the
#196 divergence warning when slot sets disagree. Restart never
evaluates data: inputs are plists, outputs are MOP calls. Collect every
schema-function name the manifest references that is unregistered and
signal ONE `materialize-unresolved-functions` error naming all of them
(approved point C: fail fast at materialize, not first write).
Idempotent; `:namespaces` keyword narrows. Returns a summary plist
(counts by provenance) for the REPL.
*Cost if wrong:* fail-fast means an image that never touches the
constrained type still refuses to materialize — deliberate (a deploy
missing behaviour its schema names is broken, loudly).

**R4 — Runtime definition API.** `ensure-namespace (name &key
nicknames)` → package, no files, no store, idempotent, manifest-logged.
`create-vertex-type` / `create-edge-type (name slot-specs &key parents
default-store keep-revisions)` → finalized class; NAME a symbol or
"PACKAGE:NAME" string (package must exist — `ensure-namespace` first;
a missing package is an error, not an implicit creation). Redefinition
of an existing name — runtime OR source-defined (approved point B) —
follows CLOS redefinition semantics with the #196 warning on slot
divergence, exactly like re-evaluating `def-vertex`. `default-store`
defaults to NIL meaning "no default store": constructors then REQUIRE
an explicit `:graph` (`default-store-not-open-error` names NIL) — a
runtime type need not commit to placement at creation.
*Cost if wrong:* the no-default-store shape is new; if it confuses, a
required `default-store` is a one-line tightening.

**R5 — The behaviour boundary.** `register-schema-function (name fn)` →
name, into an image-level EQ registry; `find-schema-function (name)` →
fn or NIL. The only v1 consumer: a slot option `:check FN-NAME` on
runtime slot-specs (and accepted by `def-vertex` slot-specs for parity)
that registers a function-backed value constraint enforced where
`validate-value-constraints` already runs, resolving the name at check
time but *verified present* at materialize (R3) and at
`create-*-type` time. The metadata stores only the name. No runtime
`defmethod`, no persisted closures, no engine loading of exported
source, no type deletion in this unit.
*Cost if wrong:* `:check` overlaps `def-value-constraint`'s territory;
kept minimal (one option, one enforcement point) so a later
consolidation is a refactor, not a migration.

**R6 — Visibility tooling.** `describe-schema (&key namespace store
since (stream *standard-output*))` — plain-text dump from the manifest
joined with live metas: grouped by namespace, per-type provenance tag
and timestamp, slots with types and `:check` names; `:since` filters by
record time (the dump doubles as a change log). `export-schema-source
(path &key namespace store)` — writes a generated-header comment,
`defpackage`+`in-package`, and literal `def-vertex`/`def-edge` forms
reconstructed from metas; loading the file is the ordinary source path
and therefore idempotent. Export never runs implicitly and the engine
never reads the file back. An Emacs wrapper is out of scope (the text
dump is SLIME-usable as-is); noted as follow-up.
*Cost if wrong:* none structural — both are read-only views.

**R7 — Scope guard.** Not in this unit: type deletion/retraction;
runtime `def-view`/index/unique definition (their macros stay
code-side; follow-ups if wanted); high-cardinality shared-type-id
types (spec §3.5's "note on scale"); the Emacs mode; peer-wire
changes (the type table already carries qualified names — a runtime
type replicates like any other once instantiated in a store).

## 3. Acceptance mapping (issue → here)

| Issue acceptance | Where |
|---|---|
| Runtime type survives restart, materialises as live class | R2+R3 |
| Restart performs no load/eval/read-with-eval of schema | R3 (plists → MOP; `*read-eval*` NIL) |
| Function resolves by name, fails cleanly when unregistered | R5 + R3's one-error-naming-all |
| Namespace creation allocates no files, no store | R4 |
| Full suite green | branch gate |
| Example-program promises (describe/export/asd flow) | R3, R6; example updated if any form drifts |

## 4. Deliverables

1. `%install-node-type` refactor of `def-node-type` (R1) — no
   behaviour change for source types; equivalence test.
2. `runtime-schema.lisp`: manifest read/write (R2), `ensure-namespace`,
   `create-vertex-type`/`create-edge-type` (R4),
   `register-schema-function`/`find-schema-function` + `:check` (R5),
   `materialize-schema` (R3).
3. `schema-tools.lisp`: `describe-schema`, `export-schema-source` (R6).
4. Tests (`tests/runtime-schema-tests.lisp`): create → write → close →
   NEW image simulated by dropping the classes/packages → materialize →
   class exists, methods compile (`compile` a defmethod form), data
   reads back typed; source-wins + divergence warning; unresolved
   `:check` name → one error naming it; no-manifest degradation;
   describe/export round-trip (export → load → same registry ids);
   torn manifest tail; edge functors work for a runtime edge type.
5. Docs: manual section (developer workflow = the example's three
   sessions), CHANGELOG, example file updated to match shipped
   signatures, epic spec §3.5 Built note + §11 unit 7 → Done.
