# Packages as namespaces (#167) — unit design

Unit 2 of the namespaces epic (#110), the last build unit. Implements
`2026-08-20-namespaces-design.md` §2, §3.1–3.3, §4 (decisions D1, D3,
D4, D7) against the code as it stands after units 1a/1b/3–6. Public
repo: neutral names throughout.

## 1. What already holds

- Type-ids are system-wide (#186); `%check-node-class-graph-unique` is
  **already deleted** — `def-node-type` carries the comment saying so,
  and `divergent-node-type-redefinition` (#196) is the remaining guard.
  This unit's "delete the check" bullet is done; what remains is the
  *placement* half.
- `def-node-type` already records `:package (package-name *package*)`;
  only `rest.lisp:507` reads it today.
- `*schema-node-metadata*` is an EQUAL hash keyed by the trailing
  `graph-name` argument, holding ordered meta lists; `update-schema`
  instantiates exactly the metas under the graph's own name.
  **~45 test fixtures reset it by that key** — the keyed shape is a de
  facto seam.
- `make-<type>` constructors default `:graph` to `*graph*` — precisely
  the "omit it and you silently get `*graph*`" behaviour §4 rejects.
- A store's `schema.dat` persists whatever types were instantiated into
  it and re-instantiates them on open, independent of the metadata
  hash's keying.

## 2. Rulings

**R1 — Constructor placement: explicit `:graph` > the class's default
store (must be open) > refuse.** `make-<type>`'s `:graph` default stops
being `*graph*` and becomes the open graph registered under the class's
declared store name. If that store is not open and no `:graph` was
passed, signal `default-store-not-open-error` naming class and store —
never fall back to `*graph*` silently, per §4's explicit rejection.
One-to-one code is unaffected: there, `*graph*` *is* the declared
store. `lookup-<type>` and the generic `make-vertex`/`make-edge`
(`:generic`) keep their current `*graph*` behaviour — they take ids or
explicit types, not class policy. *Cost if wrong:* multi-graph code
that deliberately wrote a class into `*graph*` ≠ its declared store now
gets an error or the declared store instead of the ambient graph — a
visible behaviour change, called out in CHANGELOG as breaking.

**R2 — `*schema-node-metadata*` keeps its hash-by-store shape; its
key's *meaning* changes from ownership to default store.** The spec
sentence "stops being keyed by graph-name" is implemented semantically,
not literally: the trailing argument now *is* a store attribute (the
write default), so keying registration by it loses nothing — a class is
no longer *owned* by that key, and nothing refuses instantiating it
elsewhere. Literal re-keying (flat list by class symbol) would churn
~45 test fixtures and every reset seam for zero observable gain.
Same-named classes in two packages are distinct symbols and already
coexist under any keying. *Cost if wrong:* an internal reshape later;
no on-disk or API surface depends on the hash's shape.

**R3 — A store learns a foreign type lazily, at first write, and keeps
it durably.** When a constructor targets a store whose schema lacks the
type (`lookup-node-type-by-name` miss), the engine finds the registered
meta by class symbol across the metadata hash, instantiates it into the
target under the schema lock (`instantiate-node-type` + `save-schema`),
and proceeds. Thereafter the type is in that store's `schema.dat` and
reopens normally — `update-schema` itself is unchanged (it still
instantiates the metas registered under the store's own name; foreign
types come back from the store's persisted schema, not from the hash).
A miss with *no* registered meta anywhere stays the current error.
*Cost if wrong:* first write into a foreign store pays one schema save;
concurrent first-writers serialize on the schema lock.

**R4 — Edges place by their own class default; the occupancy set is an
image-level sidecar of the type registry.** No edge-specific placement
code exists to change — R1 covers edges because `make-<type>` for an
edge class flows through the same default. The **store-occupancy set**
(spec: "maintain on write, the lookup hint for cross-store edge
queries") is maintained at the moment an edge class is instantiated
into a store — exactly the first-write event, one record per
(class, store) pair, appended to `edge-occupancy.dat` beside the type
registry, `*read-eval*` NIL on read, tolerant of a torn tail by the
same rule as the registry files. Exposed as
`edge-type-stores (name) → list of store names, or NIL for unknown` —
NIL means "no hint, sweep everything", so a lost or stale file costs a
wasted lookup, never a wrong answer. When no system directory/type
registry is configured, occupancy is maintained in-image only (same
fail-safe). **No query API is rewired in this unit** — the consumer is
the ontology/query work (#109); this unit delivers the maintained set
and the accessor. *Cost if wrong:* if a consumer needs per-store
counts or removal records later, the append-only format grows fields —
additive.

**R5 — `node-type-graph-name` keeps its name and its persisted form;
its documented meaning becomes "default store".** `schema.dat` files
and the peer wire (#206 type-table rows are name/supers only) are
untouched; renaming the accessor would be gratuitous churn on a
persisted struct. Docstrings and the manual say "default store"
wherever they said "the graph". *Cost if wrong:* naming only.

**R6 — Scope guard.** Not in this unit: placement *rules*/functions
(§4 rejected them), per-namespace defaults (rejected), semantic
enforcement of cross-namespace references (#109, opt-in), cross-store
edge *query* surface (consumer of R4's hint), and any change to
`cross-graph-transaction-error` — two namespaces in one store already
share a transaction domain by construction (§2).

## 3. Deliverables

1. `default-store-not-open-error` (readers: class name, store name) and
   the R1 constructor default in `def-node-type`'s generated
   `make-<name>` (schema.lisp).
2. `%find-registered-node-type (symbol kind)` — global meta lookup by
   class symbol across the hash — and the R3 lazy instantiation hook in
   the constructor path.
3. Occupancy: `%note-edge-occupancy (name store)` called from
   `instantiate-node-type` (edge kinds only), `edge-type-stores (name)`,
   `edge-occupancy.dat` beside the type registry, loaded lazily.
4. Docs: manual (schema chapter: default store, lazy adoption,
   occupancy hint; the "one class, many stores" example from §3.2 with
   two `in-package` forms), CHANGELOG **Changed (breaking)** entry for
   R1, spec Built notes here and in the namespaces spec (§11 unit 2 →
   Done).
5. Tests (FiveAM, `tests/package-namespace-tests.lisp`): two packages
   defining same-named classes without collision and with distinct
   data; one class written into two stores via explicit `:graph`;
   default-store placement (constructor with no `:graph` writes to the
   declared store while `*graph*` is bound to another — ablation of
   R1); refusal when the default store is closed; lazy adoption
   persists across reopen (write into foreign store, close, reopen,
   read back); edge placed by its own class default with endpoints in
   two other stores; occupancy set reflects both stores and
   `edge-type-stores` returns NIL for a never-written class; divergent
   slot warning still fires (#196 regression pin).
6. Full suite green on SBCL (ECL demoted). Closes vivace-graph#167 and
   cl-llm#20 (a class instantiable in more than one store).

**Built (#167):** Shipped as designed — R1-R5 landed unchanged, R6's
scope guard held (no placement-rule function, no per-namespace default,
no query rewiring). Two additions surfaced by review that the design
above did not anticipate: (1) a first attempt made `def-node-type`'s
registration-list maintenance **move** a redeclared class's meta to its
new default store; this broke the #186 pattern of one class registered
in more than one store (a live pattern in `tests/global-type-id-
tests.lisp`, ~150 cascading test failures + an fd exhaustion crash), so
the sweep was reverted. `%find-registered-node-type` instead takes an
optional `prefer-store` argument and checks that store's own list
first, falling back to a full scan only when unset; `%ensure-type-in-
store` passes `(graph-name graph)` so ambiguity resolves
deterministically to the calling store's own meta without disturbing
any other store's registration. See
`a-class-registered-in-two-stores-keeps-both-metas`. (2)
`%note-edge-occupancy`'s sidecar append is now guarded by
`handler-case`/`ignore-errors` around the file write, not just around the
initial `%edge-occupancy-file` lookup — a disk-full or permission failure
on the append must degrade to in-image-only for the session, never
propagate into the caller's real edge write; see
`edge-occupancy-append-failure-does-not-abort-the-write`. Full suite
green on SBCL (ECL demoted per standing directive).

## 4. Acceptance mapping

| Issue acceptance | Where |
|---|---|
| Class instantiable in >1 store (cl-llm#20) | R1+R3, tests 2–5 |
| Two packages, same symbol-name, no collision | already true post-#186; pinned by test 1 |
| One namespace spans stores | R1 explicit `:graph` + R3; test 2 |
| Existing consumers compile and write unchanged | R1 one-to-one case; full suite is the proof |
| Full suite green | gate |
