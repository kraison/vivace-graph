# Changelog

All notable changes to VivaceGraph are recorded here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html):
`MAJOR.MINOR.PATCH` — MAJOR for incompatible changes (including on-disk storage
format bumps), MINOR for backward-compatible features, PATCH for backward-compatible
fixes. The `## [Unreleased]` section accumulates changes on the `experiment` branch
between releases; cutting a release renames it to the new version and dates it.

## [Unreleased]

Nothing yet.

## [2.1.1] - 2026-07-06

A bug-fix release.

### Fixed
- **ECL: cross-graph `edge-exists-p` / adjacency read the wrong heap.** A ve/vev
  index lookup for a graph other than the current `*graph*` deserialized its
  index-list against `(heap *graph*)` — the `deserialize-index-list` default — instead
  of the owning graph's heap. So a cold `(edge-exists-p … :graph B)` (or a generated
  `make-<type>`'s type-id resolution) while `*graph*` named a *different* graph walked
  the wrong heap and returned NIL, and the per-index cache was then poisoned with the
  mis-bound list. Fixed by binding `*graph*` to the owning graph at the
  `lookup-vev-index-list` / `lookup-ve-in-index-list` / `lookup-ve-out-index-list`
  read boundaries. It manifested on **ECL** (SBCL's cache/timing masked it in the
  regression test), but the underlying flaw was implementation-independent. Full test
  suite green on both SBCL and ECL.

## [2.1.0] - 2026-07-05

A large, backward-compatible feature release: a pluggable ordered-index backend
(an mmap B+ tree alongside the skip list), `:unique` slot constraints, offline-first
peer replication, an in-memory backend, idempotent views, a modernized Prolog engine
with a safe web query surface, and cross-cutting correctness fixes. Existing on-disk
v2 graphs open without migration.

### Added
- **Pluggable ordered-index backend — an mmap B+ tree (opt-in).** All heap-backed
  ordered indexes (map/reduce views, `:unique` constraints, the spatial index) are
  now built on a shared ordered-map protocol with two interchangeable backends: the
  skip list (default) and a new page-oriented (4 KB slotted-page) **B+ tree**
  (`bplus-tree.lisp`). The backend is a per-graph choice — `:index-backend
  :bplus-tree` on `make-graph`/`open-graph`, or the global `graph-db:*index-backend*`
  — and each index persists the backend it was written with, so a graph reopens
  every index on its own engine. On disk the B+ tree beats the skip list on every
  operation once warm (page-packed keys → far fewer cache-line and page misses,
  sequential in-leaf range scans, less space), with in-place cell edits and
  merge-on-delete rebalancing. Existing graphs migrate in place via
  `regenerate-all-views` / `regenerate-unique-indexes` / `rebuild-spatial-index`
  (or snapshot + replay). (Manual Chapter 3.)
- **`:unique` slot constraints (issue #6).** `def-vertex` / `def-edge` slots may
  carry `:unique t | equal | equalp | <canonicalizer>` (the value is the uniqueness
  key — identity, case/edge folding, or an arbitrary canonical form). Enforced at
  the commit boundary: a violation aborts the whole transaction with
  `unique-constraint-violation`; NULL-exempt (SQL-style); shared across subclasses
  of the declaring type; commits racing for the same value are serialized so exactly
  one wins. Backed by a persistent, per-graph unique index (skip-list or B+ tree;
  in-RAM map on a memory-graph) reopened with the graph — not rebuilt by scanning on
  open. (Manual Chapter 8. Distributed cross-device arbitration is tracked in #51.)
- **Peer replication — offline-first, hub-and-spoke sync (Chapter 16).** A
  bidirectional *peer* mode for mobile/edge fleets, alongside the existing
  master/slave replication: each device is synced only the authorized subset of the
  graph it may see (`:export-predicate`), authors locally while disconnected, and
  reconciles on reconnect. Closed-subgraph export + manifest reconciliation, node
  re-homing, per-node origin identity, Lamport clocks, and a pluggable
  conflict-resolution policy (`:origin` partitioning by default). Runs on both the
  on-disk and in-memory backends (verified SBCL hub ↔ ECL device).
- **In-memory backend — `make-memory-graph` (issue #50, Chapter 15).** An in-RAM
  storage backend that holds the whole graph as live Lisp objects, eliminating
  per-read deserialization and pcons-chain walking — lowest-latency reads when the
  graph fits in memory (aimed at mobile/ECL). Durable via the same journal plus a
  checkpoint image; eager or fault-on-access (lazy) open. The graph model,
  `with-transaction`, OCC validation, views, spatial, `:unique`, peer replication,
  and the Prolog engine all work against it unchanged.
- **Idempotent `def-view` (issue #49).** `def-view` is now declarative and
  idempotent: redefining a view with an unchanged definition is O(1) at open (no
  rescan), and a changed definition is diffed and rebuilt automatically via a
  two-phase registry. `open-graph` / `open-memory-graph` install views on open and
  accept `:regenerate-views t`; `regenerate-all-views` forces a full rebuild.
- **Streaming results: `select` `:callback` + NDJSON web responses (issue #44).**
  `select` accepts `:callback FN`, which hands each result row to `FN` as it is
  produced -- consing nothing onto a result list -- and returns the row count.
  An embedded consumer can stream an unbounded result set with constant memory.
  The web layer uses it: a query with `format=ndjson` (a parameter for
  `def-query`, a body field for the pattern query) streams each row as its own
  JSON object on its own line (`application/x-ndjson`) instead of buffering a
  JSON array.
- **ISO exceptions: `catch/3` + `throw/1` (issue #45).** `throw(Ball)` raises a
  ball and `catch(Goal, Catcher, Recovery)` recovers from one that unifies with
  `Catcher`, propagating others to an outer catch.  Only `Goal` is protected --
  a throw in the continuation after `catch/3` succeeds is not caught (the
  continuation-swallowing trap, handled with a per-frame marker).  Built-in
  errors now carry an ISO-style ball so they are catchable: an unknown predicate
  is an `existence_error`, an uninstantiated meta-call an `instantiation_error`,
  a non-callable goal a `type_error`.  The error vocabulary is keywords
  (`(:error (:existence-error :procedure foo/2) Ctx)`) so a ball unifies
  regardless of the query's package.  Resource (budget/timeout) and permission
  (effect-policy) errors are deliberately **not** catchable, so a bounded,
  untrusted query cannot `catch(Goal, _, true)` to swallow its own enforcement.
- **Prolog control-flow core (issue #45, Phase 0).** `not`/`\+`, `if` (the
  two- and three-argument `Cond -> Then [; Else]` soft cut), `once`, and `forall`
  are now first-class compiler constructs: they expand through `compile-body`, so
  they thread bindings and compose with conjunction and cut instead of routing
  through the runtime `call/1` functors.  Each opaque construct (`not`, `once`,
  the condition of `if`) is a proper cut barrier, while a cut in a `Then`/`Else`
  branch or in the tail after the construct still cuts the enclosing clause.
  A non-static (meta-call variable) sub-goal, e.g. `(not ?G)`, transparently
  falls back to the runtime functor, so existing behavior is preserved.
- **Compiled `call/N` and a runtime meta-call solver (issue #45, Phase 0.2).**
  `(call Goal Extra...)` now appends the extra arguments to `Goal` (call/N).
  When `Goal` is a static template the call compiles inline through
  `compile-body`, so it composes with cut and the control constructs (e.g.
  `(call (or ...))`, `(call (g-knows ?a) ?b)`).  When `Goal` is a variable, the
  new `%solve` runtime solver proves it -- handling conjunction, disjunction,
  call/N and atomic/compound goals.  `call/1` and the control runtime functors
  (`not`/`if`/`once`/`forall`) route through `%solve`.
- **All-solutions aggregation (issue #45, Phase 0.3).** New `findall/3` (collects
  every template instance in order, always succeeds, `[]` on no solutions).
  `bagof/3` and `setof/3` now **group by the goal's free (witness) variables** --
  the variables in the goal but not in the template and not existentially
  quantified -- yielding one solution per witness binding (and still failing when
  the goal has no solutions).  The `^` operator (`(^ Var Goal)`, nestable, accepts
  a single var or a list) marks variables as existential so they are not treated
  as witnesses.  `setof` sorts each group by the standard order of terms and
  removes duplicates.
- **Query resource bounds (issue #45, Phase 0.4).** Queries can now be bounded by
  a maximum inference count (`:max-inferences` select option / `*inference-budget*`
  / `*default-inference-budget*`) and a wall-clock timeout (`:timeout` seconds /
  `*default-query-timeout*`).  Exceeding either aborts the query with a catchable
  `prolog-resource-error`, so a runaway, non-terminating, or cyclic-recursive
  query fails cleanly instead of hanging or overflowing the Lisp control stack.
  Both default to nil (unlimited): trusted queries are unchanged, untrusted ones
  (e.g. the planned #44 web surface) opt in.  Solution count remains bounded by
  the existing `:limit`.
- **Effect partitioning / query effect policy (issue #45, Phase 1).** The
  side-effecting Prolog functors are now tagged by effect -- `:write` (graph
  mutation: `retract`), `:eval` (arbitrary Lisp: `lisp`/`lispp`/`is`/`trigger`),
  `:io` (`read`/`write`/`nl`) -- and check the per-query policy before acting.
  The `:effects` select option (or `*allowed-effects*` / `*default-allowed-effects*`)
  is `t` for all (the default) or a list of permitted tags; a disallowed effect
  aborts with a catchable `prolog-permission-error`.  Reads and pure logic are
  always allowed, so `:effects nil` is a safe read-only query mode (the basis for
  exposing queries to untrusted callers).  The check is transitive -- an effect
  reached through a user rule or meta-call is gated the same way.
- **Snapshot query mode (issue #45, Phase 1).** `select` accepts `:snapshot t`,
  which runs the query under a single consistent MVCC read snapshot: every read
  resolves at one epoch, so the result is stable against concurrent writers (a
  vertex committed after the query started is invisible to it).  Implemented as
  a lightweight read transaction (`with-read-snapshot` / `call-with-read-snapshot`)
  that registers active for the query's extent -- holding the reaper's retention
  floor -- and is discarded without commit or validation.  It inherits an
  enclosing transaction if one is already active.  Together with the resource
  bounds, the effect policy, and `:limit`/`:skip`, this gives a query surface
  safe to expose to untrusted callers.

### Changed
- **Unknown Prolog predicates are now noisy.** A goal naming an undefined
  predicate signals a `prolog-error` -- on both the compiled query path and the
  dynamic meta-call path -- instead of silently yielding no answers (the
  compiled path) or aborting with an opaque message (the old `call/1`).  This
  surfaces mistyped predicate names instead of letting them masquerade as empty
  results.  (A future `catch/3` + ISO `existence_error` will make this
  recoverable; see #45.)

- **`select-count` / `select` `:count`.** `select-count` (already exported but
  never implemented) now returns the integer number of solutions to a query
  without projecting or consing any per-solution bindings; the underlying
  `select` `:count t` option does the same and composes with `:limit` and
  `:skip` (so a capped or offset count counts the rows `select` would return).

- **`def-query` -- named parameterized queries over the web (issue #44).** A new
  `def-query` registers a server-authored, read-only graph query as a REST
  endpoint at `POST /graph/:graph/query/<name>`.  The author declares typed,
  named parameters (`:string`/`:integer`/`:number`/`:boolean`/`:keyword`),
  result variables, and the query goals; the client supplies only the
  parameters.  Each query runs through `select` with safe defaults the author
  may override -- read-only (`:effects nil`), and a result limit, inference
  budget, and wall-clock timeout.  A read-only query runs under a lightweight
  MVCC read snapshot; a query whose `:effects` permit side effects instead runs
  inside a `with-transaction`, so its writes flatten into one transaction that
  provides the same snapshot and commits on success (or rolls back if a bound or
  permission error aborts it).  Responses are a JSON array of objects keyed by the camelCase result
  names; a missing/malformed parameter is a 400, a resource-bound breach a 400,
  a forbidden effect a 403, and an unknown query a 404.  Parameter values are
  injected through a new pure `param/2` functor (and `*query-params*`), so
  injection works under the read-only policy.
- **Constrained JSON pattern queries (issue #44, tier 2).** Clients may POST an
  ad-hoc, read-only query as a JSON object to `POST /graph/:graph/query` -- no
  server-authored template and no client Lisp.  The body is a
  `{match, where, select, limit, skip}` document of typed pattern objects
  (`{vertex,type}`, `{edge,from,to}`, `{slot,name,bind|value}`,
  `{compare,args}`) compiled to a bounded `select`.  Type/edge names are
  resolved against the live schema (an unknown one is a 400), which also
  determines the package the query compiles in; only a fixed set of safe pattern
  kinds is expressible (no arbitrary predicate naming).  The query runs read-only
  (`:effects nil`), under one MVCC snapshot, with the client `:limit` capped at
  `*query-default-limit*` and the inference/time budgets applied; a malformed
  query or a bound breach is a 400.  Results use the same JSON array-of-objects
  shape as `def-query`.

### Fixed
- **Wrong-graph (`*graph*`) leaks across the core node/index/query layer.** A class
  of latent bug where code holding a specific graph resolved node ids or schema
  type-ids through the dynamic `*graph*` instead — so it operated on the *wrong*
  graph whenever `*graph*` differed (a reopened graph, a second open graph, a
  snapshot/replay target, or a slave/peer graph). Fixed in `map-view` /
  `invoke-graph-view`, `traverse`, `edge-exists-p`, the generated `make-<type>`
  type-id resolution, the `ve-index` index-list heap (a foreign-heap allocation that
  could corrupt adjacency), and `apply-transaction` (now binds `*graph*` to the
  target as a structural guard). Makes the snapshot/replay-into-a-fresh-graph idiom
  correct for graphs with views. A cross-graph regression test guards it.
- **REST procedure/query POST routes never worked over HTTP.** Their ningle
  handlers were quoted lambdas (`'(lambda (params) ...)`) -- a *list*, not a
  function -- so the server returned the list verbatim and the response was
  malformed, and the handler also referenced the route capture
  (`procedure-name`) as an unbound free variable instead of reading it from the
  params.  Replaced with real closures that pull the capture via
  `(get-param params :procedure-name)` / `:query-name`.  Surfaced by the new
  end-to-end HTTP tests (the existing tests exercised handlers in-process).
- **REST procedures were broken on ECL.** `*rest-procedures*` had `#+sbcl`/
  `#+ccl`/`#+lispworks` initforms but no `#+ecl` branch, so on ECL the variable
  was declared special but left unbound; `def-rest-procedure` and
  `call-rest-procedure` then failed with an `unbound-variable` error.  Added the
  `#+ecl` branch so REST procedures work on ECL like the other implementations.
- **`node-slot-value/3` swallowed downstream query errors.** Its `handler-case`
  wrapped `(funcall cont)` -- the continuation -- so an error raised by any goal
  *after* a `node-slot-value` (e.g. a `prolog-permission-error` from a denied
  write, or a `prolog-resource-error`) was caught and silently turned into a
  non-match.  The guard now wraps only the slot read; the continuation runs
  outside it so downstream errors propagate.
- **Prolog `if/3` else-semantics (issue #45).** `(if Test Then Else)` now runs
  `Else` only when `Test` has no solution; previously it also ran `Else` when
  `Test` succeeded but `Then` failed.  The runtime `if/3` functor (the meta-call
  path) was corrected to match.
- **Prolog `or` binding propagation.** A variable first bound inside a disjunct
  (e.g. `(or (= ?x 1) (= ?x 2))`) was lost across the disjunction's shared
  continuation because `=` had been optimized to a compile-time alias.  The `or`
  compiler macro now seeds its fresh variables so they bind on the trail at
  runtime and are visible to the continuation.
- **ECL spatial-index concurrency (issue #42).** The skip list guarded every
  operation -- reads included -- with one recursive lock on ECL, so concurrent
  spatial queries ran sequentially and timed out under high parallelism.  Replaced
  it with a per-skip-list reader/writer lock: shared read lock for readers (find,
  cursor scans, map, count), exclusive write lock for mutators.  Writers never run
  concurrently with readers (torn-read safety preserved); concurrent readers now
  run in parallel.  No-op on non-ECL (those keep the lock-free design).

## [2.0.0] - 2026-06-11

A major release: MVCC versioned nodes, a geohash spatial extension, a full
cross-implementation port (SBCL/CCL/ECL) with a comprehensive automated test
suite, and an ACID-compliance audit.

### Added
- **MVCC — immutable, versioned nodes (issue #19).** Each update now retains the
  prior version of a node in a heap-backed version chain instead of freeing it in
  place. Old versions are reclaimed by a lazy, epoch-gated reaper once no active
  reader or transaction can still observe them. Configurable retention via
  `:keep-revisions` (per node type, with a graph-level default), and
  snapshot-isolation reads for transactional lookups. As a bonus this dissolves
  the long-standing node-data read-after-free race at its source.
- **Spatial extension.** A geohash-backed, heap-resident spatial index answers
  proximity and area queries over nodes that carry a `geometry`. Declarative
  opt-in via a `:index t` geometry slot (`node-geometry` auto-wiring), bounding-box
  and radius queries, k-nearest-neighbour search, and geohash neighbour
  enumeration. Optional `graph-db/geos` integration adds exact topology, validity
  repair, and accurate distance via libgeos. (See Chapter 13 of the manual.)
- **ECL support.** The full `:graph-db` system — including the REST layer — builds
  and runs on ECL 26.5.5; the entire test suite is green on ECL (macOS arm64 and
  Linux x86_64), matching SBCL.
- **Automated test suite (FiveAM).** New `graph-db/test`, `graph-db/concurrency-test`,
  `graph-db/acid-test`, `graph-db/stress-test`, `graph-db/concurrent-stress-test`,
  and a `graph-db/perf-test` performance-benchmark system. Replaces the previous
  ad-hoc REPL-driven tests.
- **ACID-compliance audit** with dedicated regression tests (atomicity,
  consistency, isolation, durability) and broad concurrency coverage across
  SBCL/CCL/ECL.
- `migrate-graph` for upgrading a pre-MVCC (v1) on-disk graph to the v2 format via
  a logical snapshot + replay.
- This `CHANGELOG.md`.

### Changed
- **On-disk storage format bumped to v2.** The node head grows from 15 to 31 bytes
  (append-only: `commit-epoch` + `prev-pointer`). v1 graphs cannot be opened
  directly by v2 code — use `migrate-graph` (logical snapshot + replay). New
  graphs are stamped v2 automatically.
- Stable-address memory mapping: `extend-mapped-file` remaps in place
  (`MAP_FIXED`) so the base pointer never moves, enabling lock-free reads across
  SBCL/CCL/ECL.
- Project version 1.0 → 2.0.

### Fixed
- **Persistent-slot `slot-boundp` / `slot-makunbound` (issue #41).** These MOP
  generic functions were never specialized for the node metaclass, so they
  inspected the always-unbound backing CLOS slot; `slot-boundp` on a persistent
  slot was always NIL and `slot-makunbound` was a no-op on the value. Both now
  consult the data alist.
- **ECL concurrency regression from the #41 fix.** ECL's `change-class` invokes
  `slot-makunbound-using-class` on alist-backed persistent slots during node
  construction, which cleared a freshly-created node's data and triggered racy
  lazy re-materialization of the shared cached node (transient NIL slot reads). A
  dynamic guard (`*initializing-node*` / `change-node-class`) suppresses the
  destructive alist edit during (re)initialization; explicit `slot-makunbound` is
  unaffected.
- Numerous concurrency-correctness fixes surfaced by the new suites: rw-lock
  wakeup herd / FIFO behaviour, skip-list torn-read SIGSEGV on ECL (now
  serialized), insert lost-update ordering, schema class-lock rebuild on
  `open-graph` (issue #32), and replication socket/threading fixes.

### Known issues
- **ECL only**, under the high thread-count parallelism of many-core Linux hosts:
  `CONCURRENT-SPATIAL-INSERT-AND-QUERY` can deadlock (issue #42) and
  `FULL-SYSTEM-STORM` can flakily time out (issue #43). Both are timeouts, not data
  corruption; SBCL and CCL are unaffected, and ECL is green at lower parallelism.

### Compatibility
- ECL **26.5.5** is required (earlier releases such as 21.2.1 are no longer
  supported).
- CCL is supported on Linux x86_64 only; there is no usable CCL on Apple-Silicon
  macOS.
- LispWorks support is currently **untested** (no license access; the free
  Personal Edition's heap is too small to compile VivaceGraph).

[Unreleased]: https://github.com/kraison/vivace-graph/compare/v2.1.1...HEAD
[2.1.1]: https://github.com/kraison/vivace-graph/compare/v2.1.0...v2.1.1
[2.1.0]: https://github.com/kraison/vivace-graph/compare/v2.0...v2.1.0
[2.0.0]: https://github.com/kraison/vivace-graph/releases/tag/v2.0
