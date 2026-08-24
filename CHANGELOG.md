# Changelog

All notable changes to VivaceGraph are recorded here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html):
`MAJOR.MINOR.PATCH` — MAJOR for incompatible changes (including on-disk storage
format bumps), MINOR for backward-compatible features, PATCH for backward-compatible
fixes. The `## [Unreleased]` section accumulates changes on the `experiment` branch
between releases; cutting a release renames it to the new version and dates it.

## [Unreleased]

### Changed

- **`DEF-NODE-TYPE` now installs through a shared functional core**
  (#172). The macro's expansion keeps only the literal `DEFCLASS`; the
  generated helpers (`MAKE-<N>`, `LOOKUP-<N>`, `<N>-P`), the `<N>/2` and
  `<N>/3` Prolog functors for edge types, the `*SCHEMA-NODE-METADATA*`
  registration and the instantiation into an open default store all run
  in `%INSTALL-NODE-TYPE`, so a later runtime path can build a class
  from persisted metadata and get exactly the same installation. No
  behaviour change for source-defined types: the helpers are closures
  installed with `(SETF FDEFINITION)` instead of compiled `DEFUN`s, and
  the functor symbols are now interned in the class symbol's own
  package rather than the expansion-time `*PACKAGE*` — identical for
  every ordinary `DEF-VERTEX` / `DEF-EDGE`, where the class symbol is
  read into the defining package. Functors are still installed under
  both `FDEFINITION` and `*PROLOG-GLOBAL-FUNCTORS*` and exported, as
  `DEF-GLOBAL-PROLOG-FUNCTOR` did.

### Fixed

- **`MAKE-GRAPH`/`OPEN-GRAPH` accepted a slashless `LOCATION` and scattered
  its sidecar files into the parent directory** (#222). Every sidecar built
  with `(MAKE-PATHNAME :defaults (LOCATION GRAPH))` -- `.dirty`, `heap.dat`,
  `schema.dat`, and the rest -- depends on `LOCATION` being a *directory*
  pathname; a trimmed namestring kept it as a *file* pathname instead, so
  those sidecars landed next to the store rather than inside it. Both
  functions now normalize `LOCATION` once, via `UIOP:ENSURE-DIRECTORY-
  PATHNAME`, before any use -- the same fix `%REOPEN-AND-RESUME`
  (`shadow-store.lisp`, #171) already applied locally, generalized to the
  entry points themselves so every caller gets it for free.

  ⚠ **Upgrade note.** A store originally *created* through a slashless
  `LOCATION` has `transaction-id.dat`, `lamport.dat` (peer graphs) and
  `pull-cursor.dat` (peer devices) -- among others -- sitting in its
  PARENT directory, not inside the store. After this fix, `OPEN-GRAPH`
  looks for them inside the store and will not find them: the
  transaction-id watermark and, on a peer graph, the durable Lamport
  clock and pull cursor all silently reset to their zero/absent
  defaults on the next open. Before upgrading a store you know was
  ever created or reopened with a trailing-slash-free `LOCATION`,
  manually move any of those files sitting beside the store directory
  (check the PARENT of `LOCATION` for `.dirty`, `heap.dat`,
  `schema.dat`, `transaction-id.dat`, `lamport.dat`, `pull-cursor.dat`,
  `policy.dat`) into the store directory itself first. A store that
  was always opened with a trailing slash is unaffected.
- **An aborted `MAKE-GRAPH`/`OPEN-GRAPH` leaked every fd it had already
  opened** (#224). Neither function had any teardown on a non-local exit
  partway through -- a `STORE-ID-COLLISION-ERROR` or any other failure left
  the heap, indexes, vertex/edge tables and ve/vev indexes already opened
  memory-mapped and open, and could leave the graph half-registered in
  `*GRAPHS*`. `MAKE-INSTANCE` evaluates its initarg value-forms before it
  runs, so a failure partway through that argument list left the
  already-opened ones reachable from nothing but a local variable; the
  open-sequence in both functions now binds each resource to a name and
  tracks it in a small mutable list *before* handing it to `MAKE-INSTANCE`,
  so the list has every fd-bearing resource open at the moment of failure
  regardless of where in the sequence it happens. Both functions now run
  their body under `UNWIND-PROTECT`, and a new `%ABORT-GRAPH-OPEN` best-
  effort closes (`IGNORE-ERRORS` per component, tolerating slots still
  unbound) everything the partial open acquired, and deregisters the graph
  from `*GRAPHS*`/the open-store vector if that ran before the failure. The
  signalled condition always propagates unchanged. The teardown also
  stops replication before closing anything else (a master's accept
  thread and listening socket must not outlive the mmaps they
  reference) and closes every vector segment `RESTORE-VECTOR-SEGMENTS`
  had already opened (each is marked dirty-on-disk the moment it
  opens, so leaving one open forces a full rebuild-from-nodes on the
  next open). One rule needs stating explicitly: the abort deletes a
  `.dirty` marker only when nothing that can mutate an EXISTING
  store's heap has run yet (`OPEN-GRAPH`'s recovery/rebuild steps,
  `MAKE-GRAPH`'s WAL replay for a slave). Once one of those has run,
  `.dirty` is deliberately left in place -- the store now genuinely
  needs recovery, and deleting the sentinel would let a later open
  adopt stale index roots against the now-mutated heap with no
  recovery pass to catch the mismatch.
- **`%POSIX-OPEN` created files with an arbitrary permission mode on Apple
  arm64** (#218). `open(2)` is `int open(const char *, int, ...)`, and the
  `mode` argument was passed through a plain `CFFI:FOREIGN-FUNCALL` — as a
  *fixed* argument. On Darwin/arm64 variadic arguments use a different
  convention, so the callee read `mode` from where nothing had written it:
  `0140` and `0200` were both observed for a requested `0640`, on consecutive
  runs. When the resulting mode omitted owner read/write, the next ordinary
  `WITH-OPEN-FILE` on that path failed `EACCES` — and since the type registry
  (#186) and the system clock (#182) both create their files this way, **no
  graph in the image could be opened at all**, with a zero-byte file left
  behind that reproduced the failure on every subsequent run. Now routed
  through `CFFI:FOREIGN-FUNCALL-VARARGS`, which emits SBCL's variadic marker.
  x86_64 was never affected (the two conventions coincide there), which is why
  this reached a release: it presented as a broken developer machine rather
  than a defect. `open` was the only variadic call in `posix.lisp`; the rest
  (`flock`, `close`, `lseek`, `write`, `fchmod`, `rename`, `mmap`, `munmap`,
  `msync`, `getpagesize`, `gettimeofday`) have fixed signatures and are
  correct as written. Regression test asserts the created file's **mode** —
  every previous test asserted only that the fd was valid, which is exactly
  what a garbage mode still yields to its creator.

### Added

- **A node type can now be defined at runtime, from data, and survive a
  restart as a live class instead of only as metadata** (#172, unit 7 of
  the namespaces epic, #110). `ENSURE-NAMESPACE` (name &key nicknames)
  creates a package -- no files, no store -- as the functional twin of
  writing a new `IN-PACKAGE`; `CREATE-VERTEX-TYPE` / `CREATE-EDGE-TYPE`
  (name slot-specs &key parents default-store keep-revisions) are the
  runtime twins of `DEF-VERTEX`/`DEF-EDGE`, building a class from a data
  slot-spec list through the same `%INSTALL-NODE-TYPE` path a macro
  expansion uses, so a runtime type is indistinguishable from a
  source-defined one once built (redefining an existing name, runtime-
  or source-defined, is ordinary CLOS redefinition, with the existing
  #196 divergence warning on slot disagreement). `DEFAULT-STORE`
  defaults to `NIL` -- "no default store" -- so a runtime type need not
  commit to placement at creation; its generated constructor then
  requires an explicit `:GRAPH`.

  A system-level manifest, `schema-manifest.dat` beside the type
  registry, records every namespace and type -- appended by both the
  source and runtime installation paths, so it describes the WHOLE
  schema, fail-safe like the #167 occupancy sidecar (no system
  directory, or a torn/damaged file, degrades to in-image-only rather
  than aborting a definition). `MATERIALIZE-SCHEMA` (dir &key
  namespaces) is the load-order answer this manifest exists for: a
  macro carrying its own `EVAL-WHEN`, placed in its own file between
  the static schema and any file with methods on a runtime type, that
  rebuilds every runtime-defined package and class from the manifest
  before those methods compile -- the twenty-year blocker on this
  feature. It is idempotent and **source wins**: a type whose class
  already exists is left alone, with the #196 warning on divergence.
  Nothing is evaluated -- the input is plists, the output is MOP
  calls -- and it fails fast, before building anything, naming every
  offender in one condition each: `MATERIALIZE-UNRESOLVED-FUNCTIONS`
  for a `:CHECK` name the image does not provide, and
  `MATERIALIZE-UNRESOLVED-PARENTS` for a row whose parent neither
  exists nor is itself being built in this call (a half-built
  materialization would otherwise leave stub classes that poison every
  later attempt). Returns `(:NAMESPACES n :MATERIALIZED n
  :SKIPPED-EXISTING n)`.

  Behaviour is the one thing that never crosses the metadata boundary:
  a closure does not serialize, so a runtime type that wants a
  constraint names a function the image registers by code,
  `REGISTER-SCHEMA-FUNCTION` (name fn) / `FIND-SCHEMA-FUNCTION` (name),
  and the metadata stores only that name. The sole v1 consumer is a new
  `:CHECK FN-NAME` slot option (accepted by `DEF-VERTEX`/`DEF-EDGE`
  slot-specs too, for parity), enforced where value constraints already
  are, NULL-exempt, violating with the existing condition's `:reason
  :CHECK-FAILED`; presence is verified at `CREATE-*-TYPE` time
  (signalling `SCHEMA-FUNCTION-UNRESOLVED` if the name is not yet
  registered) and again at `MATERIALIZE-SCHEMA` time, resolution
  happens at each check so a re-registration takes effect immediately.
  Restart never evaluates data -- this is the invariant the whole unit
  is built to hold.

  Two read-only visibility tools close the opacity gap a runtime type
  otherwise opens (a class with no source file, ungreppable): `DESCRIBE-
  SCHEMA` (&key namespace store since stream) is a plain-text dump,
  joining the manifest with live metas, grouped by namespace, one line
  per type (kind, default store, a `[source]`/`[runtime YYYY-MM-DD]`
  provenance tag) and one line per slot (name, type, `:CHECK` name);
  `:SINCE` filters by record time, so the dump doubles as a change log.
  `EXPORT-SCHEMA-SOURCE` (path &key namespace store) writes a generated-
  header comment plus literal `DEFPACKAGE`/`DEF-VERTEX`/`DEF-EDGE` forms
  reconstructed from the metadata -- a symbol foreign to the exported
  namespace prints package-qualified so it re-reads to the SAME symbol
  rather than interning a new one under the freshly created package,
  which would silently break an EQ-keyed lookup like a `:CHECK` name.
  This is the promotion path: loading the generated file is the
  ordinary source path, idempotent (same names, same registry ids), and
  turns a runtime type into a source-defined one for good. Export never
  runs implicitly and the engine never reads the file back.

  Not in this unit, deliberately: runtime type deletion/retraction,
  runtime `DEF-VIEW`/index/unique definition (those macros stay
  code-side), and an Emacs mode (the text dump is SLIME-usable as-is).

  Final-review fixes (#172, review round 3): the manifest dedup cache
  is now seeded from the on-disk file on a fresh image's first write,
  so a reopen no longer re-appends every type row with a new `:time`
  (previously `DESCRIBE-SCHEMA :SINCE` listed the whole schema after
  any reopen); `CREATE-VERTEX-TYPE`/`CREATE-EDGE-TYPE`'s symbol-argument
  path now refuses a CL-homed name before interning any slot into it,
  instead of hitting SBCL's raw package-lock error; `EXPORT-SCHEMA-
  SOURCE` now qualifies a bare symbol whose name shadows an external
  `COMMON-LISP`/`GRAPH-DB` symbol (e.g. a slot named `TYPE`), which
  previously round-tripped to the wrong symbol silently; `DESCRIBE-
  SCHEMA`'s `:SINCE` string now parses at UTC, matching the UTC dates
  it prints; and `ENSURE-NAMESPACE` refuses `COMMON-LISP`/`KEYWORD` (or
  a nickname of either) the same way `CREATE-*-TYPE` already does.
  `REGISTER-SCHEMA-FUNCTION`'s docstring and the manual now say
  explicitly that a `:CHECK` function must be pure: OCC retry can run
  it more than once per logical write.

- **A store adopts a foreign class at first write; lookup of a class
  registered in more than one store is deterministic** (#167). Writing a
  node of a class via an explicit `:graph` that names a store other
  than the class's declared default no longer requires that store to
  have seen the class before: the first such write finds the class's
  registered metadata, instantiates it into the target store under the
  schema lock, and saves that store's `schema.dat` — the type is then
  durably part of that store, surviving close and reopen like any type
  declared there from the start. This is the mechanism behind "one
  class, many stores" (cl-llm#20; #186). Because a class can be
  registered under more than one store simultaneously,
  `%find-registered-node-type` takes an optional preferred store and
  checks that store's own registration first before falling back to a
  full scan, so `:graph`-directed lookups resolve to the calling
  store's own meta rather than whichever store a hash-table scan
  happens to visit first.
  Edge classes additionally maintain a store-occupancy hint,
  `edge-type-stores` (name) — the list of stores known to hold a given
  edge class, or `NIL` for "no hint, sweep everything." It is updated
  at the same instantiation point that adoption uses, so both a
  class's declared-store write and a lazy cross-store adoption keep it
  current. The hint is a best-effort, append-only sidecar
  (`edge-occupancy.dat`, beside the type registry) that fails safe: a
  missing system directory, an unreadable or torn file, or a never-
  written class all answer `NIL`, and a failed append degrades to
  in-image-only for the session rather than aborting the write that
  triggered it. Nothing in this change consumes the hint for query
  routing — that is left to the ontology/cross-store query work.

- **New node ids are tagged UUIDv8, carrying a 12-bit store field** for
  O(1) cross-store resolution; existing v5 ids are unchanged and a v5
  id still resolves via a per-open-store scan, so there is no flag
  day. The tag is a stable numeric id from a new append-only
  `store-registry.log` in the system directory (one entry per
  graph-name, never reused). `resolve-node-graph` reports
  `:resolved`/`:detached`/`:unknown`; `lookup-vertex-anywhere` returns
  the vertex, an `unresolved-node` marker for a registered-but-closed
  store, or (with `:if-detached :error`) signals
  `store-detached-error`. `traverse` surfaces a cross-store endpoint or
  a detached-store marker in its results without walking past it —
  continuing across stores is left to a follow-on. `active-edge-p`
  resolves cross-store endpoints too, with two narrow, documented gaps
  (no cross-store scan for an untagged v5 endpoint; an unregistered tag
  counts as live) tracked as #208. `backup` now includes a dangling
  cross-store edge rather than dropping it, and warns with
  `dangling-edge-warning` naming the edge, the missing endpoint and its
  store; a backup with no cross-store gaps never warns. (#169)

- **Detach a store, bulk-load a shadow copy, and swap it in — all in-process,
  with only two brief unavailable windows.** `detach-store` quiesces a
  graph (refuses new transactions and read pins, drains in-flight ones),
  leases a range of its system clock's epoch space via
  `clock-lease-epochs`, journals `:detach`, closes it durably, and
  returns a `store-detachment` handle; `reattach-store` reopens it and
  rejoins the ambient `*system-clock*`. `shadow-store` takes a
  consistent copy for a bulk load while the store keeps serving reads:
  quiesce, close, recursive sparse-preserving copy to
  `<location>-shadow/` (reservations are unwritten holes, so an empty
  multi-gigabyte store copies in seconds, not by materializing every
  reserved byte), reopen the live store and resume service — left
  **read-only** (Kevin's ruling: a write during the shadow window signals
  `store-not-accepting-error` with reason `:shadow-load` rather than
  being silently dropped) until the caller calls `swap-in-shadow` or
  `abandon-shadow`. A copy failure reopens the original store and
  restores full service before re-signalling. `open-shadow-graph` opens
  the copy as an unregistered graph, under the live store's own name and
  store-id (so ids minted there resolve back to the live store), against
  a leased `(start . end)` epoch range persisted in `lease.dat`; its
  allocation cursor is always derived fresh from the shadow's own
  highest committed transaction id, never from a separately persisted
  cursor, and an already-exhausted range signals
  `epoch-lease-exhausted` immediately rather than wrapping or
  colliding. `swap-in-shadow` promotes the shadow: quiesce, close,
  rename the live directory to `<location>-retired-<epoch>` **first**,
  then rename the shadow into the live location — that second rename,
  not the best-effort `:swap` journal record after it, is what "the
  swap happened" means (a crash between the two renames is out of scope
  here, tracked as #171/#212) — and reopens the new generation.
  `discard-shadow` deletes a shadow tree (hard-gated on a `-shadow`
  suffix) and `abandon-shadow` combines that with restoring the live
  store to full service.

  **Recovery policy licenses a fast, non-transactional load.** A store's
  `policy.dat` (`store-recovery-policy` / `set-store-recovery-policy`,
  default `:authored`) records whether a crash mid-load can simply be
  repaired by redoing it (`:derivable`) or whether its writes are the
  only durable record (`:authored`). `make-graph :recovery-policy`
  writes `policy.dat` at creation; `open-graph :recovery-policy` is
  only a hint once the file exists — a disagreeing value signals
  `recovery-policy-mismatch-warning` (naming the location, the
  requested policy and the on-disk one) rather than overwriting it.
  `open-shadow-graph :fast-load t` suppresses the `.txn` file and
  replication log for the shadow's transactions, but only when the
  shadow's copied policy says `:derivable`; otherwise it signals
  `fast-load-requires-derivable` rather than silently keeping the
  store's only record on an unsuppressed WAL.
  **`presize-vector-segment`** turns a bulk load's
  vector-segment capacity hazard into an upfront allocation instead of a
  mid-apply failure discovered after some writes are already durable;
  `open-shadow-graph :expected-vectors n` applies it to every segment
  the shadow's graph object carries.

  V1 ships in-process only — no separate loader process — with the
  epoch lease already shaped to carry that later without a redesign.
  (#170)

- **Whole-system restore across a shadow swap.** `retired-generations`
  lists every `<location>-retired-<epoch>` directory a system's clock
  knows about, joined to its journal record where one exists (a
  directory with none is reported `:journaled nil` and warns
  `swap-record-missing-warning`, the #212 shape; a record with no
  directory is `:present nil`). Each generation carries its ERAS — the
  half-open `[from, to)` intervals of content it actually held, which
  keeps working across a restore-then-swap chain where a promoted
  directory is later retired again under a new name.
  `prune-retired-generations (clock floor &key discard-derivable
  dry-run)` deletes generations at or before `floor`; an `:authored`
  generation still inside the window is refused by name
  (`retention-required-error`) rather than silently discarded, and a
  `:derivable` one is kept unless `:discard-derivable t`.
  `plan-system-restore` / `restore-system (clock epoch &key
  require-exact rebuild timeout)` restore every affected store to the
  generation live at `epoch`, at generation granularity — the swap is
  the *generation* mechanism, and `snapshot`/`replay` remains the sole
  mechanism for content *inside* a generation (this does **not**
  supersede it; point-in-time rewind inside a generation is a separate
  follow-up). A retained generation frozen at or before `epoch` is
  exact; one frozen later is used anyway and reported `:exact nil`
  unless `:require-exact t` refuses instead. A `:derivable` store with
  no retained generation is rebuilt by a caller-supplied `(lambda (name
  graph) ...)`, cascading to any `:derivable` dependent whose edges
  point into the rebuilt store's tag (an `:authored` dependent is left
  alone and reported `:dangling n`). Every refusal — an authored
  generation gone, no rebuild callback, the store not open, a
  replicated/peer graph (v1 is plain-graph-only), an inexact result
  under `:require-exact`, or a stranded interrupted swap — surfaces as
  one `restore-refused-error` naming every `(store . reason)` before
  any rename happens, `:authored-generation-missing`
  `:no-rebuild` `:not-open` `:unsupported-graph` `:inexact`
  `:interrupted-swap`. A manifest (`(:restore t :requested ... :at ...
  :clock ... :stores (...))`) is written to `restore-<epoch>.manifest`
  and returned; `read-restore-manifest` reads it back with
  `*read-eval*` nil. `repair-interrupted-swap (clock name location)`
  fixes the one window `swap-in-shadow` (#170) cannot recover from
  itself — a crash between its two renames — by renaming the stranded
  generation back and journaling `:swap-aborted`; restore refuses to
  start against a store in that state and names the tool. (#171)

- Defining one class name in two stores with *different* slot sets now
  signals `divergent-node-type-redefinition` (a `style-warning`): both
  definitions name one CLOS class, so the last one loaded determines the
  slots and the earlier store's data becomes unreachable through the API
  (the GH #53 failure). Identical slot sets — the multi-store feature —
  stay silent. (#196)

- **A store's persisted type-ids are reconciled with the registry at open,
  or the open is refused** (#186). This is the invariant the rest of the unit
  assumed and nothing established. `instantiate-node-type` keeps a persisted
  id without telling the registry and mints a new one from a counter that has
  never seen that store's ids, so the ordinary upgrade — open an existing
  store under a fresh system directory, then ship one more `def-vertex` —
  minted an id the store already used and `update-node-type` overwrote it:
  every persisted node of the first type then materialised as the second,
  silently. `reconcile-schema-with-registry` now runs before the schema
  replay on every open. A type the registry has never seen, at an id nothing
  else claims, is **adopted** (a single-store deployment therefore needs no
  operator action at all); a type the registry gives a different id, or an id
  it gives to a different type, signals the new
  `graph-db:store-registry-conflict`, naming both sides and pointing at
  `registry-seed-from-stores`. It is also what makes the peer type table
  honest — the table is the registry while the wire carries store ids.

- **`graph-db:with-schema-frozen`** (#186) opens a store exactly as it stands
  on disk, replaying no schema and checking no ids. The supported way to
  *read* a store the registry contradicts — a class census, a backup, the
  before-and-after of an adoption run — which an ordinary open now refuses.
  Writes made through a frozen open go out under the store's own ids.

  **It may read such a store; it may not serve one.** `start-replication`
  signals the new `graph-db:frozen-graph-cannot-replicate` for a master,
  slave or peer graph opened frozen. Every transport puts raw type-ids on the
  wire, so a frozen hub would ship a type table built from the *registry*
  while its node heads carried the store's contradicted ids — and the damage
  would land on a remote peer, where no local guard can see it.

- **A replication handshake refuses a peer whose type registry disagrees**
  (#186). After `:auth-ok`, a device compares the hub's type table against
  this image's registry and signals
  `graph-db::peer-type-registry-conflict-error`, closing the session and
  naming every conflicting symbol package-qualified. Both directions are
  checked, because neither subsumes the other: *one name at two ids* (a node
  arriving under the hub's id would materialise as some other class here) and
  *one id at two names* (the hub's type is unknown in this image and its id
  already means a local type). This is deliberately not a reconciliation —
  agreeing at a handshake would mean rewriting every node of the losing type
  because a network event said so. A hub too old to ship a table sends no
  `:type-table` key and cannot be compared, so it is still trusted; that path
  is kept for pre-#186 hubs. See docs/vivace-graph-v3-doc.org, Chapter 16,
  "Type-ids on the wire, and the handshake that refuses a disagreeing peer".

- **The peer type-table encoder refuses a type-id the wire cannot carry**
  (#186). The registry assigns 32-bit type-ids while the table's `id` field
  is frozen at `(unsigned-byte 16)`. Previously only the reference *parser*
  enforced that, so a hub emitted a row its own parser could not read and the
  failure landed on the device. The encoder now signals at the hub, naming
  the type and the id. Widening the field is a change to a frozen external
  contract and is tracked separately as #199.

- **Adopting global type-ids on an existing system** (#186).
  `registry-seed-from-stores` seeds the type registry from stores that were
  each numbered from 1, and reports which of them must now be rewritten. It
  opens no graph: each store is read from its `schema.dat` and the allocation
  high-water mark in its `heap.dat` header. Stores are offered their ids
  **largest on disk first**, and each keeps every id it can, so the store
  that costs most to rewrite wins every contest — the cost of adoption is
  bytes replayed, not types moved. (Measured on a five-store system: 66
  vertex type names competing for 36 ids, and the store holding 59 of the 95
  types was among the smallest, so seeding by type count would have replayed
  ~4.9 GB instead of ~1.1 GB.) The returned `seeding-report` names the seed
  store, every id that moves, the stores to migrate, and any name a single
  store's history left holding **two** ids — a case no seeding policy
  exempts, since those must unify whichever store wins. See
  docs/vivace-graph-v3-doc.org, Chapter 17, "Adopting global type-ids on an
  existing system".

- **`migrate-graph` gains `:renumber-p`** (#186), default `nil`. `nil`
  preserves the source's type-ids exactly as #166 built it. `t` takes every
  type-id from the system registry instead, so a renumbered store's ids mean
  the same thing in every other store of the system; this is the migration
  half of the adoption procedure above. `migrate-graph` now returns
  `(values new-graph unified)`, where `unified` names each type whose several
  ids collapsed into one. #166's migration tests were renamed to say which
  mode they pin (`migrate-v1-graph-to-v3-without-renumbering`,
  `migrate-v2-graph-to-v3-without-renumbering`) — the type-id guarantee is
  mode-dependent now, and the renumbering path is the exact reverse of what
  they assert.

- **The image-level system clock** (#168). `open-system-clock` /
  `close-system-clock` open a durable, crash-safe epoch counter shared by
  every store attached to it, in place of each store's own per-graph
  `transaction-id` counter. `*system-clock*` defaults to `nil`; with no
  clock bound, `make-graph`/`open-graph` and every transaction-allocation
  path behave exactly as before #168. `make-graph` and `open-graph` gain
  `:system-clock` (default `*system-clock*`); attaching a store raises the
  clock above that store's own persisted history (its highest committed
  transaction id, and for a peer-graph, its pull-cursor too, since those
  are distinct number spaces) so the clock can never reissue an epoch that
  store has already used, and refuses while the store has an in-flight
  transaction. See docs/vivace-graph-v3-doc.org, Chapter 17, "The
  image-level system clock (optional)".

- **Epoch leases and the store lifecycle journal** (#168). `clock-lease-epochs`
  reserves a range `[start, end)` on a system clock and skips the clock past
  it, so a store being detached can allocate offline from its own range with
  no further coordination — the mechanism #170's shadow-generation swap will
  use. `journal-append` / `journal-records` keep a small append-only journal
  of store lifecycle events (`:create`, `:detach`, `:swap`, `:attach`,
  `:retire`) beside the clock's own counter file; `attach-to-system-clock`
  already appends an `:attach` record. `journal-records` reads with
  `*read-eval*` bound to `nil` — the journal is data and is never evaluated.
  Both are new, currently-unconsumed primitives that #170/#171 build a
  restore path on. See docs/vivace-graph-v3-doc.org, Chapter 17.

- **`peer-observe-epoch` observes the image clock** (#168). A pulled node
  carries the *hub's* commit epoch; when the pulling store has a system
  clock bound, that observation now raises the clock instead of the store's
  own `tx-id-counter` (which a bound clock leaves dead). Without a clock the
  original per-store counter path is unchanged. Consequence worth knowing:
  the image clock is therefore not purely local — a peer sync on one store
  can advance the whole image's clock, driven by another image's clock.

- **Cross-store read snapshots pin every participating store** (#168).
  `with-read-snapshot` composes by nesting, and each nested call now takes a
  read-epoch pin on its own graph's transaction manager for the snapshot's
  extent, so a cross-store composition ends up pinning every store it
  touches, not just the innermost or outermost one. Named cost: a long
  cross-store query delays reaping in every store it touched, for its full
  duration — the intended price of a shared instant across stores, not a
  regression.

- **Registration — binding a record's geometry to a registry's regions**
  (#138). `graph-db/spacetime` gains `register-geometry` and `register-node`,
  which turn a source's `:REGISTRATION` facet into one claim per region the
  subject overlaps. Registration is **partial and fractional, not boolean**: a
  point registers at fraction 1.0, a polygon at its share of each region by
  AREA, a line at its share by LENGTH — a line's area is zero, so an area ratio
  would give it 1.0 in every region it crossed.
  - **Both geometries are repaired with `geometry-make-valid` before
    intersecting, and `fraction` is clamped to 1.0.** An invalid ring can clear
    the spatial index's `intersects` refinement and then throw inside
    `GEOSIntersection`, which would refuse the *whole* subject and drop every
    region it genuinely overlaps — the host-dependent invalid-polygon case that
    partial coverage exists to report, turned into a total loss. The clamp
    holds `fraction`'s documented `[0,1]` contract — and is pinned by a
    DIRECT unit test on `%overlap-fraction`, because end to end it cannot be
    made to fire: with subject and region both repaired the intersection is a
    subset of the subject, so the ratio never exceeds 1 by more than float
    noise.
  - **`fraction` and `precision-m` join `+claim-shared-slots+`**, so every
    claim of every tenant carries them rather than a tenant declaring them in
    `:extra-slots`. A retrieval layer weighting expansion by overlap is
    domain-neutral and cannot know each tenant's accessor names. `precision-m`
    is a magnitude in METRES (or `nil`) and is **not** the `:space` facet's
    `:precision` keyword; `fraction` is a ratio defaulting to `1.0d0`.
  - **The `:REGISTRATION` facet carries a payload** where #132 stored an
    opaque value nothing consulted: `:registry` (which must name a
    `def-source` class, since a claim's object endpoint is
    `(object-namespace object-key)`), `:registry-namespace` (a KEYWORD),
    `:claim-class`, `:producer`, `:relation`, `:method`, `:rule-version`,
    `:precision-fn`, `:confidence-fn` and `:method-fn`. `:none` stays fully
    supported.
    ⚠ **`:method-fn` is REQUIRED, and that constrains the upgrade order.** A
    tenant already on an older engine must declare the key on every
    `:registration` facet — `NIL` where that source's method is a source-wide
    constant — and land that change *first*; taking this version before it
    fails `def-source` at MACROEXPANSION for every faceted class. Declaring it
    early is inert on the older engine, which accepts and ignores facet keys
    it does not know.
    `standing` is deliberately not a field: a registration is derived by
    computation, so every claim written carries `:inferred`.
  - **`geometry-geodesic-area`** (m², by spherical excess, holes subtracted)
    and **`geometry-geodesic-length`** (metres, haversine folded over
    consecutive vertices) are CORE geometry ops — neither needs
    `graph-db/geos`. Not to be confused with `geometry-area`, which returns
    SQUARED DEGREES and does need the add-on; a degree of longitude is a
    different distance at every latitude, so a ratio of two such areas is only
    accidentally right.
  - **A refusal is a first-class result.** Both functions return `evaluated-p`
    as a second value, and `(values nil nil)` means "the scan was never
    answered", never "no region here". Three things refuse: no GEOS for an
    extended geometry (the index falls back to an over-inclusive bounding box,
    so approximating would write false positives), a geometry GEOS rejects as
    invalid (which polygons those are is host-dependent), and an intersection
    whose kind this engine's `geometry` type cannot represent.
    `register-node` adds a fourth — a subject whose geometry is unset or
    unreadable. None of them signals, and the handler catches `geos-error` and
    nothing wider, so it cannot swallow a cross-graph node escape (#53).
  - **`register-node` is idempotent** on the full `def-unique` binary tuple,
    `producer` included, so a re-ingest updates its claim instead of doubling
    it; a region the subject merely TOUCHES has a zero fraction and is dropped
    rather than written. `:graph` is where the subject is read,
    `:registry-graph` where the regions live and the claims are written — only
    plain values cross between them. Manual: Chapter 18, "Registration:
    binding geometry to a registry".

- **The general ordered index is reachable from Prolog** (#102). Two *generating*
  predicates — `(find-by-slot ?node CLASS SLOT VALUE)` for equality and
  `(find-slot-range ?node CLASS SLOT START END)` for an ascending range — bind
  `?node` once per hit. Previously the index accelerated Lisp callers and was
  invisible to the query language: `node-slot-value/3` is a *filter*, not a
  lookup, so a query over an indexed slot generated candidates with `is-a/2`
  (every instance of the class) and tested them one at a time — O(instances)
  against an O(log n) index already maintained on every commit.
  - `?node` comes first, matching the other generating predicates
    (`find-within/3`, `find-near/5`) rather than the filtering `geo-*` ones.
  - A subclass argument resolves to the owning index; an unindexed slot
    **signals** (silence would make "no index" look like "no rows", and a scan
    fallback would make the cost unpredictable); a *declared but unbuilt* index
    correctly yields nothing.
  - Either range bound may be `nil` **or left unbound** for open-ended.
  - `find-slot-range` streams via `map-index`, so an early cut does not
    materialise the whole range.
  - Not included: making the **planner** index-aware, which is separate work.

- **Named schema declarations and retraction** (#139, #140). `def-index` and
  `def-unique` take an optional `:name`; a named declaration is identified by
  `(owner . name)` rather than `(owner . slot-names)`, so **re-declaring the name
  replaces it whatever the slots became**. Unnamed declarations keep slot-name
  identity and behave exactly as before. New `undef-index` / `undef-unique`
  withdraw a declaration by name or by slots; withdrawing something never
  declared is a no-op.
  - **Why naming, rather than an unregister keyed by slots:** a macro that emits
    schema on a caller's behalf cannot name what a *previous version of itself*
    emitted, so slot-name identity gives it no way to express "this changed
    shape". Both specs stayed live — the stale `def-unique` rejecting writes the
    current schema permits, the stale `def-index` built and maintained for
    nothing, with no retraction path anywhere in the image.
  - `def-source` and `def-claim-classes` now name every spec they emit (one
    unique for the former; two uniques and three indexes for the latter).
  - **Registration replaces in place**, so `*schema-index-metadata*` and
    `*schema-unique-metadata*` hold one entry per logical declaration rather
    than one per *evaluation* — both tables are scanned linearly and previously
    grew forever in a long-lived image that reloads schema.
  - **The sidecars are reconciled against the live schema at open.**
    Maintenance is spec-driven while reopen was sidecar-driven; those agreed
    only while a spec could never go away. Without this, a withdrawn index would
    be reopened, left unmaintained, re-saved at close, and still be *readable*
    via `index-lookup` — a stale index that answers queries, which is worse than
    the useless one retraction removes. Reconciliation fails safe towards
    keeping: positive evidence is required to drop a record, never the absence
    of evidence to keep it.
  - Not included: reclaiming a retired index's heap pages (#147).

- **`claims-by-producer`** (#145) — the non-destructive counterpart to
  `delete-claims-by-producer`, returning every live claim a producer wrote across
  both arities. Same contract as the sweep: parent claim class, the producer index
  (so O(matching), not a scan), `unknown-claim-family` on an unregistered class,
  and `NIL` for a producer that has written nothing. Swept claims are not
  returned. Fills the audit direction `claims-touching` cannot serve — that one
  answers only for an endpoint the caller already names, so it structurally cannot
  find a claim nothing justifies, which is the same orphan case the uniqueness
  constraint cannot catch. Purely additive; nothing existing changes behaviour.

- **Multi-slot (tuple) keys for `def-index` and `def-unique`** (#107). Both macros
  now accept a *slot list* — `(def-index claim (ns key rel) :app)`, `(def-unique
  claim (ns key) :app)` — giving an ordered index or a uniqueness constraint over
  the tuple, keyed left to right; a bare symbol still works unchanged, as the
  arity-1 case of the same machinery. Query a tuple index with a value list:
  `(index-lookup graph 'claim '(ns key rel) (list "ops" "e1" "at"))`.
  - `:canonicalize` on a multi-slot index takes a *positional list*, one entry
    per component (`nil` = identity); a single function designator still applies
    to a single-slot index exactly as before. A positional list whose length
    doesn't match the index's arity now **signals** rather than silently
    truncating or padding — nothing shipped relied on the old behavior.
  - **Footgun, pre-existing and unchanged by this work**: the arity check above
    applies only to a *list*. A bare function designator (`string-downcase`,
    not `(string-downcase nil nil)`) is legal on a multi-slot index too, and is
    silently applied to component 0 only — every other component stays
    identity, with no signal. Use a positional list, padded with `nil`, to
    canonicalize more than the first component.
  - `index-lookup` takes `:prefix t` for a value list shorter than the index's
    arity (a prefix scan); without it, a short list **signals** rather than
    silently returning a wider result than asked for. Too many components
    always signals, `:prefix t` or not, on both `index-lookup` and
    `index-range`.
  - **The null asymmetry, worth stating plainly**: an ordinary index *stores* a
    null component under a sentinel, so the row stays findable by a prefix scan
    of its populated components; a `def-unique` constraint instead *exempts*
    any tuple containing a null component, matching SQL's NULL-never-equals-
    NULL. Two rows agreeing on every populated component but both `nil`
    elsewhere therefore do not collide.
  - `def-unique`'s build is **strict** (signals on a pre-existing duplicate)
    only when a constraint is newly declared against an already-open graph;
    it is **tolerant** (logs and keeps the first) when reconciled at graph
    open, matching the existing single-slot `:unique` split — multi-slot
    extends that policy rather than introducing a new one.
  - The peer pull-apply paths (`apply-peer-authored-op`,
    `apply-peer-create-writes`) maintain multi-slot indexes and constraints the
    same as the local-commit path, since all three route through the same
    apply functions — verified directly rather than assumed, with dedicated
    peer-suite coverage.
  - **No rebuild and no on-disk storage-version change** for existing
    single-slot indexes or constraints — a single-slot index is simply the
    arity-1 case of the tuple machinery. Manual: Chapter 8, "Multi-slot (tuple)
    indexes and unique constraints".

### Fixed

- **`migrate-graph` no longer pollutes the type registry** (#186). Creating
  the destination graph ran `update-schema`, which interned every one of the
  graph's types and assigned them real ids; `migrate-graph` then installed
  the source's schema over the top, discarding those ids and leaving the
  registry permanently holding entries at ids no store uses. Opening the
  *source* had the same effect for any type declared in the image but absent
  from that store — and wrote a registry id into a store whose every other id
  was per-graph. The schema replay is now suppressed for both of
  `migrate-graph`'s opens, since it installs the schema it wants by hand in
  either mode; a `:renumber-p nil` migration therefore leaves the registry
  untouched, which is the only answer consistent with preserving the source's
  own ids. One consequence for the record: `migrate-graph` no longer rewrites
  the source's `schema.dat` at all.

- **`open-system-clock` let two processes both allocate epochs for the same
  system directory, silently** (#182). The image-level clock (#168) had no
  cross-process exclusion: two images opening one clock directory both read
  the persisted ceiling, both reserved a block, and both started issuing
  epochs from overlapping ranges — destroying the one property the clock
  exists to provide, that no two transactions in the system share an epoch.
  `open-system-clock` now takes an exclusive, non-blocking `flock(2)` on
  `system-clock.lock` (a sibling of `system-clock.dat` and
  `system-journal.log`) before touching the ceiling file, and signals
  `system-clock-in-use` (with a `system-clock-in-use-location` reader) when
  the lock is already held. Non-blocking is deliberate: a blocking wait
  would present as a startup hang with no diagnostic. `close-system-clock`
  releases by closing the fd — there is no `LOCK_UN` path — and the kernel
  releases the lock automatically if the holder dies, so a crashed process
  leaves no residue for the next opener to clean up.

  **No recovery step, deliberately.** The counter is already crash-safe:
  `%write-clock-ceiling` persists `ceiling + block-size` before any id in
  that block is issued, so a crashed process's successor simply resumes
  above the persisted ceiling and never reissues. A `.dirty`-style marker
  or other recovery path would force manual intervention for a condition
  the ceiling protocol already handles correctly, so none was added.

- **The memory-graph native image (`VGMI`) packed `type-id` at 2 bytes and
  truncated silently; format bumped to v8** (#187). `ni-uint` writes with
  `ldb`, so a `type-id` above 65535 lost its high bits with no signal — 70000
  was restored as 4464, yielding either a node built against the **wrong
  class** or a `NIL` type lookup. Six sites were affected: the node record
  (both the live-node and `LZNODE` arms, plus its reader) and all three index
  key codecs (`type`, `ve`, `vev`).

  Unreachable while type-ids were per-graph and handed out from 1, but #166
  widened the on-disk field to 32 bits and #186 makes ids global, so this was
  the last 16-bit narrowing on the path. It mattered more than the equivalent
  assumption on the replication wire (`peer-streaming.lisp`), which validates
  and **signals**: this one failed silently, and a memory graph's image is its
  **only** durable copy — the journal is cleared on every checkpoint.

  **v5, v6 and v7 images still open.** Record and key layouts are parsed
  positionally, so `%read-memory-image` selects the type-id width from the
  version and threads it through the record reader and all three key readers;
  reading a v7 image at v8's width would shift every field after the type-id.
  Writers always emit 4 bytes. The width is a defaulted parameter on each
  codec rather than a duplicated set of `*-v7` functions.

  `ni-type-id` replaces the bare `ni-uint` on every type-id write and
  **signals `memory-image-type-id-too-wide`** rather than truncating, so the
  silent-narrowing class of bug is gone and not merely this instance of it.


- **`recreate-graph` (restore/replay) minted ids from a per-store scalar,
  bypassing both the transaction manager and any bound system clock**
  (#168). Reached via `replay` (snapshot and backup restore) and
  `migrate-graph`. Under a shared clock this could reissue an epoch some
  other store had already committed at — exactly the collision a system
  clock exists to prevent. Restore now allocates through `tm-next-epoch`,
  which draws from the system clock when the graph has one and otherwise
  falls back to the store's own counter (unchanged behaviour with no clock
  bound).

  **Behaviour change, independent of any clock:** the watermark restore
  persists afterward is now the last id actually used, not one past it —
  the old code always wasted one epoch on the trailing `+1`, including on
  an empty snapshot, which bumped the watermark for zero work. This brings
  `recreate-graph` in line with `apply-transaction` (which persists the id
  it actually used) and a fresh transaction manager's own `(1+ (max ...))`
  re-seeding; it closes a gap that was always wasted, it does not shift
  any restored node's id. Anyone who read a restored store's persisted
  highest-transaction-id and expected it one past the last restored id
  will now see it one lower. Separately, under a bound clock, restored ids
  also stop being dense from a fresh store's own zero — they draw from the
  clock's shared, current position instead, which is the point of the fix.

- **`GEOSMakeValid` returning a `GEOMETRYCOLLECTION` refused the whole
  subject** (#163). A repair that splits its input across dimensions — the
  polygonal area plus the zero-width slivers the repair shed — comes back from
  GEOS as a `GEOMETRYCOLLECTION`, which `wkt->geometry` has no kind for, so
  `geometry-make-valid` signalled. `spacetime::%repaired`'s `ignore-errors`
  then handed back the **unrepaired** ring, `geometry-intersection` threw on
  it, and `register-geometry`'s `geos-error` handler returned
  `(values nil nil)` — losing every region the subject genuinely overlapped,
  the exact loss the repair was added to close.

  **`geometry-make-valid` now keeps the collection's polygonal components and
  returns their union**, which is what `GEOSMakeValid` callers are expected to
  do: the linear components are degenerate slivers, the polygons are the
  repaired area. Valid geometry is untouched — `GEOSMakeValid` answers it with
  a `POLYGON`/`MULTIPOLYGON`, which takes the same path it always did — and
  without the add-on the base method still signals
  `geos-required-for-operation`.

  **A repair with no area in it signals `geos-error`** rather than yielding an
  empty polygon. Nothing was repaired, and "covers nothing" is a
  *measurement*: `%overlap-fraction` reads a zero-measure subject as fraction
  1.0 in *every* candidate region, so fabricating one is the same fault
  inverted. `%repaired`'s contract — something usable, else the original — is
  what the caller wants there. ⚠ The test is on the **emptiness of the
  result, on both paths**, not on whether any polygonal component was found:
  `POLYGON EMPTY` has the type id of a polygon, so a component-count test alone
  passes it through inside a collection, and a *top-level* one is not a
  collection at all. No reachable input is known for either — `GEOSMakeValid_r`
  defaults to linework mode, which keeps degenerate linework as lines rather
  than collapsing it to an empty polygon, and the structure mode that yields
  empties is not bound — so covering both is defensive consistency, not a live
  bug; a guard with a gap in it reads as covered.

  Three bindings are new in `geos/geos-ffi.lisp`: `GEOSGeomTypeId_r`,
  `GEOSGetNumGeometries_r` and `GEOSGetGeometryN_r`. ⚠ `GEOSGetGeometryN_r`
  returns **internal storage owned by the parent collection** — unlike every
  other geometry the bindings hand back, it must NOT be destroyed. The fold
  over the extracted parts frees only the intermediate unions it creates
  itself.

  Measured on the deployed data that found it: 7 of 4,196 subjects took this
  path and lost every region. Regression tests cover the repair
  (`tests/geos/makevalid-tests.lisp`) and the registration it broke
  (`tests/spacetime/register-tests.lisp`).

- **Every `LOCAL-TIME:TIMESTAMP` before 2000-03-01 was silently corrupted on
  read** (#153). `SERIALIZE` writes `day-of` — which is negative before
  local-time's epoch — through `LDB`, but `DESERIALIZE-HELP` read it back with
  `DESERIALIZE-UINT64`, so a day of `-61` returned as `18446744073709551555`
  and the timestamp came back with a nonsense year. No error was signalled at
  write or at read.

  **Read-side only, so no migration is needed.** The bytes on disk were always
  a faithful two's-complement representation; only the read was wrong.
  Deploying the fix recovers every affected timestamp already stored. Anything
  that had already *derived* a value from a corrupted read is not recovered.

  `DESERIALIZE-UINT64` itself is untouched — the rest of the storage layer uses
  it for pointers, sizes and packet lengths, which are genuinely unsigned. The
  timestamp codec now sign-extends its `day` field alone; `sec` and `nsec` are
  normalised non-negative by local-time.

- **Multi-slot index/constraint defects found by the whole-branch review** (#107).
  All five were reproduced before being fixed, and each carries a regression test
  confirmed to fail against the unfixed code.
  - **A memory graph carrying any `def-unique` was unopenable after a clean close.**
    The checkpoint image's unique-index dump had no multi-slot branch — it wrote the
    singular slot name, which is `nil` for a tuple index — and the reopen fed that to
    the single-slot resolver, ending in `(fdefinition nil)`. Since the image is the
    only durable copy of a cleanly-closed memory graph (the journal is cleared at
    checkpoint), this was data loss. The dump now records the slot *list*, and the
    loader dispatches on its shape, exactly as the on-disk sidecar already did. An
    image written by the broken code is skipped with a warning and its constraint
    rebuilt, rather than failing the open.
  - **A lazy memory graph reopened with a `def-unique` silently absent** whenever the
    image did not carry it, because the open-time install sat inside the
    `(unless (lazy-p graph) …)` block. It now runs on the lazy path too — the same
    trade `rebuild-unique-indexes` already makes, since a constraint that stops
    enforcing is worse than materializing its owner's blobs. It scans per owner type,
    so unrelated classes stay unmaterialized.
  - **`regenerate-secondary-indexes` silently emptied every `def-index`-only index.**
    Its guard consulted only the MOP `:index` slots — and every multi-slot index is
    `def-index`-only, there being no MOP surface for a tuple — so the rebuild no-opped
    after the delete and an empty sidecar was persisted. Lookups then returned empty
    instead of signalling. The guard now consults the `def-index` registry the way its
    `:unique` counterpart already did, and `regenerate` runs `install` after `rebuild`
    as `open-graph` does, so a declared index whose owner has no live node is
    recreated rather than dropped.
  - **An all-null query prefix returned wrong answers.** The query-side key builder
    computed its "every component is null" gate over the components *given* rather
    than over the index arity, conflating an all-null prefix — a real query, since the
    write side stores a null component under a sentinel and the row does sit there —
    with "no key at all". A prefix lookup missed those rows and a range bound went
    open-ended. The write side was always correct.
  - **A failed strict `def-unique` left a half-built, live constraint.** The index was
    published before the scan and the strict path signalled on the *first* duplicate,
    so the constraint covered only the prefix scanned before the error and duplicates
    in the un-scanned tail committed unchecked. The strict signal is now deferred to
    the end of a complete scan, so the constraint left behind is whole (keep-first on
    the duplicate) and enforcing; a scan that dies for any other reason unregisters
    what it created, so the build can be retried.
  - The secondary and unique sidecar readers' `handler-case` now spans the per-record
    loop, not just `cl-store:restore`: a sidecar can deserialize cleanly and still
    hold a record shape this build does not know, and that must degrade to rebuild
    like a torn write rather than failing `open-graph`.

- **A failed snapshot no longer aborts `close-graph`** (#120). `close-graph` deregisters
  the graph from `*graphs*` and *then* snapshots, with nothing guarding the call — so a
  snapshot that signalled left every mmap open (heap, indexes, vertex/edge tables, vector
  segments), `.dirty` still on disk forcing recovery on the next open, and no way to reach
  the graph by name to retry. The on-disk data was intact the whole time.
  For a disk graph the snapshot is a *logical backup*; durability is the heap/lhash mmaps
  plus the transaction journal, so losing one snapshot only means the next `replay` starts
  from an older snapshot plus more journal. The close now completes and the failure is
  reported rather than swallowed: `close-graph` returns `(values graph snapshot-problem)`,
  logs at `:error`, and `warn`s once the teardown is done.
  The handler is on `serious-condition`, **not** `error`: SBCL's `heap-exhausted-error` is
  a `storage-condition`, which is not an `error` subtype, and heap exhaustion on a large
  graph is the failure this exists for (#119). Application code guarding a snapshot with
  `(handler-case … (error (e) …))` has the same gap.
  `snapshot` reports integrity problems by *returning* `:data-integrity-issues` rather than
  signalling, and `close-graph` discarded its return value entirely — so such a graph closed
  with no snapshot taken and no indication of it. That is now the same second value.
  Deliberately unchanged: the three `save-*-index-roots` calls above the snapshot still
  abort the close. Those sidecars write atomically (temp + rename, #63), so a failure leaves
  the *previous* sidecar naming the *previous* roots; closing past that and clearing `.dirty`
  would let the next open adopt roots that no longer match the index, with no recovery pass
  to catch it. A missing snapshot costs replay time; a stale index root is silently wrong.
  The memory-graph path is also unchanged — it overrides `snapshot` to `nil` and checkpoints
  in a `close-graph :before` method, where failure *should* be fatal because the image is
  that graph's only durable record.

- **A slot mutation made after `MAKE-<TYPE>` in the same transaction was
  discarded** (#135). `MAKE-<TYPE>` serializes `BYTES` once at construction;
  a `SETF` between construction and commit updated `DATA` alone, and
  `MAYBE-INITIALIZE-BYTES` only fills an *empty* `BYTES`, so the
  construction-time value was what persisted. The value read back correctly
  for the rest of the session — the node cache serves `DATA` — and came
  back `NIL` after reopen. The `tx-update` path had always re-serialized
  for this reason; the create path never did.
  The fix went through two iterations, caught by a subsequent whole-branch
  review: refreshing `BYTES` in `APPLY-TX-WRITE (tx-create)` fixed a disk
  graph's own heap, but `%COMMIT` serializes the durable record (the `.txn`
  file, and — for a graph that journals its own feed — the replication log)
  in `PREPARE-TX-PERSISTENCE`, which runs *before* `APPLY-TRANSACTION`. So
  the journal and replication log still carried the pre-mutation bytes,
  permanently, no crash required — worst on a memory graph, which keeps
  every committed `.txn` as its durable journal until a clean-close
  checkpoint (the Android/ECL production backend). The refresh now runs in
  `PREPARE-TX-PERSISTENCE`, over the transaction's create-set, before that
  record is written; `APPLY-TX-WRITE (tx-create)`'s own copy was removed,
  returning that apply path to master's shipped behavior, which never
  re-serialized there either.

- **`COPY` of a node created in the same transaction corrupted the graph**
  (#135). It built a `tx-update` whose `OLD-NODE` was a pending create.
  The transaction committed, the graph closed, and `OPEN-GRAPH` then
  *succeeded* — the damage was in the node, not the graph: reading a data
  slot back signalled `DESERIALIZATION-ERROR` (`Deserialization failed
  for #(0 0)`). The exact mechanism is not established — an earlier
  explanation (a race with `ARCHIVE-NODE-VERSION`) was disproven by
  tracing the apply order, and no replacement has been confirmed. It now
  signals the new `COPYING-UNCOMMITTED-NODE` at the `COPY` instead of
  committing.
  The create-set membership check this guard (and the slot-mutation guard
  below) relies on was itself keyed by node id, not node identity — any
  instance carrying a re-created id (`MAKE-<TYPE>` accepts `:ID`) passed,
  letting exactly the shared-cache mutation this branch exists to stop
  through. Both guards now check by `EQ` against the create-set's
  registered node.

- **`interface.lisp` was missing an ASDF dependency on `transactions`**
  (#135), the file that defines `%COPY`, `COPYING-UNCOMMITTED-NODE` and
  `NODE-CREATED-IN-TRANSACTION-P`, all used by `COPY` above. The gap
  predates this branch — master's `interface.lisp` already called
  `UPDATE-NODE` (also defined in `transactions.lisp`) from `SAVE` — and it
  built only because `graph-db.asd` happens to declare `transactions`
  earlier in the component list; `graph-db/core`'s `:depends-on` now says
  so explicitly.

- The schema's bare (keyword) type-name lookup was package-blind: two
  same-named types in different packages silently clobbered one alias
  entry, and REST/DSL callers got whichever class was defined last. A bare
  name now resolves only when unique and signals
  `ambiguous-node-type-name` otherwise; the alias key is no longer
  written, and stale aliases in old `schema.dat` files are ignored.
  (#190)

- A torn final record in the system-clock journal (a power loss
  mid-append) made every record unreadable, since the reader signalled
  on the incomplete form. The reader now drops a torn tail — truncating
  it atomically and signalling the `system-journal-torn-tail` warning —
  and returns the intact history; damage anywhere other than the tail
  still signals, as `system-journal-corrupt`. The writer is unchanged:
  per-record fsync was considered and rejected in the issue. (#191)

### Changed

- **BREAKING: a `make-<type>` constructor's `:graph` default is now the
  class's declared store, not the ambient `*graph*`** (#167). The
  trailing argument to `def-vertex`/`def-edge` is documented from here
  on as the class's *default store*; omitting `:graph` places the node
  there, not wherever `*graph*` happens to be bound at the call site.
  If that default store is not open and no `:graph` was passed, the
  constructor now signals the new `default-store-not-open-error`
  (naming the class and the store) rather than silently writing into
  `*graph*` — placement determines recovery policy (which store's WAL,
  backup schedule and detach boundary a node falls under), so a quiet
  substitution would change a node's durability story without saying
  so. Single-package/single-store code is unaffected: there, `*graph*`
  already *is* the declared store on every call. The behaviour change
  reaches only code that relied on `*graph*` differing from a class's
  declared store to place nodes elsewhere without passing `:graph`.
  `lookup-<type>` and the generic `make-vertex`/`make-edge` keep their
  existing `*graph*` behaviour; they take ids or explicit types, not
  class policy.

- **BREAKING: peer wire protocol bumped 1 -> 2; a v1 device is now
  refused, not misparsed** (#206, #201). This is a wire-generation
  change — every peer device and hub must move together. The v2
  contract, in four points: (1) the peer wire now carries v3 node
  heads (33-byte vertex / 73-byte edge, #166's on-disk format) instead
  of the old v1 layout; (2) the type table's `name` and `supers`
  fields are downcased, **package-qualified** names
  (`package:symbol`), so two same-named types from different packages
  no longer collide on the wire (#201); (3) the type table the hub
  ships in `:auth-ok` is now **scoped to the replicated store's own
  schema**, closure-completed over `supers` so no row's superclass
  reference dangles; (4) the device auth plist **must** carry
  `:peer-protocol-version` — absent (a v1 device) or mismatched
  refuses the connection with `peer-protocol-mismatch-error`, checked
  hub-side before the schema-compatibility gate and before any
  mutation. A stale-but-present node-head size is refused too
  (`node-head-size-mismatch-error` in `transactions.lisp`), as defense
  in depth below the version gate. The wire *grammar* (4-field
  `kind,id,name,supers` type-table rows; `:` still unreserved) is
  unchanged. Coordinates with mine-action-android#29.

- **The hub resolves its type registry on the thread that starts
  replication** (#186). A hub serves each device connection on a new thread,
  and a new thread does not inherit dynamic bindings, so a session calling
  `ensure-type-registry` read the *global* `*system-directory*` — `nil`, so
  auth-ok failed on every connection and the device saw a closed socket, or
  worse, another system's directory. `start-replication` now captures it on
  the caller's thread (`peer-type-registry` on the graph).

- **A store that owes a renumbering says so at open** (#186). An id the
  store occupies but its own name lookup no longer returns — orphaned
  metadata, with nodes still on disk under it — is tolerated when it sits at
  or below the registry's high-water mark, and now emits a `log:warn` naming
  the type and both ids instead of passing silently. Above the mark it is
  still refused: the registry would hand that id to another type and there is
  no way to reserve it. The tolerance is a policy choice about
  already-orphaned metadata rather than a proof of safety — see #202.

- **Seeding breaks a size tie on the store location** (#186).
  `registry-seed-from-stores` ranked stores by heap high-water mark with a
  `sort` that is not required to be stable, and equal marks are ordinary
  (fresh or empty stores), so two images could rank one store set differently
  and seed two different registries.

- **The out-of-process harnesses set a system directory** (#186). Every
  script under `tests/replication/`, `tests/peer-replication*/`, the
  profiling modules and the `example.lisp` / `test.lisp` / `test-mop.lisp`
  scratch files called `make-graph` without one and had been failing
  outright since a system directory became mandatory; none of them is in the
  FiveAM suite, so its green hid this. Each peer harness process now takes
  its *own* system directory under `REPL_WORK`, which is what makes them
  faithful: hub and device are separate images with separate registries that
  agree because both evaluate one `schema.lisp`.

- **The peer type table is the image's type registry, not one graph's
  schema** (#186). The `:type-table` string the hub ships in its `:auth-ok`
  plist now names every type this system has assigned an id, because type-ids
  are image-level and a device may be sent an id belonging to any store.
  `peer-type-table-string` takes a registry (defaulting to this image's)
  rather than a graph. The wire *grammar* is unchanged — it is a frozen
  external contract parsed by non-Lisp peers — only what fills it. Two
  consequences, both intended: a type the hub's own graph does not
  instantiate still appears in the table, and a type name that cannot be
  encoded now fails every device connection in the image rather than only
  sessions for the store that declared it. A third consequence is not
  intended and is tracked as #200: a registry entry whose class this image
  never loaded emits an empty `supers` field, which the wire cannot tell
  apart from a type that genuinely roots at `vertex`/`edge`.

- **The downcased-name collision error names the packages, and no longer
  advises a rename** (#186). Pooling every store's types into one table
  widened that collision surface: two types that never shared a schema now
  share one namespace, and same-named symbols from two packages are no longer
  exotic. The error prints both symbols package-qualified — the package is
  the only thing that distinguishes them — and says the fix is a retirement
  or rename, which for a type with nodes on disk is a store migration rather
  than an edit.

- **The renumbering migration mints in a deterministic order** (#186).
  `renumber-schema` iterated survivors with `maphash`, so two images
  renumbering one store into two empty registries could assign different ids
  — a disagreement the new handshake guard would then refuse, though nothing
  about the two systems actually differed. It now mints in package-name then
  symbol-name order. This makes a single migration reproducible (and so
  verifiable by re-running it); it is *not* a substitute for distributing the
  registry, and images that opened different stores still disagree.

- **BREAKING: type-ids are assigned system-wide, and a system directory is
  now mandatory** (#186). `graph-db:*system-directory*` names the directory
  holding this system's shared state; type-ids are assigned there, in an
  append-only registry (`type-registry.log`) keyed on the package-qualified
  type name, in place of each graph's own counter. One type name therefore
  means one id in every store of the system, and no two names share an id.
  Vertices and edges remain separate id spaces.

  **The special has no default and is required.** `make-graph` and
  `open-graph` signal the new `graph-db:system-directory-required` when it is
  `nil`. There is deliberately no per-graph fallback: two id regimes drifting
  apart unnoticed is the failure the registry exists to prevent, and a
  fallback would make the divergence invisible. Set it once, from your
  application's configuration, before opening anything:
  `(setf graph-db:*system-directory* "/var/lib/my-system/")`.

  Existing stores keep the ids already written into their nodes — reopening
  one replays its persisted schema and does not renumber. Adopting the
  registry for a system whose stores were numbered independently is the
  seeding-plus-renumbering procedure added below.

  A store's type-ids are now **sparse**: it holds the ids of its own types,
  wherever those landed in the system's numbering, not a dense run from 1.
  Code that enumerated types by counting from 0 to the schema's `next-*-id`
  must enumerate the schema's actual ids instead; `gc.lisp`'s mark phase did
  exactly that and now uses `list-vertex-types`/`list-edge-types`. The type
  index is sized by the highest id a store holds rather than by how many
  types it has.

- **BREAKING: a node class may now be defined for more than one graph**
  (#186). The cross-graph name check existed only because type-ids were
  per-graph; with a system-wide id space it has no job, so it and its call
  are gone. `duplicate-node-class-error` is no longer signalled by anything.
  The condition remains exported so existing handlers still compile. Note
  that both definitions define the *same* CLOS class, so the last one loaded
  determines its slots — keep them identical, or use different type names.
  See the manual, Chapter 17, "Class names are global, and one class may
  serve several stores".

- **The temporal algebra now lives in its own library**,
  [cl-temporal-extent](https://github.com/kraison/cl-temporal-extent) (#159).
  Bounds, extents, the Allen relations and the standing vocabulary never
  depended on the graph — the only occurrence of `graph-db` in any of those
  files was the `in-package` form — so they are now usable without loading a
  database engine. `graph-db/spacetime` depends on the library and keeps the
  claim record, the source-onboarding contract and endpoint resolution.

  **No consumer needs to change.** `graph-db.spacetime` `:use`s the new
  package and re-exports every symbol it previously exported, so code
  written against `graph-db.spacetime:make-interval`,
  `graph-db.spacetime:+standings+` and the rest compiles unchanged. The root
  condition keeps the name `spacetime-error` for the same reason.

  922 of the spacetime suite's 1169 checks moved with the code; the
  remaining 247 are the claim and source layers, and 247 + 922 accounts for
  all of them.


- **Writing a persistent slot now requires a node the current transaction
  may mutate** (#135) — a copy registered by `COPY`, or a node created in
  that same transaction. Anything else signals the new
  `MUTATING-UNREGISTERED-NODE`. The case this matters most for is not in
  the issue: `lookup-*` returns the **shared cached instance**, so `(setf
  (slot (lookup-thing id)) v)` mutated state every other reader and
  thread could see, was never persisted, and read back correctly until
  restart — wrong and invisible until a restart exposed it. Ephemeral
  and meta slots are unaffected; the guard only ever sees persistent
  slots.
  `MARK-DELETED` is deliberately exempt from the `COPY` half of this
  guard — it copies internally, and create-then-`MARK-DELETED` in one
  transaction was measured to work correctly before this change and
  still does.
  A consequence found while installing the guard: redefining a class to
  add an `:initform` persistent slot while instances are live made a
  plain *read* signal. CLOS runs `UPDATE-INSTANCE-FOR-REDEFINED-CLASS`
  lazily on the next slot access to an obsolete instance, and that wrote
  the new slot's initform through this same guarded path — on the shared
  cached node, with no transaction bound and nothing registered.
  `*INITIALIZING-NODE*` is now bound around it, matching
  `CHANGE-NODE-CLASS`; this never shipped, so it is not a user-facing fix,
  only a defect this branch introduced and removed before release.
  Both halves of the guard (the `SETF` check and `COPY`'s create-set
  check) treated a non-`TX` `*TRANSACTION*` — e.g. a `RESTORE-TRANSACTION`
  during snapshot replay — as `NO-APPLICABLE-METHOD` rather than behaving,
  since `COPIES` and `CREATE-SET` are readers on `TX` alone. Both now test
  `(TYPEP *TRANSACTION* 'TX)` first, matching `CREATE-NODE`'s existing
  guard, and trust a non-`TX` transaction unconditionally.
  Not addressed here: `SLOT-MAKUNBOUND` semantics are unchanged on both
  backends, including the pre-existing divergence where an `:initform`
  slot resurrects as its default after reopen on a disk graph but stays
  unbound on a memory graph. That gap is out of scope for #135 and is
  getting its own spec.

- **`type-id` widened from 16 to 32 bits; on-disk storage format bumped to
  v3** (#166, unit 1a, task 1). Every place `type-id` was written or typed
  widens together (no useful intermediate state): the node head (2 -> 4
  bytes; head grows 31 -> 33, edge head 71 -> 73), `ve-key` (18 -> 20
  bytes) and `vev-key` (34 -> 36 bytes), the CLOS `type-id` slot, and the
  schema's `next-vertex-id` / `next-edge-id` counters. Type-ids remain
  **per-graph** — no global registry, no distribution change; that is a
  separate issue (#186).
  A v1 or v2 graph cannot be opened directly by this build — `open-graph`
  signals, naming the version found and the version expected and pointing
  at `migrate-graph`, rather than silently misreading a 2-byte type-id as
  4 (which would otherwise corrupt every subsequent field in the head,
  and every adjacent record via the widened `ve-key`/`vev-key`).
  `deserialize-node-head-v2` (a byte-for-byte copy of the prior 31-byte
  reader) lets `migrate-graph` read a v2 source; see the next two entries
  for the type-index sizing this widening required and the migration
  path itself.

- **The type-index no longer pre-allocates a slot for every possible
  type-id** (#166, unit 1a, task 2). At the old 16-bit ceiling that cost
  ~1.1 MB per index per store; at the widened 32-bit ceiling the same
  scheme would cost ~73 GB. A type-index now starts sized for 4,096
  types and grows by doubling, in place — the mapping's base address
  never moves, so a concurrent reader is never at risk from a grow.
  Locking changed too: one mutex per type-id (65,536 of them per index
  per store, regardless of how many types were in use) is now 256 fixed
  stripes selected by `(mod type-id 256)`. **This is a real behaviour
  change, not a pure optimization** — two type-ids that land on the same
  stripe now serialize their pushes and removes against each other,
  where before they never contended.
  Fixed in passing: `gc.lisp`'s mark phase read the type-index's cache
  directly, which was only safe while that cache was eagerly populated
  at open. Under the new lazy population this marked nothing for a type
  not yet touched this session, and `gc-heap`'s sweep then reclaimed
  those nodes as garbage on the very first reopen after this change —
  caught before release by a dedicated close/reopen/gc test, not by any
  pre-existing coverage.

- **`migrate-graph` now carries a v1 *or* v2 graph to v3 through one
  version-detecting path** (#166, unit 1a, task 3). It reads the source's
  own stamped storage-version byte and opens it with the matching head
  reader, so the same call handles either source version — no second,
  parallel migration function.
  **Corrected a false claim in `migrate-graph`'s own docstring**: the
  source directory is not left byte-for-byte untouched. Producing the
  snapshot requires opening the source, and that open unconditionally
  rewrites `schema.dat` (same content, re-serialized — type-ids
  unaffected) and creates one new, empty `tx/replication-*.log` file.
  Those are the only two files that change; the source's data — its heap
  and every vertex/edge/index table — is untouched, and a pre-#166
  engine can reopen the source directly afterward and read every node
  with its type-ids intact. That is the actual rollback story
  (repointing at the old directory, not restoring from a snapshot), and
  it was verified by reopening a post-migration source with a genuine,
  unmodified old-version engine rather than assumed from the (now-fixed)
  docstring.
  A migration needs roughly 2x the source graph's disk space while it
  runs (source + intermediate snapshot + new graph), and every engine
  that will subsequently open the migrated graph — not just the one
  performing the migration — must already be built from a #166-or-later
  checkout.

## [3.0.0] - 2026-08-09

> **MAJOR.** Per this file's SemVer preamble, MAJOR is mandatory here on two
> independent grounds: the spatial-index changes below are a breaking public-API
> change *and* an on-disk format bump (the spatial sidecar goes to format v5, and
> the memory-graph image to v7). Existing on-disk graphs still open — the spatial
> index re-derives itself automatically at first open — but stale call sites and
> old Prolog arities do not.

### Added

- **Multi-graph support is now a defined, enforced contract** (#53). Running several
  graphs in one image was previously a configuration that mostly worked; it now has
  stated semantics and tests. See the manual, "Multiple Graphs in One Image".
  - A read-write transaction belongs to exactly one graph. Touching a node whose home
    graph differs — read or write — signals the new `cross-graph-transaction-error`.
    Previously a cross-graph read inside a transaction silently returned `NIL` for a node
    that exists, indistinguishable from "no such node".
  - Read-only snapshots are per graph and compose, so a cross-graph query holds one
    snapshot per participating graph. There is deliberately no single instant across
    graphs.
  - Nodes carry their home graph, so a node's heap resolves through its own graph rather
    than the ambient `*graph*`. A node read from another graph previously dereferenced its
    offset in the *wrong* memory-mapped file.
  - Node class names must be unique across all graph schemas; a collision signals the new
    `duplicate-node-class-error`. Previously the second definition silently replaced the
    first class's slots, leaving the first graph's stored data unreachable through the API.
    Redefining a type under the same graph name is unaffected.

- **Vector segment: a dense on-disk index for `:vector-index` slots.** A slot
  declared `:vector-index t` in `def-vertex`/`def-edge` gets a dedicated
  mmap vector segment, maintained automatically by the transaction apply path
  (create/update/delete) — no parallel write path or cache to keep in sync.
- `vector-search` (graph, class-name, slot-name, query-vector, k) — top-k
  nodes of `class-name` (and its subclasses) by cosine similarity against
  `slot-name`'s vector segment, as `(score . node-id)` conses. Returns `nil`
  when no segment exists yet (declared-but-never-written slot).
- `segment-scan` and `segment-score-subset` — lower-level segment query
  primitives (`vector-search` is built on `segment-scan`).
- `rebuild-vector-segment-batched` (graph, owner-name, slot-name &key
  batch-size progress-fn) — additive, resumable, batched (re)population of a
  `:vector-index` segment from live nodes; skips ids already present. The
  migration path for a corpus written before the slot was declared
  `:vector-index`, distinct from crash recovery's `rebuild-vector-segment`
  (full drop-and-rebuild), which `restore-vector-segments` still uses.
- `vector-segment-capacity-exhausted` — an exported condition (readers
  `vsce-owner`, `vsce-slot`, `vsce-path`, `vsce-required`, `vsce-reserved`,
  `vsce-needed-bytes`, `vsce-reason`) signalled when a vector segment must grow
  past its mmap reservation and cannot relocate to a larger one — see the
  "Changed" entries below for when that can still happen and what its report
  now advises.
- **Spatial index — a per-`(owner . slot)` index registry.** The graph's single
  spatial index becomes a registry of per-declaring-class, per-geometry-slot
  indexes, mirroring `:unique` / `:vector-index` / `:index`. `spatial-indexes`
  (graph accessor) is the registry keyed `(owner-name . slot-name)`, and
  `spatial-index-for` (graph, owner, slot) reaches one index. One spatial index
  is created per declaring class per geometry slot, lazily, on the first
  geometry-valued insert. A geometry slot on a *mixin* gives its subclasses ONE
  shared index — separated at query time by the required type filter, not by
  storage — which is narrower than "per class"; a class that overrides
  `node-geometry` is indexed under `(owner . NIL)` and is still scopeable by name.
  Motivated by a downstream application team's spatial-index change request
  (CR-1).
- **`:spatial-precision` slot option — per-index geohash precision.** A geometry
  slot may declare `(slot :type geometry :index t :spatial-precision N)`; that
  index is built on an `N`-level geohash grid instead of the graph default (7).
  This is the *only* precision-declaration surface (there is deliberately no
  separate per-index declaration macro). Changing a declared precision rebuilds
  that one index automatically at open (bounded to the owner's nodes), because a
  mixed-precision index would silently miss on query.
- **`audit-spatial-slots` (graph)** — a read-only, exhaustive sweep that reports
  every class carrying more than one geometry-valued indexed slot and names the
  winning slot, for wiring into a schema test suite (see the inert-slot warning
  under Fixed).
- **`geometry-empty-p` (g)** — true when a geometry holds no coordinates, i.e. the
  EMPTY geometry of its kind (the kind is preserved: an empty polygon is still
  `:polygon`). This is how a caller tells "nothing there" from "could not compute":
  an empty result is a real answer that a spatial op returns normally — the
  intersection of two disjoint polygons is empty — while a genuine failure signals.
  Testing `geometry-coordinates` yourself is not equivalent: the kinds do not
  represent emptiness alike (an empty linestring's coordinates are a zero-length
  vector, the rest are `NIL`), and a polygon or multipolygon can hold its emptiness
  one or two levels down. See #105.
- **New spatial maintenance and query entry points.** `rebuild-spatial-indexes`
  (all indexes, the migration/repair sweep), `regenerate-spatial-index`
  (one `(owner slot)` index — the manual recovery for a degraded index),
  `regenerate-spatial-indexes` (all — the spatial half of an index-backend
  switch), and `install-spatial-indexes` (adopts a changed declared precision at
  open; creates nothing). Per-index introspection: `spatial-index-max-cells`,
  `spatial-index-precision-counts`, `spatial-index-coarsest-precision`.
- **Prolog: scoped spatial functors `find-within/3`, `find-intersects/3`,
  `find-near/5`, `find-nearest/5`.** The scope rides in second position
  (`(find-near ?node scope lat lon radius)`); it accepts a class symbol, a list
  of class symbols, or `:all`, and type-filters the yielded nodes, so the `is-a`
  goal these queries once needed is gone.

- **General ordered indexes — index a slot by its value (`:index` / `def-index`).**
  The non-unique counterpart to `:unique`, built on the same machinery. Annotate a
  slot with `(slot :index t)`, or `(slot :index string-downcase)` to index a
  canonicalized value; or declare it away from the class with
  `(def-index user email :social-app :canonicalize string-downcase)`, which is
  declarative and idempotent like `def-view` and may even be evaluated before its
  graph exists. Query with `index-lookup` (equality), `index-range` and `map-index`
  (ordered ranges). An index on a class covers that class *and its subclasses*,
  rooted where it is declared. Maintained on the commit path and durable — its root
  is persisted in a sidecar and reopened by address, not rebuilt by scanning every
  node at open — on either ordered-map backend. Admin entry points:
  `rebuild-secondary-indexes`, `regenerate-secondary-indexes`. Manual: Chapter 8,
  "General ordered indexes".
  - *Not yet reachable from Prolog* — there is no index-backed generator predicate,
    so a Prolog query still generates candidates by type and filters them. Tracked
    in #102.

- **`:spatial-index-backend` — choose the ordered-map backend for spatial indexes
  independently of the rest of the graph.** A graph can keep `:index-backend
  :bplus-tree` for views and `:unique` while running spatial on the skip list,
  which measurements favour for spatial workloads (#91).

- **`:ephemeral` slot option now works** (#90). A slot declared `:ephemeral t` is
  per-instance state that is never written to disk — ordinary CLOS storage, absent
  from everything the node serializes. It had no effect at all previously: such
  slots were categorized persistent and stored like any other, and
  `ephemeral-slot-names` returned `NIL` for every node class. Nothing declared
  `:ephemeral` anywhere, so no existing graph changes. Give ephemeral slots an
  `:initform`: an unset persistent slot reads back as `NIL`, but an unset ephemeral
  slot is an unbound CLOS slot and signals. Manual: "Persistent, ephemeral and meta
  slots".

### Changed

#### Performance

- **A scoped spatial query now costs its results, not the whole index's
  population** (#104). Every candidate id the index returned was materialized into a
  live node *before* the scope's type filter could run, because that filter took a
  node and called `typep`. Where several classes share one index — the normal
  outcome of declaring a geometry slot on a mixin, and what
  `class-spatial-index-keys` documents — a scoped query therefore paid for every
  class in the index. Measured on a 296,932-point shared index: 206 results cost
  120 ms and 29,739 results cost 126 ms, while the same query against a per-class
  index of 5,004 points cost 18 ms. Cost tracked candidates, not answers, so the
  idiomatic modelling choice was the one that produced the floor.
  Each index entry now carries its node's type tag (its type-id plus a kind bit),
  and the scope's admitted tag set is applied *inside* the range scan: a candidate
  outside the scope is never deduped, never consed and never materialized. The
  dedup table is also created once per query and threaded through every index
  scanned, rather than one per index plus a second one over the results.
  `typep` still runs on the survivors and remains authoritative — the tag set is
  only ever a conservative pre-filter, skipped entirely for `:all` and for an
  untagged (pre-#104) entry.
  *Migration:* the spatial sidecar goes to **format v5** and the memory-graph image
  to **v7** (native) / **v5** (cl-store); both re-derive the spatial indexes from
  live nodes at first open. An index that somehow arrives untagged still answers
  correctly, just at the old cost.

- **A view's map/reduce source is compiled once, not once per node** (#89).
  `add-to-view` called `compile-view-code` on every node addition, and that
  read-from-string'd and eval'd the source unconditionally — invoking the reader and
  the compiler once per view per node to rebuild functions that had not changed.
  Measured on 2,000 vertices with one map-reduce view: **~3.6x faster writes and ~85%
  less allocation** (0.90 -> 0.25 ms/node, 227 KB -> 34 KB per node), on the write path
  every ingest pays. Memoized on the source, so a redefined view still recompiles.

- **Point-in-polygon no longer boxes every coordinate** (#86). The packed
  double-float path was allocating a fresh boxed float per coordinate; type
  declarations on a private kernel, with coercion at the public boundary, removed it.
  47 KB per call -> 0, and one profiler workload went from 300.6 MB to 33.1 MB.

- **Slot categorization is computed once per class, not per slot access** (#87).
  `persistent-p` / `ephemeral-p` / `meta-p` answers are fixed once a class is
  finalized, but the name lists were walked and freshly consed on every access —
  roughly 28 rebuilds per node materialized, 26.4 MB of throwaway lists in one
  profiler workload. Now cached per class, invalidated on redefinition.

- **The B+ tree stops re-reading the leaf page its descent just read** (#97).
  `%bpt-descend-leaf-addr` read the leaf to test its flag and discarded the buffer, so
  every caller immediately re-read the same page — one wasted full-page copy per point
  lookup, insert descent, delete descent and range-cursor open. Saves ~10 KB per
  spatial query at the measured shape. The larger cause — no page cache across a
  query's covering cells — remains open in #97.

#### Other

- **`*TRANSACTION*` is `NIL` inside a read-only snapshot** (`with-read-snapshot`,
  and `select`/`do-query` with `:snapshot t`). A snapshot now populates the new,
  exported `*read-snapshots*` instead of binding `*transaction*`, so code that
  read `*transaction*` to detect "am I inside a query" sees `NIL` there. Call
  the new exported `read-transaction` (&optional graph) to ask which transaction
  a read of a given graph actually resolves through — the read-write
  `*transaction*` when it covers that graph, else that graph's read snapshot,
  else `NIL`. Second-order effects of the same change: inside a snapshot,
  `copy` now signals `no-transaction-in-progress-warning` instead of silently
  joining the snapshot, `commit`/`rollback` now signal
  `no-transaction-in-progress` instead of no-op'ing, and `make-<type>` /
  `mark-deleted` now auto-wrap their own real, committing transaction instead of
  joining a snapshot that was never going to commit — a correctness fix, but a
  visible behavior change for any code relying on the old silent join.

#### Storage growth and segment behaviour

- **A vector segment now grows its reservation *in place* before falling back to
  relocating.** On exhaustion `%seg-ensure-reservation` first tries to claim the
  address range immediately after the current window
  (`extend-reservation-in-place`, `mmap.lisp`): one `mmap`, `m-pointer` never
  moves, nothing is copied or re-mapped, and no reader can observe anything at
  all.
  **How often it actually fires: less than the design assumed.** Measured with a
  production-sized (16 GiB) reservation on Linux 5.15 and 4.15, the claim failed
  at every size tried, because the default top-down `mmap` allocator places a
  `mmap(NULL, …)` window flush against the bottom of the existing mappings —
  `libssl.so.3` sat at the window's exact end on both hosts — so the range
  immediately *above* a newly created window is occupied by construction. The
  legacy bottom-up layout behaved identically, and Darwin likewise. It succeeds
  only for a window that happens to sit below a hole. Relocation (the entry
  below) therefore remains the workhorse; this is an opportunistic saving that
  costs one `mmap` on an already-rare path when it misses.
  The claim passes `MAP_FIXED_NOREPLACE` where the constant exists (Linux
  4.17+), which makes the kernel reject cleanly instead of placing the mapping
  somewhere useless. **The safety property is not that flag**, which older Linux
  and Darwin simply ignore, leaving the address an advisory hint: it is the
  unconditional post-hoc check that the address returned is exactly the address
  requested, with a `munmap` and a fallback when it is not. Plain `MAP_FIXED` is
  never passed and must never be added — it would evict whatever occupies the
  range. (Measured, since an earlier revision of the design asserted the
  opposite: Linux 5.15 honours the flag and returns `EEXIST`; Linux 4.15 ignores
  it and places the mapping elsewhere; in neither case is the occupant touched.)
  `*segment-extend-adjacent-on-exhaustion*` (exported, default `t`) switches it
  off, which is also what keeps the relocation tests genuinely exercising
  relocation. `*segment-adjacent-extensions*` and `*segment-relocations*` count
  which path ran. **Binding this knob to NIL by itself does not stop a segment
  from growing past its reservation** — it only removes the adjacent shortcut,
  leaving `*segment-relocate-on-exhaustion*` (below) to grow it by relocating
  instead. Getting the hard, pre-durability abort back requires BOTH knobs NIL;
  see the correction to that entry's description below.
- **A vector segment's mmap reservation is no longer a growth ceiling.** When a
  doubling would pass the reservation, the segment now reserves a larger
  address window, re-maps its file into it, and releases the old window —
  completing the "re-reserve + relocate under the write lock" step
  `docs/mmap-remap-race-plan.md` Phase 3 planned but never implemented. This
  moves `m-pointer`, which the lock-free read path otherwise depends on never
  moving, and is therefore **segment-only**: every public segment entry point
  takes the segment's own rw-lock, so `%seg-grow` has real exclusion over its
  readers. The heap (`allocator.lisp`) and linear hash (`linear-hash.lisp`) have
  no such lock — for them the reservation remains a hard ceiling, and the
  primitive is named `relocate-vector-segment-mapping` so calling it from either
  reads as wrong at the call site.
  `*segment-relocate-on-exhaustion*` (exported, default `t`) switches this
  behaviour off — but, since the adjacent re-reservation entry above shares the
  same exhaustion path and runs FIRST, this knob alone no longer restores the
  previous strictly-safe pre-durability abort: left at its default,
  `*segment-extend-adjacent-on-exhaustion*` can still grow the segment in place
  without ever reaching this one. **Both knobs must be bound to NIL** to get
  that abort back.
  `vector-segment-capacity-exhausted` now fires only when relocation is
  disabled or fails (address space exhausted), and carries two new slots —
  `vsce-path` (for the direct `segment-put` path, which has no owner/slot) and
  `vsce-reason` — with a report that says which of the two happened. Both
  `munmap`s on the relocation path (the rollback and the old-window release)
  check their return code and warn: a refused `munmap` leaks an entire
  reservation, in a long-lived process, on the one path whose failure mode *is*
  address-space pressure.
- **The pre-durability capacity check now *grows* the segment instead of
  rejecting the transaction** (`validate-vector-segment-capacity` →
  `ensure-vector-segment-capacity`). Once exhaustion became recoverable, a check
  that refused any transaction needing more than the current reservation was
  over-eager. Growing in the same manager-locked region, before
  `finalize-tx-persistence`, keeps wave 1's guarantee rather than weakening it
  to a heuristic: since commits are serialised under the manager lock, no other
  *commit* can consume the capacity in between, so `apply-transaction` cannot
  need to grow **absent a concurrent lock-free mutator**. One exists:
  `rebuild-vector-segment-batched` deliberately runs *without* the manager lock
  and raises `live-count` via `segment-put`. If it interleaves, apply's grow
  branch is reachable after all — but it then *relocates* and succeeds, so the
  wave-1 failure mode (a persisted node with no segment entry) returns only if
  relocation is switched off or genuinely fails at that moment. This is not a
  regression: wave 1's validate-only version had the identical hole. The
  reservation for the full target capacity is now pre-flighted **once**, before
  any doubling runs, so an unrecoverable transaction aborts having changed
  nothing at all and the diagnostic still names the owner and slot.
  Two accepted consequences, documented at the
  function: a transaction that fails later leaves an over-sized segment (harmless
  — capacity is not semantic, `live-count` and the id array are untouched), and a
  crash mid-grow leaves the segment dirty so `restore-vector-segments` rebuilds
  it (the existing path, made survivable above 131k entries by wave 1).
- **Vector segments now get their own address-space reservation floor,
  `*segment-min-reservation*` (16 GiB), instead of inheriting the general
  `*mmap-min-reservation*` (1 GiB).** The general rule — 8× the file's size at
  open, floored — was written for heap and index files, whose size is set by the
  schema and the workload, and of which a graph has 15–20. A vector segment is
  the first mapped file whose size tracks the *corpus*, so it reached that
  ceiling far sooner (roughly once per 8× of growth), and hitting it aborts a
  transaction. Both call sites now pass an explicit reservation —
  `create-vector-segment` and `open-vector-segment`, neither of which passed
  one before (both simply took `mmap-file`'s general default) — computed as
  `max(*segment-min-reservation*, *mmap-reservation-multiplier* × size)`, so a
  segment already larger than `floor ÷ multiplier` still gets proportional
  headroom rather than being capped at the floor. A reservation is `PROT_NONE`
  `MAP_NORESERVE` anonymous address space: no RAM, no disk, no Linux commit
  charge, so on 64-bit the larger floor costs nothing real — except `RLIMIT_AS`
  / `ulimit -v`, which counts reserved address space regardless of
  `MAP_NORESERVE` and can make a graph fail to open under a capped process
  (e.g. a systemd unit's `LimitAS=`). At dimension 1024, capacity only ever
  advances by doubling, so the largest power-of-two capacity whose file still
  fits under the 16 GiB floor is 2,097,152 slots, not the byte-exact
  4,177,983 (the next doubling, 4,194,304, needs 17,246,978,112 bytes, over
  the 17,179,869,184-byte floor). (When this landed, reaching the floor still
  meant `vector-segment-capacity-exhausted`; the *relocation* entry above then
  removed the ceiling outright, so today the floor is only the point at which a
  segment starts relocating.)
  `*segment-min-reservation*` is exported — it is the one knob that actually
  raises this ceiling, unlike `*mmap-min-reservation*`, which segment files
  no longer consult.
- **A missing vector-segment file is now rebuilt at open, not ignored.**
  `restore-vector-segments` used to skip a segment whose file was absent, so a
  graph whose segment file had been lost (or deleted by an operator expecting a
  rebuild) opened clean with a permanently empty vector index, no warning and no
  error, and `vector-search` returned nothing for a corpus that was entirely
  intact in the vertices. The vertices are authoritative, so the segment is
  rebuilt from them and the recovery is reported with a warning. A graph that
  has simply never stored a vector is *not* swept: an owner class with no nodes
  in the type index is skipped outright, so a declared-but-never-written
  `:vector-index` slot costs nothing at open.
- **An unclean (dirty-flag) segment rebuild at open now warns before it
  starts**, so a multi-minute rebuild of a large segment is not silent.
- **Snapshot/replay lost specialized vectors (issue #56).** `snapshot` → `replay`
  could not round-trip a graph whose nodes had a vector-valued slot that was not
  a byte vector: the restore readtable overrode `#(` to coerce *every* vector to
  `(unsigned-byte 8)` — necessary because node ids are byte vectors, fatal for
  anything else — so a `single-float` slot (e.g. a `:vector-index` embedding)
  aborted the restore with `The value 1.0 is not of type (UNSIGNED-BYTE 8)`.
  The snapshot text format now records a vector's element type explicitly:
  `backup` writes any vector whose element type is not `T` as
  `#V(<element-type> e1 e2 ...)` (e.g. `#V(SINGLE-FLOAT 1.0 1.25)`,
  `#V((UNSIGNED-BYTE 8) 37 22)`), and `#(...)` now reads back as a plain
  `simple-vector`. Strings are unaffected. `*print-readably*` was deliberately
  *not* used: SBCL's `#A((3) SINGLE-FLOAT ...)` is an SBCL extension, and a
  snapshot must restore on SBCL, CCL, ECL and LispWorks alike.

#### On-disk and wire formats

- **The snapshot text format changed, one-way compatibly.** Snapshots written by
  this version are **not** readable by older graph-db versions (they contain
  `#V` literals older readers do not know). Snapshots written by older versions
  **are** readable by this one: ids, and an edge's `from`/`to`, are coerced back
  to id byte vectors at the consumption site in `recreate-graph`. One thing old
  files cannot give back is the *element type* of a node's own vector-valued
  slots — that was never written down — so such a slot restores from an old
  snapshot as a plain `simple-vector`; re-snapshot afterwards to record the
  types. Nothing about the on-disk graph format changed.
- **BREAKING: every spatial query now takes a required scope as its first
  positional argument.** `find-nodes-within`, `find-nodes-intersecting`,
  `find-nodes-near`, and `find-nearest-k` gain a mandatory first argument — a
  node-class name, a list of class names, or `:all` — that both *selects* which
  per-`(owner . slot)` indexes are scanned and *type-filters* the results. The
  filter is what makes a scoped query correct when sibling subclasses share a
  mixin-owned index. A required positional argument makes every stale call site a
  compile-time warning on SBCL and ECL, which is the safest way to land a
  deliberate break. Requested by a downstream application team (CR-1): they
  needed to
  query one class's geometry without dredging up another's.
  *Known limitation:* a scope resolves the named class's own geometry slots, so
  scoping to a parent does not reach an index a *subclass* declares on an extra
  geometry slot of its own (a node stored there is still a `parent` by type, but
  the parent scope will not scan it). Scope to the subclass or use `:all`; the
  general fix rides with GitHub #60.
- **BREAKING: the spatial sidecar is now `spatial-indexes.dat`, format v5**
  (was `spatial-index.root`, a single plist). It records one entry per
  `(owner . slot)` index — address, precision, backend, insert cap, and precision
  histogram — and is written on every index creation and at `close-graph`. A
  pre-v5 graph (the old file present, or a stale `:format`) **re-derives its spatial
  indexes automatically at first open**: one `map-vertices` + `map-edges` sweep
  routes each node into the `(owner . slot)` index its geometry slot selects, so
  the contents come out identical to what the single index held, merely
  partitioned. **Index only — node data is untouched and nothing is re-fetched.**
  The old `spatial-index.root` is left in place, but **downgrade after migration
  is unsupported**: an older build would reopen it as a silently stale (or empty)
  index.
- **The memory-graph image bumped, and lost a per-open cost.** Both in-memory
  formats moved — the cl-store image (v5) and the native/lazy image the ECL
  device uses (v7) — so the spatial payload is now one structural record per
  `(owner . slot)`, carrying that index's precision, insert cap, and histogram,
  restored directly into its own `mem-skip-list` the way views are. This
  **removes** the rebuild-from-nodes that a memory-graph previously ran on every
  open: that pass filtered over *all* `:index` slots and so faulted in every lazy
  node blob of any class with any indexed slot — most of the corpus on a field
  device (issue #50 `:lazy` mode). Measured 0 of 11 nodes materialized on reopen,
  against 11 of 11 before. Known limitation: a changed `:spatial-precision`
  declaration is **not** adopted on a memory reopen (doing so would re-materialize
  exactly those lazy nodes); a memory index reopens at its persisted precision
  until a forced rebuild — correct, only over-covering, never missing.

### Removed
- **BREAKING: `spatial-index` (the single whole-graph spatial-index accessor).**
  There is no longer one index for it to name; use `spatial-indexes` /
  `spatial-index-for`.
- **BREAKING: the old singular, whole-graph spatial rebuild function.** There is
  no longer one index to rebuild; replaced by `rebuild-spatial-indexes` (rebuild
  every index) and `regenerate-spatial-index` (rebuild one `(owner slot)` index).
- **BREAKING: the previous unscoped Prolog spatial arities** — `find-within` and
  `find-intersects` at arity 2, `find-near` and `find-nearest` at arity 4.
  Replaced by the scoped `/3` and `/5` forms above. The old arities are removed
  rather than left to signal, so a stale query fails at goal entry with an
  unknown-functor error that names the problem, instead of binding a scope-shaped
  argument as an area.

### Fixed

- **`graph-db/algorithms` was unusable on its own.** The add-on depends on
  `graph-db/core`, but the `NODE-ORIGINS` NIL fallback for a plain graph lived in
  `peer-merge.lisp`, which is in `graph-db/replication`. `unique-constraint.lisp` is
  in *core* and reads `NODE-ORIGINS` off any graph, so a core-only consumer hit
  `no applicable method` on the first `:unique` code path — 52 of the 64 algorithm
  tests, every one of them that touched a graph. Loading the full `graph-db` system
  hid it entirely, since that pulls in replication. The fallback now lives in
  `graph-class.lisp`, beside the class it defaults for. Present since the
  `graph-db/replication` carve-out and shipped in 2.1.0/2.1.1; found by running the
  add-on's own suite as a release gate.

- **`wkt->geometry` signalled on every EMPTY geometry** (#105). The type keyword was
  read as "everything before the first paren", but an EMPTY geometry has no paren —
  so `"POLYGON EMPTY"` produced the keyword `"POLYGON EMPTY"`, matched no branch, and
  fell through to `Unsupported WKT geometry type`. The correct EMPTY handling was
  already there, sitting inside a dispatch that could never reach it. Since GEOS
  reports an empty result as `"POLYGON EMPTY"`, this fired on the **normal** case —
  `geometry-intersection` of two disjoint polygons — and through the shared
  `geos->geometry` path it hit `union` / `difference` / `buffer` / `make-valid` too.
  The type is now read as the first token.
  A caller could not distinguish "no overlap" from "could not compute": both a
  genuine zero-area result and the parse failure came back as an error, and a
  consuming app that wrapped the call in `ignore-errors` got the same answer either
  way by coincidence. An empty result is now an empty geometry of area 0, and only a
  real failure signals.
  `geometry-empty-p` (below) is the predicate for asking that question.
  Also fixed in the same branch: `"POINT EMPTY"` returned a point at **(0, 0)**.
  That line was unreachable before this fix, so nothing had exercised it — but null
  island is a real location, and `geometry->wkt` would have written it back out as
  `POINT (0 0)`, making the falsehood durable. An empty point now carries no
  coordinates, and round-trips as `POINT EMPTY`.

- **One sliver part in a multipolygon collapsed the whole spatial index's query
  precision to 1** (#103). A multipolygon's cell budget was split across its parts in
  proportion to each part's bounding-box *area*, floored at one cell. A part under
  `1/max-cells` of the total floored to a one-cell budget — and covering any real
  geometry in one cell means precision 1. Because a query never covers more finely
  than the coarsest cell stored (the clamp that keeps a capped insert findable), one
  small island in one admin boundary widened *every* query on that index. Measured on
  a real graph: 6 sliver parts across 5 of 1,780 places held the whole index at
  precision 1 while 100,253 of 100,265 cells sat correctly at precision 5.
  The budget is now a bound on a geometry's **total** cover, not a per-part
  allowance: every part is covered at one precision, the finest whose total fits, so
  a small part stays fine *because* it is cheap and only a genuinely oversized
  geometry coarsens anything. A geometry with more parts than the cap can hold at any
  precision — coarsening cannot help there, since each part costs a cell however
  coarse the grid — falls back to a single bounded cover of its envelope instead of
  collapsing to precision 1.
  *Migration:* a heap-backed graph re-derives its spatial indexes automatically at
  first open (this is what the sidecar's v5 bump is for). A memory-graph restores
  its spatial grid structurally from the image and so would have kept the cells the
  old budget wrote — but #104's image bump re-derives them from live nodes on the
  first open of a pre-v7 image, so a memory-graph now picks this fix up
  automatically too, with no `regenerate-spatial-indexes` call. Cells covering a
  single-part geometry are byte-identical either way.

- **A skip-list read could return a freed node's data, or loop forever** (#88). The
  direct-mapped node cache is validated by *address*, so anything that frees a node had
  to evict it first and did not. Wrong reads and unbounded traversal cycles, in any
  skip-list-backed structure.

- **A node created with an explicit `:graph` was written into the ambient
  transaction's graph instead** (#96). `create-node` reused the ambient
  `*transaction*` and ignored its `graph` argument, so the node was stamped with one
  graph and stored in another, silently — the mirror image of the read-side bug the
  multi-graph work exists to fix. Now signals `cross-graph-transaction-error`.

- **A `:vector-index` slot on an *edge* class was maintained but never rebuilt** (#57).
  Both rebuild paths swept `map-vertices` with the owner as `:vertex-type`, which
  matches nothing for an edge class, so the segment came back empty after any rebuild —
  invisible while the process stayed up, because the live path had already built it
  correctly in RAM.

- **A memory graph lost its vector segments on reopen** (#58). `open-memory-graph`
  never called `restore-vector-segments`, so the segment files were written by ordinary
  maintenance and then orphaned; `vector-search` returned nothing. (Still skipped for
  `:lazy` graphs, where a rebuild sweep would materialize every deferred node.)

- **An existing segment file could be silently destroyed** (#55). `%ensure-segment`
  keyed on table registration alone, so an unregistered-but-present segment file was
  *created over* — header rewritten, capacity free-marked, contents gone, no error and
  no warning. Reachable in practice: before #58, every memory-graph reopen left every
  segment file unregistered. It is now adopted rather than overwritten.

- **Index sidecars were written in place, and a torn one failed the whole open**
  (#63). The spatial, unique and secondary sidecars now write to a temporary file and
  rename into place. More seriously, only spatial could survive *reading* a damaged
  one — unique and secondary let the error propagate out of `open-graph`, despite both
  documenting a "fall back to rebuild" contract that nothing could reach. The nodes are
  authoritative, so they now warn and rebuild.

- **A stale memory-graph image could not be recovered, and the error advised
  destroying the data** (#65). The message told the operator to delete the image and
  reopen "to rebuild from the journal" — but a clean-close checkpoint clears the
  journal, and the memory backend has no heap, so `graph.img` is the *only* durable
  record and deleting it discards the graph. The message is corrected and v5 images now
  migrate on read (including `:lazy` graphs, materializing only geometry-bearing nodes).

- **A retried migration failed with a bare `FILE-EXISTS`** (#98). `migrate-graph`'s
  default snapshot path was keyed on the graph name alone — constant across runs, users
  and processes — and cleanup only ran on success. One aborted migration therefore broke
  every later migration of that name. The path is now per-run and cleanup is on every
  exit.

- **ECL: `gettimeofday` returned `NIL`, silently collapsing every snapshot name**
  (#100). It had arms for SBCL, CCL and LispWorks and none for ECL, so the whole body
  was empty. `txn-log` built its snapshot filename from it, so on ECL every snapshot of
  a graph formatted to one constant name — and ECL's permissive `:if-exists` default
  overwrote rather than erroring, so each snapshot silently replaced its predecessor.
  It also returned a single rational on SBCL against two values on CCL, so no caller
  could be correct everywhere.

- **ECL: hash tables the engine treats as shared were not synchronized** (#101). Every
  concurrently-accessed table is created `:synchronized` on SBCL and `:shared` on CCL;
  the ECL arms had no equivalent, because ECL once had none. Modern ECL does, gated
  here on a runtime probe. The reachable case was the secondary-index registry, read
  unlocked from the public query API while a commit could be inserting into it.

- **A vector segment could not grow past its mmap reservation without
  corrupting the transaction.** The growth attempt failed inside the apply path,
  after the transaction was already durable, so the segment and the nodes
  disagreed. The capacity a transaction needs is now validated *before*
  durability, under the segment's read lock, and an over-large transaction is
  rejected with `vector-segment-capacity-exhausted` and rolled back cleanly —
  nothing is journaled and the segment is untouched.
- **Automatic crash recovery of a vector segment could not complete above
  131,072 entries.** `rebuild-vector-segment` created the fresh segment at the
  1024-entry default, and a segment's address-space reservation is derived from
  its file size *at create time*, so a ~4 MB fresh file reserved only the 1 GiB
  floor and in-place doubling ran out of reservation at 131,072 entries — while
  `restore-vector-segments` calls exactly this rebuild whenever the segment's
  clean-shutdown flag is unset. A rebuild is now created at the corpus size, so
  its reservation is derived from a realistic file (and ~8 doubling-and-relocate
  passes disappear from every rebuild).
- **Spatial insert could blow up on a country-scale geometry, and coarsening it
  naively would silently lose nodes (CR-2).** An insert now caps its geohash cover
  at 16384 cells — a per-index, *persisted* bound, so insert and remove always
  compute the identical cell set and no entry is ever orphaned. Coarsening the
  stored cover is unsafe on its own, because geohash prefixes nest one way: a query
  covering a small box at a fine precision would sort *past* a coarsely-stored
  polygon and miss it. The query therefore clamps its covering precision to the
  coarsest precision actually stored, tracked by a per-index histogram
  (`spatial-index-precision-counts` / `spatial-index-coarsest-precision`). The
  clamp is **self-healing**: delete the oversized node and its cells decrement,
  the coarse level empties, and selectivity returns on its own with no rebuild. A
  `warn` fires on each *decrease* of an index's coarsest precision (rare, loud,
  and names the node, class, slot, bbox, and the recovery path); a `log:info`
  marks the recovery. The histogram is rewritten synchronously only when the
  coarsest precision decreases (the unsafe direction); an emptied level rides the
  ordinary close-time write, because reopening too-coarse merely over-covers.
  A multipolygon splits the cap across its parts in proportion to each part's
  bounding-box *area* (with a floor of one cell per part), so a small part keeps
  full precision and only a genuinely large one is coarsened — an equal 1/N split
  coarsened small parts needlessly and, past 16384 parts, collapsed the whole
  index's query clamp to precision 1.
  The spatial sidecar is no longer written on the commit path. It used to be
  written on an index creation and on a coarsest-precision decrease, which put
  `cl-store` file I/O under the transaction-manager lock, on the post-durability
  side of the commit — a commit-convoy point and a failure-injection point after
  the data was already durable. It is now written only at `close-graph` and by the
  rebuild/regenerate admin ops; a crash forces recovery, and `open-graph`
  re-derives every spatial index from the recovered nodes after the WAL replay, so
  the histogram/clamp is reconstructed from authoritative geometry rather than from
  an incremental write.
- **A class with two geometry-valued indexed slots silently indexed only one
  (CR-3.1).** A node reaches spatial maintenance, is indexed by its first
  geometry-valued indexed slot, and every other geometry slot was inert with no
  signal. A value-based warning now fires on the write path — sampled over a
  class's first 64 nodes, so it costs nothing steady-state — naming the class,
  every geometry slot found, and which one wins. `audit-spatial-slots` (above) is
  the exhaustive read-only counterpart for classes whose two-geometry nodes lie
  beyond the sampling window. (The declared-`:type geometry` form the request
  first asked for is not buildable: the engine cannot compare the type symbol
  reliably across application packages, so the check is value-based.)

### Known limitations

Shipping with 3.0.0, tracked rather than hidden:

- **The general ordered index is not reachable from Prolog** (#102). `index-lookup` and
  friends work from Lisp; a Prolog query still generates candidates by type and filters
  them, because no index-backed generator predicate exists yet.
- **A scan-then-write transaction serializes the graph** (#92). Every node a typed scan
  visits joins the read-set, so a scanning transaction conflicts with any concurrent
  writer touching anything it scanned; measured at the retry ceiling, meaning every such
  commit completes under the global lock. Correct, but it does not scale. Deliberately
  deferred to 3.1: the fix changes isolation semantics and did not belong in a release
  that already carries a format bump. Note that backoff and a higher retry cap do *not*
  help — the collision is deterministic, not probabilistic.
- **A `:lazy` memory graph still reopens without vector segments** (#58 covers the
  non-lazy case). A rebuild sweep would materialize every deferred node, defeating
  fault-on-access.
- **Two ECL growing-writes concurrency tests are intermittent** (#95). They pass in
  isolation (20/20) and fail only under a loaded multi-suite run; four attempts to
  reproduce them deliberately failed. Instrumented so the next natural occurrence
  identifies the cause rather than raising the question again.
- **CCL: a vector-segment writer starves against concurrent scanners** (#118). On CCL
  a `segment-put` writer contending with sustained `segment-scan` readers never
  acquires the write lock — measured stalling at 6 of 128 puts while three scanners
  reached 118,000 scans, so `SCAN-IS-SAFE-AGAINST-GROWING-WRITES` and its
  `SCORE-SUBSET` twin do not terminate. The cause is that CCL alone does not use this
  repo's `rw-lock.lisp`, whose FIFO writer queue makes writer starvation structurally
  impossible; it uses `ccl:make-read-write-lock`, which offers no writer fairness
  (`rw-lock.lisp:3-5`, `graph-db.asd:50`). Not a 3.0.0 regression — those shims long
  predate this release, and vector segments are merely the first suite workload with
  sustained reader/writer contention. In principle any CCL `acquire-write-lock` user is
  exposed, though only the segment path provokes it today. **SBCL and ECL are
  unaffected on both macOS arm64 and Linux x86_64**, where the same tests pass in
  milliseconds. CCL remains the least-supported platform (Linux x86_64 only).

- **`graph-db/algorithms-io` will not load on a current Quicklisp** — its `dso-lex`
  dependency was dropped from the dist somewhere between 2025-06-22 (where it is
  present) and 2026-01-01 (where it is not), so `quickload` fails with
  `System "dso-lex" not found`. This blocks the optional GML/Pajek import +
  Graphviz export add-on and, with it, `graph-db/algorithms-test`. **The core
  `graph-db/algorithms` add-on is unaffected** and loads normally — only the
  parsing-dependent I/O layer is out of reach, which is why it was kept a separate
  system. Nothing in `graph-db` itself depends on it. Found by validating the release
  on a host with a current dist; a checkout with an older local dist still loads it.

- **`geometry-contains-point-p` uses a different point-in-polygon implementation from
  every other spatial predicate** (#99). Characterized against GEOS across 45 systematic
  cases plus generative sweeps: they agree everywhere away from a boundary, and differ
  only in boundary *convention* (half-open ray-cast versus DE-9IM). No defect, but the
  inconsistency is deliberate to revisit rather than accidental.

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
  `regenerate-all-views` / `regenerate-unique-indexes` / `regenerate-spatial-indexes`
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

[Unreleased]: https://github.com/kraison/vivace-graph/compare/v3.0.0...HEAD
[3.0.0]: https://github.com/kraison/vivace-graph/compare/v2.1.1...v3.0.0
[2.1.1]: https://github.com/kraison/vivace-graph/compare/v2.1.0...v2.1.1
[2.1.0]: https://github.com/kraison/vivace-graph/compare/v2.0...v2.1.0
[2.0.0]: https://github.com/kraison/vivace-graph/releases/tag/v2.0
