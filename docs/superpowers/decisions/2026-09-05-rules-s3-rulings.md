# Decisions: rules S3 (#332, epic #304)

The controller rulings S3 was planned and executed under, transcribed
from the plan's header and the SDD ledger so the record outlives the
worktree. **Every one was taken without Kevin in the loop**, each after
a source-verified recon pass or a task review; each carries the cost of
being wrong as it was stated at the time. Nothing here is re-derived or
improved.

**One ruling deviates from the spec's letter: S3-R2.** Spec §10's
fourth sentence — "Reads resolve at one instant under the shared clock
(#94)" — is refused, because the engine deliberately provides no
cross-store instant (`call-with-read-snapshot`'s own docstring, GH
#53). What #332 ships is what kraison/vivace-graph#94's closing comment
describes: one *comparable* epoch space, the epochs equal when nothing
commits between the two acquisitions, over stores each made atomic on
its own (#93). The cost is real and is stated under S3-R2, in
`docs/rules.md` ("Running a rule") and in the plan's self-review: a
cross-store run is consistent per store and is **not** serialised
against a premise committed after its snapshots.

S3-P2 is the one that comes close and does not deviate: spec §7 says a
run is one transaction, and a cross-store run's evaluation sits outside
it. That is a refinement §7's
own mechanism forces — the engine refuses every read of another store
inside a read-write transaction (GH #53), so "one transaction" and
"reads a scope of stores" cannot both hold literally. The reconcile,
which is what §7's atomicity is about, is still one transaction. The
rest of these rulings decide something §10 left open.

Order of authority while S3 ran: spec > these rulings > the recon's API
facts (`docs/superpowers/notes/2026-09-05-rules-s3-engine-api-facts.md`)
> the task brief.

The recon's four corrections **C1–C4** and its observation **O1** are
not repeated here. They are findings about the engine, not decisions
about the design, and they live in §C of that note with the source each
was verified against. Where a correction forced a decision it appears
below as the ruling it forced: C1 → S3-R1, C2 → S3-R2, C4 → S3-R3.

The spec is
`docs/superpowers/specs/2026-09-04-rules-as-producers-design.md`; the
contract as shipped is `docs/rules.md` ("Cross-store scope").

---

## Taken while planning

### S3-P1 — scope is a run-time argument, and the special is `graph-db::*claim-scope*`

**Decision.** `run-rule graph rule &key scope` and
`run-rules graph &key scope`; `scope` is a list of open graphs, `graph`
is put first if absent, and the rule writes only `graph` (spec §10).
During evaluation `run-rule` binds `*claim-scope*` to that list;
`claim/7` and `claim-producer/2` iterate it per route, own store first,
and with it NIL behave exactly as S1 shipped them. A Lisp caller may
bind it around a raw `select`.

**Evidence.** Spec §10 states the reads and the write side but not
where the scope lives. A run-time argument needs no schema change and
no migration, and the special is what lets the functors — homed in
`graph-db`, not in `graph-db.rules` — see it without a new parameter on
seven functors.

**Cost if wrong.** A scope that should have been a record slot must be
threaded by every caller; reversible by adding a slot later.

### S3-P2 — a cross-store run evaluates before its transaction, under composed snapshots

**Decision.** When the scope holds a store other than `graph`,
`run-rule` evaluates the body inside nested `call-with-read-snapshot`s
of every store in scope (own store included) and only then opens the
write transaction for the reconcile. A single-store scope keeps S2's
inside-the-transaction path unchanged.

**Evidence.** The engine's read-resolution rule is exhaustive: inside a
read-write transaction on A, any read of B signals
`cross-graph-transaction-error`, snapshot or no snapshot
(`transactions.lisp`, `lookup-object`'s transactional method;
`tests/multi-graph-tests.lisp`
`read-write-transaction-blocks-a-foreign-read-even-under-a-snapshot`).
Under a shared clock the composed snapshots take epochs from one
counter — equal in a quiescent image, comparable always (GH #168); the
engine deliberately provides no single instant across stores
(`call-with-read-snapshot`'s docstring, GH #53; recon C2), so no doc or
test claims one. Without a clock each store is internally consistent
and the epochs are not even comparable.

**Cost if wrong.** A cross-store run is not serialised against
concurrent premise writes — a premise committed after the snapshots is
seen by the next run, not this one, with no conflict raised.
Single-store runs keep S2's serialisation.

### S3-P3 — a premise leaves evaluation as `(identity-key . store-name)`, never as a node

**Decision.** `%desired` records for each solution the premises'
`claim-identity-key` and the name of the store they came from
(`node-graph`, with `resolve-node-graph` as the fallback), so the
reconcile — which runs inside A's transaction — reads no foreign node.
`store-name` is cl-llm's convention,
`(string-downcase (symbol-name (graph-name g)))`. The `derived-from`
record's `method` is that name when the premise's store is not the
rule's store, NIL otherwise (spec §10). Two stores holding one identity
key contribute one record (the family's `def-unique` tuple excludes
`method`), the rule's own store preferred, else the first in scope
order.

**Evidence.** The binding reason is the read pin, not the cross-graph
error (recon C4): a node `index-lookup` returns under a snapshot skips
`ensure-node-bytes`, so reading its slots after the snapshot's extent
reads B's heap unpinned. Everything the reconcile needs from a premise
is computed inside the snapshot.

**Cost if wrong.** One store name lost in the two-stores-one-identity
corner; recorded, and asserted by
`a-premise-that-moves-store-renames-its-method`.

### S3-P4 — `premises-of` resolves a premise in the store its record names, if in scope

**Decision.** A record naming no store resolves in `graph`; a record
naming a store in `scope` resolves there; a record naming a store not
in `scope` is dropped. Mirrors cl-llm's `%resolve-in` (`:absent` for a
store out of scope). `dependents-of` is unchanged: the records live in
the rule's store, and a premise from any store is looked up by its
identity key.

**Evidence.** Spec §9's reads say nothing about stores, so the
consumer's convention (kraison/cl-llm#24, `memory/trace.lisp`) decides:
a cite resolves in the store it names when that store is in scope, else
it is absent. Resolving a foreign key in `graph` instead would answer a
different claim that happens to share an identity key.

**Cost if wrong.** A caller who forgets the scope sees fewer premises,
never wrong ones.

### S3-P5 — compile stays single-store

**Decision.** The guard validates a rule's text against its own store's
schema and the cycle graph is over the rule's own store (spec §6); a
family read from a foreign store must be declared under the rule's
store's name too. A foreign store in scope whose schema lacks a family
a goal names contributes nothing.

**Evidence.** The family registry is per family symbol and the indexes
are per store (S2 recon A4), so a second `def-claim-classes` under
another store name is the whole cost of sharing a family.
`%scope-lookup` swallows the `query-precondition-error`
`%require-index` signals, per foreign store and per route, as
`%producer-candidates` and `%claim-by-identity-key` already do (recon
C1).

**Cost if wrong.** Consumers declare shared families under both names;
a cross-store cycle (A reads what B's rule derives and vice versa) is
not detected — recorded as a known limit in `docs/rules.md` and left to
the recursion slice (#333).

### S3-P6 — the unrouted walk walks every store in scope, refusal unchanged

**Decision.** `%unbound-claim-scan` maps every store in scope when no
bound is in effect; under a bound it refuses exactly as S1 does. No new
behaviour on the guarded surface.

**Evidence.** The refusal is a property of the goal's routing, not of
how many stores it would read; making a scope change it would mean a
query's admissibility depended on an operator's scope.

**Cost if wrong.** A wide scope makes an unbudgeted walk N times as
expensive; the budget that already governs it is the answer.

---

## Taken in the pre-flight scan

**None.** The scan (ledger, 2026-09-05) checked the four produce/consume
pairs across the three tasks and the arithmetic of every count a test
asserts, and found them consistent; two items were left pending on
recon B2 and B7 and both were confirmed by Task 0. Nothing needed a
ruling before Task 0 ran.

---

## Taken during execution

### S3-R1 (from recon C1) — only a FOREIGN store's missing family is swallowed

**Decision.** The own store (first in scope) calls `index-lookup` bare
and signals as S1 did; each foreign store's lookup is wrapped and
contributes nothing when that store's schema does not carry the family.

**Evidence.** A single-store goal on a family the store does not index
is S1's documented ill-typed refusal (`docs/rules.md`, "Unknown
names") and must not turn into silent emptiness because an operator
passed a scope. Recon C1 established that `claim/7`'s three indexed
routes signal today, and that only `%producer-candidates` and
`%claim-by-identity-key` swallow.

**Cost if wrong.** A scope whose *own* store lacks the family refuses
rather than reading the others.

### S3-R2 (from recon C2) — nothing claims "one instant"

**Decision.** No doc, docstring or test asserts that a cross-store
run's reads resolve at one instant. The wording is "one comparable
epoch space, equal in a quiescent image", and the clocked test asserts
only that the run works and derives from both stores. This is the
slice's one deviation from the spec's letter: §10's fourth sentence
asks for that instant, and #332 does not deliver it.

**Evidence.** The engine deliberately provides no cross-store instant
(`call-with-read-snapshot`'s own docstring, GH #53), so a test of one
would pass vacuously in a quiescent suite and fail the first time the
suite ran concurrently.

**Cost if wrong.** Spec §10's fourth sentence goes undelivered: a
cross-store run is per-store consistent and is not serialised against a
premise committed after its snapshots, so a caller reading "one
instant" literally would expect a serialisation nothing provides.
Recorded in the opening above, in `docs/rules.md` and in the plan's
self-review rather than left to be discovered; #332 does not close the
namespaces design's §12 aspiration and says so.

### S3-R3 (from recon C4) — the read pin is S3-P3's rationale

**Decision.** S3-P3's stated reason is the read pin, not the
cross-graph error: nodes never leave a snapshot's extent, and
everything the reconcile needs is computed inside it.

**Evidence.** Recon C4: under a read snapshot `lookup-object`
re-dispatches to the transactional method and never reaches
`ensure-node-bytes`, so a node returned under a snapshot is not
self-contained once the snapshot exits. On the plan's original
reasoning (the cross-graph error alone) carrying the node would have
looked safe.

**Cost if wrong.** None now; it forbids a later pass-the-node
optimisation, which is the point of recording it.

### S3-R4 (Task 2) — `method` is refreshed by copy/save, not swept and rewritten

**Decision.** A kept `derived-from` record whose premise moved store
has its `method` refreshed in `%refresh-kept`'s existing single
copy/save, together with `rule-version`. The record is not deleted and
re-inserted.

**Evidence.** Measured, not reasoned: under the brief's
sweep-and-rewrite shape
`a-premise-that-moves-store-renames-its-method` reported `:refused`
with a `DERIVATION` unique-constraint violation. `method` is not in the
family's unique tuple, and `mark-deleted` releases a unique key only
post-durability while `validate-unique-constraints` runs
pre-durability — the same collision ruling P10 already documents for
the claims themselves.

**Cost if wrong.** None known; a record's `method` changes without its
identity changing, which is what "not part of the identity tuple"
means.

### S3-R5 (Task 2) — `%desired` owns the `disjoint-premises` reset

**Decision.** `%derive` resets the three reconcile counts at the top of
every transaction attempt; `%desired` resets `disjoint-premises`, the
one count the evaluation produces.

**Evidence.** With the evaluation hoisted out of the transaction,
`%derive` runs per attempt while a cross-store `%desired` runs once, so
`%derive` resetting all four would zero a count the evaluation had
already produced —
`cross-store-validity-intersects-across-stores` would have read 0
disjoint. The single-store path re-runs `%desired`, so it stays correct
either way.

**Cost if wrong.** None; the counts are per attempt on both paths,
which is what `the-reports-counts-are-per-attempt-not-cumulative`
asserts.

### S3-R6 (Task 2) — "nothing was written to B", asserted without reading B's derivation family

**Decision.** The nothing-in-B check asserts no claim in B under
`rule/web-hosts` in either family B carries, B's own claims still
exactly `seed-b`'s, and `%graph-declares-p` false for `derivation` on
B — rather than the brief's
`(is (null (claims-by-producer b 'derivation "rule/web-hosts")))`.

**Evidence.** B never declared the derivation family, so that read
signals `query-precondition-error` (recon C1) rather than answering
NIL. The shipped assertions say the same thing and are reachable: if
the family is not declared, no record can live there.

**Cost if wrong.** None; the claim being tested is stronger, not
weaker.

### S3-R7 (Task 1, environment) — the shared `cl-temporal-extent` checkout was fast-forwarded

**Decision.** `~/work/cl-temporal-extent` (clean, on `master`, shared
with other sessions) was fast-forwarded `1ca7765` → `41b96ef` (0.3.0)
so the Quicklisp local-projects symlink resolves the version floor
`graph-db.asd` now carries. Task 1 itself avoided the checkout, running
against a read-only `git archive` extract in its scratchpad.

**Evidence.** S2 shipped `graph-db/spacetime` with
`:version "0.3.0"` on that library and the merge to `experiment` made
that the branch's floor; the checkout was three commits behind, so
every suite command in the plan aborted at system resolution.
Fast-forwarding a clean checkout to its own origin is not a source
edit.

**Cost if wrong.** Another session holding FASLs built against 0.2.0
recompiles; nothing in that repo was edited, and the branch it is on
did not change.

### S3-R8 (Task 3) — `premises-of` reads its `scope` as given

**Decision.** `premises-of` does not normalise its `scope`: it neither
puts `graph` first nor drops duplicates, because the list is only ever
searched by store name. A record with a NIL `method` resolves in
`graph` whether or not `graph` is in the list, and a non-keyword-named
store in the list meets `%store-name`'s `check-type`.

**Evidence.** `%normalize-scope`'s two jobs — own store first, read
each store once — are properties of an *evaluation*, and there is no
evaluation here. Normalising would also make `(premises-of a c :scope
(list b))` quietly resolve own-store records it already resolves, at
the cost of implying the argument means what it means to `run-rule`.

**Cost if wrong.** A caller passing `run-rule`'s exact scope gets the
same answer either way; a caller passing a bare foreign store keeps
own-store premises, which is what S3-P4 wants.

---

## Taken in the final review

The whole-branch review's rulings, taken without Kevin in the loop like
the rest, and shipped with their tests in one commit on top of
`f37c54e`.

### S3-F1 — a foreign store in scope is an operator error inside a transaction

**Decision.** `run-rule` and `premises-of` signal, before anything is
evaluated or read, when the scope names a store other than `graph` and
a transaction is open. `run-rules` inherits it through every `run-rule`
it calls; a `run-rules` with nothing runnable reads nothing and so
refuses nothing. Single-store calls are unchanged, inside a transaction
or out.

**Evidence.** A read-write transaction refuses every cross-graph read
outright and does not fall through to the read snapshots
(`transactions.lisp:319-323`; `tests/multi-graph-tests.lisp:1183`,
`read-write-transaction-blocks-a-foreign-read-even-under-a-snapshot`).
`call-with-read-snapshot` funcalls straight through for a store the
open transaction already covers, so `%under-snapshots` cannot rescue
the case: without the guard the run reached its first foreign read and
let `cross-graph-transaction-error` escape unreported, past every
handler `run-rule` has.

**Cost if wrong.** A caller who wants the run inside their own
transaction must restructure — evaluate outside it, or drop the foreign
store. That is the engine's constraint rather than this system's;
refusing up front only makes it legible.

### S3-F2 — S3-R1's own-store half is asserted directly

**Decision.** `the-own-store-still-refuses-a-family-it-does-not-index`
asserts the FIRST store's bare lookup: under a two-store scope, a goal
on a family the own store does not index still signals
`query-precondition-error`, so a scope cannot turn S1's ill-typed
refusal into silence.

**Evidence.** No test reached `%scope-lookup`'s first-store branch —
`a-store-lacking-the-family-contributes-nothing` and
`the-walk-skips-a-store-that-lacks-the-family` exercise the foreign,
swallowed half only. Non-vacuity by ablation: wrapping the first
store's lookup in the same `handler-case` turns this test red (1
failure of 401) and leaves the foreign control green; restored,
401/401.

**Cost if wrong.** None; the test asserts behaviour S3-R1 chose and
`%scope-lookup` already had.

### S3-F3 — spec §10's fourth sentence is refused, not delivered

**Decision.** The record's opening said "none deviates from the spec's
letter". S3-R2 does: §10's "Reads resolve at one instant under the
shared clock (#94)" is refused. The opening, S3-R2's own cost, and the
plan's "Self-review against the spec" line for that sentence are
corrected to say so.

**Evidence.** `call-with-read-snapshot`'s docstring (GH #53) states
that the engine provides no cross-store instant;
kraison/vivace-graph#94's closing comment describes what the shared
clock does give — one comparable epoch space — and #93 is the
per-store atomicity underneath it. The plan claimed Task 2 delivered
the sentence through the composed snapshots and the clocked fixture;
that fixture asserts only that the run derives, which is what S3-R2
chose.

**Cost if wrong.** The deviation is now visible where a consumer looks
for it. The behaviour is unchanged: a cross-store run is per-store
consistent and not serialised against a premise committed after its
snapshots.

### S3-F4 — `%premise-ref` defaults through `node-home-graph`

**Decision.** An unstamped premise node takes the engine's convention —
`(node-home-graph node graph)`, a NIL home meaning "unknown, not
foreign" (`node-class.lisp:453`) — instead of `resolve-node-graph`'s
scan of every open store.

**Evidence.** Recon B3 says every lookup on today's call graph stamps
`node-graph`, which makes the fallback unreachable; that is a fact
about today's callers, not an invariant, so the fallback's failure mode
still matters. The scan's is an unhandled condition — on the
single-store path it meets the cross-graph refusal inside the
transaction (GH #53) — while the default's is a dropped store name,
the same "fewer premises, never wrong ones" trade S3-P4 chose. No new
test: the branch is unreachable, and the docstring says so.

**Cost if wrong.** An unstamped foreign node would be recorded as the
own store's: its `derived-from` record's `method` reads NIL, and
`premises-of` then looks for the premise in `graph` and drops it when
it is not there.

---

## Carried forward

Seen in the same review and deliberately not fixed in this pass. Each
is a nit or a latent tidy, none changes behaviour a consumer can see,
and none belongs in a commit whose subject is the transaction refusal.

- **`%normalize-scope` does not check `graph-open-p`.** A closed store
  passes the type and keyword checks and fails later, at its first
  read.
- **`%reconcile-provenance`'s merge branch is unreachable.**
  `%merge-premise-refs` has already made each premise key unique per
  derived claim, so the `seen` case of the `wanted` accumulation cannot
  fire; the merge rule it applies is right, and the branch is dead.
- **`rules-in-scope` now reads two ways.** It means "the rules a
  compile checks against", which since #332 collides with `:scope`, the
  stores. A rename touches S1 and S2 code and belongs on its own.
- **`with-two-stores` leaks A if B fails to open.** Both `make-graph`
  calls are in the `let*`, so a failure on B's leaves A open until the
  fixture's scratch cleanup runs.
- **`%scope-lookup` copies the first store's list.** `append` copies
  every list but the last, so even a single-store scope pays one copy
  per route.
