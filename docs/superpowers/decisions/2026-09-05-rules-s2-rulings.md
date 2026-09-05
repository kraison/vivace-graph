# Decisions: rules S2 (#331, epic #304)

The controller rulings S2 was planned and executed under, transcribed
from the plan's header and the SDD ledger so the record outlives the
worktree. **Every one was taken without Kevin in the loop**, each
after a source-verified recon pass or a task review; each carries the
cost of being wrong as it was stated at the time. Nothing here is
re-derived or improved.

**Four deviate from the spec** — P1, P2, P8 and P10 — and are the
deviation record for this slice. The rest refine it: they decide
something the spec left open, or fix a spelling the spec's own
mechanism contradicts.

Order of authority while S2 ran: spec > these rulings > the recon's
API facts (`docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md`)
> the task brief.

The recon's eight corrections **C1–C8** are not repeated here. They
are findings about the engine, not decisions about the design, and
they live in §C of that note with the source each was verified
against. Where a correction forced a decision it appears below as the
ruling it forced: C1 → P10, C5 → P11, C3 → T3-R2, C7 → T2-R1.
The spec is `docs/superpowers/specs/2026-09-04-rules-as-producers-design.md`.

---

## Taken while planning

### P1 (deviation) — one `rule` vertex per rule NAME, not per version

**Decision.** A store holds one `rule` record per name. A new version
is `copy`, `setf rule-version`, `save`, not a second record.
`run-rules` therefore has exactly one candidate per name.

**Evidence.** Spec §5 says "one vertex per rule version" and, in the
same section, gives `def-source` the identity
`(:namespace :rule :key-slot name)`. `def-source` emits `def-unique`
on the key slot (`spacetime/source.lisp:229`), so the identity facet
itself makes a second record with that name a uniqueness violation.
The two sentences cannot both hold; the identity facet wins because it
is the mechanism. The engine's revision history is the record of the
old text, and `rule-version` on every derived claim is the record of
which version derived it.

**Cost if wrong.** Two versions of one rule cannot coexist as records.
Reversing means a composite key slot and a policy for which enabled
version runs.

### P2 (deviation) — compile per run, not on `open-graph`

**Decision.** `run-rule` compiles the rule it is about to run, and
`run-rules` compiles every candidate first. A rule that fails to
compile is reported (`outcome :refused`, tag `:rule`) and skipped; the
store opens regardless.

**Evidence.** Spec §6 says compilation runs on `open-graph`.
`open-graph` is a `defun` with no hook (`graph.lisp:1103`); adding one
is a core change, which spec §13 puts outside this slice. A rule
compiles in milliseconds, rules are few, and `run-rules` runs seldom.
Reporting-and-skipping is what §6 wanted from compiling at open.

**Cost if wrong.** Compile cost on every run; a cache is a later
addition behind the same functions.

### P3 — a rule write is refused at commit when it does not compile

**Decision.** `%validate-rule-writes` joins
`graph-db:*commit-validators*` and signals `rule-compile-error` — a
`graph-db:constraint-violation` — for a written `rule` vertex that
does not compile against the post-commit rule set, the cycle check
included.

**Evidence.** Spec §6: "compilation runs ... on every rule write";
§5: "validated like any other write". The validator follows
`%validate-extent-disjointness`' shape
(`spacetime/temporal.lisp:69-115`). So the store never holds a rule
that could not run *at the time it was written*; P2 covers drift after
that (a `def-rule` added later, a schema change).

**Cost if wrong.** An operator cannot park a half-written rule in the
store; they use `enabled nil` for that, and a disabled rule still has
to compile.

### P4 — `run-rule` always runs under a resource bound

**Decision.** `run-rule` binds `*query-default-max-inferences*` /
`*query-default-timeout*` from `*rules-max-inferences*` /
`*rules-timeout*` (defaults: the DSL's) and signals a plain error if
an operator has NILed both. `:allow-cost-unbounded` is never passed,
so an unrouted `claim/7` goal in a body is refused as cost-unbounded
(→ report refusal `:budget`) and the family walk is unreachable from
`run-rule`.

**Evidence.** #331's carried finding: `map-vertices` `:record-reads`
defaults to `T`, so inside `run-rule`'s single transaction a family
walk would join every scanned node to the read set and the run would
conflict with any concurrent writer touching them. This is the
"refuse the walk the way a budget refuses it" option the issue argued
for, rather than `:record-reads nil`.

**Cost if wrong.** Not stated in the ruling. In practice a rule author
must bind the namespace and key of every body goal, or put a
`claim-producer` generator in front of it.

### P5 — `claim-producer` generators are moved to the front of the body

**Decision.** A body goal `(claim-producer ?v "p")` — variable first,
literal producer second — is a generator; `compile-rule` moves every
such goal to the front of the body. Every other goal keeps its order.

**Evidence.** The S1 handoff records that goal order is load-bearing
and unenforced, and names `compile-rule` as the place to fix it.
Running a generator first is never wrong: a later `claim/7` on `?v`
takes its node route, and an otherwise unrouted `claim/7` on `?v`
becomes routed.

**Cost if wrong.** A rule author who relied on a filter-direction
`claim-producer` after an indexed `claim/7` gets the same rows via the
producer route, possibly slower. Recorded in `docs/rules.md`.

### P6 — the recursion graph is over relation names, and an unbound relation reads everything

**Decision.** The cycle graph's nodes are relation names. A body
`claim/7` whose relation argument is a variable gets an edge to every
head relation in scope, its own included, so such a rule is always the
one-node cycle and is refused with a message saying to bind the
relation. Families are ignored in the graph: a rule deriving relation
R in family F1 that reads R in F2 is refused.

**Evidence.** Spec §6 keys the graph on relations. Ignoring families
is conservative and spec-literal.

**Cost if wrong.** Not stated in the ruling. A rule set that
legitimately crosses families on one relation name has to rename one
side.

### P7 — a solution's extent is intersected whatever the family's temporality

**Decision.** `:extent-policy :premises` intersects the premises'
extents for a non-temporal family too, and still drops the disjoint
solutions. `:none` derives no extent anywhere.

**Evidence.** Spec §8 states the `:premises` policy for temporal
families. Applying it to a non-temporal family costs nothing and
records the validity the premises had; the derived claim's identity
ignores the extent there, so duplicates collapse by endpoints alone
and the first solution's extent is kept (see T4-R3).

**Cost if wrong.** Not stated in the ruling. A non-temporal derived
claim carries an extent its identity does not use.

### P8 (deviation) — `run-rules` includes a `def-rule` only where the store carries its family

**Decision.** `run-rules` filters `*def-rules*` by
`(graph-db.query:schema-type-names graph :vertex)`. A def-rule that
is filtered out is not reported at all. `run-rule` called on such a
rule by name is refused with tag `:rule`.

**Evidence.** `def-rule`s are image-wide; a family the store's schema
does not declare cannot be swept or derived into — the producer index
lookup signals `query-precondition-error`. Silence rather than a
report because it is not this store's rule. The spec has no
per-store filter, so a strict reading would report every def-rule in
the image against every store.

**Cost if wrong.** A def-rule that the operator expected to run
somewhere is silently absent from that store's reports; `run-rule` by
name is the way to see why.

### P10 (deviation, from recon C1) — `run-rule` reconciles, it does not sweep-then-insert

**Decision.** `run-rule` derives first, keys every solution by
identity, and then, in the same transaction: keeps every existing
claim of the producer whose identity is re-derived (refreshing its
`rule-version` by copy/save when the version changed), marks deleted
every existing claim no longer derived, and constructs only the new
identities. The `derived-from` records reconcile the same way. The
report gains `kept`.

**Evidence.** `validate-unique-constraints` reads the committed unique
index before durability and only skips the *deleting* write, so a
`mark-deleted` followed by a fresh claim with the same identity tuple
in ONE transaction always collides
(`tests/spacetime/claim-query-tests.lisp:128-140` asserts exactly
that; design §6.4 splits the transactions for this reason). Spec §7's
"sweep-then-insert in one transaction" is therefore unbuildable as
written. Order does not change the answer, because a body reads the
committed store either way (recon A6). Atomicity holds, and unchanged
claims keep their node id and version chain — better provenance than
the spec asked for.

**Cost if wrong.** A retracted derived claim whose identity is
re-derived is kept retracted, not re-asserted — a retraction is
someone's deliberate act; the operator deletes it and reruns.

### P11 (from recon C5) — two solutions differing only in extent KIND collapse

**Decision.** The dedupe key uses the extent *start* alone for a
temporal family, so an instant at T and an interval starting at T are
one identity; the first solution's extent is kept.

**Evidence.** `claim-identity-key` keys a temporal family on
`extent-sexp-start-key`, so the family's own identity rule would force
the collapse anyway.

**Cost if wrong.** None beyond the loss of an instant/interval
distinction that identity already loses.

### P9 — `extent-intersection`'s keyword contract

**Decision.** `(extent-intersection a b &key precision semantics
standing)`, defaulting to A's standing and semantics and the coarser
precision. An instant on either side gives an instant (the point
narrowed to the other extent's hull); a result that is certainly empty
(`extents-disjoint-p`) is NIL. Fuzzy bounds combine coordinate-wise:
the later start (max of earliests, max of latests), the earlier end
(min of both), `:unbounded` read as −∞ in an earliest and +∞ in a
latest.

**Evidence.** The library must not assume `:inferred`; the rules
caller passes `:semantics :validity :standing :inferred`. The
library is public and domain-neutral
(kraison/cl-temporal-extent#5).

**Cost if wrong.** Not stated in the ruling. The defaults are the
reversible half; the coordinate-wise combination is the algebra's own.

---

## Taken in the pre-flight scan

### PF1 — substring assertions are case-insensitive, refusal messages are downcased

**Decision.** Every substring assertion in the S2 tests uses
`(search sub text :test #'char-equal)`, and refusal messages print
symbols downcased with `~(~A~)`.

**Evidence.** `~S` and `symbol-name` print uppercase and `search` is
case-sensitive, so the plan's tests as written would have gone red on
correct code.

**Cost if wrong.** None beyond wording.

### PF2 — an effecting functor in a body is a RUN refusal, not a compile refusal

**Decision.** `(retract ?p)` in a body compiles. It is refused when
the goal runs with effects off (`prolog-permission-error` → report tag
`:rule`). The compile tests use an unregistered functor
`(no-such-functor ?p)` as their bad body, and a run test covers
`(retract ?p)`.

**Evidence.** `require-effect` runs inside functor bodies and the
guard whitelists by registry; recon A16 confirmed there is no static
effect classification to consult. Spec §6's "refused by the guard as
today" is not what the guard does.

**Cost if wrong.** A bad rule is stored and refused at every run
instead of at write; reversible by adding a static check once a
registry exists.

---

## Taken during execution

### T1-R1 — `extent-intersection` normalises to effective bounds, and an open end stays a range

**Decision.** The plan's test
`an-open-end-is-narrowed-by-the-other-extents-end` expected an exact
end; the expectation changed to the honest answer — end earliest = the
result's start earliest, end latest = the other extent's end, not
exact — and `extent-intersection` normalises its result to effective
bounds (the end's earliest raised to the start's earliest, the start's
latest lowered to the end's latest) so a constructed extent never
carries an end that precedes its start.

**Evidence.** Under the library's own semantics an `unknown-bound`
means unknown (CHANGELOG #2), so the coordinate-wise end of an open
end is a range. "Still going" is a consumer's reading; the algebra
reads unknown as unknown everywhere else.

**Cost if wrong.** A caller wanting open-ended-means-forever gets a
fuzzy end and must say so itself.

### T2-R1 (from recon C7) — `graph-db.spacetime:canonical-relation-p`, single colon

**Decision.** The `:check` slot options in `def-rules-schema` name the
predicate with a single colon; recon A2's `::` fallback is
unnecessary.

**Evidence.** The symbol is exported and the schema-function registry
is keyed on it.

**Cost if wrong.** None.

### T2-R2 — the guard test uses an unregistered functor, and the rule accessors are explicit

**Decision.** Task 2's guard assertion uses `(no-such-functor ?x)`
rather than `(retract ?x)` (PF2 had been missed in that test), and
`def-rules-schema` writes explicit `:accessor rule-*` clauses.

**Evidence.** PF2, above. `def-vertex` defaults an accessor to the
bare slot name, which would have given `name`, `version`, `head`,
`body` rather than the `rule-`-prefixed accessors the package
exports.

**Cost if wrong.** None.

### T3-R1 (from recon A13) — the write validator compiles under the manager lock

**Decision.** `%validate-rule-writes` does its compiling — including
creating and deleting the guard's scratch package — inside the
manager-locked commit region.

**Evidence.** `*commit-validators*` are called as `(funcall fn tx
graph)` in that region (`transactions.lisp:3441`). Rule writes are
rare, and the scratch package's name race is a counter with 64
retries.

**Cost if wrong.** A rule commit holds the lock for milliseconds more;
revisit with a compile cache if rule writes ever become frequent.

### T3-R2 (from recon C3) — `%engine-goal-p` follows the functors' home package

**Decision.** `%engine-goal-p` compares a goal head's package against
`(symbol-package 'graph-db:claim/7)`, not against a literal
`GRAPH-DB`.

**Evidence.** The guard's canonical head comes from the whitelist's
home package. Comparing to where S1 actually homed `claim/7` means
the compiler follows the functors if they ever move.

**Cost if wrong.** None.

### T3-R3 (from recon A10) — a bare `?` in rule text is a compile refusal

**Decision.** `compile-rule` refuses a bare `?` anywhere in the rule
text, head or body.

**Evidence.** Read into the guard's scratch package every `?` in one
text is the *same* NAMED variable, not the engine's anonymous one, so
all of them would have to unify — the rule would mean something other
than it reads. `replace-?-vars` only replaces `graph-db::?` by `eq`.

**Cost if wrong.** A rule author writes named variables.

### T3-R4 — `%validate-rule-writes` guards with `(find-class 'rule nil)`

**Decision.** The validator is inert until some store has evaluated
`def-rules-schema`, tested by `(find-class 'rule nil)`.

**Evidence.** The validator is image-wide but the `rule` class is made
per store by the schema macro, so in an image that never ran it there
is no class to `typep` a written node against.

**Cost if wrong.** None.

### T3-R5 — `enabled` reads the same on a stored rule and a `def-rule`

**Decision.** `rules-in-scope` filters disabled def-rules exactly as
it filters disabled stored rules. The docstring and `docs/rules.md`
were reconciled to say "every enabled def-rule".

**Evidence.** `enabled` is one slot with one meaning; a disabled rule
is not run and does not constrain the cycle graph. The one place
`enabled` buys no exemption is the write: `%validate-rule-writes`
compiles every `rule` record it commits, disabled or not.

**Cost if wrong.** A rule set that a disabled rule would have made
cyclic compiles while that rule stays disabled, and is refused when it
is enabled — which is the write-time refusal working, one step later.

### T4-R1 (from recon A9) — `extent-intersection` is called through its own package

**Decision.** `%premise-extent` calls
`temporal-extent:extent-intersection`; there is no spacetime
re-export.

**Evidence.** Recon A9 enumerated `graph-db.spacetime`'s re-exports;
`extent-intersection` is not among them and adding one is a change to
spacetime this slice does not need.

**Cost if wrong.** None.

### T4-R2 — `%stored-rules` answers NIL for a store with no `rule` type

**Decision.** `%stored-rules` and `%resolve-rule` check
`graph-db.query:schema-type-names` before naming the `rule` type, so
`run-rules` on a store that never evaluated `def-rules-schema` reports
nothing rather than erring.

**Evidence.** `index-lookup` on a type the schema does not carry
signals `query-precondition-error` — which is what `%resolve-rule`
would hit. (`map-vertices` is the softer of the two: it skips a
designator that resolves to no registered type, so the guard there is
a fast path rather than the thing that prevents an error. See T5-R4.)

**Cost if wrong.** A store that forgot `def-rules-schema` runs no
rules silently; `run-rule` by name still says there is no such rule.

### T4-R3 — a collapsed non-temporal claim keeps the FIRST solution's extent

**Decision.** When solutions collapse to one identity in a
non-temporal family, the first solution's extent is kept and the
premises are unioned. There is no re-intersection. Documented, not
changed.

**Evidence.** The extent plays no part in a non-temporal family's
identity, so the collapse is by endpoints alone and some extent has to
win. Re-intersecting would narrow a claim's validity by the accident
of how many ways it was derived, which is not what "the intersection
of its premises" means for a claim derived twice over.

**Cost if wrong.** A caller reading the extent of a multiply-derived
non-temporal claim gets one derivation's window, not the union or the
intersection of all of them. `premises-of` names all the premises
either way.

### T5-R1 — the S1 gate's test asserts the refusal, not the row count

**Decision.** The plan's test for the explicit-NIL `?c` gate
(`(is (zerop (select-count () (claim-producer nil "scan-a"))))`) is
vacuous: both the old code and the new answer zero rows there. The
shipped test runs under a budget and asserts that
`(claim-producer nil ?p)` is **not** refused as cost-unbounded, with
the genuinely unbound goal as the control that still is.

**Evidence.** With `?c` bound to NIL the old code took the generator
branch and unified against nothing — a cost difference, not an answer
difference — and took the neither-bound refusal branch when `?p` was
unbound too. The refusal is the only user-visible half.
`%unbound-p` is also written without the plan's `graph-db::` prefix,
since `rules/facts.lisp` is `(in-package #:graph-db)`.

**Cost if wrong.** None; the answer-shape assertions are still there.

### T5-R2 — the provenance keep is restricted to `derived-from`, not only to one per pair

**Decision.** `%reconcile-provenance` keeps a record only when its
relation is `"derived-from"` **and** its pair has not already been
kept. Everything else the producer holds is swept, as a record the
derivation no longer asks for already was.

**Evidence.** The filed minor was that a second record for one pair
sees `:kept`, which is truthy, and is kept as well. Fixing only that
leaves the outcome dependent on which of the two the producer index
happens to yield first — a foreign record could win and the real one
be swept. `def-claim-classes`' identity constraint makes an exact
twin impossible, so the reachable duplicate differs in its relation,
and restricting the keep removes the ordering hazard rather than
merely halving it. The records under `rule/<name>` are the rule's
alone.

**Cost if wrong.** A consumer that wrote its own annotation claims
under a rule's producer name loses them on the next run. That name is
the rule's; annotations belong under the consumer's own producer.

### T5-R3 — the `*print-case*` fix is tested as a unit, because the engine has the same bug

**Decision.** `%constructor` builds `MAKE-<CLASS>` from `symbol-name`
rather than `format`, and the test calls `%constructor` directly under
`*print-case* :downcase` rather than running a rule.

**Evidence.** `make-functor-symbol` builds `NAME/ARITY` with
`(format nil "~{~a~}" ...)` (`prologc.lisp:218`), so under
`:downcase` **no** Prolog goal resolves — a `run-rule` test would go
red on core, not on this. That is a pre-existing core defect and spec
§13 puts core outside this slice; filed as
kraison/vivace-graph#342.

**Cost if wrong.** The unit test does not prove `run-rule` survives a
changed `*print-case*` — it does not, for reasons this slice cannot
fix.

### T5-R4 — T4-R2's test names `%resolve-rule`'s guard, not `%stored-rules`'

**Decision.** `a-store-without-the-rule-schema-runs-no-rules` asserts
that `run-rule` by name says "no rule named" on such a store, as well
as that `run-rules` reports nothing.

**Evidence.** Ablation: removing the `%graph-declares-p` guard from
`%stored-rules` left the test green, because `resolve-node-type-ids`
skips a designator that resolves to no registered type
(`node-class.lisp:338`), so `map-vertices` returns NIL rather than
erring. Removing it from `%resolve-rule` turned the test red with a
`query-precondition-error`. A test that names a mechanism it does not
exercise is worse than no test.

**Cost if wrong.** None; the `%stored-rules` guard stays as the fast
path it is.

### T5-R5 — the CHANGELOG entry describes the reconcile, not the sweep

**Decision.** The plan's CHANGELOG text ("`run-rule` sweeps the rule's
previous derivation and derives afresh") is pre-P10 wording; the entry
says "derives afresh and reconciles the result with the rule's
previous derivation".

**Evidence.** P10 replaced sweep-then-insert, for a reason the entry
would otherwise contradict. Task 4's commit subject carries the same
stale wording; its body corrects it, and the history is not being
rewritten.

**Cost if wrong.** None.
