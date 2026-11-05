# A forward step reads what it touches, not the producer's derivation

**Status:** design written 2026-11-05 from #457's direction (raised by the
forward-rules branch's final review and named as the next unit in #456's
hand-off); builder check folded in. Kevin was not in the room: every
choice below is Claude's call, listed in §7 for him to check.
**Prior specs:** `2026-11-04-forward-rules-design.md` (the step, §4.1's
partial reconcile, §4.2's targeted path),
`2026-09-06-recursive-rules-fixpoint-design.md` (the stratum run and its
`existing` table, GH #333).

## 0. Problem

Three reads in `rules/run.lisp` enumerate the touched rule's whole
derivation through `claims-by-producer`:

| read | where | when |
|------|-------|------|
| `%seeded-existing` | `reset-run`, per rule per stratum run | before round 0 |
| `%reconcile-claims` | `finish-run` and `%rederive-dependents` | at the fixpoint |
| `%reconcile-provenance` (the `derivation` family) | same | same |

`run-rules` recomputes the derivation from scratch, so reading all of it
is the price of the job. The forward step (`%run-stratum` with
`:forward-delta`) and the targeted path (`%rederive-dependents`) derive
from a delta, so a one-claim commit on a store with N derived claims
under the touched rule reads N claims and N derivation records three
times over. `docs/rules.md` "Forward evaluation / Cost" says this is
what stops a consumer with a large derived set from opting in.

## 1. Goals and non-goals

Goals:

- A forward or targeted step's reads are proportional to what the step
  derives, keeps or sweeps, never to the producer's standing derivation.
- The step's results are unchanged: the rules suite, the standing-query
  equivalence test (forward spec §8) and `run-rules`' idempotence over a
  forward-derived store all hold as they do today.
- `forward-rules-status` reports the last step's elapsed time, so a
  consumer can see the cost before and after opting in.
- The cost is measured, not asserted: a perf bench runs a one-claim step
  against two derived-set sizes and reports the ratio.

Non-goals:

- The full path (`run-rule`, `run-rules`, `%run-stratum` without a
  delta, and the forward step's `:full` mode for recursion and `not`)
  keeps its enumerating reads. Its job is the whole derivation.
- `%stratum-base-facts`' family walk (#350's sibling) is not touched; the
  forward path does not take it.
- No new index. The subject-relation index and the derivation family's
  subject index already answer the per-identity questions.

## 2. The per-identity resolver

A derived claim's identity, less the producer, is its dedupe key
(`%dedupe-key`, `%existing-key`): subject namespace and key, relation,
object pair, extent start. Given the rule, a dedupe key names at most
one current claim, found through the subject-relation index the way
`%claim-by-identity-key` finds one by its full key:

```
(%claim-by-dedupe-key graph compiled key)
  => index-lookup on the family parent over
     +claim-subject-relation-index-slots+ (sns skey rel),
     filtered to the rule's producer and an EQUAL %EXISTING-KEY,
     a current claim over a retracted twin (D4), else NIL.
```

The candidates are the claims of one subject under one relation, from
every producer: bounded by the subject, never by the derivation.

`%stratum-run`'s `existing` becomes a small struct, `%existing`: a table
from dedupe key to claim (or a cached miss), a `lazy-p` flag, and the
graph and compiled rule to resolve with. `(%existing-claim ex key)`
answers from the table, and under `lazy-p` resolves a miss through
`%claim-by-dedupe-key` and caches it. The full path seeds the table as
`%seeded-existing` does today, `lazy-p` nil. The forward path builds an
empty lazy one. `%round-delta` asks the resolver instead of `gethash`,
and records what it kept or constructed under the key, so the
reconcile at the fixpoint finds every desired key in the table without
a second lookup.

## 3. The reconcile, keyed by the step

`%reconcile-claims` keeps its enumerating body for the full path (no
`:sweep-keys`). With `:sweep-keys` it takes an `:existing` resolver and
walks the step's own keys:

1. each key of `order` (the step's desired identities): resolve; a claim
   found is kept, refreshed and re-opened as today; none found, one is
   constructed and counted `derived`, as the enumerating pass does. On
   the stratum path the branch never fires (`%round-delta` put every
   key in the table); on the targeted path it is the reconcile's
   completeness: `%rederive-dependents` has no round pass, and a
   desired key that stands nowhere must become a claim there too;
2. each key of `sweep-keys` not in `desired`: the claim under it, when
   current, is retracted.

`sweep-keys` stays a key -> T table and every key resolves through
`existing` **inside the transaction**: the dependents
`%rederive-dependents` is handed were read before it opened, and a
version that moved since (another commit, a hand retraction) must be
retracted as it stands now, not as it was read, exactly as the
enumerating pass read it fresh through the overlay (GH #324). One
index lookup per swept key, bounded by the dependents. The counts and
the `(decf kept derived)` correction in `finish-run` are unchanged: a
key `%round-delta` constructed this run is in the table, so the
reconcile counts it kept exactly as the enumerating pass did.

`%reconcile-provenance` with `:sweep-keys` reads the `derivation`
records per derived claim instead of per producer: for each claim of
`standing` and of `swept`, `claims-touching` on the derivation family
with the claim's identity key as subject, filtered in Lisp to the
rule's producer, exactly the record set the enumerating pass would have
met for that subject. Keep, refresh, retract and the `touched` rule are
as today. A record whose subject the step neither stands nor swept is
never read, and was never a candidate for retraction before either
(`touched` is a subset of standing-under-a-sweep-key and swept), so the
outcome is identical.

The one behavioural difference, accepted: two **current** claims of
one dedupe key under one producer (a hand-made duplicate the uniqueness
constraint missed) used to have the second swept by the enumerating
pass when it was under a sweep key; the per-key path keeps the one the
index answers first and leaves the other. `run-rules` still sweeps it.

## 4. Status

`forward-rules-status` gains `:last-step-elapsed`, the wall seconds of
the last `forward-step` drain (every delta it ran, strata and agenda
included), NIL until a step has run. Per-rule elapsed stays on the
`rule-report`s as before.

## 5. Measurement

`tests/perf/rules-bench.lisp` gains `bench-forward-step-reads`: a store
with N base claims and one rule deriving one claim per base claim;
`enable-forward-rules` brings it to its fixpoint; then one base claim is
committed and the synchronous step timed. Run at N/10 and at N, the
reported `forward-step-scale-ratio` is the N time over the N/10 time:
near 1 when the step is bounded, near 10 when it is not. Reported, not
gated, as #451's bench is; the N = 10^5 numbers go on #457.
`*rules-max-solutions*` (100,000) is bound above N for the fixture.

A FiveAM test is the mutation gate: with `claims-by-producer` swapped
for a counting wrapper, a one-claim commit's forward step and a
one-claim retraction's targeted step each call it **zero** times for
the touched producer. Reverting any one of the three reads fails it.

## 6. Docs that travel

- `docs/rules.md`: the "Forward evaluation / Cost" paragraph says what a
  step reads now and names the bench; the `forward-rules-status` plist
  gains `:last-step-elapsed`.
- `docs/superpowers/specs/2026-11-04-forward-rules-design.md` §4.1 item
  3 points here for the reconcile's reads.
- #457 closes with the bench numbers and the landing SHA.

## 7. Calls I made

- A resolver struct over the existing table rather than a second code
  path through `%round-delta`: the full path's behaviour is byte for
  byte what it was.
- `%reconcile-claims` branches on `:sweep-keys`, which already means
  "derived from a delta", rather than on a new flag.
- Provenance read through `claims-touching` on the subject side without
  `:relation`, filtered in Lisp, so a non-`derived-from` record under
  the producer is still swept as the enumerating pass sweeps it.
- The duplicate-current-claim difference in §3 is accepted and
  documented, not engineered around.
- From the builder check: the per-key reconcile keeps the construct
  branch (the targeted path has no round pass), and a sweep key is
  resolved inside the transaction rather than carried in as the object
  the caller read outside it.
- One status field, `:last-step-elapsed`; the reports' per-rule times
  are not surfaced on the status.
- The bench reports a ratio across two sizes rather than counting reads:
  the harness measures time, and the test is what counts reads.

## 8. Acceptance

#457's three items: the step for a one-claim commit on a store with 10^5
derived claims under the touched rule reads a bounded number of claims
(the bench ratio, recorded on the issue); the rules suite green; the
standing-query equivalence test passes.
