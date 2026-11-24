# A page of claims valid at an instant skips the claims that aren't

**Issue:** kraison/vivace-graph#499. **Date:** 2026-11-24. **Status:**
design for approval. Kevin chose approach 1 below (a validity index),
accepted the race in §5 and chose where §6 measures. The rest is my
proposal.

## Intent

A consumer that wants "what holds at T" pages a relation like this:

```
(select (claim ?c belief "ns" ?k "rel" ?ons ?okey)
        (> ?k "<after>")
        (claim-current ?c)
        (claim-valid-at ?c "<T>")
        (claim-recorded-at ?c ?at)
        :limit 500)
```

Today `claim/7` takes the subject namespace route (#389, `%namespace-route`
in rules/facts.lisp). It walks every key the vocabulary index files under
the namespace, reads each key's claims from the subject-relation index,
and unifies each claim into the continuation. `claim-valid-at/2` then
rejects the claims whose validity ended before T, one at a time. #451
skips the keys before the bound, but every claim after it is still read.

In a relation that is mostly history, nearly all of that work is wasted.
The issue measures one at about 94% history, in long runs of keys whose
every claim is closed. A 50-row page cannot cross the longest run inside
a 100,000-inference budget. Each closed claim costs about 4 inferences
to step over, and the keys themselves cost wall time even where no goal
runs. Retracting the history doesn't help: a retracted claim stays in
every index, so a page still walks it.

After this change, that page costs about what it returns. A new ordered
index files each claim by whether it is current and where its validity
ends. The route reads the claims still open, in key order, and merges in
the few that closed at or after T. A claim whose validity ended before
T, and a key all of whose claims did, is never read. Goal text written
as above gets the speed-up with no edit.

## Goals

- A page of claims current and valid at T costs in proportion to the
  claims valid at T in its key range. History ahead of them costs
  nothing.
- The same answers: the same rows in the same order, with the key bound,
  `:offset` paging inside one key, and the trailing goals unchanged.
- The query text the docs already recommend is what gets faster.
- Retracted claims are skipped too, since `claim-current` is in the
  shape. The issue left this to the design. The same index carries it
  at no extra cost (calls, below).

## Non-goals

- A free T, `claim-valid-at` without `claim-current`, or either goal not
  right after the claim goal. Those keep today's route.
- The object side: a namespace bound on the object, its key free. Only
  the subject route with a relation bound is fused, because that is the
  shape that is slow.
- `claim-valid-from` / `claim-valid-to` filters written by the caller.
  They stay correct and pay for themselves.
- A snapshot cursor. A key bound is still not a cursor (`docs/rules.md`,
  "Paging").
- Compaction, or any change to how a claim is closed or retracted.

## Anti-goals

- A route that can drop a row the unfused query would return, other than
  in the race named under §5. The goals the rewrite fuses stay in the
  body, so a row the route wrongly admits is still filtered. The index
  may only ever skip a claim the predicates would reject.
- A second collation for the key bound. As in #451, a key is skipped by
  `>/2`'s own predicate (`%key-after-p`), never by its position in an
  index.
- Fusing across a cut, `not`, `or`, `if`, or any goal between the claim
  goal and the goals it fuses.

## Constraints

- Public repository: domain-neutral text throughout.
- Rules depends on spacetime and query; core depends on neither. The
  index is declared in spacetime (`def-claim-classes`), the route and
  the rewrite live in rules, and core gains nothing.
- 80 columns, spaces, terse comments that point here.
- The peer wire is untouched. A secondary index is local to a store,
  and its sidecar follows the existing index persistence.
- ECL is not release-gating (CLAUDE.md); verify on SBCL.

## Approaches

1. **A validity index (chosen).** One more ordered composite index per
   claim family, keyed by namespace, relation, currency, validity end
   and key. A page reads the open bucket in key order and merges in the
   claims that closed at or after T. It is built from what exists: a
   multi-slot `def-index` with per-slot `:canonicalize` (GH #107), as
   #392's touched-at index is. The costs are one more index entry on
   every claim write, an index build when an existing store first opens
   on this engine, and a fallback to today's route where the merge set
   would be large.
2. **An interval-augmented key index.** One ordered index by key per
   namespace and relation, with each span carrying the latest validity
   end beneath it, so a page skips any span that ended before T. It is
   exact for every T with no fallback. But it is a new on-disk structure
   in the storage layer, with aggregate maintenance on every write. That
   is far more work, in the layer every consumer rides on. Not chosen.
3. **An open-claims index for "now" queries only.** It takes the route
   when a per-relation watermark shows that no closed claim can be valid
   at T. It is the simplest, but a consumer that writes while it reads
   moves the watermark past its own T, so the route would rarely fire.
   Dropped.

## Design

### 1. The index

`def-claim-classes` (spacetime/claim.lisp) declares one more index on
every family's parent class, next to `claim-touched-at`:

```
(graph-db:def-index ,parent (subject-namespace relation
                             transaction-extent-sexp extent-sexp
                             subject-key)
    ,graph-name :name claim-validity
    :canonicalize (nil nil <currency> <validity-end> nil))
```

- **Currency**, from `transaction-extent-sexp`: one value for a current
  claim (an open transaction period) and another for a retracted one.
  Core never passes a NIL slot to a canonicalizer: it stores
  `+null-component+` (`%canonical-components`, index.lisp). An
  unstamped claim, one with no transaction extent, is current to
  `claim-current-p`, so the route reads the null-currency bucket as
  current too (§2).
- **Validity end**, from `extent-sexp`: one value for an open claim,
  where the end's latest edge is `:unbounded` (an unknown end counts as
  open). Otherwise it is the latest edge as `%timestamp-key`'s
  `(day sec nsec)`, which orders like the instant it encodes. A claim
  with no extent is never valid at any T. It sits under the null
  component, which no read visits.
- **The invariant** a test pins: a claim is outside both read ranges
  (§2) only if its latest validity end is before T, or it is retracted.
  So every claim the route skips is one that `claim-valid-at/2` or
  `claim-current/1` would reject.

The index is maintained by the existing secondary-index machinery on
every save. Closing a claim's validity (a copy, a new extent, a save)
moves its entry from the open bucket to its end. Retraction moves it to
the retracted bucket, and re-assertion (`register-node` re-opening a
retracted claim) moves it back. An existing store builds the index at
its first open on this engine (`install-secondary-indexes`); §6 measures
what that costs.

### 2. The route

A new global functor in rules/facts.lisp, the fused form:

```
(claim-current-valid ?c ?family ?sns ?skey ?rel ?ons ?okey
                     ?skey-after ?okey-after ?at)
```

**Contract:** exactly the rows of `claim-after/9`, given its first nine
arguments, that are current (`claim-current/1`) and whose validity
possibly contains `?at` (`claim-valid-at/2`'s predicate). The solution
order is the same as `claim-after/9`'s. It filters every candidate by
both predicates on every route, so it is complete when written by hand.

It takes the validity route only when all of these hold at run time.
Otherwise it answers by `claim-after/9`'s route plus the two filters.

- `%claim-route` would pick the subject vocabulary route with the
  relation bound: the namespace is bound and names a keyword, the
  subject key is unbound, and the relation is bound.
- `?at` is bound to an instant, read as `claim-valid-at/2` reads it.
- No read-write transaction covers a store in scope (`*transaction*`).
- No store in scope is read under a snapshot or an as-of epoch
  (`*read-snapshots*`) that a commit has passed. A snapshot of a store
  has been passed when its transaction's `start-tx-id` differs from the
  store manager's `tm-current-epoch`. This was revised on 2026-11-24;
  see "Under a snapshot" below.
  - A transaction reads at its start epoch (`lookup-object`,
    transactions.lisp), and `call-with-read-snapshot` inside one enters
    nothing in `*read-snapshots*`.
  - The index releases an entry when a write applies (GH #345). So at
    an older epoch, a claim that a later commit closed has already lost
    its open entry, while it still resolves as open.

**Under a snapshot** (Kevin, 2026-11-24). Every guarded query
compiles to `(select (:effects nil :snapshot t …))`
(`query/compiled-cache.lisp`), and so does `profile-goals`
(prolog-explain.lisp). A route that fell back under every snapshot
would never run for a guarded page. In the measurement, the fused page
kept a ratio of 286 against the ≤ 1.5 acceptance.

So the route also runs under a snapshot that no commit has passed:

- `create-transaction` reads `tm-current-epoch` under the manager
  lock. A commit takes its epoch (`tm-next-epoch`) and applies to the
  index under that same lock.
- So while the snapshot's `start-tx-id` equals the store's
  `tm-current-epoch`, no commit has applied since the snapshot began,
  and the index holds exactly the snapshot's state.
- An as-of read starts at its epoch + 1, so it passes only at the
  latest epoch (`latest-epoch`).
- Under a shared clock, another store's commits advance the epoch too.
  The check is then conservative: it can only fall back.

The check is made once per store, at the route's start, before
anything is collected. A commit after the check is §5's race.
Node reads still resolve through the snapshot. `index-lookup`
re-checks each node against its key (`%index-hit-p`), so a node a
later commit changed drops out rather than being returned at the wrong
version.
- Every store in scope that carries the family has the index.
- The **closed set** is within the cap. This is every current claim of
  the namespace and relation whose validity end is at or after T, over
  all stores in scope. The cap is 10,000 entries (calls, below), and
  the route stops counting once it is past the cap.

**The validity route, per page:**

Each step reads two currency buckets per store: current, and the null
component, which holds unstamped claims (§1). That is four index ranges
per store.

1. **The closed set:** for each store and bucket, one index range from
   (namespace, relation, currency, T's key) up to the last closed end.
   It holds the claims that were valid at some instant at or after T:
   the claims
   closed during a read whose T is its start, and instants recorded
   after T. The route sorts them by (key, store order, id).
2. **The open bucket:** for each store, the index's (namespace,
   relation, current, open) prefix in index order. A key is skipped by
   `%key-after-p` on the entry's stored key, before its node is
   resolved, as #451 does. Resolve only the entries that are read.
3. **Merge** the two by key. Within a key, emit today's order: the own
   store's claims in id order, then each foreign store's in scope
   order. That is the order the subject-relation index gives today,
   since an index entry ends in its node's id. Every candidate passes
   through the existing `emit` (key bounds, `%exclude-producers`,
   `%unify-claim`).
4. This run's own derivation (`%derived-all`) follows at the end,
   unfiltered, as it does today.

The route never runs inside a transaction, so it needs no overlay merge.
That also avoids a second ordering problem: `index-lookup` puts a
transaction's own claims after the committed ones within a key, while
`map-index` merges them in by key and id. An entry's node is checked
with `%index-hit-p` as `map-index` checks it.

A claim that is open but starts after T is read and then rejected by
the filter. That costs one candidate per claim written after T, which
is small for a recent T.

### 3. The rewrite

A clause-body rewriter, `%fuse-validity`, registered so that it runs
after `%fuse-key-bounds` and `%fuse-touched-cursor`. Rewriters apply in
list order, and `pushnew` puts the newest first, so the registration
point matters; a test pins the order.

It fuses a claim goal and its two filters:

```
(claim ?c F S ?k R O OK) [kept > goals] G1 G2
  => (claim-current-valid ?c F S ?k R O OK nil nil T) [kept >] G1 G2
(claim-after ?c F S ?k R O OK SA OA) [kept > goals] G1 G2
  => (claim-current-valid ?c F S ?k R O OK SA OA T) [kept >] G1 G2
```

All of these must hold:

- The claim goal is `claim/7` or `claim-after/9`, recognised as
  `%engine-functor-goal-p` recognises it.
- `G1 G2` are `(claim-current ?c)` and `(claim-valid-at ?c T)`, in
  either order, recognised the same way.
- They come right after the claim goal, or right after the `>` goals
  that `%fuse-key-bounds` kept there.
- `?c` is a named variable that occurs in no earlier goal of the body,
  so the claim goal generates it.
- `T` is a string literal or a variable.
- A variable `T` occurs in no argument of the claim goal. The fused
  goal reads `T` when it is called, so a `T` the claim goal binds would
  be free then and fail every claim.
- The goals are top-level goals of the body, the only list a rewriter is
  offered.

The two filter goals stay in the body, as the comparisons do under
#451, so correctness never depends on the route. The rewriter returns
its input `eq` when nothing fuses.

**Rule bodies:** handle it exactly as `claim-after/9` is handled.
`claim-current-valid/10` is `:withheld` in rules/standing.lisp, and
`compile-rule` refuses it hand-written in a premise, with a message
that says to write the three goals. The rewrite still applies where
#451's does.

`explain-goals` gets a row for a hand-written `claim-current-valid/10`:
"validity index from the key bound, or claim-after's route; decided at
run time". It cannot know the cap or the snapshot state without running
the query. `explain-goals` doesn't apply the rewriters
(prolog-explain.lisp), so the three-goal text keeps explaining as the
vocabulary route, as #392's text does (`docs/rules.md` says so for the
touched-at row). `docs/rules.md` gets the row and that note.

### 4. Budget and timeout

The functor calls `%tick` once for each candidate claim it resolves, on
every route, the fallback included, whether or not the claim then
passes its two filters. So a budget or deadline still preempts between
claims. It is not once per row emitted: the functor filters inside
Lisp, so a run of rejected claims would otherwise be work no bound can
stop, against GH #285's rule ("the work between two `%TICK`s is one
key's claims").

A claim the index skips is never resolved, so it costs no inference. A
skipped key in the open bucket is one string comparison on the entry's
stored key, as under #451. The closed set costs one index step per
entry, bounded by the cap; an entry is resolved, and ticks, only when
it is a candidate for the page. On the fallback a closed claim costs
one inference, against about four today.

### 5. The race

The route reads the index's newest state, with or without a snapshot.
(This section first said "the board's reads take no snapshot", which is
wrong: guarded reads take one, §2 "Under a snapshot".) A claim can
close while a page is being read, after the closed set was collected
and before the walk of the open bucket reaches it. Its open entry is
then gone, and its new entry is not in the collected set, so the page
leaves it out. Under a snapshot, the claim stays out even if the walk
meets its new entry: the snapshot resolves the open version, which
`%index-hit-p` rejects. Today's route returns it, because its
subject-relation entry never moves and its new end is still at or
after T.

So a claim whose validity closes during the page that would hold it may
be missing from that page. The route never returns a wrong row, and a
later read at a later T is right. Pages are already not atomic across
their run: a claim written mid-read at a key before the bound is missed
by every later page. **Accepted (Kevin, 2026-11-24), documented under
"Paging".** The alternatives were holding the claim class's read lock
for a page, which makes every writer wait on readers, and re-checking
before each key for claims closed since the collection, which narrows
the race to closes written with a backdated end. The second costs code,
and work on every key while a writer is busy.

### 6. Measured before the pin

On the existing 2.1 GB copy of the largest consumer store, taken at an
earlier pin (2026-11-03), record the figures below (Kevin, 2026-11-24).
Copy it to scratch first, because opening a store writes to it. Scale
the figures to the live store's 4.5 GB; the consumer's pin issue names
the paths. Record:

- how long the index build takes at its first open, and how much disk
  it uses;
- a write bench (tests/perf) with and without the index: claims written
  per second, and the cost of a validity close.

This is reported, not gated. It goes in the CHANGELOG entry and the
consumer's pin issue.

### 7. Documentation

- `docs/rules.md`: the route table row, "Solution order" (unchanged,
  said so), the fused shape and its conditions, and the race.
- `docs/guarded-query.md` "Paging": a page of claims valid at T costs
  what it returns when the three goals sit together. A free T, an
  `:as-of` below the latest epoch, a snapshot a commit has passed and a
  far-back T keep today's cost.
- `docs/vivace-graph-v3-doc.org`: the claim chapter's list of indexes.
- `CHANGELOG.md` `[Unreleased]`: under Added, the index, the functor
  and the rewrite; under Changed, the page cost and the write cost (§6).
- The docstrings of `claim/7` and `claim-valid-at/2` point at
  `claim-current-valid/10`.

## Acceptance

- **The issue's table.** One relation with N claims whose validity is
  closed (each closed by a later unary claim of the same producer, as an
  absence closes it), sorted ahead of 600 valid ones. At N = 3,000 and
  N = 30,000, the inferences one 50-row page uses stay within 1.5× the
  N = 0 baseline. The same page with a `true` goal between the claim
  goal and its filters grows with N. Under mutation, removing the route
  (always fall back) makes the fused page grow too.
- **Nothing closed is resolved.** A counting hook on node resolution in
  the route (notinline, as `%scope-lookup` is for #451) shows that the
  closed claims ahead of the page are never resolved.
- **Equivalence.** Fused and unfused queries answer the same rows in
  the same order over a fixture with:
  - open claims, claims closed before T, claims closed after T, and
    instant claims at T, before T and after T;
  - retracted open and retracted closed claims;
  - several claims and producers per key;
  - two stores in scope, and a derivation in the run;
  - T before everything, between, after everything, exactly at a close
    end, and inside one second of a close end;
  - a subject key bound and an object key bound;
  - `:offset` paging inside one key;
  - T given through a variable bound earlier.

  The fixture also holds unstamped claims (no transaction extent). The
  fallbacks are each shown to answer the same: a free T, `:as-of`, a
  snapshot, a read-write transaction (including one where another
  commit closed a claim after it began), and a closed set over the cap.
- **Under a snapshot** (§2, revised 2026-11-24).
  - The route runs under a snapshot that no commit has passed: a
    `:snapshot t` select, a guarded query, and an `:as-of` read at
    `latest-epoch`.
  - It falls back under a snapshot that a commit has passed, and under
    an `:as-of` below the latest epoch.
  - A claim closed by a commit after the snapshot began is still
    returned under that snapshot. Under mutation, dropping the epoch
    check loses that claim.
  - The guarded valid-at page runs the route. Its resolutions are
    counted, so the test fails when the route falls back.
- **The index.** A close moves the entry, a retraction moves it, and a
  re-assertion moves it back. An index rebuilt at open
  (`regenerate-secondary-indexes`) equals the maintained one, entry for
  entry.
- **The rewriter.** It fuses: both filter orders, behind `claim/7`, and
  behind `claim-after/9` with its kept comparisons. It leaves alone:
  only one filter, a goal between, a different `?c`, a `?c` bound
  earlier, a numeric T, and a body with no claim goal. It returns `eq`
  when nothing fuses. Rewriter order is pinned.
- **Rules.** `claim-current-valid/10` is refused hand-written in a rule
  premise and withheld from text.
- **Perf.** A bench in `tests/perf/`: page cost against the size of the
  history ahead, and the write cost of §6. Reported, not gated.
- Every suite the branch touches is green locally, and the push CI on
  `experiment` is green before the unit counts as landed.

## Calls I made

- **Retracted claims are skipped too.** They take the same index and
  the same route, because `claim-current` is in the shape. Cost if
  wrong: none to answers; a query without `claim-current` keeps today's
  route.
- **The closed set cap is 10,000 entries.** Above it the route falls
  back to today's, so a far-back T costs what it costs today. Cost if
  wrong: a mid-range T falls back too often (raise it) or a page sorts
  too much (lower it).
- **The index is declared on every claim family**, not opted into.
  Cost: one more entry on every claim write in every store, measured in
  §6.
- **The open bucket is walked from its start with `%key-after-p`, not
  sought to the bound.** That is #451's equivalence by construction.
  Cost: one string comparison per open key before the bound, no inference
  and no node read. A seek is a later optimisation if the index's
  string collation is shown to agree with `>/2`.
- **The functor's name and arity.** `claim-current-valid/10` names the
  two goals it fuses and carries #451's two key bounds plus T.
- **Fall back inside any read-write transaction, and under a snapshot a
  commit has passed.** This first said "under any snapshot". That missed
  that every guarded query reads under one, so the route never ran for
  them (measured, ratio 286). Kevin chose the epoch check on 2026-11-24
  (§2 "Under a snapshot"). The other options were:
  - Exact under any snapshot, merging the claims written since the
    snapshot from the touched-at index. That is more code, and each page
    pays for the writes made during it.
  - The route only for reads without a snapshot. That changes the
    consumers' read contract.

  Cost: a query inside a read-write transaction, an older outer
  snapshot such as a rule run after a commit, and a commit landing
  between a guarded query's snapshot and its route all keep today's
  route.
- **One inference per candidate resolved, on every route** (§4). Cost:
  the fused fallback's inference count differs from today's, lower per
  closed claim.
- **Unstamped claims are read from the null-currency bucket.** That is
  four ranges per store instead of two. The alternatives were declaring
  them out of scope, or changing core so canonicalizers see NIL.
- **`explain-goals` reports the fused route only for hand-written goal
  text, following #392.** Making it apply the rewriters would change
  #451's and #392's rows too, so that is its own issue if wanted.
- **T is read once, when the fused goal is called** (found in Task 3's
  review). So the rewrite skips a `T` that the claim goal binds (§3).
  A malformed T string is refused even when no candidate exists, where
  the three goals answer an empty page. Cost: only text that names a
  bad instant changes, and GH #388 already wants that refused.

## Left to the builder

- The canonical values for currency, open and no-extent, and where they
  sort, as long as the two read ranges in §2 are what they say.
- How the route shares code with `%claim-goal`.
- The node-resolution counting hook.
- Fixture shapes and bench labels, within the perf README's rules.
- Test names, grepped for first (CLAUDE.md engine traps).
