# Restore across a shadow swap (#171) — design

Unit 6 of the namespaces epic (#110). Implements
`2026-08-20-namespaces-design.md` §9 (decision D13) against what #170
actually built. Public repo: names and examples are domain-neutral.

## 1. What exists, and the one thing that does not

- `swap-in-shadow` (#170) renames the live generation to
  `<location>-retired-<E3>` and appends `(:kind :swap :store NAME
  :retired PATH :epoch E3)`. Nothing ever deletes a retired generation,
  so retention today is infinite and unmanaged.
- The clock journal (`journal-records`) is the lifecycle history, read
  with `*read-eval*` nil. A `:swap` record can be missing after a
  post-rename failure (#212); the retired directory's name still
  carries `E3`.
- `store-recovery-policy` reads `policy.dat`: `:authored` (default) or
  `:derivable`.
- A crash between the two renames leaves the live data only at the
  retired path (#170 docstring: "#171's territory").
- **There is no physical point-in-time rewind inside a generation.** An
  on-disk graph discards each `.txn` once applied
  (`retain-committed-transaction-p` → NIL); `snapshot`/`replay` is
  logical and restores *a snapshot*, not *T*, into an empty graph. The
  spec's "rewind physically to T" has no primitive to stand on.

## 2. Rulings

These decide the issue's open question and the gaps §9 leaves.

**R1 — Restore resolves at generation granularity; shadow swap does
not supersede logical replay.** The issue's open item is answered *no*:
the swap is the *generation* mechanism, `snapshot`/`replay` stays the
*content* mechanism inside a generation, and true point-in-time
recovery inside a generation is a separate feature (filed as a
follow-up, not built here). Restore-to-T therefore means: for every
store, put in place the generation that was live at `T`, and state
exactly which epoch that generation's content reflects.
*Cost if wrong:* a later PITR unit adds a `:rewind` step after
generation selection; nothing here blocks it, because the manifest
already records requested-vs-actual epoch.

**R2 — "Rewound to T" is exact when the retained generation has no
writes after T, and is otherwise reported, never faked.** A retained
generation was frozen at its last commit `E0 ≤ E3`. If `E0 ≤ T`, its
content at `T` *is* its content at `E0` — exact, and this covers the
spec's detach-window case `E1 ≤ T < E2` directly. If `T < E0`, writes in
`(T, E0]` cannot be undone; the manifest marks the store
`:exact nil :state-at E0`. Default behaviour is to proceed (the manifest
makes it non-silent, which is what §9 forbids); `:require-exact t`
refuses instead. *Cost if wrong:* an operator who wanted refusal by
default gets a manifest line to read; the flag is one keyword away.

**R3 — Retention is enforced at prune time, not swap time, because
swap never deletes.** New `prune-retired-generations (clock floor)`
deletes retired generations with `E3 ≤ FLOOR`. For `E3 > FLOOR` an
`:authored` store's generation is refused by name
(`retention-required-error`, listing every blocked generation); a
`:derivable` one is kept unless `:discard-derivable t`. `FLOOR` is the
operator's restore-window floor in epochs — epochs are the system's
time axis (§6), and no wall-clock is persisted in the journal. Every
deletion appends `(:kind :retire :store NAME :retired PATH :swap-epoch
E3)`. *Cost if wrong:* a window expressed in epochs is less intuitive
than days; an operator can map it with the journal's `:epoch` stamps.

**R4 — The filesystem is the source of truth for generations; the
journal annotates it.** `retired-generations (clock)` lists
`<location>-retired-<E3>` directories for every registered store and
joins them to `:swap` records. A directory with no record (the #212
shape) is kept, reported with `:journaled nil`, and warned about
(`swap-record-missing-warning`) — the same tolerance the torn-tail
reader chose in #191. A record with no directory is reported as
`:present nil` — *lost*, since a deliberate prune leaves a `:retire`
record and `retired-generations` omits such generations entirely (Task 2
ruling: pruned is bookkeeping, lost is a finding). *Cost if wrong:* none foreseeable;
this strictly adds information.

**R5 — Rebuild is a caller-supplied function; cascade is found by
store tag.** The engine cannot regenerate a derivable store; the
operator passes `:rebuild (lambda (name graph) ...)` which populates a
fresh, empty graph the engine created at the live location. After a
rebuild the store's node ids are new, so every edge in *any other open
store* whose endpoint carries the rebuilt store's 12-bit tag
(`id-store-tag`, #169) now dangles. Cascade: a dependent `:derivable`
store is rebuilt too (fixpoint over the set); a dependent `:authored`
store is **not** touched — §9 says authored cross-boundary assertions
key on external identity — and the manifest records its dangling-edge
count under `:dangling`. *Cost if wrong:* a scan of every open store's
edges per rebuild; bounded by the rebuild itself, which is already a
full load.

**R2a (fix round 3) — A generation's content era is not always its own
last live window.** Selection of "the generation live at T" runs over
each generation's ERAS: a list of half-open `[from, to)` intervals made
of its own live window plus, when a `:restore` record promoted a
directory into the window this generation later closed, that directory's
eras as well — recursively through chains of restores. Ties go to the
matching era with the latest `from`. Without inheritance the sequence
swap (retires r1) → restore-to-T (promotes r1, retires r2) → swap
(retires the promoted directory as r3) leaves T's content in r3 while
the plan for T reports `:unchanged` — the generation is still on disk
but unreachable. Retiring also consumes epochs until the
`<location>-retired-<E>` name is free, so two retiring events with no
commit between them (two restores in a row, or a rewind the cascade then
rebuilds) do not collide. *Cost if wrong:* eras are derived from the
journal on every read, so a corrected rule needs no on-disk migration.

**R6 — The interrupted-swap window is repaired by an explicit,
idempotent tool, not inside restore.** `repair-interrupted-swap (clock
name location)` handles the only shape #170 cannot: live missing (or a
half-moved shadow) while exactly one `<location>-retired-<E3>` is newer
than any `:swap` record. It renames the retired generation back and
appends `(:kind :swap-aborted ...)`. Restore refuses to start while a
registered store is in that state, naming the tool. *Cost if wrong:* an
operator runs one extra command.

## 3. Operations

All live in a new `system-restore.lisp` (after `shadow-store.lisp` in
the `.asd`), exported from `package.lisp`.

```
(retired-generations clock)  → list of plists
  (:store NAME :location LIVE :retired PATH :swap-epoch E3
   :journaled BOOL :present BOOL :policy :authored|:derivable)

(prune-retired-generations clock floor &key discard-derivable dry-run)
  → list of plists actually (or, dry-run, would-be) deleted; signals
  retention-required-error listing blocked :authored generations.

(plan-system-restore clock T &key require-exact rebuild)
  → manifest (see §4) with no side effects. Signals
  restore-refused-error (authored generation gone; inexact under
  :require-exact; interrupted swap present; :derivable store with no
  retained generation and no :rebuild).

(restore-system clock T &key require-exact rebuild (timeout 60))
  → manifest, after executing the plan:
    per affected store, in dependency order:
      quiesce + close the live graph (same sequence as swap-in-shadow)
      rename live → <location>-retired-<Enow>   (:kind :retire-live)
      rename <location>-retired-<E3> → live     (:kind :restore ...)
      reopen + attach-to-system-clock
    per rebuilt store: make-graph at live, call REBUILD, then cascade.
    The manifest is also written to <clock-dir>/restore-<Enow>.manifest
    readably, *read-eval* nil on read — same discipline as the journal.

(repair-interrupted-swap clock name location) → :repaired | :nothing-to-do
```

`restore-system` uses `plan-system-restore` first and executes only a
plan that raised nothing: every refusal fires before any rename.
Failure after the first rename of a store follows the `swap-in-shadow`
pattern exactly: the store's two renames are the commit point, a
failure before them restores the old name and resignals, a failure
after them reopens the new generation and warns.

Epoch `T` is an integer epoch from the system clock. Stores not
affected by any swap with `E3 > T` are untouched and appear in the
manifest as `:action :unchanged`.

## 4. The manifest

One plist per store, plus a header; printed readably, no evaluation:

```
(:restore t :requested T :at Enow :clock LOCATION
 :stores
 ((:store "shipping" :action :rewound  :state-at E0 :exact t
   :from "<loc>-retired-1234" :retired-live "<loc>-retired-5678")
  (:store "orders"   :action :rebuilt  :state-at Enow :exact nil)
  (:store "catalog"  :action :rebuilt  :state-at Enow :exact nil
   :cascade-from "orders")
  (:store "audit"    :action :unchanged :dangling 17)
  ...))
```

`:action` ∈ `:rewound | :rebuilt | :unchanged | :refused` — but
`plan-system-restore` raises `restore-refused-error` as soon as any
refusal exists, so a caller never actually receives a manifest
containing `:refused`; the reasons instead ride on the error's own
`reasons` slot. `:dangling` appears on an `:authored` dependent of a
rebuilt store. A manifest in which any store is `:rebuilt` or
`:exact nil` is exactly the "inconsistent instant" §9 wants *recorded*;
`restore-system` also signals `restore-inexact-warning` summarising
those lines so a caller cannot miss it.

## 5. Out of scope (filed as follow-ups, not built)

- Point-in-time rewind inside a generation (needs retained `.txn` +
  snapshot base; R1).
- Out-of-process rebuild; retention by wall-clock.
- #212's double-open on attach failure (unchanged by this unit).

## 6. Testing

FiveAM, `tests/system-restore-tests.lisp`, registered after
`detach-tests`. All under a temporary system directory with its own
clock; every guard ablated:

- retired-generations: joins dir+record; `:journaled nil` on a dir with
  no record (simulate #212 by deleting the record's line); `:present
  nil` on a record with no dir.
- prune: deletes `E3 ≤ floor`; refuses authored `E3 > floor` naming it;
  deletes derivable only with `:discard-derivable`; `:dry-run` deletes
  nothing; appends `:retire`.
- plan/restore, exact path: swap at `E3 > T` with `E0 ≤ T` → `:rewound
  :exact t`; data read back equals the pre-swap content; journal has
  `:retire-live` + `:restore`; the post-swap generation is retained and
  a second restore to a later T puts it back (round trip).
- inexact: write after T, then swap → `:exact nil :state-at E0`;
  `:require-exact t` refuses before any rename (directory layout
  unchanged, graph still open and accepting).
- authored generation pruned → `restore-refused-error`, nothing moved.
- derivable, no generation, `:rebuild` → `:rebuilt`; cascade rebuilds a
  derivable dependent whose edges pointed at the rebuilt store, and
  reports `:dangling N` on an authored dependent; dependency order.
- detach-window case `E1 ≤ T < E2` → generation restored, `:state-at
  E0`, exact.
- interrupted swap: construct the between-renames layout by hand;
  restore refuses naming `repair-interrupted-swap`; the tool repairs,
  is idempotent, and appends `:swap-aborted`.
- manifest file written; reads back with `*read-eval*` nil.
- Full suite green on SBCL (ECL demoted).

## 7. Docs

`docs/vivace-graph-v3-doc.org` ch.17: new "Restore across a swap"
section after "Detach, shadow load, and the swap", and ch.11 gains a
pointer; `CHANGELOG.md` Unreleased `### Added`; the namespaces spec §9
gets a **Built (#171)** note naming R1–R6.

**Built (#171):** all five operations shipped as designed in §3, with
one shape the design didn't anticipate: a generation's content is
tracked by ERAS (a list of `[from, to)` intervals, R2a), not a single
live-window interval, because a restore can promote a retired
directory back to live and a later swap can retire it again under a
new name — without inheritance that directory's original content era
would be attributed to nobody. Two defects surfaced only by
implementation, not foreseeable from the design: retired-generation
name collisions (`%retired-path-for` now consumes epochs until an
unused `<location>-retired-<E>` name is free — two retiring events
with no commit between them, e.g. two restores in a row, otherwise
compute the same name), and a `%reopen-and-resume` bug where a
slashless `location` string misplaced sidecar files in the parent
directory and froze the transaction-id watermark (worked around here,
filed as the real defect against `open-graph`/`make-graph` at #222).
`%known-locations`'s stranded-swap detection also widened past the
design's two sources (journal `:swap`/`:retire-live` records, open
`*graphs*`) to a third — an `:attach` record's own `:location` field —
since that is the only trace left when a crash lands between
`swap-in-shadow`'s two renames before any `:swap` record exists.
