# Detach Quiescence and Shadow Bulk Load (#170) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A store can be taken out of service without stopping the system
(detach = refuse + drain + close + journal, over the existing pin
machinery); a bulk load builds a shadow generation and swaps it in, so the
store is offline only for two brief windows (the consistent copy and the
swap), a killed loader leaves the live generation byte-identical, a
derivable store may load without WAL, and a load that would exceed
vector-segment capacity fails before writing anything.

**Architecture:** Six layers. (1) **Quiescence**: transaction-manager
gains an `accepting-p` flag checked at the two choke points —
`create-transaction` (via `call-with-transaction`) and `pin-read-epoch` —
plus `%quiesce-transaction-manager` which flips it and waits for
`reap-safe-floor` to report nothing active. (2) **Detach**:
`detach-store` quiesces, leases an epoch range from the image clock
(`clock-lease-epochs`), journals `:detach` with the range, closes the
graph, and returns a `store-detachment` handle. `reattach-store` reopens
and re-attaches to the clock (which journals `:attach`). #169's machinery
makes the closed store honestly `:detached` to every reader for free.
(3) **Shadow**: `shadow-store` makes a consistent copy — v1 consistency
window is quiesce → `close-graph` → file copy → `open-graph` + clock
reattach, seconds of unavailability, exactly the "copies in seconds"
budget — and leaves the reopened live store in a **read-only window**
(ruled by Kevin, 2026-08-22: the spec promises "readers continue" and
says nothing about writers, and a write accepted against the doomed
generation would be silently discarded at swap — so `accepting-p` goes
`:read-only`: pins and reads flow, new write transactions signal
`store-not-accepting-error` with reason `:shadow-load`, until
`swap-in-shadow` replaces the generation or `abandon-shadow` restores
full service). Then `open-shadow-graph` opens the copy WITHOUT
registering it
(`*graphs*`, open-store vector, and `.dirty`-independent: it is a private
graph only the loader holds), with the SAME graph-name (schema metadata
applies) and the same store-id set directly (so minted v8 ids carry the
live store's tag), transaction ids drawn from the lease, and the lease
persisted as `lease.dat` in the shadow directory (the out-of-process
survival requirement from the #170/#182 comment). `discard-shadow`
deletes the shadow directory. (4) **Swap**: `swap-in-shadow` quiesces and
closes the live store, renames live → `<name>.retired-<epoch>`, shadow →
live, journals `:swap`, reopens and reattaches. (5) **Fast path**: a
persisted per-store `recovery-policy` (`policy.dat`; absent = `:authored`)
licenses `:fast-load t` on `open-shadow-graph`, which suppresses WAL
persistence (`persist-transaction` becomes a no-op on that graph) — sound
for a shadow because a crashed load is discarded and redone, and gated on
`:derivable` because only a derivable store's source material is
guaranteed re-runnable. (6) **Presize**: `presize-vector-segment` grows a
segment to capacity ≥ N upfront under its write lock, so allocation
failure precedes any node write; `open-shadow-graph :expected-vectors N`
presizes.

Out of scope, stated: out-of-process detach (the lease + `lease.dat` +
journal records accommodate it later, per the issue's #182 comment);
cross-store traversal continuation; #171's restore/cascade semantics.

**Tech Stack:** Common Lisp (SBCL primary), FiveAM, uiop for file copy.

**Spec:** `docs/superpowers/specs/2026-08-20-namespaces-design.md` §8
(D12), GH #170 including its #182-constraint comment, and the acceptance
list in the issue.

## Global Constraints

- Lisp: spaces only, never tabs; hard 80-column limit (96 is a defect).
  Comments terse: invariant + `(GH #170)`.
- Every SBCL run: from the worktree, FIRST
  `--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (truename ".")) :inherit-configuration))'`,
  and `--dynamic-space-size 16384`.
- **The merge gate is `(asdf:test-system :graph-db)`, controller-run.**
  Implementers run focused suites only and report expected gate deltas.
  Branch baseline (post-#210, 11afe7d): 4073 checks / 4063 pass /
  10 skip / 0 fail.
- Test-form conventions that are LAW in this codebase (every prior unit
  tripped on one): `with-transaction` takes the transaction manager —
  `(with-transaction ((transaction-manager g)) ...)`; `:generic`
  (type-0) edges are invisible to typed adjacency scans and `traverse`
  needs `:edge-type`; tests binding `*system-directory*` must also
  rebind `graph-db::*store-registry*` (and now `*type-registry*` where
  types are minted) to nil per test; `make-vertex` is
  `(make-vertex type data &key graph id ...)`, `make-edge` is
  `(make-edge type from to weight data &key graph id ...)`.
- Ablate every guard; each test names its nearest wrong implementation.
- ECL demoted: verify on SBCL only, say so.
- Host safety: live `ma-dev-server` on this host — NEVER
  `pkill`/`killall`/`pgrep`; kill only a PID you started; one SBCL at a
  time; never touch `/data0`. Never end a turn waiting on a background
  run — bounded foreground stretches
  (`timeout 570 bash -c 'while kill -0 <PID> 2>/dev/null; do sleep 15; done'`).
- Worktree `.worktrees/170-detach-shadow`, branch `170-detach-shadow` off
  `experiment` (11afe7d). PR against `experiment`. Pushing explicit-only.
- PUBLIC repo: domain-neutral text everywhere.

## Contract-first note to implementers

Unlike earlier plans, several steps here say "read X and adapt" instead
of verbatim code: this unit hooks deep machinery (transaction creation,
txn-id allocation, open/close internals) where every prior unit's
verbatim code needed correction against the real source. The CONTRACTS
(names, signatures, semantics, tests) are fixed; the splice points you
verify against the code. When the code contradicts a contract, report
NEEDS_CONTEXT or flag a substantive deviation — do not silently improvise
a different interface.

---

### Task 1: Quiescence, detach, reattach

**Files:**
- Modify: `transactions.lisp` (transaction-manager `accepting-p` slot;
  refusal checks in `create-transaction` and `pin-read-epoch`;
  `%quiesce-transaction-manager`; `detach-store`; `reattach-store`;
  conditions)
- Modify: `package.lisp` (exports)
- Create: `tests/detach-tests.lisp` (+ .asd entry after
  `store-resolver-tests`)

**Interfaces (produced; Tasks 2–4 consume):**

```lisp
(define-condition store-not-accepting-error (error) (name reason))
;; reason :detaching | :swapping | :shadow-load; readers
;; store-not-accepting-name / store-not-accepting-reason.  Signaled by
;; NEW transactions on any non-T accepting state, and by NEW read pins
;; only under FULL quiesce (:detaching/:swapping) -- :read-only
;; (reason :shadow-load) refuses transactions but ADMITS pins/reads.
;; ACCEPTING-P states: T (all), :read-only (pins yes, txns no),
;; :detaching/:swapping (nothing new).

(define-condition detach-drain-timeout (error) (name seconds))
;; readers detach-timeout-name / detach-timeout-seconds.  Quiesce wave
;; failed to drain in time; ACCEPTING-P is restored to T before this
;; is signaled (the store resumes service -- a failed detach must not
;; strand it half-dead).

(defstruct store-detachment
  graph-name location store-id lease-start lease-end)

(defun %quiesce-transaction-manager (tm reason timeout)
  ;; Flip ACCEPTING-P to REASON (a keyword; NIL/T semantics: T =
  ;; accepting).  Wait in short sleeps until REAP-SAFE-FLOOR is NIL
  ;; (no active txns, no read pins) or TIMEOUT seconds pass.  On
  ;; timeout: restore ACCEPTING-P to T and signal DETACH-DRAIN-TIMEOUT.
  ;; On success return T with ACCEPTING-P left at REASON.
  )

(defun detach-store (graph &key (lease-epochs 1000000) (timeout 60))
  ;; Requires (graph-system-clock graph) non-nil -- detach without an
  ;; image clock has no lease to hand over; signal a plain error naming
  ;; the requirement.  Steps: quiesce (:detaching, TIMEOUT);
  ;; CLOCK-LEASE-EPOCHS for LEASE-EPOCHS; JOURNAL-APPEND :detach with
  ;; :store name :lease-start s :lease-end e; CLOSE-GRAPH (snapshot-p
  ;; t default); return the STORE-DETACHMENT handle.
  )

(defun reattach-store (detachment &key (buffer-pool-size nil bps-p))
  ;; OPEN-GRAPH at the handle's location/name (pass buffer-pool-size
  ;; through when supplied), ATTACH-TO-SYSTEM-CLOCK to the image clock
  ;; (which journals :attach), return the new graph.
  )
```

- [ ] **Step 1: Write the failing tests** (`tests/detach-tests.lisp`,
  suite `detach-suite :in graph-db-suite`). Fixture: adapt
  `with-two-stores` from `tests/store-resolver-tests.lisp` — but detach
  needs the image CLOCK too: build `with-clocked-store` opening ONE
  disk store under a temp system directory with
  `open-system-clock` + `attach-to-system-clock` (read
  `tests/system-clock-tests.lisp`'s `two-stores-on-one-clock-...` for
  the exact wiring and reuse its idiom), rebinding `*system-directory*`,
  `*store-registry*` and closing clock+graph in the unwind. Tests:

```lisp
(test detach-refuses-new-transactions-and-pins
  "During and after quiesce, a NEW transaction and a NEW read pin both
signal STORE-NOT-ACCEPTING-ERROR.  Nearest wrong implementation: close
without refusing -- the segfault-shaped hazard the spec names."
  ;; open clocked store; write one vertex; DETACH-STORE.
  ;; After detach the graph object is closed; assert:
  ;;   (signals store-not-accepting-error
  ;;     (with-transaction ((transaction-manager g)) ...))
  ;;   (signals store-not-accepting-error
  ;;     (pin-read-epoch (transaction-manager g)))
  )

(test detach-drains-in-flight-readers-first
  "A pinned reader taken BEFORE detach holds the drain; detach completes
only after the pin is released.  Nearest wrong implementation: skip the
drain (close underneath the reader).  Mechanism: take a pin, start
DETACH-STORE in a second thread (bordeaux-threads, like the
concurrency tests), assert it has NOT completed while the pin is held
(sleep + flag), release the pin, join, assert detached."
  )

(test detach-timeout-restores-service
  "A drain that cannot complete times out, signals
DETACH-DRAIN-TIMEOUT, and the store ACCEPTS transactions again --
a failed detach must not strand the store half-dead.  Hold a pin,
call DETACH-STORE :timeout 1, expect the condition, then a
with-transaction write must succeed."
  )

(test detach-journals-and-leases
  "The clock journal records :detach with the lease range; the clock's
next epoch is past LEASE-END (CLOCK-LEASE-EPOCHS semantics).  Read
JOURNAL-RECORDS, find the :detach record, assert :lease-start/-end
present and (>= (clock-current-epoch clock) lease-end)."
  )

(test detached-store-is-detached-to-the-resolver
  "#169 integration: after DETACH-STORE, RESOLVE-NODE-GRAPH on a node
id minted there reports :DETACHED and LOOKUP-VERTEX-ANYWHERE returns
the marker."
  )

(test reattach-restores-service-and-journals
  "REATTACH-STORE reopens, the same node reads back, a new write
succeeds, and the journal carries the :attach record after the
:detach one."
  )
```

Write real assertions; the sketches above fix the scenario and the
condition names, not the exact forms.

- [ ] **Step 2: RED** (skeleton-first allowed as in every prior unit;
  behavioral RED recorded per test).

- [ ] **Step 3: Implement.** Splice points, verified this session:
  - `call-with-transaction` (transactions.lisp:2863) calls
    `create-transaction` — put the accepting check inside
    `create-transaction` under the tm lock (or at its entry), signaling
    `store-not-accepting-error` with the tm's graph name. Find where
    `create-transaction` is defined and whether recovery/replication
    paths also call it — the check must NOT break
    recovery-transaction replay on open (those run before detach could
    ever have flipped the flag, but confirm and say so).
  - `pin-read-epoch` (transactions.lisp:652): check `accepting-p`
    before registering; `with-read-pin` (graph-class.lisp:244) then
    propagates the signal to `map-vertices`/`map-edges` for free.
  - `accepting-p` initform `t`; readers treat any non-`t` value as the
    refusal reason keyword.
  - Drain condition: `reap-safe-floor` nil under the tm lock — but note
    `minimum-start-transaction-id` semantics (read it): confirm nil
    means "no active transactions".
  - `clock-lease-epochs` is system-clock.lisp:289; `journal-append`
    kinds already include `:detach`/`:attach`/`:swap`/`:retire`.
  - `attach-to-system-clock` (transactions.lisp ~2990) already refuses
    with active transactions and journals `:attach` — `reattach-store`
    composes `open-graph` + it; do not reimplement.

- [ ] **Step 4: Focused suite green; count checks; also run
  `system-clock-suite` (journal interplay) and one concurrency-adjacent
  canary you judge cheapest.**

- [ ] **Step 5: Ablations — run (a) and (b).** (a) Remove the
  `pin-read-epoch` check: `detach-refuses-...` fails its pin half.
  (b) Make `%quiesce-transaction-manager` return immediately (no
  drain): `detach-drains-in-flight-readers-first` fails. Revert, green.

- [ ] **Step 6: Commit**
  `feat(detach): quiescence protocol over the pin machinery (#170)`

---

### Task 2: Shadow generation

**Files:**
- Create: `shadow-store.lisp` (+ .asd entry — needs `graph`,
  `transactions`; place near `backup` in the chain and declare
  `:depends-on` accordingly, reporting the position)
- Modify: `graph-class.lisp` (graph slot `shadow-p`, initform nil;
  graph slot `epoch-lease`, initform nil — a `(next . end)` cons or
  small struct), `transactions.lisp` (txn-id allocation consults the
  lease first — find the allocation point: the function that yields the
  next transaction id when a system clock is attached vs the per-store
  counter; add the lease branch BEFORE both, erroring past `end` with a
  named condition `epoch-lease-exhausted`), `graph.lisp`/`open-graph`
  (a `:shadow-p` keyword that skips `*graphs*` registration,
  `%register-open-store`, and replication start), `package.lisp`
- Modify: `tests/detach-tests.lisp` (append)

**Interfaces (produced):**

```lisp
(defun shadow-store (graph &key (timeout 60))
  ;; Consistent copy of GRAPH's directory: quiesce (:swapping, TIMEOUT)
  ;; -> CLOSE-GRAPH -> copy directory tree to "<location>-shadow/"
  ;; (uiop:collect-sub*directories / copy-file loop; NO shell-outs) ->
  ;; reopen via OPEN-GRAPH + ATTACH-TO-SYSTEM-CLOCK (service resumes)
  ;; -> set the reopened graph's ACCEPTING-P to :READ-ONLY (reason
  ;; :shadow-load: reads and pins flow, new writes signal -- Kevin's
  ;; ruling; no write is ever silently discarded at swap)
  ;; -> return (values shadow-location reopened-graph).  The live store
  ;; is fully unavailable only for close+copy+reopen -- seconds
  ;; (spec sec.8) -- and write-unavailable until swap or abandon.
  )

(defun abandon-shadow (graph shadow-location)
  ;; The lifecycle exit that is not a swap: DISCARD-SHADOW the
  ;; directory, restore GRAPH's ACCEPTING-P to T (full service).
  )

(defun open-shadow-graph (shadow-location graph-name
                          &key lease fast-load expected-vectors
                               (buffer-pool-size nil))
  ;; OPEN-GRAPH :shadow-p t -- unregistered (not in *graphs*, not in
  ;; the open-store vector, no replication), SAME graph-name so schema
  ;; metadata instantiates, store-id set directly from
  ;; STORE-REGISTRY-INTERN so minted v8 ids carry the LIVE store's tag.
  ;; LEASE is (start . end) from the STORE-DETACHMENT (or DETACH-STORE
  ;; was never called and the caller passes one from
  ;; CLOCK-LEASE-EPOCHS); persist it as lease.dat in the shadow dir
  ;; ((:lease-start s :lease-end e) printed readably, *read-eval* nil
  ;; on read) and install it as the graph's epoch-lease so transaction
  ;; ids come from the lease.  FAST-LOAD and EXPECTED-VECTORS are
  ;; Task 4/5 hooks -- accept and stash them now (fast-load errors
  ;; :not-implemented until Task 4? NO: accept the keyword, signal a
  ;; clear "arrives in a later task" error so Task 4 replaces it).
  )

(defun discard-shadow (shadow-location)
  ;; Close if somehow open (it never registers, so just delete);
  ;; UIOP:DELETE-DIRECTORY-TREE with :validate confining the path to
  ;; one that ENDS in "-shadow" -- refuse anything else (this function
  ;; deletes trees; the guard is the whole safety story).
  )
```

- [ ] **Step 1: Tests** (append to detach-tests):

```lisp
(test shadow-copy-is-consistent-and-reads-resume
  ;; write 3 nodes; SHADOW-STORE; assert the live graph serves READS
  ;; (map-vertices under a pin) afterward; OPEN-SHADOW-GRAPH with a
  ;; lease from CLOCK-LEASE-EPOCHS; assert the 3 nodes are present in
  ;; the shadow.

(test live-store-is-read-only-during-the-shadow-window
  ;; Kevin's ruling: after SHADOW-STORE, a new write transaction on the
  ;; live graph signals STORE-NOT-ACCEPTING-ERROR with reason
  ;; :shadow-load, while PIN-READ-EPOCH succeeds; ABANDON-SHADOW
  ;; deletes the shadow dir and a write then succeeds.  Nearest wrong
  ;; implementation: writes allowed and silently discarded at swap.

(test shadow-is-unregistered
  ;; while the shadow is open: *graphs* has no second entry, the
  ;; open-store vector still maps the store-id to the LIVE graph, and
  ;; RESOLVE-NODE-GRAPH on a shadow-minted id resolves to the LIVE
  ;; graph (same tag).  Nearest wrong implementation: plain OPEN-GRAPH
  ;; (clobbers *graphs*, trips the #209 collision guard).

(test shadow-writes-draw-from-the-lease
  ;; a node written in the shadow has commit-epoch/transaction id
  ;; within [lease-start, lease-end); exhausting a tiny lease (e.g. 3
  ;; epochs) signals EPOCH-LEASE-EXHAUSTED.  Nearest wrong
  ;; implementation: shadow ids from the store's own counter --
  ;; colliding with the live store's post-copy writes.

(test lease-survives-in-the-shadow-directory
  ;; lease.dat exists in the shadow dir and reads back the exact range
  ;; (the out-of-process survival requirement, GH #170 comment).

(test discard-shadow-refuses-non-shadow-paths
  ;; (signals error (discard-shadow <the live store's directory>)) --
  ;; the -shadow suffix guard.  THE deletion-safety test.
```

- [ ] **Step 2: RED. Step 3: Implement** (splice notes: find the txn-id
  allocation point by reading how `attach-to-system-clock`'s clock is
  consulted — grep `clock-next-epoch` in transactions.lisp; the lease
  branch goes where the clock branch is chosen, BEFORE it). Copying:
  plain recursive file copy; the store is closed, so no mmap hazards.
- [ ] **Step 4: Focused green + counts. Step 5: Ablations** — (a)
  registration NOT skipped (drop :shadow-p handling): the
  unregistered test fails (collision guard fires or vector clobbered);
  (b) lease branch removed: the lease test fails. Revert, green.
- [ ] **Step 6: Commit**
  `feat(shadow): shadow generations with leased epochs (#170)`

---

### Task 3: The swap, and the acceptance scenarios

**Files:** `shadow-store.lisp` (swap), `tests/detach-tests.lisp`,
`package.lisp`.

**Interfaces:**

```lisp
(defun swap-in-shadow (graph shadow-location &key (timeout 60))
  ;; GRAPH is the live, open store.  Quiesce (:swapping) -> CLOSE-GRAPH
  ;; -> rename live dir to "<location>-retired-<epoch>" -> rename
  ;; shadow dir to the live location -> JOURNAL-APPEND :swap with
  ;; :store name :retired <retired-path> -> OPEN-GRAPH at the live
  ;; location + ATTACH-TO-SYSTEM-CLOCK -> return (values new-graph
  ;; retired-path).  Any failure BEFORE the first rename leaves the
  ;; live store intact (reopen it and signal); between the renames is
  ;; the unavoidable window -- order them so the live data always
  ;; exists under SOME name (rename live away FIRST, then shadow in;
  ;; a crash between leaves live at the retired path, recoverable by
  ;; hand -- state this in the docstring, it is #171's territory).
```

- [ ] **Tests:**

```lisp
(test swap-in-shadow-end-to-end
  ;; THE acceptance scenario: open clocked store with data A; SHADOW-
  ;; STORE; OPEN-SHADOW-GRAPH; bulk-write data B into the shadow
  ;; (plain transactions); MEANWHILE the live store serves a READ and
  ;; REFUSES a write with reason :shadow-load (the ruled read-only
  ;; window -- "offline only for the swap" means read service);
  ;; SWAP-IN-SHADOW; assert: new graph serves A+B AND accepts a fresh
  ;; write (full service restored); retired dir exists (the old
  ;; generation is kept, not deleted); journal carries :swap and
  ;; :attach.

(test killed-loader-leaves-live-byte-identical
  ;; (use ABANDON-SHADOW as the discard path -- it must also restore
  ;; write service, asserted at the end)
  ;; hash every file in the live directory (sha or byte-compare);
  ;; SHADOW-STORE (copy window); open shadow, write garbage into it,
  ;; simulate the kill by just NOT swapping (close nothing -- abandon
  ;; the object) and DISCARD-SHADOW; byte-compare the live directory
  ;; against the pre-shadow state EXCLUDING files the reopen itself
  ;; legitimately touches (.dirty, txn dir, replication log) -- derive
  ;; the exclusion list from what a bare close/open round-trip changes
  ;; (measure it in the test first!), then assert everything else
  ;; identical.  Nearest wrong implementation: loader writes into the
  ;; live mmaps (any shared-file bug) -- heap.dat diverges.

(test swap-failure-before-rename-restores-service
  ;; force a failure before the first rename (e.g. shadow-location
  ;; does not exist); the live store must still be OPEN-or-reopened
  ;; and serving.
```

- [ ] RED → implement → focused green → ablation (skip the quiesce in
  swap: the end-to-end test with a held pin... add the pin-held variant
  inline — a pin held across SWAP-IN-SHADOW must delay it, same
  mechanism as Task 1's drain test) → commit
  `feat(swap): atomic generation swap with retirement (#170)`

---

### Task 4: Recovery policy and the WAL-suppressed fast path

**Files:** `shadow-store.lisp` or `graph.lisp` (policy read/write),
`transactions.lisp` (`persist-transaction` no-op branch),
`graph-class.lisp` (slot `wal-suppressed-p`), `package.lisp`, tests.

**Contracts:** `store-recovery-policy (location)` → `:derivable` /
`:authored` (reads `policy.dat`, absent = `:authored`);
`(setf-able via) set-store-recovery-policy (location policy)`;
`make-graph`/`open-graph` gain `:recovery-policy` (persisted at create;
open reads the file). `open-shadow-graph :fast-load t` requires the
policy `:derivable` (else `error 'fast-load-requires-derivable`) and
sets `wal-suppressed-p`; `persist-transaction` returns without writing
the .txn file or the replication log when the transaction's graph has
`wal-suppressed-p` (find how the transaction reaches its graph — the
`graph` accessor on tx exists per `transaction-prepare-pathname`).
Tests: fast load leaves the shadow's txn directory EMPTY of .txn files
while the data reads back; `:fast-load t` on an `:authored` store
signals; a NORMAL (non-shadow) graph is never WAL-suppressed (assert
the slot's initform path — nearest wrong implementation: a dynamic
variable that leaks suppression to other graphs). Ablation: drop the
policy gate — the authored-refusal test fails. Commit
`feat(load): recovery policy licenses the WAL-free fast path (#170)`.

---

### Task 5: Vector-segment presize

**Files:** `segment.lisp` (`presize-vector-segment`),
`shadow-store.lisp` (wire `:expected-vectors`), `package.lisp`, tests
(append; if the graph↔segment wiring lives elsewhere — grep
`create-vector-segment` callers — put the wiring where the graph's
segments are opened and REPORT the location).

**Contract:** `presize-vector-segment (segment n)` — under the
segment's write lock, grow (the existing doubling path, segment.lisp
~490) until `segment-capacity ≥ n`; any allocation failure surfaces
HERE, before a single node is written. `open-shadow-graph
:expected-vectors n` presizes each of the shadow's segments (or its
one segment — match reality). Tests: presize to 10000 → capacity ≥
10000 and a subsequent `segment-put` burst triggers NO grow (peek the
capacity before/after); presize failure (simulate: an absurd N whose
file size cannot be reserved — if not reliably simulable on this host,
test instead that presize on a too-large-for-uint N signals cleanly
and REPORT the substitution). Ablation: make presize a no-op — the
no-grow assertion fails. Commit
`feat(segment): presize turns mid-apply capacity failure upfront (#170)`.

---

### Task 6: Documentation

CHANGELOG (`### Added`, model = the #169 entry); manual — new Chapter 17
section "Detach, shadow load, and the swap" (the lifecycle, the two
brief windows, the discard guarantee, policy + fast path, presize) plus
API-reference entries for `detach-store`, `reattach-store`,
`shadow-store`, `open-shadow-graph`, `swap-in-shadow`,
`discard-shadow`, `store-recovery-policy`, `presize-vector-segment` and
the three conditions; spec §8 gets the "**Built (#170):**" note naming
the two-window v1 consistency choice and the out-of-process deferral.
Domain-neutral; 80-col. Commit
`docs: detach quiescence and shadow bulk load (#170)`.

Whole-branch review + PR are the controller's.

---

## Self-Review

- Acceptance list coverage: bulk-load while serving (T3 end-to-end),
  offline only for the windows (T2/T3 tests assert service resumes),
  killed loader byte-identity (T3), capacity fails upfront (T5), suite
  green (gate). Recovery-policy licensing (scope narrative) is T4.
- The one deliberate narrowing vs the spec's prose: v1's consistent
  copy uses a second brief window (close-copy-reopen) rather than
  copying under a live mmap — stated in the architecture, tested as
  "service resumes", and recorded for the spec note in T6.
- The riskiest splice is the txn-id lease branch (T2); it is
  contract-first with an explicit NEEDS_CONTEXT instruction.
- The read-only shadow window is Kevin's explicit ruling (2026-08-22),
  pinned by its own test; no write is ever silently discarded.
