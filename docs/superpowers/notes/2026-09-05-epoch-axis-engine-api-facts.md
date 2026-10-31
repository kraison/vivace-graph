# Engine API facts, verified for the epoch axis (#347)

A snapshot, not a maintained document. The recon pass for
kraison/vivace-graph#347 ("spacetime: an epoch axis for claim reads"):
nine items E1-E9, each settled from source, with the load-bearing
claims re-checked in a live image. It is committed because it is the
record of why the #347 tasks will say what they say -- §C is the
finding -> correction map, and it is read first. Its model is the S3
note (`docs/superpowers/notes/2026-09-05-rules-s3-engine-api-facts.md`).

**Pinned to `3a8ca96`** (`feat/epoch-axis`, which is `origin/experiment`
head plus this note). Every `file:line` will drift; the quoted forms are
what to match on, not the numbers. Paths are relative to the repo root.

**One image run was made.** A single `sbcl --non-interactive` loaded
`:graph-db/spacetime` from this worktree and exercised two stores on one
system clock, plus a create/update/retract series. Everything it
established is marked **VERIFIED (image)**; the run's own crash is
itself finding C3. Items it did not reach -- the reaped chain and the
clockless store -- are marked **inference (source)** and each names the
existing test that already pins the same behaviour. Nothing else here
was executed.

Reading order:

- **§C -- corrections to the issue's assumptions.** Six. C1 is the
  headline and it is good news; C2, C3 and C4 are traps that will
  produce a plausible-looking wrong implementation if missed.
- **§E -- the nine items**, each with the form quoted.
- **§S -- shape of the smallest correct change.**

Line lengths here exceed the repo's 80-column rule inside quoted forms
and tables. Left as verified rather than rewrapped by hand, which risks
silently corrupting a quoted form. Every code line is byte-for-byte; the
only edit is a `...` standing for lines dropped from a form's middle.

---

# §C -- corrections to the issue's assumptions

## C1 (HEADLINE) -- the commit epoch IS recoverable, with no new persisted field

**Issue assumes (part 1):** "If the epoch is not recoverable from a
version without a new persisted field, say so in the issue and the field
is the design."

**Reality: it is recoverable, directly, from the node object the reader
already holds.** `commit-epoch` is a slot on `node`, carried in the
serialized node head, stamped by the shared apply path with the
committing transaction's id, and populated on every read path
(deserialize, cache, `vertex-history`). Part 1 is a one-line reader over
an accessor that already exists; part 1 costs a package export, not a
migration.

`node-class.lisp:420-427`:

```lisp
   ;; MVCC (v2 head): commit-epoch = the committing transaction-id when this
   ;; version was written (global monotonic; for snapshot reads + the reaper).
   ;; prev-pointer = LOCAL heap address of the previous version's archived head
   ;; (0 = none).  Both are serialized in the node head; see serialize-node-head.
   (commit-epoch :accessor commit-epoch :initform 0 :initarg :commit-epoch
                 :type (unsigned-byte 64) :meta t :persistent nil)
   (prev-pointer :accessor prev-pointer :initform 0 :initarg :prev-pointer
                 :type (unsigned-byte 64) :meta t :persistent nil)
```

**VERIFIED (image).** A claim taken straight out of `claims-touching`
reports the id of the transaction that created it:

```
tx-id A=1 tx-id B=2 distinct=T ordered=T
commit-epoch of claim from claims-touching A = 1 (= tx-id? T) revision=0 prev=0
commit-epoch of claim from claims-touching B = 2 (= tx-id? T) revision=0
lookup-vertex A commit-epoch = 1
```

The only work part 1 needs beyond a reader: **`commit-epoch` is not
exported from `GRAPH-DB`.** `package.lisp` exports `#:revision` (:374),
`#:deleted-p` (:375) and `#:vertex-history` (:366) but no
`#:commit-epoch` and no `#:prev-pointer`. `TM-NEXT-EPOCH` and friends are
deliberately internal and say so (`package.lisp:187-189`); `commit-epoch`
is not covered by that comment -- it is a per-node datum, not an
allocator, and reading it burns nothing. See §S.

## C2 -- `resolve-version-at-epoch` is a *snapshot-start* predicate, not an "as of E" predicate

**Issue assumes (part 2, implicitly):** an `:as-of-epoch E` read
"could be implemented as resolve each candidate through
`resolve-version-at-epoch node graph E`".

**Reality: the comparison is strict `<`, against a start epoch, so
passing `E` excludes the commit made AT `E`.** "Created at or before E"
-- the issue's own wording for part 2 -- is
`(resolve-version-at-epoch node graph (1+ E))`.

`transactions.lisp:650` is `(if (< (commit-epoch live-node) epoch) ...)`,
and the engine states the off-by-one in its own words at
`peer-streaming.lisp:956-958`:

```lisp
      ;; A reader with start-tx-id S sees commits with commit-epoch < S; so the
      ;; snapshot's frontier -- the highest tx-id it reflects -- is S-1.  The
      ;; snapshot lives in *READ-SNAPSHOTS*, not *TRANSACTION* (GH #53).
```

That is correct for its own caller: a transaction's `start-tx-id` is
`(tm-current-epoch tm)`, documented as "The next epoch TM-NEXT-EPOCH
would return" (`transactions.lisp:3212-3213`), so it names an epoch that
has not been issued. A *consumer's* `E`, by contrast, is a `transaction-id`
that HAS been issued -- the writer's own committed id, which is exactly
what the issue's motivating case hands it. The two numbers are on the
same axis but on opposite sides of the fence. A `:as-of-epoch E` built by
passing `E` through unchanged will silently return the version *before*
the decision's own transaction on every read, which is the one answer the
consumer most needs to be right.

## C3 -- `resolve-version-at-epoch` is not self-contained: it needs `*graph*` bound

**Not assumed anywhere; found by running it.** The function takes `graph`
as an argument and looks callable standalone. It is not: the archived
head it materializes is read by `deserialize-vertex-head`, which resolves
the stored type-id to a class through the special `*graph*`, not through
the argument. Called outside a `*graph*` binding it dies well below the
surface.

**VERIFIED (image)** -- the run's own backtrace, calling
`(graph-db::resolve-version-at-epoch live ga 1)` with `*graph*` NIL:

```
  There is no applicable method for the generic function
    #<STANDARD-GENERIC-FUNCTION GRAPH-DB:SCHEMA (1)>
  when called with arguments
    (NIL).
...
6: (GRAPH-DB::LOOKUP-NODE-TYPE-BY-ID 2 :VERTEX :GRAPH NIL)
7: (GRAPH-DB::DESERIALIZE-VERTEX-HEAD #<MEMORY ...ga//heap.dat...> 2474)
8: (GRAPH-DB::RESOLVE-VERSION-AT-EPOCH #<EA-CLAIM-UNARY "6b96..." REV 2> #<GRAPH-DB::GRAPH :EP-A ...> 1)
```

`vertex-history` binds it and says why (`transactions.lisp:709-710`):

```lisp
  (let ((*graph* graph)   ; DESERIALIZE-VERTEX-HEAD resolves the node type
                          ; through *GRAPH*, not through an argument.
```

`resolve-version-at-epoch` has no such binding, and its only in-tree
caller (`transactions.lisp:342-345`, inside `lookup-object`'s
transactional method) sits *outside* the `(let ((*graph* (graph
transaction))) ...)` two lines above it -- it happens to be safe only
because `with-transaction (:graph G)` binds `*graph*` for the whole body
(`transactions.lisp:449-452`). A cross-store `:as-of-epoch` read is
explicitly NOT inside a transaction on those stores (#345/#332: a
cross-store scope runs with `*transaction*` nil), so this is a live trap,
not a theoretical one.

It also takes no read pin. `vertex-history` does, and documents the
reaper interaction that pin is protecting against
(`transactions.lisp:711`, and its docstring at `:698-708`).

**Consequence for the design:** build `%claim-as-of-epoch` on
`vertex-history`, exactly as `%claim-as-of` is, rather than on
`resolve-version-at-epoch`. It is the same walk (the engine says so in
`transactions.lisp:667-670`), it already binds `*graph*`, it already
pins, it is exported, and it hands back the commit epoch of every version
as the cdr of each cons -- which is the whole of what the epoch reader
needs. Reaching under to `resolve-version-at-epoch` buys an early exit
and costs two invariants.

## C4 -- the epoch axis alone cannot tell "reaped" from "created after E"; `revision` can

**Issue assumes (part 2):** "with the same `reaped-claim` reporting as
`:as-of` when the version of that age is past `:keep-revisions`."

**Reality: the mechanism `%claim-as-of` uses to make that distinction has
no epoch twin, and a naive port will report `reaped-claim` for every
claim created after E.** But `revision` closes the gap exactly, and more
sharply than the wall-clock rule does.

When the walk runs out of versions, two situations are indistinguishable
from the chain alone, because reaping *severs the pointer*
(`transactions.lisp:804-816`, `%sever-prev-pointer`, which zeroes the
owner's prev-pointer both in the lhash head and on disk):

1. the claim was created after E -- the oldest version IS the create, and
   its prev-pointer is 0 because a create has none
   (`transactions.lisp:968-969`);
2. older versions existed and were reaped -- the oldest RETAINED version's
   prev-pointer is 0 because the reaper set it to 0.

`%claim-as-of` breaks the tie with the immutable *transaction extent
start*, a wall-clock timestamp (`claim-query.lisp:177-184`). There is no
epoch equivalent: nothing records "the epoch at which this claim was
created" except the create version's own `commit-epoch`, which is
precisely the thing that got reaped.

**The discriminator is `revision`.** A create sets it to 0
(`transactions.lisp:952`, `(setf (revision node) 0)`); every update
increments it (`transactions.lisp:982-983`). So on the OLDEST RETAINED
version:

| oldest retained `revision` | meaning |
|---|---|
| `0` | that version is the create. Nothing was reaped below it. If its `commit-epoch > E`, the claim did not exist at E -> **drop it** (NIL). |
| `> 0` | at least `revision` older versions once existed and are gone. If no retained version has `commit-epoch <= E`, the answer is unavailable -> **`reaped-claim`**. |

**VERIFIED (image)**, for the retained case: a create/update/retract
series under `:keep-revisions 10` gives

```
create=1 update=3 retract=4
vertex-history epochs newest-first = (4 3 1)
vertex-history revisions newest-first = (2 1 0)
oldest retained: revision=0 prev-pointer=0 commit-epoch=1
```

**inference (source)** for the reaped case: `reap-node-chain`
(`transactions.lisp:835-864`) retains `keep` archived versions and then
severs, and `resolve-version-at-epoch` returns NIL at `(when (zerop p)
(return nil))`. Not exercised in the image (the run stopped at C3's
crash before reaching it). `tests/spacetime/claim-tests.lisp`'s
`as-of-reports-reaped-not-a-lie` (with the `kr-claim` family declared
`:keep-revisions 1` at `:232`) already pins the wall-clock half.

This is strictly better than the `:as-of` rule, which cannot distinguish
either when the transaction extent is NIL and falls back to reporting
`reaped-claim` (`claim-query.lisp:181`). Worth saying so in #347: the
epoch reader gets a *sharper* reaped/absent distinction than the wall-clock
reader, not a weaker one.

## C5 -- `attach-to-system-clock` lives in `transactions.lisp`, not `graph.lisp`

Minor, but it will cost a search. `graph.lisp:585` and `graph.lisp:1090`
are the two CALL sites (`make-graph` and `open-graph`, both
`(when system-clock (attach-to-system-clock graph system-clock))`). The
definition is `transactions.lisp:3233-3266`.

## C6 -- `:as-of-epoch` must bypass the open-transaction overlay, for the reason `:as-of` does

The issue does not mention it. `claims-touching` routes `:as-of` around
`%overlay-transaction`, and its docstring states the rule
(`claim-query.lisp:258-261`):

```
Inside an open transaction the answer is what THAT transaction will
commit (GH #324): its own retractions, updates and new claims are
visible, so retract-then-assert on one series reads correctly before
the commit.  :AS-OF is the exception -- it answers committed history
only, since an uncommitted change is not yet history.
```

An uncommitted write has NO epoch at all -- `transaction-id` is assigned
at `transactions.lisp:3459`, inside `%commit`, after validation -- so for
`:as-of-epoch` the exception is not a policy choice but a structural
fact. Say so in the docstring rather than restating #324's reasoning.

---

# §E -- the nine items

## E1 -- where a version's commit epoch lives

**Settled: on the version, as a first-class node-head field, reachable by
accessor from the node object a reader holds. No table walk.**

The slot, `node-class.lisp:420-427`, is quoted in C1. It is `:meta t
:persistent nil` -- i.e. it is head metadata, not a user data slot, so it
does not travel in the serialized data alist; it travels in the fixed
node head. `primitive-node.lisp:3` names the layout:

```lisp
;; v3 head: flags(1) type-id(4) revision(4) data-pointer(8) commit-epoch(8)
```

written by `pack-node-head` (`primitive-node.lisp:63-71`):

```lisp
(defun pack-node-head (vec i n)
  "Fill the node head of N into VEC starting at index I; return the next index."
  (setf (aref vec i) (flags-as-int n))
  (setq i (pack-uint vec (1+ i) (type-id n)      4))
  (setq i (pack-uint vec i       (revision n)     4))
  (setq i (pack-uint vec i       (data-pointer n) 8))
  (setq i (pack-uint vec i       (commit-epoch n) 8))   ;; MVCC v2
  (setq i (pack-uint vec i       (prev-pointer n) 8))   ;; MVCC v2
  i)
```

and read back by `deserialize-node-head` (`primitive-node.lisp:109-114`),
whence `%make-vertex` sets it on the instance (`vertex.lisp:42`,
`(when commit-epoch (setf (commit-epoch vertex) commit-epoch))`).

**Who stamps it.** One dynamic variable, bound once per apply
(`transactions.lisp:606-609`):

```lisp
;; The committing transaction-id, bound by APPLY-TRANSACTION (the shared apply
;; path, so masters, slaves, restore and recovery all stamp consistently).  Never
;; travels on the replication wire -- the slave re-derives it from the tx header.
(defvar *commit-epoch* 0)
```

bound at `transactions.lisp:1902-1903`:

```lisp
            ;; MVCC: every write in this transaction is stamped with this id.
            (*commit-epoch* (transaction-id transaction)))
```

and consumed on create (`transactions.lisp:967-969`) and update
(`transactions.lisp:990-994`):

```lisp
    ;; MVCC: stamp the committing epoch; a fresh node has no prior version.
    (setf (commit-epoch node) *commit-epoch*
          (prev-pointer node) 0)
```

```lisp
    ;; MVCC: archive the prior version's head (it still points at the retained
    ;; old data block) and chain the new live head to it.  The old data block is
    ;; NO LONGER freed here -- REAP-OLD-VERSIONS reclaims it when epoch-safe.
    (setf (prev-pointer new-node) (archive-node-version old-node graph)
          (commit-epoch new-node) *commit-epoch*)
```

The memory-graph backend does the same at `memory-graph.lisp:1242` and
`:1265`; replication rebinds it per applied op at
`peer-streaming.lisp:1133` and `:1157`.

**Reachable without a walk?** Yes, for the live version -- the claim
object a reader holds IS the live head, and `(commit-epoch c)` answers
immediately. For older versions the accessor is the same; the *versions*
come from a walk, and the supported walk is `vertex-history`
(`transactions.lisp:671-676`), which hands back the epoch alongside each
version so no second read is needed:

```lisp
(defun vertex-history (graph id &key limit)
  "Return the retained versions of the vertex ID in GRAPH as a list of
\(VERSION . COMMIT-EPOCH) conses, NEWEST FIRST.  The live version is included
and is always the first entry.  ID is a 16-byte id array or its string form.
Returns NIL if GRAPH holds no vertex with that id.  LIMIT, when given, caps the
result at the LIMIT newest versions.
```

**VERIFIED (image):** `vertex-history epochs newest-first = (4 3 1)` for
a claim created at 1, updated at 3, retracted at 4; and the same claim
fetched fresh through `lookup-vertex` reports `commit-epoch = 1` before
those writes.

**The zero.** `commit-epoch` is `0` in exactly two cases and both mean
"no epoch": a v1 head, whose reader hard-codes it
(`primitive-node.lisp:204`, `0 ;; commit-epoch (v1 has none)`), and the
`:initform 0` on a node never applied. It is never a legitimate issued
id: a store's own counter is seeded `(1+ (max (load-highest-transaction-id
graph) ...))` (`transactions.lisp:3031`), so the first id is >= 1, and
`no-clock-means-per-store-counters-unchanged`
(`tests/system-clock-tests.lisp:448`) asserts `(is (= 1 ia ib))`. So
`(if (plusp e) e nil)` is a sound NIL-for-no-epoch rule, which is exactly
what the issue asks part 1's reader to return.

**Not exported.** See C1.

## E2 -- what `transaction-id` is under a shared clock

**Settled.** The id is allocated once, at commit, from `tm-next-epoch`
(`transactions.lisp:3459`):

```lisp
               (setf (transaction-id tx) (tm-next-epoch tm))
```

which resolves the three regimes in a fixed order
(`transactions.lisp:3192-3209`):

```lisp
(defun tm-next-epoch (transaction-manager)
  "Allocate a fresh epoch: from the lease when TRANSACTION-MANAGER's
graph has one (shadow stores, GH #170), else from the image clock when
the graph has one, otherwise from this manager's own counter (pre-#168
behaviour)."
  (let ((lease (tm-lease transaction-manager)))
    (if lease
        (let ((next (epoch-lease-next lease)))
          (when (>= next (epoch-lease-end lease))
            (error 'epoch-lease-exhausted
                   :name (graph-name (graph transaction-manager))
                   :end (epoch-lease-end lease)))
          (setf (epoch-lease-next lease) (1+ next))
          next)
        (let ((clock (tm-clock transaction-manager)))
          (if clock
              (clock-next-epoch clock)
              (prog1 (tx-id-counter transaction-manager)
                (incf (tx-id-counter transaction-manager))))))))
```

`tm-clock` is just the store's clock slot (`transactions.lisp:3179-3183`):

```lisp
(defun tm-clock (transaction-manager)
  "TRANSACTION-MANAGER's image clock, or NIL for its own counter (GH #168).
A transaction-manager always has a graph -- INITIALIZE-INSTANCE :AFTER
dereferences it immediately, so a NIL graph dies there first."
  (graph-system-clock (graph transaction-manager)))
```

and the clock is one lock-guarded counter (`system-clock.lisp:284-289`):

```lisp
(defun clock-next-epoch (clock)
  "Allocate and return a fresh epoch."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%clock-reserve clock 1)
    (prog1 (system-clock-counter clock)
      (incf (system-clock-counter clock)))))
```

**With a clock: one counter, unique across stores.** Pinned by
`two-stores-on-one-clock-get-disjoint-ordered-epochs`
(`tests/system-clock-tests.lisp:389-419`), which interleaves three
commits on each of two stores and asserts

```lisp
                        (let ((sorted (sort (copy-list ids) #'<)))
                          ;; No two transactions anywhere share an epoch.
                          (is (= (length sorted)
                                 (length (remove-duplicates sorted))))))
```

**VERIFIED (image)** independently: two claim families in two stores
attached to one clock, `tx-id A=1 tx-id B=2 distinct=T ordered=T`, and a
third store on the same clock continued the same sequence.

**Without a clock: per-store counters that collide by construction.**
`no-clock-means-per-store-counters-unchanged`
(`tests/system-clock-tests.lisp:421-451`) asserts the values, not just
their equality:

```lisp
                 (is (= 1 ia ib))
                 (is (= 2 ia2)))
```

Two stores' epoch 1 are two unrelated transactions. This is the whole of
E9's case.

**A third regime exists and the issue does not mention it.** A shadow
store allocates from a leased range (`tm-lease`,
`transactions.lisp:3185-3190`; `clock-lease-epochs`,
`system-clock.lisp:314-323`). Leased ids ARE drawn from the clock's
sequence and skip it forward, so they remain globally unique and
comparable -- a lease is not a third comparability regime, only a third
allocation path. Worth one sentence in the eventual docstring so nobody
adds a fourth branch.

## E3 -- how a store's clock attachment is visible

**Settled: a public reader on the graph, returning the clock object
itself or NIL.** `graph-class.lisp:223-229`:

```lisp
   ;; The image-level epoch clock (GH #168), or NIL for this store's own
   ;; counter.  NIL is the pre-#168 behaviour and the default.  Reader
   ;; public, writer internal: ATTACH-TO-SYSTEM-CLOCK is the only entry
   ;; point -- a bare SETF would skip its watermark/journal (GH #183).
   (system-clock :reader graph-system-clock
                 :accessor %graph-system-clock
                 :initarg :system-clock :initform nil)
```

`#:graph-system-clock` and `#:attach-to-system-clock` are both exported
(`package.lisp:46-47`), as are `#:system-clock`, `#:open-system-clock`,
`#:close-system-clock` (`package.lisp:26-30`). The struct's own accessors
(`system-clock-location`, `-counter`, ...) are NOT exported; a caller
gets an opaque object it can only compare.

That comparison is the whole test for "same clock": `attach-to-system-clock`
stores the very object it is handed (`transactions.lisp:3264`,
`(setf (%graph-system-clock graph) clock)`), so two graphs attached to
one clock return `eq` values.

**VERIFIED (image):**

```
graph-system-clock ga eq gb: T
graph-system-clock ga eq clock: T
type of clock: SYSTEM-CLOCK
```

So the predicate a consumer (or `:as-of-epoch`) needs is exactly:

- attached at all: `(graph-system-clock g)` non-NIL;
- mutually comparable: `(eq (graph-system-clock g1) (graph-system-clock g2))`
  and non-NIL.

`attach-to-system-clock` (`transactions.lisp:3233-3266`) additionally
refuses while the store has in-flight transactions
(`attach-with-active-transactions`) and raises the clock above the store's
persisted history before recording the attach, so an attach cannot make
an already-issued local id ambiguous. That matters for #347: a store
attached mid-life has old versions carrying LOCAL-counter epochs below
the watermark, and those are not comparable with another store's. See E9.

## E4 -- `%claim-as-of` today, and what the epoch twin must do differently

Quoted whole (`claim-query.lisp:140-184`), with its helper:

```lisp
(defun %claim-effective-stamp (version)
  "VERSION's place on the wall clock: its :AS-OF stamp, else the start of
its (immutable) transaction extent, else NIL for a claim predating both
axes -- treated as arbitrarily old."
  (or (claim-version-stamp version)
      (let ((te (claim-transaction-extent version)))
        (when te
          (let ((b (extent-start te)))
            (let ((e (bound-earliest b)))
              (unless (eq e :unbounded) e)))))))

(defun %claim-as-of (graph claim at)
  "The version of CLAIM believed AT (a TIMESTAMP), or NIL when the claim
was not believed then (not yet created, or already retracted), or a
REAPED-CLAIM when it existed but every version of that age is reaped.
Walks VERTEX-HISTORY newest-first over the family's retained chain."
  (let* ((history (graph-db:vertex-history graph (graph-db:id claim)))
         (instant (make-instant (exact-bound at)))
         (resolved
           (loop for (version . nil) in history
                 for stamp = (%claim-effective-stamp version)
                 when (or (null stamp)
                          (not (local-time:timestamp< at stamp)))
                   return version)))
    (cond
      (resolved
       ;; Believed at AT only while AT falls inside that version's
       ;; transaction period: a retraction closes it, so an instant
       ;; after the close resolves to the retracted version and drops
       ;; here.  A NIL extent predates the axis: indeterminate, kept.
       (let ((te (claim-transaction-extent resolved)))
         (if (or (null te) (not (extents-disjoint-p te instant)))
             resolved
             nil)))
      ((null history) nil)
      (t
       ;; No retained version is old enough.  The immutable transaction
       ;; start on ANY version says whether the claim existed at AT.
       (let ((te (claim-transaction-extent (car (first history)))))
         (cond ((null te) (%make-reaped-claim (graph-db:id claim)))
               ((let ((e (bound-earliest (extent-start te))))
                  (and (not (eq e :unbounded))
                       (local-time:timestamp< at e)))
                nil)                    ; did not exist yet
               (t (%make-reaped-claim (graph-db:id claim)))))))))
```

Note the two-step shape: **select** a version by stamp, then **filter**
it by whether the transaction period was still open at `at`. The second
step is needed because the retraction is itself a version, and resolving
"at an instant after the close" lands ON the retracted version rather
than past it.

### What the epoch twin must do differently, precisely

Four changes; everything else is the same walk.

1. **Selection compares integers, not timestamps, and reads the cdr.**
   `vertex-history` already yields `(VERSION . COMMIT-EPOCH)`, so the
   `%claim-effective-stamp` call disappears entirely:

   ```lisp
   (loop for (version . epoch) in history
         when (<= epoch e) return version)
   ```

   Newest-first ordering makes the first hit the newest version with
   `commit-epoch <= E`, which is the issue's "created at or before E".
   Note `<=`, not `<` -- see C2 for why the engine's own internal
   predicate is strict and this one must not be.

2. **"Not retracted at or before E" needs no separate mechanism -- but
   it is not the same test as `%claim-as-of`'s.** A retraction IS a
   version: `retract-claim` is `copy` + `save`
   (`claim-query.lisp:407-414`), so it commits through `tx-update`, gets
   a fresh archived predecessor and a fresh `commit-epoch`. Therefore
   **the retraction's epoch is the retracting version's own
   `commit-epoch`** -- there is nothing else to look up.

   And that makes the filter step *fall out of the selection*: if the
   retraction committed at or before E, step 1 already selected the
   retracted version (or a later one), and `claim-current-p` on the
   selected version is NIL. If it committed after E, step 1 selected an
   earlier, still-open version. So the epoch twin's filter is simply
   `(claim-current-p resolved)` -- no `extents-disjoint-p`, no instant
   probe, no wall clock anywhere in the function.

   **VERIFIED (image),** create at 1, in-place update at 3, retract at 4:

   ```
   live commit-epoch=4 (= retract tx? T) current-p=NIL
   vertex-history epochs newest-first = (4 3 1)
   vertex-history revisions newest-first = (2 1 0)
   vertex-history current-p newest-first = (NIL T T)
   ```

   Reading the columns: `:as-of-epoch 3` selects the version at 3, whose
   `claim-current-p` is T -> returned. `:as-of-epoch 4` selects the
   version at 4, `claim-current-p` NIL -> dropped. `:as-of-epoch 2`
   selects the version at 1 -> returned, with the pre-update validity.
   That is the whole of part 2's semantics, with one integer compare and
   one predicate.

   One caveat to carry into the docstring: `claim-current-p` treats a NIL
   transaction extent as still-believed (`claim-query.lisp:385-390`, "a
   claim predating the axis was never retracted"). The epoch reader
   inherits that, unchanged, and should.

3. **The exhausted-history branch uses `revision`, not the extent.** See
   C4 for the rule and the table.

4. **Nothing calls `claim-version-stamp`.** See E5.

## E5 -- `claim-version-stamp` and the `save :before`

The per-version wall-clock stamp is a real persisted claim slot
(`spacetime/claim.lisp:170-174`):

```lisp
    ;; The time->version mapping for :AS-OF (GH #300): stamped at every
    ;; create and save; replicates with the claim, so an as-of read is
    ;; node-local by construction and no epoch is ever exposed.
    (version-stamp :initarg :version-stamp :accessor claim-version-stamp
                   :initform nil)
```

maintained by a `:before` method the family macro generates
(`spacetime/claim.lisp:476-482`):

```lisp
       ;; The update half of the :AS-OF stamp (GH #300): every
       ;; copy/setf/SAVE re-stamps.  Replication's direct UPDATE-NODE
       ;; path bypasses this deliberately -- remote state keeps its
       ;; remote stamp.
       (defmethod graph-db:save :before ((c ,parent) &key graph)
         (declare (ignore graph))
         (setf (claim-version-stamp c) (%st-now)))
```

**VERIFIED (image):** `version-stamp A = @2026-09-05T18:52:31.007346Z` on
a freshly created claim, so the create half is live too (via
`%stamp-now`, `spacetime/claim.lisp:246-247`).

**Answer: an epoch reader needs no persisted slot at all.** E1's
`commit-epoch` is enough, and it is strictly better suited:

| | `claim-version-stamp` (a persisted claim slot) | `commit-epoch` (a node-head field) |
|---|---|---|
| cost to add | already there for `:as-of` | already there for MVCC |
| cost to add an *epoch* one | a new persistent slot on every claim family -> a data migration, a `def-claim-classes` change, a replication question, and a second source of truth that can disagree with the head | nothing |
| set by | a CLOS `:before` on `save`, which replication's direct `update-node` path deliberately bypasses | the shared apply path, which the comment at `transactions.lisp:606-608` says covers "masters, slaves, restore and recovery" |
| meaning under two stores | two independent wall clocks | one counter, when both are attached |

The second row is the design answer to the issue's "if the epoch is not
recoverable ... the field is the design": the field is not needed, and
adding one would be worse than not adding one, because the `:before`
method's own comment records a path that skips it.

## E6 -- `:keep-revisions` and reaping, under an epoch query

**"The version of that age is reaped" means: the prev-pointer chain from
the live head has been severed above the version E would have selected,
so no version with `commit-epoch <= E` is still reachable.**

The retention window (`transactions.lisp:867-876`, `node-keep-revisions`)
is the node type's `:keep-revisions` if set, else the graph's (default 0).
`def-claim-classes` defaults it high on purpose
(`spacetime/claim.lisp:321-323`):

```lisp
(defmacro def-claim-classes (parent graph-name
                             &key extra-slots temporal
                                  (keep-revisions (1- (expt 2 32))))
```

so an ordinary claim family is effectively never reaped and this branch
is the exception, not the rule. `reap-old-versions`
(`transactions.lisp:878-897`) runs post-commit inside the manager lock;
`reap-node-chain` (`transactions.lisp:835-864`) frees the oldest suffix
and severs exactly one pointer.

`vertex-history` already says what a short chain does and does not mean,
and #347's docstring should point at it rather than restate it
(`transactions.lisp:698-703`):

```
⚠ THE DEPTH AVAILABLE IS BOUNDED BY KEEP-REVISIONS -- the node type's if it
sets one, otherwise the graph's (default 0, i.e. NO history beyond the live
version).  REAP-OLD-VERSIONS discards versions past that window as soon as no
active reader could still observe them.  So a SHORT HISTORY DOES NOT MEAN THE
VERTEX WAS EDITED FEW TIMES
```

**Does `reaped-claim` need an epoch field?** No -- and adding one would
be wrong. The struct is deliberately minimal
(`claim-query.lisp:133-138`):

```lisp
(defstruct (reaped-claim (:constructor %make-reaped-claim (id)))
  "An :AS-OF answer the store can no longer give: the claim existed at
the asked instant, but every version stamped then is past the family's
:KEEP-REVISIONS window and reaped.  Reported, never silently substituted
(GH #300)."
  id)
```

The value a field would carry is the epoch that was *asked for*, which
the caller already has (it passed it), or the epoch of the version that
was reaped, which by definition is gone. `reaped-claim` says "this id,
at the instant you named, is unanswerable"; the instant is the caller's.
Reuse the struct verbatim -- one type for both axes keeps the consumer's
`reaped-claim-p` check unchanged, and the second sentence of the
docstring can be widened from "stamped then" to "of that age" so it
covers both.

Note also that `reaped-claim` values flow through every downstream filter
untouched -- `claims-touching` guards each with `(or (reaped-claim-p c) ...)`
at `claim-query.lisp:314`, `:319` and `:326`. `:as-of-epoch` gets that
for free by returning the same struct.

**inference (source)** for this whole item; the image run stopped before
it. See C4.

## E7 -- snapshot epochs vs commit epochs

**A read snapshot's `start-tx-id` is `tm-current-epoch` at creation**
(`transactions.lisp:3294-3297`):

```lisp
           (start-tx-id (tm-current-epoch transaction-manager))
           (tx (make-instance 'tx
                              :sequence-number sequence-number
                              :start-tx-id start-tx-id
```

and inside `lookup-object` the resolution is exactly the one #347 wants
(`transactions.lisp:337-345`):

```lisp
                ;; P4: resolve the version visible at this transaction's snapshot
                ;; (commit-epoch < start-tx-id).  The resolved (possibly archived)
                ;; version is cached only in the txn-private local-cache, so reads
                ;; are repeatable within the transaction.  Validation keys the
                ;; read-set by id, so OCC is unaffected.
                (when *snapshot-reads-p*
                  (setq value (resolve-version-at-epoch
                               value (graph transaction)
                               (start-tx-id transaction))))
```

**So yes, in principle: "the version live at epoch E" is the same
question a snapshot answers internally.** But three things stand between
that observation and the implementation the issue sketches, and all three
are corrections: the strict `<` (C2), the missing `*graph*` binding and
read pin (C3), and the reaped/absent conflation (C4).

The contract, quoted whole (`transactions.lisp:645-666`):

```lisp
(defun resolve-version-at-epoch (live-node graph epoch)
  "Return the version of LIVE-NODE visible to a reader whose snapshot is EPOCH
(the newest version with commit-epoch < EPOCH), or NIL if the node did not exist
before EPOCH.  Materializes an archived version (full head + data bytes) when the
live head is newer than EPOCH."
  (if (< (commit-epoch live-node) epoch)
      live-node
      (let ((id (id live-node))
            (edge-p (typep live-node 'edge))
            (p (prev-pointer live-node)))
        (loop
          (when (zerop p) (return nil))   ; nothing old enough -> invisible
          (let ((ver (if edge-p
                         (deserialize-edge-head (heap graph) p)
                         (deserialize-vertex-head (heap graph) p))))
            (setf (id ver) id)
            (if (< (commit-epoch ver) epoch)
                (progn (ensure-node-bytes ver graph) (return ver))
                (setf p (prev-pointer ver))))))))
```

Answering the two asked cases directly:

- **All versions newer than E:** walks to `(zerop p)` and returns
  **NIL**. Correct, and the docstring says "the node did not exist before
  EPOCH".
- **None retained (archived away):** ALSO returns **NIL**, by the same
  `(zerop p)` exit, because `%sever-prev-pointer`
  (`transactions.lisp:804-816`) zeroes the pointer. The docstring's
  "did not exist before EPOCH" is therefore not the only reading of a NIL
  return, and #347 must not treat it as one. This is C4.

The comment above the function frames the guarantee that makes the
conflation invisible to its current caller
(`transactions.lisp:635-640`):

```lisp
;;; A transaction observes the newest version with commit-epoch < its
;;; start-tx-id.  When the live head is too new (committed after the reader
;;; started), walk the prev-pointer chain and materialize the archived version
;;; that was live as of the reader's snapshot.  The reaper's floor retains every
;;; version an active transaction could need, so the chain is guaranteed present.
```

"The reaper's floor retains every version an active transaction could
need" -- true for a transaction that is *currently active*, whose
`start-tx-id` bounds `reap-safe-floor`. It is NOT true for a historical
`E` handed in from outside months later. `:as-of-epoch` is precisely the
case the sentence does not cover, and that is the whole reason
`reaped-claim` has to exist on this axis too.

**Recommendation: do not call `resolve-version-at-epoch` from the claim
layer at all.** Walk `vertex-history`, as `%claim-as-of` does. See C3.

## E8 -- index membership bounds part 2's contract

**Settled, and it is the same limit #345 records for snapshots.**
`claims-touching` and `claims-by-producer` both find candidates through
the CURRENT secondary index (`claim-query.lisp:276-287` and `:439-440`,
all `graph-db:index-lookup`), and only then resolve each candidate's
version. `index-lookup` resolves through the live table and drops
soft-deleted nodes (`index.lisp:1006-1010`):

```lisp
        (when key
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
```

`%node-by-id` is `(or (lookup-vertex ...) (lookup-edge ...))`
(`spatial-query.lisp:35-38`) -- the LIVE node.

So a claim **deleted** (`mark-deleted`, `interface.lisp:108-118` -- a
soft delete, distinct from `retract-claim`) after E never enters the
candidate set, and no amount of version resolution can bring it back:
the walk is only ever applied to ids the current index still yields.
`delete-claims-by-producer` (`claim-query.lisp:450-463`) is a bulk
`mark-deleted`, so a regenerating producer's sweep has exactly this
effect.

`docs/rules.md:250-260` already states the snapshot form of this and
should be the place #347's docs point at:

```
**A snapshot hides an insert, not a delete.** Secondary-index
*membership* is not snapshot-versioned: `%ix-release` removes the entry
outright, post-durability, and `index-lookup`'s only snapshot-aware
step is resolving an id it has already found.
```

**The contract sentence part 2 must carry:** `:as-of-epoch E` returns the
version of each claim that the CURRENT index still knows about, resolved
to epoch E. A retraction is fully visible (it is a version, and the claim
stays in the index -- `retract-claim`'s docstring, `claim-query.lisp:395`:
"NOT a deletion. A retracted claim still occupies its identity tuple").
A deletion is not. The distinction between retraction and deletion is
already the subsystem's, and #347 does not change it -- it only has to
say so, because a reader who has just been told "as of epoch E" will
reasonably expect otherwise.

## E9 -- clock regimes

**Recommendation: refuse, as the issue proposes. The evidence is
unambiguous and there are three regimes, not two.**

| store state | `(graph-system-clock g)` | epochs are | comparable with another store's? |
|---|---|---|---|
| attached to clock C | C (`eq`) | drawn from C's single counter | yes, with any other store attached to C |
| attached to clock D | D | drawn from D's counter | **no** -- two counters, both starting near 0 |
| never attached | NIL | this store's own `tx-id-counter` | **no** -- and it collides: both stores' first id is 1 |
| attached mid-life | C | *recent* versions from C; versions written before the attach carry the local counter's values, below the attach watermark | partially; a single store's own history is monotone but old epochs are not C's |

Rows 2-4 are the case for refusing. Row 3's collision is not a
theoretical risk, it is asserted as the intended behaviour by
`no-clock-means-per-store-counters-unchanged`
(`tests/system-clock-tests.lisp:446-449`), whose comment is explicit that
"their ids DO collide". Answering `:as-of-epoch 7` from a clockless store
would return an answer that looks identical to the attached case and
means something unrelated -- the exact failure mode this subsystem exists
to prevent (compare `claims-touching`'s own docstring principle at
`claim-query.lisp:263-265`: "an out-of-range ROLE signals rather than
silently returning NIL ... this subsystem exists to keep those two cases
from being confused").

Row 4 is the honest caveat and does NOT justify refusing.
`attach-to-system-clock` raises the clock above the store's persisted
history before recording the attach (`transactions.lisp:3251-3264`), so
within one store epochs stay
monotone across an attach and `<=` still selects correctly. What is lost
is only that a pre-attach epoch of store A and a pre-attach epoch of
store B are unrelated. That is a documentation matter, not a refusal;
the same is true of any epoch predating whichever clock is in play, and
it is why part 1's reader returns NIL rather than 0.

**What to signal.** `query-precondition-error` (`globals.lisp:487-490`)
is the existing type for "the caller asked something this store cannot
answer", is already used for exactly this shape of refusal in the index
layer (GH #286), and is already something a REST/server layer knows how
to tell apart from a defect:

```lisp
(define-condition query-precondition-error (error)
  ((reason :initarg :reason :reader query-precondition-error-reason))
  (:report (lambda (c s)
             (format s "~A" (query-precondition-error-reason c)))))
```

A dedicated subtype (`epoch-axis-unavailable`, with a `graph-name` slot)
is worth it if the consumer needs to branch on it -- kraison/cl-llm#24
plausibly wants "fall back to the wall-clock axis" rather than "fail" --
and is cheap. Recommend the subtype.

**Refusing does not require comparing two stores.** The check is
per-store and local: `:as-of-epoch` on store G refuses iff
`(graph-system-clock G)` is NIL. Whether two stores share a clock is the
*consumer's* precondition, checked with the `eq` test from E3, and it
belongs there rather than in a single-store reader that cannot see the
other store. Say so, or someone will try to make `claims-touching`
enforce it.

---

# §S -- shape of the smallest correct change

Three functions, one export, one condition. No persisted field, no
migration, no write-side change, and `split-claim-identity-key` untouched
(the issue's part 3 needs no work: nothing below touches identity).

### S1. Export the accessor -- `package.lisp`

```lisp
           ;; MVCC: a version's committing epoch (GH #347)
           #:commit-epoch
```

next to `#:vertex-history` (`package.lisp:365-366`), which is already
exported for the same reason and hands out the same number. `#:revision`
is already exported (`package.lisp:374`), so C4's discriminator needs
nothing. Do NOT export `#:prev-pointer` -- it is a heap address and no
consumer has a use for it.

### S2. `graph-db.spacetime:claim-commit-epoch` -- issue part 1

```lisp
(defun claim-commit-epoch (claim)
  "The epoch of the transaction that committed CLAIM'S version, or NIL
for a version written before the store had a clock (and for a
REAPED-CLAIM, which is a version the store no longer holds).  Comparable
across stores only while they share one SYSTEM-CLOCK -- see
GRAPH-DB:GRAPH-SYSTEM-CLOCK.  The number is the writer's own
TRANSACTION-ID; no new field records it (GH #347)."
  (unless (reaped-claim-p claim)
    (let ((e (graph-db:commit-epoch claim)))
      (and (plusp e) e))))
```

Export `#:claim-commit-epoch` from `spacetime/package.lisp` beside
`#:claim-version-stamp` (`spacetime/package.lisp:57`).

The `reaped-claim-p` guard is not optional: `claims-touching :as-of`
already returns `reaped-claim` structs mixed into the same list as
claims, and a consumer mapping this over a result set will hit one.

### S3. `%claim-as-of-epoch` -- the resolver, `spacetime/claim-query.lisp`

Beside `%claim-as-of`, same shape, no wall clock:

```lisp
(defun %claim-as-of-epoch (graph claim epoch)
  "The version of CLAIM live at EPOCH -- committed at or before EPOCH and
not retracted at or before it -- or NIL when it was not, or a
REAPED-CLAIM when it existed then but no version of that age is
retained.  Walks VERTEX-HISTORY newest-first over the family's retained
chain, comparing each version's own commit epoch (GH #347).")
```

Body, in prose so the eventual code is written against the facts rather
than copied from a sketch:

1. `history` := `(graph-db:vertex-history graph (graph-db:id claim))`.
   NOT `resolve-version-at-epoch` -- C3.
2. select the first `(version . e)` with `(<= e epoch)`. `<=`, not `<` --
   C2.
3. if one was selected: return it if `(claim-current-p version)`, else
   NIL. That single predicate covers "not retracted at or before E" --
   E4 step 2. Do NOT copy `%claim-as-of`'s `extents-disjoint-p` probe;
   there is no instant to probe with.
4. if none was selected and `history` is non-NIL: look at the OLDEST
   entry. `(zerop (graph-db:revision version))` -> the claim was created
   after `epoch`, return NIL. Otherwise -> `(%make-reaped-claim
   (graph-db:id claim))`. This is C4 and it is the only part with no
   counterpart in `%claim-as-of`.
5. `history` NIL -> NIL, as `%claim-as-of` does.

### S4. `:as-of-epoch` on the two readers -- issue part 2

`claims-touching` (`claim-query.lisp:221`) and `claims-by-producer`
(`claim-query.lisp:420`) each gain `as-of-epoch` in the lambda list and,
at the top:

```lisp
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when (and as-of-epoch (null (graph-db:graph-system-clock graph)))
    (error 'epoch-axis-unavailable :graph-name (graph-db:graph-name graph)))
```

modelled on the existing `:at`/`:during` exclusion three lines below
(`claim-query.lisp:270-271`). Then the resolve step becomes a three-way
`cond` in place of today's `if`, with `%claim-as-of-epoch` in the new
arm and `%overlay-transaction` still only on the neither-axis arm (C6).
Everything downstream -- `:current`, `:at`/`:during`, the object-side
relation filter, `%paginate` -- is unchanged, because all three already
guard on `(or (reaped-claim-p c) ...)`.

### S5. `epoch-axis-unavailable` -- `spacetime/conditions` or `globals.lisp`

A `query-precondition-error` subtype with a `graph-name` reader (E9), so
the consumer can branch to the wall-clock axis rather than fail. Export
it and its reader.

---

## What the tests must pin

The fixture is the part most likely to be got wrong, so it comes first.

**F. A two-store, one-clock fixture.** No such fixture exists in
`tests/spacetime/`; `with-claim-graph`
(`tests/spacetime/claim-tests.lisp:16-24`) makes ONE graph and passes no
`:system-clock`. Build the new one from
`with-clock-system-dir` + the body of
`two-stores-on-one-clock-get-disjoint-ordered-epochs`
(`tests/system-clock-tests.lisp:9-20`, `:389-419`), which already does
every hard part: binds `graph-db::*system-directory*`, `*type-registry*`
and `*store-registry*`, opens the clock in its own temp dir, and closes
clock and graphs in the right order. Two claim FAMILIES are needed, not
one: `def-claim-classes` binds node types to a graph name and class
names are globally unique (`spacetime/claim.lisp:324-327`). Assert in
the fixture itself that `(eq (graph-system-clock g1) (graph-system-clock g2))`
-- otherwise a fixture that silently fails to attach makes every test
below pass for the wrong reason.

Then:

1. **Epochs are one sequence across the two stores.** Commit in A, B, A;
   assert the three `transaction-id`s are distinct and strictly
   increasing, and that each created claim's `claim-commit-epoch` equals
   its own transaction's id. Pins C1 and E2 together. Assert the ids, not
   just their distinctness -- see the reasoning in
   `no-clock-means-per-store-counters-unchanged`'s comment
   (`tests/system-clock-tests.lisp:425-430`).

2. **A claim whose versions straddle E.** Create at `e1`, update the
   validity extent at `e2 > e1`. Assert `:as-of-epoch e1` returns the
   version with the OLD extent, `:as-of-epoch e2` the new one, and
   `:as-of-epoch (1- e2)` the old one. The third assertion is the one
   that catches C2; without it a `<` implementation passes the first two.

3. **A retraction at E+1.** Create at `e1`, retract at `e2`. Assert
   `:as-of-epoch e1` returns the claim, `:as-of-epoch e2` returns NIL,
   and `:as-of-epoch (1- e2)` returns the claim. Then assert
   `(claim-commit-epoch <live>)` equals the RETRACTING transaction's id,
   which pins E4's "the retraction is a version and its epoch is that
   version's epoch" -- the fact the whole filter rests on.

4. **A reaped version.** A family declared `:keep-revisions 1` (the
   `kr-claim` pattern, `tests/spacetime/claim-tests.lisp:232`), created
   at `e1`, updated twice. Assert `:as-of-epoch e1` returns a
   `reaped-claim` with the right id -- **and, as a control in the same
   test, that a claim created AFTER `e1` in the same store returns NIL
   rather than a `reaped-claim`.** Without that control the test passes
   against an implementation that reports `reaped-claim` unconditionally,
   which is the exact defect C4 predicts. This pair is the non-vacuity
   proof for the `revision` discriminator; nothing else in the suite
   distinguishes the two.

5. **A clockless store.** `make-graph` with `*system-clock*` bound NIL
   and no `:system-clock`. Assert `:as-of-epoch` signals
   `epoch-axis-unavailable`, that `:as-of` on the SAME store still works
   (the refusal must be axis-scoped, not store-scoped), and that
   `claim-commit-epoch` on a claim there still returns an integer -- the
   store has a local counter, and part 1's reader is not the thing that
   refuses. Bind `*system-clock*` explicitly rather than relying on the
   default, for the run-order reason
   `tests/system-clock-tests.lisp:428-430` gives.

6. **`:as-of` and `:as-of-epoch` are exclusive**, and passing both
   signals rather than silently preferring one.

7. **The index-membership bound is stated, and pinned.** Create at `e1`,
   `mark-deleted` at `e2`; assert `:as-of-epoch e1` does NOT return it,
   with a comment naming #345 and `docs/rules.md`. A test that documents
   a limit is cheaper than the bug report from the consumer who assumed
   otherwise.

8. **`split-claim-identity-key` is unchanged.** The existing #321 tests
   suffice; no new test, but the plan should say out loud that part 3 is
   a no-op so nobody "implements" it.
