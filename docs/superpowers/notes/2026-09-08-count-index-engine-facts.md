# Engine API facts, verified for the counting index (#361)

A snapshot, not a maintained document. The recon pass for
kraison/vivace-graph#361 (`def-count-index`, spec
`docs/superpowers/specs/2026-09-08-count-index-design.md`):
thirty-three numbered facts, each settled by reading the cited line in
this worktree. Its model is the #350 note
(`docs/superpowers/notes/2026-09-07-claim-vocabulary-engine-facts.md`),
which is still true at this HEAD except where §X below says otherwise;
facts it already settled are cited by reference rather than re-quoted,
with their line numbers re-verified.

**Pinned to `4a2e645`** (`feat/count-index`, the spec commit, on
`experiment` `837b460`). Every `file:line` will drift; the quoted forms
are what to match on, not the numbers. Paths are relative to the repo
root.

**No image was run.** Every item is `source` evidence, read at the line
cited. Where a behaviour is only implied by the code (not stated in
it), the item says so in its own words.

Reading order:

- **§X — spec assumptions that do not hold.** Eight. X1, X2, X4, X5 and
  X6 each produce a plausible-looking wrong implementation if missed;
  X3 changes a ruling's *reason*, not its conclusion; X7 and X8 are
  correctness holes the spec does not mention at all.
- **§A — the spec registry and its kinds** (A1–A5).
- **§B — the ordered map: keys, values, in-place update** (B6–B11).
- **§C — maintenance dispatch and every caller** (C12–C16).
- **§D — persistence and open** (D17–D22).
- **§E — spacetime** (E23–E28).
- **§F — tests and runners** (F29–F33).
- **§G — traps.**

Line lengths here exceed the repo's 80-column rule inside quoted forms
and tables. Left as verified rather than rewrapped by hand, which risks
silently corrupting a quoted form. Every code line is byte-for-byte;
the only edits are `...`, standing either for lines dropped from a
form's middle or for the tail of a subform that is not the point of the
quote.

---

# §X — spec assumptions that do not hold

## X1 (HEADLINE) — a count index cannot live in the secondary registry, at either level

**Spec assumes** (§2.1): "it registers an `index-spec` of kind
`:count` in the same schema registry under the same named identity
(`%spec-identity`) ... builds now when the graph is open and at open
otherwise (`install-secondary-indexes`)".

**Reality: neither the *declaration* table nor the *built-object* table
can hold both without a silent wrong answer.**

*Declaration table.* `*schema-index-metadata*` (`index.lisp:87-89`) is
read by four functions that ask nothing about a kind:

- `%applicable-index-descriptors` (`index.lisp:213-225`) turns every
  registered spec into a `(slot-names owner canonicalize)` descriptor;
- `class-secondary-index-descriptors` (`index.lisp:227-247`) is "the
  single input to maintenance (apply / rebuild)" — its own docstring;
- `%index-spec-declared-p` (`index.lisp:190-211`) — the sidecar
  reconciliation;
- `install-secondary-indexes` (`index.lisp:874-880`) → `%ensure-index-built`
  → `%build-index-for-spec`.

So a `:count` spec dropped into that table is *also* built and
maintained as an ordinary ordered secondary index — the exact double
cost R6 withdraws `claim-relation` to avoid.

*Built-object table.* `(secondary-indexes graph)` is keyed
`(owner . slot-names)` — `%slot-index-for` (`index.lisp:559-583`),
`%secondary-index-lookup` (`index.lisp:929-945`), `%ensure-index-built`
(`index.lisp:868-872`), `restore-secondary-index-roots`
(`index.lisp:781-782`). The family declares **both** a secondary index
and a count index on `(subject-namespace subject-key)` owned by the
parent class (spec §3.1 vs `spacetime/claim.lisp:440-441`), so both
resolve to the **same hash key** `(ct-claim subject-namespace subject-key)`.
`%slot-index-for`'s `(or (gethash key reg) ...)` would hand the count
maintenance the secondary index's skip-list.

And `save-secondary-index-roots` (`index.lisp:673-691`) maphashes that
same table and builds each record from the `slot-index` struct's own
fields, so a count index sitting there is saved and reopened *as a
secondary index*, with `%index-comp-lessp` and `%index-key-serialize`
(see X4, X5).

**The engine's own precedent is a parallel registry per kind.**
`def-unique` does not share `def-index`'s table: `*schema-unique-metadata*`
(`unique-constraint.lisp:270-273`, whose docstring says "mirroring
*SCHEMA-INDEX-METADATA* (index.lisp)"), a parallel struct
`unique-tuple-spec` (`:275-276`) with the same slots plus `scope`, and
parallel `register-`/`unregister-unique-tuple-spec` (`:285-296`,
`:298-`). The two share exactly one thing — `%spec-identity` — and
`unique-tuple-spec-identity` says so (`unique-constraint.lisp:277-283`):

```lisp
(defun unique-tuple-spec-identity (spec)
  "See %SPEC-IDENTITY (index.lisp).  The two registries share ONE identity rule
on purpose -- GH #140: \"the two registries want one mechanism, not two\"."
```

Same for the built objects: `unique-indexes`, `spatial-indexes` and
`secondary-indexes` are three separate graph slots
(`graph-class.lisp:171`, `:132`, `:203`).

**Consequence for the plan.** `def-count-index` wants its own
`*schema-count-metadata*`, its own `count-index-spec` struct (sharing
`%spec-identity`), its own graph slot (`count-indexes`), and its own
build / install / save / restore quartet. A `kind` slot on
`index-spec` would require editing every one of the four readers above
to filter on it, and the `(owner . slot-names)` collision would still
have to be solved separately.

## X2 — `update-in-skip-list` is not an upsert on the memory backend

**Spec assumes** (§2.3): "Counters update in place through the
backend's update path (`update-in-skip-list`)".

**Reality: the three backends disagree about an absent key.**

- Heap skip list (`skip-list.lisp:732-781`) — upserts: the `if node`
  has an else arm `(return-from update-in-skip-list (add-to-skip-list skip-list key value))`
  (`:779-780`).
- B+ tree (`bplus-tree.lisp:964-968`) — upserts, by remove-then-add:

```lisp
(defmethod update-in-skip-list ((tree bplus-tree) key value &optional old-value)
  (declare (ignore old-value))
  ;; Duplicate-free composite keys: replace = remove + add.
  (bpt-remove tree key)
  (add-to-skip-list tree key value))
```

- Memory skip list (`mem-skip-list.lisp:207-217`) — **does not**:

```lisp
(defmethod update-in-skip-list ((sl mem-skip-list) key value &optional old-value)
  "In-place update of the value under KEY (nodes are live, so just SETF the
slot).  Returns the node, or NIL if KEY is absent."
  (declare (ignore old-value))
  (with-write-lock ((mem-skip-list-lock sl))
    ...
        (when (and (not (eq node (mem-skip-list-tail sl)))
                   (funcall (mem-skip-list-key-equal sl) key (%sn-key node)))
          (setf (%sn-value node) value)
          node)))))
```

The docstring states the divergence outright. On a memory graph the
**first** increment of every name would silently do nothing and return
NIL. The maintenance must read-then-branch (`add-to-skip-list` when
absent, `update-in-skip-list` when present) on every backend, not lean
on an upsert.

## X3 — the counters *are* serialised, but not by "the backend's write lock"

**Spec assumes** (§2.3): "the maintenance takes no lock of its own, the
backend's write lock covers the read-modify-write per key."

**Reality: on SBCL there is no backend write lock at all**, and what
actually serialises the read-modify-write is the transaction manager.

`with-sl-write-lock` expands to `(progn ...)` on every implementation
but ECL (`skip-list.lisp:45-47`):

```lisp
(defmacro with-sl-write-lock ((skip-list) &body body)
  #+ecl `(with-write-lock ((%sl-lock ,skip-list)) ,@body)
  #-ecl `(progn ,@body))
```

And a *read* then a *write* are two separate public boundaries anyway,
so even on ECL the pair is not atomic.

What is true: `%commit` (`transactions.lisp:3570`) takes the manager
lock at `:3586` and calls apply inside it at `:3641`, with a comment
that names this exact property (`:3634-3641`):

```lisp
               ;; apply-transaction must run inside the manager lock so that:
               ;; (a) applies happen in tx-id order (no concurrent-apply race
               ;;     where a lower-tx-id apply overwrites a higher-tx-id apply);
               ;; (b) the graph cache is updated before the lock is released, so
               ;;     any transaction created after this commit (start-tx-id >
               ;;     this tx-id) reads the committed value rather than a stale
               ;;     pre-apply snapshot.
               (apply-transaction tx (graph tx)))
```

`call-with-transaction-manager-lock` is `with-recursive-lock-held` on
`(lock transaction-manager)` (`transactions.lisp:3154-3157`) — one lock
per store. `apply-transaction`'s own `with-transaction-lock`
(`transactions.lisp:1982`) is **not** a serialiser: `transaction-lock`
is a per-`tx` slot with `:transaction-lock (make-rw-lock)` in the class
default-initargs (`transactions.lisp:494-496`, `:552`), so every
transaction holds a different one.

The two replication apply paths do not take the manager lock; they get
their serialisation from the device's single writer thread
(`peer-writer-loop`, `peer-streaming.lisp:1477-1479`: "The device's
single writer (WP-8) ... All device graph mutations funnel through
here").

**Consequence.** The ruling stands — a counter is not raced between
commits of one store — but the plan must say *why* (the manager lock /
the single writer), not "the backend's write lock", and must not
promise anything for a caller that mutates a count map outside those
two funnels.

## X4 — `%index-comp-lessp` / `%index-equal` cannot order a depth-first key

**Spec assumes** (§2.2): "within a depth the order is the index
collation (`%index-comp-lessp` without the id tie-break)."

**Reality: no such variant exists, and reusing the id-position pair is
either a crash or a silent key merge.** `%index-comp-lessp`
(`index.lisp:318-330`) special-cases the last shared position:

```lisp
    (loop for a in key1
          for b in key2
          for i from 0
          do (if (and (= i (1- n1)) (= i (1- n2)))
                 (return (key-vector< a b))
                 (cond ((less-than a b) (return t))
```

`key-vector<` (`utilities.lisp:352-359`) opens with
`(array-dimension v1 0)` and then `(< (aref v1 0) (aref v2 0))`, so on
a count key `(1 :ns)` vs `(1 :other)` it type-errors on the keyword,
and on `(1 "ns")` vs `(1 "other")` it type-errors on the character.
Loud, at least.

`%index-equal` (`index.lisp:332-341`) is the silent half: the last
position is compared with `EQUALP`, so `(1 "at")` and `(1 "AT")` are
**one key** and their counters merge.

The id-free order already exists and is otherwise unused outside
`ix-map`'s open-ended filter — `%index-value-lessp`, `index.lisp:343-355`:

```lisp
(defun %index-value-lessp (a b)
  "Lexicographic order for two VALUE-only component lists (no trailing id), by
LESS-THAN per component; a strict prefix sorts before its extension.  Used by
IX-MAP's open-ended range filter, where there is no id to anchor the
%INDEX-COMP-LESSP last-position special case (GH #107)."
  (loop for x in a
        for y in b
        do (cond ((less-than x y) (return t))
                 ((equal x y))
                 (t (return nil)))
        finally (return (< (length a) (length b)))))
```

Its matching equality does **not** exist and must be written (plain
`EQUAL` on the whole list is the right one — `%index-equal`'s
per-component test for the non-last positions).

Sentinels: with `%index-value-lessp` a **one-element** head `(:gmin)`
and tail `(:gmax)` are correct for a depth-first key, because `less-than`
resolves `+min-sentinel+` / `+max-sentinel+` against a number first
(`utilities.lisp:252-277`) and the length tie-break never fires. Do not
reuse `%index-head-key` / `%index-tail-key` (`index.lisp:256-262`):
those append `+null-key+` / `+max-key+` for the id slot the count key
does not have.

## X5 — the index key codec cannot be reused either

**Spec assumes** (implicitly, §2.2, by putting the count map "on the
graph's index backend like a secondary index").

**Reality:** `make-secondary-skip-list` (`index.lisp:272-296`) passes
`:key-serializer '%index-key-serialize :key-deserializer '%index-key-deserialize`,
and `%index-key-serialize` (`index.lisp:380-389`) splices the last
element in **raw, as a 16-byte id**:

```lisp
  (let ((vals (butlast key)))
    (if (= (length vals) 1)
        (coerce (view-key-serialize key) '(simple-array (unsigned-byte 8) (*)))
        (let ((id (car (last key))))
          (concatenate '(simple-array (unsigned-byte 8) (*))
                       id (list +index-tuple+) (serialize vals))))))
```

On a count key the last element is a value, not a byte vector; and the
one-element sentinel `(:gmin)` makes `vals` empty, so it takes the
tuple branch with a keyword where the id belongs. `%index-key-deserialize`
(`:391-403`) mirrors the same assumption (it copies 16 bytes off the
front).

`SERIALIZE` / `DESERIALIZE` work directly and are the right pair:
`(deserialize array)` returns `(values object total-length)`
(`serialize.lisp:118-126`), which is exactly what `read-skip-node`
consumes to find the value's offset (`skip-list.lisp:307-311`), and
`serialize` on a list writes `+list+`-tagged bytes with a
`(unsigned-byte 8)` element type (`serialize.lisp:597-628`).

**The counter pair round-trips.** A dotted pair takes `serialize`'s
`+dotted-list+` branch (`serialize.lisp:612-628`) and comes back
through `deserialize-help ((become (eql +dotted-list+)) ...)`
(`serialize.lisp:588-595`). With CURRENT-P NIL, `(n . NIL)` is just the
proper list `(n)`, so `car`/`cdr` still read correctly. `make-heap-index`
already gives every backend `:value-serializer 'serialize
:value-deserializer 'deserialize` (`bplus-tree.lisp:1036-1037`,
`:1051-1052`), so no value-codec work is needed on either on-disk
backend, and the memory backend stores plain Lisp objects with no codec
at all (`index.lisp:288-296`).

## X6 — `peer-purge-node` is a fourth maintenance call site, and it bypasses the generic

**Spec assumes** (§2.3): "A fourth method set on
`apply-tx-write-to-secondary-indexes`, so the commit apply and the two
replication apply paths inherit it."

**Reality: there is a third replication path that does not go through
the generic at all.** `peer-purge-node` (`peer-streaming.lisp:1430-1445`)
calls `%ix-release` directly:

```lisp
  ;; General ordered index: release the purged node's index entries too (guarded, so
  ;; a graph with no secondary indexes pays nothing).
  (when (secondary-indexes graph)
    (%ix-release node graph))
```

reached from `apply-peer-purge` (`:1467-1475`) and thence from the
writer loop's `:purge` op (`:1510`). A count index maintained only by a
new method on `apply-tx-write-to-secondary-indexes` never decrements
here: counters drift permanently high and R4's "the map's keys are then
exactly the names with a committed claim" stops holding for any device
that purges. `tests/peer-index-tests.lisp:88-99` and `:135-149` pin the
secondary-index half of this behaviour and are the template for the
count-index half.

## X7 — a counter is not idempotent under re-apply, and two paths deliberately re-apply

Not addressed by the spec at all. `ix-put` is a *set* insert:
`add-to-skip-list` on a duplicate-free list returns NIL for a duplicate
key rather than inserting or signalling — heap `skip-list.lisp:683-688`
("ATTEMPT TO INSERT DUP KV ... (return-from add-to-skip-list nil)"),
memory `mem-skip-list.lisp:89-101` ("a second identical key is a no-op
returning NIL (matching the on-disk list's non-erroring behaviour)").
So applying one `tx-create` twice leaves one entry — and would leave a
counter at 2.

Two paths re-apply on purpose, and both announce it by binding
`*add-to-indexes-unless-present-p*` (`transactions.lisp:38-42`: "Needed
when potentially recovering from a transaction multiple times"):

1. **Crash recovery.** `recover-transactions` (`transactions.lisp:3723-3733`)
   binds it T and replays every `.txn` through `apply-transaction`,
   which runs `apply-tx-writes-to-secondary-indexes`
   (`transactions.lisp:2008`).
2. **Device state-sync pull.** `apply-peer-create-writes`
   (`peer-streaming.lisp:1127-1142`) binds it T — docstring:
   "Idempotent upsert by node UUID: *ADD-TO-INDEXES-UNLESS-PRESENT-P*
   makes a refresh re-pull safe."

For (1) there is an accident that saves the counters today, and the
plan must not rely on it silently: at open, `recover-transactions` runs
at `graph.lisp:995`, **before** `restore-secondary-index-roots` at
`:1051`. The replay creates fresh registry entries via `%slot-index-for`
(the registry starts NIL, `graph-class.lisp:203`), and the restore then
**replaces** each declared record's entry outright
(`index.lisp:781-793`, `(setf (gethash (cons owner slot-names) reg) (%make-slot-index ...))`).
So the replay's index writes are discarded, not doubled — which for a
counter means a crash-recovery open leaves the *pre-crash sidecar's*
values, missing the replayed tail, until something rebuilds. There is
no `(when crash-recovery-p (rebuild-secondary-indexes graph))` the way
there is for spatial (`graph.lisp:1043-1044`).

For (2) nothing saves the counters: a refresh re-pull of the same node
increments twice.

## X8 — deleting the `def-index` form is not a withdrawal

**Spec assumes** (R6, §3.1): "no longer declares the `claim-relation`
secondary index ... An existing family drops that record at open (the
reconciliation of #147)."

**Reality: the reconciliation only fires when the spec is *unregistered*,
and removing the macro's form does not unregister anything in an image
that already ran the older macro.** `%index-spec-declared-p`
(`index.lisp:190-211`) answers T while a matching spec is still in
`*schema-index-metadata*`, and nothing removes it: `def-claim-classes`
emits `def-index` forms (`spacetime/claim.lisp:440-453`), never an
`undef-index`. The macro's own ⚠ block says this is the failure mode
(`spacetime/claim.lisp:412-421`):

```lisp
       ;; ⚠ EVERY DECLARATION THIS MACRO EMITS IS NAMED -- the value
       ;; constraint above included (GH #139, #140, #149).  This macro
       ;; emits schema on a tenant's behalf, and a LATER VERSION OF IT
       ;; CANNOT NAME WHAT AN EARLIER VERSION EMITTED.  Unnamed, identity
       ;; is (owner . slot-names), so changing what is declared here would
       ;; leave BOTH the old and the new spec live in every long-lived
       ;; image -- the stale unique rejecting writes the current schema
       ;; permits, the stale index built and maintained for nothing.
```

In a **fresh** image the spec is simply never registered, the class arm
of `%index-spec-declared-p` finds no `:index` slot option on `relation`
(`ct-claim`'s slots are plain, `spacetime/claim.lisp:148-184`), and the
record is dropped and its pages reclaimed — R6 works. In a **long-lived**
image that loaded the old macro first, the record is kept and the index
is maintained forever.

The fix is one emitted form. Do **not** use `undef-index`: it routes
through `%withdrawn-p` (`index.lisp:132-144`), which warns
`schema-withdrawal-matched-nothing` whenever nothing matched — i.e. on
every fresh image. Use the silent boolean the macro layer wraps,
exported at `package.lisp:465`:

```lisp
(graph-db:unregister-index-spec ',parent ',graph-name :name 'claim-relation)
```

`unregister-index-spec` (`index.lisp:164-175`) returns T or NIL and its
docstring says "Withdrawing something never declared is a no-op, not an
error: a macro that clears before declaring must not have to know
whether it ran before." The `:name` symbol must be the one the old
macro interned — `GRAPH-DB.SPACETIME::CLAIM-RELATION`, i.e. written
unqualified in `spacetime/claim.lisp` as it is today.

---

# §A — the spec registry and its kinds

## A1 — `index-spec`: every slot, and there is no kind

`index.lisp:96-97`, the whole struct:

```lisp
(defstruct (index-spec (:constructor make-index-spec))
  owner-name slot-names graph-name canonicalize name)
```

Five slots, no kind, no arity. `make-index-spec` is the constructor
`def-index` calls (`index.lisp:882-904`). The table it lands in,
`index.lisp:87-89`:

```lisp
(defvar *schema-index-metadata* (make-hash-table)
  "graph-name (symbol) -> list of INDEX-SPECs (newest first): the declarative
DEF-INDEX registry, reconciled at open by INSTALL-SECONDARY-INDEXES.")

```

Keyed **by graph name** (a symbol/keyword), `:test` default `EQL`. Not
by class, not by kind.

`%spec-identity`, `index.lisp:99-115` (docstring elided in the middle):

```lisp
(defun %spec-identity (owner-name slot-names name)
  "A declaration's identity: (OWNER . NAME) when named, (OWNER . SLOT-NAMES)
otherwise (GH #139, #140).
...
Unnamed declarations keep slot-name identity, so everything written before this
change behaves as it did: two unnamed indexes on one owner with different slot
lists are two DIFFERENT indexes and both stay live, which is the legitimate
multi-index case.  A name is a symbol and SLOT-NAMES a list, so the two
identity spaces cannot collide under EQUAL."
  (cons owner-name (or name slot-names)))
```

`index-spec-identity` (`:146-148`) is the wrapper.
`register-index-spec` (`:150-162`) **replaces in place** on identity
match, `unregister-index-spec` (`:164-175`) removes on the same key,
`%registered-index-specs` (`:177-188`) de-dupes newest-wins.

**A named count spec would not collide with `claim-subject` by
identity** (`claim-subject-count` ≠ `claim-subject`) — but see X1: the
identity is not the problem, the four kind-blind readers of the table
are.

## A2 — `def-unique` proves the parallel-table pattern

Quoted at X1. The three facts the plan needs, restated:

| | `def-index` | `def-unique` |
|---|---|---|
| declaration table | `*schema-index-metadata*` (`index.lisp:87`) | `*schema-unique-metadata*` (`unique-constraint.lisp:270`) |
| spec struct | `index-spec` (`index.lisp:96`) | `unique-tuple-spec` (`unique-constraint.lisp:275`), same slots + `scope` |
| identity | `%spec-identity` (`index.lisp:99`) | `%spec-identity`, via `unique-tuple-spec-identity` (`unique-constraint.lisp:277`) |
| built-object slot | `secondary-indexes` (`graph-class.lisp:203`) | `unique-indexes` (`graph-class.lisp:171`) |
| withdrawal warning | `%withdrawn-p` (`index.lisp:132`) | `%withdrawn-p`, shared |

Two registries, one identity rule, one warning helper. A third for
`:count` follows the same shape and needs no new mechanism.

## A3 — how a query finds a built index, and where a count map's would go

The chain, all in `index.lisp`, all unchanged since the #350 note
(fact A1 there):

- `%secondary-index-lookup` (`:929-945`) — `(gethash (cons class-name slot-names) reg)`,
  then a walk **up** `class-precedence-list` so an ancestor's index
  covers a subclass.
- `%slot-index-declared-p` (`:947-960`) — MOP `:index` slot options,
  **arity 1 only**.
- `%def-index-declared-p` (`:962-971`) — a registered spec whose
  `slot-names` match and whose owner is `class-name` or an ancestor
  (`subtypep`).
- `%require-index` (`:973-988`) — the three-way answer: the
  `slot-index`; NIL for *declared but empty*; `query-precondition-error`
  for *not indexed at all*.

`%slot-index-for` (`:559-583`) is the get-or-create, and it is where the
collision bites:

```lisp
    (let* ((reg (or (secondary-indexes graph)
                    (setf (secondary-indexes graph) ...)))
           (slot-names (%normalize-slots slot-name))
           (key (cons owner-name slot-names))
           ...
      (or (gethash key reg)
          (let ((six (%make-slot-index ...)))
            (setf (slot-index-skip-list six)
                  (make-secondary-skip-list graph (length slot-names)))
            (setf (gethash key reg) six))))))
```

**The collision, concretely.** Spec §3.1 declares
`claim-subject-count` on the *parent* over `(subject-namespace subject-key)`;
`spacetime/claim.lisp:440-441` already declares `claim-subject` on the
parent over the same two slots. Key `(ct-claim subject-namespace subject-key)`
for both. Same for `claim-relation-count` vs the (withdrawn)
`claim-relation` on `(relation)`.

**A keying that avoids it:** a separate graph slot, `count-indexes`,
keyed the same `(owner . slot-names)` — no key change needed, and it
keeps `save`/`restore` able to maphash one table of one kind (X1). If a
single table is insisted on, the key must carry the kind
(`(list* :count owner slot-names)`), and then `%secondary-index-lookup`'s
CPL walk, `save-secondary-index-roots`'s maphash and
`%ensure-index-built`'s key must all be taught about it — four edits
against one new slot.

## A4 — `%index-spec-declared-p` fails safe *towards keeping*

`index.lisp:190-211`. The rule that decides whether a sidecar record
survives:

```lisp
⚠ FAILS SAFE TOWARDS KEEPING.  When the owner class cannot be found or is not
yet finalized we answer T, because dropping an index that IS declared is a
regression while keeping one that is not is merely today's behaviour.  Positive
evidence is required to drop, never the absence of evidence to keep.
```

For the count index this means the same predicate has to exist for the
count registry, or every count record is unconditionally kept (or
unconditionally dropped, which strands or rebuilds). It is also the
predicate X8 turns on.

## A5 — `%tuple-indexable-p` and the canonicalizer resolver, both reusable

`%tuple-indexable-p` (`index.lisp:446-454`) — verbatim, because §2.3
names it:

```lisp
(defun %tuple-indexable-p (node slot-names)
  "True unless some component of NODE's SLOT-NAMES tuple is a real geometry
value -- geometry is the spatial index's domain, not this one's.  Unlike
%INDEXABLE-VALUE-P, a null component does NOT fail this gate: a tuple with a
null component is still indexed, via +NULL-COMPONENT+ in %INDEX-TUPLE-KEY
(GH #107)."
  (notany (lambda (s) (geometryp (slot-value node s))) slot-names))
```

Note it does **not** gate on all-null; `%index-tuple-key`
(`index.lisp:428-444`) is what returns NIL "only when EVERY component
is null". Spec §2.3's "A node that is not indexable (`%tuple-indexable-p`
false, every component null)" conflates the two gates: they are
separate, and the all-null one is `%index-tuple-key`'s.

`%resolve-index-canonicalizers` (`index.lisp:38-60`) turns a
`:canonicalize` spec into one function per slot; a positional list must
have exactly `arity` entries, anything else (T, NIL, a symbol, `#'fn`,
a `lambda` form) applies to component 0 only.
`%resolve-index-canonicalizer` (`:25-36`) resolves a bare symbol with
`(fdefinition spec)` — **eagerly**. A `:current-p` resolved the same way
would need its function defined at `%slot-index-for` time; storing the
symbol and `funcall`ing it at maintenance time is the safer shape (see
G13).

---

# §B — the ordered map: keys, values, in-place update

## B6 — `make-secondary-skip-list`: the two methods, verbatim

`index.lisp:272-296`:

```lisp
(defgeneric make-secondary-skip-list (graph arity)
  (:documentation "The ordered map backing a secondary index of ARITY value
components -- a flat (v1 ... vn id) composite under %INDEX-COMP-LESSP /
%INDEX-EQUAL.  Follows *INDEX-BACKEND* on an on-disk graph and returns a
MEM-SKIP-LIST on a memory-graph. ...")
  (:method ((graph graph) arity)
    (make-heap-index (graph-index-backend graph) (indexes graph)
                     '%index-comp-lessp
                     :head-key (%index-head-key arity)
                     :tail-key (%index-tail-key arity)
                     :key-equal '%index-equal
                     :key-serializer '%index-key-serialize
                     :key-deserializer '%index-key-deserialize))
  (:method ((graph memory-graph-mixin) arity)
    (make-mem-skip-list
     :key-equal '%index-equal
     :key-comparison '%index-comp-lessp
     :value-equal 'equal
     :head-key (%index-head-key arity)
     :head-value nil
     :tail-key (%index-tail-key arity)
     :tail-value nil
     :duplicates-allowed-p nil)))
```

The comment above it (`:264-271`) explains why it is its own generic
and not `make-view-skip-list`; a count map wants a third, for the same
reason.

`%open-secondary-skip-list` (`:298-316`) is the reopen half, calling
`open-heap-index` with the same four codec arguments.

## B7 — the value slot: what each backend stores, and that a cons is fine

`make-heap-index` (`bplus-tree.lisp:1014-1053`) hands **both** on-disk
backends the same value codec:

```lisp
  (ecase backend
    (:skip-list
     (make-skip-list
      :heap heap :duplicates-allowed-p nil
      :key-equal key-equal :key-comparison comparison
      ...
      :value-equal 'equal
      :key-serializer key-serializer
      :key-deserializer key-deserializer
      :value-serializer 'serialize :value-deserializer 'deserialize))
    (:bplus-tree
     (make-bplus-tree
      :heap heap :key-equal key-equal :key-comparison comparison
      :value-equal 'equal
      :key-serializer key-serializer :key-deserializer key-deserializer
      :value-serializer 'serialize :value-deserializer 'deserialize))))
```

`open-heap-index` (`:1054-1077`) repeats it. So the value slot is
"anything `SERIALIZE` round-trips", on both.

**What the existing consumers put there:**

- secondary index — **NIL**. `ix-put`, `index.lisp:500-505`: "the
  skip-node VALUE is unused -- store NIL, not the raw id byte array
  (which SERIALIZE cannot round-trip)".
- views — a real aggregate value, updated in place with
  `update-in-skip-list` (`views.lisp:398`, `:411`, `:527`).
- `:unique` — see `unique-constraint.lisp:137`, same `open-heap-index`.

**A cons of two integers round-trips.** `serialize ((list list))`
(`serialize.lisp:597-628`) has a `+dotted-list+` branch for an improper
list; `deserialize-help ((become (eql +dotted-list+)) ...)`
(`:588-595`) rebuilds it with `nconc`. `serialize ((int integer))`
(`:436-454`) is variable-width — 3 bytes up to 255, 4 to 65535 — which
matters for B8. With CURRENT-P NIL the pair `(n . NIL)` is the proper
list `(n)` and takes the ordinary `+list+` branch.

The memory backend stores plain Lisp objects (no serializer arguments
at all, `index.lisp:288-296`), so a cons is stored as itself.

On the B+ tree the value comes back through `%bpt-materialize`
(`bplus-tree.lisp:887-891`), which fabricates a `skip-node` so
`%SN-VALUE` reads the same on every backend.

## B8 — `update-in-skip-list`: signature, semantics, and the OLD-VALUE argument that decides the path

`skip-list.lisp:730-780`. Signature: `(skip-list key value &optional old-value)`,
a generic (`:730`). The heap method's structure:

```lisp
(defmethod update-in-skip-list ((skip-list skip-list) key value &optional old-value)
  (with-sl-write-lock (skip-list)
    (let ((lock nil))
      (let ((node (%find-in-skip-list skip-list key)))
        (if node
            ...
                   (let* ((skey ...)
                          (old-sval (funcall (%sl-value-serializer skip-list) old-value))
                          (sval (funcall (%sl-value-serializer skip-list) value)))
                     (if (<= (length sval) (length old-sval))
                         (progn
                           #+sbcl (sb-ext:cas (%sn-value node) (%sn-value node) value)
                           #+sbcl (sb-ext:cas (%sn-svalue node) (%sn-svalue node) sval)
                           ...
                           (let* ((offset (+ (%sn-addr node)
                                             8 1 1
                                             (length skey)
                                             (* (%sn-level node) 8))))
                             (dotimes (i (length sval))
                               (set-byte (%sl-heap skip-list) offset (aref sval i))
                               (incf offset))
                             ...))
                         (progn
                           ;; Release BEFORE remove+add ...
                           (unlock-skip-node skip-list lock)
                           (setq lock nil)
                           (remove-from-skip-list skip-list key)
                           (let ((new-node (add-to-skip-list skip-list key value)))
                             new-node)))))
              ...
            (return-from update-in-skip-list
              (add-to-skip-list skip-list key value)))))))
```

Three things the plan must know:

1. **OLD-VALUE is the in-place gate, not a CAS witness.** The comparison
   is `(<= (length sval) (length old-sval))` where `old-sval` is
   `serialize` of the *argument*. Omit it and `old-sval` is
   `(serialize NIL)` — short — so **every** counter update takes the
   remove+add path. `tests/skip-list-tests.lisp:205` says so in a
   comment: `(update-in-skip-list sl 2 222 20)   ; pass old-value to hit the in-place path`.
2. **The remove+add path defers a heap free until the graph closes.**
   `remove-from-skip-list` pushes the address onto `%sl-deferred-frees`
   (`skip-list.lisp:874-881`, "DEFER the free (GH #294)"), drained only
   by `close-skip-list` / `delete-skip-list`
   (`skip-list.lisp:473-495`). So a counter that crosses a
   serialization-length boundary (255→256, 65535→65536) leaks one
   skip-node's pages for the session. With OLD-VALUE passed this is
   rare; without it, it is one per increment.
3. **On an absent key: upsert on heap and B+ tree, no-op on memory** —
   X2.

The counter *read* before the update: `find-in-skip-list` is a generic
(`skip-list.lisp:614-617`) returning `(values node level preds succs)`;
`(%sn-value node)` is the value. The B+ tree method
(`bplus-tree.lisp:955-962`) materializes a `skip-node` for the same
accessor; the memory method (`mem-skip-list.lisp:195-205`) returns
`(values node 0 preds succs)` or `(values nil -1 ...)`.

## B9 — the locks actually taken, and the ECL nesting hazard

`with-sl-read-lock` / `with-sl-write-lock` are `(progn ...)` off ECL
(`skip-list.lisp:42-47`), and the file's own header states the
never-nest rule (`skip-list.lisp:34-40`). But
`update-in-skip-list`'s fallback path already nests: `:733` takes the
write lock, `:774` calls `remove-from-skip-list` (which takes it again
at `:795`) and `:775` calls `add-to-skip-list` (`:668`). A no-op on
SBCL — the platform this unit is gated on — and a pre-existing shape
the views path shares. Do not add a lock of your own on top; note that
a count index makes this path routine rather than rare.

The memory backend's locks are real on every platform:
`with-write-lock ((mem-skip-list-lock sl))` in `add-to-skip-list`
(`mem-skip-list.lisp:92`), `remove-from-skip-list` (`:114`),
`update-in-skip-list` (`:211`), and a read lock in `find-in-skip-list`
(`:196`) and `make-range-cursor` (`:176`). They are per-boundary, so a
read-then-update is still two acquisitions there too.

## B10 — duplicate-free insert semantics (why `ix-put` is idempotent and a counter is not)

`add-to-skip-list` on a duplicate-free heap list logs and returns NIL
(`skip-list.lisp:683-688`); the memory method says the same in its
docstring (`mem-skip-list.lisp:89-91`). `remove-from-skip-list` without
a VALUE argument reads the existing node's value first and then targets
that exact `(key,value)` pair (`skip-list.lisp:798-810`), so removing a
counter node by key alone works with a cons value. This is the fact
behind X7.

## B11 — range cursors: unchanged, one protocol, three backends

Fact A3 of the #350 note holds verbatim at this HEAD:
`make-range-cursor` / `cursor-next` are generics (`cursors.lisp:6-11`);
all three backends yield an object read with `%SN-KEY` / `%SN-VALUE`;
every caller passes `:eoc` and stops on `(eql node :eoc)`
(`index.lisp:496`, `:540`, `:547`, `:556`, `:1055-1057`, `:1131-1135`).
`map-index-prefixes` (`index.lisp:1086-1112`) and `index-count`
(`:1114-1136`) are the #350 additions and are the closest templates for
`map-count-index` and `count-index-lookup` — same argument order (FN
first, then graph), same `%require-index`-guard-on-NIL shape, same
`query-precondition-error` for an arity mistake.

---

# §C — maintenance dispatch and every caller

## C12 — the generic and its three methods, verbatim

`index.lisp:611-628`, unchanged from the #350 note (fact X1 there):

```lisp
(defgeneric apply-tx-write-to-secondary-indexes (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-create) graph)
  (%ix-claim (node write) graph))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-update) graph)
  (%ix-release (old-node write) graph)
  (unless (deleted-p (node write))
    (%ix-claim (node write) graph)))

;; tx-delete is a tx-update subclass; the node is marked deleted -> release only.
(defmethod apply-tx-write-to-secondary-indexes ((write tx-delete) graph)
  (%ix-release (old-node write) graph)
  (%ix-release (node write) graph))

(defun apply-tx-writes-to-secondary-indexes (writes graph)
  (dolist (write writes) (apply-tx-write-to-secondary-indexes write graph)))
```

Note the dispatch order trap the comment names: **`tx-delete` is a
subclass of `tx-update`** (`transactions.lisp:1016-1021`), so a
count-index method set must specialize all three, not two — a
`tx-update` method alone would fire for deletes.

Note also the `tx-delete` method releases **twice** (old and new). For a
set index that is idempotent (`ix-remove` of an absent key is a no-op).
For a counter it is a double decrement unless the count method
subtracts once.

## C13 — the write objects, and what is readable at apply

`tx-write` carries `node` (`transactions.lisp:1000-1003`); `tx-update`
adds `old-node` (`:1016-1020`); `tx-create` has only `node` (`:1014`).
`(id write)` is defined on `tx-write` (`:1006-1007`).

Where they come from:
- create — `%create-node` / `create-node` push a `tx-create`.
- update — `update-node` (`transactions.lisp:2969-3001`) pushes
  `(make-instance 'tx-update :node new-node :old-node old-node)` where
  NEW-NODE must be a `COPY` registered in `(copies *transaction*)`.
- delete — `delete-node` (`:3002-3028`) pushes a `tx-delete` whose NODE
  is a `%copy` with `deleted-p` T and NEW bytes copied from OLD.

**The data slots are readable at apply, with no `ensure-node-bytes`
call of your own.** `%index-tuple-key` reads `(slot-value node s)`
(`index.lisp:428-444`); `slot-value-using-class :around` on `node-class`
(`primitive-node.lisp:506-516`) routes a persistent slot to
`node-slot-value` (`:442-449`), which calls `maybe-init-node-data`
(`:300`) — the lazy deserialize. It defaults `:graph` to `*GRAPH*`,
which `apply-transaction` binds (`transactions.lisp:1989`); the
replication paths do **not** bind it (see C15), a pre-existing
asymmetry the count maintenance inherits rather than introduces.

`%ix-claim` / `%ix-release`, `index.lisp:589-609`, the shape a count
maintenance mirrors:

```lisp
(defun %ix-claim (node graph)
  "Index NODE's indexed slot values (create / new value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((slot-names (first d)))
      ;; Gate BEFORE %SLOT-INDEX-FOR so a geometry component never creates an
      ;; ordered index ...
      (when (%tuple-indexable-p node slot-names)
        (let* ((six (%slot-index-for graph d))
               (key (%index-tuple-key six node)))
          (when key (ix-put six key (id node))))))))

(defun %ix-release (node graph)
  "Remove NODE's indexed slot values (delete / old value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((slot-names (first d)))
      (when (%tuple-indexable-p node slot-names)
        (let* ((six (%slot-index-for graph d))
               (key (%index-tuple-key six node)))
          (when key (ix-remove six key (id node))))))))
```

## C14 — how the applicable indexes are found for a node's class

`class-secondary-index-descriptors` (`index.lisp:227-247`) is the single
entry, and its docstring says so ("This is the single input to
maintenance (apply / rebuild)"). It unions the MOP `:index` slots
(`class-indexed-slots`, `:68-79`) with
`%applicable-index-descriptors` (`:213-225`), de-duped by
`(owner . slot-names)`:

```lisp
(defun %applicable-index-descriptors (class graph)
  "(slot-names owner spec) descriptors from the DEF-INDEX registry applying to
CLASS: owner is CLASS or an ancestor (subtype IS-A) and every slot in
SLOT-NAMES exists in CLASS. ..."
  (when (class-finalized-p class)
    (loop for spec in (%registered-index-specs graph)
          for owner = (index-spec-owner-name spec)
          for slot-names = (index-spec-slot-names spec)
          when (and (subtypep (class-name class) owner)
                    (every (lambda (s) (%slot-present-p class s)) slot-names))
          collect (list slot-names owner (index-spec-canonicalize spec)))))
```

A count index finds its applicable declarations the same way, over its
own registry: `subtypep` on the owner plus every named slot present.
That `(every ... %slot-present-p ...)` is why declaring the object
index on the parent "also works" per the macro's comment
(`spacetime/claim.lisp:435-439`) — but see #350 fact X3: the *query*
side does not, and the same asymmetry applies to a count index's
lookup.

## C15 — every call site of the maintenance entry

Four, and one bypass:

| site | file:line | context |
|---|---|---|
| commit apply | `transactions.lisp:2008` | inside `apply-transaction`, post-durability, after unique, before `reap-old-versions`; `*graph*` bound (`:1989`), `*commit-epoch*` bound (`:1991`) |
| device state-sync pull | `peer-streaming.lisp:1140` | inside `apply-peer-create-writes` (`:1127`); `*add-to-indexes-unless-present-p*` T (`:1134`) |
| device authored pull | `peer-streaming.lisp:1177` | inside `apply-peer-authored-op` (`:1146`); same binding (`:1158`); writes are `final-writes`, post-merge-policy — a divergent vertex update becomes a **new** `tx-update` built off the local current version (`peer-merge-write`, `:1097-1122`) |
| crash recovery replay | `transactions.lisp:2008`, reached from `recover-transactions` (`:3723-3733`) | same generic; see X7 |
| **bypass** | `peer-streaming.lisp:1445` | `peer-purge-node` calls `%ix-release` directly — X6 |

`apply-transaction`'s ordering, `transactions.lisp:2004-2010`:

```lisp
        (apply-tx-writes-to-views writes graph)
        (apply-tx-writes-to-spatial-index writes graph)
        (apply-tx-writes-to-vector-segments writes graph)
        (apply-tx-writes-to-unique-indexes writes graph)   ; issue #6
        (apply-tx-writes-to-secondary-indexes writes graph) ; general ordered index
        (reap-old-versions writes graph)
        (persist-highest-transaction-id (transaction-id transaction) graph)))))
```

A count pass belongs immediately after the secondary pass, on the same
`writes` list.

## C16 — the replication receive path, before and after

`apply-peer-create-writes` (`peer-streaming.lisp:1127-1145`) — the
whole body around the call:

```lisp
  (let ((*commit-epoch* tx-id)
        (*add-to-indexes-unless-present-p* t)
        (*peer-apply-origin* origin))
    (apply-tx-writes writes graph)
    (apply-tx-writes-to-views writes graph)
    (apply-tx-writes-to-spatial-index writes graph)
    (apply-tx-writes-to-unique-indexes writes graph)   ; #6: keep the device index complete
    (apply-tx-writes-to-secondary-indexes writes graph) ; general ordered index
    (reap-old-versions writes graph)
    ;; Keep the device's tx-id-counter above this pulled node's hub epoch ...
    (peer-observe-epoch graph tx-id)))
```

`apply-peer-authored-op` (`:1146-1191`) is the same list over
`final-writes`, then per-field stamps, conflicts, the pull cursor and
the Lamport clock. **Same write objects** (`tx-create` / `tx-update` /
`tx-delete` instances), so a count method on the generic is inherited
by both — subject to X6 (purge) and X7 (re-pull).

Neither binds `*GRAPH*`; both run on the single writer thread
(`peer-writer-loop`, `:1477-1479`).

---

# §D — persistence and open

## D17 — the sidecar: file, record shape, no version constant

File name, `index.lisp:670-671`:

```lisp
(defun secondary-index-root-file (location)
  (format nil "~A/secondary-indexes.dat" location))
```

Writer, `index.lisp:673-691` — note it builds each record from the
`slot-index` struct, not from the hash key:

```lisp
(defun save-secondary-index-roots (graph)
  "Persist the on-disk secondary indexes' roots (owner slot-names address
backend-tag).  No-op with no heap (memory) or no indexes.  Called at
CLOSE-GRAPH.  The canonicalizer is NOT stored (a function is not
serializable); it is re-resolved from the owner class's live :INDEX spec on
reopen -- only the address+backend are needed to reopen the ordered map."
  (when (and (indexes graph) (secondary-indexes graph))
    (let ((roots '()))
      (maphash (lambda (k six)
                 (declare (ignore k))
                 (when (and (slot-index-skip-list six)
                            (view-index-p (slot-index-skip-list six)))
                   (push (list (slot-index-owner-name six)
                               (slot-index-slot-names six)
                               (view-index-address (slot-index-skip-list six))
                               (view-index-backend-tag (slot-index-skip-list six)))
                         roots)))
               (secondary-indexes graph))
      (%atomic-cl-store roots (secondary-index-root-file (location graph))))))
```

Reader's destructuring, `index.lisp:751-753`:

```lisp
                        (destructuring-bind (owner stored-slot address
                                             &optional (backend :skip-list)) r
```

**There is no format version constant** — `grep -n version index.lisp`
finds nothing in this region; the `&optional` on `backend` *is* the
versioning mechanism (a pre-B+-tree sidecar has three elements). So:

- **New build, old sidecar** — works, `backend` defaults.
- **Old build, new sidecar with a 5th element** — `destructuring-bind`
  signals, the `handler-case` at `:809-814` warns and returns NIL, and
  `open-graph` falls back to `rebuild-secondary-indexes`. Safe, slow,
  and it drops the whole file's records, not just the new one.

Given X1's parallel registry, the cleaner shape is a **separate
sidecar** (`count-indexes.dat`) with its own save/restore pair and its
own `%atomic-cl-store`, which leaves the existing file byte-compatible
in both directions.

`%atomic-cl-store` is the temp-plus-rename writer (#63); the
close-graph comment (`graph.lisp:1245-1252`) explains why these three
saves are deliberately unguarded: "a stale index root is silently
wrong."

## D18 — `restore-secondary-index-roots`: the reconciliation and the reclaim

`index.lisp:729-816`. The load-bearing middle, `:754-806`:

```lisp
                          (let ((slot-names (%normalize-slots stored-slot)))
                            ;; ⚠ RECONCILE AGAINST THE LIVE SCHEMA (GH #139,
                            ;; #140).  Maintenance is SPEC-driven ... while this
                            ;; restore is SIDECAR-driven. ...
                            (if (%index-spec-declared-p owner slot-names
                                                        graph)
                                (setf (gethash (cons owner slot-names) reg)
                                      (%make-slot-index
                                       :owner-name owner
                                       :slot-names slot-names
                                       :canonicalizers
                                       (%owner-slot-canonicalizer
                                        owner slot-names graph)
                                       :skip-list
                                       (%open-secondary-skip-list
                                        graph address
                                        (length slot-names)
                                        backend)))
                                ;; Withdrawn/re-shaped: this open is the
                                ;; safe moment -- nothing can be mid-read
                                ;; -- so reclaim the orphaned pages
                                ;; instead of stranding them (GH #147).
                                (handler-case
                                    (let ((stale (%open-secondary-skip-list
                                                  graph address
                                                  (length slot-names)
                                                  backend)))
                                      (delete-view-index stale)
                                      (log:info "reclaimed retired index ~
~A.~A (GH #147)" owner slot-names))
                                  (error (e)
                                    (warn "could not reclaim retired ~
index ~A.~A: ~A" owner slot-names e))))))))
```

Return contract (`:733-736`, `:746-748`): T if a sidecar was present and
readable — **including the empty case** — NIL to fall back to rebuild.
A partial restore is left in place on purpose (`:807-812`).

This is the mechanism R6 relies on to drop `claim-relation` — and X8 is
why it will not fire in a long-lived image without an explicit
`unregister-index-spec`.

## D19 — build now / build at open

`def-index` (`index.lisp:882-904`) registers, then builds immediately if
the graph is open:

```lisp
     (register-index-spec spec)
     (let ((g (lookup-graph ',graph-name)))
       (when g (%ensure-index-built g spec)))
     spec))
```

`%ensure-index-built` (`:868-872`) is idempotent on the registry key.
`%build-index-for-spec` (`:843-866`) scans, typed, skipping deleted
nodes:

```lisp
    (flet ((index-node (node)
             (unless (deleted-p node)
               (let ((key (ignore-errors
                           (and (%tuple-indexable-p node slot-names)
                                (%index-tuple-key six node)))))
                 (when key (ix-put six key (id node)))))))
      (if (subtypep owner 'edge)
          (map-edges #'index-node graph :edge-type owner)
          (map-vertices #'index-node graph :vertex-type owner)))
```

`install-secondary-indexes` (`:874-880`) is the open-time sweep over
`%registered-index-specs`. `rebuild-secondary-indexes` (`:647-668`) is
the untyped, whole-store scan used when there is no sidecar; it is
guarded by `%graph-has-indexed-slots-p` (`:634-645`) and skips deleted
nodes at `:654`. Both are the "apply the create rule per live node"
scan spec §2.3 asks for; note `%build-index-for-spec`'s
`ignore-errors` tolerance for a legacy node, which the count scan
should copy so one bad node does not abort an open.

`regenerate-secondary-indexes` (`:818-841`) is the backend-switch entry:
delete every index, `rebuild`, `install`, `save`. A count index needs
its own or an extension of this, or a backend switch silently keeps the
old count maps.

## D20 — `graph.lisp`'s open sequence, verbatim

`graph.lisp:1045-1057`:

```lisp
        ;; Unique constraints (issue #6): reopen the persistent unique skip-lists from
        ;; the sidecar (durable, no scan); only rebuild from nodes if there is no
        ;; sidecar -- a fresh graph, or a crash before CLOSE-GRAPH saved the roots.
        (unless (restore-unique-index-roots graph)
          (rebuild-unique-indexes graph))
        ;; General ordered indexes: same reopen-or-rebuild story as unique.
        (unless (restore-secondary-index-roots graph)
          (rebuild-secondary-indexes graph))
        ;; Build any def-index'd index not covered by the sidecar (declared before
        ;; this graph existed, or added since the last close); no-op otherwise.
        (install-secondary-indexes graph)
        ;; Same for a def-unique'd multi-slot constraint (GH #107).
        (install-unique-tuple-constraints graph))
```

Earlier in the same `let`: `recover-transactions` at `:995`,
`crash-recovery-p` captured at `:994`, the spatial re-derive at
`:1043-1044`. **The replay precedes the restore** — X7.

Close, `graph.lisp:1253-1254`, inside the `(when (graph-open-p graph))`
block that begins at `:1235`:

```lisp
      (save-unique-index-roots graph)
      (save-secondary-index-roots graph)
```

with the "deliberately NOT guarded" rationale at `:1246-1252`.

## D21 — the memory-graph equivalent

`memory-graph.lisp:1116-1125`:

```lisp
    ;; General ordered indexes: rebuild-on-open on the memory backend (image
    ;; persistence is a follow-up, mirroring unique's v1).  REBUILD covers the
    ;; MOP :INDEX slots; INSTALL covers DEF-INDEX declarations.  NOT on a LAZY
    ;; graph: rebuilding scans every node and would thus MATERIALIZE the
    ;; LZNODE blobs, defeating fault-on-access ...
    (unless (lazy-p graph)
      (rebuild-secondary-indexes graph)
      (install-secondary-indexes graph)))
```

So a memory graph has **no sidecar at all**: counters are rebuilt from
nodes at every open, and a `lazy-p` memory graph gets nothing built —
`count-index-lookup` must answer 0/0 there (the declared-but-empty
case), not signal. `docs/general-index-design.md:206-217` (§11a) is the
existing statement of that limitation.

## D22 — the withdrawn-declaration reclaim is pinned by a test

`tests/index-tests.lisp:889-921`
(`a-withdrawn-index-s-pages-are-reclaimed-at-open`) — the exact shape a
count-index withdrawal test copies: declare with `:name` on an open
graph, write a node, close, `undef-index`, then swap
`(fdefinition 'graph-db::delete-view-index)` to count calls, reopen,
close, and assert `(plusp freed)`. Note it uses a **dedicated class
with no `:index` slot options**, and says why: "the declared-p
predicate's class arm is graph-agnostic by design, so a shared class
would keep the record alive past the undef."

---

# §E — spacetime

## E23 — `def-claim-classes`: the five `def-index` forms at this HEAD

`spacetime/claim.lisp:435-453`, verbatim, with line numbers:

```lisp
435:       ;; Subject index on PARENT reaches both arities via SUBTYPEP.  Object
436:       ;; index on BINARY, where those slots live -- declaring it on PARENT
437:       ;; also works (%APPLICABLE-INDEX-DESCRIPTORS requires every named slot
438:       ;; to exist) but reads as a mistake.  PRODUCER index exists so the
439:       ;; regeneration sweep is not a full scan (design §4, plan note 2).
440:       (graph-db:def-index ,parent (subject-namespace subject-key)
441:           ,graph-name :name claim-subject)
442:       (graph-db:def-index ,binary (object-namespace object-key)
443:           ,graph-name :name claim-object)
444:       (graph-db:def-index ,parent (producer) ,graph-name
445:                           :name claim-producer)
446:       ;; (subject relation) queries ride this instead of filtering the
447:       ;; whole endpoint result caller-side (GH #302).
448:       (graph-db:def-index ,parent (subject-namespace subject-key
449:                                    relation)
450:           ,graph-name :name claim-subject-relation)
451:       ;; The vocabulary listing skips to distinct relations (GH #350).
452:       (graph-db:def-index ,parent (relation) ,graph-name
453:                           :name claim-relation)
454:       (fmakunbound ',(intern (format nil "MAKE-~A" parent) home))
```

| slots | owner | `:name` | line | R6 |
|---|---|---|---|---|
| `(subject-namespace subject-key)` | `,parent` | `claim-subject` | 440-441 | stays |
| `(object-namespace object-key)` | `,binary` | `claim-object` | 442-443 | stays |
| `(producer)` | `,parent` | `claim-producer` | 444-445 | stays |
| `(subject-namespace subject-key relation)` | `,parent` | `claim-subject-relation` | 448-450 | stays |
| `(relation)` | `,parent` | `claim-relation` | 452-453 | **removed**, plus X8's unregister |

`home` / `unary` / `binary`, `spacetime/claim.lisp:360-363`:

```lisp
  (let* ((home (symbol-package parent))
         (unary (intern (format nil "~A-UNARY" parent) home))
         (binary (intern (format nil "~A-BINARY" parent) home))
```

The `def-index` block sits inside the same `progn` as the two
`def-unique`s (`:422`, `:425`) and the `def-value-constraint`s (`:396`,
`:403`, `:432`); the three `def-count-index` forms go after `:453`,
before `fmakunbound` at `:454`, and every one must carry a `:name`
(the ⚠ block at `:412-421`).

## E24 — the vocabulary section: what stays and what goes

`spacetime/claim-query.lisp`, all of §"Vocabulary: what a family names"
(`:563-795`, the header comment at `:563-575`):

| lines | function | fate under this spec |
|---|---|---|
| 576-582 | `%refuse-vocabulary-axis` | **keeps** (§3.4: the two axes stay refused) |
| 584-594 | `%vocabulary-sources` | **keeps**, re-pointed at the count indexes (parent for subject, binary for object) |
| 596-602 | `%vocabulary-key` | **goes** — its whole job is "scalar at arity 1, tuple otherwise" for `map-index`/`index-count`; `map-count-index`'s prefix is always a list |
| 604-608 | `%vocabulary-view` | **keeps** (§3.3 needs the commit view) |
| 610-614 | `%view-resolve` | **keeps** for §3.4's as-of confirmation |
| 616-626 | `%claim-tuple` | **keeps** — §3.3 buckets the transaction's writes by name and this is how a claim's name tuple is read |
| 628-645 | `%created-under` | **keeps in shape**, but must return a *delta* (per-name ±all, ±current) rather than a list of claims |
| 647-659 | `%name-admitted-p` | **goes** — a name's existence is now the counter, not a resolution |
| 661-678 | `%name-count` | **goes** — replaced by the counter |
| 680-726 | `%walk-names` | **goes** — replaced by `map-count-index` at depth 1 / 2 |
| 728-733 | `%name-lessp` | **keeps** (`graph-db::less-than`; still the merge order) |
| 735-746 | `%merge-names` | **keeps** verbatim — merging two role lists and summing counts is unchanged |
| 748-766 | `claim-namespaces` | **keeps signature and docstring**, new body |
| 768-776 | `claim-relations` | same |
| 778-795 | `claim-keys` | same; `%paginate` (`:246-254`) still applied last |

`%paginate` and `%overlay-transaction` (`:246-254`, `:256-279`) are
outside the vocabulary section and untouched; §3.3 cannot reuse
`%overlay-transaction`, which returns a list of nodes, not a delta —
the same finding as the #350 note's B11.

## E25 — `claim-current-p` and what `retract-claim` writes

`claim-current-p`, `spacetime/claim-query.lisp:469-474`:

```lisp
(defun claim-current-p (claim)
  "True while CLAIM is still believed: its transaction period is open, or
absent -- a claim predating the axis was never retracted.  NIL once
RETRACT-CLAIM has closed the period (GH #162)."
  (let ((e (claim-transaction-extent claim)))
    (or (null e) (bound-unknown-p (extent-end e)))))
```

`retract-claim` (`:476-502`) — the write it produces, `:487-495`:

```lisp
  (flet ((%retract ()
           (let* ((c (graph-db:copy claim))
                  (e (claim-transaction-extent c))
                  (start (if e (extent-start e) (unknown-bound))))
             (setf (claim-transaction-extent-sexp c)
                   (extent->sexp (make-interval start (exact-bound at)
                                                :semantics :transaction
                                                :standing :asserted)))
             (graph-db:save c)
             c)))
```

`save` on a vertex is `update-node` (`interface.lisp:120-131`), which
pushes `(make-instance 'tx-update :node new-node :old-node old-node)`
(`transactions.lisp:2996-3000`). So **yes: retraction is exactly the
predicate-flip `tx-update`** spec §2.3 describes — the indexed tuple is
unchanged (namespaces, keys and relation are untouched), `old-node`
satisfies `claim-current-p` and `node` does not. `all` must not move;
`current` moves by −1.

`retract-claim` joins an ambient transaction and opens its own
otherwise (`:500-502`), and returns the *saved copy*; an
already-retracted claim comes back unchanged with no write at all
(`:500`, `((not (claim-current-p claim)) claim)`) — so a
retract-twice test produces one `tx-update`, not two.

**There is no hard delete of a claim** other than
`delete-claims-by-producer` (`:547-561`) → `graph-db:mark-deleted` →
`delete-vertex` → `delete-node` → `tx-delete` (see #350 note X1, still
true).

## E26 — `%as-of-snapshot`: signature and export state

`transactions.lisp:3544-3547`:

```lisp
(defun %as-of-snapshot (graph)
  "GRAPH's open as-of snapshot transaction, or NIL (GH #115)."
  (let ((s (and *read-snapshots* (gethash graph *read-snapshots*))))
    (and (typep s 'as-of-tx) s)))
```

**Not exported.** `grep -n as-of-snapshot package.lisp` finds nothing;
the only callers are `as-of-skipped-count` (`:3549-3553`) and, for
§3.4, spacetime code, which must write `graph-db::%as-of-snapshot`.
`latest-epoch` (`:3538-3542`) and `with-as-of` **are** exported and are
what the existing vocabulary test uses
(`tests/spacetime/vocabulary-tests.lisp:105-108`).

## E27 — the object index lives on BINARY (still)

Unchanged from the #350 note's X3: `claim-object` is declared on
`,binary` (`spacetime/claim.lisp:442-443`), everything else on
`,parent`. `%vocabulary-sources` (`claim-query.lisp:584-594`) already
threads it correctly and its docstring says why. Passing the parent for
the object slots **signals**, it does not return NIL —
`%def-index-declared-p` tests `(subtypep class-name (index-spec-owner-name spec))`
(`index.lisp:968-970`), i.e. `(subtypep 'ct-claim 'ct-claim-binary)` →
NIL, and `%require-index` then signals (`:984-988`). The count indexes
must inherit exactly the same owner split (spec §3.1's table already
does: `claim-object-count` on binary).

## E28 — the spacetime export site

`spacetime/package.lisp:53-54`:

```lisp
   #:claim-extent #:claims-touching
   #:claim-namespaces #:claim-relations #:claim-keys     ; GH #350
```

Nothing new is exported from spacetime by this unit (R5: the three
signatures do not change). The engine exports go on
`package.lisp:466-467`, which already reads:

```lisp
           #:index-lookup #:index-range #:map-index
           #:map-index-prefixes #:index-count            ; GH #350
```

`def-index` / `undef-index` / `unregister-index-spec` are at
`package.lisp:415`, `:417` and `:465`. A `#:def-count-index
#:undef-count-index` pair belongs beside `:415`, and
`#:count-index-lookup #:map-count-index` beside `:467`.

---

# §F — tests and runners

## F29 — `tests/index-tests.lisp`: fixtures, suite, reopen shape

- `(in-package #:graph-db/test)` at `:6`; graph names
  `*ix-graph-name*` `:8` and `*ix-di-graph-name*` `:75`, each with an
  `eval-when` clearing `graph-db::*schema-node-metadata*` (`:10-11`,
  `:77-78`).
- Suite, `:93-97`:

```lisp
(def-suite index-suite
  :description "General ordered secondary index (:INDEX / def-index)."
  :in graph-db-suite)

(in-suite index-suite)
```

- `with-ix-graph`, `:99-107` — note `:backend` is a macro keyword, so
  one test body runs on `:skip-list` or `:bplus-tree`:

```lisp
(defmacro with-ix-graph ((g &key (backend :skip-list)) &body body)
  "A fresh on-disk graph named *IX-GRAPH-NAME* on BACKEND, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ix-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :index-backend ,backend)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))
```

- `with-ix-di-graph` `:109-117` (def-index-only schema);
  `with-ix-memory-graph` `:774-781`:

```lisp
(defmacro with-ix-memory-graph ((g) &body body)
  "A fresh in-memory graph named *IX-GRAPH-NAME*, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (graph-db::make-memory-graph *ix-graph-name* (namestring ,dir))))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))
```

- **Classes already present**: `ix-person` (`:13-18`, MOP `:index`
  slots), `ix-employee` (`:22-24`), `ix-claim` with `(ns key rel)`
  (`:31-37`) **and** a second `(ns key)` index with a positional
  canonicalizer (`:56-57`), `ix-dual` (`:47-52`), `ix-solo` (`:64-68`).
  So `ix-claim` is the natural count-index owner and already carries two
  distinct secondary indexes — a count index on `(ns key)` would prove
  the X1 collision is solved.
- Reopen shape, `:540-554` (`reopen-restores-index`):
  `with-temp-directory` → `make-graph` → `unwind-protect` body →
  `close-graph` → `open-graph` → assert → `(ignore-errors (close-graph g))`
  → `(collect-garbage)`. Also at `:444-456`, `:581-609`, `:611-618`.
- **The build-at-open shape** spec §4 asks for
  ("done with a withdrawn open and close so the sidecar cannot restore
  it") is `:1035-1044`:

```lisp
(test def-index-declared-on-an-open-graph-indexes-existing-instances
  "GH #350 spec §3: a DEF-INDEX evaluated while the graph is open builds
over the instances already stored -- the relation index's no-migration
contract."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "late"))
    (def-index ix-claim (rel) :graph-db-index-test :name ix-late-rel)
    (unwind-protect
         (is (= 1 (length (index-lookup g 'ix-claim '(rel) "late"))))
      (undef-index ix-claim :graph-db-index-test :name ix-late-rel))))
```

  and the withdrawal/reclaim shape is `:889-921` (D22).
- Registered in `graph-db.asd:721` (`(:file "index-tests")`); a new
  `count-index-tests` file goes after it, and its suite must be `:in
  graph-db-suite`.

## F30 — the `%count-seeks` probe (GH #350), the template for a cost claim

`tests/index-tests.lisp:928-945`:

```lisp
(defvar *ix-seeks* nil
  "Seek counter for %COUNT-SEEKS; NIL when not counting.")

(defun %count-seeks (thunk)
  "Run THUNK counting MAKE-RANGE-CURSOR calls -- the one seek each hop
of the prefix walk makes -- through an :AROUND method removed afterwards.
Returns the count.  Callers prove the probe live with a control."
  (let* ((gf #'make-range-cursor)
         (method (eval '(defmethod make-range-cursor :around
                            ((index t) start end &key &allow-other-keys)
                          (declare (ignore start end))
                          (when *ix-seeks* (incf *ix-seeks*))
                          (call-next-method)))))
    (unwind-protect
         (let ((*ix-seeks* 0))
           (funcall thunk)
           *ix-seeks*)
      (remove-method gf method))))
```

Used with a control at `:977-996` — "Control: one prefix INDEX-LOOKUP
is one seek, so the probe fires. Ablation, recorded in the task report:
a linear hop makes the walk cost N+1 seeks and turns the second check
red."

For §4's "zero node resolutions" claim the same `:around` shape works
on `lookup-vertex`, which is a **generic** (methods at
`vertex.lisp:107` and `:112`), so an `fdefinition` swap will not catch
it. The one plain function on that path is `%node-by-id`
(`spatial-query.lisp:34-40`), declared `notinline` for exactly this
purpose:

```lisp
(declaim (notinline %node-by-id))
(defun %node-by-id (id graph)
  "Resolve a spatial-index id (uuid bytes) to its live node, or NIL."
  (or (lookup-vertex id :graph graph)
      (lookup-edge id :graph graph)))
```

Either works; pick one, keep a control, and prove the probe fired.

## F31 — `tests/spacetime/vocabulary-tests.lisp`

- `(in-package #:graph-db/spacetime-test)` `:4`, `(in-suite spacetime-suite)`
  `:6` — the spacetime suite is a **root** suite (`tests/spacetime/suite.lisp:5-6`,
  no `:in`), run by `run-spacetime-tests` (`:8-24`).
- Two helpers the file adds because `make-u` / `make-b` fix the
  namespace (`tests/spacetime/claim-identity-tests.lisp:7-18`):
  `%ns-u (ns subject &key relation producer)` `:8-12` and
  `%ns-b (ns subject ons object &key relation producer)` `:14-20`.
  Both use `:standing :inferred`.
- Ten tests, `:22`, `:40`, `:52`, `:73`, `:86`, `:99`, `:119`,
  `:148`, `:178`, `:197`. R5 makes every one of them the contract:
  **none may change.**
- The as-of test `:99-117` is the one §3.4 must keep honest; it already
  distinguishes "counts are entry counts, the documented live-membership
  bound" from "under `:current` each entry is resolved". Under the count
  index the `:counts t` numbers become *counter* values, which is the
  same answer for its data — check that assertion survives, and if the
  spec means it to change, R5 says it must not.
- The graph fixture is `with-claim-graph`
  (`tests/spacetime/claim-tests.lisp:16-24`), which **always makes a
  fresh graph** — a "built at open over pre-existing claims" test must
  spell out `make-graph` / `close-graph` / `open-graph` by hand; the
  shape to copy is `tests/spacetime/claim-query-tests.lisp:64-85`.
- New spacetime test files go in `graph-db.asd:598-615`, after
  `(:file "vocabulary-tests")` at `:615`.

## F32 — `tests/peer-index-tests.lisp`: not two peers, one device plus hand-built ops

The file is 149 lines and sets up **one** device graph; there is no
second peer and no socket. The pieces:

- suite `peer-index-suite` `:in graph-db-suite` (`:8-12`) — **this is
  the name** spec §4's replication bullet should target.
- `*pi-graph-name*` `:14`, `*pi-remote-origin*` `:15`, the
  `*schema-node-metadata*` clear `:17-18`.
- classes `pi-item` (`:20-23`, MOP `:index` on `sku`) and `pi-claim`
  (`:28-33`) with `(def-index pi-claim (ns key rel) ...)` at `:35`.
- the fixture, `:37-45`:

```lisp
(defmacro with-pi-device ((g) &body body)
  "An on-disk DEVICE peer-graph named *PI-GRAPH-NAME* bound to G and *graph*."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *pi-graph-name* (namestring dir)
                           :peer-role :device :origin-id (id16 3)
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))
```

- `pi-authored-create` (`:47-64`) builds a `peer-op` with one
  `tx-create` over a `%make-vertex` (not `make-instance`; the docstring
  points at `tests/peer-unique-tests.lisp`'s equivalent for the GH #135
  reason).
- **How a write is "streamed and awaited": it is not.** Each test calls
  the apply function directly and synchronously —
  `(graph-db::apply-peer-authored-op g op)` (`:69`, `:105`),
  `(graph-db::apply-peer-create-writes g 7777 (list write) origin)`
  (`:84`, `:127`), `(graph-db::apply-peer-purge g (list nid))`
  (`:96`, `:146`) —
  then asserts with `index-lookup`. So a count-index replication test
  is: build the op, apply it, read `count-index-lookup`, compare with
  the same counter on a locally-committed graph. A retraction over the
  wire is a `tx-update`; build it the same way with an `:old-node`.
- Registered at `graph-db.asd:731`.

## F33 — the runners, adapted to this worktree

Runner file, written once to
`/home/raison/work/vg-c3-notes/vg-361-suites.lisp` (outside every
checkout). Four suites: `index-suite` and the new `count-index-suite`
and `peer-index-suite` in `graph-db/test`, plus the spacetime root
suite through its own entry point.

```lisp
;; /home/raison/work/vg-c3-notes/vg-361-suites.lisp
;; The four suites #361 touches, CI-style, in a fresh image.
;; Mirrors RUN-TESTS (tests/suite.lisp): FiveAM's RUN bypasses the
;; test system's :perform, so the system directory is bound here.
(ql:quickload '(:graph-db/test :graph-db/spacetime-test) :silent t)
(format t "~&== graph-db loaded from ~a~%"
        (asdf:system-source-directory :graph-db))
(in-package :graph-db/test)
(log:config :error)
(let* ((system-dir (make-temp-directory))
       (graph-db::*system-directory* (namestring system-dir))
       (graph-db::*type-registry* nil)
       (ok t))
  (unwind-protect
       (progn
         (dolist (s '(index-suite count-index-suite peer-index-suite))
           (let ((r (fiveam:run s)))
             (fiveam:explain! r)
             (format t "~&== ~a ~a~%" s
                     (if (fiveam:results-status r) "PASS" "FAIL"))
             (unless (fiveam:results-status r) (setf ok nil))))
         (unless (graph-db/spacetime-test::run-spacetime-tests)
           (setf ok nil)))
    (graph-db-test-scratch:cleanup-scratch-run))
  (sb-ext:exit :code (if ok 0 1)))
```

(Until `count-index-suite` exists, drop it from the list or
`fiveam:run` signals on an unknown test name.)

Run from the worktree root, **foreground**, timeout 600000 ms — a
background run is killed by the harness's memory heuristic:

```bash
cd /home/raison/work/vg-c3/.worktrees/count-index
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --load /home/raison/work/vg-c3-notes/vg-361-suites.lisp \
  > /home/raison/work/vg-c3-notes/vg-361-suites.log 2>&1; echo "exit=$?"
grep -E "loaded from|Did [0-9]+ checks|Fail:|^== " \
  /home/raison/work/vg-c3-notes/vg-361-suites.log
```

A single test while iterating:

```bash
cd /home/raison/work/vg-c3/.worktrees/count-index
sbcl --dynamic-space-size 4096 --non-interactive \
  --eval '(push #p"./" asdf:*central-registry*)' \
  --eval '(ql:quickload :graph-db/test :silent t)' \
  --eval '(in-package :graph-db/test)' \
  --eval '(let* ((d (make-temp-directory)) (graph-db::*system-directory* (namestring d)) (graph-db::*type-registry* nil)) (unwind-protect (fiveam:explain! (fiveam:run (quote TEST-NAME))) (graph-db-test-scratch:cleanup-scratch-run)))' 2>&1 | tail -30
```

For a spacetime test swap `:graph-db/spacetime-test` and
`(in-package :graph-db/spacetime-test)`; the `let*` binding stays,
because `run-spacetime-tests` is not being used.

Always read back `Did N checks` — a suite whose tests never registered
reports green with zero checks. `*slow-suites*` (`tests/suite.lisp:8-14`)
is the fast-CI exclusion list; a new `count-index-suite` should **not**
go on it.

---

# §G — traps

**G1 — the registry key collides; a `kind` slot does not fix it.** X1.
The family declares a secondary index and a count index on the same
owner and the same slots. `(secondary-indexes graph)` is one key space
and `save-secondary-index-roots` maphashes it. Use a parallel table and
a parallel graph slot, as `def-unique` does.

**G2 — a count spec in `*schema-index-metadata*` builds a second, real
secondary index.** X1. `class-secondary-index-descriptors` is
kind-blind and is "the single input to maintenance"
(`index.lisp:227-232`).

**G3 — `%index-comp-lessp` type-errors on a depth-first key; `%index-equal`
merges "at" and "AT".** X4. Use `%index-value-lessp` (`index.lisp:343-355`)
and write a matching `EQUAL`-based key-equal. Sentinels are the
one-element `(:gmin)` / `(:gmax)`, **not** `%index-head-key` /
`%index-tail-key`.

**G4 — `%index-key-serialize` splices the last element as a 16-byte
id.** X5. Pass `'serialize` / `'deserialize` as the count map's key
codec; `deserialize` already returns `(values object length)`, which is
what `read-skip-node` (`skip-list.lisp:307-311`) needs.

**G5 — value serialization of a cons works, but its *length* varies.**
`serialize ((int integer))` is variable width (`serialize.lisp:436-454`),
so `(255 . 1)` → `(256 . 1)` grows the serialized pair and pushes
`update-in-skip-list` onto its remove+add path, which defers a heap
free until close (`skip-list.lisp:874-881`, `:473-480`). **Pass the true
OLD-VALUE** as the fourth argument or *every* update takes that path
(`tests/skip-list-tests.lisp:205` says so).

**G6 — `update-in-skip-list` does not insert on the memory backend.**
X2. Read first, then `add-to-skip-list` or `update-in-skip-list`.

**G7 — the counters are serialised by the transaction-manager lock (and
the device's single writer), not by any backend lock.** X3. Do not add
a lock inside the maintenance: `with-sl-write-lock` is `(progn)` on
SBCL and nesting it deadlocks on ECL (`skip-list.lisp:34-47`).

**G8 — `tx-delete` is a subclass of `tx-update`**
(`transactions.lisp:1021`), so all three methods must be written, and
the existing `tx-delete` method releases **twice**
(`index.lisp:623-625`) — idempotent for a set, a double decrement for a
counter.

**G9 — `peer-purge-node` bypasses the generic.** X6. `peer-streaming.lisp:1445`.

**G10 — a counter is not idempotent, and two paths re-apply.** X7.
Crash recovery (`transactions.lisp:3723-3733`) and the device state-sync
re-pull (`peer-streaming.lisp:1127-1142`) both bind
`*add-to-indexes-unless-present-p*` T precisely because they expect to
run twice.

**G11 — `%index-key` returns NIL at full arity when every component is
null** (`index.lisp:426`). `count-index-lookup` must read that as "0 0",
not as an error and not as "everything"; `index-count` already does
(`index.lisp:1126-1127`, `; an all-null full tuple matches nothing`). A
*shorter* all-null prefix is a real key
(`tests/index-tests.lisp:335-357` pins both halves), and stored nulls
read back as `+NULL-COMPONENT+`, not NIL — `%ix-prefix-out`
(`index.lisp:1062-1066`) is the existing substitution to copy.

**G12 — `%require-index` returns NIL for a declared-but-empty index**
(`index.lisp:973-988`), and the count equivalent must too: a
declared-but-never-built count index answers 0/0 and calls FN zero
times. On a `lazy-p` memory graph nothing is ever built
(`memory-graph.lisp:1122-1125`), so that is the normal state there.

**G13 — CURRENT-P resolution timing.** `%resolve-index-canonicalizer`
resolves a symbol with `(fdefinition spec)` **eagerly**
(`index.lisp:25-36`). `claim-current-p` lives in `claim-query.lisp`,
which loads *after* `claim.lisp` (`graph-db.asd:566-569`, `:serial t`),
so a `def-claim-classes` expansion that resolved it at macroexpansion
would be fine in practice but fragile. Store the **symbol** in the count
spec and `funcall` it at maintenance time; also re-resolve it on
sidecar restore, the way `%owner-slot-canonicalizer`
(`index.lisp:693-727`) re-resolves canonicalizers ("a function is not
serializable").

**G14 — the sidecar has no version constant, and its reader is one
`destructuring-bind`.** D17. A fifth element is forward-safe for new
builds (`&optional`) and makes an old build discard the whole file and
rebuild. A separate `count-indexes.dat` avoids the question entirely.

**G15 — removing a `def-index` form does not withdraw it.** X8. Emit
`(graph-db:unregister-index-spec ',parent ',graph-name :name 'claim-relation)`,
not `undef-index` (which warns when it matches nothing,
`index.lisp:132-144`). And re-evaluating `def-claim-classes` must stay
idempotent: every declaration it emits is `:name`d for the reason at
`spacetime/claim.lisp:412-421`, so the three count declarations need
`:name claim-subject-count` / `claim-object-count` /
`claim-relation-count` exactly as spec §3.1 says.

**G16 — the test package import rule.** `tests/package.lisp` is
`(:use #:cl #:fiveam)` only, with a curated `:import-from #:graph-db`
list; the index region is `:276-286`. A newly exported `graph-db`
symbol used unqualified in `tests/index-tests.lisp` or a new
`tests/count-index-tests.lisp` **must** be added there, or it silently
interns a fresh symbol and fails at fasl-load time —
`tests/README.md:23-34` states the rule and says it bit two
implementers. `tests/spacetime/package.lisp:8-9` does `:use
#:graph-db.spacetime` but **not** `#:graph-db`, so anything from the
engine stays `graph-db:`-qualified there.

**G17 — 80 columns, spaces only.** `index.lisp` and
`spacetime/claim-query.lisp` both hold to it. The three count
primitives' docstrings have to carry the live-at-commit bound (R1), the
`:current` rule and the not-an-atomic-snapshot warning; break the form
and put the long version in `docs/general-index-design.md` (a new
section after §6a, which ends at `:145`) and the `.org` chapter.

**G18 — a stale FASL survives a broken edit.** After any edit that
changes a package export, run the suites in a *fresh* image (F33),
never in a REPL that already loaded the old system: ASDF will load a
stale FASL and report a correct tree red, or a broken one green.
