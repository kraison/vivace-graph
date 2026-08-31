# Ontology evaluator — pre-image, delta and the other endpoint

**Status:** design note, not approved, not implemented
**Issues:** #109 (epic); decides once for #158 (unit 5), #155 (unit 2),
#156 (unit 3); #157's 4b inherits it (disjointness note, open question 3)
**Branch:** `experiment`

The #109 thread closes with one question that spans three units: unit 1's
evaluator "validates a node against a spec and nothing more", and units 5,
2 and 3 each need something it does not have — the prior value, a delta,
the other endpoint. "Either the evaluator gains access to the pre-image and
the graph, or each family grows its own hook. Three separate hooks would be
exactly the divergence #140 was about." This note answers it, with the
answer measured from what the commit path already holds rather than from
what the issues assumed it lacks.

## 1. What the commit path already has

Read from `transactions.lisp` and `value-constraint.lisp` as of `26d0e18`.

**The pre-image is already in the write record.** A transaction's write set
is a list of `tx-write` objects of three classes: `tx-create` (node only),
`tx-update` (node **and `old-node`**, `transactions.lisp:885`) and
`tx-delete` (a `tx-update`). `old-node` is the committed instance the copy
was taken from. `validate-value-constraints` iterates `(writes tx)` and
hands the evaluator `(node write)` — one frame above the evaluator, the
pre-image is in hand and discarded. #158 does not need new machinery to
find the prior value; it needs the evaluator to be told it.

**The delta is the write set.** `(writes tx)` *is* the transaction's
proposed change: every created, updated and deleted node, each with its
pre-image where one exists. #155's "what does the count mean
mid-transaction" is answered by reading the store *through* the write set,
which is what `validate-unique-constraints` already does for keys with its
`intra` table — the same-transaction claimants overlaid on the index.

**The store is readable inside the lock, and already is read there.** The
manager lock serialises commits; `%check-unique-key` reads the unique index
under it and relies on exactly that: "the index reflects it, since prior
commits' APPLY ran under this same lock" (`unique-constraint.lisp:661`).
Node reads are MVCC and take no further lock. #156's "a graph read inside
the commit path" is therefore not a new category of thing — it is an index
lookup of the kind the commit path performs today, and its cost question is
"how many and how selective", not "whether".

**The live hole is a `tx-update`.** `rest-put-vertex` (`rest.lisp:480`)
copies the vertex inside a transaction, writes each named slot with
`slot-value`, and saves — a `tx-update` with an `old-node`. The same hook
that closes #158 for the accessor path closes it for REST.

## 2. Decision: one commit view, not three hooks

Every constraint family evaluates against a **commit view**: a value built
once per commit (and once per audit pass) that answers questions about
*post-commit state* by overlaying the transaction's writes on the store.

```lisp
(defstruct commit-view
  graph          ; the store being committed to (one; see §5)
  writes         ; id -> tx-write, this transaction's delta; empty for audit
  )

(view-node view id)        ; the node as it will be after commit:
                           ;   the write's node if ID is written, else the
                           ;   store's; NIL if deleted in this transaction
(view-old-node view id)    ; the committed pre-image: OLD-NODE for an
                           ;   update/delete, the store's node for an
                           ;   untouched id, NIL for a create
(view-writes view)         ; the delta, for families that count it
(view-lookup view class slots key)
                           ; INDEX-LOOKUP overlaid with the delta: rows
                           ;   the transaction deletes are removed, rows
                           ;   it creates or updates are substituted
```

The evaluator's signature becomes `(spec node view)`. `%value-constraint-
violations` gains the view; `validate-value-constraints` builds it once from
`tx`; `check-value-constraints` builds a **store-only** view (no writes), in
which `view-node` and `view-old-node` coincide. Unit 1's `:one-of` /
`:required` / `:check` evaluators ignore the view and are unchanged.

**Why this and not per-family hooks.** Each of the three units, written on
its own, would re-derive "what is the post-commit state" — the pre-image for
one, the overlay for another, the store read for the third — and the three
derivations would disagree at the edges (a node updated *and* counted; an
endpoint created in the same commit; a delete that makes a count legal).
The view states the rule once: **post-commit state = store ∪ writes, writes
win, deleted is absent**. It is the rule `intra` already encodes for unique
keys, made available to every family.

**Why a struct and not "pass the tx".** The audit pass has no transaction.
One view type with an empty delta lets the batch pass and the write path run
the *same* evaluator, which is the property unit 1 established ("the audit
pass returns a spec count") and #157 4a wants to reuse. Passing `tx` would
fork the two paths at the first family that needs the delta.

## 3. What each unit gets from it

### Unit 5 — #158, a slot that may not change after creation

The check is `(slot-value node slot)` against `(slot-value (view-old-node
view (id node)) slot)`. A create has no pre-image and passes. A change from
NIL to a value is the creation write and passes. Any other change signals.

**Null semantics, stated explicitly (the #107 rule):** NIL is "not yet
written". So NIL → value is allowed once; value → NIL is a change and is
refused; value → same value is not a change.

**Not plain write-once, because #162 already broke it.** The motivating
slot — the claim's transaction extent — is *start*-immutable and
*end*-closeable: `retract-claim` closes `[recorded, open)` to `[recorded,
at)`, and re-registration re-opens a retracted claim with a fresh stamp.
A family that can only say "never changes" cannot express the substrate's
own rule for the field that motivated it. So the family is **transitions**,
with write-once as its degenerate case:

```lisp
(def-value-constraint parent slot graph-name
  :write-once t)                      ; sugar: transition (old new) -> (null old)
(def-value-constraint parent slot graph-name
  :transition transaction-extent-step ; a named schema function of (OLD NEW)
  :name transaction-extent-immutable)
```

`:transition` names a function in the schema-function registry (#172's
`register-schema-function`, already used by `:check`), of two arguments —
the pre-image value and the proposed value — returning true when the change
is legal. Declared by name for the reason `:check` is: behaviour ships in
the image, structure in the data. The substrate registers
`transaction-extent-step` itself: NIL → anything; open → closed with the
same start; closed → open with a later start; everything else refused. That
makes the #148/#162 rule *testable* at commit rather than a docstring.

The audit pass cannot check a transition — there is no pre-image after the
fact — so a transition spec contributes to the spec count but never to the
violation list, and the report must say so rather than let "0 violations"
read as "audited". `:write-once` is auditable only as "slot is non-NIL",
which is `:required`, not this.

### Unit 2 — #155, cardinality

Counting is `(length (view-lookup view class slots key))` — the post-commit
membership, with this transaction's adds and removes already applied.
"Counting what?" becomes "which index": peer claims through the subject
index, edges through `ve-index`, repeated slot values through the node
itself. Whether the count is affordable is then a measurement of the
lookup's selectivity, which is what the issue asks for and what `tests/perf`
exists to answer.

### Unit 3 — #156, domain and range

The other endpoint is `(view-node view other-id)`: created in this commit
→ found in the writes; existing → read from the store under the lock, the
same class of read the unique validator performs. A missing endpoint and a
wrong-typed endpoint stay distinct: `view-node` returning NIL is
`:dangling`, a node of the wrong class is `:wrong-type`, two reasons in the
violation.

**Cross-store endpoints are out of the view (§5).** A claim's object may
live in another store, and a read-write transaction is single-graph. For
those, range is answered at the *schema* level — the classes a namespace is
declared to hold (`namespace-sources`, #132) — which needs no node read;
where a namespace holds several classes and the range names only one, that
endpoint is `:undecidable` and reported as such, not read across the
boundary. #156 decides whether that is acceptable or whether range must be
declared over namespaces rather than classes; this note only fixes that the
view will not read another store.

### Unit 4b — #157, claim-asserted disjointness

Expressible through `view-lookup`: the other membership claims about a
subject are the subject-index rows overlaid with this commit. That answers
the disjointness note's open question 3 — no fourth registry, no second
read-in-commit mechanism.

## 4. The audit pass

One evaluator, a store-only view. `check-value-constraints` keeps its
contract — `(values violations checked spec-count)` — and gains one line in
the report: how many specs are transitions and were therefore not audited.
Zero violations over a schema that is all transitions must not print as
clean.

## 5. Scope: one store, under the lock

The view reads the store it is committing to and nothing else. Two reasons,
both already load-bearing elsewhere: a read-write transaction is
single-graph (3.0's multi-graph contract), and a node must not cross a
`*graph*` binding (#53). A family needing the other store gets a schema-
level answer or an explicit `:undecidable`, never a read.

Cost under the lock: building the view is O(writes); `view-node` and
`view-old-node` are hash lookups plus, for untouched ids, one cached node
read; `view-lookup` is one index lookup plus a filter over the delta. That
is the same order as the unique validator's work today. Measure before
#155/#156 land (the perf suite's registration and commit throughput
cases), not after.

## 6. Testing

- **Ablation over inspection** (#109's cost lesson): each unit's tests must
  be shown to go red with the check removed. The #158 trap in particular:
  a test that creates the node and reads the slot back proves nothing;
  the discriminating case is an *update* that changes the slot — through
  `copy`/`save` **and through `rest-put-vertex`**, the live hole.
- **The overlay's edges**, once, in the view's own tests: a node updated
  and counted in one commit; both endpoints created in one commit; a
  delete that makes a count legal; a create that has no pre-image.
- **Store-only view equals write-path view** on a quiescent graph: the same
  node, the same spec, the same verdict from both paths.
- **The transition sugar**: `:write-once t` and an explicit `(lambda (old
  new) (null old))` produce the same verdicts.

## 7. What this note does not decide

- #156's cross-store range: class-level with `:undecidable`, or declared
  over namespaces. The view's boundary is fixed here; the declaration's
  shape is #156's.
- Whether `:transition` should also be a `def-vertex` slot option, as
  `:check` is (#172). Registry form first — named, replace-not-stack, one
  identity rule — and the slot-option form when the runtime-schema path
  needs it.
- Retention of pre-images for the audit pass (an `:as-of` view). #148
  scoped it out; nothing here reopens it.

## 8. Files

- `value-constraint.lisp`: `commit-view`, the view accessors, the evaluator
  signature, `:transition` / `:write-once` in `def-value-constraint` and the
  spec struct, the audit report line.
- `transactions.lisp`: the call site is unchanged — `validate-value-
  constraints tx graph` builds the view itself.
- `spacetime/claim.lisp`: the substrate's `transaction-extent-step` and its
  declaration in `def-claim-classes`, replacing the `⚠` block that names
  #158 as the fix; `claim-query.lisp`'s accessor guard stays as the
  fast-fail with the better error site, as `check-standing` did for unit 1.
- `docs/superpowers/specs/2026-08-18-transaction-time-design.md`: the
  immutability section's "known limitation" closes when #158 lands.
