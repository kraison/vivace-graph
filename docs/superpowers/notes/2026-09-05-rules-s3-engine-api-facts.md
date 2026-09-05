# Engine API facts, verified for rules S3 (#332)

A snapshot, not a maintained document. The recon pass for S3's plan
(`docs/superpowers/plans/2026-09-05-rules-s3-cross-store-scope.md`,
Task 0): nine assumptions B1–B9, each confirmed or refuted from source,
every correction re-checked against a second location before it was
kept. It is committed because it is the record of why the S3 tasks say
what they say — §C is the finding → correction map the plan text is to
be amended from. Its model is the S2 note
(`docs/superpowers/notes/2026-09-05-rules-s2-engine-api-facts.md`),
whose A4, A6 and C2 B6 and B9 build on.

**Pinned to `54dddbd`** (this branch's tip, `feat/rules-s3`; the tree is
`origin/experiment` `a75cf96` plus the plan doc, so every engine line
number is `a75cf96`'s). Every `file:line` will drift; the quoted forms
are what to match on, not the numbers. Paths beginning `~/work/cl-llm`
are the consumer repo, not this one.

**Nothing here was evaluated in an image.** Every item settled from
source, so no `sbcl --non-interactive` run was needed and none was made.
A claim about *runtime* behaviour is therefore an inference from the
code and is marked as such where it matters.

Reading order:

- **§C — corrections.** Four defects this pass found in the S3 plan
  before execution began, plus one observation (O1) that is outside
  B1–B9 and is flagged as not settled to the same depth. C1 is blocking
  for Task 1.
- **§B — the nine items** in order, each with the form quoted.

Line lengths here exceed the repo's 80-column rule in a few dozen places
(quoted forms and tables). Left as verified rather than rewrapped by
hand, which risks silently corrupting a quoted form. Every quoted form
is verbatim; the only edits are a `...` standing for lines dropped from
the middle of a form. Every code line is byte-for-byte.

---

# §C — corrections to the S3 plan

## C1 (BLOCKING for Task 1) — a store in scope that lacks the family makes `claim/7` SIGNAL, not contribute nothing

**Plan assumes (S3-P5, and B9's framing):** "A store in scope whose
schema lacks a family a goal names simply contributes nothing (the
`query-precondition-error` `%producer-candidates` already swallows)."

**Reality: only `%producer-candidates` swallows it. `claim/7`'s three
indexed routes call `index-lookup` bare, and `index-lookup` signals.**

`%require-index` is the gate, and its `t` clause is an error, not a NIL
(`index.lisp:973-988`):

```lisp
(defun %require-index (graph class-name slot-name)
  "The SLOT-INDEX for CLASS-NAME.SLOT-NAME.  Returns NIL when the slot is a declared
index (via :INDEX or DEF-INDEX) but no entries exist yet (a legitimately empty
result); signals only when the slot is not indexed at all (a programming error)."
  (let* ((slot-names (%normalize-slots slot-name))
         (six (%secondary-index-lookup graph class-name slot-names)))
    (cond (six six)
          ((or (%slot-index-declared-p class-name slot-names)
               (%def-index-declared-p graph class-name slot-names))
           nil)                                             ; declared, empty
          ;; A caller's error, typed so a server can tell it from a
          ;; defect (GH #286).
          (t (error 'query-precondition-error
                    :reason (format nil "No secondary index on ~S.~S in ~S"
                                    class-name slot-name
                                    (graph-name graph)))))))
```

Both escape clauses are per-store or arity-limited, so neither saves a
store that never declared the family:

- `%secondary-index-lookup` reads `(secondary-indexes graph)` — that
  store's own registry (`index.lisp:929-945`).
- `%def-index-declared-p` reads `%registered-index-specs graph`, which
  is `(gethash (graph-name graph) *schema-index-metadata*)`
  (`index.lisp:177-186`) — again that store's own.
- `%slot-index-declared-p` is class-level but "Only matches at arity 1"
  (`index.lisp:947-960`). Every claim index this code uses is a tuple —
  `+claim-subject-index-slots+` (2), `+claim-subject-relation-index-slots+`
  (3), `+claim-object-index-slots+` (2) — and `producer`, the one
  arity-1 index, is declared by `def-index`, not by an `:index t` slot
  option (`spacetime/claim.lisp`'s `+claim-shared-slots+` has no
  `:index`). So it is NIL for all four.

And `claim/7` catches nothing (`rules/facts.lisp:141-159`):

```lisp
                 ((and sns skey rel)
                  (index-lookup g parent
                                +claim-subject-relation-index-slots+
                                (list sns skey rel)))
                 ((and sns skey)
                  (index-lookup g parent +claim-subject-index-slots+
                                (list sns skey)))
                 ((and ons okey)
                  (index-lookup g binary +claim-object-index-slots+
                                (list ons okey)))
```

**Second location, and the shape of the fix.** Every *other*
`index-lookup` caller that iterates families or stores already wraps it,
and says why. `%producer-candidates` (`rules/facts.lisp:233-252`):

```lisp
        (handler-case
            (dolist (c (index-lookup graph parent
                                     +claim-producer-index-slots+
                                     producer))
              (push c out))
          ;; Also the condition a wrong component count signals
          ;; (%INDEX-BOUNDS, index.lisp) -- safe only while this index
          ;; is arity 1 and PRODUCER a bare scalar; a multi-slot one
          ;; would read a shape error as "no candidates".
          (query-precondition-error () nil))))))
```

and S2's own premise resolver, `%claim-by-identity-key`
(`rules/run.lisp:553-566`):

```lisp
          ;; A family this graph does not index, as %PRODUCER-CANDIDATES
          ;; reads it (rules/facts.lisp): no candidates, not a fault.
          (graph-db:query-precondition-error () nil)))
```

**Fix (Task 1).** Wrap each per-store `index-lookup` in `claim/7` in the
same `handler-case (... (query-precondition-error () nil))`, per store
and per route, so an undeclared family in one store of the scope
contributes nothing instead of aborting the whole goal. Amend S3-P5's
parenthetical to say the swallow must be **added** to `claim/7`, not
that it already exists. `tests/rules/suite.lisp:46-55` already ships the
fixture this needs — `rtf-claim` on `:graph-db-rules-test-foreign`, a
family "indexed in no graph here" — so a scope test can use a store
whose schema lacks `rt-claim` without inventing one.

**Two things this does NOT change.**

- The declared-but-empty case is already correct: with `rt-claim`
  declared for store B and no claims written, `%secondary-index-lookup`
  returns NIL, `%def-index-declared-p` returns T, `%require-index`
  returns NIL, and `index-lookup`'s `(when six ...)` yields NIL. No
  signal, no rows.
- **S3-P6 is right as written.** `%unbound-claim-scan` goes through
  `map-vertices :vertex-type parent`, and `resolve-node-type-ids` drops
  a designator no store-registered type answers to
  (`node-class.lisp:325-340`): "Designators that resolve to no
  registered type of KIND are skipped (so the 0 sentinel and cross-graph
  subclasses simply drop out)." The `dolist` over an empty `type-ids`
  visits nothing. Silent, no signal.

## C2 — composed snapshots under one clock are NOT one instant

**Plan assumes (Goal line; S3-P2; Task 2's `with-clocked-stores`
docstring "so the two stores' snapshots share an instant (#168)"; Task
2's `(#168)` docstring and Step 5's docs text):** "the reads of a
cross-store run resolve at one instant under the shared clock".

**Reality: the shared clock buys a single comparable epoch space, not an
atomic acquisition.** `call-with-read-snapshot` establishes each store's
snapshot independently; nothing coordinates the two, and its own
docstring denies the property (`transactions.lisp:3333-3337`):

```lisp
The snapshot is recorded in *READ-SNAPSHOTS* under GRAPH rather than bound to
*TRANSACTION*, so snapshots on several graphs COMPOSE: a cross-graph query holds
one snapshot per participating graph, each internally consistent, with
deliberately no single instant across them (GH #53).  An enclosing snapshot of
the SAME graph is inherited, as is a read-write transaction on it.
```

The epoch each snapshot resolves at is read, not reserved, at
`create-transaction` time (`transactions.lisp:3277-3291`):

```lisp
    (let* ((sequence-number (next-sequence-number transaction-manager))
           (graph (graph transaction-manager))
           (cache (cache graph))
           (start-tx-id (tm-current-epoch transaction-manager))
```

and `tm-current-epoch` reads the *graph's own* manager's clock
(`transactions.lisp:3212-3219`), which under a shared clock is
`(system-clock-counter clock)` (`system-clock.lisp:291-294`). Two
`call-with-read-snapshot`s therefore read the same counter at two
different moments.

**Second location — the engine's own test engineers the divergence
deliberately** (`tests/system-clock-tests.lisp:756-768`):

```lisp
(test cross-store-snapshot-pins-every-store
  ;; Identity, not just presence (review round 2): a hypothetical swapped
  ;; implementation -- GA's snapshot pinning TM-B, GB's pinning TM-A --
  ;; would still pass a count-only check, since both tables end up with
  ;; one entry each.  Under a SHARED clock the two managers' epochs are
  ;; usually the same number, so a value comparison alone can't tell them
  ;; apart -- unless something advances the clock between the two
  ;; snapshots' establishment.  A real write on GB before GB's own
  ;; snapshot does exactly that, so GA's and GB's own pin-time epochs
  ;; diverge and a swap becomes visible as a value mismatch.
```

"usually the same number" is the whole guarantee. `create-transaction`
reads `tm-current-epoch`, not `tm-next-epoch`, so a read-only snapshot
does not itself advance the clock — with no concurrent writer the two
epochs *are* equal. With one, they are not, and the composition then
sees store A as of `e_A` and store B as of `e_B > e_A`.

**What the clock actually buys**, and what the plan may say: the two
epochs come from one monotonic space, so they are *comparable* across
stores (spec `docs/superpowers/specs/2026-08-20-namespaces-design.md`
§6: "Below the watermark epochs are not cross-store comparable; above it
they are"), and they are *equal in a quiescent image*. Without a clock
they are not even comparable (#53).

**Fix.** Reword the Goal line, S3-P2, `with-clocked-stores`' docstring,
Task 2's `run-rule` docstring and Step 5's "Running a rule" docs to say
one *comparable epoch space*, equal when nothing commits between the
acquisitions — never "one instant". And do not write a test that asserts
one instant: it would pass vacuously in a quiescent suite and pin a
property the engine does not provide. The design's acceptance bullet
("A cross-store query resolves at one instant", §12) is an aspiration
this unit does not implement, and #332 should not claim it.

## C3 — `clos.lisp` is not loaded at all; the applicable method is in `primitive-node.lisp`

**Plan assumes (B4):** check `slot-value-using-class :around`
(`clos.lisp:38-43`).

**Reality: `clos.lisp` is not a component of any system in
`graph-db.asd`.** It is tracked in git and never compiled. `grep -n
clos graph-db.asd` finds only `:closer-mop` and two comment lines; the
base system's component list runs `... node-class, views,
primitive-node, vertex, edge ...` with no `clos` entry. Its
`graph-class` metaclass is used by exactly one class — `node`, in that
same dead file (`clos.lisp:63-87`) — and the live `node` is
`node-class.lisp:415-445`, `(:metaclass node-class)`. `clos.lisp:53`
even calls `(save-node instance)` with one argument where `save-node`
takes `(node table &key graph)` (`primitive-node.lisp:361`), which
would signal if it ever ran.

**The method that actually fires** for a claim slot read is
`primitive-node.lisp:506-516`:

```lisp
(defmethod slot-value-using-class :around ((class node-class) instance slot)
  "Around method that is alternate-version aware and will show values for the current,
   working private version of instance."
  (log:trace "slot-value-using-class~%  '~A'~%  '~A'" class (slot-definition-name slot))
  (let* (#+lispworks(slot (closer-mop::find-slot slot class))
         (slot-name (slot-definition-name slot))
         (slot-keyword-name (%persistent-slot-keyword class slot-name)))
    (if slot-keyword-name
        ;; FIXME: Check for txn and give current revision's value
        (node-slot-value instance slot-keyword-name)
        (call-next-method))))
```

Claim classes reach it because `def-vertex` expands to a `defclass` with
`(:metaclass node-class)` (`schema.lisp:797`).

**Fix.** B4's *conclusion* is unchanged and CONFIRMED (see §B), but
every task or doc line citing `clos.lisp:38-43` must cite
`primitive-node.lisp:506-516` → `node-slot-value`
(`primitive-node.lisp:441-449`) → `maybe-init-node-data`
(`primitive-node.lisp:300-336`) instead.

## C4 — the reason a premise must not escape evaluation is the read PIN, not the cross-graph error

**Plan assumes (S3-P3, and B4's framing):** the premise leaves as
`(identity-key . store-name)` because the reconcile "runs inside A's
transaction" and a foreign node read there would signal.

**That is true but is not the binding constraint**, and B4's own
question ("if any lazy slot read can call `lookup-object`") answers no —
so on the plan's stated reasoning alone, carrying the node would look
safe. It is not.

Under a read snapshot, `lookup-object` re-dispatches to the
*transactional* method and never reaches the branch that materialises
bytes (`transactions.lisp:291-296`):

```lisp
  (:method (id table (transaction null) graph)
    ;; No read-write transaction: a read-only snapshot of GRAPH, if one is
    ;; active, resolves the read (GH #53).
    (let ((snapshot (and *read-snapshots* (gethash graph *read-snapshots*))))
      (when snapshot
        (return-from lookup-object (lookup-object id table snapshot graph))))
```

The `ensure-node-bytes` call sits *after* that `return-from`, in the
`standalone` branch (`transactions.lisp:304-317`). The transactional
method calls `ensure-node-bytes` only inside `resolve-version-at-epoch`,
and only when it returns an *archived* version
(`transactions.lisp:645-663`); a live head old enough for the snapshot
is returned as-is, lazy.

So a node returned by `index-lookup` under store B's snapshot may carry
`bytes` `:INIT`. Read a slot on it after the snapshot exits and
`maybe-init-node-data` reads B's heap at the node's `data-pointer` with
no read pin in force (`primitive-node.lisp:300-322`):

```lisp
    (let ((bytes (bytes node)))
      (when (or (eq bytes :init) (null bytes))
        (setf bytes
              (setf (bytes node)
                    (read-bytes (make-mpointer
                                 :mmap (memory-mmap
                                        (heap (node-home-graph node graph)))
                                 :loc (data-pointer node))))))
```

which is exactly what `ensure-node-bytes`' docstring says must not
happen (`primitive-node.lisp:217-222`): "this is the materialization the
read paths perform WHILE A READ PIN (or the reading transaction's
start-tx-id) protects NODE's data block, so the bytes are captured
before the pin is released and the node can escape self-contained."
`call-with-read-snapshot` holds that pin only for its own extent
(`transactions.lisp:3369-3376`, `pin-read-epoch` / `unpin-read-epoch`).

**Fix.** Restate S3-P3's rationale: everything the reconcile needs from
a premise — the identity key, and the extent sexp if any later task
wants it — must be computed **inside** the snapshot extent, because a
node that escapes an `index-lookup` under a snapshot is not
self-contained. The cross-graph refusal is a second, weaker reason.
Note the asymmetry the tasks must not forget: `%unbound-claim-scan`'s
`map-vertices ... :collect-p t` *does* materialise
(`vertex.lisp:227-233`, "When collecting, each node ESCAPES the scan
pin, so materialize its bytes before FN sees it"), `index-lookup` does
not. This does not change any task's structure — S3-P3 already computes
the key during evaluation — it changes why, and it forbids a later
"optimisation" that passes nodes to the reconcile.

## O1 (observation, outside B1–B9 — not settled to the same depth)

Secondary-index **membership** is not snapshot-versioned. `%ix-release`
removes the entry outright, post-durability
(`index.lisp:602-609`, under the header "Maintenance (APPLY,
post-durability, journal-replayable -- no enforcement)"), and
`index-lookup`'s only snapshot-aware step is resolving each id it
already found. So under a read snapshot of store B, a claim *inserted*
after the snapshot is correctly invisible (`%node-by-id` →
`resolve-version-at-epoch` returns NIL, and the `(when (and node ...))`
guard drops it), but a claim *deleted* after the snapshot is also
invisible, though the snapshot's epoch predates the delete.

This is pre-existing and equally true of S2's single-store runs; it is
not an S3 regression and nothing in the plan depends on the stronger
reading. Recorded so no S3 doc or test asserts more per-store
consistency than the engine provides. **Not adversarially settled by a
live scenario** — verify by test before relying on it in either
direction.

## Finding → correction map

| finding | item(s) | correction | severity |
|---|---|---|---|
| `claim/7`'s indexed routes signal `query-precondition-error` on a scope store that never declared the family; only `%producer-candidates` and `%claim-by-identity-key` swallow it | B1, B9 (S3-P5) | C1 | **blocking for Task 1** |
| composed snapshots under one clock are one comparable epoch space, not one instant; the engine's docstring and test both say so | B2, B7 (Goal, S3-P2) | C2 | wording of the Goal, S3-P2, two docstrings, the docs; forbids one test |
| `clos.lisp` is in no system and never loaded; the live method is `primitive-node.lisp:506-516` | B4 | C3 | citation + mechanism |
| a node escaping `index-lookup` under a snapshot is not self-contained; the pin, not the cross-graph error, is what forces key-plus-store-name | B3, B4 (S3-P3) | C4 | rationale; forbids a later "pass the node" change |
| secondary-index membership is not snapshot-versioned | — (new) | O1 | observation, no plan change |

Everything else — B5, B6, B8, and the confirmed halves of B1–B4, B7,
B9 — is CONFIRMED as written, with the `file:line` drift listed per
item.

---

# §B — the nine items

## B1 — `index-lookup` on a foreign graph inside a read-write transaction — CONFIRMED (path exact; see C1 for a second failure mode the plan did not anticipate)

The path is exactly as the plan states, and every route in
`index-lookup` funnels through it.

`index-lookup` resolves every id through `%node-by-id`, in both the
collecting and the short-circuit arm (`index.lisp:990-1013`):

```lisp
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => no matches
      (let ((key (%index-key six value))
            (result '()))
        (when key
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
        (when collect-p (nreverse result))))))
```

Note `(let ((*graph* graph) ...))`: `index-lookup` rebinds `*graph*` but
**not** `*transaction*`, which is what makes the refusal below reachable.

`%node-by-id` (`spatial-query.lisp:35-38`):

```lisp
(defun %node-by-id (id graph)
  "Resolve a spatial-index id (uuid bytes) to its live node, or NIL."
  (or (lookup-vertex id :graph graph)
      (lookup-edge id :graph graph)))
```

Both arms pass the *ambient* transaction and the *explicit* graph
(`vertex.lisp:110-115`, `edge.lisp:144-148`):

```lisp
(defmethod lookup-vertex ((id array) &key (graph *graph*))
  "Return the vertex with the given ID (a 16-byte id array or its string form)
in GRAPH, or NIL if none.  Returns the vertex regardless of its deleted flag;
the generated LOOKUP-<type> functions filter deleted nodes for you."
  (lookup-object id (vertex-table graph) *transaction* graph))
```

and `lookup-object`'s transactional method refuses the mismatch before
anything else (`transactions.lisp:319-326`):

```lisp
  (:method (id table transaction (graph t))
    ;; A read-write transaction is single-graph (GH #53).
    (let ((txn-graph (graph transaction)))
      (unless (eq graph txn-graph)
        (error 'cross-graph-transaction-error
               :node id :transaction-graph txn-graph :node-graph graph)))
```

`cross-graph-transaction-error` is `conditions.lisp:16-30`; its report
prints both graph names, so a scope-refusal test can assert on them.

**No route bypasses it.** `index-lookup` has exactly one node-producing
call, `%node-by-id`. `ix-lookup` (`index.lisp:488`) reads the index's
own ordered map, not nodes; `%require-index` reads registries. The
`(node-p c)` early arm of `claim/7` (`rules/facts.lisp:141`) returns a
node the caller already held and performs no lookup at all.

**Citation drift:** the plan says `transactions.lisp ~:319-323`; the
method head is `:319` and the `error` form is `:323-325`.

**What the plan missed here is C1**, not the refusal: the *other* way a
foreign-store `index-lookup` fails is `%require-index`'s
`query-precondition-error`, and that one fires outside any transaction.

## B2 — composed snapshots, and `select :snapshot t` inheriting — CONFIRMED (all three halves, from the code)

**(a) Both graphs are registered in one `*read-snapshots*` table.**
`call-with-read-snapshot` reuses an enclosing table rather than shadowing
it, and adds its own entry (`transactions.lisp:3357-3382`):

```lisp
      (t
       (let ((txn nil)
             (pin nil)
             (table (or *read-snapshots* (make-hash-table :test 'eq))))
         ...
         (unwind-protect
              (progn
                (setq txn (create-transaction tm :allow-read-only t))
                (unwind-protect
                     (progn
                       (setq pin (pin-read-epoch tm))
                       (let ((*read-snapshots* table))
                         (setf (gethash graph table) txn)
                         (funcall thunk)))
                  (when pin (unpin-read-epoch tm pin))))
```

`(or *read-snapshots* (make-hash-table ...))` is the composition: the
inner call takes the outer's table object, so after nesting A then B the
one table holds both. The special is `transactions.lisp:31-33`:

```lisp
(defvar *read-snapshots* nil
  "Graph -> read-only snapshot transaction, or NIL.  Read-only snapshots are
per graph and may compose; read-write transactions are not (GH #53).")
```

Behaviour is pinned by `tests/multi-graph-tests.lisp:1119-1131`
(`read-only-snapshots-compose-across-graphs`) and, discriminatingly, by
`composed-snapshots-each-hide-their-own-graphs-later-commits`
(`tests/multi-graph-tests.lisp:1133-1160`), whose docstring says a
missing entry would "fall through to a live non-transactional read".

**(b) `index-lookup` on either resolves through that graph's own
snapshot.** With `*transaction*` NIL, the null-transaction method
dispatches on the *explicit* graph argument
(`transactions.lisp:291-296`):

```lisp
  (:method (id table (transaction null) graph)
    ;; No read-write transaction: a read-only snapshot of GRAPH, if one is
    ;; active, resolves the read (GH #53).
    (let ((snapshot (and *read-snapshots* (gethash graph *read-snapshots*))))
      (when snapshot
        (return-from lookup-object (lookup-object id table snapshot graph))))
```

and the re-dispatch lands on the transactional method with that
snapshot, whose `(eq graph txn-graph)` check is satisfied by
construction — the snapshot was created for exactly that graph. So B's
reads resolve at B's epoch and A's at A's, with no cross-graph error,
which is what makes S3-P2's shape legal. `read-transaction`
(`transactions.lisp:3316-3322`) states the same resolution rule in one
place.

**(c) `select :snapshot t` inherits rather than opening a second.**
Confirmed from the code, not the docstring. `run-query-goals` binds
`*graph*` and emits `:snapshot t` (`query/dsl.lisp:329-346`):

```lisp
  (let* ((*graph* graph)
         (*package* package)
         ...
                  (eval `(select (:effects nil :snapshot t
```

`select` expands that to a `call-with-read-snapshot` on `*graph*`
(`prologc.lisp:1125-1131`):

```lisp
              ,(if (cdr (assoc :snapshot options))
                   ;; Run the query under one consistent MVCC read snapshot:
                   ;; all reads resolve at a single epoch (lock-free, stable
                   ;; against concurrent writers).  Inherits an enclosing
                   ;; transaction if one is already active.
                   `(call-with-read-snapshot
                     (lambda () (funcall func #'prolog-ignore)) *graph*)
```

and the inherit is the third `cond` clause, before any transaction is
created (`transactions.lisp:3349-3356`):

```lisp
    (cond
      ((null tm) (funcall thunk))
      ;; a read-write transaction on this graph already provides a snapshot
      ((and *transaction* (%transaction-covers-graph-p *transaction* graph))
       (funcall thunk))
      ;; already snapshotted this graph -> inherit
      ((and *read-snapshots* (gethash graph *read-snapshots*)) (funcall thunk))
```

So an S3 `run-rule` that wraps `run-query-goals` in composed snapshots
gets: A inherited by the `select`, B resolved from the table, no second
transaction, no signal.

**Citation drift:** the plan says `~:3355-3372` for the registration and
`~:294` for the null method; both are right to within two lines
(`3357-3382`, `291-296`).

**See also O1** for the one consistency property this does *not* give.

## B3 — every node `index-lookup` returns carries `node-graph` — CONFIRMED, with one path named

Both arms of `lookup-node` stamp, and stamp with the graph the lookup
named, not `*graph*` (`primitive-node.lisp:338-360`):

```lisp
(defmethod lookup-node ((table lhash) (key array) (graph graph))
  (or (and *cache-enabled*
           (let ((node (gethash key (cache graph))))
             (when node
               ;; Nodes also enter the cache unstamped (APPLY-TX-WRITE :AFTER),
               ;; so stamp on the hit too (GH #53) -- but only when wrong, so the
               ;; steady state of this hot read path stays a pure read.
               (unless (eq (node-graph node) graph)
                 (setf (node-graph node) graph))
               (record-graph-read graph)
               node)))
      (let ((node (lhash-get table key)))
        (when (node-p node)
          (setf (id node) key)
          (unless (eq (node-graph node) graph)
            (setf (node-graph node) graph))
```

`finalize-node` stamps at commit (`primitive-node.lisp:208-212`):

```lisp
(defun finalize-node (node table graph)
  (setf (written-p node) t)
  (setf (node-graph node) graph)
  (save-node-flags table node)
  (setf (gethash (id node) (cache graph)) node))
```

`ensure-node-bytes` re-stamps on the standalone read path
(`primitive-node.lisp:224-225`), `apply-tx-write ((write tx-update))`
stamps both versions (`transactions.lisp:986-987`), `make-vertex` /
`make-edge` stamp at construction (`vertex.lisp:161`, `edge.lisp:254`),
and `%copy` propagates (`transactions.lisp:2857`).

Pinned by `tests/multi-graph-tests.lisp:622-635`:

```lisp
(test nodes-record-their-home-graph
  "A node read out of its own graph reports THAT graph, whatever *GRAPH* is
bound to at the time (GH #53)."
```

and `node-graph-stamped-at-creation` (`:637-651`) for the pre-commit
half.

**The one path that can return an unstamped node.** `lookup-object`'s
transactional method can answer from a cache without going through
`lookup-node` (`transactions.lisp:327-334`):

```lisp
    (let ((local-cache (local-cache transaction))
          (graph-cache (graph-cache transaction)))
      (let ((local (gethash id local-cache)))
        (if local
            local
            (let ((value (or (gethash id graph-cache)
```

`graph-cache` is `(cache graph)`. Its five writers are
`finalize-node` (`primitive-node.lisp:212`, stamps),
`lookup-node` (`:357`, stamps), `save-node`'s old-version cache
(`:382`), `save-node`'s new-version cache (`:404`), and
`apply-tx-write :after` (`transactions.lisp:945-947`, whose primary
method already stamped). Only `save-node:382` caches a node it never
stamped — `old-node` comes straight from `%lhash-get`.

**That path is unreachable for claims.** `save-node` has exactly three
call sites: `save-edge` twice (`edge.lisp:283,287`) and `clos.lisp:53`,
which is in the file C3 shows is never loaded (and calls it with the
wrong arity anyway). Claims are vertices written through the transaction
path. So for S3: **no path `index-lookup` can take in the rules code
returns a node with `node-graph` NIL**, and S3-P3's `resolve-node-graph`
fallback is belt-and-braces rather than load-bearing. Keep it — the
guarantee above is about today's call graph, not an invariant the engine
states.

**Citation drift:** the plan's `finalize-node` `:208-212` is exact;
`ensure-node-bytes` runs `:216-232`, not `:216-225`.

## B4 — reading a foreign claim's slots performs no `lookup-object` — CONFIRMED (mechanism corrected, C3; stronger constraint found, C4)

The chain, with C3's correction applied:
`claim-subject-key` &c. are plain `:accessor`s on the claim classes
(`spacetime/claim.lisp:148-183`, `+claim-shared-slots+`), so a read goes
`slot-value-using-class :around ((class node-class) ...)`
(`primitive-node.lisp:506-516`, quoted in C3) → `node-slot-value`
(`primitive-node.lisp:441-449`):

```lisp
(defun node-slot-value (node key &key (graph *graph*))
  ;;(log:info "GETTING ~A FOR ~A" key (string-id node))
  (maybe-init-node-data node :graph graph)
  (when (consp (data node))
    (cdr (assoc (if (keywordp key)
                    key
                    (intern (symbol-name key) :keyword))
                (data node)))))
```

→ `maybe-init-node-data`, which reads a **heap through an mmap**, never
`lookup-object`, and resolves that heap through the node's own graph
(`primitive-node.lisp:300-322`):

```lisp
    ;; DATA-POINTER is an address in the node's OWN heap; GRAPH (ultimately
    ;; *GRAPH*) is only a fallback for an unstamped node (GH #53).  Resolved HERE
    ;; rather than around the whole body because this is the only use of it and
    ;; this branch runs once per node, while the body runs on every slot access.
    ...
                    (read-bytes (make-mpointer
                                 :mmap (memory-mmap
                                        (heap (node-home-graph node graph)))
                                 :loc (data-pointer node))))))
```

with `node-home-graph` (`node-class.lisp:453-458`):

```lisp
(defun node-home-graph (node &optional (default *graph*))
  "NODE's graph, or DEFAULT when unknown. Use instead of a bare *GRAPH* when
resolving a node's heap, tables or schema (GH #53)."
  (if (slot-boundp node 'graph)
      (or (node-graph node) default)
      default))
```

Note the `:graph` argument does **not** flow from the accessor:
`slot-value-using-class` calls `(node-slot-value instance
slot-keyword-name)` with no `:graph`, so the fallback is `*graph*` —
irrelevant when `node-graph` is set, which B3 shows it is.

The two derived readers named in the brief add nothing.
`claim-identity-key` (`spacetime/claim-query.lisp:29-59`) reads
`*claim-families*` and six slot accessors and formats a string;
`claim-extent` (`spacetime/claim-query.lisp:331-336`):

```lisp
(defun claim-extent (claim)
  "CLAIM's TEMPORAL-EXTENT, decoded from the stored sexp, or NIL.  The stored
form is EXTENT-SEXP; the two never share a name so neither is mistaken for
the other (design §7)."
  (let ((s (claim-extent-sexp claim)))
    (when s (sexp->extent s))))
```

**Answer to the brief's conditional:** no lazy slot read can call
`lookup-object`, so the cross-graph error is not reachable this way and
S3-P3 is not forced by it. It *is* forced by the pin — see C4 — and the
practical instruction is the same: materialise inside the snapshot.

## B5 — `graph-name`, `lookup-graph`, `resolve-node-graph`, and cl-llm's `store-name` — CONFIRMED (with one trap)

`make-graph` passes NAME through verbatim (`graph.lisp:483`):

```lisp
             :graph-name name
```

into the slot (`graph-class.lisp:96`):

```lisp
  ((graph-name :accessor graph-name :initarg :graph-name)
```

and registers under the same value (`graph.lisp:534`):

```lisp
        (setf (gethash name *graphs*) graph)
```

`lookup-graph` is a bare `gethash` (`graph-class.lisp:511-512`):

```lisp
(defun lookup-graph (name)
  (gethash name *graphs*))
```

on a table whose test is `equal`, not `eq` (`graph-class.lisp:3-12`).

**Trap: nothing coerces NAME to a keyword.** "the keyword `make-graph`
was given" is true by *convention* — every fixture and every consumer
passes one — not by enforcement. A store named with a string would
register and look up fine and would break cl-llm's `store-name`, which
calls `symbol-name` (`~/work/cl-llm/memory/schema.lisp:49-51`):

```lisp
(defun store-name (graph)
  "GRAPH's name as the string a model sees: downcased (SS5)."
  (string-downcase (symbol-name (gdb:graph-name graph))))
```

S3-P3 adopts exactly this convention, so the S3 docs should say a store
in a scope must be keyword-named, or `%store-name` should be written to
tolerate a non-symbol. Recommend the former: it matches every existing
fixture and the consumer.

`resolve-node-graph`'s signature is as the plan states
(`interface.lisp:7-42`):

```lisp
(defun resolve-node-graph (id &key class-hint)
  "The open store holding ID, as (values GRAPH STATUS STORE-ID) with
STATUS one of :RESOLVED, :DETACHED (registry knows the tag, no open
graph carries it) or :UNKNOWN.  ...
```

Its trap, worth carrying into any S3 fallback: "a foreign-minted v8 id
(peer hub, cross-system restore) whose tag coincides with a local
store-id resolves to the WRONG graph yet reports :RESOLVED (GH #209)."
Per B3 this fallback should never fire in the rules path.

The consumer's own convention, for S3-P4's mirror
(`~/work/cl-llm/memory/trace.lisp:190-204`):

```lisp
(defun %store-in-scope (name scope)
  (find name scope :key #'store-name :test #'string=))

(defun %resolve-in (cite store-name graph scope at)
  "CITE resolved in the store its evidence claim named, when that store
is in SCOPE; unit-1 evidence (no store) resolves in GRAPH; a store out
of scope is :ABSENT (SS4.3).  ...
```

— a `string=` on the downcased name, and "out of scope" is a *state*,
not a drop. S3-P4 drops instead; that is a deliberate divergence
(`premises-of` returns claims, not records) and should be stated in
`docs/rules.md` rather than left to be discovered.

## B6 — a second `def-claim-classes` under another store name — CONFIRMED (nothing is cleared)

`def-claim-classes` (`spacetime/claim.lisp:321-483`) takes `graph-name`
literally in every declaration it emits — `def-vertex` ×3,
`def-value-constraint` ×2 (×3 when temporal), `def-unique` ×2,
`def-index` ×4 — e.g.:

```lisp
       (graph-db:def-unique ,unary ,unary-slots
         ,graph-name :name claim-unary-identity
         ,@(identity-options unary-slots))
```

and each registry keys on the graph name and substitutes only within
that key's list. `%register-node-type-meta` (`schema.lisp:600-609`):

```lisp
(defun %register-node-type-meta (meta)
  "Put META in *SCHEMA-NODE-METADATA* under its own store, replacing any
entry for the same class IN PLACE.  A class may be registered under more
than one store (#186); only this store's list is touched, and position
still governs UPDATE-SCHEMA's instantiation order (GH #53, #167)."
```

`register-index-spec` (`index.lisp:150-163`):

```lisp
  (let* ((g (index-spec-graph-name spec))
         (id (index-spec-identity spec))
         (existing (gethash g *schema-index-metadata*))
         (hit (find id existing :key #'index-spec-identity :test #'equal)))
    (setf (gethash g *schema-index-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
```

`register-unique-tuple-spec` (`unique-constraint.lisp:285-296`) is the
same shape on `(unique-tuple-spec-graph-name spec)`. **Nothing in the
macro or in any of the three registries touches another graph name's
entries.**

The family struct is re-registered under the same parent symbol, and
replaces silently because the arity class names are identical
(`spacetime/claim.lisp:34-48`):

```lisp
(defun %register-claim-family (parent unary binary temporal-p)
  "Register PARENT's family.  Re-declaring with the same UNARY and
BINARY names replaces silently (a flipped :TEMPORAL included); different
names signal CLAIM-FAMILY-CONFLICT and leave the entry as it was, since
replacing it would orphan every existing claim of the family (GH #323)."
```

Note `*claim-families*` is image-wide, not per store — which is exactly
why C1 bites: `claim/7` will be asked for a family a store in scope does
not index.

**S2's C2 holds, and is the reason the plan's `:graph`-on-every-call
constraint exists.** `%install-node-type` reinstalls the helpers with
the *new* store baked in (`schema.lisp:747`):

```lisp
    (%install-node-helpers name kind (node-type-graph-name meta))
```

the constructor closes over that name (`schema.lisp:482-484`, `:499`):

```lisp
(defun %make-constructor-closure (name graph-name kind)
  "The MAKE-<NAME> function for node type NAME of KIND in store
GRAPH-NAME (GH #172)."
...
        (let ((graph (%default-store-graph name graph-name graph)))
```

and `%default-store-graph` ignores `*graph*` by design
(`schema.lisp:396-402`):

```lisp
(defun %default-store-graph (class-name store-name explicit)
  "R1 resolution: EXPLICIT graph if given, else the OPEN graph named
STORE-NAME, else refuse -- never *GRAPH* (GH #167)."
```

There is one `fdefinition` per class symbol, so after
`(def-claim-classes rt-claim :graph-db-rules-b)` a bare
`(make-rt-claim-binary ...)` writes into B whatever `*graph*` or the
enclosing `with-transaction` says. The plan's Global Constraint ("every
constructor call passes `:graph`") is therefore load-bearing, including
in `seed-*` helpers written for store A.

**Two fixture details confirmed as consistent with what ships.** The
plan's `(setf (gethash :graph-db-rules-b *schema-node-metadata*) nil)`
preamble matches the existing pattern in `tests/rules/suite.lisp:26-29`
("This file owns the graph name; the clear is what lets a second file
claiming it be seen (GH #198)") and is repeated there for
`:graph-db-rules-test-foreign` and `:graph-db-rules-norule`. And S2's
A4 findings on `%warn-if-cross-file-clobber` /
`%warn-if-divergent-across-stores` still apply unchanged: both stay
silent for two expansions in one file with identical slot sets.

**Citation drift:** the plan says `spacetime/claim.lisp:376-484`; the
macro is `321-483` (the emitted `progn` begins at `:374`).

## B7 — the clock fixture, and what `call-with-read-snapshot` reads — CONFIRMED for the mechanics; see C2 for what it does NOT buy

`:SYSTEM-CLOCK` is a real `make-graph` keyword whose default is the
special (`graph.lisp:618`):

```lisp
                                   (system-clock *system-clock*)
```

documented in the same lambda list's docstring, and applied by attaching
after the transaction manager exists (`graph.lisp:585-586`):

```lisp
      (when system-clock
        (attach-to-system-clock graph system-clock))
```

which stores it on the graph (`transactions.lisp:3264`,
`(setf (%graph-system-clock graph) clock)`), the slot being
`graph-class.lisp:223-229`:

```lisp
   (system-clock :reader graph-system-clock
                 :accessor %graph-system-clock
                 :initarg :system-clock :initform nil)
```

**What `call-with-read-snapshot` reads is the GRAPH's attached clock,
not the special.** `create-transaction` → `tm-current-epoch` →
`tm-clock` (`transactions.lisp:3179-3183`):

```lisp
(defun tm-clock (transaction-manager)
  "TRANSACTION-MANAGER's image clock, or NIL for its own counter (GH #168).
A transaction-manager always has a graph -- INITIALIZE-INSTANCE :AFTER
dereferences it immediately, so a NIL graph dies there first."
  (graph-system-clock (graph transaction-manager)))
```

So **`*system-clock*` need not be bound at snapshot time** — it is read
only as `make-graph`'s / `open-graph`'s keyword default
(`graph.lisp:618`, `:1120`). A fixture that opens the clock and passes
`:system-clock clock` to each `make-graph` is sufficient and is the
shape the engine's own tests use.

The fixture shape is confirmed, with the two additions the plan's sketch
omits — `with-clock-system-dir` (a `*system-directory*` binding) and
`with-temp-directory` for the clock —
(`tests/system-clock-tests.lisp:390-419`,
`two-stores-on-one-clock-get-disjoint-ordered-epochs`):

```lisp
  (with-clock-system-dir ()
    (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let ((ga (make-graph :sc-alpha (namestring da)
                                     :buffer-pool-size 1000
                                     :system-clock clock))
                     (gb (make-graph :sc-beta (namestring db)
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      ...
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock))))))
```

`close-graph` on both, `close-system-clock` last, as the plan says.
`with-clock-system-dir` is not needed in `tests/rules/suite.lisp`:
`run-rules-tests` already binds `graph-db::*system-directory*` for the
whole run (`tests/rules/suite.lisp:14-17`).

**But the instant claim is refuted — see C2.** `with-clocked-stores`'
docstring must not say the snapshots "share an instant".

**Citation drift:** the plan says `tests/system-clock-tests.lisp:395-420`;
the test is `390-419`.

## B8 — `map-vertices` on a foreign store — CONFIRMED, and only because the call passes `:VERTEX-TYPE`

The typed scan resolves each id through `lookup-vertex`
(`vertex.lisp:234-240`):

```lisp
      (flet ((scan-type-id (type-id)
               (let ((index-list (get-type-index-list (vertex-index graph) type-id)))
                 (when index-list
                   (map-index-list
                    (lambda (id)
                      (let ((vertex (lookup-vertex id :graph graph)))
```

so it inherits B1's refusal inside a read-write transaction on another
store, and B2's resolution outside one. It also holds a read pin and
materialises when collecting (`vertex.lisp:226-233`):

```lisp
         ;; When collecting, each node ESCAPES the scan pin, so materialize its
         ;; bytes before FN sees it.  For a side-effect scan FN runs inside the
         ;; pin, so its lazy reads are already safe and we don't pre-read bytes.
         (fn (if collect-p
                 (let ((user-fn fn))
                   (lambda (node) (ensure-node-bytes node graph) (funcall user-fn node)))
                 fn)))
    (with-read-pin (graph)        ; retain whatever versions this scan observes
```

**The untyped scan is a different animal and must never be used here.**
`map-vertices`' own docstring (`vertex.lisp:204-209`):

```lisp
NOTE: the fully-untyped scan (no :VERTEX-TYPE and no :INCLUDE-VERTEX-TYPES) walks
the raw vertex lhash, which reads LIVE node versions and so BYPASSES MVCC
snapshot isolation.  It is intended for back-end / admin passes (backup, GC,
reindex) run while the graph is quiescent; a typed scan goes through the type
index + LOOKUP-VERTEX and is snapshot-consistent.  (This is why IS-A/2 enumerates
per-type instead of using the untyped scan.)
```

and that branch stamps `node-graph` by hand and never calls
`lookup-object` (`vertex.lisp:265-272`), so it would neither signal in a
foreign transaction nor honour a snapshot.

S1's call is typed, so S3-P6 is safe as written
(`rules/facts.lisp:111-115`):

```lisp
  ;; :INCLUDE-SUBCLASSES-P defaults to T, so the parent covers unary and
  ;; binary.  :COLLECT-P is what materialises node bytes before a node
  ;; escapes the scan's read pin (vertex.lisp) -- not a style choice.
  (let ((parent (graph-db.spacetime:claim-family-parent family)))
    (map-vertices #'identity graph :vertex-type parent :collect-p t)))
```

The task text should carry the constraint explicitly: **the per-store
walk must keep `:vertex-type` and `:collect-p t`.** As noted in C1, this
route also degrades correctly for a store lacking the family:
`resolve-node-type-ids` skips unregistered designators, so the scan
visits nothing and signals nothing.

**Citation drift:** the plan says `vertex.lisp:185-233`; `map-vertices`
runs `185-277`.

## B9 — `%overlay-transaction` is unreachable from the functors — CONFIRMED

`%overlay-transaction` (`spacetime/claim-query.lisp:196-219`) has
exactly two call sites in the tree: `claim-query.lisp:298`, inside
`claims-touching`, and `claim-query.lisp:445`, inside
`claims-by-producer`. It reads the ambient transaction:

```lisp
  (let ((tx graph-db::*transaction*))
    (if (null tx)
        all
        (let* ((view (graph-db:make-commit-view graph tx))
```

Neither function is reached from `claim/7` or `claim-producer/2`.
`claim/7`'s routes call `index-lookup` directly (quoted in C1) or
`%unbound-claim-scan`; `claim-producer/2` goes through
`%producer-candidates`, which also calls `index-lookup` directly, not
`claims-by-producer` (`rules/facts.lisp:233-252`, quoted in C1).

So S2's A6 extends to scope unchanged: **a read of any store during a
rule's evaluation sees committed state only**, and under S3-P2's
composed snapshots it sees committed-as-of-that-store's-snapshot state.
The corollary S2 recorded still holds and gains a cross-store edge:
`*transaction*` is NIL during a cross-store evaluation, so
`%overlay-transaction` would be a no-op even if a rule body reached for
`claims-touching` — a body that did so under S2 (inside the
transaction) and under S3 (outside it) gets *different* visibility of
its own store's uncommitted writes. Worth a line in `docs/rules.md`
under "What the functors do not see".
