# Disjointness — membership, ownership and trigger

**Status:** 4a IMPLEMENTED 2026-08-31 (`disjointness.lisp`: `def-disjoint`,
`*node-type-definition-hooks*`, `check-disjointness`); 4b open — the
membership-claim shape is undecided (see "What this note does NOT decide");
open question 3 is answered by the evaluator note: 4b reads membership
claims through the commit view when it is built
**Issue:** #157 (unit 4 of the ontology epic, #109)
**Branch:** `experiment` (no release; see #109)

#157 asks for a design note before a plan, on the grounds that "the wrong
answer to the first question makes the other two unanswerable". This answers
the three in the order the issue poses them, and the first answer does most
of the work.

## 1. What is membership?

There are two readings, and they are not two implementations of one feature —
**they are two different features, one of which is not a constraint family at
all.**

### Reading A — membership is the vertex type

A node's class is fixed at creation. `%MAKE-VERTEX` promotes a pooled
`VERTEX` to the target class via `CHANGE-CLASS` and nothing retypes it
afterwards: `UPDATE-NODE-TYPE` (`schema.lisp:283`) registers a *type* in a
store's schema table, it does not move a node between classes. There is no
retyping entry point in the public API.

So the set of classes a node instantiates is exactly the ancestors of its
class, and that set is determined entirely by the **class graph**, which is
fixed at schema-definition time.

**Therefore disjointness over vertex types is decidable statically, and no
node write can ever create or repair a violation.** Given declared-disjoint
`A` and `B`:

- if no defined class has both `A` and `B` among its ancestors, the
  constraint is vacuously true and always will be;
- if some class `C` has both, then *every* instance of `C` violates it, and
  the declaration contradicts the schema.

There is no third case, and neither case is a function of any node's data.

This directly contradicts #157's second stated difficulty — "a violation can
be created by a write to either side". For reading A that is false. It is
true only under reading B.

> Aside, worth fixing separately: `DEF-NODE-TYPE`'s docstring
> (`schema.lisp:763`) calls PARENT-TYPES "a single-inheritance superclass
> list", while `DEF-VERTEX`'s (`:803`) calls it "a list of other vertex types
> to inherit from". The macro does not enforce either. The conclusion above
> holds under both readings — with single inheritance the static check is "is
> one an ancestor of the other", with multiple inheritance it is "does any
> defined class have both as ancestors" — but the two docstrings should be
> made to agree.

### Reading B — membership is claim-asserted

A subject is asserted to belong to a class by a claim. Here disjointness
*is* a runtime constraint: two membership claims about one subject can be
written independently, each legal alone, and the second creates the
violation. This is the reading #157 describes.

It is also the reading that exists in the field. The deployed spatial tenant
keeps three mutually exclusive classes apart by **encoding the class into the
relation string** — 57,827 claims, 16.4% of that corpus, measured under #160.
The engine holds no membership representation at all today, so the tenant
smuggled one into a slot that was not meant to carry it.

### Recommendation

**Define membership as claim-asserted (reading B), and split unit 4.**

Reading A is not a constraint family; it is a **schema lint**. Building it
into the value-constraint machinery — a registry entry consulted per node at
commit — would spend the whole of unit 4's cost on a check whose answer
cannot change between one commit and the next.

| | 4a — vertex-type disjointness | 4b — claim-asserted disjointness |
|---|---|---|
| Nature | schema lint | constraint family |
| Decidable from | the class graph alone | the store's claims |
| Trigger | schema definition + audit | commit |
| Needs a membership representation | no | **yes — none exists** |
| Size | hours | the real unit |

4a is worth doing on its own and is nearly free: it turns a contradictory
declaration into an error at definition time instead of a silent no-op. It
should not be allowed to masquerade as having delivered unit 4.

## 2. What owns the spec?

#157 fears that disjointness "either fits `(owner . name)` or the rule needs
widening, and widening it touches three registries". **It fits, without
widening — if `:name` is made mandatory.**

`%SPEC-IDENTITY` (`index.lisp:99`) is `(cons owner-name (or name slot-names))`.
Its ⚠ says naming exists because "a macro that emits specs on a caller's
behalf cannot name what a previous version of itself emitted". Disjointness
has the same problem for a different reason — a *set* of classes has no
natural single owner — and the same remedy works:

- **Require `:name`.** Then identity is `(owner . name)` and `slot-names` is
  never consulted, so the fact that disjointness names no slot stops
  mattering.
- **Derive `owner` deterministically from the class set** — the set sorted by
  symbol name, first element — so that re-declaring the same set replaces
  rather than stacks, regardless of the order the caller wrote it in.

Symmetry is the trap here: `(disjoint a b)` and `(disjoint b a)` are the same
declaration and must not become two live specs. Canonicalising the set before
deriving the owner is what prevents that, and it is the one piece of this
that is easy to get wrong and silent when wrong (#139/#140 are the same
failure in the two registries that already had it).

A **fourth registry**, keyed like the other three and sharing
`%SPEC-IDENTITY` unchanged. Not a widening of the shared rule.

## 3. What triggers the check?

**4a:** schema definition/redefinition, plus an audit pass. Explicitly *not*
the commit path — there is nothing per-node to check. `AUDIT-SPATIAL-SLOTS`
(`spatial-query.lisp:428`) is the shape to copy, and `CHECK-VALUE-CONSTRAINTS`'
contract of returning a spec count so an unchecked schema cannot read as a
clean one (`value-constraint.lisp:217`) carries over.

**4b:** the commit path, in `%COMMIT`'s manager-locked pre-durability region
alongside the other validators (`transactions.lisp:3377-3401`) — but with the
same problem unit 3 (#156) has and units 1-2 do not: **it must read state the
transaction does not carry.** Checking a new membership claim means finding
the other membership claims about that subject, which is a store read inside
the commit path while holding the lock that serialises commits.

That is the same machinery #156 needs for domain and range, and it argues the
epic's ordering is right for a reason it does not state: **4b should inherit
#156's read-in-commit machinery rather than invent a second one.** Two
mechanisms for "read the other end during commit" that could disagree would be
the #140 mistake in a new place.

It also means 4b is *not* merely "the hardest of the four" as #157 has it —
it is the one with a hard dependency on #156's outcome. If #156 concludes that
reading the store inside the lock is unaffordable and pushes validation
elsewhere, 4b follows it there.

## Scope: one store

A disjointness declaration is **scoped to a single store**, deliberately.

The engine cannot honour a cross-store one: a read-write transaction is
single-store, validation runs on one store's manager, and there is no
two-phase commit (#93, decided as deferred with a documented compensating
pattern). A check that read another store during commit would be reading a
different consistency domain — racy, not merely slow.

This matters most under reading B, because claims hold endpoints as external
keys precisely so they can point anywhere. Cross-store membership is
therefore expressible almost by accident, and the spec must refuse it rather
than half-check it. Under reading A it cannot arise: a class graph is a
property of the image, but a node lives in one store.

If a genuine cross-store disjointness requirement appears, that is the trigger
condition #93 and #94 both name, and it reopens #93.

## What this note does NOT decide

- **The membership claim's shape.** 4b needs a representation for "subject S
  is a member of class C" and the substrate has none. Whether that is a
  dedicated relation, a slot on the claim, or a distinct claim class is the
  next decision, and it should be made with #160 in view: relation-as-class is
  the workaround 4b exists to retire, so the shape chosen must be one the
  deployed 57,827 claims can migrate onto.
- **Whether 4b is worth building at all** before a second tenant needs it.
  The registration abstraction (#138) was declined for resting on a single
  imagined consumer; 4b currently has exactly one real consumer, which is
  better than imagined but is not two.
- **Cost of the read-in-commit.** A measurement for #156 to make, not an
  assumption for this note.

## Testing

The trap #157 names is real and general to this epic: a test that declares
two unrelated classes disjoint and writes one node passes whether or not
anything is checked. The discriminating cases:

- 4a: a declaration naming two classes where one *is* an ancestor of the other
  must go red at definition time. Ablate the check and confirm it goes green —
  the vacuous version of this test is the default outcome, not the unlucky one.
- 4a: re-declaring the same set in the opposite argument order must **replace**,
  not stack. Assert on the registry length, not on behaviour, since two
  identical live specs behave identically until one is withdrawn.
- 4b: a second membership claim for a disjoint class must be refused **at
  commit**, and the refusal must survive close and reopen — the value-constraint
  work (#149) found a durable invalid value that construction-time checks
  missed, and this is the same shape.
- 4b: a cross-store membership claim must be refused as out of scope, with a
  condition distinguishable from "violates a disjointness constraint".

## Open questions

1. Does 4a belong in the ontology epic at all, or is it a schema-tools lint
   (`schema-tools.lisp`) that happens to share vocabulary?
2. Should 4a run at `DEF-DISJOINT` time, at `DEF-VERTEX` time (catching a
   later class that violates an existing declaration), or both? Both is
   correct and the second half is the one that is easy to omit.
3. If #156 lands a read-in-commit hook, is disjointness expressible *through*
   it rather than as a fourth registry?

## Addendum: 4b as built (2026-08-31, after the tenant answer)

The tenant's answer (on #157, mirrored as their #143) resolved every
question this note left open, and reshaped one premise: seen from the
claim store, the disjoint set is a set of **membership-object keys** —
values — not classes in that store's schema. 4a and 4b share vocabulary
and nothing else.

**The shape.** Membership is a binary claim: subject = the record's
external key, object = `(class-namespace, class-designator-key)`, one
canonical relation. The declaration ranges over object keys:

```lisp
(def-disjoint-membership claim-class graph-name
  :relation "instance-of"
  :object-namespace :classes
  :object-keys ("observation" "fortification" "unclassified")
  :name placemark-membership)
```

**The invariant**: at most one *live* (CLAIM-CURRENT-P) membership claim
per subject key within a declared set. Reclassification is
retract-then-assert; to make that atomic, `retract-claim` now JOINS an
ambient transaction instead of always opening its own — the whole
tenant flow `(with-transaction () (retract old) (make new))` commits or
fails as one unit, and the commit check evaluates against the
post-commit view, where the retracted sibling is no longer current.

**The check** runs at commit, in the manager-locked region, through the
commit view: for every written live membership claim of a declared set,
the subject's other claims are read via the family's subject index and
OVERLAID with the transaction's writes — a sibling retracted in the same
transaction does not count, one asserted in the same transaction does.
This is the view-lookup shape the evaluator note deferred "until a unit
needs it"; 4b is that unit, and the overlay lives with the check.

**The refusal** is `membership-disjointness-violation` — its own
condition, per the tenant's condition 3: distinct from
`value-constraint-violation` and catchable on its own. The scope note's
"cross-store membership must be refused" test is VACUOUS under this
shape and is recorded as such: a membership claim lives in the claim
store and names its subject by external key; there is no store
reference to cross.

**Where it lives.** The registry, declaration, check and audit are part
of the spacetime substrate (claims are its objects). Core's `%commit`
gains one generic seam — `*commit-validators*`, a list of
`(fn tx graph)` run beside the built-in validators — mirroring
`*node-type-definition-hooks*`; the substrate pushes its validator at
load. An image without spacetime pays one empty-list traversal.

**Audit**: `check-disjoint-memberships graph` sweeps the family's
claims, groups by subject, counts live members per declared set;
`(values violations checked spec-count)`, the usual contract.

**Migration**: none in place. Per the tenant, the deployed corpus is
derived and disposable — the writer emits the new shape and one
re-registration pass rebuilds; their sweep stays through a soak as
belt-and-braces.

