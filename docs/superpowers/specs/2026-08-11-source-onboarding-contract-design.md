# The source onboarding contract, and endpoint resolution — design

**Unit:** S1c, the third of three decomposed from #108.
**Issue:** [#132](https://github.com/kraison/vivace-graph/issues/132).
**Depends on:** S1a (#130) and S1b (#131), both merged.
**Split out:** [#138](https://github.com/kraison/vivace-graph/issues/138) — registration, deferred until a tenant exists.
**Programme:** `cl-llm/docs/superpowers/specs/2026-08-09-spatiotemporal-substrate-programme-design.md`.

---

## 1. What this is

The **declaration** half of what #132 originally held: what a source says about
itself, and the one capability that falls out of it.

A source declares seven facets — identity, space, time, attribution,
sensitivity, registration, indexed text — and **every facet supports an explicit
`:none`**, so that a missing declaration is a contract violation rather than a
source that silently never registers.

### 1.1 Why registration was split out

#132 as filed held two different kinds of thing. The contract is a
**declaration protocol**; registration is a **geometric computation**. They meet
at one facet out of seven.

Registration is also where the boundary rule is most likely to break. Stage A
describes it as binding "to the place spine" — but a *spine* is a tenant's
concept. The domain-neutral form is roughly "bind a record's geometry to regions
in a registry, with overlap fractions", and that abstraction has never served a
second tenant. Designing it now means designing against one imagined consumer,
which is the failure the three-way decomposition of #108 was drawn to prevent.

It is #138, deferred until at least one tenant exists (P2).

### 1.2 Boundary rule

Nothing in this unit may name a concept from any tenant application. With
registration split out, the highest-risk part of the original scope is no longer
here.

---

## 2. Enforcement is structural

`def-source` **defines** the class, wrapping `def-vertex` as
`def-claim-classes` does, and requires all seven facets. Omitting one is a
macroexpansion error.

A class defined with plain `def-vertex` simply **is not a source**. That is
correct rather than a gap: participation is opt-in, and "which classes are
sources?" is answered by "those defined with `def-source`" instead of by a
second registry that could disagree.

Enforcing at declaration time was chosen over a conformance sweep at graph open,
an on-demand checker, or a lazy check at first use. All three leave a
non-conforming class *definable*, and the failure this facet set exists to
prevent is precisely a declaration that was never made and never missed.

```lisp
(def-source incident-report :my-graph
  ((headline :initarg :headline) (body :initarg :body))
  :identity      (:namespace :reports :key-slot report-id)
  :space         (:geometry-slot where :kind :point :precision :exact)
  :time          (:extent-fn report-extent)
  :attribution   (:licence "CC-BY-4.0" :citation "Example Reports")
  :sensitivity   (:class :public)
  :registration  :none
  :indexed-text  (:text-fn report-indexed-text))
```

**`:none` and undeclared are different states by construction.** The first is a
value the macro accepts; the second does not compile.

---

## 3. The facets

### 3.1 Identity — the only facet with structural consequences

```
:identity (:namespace <keyword> :key-slot <slot-name>)
```

`:namespace` says which namespace this class's records live in; `:key-slot`
names the stable **external** key — not a node id. A node id is a location; an
external key is an identity.

`def-source` emits a `def-index` on the key slot and registers the class under
its namespace. That pairing is exactly what makes §4's resolution possible, and
it is why #131 deferred resolution to this unit rather than pull this facet
forward and split the contract across two units.

**An external key must be unique within its namespace.** Several classes may
share a namespace; two answering the same key is a contract violation, handled
in §4.2.

### 3.2 Sensitivity — declared here, enforced above

```
:sensitivity (:class <keyword>) | :none
```

`:none` is accepted — §1's rule holds for all seven facets without exception —
and **means most-restricted, not unrestricted**. A source that declines to state
a disclosure class gets the safest one, not the loosest.

That reading is what makes the facet consistent with itself. Rejecting `:none`
here was the first draft of this spec and was wrong twice over: it broke the
uniform "every facet is `:none`-able" rule that makes the contract learnable,
and it offered no better outcome than a fail-closed default already gives.

**Fail-closed means an unrecognised class also compares as most-restricted**,
never least. The substrate ships a predicate for consumers to call and **enforces
nothing itself**: the things that would enforce it — retrieval, export — live
above `graph-db` entirely, and inventing an enforcement point here would mean
inventing a source-to-claim data flow this unit has no other reason to build,
and putting an access-control decision in an engine with no notion of a
principal.

The spec and the manual must both say plainly that a tenant which never calls
the predicate gets no protection.

*Why this facet exists at all.* A provenance rule meant to exclude restricted
material was a **silent no-op** on a real filesystem for NFC/NFD reasons, and
its unit tests passed its entire life because the fixtures used the same string
literals the code did. A declared, fail-closed class is how that stops being a
per-source ad-hoc rule that can quietly fail to fire. **Any sensitivity rule
must be proven against a real corpus, never synthetic fixtures.**

### 3.3 Registration — required, and uninterpreted here

```
:registration <opaque> | :none
```

Stored verbatim; consulted by nothing until #138. A spatial tenant declaring its
real registration and getting no behaviour yet is better than being forced to
write `:none` and leave a lie in the record.

### 3.4 The remaining four

```
:space        (:geometry-slot <slot> :kind <keyword> :precision <keyword>)
              | :none
:time         (:extent-fn <function-name>) | :none
:attribution  (:licence <string> :citation <string>) | :none
:indexed-text (:text-fn <function-name>) | :none
```

`:time`'s function returns a `temporal-extent` (S1a) — a function rather than a
slot because an extent is often derived from several slots.

`:indexed-text` names the function answering "what part of this record gets
embedded".

**A correction to an earlier draft of this spec, and to Stage A's framing.**
Both said this facet "generalises the existing `indexed-text` generic" and that
"a seventh of the contract already exists". It does not exist *here*: that
generic and its mixin live in a tenant application, not in `graph-db`. The
pattern is proven — one class answering for itself what part of it is
indexable — and that is why the contract adopts it. But the substrate is
adopting a tenant's good idea, not generalising its own existing code, and
saying otherwise in this unit's spec is the boundary rule slipping in the unit
whose whole job is boundary discipline.

---

## 4. Endpoint resolution

```lisp
(resolve-endpoint namespace key)   ; => node, or NIL
```

Find the classes registered under `namespace`, `index-lookup` each on its key
slot.

**A namespace with no registered source classes signals**, and a key that
matches nothing in a known namespace returns `NIL`. Those are different
failures: the first is a typo or an unloaded system — a programming error — and
the second is an ordinary miss. Collapsing both to `NIL` would make a
misspelled namespace indistinguishable from an absent record, which is this
programme's recurring defect in miniature.

**A namespace is not a graph name.** An earlier draft of this section said it
was, and that a namespace could be resolved with `lookup-graph`. It cannot:
`*graphs*` is keyed by the name `make-graph` was called with, while
`*namespace-sources*` is keyed by the `:namespace` a class declares. They are
two registries and nothing keeps them in step — the claim was found false by
running the code, not by reading it.

What actually holds the mapping is the **class**: `def-source` records the
graph it was declared in, so resolution takes the graph from
`source-facets-graph` per class rather than from the namespace. That is also
more correct than the draft, because two classes sharing a namespace need not
share a graph.

Namespaces are therefore, today, **labels that group source classes** —
nothing more. #110 gives them a real identity, and can do so without changing
this signature, which is what makes #131 §6.1's "two implementations" a swap
rather than a rewrite.

This completes a pair. #131 shipped the inverse — `claims-touching`, answerable
from the claim graph's own indexes with no cross-graph read at all. This is the
forward direction, which needs the identity contract and so could not exist
until now.

### 4.1 It must not run inside a read-write transaction

Resolution can cross graphs, and the 3.0 contract permits cross-graph reads only
from a read-only snapshot or outside a transaction.

`resolve-endpoint` therefore **signals if called inside a read-write
transaction**, rather than letting `cross-graph-transaction-error` surface from
somewhere deeper. The caller's mistake is the call site, not the lookup.

This is the same constraint that shaped #131: claim generation resolves its
endpoints *before* opening its write transaction, which storing endpoints as
values satisfies by construction.

### 4.2 Two classes answering one key is a violation

`def-unique` cannot catch this — the classes have different owners and the
constraint registry keys on owner. So resolution detects it and **signals,
naming both classes**.

Returning the first would make which record you get depend on class-definition
order, which is the kind of silent, order-dependent answer this programme
exists to eliminate.

---

## 5. What this unit does, and what it only records

**Six of the seven facets are declaration-only here.** Identity is the one with
behaviour — it emits the index and drives resolution. Space, time, attribution,
registration and indexed-text are declared, shape-validated, stored, and
consulted by nothing inside `graph-db`. Sensitivity adds a predicate the
substrate itself never calls.

This must be stated in the manual, because the natural assumption is the
opposite. The contract's value in this unit is that declarations are
**mandatory, uniform and machine-readable**; acting on them happens above — in
registration (#138), retrieval (S5), and each tenant's ingest.

```lisp
(source-contract <class-name>)   ; => the declared facets
```

is how those layers read a class's declarations, instead of each inventing a
per-source convention. **It signals for a class that is not a source**, rather
than returning `NIL`: "this class declared nothing" and "this class is not a
source" are different facts, and a consumer that gets `NIL` for both would
treat an unconverted class as a conforming one with empty facets.

---

## 6. Testing

**Conformance by construction.** Omitting each of the seven facets fails to
expand. Seven tests, and they are the unit's reason for existing.

**Absence-vs-value, continued.** `:none` and undeclared must be distinguishable,
and neither may read as a value. This is the third consecutive unit where that
is a standing category rather than an afterthought.

**Fail-closed.** An unrecognised sensitivity class compares as most-restricted.
If that test is ever inverted the facet becomes worse than nothing, because a
caller would be trusting it.

**Ambiguity.** Two classes in one namespace answering the same key signals, and
the condition names both.

**The transaction guard.** `resolve-endpoint` inside a read-write transaction
signals at the call site.

**Resolution proper.** A declared source resolves by external key; a wrong
namespace returns `NIL` rather than a near-miss.

---

## 7. Acceptance

Unchanged, and still not this unit's own suite: **P2's two tenants**, one
declaring `space: none`, which is what proves the spatial facets are genuinely
optional rather than merely defaulted.

What this unit's suite establishes is narrower: a non-conforming source cannot
be defined, `:none` is distinguishable from silence, and an endpoint resolves to
exactly one node or to nothing.

---

## 8. Version floor

No change to any file in `graph-db/core`. No existing consumer's version floor
moves; the floor applies only to a consumer that wants to
`:depends-on (:graph-db/spacetime)`, which S1a already established.
