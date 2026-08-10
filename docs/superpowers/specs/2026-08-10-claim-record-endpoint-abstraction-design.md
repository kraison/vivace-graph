# The claim record and the endpoint abstraction — design

**Unit:** S1b, the second of three decomposed from #108.
**Issue:** [#131](https://github.com/kraison/vivace-graph/issues/131).
**Depends on:** S1a (#130), merged — `standing`, `temporal-extent`, the Allen algebra.
**Sibling:** #132 (the source onboarding contract and registration).
**Programme:** `cl-llm/docs/superpowers/specs/2026-08-09-spatiotemporal-substrate-programme-design.md`.

---

## 1. What this is

The reified claim: a relation stored as a **node**, carrying the provenance an
edge cannot. Reifying buys three properties, each load-bearing — contradictions
survive instead of being resolved away at write time; claims can be superseded
and versioned, so a rule change is a regeneration rather than a migration; and
claims are regenerable and therefore disposable.

### 1.1 Boundary rule

Nothing in this unit may name a concept from any tenant application. A design
decision justified *only* by what one tenant needs belongs in that tenant.

---

## 2. There are no edges

A claim's endpoints are **values it carries**, not graph references. Not a
convenience — the 3.0 multi-graph contract forbids the alternative.

A read-write transaction is single-graph, and touching a foreign node inside one
signals `cross-graph-transaction-error` (`transactions.lisp:229`). A claim that
was an edge to a node in another namespace could not be created at all.

The programme's §6.2 states the derived rule: **claim generation resolves its
endpoints before opening its write transaction.** Storing endpoints as external
identity satisfies that *by construction* — there is nothing left to resolve
inside the transaction, so the rule cannot be violated even carelessly.

§6.1 records why this does not paint us into a corner: the endpoint abstraction
has two implementations — external-key designators today, real edges after the
namespace work (#110) — with identical semantics. Only the physical
representation changes.

---

## 3. The class hierarchy

### 3.1 Why a hierarchy, and not a nullable object

`def-unique` **exempts any tuple containing a null** — SQL's
unknown-never-equals-unknown, chosen deliberately in #107. So a claim with a
nullable object slot would be exempt from its own uniqueness constraint whenever
the object was absent: binary claims would deduplicate, unary claims never
would, and regeneration would silently double every unary claim.

The obvious patch — a reserved non-null marker in the object slots — works, but
it spends the distinction S1a exists to protect. *"This relation is unary"* and
*"this claim has an object we could not determine"* are different facts. The
second is what `standing`'s three absence cases are for. Spelling both with one
marker reintroduces the absence-vs-value collapse in the first record built on
top of S1a.

Making arity a **type** avoids both problems. A unary claim cannot carry an
object because the slot does not exist, and both constraints become complete —
no nullable component anywhere — so the null-exemption never fires for claims at
all and stays available for genuinely unknown components.

### 3.2 The shape

```
claim            shared slots; NO uniqueness constraint; not for instantiation
├── unary-claim    def-unique (producer subject-namespace subject-key
                               relation)
└── binary-claim   adds object-namespace, object-key
                   def-unique (producer subject-namespace subject-key
                               object-namespace object-key relation)
```

**The unary constraint must sit on `unary-claim`, never on `claim`.** The parent
already has exactly the unary slot set, so declaring it there looks natural and
is wrong: `class-unique-tuple-specs` matches on `subtypep`, so it would bind
`binary-claim` too and forbid two binary claims sharing
`(producer, subject, relation)` but differing in object. That is an ordinary
one-to-many relation and must stay legal.

### 3.3 The parent is not for instantiation

`def-vertex` has no abstract marker, so this is signalled rather than enforced,
three ways:

- the macro generates constructors for the two subclasses only, never for the
  parent;
- the parent's docstring says it exists to hold shared slots and shared indexes;
- it carries no uniqueness constraint of its own.

`(make-instance 'claim)` still works and satisfies no constraint. It is visibly
wrong rather than quietly supported, which is the most the engine allows.

---

## 4. The macro

The subsystem ships a macro, not classes. It cannot ship classes: `def-vertex`
binds a node type to a graph name at definition time (`schema.lisp:460`) and
class names are globally unique across graphs, so a shipped class would collide
between tenants.

```lisp
(def-claim-classes site-claim :my-graph
  :extra-slots ((weight :type double-float)))
```

expands to three `def-vertex` forms and four declarations:

```
site-claim              parent -- shared slots + :extra-slots
site-claim-unary        no new slots
site-claim-binary       object-namespace, object-key

def-unique site-claim-unary  (producer subject-namespace subject-key
                              relation)
def-unique site-claim-binary (producer subject-namespace subject-key
                              object-namespace object-key relation)
def-index  site-claim         (subject-namespace subject-key)
def-index  site-claim-binary  (object-namespace object-key)
```

**`:extra-slots` go on the parent**, so both arities inherit them and the tenant
writes them once.

**The subject index is declared on the parent** and reaches both arities via
`subtypep`. **The object index is declared on `binary-claim`.** Declaring it on
the parent would also work — `%applicable-index-descriptors` requires every named
slot to exist in the class, so it would silently restrict itself to the subclass
that has them — but relying on a rule to rescue a declaration that reads as a
mistake is worse than putting it where it belongs.

---

## 5. The record

| slot | type | meaning |
|---|---|---|
| `subject-namespace` | keyword | which namespace the subject lives in |
| `subject-key` | string | the subject's stable **external** key, not a node id |
| `object-namespace` | keyword | `binary-claim` only |
| `object-key` | string | `binary-claim` only |
| `relation` | keyword | open vocabulary |
| `producer` | keyword | the rule or operator that made it — see §6 |
| `rule-version` | string | provenance, **not** identity |
| `method` | keyword | how it was derived — open vocabulary |
| `standing` | S1a `standing` | validated by `check-standing` at construction |
| `confidence` | double-float or nil | |
| `extent-sexp` | list | the stored form; read it through `claim-extent` (§7) |
| `geometry` | engine `geometry` | optional, unindexed here — see §7 |

The triple proper is three of these. Everything else is provenance, and that
asymmetry is the point: an edge carries the three and nothing else, which is why
an edge model must resolve contradictions at write time.

**`standing` appears twice in the design and means different things.** On the
claim it records how the claim came to be known; on its extent, how the *time*
was known. They vary independently — an `asserted` claim may carry an `observed`
extent.

---

## 6. Identity, contradiction, regeneration

### 6.1 `producer` excludes the version

`producer` names the rule or operator. `rule-version` is carried as provenance
and is never part of identity.

The alternative — folding the version into `producer` so v1 and v2 claims
coexist — makes rule changes auditable in place, but every reader must then
filter to the current version, growth is unbounded until something retires old
versions, and the regeneration sweep needs a second version-blind pass to avoid
stranding v1's claims.

### 6.2 What the constraint permits, deliberately

- **Two producers disagreeing.** Rule R and operator O assert different objects
  for the same subject and relation. Different `producer`, both persist. This is
  the entire reason for reifying.
- **One producer, many objects.** The object is in the binary tuple, so these are
  distinct claims — an ordinary one-to-many relation.

### 6.3 What it forbids

The same producer asserting the identical claim twice. At commit this signals
`unique-constraint-violation` under the transaction-manager lock, so two
concurrent producers racing the same claim resolve to exactly one winner.

### 6.4 Regeneration is sweep-then-insert

**And the constraint is not what makes it work.** If a rule stops producing a
claim it used to produce, no amount of upserting removes the orphan — only an
explicit delete does. So the unit ships:

```lisp
(delete-claims-by-producer graph claim-class producer)
```

`claim-class` is the **parent** class name, so one call sweeps both arities;
`producer` is matched exactly. Regeneration is: sweep, then insert.

**The sweep and the insert must be two separate transactions.** This is not a
style preference — a single transaction cannot work. `validate-unique-
constraints` is a **pre-durability** check, while the key release for a deleted
node happens in `apply-tx-writes-to-unique-indexes`, which runs
**post-durability**. So within one transaction the insert's uniqueness check
runs *before* the sweep's release is visible, and re-inserting an unchanged
claim collides with the copy the same transaction just deleted.

Sweep, commit, then insert. Found while building the sweep (GH #131); the
ordering is visible in `%commit`, `transactions.lisp`.

The constraint's job is narrower and still worth having — it stops a buggy or
racing producer duplicating within a run, which a sweep cannot.

**Supersession is regeneration.** There is no second mechanism.

---

## 7. Storage

**The extent is stored as its sexp.** S1a's `extent->sexp` emits only values the
engine already serializes, so the persistent slot — named `extent-sexp`, so the
stored form and the decoded value never share a name — holds that list and **no
serialize type byte is reserved**. S1a's no-core-change property carries forward.

`claim-extent` decodes on read and `(setf claim-extent)` encodes on write, so
callers see a `temporal-extent` and never the wire form. Nothing stops a caller
reaching `extent-sexp` directly; it is not hidden, only unnecessary.

Decoding on every read is a real cost. It is cheap enough to ignore until
measured; the programme's §11 discipline is to measure rather than guess.

**Geometry uses the engine's existing type and is not indexed here.** Whether
claims should be spatially indexed is a registration question, and registration
is #132.

---

## 8. The inverse query

```lisp
(claims-touching graph claim-class namespace key &key role)
```

Returns the claims naming that endpoint — a union of the two index lookups from
§4. `claim-class` is the **parent** class name, so one call covers both arities.
`:role` is `:subject`, `:object`, or `:either` (the default); `:object` searches
only `binary-claim`, which is where those slots exist.

A graph may host several unrelated claim families, so the class is a required
argument rather than inferred — there is no "the claim class" for a graph.

It answers **entirely from the claim graph's own indexes**: no cross-graph read,
no snapshot, no resolution. That is what makes it implementable in this unit.

---

## 9. Deferred to #132, with reasons

**`precision` and `fraction`.** Both are registration concepts. `fraction` is
described purely in registration terms; `precision` is "inherited from the weaker
endpoint", but endpoints here are a namespace and a string and have no
precision. Shipping a slot nothing can fill invites someone to fill it with a
guess. Adding a persistent slot later is a supported schema change, and #128
settled what a node written before the slot reads back as.

**Endpoint resolution** — turning `(namespace, key)` into a node. Resolution
requires a way to find a node by its external key, which means requiring every
tenant node class to declare and index a stable external key. That requirement is
already the **first facet of #132's onboarding contract**: *"Identity — a stable
external key."* Implementing it here would pull the contract's identity facet
forward and split the contract across two units, which is the boundary problem
the three-way decomposition existed to prevent.

§6.1's "two implementations" claim therefore goes untested in S1b. It is
untestable here regardless: with no identity contract there is nothing to resolve
*against*, so a resolver written now would be written against an imagined tenant.

**#132 inherits three obligations from this document**: add `precision` and
`fraction`; implement endpoint resolution against the identity facet; decide
whether claims are spatially indexed.

---

## 10. Testing

Four things carry the weight.

**The permit/forbid table, as tests.** Two producers disagreeing both persist;
one producer with many objects persists; the same producer twice is rejected. The
first two failing would mean we had built an edge with extra steps.

**The concurrency gate**, non-negotiable as in #107 — eight threads racing one
claim, exactly one commit and seven `unique-constraint-violation`. This unit
touches the commit path.

**Unary deduplication specifically.** A unary claim regenerating without
duplicating is what the hierarchy exists to achieve. That test must **fail
against a single-class-plus-nullable-object design** — it is the non-vacuity
check for the whole structural decision, and without it §3.1's argument is
untested.

**Absence-vs-value conformance**, as a standing category: a unary claim
(structurally no object) must be distinguishable from a binary claim whose object
is unknown.

---

## 11. Acceptance

S1b is **not proven by its own suite.** P2's two tenants are the acceptance test
for P1 as a whole, and the tenant declaring `space: none` is what proves the
geometry slot is genuinely optional rather than merely defaulted.

What this unit's suite establishes is narrower and still worth stating: identity
behaves as §6 describes, contradictions survive, regeneration leaves no orphans,
and structural absence stays distinguishable from epistemic absence.

---

## 12. Version floor

New files in an opt-in subsystem, no change to any file in `graph-db/core`. No
existing consumer's version floor moves. The floor applies only to a consumer
that wants to `:depends-on (:graph-db/spacetime)`, which S1a already established.
