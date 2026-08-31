# Registration: binding geometry to a registry — design

**Issue:** [#138](https://github.com/kraison/vivace-graph/issues/138), split
from [#132](https://github.com/kraison/vivace-graph/issues/132), which kept
the declaration half.

**Consumer:** cl-llm#13 unit 3, the claim traversal. Registration claims are
what a fusion layer expands over on the spatial side.

**Status of the deferral.** #138 was held "until at least one tenant
exists", to avoid designing a domain-neutral abstraction against one
imagined consumer. Two tenants now exist: the place spine (#55, #65–#67)
and the map-less document tenant (cl-llm#12), which declares `space: none`
and is what proves the spatial facets are optional rather than defaulted.

---

## 1. What registration is

Binding a record's geometry to regions in a registry, and recording each
binding as a claim.

**Registration is partial and fractional, not boolean.** A point registers
to one region at fraction 1.0. A polygon registers to many, each with an
overlap fraction. A line *traverses* a sequence of regions, and the useful
fact is what fraction of it falls in each.

That framing is the spine's, adopted verbatim rather than re-derived: it
has survived 290,302 ACLED events and 341 sites in production.

## 2. Two slots move to the substrate

`precision-m` and `fraction` join `+claim-shared-slots+`:

- **`precision-m`** — double-float or nil, initform `nil`. How accurately
  the subject's position is known, **in metres**.
- **`fraction`** — double-float, initform `1.0d0`. How much of the
  subject's geometry falls within the region.

**Why on every claim rather than a tenant's `:extra-slots`.** Unit 3's
traversal is domain-neutral: it weights expansion by overlap without
knowing which tenant produced a claim. If `fraction` stays a tenant slot
with a tenant accessor, that reader has to know every tenant's accessor
names, which is the boundary rule breaking in the direction hardest to
undo. A `space: none` tenant carries both slots unset; that is the cost,
and it is the same cost the `geometry` slot already pays.

**`precision-m` is a real quantity, not a discount factor**, and it flows
in both directions — a source can be *finer* than the region it joins to,
not only coarser.

**It is not the `:space` facet's `:precision`.** That one is a keyword
declared per source, describing a source's geometry. This one is a
magnitude computed per claim. The `-m` suffix keeps the two apart; do not
"harmonise" the names.

## 3. The registration facet gains a payload

#132 defined `:registration <opaque> | :none`, stored verbatim and
consulted by nothing. Every source declared today — including the spatial
ones — says `:none`, because there was nothing to say yet. This unit
defines what it carries:

```
:registration (:registry <class>             ; region class, a DEF-SOURCE
               :registry-namespace <keyword> ; where those regions live
               :claim-class <symbol>         ; the claim family to write
               :producer <string>            ; who wrote it
               :relation <string>            ; the claim's relation
               :method <string>              ; how the binding was made
               :rule-version <string>        ; bumped when the rule changes
               :precision-fn <fname>         ; => metres, or nil
               :confidence-fn <fname>        ; => double-float, or nil
               :method-fn <fname>)           ; => string, or nil
       | :none
```

Every field is derived from what the spine's rules already pass to
`upsert-spine-claim`; none is invented. The geometry itself comes from the
`:space` facet's `:geometry-slot`, and the subject namespace from
`:identity` — registration does not restate either.

**`:method-fn` joined once a source's method turned out to be a
per-record fact, not a source-wide constant** (cl-llm#13 unit 2, task 6b).
The place spine's `register-site` writes `"geometry-overlap"` for a site
with an extent and `"centroid-within"` for one with only a centroid — one
`:method` string cannot say both, and `method` is not part of
`def-unique`'s identity tuple, so writing the wrong one would silently
*update* a deployed claim's method rather than leaving it. `:method`
**stays** and remains required; `:method-fn` is a required key whose
value may be `nil`, exactly like `:precision-fn` and `:confidence-fn`.
`register-node` calls it when non-nil and writes the result as the
claim's method; when nil, `:method`'s string is written, unchanged from
before this key existed. This keeps a source with a genuinely constant
method from having to write a one-line function that returns a literal.

**`:claim-class` and `:producer` were added after Task 5 found the facet
could not write a claim without them.** A claim family is a tenant's
`def-claim-classes` output and `*claim-families*` is keyed by parent name
alone, so there is no graph-to-family lookup that is not guesswork.
`producer` is worse than missing: it is part of `def-unique`'s binary
identity tuple `(producer subject-namespace subject-key object-namespace
object-key relation)`, so a derived-but-wrong value re-inserts instead of
updating — defeating the idempotency registration exists to provide,
across every claim a tenant has already written.

**`:registry-namespace` is a KEYWORD**, matching `:identity`'s
`:namespace` and the keywords the substrate's
`subject-namespace`/`object-namespace` slots actually hold. An earlier
draft said string, which would have forced interning at the boundary and
invited a silent mismatch with deployed data.

**`standing` is not a facet field.** `register-node` writes `:inferred`,
because a registration is derived by computation — that is what the
standing vocabulary means, and it matches what tenants already store for
these claims. A tenant needing otherwise is a change with a reason, not a
default to configure.

**`:registry` must name a `DEF-SOURCE` class, not a bare vertex.** A
claim's object endpoint is `(object-namespace object-key)`, and the key
comes from the region class's own `:identity :key-slot` — so a registry
without an identity facet cannot be pointed at. `register-node` signals
rather than writing a claim whose endpoint nothing could resolve.

**`:relation` and `:method` are strings here.** When this was written
the substrate had not settled that — its own tests passed keywords
(`:relation :r`) while the place spine wrote strings — and the trap was
that `relation` is part of `def-unique`'s identity tuple, so
`:registered-at` and `"registered-at"` are DIFFERENT claims. Settled by
vivace-graph#160: `relation` and `producer` are canonical strings,
enforced at commit by `canonical-relation-p` / `canonical-producer-p` as
`:check` slot options, and this facet's `req-string` is the contract the
engine now agrees with.

`:none` must stay fully supported. The map-less tenant is what proves it.

## 4. The API

```lisp
(register-geometry geometry registry &key registry-graph)
  ;; => (values registrations evaluated-p unmeasured)
```

`registrations` is a list of `(:region <node> :fraction <double>)`,
**unordered** — see §13. `evaluated-p` is §6. `unmeasured` (#164) is a
list of `(:region <node> :error <string>)` — the candidate regions the
scan could not measure — empty for a complete scan; see §6.

**Both geometries are repaired before intersecting, and the fraction is
clamped to 1.0.** `geometry-make-valid` runs on the subject once (so the
denominator is the *repaired* subject's measure, not a self-intersecting
ring's abs-summed spherical excess) and on each region, with
`ignore-errors` falling back to the original — the repair is unavailable
without the add-on and on GEOS < 3.8. Skipping the repair would let an
invalid ring that cleared the index's `intersects` refinement throw
inside `GEOSIntersection` and refuse the **whole subject**, dropping
every region it genuinely overlaps; that is the host-dependent invalid-
polygon population §6 describes, and turning a recoverable case into a
total refusal is strictly worse than the partial-coverage report it
replaces. The clamp is the `[0,1]` contract §1 states: a repaired
intersection can measure a hair over its repaired subject, and a stored
`fraction` above 1 is a contract violation, not a larger overlap. Added
in task 6b's task 5, from the tenant rule this API replaced — which did
both, and whose loss would have been silent.

**The keyword is `registry-graph`, not `graph`.** `register-node` has
both, and its `:graph` is the *subject's*; a caller reaching for
`register-geometry` by analogy and passing the subject's graph would get
a silent wrong answer, since `node-geometry` on a foreign region returns
`NIL` through its own `ignore-errors`, the region is dropped, and the
caller sees an empty list with `evaluated-p` **true**. One name, one
meaning, in both functions.

```lisp
(register-node node &key graph registry-graph)
  ;; => (values claims-written evaluated-p unmeasured registrations
  ;;            retracted)
```

Reads `node`'s source contract, computes the registrations, and upserts one
claim per region. The fourth value is that computed list (#165), so a
caller needing the regions bound by *this* scan neither scans twice nor
reads them back off the claims, where a stale one would fold in (#162);
an unmeasured region appears in neither. Idempotent on
`(subject-namespace subject-key relation object object producer)` — the
identity the spine already uses, looked up through the declared subject
index and filtered in Lisp, since claims per subject are few. The lookup
runs outside the transaction, and OCC cannot see a phantom, so two
concurrent registrations of one subject can both take the insert branch;
`def-unique` catches the second at commit and the upsert reads it as
"someone else won" — re-read and update (#161). Moving the lookup inside
the transaction would not have helped; the retry is the usual upsert
answer and needs no lock.

**One query, exact hits.** There is no candidates-then-verify phase.
#138's original scope named one, but the tenant it would be generalised
from *retired* it: `find-nodes-intersecting` refines exactly through GEOS,
so "a direct index query has no candidate/hit distinction left", and
`find-spine-places-near` was deleted along with the old centroid-only
index.

**Exactness is conditional, and the condition is `*geos-available-p*`.**
`graph-db/geos` is an optional add-on — the engine does not depend on it,
and loading it is what flips that flag. Without it,
`find-nodes-intersecting` falls back to a **coarse bounding-box** test for
extended geometry (points stay exact), and `geometry-intersection` signals
`geos-required-for-operation`.

So the "backend whose index only bounds" is not hypothetical or future: it
is this engine without `libgeos_c`. Registration handles it by refusing
rather than approximating — see §6. Callers may read an empty
`registrations` as "no region here" **only when `evaluated-p` is true**,
and that sentence lives in `register-geometry`'s docstring rather than
being assumed at each call site.

No verify-phase hook is built. If an inexact backend ever needs to *serve*
registration rather than refuse it, the verify step is added here, in one
function; designing that extension now, against a consumer that does not
exist, is the failure this issue's deferral was written to prevent.

## 5. The query-direction trap

`find-nodes-intersecting` accepts an area of any geometry kind and tests
*region-geometry INTERSECTS area* — the right direction for "does this
region contain this point". `find-nodes-within` takes a polygon area only
and tests *candidate-geometry WITHIN area*, the wrong direction for that
question. Do not swap one for the other as a simplification. This is
recorded in mine-action's runbook §A12 and belongs in the substrate's
docstring now that the substrate owns the query.

Hits are filtered to geometry-bearing regions. A centroid-only region can
only ever "intersect" by exact float coincidence, never genuine
containment; it stays indexed and reachable by scope but never produces a
hit itself.

## 6. Partial coverage is a first-class result

Four different things make a scan unanswerable, and all take the same
exit.

**An invalid polygon.** GEOS raises `TopologyException`, and **which
polygons are invalid depends on the host's GEOS version** — four sites
killed an entire backfill on GEOS 3.10.2 while the same run succeeded on
3.14.1.

**A subject with no geometry.** Its geometry slot is unset, or was read
under the wrong ambient graph — `node-geometry`'s default wraps the read in
`ignore-errors`, so both look identical and both yield `NIL`. Reporting an
evaluated scan there would assert "this record is in no region" when the
truth is "its position is unknown". Found by Task 5, which is the path a
caller is most likely to hit.

**No GEOS at all.** Without the add-on, an extended geometry's candidates
are bounding-box approximate and its fraction cannot be computed. A
bounding box is *over*-inclusive, so approximating here would write claims
binding records to regions they do not touch — silent false positives in a
substrate whose whole posture is that absence carries a reason. A **point**
is unaffected: its candidates are exact and its fraction is 1.0 by
definition, so points register normally with or without GEOS.

**An intersection this engine cannot represent.** `wkt->geometry` parses
POINT, LINESTRING, POLYGON and MULTIPOLYGON; anything else signals
`geos-error` on the way back — the same exit as an invalid polygon, for a
different reason. Found by Task 4's review.

⚠ **A single-edge touch is NOT this case.** An earlier draft said two
polygons sharing only an edge intersect in a MULTILINESTRING. They do
not: one contiguous shared edge gives a **LINESTRING**, which parses, and
whose geodesic *area* is zero — so that region is dropped as a touch
(§13) while every other region still registers and `evaluated-p` stays
true. What cannot be represented is a **multi-component** boundary
intersection: a **MULTILINESTRING** (two *disjoint* shared edges) or a
**GEOMETRYCOLLECTION** (a vertex touch together with an edge touch),
which no earlier draft named at all. Corrected in task 6b's task 5; the
wrong wording had also reached the manual and the tenant's §9, where it
would have sent an equivalence pass hunting the wrong signature.

In every one of the four, `evaluated-p` is `nil` rather than a result:
`register-geometry` answers so for the three the scan itself meets, and
`register-node` for the missing geometry, which it detects before
scanning. It never signals for any of them, and it never catches anything
broader than `geos-error` — a broader handler would swallow the
multi-graph node-escape class (#53), and nothing but a test that signals a
plain `error` through the scan can hold that line.

Without the second value, "no regions here" and "the scan never ran" are
the same answer. A caller that ignores `evaluated-p` degrades its own
coverage silently, which is the defect the tenant has already paid for
once; callers aggregating registrations should carry a partial-coverage
flag the way `spine-backfill` carries `:sites-geos-skipped`.

**Per-region failure is not a scan failure (#164).** The section above
was written with one `geos-error` handler around the whole region loop,
so the fourth case — an unrepresentable intersection — refused the whole
subject: measured at 10 pairs in 1.3M, but clustered, costing 1,560
claims across ten consecutive days of a series where a missing claim
reads as a definite negative. The failure granularity #138 left open is
settled here: **a `geos-error` while measuring one candidate drops that
region and names it in a third value**, `unmeasured` — a list of
`(:region <node> :error <string>)` — while every other region registers
and `evaluated-p` stays true. The dropped region is never written at
fraction 0, which would assert a touch; absence carries its reason
instead. So an evaluated scan with a non-empty `unmeasured` is
**partial**, and a caller keeping coverage figures must read the third
value — `evaluated-p` alone now reports such a scan as evaluated.
`register-node` passes it through unchanged and writes no claim for
those regions. The whole-scan refusal remains for the first three cases
and for a `geos-error` inside the candidate query itself (typically GEOS
rejecting the subject), where the candidate list is unknown. Both
handlers catch `geos-error` and nothing wider, for the reason above.

## 7. The cross-graph contract

Registration spans two graphs: the subject lives in its source's graph, the
regions in the registry's. Reading a node's slots under the wrong ambient
`graph-db:*graph*` is the node-escape class (#53).

The API therefore:

- reads the subject's geometry and identity under an explicit binding of
  the subject's graph, and lets only **plain values** cross out — never a
  node object;
- reads region slots under the registry graph's binding;
- writes claims under the registry graph, in a transaction.

A read-write transaction is single-graph (3.0's multi-graph contract), so
the claim write is registry-graph-local by construction.

## 8. Geodesic polygon area joins the engine

A fraction is `area(intersection) / area(subject)`.

`graph-db` has `geometry-area`, but it returns **squared coordinate units**
(squared degrees), not m², and requires GEOS. Squared degrees are not a
usable measure here: a degree of longitude is a different distance at every
latitude, so a ratio of two such areas is only accidentally right.

`geometry-geodesic-area` is therefore a **core** op in `geometry-ops.lisp`,
not a GEOS one. The tenant's `geodesic-polygon-area-ha` shows why: it is
spherical-excess math over coordinate pairs — outer ring minus holes, zero
for points and lines — and touches GEOS nowhere. Porting it needs no
add-on and no `geos-required-for-operation` method.

GEOS is still required for the *intersection*, which is what §6's refusal
is about. Area and intersection are separate dependencies and the spec
should not conflate them.

Putting the measure in the substrate is what makes `fraction` mean the same
thing for every tenant, which is the whole point of §2's promotion — a
tenant-supplied measure would make a shared slot carry a per-tenant
meaning.

**Validation, not assertion.** The new function is checked against
mine-action's existing `geodesic-polygon-area-ha` over real spine
geometries on this host, and the spec of record is agreement with that
function, since its outputs are already in production reports.

## 9. Claims are not spatially indexed

The claim record's `geometry` slot stays unindexed. The one tenant writing
registration claims never populates it: `make-spine-claim-binary` receives
endpoints, relation, producer, method, confidence, precision, fraction,
rule-version and standing — no geometry. Indexing a slot nothing fills is
speculative.

What would change this: a tenant storing the *intersection* polygon on the
claim and asking "which registrations overlap this area". That is a new
query, and it should arrive with its consumer.

## 10. Migration

Both repositories, in this unit.

**Engine.** The two slots move into `+claim-shared-slots+`.

**Tenant.** `spine-claim` drops its `:extra-slots`; its `precision-m` and
`fraction` accessors are now the substrate's. The spine's sources gain
real `:registration` facets in place of `:none`, and `spine-register.lisp`
is reduced to what the substrate does not do.

**Verified against deployed data, on a copy.** ma-dev is this host, and
`/data0/mine-action-dev/graphs/spine` holds real registration claims
(384 MB; 873 GB free). The live server holds that graph open — a second
process opening it is refused with `.dirty exists`, and running recovery
on a graph a live server is using is worse than the problem it solves. So
the migration check runs against a **copy**, never the deployed directory,
and never with the server stopped for convenience.

The check: persisted claims written under the old extra-slots must read
back through the substrate accessors with their values intact.

## 11. Testing

- **Engine, synthetic registry:** a point in one region; a polygon
  spanning two, fractions summing to 1.0 within tolerance; a line
  traversing three; a subject outside every region.
- **The upsert's UPDATE branch, not only its count.** Re-registering a
  subject whose registration now produces different values must overwrite
  the stored `fraction` and `precision-m`. A count-only assertion leaves
  the whole `setf` block deletable with the suite still green.
- **`evaluated-p`, `geos-error`:** yields `(values nil nil)` and signals
  nothing. Proven by ablation — the same call on a valid polygon must
  return `(values ... t)` in the same run, or the test cannot tell a skip
  from an empty result. **Which polygons GEOS rejects is host-dependent,
  so the condition is raised deterministically instead**, from a region
  class's own `node-geometry`, which the scan calls inside the handler.
  A second region class signalling a plain `error` must **propagate**:
  that, and only that, pins the handler's narrowness.
- **`evaluated-p`, no GEOS:** with `*geos-available-p*` bound to `nil`, a
  polygon yields `(values nil nil)` and a **point still registers**, in the
  same run. The point is the control: without it the test cannot tell
  "refused correctly" from "broken everywhere".
- **`:registration :none`:** a source declaring it registers nothing, and
  its claims still read. This is the map-less tenant's contribution and it
  must not be a synthetic fixture.
- **Geodesic area:** agreement with `geodesic-polygon-area-ha` over real
  spine geometries.
- **Migration:** the copied spine graph, above.
- **Node escape:** a registration across two graphs leaves no node object
  outside its own graph's binding.

## 12. Not in scope

- No spatial index changes.
- No claim spatial indexing (§9).
- Namespaces, and one vertex class across two graphs — #110 and cl-llm#20.
- Unit 3's weighting. Registration produces fractions; what a retrieval
  layer does with them is that layer's design.

## 13. Open items

- **Line traversal ordering — SETTLED: unordered.** A line's regions have
  a natural order, and the spine sorts by place level — but that is a
  tenant's notion of specificity, meaningless to another registry. The
  substrate returns what the index returned, each with its fraction, and a
  tenant sorts for itself.

- **A line's fraction is a LENGTH ratio, not an area ratio.** Discovered in
  Task 4's review. `geometry-geodesic-area` is zero for a `:linestring`,
  so an area-based fraction gives a line 1.0 in *every* region it crosses,
  summing to 3.0 over three regions — which contradicts §1's "the useful
  fact is what fraction of it falls in each" and makes `fraction`
  unusable for lines by the domain-neutral reader §2 promoted it for.
  A line's fraction is therefore
  `length(intersection) / length(subject)`, computed by a core
  `geometry-geodesic-length` folding the existing haversine
  `geodesic-distance` over consecutive vertices. A zero-length subject —
  a degenerate line, or a point — keeps 1.0, which is correct for a point
  and the only defensible answer for a degenerate one.

- **A region the subject merely touches is not registered.** GEOS
  `intersects` is true for boundary contact, so an abutting region comes
  back from the candidate query with an intersection of zero area or
  length. Registering it would bind a record to a region it does not
  overlap — the mild form of the false positive §6 exists to prevent — so
  a zero fraction is dropped rather than written. This is the ordinary
  path for a **single shared edge**, whose intersection is a LINESTRING
  of zero area: the touching region drops, the rest of the scan is
  unaffected. Only a *multi-component* boundary intersection takes §6's
  refusal exit instead — see the correction there.
- **Fraction tolerance.** Fractions over a partition should sum to 1.0, but
  GEOS intersection on adjacent polygons double-counts shared boundaries by
  a negligible amount. The test needs a stated tolerance rather than an
  exact comparison.

## 14. Retraction (#162)

Registration owns it. After the upsert, `register-node` retracts the
subject's other *current* claims under the facet's producer and relation
whose region is in neither the registrations nor the unmeasured list, and
reports the count as a fifth value. An unmeasured region (§6, #164) keeps
its claim — unknown is not left — and an unevaluated scan retracts
nothing.

**Retraction closes the transaction period; it does not delete.**
`retract-claim` writes `[recorded, at)` with standing `:asserted` — the
bitemporal `[recorded, superseded)` the transaction-time design named as
the seam. The claim stays, still occupies its identity tuple, and
`claim-current-p` is NIL; `claims-touching` gains `:current` to filter.
A claim predating the axis closes as `[unknown, at)`. Retracting twice is
a no-op. This is the one sanctioned change to the transaction extent
after creation, and it is the constraint #158's write-once family must
admit: the *start* is immutable, the *end* may be closed once by the
substrate.

Transaction time rather than validity time because registration derives
from the current geometry and cannot tell a correction from a move; "we
no longer derive this" is what it honestly knows. A tenant with the
world-time story writes the validity extent itself.

A subject returning to a region re-opens its retracted claim with a fresh
stamp — the tuple is occupied, so a second claim is impossible — and the
closed period survives only in MVCC, which is #148's recorded limitation.
The tenant's producer-scoped sweep (`delete-claims-by-producer`,
sweep-then-insert) is unchanged and still the right tool for a rule that
stops producing altogether.

