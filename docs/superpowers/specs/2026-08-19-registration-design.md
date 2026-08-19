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
:registration (:registry <class>            ; the region class to bind to
               :registry-namespace <string> ; where those regions live
               :relation <string>           ; the claim's relation
               :method <string>             ; how the binding was made
               :rule-version <string>       ; bumped when the rule changes
               :precision-fn <fname>        ; => metres, or nil
               :confidence-fn <fname>)      ; => double-float, or nil
       | :none
```

Every field is derived from what the spine's rules already pass to
`upsert-spine-claim`; none is invented. The geometry itself comes from the
`:space` facet's `:geometry-slot`, and the subject namespace from
`:identity` — registration does not restate either.

`:none` must stay fully supported. The map-less tenant is what proves it.

## 4. The API

```lisp
(register-geometry geometry registry
                   &key graph registry-graph precision-m)
  ;; => (values registrations evaluated-p)
```

`registrations` is a list of `(:region <node> :fraction <double>)`, most
specific first. `evaluated-p` is §6.

```lisp
(register-node node &key graph registry-graph)
  ;; => (values claims-written evaluated-p)
```

Reads `node`'s source contract, computes the registrations, and upserts one
claim per region. Idempotent on
`(subject-namespace subject-key relation object object producer)` — the
identity the spine already uses, looked up through the declared subject
index and filtered in Lisp, since claims per subject are few.

**One query, exact hits.** There is no candidates-then-verify phase.
#138's original scope named one, but the tenant it would be generalised
from *retired* it: `find-nodes-intersecting` refines exactly through GEOS,
so "a direct index query has no candidate/hit distinction left", and
`find-spine-places-near` was deleted along with the old centroid-only
index.

**Exactness is a property of the query, stated in one place.** Callers may
read an empty `registrations` as "no region here" *because the query
refines exactly*, and that sentence lives in `register-geometry`'s
docstring rather than being assumed at each call site. A backend whose
index only bounds would need a verify step; adding one then is a change to
this function and its contract, not to its callers. No hook is built for
that today — an extension point designed against an imagined consumer is
the failure this issue's deferral was written to prevent.

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

GEOS raises `TopologyException` on an invalid polygon, and **which
polygons are invalid depends on the host's GEOS version** — four sites
killed an entire backfill on GEOS 3.10.2 while the same run succeeded on
3.14.1.

So `register-geometry` catches `geos-error`, counts the skip, and returns
`evaluated-p` = `nil`. It never signals, and it never catches anything
broader than `geos-error` — a broader handler would swallow the
multi-graph node-escape class (#53).

Without the second value, "no regions here" and "the scan never ran" are
the same answer. A caller that ignores `evaluated-p` degrades its own
coverage silently, which is the defect the tenant has already paid for
once; callers aggregating registrations should carry a partial-coverage
flag the way `spine-backfill` carries `:sites-geos-skipped`.

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

A fraction is `area(intersection) / area(subject)`. `graph-db` exports
`geometry-intersection` and `geometry-geodesic-distance` but no polygon
area; the tenant's `geodesic-polygon-area-ha` supplies it today.

`geometry-geodesic-area` joins the GEOS ops as a sibling of the geodesic
distance already there. Putting it in the substrate is what makes
`fraction` mean the same thing for every tenant, which is the whole point
of §2's promotion — a tenant-supplied measure would make a shared slot
carry a per-tenant meaning.

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
- **`evaluated-p`:** an invalid polygon yields `(values nil nil)` and
  signals nothing. Proven by ablation — the same call on a valid polygon
  must return `(values ... t)` in the same run, or the test cannot tell a
  skip from an empty result.
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

- **Line traversal ordering.** A line's regions have a natural order and
  the spine sorts by place level, which is a tenant's notion of specificity.
  Whether the substrate promises an order for a traversal, or returns them
  unordered with the fractions, is unsettled. Decide before implementing
  `register-geometry` for lines.
- **Fraction tolerance.** Fractions over a partition should sum to 1.0, but
  GEOS intersection on adjacent polygons double-counts shared boundaries by
  a negligible amount. The test needs a stated tolerance rather than an
  exact comparison.
