# Temporal claim families — a state series as claims (vivace-graph#296)

**Status:** design + implementation, 2026-09-01. Requested by the spatial
tenant (mine-action), who paused their unit on it rather than ship a
workaround. Builds on #131 (claim record), #148 (transaction axis), #157 4b
(membership disjointness), #160 (canonical relations), #162 (retraction).

## 1. The gap, restated

A claim's identity is the DEF-UNIQUE tuple
`(producer, subject-ns, subject-key, [object-ns, object-key], relation)`.
The validity extent is not in it. A **state series** — X stood in relation
R to Y during [a,b], then again during [c,d] — needs several live claims with
the same tuple and disjoint validity extents, and today the second one either
collides at commit or is turned into an update of the first. Measured on the
tenant: ~50k runs over ~1.8k subjects, recurrence (A → B → A) the common case.

The tenant's four workarounds (state in the relation, run start smuggled into
the object key, a provenance node as object, a non-claim vertex) are each
rejected in the issue; none of them is re-argued here.

## 2. What is built

### 2.1 Declaration

```lisp
(def-claim-classes region-state :my-graph :temporal t)
```

`:temporal t` marks the family. `claim-family-temporal-p` reads the flag off
the registry. Everything else about the family — the three classes, the
shared slots, the named indexes, the standing and canonical-name constraints,
the transaction-stamp transition — is unchanged.

### 2.2 Identity: the extent START joins the tuple

For a temporal family the two DEF-UNIQUE tuples become

```
unary:  (producer subject-ns subject-key relation extent-sexp)
binary: (producer subject-ns subject-key object-ns object-key relation
         extent-sexp)
```

with the last position canonicalised by `extent-sexp-start-key`: the stored
extent's **start bound**, rendered as
`((day sec nsec) (day sec nsec))` — `local-time`'s own three fixnums per
timestamp, `:unbounded` where the bound is open. Fixnums, not timestamp
objects, because the memory backend's unique index is an `EQUAL` hash table
and structures are `EQUAL` only when `EQ`; and not nanoseconds-since-epoch,
because that is a bignum on CCL/ECL and `serialize` has no bignum method.

Why the start and not the whole extent: an *ongoing* run — a state the
subject is still in — is rewritten by each ingest with a later end. Keying
on the start lets that rewrite be an UPDATE of the same claim (COPY, SETF
`claim-extent`, SAVE) rather than a new identity per day. Keying on the whole
extent would make the daily rewrite a fresh claim overlapping the old one,
and the overlap check below would then refuse the tenant's own write.

The declarations keep their names (`claim-unary-identity`,
`claim-binary-identity`), so re-evaluating `def-claim-classes` with or
without `:temporal` REPLACES the tuple (GH #139/#140 naming rule). The
unique sidecar reconciles by `(owner . slot-names)`, so a graph opened under
the other shape drops the old index and builds the new one at open.

A temporal claim **must carry an extent**: the constructor signals
`missing-claim-identity-component` (slot `:extent`) without one, and a
named `def-value-constraint … extent-sexp :required t`
(`claim-extent-required`) refuses it on every other write path. Without
this the null-exempt rule of DEF-UNIQUE would leave an extent-less temporal
claim under no identity at all.

### 2.3 The constraint that makes it more than a wider key

For a temporal family, live claims sharing the **base tuple** (everything
but the extent) must have pairwise **disjoint** validity extents. Enforced
at commit by `%validate-extent-disjointness`, a `*commit-validators*` entry
beside the membership check, evaluating POST-commit state through the
commit view (a run retracted in the same transaction does not count; one
created in it does). The refusal is `extent-disjointness-violation`, its own
condition, carrying the family, subject, object, relation and the ids of
the claims whose extents overlap. `check-extent-disjointness graph family`
is the audit — runs written before the flag, or past the check — returning
`(values violations checked)`.

**Disjoint** means: the Allen relation set between the two extents is a
subset of `{:before, :after}` — `extents-disjoint-p`. Three consequences,
each deliberate:

- `:meets` is NOT disjoint. Intervals are closed (cl-temporal-extent design
  §3.2), so two runs that meet share their boundary instant, and "at most
  one live claim per (subject, object, relation) *at any instant*" would be
  violated at that instant. Day-granular runs built with granule bounds
  (`make-granule-interval … :day`, or `make-bound` over the day) never meet:
  the first ends at 23:59:59.999999999 and the next starts at 00:00:00.
- An **ambiguous** pair (fuzzy bounds that might or might not overlap) is
  refused. The algebra's rule — definite only when no choice within either
  range could give another answer — is applied as written; a constraint that
  admitted "possibly overlapping" would not be a constraint.
- A claim with NO extent overlaps everything. Unreachable for a temporal
  family (2.2 requires one); stated so the predicate is total.

### 2.4 Membership per instant

`def-disjoint-membership` (4b) counts every `claim-current-p` membership
claim in the set. For a temporal family the check becomes: no two live
membership claims of the subject within the set may **possibly overlap** —
a region may be `A` during [a,b] and `B` during [c,d] with both claims live.
Same predicate as 2.3, applied across object keys instead of within one.
`check-disjoint-memberships` (the audit) applies the same rule. Non-temporal
families keep the count-based check unchanged.

### 2.5 Reads

`claims-touching` gains two validity filters, orthogonal to `:current`:

```lisp
(claims-touching g 'region-state :region "r1" :at day)       ; a TIMESTAMP
(claims-touching g 'region-state :region "r1" :during window) ; an EXTENT
```

`:at ts` keeps claims whose extent possibly contains `ts`; `:during e`
keeps claims whose extent possibly shares an instant with `e` — i.e. runs
*intersecting* the window, which is what "the runs during [a,b]" asks for
on a timeline; Allen-precise `:during` is one `allen-relation` call away on
the result. Both are the negation of `extents-disjoint-p`. A claim with no
extent makes no validity statement and is excluded by either filter. Both
filters run over the candidate set the subject/object index already
returned, so the read is index-bounded per endpoint, not a scan.

### 2.6 What the transaction axis does not change

Untouched. A series is a set of claims; each is stamped, retracted
(`retract-claim`) and re-asserted independently. A retracted run still
occupies its `(base tuple, start)` identity, exactly as a retracted claim
occupies its tuple today; re-asserting it is the same COPY + re-stamp path
`%update-registration-claim` uses.

## 3. Out of scope, recorded

- **Registration** (`register-node`) writes claims without an extent and
  matches its upsert on the base tuple. A registration facet's
  `:claim-class` must therefore be a non-temporal family; a temporal one is
  refused by `claim-extent-required` at the first insert. Making registration
  temporal (a subject's region membership as a series) is a separate unit.
- **A disjointness *policy*** (e.g. admitting `:meets`, or admitting
  ambiguous pairs) is not offered. One rule, the strict one; relax it when a
  tenant shows the case.
- **Moving `extents-disjoint-p` into cl-temporal-extent.** It belongs there
  eventually; kept in `graph-db.spacetime` for now to avoid a cross-repo
  release inside this unit.
- **Switching an existing family's `:temporal` flag on a populated graph** is
  a schema migration: the unique index is rebuilt tolerantly at open
  (keep-first on duplicates, logged), and pre-existing overlapping runs are
  found by the audit, not refused retroactively.

## 4. Acceptance (from the issue) → tests

`tests/spacetime/temporal-tests.lisp`:

- A recurring series A → B → A is storable as three membership-shaped
  claims with one relation and disjoint extents.
- An overlapping fourth is refused at commit with
  `extent-disjointness-violation`; nothing of the refused transaction
  survives.
- `def-disjoint-membership` holds per instant over that series; an
  overlapping cross-key membership is refused; the audit finds a
  pre-existing overlap.
- `claims-touching … :at d` returns exactly the run covering `d`;
  `:during` returns the intersecting runs.
- Plus: a same-start rewrite is an update (the ongoing-run case); a retracted
  run does not block an overlapping successor; two overlapping runs in one
  transaction are refused; the identity survives close/open; a temporal
  claim without an extent is refused at construction and at commit; a
  non-temporal family's behaviour is unchanged.
