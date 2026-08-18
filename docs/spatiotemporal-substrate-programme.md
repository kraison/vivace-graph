# SpatioTemporal substrate programme — pointer

This repo holds four of the eight units of a programme that spans three repositories.
This file exists so engine work can find the programme it belongs to; it is a pointer,
not a second copy of the argument.

**Programme design:** [`2026-08-09-spatiotemporal-substrate-programme-design.md`](https://github.com/kraison/cl-llm/blob/main/docs/superpowers/specs/2026-08-09-spatiotemporal-substrate-programme-design.md)
in `kraison/cl-llm`.

**Tracker:** kraison/cl-llm#15 · **Board:** https://github.com/users/kraison/projects/1
(private)

## The units in this repo

| | Unit | Issue | Phase |
|---|---|---|---|
| M | Multi-slot indexes — composite `def-index`, leading-prefix lookup, class-level multi-slot unique | #107 | 1 |
| S1 | `graph-db/spacetime` — claim + temporal-extent substrate, opt-in subsystem | #108 | 1 |
| S4 | `graph-db/ontology` — declarative formalism + constraint validator | #109 | 3 |
| S0 | Namespaces — complete the graphs-as-namespaces design | #110 | 3→4 |

S0 is the design already recorded in `namespace-design-discussion.md` and
`namespace-design-handoff.md` in this directory. **Do not re-derive its agreed shape** —
the open items are the work.

S4's unit 1 — declarative value constraints, `def-value-constraint` — landed on
`experiment`; see `docs/value-constraint-design.md`. Tracked by #149, one unit of the
#109 epic; units 2-4 (cardinality beyond `:required`, domain/range, disjointness) are
deliberately unfiled until unit 1's shape informs theirs.

## Why this matters to the engine

The substrate (S1) is an **opt-in subsystem** beside `graph-db/geos` and
`graph-db/replication`. `graph-db/core` stays semantically neutral: nobody who just
wants a graph pays for a knowledge-representation opinion. The core gains nothing from
this programme except, eventually, an interval index — and only once measured, since a
composite `(place, valid-start)` index with leading-prefix range may already serve the
access path.

**The boundary rule is a PR review checklist item:** nothing in `graph-db/spacetime` may
name a concept from a tenant application. A design decision justified only by what one
tenant needs belongs in that tenant.

## Related open issues

#93 (multi-graph 2PC), #94 (global cross-graph epoch — a correctness gate for reasoning
spanning namespaces, not an optimisation), #102 (index-backed generator predicate — on
the retrieval layer's path *and* gating any Datalog work), #104 and #105 (fixed in the
engine, still open on the tracker), #45 (Prolog modernization; phases 0-1 complete and
merged, remaining phases tracked as sub-issues #121–#124 plus #102).

How much of #45 this programme actually needs is accounted for in §12.1 of the
programme design — about half of phase 2, one bullet of phase 4, and nothing else until
the reasoning layer's inference half.
