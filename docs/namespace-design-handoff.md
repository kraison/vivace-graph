# Graphs as namespaces — handoff

Paste the prompt below into a fresh session started in `/Users/kraison/work/vivace-graph-v3`.
Written 2026-08-06. The design itself is **`docs/namespace-design-discussion.md`**, in this
directory — this file is only the pickup, not a second copy of the argument.

---

## The prompt

> I want to pick up the **graphs-as-namespaces** design for VivaceGraph. It was parked
> deliberately on 2026-07-29 so the mine-action spine work could go first; that work is
> now done and merged, so the constraints it was waiting on are known.
>
> **Read `docs/namespace-design-discussion.md` in this repo first — the whole thing.** It
> is the full record: the core observation, why collapsing to one graph was examined and
> rejected, why namespaces are the safer engine investment than cross-graph edges, the
> five-point agreed shape, and three open items. Do not re-derive any of it; the
> conclusions were reached with the operator and hold.
>
> The one-line version, so you know what you are reading toward: **"multiple graphs" in VG
> conflates physical file partitioning with transactional and schema isolation. Keep the
> first, drop the second.** A graph becomes a namespace; transactions, the snapshot clock
> and the type-id space become shared.
>
> Start with `superpowers:brainstorming` on **the open items**, not on the agreed shape.
> The largest is **exclusive / detached bulk-load mode per namespace** — under a single
> writer held by the live server, bulk ingest must run in-process, and splitting data files
> by namespace is what makes a detach/load/reattach path possible at all. The other two are
> inbound cross-namespace index lookup, and an audit that the external-identity slots are
> actually indexed.
>
> **Also read**, because they bound the design:
> - memory `vg-namespace-design` (the parked summary), `graph-db-mvcc-contract`,
>   `multi-graph-node-escape-bug`, `spatial-index-facts`
> - `~/quicklisp/local-projects/mine-action/docs/superpowers/specs/2026-07-29-spatiotemporal-graphrag-stage-a-design.md`
>   §3.2 — the claim-as-reified-relation shape this design depends on, now built and
>   running at 300k+ claims
>
> ⚠ **What changed since the parking, and must be re-checked rather than assumed:**
> - **VG 3.0 shipped**, and with it per-`(owner . slot)` spatial indexes. The memory
>   `spatial-index-facts` carries a correction banner; "one spatial index per graph" is no
>   longer true.
> - **vivace-graph #53** (multi-graph node escape) was the structural motivation for global
>   type-ids. Check its current state before treating it as open.
> - **#104 and #105 are fixed** in the engine but still show open on the tracker.
> - mine-action now runs **five** graphs (ops, knowledge, forensics, spine, imsma), not
>   three. The spine is the derived, disposable namespace this design predicted, built for
>   real — it is the best evidence available that point 4 of the agreed shape works.
> - The engine is on branch **`experiment`**; production host `ma` runs `a8e0d15` and
>   mine-action now requires ≥ `7ac1458`, so ma is behind and any format change lands on a
>   host that is not current.
>
> ⚠ **The one-way door.** Widening edge endpoints to `(graph, node-id)` is an on-disk
> format change; global type-ids are a data migration. The design chose namespaces
> specifically because it is the same problem with the far better failure mode. Do not
> reopen that trade without saying so explicitly.

---

## Why this file exists

The design discussion was written on 2026-07-29 and left **untracked** in the working tree
for eight days — one `git clean -fdx` from gone. The operator's instruction at the time was
"please make sure that our discussion about VG is logged somewhere so that it doesn't get
lost at the next context compaction"; an uncommitted file does not satisfy that. Both files
are committed together with this one.

If you are reading this and `namespace-design-discussion.md` is missing, memory
`vg-namespace-design` carries the load-bearing conclusions in compressed form.
