# mmap-backed native Lisp heaps for VG objects (ECL) — design note / R&D

**Status:** deferred R&D. Captured 2026-07-04 from a design discussion. NOT scheduled.
The near-term boot-latency work is the *VG-native memory-image serializer* (a fast
replacement for cl-store) and, if needed, *fault-on-access* over the existing mmap
store — both of which need no ECL surgery. This note records the arena idea so it can
be picked up later without re-deriving it.

## Motivation

The in-memory `memory-graph` backend (#50) makes on-device queries 8–80× faster by
holding nodes as live Lisp objects (no per-read deserialization). Its costs are:

- **Open latency** — restoring the cl-store image + (until 19e229b) rebuilding the
  derived structures. cl-store is generic and slow; on Android it is the dominant
  open cost.
- **RAM** — the whole node set is resident (~3.7× the on-disk backend on the Izium
  scope).

The arena idea attacks both at once: keep VG objects (nodes + index/skip-list
structure) as **native Lisp objects in an mmap-backed memory region** that the OS can
page, and persist by mapping the region rather than serializing it. Close = msync;
open = mmap + a small fixup. In the limit, **open cost becomes proportional to the
schema size, not the data size.**

Lineage: Lisp Machine `make-area` (per-area GC/paging policy), Symbolics **Statice**,
**PLOB** (persistent CLOS on CMUCL/LispWorks), **AllegroCache**, and — closest to this
proposal — Paul Wilson's **Texas Persistent Store** (pointer swizzling at page-fault
time: mmap + per-page fixup → near-native pointer access).

## The hard constraint (why "no serialization" is really "swizzle the boundary")

You cannot have all three at once:

1. **Native pointers** — a slot holds a real machine address, zero access overhead.
2. **A GC-managed object graph that includes metaobjects** — a CLOS instance points to
   its class → generic functions → method tables → packages → …
3. **Persistence across processes** — close, reopen, possibly at a different base
   address.

A `cl_object` is an absolute virtual address in *one* process's address space, and a
node's transitive closure includes un-persistable things (its class, symbols,
functions). So a persistent arena is not "no serialization"; it is **serialize/swizzle
only the boundary pointers** (the handful of references that leave the arena), while
everything inside the arena stays natively pointer-connected.

## The prize: schema-proportional open

Design the arena so its **only outbound pointers are to a small, fixed set of
metaobjects** — the ~dozen VG node classes, interned symbols, maybe a few functions.
Then reopen is:

1. `mmap` the region, then
2. fix up a **tiny table** — one entry per class/symbol (schema-sized, not data-sized).

That is the whole point. cl-store is O(every slot of every node): decode + type
dispatch + allocate, each item. A well-built arena is O(distinct classes/symbols) for
the metaobject fixup, and — depending on the address strategy below — either **zero**
or **O(pointers)** for intra-arena references, where each fixup is an *integer add*,
not a decode-and-allocate. That is the difference between "open scales with the scope"
and "open is ~constant."

## The load-bearing decision: address strategy

Intra-arena pointers are native machine addresses; their validity on reopen depends on
where the region lands.

- **Fixed address (`MAP_FIXED`).** Always map at the same VA ⇒ intra-arena pointers are
  valid for free; only the (small) metaobject boundary needs fixup. *But* fragile on
  Android: ASLR, no reliable way to reserve a fixed high-address region, address-space
  conflicts. High risk on the actual target.
- **Relocate-on-open (recommended default).** Map anywhere; add `new_base − old_base`
  to every intra-arena pointer on open. O(pointers), but each fixup is an integer add —
  far cheaper per item than cl-store's decode+allocate, and no fixed-address fragility.
  Not the "constant-time" dream, but robust, and still a large constant-factor win.
- **Offset-based intra-arena pointers.** Store `base + offset` everywhere; never
  relocate. Kills the native-access benefit (every slot access translates) — this is
  essentially what VG's *on-disk* store already is (`data-pointer` = heap offset). Not
  worth building a second time.

Recommendation: **relocate-on-open**, with fixed-address as an optional fast path if a
reliable reservation turns out to be achievable on the target.

## Write path: promote-into-arena (not "allocate everything here")

Do **not** redirect the global allocator so all allocation lands in the arena — node
construction allocates transient garbage (temp strings, boxes) you don't want
tenured, and the allocator is the runtime's hottest path (a per-thread "allocate in
area X" special adds a branch to every cons/instance). Instead:

- Build nodes normally on the main heap, then **promote the retained closure** into the
  arena: copy the instance + slots + owned value objects (strings, bignums, nested
  value structs) into arena storage, interning them arena-locally, and redirecting
  class/symbol pointers to **fixup-table slots**.
- Promotion is structurally a serializer whose *output is pointer-connected native
  objects* rather than a byte stream. So the write path is not free either — but it is
  paid at checkpoint time, not per read, and it produces something mmap-persistable.

## ECL integration (why ECL, and what it costs)

ECL is the **best** CL for this — better than SBCL — because it compiles to C, uses
plain structs (`struct ecl_instance` with a `slots` array of `cl_object`), and uses the
**Boehm** conservative, **non-moving** GC. Non-moving is essential: object addresses are
stable within a session, and an mmap region can be handed to `GC_add_roots` so Boehm
scans it for pointers into the main heap.

Costs / obstacles specific to ECL + Android:

- **Runtime-C surgery = a maintained ECL fork.** The instance/cons/string allocators
  and the arena-aware promotion live in ECL's runtime. Coupling VG to ECL's internal
  object ABI is a standing liability — every ECL bump (cf. the clang-21 break) can move
  `struct ecl_instance` layout or allocator internals. Budget for fork maintenance.
- **GC scan cost.** A large arena registered as GC roots is conservatively scanned every
  GC. We already saw ECL go GC-bound with an oversized buffer pool (#43); a
  hundreds-of-MB scanned arena could reintroduce pauses. Mitigations: keep the arena
  self-contained (few outbound pointers ⇒ can we tell Boehm to *not* scan it and instead
  treat it as leaf/uncollectable + manually keep the boundary alive?), or segment it.
- **Boundary closure discipline.** Strings, bignums, ratios, nested value objects are
  separately allocated; if a node slot points to one it must be promoted into the arena
  too, or it becomes another boundary pointer. The promotion pass must handle every
  value type that can appear in a node slot (reuse the type coverage in `serialize.lisp`).
- **Android address space.** 64-bit helps, but fixed-address reservation is unreliable;
  assume relocate-on-open.

## Relationship to existing VG machinery

- VG's **on-disk store** is already an offset-addressed persistent heap (`data-pointer`
  = mmap offset) with deserialize-on-read. The arena is the "native pointer" cousin:
  same mmap backing, but pointers instead of offsets, so no per-read decode.
- The **`memory-graph`** backend removed per-read decode by keeping live objects +
  cl-store checkpoint. The arena removes the *checkpoint* decode too, by making the
  live objects themselves mmap-resident.
- The **type coverage** and tag scheme in `serialize.lisp` / `globals.lisp` is the
  reference for what the promotion pass must handle.

## Phased de-risking plan (when this is picked up)

1. **Measure first** (this is the gate for the whole program): of Android open time,
   how much is cl-store overhead vs unavoidable object allocation? If cl-store
   dominates, the VG-native serializer (separate, near-term) already captures most of
   the win and the arena may not be needed.
2. **Narrow prototype:** one node type, **relocate-on-open** (not fixed-address),
   `GC_add_roots` integration, promotion + a class/symbol fixup table. Measure open
   time vs the VG-native serializer across scope sizes, and *feel the ECL-fork
   maintenance cost.*
3. **Graduation criteria:** prototype shows (a) ~constant-or-near-constant open
   regardless of scope, materially better than the VG-native serializer, AND (b) the
   fork/GC burden looks survivable on the Android toolchain. Only then does it move
   from exotic to roadmap.

## Bottom line

The arena is the theoretically-right endgame (it's where OODBs converge), and worth
having eventually. But it is an ECL-fork research program with real GC and
address-space caveats, and it is **not required to beat cl-store on Android**. Prove the
VG-native serializer (and, if needed, fault-on-access) can't hit the boot-latency
target before paying for the arena.

## See also

- `docs/memory-graph-design.md` — the in-memory backend (GH #50).
- `docs/memory-graph-handoff.md` — app-side handoff (durability, fast-open).
- `serialize.lisp` / `globals.lisp` — the type/tag coverage the promotion pass mirrors.
- Prior art: Wilson, "Pointer Swizzling at Page Fault Time" (Texas Persistent Store);
  PLOB (Kirschke); AllegroCache; Symbolics Statice.
