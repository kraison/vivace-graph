# GH #219 — `map-vertices` without `:vertex-type` — debug report

**Verdict: NOT AN ENGINE DEFECT.** The untyped walk is correct and complete.
The reported "48% loss" is a double-count in the per-type cross-check.

## What actually happens

`map-vertices` with `:vertex-type` defaults to `:include-subclasses-p t`
(`vertex.lisp:180`), and the typed branch expands the request through
`resolve-node-type-ids` (`vertex.lisp:230-241`) before walking the type index.
So a walk of a PARENT type already visits every vertex of every subtype.
Summing `parent + subtype-a + subtype-b` therefore counts the subtypes twice.

On the reporter's store `IMSMA-EVIDENCE-POINT` and `IMSMA-TURNING-POINT` are
subclasses of `IMSMA-RECORD`. The real store size is 14512 — which is exactly
what the untyped scan returned. The "28169" is `14512 + 5004 + 8653`, i.e. the
14512 plus a second copy of the two subtypes.

The edge line in the issue corroborates this: the two edge types are siblings
(no inheritance), so no double count, and untyped == the sum exactly (13635).

## Reproduction

`/tmp/.../scratchpad/repro3.lisp` builds the reporter's exact shape — a base
type with two populated subtypes plus two unrelated empty types, populated with
855 / 5004 / 8653 vertices and 5002 / 8633 edges — closes and reopens the store,
and prints:

```
untyped              = 14512
R-RECORD (default)   = 14512     <- parent, subclasses included
R-RECORD (no subs)   =   855
R-EVIDENCE           =  5004
R-TURNING            =  8653
S-FIND               =     0
S-AREA               =     0
naive sum            = 28169
edges untyped        = 13635  e1=5002 e2=8633
```

Every figure in the issue is reproduced to the digit, including the two zeros
and the edge total. 855 + 5004 + 8653 = 14512, and 14512 + 5004 + 8653 = 28169.

## Things ruled out along the way

- **Cold open / size.** Neither mattered. Untyped == the disjoint per-type sum
  on 23-vertex and 6000-vertex stores, hot and after a `close-graph`/`open-graph`
  round trip, with vertices written interleaved and in per-type blocks, with
  three and with five types.
- **`map-lhash` iteration** (`linear-hash.lisp:782-793`) — walks
  `0..%bucket-count-1`, and `read-bucket` (`linear-hash.lisp:502-518`) follows
  the overflow chain. The untyped total also matches
  `(read-lhash-count (vertex-table g))` exactly, which is maintained by the
  insert path, not by the scan — an independent witness that no pair is dropped.
- **`deserialize-vertex-head`** (`vertex.lisp:65-87`) — resolves type-id -> class
  via `*graph*`, and `map-vertices` binds `*graph*` to its GRAPH argument
  (`vertex.lisp:205`). The `migrate-graph` comment the reporter flagged
  (`backup.lisp:376`) is that binding being honoured by a caller that maps a
  graph other than the ambient one, not a divergent code path.
- **Pooled node buffers** (`buffer-pool.lisp:500-524`) — `get-vertex-buffer`
  pops a buffer and never returns it, so there is no stale `deleted-p` /
  `written-p` carried between deserializations.

## Blast radius

None: nothing to fix in the storage or scan path. `map-edges` shares the same
`:include-subclasses-p t` default and the same untyped `map-lhash` branch, so it
has the same *reporting* trap; its numbers in the issue happen to be clean only
because those two edge types are siblings.

## Proposed minimal change (optional, docs-only)

There is no code defect to fix. If anything ships for #219 it should be a
docstring note on `map-vertices` / `map-edges` (`vertex.lisp:181-198`) saying
that per-type walks OVERLAP by default and a per-type sum is only comparable to
the untyped total with `:include-subclasses-p nil` on non-leaf types. Close #219
as not-a-bug with the arithmetic above.

## Regression test

`tests/map-vertices-untyped-tests.lisp` (registered in `graph-db.asd`), suite
`map-vertices-untyped-suite`, 12 checks, all passing:

- untyped == the DISJOINT per-type sum == `read-lhash-count` of the vertex
  table, across a close/reopen;
- the parent's default walk equals the untyped total and equals
  bare + subtype-a + subtype-b (pinning the overlap that misled the report);
- the same equality at 2700 vertices.
