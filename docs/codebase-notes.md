# Code idioms and efficient access patterns

How the engine is used and tested here, in a few shapes, and what each
choice costs: for people, and for agents with a small context window.
Every qualified name here, such as `graph-db:map-edges`, is checked by
`tests/codebase-notes-tests.lisp`, so a rename fails the suite. The traps
live in `CLAUDE.md` and are linked, not copied.

## Idioms

**A graph needs a system directory.** `graph-db:make-graph` takes a name and
a location. Type ids come from the system-wide registry, so
`graph-db:*system-directory*` must be bound first; otherwise it fails with
`SYSTEM-DIRECTORY-REQUIRED` (GH #186). In tests,
`graph-db/test::with-test-graph` makes a scratch graph and binds
`graph-db:*graph*` to it.

**Writes go in a transaction.** Wrap every make, change or delete in
`graph-db:with-transaction`. `graph-db:def-vertex` and `graph-db:def-edge`
define a type and its `make-<type>` constructor; an edge takes `:from` and
`:to`.

```lisp
(with-transaction ()
  (make-link :from a :to b))
```

**A read inside the transaction sees its writes.** Lookups, typed and
adjacency scans, `graph-db:edge-exists-p`, `graph-db:index-lookup`,
`graph-db:index-count`, `graph-db:map-index-prefixes` and
`graph-db:count-index-lookup`, the spatial queries and
`graph-db:vector-search` return the transaction's own creates, saves and
deletes (#408, #409, #410, #411). Not the untyped scans or views.
`graph-db:*read-own-writes*` turns it off.

**A pass over a type.** `graph-db:map-vertices` with `:vertex-type`, or
`graph-db:map-edges` with `:edge-type`. A named type includes its subtypes
unless `:include-subclasses-p` is nil.

**One node's edges.** `graph-db:map-edges` with `:vertex` and `:direction`
(`:out` or `:in`), or `graph-db:outgoing-edges` and
`graph-db:incoming-edges`. An edge's ends are ids, `graph-db:from` and
`graph-db:to`; `graph-db:lookup-vertex` turns an id into its vertex.

```lisp
(map-edges (lambda (e) (lookup-vertex (to e)))
           *graph* :vertex v :direction :out :edge-type 'link :collect-p t)
```

**One pair.** `graph-db:edge-exists-p` takes an edge type and two vertices.

**A lookup by an indexed slot.** `graph-db:index-lookup` takes the graph,
the class, the slot and the value, and returns the matching vertices.
`graph-db:def-index` declares the index.

## Efficient choices

- **Name the type.** A typed or adjacency scan goes through an index and
  is snapshot-consistent. A fully untyped scan, with no type and no
  vertex, walks the raw edge table: it reads live versions, bypasses MVCC,
  and is refused under an as-of snapshot. See `graph-db:map-edges`'s
  docstring in `edge.lisp`.
- **Scan the small side.** To tally what one edge type connects, use one
  pass over that type, not a traversal per vertex of the side it points
  at.
- **Per-type counts overlap.** A parent type includes its subtypes, so
  summing a parent and its subtypes counts them twice. Pass
  `:include-subclasses-p nil` to every non-leaf type when the counts must
  add up (GH #219).
- **Pass `:collect-p t` only when you use the list,** since it builds one
  cons per edge.
- **Key a hash table by an id, with `:test 'equalp`.** An id is a 16-byte
  vector, so `eq` or `eql` on ids or nodes is true only while the node
  cache returns the same object.

## Traps, where they live

All in `CLAUDE.md`, «Tests»:

- **A bare `fiveam:run!` of a suite fails.** Run one suite with
  `tests/run-suite.sh`, which binds the fixture the way `run-tests` does.
- **The node cache hides identity bugs.**
- **One compiled-file cache per checkout.**
- **A suite needs a 16 GiB heap.**
