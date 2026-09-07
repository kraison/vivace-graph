# Time travel: reading the graph as of an epoch

GH #115 (Phase C-3 of #117). Spec: `superpowers/specs/2026-09-07-time-travel-api-design.md`.

## The epoch model

Every commit takes the next epoch from the store's transaction manager
(or from the `system-clock` the store is attached to). A node's
`commit-epoch` is the epoch of the transaction that wrote that version;
`:keep-revisions` says how many prior versions the reaper retains. An
epoch names a point in **this node's own history** (#116): it is not
portable to a replica, and it compares across stores only while they
share one system clock.

## Reading as of an epoch

    (with-as-of ((graph) epoch) ...)          ; an extent
    (lookup-vertex id :as-of epoch)           ; one call
    (map-vertices fn graph :vertex-type 'user :as-of epoch)
    (select (:as-of epoch) (?u) (is-a ?u user))
    (latest-epoch graph)                      ; the newest epoch you may name

Inside the extent every read of GRAPH resolves to the version whose
commit epoch is the newest **at or below** EPOCH (inclusive, as
`claims-touching :as-of-epoch` reads it). A node created after EPOCH
is absent; one deleted after EPOCH is present; typed scans, adjacency
(`outgoing-edges`, `traverse`), the generated lookup functions and the
Prolog functors all answer at EPOCH. Snapshots on several graphs
compose by nesting, one epoch per graph.

Refused (`as-of-refused`, with a `reason`): an epoch above
`latest-epoch`; an extent inside a read-write transaction on the graph;
a second epoch inside an open snapshot of the graph (the same epoch
inherits); the untyped `map-vertices`/`map-edges` scan, which reads live
versions; and the in-memory backend or a graph with no transaction
manager yet, both of which keep no history and refuse with the same
`:no-version-history` reason.

## Depth: keep-revisions is the depth of time travel

The default `:keep-revisions 0` keeps the live version and one lagging
version; an as-of read older than that on an updated node cannot be
answered. The engine says so rather than substituting a newer version:
`version-reaped-error` names the id, the epoch asked for, and the oldest
retained epoch. `:if-reaped :skip` on the extent skips such nodes and
counts them (`as-of-skipped-count`). Absence is told from reaping by the
oldest retained version's `revision`: 0 means the node was created
after the epoch. Once that creation version is itself reaped the two
cannot be told apart, and the read reports reaped.

Under `:if-reaped :error` (the default) a typed scan or `select` at an
epoch can itself signal `version-reaped-error` for any visited node
whose version at that epoch was reaped; use `:if-reaped :skip` to skip
and count instead.

An open as-of extent holds the reaper's floor at its epoch, so versions
live then are retained on any node updated meanwhile; versions reaped
before the extent opened are gone.

## History

`vertex-history`, `edge-history` and `node-history` return the retained
versions newest first as `(version . commit-epoch)`. A history never
signals; an oldest entry with `revision` above 0 means the chain was cut.

## Bounds

- Value indexes (slot indexes, the spacetime endpoint and producer
  indexes) keep live membership (#345): a node hard-removed from one
  after the epoch is not enumerated from it.
- Cost: membership at an epoch walks every entry ever indexed under a
  type, including tombstones, and resolves each id. Phase C-1 (#113)
  replaces that walk with epoch-stamped index entries; the API above
  does not change when it lands.
