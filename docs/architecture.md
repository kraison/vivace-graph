# VivaceGraph architecture

Drawn from experiment at 4d09242, 2026-11-28. Each figure names the
files it rests on. The figures are SVG files drawn by
`docs/architecture/figures.py`, with every box placed by hand, using the
shared helpers in `svgfig.py` (vendored from the diagrams skill). When a
change alters what a figure shows, edit that figure's function, run
`python3 docs/architecture/figures.py`, look at the result, and update
this page's date and commit.

VivaceGraph is a graph database in Common Lisp: an object model over
memory-mapped files, ACID transactions with multi-version storage, a
Prolog engine, and optional replication, REST, GUI, temporal-claim and
rules systems, all loaded into one Lisp image. The prose manual is
`docs/vivace-graph-v3-doc.org`; this page is the set of pictures it
lacks, and it points into the manual rather than repeating it.

- [The layers](#the-layers)
- [The process and what it talks to](#the-process-and-what-it-talks-to)
- [On disk](#on-disk)
- [The claim model](#the-claim-model)
- [Inside the process](#inside-the-process)
- [A commit](#a-commit)

## The layers

`graph-db.asd` defines a dozen systems. `graph-db/core` is the engine:
seventy-seven files in one dependency chain, drawn here as eight rows in
load order, the storage primitives at the bottom. Everything above the
core is an add-on that loads only when asked for. The full `graph-db`
system is the core plus the network transport, the guarded-query
layer and the REST server; it is what the manual's examples load.

![the layers of graph-db/core in load order and the systems built on
it, with what each takes from the core](architecture/layers.svg)

Teal: a system of this repository. Each arrow says what the upper side
takes from the lower; the two-headed one is a call in each direction,
because the core calls `replicate-transaction` on every master commit
and the only method lives in the transport. Not drawn: the test
systems, the profiler, and the forward declarations by which lower core
files call upper ones (`graph.lisp:3-16`, `transactions.lisp:3-11`),
which is why the rows are a load order and not a strict dependency
order. The add-ons write into the `graph-db` package and plug into the
core through hooks: commit validators (`spacetime/membership.lisp:206`,
`spacetime/temporal.lisp:115`) and after-commit hooks
(`rules/forward.lisp:321`).

*Drawn from:* `graph-db.asd:13-173` (core), `:183-197`, `:202-216`,
`:237-250`, `:275-296`, `:306-327`, `:494-522`, `:553-562`,
`:589-616`; `geos/geos-ffi.lisp:50-95`; the manual, Part II.

## The process and what it talks to

Everything runs inside one Lisp image. The engine opens stores and runs
commits on whatever thread calls it; the four doors at the right are
the places the image listens or connects. The replication listener
binds every interface and has no default port; REST and the GUI bind
loopback unless told otherwise, and the GUI has no authentication at
all, so a non-loopback bind exposes its API.

![a graph-db image, the processes that connect to it, the files it keeps
and the subprocesses it runs](architecture/system.svg)

An arrowhead marks the side that listens; two heads, calls each way. A
dashed box is a subprocess: each REST login runs `openssl passwd` (or
`htpasswd` to manage users) with the password on standard input, and
`visualize :render` runs Graphviz. The system directory is the one place
two images meet: both append to the type and store registries under
`flock`, but the system clock's lock has a single owner. Not drawn: the
embedded mode in which `make-gui-app` mounts the cockpit under a host
application's own listener, and the shared libraries loaded in-process
(libc through CFFI, `libgeos_c` for the GEOS add-on).

*Drawn from:* `rest.lisp:3, 113-142, 156-215, 705-764`;
`gui/server.lisp:10, 272-337`; `gui/static/js/api.js:22`;
`transaction-streaming.lisp:15-19, 97-131, 298-317, 386-414, 460-495`;
`peer-streaming.lisp:18-44, 1045-1062, 1687-1735, 1744-1782`;
`algorithms/io.lisp:282-302`; `system-clock.lisp:36-43, 69-84`;
`store-registry.lisp:31-35, 117-134`; `type-registry.lisp:81-82,
311-325`; the manual, Chapters 8 (REST) and 9 (replication).

## On disk

A graph is a directory. Two files are allocator regions that everything
else points into: `heap.dat` holds node data, the archived versions a
read at an epoch walks, and the cells of every adjacency chain;
`indexes.dat` holds every ordered map. The node tables and the three
adjacency indexes are linear-hash directories whose values are small
fixed heads; the sidecars at the right hold roots and metadata, not
contents. The manual's Chapter 3 describes the engine; its directory
tree there is older than this figure (see the drift issue below).

![the files of one graph directory, what each holds and which point into
heap.dat and indexes.dat; the files outside it at the right](architecture/disk.svg)

Heavy black: a file the engine maps or appends to. A dotted box is
optional or per variant: the peer sidecars exist only for a peer role,
the memory-graph backend keeps none of the mapped files and writes a
`graph.img` instead, and shadow generations live beside the directory,
not in it. Not drawn: the legacy `spatial-index.root`, read only as a
migration signal, and the unused `main`/`meta`/`data.dat` constants.

| file | holds | written by |
|---|---|---|
| `heap.dat` | node data, archived heads, index-list cells | every commit; `gc-heap` at open |
| `indexes.dat` | views, unique, secondary, count and spatial maps | commits; never reclaimed |
| `vertex/`, `edge/` | linear hash: id to 33- or 73-byte head | commits |
| `ve-index-in/`, `ve-index-out/`, `vev-index/` | linear hash: adjacency heads | commits |
| `vertex-index.dat`, `edge-index.dat` | index-list heads by type id | commits; grows on demand |
| `schema.dat` | the cl-store'd schema | `def-vertex`, `def-edge`, runtime schema |
| `views.dat` | per view: code, root, backend | view creation and deletion |
| `spatial-indexes.dat` | per spatial index: owner, slot, root, precision | make-graph, close, rebuilds |
| `unique-`, `secondary-`, `count-indexes.dat` | root addresses | close-graph, via `.tmp` and rename |
| `vseg-<owner>-<slot>.dat` | a vector segment per `:vector-index` slot | the first conforming write |
| `tx/` | `prep-*.txn-tmp`, `<id>.txn`, `replication-*.log` | every commit; masters and peers log |
| `txn-log/` | `snap-<time>-<uuid>` under `in-progress/` until complete | `snapshot`; `close-graph` by default |
| `transaction-id.dat` | the highest committed id | commits |
| `.dirty` | the open marker; refuses a second opener | open; removed by `close-graph` |
| `policy.dat` | the recovery policy | only with `:recovery-policy` |
| peer sidecars, `applied-ops/` | Lamport counters, stamps, conflicts, rejections | the peer roles |

Outside the directory, `*system-directory*` holds `type-registry.log`,
`store-registry.log` and its `.lock`, `edge-occupancy.dat` and
`schema-manifest.dat`; the clock directory holds `system-clock.lock`,
`system-clock.dat`, `system-journal.log` and the restore manifests.

*Drawn from:* `graph.lisp:495-664, 884-1137, 1302-1440`;
`allocator.lisp:3-8, 222-233`; `linear-hash.lisp:52-55, 258-294`;
`ve-index.lisp:8-11, 144-159`; `vev-index.lisp:12-14, 174-189`;
`type-index.lisp:59-82`; `primitive-node.lisp:3-14, 64-79`;
`edge.lisp:3-8, 75-85`; `schema.lisp:110-120`; `views.lisp:184-230`;
`index.lisp:697-717`; `count-index.lisp:455-479`;
`unique-constraint.lisp:970-1000`; `spatial-registry.lisp:117-125`;
`transactions.lisp:1176-1200, 1662-1669, 2540-2585, 2655-2722,
3232-3250, 3686-3692, 3882-3906`; `txn-log.lisp:7-86, 170-189`;
`backup.lisp:215-247, 331-364`; `memory-graph.lisp:204, 352-392,
1047-1068, 1189-1203, 1328`; `shadow-store.lisp:85-120, 305-307,
514`; `system-restore.lisp:606-609`; `peer-merge.lisp:134-137,
204-207, 400-403`; `peer-streaming.lisp:1836-1838`;
`globals.lisp:95-116, 146-153, 311-333, 343-374, 388-421`;
`type-occupancy.lisp:30-36`; `runtime-schema.lisp:21-27`.

## The claim model

The spacetime add-on stores claims as ordinary vertices: a claim family
is a parent class with a unary and a binary subclass, declared by
`def-claim-classes`, and nothing in `spacetime/` or `rules/` writes a
file of its own. What makes the model worth a picture is how its
records point at one another: almost never by an edge, almost always by
a value that names a record, joined in code. The manual's spacetime
chapters and `docs/rules.md` give the semantics.

![the spacetime claim model: a claim family's slots and keys, its
endpoints, the rules schema and the derivation and firing records that
name claims by identity key](architecture/claims.svg)

A solid arrow is an edge node the store can follow: the `subject-of`
and `object-of` edges, which are an optional cache made only when the
endpoint is unambiguous and in the same store. A dashed arrow is a
value: an endpoint is named by namespace and key and found through its
class's key-slot index; a derivation's subject and object are identity
keys, so a premise may sit in another store, named by `method`; a
derived claim names its rule only through the string `rule/NAME` and
its `rule-version`; a firing names its production by relation. Not
drawn: the registration claims `register-node` writes (binary claims
with standing `:inferred`), the retraction and reopen steps, which are
a copy and a save that close or reopen the transaction extent, and the
versions a read at an epoch walks, which are the engine's archived
heads, not an entity of the model.

*Drawn from:* `spacetime/claim.lisp:14-48, 111-134, 160-262, 373-420,
468-585, 628-630`; `spacetime/claim-query.lisp:29-131, 162-244,
396-462, 520-579, 641-688`; `spacetime/source.lisp:16-45, 199-279`;
`spacetime/resolve.lisp:9-72`; `spacetime/link.lisp:41-143, 285-300`;
`spacetime/register.lisp:297-313, 374-398`;
`spacetime/membership.lisp:36-42, 83-102`; `rules/schema.lisp:6-56`;
`rules/compile.lisp:12-43, 178-203, 268-274`; `rules/run.lisp:121-128,
546-572, 596-604, 611-702, 2102-2170`; `rules/facts.lisp:17-33, 65-73,
1164-1187`; `rules/forward.lisp:15-50, 260-272`;
`rules/productions.lisp:7-13`; `query/compiled-cache.lisp:115-132,
187`; `docs/rules.md:881-905`.

## Inside the process

The engine starts very few threads. A commit, its journal write, the
apply, the reaper, view and index upkeep, spatial indexing and forward
derivation all run on the thread that called `with-transaction`. The
threads that do exist come with a role: a plain disk graph starts only
the process-wide buffer-pool thread; a master adds a listener and a
session per slave; a slave, one receiving thread; a peer hub or
device, its own; a graph with forward rules gets a timer thread the
first time a threshold is scheduled; HTTP threads exist only after
`start-rest` or `start-gui`.

![what runs on the caller's thread, the threads the engine starts and
when, and the lock domains](architecture/threads.svg)

Teal: a thread of this process. The lock column is the order the
commit path takes them, outermost first: the transaction-manager lock
is a recursive mutex, one per store, and the retry fallback holds it
for a whole body. The per-class read-write locks the schema creates are
taken by nothing. Not drawn: CCL's short-lived "Timed Process" threads
under every `receive-message` poll, log4cl's watcher thread (started
only by an application's appender), and the per-implementation
differences in what is recursive (`utilities.lisp:513-523`,
`rw-lock.lisp:252-259`, `linear-hash.lisp:313-316`).

*Drawn from:* `buffer-pool.lisp:22-24, 57-138, 320-332, 467-478`;
`transaction-streaming.lisp:218-234, 270-317, 366-452, 460-495`;
`peer-streaming.lisp:955, 992, 1045-1062, 1326, 1420, 1482-1563,
1691-1734, 1744-1782`; `rules/thresholds.lisp:172-259`;
`rules/forward.lisp:38-52, 167-212, 247-258, 291-364`;
`rest.lisp:760-764`; `gui/server.lisp:321-337`; `gui/api.lisp:208-280`;
`transactions.lisp:886-927, 1003-1021, 1220, 2035-2067, 2745-2750,
2881, 3163-3166, 3296-3298, 3412, 3460-3463, 3492, 3738-3802,
3878-3880, 3915-3920, 3959-3973`; `graph-class.lisp:110, 214, 303-322,
445-449, 516-523`; `graph.lisp:649, 1054-1055, 1173-1174`;
`gc.lisp:151-172`; `schema.lisp:61-69, 104-107`; `views.lisp:14,
98-132, 204, 238, 540-543, 612, 746, 763, 1146-1151`;
`linear-hash.lisp:230-233, 313-316, 420-440`; `type-index.lisp:13-57,
97-161`; `rw-lock.lisp:183-191, 252-259`; `utilities.lisp:513-523,
556-640`; `mailbox.lisp:10-45`; `spatial-query.lisp:433, 492`;
`spacetime/link.lisp:285-298, 336`; `query/compiled-cache.lisp:193-221`;
`globals.lisp:78-84`.

## A commit

Every write passes through one path, and the manual's Chapter 5 tells
it in prose; this is the same path with its lock boundary drawn. Off
the lock, a transaction registers, runs its body against the versions
visible at its start epoch, and serializes its writes to a temporary
journal file. Under the transaction-manager lock it is validated
against every transaction committed since it started, given its epoch,
made durable by one rename, applied, and handed to the slave sessions.
After the lock come the bookkeeping and the after-commit hooks, which
is where forward rules run.

![the commit path of with-transaction in order, banded by whether the
transaction-manager lock is held, with the retry loop and the side
effects](architecture/commit.svg)

The teal band is held under the lock. A conflict retries the body; at
`*maximum-transaction-attempts*` (8) the body runs once more under the
lock with no handler, so nothing can conflict and nothing else can
start. A read at an epoch is the read-only specialisation of the same
machinery: it registers a transaction one epoch above the one asked
for, pins the reaper floor, and resolves each read along the archived
heads; `docs/time-travel.md` gives the semantics and the refusals. Not
drawn: `tx-overlay`, the `:committing` count that keeps the lagging
version a concurrent committer may still need, and the Lamport and
field-stamp steps a peer adds to the journal write.

*Drawn from:* `transactions.lisp:16-48, 138-142, 256-275, 325-402,
430-467, 477-513, 588-598, 667-766, 868-927, 960-1021, 1070-1121,
2033-2067, 2576-2613, 2815-2919, 2962-2968, 3099-3134, 3163-3166,
3271-3316, 3346-3381, 3396-3414, 3475-3521, 3540-3682, 3686-3834,
3878-3906`; `globals.lisp:78-84, 540-548`; `graph-class.lisp:483-493`;
`vertex.lisp:124-131, 238-248, 263, 278-336`; `edge.lisp:158-165,
424-426, 612-615`; `prologc.lisp:1667-1677`;
`transaction-streaming.lisp:228-234`; `docs/time-travel.md:27-63`.

Where the other docs disagree with the code these figures were drawn
from: #503.
