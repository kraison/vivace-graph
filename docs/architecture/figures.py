"""docs/architecture/figures.py -- draws the figures in
docs/architecture.md as standalone SVG files beside this script.

Every box and arrow is placed by hand: the coordinates below are the
layout.  After a change the figures show, edit the figure's function,
then run

    python3 docs/architecture/figures.py

and look at the result before committing it.  Python 3, standard
library only, with the shared helpers in svgfig.py (vendored from the
diagrams skill)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.dont_write_bytecode = True  # no __pycache__ beside the figures
from svgfig import Fig, write_figures  # noqa: E402  (vendored beside us)


# ================================================================ FIG 1
# The systems graph-db.asd defines and the layers of graph-db/core, in
# load order from the bottom.  Core rows: x 24..464; column A x 620..880
# (the add-ons that need only the core); column B x 960..1216 (the ones
# that need the full graph-db, plus algorithms).  The 156 px gap between
# the core and column A carries the arrows into the core, one lane each.
CORE_X, CORE_W = 24, 440
A_X, A_W = 620, 260
B_X, B_W = 960, 256
ROW_H, ROW_P = 86, 100

CORE = [  # top row first: title, then up to three file lines, then exports
    ("8  API, secondary indexes, constraints, the in-RAM backend",
     ["interface · traverse · memory-graph · unique-constraint · index",
      "count-index · value-constraint · cardinality · domain-range",
      "evaluator · disjointness · spatial-registry"],
     "save, mark-deleted, traverse; def-index, def-unique; "
     "make-memory-graph"),
    ("7  Prolog engine",
     ["functor · prologc · prolog-tabling · prolog-functors",
      "prolog-graph-predicates · spatial-query · prolog-explain"],
     "select, select-flat, do-query; <-; table-functor; explain-goals"),
    ("6  Transactions, durability, recovery",
     ["transactions · tx-overlay · transaction-restore",
      "transaction-log-streaming · shadow-store · system-restore",
      "backup · replication · txn-log"],
     "with-transaction, update-node, delete-node; snapshot, replay"),
    ("5  Schema and object model",
     ["schema · runtime-schema · schema-tools · type-seeding",
      "node-class · views · primitive-node · vertex · edge · gc"],
     "def-vertex, def-edge, def-view; lookup-vertex, map-vertices, "
     "outgoing-edges"),
    ("4  Graph store and adjacency",
     ["graph-class · ve-index · vev-index · type-index · graph · stats"],
     "make-graph, open-graph, close-graph; the *graphs* registry"),
    ("3  Collections and spatial primitives",
     ["geometry · geometry-ops · geohash · linear-hash · cursors",
      "skip-list · skip-list-cursors · mem-skip-list · bplus-tree",
      "spatial-index · index-list"],
     "on-disk hash tables and ordered maps; geohash cells"),
    ("2  System-directory registries",
     ["sidecar-io · system-clock · type-registry · store-registry",
      "type-occupancy"],
     "type ids, store ids and the epoch clock, shared through files"),
    ("1  Platform and storage primitives",
     ["package · cl-store-ecl · globals · conditions · posix · utilities",
      "queue · mailbox · rw-lock · mmap · pcons · node-id · buffer-pool",
      "serialize · allocator · segment"],
     "mmap and POSIX over CFFI; the binned heap; binary serialization"),
]


def core_row(i):
    """y of core row I, 0 the top (layer 8)."""
    return 40 + i * ROW_P


def fig_layers():
    f = Fig("f1", 1240, 1010,
            "the layers of graph-db/core in load order and the systems "
            "built on it, with what each takes from the core")
    cr = CORE_X + CORE_W
    for i, (title, files, ex) in enumerate(CORE):
        y = core_row(i)
        f.box(CORE_X, y, CORE_W, ROW_H, title, files + [ex], "live")
    # the files under the core
    fy = core_row(7) + ROW_H + 44
    f.cyl(CORE_X, fy, 210, 70, "graph directories",
          ["one per store (figure 3)"], "store")
    f.cyl(CORE_X + 230, fy, 210, 70, "the system directory",
          ["registries and the clock"], "store")
    f.edge([(128, core_row(7) + ROW_H), (128, fy)])
    f.label(136, fy - 14, "mmap, pwrite, rename")
    f.edge([(358, core_row(7) + ROW_H), (358, fy)])
    f.label(366, fy - 14, "append under flock")

    # column A: add-ons that need only the core
    f.box(A_X, core_row(0), A_W, ROW_H, "graph-db/rules",
          ["rules/: facts, schema, compile, run, delta,",
           "forward, thresholds, productions, standing",
           "def-rule, run-rules, enable-forward-rules",
           "hooks *after-commit-hooks* for forward steps"], "live")
    f.box(A_X, core_row(1), A_W, ROW_H, "graph-db/spacetime",
          ["spacetime/: claim, claim-query, source,",
           "resolve, link, register, membership, temporal",
           "def-claim-classes, def-source, retract-claim",
           "+ cl-temporal-extent, local-time"], "live")
    f.box(A_X, core_row(2), A_W, ROW_H, "graph-db/query",
          ["query/: compiled-cache, dsl, guard",
           "run-guarded-prolog, run-pattern-query,",
           "run-query-goals; the compiled-query cache",
           "+ cl-json"], "live")
    f.box(A_X, core_row(3), A_W, ROW_H, "graph-db/replication",
          ["transaction-streaming, peer-merge,",
           "peer-streaming: the TCP transport for",
           "master/slave and hub/device roles",
           "+ usocket"], "live")
    f.box(A_X, core_row(4) + 10, A_W, ROW_H - 10, "graph-db/geos",
          ["geos/: ffi, context, bridge, ops",
           "libgeos_c in-process; soft-fails when absent",
           "needs the full graph-db to load (its .asd)"], "live")
    # column B: algorithms, and the full graph-db with what needs it
    f.box(B_X, core_row(0), B_W, ROW_H, "graph-db/algorithms (+ -io)",
          ["shortest-path, page-rank, components,",
           "flow, projection; GML, Pajek, Graphviz",
           "needs only the core",
           "+ cl-ppcre, yacc, parse-number"], "live")
    f.box(B_X, core_row(1), B_W, ROW_H, "graph-db/gui",
          ["gui/: api, prolog, server + static/",
           "start-gui, make-gui-app; the cockpit",
           "+ ningle, clack, cl-json"], "live")
    f.box(B_X, core_row(2), B_W, ROW_H, "graph-db  (the full system)",
          ["rest.lisp: start-rest, def-rest-procedure,",
           "def-query, add-rest-user",
           "= core + replication + query + REST",
           "+ hunchentoot, ningle, clack"], "live")

    # edges into the core, each on its own lane in the gap
    y = core_row(1) + 30                       # spacetime -> layer 8
    f.edge([(A_X, y), (560, y), (560, core_row(0) + 50),
            (cr, core_row(0) + 50)])
    f.label(512, core_row(0) + 44, "schema, indexes,", "middle")
    f.label(512, core_row(0) + 64, "commit validators", "middle")
    y = core_row(2) + 30                       # query -> layer 7
    f.edge([(A_X, y), (540, y), (540, core_row(1) + 30),
            (cr, core_row(1) + 30)])
    f.label(502, core_row(1) + 24, "goals, bindings", "middle")
    y = core_row(3) + 46                       # replication <-> layer 6
    f.edge([(A_X, y), (590, y), (590, core_row(2) + 56),
            (cr, core_row(2) + 56)], start=True)
    f.label(527, core_row(2) + 50, "replicate-transaction", "middle")
    f.label(527, core_row(2) + 70, "apply-transaction", "middle")
    y = core_row(4) + 60                       # geos -> layer 3
    f.edge([(A_X, y), (600, y), (600, core_row(5) + 24),
            (cr, core_row(5) + 24)])
    f.label(532, core_row(5) + 18, "geometry ops", "middle")
    # rules -> spacetime (adjacent), rules -> query (lane at x 912)
    f.edge([(A_X + 130, core_row(0) + ROW_H), (A_X + 130, core_row(1))])
    f.label(A_X + 138, core_row(1) - 4, "claims, derivations")
    f.edge([(A_X + A_W, core_row(0) + 60), (912, core_row(0) + 60),
            (912, core_row(2) + 44), (A_X + A_W, core_row(2) + 44)])
    f.label(918, core_row(0) + 70, "compiled goals", "start", rotate=90)
    # gui -> graph-db, graph-db -> query, graph-db -> replication
    f.edge([(B_X + 128, core_row(1) + ROW_H), (B_X + 128, core_row(2))])
    f.label(B_X + 136, core_row(2) - 4, "stores, Prolog, stats")
    f.edge([(B_X, core_row(2) + 24), (A_X + A_W, core_row(2) + 24)],
           halo=True)
    f.label(920, core_row(2) + 12, "guarded", "middle")
    f.label(920, core_row(2) + 46, "queries", "middle")
    f.edge([(B_X, core_row(2) + 72), (940, core_row(2) + 72),
            (940, core_row(3) + 30), (A_X + A_W, core_row(3) + 30)])
    f.label(886, core_row(3) + 48, "master, slave, peer roles")
    # algorithms -> core layer 8, over the top
    f.edge([(B_X + 128, core_row(0)), (B_X + 128, 22), (400, 22),
            (400, core_row(0))])
    f.label(700, 16, "map-vertices, outgoing-edges, with-transaction",
            "middle")
    # legend
    ly = fy + 100
    f.text(24, ly, "Teal: a system of this repository, loaded into one "
           "Lisp image; heavy black: files on disk. Each core row loads "
           "after the one beneath it. An arrow says what the", "t3")
    f.text(24, ly + 14, "upper side takes from the lower; the two-headed "
           "one is a call each way. Not drawn: the test systems, the "
           "profiler, and the forward declarations by which lower core",
           "t3")
    f.text(24, ly + 28, "files call upper ones (graph.lisp:3-16, "
           "transactions.lisp:3-11).", "t3")
    return f.svg()


# ================================================================ FIG 2
# One graph-db image on a host: its four doors (the replication listener
# and client, REST, the GUI) in a column at the image's right, each
# facing the process that uses it in the right-hand region; the engine
# at the left; the files and subprocesses under the image.
def fig_system():
    f = Fig("f2", 1240, 700,
            "a graph-db image, the processes that connect to it, the "
            "files it keeps and the subprocesses it runs")
    f.rect(12, 20, 880, 650, "rg", rx=10)
    f.text(880, 40, "ONE HOST", "rt", "end")
    f.rect(926, 20, 302, 650, "rg", rx=10)
    f.text(1212, 40, "OTHER PROCESSES · ANY HOST", "rt", "end")
    f.rect(28, 56, 836, 352, "hostbox", rx=8)
    f.text(848, 78, "a graph-db image · one Lisp process", "rt", "end")

    # the engine
    f.box(44, 96, 320, 290, "graph-db/core",
          ["Every open store: its tables, indexes",
           "and transaction manager.  A commit, its",
           "journal write, the apply, the reaper, view",
           "and index upkeep and forward derivation",
           "all run on the caller's thread (figure 5).",
           "",
           "Started threads: buffer-pool-thread (one",
           "per process), the replication threads,",
           "a forward timer per graph on demand."], "live")
    # the doors, right column of the image
    DX, DW, DH = 600, 248, 76
    doors = [
        (96, "replication listener",
         ["master or hub role; start-replication",
          "all interfaces · :replication-port",
          "framed packets, protocol 2, no TLS"]),
        (176, "replication client",
         ["slave or device role; connects out",
          "to master-host · peer-host",
          "slave: receive, apply; device: pull, push"]),
        (256, "start-gui",
         ["clack · JSON + vendored static files",
          "127.0.0.1:4270 · no authentication",
          "or mounted in a host's own app"]),
        (336, "start-rest",
         ["hunchentoot via clack · JSON",
          "127.0.0.1:8080 by default",
          "username and password per request"]),
    ]
    for y, t, ls in doors:
        f.box(DX, y - 4, DW, DH - 6, t, ls, "live")
    # engine <-> doors
    f.edge([(364, 120), (DX, 120)])
    f.label(482, 114, "each commit, via a mailbox", "middle")
    f.edge([(DX, 200), (364, 200)])
    f.label(482, 194, "apply-transaction, no OCC", "middle")
    f.edge([(DX, 280), (364, 280)], start=True)
    f.label(482, 274, "open-graph, select, stats", "middle")
    f.edge([(DX, 360), (364, 360)], start=True)
    f.label(482, 354, "procedures, def-query", "middle")

    # the other processes
    OX, OW = 942, 270
    f.box(OX, 92, OW, 60, "a slave image",
          ["a slave-graph: one thread connects,", "receives, applies"],
          "live")
    f.box(OX, 172, OW, 60, "a peer device image",
          ["a peer-graph :device; peer-sync pulls,", "then pushes"],
          "live")
    f.box(OX, 252, OW, 60, "browser",
          ["the cockpit's front end; relative",
           "api/ paths, nothing from a CDN"])
    f.box(OX, 332, OW, 60, "HTTP client",
          ["curl, an application; username and",
           "password as request parameters"])
    # others -> doors; the head is at the side that listens
    f.edge([(OX, 122), (DX + DW, 122)])
    f.label(895, 116, "tx packets", "middle")
    f.edge([(OX, 202), (DX + DW, 202)])
    f.label(895, 196, "ops, acks", "middle")
    f.edge([(OX, 282), (DX + DW, 282)])
    f.label(895, 276, "HTTP · pages", "middle")
    f.edge([(OX, 362), (DX + DW, 362)])
    f.label(895, 356, "HTTP · JSON", "middle")

    # under the image: files and subprocesses
    f.cyl(44, 450, 270, 86, "graph directories",
          ["one per open store (figure 3);",
           "a .dirty file while open, the journal in tx/"], "store")
    f.cyl(340, 450, 220, 86, "the system directory",
          ["type-registry.log, store-registry.log",
           "edge-occupancy.dat, schema-manifest.dat",
           "+ the clock dir: system-clock.dat, .lock"], "store")
    f.box(640, 450, 224, 60, "htpasswd · openssl passwd",
          ["one subprocess per REST login"], "sub")
    f.box(640, 560, 224, 60, "dot · circo · twopi · fdp",
          ["visualize :render (algorithms-io)"], "sub")
    f.box(340, 580, 220, 70, "another graph-db image",
          ["appends to the registries under",
           "flock; the clock lock has one owner"], "live")
    f.edge([(180, 386), (180, 450)])
    f.label(188, 424, "mmap, pwrite, rename")
    f.edge([(330, 386), (330, 408), (450, 408), (450, 450)])
    f.label(458, 436, "ids, epochs, under flock")
    f.edge([(500, 580), (500, 536)])
    f.label(508, 562, "flock")
    f.edge([(724, 406), (724, 450)])
    f.label(732, 434, "password on stdin")
    f.edge([(580, 408), (580, 590), (640, 590)])
    f.label(588, 580, "DOT text")
    f.text(24, 690, "Teal: this repository's code; heavy black: files; "
           "a dashed box is a subprocess. An arrowhead marks the side "
           "that listens; two heads, calls each way.", "t3")
    return f.svg()



# ================================================================ FIG 3
# One graph directory: the node tables at the left, the two allocator
# regions in the middle (everything else points into them), the index
# files and sidecars at the right, and the files outside the directory
# in the last column.  Gaps of 86 and 70 px carry the pointer arrows.
def fig_disk():
    f = Fig("f3", 1240, 780,
            "the files of one graph directory, what each holds and "
            "which point into heap.dat and indexes.dat; the files "
            "outside it at the right")
    C0, W0 = 24, 230
    C1, W1 = 340, 280
    C2, W2 = 680, 250
    C3, W3 = 970, 246
    # node tables
    f.cyl(C0, 60, W0, 92, "vertex/",
          ["linear hash: struct, config, table,",
           "overflow .dat; key: the 16-byte id",
           "value: the 33-byte node head"], "store")
    f.cyl(C0, 180, W0, 92, "edge/",
          ["the same four files; key: node id",
           "value: the 73-byte edge head",
           "(the head + from, to, weight)"], "store")
    f.cyl(C0, 320, W0, 112, "tx/  the write-ahead log",
          ["prep-<seq>.txn-tmp, renamed to",
           "<id>.txn at commit; deleted (or",
           ".committed) once applied; recovery",
           "replays the rest. replication-<id>.log"], "store")
    f.cyl(C0, 462, W0, 96, "txn-log/  snapshots",
          ["snap-<time>-<uuid>: a plist per node,",
           "written under in-progress/, renamed",
           "close-graph takes one; :keep prunes"], "store")
    f.box(C0, 590, W0, 86, "small sidecars",
          [".dirty  while open; refuses a 2nd opener",
           "transaction-id.dat  the highest id",
           "policy.dat  with :recovery-policy only"])
    # the two allocator regions
    f.cyl(C1, 60, W1, 156, "heap.dat",
          ["the binned allocator: magic, version 3,",
           "128 free bins, data from byte 1034",
           "node data at each head's data-pointer",
           "archived heads along prev-pointer (MVCC)",
           "index-list cells of every adjacency chain",
           "gc-heap sweeps it at open (:gc-heap-p)"], "store")
    f.cyl(C1, 300, W1, 126, "indexes.dat",
          ["a second allocator region, never reclaimed",
           "every ordered map: views, :unique,",
           "def-index, count and spatial indexes",
           "a skip list or a B+ tree, tagged per map"], "store")
    f.box(C1, 470, W1, 100, "the memory-graph variant",
          ["no heap, tables or index files: graph.img,",
           "a cl-store or VGMI native image, instead",
           "tx/*.txn are kept until a checkpoint",
           "no txn-log/ snapshot; .dirty tolerated"], "note")
    # adjacency and type indexes
    f.cyl(C2, 60, W2, 80, "ve-index-in/ · ve-index-out/",
          ["linear hash; key: node id + type id",
           "value: a 17-byte index-list head"], "store")
    f.cyl(C2, 160, W2, 80, "vev-index/",
          ["key: out id, in id, type id (36 bytes)",
           "value: an index-list head"], "store")
    f.cyl(C2, 260, W2, 94, "vertex-index.dat",
          ["and edge-index.dat: a raw array of",
           "index-list heads by type id; 4096",
           "slots, grows on demand"], "store")
    f.box(C2, 390, W2, 170, "index sidecars",
          ["schema.dat  the cl-store'd schema",
           "views.dat  per view: code, root, backend",
           "spatial-indexes.dat  owner, slot, root,",
           "   precision, backend, histogram",
           "unique-, secondary-, count-indexes.dat",
           "   roots only; written at close via .tmp",
           "vseg-<owner>-<slot>.dat  vector segments"])
    # outside the directory
    f.cyl(C3, 60, W3, 108, "the system directory",
          ["outside every graph; *system-directory*",
           "type-registry.log · store-registry.log",
           "store-registry.lock · edge-occupancy.dat",
           "schema-manifest.dat; flock across images"], "store")
    f.cyl(C3, 196, W3, 94, "the clock directory",
          ["system-clock.lock  one owner (LOCK_EX)",
           "system-clock.dat  the 8-byte ceiling",
           "system-journal.log · restore-*.manifest"], "store")
    f.box(C3, 330, W3, 112, "peer role only",
          ["lamport.dat · pull-cursor.dat",
           "push-ack.dat · field-stamps.dat",
           "node-origins.dat · conflicts.dat",
           "rejections.dat · applied-ops/ (lhash)"], "note")
    f.box(C3, 474, W3, 70, "shadow generations",
          ["lease.dat inside the shadow copy; retired",
           "copies beside it: <live>-retired-<epoch>"], "note")
    # edges: heads point into the heap
    f.edge([(C0 + W0, 106), (C1, 106)])
    f.label(297, 98, "data-pointer,", "middle")
    f.label(297, 124, "prev-pointer", "middle")
    f.edge([(C0 + W0, 226), (310, 226), (310, 150), (C1, 150)])
    f.edge([(C2, 100), (C1 + W1, 100)])
    f.label(645, 92, "index-list", "middle")
    f.label(645, 118, "cells", "middle")
    f.edge([(C2, 200), (C1 + W1, 200)])
    f.edge([(C2, 300), (650, 300), (650, 180), (C1 + W1, 180)])
    # sidecars hold roots into indexes.dat
    f.edge([(C2, 440), (650, 440), (650, 400), (C1 + W1, 400)],
           cls="soft")
    f.label(636, 394, "roots", "middle")
    f.text(24, 726, "Heavy black: a file or directory the engine maps or "
           "appends to; a dotted box is optional or per variant. A solid "
           "arrow is a pointer stored in the file; a dashed one is a root",
           "t3")
    f.text(24, 740, "address that names a map. Not drawn: the legacy "
           "spatial-index.root, read only as a migration signal, and the "
           "unused main/meta/data.dat constants.", "t3")
    return f.svg()


# ================================================================ FIG 4
# The claim model: the claim family in the middle, the endpoints at the
# right, the rules schema at the left, derivations and firings below.
def fig_claims():
    f = Fig("f4", 1240, 770,
            "the spacetime claim model: a claim family's slots and keys, "
            "its endpoints, the rules schema and the derivation and "
            "firing records that name claims by identity key")
    D = "⇢"
    LX, LW = 24, 230
    CX, CW = 330, 350
    rule = f.entity(LX, 40, "RULES SCHEMA", "rule  (def-source)",
                    "name · namespace :rule",
                    ["version · enabled",
                     "family: a claim class name",
                     "head · body",
                     "extent-policy :premises|:none"],
                    "def-rule, retire-rule", w=LW)
    prod = f.entity(LX, 530, "RULES SCHEMA", "production (def-source)",
                    "name · namespace :production",
                    ["version · family",
                     "body · salience",
                     "writes · action"],
                    "def-production", w=LW)
    claim = f.entity(CX, 60, "CLAIM FAMILY · def-claim-classes",
                     "<parent>  unary | binary",
                     "producer · subject · relation (+ object)",
                     [f"subject-namespace · subject-key {D} source",
                      "relation",
                      f"object-ns · object-key {D} source  (binary)",
                      f"producer {D} rule/NAME | production/NAME",
                      "rule-version · method · standing",
                      "confidence · fraction · geometry · precision-m",
                      "extent-sexp  (validity time)",
                      "transaction-extent-sexp  (record time)",
                      "version-stamp  (re-stamped on every save)"],
                     "callers, register-node, the rules", w=CW)
    src = f.entity(960, 60, "ENDPOINT · def-source", "<class>  (def-vertex)",
                   "key-slot (def-unique)",
                   ["namespace: a keyword",
                    "its own slots",
                    "registration facet (optional)"],
                   "callers", w=256)
    f.box(720, 230, 210, 96, "subject-of · object-of",
          ["def-edge, no slots: claim to endpoint",
           "an optional cache, made only for an",
           "unambiguous same-store match, or",
           "deferred (*link-claims-at-write*)"])
    deriv = f.entity(CX, 330, "CLAIM FAMILY · derivation",
                     "derivation  (binary claim)",
                     "rule/NAME · subject · relation · object",
                     [f"subject (:claim . derived identity key) {D}",
                      "relation \"derived-from\"",
                      f"object (:claim . premise identity key) {D}",
                      f"producer rule/NAME {D} rule",
                      "method: the premise's store (nil: own)",
                      "standing :inferred · no extent"],
                     "run-rules, forward steps", w=CW)
    fire = f.entity(CX, 570, "CLAIM FAMILY · firing", "firing  (binary claim)",
                    "production/NAME · subject · relation · object",
                    ["subject (:production . FNV-1a hash)",
                     f"relation = the production's name {D}",
                     f"object (:instantiation . premise keys) {D}",
                     "standing :inferred"],
                    "run-agenda", w=CW)
    f.box(720, 370, 210, 170, "indexes on the family",
          ["claim-subject · claim-object",
           "claim-producer · claim-relation",
           "claim-subject-relation",
           "claim-touched-at  (the change feed)",
           "claim-validity  (current, end)",
           "counts: subject, object, relation",
           "in indexes.dat; roots in sidecars"])
    f.box(960, 370, 256, 170, "image-only, never persisted",
          ["claim families · source contracts",
           "namespace → source class",
           "membership disjointness specs",
           "rule and production specs",
           "compiled rules · forward state",
           "the compiled-query cache"], "note")
    cr, cl, cb = claim["r"], claim["x"], claim["b"]
    # endpoints are named by value
    y = claim["row"](0)
    f.edge([(cr, y), (src["x"], y)], cls="soft")
    f.label(820, y - 6, "by (namespace, key), via the key-slot index",
            "middle")
    y = claim["row"](2)
    f.edge([(cr, y), (src["x"], y)], cls="soft")
    # the cached edges
    f.edge([(cr, 250), (720, 250)])
    f.label(700, 244, "from", "middle")
    f.edge([(930, 250), (1088, 250), (1088, src["b"])])
    f.label(1010, 244, "to: the endpoint's id", "middle")
    # producer strings name a rule, by name
    y = claim["row"](3)
    f.edge([(cl, y), (rule["r"], y)], cls="soft")
    f.label(292, y - 20, "rule/NAME", "middle")
    f.label(292, y - 6, "rule-version", "middle")
    y = deriv["row"](3)
    f.edge([(cl, y), (134, y), (134, rule["b"])], cls="soft")
    # derivation and firing keys name claims
    y = deriv["row"](0)
    f.edge([(cl, y), (306, y), (306, 250), (cl, 250)], cls="soft")
    f.label(300, (y + 250) / 2, "names claims, any store", "middle",
            rotate=-90)
    y = fire["row"](2)
    f.edge([(cr, y), (700, y), (700, 290), (600, 290), (600, cb)],
           cls="soft")
    y = fire["row"](1)
    f.edge([(cl, y), (prod["r"], y)], cls="soft")
    f.label(292, y - 6, "by name", "middle")
    f.text(24, 726, "A solid arrow is an edge node the store can follow; "
           "a dashed arrow is a value that names a record (an identity "
           "key, a producer string, a namespace and key), joined", "t3")
    f.text(24, 740, "in code. The rule and derivation classes must be "
           "loaded before a store that holds them is opened.", "t3")
    return f.svg()


# ================================================================ FIG 5
# Inside the process: what the caller's thread does (left), the threads
# the engine starts and when (middle), the lock domains (right).
CALLER = [
    ("A COMMIT  (figure 6)", [
        ("the body", "reads record a read-set; writes a write-set"),
        ("prepare", "serialize to tx/prep-<seq>.txn-tmp"),
        ("validate", "under the TM lock; retry on conflict"),
        ("rename", "tx/<id>.txn: the durability point"),
        ("apply", "heads, cache, views, spatial, vectors"),
        ("indexes", "unique, secondary, count"),
        ("reap", "old versions above the floor"),
        ("hooks", "*after-commit-hooks*: forward-step")]),
    ("AT OPEN", [
        ("recover", "replay the tx/*.txn left behind"),
        ("gc-heap", "sweep heap.dat (:gc-heap-p)"),
        ("rebuild", "stale spatial, unique, count indexes")]),
    ("ON DEMAND", [
        ("snapshot", "txn-log/snap-*; close-graph's default"),
        ("replay", "a snapshot into a fresh store"),
        ("peer-sync", "a device pulls, then pushes"),
        ("link sweep", "claim endpoints, under a graph lock"),
        ("forward-step", "with :drain :caller, on this thread")]),
]

THREADS = [
    ("buffer-pool-thread", "ANY DISK GRAPH", "live", [
        "one per process: the first open with :buffer-pool-p",
        "loop: monitor-buffer-pool, then sleep 1; tops up free lists",
        "close-graph never stops it; stop-buffer-pool joins"]),
    ("server listener for <name>", "MASTER", "live", [
        "start-replication; wait-for-input 1 s, then accept",
        "stop-replication sets the flag and joins it"]),
    ("slave session  (one per slave)", "MASTER", "live", [
        "handshake; replays tx/replication-*.log; then every",
        "mailbox message (:timeout 1) to the socket",
        "exits on :shutdown, the flag or a broken pipe; not joined"]),
    ("<name>-repl-slave-thread", "SLAVE", "live", [
        "connects (10 tries, doubling backoff); reads a tx into",
        "tx/*.txn, then apply-transaction: no TM lock, no OCC",
        "reconnects on EOF; stop: flag + join"]),
    ("<name>-peer-hub · peer hub session", "HUB", "live", [
        "accept; per device: handshake, auth, a pull phase under",
        "with-read-snapshot, a push phase through with-transaction"]),
    ("<name>-peer-writer", "DEVICE", "live", [
        "the store's single writer: mailbox ops :state-create,",
        ":authored, :purge, :barrier, :local-write; replies on a",
        "per-request mailbox; :shutdown + join"]),
    ("forward timer  (one per graph)", "RULES", "live", [
        "started by the first threshold timer, not by enable-",
        "forward-rules; condition-wait to the earliest instant;",
        "fires forward-step here unless :drain :caller; joined"]),
    ("clack-handler-hunchentoot", "HTTP", "live", [
        "only after start-rest or start-gui: a listener thread,",
        "then one hunchentoot-worker per connection (the default",
        "taskmaster); stop-rest, stop-gui"]),
]

LOCKS = [
    ("TM lock", "a recursive mutex per store: create-transaction,",
     "all of %commit; the retry fallback holds it for the body"),
    ("under it, in order", "system-clock lock (tm-next-epoch) ·",
     "replication-log-lock · lamport-lock · the per-tx rw-lock ·",
     "lhash split lock then bucket · type-index stripes · view-group",
     "write locks (try-lock + sleep) · read-pins-lock · watermark-lock"),
    ("txn-lock", "snapshot and spatial rebuilds only; not on the",
     "commit path"),
    ("views-lock", "save-views; per-view-group rw-locks: map and",
     "reduce read, regenerate and delete write"),
    ("slaves-lock", "session add and remove; broadcast reads without it"),
    ("*forward-states-lock*", "then a per-state lock; the timer holds",
     "the state lock while it waits"),
    ("*gui-rw-lock*", "outermost in the GUI: reads shared, open and",
     "close exclusive"),
    ("buffer pool", "*buffer-pool-lock* on CCL and ECL; atomic",
     "push and pop on SBCL"),
    ("per-class rw-locks", "created by the schema, taken by nothing"),
]


def fig_threads():
    f = Fig("f5", 1240, 900,
            "what runs on the caller's thread, the threads the engine "
            "starts and when, and the lock domains")
    PX, PW = 24, 340
    TX, TW = 450, 380
    LX, LW = 866, 350
    RH = 24
    GAP = (PX + PW + TX) / 2          # label x between the two columns
    # the threads first, so the caller's frame can match their height
    ty = 40
    tops = {}
    for title, tag, kind, lines in THREADS:
        h = 30 + 15 * len(lines)
        f.box(TX, ty, TW, h, title, lines, kind)
        f.text(TX + TW - 10, ty + 20, tag, "tag", "end")
        tops[title] = (ty, h)
        ty += h + 14
    col_bottom = ty - 14
    # the caller's thread
    y = 40
    frame = len(f.parts)
    f.add("")
    f.text(PX + 12, y + 22, "the caller's thread", "t1")
    f.text(PX + 12, y + 40, "an application thread, an HTTP worker,", "t2")
    f.text(PX + 12, y + 55, "a hub session or the forward timer", "t2")
    y += 66
    zebra = False
    rows = {}
    for gname, steps in CALLER:
        f.text(PX + 12, y + 12, gname, "grp")
        y += 20
        for name, detail in steps:
            if zebra:
                f.rect(PX + 1, y, PW - 2, RH, "zebra", rx=0)
            zebra = not zebra
            f.text(PX + 14, y + RH / 2 + 4.5, name, "rn")
            f.text(PX + 118, y + RH / 2 + 4.5, detail, "rr")
            rows[name] = y + RH / 2
            y += RH
        y += 6
    f.parts[frame] = (f'<rect x="{PX}" y="40" width="{PW}" '
                      f'height="{col_bottom - 40}" rx="6" class="nd live"/>')
    # the locks
    ly = 40
    lframe = len(f.parts)
    f.add("")
    f.text(LX + 12, ly + 22, "lock domains", "t1")
    yy = ly + 42
    for name, *lines in LOCKS:
        f.text(LX + 12, yy, name, "tms")
        yy += 15
        for ln in lines:
            f.text(LX + 12, yy, ln, "t3")
            yy += 14
        yy += 8
    f.parts[lframe] = (f'<rect x="{LX}" y="{ly}" width="{LW}" '
                       f'height="{yy - ly}" rx="6" class="nd"/>')
    # edges between the caller and the threads, orthogonal, one lane each
    t = tops["buffer-pool-thread"]
    f.edge([(PX + PW, rows["the body"]), (GAP + 12, rows["the body"]),
            (GAP + 12, t[0] + 30), (TX, t[0] + 30)], start=True)
    f.label(GAP + 6, t[0] + 24, "get-buffer", "end")
    t = tops["slave session  (one per slave)"]
    f.edge([(PX + PW, rows["apply"]), (GAP, rows["apply"]),
            (GAP, t[0] + 30), (TX, t[0] + 30)])
    f.label(GAP + 6, t[0] + 24, "the tx", "start")
    t = tops["forward timer  (one per graph)"]
    f.edge([(PX + PW, rows["hooks"]), (GAP - 12, rows["hooks"]),
            (GAP - 12, t[0] + 30), (TX, t[0] + 30)])
    f.label(GAP - 6, t[0] + 24, "a timer", "start")
    t = tops["clack-handler-hunchentoot"]
    f.edge([(TX, t[0] + 30), (PX + PW, t[0] + 30)])
    f.label(GAP, t[0] + 24, "a request", "middle")
    t = tops["server listener for <name>"]
    t2 = tops["slave session  (one per slave)"]
    f.edge([(TX + TW - 30, t[0] + t[1]), (TX + TW - 30, t2[0])])
    f.label(TX + TW - 36, t2[0] - 5, "accept, spawn", "end")
    bottom = max(col_bottom, yy) + 30
    f.text(24, bottom, "Teal: a thread of this process. Threads exist by "
           "role: a plain graph starts only the buffer pool; shadow opens "
           "start no replication thread. The arrows say what passes",
           "t3")
    f.text(24, bottom + 14, "between the caller and a thread. Not drawn: "
           "CCL's per-poll \"Timed Process\" threads under receive-message, "
           "and log4cl's watcher, which only an application's", "t3")
    f.text(24, bottom + 28, "appender starts.", "t3")
    f.h = bottom + 44
    return f.svg()


# ================================================================ FIG 6
# The commit path: the steps in order, banded by whether the TM lock is
# held, with the retry loop at the left and the side effects at the right.
COMMIT = [
    ("OFF THE LOCK", [
        ("with-transaction", ["call-with-transaction binds *transaction*"]),
        ("create-transaction", ["the TM lock, briefly: the accepting-p gate;",
                                "start-tx-id = the current epoch; a sequence",
                                "number; registered :active in the TM table"]),
        ("the body", ["lookup-object: own writes first, then the version",
                      "visible at start-tx-id (resolve-version-at-epoch);",
                      "read-set by node id, unless *record-reads* is nil"]),
        ("writes", ["creates to create-set; updates and deletes to",
                    "write-set; nothing touches the store yet"]),
        ("%commit: prepare", ["state :committing; prepare-tx-persistence",
                              "serializes to tx/prep-<seq>.txn-tmp, id 0"])]),
    ("UNDER THE TM LOCK  (a recursive mutex, one per store)", [
        ("finish-tx-id", ["= the current epoch"]),
        ("validate", ["an empty write-set passes; else every committed",
                      "tx between start and finish: a write/write or",
                      "read/write overlap signals validation-conflict"]),
        ("validators", ["validate-live-heads; *commit-validators* with",
                        "*read-own-writes* nil; vector-segment checks"]),
        ("the epoch", ["transaction-id = tm-next-epoch: the shadow lease,",
                       "else the system clock, else the store's counter"]),
        ("prune", ["prune-committed-transactions keeps what a",
                   ":committing transaction still needs"]),
        ("finalize", ["rename to tx/<hex id>.txn: the durability point;",
                      "masters and peers append replication-<id>.log"]),
        ("apply", ["*commit-epoch*; archive the old head on prev-pointer;",
                   "cache; views; spatial; vectors; unique, secondary",
                   "and count indexes; reap-old-versions; watermark"]),
        ("broadcast", ["a master: the tx into each slave session's mailbox"])]),
    ("AFTER", [
        ("committed", ["state :committed; patch the .txn header id"]),
        ("cleanup", ["delete the .txn, or rename it .committed; the tx",
                     "stays in the TM table for OCC until pruned"]),
        ("hooks", ["*after-commit-hooks* on this thread, no lock:",
                   "the forward-rules drain"])]),
]


def fig_commit():
    f = Fig("f6", 1240, 900,
            "the commit path of with-transaction in order, banded by "
            "whether the transaction-manager lock is held, with the "
            "retry loop and the side effects")
    PX, PW = 70, 560
    NAME_W = 140
    FX, FW = 680, 220          # the files and threads a step reaches
    SX, SW = 940, 276          # the side notes
    LANE = 655
    y = 40
    rows = {}
    bands = []
    for gname, steps in COMMIT:
        by = y
        y += 24
        for name, lines in steps:
            h = 15 * len(lines) + 12
            f.text(PX + 12, y + 16, name, "tms")
            for i, ln in enumerate(lines):
                f.text(PX + NAME_W, y + 16 + 15 * i, ln, "rr")
            rows[name] = (y, h)
            y += h
        bands.append((gname, by, y + 6))
        y += 18
    for i, (gname, by, ey) in enumerate(bands):
        cls = "nd live" if i == 1 else "nd"
        f.parts.insert(0, f'<rect x="{PX}" y="{by}" width="{PW}" '
                       f'height="{ey - by}" rx="6" class="{cls}"/>')
        f.text(PX + 12, by + 16, gname, "grp")
    bottom = y
    # the retry loop, left of the pipeline
    r = rows["validate"]
    b = rows["the body"]
    f.edge([(PX, r[0] + r[1] / 2), (44, r[0] + r[1] / 2),
            (44, b[0] + b[1] / 2), (PX, b[0] + b[1] / 2)])
    f.label(38, (r[0] + b[0]) / 2 + 30, "validation-conflict: retry",
            "middle", rotate=-90)
    # what the steps reach, in the column beside the pipeline
    f.cyl(FX, 380, FW, 96, "tx/",
          ["prep-<seq>.txn-tmp, then",
           "<hex id>.txn; replication-<id>.log",
           "for masters and peers"], "store")
    f.cyl(FX, 560, FW, 90, "heap.dat",
          ["the archived head and its",
           "prev-pointer; the new data;",
           "reaped chains cut"], "store")
    f.box(FX, 690, FW, 108, "slave sessions · forward timer",
          ["the broadcast fills each session's",
           "mailbox while the lock is held; the",
           "hooks drain the forward queue on",
           "this thread, or schedule a timer",
           "(figure 5)"], "live")
    r = rows["%commit: prepare"]
    f.edge([(PX + PW, r[0] + 12), (LANE, r[0] + 12), (LANE, 410),
            (FX, 410)])
    f.label(LANE - 6, r[0] + 6, "write", "end")
    r = rows["finalize"]
    f.edge([(PX + PW, r[0] + 12), (LANE, r[0] + 12), (LANE, 450),
            (FX, 450)])
    f.label(LANE - 6, r[0] + 6, "rename", "end")
    r = rows["apply"]
    f.edge([(PX + PW, r[0] + 14), (FX, r[0] + 14)])
    f.label(LANE, r[0] + 8, "write", "middle")
    r = rows["broadcast"]
    f.edge([(PX + PW, r[0] + 12), (LANE, r[0] + 12), (LANE, 712),
            (FX, 712)])
    f.label(LANE - 6, r[0] + 6, "mailbox", "end")
    r = rows["hooks"]
    f.edge([(PX + PW, r[0] + 12), (LANE, r[0] + 12), (LANE, 780),
            (FX, 780)])
    f.label(LANE - 6, r[0] + 6, "drain", "end")
    # side notes
    f.box(SX, 40, SW, 124, "the retry and the fallback",
          ["each validation-conflict runs the body",
           "again; at *maximum-transaction-attempts*",
           "(8) it runs once more under the TM lock",
           "with no handler: nothing can conflict,",
           "and nothing else can start meanwhile",
           "any other error propagates at once"])
    f.box(SX, 200, SW, 214, "a read at an epoch (with-as-of)",
          ["create-transaction :start-epoch as-of+1,",
           "class as-of-tx, read-only; a pin at the",
           "current epoch holds the reaper floor",
           "each read: the live head if its commit-",
           "epoch < epoch, else the first archived",
           "head older than it, along prev-pointer;",
           "a chain that runs out is absent (revision",
           "0) or reaped: :if-reaped :skip or :error",
           "refused: :no-version-history, :future-",
           "epoch, :read-write-transaction,",
           ":untyped-scan. Never validated; unpinned",
           "on exit"])
    f.box(SX, 450, SW, 94, "recovery at open",
          ["recover-transactions replays the",
           "tx/*.txn left behind, in hex order,",
           "with no locking, before the store is",
           "handed out"])
    f.box(SX, 580, SW, 94, "a slave or a device",
          ["skips the teal band: a replicated",
           "transaction has a no-op lock and no",
           "validation; the device's writer is the",
           "store's only writer"])
    f.text(24, bottom + 28, "The teal band is held under the "
           "transaction-manager lock; everything else runs on the "
           "committing thread without it. A read-only with-transaction "
           "takes the same path:", "t3")
    f.text(24, bottom + 42, "it writes and renames a .txn, takes the lock "
           "and consumes an epoch.", "t3")
    f.h = max(bottom + 60, 820)
    return f.svg()


FIGURES = [("layers", fig_layers), ("system", fig_system),
           ("disk", fig_disk), ("claims", fig_claims),
           ("threads", fig_threads), ("commit", fig_commit)]

if __name__ == "__main__":
    write_figures(FIGURES, os.path.dirname(os.path.abspath(__file__)))
