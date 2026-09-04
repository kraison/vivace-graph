# VG GUI v1 — design (GH #106)

Date 2026-08-27. Approved section-by-section in session; this file is
the record. Epic tracking: #106 (tracker), board "VG GUI".

## Purpose and audience

A web-based cockpit for VivaceGraph, in-tree. Day-1 audience is the
operator/developer working against real stores (usefulness on live
data over polish). Long-term: the same GUI is the cockpit for a
compiled, source-free distributable VG binary — a standing constraint,
not a v1 deliverable.

## v1 scope

- **Management/stats pane** (the frame): known-store roster,
  open/close, per-graph stats.
- **Explorer** (the daily-use magnet): Bloom-style click-to-expand
  neighborhood exploration with a node inspector. Read-only.
- **Fast follow, not v1**: the query workbench (Prolog `select` with
  tabular results and result-to-canvas handoff). The guard pipeline it
  calls lives in `graph-db/query` since #322 (`docs/guarded-query.md`).
- **Later versions, explicitly out**: uploads/exports, node editing,
  graph creation, free-form path opening, auth, search-by-slot,
  saved layouts/perspectives, style editors, multi-graph canvases,
  native (non-web) shell.

## Decisions (with rationale)

1. **In-tree**, new subsystem — ships with the engine like the
   Neo4j-Desktop analogue it is; inherits the repo's domain-neutral
   discipline (#197) for any demo content.
2. **Approach B — own subsystem `graph-db/gui`**, not an extension of
   `rest.lisp` (which stays untouched) and not a strict consumer of
   the public REST API. Rationale: the cockpit is an operational,
   private surface (registry internals, image-management verbs) on a
   different evolution clock than the public data API, and the
   explorer needs batch endpoints the REST routes don't and shouldn't
   have. Optional, separable system — matters for the embeddable core
   and the binary future.
3. **Frontend posture: no-build, vendored.** Plain HTML/CSS/ES-module
   JS served by the Lisp image; the one vendored library is a single
   committed `cytoscape.min.js` (MIT) with a `VENDOR.md` recording
   version/source/license. No npm, no bundler, ever, in dev or
   release. If the frame someday outgrows this, migrating it later is
   cheap; the reverse is not.
4. **Writes**: explorer pane strictly read-only; management pane may
   open/close graphs (that IS the cockpit's job). Node-level editing
   deferred — the query workbench supersedes it.
5. **Roster source**: derived from what the system already knows —
   store registry names + clock-journal `:attach` locations, deduped;
   graceful fallback to `*graphs*` when no system directory is bound.
   No config file, no free-form paths, no creation in v1.
6. **Exposure**: binds 127.0.0.1 by default; localhost is the v1
   security boundary. Auth is a named later-version item.

## Backend

Layout: `gui/package.lisp`, `gui/server.lisp` (lifecycle + static),
`gui/api.lisp` (endpoints), `gui/static/` (frontend). Defsystem
`graph-db/gui`, `:depends-on (:graph-db :ningle :clack :cl-json ...)`
— all already in the full system's dependency set. Static assets
resolve via `asdf:system-relative-pathname` (the binary future is a
serving-strategy swap, not a layout change).

Endpoints (JSON; node ids as the engine's standard hex strings):

- `GET /api/graphs` — roster: name, location, open/closed. Cheap; no
  per-store stats.
- `POST /api/graphs/:name/open` `.../close` — management verbs;
  open strictly at the roster's recorded location. Dirty stores
  surface `store-not-closed-cleanly-error`'s report verbatim (409).
- `GET /api/graphs/:name/stats` — vertex/edge totals (lhash counts),
  per-type counts, view + index inventories, on-disk size, schema
  summary.
- `GET .../types`, `GET .../nodes?type=X&limit=N` — explorer entry
  ramp: type inventory, bounded node sample.
- `GET .../node/:id` — inspection: type, slots (node data is an
  ALIST; serialize as a JSON object), in/out edge counts.
- `GET .../neighborhood/:id?limit=N` — the hot path: one round trip,
  viz-shaped `{nodes:[...], edges:[...]}`, both directions,
  type-labeled, capped with a `truncated` flag, under
  `with-read-snapshot` for internal consistency.

Reads call engine internals directly (`lookup-vertex`,
`outgoing-edges`/`incoming-edges`, type index/registry) — no proxying
through REST.

## Frontend

`gui/static/`: `index.html` (single page; left sidebar roster+stats,
main canvas, right-docked inspector), `css/gui.css`, `js/{api,roster,
stats,explorer,inspector}.js`, `vendor/cytoscape.min.js` + VENDOR.md.

Interaction: roster select → stats pane (totals, per-type table,
views/indexes). Per-type table → bounded node sample → click seeds
the canvas. Canvas: single-click = inspector (type, slots, in/out
counts); double-click/expand = neighborhood batch MERGED additively
(Bloom-style); deterministic type→hue coloring; direction arrows,
type on hover; element count + server `truncated` flags surfaced;
remove-from-canvas and clear are view-local only.

## Lifecycle, errors, testing

- `start-gui (&key (port 4270) (bind "127.0.0.1"))` / `stop-gui`,
  idempotent, mirroring `start-rest`. The GUI holds NO graph state —
  every request resolves by name at request time (closed mid-session
  → clean JSON error, never a stale handle). The two management verbs
  serialize through one lock (clean 409 over a surfaced race).
- Every handler: `handler-case` → `{error, message}` with honest
  codes (404 unknown graph/node/type, 409 dirty/conflict, 400
  malformed id, 500 with the condition's report). No backtrace to the
  browser; details to log4cl.
- Tests: `graph-db/gui-test` (FiveAM + drakma) drives the real HTTP
  surface on an ephemeral port with scratch graphs: roster incl.
  no-system-dir fallback, open/close incl. dirty 409, stats vs known
  fixtures, neighborhood shape/caps/truncated, alist→JSON fidelity,
  the error contract. Frontend JS has NO automated harness in v1 —
  the stated price of the no-toolchain posture; coverage is the API
  tests plus daily use.
- Docs travel per unit: manual chapter, CLAUDE.md pointer, CHANGELOG.

## Decomposition (the epic's units)

- **U1** backend: system, lifecycle, static serving, all endpoints,
  gui-test suite.
- **U2** frame frontend: roster + stats panes, open/close UX.
- **U3** explorer frontend: canvas, expansion, inspector.
- **Follow-up (own issue, not v1)**: query workbench.

Execution per unit follows the repo's established method: implementer
→ independent review → fix rounds → controller fresh-image gate →
diff review → push on approval. SBCL only (ECL demoted per standing
directive).
