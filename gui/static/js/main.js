// Frame wiring (GH #270, #271, #278, #279).  All app state -- the
// selected graph name and the active main-region tab -- lives here; the
// panes render from server responses and report events back.  Nothing
// is attached to window (the vendored cytoscape and CodeMirror UMD
// globals are read only inside explorer.js and prolog.js).

import { api } from "./api.js";
import { createRosterPane } from "./roster.js";
import { createStatsPane } from "./stats.js";
import { createExplorer } from "./explorer.js";
import { createInspector } from "./inspector.js";
import { createWorkbench } from "./workbench.js";
import { createResults } from "./wb-results.js";
import { createWorkbenchSplitter } from "./wb-splitter.js";

const state = { selected: null, tab: "explorer", surface: "builder" };

const tabs = {
  explorer: { btn: document.getElementById("tab-explorer"),
              view: document.getElementById("explorer-view") },
  query: { btn: document.getElementById("tab-query"),
           view: document.getElementById("workbench-view") },
};

function showTab(name) {
  state.tab = name;
  for (const [key, t] of Object.entries(tabs)) {
    const on = key === name;
    t.view.hidden = !on;
    t.btn.classList.toggle("active", on);
    t.btn.setAttribute("aria-selected", String(on));
  }
  // Cytoscape measured a zero-size container while the tab was
  // hidden; re-measure now that it has one again (GH #278).
  if (name === "explorer") explorer.resize();
}

const explorer = createExplorer({
  hostEl: document.getElementById("cy"),
  placeholderEl: document.getElementById("canvas-placeholder"),
  countsEl: document.getElementById("canvas-counts"),
  noticeEl: document.getElementById("canvas-notice"),
  clearBtn: document.getElementById("canvas-clear"),
  pickPanel: document.getElementById("pick-list"),
  pickTitle: document.getElementById("pick-list-title"),
  pickNotice: document.getElementById("pick-list-notice"),
  pickItems: document.getElementById("pick-list-items"),
  pickClose: document.getElementById("pick-list-close"),
  tooltipEl: document.getElementById("edge-tooltip"),
  onElementSelect: (id) => inspector.show(state.selected, id),
  onElementRemoved: () => inspector.hide(),
  onCleared: () => inspector.hide(),
});

const inspector = createInspector({
  dockEl: document.getElementById("inspector-dock"),
  onExpand: (id) => explorer.expandNode(id),
  onRemove: (id) => explorer.removeElement(id),
});

const stats = createStatsPane({
  bodyEl: document.getElementById("stats-body"),
  onTypeSelect: (type) => {
    showTab("explorer");
    explorer.showTypeSample(type);
  },
});

createWorkbenchSplitter({
  paneEl: document.getElementById("workbench-view"),
  handleEl: document.getElementById("wb-splitter"),
});

// Result-to-canvas handoff (GH #278): the surfaces hand over node ids
// only; the merge itself is the explorer's own entry point.  One
// results table serves both surfaces -- they answer in one envelope.
const results = createResults({
  statusEl: document.getElementById("wb-status"),
  tableEl: document.getElementById("wb-table"),
  sendBtn: document.getElementById("wb-send"),
  onSendToCanvas: (ids) => {
    showTab("explorer");
    explorer.addNodes(ids);
  },
});

// Free-text Prolog (GH #279): created only if the server advertises
// the capability, so a default GUI never even fetches CodeMirror.
let prolog = null;

const surfaces = {
  builder: { btn: document.getElementById("wb-tab-builder"),
             view: document.getElementById("wb-builder") },
  prolog: { btn: document.getElementById("wb-tab-prolog"),
            view: document.getElementById("wb-prolog") },
};

function showSurface(name) {
  state.surface = name;
  for (const [key, s] of Object.entries(surfaces)) {
    const on = key === name;
    s.view.hidden = !on;
    s.btn.classList.toggle("active", on);
    s.btn.setAttribute("aria-selected", String(on));
  }
  // The table below the splitter belongs to whichever surface last ran;
  // switching surfaces without clearing would show one pane's results
  // under the other's input.
  results.clear();
  // CodeMirror measured a zero-size container while hidden.
  if (name === "prolog" && prolog) prolog.refresh();
}

const workbench = createWorkbench({
  matchEl: document.getElementById("wb-match"),
  whereEl: document.getElementById("wb-where"),
  selectEl: document.getElementById("wb-select"),
  addVertexBtn: document.getElementById("wb-add-vertex"),
  addEdgeBtn: document.getElementById("wb-add-edge"),
  addSlotBtn: document.getElementById("wb-add-slot"),
  addCompareBtn: document.getElementById("wb-add-compare"),
  limitEl: document.getElementById("wb-limit"),
  runBtn: document.getElementById("wb-run"),
  errorEl: document.getElementById("wb-error"),
  results,
  // The builder already loads /types and /stats; the editor's overlay
  // needs exactly those names, so it rides along rather than refetching.
  onSchema: (name, names) => {
    if (prolog) prolog.setGraph(name, names);
  },
});

for (const [name, t] of Object.entries(tabs)) {
  t.btn.addEventListener("click", () => showTab(name));
}

for (const [name, s] of Object.entries(surfaces)) {
  s.btn.addEventListener("click", () => showSurface(name));
}

// CodeMirror 5 ships a UMD bundle, not an ES module: classic tags,
// injected only when the capability says the editor exists.
function loadScript(src) {
  return new Promise((resolve, reject) => {
    const el = document.createElement("script");
    el.src = src;
    el.addEventListener("load", () => resolve());
    el.addEventListener("error",
                        () => reject(new Error(`cannot load ${src}`)));
    document.head.appendChild(el);
  });
}

function loadStylesheet(href) {
  const el = document.createElement("link");
  el.rel = "stylesheet";
  el.href = href;
  document.head.appendChild(el);
}

async function enableProlog(capabilities) {
  loadStylesheet("/vendor/codemirror.css");
  await loadScript("/vendor/codemirror.js");
  // overlay.js is an ADDON, not part of lib/codemirror.js: without it
  // CodeMirror.overlayMode is undefined and building the vg-prolog mode
  // throws during editor construction (GH #279).
  await loadScript("/vendor/codemirror-overlay.js");
  await loadScript("/vendor/codemirror-commonlisp.js");
  const { createPrologPane } = await import("./prolog.js");
  prolog = createPrologPane({
    hostEl: document.getElementById("wb-editor"),
    limitEl: document.getElementById("wb-prolog-limit"),
    runBtn: document.getElementById("wb-prolog-run"),
    balanceEl: document.getElementById("wb-balance"),
    errorEl: document.getElementById("wb-prolog-error"),
    results,
    capabilities,
  });
}

function selectGraph(name) {
  // Switching graphs empties the canvas + inspector: nodes from
  // another graph must never linger (GH #271).
  if (name !== state.selected) inspector.hide();
  state.selected = name;
  explorer.setGraph(name);
  roster.setSelected(name);
  stats.showGraph(name);
  // The workbench is schema-bound, so it reloads from scratch for the
  // new graph -- a stale pattern row cannot survive the switch.
  workbench.setGraph(name);
}

const roster = createRosterPane({
  listEl: document.getElementById("roster-list"),
  refreshBtn: document.getElementById("roster-refresh"),
  errorStrip: document.getElementById("error-strip"),
  errorText: document.getElementById("error-strip-text"),
  errorDismiss: document.getElementById("error-strip-dismiss"),
  onSelect: selectGraph,
  onRosterChange: (graphs) => {
    // Selection survives a refresh while its graph is still listed;
    // the stats pane re-renders either way (open state may have
    // changed under it).
    const current = graphs.find((g) => g.name === state.selected);
    if (state.selected && !current) {
      state.selected = null;
    }
    if (!current || !current.open) {
      // Closed (or vanished) selected graph: its canvas contents and
      // its schema-bound builder are stale by definition (#271, #278).
      explorer.clear();
      inspector.hide();
      workbench.clear();
    }
    roster.setSelected(state.selected);
    stats.showGraph(state.selected);
  },
});

// A broken editor must not look like a disabled one.  The sub-tab nav is
// revealed as soon as the SERVER says the capability is on, before
// anything is loaded, and a failure to load leaves the nav visible with
// the Prolog tab disabled and the reason on screen.  Previously the nav
// was revealed on enableProlog's last line, so any throw inside it left
// the tab hidden and indistinguishable from :allow-prolog being off --
// which is exactly how a missing CodeMirror addon went unnoticed
// (GH #279).
function prologUnavailable(reason) {
  const btn = surfaces.prolog.btn;
  btn.disabled = true;
  btn.classList.add("failed");
  btn.textContent = "Prolog (unavailable)";
  btn.title = reason;
  const errorEl = document.getElementById("wb-prolog-error");
  errorEl.textContent = reason;
  errorEl.hidden = false;
  showSurface("builder");
  roster.showError(reason);
}

// Capabilities first, roster second: the Prolog sub-tab must exist
// before a graph selection would populate it (GH #279).
async function boot() {
  let capabilities = null;
  try {
    capabilities = await api.capabilities();
  } catch (err) {
    // A GUI that cannot read its capabilities is still a working GUI:
    // the Prolog sub-tab simply never appears, which is honest -- we do
    // not know whether the server offers it.
    console.warn("capabilities unavailable:", err.message);
  }
  if (capabilities && capabilities.allowProlog && capabilities.prolog) {
    document.getElementById("wb-subtabs").hidden = false;
    try {
      await enableProlog(capabilities.prolog);
    } catch (err) {
      prologUnavailable(
        `The Prolog editor could not load: ${err.message}. The server ` +
        "offers free-text Prolog; this is a client-side fault.");
    }
  }
  roster.refresh();
}

boot();
