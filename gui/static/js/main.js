// Frame wiring (GH #270, #271, #278).  All app state -- the selected
// graph name and the active main-region tab -- lives here; the panes
// render from server responses and report events back.  Nothing is
// attached to window (the vendored cytoscape UMD global is read only
// inside explorer.js).

import { createRosterPane } from "./roster.js";
import { createStatsPane } from "./stats.js";
import { createExplorer } from "./explorer.js";
import { createInspector } from "./inspector.js";
import { createWorkbench } from "./workbench.js";
import { createWorkbenchSplitter } from "./wb-splitter.js";

const state = { selected: null, tab: "explorer" };

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

// Result-to-canvas handoff (GH #278): the workbench hands over node
// ids only; the merge itself is the explorer's own entry point.
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
  sendBtn: document.getElementById("wb-send"),
  errorEl: document.getElementById("wb-error"),
  statusEl: document.getElementById("wb-status"),
  tableEl: document.getElementById("wb-table"),
  onSendToCanvas: (ids) => {
    showTab("explorer");
    explorer.addNodes(ids);
  },
});

for (const [name, t] of Object.entries(tabs)) {
  t.btn.addEventListener("click", () => showTab(name));
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

roster.refresh();
