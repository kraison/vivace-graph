// Frame wiring (GH #270, #271).  All app state -- just the selected
// graph name -- lives here; the panes render from server responses
// and report events back.  Nothing is attached to window (the
// vendored cytoscape UMD global is read only inside explorer.js).

import { createRosterPane } from "./roster.js";
import { createStatsPane } from "./stats.js";
import { createExplorer } from "./explorer.js";
import { createInspector } from "./inspector.js";

const state = { selected: null };

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
  onTypeSelect: (type) => explorer.showTypeSample(type),
});

function selectGraph(name) {
  // Switching graphs empties the canvas + inspector: nodes from
  // another graph must never linger (GH #271).
  if (name !== state.selected) inspector.hide();
  state.selected = name;
  explorer.setGraph(name);
  roster.setSelected(name);
  stats.showGraph(name);
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
      // Closed (or vanished) selected graph: its canvas contents are
      // stale by definition (GH #271).
      explorer.clear();
      inspector.hide();
    }
    roster.setSelected(state.selected);
    stats.showGraph(state.selected);
  },
});

roster.refresh();
