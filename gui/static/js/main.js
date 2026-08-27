// Frame wiring (GH #270).  All app state -- just the selected graph
// name -- lives here; the panes render from server responses and
// report events back.  Nothing is attached to window.

import { createRosterPane } from "./roster.js";
import { createStatsPane } from "./stats.js";

const state = { selected: null };

const stats = createStatsPane({
  bodyEl: document.getElementById("stats-body"),
});

const roster = createRosterPane({
  listEl: document.getElementById("roster-list"),
  refreshBtn: document.getElementById("roster-refresh"),
  errorStrip: document.getElementById("error-strip"),
  errorText: document.getElementById("error-strip-text"),
  errorDismiss: document.getElementById("error-strip-dismiss"),
  onSelect: (name) => {
    state.selected = name;
    roster.setSelected(name);
    stats.showGraph(name);
  },
  onRosterChange: (graphs) => {
    // Selection survives a refresh while its graph is still listed;
    // the stats pane re-renders either way (open state may have
    // changed under it).
    if (state.selected &&
        !graphs.some((g) => g.name === state.selected)) {
      state.selected = null;
    }
    roster.setSelected(state.selected);
    stats.showGraph(state.selected);
  },
});

roster.refresh();
