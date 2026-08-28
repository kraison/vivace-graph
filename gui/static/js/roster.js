// Roster pane (GH #270): graph list with open/closed badges, the
// open/close verbs, and the dismissible error strip.  No polling --
// the list re-renders from the server after each verb and on the
// manual Refresh button.  Selection state lives in main.js; this
// module reports events through the callbacks it is given.

import { api } from "./api.js";

export function createRosterPane({ listEl, refreshBtn, errorStrip,
                                   errorText, errorDismiss,
                                   onSelect, onRosterChange }) {
  let graphs = [];
  let selected = null;
  let busy = false;

  function showError(message) {
    errorText.textContent = message;
    errorStrip.hidden = false;
  }

  function clearError() {
    errorStrip.hidden = true;
    errorText.textContent = "";
  }

  errorDismiss.addEventListener("click", clearError);

  function render() {
    refreshBtn.disabled = busy;
    listEl.textContent = "";
    if (graphs.length === 0) {
      const li = document.createElement("li");
      li.className = "roster-empty";
      li.textContent = "No graphs known to this image.";
      listEl.appendChild(li);
      return;
    }
    for (const g of graphs) {
      const li = document.createElement("li");
      li.className = "roster-item" +
        (g.name === selected ? " selected" : "");
      li.dataset.graph = g.name;
      if (g.location) li.title = g.location;

      const block = document.createElement("div");
      block.className = "roster-name-block";
      const name = document.createElement("div");
      name.className = "roster-name";
      name.textContent = g.name;
      block.appendChild(name);
      if (g.location) {
        const loc = document.createElement("div");
        loc.className = "roster-location";
        loc.textContent = g.location;
        block.appendChild(loc);
      }

      const badge = document.createElement("span");
      badge.className = "badge" + (g.open ? " open" : "");
      badge.textContent = g.open ? "open" : "closed";

      const verb = document.createElement("button");
      verb.type = "button";
      verb.textContent = g.open ? "Close" : "Open";
      verb.disabled = busy;
      verb.addEventListener("click", (ev) => {
        ev.stopPropagation();
        runVerb(g.name, g.open ? "close" : "open");
      });

      li.append(block, badge, verb);
      li.addEventListener("click", () => onSelect(g.name));
      listEl.appendChild(li);
    }
  }

  async function refresh() {
    // Visible feedback: an unchanged roster re-renders identically,
    // so without this the button appears to do nothing.
    refreshBtn.disabled = true;
    refreshBtn.textContent = "Refreshing\u2026";
    try {
      const body = await api.graphs();
      graphs = body.graphs || [];
    } catch (err) {
      graphs = [];
      showError(err.message);
    }
    refreshBtn.textContent = "Refresh";
    render();
    onRosterChange(graphs);
  }

  async function runVerb(name, verb) {
    if (busy) return;
    busy = true;
    render();
    clearError();
    try {
      await (verb === "open" ? api.openGraph(name)
                             : api.closeGraph(name));
    } catch (err) {
      // Engine error text verbatim -- e.g. the dirty-store 409 report.
      showError(err.message);
    }
    busy = false;
    await refresh();
  }

  function setSelected(name) {
    selected = name;
    render();
  }

  refreshBtn.addEventListener("click", () => refresh());

  return { refresh, setSelected, showError,
           getGraph: (name) => graphs.find((g) => g.name === name) };
}
