// Stats pane (GH #270): totals, per-type counts, views, indexes,
// on-disk size and schema for the selected graph.  Per-type rows
// carry data-type / data-kind attributes -- U3's explorer entry ramp
// hooks them (GH #271).  A closed or unknown graph shows the API's
// error message, never a blank pane.

import { api } from "./api.js";

export function humanBytes(n) {
  if (typeof n !== "number" || !isFinite(n) || n < 0) return "?";
  const units = ["B", "KiB", "MiB", "GiB", "TiB"];
  let i = 0;
  let v = n;
  while (v >= 1024 && i < units.length - 1) {
    v /= 1024;
    i += 1;
  }
  return i === 0 ? `${v} B` : `${v.toFixed(1)} ${units[i]}`;
}

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function heading(text) {
  return el("h3", null, text);
}

function statBox(value, label) {
  const box = el("div", "stat");
  box.appendChild(el("span", "stat-value", String(value)));
  box.appendChild(el("span", "stat-label", label));
  return box;
}

function typeTable(vertexCounts, edgeCounts) {
  const table = el("table", "type-table");
  const body = document.createElement("tbody");
  const addRows = (counts, kind) => {
    for (const [type, count] of Object.entries(counts || {})) {
      const tr = document.createElement("tr");
      tr.dataset.type = type;
      tr.dataset.kind = kind;
      // Explorer entry ramp (GH #271): vertex rows are clickable
      // seeds; edge rows stay inert (no edge sample endpoint).
      if (kind === "vertex") tr.classList.add("type-row-seed");
      const name = el("td");
      name.appendChild(el("span", "kind-tag", kind === "vertex"
                          ? "V" : "E"));
      name.appendChild(document.createTextNode(type));
      tr.appendChild(name);
      tr.appendChild(el("td", "count", String(count)));
      body.appendChild(tr);
    }
  };
  addRows(vertexCounts, "vertex");
  addRows(edgeCounts, "edge");
  table.appendChild(body);
  return table;
}

function listOf(items, renderItem) {
  const ul = el("ul", "stats-list");
  if (!items || items.length === 0) {
    ul.appendChild(el("li", "dim", "none"));
    return ul;
  }
  for (const item of items) ul.appendChild(renderItem(item));
  return ul;
}

function viewItem(v) {
  const li = el("li");
  li.appendChild(document.createTextNode(`${v.name} `));
  li.appendChild(el("span", "dim", `on ${v.class}`));
  return li;
}

function indexItem(ix) {
  const slots = (ix.slots || []).join(", ");
  const li = el("li");
  li.appendChild(document.createTextNode(`${ix.owner}(${slots}) `));
  li.appendChild(el("span", "dim", ix.kind));
  return li;
}

function schemaItem(t) {
  const li = el("li");
  li.appendChild(document.createTextNode(t.name));
  const slots = (t.slots || []).join(", ");
  li.appendChild(el("span", "schema-slots",
                    slots ? ` → ${slots}` : " → (no slots)"));
  return li;
}

export function createStatsPane({ bodyEl, onTypeSelect }) {
  // Monotonic token: a slow response for a graph the user has since
  // navigated away from must not overwrite the newer render.
  let requestToken = 0;

  function showPlaceholder(text) {
    bodyEl.textContent = "";
    bodyEl.appendChild(el("p", "placeholder", text));
  }

  function showErrorMessage(message) {
    bodyEl.textContent = "";
    bodyEl.appendChild(el("p", "stats-error", message));
  }

  function renderStats(stats) {
    bodyEl.textContent = "";
    const totals = el("div", "stats-totals");
    totals.appendChild(statBox(stats.vertexCount, "vertices"));
    totals.appendChild(statBox(stats.edgeCount, "edges"));
    totals.appendChild(statBox(humanBytes(stats.onDiskBytes),
                               "on disk"));
    bodyEl.appendChild(totals);

    bodyEl.appendChild(heading("Counts by type"));
    const table = typeTable(stats.vertexCountsByType,
                            stats.edgeCountsByType);
    // Delegated seam for the explorer (GH #271): the pane reports the
    // clicked vertex type; it never touches the canvas itself.
    if (onTypeSelect) {
      table.addEventListener("click", (ev) => {
        const row = ev.target.closest("tr.type-row-seed");
        if (row) onTypeSelect(row.dataset.type);
      });
    }
    bodyEl.appendChild(table);

    bodyEl.appendChild(heading("Views"));
    bodyEl.appendChild(listOf(stats.views, viewItem));

    bodyEl.appendChild(heading("Indexes"));
    bodyEl.appendChild(listOf(stats.indexes, indexItem));

    bodyEl.appendChild(heading("Schema"));
    const schema = stats.schema || {};
    const types = [...(schema.vertexTypes || []),
                   ...(schema.edgeTypes || [])];
    bodyEl.appendChild(listOf(types, schemaItem));
  }

  async function showGraph(name) {
    const token = ++requestToken;
    if (!name) {
      showPlaceholder("Select a graph.");
      return;
    }
    try {
      const stats = await api.stats(name);
      if (token !== requestToken) return; // stale response, drop it
      renderStats(stats);
    } catch (err) {
      if (token !== requestToken) return;
      // 404 "Graph X is not open", etc. -- the API's message, as-is.
      showErrorMessage(err.message);
    }
  }

  return {
    showGraph,
    clear: () => {
      requestToken += 1; // invalidate any in-flight fetch
      showPlaceholder("Select a graph.");
    },
  };
}
