// Inspector dock (GH #271): fetches /node/:id on demand and renders
// a vertex (type, id, slots, in/out counts, Expand/Remove) or an edge
// (type, id, from/to, slots).  Read-only; Remove is view-local.

import { api } from "./api.js";

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function slotsTable(slots) {
  const table = el("table", "slots-table");
  const body = document.createElement("tbody");
  const entries = Object.entries(slots || {});
  if (entries.length === 0) {
    const tr = document.createElement("tr");
    tr.appendChild(el("td", "dim", "no slots"));
    body.appendChild(tr);
  }
  for (const [name, value] of entries) {
    const tr = document.createElement("tr");
    tr.appendChild(el("td", "slot-name", name));
    // Values arrive already JSON-decoded; render compactly.
    tr.appendChild(el("td", "slot-value",
                      value === null ? "null"
                      : typeof value === "object"
                        ? JSON.stringify(value)
                        : String(value)));
    body.appendChild(tr);
  }
  table.appendChild(body);
  return table;
}

function idLine(label, id) {
  const p = el("p", "inspector-id");
  p.appendChild(el("span", "dim", `${label} `));
  p.appendChild(el("code", null, id));
  return p;
}

export function createInspector({ dockEl, onExpand, onRemove }) {
  // Same stale-token pattern as stats.js: a slow response for an
  // element the user has since clicked away from must not win.
  let requestToken = 0;

  function hide() {
    requestToken += 1;
    dockEl.hidden = true;
    dockEl.textContent = "";
  }

  function header(title) {
    const head = el("div", "inspector-header");
    head.appendChild(el("h2", null, title));
    const dismiss = el("button", null, "×");
    dismiss.type = "button";
    dismiss.setAttribute("aria-label", "Dismiss inspector");
    dismiss.addEventListener("click", hide);
    head.appendChild(dismiss);
    return head;
  }

  function renderVertex(node) {
    dockEl.textContent = "";
    dockEl.appendChild(header(node.type));
    dockEl.appendChild(idLine("id", node.id));
    dockEl.appendChild(el("p", "inspector-degrees",
                          `in ${node.inEdgeCount} · ` +
                          `out ${node.outEdgeCount}`));
    dockEl.appendChild(el("h3", null, "Slots"));
    dockEl.appendChild(slotsTable(node.slots));
    const actions = el("div", "inspector-actions");
    const expand = el("button", null, "Expand");
    expand.type = "button";
    expand.addEventListener("click", () => onExpand(node.id));
    const remove = el("button", null, "Remove");
    remove.type = "button";
    remove.title = "Remove from canvas (view only)";
    remove.addEventListener("click", () => {
      onRemove(node.id);
      hide();
    });
    actions.append(expand, remove);
    dockEl.appendChild(actions);
  }

  function renderEdge(edge) {
    dockEl.textContent = "";
    dockEl.appendChild(header(edge.type));
    dockEl.appendChild(idLine("id", edge.id));
    dockEl.appendChild(idLine("from", edge.from));
    dockEl.appendChild(idLine("to", edge.to));
    dockEl.appendChild(el("h3", null, "Slots"));
    dockEl.appendChild(slotsTable(edge.slots));
  }

  function renderError(message) {
    dockEl.textContent = "";
    dockEl.appendChild(header("Inspector"));
    dockEl.appendChild(el("p", "inspector-error", message));
  }

  async function show(graphName, id) {
    if (!graphName) return;
    const token = ++requestToken;
    dockEl.hidden = false;
    try {
      const body = await api.node(graphName, id);
      if (token !== requestToken) return; // stale response, drop it
      // An edge body carries from/to; a vertex carries edge counts.
      if (body.from !== undefined) renderEdge(body);
      else renderVertex(body);
    } catch (err) {
      if (token !== requestToken) return;
      renderError(err.message);
    }
  }

  return { show, hide };
}
