// Shared query results table (GH #278, extracted in GH #279).
//
// Both query surfaces -- the schema-driven builder and the free-text
// Prolog editor -- answer in the SAME envelope ({columns, rows,
// rowCount, limit, truncated}), so they render through one table and
// one "send to canvas" handoff.  This module owns the results half of
// the query pane; the surfaces above the splitter own only their own
// input.
//
// The handoff never touches the explorer: it reports ids through
// onSendToCanvas and main.js wires that to the canvas.

const NODE_ID_RE = /^[0-9a-f]{32}$/;

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

export function createResults({ statusEl, tableEl, sendBtn,
                                onSendToCanvas }) {
  let lastIds = [];

  function syncSend() {
    sendBtn.hidden = lastIds.length === 0;
    sendBtn.textContent =
      `Send ${lastIds.length} node${lastIds.length === 1 ? "" : "s"} ` +
      "to canvas";
  }

  function clear() {
    statusEl.textContent = "";
    tableEl.textContent = "";
    lastIds = [];
    syncSend();
  }

  function status(text) {
    statusEl.textContent = text;
  }

  function show(body) {
    const columns = body.columns || [];
    const rows = body.rows || [];
    tableEl.textContent = "";
    const ids = [];
    const table = el("table", "wb-table");
    const head = document.createElement("thead");
    const headRow = document.createElement("tr");
    for (const c of columns) headRow.appendChild(el("th", null, c));
    head.appendChild(headRow);
    table.appendChild(head);
    const tbody = document.createElement("tbody");
    for (const row of rows) {
      const tr = document.createElement("tr");
      for (const c of columns) {
        const value = row[c];
        const td = document.createElement("td");
        if (typeof value === "string" && NODE_ID_RE.test(value)) {
          if (!ids.includes(value)) ids.push(value);
          // A node id is a handoff affordance, not just text.
          const b = el("button", "wb-id", value);
          b.type = "button";
          b.title = "Add this node to the canvas";
          b.addEventListener("click", () => onSendToCanvas([value]));
          td.appendChild(b);
        } else {
          td.textContent = value === null || value === undefined
            ? "null"
            : typeof value === "object"
              ? JSON.stringify(value)
              : String(value);
        }
        tr.appendChild(td);
      }
      tbody.appendChild(tr);
    }
    table.appendChild(tbody);
    tableEl.appendChild(table);
    lastIds = ids;
    syncSend();
    statusEl.textContent =
      `${body.rowCount} row${body.rowCount === 1 ? "" : "s"}` +
      (body.truncated
        ? ` — truncated at the server's limit of ${body.limit}`
        : "");
  }

  sendBtn.addEventListener("click", () => {
    if (lastIds.length > 0) onSendToCanvas(lastIds);
  });
  syncSend();

  return { clear, show, status };
}
