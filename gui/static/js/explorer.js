// Explorer canvas (GH #271): cytoscape mount, type-sample entry ramp,
// Bloom-style additive expansion, view-local remove/clear.  Strictly
// read-only -- no mutation endpoint is ever called.
//
// cytoscape's UMD build is not an ES module; index.html loads it with
// a classic <script> tag and this file reads window.cytoscape -- the
// ONE deliberate exception to the no-window-globals rule (see
// vendor/VENDOR.md).

import { api } from "./api.js";

const SAMPLE_LIMIT = 50;        // /nodes default, made explicit so
const NEIGHBORHOOD_LIMIT = 100; // truncation notices can name it

// Deterministic type -> hue: djb2 over the type name, mod 360.
// Stable across sessions by construction (no randomness, no state).
// Hues moved once when type names went kebab on the wire (GH #277) --
// the mapping is over whatever string the API sends, by design.
export function typeHue(type) {
  let h = 5381;
  for (let i = 0; i < type.length; i += 1) {
    h = ((h * 33) ^ type.charCodeAt(i)) >>> 0;
  }
  return h % 360;
}

const nodeColor = (type) => `hsl(${typeHue(type)}, 55%, 50%)`;
const edgeColor = (type) => `hsl(${typeHue(type)}, 45%, 55%)`;

const CY_STYLE = [
  { selector: "node",
    style: {
      "background-color": "data(color)",
      "label": "data(type)",
      "color": "#8b96a3",
      "font-size": "9px",
      "text-valign": "bottom",
      "text-margin-y": 4,
      "width": 26,
      "height": 26,
      "border-width": 1,
      "border-color": "#303a46",
    } },
  { selector: "node:selected",
    style: {
      "border-width": 3,
      "border-color": "#4fa3d1",
    } },
  { selector: "edge",
    style: {
      "line-color": "data(color)",
      "target-arrow-color": "data(color)",
      "target-arrow-shape": "triangle",
      "curve-style": "bezier",
      "width": 1.5,
      "arrow-scale": 0.9,
    } },
  { selector: "edge:selected",
    style: { "width": 3 } },
];

// Layout choice (GH #271): cose with randomize:false and no
// animation.  Existing nodes keep their positions as the starting
// state and new nodes are pre-placed on a ring around their anchor,
// so a merge nudges the picture instead of scrambling it.
const LAYOUT_OPTS = {
  name: "cose",
  animate: false,
  randomize: false,
  numIter: 400,
  fit: false,
  padding: 40,
};

export function createExplorer({ hostEl, placeholderEl, countsEl,
                                 noticeEl, clearBtn, pickPanel,
                                 pickTitle, pickNotice, pickItems,
                                 pickClose, tooltipEl,
                                 onElementSelect, onElementRemoved,
                                 onCleared }) {
  let graphName = null;
  // Bumped on clear/switch: a late response from the old graph (or a
  // cleared canvas) must never merge into the new one.
  let generation = 0;
  let lastExpand = { id: null, at: 0 };

  const cy = window.cytoscape({
    container: hostEl,
    style: CY_STYLE,
    wheelSensitivity: 0.3,
  });

  function refreshStatus() {
    const n = cy.nodes().length;
    const e = cy.edges().length;
    countsEl.textContent = `${n} node${n === 1 ? "" : "s"} · ` +
      `${e} edge${e === 1 ? "" : "s"}`;
    placeholderEl.hidden = n > 0;
  }

  function notice(text) {
    noticeEl.textContent = text || "";
  }

  function closePickList() {
    pickPanel.hidden = true;
    pickItems.textContent = "";
    pickNotice.textContent = "";
  }

  function clear() {
    generation += 1;
    cy.elements().remove();
    closePickList();
    notice("");
    tooltipEl.hidden = true;
    refreshStatus();
    // The inspector may still show a removed element (GH #271 m-1).
    if (onCleared) onCleared();
  }

  function setGraph(name) {
    if (name !== graphName) {
      graphName = name;
      clear();
    }
  }

  // Merge NODES/EDGES ({id, type, ...} wire briefs) additively --
  // never resets the canvas.  New nodes start on a deterministic ring
  // around ANCHORID's position (or the viewport center), then one
  // cose pass settles the whole picture.
  function merge(nodes, edges, anchorId) {
    const anchor = anchorId && cy.getElementById(anchorId);
    const center = (anchor && anchor.length > 0)
      ? anchor.position()
      : { x: 0, y: 0 };
    const fresh = (nodes || []).filter(
      (n) => cy.getElementById(n.id).length === 0);
    fresh.forEach((n, i) => {
      const angle = (2 * Math.PI * i) / fresh.length;
      cy.add({
        group: "nodes",
        data: { id: n.id, type: n.type, color: nodeColor(n.type) },
        position: { x: center.x + 90 * Math.cos(angle),
                    y: center.y + 90 * Math.sin(angle) },
      });
    });
    for (const e of (edges || [])) {
      if (cy.getElementById(e.id).length === 0 &&
          cy.getElementById(e.from).length > 0 &&
          cy.getElementById(e.to).length > 0) {
        cy.add({
          group: "edges",
          data: { id: e.id, type: e.type, source: e.from,
                  target: e.to, color: edgeColor(e.type) },
        });
      }
    }
    if (cy.nodes().length > 1) cy.layout(LAYOUT_OPTS).run();
    if (cy.nodes().length === fresh.length) cy.fit(undefined, 60);
    refreshStatus();
  }

  function seedNode(brief) {
    merge([brief], [], null);
    cy.center(cy.getElementById(brief.id));
  }

  async function expandNode(id) {
    if (!graphName) return;
    // dblclick and dbltap can both fire for one gesture; one fetch
    // per gesture is enough.
    const now = Date.now();
    if (lastExpand.id === id && now - lastExpand.at < 500) return;
    lastExpand = { id, at: now };
    const gen = generation;
    try {
      const body = await api.neighborhood(graphName, id,
                                          NEIGHBORHOOD_LIMIT);
      if (gen !== generation) return; // canvas cleared/switched
      merge(body.nodes, body.edges, id);
      const shown = (body.edges || []).length;
      notice(body.truncated
             ? `neighborhood of ${id.slice(0, 8)}… truncated — ` +
               `showing ${shown} edge${shown === 1 ? "" : "s"} ` +
               `of more (limit ${NEIGHBORHOOD_LIMIT})`
             : "");
    } catch (err) {
      if (gen !== generation) return;
      notice(err.message);
    }
  }

  // View-local removal: cytoscape drops the node's connected edges
  // with it, so nothing dangles.
  function removeElement(id) {
    const ele = cy.getElementById(id);
    if (ele.length > 0) {
      ele.remove();
      refreshStatus();
      if (onElementRemoved) onElementRemoved(id);
    }
  }

  async function showTypeSample(type) {
    if (!graphName) return;
    const gen = generation;
    pickPanel.hidden = false;
    pickTitle.textContent = `Pick a ${type} node`;
    pickItems.textContent = "";
    pickNotice.textContent = "loading…";
    try {
      const body = await api.nodes(graphName, type, SAMPLE_LIMIT);
      if (gen !== generation || pickPanel.hidden) return;
      pickItems.textContent = "";
      pickNotice.textContent = body.truncated
        ? `showing ${body.nodes.length} of more ` +
          `(limit ${SAMPLE_LIMIT})`
        : `${body.nodes.length} node${
            body.nodes.length === 1 ? "" : "s"}`;
      if (body.nodes.length === 0) {
        pickNotice.textContent = "no nodes of this type";
        return;
      }
      for (const n of body.nodes) {
        const li = document.createElement("li");
        const id = document.createElement("span");
        id.className = "pick-id";
        id.textContent = n.id;
        const ty = document.createElement("span");
        ty.className = "pick-type";
        ty.textContent = n.type;
        li.append(id, ty);
        li.addEventListener("click", () => {
          seedNode(n);
          closePickList();
        });
        pickItems.appendChild(li);
      }
    } catch (err) {
      if (gen !== generation || pickPanel.hidden) return;
      pickItems.textContent = "";
      pickNotice.textContent = err.message;
    }
  }

  pickClose.addEventListener("click", closePickList);
  clearBtn.addEventListener("click", clear);

  // Single click/tap = inspect; double = expand; right-click (cxttap)
  // = view-local remove.
  cy.on("tap", "node, edge", (ev) => {
    if (onElementSelect) onElementSelect(ev.target.id());
  });
  cy.on("dblclick dbltap", "node", (ev) => {
    expandNode(ev.target.id());
  });
  cy.on("cxttap", "node, edge", (ev) => {
    removeElement(ev.target.id());
  });

  // Cheap DOM tooltip: edge type on hover (cytoscape has no titles).
  cy.on("mouseover", "edge", (ev) => {
    const pos = ev.target.midpoint();
    const pan = cy.pan();
    const zoom = cy.zoom();
    tooltipEl.textContent = ev.target.data("type");
    tooltipEl.style.left = `${pos.x * zoom + pan.x + 12}px`;
    tooltipEl.style.top = `${pos.y * zoom + pan.y - 8}px`;
    tooltipEl.hidden = false;
  });
  cy.on("mouseout", "edge", () => {
    tooltipEl.hidden = true;
  });

  refreshStatus();

  return { setGraph, clear, showTypeSample, expandNode,
           removeElement,
           hasElement: (id) => cy.getElementById(id).length > 0 };
}
