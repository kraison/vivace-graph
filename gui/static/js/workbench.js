// Query workbench (GH #278): a schema-driven match/where/select
// builder over the structured DSL in query-dsl.lisp, a results table,
// and the row-ids -> canvas handoff.
//
// The builder GENERATES every query variable (?v1, ?b1, ...) and the
// operator picks them from dropdowns, never types them; type and slot
// names come only from /types and /stats, so the pane cannot express a
// query this graph's schema does not support.  Free text appears in
// exactly one place, a literal slot or comparison value.
//
// That literal box has one sharp edge, guarded rather than hidden: the
// DSL's compare arm runs its args through %DSL-VAR-OR-LITERAL, which
// INTERNS a leading "?" as a query variable (the slot "value" arm does
// not -- it passes the datum raw).  A typed "?b1" would therefore turn
// a filter into a vacuous match-all.  So a "?"-leading literal is
// refused here, and comparing two bound variables -- which the DSL
// does support -- is an explicit second mode instead of an accident.
//
// The results table and the canvas handoff moved to wb-results.js in
// GH #279: the free-text Prolog surface answers in the same envelope
// and renders through the same table.

import { api } from "./api.js";

const NUMBER_RE = /^-?\d+(\.\d+)?$/;

// Exactly *DSL-COMPARE-OPS* (query-dsl.lisp); nothing else compiles.
const COMPARE_OPS = ["<", "<=", ">", ">=", "=", "==", "/="];

const RIGHT_MODES = [{ value: "literal", text: "literal" },
                     { value: "variable", text: "variable" }];

const DEFAULT_LIMIT = 50;

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function option(value, text) {
  const o = document.createElement("option");
  o.value = value;
  o.textContent = text === undefined ? value : text;
  return o;
}

function select(values, current, onChange, placeholder) {
  const s = document.createElement("select");
  if (placeholder !== undefined) s.appendChild(option("", placeholder));
  for (const v of values) {
    s.appendChild(typeof v === "string" ? option(v) : option(v.value,
                                                             v.text));
  }
  s.value = current === undefined || current === null ? "" : current;
  s.addEventListener("change", () => onChange(s.value));
  return s;
}

// A free-text value stays a literal: a number when it reads as one,
// otherwise the string.  Nothing here is ever read as a symbol.
function literal(text) {
  const t = (text || "").trim();
  return NUMBER_RE.test(t) ? Number(t) : t;
}

export function createWorkbench({ matchEl, whereEl, selectEl,
                                  addVertexBtn, addEdgeBtn, addSlotBtn,
                                  addCompareBtn, limitEl, runBtn,
                                  errorEl, results, onSchema }) {
  let graphName = null;
  let schema = null;
  let patterns = [];            // vertex/edge match rows
  let constraints = [];         // where rows
  let chosen = [];              // selected variable names, in order
  let nextVertexVar = 1;
  let nextBindVar = 1;
  // Same stale-token rule as stats.js: a slow response for a graph or
  // a query the operator has moved on from must not win.
  let requestToken = 0;

  function showError(message) {
    errorEl.textContent = message || "";
    errorEl.hidden = !message;
  }

  function vertexRows() {
    return patterns.filter((p) => p.kind === "vertex");
  }

  function bindVars() {
    return constraints
      .filter((c) => c.kind === "slot" && c.mode === "bind")
      .map((c) => c.bind);
  }

  function allVars() {
    return [...vertexRows().map((p) => p.varName), ...bindVars()];
  }

  function typeOfVar(name) {
    const row = vertexRows().find((p) => p.varName === name);
    return row ? row.type : null;
  }

  function slotsFor(varName) {
    const type = typeOfVar(varName);
    return type && schema ? schema.slotsOf(type) : [];
  }

  // Dropping a match row invalidates whatever referenced its variable;
  // prune rather than ship a query with a dangling variable.
  function prune() {
    const live = new Set(vertexRows().map((p) => p.varName));
    patterns = patterns.filter(
      (p) => p.kind === "vertex" ||
             (live.has(p.from) && live.has(p.to)));
    constraints = constraints.filter(
      (c) => c.kind !== "slot" || live.has(c.varName));
    const bound = new Set(allVars());
    constraints = constraints.filter(
      (c) => c.kind !== "compare" || bound.has(c.left));
    // A retyped variable's old slot is not in the new type's schema,
    // and a compare row's right-hand variable may have just gone away.
    for (const c of constraints) {
      if (c.kind === "slot" && c.slot &&
          !slotsFor(c.varName).includes(c.slot)) {
        c.slot = "";
      }
      if (c.kind === "compare" && c.right && !bound.has(c.right)) {
        c.right = "";
      }
    }
    chosen = chosen.filter((v) => bound.has(v));
  }

  function removeBtn(onClick) {
    const b = el("button", "wb-remove", "×");
    b.type = "button";
    b.setAttribute("aria-label", "Remove row");
    b.addEventListener("click", onClick);
    return b;
  }

  function renderMatch() {
    matchEl.textContent = "";
    if (patterns.length === 0) {
      matchEl.appendChild(el("li", "dim", "no patterns yet"));
      return;
    }
    patterns.forEach((p, i) => {
      const li = el("li", "wb-row");
      if (p.kind === "vertex") {
        li.appendChild(select(schema.vertexTypes, p.type, (v) => {
          p.type = v;
          prune();
          render();
        }));
        li.appendChild(el("span", "wb-var", `as ${p.varName}`));
      } else {
        li.appendChild(select(schema.edgeTypes, p.type, (v) => {
          p.type = v;
          render();
        }));
        li.appendChild(el("span", "dim", "from"));
        li.appendChild(select(vertexRows().map((r) => r.varName),
                              p.from, (v) => { p.from = v; render(); }));
        li.appendChild(el("span", "dim", "to"));
        li.appendChild(select(vertexRows().map((r) => r.varName),
                              p.to, (v) => { p.to = v; render(); }));
      }
      li.appendChild(removeBtn(() => {
        patterns.splice(i, 1);
        prune();
        render();
      }));
      matchEl.appendChild(li);
    });
  }

  function renderSlotRow(c, li) {
    li.appendChild(select(vertexRows().map((r) => r.varName), c.varName,
                          (v) => {
                            c.varName = v;
                            c.slot = "";
                            render();
                          }));
    li.appendChild(el("span", "dim", "."));
    li.appendChild(select(slotsFor(c.varName), c.slot, (v) => {
      c.slot = v;
      render();
    }, "slot…"));
    li.appendChild(select([{ value: "bind", text: "binds" },
                           { value: "value", text: "equals" }],
                          c.mode, (v) => {
                            c.mode = v;
                            if (v === "bind" && !c.bind) {
                              c.bind = `?b${nextBindVar++}`;
                              chosen.push(c.bind);
                            }
                            prune();
                            render();
                          }));
    if (c.mode === "bind") {
      li.appendChild(el("span", "wb-var", c.bind));
    } else {
      const input = el("input", "wb-literal");
      input.type = "text";
      input.value = c.value || "";
      input.placeholder = "value";
      input.addEventListener("input", () => { c.value = input.value; });
      li.appendChild(input);
    }
  }

  function renderCompareRow(c, li) {
    li.appendChild(select(bindVars(), c.left, (v) => {
      c.left = v;
      render();
    }, "variable…"));
    li.appendChild(select(COMPARE_OPS, c.op, (v) => {
      c.op = v;
      render();
    }));
    li.appendChild(select(RIGHT_MODES, c.rightMode, (v) => {
      c.rightMode = v;
      render();
    }));
    if (c.rightMode === "variable") {
      li.appendChild(select(allVars(), c.right, (v) => {
        c.right = v;
        render();
      }, "variable…"));
      return;
    }
    const input = el("input", "wb-literal");
    input.type = "text";
    input.value = c.value || "";
    input.placeholder = "value";
    input.addEventListener("input", () => { c.value = input.value; });
    li.appendChild(input);
  }

  function renderWhere() {
    whereEl.textContent = "";
    if (constraints.length === 0) {
      whereEl.appendChild(el("li", "dim", "no constraints"));
      return;
    }
    constraints.forEach((c, i) => {
      const li = el("li", "wb-row");
      if (c.kind === "slot") renderSlotRow(c, li);
      else renderCompareRow(c, li);
      li.appendChild(removeBtn(() => {
        constraints.splice(i, 1);
        prune();
        render();
      }));
      whereEl.appendChild(li);
    });
  }

  function renderSelect() {
    selectEl.textContent = "";
    const vars = allVars();
    if (vars.length === 0) {
      selectEl.appendChild(el("span", "dim", "no bound variables"));
      return;
    }
    for (const v of vars) {
      const label = el("label", "wb-check");
      const box = document.createElement("input");
      box.type = "checkbox";
      box.checked = chosen.includes(v);
      box.addEventListener("change", () => {
        chosen = box.checked
          ? [...chosen, v]
          : chosen.filter((x) => x !== v);
      });
      label.appendChild(box);
      const type = typeOfVar(v);
      label.appendChild(document.createTextNode(
        type ? ` ${v} (${type})` : ` ${v}`));
      selectEl.appendChild(label);
    }
  }

  function renderControls() {
    const ready = Boolean(graphName && schema);
    for (const b of [addVertexBtn, addSlotBtn, addCompareBtn, runBtn]) {
      b.disabled = !ready;
    }
    addEdgeBtn.disabled = !ready || vertexRows().length === 0;
    limitEl.disabled = !ready;
  }

  function render() {
    if (!schema) {
      matchEl.textContent = "";
      whereEl.textContent = "";
      selectEl.textContent = "";
      matchEl.appendChild(el("li", "dim", graphName
                             ? "schema unavailable"
                             : "select an open graph"));
      renderControls();
      return;
    }
    renderMatch();
    renderWhere();
    renderSelect();
    renderControls();
  }

  function clearResults() {
    results.clear();
    showError("");
  }

  function buildDsl() {
    const match = patterns.map((p) =>
      p.kind === "vertex"
        ? { vertex: p.varName, type: p.type }
        : { edge: p.type, from: p.from, to: p.to });
    const where = constraints.map((c) =>
      c.kind === "slot"
        ? (c.mode === "bind"
            ? { slot: c.varName, name: c.slot, bind: c.bind }
            : { slot: c.varName, name: c.slot,
                value: literal(c.value) })
        : { compare: c.op,
            args: [c.left, c.rightMode === "variable"
                             ? c.right
                             : literal(c.value)] });
    const limit = parseInt(limitEl.value, 10);
    return {
      match,
      where,
      select: chosen,
      limit: Number.isInteger(limit) && limit > 0 ? limit : DEFAULT_LIMIT,
    };
  }

  async function run() {
    if (!graphName || !schema) return;
    if (patterns.length === 0) {
      showError("Add at least one match pattern.");
      return;
    }
    if (chosen.length === 0) {
      showError("Select at least one variable to return.");
      return;
    }
    if (constraints.some((c) => c.kind === "slot" && !c.slot)) {
      showError("Every slot constraint needs a slot.");
      return;
    }
    if (constraints.some((c) => c.kind === "compare" &&
                                c.rightMode === "variable" &&
                                !c.right)) {
      showError("Every variable comparison needs a right-hand " +
                "variable.");
      return;
    }
    // A "?"-leading literal would reach the DSL's compare arm and be
    // interned as a variable, silently making the row match-all.
    const sigil = constraints.find(
      (c) => c.kind === "compare" && c.rightMode !== "variable" &&
             String(c.value).trim().startsWith("?"));
    if (sigil) {
      showError("A comparison literal cannot start with \"?\" — that " +
                "is how the DSL spells a variable, and it would make " +
                "the row match everything. Switch the row to " +
                "\"variable\" to compare two bound variables.");
      return;
    }
    const token = ++requestToken;
    clearResults();              // drops a stale "send to canvas"
    results.status("running…");
    runBtn.disabled = true;
    try {
      const body = await api.query(graphName, buildDsl());
      if (token !== requestToken) return; // stale response, drop it
      results.show(body);
    } catch (err) {
      if (token !== requestToken) return;
      // The server's message, verbatim -- it names the offending
      // pattern, type or bound.
      results.status("");
      showError(err.message);
    }
    renderControls();
  }

  function buildSchema(types, stats) {
    const slots = new Map();
    const names = new Set();
    const s = (stats && stats.schema) || {};
    for (const t of [...(s.vertexTypes || []),
                     ...(s.edgeTypes || [])]) {
      slots.set(t.name, t.slots || []);
      names.add(t.name);
      for (const slot of t.slots || []) names.add(slot);
    }
    for (const t of [...(types.vertexTypes || []),
                     ...(types.edgeTypes || [])]) {
      names.add(t);
    }
    return {
      vertexTypes: types.vertexTypes || [],
      edgeTypes: types.edgeTypes || [],
      slotsOf: (type) => slots.get(type) || [],
      // Every schema name a query may spell -- the Prolog editor dims
      // heads that are neither these nor a registered functor.
      names: [...names],
    };
  }

  async function setGraph(name) {
    const token = ++requestToken;
    graphName = name;
    patterns = [];
    constraints = [];
    chosen = [];
    nextVertexVar = 1;
    nextBindVar = 1;
    clearResults();
    schema = null;
    onSchema(null, []);
    render();
    if (!name) return;
    try {
      // Both endpoints, as the schema-driven contract intends: /types
      // is the inventory, /stats carries each type's slots.
      const [types, stats] = await Promise.all([api.types(name),
                                                api.stats(name)]);
      if (token !== requestToken) return;
      schema = buildSchema(types, stats);
      onSchema(name, schema.names);
    } catch (err) {
      if (token !== requestToken) return;
      schema = null;
      showError(err.message);
    }
    render();
  }

  addVertexBtn.addEventListener("click", () => {
    if (!schema) return;
    if (schema.vertexTypes.length === 0) {
      showError("This graph defines no vertex types.");
      return;
    }
    const varName = `?v${nextVertexVar++}`;
    patterns.push({ kind: "vertex", type: schema.vertexTypes[0],
                    varName });
    chosen.push(varName);
    render();
  });

  addEdgeBtn.addEventListener("click", () => {
    const vars = vertexRows().map((r) => r.varName);
    if (!schema || vars.length === 0) return;
    if (schema.edgeTypes.length === 0) {
      showError("This graph defines no edge types.");
      return;
    }
    patterns.push({ kind: "edge", type: schema.edgeTypes[0],
                    from: vars[0], to: vars[vars.length - 1] });
    render();
  });

  addSlotBtn.addEventListener("click", () => {
    const vars = vertexRows().map((r) => r.varName);
    if (vars.length === 0) {
      showError("Add a vertex pattern first — a slot constraint " +
                "needs a variable to constrain.");
      return;
    }
    const bind = `?b${nextBindVar++}`;
    constraints.push({ kind: "slot", varName: vars[0], slot: "",
                       mode: "bind", bind, value: "" });
    chosen.push(bind);
    render();
  });

  addCompareBtn.addEventListener("click", () => {
    const binds = bindVars();
    if (binds.length === 0) {
      showError("Add a slot constraint that binds a variable first " +
                "— a comparison needs one.");
      return;
    }
    constraints.push({ kind: "compare", left: binds[0], op: "<",
                       rightMode: "literal", value: "", right: "" });
    render();
  });

  runBtn.addEventListener("click", () => run());

  render();

  return {
    setGraph,
    clear: () => setGraph(null),
  };
}
