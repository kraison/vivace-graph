// Free-text Prolog editor (GH #279) -- the accessory to gui/prolog.lisp's
// read guard, not a second guard.  Nothing here decides what is safe:
// the server refuses free text outright unless it was started with
// :ALLOW-PROLOG, and re-derives its whitelist per request.  What this
// pane does is make a refusal predictable BEFORE the round trip -- an
// unknown head is dimmed as you type, and the paren depth is on screen
// next to Run.
//
// CodeMirror 5's UMD build is not an ES module, so main.js loads it with
// classic script tags and this module reads window.CodeMirror -- the
// same deliberate exception cytoscape gets (see vendor/VENDOR.md).

import { api } from "./api.js";

const DEFAULT_LIMIT = 50;

// Paren depth, ignoring parens inside strings, |names| and ; comments.
// The server's own screen (%SCAN-QUERY-TEXT) reads the text the same
// way, so what this reports is what that will decide.
export function parenBalance(text) {
  let depth = 0;
  let extra = 0;
  let i = 0;
  while (i < text.length) {
    const ch = text[i];
    if (ch === "\\") {
      i += 2;
    } else if (ch === ";") {
      const nl = text.indexOf("\n", i);
      i = nl === -1 ? text.length : nl;
    } else if (ch === '"' || ch === "|") {
      const close = ch;
      i += 1;
      while (i < text.length && text[i] !== close) {
        i += text[i] === "\\" ? 2 : 1;
      }
      i += 1;
    } else if (ch === "(") {
      depth += 1;
      i += 1;
    } else if (ch === ")") {
      if (depth === 0) extra += 1;
      else depth -= 1;
      i += 1;
    } else {
      i += 1;
    }
  }
  return { depth, extra };
}

// Overlay tokens: ?variables get their own class, and a head symbol the
// server would not recognise is dimmed.  The overlay tracks only "is
// this token in head position", which is all it needs -- whitespace
// after "(" does not end head position, a token or a ")" does.
export function makeOverlay(known) {
  return {
    startState: () => ({ head: false }),
    token(stream, state) {
      const ch = stream.peek();
      if (ch === "(") {
        stream.next();
        state.head = true;
        return null;
      }
      if (ch === ")") {
        stream.next();
        state.head = false;
        return null;
      }
      if (ch === ";") {
        stream.skipToEnd();
        return null;
      }
      if (ch === '"' || ch === "|") {
        stream.next();
        while (!stream.eol()) {
          const c = stream.next();
          if (c === "\\") stream.next();
          else if (c === ch) break;
        }
        state.head = false;
        return null;
      }
      if (/\s/.test(ch)) {
        // Whitespace does not leave head position: "( is-a" still has
        // is-a as the head.
        stream.next();
        return null;
      }
      stream.eatWhile(/[^\s()"';]/);
      const word = stream.current();
      const wasHead = state.head;
      state.head = false;
      if (word === "") {
        // Defensive: an overlay token must always consume something.
        stream.next();
        return null;
      }
      if (word.startsWith("?")) return "vg-var";
      if (wasHead && !known.has(word.toLowerCase())) return "vg-unknown";
      return null;
    },
  };
}

export function createPrologPane({ hostEl, limitEl, runBtn, balanceEl,
                                  errorEl, results, capabilities }) {
  const CM = window.CodeMirror;
  let graphName = null;
  let ready = false;
  // Functor names are server-wide (/api/capabilities); type and slot
  // names are per graph and come from the schema the builder already
  // loads.  Both feed one lowercase set: the overlay only needs to know
  // whether a head is nameable, not what kind of name it is.
  const known = new Set(
    (capabilities.functors || []).map((f) => f.toLowerCase()));
  const functorNames = new Set(known);
  let requestToken = 0;

  if (!CM.modes["vg-prolog"]) {
    CM.defineMode("vg-prolog", (config) =>
      CM.overlayMode(CM.getMode(config, "text/x-common-lisp"),
                     makeOverlay(known)));
  }

  const cm = CM(hostEl, {
    value: "",
    mode: "vg-prolog",
    lineNumbers: true,
    lineWrapping: true,
    viewportMargin: Infinity,
  });

  function showError(message) {
    errorEl.textContent = message || "";
    errorEl.hidden = !message;
  }

  // Two readings at once: the nesting depth AT THE CURSOR (where am I?)
  // and whether the document as a whole balances (will the server read
  // this at all?).
  function renderBalance() {
    const whole = parenBalance(cm.getValue());
    const here = parenBalance(
      cm.getRange({ line: 0, ch: 0 }, cm.getCursor()));
    const bad = whole.extra > 0 || whole.depth > 0;
    balanceEl.classList.toggle("unbalanced", bad);
    balanceEl.textContent = whole.extra > 0
      ? `⚠ ${whole.extra} stray )`
      : whole.depth > 0
        ? `⚠ ${whole.depth} unclosed (`
        : `depth ${here.depth}`;
    balanceEl.title = bad
      ? "Unbalanced: the server refuses this before it reads it."
      : "Parenthesis depth at the cursor.";
  }

  function syncControls() {
    ready = Boolean(graphName);
    runBtn.disabled = !ready;
    limitEl.disabled = !ready;
  }

  async function run() {
    if (!graphName) return;
    const text = cm.getValue().trim();
    if (!text) {
      showError("Type at least one goal.");
      return;
    }
    const { depth, extra } = parenBalance(text);
    if (depth > 0 || extra > 0) {
      showError("Unbalanced parentheses — the server refuses a query " +
                "it cannot read.");
      return;
    }
    const token = ++requestToken;
    showError("");
    results.clear();
    results.status("running…");
    runBtn.disabled = true;
    const limit = parseInt(limitEl.value, 10);
    try {
      const body = await api.prolog(graphName, text,
                                    Number.isInteger(limit) && limit > 0
                                      ? limit
                                      : DEFAULT_LIMIT);
      if (token !== requestToken) return;   // stale response, drop it
      results.show(body);
    } catch (err) {
      if (token !== requestToken) return;
      results.status("");
      // The server's message verbatim: it names the offending symbol.
      showError(err.message);
    }
    syncControls();
  }

  // The schema half of the dim set. Replacing it wholesale (rather than
  // adding) keeps a previous graph's type names from staying "known".
  function setSchemaNames(names) {
    known.clear();
    for (const f of functorNames) known.add(f);
    for (const n of names) known.add(String(n).toLowerCase());
    // The overlay reads `known` on every token; force a re-tokenize so
    // the change is visible without an edit.
    cm.setOption("mode", "vg-prolog");
  }

  function setGraph(name, schemaNames) {
    graphName = name || null;
    setSchemaNames(name ? (schemaNames || []) : []);
    showError("");
    syncControls();
  }

  cm.on("change", renderBalance);
  cm.on("cursorActivity", renderBalance);
  runBtn.addEventListener("click", () => run());
  // Ctrl/Cmd-Enter runs, the one shortcut worth having in an editor
  // whose Enter must stay a newline.
  cm.setOption("extraKeys", {
    "Ctrl-Enter": () => run(),
    "Cmd-Enter": () => run(),
  });

  renderBalance();
  syncControls();

  return {
    setGraph,
    clear: () => setGraph(null, []),
    refresh: () => cm.refresh(),
  };
}
