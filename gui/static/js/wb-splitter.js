// Draggable builder/results divider for the query pane (GH #278).
// A long query needs to be visible all at once, so the builder's share
// of the pane is the operator's to set: drag, arrow keys, or
// double-click to reset.  The split is remembered per browser and is
// purely cosmetic -- a missing or unreadable value falls back to the
// CSS default.

const KEY = "vg-gui.wb-split";
const MIN = 15;      // percent -- keep the results table usable
const MAX = 85;      // ...and always leave the divider grabbable
const STEP = 3;      // percent per arrow key press

function clamp(pct) {
  return Math.min(MAX, Math.max(MIN, pct));
}

function load() {
  try {
    const raw = window.localStorage.getItem(KEY);
    const pct = raw === null ? NaN : Number(raw);
    return Number.isFinite(pct) ? clamp(pct) : null;
  } catch {
    // Private windows and blocked site data both throw; the CSS
    // default is a perfectly good answer.
    return null;
  }
}

function save(pct) {
  try {
    window.localStorage.setItem(KEY, String(pct));
  } catch {
    // Not remembering the split is not worth an error path.
  }
}

export function createWorkbenchSplitter({ paneEl, handleEl }) {
  let pct = load();

  function apply(next, persist) {
    pct = clamp(next);
    paneEl.style.setProperty("--wb-split", `${pct}%`);
    handleEl.setAttribute("aria-valuenow", String(Math.round(pct)));
    if (persist) save(pct);
  }

  function currentPct() {
    if (pct !== null) return pct;
    // Not yet set: read what the CSS default actually produced rather
    // than assuming it.
    const paneH = paneEl.getBoundingClientRect().height;
    const builder = handleEl.previousElementSibling;
    if (!paneH || !builder) return 55;
    return clamp((builder.getBoundingClientRect().height / paneH) * 100);
  }

  handleEl.setAttribute("aria-valuemin", String(MIN));
  handleEl.setAttribute("aria-valuemax", String(MAX));
  if (pct !== null) apply(pct, false);

  handleEl.addEventListener("pointerdown", (ev) => {
    ev.preventDefault();
    const rect = paneEl.getBoundingClientRect();
    handleEl.setPointerCapture(ev.pointerId);
    const move = (e) => {
      if (rect.height > 0) {
        apply(((e.clientY - rect.top) / rect.height) * 100, false);
      }
    };
    const up = (e) => {
      handleEl.releasePointerCapture(e.pointerId);
      handleEl.removeEventListener("pointermove", move);
      handleEl.removeEventListener("pointerup", up);
      handleEl.removeEventListener("pointercancel", up);
      save(pct === null ? currentPct() : pct);
    };
    handleEl.addEventListener("pointermove", move);
    handleEl.addEventListener("pointerup", up);
    handleEl.addEventListener("pointercancel", up);
  });

  handleEl.addEventListener("keydown", (ev) => {
    const delta = ev.key === "ArrowUp" ? -STEP
      : ev.key === "ArrowDown" ? STEP
      : 0;
    if (delta === 0) return;
    ev.preventDefault();
    apply(currentPct() + delta, true);
  });

  handleEl.addEventListener("dblclick", () => {
    try {
      window.localStorage.removeItem(KEY);
    } catch {
      // See load(): storage being unavailable is not an error here.
    }
    pct = null;
    paneEl.style.removeProperty("--wb-split");
    handleEl.removeAttribute("aria-valuenow");
  });
}
