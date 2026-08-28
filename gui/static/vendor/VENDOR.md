# Vendored frontend libraries (GH #271)

The GUI's no-build posture (design decision 3,
`docs/superpowers/specs/2026-08-27-vg-gui-v1-design.md`): every
third-party file is committed here verbatim, fetched once by hand.
No npm, no bundler, no CDN at runtime.

## cytoscape.min.js

- **Version**: 3.34.2
- **Source**: https://unpkg.com/cytoscape@3.34.2/dist/cytoscape.min.js
  (the npm `cytoscape` package's official minified UMD build; the same
  file ships in the GitHub release of the
  [cytoscape/cytoscape.js](https://github.com/cytoscape/cytoscape.js)
  repository)
- **Retrieved**: 2026-08-27
- **License**: MIT (The Cytoscape Consortium; header retained in the
  file itself)
- **Size**: 435,503 bytes

The UMD build is not an ES module: `index.html` loads it with a
classic `<script>` tag before the module entry point, and
`js/explorer.js` reads `window.cytoscape` — the one deliberate
exception to the no-window-globals rule.

### Upgrade procedure

1. Download the new version's `dist/cytoscape.min.js` from unpkg or
   the GitHub release and overwrite `cytoscape.min.js`.
2. Update the version, source URL, retrieval date and size above.
3. Done — there is no build step. Reload the GUI page and click
   through the explorer.

## codemirror.js, codemirror.css, codemirror-overlay.js, codemirror-commonlisp.js (GH #279)

The editor for the free-text Prolog surface. Four files, all from the
same npm package and the same version.

- **Version**: 5.65.21 (**CodeMirror 5**, deliberately — CodeMirror 6
  is distributed as ES modules that must be bundled, which the no-build
  posture rules out)
- **Source**:
  - https://unpkg.com/codemirror@5.65.21/lib/codemirror.js
  - https://unpkg.com/codemirror@5.65.21/lib/codemirror.css
  - https://unpkg.com/codemirror@5.65.21/addon/mode/overlay.js
    (saved as `codemirror-overlay.js`)
  - https://unpkg.com/codemirror@5.65.21/mode/commonlisp/commonlisp.js
    (saved as `codemirror-commonlisp.js` — the vendor directory is flat)
- **Retrieved**: 2026-08-28
- **License**: MIT (Marijn Haverbeke and others; the notice is retained
  at the top of each JS file)
- **Size**: 402,055 + 8,720 + 3,243 + 4,597 bytes

⚠ **`overlayMode` is an ADDON, not core.** `CodeMirror.overlayMode` is
defined by `addon/mode/overlay.js`, *not* by `lib/codemirror.js`.
Omitting it does not fail at load — it fails when the mode is first
built, during editor construction, which is far enough from the missing
`<script>` to be hard to trace. The `codemirror-entry-points-are-vendored-and-loaded`
test in `tests/gui/gui-tests.lisp` now checks that every
`CodeMirror.<x>` the frontend calls is defined in a vendor file
`main.js` actually loads; add an addon there and it stays checked.

CodeMirror 5 ships no minified build on npm (`lib/codemirror.min.js` is
a 404), so these are the unminified originals.

The bundle is UMD, not an ES module: `js/main.js` injects classic
`<script>` tags for the two JS files and a `<link>` for the CSS, and
`js/prolog.js` reads `window.CodeMirror` — the second deliberate
exception to the no-window-globals rule, alongside cytoscape. The tags
are injected only when `GET /api/capabilities` reports `allowProlog`,
so a GUI started without `:allow-prolog` never fetches these 415 KB at
all.

No theme file is vendored: `css/gui.css` styles `.CodeMirror` from the
cockpit's own variables. The Prolog syntax colouring is an *overlay*
mode defined in `js/prolog.js` over the stock Common Lisp mode; it adds
`?variable` and unknown-head tokens and needs no further addons.

### Upgrade procedure

1. Download the four files from unpkg at the new 5.x version (keep the
   `overlay.js` → `codemirror-overlay.js` and `commonlisp.js` →
   `codemirror-commonlisp.js` renames).
2. Update the version, source URLs, retrieval date and sizes above.
3. Run `node tools/check-codemirror-mode.mjs`. It loads the real
   vendored files under a DOM shim, builds the `vg-prolog` mode the way
   the pane does, and tokenizes with a real `StringStream` — so a
   missing addon or a renamed API fails here rather than silently in a
   browser. It is not part of any test gate (it needs node, which the
   SBCL/CCL/ECL matrix does not have).
4. Reload the GUI with `:allow-prolog t`, open Query → Prolog, and
   check that `?vars` colour, an unknown head dims, and the paren
   depth indicator tracks the cursor.
