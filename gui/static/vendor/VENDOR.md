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
