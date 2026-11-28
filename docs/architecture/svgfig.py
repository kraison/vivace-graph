"""svgfig.py -- hand-placed SVG figures for a repo's docs/architecture.md.

Canonical copy: ~/.claude/skills/diagrams/svgfig.py.  A repo vendors it
as docs/architecture/svgfig.py, beside its figures.py, so the figures
rebuild from the repo alone.  Change the canonical copy, then copy it
into each repo; never edit a repo's copy in place.

Version 1, 2026-11-28 (first used by kraison/argus docs/architecture).
Python 3, standard library only.

A figure is a Fig: boxes, cylinders, entities and edges at coordinates
you choose.  Nothing is laid out for you; that is the point.  See the
diagrams skill for the conventions."""
import html
import os


def e(s):
    return html.escape(s, quote=True)


# The house palette: argus's page tokens.  A repo with its own UI
# palette passes its own to css(); keep the token names.
LIGHT = {"paper": "#e9ecee", "surface": "#f7f9fa", "sunk": "#dfe4e7",
         "ink": "#131b21", "muted": "#56666f", "line": "#79868e",
         "live": "#0f6d63", "open": "#8a5a00", "halt": "#a02c26",
         "virt": "#56399c"}
DARK = {"paper": "#0f1518", "surface": "#172025", "sunk": "#0a0f11",
        "ink": "#e4eaed", "muted": "#93a4ad", "line": "#5d6a72",
        "live": "#52c2b1", "open": "#dda63f", "halt": "#f2756c",
        "virt": "#b49ae9"}

RULES = """
.bg { fill: var(--surface); }
text { font-family: var(--face-text); }
.nd { fill: var(--surface); stroke: var(--line); stroke-width: 1.2; }
.nd.live { stroke: var(--live); stroke-width: 1.8; }
.nd.virt { stroke: var(--virt); stroke-width: 1.8; }
.nd.halt { stroke: var(--halt); stroke-width: 1.8; }
.nd.open { stroke: var(--open); stroke-width: 1.8; }
.nd.store { stroke: var(--ink); stroke-width: 2.6; }
.nd.sub { stroke-dasharray: 5 3; }
.nd.note { fill: none; stroke-dasharray: 2 3; }
.rg { fill: none; stroke: var(--line); stroke-width: 1.2;
  stroke-dasharray: 8 6; }
.hostbox { fill: none; stroke: var(--ink); stroke-width: 1.4; }
.rt, .t1, .tm, .tms, .tk, .tag, .grp, .rn, .bt {
  font-family: var(--face-data); }
.rt { font-size: 11px; letter-spacing: 0.08em; fill: var(--muted); }
.t1 { font-size: 13px; font-weight: 700; fill: var(--ink); }
.t2 { font-size: 12px; fill: var(--muted); }
.t3 { font-size: 11px; fill: var(--muted); }
.tm { font-size: 11.5px; fill: var(--ink); }
.tms { font-size: 11.5px; font-weight: 700; fill: var(--ink); }
.tk { font-size: 11px; fill: var(--muted); }
.tby { font-size: 11px; font-style: italic; fill: var(--muted); }
.tag { font-size: 9.5px; letter-spacing: 0.1em; fill: var(--muted); }
.grp { font-size: 10.5px; letter-spacing: 0.1em; fill: var(--muted); }
.rn { font-size: 12px; fill: var(--ink); }
.rr { font-size: 11.5px; fill: var(--muted); }
.zebra { fill: var(--paper); opacity: 0.6; }
.badge { fill: var(--sunk); stroke: var(--line); stroke-width: 0.8; }
.bt { font-size: 10px; font-weight: 700; fill: var(--ink); }
.div { stroke: var(--sunk); stroke-width: 1; }
.dot { fill: var(--ink); }
.ed { fill: none; stroke: var(--ink); stroke-width: 1.3; }
.ed.soft { stroke-dasharray: 5 4; }
.ed.halt { stroke: var(--halt); stroke-width: 2; }
.ed.mut { stroke: var(--line); stroke-dasharray: 2 3; }
.halo { fill: none; stroke: var(--surface); stroke-width: 8; }
.lb { font-size: 11.5px; fill: var(--muted); paint-order: stroke;
  stroke: var(--surface); stroke-width: 4px; stroke-linejoin: round; }
.lb.halt { fill: var(--halt); }
.ah { fill: var(--ink); }
.ah-h { fill: var(--halt); }
.ah-m { fill: var(--line); }
"""

FACES = """  --face-text: Charter, "Iowan Old Style", "Charis SIL", Cambria, Georgia,
    "Times New Roman", serif;
  --face-data: ui-monospace, "DejaVu Sans Mono", "Liberation Mono", Menlo,
    Consolas, monospace; }"""


def css(light=LIGHT, dark=DARK):
    """The stylesheet a standalone figure carries: tokens, the dark
    tokens under prefers-color-scheme, then the classes."""
    def tokens(p):
        return " ".join(f"--{k}:{v};" for k, v in p.items())
    return (f"\n:root {{ {tokens(light)}\n{FACES}\n"
            f"@media (prefers-color-scheme: dark) {{\n"
            f"  :root {{ {tokens(dark)} }} }}" + RULES)


class Fig:
    """One figure, W by H.  PID prefixes its marker ids, so several
    figures can share a page.  LABEL is its aria-label: the figure's
    claim in one sentence."""

    def __init__(self, pid, w, h, label):
        self.pid, self.w, self.h, self.aria = pid, w, h, label
        self.parts = []

    def add(self, s):
        self.parts.append(s)

    def defs(self):
        p = self.pid
        out = ["<defs>"]
        for suf, cls in (("a", "ah"), ("h", "ah-h"), ("m", "ah-m")):
            out.append(
                f'<marker id="{p}-{suf}" viewBox="0 0 10 10" refX="9.6" '
                f'refY="5" markerWidth="9" markerHeight="9" '
                f'markerUnits="userSpaceOnUse" orient="auto-start-reverse">'
                f'<path d="M0,0.8 L10,5 L0,9.2 z" class="{cls}"/></marker>')
        out.append("</defs>")
        return "".join(out)

    def svg(self):
        """The figure for inlining in a page that carries the styles."""
        return (f'<svg viewBox="0 0 {self.w} {self.h}" role="img" '
                f'aria-label="{e(self.aria)}" '
                f'xmlns="http://www.w3.org/2000/svg">'
                + self.defs() + "".join(self.parts) + "</svg>")

    # shapes ----------------------------------------------------------
    def rect(self, x, y, w, h, cls, rx=6):
        self.add(f'<rect x="{x}" y="{y}" width="{w}" height="{h}" '
                 f'rx="{rx}" class="{cls}"/>')

    def text(self, x, y, s, cls, anchor="start", rotate=None):
        tr = f' transform="rotate({rotate} {x} {y})"' if rotate else ""
        self.add(f'<text x="{x}" y="{y}" class="{cls}" '
                 f'text-anchor="{anchor}"{tr}>{e(s)}</text>')

    def box(self, x, y, w, h, title, lines=(), kind="", tcls="t1"):
        """A node: a title (monospace, bold) and sublines.  KIND is
        live, virt, halt, open, store, sub or note."""
        self.rect(x, y, w, h, f"nd {kind}".strip())
        self.text(x + 10, y + 20, title, tcls)
        for i, ln in enumerate(lines):
            self.text(x + 10, y + 37 + 15 * i, ln, "t2")

    def cyl(self, x, y, w, h, title, lines=(), kind=""):
        """A store on disk."""
        ry = 7
        rx = w / 2
        self.add(
            f'<path d="M{x},{y+ry} L{x},{y+h-ry} A{rx},{ry} 0 0 0 '
            f'{x+w},{y+h-ry} L{x+w},{y+ry}" class="nd {kind}"/>')
        self.add(f'<ellipse cx="{x+rx}" cy="{y+ry}" rx="{rx}" ry="{ry}" '
                 f'class="nd {kind}"/>')
        self.text(x + 10, y + 30, title, "t1")
        for i, ln in enumerate(lines):
            self.text(x + 10, y + 47 + 15 * i, ln, "t2")

    def entity(self, x, y, tag, name, key, attrs, by, w=200,
               store=False):
        """An ERD entity: TAG (its group), NAME, its KEY, one ATTRS line
        each, and who writes it.  Returns its geometry; ROW(i) is the y
        of attribute I, where an edge for that attribute should leave."""
        h = 52 + 15 * len(attrs) + 22
        self.rect(x, y, w, h, "nd store" if store else "nd")
        self.text(x + 10, y + 14, tag, "tag")
        self.text(x + 10, y + 30, name, "t1")
        self.text(x + 10, y + 45, "key " + key, "tk")
        self.add(f'<line x1="{x}" y1="{y+52}" x2="{x+w}" y2="{y+52}" '
                 f'class="div"/>')
        for i, a in enumerate(attrs):
            self.text(x + 10, y + 52 + 15 * (i + 1) - 1, a, "tm")
        self.text(x + 10, y + h - 8, "by " + by, "tby")
        return dict(x=x, y=y, w=w, h=h, r=x + w, b=y + h,
                    row=lambda i, y=y: y + 52 + 15 * (i + 1) - 5)

    def edge(self, pts, cls="", end=True, start=False, halo=False):
        """A polyline through PTS.  CLS: soft (dashed), halt (red),
        mut (dotted grey).  HALO lays a ground-coloured band under it,
        so a line it crosses reads as passing under."""
        d = "M" + " L".join(f"{a},{b}" for a, b in pts)
        if halo:
            self.add(f'<path d="{d}" class="halo"/>')
        suf = "h" if "halt" in cls else ("m" if "mut" in cls else "a")
        mk = ""
        if end:
            mk += f' marker-end="url(#{self.pid}-{suf})"'
        if start:
            mk += f' marker-start="url(#{self.pid}-{suf})"'
        full = ("ed " + cls).strip()
        self.add(f'<path d="{d}" class="{full}"{mk}/>')

    def label(self, x, y, s, anchor="start", cls="lb", rotate=None):
        """Edge text, haloed so it reads over lines."""
        self.text(x, y, s, cls, anchor, rotate)


def standalone(svg, sheet=None):
    """SVG as a file of its own: its styles inside it, its own ground
    and its size, so it reads the same on GitHub in either theme."""
    tag, rest = svg.split(">", 1)
    w, h = tag.split('viewBox="0 0 ')[1].split('"')[0].split()
    tag = tag.replace("<svg ", f'<svg width="{w}" height="{h}" ', 1)
    return ('<?xml version="1.0" encoding="UTF-8"?>\n' + tag + ">"
            + "<style>" + (sheet or css()) + "</style>"
            + '<rect width="100%" height="100%" class="bg"/>' + rest + "\n")


def write_figures(figures, here, sheet=None):
    """Write each (name, function) in FIGURES as HERE/name.svg."""
    for name, fn in figures:
        path = os.path.join(here, name + ".svg")
        with open(path, "w") as out:
            out.write(standalone(fn(), sheet))
        print(path)
