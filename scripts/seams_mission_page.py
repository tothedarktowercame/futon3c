#!/usr/bin/env python3
"""seams_mission_page.py — M-futon-seams as a working page.

The mission text is the main column and accumulates as the mission is worked;
the margin carries two columns of annotations, (a) the pattern working and
(b) the PROOF-2a reading. The page is a view of two files, not a snapshot:
re-run it and it follows whatever the mission and the annotations now say.

  seams_mission_page.py OUT.html [--rev HEAD] [--worktree]

Deterministic for fixed inputs: the mission is read from a git revision, the
annotations are sorted by (anchor start, id), and nothing in the output is
derived from the clock.

Anchors are exact character spans of the mission at a recorded sha. At render
time each is re-checked against the revision being rendered; a note whose
quote is no longer at its offsets is drawn FLAGGED in place, never silently
moved. A moved anchor is a fact about the mission having changed under an
annotation, which is the thing a reader needs to see.
"""
import argparse, html, json, os, re, subprocess, sys, hashlib

REPO = "/home/joe/code/futon3c"
MISSION = "holes/missions/M-futon-seams.md"
ANNOTATIONS = "holes/labs/M-futon-seams/annotations.edn"
CASCADES = ["holes/labs/M-futon-seams/proto/instance-4.edn",
            "holes/labs/M-futon-seams/proto/instance-5.edn",
            "holes/labs/M-futon-seams/proto/instance-6.edn",
            "holes/labs/M-futon-seams/proto/instance-7.edn"]

OPEN, CLOSE = "\x00", "\x01"          # anchor sentinels, absent from markdown


def sh(*args, **kw):
    return subprocess.run(args, capture_output=True, text=True, cwd=REPO, **kw)


def read_mission(rev, worktree):
    if worktree:
        return open(os.path.join(REPO, MISSION), encoding="utf-8").read(), "worktree"
    r = sh("git", "show", f"{rev}:{MISSION}")
    if r.returncode:
        sys.exit(f"seams_mission_page: cannot read {MISSION} at {rev}: {r.stderr.strip()}")
    return r.stdout, sh("git", "rev-parse", "--short", rev).stdout.strip()


def edn_to_json(path):
    r = sh("bb", "-e",
           '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
           f'(print (j/generate-string (edn/read-string (slurp "{path}"))))')
    if r.returncode:
        sys.exit(f"seams_mission_page: cannot read {path}: {r.stderr.strip()}")
    return json.loads(r.stdout)


# ---------------------------------------------------------------- markdown

def inline(s):
    """Escape, then the four inline forms the mission uses. Sentinels survive."""
    s = html.escape(s, quote=False)
    s = re.sub(r"`([^`]+)`", lambda m: f"<code>{m.group(1)}</code>", s)
    s = re.sub(r"\*\*([^*]+)\*\*", lambda m: f"<strong>{m.group(1)}</strong>", s)
    s = re.sub(r"(?<![*\w])\*([^*\n]+)\*(?!\*)", lambda m: f"<em>{m.group(1)}</em>", s)
    return s


def render_markdown(text):
    """A small renderer for the subset the mission uses. Deterministic."""
    out, lines, i = [], text.split("\n"), 0
    while i < len(lines):
        ln = lines[i]
        if ln.startswith("```"):
            j = i + 1
            buf = []
            while j < len(lines) and not lines[j].startswith("```"):
                buf.append(lines[j]); j += 1
            body = html.escape("\n".join(buf), quote=False)
            out.append(f"<pre class=\"code\">{body}</pre>")
            i = j + 1
            continue
        m = re.match(r"^(#{1,6})\s+(.*)$", ln)
        if m:
            lvl = len(m.group(1))
            slug = re.sub(r"[^a-z0-9]+", "-", m.group(2).lower()).strip("-")[:48]
            out.append(f'<h{lvl} id="s-{slug}">{inline(m.group(2))}</h{lvl}>')
            i += 1
            continue
        if re.match(r"^\s*[-*]\s+", ln):
            items = []
            while i < len(lines) and re.match(r"^\s*[-*]\s+", lines[i]):
                items.append(inline(re.sub(r"^\s*[-*]\s+", "", lines[i]))); i += 1
            out.append("<ul>" + "".join(f"<li>{x}</li>" for x in items) + "</ul>")
            continue
        if re.match(r"^\s*\d+\.\s+", ln):
            items = []
            while i < len(lines) and re.match(r"^\s*\d+\.\s+", lines[i]):
                items.append(inline(re.sub(r"^\s*\d+\.\s+", "", lines[i]))); i += 1
            out.append("<ol>" + "".join(f"<li>{x}</li>" for x in items) + "</ol>")
            continue
        if ln.startswith(">"):
            items = []
            while i < len(lines) and lines[i].startswith(">"):
                items.append(inline(lines[i].lstrip("> "))); i += 1
            out.append("<blockquote>" + " ".join(items) + "</blockquote>")
            continue
        if not ln.strip():
            i += 1
            continue
        para = []
        while i < len(lines) and lines[i].strip() and not lines[i].startswith(("#", "```", ">")) \
                and not re.match(r"^\s*([-*]|\d+\.)\s+", lines[i]):
            para.append(lines[i]); i += 1
        out.append(f"<p>{inline(' '.join(para))}</p>")
    return "\n".join(out)


# ---------------------------------------------------------------- anchors

def place_anchors(text, notes):
    """Insert sentinels at each live anchor, outermost first, and report which
    anchors no longer hold. Offsets are applied from the end so earlier ones
    stay valid."""
    live, stale = [], []
    for n in notes:
        a = n["anchor"]
        s, e, q = a["start"], a["end"], a["quote"]
        if text[s:e] == q:
            live.append(n)
        else:
            found = text.find(q)
            n["_stale"] = ("quote found at %d, recorded %d" % (found, s) if found >= 0
                           else "quote no longer present in the mission")
            stale.append(n)
    marks = []
    for n in live:
        marks.append((n["anchor"]["start"], "open", n["id"]))
        marks.append((n["anchor"]["end"], "close", n["id"]))
    # apply right to left; at one position, closes before opens
    marks.sort(key=lambda m: (-m[0], 0 if m[1] == "close" else 1))
    for pos, kind, nid in marks:
        tag = f"{OPEN}{nid}{OPEN}" if kind == "open" else CLOSE
        text = text[:pos] + tag + text[pos:]
    return text, live, stale


def resolve_sentinels(rendered):
    rendered = re.sub(OPEN + r"([a-zA-Z0-9_-]+)" + OPEN,
                      lambda m: f'<span class="anchor" id="anc-{m.group(1)}" '
                                f'data-note="{m.group(1)}">', rendered)
    return rendered.replace(CLOSE, "</span>")


# ---------------------------------------------------------------- page

def note_html(n, figures):
    col = n.get("column", "pattern")
    cls = "note col-" + ("a" if col == "pattern" else "b")
    if n.get("status") != "reviewed":
        cls += " draft"
    if n.get("_stale"):
        cls += " stale"
    verdict = n.get("verdict")
    bits = []
    head = [f'<span class="nid">{html.escape(n["id"])}</span>',
            f'<span class="nauth">{html.escape(n.get("author", "?"))}</span>']
    if n.get("status") != "reviewed":
        head.append('<span class="ndraft">draft</span>')
    if verdict:
        head.append(f'<span class="verdict v-{html.escape(verdict)}">'
                    f'{html.escape(verdict)}</span>')
    bits.append('<p class="nhead">' + " ".join(head) + "</p>")
    if n.get("_stale"):
        bits.append(f'<p class="nstale">anchor no longer holds — {html.escape(n["_stale"])}. '
                    f'Recorded quote: “{html.escape(n["anchor"]["quote"][:80])}”</p>')
    bits.append(f'<p class="nbody">{html.escape(n.get("body",""))}</p>')
    fig = n.get("figure")
    if fig and fig in figures:
        # The rail is ~20rem and the lattice is up to 1540px: in the rail it is
        # a thumbnail that says "there is a shape here", and the shape itself is
        # readable in an overlay. Both are the same SVG, emitted once.
        bits.append(f'<div class="nfig" data-fig="{html.escape(fig)}" '
                    f'title="click to open full width">{figures[fig]}'
                    f'<span class="figopen">open ⤢</span></div>')
    if n.get("refs"):
        bits.append('<p class="nrefs">' +
                    " · ".join(f"<code>{html.escape(r)}</code>" for r in n["refs"]) + "</p>")
    return (f'<aside class="{cls}" id="note-{html.escape(n["id"])}" '
            f'data-anchor="{html.escape(n["id"])}">' + "".join(bits) + "</aside>")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--rev", default="HEAD")
    ap.add_argument("--worktree", action="store_true",
                    help="render the working copy instead of a committed revision")
    a = ap.parse_args()

    text, revlabel = read_mission(a.rev, a.worktree)
    sha = hashlib.sha256(text.encode()).hexdigest()
    notes = edn_to_json(ANNOTATIONS)
    notes.sort(key=lambda n: (n["anchor"]["start"], n["id"]))

    figures = {}
    bundle = os.path.join("/tmp", "seams-bundle.json")
    r = sh("bb", "holes/labs/M-futon-seams/build_page.clj", bundle, *CASCADES)
    if r.returncode == 0 and os.path.exists(bundle):
        sys.path.insert(0, os.path.join(REPO, "scripts"))
        import seams_page
        for b in json.load(open(bundle)):
            figures[f"instance-{b['instance']}"] = seams_page.svg(b)

    marked, live, stale = place_anchors(text, notes)
    body = resolve_sentinels(render_markdown(marked))

    cols = {"a": [n for n in notes if n.get("column") == "pattern"],
            "b": [n for n in notes if n.get("column") == "proof2a"]}
    # Notes are emitted into their rails server-side: the page is readable with
    # no JavaScript at all, and the script only sets each note's vertical offset.
    rail_a = "".join(note_html(n, figures) for n in notes
                     if n.get("column") != "proof2a")
    rail_b = "".join(note_html(n, figures) for n in notes
                     if n.get("column") == "proof2a")

    counts = (f'{len(notes)} notes — {len(cols["a"])} pattern, {len(cols["b"])} PROOF-2a; '
              f'{sum(1 for n in notes if n.get("status") == "reviewed")} reviewed, '
              f'{len(stale)} with a stale anchor')

    open(a.out, "w", encoding="utf-8").write(PAGE.format(
        css=CSS, js=JS, body=body, rail_a=rail_a, rail_b=rail_b,
        rev=html.escape(revlabel),
        sha=sha[:16], counts=html.escape(counts),
        mission=html.escape(MISSION)))
    print(a.out)
    print(f"  rev {revlabel} sha {sha[:16]} · {counts}")
    for n in stale:
        print(f"  STALE {n['id']}: {n['_stale']}")


CSS = """
:root { --measure: 34rem; --gutter: 1.8rem; --colw: 20rem; }
*,*::before,*::after { box-sizing:border-box; }
body { margin:0; padding:2.5rem 3vw 8rem; background:#fffff8; color:#111;
       font:1.02rem/1.62 et-book, Palatino, "Palatino Linotype", Georgia, serif; }
.page { position:relative; display:grid; grid-template-columns:
        var(--measure) var(--colw) var(--colw); column-gap:var(--gutter);
        align-items:start; }
.main { grid-column:1; }
.rail { position:relative; }
.rail-a { grid-column:2; }
.rail-b { grid-column:3; }
.railhead { position:sticky; top:0; background:#fffff8; padding:.2rem 0 .4rem;
            font-size:.68rem; font-variant:small-caps; letter-spacing:.07em;
            color:#999; border-bottom:1px solid #eae6d8; z-index:2; }
h1 { font-size:1.55rem; font-weight:400; margin:0 0 .2rem; }
h2 { font-size:1.15rem; font-weight:400; margin:2.2rem 0 .5rem;
     border-bottom:1px solid #e6e2d4; padding-bottom:.25rem; }
h3 { font-size:.98rem; font-weight:600; margin:1.6rem 0 .4rem; }
p { margin:0 0 .85rem; }
code { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.85em; }
pre.code { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.72rem;
           line-height:1.5; background:#f4f2ea; border-left:3px solid #c9c4b0;
           padding:.6rem .8rem; overflow-x:auto; white-space:pre-wrap; }
blockquote { margin:0 0 .85rem; padding-left:.9rem; border-left:3px solid #c9c4b0;
             color:#444; }
ul,ol { margin:0 0 .85rem; padding-left:1.2rem; }
li { margin:0 0 .3rem; }
.anchor { border-bottom:1.5px solid #b8431f; cursor:pointer; }
.anchor.lit { background:#fbe6dd; }
.note { position:absolute; width:100%; font-size:.72rem; line-height:1.45;
        border-left:2px solid #ddd; padding:.15rem 0 .15rem .6rem; color:#333; }
.note.col-a { border-left-color:#1b6b3a; }
.note.col-b { border-left-color:#2a4d8f; }
.note.draft { background:repeating-linear-gradient(135deg,transparent,transparent 8px,
              rgba(0,0,0,.018) 8px, rgba(0,0,0,.018) 16px); }
.note.stale { border-left-color:#b8431f; background:#fdf1ec; }
.note.lit { background:#fbe6dd; }
.nhead { margin:0 0 .25rem; font-size:.62rem; font-family:ui-monospace,Menlo,monospace; }
.nid { color:#555; }
.nauth { color:#aaa; margin-left:.4rem; }
.ndraft { color:#a8791d; margin-left:.4rem; font-variant:small-caps; letter-spacing:.05em; }
.verdict { margin-left:.4rem; font-variant:small-caps; letter-spacing:.05em; }
.v-fits { color:#1b6b3a; } .v-breaks { color:#b8431f; } .v-missing-field { color:#a8791d; }
.nbody { margin:0; }
.nstale { margin:0 0 .3rem; color:#b8431f; font-size:.68rem; }
.nrefs { margin:.3rem 0 0; color:#999; font-size:.62rem; word-break:break-all; }
.nfig { margin:.4rem 0; overflow:hidden; cursor:zoom-in; position:relative;
        border:1px solid #eae6d8; background:#fff; padding:.2rem; }
.figopen { position:absolute; right:.25rem; bottom:.2rem; font-size:.58rem;
           color:#999; font-family:ui-monospace,Menlo,monospace;
           background:#fffff8; padding:0 .2rem; }
.figmodal { position:fixed; inset:0; background:rgba(255,255,248,.97); z-index:50;
            display:flex; align-items:center; justify-content:center; padding:3vw;
            cursor:zoom-out; }
.figmodal svg { width:100%; max-width:1600px; height:auto; }
.figmodal .figcap { position:absolute; top:1.2rem; left:3vw; font-size:.72rem;
                    color:#888; font-family:ui-monospace,Menlo,monospace; }
.nfig svg { width:100%; height:auto; min-width:0; }
.nfig .nid, .nfig .nfam, .nfig .nprod, .nfig .nhole { font-size:9px; }
.masthead { max-width:var(--measure); }
.sub { color:#666; font-size:.8rem; margin-bottom:.4rem; }
.prov { color:#999; font-size:.68rem; font-family:ui-monospace,Menlo,monospace;
        margin-bottom:2rem; }
footer { margin-top:5rem; padding-top:1rem; border-top:1px solid #e6e2d4;
         color:#999; font-size:.7rem; }
@media (max-width:78rem) {
  .page { grid-template-columns:1fr; }
  .rail { display:none; }
  .note { position:static; width:auto; margin:.6rem 0; }
}
"""

JS = """
// Place each note beside its anchor, stacking downward so two notes anchored
// close together do not overlap. Layout only -- the note's column and order
// come from the data, not from here.
function layout() {
  const wide = window.matchMedia('(min-width: 78rem)').matches;
  const rails = {a: document.querySelector('.rail-a'), b: document.querySelector('.rail-b')};
  if (!wide) return;
  const bottom = {a: 0, b: 0};
  document.querySelectorAll('.note').forEach(n => {
    const col = n.classList.contains('col-a') ? 'a' : 'b';
    const anc = document.getElementById('anc-' + n.dataset.anchor);
    const rail = rails[col];
    const top = anc ? anc.getBoundingClientRect().top + window.scrollY
                      - rail.getBoundingClientRect().top - window.scrollY : bottom[col];
    const y = Math.max(top, bottom[col] + 10);
    n.style.top = y + 'px';
    bottom[col] = y + n.offsetHeight;
  });
  rails.a.style.minHeight = bottom.a + 'px';
  rails.b.style.minHeight = bottom.b + 'px';
}
document.addEventListener('click', e => {
  const open = e.target.closest('.nfig');
  if (open) {
    const m = document.createElement('div');
    m.className = 'figmodal';
    m.innerHTML = '<span class="figcap">' + open.dataset.fig + '</span>'
                + open.querySelector('svg').outerHTML;
    m.addEventListener('click', () => m.remove());
    document.body.appendChild(m);
    return;
  }
  const t = e.target.closest('.anchor, .note');
  document.querySelectorAll('.lit').forEach(x => x.classList.remove('lit'));
  if (!t) return;
  const id = t.dataset.note || t.dataset.anchor;
  document.getElementById('anc-' + id)?.classList.add('lit');
  document.getElementById('note-' + id)?.classList.add('lit');
});
window.addEventListener('load', layout);
window.addEventListener('resize', layout);
"""

PAGE = """<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>M-futon-seams — worked against PROOF-2a</title>
<style>{css}</style></head><body>
<div class="masthead">
<h1>M-futon-seams, worked</h1>
<p class="sub">The mission is the main text and grows as it is worked. The margin carries
two columns: <b>the pattern working</b> — cascade nodes, work-state tokens, context→pattern
edges — and <b>the PROOF-2a reading</b> of the same span: which clause or data shape it
exercises, and whether this example fits the shape, breaks it, or needs a field that does
not exist. Breaks are the evidence worth most. Click a marked span or a note to pair them.</p>
<p class="prov">{mission} at {rev} · sha256 {sha}… · {counts}</p>
</div>
<div class="page">
  <article class="main">{body}</article>
  <div class="rail rail-a"><p class="railhead">the pattern working</p>{rail_a}</div>
  <div class="rail rail-b"><p class="railhead">the PROOF-2a reading</p>{rail_b}</div>
</div>
<footer>Generated by <code>scripts/seams_mission_page.py</code> from
<code>{mission}</code> and <code>holes/labs/M-futon-seams/annotations.edn</code>.
Re-runnable and byte-deterministic for a fixed revision and annotation set.
Anchors are checked against the revision rendered; a note whose quote has moved is
drawn flagged in place rather than relocated.</footer>
<script>{js}</script>
</body></html>
"""

if __name__ == "__main__":
    main()
