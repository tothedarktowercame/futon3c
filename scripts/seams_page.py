#!/usr/bin/env python3
"""seams_page.py — render the M-futon-seams cascades as semilattice diagrams.

Reads the bundle build_page.clj emits (cascade + layout depths + frontier
analysis + kernel numbers) and writes one static page. The diagrams are
generated from the same EDN the kernels run on, so a diagram cannot drift
from the cascade it draws -- which is instance 7's defect, and the reason
this script does not reimplement the kernel.

  seams_page.py BUNDLE.json OUT.html
"""
import html, json, sys

C = {"pattern": "#1b6b3a", "hole": "#a8791d", "accent": "#b8431f",
     "rule": "#c9c4b0", "ink": "#111", "quiet": "#888"}

TITLES = {4: "Provider versus role",
          5: "Transport and Room",
          6: "Prompts as an interface",
          7: "Editor coupling"}

STANDS = {
 4: ("The mission's IDENTIFY exit names this one first, because it is the smallest surface: "
     "<b>51 provider-literal agent ids</b> across <code>src/</code>, and three sites that decide "
     "routing by reading the provider out of the identifier — "
     "<code>transport/http.clj:4752</code>, <code>:4795</code>, "
     "<code>apm/library_loop_adapter.clj:316</code>. The role is <i>implementer</i>; the string says "
     "<i>codex</i>."),
 5: ("<code>grep defprotocol src/futon3c/transport/</code> returns nothing, so there is no interface "
     "for an adapter to satisfy. <code>irc.clj</code> sits as a peer of <code>http.clj</code> and "
     "<code>ws.clj</code>; <b>16 hardcoded <code>#futon</code> literals</b> pin the room; and Matrix "
     "reaches the system through <code>matrix-ircd</code>, impersonating the transport the code "
     "insists on."),
 6: ("The couplings were hardcoded not only in code but <b>in the prompt text given to agents</b> — "
     "&ldquo;tell claude-5 to do blah and store it at /home/joe&rdquo;. The retrofit left the "
     "original in place and added a mode flag, a shadow module mirroring the call sites, and "
     "regex interception of natural language at runtime. Both versions are live at once. This is "
     "the only one of the four cascades that exercises a <b>conflict</b>."),
 7: ("<b>30 <code>.el</code> files, 23,325 lines.</b> <code>session-turn-analysis.el</code> is "
     "<b>497 lines of which 48</b> touch buffers, overlays, points, markers, faces or windows — "
     "about a tenth. The turn record format already has <b>three writers</b> (elisp, "
     "<code>turn_batch.py</code>, the ClojureScript reader) agreeing by convention, and they have "
     "already disagreed once."),
}

def short(pid):
    return pid.split("/", 1)[1] if "/" in pid else pid

def svg(b):
    """Layer the patterns by longest-path depth and draw the above-relation."""
    pats, above = b["patterns"], b["above"]
    layers = {}
    for pid, p in sorted(pats.items()):
        layers.setdefault(p["depth"], []).append(pid)
    COLW, ROWH, BW, BH = 250, 118, 196, 74
    # A hole gets its own column past the deepest pattern: it is not produced
    # by anything, so placing it among the produced nodes would imply an edge.
    open_holes = [h for h in (b.get("holes") or []) if h.get("status") != "closed"]
    cols = max(layers) + 1 + (1 if open_holes else 0)
    width = COLW * cols + 130
    height = ROWH * max(len(v) for v in layers.values()) + 60
    pos = {}
    for d, ids in layers.items():
        top = (height - ROWH * len(ids)) / 2
        for i, pid in enumerate(ids):
            pos[pid] = (54 + d * COLW, top + i * ROWH + 20)

    out = [f'<svg viewBox="0 0 {width} {height}" class="lattice" '
           f'role="img" aria-label="pattern semilattice for instance {b["instance"]}">']
    out.append('<defs><marker id="a" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="7" '
               'markerHeight="7" orient="auto"><path d="M0 0 L8 4 L0 8 z" fill="#8a8578"/></marker></defs>')
    for e in above:
        cx, cy = pos[e["context"]]
        px, py = pos[e["pattern"]]
        x1, y1 = cx + BW, cy + BH / 2
        x2, y2 = px, py + BH / 2
        mid = (x1 + x2) / 2
        dash = ' stroke-dasharray="5 4"' if e["kind"] == "jointly-with" else ""
        out.append(
            f'<path d="M{x1} {y1} C{mid} {y1} {mid} {y2} {x2} {y2}" fill="none" '
            f'stroke="#8a8578" stroke-width="1.4"{dash} marker-end="url(#a)">'
            f'<title>{html.escape(e["kind"])} — {html.escape(e["via"])}</title></path>')
    for pid, (x, y) in pos.items():
        p = pats[pid]
        prod = ", ".join(short(t) for t in p["produces"])
        out.append(
            f'<g class="node" data-pat="{html.escape(pid)}">'
            f'<title>{html.escape(p["receipt"]["reading"])}</title>'
            f'<rect x="{x}" y="{y}" width="{BW}" height="{BH}" rx="3" fill="#fff" '
            f'stroke="{C["pattern"]}" stroke-width="1.2"/>'
            f'<text x="{x+10}" y="{y+19}" class="nfam">{html.escape(pid.split("/")[0])}/</text>'
            f'<text x="{x+10}" y="{y+35}" class="nid">{html.escape(short(pid))}</text>'
            f'<text x="{x+10}" y="{y+56}" class="nprod">⊢ {html.escape(prod)}</text></g>')
    for k, h in enumerate(h for h in (b.get("holes") or [])
                          if h.get("status") != "closed"):
        x = 54 + (max(layers) + 1) * COLW
        y = (height - BH) / 2 + k * ROWH
        out.append(
            f'<g class="node"><title>{html.escape(h["why-not-citable"])}</title>'
            f'<rect x="{x}" y="{y}" width="{BW}" height="{BH}" rx="3" fill="#fffdf5" '
            f'stroke="{C["hole"]}" stroke-width="1.4" stroke-dasharray="4 3"/>'
            f'<text x="{x+10}" y="{y+19}" class="nhole">HOLE</text>'
            f'<text x="{x+10}" y="{y+35}" class="nid">{html.escape(short(h["token"]))}</text>'
            f'<text x="{x+10}" y="{y+56}" class="nprod">no pattern produces this</text></g>')
    # Markings clause 0 asks for, drawn on the cascade they belong to.
    # Distinct maximal pairs only: in instance 5 all three missing meets share
    # one pair, and drawing it three times would say three things.
    seen = set()
    for m in (b.get("meets", {}) or {}).get("missing", []):
        mc = m["maximal-common"]
        if len(mc) != 2 or tuple(mc) in seen:
            continue
        seen.add(tuple(mc))
        if mc[0] not in pos or mc[1] not in pos:
            continue
        (x1, y1), (x2, y2) = pos[mc[0]], pos[mc[1]]
        cx = max(x1, x2) + BW + 26
        out.append(
            f'<path d="M{x1+BW} {y1+BH/2} C{cx} {y1+BH/2} {cx} {y2+BH/2} {x2+BW} {y2+BH/2}" '
            f'fill="none" stroke="{C["hole"]}" stroke-width="1.6" stroke-dasharray="3 3">'
            f'<title>no meet: these are the maximal units of the common part of '
            f'{html.escape(short(m["pair"][0]))} and {html.escape(short(m["pair"][1]))}, '
            f'and neither is below the other</title></path>'
            f'<text x="{cx+6}" y="{(y1+y2)/2+BH/2}" class="nmeet">no meet</text>')

    conf_seen = set()
    for w in b.get("wide-states", []):
        for a_id, b_id, toks in w.get("conflicts", []):
            key = tuple(sorted((a_id, b_id)))
            if key in conf_seen or a_id not in pos or b_id not in pos:
                continue
            conf_seen.add(key)
            (x1, y1), (x2, y2) = pos[a_id], pos[b_id]
            cx = min(x1, x2) - 24
            out.append(
                f'<path d="M{x1} {y1+BH/2} C{cx} {y1+BH/2} {cx} {y2+BH/2} {x2} {y2+BH/2}" '
                f'fill="none" stroke="{C["accent"]}" stroke-width="2" stroke-dasharray="6 3">'
                f'<title>conflicting frontier: each produces a token the other forbids '
                f'({html.escape(", ".join(short(t) for t in toks))})</title></path>'
                f'<text x="{cx-4}" y="{(y1+y2)/2+BH/2}" class="nconf" '
                f'text-anchor="end">conflict</text>')

    out.append("</svg>")
    return "".join(out)

def runs_table(b):
    rows = []
    for r in b["runs"]:
        l, c, i = r["list"], r["coapp"], r["inter"]
        lp = f'{l["p-min"]:.3f}' if l["p-min"] == l["p-max"] else f'{l["p-min"]:.3f}–{l["p-max"]:.3f}'
        le = f'{l["e-min"]:.3f}' if l["e-min"] == l["e-max"] else f'{l["e-min"]:.3f}–{l["e-max"]:.3f}'
        hit = ' class="gap"' if c["p"] - l["p-max"] > 0.05 else ""
        rows.append(
            f'<tr{hit}><td>{r["horizon"]}</td><td>{lp}</td><td>{le}</td>'
            f'<td>{c["p"]:.3f}</td><td>{c["e"]:.3f}</td>'
            f'<td>{i["p"]:.3f}</td><td>{i["e"]:.3f}</td></tr>')
    return ('<table class="runs"><thead><tr><th>horizon</th>'
            '<th colspan="2">:list (over all linear extensions)</th>'
            '<th colspan="2">:coapp</th><th colspan="2">:inter</th></tr>'
            '<tr><th></th><th>p(all wants)</th><th>E[#wants]</th><th>p</th><th>E</th>'
            '<th>p</th><th>E</th></tr></thead><tbody>'
            + "".join(rows) + "</tbody></table>")

def instance_section(b):
    n = b["instance"]
    pats = b["patterns"]
    produced_by = {}
    for pid, p in pats.items():
        for t in p["produces"]:
            produced_by.setdefault(t, []).append(pid)

    toks = []
    for t, meta in b["tokens"].items():
        by = produced_by.get(t)
        mark = (f'<span class="by">{html.escape(short(by[0]))}</span>' if by
                else '<span class="nobody">no producer</span>')
        want = ' <span class="wantmark">want</span>' if t in b["want"] else ""
        toks.append(f'<tr data-tok="{html.escape(t)}" '
                    f'data-by="{html.escape(by[0]) if by else ""}">'
                    f'<td class="tok">{html.escape(short(t))}{want}</td>'
                    f'<td>{html.escape(meta["statement"])}</td><td>{mark}</td></tr>')

    edges = []
    for e in b["above"]:
        k = "differentiates" if e["kind"] == "differentiates" else "jointly&nbsp;with"
        edges.append(
            f'<p class="edge" data-from="{html.escape(e["context"])}" '
            f'data-to="{html.escape(e["pattern"])}"><span class="efrom">{html.escape(short(e["context"]))}</span>'
            f'<span class="ekind {e["kind"]}"> {k} </span>'
            f'<span class="eto">{html.escape(short(e["pattern"]))}</span><br>'
            f'<span class="evia">{html.escape(e["via"])}</span></p>')

    wide = []
    for w in sorted(b["wide-states"], key=lambda w: not w.get("conflicts"))[:6]:
        st = ", ".join(short(s) for s in w["state"]) or "∅ (the initial state)"
        fr = " · ".join(short(f) for f in w["frontier"])
        conf = ""
        if w.get("conflicts"):
            pairs = "; ".join(
                f'{html.escape(short(a))} produces {html.escape(", ".join(short(t) for t in toks))}, '
                f'which {html.escape(short(b))} forbids'
                for a, b, toks in w["conflicts"])
            conf = f'<br><span class="wconf">CONFLICT — {pairs}</span>'
        wide.append(f'<p class="wide{" hasconf" if w.get("conflicts") else ""}">'
                    f'<span class="wstate">{html.escape(st)}</span><br>'
                    f'<span class="wfront">both enabled, neither above the other: '
                    f'{html.escape(fr)}</span>{conf}</p>')

    holes = ""
    for h in (b.get("holes") or []):
        if h.get("status") == "closed":
            holes += (f'<div class="holebox closed"><p class="hh">Closed hole: '
                      f'<code>{html.escape(short(h["token"]))}</code></p>'
                      f'<p>{html.escape(h.get("closed-by",""))}</p>'
                      f'<p class="hc">{html.escape(h["consequence"])}</p></div>')
            continue
        holes = (f'<div class="holebox"><p class="hh">Unproduced want: '
                 f'<code>{html.escape(short(h["token"]))}</code></p>'
                 f'<p>{html.escape(h["wanted"])}</p>'
                 f'<p>Nearest pattern: <code>{html.escape(h["nearest"])}</code> — '
                 f'{html.escape(h["why-not-citable"])}</p>'
                 f'<p class="hc">{html.escape(h["consequence"])}</p></div>')

    return f"""
<section class="instance" id="instance-{n}">
  <h2>Instance {n} — {html.escape(TITLES[n])}</h2>
  <p class="stands">{STANDS[n]}</p>
  <div class="latwrap">{svg(b)}</div>
  <p class="legend"><span class="k-solid">solid</span> differentiates ·
     <span class="k-dash">dashed</span> jointly with — both contexts are required ·
     hover any node for its reading, any edge for its reason ·
     <span class="k-hole">amber dashed</span> a want no pattern produces</p>
  {holes}
  <h3>Work-state tokens</h3>
  <table class="toks"><thead><tr><th>token</th><th>what it means, on this instance</th>
    <th>produced by</th></tr></thead><tbody>{''.join(toks)}</tbody></table>
  <h3>Why each edge is there</h3>
  <div class="edges">{''.join(edges)}</div>
  <h3>Where the kernels can disagree</h3>
  <p class="note">{b['wide-count']} of {b['reachable']} reachable states have a frontier
    of two or more — two patterns enabled with neither above the other. On a chain there
    are none, and the three kernels coincide. {b['linear-extensions']} linear extensions
    of this order exist; <code>:list</code> is run on every one of them.</p>
  <div class="wides">{''.join(wide)}</div>
  <h3>kernels.clj output</h3>
  {runs_table(b)}
</section>"""

def main():
    bundle, out = sys.argv[1], sys.argv[2]
    data = json.load(open(bundle))
    body = "".join(instance_section(b) for b in sorted(data, key=lambda x: x["instance"]))
    open(out, "w", encoding="utf-8").write(PAGE.format(body=body, css=CSS, js=JS))
    print(out)

JS = """
// Clicking a pattern lights what it produces and the edges that touch it --
// the same gesture the turn feed uses to link a cue to its note.
document.addEventListener('click', e => {
  const g = e.target.closest('.node');
  document.querySelectorAll('.lit').forEach(n => n.classList.remove('lit'));
  if (!g || !g.dataset.pat) return;
  const id = g.dataset.pat;
  g.classList.add('lit');
  const sec = g.closest('section');
  sec.querySelectorAll(`tr[data-by="${id}"]`).forEach(r => r.classList.add('lit'));
  sec.querySelectorAll(`.edge[data-from="${id}"], .edge[data-to="${id}"]`)
     .forEach(p => p.classList.add('lit'));
});
"""

CSS = """
:root { --measure: clamp(22rem, 46vw, 46rem); --pad: 4vw; }
*,*::before,*::after { box-sizing: border-box; }
body { margin:0; padding:3rem var(--pad) 6rem; background:#fffff8; color:#111;
       font:1.02rem/1.65 et-book, Palatino, "Palatino Linotype", Georgia, serif; }
h1 { font-size:1.7rem; font-weight:400; margin:0 0 .3rem; }
h2 { font-size:1.25rem; font-weight:400; margin:0 0 .6rem;
     border-bottom:1px solid #e6e2d4; padding-bottom:.3rem; }
h3 { font-size:.85rem; font-weight:600; font-variant:small-caps; letter-spacing:.06em;
     color:#666; margin:2rem 0 .6rem; }
p, li { max-width: var(--measure); }
code { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.85em; }
.sub { color:#666; font-size:.85rem; max-width:var(--measure); }
.instance { margin:0 0 5rem; }
.stands { max-width:var(--measure); }
.latwrap { overflow-x:auto; margin:1.4rem 0 .5rem; }
.lattice { max-width:100%; height:auto; min-width:640px; }
.nfam { font-size:10px; fill:#999; font-family:ui-monospace,Menlo,monospace; }
.nid  { font-size:12.5px; fill:#1b6b3a; font-weight:600;
        font-family:ui-monospace,Menlo,monospace; }
.nprod{ font-size:10.5px; fill:#555; font-family:ui-monospace,Menlo,monospace; }
.nhole{ font-size:11px; fill:#a8791d; font-weight:700;
        font-family:ui-monospace,Menlo,monospace; }
.nmeet{ font-size:9.5px; fill:#a8791d; font-family:ui-monospace,Menlo,monospace; }
.nconf{ font-size:9.5px; fill:#b8431f; font-weight:700;
        font-family:ui-monospace,Menlo,monospace; }
.node { cursor:pointer; }
.node.lit rect { fill:#fbe6dd; stroke-width:2; }
tr.lit td, p.edge.lit { background:#fbe6dd; }
.legend { font-size:.72rem; color:#888; max-width:none; }
.k-solid{ color:#555; border-bottom:1.5px solid #8a8578; }
.k-dash { color:#555; border-bottom:1.5px dashed #8a8578; }
.k-hole { color:#a8791d; }
table { border-collapse:collapse; font-size:.78rem; margin:.4rem 0 1rem; }
th { text-align:left; font-weight:600; color:#666; font-variant:small-caps;
     letter-spacing:.04em; border-bottom:1px solid #ddd; padding:.25rem .7rem .25rem 0; }
td { padding:.3rem .7rem .3rem 0; border-bottom:1px solid #f0ece0; vertical-align:top; }
.toks td:nth-child(2) { max-width:34rem; }
.tok { font-family:ui-monospace,Menlo,monospace; color:#333; white-space:nowrap; }
.by { font-family:ui-monospace,Menlo,monospace; color:#1b6b3a; font-size:.72rem; }
.nobody { color:#a8791d; font-weight:600; font-size:.72rem; }
.wantmark { font-size:.6rem; color:#b8431f; font-variant:small-caps;
            letter-spacing:.06em; margin-left:.3rem; }
.edges { columns:2 21rem; column-gap:2.2rem; }
.edge { font-size:.74rem; line-height:1.5; margin:0 0 .9rem; break-inside:avoid;
        max-width:none; }
.efrom,.eto { font-family:ui-monospace,Menlo,monospace; color:#1b6b3a; }
.ekind { color:#8a8578; font-size:.68rem; font-variant:small-caps; letter-spacing:.05em; }
.ekind.jointly-with { color:#a8791d; }
.evia { color:#555; }
.wides { columns:2 21rem; column-gap:2.2rem; }
.wide { font-size:.73rem; margin:0 0 .8rem; break-inside:avoid; max-width:none; }
.wstate { font-family:ui-monospace,Menlo,monospace; color:#777; }
.wfront { color:#b8431f; }
.wconf { color:#8a2a12; font-weight:600; }
.wide.hasconf { background:#fbe6dd; padding:.4rem .5rem; border-left:2px solid #b8431f; }
.note { font-size:.8rem; color:#555; }
.runs td:first-child, .runs th:first-child { text-align:right; padding-right:1.2rem; }
.runs td { font-family:ui-monospace,Menlo,monospace; font-size:.74rem; }
.runs tr.gap td { background:#fbe6dd; }
.holebox.closed { background:#f6f8f4; border-left-color:#1b6b3a; }
.holebox.closed .hh { color:#1b6b3a; }
.holebox { background:#fffdf5; border-left:3px solid #a8791d; padding:.7rem .9rem;
           margin:1rem 0; max-width:var(--measure); font-size:.8rem; }
.holebox p { margin:.3rem 0; }
.hh { font-weight:600; color:#a8791d; }
.hc { color:#555; font-style:italic; }
footer { margin-top:5rem; padding-top:1rem; border-top:1px solid #e6e2d4;
         color:#999; font-size:.72rem; max-width:none; }
footer a { color:#999; }
"""

PAGE = """<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>M-futon-seams — pattern cascades as semilattices</title>
<style>{css}</style></head><body>
<h1>M-futon-seams — three instances as pattern cascades</h1>
<p class="sub">Companion to <code>futon3c/holes/missions/M-futon-seams.md</code>, the record of
Rob's interface-first discipline. Each instance below is written as a cascade in the
<code>:m-futon-seams/proto-cascade-v1</code> schema and run through
<code>holes/labs/M-futon-seams/proto/kernels.clj</code>, the PROOF-2a kernel prototype.
Every pattern drawn is a real file in <code>futon3/library</code> whose
<code>@flexiarg</code> declaration matches its path, carrying the sha256 of the bytes
that were read. The diagrams are generated from the same EDN the kernels run on.</p>
<p class="sub"><b>The question these examples exist to settle.</b> PROOF-2a clause 0
asks whether a cascade's semilattice structure does any work: flatten it to a
topological order, recompute, and if nothing changes the record says so. Below, the
structure is drawn, the states where two patterns are enabled at once are listed, and
the three candidate kernels are run at three horizons. Joe: settle it with examples,
not fiat.</p>
{body}
<section class="instance">
<h2>What the three runs say</h2>
<p><b>The semilattice changes the rate, not the reachability.</b> On all three
instances the co-application kernel reaches the wants sooner than any linear
extension, and by horizon 14 the difference is gone (0.998 against 1.000). The
structure does work exactly when the click budget is shorter than the cascade is
deep — instance 7 at horizon 6 is the sharpest case here: <code>:coapp</code>
reaches all three wants with probability 0.853 while every one of the 21 linear
extensions reaches them with probability 0.</p>
<p><b><code>:inter</code> tracks <code>:list</code>, not <code>:coapp</code>.</b>
Choosing one frontier pattern uniformly is still choosing one. The contrast that
matters is one-at-a-time against several-at-once, not which one-at-a-time rule
you use.</p>
<p><b>Instance 5 never reaches its wants at any horizon</b>, because one want has
no producer: the pattern that would produce it is in the library and cannot be
cited. That is a finding about the library rather than about the instance, and it
is the kind PROOF-2a clause T calls a typed exclusion — the record should say
<i>what would make this target feasible</i>, and here the answer is a two-character
edit to a header line in 28 files.</p>
<p><b>No conflicts appeared.</b> In every wide state the enabled patterns were
compatible: no pattern's <code>:produces</code> intersects another's
<code>:forbids</code>. These three cascades therefore do not exercise the case
where co-application would be unsound, and a kernel choice made on this evidence
alone would not be tested against it.</p>
</section>
<footer>
Cascades and page by claude-1, 2026-09-24, at Joe's request via claude-10.
Kernel prototype <code>kernels.clj</code> by claude-10. Mission
<code>M-futon-seams.md</code> records a conversation with Rob
(<code>@facadebootstrap</code>). Patterns from <code>futon3/library</code>.
Run them yourself:
<code>bb holes/labs/M-futon-seams/proto/kernels.clj 0.8 6 holes/labs/M-futon-seams/proto/instance-4.edn</code>
</footer>
<script>{js}</script>
</body></html>
"""

if __name__ == "__main__":
    main()
