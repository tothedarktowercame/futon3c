#!/usr/bin/env python3
"""pattern_component_svg.py — draw the pattern library's giant component as
one large SVG cascade, from current data.

  pattern_component_svg.py OUT.html [--anchor 象/诺必践]
                           [--library DIR] [--batches DIR]

The component is named by a pattern it must contain (--anchor), not by being
the largest, and is taken over the links mined_pattern_graph.py counts as
strong: @why, @how, co-cited, rejected-beside. The weak mined kinds
(co-rejected, next-in-session) are left out, as in that script's
giant_without_weak_edges.

Layout: column = hops from the anchor (breadth-first over the undirected
links), row order by barycentre sweeps so a pattern sits near the patterns
it links to. Arrows only on @why (pattern -> what it follows from) and @how
(pattern -> pattern its recipe cites). Everything is recomputed from the
flexiarg library and the operator-turn analyses on each run; nothing is read
from a stored graph file.
"""
import argparse
import collections
import datetime
import hashlib
import html
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import mined_pattern_graph as mpg  # noqa: E402

KINDS = ["why", "how", "co-cited", "rejected-beside"]
COLOUR = {"why": "#333333", "how": "#2b6cb0", "co-cited": "#2f855a", "rejected-beside": "#c05621"}
NS_FILL = ["#f7f3e8", "#eef4fb", "#f1f8ef", "#fbf0ec", "#f4effa", "#eef8f7", "#fbf6e6", "#f3f3f3"]

CW, BW, RH, BH, TOP, LEFT = 300, 250, 17, 14, 70, 20


def esc(s):
    return html.escape(str(s), quote=True)


def build(library, batches, anchor):
    ids = mpg.library_ids(library)
    if anchor not in ids:
        sys.exit(f"anchor {anchor} is not a library pattern")
    mined, records = mpg.mined_edges(batches, ids)
    edges = {"why": mpg.why_edges(ids), "how": mpg.how_edges(ids),
             "co-cited": mined["co-cited"], "rejected-beside": mined["rejected-beside"]}
    links = {}
    for kind in KINDS:
        for a, b, _ in edges[kind]:
            if a == b:
                continue
            key = (a, b, kind) if kind in ("why", "how") else (min(a, b), max(a, b), kind)
            links[key] = links.get(key, 0) + 1
    nbr = collections.defaultdict(set)
    for a, b, _ in links:
        nbr[a].add(b)
        nbr[b].add(a)
    hop = {anchor: 0}
    queue = collections.deque([anchor])
    while queue:
        n = queue.popleft()
        for m in sorted(nbr[n]):
            if m not in hop:
                hop[m] = hop[n] + 1
                queue.append(m)
    comp_links = {k: v for k, v in links.items() if k[0] in hop}
    return ids, records, hop, nbr, comp_links


def order(hop, nbr):
    cols = collections.defaultdict(list)
    for n in sorted(hop):
        cols[hop[n]].append(n)
    for sweep in range(8):
        pos = {n: i / max(1, len(cols[hop[n]])) for d in cols for i, n in enumerate(cols[d])}
        seq = sorted(cols) if sweep % 2 == 0 else sorted(cols, reverse=True)
        for d in seq:
            side = d - 1 if sweep % 2 == 0 else d + 1

            def bary(n):
                xs = [pos[m] for m in nbr[n] if hop.get(m) in (side, d) and m != n]
                return sum(xs) / len(xs) if xs else pos[n]
            cols[d].sort(key=bary)
    return cols


def svg(hop, nbr, links, cols, anchor):
    height = TOP + RH * max(len(c) for c in cols.values()) + 40
    width = LEFT + CW * len(cols) + 40
    at = {}
    for d, col in cols.items():
        y0 = TOP + (RH * (max(len(c) for c in cols.values()) - len(col))) / 2
        for i, n in enumerate(col):
            at[n] = (LEFT + d * CW, y0 + i * RH)
    namespaces = sorted({n.split("/")[0] for n in hop})
    fill = {ns: NS_FILL[i % len(NS_FILL)] for i, ns in enumerate(namespaces)}
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height:.0f}" '
           f'font-family="sans-serif" font-size="10">',
           '<defs>' + "".join(
               f'<marker id="arr-{k}" viewBox="0 0 6 6" refX="6" refY="3" markerWidth="5" markerHeight="5" '
               f'orient="auto"><path d="M0,0 L6,3 L0,6 z" fill="{COLOUR[k]}"/></marker>'
               for k in ("why", "how")) + '</defs>']
    for d, col in sorted(cols.items()):
        out.append(f'<text x="{LEFT + d * CW}" y="{TOP - 30}" font-size="13" font-weight="bold">'
                   f'hop {d}</text><text x="{LEFT + d * CW}" y="{TOP - 15}" fill="#666">'
                   f'{len(col)} patterns</text>')
    for kind in reversed(KINDS):
        for (a, b, k), count in sorted(links.items()):
            if k != kind:
                continue
            (xa, ya), (xb, yb) = at[a], at[b]
            ya, yb = ya + BH / 2, yb + BH / 2
            if hop[a] == hop[b]:
                x = xa + BW
                bend = x + 20 + min(60, abs(ya - yb) / 8)
                d = f"M{x:.0f},{ya:.0f} C{bend:.0f},{ya:.0f} {bend:.0f},{yb:.0f} {x:.0f},{yb:.0f}"
            else:
                if xa > xb:
                    x1, x2 = xa, xb + BW
                else:
                    x1, x2 = xa + BW, xb
                mx = (x1 + x2) / 2
                d = f"M{x1:.0f},{ya:.0f} C{mx:.0f},{ya:.0f} {mx:.0f},{yb:.0f} {x2:.0f},{yb:.0f}"
            marker = f' marker-end="url(#arr-{k})"' if k in ("why", "how") else ""
            dash = ' stroke-dasharray="4,2"' if k == "how" else ""
            op = 0.55 if k in ("why", "how") else 0.3
            out.append(f'<path d="{d}" fill="none" stroke="{COLOUR[k]}" stroke-opacity="{op}" '
                       f'stroke-width="{min(3, 0.6 + 0.3 * count):.1f}"{dash}{marker}>'
                       f'<title>{esc(a)} {k} {esc(b)} ({count})</title></path>')
    for n, (x, y) in sorted(at.items()):
        ns = n.split("/")[0]
        xiang = ns == "象"
        stroke = "#d6336c" if xiang else "#999"
        sw = 2.5 if n == anchor else (1.5 if xiang else 0.6)
        label = n if len(n) <= 40 else n[:39] + "…"
        out.append(f'<g><title>{esc(n)} — hop {hop[n]}, {len(nbr[n])} linked patterns</title>'
                   f'<rect x="{x}" y="{y:.0f}" width="{BW}" height="{BH}" rx="3" '
                   f'fill="{"#fde2ec" if xiang else fill[ns]}" stroke="{stroke}" stroke-width="{sw}"/>'
                   f'<text x="{x + 4}" y="{y + 10.5:.0f}"{" font-weight=\"bold\"" if xiang else ""}>'
                   f'{esc(label)}</text></g>')
    out.append("</svg>")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--anchor", default="象/诺必践")
    ap.add_argument("--library", default="/home/joe/code/futon3/library")
    ap.add_argument("--batches", default="/home/joe/code/storage/operator-turns/batches")
    args = ap.parse_args()
    ids, records, hop, nbr, links = build(args.library, args.batches, args.anchor)
    cols = order(hop, nbr)
    members = sorted(hop)
    digest = hashlib.sha256("\n".join(members).encode()).hexdigest()[:16]
    by_kind = collections.Counter(k for _, _, k in links)
    by_ns = collections.Counter(n.split("/")[0] for n in members)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    legend = " ".join(f'<span style="color:{COLOUR[k]}">■ {k} ({by_kind[k]})</span>' for k in KINDS)
    page = f"""<!doctype html><html><head><meta charset="utf-8">
<title>Pattern library: the component containing {esc(args.anchor)}</title>
<style>body{{font-family:sans-serif;margin:1.5em;max-width:none}} p,ul,pre{{max-width:60em}}
pre{{background:#f5f5f5;padding:.6em}} .fig{{overflow:auto;border:1px solid #ddd}}</style></head><body>
<h1>The component containing {esc(args.anchor)}</h1>
<p><b>{len(members)}</b> of {len(ids)} library patterns, joined by {len(links)} links
drawn from {records} operator-turn analyses and the library's own @why and @how lines.
Member-list hash <code>{digest}</code> (sha256 of the sorted ids, first 16 hex).
Generated {stamp}. This is a one-off rendering: the next run on newer data may differ.</p>
<p>Links: {legend}. Arrows on @why (a pattern points to what it follows from)
and @how (a pattern points to a pattern its recipe cites, dashed). Mined links are
undirected. Columns are hops from the anchor; 象 patterns are pink, and the anchor
has a thick border. Hover a box or a line for its full id.</p>
<p>By namespace: {", ".join(f"{esc(ns)} {c}" for ns, c in by_ns.most_common())}.</p>
<h2>Recreate from current data</h2>
<pre>cd /home/joe/code/futon3c
python3 scripts/pattern_component_svg.py OUT.html --anchor {esc(args.anchor)}
# inputs: futon3/library/**/*.flexiarg and storage/operator-turns/batches/*/*.analysis.json
# component summary for every link kind: python3 scripts/mined_pattern_graph.py --out FILE.json</pre>
<p>Weak mined kinds (turned down together, cited in consecutive turns) are excluded;
including them grows the component to about 770. "Giant" here is the component
named by its anchor, not whichever component is largest.</p>
<div class="fig">{svg(hop, nbr, links, cols, args.anchor)}</div>
</body></html>"""
    with open(args.out, "w") as fh:
        fh.write(page)
    print(f"{len(members)} patterns, {len(links)} links, {len(cols)} hops, hash {digest} -> {args.out}")


if __name__ == "__main__":
    main()
