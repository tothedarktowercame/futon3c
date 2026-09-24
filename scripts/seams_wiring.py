#!/usr/bin/env python3
"""seams_wiring.py — draw a wiring diagram from its EDN.

Boundary ports left and right, construction nodes between them, laid out by
longest path from the have-port. A node is drawn by its satiety: full carries
a witness and is solid; hungry carries what it owes and is dashed. A node
whose role is :deviation sits below the path in red — real work the plan has
no node for, which is the shape of a plan-versus-realised finding.
"""
import html

C = {"full": "#1b6b3a", "hungry": "#a8791d", "port": "#555",
     "dev": "#b8431f", "rule": "#8a8578"}


def short(x):
    x = str(x).lstrip(":")
    return x.split("/", 1)[1] if "/" in x else x


def svg(w):
    nodes = {n["id"]: n for n in w["nodes"]}
    edges = w["hyperedges"]
    outs = {}
    for e in edges:
        a = [x["node"] for x in e["ends"] if x["role"] == "from"][0]
        b = [x["node"] for x in e["ends"] if x["role"] == "to"][0]
        outs.setdefault(a, []).append((b, e))

    # longest path from have-port, ignoring deviation edges for the spine
    depth = {"have-port": 0}
    for _ in range(len(nodes) + 1):
        for a, lst in outs.items():
            for b, e in lst:
                if a in depth and e.get("kind") == "composes":
                    depth[b] = max(depth.get(b, 0), depth[a] + 1)
    dev = [n for n in w["nodes"] if n.get("role") == "deviation"]
    for n in dev:
        depth[n["id"]] = 1

    spine = [n for n in w["nodes"] if n.get("role") != "deviation"]
    maxd = max(depth.get(n["id"], 0) for n in spine)
    if maxd == 0:
        raise SystemExit("seams_wiring: no depth propagated — check the node id form")
    depth["want-port"] = maxd
    layers = {}
    for n in spine:
        layers.setdefault(depth.get(n["id"], 0), []).append(n["id"])

    # Laid out depth DOWNWARD, like the cascades: a margin column can hold a
    # tall narrow diagram at full size, and cannot hold a wide one at any size
    # that keeps its labels readable.
    COLW, ROWH, BW, BH = 205, 100, 186, 76
    lanes = max(len(v) for v in layers.values())
    width = COLW * lanes + 46
    height = ROWH * (maxd + 1) + 34 + (ROWH if dev else 0)
    pos = {}
    for d, ids in sorted(layers.items()):
        left = (COLW * lanes - COLW * len(ids)) / 2
        for i, nid in enumerate(sorted(ids)):
            pos[nid] = (23 + left + i * COLW, 18 + d * ROWH)
    for i, n in enumerate(dev):
        pos[n["id"]] = (23 + (COLW * lanes - COLW) / 2 + i * COLW,
                        18 + (maxd + 1) * ROWH)

    out = [f'<svg viewBox="0 0 {width} {height}" class="wiring" role="img" '
           f'aria-label="wiring diagram for instance {w.get("instance")}">']
    out.append('<defs><marker id="wa" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="7" '
               'markerHeight="7" orient="auto"><path d="M0 0 L8 4 L0 8 z" fill="#8a8578"/>'
               '</marker><marker id="wd" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="7" '
               'markerHeight="7" orient="auto"><path d="M0 0 L8 4 L0 8 z" fill="#b8431f"/>'
               '</marker></defs>')

    for a, lst in sorted(outs.items()):
        for b, e in lst:
            if a not in pos or b not in pos:
                continue
            x1, y1 = pos[a][0] + BW / 2, pos[a][1] + BH
            x2, y2 = pos[b][0] + BW / 2, pos[b][1]
            mid = (y1 + y2) / 2
            kind = e.get("kind", "composes")
            colour = C["dev"] if kind in ("deviates", "partly-discharges") else C["rule"]
            dash = ' stroke-dasharray="5 4"' if kind != "composes" else ""
            if e.get("kind-in-cascade") == "jointly-with":
                dash = ' stroke-dasharray="5 4"'
            marker = "wd" if colour == C["dev"] else "wa"
            out.append(
                f'<path d="M{x1} {y1} C{x1} {mid} {x2} {mid} {x2} {y2}" fill="none" '
                f'stroke="{colour}" stroke-width="1.4"{dash} marker-end="url(#{marker})">'
                f'<title>{html.escape(kind)} — {html.escape(e.get("via", ""))}</title></path>')

    for nid, (x, y) in sorted(pos.items()):
        n = nodes[nid]
        role = n.get("role")
        full = n.get("satiety") == "full"
        if role == "deviation":
            stroke, fill = C["dev"], "#fdf1ec"
        elif role == "scope":
            stroke, fill = C["port"], "#f4f2ea"
        else:
            stroke, fill = (C["full"], "#fff") if full else (C["hungry"], "#fffdf5")
        dash = "" if full or role == "scope" else ' stroke-dasharray="4 3"'
        tip = n.get("witness") or n.get("owed") or n.get("via", "")
        lic = n.get("licensed-by")
        form = n.get("form", "")
        out.append(
            f'<g class="wnode"><title>{html.escape(str(tip))}</title>'
            f'<rect x="{x}" y="{y}" width="{BW}" height="{BH}" rx="3" fill="{fill}" '
            f'stroke="{stroke}" stroke-width="1.3"{dash}/>'
            f'<text x="{x+9}" y="{y+17}" class="wid">{html.escape(short(nid))}</text>')
        words, line, ln = form.split(), "", 0
        for word in words:
            if len(line) + len(word) > 26:
                out.append(f'<text x="{x+9}" y="{y+31+ln*11}" class="wform">'
                           f'{html.escape(line)}</text>')
                line, ln = word, ln + 1
                if ln >= 3:
                    break
            else:
                line = (line + " " + word).strip()
        if ln < 3 and line:
            out.append(f'<text x="{x+9}" y="{y+31+ln*11}" class="wform">'
                       f'{html.escape(line)}{"…" if len(words) > 0 and ln == 2 else ""}</text>')
        if lic and lic != ":deviation/none" and str(lic) != "deviation/none":
            out.append(f'<text x="{x+9}" y="{y+BH-8}" class="wlic">⊢ '
                       f'{html.escape(short(lic))}</text>')
        elif role == "deviation":
            out.append(f'<text x="{x+9}" y="{y+BH-8}" class="wdev">'
                       f'licensed by nothing in the cascade</text>')
        out.append("</g>")
    out.append("</svg>")
    return "".join(out)


CSS = """
.wiring { max-width:100%; height:auto; }
.wid { font-size:12.5px; fill:#333; font-weight:600; font-family:ui-monospace,Menlo,monospace; }
.wform { font-size:9px; fill:#666; }
.wlic { font-size:8.5px; fill:#1b6b3a; font-family:ui-monospace,Menlo,monospace; }
.wdev { font-size:8.5px; fill:#b8431f; font-family:ui-monospace,Menlo,monospace; }
.wnode { cursor:help; }
"""
