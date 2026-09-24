#!/usr/bin/env python3
"""seams_wiring.py — draw a token-flow wiring diagram from its EDN.

Each box carries typed ports: the tokens it needs along the top, the tokens
it produces along the bottom, and any token it FORBIDS as a filled red
inhibitor among the inputs. An edge runs from a producer's output port to a
consumer's input port; both ends are labelled with the token, so what a node
takes in and hands on is readable off the box rather than hidden inside it.

Depth runs downward rather than left to right. That is a measured constraint,
not a preference: a left-to-right flow of eight depths needs about 1900px,
and at the width where its labels match the body text it cannot fit any
margin. Ports keep their meaning either way.

A dangling output -- produced, needed by nobody, wanted by nobody -- ends in
an open amber stub marked "unused". An unfed want is an amber open port on
the want box. Both are findings about the cascade, so they are drawn.
"""
import html

C = {"full": "#1b6b3a", "hungry": "#a8791d", "port": "#555",
     "dev": "#b8431f", "rule": "#8a8578", "tok": "#667"}


def short(x):
    x = str(x).lstrip(":")
    return x.split("/", 1)[1] if "/" in x else x


def wrap_id(text, maxchars=21):
    """Break a name at '-' boundaries; see seams_page.wrap_id."""
    if len(text) <= maxchars:
        return [text]
    parts, lines, cur = text.split("-"), [], ""
    for i, part in enumerate(parts):
        piece = part + ("-" if i < len(parts) - 1 else "")
        if cur and len(cur) + len(piece) > maxchars:
            lines.append(cur); cur = piece
        else:
            cur += piece
    if cur:
        lines.append(cur)
    return lines[:3]


def _abbrev(t, n=11):
    t = short(t)
    return t if len(t) <= n else t[:n - 1] + "…"


def svg(w):
    nodes = {n["id"]: n for n in w["nodes"]}
    edges = w.get("edges") or []
    carries = [e for e in edges if e.get("kind") != "inhibits"]

    # Seed from the token flow's own roots, not only from have-port: a cascade
    # whose :initial is empty gives have-port no outgoing edge, and seeding
    # from it alone left every node at depth 0.
    fed = {e["to"]["node"] for e in carries}
    depth = {"have-port": 0}
    for n in w["nodes"]:
        if n["id"] not in fed and n["id"] not in ("have-port", "want-port") \
                and n.get("role") != "deviation":
            depth[n["id"]] = 1
    for _ in range(len(nodes) + 1):
        for e in carries:
            a, b = e["from"]["node"], e["to"]["node"]
            if a in depth:
                depth[b] = max(depth.get(b, 0), depth[a] + 1)
    dev = [n for n in w["nodes"] if n.get("role") == "deviation"]
    for n in dev:
        depth.setdefault(n["id"], 1)
    spine = [n for n in w["nodes"] if n.get("role") != "deviation"]
    maxd = max(depth.get(n["id"], 0) for n in spine)
    if maxd == 0:
        raise SystemExit("seams_wiring: no depth propagated — check the node id form")
    depth["want-port"] = maxd
    layers = {}
    for n in spine:
        layers.setdefault(depth.get(n["id"], 0), []).append(n["id"])

    COLW, ROWH, BW, BH = 205, 146, 186, 96
    lanes = max(len(v) for v in layers.values())
    width = COLW * lanes + 46
    height = ROWH * (maxd + 1) + 40 + (ROWH if dev else 0)
    pos = {}
    for d, ids in sorted(layers.items()):
        left = (COLW * lanes - COLW * len(ids)) / 2
        for i, nid in enumerate(sorted(ids)):
            pos[nid] = (23 + left + i * COLW, 20 + d * ROWH)
    for i, n in enumerate(dev):
        pos[n["id"]] = (23 + (COLW * lanes - COLW) / 2 + i * COLW,
                        20 + (maxd + 1) * ROWH)

    def port_xy(nid, token, side):
        n = nodes[nid]
        keys = sorted(n.get("in") or []) + sorted(n.get("forbids") or []) \
            if side == "in" else sorted(n.get("out") or [])
        if token not in keys:
            return None
        x, y = pos[nid]
        k = keys.index(token)
        step = BW / (len(keys) + 1)
        return (x + step * (k + 1), y if side == "in" else y + BH)

    out = [f'<svg viewBox="0 0 {width} {height}" class="wiring" role="img" '
           f'aria-label="token-flow wiring for instance {w.get("instance")}">']
    out.append('<defs><marker id="wa" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="6" '
               'markerHeight="6" orient="auto"><path d="M0 0 L8 4 L0 8 z" fill="#8a8578"/>'
               '</marker><marker id="wd" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="6" '
               'markerHeight="6" orient="auto"><path d="M0 0 L8 4 L0 8 z" fill="#b8431f"/>'
               '</marker></defs>')

    for e in edges:
        a, b = e["from"]["node"], e["to"]["node"]
        if a not in pos or b not in pos:
            continue
        p1 = port_xy(a, e["token"], "out")
        p2 = port_xy(b, e["token"], "in")
        if not p1 or not p2:
            continue
        inhibit = e.get("kind") == "inhibits"
        colour = C["dev"] if inhibit else C["rule"]
        dash = ' stroke-dasharray="4 3"' if inhibit else ""
        mid = (p1[1] + p2[1]) / 2
        out.append(
            f'<path d="M{p1[0]} {p1[1]} C{p1[0]} {mid} {p2[0]} {mid} {p2[0]} {p2[1]}" '
            f'fill="none" stroke="{colour}" stroke-width="1.3"{dash} '
            f'marker-end="url(#{"wd" if inhibit else "wa"})">'
            f'<title>{"forbids" if inhibit else "carries"} {html.escape(short(e["token"]))}'
            f' — {html.escape(short(a))} → {html.escape(short(b))}</title></path>')

    dangling = {(d["node"], d["token"]) for d in (w.get("dangling-outputs") or [])}
    unfed = {u["want"] for u in (w.get("unfed-wants") or [])}

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
        out.append(
            f'<g class="wnode"><title>{html.escape(str(tip))}</title>'
            f'<rect x="{x}" y="{y}" width="{BW}" height="{BH}" rx="3" fill="{fill}" '
            f'stroke="{stroke}" stroke-width="1.3"{dash}/>'
            + "".join(
                f'<text x="{x+9}" y="{y+22+i*15}" class="wid">{html.escape(l)}</text>'
                for i, l in enumerate(wrap_id(short(nid)))))
        base = 22 + 15 * len(wrap_id(short(nid)))
        words, line, ln = str(n.get("form", "")).split(), "", 0
        for word in words:
            if len(line) + len(word) > 30:
                out.append(f'<text x="{x+9}" y="{y+base+ln*10}" class="wform">'
                           f'{html.escape(line)}</text>')
                line, ln = word, ln + 1
                if ln >= 2:
                    break
            else:
                line = (line + " " + word).strip()
        if ln < 2 and line:
            out.append(f'<text x="{x+9}" y="{y+base+ln*10}" class="wform">'
                       f'{html.escape(line)}</text>')
        lic = n.get("licensed-by")
        if lic and "deviation/none" not in str(lic):
            lictxt = str(lic).lstrip(":").split("/", 1)[0] + "/"
            out.append(f'<text x="{x+9}" y="{y+BH-9}" class="wlic">⊢ '
                       f'{html.escape(lictxt[:24] + ("…" if len(lictxt) > 24 else ""))}'
                       f'</text>')
        elif role == "deviation":
            out.append(f'<text x="{x+9}" y="{y+BH-9}" class="wdev">'
                       f'licensed by nothing in the cascade</text>')
        out.append("</g>")

        ins = sorted(n.get("in") or []) + sorted(n.get("forbids") or [])
        forb = set(n.get("forbids") or [])
        for k, t in enumerate(ins):
            px = x + BW / (len(ins) + 1) * (k + 1)
            bad = t in forb
            openport = nid == "want-port" and t in unfed
            out.append(
                (f'<circle cx="{px}" cy="{y}" r="3.4" fill="{C["dev"]}">'
                 if bad else
                 f'<rect x="{px-3}" y="{y-3}" width="6" height="6" '
                 f'fill="{"#fffdf5" if openport else "#fff"}" '
                 f'stroke="{C["hungry"] if openport else C["port"]}" stroke-width="1.1"/>')
                + f'<title>{"forbids" if bad else "needs"} {html.escape(short(t))}'
                  f'{" — UNFED: nothing produces it" if openport else ""}</title>'
                + (f'</circle>' if bad else ''))
            # stagger: two ports 60px apart hold ~8 characters each before
            # their labels touch, and these names are longer than that
            out.append(f'<text x="{px}" y="{y - (7 if k % 2 == 0 else 17)}" '
                       f'class="wtok" text-anchor="middle">'
                       f'{html.escape(_abbrev(t))}</text>')
        outs = sorted(n.get("out") or [])
        for k, t in enumerate(outs):
            px = x + BW / (len(outs) + 1) * (k + 1)
            loose = (nid, t) in dangling
            out.append(
                f'<rect x="{px-3}" y="{y+BH-3}" width="6" height="6" '
                f'fill="{"#fffdf5" if loose else "#fff"}" '
                f'stroke="{C["hungry"] if loose else C["port"]}" stroke-width="1.1">'
                f'<title>produces {html.escape(short(t))}'
                f'{" — UNUSED: no node needs it and it is not a want" if loose else ""}'
                f'</title></rect>')
            out.append(f'<text x="{px}" y="{y+BH + (12 if k % 2 == 0 else 22)}" '
                       f'class="wtok" text-anchor="middle">'
                       f'{html.escape(_abbrev(t))}</text>')
            if loose:
                ly = y + BH + (24 if k % 2 == 0 else 34)
                out.append(f'<path d="M{px} {y+BH+3} l0 {ly-y-BH-11}" '
                           f'stroke="{C["hungry"]}" stroke-width="1.2" '
                           f'stroke-dasharray="2 2"/>'
                           f'<text x="{px}" y="{ly}" class="wloose" '
                           f'text-anchor="middle">unused</text>')
    out.append("</svg>")
    return "".join(out)


CSS = """
.wiring { max-width:100%; height:auto; }
.wid { font-size:12.5px; fill:#333; font-weight:600; font-family:ui-monospace,Menlo,monospace; }
.wform { font-size:8.5px; fill:#666; }
.wlic { font-size:8.5px; fill:#1b6b3a; font-family:ui-monospace,Menlo,monospace; }
.wdev { font-size:8.5px; fill:#b8431f; font-family:ui-monospace,Menlo,monospace; }
.wtok { font-size:7.5px; fill:#667; font-family:ui-monospace,Menlo,monospace; }
.wloose { font-size:7.5px; fill:#a8791d; font-family:ui-monospace,Menlo,monospace; }
.wnode { cursor:help; }
"""
