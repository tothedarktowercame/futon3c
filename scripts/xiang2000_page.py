#!/usr/bin/env python3
"""xiang2000_page.py — M-象-2000 v2 artefacts as one page for Joe's VERIFY.

  xiang2000_page.py OUT.html

Reads the lab's EDN (cascade v1 and v2, typed hole v1 and v2, wiring v2, the
build plan as data) and draws four figures from them, so nothing on the page
is typed in twice: what changed in the cascade, the typed hole by satiety and
evidence rung, the build plan by what each packet waits on, and the
acceptance case by which rows a query can answer. The full token-flow wiring
is an appendix as a table of each pattern's ports. (seams_wiring.py, the
existing renderer, was tried first: it does not wrap CJK text, and at 35 nodes
the boxes overprint.)

Deterministic for fixed inputs; nothing is derived from the clock except the
generation stamp in the footer.
"""
import html, json, os, subprocess, sys, datetime

LAB = "/home/joe/code/futon3c/holes/labs/M-象-2000"


def edn(path):
    r = subprocess.run(
        ["bb", "-e",
         '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
         f'(print (j/generate-string (edn/read-string (slurp "{path}"))))'],
        capture_output=True, text=True)
    if r.returncode:
        sys.exit(f"xiang2000_page: cannot read {path}: {r.stderr.strip()}")
    return json.loads(r.stdout)


def esc(s):
    return html.escape(str(s), quote=True)


# English glosses. The patterns and tokens are written in Chinese; the gloss is
# a reading aid, not a translation of record (the flexiarg files are that).
PGLOSS = {
    "translation/route-the-untranslatable": "route what cannot be translated",
    "agency/delivery-receipt": "every send gets a receipt",
    "social/explicit-exit-over-abandonment": "exit explicitly, don't drift",
    "inbox-zero/gate-fails-loudly": "checks report changes, loudly",
    "象/言即行": "speech is action: type on the envelope",
    "象/象不忘": "cite the record, don't restate it",
    "象/行有定名": "every act has its own minted id",
    "象/双时并记": "valid time and system time",
    "象/诺必践": "a promise is a debt (five outcomes)",
    "象/两种规格": "delivered is not done",
    "象/收回亦是行": "withdrawal is an act",
    "象/名分有据": "authority on record: scope, time, chain",
    "象/释义非授": "an interpretation has no effect",
    "象/要约与接受": "offer and acceptance",
    "agency/state-atomicity": "state transitions all-or-nothing",
    "social/idempotent-handoff": "re-sent handoffs are no-ops",
    "象/视图出于史": "views are compiled from history",
    "象/以史为据": "constraints are relations over history",
    "象/明言假设": "state the assumptions you filled in",
    "translation/declare-what-is-lost": "declare what a translation drops",
    "象/翻译契约": "the translation contract",
    "象/覆水难收": "reconstruct / replay / compensate",
    "象/三层规格": "three levels of specification",
    "象/裁亦有失": "every ruling gets a HOWEVER",
    "象/提交非生效": "committed is not live",
    "象/制度有时": "rules have versions and intervals",
    "象/措施随案": "measures belong to their incident",
    "象/答必真且中的": "answers true and responsive",
    "象/欠与被欠": "owed and owing, as of T",
    "象/限定随论": "qualifications travel with claims",
    "象/引非读": "a pointer is not a reading",
    "象/每段自验": "each phase reruns the acceptance case",
    "象/同构方为同": "match by structure, not wording",
}
TGLOSS = {
    "类型化退回": "typed refusal", "行为类型": "act types", "引用": "references",
    "投递回执": "delivery receipts", "显式退出承诺": "explicit exit",
    "闸门响亮": "loud gates", "行为定名": "minted act ids", "双时": "two time axes",
    "承诺落记": "promise records", "承诺终局分记": "five promise outcomes",
    "送达与办成分记": "delivered ≠ done", "撤销类型": "withdrawal type",
    "存续可查": "in force as of T", "授权来源": "authority source",
    "解读效力分记": "interpretation ≠ effect", "协议记录": "agreement records",
    "状态转换原子": "atomic transitions", "交接幂等": "idempotent handoff",
    "历史完备": "complete history", "约束即关系": "constraints as relations",
    "假设明言": "stated assumptions", "所弃成文": "declared losses",
    "级联可判错": "refutable translation", "三分回溯": "three kinds of rewind",
    "补救清单": "compensation list", "三层规格成文": "three-level rules",
    "规则带失败方式": "rules carry HOWEVER", "三时分记": "adopted/committed/live",
    "制度版本生效区间": "rule versions & intervals", "清结证明": "clearance record",
    "回答声明总体": "answers state population", "回答真实切题": "true & responsive",
    "承诺可追讨": "promises collectable", "行权有据": "acts authorised",
    "成效可验": "effect checkable", "建造可自验": "build checks itself",
    "限定随附": "qualifications attached", "引用分等": "evidence rungs",
    "证实可分": "attestations separable", "正确性条件生成": "generated checks",
    "答后知": "asker then knows", "输入侧学会记录": "understood ≠ received",
    "反事实世界模型": "counterfactual model",
}

INK, MUTED, RULE = "#1d1d1b", "#6b6860", "#cfcabb"
NEW, CHG, SAME = "#1b6b3a", "#b07a12", "#8a8578"
HUNGRY, PARTIAL, FULL = "#b8431f", "#c9a227", "#1b6b3a"


def wrap(text, width):
    """Split TEXT into lines of at most WIDTH characters, at spaces."""
    lines, cur = [], ""
    for w in str(text).split():
        if cur and len(cur) + 1 + len(w) > width:
            lines.append(cur)
            cur = w
        else:
            cur = (cur + " " + w).strip()
    if cur:
        lines.append(cur)
    return lines


def short(pid):
    return pid.split("/", 1)[1] if "/" in pid else pid


def layer(nodes, edges):
    """Longest-path depth over EDGES (a, b), then two barycentre sweeps so a
    node sits near the nodes it is joined to. Returns {node: (depth, order)}."""
    depth = {n: 0 for n in nodes}
    for _ in range(len(nodes) + 1):
        for a, b in edges:
            if a in depth and b in depth:
                depth[b] = max(depth[b], depth[a] + 1)
    cols = {}
    for n in sorted(nodes):
        cols.setdefault(depth[n], []).append(n)
    preds = {n: [a for a, b in edges if b == n] for n in nodes}
    succs = {n: [b for a, b in edges if a == n] for n in nodes}
    for sweep in range(4):
        order = {n: i for d in cols for i, n in enumerate(cols[d])}
        for d in sorted(cols):
            nbr = preds if sweep % 2 == 0 else succs
            def bary(n):
                xs = [order[m] for m in nbr[n] if m in order]
                return sum(xs) / len(xs) if xs else order[n]
            cols[d].sort(key=bary)
    return {n: (depth[n], cols[depth[n]].index(n)) for n in nodes}, cols


# ---------------------------------------------------------------- figure 1
def cascade_status(v1, v2):
    st = {}
    for pid, p in v2["patterns"].items():
        q = v1["patterns"].get(pid)
        if q is None:
            st[pid] = "new"
        elif (sorted(q["guard"]["needs"]) != sorted(p["guard"]["needs"])
              or sorted(q["produces"]) != sorted(p["produces"])):
            st[pid] = "changed"
        else:
            st[pid] = "same"
    return st


def fig_cascade(v1, v2):
    pats = v2["patterns"]
    prod = {}
    for pid, p in pats.items():
        for t in p["produces"]:
            prod.setdefault(t, []).append(pid)
    edges, etok = [], {}
    for pid, p in pats.items():
        for t in p["guard"]["needs"]:
            for s in prod.get(t, []):
                edges.append((s, pid))
                etok.setdefault((s, pid), []).append(t)
    want = set(v2["want"])
    for t in want:
        for s in prod.get(t, []):
            edges.append((s, "WANT"))
            etok.setdefault((s, "WANT"), []).append(t)
    v1edges = set()
    v1prod = {}
    for pid, p in v1["patterns"].items():
        for t in p["produces"]:
            v1prod.setdefault(t, []).append(pid)
    for pid, p in v1["patterns"].items():
        for t in p["guard"]["needs"]:
            for s in v1prod.get(t, []):
                v1edges.add((s, pid, t))
    for t in v1["want"]:
        for s in v1prod.get(t, []):
            v1edges.add((s, "WANT", t))

    nodes = list(pats) + ["WANT"]
    pos, cols = layer(nodes, edges)
    # the want box goes one column past the deepest pattern
    maxd = max(d for n, (d, _) in pos.items() if n != "WANT")
    cols[pos["WANT"][0]].remove("WANT")
    cols.setdefault(maxd + 1, []).append("WANT")
    pos["WANT"] = (maxd + 1, 0)
    st = cascade_status(v1, v2)

    COLW, BW, BH, ROWH = 196, 172, 54, 66
    tallest = max(len(v) for v in cols.values())
    W = COLW * (maxd + 2) + 40
    H = ROWH * tallest + 40
    xy = {}
    for d, ids in cols.items():
        top = (H - ROWH * len(ids)) / 2
        for i, n in enumerate(ids):
            h = BH if n != "WANT" else ROWH * 3
            xy[n] = (20 + d * COLW, top + i * ROWH + (ROWH - BH) / 2 if n != "WANT" else H / 2 - h / 2, h)
    out = [f'<svg viewBox="0 0 {W} {H}" width="{W}" height="{H}" xmlns="http://www.w3.org/2000/svg" '
           f'font-family="system-ui,sans-serif" role="img" aria-label="cascade v2 token flow">']
    for (a, b) in sorted(set(edges)):
        ax, ay, ah = xy[a]
        bx, by, bh = xy[b]
        x1, y1 = ax + BW, ay + ah / 2
        x2, y2 = bx, by + bh / 2
        toks = etok[(a, b)]
        isnew = any((a, b, t) not in v1edges for t in toks)
        col = NEW if isnew else "#b9b4a6"
        wdt = 1.6 if isnew else 1
        mx = (x1 + x2) / 2
        out.append(f'<path d="M{x1:.0f},{y1:.0f} C{mx:.0f},{y1:.0f} {mx:.0f},{y2:.0f} {x2:.0f},{y2:.0f}" '
                   f'fill="none" stroke="{col}" stroke-width="{wdt}" opacity="0.85">'
                   f'<title>{esc(short(a))} → {esc(short(b) if b != "WANT" else "want")}: '
                   f'{esc(", ".join(t + " (" + TGLOSS.get(t, "") + ")" for t in toks))}'
                   f'{" — new in v2" if isnew else ""}</title></path>')
    for n, (x, y, h) in xy.items():
        if n == "WANT":
            ws = sorted(want)
            out.append(f'<rect x="{x}" y="{y:.0f}" width="{BW}" height="{h:.0f}" rx="6" fill="#f6f1e3" stroke="{INK}" stroke-width="1.5"/>')
            out.append(f'<text x="{x + 8}" y="{y + 18:.0f}" font-size="12" font-weight="700" fill="{INK}">want</text>')
            for i, t in enumerate(ws):
                c = NEW if t not in v1["want"] else INK
                out.append(f'<text x="{x + 8}" y="{y + 38 + i * 30:.0f}" font-size="12" fill="{c}">{esc(t)}</text>')
                out.append(f'<text x="{x + 8}" y="{y + 51 + i * 30:.0f}" font-size="9.5" fill="{MUTED}">{esc(TGLOSS.get(t, ""))}</text>')
            continue
        s = st[n]
        stroke = {"new": NEW, "changed": CHG, "same": SAME}[s]
        fill = {"new": "#e8f3ea", "changed": "#fbf0d6", "same": "#ffffff"}[s]
        p = pats[n]
        tip = (f'{n} — {PGLOSS.get(n, "")}\nneeds: {", ".join(sorted(p["guard"]["needs"])) or "—"}'
               f'\nproduces: {", ".join(sorted(p["produces"]))}\n\n{p["receipt"]["reading"]}')
        out.append(f'<g><title>{esc(tip)}</title>'
                   f'<rect x="{x}" y="{y:.0f}" width="{BW}" height="{BH}" rx="5" fill="{fill}" stroke="{stroke}" '
                   f'stroke-width="{2 if s != "same" else 1}"/>'
                   f'<text x="{x + 7}" y="{y + 17:.0f}" font-size="{13 if len(short(n)) < 20 else 10.5}" fill="{INK}">{esc(short(n))}</text>'
                   + "".join(f'<text x="{x + 7}" y="{y + 31 + 11 * i:.0f}" font-size="9.5" fill="{MUTED}">{esc(line)}</text>'
                             for i, line in enumerate(wrap(PGLOSS.get(n, ""), 32)[:2]))
                   + '</g>')
    out.append("</svg>")
    counts = {k: sum(1 for v in st.values() if v == k) for k in ("new", "changed", "same")}
    return "\n".join(out), counts


# ---------------------------------------------------------------- figure 2
GROUPS = [
    ("Foundation", ["类型化退回", "行为类型", "引用", "投递回执", "显式退出承诺", "闸门响亮", "行为定名", "双时"]),
    ("Promises", ["承诺落记", "承诺终局分记", "送达与办成分记", "撤销类型", "存续可查"]),
    ("Authority", ["授权来源", "解读效力分记", "协议记录"]),
    ("History", ["状态转换原子", "交接幂等", "历史完备", "约束即关系", "假设明言", "所弃成文", "级联可判错", "三分回溯", "补救清单"]),
    ("Rules", ["三层规格成文", "规则带失败方式", "三时分记", "制度版本生效区间", "清结证明"]),
    ("Want", ["回答声明总体", "回答真实切题", "承诺可追讨", "行权有据", "成效可验", "建造可自验"]),
    ("Process & library", ["限定随附", "引用分等", "证实可分"]),
    ("Holes (nothing produces these)", ["正确性条件生成", "答后知", "输入侧学会记录", "反事实世界模型"]),
]


def fig_hole(h1, h2, want):
    fills, f1 = h2["fills"], h1["fills"]
    regr = {r["token"]: r for r in h2.get("regrades", [])}
    listed = [t for _, ts in GROUPS for t in ts]
    assert sorted(listed) == sorted(fills), (set(listed) ^ set(fills))
    CW, CH, GAP = 150, 58, 8
    PER = 6
    rows = []
    y = 10
    for g, ts in GROUPS:
        rows.append(("label", g, y))
        y += 22
        for i in range(0, len(ts), PER):
            rows.append(("cells", ts[i:i + PER], y))
            y += CH + GAP
        y += 6
    W = 20 + PER * (CW + GAP)
    H = y + 4
    out = [f'<svg viewBox="0 0 {W} {H}" width="{W}" height="{H}" xmlns="http://www.w3.org/2000/svg" '
           f'font-family="system-ui,sans-serif" role="img" aria-label="typed hole v2">']
    for kind, v, y in rows:
        if kind == "label":
            out.append(f'<text x="12" y="{y + 14}" font-size="11" letter-spacing="0.08em" fill="{MUTED}">{esc(v.upper())}</text>')
            continue
        for i, t in enumerate(v):
            x = 12 + i * (CW + GAP)
            f = fills[t]
            sat, rung = f["satiety"], f.get("rung", "none")
            old = f1.get(t, {}).get("satiety")
            col = {"hungry": HUNGRY, "partial": PARTIAL, "full": FULL}[sat]
            fill = {"hungry": "#ffffff", "partial": "#fbf3d9", "full": "#e3f1e6"}[sat]
            sw = 2.6 if t in want else 1.2
            tip = f'{t} — {TGLOSS.get(t, "")}\nsatiety {sat} (v1: {old or "not in v1"}), rung {rung}'
            if f.get("read-note"):
                tip += "\n\nread: " + f["read-note"]
            tip += "\n\nmissing: " + f.get("missing", "")
            if t in regr:
                tip += "\n\nregraded: " + regr[t]["why"]
            out.append(f'<g><title>{esc(tip)}</title>'
                       f'<rect x="{x}" y="{y}" width="{CW}" height="{CH}" rx="5" fill="{fill}" stroke="{col}" stroke-width="{sw}"/>'
                       f'<text x="{x + 7}" y="{y + 19}" font-size="13" fill="{INK}">{esc(t)}</text>'
                       f'<text x="{x + 7}" y="{y + 34}" font-size="9.5" fill="{MUTED}">{esc(TGLOSS.get(t, "")[:28])}</text>')
            badge = {"named": "N", "read": "R", "witnessed": "W", "none": "–"}[rung]
            out.append(f'<text x="{x + 7}" y="{y + 50}" font-size="10" fill="{col}" font-weight="700">{sat}</text>'
                       f'<text x="{x + CW - 18}" y="{y + 50}" font-size="11" font-weight="700" fill="{INK}">{badge}</text>')
            if old is None:
                out.append(f'<text x="{x + CW - 30}" y="{y + 15}" font-size="9" fill="{NEW}" font-weight="700">v2</text>')
            elif t in regr:
                out.append(f'<text x="{x + CW - 14}" y="{y + 15}" font-size="12" fill="{CHG}" font-weight="700">↻</text>')
            out.append("</g>")
    out.append("</svg>")
    sat = {}
    for f in fills.values():
        sat[f["satiety"]] = sat.get(f["satiety"], 0) + 1
    rungs = {}
    for f in fills.values():
        rungs[f.get("rung", "none")] = rungs.get(f.get("rung", "none"), 0) + 1
    return "\n".join(out), sat, rungs


# ---------------------------------------------------------------- figure 3
THEMECOL = {"semantics": "#8a3a2a", "new-type": "#2f5d8a", "format": "#7a5a9a", "scope": "#5a7a3a"}


def critical(packets, target="P14", also=("P13b",)):
    deps = {p["id"]: p["deps"] for p in packets}
    need = set()
    stack = [target, *also]
    while stack:
        n = stack.pop()
        if n in need:
            continue
        need.add(n)
        stack.extend(deps.get(n, []))
    return need


def fig_plan(bp):
    packets = bp["packets"]
    ids = [p["id"] for p in packets]
    P = {p["id"]: p for p in packets}
    edges = [(d, p["id"]) for p in packets for d in p["deps"]]
    pos, cols = layer(ids, edges)
    crit = critical(packets) | {"P0"}
    COLW, BW, BH, ROWH = 205, 180, 52, 64
    maxd = max(cols)
    tallest = max(len(v) for v in cols.values())
    W, H = COLW * (maxd + 1) + 40, ROWH * tallest + 30
    xy = {}
    for d, ns in cols.items():
        for i, n in enumerate(ns):
            xy[n] = (20 + d * COLW, 15 + i * ROWH)
    out = [f'<svg viewBox="0 0 {W} {H}" width="{W}" height="{H}" xmlns="http://www.w3.org/2000/svg" '
           f'font-family="system-ui,sans-serif" role="img" aria-label="build plan v2">']
    for a, b in edges:
        (ax, ay), (bx, by) = xy[a], xy[b]
        x1, y1, x2, y2 = ax + BW, ay + BH / 2, bx, by + BH / 2
        c = (a in crit and b in crit)
        mx = (x1 + x2) / 2
        out.append(f'<path d="M{x1},{y1} C{mx},{y1} {mx},{y2} {x2},{y2}" fill="none" '
                   f'stroke="{INK if c else "#c3beb0"}" stroke-width="{2 if c else 1}"/>')
    for n, (x, y) in xy.items():
        p = P[n]
        dec = p.get("decision")
        ready = not p["deps"] and not dec
        stroke = THEMECOL[dec["theme"]] if dec else (NEW if ready else SAME)
        fill = "#ffffff" if dec else ("#e8f3ea" if ready else "#f7f5ef")
        dash = ' stroke-dasharray="5,3"' if dec else ""
        tip = f'{n}: {p["title"]}\ndepends on: {", ".join(p["deps"]) or "nothing"}'
        if dec:
            tip += f'\nNEEDS JOE ({dec["theme"]}): {dec["question"]}'
        tip += f'\nbad-case tests owed for: {", ".join(p["patterns"])}\nv2: {p["v2"]}'
        if p.get("status"):
            tip += f'\nstatus: {p["status"]}'
        out.append(f'<g><title>{esc(tip)}</title>'
                   f'<rect x="{x}" y="{y}" width="{BW}" height="{BH}" rx="5" fill="{fill}" stroke="{stroke}" '
                   f'stroke-width="{2.4 if n in crit else 1.4}"{dash}/>'
                   f'<text x="{x + 7}" y="{y + 17}" font-size="12.5" font-weight="700" fill="{INK}">{esc(n)}'
                   f'{" ◆" if dec else ""}</text>'
                   + "".join(f'<text x="{x + 7}" y="{y + 32 + 12 * i}" font-size="9.5" fill="{MUTED}">{esc(line)}</text>'
                             for i, line in enumerate(wrap(p["title"], 36)[:2]))
                   + '</g>')
    out.append("</svg>")
    return "\n".join(out), crit


# ---------------------------------------------------------------- page
CSS = """
body{margin:0;background:#fbfaf6;color:#1d1d1b;font-family:Georgia,serif;line-height:1.5}
main{max-width:62rem;margin:0 auto;padding:1rem 1.25rem 5rem}
h1{font-size:1.9rem;margin:2rem 0 .3rem} h2{font-size:1.3rem;margin:2.6rem 0 .4rem;border-top:1px solid #cfcabb;padding-top:1rem}
.lede{color:#6b6860;max-width:44rem}
.fig{overflow-x:auto;border:1px solid #e3dfd2;background:#fff;padding:.5rem;margin:.8rem 0;
     width:96vw;position:relative;left:50%;margin-left:-48vw;box-sizing:border-box}
.fig svg{display:block;margin:0 auto;max-width:100%;height:auto}
.key{font:13px system-ui,sans-serif;color:#6b6860;margin:.3rem 0 .6rem} .key span{display:inline-block;margin-right:1.2rem}
.sw{display:inline-block;width:12px;height:12px;vertical-align:-1px;margin-right:4px;border:2px solid}
table{border-collapse:collapse;font:14px system-ui,sans-serif;width:100%} td,th{border-top:1px solid #e3dfd2;padding:.35rem .5rem;text-align:left;vertical-align:top}
th{font-weight:600;color:#6b6860;font-size:12px;letter-spacing:.05em;text-transform:uppercase}
.q{color:#1b6b3a;font-weight:700} .s{color:#b8431f;font-weight:700}
.verify li{margin:.5rem 0} code{font-size:.9em;background:#f1eee4;padding:0 .2em}
.muted{color:#6b6860;font-size:.92rem} .zh{font-size:1.05em}
"""


def main():
    out = sys.argv[1]
    c1 = edn(f"{LAB}/cascade-象2000.edn")
    c2 = edn(f"{LAB}/cascade-象2000-v2.edn")
    h1 = edn(f"{LAB}/typed-hole-象2000.edn")
    h2 = edn(f"{LAB}/typed-hole-象2000-v2.edn")
    w2 = edn(f"{LAB}/wiring-象2000-v2.edn")
    bp = edn(f"{LAB}/build-plan-象2000-v2.edn")

    f1, counts = fig_cascade(c1, c2)
    f2, sat, rungs = fig_hole(h1, h2, set(c2["want"]))
    f3, crit = fig_plan(bp)

    pk = bp["packets"]
    decided = [p for p in pk if p.get("decision")]
    crit_dec = [p["id"] for p in decided if p["id"] in crit]
    ready = [p["id"] for p in pk if not p["deps"] and not p.get("decision")]
    regr = h2.get("regrades", [])
    new_pats = sorted(k for k in c2["patterns"] if k not in c1["patterns"])
    dangling = [d["token"] for d in w2.get("dangling-outputs", [])]

    H = []
    H.append(f"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>M-象-2000 v2 — for VERIFY</title><style>{CSS}</style></head><body><main>
<h1>M-象-2000 v2: cascade, typed hole, wiring, build plan</h1>
<p class="lede">For Joe to verify before anything is dispatched. Everything below is drawn from the
EDN files in <code>futon3c/holes/labs/M-象-2000/</code>; hover over any box for its full text.
v1 is kept beside v2 in the same directory.</p>""")

    H.append("<h2>What needs your judgement</h2><ol class='verify'>")
    H.append(f"""<li><b>A fifth want: <span class="zh">建造可自验</span> (the build checks itself).</b>
v1 wanted a faithful <i>system</i>. ARGUE-1 found every miss happened while <i>designing</i>, where the design
had no reach. v2 adds a want that the design must hold for its own construction: each phase reruns the
acceptance case, claims carry their qualifications across phases, and code references are graded.
Accept, or keep the process patterns outside the want?</li>
<li><b>Authority now needs two patterns, not one.</b> <span class="zh">名分有据</span> no longer produces
<span class="zh">行权有据</span> on its own; <span class="zh">释义非授</span> does, and it needs a record of authority
<i>and</i> a refutable interpretation. Reason: the evidence store labels every turn under your name
— including the 42 notices — as your <code>:question</code>. An interpretation is already acting as if it
were yours.</li>
<li><b>Clearance is narrowed.</b> v1: "if P had been in force at T0 the incident would not have happened".
v2: "resolved, and its measures can safely end", plus a list of effects that need compensating. The
counterfactual needs a world model, now its own hole (<span class="zh">反事实世界模型</span>).</li>
<li><b>Of your {len(decided)} decisions, {len(crit_dec)} block the acceptance case:</b>
{", ".join(crit_dec)}. The rest can wait. Details and grouping below.</li>
<li><b>A gap no packet covers:</b> {esc(bp["findings"][1])}</li>
</ol>""")

    H.append(f"""<p class="muted">What I checked: <code>cascade_check.py</code> and <code>wiring_check.py</code> pass on v2
(33 patterns; 67 token edges; 0 unfed wants). Every pointer in the v2 hole resolves, and each of v1's 48 was read at
HEAD — the read is written into the hole as <code>:read-note</code>. The packet dependencies in the plan's EDN
were checked against the markdown's prose (and the check was shown to catch a planted mismatch). Not checked:
nothing here has been run; P0 does not exist yet.</p>""")

    H.append(f"""<h2>1. The cascade: what changed</h2>
<p>Patterns, left to right in the order their tokens feed each other; the box on the right is the want.
{counts["new"]} patterns are new, {counts["changed"]} changed what they need or produce, {counts["same"]} are as in v1.
Green lines are token flows v1 did not have.</p>
<div class="key"><span><i class="sw" style="border-color:{NEW};background:#e8f3ea"></i>new in v2</span>
<span><i class="sw" style="border-color:{CHG};background:#fbf0d6"></i>changed</span>
<span><i class="sw" style="border-color:{SAME}"></i>unchanged</span>
<span style="color:{NEW}">— new token flow</span></div>
<div class="fig">{f1}</div>
<p class="muted">Produced but consumed by nothing (the wiring's "dangling outputs"): {", ".join(dangling)}.
<span class="zh">证实可分</span> is dangling on purpose — it serves the pattern library's growth, not the faithful system.</p>""")

    H.append("<table><tr><th>new pattern</th><th>reads as</th><th>gap it covers</th></tr>")
    SRC = {"象/覆水难收": "codex 1", "象/释义非授": "codex 3", "象/双时并记": "codex 4", "象/行有定名": "codex 4",
           "象/提交非生效": "codex 4", "象/措施随案": "codex 6", "象/同构方为同": "codex 7",
           "象/限定随论": "ARGUE-1 U1", "象/引非读": "ARGUE-1 U4", "象/每段自验": "ARGUE-1 U5", "象/裁亦有失": "ARGUE-1 U7",
           "agency/state-atomicity": "codex 5 (already in library)", "social/idempotent-handoff": "codex 5 (already in library)",
           "translation/declare-what-is-lost": "ARGUE-1 U6 (already in library)"}
    for k in new_pats:
        H.append(f"<tr><td class='zh'>{esc(k)}</td><td>{esc(PGLOSS.get(k, ''))}</td><td>{esc(SRC.get(k, ''))}</td></tr>")
    H.append("</table>")

    H.append(f"""<h2>2. The typed hole: what the stack already has</h2>
<p>One box per token. The border says how much of the token a running component supplies; the letter says how
well that is evidenced (象/引非读): <b>N</b> the pointer resolves, <b>R</b> the code was read and what it does is
written down, <b>W</b> a test or run shows it, <b>–</b> nothing to point at. Heavy border = a want token.
↻ = regraded from v1 after reading; <span style="color:{NEW}">v2</span> = token new in v2.</p>
<div class="key"><span><i class="sw" style="border-color:{PARTIAL};background:#fbf3d9"></i>partial ({sat.get("partial", 0)})</span>
<span><i class="sw" style="border-color:{HUNGRY}"></i>hungry ({sat.get("hungry", 0)})</span>
<span>rungs: W {rungs.get("witnessed", 0)} · R {rungs.get("read", 0)} · N {rungs.get("named", 0)} · – {rungs.get("none", 0)}</span></div>
<div class="fig">{f2}</div>
<p>Reading v1's pointers changed these:</p><table><tr><th>token</th><th>v1 → v2</th><th>why</th></tr>""")
    for r in regr:
        H.append(f"<tr><td class='zh'>{esc(r['token'])}<br><span class='muted'>{esc(TGLOSS.get(r['token'], ''))}</span></td>"
                 f"<td>{esc(r['v1'])} → {esc(r['v2'])}</td><td>{esc(r.get('why-en') or r['why'])}</td></tr>")
    H.append("</table>")

    H.append(f"""<h2>3. The build plan: what waits on what</h2>
<p>{len(pk)} packets, left to right by dependency. Dashed boxes with ◆ wait on you, coloured by kind of decision;
green boxes can be dispatched now ({", ".join(ready)}). Heavy boxes and black lines are the packets an acceptance case
answered entirely by queries needs: {", ".join(sorted(crit, key=lambda n: (bp_depth(pk, n), n)))}.</p>
<div class="key">""" + "".join(
        f'<span><i class="sw" style="border-color:{c}"></i>{esc(bp["decision-themes"][t]["label"])}</span>'
        for t, c in THEMECOL.items()) + f"""<span><i class="sw" style="border-color:{NEW};background:#e8f3ea"></i>ready now</span></div>
<div class="fig">{f3}</div>""")

    H.append("<table><tr><th>decision</th><th>kind</th><th>question</th><th>blocks the acceptance case?</th></tr>")
    order = {"semantics": 0, "new-type": 1, "format": 2, "scope": 3}
    for p in sorted(decided, key=lambda p: (p["id"] not in crit, order[p["decision"]["theme"]], p["id"])):
        d = p["decision"]
        H.append(f"<tr><td><b>{esc(p['id'])}</b> {esc(p['title'])}</td>"
                 f"<td style='color:{THEMECOL[d['theme']]}'>{esc(bp['decision-themes'][d['theme']]['label'])}</td>"
                 f"<td>{esc(d['question'])}</td><td>{'<b>yes</b>' if p['id'] in crit else 'no'}</td></tr>")
    H.append("</table>")

    H.append("""<h2>4. The acceptance case, row by row</h2>
<p>P0 reconstructs the 09-24 red-tape incident from the stores. A row is a <span class="q">query</span> if P0 can
answer it from what exists today, and a <span class="s">stub</span> (filled by hand, and labelled so) until the
named packets land. Every packet's acceptance includes rerunning P0 and showing a stub became a query.</p>
<table><tr><th>when</th><th>row</th><th>by</th><th>today</th><th>how</th></tr>""")
    for r in bp["acceptance"]:
        tag = ("<span class='q'>query</span>" if r["today"] == "query"
               else f"<span class='s'>stub</span> until {esc(', '.join(r['until']))}")
        how = esc(r["how"])
        if r.get("also"):
            how += f"<br><span class='s'>stub</span> within it: {esc(r['also']['stub'])} — until {esc(', '.join(r['also']['until']))}"
        H.append(f"<tr><td>{esc(r['when'])}</td><td>{esc(r['row'])}</td><td>{esc(r['who'])}</td><td>{tag}</td><td>{how}</td></tr>")
    H.append("</table>")

    H.append("""<h2>Appendix: every pattern's ports (v2)</h2>
<p class="muted">The wiring in table form: what each pattern needs and produces. <code>wiring-象2000-v2.edn</code>
is derived from these by <code>wiring_from_cascade.py</code>; an edge runs from a token's producer to each pattern that
needs it. Bold = changed in v2.</p>
<table><tr><th>pattern</th><th>needs</th><th>produces</th></tr>""")
    st = cascade_status(c1, c2)
    for pid in sorted(c2["patterns"], key=lambda k: (st[k] == "same", k)):
        p = c2["patterns"][pid]
        q = c1["patterns"].get(pid)
        def cell(toks, old):
            return ", ".join(("<b>%s</b>" % esc(t)) if (old is not None and t not in old) else esc(t) for t in sorted(toks)) or "—"
        H.append(f"<tr><td class='zh'>{esc(pid)}<br><span class='muted'>{esc(PGLOSS.get(pid, ''))} · {st[pid]}</span></td>"
                 f"<td class='zh'>{cell(p['guard']['needs'], q['guard']['needs'] if q else None)}</td>"
                 f"<td class='zh'>{cell(p['produces'], q['produces'] if q else None)}</td></tr>")
    H.append("</table>")
    H.append(f"""<p class="muted">Generated {datetime.datetime.now(datetime.timezone.utc):%Y-%m-%d %H:%M} UTC by
<code>futon3c/scripts/xiang2000_page.py</code>. Sources: cascade-象2000(-v2).edn, typed-hole-象2000(-v2).edn,
wiring-象2000-v2.edn, build-plan-象2000-v2.edn, BUILD-PLAN-象2000.md; patterns in futon3 <code>library/象/</code> (827b1f9).</p>
</main></body></html>""")
    with open(out, "w", encoding="utf-8") as f:
        f.write("\n".join(H))
    print(f"wrote {out}")


def bp_depth(packets, n):
    deps = {p["id"]: p["deps"] for p in packets}
    def d(x):
        return 0 if not deps[x] else 1 + max(d(y) for y in deps[x])
    return d(n)


if __name__ == "__main__":
    main()
