#!/usr/bin/env python3
"""Render a :cascade/record-draft-v0 EDN file as one HTML page, optionally with a held-out check.
Usage: render-cascade-record.py RECORD.edn OUT.html [HELDOUT.edn]"""
import html, json, subprocess, sys

def load(path):
    code = ('(require (quote [cheshire.core :as j]))'
            f'(println (j/generate-string (clojure.edn/read-string (slurp "{path}"))))')
    return json.loads(subprocess.run(["bb", "-e", code], capture_output=True, text=True, check=True).stdout)

e = lambda x: html.escape(str(x if x is not None else ""))
CSS = """body{font:16px/1.5 system-ui,sans-serif;max-width:52rem;margin:2rem auto;padding:0 1rem;color:#1c2024;background:#fbfbf9}
h1{font-size:1.5rem;margin-bottom:.2rem}h2{font-size:1.1rem;margin-top:2rem}.sub{color:#5c6470}
.unit{border:1px solid #d5d8dc;border-left:5px solid #8a93a0;border-radius:6px;padding:.6rem .9rem;margin:.2rem 0;background:#fff}
.unit.held{border-left-color:#2e7d4f}.unit.weak{border-left-color:#c58a12}.unit.absent{border-left-color:#b23b3b}
.badge{font-size:.78rem;padding:.05rem .45rem;border-radius:9px;background:#eceef0;margin-left:.4rem;white-space:nowrap}
.held .v,.v.held{background:#d8eee0}.weak .v,.v.weak{background:#f6e7c4}.absent .v,.v.absent{background:#f4d6d6}
.flow{color:#5c6470;font-size:.9rem}.arrow{text-align:center;color:#8a93a0;font-size:.85rem;margin:0}
details{margin-top:.3rem;font-size:.92rem}summary{cursor:pointer;color:#3a5a8c}blockquote{margin:.3rem 0 .3rem .6rem;padding-left:.6rem;border-left:2px solid #d5d8dc}
.t{color:#5c6470;font-size:.8rem}table{border-collapse:collapse;width:100%;font-size:.92rem}td,th{border-bottom:1px solid #e2e4e7;padding:.3rem .5rem;text-align:left;vertical-align:top}"""

def quotes(rows, note=False):
    return "".join(f"<blockquote>{e(q.get('quote'))} <span class=t>{e(q.get('turn',''))}"
                   f"{' · ' + e(q.get('session')) if q.get('session') else ''}</span>"
                   f"{'<br>' + e(q.get('note')) if note and q.get('note') else ''}</blockquote>" for q in rows)

def unit(u, chk):
    v = chk.get("verdict", "") if chk else ""
    p = str(u.get("pattern", ""))
    kind = "proposed" if p.startswith("proposed/") else "library"
    head = (f"<b>{e(u.get('index'))}. {e(u.get('title') or p)}</b><span class=badge>{kind}: {e(p)}"
            f"{' · ' + e(u['sha256'][:10]) if u.get('sha256') else ''}</span>")
    if v: head += f"<span class='badge v'>{e(v)} on later turns</span>"
    flow = f"needs {e(', '.join(map(str, u.get('needs', []))))} → establishes {e(u.get('establishes'))}"
    body = ""
    if u.get("quote"): body += quotes([{"quote": u["quote"]}])
    if u.get("instances"): body += f"<details><summary>{len(u['instances'])} mined instances</summary>{quotes(u['instances'])}</details>"
    if chk and chk.get("instances"): body += f"<details><summary>{len(chk['instances'])} on later turns</summary>{quotes(chk['instances'])}</details>"
    if chk and chk.get("against"): body += f"<details><summary>{len(chk['against'])} against</summary>{quotes(chk['against'], True)}</details>"
    return f"<div class='unit {e(v)}'>{head}<div class=flow>{flow}</div>{body}</div>"

def main():
    r = load(sys.argv[1]); h = load(sys.argv[3]) if len(sys.argv) > 3 else {}
    checks = {u["index"]: u for u in h.get("units", [])}
    out = [f"<!doctype html><meta charset=utf-8><title>{e(r.get('id'))}</title><style>{CSS}</style>",
           f"<h1>{e(r.get('id'))}</h1><p class=sub>{e(r.get('provenance', 'stated'))} · {e(r.get('status'))} · {e(r.get('applies-to'))}"
           f"{' · G ' + e(r['G']) if r.get('G') is not None else ''}</p>", f"<p>{e(r.get('rationale'))}</p>"]
    if h: out.append(f"<h2>Tested on later turns ({e(h.get('turns-read'))} turns, {e(h.get('period'))})</h2><p>{e(h.get('summary'))}</p>")
    out.append("<h2>Steps</h2>")
    us = r.get("units", [])
    for i, u in enumerate(us):
        out.append(unit(u, checks.get(u.get("index"))))
        if i + 1 < len(us): out.append("<p class=arrow>↓</p>")
    if r.get("edges"):
        ev = {tuple(x["edge"]): x for x in h.get("edges", [])}
        rows = ""
        for ed in r["edges"]:
            c = ev.get((ed[0], ed[1]), {})
            rows += f"<tr><td>{e(ed[0])} → {e(ed[1])}</td><td><span class='badge v {e(c.get('verdict', ''))}'>{e(c.get('verdict', 'not tested'))}</span></td></tr>"
        out.append(f"<h2>Order</h2><table><tr><th>Edge</th><th>On later turns</th></tr>{rows}</table>")
    if h.get("standing-rules"): out.append(f"<p>Standing rule, not a step: {e(', '.join(map(str, h['standing-rules'])))}.</p>")
    if h.get("missing"): out.append(f"<h2>What it misses</h2><p>{e(h['missing'])}</p>")
    if r.get("reads"):
        out.append("<h2>Reads</h2><table><tr><th>Read</th><th>Source</th><th>Kind</th></tr>" + "".join(
            f"<tr><td>{e(k)}</td><td>{e(v.get('source'))}</td><td>{e(v.get('kind'))}</td></tr>" for k, v in r["reads"].items()) + "</table>")
    if r.get("alternatives"):
        out.append("<h2>Alternatives</h2>")
        for a in r["alternatives"]:
            pick = f" Pick {e(a['pick'])}." if a.get("pick") else ""
            g = f" G {e(a['G'])}." if a.get("G") is not None else ""
            out.append(f"<p><b>{e(a.get('id'))}.</b> {e(a.get('rationale'))}{pick}{g} {e(a.get('record', ''))}</p>{quotes(a.get('instances', []))}")
    if r.get("counter-examples"):
        out.append(f"<h2>Counter-examples</h2><details><summary>{len(r['counter-examples'])} turns</summary>{quotes(r['counter-examples'], True)}</details>")
    if r.get("attestation"): out.append(f"<h2>Attestation</h2><p>{e(json.dumps(r['attestation'], ensure_ascii=False))}</p>")
    open(sys.argv[2], "w").write("\n".join(out))

main()
