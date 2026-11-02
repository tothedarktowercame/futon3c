#!/usr/bin/env python3
"""Harvest an ant job's reply: extract fenced JSON blocks, check every citation, write rows.

Usage: ant-harvest.py JOB_ID KIND SEAT
Reads GET /api/alpha/invoke/jobs/JOB_ID, finds ```json blocks, validates shape, checks each
cite (path:line exists and the line contains the quote), writes
holes/excursions/ant-reads/<stem>.<kind>.json with the check results. Prints a summary.
"""
import json, pathlib, re, sys, urllib.request, datetime
HERE = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions" / "ant-reads"
def fetch(job):
    with urllib.request.urlopen(f"http://localhost:7070/api/alpha/invoke/jobs/{job}", timeout=20) as r:
        return json.load(r)
def text_of(j):
    j = j.get("job", j)
    for k in ("result", "answer", "reply", "output", "message"):
        v = j.get(k)
        if isinstance(v, str) and v.strip(): return v
        if isinstance(v, dict):
            for kk in ("text", "answer", "content", "message"):
                if isinstance(v.get(kk), str) and v[kk].strip(): return v[kk]
    return json.dumps(j)
def check_cite(c):
    if not isinstance(c, dict): return "not-an-object"
    if "unsure" in c: return "unsure"
    cite, quote = c.get("cite", ""), c.get("quote", "")
    m = re.match(r"^(/[^:]+|[^:]+):(\d+)$", cite or "")
    if not m: return "no-cite" if not cite else "cite-not-path:line"
    p, ln = m.group(1), int(m.group(2))
    if not p.startswith("/"): p = "/home/joe/code/" + p
    try: lines = pathlib.Path(p).read_text(errors="replace").splitlines()
    except Exception: return "path-missing"
    if ln < 1 or ln > len(lines): return "line-out-of-range"
    if quote and quote.strip() and quote.strip()[:60] not in lines[ln - 1]: return "quote-not-on-line"
    return "ok"
def walk(obj, out):
    if isinstance(obj, dict):
        if "cite" in obj or "unsure" in obj: out.append(check_cite(obj)); return
        for v in obj.values(): walk(v, out)
    elif isinstance(obj, list):
        for v in obj: walk(v, out)
def main(job, kind, seat):
    j = fetch(job); state = j.get("job", j).get("state")
    txt = text_of(j)
    blocks = re.findall(r"```json\s*(\{.*?\})\s*```", txt, re.S)
    HERE.mkdir(parents=True, exist_ok=True)
    summary = []
    for b in blocks:
        try: row = json.loads(b)
        except Exception as e:
            summary.append(("?", "bad-json", str(e)[:60])); continue
        checks = []; walk(row, checks)
        stem = row.get("stem", "?")
        rec = {"stem": stem, "kind": kind, "seat": seat, "job": job, "at": datetime.datetime.now(datetime.timezone.utc).replace(microsecond=0).isoformat(),
               "row": row, "checks": checks, "ok": sum(1 for c in checks if c == "ok"), "unsure": sum(1 for c in checks if c == "unsure"),
               "failed": [c for c in checks if c not in ("ok", "unsure")]}
        (HERE / f"{stem}.{kind}.json").write_text(json.dumps(rec, indent=1, ensure_ascii=False) + "\n")
        summary.append((stem, f"ok {rec['ok']} unsure {rec['unsure']} failed {len(rec['failed'])}", ",".join(rec["failed"])))
    print(f"job {job} state {state}; reply {len(txt)} chars; blocks {len(blocks)}")
    for s in summary: print("  ", *s)
if __name__ == "__main__":
    main(*sys.argv[1:4])
