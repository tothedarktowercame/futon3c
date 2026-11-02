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
def _first_line(q):
    for ln in (q or "").splitlines():
        if ln.strip(): return ln.strip()[:60]
    return ""
def check_cite(c):
    """ok | ok-range | ok-path | ok-absent are accepted; everything else is a failure.
    Forms: path:line (quote on that line); path:a-b (quote on some line in a..b);
    bare path + quote found in the file (ok-path, weaker); bare path to a file that
    does not exist with value false or an `ls: cannot access` quote (ok-absent)."""
    if not isinstance(c, dict): return "not-an-object"
    if "unsure" in c: return "unsure"
    cite, quote = c.get("cite", ""), c.get("quote", "")
    if not cite: return "no-cite"
    q = _first_line(quote)
    m = re.match(r"^(/[^:]+|[^:]+):(\d+)(?:-(\d+))?$", cite)
    if not m:
        if ":" in cite or " " in cite.strip(): return "cite-not-path:line"
        p = cite if cite.startswith("/") else "/home/joe/code/" + cite
        path = pathlib.Path(p)
        if not path.exists():
            absent = c.get("value") is False or (quote or "").startswith("ls: cannot access")
            return "ok-absent" if absent else "path-missing"
        if path.is_dir(): return "ok-path" if c.get("value") is not False else "value-false-but-exists"
        try: body = path.read_text(errors="replace")
        except Exception: return "unreadable"
        return "ok-path" if q and q in body else "quote-not-in-file"
    p, a, b = m.group(1), int(m.group(2)), int(m.group(3) or m.group(2))
    if not p.startswith("/"): p = "/home/joe/code/" + p
    try: lines = pathlib.Path(p).read_text(errors="replace").splitlines()
    except Exception: return "path-missing"
    if a < 1 or b > len(lines) or b < a: return "line-out-of-range"
    if not q: return "ok" if a == b else "ok-range"
    for i in range(a, b + 1):
        if q in lines[i - 1]: return "ok" if a == b else "ok-range"
    # one line off either way: the quote is real, the number slipped (seen on 5 of 7 failures, 2026-11-02)
    for i in (a - 1, b + 1):
        if 1 <= i <= len(lines) and q in lines[i - 1]: return "ok-near"
    return "quote-not-on-line"
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
               "row": row, "checks": checks, "ok": sum(1 for c in checks if c.startswith("ok")), "unsure": sum(1 for c in checks if c == "unsure"),
               "failed": [c for c in checks if not c.startswith("ok") and c != "unsure"]}
        (HERE / f"{stem}.{kind}.json").write_text(json.dumps(rec, indent=1, ensure_ascii=False) + "\n")
        summary.append((stem, f"ok {rec['ok']} unsure {rec['unsure']} failed {len(rec['failed'])}", ",".join(rec["failed"])))
    print(f"job {job} state {state}; reply {len(txt)} chars; blocks {len(blocks)}")
    for s in summary: print("  ", *s)
if __name__ == "__main__":
    main(*sys.argv[1:4])
