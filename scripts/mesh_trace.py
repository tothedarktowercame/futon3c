#!/usr/bin/env python3
"""Read-only mesh trace for Agency coordination — "how is the mesh moving?"

Projects both durable invoke-jobs (GET /api/alpha/invoke/jobs) and social-layer
coordination mesh edges (GET /api/alpha/coordination/edges) into a who-invoked-
whom view. The coordination source covers direct agent->agent invokes that do
not create invoke-jobs rows (IRC relay, dispatch direct-invoke, whistles).

This is an OBSERVATION tool. It mutates nothing, restarts nothing, and is safe
to run against a live Agency while a collaboration is in flight.
"""
import sys, json, argparse, urllib.request
from datetime import datetime, timezone

ap = argparse.ArgumentParser()
ap.add_argument("--base", default="http://localhost:7070")
ap.add_argument("--agent", help="only edges where this agent is sender or recipient")
ap.add_argument("--stuck", action="store_true",
                help="only show invoke-jobs edges that look stuck")
ap.add_argument("--running-secs", type=float, default=120.0,
                help="flag a running job older than this many seconds (default 120)")
ap.add_argument("--limit", type=int, default=40, help="max edges to print (most recent first)")
a = ap.parse_args()

TERMINAL = {"succeeded", "failed", "error", "done", "complete", "completed", "timeout", "cancelled"}

def now():
    return datetime.now(timezone.utc)

def parse_ts(s):
    if not s:
        return None
    try:
        return datetime.fromisoformat(str(s).replace("Z", "+00:00"))
    except Exception:
        return None

def fetch(path):
    req = urllib.request.Request(a.base + path)
    with urllib.request.urlopen(req, timeout=8) as r:
        return json.loads(r.read().decode())

try:
    data = fetch("/api/alpha/invoke/jobs")
except Exception as e:
    sys.exit(f"mesh_trace: could not reach Agency at {a.base}: {e}")

try:
    coord_data = fetch(f"/api/alpha/coordination/edges?limit={max(a.limit * 3, 100)}")
    coordination_edges = coord_data.get("edges", [])
except Exception as e:
    coordination_edges = []
    print(f"mesh_trace: coordination edge endpoint unavailable: {e}", file=sys.stderr)

jobs = data.get("jobs", [])
# Oldest-first for bellback pairing, then we print newest-first.
jobs_chrono = sorted(jobs, key=lambda j: j.get("created-at") or "")

# --- pair bell -> bellback: a later job to->from after a from->to job ---
awaiting = {}   # (frm,to) -> list of job-ids still without a returning edge
belled_back = set()
for j in jobs_chrono:
    frm, to = j.get("caller"), j.get("agent-id")
    back_key = (to, frm)            # a bell FROM the recipient back to the sender
    if awaiting.get(back_key):
        prior = awaiting[back_key].pop(0)
        belled_back.add(prior)      # the prior outbound edge got a reply edge
    awaiting.setdefault((frm, to), []).append(j.get("job-id"))

def is_terminal(j):
    return str(j.get("state", "")).lower() in TERMINAL

def running_secs(j):
    st = parse_ts(j.get("started-at") or j.get("created-at") or j.get("at"))
    return (now() - st).total_seconds() if st else 0.0

def delivered(j):
    return str((j.get("delivery") or {}).get("status", "")).lower() in {"delivered", "done", "ok"}

def flags(j):
    f = []
    state = str(j.get("state", "")).lower()
    if state == "running" and running_secs(j) > a.running_secs:
        f.append(f"SLOW>{int(a.running_secs)}s")
    if state == "queued":
        f.append("QUEUED")
    if is_terminal(j) and not delivered(j):
        f.append("UNDELIVERED")
    if is_terminal(j) and j.get("job-id") not in belled_back and j.get("caller") not in (None, "http-caller", "joe"):
        f.append("NO-BELLBACK")
    if str(j.get("state", "")).lower() in {"failed", "error", "timeout"}:
        f.append("FAILED")
    return f

def job_row(j):
    return {"source": "invoke-jobs",
            "from": j.get("caller") or "?",
            "to": j.get("agent-id") or "?",
            "surface": j.get("surface") or "bell",
            "state": j.get("state", "?"),
            "delivery": (j.get("delivery") or {}).get("status", "?"),
            "at": j.get("created-at"),
            "age-secs": running_secs(j),
            "flags": flags(j),
            "job-id": j.get("job-id")}

def edge_row(e):
    ok = e.get("ok?")
    state = "ok" if ok is True else "error" if ok is False else str(e.get("kind") or "recorded")
    return {"source": "coordination",
            "from": e.get("from") or "?",
            "to": e.get("to") or "?",
            "surface": e.get("surface") or "?",
            "state": state,
            "delivery": "-",
            "at": e.get("at"),
            "age-secs": running_secs(e),
            "flags": [],
            "evidence-id": e.get("evidence-id") or e.get("id")}

def duplicate_of_job(edge, job_rows):
    et = parse_ts(edge.get("at"))
    if et is None:
        return False
    for row in job_rows:
        if row["from"] == edge["from"] and row["to"] == edge["to"]:
            jt = parse_ts(row.get("at"))
            if jt and abs((et - jt).total_seconds()) <= 2.0:
                return True
    return False

job_rows = [job_row(j) for j in jobs_chrono]
coord_rows = [edge_row(e) for e in coordination_edges]
coord_rows = [r for r in coord_rows if not duplicate_of_job(r, job_rows)]
rows = sorted(job_rows + coord_rows, key=lambda r: r.get("at") or "", reverse=True)

if a.agent:
    rows = [r for r in rows if a.agent in (r.get("from"), r.get("to"))]

printed = 0
print(f"{'FROM':>14} -> {'TO':<14} {'SURFACE':<10} {'STATE':<10} {'DELIV':<10} AGE   FLAGS")
print("-" * 92)
for r in rows:
    fl = r.get("flags", [])
    if a.stuck and not fl:
        continue
    if printed >= a.limit:
        break
    frm = (r.get("from") or "?")[:14]
    to = (r.get("to") or "?")[:14]
    surface = str(r.get("surface") or "?")[:10]
    state = str(r.get("state") or "?")[:10]
    deliv = str(r.get("delivery") or "?")[:10]
    age = f"{int(r.get('age-secs') or 0)}s"
    bb = ""
    if r.get("source") == "invoke-jobs" and r.get("job-id") in belled_back:
        bb = "↩"
    print(f"{frm:>14} -> {to:<14} {surface:<10} {state:<10} {deliv:<10} {age:<5} {bb}{' '.join(fl)}")
    printed += 1

# --- summary ---
total = len(jobs)
running = sum(1 for j in jobs if str(j.get("state", "")).lower() == "running")
queued = sum(1 for j in jobs if str(j.get("state", "")).lower() == "queued")
failed = sum(1 for j in jobs if str(j.get("state", "")).lower() in {"failed", "error", "timeout"})
undelivered = sum(1 for j in jobs if is_terminal(j) and not delivered(j))
real_caller = sum(1 for j in jobs if j.get("caller") not in (None, "http-caller"))
print("-" * 92)
print(f"jobs={total}  coordination-edges={len(coordination_edges)}  merged-coordination={len(coord_rows)}  "
      f"running={running}  queued={queued}  failed={failed}  undelivered={undelivered}  "
      f"edges-with-real-caller={real_caller}/{total}")
if real_caller < total:
    print("note: invoke-jobs without a real caller show 'http-caller' — pass agency_send.py --from <id> to label them.")
