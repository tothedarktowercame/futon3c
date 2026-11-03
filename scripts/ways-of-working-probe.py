#!/usr/bin/env python3
"""Probe the declared ways of working and write their live state.

Usage: ways-of-working-probe.py DECLARATIONS OUT [--summary summary.json] [--graph graph.json]

Reads holes/excursions/ways-of-working.json, runs one probe per way that
declares one, and writes a JSON with, per way: monitor (on/off/broken/not-built),
invariant (holding/failing/unknown), last-observation, violations, evidence.
A way with probe "none" is reported as monitor not-built, invariant unknown:
the page shows that honestly rather than guessing. No state is changed.
"""
import datetime, glob, json, os, pathlib, re, subprocess, sys, urllib.parse, urllib.request

from xiang_marks import MARKS, bracket, paragraphs

HOME = pathlib.Path.home()
CODE = HOME / "code"
NOW = datetime.datetime.now(datetime.timezone.utc)

def iso(dt): return dt.replace(microsecond=0).isoformat()

def run(cmd, cwd=None, timeout=60):
    try:
        p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout)
        return p.returncode, p.stdout.strip(), p.stderr.strip()
    except Exception as e:  # noqa: BLE001
        return 99, "", str(e)

TERMINAL_STATES = {"done", "failed", "cancelled", "timed-out", "delivered"}
FEEDBACK_GRACE = datetime.timedelta(minutes=30)
TURN_STORE = HOME / ".emacs-graph" / "session-turn-analysis"

def get_json(url, timeout=120):
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=timeout) as response:
        return json.load(response)

def parse_time(value):
    if not value: return None
    return datetime.datetime.fromisoformat(str(value).replace("Z", "+00:00"))

def fetch_feedback_jobs(cutoff, base="http://localhost:7070", max_records=10000):
    """Read a bounded newest-first window, growing only until it crosses CUTOFF."""
    limit = 500
    while True:
        data = get_json(f"{base}/api/alpha/invoke/jobs?limit={limit}")
        if not data.get("ok") or not isinstance(data.get("jobs"), list):
            raise RuntimeError("Agency invoke-jobs response is not usable")
        jobs = data["jobs"]
        if not jobs or parse_time(jobs[-1].get("created-at")) <= cutoff or len(jobs) < limit:
            return jobs
        if limit >= max_records:
            raise RuntimeError("Agency invoke-jobs window did not cross the 24-hour cutoff")
        limit = min(limit * 2, max_records)

def fetch_promise_history(cutoff, base="http://localhost:7073", max_pages=20):
    query = {"tags": "promise-history", "since": iso(cutoff), "limit": "1000"}
    entries = []
    for _ in range(max_pages):
        data = get_json(base + "/api/alpha/evidence?" + urllib.parse.urlencode(query))
        if not isinstance(data.get("entries"), list):
            raise RuntimeError("futon1b promise-history response is not usable")
        entries.extend(data["entries"])
        cursor = data.get("next-cursor")
        if not cursor or not data["entries"]: return entries
        query.update({"cursor-at": cursor["at"], "cursor-id": cursor["id"]})
    raise RuntimeError("futon1b promise-history exceeded bounded pagination")

def fetch_operator_replies(cutoff, base="http://localhost:7073", max_pages=10):
    """Join final assistant evidence to local operator-turn evidence ids."""
    records = []
    for filename in glob.glob(str(TURN_STORE / "turn-*.json")):
        if os.path.basename(filename).count(".") > 1: continue
        try: record = json.loads(pathlib.Path(filename).read_text())
        except Exception: continue
        at = parse_time(record.get("created_at")) if isinstance(record, dict) else None
        if at and at >= cutoff and (record.get("origin") or "operator") == "operator":
            records.append(record)
    by_session = {}
    for record in records: by_session.setdefault(record.get("session_id"), []).append(record)
    replies = []
    for session_id, session_records in by_session.items():
        by_evidence = {r.get("evidence_id"): r for r in session_records if r.get("evidence_id")}
        if not session_id or not by_evidence: continue
        query = {"session-id": session_id, "since": iso(cutoff), "limit": "1000"}
        for _ in range(max_pages):
            data = get_json(base + "/api/alpha/evidence?" + urllib.parse.urlencode(query))
            if not isinstance(data.get("entries"), list):
                raise RuntimeError("futon1b operator-reply response is not usable")
            for entry in data["entries"]:
                body = entry.get("evidence/body")
                operator = by_evidence.get(entry.get("evidence/in-reply-to"))
                if (not isinstance(body, dict) or body.get("role") != "assistant"
                        or body.get("segment-final") is False or not operator): continue
                replies.append({"job-id": body.get("job-id") or body.get("turn-id") or entry.get("evidence/id"),
                                "agent-id": operator.get("agent_id"),
                                "text": body.get("unified-text") or body.get("text") or ""})
            cursor = data.get("next-cursor")
            if not cursor or not data["entries"]: break
            query.update({"cursor-at": cursor["at"], "cursor-id": cursor["id"]})
        else: raise RuntimeError("futon1b operator replies exceeded bounded pagination")
    return replies

def bell_emitter(job):
    prompt = next((e.get("text", "") for e in job.get("events", []) if e.get("type") == "prompt"), "")
    match = re.match(r"\s*(\[[^\]\n]+\])", prompt)
    return match.group(1) if match else "unlabelled"

def strict_marked_reply(text):
    ps = paragraphs(text)
    if not ps: return False, 0, 0
    valid = []
    for paragraph in ps:
        if paragraph[0] in MARKS:
            target, _ = bracket(paragraph[1:])
            if target: valid.append(paragraph[0])
    return len(valid) == len(ps) and valid[0] == "㊥", len(valid), len(ps)

def classify_feedback_reaches(jobs, promise_history, replies, now):
    """Pure 24-hour classifier; all inputs are already bounded records."""
    cutoff = now - datetime.timedelta(hours=24)
    window_jobs = [j for j in jobs if (parse_time(j.get("created-at")) or cutoff) >= cutoff]
    bells = [j for j in window_jobs if j.get("surface") == "bell"]
    by_id = {j.get("job-id"): j for j in window_jobs}
    park_types = {}
    for entry in promise_history:
        body = entry.get("evidence/body") or {}
        park_id = body.get("park-id") or body.get("id") or body.get("history/promise-id")
        if park_id: park_types.setdefault(str(park_id), set()).add(str(entry.get("evidence/type")))
    no_path, emitters = [], {}
    for job in bells:
        missing_agent = (job.get("terminal-code") == "agent-not-found"
                         or any(e.get("code") == "agent-not-found" for e in job.get("events", [])))
        if not str(job.get("caller") or "").strip() or job.get("caller") == "http-caller" or missing_agent:
            no_path.append(job)
            emitter = bell_emitter(job)
            row = emitters.setdefault(emitter, {"emitter": emitter, "count": 0, "job-ids": []})
            row["count"] += 1
            if len(row["job-ids"]) < 10: row["job-ids"].append(job.get("job-id"))
    addressed_terminal = [j for j in bells if j not in no_path and j.get("state") in TERMINAL_STATES]
    return_failures, pending = [], []
    for job in addressed_terminal:
        auto, received = job.get("auto-bellback") or {}, False
        if auto.get("bell-job-id"):
            child = by_id.get(auto["bell-job-id"])
            received = bool(child and child.get("delivery", {}).get("status") == "delivered")
        elif auto.get("suppressed?") and auto.get("reason") == "parked-on":
            received = "promise/ready-acked" in park_types.get(str(auto.get("park-id")), set())
        if received: continue
        finished = parse_time(job.get("finished-at")) or parse_time(job.get("created-at")) or now
        row = {"job-id": job.get("job-id"), "caller": job.get("caller"),
               "agent-id": job.get("agent-id"), "age-minutes": int((now - finished).total_seconds() / 60)}
        (pending if now - finished < FEEDBACK_GRACE else return_failures).append(row)
    reply_failures, marked_paragraphs, reply_paragraphs = [], 0, 0
    for reply in replies:
        passes, marked_count, paragraph_count = strict_marked_reply(reply.get("text", ""))
        marked_paragraphs += marked_count; reply_paragraphs += paragraph_count
        if not passes: reply_failures.append({k: reply.get(k) for k in ("job-id", "agent-id")})
    violations = ([{"kind": "bell-no-return-path", **row}
                   for row in sorted(emitters.values(), key=lambda r: (-r["count"], r["emitter"]))]
                  + [{"kind": "return-not-received", **row} for row in return_failures]
                  + [{"kind": "operator-reply-marks", **row} for row in reply_failures[:10]])
    rate = marked_paragraphs / reply_paragraphs if reply_paragraphs else 1.0
    return {"monitor": "on", "invariant": "failing" if no_path or return_failures or reply_failures else "holding",
            "last-observation": iso(now), "observed-at": iso(now), "violations": violations,
            "violation-count": len(no_path) + len(return_failures) + len(reply_failures),
            "pending": pending, "pending-count": len(pending),
            "evidence": [f"window start: {iso(cutoff)}",
                         f"return path: {len(no_path)} of {len(bells)} bells lacked an addressed path ({len(emitters)} emitters)",
                         f"return receipt: {len(return_failures)} of {len(addressed_terminal)} completed addressed bells unproved after 30m; {len(pending)} pending inside grace",
                         f"intent marks: {len(reply_failures)} of {len(replies)} operator-facing replies failed strict form; leading-mark paragraphs {marked_paragraphs}/{reply_paragraphs} ({rate:.1%})"],
            "denominator": (f"{len(no_path)}/{len(bells)} bells without path; "
                            f"{len(return_failures)}/{len(addressed_terminal)} completed addressed returns unproved; "
                            f"{len(reply_failures)}/{len(replies)} operator replies malformed")}

def probe_feedback_reaches():
    cutoff = NOW - datetime.timedelta(hours=24)
    try:
        return classify_feedback_reaches(fetch_feedback_jobs(cutoff),
                                         fetch_promise_history(cutoff),
                                         fetch_operator_replies(cutoff), NOW)
    except Exception as error:  # never turn missing evidence into holding
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "last-observation": None, "violations": [], "violation-count": 0,
                "evidence": [f"feedback-reaches read failed: {error}"],
                "denominator": "unavailable because a required read failed"}

def probe_git_clean():
    rows, no_upstream = [], []
    for d in sorted(CODE.iterdir()):
        if not (d / ".git").exists(): continue
        rc, ab, _ = run(["git", "rev-list", "--left-right", "--count", "@{u}...HEAD"], cwd=d)
        if rc != 0:
            no_upstream.append(d.name); continue
        behind, ahead = (ab.split() + ["0", "0"])[:2]
        rc, st, _ = run(["git", "status", "--porcelain"], cwd=d)
        lines = [l for l in st.splitlines() if l.strip()]
        untracked = sum(1 for l in lines if l.startswith("??"))
        modified = len(lines) - untracked
        if untracked or modified or int(ahead):
            rows.append({"repo": d.name, "untracked": untracked, "modified": modified,
                         "ahead": int(ahead), "behind": int(behind)})
    rc, active, _ = run(["systemctl", "--user", "is-active", "futon-sync-check-clean.timer"])
    rc2, enabled, _ = run(["systemctl", "--user", "is-enabled", "futon-sync-check-clean.timer"])
    log = CODE / "futon0" / "data" / "inbox-zero-delta.log"
    last = iso(datetime.datetime.fromtimestamp(log.stat().st_mtime, datetime.timezone.utc)) if log.exists() else None
    monitor = "on" if active == "active" else "off"
    evidence = [f"timer futon-sync-check-clean.timer: {active or 'absent'} ({enabled or 'unknown'})",
                f"delta notifier last wrote: {last or 'never'}",
                f"repos with no upstream (push undefined): {len(no_upstream)}"]
    return {"monitor": monitor, "invariant": "failing" if rows else "holding",
            "last-observation": last, "observed-at": iso(NOW),
            "violations": rows, "violation-count": len(rows), "evidence": evidence,
            "denominator": f"{len(rows) + 0} dirty of {len(list(p for p in CODE.iterdir() if (p/'.git').exists())) - len(no_upstream)} repos with an upstream"}

def probe_cascade_standards(summary):
    if not summary:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["summary.json absent"]}
    std = summary.get("standards", {})
    failing = [k for k, v in std.items() if k.startswith("s") and "-" in k and v is False]
    holes = summary.get("holes", {})
    ev = [f"{k}: {v}" for k, v in std.items() if not isinstance(v, dict)]
    ev.append(f"honest holes: {len(holes) if isinstance(holes, (list, dict)) else holes}")
    failing.append("s6-readable")
    ev.append("s6-readable: False (Joe 2026-11-02: cannot keep a picture of the whole stack; sampled-question test not built)")
    return {"monitor": "on", "invariant": "failing" if failing else "holding",
            "last-observation": summary.get("s4-evidence", {}).get("as-of") or std.get("s4-evidence", {}).get("as-of"),
            "observed-at": iso(NOW), "violations": failing, "violation-count": len(failing), "evidence": ev}

def probe_draw_pile(graph):
    if not graph:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["graph.json absent"]}
    t = graph.get("tickets", {})
    # the endpoint lists worktree copies (futon2-<branch>, futon3c-index-check, ...) as separate items;
    # one stem is one piece of work, so count distinct stems, preferring canonical repo paths (claude-16, 2026-11-02)
    import re as _re
    items = t.get("items", [])
    stems = {i.get("stem") for i in items if i.get("stem")}
    raw_total = t.get("count-total") or len(items)
    total = len(stems) if stems else raw_total
    canonical = {i.get("stem") for i in items if _re.match(r"/home/joe/code/futon[0-9a-z]*/", i.get("path") or "")}
    placed = set()
    for f in (HOME / "notes" / "commons-map").glob("placements-*.tsv"):
        if f.name.startswith("placements-who"): continue
        for i, line in enumerate(f.read_text().splitlines()):
            if i == 0 or not line.strip(): continue
            placed.add(line.split("\t")[0])
    # ant placements (claude-16, 2026-11-02 21:3xZ): an item counts as placed if a reader gave it a what,
    # a status and a cited way; those rows are committed under futon3c/holes/excursions/ant-reads/
    import json as _json
    ant_dir = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions" / "ant-reads"
    ant_placed = set()
    for f in ant_dir.glob("*.A1.json") if ant_dir.exists() else []:
        try: r = _json.loads(f.read_text()).get("row") or {}
        except Exception: continue
        if all(isinstance(r.get(k), dict) and ("value" in r[k]) for k in ("what", "status", "way")) and r["way"].get("cite"):
            ant_placed.add(r.get("stem") or f.name.split(".")[0])
    placed |= ant_placed
    committed = bool(ant_placed)  # the ant rows are on a committed path; the commons-map TSVs are not
    unplaced = len(stems - placed) if stems else max(total - len(placed), 0)  # ids placed but not in the pile must not be subtracted (claude-16)
    # claude-16 (2026-11-02 21:1xZ): over half the unplaced stems say closed in their own status line.
    # A closed item needs an attestation upward (IT-4), not a placement; count the two apart.
    _closed_re = _re.compile(r"^\*{0,2}status\*{0,2}:?\*{0,2}\s*.*\b(done|closed|complete[d]?|retired|superseded)\b", _re.I)
    path_of = {}
    for i in items:
        st, pth = i.get("stem"), i.get("path") or ""
        if st and st not in path_of or (st and _re.match(r"/home/joe/code/futon[0-9a-z]*/", pth)): path_of[st] = pth
    closed_unplaced = set()
    for st in (stems - placed):
        try:
            head = pathlib.Path(path_of.get(st, "")).read_text(errors="replace").splitlines()[:15]
        except Exception:
            continue
        if any(_closed_re.search(l) for l in head): closed_unplaced.add(st)
    open_unplaced = len(stems - placed) - len(closed_unplaced)
    ev = [f"of the unplaced: {open_unplaced} open (need a placement), {len(closed_unplaced)} closed in their own status line (need an attestation upward, IT-4, not a placement; claude-16 finding 2026-11-02)",f"draw pile (unclocked missions, excursions, tickets): {total} distinct stems ({raw_total} items listed; the rest are worktree copies; {len(canonical)} stems on canonical repo paths)",
          f"placements read by the commons map: {len(placed) - len(ant_placed)} ids (advisory, under ~/notes) plus {len(ant_placed)} ant placements (what + status + cited way; committed under ant-reads/); {len(stems & placed) if stems else 0} of all placed ids are in the pile",
          "the pile on this page is the evidence of this failure"]
    return {"monitor": "on", "invariant": "failing" if unplaced or not committed else "holding",
            "last-observation": iso(NOW), "observed-at": iso(NOW),
            "violations": [{"unplaced-open": open_unplaced, "unplaced-closed-unattested": len(closed_unplaced)}], "violation-count": unplaced, "evidence": ev,
            "denominator": f"{unplaced} of {total} unplaced: {open_unplaced} open, {len(closed_unplaced)} closed without attestation"}

# zone-health checks that bear on "stays alive and paid" (claude-16, 2026-11-03, README-hardening):
# the machine, its services, off-box copies and access. Bills and runway are not machine-checkable here.
ALIVE_CHECKS = ["user-services", "system-units", "futon1b-health", "agency-health", "federation",
                "firewall", "drive-health", "write-rate", "disk-space", "git-hub-backup", "evidence-backup",
                "pass-store", "pass-hubs-agree", "parked-timers"]

def probe_zone_health():
    zh = CODE / "futon0" / "scripts" / "zone-health.bb"
    rc, out, err = run(["bb", str(zh), "--json"], timeout=300)
    rows = []
    for line in out.splitlines():
        try: rows.append(json.loads(line))
        except Exception: pass  # noqa: BLE001
    if not rows:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0,
                "evidence": [f"zone-health produced no checks (rc {rc}): {err[:200]}"]}
    by = {r.get("id"): r for r in rows}
    seen = [by[c] for c in ALIVE_CHECKS if c in by]
    fails = [r for r in seen if r.get("status") != "pass"]
    _, dw, _ = run(["systemctl", "is-active", "disk-watch.timer"])
    ev = [f"zone-health: {len(seen) - len(fails)} of {len(seen)} checks for this way pass"]
    ev += [f"FAIL {r['id']}: {r.get('summary')}" for r in fails]
    ev += [f"disk-watch.timer: {dw or 'absent'} (the one check that pushes an alert unasked, drive only)",
           "read at publish time; zone-health has no cadence of its own",
           "not watched: bills paid, runway known (hand reading only)"]
    return {"monitor": "on", "invariant": "failing" if fails else "holding",
            "last-observation": iso(NOW), "observed-at": iso(NOW),
            "violations": [{"check": r["id"], "summary": r.get("summary"), "items": (r.get("items") or [])[:10]} for r in fails],
            "violation-count": len(fails), "evidence": ev,
            "denominator": f"{len(fails)} of {len(seen)} zone-health checks failing; bills and runway unwatched"}

def main():
    args = sys.argv[1:]
    decl, out = pathlib.Path(args[0]), pathlib.Path(args[1])
    summary = graph = None
    if "--summary" in args: summary = json.loads(pathlib.Path(args[args.index("--summary") + 1]).read_text())
    if "--graph" in args: graph = json.loads(pathlib.Path(args[args.index("--graph") + 1]).read_text())
    d = json.loads(decl.read_text())
    probes = {"git-clean": lambda: probe_git_clean(),
              "cascade-standards": lambda: probe_cascade_standards(summary),
              "draw-pile": lambda: probe_draw_pile(graph),
              "feedback-reaches": lambda: probe_feedback_reaches(),
              "zone-health": lambda: probe_zone_health()}
    ways = []
    for w in d["ways"]:
        w = dict(w)
        p = w.get("probe", "none")
        if p in probes:
            try: w["live"] = probes[p]()
            except Exception as e:  # noqa: BLE001
                w["live"] = {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                             "violations": [], "violation-count": 0, "evidence": [f"probe error: {e}"]}
        else:
            w["live"] = {"monitor": "not-built", "invariant": "unknown", "observed-at": iso(NOW),
                         "violations": [], "violation-count": 0,
                         "evidence": ["no probe declared: the way of working is prose until one is built"]}
        if w.get("scope-note"):
            w["live"]["evidence"].append("scope: " + w["scope-note"])
        ways.append(w)
    counts = {}
    for w in ways:
        k = w["live"]["monitor"] + "/" + w["live"]["invariant"]; counts[k] = counts.get(k, 0) + 1
    out.write_text(json.dumps({"schema": "ways-of-working-live-v1", "declared": d["as-of"],
                               "probed-at": iso(NOW), "states": d["states"], "counts": counts,
                               "provenance": d["provenance"], "ways": ways}, indent=1))
    print(f"wrote {out}: " + ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))

if __name__ == "__main__":
    main()
