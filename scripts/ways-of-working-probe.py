#!/usr/bin/env python3
"""Probe the declared ways of working and write their live state.

Usage: ways-of-working-probe.py DECLARATIONS OUT [--summary summary.json] [--graph graph.json]

Reads holes/excursions/ways-of-working.json, runs one probe per way that
declares one, and writes a JSON with, per way: monitor (on/off/broken/not-built),
invariant (holding/failing/unknown), last-observation, violations, evidence.
A way with probe "none" is reported as monitor not-built, invariant unknown:
the page shows that honestly rather than guessing. No state is changed.
"""
import collections
import datetime, glob, hashlib, json, os, pathlib, re, subprocess, sys, time, urllib.parse, urllib.request

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

def join_operator_replies(operator_rows, evidence_rows):
    """Find one final assistant row for each matched operator evidence row."""
    rows_by_session = collections.defaultdict(list)
    for row in evidence_rows:
        if row.get("evidence/session-id"):
            rows_by_session[row["evidence/session-id"]].append(row)
    replies = []
    for operator in operator_rows:
        session_id = operator.get("evidence/session-id")
        body = operator.get("evidence/body") or {}
        session_rows = sorted(rows_by_session.get(session_id, []),
                              key=lambda row: row.get("evidence/at", ""))
        finals = [row for row in session_rows
                  if isinstance(row.get("evidence/body"), dict)
                  and row["evidence/body"].get("role") == "assistant"
                  and row["evidence/body"].get("segment-final") is not False]
        matches = [row for row in finals
                   if body.get("turn-id")
                   and row["evidence/body"].get("turn-id") == body.get("turn-id")]
        if not matches:  # Legacy rows: one conversational window in this session.
            start = operator.get("evidence/at", "")
            later_users = [row.get("evidence/at", "") for row in session_rows
                           if row.get("evidence/at", "") > start
                           and isinstance(row.get("evidence/body"), dict)
                           and row["evidence/body"].get("role") == "user"]
            end = min(later_users) if later_users else None
            matches = [row for row in finals if row.get("evidence/at", "") > start
                       and (end is None or row.get("evidence/at", "") < end)]
        if not matches: continue
        reply = matches[-1]
        reply_body = reply["evidence/body"]
        replies.append({"job-id": reply_body.get("job-id") or reply_body.get("turn-id")
                                  or reply.get("evidence/id"),
                        "agent-id": (operator.get("evidence/subject") or {}).get("ref/id"),
                        "text": reply_body.get("unified-text") or reply_body.get("text") or ""})
    return replies

def fetch_operator_replies(operator_rows, cutoff, now=NOW,
                           base="http://localhost:7073", bucket_hours=2):
    """Read bounded session/time buckets, then join the census's operator rows."""
    sessions = {row.get("evidence/session-id") for row in operator_rows
                if row.get("evidence/session-id")}
    entries = {}
    for session_id in sessions:
        start = cutoff
        while start < now:
            end = min(start + datetime.timedelta(hours=bucket_hours), now)
            query = {"session-id": session_id, "since": iso(start),
                     "before": iso(end), "limit": "1000"}
            data = get_json(base + "/api/alpha/evidence?" + urllib.parse.urlencode(query))
            rows = data.get("entries")
            if not isinstance(rows, list):
                raise RuntimeError("futon1b operator-reply response is not usable")
            if len(rows) >= 1000 or data.get("count", 0) >= 1000:
                raise RuntimeError(f"futon1b operator-reply bucket truncated: {session_id} {iso(start)}")
            for entry in rows:
                if entry.get("evidence/id"): entries[entry["evidence/id"]] = entry
            start = end
    return join_operator_replies(operator_rows, list(entries.values()))

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

# Machine callers that poll the original job to a terminal result instead of
# reading a bellback: futon2 flight_runner.clj:120-168 ("bell, then poll";
# "a bellback has no return path to the machine's persona"). For these, the
# terminal result is the receipt; an inbox copy is not (codex-12, 2026-11-03).
POLLING_CALLERS = {"wm-full-loop"}

def classify_feedback_reaches(jobs, promise_history, replies, now, operator_turns=None):
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
    # A brief-mode bell is a notification: it asks for no work, so it needs no
    # return path (E-autoclock-wow "Notifications are exempt only if typed as
    # such"). 2026-11-03: all 55 "no path" bells were brief-mode sweeper notices.
    notices = [j for j in bells if j.get("mode") == "brief"]
    bells = [j for j in bells if j.get("mode") != "brief"]
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
        if job.get("caller") in POLLING_CALLERS:
            # The caller reads the original job by polling; its inbox copy is a
            # redundant duplicate nobody reads (wm-inbox-discovery-REPORT.md).
            received = job.get("state") in TERMINAL_STATES and job.get("result") is not None
        elif auto.get("bell-job-id"):
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
                         f"return path: {len(no_path)} of {len(bells)} work bells lacked an addressed path ({len(emitters)} emitters); {len(notices)} brief-mode notifications exempt",
                         f"return receipt: {len(return_failures)} of {len(addressed_terminal)} completed addressed bells unproved after 30m; {len(pending)} pending inside grace",
                         f"intent marks: {len(reply_failures)} of {len(replies)} operator-facing replies failed strict form; leading-mark paragraphs {marked_paragraphs}/{reply_paragraphs} ({rate:.1%})",
                         f"replies found: {len(replies)} of {operator_turns if operator_turns is not None else len(replies)} operator turns",
                         "marks judged on the whole turn text, including narration between tool calls"],
            "denominator": (f"{len(no_path)}/{len(bells)} work bells without path; "
                            f"{len(return_failures)}/{len(addressed_terminal)} completed addressed returns unproved; "
                            f"{len(reply_failures)}/{len(replies)} operator replies malformed")}

def probe_feedback_reaches(census_fn=None):
    cutoff = NOW - datetime.timedelta(hours=24)
    try:
        census = (census_fn or read_operator_census)()
        operator_rows = census["operator-evidence"]
        return classify_feedback_reaches(fetch_feedback_jobs(cutoff),
                                         fetch_promise_history(cutoff),
                                         fetch_operator_replies(operator_rows, cutoff), NOW,
                                         operator_turns=len(operator_rows))
    except Exception as error:  # never turn missing evidence into holding
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "last-observation": None, "violations": [], "violation-count": 0,
                "evidence": [f"feedback-reaches read failed: {error}"],
                "denominator": "unavailable because a required read failed"}

OPERATOR_WINDOW = datetime.timedelta(hours=24)
OPERATOR_GRACE = datetime.timedelta(minutes=2)
OPERATOR_QUERY_PAD = datetime.timedelta(seconds=60)
OPERATOR_QUERY_LIMIT = 200

INTENT_OPENERS = {"propose", "ask-action", "offer", "delegate", "promise",
                  "report-problem", "clarify", "verify"}
INTENT_PENDING = datetime.timedelta(hours=24)
INTENT_STALE = datetime.timedelta(days=7)
INTENT_RECORD_LIMIT = 50000
INTENT_JOB_ACT = re.compile(r"^(invoke-\d+-\d+-[0-9a-f]+)-b-\d+$")

def _act_id(value):
    if isinstance(value, str): return value
    if isinstance(value, dict): return value.get("act") or value.get("id")
    return None

def read_intent_records(record_root=TURN_STORE, limit=INTENT_RECORD_LIMIT):
    """Read a bounded turn-record census and retain the newest valid row/session."""
    root = pathlib.Path(record_root)
    if not root.is_dir() or not os.access(root, os.R_OK):
        raise OSError(f"unreadable 象 record root: {root}")
    paths = list(root.glob("turn-*.json"))
    if len(paths) > limit:
        raise RuntimeError(f"象 record census truncated: {len(paths)} exceeds {limit}")
    valid, invalid = [], 0
    for path in paths:
        try:
            row = json.loads(path.read_text())
            created_at = parse_time(row.get("created_at")) if isinstance(row, dict) else None
        except (OSError, ValueError):
            invalid += 1; continue
        if (not isinstance(row, dict) or not row.get("session_id")
                or not created_at or not isinstance(row.get("ports"), dict)):
            invalid += 1; continue
        valid.append(row)
    latest = {}
    for row in valid:
        sid = row["session_id"]
        if sid not in latest or parse_time(row["created_at"]) > parse_time(latest[sid]["created_at"]):
            latest[sid] = row
    return valid, latest, invalid

def read_intent_sidecars(sidecar_root, limit=INTENT_RECORD_LIMIT):
    root = pathlib.Path(sidecar_root)
    if not root.is_dir() or not os.access(root, os.R_OK):
        raise OSError(f"unreadable 象 sidecar root: {root}")
    paths = list(root.glob("moves-scale*/shard-*/turn-*.moves.json"))
    if len(paths) > limit:
        raise RuntimeError(f"象 sidecar census truncated: {len(paths)} exceeds {limit}")
    rows = []
    for path in paths:
        try: row = json.loads(path.read_text())
        except (OSError, ValueError): continue
        if isinstance(row, dict): rows.append(row)
    return rows

def fetch_intent_job(job_id, base="http://localhost:7070"):
    try: data = get_json(f"{base}/api/alpha/invoke/jobs/{job_id}", timeout=3)
    except Exception: return None
    return data.get("job") or data

def classify_intents(records, latest, sidecars, now=NOW, job_state=None, invalid=0):
    """Classify operator opener acts with explicit v1 closure witnesses."""
    job_state = job_state or fetch_intent_job
    candidates, meta = {}, {}
    for sid, row in latest.items():
        acts = {a.get("id"): a for a in (row.get("acts") or []) if isinstance(a, dict) and a.get("id")}
        for act in acts.values():
            if act.get("author") == "operator" and act.get("kind") in INTENT_OPENERS:
                candidates.setdefault(act["id"], act)
                meta.setdefault(act["id"], (sid, row.get("agent_id", "unknown")))
        for port in row["ports"].get("still_open") or []:
            if not isinstance(port, dict): continue
            aid = _act_id(port); source = acts.get(aid, {})
            merged = dict(port)
            for key in ("author", "kind", "text", "at"):
                if source.get(key) is not None: merged[key] = source[key]
            if merged.get("author") == "operator" and merged.get("kind") in INTENT_OPENERS and aid:
                candidates.setdefault(aid, merged); meta.setdefault(aid, (sid, row.get("agent_id", "unknown")))

    witnesses = {}
    # Kernel closure is accepted only when an inferred closes edge backs it.
    for row in records:
        inferred = {w.get("opener"): w for w in (row.get("ports") or {}).get("inferred") or []
                    if isinstance(w, dict) and w.get("effect") == "closes"}
        for closed in (row.get("ports") or {}).get("closed_this_turn") or []:
            aid = _act_id(closed)
            if aid in inferred:
                witnesses.setdefault(aid, {"status": "answered", "kind": "kernel",
                                           "answer": inferred[aid].get("answer")})
    for sidecar in sidecars:
        for act in sidecar.get("acts") or []:
            if not isinstance(act, dict): continue
            kind = act.get("kind")
            for answer in act.get("answers") or []:
                target = _act_id(answer)
                if target and answer.get("effect") == "closes":
                    status = "met" if kind in {"report", "fulfil", "approve", "accept"} else "answered"
                    if kind == "redirect": status = "superseded"
                    if kind in {"withdraw", "retract"}: status = "withdrawn"
                    witnesses[target] = {"status": status, "kind": "sidecar-answer", "answer": act.get("act")}
            scope = act.get("scope") or {}
            target = _act_id(scope.get("closes"))
            if not target and kind in {"withdraw", "retract"}: target = _act_id(scope.get("within"))
            if target:
                witnesses[target] = {"status": "withdrawn" if kind in {"withdraw", "retract"} else "answered",
                                     "kind": "sidecar-scope", "answer": act.get("act")}
    for aid in candidates:
        match = INTENT_JOB_ACT.match(aid)
        if not match or aid in witnesses: continue
        job = job_state(match.group(1))
        state = (job or {}).get("state")
        if state in TERMINAL_STATES:
            witnesses[aid] = {"status": "met" if state in {"done", "delivered"} else "answered",
                              "kind": "agency-job", "job-state": state}

    statuses, pending, violations = collections.Counter(), [], []
    for aid, act in candidates.items():
        if aid in witnesses:
            statuses[witnesses[aid]["status"]] += 1; continue
        statuses["open"] += 1
        at = parse_time(act.get("at") or act.get("since"))
        sid, agent = meta[aid]
        item = {"act-id": aid, "session": sid, "agent": agent,
                "date": at.date().isoformat() if at else "unknown",
                "kind": act.get("kind", "unknown"),
                "text": re.sub(r"\s+", " ", act.get("text") or "")[:80]}
        if at and now - at < INTENT_PENDING:
            pending.append(item); continue
        progress = any(act.get(k) for k in ("owner", "debtor", "assignee", "progress", "progress-at"))
        deferred = act.get("deferred") is True or act.get("disposition") == "deferred"
        if at and now - at > INTENT_STALE and not progress and not deferred:
            violations.append(item)
    invariant = "failing" if violations else "holding"
    return {"monitor": "on", "invariant": invariant, "last-observation": iso(now),
            "observed-at": iso(now), "violations": violations,
            "violation-count": len(violations), "pending": pending, "pending-count": len(pending),
            "status-counts": dict(statuses),
            "evidence": [f"intent census: {len(candidates)} deduplicated operator opener acts; {invalid} invalid records skipped",
                         "closure states: " + ", ".join(f"{k} {v}" for k, v in sorted(statuses.items())),
                         f"open aging: {len(pending)} pending under 24h; {len(violations)} stale over 7d without owner/progress/deferment",
                         "v1 limitation: Joe's next-turn stance is not counted yet"],
            "denominator": f"{len(violations)} of {statuses['open']} open operator intents stale and unwitnessed"}

def probe_intents_tracked(now=NOW, record_root=TURN_STORE, sidecar_root=None,
                          job_state=None):
    sidecar_root = pathlib.Path(sidecar_root or CODE / "futon3c" / "holes" / "labs" / "M-象-2000" / "data")
    try:
        records, latest, invalid = read_intent_records(record_root)
        sidecars = read_intent_sidecars(sidecar_root)
        return classify_intents(records, latest, sidecars, now, job_state, invalid)
    except Exception as exc:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(now),
                "last-observation": None, "violations": [], "violation-count": 0,
                "evidence": [f"intents-tracked probe broken: {exc}"],
                "denominator": "unavailable because a required census failed"}

def normalize_turn_text(text):
    text = re.sub(r"^\N{SPEAKING HEAD IN SILHOUETTE}\s*", "", (text or "").strip())
    return re.sub(r"\s+", " ", text).strip()

def parse_operator_header(text):
    """Parse only the first CURRENT TURN header, never quoted body headers."""
    text = text or ""
    if not text.startswith("--- CURRENT TURN ---\n") or "\n---\n" not in text:
        return None
    header, payload = text.split("\n---\n", 1)
    fields = {}
    for line in header.splitlines()[1:]:
        if ":" in line:
            key, value = line.split(":", 1); fields[key.strip().lower()] = value.strip()
    if fields.get("origin") != "operator": return None
    wrapped = re.match(r"\s*Agent:\s*[^\n]+\n\s*User message:\s*\n(.*)\Z", payload, re.S)
    return fields, (wrapped.group(1) if wrapped else payload).strip()

def _content_text(content):
    if isinstance(content, str): return content
    if isinstance(content, list):
        return "\n".join(str(x.get("text", "")) for x in content
                         if isinstance(x, dict) and x.get("text"))
    return ""

def read_operator_turns(now=NOW, claude_root=None, codex_root=None):
    claude_root = pathlib.Path(claude_root or HOME / ".claude" / "projects")
    codex_root = pathlib.Path(codex_root or HOME / ".codex" / "sessions")
    bad = [str(p) for p in (claude_root, codex_root)
           if not p.is_dir() or not os.access(p, os.R_OK)]
    if bad: raise OSError("unreadable transcript root(s): " + ", ".join(bad))
    cutoff, records = now - OPERATOR_WINDOW, []
    sources = (("claude", claude_root.glob("**/*.jsonl")),
               ("codex", codex_root.glob("**/rollout-*.jsonl")))
    for vendor, files in sources:
        for path in files:
            try:
                if datetime.datetime.fromtimestamp(path.stat().st_mtime, datetime.timezone.utc) < cutoff: continue
                stream = path.open(errors="replace")
            except OSError as exc:
                raise OSError(f"unreadable transcript {path}: {exc}") from exc
            with stream:
                for line in stream:
                    try: row = json.loads(line)
                    except (ValueError, TypeError): continue
                    if vendor == "claude":
                        if row.get("type") != "user": continue
                        message = row.get("message") or {}; message_id = row.get("uuid")
                    else:
                        message = row.get("payload") or {}
                        if (row.get("type") != "response_item" or message.get("type") != "message"
                                or message.get("role") != "user"): continue
                        message_id = message.get("id")
                    parsed = parse_operator_header(_content_text(message.get("content")))
                    try: at = parse_time(row.get("timestamp"))
                    except (TypeError, ValueError): continue
                    if not parsed or not at or at < cutoff or at > now or not message_id: continue
                    header, payload = parsed
                    records.append({"vendor": vendor, "id": message_id, "at": at,
                                    "agent": header.get("to", "unknown"),
                                    "surface": header.get("surface", "unknown"),
                                    "text": normalize_turn_text(payload)})
    unique = {(r["vendor"], r["id"]): r for r in records}
    return sorted(unique.values(), key=lambda r: (r["at"], r["vendor"], r["id"]))

def evidence_windows(turns, minutes=10):
    buckets = {}
    for turn in turns:
        buckets.setdefault(int(turn["at"].timestamp()) // (minutes * 60), []).append(turn)
    return [(min(t["at"] for t in rows) - OPERATOR_QUERY_PAD,
             max(t["at"] for t in rows) + OPERATOR_QUERY_PAD)
            for _, rows in sorted(buckets.items())]

def fetch_operator_evidence(turns, opener=urllib.request.urlopen, pause=time.sleep,
                            base="http://127.0.0.1:7073", limit=OPERATOR_QUERY_LIMIT):
    entries, windows = {}, evidence_windows(turns)
    for index, (start, end) in enumerate(windows):
        query = urllib.parse.urlencode({"author": "joe", "since": iso(start),
                                        "before": iso(end), "limit": limit})
        request = urllib.request.Request(base + "/api/alpha/evidence?" + query,
                                         headers={"Accept": "application/json"})
        try:
            with opener(request, timeout=20) as response: page = json.load(response)
        except Exception as exc:
            raise RuntimeError(f"futon1b query failed for {iso(start)}: {exc}") from exc
        rows = page.get("entries")
        if not isinstance(rows, list): raise RuntimeError(f"futon1b malformed page for {iso(start)}")
        if len(rows) >= limit or page.get("count", 0) >= limit:
            raise RuntimeError(f"futon1b window truncated at limit {limit}: {iso(start)} to {iso(end)}")
        for row in rows:
            if row.get("evidence/id"): entries[row["evidence/id"]] = row
        if index + 1 < len(windows): pause(1.0)
    return list(entries.values())

def _valid_operator_evidence(row):
    origin, body = row.get("evidence/origin") or {}, row.get("evidence/body") or {}
    return (str(origin.get("kind", "")).lstrip(":") == "operator"
            and body.get("event") == "chat-turn" and body.get("role") == "user")

def match_operator_turns(turns, evidence, now=NOW):
    pairs = []
    for ti, turn in enumerate(turns):
        transport = "emacs-claude-repl" if turn["vendor"] == "claude" else "emacs-codex-repl"
        for ei, row in enumerate(evidence):
            body = row.get("evidence/body") or {}
            if (not _valid_operator_evidence(row) or body.get("transport") != transport
                    or normalize_turn_text(body.get("text")) != turn["text"]): continue
            try: distance = abs((parse_time(row.get("evidence/at")) - turn["at"]).total_seconds())
            except (TypeError, ValueError): continue
            if distance <= OPERATOR_QUERY_PAD.total_seconds(): pairs.append((distance, ti, ei))
    used_turns, used_evidence = set(), set()
    for _, ti, ei in sorted(pairs):
        if ti not in used_turns and ei not in used_evidence:
            used_turns.add(ti); used_evidence.add(ei)
    pending, violations = [], []
    for index, turn in enumerate(turns):
        if index in used_turns: continue
        item = {"at": iso(turn["at"]), "agent": turn["agent"], "surface": turn["surface"],
                "vendor-id": turn["id"],
                "text-digest": hashlib.sha256(turn["text"].encode()).hexdigest()[:12]}
        (pending if now - turn["at"] < OPERATOR_GRACE else violations).append(item)
    return {"matched": len(used_turns), "pending": pending, "violations": violations,
            "operator-evidence": [evidence[ei] for ei in sorted(used_evidence)]}

def read_operator_census(now=NOW, claude_root=None, codex_root=None,
                         fetcher=fetch_operator_evidence):
    turns = read_operator_turns(now, claude_root, codex_root)
    result = match_operator_turns(turns, fetcher(turns), now)
    return {"turns": turns, **result}

def probe_operator_turns(now=NOW, claude_root=None, codex_root=None,
                         fetcher=fetch_operator_evidence, census=None):
    try:
        result = census or read_operator_census(now, claude_root, codex_root, fetcher)
        turns = result["turns"]
    except Exception as exc:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(now),
                "violations": [], "violation-count": 0,
                "evidence": [f"operator-turn probe broken: {exc}"]}
    violations, pending = result["violations"], result["pending"]
    return {"monitor": "on", "invariant": "failing" if violations else "holding",
            "last-observation": iso(now), "observed-at": iso(now),
            "violations": violations, "violation-count": len(violations),
            "pending": pending, "pending-count": len(pending),
            "evidence": [f"operator turns (24h): {len(turns)} turns, {result['matched']} matched, {len(pending)} pending",
                         "limitation: this does not prove dictated turns were tagged dictated"],
            "denominator": f"{len(violations)} of {len(turns) - len(pending)} mature operator turns unmatched"}

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
    operator_cache = {}
    def shared_operator_census():
        if "value" not in operator_cache:
            operator_cache["value"] = read_operator_census()
        return operator_cache["value"]
    probes = {"git-clean": lambda: probe_git_clean(),
              "cascade-standards": lambda: probe_cascade_standards(summary),
              "draw-pile": lambda: probe_draw_pile(graph),
              "feedback-reaches": lambda: probe_feedback_reaches(shared_operator_census),
              "intents-tracked": lambda: probe_intents_tracked(),
              "operator-turns": lambda: probe_operator_turns(census=shared_operator_census()),
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
