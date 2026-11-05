#!/usr/bin/env python3
"""Probe the declared ways of working and write their live state.

Usage: ways-of-working-probe.py DECL OUT [--summary summary.json] [--graph graph.json]
       ways-of-working-probe.py DECL --run <probe> [--summary S] [--graph G] [--results-dir DIR]
       ways-of-working-probe.py DECL OUT --assemble [--results-dir DIR]

Reads holes/excursions/ways-of-working.json, runs one probe per way that
declares one, and writes a JSON with, per way: monitor (on/off/broken/not-built),
invariant (holding/failing/unknown), last-observation, violations, evidence.
A way with probe "none" is reported as monitor not-built, invariant unknown:
the page shows that honestly rather than guessing. No state is changed.
"""
import collections
import datetime, difflib, glob, hashlib, json, os, pathlib, re, subprocess, sys, time, urllib.parse, urllib.request

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

# futon1b admits one expensive read at a time and answers 503
# (expensive-read-busy) while another holds it; the cascade publisher's own
# pattern reads hold it for 8-12 s (2026-11-03 23:48 and 23:57 publishes).
# A busy store is a reason to wait, not a broken monitor; after the backoff
# runs out the probe still reports broken.
BUSY_BACKOFF_S = (2, 4, 8, 16)

def open_json(request, timeout, opener=None, pause=None):
    opener = opener or urllib.request.urlopen
    pause = pause or time.sleep
    for wait in BUSY_BACKOFF_S + (None,):
        try:
            with opener(request, timeout=timeout) as response:
                return json.load(response)
        except urllib.error.HTTPError as exc:
            if exc.code != 503 or wait is None: raise
            pause(wait)

def get_json(url, timeout=120):
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    return open_json(req, timeout)

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
        whole_text = reply_body.get("unified-text") or reply_body.get("text") or ""
        reply_paragraphs = paragraphs(whole_text)
        first_mark = next((index for index, paragraph in enumerate(reply_paragraphs)
                           if paragraph and paragraph[0] in MARKS), None)
        judged_text = ("\n\n".join(reply_paragraphs[first_mark:])
                       if first_mark is not None else whole_text)
        replies.append({"job-id": reply_body.get("job-id") or reply_body.get("turn-id")
                                  or reply.get("evidence/id"),
                        "agent-id": operator.get("probe/agent-id")
                                    or (operator.get("evidence/subject") or {}).get("ref/id"),
                        "text": judged_text})
    return replies

def fetch_operator_replies(operator_rows, cutoff, now=NOW,
                           base="http://localhost:7073", bucket_hours=2,
                           max_pages=20):
    """Read bounded session/time buckets, paging each with next-cursor.

    A bucket can hold more than one page of rows (a chatty session's
    promise-lifecycle churn filled a 2-hour bucket on 2026-11-04 and broke
    the probe), so a full page is followed via the API's next-cursor the
    same way fetch_promise_history does, under a page bound so a broken
    cursor loop still fails loudly."""
    sessions = {row.get("evidence/session-id") for row in operator_rows
                if row.get("evidence/session-id")}
    entries = {}
    for session_id in sessions:
        start = cutoff
        while start < now:
            end = min(start + datetime.timedelta(hours=bucket_hours), now)
            query = {str(k): str(v) for k, v in
                     {"session-id": session_id, "since": iso(start),
                      "before": iso(end), "limit": "1000"}.items()}
            for _ in range(max_pages):
                data = get_json(base + "/api/alpha/evidence?" + urllib.parse.urlencode(query))
                rows = data.get("entries")
                if not isinstance(rows, list):
                    raise RuntimeError("futon1b operator-reply response is not usable")
                for entry in rows:
                    if entry.get("evidence/id"): entries[entry["evidence/id"]] = entry
                cursor = data.get("next-cursor")
                if not cursor and len(rows) >= int(query["limit"]):
                    raise RuntimeError(f"futon1b operator-reply bucket full with no "
                                       f"next-cursor: {session_id} {iso(start)}")
                if not cursor or not rows: break
                query.update({"cursor-at": cursor["at"], "cursor-id": cursor["id"]})
            else:
                raise RuntimeError(f"futon1b operator-reply bucket exceeded bounded "
                                   f"pagination: {session_id} {iso(start)}")
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
                         "marks judged on the final reply segment; narration between tool calls excluded"],
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
# Clock skew between the futon1b evidence row (written when Joe sends the
# turn) and the transcript row (written when the turn is delivered to the
# seat and analysed) is minutes, not seconds: the two 2026-11-04 false
# violations were 319 s and 426 s apart (audit
# ~/notes/ways-of-working-audit/2026-11-04/operator-turns.md). 10 minutes
# covers both with margin without reaching Joe's habitual re-sends of
# similar prompts (hours apart).
OPERATOR_MATCH_WINDOW = datetime.timedelta(minutes=10)
# Dictated turns are re-transcribed on the way into the store: the observed
# drift was ratio 0.985 over ~4.4 KB ("end-of-day" vs "end-of-the-day").
# Fuzzy matching is only allowed for long turns: a 0.95 ratio over a short
# greeting would match unrelated greetings. Short turns must match exactly.
OPERATOR_FUZZY_MIN_LEN = 200
OPERATOR_FUZZY_RATIO = 0.95
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

def _is_pasted_text(act):
    """True when the act's own record marks it as pasted terminal/output text
    (e.g. the 'claude is thinking...' status line Joe pasted on 10-28).
    NOTE: standing? false alone is NOT the marker — 象 stamps it on real,
    standing asks too (codex-2's 10-21 pair, claude-15 f-s2-0); only the
    `about` field saying 'pasted' identifies non-intent text."""
    about = str(act.get("about") or "").lower()
    return "past" in about  # pasted / paste

def read_intent_records(record_root=TURN_STORE, limit=INTENT_RECORD_LIMIT):
    """Read a bounded turn-record census and retain the newest valid row/session."""
    root = pathlib.Path(record_root)
    if not root.is_dir() or not os.access(root, os.R_OK):
        raise OSError(f"unreadable 象 record root: {root}")
    # 象 writes sibling artefacts beside the turn records (.draft/.analysis/
    # .candidates); they are dicts without session_id/ports BY DESIGN and were
    # 16,610 of the 19,965 "invalid" records.  Narrow the census to real
    # turn records only.
    artefact_suffixes = (".draft.json", ".analysis.json", ".candidates.json")
    paths = [p for p in root.glob("turn-*.json")
             if not p.name.endswith(artefact_suffixes)]
    if len(paths) > limit:
        raise RuntimeError(f"象 record census truncated: {len(paths)} exceeds {limit}")
    valid, invalid = [], 0
    unanalysed = {"total": 0, "operator": 0}
    for path in paths:
        try:
            row = json.loads(path.read_text())
            created_at = parse_time(row.get("created_at")) if isinstance(row, dict) else None
        except (OSError, ValueError):
            invalid += 1; continue
        if not isinstance(row, dict) or not created_at:
            invalid += 1; continue
        if row.get("ports") is None:
            # A real turn record that analysis never ran on: typed census
            # hole, not corruption.  Intents here were lost UPSTREAM.
            unanalysed["total"] += 1
            if row.get("origin") == "operator":
                unanalysed["operator"] += 1
            continue
        if not row.get("session_id") or not isinstance(row.get("ports"), dict):
            invalid += 1; continue
        row["_record"] = path.stem
        valid.append(row)
    latest = {}
    for row in valid:
        sid = row["session_id"]
        if sid not in latest or parse_time(row["created_at"]) > parse_time(latest[sid]["created_at"]):
            latest[sid] = row
    return valid, latest, invalid, unanalysed

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

def classify_intents(records, latest, sidecars, now=NOW, job_state=None, invalid=0,
                     unanalysed=None):
    """Classify operator opener acts with explicit v1 closure witnesses."""
    job_state = job_state or fetch_intent_job
    unanalysed = unanalysed or {"total": 0, "operator": 0}
    candidates, meta = {}, {}
    for sid, row in latest.items():
        acts = {a.get("id"): a for a in (row.get("acts") or []) if isinstance(a, dict) and a.get("id")}
        for act in acts.values():
            if act.get("author") == "operator" and act.get("kind") in INTENT_OPENERS:
                if _is_pasted_text(act):
                    continue  # pasted terminal/output text, not an intent
                candidates.setdefault((sid, act["id"]), act)
                meta.setdefault((sid, act["id"]), (sid, row.get("agent_id", "unknown")))
        for port in row["ports"].get("still_open") or []:
            if not isinstance(port, dict): continue
            aid = _act_id(port); source = acts.get(aid, {})
            merged = dict(port)
            for key in ("author", "kind", "text", "at", "about"):
                if source.get(key) is not None: merged[key] = source[key]
            if (merged.get("author") == "operator" and merged.get("kind") in INTENT_OPENERS
                    and aid and not _is_pasted_text(merged)):
                candidates.setdefault((sid, aid), merged)
                meta.setdefault((sid, aid), (sid, row.get("agent_id", "unknown")))

    witnesses = {}
    # Kernel closure is accepted only when an inferred closes edge backs it.
    for row in records:
        inferred = {w.get("opener"): w for w in (row.get("ports") or {}).get("inferred") or []
                    if isinstance(w, dict) and w.get("effect") == "closes"}
        for closed in (row.get("ports") or {}).get("closed_this_turn") or []:
            aid = _act_id(closed)
            if aid in inferred:
                witnesses.setdefault((row.get("session_id"), aid), {"status": "answered", "kind": "kernel",
                                           "answer": inferred[aid].get("answer")})
    # Act ids restart with each session of a seat ("zai-7-turn-1-..."), so
    # every key is (session, act-id); a sidecar names its record, not a session.
    record_session = {r.get("_record"): r.get("session_id") for r in records}
    for sidecar in sidecars:
        sidecar_sid = record_session.get(sidecar.get("record"))
        for act in sidecar.get("acts") or []:
            if not isinstance(act, dict): continue
            kind = act.get("kind")
            for answer in act.get("answers") or []:
                target = _act_id(answer)
                if target and answer.get("effect") == "closes":
                    status = "met" if kind in {"report", "fulfil", "approve", "accept"} else "answered"
                    if kind == "redirect": status = "superseded"
                    if kind in {"withdraw", "retract"}: status = "withdrawn"
                    witnesses[(sidecar_sid, target)] = {"status": status, "kind": "sidecar-answer", "answer": act.get("act")}
            scope = act.get("scope") or {}
            target = _act_id(scope.get("closes"))
            if not target and kind in {"withdraw", "retract"}: target = _act_id(scope.get("within"))
            if target:
                witnesses[(sidecar_sid, target)] = {"status": "withdrawn" if kind in {"withdraw", "retract"} else "answered",
                                     "kind": "sidecar-scope", "answer": act.get("act")}
    for key in candidates:
        aid = key[1]
        match = INTENT_JOB_ACT.match(aid)
        if not match or key in witnesses: continue
        job = job_state(match.group(1))
        state = (job or {}).get("state")
        if state in TERMINAL_STATES:
            witnesses[key] = {"status": "met" if state in {"done", "delivered"} else "answered",
                              "kind": "agency-job", "job-state": state}

    statuses, pending, violations, unwitnessable = collections.Counter(), [], [], []
    for key, act in candidates.items():
        aid = key[1]
        if key in witnesses:
            statuses[witnesses[key]["status"]] += 1; continue
        statuses["open"] += 1
        at = parse_time(act.get("at") or act.get("since"))
        sid, agent = meta[key]
        item = {"act-id": aid, "session": sid, "agent": agent,
                "date": at.date().isoformat() if at else "unknown",
                "kind": act.get("kind", "unknown"),
                "text": re.sub(r"\s+", " ", act.get("text") or "")[:80]}
        if at and now - at < INTENT_PENDING:
            pending.append(item); continue
        progress = any(act.get(k) for k in ("owner", "debtor", "assignee", "progress", "progress-at"))
        deferred = act.get("deferred") is True or act.get("disposition") == "deferred"
        if at and now - at > INTENT_STALE and not progress and not deferred:
            # Finding 6: Zai seats write no assistant rows, so no probe can
            # witness closure for their acts.  Not stale — unwitnessable by
            # construction, kept visible instead.
            if str(agent).startswith("zai-"):
                unwitnessable.append(item)
            else:
                violations.append(item)
    if unanalysed["operator"] > 0:
        # Real operator turns 象 never analysed: the intents were lost
        # upstream and never became records with acts.  A violation of the
        # way, named as such, not a silently skipped count.
        violations.append({"act-id": "operator-turns-never-analysed",
                           "count": unanalysed["operator"],
                           "kind": "unanalysed-turn",
                           "note": "operator turns with no 象 analysis: intents lost upstream (violation of the way intents-are-tracked)"})
    invariant = "failing" if violations else "holding"
    return {"monitor": "on", "invariant": invariant, "last-observation": iso(now),
            "observed-at": iso(now), "violations": violations,
            "violation-count": len(violations), "pending": pending, "pending-count": len(pending),
            "unwitnessable-count": len(unwitnessable), "unwitnessable": unwitnessable,
            "status-counts": dict(statuses),
            "evidence": [f"intent census: {len(candidates)} deduplicated operator opener acts; {invalid} invalid records skipped (real invalid only; 象 .draft/.analysis/.candidates artefacts excluded from the census)",
                         f"operator turns never analysed: {unanalysed['operator']} of {unanalysed['total']} un-analysed turn records — intents lost upstream; counted as a violation of the way when non-zero",
                         "closure states: " + ", ".join(f"{k} {v}" for k, v in sorted(statuses.items())),
                         f"open aging: {len(pending)} pending under 24h; {len(violations)} stale over 7d without owner/progress/deferment; {len(unwitnessable)} stale-looking acts unwitnessable by construction (Zai seats write no assistant rows) and not counted as violations",
                         "v1 limitation: Joe's next-turn stance is not counted yet"],
            "denominator": f"{len(violations)} of {statuses['open']} open operator intents stale and unwitnessed"}

def probe_intents_tracked(now=NOW, record_root=TURN_STORE, sidecar_root=None,
                          job_state=None):
    sidecar_root = pathlib.Path(sidecar_root or CODE / "futon3c" / "holes" / "labs" / "M-象-2000" / "data")
    try:
        records, latest, invalid, unanalysed = read_intent_records(record_root)
        sidecars = read_intent_sidecars(sidecar_root)
        return classify_intents(records, latest, sidecars, now, job_state, invalid, unanalysed)
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

def evidence_windows(turns, minutes=10, pad_before=None, pad_after=None):
    buckets = {}
    for turn in turns:
        buckets.setdefault(int(turn["at"].timestamp()) // (minutes * 60), []).append(turn)
    before = OPERATOR_QUERY_PAD if pad_before is None else pad_before
    after = OPERATOR_QUERY_PAD if pad_after is None else pad_after
    return [(min(t["at"] for t in rows) - before,
             max(t["at"] for t in rows) + after)
            for _, rows in sorted(buckets.items())]

def fetch_operator_evidence(turns, opener=urllib.request.urlopen, pause=time.sleep,
                            base="http://127.0.0.1:7073", limit=OPERATOR_QUERY_LIMIT):
    entries, windows = {}, evidence_windows(
        turns, pad_before=OPERATOR_MATCH_WINDOW + OPERATOR_QUERY_PAD)
    for index, (start, end) in enumerate(windows):
        query = urllib.parse.urlencode({"author": "joe", "since": iso(start),
                                        "before": iso(end), "limit": limit})
        request = urllib.request.Request(base + "/api/alpha/evidence?" + query,
                                         headers={"Accept": "application/json"})
        try:
            page = open_json(request, 20, opener, pause)
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

def _pair_rank(turn_text, evidence_text):
    """0 = exact text, 1 = fuzzy (long-turn transcription drift), None = no match.

    There is no shared per-row id: the futon1b evidence body carries a
    turn-id from the turn-analysis pool ("claude-14-turn-1467") and a
    session id, not the transcript row uuid, so text + a bounded window is
    the join. Exact wins over fuzzy; short turns never go fuzzy."""
    if turn_text == evidence_text: return 0
    if min(len(turn_text), len(evidence_text)) >= OPERATOR_FUZZY_MIN_LEN:
        if difflib.SequenceMatcher(None, turn_text, evidence_text).ratio() >= OPERATOR_FUZZY_RATIO:
            return 1
    return None

def _match_pairs(turns, evidence):
    """Greedy one-to-one pairing of turns to evidence rows, nearest first.

    A pair needs transport agreement, a text match (exact, or fuzzy for long
    turns — dictation drift), and a timestamp distance within
    OPERATOR_MATCH_WINDOW: the evidence row is written at send, the
    transcript row minutes later, so the old 60 s pad logged false
    violations at 319 s and 426 s skew. Exact matches take priority so a
    drifted copy never steals an evidence row from its exact twin."""
    pairs, window = [], OPERATOR_MATCH_WINDOW.total_seconds()
    for ti, turn in enumerate(turns):
        transport = "emacs-claude-repl" if turn["vendor"] == "claude" else "emacs-codex-repl"
        for ei, row in enumerate(evidence):
            body = row.get("evidence/body") or {}
            if not _valid_operator_evidence(row) or body.get("transport") != transport: continue
            rank = _pair_rank(turn["text"], normalize_turn_text(body.get("text")))
            if rank is None: continue
            try: distance = abs((parse_time(row.get("evidence/at")) - turn["at"]).total_seconds())
            except (TypeError, ValueError): continue
            if distance <= window: pairs.append((rank, distance, ti, ei))
    used_turns, used_evidence, matched_pairs = set(), set(), []
    for _, _, ti, ei in sorted(pairs):
        if ti not in used_turns and ei not in used_evidence:
            used_turns.add(ti); used_evidence.add(ei); matched_pairs.append((ti, ei))
    return matched_pairs

def _pending_and_violations(turns, matched_keys, now=NOW):
    pending, violations = [], []
    for turn in turns:
        if _turn_key(turn) in matched_keys: continue
        item = {"at": iso(turn["at"]), "agent": turn["agent"], "surface": turn["surface"],
                "vendor-id": turn["id"],
                "text-digest": hashlib.sha256(turn["text"].encode()).hexdigest()[:12]}
        (pending if now - turn["at"] < OPERATOR_GRACE else violations).append(item)
    return pending, violations

def match_operator_turns(turns, evidence, now=NOW):
    matched_pairs = _match_pairs(turns, evidence)
    pending, violations = _pending_and_violations(turns, {turns[ti]["vendor"] + "/" + turns[ti]["id"]
                                                          for ti, _ in matched_pairs}, now)
    operator_evidence = [dict(evidence[ei], **{"probe/agent-id": turns[ti]["agent"]})
                         for ti, ei in sorted(matched_pairs)]
    return {"matched": len(matched_pairs), "pending": pending, "violations": violations,
            "operator-evidence": operator_evidence}

CENSUS_CACHE = HOME / ".cache" / "futon3c" / "ways-of-working" / "operator-census.json"

def _turn_key(turn):
    return turn["vendor"] + "/" + turn["id"]

def load_census_cache(path):
    """Read the matched-evidence cache. Corrupt or unreadable: start empty, warn on stderr."""
    try:
        with open(path) as fh: data = json.load(fh)
        if not isinstance(data, dict) or not all(isinstance(v, dict) for v in data.values()):
            raise ValueError("cache is not a dict of rows")
        return data
    except FileNotFoundError:
        return {}
    except (OSError, ValueError) as exc:
        print(f"operator-census cache unreadable at {path} ({exc}); starting empty",
              file=sys.stderr)
        return {}

def write_census_cache(path, entries):
    path = pathlib.Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".tmp." + str(os.getpid()))
    with open(tmp, "w") as fh: json.dump(entries, fh)
    os.replace(tmp, path)

def read_operator_census(now=NOW, claude_root=None, codex_root=None,
                         fetcher=fetch_operator_evidence, cache_path=CENSUS_CACHE):
    turns = read_operator_turns(now, claude_root, codex_root)
    cache_path = pathlib.Path(cache_path)
    live_keys = {_turn_key(t) for t in turns}
    # keep only entries whose turn is still inside the 24 h window
    cached = {k: v for k, v in load_census_cache(cache_path).items() if k in live_keys}
    rest = [t for t in turns if _turn_key(t) not in cached]
    # with zero turns we still ping the fetcher: a broken store must break the
    # probe even when there is nothing to look up (fetcher([]) does no HTTP)
    new_matches = {}
    if rest or not turns:
        # a row already held by a cached turn is spoken for: matching stays one-to-one
        held = {v.get("evidence/id") for v in cached.values()}
        evidence = [r for r in fetcher(rest) if r.get("evidence/id") not in held]
        for ti, ei in _match_pairs(rest, evidence):
            new_matches[_turn_key(rest[ti])] = dict(evidence[ei], **{"probe/agent-id": rest[ti]["agent"]})
    merged = {**cached, **new_matches}
    if merged != load_census_cache(cache_path):
        write_census_cache(cache_path, merged)
    matched_keys = {_turn_key(t) for t in turns if _turn_key(t) in merged}
    pending, violations = _pending_and_violations(turns, matched_keys, now)
    operator_evidence = [merged[_turn_key(t)] for t in turns if _turn_key(t) in merged]
    return {"turns": turns, "matched": len(matched_keys), "pending": pending,
            "violations": violations, "operator-evidence": operator_evidence}

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
    # population fixed 2026-11-05 (zai-6, audit git-clean.md, claude-16 fix list):
    # - .repair-limbs-fixture-* dirs are leaked test fixtures, not work; excluded
    #   from the population and counted separately, since the leak is a defect.
    # - "no @{u}" does not mean "push undefined": a worktree shares its parent
    #   repo's remotes, and a repo with no remote at all is a single-copy repo.
    #   Both are checked for commits not reachable from any remote ref, and a
    #   dirty tree counts for them as for upstream repos.
    rows, no_upstream, checked, fixtures = [], [], 0, 0
    for d in sorted(CODE.iterdir()):
        if not (d / ".git").exists(): continue
        if d.name.startswith(".repair-limbs-fixture-"):
            fixtures += 1; continue
        checked += 1
        ahead = behind = 0
        rc, ab, _ = run(["git", "rev-list", "--left-right", "--count", "@{u}...HEAD"], cwd=d)
        if rc == 0:
            behind, ahead = (ab.split() + ["0", "0"])[:2]
            ahead, behind = int(ahead), int(behind)
        else:
            no_upstream.append(d.name)
            # commits visible only on this box; counts all of HEAD when there
            # is no remote (single-copy repo)
            rc2, cnt, _ = run(["git", "rev-list", "--count", "HEAD", "--not", "--remotes"], cwd=d)
            ahead = int(cnt) if rc2 == 0 and cnt.isdigit() else 0
        rc, st, _ = run(["git", "status", "--porcelain"], cwd=d)
        lines = [l for l in st.splitlines() if l.strip()]
        untracked = sum(1 for l in lines if l.startswith("??"))
        modified = len(lines) - untracked
        if untracked or modified or ahead:
            rows.append({"repo": d.name, "untracked": untracked, "modified": modified,
                         "ahead": ahead, "behind": behind})
    rc, active, _ = run(["systemctl", "--user", "is-active", "futon-sync-check-clean.timer"])
    rc2, enabled, _ = run(["systemctl", "--user", "is-enabled", "futon-sync-check-clean.timer"])
    log = CODE / "futon0" / "data" / "inbox-zero-delta.log"
    last = iso(datetime.datetime.fromtimestamp(log.stat().st_mtime, datetime.timezone.utc)) if log.exists() else None
    monitor = "on" if active == "active" else "off"
    evidence = [f"timer futon-sync-check-clean.timer: {active or 'absent'} ({enabled or 'unknown'}); this is the futon0 watcher, parked by Joe, and is NOT this probe",
                f"probe cadence: this probe re-reads every 5 min via the ways-of-working results refresh; monitor 'off' above refers only to the parked timer",
                f"delta notifier last wrote: {last or 'never'}",
                f"repos with no @{{u}} (still checked for unpushed/dirty): {len(no_upstream)}",
                f"excluded .repair-limbs-fixture-* leaked test fixtures: {fixtures} (the leak is itself a defect, not part of the population)"]
    return {"monitor": monitor, "invariant": "failing" if rows else "holding",
            "last-observation": last, "observed-at": iso(NOW),
            "violations": rows, "violation-count": len(rows), "evidence": evidence,
            "denominator": f"{len(rows)} dirty or unpushed of {checked} repos checked ({checked - len(no_upstream)} with @{{u}}, {len(no_upstream)} without; {fixtures} fixture dirs excluded)"}

def probe_cascade_standards(summary):
    if not summary:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["summary.json absent"]}
    std = summary.get("standards")
    if not isinstance(std, dict) or not std:
        # shape drift must not read as all-pass (zai-4 audit 2026-11-04)
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0,
                "evidence": ["standards absent from summary: shape drift, not a pass"]}
    failing, unchecked, ev = [], [], []
    for k, v in std.items():
        if not (k.startswith("s") and "-" in k):
            continue
        state = v.get("state") if isinstance(v, dict) else v
        if v is False or state is False:
            failing.append(k); ev.append(f"{k}: False")
        elif state == "unchecked" or v == "unchecked":
            # an unchecked standard does not pass (Joe 2026-11-05)
            reason = (v.get("reason") if isinstance(v, dict) else None) or "no reason given"
            unchecked.append(k); ev.append(f"{k}: unchecked - {reason}")
        elif v is True or state is True:
            ev.append(f"{k}: True")
    holes = summary.get("holes", {})
    ev.append(f"honest holes: {len(holes) if isinstance(holes, (list, dict)) else holes}")
    failing.extend(unchecked)
    failing.append("s6-readable")
    ev.append("s6-readable: False (Joe 2026-11-02: cannot keep a picture of the whole stack; sampled-question test not built)")
    return {"monitor": "on", "invariant": "failing" if failing else "holding",
            "last-observation": std.get("s4-evidence", {}).get("as-of") if isinstance(std.get("s4-evidence"), dict) else None,
            "observed-at": iso(NOW), "violations": failing, "violation-count": len(failing),
            "evidence": ev}

# Joe, 2026-11-05: "Let's give the benefit of the doubt and assume that they
# were all completed at the time of dispatch, punch them" — every E-kimi-task
# file that existed in futon2 by this instant is punched. And: "In future
# autoclock-wow will *not* assume that all dispatches completed" — an
# E-kimi-task first added after this cutoff is punched only by its own
# "DONE — confirmed <date> (Giraffe closing check)" status line.
KIMI_AMNESTY_CUTOFF = "2026-11-05T00:40:00+00:00"

def git_first_added(path):
    """ISO datetime of the commit that first added PATH, or None. Walks up to
    the containing repo; the oldest --diff-filter=A line is the first add."""
    p = pathlib.Path(path)
    repo = next((r for r in [p.parent, *p.parents] if (r / ".git").exists()), None)
    if repo is None: return None
    try: rel = p.relative_to(repo)
    except ValueError: return None
    added = _first_added_in_dir(repo, rel.parent)
    return added.get(str(rel))

_FIRST_ADDED_CACHE = {}

def _first_added_in_dir(repo, reldir):
    """{repo-relative path: first-add datetime} for every file ever added under
    RELDIR, from ONE git log call (per-file calls cost ~95 s per probe run over
    futon2's E-kimi-task files, which broke the 1-min publish). git log is
    newest-first, so the last date seen for a path is its first add."""
    key = (str(repo), str(reldir))
    if key in _FIRST_ADDED_CACHE: return _FIRST_ADDED_CACHE[key]
    rc, out, _ = run(["git", "log", "--diff-filter=A", "--name-only", "--format=@@%aI",
                      "--", str(reldir) if str(reldir) != "." else "."], cwd=repo, timeout=120)
    added, when = {}, None
    if rc == 0:
        for line in out.splitlines():
            if line.startswith("@@"): when = parse_time(line[2:])
            elif line.strip() and when is not None: added[line.strip()] = when
    _FIRST_ADDED_CACHE[key] = added
    return added

def _punch_rules(stem, body_text, repair_store, first_added):
    """Machine-minted completion rules (Joe, 2026-11-05). A punched item left
    the drawable pile; it is not a violation and needs no placement."""
    rules = []
    if stem.startswith("T-repair-occ-"):
        # "1 yes": DONE with a repair-store witness citing a dismissal or a
        # landing sha. OPEN repair tickets stay drawable.
        done = any(re.match(r"\*{0,2}status\*{0,2}:?\*{0,2}\s*\**\s*done\b", l, re.I)
                   for l in body_text.splitlines()[:15])
        occ = stem[len("T-repair-occ-"):]
        store = pathlib.Path(repair_store) if repair_store else None
        if done and store and (store / "ticket-links" / f"repair-occ-{occ}.edn").exists():
            dismissed = bool(list((store / "dismissals").glob(f"repair-occ-{occ[:16]}*")))
            sha = re.search(r"\b[0-9a-f]{40}\b", body_text)
            if dismissed or sha: rules.append("repair-witness")
    if stem.startswith("E-kimi-task-"):
        if re.search(r"status\*{0,2}:?\*{0,2}\s*done\s*[—-]\s*confirmed", body_text[:2000], re.I):
            rules.append("kimi-confirmed")
        elif first_added is not None and first_added <= parse_time(KIMI_AMNESTY_CUTOFF):
            rules.append("kimi-amnesty-2026-11-05")
    return rules

def probe_draw_pile(graph, commons_map_dir=None, ant_reads_dir=None, repair_store_dir=None):
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
    commons_map = pathlib.Path(commons_map_dir) if commons_map_dir else HOME / "notes" / "commons-map"
    for f in commons_map.glob("placements-*.tsv") if commons_map.exists() else []:
        if f.name.startswith("placements-who"): continue
        for i, line in enumerate(f.read_text().splitlines()):
            if i == 0 or not line.strip(): continue
            placed.add(line.split("\t")[0])
    # ant placements (claude-16, 2026-11-02 21:3xZ): an item counts as placed if a reader gave it a what,
    # a status and a cited way; those rows are committed under futon3c/holes/excursions/ant-reads/
    import json as _json
    ant_dir = pathlib.Path(ant_reads_dir) if ant_reads_dir else pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions" / "ant-reads"
    ant_placed = set()
    for f in ant_dir.glob("*.A1.json") if ant_dir.exists() else []:
        try: r = _json.loads(f.read_text()).get("row") or {}
        except Exception: continue
        if all(isinstance(r.get(k), dict) and ("value" in r[k]) for k in ("what", "status", "way")) and r["way"].get("cite"):
            ant_placed.add(r.get("stem") or f.name.split(".")[0])
    placed |= ant_placed
    committed = bool(ant_placed)  # the ant rows are on a committed path; the commons-map TSVs are not
    repair_store = repair_store_dir or "/home/joe/code/futon2/data/wm-repair-obligations"
    path_of = {}
    for i in items:
        st, pth = i.get("stem"), i.get("path") or ""
        if st and st not in path_of or (st and _re.match(r"/home/joe/code/futon[0-9a-z]*/", pth)): path_of[st] = pth
    # punched items (Joe, 2026-11-05): completed machine-minted tickets leave
    # the drawable pile; reported per rule, never as violations. An item that
    # is placed is already out of the pile; punch is counted only for the rest
    # so the per-rule counts are the effect on the drawable pile.
    punch = {}
    for st, pth in path_of.items():
        if not pth or st in placed: continue
        try: body = pathlib.Path(pth).read_text(errors="replace")
        except Exception: continue
        rules = _punch_rules(st, body, repair_store, git_first_added(pth))
        if rules: punch[st] = rules[0]
    drawable = stems - placed - set(punch)
    unplaced = len(drawable) if stems else max(total - len(placed), 0)  # ids placed but not in the pile must not be subtracted (claude-16)
    # claude-16 (2026-11-02 21:1xZ): over half the unplaced stems say closed in their own status line.
    # A closed item needs an attestation upward (IT-4), not a placement; count the two apart.
    # anchored 2026-11-05 (zai-5 audit): the status VALUE must start with the
    # closure word, so "Status: open. Diagnosis complete" stays open.
    _closed_re = _re.compile(r"^\*{0,2}status\*{0,2}:?\*{0,2}\s*\**\s*(done|closed|complete[d]?|retired|superseded)\b", _re.I)
    closed_unplaced = set()
    for st in drawable:
        try:
            head = pathlib.Path(path_of.get(st, "")).read_text(errors="replace").splitlines()[:15]
        except Exception:
            continue
        if any(_closed_re.search(l) for l in head): closed_unplaced.add(st)
    open_unplaced = len(drawable) - len(closed_unplaced)
    punch_counts = {r: sum(1 for v in punch.values() if v == r)
                    for r in ("repair-witness", "kimi-amnesty-2026-11-05", "kimi-confirmed")}
    ev = [f"of the unplaced: {open_unplaced} open (need a placement), {len(closed_unplaced)} closed in their own status line (need an attestation upward, IT-4, not a placement; claude-16 finding 2026-11-02)",
          f"punched (completed machine-minted, not drawable, not violations): {len(punch)} items — repair-witness: {punch_counts['repair-witness']}, kimi-amnesty-2026-11-05: {punch_counts['kimi-amnesty-2026-11-05']}, kimi-confirmed: {punch_counts['kimi-confirmed']} (Joe rulings 2026-11-05; amnesty is dated, not a standing rule)",
f"draw pile (unclocked missions, excursions, tickets): {total} distinct stems ({raw_total} items listed; the rest are worktree copies; {len(canonical)} stems on canonical repo paths)",
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

def probe_zone_health(runner=None):
    # runner is injectable so the test can feed synthetic zone-health JSON.
    zh = CODE / "futon0" / "scripts" / "zone-health.bb"
    rc, out, err = (runner or run)(["bb", str(zh), "--json", "--only", ",".join(ALIVE_CHECKS)], timeout=300)
    rows = []
    for line in out.splitlines():
        try: rows.append(json.loads(line))
        except Exception: pass  # noqa: BLE001
    if not rows:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0,
                "evidence": [f"zone-health produced no checks (rc {rc}): {err[:200]}"]}
    by = {r.get("id"): r for r in rows}
    # A check missing from zone-health's output (crashed, or renamed so --only
    # drops it) must not silently shrink the denominator: a missing check is a
    # broken reading, named. Audit 2026-11-04 (zai-7).
    missing = [c for c in ALIVE_CHECKS if c not in by]
    seen = [by[c] for c in ALIVE_CHECKS if c in by]
    fails = [r for r in seen if r.get("status") != "pass"]
    _, dw, _ = run(["systemctl", "is-active", "disk-watch.timer"])
    ev = [f"zone-health: {len(seen) - len(fails)} of {len(seen)} reported checks for this way pass"]
    ev += [f"FAIL {r['id']}: {r.get('summary')}" for r in fails]
    ev += [f"MISSING {c}: zone-health did not report this check" for c in missing]
    ev += [f"disk-watch.timer: {dw or 'absent'} (the one check that pushes an alert unasked, drive only)",
           "read at publish time; zone-health has no cadence of its own",
           "not watched: bills paid, runway known (hand reading only)"]
    violations = [{"check": r["id"], "summary": r.get("summary"), "items": (r.get("items") or [])[:10]} for r in fails]
    violations += [{"check": c, "summary": "zone-health did not report this check (crashed or renamed)", "items": []}
                   for c in missing]
    if missing:
        return {"monitor": "broken", "invariant": "unknown",
                "last-observation": iso(NOW), "observed-at": iso(NOW),
                "violations": violations,
                "violation-count": len(fails) + len(missing), "evidence": ev,
                "denominator": f"{len(fails)} of {len(seen)} reported checks failing; "
                               f"{len(missing)} of {len(ALIVE_CHECKS)} checks missing from zone-health output; "
                               "bills and runway unwatched"}
    return {"monitor": "on", "invariant": "failing" if fails else "holding",
            "last-observation": iso(NOW), "observed-at": iso(NOW),
            "violations": violations,
            "violation-count": len(fails), "evidence": ev,
            "denominator": f"{len(fails)} of {len(seen)} zone-health checks failing; bills and runway unwatched"}

# E-autoclock-wow ("every dispatch names its META item",
# holes/excursions/E-autoclock-wow.md): the gate (`admit-request` at the
# `create-invoke-job!` seam) is designed but not built, so this probe reports
# the gate's state from facts that exist today, not from the gate:
#   1. whether `admit-request` appears in the `create-invoke-job!` body;
#   2. how many `E-kimi-task-*.md` excursions were minted in the last 24 h
#      (the per-dispatch-excursion workaround the gate should replace).
# With no gate the reading is on/failing, never holding: the way's invariant
# ("a request without an agreed requisition is refused") cannot hold while
# nothing refuses anything (audit 2026-11-04; fix list item 8).
AUTOCLOCK_WINDOW = datetime.timedelta(hours=24)
AUTOCLOCK_REPO = HOME / "code" / "futon2"
AUTOCLOCK_SEAM = HOME / "code" / "futon3c" / "src" / "futon3c" / "transport" / "http.clj"
GATE_MARKER = "admit-request"
SEAM_NAME = "create-invoke-job!"

def recent_minted_kimi_tasks(now=NOW, repo=AUTOCLOCK_REPO, window=AUTOCLOCK_WINDOW):
    """E-kimi-task-*.md files added to the futon2 repo inside the window."""
    repo = pathlib.Path(repo)
    rc, out, err = run(["git", "log", "--diff-filter=A", "--name-only", "--format=",
                        f"--since={iso(now - window)}", "--",
                        "holes/excursions/E-kimi-task-*.md"], cwd=repo, timeout=120)
    if rc != 0:
        raise RuntimeError(f"git log over {repo} failed: {err[:200]}")
    return sorted({line.strip() for line in out.splitlines() if line.strip()})

def gate_present_at_seam(seam=AUTOCLOCK_SEAM, marker=GATE_MARKER, name=SEAM_NAME):
    """True when marker is called inside the seam def's own body."""
    text = pathlib.Path(seam).read_text()
    start = text.find(f"(defn {name}")
    if start < 0: start = text.find(f"(defn- {name}")
    if start < 0:
        raise RuntimeError(f"seam {name} not found in {seam}")
    rest = text[start:]
    nxt = re.search(r"^(\(defn |\(defn- |\(def )", rest[1:], re.M)
    body = rest[1:1 + nxt.start()] if nxt else rest
    code = "\n".join(line.split(";", 1)[0] for line in body.splitlines())
    return marker in code

def probe_autoclock_gate(now=NOW, repo=AUTOCLOCK_REPO, seam=AUTOCLOCK_SEAM):
    violations = []
    try:
        gate = gate_present_at_seam(seam)
        mints = recent_minted_kimi_tasks(now, repo)
    except Exception as error:  # never turn missing evidence into holding
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(now),
                "last-observation": None, "violations": [], "violation-count": 0,
                "evidence": [f"autoclock-gate read failed: {error}"],
                "denominator": "unavailable because a required read failed"}
    if not gate:
        violations.append({"check": "gate-not-built",
                           "summary": f"no {GATE_MARKER} at the {SEAM_NAME} seam; nothing refuses a request without an agreed requisition (owner: claude-4, E-autoclock-wow)"})
    if mints:
        violations.append({"check": "minted-excursion-workaround",
                           "summary": f"{len(mints)} E-kimi-task-* excursions minted in the last 24 h; each is an agent-minted target with no operator turn behind it",
                           "files": mints[:10]})
    return {"monitor": "on",
            "invariant": "holding" if (gate and not mints) else "failing",
            "last-observation": iso(now), "observed-at": iso(now),
            "violations": violations, "violation-count": len(violations),
            "evidence": [f"gate {GATE_MARKER} at {SEAM_NAME}: {'present' if gate else 'absent (designed, zero lines; E-autoclock-wow gate design)'}",
                         f"E-kimi-task-* minted in the last 24 h: {len(mints)}",
                         "counts 1 and 4 of the excursion's Probe section need per-job requisition data only the gate will write; this probe reads what exists today"],
            "denominator": f"{len(mints)} minted-excursion files in 24 h; gate {'present' if gate else 'absent'}"}

RESULTS_DIR = HOME / ".cache" / "futon3c" / "ways-of-working" / "results"

def build_probe_registry(summary=None, graph=None):
    operator_cache = {}
    def shared_operator_census():
        if "value" not in operator_cache:
            operator_cache["value"] = read_operator_census()
        return operator_cache["value"]
    return {"git-clean": lambda: probe_git_clean(),
            "cascade-standards": lambda: probe_cascade_standards(summary),
            "draw-pile": lambda: probe_draw_pile(graph),
            "feedback-reaches": lambda: probe_feedback_reaches(shared_operator_census),
            "intents-tracked": lambda: probe_intents_tracked(),
            "operator-turns": lambda: probe_operator_turns(census=shared_operator_census()),
            "zone-health": lambda: probe_zone_health(),
            "autoclock-gate": lambda: probe_autoclock_gate()}

def broken_live(exc):
    return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
            "violations": [], "violation-count": 0,
            "evidence": [f"probe error: {exc}"]}

def run_probe(name, registry):
    try: return registry[name]()
    except Exception as e:  # noqa: BLE001
        return broken_live(e)

def write_probe_result(results_dir, name, live):
    results_dir = pathlib.Path(results_dir)
    results_dir.mkdir(parents=True, exist_ok=True)
    path = results_dir / (name + ".json")
    tmp = path.with_name(path.name + ".tmp." + str(os.getpid()))
    payload = {"probe": name, "observed-at": live.get("observed-at") or iso(NOW), "live": live}
    with open(tmp, "w") as fh: json.dump(payload, fh, indent=1)
    os.replace(tmp, path)
    return path

def read_probe_result(results_dir, name):
    try:
        data = json.loads((pathlib.Path(results_dir) / (name + ".json")).read_text())
        live = data["live"]
        if not isinstance(live, dict): raise ValueError("live is not a dict")
        live["observed-at"] = data.get("observed-at") or live.get("observed-at")
        return live
    except (OSError, ValueError, KeyError, TypeError):
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0,
                "evidence": [f"no result yet for probe {name}"]}

def assemble_document(decl_path, results_dir, registry_names=()):
    """Build the ways-of-working-live-v1 document from per-probe result files."""
    d = json.loads(pathlib.Path(decl_path).read_text())
    ways, observed = [], []
    for w in d["ways"]:
        w = dict(w)
        p = w.get("probe", "none")
        if p in registry_names:
            w["live"] = read_probe_result(results_dir, p)
            first = (w["live"].get("evidence") or [""])[0]
            if not str(first).startswith("no result yet"):
                observed.append(w["live"].get("observed-at"))
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
    # never claim fresher than the stalest part: probed-at is the oldest observed-at read
    return {"schema": "ways-of-working-live-v1", "declared": d["as-of"],
            "probed-at": min(observed) if observed else iso(NOW),
            "states": d["states"], "counts": counts,
            "provenance": d["provenance"], "ways": ways}

def write_document(out, doc):
    pathlib.Path(out).write_text(json.dumps(doc, indent=1))
    print(f"wrote {out}: " + ", ".join(f"{k} {v}" for k, v in sorted(doc["counts"].items())))

def _parse_args(args):
    flags, positional, i = {}, [], 0
    value_flags = ("--summary", "--graph", "--run", "--results-dir")
    while i < len(args):
        token = args[i]
        if token in value_flags:
            flags[token] = args[i + 1]; i += 2
        elif token == "--assemble":
            flags["--assemble"] = True; i += 1
        else:
            positional.append(token); i += 1
    return flags, positional

def main(argv=None):
    args = sys.argv[1:] if argv is None else argv
    flags, positional = _parse_args(args)
    summary = json.loads(pathlib.Path(flags["--summary"]).read_text()) if "--summary" in flags else None
    graph = json.loads(pathlib.Path(flags["--graph"]).read_text()) if "--graph" in flags else None
    results_dir = pathlib.Path(flags.get("--results-dir", RESULTS_DIR))
    decl = pathlib.Path(positional[0])
    d = json.loads(decl.read_text())
    declared_probes = {w.get("probe", "none") for w in d["ways"]}
    if "--run" in flags:
        name = flags["--run"]
        if name not in declared_probes:
            sys.exit(f"no way declares probe {name}")
        registry = build_probe_registry(summary, graph)
        live = run_probe(name, registry)
        path = write_probe_result(results_dir, name, live)
        print(f"wrote {path}: {live['monitor']}/{live['invariant']} at {live.get('observed-at')}")
        return
    registry = build_probe_registry(summary, graph)
    if not flags.get("--assemble"):
        # full run: run each declared probe through the same code as --run,
        # then assemble, so the two paths cannot drift apart
        for w in d["ways"]:
            name = w.get("probe", "none")
            if name in registry:
                write_probe_result(results_dir, name, run_probe(name, registry))
    doc = assemble_document(decl, results_dir, registry_names=set(registry))
    write_document(positional[1], doc)

if __name__ == "__main__":
    main()
