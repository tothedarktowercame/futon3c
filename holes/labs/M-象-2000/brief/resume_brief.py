#!/usr/bin/env python3
"""Render a classical mission resume brief from turn records."""
import argparse
import datetime as dt
import importlib.util
import json
import os
import re
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
DEFAULT_DIR = Path.home() / ".emacs-graph/session-turn-analysis"
DEFAULT_MOVES_GLOB = ("holes/labs/M-象-2000/data/moves-scale*/shard-*/"
                      "turn-*.moves.json")
# minus40 (40ºBelowDB): the memory-file facts with their said/unsaid times.
# Read through its own m40.py so the brief shares its store, cache and
# true-at rule rather than keeping a second copy of either.
M40_SCRIPT = ROOT.parent / "minus40/scripts/m40.py"
FACT_CAP = 15
M40_BASE = "http://127.0.0.1:7093"
AGENCY_BASE = os.environ.get("FUTON3C_AGENCY_BASE", "http://localhost:7070")
# A bell recorded as a 象 turn gets act ids <job-id>-b-<n>; the job's state at
# the Agency says whether the ask was answered (E-agency-work-orders W1).
JOB_ACT = re.compile(r"^(invoke-\d+-\d+-[0-9a-f]+)-b-\d+$")
# session_id -> [(created_at, turn_id, agent_id, text, record_id)] over every
# record load_records parsed, joined or not: the writing-turn lookup for facts.
SESSION_INDEX = {}

def instant(s):
    """Parse S as an aware UTC instant. A bare date or naive time (as --since
    takes) is read as UTC so it compares with the records' Z-suffixed stamps."""
    if not s:
        return dt.datetime.min.replace(tzinfo=dt.timezone.utc)
    t = dt.datetime.fromisoformat(s.replace("Z", "+00:00"))
    return t if t.tzinfo else t.replace(tzinfo=dt.timezone.utc)


def load_moves(pattern=DEFAULT_MOVES_GLOB, root=ROOT):
    """Index labelled sidecar acts by act id -> {"within", "quote", "move"}.

    Reads every file matching the glob (relative to ROOT or absolute),
    skipping unreadable ones; a malformed file contributes nothing.
    """
    index = {}
    base = Path(root)
    for path in sorted(base.glob(pattern) if not Path(pattern).is_absolute()
                       else Path(pattern).parent.glob(Path(pattern).name)):
        try:
            sidecar = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError, ValueError):
            continue
        for act in sidecar.get("acts") or []:
            act_id = act.get("act") if isinstance(act, dict) else None
            if act_id:
                index[act_id] = {"within": (act.get("scope") or {}).get("within"),
                                 "quote": act.get("quote"), "move": act.get("move")}
    return index

def aid(x):
    return x if isinstance(x, str) else x.get("act", x.get("id", ""))

def mission_excursions(path):
    """Excursions the mission DECLARES as its own: a line in the mission file
    `**Excursions:** E-a, E-b` (or `Excursions: …`). Mentions elsewhere in the
    file are not a declaration: a mission names other teams' excursions in
    passing, and joining on those pulled 57 sessions into one brief."""
    if not path.exists():
        return []
    for line in path.read_text().splitlines():
        m = re.match(r"^\**Excursions:\**\s*(.*)$", line.strip())
        if m:
            return list(dict.fromkeys(re.findall(r"\bE-[A-Za-z0-9-]+\b", m.group(1))))
    return []

def load_records(directory, mission, seats, since=None, join="auto", excursions=()):
    rows = []
    for path in sorted(directory.glob("turn-*.json")):
        try:
            row = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        if not isinstance(row, dict):
            continue
        if not row.get("turn_id") or not row.get("session_id"):
            continue
        if row.get("origin", "operator") == "operator":
            SESSION_INDEX.setdefault(row["session_id"], []).append(
                (row.get("created_at") or "", row.get("turn_id"), row.get("agent_id"),
                 (row.get("source_text") or "")[:200], path.name.split(".", 1)[0]))
        if since and instant(row.get("created_at")) < instant(since):
            continue
        row["_record_id"] = path.name.split(".", 1)[0]
        rows.append(row)
    mission_rows = [r for r in rows if r.get("mission_id") == mission]
    if join == "mission":
        return mission_rows, "mission"
    matched = [r for r in rows
               if r.get("mission_id") == mission or r.get("excursion_id") in excursions]
    if join == "auto" and matched:
        return matched, "mission-excursion"
    if not seats:
        raise SystemExit("no mission_id records found; provide --seats")
    return [r for r in rows if r.get("agent_id") in seats], "seat"

def seeds(path):
    if not path.exists():
        return []
    lines, found = path.read_text().splitlines(), []
    for i, line in enumerate(lines):
        if "**Operator-voice anchor" not in line:
            continue
        date = re.search(r"(20\d\d-\d\d-\d\d)", line)
        quoted = re.search(r'"([^"]+)"', line)
        block, j = ([quoted.group(1)] if quoted else []), i + 1
        if not block:
            while j < len(lines) and not lines[j].startswith(">"):
                if lines[j].startswith("## ") or "**Operator-voice anchor" in lines[j]:
                    break
                j += 1
            while j < len(lines) and lines[j].startswith(">"):
                block.append(lines[j])
                j += 1
        if block:
            found.append((date.group(1) if date else "", "\n".join(block)))
    return [x[1] for x in sorted(found, reverse=True)[:3]]

def unique(rows):
    out, seen = [], set()
    rows.sort(key=lambda x: instant(x.get("at") or x.get("since")), reverse=True)
    for row in rows:
        key = aid(row)
        if key and key not in seen:
            seen.add(key)
            out.append(row)
    return out

def job_of(act_id):
    m = JOB_ACT.match(act_id or "")
    return m.group(1) if m else None

def agency_job_state(job_id, base=AGENCY_BASE, timeout=2.0, _cache={}):
    """(state, finished-at) of an Agency job; (None, None) when the Agency
    does not answer or does not know it. Cached for the process."""
    if job_id not in _cache:
        try:
            with urllib.request.urlopen(base + "/api/alpha/invoke/jobs/" + job_id,
                                        timeout=timeout) as r:
                d = json.load(r)
            job = d.get("job") or d
            _cache[job_id] = (job.get("state"), job.get("finished-at"))
        except Exception:
            _cache[job_id] = (None, None)
    return _cache[job_id]

ENDED = ("done", "failed", "cancelled", "canceled")

def prepare(records, now, job_state=None):
    """JOB_STATE, when given, maps a job id to (state, finished-at); an open
    bell ask whose job has ended moves to the closed list with the job's
    state as its witness, since the bellback is the answer."""
    acts = {a.get("id"): a for r in records for a in (r.get("acts") or [])}
    latest = {}
    for r in records:
        if not isinstance(r.get("ports"), dict):
            continue
        sid = r["session_id"]
        if sid not in latest or instant(r.get("created_at")) > instant(latest[sid].get("created_at")):
            latest[sid] = r
    data = {k: [] for k in ("awaiting_you", "awaiting_others", "asked",
                             "in_force", "adopted", "obligations", "closed")}
    for r in latest.values():
        sid = r["session_id"]
        for p in r["ports"].get("still_open") or []:
            row, source = dict(p), acts.get(aid(p), {})
            for k in ("author", "kind", "text", "at"):
                row[k] = source.get(k, row.get(k))
            row["session_id"] = sid
            row["session_agent"] = r.get("agent_id", "unknown")
            row["session_last"] = r.get("created_at")
            job = job_of(aid(row))
            if job and job_state:
                state, finished = job_state(job)
                if state in ENDED:
                    row["inferred"] = "job {0}{1}".format(
                        state, (" " + finished[:16] + "Z") if finished else "")
                    row["closed_by_job"] = job
                    data["closed"].append(row)
                    continue
            if row.get("author") == "operator":
                data["asked"].append(row)
            elif row.get("author") == r.get("agent_id"):
                data["awaiting_you"].append(row)
            else:
                data["awaiting_others"].append(row)
        standings = r.get("standings") or {}
        for source_key, dest in (("in_force", "in_force"),
                                 ("adopted_not_applied", "adopted")):
            for identifier in standings.get(source_key) or []:
                row = dict(acts.get(identifier, {"id": identifier}))
                row.setdefault("at", r.get("created_at"))
                data[dest].append(row)
        for ob in r.get("obligations") or []:
            row = dict(ob)
            row.setdefault("id", row.get("act") or row.get("source_act"))
            if row.get("source") in acts:
                row["source_text"] = acts[row["source"]].get("text")
                row["id"] = row["source"]
            row.setdefault("at", r.get("created_at"))
            data["obligations"].append(row)
    for r in records:
        if not isinstance(r.get("ports"), dict):
            continue
        if now - instant(r.get("created_at")) > dt.timedelta(days=7):
            continue
        sid = r["session_id"]
        inferred = {}
        for witness in r["ports"].get("inferred") or []:
            if witness.get("effect") == "closes":
                inferred.setdefault(witness.get("opener"), []).append(witness.get("rule"))
        for c in r["ports"].get("closed_this_turn") or []:
            row = dict(c) if isinstance(c, dict) else {"act": c}
            source = acts.get(aid(row), {})
            for k in ("author", "kind", "text", "at"):
                row[k] = source.get(k, row.get(k))
            row.setdefault("at", r.get("created_at"))
            row["session_id"] = sid
            if not row.get("inferred") and inferred.get(aid(row)):
                row["inferred"] = ", ".join(sorted(set(inferred[aid(row)])))
            data["closed"].append(row)
    for key in data:
        data[key] = unique(data[key])
    data["in_force"].sort(key=lambda x: (x.get("kind") != "constrain",
                                         -instant(x.get("at")).timestamp()))
    data["record_ids"] = sorted(set(r["_record_id"] for r in records))
    data["record_count"] = len(records)
    data["session_count"] = len(set(r["session_id"] for r in records))
    data["newest"] = max((instant(r.get("created_at")) for r in records), default=None)
    return data

def shortened(text):
    text = " ".join(str(text or "").split())
    return text if len(text) <= 120 else text[:119] + "…"

def port_line(row, now):
    when = instant(row.get("at") or row.get("since"))
    age = max(0, (now.date() - when.date()).days)
    return "- {kind}: {text} — {date}, {sid}, {age}d".format(
        kind=row.get("kind", "unknown"), text=shortened(row.get("text")),
        date=when.date(), sid=str(row.get("session_id", "unknown"))[:8], age=age)

def act_line(row):
    return "- {0}: {1}".format(row.get("kind", "unknown"),
                                shortened(row.get("text") or row.get("source_text")))

def port_section(rows, now):
    stale, recent = {}, []
    for row in rows:
        if now - instant(row.get("session_last")) > dt.timedelta(days=14):
            stale.setdefault(row["session_id"], []).append(row)
        else:
            recent.append(row)
    output = []
    for sid, items in sorted(
            stale.items(), key=lambda pair: instant(pair[1][0].get("session_last")),
            reverse=True):
        sample = items[0]
        output.append("- {0}, session {1}, last turn {2}: {3} items".format(
            sample.get("session_agent", "unknown"), sid[:8],
            instant(sample.get("session_last")).date(), len(items)))
    by_kind = {}
    for row in recent:
        by_kind.setdefault(row.get("kind", "unknown"), []).append(row)
    ordered = sorted(by_kind.items(),
                     key=lambda pair: instant(pair[1][0].get("at") or pair[1][0].get("since")),
                     reverse=True)
    for kind, items in ordered:
        output += ["### {0} ({1})".format(kind, len(items)), ""]
        output += [port_line(row, now) for row in items[:8]]
        if len(items) > 8:
            oldest = instant(items[-1].get("at") or items[-1].get("since")).date()
            output.append("- … and {0} more, oldest {1}".format(len(items) - 8, oldest))
        output.append("")
    return output

def load_m40(script=M40_SCRIPT):
    """minus40's m40.py as a module, or None when that checkout is absent."""
    if not Path(script).exists():
        return None
    spec = importlib.util.spec_from_file_location("m40", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

def store_answers(base, timeout=2.0):
    try:
        with urllib.request.urlopen(base + "/health", timeout=timeout) as r:
            return r.status == 200
    except Exception:
        return False

def read_facts(m40=None, base=None):
    """(rows, note). Rows come from the store when it answers (through m40's
    ten-minute cache); when it does not, from the last cache on disk, with a
    note saying so; otherwise none, with the reason. Never raises: a brief
    must render whether or not minus40 is up."""
    m40 = m40 or load_m40()
    if m40 is None:
        return [], "not read (minus40 checkout absent at %s)" % M40_SCRIPT
    base = base or m40.BASE
    try:
        if store_answers(base):
            return m40.facts(), None
        if os.path.exists(m40.CACHE):
            with open(m40.CACHE) as fh:
                rows = json.load(fh)
            cached = dt.datetime.fromtimestamp(os.path.getmtime(m40.CACHE), dt.timezone.utc)
            return rows, "store at %s not answering; cache of %s" % (
                base, cached.strftime("%Y-%m-%dT%H:%MZ"))
        return [], "not read (store at %s not answering, no cache)" % base
    except Exception as e:  # a brief never fails on its facts
        return [], "not read (%s)" % e

def fact_said(f):
    return f.get("again") or f.get("said") or ""

def mission_facts(rows, terms, now, since=None, cap=FACT_CAP):
    """Facts true at NOW whose text or memory id mentions any of TERMS
    (case-insensitive), said at or after SINCE when given, newest first,
    at most CAP. The fact's stored times are ISO-8601 Z strings, so the
    comparison is on strings in that form, as m40's own true_at does."""
    t = now.astimezone(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    floor = (since + "T00:00:00Z" if since and len(since) == 10 else since) or ""
    wanted = [w.lower() for w in terms if w]
    out = []
    for f in rows:
        said = f.get("said") or ""
        if not said or said > t or (f.get("unsaid") and f["unsaid"] <= t):
            continue
        if floor and fact_said(f) < floor:
            continue
        hay = (f.get("text", "") + " " + f.get("memory", "")).lower()
        if wanted and not any(w in hay for w in wanted):
            continue
        out.append(f)
    out.sort(key=fact_said, reverse=True)
    return out[:cap]

def fact_line(f):
    stem = (f.get("memory") or "").split("/", 1)[-1]
    line = "- {0} {1}: {2}".format(fact_said(f)[:10], stem, shortened(f.get("text", "")))
    turn = f.get("_turn")
    if turn:
        line += "\n  ↳ written during {0} ({1}), after the operator said: “{2}”".format(
            turn.get("turn_id"), turn.get("agent") or "?", shortened(turn.get("text") or ""))
    return line

def read_versions(base=M40_BASE, timeout=3.0):
    """minus40's :memory/version edges as (memory, at, session); the ingest
    hook records the writing session since 2026-10-11. Empty when the store
    does not answer."""
    try:
        req = urllib.request.Request(
            base + "/api/alpha/hyperedges?type=memory/version&limit=1000",
            headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=timeout) as r:
            d = json.load(r)
    except Exception:
        return []
    out = []
    for h in d.get("hyperedges", []):
        p = h.get("hx/props") or {}
        out.append(((p.get("roles") or {}).get("memory"),
                    p.get("at") or h.get("prop/at") or "", p.get("session")))
    return out

def writing_turn(fact, versions, index, slack_s=120):
    """The operator turn during which FACT was written, or None. The version
    of the fact's memory nearest at or before the fact's said time (within
    SLACK_S after it, since the hook stamps the file's mtime) names the
    writing session; the latest operator record in that session at or before
    that time is the turn. Both halves are classical lookups."""
    said = fact_said(fact)
    memory = fact.get("memory")
    if not said or not memory:
        return None
    limit = (instant(said) + dt.timedelta(seconds=slack_s)).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
    hits = sorted(v for v in versions if v[0] == memory and v[1] and v[1] <= limit and v[2])
    if not hits:
        return None
    session = hits[-1][2]
    turns = sorted(t for t in index.get(session, []) if t[0] and t[0] <= limit)
    if not turns:
        return None
    at, turn_id, agent, text, record_id = turns[-1]
    return {"turn_id": turn_id, "agent": agent, "text": text, "record_id": record_id,
            "at": at, "session_id": session}

def render(mission, join, seed_rows, data, now, excursions=()):
    lines = ["# Resume brief: " + mission, ""]
    if join == "seat":
        lines += ["_Join: explicit seats fallback; not a mission-id join._", ""]
    elif join == "mission":
        lines += ["_Join: mission_id {0}._".format(mission), ""]
    else:
        suffix = (" + excursions " + ", ".join(excursions)) if excursions else ""
        lines += ["_Join: mission_id {0}{1}._".format(mission, suffix), ""]
    newest = data["newest"].date() if data["newest"] else "none"
    lines += ["_Read {0} records in {1} sessions, newest {2}._".format(
        data["record_count"], data["session_count"], newest), ""]
    sections = [
        ("Seed", seed_rows, len(seed_rows)),
        ("Awaiting you: agent proposals and asks", port_section(data["awaiting_you"], now),
         len(data["awaiting_you"])),
        ("Awaiting other agents", port_section(data["awaiting_others"], now),
         len(data["awaiting_others"])),
        ("Asked by the operator, no answer linked", port_section(data["asked"], now),
         len(data["asked"])),
        ("In force", [act_line(x) for x in data["in_force"]], len(data["in_force"])),
        ("Adopted, not applied", [act_line(x) for x in data["adopted"]], len(data["adopted"])),
        ("Obligations", ["- {0} → {1}: {2}".format(
            x.get("debtor", "unknown"), x.get("creditor", "unknown"),
            shortened(x.get("source_text") or x.get("text") or x.get("source")))
            for x in data["obligations"]], len(data["obligations"])),
    ]
    closed = []
    for x in data["closed"]:
        witness = x.get("inferred")
        closed.append(port_line(x, now) + ((" _(inferred: {0})_".format(witness)) if witness else ""))
    sections.append(("Recently closed", closed, len(data["closed"])))
    moves = data.get("_moves") or {}
    acts = data.get("_acts") or {}
    binder = {"with": 0, "without": 0, "unlabelled": 0}
    section_rows = {"Awaiting you: agent proposals and asks": data["awaiting_you"],
                    "Awaiting other agents": data["awaiting_others"],
                    "Asked by the operator, no answer linked": data["asked"],
                    "Recently closed": data["closed"]}
    for title, rows, count in sections:
        lines += ["## {0} ({1})".format(title, count), ""]
        lines += rows or ["_None._"]
        for row in section_rows.get(title, []):
            entry = moves.get(aid(row))
            if entry is None:
                binder["unlabelled"] += 1
                continue
            within = entry.get("within")
            if not within:
                binder["without"] += 1
                continue
            binder["with"] += 1
            opener = acts.get(within) or {}
            text = opener.get("text") or entry.get("quote") or within
            lines.append("  within: {0} “{1}” ({2})".format(
                opener.get("kind", "unknown"), shortened(text), within))
        lines.append("")
    data["within_binders"] = binder
    total = binder["with"] + binder["without"] + binder["unlabelled"]
    lines += ["_Binders: {0} of {1} items carry a within link; {2} labelled without one; "
              "{3} unlabelled._".format(binder["with"], total, binder["without"],
                                        binder["unlabelled"]), ""]
    facts = data.get("facts", [])
    lines += ["## Facts ({0})".format(len(facts)), ""]
    note = data.get("facts_note")
    if note:
        lines.append("_facts: {0}_".format(note))
    lines += [fact_line(f) for f in facts] or ([] if note else ["_None._"])
    lines.append("")
    return "\n".join(lines)

def write_measurement(directory, mission, seat, now, data):
    target = directory / "briefs"
    target.mkdir(parents=True, exist_ok=True)
    stamp = now.strftime("%Y%m%dT%H%M%SZ")
    path = target / ("brief-{0}-{1}-{2}.json".format(mission, seat or "all", stamp))
    sections = {
        "seed": [],
        "awaiting_you": [aid(x) for x in data["awaiting_you"]],
        "awaiting_other_agents": [aid(x) for x in data["awaiting_others"]],
        "no_answer_linked": [aid(x) for x in data["asked"]],
        "in_force": [aid(x) for x in data["in_force"]],
        "adopted_not_applied": [aid(x) for x in data["adopted"]],
        "obligations": [aid(x) for x in data["obligations"]],
        "recently_closed": [aid(x) for x in data["closed"]],
        "facts": [f.get("id") for f in data.get("facts", [])],
    }
    binder = data.get("within_binders") or {}
    payload = {"mission": mission, "seat": seat,
               "within": {"with": binder.get("with", 0),
                          "without": binder.get("without", 0),
                          "unlabelled": binder.get("unlabelled", 0)},
               "at": now.isoformat().replace("+00:00", "Z"),
               "sections": sections, "record_ids": data["record_ids"]}
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n")
    return path

def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("--mission", required=True)
    p.add_argument("--seat")
    p.add_argument("--seats")
    p.add_argument("--since")
    p.add_argument("--dir", type=Path, default=DEFAULT_DIR)
    p.add_argument("--out", type=Path)
    p.add_argument("--join", choices=["auto", "mission", "seats"], default="auto",
                   help="auto: mission or listed excursion when any matches, else seats")
    p.add_argument("--excursions",
                   help="comma-separated excursion ids; defaults to E-* tokens in the mission file")
    p.add_argument("--moves-glob", default=DEFAULT_MOVES_GLOB,
                   help="glob of moves sidecars to read within links from")
    p.add_argument("--no-facts", action="store_true",
                   help="skip the minus40 Facts section (no store contact)")
    p.add_argument("--no-agency", action="store_true",
                   help="do not ask the Agency whether open bell asks' jobs have ended")
    p.add_argument("--fact-terms",
                   help="comma-separated extra terms a fact may mention; "
                        "the mission id and excursions always count")
    args = p.parse_args(argv)
    mission_file = ROOT / "holes/missions" / (args.mission + ".md")
    excursions = (list(filter(None, args.excursions.split(","))) if args.excursions
                  else mission_excursions(mission_file))
    seat_text = args.seats or args.seat or ""
    records, join = load_records(args.dir, args.mission,
                                 set(filter(None, seat_text.split(","))), args.since,
                                 args.join, set(excursions))
    now = dt.datetime.now(dt.timezone.utc)
    data = prepare(records, now, None if args.no_agency else agency_job_state)
    data["_moves"] = load_moves(args.moves_glob)
    data["_acts"] = {a.get("id"): a for r in records for a in (r.get("acts") or [])}
    if args.no_facts:
        data["facts"], data["facts_note"] = [], "not read (--no-facts)"
    else:
        rows, note = read_facts()
        terms = [args.mission] + list(excursions) + list(
            filter(None, (args.fact_terms or "").split(",")))
        data["facts"] = mission_facts(rows, terms, now, args.since)
        data["facts_note"] = note
        versions = read_versions() if data["facts"] else []
        for f in data["facts"]:
            f["_turn"] = writing_turn(f, versions, SESSION_INDEX)
    output = render(args.mission, join, seeds(mission_file), data, now, excursions)
    if args.out:
        args.out.write_text(output)
    print(output, end="")
    write_measurement(args.dir, args.mission, args.seat, now, data)

if __name__ == "__main__":
    main()
