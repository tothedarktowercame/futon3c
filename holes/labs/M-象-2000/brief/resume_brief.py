#!/usr/bin/env python3
"""Render a classical mission resume brief from turn records."""
import argparse
import datetime as dt
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
DEFAULT_DIR = Path.home() / ".emacs-graph/session-turn-analysis"

def instant(s):
    return (dt.datetime.fromisoformat(s.replace("Z", "+00:00")) if s
            else dt.datetime.min.replace(tzinfo=dt.timezone.utc))

def aid(x):
    return x if isinstance(x, str) else x.get("act", x.get("id", ""))

def mission_excursions(path):
    if not path.exists():
        return []
    return list(dict.fromkeys(re.findall(r"\bE-[A-Za-z0-9-]+\b", path.read_text())))

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

def prepare(records, now):
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
    for title, rows, count in sections:
        lines += ["## {0} ({1})".format(title, count), ""]
        lines += rows or ["_None._"]
        lines.append("")
    lines += ["## Facts (0)", "",
              "facts: not read (no minus40 fact read in this version)", ""]
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
        "facts": [],
    }
    payload = {"mission": mission, "seat": seat,
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
    args = p.parse_args(argv)
    mission_file = ROOT / "holes/missions" / (args.mission + ".md")
    excursions = (list(filter(None, args.excursions.split(","))) if args.excursions
                  else mission_excursions(mission_file))
    seat_text = args.seats or args.seat or ""
    records, join = load_records(args.dir, args.mission,
                                 set(filter(None, seat_text.split(","))), args.since,
                                 args.join, set(excursions))
    now = dt.datetime.now(dt.timezone.utc)
    data = prepare(records, now)
    output = render(args.mission, join, seeds(mission_file), data, now, excursions)
    if args.out:
        args.out.write_text(output)
    print(output, end="")
    write_measurement(args.dir, args.mission, args.seat, now, data)

if __name__ == "__main__":
    main()
