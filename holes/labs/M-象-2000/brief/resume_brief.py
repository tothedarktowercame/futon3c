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

def load_records(directory, mission, seats, since=None):
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
        rows.append(row)
    matched = [r for r in rows if r.get("mission_id") == mission]
    if matched:
        return matched, "mission"
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
        block, j = [], i + 1
        while j < len(lines) and not lines[j].startswith(">"):
            if lines[j].startswith("## "):
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
    data = {k: [] for k in ("owed", "asked", "in_force", "adopted", "obligations", "closed")}
    record_ids = []
    for r in latest.values():
        record_ids.append(r.get("evidence_id") or r.get("turn_id"))
        sid = r["session_id"]
        for p in r["ports"].get("still_open") or []:
            row, source = dict(p), acts.get(aid(p), {})
            for k in ("author", "kind", "text", "at"):
                row[k] = source.get(k, row.get(k))
            row["session_id"] = sid
            data["asked" if row.get("author") == "operator" else "owed"].append(row)
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
            if now - instant(row.get("at")) <= dt.timedelta(days=7):
                data["closed"].append(row)
    for key in data:
        data[key] = unique(data[key])
    data["in_force"].sort(key=lambda x: (x.get("kind") != "constrain",
                                         -instant(x.get("at")).timestamp()))
    data["record_ids"] = sorted(set(filter(None, record_ids)))
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

def render(mission, join, seed_rows, data, now):
    lines = ["# Resume brief: " + mission, ""]
    if join == "seat":
        lines += ["_Join: explicit seats fallback; not a mission-id join._", ""]
    sections = [
        ("Seed", seed_rows),
        ("Owed to the operator", [port_line(x, now) for x in data["owed"]]),
        ("Asked by the operator, no answer linked", [port_line(x, now) for x in data["asked"]]),
        ("In force", [act_line(x) for x in data["in_force"]]),
        ("Adopted, not applied", [act_line(x) for x in data["adopted"]]),
        ("Obligations", ["- {0} → {1}: {2}".format(
            x.get("debtor", "unknown"), x.get("creditor", "unknown"),
            shortened(x.get("source_text") or x.get("text") or x.get("source")))
            for x in data["obligations"]]),
    ]
    closed = []
    for x in data["closed"]:
        witness = x.get("inferred")
        closed.append(port_line(x, now) + ((" _(inferred: {0})_".format(witness)) if witness else ""))
    sections.append(("Recently closed", closed))
    for title, rows in sections:
        lines += ["## {0} ({1})".format(title, len(rows)), ""]
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
        "owed_to_operator": [aid(x) for x in data["owed"]],
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
    args = p.parse_args(argv)
    seat_text = args.seats or args.seat or ""
    records, join = load_records(args.dir, args.mission,
                                 set(filter(None, seat_text.split(","))), args.since)
    now = dt.datetime.now(dt.timezone.utc)
    data = prepare(records, now)
    output = render(args.mission, join,
                    seeds(ROOT / "holes/missions" / (args.mission + ".md")),
                    data, now)
    if args.out:
        args.out.write_text(output)
    print(output, end="")
    write_measurement(args.dir, args.mission, args.seat, now, data)

if __name__ == "__main__":
    main()
