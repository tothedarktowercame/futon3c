#!/usr/bin/env python3
"""Publish a compact, classical scope ledger for one 象 session."""
import argparse
import datetime as dt
import html
import json
import os
import re
from pathlib import Path
from urllib.parse import quote
from urllib.request import urlopen

RECORDS = Path.home() / ".emacs-graph" / "session-turn-analysis"
OUT = Path("/var/www/zone.hyperreal.enterprises/wip/turns")


def read_records(directory=RECORDS, agent=None, session=None):
    rows = []
    for path in Path(directory).glob("turn-*.json"):
        if path.name.endswith(".draft.json"):
            continue
        try:
            row = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        if not isinstance(row, dict):
            continue
        if agent and row.get("agent_id") != agent:
            continue
        if session and row.get("session_id") != session:
            continue
        rows.append(row)
    return sorted(rows, key=lambda r: r.get("created_at", ""))


def latest_session(records, agent):
    rows = read_records(records, agent=agent)
    return rows[-1].get("session_id") if rows else None


def fetch_scopes(session, turn_id, base="http://localhost:7070", opener=None):
    if opener is not None:
        return opener(session, turn_id)
    url = (f"{base}/api/alpha/xiang/scopes?session={quote(session)}"
           f"&turn={quote(turn_id)}")
    try:
        with urlopen(url, timeout=10) as response:
            if response.status == 404:
                return None
            if response.status != 200:
                return None
            return json.loads(response.read())
    except Exception:
        return None


def local_id(act_id, turn_id, earlier=False):
    text = str(act_id)
    match = re.search(r"-f-s(\d+)-(\d+)$", text)
    if match:
        number = match.group(1)
        suffix = "" if match.group(2) == "0" else "." + match.group(2)
        if text.startswith(turn_id + "-"):
            return "s" + number + suffix
        return "t%s:s%s%s" % (text.split("-f-", 1)[0].rsplit("-", 1)[-1], number, suffix)
    match = re.search(r"-r-(\d+)$", text)
    if match:
        number = match.group(1)
        if text.startswith(turn_id + "-"):
            return "r" + number
        return "t%s:r%s" % (text.split("-r-", 1)[0].rsplit("-", 1)[-1], number)
    if text.startswith(turn_id + "-"):
        tail = text.rsplit("-", 1)[-1]
        return ("s" if "-f-" in text else "r") + tail
    if "-f-" in text:
        return "t%s:s%s" % (text.split("-f-", 1)[0].rsplit("-", 1)[-1],
                              text.rsplit("-", 1)[-1])
    return text


def _tail(turn_id):
    return str(turn_id).rsplit("-", 1)[-1]


def _act_label(act, turn_id):
    return local_id(act["id"], turn_id)


def make_row(record, scope, previous_open=0):
    turn_id = record.get("turn_id", "")
    acts = (scope or {}).get("acts", []) if scope else []
    edges = (scope or {}).get("edges", []) if scope else []
    by_id = {a.get("id"): a for a in acts}
    within = {e["act"]: e for e in edges if e.get("rule") != "OPEN"}
    closes = [e for e in edges if e.get("rule") == "OPEN"]
    current = lambda a: a.get("turn") == turn_id or str(a.get("id", "")).startswith(turn_id + "-")
    operators = [a for a in acts if current(a) and (a.get("side") == "operator" or a.get("author") == "operator")]
    replies = [a for a in acts if current(a) and (a.get("side") == "agent" or a.get("author") not in ("operator", None))]

    def parent_token(edge):
        return _act_label(by_id.get(edge.get("within"), {"id": edge.get("within")}), turn_id)

    op_tokens = []
    for act in operators:
        aid = act.get("id")
        label = _act_label(act, turn_id)
        edge = within.get(aid)
        op_tokens.append(("⊢" + label) if not edge else (label + "↳" + parent_token(edge)))

    reply_text = record.get("reply_text") or ""
    reply_tokens = []
    for act in replies:
        label = _act_label(act, turn_id)
        start = int(act.get("start", 0))
        glyph = reply_text[start:start + 1] or (str(act.get("text", " "))[:1] or "·")
        edge = within.get(act.get("id"))
        token = glyph + ("↳" + parent_token(edge) if edge else "⊢" + label)
        for close in closes:
            if close.get("act") == act.get("id"):
                target = by_id.get(close.get("within"), {"id": close.get("within")})
                token += "✓" + _act_label(target, turn_id)
        reply_tokens.append(token)

    still_open = (record.get("ports") or {}).get("still_open", [])
    closed = (record.get("ports") or {}).get("closed_this_turn", [])
    open_count = len(still_open)
    opened = max(0, open_count - previous_open + len(closed))
    in_force = len((record.get("standings") or {}).get("in_force", []))
    line = (f"{record.get('created_at', '')[11:16]} {_tail(turn_id)}  "
            f"{'  '.join(op_tokens) or '·'} │ "
            f"{'  '.join(reply_tokens) or '·'} │ "
            f"open {open_count} (+{opened} −{len(closed)}) │ force {in_force}")
    json_row = {"turn_id": turn_id, "record_id": record.get("id"),
                "created_at": record.get("created_at"), "line": line,
                "operator": [{"id": a.get("id"), "kind": a.get("kind"),
                               "parent": within.get(a.get("id"), {}).get("within")} for a in operators],
                "replies": [{"id": a.get("id"), "kind": a.get("kind"),
                             "parent": within.get(a.get("id"), {}).get("within")} for a in replies],
                "closes": closes, "open_count": open_count,
                "opened": opened, "closed": len(closed), "in_force": in_force}
    return line, json_row, acts, edges


def build(records, agent, session, opener=None, base="http://localhost:7070"):
    rows, details, previous = [], [], 0
    for record in reversed(records):
        scope = fetch_scopes(session, record.get("turn_id", ""), base, opener)
        if scope is None:
            rows.append((record.get("created_at", "")[11:16] + " " + _tail(record.get("turn_id", "")) + "  ·", None))
            continue
        line, data, acts, edges = make_row(record, scope, previous)
        previous = data["open_count"]
        rows.append((line, data))
        details.append((data, acts, edges))
    return rows, details


def render(session, agent, rows, details, generated):
    lines = [f"Scope ledger  session={html.escape(session or '·')}  agent={html.escape(agent)}  generated={generated}  turns={len(rows)}",
             "legend: ⊢ root  ↳ within  ✓ closes  open count (delta)  force=in-force standings", ""]
    body = []
    for line, data in rows:
        body.append("<pre>" + html.escape(line) + "</pre>")
        if data is not None:
            acts = next((a for d, a, _ in details if d is data), [])
            body.append("<details><summary>acts</summary><ul>" + "".join(
                f"<li><code>{html.escape(str(a.get('id')))}</code> "
                f"{html.escape(str(a.get('kind')))}: {html.escape(str(a.get('text', ''))[:120])}</li>" for a in acts) + "</ul></details>")
    return "<!doctype html><meta charset='utf-8'><title>Scope ledger</title><style>body{font:14px monospace;white-space:normal}pre{margin:4px 0}details{margin:0 0 8px 2em;font:12px monospace}</style>" + "\n".join(lines + body)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--agent", required=True)
    parser.add_argument("--session")
    parser.add_argument("--records", type=Path, default=RECORDS)
    parser.add_argument("--out", type=Path, default=OUT)
    parser.add_argument("--limit", type=int, default=20)
    args = parser.parse_args()
    session = args.session or latest_session(args.records, args.agent)
    records = read_records(args.records, agent=args.agent, session=session)
    if args.limit > 0:
        records = records[-args.limit:]
    now = dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat()
    rows, details = build(records, args.agent, session)
    payload = {"session": session, "agent": args.agent, "generated_at": now,
               "turns": [data for _, data in rows if data is not None],
               "missing": [line for line, data in rows if data is None]}
    args.out.mkdir(parents=True, exist_ok=True)
    (args.out / f"scopes-{args.agent}.html").write_text(render(session, args.agent, rows, details, now), encoding="utf-8")
    (args.out / f"scopes-{args.agent}.json").write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
