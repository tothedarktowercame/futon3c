#!/usr/bin/env python3
"""Publish a compact, classical scope ledger for one 象 session."""
import argparse
import hashlib
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


def _sentence(act_id):
    m = re.search(r"-f-s(\d+)-(\d+)$", str(act_id))
    return (m.group(1), m.group(2)) if m else None


def _act_label(act, turn_id, multi=()):
    """s<n> for a sentence with one fragment; s<n>.<i> for every fragment of a
    sentence that has several (two fragments both printed as s1 made the
    line unreadable, 2026-10-15)."""
    label = local_id(act["id"], turn_id)
    sent = _sentence(act["id"])
    if sent and sent[0] in multi and "." not in label.rsplit(":", 1)[-1]:
        label += "." + sent[1]
    return label


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

    # sentences of this turn with more than one fragment get .<i> on every fragment
    counts = {}
    for a in operators:
        sent = _sentence(a.get("id"))
        if sent:
            counts[sent[0]] = counts.get(sent[0], 0) + 1
    multi = {n for n, c in counts.items() if c > 1}

    def parent_token(edge):
        return _act_label(by_id.get(edge.get("within"), {"id": edge.get("within")}), turn_id, multi)

    op_tokens = []
    for act in operators:
        aid = act.get("id")
        label = _act_label(act, turn_id, multi)
        edge = within.get(aid)
        op_tokens.append(("⊢" + label) if not edge else (label + "↳" + parent_token(edge)))

    reply_text = record.get("reply_text") or ""
    reply_tokens = []
    # commit acts (<turn>-c-<sha>) have no glyph in the reply text: count them
    commits = [a for a in replies if "-c-" in str(a.get("id", ""))]
    replies = [a for a in replies if "-c-" not in str(a.get("id", ""))]
    for act in replies:
        label = _act_label(act, turn_id, multi)
        start = int(act.get("start", 0))
        glyph = reply_text[start:start + 1] or (str(act.get("text", " "))[:1] or "·")
        edge = within.get(act.get("id"))
        token = glyph + ("↳" + parent_token(edge) if edge else "⊢" + label)
        for close in closes:
            if close.get("act") == act.get("id"):
                target = by_id.get(close.get("within"), {"id": close.get("within")})
                token += "✓" + _act_label(target, turn_id, multi)
        reply_tokens.append(token)
    if commits:
        reply_tokens.append("⎘%d" % len(commits))

    ports = record.get("ports")
    still_open = (ports or {}).get("still_open", [])
    closed = (ports or {}).get("closed_this_turn", [])
    open_count = len(still_open) if ports else None
    # opened this turn = what is open after it, less what was open before,
    # plus what closed in it (the closed ones were open before and are not now)
    opened = (max(0, open_count - previous_open + len(closed))
              if (ports and previous_open is not None) else None)
    in_force = len((record.get("standings") or {}).get("in_force", [])) if record.get("standings") else None
    fmt = lambda v: "·" if v is None else str(v)
    line = (f"{record.get('created_at', '')[11:16]} {_tail(turn_id)}  "
            f"{'  '.join(op_tokens) or '·'} │ "
            f"{'  '.join(reply_tokens) or '·'} │ "
            f"open {fmt(open_count)} (+{fmt(opened)} −{len(closed)}) │ force {fmt(in_force)}")
    json_row = {"turn_id": turn_id, "record_id": record.get("id"),
                "created_at": record.get("created_at"), "line": line,
                "operator": [{"id": a.get("id"), "kind": a.get("kind"),
                               "parent": within.get(a.get("id"), {}).get("within")} for a in operators],
                "replies": [{"id": a.get("id"), "kind": a.get("kind"),
                             "parent": within.get(a.get("id"), {}).get("within")} for a in replies],
                "closes": closes, "open_count": open_count,
                "opened": opened, "closed": len(closed), "in_force": in_force}
    return line, json_row, acts, edges


def fingerprint(record):
    """What of a record the row reads; a cached scope is reused while this holds."""
    keys = ("turn_id", "created_at", "reply_text", "ports", "standings", "analysis_status")
    text = json.dumps([record.get(k) for k in keys], sort_keys=True, default=str, ensure_ascii=False)
    return hashlib.sha1(text.encode("utf-8")).hexdigest()


def build(records, agent, session, opener=None, base="http://localhost:7070",
          cache=None, refetch_newest=1):
    """Rows newest first. The open delta needs the previous turn's open count,
    so the rows are built oldest first and reversed at the end (built newest
    first, the first turn had no predecessor and its whole open set read as
    opened: "+53", 2026-10-15).

    CACHE maps turn id to {"fingerprint", "scope"} and is updated in place.
    The route answers in about five seconds per turn, so twenty turns took
    two minutes and the page lagged a fresh turn by up to seven (2026-10-15);
    a turn whose record is unchanged reuses its cached scope, and the newest
    REFETCH_NEWEST records are always asked for again."""
    rows, details, previous = [], [], None
    total = len(records)
    for index, record in enumerate(records):          # records arrive oldest first
        turn_id = record.get("turn_id", "")
        stamp = fingerprint(record)
        cached = (cache or {}).get(turn_id)
        fresh = (cached is not None and cached.get("fingerprint") == stamp
                 and index < total - refetch_newest)
        scope = cached["scope"] if fresh else fetch_scopes(session, turn_id, base, opener)
        if scope is None:
            rows.append((record.get("created_at", "")[11:16] + " " + _tail(turn_id) + "  ·", None))
            continue
        if cache is not None:
            cache[turn_id] = {"fingerprint": stamp,
                              "scope": {"acts": scope.get("acts", []), "edges": scope.get("edges", [])}}
        line, data, acts, edges = make_row(record, scope, previous)
        if data["open_count"] is not None:
            previous = data["open_count"]
        rows.append((line, data))
        details.append((data, acts, edges))
    rows.reverse()
    details.reverse()
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
    parser.add_argument("--no-cache", action="store_true",
                        help="ask the route for every turn (the cache file beside the page is still rewritten)")
    args = parser.parse_args()
    session = args.session or latest_session(args.records, args.agent)
    records = read_records(args.records, agent=args.agent, session=session)
    if args.limit > 0:
        records = records[-args.limit:]
    now = dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat()
    cache_path = args.out / f"scopes-{args.agent}.cache.json"
    cache = {}
    if not args.no_cache:
        try:
            loaded = json.loads(cache_path.read_text())
            cache = loaded if isinstance(loaded, dict) and loaded.get("session") == session else {}
            cache = cache.get("turns", {}) if cache else {}
        except (OSError, json.JSONDecodeError):
            cache = {}
    rows, details = build(records, args.agent, session, cache=cache)
    wanted = {r.get("turn_id") for r in records}
    cache = {t: v for t, v in cache.items() if t in wanted}
    payload = {"session": session, "agent": args.agent, "generated_at": now,
               "turns": [data for _, data in rows if data is not None],
               "missing": [line for line, data in rows if data is None]}
    args.out.mkdir(parents=True, exist_ok=True)
    (args.out / f"scopes-{args.agent}.html").write_text(render(session, args.agent, rows, details, now), encoding="utf-8")
    (args.out / f"scopes-{args.agent}.json").write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    cache_path.write_text(json.dumps({"session": session, "turns": cache}, ensure_ascii=False) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
