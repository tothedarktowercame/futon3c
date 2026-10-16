#!/usr/bin/env python3
"""The memory layer over a whole session: one row per turn, one column per
minus40 memory that ever reached a turn of the session, and in each cell
the strongest thing the record shows for that memory at that turn.

  retrieved-only  the memory's name (and up to three matching lines) came
                  back from m40_find; its statements were not read
  reached         its statements came back (m40_state, m40_asof,
                  m40_history, m40_near): the text entered the context
  echoed          the reply shares a run of five words, or the name, with
                  a memory that was reached or retrieved
  (blank)         no recorded call named it in this turn

Each cell also carries whether the memory was in force when the reply was
written (said / unsaid / said-again dates from the minus40 store, the same
data m40_history renders), and each column lists those events, so a
memory withdrawn mid-session shows where along the column it stopped.

Sources are those of scope_view_data.py: 象 turn records for the rows and
the reply text, the Zai harness :turn-round evidence for the calls, the
minus40 facts for the memories and their dates. A turn with no recorded
minus40 call is a row of blanks, not an absent row: the view should show
how often the seat consulted memory as well as what it found.

  python3 -I scripts/scope_view_over_time.py --agent zai-2 \
      --session zai-d19c... --out web/scopes/fixtures/over-time-zai-2.json --md
"""
import argparse
import json
import os
import sys
import urllib.parse

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import scope_view_data as sv  # noqa: E402

TEXT_TOOLS = ("m40_state", "m40_asof", "m40_history", "m40_near")
RANK = {"": 0, "retrieved-only": 1, "reached": 2, "echoed": 3}
CELL_GLOSS = {
    "retrieved-only": "named by m40_find in this turn; the statements were not read",
    "reached": "its statements were returned by a recorded call in this turn",
    "echoed": "the reply shares a run of five words, or the memory's name, with it",
}


def session_turns(session):
    q = urllib.parse.urlencode({"session": session, "limit": 500})
    turns = sv.get_json(f"{sv.AGENCY}/api/alpha/xiang/turns?{q}")["turns"]
    return sorted(turns, key=lambda t: t["created-at"])


def record_view(record_id):
    return sv.get_json(f"{sv.AGENCY}/api/alpha/xiang/turns/{urllib.parse.quote(record_id)}")["record"]


def column_events(facts):
    events = []
    for f in facts:
        events.append({"at": f["said"], "event": "said", "text": f["text"]})
        if f.get("unsaid"):
            events.append({"at": f["unsaid"], "event": "unsaid", "text": f["text"]})
        if f.get("again"):
            events.append({"at": f["again"], "event": "again", "text": f["text"]})
    return sorted(events, key=lambda e: e["at"])


def build(agent, session):
    facts = sv.m40_facts()
    by_name, by_text, by_mem = {}, {f["text"]: f for f in facts}, {}
    for f in facts:
        by_name.setdefault(f["memory"].split("/", 1)[-1], []).append(f)
        by_mem.setdefault(f["memory"], []).append(f)

    rows, cells, columns = [], {}, {}
    for t in session_turns(session):
        turn = t["turn-id"]
        calls = sv.trace_for(agent, session, turn)
        row = {"turn": turn, "record": t["id"], "at": t["created-at"],
               "calls": [{"tool": c["tool"], "args": c["args"], "round": c["round"],
                          "error": c["error"], "truncated": c["truncated"]} for c in calls],
               "memories": 0}
        rows.append(row)
        if not calls:
            continue
        rec = record_view(t["id"])
        reply = rec.get("reply_text") or ""
        turn_at = rec.get("reply_at") or rec["created_at"]
        row["reply_at"] = turn_at
        row["reply_chars"] = len(reply)
        found = {}
        for c in calls:
            if c["error"]:
                continue
            for name, project in sv.memories_named(c["tool"], c["text"], by_text):
                cands = by_name.get(name, [])
                if project:
                    cands = [f for f in cands if f["memory"].startswith(project + "/")] or cands
                if not cands:
                    continue
                mem = cands[0]["memory"]
                level = "reached" if c["tool"] in TEXT_TOOLS else "retrieved-only"
                prev = found.get(mem, {"level": "", "calls": []})
                if RANK[level] > RANK[prev["level"]]:
                    prev["level"] = level
                prev["calls"].append(f"{c['tool']} r{c['round']}")
                found[mem] = prev
        for mem, info in found.items():
            mfacts = by_mem[mem]
            name = mem.split("/", 1)[-1]
            labels = [lab for _, _, lab in sv.echoes(reply, name, sv.memory_text(mfacts))]
            if labels:
                info["level"] = "echoed"
                info["echoes"] = labels
            newest = max(mfacts, key=lambda f: f["said"])
            info["status"] = sv.memory_status(newest, turn_at)
            cells[f"{turn}|{mem}"] = info
            if mem not in columns:
                columns[mem] = {"memory": mem, "name": name,
                                "project": mem.split("/", 1)[0],
                                "facts": len(mfacts),
                                "events": column_events(mfacts)}
        row["memories"] = len(found)

    ordered = sorted(columns.values(),
                     key=lambda c: (-sum(RANK[cells[f"{r['turn']}|{c['memory']}"]["level"]]
                                         for r in rows if f"{r['turn']}|{c['memory']}" in cells),
                                    c["name"]))
    return {"agent": agent, "session": session,
            "rows": rows, "columns": ordered, "cells": cells,
            "legend": {**CELL_GLOSS,
                       **{k: sv.RELATION_GLOSS[k] for k in
                          ("in-force", "retracted-since", "not-yet-said", "not-in-force")}}}


SYMBOL = {"": "·", "retrieved-only": "○", "reached": "●", "echoed": "◉"}


def markdown(data):
    cols = data["columns"]
    rows = data["rows"]
    out = [f"Session {data['session']} ({data['agent']}): {len(rows)} turns, "
           f"{sum(1 for r in rows if r['calls'])} with minus40 calls, {len(cols)} memories ever named.",
           "", "· none  ○ retrieved-only  ● reached  ◉ echoed; a cell's suffix marks a memory not in force at the reply (†).", ""]
    head = ["turn (created)", "calls"] + [f"c{i+1}" for i in range(len(cols))]
    out.append("| " + " | ".join(head) + " |")
    out.append("|" + "---|" * len(head))
    for r in rows:
        line = [f"{r['turn'][-8:]} {r['at'][5:16]}", str(len(r["calls"]))]
        for c in cols:
            cell = data["cells"].get(f"{r['turn']}|{c['memory']}")
            if not cell:
                line.append(SYMBOL[""])
            else:
                line.append(SYMBOL[cell["level"]] + ("" if cell["status"] == "in-force" else "†"))
        out.append("| " + " | ".join(line) + " |")
    out.append("")
    out.append("Columns:")
    for i, c in enumerate(cols):
        ev = c["events"]
        said = min(e["at"] for e in ev if e["event"] == "said")[:10]
        unsaid = [e["at"][:10] for e in ev if e["event"] == "unsaid"]
        again = [e["at"][:10] for e in ev if e["event"] == "again"]
        hist = f"first said {said}, {c['facts']} facts"
        if unsaid:
            hist += f", {len(unsaid)} unsaid (last {max(unsaid)})"
        if again:
            hist += f", {len(again)} said again (last {max(again)})"
        out.append(f"- c{i+1} {c['name']} ({c['project']}): {hist}")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--agent", required=True)
    ap.add_argument("--session", required=True)
    ap.add_argument("--out")
    ap.add_argument("--md", action="store_true", help="print the grid as markdown")
    args = ap.parse_args()
    data = build(args.agent, args.session)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as fh:
            fh.write(json.dumps(data, ensure_ascii=False, indent=1) + "\n")
        print(f"{args.out}: {len(data['rows'])} rows, {len(data['columns'])} columns, {len(data['cells'])} cells")
    if args.md or not args.out:
        print(markdown(data))


if __name__ == "__main__":
    main()
