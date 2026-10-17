#!/usr/bin/env python3
"""memory_use_rate.py — how often seats consult the minus40 memory store.

One row per seat per day, from the harness round records in futon1b (the
Zai harness writes a :turn-start entry with the prompt and one :turn-round
entry per model round, each round carrying its tool calls).  Read-only.

    python3 -I scripts/memory_use_rate.py --since 2026-10-14
    python3 -I scripts/memory_use_rate.py --since 2026-10-14 --agents zai-1,zai-2 --json out.json

Columns:
  turns          turns that had at least one round recorded
  with-tools     turns with at least one tool call of any kind
  using-m40      turns with at least one minus40 call (m40_* or memory_assert)
  m40-calls      number of minus40 calls
  requisitioned  turns whose prompt asked for the store (names a minus40 tool,
                 "minus40" or "M40"), and how many of those used it
  instructed     turns that used the store with no ask in the packet but the
  (standing)     harness's standing instruction in force (--standing-since;
                 the Zai harness has told every seat to call m40_find /
                 memory_assert since 2026-10-15, so a packet-silent used turn
                 is not "unprompted" in any strong sense)
  no-instruct    turns that used the store with neither a packet ask nor the
  (used)         standing instruction
  injected       turns where the harness injected memories before round 1
  (no call)      (turn-start :injected) and the seat made no m40 call of its
                 own — the delivery the seat never had to choose

"Requisitioned" is read from the prompt text, so a turn the operator asked
in other words lands in "instructed" or "no-instruct", not
"requisitioned"; the columns say what the record can show, not what the
seat understood.
"""
import argparse
import collections
import json
import re
import sys
import urllib.parse
import urllib.request

AGENCY = "http://localhost:7070"
MEMORY_TOOLS = ("m40_find", "m40_state", "m40_asof", "m40_history", "m40_near",
                "memory_assert", "sorry_list", "sorry_discharge")
REQUISITION = re.compile(r"m40_\w+|minus40|\bM40\b|40ºBelow|memory_assert", re.I)


def fetch_evidence(base, page_limit=1000, **params):
    """All evidence rows matching PARAMS, following next-cursor."""
    rows = []
    q = {k.replace("_", "-"): v for k, v in params.items() if v is not None}
    q["limit"] = str(page_limit)
    while True:
        url = base.rstrip("/") + "/api/alpha/evidence?" + urllib.parse.urlencode(q)
        req = urllib.request.Request(url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=60) as resp:
            page = json.load(resp)
        rows.extend(page.get("entries", []))
        cursor = page.get("next-cursor")
        if not cursor:
            return rows
        q["cursor-at"] = cursor["at"]
        q["cursor-id"] = cursor["id"]


def body_of(row):
    b = row.get("evidence/body")
    if isinstance(b, str):
        try:
            return json.loads(b)
        except ValueError:
            return {}
    return b or {}


def tabulate(starts, rounds, standing_since=None):
    """Per (agent, day) counts from :turn-start and :turn-round rows.

    STARTS and ROUNDS are evidence rows (dicts with evidence/author,
    evidence/at, evidence/body); STANDING_SINCE, when set, is the ISO date
    from which the harness's standing m40 instruction counts as an
    instruction (turns at or after it).  Returns ([row, ...], [turn, ...])
    where a row is {agent, day, turns, with_tools, using_m40, m40_calls,
    requisitioned, requisitioned_using, instructed_using, no_instruction_using,
    injected, injected_unused} and a turn is the per-turn record the rows
    were summed from.
    """
    prompts = {}
    for s in starts:
        b = body_of(s)
        if b.get("turn-id"):
            prompts[b["turn-id"]] = {"prompt": b.get("prompt") or "",
                                     "dispatch": b.get("dispatch-id"),
                                     "injected": b.get("injected") or [],
                                     "at": s.get("evidence/at")}
    turns = {}
    for r in rounds:
        b = body_of(r)
        tid = b.get("turn-id")
        if not tid:
            continue
        t = turns.setdefault(tid, {"turn": tid, "agent": r.get("evidence/author"),
                                   "at": r.get("evidence/at"), "rounds": 0,
                                   "calls": 0, "m40_calls": 0, "m40_tools": []})
        if r.get("evidence/at") and r["evidence/at"] < t["at"]:
            t["at"] = r["evidence/at"]
        t["rounds"] += 1
        for c in b.get("calls") or []:
            t["calls"] += 1
            if c.get("tool") in MEMORY_TOOLS:
                t["m40_calls"] += 1
                t["m40_tools"].append(c["tool"])
    for t in turns.values():
        p = prompts.get(t["turn"], {})
        t["requisitioned"] = bool(REQUISITION.search(p.get("prompt", "")))
        t["standing"] = bool(standing_since and (t["at"] or "") >= standing_since)
        t["injected"] = list(p.get("injected") or [])
        t["dispatch"] = p.get("dispatch")
        t["day"] = (t["at"] or "")[:10]
    by = collections.OrderedDict()
    for t in sorted(turns.values(), key=lambda t: (t["agent"] or "", t["at"] or "")):
        row = by.setdefault((t["agent"], t["day"]), {
            "agent": t["agent"], "day": t["day"], "turns": 0, "with_tools": 0,
            "using_m40": 0, "m40_calls": 0, "requisitioned": 0,
            "requisitioned_using": 0, "instructed_using": 0,
            "no_instruction_using": 0, "injected": 0, "injected_unused": 0})
        row["turns"] += 1
        row["with_tools"] += 1 if t["calls"] else 0
        used = t["m40_calls"] > 0
        row["using_m40"] += 1 if used else 0
        row["m40_calls"] += t["m40_calls"]
        row["requisitioned"] += 1 if t["requisitioned"] else 0
        row["requisitioned_using"] += 1 if (t["requisitioned"] and used) else 0
        row["instructed_using"] += 1 if (used and not t["requisitioned"]
                                         and t["standing"]) else 0
        row["no_instruction_using"] += 1 if (used and not t["requisitioned"]
                                             and not t["standing"]) else 0
        row["injected"] += 1 if t["injected"] else 0
        row["injected_unused"] += 1 if (t["injected"] and not used) else 0
    return list(by.values()), sorted(turns.values(), key=lambda t: t["at"] or "")


def markdown(rows):
    head = ("| seat | day | turns | with tools | using minus40 | m40 calls "
            "| requisitioned (used) | instructed (used) | no instruct (used) "
            "| injected (no call) |\n"
            "|---|---|---|---|---|---|---|---|---|---|")
    lines = [head]
    tot = collections.Counter()
    for r in rows:
        lines.append(f"| {r['agent']} | {r['day']} | {r['turns']} | {r['with_tools']} | "
                     f"{r['using_m40']} | {r['m40_calls']} | "
                     f"{r['requisitioned']} ({r['requisitioned_using']}) | "
                     f"{r['instructed_using']} | {r['no_instruction_using']} | "
                     f"{r['injected']} ({r['injected_unused']}) |")
        for k in ("turns", "with_tools", "using_m40", "m40_calls",
                  "requisitioned", "requisitioned_using", "instructed_using",
                  "no_instruction_using", "injected", "injected_unused"):
            tot[k] += r[k]
    lines.append(f"| **all** | | {tot['turns']} | {tot['with_tools']} | {tot['using_m40']} | "
                 f"{tot['m40_calls']} | {tot['requisitioned']} ({tot['requisitioned_using']}) | "
                 f"{tot['instructed_using']} | {tot['no_instruction_using']} | "
                 f"{tot['injected']} ({tot['injected_unused']}) |")
    return "\n".join(lines)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--since", help="ISO date or instant; rows before it are not fetched")
    ap.add_argument("--agents", default="zai-1,zai-2,zai-3,zai-4,zai-5",
                    help="comma-separated authors (default: the five Zai seats)")
    ap.add_argument("--base", default=AGENCY)
    ap.add_argument("--standing-since", default="2026-10-15", metavar="DATE",
                    help="ISO date from which the harness's standing m40 "
                         "instruction counts as an instruction (the Zai "
                         "harness has carried it since 2026-10-15); "
                         "pass '' to disable")
    ap.add_argument("--json", metavar="PATH", help="also write rows and per-turn records here")
    args = ap.parse_args(argv)
    starts, rounds = [], []
    for agent in [a for a in args.agents.split(",") if a]:
        starts += fetch_evidence(args.base, tag="turn-start", author=agent, since=args.since)
        rounds += fetch_evidence(args.base, tag="turn-round", author=agent, since=args.since)
    rows, turns = tabulate(starts, rounds, standing_since=args.standing_since or None)
    print(markdown(rows))
    print(f"\n{len(starts)} turn-start rows, {len(rounds)} turn-round rows"
          + (f" since {args.since}" if args.since else ""), file=sys.stderr)
    if args.json:
        with open(args.json, "w") as f:
            json.dump({"since": args.since, "rows": rows, "turns": turns}, f,
                      ensure_ascii=False, indent=1)
    return 0


if __name__ == "__main__":
    sys.exit(main())
