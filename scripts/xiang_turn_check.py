#!/usr/bin/env python3
"""Is every turn being read and aligned?  One screen of text, for Emacs.

    python3 -I scripts/xiang_turn_check.py [--hours 24] [--agent claude-3] [--list]

For each turn the turn service recorded in the window, the stages it has
reached, and which of them were done by a model and which by classical code:

  operator turns   recorded -> cues (classical) -> reading (象, model)
                   -> reply attached -> reply acts (classical: the reply's
                   marks) -> ports (classical rules: which reply act answers
                   which operator act)
  bell turns       recorded ("declared": no reading, by design) -> reply
                   attached -> reply acts -> ports

Plus 馬 cards (model) per enabled agent.  A stage that should have happened
and has not is listed under PROBLEMS.  Reads only; changes nothing.
"""
import argparse
import collections
import functools
import json
import os
import subprocess
import urllib.request
from datetime import datetime, timedelta, timezone

BASE = "http://localhost:7070"
MA_ENABLED = os.path.expanduser("~/code/data/minus40/ma-enabled.txt")
# A reply has until this long after the turn before a missing one is a problem.
REPLY_GRACE = timedelta(minutes=30)
# Where a commit act's sha is looked up, for its Agent-Id trailer.
REPOS = [os.path.expanduser("~/code/" + r) for r in (
    "futon0", "futon1b", "futon2", "futon3", "futon3a", "futon3b", "futon3c", "futon4",
    "futon5", "futon6", "p4ng", "mathlib4", "minus40", "apm-lean")]


@functools.lru_cache(maxsize=None)
def commit_agent(sha):
    """The Agent-Id trailer of SHA in the first repo that has it, '' if unsigned,
    None if no repo has it."""
    for r in REPOS:
        out = subprocess.run(["git", "-C", r, "log", "-1", "--format=%(trailers:key=Agent-Id,valueonly)", sha],
                             capture_output=True, text=True)
        if out.returncode == 0:
            return out.stdout.strip()
    return None


def get(path):
    with urllib.request.urlopen(BASE + path, timeout=30) as r:
        return json.load(r)


def at(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00")) if s else None


def stages(rec, now):
    """(facts, problems) for one turn record."""
    origin = rec.get("origin") or "?"
    status = rec.get("analysis_status")
    created = at(rec.get("created_at"))
    old = created and now - created > REPLY_GRACE
    turn = rec.get("turn_id")
    acts = rec.get("acts") or []
    op_acts = [a for a in acts if a.get("author") == "operator" and a.get("turn") == turn]
    reply_acts = [a for a in acts if a.get("author") != "operator" and a.get("turn") == turn]
    cue_sentences = [s for s in rec.get("sentences") or [] if s.get("cues")]
    ports = rec.get("ports_status") or {}
    # alignment: a reply act whose bracket quote was found in the operator's
    # turn carries the operator act it answers as "target" (classical)
    targeted = [a for a in reply_acts if a.get("target")]
    commits = [a for a in reply_acts if a.get("kind") == "commit"]
    foreign = []
    for c in commits:
        sha = c["id"].rsplit("-c-", 1)[-1]
        who = commit_agent(sha)
        ids = set(who.split()) if who else set()
        if ids and rec.get("agent_id") not in ids:
            foreign.append("%s is %s's" % (sha, "/".join(sorted(ids))))
    f = {
        "origin": origin,
        "status": status,
        "cues": bool(cue_sentences),
        "reading": status == "analyzed",
        "reply": bool((rec.get("reply_text") or "").strip()),
        "reply_acts": len(reply_acts),
        "op_acts": len(op_acts),
        "ports": ports.get("state"),
        "rules": ports.get("rules_version"),
        "aligned": len(targeted),
        "unmatched": len(rec.get("unmatched") or []),
        "commits": len(commits),
    }
    p = []
    if origin == "system":      # session-end rows: no reply by nature
        return f, p
    if foreign:
        p.append("commit acts from another seat: " + ", ".join(foreign))
    if origin == "operator":
        if status not in ("analyzed", "drafted") and old:
            p.append("no reading (status %s)" % status)
    if not f["reply"] and old:
        p.append("no reply attached")
    if f["reply"] and f["reply_acts"] == 0 and origin == "operator":
        p.append("reply has no marked acts")
    if f["reply"] and f["ports"] != "computed":
        p.append("ports %s" % (f["ports"] or "missing"))
    if origin == "operator" and f["reply"] and f["op_acts"] and not f["aligned"]:
        p.append("no reply act quotes the operator's words")
    return f, p


def ma_cards(since):
    try:
        rows = get("/api/alpha/evidence?tag=ma-card&limit=500").get("entries", [])
    except Exception as e:
        return {"error": str(e)}
    c = collections.Counter()
    for r in rows:
        if at(r.get("evidence/at")) and at(r["evidence/at"]) >= since:
            c[(r.get("evidence/body") or {}).get("observed-agent")] += 1
    return c


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--hours", type=float, default=24)
    ap.add_argument("--agent")
    ap.add_argument("--list", action="store_true", help="one line per turn")
    a = ap.parse_args()
    now = datetime.now(timezone.utc)
    since = now - timedelta(hours=a.hours)
    turns = [t for t in get("/api/alpha/xiang/turns?limit=3000")["turns"]
             if at(t.get("created-at")) and at(t["created-at"]) >= since
             and (not a.agent or t.get("agent-id") == a.agent)]
    by = collections.defaultdict(lambda: collections.Counter())
    problems = []
    lines = []
    for t in turns:
        rec = get("/api/alpha/xiang/turns/" + t["id"]).get("record") or {}
        f, p = stages(rec, now)
        k = by[f["origin"]]
        k["turns"] += 1
        if f["origin"] == "system":
            continue
        for key in ("cues", "reading", "reply"):
            k[key] += f[key]
        k["with reply acts"] += f["reply_acts"] > 0
        k["ports computed"] += f["ports"] == "computed"
        k["aligned"] += f["aligned"] > 0
        k["unmatched targets"] += f["unmatched"] > 0
        k["rules " + str(f["rules"])] += 1
        if p:
            problems.append((t.get("created-at"), t.get("agent-id"), t["id"], "; ".join(p)))
        if a.list:
            lines.append("%s %-9s %-9s %-8s cue=%d read=%d reply=%d racts=%d ports=%s aligned=%d  %s" % (
                t.get("created-at", "")[11:16], t.get("agent-id"), f["origin"], f["status"],
                f["cues"], f["reading"], f["reply"], f["reply_acts"], f["ports"], f["aligned"],
                ("PROBLEM: " + "; ".join(p)) if p else ""))
    health = get("/api/alpha/xiang/health")
    print("象 turn check, last %gh%s, %s" % (a.hours, (" for " + a.agent) if a.agent else "",
                                         now.strftime("%Y-%m-%d %H:%MZ")))
    print("service: %s; seat %s; outstanding %s" % (
        (health.get("health") or {}).get("state"), health.get("seat"), health.get("outstanding")))
    for origin, k in sorted(by.items()):
        n = k["turns"]
        print("\n%s turns: %d" % (origin, n))
        for key in ("cues", "reading", "reply", "with reply acts", "ports computed", "aligned"):
            print("  %-16s %4d  (%3.0f%%)" % (key, k[key], 100.0 * k[key] / n if n else 0))
        print("  turns with sentences no cue classified: %d;  %s" % (
            k["unmatched targets"], ", ".join("%s: %d" % (r, v) for r, v in k.items() if r.startswith("rules "))))
    cards = ma_cards(since)
    enabled = [l.strip() for l in open(MA_ENABLED) if l.strip() and not l.startswith("#")] \
        if os.path.exists(MA_ENABLED) else []
    print("\n馬 cards (model) in window: " + (", ".join("%s %d" % (ag, cards.get(ag, 0)) for ag in enabled)
                                           if isinstance(cards, collections.Counter) else str(cards)))
    print("\nmodel steps: 象 reading (operator turns), 馬 cards.  classical: cues, reply acts,"
          " ports, alignment (a reply act quoting the operator).  system = session-end rows.")
    print("\nPROBLEMS: %d" % len(problems))
    for p in problems[:40]:
        print("  %s %-9s %s  %s" % (p[0][5:16] if p[0] else "", p[1], p[2], p[3]))
    if lines:
        print("\n" + "\n".join(lines))


if __name__ == "__main__":
    main()
