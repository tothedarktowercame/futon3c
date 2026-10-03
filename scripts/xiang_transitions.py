#!/usr/bin/env python3
"""xiang_transitions.py — evidence for the dialogue game's transitions.

The adjacency table in futon3c.logic.xiang says which intents answer which.
It is a prior. This counts what the 象 readings actually show, in two
matrices, and tunes the prior with them:

  consecutive   intent of a fragment in turn t  ->  intent of a fragment in turn t+1,
                within one session (an operator turn, then the reply it got, ...)
  explicit      opener intent -> answer intent, where the answer's record names the
                opener (in_reply_to, or an acceptance's offer)

  xiang_transitions.py [--dir DIR] [--alpha 1.0] [--json OUT.json]

Prints the pairs the data has that the table lacks, the pairs the table has
that the data lacks, and the posterior; --json writes the posterior as
{opener: {answer: p}} for futon3c.logic.xiang/flow's :matrix. Classical:
counts over files, standard library only.
"""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import glob
import json
import os
import sys

DEFAULT_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")

# Mirror of futon3c.logic.xiang/adjacency (2026-10-03). Keep the two in step;
# the Clojure side is the authority and its test compares them when both load.
ADJACENCY = {
    "propose": {"approve", "disagree", "defer", "redirect", "extend", "commit", "withdraw", "retract"},
    "ask-action": {"accept", "disagree", "qualify", "offer", "report", "explain"},
    "offer": {"accept", "disagree", "defer", "retract", "withdraw"},
    "delegate": {"promise", "report-problem", "retract", "disagree", "report"},
    "promise": {"fulfil", "release", "lapse", "report-problem", "qualify"},
    "report-problem": {"verify", "explain", "redirect", "commit", "collect"},
    "clarify": {"explain", "report"},
    "constrain": {"withdraw", "rule-withdraw", "qualify", "commit"},
    "verify": {"report", "approve", "explain", "report-problem"},
}


def load_turns(directory: str) -> list[dict]:
    """Analysed turns: {session, at, turn_id, intents [..], reply_to}."""
    turns = []
    for path in glob.glob(os.path.join(directory, "turn-*.json")):
        if path.endswith((".analysis.json", ".candidates.json", ".draft.json", ".patterns.json")):
            continue
        try:
            rec = json.load(open(path, encoding="utf-8"))
            ana = json.load(open(path + ".analysis.json", encoding="utf-8"))
        except (OSError, ValueError):
            continue
        intents = [f.get("intent") for s in ana.get("sentences", []) for f in s.get("fragments", [])
                   if isinstance(f, dict) and f.get("intent")]
        marks = [m.get("intent") for m in rec.get("proforma_marks", []) if isinstance(m, dict) and m.get("intent")]
        turns.append({"session": rec.get("session_id"), "at": rec.get("created_at") or "",
                      "turn_id": rec.get("turn_id"), "origin": rec.get("origin", "operator"),
                      "intents": intents or marks,
                      "reply_to": rec.get("in_reply_to") or rec.get("answers")})
    return turns


def count(turns: list[dict]) -> tuple[Counter, Counter]:
    consecutive: Counter = Counter()
    explicit: Counter = Counter()
    by_session = defaultdict(list)
    for t in turns:
        by_session[t["session"]].append(t)
    by_turn = {t["turn_id"]: t for t in turns if t.get("turn_id")}
    for session, ts in by_session.items():
        ts.sort(key=lambda t: t["at"])
        for a, b in zip(ts, ts[1:]):
            for i in a["intents"]:
                for j in b["intents"]:
                    consecutive[(i, j)] += 1
        for t in ts:
            opener = by_turn.get(t.get("reply_to"))
            if opener:
                for i in opener["intents"]:
                    for j in t["intents"]:
                        explicit[(i, j)] += 1
    return consecutive, explicit


def posterior(counts: Counter, alpha: float = 1.0) -> dict[str, dict[str, float]]:
    total: Counter = Counter()
    for opener, answers in ADJACENCY.items():
        for a in answers:
            total[(opener, a)] += alpha
    total.update(counts)
    by_opener = defaultdict(dict)
    sums: Counter = Counter()
    for (o, a), n in total.items():
        sums[o] += n
    for (o, a), n in total.items():
        by_opener[o][a] = n / sums[o]
    return {o: dict(sorted(row.items(), key=lambda kv: -kv[1])) for o, row in by_opener.items()}


def report(consecutive: Counter, explicit: Counter, post: dict) -> str:
    out = [f"{sum(consecutive.values())} consecutive pairs, {sum(explicit.values())} explicit pairs"]
    table = {(o, a) for o, answers in ADJACENCY.items() for a in answers}
    surprises = [(n, o, a) for (o, a), n in explicit.items() if (o, a) not in table and o in ADJACENCY]
    if surprises:
        out.append("Explicit answers the table lacks (candidates for a row):")
        for n, o, a in sorted(surprises, reverse=True)[:20]:
            out.append(f"  {n:5d}  {o} -> {a}")
    missing = [(o, a) for (o, a) in table if explicit.get((o, a), 0) == 0 and consecutive.get((o, a), 0) == 0]
    if missing:
        out.append(f"Rows the table has that no reading shows ({len(missing)}): "
                   + ", ".join(f"{o}->{a}" for o, a in sorted(missing)[:20]) + ("…" if len(missing) > 20 else ""))
    out.append("Posterior (explicit counts + prior), top answers per opener:")
    for o, row in sorted(post.items()):
        top = ", ".join(f"{a} {p:.2f}" for a, p in list(row.items())[:4])
        out.append(f"  {o:<15} {top}")
    return "\n".join(out)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--dir", default=DEFAULT_DIR)
    ap.add_argument("--alpha", type=float, default=1.0, help="pseudo-count per table row")
    ap.add_argument("--json", help="write the posterior here, for logic.xiang/flow :matrix")
    ap.add_argument("--from", dest="source", choices=["explicit", "consecutive"], default="explicit",
                    help="which counts tune the prior (default explicit)")
    a = ap.parse_args(argv)
    turns = load_turns(a.dir)
    if not turns:
        print(f"No analysed turns under {a.dir}", file=sys.stderr)
        return 1
    consecutive, explicit = count(turns)
    post = posterior(explicit if a.source == "explicit" else consecutive, a.alpha)
    print(f"{len(turns)} analysed turns in {len({t['session'] for t in turns})} sessions")
    print(report(consecutive, explicit, post))
    if a.json:
        with open(a.json, "w", encoding="utf-8") as fh:
            json.dump(post, fh, ensure_ascii=False, indent=1)
        print(f"wrote {a.json}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
