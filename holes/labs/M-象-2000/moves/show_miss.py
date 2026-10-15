#!/usr/bin/env python3
"""Show a within-rule miss as three texts, so a person can judge it.

The report's "Ten instructive combined-rule misses" names act ids: the act,
the target the combined rule predicted, the target the labeller drew. An id
says nothing on its own. This prints, for each miss, the act's own words,
then the words of the act the rule says it sits within, then the words of
the act the labeller chose, each with its kind and, when it lives in another
turn, that turn's id. Read-only; stdlib only.

  python3 -I moves/show_miss.py            the first three misses in the report
  python3 -I moves/show_miss.py 1 4 7      misses by number (1-10)
  python3 -I moves/show_miss.py --all
"""
import glob
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LAB = os.path.join(HERE, "..")
REPORT = os.path.join(LAB, "REPORT-2026-10-14-within-rules.md")
MISS = re.compile(r"^- (\S+) \[[^\]]+\] predicted (\S+) \(([^)]*)\), gold (\S+) \(([^)]*)\)")


def misses():
    """[(act, predicted, predicted_kind, gold, gold_kind)] from the report."""
    out, on = [], False
    for line in open(REPORT, encoding="utf-8"):
        if line.startswith("## Ten instructive"):
            on = True
            continue
        if on and line.startswith("## "):
            break
        m = MISS.match(line) if on else None
        if m:
            out.append(m.groups())
    return out


def texts():
    """act id -> (kind, text). Sidecars and inputs both carry words for acts;
    inputs also carry the operator fragments the sidecars only reference."""
    by = {}
    for path in glob.glob(os.path.join(LAB, "data", "moves-scale*", "shard-*", "turn-*.moves.json")):
        rec = json.load(open(path, encoding="utf-8"))
        for a in rec.get("acts", []):
            if a.get("quote"):
                by.setdefault(a["act"], (a.get("kind") or "?", a["quote"]))
        inp = os.path.join(os.path.dirname(path), "input",
                           os.path.basename(path).replace(".moves.json", ".input.json"))
        if os.path.exists(inp):
            d = json.load(open(inp, encoding="utf-8"))
            for f in d.get("fragments", []):
                by.setdefault(f["id"], (f.get("intent") or "?", f.get("text") or ""))
            for k in ("acts", "reply_acts", "previous_reply_acts"):
                for a in d.get(k, []):
                    if a.get("quote") or a.get("text"):
                        by.setdefault(a["id"], (a.get("kind") or "reply", a.get("quote") or a.get("text")))
    return by


def turn_of(act_id):
    return act_id.rsplit("-f-", 1)[0].rsplit("-r-", 1)[0]


def show(n, miss, by, width=240):
    act, pred, pkind, gold, gkind = miss
    here = turn_of(act)

    def line(label, aid, kind):
        if aid == "None":
            return "  %-16s (the rule predicted nothing)" % label
        k, t = by.get(aid, (kind, "(words not found in the lab data)"))
        where = "" if turn_of(aid) == here else "  [in %s]" % turn_of(aid)
        t = " ".join(t.split())
        return "  %-16s %s: \"%s\"%s" % (label, k, t[:width] + ("…" if len(t) > width else ""), where)

    print("Miss %d  (%s)" % (n, act))
    print(line("the act", act, "?"))
    print(line("rule: within", pred, pkind))
    print(line("labeller: within", gold, gkind))
    print()


def main():
    args = [a for a in sys.argv[1:]]
    ms = misses()
    if not ms:
        print("no misses parsed from", REPORT)
        return 1
    if "--all" in args:
        wanted = range(1, len(ms) + 1)
    elif args:
        wanted = [int(a) for a in args]
    else:
        wanted = [1, 2, 3]
    by = texts()
    print("A within edge says which earlier act an act is answering or working under.")
    print("For each miss: the act, the act the position rule chose, the act the labeller chose.\n")
    for n in wanted:
        if 1 <= n <= len(ms):
            show(n, ms[n - 1], by)
    return 0


if __name__ == "__main__":
    sys.exit(main())
