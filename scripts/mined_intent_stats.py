#!/usr/bin/env python3
"""Intent counts for the Kimi-mined operator turns, in the feed's vocabulary.

Reads every published analysis under the operator-turn batches, maps each
fragment's open intent label through resources/turnfeed/intent-crosswalk.json,
and writes one JSON the feed's legend reads beside its own counts: per feed
intent, the fragments and how many cite a library pattern; plus the
fragments the crosswalk leaves unmapped, by their original label.

usage: mined_intent_stats.py [--batches DIR] [--out FILE]
"""
import argparse, collections, glob, json, os, time

HERE = os.path.dirname(os.path.abspath(__file__))
CROSSWALK = os.path.join(HERE, "..", "resources", "turnfeed", "intent-crosswalk.json")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--batches", default="/home/joe/code/storage/operator-turns/batches")
    ap.add_argument("--out", default="/var/www/zone.hyperreal.enterprises/wip/turns/mined-intents.json")
    a = ap.parse_args()
    cmap = json.load(open(CROSSWALK))["map"]
    by = collections.defaultdict(lambda: {"n": 0, "with-pattern": 0})
    unmapped = collections.Counter()
    turns = frags = cited = 0
    for f in sorted(glob.glob(os.path.join(a.batches, "*", "*.analysis.json"))):
        try:
            d = json.load(open(f))
        except (OSError, ValueError):
            continue
        turns += 1
        for s in d.get("sentences", []):
            # The feed's rule (turn_margin_html.fragments): a sentence with no
            # fragment and an unresolved_reason is one "unresolved" note.
            if not s.get("fragments") and s.get("unresolved_reason"):
                by["unresolved"]["n"] += 1
                continue
            for x in s.get("fragments", []):
                frags += 1
                label = x.get("intent") or "(none)"
                has_pat = bool(x.get("pattern_refs"))
                cited += has_pat
                feed = cmap.get(label)
                if feed is None:
                    unmapped[label] += 1
                    continue
                by[feed]["n"] += 1
                by[feed]["with-pattern"] += has_pat
    out = {"generated-at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
           "source": a.batches, "turns": turns, "fragments": frags,
           "with-pattern": cited, "by-intent": by,
           "unmapped": {"n": sum(unmapped.values()), "labels": len(unmapped),
                        "top": unmapped.most_common(12)}}
    json.dump(out, open(a.out, "w"), indent=1)
    print(f"{turns} turns, {frags} fragments, {out['unmapped']['n']} unmapped -> {a.out}")


if __name__ == "__main__":
    main()
