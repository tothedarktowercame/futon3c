#!/usr/bin/env python3
"""Train and score an edition of 小象 on every reading published so far.

Joe (2026-09-30): while the 象 backfill runs, keep training editions of 小象 so
its improvement is visible and the tagging strategy can be adjusted.  Each run
loads the live readings and every batch block, keeps fragments whose intent is
on the closed list, runs xiaoxiang's grouped held-out evaluation and the
segmenter's boundary agreement, and appends one row to the history file.

  xiaoxiang_editions.py run  [--note TEXT]   evaluate now, append, print the row
  xiaoxiang_editions.py show                 print the history as a table

Off-list intents (the older batches used an open vocabulary) are counted and
left out, not mapped: a mapping is a labelling decision and belongs in a file
Joe can read, not in this loader.
"""
import argparse, collections, datetime, glob, json, os, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiaoxiang as xx  # noqa: E402

BATCHES = "/home/joe/code/storage/operator-turns/batches"
HISTORY = "/home/joe/code/storage/operator-turns/xiaoxiang-editions.jsonl"
CLOSED = set("report-problem explain report clarify qualify approve disagree collect "
             "constrain extend propose prioritize redirect defer delegate ask-action "
             "continue verify explore retract withdraw".split())


def rows_now(live=xx.DEFAULT_DIR, batches=BATCHES):
    rows, off = [], 0
    sources = [("live", live)] + [(os.path.basename(d), d)
                                  for d in sorted(glob.glob(os.path.join(batches, "*")))]
    for name, directory in sources:
        for r in xx.load(directory):
            if r["intent"] not in CLOSED:
                off += 1
                continue
            r["turn"] = f"{name}/{r['turn']}"   # turn ids repeat across sessions
            rows.append(r)
    return rows, off


def run(note=""):
    rows, off = rows_now()
    rep = xx.evaluate(rows)
    seg = xx.seg_eval(xx.DEFAULT_DIR)
    per = rep.get("per_intent", {})
    row = {"at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
           "note": note, "fragments": rep["fragments"], "turns": rep["turns"],
           "off_list_fragments": off,
           "labellers": dict(collections.Counter(r["labeller"] for r in rows).most_common(8)),
           "accuracy": round(rep["accuracy"], 4), "macro_f1": round(rep["macro_f1"], 4),
           "majority": round(rep["majority_baseline"]["accuracy"], 4),
           "f1": {k: round(v.get("f1") or 0, 3) for k, v in sorted(per.items())},
           "segment_f1": seg["boundary_f1"]}
    with open(HISTORY, "a") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")
    return row


def show():
    if not os.path.exists(HISTORY):
        print("no editions yet")
        return
    print(f"{'at':20} {'turns':>6} {'frags':>6} {'acc':>6} {'mF1':>6} {'base':>6} {'seg':>6}  note")
    for line in open(HISTORY):
        r = json.loads(line)
        print(f"{r['at']:20} {r['turns']:6} {r['fragments']:6} {r['accuracy']:6.3f} "
              f"{r['macro_f1']:6.3f} {r['majority']:6.3f} {r['segment_f1']:6.3f}  {r['note']}")


def main(argv=None):
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run"); r.add_argument("--note", default="")
    sub.add_parser("show")
    a = ap.parse_args(argv)
    if a.cmd == "run":
        print(json.dumps(run(a.note), ensure_ascii=False, indent=1))
    else:
        show()
    return 0


if __name__ == "__main__":
    sys.exit(main())
