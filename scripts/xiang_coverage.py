#!/usr/bin/env python3
"""xiang_coverage.py -- how much of Joe's turn corpus 象 has read, per window.

  python3 futon3c/scripts/xiang_coverage.py          # table
  python3 futon3c/scripts/xiang_coverage.py --line   # one line, for logs
  watch -n 60 python3 futon3c/scripts/xiang_coverage.py

Batch windows are the block directories under storage/operator-turns/batches;
a turn record is X.json, its reading X.json.analysis.json.  "refused" counts
pending turns the backfill loop has tried and the validator turned down (from
backfill-loop.jsonl); the loop tries a turn at most twice per run.  "live" is
~/.emacs-graph/session-turn-analysis, counted by its analysis_status and dated
by created_at; live records left requested/failed are copied to
batches/live-retry_block000 and count as read once read there.  Per-pack progress: tail -f storage/operator-turns/backfill-loop.jsonl
"""
import collections, glob, json, os, sys

BATCHES = "/home/joe/code/storage/operator-turns/batches"
LOG = "/home/joe/code/storage/operator-turns/backfill-loop.jsonl"
LIVE = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
# The window the PBASE comparison waits on (Joe, 2026-10-01).
TARGET = ("2026-08-22", "9999")


def refused_ids():
    out = set()
    if os.path.exists(LOG):
        for line in open(LOG, encoding="utf-8"):
            row = json.loads(line)
            out.update(t for t, _ in row.get("reasons") or [])
    return out


def batch_windows():
    refused = refused_ids()
    win = collections.OrderedDict()
    for d in sorted(glob.glob(os.path.join(BATCHES, "*_block*"))):
        name = os.path.basename(d).split("_block")[0]
        c = win.setdefault(name, collections.Counter())
        for f in os.listdir(d):
            if f.count(".json") != 1 or f == "MANIFEST.json" or not f.endswith(".json"):
                continue
            try:   # block002 of Aug-Sep also holds candidate files, not turns
                if "sentences" not in json.load(open(os.path.join(d, f))):
                    continue
            except ValueError:
                continue
            c["turns"] += 1
            if os.path.exists(os.path.join(d, f + ".analysis.json")):
                c["read"] += 1
            elif f[:-5] in refused:
                c["refused"] += 1
            else:
                c["pending"] += 1
    return win


RETRY = os.path.join(BATCHES, "live-retry_block000")


def live_days():
    """Live records by (day, status); a stuck live record read in the retry block counts as analyzed."""
    c = collections.Counter()
    for f in glob.glob(os.path.join(LIVE, "*.json")):
        if f.count(".json") != 1:
            continue
        try:
            r = json.load(open(f))
        except ValueError:
            continue
        day = (r.get("created_at") or "")[:10]
        st = r.get("analysis_status") or "?"
        if st != "analyzed" and os.path.exists(os.path.join(RETRY, os.path.basename(f) + ".analysis.json")):
            st = "analyzed"
        c[(day, st)] += 1
    return c


def main():
    win, live = batch_windows(), live_days()
    tot = collections.Counter()
    rows = []
    for name, c in win.items():
        if name.startswith("live-retry"):
            continue   # counted through the live records below
        rows.append((name, c["turns"], c["read"], c["refused"], c["pending"]))
        if name.split("_")[0] >= TARGET[0]:
            tot.update(c)
    lc = collections.Counter()
    for (day, st), n in live.items():
        if day >= TARGET[0]:
            lc[st] += n
    lt = sum(lc.values())
    tot["turns"] += lt
    tot["read"] += lc["analyzed"]
    tot["refused"] += lc["refused"]
    tot["pending"] += lt - lc["analyzed"] - lc["refused"]
    pct = 100.0 * tot["read"] / tot["turns"] if tot["turns"] else 0.0
    line = (f"象 coverage since {TARGET[0]}: {tot['read']}/{tot['turns']} read ({pct:.1f}%), "
            f"{tot['refused']} refused, {tot['pending']} pending")
    if "--line" in sys.argv:
        print(line)
        return
    print(f"{'window':<24}{'turns':>7}{'read':>7}{'refused':>9}{'pending':>9}")
    for name, n, r, x, p in rows:
        print(f"{name:<24}{n:>7}{r:>7}{x:>9}{p:>9}")
    print(f"{'live (since ' + TARGET[0] + ')':<24}{lt:>7}{lc['analyzed']:>7}{lc['refused']:>9}{lt - lc['analyzed'] - lc['refused']:>9}"
          f"   {dict(lc)}")
    print(line)


if __name__ == "__main__":
    main()
