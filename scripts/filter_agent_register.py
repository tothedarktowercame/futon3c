#!/usr/bin/env python3
"""filter_agent_register.py — drop the agent-to-agent register from the corpus.

An interim filter, standing in for the provenance join until the invoke-start
origins are harvested.  It re-derives the k=6 cut from the saved linkage and
microcluster assignment, then drops whichever of those clusters are named on
the command line.

  filter_agent_register.py 1 2 5 6            # cluster ids to drop
"""
import json, re, sys
import numpy as np
from scipy.cluster.hierarchy import fcluster

BASE = "/home/joe/code/storage/operator-turns"
CORPUS, OUT = f"{BASE}/operator-turns.jsonl", f"{BASE}/operator-turns-filtered.jsonl"
MACHINE = re.compile(r"\[Session-mode structural analysis request.*?\[End structural analysis request\]", re.S)
RESUMED = re.compile(r"\n?-{3} resumed:.*\Z", re.S)
CODEISH = re.compile(r"```.*?```", re.S)
K = 6

clean = lambda t: CODEISH.sub(" ", RESUMED.sub(" ", MACHINE.sub(" ", t or "")))

drop = {int(a) for a in sys.argv[1:]} or {1, 2, 5, 6}
rows = [json.loads(l) for l in open(CORPUS)]
keep_idx = [i for i, r in enumerate(rows) if len(clean(r.get("text")).split()) >= 3]
micro = np.load(f"{BASE}/analysis/micro-assign.npy")
link = np.load(f"{BASE}/analysis/linkage.npy")
assert len(micro) == len(keep_idx), (len(micro), len(keep_idx))

flat = fcluster(link, K, criterion="maxclust")[micro]
kept = [keep_idx[j] for j in range(len(keep_idx)) if flat[j] not in drop]
with open(OUT, "w") as f:
    for i in kept:
        f.write(json.dumps(rows[i]) + "\n")
print(f"dropped clusters {sorted(drop)}: {len(keep_idx) - len(kept)} turns out, "
      f"{len(kept)} kept -> {OUT}")
