#!/usr/bin/env python3
"""target_value.py — G per target for PROOF-2a Clause T on M-futon-seams.
claude-10, 2026-09-24. Inputs: mission-C.edn (claude-1, owner), target-cost.edn.
G(i) = -sum_o w_o * attained_i(o) + lam * E[attempts_i]. Every instance cascade
completes with probability 1 at theta 0.8 (target_cost: no stalls), so
attained_i(o) = 1 when instance i has a want serving o. Weights are :unstated,
so instead of one number: sweep the weight simplex over the outcomes on a grid
and lam over a range, and report how often each ranking of 4,5,6,7 occurs and
the conditions under which the mission's own order (4 before 5 before 7) holds.
Also checks every cue span against the mission at HEAD. Deterministic."""
import itertools, json, re, subprocess, hashlib, os
here = os.path.dirname(os.path.abspath(__file__))
edn = lambda f: json.loads(subprocess.check_output(["bb", "-e",
      '(println (cheshire.core/generate-string (clojure.edn/read-string (slurp "%s"))))' % f], text=True))
C = edn(os.path.join(here, "mission-C.edn"))
cost = {t["instance"]: t["expected-attempts"] for t in edn(os.path.join(here, "target-cost.edn"))["targets"]}
mission = subprocess.check_output(["git", "show", "HEAD:holes/missions/M-futon-seams.md"], cwd=os.path.join(here, "../../../.."))
sha = hashlib.sha256(mission).hexdigest()
text = mission.decode()
cues = {o: text[v["cue"][0]:v["cue"][1]] for o, v in C["outcomes"].items()}
served = {}
for s in C["served-by"]:
    if s.get("status") != "prospective":
        served.setdefault(s["instance"], set()).add(s["outcome"])
targets = sorted(served)
outs = sorted({o for v in served.values() for o in v})
N = 10  # simplex grid step 1/N
grid = [c for c in itertools.product(range(N + 1), repeat=len(outs)) if sum(c) == N]
lams = [0, 0.02, 0.05, 0.1, 0.2, 0.4]
rankings, mission_order = {}, {}
for lam in lams:
    cnt, ok = {}, 0
    for c in grid:
        w = {o: x / N for o, x in zip(outs, c)}
        G = {i: -sum(w[o] for o in served[i]) + lam * cost[i] for i in targets}
        r = tuple(sorted(targets, key=lambda i: (G[i], i)))
        cnt[r] = cnt.get(r, 0) + 1
        ok += G["4"] < G["5"] < G["7"]
    rankings[lam] = sorted(((" > ".join(k), round(v / len(grid), 3)) for k, v in cnt.items()), key=lambda x: -x[1])[:4]
    mission_order[lam] = round(ok / len(grid), 3)
out = {"mission_sha_at_HEAD": sha, "mission_sha_in_C": C["mission-sha"], "sha_matches": sha == C["mission-sha"],
       "cues": cues, "served": {i: sorted(v) for i, v in served.items()}, "cost": cost,
       "grid_points": len(grid), "outcomes_swept": outs,
       "share_of_weights_giving_4_before_5_before_7": {str(k): v for k, v in mission_order.items()},
       "top_rankings": {str(k): v for k, v in rankings.items()},
       "analytic": "served(4) is a subset of served(5), so on value alone 5 beats 4 whenever w(no-drifting-forks) > 0; 4 comes before 5 iff lam * (cost5 - cost4) > w(no-drifting-forks), i.e. lam * 1.25 > w_drift"}
json.dump(out, open(os.path.join(here, "target-value.json"), "w"), indent=1, sort_keys=True)
print(json.dumps(out, indent=1, sort_keys=True))
