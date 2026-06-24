#!/usr/bin/env python3
"""vwm_harness.py — Virtual War Machine v1: speculative fiction over the stack (no code changes).

Builds BACKWARDS from the WM. The WM's real failure is the STALL: an agent enters a mission, its candidate
patterns are a flat, low-cosine cluster (top ~0.38), nothing clears confidence → R6 abstain → nothing applied
(seen directly in mission-pattern-scopes.edn: war-machine missions with `:applied []` and max try-candidate cos
< 0.5). Hypothesis (Joe): a pattern CASCADE — the "downward growth" of what missions LIKE this one actually
applied (the semilattice cluster's cited patterns) — gives a warranted next move where single-pattern cosine-argmax
stalls. The VWM tests this speculatively and emits VSATARCS "Lucid Scenes" (the WM dreaming) — generatively, not
hand-authored. NO code is changed; we explore the stories + where they break.

Inputs (real/current): futon6/data/mission-pattern-scopes.edn (per-mission :applied + :try-candidates cosine),
futon3c/holes/excursions/pipeline-semilattice-clusters.edn (mission->cluster; the downward growth).
Outputs: console verdict + generated Lucid Scenes -> futon5a/holes/stories/vwm-lucid-scenes.md (VSATARCS-readable).

Run: python3 futon3c/holes/labs/vwm/vwm_harness.py
"""
import re
from pathlib import Path
from collections import defaultdict, Counter

ROOT = Path("/home/joe/code")
STALL_COS = 0.5          # max candidate cosine below this = the WM can't confidently pick = stall
CASCADE_K = 5            # cascade size from the cluster's downward growth

scopes = (ROOT/"futon6/data/mission-pattern-scopes.edn").read_text()
semi = (ROOT/"futon3c/holes/excursions/pipeline-semilattice-clusters.edn").read_text()

# per-mission: applied patterns + top candidate cosine (the WM's flat prior)
APPLIED, MAXCOS, CANDS = {}, {}, {}
for m, app, cands in re.findall(r':mission "M-([^"]+)" :applied \[([^\]]*)\] :try-candidates \[(.*?)\]\}', scopes, re.S):
    APPLIED[m] = re.findall(r'"([^"]+)"', app)
    cps = [(p, float(c)) for p, c in re.findall(r':pattern "([^"]+)" :cos ([0-9.]+)', cands)]
    CANDS[m] = cps
    MAXCOS[m] = max([c for _, c in cps], default=0.0)

# mission -> cluster (semilattice membership)
CLUSTER = {m: int(c) for m, c in re.findall(r':mission "M-([^"]+)"\s*:cluster (\d+)', semi)}

# downward growth: cluster -> cited patterns (what missions in the cluster actually applied) + usage count
cluster_apply = defaultdict(Counter)        # cluster -> Counter(pattern -> #missions applying it)
cluster_size = Counter()
for m, cl in CLUSTER.items():
    if m in APPLIED:
        cluster_size[cl] += 1
        for p in set(APPLIED[m]):
            cluster_apply[cl][p] += 1

def cascade_for(cl):   # the cluster's top cited patterns = the downward-growth cascade
    return cluster_apply[cl].most_common(CASCADE_K)

# ---- the stall set + the speculative unstick test ----
stalls = sorted([m for m in CANDS if MAXCOS[m] < STALL_COS], key=lambda m: MAXCOS[m])
unstuck = []
for m in stalls:
    cl = CLUSTER.get(m)
    casc = cascade_for(cl) if cl is not None else []
    # cascade warrant = top cited pattern's usage fraction in the cluster (a USAGE prior, not raw cosine)
    warrant = (casc[0][1] / cluster_size[cl]) if casc and cluster_size[cl] else 0.0
    unstuck.append({"m": m, "cos": MAXCOS[m], "cl": cl, "casc": casc, "warrant": warrant})

have_cascade = [u for u in unstuck if u["casc"]]
print(f"=== VWM stall analysis (real/current, from the downward growth) ===")
print(f"  missions scored: {len(CANDS)} | STALL set (max candidate cos < {STALL_COS}): {len(stalls)}")
print(f"  of stalls, a cluster CASCADE is available (warranted move where cosine-argmax stalled): "
      f"{len(have_cascade)}/{len(stalls)} = {len(have_cascade)/max(1,len(stalls)):.0%}")
import statistics as st
print(f"  mean single-pattern cosine (the flat WM prior): {st.mean([u['cos'] for u in unstuck]):.3f}")
print(f"  mean cluster-cascade warrant (usage of top cited pattern): {st.mean([u['warrant'] for u in have_cascade]):.2f}")
print("  -> where the agent's single-pattern cosine is flat+low (stall), the cluster's downward-growth cascade")
print("     offers a usage-warranted set of moves. (Whether it's the RIGHT move = the Lucid Scene, agent-judged.)")
print("\n  sample stalls + their dreamed cascades:")
for u in have_cascade[:6]:
    print(f"   M-{u['m']:34} cos={u['cos']:.2f} cl={u['cl']}  cascade={[p for p,_ in u['casc'][:4]]}")

# ---- HORIZON SCAN (design-as-defensive-driving): which CLUSTERS lack good-enough patterns? ----
# cluster terms (to name the gap class)
CTERMS = {}
for cl, terms in re.findall(r':cluster (\d+)\s*:size \d+\s*:terms \[(.*?)\]', semi, re.S):
    CTERMS[int(cl)] = re.findall(r'"([^"]+)"', terms)
CMISS = defaultdict(list)
for m, cl in CLUSTER.items():
    if m in CANDS: CMISS[cl].append(m)

print("\n=== HORIZON SCAN — clusters ranked by pattern-coverage (low = 'we have no idea how to tackle this class') ===")
rows = []
for cl, ms in CMISS.items():
    if not ms: continue
    applied_rate = sum(1 for m in ms if APPLIED.get(m)) / len(ms)   # did missions like this ever find patterns?
    mean_cos = st.mean(MAXCOS[m] for m in ms)                        # retrieval-prior strength for the class
    top = cluster_apply[cl].most_common(1)
    rows.append({"cl": cl, "n": len(ms), "applied_rate": applied_rate, "mean_cos": mean_cos,
                 "n_cited": len(cluster_apply[cl]), "top": (top[0] if top else None)})
rows.sort(key=lambda r: (r["applied_rate"], r["mean_cos"]))   # worst-covered first = the horizon gaps
print(f"  {'cl':>2} {'n':>3} {'applied%':>8} {'mean_cos':>8} {'#cited':>6}  terms (the class)  -> gap?")
for r in rows:
    gap = r["applied_rate"] < 0.25 and r["mean_cos"] < 0.45
    terms = ", ".join(CTERMS.get(r["cl"], [])[:4])
    print(f"  {r['cl']:>2} {r['n']:>3} {r['applied_rate']:>7.0%} {r['mean_cos']:>8.2f} {r['n_cited']:>6}  "
          f"{terms:32} {'<<< GAP: seed class patterns' if gap else ''}")
gaps = [r for r in rows if r["applied_rate"] < 0.25 and r["mean_cos"] < 0.45]
print(f"\n  HORIZON GAPS ({len(gaps)} clusters) — classes of work with no good-enough patterns (seed here, defensively):")
for r in gaps:
    ex = [m for m in CMISS[r["cl"]]][:3]
    print(f"   cluster {r['cl']} [{', '.join(CTERMS.get(r['cl'], [])[:5])}] — e.g. {ex}")
print("  -> relaxation of 'seed the perfect pattern' = seed GOOD-ENOUGH patterns for each gap CLASS, then the")
print("     whole cluster becomes tractable (and the VWM will then find them when it looks).")

# ---- emit generative Lucid Scenes (VSATARCS-readable) ----
def scene(u):
    casc = ", ".join(f"`{p}` ({n})" for p, n in u["casc"])
    top = u["casc"][0][0] if u["casc"] else "—"
    anchor = "vwm-" + u["m"]
    return f"""## Scene: Stuck on M-{u['m']} | {anchor}

*Authored:* 2026-06-23 · VWM (generated)
*Status:* :imagined

I imagine being asked to advance **M-{u['m']}**. I look for a move and my candidates come back flat — best
match only cos {u['cos']:.2f} — nothing I'm confident enough to do. I would abstain, and stall.

But I am not alone: missions *like* me (cluster {u['cl']}) have walked this ground. Their downward growth — the
patterns they actually applied — is a cascade I can dream applying: {casc}. The strongest, **`{top}`**, is
warranted by {u['warrant']:.0%} of my cluster-siblings.

*Where it could break:* the cascade tells me *what kind* of move worked for missions like me, not that it fits
*this* circumstance — a relevance check (and the gate) still stands. But it turns "nothing confident" into "a
warranted place to start."
"""

out = ROOT/"futon5a/holes/stories/vwm-lucid-scenes.md"
header = """# VWM — Generated Lucid Scenes (the War Machine dreaming, generatively)

**Type:** `:lucid` (VWM-emitted, not hand-authored) · **Source:** `futon3c/holes/labs/vwm/vwm_harness.py`
Speculative fiction over the stack: for each real stall (flat-cosine mission), the VWM dreams applying the
*downward-growth cascade* of what missions like it actually did. No code is changed. Companion to the
hand-authored `war-machine-lucid-scenes.md`.

---

"""
out.write_text(header + "\n".join(scene(u) for u in have_cascade[:12]))
print(f"\n  wrote {len(have_cascade[:12])} generated Lucid Scenes -> {out} (VSATARCS-readable)")
