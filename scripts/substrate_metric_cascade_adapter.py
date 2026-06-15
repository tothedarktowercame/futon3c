#!/usr/bin/env python3
"""Cascade adapter for the substrate metric (M-differentiable-substrate).

Projects mined mission-triple CASCADE graphs (futon6/data/mission-triples/*.edn,
miner v1) into the OR-curvature core of substrate_metric_e1_curvature.py — the
ends->edges projection claude-3 scoped. Each cascade hyperedge
(:differentiates / :states) is a role-typed PAIR (miner v1 emits no n-ary
:jointly-with; when a later miner does, clique/star-expand here, never chain —
the semilattice forbids the order a chain would impose). Node :satiety is read
and reported as the candidate mu-feed, but v1 curvature is purely TOPOLOGICAL
(legacy lazy-walk) — an honest plumbing read: most miner-v1 cascades are skeletal
cycles (a pattern-cite chain closed by :states), so trivial curvature here is the
corpus telling the truth, not a bug. Satiety-weighted mu is the v2 refinement
(the existing blended_mu beta channel).

TERRAIN HONESTY (policy-landscape-drainage trap 3): curvature is computed over
the MINED corpus as-of the miner commit (commit-pinned per mission), not live
HEAD. This is a snapshot read, not live terrain.

Usage:
  python3 scripts/substrate_metric_cascade_adapter.py [--corpus DIR] [--out FILE] [--top N]
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any

import edn_format

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import substrate_metric_e1_curvature as eng  # noqa: E402  (curvature core: reused, not reimplemented)

K = edn_format.Keyword


def kw(x: Any) -> str:
    """Normalize an edn keyword/symbol/string to a stable str key."""
    return str(x)


def get(m: Any, key: str, default: Any = None) -> Any:
    if m is None:
        return default
    v = m.get(K(key), default)
    return v


def satiety_scalar(satiety: Any) -> float:
    """Candidate mu-feed (v2): :full -> 1.0; {:hungry-for _} -> 0.0. Reported in
    v1, not yet fed into the lazy walk (curvature stays topological)."""
    if satiety == K("full"):
        return 1.0
    return 0.0  # {:hungry-for :parse|:payoff|...} — un-discharged tension


def project_cascade(triple: Any) -> dict[str, Any]:
    """ends->edges projection of one mission's :cascade. Returns edges
    [{"a","b","relation"}], the node->satiety scalar map, and any n-ary edges
    deferred (none under miner v1)."""
    cascade = get(triple, "cascade") or {}
    nodes_raw = get(cascade, "nodes") or []
    hyperedges = get(cascade, "hyperedges") or []

    satiety = {}
    for n in nodes_raw:
        satiety[kw(get(n, "id"))] = satiety_scalar(get(n, "satiety"))

    edges: list[dict[str, str]] = []
    deferred_nary: list[dict[str, Any]] = []
    for he in hyperedges:
        ends = get(he, "ends") or []
        relation = kw(get(he, "kind"))
        if len(ends) != 2:
            # miner v1 emits none of these; future :jointly-with lands here.
            deferred_nary.append({"relation": relation, "arity": len(ends)})
            continue
        a = kw(get(ends[0], "node"))
        b = kw(get(ends[1], "node"))
        if a == b:
            continue
        edges.append({"a": a, "b": b, "relation": relation})
    return {"edges": edges, "satiety": satiety, "deferred_nary": deferred_nary}


def build_adj(edges: list[dict[str, str]]):
    """multi (list adjacency, for legacy_mu) + simple (set adjacency, for BFS),
    mirroring the engine's build_graph for already-projected {a,b,relation}."""
    multi: dict[str, list[str]] = defaultdict(list)
    simple: dict[str, set[str]] = defaultdict(set)
    for e in edges:
        a, b = e["a"], e["b"]
        multi[a].append(b)
        multi[b].append(a)
        simple[a].add(b)
        simple[b].add(a)
    return multi, simple


def mission_curvature(proj: dict[str, Any]) -> dict[str, Any]:
    edges = proj["edges"]
    multi, simple = build_adj(edges)
    nodes = set(simple.keys())
    comps = eng.connected_components(simple) if nodes else []
    per_edge = []
    for e in edges:
        try:
            r = eng.curvature_for_edge(
                multi, simple, {"edge": [e["a"], e["b"]], "relation": e["relation"]}, legacy=True
            )
            per_edge.append({"edge": [e["a"], e["b"]], "relation": e["relation"],
                             "kappa": r.kappa, "w1": r.w1, "d": r.d})
        except RuntimeError as ex:
            per_edge.append({"edge": [e["a"], e["b"]], "relation": e["relation"], "error": str(ex)})
    kappas = [pe["kappa"] for pe in per_edge if "kappa" in pe]
    return {
        "n_nodes": len(nodes),
        "n_edges": len(edges),
        "n_components": len(comps),
        "n_deferred_nary": len(proj["deferred_nary"]),
        "satiety_full_frac": (sum(1 for v in proj["satiety"].values() if v >= 1.0) / len(proj["satiety"]))
        if proj["satiety"] else 0.0,
        "kappa": eng.quantiles(kappas) if kappas else None,
        "per_edge": per_edge,
    }


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--corpus", default="/home/joe/code/futon6/data/mission-triples")
    ap.add_argument("--out", default=None)
    ap.add_argument("--top", type=int, default=10)
    args = ap.parse_args()

    paths = sorted(Path(args.corpus).glob("*.edn"))
    missions = {}
    for p in paths:
        try:
            triple = edn_format.loads(p.read_text())
        except Exception as ex:  # noqa: BLE001 — a parse failure is data, report it
            missions[p.stem] = {"parse_error": str(ex)}
            continue
        proj = project_cascade(triple)
        cur = mission_curvature(proj)
        cur["mission"] = kw(get(triple, "mission")) if get(triple, "mission") else p.stem
        missions[p.stem] = cur

    ok = {k: v for k, v in missions.items() if "kappa" in v and v["kappa"]}
    all_medians = sorted((v["kappa"]["median"] for v in ok.values()))
    report = {
        "corpus": args.corpus,
        "terrain": "mined snapshot (commit-pinned per mission), NOT live HEAD",
        "curvature": "topological OR-Ricci (legacy lazy-walk, alpha=%.2f); satiety mu-feed is v2" % eng.ALPHA,
        "n_missions": len(missions),
        "n_with_curvature": len(ok),
        "n_empty_or_unprojectable": len(missions) - len(ok),
        "global_median_kappa": eng.quantiles(all_medians) if all_medians else None,
        "deferred_nary_total": sum(v.get("n_deferred_nary", 0) for v in ok.values()),
        "missions": missions,
    }
    if args.out:
        Path(args.out).write_text(json.dumps(report, indent=2))

    # --- terminal summary ---
    print("CASCADE CURVATURE — first read (%s)" % report["curvature"])
    print("terrain: %s" % report["terrain"])
    print("%d missions, %d with curvature, %d empty/unprojectable; n-ary deferred: %d"
          % (report["n_missions"], report["n_with_curvature"],
             report["n_empty_or_unprojectable"], report["deferred_nary_total"]))
    if report["global_median_kappa"]:
        g = report["global_median_kappa"]
        print("global per-mission median kappa: min %.4f / median %.4f / max %.4f"
              % (g["min"], g["median"], g["max"]))
    # the richly-grounded row stands out by edge count + satiety, not topology
    ranked = sorted(ok.values(), key=lambda v: (v["n_edges"], v["satiety_full_frac"]), reverse=True)
    print("\ntop %d by edge-count (depth proxy):" % args.top)
    for v in ranked[: args.top]:
        kq = v["kappa"]
        print("  %-34s edges=%2d nodes=%2d comps=%d satiety_full=%.2f kappa[min/med/max]=%.3f/%.3f/%.3f"
              % (v["mission"][:34], v["n_edges"], v["n_nodes"], v["n_components"],
                 v["satiety_full_frac"], kq["min"], kq["median"], kq["max"]))


if __name__ == "__main__":
    main()
