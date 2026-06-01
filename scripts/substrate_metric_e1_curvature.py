#!/usr/bin/env python3
"""Bounded E1 Ollivier-Ricci curvature run for M-substrate-metric.

Read-only. Fetches only E1 feeds-mu relations plus mission/sorry docs from the
hyperedge API. Computes lazy-walk Ollivier-Ricci curvature on a capped edge
sample with scipy.optimize.linprog for exact transport on sparse supports.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import time
import urllib.parse
import urllib.request
from collections import Counter, defaultdict, deque
from dataclasses import dataclass
from typing import Any

import numpy as np
from scipy.optimize import linprog


FUTON1A = os.environ.get("FUTON1A_URL", "http://localhost:7071")
PENHOLDER = os.environ.get("FUTON1A_PENHOLDER", "api")
ALPHA = 0.5

FEEDS_MU_TYPES = [
    "code/v05/related-mission",
    "code/v05/mission-cross-ref",
    "code/v05/file->mission",
    "code/v05/file→mission",
    "code/v05/sorry->related-missions",
]

MISSION_PHASE = {
    "head": 0.10,
    "identify": 0.10,
    "map": 0.10,
    "derive": 0.10,
    "argue": 0.35,
    "verify": 0.35,
    "instantiate": 0.65,
    "document": 0.65,
    "complete": 1.00,
    "closed": 1.00,
    "dissolved": 1.00,
}

SORRY_STATUS = {
    "open": 0.00,
    "reopened": 0.00,
    "addressed": 0.65,
    "closed": 1.00,
    "foreclosed": 1.00,
    "falsified": 1.00,
    "n-a-by-design": 1.00,
    "acknowledged-v1-in-force": 1.00,
}


def fetch_hyperedges_by_type(hx_type: str, limit: int) -> list[dict[str, Any]]:
    query = urllib.parse.urlencode({"type": hx_type, "limit": str(limit)})
    req = urllib.request.Request(
        f"{FUTON1A}/api/alpha/hyperedges?{query}",
        headers={"Accept": "application/json", "X-Penholder": PENHOLDER},
    )
    with urllib.request.urlopen(req, timeout=20) as resp:
        body = json.loads(resp.read().decode("utf-8"))
    return list(body.get("hyperedges") or [])


def endpoint_type(endpoint: str) -> str:
    if "/mission/" in endpoint:
        return "mission"
    if "/sorry/" in endpoint:
        return "sorry"
    if "/pattern/" in endpoint:
        return "pattern"
    if "/file/" in endpoint:
        return "file"
    return "unknown"


def mission_artifact_endpoint(endpoint: str) -> bool:
    """Mission-adjacent report artifacts are not O1 mission nodes."""
    if "/mission/" not in endpoint:
        return False
    tail = endpoint.rsplit("/", 1)[-1]
    return bool(re.search(r"(?:\.R[0-9][A-Za-z0-9.-]*|\.OR-sample)$", tail))


def edge_relation(edge: dict[str, Any]) -> str | None:
    hx_type = str(edge.get("hx/type") or edge.get("type") or "")
    if hx_type.endswith("related-mission"):
        return "related-mission"
    if hx_type.endswith("mission-cross-ref"):
        return "mission-cross-ref"
    if "file" in hx_type and "mission" in hx_type:
        return "file->mission"
    if "sorry" in hx_type and "mission" in hx_type:
        return "sorry->related-missions"
    return None


def endpoints(edge: dict[str, Any]) -> tuple[str, str] | None:
    eps = [
        x
        for x in edge.get("hx/endpoints", [])
        if not (isinstance(x, str) and x.startswith("dir:"))
    ]
    if len(eps) != 2:
        return None
    return str(eps[0]), str(eps[1])


def norm_token(x: Any) -> str | None:
    if x is None:
        return None
    s = str(x).strip().lower()
    if s.startswith(":"):
        s = s[1:]
    return s


def prop(props: dict[str, Any], key: str) -> Any:
    return props.get(key) if key in props else props.get(key.split("/")[-1])


def resolution_state(endpoint: str, docs: dict[str, dict[str, Any]]) -> dict[str, Any]:
    typ = endpoint_type(endpoint)
    doc = docs.get(endpoint) or {}
    props = doc.get("hx/props") or {}
    if typ == "mission":
        raw = prop(props, "mission/phase") or prop(props, "mission/status") or prop(props, "phase")
        token = norm_token(raw)
        resolvedness = MISSION_PHASE.get(token)
        return {
            "resolvedness": resolvedness if resolvedness is not None else "unknown",
            "actionable": resolvedness is not None and resolvedness < 1.0,
            "raw": {"phase": raw, "normalized": token},
        }
    if typ == "sorry":
        raw = prop(props, "sorry/status") or prop(props, "status")
        token = norm_token(raw)
        resolvedness = SORRY_STATUS.get(token)
        return {
            "resolvedness": resolvedness if resolvedness is not None else "unknown",
            "actionable": resolvedness is not None and resolvedness < 1.0,
            "raw": {"status": raw, "normalized": token},
        }
    if typ == "pattern":
        return {"resolvedness": "unknown", "actionable": False, "raw": {"reason": "no-pattern-docs"}}
    return {"resolvedness": "unknown", "actionable": False, "raw": {"reason": f"{typ}-non-actionable"}}


def pair(a: str, b: str) -> tuple[str, str]:
    return (a, b) if a < b else (b, a)


def build_graph(edges: list[dict[str, Any]]) -> tuple[dict[str, list[str]], dict[str, set[str]], list[dict[str, Any]]]:
    multi: dict[str, list[str]] = defaultdict(list)
    simple: dict[str, set[str]] = defaultdict(set)
    normalized = []
    for edge in edges:
        relation = edge_relation(edge)
        eps = endpoints(edge)
        if not relation or not eps:
            continue
        a, b = eps
        if mission_artifact_endpoint(a) or mission_artifact_endpoint(b):
            continue
        multi[a].append(b)
        multi[b].append(a)
        simple[a].add(b)
        simple[b].add(a)
        normalized.append({"a": a, "b": b, "relation": relation})
    return multi, simple, normalized


def connected_components(simple: dict[str, set[str]]) -> list[set[str]]:
    remaining = set(simple.keys())
    comps = []
    while remaining:
        start = next(iter(remaining))
        q = deque([start])
        seen = {start}
        while q:
            node = q.popleft()
            for nbr in simple[node]:
                if nbr not in seen:
                    seen.add(nbr)
                    q.append(nbr)
        remaining -= seen
        comps.append(seen)
    return comps


def reachable_without(simple: dict[str, set[str]], start: str, blocked: tuple[str, str]) -> set[str]:
    q = deque([start])
    seen = {start}
    while q:
        node = q.popleft()
        for nbr in simple[node]:
            if pair(node, nbr) == blocked:
                continue
            if nbr not in seen:
                seen.add(nbr)
                q.append(nbr)
    return seen


def bridge_candidates(simple: dict[str, set[str]], edges: list[dict[str, Any]]) -> list[dict[str, Any]]:
    multiplicity = Counter(pair(e["a"], e["b"]) for e in edges)
    rel_by_pair = {pair(e["a"], e["b"]): e["relation"] for e in edges}
    comps = connected_components(simple)
    comp_by_node = {node: comp for comp in comps for node in comp}
    out = []
    for p, n in multiplicity.items():
        if n != 1:
            continue
        a, b = p
        side = reachable_without(simple, a, p)
        if b in side:
            continue
        comp_size = len(comp_by_node[a])
        split = [len(side), comp_size - len(side)]
        out.append(
            {
                "edge": [a, b],
                "relation": rel_by_pair[p],
                "component_size": comp_size,
                "split_sizes": split,
                "bridge_score": split[0] * split[1],
            }
        )
    return sorted(out, key=lambda x: x["bridge_score"], reverse=True)


def mu(multi: dict[str, list[str]], node: str) -> dict[str, float]:
    nbrs = multi.get(node, [])
    if not nbrs:
        return {node: 1.0}
    mass = (1.0 - ALPHA) / len(nbrs)
    out = defaultdict(float)
    out[node] += ALPHA
    for nbr in nbrs:
        out[nbr] += mass
    return dict(out)


def bfs_distances(simple: dict[str, set[str]], source: str, targets: set[str]) -> dict[str, int]:
    q = deque([(source, 0)])
    seen = {source}
    found = {}
    while q and len(found) < len(targets):
        node, dist = q.popleft()
        if node in targets:
            found[node] = dist
        for nbr in simple[node]:
            if nbr not in seen:
                seen.add(nbr)
                q.append((nbr, dist + 1))
    return found


def wasserstein(multi: dict[str, list[str]], simple: dict[str, set[str]], x: str, y: str) -> float:
    supply = mu(multi, x)
    demand = mu(multi, y)
    sources = list(supply.keys())
    targets = list(demand.keys())
    costs = []
    for s in sources:
        dist = bfs_distances(simple, s, set(targets))
        costs.extend(float(dist[t]) for t in targets)
    n = len(sources) * len(targets)
    a_eq = []
    b_eq = []
    for i, s in enumerate(sources):
        row = np.zeros(n)
        for j in range(len(targets)):
            row[i * len(targets) + j] = 1.0
        a_eq.append(row)
        b_eq.append(supply[s])
    for j, t in enumerate(targets):
        row = np.zeros(n)
        for i in range(len(sources)):
            row[i * len(targets) + j] = 1.0
        a_eq.append(row)
        b_eq.append(demand[t])
    res = linprog(
        c=np.array(costs),
        A_eq=np.vstack(a_eq),
        b_eq=np.array(b_eq),
        bounds=(0.0, None),
        method="highs",
    )
    if not res.success:
        raise RuntimeError(f"transport failed for {x} {y}: {res.message}")
    return float(res.fun)


@dataclass
class CurvatureResult:
    x: str
    y: str
    relation: str
    kappa: float
    w1: float
    bridge_score: int | None
    elapsed_ms: float


def curvature_for_edge(
    multi: dict[str, list[str]], simple: dict[str, set[str]], edge: dict[str, Any]
) -> CurvatureResult:
    start = time.perf_counter()
    x, y = edge["edge"]
    w1 = wasserstein(multi, simple, x, y)
    return CurvatureResult(
        x=x,
        y=y,
        relation=edge["relation"],
        kappa=1.0 - w1,  # d(x,y)=1 for sampled graph edges
        w1=w1,
        bridge_score=edge.get("bridge_score"),
        elapsed_ms=(time.perf_counter() - start) * 1000.0,
    )


def quantiles(xs: list[float]) -> dict[str, float]:
    arr = np.array(xs, dtype=float)
    return {
        "min": float(np.min(arr)),
        "p10": float(np.quantile(arr, 0.10)),
        "median": float(np.quantile(arr, 0.50)),
        "p90": float(np.quantile(arr, 0.90)),
        "max": float(np.max(arr)),
    }


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit-per-type", type=int, default=2000)
    ap.add_argument("--edge-cap", type=int, default=200)
    ap.add_argument("--top", type=int, default=25)
    args = ap.parse_args()

    t0 = time.perf_counter()
    fetched = {t: fetch_hyperedges_by_type(t, args.limit_per_type) for t in FEEDS_MU_TYPES}
    mission_docs = {
        (doc.get("hx/endpoints") or [None])[0]: doc
        for doc in fetch_hyperedges_by_type("code/v05/mission-doc", 1000)
    }
    sorry_docs = {
        (doc.get("hx/endpoints") or [None])[0]: doc
        for doc in fetch_hyperedges_by_type("code/v05/sorry", 1000)
    }
    docs = {**mission_docs, **sorry_docs}
    multi, simple, edges = build_graph([e for xs in fetched.values() for e in xs])
    comps = connected_components(simple)
    bridges = bridge_candidates(simple, edges)
    sample_edges = bridges[: args.edge_cap]

    results = [curvature_for_edge(multi, simple, e) for e in sample_edges]
    by_node: dict[str, list[CurvatureResult]] = defaultdict(list)
    for r in results:
        by_node[r.x].append(r)
        by_node[r.y].append(r)

    candidates = []
    for node, incident in by_node.items():
        state = resolution_state(node, docs)
        min_edge = min(incident, key=lambda r: r.kappa)
        resolvedness = state["resolvedness"]
        actionable = bool(state["actionable"])
        propose = actionable and isinstance(resolvedness, (float, int)) and resolvedness < 1.0 and min_edge.kappa < 0.0
        if propose:
            candidates.append(
                {
                    "node": node,
                    "node_type": endpoint_type(node),
                    "resolvedness": resolvedness,
                    "min_incident_kappa": min_edge.kappa,
                    "action_intensity": max(0.0, -min_edge.kappa) * (1.0 - float(resolvedness)),
                    "strain_edge": [min_edge.x, min_edge.y],
                    "relation": min_edge.relation,
                    "raw_resolution": state["raw"],
                }
            )
    candidates.sort(key=lambda c: c["action_intensity"], reverse=True)
    elapsed = time.perf_counter() - t0

    out = {
        "report": "substrate-metric/e1-or-sample",
        "limit_per_type": args.limit_per_type,
        "edge_cap": args.edge_cap,
        "fetched_by_type": {k: len(v) for k, v in fetched.items()},
        "node_count": len(simple),
        "edge_count": len(edges),
        "component_count": len(comps),
        "largest_component": max(len(c) for c in comps) if comps else 0,
        "bridge_candidate_count": len(bridges),
        "curvature_edges_sampled": len(results),
        "curvature_distribution": quantiles([r.kappa for r in results]) if results else {},
        "timing": {
            "total_seconds": elapsed,
            "mean_ms_per_edge": float(np.mean([r.elapsed_ms for r in results])) if results else 0.0,
            "max_ms_per_edge": float(np.max([r.elapsed_ms for r in results])) if results else 0.0,
        },
        "top_negative_edges": [
            {
                "edge": [r.x, r.y],
                "relation": r.relation,
                "kappa": r.kappa,
                "w1": r.w1,
                "bridge_score": r.bridge_score,
            }
            for r in sorted(results, key=lambda r: r.kappa)[: args.top]
        ],
        "top_propose_candidates": candidates[: args.top],
    }
    print(json.dumps(out, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
