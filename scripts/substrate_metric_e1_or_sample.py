#!/usr/bin/env python3
"""Bounded Ollivier-Ricci sample for M-substrate-metric E1.

Read-only, E1-only substrate scope:
- fetches only feeds-mu relation families by hyperedge type;
- builds the structural multigraph;
- samples top structural bridge candidates;
- computes exact W1 by scipy.optimize.linprog over each local support.

This is a timing/scaling probe, not the full live curvature service.
"""

from __future__ import annotations

import argparse
import json
import time
import urllib.parse
import urllib.request
from collections import Counter, defaultdict, deque
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import numpy as np
from scipy.optimize import linprog


DEFAULT_TYPES = [
    "code/v05/related-mission",
    "code/v05/mission-cross-ref",
    "code/v05/file->mission",
    "code/v05/file→mission",
    "code/v05/sorry->related-missions",
]


@dataclass(frozen=True)
class Edge:
    a: str
    b: str
    relation: str


def fetch_type(base_url: str, hx_type: str, limit: int) -> List[dict]:
    query = urllib.parse.urlencode({"type": hx_type, "limit": str(limit)})
    req = urllib.request.Request(
        f"{base_url.rstrip('/')}/api/alpha/hyperedges?{query}",
        headers={"Accept": "application/json", "X-Penholder": "api"},
    )
    with urllib.request.urlopen(req, timeout=10) as resp:
        body = json.loads(resp.read().decode("utf-8"))
    return list(body.get("hyperedges") or [])


def normalize_relation(relation: str) -> Optional[str]:
    if relation.endswith("related-mission"):
        return "related-mission"
    if relation.endswith("mission-cross-ref"):
        return "mission-cross-ref"
    if "file" in relation and "mission" in relation:
        return "file->mission"
    if "sorry" in relation and "mission" in relation:
        return "sorry->related-missions"
    return None


def edge_from_hx(hx: dict) -> Optional[Edge]:
    relation = normalize_relation(str(hx.get("hx/type") or ""))
    endpoints = [
        e
        for e in hx.get("hx/endpoints", [])
        if not (isinstance(e, str) and e.startswith("dir:"))
    ]
    if relation and len(endpoints) == 2:
        return Edge(str(endpoints[0]), str(endpoints[1]), relation)
    return None


def build_graph(edges: Sequence[Edge]) -> Dict[str, List[str]]:
    graph: Dict[str, List[str]] = defaultdict(list)
    for edge in edges:
        graph[edge.a].append(edge.b)
        graph[edge.b].append(edge.a)
    return dict(graph)


def connected_components(graph: Dict[str, List[str]]) -> List[set[str]]:
    remaining = set(graph)
    components: List[set[str]] = []
    while remaining:
        start = next(iter(remaining))
        seen = {start}
        q = deque([start])
        while q:
            node = q.popleft()
            for nbr in graph.get(node, []):
                if nbr not in seen:
                    seen.add(nbr)
                    q.append(nbr)
        components.append(seen)
        remaining -= seen
    return components


def canonical_pair(a: str, b: str) -> Tuple[str, str]:
    return (a, b) if a <= b else (b, a)


def reachable_without_pair(
    graph: Dict[str, List[str]], source: str, blocked: Tuple[str, str]
) -> set[str]:
    seen = {source}
    q = deque([source])
    while q:
        node = q.popleft()
        for nbr in graph.get(node, []):
            if canonical_pair(node, nbr) == blocked:
                continue
            if nbr not in seen:
                seen.add(nbr)
                q.append(nbr)
    return seen


def bridge_candidates(graph: Dict[str, List[str]], edges: Sequence[Edge]) -> List[dict]:
    multiplicity = Counter(canonical_pair(e.a, e.b) for e in edges)
    relation_by_pair = {canonical_pair(e.a, e.b): e.relation for e in edges}
    comps = connected_components(graph)
    comp_by_node = {node: comp for comp in comps for node in comp}
    out = []
    for pair, n in multiplicity.items():
        if n != 1:
            continue
        a, b = pair
        side = reachable_without_pair(graph, a, pair)
        if b in side:
            continue
        comp_size = len(comp_by_node[a])
        other = comp_size - len(side)
        out.append(
            {
                "edge": [a, b],
                "relation": relation_by_pair[pair],
                "component_size": comp_size,
                "split_sizes": [len(side), other],
                "bridge_score": len(side) * other,
            }
        )
    out.sort(key=lambda x: x["bridge_score"], reverse=True)
    return out


def lazy_measure(graph: Dict[str, List[str]], node: str, alpha: float) -> Dict[str, float]:
    nbrs = graph.get(node, [])
    if not nbrs:
        return {node: 1.0}
    mass = (1.0 - alpha) / len(nbrs)
    out = defaultdict(float)
    out[node] += alpha
    for nbr in nbrs:
        out[nbr] += mass
    return dict(out)


def shortest_distances(graph: Dict[str, List[str]], source: str) -> Dict[str, int]:
    seen = {source: 0}
    q = deque([source])
    while q:
        node = q.popleft()
        for nbr in graph.get(node, []):
            if nbr not in seen:
                seen[nbr] = seen[node] + 1
                q.append(nbr)
    return seen


def wasserstein_1(
    graph: Dict[str, List[str]], p: Dict[str, float], q: Dict[str, float]
) -> float:
    left = list(p)
    right = list(q)
    costs = []
    for a in left:
        dist = shortest_distances(graph, a)
        for b in right:
            if b not in dist:
                raise ValueError(f"disconnected support pair: {a} {b}")
            costs.append(float(dist[b]))

    c = np.array(costs, dtype=float)
    a_eq = []
    b_eq = []

    for i, node in enumerate(left):
        row = np.zeros(len(left) * len(right))
        for j in range(len(right)):
            row[i * len(right) + j] = 1.0
        a_eq.append(row)
        b_eq.append(p[node])

    for j, node in enumerate(right):
        row = np.zeros(len(left) * len(right))
        for i in range(len(left)):
            row[i * len(right) + j] = 1.0
        a_eq.append(row)
        b_eq.append(q[node])

    res = linprog(
        c,
        A_eq=np.array(a_eq),
        b_eq=np.array(b_eq),
        bounds=(0.0, None),
        method="highs",
    )
    if not res.success:
        raise RuntimeError(res.message)
    return float(res.fun)


def run(args: argparse.Namespace) -> dict:
    t0 = time.perf_counter()
    fetched = {hx_type: fetch_type(args.futon1a_url, hx_type, args.limit) for hx_type in DEFAULT_TYPES}
    edges = [edge for xs in fetched.values() for hx in xs if (edge := edge_from_hx(hx))]
    graph = build_graph(edges)
    bridges = bridge_candidates(graph, edges)
    selected = bridges[: args.sample]

    rows = []
    solve_start = time.perf_counter()
    for candidate in selected:
        a, b = candidate["edge"]
        p = lazy_measure(graph, a, args.alpha)
        q = lazy_measure(graph, b, args.alpha)
        edge_start = time.perf_counter()
        w1 = wasserstein_1(graph, p, q)
        elapsed_ms = (time.perf_counter() - edge_start) * 1000.0
        rows.append(
            {
                **candidate,
                "support_sizes": [len(p), len(q)],
                "w1": w1,
                "kappa": 1.0 - w1,
                "solve_ms": elapsed_ms,
            }
        )

    return {
        "report": "substrate-metric/e1-or-sample",
        "limit_per_type": args.limit,
        "sample": args.sample,
        "alpha": args.alpha,
        "fetched_by_type": {k: len(v) for k, v in fetched.items()},
        "node_count": len(graph),
        "edge_count": len(edges),
        "component_count": len(connected_components(graph)),
        "bridge_candidate_count": len(bridges),
        "sampled_edges": rows,
        "solve_total_ms": (time.perf_counter() - solve_start) * 1000.0,
        "total_ms": (time.perf_counter() - t0) * 1000.0,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--futon1a-url", default=os.environ.get("FUTON1A_URL", "http://localhost:7071"))
    parser.add_argument("--limit", type=int, default=2000)
    parser.add_argument("--sample", type=int, default=8)
    parser.add_argument("--alpha", type=float, default=0.5)
    args = parser.parse_args()
    print(json.dumps(run(args), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
