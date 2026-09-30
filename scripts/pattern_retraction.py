#!/usr/bin/env python3
"""Compute deterministic, inexpensive retractions of the pattern graph."""
import argparse
import hashlib
import heapq
import json
import os
import sys

import mined_pattern_graph


KIND_ORDER = tuple(mined_pattern_graph.KINDS)
DEFAULT_WEIGHTS = {
    "why": 1, "how": 1, "co-cited": 2, "rejected-beside": 3,
    "co-rejected": 8, "next-in-session": 8,
}
WEAK_KINDS = {"co-rejected", "next-in-session"}


def canonical_graph(graph):
    edges = []
    for edge in graph.get("edges", []):
        item = {"a": min(edge["a"], edge["b"]),
                "b": max(edge["a"], edge["b"]),
                "kind": edge["kind"],
                "evidence": sorted(edge.get("evidence", []),
                                   key=lambda x: json.dumps(x, sort_keys=True,
                                                            ensure_ascii=False))}
        edges.append(item)
    return sorted(edges, key=lambda x: (x["a"], x["b"], x["kind"],
                                       json.dumps(x["evidence"], sort_keys=True,
                                                  ensure_ascii=False)))


def graph_digest(edges):
    raw = json.dumps(edges, sort_keys=True, separators=(",", ":"),
                     ensure_ascii=False).encode()
    return hashlib.sha256(raw).hexdigest()


def pair_graph(edges, weights):
    pairs = {}
    for edge in edges:
        key = (edge["a"], edge["b"])
        value = pairs.setdefault(key, {"kinds": [], "by_kind": {}})
        if edge["kind"] not in value["kinds"]:
            value["kinds"].append(edge["kind"])
        value["by_kind"].setdefault(edge["kind"], []).extend(edge["evidence"])
    order = {kind: index for index, kind in enumerate(KIND_ORDER)}
    for value in pairs.values():
        value["kinds"].sort(key=lambda k: (order.get(k, len(order)), k))
        value["kind_used"] = min(value["kinds"],
                                 key=lambda k: (weights[k], order.get(k, len(order)), k))
        value["cost"] = weights[value["kind_used"]]
    return pairs


def adjacency(pairs, removed=frozenset()):
    out = {}
    for (a, b), value in pairs.items():
        if (a, b) in removed:
            continue
        out.setdefault(a, []).append((b, value["cost"], (a, b)))
        out.setdefault(b, []).append((a, value["cost"], (a, b)))
    for node in out:
        out[node].sort(key=lambda x: (x[1], x[0], x[2]))
    return out


def shortest_path(adj, source, target):
    queue = [(0, (source,), source, ())]
    best = {}
    while queue:
        cost, nodes, node, path = heapq.heappop(queue)
        key = (cost, nodes)
        if node in best and best[node] <= key:
            continue
        best[node] = key
        if node == target:
            return cost, path
        for neighbour, edge_cost, edge in adj.get(node, []):
            if neighbour in nodes:
                continue
            heapq.heappush(queue, (cost + edge_cost, nodes + (neighbour,),
                                  neighbour, path + (edge,)))
    return None


def prune_nonseed_leaves(edge_set, seeds):
    edges = set(edge_set)
    while True:
        degree = {}
        for a, b in edges:
            degree[a] = degree.get(a, 0) + 1
            degree[b] = degree.get(b, 0) + 1
        leaves = sorted(n for n, d in degree.items() if d == 1 and n not in seeds)
        if not leaves:
            return edges
        leaf = leaves[0]
        edges.remove(next(edge for edge in edges if leaf in edge))


def steiner_tree(pairs, seeds, removed=frozenset()):
    adj = adjacency(pairs, removed)
    closure = []
    for i, left in enumerate(seeds):
        for right in seeds[i + 1:]:
            found = shortest_path(adj, left, right)
            if found is None:
                return None
            cost, path = found
            closure.append((cost, left, right, path))
    parent = {seed: seed for seed in seeds}

    def find(node):
        while parent[node] != node:
            parent[node] = parent[parent[node]]
            node = parent[node]
        return node

    expanded = set()
    for cost, left, right, path in sorted(closure,
                                          key=lambda x: (x[0], x[1], x[2], x[3])):
        del cost
        aroot, broot = find(left), find(right)
        if aroot != broot:
            parent[aroot] = broot
            expanded.update(path)
    if len({find(seed) for seed in seeds}) != 1:
        return None
    return prune_nonseed_leaves(expanded, set(seeds))


def authored_direction(pair, kinds, by_kind):
    directions = []
    a, b = pair
    for kind in kinds:
        if kind not in {"why", "how"}:
            continue
        for evidence in by_kind.get(kind, []):
            path = evidence.get("file", "")
            source = next((node for node in (a, b)
                           if path.replace("\\", "/").endswith("/" + node + ".flexiarg")),
                          None)
            if source:
                directions.append((source, b if source == a else a))
    if not directions:
        return None
    source, target = sorted(set(directions))[0]
    return {"from": source, "to": target}


def render_tree(edge_set, pairs):
    nodes = sorted({node for edge in edge_set for node in edge})
    rendered = []
    for pair in sorted(edge_set):
        value = pairs[pair]
        evidence = []
        for kind in value["kinds"]:
            evidence.extend(value["by_kind"].get(kind, []))
        evidence.sort(key=lambda x: json.dumps(x, sort_keys=True, ensure_ascii=False))
        rendered.append({"a": pair[0], "b": pair[1], "kinds": value["kinds"],
                         "kind_used": value["kind_used"],
                         "direction": authored_direction(pair, value["kinds"],
                                                          value["by_kind"]),
                         "evidence": evidence})
    return {"cost": sum(pairs[e]["cost"] for e in edge_set),
            "size": len(nodes),
            "weak_edges": sum(pairs[e]["kind_used"] in WEAK_KINDS for e in edge_set),
            "nodes": nodes, "edges": rendered}


def components(nodes, pairs):
    adj = adjacency(pairs)
    unseen = set(nodes)
    result = []
    while unseen:
        start = min(unseen)
        stack, found = [start], set()
        while stack:
            node = stack.pop()
            if node in found:
                continue
            found.add(node)
            stack.extend(n for n, _, _ in adj.get(node, []) if n not in found)
        unseen -= found
        result.append(sorted(found))
    return sorted(result, key=lambda c: (-len(c), c))


def retractions(graph, seeds, k, weights):
    canonical = canonical_graph(graph)
    pairs = pair_graph(canonical, weights)
    nodes = sorted({n for edge in canonical for n in (edge["a"], edge["b"])})
    unknown = sorted(set(seeds) - set(nodes))
    if unknown:
        raise ValueError("unknown seed id: " + ", ".join(unknown))
    comps = components(nodes, pairs)
    seed_components = [{"component": index, "size": len(comp),
                        "seeds": sorted(set(seeds) & set(comp))}
                       for index, comp in enumerate(comps, 1)
                       if set(seeds) & set(comp)]
    connected = len(seed_components) == 1
    answer = []
    if connected:
        first = set() if len(seeds) == 1 else steiner_tree(pairs, seeds)
        if first is not None:
            answer.append(render_tree(first, pairs))
            if len(seeds) == 1:
                answer[0]["nodes"] = list(seeds)
                answer[0]["size"] = 1
        candidates, seen_removed = [], {frozenset()}

        def branch(rendered, already_removed):
            previous = {(edge["a"], edge["b"]) for edge in rendered["edges"]}
            for edge in sorted(previous):
                removed = frozenset(set(already_removed) | {edge})
                if removed in seen_removed:
                    continue
                seen_removed.add(removed)
                tree = steiner_tree(pairs, seeds, removed)
                if tree is not None:
                    candidate = render_tree(tree, pairs)
                    key = (candidate["cost"], candidate["nodes"],
                           [(e["a"], e["b"], e["kind_used"])
                            for e in candidate["edges"]])
                    candidates.append((key, candidate, removed))

        if answer:
            branch(answer[0], frozenset())
        while len(answer) < k and candidates:
            candidates.sort(key=lambda x: x[0])
            _, chosen, removed = candidates.pop(0)
            branch(chosen, removed)
            if tuple(chosen["nodes"]) not in {tuple(item["nodes"]) for item in answer}:
                answer.append(chosen)
    for rank, item in enumerate(answer, 1):
        item["rank"] = rank
    component_size = seed_components[0]["size"] if connected else 0
    return {"graph": {"digest": graph_digest(canonical),
                      "patterns": graph.get("patterns", len(nodes)),
                      "edges": len(canonical), "inputs": graph.get("records")},
            "seeds": seeds, "params": {"k": k, "weights": weights},
            "component": {"size": component_size, "seeds_connected": connected,
                          "seed_components": seed_components},
            "retractions": answer}


def parse_weights(raw):
    weights = dict(DEFAULT_WEIGHTS)
    if not raw:
        return weights
    for assignment in raw.split(","):
        kind, sep, value = assignment.partition("=")
        if not sep or kind not in weights:
            raise ValueError("invalid weight: " + assignment)
        weights[kind] = int(value)
        if weights[kind] < 0:
            raise ValueError("weight must be non-negative: " + assignment)
    return weights


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--seeds", required=True)
    parser.add_argument("--k", type=int, default=3)
    parser.add_argument("--weights")
    parser.add_argument("--graph")
    parser.add_argument("--out")
    args = parser.parse_args(argv)
    try:
        seeds = sorted(set(filter(None, (part.strip() for part in args.seeds.split(",")))))
        if not seeds:
            raise ValueError("at least one seed id is required")
        if args.k < 1:
            raise ValueError("k must be positive")
        weights = parse_weights(args.weights)
        if args.graph:
            with open(args.graph) as handle:
                graph = json.load(handle)
        else:
            graph = mined_pattern_graph.build_graph(
                "/home/joe/code/storage/operator-turns/batches",
                os.path.expanduser("~/.emacs-graph/session-turn-analysis"),
                "/home/joe/code/futon3/library")
        result = retractions(graph, seeds, args.k, weights)
        text = json.dumps(result, indent=2, sort_keys=True, ensure_ascii=False) + "\n"
        if args.out:
            with open(args.out, "w") as handle:
                handle.write(text)
        else:
            sys.stdout.write(text)
        return 0
    except (ValueError, OSError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
