#!/usr/bin/env python3
"""Build the pattern graph that the operator-turn mining implies.

Reads every published *.analysis.json under the batch directories and the
live session-turn analysis directory, and emits typed, evidenced edges between
library patterns.  Nothing here is written
into the flexiargs: @why stays authored, and these edges are a separate,
regenerable layer that says where the mining put patterns next to each other.

Edge kinds, strongest first:
  why             authored causal ancestry
  how             authored practical recipe
  used-together   patterns retained as jointly enacted by an applied run diff
  co-cited         two patterns cited in the same operator turn
  rejected-beside  a pattern read and turned down for a fragment that cites
                   another; the rejection reason says where the boundary runs
  next-in-session  patterns cited in consecutive analysed turns of a session
"Both turned down for the same fragment" (co-rejected) is no longer written
down (Joe, 2026-09-30: too weak an association to record, until further
notice). It mostly recorded what the search returned together, and it was what
attached 253 patterns to the giant component. To bring it back, restore the
pairs(rejs) loop in mined_edges and the kind in KINDS; pattern_retraction.py
still prices the kind for graph files written before this date.
The graph is undirected; authored edges from the library are included, marked
as such, so components can be read with or without them. There are two
authored kinds, and they are different topologies (Joe, 2026-09-27):
  why   @why lists the patterns this one follows from: causal, reasoning
        backwards from the pattern to what forced it
  how   pattern ids cited in @how, the pattern's practical recipe:
        pragmatic, reasoning forwards to what you do next
@why is a list of ids; @how is prose that sometimes cites ids, so an id
counts there only where it resolves to a library file.

Usage: mined_pattern_graph.py [--batches DIR] [--live DIR] [--library DIR] [--out FILE]
Prints a component summary per cumulative edge kind.
"""
import argparse
import collections
import glob
import json
import os
import re

KINDS = ["why", "how", "used-together", "co-cited", "rejected-beside", "next-in-session"]
DEFAULT_DIFFS = "/home/joe/code/storage/operator-turns/pattern-graph-diffs/applied"


def library_ids(lib):
    ids = {}
    for p in glob.glob(os.path.join(lib, "**", "*.flexiarg"), recursive=True):
        ids[os.path.relpath(p, lib)[: -len(".flexiarg")]] = p
    return ids


def why_edges(ids):
    out = []
    for pid, path in ids.items():
        with open(path, errors="ignore") as fh:
            for line in fh:
                if line.lstrip().startswith("@why"):
                    for tok in re.split(r"[\s,\[\]]+", line.strip()[4:]):
                        if tok in ids and tok != pid:
                            out.append((pid, tok, {"file": path}))
    return out


ID_IN_PROSE = re.compile(r"[\w\-\u4e00-\u9fff]+/[\w\-\u4e00-\u9fff]+")


def how_edges(ids):
    out = []
    for pid, path in ids.items():
        with open(path, errors="ignore") as fh:
            for line in fh:
                if line.lstrip().startswith("@how"):
                    for tok in ID_IN_PROSE.findall(line):
                        if tok in ids and tok != pid:
                            out.append((pid, tok, {"file": path}))
    return out


def pairs(xs):
    xs = sorted(xs)
    return [(xs[i], xs[j]) for i in range(len(xs)) for j in range(i + 1, len(xs))]


def analysis_inputs(batches, live):
    """Return (path, display path, fallback session) for batch and live records."""
    inputs = []
    for path in sorted(glob.glob(os.path.join(batches, "*", "*.analysis.json"))):
        rel = os.path.relpath(path, batches)
        inputs.append((path, rel, rel.split("/")[0]))
    if live:
        for path in sorted(glob.glob(os.path.join(live, "turn-*.json.analysis.json"))):
            rel = os.path.join("live", os.path.relpath(path, live))
            inputs.append((path, rel, "live"))
    return inputs


def mined_edges(batches, live, ids):
    edges = collections.defaultdict(list)
    sessions = collections.defaultdict(list)
    records = 0
    for f, rel, fallback_session in analysis_inputs(batches, live):
        d = json.load(open(f))
        records += 1
        req = {}
        request_path = os.path.expanduser(d.get("request_file", ""))
        if request_path and not os.path.isabs(request_path):
            request_path = os.path.join(os.path.dirname(f), request_path)
        if os.path.exists(request_path):
            req = json.load(open(request_path))
        turn_cites = set()
        for s in d.get("sentences", []):
            for k, fr in enumerate(s.get("fragments", [])):
                where = f"{rel}#{s['id']}.{k}"
                cited = {r["id"] for r in fr.get("pattern_refs", []) if r["id"] in ids}
                rejs = [r for r in fr.get("pattern_rejections", []) if r["id"] in ids]
                turn_cites |= cited
                for r in rejs:
                    for c in cited:
                        if c != r["id"]:
                            edges["rejected-beside"].append(
                                (r["id"], c, {"at": where, "reason": r.get("reason", "")}))
        for a, b in pairs(turn_cites):
            edges["co-cited"].append((a, b, {"at": rel}))
        if turn_cites:
            sessions[req.get("session_id") or fallback_session].append(
                (req.get("created_at") or "", rel, sorted(turn_cites)))
    for turns in sessions.values():
        turns.sort()
        for (_, r1, a), (_, r2, b) in zip(turns, turns[1:]):
            for x in a:
                for y in b:
                    if x != y:
                        edges["next-in-session"].append((x, y, {"at": f"{r1} -> {r2}"}))
    return edges, records


def components(edge_list, nodes):
    parent = {n: n for n in nodes}

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    for a, b, _ in edge_list:
        parent[find(a)] = find(b)
    groups = collections.defaultdict(list)
    for n in nodes:
        groups[find(n)].append(n)
    return sorted(groups.values(), key=len, reverse=True)


def applied_diffs(directory, ids):
    edges, uses = [], []
    if not directory or not os.path.isdir(directory):
        return edges, uses
    for path in sorted(glob.glob(os.path.join(directory, "*.json"))):
        with open(path) as handle:
            diff = json.load(handle)
        if diff.get("schema") != "pattern-graph-diff-v1":
            continue
        source = diff.get("source", {})
        for item in diff.get("add_edges", []):
            if item.get("a") in ids and item.get("b") in ids:
                for evidence in item.get("evidence", []):
                    edges.append((item["a"], item["b"], evidence))
        for item in diff.get("add_uses", []):
            if item.get("pattern") in ids:
                uses.append({"pattern": item["pattern"], "run": source.get("run"),
                             "target": source.get("target"),
                             "position": item.get("position"),
                             "wants": item.get("wants", [])})
    return edges, uses


def build_graph(batches, live, library, diffs=None):
    """Return the JSON graph value without printing or writing it."""
    ids = library_ids(library)
    edges, records = mined_edges(batches, live, ids)
    edges["why"] = why_edges(ids)
    edges["how"] = how_edges(ids)
    edges["used-together"], uses = applied_diffs(diffs, ids)

    merged = {}
    for kind in KINDS:
        for a, b, ev in edges[kind]:
            key = (min(a, b), max(a, b), kind)
            e = merged.setdefault(key, {"a": key[0], "b": key[1], "kind": kind, "evidence": []})
            e["evidence"].append(ev)

    acc = []
    summary = []
    for kind in KINDS:
        acc += edges[kind]
        comps = components(acc, list(ids))
        singles = sum(1 for c in comps if len(c) == 1)
        n = len({(min(a, b), max(a, b)) for a, b, _ in edges[kind]})
        summary.append({"through": kind, "edges": n, "giant": len(comps[0]),
                        "components": len(comps), "singletons": singles})

    strong = [e for k in ("used-together", "co-cited", "rejected-beside") for e in edges[k]]
    giant = components(strong + edges["why"] + edges["how"], list(ids))[0]
    final_components = components(acc, list(ids))
    component_by_pattern = {
        pattern: index for index, component in enumerate(final_components, 1)
        for pattern in component
    }
    xiang_patterns = sorted(pattern for pattern in ids if pattern.startswith("象/"))
    xiang_components = sorted({component_by_pattern[pattern] for pattern in xiang_patterns})
    xiang_in_giant = bool(xiang_patterns) and xiang_components == [1]
    xiang_summary = {"patterns": xiang_patterns,
                     "components": xiang_components,
                     "giant_component": 1 if final_components else None,
                     "in_giant_component": xiang_in_giant}
    return {"records": records, "patterns": len(ids), "pattern_ids": sorted(ids),
            "summary": summary,
            "giant_without_weak_edges": sorted(giant),
            "象_family": xiang_summary,
            "uses": uses,
            "edges": sorted(merged.values(), key=lambda e: (e["kind"], e["a"], e["b"]))}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--batches", default="/home/joe/code/storage/operator-turns/batches")
    ap.add_argument("--live", default=os.path.expanduser("~/.emacs-graph/session-turn-analysis"),
                    help="live turn analysis directory; pass an empty path to disable")
    ap.add_argument("--library", default="/home/joe/code/futon3/library")
    ap.add_argument("--out", default="/home/joe/code/storage/operator-turns/mined-pattern-graph.json")
    ap.add_argument("--diffs", default=DEFAULT_DIFFS,
                    help="directory of inspected, manually applied pattern graph diffs")
    args = ap.parse_args()
    graph = build_graph(args.batches, args.live, args.library, args.diffs)

    print(f"{graph['records']} analyses, {graph['patterns']} library patterns")
    for row in graph["summary"]:
        print(f"+{row['through']:16} {row['edges']:5} edges  "
              f"giant {row['giant']:4}/{graph['patterns']}  "
              f"components {row['components']:4}  singletons {row['singletons']}")
    xiang = graph["象_family"]
    print("象 family components "
          f"{xiang['components'] or 'none'}; giant component 1; "
          f"in giant: {str(xiang['in_giant_component']).lower()}; "
          f"patterns {len(xiang['patterns'])}")
    with open(args.out, "w") as fh:
        json.dump(graph, fh, indent=1)
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
