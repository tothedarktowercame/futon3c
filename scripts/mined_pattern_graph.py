#!/usr/bin/env python3
"""Build the pattern graph that the operator-turn mining implies.

Reads every published *.analysis.json under the batch directories and emits
typed, evidenced edges between library patterns.  Nothing here is written
into the flexiargs: @why stays authored, and these edges are a separate,
regenerable layer that says where the mining put patterns next to each other.

Edge kinds, strongest first:
  co-cited         two patterns cited in the same operator turn
  rejected-beside  a pattern read and turned down for a fragment that cites
                   another; the rejection reason says where the boundary runs
  co-rejected      two patterns both turned down for the same fragment (these
                   largely reflect what the search returned together)
  next-in-session  patterns cited in consecutive analysed turns of a session
The graph is undirected; @why edges from the library are included, marked
as such, so components can be read with or without them.

Usage: mined_pattern_graph.py [--batches DIR] [--library DIR] [--out FILE]
Prints a component summary per cumulative edge kind.
"""
import argparse
import collections
import glob
import json
import os
import re

KINDS = ["why", "co-cited", "rejected-beside", "co-rejected", "next-in-session"]


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


def pairs(xs):
    xs = sorted(xs)
    return [(xs[i], xs[j]) for i in range(len(xs)) for j in range(i + 1, len(xs))]


def mined_edges(batches, ids):
    edges = collections.defaultdict(list)
    sessions = collections.defaultdict(list)
    records = 0
    for f in sorted(glob.glob(os.path.join(batches, "*", "*.analysis.json"))):
        d = json.load(open(f))
        records += 1
        req = {}
        if os.path.exists(d.get("request_file", "")):
            req = json.load(open(d["request_file"]))
        rel = os.path.relpath(f, batches)
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
                for a, b in pairs({r["id"] for r in rejs}):
                    edges["co-rejected"].append((a, b, {"at": where}))
        for a, b in pairs(turn_cites):
            edges["co-cited"].append((a, b, {"at": rel}))
        if turn_cites:
            sessions[req.get("session_id") or rel.split("/")[0]].append(
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


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--batches", default="/home/joe/code/storage/operator-turns/batches")
    ap.add_argument("--library", default="/home/joe/code/futon3/library")
    ap.add_argument("--out", default="/home/joe/code/storage/operator-turns/mined-pattern-graph.json")
    args = ap.parse_args()

    ids = library_ids(args.library)
    edges, records = mined_edges(args.batches, ids)
    edges["why"] = why_edges(ids)

    merged = {}
    for kind in KINDS:
        for a, b, ev in edges[kind]:
            key = (min(a, b), max(a, b), kind)
            e = merged.setdefault(key, {"a": key[0], "b": key[1], "kind": kind, "evidence": []})
            e["evidence"].append(ev)

    print(f"{records} analyses, {len(ids)} library patterns")
    acc = []
    summary = []
    for kind in KINDS:
        acc += edges[kind]
        comps = components(acc, list(ids))
        singles = sum(1 for c in comps if len(c) == 1)
        n = len({(min(a, b), max(a, b)) for a, b, _ in edges[kind]})
        summary.append({"through": kind, "edges": n, "giant": len(comps[0]),
                        "components": len(comps), "singletons": singles})
        print(f"+{kind:16} {n:5} edges  giant {len(comps[0]):4}/{len(ids)}"
              f"  components {len(comps):4}  singletons {singles}")

    strong = [e for k in ("co-cited", "rejected-beside") for e in edges[k]]
    giant = components(strong + edges["why"], list(ids))[0]
    with open(args.out, "w") as fh:
        json.dump({"records": records, "patterns": len(ids), "summary": summary,
                   "giant_without_weak_edges": sorted(giant),
                   "edges": sorted(merged.values(), key=lambda e: (e["kind"], e["a"], e["b"]))},
                  fh, indent=1)
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
