#!/usr/bin/env python3
"""Inspect or manually stage an append-only pattern-graph diff."""
import argparse
import hashlib
import json
import os
import shutil
import sys

import mined_pattern_graph


DEFAULT_GRAPH = "/home/joe/code/storage/operator-turns/mined-pattern-graph.json"
DEFAULT_APPLIED = "/home/joe/code/storage/operator-turns/pattern-graph-diffs/applied"


def sha256(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(65536), b""):
            digest.update(block)
    return digest.hexdigest()


def inspect_diff(diff_path, graph_path):
    with open(diff_path) as handle:
        diff = json.load(handle)
    with open(graph_path) as handle:
        graph = json.load(handle)
    ids = set(graph.get("pattern_ids", []))
    source = diff.get("source", {})
    uses = []
    for use in diff.get("add_uses", []):
        known = use.get("pattern") in ids
        uses.append(dict(use, in_graph=known, disposition="add" if known else "would be skipped"))
    by_pair = {}
    for edge in graph.get("edges", []):
        key = tuple(sorted((edge["a"], edge["b"])))
        by_pair.setdefault(key, []).append(edge["kind"])
    links = []
    applicable = []
    for edge in diff.get("add_edges", []):
        key = tuple(sorted((edge.get("a"), edge.get("b"))))
        known = all(node in ids for node in key)
        links.append({"a": key[0], "b": key[1],
                      "existing_kinds": sorted(set(by_pair.get(key, []))),
                      "disposition": "add" if known else "would be skipped"})
        if known:
            applicable.append((key[0], key[1], edge.get("evidence", [])))
    existing = [(e["a"], e["b"], e.get("evidence", [])) for e in graph.get("edges", [])]
    nodes = list(ids)
    before = mined_pattern_graph.components(existing, nodes)[0] if nodes else []
    after = mined_pattern_graph.components(existing + applicable, nodes)[0] if nodes else []
    return {
        "run": source.get("run"), "target": source.get("target"),
        "run_outcome": source.get("run_outcome"), "enactment": source.get("enactment"),
        "want_accounting": source.get("want_accounting", {}),
        "base": {"diff_sha256": diff.get("base", {}).get("sha256"),
                 "current_sha256": sha256(graph_path),
                 "matches_current": diff.get("base", {}).get("sha256") == sha256(graph_path)},
        "uses": uses, "links": links,
        "largest_component": {"before": len(before), "after": len(after),
                              "newly_joined": sorted(set(after) - set(before))}}


def print_inspection(report):
    accounting = report["want_accounting"]
    print(f"run: {report['run']}")
    print(f"target: {report['target']}")
    print(f"outcome: {report['run_outcome']}")
    print(f"enactment: {report['enactment']}")
    print("want accounting: " + str(accounting.get("status")) +
          (f" ({accounting.get('reason')})" if accounting.get("reason") else ""))
    base = report["base"]
    print(f"base graph: {'matches current graph' if base['matches_current'] else 'DIFFERS from current graph (additive diff remains inspectable)'}")
    print("uses:")
    for use in report["uses"]:
        print(f"  {use['position']}. {use['pattern']} [{use['disposition']}]")
        for want in use.get("wants", []):
            print(f"     {want['want']}: {want['outcome']}")
    print("links:")
    for link in report["links"]:
        existing = ", ".join(link["existing_kinds"]) or "no link today"
        print(f"  {link['a']} -- {link['b']}: {existing} [{link['disposition']}]")
    component = report["largest_component"]
    joined = ", ".join(component["newly_joined"]) or "none"
    print(f"largest component: {component['before']} -> {component['after']}; newly joined: {joined}")


def apply_diff(diff_path, applied_dir):
    with open(diff_path) as handle:
        diff = json.load(handle)
    if diff.get("schema") != "pattern-graph-diff-v1":
        raise ValueError("refusing diff: schema is not pattern-graph-diff-v1")
    if diff.get("nothing_to_add"):
        raise ValueError("refusing diff: nothing_to_add=" + str(diff["nothing_to_add"]))
    run = diff.get("source", {}).get("run")
    if not run:
        raise ValueError("refusing diff: source.run is missing")
    os.makedirs(applied_dir, exist_ok=True)
    destination = os.path.join(applied_dir, run + ".json")
    if os.path.exists(destination):
        raise ValueError("refusing diff: applied file already exists for run " + run)
    with open(diff_path, "rb") as source, open(destination, "xb") as target:
        shutil.copyfileobj(source, target)
    return destination


def main(argv=None):
    parser = argparse.ArgumentParser()
    commands = parser.add_subparsers(dest="command", required=True)
    inspect_parser = commands.add_parser("inspect")
    inspect_parser.add_argument("diff")
    inspect_parser.add_argument("--graph", default=DEFAULT_GRAPH)
    inspect_parser.add_argument("--json", action="store_true")
    apply_parser = commands.add_parser("apply")
    apply_parser.add_argument("diff")
    apply_parser.add_argument("--applied-dir", default=DEFAULT_APPLIED)
    args = parser.parse_args(argv)
    try:
        if args.command == "inspect":
            report = inspect_diff(args.diff, args.graph)
            if args.json:
                print(json.dumps(report, indent=1, sort_keys=True))
            else:
                print_inspection(report)
        else:
            destination = apply_diff(args.diff, args.applied_dir)
            print(f"copied byte-for-byte to {destination}")
            print("next: python3 scripts/mined_pattern_graph.py")
        return 0
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(str(error), file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
