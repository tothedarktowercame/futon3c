#!/usr/bin/env python3
"""Read-only corroboration census for authored pattern-library edges.

The mined graph is evidence only for use-derived relations.  Its authored
``why`` and ``how`` rows are deliberately excluded from corroboration so that
an authored relation cannot corroborate itself.
"""
import argparse
import collections
import datetime as dt
import hashlib
import json
import os
import pathlib
import re
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import mined_pattern_graph as mined  # noqa: E402

EXPECTED_SHA256 = "9992e2c62e41453d9ae33ba6c660b20195c91006759c84a3154af858436612e5"
DIRECTIVES = ("references", "see-also", "why", "how")
USE_KINDS = {"co-cited", "rejected-beside", "next-in-session", "used-together"}


def sha256(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def git_blame(path, library):
    """Return one ``line -> (sha, UTC-date)`` entry per source line."""
    result = subprocess.run(
        ["git", "-C", str(library.parent), "blame", "--line-porcelain", "--", str(path)],
        text=True, capture_output=True, check=False)
    if result.returncode:
        raise RuntimeError("git blame failed for %s: %s" % (path, result.stderr.strip()))
    answer, line, commit, timestamp = {}, 0, None, None
    for raw in result.stdout.splitlines():
        if re.match(r"^[0-9a-f]{40} ", raw):
            commit = raw.split()[0]
        elif raw.startswith("author-time "):
            timestamp = int(raw.split()[1])
        elif raw.startswith("\t"):
            line += 1
            answer[line] = (commit, dt.datetime.fromtimestamp(timestamp, dt.timezone.utc).date().isoformat())
    return answer


def directive_tokens(directive, body):
    if directive == "how":
        return mined.ID_IN_PROSE.findall(body)
    return [x for x in re.split(r"[\s,\[\]]+", body) if x]


def authored_edges(library):
    """Read authored lines, retaining only references that resolve to a file."""
    library = pathlib.Path(library)
    ids = mined.library_ids(str(library))
    edges, unresolved = [], collections.Counter()
    directive_re = re.compile(r"^\s*@(?P<directive>references|see-also|why|how)\b(?P<body>.*)$")
    for source, path in sorted(ids.items()):
        blame = git_blame(path, library)
        relative = pathlib.Path(path).relative_to(library).as_posix()
        pattern_set = relative.split("/", 1)[0]
        with open(path, encoding="utf-8", errors="replace") as handle:
            for lineno, line in enumerate(handle, 1):
                match = directive_re.match(line)
                if not match:
                    continue
                directive = match.group("directive")
                for target in directive_tokens(directive, match.group("body")):
                    if target in ids and target != source:
                        commit, date = blame[lineno]
                        edges.append({"a": source, "b": target, "directive": directive,
                                      "file": relative, "set": pattern_set, "line": lineno,
                                      "authored-sha": commit, "authored-date": date})
                    elif target != source:
                        unresolved[directive] += 1
    return ids, edges, unresolved


def adjacency(nodes, edges):
    graph = {n: set() for n in nodes}
    for edge in edges:
        a, b = edge["a"], edge["b"]
        graph.setdefault(a, set()).add(b)
        graph.setdefault(b, set()).add(a)
    return graph


def components(graph, skip_edge=None, skip_node=None):
    seen, groups = set(), []
    for node in sorted(graph):
        if node == skip_node or node in seen:
            continue
        todo, group = [node], set()
        while todo:
            current = todo.pop()
            if current in seen or current == skip_node:
                continue
            seen.add(current); group.add(current)
            for nxt in graph[current]:
                if skip_edge and frozenset((current, nxt)) == skip_edge:
                    continue
                if nxt not in seen and nxt != skip_node:
                    todo.append(nxt)
        groups.append(group)
    return sorted(groups, key=lambda g: (-len(g), sorted(g)))


def bridge_details(graph):
    base = len(components(graph))
    details = []
    for a in sorted(graph):
        for b in sorted(graph[a]):
            if a >= b:
                continue
            after = components(graph, skip_edge=frozenset((a, b)))
            if len(after) > base:
                details.append({"a": a, "b": b,
                                "detached-size": min(map(len, after))})
    return details


def articulation_points(graph):
    base = len(components(graph))
    return [node for node in sorted(graph)
            if len(components(graph, skip_node=node)) > base]


def turn_id(evidence):
    if evidence.get("run"):
        return str(evidence["run"])
    at = evidence.get("at", "")
    first = at.split(" -> ", 1)[0]
    return pathlib.Path(first).name.replace(".json.analysis.json", "") or at


def use_index(mined_graph):
    answer = collections.defaultdict(lambda: collections.defaultdict(set))
    for edge in mined_graph.get("edges", []):
        kind = edge.get("kind")
        if kind not in USE_KINDS:
            continue
        key = tuple(sorted((edge["a"], edge["b"])))
        for evidence in edge.get("evidence", []):
            answer[key][kind].add(turn_id(evidence))
    return answer


def classify_authored_edges(edges, cited, evidence):
    result = []
    for edge in edges:
        row = dict(edge)
        kinds = evidence.get(tuple(sorted((edge["a"], edge["b"]))), {})
        if edge["a"] not in cited or edge["b"] not in cited:
            row.update(status="no-evidence", evidence={})
        elif kinds:
            row.update(status="corroborated",
                       evidence={kind: sorted(ids) for kind, ids in sorted(kinds.items())})
        else:
            row.update(status="uncorroborated", evidence={})
        result.append(row)
    return result


def cited_patterns(path):
    with open(path, encoding="utf-8") as handle:
        return {json.loads(line)["pattern"] for line in handle if line.strip()}


def tsv(rows, path):
    columns = ["a", "b", "directive", "status", "file", "set", "line", "authored-sha",
               "authored-date", "evidence"]
    with open(path, "w", encoding="utf-8") as handle:
        handle.write("\t".join(columns) + "\n")
        for row in rows:
            values = [str(row.get(c, "")) if c != "evidence" else json.dumps(row[c], sort_keys=True)
                      for c in columns]
            handle.write("\t".join(values) + "\n")


def report(rows, all_nodes, output, metadata, unresolved, cited_count):
    all_graph = adjacency(all_nodes, rows)
    giant = components(all_graph)[0] if all_graph else set()
    giant_rows = [r for r in rows if r["a"] in giant and r["b"] in giant]
    corroborated_rows = [r for r in giant_rows if r["status"] == "corroborated"]
    corroborated_giant = components(adjacency(giant, corroborated_rows))[0] if giant else set()
    directives = {d: [r for r in giant_rows if r["directive"] == d] for d in DIRECTIVES}
    bridge_map = {(b["a"], b["b"]): b for b in bridge_details(adjacency(giant, giant_rows))}
    articulations = articulation_points(adjacency(giant, giant_rows))
    graph = adjacency(giant, giant_rows)
    hubs = sorted(graph, key=lambda n: (-len(graph[n]), n))[:20]
    bad_bridges = [r for r in giant_rows
                   if tuple(sorted((r["a"], r["b"]))) in bridge_map
                   and r["status"] != "corroborated"]
    by_directive = collections.Counter(r["directive"] for r in giant_rows)
    by_status_directive = collections.Counter((r["directive"], r["status"]) for r in giant_rows)
    component_count = len(components(all_graph))
    singleton_count = sum(len(c) == 1 for c in components(all_graph))
    giant_set_count = len({r["set"] for r in giant_rows})
    lines = ["# Authored giant-component corroboration", "",
             "| input sha256 | futon3 HEAD | script sha256 | run date |",
             "| --- | --- | --- | --- |",
             "| {input-sha256} | {futon3-head} | {script-sha256} | {run-date} |".format(**metadata), "",
             "The input SHA was verified before parsing. Only use-derived mined kinds "
             "(`co-cited`, `rejected-beside`, `next-in-session`, `used-together`) can corroborate; "
             "mined `why`/`how` edges are excluded to avoid circular evidence. The citation audit contains "
             "**%d** distinct cited patterns." % cited_count, "",
             "## (a) Authored-edge classification", "",
             "`edges.tsv` contains one row per authored edge whose endpoints lie in the authored giant, "
             "including directive, source file/set, authored line/sha/date, status, and use evidence turn ids.", "",
             "| directive | total | corroborated | uncorroborated | no-evidence |",
             "| --- | ---: | ---: | ---: | ---: |"]
    for directive in DIRECTIVES:
        lines.append("| %s | %d | %d | %d | %d |" % (directive, by_directive[directive],
            by_status_directive[directive, "corroborated"],
            by_status_directive[directive, "uncorroborated"],
            by_status_directive[directive, "no-evidence"]))
    lines += ["", "Unresolved prose/reference tokens were excluded: " +
              ", ".join("%s=%d" % (d, unresolved[d]) for d in DIRECTIVES) + ".", "",
              "## (b) Giant sizes", "",
              "| graph | giant size |", "| --- | ---: |",
              "| all authored directives | %d |" % len(giant),
              "| corroborated authored edges only (same authored giant node universe) | %d |" % len(corroborated_giant)]
    for directive in DIRECTIVES:
        local = components(adjacency(all_nodes, [r for r in rows if r["directive"] == directive]))[0]
        lines.append("| %s only | %d |" % (directive, len(local)))
    lines += ["", "## (c) Top 20 hubs", ""]
    for hub in hubs:
        hub_rows = [r for r in giant_rows if hub in (r["a"], r["b"])]
        weak_neighbors = {r["b"] if r["a"] == hub else r["a"]
                          for r in hub_rows if r["status"] != "corroborated"}
        lines += ["### `%s` — degree %d; uncorroborated/no-evidence degree %d" %
                  (hub, len(graph[hub]), len(weak_neighbors)), "",
                  "| edge | directive | status | set | authored date | authored sha |",
                  "| --- | --- | --- | --- | --- | --- |"]
        for r in sorted(hub_rows, key=lambda row: (row["a"], row["b"], row["directive"], row["line"])):
            lines.append("| `%s` — `%s` | %s | %s | %s | %s | `%s` |" %
                         (r["a"], r["b"], r["directive"], r["status"], r["set"],
                          r["authored-date"], r["authored-sha"]))
        lines.append("")
    lines += ["## (d) Bridges and articulation points", "",
              "Bridges: %d. Articulation points: %d." % (len(bridge_map), len(articulations)), "",
              "Articulation points: " + (", ".join("`%s`" % p for p in articulations) or "none") + ".", "",
              "### Editorial list: uncorroborated/no-evidence bridges", "",
              "| edge | directive | status | file | authored sha/date | detached piece |",
              "| --- | --- | --- | --- | --- | ---: |"]
    for r in sorted(bad_bridges, key=lambda row: (row["a"], row["b"], row["directive"], row["line"])):
        bridge = bridge_map[tuple(sorted((r["a"], r["b"]))) ]
        lines.append("| `%s` — `%s` | %s | %s | `%s` | `%s` / %s | %d |" %
                     (r["a"], r["b"], r["directive"], r["status"], r["file"],
                      r["authored-sha"], r["authored-date"], bridge["detached-size"]))
    lines += ["", "## Cross-check", "",
              "This run has %d library nodes (%+d against Claude-5), %d authored edge records, %d components "
              "(%+d), giant %d (%+d), %d singleton components (%+d), and %d sets in the giant (%+d). "
              "Claude-5's cited baseline is 1486 nodes, 277 components, giant 866 across 97 sets, and 256 "
              "isolated. Differences are reported rather than tuned: this census includes @references and "
              "@see-also in addition to @why/@how, while the pinned mined graph itself names 1431 ids; the "
              "current library's node universe is %d. Top-degree comparison is available in the hub "
              "table; this report does not assume its directives are identical to Claude-5's graph." %
              (len(all_nodes), len(all_nodes) - 1486, len(rows), component_count,
               component_count - 277, len(giant), len(giant) - 866,
               singleton_count, singleton_count - 256, giant_set_count, giant_set_count - 97,
               len(all_nodes)), "",
              "## Headline", "",
              "- Giant size under corroborated-only authored edges: **%d**" % len(corroborated_giant),
              "- Bridges in the authored giant: **%d**" % len(bridge_map),
              "- Uncorroborated/no-evidence bridges: **%d**" % len(bad_bridges), ""]
    pathlib.Path(output).write_text("\n".join(lines), encoding="utf-8")
    return len(giant), len(corroborated_giant), len(bridge_map), len(bad_bridges), giant_rows


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--library", default="/home/joe/code/futon3/library")
    parser.add_argument("--mined", default="/home/joe/code/storage/operator-turns/mined-pattern-graph.json")
    parser.add_argument("--citation-audit", default="/home/joe/code/storage/operator-turns/citation-audit.jsonl")
    parser.add_argument("--out", default="holes/labs/pattern-giant-corroboration")
    args = parser.parse_args()
    actual = sha256(args.mined)
    if actual != EXPECTED_SHA256:
        raise SystemExit("refusing unpinned mined graph: expected %s, got %s" % (EXPECTED_SHA256, actual))
    nodes, authored, unresolved = authored_edges(args.library)
    mined_graph = json.loads(pathlib.Path(args.mined).read_text(encoding="utf-8"))
    cited = cited_patterns(args.citation_audit)
    classified = classify_authored_edges(authored, cited, use_index(mined_graph))
    out = pathlib.Path(args.out); out.mkdir(parents=True, exist_ok=True)
    metadata = {"input-sha256": actual,
                "futon3-head": subprocess.check_output(["git", "-C", str(pathlib.Path(args.library).parent), "rev-parse", "HEAD"], text=True).strip(),
                "script-sha256": sha256(__file__),
                "run-date": dt.datetime.now(dt.timezone.utc).date().isoformat()}
    _, corroborated, bridges, weak_bridges, giant_rows = report(classified, nodes, out / "REPORT.md", metadata, unresolved, len(cited))
    tsv(giant_rows, out / "edges.tsv")
    print("corroborated_giant=%d bridges=%d uncorroborated_or_no_evidence_bridges=%d" %
          (corroborated, bridges, weak_bridges))


if __name__ == "__main__":
    main()
