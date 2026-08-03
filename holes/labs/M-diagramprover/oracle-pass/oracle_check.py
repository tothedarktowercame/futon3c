#!/usr/bin/env python3
"""Independent NetworkX and y0 checks over the durable engine export."""

from __future__ import annotations

import json
import sys
from importlib.metadata import version
from pathlib import Path

import dowhy
import networkx as nx
import pandas as pd
from y0.algorithm.identify import identify_outcomes
from y0.dsl import Variable
from y0.graph import NxMixedGraph


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]


def read_json(path: Path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def graph_from_export(data):
    graph = nx.DiGraph()
    graph.add_nodes_from(data["variables"])
    graph.add_edges_from((edge["from"], edge["to"]) for edge in data["arrows"])
    if not nx.is_directed_acyclic_graph(graph):
        raise AssertionError("exported graph is not acyclic")
    return graph


def separated(graph, x, y, given=()):
    return nx.is_d_separator(graph, {x}, {y}, set(given))


def implication_check(graph, implications):
    verdicts = []
    for ci in implications:
        holds = separated(graph, ci["x"], ci["y"], ci["given"])
        verdicts.append({**ci, "holds": holds})
    disagreements = [ci for ci in verdicts if not ci["holds"]]
    return {
        "checked": len(verdicts),
        "agreements": len(verdicts) - len(disagreements),
        "disagreements": disagreements,
    }


def bow_checks(export):
    """Independent NetworkX verdicts for every fixture d-sep claim."""
    graphs = {name: graph_from_export(data) for name, data in export["bow-graphs"].items()}
    queries = [
        ("simpson-marginal", "simpson", "treatment", "recovery", [], False, True),
        ("simpson-adjusted", "simpson", "treatment", "recovery", ["severity"], True, True),
        ("sprinkler-marginal", "sprinkler", "rain", "sprinkler", [], True, False),
        ("sprinkler-conditioned", "sprinkler", "rain", "sprinkler", ["wet-grass"], False, False),
        ("monty-marginal", "monty", "choice", "prize", [], True, False),
        ("monty-conditioned", "monty", "choice", "prize", ["host-opens"], False, False),
        ("firing-rung2", "firing-squad", "soldier-A", "death", [], False, False),
    ]
    verdicts = []
    for claim, fixture, x, y, given, expected, cut_outgoing in queries:
        graph = graphs[fixture].copy()
        if cut_outgoing:
            graph.remove_edges_from(list(graph.out_edges(x)))
        actual = separated(graph, x, y, given)
        verdicts.append({"claim": claim, "separated": actual, "expected": expected})
    disagreements = [item for item in verdicts if item["separated"] != item["expected"]]
    return {"checked": len(verdicts), "agreements": len(verdicts) - len(disagreements),
            "disagreements": disagreements, "verdicts": verdicts}


def bow_frontdoor_y0():
    """Latent projection: U becomes the smoking<->cancer bidirected edge."""
    smoking, tar, cancer = map(Variable, ["smoking", "tar", "cancer"])
    graph = NxMixedGraph.from_edges(
        nodes=[smoking, tar, cancer],
        directed=[(smoking, tar), (tar, cancer)],
        undirected=[(smoking, cancer)],
    )
    expression = identify_outcomes(graph, treatments=smoking, outcomes=cancer)
    return {
        "identifiable": expression is not None,
        "query": "P(cancer | do(smoking))",
        "encoding": "U latent-projected as smoking <-> cancer; smoking -> tar -> cancer",
        "frontier-marker": "engine refuses observed-only backdoor; y0 ID succeeds",
    }


def receipt(export, receipt_id):
    return next(item for item in export["receipts"] if item["id"] == receipt_id)


def y0_identification(data, treatment, outcome, conditions=None):
    nodes = {name: Variable(name) for name in data["variables"]}
    graph = NxMixedGraph.from_edges(
        nodes=nodes.values(),
        directed=[(nodes[e["from"]], nodes[e["to"]]) for e in data["arrows"]],
    )
    expression = identify_outcomes(
        graph,
        treatments=nodes[treatment],
        outcomes=nodes[outcome],
        conditions={nodes[name] for name in conditions} if conditions else None,
    )
    return {
        "identifiable": expression is not None,
        # y0's expression printer iterates internal sets in hash order. Keep
        # the reproducible verdict and query, rather than serializing an
        # algebraically equivalent but byte-unstable rendering.
        "query": {
            "treatment": treatment,
            "outcome": outcome,
        },
        "encoding": "all exported variables observed; directed arcs only; no latent/bidirected arcs",
        "conditions": sorted(conditions or []),
    }


def main():
    export = read_json(HERE / "engine-export.json")
    # Read-only source specs are loaded to pin provenance and counts; graph
    # verdicts deliberately use only the exported arrow lists.
    memory_source = read_json(ROOT / "docs/memory-causal-graph-spec.json")
    lean_source = read_json(ROOT / "docs/lean-proof-pipeline-causal-spec.json")
    memory = graph_from_export(export["memory-graph"])
    lean = graph_from_export(export["lean-graph"])
    memory_implications = implication_check(memory, export["implied-independencies"])
    lean_implications = implication_check(lean, export["lean-implied-independencies"])

    q3_results = {}
    for name, data in export["q3-variants"].items():
        graph = graph_from_export(data)
        q3_results[name] = {
            "marginal-separated": separated(graph, "M-in-store", "V12-minus-M"),
            "v18-separated-given-v12-minus-m": separated(
                graph, "M-in-store", "V18", ["V12-minus-M"]
            ),
        }

    q3_expected = {
        "star-forest": {
            "marginal-separated": True,
            "v18-separated-given-v12-minus-m": True,
        },
        "populated-graph": {
            "marginal-separated": False,
            "v18-separated-given-v12-minus-m": True,
        },
    }
    q3_disagreements = [
        {"graph": name, "expected": q3_expected[name], "actual": actual}
        for name, actual in q3_results.items()
        if actual != q3_expected[name]
    ]

    r2_receipt = receipt(export, "R2")
    r2_expected = {
        verdict["graph"]: verdict["holds?"] for verdict in r2_receipt["verdicts"]
    }
    r2_expected["content-removal-effect"] = r2_receipt["duplication-debt"][
        "content-removal-effect?"
    ]
    copied = graph_from_export(export["r2-variants"]["copied-class"])
    extracted = graph_from_export(export["r2-variants"]["extracted-class"])
    r2_actual = {
        "copied-class": not separated(copied, "P19", "P16"),
        "extracted-class": not separated(extracted, "P19", "P16"),
        "content-removal-effect": not separated(copied, "remove-content", "P16"),
    }
    r2_disagreements = [
        {"claim": name, "expected": expected, "actual": r2_actual[name]}
        for name, expected in r2_expected.items()
        if r2_actual[name] != expected
    ]

    r3_receipt = receipt(export, "R3")
    r3_expected = {
        verdict["graph"]: verdict["holds?"] for verdict in r3_receipt["verdicts"]
    }
    current = graph_from_export(export["r3-variants"]["current-sensors"])
    hypothetical = graph_from_export(
        export["r3-variants"]["with-hypothetical-t05"]
    )
    r3_actual = {
        "current-sensors": separated(
            current, "P16-at-k+1", "P10-at-k", ["T04-at-k"]
        ),
        "with-hypothetical-t05": separated(
            hypothetical, "P16-at-k+1", "T04-at-k", ["T05-at-k"]
        ),
    }
    r3_disagreements = [
        {"claim": name, "expected": expected, "actual": r3_actual[name]}
        for name, expected in r3_expected.items()
        if r3_actual[name] != expected
    ]

    q1_id = y0_identification(export["memory-graph"], "V06", "V18")
    # P01 and P10-pre are observed adjustment variables in the exported R1
    # selection graph. ID checks the total effect in that fully observed DAG;
    # IDC additionally verifies its conditional observational query.
    r1_id = y0_identification(export["r1-selection"], "P20", "P16")
    r1_idc = y0_identification(
        export["r1-selection"], "P20", "P16", ["P01", "P10-pre"]
    )
    bow = bow_checks(export)
    bow_y0 = bow_frontdoor_y0()

    result = {
        "tool-versions": {
            "python": sys.version.split()[0],
            "networkx": nx.__version__,
            "y0": version("y0"),
            "dowhy": dowhy.__version__,
            "pandas": pd.__version__,
        },
        "source-spec-counts": {
            "memory-variables": len(memory_source["variables"]),
            "memory-arrows": len(memory_source["arrows"]),
            "lean-variables": len(lean_source["variables"]),
            "lean-arrows": len(lean_source["arrows"]),
        },
        "networkx-implications": memory_implications,
        "networkx-lean-implications": lean_implications,
        "q3": {"verdicts": q3_results, "disagreements": q3_disagreements},
        "r2": {"verdicts": r2_actual, "disagreements": r2_disagreements},
        "r3": {"verdicts": r3_actual, "disagreements": r3_disagreements},
        "identification": {"Q1": q1_id, "R1-ID": r1_id, "R1-IDC": r1_idc},
        "bow": {"networkx": bow, "frontdoor-y0": bow_y0},
    }
    with (HERE / "python-results.json").open("w", encoding="utf-8") as stream:
        json.dump(result, stream, indent=2, sort_keys=True)
        stream.write("\n")

    structural_disagreements = (
        memory_implications["disagreements"]
        + lean_implications["disagreements"]
        + q3_disagreements
        + r2_disagreements
        + r3_disagreements
        + bow["disagreements"]
    )
    if structural_disagreements:
        raise SystemExit("oracle disagreement; inspect python-results.json")
    if not all(item["identifiable"] for item in result["identification"].values()):
        raise SystemExit("y0 identification failure; inspect python-results.json")
    if not bow_y0["identifiable"]:
        raise SystemExit("y0 did not identify the front-door fixture")
    print(
        "NetworkX memory/Lean implications: "
        f"{memory_implications['agreements']}/{lean_implications['agreements']} "
        "agreements, 0 disagreements"
    )
    print("Q3: 4/4 verdicts agree")
    print("R2/R3: 3/3 and 2/2 verdicts agree")
    print("y0: Q1 ID, R1 ID, and R1 conditional IDC identifiable")
    print(f"Book-of-Why NetworkX: {bow['agreements']}/{bow['checked']} agreements")
    print("Book-of-Why y0: front-door identifiable (deliberate frontier marker)")


if __name__ == "__main__":
    main()
