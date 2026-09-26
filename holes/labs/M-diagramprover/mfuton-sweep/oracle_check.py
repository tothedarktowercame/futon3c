#!/usr/bin/env python3
"""Independent NetworkX, y0, and Boolean-world checks for the mfuton sweep."""

from __future__ import annotations

import itertools
import json
import sys
from importlib.metadata import version
from pathlib import Path

import networkx as nx
from y0.algorithm.identify import identify_outcomes
from y0.algorithm.simplify_latent import simplify_latent_dag
from y0.dsl import Variable
from y0.graph import NxMixedGraph


HERE = Path(__file__).resolve().parent
CONVERTED = HERE / "converted"


def read_json(path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def directed_graph(spec):
    graph = nx.DiGraph()
    graph.add_nodes_from(variable["id"] for variable in spec["variables"])
    graph.add_edges_from((edge["from"], edge["to"]) for edge in spec["arrows"])
    if not nx.is_directed_acyclic_graph(graph):
        raise AssertionError(f"converted graph is cyclic: {spec['id']}")
    return graph


def networkx_checks(graph, implications):
    verdicts = []
    for implication in implications:
        holds = nx.is_d_separator(
            graph,
            {implication["x"]},
            {implication["y"]},
            set(implication["given"]),
        )
        verdicts.append({**implication, "oracle-holds": holds})
    return verdicts


def y0_check(spec, pair):
    variables = {item["id"]: Variable(item["id"]) for item in spec["variables"]}
    graph = nx.DiGraph()
    for item in spec["variables"]:
        graph.add_node(
            variables[item["id"]],
            latent=item["kind"] == "latent-unobserved",
        )
    graph.add_edges_from(
        (variables[edge["from"]], variables[edge["to"]])
        for edge in spec["arrows"]
    )
    simplified = simplify_latent_dag(graph, tag="latent").graph
    admg = NxMixedGraph.from_latent_variable_dag(simplified, tag="latent")
    expression = identify_outcomes(
        admg,
        treatments=variables[pair["treatment"]],
        outcomes=variables[pair["outcome"]],
    )
    return {"identifiable?": expression is not None}


def equation_value(equation, world):
    tokens = equation.split()
    if len(tokens) == 1:
        return bool(world[tokens[0]])
    if len(tokens) == 2 and tokens[0] == "not":
        return not bool(world[tokens[1]])
    if len(tokens) == 3 and tokens[1] == "and":
        return bool(world[tokens[0]]) and bool(world[tokens[2]])
    if len(tokens) == 3 and tokens[1] == "or":
        return bool(world[tokens[0]]) or bool(world[tokens[2]])
    raise ValueError(f"unsupported converted equation: {equation}")


def evaluate_world(graph, equations, assignment, intervention=None):
    intervention = intervention or {}
    values = {}
    for node in nx.topological_sort(graph):
        if node in intervention:
            values[node] = bool(intervention[node])
        elif node in assignment:
            values[node] = bool(assignment[node])
        else:
            values[node] = equation_value(equations[node], values)
    return values


def counterfactual_check(spec, query):
    graph = directed_graph(spec)
    equations = spec["structural_equations"]
    exogenous = sorted(node for node in graph if graph.in_degree(node) == 0)
    assignments = [
        dict(zip(exogenous, values))
        for values in itertools.product((False, True), repeat=len(exogenous))
    ]
    consistent = [
        assignment
        for assignment in assignments
        if all(
            evaluate_world(graph, equations, assignment)[node] == value
            for node, value in query["evidence"].items()
        )
    ]
    if not consistent:
        return {"method": "refusal", "reason": "evidence-inconsistent"}
    outcomes = [
        evaluate_world(graph, equations, assignment, query["intervention"])[
            query["outcome"]
        ]
        for assignment in consistent
    ]
    if len(set(outcomes)) != 1:
        return {
            "method": "refusal",
            "reason": "counterfactual-underdetermined",
            "values": sorted(set(outcomes)),
        }
    return {"method": "deterministic-scm", "answer": outcomes[0]}


def main():
    engine = read_json(HERE / "engine-results.json")
    discrepancies = []
    nx_checked = 0
    nx_agreements = 0
    y0_checked = 0
    y0_agreements = 0
    cf_checked = 0
    cf_agreements = 0
    fixture_results = []

    for fixture in engine["fixtures"]:
        spec = read_json(CONVERTED / fixture["source-file"])
        graph = directed_graph(spec)
        nx_verdicts = networkx_checks(graph, fixture["implied-independencies"])
        nx_disagreements = []
        for verdict in nx_verdicts:
            nx_checked += 1
            if verdict["oracle-holds"]:
                nx_agreements += 1
            else:
                discrepancy = {
                    "fixture": fixture["example-id"],
                    "verdict-type": "d-separation",
                    "our-verdict": {
                        "x": verdict["x"],
                        "y": verdict["y"],
                        "given": verdict["given"],
                        "holds": True,
                    },
                    "oracle": "NetworkX",
                    "oracle-verdict": {"holds": False},
                    "rob-expectation": None,
                }
                discrepancies.append(discrepancy)
                nx_disagreements.append(discrepancy)

        y0 = None
        if fixture.get("pair"):
            y0_checked += 1
            y0 = y0_check(spec, fixture["pair"])
            expected = fixture["identification"]["identifiable?"]
            if y0["identifiable?"] == expected:
                y0_agreements += 1
            else:
                discrepancies.append(
                    {
                        "fixture": fixture["example-id"],
                        "verdict-type": "identification",
                        "pair": fixture["pair"],
                        "our-verdict": fixture["identification"],
                        "oracle": "y0",
                        "oracle-verdict": y0,
                        "rob-expectation": None,
                    }
                )

        cf = None
        if fixture.get("counterfactual"):
            cf_checked += 1
            cf_record = fixture["counterfactual"]
            cf = counterfactual_check(spec, cf_record["query"])
            engine_cf = cf_record["engine"]
            agrees = (
                cf["method"] == engine_cf["method"]
                and cf.get("answer") == engine_cf.get("answer")
                and cf.get("reason") == engine_cf.get("reason")
            )
            rob_agrees = engine_cf.get("answer") == cf_record["rob-expected-value"]
            if agrees and rob_agrees:
                cf_agreements += 1
            else:
                discrepancies.append(
                    {
                        "fixture": fixture["example-id"],
                        "verdict-type": "counterfactual",
                        "our-verdict": engine_cf,
                        "oracle": "independent exhaustive Boolean worlds",
                        "oracle-verdict": cf,
                        "rob-expectation": cf_record["query"]["rob-expectation"],
                        "rob-expected-value": cf_record["rob-expected-value"],
                    }
                )

        fixture_results.append(
            {
                "example-id": fixture["example-id"],
                "networkx-checked": len(nx_verdicts),
                "networkx-disagreements": nx_disagreements,
                "y0": y0,
                "counterfactual-oracle": cf,
            }
        )

    result = {
        "tool-versions": {
            "python": sys.version.split()[0],
            "networkx": nx.__version__,
            "y0": version("y0"),
        },
        "networkx": {"checked": nx_checked, "agreements": nx_agreements},
        "y0": {"checked": y0_checked, "agreements": y0_agreements},
        "counterfactual-enumeration": {
            "checked": cf_checked,
            "agreements": cf_agreements,
        },
        "fixtures": fixture_results,
        "discrepancies": discrepancies,
    }
    with (HERE / "python-results.json").open("w", encoding="utf-8") as stream:
        json.dump(result, stream, indent=2, sort_keys=True)
        stream.write("\n")
    print(
        f"mfuton NetworkX {nx_agreements}/{nx_checked}; "
        f"y0 {y0_agreements}/{y0_checked}; "
        f"Boolean worlds {cf_agreements}/{cf_checked}; "
        f"discrepancies {len(discrepancies)}"
    )


if __name__ == "__main__":
    main()
