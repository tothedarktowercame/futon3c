#!/usr/bin/env python3
import contextlib
import io
import json
import os
import random
import tempfile
import unittest

import pattern_retraction as subject


def edge(a, b, kind, evidence=None):
    return {"a": min(a, b), "b": max(a, b), "kind": kind,
            "evidence": evidence or [{"at": "fixture"}]}


class PatternRetractionTest(unittest.TestCase):
    def graph(self, edges):
        return {"records": 7, "patterns": len({n for e in edges for n in (e["a"], e["b"])}),
                "edges": edges}

    def test_authored_path_precedes_weak_shortcut(self):
        graph = self.graph([
            edge("p/a", "p/m", "why", [{"file": "/library/p/a.flexiarg"}]),
            edge("p/m", "p/b", "how", [{"file": "/library/p/m.flexiarg"}]),
            edge("p/a", "p/b", "co-rejected"),
        ])
        result = subject.retractions(graph, ["p/a", "p/b"], 2,
                                     dict(subject.DEFAULT_WEIGHTS))
        self.assertEqual(["p/a", "p/b", "p/m"], result["retractions"][0]["nodes"])
        self.assertEqual(2, result["retractions"][0]["cost"])
        self.assertEqual(0, result["retractions"][0]["weak_edges"])
        self.assertEqual(8, result["retractions"][1]["cost"])
        self.assertGreater(result["retractions"][1]["weak_edges"], 0)
        self.assertEqual({"from": "p/a", "to": "p/m"},
                         result["retractions"][0]["edges"][0]["direction"])

    def test_k_results_have_distinct_node_sets(self):
        graph = self.graph([
            edge("p/a", "p/x", "why"), edge("p/x", "p/b", "why"),
            edge("p/a", "p/y", "co-cited"), edge("p/y", "p/b", "co-cited"),
            edge("p/a", "p/z", "rejected-beside"),
            edge("p/z", "p/b", "rejected-beside"),
        ])
        rows = subject.retractions(graph, ["p/a", "p/b"], 3,
                                   dict(subject.DEFAULT_WEIGHTS))["retractions"]
        self.assertEqual(3, len(rows))
        self.assertEqual(3, len({tuple(row["nodes"]) for row in rows}))

    def test_three_seeds_are_connected(self):
        graph = self.graph([edge("p/a", "p/x", "why"),
                            edge("p/b", "p/x", "why"),
                            edge("p/c", "p/x", "why")])
        row = subject.retractions(graph, ["p/a", "p/b", "p/c"], 1,
                                  dict(subject.DEFAULT_WEIGHTS))["retractions"][0]
        self.assertTrue({"p/a", "p/b", "p/c"}.issubset(row["nodes"]))
        self.assertEqual(3, len(row["edges"]))

    def test_disconnected_seeds(self):
        graph = self.graph([edge("p/a", "p/x", "why"),
                            edge("p/b", "p/y", "why")])
        result = subject.retractions(graph, ["p/a", "p/b"], 3,
                                     dict(subject.DEFAULT_WEIGHTS))
        self.assertFalse(result["component"]["seeds_connected"])
        self.assertEqual([], result["retractions"])
        self.assertEqual(2, len(result["component"]["seed_components"]))

    def test_unknown_seed_is_a_nonzero_cli_error(self):
        graph = self.graph([edge("p/a", "p/b", "why")])
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "graph.json")
            with open(path, "w") as handle:
                json.dump(graph, handle)
            stderr = io.StringIO()
            with contextlib.redirect_stderr(stderr):
                status = subject.main(["--graph", path, "--seeds", "p/a,p/missing"])
        self.assertNotEqual(0, status)
        self.assertIn("p/missing", stderr.getvalue())

    def test_output_is_identical_when_edges_are_shuffled(self):
        edges = [edge("p/a", "p/x", "why"), edge("p/x", "p/b", "how"),
                 edge("p/a", "p/b", "co-rejected")]
        shuffled = list(edges)
        random.Random(19).shuffle(shuffled)
        one = subject.retractions(self.graph(edges), ["p/a", "p/b"], 3,
                                  dict(subject.DEFAULT_WEIGHTS))
        two = subject.retractions(self.graph(shuffled), ["p/a", "p/b"], 3,
                                  dict(subject.DEFAULT_WEIGHTS))
        self.assertEqual(json.dumps(one, sort_keys=True), json.dumps(two, sort_keys=True))

    def test_ranks_are_in_cost_order_when_the_first_tree_is_not_cheapest(self):
        # 14-node subgraph of the live graph (2026-09-30, ids renamed) on which
        # the first approximate tree costs 18 and two alternatives cost 16.
        fixture = [
            ["p/n00", "p/n02", "co-cited"],
            ["p/n01", "p/n03", "co-cited"],
            ["p/n01", "p/n04", "co-cited"],
            ["p/n01", "p/n08", "co-cited"],
            ["p/n02", "p/n03", "co-cited"],
            ["p/n02", "p/n04", "co-cited"],
            ["p/n03", "p/n04", "co-cited"],
            ["p/n03", "p/n06", "co-cited"],
            ["p/n04", "p/n08", "co-cited"],
            ["p/n04", "p/n09", "co-cited"],
            ["p/n00", "p/n09", "co-rejected"],
            ["p/n01", "p/n08", "co-rejected"],
            ["p/n01", "p/n09", "co-rejected"],
            ["p/n08", "p/n09", "co-rejected"],
            ["p/n10", "p/n12", "how"],
            ["p/n10", "p/n13", "how"],
            ["p/n11", "p/n13", "how"],
            ["p/n12", "p/n13", "how"],
            ["p/n00", "p/n01", "next-in-session"],
            ["p/n00", "p/n04", "next-in-session"],
            ["p/n01", "p/n03", "next-in-session"],
            ["p/n01", "p/n04", "next-in-session"],
            ["p/n01", "p/n08", "next-in-session"],
            ["p/n04", "p/n06", "next-in-session"],
            ["p/n04", "p/n07", "next-in-session"],
            ["p/n04", "p/n08", "next-in-session"],
            ["p/n06", "p/n09", "next-in-session"],
            ["p/n00", "p/n01", "rejected-beside"],
            ["p/n01", "p/n08", "rejected-beside"],
            ["p/n05", "p/n06", "rejected-beside"],
            ["p/n07", "p/n08", "why"],
            ["p/n07", "p/n12", "why"],
            ["p/n09", "p/n12", "why"],
            ["p/n10", "p/n12", "why"],
        ]
        graph = self.graph([edge(a, b, kind) for a, b, kind in fixture])
        rows = subject.retractions(graph, ["p/n00", "p/n05", "p/n11"], 3,
                                   dict(subject.DEFAULT_WEIGHTS))["retractions"]
        self.assertEqual([16, 16, 18], [row["cost"] for row in rows])
        self.assertEqual([1, 2, 3], [row["rank"] for row in rows])

    def test_weight_override_changes_ranking(self):
        graph = self.graph([edge("p/a", "p/x", "why"),
                            edge("p/x", "p/b", "why"),
                            edge("p/a", "p/b", "co-rejected")])
        defaults = subject.retractions(graph, ["p/a", "p/b"], 2,
                                       dict(subject.DEFAULT_WEIGHTS))
        weights = dict(subject.DEFAULT_WEIGHTS)
        weights["co-rejected"] = 1
        changed = subject.retractions(graph, ["p/a", "p/b"], 2, weights)
        self.assertEqual(["p/a", "p/b", "p/x"], defaults["retractions"][0]["nodes"])
        self.assertEqual(["p/a", "p/b"], changed["retractions"][0]["nodes"])


if __name__ == "__main__":
    unittest.main()
