import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import pattern_giant_corroboration as census


class GiantCorroborationTest(unittest.TestCase):
    def test_statuses_and_non_circular_why(self):
        edges = [
            {"a": "a/x", "b": "b/y", "directive": "references"},
            {"a": "b/y", "b": "c/z", "directive": "why"},
            {"a": "c/z", "b": "d/q", "directive": "how"},
        ]
        evidence = census.use_index({"edges": [
            {"a": "a/x", "b": "b/y", "kind": "co-cited",
             "evidence": [{"at": "batch/turn-1.json.analysis.json"}]},
            # This authored mined edge is deliberately ignored: it cannot
            # corroborate the same authored @why relation.
            {"a": "b/y", "b": "c/z", "kind": "why",
             "evidence": [{"at": "batch/turn-circular.json.analysis.json"}]},
        ]})
        rows = census.classify_authored_edges(edges, {"a/x", "b/y", "c/z"}, evidence)
        self.assertEqual(["corroborated", "uncorroborated", "no-evidence"],
                         [row["status"] for row in rows])
        self.assertEqual({"co-cited": ["batch/turn-1"]}, rows[0]["evidence"])
        self.assertEqual({}, rows[1]["evidence"])

    def test_bridge_detaches_known_piece(self):
        graph = census.adjacency({"a/x", "b/y", "c/z", "d/q"}, [
            {"a": "a/x", "b": "b/y"}, {"a": "b/y", "b": "c/z"},
            {"a": "c/z", "b": "d/q"}, {"a": "d/q", "b": "c/z"},
        ])
        bridges = {(item["a"], item["b"]): item["detached-size"]
                   for item in census.bridge_details(graph)}
        self.assertEqual(1, bridges[("a/x", "b/y")])
        self.assertEqual(2, bridges[("b/y", "c/z")])


if __name__ == "__main__":
    unittest.main()
