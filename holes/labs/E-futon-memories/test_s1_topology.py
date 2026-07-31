import importlib.util
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("s1_topology", HERE / "s1_topology.py")
s1 = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(s1)


class RelationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.commits = s1.load_commits(HERE.parents[2])
        cls.edges, cls.controls = s1.derive(cls.commits)

    def test_pin_and_count(self):
        self.assertEqual(1828, len(self.commits))
        self.assertEqual(s1.PIN, self.commits[-1]["sha"])

    def test_cochange_positive_control(self):
        control = self.controls["cochange"]
        self.assertIsNotNone(control)
        self.assertGreaterEqual(control["count"], s1.COCHANGE_MIN_COUNT)
        self.assertGreaterEqual(control["jaccard"], s1.COCHANGE_MIN_JACCARD)

    def test_aliases_cannot_affect_non_author_relations(self):
        altered = [dict(c, author=("Joseph Corneli" if c["author"] == "Joe Corneli"
                                   else "Joe Corneli" if c["author"] == "Joseph Corneli"
                                   else c["author"])) for c in self.commits]
        edges2, _ = s1.derive(altered)
        self.assertEqual(self.edges, edges2)

    def test_no_clique_expansion(self):
        edge = next(e for e in self.edges
                    if e["kind"] == "same-file" and e["key"] == "dev/futon3c/dev.clj")
        self.assertEqual(162, len(edge["vertices"]))
        self.assertEqual(1, sum(e["kind"] == "same-file" and
                                e["key"] == "dev/futon3c/dev.clj" for e in self.edges))


if __name__ == "__main__":
    unittest.main()
