import unittest

from lean_goal_features import features, parse_goal


class LeanGoalFeaturesTest(unittest.TestCase):
    def test_parses_case_context_target_and_references(self):
        result = parse_goal("""case mp
U : Set Cube
f g : Cube
hf : f ∈ U
⊢ APMT94A01.rho f g < ε
""", ["APMT94A01.rho", "single_le"])
        self.assertEqual(result["case"], "mp")
        self.assertEqual(result["hypotheses"][1]["binders"], ["f", "g"])
        self.assertEqual(result["target"], "APMT94A01.rho f g < ε")
        self.assertEqual(result["local_declaration_refs"], ["APMT94A01.rho"])

    def test_feature_manifest_declares_blinding(self):
        capture = {"schema": "capture/v1", "source": "Main.lean",
                   "source_sha256": "abc", "reachability": {"reachable": []},
                   "goal_captures": [{"region_id": "r1", "line": 2,
                                      "goals": ["⊢ True"]}]}
        result = features(capture)
        self.assertIn("no dialogue", result["blinding"])
        self.assertEqual(result["regions"][0]["goals"][0]["target"], "True")


if __name__ == "__main__":
    unittest.main()
