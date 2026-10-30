import json
import tempfile
import unittest
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import xiang_platform_replay as replay


class XiangPlatformReplayTest(unittest.TestCase):
    def test_loads_only_linked_task_case(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "seat.jsonl"
            rows = [
                {"type": "user", "uuid": "u1", "timestamp": "t",
                 "message": {"content": [{"type": "text", "text": "A reranking task. X"}]}},
                {"type": "assistant", "parentUuid": "u1",
                 "message": {"content": [{"type": "text", "text": "[]"}]}},
                {"type": "user", "uuid": "u2",
                 "message": {"content": [{"type": "text", "text": "unrelated"}]}},
            ]
            path.write_text("".join(json.dumps(row) + "\n" for row in rows))
            cases = replay.load_frozen_cases(path, "lu")
            self.assertEqual(1, len(cases))
            self.assertEqual("[]", cases[0]["baseline_reply"])

    def test_bad_unlinked_case_is_not_admitted(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "seat.jsonl"
            path.write_text(json.dumps({
                "type": "user", "uuid": "u1",
                "message": {"content": [{"type": "text", "text": "馬 alignment reading: X"}]},
            }) + "\n")
            self.assertEqual([], replay.load_frozen_cases(path, "ma"))

    def test_comparisons_are_task_specific(self):
        ma_a = '[{"act":"a","verdict":"answered","by":"r","quote":"yes indeed here"}]'
        ma_b = '[{"act":"a","verdict":"answered","by":"r","quote":"yes indeed here"}]'
        self.assertEqual(1.0, replay.comparison(ma_a, ma_b, "ma")["exact_agreement"])
        lu_a = '[{"pattern":"a"},{"pattern":"b"},{"pattern":"c"}]'
        lu_b = '[{"pattern":"b"},{"pattern":"d"},{"pattern":"a"}]'
        self.assertEqual(2, replay.comparison(lu_a, lu_b, "lu")["top3_overlap"])

    def test_grounding_constructs_the_exact_bad_case_the_check_names(self):
        prompt = ("Operator turn:\nthis quote has five exact words here\n\n"
                  "--- pattern p\nIF: x")
        good = '[{"pattern":"p","quote":"this quote has five exact words"}]'
        bad = '[{"pattern":"p","quote":"this quote has five invented words"}]'
        self.assertTrue(replay.grounding(prompt, good, "lu")["fully_grounded"])
        self.assertFalse(replay.grounding(prompt, bad, "lu")["fully_grounded"])

    def test_credential_permissions_are_enforced(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "key"
            path.write_text("secret\n")
            path.chmod(0o644)
            with self.assertRaisesRegex(ValueError, "group/world"):
                replay.read_key(path)

    def test_cost_bound_is_not_lower_than_max_output_charge(self):
        bound = replay.conservative_bound("claude-sonnet-5-5", "x", 2000)
        self.assertGreaterEqual(bound, 0.02)


if __name__ == "__main__":
    unittest.main()
