"""Tests for ma_observe.py's card logic (no network, no model)."""
import os
import sys
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ma_observe as ma  # noqa: E402


class CardTest(unittest.TestCase):
    def test_parse_card_reads_tagged_json_and_fenced_json(self):
        raw = '<observations>x</observations>\n<agent-card>```json\n{"purpose": "p", "repair": ["r"], "extra": 1}\n```</agent-card>'
        card = ma.parse_card(raw)
        self.assertEqual(card["purpose"], "p")
        self.assertEqual(card["repair"], ["r"])
        self.assertNotIn("extra", card)
        self.assertIsNone(ma.parse_card("<agent-card>not json</agent-card>"))
        self.assertIsNone(ma.parse_card("no card"))

    def test_done_lines_take_the_marker_not_a_mention(self):
        obs = "* ✅ (1) a\n  * ✅ b\n* 🟡 (2) the ✅ marker means done\n> tiers: ✅ work"
        self.assertEqual(ma.done_lines(obs), ["✅ (1) a", "✅ b"])

    def test_held_counts_verbatim_carries(self):
        first = ma.held(["a", "b"], None)
        second = ma.held(["a", "c"], first)
        self.assertEqual(second, [{"text": "a", "held": 2}, {"text": "c", "held": 1}])

    def test_build_card_tiers(self):
        rows = [{"evidence/body": {"event": "observation", "observations": "* ✅ (1) old done\n* 🟡 x"}},
                {"evidence/body": {"event": "turn-commits", "commits": [{"sha": "abcdef1234", "subject": "s"}]}}]
        with mock.patch.object(ma, "xiang_patterns", return_value={"p/a": 3, "p/b": 1}), \
             mock.patch.object(ma, "clock_of", return_value={"excursion-id": "E-x"}), \
             mock.patch.object(ma, "mission_patterns", return_value=["m/p"]):
            card = ma.build_card("claude-3", "s", rows, "* ✅ (2) new done\n* 🔴 (2) broke",
                                 {"purpose": "p", "open_threads": ["t"], "repair": [], "ramifications": []},
                                 {"standing": {"purpose": {"text": "p", "held": 2}, "open_threads": [{"text": "t", "held": 4}]}})
        self.assertEqual(card["settled"]["engaged"], {"excursion-id": "E-x"})
        self.assertEqual(card["settled"]["done"], ["✅ (1) old done"])
        self.assertEqual(card["settled"]["commits"]["last"], ["abcdef123 s"])
        self.assertEqual(card["standing"]["purpose"], {"text": "p", "held": 3})
        self.assertEqual(card["standing"]["open_threads"], [{"text": "t", "held": 5}])
        self.assertEqual(card["standing"]["xiang_patterns"], [["p/a", 3]])
        self.assertEqual(card["fresh"]["done"], ["✅ (2) new done"])
        self.assertEqual(card["fresh"]["problems"], ["🔴 (2) broke"])

    def test_system_prompt_appends_card_instruction_in_mastras_slot(self):
        sp = ma.system_prompt()
        self.assertTrue(sp.startswith("You are the memory consciousness"))
        self.assertIn("=== CUSTOM INSTRUCTIONS ===", sp)
        self.assertIn("<agent-card>", sp)


if __name__ == "__main__":
    unittest.main()
