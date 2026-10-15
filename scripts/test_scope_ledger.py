import json
import sys
import tempfile
import unittest
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from scope_ledger import build, make_row, read_records


class ScopeLedgerTest(unittest.TestCase):
    def setUp(self):
        self.record = {"id": "turn-r1", "turn_id": "agent-turn-7", "agent_id": "claude-16",
                       "session_id": "sess", "created_at": "2026-10-15T19:26:00Z",
                       "reply_text": "㊢abc㊬xyz㊟", "ports": {"still_open": ["a", "b"], "closed_this_turn": ["c"]},
                       "standings": {"in_force": ["x"]}}
        self.scope = {"acts": [{"id": "agent-turn-7-f-s1-0", "kind": "ask-action", "side": "operator", "start": 0},
                                {"id": "agent-turn-7-f-s2-0", "kind": "propose", "side": "operator", "start": 1},
                                {"id": "agent-turn-7-r-0", "kind": "report", "side": "agent", "start": 0},
                                {"id": "agent-turn-7-r-1", "kind": "qualify", "side": "agent", "start": 4},
                                {"id": "old-turn-6-r-0", "kind": "report", "side": "agent", "start": 0}],
                      "edges": [{"act": "agent-turn-7-f-s2-0", "within": "agent-turn-7-f-s1-0", "rule": "R3"},
                                {"act": "agent-turn-7-r-0", "within": "agent-turn-7-f-s1-0", "rule": "R5"},
                                {"act": "agent-turn-7-r-1", "within": "old-turn-6-f-s1-0", "rule": "R4"},
                                {"act": "agent-turn-7-r-0", "within": "agent-turn-7-f-s1-0", "rule": "OPEN"}]}

    def test_line_and_ids(self):
        line, row, _, _ = make_row(self.record, self.scope)
        self.assertEqual("19:26 7  ⊢s1  s2↳s1 │ ㊢↳s1✓s1  ㊬↳t6:s1 │ open 2 (+3 −1) │ force 1", line)
        self.assertEqual("agent-turn-7-f-s1-0", row["operator"][0]["id"])
        self.assertEqual("agent-turn-7-f-s1-0", row["operator"][1]["parent"])

    def test_missing_route_and_previous_target(self):
        records = [self.record, dict(self.record, turn_id="agent-turn-6", created_at="2026-10-14T19:26:00Z")]
        rows, _ = build(records, "claude-16", "sess", opener=lambda _s, turn: None if turn == "agent-turn-6" else self.scope)
        self.assertTrue(rows[0][0].endswith("·"))
        self.assertIn("t6:s1", rows[1][0])
        self.assertIn("agent-turn-7-f-s2-0", rows[1][1]["operator"][1]["id"])

    def test_records_skip_drafts_and_non_objects(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "turn-a.json"
            p.write_text(json.dumps({"created_at": "1", "agent_id": "a"}))
            (Path(d) / "turn-a.json.draft.json").write_text("{}")
            (Path(d) / "turn-b.json").write_text("[]")
            self.assertEqual(1, len(read_records(d, agent="a")))


if __name__ == "__main__":
    unittest.main()
