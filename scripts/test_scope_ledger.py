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

    def test_multi_fragment_sentences_commits_and_missing_ports(self):
        """Two fragments of one sentence read s1.0 and s1.1; commit acts fold
        into ⎘n; a record whose ports are not yet written shows · not 0."""
        scope = {"acts": [{"id": "agent-turn-9-f-s1-0", "kind": "approve", "side": "operator", "start": 0},
                          {"id": "agent-turn-9-f-s1-1", "kind": "clarify", "side": "operator", "start": 20},
                          {"id": "agent-turn-9-r-0", "kind": "report", "side": "agent", "start": 0},
                          {"id": "agent-turn-9-c-abc123", "kind": "commit", "side": "agent"},
                          {"id": "agent-turn-9-c-def456", "kind": "commit", "side": "agent"}],
                 "edges": [{"act": "agent-turn-9-f-s1-1", "within": "agent-turn-9-f-s1-0", "rule": "R3"},
                           {"act": "agent-turn-9-r-0", "within": "agent-turn-9-f-s1-1", "rule": "R3"}]}
        record = {"turn_id": "agent-turn-9", "created_at": "2026-10-15T19:47:00Z",
                  "reply_text": "㊢ (x) done"}
        line, row, _, _ = make_row(record, scope, previous_open=5)
        self.assertEqual("19:47 9  ⊢s1.0  s1.1↳s1.0 │ ㊢↳s1.1  ⎘2 │ open · (+· −0) │ force ·", line)
        self.assertEqual(2, len(row["replies"]) + 1)  # the two commits are not rows

    def test_rows_are_built_oldest_first_so_the_delta_has_a_predecessor(self):
        older = dict(self.record, turn_id="agent-turn-6", created_at="2026-10-14T19:00:00Z",
                     ports={"still_open": [{"act": "a"}, {"act": "b"}], "closed_this_turn": []})
        newer = dict(self.record, ports={"still_open": [{"act": "a"}, {"act": "b"}, {"act": "c"}],
                                         "closed_this_turn": [{"act": "d"}]})
        rows, _ = build([older, newer], "claude-16", "sess", opener=lambda _s, _t: self.scope)
        self.assertEqual("agent-turn-7", rows[0][1]["turn_id"])        # newest first in the output
        self.assertIn("open 3 (+2 −1)", rows[0][0])                    # 3 now, 2 before, 1 closed: 2 opened
        self.assertIn("open 2 (+· −0)", rows[1][0])                    # the oldest has no predecessor

    def test_cached_scopes_are_reused_except_for_changed_or_newest_records(self):
        """A second build asks the route only for the newest record and for
        records whose fingerprint changed; the cached rows read the same."""
        older = dict(self.record, turn_id="agent-turn-6", created_at="2026-10-14T19:00:00Z")
        middle = dict(self.record)
        newest = dict(self.record, turn_id="agent-turn-8", created_at="2026-10-15T20:00:00Z")
        calls, cache = [], {}

        def opener(_session, turn):
            calls.append(turn)
            return self.scope

        first, _ = build([older, middle, newest], "claude-16", "sess", opener=opener, cache=cache)
        self.assertEqual(["agent-turn-6", "agent-turn-7", "agent-turn-8"], calls)
        self.assertEqual({"agent-turn-6", "agent-turn-7", "agent-turn-8"}, set(cache))
        calls.clear()
        second, _ = build([older, middle, newest], "claude-16", "sess", opener=opener, cache=cache)
        self.assertEqual(["agent-turn-8"], calls)
        self.assertEqual([line for line, _ in first], [line for line, _ in second])
        calls.clear()
        changed = dict(middle, ports={"still_open": ["a"], "closed_this_turn": []})
        rows, _ = build([older, changed, newest], "claude-16", "sess", opener=opener, cache=cache)
        self.assertEqual(["agent-turn-7", "agent-turn-8"], calls)
        self.assertIn("open 1", rows[1][0])
        # a turn the route did not answer is not cached, so it is asked again
        calls.clear()
        build([older, dict(self.record, turn_id="agent-turn-9")], "claude-16", "sess",
              opener=lambda _s, turn: (calls.append(turn), None)[1] if turn == "agent-turn-9" else self.scope,
              cache=cache)
        self.assertEqual(["agent-turn-9"], calls)
        self.assertNotIn("agent-turn-9", cache)

    def test_records_skip_drafts_and_non_objects(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "turn-a.json"
            p.write_text(json.dumps({"created_at": "1", "agent_id": "a"}))
            (Path(d) / "turn-a.json.draft.json").write_text("{}")
            (Path(d) / "turn-b.json").write_text("[]")
            self.assertEqual(1, len(read_records(d, agent="a")))


if __name__ == "__main__":
    unittest.main()
