#!/usr/bin/env python3
"""Tests for memory_use_rate.tabulate: the bad cases the columns are named for."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import memory_use_rate as mur  # noqa: E402


def start(agent, turn, prompt, at):
    return {"evidence/author": agent, "evidence/at": at,
            "evidence/body": {"event": "turn-start", "turn-id": turn, "prompt": prompt,
                              "dispatch-id": "invoke-" + turn}}


def rnd(agent, turn, calls, at, n=1):
    return {"evidence/author": agent, "evidence/at": at,
            "evidence/body": {"event": "turn-round", "turn-id": turn, "round": n,
                              "calls": [{"tool": c, "args": ""} for c in calls]}}


class Tabulate(unittest.TestCase):
    def test_requisitioned_and_unprompted_are_told_apart(self):
        starts = [start("zai-1", "t1", "Please run m40_find for the rule", "2026-10-16T03:40:00Z"),
                  start("zai-1", "t2", "Review this commit's test claim", "2026-10-16T04:00:00Z"),
                  start("zai-1", "t3", "Review another commit", "2026-10-16T05:00:00Z")]
        rounds = [rnd("zai-1", "t1", ["m40_find", "Read"], "2026-10-16T03:41:00Z"),
                  rnd("zai-1", "t2", ["m40_state"], "2026-10-16T04:01:00Z"),
                  rnd("zai-1", "t3", ["Bash"], "2026-10-16T05:01:00Z"),
                  rnd("zai-1", "t3", [], "2026-10-16T05:02:00Z", n=2)]
        rows, turns = mur.tabulate(starts, rounds)
        self.assertEqual(len(rows), 1)
        r = rows[0]
        self.assertEqual((r["turns"], r["with_tools"], r["using_m40"], r["m40_calls"]), (3, 3, 2, 2))
        self.assertEqual((r["requisitioned"], r["requisitioned_using"], r["unprompted"]), (1, 1, 1))
        self.assertEqual(len(turns), 3)

    def test_a_turn_with_no_tool_calls_counts_as_a_turn_but_not_with_tools(self):
        rows, _ = mur.tabulate([], [rnd("zai-2", "t9", [], "2026-10-15T10:00:00Z")])
        self.assertEqual((rows[0]["turns"], rows[0]["with_tools"], rows[0]["using_m40"]), (1, 0, 0))

    def test_days_split_rows_and_string_bodies_are_read(self):
        import json
        rows, _ = mur.tabulate([], [
            {"evidence/author": "zai-3", "evidence/at": "2026-10-14T23:59:00Z",
             "evidence/body": json.dumps({"turn-id": "a", "calls": [{"tool": "m40_asof"}]})},
            rnd("zai-3", "b", ["m40_history", "m40_history"], "2026-10-15T00:01:00Z")])
        self.assertEqual([(r["day"], r["m40_calls"]) for r in rows],
                         [("2026-10-14", 1), ("2026-10-15", 2)])

    def test_requisition_regex_matches_the_packet_wording_not_the_word_memory(self):
        self.assertTrue(mur.REQUISITION.search("call m40_find then m40_history"))
        self.assertTrue(mur.REQUISITION.search("use the minus40 store"))
        self.assertFalse(mur.REQUISITION.search("remember to commit; the memory of the team"))


if __name__ == "__main__":
    unittest.main()
