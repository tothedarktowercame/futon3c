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
    def test_requisitioned_and_packet_silent_are_told_apart(self):
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
        self.assertEqual((r["requisitioned"], r["requisitioned_using"]), (1, 1))
        self.assertEqual(len(turns), 3)

    def test_standing_instruction_counts_so_packet_silent_use_is_not_unprompted(self):
        # The exact case G12 is named for: a used turn with no ask in the
        # packet, before and after the harness's standing instruction.
        starts = [start("zai-1", "t1", "Review this commit", "2026-10-14T10:00:00Z"),
                  start("zai-1", "t2", "Review that commit", "2026-10-16T10:00:00Z")]
        rounds = [rnd("zai-1", "t1", ["m40_find"], "2026-10-14T10:01:00Z"),
                  rnd("zai-1", "t2", ["memory_assert"], "2026-10-16T10:01:00Z")]
        rows, turns = mur.tabulate(starts, rounds, standing_since="2026-10-15")
        by_day = {r["day"]: r for r in rows}
        self.assertEqual(by_day["2026-10-14"]["no_instruction_using"], 1)
        self.assertEqual(by_day["2026-10-14"]["instructed_using"], 0)
        self.assertEqual(by_day["2026-10-16"]["instructed_using"], 1)
        self.assertEqual(by_day["2026-10-16"]["no_instruction_using"], 0)
        # the same turns with the standing instruction unknown: both silent
        rows2, _ = mur.tabulate(starts, rounds, standing_since=None)
        self.assertEqual(sum(r["no_instruction_using"] for r in rows2), 2)

    def test_injected_is_read_and_unused_injection_is_shown(self):
        # Delivery that came from the harness, not the seat's choice: the
        # injected ids are on turn-start, and a turn that also made an m40
        # call of its own is not counted as unused.
        s1 = start("zai-5", "t1", "Do the thing", "2026-10-17T04:40:00Z")
        s1["evidence/body"]["injected"] = ["kimi-unique-requisition",
                                           "agency-send-mode-brief-trap"]
        s2 = start("zai-5", "t2", "Do another thing", "2026-10-17T04:50:00Z")
        s2["evidence/body"]["injected"] = ["lean-top-is-analytic"]
        starts = [s1, s2]
        rounds = [rnd("zai-5", "t1", [], "2026-10-17T04:41:00Z"),
                  rnd("zai-5", "t2", ["m40_find"], "2026-10-17T04:51:00Z")]
        rows, turns = mur.tabulate(starts, rounds)
        r = rows[0]
        self.assertEqual((r["injected"], r["injected_unused"]), (2, 1))
        t = {t["turn"]: t for t in turns}
        self.assertEqual(t["t1"]["injected"], ["kimi-unique-requisition",
                                               "agency-send-mode-brief-trap"])
        self.assertEqual(t["t2"]["injected"], ["lean-top-is-analytic"])
        # absence in words, not a blank: a turn with nothing injected lists []
        rows3, turns3 = mur.tabulate(
            [start("zai-5", "t3", "Third thing", "2026-10-17T05:00:00Z")],
            [rnd("zai-5", "t3", [], "2026-10-17T05:01:00Z")])
        self.assertEqual((rows3[0]["injected"], rows3[0]["injected_unused"]), (0, 0))
        self.assertEqual(turns3[0]["injected"], [])

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
